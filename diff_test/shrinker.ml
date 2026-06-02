(* Shrinker — automatic minimization of disagreeing programs (roadmap #9a).

   Given a program on which backends disagree, produce the smallest program
   that still disagrees THE SAME WAY, by delta debugging the source text:

     1. remove whole top-level declarations (;;-separated)
     2. remove handler arms / match arms (lines starting with "| ")
     3. replace parenthesized subexpressions with literals (0, 1, true, [], "")
     4. repeat to a fixpoint

   "Disagrees the same way" = the same DISAGREEMENT SIGNATURE: the same set of
   (backend, result-category) pairs dissenting from the reference. Comparing
   categories rather than exact outputs lets values change as the program
   shrinks while preventing the shrink from drifting onto a different bug.

   Each candidate is checked by running the differential engine on ALL the
   backends of the original run: the signature includes both "these backends
   dissent this way" AND "everyone else agrees". Without the second half, a
   shrink can drift onto a different bug — e.g. deleting a handler's return
   arm makes the VM (which originally agreed) start rejecting the program
   (BUG-4), silently changing which bug the repro reproduces. Most candidates
   are ill-typed and rejected in-process before any backend subprocess runs,
   which is what keeps shrinking fast.

   This automates the by-hand reduction loop used to characterize BUG-4
   through BUG-10 (see the parity postmortem): each of those took minutes of
   manual deletion and re-running; the recipe was completely mechanical. *)

(* The disagreement signature: which backends dissent from the reference and
   with what result category. *)
type signature = (Differential.backend * string) list

let signature_of_results (results : (Differential.backend * Differential.result) list)
    : signature option =
  match Differential.verdict results with
  | Differential.Agree _ -> None
  | Differential.Disagree results -> (
      match results with
      | [] -> None
      | (_, reference) :: rest ->
          Some
            (List.filter_map
               (fun (b, r) ->
                 if Differential.results_agree reference r then None
                 else Some (b, Differential.result_label r))
               rest))

(* ---- Candidate generation ----------------------------------------------- *)

(* Split a program into ;;-separated declarations (the separators stay with
   the preceding chunk so rejoining is exact). *)
let split_decls (src : string) : string list =
  let parts = ref [] in
  let buf = Buffer.create 256 in
  let n = String.length src in
  let i = ref 0 in
  while !i < n do
    if !i + 1 < n && src.[!i] = ';' && src.[!i + 1] = ';' then begin
      Buffer.add_string buf ";;";
      parts := Buffer.contents buf :: !parts;
      Buffer.clear buf;
      i := !i + 2
    end
    else begin
      Buffer.add_char buf src.[!i];
      incr i
    end
  done;
  if String.trim (Buffer.contents buf) <> "" then
    parts := Buffer.contents buf :: !parts;
  List.rev !parts

(* Candidates from removing one declaration at a time (later decls first:
   earlier ones are more likely to be depended upon). *)
let decl_removal_candidates (src : string) : string list =
  let decls = split_decls src in
  let n = List.length decls in
  if n <= 1 then []
  else
    List.init n (fun drop ->
        let drop = n - 1 - drop in
        decls
        |> List.filteri (fun i _ -> i <> drop)
        |> String.concat "")

(* Candidates from removing one "| ..." arm line at a time (handler arms,
   match arms). Never removes the last arm of a block. *)
let arm_removal_candidates (src : string) : string list =
  let lines = String.split_on_char '\n' src in
  let indexed = List.mapi (fun i l -> (i, l)) lines in
  let arm_lines =
    List.filter_map
      (fun (i, l) ->
        let t = String.trim l in
        if String.length t > 2 && String.sub t 0 2 = "| " then Some i else None)
      indexed
  in
  List.map
    (fun drop ->
      indexed
      |> List.filter_map (fun (i, l) -> if i = drop then None else Some l)
      |> String.concat "\n")
    arm_lines

(* Find all balanced-paren regions [(start, end_exclusive)] in the source,
   widest first (replacing a big region first shrinks fastest). *)
let paren_regions (src : string) : (int * int) list =
  let regions = ref [] in
  let stack = ref [] in
  String.iteri
    (fun i c ->
      if c = '(' then stack := i :: !stack
      else if c = ')' then
        match !stack with
        | start :: rest ->
            stack := rest;
            regions := (start, i + 1) :: !regions
        | [] -> ())
    src;
  List.sort (fun (s1, e1) (s2, e2) -> compare (e2 - s2) (e1 - s1)) !regions

let replacement_literals = [ "0"; "1"; "true"; "false"; "[]"; "\"\"" ]

(* Candidates from replacing one parenthesized region with one literal.
   Capped: the K widest regions only, to bound the candidate count. *)
let paren_replacement_candidates ?(max_regions = 40) (src : string) :
    string list =
  let regions = paren_regions src in
  let regions = List.filteri (fun i _ -> i < max_regions) regions in
  List.concat_map
    (fun (s, e) ->
      List.map
        (fun lit ->
          String.sub src 0 s ^ lit ^ String.sub src e (String.length src - e))
        replacement_literals)
    regions

(* ---- The shrink loop ---------------------------------------------------- *)

type progress = { mutable checks : int; mutable rounds : int }

(* Shrink [source]: returns the smallest interesting variant found, the
   number of differential checks spent, and the original signature.
   [check] runs the differential engine and returns the signature (None =
   no disagreement). *)
let shrink_with ~(check : string -> signature option)
    ~(log : string -> unit) (source : string) : string * int =
  let progress = { checks = 0; rounds = 0 } in
  let checked = Hashtbl.create 64 in
  let original_sig =
    match check source with
    | Some s -> s
    | None -> invalid_arg "Shrinker.shrink: program does not disagree"
  in
  progress.checks <- 1;
  let is_interesting candidate =
    (* Memoize: text-identical candidates appear across rounds *)
    match Hashtbl.find_opt checked candidate with
    | Some r -> r
    | None ->
        progress.checks <- progress.checks + 1;
        let r = check candidate = Some original_sig in
        Hashtbl.replace checked candidate r;
        r
  in
  let smaller a b = String.length a < String.length b in
  let rec round src =
    progress.rounds <- progress.rounds + 1;
    log
      (Printf.sprintf "round %d: %d chars, %d checks so far" progress.rounds
         (String.length src) progress.checks);
    (* Try candidate groups in order of expected payoff; take the FIRST
       interesting smaller candidate and start a new round from it. *)
    let candidates =
      decl_removal_candidates src
      @ arm_removal_candidates src
      @ paren_replacement_candidates src
    in
    match
      List.find_opt (fun c -> smaller c src && is_interesting c) candidates
    with
    | Some better -> round better
    | None -> src
  in
  let result = round source in
  (result, progress.checks)

(* Convenience wrapper using the differential engine directly. The signature
   is computed over ALL of [backends] — see the module comment for why
   restricting to the dissenters is unsound. *)
let shrink ~state ~(backends : Differential.backend list) ~timeout
    ~(log : string -> unit) (source : string) : string * int =
  let check src =
    let results = Differential.run_all ~backends ~state ~timeout src in
    signature_of_results results
  in
  shrink_with ~check ~log source
