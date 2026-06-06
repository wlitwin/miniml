(* Lossless CST round-trip runner (roadmap #17, increment 1).

   For every MiniML source in the corpus, lex it into the lossless piece stream
   (Cst.of_source) and reassemble it (Cst.reconstruct); the result MUST equal
   the original byte-for-byte. This proves the lexer's token spans tile the
   source with no loss — the invariant a formatter (#21) and LSP (#22) build on.

   Corpus = every stdlib/*.mml and self_host/*.mml file (real, comment-rich
   source) plus the `source` of every cross_test case (small, construct-dense
   snippets). Pure lexing, so it runs in well under a second. *)

open Test_format

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* All *.mml files directly under [dir] (sorted), or [] if [dir] is absent. *)
let mml_files dir =
  if not (Sys.file_exists dir) then []
  else
    let entries = Sys.readdir dir in
    let files =
      Array.to_list entries
      |> List.filter (fun e -> Filename.check_suffix e ".mml")
      |> List.map (fun e -> Filename.concat dir e)
    in
    List.sort String.compare files

(* Resolve a repo-relative path whether the runner is invoked from the repo
   root (via make) or from compiler_test/. *)
let resolve rel =
  if Sys.file_exists rel then Some rel
  else
    let up = Filename.concat ".." rel in
    if Sys.file_exists up then Some up else None

(* Locate the first differing byte offset between [a] and [b], for diagnostics. *)
let first_diff a b =
  let n = min (String.length a) (String.length b) in
  let rec go i =
    if i >= n then n else if a.[i] <> b.[i] then i else go (i + 1)
  in
  go 0

let snippet s off =
  let lo = max 0 (off - 20) in
  let hi = min (String.length s) (off + 20) in
  String.escaped (String.sub s lo (hi - lo))

let () =
  let passed = ref 0 and failed = ref 0 and parse_skipped = ref 0 in
  let check label source rebuilt =
    if rebuilt = source then incr passed
    else begin
      incr failed;
      let off = first_diff source rebuilt in
      Printf.printf "  FAIL  %s\n" label;
      Printf.printf "        first diff at offset %d (len src=%d rebuilt=%d)\n"
        off (String.length source) (String.length rebuilt);
      Printf.printf "        src     : ...%s...\n" (snippet source off);
      Printf.printf "        rebuilt : ...%s...\n" (snippet rebuilt off)
    end
  in
  (* Two lossless paths must each reproduce the source byte-for-byte: the raw
     piece-stream reconstruction (increment 1) and the flat green tree's
     to_source (increment 2). *)
  let report label source =
    check (label ^ " [pieces]") source (Interpreter.Cst.roundtrip source);
    check (label ^ " [green]") source
      (Interpreter.Cst.to_source (Interpreter.Cst.flat_of_source source));
    (* The parser-produced STRUCTURED tree must also round-trip. Sources that
       don't parse on the OCaml reference are out of scope (skipped), exactly as
       in the IR-parity runner. *)
    match Interpreter.Cst_build.cst_of_source source with
    | tree -> check (label ^ " [parsed]") source (Interpreter.Cst.to_source tree)
    | exception _ -> incr parse_skipped
  in
  (* Real source files. *)
  let file_corpus =
    List.concat_map mml_files
      (List.filter_map resolve [ "stdlib"; "self_host" ])
  in
  List.iter (fun path -> report (Filename.basename path) (read_file path))
    file_corpus;
  Printf.printf "Round-tripped %d source files.\n%!" (List.length file_corpus);
  (* Cross-test case sources. *)
  let tests_dir =
    match resolve "cross_test/tests" with
    | Some d -> Some d
    | None -> resolve "tests"
  in
  let case_count = ref 0 in
  (match tests_dir with
  | None -> Printf.printf "(no cross_test corpus found — skipping cases)\n"
  | Some dir ->
      List.iter
        (fun file ->
          let base = Filename.basename file in
          List.iter
            (fun tc ->
              incr case_count;
              report (base ^ " :: " ^ tc.name) tc.source)
            (parse_test_file file))
        (find_test_files dir));
  Printf.printf "Round-tripped %d cross-test case sources.\n%!" !case_count;
  Printf.printf "\n==============================\n";
  Printf.printf "CST round-trip: %d passed, %d failed (%d parsed-tree skipped: unparseable on ref)\n"
    !passed !failed !parse_skipped;
  if !failed > 0 then exit 1
