(* Type-directed random MiniML program generator — the program source for
   differential fuzzing (roadmap #8).

   Design rules:

   - WELL-TYPED BY CONSTRUCTION: every production knows the type it produces
     and the types of everything in scope, so generated programs typecheck
     (a program rejected by the frontend is a generator bug — harmless for
     soundness since rejection is unanimous, but wasted throughput).

   - TERMINATING BY CONSTRUCTION: recursion only via the structural countdown
     pattern (`if n <= 0 do base else ... f (n - 1) ...`), while loops only
     over bounded counters, for-in only over finite list expressions. The
     differential runner's timeout is the backstop, not the strategy.

   - REPRODUCIBLE: all randomness from a Random.State seeded by the caller.
     Same seed -> same program, byte for byte.

   - BIASED toward the historically bug-rich areas (see the parity postmortem):
     effect handlers (non-tail resume, multishot via copy_continuation,
     control-flow escape through handlers, nested handlers, mutable state
     shared across resumes), numeric formatting (via show), multi-print
     output framing, pattern matching with guards, closures.

   - AVOIDS known divergences so every disagreement is a NEW finding:
     no whole floats in display position (floats only flow through `show` —
     BUG-3), no nondeterminism (Sys.time), no Map/Set iteration-order
     dependence, shallow recursion (no stack-limit differences).

   Generated program shape:

     effect Fuzz0 = op0 : int -> int ... end ;;
     let rec helper0 n acc = ...                 (bounded recursion) ;;
     let helper1 x y = ... ;;
     print <expr> ;;                             (output framing)
     ...
     <final expression>                          (displayed value)
*)

(* ---- Generation context ------------------------------------------------- *)

type ty = TInt | TBool | TString | TIntList | TIntOption

type ctx = {
  rng : Random.State.t;
  budget : int ref; (* remaining expression-node fuel — SHARED by sub-contexts *)
  counter : int ref; (* fresh-name counter — shared *)
  vars : (string * ty) list; (* in-scope variables *)
  fns : (string * int) list; (* helper functions: name, arity (ints -> int) *)
  ops : string list; (* effect operations in scope (all int -> int) *)
  in_handler : bool; (* lexically inside a handle body: performs allowed *)
  in_function : bool; (* lexically inside a function: return allowed *)
  mutables : string list; (* in-scope `let mut` cells (int) *)
  loop_depth : int;
      (* nesting depth of loops (for-in / while). Nested loops over lists
         multiply iteration counts, so depth is capped to keep programs fast
         (a fuzz program that times out wastes a whole timeout per backend). *)
}

let fresh ctx prefix =
  let n = !(ctx.counter) in
  incr ctx.counter;
  Printf.sprintf "%s%d" prefix n

let spend ctx = decr ctx.budget
let rand ctx n = Random.State.int ctx.rng n

(* Pick a production from a weighted list. *)
let pick ctx (weighted : (int * (unit -> 'a)) list) : 'a =
  let weighted = List.filter (fun (w, _) -> w > 0) weighted in
  let total = List.fold_left (fun a (w, _) -> a + w) 0 weighted in
  let n = rand ctx total in
  let rec go acc = function
    | [] -> assert false
    | [ (_, p) ] -> p ()
    | (w, p) :: rest -> if n < acc + w then p () else go (acc + w) rest
  in
  go 0 weighted

let vars_of_type ctx ty = List.filter (fun (_, t) -> t = ty) ctx.vars

(* ---- Expressions --------------------------------------------------------- *)
(* Every generator returns a fully parenthesized source string of its type. *)

let rec gen_int ctx : string =
  spend ctx;
  let leaf () = gen_int_leaf ctx in
  if !(ctx.budget) <= 0 then leaf ()
  else
    pick ctx
      [
        (3, leaf);
        (4, fun () -> gen_int_arith ctx);
        (2, fun () -> gen_int_if ctx);
        (2, fun () -> gen_int_let ctx);
        (2, fun () -> gen_int_match ctx);
        (2, fun () -> gen_int_call ctx);
        (1, fun () -> gen_int_lambda_app ctx);
        ((if ctx.loop_depth < 2 then 2 else 0), fun () -> gen_int_fold ctx);
        (1, fun () -> gen_int_list_op ctx);
        ((if ctx.loop_depth < 2 then 1 else 0), fun () -> gen_int_while ctx);
        ((if ctx.ops <> [] && ctx.in_handler then 4 else 0),
         fun () -> gen_perform ctx);
        ((if ctx.mutables <> [] then 2 else 0), fun () -> gen_int_mut_read ctx);
      ]

and gen_int_leaf ctx =
  let vars = vars_of_type ctx TInt in
  pick ctx
    [
      (3, fun () -> string_of_int (rand ctx 20));
      (1, fun () -> string_of_int (rand ctx 1000));
      ( (if vars <> [] then 6 else 0),
        fun () -> fst (List.nth vars (rand ctx (List.length vars))) );
      ((if ctx.mutables <> [] then 2 else 0), fun () -> gen_int_mut_read ctx);
    ]

and gen_int_mut_read ctx =
  List.nth ctx.mutables (rand ctx (List.length ctx.mutables))

and gen_int_arith ctx =
  let op =
    (* Division/mod included: division-by-zero must be a RuntimeError on every
       backend (category agreement); a backend that yields a value instead is
       a finding. Biased toward + - * so most programs complete. *)
    pick ctx
      [
        (8, fun () -> "+");
        (6, fun () -> "-");
        (6, fun () -> "*");
        (1, fun () -> "/");
        (1, fun () -> "mod");
      ]
  in
  (* AVOIDANCE (arithmetic envelope, see Phase-0 postmortem): integer overflow
     is backend-divergent (OCaml wraps at 63 bits, JS float64 loses precision /
     goes to Infinity, native wraps at 64) — i.e. undefined behavior, not a
     valid differential-testing input. Products are masked to 20 bits so every
     intermediate stays far below 2^53 (products of masked values are <= 2^40;
     budget-bounded sums of those stay < 2^53). *)
  if op = "*" then
    Printf.sprintf "(((%s) %s (%s)) land 1048575)" (gen_int ctx) op
      (gen_int ctx)
  else Printf.sprintf "(%s %s %s)" (gen_int ctx) op (gen_int ctx)

and gen_int_if ctx =
  Printf.sprintf "(if %s do %s else %s)" (gen_bool ctx) (gen_int ctx)
    (gen_int ctx)

and gen_int_let ctx =
  let name = fresh ctx "v" in
  let bound = gen_int ctx in
  let ctx' = { ctx with vars = (name, TInt) :: ctx.vars } in
  Printf.sprintf "(let %s = %s in %s)" name bound (gen_int ctx')

and gen_int_match ctx =
  pick ctx
    [
      ( 2,
        fun () ->
          (* match on int with literal / guard / catch-all arms *)
          let scrut = gen_int ctx in
          let n = fresh ctx "m" in
          let ctx' = { ctx with vars = (n, TInt) :: ctx.vars } in
          Printf.sprintf
            "(match %s with | 0 -> %s | %s when (%s > 7) -> %s | %s -> %s)"
            scrut (gen_int ctx) n n (gen_int ctx') n (gen_int ctx') );
      ( 2,
        fun () ->
          (* match on option *)
          let scrut = gen_int_option ctx in
          let x = fresh ctx "o" in
          let ctx' = { ctx with vars = (x, TInt) :: ctx.vars } in
          Printf.sprintf "(match %s with | Some %s -> %s | None -> %s)" scrut x
            (gen_int ctx') (gen_int ctx) );
      ( 2,
        fun () ->
          (* match on list: [] / cons *)
          let scrut = gen_int_list ctx in
          let h = fresh ctx "h" in
          let t = fresh ctx "t" in
          let ctx' =
            { ctx with vars = (h, TInt) :: (t, TIntList) :: ctx.vars }
          in
          Printf.sprintf "(match %s with | [] -> %s | %s :: %s -> %s)" scrut
            (gen_int ctx) h t (gen_int ctx') );
    ]

and gen_int_call ctx =
  match ctx.fns with
  | [] -> gen_int_leaf ctx
  | fns ->
      let name, arity = List.nth fns (rand ctx (List.length fns)) in
      let args =
        List.init arity (fun i ->
            (* The recursive helper's first parameter is its countdown depth.
               Keep it a small literal so non-tail recursion stays shallow —
               otherwise generated programs hit stack limits, which differ per
               backend (resource-limit noise, not semantic findings). *)
            if i = 0 && String.length name >= 4 && String.sub name 0 4 = "loop"
            then string_of_int (rand ctx 15)
            else gen_int ctx)
      in
      Printf.sprintf "(%s %s)" name (String.concat " " args)

and gen_int_lambda_app ctx =
  (* Immediately-applied lambda: closure creation + capture.
     AVOIDANCE (bugs/BUG-11): no performs inside the lambda body — emit-js
     compiles every lambda as a JS function with its own trampoline, so a
     continuation captured inside it does not extend past the call boundary.
     Re-allow (drop in_handler = false) once BUG-11 is fixed. *)
  let x = fresh ctx "p" in
  let ctx' = { ctx with vars = (x, TInt) :: ctx.vars; in_handler = false } in
  Printf.sprintf "((fn %s -> %s) %s)" x (gen_int ctx') (gen_int ctx)

and gen_int_fold ctx =
  (* for-in fold over a list: exercises fold-callback lowering, continue/break.
     AVOIDANCE (bugs/BUG-11): no performs inside the body — the body compiles
     to a fold-callback JS function on emit-js, and continuations captured
     inside it do not extend through the fold call. Re-allow (drop
     in_handler = false) once BUG-11 is fixed. *)
  let x = fresh ctx "i" in
  let acc = fresh ctx "a" in
  let ctx' =
    {
      ctx with
      vars = (x, TInt) :: (acc, TInt) :: ctx.vars;
      loop_depth = ctx.loop_depth + 1;
      in_handler = false;
    }
  in
  let body =
    pick ctx
      [
        (4, fun () -> gen_int ctx');
        ( 1,
          fun () ->
            (* continue with a value for this iteration *)
            Printf.sprintf "(if %s do continue %s else %s)" (gen_bool ctx')
              (gen_int ctx') (gen_int ctx') );
        ( 1,
          fun () ->
            (* break out of the fold with a value *)
            Printf.sprintf "(if %s do break %s else %s)" (gen_bool ctx')
              (gen_int ctx') (gen_int ctx') );
      ]
  in
  Printf.sprintf "(for %s in %s with %s = %s do %s end)" x (gen_int_list ctx)
    acc (gen_int ctx) body

and gen_int_while ctx =
  (* Bounded while loop over a fresh mutable counter *)
  let c = fresh ctx "w" in
  let acc = fresh ctx "s" in
  let bound = 2 + rand ctx 5 in
  let ctx' =
    {
      ctx with
      mutables = c :: acc :: ctx.mutables;
      loop_depth = ctx.loop_depth + 1;
    }
  in
  Printf.sprintf
    "(let mut %s = 0 in let mut %s = %s in (for %s < %d do %s := (%s + 1); %s \
     := %s end); %s)"
    c acc (gen_int ctx) c bound c c acc (gen_int ctx') acc

and gen_int_list_op ctx =
  pick ctx
    [
      (2, fun () -> Printf.sprintf "(List.length %s)" (gen_int_list ctx));
      (2, fun () -> Printf.sprintf "(String.length %s)" (gen_string ctx));
      ( 2,
        fun () ->
          (* AVOIDANCE (bugs/BUG-11): no performs inside the List.fold callback
             (same emit-js lambda-boundary limitation as gen_int_fold). *)
          let x = fresh ctx "e" in
          let ctx' =
            {
              ctx with
              vars = (x, TInt) :: ctx.vars;
              loop_depth = ctx.loop_depth + 1;
              in_handler = false;
            }
          in
          Printf.sprintf "(List.fold (fn %s %s -> (%s + %s)) %s %s)" "zz" x "zz"
            (gen_int ctx') (gen_int ctx) (gen_int_list ctx) );
    ]

and gen_perform ctx =
  let op = List.nth ctx.ops (rand ctx (List.length ctx.ops)) in
  Printf.sprintf "(perform %s %s)" op (gen_int ctx)

(* ---- Bool ----------------------------------------------------------------- *)

and gen_bool ctx : string =
  spend ctx;
  if !(ctx.budget) <= 0 then if rand ctx 2 = 0 then "true" else "false"
  else
    pick ctx
      [
        (1, fun () -> if rand ctx 2 = 0 then "true" else "false");
        ( 4,
          fun () ->
            let op =
              List.nth [ "<"; ">"; "<="; ">="; "="; "<>" ] (rand ctx 6)
            in
            Printf.sprintf "(%s %s %s)" (gen_int ctx) op (gen_int ctx) );
        ( 2,
          fun () ->
            let op = if rand ctx 2 = 0 then "&&" else "||" in
            Printf.sprintf "(%s %s %s)" (gen_bool ctx) op (gen_bool ctx) );
        (1, fun () -> Printf.sprintf "(not %s)" (gen_bool ctx));
        ( 1,
          fun () ->
            Printf.sprintf "(%s = %s)" (gen_string ctx) (gen_string ctx) );
      ]

(* ---- String ---------------------------------------------------------------- *)
(* Floats appear ONLY here, through `show` — bare %g format agrees on every
   backend. Float DISPLAY (the dot-append variant) hits BUG-3 on emit-js for
   nested whole floats, so floats never flow to display position. *)

and gen_string ctx : string =
  spend ctx;
  let words = [ "fuzz"; "x"; "abc"; "q9"; "_"; "hello world"; "" ] in
  let leaf () =
    Printf.sprintf "\"%s\"" (List.nth words (rand ctx (List.length words)))
  in
  if !(ctx.budget) <= 0 then leaf ()
  else
    pick ctx
      [
        (2, leaf);
        ( 2,
          fun () ->
            Printf.sprintf "(%s ^ %s)" (gen_string ctx) (gen_string ctx) );
        (2, fun () -> Printf.sprintf "(string_of_int %s)" (gen_int ctx));
        (2, fun () -> Printf.sprintf "(show %s)" (gen_int ctx));
        (1, fun () -> Printf.sprintf "(show %s)" (gen_bool ctx));
        (2, fun () -> Printf.sprintf "(show %s)" (gen_int_list ctx));
        (2, fun () -> Printf.sprintf "(show %s)" (gen_float ctx));
        ( 1,
          fun () ->
            Printf.sprintf "(if %s do %s else %s)" (gen_bool ctx)
              (gen_string ctx) (gen_string ctx) );
      ]

(* Float expressions: literals across the %g-interesting ranges, plus
   arithmetic. Only ever stringified via `show` (see gen_string). *)
and gen_float ctx : string =
  spend ctx;
  let lit () =
    pick ctx
      [
        (3, fun () -> Printf.sprintf "%d.%d" (rand ctx 100) (1 + rand ctx 99));
        (2, fun () -> Printf.sprintf "%d.0" (rand ctx 100));
        (* > 6 significant digits: exercises %g rounding *)
        ( 2,
          fun () ->
            Printf.sprintf "%d.%d" (1000000 + rand ctx 8999999) (rand ctx 999)
        );
        (* small: exercises scientific-notation threshold *)
        (2, fun () -> Printf.sprintf "0.000%d" (1 + rand ctx 999));
      ]
  in
  if !(ctx.budget) <= 0 then lit ()
  else
    pick ctx
      [
        (4, lit);
        ( 2,
          fun () ->
            let op = List.nth [ "+"; "-"; "*"; "/" ] (rand ctx 4) in
            Printf.sprintf "(%s %s %s)" (gen_float ctx) op (gen_float ctx) );
      ]

(* ---- Int list --------------------------------------------------------------- *)

and gen_int_list ctx : string =
  spend ctx;
  let lit () =
    let n = rand ctx 5 in
    Printf.sprintf "[%s]"
      (String.concat "; "
         (List.init n (fun _ -> string_of_int (rand ctx 50))))
  in
  let vars = vars_of_type ctx TIntList in
  if !(ctx.budget) <= 0 then lit ()
  else
    pick ctx
      [
        (3, lit);
        ( (if vars <> [] then 3 else 0),
          fun () -> fst (List.nth vars (rand ctx (List.length vars))) );
        ( 2,
          fun () -> Printf.sprintf "(%s :: %s)" (gen_int ctx) (gen_int_list ctx)
        );
        ( 2,
          fun () ->
            (* AVOIDANCE (bugs/BUG-11): no performs inside the List.map callback. *)
            let x = fresh ctx "e" in
            let ctx' =
              {
                ctx with
                vars = (x, TInt) :: ctx.vars;
                loop_depth = ctx.loop_depth + 1;
                in_handler = false;
              }
            in
            Printf.sprintf "(List.map (fn %s -> %s) %s)" x (gen_int ctx')
              (gen_int_list ctx) );
        ( 1,
          fun () ->
            (* AVOIDANCE (bugs/BUG-11): no performs inside the List.filter callback. *)
            let x = fresh ctx "e" in
            let ctx' =
              {
                ctx with
                vars = (x, TInt) :: ctx.vars;
                loop_depth = ctx.loop_depth + 1;
                in_handler = false;
              }
            in
            Printf.sprintf "(List.filter (fn %s -> %s) %s)" x (gen_bool ctx')
              (gen_int_list ctx) );
        (1, fun () -> Printf.sprintf "(List.rev %s)" (gen_int_list ctx));
      ]

(* ---- Int option ------------------------------------------------------------- *)

and gen_int_option ctx : string =
  spend ctx;
  if !(ctx.budget) <= 0 then "None"
  else
    pick ctx
      [
        (1, fun () -> "None");
        (3, fun () -> Printf.sprintf "(Some %s)" (gen_int ctx));
        ( 1,
          fun () ->
            Printf.sprintf "(if %s do (Some %s) else None)" (gen_bool ctx)
              (gen_int ctx) );
      ]

(* ---- Handlers: the centerpiece ---------------------------------------------- *)

(* A handler arm body for `| op arg k -> ...`. The resume patterns cover the
   historical bug classes: tail resume, NON-TAIL resume (let / binop operand —
   the emit-js trampoline bug), multishot via copy_continuation (the
   cross-fiber bug), and abort (no resume). *)
and gen_arm ctx : string =
  let arm_ctx = { ctx with vars = ("arg", TInt) :: ctx.vars } in
  pick ctx
    [
      (* tail resume *)
      (4, fun () -> Printf.sprintf "resume k %s" (gen_int arm_ctx));
      (* non-tail: resume as let-bound RHS *)
      ( 3,
        fun () ->
          let r = fresh ctx "r" in
          let ctx' = { arm_ctx with vars = (r, TInt) :: arm_ctx.vars } in
          Printf.sprintf "let %s = resume k %s in %s" r (gen_int arm_ctx)
            (gen_int ctx') );
      (* non-tail: resume as binop operand *)
      ( 2,
        fun () ->
          Printf.sprintf "((resume k %s) + %s)" (gen_int arm_ctx)
            (gen_int arm_ctx) );
      (* multishot: copy the continuation, resume both *)
      ( 3,
        fun () ->
          Printf.sprintf
            "let k2 = copy_continuation k in ((resume k %s) + (resume k2 %s))"
            (gen_int arm_ctx) (gen_int arm_ctx) );
      (* abort: drop the continuation *)
      (2, fun () -> gen_int arm_ctx);
    ]

(* A handle expression of type int. [ops] are the operations this handler
   handles; the body may perform any of them. *)
and gen_handle ctx ops : string =
  let body_ctx = { ctx with ops; in_handler = true } in
  let body = gen_int body_ctx in
  (* Return-arm policy (one remaining known-divergence avoidance):
     - OPTIONAL (BUG-4 fixed 2026-06-02: the parser synthesizes the identity
       return arm, so an omitted one means "produce the body value" on every
       backend).
     - SIMPLE arithmetic only: a match/for-in return arm makes instance
       resolution for multishot `+` ambiguous on the compilers but not the
       oracle (bugs/BUG-5). Allow full expressions once BUG-5 is fixed. *)
  let return_arm =
    pick ctx
      [
        (2, fun () -> "" (* no return arm: defaults to identity *));
        (3, fun () -> "\n| return x -> x");
        ( 2,
          fun () -> Printf.sprintf "\n| return x -> (x + %d)" (rand ctx 10) );
        (1, fun () -> "\n| return x -> (x * 2)");
      ]
  in
  let arms =
    List.map (fun op -> Printf.sprintf "\n| %s arg k -> %s" op (gen_arm ctx)) ops
  in
  Printf.sprintf "(handle\n  %s\nwith%s%s)" body return_arm
    (String.concat "" arms)

(* ---- Top-level program ------------------------------------------------------- *)

(* A bounded-recursion helper:
   let rec fN n acc = if n <= 0 do acc else <expr that recurses on (n - 1)> *)
let gen_rec_helper ctx name =
  (* The helper's OWN name is deliberately NOT in scope for the generated step
     expression: the hard-wired `name (n - 1) ...` below is the only self-call.
     If the step could also call the helper, each iteration would spawn whole
     nested runs — bounded depth but exponential work (found the hard way:
     fuzz programs timing out / overflowing on every backend). *)
  let ctx' =
    {
      ctx with
      vars = [ ("n", TInt); ("acc", TInt) ];
      in_function = true;
    }
  in
  (* The recursive call is hard-wired to (n - 1) so termination is structural.
     The accumulator is masked to 20 bits each step: the step expression can
     reference acc multiplicatively, and unmasked that compounds exponentially
     across iterations, blowing past 2^53 where backend integer semantics
     diverge (the arithmetic-envelope avoidance — see gen_int_arith). *)
  let step =
    pick ctx
      [
        ( 3,
          fun () ->
            Printf.sprintf "(%s (n - 1) ((acc + %s) land 1048575))" name
              (gen_int ctx') );
        ( 2,
          fun () ->
            Printf.sprintf "(%s (n - 1) ((acc + n) land 1048575))" name );
        ( 1,
          fun () ->
            Printf.sprintf "((%s (n - 1) acc) + %s)" name (gen_int ctx') );
      ]
  in
  Printf.sprintf "let rec %s n acc = if n <= 0 do acc else %s" name step

(* A non-recursive helper of arity [arity], possibly containing return
   (control-flow escape) and a handler (return crossing the handler — the
   bug class behind native #25 / emit-js handler escape). *)
let gen_helper ctx name arity ~effects =
  let params = List.init arity (fun i -> Printf.sprintf "x%d" i) in
  let ctx' =
    {
      ctx with
      vars = List.map (fun p -> (p, TInt)) params @ ctx.vars;
      in_function = true;
    }
  in
  let body =
    pick ctx
      [
        (3, fun () -> gen_int ctx');
        (* early return *)
        ( 2,
          fun () ->
            Printf.sprintf "(if %s do return %s else %s)" (gen_bool ctx')
              (gen_int ctx') (gen_int ctx') );
        (* handler inside the function; return may escape through it *)
        ( (if effects <> [] then 3 else 0),
          fun () ->
            let with_return =
              Printf.sprintf "(if %s do return %s else %s)" (gen_bool ctx')
                (gen_int ctx')
                (gen_handle ctx' effects)
            in
            with_return );
        (* for-in with return escaping the fold callback *)
        ( 2,
          fun () ->
            let x = fresh ctx "i" in
            let a = fresh ctx "a" in
            let loop_ctx =
              {
                ctx' with
                vars = (x, TInt) :: (a, TInt) :: ctx'.vars;
                loop_depth = ctx'.loop_depth + 1;
              }
            in
            Printf.sprintf
              "(for %s in %s with %s = 0 do (if %s do return %s else (%s + \
               %s)) end)"
              x (gen_int_list ctx') a (gen_bool loop_ctx) (gen_int loop_ctx)
              a (gen_int loop_ctx) );
      ]
  in
  Printf.sprintf "let %s %s = %s" name (String.concat " " params) body

(* Generate one complete program from a seed. [size] is the expression-node
   budget (bigger = more complex programs). *)
let generate ~seed ~size : string =
  let rng = Random.State.make [| seed |] in
  let ctx =
    {
      rng;
      budget = ref size;
      counter = ref 0;
      vars = [];
      fns = [];
      ops = [];
      in_handler = false;
      in_function = false;
      mutables = [];
      loop_depth = 0;
    }
  in
  let buf = Buffer.create 1024 in
  let emit fmt = Printf.ksprintf (fun s -> Buffer.add_string buf s) fmt in

  (* 1. Effect declaration (most of the time — it's the priority area) *)
  let num_ops = if rand ctx 5 = 0 then 0 else 1 + rand ctx 2 in
  let ops = List.init num_ops (fun i -> Printf.sprintf "op%d" i) in
  if ops <> [] then begin
    emit "effect Fuzz =\n";
    List.iter (fun op -> emit "  %s : int -> int\n" op) ops;
    emit "end\n;;\n"
  end;

  (* 2. A bounded recursive helper *)
  let rec_name = "loop0" in
  emit "%s\n;;\n" (gen_rec_helper { ctx with budget = ref (size / 3) } rec_name);
  let ctx = { ctx with fns = (rec_name, 2) :: ctx.fns } in

  (* 3. 1-2 plain helpers (may contain handlers / early returns) *)
  let n_helpers = 1 + rand ctx 2 in
  let ctx =
    List.fold_left
      (fun ctx i ->
        let name = Printf.sprintf "fn%d" i in
        let arity = 1 + rand ctx 2 in
        emit "%s\n;;\n"
          (gen_helper { ctx with budget = ref (size / 2) } name arity ~effects:ops);
        { ctx with fns = (name, arity) :: ctx.fns })
      ctx
      (List.init n_helpers (fun i -> i))
  in

  (* 4. A top-level mutable cell (heap state shared across multishot resumes) *)
  let ctx =
    if rand ctx 2 = 0 then begin
      emit "let mut cell = 0\n;;\n";
      { ctx with mutables = [ "cell" ] }
    end
    else ctx
  in

  (* 5. Print statements: multi-print output framing + value formatting *)
  let n_prints = 1 + rand ctx 3 in
  for _ = 1 to n_prints do
    let printable =
      pick ctx
        [
          (3, fun () -> gen_int { ctx with budget = ref (size / 3) });
          (3, fun () -> gen_string { ctx with budget = ref (size / 3) });
          (2, fun () -> gen_int_list { ctx with budget = ref (size / 3) });
          ( (if ops <> [] then 4 else 0),
            fun () -> gen_handle { ctx with budget = ref (size / 2) } ops );
        ]
    in
    emit "print %s;;\n" printable
  done;

  (* 6. Final expression: a handler when there are effects, else data *)
  let final =
    pick ctx
      [
        ( (if ops <> [] then 5 else 0),
          fun () -> gen_handle { ctx with budget = ref size } ops );
        (2, fun () -> gen_int_list { ctx with budget = ref (size / 2) });
        (2, fun () -> gen_int { ctx with budget = ref (size / 2) });
        ( 1,
          fun () ->
            Printf.sprintf "(%s, %s)"
              (gen_int { ctx with budget = ref (size / 4) })
              (gen_string { ctx with budget = ref (size / 4) }) );
      ]
  in
  emit "%s\n" final;
  Buffer.contents buf
