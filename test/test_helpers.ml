let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let test name f =
  incr tests_run;
  try
    f ();
    incr tests_passed;
    Printf.printf "  PASS: %s\n" name
  with e ->
    incr tests_failed;
    Printf.printf "  FAIL: %s -- %s\n" name (Printexc.to_string e)

let expect_value source expected =
  let result = Interpreter.Interp.run_string source in
  if result <> expected then
    failwith
      (Printf.sprintf "expected %s, got %s"
         (Interpreter.Bytecode.pp_value expected)
         (Interpreter.Bytecode.pp_value result))

let expect_int source n = expect_value source (Interpreter.Bytecode.VInt n)
let expect_float source f = expect_value source (Interpreter.Bytecode.VFloat f)
let expect_bool source b = expect_value source (Interpreter.Bytecode.VBool b)

let expect_string source s =
  expect_value source (Interpreter.Bytecode.VString s)

let expect_unit source = expect_value source Interpreter.Bytecode.VUnit

let expect_type_error source =
  try
    let _ = Interpreter.Interp.run_string source in
    failwith "expected type error, but succeeded"
  with Interpreter.Interp.Error msg ->
    if not (String.starts_with ~prefix:"Type error" msg) then
      failwith (Printf.sprintf "expected type error, got: %s" msg)

let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if String.sub haystack i nlen = needle then found := true
    done;
    !found

let expect_type_error_msg source expected_substr =
  try
    let _ = Interpreter.Interp.run_string source in
    failwith "expected type error, but succeeded"
  with Interpreter.Interp.Error msg ->
    if not (String.starts_with ~prefix:"Type error" msg) then
      failwith (Printf.sprintf "expected type error, got: %s" msg);
    if not (contains_substring msg expected_substr) then
      failwith
        (Printf.sprintf "expected message containing %S, got: %s"
           expected_substr msg)

let expect_runtime_error source =
  try
    let _ = Interpreter.Interp.run_string source in
    failwith "expected runtime error, but succeeded"
  with Interpreter.Interp.Error msg ->
    if not (String.starts_with ~prefix:"Runtime error" msg) then
      failwith (Printf.sprintf "expected runtime error, got: %s" msg)

let stdlib_state_lazy =
  lazy (Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()))

let get_stdlib_state () = Lazy.force stdlib_state_lazy

(* The typechecking context with stdlib loaded — what Analysis functions now take
   (Analysis.state = Typechecker.ctx, decoupled from the VM's repl_state). Reuses
   the shared lazy state. *)
let get_analysis_ctx () = (Lazy.force stdlib_state_lazy).Interpreter.Interp.ctx

let run_stdlib source =
  (* helpers evaluate a bare expression, which always has a value; a binding-
     terminated program (no value) defaults to VUnit *)
  match Interpreter.Interp.run_string_in_state (get_stdlib_state ()) source with
  | Some v -> v
  | None -> Interpreter.Bytecode.VUnit

let expect_stdlib_int source n =
  let result = run_stdlib source in
  if result <> Interpreter.Bytecode.VInt n then
    failwith
      (Printf.sprintf "expected %d, got %s" n
         (Interpreter.Bytecode.pp_value result))

let expect_stdlib_float source f =
  let result = run_stdlib source in
  match result with
  | Interpreter.Bytecode.VFloat f2 when abs_float (f -. f2) < 0.001 -> ()
  | v ->
      failwith
        (Printf.sprintf "expected %f, got %s" f
           (Interpreter.Bytecode.pp_value v))

let expect_stdlib_bool source b =
  let result = run_stdlib source in
  if result <> Interpreter.Bytecode.VBool b then
    failwith
      (Printf.sprintf "expected %b, got %s" b
         (Interpreter.Bytecode.pp_value result))

let expect_stdlib_string source s =
  let result = run_stdlib source in
  if result <> Interpreter.Bytecode.VString s then
    failwith
      (Printf.sprintf "expected %s, got %s" s
         (Interpreter.Bytecode.pp_value result))

let expect_stdlib_value source expected =
  let result = run_stdlib source in
  if result <> expected then
    failwith
      (Printf.sprintf "expected %s, got %s"
         (Interpreter.Bytecode.pp_value expected)
         (Interpreter.Bytecode.pp_value result))

(* Like expect_type_error_msg, but with the stdlib loaded — needed for errors
   that only surface after stdlib typeclass instances resolve (e.g. map
   pattern exhaustiveness, which requires the Map instance). wrap_errors
   converts raw Typechecker/Compiler/Vm exceptions into Interp.Error. *)
let expect_stdlib_type_error_msg source expected_substr =
  try
    let _ = Interpreter.Interp.wrap_errors (fun () -> run_stdlib source) in
    failwith "expected type error, but succeeded"
  with Interpreter.Interp.Error msg ->
    if not (String.starts_with ~prefix:"Type error" msg) then
      failwith (Printf.sprintf "expected type error, got: %s" msg);
    if not (contains_substring msg expected_substr) then
      failwith
        (Printf.sprintf "expected message containing %S, got: %s"
           expected_substr msg)

let print_summary () =
  Printf.printf "\n==============================\n";
  Printf.printf "%d/%d tests passed" !tests_passed !tests_run;
  if !tests_failed > 0 then Printf.printf " (%d FAILED)" !tests_failed;
  Printf.printf "\n";
  if !tests_failed > 0 then exit 1
