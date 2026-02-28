let usage () =
  Printf.eprintf "Usage: mmlc <file.mml> [-o <output>] [--emit-ir]\n";
  exit 1

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then usage ();
  let source_file = ref "" in
  let output_file = ref "" in
  let emit_ir = ref false in
  let i = ref 1 in
  while !i < argc do
    match Sys.argv.(!i) with
    | "-o" ->
      if !i + 1 >= argc then begin
        Printf.eprintf "Error: -o requires an argument\n";
        exit 1
      end;
      output_file := Sys.argv.(!i + 1);
      i := !i + 2
    | "--emit-ir" ->
      emit_ir := true;
      i := !i + 1
    | "-h" | "--help" ->
      usage ()
    | arg when String.length arg > 0 && arg.[0] = '-' ->
      Printf.eprintf "Unknown flag: %s\n" arg;
      exit 1
    | arg ->
      source_file := arg;
      i := !i + 1
  done;
  if !source_file = "" then begin
    Printf.eprintf "Error: no source file specified\n";
    exit 1
  end;
  (* Default output: strip extension *)
  if !output_file = "" then
    output_file := Filename.remove_extension !source_file;
  try
    if !emit_ir then
      Interpreter_native.Driver.emit_ir_to_stdout ~source_file:!source_file
    else
      Interpreter_native.Driver.compile_to_native
        ~source_file:!source_file
        ~output:!output_file
  with
  | Interpreter.Interp.Error msg ->
    Printf.eprintf "%s\n" msg;
    exit 1
  | Interpreter_native.Driver.Driver_error msg ->
    Printf.eprintf "Driver error: %s\n" msg;
    exit 1
  | Failure msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
