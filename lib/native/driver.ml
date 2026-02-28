(** Native compilation driver.

    Orchestrates the full pipeline: source -> frontend -> codegen -> .ll -> clang -> binary.
    Also provides an --emit-ir mode that just prints the LLVM IR. *)

open Interpreter

exception Driver_error of string

let error msg = raise (Driver_error msg)

(* ---- Runtime path discovery ---- *)

let find_runtime_file filename =
  let candidates = [
    "native_rt/" ^ filename;
    Filename.concat (Filename.dirname Sys.executable_name) ("../../../native_rt/" ^ filename);
  ] in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None ->
    match Sys.getenv_opt "MMLC_RUNTIME_DIR" with
    | Some dir ->
      let path = Filename.concat dir filename in
      if Sys.file_exists path then path
      else error (Printf.sprintf "%s not found in MMLC_RUNTIME_DIR=%s" filename dir)
    | None ->
      error (Printf.sprintf "Cannot find native_rt/%s. Run from project root or set MMLC_RUNTIME_DIR." filename)

let find_runtime_c () = find_runtime_file "runtime.c"

let find_context_asm () =
  let arch =
    let ic = Unix.open_process_in "uname -m" in
    let a = try String.trim (input_line ic) with End_of_file -> "" in
    ignore (Unix.close_process_in ic); a
  in
  let filename = match arch with
    | "x86_64" -> "context_x86_64.S"
    | "arm64" | "aarch64" -> "context_arm64.S"
    | _ -> error ("Unsupported architecture for native codegen: " ^ arch)
  in
  find_runtime_file filename

(* ---- Frontend pipeline (shared with bytecode compiler) ---- *)

let run_frontend source =
  Interp.start_js_capture ();
  let state = Std.register_all (Interp.repl_state_init ()) in
  Interp.stop_js_capture ();
  let stdlib_programs = !Interp.captured_typed_setups in
  Interp.wrap_errors (fun () ->
    let tokens = Lexer.tokenize source in
    let program = Parser.parse_program tokens in
    let (ctx', typed_program) =
      Typechecker.check_program_in_ctx state.Interp.ctx program in
    let result = Typechecker.transform_constraints ctx' typed_program in
    (stdlib_programs, result, ctx'.type_env)
  )

(* ---- Compilation ---- *)

let compile_ir (source : string) : string =
  let (stdlib_programs, typed_program, type_env) = run_frontend source in
  let typed_program = Typechecker.classify_handlers typed_program in
  let stdlib_programs = List.map (fun (te, p) -> (te, Typechecker.classify_handlers p)) stdlib_programs in
  Codegen.compile_program_with_stdlib type_env stdlib_programs typed_program

let compile_to_native ~source_file ~output =
  let ic = open_in source_file in
  let source = In_channel.input_all ic in
  close_in ic;

  let ir_text = compile_ir source in

  (* Write .ll to temp file *)
  let ll_file = Filename.temp_file "mml_" ".ll" in
  Fun.protect ~finally:(fun () -> try Sys.remove ll_file with _ -> ()) (fun () ->
    let oc = open_out ll_file in
    output_string oc ir_text;
    close_out oc;

    let runtime_c = find_runtime_c () in
    let context_asm = find_context_asm () in

    (* Discover Boehm GC flags for the platform *)
    let gc_flags =
      if Sys.file_exists "/opt/homebrew/include/gc/gc.h" then
        "-I/opt/homebrew/include -L/opt/homebrew/lib -lgc"
      else if Sys.file_exists "/usr/local/include/gc/gc.h" then
        "-I/usr/local/include -L/usr/local/lib -lgc"
      else
        (* Fallback: try pkg-config, else bare -lgc *)
        let ic = Unix.open_process_in "pkg-config --cflags --libs bdw-gc 2>/dev/null" in
        let flags = try input_line ic with End_of_file -> "-lgc" in
        ignore (Unix.close_process_in ic);
        flags
    in

    (* Invoke clang: compile .ll + runtime.c + context asm -> binary *)
    let cmd = Printf.sprintf "clang -O2 %s -o %s %s %s %s"
      gc_flags
      (Filename.quote output)
      (Filename.quote runtime_c)
      (Filename.quote context_asm)
      (Filename.quote ll_file)
    in
    let exit_code = Sys.command cmd in
    if exit_code <> 0 then
      error (Printf.sprintf "clang failed with exit code %d" exit_code)
  )

let emit_ir_to_stdout ~source_file =
  let ic = open_in source_file in
  let source = In_channel.input_all ic in
  close_in ic;
  let ir_text = compile_ir source in
  print_string ir_text
