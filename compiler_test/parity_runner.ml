(* Compiler parity test runner
   Compares OCaml compiler output vs self-hosted compiler output.
   Both run on the OCaml VM — differences indicate a compiler bug. *)

type expectation =
  | Value of string
  | TypeError
  | TypeErrorMsg of string
  | RuntimeError of string

type test_case = {
  name : string;
  source : string;
  expect : expectation;
}

(* --- Parsing (from cross_test/runner.ml) --------------------------------- *)

type parse_state =
  | Idle
  | CollectingSource of string

let parse_test_file filename =
  let ic = open_in filename in
  let tests = ref [] in
  let state = ref Idle in
  let source_buf = Buffer.create 256 in
  let flush name expect =
    let source = String.trim (Buffer.contents source_buf) in
    tests := { name; source; expect } :: !tests;
    state := Idle;
    Buffer.clear source_buf
  in
  (try while true do
    let line = input_line ic in
    let trimmed = String.trim line in
    if String.length trimmed > 9
       && String.sub trimmed 0 9 = "--- test:" then begin
      let name = String.trim (String.sub trimmed 9 (String.length trimmed - 9)) in
      state := CollectingSource name;
      Buffer.clear source_buf
    end
    else if String.length line > 12
            && String.sub line 0 12 = "--- expect: " then begin
      match !state with
      | CollectingSource name ->
        let raw = String.sub line 12 (String.length line - 12) in
        let expected =
          let len = String.length raw in
          if len >= 2 && raw.[0] = '"' && raw.[len - 1] = '"'
          then String.sub raw 1 (len - 2)
          else raw
        in
        flush name (Value expected)
      | Idle -> ()
    end
    else if String.length trimmed >= 22
            && String.sub trimmed 0 22 = "--- expect-type-error:" then begin
      match !state with
      | CollectingSource name ->
        let substr = String.trim (String.sub trimmed 22 (String.length trimmed - 22)) in
        if substr = "" then flush name TypeError
        else flush name (TypeErrorMsg substr)
      | Idle -> ()
    end
    else if trimmed = "--- expect-type-error" then begin
      match !state with
      | CollectingSource name -> flush name TypeError
      | Idle -> ()
    end
    else if String.length trimmed >= 25
            && String.sub trimmed 0 25 = "--- expect-runtime-error:" then begin
      match !state with
      | CollectingSource name ->
        let substr = String.trim (String.sub trimmed 25 (String.length trimmed - 25)) in
        flush name (RuntimeError substr)
      | Idle -> ()
    end
    else begin
      match !state with
      | CollectingSource _ ->
        if Buffer.length source_buf > 0 || trimmed <> "" then begin
          if String.length trimmed > 3 && String.sub trimmed 0 3 = "===" then
            ()
          else begin
            if Buffer.length source_buf > 0 then Buffer.add_char source_buf '\n';
            Buffer.add_string source_buf line
          end
        end
      | Idle -> ()
    end
  done with End_of_file -> ());
  close_in ic;
  List.rev !tests

(* --- Helpers ------------------------------------------------------------- *)

let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else begin
    let found = ref false in
    for i = 0 to hlen - nlen do
      if String.sub haystack i nlen = needle then found := true
    done;
    !found
  end

(* --- Result types -------------------------------------------------------- *)

type run_result =
  | Ok of string      (* printed output + pp_value *)
  | TypeErr of string  (* type error message *)
  | RuntimeErr of string (* runtime error message *)
  | OtherErr of string   (* unexpected exception *)

(* --- OCaml compiler path ------------------------------------------------- *)

let is_selfhost_type_error msg =
  contains_substring msg "Type error" || contains_substring msg "type error"
  || contains_substring msg "TypeError" || contains_substring msg "Unify error"
  || contains_substring msg "unbound variable" || contains_substring msg "unknown type"
  || contains_substring msg "cannot unify"
  || contains_substring msg "is not mutable"
  || contains_substring msg "has no field"
  || contains_substring msg "not handled"
  || contains_substring msg "unknown effect"
  || contains_substring msg "expects" (* covers "type X expects N type argument(s)" and "effect X expects N type parameter(s)" *)
  || contains_substring msg "record update on non-record"
  || contains_substring msg "has unhandled effects"
  || contains_substring msg "unknown constructor"
  || contains_substring msg "unknown module"
  || contains_substring msg "break outside"
  || contains_substring msg "continue outside"
  || contains_substring msg "break with value"
  || (contains_substring msg "non-exhaustive" && contains_substring msg "missing")
  || contains_substring msg "expected constructor name"
  || contains_substring msg "no instance of"
  || contains_substring msg "cannot derive"
  || contains_substring msg "GADT constructor"
  || contains_substring msg "would escape"

let run_ocaml state source =
  let outputs = ref [] in
  Interpreter.Interp.output_fn := (fun s -> outputs := s :: !outputs);
  try
    let result = Interpreter.Interp.run_string_in_state state source in
    Interpreter.Interp.output_fn := print_endline;
    let pp = Interpreter.Bytecode.pp_value result in
    let actual =
      let outs = List.rev !outputs in
      if outs <> [] && pp = "()" then String.concat "\n" outs
      else if outs <> [] then String.concat "\n" outs ^ "\n" ^ pp
      else pp
    in
    Ok actual
  with
  | Interpreter.Typechecker.Type_error (msg, _) ->
    Interpreter.Interp.output_fn := print_endline;
    TypeErr msg
  | Interpreter.Interp.Error msg ->
    Interpreter.Interp.output_fn := print_endline;
    if String.starts_with ~prefix:"Type error" msg then TypeErr msg
    else if String.starts_with ~prefix:"Runtime error" msg then RuntimeErr msg
    else RuntimeErr msg
  | Interpreter.Vm.Runtime_error msg ->
    Interpreter.Interp.output_fn := print_endline;
    RuntimeErr msg
  | exn ->
    Interpreter.Interp.output_fn := print_endline;
    OtherErr (Printexc.to_string exn)

(* --- Self-hosted compiler path ------------------------------------------- *)

type selfhost_compile_result =
  | Compiled of string    (* JSON bundle ready to run *)
  | CompileErr of string  (* compiler error message *)

(* Batch-compile all test sources in one compiler invocation *)
let batch_selfhost_compile prepared tests =
  (* Write each test source to a temp file *)
  let tmpfiles = List.map (fun tc ->
    let f = Filename.temp_file "parity_test_" ".mml" in
    let oc = open_out f in
    output_string oc tc.source;
    close_out oc;
    f
  ) tests in
  (* Write manifest listing all temp files *)
  let manifest = Filename.temp_file "parity_manifest_" ".txt" in
  let oc = open_out manifest in
  List.iter (fun f -> output_string oc (f ^ "\n")) tmpfiles;
  close_out oc;
  (* Run batch compilation *)
  Interpreter.Interp.script_argv := [|"compiler.json"; "--batch"; manifest|];
  let output_parts = ref [] in
  let total = List.length tests in
  let compiled_count = ref 0 in
  Interpreter.Interp.output_fn := (fun s ->
    output_parts := s :: !output_parts;
    if s = "===BATCH-SEP===" then begin
      incr compiled_count;
      Printf.printf "\r  Compiling: %d/%d%!" !compiled_count total
    end
  );
  let batch_err = ref "" in
  let batch_ok =
    try let _ = Interpreter.Deserialize.run_prepared prepared in true
    with exn ->
      batch_err := Printexc.to_string exn;
      false
  in
  Interpreter.Interp.output_fn := print_endline;
  if total > 0 then Printf.printf "\r%s\r%!" (String.make 40 ' ');
  (* Clean up temp files *)
  List.iter (fun f -> (try Sys.remove f with _ -> ())) tmpfiles;
  (try Sys.remove manifest with _ -> ());
  if not batch_ok then begin
    let last_output =
      match !output_parts with
      | [] -> ""
      | parts ->
        let non_sep = List.filter (fun s -> s <> "===BATCH-SEP===") parts in
        match non_sep with
        | [] -> ""
        | last :: _ -> "\n    Last compiler output: " ^ (String.sub last 0 (min 200 (String.length last)))
    in
    Printf.eprintf "\nSelf-hosted compiler crashed after compiling %d/%d tests.\n  Error: %s%s\n%!"
      !compiled_count total !batch_err last_output;
    Array.make (List.length tests) (CompileErr ("batch compilation failed: " ^ !batch_err))
  end
  else begin
    (* Parse batch output: lines separated by ===BATCH-SEP=== *)
    let raw = List.rev !output_parts in
    let segments = ref [] in
    let current = Buffer.create 4096 in
    List.iter (fun line ->
      if line = "===BATCH-SEP===" then begin
        segments := Buffer.contents current :: !segments;
        Buffer.clear current
      end else begin
        if Buffer.length current > 0 then Buffer.add_char current '\n';
        Buffer.add_string current line
      end
    ) raw;
    let results = Array.of_list (List.rev !segments) in
    Array.map (fun segment ->
      let s = String.trim segment in
      if String.length s >= 14 && String.sub s 0 14 = "COMPILE-ERROR:" then
        CompileErr (String.sub s 14 (String.length s - 14))
      else
        Compiled s
    ) results
  end

(* Run a precompiled test program *)
let run_selfhost_compiled builtins compile_result =
  match compile_result with
  | CompileErr msg ->
    if is_selfhost_type_error msg then TypeErr msg
    else RuntimeErr msg
  | Compiled test_json ->
    try
      let outputs = ref [] in
      Interpreter.Interp.output_fn := (fun s -> outputs := s :: !outputs);
      let result = Interpreter.Deserialize.load_bundle test_json builtins in
      Interpreter.Interp.output_fn := print_endline;
      let pp = Interpreter.Bytecode.pp_value result in
      let actual =
        let outs = List.rev !outputs in
        if outs <> [] && pp = "()" then String.concat "\n" outs
        else if outs <> [] then String.concat "\n" outs ^ "\n" ^ pp
        else pp
      in
      Ok actual
    with
    | Interpreter.Vm.Runtime_error msg ->
      Interpreter.Interp.output_fn := print_endline;
      RuntimeErr msg
    | Interpreter.Interp.Error msg ->
      Interpreter.Interp.output_fn := print_endline;
      if is_selfhost_type_error msg then TypeErr msg
      else RuntimeErr msg
    | Failure msg ->
      Interpreter.Interp.output_fn := print_endline;
      if is_selfhost_type_error msg then TypeErr msg
      else RuntimeErr msg
    | exn ->
      Interpreter.Interp.output_fn := print_endline;
      OtherErr (Printexc.to_string exn)

(* --- Test running -------------------------------------------------------- *)

let run_tests _state builtins tests selfhost_compiled =
  let passed = ref 0 in
  let failed = ref 0 in
  let failures = ref [] in
  let total = List.length tests in
  let current = ref 0 in
  let fail name fmt =
    Printf.ksprintf (fun msg ->
      Printf.printf "FAIL\n    %s\n%!" msg;
      incr failed;
      failures := name :: !failures
    ) fmt
  in
  List.iter (fun tc ->
    flush stdout;
    incr current;
    Printf.printf "  [%d/%d] %s... %!" !current total tc.name;
    let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
    let ocaml_result = run_ocaml state tc.source in
    let selfhost_result = run_selfhost_compiled builtins selfhost_compiled.(!current - 1) in
    match tc.expect, ocaml_result, selfhost_result with
    (* Both succeed with matching output *)
    | Value expected, Ok ocaml_out, Ok selfhost_out ->
      if ocaml_out = expected && selfhost_out = expected then begin
        Printf.printf "PASS\n%!";
        incr passed
      end else if ocaml_out <> expected then
        fail tc.name "OCaml compiler wrong: expected %S, got %S" expected ocaml_out
      else
        fail tc.name "parity mismatch: ocaml=%S, selfhost=%S" ocaml_out selfhost_out
    (* Both produce type errors *)
    | TypeError, TypeErr _, TypeErr _ ->
      Printf.printf "PASS\n%!";
      incr passed
    | TypeErrorMsg substr, TypeErr ocaml_msg, TypeErr _selfhost_msg ->
      if contains_substring ocaml_msg substr then begin
        Printf.printf "PASS\n%!";
        incr passed
      end else
        fail tc.name "OCaml type error doesn't contain %S: %s" substr ocaml_msg
    (* Both produce runtime errors *)
    | RuntimeError substr, RuntimeErr ocaml_msg, RuntimeErr _selfhost_msg ->
      if contains_substring ocaml_msg substr then begin
        Printf.printf "PASS\n%!";
        incr passed
      end else
        fail tc.name "OCaml runtime error doesn't contain %S: %s" substr ocaml_msg
    (* Parity violations *)
    | Value _, Ok _, TypeErr msg ->
      fail tc.name "OCaml succeeded but selfhost got type error: %s" msg
    | Value _, Ok _, RuntimeErr msg ->
      fail tc.name "OCaml succeeded but selfhost got runtime error: %s" msg
    | Value _, Ok _, OtherErr msg ->
      fail tc.name "OCaml succeeded but selfhost got exception: %s" msg
    | Value _, TypeErr msg, _ ->
      fail tc.name "OCaml got unexpected type error: %s" msg
    | Value _, RuntimeErr msg, _ ->
      fail tc.name "OCaml got unexpected runtime error: %s" msg
    | Value _, OtherErr msg, _ ->
      fail tc.name "OCaml got unexpected exception: %s" msg
    | TypeError, TypeErr _, other | TypeErrorMsg _, TypeErr _, other ->
      let msg = match other with
        | Ok s -> "succeeded with: " ^ s
        | RuntimeErr s -> "runtime error: " ^ s
        | OtherErr s -> "exception: " ^ s
        | TypeErr _ -> "type error (unexpected match)"
      in
      fail tc.name "OCaml got type error but selfhost %s" msg
    | TypeError, other, _ | TypeErrorMsg _, other, _ ->
      let msg = match other with
        | Ok s -> "succeeded with: " ^ s
        | RuntimeErr s -> "runtime error: " ^ s
        | OtherErr s -> "exception: " ^ s
        | TypeErr _ -> "type error (unexpected match)"
      in
      fail tc.name "expected type error but OCaml %s" msg
    | RuntimeError _, RuntimeErr _, other ->
      let msg = match other with
        | Ok s -> "succeeded with: " ^ s
        | TypeErr s -> "type error: " ^ s
        | OtherErr s -> "exception: " ^ s
        | RuntimeErr _ -> "runtime error (unexpected match)"
      in
      fail tc.name "OCaml got runtime error but selfhost %s" msg
    | RuntimeError _, other, _ ->
      let msg = match other with
        | Ok s -> "succeeded with: " ^ s
        | TypeErr s -> "type error: " ^ s
        | OtherErr s -> "exception: " ^ s
        | RuntimeErr _ -> "runtime error (unexpected match)"
      in
      fail tc.name "expected runtime error but OCaml %s" msg
  ) tests;
  Printf.printf "\n%!";
  (!passed, !failed, !failures)

(* --- Directory scanning -------------------------------------------------- *)

let find_test_files dir =
  let files = ref [] in
  let handle = Unix.opendir dir in
  (try while true do
    let entry = Unix.readdir handle in
    if Filename.check_suffix entry ".tests" then
      files := Filename.concat dir entry :: !files
  done with End_of_file -> ());
  Unix.closedir handle;
  List.sort String.compare !files

(* --- Argument parsing ----------------------------------------------------- *)

let parse_args argv =
  let files = ref [] in
  let filters = ref [] in
  let i = ref 1 in
  while !i < Array.length argv do
    if argv.(!i) = "-t" && !i + 1 < Array.length argv then begin
      filters := String.lowercase_ascii argv.(!i + 1) :: !filters;
      i := !i + 2
    end else begin
      files := argv.(!i) :: !files;
      i := !i + 1
    end
  done;
  (List.rev !files, List.rev !filters)

let filter_tests filters tests =
  match filters with
  | [] -> tests
  | _ ->
    List.filter (fun tc ->
      let name_lower = String.lowercase_ascii tc.name in
      List.exists (fun f -> contains_substring name_lower f) filters
    ) tests

(* --- Main ---------------------------------------------------------------- *)

let () =
  (* Load the pre-compiled self-hosted compiler *)
  let compiler_json_path =
    if Sys.file_exists "js/compiler.json" then "js/compiler.json"
    else if Sys.file_exists "../js/compiler.json" then "../js/compiler.json"
    else begin
      Printf.eprintf "Cannot find js/compiler.json\n";
      exit 1
    end
  in
  (* Check if compiler.json is older than any self_host source file *)
  let self_host_files = [
    "self_host/token.mml"; "self_host/ast.mml"; "self_host/bytecode.mml";
    "self_host/types.mml"; "self_host/lexer.mml"; "self_host/parser.mml";
    "self_host/typechecker.mml"; "self_host/compiler.mml";
    "self_host/serialize.mml"; "self_host/main.mml"
  ] in
  let compiler_mtime =
    try (Unix.stat compiler_json_path).Unix.st_mtime
    with _ -> 0.0
  in
  let stale_files = List.filter (fun f ->
    try (Unix.stat f).Unix.st_mtime > compiler_mtime
    with _ -> false
  ) self_host_files in
  if stale_files <> [] then
    Printf.eprintf "WARNING: %s is older than: %s\n  Run 'make self-host-compile' to rebuild.\n%!"
      compiler_json_path (String.concat ", " stale_files);
  Printf.printf "Loading self-hosted compiler from %s...\n%!" compiler_json_path;
  let ic = open_in compiler_json_path in
  let compiler_json_str = In_channel.input_all ic in
  close_in ic;
  Printf.printf "Parsing compiler JSON (%d bytes)...\n%!" (String.length compiler_json_str);
  let compiler_parsed = Interpreter.Deserialize.parse_json compiler_json_str in
  (* Set up a reference builtin table (shared across runs) *)
  let init_state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
  let builtins = Interpreter.Interp.builtin_table init_state in
  Printf.printf "Preparing compiler bundle...\n%!";
  let prepared = Interpreter.Deserialize.prepare_bundle compiler_parsed builtins in
  Printf.printf "Compiler ready.\n%!";
  (* Find test files *)
  let (arg_files, filters) = parse_args Sys.argv in
  let files =
    if arg_files <> [] then arg_files
    else
      try find_test_files "tests" with Unix.Unix_error _ ->
      try find_test_files "cross_test/tests" with Unix.Unix_error _ ->
        Printf.eprintf "No test files found. Usage: parity_runner [file.tests ...] [-t \"name\" ...]\n";
        exit 1
  in
  (* Collect all tests per file *)
  let file_tests = List.map (fun file ->
    (file, filter_tests filters (parse_test_file file))
  ) files in
  let all_tests = List.concat_map snd file_tests in
  Printf.printf "Batch compiling %d tests with self-hosted compiler...\n%!" (List.length all_tests);
  let all_compiled = batch_selfhost_compile prepared all_tests in
  Printf.printf "Batch compilation complete (%d results).\n\n%!" (Array.length all_compiled);
  let total_passed = ref 0 in
  let total_failed = ref 0 in
  let all_failures = ref [] in
  let batch_offset = ref 0 in
  List.iter (fun (file, tests) ->
    Printf.printf "=== %s ===\n%!" (Filename.basename file);
    if tests = [] && filters <> [] then
      Printf.printf "  (no matching tests)\n\n"
    else begin
      (* Fresh state per file so top-level defs don't leak across test files *)
      let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
      let n = List.length tests in
      let file_compiled = Array.sub all_compiled !batch_offset n in
      batch_offset := !batch_offset + n;
      let (p, f, failures) = run_tests state builtins tests file_compiled in
      total_passed := !total_passed + p;
      total_failed := !total_failed + f;
      all_failures := !all_failures @ failures
    end
  ) file_tests;
  Printf.printf "==============================\n";
  Printf.printf "%d/%d parity tests passed"
    !total_passed (!total_passed + !total_failed);
  if !total_failed > 0 then
    Printf.printf " (%d FAILED)" !total_failed;
  Printf.printf "\n";
  if !total_failed > 0 then begin
    Printf.printf "Failures:\n";
    List.iter (fun name -> Printf.printf "  - %s\n" name) !all_failures;
    exit 1
  end
