(* Native backend test runner.
   Parses .tests files (same format as cross_test) and runs each test case
   through the native compiler (LLVM IR -> clang -> binary), comparing
   stdout output against expected results. *)

(* --- Test file parsing (reused from cross_test/runner.ml) --- *)

type expectation =
  | Value of string
  | TypeError
  | TypeErrorMsg of string
  | RuntimeError of string

type test_case = {
  name : string;
  source : string;
  expect : expectation;
  expect_native : expectation option;
}

type parse_state =
  | Idle
  | CollectingSource of string

let parse_test_file filename =
  let ic = open_in filename in
  let tests = ref [] in
  let state = ref Idle in
  let source_buf = Buffer.create 256 in
  let pending_native = ref None in
  let flush name expect =
    let source = String.trim (Buffer.contents source_buf) in
    tests := { name; source; expect; expect_native = !pending_native } :: !tests;
    state := Idle;
    pending_native := None;
    Buffer.clear source_buf
  in
  (try while true do
    let line = input_line ic in
    let trimmed = String.trim line in
    if String.length trimmed > 9
       && String.sub trimmed 0 9 = "--- test:" then begin
      let name = String.trim (String.sub trimmed 9 (String.length trimmed - 9)) in
      state := CollectingSource name;
      pending_native := None;
      Buffer.clear source_buf
    end
    else if String.length line > 19
            && String.sub line 0 19 = "--- expect-native: " then begin
      match !state with
      | CollectingSource _ ->
        let raw = String.sub line 19 (String.length line - 19) in
        let expected =
          let len = String.length raw in
          if len >= 2 && raw.[0] = '"' && raw.[len - 1] = '"'
          then String.sub raw 1 (len - 2)
          else raw
        in
        pending_native := Some (Value expected)
      | Idle -> ()
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

(* --- Helpers --- *)

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

(* Run a command and capture stdout+stderr *)
let run_command cmd =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let buf = Buffer.create 256 in
  (try while true do
    let line = input_line ic in
    if Buffer.length buf > 0 then Buffer.add_char buf '\n';
    Buffer.add_string buf line
  done with End_of_file -> ());
  let status = Unix.close_process_in ic in
  let output = Buffer.contents buf in
  match status with
  | Unix.WEXITED code -> (code, output)
  | _ -> (1, output)

(* --- Running tests --- *)

let run_tests tests =
  let passed = ref 0 in
  let failed = ref 0 in
  let skipped = ref 0 in
  let failures = ref [] in
  let skip name reason =
    Printf.printf "  SKIP: %s (%s)\n" name reason;
    incr skipped
  in
  let fail name fmt =
    Printf.ksprintf (fun msg ->
      Printf.printf "  FAIL: %s\n    %s\n" name msg;
      incr failed;
      failures := name :: !failures
    ) fmt
  in
  let tmp_src = Filename.temp_file "mml_test_" ".mml" in
  let tmp_bin = Filename.temp_file "mml_test_" "_bin" in
  List.iter (fun tc ->
    flush stdout;
    match tc.expect with
    | Value _ ->
      let expected = match tc.expect_native with
        | Some (Value v) -> v
        | _ -> (match tc.expect with Value v -> v | _ -> "")
      in
      (* Write source to temp file *)
      let oc = open_out tmp_src in
      output_string oc tc.source;
      close_out oc;
      (* Try to compile *)
      (try
        Interpreter_native.Driver.compile_to_native ~source_file:tmp_src ~output:tmp_bin;
        (* Run the binary *)
        let (exit_code, output) = run_command tmp_bin in
        (* Strip trailing newline only (not all whitespace — format tests need padding) *)
        let actual =
          let len = String.length output in
          if len > 0 && output.[len - 1] = '\n' then String.sub output 0 (len - 1)
          else output
        in
        if exit_code <> 0 then
          fail tc.name "binary exited with code %d, output: %s" exit_code actual
        else if actual = expected then begin
          Printf.printf "  PASS: %s\n" tc.name;
          incr passed
        end else
          fail tc.name "expected: %s\n    actual:   %s" expected actual
      with
      | Failure msg when false ->
        skip tc.name (String.sub msg 0 (min 80 (String.length msg)))
      | Failure msg ->
        fail tc.name "codegen failure: %s" msg
      | Interpreter.Typechecker.Type_error (msg, _) ->
        fail tc.name "unexpected type error: %s" msg
      | Interpreter_native.Driver.Driver_error msg ->
        fail tc.name "driver error: %s" msg
      | exn ->
        fail tc.name "exception: %s" (Printexc.to_string exn))
    | TypeError ->
      let oc = open_out tmp_src in
      output_string oc tc.source;
      close_out oc;
      (try
        let _ir = Interpreter_native.Driver.compile_ir tc.source in
        fail tc.name "expected type error, but compiled successfully"
      with
      | Interpreter.Typechecker.Type_error _ ->
        Printf.printf "  PASS: %s\n" tc.name;
        incr passed
      | Interpreter.Interp.Error msg when String.length msg > 10
        && String.sub msg 0 10 = "Type error" ->
        Printf.printf "  PASS: %s\n" tc.name;
        incr passed
      | exn ->
        fail tc.name "expected type error, got: %s" (Printexc.to_string exn))
    | TypeErrorMsg substr ->
      let oc = open_out tmp_src in
      output_string oc tc.source;
      close_out oc;
      (try
        let _ir = Interpreter_native.Driver.compile_ir tc.source in
        fail tc.name "expected type error, but compiled successfully"
      with
      | Interpreter.Typechecker.Type_error (msg, _) ->
        if contains_substring msg substr then begin
          Printf.printf "  PASS: %s\n" tc.name;
          incr passed
        end else
          fail tc.name "expected type error containing %S, got: %s" substr msg
      | Interpreter.Interp.Error msg when contains_substring msg substr ->
        Printf.printf "  PASS: %s\n" tc.name;
        incr passed
      | exn ->
        fail tc.name "expected type error, got: %s" (Printexc.to_string exn))
    | RuntimeError substr ->
      (* Runtime errors: compile and run, expect non-zero exit with matching stderr *)
      let oc = open_out tmp_src in
      output_string oc tc.source;
      close_out oc;
      (try
        Interpreter_native.Driver.compile_to_native ~source_file:tmp_src ~output:tmp_bin;
        let (exit_code, output) = run_command tmp_bin in
        if exit_code <> 0 && contains_substring output substr then begin
          Printf.printf "  PASS: %s\n" tc.name;
          incr passed
        end else if exit_code = 0 then
          fail tc.name "expected runtime error, but exited successfully with: %s" (String.trim output)
        else
          fail tc.name "expected error containing %S, got: %s" substr (String.trim output)
      with
      | Failure msg when contains_substring msg "not yet implemented" ||
                         contains_substring msg "Phase" ->
        skip tc.name (String.sub msg 0 (min 80 (String.length msg)))
      | Failure msg ->
        fail tc.name "codegen failure: %s" msg
      | Interpreter.Typechecker.Type_error (msg, _) ->
        fail tc.name "unexpected type error: %s" msg
      | Interpreter_native.Driver.Driver_error msg ->
        fail tc.name "driver error: %s" msg
      | exn ->
        fail tc.name "exception: %s" (Printexc.to_string exn))
  ) tests;
  (try Sys.remove tmp_src with _ -> ());
  (try Sys.remove tmp_bin with _ -> ());
  Printf.printf "\n";
  (!passed, !failed, !skipped, !failures)

(* --- Argument parsing --- *)

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

(* --- Main --- *)

let () =
  let (arg_files, filters) = parse_args Sys.argv in
  let files =
    if arg_files <> [] then arg_files
    else begin
      Printf.eprintf "Usage: native_test_runner [file.tests ...] [-t \"filter\"]\n";
      exit 1
    end
  in
  let total_passed = ref 0 in
  let total_failed = ref 0 in
  let total_skipped = ref 0 in
  let all_failures = ref [] in
  List.iter (fun file ->
    Printf.printf "=== %s ===\n" (Filename.basename file);
    let tests = filter_tests filters (parse_test_file file) in
    if tests = [] && filters <> [] then
      Printf.printf "  (no matching tests)\n\n"
    else begin
      let (p, f, s, failures) = run_tests tests in
      total_passed := !total_passed + p;
      total_failed := !total_failed + f;
      total_skipped := !total_skipped + s;
      all_failures := !all_failures @ failures
    end
  ) files;
  Printf.printf "==============================\n";
  Printf.printf "%d passed, %d failed, %d skipped (native)"
    !total_passed !total_failed !total_skipped;
  Printf.printf "\n";
  if !total_failed > 0 then begin
    Printf.printf "Failures:\n";
    List.iter (fun name -> Printf.printf "  - %s\n" name) !all_failures;
    exit 1
  end
