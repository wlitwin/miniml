(* Cross-VM test runner — OCaml VM backend
   Parses .t test files and runs each test case through the OCaml interpreter,
   comparing pp_value output against expected results. *)

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

(* --- Parsing ------------------------------------------------------------ *)

type parse_state =
  | Idle
  | CollectingSource of string (* test name *)

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
    (* --- test: <name> *)
    if String.length trimmed > 9
       && String.sub trimmed 0 9 = "--- test:" then begin
      let name = String.trim (String.sub trimmed 9 (String.length trimmed - 9)) in
      state := CollectingSource name;
      Buffer.clear source_buf
    end
    (* --- expect: <pp_value output> — supports "quoted" values for whitespace *)
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
    (* --- expect-type-error: <substring> *)
    else if String.length trimmed >= 22
            && String.sub trimmed 0 22 = "--- expect-type-error:" then begin
      match !state with
      | CollectingSource name ->
        let substr = String.trim (String.sub trimmed 22 (String.length trimmed - 22)) in
        if substr = "" then flush name TypeError
        else flush name (TypeErrorMsg substr)
      | Idle -> ()
    end
    (* --- expect-type-error *)
    else if trimmed = "--- expect-type-error" then begin
      match !state with
      | CollectingSource name -> flush name TypeError
      | Idle -> ()
    end
    (* --- expect-runtime-error: <substring> *)
    else if String.length trimmed >= 25
            && String.sub trimmed 0 25 = "--- expect-runtime-error:" then begin
      match !state with
      | CollectingSource name ->
        let substr = String.trim (String.sub trimmed 25 (String.length trimmed - 25)) in
        flush name (RuntimeError substr)
      | Idle -> ()
    end
    (* Source line or ignored line *)
    else begin
      match !state with
      | CollectingSource _ ->
        (* Skip section headers and blank lines before first source line *)
        if Buffer.length source_buf > 0 || trimmed <> "" then begin
          if String.length trimmed > 3 && String.sub trimmed 0 3 = "===" then
            () (* skip section headers *)
          else begin
            if Buffer.length source_buf > 0 then Buffer.add_char source_buf '\n';
            Buffer.add_string source_buf line
          end
        end
      | Idle -> () (* skip blank lines, section headers outside tests *)
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

(* --- Running ------------------------------------------------------------- *)

let run_tests _state tests =
  let passed = ref 0 in
  let failed = ref 0 in
  let failures = ref [] in
  let fail name fmt =
    Printf.ksprintf (fun msg ->
      Printf.printf "  FAIL: %s\n    %s\n" name msg;
      incr failed;
      failures := name :: !failures
    ) fmt
  in
  List.iter (fun tc ->
    flush stdout;
    let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
    try
      match tc.expect with
      | Value expected ->
        let outputs = ref [] in
        Interpreter.Interp.output_fn := (fun s -> outputs := s :: !outputs);
        (let result =
          try Interpreter.Interp.run_string_in_state state tc.source
          with e -> Interpreter.Interp.output_fn := print_endline; raise e
        in
        Interpreter.Interp.output_fn := print_endline;
        let pp = Interpreter.Bytecode.pp_value result in
        let actual =
          let outs = List.rev !outputs in
          if outs <> [] && pp = "()" then String.concat "\n" outs
          else if outs <> [] then String.concat "\n" outs ^ "\n" ^ pp
          else pp
        in
        if actual = expected then begin
          Printf.printf "  PASS: %s\n" tc.name;
          incr passed
        end else
          fail tc.name "expected: %s\n    actual:   %s" expected actual)
      | TypeError ->
        (try
          let _ = Interpreter.Interp.run_string_in_state state tc.source in
          fail tc.name "expected type error, but succeeded"
        with
        | Interpreter.Typechecker.Type_error _ ->
          Printf.printf "  PASS: %s\n" tc.name;
          incr passed
        | Interpreter.Interp.Error msg ->
          if String.starts_with ~prefix:"Type error" msg then begin
            Printf.printf "  PASS: %s\n" tc.name;
            incr passed
          end else
            fail tc.name "expected type error, got: %s" msg)
      | TypeErrorMsg substr ->
        (try
          let _ = Interpreter.Interp.run_string_in_state state tc.source in
          fail tc.name "expected type error, but succeeded"
        with
        | Interpreter.Typechecker.Type_error (msg, _) ->
          if contains_substring msg substr then begin
            Printf.printf "  PASS: %s\n" tc.name;
            incr passed
          end else
            fail tc.name "expected type error containing %S, got: %s" substr msg
        | Interpreter.Interp.Error msg ->
          if String.starts_with ~prefix:"Type error" msg
             && contains_substring msg substr then begin
            Printf.printf "  PASS: %s\n" tc.name;
            incr passed
          end else
            fail tc.name "expected type error containing %S, got: %s" substr msg)
      | RuntimeError substr ->
        Interpreter.Interp.output_fn := (fun _ -> ());
        (try
          let _ = Interpreter.Interp.run_string_in_state state tc.source in
          Interpreter.Interp.output_fn := print_endline;
          fail tc.name "expected runtime error, but succeeded"
        with
        | Interpreter.Vm.Runtime_error msg ->
          Interpreter.Interp.output_fn := print_endline;
          if contains_substring msg substr then begin
            Printf.printf "  PASS: %s\n" tc.name;
            incr passed
          end else
            fail tc.name "expected runtime error containing %S, got: %s" substr msg
        | Interpreter.Interp.Error msg ->
          Interpreter.Interp.output_fn := print_endline;
          if String.starts_with ~prefix:"Runtime error" msg
             && contains_substring msg substr then begin
            Printf.printf "  PASS: %s\n" tc.name;
            incr passed
          end else
            fail tc.name "expected runtime error containing %S, got: %s" substr msg)
    with exn ->
      Interpreter.Interp.output_fn := print_endline;
      fail tc.name "exception: %s" (Printexc.to_string exn)
  ) tests;
  Printf.printf "\n";
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
  let (arg_files, filters) = parse_args Sys.argv in
  let files =
    if arg_files <> [] then arg_files
    else
      try find_test_files "tests" with Unix.Unix_error _ ->
      try find_test_files "cross_test/tests" with Unix.Unix_error _ ->
        Printf.eprintf "No test files found. Usage: runner [file.tests ...] [-t \"name\" ...]\n";
        exit 1
  in
  let state = Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ()) in
  let total_passed = ref 0 in
  let total_failed = ref 0 in
  let all_failures = ref [] in
  List.iter (fun file ->
    Printf.printf "=== %s ===\n" (Filename.basename file);
    let tests = filter_tests filters (parse_test_file file) in
    if tests = [] && filters <> [] then
      Printf.printf "  (no matching tests)\n\n"
    else begin
      let (p, f, failures) = run_tests state tests in
      total_passed := !total_passed + p;
      total_failed := !total_failed + f;
      all_failures := !all_failures @ failures
    end
  ) files;
  Printf.printf "==============================\n";
  Printf.printf "%d/%d cross-VM tests passed (ocaml)"
    !total_passed (!total_passed + !total_failed);
  if !total_failed > 0 then
    Printf.printf " (%d FAILED)" !total_failed;
  Printf.printf "\n";
  if !total_failed > 0 then begin
    Printf.printf "Failures:\n";
    List.iter (fun name -> Printf.printf "  - %s\n" name) !all_failures;
    exit 1
  end
