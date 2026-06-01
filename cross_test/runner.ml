(* Cross-VM test runner — OCaml VM backend
   Parses .t test files and runs each test case through the OCaml interpreter,
   comparing pp_value output against expected results. *)

open Test_format

(* --- Running ------------------------------------------------------------- *)

(* Set by --oracle: run tests with the Oracle reference interpreter instead of
   the compile-to-bytecode + VM path. *)
let use_oracle = ref false

let run_tests _state tests =
  let passed = ref 0 in
  let failed = ref 0 in
  let skipped = ref 0 in
  let failures = ref [] in
  (* Backend dispatch: bytecode VM (default) or the Oracle reference
     interpreter (--oracle). Oracle errors surface as Interp.Error so the
     existing expectation handling applies. *)
  let run_source state source =
    if !use_oracle then
      try Interpreter.Interp.oracle_run_string_in_state state source
      with Interpreter.Oracle.Oracle_error msg ->
        raise (Interpreter.Interp.Error ("Runtime error: " ^ msg))
    else Interpreter.Interp.run_string_in_state state source
  in
  let fail name fmt =
    Printf.ksprintf
      (fun msg ->
        Printf.printf "  FAIL: %s\n    %s\n" name msg;
        incr failed;
        failures := name :: !failures)
      fmt
  in
  List.iter
    (fun tc ->
      flush stdout;
      let state =
        Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ())
      in
      try
        match tc.expect with
        | Value expected ->
            let outputs = ref [] in
            (state.Interpreter.Interp.output_fn :=
               fun s -> outputs := s :: !outputs);
            let result =
              try run_source state tc.source
              with e ->
                state.Interpreter.Interp.output_fn := print_endline;
                raise e
            in
            state.Interpreter.Interp.output_fn := print_endline;
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
            end
            else fail tc.name "expected: %s\n    actual:   %s" expected actual
        | TypeError -> (
            try
              let _ = run_source state tc.source in
              fail tc.name "expected type error, but succeeded"
            with
            | Interpreter.Typechecker.Type_error _ ->
                Printf.printf "  PASS: %s\n" tc.name;
                incr passed
            | Interpreter.Interp.Error msg ->
                if String.starts_with ~prefix:"Type error" msg then begin
                  Printf.printf "  PASS: %s\n" tc.name;
                  incr passed
                end
                else fail tc.name "expected type error, got: %s" msg)
        | TypeErrorMsg substr -> (
            try
              let _ = run_source state tc.source in
              fail tc.name "expected type error, but succeeded"
            with
            | Interpreter.Typechecker.Type_error (msg, _) ->
                if contains_substring msg substr then begin
                  Printf.printf "  PASS: %s\n" tc.name;
                  incr passed
                end
                else
                  fail tc.name "expected type error containing %S, got: %s"
                    substr msg
            | Interpreter.Interp.Error msg ->
                if
                  String.starts_with ~prefix:"Type error" msg
                  && contains_substring msg substr
                then begin
                  Printf.printf "  PASS: %s\n" tc.name;
                  incr passed
                end
                else
                  fail tc.name "expected type error containing %S, got: %s"
                    substr msg)
        | RuntimeError substr -> (
            (state.Interpreter.Interp.output_fn := fun _ -> ());
            try
              let _ = run_source state tc.source in
              state.Interpreter.Interp.output_fn := print_endline;
              fail tc.name "expected runtime error, but succeeded"
            with
            | Interpreter.Vm.Runtime_error msg ->
                state.Interpreter.Interp.output_fn := print_endline;
                if contains_substring msg substr then begin
                  Printf.printf "  PASS: %s\n" tc.name;
                  incr passed
                end
                else
                  fail tc.name "expected runtime error containing %S, got: %s"
                    substr msg
            | Interpreter.Interp.Error msg ->
                state.Interpreter.Interp.output_fn := print_endline;
                if
                  String.starts_with ~prefix:"Runtime error" msg
                  && contains_substring msg substr
                then begin
                  Printf.printf "  PASS: %s\n" tc.name;
                  incr passed
                end
                else
                  fail tc.name "expected runtime error containing %S, got: %s"
                    substr msg)
      with
      | Interpreter.Oracle.Unsupported what ->
          (* Oracle doesn't support this construct yet — tracked, not a failure *)
          state.Interpreter.Interp.output_fn := print_endline;
          Printf.printf "  SKIP: %s (%s)\n" tc.name what;
          incr skipped
      | exn ->
          state.Interpreter.Interp.output_fn := print_endline;
          fail tc.name "exception: %s" (Printexc.to_string exn))
    tests;
  Printf.printf "\n";
  (!passed, !failed, !skipped, !failures)

(* --- Argument parsing ----------------------------------------------------- *)

let parse_args argv =
  let files = ref [] in
  let filters = ref [] in
  let i = ref 1 in
  while !i < Array.length argv do
    if argv.(!i) = "-t" && !i + 1 < Array.length argv then begin
      filters := String.lowercase_ascii argv.(!i + 1) :: !filters;
      i := !i + 2
    end
    else if argv.(!i) = "--oracle" then begin
      (* Run tests with the Oracle reference interpreter instead of the
         compile-to-bytecode + VM path. *)
      use_oracle := true;
      i := !i + 1
    end
    else begin
      files := argv.(!i) :: !files;
      i := !i + 1
    end
  done;
  (List.rev !files, List.rev !filters)

(* --- Main ---------------------------------------------------------------- *)

let () =
  let arg_files, filters = parse_args Sys.argv in
  let files =
    if arg_files <> [] then arg_files
    else
      try find_test_files "tests"
      with Unix.Unix_error _ -> (
        try find_test_files "cross_test/tests"
        with Unix.Unix_error _ ->
          Printf.eprintf
            "No test files found. Usage: runner [file.tests ...] [-t \"name\" \
             ...]\n";
          exit 1)
  in
  let state =
    Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ())
  in
  let total_passed = ref 0 in
  let total_failed = ref 0 in
  let total_skipped = ref 0 in
  let all_failures = ref [] in
  List.iter
    (fun file ->
      Printf.printf "=== %s ===\n" (Filename.basename file);
      let tests = filter_tests filters (parse_test_file file) in
      if tests = [] && filters <> [] then
        Printf.printf "  (no matching tests)\n\n"
      else begin
        let p, f, s, failures = run_tests state tests in
        total_passed := !total_passed + p;
        total_failed := !total_failed + f;
        total_skipped := !total_skipped + s;
        all_failures := !all_failures @ failures
      end)
    files;
  Printf.printf "==============================\n";
  Printf.printf "%d/%d cross-VM tests passed (%s)" !total_passed
    (!total_passed + !total_failed)
    (if !use_oracle then "oracle" else "ocaml");
  if !total_failed > 0 then Printf.printf " (%d FAILED)" !total_failed;
  if !total_skipped > 0 then Printf.printf " (%d skipped)" !total_skipped;
  Printf.printf "\n";
  if !total_failed > 0 then begin
    Printf.printf "Failures:\n";
    List.iter (fun name -> Printf.printf "  - %s\n" name) !all_failures;
    exit 1
  end
