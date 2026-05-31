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
  skip_native : string option;  (* reason; test is skipped on the native backend *)
}

type parse_state = Idle | CollectingSource of string

let parse_test_file filename =
  let ic = open_in filename in
  let tests = ref [] in
  let state = ref Idle in
  let source_buf = Buffer.create 256 in
  let pending_native = ref None in
  let pending_skip = ref None in
  let flush name expect =
    let source = String.trim (Buffer.contents source_buf) in
    tests :=
      {
        name;
        source;
        expect;
        expect_native = !pending_native;
        skip_native = !pending_skip;
      }
      :: !tests;
    state := Idle;
    pending_native := None;
    pending_skip := None;
    Buffer.clear source_buf
  in
  (try
     while true do
       let line = input_line ic in
       let trimmed = String.trim line in
       if String.length trimmed > 9 && String.sub trimmed 0 9 = "--- test:" then begin
         let name =
           String.trim (String.sub trimmed 9 (String.length trimmed - 9))
         in
         state := CollectingSource name;
         pending_native := None;
         pending_skip := None;
         Buffer.clear source_buf
       end
       else if
         String.length line > 17 && String.sub line 0 17 = "--- skip-native: "
       then begin
         match !state with
         | CollectingSource _ ->
             pending_skip :=
               Some
                 (String.trim (String.sub line 17 (String.length line - 17)))
         | Idle -> ()
       end
       else if
         String.length line > 19 && String.sub line 0 19 = "--- expect-native: "
       then begin
         match !state with
         | CollectingSource _ ->
             let raw = String.sub line 19 (String.length line - 19) in
             let expected =
               let len = String.length raw in
               if len >= 2 && raw.[0] = '"' && raw.[len - 1] = '"' then
                 String.sub raw 1 (len - 2)
               else raw
             in
             pending_native := Some (Value expected)
         | Idle -> ()
       end
       else if String.length line > 12 && String.sub line 0 12 = "--- expect: "
       then begin
         match !state with
         | CollectingSource name ->
             let raw = String.sub line 12 (String.length line - 12) in
             let expected =
               let len = String.length raw in
               if len >= 2 && raw.[0] = '"' && raw.[len - 1] = '"' then
                 String.sub raw 1 (len - 2)
               else raw
             in
             flush name (Value expected)
         | Idle -> ()
       end
       else if
         String.length trimmed >= 22
         && String.sub trimmed 0 22 = "--- expect-type-error:"
       then begin
         match !state with
         | CollectingSource name ->
             let substr =
               String.trim (String.sub trimmed 22 (String.length trimmed - 22))
             in
             if substr = "" then flush name TypeError
             else flush name (TypeErrorMsg substr)
         | Idle -> ()
       end
       else if trimmed = "--- expect-type-error" then begin
         match !state with
         | CollectingSource name -> flush name TypeError
         | Idle -> ()
       end
       else if
         String.length trimmed >= 25
         && String.sub trimmed 0 25 = "--- expect-runtime-error:"
       then begin
         match !state with
         | CollectingSource name ->
             let substr =
               String.trim (String.sub trimmed 25 (String.length trimmed - 25))
             in
             flush name (RuntimeError substr)
         | Idle -> ()
       end
       else begin
         match !state with
         | CollectingSource _ ->
             if Buffer.length source_buf > 0 || trimmed <> "" then begin
               if
                 (String.length trimmed > 3 && String.sub trimmed 0 3 = "===")
                 || (String.length trimmed > 4 && String.sub trimmed 0 4 = "--- ")
               then () (* skip section banners and directives meant for other backends
                          (e.g. --- skip-emit-js:) — don't absorb them into the source *)
               else begin
                 if Buffer.length source_buf > 0 then
                   Buffer.add_char source_buf '\n';
                 Buffer.add_string source_buf line
               end
             end
         | Idle -> ()
       end
     done
   with End_of_file -> ());
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
  (try
     while true do
       let line = input_line ic in
       if Buffer.length buf > 0 then Buffer.add_char buf '\n';
       Buffer.add_string buf line
     done
   with End_of_file -> ());
  let status = Unix.close_process_in ic in
  let output = Buffer.contents buf in
  match status with Unix.WEXITED code -> (code, output) | _ -> (1, output)

(* --- Running tests --- *)

(* Outcome of a single test, computed without any printing so workers can run in
   parallel and the parent can print results in deterministic (source) order. *)
type result = RPass | RFail of string | RSkip of string

(* Run one test through the native pipeline using the given (worker-local) temp
   files. Pure w.r.t. shared state — returns the outcome instead of printing. *)
let run_one ~tmp_src ~tmp_bin tc : result =
  match tc.skip_native with
  | Some reason -> RSkip reason
  | None -> (
      match tc.expect with
      | Value _ -> (
          let expected =
            match tc.expect_native with
            | Some (Value v) -> v
            | _ -> ( match tc.expect with Value v -> v | _ -> "")
          in
          let oc = open_out tmp_src in
          output_string oc tc.source;
          close_out oc;
          try
            Interpreter_native.Driver.compile_to_native ~source_file:tmp_src
              ~output:tmp_bin;
            let exit_code, output = run_command tmp_bin in
            (* Strip trailing newline only (not all whitespace — format tests need padding) *)
            let actual =
              let len = String.length output in
              if len > 0 && output.[len - 1] = '\n' then
                String.sub output 0 (len - 1)
              else output
            in
            if exit_code <> 0 then
              RFail
                (Printf.sprintf "binary exited with code %d, output: %s"
                   exit_code actual)
            else if actual = expected then RPass
            else
              RFail (Printf.sprintf "expected: %s\n    actual:   %s" expected actual)
          with
          | Failure msg -> RFail (Printf.sprintf "codegen failure: %s" msg)
          | Interpreter.Typechecker.Type_error (msg, _) ->
              RFail (Printf.sprintf "unexpected type error: %s" msg)
          | Interpreter_native.Driver.Driver_error msg ->
              RFail (Printf.sprintf "driver error: %s" msg)
          | exn -> RFail (Printf.sprintf "exception: %s" (Printexc.to_string exn)))
      | TypeError -> (
          let oc = open_out tmp_src in
          output_string oc tc.source;
          close_out oc;
          try
            let _ir = Interpreter_native.Driver.compile_ir tc.source in
            RFail "expected type error, but compiled successfully"
          with
          | Interpreter.Typechecker.Type_error _ -> RPass
          | Interpreter.Interp.Error msg
            when String.length msg > 10 && String.sub msg 0 10 = "Type error" ->
              RPass
          | exn ->
              RFail
                (Printf.sprintf "expected type error, got: %s"
                   (Printexc.to_string exn)))
      | TypeErrorMsg substr -> (
          let oc = open_out tmp_src in
          output_string oc tc.source;
          close_out oc;
          try
            let _ir = Interpreter_native.Driver.compile_ir tc.source in
            RFail "expected type error, but compiled successfully"
          with
          | Interpreter.Typechecker.Type_error (msg, _) ->
              if contains_substring msg substr then RPass
              else
                RFail
                  (Printf.sprintf "expected type error containing %S, got: %s"
                     substr msg)
          | Interpreter.Interp.Error msg when contains_substring msg substr ->
              RPass
          | exn ->
              RFail
                (Printf.sprintf "expected type error, got: %s"
                   (Printexc.to_string exn)))
      | RuntimeError substr -> (
          (* Runtime errors: compile and run, expect non-zero exit with matching stderr *)
          let oc = open_out tmp_src in
          output_string oc tc.source;
          close_out oc;
          try
            Interpreter_native.Driver.compile_to_native ~source_file:tmp_src
              ~output:tmp_bin;
            let exit_code, output = run_command tmp_bin in
            if exit_code <> 0 && contains_substring output substr then RPass
            else if exit_code = 0 then
              RFail
                (Printf.sprintf
                   "expected runtime error, but exited successfully with: %s"
                   (String.trim output))
            else
              RFail
                (Printf.sprintf "expected error containing %S, got: %s" substr
                   (String.trim output))
          with
          | Failure msg
            when contains_substring msg "not yet implemented"
                 || contains_substring msg "Phase" ->
              RSkip (String.sub msg 0 (min 80 (String.length msg)))
          | Failure msg -> RFail (Printf.sprintf "codegen failure: %s" msg)
          | Interpreter.Typechecker.Type_error (msg, _) ->
              RFail (Printf.sprintf "unexpected type error: %s" msg)
          | Interpreter_native.Driver.Driver_error msg ->
              RFail (Printf.sprintf "driver error: %s" msg)
          | exn -> RFail (Printf.sprintf "exception: %s" (Printexc.to_string exn))))

(* Number of parallel workers. Each test is independent and dominated by an external
   clang invocation, so this scales near-linearly with cores. Override with
   NATIVE_TEST_JOBS (e.g. =1 for deterministic, easy-to-debug sequential runs). *)
let worker_count () =
  match Sys.getenv_opt "NATIVE_TEST_JOBS" with
  | Some s -> ( try max 1 (int_of_string s) with _ -> 1)
  | None -> ( try max 1 (Domain.recommended_domain_count ()) with _ -> 1)

(* Run every test, returning a result array indexed by position in [all_tests].
   Uses a pool of forked worker PROCESSES (not Domains): the native Driver carries
   module-level mutable state that is not safe to share across concurrent domains,
   whereas separate processes are fully isolated. Each worker owns its temp files
   and marshals its (index, result) pairs back through a temp file; the parent reads
   them after every worker exits, so printing stays in source order. *)
let run_all results_init all_tests =
  let total = Array.length all_tests in
  let results = Array.make (max 1 total) results_init in
  if total = 0 then [||]
  else begin
    let nworkers = min (worker_count ()) total in
    if nworkers <= 1 then begin
      let tmp_src = Filename.temp_file "mml_test_" ".mml" in
      let tmp_bin = Filename.temp_file "mml_test_" "_bin" in
      Array.iteri (fun i tc -> results.(i) <- run_one ~tmp_src ~tmp_bin tc) all_tests;
      (try Sys.remove tmp_src with _ -> ());
      (try Sys.remove tmp_bin with _ -> ());
      results
    end
    else begin
      flush stdout;
      flush stderr;
      let result_files =
        Array.init nworkers (fun _ -> Filename.temp_file "mml_res_" ".dat")
      in
      let pids = Array.make nworkers (-1) in
      for w = 0 to nworkers - 1 do
        match Unix.fork () with
        | 0 ->
            (* Child: handle the round-robin slice {i | i mod nworkers = w}. *)
            let tmp_src = Filename.temp_file "mml_test_" ".mml" in
            let tmp_bin = Filename.temp_file "mml_test_" "_bin" in
            let mine = ref [] in
            let i = ref w in
            while !i < total do
              let r =
                try run_one ~tmp_src ~tmp_bin all_tests.(!i)
                with exn ->
                  RFail
                    (Printf.sprintf "worker exception: %s"
                       (Printexc.to_string exn))
              in
              mine := (!i, r) :: !mine;
              i := !i + nworkers
            done;
            (try Sys.remove tmp_src with _ -> ());
            (try Sys.remove tmp_bin with _ -> ());
            let oc = open_out_bin result_files.(w) in
            Marshal.to_channel oc !mine [];
            close_out oc;
            (* _exit: skip at_exit/stdout-flush so we never duplicate parent output. *)
            Unix._exit 0
        | pid -> pids.(w) <- pid
      done;
      Array.iter (fun pid -> ignore (Unix.waitpid [] pid)) pids;
      (* Unreported indices keep [results_init] (a sentinel failure), so a worker
         that died without writing surfaces loudly rather than as silent passes. *)
      Array.iter
        (fun rf ->
          (try
             let ic = open_in_bin rf in
             let lst : (int * result) list = Marshal.from_channel ic in
             close_in ic;
             List.iter (fun (i, r) -> results.(i) <- r) lst
           with _ -> ());
          try Sys.remove rf with _ -> ())
        result_files;
      results
    end
  end

(* --- Argument parsing --- *)

let parse_args argv =
  let files = ref [] in
  let filters = ref [] in
  let i = ref 1 in
  while !i < Array.length argv do
    if argv.(!i) = "-t" && !i + 1 < Array.length argv then begin
      filters := String.lowercase_ascii argv.(!i + 1) :: !filters;
      i := !i + 2
    end
    else begin
      files := argv.(!i) :: !files;
      i := !i + 1
    end
  done;
  (List.rev !files, List.rev !filters)

let filter_tests filters tests =
  match filters with
  | [] -> tests
  | _ ->
      List.filter
        (fun tc ->
          let name_lower = String.lowercase_ascii tc.name in
          List.exists (fun f -> contains_substring name_lower f) filters)
        tests

(* --- Main --- *)

let () =
  let arg_files, filters = parse_args Sys.argv in
  let files =
    if arg_files <> [] then arg_files
    else begin
      Printf.eprintf
        "Usage: native_test_runner [file.tests ...] [-t \"filter\"]\n";
      exit 1
    end
  in
  (* Parse + filter each file up front, preserving order, so we can run every test
     across all files through one worker pool (maximizing utilization) while still
     printing results grouped per file. *)
  let groups =
    List.map
      (fun file ->
        (Filename.basename file, Array.of_list (filter_tests filters (parse_test_file file))))
      files
  in
  let all_tests = Array.concat (List.map snd groups) in
  let results = run_all (RFail "worker did not report a result") all_tests in
  let total_passed = ref 0 in
  let total_failed = ref 0 in
  let total_skipped = ref 0 in
  let all_failures = ref [] in
  let pos = ref 0 in
  List.iter
    (fun (basename, tests) ->
      Printf.printf "=== %s ===\n" basename;
      if Array.length tests = 0 && filters <> [] then
        Printf.printf "  (no matching tests)\n\n"
      else begin
        Array.iter
          (fun tc ->
            let r = results.(!pos) in
            incr pos;
            match r with
            | RPass ->
                Printf.printf "  PASS: %s\n" tc.name;
                incr total_passed
            | RSkip reason ->
                Printf.printf "  SKIP: %s (%s)\n" tc.name reason;
                incr total_skipped
            | RFail msg ->
                Printf.printf "  FAIL: %s\n    %s\n" tc.name msg;
                incr total_failed;
                all_failures := tc.name :: !all_failures)
          tests;
        Printf.printf "\n"
      end)
    groups;
  Printf.printf "==============================\n";
  Printf.printf "%d passed, %d failed, %d skipped (native)" !total_passed
    !total_failed !total_skipped;
  Printf.printf "\n";
  if !total_failed > 0 then begin
    Printf.printf "Failures:\n";
    List.iter (fun name -> Printf.printf "  - %s\n" name) (List.rev !all_failures);
    exit 1
  end
