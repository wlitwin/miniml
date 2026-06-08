(* Differential execution — run one MiniML program on multiple backends and
   compare observable behavior. Any disagreement is a bug somewhere: in a
   backend lowering, in the oracle (the spec, lib/oracle.ml), or — rarest —
   a divergence the language definition has to decide.

   This is the comparison engine for differential fuzzing (roadmap Phase 2):
   generated programs have no known-correct answer, so agreement across
   independent implementations IS the test. It is also useful standalone:
   `make diff FILE=prog.mml` answers "do all backends agree on this program?"
   without writing an expected value.

   Design:

   - Each backend runs in a FORKED subprocess. A hung backend (infinite loop)
     times out without hanging the runner; a crashing backend (segfault,
     stack overflow) is reported as Crash instead of taking the runner down.
     The fork inherits the parent's already-built interpreter state (stdlib
     compiled once, copy-on-write), so per-run setup cost is minimal.

   - Observable behavior is normalized to a [result]. For successful runs this
     is the same notion the cross-test runners compare against expected
     values: captured prints followed by the final value. For failures it is
     an error CATEGORY: backends legitimately word their errors differently,
     but "it errored" vs "it produced a value" is a real difference.

   - The verdict is category-based agreement (see [results_agree]); for
     Output, the strings must match exactly. *)

(* ---- Backends ----------------------------------------------------------- *)

type backend =
  | Oracle (* lib/oracle.ml — the reference interpreter / executable spec *)
  | Vm (* compile to bytecode, run on the OCaml VM *)
  | EmitJs (* compile to JavaScript (js_codegen), run with node *)
  | Native (* compile to LLVM IR -> clang -> binary, run it *)

let backend_name = function
  | Oracle -> "oracle"
  | Vm -> "vm"
  | EmitJs -> "emit-js"
  | Native -> "native"

let backend_of_string = function
  | "oracle" -> Some Oracle
  | "vm" -> Some Vm
  | "emit-js" | "emitjs" | "js" -> Some EmitJs
  | "native" -> Some Native
  | _ -> None

(* Oracle first: it is the reference, so reports read "X differs from the
   oracle". *)
let all_backends = [ Oracle; Vm; EmitJs; Native ]

(* The fast tier for high-throughput fuzzing: native costs ~1s per program
   (clang); the rest are tens of milliseconds. *)
let fast_backends = [ Oracle; Vm; EmitJs ]

(* ---- Results ------------------------------------------------------------ *)

type result =
  | Output of string
      (* the program's observable output: prints + final value *)
  | Rejected of string
      (* frontend rejection (lex / parse / type error). All backends share the
         frontend, so rejection is unanimous by construction; a program that
         is Rejected on one backend and runs on another indicates the backends
         were built from different frontends (should be impossible in-tree). *)
  | RuntimeError of string (* the program ran and failed *)
  | BackendError of string
      (* backend-specific COMPILATION failure (codegen / driver / bytecode
         compiler error). Always a finding when other backends run the
         program. *)
  | Timeout (* exceeded the time limit *)
  | Crash of string
      (* abnormal termination: resource limits (stack overflow), signals
         (segfault), or the backend subprocess dying without reporting *)

let result_label = function
  | Output _ -> "output"
  | Rejected _ -> "rejected"
  | RuntimeError _ -> "runtime-error"
  | BackendError _ -> "backend-error"
  | Timeout -> "timeout"
  | Crash _ -> "crash"

let pp_result = function
  | Output s -> Printf.sprintf "output: %s" s
  | Rejected s -> Printf.sprintf "rejected: %s" s
  | RuntimeError s -> Printf.sprintf "runtime error: %s" s
  | BackendError s -> Printf.sprintf "backend error: %s" s
  | Timeout -> "timeout"
  | Crash s -> Printf.sprintf "crash: %s" s

(* Category-based agreement. An equivalence relation: same constructor agrees
   (with string equality for Output); different constructors never do. Error
   MESSAGES are not compared — backends word errors differently. *)
let results_agree (a : result) (b : result) : bool =
  match (a, b) with
  | Output x, Output y -> String.equal x y
  | Rejected _, Rejected _ -> true
  | RuntimeError _, RuntimeError _ -> true
  | BackendError _, BackendError _ -> true
  | Timeout, Timeout -> true
  | Crash _, Crash _ -> true
  | _ -> false

type verdict =
  | Agree of result (* every backend produced (the equivalent of) this *)
  | Disagree of (backend * result) list (* the full table *)

let verdict (results : (backend * result) list) : verdict =
  match results with
  | [] -> invalid_arg "Differential.verdict: no results"
  | (_, first) :: rest ->
      (* results_agree is an equivalence relation, so first-vs-all suffices *)
      if List.for_all (fun (_, r) -> results_agree first r) rest then
        Agree first
      else Disagree results

(* The backends that disagree with the reference (the first listed backend,
   normally the oracle). *)
let dissenters (results : (backend * result) list) : backend list =
  match results with
  | [] -> []
  | (_, reference) :: rest ->
      List.filter_map
        (fun (b, r) -> if results_agree reference r then None else Some b)
        rest

(* ---- Shared interpreter state ------------------------------------------- *)

(* Building the interpreter state compiles the whole stdlib (~100ms). Do it
   once; forks inherit it copy-on-write. The state is MUTATED by runs (output
   capture, REPL ctx threading), which is exactly why every run happens in a
   fork — the parent's copy stays pristine. *)
let make_state () =
  Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ())

(* ---- Output protocol ---------------------------------------------------- *)

(* Combine captured prints and the final value into the observable-output
   string — the same rule as cross_test/runner.ml, which the emit-js and
   native backends' generated code also follows (the cross-test suite locks
   this protocol across all backends). *)
let observable outputs value_pp =
  match outputs with
  | [] -> value_pp
  | outs ->
      if String.equal value_pp "()" then String.concat "\n" outs
      else String.concat "\n" outs ^ "\n" ^ value_pp

let strip_trailing_newline s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\n' then String.sub s 0 (len - 1) else s

(* Categorize an Interp.Error message (the wrapped form of frontend/runtime
   errors) by its prefix. *)
let categorize_error_message msg =
  let has_prefix p = String.length msg >= String.length p
                     && String.sub msg 0 (String.length p) = p in
  if has_prefix "Type error" || has_prefix "Lex error" || has_prefix "Parse error"
  then Rejected msg
  else if has_prefix "Compile error" then BackendError msg
  else RuntimeError msg

(* ---- In-process backends: VM and Oracle --------------------------------- *)

(* Run [eval] (which returns the program's final value) with print output
   captured, and normalize exceptions into result categories. *)
let run_in_process state (eval : unit -> Interpreter.Bytecode.value) : result =
  let outputs = ref [] in
  (state.Interpreter.Interp.output_fn := fun s -> outputs := s :: !outputs);
  match eval () with
  | value ->
      Output
        (observable (List.rev !outputs) (Interpreter.Bytecode.pp_value value))
  | exception Interpreter.Lexer.Lex_error (msg, _) ->
      Rejected ("Lex error: " ^ msg)
  | exception Interpreter.Parser.Parse_error (msg, _) ->
      Rejected ("Parse error: " ^ msg)
  | exception Interpreter.Typechecker.Type_error (msg, _) ->
      Rejected ("Type error: " ^ msg)
  | exception Interpreter.Compiler.Compile_error msg ->
      BackendError ("Compile error: " ^ msg)
  | exception Interpreter.Vm.Runtime_error msg -> RuntimeError msg
  | exception Interpreter.Oracle.Oracle_error msg -> RuntimeError msg
  | exception Interpreter.Oracle.Unsupported msg -> BackendError msg
  | exception Interpreter.Interp.Error msg -> categorize_error_message msg
  | exception Stack_overflow -> Crash "stack overflow"

let run_vm state source : result =
  run_in_process state (fun () ->
      Interpreter.Interp.run_string_in_state state source)

let run_oracle state source : result =
  run_in_process state (fun () ->
      Interpreter.Interp.oracle_run_string_in_state state source)

(* ---- Subprocess backends: emit-js and native ----------------------------- *)

(* Run a shell command capturing stdout and stderr separately.
   Returns (exit_code, stdout, stderr). *)
let run_command_capture cmd : int * string * string =
  let stdout_f = Filename.temp_file "diff_stdout" ".txt" in
  let stderr_f = Filename.temp_file "diff_stderr" ".txt" in
  let full =
    Printf.sprintf "%s > %s 2> %s" cmd (Filename.quote stdout_f)
      (Filename.quote stderr_f)
  in
  let code = Sys.command full in
  let read_all f =
    let ic = open_in_bin f in
    let s = In_channel.input_all ic in
    close_in ic;
    s
  in
  let out = read_all stdout_f in
  let err = read_all stderr_f in
  (try Sys.remove stdout_f with Sys_error _ -> ());
  (try Sys.remove stderr_f with Sys_error _ -> ());
  (code, out, err)

(* Categorize the result of running a compiled program (node script or native
   binary): exit 0 -> its stdout is the observable output; non-zero -> the
   program failed at runtime. *)
let result_of_execution exit_code stdout stderr : result =
  if exit_code = 0 then Output (strip_trailing_newline stdout)
  else
    RuntimeError
      (strip_trailing_newline (if String.length stderr > 0 then stderr else stdout))

let run_emit_js state source : result =
  match Interpreter.Interp.emit_js state ~source () with
  | js ->
      let js_file = Filename.temp_file "diff_prog" ".js" in
      let oc = open_out js_file in
      output_string oc js;
      close_out oc;
      let code, out, err =
        run_command_capture (Printf.sprintf "node %s" (Filename.quote js_file))
      in
      (try Sys.remove js_file with Sys_error _ -> ());
      result_of_execution code out err
  | exception Interpreter.Lexer.Lex_error (msg, _) ->
      Rejected ("Lex error: " ^ msg)
  | exception Interpreter.Parser.Parse_error (msg, _) ->
      Rejected ("Parse error: " ^ msg)
  | exception Interpreter.Typechecker.Type_error (msg, _) ->
      Rejected ("Type error: " ^ msg)
  | exception Interpreter.Js_codegen.Codegen_error msg ->
      BackendError ("JS codegen error: " ^ msg)
  | exception Interpreter.Interp.Error msg -> categorize_error_message msg
  | exception Stack_overflow -> Crash "stack overflow (during compilation)"

let run_native source : result =
  let src_file = Filename.temp_file "diff_prog" ".mml" in
  let bin_file = Filename.temp_file "diff_prog" ".bin" in
  let oc = open_out src_file in
  output_string oc source;
  close_out oc;
  let cleanup () =
    (try Sys.remove src_file with Sys_error _ -> ());
    try Sys.remove bin_file with Sys_error _ -> ()
  in
  match
    Interpreter_native.Driver.compile_to_native ~source_file:src_file
      ~output:bin_file ()
  with
  | () ->
      let code, out, err = run_command_capture (Filename.quote bin_file) in
      cleanup ();
      result_of_execution code out err
  | exception Interpreter.Lexer.Lex_error (msg, _) ->
      cleanup ();
      Rejected ("Lex error: " ^ msg)
  | exception Interpreter.Parser.Parse_error (msg, _) ->
      cleanup ();
      Rejected ("Parse error: " ^ msg)
  | exception Interpreter.Typechecker.Type_error (msg, _) ->
      cleanup ();
      Rejected ("Type error: " ^ msg)
  | exception Interpreter_native.Driver.Driver_error msg ->
      cleanup ();
      BackendError ("Native driver error: " ^ msg)
  | exception Failure msg ->
      cleanup ();
      BackendError ("Native codegen failure: " ^ msg)
  | exception Interpreter.Interp.Error msg ->
      cleanup ();
      categorize_error_message msg
  | exception Stack_overflow ->
      cleanup ();
      Crash "stack overflow (during compilation)"

(* ---- Fork isolation with timeout ----------------------------------------- *)

(* Run [f] in a forked child with a wall-clock timeout. The child gets its own
   session/process group so that on timeout we can kill it together with any
   grandchildren it spawned (node, clang, the compiled binary). The result is
   marshalled back over a pipe. A child that dies without reporting (segfault,
   OOM kill) is a Crash. *)
let run_isolated ~(timeout : float) (f : unit -> result) : result =
  let r_fd, w_fd = Unix.pipe () in
  match Unix.fork () with
  | 0 -> (
      (* Child *)
      ignore (Unix.setsid ());
      Unix.close r_fd;
      let result = try f () with exn -> Crash (Printexc.to_string exn) in
      let oc = Unix.out_channel_of_descr w_fd in
      (try
         Marshal.to_channel oc result [];
         flush oc
       with _ -> ());
      (* _exit: skip at_exit handlers and inherited buffer flushing *)
      Unix._exit 0)
  | pid ->
      (* Parent *)
      Unix.close w_fd;
      let result =
        match Unix.select [ r_fd ] [] [] timeout with
        | [], _, _ ->
            (* Timeout: kill the child's whole process group *)
            (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
            Timeout
        | _ -> (
            let ic = Unix.in_channel_of_descr r_fd in
            match (Marshal.from_channel ic : result) with
            | result -> result
            | exception _ ->
                Crash "backend subprocess died without reporting a result")
      in
      (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
      (try Unix.close r_fd with Unix.Unix_error _ -> ());
      result

(* ---- Public API ----------------------------------------------------------- *)

let run_backend ~state ~timeout (backend : backend) (source : string) : result
    =
  run_isolated ~timeout (fun () ->
      match backend with
      | Oracle -> run_oracle state source
      | Vm -> run_vm state source
      | EmitJs -> run_emit_js state source
      | Native -> run_native source)

(* Run [source] on every backend (sequentially; the fuzzer parallelizes across
   programs, not backends) and return the result table in the given order. *)
let run_all ?(backends = all_backends) ~state ~timeout (source : string) :
    (backend * result) list =
  List.map (fun b -> (b, run_backend ~state ~timeout b source)) backends
