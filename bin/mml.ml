(* `mml` — the MiniML all-in-one toolchain binary (roadmap #24). Go-style
   subcommands over the reference compiler library: run / fmt / check / lsp.
   Each command is a thin shell over Interpreter.{Interp,Formatter,Analysis,Lsp}
   — the build-internal multi-flag entry point (bin/main.ml) is left untouched. *)

module I = Interpreter
module N = Interpreter_native

let version = "0.1.0-dev"

let usage =
  "mml — the MiniML toolchain\n\n\
   Usage: mml <command> [args]\n\n\
   A <target> is a single .mml file or a project directory (one with an mml.mod\n\
   manifest; each foo.mml becomes module Foo, main.mml is the entry).\n\n\
   Commands:\n\
  \  run <target> [args...]    typecheck, compile and run\n\
  \  build [-o OUT] [--emit native|ir] <target>\n\
  \                            compile to a native executable (default) or LLVM IR\n\
  \  fmt [-w] <files...>       format to stdout, or rewrite in place with -w\n\
  \  check <files...>          print diagnostics (exit 1 if any error)\n\
  \  test [dir]                run `let test_* () = <bool>` in *_test.mml files\n\
  \  get <url>[@version]       fetch a dependency and add it to mml.mod\n\
  \  lsp                       start the language server (stdio JSON-RPC)\n\
  \  version                   print the version\n\
  \  help                      show this help\n"

let read_file path =
  let ic = open_in_bin path in
  let s = In_channel.input_all ic in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(* The source to compile/run for [target]: a project directory's combined source
   (libraries wrapped as modules, in dependency order, + the entry), or a single
   file's contents. *)
let source_of target =
  if I.Project.is_project target then
    I.Project.combined_source (I.Project.load target)
  else read_file target

module D = I.Diagnostic

(* Diagnostics for a target as (file, line, col, severity, code, message). For a
   project, spans are mapped from the combined source back to the originating
   file via the line map; for a single file they're verbatim. *)
let target_diagnostics st target =
  let conv map (d : D.t) =
    let file, line = map d.D.span.D.lo.D.line in
    (file, line, d.D.span.D.lo.D.col, d.D.severity, d.D.code, d.D.message)
  in
  if I.Project.is_project target then
    let combined, segs = I.Project.combined_with_map (I.Project.load target) in
    List.map (conv (I.Project.map_line segs)) (I.Analysis.diagnostics st combined)
  else List.map (conv (fun l -> (target, l))) (I.Analysis.diagnostics st (read_file target))

let diag_line (file, line, col, _sev, code, msg) =
  Printf.sprintf "%s:%d:%d: [%s] %s" file line col code msg

let has_errors ds = List.exists (fun (_, _, _, sev, _, _) -> sev = D.Error) ds

(* Print a project's diagnostics (to stderr) when a run/build of it failed, so
   the error points at the right file:line instead of the combined source. *)
let report_project st target =
  if I.Project.is_project target then
    List.iter
      (fun d -> Printf.eprintf "%s\n" (diag_line d))
      (try target_diagnostics st target with _ -> [])

let cmd_run target args =
  let st = I.Std.register_all (I.Interp.repl_state_init ()) in
  st.I.Interp.argv := Array.of_list (target :: args);
  try ignore (I.Interp.wrap_errors (fun () -> I.Interp.run_string_in_state st (source_of target)))
  with
  | I.Interp.Error msg ->
      (* attribute project errors to their source files; pass through for a file *)
      if I.Project.is_project target then report_project st target
      else Printf.eprintf "%s\n" msg;
      exit 1
  | I.Project.Build_error msg ->
      Printf.eprintf "mml run: %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "mml run: %s\n" msg;
      exit 1

let cmd_fmt args =
  let write, files =
    match args with "-w" :: rest -> (true, rest) | _ -> (false, args)
  in
  if files = [] then begin
    Printf.eprintf "mml fmt: expected one or more files\n";
    exit 1
  end;
  List.iter
    (fun f ->
      match I.Formatter.format_source (read_file f) with
      | out -> if write then write_file f out else print_string out
      | exception I.Formatter.Unsupported what ->
          Printf.eprintf "mml fmt: %s: unsupported construct (%s)\n" f what;
          exit 1
      | exception e ->
          Printf.eprintf "mml fmt: %s: %s\n" f (Printexc.to_string e);
          exit 1)
    files

let cmd_build args =
  let out = ref None and emit = ref "native" and file = ref None in
  let rec parse = function
    | "-o" :: o :: rest -> out := Some o; parse rest
    | "--emit" :: t :: rest -> emit := t; parse rest
    | f :: rest when !file = None -> file := Some f; parse rest
    | [] -> ()
    | x :: _ ->
        Printf.eprintf "mml build: unexpected argument %S\n" x;
        exit 1
  in
  parse args;
  match !file with
  | None ->
      Printf.eprintf "mml build: expected a source file or project directory\n";
      exit 1
  | Some f -> (
      (* For a project, the build runs on the combined source; native needs a
         file on disk, so the combined source is written to a temp file. *)
      let project = I.Project.is_project f in
      let default_out =
        if project then Filename.basename (I.Project.load f).I.Project.name
        else Filename.remove_extension f
      in
      let with_source_file g =
        if project then begin
          let tmp = Filename.temp_file "mml_build_" ".mml" in
          Fun.protect
            ~finally:(fun () -> try Sys.remove tmp with _ -> ())
            (fun () ->
              write_file tmp (I.Project.combined_source (I.Project.load f));
              g tmp)
        end
        else g f
      in
      try
        match !emit with
        | "native" ->
            let output = Option.value ~default:default_out !out in
            with_source_file (fun src_file -> N.Driver.compile_to_native ~source_file:src_file ~output)
        | "ir" ->
            let ir = N.Driver.compile_ir (source_of f) in
            (match !out with Some o -> write_file o ir | None -> print_string ir)
        | t ->
            Printf.eprintf "mml build: unknown --emit target %S (native|ir)\n" t;
            exit 1
      with
      | (N.Driver.Driver_error msg | I.Interp.Error msg) when project ->
          (* attribute the project's errors to their source files *)
          report_project (I.Analysis.make_state ()) f;
          ignore msg;
          exit 1
      | N.Driver.Driver_error msg | I.Interp.Error msg | I.Project.Build_error msg ->
          Printf.eprintf "mml build: %s\n" msg;
          exit 1
      | Sys_error msg ->
          Printf.eprintf "mml build: %s\n" msg;
          exit 1)

(* `mml get <url>[@version]` — fetch a dependency and add it to ./mml.mod. With
   no version, the latest released tag is used. mml.sum is updated on the next
   load (build/run/check). *)
let cmd_get arg =
  let url, ver =
    match String.rindex_opt arg '@' with
    | Some i -> (String.sub arg 0 i, Some (String.sub arg (i + 1) (String.length arg - i - 1)))
    | None -> (arg, None)
  in
  let version =
    match ver with
    | Some v -> (
        match I.Semver.parse v with
        | Some v -> v
        | None ->
            Printf.eprintf "mml get: %S is not a version (vMAJOR.MINOR.PATCH)\n" v;
            exit 1)
    | None -> (
        match I.Fetch.latest_version url with
        | Some v -> v
        | None ->
            Printf.eprintf "mml get: no released (semver-tagged) versions found for %s\n" url;
            exit 1)
  in
  (try ignore (I.Fetch.ensure url version)
   with I.Fetch.Fetch_error m ->
     Printf.eprintf "mml get: %s\n" m;
     exit 1);
  if not (Sys.file_exists "mml.mod") then begin
    Printf.eprintf "mml get: no mml.mod in the current directory\n";
    exit 1
  end;
  (* rewrite mml.mod: drop any existing require for this module, append the new *)
  let lines = String.split_on_char '\n' (read_file "mml.mod") in
  let keep line =
    match String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "") with
    | "require" :: m :: _ when m = url -> false
    | _ -> true
  in
  let kept = List.filter keep lines in
  let kept = List.filter (fun l -> String.trim l <> "") kept in
  let text =
    String.concat "\n" kept ^ Printf.sprintf "\nrequire %s %s\n" url (I.Semver.to_string version)
  in
  write_file "mml.mod" text;
  Printf.printf "added require %s %s\n" url (I.Semver.to_string version)

(* `mml test [dir]` — run each `let test_* = <bool>` in the project's
   `*_test.mml` files (with the project's libraries available) and report
   pass/fail. A test passes if it evaluates to `true`; `false`, the wrong type,
   or a runtime error fail it. Each test runs in a fresh state, so one failure
   doesn't affect another. *)
let cmd_test target =
  let proj =
    try I.Project.load target
    with I.Project.Build_error m ->
      Printf.eprintf "mml test: %s\n" m;
      exit 1
  in
  let total = ref 0 and failed = ref 0 in
  List.iter
    (fun (tf : I.Project.unit_) ->
      List.iter
        (fun name ->
          incr total;
          (* tests are unit-functions, so each runs only when applied — a crash
             in one doesn't affect the others *)
          let prog = I.Project.test_program proj tf (name ^ " ()") in
          let label = Printf.sprintf "%s.%s" tf.I.Project.name name in
          match
            try Ok (I.Interp.wrap_errors (fun () ->
                        I.Interp.run_string_in_state (I.Std.register_all (I.Interp.repl_state_init ())) prog))
            with I.Interp.Error m -> Error m
          with
          | Ok (I.Bytecode.VBool true) -> Printf.printf "ok   %s\n" label
          | Ok (I.Bytecode.VBool false) ->
              incr failed;
              Printf.printf "FAIL %s\n" label
          | Ok _ ->
              incr failed;
              Printf.printf "FAIL %s (test did not evaluate to a bool)\n" label
          | Error m ->
              incr failed;
              Printf.printf "FAIL %s: %s\n" label m)
        (I.Project.test_decl_names tf))
    proj.I.Project.tests;
  Printf.printf "\n%d passed, %d failed (of %d)\n" (!total - !failed) !failed !total;
  if !failed > 0 then exit 1

let cmd_check targets =
  let st = I.Analysis.make_state () in
  let had_error = ref false in
  List.iter
    (fun t ->
      let ds =
        try target_diagnostics st t
        with I.Project.Build_error m ->
          Printf.eprintf "mml check: %s\n" m;
          exit 1
      in
      if has_errors ds then had_error := true;
      List.iter (fun d -> print_endline (diag_line d)) ds)
    targets;
  if !had_error then exit 1

let () =
  match Array.to_list Sys.argv with
  | _ :: "run" :: file :: rest -> cmd_run file rest
  | _ :: "build" :: (_ :: _ as rest) -> cmd_build rest
  | _ :: "fmt" :: rest -> cmd_fmt rest
  | _ :: "check" :: (_ :: _ as files) -> cmd_check files
  | [ _; "test" ] -> cmd_test "."
  | [ _; "test"; dir ] -> cmd_test dir
  | [ _; "get"; arg ] -> cmd_get arg
  | _ :: "lsp" :: _ -> I.Lsp.serve stdin stdout
  | _ :: ("version" | "--version" | "-v") :: _ -> print_endline version
  | (_ :: ("help" | "--help" | "-h") :: _) | [ _ ] -> print_string usage
  | _ :: cmd :: _ ->
      Printf.eprintf "mml: unknown command %S\n\n%s" cmd usage;
      exit 1
  | [] -> print_string usage
