(* `mml` — the MiniML all-in-one toolchain binary (roadmap #24). Go-style
   subcommands over the reference compiler library: run / fmt / check / lsp.
   Each command is a thin shell over Interpreter.{Interp,Formatter,Analysis,Lsp}
   — the build-internal multi-flag entry point (bin/main.ml) is left untouched. *)

module I = Interpreter

let version = "0.1.0-dev"

let usage =
  "mml — the MiniML toolchain\n\n\
   Usage: mml <command> [args]\n\n\
   Commands:\n\
  \  run <file> [args...]   typecheck, compile and run a program\n\
  \  fmt [-w] <files...>    format to stdout, or rewrite in place with -w\n\
  \  check <files...>       print diagnostics (exit 1 if any error)\n\
  \  lsp                    start the language server (stdio JSON-RPC)\n\
  \  version                print the version\n\
  \  help                   show this help\n"

let read_file path =
  let ic = open_in_bin path in
  let s = In_channel.input_all ic in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let cmd_run file args =
  let st = I.Std.register_all (I.Interp.repl_state_init ()) in
  st.I.Interp.argv := Array.of_list (file :: args);
  try ignore (I.Interp.wrap_errors (fun () -> I.Interp.run_string_in_state st (read_file file)))
  with
  | I.Interp.Error msg ->
      Printf.eprintf "%s\n" msg;
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

let cmd_check files =
  let st = I.Analysis.make_state () in
  let had_error = ref false in
  List.iter
    (fun f ->
      List.iter
        (fun (d : I.Diagnostic.t) ->
          if d.I.Diagnostic.severity = I.Diagnostic.Error then had_error := true;
          Printf.printf "%s:%s\n" f (I.Diagnostic.to_string d))
        (I.Analysis.diagnostics st (read_file f)))
    files;
  if !had_error then exit 1

let () =
  match Array.to_list Sys.argv with
  | _ :: "run" :: file :: rest -> cmd_run file rest
  | _ :: "fmt" :: rest -> cmd_fmt rest
  | _ :: "check" :: (_ :: _ as files) -> cmd_check files
  | _ :: "lsp" :: _ -> I.Lsp.serve stdin stdout
  | _ :: ("version" | "--version" | "-v") :: _ -> print_endline version
  | (_ :: ("help" | "--help" | "-h") :: _) | [ _ ] -> print_string usage
  | _ :: cmd :: _ ->
      Printf.eprintf "mml: unknown command %S\n\n%s" cmd usage;
      exit 1
  | [] -> print_string usage
