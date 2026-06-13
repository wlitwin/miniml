(* Subprocess execution — the OCaml-reference twin of the MiniML [Process.run]
   builtin (stdlib/process.mml; VM impl in std.ml register_process; native in
   native_rt/runtime.c mml_process_run). Runs a command with an explicit argv
   (no shell, so no quoting/injection), captures stdout and stderr, and returns
   (exit_code, stdout, stderr). A command that cannot be executed yields code 127
   (matching the native exec-failure path). *)

let run (cmd : string) (args : string list) : int * string * string =
  let argv = Array.of_list (cmd :: args) in
  try
    let ic, oc, ec = Unix.open_process_args_full cmd argv (Unix.environment ()) in
    let out = In_channel.input_all ic in
    let err = In_channel.input_all ec in
    let status = Unix.close_process_full (ic, oc, ec) in
    let code = match status with Unix.WEXITED c -> c | _ -> -1 in
    (code, out, err)
  with Unix.Unix_error _ -> (127, "", "")
