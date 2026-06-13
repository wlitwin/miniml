(* Byte-level file IO — the OCaml-reference twin of the MiniML [IO] builtins
   (registered in self_host/main.mml; VM impls in std.ml). Only the subset the
   in-MiniML tooling needs is mirrored here; semantics MUST match the MiniML
   builtins so translated tooling behaves identically. Strings are bytes on every
   backend, so reads/writes are binary (no text decoding). *)

let read_file (path : string) : string =
  In_channel.with_open_bin path In_channel.input_all

let write_file (path : string) (contents : string) : unit =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)

let file_exists (path : string) : bool = Sys.file_exists path

let write (s : string) : unit =
  print_string s;
  flush stdout

let write_err (s : string) : unit =
  prerr_string s;
  flush stderr
