(* Filesystem access — the OCaml-reference twin of the MiniML [Fs] builtins
   (stdlib/fs.mml signatures; VM impls in std.ml register_fs; native in
   native_rt/runtime.c as the mml_fs_ functions). Semantics MUST match those so
   the in-MiniML tooling translates faithfully. *)

exception Fs_error of string

(* Directory entries (names only, not full paths), as a list — matching the
   MiniML [Fs.read_dir : string -> string list]. *)
let read_dir (path : string) : string list =
  try Array.to_list (Sys.readdir path)
  with Sys_error msg -> raise (Fs_error ("Fs.read_dir: " ^ msg))

let is_directory (path : string) : bool =
  try Sys.is_directory path with Sys_error _ -> false

(* Create a single directory; an existing directory is not an error (matching
   the VM/native EEXIST handling). *)
let make_dir (path : string) : unit =
  try Unix.mkdir path 0o755
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (e, _, _) ->
      raise (Fs_error ("Fs.make_dir: " ^ Unix.error_message e))

let remove (path : string) : unit =
  try if is_directory path then Unix.rmdir path else Sys.remove path
  with
  | Sys_error msg -> raise (Fs_error ("Fs.remove: " ^ msg))
  | Unix.Unix_error (e, _, _) ->
      raise (Fs_error ("Fs.remove: " ^ Unix.error_message e))

let rename (src : string) (dst : string) : unit =
  try Sys.rename src dst
  with Sys_error msg -> raise (Fs_error ("Fs.rename: " ^ msg))
