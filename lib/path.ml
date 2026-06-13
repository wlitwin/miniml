(* POSIX path manipulation — the OCaml-reference twin of stdlib/path.mml, so the
   in-MiniML tooling (fetch/project) can be written compiler-agnostically against
   one [Path] surface and mechanically translated. Pure string ops; forward-slash
   separators. Semantics MUST match stdlib/path.mml byte-for-byte. *)

let sep : string = "/"

let is_absolute (p : string) : bool =
  String.length p > 0 && p.[0] = '/'

(* The final path component, e.g. dir/sub/x.txt -> x.txt. *)
let basename (p : string) : string =
  match String.rindex_opt p '/' with
  | None -> p
  | Some i -> String.sub p (i + 1) (String.length p - i - 1)

(* Everything up to the final component; no slash -> ".", a leading-slash-only
   match -> "/". *)
let dirname (p : string) : string =
  match String.rindex_opt p '/' with
  | None -> "."
  | Some 0 -> "/"
  | Some i -> String.sub p 0 i

(* The extension of the final component, including the dot (".txt"), or "" when
   there is none (a leading-dot file like .bashrc has no extension). *)
let extension (p : string) : string =
  let base = basename p in
  match String.rindex_opt base '.' with
  | None -> ""
  | Some 0 -> ""
  | Some i -> String.sub base i (String.length base - i)

(* The path without its final extension, e.g. dir/x.txt -> dir/x (a leading-dot
   file like .bashrc is unchanged — it has no extension). *)
let remove_extension (p : string) : string =
  let e = extension p in
  String.sub p 0 (String.length p - String.length e)

(* Join two path segments with a single separator; an absolute b wins. *)
let join (a : string) (b : string) : string =
  if a = "" then b
  else if is_absolute b then b
  else
    let n = String.length a in
    if String.sub a (n - 1) 1 = "/" then a ^ b else a ^ "/" ^ b
