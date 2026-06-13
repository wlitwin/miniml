(* MD5 digest — a superset of OCaml's [Stdlib.Digest] (so existing in-compiler
   users of Digest.string/to_hex keep working) that adds [md5], the OCaml-
   reference twin of the MiniML [Digest.md5] builtin (stdlib/digest.mml; VM impl
   in std.ml register_digest; native mml_digest_md5). Lets the in-MiniML tooling
   call one [Digest.md5] surface and translate faithfully. *)

include Stdlib.Digest

(* Hex MD5 of a string — exactly [to_hex (string s)], matching Digest.md5 on
   every backend. *)
let md5 (s : string) : string = to_hex (string s)
