(* Lossless concrete-syntax foundation (roadmap #17), increment 1.

   The lexer already records an exact half-open source span for every token
   (token.ml: [loc.offset, end_offset)), and the ONLY bytes it discards are
   whitespace and comments — which always sit in the gap between one token's
   [end_offset] and the next token's [loc.offset]. So the full original source
   is recoverable from the token stream alone: nothing about the lexer needs to
   change. This module exposes that recoverable "trivia" and proves, via
   [reconstruct] plus a corpus round-trip test, that token spans tile the source
   with zero loss — the invariant a formatter ([#21]) and LSP ([#22]) depend on.

   This layer is deliberately separate from the AST: it touches no AST node, no
   pipeline pass, and no backend. Later increments build a green-tree CST on top
   of these pieces; the AST stays the semantic tree that the backends consume. *)

(* Recovered inter-token text: whitespace and/or comments. *)
type trivia = string

(* A significant token together with the exact source bytes around it. Pieces
   in order concatenate (leading ^ text, repeatedly) back to the input verbatim;
   that is the lossless contract [reconstruct] checks. *)
type piece = {
  leading : trivia; (* whitespace/comments immediately before [token] *)
  token : Token.token; (* the significant token *)
  text : string; (* exact source spelling of [token] (quotes/escapes kept) *)
}

(* The trivia immediately preceding [tok], given the offset at which the
   previous token ended ([0] for the first token). *)
let trivia_before source ~prev_end (tok : Token.token) : trivia =
  String.sub source prev_end (tok.loc.offset - prev_end)

(* The exact source spelling of [tok] — sliced from the source, NOT re-rendered
   from the kind, so the original quoting, escaping, and number/identifier
   spelling are preserved byte-for-byte. *)
let token_text source (tok : Token.token) : string =
  String.sub source tok.loc.offset (tok.end_offset - tok.loc.offset)

(* Pair every token (including the terminal [EOF], whose leading trivia captures
   any trailing whitespace/comments and whose text is empty) with its
   surrounding source bytes. *)
let pieces source (tokens : Token.token list) : piece list =
  let prev_end = ref 0 in
  List.map
    (fun (tok : Token.token) ->
      let leading = trivia_before source ~prev_end:!prev_end tok in
      let text = token_text source tok in
      prev_end := tok.end_offset;
      { leading; token = tok; text })
    tokens

(* Lex [source] into its lossless piece stream. *)
let of_source source : piece list = pieces source (Lexer.tokenize source)

(* Reassemble the original source text from a piece stream. *)
let reconstruct (ps : piece list) : string =
  let buf = Buffer.create 1024 in
  List.iter
    (fun p ->
      Buffer.add_string buf p.leading;
      Buffer.add_string buf p.text)
    ps;
  Buffer.contents buf

(* The round-trip invariant in one call: lex [source] and rebuild it. Equals
   [source] exactly iff the token spans tile it losslessly. *)
let roundtrip source : string = reconstruct (of_source source)
