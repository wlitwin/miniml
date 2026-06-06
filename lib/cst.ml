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

(* --- Green tree (increment 2) ------------------------------------------- *)

(* The lossless green tree: an immutable, position-independent syntax tree whose
   leaves are [piece]s (a token plus its leading trivia, the proven-lossless
   unit from increment 1) and whose interior [Node]s carry a syntactic [kind].
   "Green" in the Roslyn / rust-analyzer sense — it stores text and shape, not
   absolute offsets, so subtrees are shareable and a future red/typed layer can
   compute positions on demand.

   [to_source] is deliberately kind-AGNOSTIC: it only concatenates leaf text in
   order, so any tree built from a source's pieces round-trips regardless of how
   it is bracketed into nodes. Kinds exist for consumers (formatter [#21], LSP
   [#22]) and for the CST-emitting parser (increment 3); the parser decides
   which spans to wrap, never how the bytes are reproduced. *)

(* Interior node categories. Intentionally COARSE — it mirrors the AST's
   groupings (ast.ml) rather than its every constructor — and grows as
   increment 3 teaches the parser which spans to wrap. *)
type node_kind =
  | SourceFile (* the root: a sequence of top-level declarations *)
  | Decl (* a top-level declaration (let / type / module / ...) *)
  | Expr (* an expression *)
  | Pattern (* a pattern *)
  | TypeExpr (* a type expression / annotation *)
  | MatchArm (* one `| pat [when g] => body` arm *)
  | HandlerArm (* one handler arm *)
  | RecordField (* a `name = expr` or `name : type` field *)
  | Param (* a function parameter *)
  | Error (* a malformed span (error recovery, [#18]) *)

type tree = Node of node_kind * tree list | Leaf of piece

(* Reassemble source from a green tree: leaf text in left-to-right order, with
   each leaf's leading trivia. Round-trips for any tree built from real pieces. *)
let rec to_source : tree -> string = function
  | Leaf p -> p.leading ^ p.text
  | Node (_, children) ->
      let buf = Buffer.create 256 in
      List.iter (fun c -> Buffer.add_string buf (to_source c)) children;
      Buffer.contents buf

(* The trivially-correct CST for a source: one [SourceFile] node whose children
   are every token leaf, in order. Lossless by construction (to_source = source)
   and the baseline the parser's structured tree (increment 3) must match
   byte-for-byte. *)
let flat_of_source source : tree =
  Node (SourceFile, List.map (fun p -> Leaf p) (of_source source))

(* The leaf pieces of a tree, in source order. *)
let rec leaves : tree -> piece list = function
  | Leaf p -> [ p ]
  | Node (_, children) -> List.concat_map leaves children

(* The significant tokens of a tree, in source order (trivia is carried inside
   each piece's [leading], not as separate leaves). *)
let tokens (t : tree) : Token.token list =
  List.map (fun p -> p.token) (leaves t)
