(* Build a lossless green tree from the parser's CST event stream (roadmap #17,
   increment 3). OCaml-only glue between the (translated) Parser event recorder
   and the (untranslated) Cst green-tree types — kept out of the translator set
   so the self-hosted compiler is unaffected.

   The parser emits a flat, balanced stream of CstStart/CstFinish/CstToken
   events in source order (see parser.ml). Folding it with the lossless token
   pieces (Cst.pieces) reconstructs the bracketed tree: Start opens a node,
   Token attaches the next piece as a leaf, Finish closes a node into its
   parent. The parser never advances over EOF, so the trailing piece(s) (EOF +
   any trailing trivia) carry no Token event — they are appended to the root so
   to_source reproduces the file tail. *)

let map_kind : Parser.cst_node_kind -> Cst.node_kind = function
  | Parser.CstSourceFile -> Cst.SourceFile
  | Parser.CstDecl -> Cst.Decl
  | Parser.CstExpr -> Cst.Expr
  | Parser.CstPattern -> Cst.Pattern
  | Parser.CstTypeExpr -> Cst.TypeExpr
  | Parser.CstMatchArm -> Cst.MatchArm
  | Parser.CstHandlerArm -> Cst.HandlerArm
  | Parser.CstRecordField -> Cst.RecordField
  | Parser.CstParam -> Cst.Param
  | Parser.CstError -> Cst.Error

let tree_of_events (events : Parser.cst_event list) (pieces : Cst.piece list) :
    Cst.tree =
  let pieces = Array.of_list pieces in
  let pi = ref 0 in
  (* Open nodes, innermost first; each carries its children in reverse order. *)
  let stack = ref [] in
  let root = ref None in
  let add_child c =
    match !stack with
    | (k, cs) :: rest -> stack := (k, c :: cs) :: rest
    | [] -> failwith "cst_build: leaf with no open node"
  in
  List.iter
    (fun ev ->
      match ev with
      | Parser.CstStart k -> stack := (map_kind k, []) :: !stack
      | Parser.CstToken ->
          if !pi >= Array.length pieces then
            failwith "cst_build: more token events than tokens";
          add_child (Cst.Leaf pieces.(!pi));
          incr pi
      | Parser.CstFinish -> (
          match !stack with
          | (k, cs) :: rest ->
              let node = Cst.Node (k, List.rev cs) in
              stack := rest;
              if rest = [] then root := Some node else add_child node
          | [] -> failwith "cst_build: finish with no open node"))
    events;
  if !stack <> [] then failwith "cst_build: unbalanced events (open node left)";
  let root =
    match !root with Some r -> r | None -> Cst.Node (Cst.SourceFile, [])
  in
  (* Trailing pieces the parser never consumed (EOF + trailing trivia). *)
  let trailing = ref [] in
  while !pi < Array.length pieces do
    trailing := Cst.Leaf pieces.(!pi) :: !trailing;
    incr pi
  done;
  match root with
  | Cst.Node (k, cs) -> Cst.Node (k, cs @ List.rev !trailing)
  | leaf -> Cst.Node (Cst.SourceFile, leaf :: List.rev !trailing)

(* Parse [source] and return its structured, lossless green tree. *)
let cst_of_source source : Cst.tree =
  let tokens = Lexer.tokenize source in
  let _prog, events = Parser.parse_program_with_events tokens in
  let pieces = Cst.pieces source tokens in
  tree_of_events events pieces
