(* Compiler-as-a-library surface for tooling (roadmap #20), and the analysis the
   LSP ([#22]) is built on. OCaml-only: a thin, side-effect-light API over the
   reference compiler that answers questions about a source string — starting
   with its diagnostics. The CLI and the LSP server are both meant to be shells
   over this module rather than re-implementing the pipeline.

   A [state] is a typechecking context with the standard library loaded; it is
   expensive to build (it typechecks all of stdlib), so a caller builds one with
   [make_state] and reuses it across edits. *)

type state = Interp.repl_state

(* A fresh analysis state with the full standard library loaded. *)
let make_state () : state = Std.register_all (Interp.repl_state_init ())

(* Token kinds that begin a top-level declaration — recovery resyncs to one of
   these (or `;;` / EOF) after a parse error. *)
let is_decl_start = function
  | Token.LET | Token.TYPE | Token.NEWTYPE | Token.MODULE | Token.EFFECT
  | Token.CLASS | Token.INSTANCE | Token.EXTERN | Token.OPEN ->
      true
  | _ -> false

(* Parse [tokens] with panic-mode recovery at declaration boundaries (roadmap
   #18): a failed declaration becomes a diagnostic and the parser resyncs to the
   next `;;` / declaration keyword / EOF, so a single syntax error doesn't blank
   the rest of the file. Returns the partial program (the declarations that DID
   parse — what hover/go-to-def will run on) and every parse diagnostic.

   Resync always makes progress (it advances at least one token), so it cannot
   loop. Reuses the real [parse_decl]; the recovery is OCaml-only, so the
   self-hosted parser is untouched. *)
let parse_recover (src : string) (tokens : Token.token list) :
    Ast.program * Diagnostic.t list =
  Parser.fresh_param_counter := 0;
  let p = Parser.create tokens in
  let decls = ref [] and diags = ref [] in
  while Parser.peek_kind p <> Token.EOF do
    p.Parser.suppress_do <- false;
    let before = p.Parser.pos in
    (match Parser.parse_decl p with
    | ds -> decls := List.rev_append ds !decls
    | exception Parser.Parse_error (msg, loc) ->
        diags :=
          Diagnostic.make ~code:"parse" ~span:(Diagnostic.span_at src loc) msg :: !diags;
        if p.Parser.pos = before then ignore (Parser.advance p);
        let scanning = ref true in
        while !scanning do
          match Parser.peek_kind p with
          | Token.EOF | Token.DOUBLE_SEMICOLON -> scanning := false
          | k when is_decl_start k -> scanning := false
          | _ -> ignore (Parser.advance p)
        done);
    if Parser.peek_kind p = Token.DOUBLE_SEMICOLON then ignore (Parser.advance p)
  done;
  (List.rev !decls, List.rev !diags)

(* Typecheck [src] against [state]'s stdlib context and return its diagnostics.
   Lex errors stop early (one diagnostic). Otherwise parsing recovers at
   declaration boundaries and reports EVERY syntax error, then the partial
   program is typechecked and its first type error (if any) is added — so editing
   in one broken declaration still surfaces problems in the rest. A clean source
   yields []. Warnings are drained so repeated calls on the same [state] stay
   independent.

   Type checking is still first-error-only — multi-type-error recovery is a
   later increment. *)
let diagnostics (state : state) (src : string) : Diagnostic.t list =
  let drain () = ignore (Typechecker.take_warnings ()) in
  match Lexer.tokenize src with
  | exception Lexer.Lex_error (msg, loc) ->
      [ Diagnostic.make ~code:"lex" ~span:(Diagnostic.span_at src loc) msg ]
  | tokens ->
      let program, parse_diags = parse_recover src tokens in
      let type_diags =
        match
          let ctx', typed =
            Typechecker.check_program_in_ctx state.Interp.ctx program
          in
          ignore (Typechecker.transform_constraints ctx' typed)
        with
        | () ->
            drain ();
            []
        | exception Typechecker.Type_error (msg, loc) ->
            drain ();
            [ Diagnostic.make ~code:"type" ~span:(Diagnostic.span_at src loc) msg ]
        | exception _ ->
            (* Never let a typechecker failure on a PARTIAL program crash the
               analysis — the parse diagnostics are still useful. *)
            drain ();
            []
      in
      parse_diags @ type_diags
