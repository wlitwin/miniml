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

(* Typecheck [src] against [state]'s stdlib context and return its diagnostics.
   Each phase's first error (lex / parse / type) is reported as one structured
   diagnostic spanning the offending token; a clean source yields []. The
   typechecker's accumulated warnings are drained so repeated calls on the same
   [state] stay independent.

   Single-error-per-phase for now: multi-error reporting needs parser/typechecker
   error RECOVERY ([#18]), which builds on this. *)
let diagnostics (state : state) (src : string) : Diagnostic.t list =
  let drain () = ignore (Typechecker.take_warnings ()) in
  match
    let tokens = Lexer.tokenize src in
    let program = Parser.parse_program tokens in
    let ctx', typed = Typechecker.check_program_in_ctx state.Interp.ctx program in
    ignore (Typechecker.transform_constraints ctx' typed)
  with
  | () ->
      drain ();
      []
  | exception Lexer.Lex_error (msg, loc) ->
      drain ();
      [ Diagnostic.make ~code:"lex" ~span:(Diagnostic.span_at src loc) msg ]
  | exception Parser.Parse_error (msg, loc) ->
      drain ();
      [ Diagnostic.make ~code:"parse" ~span:(Diagnostic.span_at src loc) msg ]
  | exception Typechecker.Type_error (msg, loc) ->
      drain ();
      [ Diagnostic.make ~code:"type" ~span:(Diagnostic.span_at src loc) msg ]
