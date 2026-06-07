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

(* The name a top-level declaration binds, and the token that names it, read off
   the declaration's leading tokens — so a nested let-binding in the body is not
   mistaken for the declaration's own name. *)
let decl_name (toks : Token.token list) : (string * Token.loc) option =
  let rec drop_vis = function
    | (t : Token.token) :: r when t.kind = Token.PUB || t.kind = Token.OPAQUE ->
        drop_vis r
    | ts -> ts
  in
  let ident_tok = function
    | (t : Token.token) :: _ -> (
        match t.kind with
        | Token.IDENT n | Token.UIDENT n -> Some (n, t.loc)
        | _ -> None)
    | [] -> None
  in
  match drop_vis toks with
  | { kind = Token.LET; _ } :: rest ->
      let rec drop_rm = function
        | (t : Token.token) :: r when t.kind = Token.REC || t.kind = Token.MUT ->
            drop_rm r
        | ts -> ts
      in
      ident_tok (drop_rm rest)
  | { kind = Token.TYPE | Token.NEWTYPE; _ } :: rest ->
      (* the name is the last identifier before `=` (after any type params) *)
      let rec last best = function
        | ({ kind = Token.EQ; _ } : Token.token) :: _ -> best
        | { kind = Token.IDENT n | Token.UIDENT n; loc; _ } :: r ->
            last (Some (n, loc)) r
        | _ :: r -> last best r
        | [] -> best
      in
      last None rest
  | { kind = Token.MODULE | Token.EFFECT | Token.CLASS | Token.EXTERN; _ } :: rest
    ->
      ident_tok rest
  | _ -> None

(* Parse [tokens] with panic-mode recovery at declaration boundaries (roadmap
   #18): a failed declaration becomes a diagnostic and the parser resyncs to the
   next `;;` / declaration keyword / EOF, so a single syntax error doesn't blank
   the rest of the file. Returns the partial program (the declarations that DID
   parse — what hover/go-to-def will run on), every parse diagnostic, and the
   name token of each top-level declaration (for go-to-def — collected here so it
   tolerates syntax errors, which the CST parser does not).

   Resync always makes progress (it advances at least one token), so it cannot
   loop. Reuses the real [parse_decl]; the recovery is OCaml-only, so the
   self-hosted parser is untouched. *)
let parse_recover (src : string) (tokens : Token.token list) :
    Ast.program * Diagnostic.t list * (string * Token.loc) list =
  Parser.fresh_param_counter := 0;
  let p = Parser.create tokens in
  let decls = ref [] and diags = ref [] and defs = ref [] in
  while Parser.peek_kind p <> Token.EOF do
    p.Parser.suppress_do <- false;
    let before = p.Parser.pos in
    (match Parser.parse_decl p with
    | ds ->
        (match decl_name (Array.to_list (Array.sub p.Parser.tokens before (p.Parser.pos - before))) with
        | Some nl -> defs := nl :: !defs
        | None -> ());
        decls := List.rev_append ds !decls
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
  (List.rev !decls, List.rev !diags, List.rev !defs)

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
      let program, parse_diags, _ = parse_recover src tokens in
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

(* --- Positions ---------------------------------------------------------- *)

(* The byte offset of 1-based [line]/[col] in [src] (the compiler's convention;
   the LSP server converts from its 0-based line/character at the boundary). *)
let offset_of_line_col (src : string) ~line ~col : int =
  let n = String.length src in
  let off = ref 0 and ln = ref 1 in
  while !ln < line && !off < n do
    (if src.[!off] = '\n' then incr ln);
    incr off
  done;
  min n (!off + (col - 1))

(* The significant token whose span covers byte [cursor], if any. *)
let token_at (src : string) (cursor : int) : Token.token option =
  match Lexer.tokenize src with
  | exception _ -> None
  | toks ->
      List.find_opt
        (fun (t : Token.token) -> t.loc.offset <= cursor && cursor < t.end_offset)
        toks

(* --- Typed tree (with per-declaration error isolation) ------------------ *)

(* Apply [f] to [te] and every typed sub-expression, parent before child. *)
let rec visit_texpr f (te : Typechecker.texpr) : unit =
  f te;
  Typechecker.iter_texpr_children (visit_texpr f) te

(* Apply [f] to every typed sub-expression of a declaration, recursing into
   module bodies. *)
let rec visit_tdecl f (td : Typechecker.tdecl) : unit =
  match td with
  | Typechecker.TDLet (_, te)
  | Typechecker.TDLetMut (_, te)
  | Typechecker.TDLetRec (_, te)
  | Typechecker.TDExpr te ->
      visit_texpr f te
  | Typechecker.TDLetRecAnd binds -> List.iter (fun (_, te) -> visit_texpr f te) binds
  | Typechecker.TDModule (_, decls, _) -> List.iter (visit_tdecl f) decls
  | Typechecker.TDType _ | Typechecker.TDClass _ | Typechecker.TDEffect _
  | Typechecker.TDExtern _ | Typechecker.TDOpen _ ->
      ()

(* Typecheck [program] one declaration at a time against [state]'s context,
   threading the context but ISOLATING failures: a declaration that doesn't
   typecheck is skipped (it contributes no bindings), so a type error in one
   place doesn't blank the typed tree the rest of the file produces. This is the
   typed-side analogue of [parse_recover] and what hover / go-to-def run on. *)
let typed_recover (state : state) (program : Ast.program) :
    Typechecker.tdecl list =
  let ctx = ref state.Interp.ctx and acc = ref [] in
  List.iter
    (fun decl ->
      match Typechecker.check_program_in_ctx !ctx [ decl ] with
      | ctx', tds ->
          ctx := ctx';
          acc := List.rev_append tds !acc
      | exception _ -> ())
    program;
  ignore (Typechecker.take_warnings ());
  List.rev !acc

(* --- Hover -------------------------------------------------------------- *)

(* The inferred type at 1-based [line]/[col], rendered for display, or [None] if
   the position isn't on a typed sub-expression. Finds the token under the
   cursor, then the DEEPEST typed node starting at that token (pre-order visits
   parent before child, so the last match at the token's offset is the
   innermost). Works declaration-by-declaration, so a type error elsewhere in
   the file doesn't suppress hover here. *)
let hover (state : state) (src : string) ~line ~col : string option =
  let cursor = offset_of_line_col src ~line ~col in
  match token_at src cursor with
  | None -> None
  | Some tok ->
      let target = tok.loc.offset in
      let program, _, _ = parse_recover src (Lexer.tokenize src) in
      let best = ref None in
      List.iter
        (visit_tdecl (fun te ->
             if te.Typechecker.loc.offset = target then best := Some te.Typechecker.ty))
        (typed_recover state program);
      Option.map Types.pp_ty !best

(* --- Go to definition --------------------------------------------------- *)

(* Walk a typed declaration tracking the local bindings in scope; when the typed
   node at [target] is a variable, record the innermost binder of its name (its
   defining construct's location) into [found]. Patterns (match/fun multi-arg)
   bind via the desugared TEFun/TELet chain, so the common cases are covered. *)
let rec resolve_local (target : int) (found : Token.loc option ref)
    (td : Typechecker.tdecl) : unit =
  let open Typechecker in
  let rec walk scope (te : texpr) : unit =
    (match te.expr with
    | TEVar name when te.loc.offset = target && !found = None ->
        (match List.assoc_opt name scope with Some loc -> found := Some loc | None -> ())
    | _ -> ());
    match te.expr with
    | TELet (name, _, v, body) | TELetMut (name, v, body) ->
        walk scope v;
        walk ((name, te.loc) :: scope) body
    | TELetRec (name, _, v, body) ->
        let scope = (name, te.loc) :: scope in
        walk scope v;
        walk scope body
    | TELetRecAnd (binds, body) ->
        let scope = List.map (fun (n, _) -> (n, te.loc)) binds @ scope in
        List.iter (fun (_, e) -> walk scope e) binds;
        walk scope body
    | TEFun (param, body, _) -> walk ((param, te.loc) :: scope) body
    | _ -> iter_texpr_children (walk scope) te
  in
  match td with
  | TDLet (_, te) | TDLetMut (_, te) | TDLetRec (_, te) | TDExpr te -> walk [] te
  | TDLetRecAnd binds -> List.iter (fun (_, te) -> walk [] te) binds
  | TDModule (_, decls, _) -> List.iter (fun d -> resolve_local target found d) decls
  | _ -> ()

(* The definition location of the identifier at 1-based [line]/[col], as a
   1-based (line, col), or [None]. A locally-bound name resolves to its binder;
   otherwise a top-level declaration of that name. *)
let definition (state : state) (src : string) ~line ~col : (int * int) option =
  let cursor = offset_of_line_col src ~line ~col in
  match token_at src cursor with
  | None -> None
  | Some tok -> (
      let name =
        match tok.kind with Token.IDENT n | Token.UIDENT n -> Some n | _ -> None
      in
      match name with
      | None -> None
      | Some name ->
          let program, _, defs = parse_recover src (Lexer.tokenize src) in
          let found = ref None in
          List.iter (resolve_local tok.loc.offset found) (typed_recover state program);
          let loc =
            match !found with
            | Some loc -> Some loc
            | None -> List.assoc_opt name defs
          in
          (* A binder with no real location (e.g. a desugared function parameter,
             whose position the typed tree doesn't carry) yields line 0 — don't
             jump to the start of the file; report no definition instead. *)
          (match loc with
          | Some (l : Token.loc) when l.line > 0 -> Some (l.line, l.col)
          | _ -> None))

