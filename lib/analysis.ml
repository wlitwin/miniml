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

(* The typechecker's accumulated warnings ("Warning at line N: msg") as Warning
   diagnostics spanning that line. Anything that doesn't match the format is
   reported whole at the start of the file rather than dropped. *)
let warning_diagnostics (src : string) (ws : string list) : Diagnostic.t list =
  let prefix = "Warning at line " in
  let plen = String.length prefix in
  List.map
    (fun w ->
      match
        if String.length w > plen && String.sub w 0 plen = prefix then
          let rest = String.sub w plen (String.length w - plen) in
          match String.index_opt rest ':' with
          | Some i -> (
              match int_of_string_opt (String.trim (String.sub rest 0 i)) with
              | Some line ->
                  Some (line, String.trim (String.sub rest (i + 1) (String.length rest - i - 1)))
              | None -> None)
          | None -> None
        else None
      with
      | Some (line, msg) ->
          Diagnostic.make ~severity:Diagnostic.Warning ~code:"warning"
            ~span:(Diagnostic.line_span src line) msg
      | None ->
          Diagnostic.make ~severity:Diagnostic.Warning ~code:"warning"
            ~span:(Diagnostic.span_at src { line = 1; col = 1; offset = 0 }) w)
    ws

(* Typecheck [src] against [state]'s stdlib context and return its diagnostics.
   Lex errors stop early (one diagnostic). Otherwise parsing recovers at
   declaration boundaries and reports EVERY syntax error, then each declaration
   is typechecked INDEPENDENTLY (threading the context on success) so EVERY type
   error is reported, not just the first — editing one broken declaration still
   surfaces problems in the rest. Typechecker warnings are surfaced too. A clean
   source yields []. *)
let diagnostics (state : state) (src : string) : Diagnostic.t list =
  match Lexer.tokenize src with
  | exception Lexer.Lex_error (msg, loc) ->
      ignore (Typechecker.take_warnings ());
      [ Diagnostic.make ~code:"lex" ~span:(Diagnostic.span_at src loc) msg ]
  | tokens ->
      let program, parse_diags, _ = parse_recover src tokens in
      let ctx = ref state.Interp.ctx and type_diags = ref [] in
      let type_diag loc msg =
        type_diags := Diagnostic.make ~code:"type" ~span:(Diagnostic.span_at src loc) msg :: !type_diags
      in
      List.iter
        (fun decl ->
          match Typechecker.check_program_in_ctx !ctx [ decl ] with
          | ctx', tds -> (
              (* binding succeeded: thread the context so later declarations see
                 it, then resolve this declaration's constraints *)
              ctx := ctx';
              match Typechecker.transform_constraints ctx' tds with
              | _ -> ()
              | exception Typechecker.Type_error (msg, loc) -> type_diag loc msg
              | exception _ -> ())
          | exception Typechecker.Type_error (msg, loc) -> type_diag loc msg
          | exception _ -> ())
        program;
      let warns = warning_diagnostics src (Typechecker.take_warnings ()) in
      parse_diags @ List.rev !type_diags @ warns

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

(* --- Completion --------------------------------------------------------- *)

(* LSP CompletionItemKind values used here. *)
let kind_function = 3
let kind_variable = 6
let kind_keyword = 14

let keywords =
  [
    "let"; "rec"; "mut"; "in"; "if"; "do"; "else"; "end"; "fn"; "match"; "with";
    "type"; "newtype"; "module"; "effect"; "class"; "instance"; "extern"; "open";
    "handle"; "perform"; "resume"; "for"; "return"; "break"; "continue"; "true";
    "false"; "and"; "of"; "where"; "pub"; "opaque"; "when";
  ]

(* A plain (unqualified, non-operator) identifier we'd offer as a completion. *)
let is_ident_like (s : string) : bool =
  String.length s > 0
  && (match s.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false)
  && (not (String.contains s '.'))
  && String.for_all
       (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true | _ -> false)
       s

(* The variables a pattern binds (PatVar / PatAs and their sub-patterns). *)
let rec pat_binders (acc : string list ref) (p : Ast.pattern) : unit =
  match p with
  | Ast.PatVar n -> acc := n :: !acc
  | Ast.PatAs (p, n) -> acc := n :: !acc; pat_binders acc p
  | Ast.PatTuple ps | Ast.PatArray ps | Ast.PatSet ps -> List.iter (pat_binders acc) ps
  | Ast.PatCons (a, b) | Ast.PatOr (a, b) -> pat_binders acc a; pat_binders acc b
  | Ast.PatConstruct (_, Some p) | Ast.PatPolyVariant (_, Some p) | Ast.PatAnnot (p, _) ->
      pat_binders acc p
  | Ast.PatRecord fs -> List.iter (fun (_, p) -> pat_binders acc p) fs
  | Ast.PatMap pairs -> List.iter (fun (k, v) -> pat_binders acc k; pat_binders acc v) pairs
  | _ -> ()

(* Every name bound anywhere in an expression — let/fn-param/match-pattern/loop/
   handler binders, recursively. Over-inclusive for completion (not scope-
   filtered), which is fine: the editor filters by the typed prefix. *)
let rec expr_binders (acc : string list ref) (e : Ast.expr) : unit =
  let r = expr_binders acc in
  let push n = acc := n :: !acc in
  match e with
  | Ast.ELoc (_, e) -> r e
  | Ast.ELet (n, a, b) | Ast.ELetMut (n, a, b) -> push n; r a; r b
  | Ast.ELetRec (n, _, a, b) -> push n; r a; r b
  | Ast.ELetRecAnd (bs, body) ->
      List.iter (fun (n, _, e) -> push n; r e) bs;
      r body
  | Ast.EFun (p, b) -> push p.Ast.name; r b
  | Ast.EMatch (s, arms, _) ->
      r s;
      List.iter
        (fun (pat, g, body) ->
          pat_binders acc pat;
          (match g with Some e -> r e | None -> ());
          r body)
        arms
  | Ast.EWhileLet (pat, e, body) -> pat_binders acc pat; r e; r body
  | Ast.EFor { for_var; for_iter; for_body; for_index } ->
      push for_var;
      Option.iter push for_index;
      r for_iter;
      r for_body
  | Ast.EForFold { loop_var; loop_iter; accum_var; accum_init; fold_body; fold_index } ->
      push loop_var; push accum_var; Option.iter push fold_index;
      r loop_iter; r accum_init; r fold_body
  | Ast.EForNumeric { fn_var; fn_init; fn_cond; fn_step; fn_body } ->
      push fn_var; r fn_init; r fn_cond; r fn_step; r fn_body
  | Ast.EWhile { while_cond; while_body } -> r while_cond; r while_body
  | Ast.EHandle (body, arms) ->
      r body;
      List.iter
        (function
          | Ast.HReturn (n, e) -> push n; r e
          | Ast.HOp { arg; k; body; _ } -> push arg; push k; r body)
        arms
  | Ast.EApp (a, b) | Ast.EBinop (_, a, b) | Ast.ECons (a, b) | Ast.ESeq (a, b)
  | Ast.EIndex (a, b) | Ast.EResume (a, b) ->
      r a; r b
  | Ast.EIf (a, b, c) -> r a; r b; r c
  | Ast.EUnop (_, e) | Ast.EField (e, _) | Ast.EAnnot (e, _) | Ast.ECoerce (e, _)
  | Ast.EReturn e | Ast.EPerform (_, e) | Ast.ELocalOpen (_, e) | Ast.EAssign (_, e)
  | Ast.EConstruct (_, Some e) | Ast.EPolyVariant (_, Some e) | Ast.EBreak (Some e)
  | Ast.EContinueLoop (Some e) ->
      r e
  | Ast.EFieldAssign (o, _, e) -> r o; r e
  | Ast.ETuple es | Ast.EList es | Ast.EArray es | Ast.ESet es | Ast.ECollTyped (_, es) ->
      List.iter r es
  | Ast.ERecord fs -> List.iter (fun (_, e) -> r e) fs
  | Ast.ERecordUpdate (base, fs) -> r base; List.iter (fun (_, e) -> r e) fs
  | Ast.EMap pairs | Ast.EMapTyped (_, pairs) -> List.iter (fun (k, v) -> r k; r v) pairs
  | _ -> ()

let decl_binders (acc : string list ref) (d : Ast.decl) : unit =
  match d with
  | Ast.DLet { params; body; _ } -> List.iter (fun (p : Ast.param) -> acc := p.name :: !acc) params; expr_binders acc body
  | Ast.DLetMut (_, e) | Ast.DExpr e -> expr_binders acc e
  | Ast.DLetRec b | Ast.DLetRecAnd [ b ] ->
      List.iter (fun (p : Ast.param) -> acc := p.name :: !acc) b.Ast.params;
      expr_binders acc b.Ast.body
  | Ast.DLetRecAnd bs ->
      List.iter
        (fun (b : Ast.letrec_binding) ->
          List.iter (fun (p : Ast.param) -> acc := p.name :: !acc) b.params;
          expr_binders acc b.body)
        bs
  | _ -> ()

(* The completion candidates visible in [src]: every name in scope (the standard
   library and builtins, from the typecheck context) plus the file's own
   top-level declarations and local bindings (let / parameters / patterns /
   loops / handlers), plus the language keywords. Returned as (label,
   CompletionItemKind) pairs, de-duplicated; the editor filters by the typed
   prefix. Local names are over-inclusive (whole file, not scope-filtered). *)
let completions (state : state) (src : string) : (string * int) list =
  let seen = Hashtbl.create 512 in
  let out = ref [] in
  let add kind name =
    if is_ident_like name && not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      out := (name, kind) :: !out
    end
  in
  (* the file's own names first (most relevant): top-level declarations, then
     every local binding (parameters, let-ins, pattern/loop/handler vars) *)
  (match Lexer.tokenize src with
  | exception _ -> ()
  | tokens ->
      let program, _, defs = parse_recover src tokens in
      List.iter (fun (name, _) -> add kind_variable name) defs;
      let locals = ref [] in
      List.iter (decl_binders locals) program;
      List.iter (add kind_variable) !locals);
  (* everything in scope: stdlib + builtins *)
  List.iter
    (fun (name, (sch : Types.scheme)) ->
      let kind = match sch.Types.body with Types.TArrow _ -> kind_function | _ -> kind_variable in
      add kind name)
    state.Interp.ctx.Typechecker.vars;
  List.iter (add kind_keyword) keywords;
  List.rev !out

