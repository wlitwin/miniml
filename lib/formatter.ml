(* Opinionated / canonical formatter (roadmap #21).

   Pretty-prints the AST to a canonical surface syntax: the AST carries the full
   structure needed for layout and precedence, so formatting is a structural
   walk. Two invariants make this safe to apply to real code, both checked over
   the corpus by compiler_test/format_runner.ml:
     - SEMANTIC PRESERVATION: parse (format src) is structurally equal to
       parse src (modulo source locations, the is_generated hint, and
       sequence/let-scope associativity). Guaranteed largely by being
       PAREN-LIBERAL — wrapping any non-atomic sub-expression in parentheses,
       which never changes the parsed AST — plus context-specific delimiting
       where a `;` or `|` would otherwise be absorbed.
     - IDEMPOTENCE: format (format src) = format src.

   Increment 1: expression + core-declaration core. Increment 2: module / class /
   instance / effect declarations, faithful string/rune/float literals, and the
   sequence/arm/destructure delimiting that real (self-host) code exercises.
   Increment 3: GADT constructors, map/set patterns, typed-collection (#Name[..]
   / #Name{..}) and poly-variant-type literals, expression-level let-rec(-and) —
   the corpus is now ~fully covered (only empty typed-map literals are unhandled).
   NOT yet: COMMENTS — a later increment weaves them in from the CST trivia. *)

exception Unsupported of string

(* --- A minimal Wadler-style document combinator (no width-based wrapping yet;
   breaks exactly where [line] says). ------------------------------------- *)
type doc =
  | Nil
  | Text of string
  | Line (* newline + current indentation *)
  | Cat of doc * doc
  | Nest of int * doc (* indent sub-document by N more spaces *)

let ( ^^ ) a b = Cat (a, b)
let text s = Text s
let line = Line
let concat ds = List.fold_left ( ^^ ) Nil ds

(* Intersperse [sep] between docs. *)
let rec sepby sep = function
  | [] -> Nil
  | [ d ] -> d
  | d :: rest -> d ^^ sep ^^ sepby sep rest

let render (d : doc) : string =
  let buf = Buffer.create 1024 in
  let rec go indent = function
    | Nil -> ()
    | Text s -> Buffer.add_string buf s
    | Line ->
        Buffer.add_char buf '\n';
        for _ = 1 to indent do
          Buffer.add_char buf ' '
        done
    | Cat (a, b) ->
        go indent a;
        go indent b
    | Nest (n, a) -> go (indent + n) a
  in
  go 0 d;
  Buffer.contents buf

let indent_width = 2
let nest d = Nest (indent_width, d)
let parens d = text "(" ^^ d ^^ text ")"

(* --- Names / literals --------------------------------------------------- *)

(* MiniML string literal: the lexer supports only the backslash escapes n, t,
   backslash, and double-quote; every other byte (including CR and UTF-8) is
   taken literally, so emit it raw. *)
let string_lit s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* A rune literal: a character in single quotes ('a', '\n', or a UTF-8 char). *)
let rune_lit cp =
  let body =
    match cp with
    | 10 -> "\\n"
    | 9 -> "\\t"
    | 92 -> "\\\\"
    | 39 -> "\\'"
    | 0 -> "\\0"
    | _ when cp >= 32 && cp < 127 -> String.make 1 (Char.chr cp)
    | _ -> Utf8.rune_to_string cp
  in
  "'" ^ body ^ "'"

(* MiniML float literals are DECIMAL-ONLY (the lexer has no exponent syntax), so
   we must never emit scientific notation. Find the shortest decimal string that
   the actual lexer reads back as exactly [f]. *)
let float_lit f =
  let lexes_to s =
    match Lexer.tokenize s with
    | [ { Token.kind = Token.FLOAT g; _ }; { Token.kind = Token.EOF; _ } ] -> g = f
    | _ -> false
  in
  let has_exp s = String.contains s 'e' || String.contains s 'E' in
  (* shortest %g that is decimal (no exponent) and round-trips *)
  let rec try_g p =
    if p > 17 then None
    else
      let s = Printf.sprintf "%.*g" p f in
      if has_exp s then try_g (p + 1)
      else
        let s = if String.contains s '.' then s else s ^ ".0" in
        if lexes_to s then Some s else try_g (p + 1)
  in
  match try_g 1 with
  | Some s -> s
  | None ->
      (* %g insisted on an exponent (tiny/huge); expand to fixed-point decimal *)
      let rec try_f p =
        if p > 30 then Printf.sprintf "%.17f" f
        else
          let s = Printf.sprintf "%.*f" p f in
          if lexes_to s then s else try_f (p + 1)
      in
      try_f 1

(* --- Types -------------------------------------------------------------- *)

let rec doc_ty (t : Ast.ty_annot) : doc =
  match t with
  | Ast.TyName s -> text s
  | Ast.TyVar s -> text ("'" ^ s)
  | Ast.TyList t -> doc_ty_atom t ^^ text " list"
  | Ast.TyArray t -> doc_ty_atom t ^^ text " array"
  | Ast.TyArrow (a, b, eff) ->
      (* `a -> b` with an optional effect annotation AFTER the return type. *)
      let base = doc_ty_atom a ^^ text " -> " ^^ doc_ty b in
      (match eff with None -> base | Some e -> base ^^ text " / " ^^ doc_eff_bare e)
  | Ast.TyTuple ts -> sepby (text " * ") (List.map doc_ty_atom ts)
  | Ast.TyApp (args, name) -> (
      match args with
      | [] -> text name
      | [ a ] -> doc_ty_atom a ^^ text " " ^^ text name
      | _ -> parens (sepby (text ", ") (List.map doc_ty args)) ^^ text " " ^^ text name)
  | Ast.TyQualified (path, name) -> text (String.concat "." path ^ "." ^ name)
  | Ast.TyRecord (fields, is_open) ->
      let fs =
        List.map (fun (n, t) -> text (n ^ " : ") ^^ doc_ty t) fields
        @ if is_open then [ text ".." ] else []
      in
      text "{ " ^^ sepby (text "; ") fs ^^ text " }"
  | Ast.TyWithEffect (t, e) -> doc_ty t ^^ text " / " ^^ doc_eff_bare e
  | Ast.TyPolyVariant (kind, tags) ->
      let open_marker =
        match kind with Ast.PVExact -> "" | Ast.PVLower -> "> " | Ast.PVUpper -> "< "
      in
      let tag (name, payload) =
        match payload with
        | None -> text ("`" ^ name)
        | Some t -> text ("`" ^ name ^ " of ") ^^ doc_ty t
      in
      text ("[" ^ open_marker) ^^ sepby (text " | ") (List.map tag tags) ^^ text "]"

and doc_eff (e : Ast.eff_annot) : doc =
  (* rendered inside an arrow: -<eff>-> *)
  match e with Ast.EffAnnotPure -> Nil | Ast.EffAnnotRow _ -> doc_eff_bare e

and doc_eff_bare (e : Ast.eff_annot) : doc =
  match e with
  | Ast.EffAnnotPure -> text "pure"
  | Ast.EffAnnotRow items ->
      sepby (text ", ")
        (List.map
           (function
             | Ast.EffLabel (n, []) -> text n
             | Ast.EffLabel (n, args) ->
                 text n ^^ text " " ^^ sepby (text " ") (List.map doc_ty_atom args)
             | Ast.EffVar v -> text ("'" ^ v))
           items)

and doc_ty_atom (t : Ast.ty_annot) : doc =
  match t with
  | Ast.TyName _ | Ast.TyVar _ | Ast.TyQualified _ | Ast.TyRecord _
  | Ast.TyPolyVariant _ ->
      doc_ty t
  | _ -> parens (doc_ty t)

(* --- Patterns ----------------------------------------------------------- *)

let rec doc_pat (p : Ast.pattern) : doc =
  match p with
  | Ast.PatWild -> text "_"
  | Ast.PatVar s -> text s
  | Ast.PatInt n -> text (string_of_int n)
  | Ast.PatFloat f -> text (float_lit f)
  | Ast.PatBool b -> text (if b then "true" else "false")
  | Ast.PatString s -> text (string_lit s)
  | Ast.PatUnit -> text "()"
  | Ast.PatNil -> text "[]"
  | Ast.PatTuple ps -> parens (sepby (text ", ") (List.map doc_pat ps))
  | Ast.PatCons (a, b) -> doc_pat_atom a ^^ text " :: " ^^ doc_pat_atom b
  | Ast.PatConstruct (name, None) -> text name
  | Ast.PatConstruct (name, Some arg) -> text name ^^ text " " ^^ doc_pat_atom arg
  | Ast.PatRecord fields ->
      text "{ "
      ^^ sepby (text "; ")
           (List.map (fun (n, p) -> text (n ^ " = ") ^^ doc_pat p) fields)
      ^^ text " }"
  | Ast.PatAs (p, name) -> doc_pat_atom p ^^ text (" as " ^ name)
  | Ast.PatOr (a, b) -> doc_pat a ^^ text " | " ^^ doc_pat b
  | Ast.PatArray ps -> text "#[" ^^ sepby (text "; ") (List.map doc_pat ps) ^^ text "]"
  | Ast.PatPolyVariant (tag, None) -> text ("`" ^ tag)
  | Ast.PatPolyVariant (tag, Some p) -> text ("`" ^ tag ^ " ") ^^ doc_pat_atom p
  | Ast.PatPin name -> text ("^" ^ name)
  | Ast.PatAnnot (p, t) -> parens (doc_pat p ^^ text " : " ^^ doc_ty t)
  | Ast.PatMap entries ->
      text "#{"
      ^^ sepby (text "; ")
           (List.map (fun (k, v) -> doc_pat k ^^ text ": " ^^ doc_pat v) entries)
      ^^ text "}"
  | Ast.PatSet elems -> text "#{" ^^ sepby (text "; ") (List.map doc_pat elems) ^^ text "}"

and doc_pat_atom (p : Ast.pattern) : doc =
  match p with
  | Ast.PatWild | Ast.PatVar _ | Ast.PatInt _ | Ast.PatFloat _ | Ast.PatBool _
  | Ast.PatString _ | Ast.PatUnit | Ast.PatNil | Ast.PatTuple _ | Ast.PatRecord _
  | Ast.PatArray _ | Ast.PatConstruct (_, None) | Ast.PatPolyVariant (_, None)
  | Ast.PatPin _ | Ast.PatMap _ | Ast.PatSet _ ->
      doc_pat p
  | _ -> parens (doc_pat p)

(* --- Expressions (paren-liberal) ---------------------------------------- *)

let binop_str = function
  | Ast.Add -> "+" | Ast.Sub -> "-" | Ast.Mul -> "*" | Ast.Div -> "/"
  | Ast.Mod -> "mod" | Ast.Eq -> "=" | Ast.Neq -> "<>" | Ast.Lt -> "<"
  | Ast.Gt -> ">" | Ast.Le -> "<=" | Ast.Ge -> ">=" | Ast.And -> "&&"
  | Ast.Or -> "||" | Ast.Concat -> "^" | Ast.Land -> "land" | Ast.Lor -> "lor"
  | Ast.Lxor -> "lxor" | Ast.Lsl -> "lsl" | Ast.Lsr -> "lsr" | Ast.Pipe -> "|>"

let unop_str = function Ast.Neg -> "-" | Ast.Not -> "not " | Ast.Lnot -> "lnot "

(* An identifier that is actually an operator (e.g. used as a value, [fold (+)])
   must be parenthesized to print/reparse as a value. *)
let is_keyword_operator = function
  | "land" | "lor" | "lxor" | "lsl" | "lsr" | "lnot" | "mod" | "not" -> true
  | _ -> false

let is_operator_name s =
  s <> ""
  &&
  let c = s.[0] in
  (not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'))
  || is_keyword_operator s

let doc_var s =
  (* A qualified operator like `Eq.(=)` is stored as the name "Eq.="; it must
     print with the operator parenthesized: `Eq.(=)`, not `Eq.=`. *)
  match String.rindex_opt s '.' with
  | Some i when i > 0 && i < String.length s - 1 ->
      let modpath = String.sub s 0 i in
      let last = String.sub s (i + 1) (String.length s - i - 1) in
      if is_operator_name last then text (modpath ^ ".(" ^ last ^ ")") else text s
  | _ -> if is_operator_name s then text ("(" ^ s ^ ")") else text s

(* `(type 'a 'b)` locally-abstract-type clause for `let rec`. *)
let tp_clause = function
  | [] -> Nil
  | tps -> text " (type " ^^ sepby (text " ") (List.map (fun t -> text ("'" ^ t)) tps) ^^ text ")"

let rec strip_loc (e : Ast.expr) : Ast.expr =
  match e with Ast.ELoc (_, e) -> strip_loc e | e -> e

let is_atom (e : Ast.expr) : bool =
  match strip_loc e with
  | Ast.EInt _ | Ast.EFloat _ | Ast.EBool _ | Ast.EString _ | Ast.EByte _
  | Ast.ERune _ | Ast.EUnit | Ast.EVar _ | Ast.ENil | Ast.ETuple _ | Ast.EList _
  | Ast.EArray _ | Ast.ERecord _ | Ast.EMap _ | Ast.ESet _
  | Ast.EConstruct (_, None) | Ast.EPolyVariant (_, None) | Ast.EField _
  | Ast.EIndex _ ->
      true
  | _ -> false

(* A "greedy" expression's printed form ends with an open sub-expression that
   would swallow following tokens (e.g. a match arm body, a let-in body, a fn
   body absorbs a trailing `;`). Safe in tail positions; must be parenthesized
   as the left of a sequence or the right of an assignment. *)
(* Patterns we render as a destructure-let (`let (a, b) = e in ..`) when they are
   the sole arm of a partial match. Restricted to tuple/record: these always
   reparse to the same single-arm partial EMatch. Cons/constructor/literal/var
   patterns either don't round-trip through let-binding or parse as a different
   node, so those stay as an explicit `@partial match`. *)
let is_destructure_pat (p : Ast.pattern) : bool =
  match p with Ast.PatTuple _ | Ast.PatRecord _ -> true | _ -> false

(* Would a following `; stmt` be swallowed? (left of a sequence / assignment RHS)
   let/fn/match/handle bodies extend greedily; a sequence inherits from its tail.
   if/else does NOT — its else-branch is parsed stop-at-`;`. *)
let rec absorbs_semicolon (e : Ast.expr) : bool =
  match strip_loc e with
  | Ast.EFun _ | Ast.EMatch _ | Ast.EHandle _ | Ast.ELet _ | Ast.ELetRec _
  | Ast.ELetMut _ | Ast.ELetRecAnd _ ->
      true
  | Ast.ESeq (_, b) -> absorbs_semicolon b
  | _ -> false

(* Would a following `| arm` be swallowed? (a match/handler arm body) Only an
   open arm-list (match/handle) does — plus anything whose TAIL is one. *)
let rec absorbs_pipe (e : Ast.expr) : bool =
  match strip_loc e with
  | Ast.EMatch _ | Ast.EHandle _ -> true
  | Ast.ELet (_, _, b) | Ast.ELetMut (_, _, b) | Ast.ELetRec (_, _, _, b) -> absorbs_pipe b
  | Ast.ELetRecAnd (_, b) | Ast.EFun (_, b) -> absorbs_pipe b
  | Ast.EIf (_, _, f) -> ( match strip_loc f with Ast.EUnit -> false | _ -> absorbs_pipe f)
  | Ast.ESeq (_, b) -> absorbs_pipe b
  | _ -> false

let rec doc_expr (e : Ast.expr) : doc =
  match strip_loc e with
  | Ast.EInt n -> text (string_of_int n)
  | Ast.EFloat f -> text (float_lit f)
  | Ast.EBool b -> text (if b then "true" else "false")
  | Ast.EString s -> text (string_lit s)
  | Ast.EByte n -> text (Printf.sprintf "#%02x" n)
  | Ast.ERune n -> text (rune_lit n)
  | Ast.EUnit -> text "()"
  | Ast.EVar s -> doc_var s
  | Ast.ENil -> text "[]"
  | Ast.ETuple es -> parens (sepby (text ", ") (List.map doc_expr es))
  | Ast.EList es -> text "[" ^^ sepby (text "; ") (List.map doc_expr es) ^^ text "]"
  | Ast.EArray es -> text "#[" ^^ sepby (text "; ") (List.map doc_expr es) ^^ text "]"
  | Ast.ERecord fields -> doc_record_fields fields
  | Ast.ERecordUpdate (base, fields) ->
      text "{ " ^^ doc_atom base ^^ text " with "
      ^^ sepby (text "; ")
           (List.map (fun (n, e) -> text (n ^ " = ") ^^ doc_expr e) fields)
      ^^ text " }"
  | Ast.EField (e, f) -> doc_atom e ^^ text ("." ^ f)
  | Ast.EIndex (e, i) -> doc_atom e ^^ text ".[" ^^ doc_expr i ^^ text "]"
  | Ast.EConstruct (name, None) -> text name
  | Ast.EConstruct (name, Some arg) -> text name ^^ text " " ^^ doc_arg arg
  | Ast.EPolyVariant (tag, None) -> text ("`" ^ tag)
  | Ast.EPolyVariant (tag, Some arg) -> text ("`" ^ tag ^ " ") ^^ doc_arg arg
  | Ast.ECons (a, b) -> doc_atom a ^^ text " :: " ^^ doc_atom b
  | Ast.EBinop (op, l, r) ->
      doc_atom l ^^ text (" " ^ binop_str op ^ " ") ^^ doc_atom r
  | Ast.EUnop (op, e) -> text (unop_str op) ^^ doc_atom e
  | Ast.EApp _ as app -> doc_app app
  | Ast.EAnnot (e, t) -> parens (doc_expr e ^^ text " : " ^^ doc_ty t)
  | Ast.ECoerce (e, t) -> parens (doc_expr e ^^ text " :> " ^^ doc_ty t)
  | Ast.EAssign (name, e) -> text (name ^ " := ") ^^ doc_seq_lhs e
  | Ast.EFieldAssign (obj, f, e) ->
      doc_atom obj ^^ text ("." ^ f ^ " := ") ^^ doc_seq_lhs e
  | Ast.EIf (c, t, f) -> doc_if c t f
  | Ast.ELet (name, v, body) ->
      text ("let " ^ name ^ " = ") ^^ doc_expr v ^^ text " in" ^^ line ^^ doc_expr body
  | Ast.ELetMut (name, v, body) ->
      text ("let mut " ^ name ^ " = ") ^^ doc_expr v ^^ text " in" ^^ line ^^ doc_expr body
  | Ast.ELetRec (name, type_params, v, body) ->
      (* the string list is locally-abstract TYPE params; value params are
         already wrapped inside [v]. *)
      text "let rec" ^^ tp_clause type_params ^^ text " " ^^ doc_var name
      ^^ text " = " ^^ doc_expr v ^^ text " in" ^^ line ^^ doc_expr body
  | Ast.ESeq (a, b) -> doc_seq_lhs a ^^ text ";" ^^ line ^^ doc_expr b
  | Ast.EFun (param, body) -> doc_fun [ param ] body
  | Ast.EMatch (scrut, arms, kind) -> doc_match scrut arms kind
  | Ast.EReturn e -> text "return " ^^ doc_atom e
  | Ast.EBreak None -> text "break"
  | Ast.EBreak (Some e) -> text "break " ^^ doc_atom e
  | Ast.EContinueLoop None -> text "continue"
  | Ast.EContinueLoop (Some e) -> text "continue " ^^ doc_atom e
  | Ast.EPerform (op, arg) -> text ("perform " ^ op ^ " ") ^^ doc_atom arg
  | Ast.EResume (k, v) -> text "resume " ^^ doc_atom k ^^ text " " ^^ doc_atom v
  | Ast.ELocalOpen (m, e) -> text (m ^ ".(") ^^ doc_expr e ^^ text ")"
  | Ast.EWhile { while_cond; while_body } ->
      (* No `while` keyword in MiniML: a while-loop is `for cond do body end`. *)
      text "for " ^^ doc_expr while_cond ^^ text " do"
      ^^ nest (line ^^ doc_expr while_body) ^^ line ^^ text "end"
  | Ast.EForNumeric { fn_var; fn_init; fn_cond; fn_step; fn_body } ->
      text ("for " ^ fn_var ^ " = ") ^^ doc_expr fn_init ^^ text "; "
      ^^ doc_expr fn_cond ^^ text "; " ^^ doc_expr fn_step ^^ text " do"
      ^^ nest (line ^^ doc_expr fn_body) ^^ line ^^ text "end"
  | Ast.EFor { for_var; for_iter; for_body; for_index } ->
      let idx = match for_index with Some i -> text (" with index " ^ i) | None -> Nil in
      text ("for " ^ for_var ^ " in ") ^^ doc_expr for_iter ^^ idx ^^ text " do"
      ^^ nest (line ^^ doc_expr for_body) ^^ line ^^ text "end"
  | Ast.EForFold { loop_var; loop_iter; accum_var; accum_init; fold_body; fold_index } ->
      let idx = match fold_index with Some i -> text (" with index " ^ i) | None -> Nil in
      text ("for " ^ loop_var ^ " in ") ^^ doc_expr loop_iter ^^ idx
      ^^ text (" with " ^ accum_var ^ " = ") ^^ doc_expr accum_init ^^ text " do"
      ^^ nest (line ^^ doc_expr fold_body) ^^ line ^^ text "end"
  | Ast.EWhileLet (pat, e, body) ->
      text "for let " ^^ doc_pat pat ^^ text " = " ^^ doc_expr e ^^ text " do"
      ^^ nest (line ^^ doc_expr body) ^^ line ^^ text "end"
  | Ast.EHandle (body, arms) -> doc_handle body arms
  | Ast.EMap pairs -> doc_map pairs
  | Ast.ESet es -> text "#{" ^^ sepby (text "; ") (List.map doc_expr es) ^^ text "}"
  | Ast.ELetRecAnd (bindings, body) -> (
      let one kw (name, tps, e) =
        text kw ^^ tp_clause tps ^^ text " " ^^ doc_var name ^^ text " = " ^^ doc_expr e
      in
      match bindings with
      | [] -> doc_expr body
      | first :: rest ->
          one "let rec" first
          ^^ concat (List.map (fun b -> line ^^ one "and" b) rest)
          ^^ text " in" ^^ line ^^ doc_expr body)
  (* Type-annotated collection literals: `#Name[..]` (list/array/set) and
     `#Name{k: v; ..}` (map). *)
  | Ast.ECollTyped (name, elems) ->
      text ("#" ^ name ^ "[") ^^ sepby (text "; ") (List.map doc_expr elems) ^^ text "]"
  | Ast.EMapTyped (_, []) -> raise (Unsupported "empty typed map literal")
  | Ast.EMapTyped (name, pairs) ->
      text ("#" ^ name ^ "{")
      ^^ sepby (text "; ")
           (List.map (fun (k, v) -> doc_expr k ^^ text ": " ^^ doc_expr v) pairs)
      ^^ text "}"
  | Ast.ELoc _ -> assert false (* stripped above *)

and doc_record_fields fields =
  text "{ "
  ^^ sepby (text "; ")
       (List.map (fun (n, e) -> text (n ^ " = ") ^^ doc_expr e) fields)
  ^^ text " }"

and doc_map pairs =
  text "#{"
  ^^ sepby (text "; ")
       (List.map (fun (k, v) -> doc_expr k ^^ text ": " ^^ doc_expr v) pairs)
  ^^ text "}"

(* An expression used as an operand / argument: atoms print bare, everything
   else is parenthesized (paren-liberal => always semantics-preserving). *)
and doc_atom (e : Ast.expr) : doc =
  if is_atom e then doc_expr e else parens (doc_expr e)

(* An argument in application / constructor position: nullary constructors and
   polyvariants would grab the following argument as their own ([f None x] would
   reparse as [f (None x)]), so parenthesize anything that could absorb a
   neighbour. *)
and doc_arg (e : Ast.expr) : doc =
  match strip_loc e with
  | Ast.EConstruct _ | Ast.EPolyVariant _ -> parens (doc_expr e)
  | _ -> doc_atom e

(* In a sequence-LHS / assignment-RHS position, parenthesize if a following `;`
   would be absorbed. *)
and doc_seq_lhs (e : Ast.expr) : doc =
  if absorbs_semicolon e then parens (doc_expr e) else doc_expr e

(* In a match/handler arm body, parenthesize if a following `|` would be absorbed. *)
and doc_arm_body (e : Ast.expr) : doc =
  if absorbs_pipe e then parens (doc_expr e) else doc_expr e

and doc_app (e : Ast.expr) : doc =
  (* flatten left-nested application *)
  let rec flatten acc e =
    match strip_loc e with Ast.EApp (f, x) -> flatten (x :: acc) f | head -> (head, acc)
  in
  let head, args = flatten [] e in
  doc_atom head ^^ concat (List.map (fun a -> text " " ^^ doc_arg a) args)

and doc_if c t f =
  (* `if c do A else B` takes NO `end` (the parser consumes `end` only for the
     else-less form `if c do A end`). *)
  match strip_loc f with
  | Ast.EUnit ->
      text "if " ^^ doc_expr c ^^ text " do"
      ^^ nest (line ^^ doc_expr t) ^^ line ^^ text "end"
  | _ ->
      (* The else-branch is parsed with parse_expr_no_seq, so a bare `;`
         sequence there must be parenthesized to stay inside the branch. *)
      let f_doc =
        match strip_loc f with Ast.ESeq _ -> parens (doc_expr f) | _ -> doc_expr f
      in
      text "if " ^^ doc_expr c ^^ text " do"
      ^^ nest (line ^^ doc_expr t)
      ^^ line ^^ text "else"
      ^^ nest (line ^^ f_doc)

and doc_fun params body =
  text "fn "
  ^^ sepby (text " ") (List.map doc_param params)
  ^^ text " -> " ^^ doc_expr body

and doc_param (p : Ast.param) : doc =
  match (p.name, p.annot) with
  (* The unit param prints as `()`; printing it as `(_ : unit)` would reparse
     through the pattern path as a fresh-named destructure param. *)
  | "_", Some (Ast.TyName "unit") -> text "()"
  | _, None -> text p.name
  | _, Some t -> parens (text p.name ^^ text " : " ^^ doc_ty t)

and doc_match scrut arms kind =
  match (kind, arms) with
  (* A single-arm partial match over a DESTRUCTURE pattern is a destructure-let;
     print it that way (canonical, and avoids `@partial` which can't follow `;`).
     A var/wildcard pattern would instead parse as a plain ELet/etc., so only
     refractor genuine destructures. *)
  | Ast.Partial, [ (pat, None, body) ] when is_destructure_pat pat ->
      text "let " ^^ doc_pat pat ^^ text " = " ^^ doc_expr scrut ^^ text " in"
      ^^ line ^^ doc_expr body
  | _ -> doc_match_full scrut arms kind

and doc_match_full scrut arms kind =
  let prefix = match kind with Ast.Partial -> text "@partial" ^^ line | Ast.Total -> Nil in
  let arm (pat, guard, body) =
    let g = match guard with Some e -> text " when " ^^ doc_expr e | None -> Nil in
    (* A match/handle body would absorb the next arm's `|`; parenthesize it. *)
    line ^^ text "| " ^^ doc_pat pat ^^ g ^^ text " -> " ^^ doc_arm_body body
  in
  prefix ^^ text "match " ^^ doc_expr scrut ^^ text " with"
  ^^ concat (List.map arm arms)

and doc_handle body arms =
  let arm = function
    | Ast.HReturn (name, b) ->
        line ^^ text ("| return " ^ name ^ " -> ") ^^ doc_arm_body b
    | Ast.HOp { op_name; arg; k; body } ->
        line ^^ text (Printf.sprintf "| %s %s %s -> " op_name arg k) ^^ doc_arm_body body
  in
  text "handle " ^^ doc_expr body ^^ text " with" ^^ concat (List.map arm arms)

(* --- Declarations ------------------------------------------------------- *)

let doc_params params = concat (List.map (fun p -> text " " ^^ doc_param p) params)

let doc_ret_annot = function None -> Nil | Some t -> text " : " ^^ doc_ty t

let doc_constraints = function
  | [] -> Nil
  | cs ->
      text " where "
      ^^ sepby (text ", ")
           (List.map
              (fun (cls, vars) ->
                text (cls ^ " " ^ String.concat " " (List.map (fun v -> "'" ^ v) vars)))
              cs)

let doc_type_def_binding (b : Ast.type_def_binding) : doc =
  let params =
    match b.td_params with
    | [] -> Nil
    | [ p ] -> text ("'" ^ p ^ " ")
    | ps -> parens (sepby (text ", ") (List.map (fun p -> text ("'" ^ p)) ps)) ^^ text " "
  in
  let deriving =
    match b.td_deriving with
    | [] -> Nil
    | ds -> text (" deriving " ^ String.concat ", " ds)
  in
  let rhs =
    match b.td_def with
    | Ast.TDAlias t -> doc_ty t
    | Ast.TDRecord fields ->
        text "{ "
        ^^ sepby (text "; ")
             (List.map
                (fun (f : Ast.record_field_def) ->
                  (if f.rfd_mutable then text "mut " else Nil)
                  ^^ text (f.rfd_name ^ " : ")
                  ^^ doc_ty f.rfd_type)
                fields)
        ^^ text " }"
    | Ast.TDVariant ctors ->
        let ctor (name, arg, ret) =
          match (arg, ret) with
          | None, None -> text name
          | Some t, None -> text (name ^ " of ") ^^ doc_ty t
          (* GADT: `Name : arg -> ret` (or `Name : ret` with no argument). The
             arg uses doc_ty_atom so a function/tuple arg is parenthesized —
             else `(a -> b) -> r` would reparse as the tuple-collapsed `a*b->r`. *)
          | Some a, Some r -> text (name ^ " : ") ^^ doc_ty_atom a ^^ text " -> " ^^ doc_ty r
          | None, Some r -> text (name ^ " : ") ^^ doc_ty r
        in
        sepby (text " | ") (List.map ctor ctors)
    | Ast.TDNewtype (cname, t) -> text (cname ^ " of ") ^^ doc_ty t
  in
  params ^^ text (b.td_name ^ " = ") ^^ rhs ^^ deriving

let type_kw (b : Ast.type_def_binding) =
  match b.td_def with Ast.TDNewtype _ -> "newtype " | _ -> "type "

let rec doc_decl (d : Ast.decl) : doc =
  match d with
  | Ast.DLet { name; params; ret_annot; constraints; body } ->
      text "let " ^^ doc_var name ^^ doc_params params ^^ doc_ret_annot ret_annot
      ^^ doc_constraints constraints ^^ text " =" ^^ nest (line ^^ doc_expr body)
  | Ast.DLetMut (name, body) ->
      text "let mut " ^^ doc_var name ^^ text " =" ^^ nest (line ^^ doc_expr body)
  | Ast.DLetRec b ->
      text "let rec" ^^ tp_clause b.type_params ^^ text " " ^^ doc_var b.lr_name
      ^^ doc_params b.params ^^ doc_ret_annot b.ret_annot
      ^^ doc_constraints b.constraints ^^ text " =" ^^ nest (line ^^ doc_expr b.body)
  | Ast.DLetRecAnd bs ->
      let one kw (b : Ast.letrec_binding) =
        text kw ^^ tp_clause b.type_params ^^ text " " ^^ doc_var b.lr_name
        ^^ doc_params b.params ^^ doc_ret_annot b.ret_annot
        ^^ doc_constraints b.constraints ^^ text " =" ^^ nest (line ^^ doc_expr b.body)
      in
      (match bs with
      | [] -> Nil
      | first :: rest ->
          one "let rec" first
          ^^ concat (List.map (fun b -> line ^^ one "and" b) rest))
  | Ast.DType b -> text (type_kw b) ^^ doc_type_def_binding b
  | Ast.DTypeAnd bs -> (
      match bs with
      | [] -> Nil
      | first :: rest ->
          text (type_kw first) ^^ doc_type_def_binding first
          ^^ concat (List.map (fun b -> line ^^ text "and " ^^ doc_type_def_binding b) rest))
  | Ast.DExpr e -> doc_expr e
  | Ast.DExtern (name, t) -> text "extern " ^^ doc_var name ^^ text " : " ^^ doc_ty t
  | Ast.DOpen (m, None) -> text ("open " ^ m)
  | Ast.DOpen (m, Some names) -> text (Printf.sprintf "open %s (%s)" m (String.concat ", " names))
  | Ast.DClass { class_name; tyvars; fundeps; methods } ->
      let tvs = concat (List.map (fun v -> text (" '" ^ v)) tyvars) in
      let fds =
        match fundeps with
        | [] -> Nil
        | _ ->
            let dep (from_vs, to_vs) =
              let vs vs = String.concat " " (List.map (fun v -> "'" ^ v) vs) in
              text (vs from_vs ^ " -> " ^ vs to_vs)
            in
            text " where " ^^ sepby (text ", ") (List.map dep fundeps)
      in
      let meth (name, t) = line ^^ doc_var name ^^ text " : " ^^ doc_ty t in
      text "class " ^^ text class_name ^^ tvs ^^ fds ^^ text " ="
      ^^ nest (concat (List.map meth methods))
      ^^ line ^^ text "end"
  | Ast.DInstance { inst_class; inst_types; inst_constraints; inst_methods } ->
      let tys = concat (List.map (fun t -> text " " ^^ doc_ty_atom t) inst_types) in
      let meth (name, params, body) =
        line ^^ text "let " ^^ doc_var name ^^ doc_params params ^^ text " = "
        ^^ doc_expr body
      in
      text "instance " ^^ text inst_class ^^ tys
      ^^ doc_constraints inst_constraints ^^ text " ="
      ^^ nest (concat (List.map meth inst_methods))
      ^^ line ^^ text "end"
  | Ast.DEffect (name, type_params, ops) ->
      let tps = concat (List.map (fun v -> text (" '" ^ v)) type_params) in
      let op (n, t) = line ^^ text (n ^ " : ") ^^ doc_ty t in
      text "effect " ^^ text name ^^ tps ^^ text " ="
      ^^ nest (concat (List.map op ops))
      ^^ line ^^ text "end"
  | Ast.DModule (name, items) ->
      (* Separate inner decls with `;;` (an optional module separator) so none
         can merge with the next on reparse, mirroring top-level. *)
      let item md = doc_module_decl md ^^ text ";;" in
      text ("module " ^ name ^ " =")
      ^^ nest (concat (List.map (fun md -> line ^^ item md) items))
      ^^ line ^^ text "end"

and doc_module_decl (md : Ast.module_decl) : doc =
  let vis = match md.vis with Ast.Public -> "pub " | Ast.Private -> "" | Ast.Opaque -> "opaque " in
  text vis ^^ doc_decl md.decl

let format_program (prog : Ast.program) : string =
  let docs = List.map doc_decl prog in
  (* Separate top-level declarations with `;;` so adjacent decls never merge
     (e.g. a `let mut x = 0` followed by `x := 5`, or a decl body absorbing the
     next decl as an application). *)
  let joined = sepby (text ";;" ^^ line ^^ line) docs in
  render joined ^ "\n"

let format_source (src : string) : string =
  let tokens = Lexer.tokenize src in
  let prog = Parser.parse_program tokens in
  format_program prog

(* --- Deep loc-stripping, for the semantic-preservation property --------- *)

(* Remove every ELoc wrapper so two structurally-equal ASTs that differ only in
   source positions compare equal with polymorphic (=). Patterns and types carry
   no expressions, so only expression positions need recursion. *)
(* [is_generated] is an internal desugaring hint, not semantics: a destructure
   or unit param prints back to a plain param that reparses with the flag off.
   Normalize it away so structural equality reflects MEANING. *)
let norm_param (p : Ast.param) : Ast.param = { p with Ast.is_generated = false }

let rec strip_e (e : Ast.expr) : Ast.expr =
  match e with
  | Ast.ELoc (_, e) -> strip_e e
  | Ast.EInt _ | Ast.EFloat _ | Ast.EBool _ | Ast.EString _ | Ast.EByte _
  | Ast.ERune _ | Ast.EUnit | Ast.EVar _ | Ast.ENil ->
      e
  | Ast.ELet (n, a, b) -> Ast.ELet (n, strip_e a, strip_e b)
  | Ast.ELetRec (n, ps, a, b) -> Ast.ELetRec (n, ps, strip_e a, strip_e b)
  | Ast.EFun (p, b) -> Ast.EFun (norm_param p, strip_e b)
  | Ast.EApp (f, x) -> Ast.EApp (strip_e f, strip_e x)
  | Ast.EIf (c, t, f) -> Ast.EIf (strip_e c, strip_e t, strip_e f)
  | Ast.EBinop (op, l, r) -> Ast.EBinop (op, strip_e l, strip_e r)
  | Ast.EUnop (op, e) -> Ast.EUnop (op, strip_e e)
  | Ast.ETuple es -> Ast.ETuple (List.map strip_e es)
  | Ast.ERecord fs -> Ast.ERecord (List.map (fun (n, e) -> (n, strip_e e)) fs)
  | Ast.ERecordUpdate (b, fs) ->
      Ast.ERecordUpdate (strip_e b, List.map (fun (n, e) -> (n, strip_e e)) fs)
  | Ast.EField (e, f) -> Ast.EField (strip_e e, f)
  | Ast.EIndex (e, i) -> Ast.EIndex (strip_e e, strip_e i)
  | Ast.ECons (a, b) -> Ast.ECons (strip_e a, strip_e b)
  | Ast.EList es -> Ast.EList (List.map strip_e es)
  | Ast.EArray es -> Ast.EArray (List.map strip_e es)
  | Ast.EConstruct (n, eo) -> Ast.EConstruct (n, Option.map strip_e eo)
  | Ast.EMatch (s, arms, k) ->
      Ast.EMatch
        ( strip_e s,
          List.map (fun (p, g, b) -> (p, Option.map strip_e g, strip_e b)) arms,
          k )
  | Ast.ELetMut (n, a, b) -> Ast.ELetMut (n, strip_e a, strip_e b)
  | Ast.EAssign (n, e) -> Ast.EAssign (n, strip_e e)
  | Ast.EFieldAssign (o, f, e) -> Ast.EFieldAssign (strip_e o, f, strip_e e)
  | Ast.ESeq _ ->
      (* Sequencing is associative: `(a; b); c` and `a; b; c` mean the same but
         nest differently. Canonicalize to a right-nested chain so structural
         equality reflects meaning (the formatter flattens seq layout anyway). *)
      let rec flatten e =
        match e with
        | Ast.ELoc (_, e) -> flatten e
        | Ast.ESeq (a, b) -> flatten a @ flatten b
        | e -> [ strip_e e ]
      in
      let rec rebuild = function
        | [] -> Ast.EUnit
        | [ x ] -> x
        | x :: rest -> Ast.ESeq (x, rebuild rest)
      in
      rebuild (flatten e)
  | Ast.EAnnot (e, t) -> Ast.EAnnot (strip_e e, t)
  | Ast.EWhile { while_cond; while_body } ->
      Ast.EWhile { while_cond = strip_e while_cond; while_body = strip_e while_body }
  | Ast.EFor r -> Ast.EFor { r with for_iter = strip_e r.for_iter; for_body = strip_e r.for_body }
  | Ast.EForFold r ->
      Ast.EForFold
        { r with loop_iter = strip_e r.loop_iter; accum_init = strip_e r.accum_init; fold_body = strip_e r.fold_body }
  | Ast.EWhileLet (p, e, b) -> Ast.EWhileLet (p, strip_e e, strip_e b)
  | Ast.EForNumeric r ->
      Ast.EForNumeric
        { r with fn_init = strip_e r.fn_init; fn_cond = strip_e r.fn_cond; fn_step = strip_e r.fn_step; fn_body = strip_e r.fn_body }
  | Ast.EBreak eo -> Ast.EBreak (Option.map strip_e eo)
  | Ast.EContinueLoop eo -> Ast.EContinueLoop (Option.map strip_e eo)
  | Ast.EReturn e -> Ast.EReturn (strip_e e)
  | Ast.ELetRecAnd (bs, b) ->
      Ast.ELetRecAnd (List.map (fun (n, ps, e) -> (n, ps, strip_e e)) bs, strip_e b)
  | Ast.EPerform (op, e) -> Ast.EPerform (op, strip_e e)
  | Ast.EHandle (b, arms) ->
      Ast.EHandle
        ( strip_e b,
          List.map
            (function
              | Ast.HReturn (n, e) -> Ast.HReturn (n, strip_e e)
              | Ast.HOp r -> Ast.HOp { r with body = strip_e r.body })
            arms )
  | Ast.EResume (k, v) -> Ast.EResume (strip_e k, strip_e v)
  | Ast.EMap ps -> Ast.EMap (List.map (fun (k, v) -> (strip_e k, strip_e v)) ps)
  | Ast.EMapTyped (t, ps) -> Ast.EMapTyped (t, List.map (fun (k, v) -> (strip_e k, strip_e v)) ps)
  | Ast.ESet es -> Ast.ESet (List.map strip_e es)
  | Ast.ECollTyped (t, es) -> Ast.ECollTyped (t, List.map strip_e es)
  | Ast.EPolyVariant (tag, eo) -> Ast.EPolyVariant (tag, Option.map strip_e eo)
  | Ast.ECoerce (e, t) -> Ast.ECoerce (strip_e e, t)
  | Ast.ELocalOpen (m, e) -> Ast.ELocalOpen (m, strip_e e)

let strip_letrec (b : Ast.letrec_binding) : Ast.letrec_binding =
  { b with params = List.map norm_param b.params; body = strip_e b.body }

let rec strip_decl (d : Ast.decl) : Ast.decl =
  match d with
  | Ast.DLet r ->
      Ast.DLet { r with params = List.map norm_param r.params; body = strip_e r.body }
  | Ast.DLetMut (n, e) -> Ast.DLetMut (n, strip_e e)
  | Ast.DLetRec b -> Ast.DLetRec (strip_letrec b)
  | Ast.DLetRecAnd bs -> Ast.DLetRecAnd (List.map strip_letrec bs)
  | Ast.DExpr e -> Ast.DExpr (strip_e e)
  | Ast.DInstance r ->
      Ast.DInstance
        { r with inst_methods = List.map (fun (n, ps, e) -> (n, List.map norm_param ps, strip_e e)) r.inst_methods }
  | Ast.DModule (n, items) ->
      Ast.DModule
        (n, List.map (fun (md : Ast.module_decl) -> { md with Ast.decl = strip_decl md.decl }) items)
  | (Ast.DType _ | Ast.DTypeAnd _ | Ast.DClass _ | Ast.DEffect _ | Ast.DExtern _
    | Ast.DOpen _) as d ->
      d

let strip_program (p : Ast.program) : Ast.program = List.map strip_decl p
