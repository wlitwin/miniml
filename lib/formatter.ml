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
       where a `;` or `|` would otherwise be absorbed. Binop/cons operands are
       the one refinement: redundant parens are dropped by a precedence model
       (binop_prec) that mirrors the parser's binding powers exactly.
     - IDEMPOTENCE: format (format src) = format src.

   Increment 1: expression + core-declaration core. Increment 2: module / class /
   instance / effect declarations, faithful string/rune/float literals, and the
   sequence/arm/destructure delimiting that real (self-host) code exercises.
   Increment 3: GADT constructors, map/set patterns, typed-collection (#Name[..]
   / #Name{..}) and poly-variant-type literals, expression-level let-rec(-and) —
   the corpus is now ~fully covered (only empty typed-map literals are unhandled).
   Comment preservation (incr 1-4): declaration comments (leading own-line +
   trailing same-line) are recovered from the lossless CST trivia and woven back
   at declaration boundaries — at top level, in module bodies, and recursively.
   Inline (intra-declaration) comments are re-anchored to the next significant
   token and flushed own-line at that token during rendering (see [render] /
   [Mark]): statement comments in expression bodies, comments between
   class/instance/effect members (the parser brackets members in CstDecl nodes
   so their offsets are recoverable), and — via an entry [Mark] on every
   expression — comments before ANY sub-expression (an application argument, a
   list element, a binop operand). The only comments still dropped sit before a
   bare delimiter or keyword the AST does not model a position for (`in`, `)`,
   `->`); dropping is always safe for the invariants. *)

exception Unsupported of string

(* Raised by the comment-reanchoring pass when it can't confidently map comments
   back onto the formatted tree; [format_source] catches it and falls back to
   comment-free formatting. A dedicated exception (not stdlib [Exit] under a
   catch-all) so the fallback is explicit — and so it translates to a specific
   MiniML effect handler rather than an inexpressible catch-all-over-exceptions. *)
exception Reanchor_bail

(* A recovered comment, with the offset of the significant token it precedes
   ([cnext]) so it can be re-anchored to that token during rendering. Defined
   here (above [render]) because [render] flushes inline comments at [Mark]s. *)
type comment = {
  cs : int; (* absolute start offset *)
  ce : int; (* absolute end offset (exclusive) *)
  cnext : int; (* offset of the next significant token (the re-anchor point) *)
  ctext : string; (* exact source spelling, line comments right-trimmed *)
  cown : bool; (* starts its own line (vs. trailing code on the same line) *)
}

(* Inline (intra-declaration) comments awaiting emission, sorted by [cs]. The
   renderer consumes this as it reaches each [Mark]; comments whose anchor is
   never reached stay here and are simply dropped (always safe). Set per format
   by [format_source_with_comments]; empty for the plain ([format_program])
   path, which makes every [Mark] a no-op. *)
let render_pending : comment list ref = ref []

(* The start offsets of a class/instance/effect's members, set by the
   declaration weaver around a [doc_decl] call so its member loop can emit a
   leading [Mark] per member (anchoring comments between members). [None] on the
   plain path — no member marks. *)
let member_marks : int array option ref = ref None

(* A comment split into the lines it should render as. The first line is verbatim
   (placed wherever the cursor is); a multi-line block comment's continuation
   lines are re-based — their common leading indentation removed — so the caller
   can re-emit them at the current indentation (continuations otherwise keep
   their original absolute column and drift out of alignment under nesting).
   Single-line comments (every `--` comment) yield one line, unchanged. *)
let comment_lines (c : comment) : string list =
  let lead l =
    let n = ref 0 in
    while !n < String.length l && (l.[!n] = ' ' || l.[!n] = '\t') do
      incr n
    done;
    !n
  in
  match String.split_on_char '\n' c.ctext with
  | ([] | [ _ ]) as ls -> ls
  | first :: rest ->
      let nonblank = List.filter (fun l -> String.trim l <> "") rest in
      (* Minimum leading-whitespace across the non-blank continuation lines.
         Seed the fold with the first line's lead (0 when there are none) rather
         than max_int — MiniML has no max_int, and this keeps the shared source
         in the common subset. *)
      let minlead =
        match nonblank with
        | [] -> 0
        | hd :: tl -> List.fold_left (fun m l -> min m (lead l)) (lead hd) tl
      in
      let fix l =
        if String.trim l = "" then ""
        else
          let li = lead l in
          String.make (li - minlead) ' ' ^ String.sub l li (String.length l - li)
      in
      first :: List.map fix rest

(* --- A Wadler/Prettier-style document combinator with width-based wrapping.

   [Line] is a HARD break (always a newline) — the structural separators the
   printer has always emitted, plus the comment [Mark] anchors. [GLine]/[GSoft]
   are SOFT breaks that only exist inside a [Group]: the group is laid out flat
   (GLine -> space, GSoft -> nothing) when its flattened form fits the remaining
   width, otherwise broken (both -> newline + indent). A group that contains a
   hard break or a comment-flushing [Mark] is forced to break.

   Crucially the flat-or-break choice depends ONLY on the document (derived from
   the AST), never on the input's whitespace — so width wrapping stays
   idempotent and semantics-preserving. *)
type doc =
  | Nil
  | Text of string
  | Line (* hard break: newline + current indentation *)
  | GLine (* soft break: space when flat, newline+indent when broken *)
  | GSoft (* soft break: nothing when flat, newline+indent when broken *)
  | Cat of doc * doc
  | Nest of int * doc (* indent sub-document by N more spaces *)
  | Group of doc (* lay out flat if it fits, else break its soft lines *)
  | Mark of int
    (* a re-anchor point at source offset N: at render time, flush every pending
       inline comment whose [cnext] is N, each on its own line at the current
       indentation. A no-op when nothing matches. *)

let ( ^^ ) a b = Cat (a, b)
let text s = Text s
let line = Line
let concat ds = List.fold_left ( ^^ ) Nil ds

(* Intersperse [sep] between docs. *)
let rec sepby sep = function
  | [] -> Nil
  | [ d ] -> d
  | d :: rest -> d ^^ sep ^^ sepby sep rest

let max_width = 80

let render (d : doc) : string =
  let buf = Buffer.create 1024 in
  let newline indent =
    Buffer.add_char buf '\n';
    for _ = 1 to indent do
      Buffer.add_char buf ' '
    done
  in
  let will_flush off = List.exists (fun c -> c.cnext = off) !render_pending in
  (* A group must break if it contains a hard [Line] or a comment-flushing
     [Mark] anywhere (those emit a newline regardless of mode). *)
  let rec forces_break = function
    | Line -> true
    | Mark off -> will_flush off
    | Cat (a, b) -> forces_break a || forces_break b
    | Nest (_, a) | Group a -> forces_break a
    | _ -> false
  in
  (* Does the flat layout of [items] fit before the next newline within [rem]
     columns? [items] is a stack of (indent, flat?, doc). *)
  let rec fits rem items =
    if rem < 0 then false
    else
      match items with
      | [] -> true
      | (i, flat, d) :: rest -> (
          match d with
          | Nil -> fits rem rest
          | Text s -> fits (rem - String.length s) rest
          | Cat (a, b) -> fits rem ((i, flat, a) :: (i, flat, b) :: rest)
          | Nest (n, a) -> fits rem ((i + n, flat, a) :: rest)
          | Group a -> fits rem ((i, true, a) :: rest)
          | Line -> true
          | GLine -> if flat then fits (rem - 1) rest else true
          | GSoft -> if flat then fits rem rest else true
          | Mark off -> if will_flush off then true else fits rem rest)
  in
  let rec go col stack =
    match stack with
    | [] -> ()
    | (i, flat, d) :: rest -> (
        match d with
        | Nil -> go col rest
        | Text s ->
            Buffer.add_string buf s;
            go (col + String.length s) rest
        | Cat (a, b) -> go col ((i, flat, a) :: (i, flat, b) :: rest)
        | Nest (n, a) -> go col ((i + n, flat, a) :: rest)
        | Line ->
            newline i;
            go i rest
        | GLine ->
            if flat then begin
              Buffer.add_char buf ' ';
              go (col + 1) rest
            end
            else begin
              newline i;
              go i rest
            end
        | GSoft ->
            if flat then go col rest
            else begin
              newline i;
              go i rest
            end
        | Group a ->
            let flat' =
              (not (forces_break a)) && fits (max_width - col) ((i, true, a) :: rest)
            in
            go col ((i, flat', a) :: rest)
        | Mark off ->
            (* Flush, in source order, every pending inline comment anchored
               here; each on its own line at the current indentation. *)
            let flushed = ref false in
            render_pending :=
              List.filter
                (fun c ->
                  if c.cnext = off then begin
                    (match comment_lines c with
                    | [] -> ()
                    | first :: conts ->
                        Buffer.add_string buf first;
                        List.iter
                          (fun l ->
                            newline i;
                            Buffer.add_string buf l)
                          conts);
                    newline i;
                    flushed := true;
                    false
                  end
                  else true)
                !render_pending;
            go (if !flushed then i else col) rest)
  in
  go 0 [ (0, false, d) ];
  Buffer.contents buf

let indent_width = 2
let nest d = Nest (indent_width, d)
let parens d = text "(" ^^ d ^^ text ")"
let group d = Group d

(* A bracketed, width-wrappable sequence: flat on one line when it fits, else
   one element per line, the delimiters on their own lines. [pad] puts a space
   inside the delimiters when flat (records); otherwise they hug (lists, etc.).*)
let wrap ~op ~cl ~pad ~sep (items : doc list) : doc =
  match items with
  | [] -> text op ^^ text cl
  | _ ->
      let edge = if pad then GLine else GSoft in
      group
        (text op
        ^^ nest (edge ^^ sepby (text sep ^^ GLine) items)
        ^^ edge ^^ text cl)

(* --- Names / literals --------------------------------------------------- *)

(* MiniML string literal: the lexer supports only the backslash escapes n, t,
   backslash, and double-quote; every other byte (including CR and UTF-8) is
   taken literally, so emit it raw. *)
let string_lit s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  (* Iterate by BYTE index, not String.iter: in MiniML String.iter yields runes
     (decoded codepoints), so a multi-byte UTF-8 char would round-trip through a
     single truncated byte. String.get / String.length are byte-based in both
     OCaml and MiniML, so this emits every byte verbatim in both compilers. *)
  let n = String.length s in
  let rec go i =
    if i < n then begin
      (match String.get s i with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c);
      go (i + 1)
    end
  in
  go 0;
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

(* A match/handler arm body that renders as its own multi-line block stays on
   the arm line right after `->` (the block breaks internally). A flatter body
   is wrapped in a group so an over-wide one breaks onto the next line instead
   of overflowing. *)
let body_stays_inline (e : Ast.expr) : bool =
  match strip_loc e with
  | Ast.ELet _ | Ast.ELetMut _ | Ast.ELetRec _ | Ast.ELetRecAnd _ | Ast.ESeq _
  | Ast.EMatch _ | Ast.EHandle _ | Ast.EIf _ | Ast.EWhile _ | Ast.EFor _
  | Ast.EForFold _ | Ast.EForNumeric _ | Ast.EWhileLet _ ->
      true
  | _ -> false

(* Operator precedence for parenthesizing binop operands — higher binds tighter.
   Mirrors the parser's binding powers (parser.ml: bp_of_binop): |> < || < && <
   comparisons < (`::` = 6) < +/-/^/lor/lxor < *///mod/land/lsl/lsr. All
   left-associative except `::` (right). Used only to DROP parentheses the
   paren-liberal printer would otherwise add; getting it wrong can only fail the
   semantic-preservation gate, never silently change meaning. *)
let binop_prec = function
  | Ast.Pipe -> 1
  | Ast.Or -> 2
  | Ast.And -> 3
  | Ast.Eq | Ast.Neq | Ast.Lt | Ast.Gt | Ast.Le | Ast.Ge -> 4
  | Ast.Add | Ast.Sub | Ast.Concat | Ast.Lor | Ast.Lxor -> 7
  | Ast.Mul | Ast.Div | Ast.Mod | Ast.Land | Ast.Lsl | Ast.Lsr -> 8

let cons_prec = 6

(* How an operand parenthesizes against an enclosing operator: a self-delimiting
   or tighter-than-any-operator expression never needs parens; an operator child
   is compared by precedence/associativity; anything else (let / if / match / fn
   / seq / unop / annot / ...) stays parenthesized (paren-liberal, always safe). *)
type operand_kind = Tight | OpK of int | Loose

let operand_kind (e : Ast.expr) : operand_kind =
  match strip_loc e with
  | Ast.EBinop (op, _, _) -> OpK (binop_prec op)
  | Ast.ECons _ -> OpK cons_prec
  | Ast.EInt _ | Ast.EFloat _ | Ast.EBool _ | Ast.EString _ | Ast.EByte _
  | Ast.ERune _ | Ast.EUnit | Ast.EVar _ | Ast.ENil | Ast.ETuple _ | Ast.EList _
  | Ast.EArray _ | Ast.ERecord _ | Ast.ERecordUpdate _ | Ast.EMap _ | Ast.ESet _
  | Ast.EConstruct (_, None) | Ast.EPolyVariant (_, None) | Ast.EField _
  | Ast.EIndex _ | Ast.EApp _ ->
      Tight
  | _ -> Loose

(* Flatten the left spine of a same-precedence binop chain into a head operand
   and a list of (operator, operand) steps, so the whole chain wraps as one
   group (operators starting each continuation line) instead of nesting. Only
   same-precedence (associative) steps are pulled in; a differently-bound operand
   stays a single sub-expression (its own group / parenthesized as needed). *)
let rec flatten_binop prec (e : Ast.expr) : Ast.expr * (Ast.binop * Ast.expr) list =
  match strip_loc e with
  | Ast.EBinop (op, l, r) when binop_prec op = prec ->
      let first, rest = flatten_binop prec l in
      (first, rest @ [ (op, r) ])
  | _ -> (e, [])

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
   if/else absorbs through its else-branch: the branch itself is parsed
   stop-at-`;`, but a greedy tail there (a let-in body, match, fn) is parsed
   with the full expression grammar and so swallows a following `;`. *)
let rec absorbs_semicolon (e : Ast.expr) : bool =
  match strip_loc e with
  | Ast.EFun _ | Ast.EMatch _ | Ast.EHandle _ | Ast.ELet _ | Ast.ELetRec _
  | Ast.ELetMut _ | Ast.ELetRecAnd _ ->
      true
  | Ast.ESeq (_, b) -> absorbs_semicolon b
  | Ast.EIf (_, _, f) -> (
      match strip_loc f with Ast.EUnit -> false | _ -> absorbs_semicolon f)
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

(* The source offset where [e] begins (its leftmost token), if known. Drives
   [mark]: inline comments are re-anchored to the next token's offset, and a
   statement printed at a line break is preceded by a [Mark] of its start. *)
let rec start_offset (e : Ast.expr) : int option =
  match e with
  | Ast.ELoc (loc, _) -> Some loc.offset
  | Ast.ESeq (a, _) | Ast.EBinop (_, a, _) | Ast.ECons (a, _) | Ast.EApp (a, _)
  | Ast.EField (a, _) | Ast.EIndex (a, _) ->
      start_offset a
  | _ -> None

(* A re-anchor point for inline comments immediately preceding statement [e].
   Placed right after the [line] that opens [e]'s line, so flushed comments land
   own-line at the right indentation. A no-op when [e]'s start is unknown or no
   pending comment anchors there. *)
let mark (e : Ast.expr) : doc =
  match start_offset e with Some o -> Mark o | None -> Nil

(* A leading [Mark] for the i-th class/instance/effect member, if the weaver has
   supplied member offsets (anchors comments sitting between members). *)
let member_mark i : doc =
  match !member_marks with
  | Some arr when i < Array.length arr -> Mark arr.(i)
  | _ -> Nil

let rec doc_expr (e : Ast.expr) : doc =
  (* An entry [Mark] at every expression catches inline comments that sit before
     ANY token, not just statement anchors — `f a (* c *) b`, a comment before a
     list element, etc. A [Mark] whose offset no pending comment targets is inert
     (no width or layout effect), so comment-free output is unchanged; when one
     fires it forces the enclosing width group to break and lands the comment on
     its own line just before the token. *)
  mark e ^^ doc_expr_body e

and doc_expr_body (e : Ast.expr) : doc =
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
  | Ast.ETuple es -> wrap ~op:"(" ~cl:")" ~pad:false ~sep:"," (List.map doc_expr es)
  | Ast.EList es -> wrap ~op:"[" ~cl:"]" ~pad:false ~sep:";" (List.map doc_expr es)
  | Ast.EArray es -> wrap ~op:"#[" ~cl:"]" ~pad:false ~sep:";" (List.map doc_expr es)
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
  | Ast.ECons (a, b) ->
      doc_oper ~parent_prec:cons_prec ~parent_right:true ~left:true a
      ^^ text " :: "
      ^^ doc_oper ~parent_prec:cons_prec ~parent_right:true ~left:false b
  | Ast.EBinop (op, _, _) ->
      (* Flatten the same-precedence chain and lay it out as one group: flat on
         one line, or each subsequent operand on its own line led by its
         operator (`a\n+ b\n+ c`). *)
      let p = binop_prec op in
      let first, steps = flatten_binop p (strip_loc e) in
      group
        (doc_oper ~parent_prec:p ~parent_right:false ~left:true first
        ^^ concat
             (List.map
                (fun (o, operand) ->
                  GLine
                  ^^ text (binop_str o ^ " ")
                  ^^ doc_oper ~parent_prec:p ~parent_right:false ~left:false operand)
                steps))
  | Ast.EUnop (op, e) -> text (unop_str op) ^^ doc_atom e
  | Ast.EApp _ as app -> doc_app app
  | Ast.EAnnot (e, t) -> parens (doc_expr e ^^ text " : " ^^ doc_ty t)
  | Ast.ECoerce (e, t) -> parens (doc_expr e ^^ text " :> " ^^ doc_ty t)
  | Ast.EAssign (name, e) -> text (name ^ " := ") ^^ doc_seq_lhs e
  | Ast.EFieldAssign (obj, f, e) ->
      doc_atom obj ^^ text ("." ^ f ^ " := ") ^^ doc_seq_lhs e
  | Ast.EIf (c, t, f) -> doc_if c t f
  | Ast.ELet (name, v, body) ->
      text ("let " ^ name ^ " = ") ^^ doc_expr v ^^ text " in" ^^ line
      ^^ mark body ^^ doc_expr body
  | Ast.ELetMut (name, v, body) ->
      text ("let mut " ^ name ^ " = ") ^^ doc_expr v ^^ text " in" ^^ line
      ^^ mark body ^^ doc_expr body
  | Ast.ELetRec (name, type_params, v, body) ->
      (* the string list is locally-abstract TYPE params; value params are
         already wrapped inside [v]. *)
      text "let rec" ^^ tp_clause type_params ^^ text " " ^^ doc_var name
      ^^ text " = " ^^ doc_expr v ^^ text " in" ^^ line ^^ mark body ^^ doc_expr body
  | Ast.ESeq (a, b) -> doc_seq_lhs a ^^ text ";" ^^ line ^^ mark b ^^ doc_expr b
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
      ^^ nest (line ^^ mark while_body ^^ doc_expr while_body) ^^ line ^^ text "end"
  | Ast.EForNumeric { fn_var; fn_init; fn_cond; fn_step; fn_body } ->
      text ("for " ^ fn_var ^ " = ") ^^ doc_expr fn_init ^^ text "; "
      ^^ doc_expr fn_cond ^^ text "; " ^^ doc_expr fn_step ^^ text " do"
      ^^ nest (line ^^ mark fn_body ^^ doc_expr fn_body) ^^ line ^^ text "end"
  | Ast.EFor { for_var; for_iter; for_body; for_index } ->
      let idx = match for_index with Some i -> text (" with index " ^ i) | None -> Nil in
      text ("for " ^ for_var ^ " in ") ^^ doc_expr for_iter ^^ idx ^^ text " do"
      ^^ nest (line ^^ mark for_body ^^ doc_expr for_body) ^^ line ^^ text "end"
  | Ast.EForFold { loop_var; loop_iter; accum_var; accum_init; fold_body; fold_index } ->
      let idx = match fold_index with Some i -> text (" with index " ^ i) | None -> Nil in
      text ("for " ^ loop_var ^ " in ") ^^ doc_expr loop_iter ^^ idx
      ^^ text (" with " ^ accum_var ^ " = ") ^^ doc_expr accum_init ^^ text " do"
      ^^ nest (line ^^ mark fold_body ^^ doc_expr fold_body) ^^ line ^^ text "end"
  | Ast.EWhileLet (pat, e, body) ->
      text "for let " ^^ doc_pat pat ^^ text " = " ^^ doc_expr e ^^ text " do"
      ^^ nest (line ^^ mark body ^^ doc_expr body) ^^ line ^^ text "end"
  | Ast.EHandle (body, arms) -> doc_handle body arms
  | Ast.EMap pairs -> doc_map pairs
  | Ast.ESet es -> wrap ~op:"#{" ~cl:"}" ~pad:false ~sep:";" (List.map doc_expr es)
  | Ast.ELetRecAnd (bindings, body) -> (
      let one kw (name, tps, e) =
        text kw ^^ tp_clause tps ^^ text " " ^^ doc_var name ^^ text " = " ^^ doc_expr e
      in
      match bindings with
      | [] -> doc_expr body
      | first :: rest ->
          one "let rec" first
          ^^ concat (List.map (fun b -> line ^^ one "and" b) rest)
          ^^ text " in" ^^ line ^^ mark body ^^ doc_expr body)
  (* Type-annotated collection literals: `#Name[..]` (list/array/set) and
     `#Name{k: v; ..}` (map). *)
  | Ast.ECollTyped (name, elems) ->
      wrap ~op:("#" ^ name ^ "[") ~cl:"]" ~pad:false ~sep:";" (List.map doc_expr elems)
  | Ast.EMapTyped (name, pairs) ->
      (* The empty case prints as `#Name{}`, which the parser keeps as an empty
         map (parse_set_or_map treats `#{}` as a map for backwards compat), so it
         round-trips via [wrap]'s empty branch. *)
      wrap ~op:("#" ^ name ^ "{") ~cl:"}" ~pad:false ~sep:";"
        (List.map (fun (k, v) -> doc_expr k ^^ text ": " ^^ doc_expr v) pairs)
  | Ast.ELoc _ -> assert false (* stripped above *)

and doc_record_fields fields =
  wrap ~op:"{" ~cl:"}" ~pad:true ~sep:";"
    (List.map (fun (n, e) -> text (n ^ " = ") ^^ doc_expr e) fields)

and doc_map pairs =
  wrap ~op:"#{" ~cl:"}" ~pad:false ~sep:";"
    (List.map (fun (k, v) -> doc_expr k ^^ text ": " ^^ doc_expr v) pairs)

(* An expression used as an operand / argument: atoms print bare, everything
   else is parenthesized (paren-liberal => always semantics-preserving). *)
and doc_atom (e : Ast.expr) : doc =
  if is_atom e then doc_expr e else parens (doc_expr e)

(* A binop / cons operand: drop the parentheses [doc_atom] would add when
   precedence and associativity make them redundant. [parent_prec] is the
   enclosing operator's precedence, [parent_right] its right-associativity, and
   [left] which side this operand is on. An equal-precedence child needs parens
   only on the associativity's "wrong" side (right child of a left-assoc op, or
   left child of a right-assoc op); a looser child always, a tighter child
   never. Non-operator/keyword operands stay parenthesized. *)
and doc_oper ~parent_prec ~parent_right ~left (e : Ast.expr) : doc =
  let needs =
    match operand_kind e with
    | Tight -> false
    | Loose -> true
    | OpK cp ->
        if cp <> parent_prec then cp < parent_prec
        else if left then parent_right
        else not parent_right
  in
  if needs then parens (doc_expr e) else doc_expr e

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
  (* Flat: `head a b c`. Too wide: the head stays, each argument on its own
     indented line. *)
  group (doc_atom head ^^ nest (concat (List.map (fun a -> GLine ^^ doc_arg a) args)))

and doc_if c t f =
  (* `if c do A else B` takes NO `end` (the parser consumes `end` only for the
     else-less form `if c do A end`). *)
  match strip_loc f with
  | Ast.EUnit ->
      text "if " ^^ doc_expr c ^^ text " do"
      ^^ nest (line ^^ mark t ^^ doc_expr t) ^^ line ^^ text "end"
  | _ ->
      (* The else-branch is parsed with parse_expr_no_seq, so a bare `;`
         sequence there must be parenthesized to stay inside the branch. A
         parenthesized branch gets a formatter-added `(`, so its start no longer
         matches a comment's next-token offset — skip [mark] there to stay
         idempotent. *)
      let f_doc, f_mark =
        match strip_loc f with
        | Ast.ESeq _ -> (parens (doc_expr f), Nil)
        | _ -> (doc_expr f, mark f)
      in
      text "if " ^^ doc_expr c ^^ text " do"
      ^^ nest (line ^^ mark t ^^ doc_expr t)
      ^^ line ^^ text "else"
      ^^ nest (line ^^ f_mark ^^ f_doc)

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
      ^^ line ^^ mark body ^^ doc_expr body
  | _ -> doc_match_full scrut arms kind

(* Attach an arm body after its `[head] ->`: inline for a block-like body, else
   in a group so an over-wide body breaks onto the next indented line. *)
and doc_arm body_doc body =
  if body_stays_inline body then body_doc ^^ text " " ^^ doc_arm_body body
  else body_doc ^^ group (nest (GLine ^^ doc_arm_body body))

and doc_match_full scrut arms kind =
  let prefix = match kind with Ast.Partial -> text "@partial" ^^ line | Ast.Total -> Nil in
  let arm (pat, guard, body) =
    let g = match guard with Some e -> text " when " ^^ doc_expr e | None -> Nil in
    (* A match/handle body would absorb the next arm's `|`; parenthesize it. *)
    doc_arm (line ^^ text "| " ^^ doc_pat pat ^^ g ^^ text " ->") body
  in
  prefix ^^ text "match " ^^ doc_expr scrut ^^ text " with"
  ^^ concat (List.map arm arms)

and doc_handle body arms =
  let arm = function
    | Ast.HReturn (name, b) -> doc_arm (line ^^ text ("| return " ^ name ^ " ->")) b
    | Ast.HOp { op_name; arg; k; body } ->
        doc_arm (line ^^ text (Printf.sprintf "| %s %s %s ->" op_name arg k)) body
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
      ^^ doc_constraints constraints ^^ text " ="
      ^^ nest (line ^^ mark body ^^ doc_expr body)
  | Ast.DLetMut (name, body) ->
      text "let mut " ^^ doc_var name ^^ text " ="
      ^^ nest (line ^^ mark body ^^ doc_expr body)
  | Ast.DLetRec b ->
      text "let rec" ^^ tp_clause b.type_params ^^ text " " ^^ doc_var b.lr_name
      ^^ doc_params b.params ^^ doc_ret_annot b.ret_annot
      ^^ doc_constraints b.constraints ^^ text " ="
      ^^ nest (line ^^ mark b.body ^^ doc_expr b.body)
  | Ast.DLetRecAnd bs ->
      let one kw (b : Ast.letrec_binding) =
        text kw ^^ tp_clause b.type_params ^^ text " " ^^ doc_var b.lr_name
        ^^ doc_params b.params ^^ doc_ret_annot b.ret_annot
        ^^ doc_constraints b.constraints ^^ text " ="
        ^^ nest (line ^^ mark b.body ^^ doc_expr b.body)
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
      let meth i (name, t) =
        line ^^ member_mark i ^^ doc_var name ^^ text " : " ^^ doc_ty t
      in
      text "class " ^^ text class_name ^^ tvs ^^ fds ^^ text " ="
      ^^ nest (concat (List.mapi meth methods))
      ^^ line ^^ text "end"
  | Ast.DInstance { inst_class; inst_types; inst_constraints; inst_methods } ->
      let tys = concat (List.map (fun t -> text " " ^^ doc_ty_atom t) inst_types) in
      let meth i (name, params, body) =
        line ^^ member_mark i ^^ text "let " ^^ doc_var name ^^ doc_params params
        ^^ text " = " ^^ doc_expr body
      in
      text "instance " ^^ text inst_class ^^ tys
      ^^ doc_constraints inst_constraints ^^ text " ="
      ^^ nest (concat (List.mapi meth inst_methods))
      ^^ line ^^ text "end"
  | Ast.DEffect (name, type_params, ops) ->
      let tps = concat (List.map (fun v -> text (" '" ^ v)) type_params) in
      let op i (n, t) = line ^^ member_mark i ^^ text (n ^ " : ") ^^ doc_ty t in
      text "effect " ^^ text name ^^ tps ^^ text " ="
      ^^ nest (concat (List.mapi op ops))
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
  (* No source, hence no comments to weave: every [Mark] must be inert. *)
  render_pending := [];
  let docs = List.map doc_decl prog in
  (* Separate top-level declarations with `;;` so adjacent decls never merge
     (e.g. a `let mut x = 0` followed by `x := 5`, or a decl body absorbing the
     next decl as an application). *)
  let joined = sepby (text ";;" ^^ line ^^ line) docs in
  render joined ^ "\n"

(* --- Comment preservation (roadmap #21, comment increment 1) -------------

   The AST is desugared and carries source positions only on expression atoms
   (ELoc), so it cannot, on its own, tell us where comments sat. The lossless
   CST (cst.ml) can: every comment lives in the inter-token TRIVIA, and each
   token's exact span tiles the source. So we recover comments straight from
   that trivia and weave them back at DECLARATION boundaries — the boundaries
   the CST records as [Decl] nodes, which exist even for the declaration forms
   that have no AST position at all (type / open / extern / effect / class).

   Increment 1 handles TOP-LEVEL declarations only: a comment in the gap before
   a declaration becomes its leading comment (own its line) or, if it sits on
   the same line as the preceding declaration, that declaration's trailing
   comment. Comments INSIDE a declaration's span (intra-expression, module
   bodies) are dropped for now — exactly as before this increment — and woven
   in by later increments. Dropping is always safe for the formatter's two
   invariants (it can neither change the parsed AST nor break idempotence).

   Increment 3 adds INTRA-declaration (inline) comments: comments inside a leaf
   declaration's span are re-anchored to the next significant token and emitted
   own-line at that token's [Mark] during rendering (see [render]). A comment
   whose next token is not a statement anchor (e.g. between operands of `+`) has
   no [Mark] and is dropped. ([comment] itself is defined above [render].) *)

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t' || s.[!n - 1] = '\r') do
    decr n
  done;
  String.sub s 0 !n

(* Pull the comments out of one trivia run [s], which begins at absolute offset
   [abs]. Trivia is whitespace and comments only (the lexer guarantees no string
   literals leak in), so a flat scan is safe. [cown] is true when a newline
   separates the comment from the previous significant content. *)
let scan_trivia abs ~cnext (s : string) : comment list =
  let n = String.length s in
  let out = ref [] in
  let i = ref 0 in
  let nl = ref false in
  while !i < n do
    let c = s.[!i] in
    if c = '\n' then begin
      nl := true;
      incr i
    end
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if c = '(' && !i + 1 < n && s.[!i + 1] = '*' then begin
      let start = !i in
      i := !i + 2;
      let depth = ref 1 in
      while !depth > 0 && !i < n do
        if !i + 1 < n && s.[!i] = '(' && s.[!i + 1] = '*' then begin
          depth := !depth + 1;
          i := !i + 2
        end
        else if !i + 1 < n && s.[!i] = '*' && s.[!i + 1] = ')' then begin
          depth := !depth - 1;
          i := !i + 2
        end
        else incr i
      done;
      out :=
        {
          cs = abs + start;
          ce = abs + !i;
          cnext;
          ctext = String.sub s start (!i - start);
          cown = !nl;
        }
        :: !out;
      nl := false
    end
    else if c = '-' && !i + 1 < n && s.[!i + 1] = '-' then begin
      let start = !i in
      while !i < n && s.[!i] <> '\n' do
        incr i
      done;
      out :=
        {
          cs = abs + start;
          ce = abs + !i;
          cnext;
          ctext = rstrip (String.sub s start (!i - start));
          cown = !nl;
        }
        :: !out;
      nl := false
    end
    else incr i
  done;
  List.rev !out

(* Every comment in [src], in source order, recovered from token trivia. Each
   comment records [cnext] = the offset of the token whose leading trivia it
   sits in (the token it immediately precedes). *)
let all_comments (src : string) : comment list =
  let prev = ref 0 in
  List.concat_map
    (fun (p : Cst.piece) ->
      let abs = !prev in
      prev := p.token.end_offset;
      scan_trivia abs ~cnext:p.token.loc.offset p.leading)
    (Cst.of_source src)

(* The [start, end) source span of a CST node (excludes its leading trivia, so
   comments always fall in the gaps between sibling spans). *)
let node_span (node : Cst.tree) : int * int =
  match Cst.leaves node with
  | [] -> raise Reanchor_bail
  | ls ->
      let first = List.hd ls in
      let last = List.nth ls (List.length ls - 1) in
      (first.token.loc.offset, last.token.end_offset)

(* Whether a Decl node is a MODULE (its first significant token, skipping a
   `pub`/`opaque` prefix, is `module`). Only modules have their body comments
   handled by the recursive declaration weaver; a class/instance/effect also has
   child Decl nodes now (one per member) but is a comment LEAF — its body
   comments, between and inside members, are flushed by render-time [Mark]s. *)
let is_module_node (node : Cst.tree) : bool =
  let rec first = function
    | (p : Cst.piece) :: rest -> (
        match p.token.kind with
        | Token.PUB | Token.OPAQUE -> first rest
        | k -> k = Token.MODULE)
    | [] -> false
  in
  first (Cst.leaves node)

(* Spans of the LEAF declaration nodes — every declaration but a module (which
   recurses). Inline (intra-declaration) comments are exactly those inside a leaf
   span; a module's own body comments sit between its inner Decl nodes and are
   handled by the recursive declaration weaver instead. *)
let rec leaf_spans (t : Cst.tree) : (int * int) list =
  match t with
  | Cst.Node (Cst.Decl, children) ->
      if is_module_node t then List.concat_map leaf_spans children
      else [ node_span t ]
  | Cst.Node (_, children) -> List.concat_map leaf_spans children
  | Cst.Leaf _ -> []

(* The direct child CstDecl nodes of [node], in source order. For SourceFile
   these are the top-level decls; for a module's Decl node they are its body
   declarations (the parser brackets both, mirroring each other). *)
let child_decl_nodes (node : Cst.tree) : Cst.tree array =
  match node with
  | Cst.Node (_, children) ->
      Array.of_list
        (List.filter (function Cst.Node (Cst.Decl, _) -> true | _ -> false) children)
  | _ -> [||]

(* For a module's Decl node, the offset range of its BODY: from just after the
   header `=` to the `end` keyword. Module-body comments live in this range
   (minus the inner decl spans). *)
let module_body_range (node : Cst.tree) : int * int =
  match node with
  | Cst.Node (_, children) ->
      let rec find_lo before = function
        | Cst.Node (Cst.Decl, _) :: _ -> before
        | Cst.Leaf p :: rest -> find_lo p.token.end_offset rest
        | _ :: rest -> find_lo before rest
        | [] -> before
      in
      let lo = find_lo 0 children in
      let ls = Cst.leaves node in
      let hi = (List.nth ls (List.length ls - 1)).token.loc.offset in
      (lo, hi)
  | _ -> raise Reanchor_bail

(* How many AST decls a span's source slice parses to. Counting (not comparing)
   sidesteps the fresh-param counter differing between a full parse and a slice
   parse, and absorbs any 1-node-to-N-decls desugaring (top-level / module-body
   `let (a, b) = e`). Module-body slices are parsed wrapped so `pub`/`opaque`
   and the module-only grammar resolve. *)
let decls_in_slice ~top (src : string) ((s, e) : int * int) : int =
  let slice = String.sub src s (e - s) in
  if top then List.length (Parser.parse_program (Lexer.tokenize slice))
  else
    match Parser.parse_program (Lexer.tokenize ("module Fmt__Wrap =\n" ^ slice ^ "\nend")) with
    | [ Ast.DModule (_, items) ] -> List.length items
    | _ -> raise Reanchor_bail

(* Split [items] into consecutive groups of the given [counts] (one group per
   CST Decl node). Any length mismatch means our span/AST model is off — bail. *)
let segment (counts : int list) (items : 'a list) : 'a list list =
  let rec take k xs acc =
    if k = 0 then (List.rev acc, xs)
    else match xs with [] -> raise Reanchor_bail | x :: r -> take (k - 1) r (x :: acc)
  in
  let rec go counts items acc =
    match counts with
    | [] -> if items <> [] then raise Reanchor_bail else List.rev acc
    | k :: rest ->
        let g, items' = take k items [] in
        go rest items' (g :: acc)
  in
  go counts items []

(* A comment renders as its exact source spelling. Multi-line block comments
   keep their internal bytes verbatim (idempotent, lossless); only the first
   line picks up the surrounding indentation. *)
let doc_comment (c : comment) : doc =
  match comment_lines c with
  | [] -> Nil
  | first :: conts ->
      text first ^^ concat (List.map (fun l -> line ^^ text l) conts)

(* Render a sequence of sibling declarations (a SourceFile's or a module body's)
   with the comments in the gaps between them woven back. [top] selects the
   top-level vs module-body layout (`;;`-and-blank-line vs one-`;;`-per-line),
   [nodes]/[groups] are the CST Decl nodes and the AST decls segmented to match,
   and [lo, hi) bounds the comment range owned by this level. *)
let rec doc_level ~top (src : string) (nodes : Cst.tree array)
    (groups : Ast.module_decl list array) (comments : comment list) ~lo ~hi : doc =
  let n = Array.length nodes in
  let spans = Array.map node_span nodes in
  let inside cs = Array.exists (fun (s, e) -> s <= cs && cs < e) spans in
  let slot_of cs =
    let c = ref 0 in
    Array.iter (fun (_, e) -> if e <= cs then incr c) spans;
    !c
  in
  (* Bucket the comments owned by this level into the n+1 gap slots. Comments
     inside an inner span belong to that node (handled by recursion for modules,
     dropped for leaf decls — intra-expression, a later increment). *)
  let slots = Array.make (n + 1) [] in
  List.iter
    (fun c ->
      if c.cs >= lo && c.cs < hi && not (inside c.cs) then
        slots.(slot_of c.cs) <- c :: slots.(slot_of c.cs))
    comments;
  Array.iteri (fun i l -> slots.(i) <- List.rev l) slots;
  (* leading.(i): own-line comments above node i; trailing.(i): a same-line
     comment after node i; tail: own-line comments after the last node. *)
  let leading = Array.make (max n 1) [] in
  let trailing = Array.make (max n 1) None in
  let tail = ref [] in
  Array.iteri
    (fun i cs ->
      let cs, tr =
        match cs with
        | first :: rest when i >= 1 && not first.cown -> (rest, Some first)
        | _ -> (cs, None)
      in
      (match tr with Some c -> trailing.(i - 1) <- Some c | None -> ());
      if i < n then leading.(i) <- cs else tail := cs)
    slots;
  let trail_doc i =
    match trailing.(i) with Some c -> text " " ^^ doc_comment c | None -> Nil
  in
  if top then begin
    let frag i =
      let lead = concat (List.map (fun c -> doc_comment c ^^ line) leading.(i)) in
      let body =
        sepby (text ";;" ^^ line ^^ line)
          (List.map (doc_module_decl_c src nodes.(i) comments) groups.(i))
      in
      let sep_after = if i < n - 1 then text ";;" else Nil in
      let gap = if i < n - 1 then line ^^ line else Nil in
      lead ^^ body ^^ sep_after ^^ trail_doc i ^^ gap
    in
    let all = concat (List.init n frag) in
    let tail_doc =
      concat
        (List.mapi (fun j c -> (if j > 0 || n > 0 then line else Nil) ^^ doc_comment c) !tail)
    in
    all ^^ tail_doc
  end
  else begin
    (* Module body: every item on its own line, each suffixed with `;;`. *)
    let frag i =
      let lead = concat (List.map (fun c -> line ^^ doc_comment c) leading.(i)) in
      let items = groups.(i) in
      let last = List.length items - 1 in
      concat
        (List.mapi
           (fun j md ->
             (if j = 0 then lead else Nil)
             ^^ line
             ^^ doc_module_decl_c src nodes.(i) comments md
             ^^ text ";;"
             ^^ if j = last then trail_doc i else Nil)
           items)
    in
    let all = concat (List.init n frag) in
    let tail_doc = concat (List.map (fun c -> line ^^ doc_comment c) !tail) in
    all ^^ tail_doc
  end

(* Render one declaration item, comment-aware: a nested module recurses so its
   body comments are woven in; everything else delegates to the plain printer
   (whose [Mark]s flush this declaration's inline comments during rendering). *)
and doc_module_decl_c (src : string) (node : Cst.tree) (comments : comment list)
    (md : Ast.module_decl) : doc =
  let vis =
    match md.Ast.vis with Ast.Public -> "pub " | Ast.Private -> "" | Ast.Opaque -> "opaque "
  in
  match md.Ast.decl with
  | Ast.DModule (name, inner_items) ->
      let inner_nodes = child_decl_nodes node in
      let lo, hi = module_body_range node in
      let counts =
        Array.to_list
          (Array.map (fun nd -> decls_in_slice ~top:false src (node_span nd)) inner_nodes)
      in
      let groups = Array.of_list (segment counts inner_items) in
      if Array.length groups <> Array.length inner_nodes then raise Reanchor_bail;
      text vis
      ^^ text ("module " ^ name ^ " =")
      ^^ nest (doc_level ~top:false src inner_nodes groups comments ~lo ~hi)
      ^^ line ^^ text "end"
  | Ast.DClass { methods; _ } -> member_decl vis node (List.length methods) md.Ast.decl
  | Ast.DInstance { inst_methods; _ } ->
      member_decl vis node (List.length inst_methods) md.Ast.decl
  | Ast.DEffect (_, _, ops) -> member_decl vis node (List.length ops) md.Ast.decl
  | _ -> text vis ^^ doc_decl md.Ast.decl

(* Render a class/instance/effect, supplying [doc_decl] with the member start
   offsets (from the CST member nodes) so its loop emits a leading [Mark] per
   member — anchoring comments between members. Falls back to a plain render if
   the CST member count doesn't match the AST. *)
and member_decl vis (node : Cst.tree) (n_members : int) (decl : Ast.decl) : doc =
  let mnodes = child_decl_nodes node in
  if Array.length mnodes = n_members then
    member_marks := Some (Array.map (fun nd -> fst (node_span nd)) mnodes);
  let body = doc_decl decl in
  member_marks := None;
  text vis ^^ body

(* Render with declaration comments interleaved. Falls back to the plain printer
   on any structural surprise so comments can never make formatting worse. *)
let format_source_with_comments (src : string) (prog : Ast.program) : string =
  let tree = Cst_build.cst_of_source src in
  let top_nodes =
    match tree with
    | Cst.Node (Cst.SourceFile, _) -> child_decl_nodes tree
    | _ -> raise Reanchor_bail
  in
  let comments = all_comments src in
  let counts =
    Array.to_list (Array.map (fun nd -> decls_in_slice ~top:true src (node_span nd)) top_nodes)
  in
  let items = List.map (fun d -> Ast.{ vis = Private; decl = d }) prog in
  let groups = Array.of_list (segment counts items) in
  if Array.length groups <> Array.length top_nodes then raise Reanchor_bail;
  (* Inline comments — those inside a leaf declaration's span — are flushed at
     render time by their anchoring [Mark]s; the declaration weaver ([doc_level])
     handles the rest (gaps between declarations). *)
  let leaves = leaf_spans tree in
  render_pending :=
    List.filter
      (fun c -> List.exists (fun (s, e) -> s <= c.cs && c.cs < e) leaves)
      comments;
  let result =
    render
      (doc_level ~top:true src top_nodes groups comments ~lo:0 ~hi:(String.length src))
    ^ "\n"
  in
  render_pending := [];
  result

let format_source (src : string) : string =
  let tokens = Lexer.tokenize src in
  let prog = Parser.parse_program tokens in
  (* Comment-aware formatting, falling back to comment-free [format_program] if
     the reanchoring pass bails ([Reanchor_bail]). [Unsupported] is NOT caught —
     it propagates to the caller (the format runner / `mml fmt`), which skips
     unsupported constructs. Catching one named exception (not a catch-all) is
     what makes this expressible as a MiniML effect handler. *)
  try format_source_with_comments src prog with Reanchor_bail -> format_program prog

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
