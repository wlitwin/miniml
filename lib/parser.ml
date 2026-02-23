exception Parse_error of string * Token.loc

type t = {
  tokens: Token.token array;
  mutable pos: int;
}

let create tokens =
  { tokens = Array.of_list tokens; pos = 0 }

let peek p = p.tokens.(p.pos)
let peek_kind p = (peek p).kind
let peek_kind_at p offset =
  let idx = p.pos + offset in
  if idx < Array.length p.tokens then p.tokens.(idx).kind
  else Token.EOF

let advance p =
  let tok = p.tokens.(p.pos) in
  if tok.kind <> Token.EOF then p.pos <- p.pos + 1;
  tok

let error p msg =
  let tok = peek p in
  raise (Parse_error (msg, tok.loc))

let expect p kind =
  let tok = peek p in
  if tok.kind = kind then ignore (advance p)
  else error p (Printf.sprintf "expected %s, got %s"
    (Token.pp_token_kind kind) (Token.pp_token_kind tok.kind))

let expect_ident p =
  match (peek p).kind with
  | Token.IDENT s -> ignore (advance p); s
  | Token.UNDERSCORE -> ignore (advance p); "_"
  | _ -> error p "expected identifier"

let expect_uident p =
  match (peek p).kind with
  | Token.UIDENT s -> ignore (advance p); s
  | _ -> error p "expected constructor name"

(* Convert keyword tokens to their string name for use after DOT in qualified paths *)
let keyword_as_ident = function
  | Token.MOD -> Some "mod"
  | Token.NOT -> Some "not"
  | Token.LAND -> Some "land"
  | Token.LOR -> Some "lor"
  | Token.LXOR -> Some "lxor"
  | Token.LNOT -> Some "lnot"
  | Token.LSL -> Some "lsl"
  | Token.LSR -> Some "lsr"
  | _ -> None

let token_to_op_name = function
  | Token.PLUS -> Some "+"
  | Token.MINUS -> Some "-"
  | Token.STAR -> Some "*"
  | Token.SLASH -> Some "/"
  | Token.MOD -> Some "mod"
  | Token.CARET -> Some "^"
  | Token.AMPAMP -> Some "&&"
  | Token.PIPEPIPE -> Some "||"
  | Token.NOT -> Some "not"
  | Token.LT -> Some "<"
  | Token.GT -> Some ">"
  | Token.LE -> Some "<="
  | Token.GE -> Some ">="
  | Token.EQ -> Some "="
  | Token.NEQ -> Some "<>"
  | Token.LAND -> Some "land"
  | Token.LOR -> Some "lor"
  | Token.LXOR -> Some "lxor"
  | Token.LNOT -> Some "lnot"
  | Token.LSL -> Some "lsl"
  | Token.LSR -> Some "lsr"
  | _ -> None

(* Read a qualified path starting from an already-consumed UIDENT.
   Returns (module_path, final_segment_kind) where final_segment_kind
   tells what follows: lowercase ident, uppercase ident, or LPAREN (local open). *)
let read_qualified_path p (first_uident : string) =
  let path = ref [first_uident] in
  let result = ref None in
  let continue = ref true in
  while !continue do
    if peek_kind p = Token.DOT then begin
      let next = peek_kind_at p 1 in
      match next with
      | Token.IDENT s ->
        ignore (advance p); (* consume DOT *)
        ignore (advance p); (* consume IDENT *)
        let qualified = String.concat "." (List.rev !path) ^ "." ^ s in
        result := Some (`Var qualified);
        continue := false
      | Token.UIDENT s ->
        ignore (advance p); (* consume DOT *)
        ignore (advance p); (* consume UIDENT *)
        path := s :: !path
      | Token.LPAREN ->
        (* Check for qualified operator: ClassName.(op) *)
        (match token_to_op_name (peek_kind_at p 2) with
         | Some op when peek_kind_at p 3 = Token.RPAREN ->
           ignore (advance p); (* consume DOT *)
           ignore (advance p); (* consume LPAREN *)
           ignore (advance p); (* consume operator *)
           ignore (advance p); (* consume RPAREN *)
           let qualified = String.concat "." (List.rev !path) ^ "." ^ op in
           result := Some (`Var qualified);
           continue := false
         | _ ->
           ignore (advance p); (* consume DOT *)
           (* local open: Module.(expr) *)
           let mod_path = String.concat "." (List.rev !path) in
           result := Some (`LocalOpen mod_path);
           continue := false)
      | kw ->
        (match keyword_as_ident kw with
         | Some s ->
           ignore (advance p); (* consume DOT *)
           ignore (advance p); (* consume keyword *)
           let qualified = String.concat "." (List.rev !path) ^ "." ^ s in
           result := Some (`Var qualified);
           continue := false
         | None ->
           continue := false)
    end else
      continue := false
  done;
  match !result with
  | Some r -> r
  | None ->
    (* Path ended with a UIDENT, treat last segment as constructor *)
    match !path with
    | [single] -> `Constructor single
    | _ ->
      let rev = List.rev !path in
      let last = List.hd (List.rev rev) in
      let prefix = List.rev (List.tl (List.rev rev)) in
      let qualified = String.concat "." prefix ^ "." ^ last in
      `Constructor qualified

let at_expr_start p =
  match peek_kind p with
  | Token.INT _ | Token.FLOAT _ | Token.STRING _ | Token.INTERP_STRING _
  | Token.BYTE _ | Token.RUNE _
  | Token.TRUE | Token.FALSE
  | Token.IDENT _ | Token.UIDENT _ | Token.LPAREN | Token.LBRACE
  | Token.LBRACKET | Token.FN | Token.IF | Token.LET | Token.MATCH
  | Token.MINUS | Token.NOT | Token.LNOT
  | Token.PERFORM | Token.HANDLE | Token.TRY | Token.RESUME
  | Token.FOR | Token.BREAK | Token.CONTINUE | Token.RETURN
  | Token.HASH | Token.DO | Token.POLYTAG _ -> true
  | _ -> false

(* ---- Type annotation parsing ---- *)

let rec parse_ty_atom p =
  match peek_kind p with
  | Token.IDENT "int" -> ignore (advance p); Ast.TyName "int"
  | Token.IDENT "bool" -> ignore (advance p); Ast.TyName "bool"
  | Token.IDENT "string" -> ignore (advance p); Ast.TyName "string"
  | Token.IDENT "unit" -> ignore (advance p); Ast.TyName "unit"
  | Token.IDENT s -> ignore (advance p); Ast.TyName s
  | Token.UIDENT s ->
    ignore (advance p);
    (* Qualified type: Module.typename or Module.Inner.typename *)
    let mod_parts = ref [s] in
    let final_name = ref None in
    let continue_reading = ref true in
    while !continue_reading do
      if peek_kind p = Token.DOT then begin
        match peek_kind_at p 1 with
        | Token.UIDENT s2 ->
          ignore (advance p); ignore (advance p);
          mod_parts := s2 :: !mod_parts
        | Token.IDENT s2 ->
          ignore (advance p); ignore (advance p);
          final_name := Some s2;
          continue_reading := false
        | _ -> continue_reading := false
      end else
        continue_reading := false
    done;
    (match !final_name with
     | Some name ->
       (* Module.typename *)
       Ast.TyQualified (List.rev !mod_parts, name)
     | None ->
       (* Just a bare UIDENT — treat as type name *)
       Ast.TyName s)
  | Token.TYVAR s -> ignore (advance p); Ast.TyVar s
  | Token.LPAREN ->
    ignore (advance p);
    let ty = parse_ty p in
    if peek_kind p = Token.COMMA then begin
      let parts = ref [ty] in
      while peek_kind p = Token.COMMA do
        ignore (advance p);
        parts := parse_ty p :: !parts
      done;
      expect p Token.RPAREN;
      Ast.TyTuple (List.rev !parts)
    end else begin
      expect p Token.RPAREN;
      ty
    end
  | Token.LBRACE ->
    ignore (advance p);
    let (fields, is_open) = parse_ty_record_fields p in
    expect p Token.RBRACE;
    Ast.TyRecord (List.map (fun (_, n, t) -> (n, t)) fields, is_open)
  | Token.LBRACKET ->
    ignore (advance p);
    let kind = match peek_kind p with
      | Token.GT -> ignore (advance p); Ast.PVLower
      | Token.LT -> ignore (advance p); Ast.PVUpper
      | _ -> Ast.PVExact
    in
    if peek_kind p = Token.PIPE then ignore (advance p);
    let tags = ref [] in
    while (match peek_kind p with Token.POLYTAG _ -> true | _ -> false) do
      let tag = match (advance p).kind with
        | Token.POLYTAG s -> s
        | _ -> assert false
      in
      let payload = if peek_kind p = Token.OF then
        (ignore (advance p); Some (parse_ty p)) else None in
      tags := (tag, payload) :: !tags;
      if peek_kind p = Token.PIPE then ignore (advance p)
    done;
    expect p Token.RBRACKET;
    Ast.TyPolyVariant (kind, List.rev !tags)
  | _ -> error p "expected type"

and parse_ty_postfix p =
  let ty = parse_ty_atom p in
  let rec loop ty =
    match peek_kind p with
    | Token.IDENT "list" ->
      ignore (advance p);
      loop (Ast.TyList ty)
    | Token.IDENT "array" ->
      ignore (advance p);
      loop (Ast.TyArray ty)
    | Token.IDENT "map" ->
      ignore (advance p);
      (match ty with
       | Ast.TyTuple [k; v] -> loop (Ast.TyMap (k, v))
       | _ -> error p "map type requires two type arguments: (k, v) map")
    | Token.IDENT name when peek_kind_at p 1 <> Token.COLON ->
      ignore (advance p);
      let args = match ty with
        | Ast.TyTuple parts -> parts
        | _ -> [ty]
      in
      loop (Ast.TyApp (args, name))
    | Token.UIDENT _ when peek_kind_at p 1 = Token.DOT ->
      (* ('a, 'b) Module.typename *)
      let mod_parts = ref [] in
      let first_mod = match (advance p).kind with
        | Token.UIDENT s -> s
        | _ -> assert false
      in
      mod_parts := [first_mod];
      let final_name = ref "" in
      let continue_reading = ref true in
      while !continue_reading do
        if peek_kind p = Token.DOT then begin
          match peek_kind_at p 1 with
          | Token.UIDENT s2 ->
            ignore (advance p); ignore (advance p);
            mod_parts := s2 :: !mod_parts
          | Token.IDENT s2 ->
            ignore (advance p); ignore (advance p);
            final_name := s2;
            continue_reading := false
          | _ -> continue_reading := false
        end else
          continue_reading := false
      done;
      if !final_name = "" then
        error p "expected Module.typename in type annotation"
      else begin
        let qualified = String.concat "." (List.rev !mod_parts) ^ "." ^ !final_name in
        let args = match ty with
          | Ast.TyTuple parts -> parts
          | _ -> [ty]
        in
        loop (Ast.TyApp (args, qualified))
      end
    | _ -> ty
  in
  loop ty

and parse_ty_tuple p =
  let first = parse_ty_postfix p in
  match peek_kind p with
  | Token.STAR ->
    let parts = ref [first] in
    while peek_kind p = Token.STAR do
      ignore (advance p);
      parts := parse_ty_postfix p :: !parts
    done;
    Ast.TyTuple (List.rev !parts)
  | _ -> first

and parse_ty p =
  let left = parse_ty_tuple p in
  match peek_kind p with
  | Token.ARROW ->
    ignore (advance p);
    let right = parse_ty p in
    (* Check for / eff_annot after the arrow's return type *)
    let eff = if peek_kind p = Token.SLASH then begin
      ignore (advance p);
      if peek_kind p = Token.IDENT "pure" then
        (ignore (advance p); Some Ast.EffAnnotPure)
      else
        Some (Ast.EffAnnotRow (parse_eff_items p))
    end else None in
    Ast.TyArrow (left, right, eff)
  | _ -> left

and is_eff_param_start = function
  (* Type atoms that can start an effect type parameter.
     Excludes UIDENT since that would be another effect label. *)
  | Token.IDENT _ | Token.TYVAR _ | Token.LPAREN | Token.LBRACE -> true
  | _ -> false

and parse_eff_type_params p =
  let params = ref [] in
  while is_eff_param_start (peek_kind p) do
    params := parse_ty_atom p :: !params
  done;
  List.rev !params

and parse_eff_items p =
  let items = ref [] in
  let continue_parsing = ref true in
  while !continue_parsing do
    match peek_kind p with
    | Token.UIDENT _ ->
      let name = expect_uident p in
      let params = parse_eff_type_params p in
      items := Ast.EffLabel (name, params) :: !items;
      if peek_kind p = Token.COMMA then ignore (advance p)
      else continue_parsing := false
    | Token.TYVAR s ->
      ignore (advance p);
      items := Ast.EffVar s :: !items;
      if peek_kind p = Token.COMMA then ignore (advance p)
      else continue_parsing := false
    | _ -> continue_parsing := false
  done;
  List.rev !items

and parse_ty_record_fields p =
  let fields = ref [] in
  let is_open = ref false in
  let first = ref true in
  while peek_kind p <> Token.RBRACE do
    if not !first then expect p Token.SEMICOLON;
    if peek_kind p = Token.RBRACE then () (* trailing semicolon *)
    else if peek_kind p = Token.DOTDOT then begin
      ignore (advance p);
      is_open := true
    end else begin
      first := false;
      let is_mut = peek_kind p = Token.MUT in
      if is_mut then ignore (advance p);
      let name = expect_ident p in
      expect p Token.COLON;
      let ty = parse_ty p in
      fields := (is_mut, name, ty) :: !fields
    end
  done;
  (List.rev !fields, !is_open)

(* ---- Pattern parsing ---- *)

let rec parse_pattern_atom p =
  match peek_kind p with
  | Token.UNDERSCORE -> ignore (advance p); Ast.PatWild
  | Token.INT n -> ignore (advance p); Ast.PatInt n
  | Token.FLOAT f -> ignore (advance p); Ast.PatFloat f
  | Token.TRUE -> ignore (advance p); Ast.PatBool true
  | Token.FALSE -> ignore (advance p); Ast.PatBool false
  | Token.STRING s -> ignore (advance p); Ast.PatString s
  | Token.IDENT s -> ignore (advance p); Ast.PatVar s
  | Token.UIDENT s ->
    ignore (advance p);
    (* Read possible qualified path: Module.Ctor *)
    let ctor_name =
      let path = ref [s] in
      let continue_reading = ref true in
      while !continue_reading do
        if peek_kind p = Token.DOT then begin
          match peek_kind_at p 1 with
          | Token.UIDENT s2 ->
            ignore (advance p); (* DOT *)
            ignore (advance p); (* UIDENT *)
            path := s2 :: !path
          | _ -> continue_reading := false
        end else
          continue_reading := false
      done;
      match !path with
      | [single] -> single
      | rev_path ->
        let parts = List.rev rev_path in
        String.concat "." parts
    in
    (* Check for constructor argument *)
    let arg = if at_pattern_arg_start p then
      Some (parse_pattern_atom p)
    else
      None
    in
    Ast.PatConstruct (ctor_name, arg)
  | Token.LBRACKET ->
    ignore (advance p);
    if peek_kind p = Token.RBRACKET then begin
      ignore (advance p);
      Ast.PatNil
    end else begin
      let pats = parse_pattern_list_elems p in
      expect p Token.RBRACKET;
      (* Desugar [a; b; c] to a :: b :: c :: [] *)
      List.fold_right (fun pat acc -> Ast.PatCons (pat, acc)) pats Ast.PatNil
    end
  | Token.LPAREN ->
    ignore (advance p);
    if peek_kind p = Token.RPAREN then begin
      ignore (advance p);
      Ast.PatUnit
    end else begin
      let pat = parse_pattern p in
      match peek_kind p with
      | Token.COMMA ->
        let pats = ref [pat] in
        while peek_kind p = Token.COMMA do
          ignore (advance p);
          pats := parse_pattern p :: !pats
        done;
        expect p Token.RPAREN;
        Ast.PatTuple (List.rev !pats)
      | Token.COLON ->
        ignore (advance p);
        let ty = parse_ty p in
        expect p Token.RPAREN;
        Ast.PatAnnot (pat, ty)
      | _ ->
        expect p Token.RPAREN;
        pat
    end
  | Token.LBRACE ->
    ignore (advance p);
    let fields = parse_pattern_record_fields p in
    expect p Token.RBRACE;
    Ast.PatRecord fields
  | Token.POLYTAG tag ->
    ignore (advance p);
    if at_pattern_arg_start p then
      Ast.PatPolyVariant (tag, Some (parse_pattern_atom p))
    else
      Ast.PatPolyVariant (tag, None)
  | Token.CARET ->
    ignore (advance p);
    let name = expect_ident p in
    Ast.PatPin name
  | Token.HASH ->
    ignore (advance p);
    (match peek_kind p with
     | Token.LBRACKET ->
       ignore (advance p);
       if peek_kind p = Token.RBRACKET then begin
         ignore (advance p);
         Ast.PatArray []
       end else begin
         let pats = parse_pattern_list_elems p in
         expect p Token.RBRACKET;
         Ast.PatArray pats
       end
     | Token.LBRACE ->
       ignore (advance p);
       if peek_kind p = Token.RBRACE then begin
         ignore (advance p);
         Ast.PatMap []
       end else begin
         let entries = parse_pattern_map_entries p in
         expect p Token.RBRACE;
         Ast.PatMap entries
       end
     | _ -> error p "expected '[' or '{' after '#' in pattern")
  | _ -> error p "expected pattern"

and parse_pattern_cons p =
  let pat = parse_pattern_atom p in
  match peek_kind p with
  | Token.COLONCOLON ->
    ignore (advance p);
    let rest = parse_pattern_cons p in
    Ast.PatCons (pat, rest)
  | _ -> pat

and parse_pattern_as p =
  let pat = parse_pattern_cons p in
  match peek_kind p with
  | Token.IDENT "as" ->
    ignore (advance p);
    let name = expect_ident p in
    Ast.PatAs (pat, name)
  | _ -> pat

and parse_pattern p =
  let pat = ref (parse_pattern_as p) in
  while peek_kind p = Token.PIPE do
    ignore (advance p);
    let next_pat = parse_pattern_as p in
    pat := Ast.PatOr (!pat, next_pat)
  done;
  !pat

and at_pattern_arg_start p =
  match peek_kind p with
  | Token.IDENT "as" -> false  (* 'as' is pattern keyword, not an argument *)
  | Token.INT _ | Token.TRUE | Token.FALSE | Token.STRING _
  | Token.IDENT _ | Token.UIDENT _ | Token.LPAREN | Token.LBRACKET | Token.LBRACE
  | Token.UNDERSCORE | Token.HASH | Token.POLYTAG _ | Token.CARET -> true
  | _ -> false

and parse_pattern_list_elems p =
  let first = parse_pattern p in
  let pats = ref [first] in
  while peek_kind p = Token.SEMICOLON do
    ignore (advance p);
    if peek_kind p <> Token.RBRACKET then
      pats := parse_pattern p :: !pats
  done;
  List.rev !pats

and parse_pattern_record_fields p =
  let fields = ref [] in
  let first = ref true in
  while peek_kind p <> Token.RBRACE do
    if not !first then expect p Token.SEMICOLON;
    if peek_kind p = Token.RBRACE then () (* trailing semicolon *)
    else begin
      first := false;
      if peek_kind p = Token.UNDERSCORE then
        ignore (advance p) (* {x; _} wildcard — just skip *)
      else begin
        let name = expect_ident p in
        let pat =
          if peek_kind p = Token.EQ then begin
            ignore (advance p);
            parse_pattern p
          end else
            Ast.PatVar name
        in
        fields := (name, pat) :: !fields
      end
    end
  done;
  List.rev !fields

and parse_pattern_map_entries p =
  let entries = ref [] in
  let first = ref true in
  while peek_kind p <> Token.RBRACE do
    if not !first then expect p Token.SEMICOLON;
    if peek_kind p = Token.RBRACE then () (* trailing semicolon *)
    else begin
      first := false;
      let key = parse_pattern_atom p in
      expect p Token.COLON;
      let value = parse_pattern p in
      entries := (key, value) :: !entries
    end
  done;
  List.rev !entries

(* ---- Expression parsing (Pratt / precedence climbing) ---- *)

(* Binding power for binary operators *)
let bp_of_binop = function
  | Token.PIPEARROW -> (1, 2)      (* left-assoc, lowest precedence *)
  | Token.PIPEPIPE -> (2, 3)       (* right-assoc *)
  | Token.AMPAMP -> (4, 5)         (* right-assoc *)
  | Token.EQ | Token.NEQ | Token.LT | Token.GT
  | Token.LE | Token.GE -> (6, 7)  (* non-assoc, using left-assoc here *)
  | Token.COLONCOLON -> (9, 8)     (* right-assoc *)
  | Token.PLUS | Token.MINUS -> (10, 11) (* left-assoc *)
  | Token.CARET -> (10, 11)        (* same level as +/- *)
  | Token.STAR | Token.SLASH | Token.MOD -> (12, 13) (* left-assoc *)
  | Token.LOR | Token.LXOR -> (10, 11)         (* same as +/- *)
  | Token.LAND | Token.LSL | Token.LSR -> (12, 13) (* same as *// *)
  | _ -> (-1, -1)

let is_binop = function
  | Token.PLUS | Token.MINUS | Token.STAR | Token.SLASH | Token.MOD
  | Token.EQ | Token.NEQ | Token.LT | Token.GT | Token.LE | Token.GE
  | Token.AMPAMP | Token.PIPEPIPE | Token.CARET | Token.COLONCOLON
  | Token.LAND | Token.LOR | Token.LXOR | Token.LSL | Token.LSR
  | Token.PIPEARROW -> true
  | _ -> false

let binop_of_token = function
  | Token.PLUS -> Ast.Add
  | Token.MINUS -> Ast.Sub
  | Token.STAR -> Ast.Mul
  | Token.SLASH -> Ast.Div
  | Token.MOD -> Ast.Mod
  | Token.EQ -> Ast.Eq
  | Token.NEQ -> Ast.Neq
  | Token.LT -> Ast.Lt
  | Token.GT -> Ast.Gt
  | Token.LE -> Ast.Le
  | Token.GE -> Ast.Ge
  | Token.AMPAMP -> Ast.And
  | Token.PIPEPIPE -> Ast.Or
  | Token.CARET -> Ast.Concat
  | Token.LAND -> Ast.Land
  | Token.LOR -> Ast.Lor
  | Token.LXOR -> Ast.Lxor
  | Token.LSL -> Ast.Lsl
  | Token.LSR -> Ast.Lsr
  | Token.PIPEARROW -> Ast.Pipe
  | _ -> assert false

type fun_param = FPParam of Ast.param | FPPat of string * Ast.pattern * Ast.ty_annot option

(* Convert fun_param list to plain param list + body desugar *)
let resolve_fun_params (fun_params : fun_param list) (body : Ast.expr) : Ast.param list * Ast.expr =
  let body = List.fold_left (fun body fp ->
    match fp with
    | FPParam _ -> body
    | FPPat (name, pat, _) ->
      Ast.EMatch (Ast.EVar name, [(pat, None, body)], true)
  ) body fun_params in
  let params = List.map (fun fp ->
    match fp with
    | FPParam param -> param
    | FPPat (name, _, annot) -> Ast.{ name; annot; is_generated = true }
  ) fun_params in
  (params, body)

let fresh_param_counter = ref 0
let fresh_param_name () =
  incr fresh_param_counter;
  Printf.sprintf "__p%d" !fresh_param_counter

(* Parse a format specifier string and wrap expr in appropriate __fmt_* calls.
   Format: [fill/align][0][width][.precision][type]
   Examples: ".2f", "x", "X", "o", "b", ">10", "<10", "08x", ">10.2f" *)
let parse_format_spec spec expr =
  let len = String.length spec in
  let i = ref 0 in
  (* Parse optional alignment: < or > *)
  let align = if !i < len && (spec.[!i] = '<' || spec.[!i] = '>') then
    let a = spec.[!i] in incr i; Some a
  else None in
  (* Parse optional zero-pad flag *)
  let zero_pad = if !i < len && spec.[!i] = '0' &&
    !i + 1 < len && spec.[!i + 1] >= '1' && spec.[!i + 1] <= '9' then
    (incr i; true)
  else false in
  (* Parse optional width *)
  let width_start = !i in
  while !i < len && spec.[!i] >= '0' && spec.[!i] <= '9' do incr i done;
  let width = if !i > width_start then
    Some (int_of_string (String.sub spec width_start (!i - width_start)))
  else None in
  (* Parse optional .Nf (float precision) *)
  let float_prec =
    if !i < len && spec.[!i] = '.' then begin
      incr i;
      let ps = !i in
      while !i < len && spec.[!i] >= '0' && spec.[!i] <= '9' do incr i done;
      let n = if !i > ps then int_of_string (String.sub spec ps (!i - ps)) else 0 in
      if !i < len && spec.[!i] = 'f' then (incr i; Some n)
      else None
    end else None in
  (* Parse optional type specifier: x X o b *)
  let type_spec = if !i < len then
    (match spec.[!i] with
     | 'x' -> incr i; Some 'x'
     | 'X' -> incr i; Some 'X'
     | 'o' -> incr i; Some 'o'
     | 'b' -> incr i; Some 'b'
     | _ -> None)
  else None in
  (* Build AST bottom-up *)
  (* 1. Type conversion *)
  let inner = match float_prec with
    | Some n -> Ast.EApp (Ast.EApp (Ast.EVar "__fmt_float", Ast.EInt n), expr)
    | None ->
      (match type_spec with
       | Some 'x' -> Ast.EApp (Ast.EVar "__fmt_hex", expr)
       | Some 'X' -> Ast.EApp (Ast.EVar "__fmt_hex_upper", expr)
       | Some 'o' -> Ast.EApp (Ast.EVar "__fmt_oct", expr)
       | Some 'b' -> Ast.EApp (Ast.EVar "__fmt_bin", expr)
       | _ -> Ast.EApp (Ast.EVar "show", expr))
  in
  (* 2. Zero-pad + width *)
  let inner = if zero_pad then
    match width with
    | Some w -> Ast.EApp (Ast.EApp (Ast.EVar "__fmt_zero_pad", Ast.EInt w), inner)
    | None -> inner
  else inner in
  (* 3. Alignment + width (non-zero-pad) *)
  let inner = if not zero_pad then
    match align, width with
    | Some '>', Some w -> Ast.EApp (Ast.EApp (Ast.EVar "__fmt_pad_left", Ast.EInt w), inner)
    | Some '<', Some w -> Ast.EApp (Ast.EApp (Ast.EVar "__fmt_pad_right", Ast.EInt w), inner)
    | _ -> inner
  else inner in
  inner

(* Parse an atom (highest precedence) *)
let rec parse_atom p =
  let loc = (peek p).loc in
  let expr = parse_atom_inner p in
  Ast.ELoc (loc, expr)

and parse_atom_inner p =
  match peek_kind p with
  | Token.INT n -> ignore (advance p); Ast.EInt n
  | Token.FLOAT f -> ignore (advance p); Ast.EFloat f
  | Token.STRING s -> ignore (advance p); Ast.EString s
  | Token.INTERP_STRING parts ->
    ignore (advance p);
    let exprs = List.map (fun (part : Token.interp_part) -> match part with
      | Token.IPLit s -> Ast.EString s
      | Token.IPExpr (src, fmt) ->
        let tokens = Lexer.tokenize src in
        let inner_p = create tokens in
        let inner_expr = parse_expr inner_p in
        (match fmt with
         | None -> Ast.EApp (Ast.EVar "show", inner_expr)
         | Some spec -> parse_format_spec spec inner_expr)
    ) parts in
    (match exprs with
     | [] -> Ast.EString ""
     | [e] -> e
     | first :: rest ->
       List.fold_left (fun acc e -> Ast.EBinop (Ast.Concat, acc, e)) first rest)
  | Token.TRUE -> ignore (advance p); Ast.EBool true
  | Token.FALSE -> ignore (advance p); Ast.EBool false
  | Token.BYTE n -> ignore (advance p); Ast.EByte n
  | Token.RUNE n -> ignore (advance p); Ast.ERune n
  | Token.IDENT s -> ignore (advance p); Ast.EVar s
  | Token.UIDENT s ->
    ignore (advance p);
    (match read_qualified_path p s with
     | `Var qualified -> Ast.EVar qualified
     | `Constructor qualified -> Ast.EConstruct (qualified, None)
     | `LocalOpen mod_path ->
       ignore (advance p); (* consume LPAREN *)
       let inner = parse_expr p in
       expect p Token.RPAREN;
       Ast.ELocalOpen (mod_path, inner))
  | Token.LPAREN ->
    ignore (advance p);
    (* Check for operator-as-variable syntax, e.g. (+) or (mod) *)
    let op_name = token_to_op_name (peek_kind p) in
    (match op_name with
     | Some name when peek_kind_at p 1 = Token.RPAREN ->
       ignore (advance p); (* consume operator *)
       ignore (advance p); (* consume ) *)
       Ast.EVar name
     | _ ->
    if peek_kind p = Token.RPAREN then begin
      ignore (advance p);
      Ast.EUnit
    end else begin
      let expr = parse_expr p in
      match peek_kind p with
      | Token.COMMA ->
        let exprs = ref [expr] in
        while peek_kind p = Token.COMMA do
          ignore (advance p);
          exprs := parse_expr p :: !exprs
        done;
        expect p Token.RPAREN;
        Ast.ETuple (List.rev !exprs)
      | Token.COLON ->
        ignore (advance p);
        let ty = parse_ty p in
        expect p Token.RPAREN;
        Ast.EAnnot (expr, ty)
      | Token.COLONGT ->
        ignore (advance p);
        let ty = parse_ty p in
        expect p Token.RPAREN;
        Ast.ECoerce (expr, ty)
      | _ ->
        expect p Token.RPAREN;
        expr
    end)
  | Token.LBRACE ->
    ignore (advance p);
    if peek_kind p = Token.RBRACE then begin
      ignore (advance p);
      Ast.ERecord []
    end else if (match peek_kind p with
        | Token.IDENT _ ->
          let next = peek_kind_at p 1 in
          next = Token.EQ || next = Token.SEMICOLON || next = Token.RBRACE
        | _ -> false) then begin
      let fields = parse_record_fields p in
      expect p Token.RBRACE;
      Ast.ERecord fields
    end else begin
      let base = parse_expr p in
      expect p Token.WITH;
      let fields = parse_record_fields p in
      expect p Token.RBRACE;
      Ast.ERecordUpdate (base, fields)
    end
  | Token.LBRACKET ->
    ignore (advance p);
    if peek_kind p = Token.RBRACKET then begin
      ignore (advance p);
      Ast.ENil
    end else begin
      let elems = parse_list_elems p in
      expect p Token.RBRACKET;
      Ast.EList elems
    end
  | Token.HASH ->
    ignore (advance p);
    (match peek_kind p with
     | Token.LBRACE ->
       ignore (advance p);
       (match parse_set_or_map p with
        | `Map pairs -> expect p Token.RBRACE; Ast.EMap pairs
        | `Set elems -> expect p Token.RBRACE; Ast.ESet elems
        | `Update expr -> expect p Token.RBRACE; expr)
     | Token.LBRACKET ->
       ignore (advance p);
       if peek_kind p = Token.RBRACKET then begin
         ignore (advance p); Ast.EArray []
       end else begin
         let elems = parse_list_elems p in
         expect p Token.RBRACKET;
         Ast.EArray elems
       end
     | Token.UIDENT name | Token.IDENT name ->
       ignore (advance p);
       (match peek_kind p with
        | Token.LBRACE ->
          ignore (advance p);
          (match parse_set_or_map p with
           | `Map pairs -> expect p Token.RBRACE; Ast.EMapTyped (name, pairs)
           | `Set elems -> expect p Token.RBRACE; Ast.ECollTyped (name, elems)
           | `Update expr -> expect p Token.RBRACE; expr)
        | Token.LBRACKET ->
          ignore (advance p);
          if peek_kind p = Token.RBRACKET then begin
            ignore (advance p); Ast.ECollTyped (name, [])
          end else begin
            let elems = parse_list_elems p in
            expect p Token.RBRACKET;
            Ast.ECollTyped (name, elems)
          end
        | _ -> error p "expected '{' or '[' after type name")
     | _ -> error p "expected '{', '[', or type name after '#'")
  | Token.POLYTAG tag ->
    ignore (advance p);
    if at_atom_start p && not (is_keyword_expr_start p) then
      Ast.EPolyVariant (tag, Some (parse_postfix p))
    else
      Ast.EPolyVariant (tag, None)
  | Token.DO ->
    ignore (advance p);
    let e = parse_expr p in
    expect p Token.END;
    e
  | Token.FN -> parse_fun p
  | Token.IF -> parse_if p
  | Token.LET -> parse_let_expr p
  | Token.MATCH -> parse_match p false
  | Token.PARTIAL -> ignore (advance p); parse_match p true
  | Token.MINUS ->
    ignore (advance p);
    let expr = parse_postfix p in
    Ast.EUnop (Ast.Neg, expr)
  | Token.NOT ->
    ignore (advance p);
    let expr = parse_postfix p in
    Ast.EUnop (Ast.Not, expr)
  | Token.LNOT ->
    ignore (advance p);
    let expr = parse_postfix p in
    Ast.EUnop (Ast.Lnot, expr)
  | Token.PERFORM ->
    ignore (advance p);
    let op_name = expect_ident p in
    let arg = parse_postfix p in
    Ast.EPerform (op_name, arg)
  | Token.HANDLE -> parse_handle_expr p
  | Token.TRY -> parse_try_expr p
  | Token.RESUME ->
    ignore (advance p);
    let k = parse_atom p in
    let v = parse_atom p in
    Ast.EResume (k, v)
  | Token.FOR -> parse_for_expr p
  | Token.BREAK ->
    ignore (advance p);
    if at_atom_start p then
      Ast.EBreak (Some (parse_atom p))
    else
      Ast.EBreak None
  | Token.CONTINUE ->
    ignore (advance p);
    Ast.EContinueLoop
  | Token.RETURN ->
    ignore (advance p);
    Ast.EReturn (parse_expr_bp p 0)
  | Token.DOT ->
    (* .field → fn __r -> __r.field *)
    ignore (advance p);
    let field = expect_ident p in
    let r = Ast.{ name = "__r"; annot = None; is_generated = false } in
    Ast.EFun (r, Ast.EField (Ast.EVar "__r", field))
  | _ -> error p "expected expression"

and at_atom_start p =
  match peek_kind p with
  | Token.INT _ | Token.FLOAT _ | Token.STRING _ | Token.INTERP_STRING _
  | Token.BYTE _ | Token.RUNE _
  | Token.TRUE | Token.FALSE
  | Token.IDENT _ | Token.UIDENT _ | Token.LPAREN | Token.LBRACE
  | Token.LBRACKET | Token.HASH | Token.DOT | Token.POLYTAG _ -> true
  | _ -> false

(* Check whether the current token is adjacent to the previous (no whitespace) *)
and is_adjacent p =
  p.pos > 0 &&
  let prev = p.tokens.(p.pos - 1) in
  let cur = peek p in
  prev.end_offset = cur.loc.offset

(* Parse postfix field access and indexing (binds tighter than application) *)
and parse_postfix p =
  let expr = parse_atom p in
  let rec loop expr =
    match peek_kind p with
    | Token.DOT when is_adjacent p ->
      ignore (advance p);
      if peek_kind p = Token.LBRACKET then begin
        ignore (advance p);
        let idx = parse_expr p in
        expect p Token.RBRACKET;
        loop (Ast.EIndex (expr, idx))
      end else begin
        let field = expect_ident p in
        loop (Ast.EField (expr, field))
      end
    | _ -> expr
  in
  loop expr

(* Parse function application (left-to-right) *)
and parse_app p =
  let fn = parse_postfix p in
  let rec loop expr =
    match peek_kind p with
    | _ when at_atom_start p && not (is_keyword_expr_start p) ->
      let arg = parse_postfix p in
      (* If applying to a nullary constructor, treat as constructor application *)
      (match expr with
       | Ast.EConstruct (name, None)
       | Ast.ELoc (_, Ast.EConstruct (name, None)) ->
         loop (Ast.EConstruct (name, Some arg))
       | _ ->
         loop (Ast.EApp (expr, arg)))
    | _ -> expr
  in
  loop fn

and is_keyword_expr_start p =
  match peek_kind p with
  | Token.FN | Token.IF | Token.LET | Token.MATCH
  | Token.PERFORM | Token.HANDLE | Token.TRY | Token.RESUME
  | Token.FOR | Token.BREAK | Token.CONTINUE | Token.RETURN -> true
  | _ -> false

(* Pratt expression parser *)
and parse_expr_bp p min_bp =
  let lhs = parse_app p in
  let rec loop lhs =
    let kind = peek_kind p in
    if is_binop kind then begin
      let (l_bp, r_bp) = bp_of_binop kind in
      if l_bp < min_bp then lhs
      else begin
        ignore (advance p);
        if kind = Token.COLONCOLON then begin
          let rhs = parse_expr_bp p r_bp in
          loop (Ast.ECons (lhs, rhs))
        end else begin
          let rhs = parse_expr_bp p r_bp in
          loop (Ast.EBinop (binop_of_token kind, lhs, rhs))
        end
      end
    end else
      lhs
  in
  loop lhs

(* Top-level expression parser with sequence handling *)
and parse_expr_no_seq p =
  (* Like parse_expr but does NOT consume ; sequences.
     Handles := assignments but stops before ;.
     Used for else branches so that ; ends the if-expression. *)
  let e = parse_expr_bp p 0 in
  match peek_kind p with
  | Token.COLONEQUAL ->
    ignore (advance p);
    let rhs = parse_expr_bp p 0 in
    let e_inner = match e with Ast.ELoc (_, inner) -> inner | e -> e in
    (match e_inner with
     | Ast.EVar name -> Ast.EAssign (name, rhs)
     | Ast.EField (obj, field) -> Ast.EFieldAssign (obj, field, rhs)
     | _ -> error p "left side of := must be a variable or field access")
  | _ -> e

and parse_expr p =
  let e = parse_expr_bp p 0 in
  match peek_kind p with
  | Token.COLONEQUAL ->
    ignore (advance p);
    let rhs = parse_expr_bp p 0 in
    let e_inner = match e with Ast.ELoc (_, inner) -> inner | e -> e in
    let assign = match e_inner with
     | Ast.EVar name -> Ast.EAssign (name, rhs)
     | Ast.EField (obj, field) -> Ast.EFieldAssign (obj, field, rhs)
     | _ -> error p "left side of := must be a variable or field access"
    in
    (match peek_kind p with
     | Token.SEMICOLON when peek_next_kind p <> Some Token.SEMICOLON ->
       ignore (advance p);
       if at_expr_start p then
         Ast.ESeq (assign, parse_expr p)
       else assign
     | _ -> assign)
  | Token.SEMICOLON when peek_next_kind p <> Some Token.SEMICOLON ->
    ignore (advance p);
    if at_expr_start p then begin
      let e2 = parse_expr p in
      Ast.ESeq (e, e2)
    end else
      e
  | _ -> e

and peek_next_kind p =
  if p.pos + 1 < Array.length p.tokens then
    Some p.tokens.(p.pos + 1).kind
  else
    None

(* Parse a fn parameter that may be a destructuring pattern *)
and parse_fun_param p =
  match peek_kind p with
  | Token.LPAREN ->
    ignore (advance p);
    if peek_kind p = Token.RPAREN then begin
      (* () — unit param *)
      ignore (advance p);
      FPParam Ast.{ name = "_"; annot = Some (Ast.TyName "unit"); is_generated = false }
    end else begin
      (* Parse inner pattern (handles constructors, records, etc.) *)
      let pat = parse_pattern p in
      (* Check for tuple: (pat, pat, ...) *)
      let pat = if peek_kind p = Token.COMMA then begin
        let pats = ref [pat] in
        while peek_kind p = Token.COMMA do
          ignore (advance p);
          pats := parse_pattern p :: !pats
        done;
        Ast.PatTuple (List.rev !pats)
      end else pat in
      let annot = if peek_kind p = Token.COLON then begin
        ignore (advance p); Some (parse_ty p)
      end else None in
      expect p Token.RPAREN;
      match pat, annot with
      | Ast.PatVar name, _ -> FPParam Ast.{ name; annot; is_generated = false }
      | _, _ ->
        let pname = fresh_param_name () in
        FPPat (pname, pat, annot)
    end
  | Token.LBRACE ->
    (* Record pattern *)
    let pat = parse_pattern_atom p in
    let pname = fresh_param_name () in
    FPPat (pname, pat, None)
  | Token.IDENT s ->
    ignore (advance p);
    FPParam Ast.{ name = s; annot = None; is_generated = false }
  | Token.UNDERSCORE ->
    ignore (advance p);
    FPParam Ast.{ name = "_"; annot = None; is_generated = false }
  | _ -> error p "expected parameter"

(* fn (x: int) (y: int) -> body
   fn | pat1 -> e1 | pat2 -> e2   (lambda match) *)
and parse_fun p =
  expect p Token.FN;
  if peek_kind p = Token.PIPE then begin
    (* Lambda match: fn | pat1 -> e1 | pat2 -> e2 *)
    ignore (advance p);
    let arms = ref [] in
    let continue = ref true in
    while !continue do
      let pat = parse_pattern p in
      let guard = if peek_kind p = Token.WHEN then begin
        ignore (advance p);
        Some (parse_expr p)
      end else None in
      expect p Token.ARROW;
      let body = parse_expr p in
      arms := (pat, guard, body) :: !arms;
      if peek_kind p = Token.PIPE then
        ignore (advance p)
      else
        continue := false
    done;
    let param = Ast.{ name = "__x"; annot = None; is_generated = false } in
    Ast.EFun (param, Ast.EMatch (Ast.EVar "__x", List.rev !arms, false))
  end else begin
    (* Regular lambda: fn x y -> body *)
    let fun_params = ref [] in
    while peek_kind p <> Token.ARROW do
      fun_params := parse_fun_param p :: !fun_params
    done;
    (* fn -> body  is treated as  fn () -> body *)
    if !fun_params = [] then
      fun_params := [FPParam Ast.{ name = "_"; annot = Some (Ast.TyName "unit"); is_generated = false }];
    expect p Token.ARROW;
    let body = parse_expr p in
    (* Wrap pattern params with match desugaring *)
    let body = List.fold_left (fun body fp ->
      match fp with
      | FPParam _ -> body
      | FPPat (name, pat, _) ->
        Ast.EMatch (Ast.EVar name, [(pat, None, body)], true)
    ) body !fun_params in
    (* Wrap all params as EFun *)
    let params = List.map (fun fp ->
      match fp with
      | FPParam param -> param
      | FPPat (name, _, annot) -> Ast.{ name; annot; is_generated = false }
    ) !fun_params in
    List.fold_left (fun body param ->
      Ast.EFun (param, body)
    ) body params
  end

and parse_param p =
  (* parse_param now supports destructuring patterns, delegating to parse_fun_param *)
  parse_fun_param p

and parse_if p =
  expect p Token.IF;
  let cond = parse_expr p in
  expect p Token.DO;
  let then_e = parse_expr p in
  match peek_kind p with
  | Token.ELSE ->
    ignore (advance p);
    let else_e = parse_expr_no_seq p in
    Ast.EIf (cond, then_e, else_e)
  | Token.END ->
    ignore (advance p);
    Ast.EIf (cond, then_e, Ast.EUnit)
  | _ -> error p "expected 'else' or 'end' after if body"

and parse_type_params p =
  (* Parse (type 'a 'b ...) for locally abstract types in let rec *)
  if peek_kind p = Token.LPAREN && peek_kind_at p 1 = Token.TYPE then begin
    ignore (advance p); (* consume ( *)
    ignore (advance p); (* consume type *)
    let tyvars = ref [] in
    while (match peek_kind p with Token.TYVAR _ -> true | _ -> false) do
      match advance p with
      | { kind = Token.TYVAR s; _ } -> tyvars := s :: !tyvars
      | _ -> ()
    done;
    if !tyvars = [] then error p "expected type variable after 'type'";
    expect p Token.RPAREN;
    List.rev !tyvars
  end else
    []

and parse_let_expr p =
  expect p Token.LET;
  let is_rec = peek_kind p = Token.REC in
  if is_rec then ignore (advance p);
  let is_mut = peek_kind p = Token.MUT in
  if is_mut then ignore (advance p);
  if is_rec && is_mut then error p "let rec mut is not supported";
  if (not is_rec) && (not is_mut) && is_destruct_start p then begin
    let pat = parse_pattern p in
    expect p Token.EQ;
    let e1 = parse_expr p in
    expect p Token.IN;
    let e2 = parse_expr p in
    Ast.EMatch (e1, [(pat, None, e2)], true)
  end else begin
    let type_params = if is_rec then parse_type_params p else [] in
    let name = expect_ident p in
    if is_mut then begin
      let annot = if peek_kind p = Token.COLON then begin
        ignore (advance p);
        Some (parse_ty p)
      end else None in
      expect p Token.EQ;
      let body = parse_expr p in
      let body = match annot with Some ty -> Ast.EAnnot (body, ty) | None -> body in
      expect p Token.IN;
      let rest = parse_expr p in
      Ast.ELetMut (name, body, rest)
    end else begin
      let params = ref [] in
      while peek_kind p <> Token.EQ && peek_kind p <> Token.COLON do
        params := parse_param p :: !params
      done;
      let ret_annot = if peek_kind p = Token.COLON then begin
        ignore (advance p);
        let ty = parse_ty p in
        if peek_kind p = Token.SLASH then begin
          ignore (advance p);
          let eff = if peek_kind p = Token.IDENT "pure" then
            (ignore (advance p); Ast.EffAnnotPure)
          else
            Ast.EffAnnotRow (parse_eff_items p)
          in
          Some (Ast.TyWithEffect (ty, eff))
        end else
          Some ty
      end else None in
      expect p Token.EQ;
      let body = parse_expr p in
      (* Wrap params around body *)
      let full_body = wrap_params_expr (List.rev !params) ret_annot body in
      if is_rec && peek_kind p = Token.AND then begin
        (* Mutual recursion: let rec f = ... and g = ... in body *)
        let bindings = ref [(name, type_params, full_body)] in
        while peek_kind p = Token.AND do
          ignore (advance p);
          let and_name = expect_ident p in
          let and_type_params = parse_type_params p in
          let and_params = ref [] in
          while peek_kind p <> Token.EQ && peek_kind p <> Token.COLON do
            and_params := parse_param p :: !and_params
          done;
          let and_ret_annot = if peek_kind p = Token.COLON then begin
            ignore (advance p);
            let ty = parse_ty p in
            if peek_kind p = Token.SLASH then begin
              ignore (advance p);
              let eff = if peek_kind p = Token.IDENT "pure" then
                (ignore (advance p); Ast.EffAnnotPure)
              else
                Ast.EffAnnotRow (parse_eff_items p)
              in
              Some (Ast.TyWithEffect (ty, eff))
            end else
              Some ty
          end else None in
          expect p Token.EQ;
          let and_body = parse_expr p in
          let and_full_body = wrap_params_expr (List.rev !and_params) and_ret_annot and_body in
          bindings := (and_name, and_type_params, and_full_body) :: !bindings
        done;
        expect p Token.IN;
        let rest = parse_expr p in
        Ast.ELetRecAnd (List.rev !bindings, rest)
      end else begin
        expect p Token.IN;
        let rest = parse_expr p in
        if is_rec then Ast.ELetRec (name, type_params, full_body, rest)
        else Ast.ELet (name, full_body, rest)
      end
    end
  end

and is_destruct_start p =
  match peek_kind p with
  | Token.LPAREN | Token.LBRACE | Token.LBRACKET | Token.UIDENT _ -> true
  | Token.IDENT _ when peek_kind_at p 1 = Token.COLONCOLON -> true
  | _ -> false

and wrap_params_expr fun_params ret_annot body =
  (* Apply match desugaring for destructuring pattern params *)
  let body = List.fold_left (fun body fp ->
    match fp with
    | FPParam _ -> body
    | FPPat (name, pat, _) ->
      Ast.EMatch (Ast.EVar name, [(pat, None, body)], true)
  ) body fun_params in
  (* Extract Ast.param from each fun_param *)
  let params = List.map (fun fp ->
    match fp with
    | FPParam param -> param
    | FPPat (name, _, annot) -> Ast.{ name; annot; is_generated = true }
  ) fun_params in
  match ret_annot with
  | Some (Ast.TyWithEffect (ret_ty, eff_annot)) when params <> [] ->
    (* Effect annotation on return type: build full arrow type annotation *)
    let body = Ast.EAnnot (body, ret_ty) in
    let full_fun = List.fold_right (fun param body ->
      Ast.EFun (param, body)
    ) params body in
    let rec build_arrow = function
      | [] -> ret_ty
      | [p] ->
        let pty = match p.Ast.annot with
          | Some t -> t
          | None -> Ast.TyVar ("'_eff_p_" ^ p.Ast.name)
        in
        Ast.TyArrow (pty, ret_ty, Some eff_annot)
      | p :: rest ->
        let pty = match p.Ast.annot with
          | Some t -> t
          | None -> Ast.TyVar ("'_eff_p_" ^ p.Ast.name)
        in
        Ast.TyArrow (pty, build_arrow rest, None)
    in
    Ast.EAnnot (full_fun, build_arrow params)
  | _ ->
    let body = match ret_annot with
      | Some ty -> Ast.EAnnot (body, ty)
      | None -> body
    in
    List.fold_right (fun param body ->
      Ast.EFun (param, body)
    ) params body

and parse_match p partial =
  expect p Token.MATCH;
  let scrut = parse_expr p in
  expect p Token.WITH;
  (* Optional leading pipe *)
  if peek_kind p = Token.PIPE then ignore (advance p);
  let arms = ref [] in
  let continue = ref true in
  while !continue do
    let pat = parse_pattern p in
    (* Optional guard *)
    let guard = if peek_kind p = Token.WHEN then begin
      ignore (advance p);
      Some (parse_expr p)
    end else None in
    expect p Token.ARROW;
    let body = parse_expr p in
    arms := (pat, guard, body) :: !arms;
    if peek_kind p = Token.PIPE then
      ignore (advance p)
    else
      continue := false
  done;
  Ast.EMatch (scrut, List.rev !arms, partial)

and parse_handle_expr p =
  expect p Token.HANDLE;
  let body = parse_expr p in
  expect p Token.WITH;
  (* Optional leading pipe *)
  if peek_kind p = Token.PIPE then ignore (advance p);
  let arms = ref [] in
  let continue_parsing = ref true in
  while !continue_parsing do
    match peek_kind p with
    | Token.RETURN ->
      ignore (advance p);
      let name = expect_ident p in
      expect p Token.ARROW;
      let handler_body = parse_expr p in
      arms := Ast.HReturn (name, handler_body) :: !arms;
      if peek_kind p = Token.PIPE then ignore (advance p)
      else continue_parsing := false
    | Token.IDENT s ->
      ignore (advance p);
      let arg_name = match peek_kind p with
        | Token.LPAREN when peek_kind_at p 1 = Token.RPAREN ->
          ignore (advance p); ignore (advance p);
          "_"
        | _ -> expect_ident p
      in
      let k_name = expect_ident p in
      expect p Token.ARROW;
      let handler_body = parse_expr p in
      arms := Ast.HOp (s, arg_name, k_name, handler_body) :: !arms;
      if peek_kind p = Token.PIPE then ignore (advance p)
      else continue_parsing := false
    | _ -> continue_parsing := false
  done;
  Ast.EHandle (body, List.rev !arms)

and parse_try_expr p =
  expect p Token.TRY;
  let body = parse_expr p in
  expect p Token.WITH;
  (* Optional leading pipe *)
  if peek_kind p = Token.PIPE then ignore (advance p);
  let arms = ref [Ast.HReturn ("__x", Ast.EVar "__x")] in
  let continue_parsing = ref true in
  while !continue_parsing do
    match peek_kind p with
    | Token.IDENT s ->
      ignore (advance p);
      let arg_name = match peek_kind p with
        | Token.LPAREN when peek_kind_at p 1 = Token.RPAREN ->
          ignore (advance p); ignore (advance p);
          "_"
        | _ -> expect_ident p
      in
      expect p Token.ARROW;
      let handler_body = parse_expr p in
      arms := Ast.HOp (s, arg_name, "__k", handler_body) :: !arms;
      if peek_kind p = Token.PIPE then ignore (advance p)
      else continue_parsing := false
    | _ -> continue_parsing := false
  done;
  Ast.EHandle (body, List.rev !arms)

and lookahead_in_after_balanced p =
  (* Check if balanced brackets at current position are followed by IN.
     Used to detect for-in with pattern: for (x, y) in coll do ... end *)
  let depth = ref 1 in
  let i = ref 1 in
  let hit_eof = ref false in
  while !depth > 0 && not !hit_eof do
    (match peek_kind_at p !i with
     | Token.LPAREN | Token.LBRACE | Token.LBRACKET -> incr depth
     | Token.RPAREN | Token.RBRACE | Token.RBRACKET -> decr depth
     | Token.EOF -> hit_eof := true
     | _ -> ());
    incr i
  done;
  not !hit_eof && !depth = 0 && peek_kind_at p !i = Token.IN

and pat_to_name_and_wrap pat =
  match pat with
  | Ast.PatVar name -> (name, fun body -> body)
  | Ast.PatWild -> ("_", fun body -> body)
  | _ ->
    let pname = fresh_param_name () in
    (pname, fun body -> Ast.EMatch (Ast.EVar pname, [(pat, None, body)], true))

and parse_for_in_rest p elem_name elem_wrap coll =
  match peek_kind p with
  | Token.WITH ->
    ignore (advance p);
    (* Parse accumulator: ident or pattern *)
    let acc_pat = match peek_kind p with
      | Token.LPAREN | Token.LBRACE | Token.LBRACKET ->
        parse_pattern_atom p
      | _ ->
        let name = expect_ident p in
        Ast.PatVar name
    in
    let (acc_name, acc_wrap) = pat_to_name_and_wrap acc_pat in
    expect p Token.EQ;
    let init = parse_expr_bp p 0 in
    expect p Token.DO;
    let body = parse_expr p in
    expect p Token.END;
    Ast.EForFold (elem_name, coll, acc_name, init, elem_wrap (acc_wrap body))
  | Token.DO ->
    ignore (advance p);
    let body = parse_expr p in
    expect p Token.END;
    Ast.EFor (elem_name, coll, elem_wrap body)
  | _ -> error p "expected 'do' or 'with' after for collection"

and parse_for_expr p =
  expect p Token.FOR;
  match peek_kind p with
  | Token.DO ->
    (* for do body end — infinite loop *)
    ignore (advance p);
    let body = parse_expr p in
    expect p Token.END;
    Ast.EWhile (Ast.EBool true, body)
  | Token.LET ->
    (* for let pat = expr do body end — while-let *)
    ignore (advance p);
    let pat = parse_pattern p in
    expect p Token.EQ;
    let scrutinee = parse_expr_bp p 0 in
    expect p Token.DO;
    let body = parse_expr p in
    expect p Token.END;
    Ast.EWhileLet (pat, scrutinee, body)
  | (Token.IDENT _ | Token.UNDERSCORE) when peek_kind_at p 1 = Token.IN ->
    (* for x in coll [with acc = init] do body end *)
    let var_name = if peek_kind p = Token.UNDERSCORE
      then (ignore (advance p); "_")
      else expect_ident p in
    expect p Token.IN;
    let coll = parse_expr_bp p 0 in
    parse_for_in_rest p var_name (fun body -> body) coll
  | (Token.LPAREN | Token.LBRACE | Token.LBRACKET) when lookahead_in_after_balanced p ->
    (* for (pattern) in coll [with acc = init] do body end *)
    let pat = parse_pattern_atom p in
    expect p Token.IN;
    let coll = parse_expr_bp p 0 in
    let (elem_name, elem_wrap) = pat_to_name_and_wrap pat in
    parse_for_in_rest p elem_name elem_wrap coll
  | _ ->
    (* for cond do body end — while loop *)
    let cond = parse_expr_bp p 0 in
    expect p Token.DO;
    let body = parse_expr p in
    expect p Token.END;
    Ast.EWhile (cond, body)

and parse_record_fields p =
  let fields = ref [] in
  let first = ref true in
  while peek_kind p <> Token.RBRACE do
    if not !first then expect p Token.SEMICOLON;
    if peek_kind p = Token.RBRACE then () (* trailing semicolon *)
    else begin
      first := false;
      let name = expect_ident p in
      let expr =
        if peek_kind p = Token.EQ then begin
          ignore (advance p);
          parse_expr_bp p 0
        end else
          Ast.EVar name
      in
      fields := (name, expr) :: !fields
    end
  done;
  List.rev !fields

and parse_map_pairs p =
  if peek_kind p = Token.RBRACE then []
  else begin
    let pairs = ref [] in
    let first = ref true in
    while peek_kind p <> Token.RBRACE do
      if not !first then expect p Token.SEMICOLON;
      first := false;
      let key = parse_expr_bp p 0 in
      expect p Token.COLON;
      let value = parse_expr_bp p 0 in
      pairs := (key, value) :: !pairs
    done;
    List.rev !pairs
  end

(* Parse brace contents, disambiguating set #{e; e} from map #{k:v; k:v} *)
and parse_set_or_map p =
  if peek_kind p = Token.RBRACE then
    `Map []  (* empty #{} stays as map for backwards compat *)
  else begin
    let first_expr = parse_expr_bp p 0 in
    if peek_kind p = Token.COLON then begin
      (* It's a map: first_expr is the first key *)
      ignore (advance p);
      let first_val = parse_expr_bp p 0 in
      let pairs = ref [(first_expr, first_val)] in
      while peek_kind p = Token.SEMICOLON do
        ignore (advance p);
        if peek_kind p <> Token.RBRACE then begin
          let key = parse_expr_bp p 0 in
          expect p Token.COLON;
          let value = parse_expr_bp p 0 in
          pairs := (key, value) :: !pairs
        end
      done;
      `Map (List.rev !pairs)
    end else if peek_kind p = Token.WITH then begin
      (* Map update: #{ base with k: v; k2: v2 } *)
      ignore (advance p);
      let pairs = ref [] in
      let continue = ref true in
      while !continue do
        let key = parse_expr_bp p 0 in
        expect p Token.COLON;
        let value = parse_expr_bp p 0 in
        pairs := (key, value) :: !pairs;
        if peek_kind p = Token.SEMICOLON then
          (ignore (advance p);
           if peek_kind p = Token.RBRACE then continue := false)
        else
          continue := false
      done;
      (* Desugar to nested set calls: set k2 v2 (set k1 v1 base) *)
      let result = List.fold_left (fun acc (k, v) ->
        Ast.EApp (Ast.EApp (Ast.EApp (Ast.EVar "set", k), v), acc)
      ) first_expr (List.rev !pairs) in
      `Update result
    end else begin
      (* It's a set: first_expr is the first element *)
      let elems = ref [first_expr] in
      while peek_kind p = Token.SEMICOLON do
        ignore (advance p);
        if peek_kind p <> Token.RBRACE then
          elems := parse_expr_bp p 0 :: !elems
      done;
      `Set (List.rev !elems)
    end
  end

and parse_list_elems p =
  let first = parse_expr_bp p 0 in
  let elems = ref [first] in
  while peek_kind p = Token.SEMICOLON do
    ignore (advance p);
    if peek_kind p <> Token.RBRACKET then
      elems := parse_expr_bp p 0 :: !elems
  done;
  List.rev !elems

(* ---- Top-level declarations ---- *)

let is_tyvar_token = function
  | Token.TYVAR _ -> true
  | _ -> false

(* Decompose a GADT constructor signature into (arg_type, return_type).
   e.g. "int -> int expr" becomes (Some (TyName "int"), TyApp ([TyName "int"], "expr"))
        "bool expr * int expr -> int expr" becomes (Some (TyTuple [...]), TyApp ([...], "expr"))
        "('a, 'a) eq" becomes (None, TyApp ([TyVar "a"; TyVar "a"], "eq")) *)
let decompose_gadt_sig ty =
  let rec collect_args = function
    | Ast.TyArrow (arg, rest, _) ->
      let (more_args, ret) = collect_args rest in
      (arg :: more_args, ret)
    | other -> ([], other)
  in
  match collect_args ty with
  | ([], ret) -> (None, ret)
  | ([arg], ret) -> (Some arg, ret)
  | (args, ret) -> (Some (Ast.TyTuple args), ret)

let parse_type_decl p =
  expect p Token.TYPE;
  (* Parse optional type parameters *)
  let type_params = match peek_kind p with
    | Token.TYVAR s ->
      ignore (advance p);
      [s]
    | Token.LPAREN when is_tyvar_token (peek_kind_at p 1) ->
      ignore (advance p);
      let params = ref [] in
      let first = ref true in
      while peek_kind p <> Token.RPAREN do
        if not !first then expect p Token.COMMA;
        first := false;
        (match peek_kind p with
         | Token.TYVAR s -> ignore (advance p); params := s :: !params
         | _ -> error p "expected type variable in type parameter list")
      done;
      expect p Token.RPAREN;
      List.rev !params
    | _ -> []
  in
  let name = expect_ident p in
  expect p Token.EQ;
  let def = match peek_kind p with
  | Token.LBRACE ->
    ignore (advance p);
    let (fields, _is_open) = parse_ty_record_fields p in
    expect p Token.RBRACE;
    Ast.TDRecord fields
  | Token.PIPE | Token.UIDENT _ ->
    if peek_kind p = Token.PIPE then ignore (advance p);
    let ctors = ref [] in
    let continue = ref true in
    while !continue do
      let ctor_name = expect_uident p in
      if peek_kind p = Token.COLON then begin
        (* GADT syntax: Ctor : type -> ... -> ret_type *)
        ignore (advance p);
        let full_sig = parse_ty p in
        let (arg_ty, ret_ty) = decompose_gadt_sig full_sig in
        ctors := (ctor_name, arg_ty, Some ret_ty) :: !ctors
      end else begin
        let arg = if peek_kind p = Token.OF then begin
          ignore (advance p);
          Some (parse_ty p)
        end else None in
        ctors := (ctor_name, arg, None) :: !ctors
      end;
      if peek_kind p = Token.PIPE then
        ignore (advance p)
      else
        continue := false
    done;
    Ast.TDVariant (List.rev !ctors)
  | _ ->
    let rhs = parse_ty p in
    Ast.TDAlias rhs
  in
  let deriving = if peek_kind p = Token.DERIVING then begin
    ignore (advance p);
    let classes = ref [] in
    let continue = ref true in
    while !continue && (match peek_kind p with Token.UIDENT _ -> true | _ -> false) do
      let cls = expect_uident p in
      classes := cls :: !classes;
      if peek_kind p = Token.COMMA then ignore (advance p)
      else continue := false
    done;
    List.rev !classes
  end else [] in
  if peek_kind p <> Token.AND then
    Ast.DType (type_params, name, def, deriving)
  else begin
    (* Mutual type recursion: type ... and ... *)
    let defs = ref [(type_params, name, def, deriving)] in
    while peek_kind p = Token.AND do
      ignore (advance p);
      let and_type_params = match peek_kind p with
        | Token.TYVAR s ->
          ignore (advance p);
          [s]
        | Token.LPAREN when is_tyvar_token (peek_kind_at p 1) ->
          ignore (advance p);
          let params = ref [] in
          let first = ref true in
          while peek_kind p <> Token.RPAREN do
            if not !first then expect p Token.COMMA;
            first := false;
            (match peek_kind p with
             | Token.TYVAR s -> ignore (advance p); params := s :: !params
             | _ -> error p "expected type variable in type parameter list")
          done;
          expect p Token.RPAREN;
          List.rev !params
        | _ -> []
      in
      let and_name = expect_ident p in
      expect p Token.EQ;
      let and_def = match peek_kind p with
      | Token.LBRACE ->
        ignore (advance p);
        let (fields, _is_open) = parse_ty_record_fields p in
        expect p Token.RBRACE;
        Ast.TDRecord fields
      | Token.PIPE | Token.UIDENT _ ->
        if peek_kind p = Token.PIPE then ignore (advance p);
        let ctors = ref [] in
        let cont = ref true in
        while !cont do
          let ctor_name = expect_uident p in
          if peek_kind p = Token.COLON then begin
            ignore (advance p);
            let full_sig = parse_ty p in
            let (arg_ty, ret_ty) = decompose_gadt_sig full_sig in
            ctors := (ctor_name, arg_ty, Some ret_ty) :: !ctors
          end else begin
            let arg = if peek_kind p = Token.OF then begin
              ignore (advance p);
              Some (parse_ty p)
            end else None in
            ctors := (ctor_name, arg, None) :: !ctors
          end;
          if peek_kind p = Token.PIPE then
            ignore (advance p)
          else
            cont := false
        done;
        Ast.TDVariant (List.rev !ctors)
      | _ ->
        let rhs = parse_ty p in
        Ast.TDAlias rhs
      in
      let and_deriving = if peek_kind p = Token.DERIVING then begin
        ignore (advance p);
        let classes = ref [] in
        let cont = ref true in
        while !cont && (match peek_kind p with Token.UIDENT _ -> true | _ -> false) do
          let cls = expect_uident p in
          classes := cls :: !classes;
          if peek_kind p = Token.COMMA then ignore (advance p)
          else cont := false
        done;
        List.rev !classes
      end else [] in
      defs := (and_type_params, and_name, and_def, and_deriving) :: !defs
    done;
    Ast.DTypeAnd (List.rev !defs)
  end

let rec extract_pat_vars = function
  | Ast.PatVar name -> [name]
  | Ast.PatWild | Ast.PatInt _ | Ast.PatFloat _ | Ast.PatBool _
  | Ast.PatString _ | Ast.PatUnit | Ast.PatNil -> []
  | Ast.PatTuple pats -> List.concat_map extract_pat_vars pats
  | Ast.PatCons (hd, tl) -> extract_pat_vars hd @ extract_pat_vars tl
  | Ast.PatConstruct (_, None) -> []
  | Ast.PatConstruct (_, Some p) -> extract_pat_vars p
  | Ast.PatRecord fields -> List.concat_map (fun (_, p) -> extract_pat_vars p) fields
  | Ast.PatAs (inner, name) -> name :: extract_pat_vars inner
  | Ast.PatOr (p1, _) -> extract_pat_vars p1
  | Ast.PatArray pats -> List.concat_map extract_pat_vars pats
  | Ast.PatMap entries -> List.concat_map (fun (k, v) -> extract_pat_vars k @ extract_pat_vars v) entries
  | Ast.PatPolyVariant (_, None) -> []
  | Ast.PatPolyVariant (_, Some p) -> extract_pat_vars p
  | Ast.PatPin _ -> []
  | Ast.PatAnnot (p, _) -> extract_pat_vars p

(* Parse: where ClassName 'tyvar ['tyvar2 ...], ClassName2 'tyvar3, ... *)
and parse_constraints p =
  if peek_kind p <> Token.WHERE then []
  else begin
    ignore (advance p);
    let constraints = ref [] in
    let continue = ref true in
    while !continue do
      (match peek_kind p with
       | Token.UIDENT class_name_start ->
         ignore (advance p);
         let class_name =
           if peek_kind p = Token.DOT then begin
             ignore (advance p);
             match peek_kind p with
             | Token.UIDENT s2 -> ignore (advance p); class_name_start ^ "." ^ s2
             | _ -> error p "expected class name after dot"
           end else class_name_start
         in
         let tyvars = ref [] in
         while (match peek_kind p with Token.TYVAR _ -> true | _ -> false) do
           match (advance p).kind with
           | Token.TYVAR s -> tyvars := s :: !tyvars
           | _ -> ()
         done;
         if !tyvars = [] then error p "constraint requires at least one type variable";
         constraints := (class_name, List.rev !tyvars) :: !constraints;
         if peek_kind p = Token.COMMA then ignore (advance p)
         else continue := false
       | _ -> continue := false)
    done;
    List.rev !constraints
  end

let parse_let_decl p =
  expect p Token.LET;
  let is_rec = peek_kind p = Token.REC in
  if is_rec then ignore (advance p);
  let is_mut = peek_kind p = Token.MUT in
  if is_mut then ignore (advance p);
  if is_rec && is_mut then error p "let rec mut is not supported";
  if (not is_rec) && (not is_mut) && is_destruct_start p then begin
    let pat = parse_pattern p in
    expect p Token.EQ;
    let body = parse_expr p in
    if peek_kind p = Token.IN then begin
      ignore (advance p);
      let rest = parse_expr p in
      [Ast.DExpr (Ast.EMatch (body, [(pat, None, rest)], true))]
    end else begin
      (* Top-level destructuring: bind temp, extract each variable *)
      let tmp = "__destruct" in
      let vars = extract_pat_vars pat in
      let decls = [Ast.DLet (tmp, [], None, [], body)] in
      decls @ List.map (fun v ->
        Ast.DLet (v, [], None, [],
          Ast.EMatch (Ast.EVar tmp, [(pat, None, Ast.EVar v)], true))
      ) vars
    end
  end else if is_mut then begin
    let name = expect_ident p in
    let annot = if peek_kind p = Token.COLON then begin
      ignore (advance p);
      Some (parse_ty p)
    end else None in
    expect p Token.EQ;
    let body = parse_expr p in
    let body = match annot with Some ty -> Ast.EAnnot (body, ty) | None -> body in
    if peek_kind p = Token.IN then begin
      ignore (advance p);
      let rest = parse_expr p in
      [Ast.DExpr (Ast.ELetMut (name, body, rest))]
    end else
      [Ast.DLetMut (name, body)]
  end else begin
    let type_params = if is_rec then parse_type_params p else [] in
    let name = expect_ident p in
    let fun_params = ref [] in
    while peek_kind p <> Token.EQ && peek_kind p <> Token.COLON
          && peek_kind p <> Token.WHERE do
      fun_params := parse_param p :: !fun_params
    done;
    let ret_annot = if peek_kind p = Token.COLON then begin
      ignore (advance p);
      let ty = parse_ty p in
      (* Check for / eff after return type annotation *)
      if peek_kind p = Token.SLASH then begin
        ignore (advance p);
        let eff = if peek_kind p = Token.IDENT "pure" then
          (ignore (advance p); Ast.EffAnnotPure)
        else
          Ast.EffAnnotRow (parse_eff_items p)
        in
        Some (Ast.TyWithEffect (ty, eff))
      end else
        Some ty
    end else None in
    let constraints = parse_constraints p in
    expect p Token.EQ;
    let body = parse_expr p in
    let fun_params = List.rev !fun_params in
    let (params, body) = resolve_fun_params fun_params body in
    (* Check if this is a let-in expression (not a declaration) *)
    if peek_kind p = Token.IN then begin
      ignore (advance p);
      let rest = parse_expr p in
      let full_body = wrap_params_expr fun_params ret_annot body in
      let expr = if is_rec then Ast.ELetRec (name, type_params, full_body, rest)
                 else Ast.ELet (name, full_body, rest) in
      [Ast.DExpr expr]
    end else begin
      if is_rec && peek_kind p = Token.AND then begin
        (* Mutual recursion: let rec f ... = ... and g ... = ... *)
        let bindings = ref [(name, type_params, params, ret_annot, constraints, body)] in
        while peek_kind p = Token.AND do
          ignore (advance p);
          let and_name = expect_ident p in
          let and_type_params = parse_type_params p in
          let and_fun_params = ref [] in
          while peek_kind p <> Token.EQ && peek_kind p <> Token.COLON
                && peek_kind p <> Token.WHERE do
            and_fun_params := parse_param p :: !and_fun_params
          done;
          let and_ret_annot = if peek_kind p = Token.COLON then begin
            ignore (advance p);
            let ty = parse_ty p in
            if peek_kind p = Token.SLASH then begin
              ignore (advance p);
              let eff = if peek_kind p = Token.IDENT "pure" then
                (ignore (advance p); Ast.EffAnnotPure)
              else
                Ast.EffAnnotRow (parse_eff_items p)
              in
              Some (Ast.TyWithEffect (ty, eff))
            end else
              Some ty
          end else None in
          let and_constraints = parse_constraints p in
          expect p Token.EQ;
          let and_body = parse_expr p in
          let and_fun_params = List.rev !and_fun_params in
          let (and_params, and_body) = resolve_fun_params and_fun_params and_body in
          bindings := (and_name, and_type_params, and_params, and_ret_annot, and_constraints, and_body) :: !bindings
        done;
        if peek_kind p = Token.IN then begin
          (* Expression: let rec f ... = ... and g ... = ... in body *)
          ignore (advance p);
          let rest = parse_expr p in
          let expr_bindings = List.map (fun (n, tp, ps, ra, _cs, b) ->
            let full = wrap_params_expr (List.map (fun p -> FPParam p) ps) ra b in
            (n, tp, full)
          ) (List.rev !bindings) in
          [Ast.DExpr (Ast.ELetRecAnd (expr_bindings, rest))]
        end else
          [Ast.DLetRecAnd (List.rev !bindings)]
      end else if is_rec then
        [Ast.DLetRec (name, type_params, params, ret_annot, constraints, body)]
      else
        [Ast.DLet (name, params, ret_annot, constraints, body)]
    end
  end

let parse_class_name p =
  match peek_kind p with
  | Token.UIDENT s ->
    ignore (advance p);
    if peek_kind p = Token.DOT then begin
      ignore (advance p);
      match peek_kind p with
      | Token.UIDENT s2 -> ignore (advance p); s ^ "." ^ s2
      | _ -> error p "expected class name after dot"
    end else s
  | _ -> error p "expected class name"

let parse_class_decl p =
  expect p Token.CLASS;
  let class_name = expect_uident p in
  if peek_kind p = Token.DOT then
    error p "class declarations cannot use qualified names; define the class inside a module instead";
  (* Parse one or more type variables *)
  let tyvars = ref [] in
  let continue_tyvars = ref true in
  while !continue_tyvars do
    match peek_kind p with
    | Token.TYVAR s -> ignore (advance p); tyvars := s :: !tyvars
    | _ -> continue_tyvars := false
  done;
  let tyvars = List.rev !tyvars in
  if tyvars = [] then error p "expected at least one type variable";
  (* Parse optional functional dependencies: where 'a -> 'b, 'c -> 'd *)
  let fundeps =
    if peek_kind p = Token.WHERE then begin
      ignore (advance p);
      let deps = ref [] in
      let continue = ref true in
      while !continue do
        (* Parse determining tyvars *)
        let from_vars = ref [] in
        while (match peek_kind p with Token.TYVAR _ -> true | _ -> false) do
          match (advance p).kind with
          | Token.TYVAR s ->
            if not (List.mem s tyvars) then
              error p (Printf.sprintf "functional dependency type variable '%s not in class parameters" s);
            from_vars := s :: !from_vars
          | _ -> ()
        done;
        if !from_vars = [] then error p "expected type variable(s) before '->'";
        expect p Token.ARROW;
        (* Parse determined tyvars *)
        let to_vars = ref [] in
        while (match peek_kind p with Token.TYVAR _ -> true | _ -> false) do
          match (advance p).kind with
          | Token.TYVAR s ->
            if not (List.mem s tyvars) then
              error p (Printf.sprintf "functional dependency type variable '%s not in class parameters" s);
            to_vars := s :: !to_vars
          | _ -> ()
        done;
        if !to_vars = [] then error p "expected type variable(s) after '->'";
        deps := (List.rev !from_vars, List.rev !to_vars) :: !deps;
        if peek_kind p = Token.COMMA then
          ignore (advance p)
        else
          continue := false
      done;
      List.rev !deps
    end else []
  in
  expect p Token.EQ;
  let methods = ref [] in
  while peek_kind p <> Token.END do
    let s = expect_ident p in
    expect p Token.COLON;
    let ty = parse_ty p in
    methods := (s, ty) :: !methods;
    if peek_kind p = Token.SEMICOLON then ignore (advance p)
  done;
  expect p Token.END;
  if !methods = [] then error p "class must have at least one method";
  Ast.DClass (class_name, tyvars, fundeps, List.rev !methods)

let parse_instance_decl p =
  expect p Token.INSTANCE;
  let class_name = parse_class_name p in
  (* Parse one or more type atoms until '=' or 'where' *)
  let inst_tys = ref [] in
  while peek_kind p <> Token.EQ && peek_kind p <> Token.WHERE do
    inst_tys := parse_ty_atom p :: !inst_tys
  done;
  let inst_tys = List.rev !inst_tys in
  if inst_tys = [] then error p "expected at least one instance type";
  let constraints = parse_constraints p in
  expect p Token.EQ;
  let methods = ref [] in
  while peek_kind p <> Token.END do
    expect p Token.LET;
    let method_name =
      if peek_kind p = Token.LPAREN then begin
        ignore (advance p);
        let name = match peek_kind p with
          | Token.PLUS -> "+"
          | Token.MINUS -> "-"
          | Token.STAR -> "*"
          | Token.SLASH -> "/"
          | Token.LT -> "<"
          | Token.GT -> ">"
          | Token.LE -> "<="
          | Token.GE -> ">="
          | Token.EQ -> "="
          | Token.NEQ -> "<>"
          | Token.LAND -> "land"
          | Token.LOR -> "lor"
          | Token.LXOR -> "lxor"
          | Token.LNOT -> "lnot"
          | Token.LSL -> "lsl"
          | Token.LSR -> "lsr"
          | _ -> error p "expected operator"
        in
        ignore (advance p);
        expect p Token.RPAREN;
        name
      end else
        expect_ident p
    in
    let params = ref [] in
    while peek_kind p <> Token.EQ && peek_kind p <> Token.COLON
          && peek_kind p <> Token.WHERE do
      params := parse_param p :: !params
    done;
    let ret_annot = if peek_kind p = Token.COLON then begin
      ignore (advance p);
      Some (parse_ty p)
    end else None in
    let _constraints = parse_constraints p in
    expect p Token.EQ;
    let body = parse_expr p in
    let fun_params = List.rev !params in
    (* Apply match desugaring for destructuring pattern params *)
    let body = List.fold_left (fun body fp ->
      match fp with
      | FPParam _ -> body
      | FPPat (name, pat, _) ->
        Ast.EMatch (Ast.EVar name, [(pat, None, body)], true)
    ) body fun_params in
    let body = match ret_annot with
      | Some ty -> Ast.EAnnot (body, ty)
      | None -> body
    in
    let plain_params = List.map (fun fp ->
      match fp with
      | FPParam param -> param
      | FPPat (name, _, annot) -> Ast.{ name; annot; is_generated = true }
    ) fun_params in
    methods := (method_name, plain_params, body) :: !methods
  done;
  expect p Token.END;
  if !methods = [] then error p "instance must implement at least one method";
  Ast.DInstance (class_name, inst_tys, constraints, List.rev !methods)

let parse_effect_decl p =
  expect p Token.EFFECT;
  let name = parse_class_name p in
  (* Parse optional type parameters: effect State 'a = ... *)
  let type_params = ref [] in
  while (match peek_kind p with Token.TYVAR _ -> true | _ -> false) do
    match (advance p).kind with
    | Token.TYVAR s -> type_params := s :: !type_params
    | _ -> ()
  done;
  let type_params = List.rev !type_params in
  expect p Token.EQ;
  let ops = ref [] in
  while peek_kind p <> Token.END do
    let s = expect_ident p in
    expect p Token.COLON;
    let ty = parse_ty p in
    ops := (s, ty) :: !ops;
    if peek_kind p = Token.SEMICOLON then ignore (advance p)
  done;
  expect p Token.END;
  if !ops = [] then error p "effect must have at least one operation";
  Ast.DEffect (name, type_params, List.rev !ops)

let parse_extern_decl p =
  expect p Token.EXTERN;
  let name = match peek_kind p with
    | Token.UIDENT mod_name ->
      ignore (advance p);
      expect p Token.DOT;
      let field = expect_ident p in
      mod_name ^ "." ^ field
    | _ -> expect_ident p
  in
  expect p Token.COLON;
  let ty = parse_ty p in
  Ast.DExtern (name, ty)

let rec parse_module_body_item p : Ast.module_decl list =
  match peek_kind p with
  | Token.PUB ->
    ignore (advance p);
    let decls = parse_inner_decl p in
    List.map (fun d -> Ast.{ vis = Public; decl = d }) decls
  | Token.OPAQUE ->
    ignore (advance p);
    (* opaque only applies to type declarations *)
    let decl = parse_type_decl p in
    [Ast.{ vis = Opaque; decl }]
  | _ ->
    let decls = parse_inner_decl p in
    List.map (fun d -> Ast.{ vis = Private; decl = d }) decls

and parse_inner_decl p =
  match peek_kind p with
  | Token.TYPE -> [parse_type_decl p]
  | Token.LET -> parse_let_decl p
  | Token.CLASS -> [parse_class_decl p]
  | Token.INSTANCE -> [parse_instance_decl p]
  | Token.EFFECT -> [parse_effect_decl p]
  | Token.EXTERN -> [parse_extern_decl p]
  | Token.MODULE -> [parse_module_decl p]
  | Token.OPEN -> [parse_open_decl p]
  | _ -> error p "expected declaration inside module"

and parse_module_decl p =
  expect p Token.MODULE;
  let name = expect_uident p in
  expect p Token.EQ;
  let items = ref [] in
  while peek_kind p <> Token.END do
    let module_items = parse_module_body_item p in
    items := List.rev_append module_items !items;
    (* Optional ;; separator *)
    if peek_kind p = Token.DOUBLE_SEMICOLON then
      ignore (advance p)
  done;
  expect p Token.END;
  Ast.DModule (name, List.rev !items)

and parse_open_decl p =
  expect p Token.OPEN;
  let name = expect_uident p in
  (* Check for selective import: open M (x, y) *)
  if peek_kind p = Token.LPAREN then begin
    ignore (advance p);
    let names = ref [] in
    let first = ref true in
    while peek_kind p <> Token.RPAREN do
      if not !first then expect p Token.COMMA;
      first := false;
      let n = expect_ident p in
      names := n :: !names
    done;
    expect p Token.RPAREN;
    Ast.DOpen (name, Some (List.rev !names))
  end else
    Ast.DOpen (name, None)

and parse_decl p =
  match peek_kind p with
  | Token.TYPE -> [parse_type_decl p]
  | Token.LET -> parse_let_decl p
  | Token.CLASS -> [parse_class_decl p]
  | Token.INSTANCE -> [parse_instance_decl p]
  | Token.EFFECT -> [parse_effect_decl p]
  | Token.EXTERN -> [parse_extern_decl p]
  | Token.MODULE -> [parse_module_decl p]
  | Token.OPEN -> [parse_open_decl p]
  | _ ->
    let expr = parse_expr p in
    [Ast.DExpr expr]

let parse_program tokens =
  let p = create tokens in
  let decls = ref [] in
  while peek_kind p <> Token.EOF do
    let ds = parse_decl p in
    decls := List.rev_append ds !decls;
    (* Optional ;; separator *)
    if peek_kind p = Token.DOUBLE_SEMICOLON then
      ignore (advance p)
  done;
  List.rev !decls

let parse_expr_string tokens =
  let p = create tokens in
  parse_expr p
