(* OCaml-to-MiniML Translator
   Uses OCaml compiler-libs to parse .ml files and emit equivalent MiniML source. *)

open Parsetree
open Asttypes

(* --- Emitter: Buffer-based output with indentation --- *)

type emitter = {
  buf: Buffer.t;
  mutable indent: int;
  mutable at_line_start: bool;
}

let create_emitter () =
  { buf = Buffer.create 4096; indent = 0; at_line_start = true }

let emit_indent e =
  if e.at_line_start then begin
    for _ = 1 to e.indent do
      Buffer.add_string e.buf "  "
    done;
    e.at_line_start <- false
  end

let emit e s =
  emit_indent e;
  Buffer.add_string e.buf s

let _emit_char e c =
  emit_indent e;
  Buffer.add_char e.buf c

let newline e =
  Buffer.add_char e.buf '\n';
  e.at_line_start <- true

let emit_line e s =
  emit e s;
  newline e

let indent e = e.indent <- e.indent + 1
let dedent e = e.indent <- e.indent - 1

let contents e = Buffer.contents e.buf

(* --- Name translation --- *)

(* MiniML keywords that can't be used as variable names *)
let escape_keyword name =
  match name with
  | "fn" -> "fn_"
  | "do" -> "do_"
  | "end" -> "end_"
  | "pub" -> "pub_"
  | "continue" -> "continue_"
  | "perform" -> "perform_"
  | "handle" -> "handle_"
  | "resume" -> "resume_"
  | "effect" -> "effect_"
  | "mut" -> "mut_"
  | "for" -> "for_"
  | "break" -> "break_"
  | "where" -> "where_"
  | "opaque" -> "opaque_"
  | "deriving" -> "deriving_"
  | _ -> name

let translate_ident name =
  match name with
  (* stdlib function renames *)
  | "fold_left" -> "fold"
  | "fold_left2" -> "fold2"
  | "fold_right" -> "fold_right"
  | "for_all" -> "forall"
  | "for_all2" -> "forall2"
  | "add_last" -> "push"
  | _ -> escape_keyword name

let translate_module_name name =
  match name with
  | "Hashtbl" -> "Hashtbl"
  | _ -> name

(* Translate a qualified name like Module.func *)
let translate_qualified modname funcname =
  match modname, funcname with
  (* Hashtbl *)
  | "Hashtbl", "replace" -> ("Hashtbl", "set")
  | "Hashtbl", "add" -> ("Hashtbl", "set")
  | "Hashtbl", "find_opt" -> ("Hashtbl", "get")
  | "Hashtbl", "mem" -> ("Hashtbl", "has")
  (* Dynarray *)
  | "Dynarray", "add_last" -> ("Dynarray", "push")
  (* Char → Byte *)
  | "Char", "code" -> ("Byte", "to_int")
  | "Char", "chr" -> ("Byte", "of_int")
  | "Char", "escaped" -> ("Byte", "to_string")
  | "Char", "uppercase_ascii" -> ("Byte", "to_upper")
  | "Char", "lowercase_ascii" -> ("Byte", "to_lower")
  (* Buffer: add_char → add_byte *)
  | "Buffer", "add_char" -> ("Buffer", "add_byte")
  (* String *)
  | "String", "unsafe_get" -> ("String", "get")
  (* List: find_opt → find (MiniML find returns option), find → list_find (raises) *)
  | "List", "find_opt" -> ("List", "find")
  | "List", "find" -> ("", "list_find")
  (* List.concat in OCaml flattens a list of lists; MiniML List.flatten *)
  | "List", "concat" -> ("List", "flatten")
  (* Option *)
  | "Option", "get" -> ("Option", "unwrap")
  | _ -> (translate_module_name modname, translate_ident funcname)

(* Map OCaml operators to MiniML *)
let translate_op op =
  match op with
  | "+" | "-" | "*" | "/" | "mod" -> op
  | "=" | "<>" | "<" | ">" | "<=" | ">=" -> op
  | "==" -> "="  (* physical equality → structural equality *)
  | "!=" -> "<>" (* physical inequality → structural inequality *)
  | "^" -> "^"
  | "&&" -> "&&"
  | "||" -> "||"
  | ":=" -> ":="
  | "::" -> "::"
  | "@" -> "@"
  | "land" -> "land"
  | "lor" -> "lor"
  | "lxor" -> "lxor"
  | "lsl" -> "lsl"
  | "lsr" -> "lsr"
  | "|>" -> "|>"
  | _ -> op

(* Check if an operator is prefix/unary *)
let is_prefix_op op =
  match op with
  | "!" | "~-" | "~-." | "not" | "lnot" -> true
  | _ -> false

(* Convert an exception constructor name to an effect operation name.
   E.g., "Unify_error" → "unify_error", "TypeError" → "type_error" *)
let exn_to_effect_op name =
  (* Just lowercase the first char — exception names like Unify_error
     are already snake_case with a capital first letter *)
  if String.length name > 0 then
    String.make 1 (Char.lowercase_ascii name.[0]) ^ String.sub name 1 (String.length name - 1)
  else name

(* Convert exception name to effect name.
   E.g., "Unify_error" → "UnifyError" effect with "unify_error" operation *)
let exn_to_effect_name name =
  (* Keep the constructor-style name for the effect *)
  name ^ "Exn"

(* --- Parsetree walkers --- *)

(* Emit a type expression *)
let rec emit_type e (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_any -> emit e "_"
  | Ptyp_var s -> emit e ("'" ^ s)
  | Ptyp_arrow (_, t1, t2) ->
    let needs_parens = match t1.ptyp_desc with
      | Ptyp_arrow _ -> true | _ -> false
    in
    if needs_parens then emit e "(";
    emit_type e t1;
    if needs_parens then emit e ")";
    emit e " -> ";
    emit_type e t2


  | Ptyp_tuple ts ->
    List.iteri (fun i t ->
      if i > 0 then emit e " * ";
      let needs_parens = match t.ptyp_desc with
        | Ptyp_arrow _ | Ptyp_tuple _ -> true | _ -> false
      in
      if needs_parens then emit e "(";
      emit_type e t;
      if needs_parens then emit e ")"
    ) ts


  | Ptyp_constr (lid, args) ->
    let name = Longident.last lid.txt in
    (* ref type: t ref → t Ref.t *)
    if name = "ref" then begin
      (match args with
       | [arg] ->
         emit_type e arg;
         emit e " Ref.t"
       | _ -> emit e "(* ref with unexpected args *)");
    end else
    let module_path = match lid.txt with
      | Longident.Ldot (Longident.Lident m, _) -> Some m
      | _ -> None
    in
    let full_name = match module_path with
      | Some m ->
        let m = translate_module_name m in
        m ^ "." ^ name
      | None -> name
    in
    (match args with
     | [] -> emit e full_name
     | [arg] ->
       let needs_parens = match arg.ptyp_desc with
         | Ptyp_tuple _ | Ptyp_arrow _ -> true | _ -> false
       in
       if needs_parens then emit e "(";
       emit_type e arg;
       if needs_parens then emit e ")";
       emit e (" " ^ full_name)
     | _ ->
       emit e "(";
       List.iteri (fun i arg ->
         if i > 0 then emit e ", ";
         emit_type e arg
       ) args;
       emit e (") " ^ full_name))


  | Ptyp_alias (t, s) ->
    emit_type e t;
    emit e (" as '" ^ s.txt)


  | Ptyp_variant _ ->
    emit e "(* TODO: polymorphic variant type *)"


  | Ptyp_poly (_, t) ->
    emit_type e t


  | Ptyp_package _ -> emit e "(* TODO: package type *)"

  | Ptyp_extension _ -> emit e "(* TODO: extension *)"

  | Ptyp_object _ | Ptyp_class _ | Ptyp_open _ ->

    emit e "(* TODO: object/class/open type *)"

(* Emit a pattern *)
and emit_pattern e (pat : pattern) =
  match pat.ppat_desc with
  | Ppat_any -> emit e "_"
  | Ppat_var v -> emit e (escape_keyword v.txt)
  | Ppat_constant c -> emit_constant e c
  | Ppat_tuple ps ->
    emit e "(";
    List.iteri (fun i p ->
      if i > 0 then emit e ", ";
      emit_pattern e p
    ) ps;
    emit e ")"
  | Ppat_construct (lid, arg) ->
    let name = Longident.last lid.txt in
    let full_name = match lid.txt with
      | Longident.Ldot (Longident.Lident m, n) -> m ^ "." ^ n
      | _ -> name
    in
    (match full_name with
     | "true" -> emit e "true"
     | "false" -> emit e "false"
     | "()" -> emit e "()"
     | "[]" -> emit e "[]"
     | "::" ->
       (match arg with
        | Some (_, { ppat_desc = Ppat_tuple [hd; tl]; _ }) ->
          let needs_parens = match hd.ppat_desc with
            | Ppat_construct ({ txt = Lident "::"; _ }, _) -> true
            | _ -> false
          in
          if needs_parens then emit e "(";
          emit_pattern e hd;
          if needs_parens then emit e ")";
          emit e " :: ";
          emit_pattern e tl
        | _ ->
          emit e "(::) ";
          Option.iter (fun (_, p) -> emit_pattern e p) arg)
     | _ ->
       emit e full_name;
       Option.iter (fun (_, p) ->
         emit e " ";
         let needs_parens = match p.ppat_desc with
           | Ppat_tuple _ | Ppat_construct (_, Some _) | Ppat_alias _ -> true
           | _ -> false
         in
         if needs_parens then emit e "(";
         emit_pattern e p;
         if needs_parens then emit e ")"
       ) arg)
  | Ppat_record (fields, closed) ->
    emit e "{";
    List.iteri (fun i (lid, p) ->
      if i > 0 then emit e "; ";
      let fname = Longident.last lid.txt in
      (match p.ppat_desc with
       | Ppat_var v when String.equal v.txt fname ->
         emit e fname  (* punning *)
       | _ ->
         emit e fname;
         emit e " = ";
         emit_pattern e p)
    ) fields;
    if closed = Open then emit e "; _";
    emit e "}"
  | Ppat_array ps ->
    emit e "#[";
    List.iteri (fun i p ->
      if i > 0 then emit e "; ";
      emit_pattern e p
    ) ps;
    emit e "]"
  | Ppat_or (p1, p2) ->
    emit_pattern e p1;
    emit e " | ";
    emit_pattern e p2
  | Ppat_constraint (p, t) ->
    emit e "(";
    emit_pattern e p;
    emit e " : ";
    emit_type e t;
    emit e ")"
  | Ppat_alias (p, name) ->
    (match p.ppat_desc with
     | Ppat_or _ ->
       (* MiniML doesn't support (A | B) as x; use variable as catch-all *)
       emit e name.txt
     | _ ->
       emit_pattern e p;
       emit e (" as " ^ name.txt))
  | Ppat_variant (label, arg) ->
    emit e ("`" ^ label);
    Option.iter (fun p ->
      emit e " ";
      emit_pattern e p
    ) arg
  | Ppat_lazy _ -> emit e "(* TODO: lazy pattern *)"
  | Ppat_interval _ ->
    (* Handled specially in emit_cases — should not reach here *)
    emit e "_interval_"
  | Ppat_type _ -> emit e "(* TODO: type pattern *)"
  | Ppat_unpack _ -> emit e "(* TODO: unpack pattern *)"
  | Ppat_exception _ -> emit e "(* TODO: exception pattern *)"
  | Ppat_extension _ -> emit e "(* TODO: extension pattern *)"
  | Ppat_open _ -> emit e "(* TODO: open pattern *)"
  | Ppat_effect _ -> emit e "(* TODO: effect pattern *)"

(* Emit a constant *)
and emit_constant e (c : Parsetree.constant) =
  match c.pconst_desc with
  | Pconst_integer (s, _) ->
    if String.length s > 0 && s.[0] = '-' then
      emit e ("(" ^ s ^ ")")
    else emit e s
  | Pconst_float (s, _) ->
    if String.length s > 0 && s.[0] = '-' then
      emit e ("(" ^ s ^ ")")
    else emit e s
  | Pconst_char c ->
    (* OCaml char → MiniML byte literal (#XX hex format).
       OCaml char is 0-255, which maps to MiniML byte, not rune. *)
    emit e (Printf.sprintf "#%02x" (Char.code c))
  | Pconst_string (s, _, _) ->
    emit e "\"";
    (* Custom escaping: preserve UTF-8 bytes, only escape what MiniML needs *)
    String.iter (fun c ->
      match c with
      | '\\' -> emit e "\\\\"
      | '"' -> emit e "\\\""
      | '\n' -> emit e "\\n"
      | '\t' -> emit e "\\t"
      | '\000' -> emit e "\\0"
      | c -> Buffer.add_char e.buf c; e.at_line_start <- false
    ) s;
    emit e "\""

(* Check if an expression is a simple atom (no parens needed) *)
and is_atom (expr : expression) =
  match expr.pexp_desc with
  | Pexp_ident _ | Pexp_constant _ | Pexp_tuple _
  | Pexp_array _ | Pexp_record _ | Pexp_construct (_, None) -> true
  | Pexp_construct ({ txt = Lident "true"; _ }, _)
  | Pexp_construct ({ txt = Lident "false"; _ }, _)
  | Pexp_construct ({ txt = Lident "()"; _ }, _) -> true
  | _ -> false

(* Check if an expression ends with a match/try (recursively through let/seq) *)
and ends_with_match (expr : expression) =
  match expr.pexp_desc with
  | Pexp_match _ | Pexp_try _ | Pexp_function _ -> true
  | Pexp_let (_, _, body) -> ends_with_match body
  | Pexp_sequence (_, e2) -> ends_with_match e2
  | Pexp_ifthenelse (_, _, Some el) -> ends_with_match el
  | Pexp_ifthenelse (_, then_e, None) -> ends_with_match then_e
  | _ -> false

(* Emit an expression *)
and emit_expr e (expr : expression) =
  match expr.pexp_desc with
  | Pexp_ident lid ->
    let name = match lid.txt with
      | Longident.Lident s ->
        translate_ident s
      | Longident.Ldot (Longident.Lident "Fun", "id") ->
        "(fn x -> x)"
      | Longident.Ldot (Longident.Lident m, s) ->
        let (m, s) = translate_qualified m s in
        if m = "" then s else m ^ "." ^ s
      | Longident.Ldot (Longident.Ldot (Longident.Lident m1, m2), s) ->
        m1 ^ "." ^ m2 ^ "." ^ translate_ident s
      | _ -> Format.asprintf "%a" Pprintast.longident lid.txt
    in
    emit e name

  | Pexp_constant c -> emit_constant e c

  | Pexp_let (rf, bindings, body) ->
    if rf = Nonrecursive && List.length bindings > 1 then begin
      (* Non-recursive let...and → sequential lets *)
      List.iter (fun vb ->
        emit_let_bindings e Nonrecursive [vb] false;
        emit e " in";
        newline e
      ) bindings;
      emit_expr e body
    end else begin
      emit_let_bindings e rf bindings false;
      emit e " in";
      newline e;
      emit_expr e body
    end

  | Pexp_function (params, constraint_, body) ->
    emit_fun e params constraint_ body

  | Pexp_apply (fn, args) ->
    emit_apply e fn args

  | Pexp_match (scrut, cases) ->
    emit e "match ";
    emit_expr e scrut;
    emit e " with";
    newline e;
    emit_cases e cases

  | Pexp_try (body, cases) ->
    (* Check for try E with _ -> () pattern — emit as just E *)
    let is_catch_all_unit = match cases with
      | [{ pc_lhs = { ppat_desc = Ppat_any; _ };
           pc_guard = None;
           pc_rhs = { pexp_desc = Pexp_construct ({ txt = Lident "()"; _ }, None); _ } }] -> true
      | _ -> false
    in
    if is_catch_all_unit then begin
      emit e "(* try/catch-all *) ";
      emit_expr e body
    end else begin
      emit e "do try";
      newline e;
      indent e;
      emit_expr e body;
      dedent e;
      newline e;
      emit e "with";
      newline e;
      emit_exn_cases e cases;
      emit e "end"
    end

  | Pexp_tuple es ->
    emit e "(";
    List.iteri (fun i ex ->
      if i > 0 then emit e ", ";
      emit_expr e ex
    ) es;
    emit e ")"

  | Pexp_construct (lid, arg) ->
    let name = Longident.last lid.txt in
    let full_name = match lid.txt with
      | Longident.Ldot (Longident.Lident m, n) -> m ^ "." ^ n
      | _ -> name
    in
    (match full_name with
     | "true" -> emit e "true"
     | "false" -> emit e "false"
     | "()" -> emit e "()"
     | "[]" -> emit e "[]"
     | "::" ->
       (match arg with
        | Some { pexp_desc = Pexp_tuple [hd; tl]; _ } ->
          emit_expr e hd;
          emit e " :: ";
          emit_expr e tl
        | _ ->
          emit e "(::)";
          Option.iter (fun a ->
            emit e " ";
            emit_expr e a
          ) arg)
     | _ ->
       emit e full_name;
       Option.iter (fun a ->
         emit e " ";
         let needs_parens = not (is_atom a) in
         if needs_parens then emit e "(";
         emit_expr e a;
         if needs_parens then emit e ")"
       ) arg)

  | Pexp_variant (label, arg) ->
    emit e ("`" ^ label);
    Option.iter (fun a ->
      emit e " ";
      let needs_parens = not (is_atom a) in
      if needs_parens then emit e "(";
      emit_expr e a;
      if needs_parens then emit e ")"
    ) arg

  | Pexp_record (fields, base) ->
    emit e "{";
    Option.iter (fun b ->
      emit_expr e b;
      emit e " with "
    ) base;
    List.iteri (fun i (lid, ex) ->
      if i > 0 then emit e "; ";
      let fname = Longident.last lid.txt in
      (* Check for punning: {x = x} → {x} *)
      (match ex.pexp_desc with
       | Pexp_ident { txt = Longident.Lident n; _ } when String.equal n fname ->
         emit e fname
       | _ ->
         emit e fname;
         emit e " = ";
         emit_expr e ex)
    ) fields;
    emit e "}"

  | Pexp_field (ex, lid) ->
    (* Wrap non-atom expressions in parens so .field binds to the whole expression *)
    let needs_parens = not (is_atom ex) && (match ex.pexp_desc with
      | Pexp_field _ -> false  (* chained field access is fine *)
      | Pexp_ident _ -> false
      | _ -> true) in
    if needs_parens then emit e "(";
    emit_expr e ex;
    if needs_parens then emit e ")";
    emit e ".";
    emit e (Longident.last lid.txt)

  | Pexp_setfield (ex, lid, v) ->
    emit_expr e ex;
    emit e ".";
    emit e (Longident.last lid.txt);
    emit e " := ";
    emit_expr e v

  | Pexp_array es ->
    emit e "#[";
    List.iteri (fun i ex ->
      if i > 0 then emit e "; ";
      emit_expr e ex
    ) es;
    emit e "]"

  | Pexp_ifthenelse (cond, then_e, else_e) ->
    emit e "if ";
    emit_expr e cond;
    emit e " do";
    newline e;
    indent e;
    (* Wrap then-body in do...end if it ends with a match/try
       (avoids parser ambiguity with else or enclosing constructs) *)
    let needs_wrap = else_e <> None && ends_with_match then_e in
    if needs_wrap then begin
      emit e "do";
      newline e;
      indent e;
      emit_expr e then_e;
      newline e;
      dedent e;
      emit e "end"
    end else
      emit_expr e then_e;
    dedent e;
    (match else_e with
     | Some el ->
       (* Check if else branch is unit (no-else in MiniML) *)
       (match el.pexp_desc with
        | Pexp_construct ({ txt = Lident "()"; _ }, None) ->
          newline e;
          emit e "end"
        | Pexp_ifthenelse _ ->
          (* else if chain — no indent *)
          newline e;
          emit e "else ";
          emit_expr e el
        | Pexp_sequence _ | Pexp_let _ ->
          (* Sequence/let in else needs do...end block to contain scope *)
          newline e;
          emit e "else do";
          newline e;
          indent e;
          emit_expr e el;
          newline e;
          dedent e;
          emit e "end"
        | _ ->
          newline e;
          emit e "else";
          newline e;
          indent e;
          emit_expr e el;
          dedent e)
     | None ->
       newline e;
       emit e "end")

  | Pexp_sequence (e1, e2) ->
    (* Wrap match/try in do...end when they appear as the first part of a sequence,
       to prevent their cases from leaking into an outer match *)
    let wrap_e1 = match e1.pexp_desc with
      | Pexp_match _ | Pexp_try _ | Pexp_function _ -> true
      | _ -> false
    in
    if wrap_e1 then begin
      emit e "do";
      newline e;
      indent e;
      emit_expr e e1;
      newline e;
      dedent e;
      emit e "end";
    end else
      emit_expr e e1;
    emit e ";";
    newline e;
    emit_expr e e2

  | Pexp_while (cond, body) ->
    emit e "for ";
    emit_expr e cond;
    emit e " do";
    newline e;
    indent e;
    emit_expr e body;
    newline e;
    dedent e;
    emit e "end"

  | Pexp_for (var, lo, hi, dir, body) ->
    let var_name = match var.ppat_desc with
      | Ppat_var v -> v.txt
      | _ -> "_loop_var"
    in
    emit e "let mut ";
    emit e var_name;
    emit e " = ";
    emit_expr e lo;
    emit e " in";
    newline e;
    emit e "for ";
    emit e var_name;
    emit e (if dir = Upto then " <= " else " >= ");
    emit_expr e hi;
    emit e " do";
    newline e;
    indent e;
    (* Wrap body in do...end if it ends with a match/try
       (avoids `;` being parsed as part of last match arm) *)
    if ends_with_match body then begin
      emit e "do ";
      emit_expr e body;
      newline e;
      emit e "end"
    end else
      emit_expr e body;
    emit e ";";
    newline e;
    emit e var_name;
    emit e (if dir = Upto then " := " ^ var_name ^ " + 1" else " := " ^ var_name ^ " - 1");
    newline e;
    dedent e;
    emit e "end"

  | Pexp_constraint (ex, ty) ->
    emit e "(";
    emit_expr e ex;
    emit e " : ";
    emit_type e ty;
    emit e ")"

  | Pexp_coerce (ex, _, ty) ->
    emit e "(";
    emit_expr e ex;
    emit e " : ";
    emit_type e ty;
    emit e ")"

  | Pexp_assert { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ } ->
    emit e "failwith \"assert false\""

  | Pexp_assert ex ->
    emit e "if not (";
    emit_expr e ex;
    emit e ") do failwith \"assertion failed\" end"

  | Pexp_open (_, ex) ->
    (* Local open: M.(expr) *)
    emit_expr e ex

  | Pexp_letop _ -> emit e "(* TODO: letop *)"
  | Pexp_pack _ -> emit e "(* TODO: pack *)"
  | Pexp_newtype _ -> emit e "(* TODO: newtype *)"
  | Pexp_lazy inner ->
    (* lazy e → fn () -> e (thunk) *)
    emit e "(fn () -> ";
    emit_expr e inner;
    emit e ")"
  | Pexp_object _ -> emit e "(* TODO: object *)"
  | Pexp_poly _ -> emit e "(* TODO: poly *)"
  | Pexp_send _ -> emit e "(* TODO: send *)"
  | Pexp_new _ -> emit e "(* TODO: new *)"
  | Pexp_override _ -> emit e "(* TODO: override *)"
  | Pexp_letmodule _ -> emit e "(* TODO: letmodule *)"
  | Pexp_letexception _ -> emit e "(* TODO: letexception *)"
  | Pexp_extension _ -> emit e "(* TODO: extension *)"
  | Pexp_unreachable -> emit e "failwith \"unreachable\""
  | _ -> emit e "(* TODO: unsupported expression *)"

(* Emit function parameters: fun x y z -> body *)
and emit_fun e params constraint_ body =
  emit e "fn";
  List.iter (fun (p : function_param) ->
    match p.pparam_desc with
    | Pparam_val (_lbl, _default, pat) ->
      emit e " ";
      emit_pattern e pat
    | Pparam_newtype name ->
      emit e (" (type " ^ name.txt ^ ")")
  ) params;
  (* Emit return type constraint if present *)
  (match constraint_ with
   | Some (Pconstraint ty) ->
     emit e ": ";
     emit_type e ty;
     emit e " "
   | _ -> ());
  (* Emit body *)
  (match body with
   | Pfunction_body body_expr ->
     emit e " ->";
     (* Wrap match/function bodies in do...end to avoid parser ambiguity *)
     let needs_wrap = match body_expr.pexp_desc with
       | Pexp_match _ | Pexp_function _ -> true
       | _ -> false
     in
     if needs_wrap then begin
       newline e;
       indent e;
       emit e "do";
       newline e;
       indent e;
       emit_expr e body_expr;
       newline e;
       dedent e;
       emit e "end";
       dedent e
     end else begin
       emit e " ";
       emit_expr e body_expr
     end
   | Pfunction_cases (cases, _, _) ->
     newline e;
     indent e;
     emit_cases e cases;
     dedent e)

(* Parse a Printf format string and substitute args to produce $"..." interpolation.
   Returns a string like: $"type {name} expects {n} arguments"
   Format specifiers: %s, %d, %i, %f, %c, %b, %Ld, %Li etc. *)
and emit_sprintf e args =
  match args with
  | [] -> emit e "\"\""
  | (_, fmt_expr) :: value_args ->
    (* Extract format string *)
    let fmt_str = match fmt_expr.pexp_desc with
      | Pexp_constant c ->
        (match c.pconst_desc with
         | Pconst_string (s, _, _) -> Some s
         | _ -> None)
      | _ -> None
    in
    match fmt_str with
    | None ->
      (* Can't parse format string, fall back to concat *)
      emit e "(";
      List.iteri (fun i (_, arg) ->
        if i > 0 then emit e " ^ ";
        emit_expr e arg
      ) args;
      emit e ")"
    | Some fmt ->
      (* Parse format string, collecting segments *)
      let buf = Buffer.create 64 in
      let arg_queue = ref (List.map snd value_args) in
      let pop_arg () = match !arg_queue with
        | a :: rest -> arg_queue := rest; a
        | [] -> fmt_expr (* shouldn't happen — return format expr as fallback *)
      in
      Buffer.add_string buf "$\"";
      let i = ref 0 in
      let len = String.length fmt in
      while !i < len do
        if fmt.[!i] = '%' && !i + 1 < len then begin
          let next = fmt.[!i + 1] in
          match next with
          | '%' ->
            Buffer.add_char buf '%';
            i := !i + 2
          | 's' | 'd' | 'i' | 'b' | 'u' ->
            (* Simple conversions: {expr} suffices *)
            let arg = pop_arg () in
            Buffer.add_char buf '{';
            let sub_e = create_emitter () in
            emit_expr sub_e arg;
            Buffer.add_string buf (contents sub_e);
            Buffer.add_char buf '}';
            i := !i + 2
          | 'f' | 'g' | 'e' | 'G' | 'E' | 'x' | 'X' | 'o' | 'c' | 'C' | 'S' ->
            (* Conversions that need a format spec: {expr:g} etc. *)
            let arg = pop_arg () in
            Buffer.add_char buf '{';
            let sub_e = create_emitter () in
            emit_expr sub_e arg;
            Buffer.add_string buf (contents sub_e);
            Buffer.add_char buf ':';
            Buffer.add_char buf next;
            Buffer.add_char buf '}';
            i := !i + 2
          | 'L' | 'l' | 'n' when !i + 2 < len ->
            (* %Ld, %Li, %ld, etc. — treat as simple integer *)
            let arg = pop_arg () in
            Buffer.add_char buf '{';
            let sub_e = create_emitter () in
            emit_expr sub_e arg;
            Buffer.add_string buf (contents sub_e);
            Buffer.add_char buf '}';
            i := !i + 3
          | _ ->
            (* Flags/width/precision: skip to conversion char, emit {expr:fmt} *)
            if next >= '0' && next <= '9' || next = '-' || next = '+' || next = ' ' || next = '#' || next = '.' then begin
              let j = ref (!i + 1) in
              while !j < len && (let c = fmt.[!j] in
                c >= '0' && c <= '9' || c = '-' || c = '+' || c = ' ' || c = '#' || c = '.') do
                incr j
              done;
              if !j < len then begin
                (* Extract format spec: everything between % and conversion char (inclusive) *)
                let fmt_spec = String.sub fmt (!i + 1) (!j - !i) in
                let arg = pop_arg () in
                Buffer.add_char buf '{';
                let sub_e = create_emitter () in
                emit_expr sub_e arg;
                Buffer.add_string buf (contents sub_e);
                Buffer.add_char buf ':';
                Buffer.add_string buf fmt_spec;
                Buffer.add_char buf '}';
                i := !j + 1
              end else begin
                Buffer.add_char buf '%';
                i := !i + 1
              end
            end else begin
              Buffer.add_char buf '%';
              i := !i + 1
            end
        end else begin
          let c = fmt.[!i] in
          (* Escape special chars in interpolation string *)
          if c = '{' then Buffer.add_string buf "\\{"
          else if c = '}' then Buffer.add_string buf "\\}"
          else if c = '"' then Buffer.add_string buf "\\\""
          else if c = '\\' then Buffer.add_string buf "\\\\"
          else Buffer.add_char buf c;
          i := !i + 1
        end
      done;
      Buffer.add_char buf '"';
      emit e (Buffer.contents buf)

(* Check if expression needs parens as a sub-expression *)
and needs_parens_subexpr (expr : expression) =
  match expr.pexp_desc with
  | Pexp_apply _ | Pexp_match _ | Pexp_ifthenelse _
  | Pexp_let _ | Pexp_function _ | Pexp_sequence _
  | Pexp_construct (_, Some _) -> true
  | _ -> false

(* Emit function application *)
and emit_apply e fn args =
  match fn.pexp_desc, args with
  (* !x → Ref.get x *)
  | Pexp_ident { txt = Lident "!"; _ }, [(_, arg)] ->
    emit e "Ref.get ";
    if needs_parens_subexpr arg then emit e "(";
    emit_expr e arg;
    if needs_parens_subexpr arg then emit e ")"
  (* Lazy.force x → x () *)
  | Pexp_ident { txt = Longident.Ldot (Lident "Lazy", "force"); _ }, [(_, arg)] ->
    emit_expr e arg;
    emit e " ()"
  (* raise (Constructor arg) → perform exn_op arg *)
  | Pexp_ident { txt = Lident "raise"; _ },
    [(_, { pexp_desc = Pexp_construct ({ txt = Lident ctor; _ }, payload); _ })] ->
    let op_name = exn_to_effect_op ctor in
    emit e ("perform " ^ op_name ^ " ");
    (match payload with
     | Some arg ->
       let needs_parens = needs_parens_subexpr arg in
       if needs_parens then emit e "(";
       emit_expr e arg;
       if needs_parens then emit e ")"
     | None -> emit e "()")
  (* raise (Module.Constructor arg) → perform exn_op arg *)
  | Pexp_ident { txt = Lident "raise"; _ },
    [(_, { pexp_desc = Pexp_construct ({ txt = Longident.Ldot (_, ctor); _ }, payload); _ })] ->
    let op_name = exn_to_effect_op ctor in
    emit e ("perform " ^ op_name ^ " ");
    (match payload with
     | Some arg ->
       let needs_parens = needs_parens_subexpr arg in
       if needs_parens then emit e "(";
       emit_expr e arg;
       if needs_parens then emit e ")"
     | None -> emit e "()")
  (* raise arg (unknown form) → failwith arg *)
  | Pexp_ident { txt = Lident "raise"; _ }, [(_, arg)] ->
    emit e "failwith (";
    emit_expr e arg;
    emit e ")"
  (* ignore e → (e; ()) *)
  | Pexp_ident { txt = Lident "ignore"; _ }, [(_, arg)] ->
    emit e "(";
    emit_expr e arg;
    emit e "; ())"
  (* incr x → Ref.set x (Ref.get x + 1) *)
  | Pexp_ident { txt = Lident "incr"; _ }, [(_, arg)] ->
    emit e "Ref.set ";
    emit_expr e arg;
    emit e " (Ref.get ";
    emit_expr e arg;
    emit e " + 1)"
  (* decr x → Ref.set x (Ref.get x - 1) *)
  | Pexp_ident { txt = Lident "decr"; _ }, [(_, arg)] ->
    emit e "Ref.set ";
    emit_expr e arg;
    emit e " (Ref.get ";
    emit_expr e arg;
    emit e " - 1)"
  (* Fun.id x → x *)
  | Pexp_ident { txt = Longident.Ldot (Lident "Fun", "id"); _ }, [(_, arg)] ->
    emit_expr e arg
  (* Dynarray.create () → Dynarray.empty () *)
  | Pexp_ident { txt = Longident.Ldot (Lident "Dynarray", "create"); _ },
    [(_, { pexp_desc = Pexp_construct ({ txt = Lident "()"; _ }, None); _ })] ->
    emit e "Dynarray.empty ()"
  (* Hashtbl.hash x → hash x (MiniML has a Hash typeclass) *)
  | Pexp_ident { txt = Longident.Ldot (Lident "Hashtbl", "hash"); _ }, [(_, arg)] ->
    emit e "(hash (";
    emit_expr e arg;
    emit e "))"
  (* String.equal a b → a = b *)
  | Pexp_ident { txt = Longident.Ldot (Lident "String", "equal"); _ },
    [(_, lhs); (_, rhs)] ->
    if needs_parens_subexpr lhs then emit e "(";
    emit_expr e lhs;
    if needs_parens_subexpr lhs then emit e ")";
    emit e " = ";
    if needs_parens_subexpr rhs then emit e "(";
    emit_expr e rhs;
    if needs_parens_subexpr rhs then emit e ")"
  (* ref expr → Ref.create expr (when not in let-binding context) *)
  | Pexp_ident { txt = Lident "ref"; _ }, [(_, arg)] ->
    emit e "Ref.create ";
    if needs_parens_subexpr arg then emit e "(";
    emit_expr e arg;
    if needs_parens_subexpr arg then emit e ")"
  (* Printf.sprintf → string interpolation $"..." *)
  | Pexp_ident { txt = Longident.Ldot (Lident "Printf", "sprintf"); _ }, _ ->
    emit_sprintf e args
  (* Format.asprintf → string interpolation $"..." *)
  | Pexp_ident { txt = Longident.Ldot (Lident "Format", "asprintf"); _ }, _ ->
    emit_sprintf e args
  (* String.split_on_char 'c' s → String.split "c" s (char→string delimiter first) *)
  | Pexp_ident { txt = Longident.Ldot (Lident "String", "split_on_char"); _ },
    [(_, c_arg); (_, s_arg)] ->
    emit e "String.split ";
    (* Convert char argument to string (delimiter comes first in MiniML) *)
    (match c_arg.pexp_desc with
     | Pexp_constant c ->
       (match c.pconst_desc with
        | Pconst_char ch ->
          let s = String.make 1 ch in
          emit e ("\"" ^ String.escaped s ^ "\"")
        | _ ->
          emit e "(String.make 1 ";
          emit_expr e c_arg;
          emit e ")")
     | _ ->
       emit e "(String.make 1 ";
       emit_expr e c_arg;
       emit e ")");
    emit e " ";
    if needs_parens_subexpr s_arg then emit e "(";
    emit_expr e s_arg;
    if needs_parens_subexpr s_arg then emit e ")"
  (* String.contains : string -> char -> bool  →  String.contains : string -> string -> bool
     OCaml: String.contains haystack char  →  MiniML: String.contains substring haystack *)
  | Pexp_ident { txt = Longident.Ldot (Lident "String", "contains"); _ },
    [(_, s_arg); (_, c_arg)] ->
    emit e "String.contains ";
    (* Emit the char/substring argument FIRST (MiniML takes sub, haystack) *)
    (match c_arg.pexp_desc with
     | Pexp_constant c ->
       (match c.pconst_desc with
        | Pconst_char ch ->
          let s = String.make 1 ch in
          emit e ("\"" ^ String.escaped s ^ "\"")
        | _ ->
          emit e "(String.make 1 ";
          emit_expr e c_arg;
          emit e ")")
     | _ ->
       emit e "(String.make 1 ";
       emit_expr e c_arg;
       emit e ")");
    emit e " ";
    (* Then emit the haystack argument *)
    if needs_parens_subexpr s_arg then emit e "(";
    emit_expr e s_arg;
    if needs_parens_subexpr s_arg then emit e ")"
  (* @ operator → List.concat *)
  | Pexp_ident { txt = Lident "@"; _ }, [(_, lhs); (_, rhs)] ->
    emit e "List.concat ";
    if needs_parens_subexpr lhs then emit e "(";
    emit_expr e lhs;
    if needs_parens_subexpr lhs then emit e ")";
    emit e " ";
    if needs_parens_subexpr rhs then emit e "(";
    emit_expr e rhs;
    if needs_parens_subexpr rhs then emit e ")"
  (* := operator → Ref.set *)
  | Pexp_ident { txt = Lident ":="; _ }, [(_, lhs); (_, rhs)] ->
    emit e "Ref.set ";
    if needs_parens_subexpr lhs then emit e "(";
    emit_expr e lhs;
    if needs_parens_subexpr lhs then emit e ")";
    emit e " ";
    if needs_parens_subexpr rhs then emit e "(";
    emit_expr e rhs;
    if needs_parens_subexpr rhs then emit e ")"
  (* Binary operators *)
  | Pexp_ident { txt = Lident op; _ }, [(_, lhs); (_, rhs)]
    when not (is_prefix_op op) && String.length op > 0 &&
         ((match op.[0] with
           | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '&' | '|' | '^'
           | ':' | '%' | '!' -> true
           | _ -> false)
          || List.mem op ["mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "||"; "&&"]) ->
    let needs_l = needs_parens_subexpr lhs in
    let needs_r = needs_parens_subexpr rhs in
    if needs_l then emit e "(";
    emit_expr e lhs;
    if needs_l then emit e ")";
    emit e (" " ^ translate_op op ^ " ");
    if needs_r then emit e "(";
    emit_expr e rhs;
    if needs_r then emit e ")"
  (* Prefix operators *)
  | Pexp_ident { txt = Lident op; _ }, [(_, arg)]
    when is_prefix_op op ->
    (match op with
     | "!" ->
       emit_expr e arg
     | "~-" | "~-." ->
       emit e "-(";
       emit_expr e arg;
       emit e ")"
     | "not" ->
       emit e "not ";
       if needs_parens_subexpr arg then emit e "(";
       emit_expr e arg;
       if needs_parens_subexpr arg then emit e ")"
     | "lnot" ->
       emit e "lnot ";
       emit_expr e arg
     | _ ->
       emit e op;
       emit e " ";
       emit_expr e arg)
  (* Normal function application *)
  | _ ->
    let needs_parens_fn = match fn.pexp_desc with
      | Pexp_function _ | Pexp_match _ | Pexp_ifthenelse _ | Pexp_let _ -> true
      | _ -> false
    in
    if needs_parens_fn then emit e "(";
    emit_expr e fn;
    if needs_parens_fn then emit e ")";
    List.iter (fun (_lbl, arg) ->
      emit e " ";
      let needs_parens = needs_parens_subexpr arg in
      if needs_parens then emit e "(";
      emit_expr e arg;
      if needs_parens then emit e ")"
    ) args

(* Emit match cases *)
and emit_cases e cases =
  List.iter (fun (case : case) ->
    emit e "| ";
    (* Handle char-related patterns that MiniML doesn't support natively.
       MiniML parser doesn't support rune literals in patterns, so we
       convert them to variable bindings + guard expressions. *)
    let char_counter = ref 0 in
    let char_guards = ref [] in
    let rec rewrite_char_pat (pat : pattern) : pattern =
      match pat.ppat_desc with
      | Ppat_constant c when (match c.pconst_desc with Pconst_char _ -> true | _ -> false) ->
        let var_name = if !char_counter = 0 then "_c" else Printf.sprintf "_c%d" !char_counter in
        incr char_counter;
        let sub = create_emitter () in
        emit sub (var_name ^ " = ");
        emit_constant sub c;
        char_guards := contents sub :: !char_guards;
        { pat with ppat_desc = Ppat_var { txt = var_name; loc = pat.ppat_loc } }
      | Ppat_interval (lo, hi) ->
        let var_name = if !char_counter = 0 then "_c" else Printf.sprintf "_c%d" !char_counter in
        incr char_counter;
        let sub = create_emitter () in
        emit sub (var_name ^ " >= ");
        emit_constant sub lo;
        emit sub (" && " ^ var_name ^ " <= ");
        emit_constant sub hi;
        char_guards := contents sub :: !char_guards;
        { pat with ppat_desc = Ppat_var { txt = var_name; loc = pat.ppat_loc } }
      | Ppat_tuple ps ->
        { pat with ppat_desc = Ppat_tuple (List.map rewrite_char_pat ps) }
      | Ppat_construct (lid, Some (vars, inner)) ->
        { pat with ppat_desc = Ppat_construct (lid, Some (vars, rewrite_char_pat inner)) }
      | Ppat_or (p1, p2) ->
        { pat with ppat_desc = Ppat_or (rewrite_char_pat p1, rewrite_char_pat p2) }
      | _ -> pat
    in
    let rewritten_pat = rewrite_char_pat case.pc_lhs in
    emit_pattern e rewritten_pat;
    let extra_guard = match !char_guards with
      | [] -> None
      | guards -> Some (String.concat " && " (List.rev guards))
    in
    (match extra_guard, case.pc_guard with
     | Some guard, None ->
       emit e (" when " ^ guard)
     | Some guard, Some user_guard ->
       emit e (" when " ^ guard ^ " && ");
       emit_expr e user_guard
     | None, Some guard ->
       emit e " when ";
       emit_expr e guard
     | None, None -> ());
    emit e " -> ";
    let body = case.pc_rhs in
    let is_multiline = match body.pexp_desc with
      | Pexp_let _ | Pexp_sequence _ | Pexp_match _
      | Pexp_ifthenelse _ -> true
      | _ -> false
    in
    let needs_wrap = ends_with_match body in
    if is_multiline then begin
      if needs_wrap then begin
        newline e;
        indent e;
        emit e "do";
        newline e;
        indent e;
        emit_expr e body;
        newline e;
        dedent e;
        emit_line e "end";
        dedent e
      end else begin
        newline e;
        indent e;
        emit_expr e body;
        newline e;
        dedent e
      end
    end else begin
      emit_expr e body;
      newline e
    end
  ) cases

(* Emit exception handler cases, translating exception constructors to effect operations.
   E.g., | Unify_error msg -> handler  becomes  | unify_error msg -> handler *)
and emit_exn_cases e cases =
  List.iter (fun (case : case) ->
    emit e "| ";
    (* Translate the exception pattern to an effect operation pattern *)
    (match case.pc_lhs.ppat_desc with
     | Ppat_construct ({ txt = Lident ctor; _ }, payload) ->
       let op_name = exn_to_effect_op ctor in
       emit e op_name;
       (match payload with
        | Some (_, pat) -> emit e " "; emit_pattern e pat
        | None -> ())
     | Ppat_construct ({ txt = Longident.Ldot (_, ctor); _ }, payload) ->
       let op_name = exn_to_effect_op ctor in
       emit e op_name;
       (match payload with
        | Some (_, pat) -> emit e " "; emit_pattern e pat
        | None -> ())
     | Ppat_any -> emit e "_"
     | _ -> emit_pattern e case.pc_lhs);
    Option.iter (fun guard ->
      emit e " when ";
      emit_expr e guard
    ) case.pc_guard;
    emit e " -> ";
    let is_multiline = match case.pc_rhs.pexp_desc with
      | Pexp_let _ | Pexp_sequence _ | Pexp_match _
      | Pexp_ifthenelse _ -> true
      | _ -> false
    in
    if is_multiline then begin
      newline e;
      indent e;
      emit_expr e case.pc_rhs;
      newline e;
      dedent e
    end else begin
      emit_expr e case.pc_rhs;
      newline e
    end
  ) cases

(* Emit let bindings *)
and emit_let_bindings e rec_flag bindings is_top =
  let kw = if rec_flag = Recursive then "let rec " else "let " in
  List.iteri (fun i (vb : value_binding) ->
    if i = 0 then begin
      if is_top then emit e "pub ";
      emit e kw
    end else begin
      emit e "and "
    end;
    (* Extract function name and parameters *)
    let (name_pat, params, ret_type, body) = decompose_fun_binding vb in
    begin
      emit_pattern e name_pat;
      List.iter (fun p ->
        emit e " ";
        emit_pattern e p
      ) params;
      Option.iter (fun ty ->
        emit e " : ";
        emit_type e ty
      ) ret_type;
      emit e " =";
      let is_multiline = match body.pexp_desc with
        | Pexp_let _ | Pexp_sequence _ | Pexp_match _
        | Pexp_ifthenelse _ | Pexp_function _ -> true
        | _ -> false
      in
      if is_multiline then begin
        newline e;
        indent e;
        emit_expr e body;
        dedent e
      end else begin
        emit e " ";
        emit_expr e body
      end
    end;
    if i < List.length bindings - 1 then newline e
  ) bindings

(* Decompose a value binding into name, params, return type, and body *)
and decompose_fun_binding (vb : value_binding) =
  let name = vb.pvb_pat in
  match vb.pvb_expr.pexp_desc with
  | Pexp_function (params, constraint_, body) ->
    let pats = List.filter_map (fun (p : function_param) ->
      match p.pparam_desc with
      | Pparam_val (_, _, pat) -> Some pat
      | Pparam_newtype _ -> None
    ) params in
    let ret_type = match constraint_ with
      | Some (Pconstraint ty) -> Some ty
      | _ -> None
    in
    (match body with
     | Pfunction_body e ->
       (name, pats, ret_type, e)
     | Pfunction_cases (cases, _, _) ->
       let fn_expr = { vb.pvb_expr with
         pexp_desc = Pexp_function ([], None, Pfunction_cases (cases, Location.none, []))
       } in
       (name, pats, ret_type, fn_expr))
  | Pexp_constraint (inner, ty) ->
    (name, [], Some ty, inner)
  | _ -> (name, [], None, vb.pvb_expr)

(* Emit a type declaration *)
and emit_type_decl e is_top ?(is_and=false) (td : type_declaration) =
  if is_top && not is_and then emit e "pub ";
  if is_and then emit e "and " else emit e "type ";
  (* Type parameters *)
  (match td.ptype_params with
   | [] -> ()
   | [(ct, _)] ->
     emit_type e ct;
     emit e " "
   | ps ->
     emit e "(";
     List.iteri (fun i (ct, _) ->
       if i > 0 then emit e ", ";
       emit_type e ct
     ) ps;
     emit e ") ");
  emit e td.ptype_name.txt;
  (match td.ptype_kind, td.ptype_manifest with
   (* type t = variant *)
   | Ptype_variant ctors, _ ->
     emit e " =";
     newline e;
     indent e;
     List.iter (fun (cd : constructor_declaration) ->
       emit e "| ";
       emit e cd.pcd_name.txt;
       (match cd.pcd_args with
        | Pcstr_tuple [] -> ()
        | Pcstr_tuple ts ->
          emit e " of ";
          List.iteri (fun i t ->
            if i > 0 then emit e " * ";
            let needs_parens = match t.ptyp_desc with
              | Ptyp_arrow _ | Ptyp_tuple _ -> true | _ -> false
            in
            if needs_parens then emit e "(";
            emit_type e t;
            if needs_parens then emit e ")"
          ) ts
        | Pcstr_record _ ->
          emit e " of (* TODO: inline record *)");
       (* GADT return type *)
       (match cd.pcd_res with
        | Some ret ->
          (match cd.pcd_args with
           | Pcstr_tuple [] ->
             emit e " : ";
             emit_type e ret
           | Pcstr_tuple ts ->
             (* Re-emit as GADT syntax: Ctor : arg -> ret *)
             (* We already emitted "of ...", so this needs restructuring *)
             (* For now, add a comment *)
             emit e " (* GADT: -> ";
             emit_type e ret;
             emit e " *)";
             ignore ts
           | _ -> ())
        | None -> ());
       newline e
     ) ctors;
     dedent e

   (* type t = { fields } *)
   | Ptype_record labels, _ ->
     emit e " = {";
     newline e;
     indent e;
     List.iteri (fun i (ld : label_declaration) ->
       if i > 0 then begin emit e ";"; newline e end;
       if ld.pld_mutable = Mutable then emit e "mut ";
       emit e ld.pld_name.txt;
       emit e ": ";
       emit_type e ld.pld_type
     ) labels;
     newline e;
     dedent e;
     emit e "}"

   (* type t = existing_type (alias) *)
   | Ptype_abstract, Some manifest ->
     emit e " = ";
     emit_type e manifest

   (* type t (abstract, no definition) *)
   | Ptype_abstract, None -> ()

   | Ptype_open, _ -> emit e " = ..")

(* --- Top-level structure items --- *)

let rec emit_structure_item e (item : structure_item) =
  match item.pstr_desc with
  | Pstr_value (rf, bindings) ->
    emit_let_bindings e rf bindings true;
    newline e

  | Pstr_type (_, tdecls) ->
    List.iteri (fun i td ->
      emit_type_decl e true ~is_and:(i > 0) td;
      if i < List.length tdecls - 1 then newline e
    ) tdecls;
    newline e

  | Pstr_exception ec ->
    let exn_name = ec.ptyexn_constructor.pext_name.txt in
    let effect_name = exn_to_effect_name exn_name in
    let op_name = exn_to_effect_op exn_name in
    emit e ("effect " ^ effect_name ^ " =");
    newline e;
    indent e;
    emit e (op_name ^ " : ");
    (match ec.ptyexn_constructor.pext_kind with
     | Pext_decl (_, Pcstr_tuple ts, _) ->
       if ts <> [] then begin
         List.iteri (fun i t ->
           if i > 0 then emit e " * ";
           emit_type e t
         ) ts
       end else
         emit e "unit"
     | _ -> emit e "unit");
    emit e " -> 'a";
    dedent e;
    newline e;
    emit e "end";
    newline e

  | Pstr_open od ->
    emit e "open ";
    (match od.popen_expr.pmod_desc with
     | Pmod_ident lid ->
       emit e (Format.asprintf "%a" Pprintast.longident lid.txt)
     | _ -> emit e "(* complex open *)");
    newline e

  | Pstr_eval (ex, _) ->
    emit_expr e ex;
    newline e

  | Pstr_module mb ->
    emit e "module ";
    (match mb.pmb_name.txt with
     | Some name -> emit e name
     | None -> emit e "_");
    emit e " =";
    newline e;
    indent e;
    (match mb.pmb_expr.pmod_desc with
     | Pmod_structure items ->
       List.iter (fun item ->
         emit_structure_item e item
       ) items
     | _ -> emit e "(* complex module expr *)");
    dedent e;
    emit e "end";
    newline e

  | Pstr_primitive vd ->
    emit e "pub extern ";
    emit e vd.pval_name.txt;
    emit e " : ";
    emit_type e vd.pval_type;
    newline e

  | Pstr_modtype _ -> emit_line e "(* TODO: module type *)"
  | Pstr_recmodule _ -> emit_line e "(* TODO: recursive module *)"
  | Pstr_class _ -> emit_line e "(* TODO: class *)"
  | Pstr_class_type _ -> emit_line e "(* TODO: class type *)"
  | Pstr_include _ -> emit_line e "(* TODO: include *)"
  | Pstr_attribute _ -> () (* skip attributes *)
  | Pstr_extension _ -> emit_line e "(* TODO: extension *)"
  | Pstr_typext _ -> emit_line e "(* TODO: type extension *)"

(* --- Main: parse and translate --- *)

let translate_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  let structure = Parse.implementation lexbuf in
  close_in ic;

  let e = create_emitter () in

  (* Derive module name from filename *)
  let basename = Filename.basename filename in
  let modname = Filename.chop_extension basename in
  let modname = String.capitalize_ascii modname in

  emit e ("module " ^ modname ^ " =");
  newline e;
  indent e;

  (* Add module-specific opens to resolve constructor ambiguity.
     In OCaml, each file is a separate compilation unit so constructors
     resolve via type inference. In MiniML's single namespace, the most
     recently defined module shadows earlier ones. *)
  let opens = match String.lowercase_ascii modname with
    | "lexer" | "parser" -> ["Token"]
    | "compiler" | "serialize" -> ["Bytecode"]
    | _ -> []
  in
  List.iter (fun m -> emit_line e ("open " ^ m)) opens;

  List.iter (fun item ->
    emit_structure_item e item;
    (* Add blank line between top-level items *)
    newline e
  ) structure;

  dedent e;
  emit_line e "end";

  print_string (contents e)

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] ->
    Printf.eprintf "Usage: ocaml_to_mml <file.ml> [file2.ml ...]\n";
    exit 1
  | files ->
    List.iter translate_file files
