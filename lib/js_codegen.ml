(** JS code generation: typed AST -> JavaScript source code.

    Walks a [Typechecker.tprogram] and emits JavaScript as a string.
    Phase 1: direct-style only (no effects/CPS/trampolines). *)

exception Codegen_error of string

let error msg = raise (Codegen_error msg)

(* ---- JS Runtime ---- *)

let js_runtime = {|"use strict";
function _call(f, args) {
  while (args.length > 0) {
    const a = f._arity !== undefined ? f._arity : f.length;
    if (a === 0) { f = f(); continue; }
    if (args.length < a) return _partial(f, a, args);
    const taken = args.splice(0, a);
    f = f.apply(null, taken);
  }
  return f;
}
function _partial(fn, arity, args) {
  const p = function() {
    return _call(fn, args.concat(Array.from(arguments)));
  };
  p._arity = arity - args.length;
  return p;
}
function _eq(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return a === b;
  if (typeof a !== "object") return false;
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!_eq(a[i], b[i])) return false;
    return true;
  }
  if ("_arr" in a && "_arr" in b) {
    if (a._arr.length !== b._arr.length) return false;
    for (let i = 0; i < a._arr.length; i++) if (!_eq(a._arr[i], b._arr[i])) return false;
    return true;
  }
  if ("_hd" in a && "_hd" in b) {
    let ca = a, cb = b;
    while (ca !== null && cb !== null) {
      if (!("_hd" in ca) || !("_hd" in cb)) return ca === cb;
      if (!_eq(ca._hd, cb._hd)) return false;
      ca = ca._tl; cb = cb._tl;
    }
    return ca === cb;
  }
  if ("_tag" in a) return a._tag === b._tag && _eq(a._val, b._val);
  if ("_ref" in a) return _eq(a._ref, b._ref);
  const ka = Object.keys(a), kb = Object.keys(b);
  if (ka.length !== kb.length) return false;
  for (const k of ka) if (!_eq(a[k], b[k])) return false;
  return true;
}
function _compare(a, b) {
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}
function _match_fail(loc) { throw new Error("Match failure at " + loc); }
function _pp(v) {
  if (v === undefined) return "()";
  if (v === null) return "[]";
  if (typeof v === "number") {
    if (!Number.isInteger(v)) {
      const s = String(v);
      return s.includes(".") || s.includes("e") ? s : s + ".";
    }
    return String(v);
  }
  if (typeof v === "boolean") return String(v);
  if (typeof v === "string") return v;
  if (Array.isArray(v)) return "(" + v.map(_pp).join(", ") + ")";
  if (v !== null && typeof v === "object") {
    if ("_arr" in v) return "#[" + v._arr.map(_pp).join("; ") + "]";
    if ("_hd" in v) {
      const r = [];
      let c = v;
      while (c !== null && typeof c === "object" && "_hd" in c) { r.push(_pp(c._hd)); c = c._tl; }
      return "[" + r.join("; ") + "]";
    }
    if ("_tag" in v) {
      if (v._val !== undefined) {
        const pv = _pp(v._val);
        if (Array.isArray(v._val) || (typeof v._val === "object" && v._val !== null && "_tag" in v._val && v._val._val !== undefined))
          return v._name + " (" + pv + ")";
        return v._name + " " + pv;
      }
      return v._name;
    }
    if ("_ref" in v) return "ref(" + _pp(v._ref) + ")";
    return "{ " + Object.entries(v).map(([k,val]) => k + " = " + _pp(val)).join("; ") + " }";
  }
  return String(v);
}
let _h = {};
function _bounce(fn) { fn._tramp = true; return fn; }
const _trampolining = new Set();
function _trampoline(fn, tag) {
  if (tag !== undefined && _trampolining.has(tag)) return fn();
  if (tag !== undefined) _trampolining.add(tag);
  try {
    let result = fn();
    while (typeof result === 'function' && result._tramp) {
      result = result();
    }
    return result;
  } finally { if (tag !== undefined) _trampolining.delete(tag); }
}
function _resolve(v) { while (typeof v === 'function' && v._tramp) v = v(); return v; }
|}

(* ---- Name mangling ---- *)

let js_reserved = [
  "abstract"; "arguments"; "await"; "boolean"; "break"; "byte"; "case"; "catch";
  "char"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do";
  "double"; "else"; "enum"; "eval"; "export"; "extends"; "false"; "final";
  "finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import";
  "in"; "instanceof"; "int"; "interface"; "let"; "long"; "native"; "new"; "null";
  "package"; "private"; "protected"; "public"; "return"; "short"; "static";
  "super"; "switch"; "synchronized"; "this"; "throw"; "throws"; "transient";
  "true"; "try"; "typeof"; "undefined"; "var"; "void"; "volatile"; "while";
  "with"; "yield"; "of"
]

let is_js_reserved name = List.mem name js_reserved

let mangle_name name =
  let buf = Buffer.create (String.length name) in
  String.iter (fun c ->
    match c with
    | '.' -> Buffer.add_char buf '$'
    | '\'' -> Buffer.add_string buf "$p"
    | '+' -> Buffer.add_string buf "$plus"
    | '-' -> Buffer.add_string buf "$minus"
    | '*' -> Buffer.add_string buf "$star"
    | '/' -> Buffer.add_string buf "$slash"
    | '^' -> Buffer.add_string buf "$caret"
    | '=' -> Buffer.add_string buf "$eq"
    | '<' -> Buffer.add_string buf "$lt"
    | '>' -> Buffer.add_string buf "$gt"
    | '!' -> Buffer.add_string buf "$bang"
    | ':' -> Buffer.add_string buf "$colon"
    | '~' -> Buffer.add_string buf "$tilde"
    | '#' -> Buffer.add_string buf "$hash"
    | c -> Buffer.add_char buf c
  ) name;
  let result = Buffer.contents buf in
  if is_js_reserved result then "_mml$" ^ result
  else result

(* ---- Context ---- *)

type ctx = {
  buf: Buffer.t;
  mutable indent: int;
  mutable scopes: (string, string) Hashtbl.t list;
  mutable tmp_counter: int;
  type_env: Types.type_env;
  mutable current_fn_name: string option;  (* for self-tail-call optimization *)
  mutable current_fn_params: string list;  (* param names for TCO *)
  mutable in_tail_position: bool;
  mutable tco_used: bool;  (* whether we emitted a continue for TCO *)
  mutable current_module: string option;  (* for qualifying extern names *)
  mutable in_cps: bool;  (* compiling in CPS mode *)
  mutable handler_tail_resume: bool;  (* skip _trampoline on tail resume in handler *)
  mutable direct_dispatch_ops: (string * string) list;  (* op_name -> direct JS fn *)
  mutable trywith_ops: string list;  (* ops that compile to throw *)
  mutable break_flag_name: string;  (* current while loop's break flag *)
  mutable break_val_name: string;   (* current while loop's break value *)
  mutable top_level_exports: (string * string) list;  (* (mml_name, js_name) pairs for harness *)
}

let create_ctx type_env =
  let tbl = Hashtbl.create 16 in
  {
    buf = Buffer.create 4096;
    indent = 0;
    scopes = [tbl];
    tmp_counter = 0;
    type_env;
    current_fn_name = None;
    current_fn_params = [];
    in_tail_position = false;
    tco_used = false;
    current_module = None;
    in_cps = false;
    handler_tail_resume = false;
    direct_dispatch_ops = [];
    trywith_ops = [];
    break_flag_name = "_break_flag";
    break_val_name = "_break_val";
    top_level_exports = [];
  }

let push_scope ctx =
  ctx.scopes <- Hashtbl.create 8 :: ctx.scopes

let pop_scope ctx =
  match ctx.scopes with
  | _ :: rest -> ctx.scopes <- rest
  | [] -> failwith "pop_scope: empty"

let bind_var ctx mml_name js_name =
  match ctx.scopes with
  | tbl :: _ -> Hashtbl.replace tbl mml_name js_name
  | [] -> failwith "bind_var: empty scope"

let lookup_var ctx name =
  let rec search = function
    | [] -> mangle_name name  (* global/extern *)
    | tbl :: rest ->
      match Hashtbl.find_opt tbl name with
      | Some js -> js
      | None -> search rest
  in
  search ctx.scopes

let fresh_tmp ctx =
  let n = ctx.tmp_counter in
  ctx.tmp_counter <- n + 1;
  Printf.sprintf "_t%d" n

(* Deduplicate JS parameter names by appending _2, _3, etc. *)
let dedup_js_params params =
  let seen = Hashtbl.create 8 in
  List.map (fun p ->
    let count = match Hashtbl.find_opt seen p with
      | Some n -> n | None -> 0 in
    Hashtbl.replace seen p (count + 1);
    if count = 0 then p
    else p ^ "_" ^ string_of_int (count + 1)
  ) params

let emit ctx s = Buffer.add_string ctx.buf s
let emit_indent ctx =
  for _ = 1 to ctx.indent do Buffer.add_string ctx.buf "  " done
let emit_line ctx s =
  emit_indent ctx; emit ctx s; emit ctx "\n"

(* ---- String escaping ---- *)

let escape_js_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.code c < 32 -> Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* ---- Tag lookup ---- *)

let tag_for_constructor type_env name =
  match List.assoc_opt name type_env.Types.constructors with
  | None -> error (Printf.sprintf "unknown constructor: %s" name)
  | Some info ->
    let (_, _, variant_def, _) = List.find (fun (n, _, _, _) ->
      String.equal n info.Types.ctor_type_name) type_env.Types.variants in
    let short_name = match String.rindex_opt name '.' with
      | Some i -> String.sub name (i + 1) (String.length name - i - 1)
      | None -> name
    in
    let rec find_tag idx = function
      | [] -> error (Printf.sprintf "constructor %s not found in variant" name)
      | (cname, _) :: _ when String.equal cname short_name -> idx
      | _ :: rest -> find_tag (idx + 1) rest
    in
    find_tag 0 variant_def

let short_constructor_name name =
  match String.rindex_opt name '.' with
  | Some i -> String.sub name (i + 1) (String.length name - i - 1)
  | None -> name

let is_newtype_ctor type_env name =
  match List.assoc_opt name type_env.Types.constructors with
  | Some info -> List.mem info.Types.ctor_type_name type_env.Types.newtypes
  | None -> false

(* ---- Effect analysis ---- *)

(* Does an expression directly contain TEPerform? *)
let rec expr_has_perform (te : Typechecker.texpr) =
  match te.expr with
  | Typechecker.TEPerform _ -> true
  | Typechecker.TEResume _ -> true
  | Typechecker.TELet (_, _, e1, e2)
  | Typechecker.TESeq (e1, e2) -> expr_has_perform e1 || expr_has_perform e2
  | Typechecker.TEIf (cond, e1, e2) ->
    expr_has_perform cond || expr_has_perform e1 || expr_has_perform e2
  | Typechecker.TEMatch (scrut, arms, _) ->
    expr_has_perform scrut ||
    List.exists (fun (_, g, body) ->
      expr_has_perform body ||
      (match g with Some g -> expr_has_perform g | None -> false)
    ) arms
  | Typechecker.TELetRec (_, _, fn_e, body) ->
    expr_has_perform fn_e || expr_has_perform body
  | Typechecker.TELetRecAnd (bindings, body) ->
    List.exists (fun (_, e) -> expr_has_perform e) bindings || expr_has_perform body
  | Typechecker.TEFun (_, body, _) -> expr_has_perform body
  | Typechecker.TEApp (fn, arg) -> expr_has_perform fn || expr_has_perform arg
  | Typechecker.TEHandle (body, arms) ->
    (* Handle is a CPS boundary, but check for performs in handler bodies *)
    expr_has_perform body ||
    List.exists (fun arm -> match arm with
      | Typechecker.THReturn (_, e) | Typechecker.THOp (_, _, _, e)
      | Typechecker.THOpProvide (_, _, e) | Typechecker.THOpTry (_, _, e) ->
        expr_has_perform e
    ) arms
  | Typechecker.TELetMut (_, e1, e2) -> expr_has_perform e1 || expr_has_perform e2
  | Typechecker.TEWhile (c, b) -> expr_has_perform c || expr_has_perform b
  | Typechecker.TECons (e1, e2) -> expr_has_perform e1 || expr_has_perform e2
  | Typechecker.TETuple es -> List.exists expr_has_perform es
  | Typechecker.TERecord fields -> List.exists (fun (_, e) -> expr_has_perform e) fields
  | Typechecker.TEBinop (_, e1, e2) -> expr_has_perform e1 || expr_has_perform e2
  | Typechecker.TEUnop (_, e) -> expr_has_perform e
  | _ -> false

(* Check if all TEResume in body are in tail position (safe to skip _trampoline) *)
let rec all_resumes_are_tail (te : Typechecker.texpr) =
  match te.expr with
  | Typechecker.TEResume _ -> true
  | Typechecker.TESeq (_, e2) -> all_resumes_are_tail e2
  | Typechecker.TELet (_, _, e1, e2) ->
    not (expr_has_perform e1) && all_resumes_are_tail e2
  | Typechecker.TELetRec (_, _, _, e2) -> all_resumes_are_tail e2
  | Typechecker.TELetMut (_, e1, e2) ->
    not (expr_has_perform e1) && all_resumes_are_tail e2
  | Typechecker.TEIf (_, then_e, else_e) ->
    all_resumes_are_tail then_e && all_resumes_are_tail else_e
  | Typechecker.TEMatch (_, arms, _) ->
    List.for_all (fun (_, _, body) -> all_resumes_are_tail body) arms
  | _ -> true  (* No resume — safe *)

(* Like expr_has_perform but ignores performs for ops in handled_ops *)
let rec expr_has_unhandled_perform handled_ops (te : Typechecker.texpr) =
  match te.expr with
  | Typechecker.TEPerform (op_name, _) -> not (List.mem op_name handled_ops)
  | Typechecker.TEResume _ -> true
  | Typechecker.TELet (_, _, e1, e2)
  | Typechecker.TESeq (e1, e2) ->
    expr_has_unhandled_perform handled_ops e1 || expr_has_unhandled_perform handled_ops e2
  | Typechecker.TEIf (cond, e1, e2) ->
    expr_has_unhandled_perform handled_ops cond ||
    expr_has_unhandled_perform handled_ops e1 ||
    expr_has_unhandled_perform handled_ops e2
  | Typechecker.TEMatch (scrut, arms, _) ->
    expr_has_unhandled_perform handled_ops scrut ||
    List.exists (fun (_, g, body) ->
      expr_has_unhandled_perform handled_ops body ||
      (match g with Some g -> expr_has_unhandled_perform handled_ops g | None -> false)
    ) arms
  | Typechecker.TELetRec (_, _, fn_e, body) ->
    expr_has_unhandled_perform handled_ops fn_e ||
    expr_has_unhandled_perform handled_ops body
  | Typechecker.TELetRecAnd (bindings, body) ->
    List.exists (fun (_, e) -> expr_has_unhandled_perform handled_ops e) bindings ||
    expr_has_unhandled_perform handled_ops body
  | Typechecker.TEFun (_, body, _) -> expr_has_unhandled_perform handled_ops body
  | Typechecker.TEApp (fn, arg) ->
    expr_has_unhandled_perform handled_ops fn ||
    expr_has_unhandled_perform handled_ops arg
  | Typechecker.TEHandle (body, arms) ->
    expr_has_unhandled_perform handled_ops body ||
    List.exists (fun arm -> match arm with
      | Typechecker.THReturn (_, e) | Typechecker.THOp (_, _, _, e)
      | Typechecker.THOpProvide (_, _, e) | Typechecker.THOpTry (_, _, e) ->
        expr_has_unhandled_perform handled_ops e
    ) arms
  | Typechecker.TELetMut (_, e1, e2) ->
    expr_has_unhandled_perform handled_ops e1 ||
    expr_has_unhandled_perform handled_ops e2
  | Typechecker.TEWhile (c, b) ->
    expr_has_unhandled_perform handled_ops c ||
    expr_has_unhandled_perform handled_ops b
  | Typechecker.TECons (e1, e2) ->
    expr_has_unhandled_perform handled_ops e1 ||
    expr_has_unhandled_perform handled_ops e2
  | Typechecker.TETuple es -> List.exists (expr_has_unhandled_perform handled_ops) es
  | Typechecker.TERecord fields ->
    List.exists (fun (_, e) -> expr_has_unhandled_perform handled_ops e) fields
  | Typechecker.TEBinop (_, e1, e2) ->
    expr_has_unhandled_perform handled_ops e1 ||
    expr_has_unhandled_perform handled_ops e2
  | Typechecker.TEUnop (_, e) -> expr_has_unhandled_perform handled_ops e
  | _ -> false

(* ---- Expression compilation ---- *)

let is_exportable_name name =
  (* Skip compiler-generated names like __destruct, and internal names *)
  not (String.length name >= 2 && name.[0] = '_' && name.[1] = '_')

(* compile_expr returns a JS expression string *)
let rec compile_non_tail ctx te =
  let saved = ctx.in_tail_position in
  ctx.in_tail_position <- false;
  let r = compile_expr ctx te in
  ctx.in_tail_position <- saved;
  r

and compile_expr ctx (te : Typechecker.texpr) : string =
  match te.expr with
  | Typechecker.TEInt n ->
    if n < 0 then Printf.sprintf "(%d)" n
    else string_of_int n
  | Typechecker.TEFloat f ->
    let s = Printf.sprintf "%.17g" f in
    if String.contains s '.' || String.contains s 'e' || String.contains s 'i'
    then s else s ^ ".0"
  | Typechecker.TEBool true -> "true"
  | Typechecker.TEBool false -> "false"
  | Typechecker.TEString s -> escape_js_string s
  | Typechecker.TEByte n -> string_of_int n
  | Typechecker.TERune n -> string_of_int n
  | Typechecker.TEUnit -> "undefined"
  | Typechecker.TENil -> "null"

  | Typechecker.TEVar name -> lookup_var ctx name

  | Typechecker.TELet (name, _scheme, e1, e2) ->
    compile_let ctx name e1 e2

  | Typechecker.TELetRec (name, _scheme, fn_expr, body) ->
    compile_letrec ctx name fn_expr body

  | Typechecker.TELetRecAnd (bindings, body) ->
    compile_letrec_and ctx bindings body

  | Typechecker.TEFun (param, body, _has_return) ->
    compile_fun ctx param body

  | Typechecker.TEApp (fn, arg) ->
    compile_app ctx te fn arg

  | Typechecker.TEBinop (op, e1, e2) ->
    compile_binop ctx op e1 e2

  | Typechecker.TEUnop (op, e) ->
    compile_unop ctx op e

  | Typechecker.TEIf (cond, then_e, else_e) ->
    compile_if ctx cond then_e else_e

  | Typechecker.TETuple exprs ->
    let elts = List.map (compile_non_tail ctx) exprs in
    "[" ^ String.concat ", " elts ^ "]"

  | Typechecker.TERecord fields ->
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    let field_strs = List.map (fun (name, e) ->
      let js_name = mangle_name name in
      let v = compile_non_tail ctx e in
      if js_name = name then name ^ ": " ^ v
      else Printf.sprintf "%s: %s" js_name v
    ) sorted in
    "({" ^ String.concat ", " field_strs ^ "})"

  | Typechecker.TERecordUpdate (base, overrides) ->
    let base_js = compile_non_tail ctx base in
    let field_strs = List.map (fun (name, e) ->
      let js_name = mangle_name name in
      js_name ^ ": " ^ compile_non_tail ctx e
    ) overrides in
    "({..." ^ base_js ^ ", " ^ String.concat ", " field_strs ^ "})"

  | Typechecker.TERecordUpdateIdx (base, pairs) ->
    let tmp = fresh_tmp ctx in
    let base_js = compile_non_tail ctx base in
    let pair_strs = List.map (fun (idx_e, val_e) ->
      let idx_js = compile_non_tail ctx idx_e in
      let val_js = compile_non_tail ctx val_e in
      Printf.sprintf "[%s]: %s" idx_js val_js
    ) pairs in
    Printf.sprintf "(%s = {...%s, %s}, %s)" tmp base_js (String.concat ", " pair_strs) tmp

  | Typechecker.TEField (e, field) ->
    let base = compile_non_tail ctx e in
    base ^ "." ^ mangle_name field

  | Typechecker.TEIndex (base_e, idx_e) ->
    let base = compile_non_tail ctx base_e in
    let idx = compile_non_tail ctx idx_e in
    let resolved = Types.repr base_e.ty in
    (match resolved with
     | Types.TString -> base ^ ".charCodeAt(" ^ idx ^ ")"
     | Types.TArray _ -> base ^ "._arr[" ^ idx ^ "]"
     | _ -> base ^ "[" ^ idx ^ "]")

  | Typechecker.TECons (hd, tl) ->
    let hd_js = compile_non_tail ctx hd in
    let tl_js = compile_non_tail ctx tl in
    "({_hd: " ^ hd_js ^ ", _tl: " ^ tl_js ^ "})"

  | Typechecker.TEConstruct (name, arg) ->
    if is_newtype_ctor ctx.type_env name then begin
      (* Newtype constructor: erased at runtime *)
      match arg with
      | Some e -> compile_non_tail ctx e
      | None -> "undefined"
    end else begin
      let tag = if String.length name > 0 && name.[0] = '`' then
        Hashtbl.hash (String.sub name 1 (String.length name - 1)) land 0x3FFFFFFF
      else
        tag_for_constructor ctx.type_env name
      in
      let sname = short_constructor_name name in
      (match arg with
       | Some e ->
         let v = compile_non_tail ctx e in
         Printf.sprintf "({_tag: %d, _name: %s, _val: %s})" tag (escape_js_string sname) v
       | None ->
         Printf.sprintf "({_tag: %d, _name: %s})" tag (escape_js_string sname))
    end

  | Typechecker.TEMatch (scrut, arms, _partial) ->
    compile_match ctx scrut arms te.loc

  | Typechecker.TESeq (e1, e2) ->
    compile_seq ctx e1 e2

  | Typechecker.TELetMut (name, init, body) ->
    compile_let_mut ctx name init body

  | Typechecker.TEAssign (name, e) ->
    let js_name = lookup_var ctx name in
    let v = compile_non_tail ctx e in
    "(" ^ js_name ^ " = " ^ v ^ ", undefined)"

  | Typechecker.TEFieldAssign (record_e, field, value_e) ->
    let base = compile_non_tail ctx record_e in
    let v = compile_non_tail ctx value_e in
    "(" ^ base ^ "." ^ mangle_name field ^ " = " ^ v ^ ", undefined)"

  | Typechecker.TEWhile (cond, body) ->
    compile_while ctx cond body

  | Typechecker.TEBreak value_te ->
    let v = compile_non_tail ctx value_te in
    "(" ^ ctx.break_val_name ^ " = " ^ v ^ ", " ^ ctx.break_flag_name ^ " = true, undefined)"

  | Typechecker.TEContinueLoop ->
    "(_cont_flag = true, undefined)"

  | Typechecker.TEFoldContinue value_te ->
    let v = compile_non_tail ctx value_te in
    "(_fold_cont_val = " ^ v ^ ", _fold_cont_flag = true, undefined)"

  | Typechecker.TEForLoop fold_te ->
    compile_expr ctx fold_te

  | Typechecker.TEArray elts ->
    let elt_strs = List.map (compile_non_tail ctx) elts in
    "{_arr: [" ^ String.concat ", " elt_strs ^ "]}"

  | Typechecker.TEReturn value_te ->
    let v = compile_non_tail ctx value_te in
    "(_ret_val = " ^ v ^ ", _ret_flag = true, undefined)"

  | Typechecker.TEPerform (op_name, arg) ->
    (match List.assoc_opt op_name ctx.direct_dispatch_ops with
     | Some direct_fn ->
       (* Simple handler direct dispatch — no CPS, no trampoline *)
       let arg_js = compile_non_tail ctx arg in
       direct_fn ^ "(" ^ arg_js ^ ")"
     | None when List.mem op_name ctx.trywith_ops ->
       (* Try/with — throw to be caught at handle boundary *)
       let arg_js = compile_non_tail ctx arg in
       let result = fresh_tmp ctx in
       emit_line ctx (Printf.sprintf "throw {_e: \"%s\", _v: %s};" op_name arg_js);
       emit_line ctx (Printf.sprintf "const %s = undefined;" result);
       result
     | None ->
       (* Perform in direct-style context — trampoline to flatten handler bounces *)
       let arg_js = compile_non_tail ctx arg in
       let result = fresh_tmp ctx in
       emit_line ctx (Printf.sprintf
         "const %s = _trampoline(function() { return _h[\"%s\"](%s, function(_r) { return _r; }); });"
         result op_name arg_js);
       result)

  | Typechecker.TEHandle (body, arms) ->
    compile_handle ctx body arms

  | Typechecker.TEResume (k_expr, val_expr) ->
    let k_js = compile_non_tail ctx k_expr in
    let v_js = compile_non_tail ctx val_expr in
    if ctx.handler_tail_resume then
      (* Tail resume in handler — no trampoline, bounce wrapper handles it *)
      k_js ^ "(" ^ v_js ^ ")"
    else
      (* Non-tail resume — trampoline to synchronously process k's bounces *)
      "_trampoline(function() { return " ^ k_js ^ "(" ^ v_js ^ "); })"

(* ---- Let bindings ---- *)

and compile_let ctx name e1 e2 =
  let v1 = compile_non_tail ctx e1 in
  let js_name = mangle_name name in
  (* Use a unique name to avoid const redeclaration in same JS block *)
  let actual_name = if js_name = "_" then fresh_tmp ctx
    else js_name ^ "_" ^ string_of_int ctx.tmp_counter ^ "" |> fun n ->
      ctx.tmp_counter <- ctx.tmp_counter + 1; n
  in
  push_scope ctx;
  bind_var ctx name actual_name;
  let result = fresh_tmp ctx in
  emit_line ctx (Printf.sprintf "const %s = %s;" actual_name v1);
  let v2 = compile_expr ctx e2 in
  emit_line ctx (Printf.sprintf "const %s = %s;" result v2);
  pop_scope ctx;
  result

and emit_js_placeholder ctx js_name te =
  let rec go te =
    match te.Typechecker.expr with
    | Typechecker.TECons _ ->
      emit_line ctx (Printf.sprintf "let %s = {_hd: null, _tl: null};" js_name)
    | Typechecker.TETuple es ->
      let nulls = String.concat ", " (List.map (fun _ -> "null") es) in
      emit_line ctx (Printf.sprintf "let %s = [%s];" js_name nulls)
    | Typechecker.TERecord fields ->
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
      let field_strs = List.map (fun (name, _) ->
        mangle_name name ^ ": null") sorted in
      emit_line ctx (Printf.sprintf "let %s = {%s};" js_name (String.concat ", " field_strs))
    | Typechecker.TEConstruct (name, payload_opt) ->
      if is_newtype_ctor ctx.type_env name then
        (match payload_opt with
         | Some inner -> go inner
         | None -> error "cannot create placeholder for nullary newtype constructor")
      else begin
        let tag = if String.length name > 0 && name.[0] = '`' then
          Hashtbl.hash (String.sub name 1 (String.length name - 1)) land 0x3FFFFFFF
        else tag_for_constructor ctx.type_env name
        in
        let sname = short_constructor_name name in
        (match payload_opt with
         | Some _ ->
           emit_line ctx (Printf.sprintf "let %s = {_tag: %d, _name: %s, _val: null};"
             js_name tag (escape_js_string sname))
         | None ->
           emit_line ctx (Printf.sprintf "let %s = {_tag: %d, _name: %s};"
             js_name tag (escape_js_string sname)))
      end
    | Typechecker.TEArray es ->
      let nulls = String.concat ", " (List.map (fun _ -> "null") es) in
      emit_line ctx (Printf.sprintf "let %s = {_arr: [%s]};" js_name nulls)
    | Typechecker.TELet (_, _, _, inner) | Typechecker.TESeq (_, inner) -> go inner
    | _ ->
      emit_line ctx (Printf.sprintf "let %s = null;" js_name)
  in
  go te

and emit_js_backpatch ctx js_name te =
  let rec go te =
    match te.Typechecker.expr with
    | Typechecker.TECons _ ->
      let computed = compile_non_tail ctx te in
      emit_line ctx (Printf.sprintf "%s._hd = %s._hd; %s._tl = %s._tl;"
        js_name computed js_name computed)
    | Typechecker.TETuple _ ->
      let computed = compile_non_tail ctx te in
      let tmp = fresh_tmp ctx in
      emit_line ctx (Printf.sprintf "const %s = %s;" tmp computed);
      emit_line ctx (Printf.sprintf "for (let _i = 0; _i < %s.length; _i++) %s[_i] = %s[_i];"
        tmp js_name tmp)
    | Typechecker.TERecord _ ->
      let computed = compile_non_tail ctx te in
      emit_line ctx (Printf.sprintf "Object.assign(%s, %s);" js_name computed)
    | Typechecker.TEConstruct (name, _) when is_newtype_ctor ctx.type_env name ->
      go te  (* newtype is erased — backpatch the underlying *)
    | Typechecker.TEConstruct _ ->
      let computed = compile_non_tail ctx te in
      let tmp = fresh_tmp ctx in
      emit_line ctx (Printf.sprintf "const %s = %s;" tmp computed);
      emit_line ctx (Printf.sprintf
        "%s._tag = %s._tag; %s._name = %s._name; if (\"_val\" in %s) %s._val = %s._val;"
        js_name tmp js_name tmp tmp js_name tmp)
    | Typechecker.TEArray _ ->
      let computed = compile_non_tail ctx te in
      let tmp = fresh_tmp ctx in
      emit_line ctx (Printf.sprintf "const %s = %s;" tmp computed);
      emit_line ctx (Printf.sprintf
        "for (let _i = 0; _i < %s._arr.length; _i++) %s._arr[_i] = %s._arr[_i];"
        tmp js_name tmp)
    | Typechecker.TELet (_, _, _, _inner) | Typechecker.TESeq (_, _inner) ->
      (* Compile the whole expression, but backpatch based on inner shape *)
      let computed = compile_non_tail ctx te in
      emit_line ctx (Printf.sprintf "Object.assign(%s, %s);" js_name computed)
    | _ ->
      let computed = compile_non_tail ctx te in
      emit_line ctx (Printf.sprintf "Object.assign(%s, %s);" js_name computed)
  in
  go te

and compile_letrec ctx name fn_expr body =
  let js_name = mangle_name name in
  push_scope ctx;
  bind_var ctx name js_name;
  (match fn_expr.Typechecker.expr with
   | Typechecker.TEFun _ ->
     compile_named_function ctx js_name fn_expr
   | _ ->
     emit_js_placeholder ctx js_name fn_expr;
     emit_js_backpatch ctx js_name fn_expr);
  let result = fresh_tmp ctx in
  let v = compile_expr ctx body in
  emit_line ctx (Printf.sprintf "const %s = %s;" result v);
  pop_scope ctx;
  result

and compile_letrec_and ctx bindings body =
  push_scope ctx;
  (* Bind all names and emit placeholders for non-function bindings *)
  List.iter (fun (name, te) ->
    let js_name = mangle_name name in
    bind_var ctx name js_name;
    match te.Typechecker.expr with
    | Typechecker.TEFun _ -> ()
    | _ -> emit_js_placeholder ctx js_name te
  ) bindings;
  (* Compile functions first, then backpatch values *)
  List.iter (fun (name, fn_expr) ->
    let js_name = mangle_name name in
    match fn_expr.Typechecker.expr with
    | Typechecker.TEFun _ -> compile_named_function ctx js_name fn_expr
    | _ -> ()
  ) bindings;
  List.iter (fun (name, te) ->
    let js_name = mangle_name name in
    match te.Typechecker.expr with
    | Typechecker.TEFun _ -> ()
    | _ -> emit_js_backpatch ctx js_name te
  ) bindings;
  let result = fresh_tmp ctx in
  let v = compile_expr ctx body in
  emit_line ctx (Printf.sprintf "const %s = %s;" result v);
  pop_scope ctx;
  result

(* ---- Functions ---- *)

and compile_fun ctx param body =
  (* Flatten TEFun chains into multi-arg function *)
  let rec collect_params params body_expr =
    match body_expr.Typechecker.expr with
    | Typechecker.TEFun (p, inner, _) ->
      collect_params (p :: params) inner
    | _ -> (List.rev params, body_expr)
  in
  let (all_params, final_body) = collect_params [param] body in
  let js_params = dedup_js_params (List.map mangle_name all_params) in
  let fn_name = fresh_tmp ctx in
  emit_indent ctx;
  emit ctx (Printf.sprintf "function %s(%s) " fn_name (String.concat ", " js_params));
  emit ctx "{\n";
  ctx.indent <- ctx.indent + 1;
  push_scope ctx;
  List.iter2 (fun mml_name js_name -> bind_var ctx mml_name js_name) all_params js_params;
  let saved_fn = ctx.current_fn_name in
  let saved_params = ctx.current_fn_params in
  let saved_tco = ctx.tco_used in
  ctx.current_fn_name <- None;
  ctx.current_fn_params <- [];
  ctx.tco_used <- false;
  (* Functions are compiled independently — clear handle-local optimizations
     so performs go through _h (which has CPS wrappers for stack safety) *)
  let saved_ddo = ctx.direct_dispatch_ops in
  let saved_two = ctx.trywith_ops in
  ctx.direct_dispatch_ops <- [];
  ctx.trywith_ops <- [];
  if expr_has_perform final_body then begin
    (* CPS-compile function body for effect support, wrapped in trampoline *)
    emit_line ctx "return _trampoline(function() {";
    ctx.indent <- ctx.indent + 1;
    compile_cps ctx final_body (fun v ->
      emit_line ctx (Printf.sprintf "return %s;" v));
    ctx.indent <- ctx.indent - 1;
    emit_line ctx "});"
  end else begin
    let v = compile_expr ctx final_body in
    emit_line ctx (Printf.sprintf "return %s;" v)
  end;
  ctx.direct_dispatch_ops <- saved_ddo;
  ctx.trywith_ops <- saved_two;
  ctx.current_fn_name <- saved_fn;
  ctx.current_fn_params <- saved_params;
  ctx.tco_used <- saved_tco;
  pop_scope ctx;
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "}";
  fn_name

and compile_named_function ctx js_name fn_expr =
  let rec collect_params params body_expr =
    match body_expr.Typechecker.expr with
    | Typechecker.TEFun (p, inner, _) ->
      collect_params (p :: params) inner
    | _ -> (List.rev params, body_expr)
  in
  match fn_expr.Typechecker.expr with
  | Typechecker.TEFun (param, body, has_return) ->
    let (all_params, final_body) = collect_params [param] body in
    let js_params = dedup_js_params (List.map mangle_name all_params) in
    (* Emit function header *)
    emit_indent ctx;
    emit ctx (Printf.sprintf "function %s(%s) " js_name (String.concat ", " js_params));
    emit ctx "{\n";
    ctx.indent <- ctx.indent + 1;
    push_scope ctx;
    List.iter2 (fun mml_name js_nm -> bind_var ctx mml_name js_nm) all_params js_params;
    let saved_fn = ctx.current_fn_name in
    let saved_params = ctx.current_fn_params in
    let saved_tco = ctx.tco_used in
    ctx.current_fn_name <- Some js_name;
    ctx.current_fn_params <- js_params;
    ctx.tco_used <- false;
    ctx.in_tail_position <- true;
    (* Functions are compiled independently — clear handle-local optimizations *)
    let saved_ddo = ctx.direct_dispatch_ops in
    let saved_two = ctx.trywith_ops in
    ctx.direct_dispatch_ops <- [];
    ctx.trywith_ops <- [];
    (* Record position where body starts *)
    let body_code_start = Buffer.length ctx.buf in
    if expr_has_perform final_body then begin
      (* CPS-compile function body for effect support, wrapped in tagged trampoline.
         Tag = function name, so recursive calls become pass-throughs (stack-safe). *)
      emit_line ctx (Printf.sprintf "return _trampoline(function() {" );
      ctx.indent <- ctx.indent + 1;
      compile_cps ctx final_body (fun v ->
        emit_line ctx (Printf.sprintf "return %s;" v));
      ctx.indent <- ctx.indent - 1;
      emit_line ctx (Printf.sprintf "}, %s);" js_name)
    end else if has_return then begin
      emit_line ctx "let _ret_flag = false, _ret_val;";
      let v = compile_expr ctx final_body in
      emit_line ctx (Printf.sprintf "return _ret_flag ? _ret_val : %s;" v)
    end else begin
      let v = compile_expr ctx final_body in
      emit_line ctx (Printf.sprintf "return %s;" v)
    end;
    let tco_was_used = ctx.tco_used in
    ctx.direct_dispatch_ops <- saved_ddo;
    ctx.trywith_ops <- saved_two;
    ctx.current_fn_name <- saved_fn;
    ctx.current_fn_params <- saved_params;
    ctx.tco_used <- saved_tco;
    ctx.in_tail_position <- false;
    pop_scope ctx;
    ctx.indent <- ctx.indent - 1;
    (* If TCO was used, wrap the body in while(true) *)
    if tco_was_used then begin
      let full_code = Buffer.contents ctx.buf in
      let body_code = String.sub full_code body_code_start (String.length full_code - body_code_start) in
      let before_body = String.sub full_code 0 body_code_start in
      Buffer.clear ctx.buf;
      Buffer.add_string ctx.buf before_body;
      let indent_str = String.make ((ctx.indent + 1) * 2) ' ' in
      Buffer.add_string ctx.buf (indent_str ^ "while (true) {\n");
      let body_lines = String.split_on_char '\n' body_code in
      List.iter (fun line ->
        if String.length line > 0 then
          Buffer.add_string ctx.buf ("  " ^ line ^ "\n")
      ) body_lines;
      Buffer.add_string ctx.buf (indent_str ^ "}\n");
    end;
    emit_line ctx "}"
  | _ ->
    let v = compile_expr ctx fn_expr in
    emit_line ctx (Printf.sprintf "const %s = %s;" js_name v)

(* ---- Function application ---- *)

and compile_app ctx _te fn arg =
  (* Collect application chain: f a b c -> (f, [a; b; c]) *)
  let rec collect_args expr acc =
    match expr.Typechecker.expr with
    | Typechecker.TEApp (inner_fn, inner_arg) ->
      collect_args inner_fn (inner_arg :: acc)
    | _ -> (expr, acc)
  in
  let (base_fn, all_args) = collect_args fn [arg] in
  let n = List.length all_args in
  (* Check for self-tail-call optimization *)
  let is_self_call = match ctx.current_fn_name, base_fn.Typechecker.expr with
    | Some fn_name, Typechecker.TEVar var_name ->
      String.equal (mangle_name var_name) fn_name &&
      n = List.length ctx.current_fn_params
    | _ -> false
  in
  if is_self_call && ctx.in_tail_position then begin
    (* Self-tail-call: emit parameter reassignment *)
    ctx.tco_used <- true;
    let arg_tmps = List.map (fun a ->
      let tmp = fresh_tmp ctx in
      let v = compile_non_tail ctx a in
      emit_line ctx (Printf.sprintf "const %s = %s;" tmp v);
      tmp
    ) all_args in
    List.iter2 (fun param tmp ->
      emit_line ctx (Printf.sprintf "%s = %s;" param tmp)
    ) ctx.current_fn_params arg_tmps;
    emit_line ctx "continue;";
    "undefined" (* unreachable, but we need to return something *)
  end else begin
    let fn_js = compile_non_tail ctx base_fn in
    let args_js = List.map (compile_non_tail ctx) all_args in
    (* Check if we know the arity matches *)
    let known_arity = count_arrows base_fn.ty in
    (* Detect dict/evidence args — their presence means the function's type
       may not include arrows for these implicit params, so we must use _call
       for correct partial application *)
    let has_implicit_args = List.exists (fun (a : Typechecker.texpr) ->
      match a.expr with
      | Typechecker.TEVar name ->
        (String.length name >= 7 && String.sub name 0 7 = "__dict_") ||
        (String.length name >= 5 && String.sub name 0 5 = "__ev_")
      | _ -> false
    ) all_args in
    if known_arity = Some n && not has_implicit_args then
      (* Direct call *)
      fn_js ^ "(" ^ String.concat ", " args_js ^ ")"
    else
      (* Use _call for safety *)
      "_call(" ^ fn_js ^ ", [" ^ String.concat ", " args_js ^ "])"
  end

and count_arrows ty =
  let rec go ty n =
    match Types.repr ty with
    | Types.TArrow (_, _, ret) -> go ret (n + 1)
    | _ -> n
  in
  let n = go ty 0 in
  if n > 0 then Some n else None

(* ---- Binary operators ---- *)

and compile_binop ctx op e1 e2 =
  match op with
  | Ast.And ->
    let a = compile_non_tail ctx e1 in
    let buf_before = Buffer.length ctx.buf in
    let b = compile_non_tail ctx e2 in
    if Buffer.length ctx.buf = buf_before then
      (* e2 is pure — safe to use && directly *)
      "(" ^ a ^ " && " ^ b ^ ")"
    else begin
      (* e2 emitted statements — need if-else for proper short-circuiting *)
      let stmts = Buffer.sub ctx.buf buf_before (Buffer.length ctx.buf - buf_before) in
      Buffer.truncate ctx.buf buf_before;
      let result = fresh_tmp ctx in
      emit_line ctx (Printf.sprintf "let %s;" result);
      emit_line ctx (Printf.sprintf "if (%s) {" a);
      ctx.indent <- ctx.indent + 1;
      Buffer.add_string ctx.buf stmts;
      emit_line ctx (Printf.sprintf "%s = %s;" result b);
      ctx.indent <- ctx.indent - 1;
      emit_line ctx "} else {";
      ctx.indent <- ctx.indent + 1;
      emit_line ctx (Printf.sprintf "%s = false;" result);
      ctx.indent <- ctx.indent - 1;
      emit_line ctx "}";
      result
    end
  | Ast.Or ->
    let a = compile_non_tail ctx e1 in
    let buf_before = Buffer.length ctx.buf in
    let b = compile_non_tail ctx e2 in
    if Buffer.length ctx.buf = buf_before then
      (* e2 is pure — safe to use || directly *)
      "(" ^ a ^ " || " ^ b ^ ")"
    else begin
      (* e2 emitted statements — need if-else for proper short-circuiting *)
      let stmts = Buffer.sub ctx.buf buf_before (Buffer.length ctx.buf - buf_before) in
      Buffer.truncate ctx.buf buf_before;
      let result = fresh_tmp ctx in
      emit_line ctx (Printf.sprintf "let %s;" result);
      emit_line ctx (Printf.sprintf "if (%s) {" a);
      ctx.indent <- ctx.indent + 1;
      emit_line ctx (Printf.sprintf "%s = true;" result);
      ctx.indent <- ctx.indent - 1;
      emit_line ctx "} else {";
      ctx.indent <- ctx.indent + 1;
      Buffer.add_string ctx.buf stmts;
      emit_line ctx (Printf.sprintf "%s = %s;" result b);
      ctx.indent <- ctx.indent - 1;
      emit_line ctx "}";
      result
    end
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
    let resolved = Types.repr e1.ty in
    (match resolved with
     | Types.TInt | Types.TFloat ->
       let a = compile_non_tail ctx e1 in
       let b = compile_non_tail ctx e2 in
       let js_op = match op with
         | Ast.Add -> "+" | Ast.Sub -> "-"
         | Ast.Mul -> "*" | Ast.Div -> "/"
         | _ -> assert false in
       if resolved = Types.TInt && op = Ast.Div then
         "((" ^ a ^ " " ^ js_op ^ " " ^ b ^ ") | 0)"  (* integer division *)
       else
         "(" ^ a ^ " " ^ js_op ^ " " ^ b ^ ")"
     | _ ->
       let method_name = match op with
         | Ast.Add -> "+" | Ast.Sub -> "-" | Ast.Mul -> "*" | Ast.Div -> "/"
         | _ -> assert false in
       compile_class_binop ctx "Num" method_name e1 e2)
  | Ast.Mod ->
    let a = compile_non_tail ctx e1 in
    let b = compile_non_tail ctx e2 in
    "(" ^ a ^ " % " ^ b ^ ")"
  | Ast.Eq | Ast.Neq ->
    let resolved = Types.repr e1.ty in
    let is_primitive = match resolved with
      | Types.TInt | Types.TFloat | Types.TBool | Types.TString
      | Types.TByte | Types.TRune | Types.TUnit -> true
      | _ -> false
    in
    let a = compile_non_tail ctx e1 in
    let b = compile_non_tail ctx e2 in
    if is_primitive then
      let js_op = if op = Ast.Eq then "===" else "!==" in
      "(" ^ a ^ " " ^ js_op ^ " " ^ b ^ ")"
    else
      let eq_call = "_eq(" ^ a ^ ", " ^ b ^ ")" in
      if op = Ast.Eq then eq_call
      else "!" ^ eq_call
  | Ast.Lt | Ast.Gt | Ast.Le | Ast.Ge ->
    let resolved = Types.repr e1.ty in
    let is_builtin = match resolved with
      | Types.TInt | Types.TFloat | Types.TString | Types.TByte | Types.TRune
      | Types.TVar { contents = Types.Unbound _ } -> true
      | _ -> false in
    if is_builtin then begin
      let a = compile_non_tail ctx e1 in
      let b = compile_non_tail ctx e2 in
      let js_op = match op with
        | Ast.Lt -> "<" | Ast.Gt -> ">" | Ast.Le -> "<=" | Ast.Ge -> ">="
        | _ -> assert false in
      "(" ^ a ^ " " ^ js_op ^ " " ^ b ^ ")"
    end else begin
      let method_name = match op with
        | Ast.Lt -> "<" | Ast.Gt -> ">" | Ast.Le -> "<=" | Ast.Ge -> ">="
        | _ -> assert false in
      compile_class_binop ctx "Ord" method_name e1 e2
    end
  | Ast.Concat ->
    let a = compile_non_tail ctx e1 in
    let b = compile_non_tail ctx e2 in
    "(" ^ a ^ " + " ^ b ^ ")"
  | Ast.Land | Ast.Lor | Ast.Lxor | Ast.Lsl | Ast.Lsr ->
    let resolved = Types.repr e1.ty in
    (match resolved with
     | Types.TInt ->
       let a = compile_non_tail ctx e1 in
       let b = compile_non_tail ctx e2 in
       let js_op = match op with
         | Ast.Land -> "&" | Ast.Lor -> "|" | Ast.Lxor -> "^"
         | Ast.Lsl -> "<<" | Ast.Lsr -> ">>"
         | _ -> assert false in
       "(" ^ a ^ " " ^ js_op ^ " " ^ b ^ ")"
     | _ ->
       let method_name = match op with
         | Ast.Land -> "land" | Ast.Lor -> "lor" | Ast.Lxor -> "lxor"
         | Ast.Lsl -> "lsl" | Ast.Lsr -> "lsr" | _ -> assert false in
       compile_class_binop ctx "Bitwise" method_name e1 e2)
  | Ast.Pipe ->
    let fn_js = compile_non_tail ctx e2 in
    let arg_js = compile_non_tail ctx e1 in
    "_call(" ^ fn_js ^ ", [" ^ arg_js ^ "])"

and compile_class_binop ctx class_name method_name e1 e2 =
  let conc_ty = Types.repr e1.ty in
  let class_def = match List.find_opt (fun (c : Types.class_def) ->
    String.equal c.class_name class_name) ctx.type_env.Types.classes with
    | Some cd -> cd
    | None -> error (Printf.sprintf "class %s not found" class_name) in
  let num_params = List.length class_def.class_params in
  let partial = List.init num_params (fun _ -> Some conc_ty) in
  let concrete_partial = List.map (fun opt ->
    match opt with
    | Some ty ->
      (match Types.repr ty with
       | Types.TVar { contents = Types.Unbound _ } -> None
       | _ -> Some (Types.repr ty))
    | None -> None
  ) partial in
  let matching = List.filter (fun (inst : Types.instance_def) ->
    String.equal inst.inst_class class_name &&
    List.length inst.inst_tys = num_params &&
    Types.match_partial_inst inst.inst_tys concrete_partial
  ) ctx.type_env.Types.instances in
  match matching with
  | [inst] ->
    let dict_access =
      mangle_name inst.Types.inst_dict_name ^ "." ^ mangle_name method_name
    in
    let a = compile_non_tail ctx e1 in
    let b = compile_non_tail ctx e2 in
    "_call(" ^ dict_access ^ ", [" ^ a ^ ", " ^ b ^ "])"
  | [] ->
    error (Printf.sprintf "no instance of %s for type %s"
      class_name (Types.pp_ty conc_ty))
  | _ ->
    (match Types.most_specific_inst matching with
     | Some inst ->
       let dict_access =
         if inst.Types.inst_constraints = [] then
           mangle_name (inst.Types.inst_dict_name ^ "$" ^ method_name)
         else
           mangle_name inst.Types.inst_dict_name ^ "." ^ mangle_name method_name
       in
       let a = compile_non_tail ctx e1 in
       let b = compile_non_tail ctx e2 in
       "_call(" ^ dict_access ^ ", [" ^ a ^ ", " ^ b ^ "])"
     | None ->
       error (Printf.sprintf "ambiguous instance for %s method %s"
         class_name method_name))

(* ---- Unary operators ---- *)

and compile_unop ctx op (e : Typechecker.texpr) =
  match op with
  | Ast.Neg ->
    let resolved = Types.repr e.ty in
    (match resolved with
     | Types.TInt | Types.TFloat ->
       let v = compile_non_tail ctx e in
       "(-" ^ v ^ ")"
     | _ ->
       compile_class_unop ctx "Num" "neg" e)
  | Ast.Not ->
    let v = compile_non_tail ctx e in
    "(!" ^ v ^ ")"
  | Ast.Lnot ->
    let resolved = Types.repr e.ty in
    (match resolved with
     | Types.TInt ->
       let v = compile_non_tail ctx e in
       "(~" ^ v ^ ")"
     | _ ->
       compile_class_unop ctx "Bitwise" "lnot" e)

and compile_class_unop ctx class_name method_name (e : Typechecker.texpr) =
  let conc_ty = Types.repr e.ty in
  let class_def = match List.find_opt (fun (c : Types.class_def) ->
    String.equal c.class_name class_name) ctx.type_env.Types.classes with
    | Some cd -> cd
    | None -> error (Printf.sprintf "class %s not found" class_name) in
  let num_params = List.length class_def.class_params in
  let partial = List.init num_params (fun _ -> Some conc_ty) in
  let concrete_partial = List.map (fun opt ->
    match opt with
    | Some ty ->
      (match Types.repr ty with
       | Types.TVar { contents = Types.Unbound _ } -> None
       | _ -> Some (Types.repr ty))
    | None -> None
  ) partial in
  let matching = List.filter (fun (inst : Types.instance_def) ->
    String.equal inst.inst_class class_name &&
    List.length inst.inst_tys = num_params &&
    Types.match_partial_inst inst.inst_tys concrete_partial
  ) ctx.type_env.Types.instances in
  match matching with
  | [inst] ->
    let dict_access =
      mangle_name inst.Types.inst_dict_name ^ "." ^ mangle_name method_name
    in
    let v = compile_non_tail ctx e in
    "_call(" ^ dict_access ^ ", [" ^ v ^ "])"
  | [] ->
    error (Printf.sprintf "no instance of %s for type %s"
      class_name (Types.pp_ty conc_ty))
  | _ ->
    error (Printf.sprintf "ambiguous instance for %s"
      class_name)

(* ---- If/else ---- *)

and compile_if ctx cond then_e else_e =
  let c = compile_expr ctx cond in
  let result = fresh_tmp ctx in
  emit_line ctx (Printf.sprintf "let %s;" result);
  emit_line ctx (Printf.sprintf "if (%s) {" c);
  ctx.indent <- ctx.indent + 1;
  let t = compile_expr ctx then_e in
  emit_line ctx (Printf.sprintf "%s = %s;" result t);
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "} else {";
  ctx.indent <- ctx.indent + 1;
  let e = compile_expr ctx else_e in
  emit_line ctx (Printf.sprintf "%s = %s;" result e);
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "}";
  result

(* ---- Sequences ---- *)

and compile_seq ctx e1 e2 =
  let v1 = compile_non_tail ctx e1 in
  emit_line ctx (v1 ^ ";");
  compile_expr ctx e2

(* ---- Mutable locals ---- *)

and compile_let_mut ctx name init body =
  let v = compile_expr ctx init in
  let js_name = mangle_name name in
  (* Use a unique name to avoid let redeclaration in same JS block *)
  let actual_name = js_name ^ "_" ^ string_of_int ctx.tmp_counter in
  ctx.tmp_counter <- ctx.tmp_counter + 1;
  push_scope ctx;
  bind_var ctx name actual_name;
  emit_line ctx (Printf.sprintf "let %s = %s;" actual_name v);
  let result = fresh_tmp ctx in
  let b = compile_expr ctx body in
  emit_line ctx (Printf.sprintf "const %s = %s;" result b);
  pop_scope ctx;
  result

(* ---- While loops ---- *)

and compile_while ctx cond body =
  let result = fresh_tmp ctx in
  let break_flag = fresh_tmp ctx in
  let break_val = fresh_tmp ctx in
  let saved_break_flag = ctx.break_flag_name in
  let saved_break_val = ctx.break_val_name in
  ctx.break_flag_name <- break_flag;
  ctx.break_val_name <- break_val;
  emit_line ctx (Printf.sprintf "let %s = undefined;" result);
  emit_line ctx (Printf.sprintf "let %s = false, %s;" break_flag break_val);
  emit_line ctx "while (true) {";
  ctx.indent <- ctx.indent + 1;
  let c = compile_expr ctx cond in
  emit_line ctx (Printf.sprintf "if (!(%s)) break;" c);
  let v_body = compile_expr ctx body in
  emit_line ctx (v_body ^ ";");
  emit_line ctx (Printf.sprintf "if (%s) break;" break_flag);
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "}";
  emit_line ctx (Printf.sprintf "%s = %s ? %s : undefined;" result break_flag break_val);
  ctx.break_flag_name <- saved_break_flag;
  ctx.break_val_name <- saved_break_val;
  result

(* ---- Pattern matching ---- *)

and compile_match ctx scrut arms loc =
  let scrut_js = compile_non_tail ctx scrut in
  let scrut_tmp = fresh_tmp ctx in
  let result = fresh_tmp ctx in
  let label = fresh_tmp ctx in
  emit_line ctx (Printf.sprintf "let %s;" result);
  emit_line ctx (Printf.sprintf "const %s = %s;" scrut_tmp scrut_js);
  emit_line ctx (Printf.sprintf "%s: {" label);
  ctx.indent <- ctx.indent + 1;
  List.iter (fun (pat, guard, body) ->
    let (conds, bindings) = compile_pattern ctx scrut_tmp pat in
    let cond_str = match conds with
      | [] -> "true"
      | _ -> String.concat " && " conds
    in
    emit_line ctx (Printf.sprintf "if (%s) {" cond_str);
    ctx.indent <- ctx.indent + 1;
    push_scope ctx;
    List.iter (fun (mml_name, js_expr) ->
      let js_name = mangle_name mml_name in
      bind_var ctx mml_name js_name;
      emit_line ctx (Printf.sprintf "const %s = %s;" js_name js_expr)
    ) bindings;
    (match guard with
     | Some g ->
       let guard_js = compile_expr ctx g in
       emit_line ctx (Printf.sprintf "if (%s) {" guard_js);
       ctx.indent <- ctx.indent + 1;
       let b = compile_expr ctx body in
       emit_line ctx (Printf.sprintf "%s = %s;" result b);
       emit_line ctx (Printf.sprintf "break %s;" label);
       ctx.indent <- ctx.indent - 1;
       emit_line ctx "}"
     | None ->
       let b = compile_expr ctx body in
       emit_line ctx (Printf.sprintf "%s = %s;" result b);
       emit_line ctx (Printf.sprintf "break %s;" label));
    pop_scope ctx;
    ctx.indent <- ctx.indent - 1;
    emit_line ctx "}"
  ) arms;
  emit_line ctx (Printf.sprintf "_match_fail(\"line %d\");" loc.Token.line);
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "}";
  result

(* Returns (conditions: string list, bindings: (mml_name * js_expr) list) *)
and compile_pattern ctx scrutinee (pat : Ast.pattern) : (string list * (string * string) list) =
  match pat with
  | Ast.PatWild -> ([], [])
  | Ast.PatVar name -> ([], [(name, scrutinee)])
  | Ast.PatInt n -> ([Printf.sprintf "%s === %d" scrutinee n], [])
  | Ast.PatFloat f -> ([Printf.sprintf "%s === %s" scrutinee (Printf.sprintf "%.17g" f)], [])
  | Ast.PatBool true -> ([scrutinee], [])
  | Ast.PatBool false -> (["!" ^ scrutinee], [])
  | Ast.PatString s -> ([Printf.sprintf "%s === %s" scrutinee (escape_js_string s)], [])
  | Ast.PatUnit -> ([], [])
  | Ast.PatNil -> ([scrutinee ^ " === null"], [])
  | Ast.PatCons (hd_pat, tl_pat) ->
    let hd_expr = scrutinee ^ "._hd" in
    let tl_expr = scrutinee ^ "._tl" in
    let (hd_conds, hd_binds) = compile_pattern ctx hd_expr hd_pat in
    let (tl_conds, tl_binds) = compile_pattern ctx tl_expr tl_pat in
    let conds = (scrutinee ^ " !== null") :: hd_conds @ tl_conds in
    (conds, hd_binds @ tl_binds)
  | Ast.PatTuple pats ->
    let all_conds = ref [] in
    let all_binds = ref [] in
    List.iteri (fun i sub_pat ->
      let elem_expr = Printf.sprintf "%s[%d]" scrutinee i in
      let (c, b) = compile_pattern ctx elem_expr sub_pat in
      all_conds := !all_conds @ c;
      all_binds := !all_binds @ b
    ) pats;
    (!all_conds, !all_binds)
  | Ast.PatConstruct (name, arg_pat) when is_newtype_ctor ctx.type_env name ->
    (* Newtype constructor: erased at runtime — match sub-pattern directly *)
    (match arg_pat with
     | Some sub_pat -> compile_pattern ctx scrutinee sub_pat
     | None -> ([], []))
  | Ast.PatConstruct (name, arg_pat) ->
    let tag = if String.length name > 0 && name.[0] = '`' then
      Hashtbl.hash (String.sub name 1 (String.length name - 1)) land 0x3FFFFFFF
    else
      tag_for_constructor ctx.type_env name
    in
    let conds = [Printf.sprintf "%s._tag === %d" scrutinee tag] in
    (match arg_pat with
     | None -> (conds, [])
     | Some sub_pat ->
       let payload_expr = scrutinee ^ "._val" in
       let (sub_conds, sub_binds) = compile_pattern ctx payload_expr sub_pat in
       (conds @ sub_conds, sub_binds))
  | Ast.PatRecord field_pats ->
    let all_conds = ref [] in
    let all_binds = ref [] in
    List.iter (fun (fname, sub_pat) ->
      let field_expr = scrutinee ^ "." ^ mangle_name fname in
      let (c, b) = compile_pattern ctx field_expr sub_pat in
      all_conds := !all_conds @ c;
      all_binds := !all_binds @ b
    ) field_pats;
    (!all_conds, !all_binds)
  | Ast.PatAs (inner_pat, name) ->
    let (conds, binds) = compile_pattern ctx scrutinee inner_pat in
    (conds, binds @ [(name, scrutinee)])
  | Ast.PatOr (p1, p2) ->
    let (conds1, binds1) = compile_pattern ctx scrutinee p1 in
    let (conds2, binds2) = compile_pattern ctx scrutinee p2 in
    let cond1 = match conds1 with [] -> "true" | cs -> String.concat " && " cs in
    let cond2 = match conds2 with [] -> "true" | cs -> String.concat " && " cs in
    let or_cond = "(" ^ cond1 ^ " || " ^ cond2 ^ ")" in
    (* For or-patterns, variable names must match on both sides,
       but extraction paths may differ. Use conditional bindings. *)
    let binds = if binds1 = binds2 then binds1
      else
        List.map (fun (name, expr1) ->
          let expr2 = List.assoc name binds2 in
          if expr1 = expr2 then (name, expr1)
          else (name, Printf.sprintf "(%s ? %s : %s)" cond1 expr1 expr2)
        ) binds1
    in
    ([or_cond], binds)
  | Ast.PatArray pats ->
    let len_cond = Printf.sprintf "%s._arr.length === %d" scrutinee (List.length pats) in
    let all_conds = ref [len_cond] in
    let all_binds = ref [] in
    List.iteri (fun i sub_pat ->
      let elem_expr = Printf.sprintf "%s._arr[%d]" scrutinee i in
      let (c, b) = compile_pattern ctx elem_expr sub_pat in
      all_conds := !all_conds @ c;
      all_binds := !all_binds @ b
    ) pats;
    (!all_conds, !all_binds)
  | Ast.PatPolyVariant (tag, arg_pat) ->
    let num_tag = Hashtbl.hash tag land 0x3FFFFFFF in
    let conds = [Printf.sprintf "%s._tag === %d" scrutinee num_tag] in
    (match arg_pat with
     | None -> (conds, [])
     | Some sub_pat ->
       let payload_expr = scrutinee ^ "._val" in
       let (sub_conds, sub_binds) = compile_pattern ctx payload_expr sub_pat in
       (conds @ sub_conds, sub_binds))
  | Ast.PatPin name ->
    let js_name = lookup_var ctx name in
    ([Printf.sprintf "_eq(%s, %s)" scrutinee js_name], [])
  | Ast.PatAnnot (inner_pat, _) ->
    compile_pattern ctx scrutinee inner_pat
  | Ast.PatMap entries ->
    let all_conds = ref [] in
    let all_binds = ref [] in
    let emit_key_expr key_pat = match key_pat with
      | Ast.PatInt n -> string_of_int n
      | Ast.PatString s -> escape_js_string s
      | Ast.PatBool b -> if b then "true" else "false"
      | Ast.PatFloat f -> Printf.sprintf "%g" f
      | Ast.PatPin name -> lookup_var ctx name
      | _ -> error "map pattern keys must be literals or pin patterns"
    in
    List.iter (fun (key_pat, val_pat) ->
      let key_expr = emit_key_expr key_pat in
      (* Condition: Map.has(key)(map) *)
      all_conds := !all_conds @ [Printf.sprintf "%s(%s)(%s)" (mangle_name "Map.has") key_expr scrutinee];
      (* Extract value: Map.get(key)(map) returns Some(v), unwrap with ._val *)
      let val_expr = Printf.sprintf "%s(%s)(%s)._val" (mangle_name "Map.get") key_expr scrutinee in
      let (c, b) = compile_pattern ctx val_expr val_pat in
      all_conds := !all_conds @ c;
      all_binds := !all_binds @ b
    ) entries;
    (!all_conds, !all_binds)

(* ---- Handle / CPS compilation for algebraic effects ---- *)

(* Try/with optimization: handler arms don't resume — use throw/catch *)
and compile_handle_trywith ctx body op_arms return_arm result =
  emit_line ctx (Printf.sprintf "const %s = (function() {" result);
  ctx.indent <- ctx.indent + 1;

  let h_saved = fresh_tmp ctx in
  emit_line ctx (Printf.sprintf "const %s = _h;" h_saved);

  (* Install _h handlers that throw (for called functions) *)
  emit_line ctx (Printf.sprintf "_h = Object.assign({}, %s, {" h_saved);
  ctx.indent <- ctx.indent + 1;
  List.iter (fun (op_name, arg_name, k_name, _) ->
    let arg_js = mangle_name arg_name in
    let k_js = mangle_name k_name in
    emit_line ctx (Printf.sprintf
      "\"%s\": function(%s, %s) { throw {_e: \"%s\", _v: %s}; },"
      op_name arg_js k_js op_name arg_js)
  ) op_arms;
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "});";

  emit_line ctx "try {";
  ctx.indent <- ctx.indent + 1;

  (* Set trywith_ops so inline performs compile as throw *)
  let saved_two = ctx.trywith_ops in
  let handled_op_names = List.map (fun (op, _, _, _) -> op) op_arms in
  ctx.trywith_ops <- handled_op_names @ ctx.trywith_ops;

  (* Check if body needs CPS for unhandled performs *)
  let body_needs_cps = expr_has_unhandled_perform handled_op_names body in

  if body_needs_cps then begin
    (* Body has unhandled performs — CPS + trampoline, try/with ops still throw *)
    emit_line ctx "return _trampoline(function() {";
    ctx.indent <- ctx.indent + 1;
    compile_cps ctx body (fun body_result ->
      match return_arm with
      | Some (ret_name, ret_body) ->
        let ret_js = mangle_name ret_name in
        let actual_ret = ret_js ^ "_" ^ string_of_int ctx.tmp_counter in
        ctx.tmp_counter <- ctx.tmp_counter + 1;
        push_scope ctx;
        bind_var ctx ret_name actual_ret;
        emit_line ctx (Printf.sprintf "const %s = %s;" actual_ret body_result);
        let v = compile_non_tail ctx ret_body in
        emit_line ctx (Printf.sprintf "return %s;" v);
        pop_scope ctx
      | None ->
        emit_line ctx (Printf.sprintf "return %s;" body_result)
    );
    ctx.indent <- ctx.indent - 1;
    emit_line ctx "});"
  end else begin
    (* Body is fully direct — compile directly *)
    let body_v = compile_non_tail ctx body in
    (match return_arm with
     | Some (ret_name, ret_body) ->
       let ret_js = mangle_name ret_name in
       let actual_ret = ret_js ^ "_" ^ string_of_int ctx.tmp_counter in
       ctx.tmp_counter <- ctx.tmp_counter + 1;
       push_scope ctx;
       bind_var ctx ret_name actual_ret;
       emit_line ctx (Printf.sprintf "const %s = %s;" actual_ret body_v);
       let rv = compile_non_tail ctx ret_body in
       emit_line ctx (Printf.sprintf "return %s;" rv);
       pop_scope ctx
     | None ->
       emit_line ctx (Printf.sprintf "return %s;" body_v))
  end;

  ctx.trywith_ops <- saved_two;

  ctx.indent <- ctx.indent - 1;
  (* Catch block: dispatch by _exc._e *)
  emit_line ctx "} catch (_exc) {";
  ctx.indent <- ctx.indent + 1;
  let first = ref true in
  List.iter (fun (op_name, arg_name, _k_name, handler_body) ->
    let prefix = if !first then "if" else "} else if" in
    first := false;
    emit_line ctx (Printf.sprintf "%s (_exc && _exc._e === \"%s\") {" prefix op_name);
    ctx.indent <- ctx.indent + 1;
    push_scope ctx;
    let arg_js = mangle_name arg_name in
    bind_var ctx arg_name arg_js;
    emit_line ctx (Printf.sprintf "const %s = _exc._v;" arg_js);
    let v = compile_non_tail ctx handler_body in
    emit_line ctx (Printf.sprintf "return %s;" v);
    pop_scope ctx;
    ctx.indent <- ctx.indent - 1
  ) op_arms;
  emit_line ctx "} else { throw _exc; }";
  ctx.indent <- ctx.indent - 1;
  emit_line ctx (Printf.sprintf "} finally { _h = %s; }" h_saved);

  ctx.indent <- ctx.indent - 1;
  emit_line ctx "})();"

and compile_handle ctx body arms =
  let result = fresh_tmp ctx in
  (* Partition arms by classified type *)
  let return_arm = ref None in
  let provide_ops = ref [] in
  let try_ops = ref [] in
  let full_ops = ref [] in
  List.iter (fun arm ->
    match arm with
    | Typechecker.THReturn (name, body) -> return_arm := Some (name, body)
    | Typechecker.THOp (op, arg, k, body) ->
      full_ops := (op, arg, k, body) :: !full_ops
    | Typechecker.THOpProvide (op, arg, body) ->
      provide_ops := (op, arg, body) :: !provide_ops
    | Typechecker.THOpTry (op, arg, body) ->
      try_ops := (op, arg, body) :: !try_ops
  ) arms;
  let provide_ops = List.rev !provide_ops in
  let try_ops = List.rev !try_ops in
  let full_ops = List.rev !full_ops in

  (* All-trywith fast path: no provide or full ops, return arm is pure *)
  let return_arm_pure = match !return_arm with
    | Some (_, rb) -> not (expr_has_perform rb)
    | None -> true in

  if full_ops = [] && provide_ops = [] && try_ops <> [] && return_arm_pure then begin
    (* Convert try_ops to the format compile_handle_trywith expects *)
    let trywith_arms = List.map (fun (op, arg, body) ->
      (op, arg, "__k_try", body)) try_ops in
    compile_handle_trywith ctx body trywith_arms !return_arm result
  end else begin
    (* Simple ops come from provide_ops (tail-resumptive, already stripped of TEResume) *)
    let simple_ops = List.map (fun (op, _, _) -> op) provide_ops in

    (* Emit: const _result = (function() { save/restore _h ... })(); *)
    emit_line ctx (Printf.sprintf "const %s = (function() {" result);
    ctx.indent <- ctx.indent + 1;

    (* Save current handler *)
    let h_saved = fresh_tmp ctx in
    emit_line ctx (Printf.sprintf "const %s = _h;" h_saved);

    (* For provide ops: emit direct handler functions *)
    let saved_direct_ops = ctx.direct_dispatch_ops in
    List.iter (fun (op_name, arg_name, value_body) ->
      let direct_fn = fresh_tmp ctx in
      let arg_js = mangle_name arg_name in
      emit_line ctx (Printf.sprintf "function %s(%s) {" direct_fn arg_js);
      ctx.indent <- ctx.indent + 1;
      push_scope ctx;
      bind_var ctx arg_name arg_js;
      (* THOpProvide bodies have TEResume stripped — compile directly as value *)
      let v = compile_non_tail ctx value_body in
      emit_line ctx (Printf.sprintf "return %s;" v);
      pop_scope ctx;
      ctx.indent <- ctx.indent - 1;
      emit_line ctx "}";
      ctx.direct_dispatch_ops <- (op_name, direct_fn) :: ctx.direct_dispatch_ops
    ) provide_ops;

    (* Remove stale direct dispatch entries for ops handled by full CPS or try arms.
       Without this, a nested handler's full/try arm for the same op would be bypassed
       because perform would find the outer handler's direct dispatch entry. *)
    let shadowed_ops = List.map (fun (op, _, _, _) -> op) full_ops
                     @ List.map (fun (op, _, _) -> op) try_ops in
    if shadowed_ops <> [] then
      ctx.direct_dispatch_ops <- List.filter
        (fun (op, _) -> not (List.mem op shadowed_ops)) ctx.direct_dispatch_ops;

    (* Install _h with handler functions *)
    emit_line ctx (Printf.sprintf "_h = Object.assign({}, %s, {" h_saved);
    ctx.indent <- ctx.indent + 1;

    (* Emit provide ops as CPS wrappers calling direct functions *)
    List.iter (fun (op_name, arg_name, _) ->
      let arg_js = mangle_name arg_name in
      let k_js = "_k" in
      let direct_fn = List.assoc op_name ctx.direct_dispatch_ops in
      emit_line ctx (Printf.sprintf
        "\"%s\": function(%s, %s) { return _bounce(function() { return %s(%s(%s)); }); },"
        op_name arg_js k_js k_js direct_fn arg_js)
    ) provide_ops;

    (* Emit try ops: handler body never resumes, just returns the fallback value *)
    List.iter (fun (op_name, arg_name, fallback_body) ->
      let arg_js = mangle_name arg_name in
      let k_js = "_k" in
      emit_line ctx (Printf.sprintf "\"%s\": function(%s, %s) {" op_name arg_js k_js);
      ctx.indent <- ctx.indent + 1;
      push_scope ctx;
      bind_var ctx arg_name arg_js;
      let v = compile_non_tail ctx fallback_body in
      emit_line ctx (Printf.sprintf "return _bounce(function() { return %s; });" v);
      pop_scope ctx;
      ctx.indent <- ctx.indent - 1;
      emit_line ctx "},"
    ) try_ops;

    (* Emit full CPS ops *)
    List.iter (fun (op_name, arg_name, k_name, handler_body) ->
      let arg_js = mangle_name arg_name in
      let k_js = mangle_name k_name in
      emit_line ctx (Printf.sprintf "\"%s\": function(%s, %s) {" op_name arg_js k_js);
      ctx.indent <- ctx.indent + 1;
      push_scope ctx;
      bind_var ctx arg_name arg_js;
      bind_var ctx k_name k_js;
      let saved_htr = ctx.handler_tail_resume in
      ctx.handler_tail_resume <- all_resumes_are_tail handler_body;
      let v = compile_non_tail ctx handler_body in
      ctx.handler_tail_resume <- saved_htr;
      emit_line ctx (Printf.sprintf "return _bounce(function() { return %s; });" v);
      pop_scope ctx;
      ctx.indent <- ctx.indent - 1;
      emit_line ctx "},"
    ) full_ops;

    ctx.indent <- ctx.indent - 1;
    emit_line ctx "});";

    (* Compile body — direct style if all performs are handled by simple ops *)
    let body_needs_cps =
      if simple_ops = [] then expr_has_perform body
      else expr_has_unhandled_perform simple_ops body in

    emit_line ctx "try {";
    ctx.indent <- ctx.indent + 1;

    if body_needs_cps then begin
      (* Body has unhandled performs — use CPS + trampoline *)
      emit_line ctx "return _trampoline(function() {";
      ctx.indent <- ctx.indent + 1;
      compile_cps ctx body (fun body_result ->
        match !return_arm with
        | Some (ret_name, ret_body) ->
          let ret_js = mangle_name ret_name in
          let actual_ret = ret_js ^ "_" ^ string_of_int ctx.tmp_counter in
          ctx.tmp_counter <- ctx.tmp_counter + 1;
          push_scope ctx;
          bind_var ctx ret_name actual_ret;
          emit_line ctx (Printf.sprintf "const %s = %s;" actual_ret body_result);
          let v = compile_non_tail ctx ret_body in
          emit_line ctx (Printf.sprintf "return %s;" v);
          pop_scope ctx
        | None ->
          emit_line ctx (Printf.sprintf "return %s;" body_result)
      );
      ctx.indent <- ctx.indent - 1;
      emit_line ctx "});"
    end else begin
      (* Body is fully direct — no CPS, no trampoline *)
      let body_v = compile_non_tail ctx body in
      (match !return_arm with
       | Some (ret_name, ret_body) ->
         let ret_js = mangle_name ret_name in
         let actual_ret = ret_js ^ "_" ^ string_of_int ctx.tmp_counter in
         ctx.tmp_counter <- ctx.tmp_counter + 1;
         push_scope ctx;
         bind_var ctx ret_name actual_ret;
         emit_line ctx (Printf.sprintf "const %s = %s;" actual_ret body_v);
         let rv = compile_non_tail ctx ret_body in
         emit_line ctx (Printf.sprintf "return %s;" rv);
         pop_scope ctx
       | None ->
         emit_line ctx (Printf.sprintf "return %s;" body_v))
    end;

    ctx.indent <- ctx.indent - 1;
    emit_line ctx (Printf.sprintf "} finally { _h = %s; }" h_saved);

    ctx.direct_dispatch_ops <- saved_direct_ops;
    ctx.indent <- ctx.indent - 1;
    emit_line ctx "})();"
  end;
  result

(* CPS compilation: compiles an expression and calls cont with the result variable.
   Emits JS code as statements. The cont function should emit code that uses the
   result (typically ending with 'return'). *)
and compile_cps ctx (te : Typechecker.texpr) (cont : string -> unit) : unit =
  if not (expr_has_perform te) then begin
    (* Pure expression — compile directly, pass result to continuation *)
    let v = compile_non_tail ctx te in
    cont v
  end else
    match te.expr with
    | Typechecker.TEPerform (op_name, arg) ->
      compile_perform_cps ctx op_name arg cont

    | Typechecker.TEHandle _ ->
      (* Handle is a CPS/direct boundary — compile directly *)
      let v = compile_non_tail ctx te in
      cont v

    | Typechecker.TEResume (k_expr, val_expr) ->
      let k_js = compile_non_tail ctx k_expr in
      let v_js = compile_non_tail ctx val_expr in
      emit_line ctx (Printf.sprintf "return _bounce(function() { return %s(%s); });" k_js v_js)
      (* Note: resume in CPS doesn't call cont — it transfers control *)

    | Typechecker.TELet (name, _, e1, e2) ->
      compile_let_cps ctx name e1 e2 cont

    | Typechecker.TELetRec (name, _, fn_expr, body) ->
      compile_letrec_cps ctx name fn_expr body cont

    | Typechecker.TESeq (e1, e2) ->
      compile_seq_cps ctx e1 e2 cont

    | Typechecker.TEIf (cond, then_e, else_e) ->
      compile_if_cps ctx cond then_e else_e cont

    | Typechecker.TEMatch (scrut, arms, _partial) ->
      compile_match_cps ctx scrut arms te.loc cont

    | Typechecker.TEApp (fn, arg) ->
      compile_app_cps ctx te fn arg cont

    | Typechecker.TEFun _ ->
      (* Lambda that contains performs in its body — compile as function *)
      let v = compile_non_tail ctx te in
      cont v

    | Typechecker.TELetMut (name, init, body) ->
      if expr_has_perform init then begin
        compile_cps ctx init (fun init_v ->
          let js_name = mangle_name name in
          let actual_name = js_name ^ "_" ^ string_of_int ctx.tmp_counter in
          ctx.tmp_counter <- ctx.tmp_counter + 1;
          push_scope ctx;
          bind_var ctx name actual_name;
          emit_line ctx (Printf.sprintf "let %s = _resolve(%s);" actual_name init_v);
          compile_cps ctx body cont;
          pop_scope ctx)
      end else begin
        let v = compile_non_tail ctx init in
        let js_name = mangle_name name in
        let actual_name = js_name ^ "_" ^ string_of_int ctx.tmp_counter in
        ctx.tmp_counter <- ctx.tmp_counter + 1;
        push_scope ctx;
        bind_var ctx name actual_name;
        emit_line ctx (Printf.sprintf "let %s = _resolve(%s);" actual_name v);
        compile_cps ctx body cont;
        pop_scope ctx
      end

    | Typechecker.TEWhile (cond, body) ->
      compile_while_cps ctx cond body cont

    | _ ->
      let v = compile_non_tail ctx te in
      cont v

and compile_while_cps ctx cond body cont =
  let loop_fn = fresh_tmp ctx in
  let break_flag = fresh_tmp ctx in
  let break_val = fresh_tmp ctx in
  let saved_break_flag = ctx.break_flag_name in
  let saved_break_val = ctx.break_val_name in
  ctx.break_flag_name <- break_flag;
  ctx.break_val_name <- break_val;
  emit_line ctx (Printf.sprintf "let %s = false, %s;" break_flag break_val);
  emit_line ctx (Printf.sprintf "function %s() {" loop_fn);
  ctx.indent <- ctx.indent + 1;
  let c = compile_non_tail ctx cond in
  emit_line ctx (Printf.sprintf "if (!(%s)) {" c);
  ctx.indent <- ctx.indent + 1;
  cont "undefined";
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "}";
  compile_cps ctx body (fun body_val ->
    emit_line ctx (Printf.sprintf "%s;" body_val);
    emit_line ctx (Printf.sprintf "if (%s) {" break_flag);
    ctx.indent <- ctx.indent + 1;
    cont break_val;
    ctx.indent <- ctx.indent - 1;
    emit_line ctx "}";
    emit_line ctx (Printf.sprintf "return _bounce(%s);" loop_fn));
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "}";
  emit_line ctx (Printf.sprintf "return _bounce(%s);" loop_fn);
  ctx.break_flag_name <- saved_break_flag;
  ctx.break_val_name <- saved_break_val

and compile_perform_cps ctx op_name (arg : Typechecker.texpr) cont =
  match List.assoc_opt op_name ctx.direct_dispatch_ops with
  | Some direct_fn ->
    (* Simple handler direct dispatch in CPS context *)
    let arg_js = compile_non_tail ctx arg in
    let result_var = fresh_tmp ctx in
    emit_line ctx (Printf.sprintf "const %s = %s(%s);" result_var direct_fn arg_js);
    cont result_var
  | None when List.mem op_name ctx.trywith_ops ->
    (* Try/with in CPS context — throw *)
    let arg_js = compile_non_tail ctx arg in
    emit_line ctx (Printf.sprintf "throw {_e: \"%s\", _v: %s};" op_name arg_js)
    (* cont is never called — throw unwinds *)
  | None ->
    let arg_js = compile_non_tail ctx arg in
    let result_var = fresh_tmp ctx in
    emit_line ctx (Printf.sprintf "return _h[\"%s\"](%s, function(%s) {"
      op_name arg_js result_var);
    ctx.indent <- ctx.indent + 1;
    emit_line ctx "return _bounce(function() {";
    ctx.indent <- ctx.indent + 1;
    cont result_var;
    ctx.indent <- ctx.indent - 1;
    emit_line ctx "});";
    ctx.indent <- ctx.indent - 1;
    emit_line ctx "});";

and compile_let_cps ctx name e1 e2 cont =
  if expr_has_perform e1 then begin
    (* e1 is effectful — CPS-compile it, then bind result and CPS-compile e2 *)
    compile_cps ctx e1 (fun v1 ->
      let js_name = mangle_name name in
      let actual_name = if js_name = "_" then fresh_tmp ctx
        else js_name ^ "_" ^ string_of_int ctx.tmp_counter |> fun n ->
          ctx.tmp_counter <- ctx.tmp_counter + 1; n
      in
      push_scope ctx;
      bind_var ctx name actual_name;
      emit_line ctx (Printf.sprintf "const %s = _resolve(%s);" actual_name v1);
      compile_cps ctx e2 cont;
      pop_scope ctx)
  end else begin
    (* e1 is pure — compile directly, then CPS-compile e2 *)
    let v1 = compile_non_tail ctx e1 in
    let js_name = mangle_name name in
    let actual_name = if js_name = "_" then fresh_tmp ctx
      else js_name ^ "_" ^ string_of_int ctx.tmp_counter |> fun n ->
        ctx.tmp_counter <- ctx.tmp_counter + 1; n
    in
    push_scope ctx;
    bind_var ctx name actual_name;
    emit_line ctx (Printf.sprintf "const %s = _resolve(%s);" actual_name v1);
    compile_cps ctx e2 cont;
    pop_scope ctx
  end

and compile_letrec_cps ctx name fn_expr body cont =
  let js_name = mangle_name name in
  push_scope ctx;
  bind_var ctx name js_name;
  (match fn_expr.Typechecker.expr with
   | Typechecker.TEFun _ ->
     compile_named_function ctx js_name fn_expr
   | _ ->
     emit_js_placeholder ctx js_name fn_expr;
     emit_js_backpatch ctx js_name fn_expr);
  compile_cps ctx body cont;
  pop_scope ctx

and compile_seq_cps ctx e1 e2 cont =
  if expr_has_perform e1 then begin
    compile_cps ctx e1 (fun v1 ->
      emit_line ctx (Printf.sprintf "_resolve(%s);" v1);
      compile_cps ctx e2 cont)
  end else begin
    let v1 = compile_non_tail ctx e1 in
    emit_line ctx (Printf.sprintf "_resolve(%s);" v1);
    compile_cps ctx e2 cont
  end

and compile_if_cps ctx cond then_e else_e cont =
  let c = compile_non_tail ctx cond in
  emit_line ctx (Printf.sprintf "if (%s) {" c);
  ctx.indent <- ctx.indent + 1;
  compile_cps ctx then_e cont;
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "} else {";
  ctx.indent <- ctx.indent + 1;
  compile_cps ctx else_e cont;
  ctx.indent <- ctx.indent - 1;
  emit_line ctx "}"

and compile_match_cps ctx scrut arms loc cont =
  (* Compile scrutinee directly, then CPS each arm body *)
  let scrut_js = compile_non_tail ctx scrut in
  let scrut_tmp = fresh_tmp ctx in
  emit_line ctx (Printf.sprintf "const %s = %s;" scrut_tmp scrut_js);
  (* Use a done flag to avoid duplicate cont calls in JS *)
  let matched = fresh_tmp ctx in
  emit_line ctx (Printf.sprintf "let %s = false;" matched);
  List.iter (fun (pat, guard, body) ->
    let (conds, bindings) = compile_pattern ctx scrut_tmp pat in
    let cond_str = match conds with
      | [] -> "true"
      | _ -> String.concat " && " conds
    in
    emit_line ctx (Printf.sprintf "if (!%s && %s) {" matched cond_str);
    ctx.indent <- ctx.indent + 1;
    push_scope ctx;
    List.iter (fun (mml_name, js_expr) ->
      let js_nm = mangle_name mml_name in
      bind_var ctx mml_name js_nm;
      emit_line ctx (Printf.sprintf "const %s = %s;" js_nm js_expr)
    ) bindings;
    (match guard with
     | Some g ->
       let guard_js = compile_non_tail ctx g in
       emit_line ctx (Printf.sprintf "if (%s) {" guard_js);
       ctx.indent <- ctx.indent + 1;
       emit_line ctx (Printf.sprintf "%s = true;" matched);
       compile_cps ctx body cont;
       ctx.indent <- ctx.indent - 1;
       emit_line ctx "}"
     | None ->
       emit_line ctx (Printf.sprintf "%s = true;" matched);
       compile_cps ctx body cont);
    pop_scope ctx;
    ctx.indent <- ctx.indent - 1;
    emit_line ctx "}"
  ) arms;
  emit_line ctx (Printf.sprintf "if (!%s) _match_fail(\"line %d\");" matched loc.Token.line)

and compile_app_cps ctx _te fn arg cont =
  (* For function application in CPS context:
     If the arguments or function contain inline performs, CPS them first.
     Otherwise, compile the app directly. *)
  let rec collect_args expr acc =
    match expr.Typechecker.expr with
    | Typechecker.TEApp (inner_fn, inner_arg) ->
      collect_args inner_fn (inner_arg :: acc)
    | _ -> (expr, acc)
  in
  let (base_fn, all_args) = collect_args fn [arg] in
  (* For now, compile arguments and function directly, then call *)
  let fn_js = compile_non_tail ctx base_fn in
  let args_js = List.map (compile_non_tail ctx) all_args in
  let n = List.length all_args in
  let known_arity = count_arrows base_fn.ty in
  let call_expr = if known_arity = Some n then
    fn_js ^ "(" ^ String.concat ", " args_js ^ ")"
  else
    "_call(" ^ fn_js ^ ", [" ^ String.concat ", " args_js ^ "])"
  in
  let result_var = fresh_tmp ctx in
  emit_line ctx (Printf.sprintf "const %s = _resolve(%s);" result_var call_expr);
  cont result_var

(* ---- Top-level declarations ---- *)

and compile_decl ctx (decl : Typechecker.tdecl) =
  match decl with
  | Typechecker.TDLet (name, te) ->
    if String.equal name "_" then begin
      let v = compile_expr ctx te in
      emit_line ctx (Printf.sprintf "%s;" v)
    end else begin
      (* Check if name already bound in current scope to avoid const redeclaration *)
      let base_name = mangle_name name in
      let js_name = match ctx.scopes with
        | tbl :: _ when Hashtbl.mem tbl name -> fresh_tmp ctx ^ "_" ^ base_name
        | _ -> base_name
      in
      (match te.expr with
       | Typechecker.TEFun _ ->
         compile_named_function ctx js_name te;
         bind_var ctx name js_name
       | _ ->
         let v = compile_expr ctx te in
         emit_line ctx (Printf.sprintf "const %s = %s;" js_name v);
         bind_var ctx name js_name);
      if List.length ctx.scopes = 1 && is_exportable_name name then
        ctx.top_level_exports <- (name, js_name) :: ctx.top_level_exports
    end

  | Typechecker.TDLetMut (name, te) ->
    let base_name = mangle_name name in
    let js_name = match ctx.scopes with
      | tbl :: _ when Hashtbl.mem tbl name -> fresh_tmp ctx ^ "_" ^ base_name
      | _ -> base_name
    in
    let v = compile_expr ctx te in
    emit_line ctx (Printf.sprintf "let %s = %s;" js_name v);
    bind_var ctx name js_name;
    if List.length ctx.scopes = 1 && is_exportable_name name then
      ctx.top_level_exports <- (name, js_name) :: ctx.top_level_exports

  | Typechecker.TDLetRec (name, te) ->
    let base_name = mangle_name name in
    let js_name = match ctx.scopes with
      | tbl :: _ when Hashtbl.mem tbl name -> fresh_tmp ctx ^ "_" ^ base_name
      | _ -> base_name
    in
    bind_var ctx name js_name;
    (match te.Typechecker.expr with
     | Typechecker.TEFun _ ->
       compile_named_function ctx js_name te
     | _ ->
       emit_js_placeholder ctx js_name te;
       emit_js_backpatch ctx js_name te);
    if List.length ctx.scopes = 1 && is_exportable_name name then
      ctx.top_level_exports <- (name, js_name) :: ctx.top_level_exports

  | Typechecker.TDLetRecAnd bindings ->
    let named_bindings = List.map (fun (name, te) ->
      let base_name = mangle_name name in
      let js_name = match ctx.scopes with
        | tbl :: _ when Hashtbl.mem tbl name -> fresh_tmp ctx ^ "_" ^ base_name
        | _ -> base_name
      in
      (name, js_name, te)
    ) bindings in
    List.iter (fun (name, js_name, te) ->
      bind_var ctx name js_name;
      if List.length ctx.scopes = 1 && is_exportable_name name then
        ctx.top_level_exports <- (name, js_name) :: ctx.top_level_exports;
      match te.Typechecker.expr with
      | Typechecker.TEFun _ -> ()
      | _ -> emit_js_placeholder ctx js_name te
    ) named_bindings;
    (* Compile functions first *)
    List.iter (fun (_name, js_name, fn_expr) ->
      match fn_expr.Typechecker.expr with
      | Typechecker.TEFun _ -> compile_named_function ctx js_name fn_expr
      | _ -> ()
    ) named_bindings;
    (* Backpatch non-function bindings *)
    List.iter (fun (_name, js_name, te) ->
      match te.Typechecker.expr with
      | Typechecker.TEFun _ -> ()
      | _ -> emit_js_backpatch ctx js_name te
    ) named_bindings

  | Typechecker.TDType _ -> ()  (* type declarations don't generate code *)
  | Typechecker.TDClass _ -> ()
  | Typechecker.TDEffect _ -> ()

  | Typechecker.TDExpr te ->
    let v = compile_expr ctx te in
    emit_line ctx (Printf.sprintf "_last_val = %s;" v)

  | Typechecker.TDExtern (name, _scheme) ->
    (* External declarations — name is already qualified by typechecker *)
    let js_name = mangle_name name in
    (* Emit fallback for user-defined externs: if the function isn't already
       defined by js_builtins, look it up from globalThis._mmlExterns.
       Uses typeof check + var so builtin function declarations are not shadowed. *)
    emit_line ctx (Printf.sprintf
      "if (typeof %s === \"undefined\") { if (globalThis._mmlExterns && globalThis._mmlExterns[%s]) var %s = globalThis._mmlExterns[%s]; else throw new Error(\"extern \" + %s + \" not provided\"); }"
      js_name (escape_js_string name) js_name (escape_js_string name) (escape_js_string name));
    bind_var ctx name js_name

  | Typechecker.TDModule (mod_name, decls) ->
    let saved_module = ctx.current_module in
    ctx.current_module <- Some mod_name;
    push_scope ctx;
    List.iter (compile_decl ctx) decls;
    ctx.current_module <- saved_module;
    (* Re-export module bindings with qualified names *)
    (match ctx.scopes with
     | current :: _ ->
       Hashtbl.iter (fun mml_name js_name ->
         let qualified = mod_name ^ "." ^ mml_name in
         (* Bind in parent scope with qualified name *)
         (match ctx.scopes with
          | _ :: parent :: _ ->
            Hashtbl.replace parent qualified js_name;
            Hashtbl.replace parent mml_name js_name
          | _ -> ())
       ) current
     | [] -> ());
    pop_scope ctx

  | Typechecker.TDOpen aliases ->
    (* Bind the aliased names *)
    List.iter (fun (short_name, qualified_name) ->
      let js_name = lookup_var ctx qualified_name in
      bind_var ctx short_name js_name
    ) aliases

(* ---- Builtins JS ---- *)

let js_builtins = {|// --- Builtins ---
function print(v) {
  const s = (typeof v === "string") ? v : _pp(v);
  if (typeof globalThis._jsOutput === "function") globalThis._jsOutput(s);
  else if (typeof process !== "undefined") process.stdout.write(s);
  return undefined;
}
function println(v) {
  const s = (typeof v === "string") ? v : _pp(v);
  if (typeof globalThis._jsOutput === "function") globalThis._jsOutput(s + "\n");
  else if (typeof process !== "undefined") process.stdout.write(s + "\n");
  return undefined;
}
function string_of_int(n) { return String(n); }
function int_of_string(s) { const n = parseInt(s, 10); if (isNaN(n)) throw new Error("int_of_string: " + s); return n; }
function float_of_int(n) { return n; }
function int_of_float(f) { return Math.trunc(f); }
function float_of_string(s) { const f = parseFloat(s); if (isNaN(f)) throw new Error("float_of_string: " + s); return f; }
function string_of_float(f) {
  const s = String(f);
  return s.includes(".") || s.includes("e") ? s : s + ".";
}
function string_of_bool(b) { return String(b); }
function failwith(msg) { throw new Error(msg); }
const not = (b) => !b;
function $caret(a, b) { return a + b; }
function $mod(a, b) { return a % b; }
function __show_value(v) { return _pp(v); }
function copy_continuation(k) { return k; }
const ignore = (_) => undefined;
function fst(t) { return t[0]; }
function snd(t) { return t[1]; }
function string_length(s) { return s.length; }
function string_sub(s, start, len) { return s.substring(start, start + len); }
function string_get(s, i) { return s.charCodeAt(i); }
function string_contains(s, sub) { return s.includes(sub); }
function string_concat(sep, parts) {
  const arr = [];
  let c = parts;
  while (c !== null) { arr.push(c._hd); c = c._tl; }
  return arr.join(sep);
}
function array_length(a) { return a._arr.length; }
function array_get(a, i) { return a._arr[i]; }
function array_set(a, i, v) { a._arr[i] = v; return undefined; }
function array_make(n, v) { return {_arr: new Array(n).fill(v)}; }
function array_of_list(lst) {
  const arr = [];
  let c = lst;
  while (c !== null) { arr.push(c._hd); c = c._tl; }
  return {_arr: arr};
}
function array_to_list(arr) {
  const a = arr._arr;
  let result = null;
  for (let i = a.length - 1; i >= 0; i--) result = {_hd: a[i], _tl: result};
  return result;
}
function array_copy(a) { return {_arr: a._arr.slice()}; }
function array_sub(a, start, len) { return {_arr: a._arr.slice(start, start + len)}; }
function __math_pow(a, b) { return Math.pow(a, b); }
function __math_sqrt(x) { return Math.sqrt(x); }
function __math_floor(x) { return Math.floor(x); }
function __math_ceil(x) { return Math.ceil(x); }
function __math_round(x) { return Math.round(x); }
function __math_abs(x) { return Math.abs(x); }
function __math_sin(x) { return Math.sin(x); }
function __math_cos(x) { return Math.cos(x); }
function __math_abs_float(x) { return Math.abs(x); }
function __byte_to_char(b) { return String.fromCharCode(b); }
function __byte_to_int(b) { return b; }
function __byte_of_int(n) { return n & 0xFF; }
function __byte_to_string(b) { return String.fromCharCode(b); }
function __char_to_byte(s) { return s.charCodeAt(0); }
function __rune_to_string(cp) {
  return String.fromCodePoint(cp);
}
function __string_to_runes(s) {
  const cps = Array.from(s).map(c => c.codePointAt(0));
  let result = null;
  for (let i = cps.length - 1; i >= 0; i--) result = {_hd: cps[i], _tl: result};
  return result;
}
function __rune_to_int(r) { return r; }
function __rune_of_int(n) { return n; }
function __int_to_hex(n) { return n.toString(16); }
function __int_to_oct(n) { return n.toString(8); }
function __int_to_bin(n) { return n.toString(2); }
function __fmt_float(prec, f) { return f.toFixed(prec); }
function __fmt_hex(n) { return n.toString(16); }
function __fmt_hex_upper(n) { return n.toString(16).toUpperCase(); }
function __fmt_oct(n) { return n.toString(8); }
function __fmt_bin(n) { return n.toString(2); }
function __fmt_zero_pad(width, s) { return s.padStart(width, "0"); }
function __fmt_pad_left(width, s) { return s.padStart(width, " "); }
function __fmt_pad_right(width, s) { return s.padEnd(width, " "); }
function __sys_time() { return Date.now() / 1000.0; }
function __sys_exit(code) { if (typeof process !== "undefined") process.exit(code); throw new Error("exit: " + code); }
// --- Module extern stubs ---
// String module
function String$length(s) { return s.length; }
function String$sub(s, start, len) {
  if (start < 0 || len < 0 || start + len > s.length)
    throw new Error("String.sub: index out of bounds");
  return s.substring(start, start + len);
}
function String$split(delim, input) {
  if (delim.length === 0) {
    let r = null;
    for (let i = input.length - 1; i >= 0; i--) r = {_hd: input[i], _tl: r};
    return r;
  }
  const parts = input.split(delim);
  let r = null;
  for (let i = parts.length - 1; i >= 0; i--) r = {_hd: parts[i], _tl: r};
  return r;
}
function String$trim(s) { return s.trim(); }
function String$starts_with(prefix, s) { return s.startsWith(prefix); }
function String$contains(sub, s) { return s.includes(sub); }
function String$replace(old_s, new_s, input) {
  if (old_s.length === 0) return input;
  return input.split(old_s).join(new_s);
}
function String$to_int(s) { const n = parseInt(s,10); return isNaN(n) ? {_tag:0,_name:"None"} : {_tag:1,_name:"Some",_val:n}; }
function String$to_float(s) { const f = parseFloat(s); return isNaN(f) ? {_tag:0,_name:"None"} : {_tag:1,_name:"Some",_val:f}; }
function String$uppercase(s) { return s.toUpperCase(); }
function String$lowercase(s) { return s.toLowerCase(); }
function String$get(s, i) {
  if (i < 0 || i >= s.length) throw new Error("String.get: index " + i + " out of bounds (length " + s.length + ")");
  return s.charCodeAt(i);
}
function String$to_bytes(s) {
  let r = null;
  for (let i = s.length - 1; i >= 0; i--) r = {_hd: s.charCodeAt(i), _tl: r};
  return r;
}
function String$of_bytes(lst) {
  let r = "";
  let c = lst;
  while (c !== null) { r += String.fromCharCode(c._hd); c = c._tl; }
  return r;
}
function String$to_byte_array(s) {
  const a = new Array(s.length);
  for (let i = 0; i < s.length; i++) a[i] = s.charCodeAt(i);
  return {_arr: a};
}
function String$of_byte_array(a) {
  let r = "";
  for (let i = 0; i < a._arr.length; i++) r += String.fromCharCode(a._arr[i]);
  return r;
}
function String$to_runes(s) { return __string_to_runes(s); }
function String$of_runes(lst) {
  let r = "";
  let c = lst;
  while (c !== null) { r += String.fromCodePoint(c._hd); c = c._tl; }
  return r;
}
function String$get_rune(s, n) {
  const cps = Array.from(s);
  if (n < 0 || n >= cps.length) throw new Error("String.get_rune: index " + n + " out of bounds");
  return cps[n].codePointAt(0);
}
function String$of_byte(b) { return String.fromCharCode(b); }
function String$rune_length(s) { return Array.from(s).length; }
function String$make(n, b) {
  if (n < 0) throw new Error("String.make: negative length");
  return String.fromCharCode(b).repeat(n);
}
function String$index_opt(s, b) { const i=s.indexOf(String.fromCharCode(b)); return i<0?{_tag:0,_name:"None"}:{_tag:1,_name:"Some",_val:i}; }
function String$rindex_opt(s, b) { const i=s.lastIndexOf(String.fromCharCode(b)); return i<0?{_tag:0,_name:"None"}:{_tag:1,_name:"Some",_val:i}; }
function String$concat(sep, lst) { return string_concat(sep, lst); }
function String$compare(a, b) { return a < b ? -1 : a > b ? 1 : 0; }
// Array module
function Array$make(n, v) { return array_make(n, v); }
function Array$get(a, i) { return array_get(a, i); }
function Array$set(a, i, v) { return array_set(a, i, v); }
function Array$length(a) { return array_length(a); }
function Array$to_list(a) { return array_to_list(a); }
function Array$of_list(l) { return array_of_list(l); }
function Array$copy(a) { return array_copy(a); }
function Array$sub(a, s, l) { return array_sub(a, s, l); }
// IO module
function IO$read_file(path) {
  if (typeof globalThis._jsReadFile === "function") return globalThis._jsReadFile(path);
  if (typeof require !== "undefined") return require("fs").readFileSync(path,"utf8");
  throw new Error("IO.read_file: not available in this environment");
}
function IO$write_file(path, data) {
  if (typeof require !== "undefined") { require("fs").writeFileSync(path,data); return undefined; }
  throw new Error("IO.write_file: not available in this environment");
}
function IO$append_file(path, data) {
  if (typeof require !== "undefined") { require("fs").appendFileSync(path,data); return undefined; }
  throw new Error("IO.append_file: not available in this environment");
}
function IO$read_line(u) {
  if (typeof require !== "undefined") {
    const buf = Buffer.alloc(1);
    let line = "";
    const fd = require("fs").openSync("/dev/stdin", "rs");
    while (true) {
      const n = require("fs").readSync(fd, buf, 0, 1);
      if (n === 0 || buf[0] === 10) break;
      line += String.fromCharCode(buf[0]);
    }
    return line;
  }
  return "";
}
function IO$file_exists(path) {
  if (typeof require !== "undefined") return require("fs").existsSync(path);
  return false;
}
// Sys module
function Sys$args(u) {
  if (globalThis._jsSysArgs) {
    const a = globalThis._jsSysArgs;
    let r = null;
    for (let i = a.length - 1; i >= 0; i--) r = {_hd: a[i], _tl: r};
    return r;
  }
  if (typeof process !== "undefined") {
    const a = process.argv.slice(1);
    let r = null;
    for (let i = a.length - 1; i >= 0; i--) r = {_hd: a[i], _tl: r};
    return r;
  }
  return null;
}
function Sys$getenv(name) {
  if (typeof process !== "undefined") {
    const v = process.env[name];
    return v === undefined ? {_tag:0,_name:"None"} : {_tag:1,_name:"Some",_val:v};
  }
  return {_tag:0,_name:"None"};
}
function Sys$exit(code) { __sys_exit(code); }
function Sys$time(u) { return __sys_time(); }
// Runtime module (stubs — eval not supported in compiled JS)
function Runtime$eval(s) { throw new Error("Runtime.eval: not supported in compiled JS"); }
function Runtime$eval_file(s) { throw new Error("Runtime.eval_file: not supported in compiled JS"); }
// --- Typeclass primitive externs ---
function __num_add_int(a, b) { return a + b; }
function __num_sub_int(a, b) { return a - b; }
function __num_mul_int(a, b) { return a * b; }
function __num_div_int(a, b) { if (b === 0) throw new Error("division by zero"); return (a / b) | 0; }
function __num_neg_int(a) { return -a; }
function __num_add_float(a, b) { return a + b; }
function __num_sub_float(a, b) { return a - b; }
function __num_mul_float(a, b) { return a * b; }
function __num_div_float(a, b) { return a / b; }
function __num_neg_float(a) { return -a; }
function __eq_int(a, b) { return a === b; }
function __neq_int(a, b) { return a !== b; }
function __eq_float(a, b) { return a === b; }
function __neq_float(a, b) { return a !== b; }
function __eq_string(a, b) { return a === b; }
function __neq_string(a, b) { return a !== b; }
function __eq_bool(a, b) { return a === b; }
function __neq_bool(a, b) { return a !== b; }
function __eq_byte(a, b) { return a === b; }
function __neq_byte(a, b) { return a !== b; }
function __eq_rune(a, b) { return a === b; }
function __neq_rune(a, b) { return a !== b; }
function __lt_int(a, b) { return a < b; }
function __gt_int(a, b) { return a > b; }
function __le_int(a, b) { return a <= b; }
function __ge_int(a, b) { return a >= b; }
function __lt_float(a, b) { return a < b; }
function __gt_float(a, b) { return a > b; }
function __le_float(a, b) { return a <= b; }
function __ge_float(a, b) { return a >= b; }
function __lt_string(a, b) { return a < b; }
function __gt_string(a, b) { return a > b; }
function __le_string(a, b) { return a <= b; }
function __ge_string(a, b) { return a >= b; }
function __lt_byte(a, b) { return a < b; }
function __gt_byte(a, b) { return a > b; }
function __le_byte(a, b) { return a <= b; }
function __ge_byte(a, b) { return a >= b; }
function __lt_rune(a, b) { return a < b; }
function __gt_rune(a, b) { return a > b; }
function __le_rune(a, b) { return a <= b; }
function __ge_rune(a, b) { return a >= b; }
function __band_int(a, b) { return a & b; }
function __bor_int(a, b) { return a | b; }
function __bxor_int(a, b) { return a ^ b; }
function __bshl_int(a, b) { return a << b; }
function __bshr_int(a, b) { return a >> b; }
function __bnot_int(a) { return ~a; }
function __show_int(a) { return String(a); }
function __show_float(a) { return string_of_float(a); }
function __show_bool(a) { return String(a); }
function __show_string(a) { return a; }
function __show_unit(_) { return "()"; }
function __show_byte(a) { return "#" + a.toString(16).padStart(2, "0"); }
function __show_rune(a) { return String.fromCodePoint(a); }
function __index_at_array(i, arr) { if (i < 0 || i >= arr.length) throw new Error("array index out of bounds: " + i + " (length " + arr.length + ")"); return arr[i]; }
function __index_at_string(i, s) { if (i < 0 || i >= s.length) throw new Error("string index out of bounds: " + i + " (length " + s.length + ")"); return s.charCodeAt(i); }
// --- Canvas builtins (browser only) ---
function Canvas$init(w, h) {
  if (typeof globalThis._canvasInit === "function") globalThis._canvasInit(w, h);
  return undefined;
}
function Canvas$clear(color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.fillStyle = color; ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height); }
  return undefined;
}
function Canvas$fill_rect(x, y, w, h, color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.fillStyle = color; ctx.fillRect(x, y, w, h); }
  return undefined;
}
function Canvas$stroke_rect(x, y, w, h, color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.strokeStyle = color; ctx.lineWidth = 1; ctx.strokeRect(x, y, w, h); }
  return undefined;
}
function Canvas$fill_circle(x, y, r, color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.fillStyle = color; ctx.beginPath(); ctx.arc(x, y, r, 0, 2*Math.PI); ctx.fill(); }
  return undefined;
}
function Canvas$draw_text(text, x, y, color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.fillStyle = color; ctx.textBaseline = "top"; ctx.fillText(text, x, y); }
  return undefined;
}
function Canvas$set_font(font) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.font = font; }
  return undefined;
}
function Canvas$mouse_x(_) { return globalThis._canvasMouseX || 0; }
function Canvas$mouse_y(_) { return globalThis._canvasMouseY || 0; }
function Canvas$mouse_down(_) { return !!globalThis._canvasMouseDown; }
function Canvas$mouse_clicked(_) { return !!globalThis._canvasMouseClicked; }
function Canvas$key_down(key) { return !!(globalThis._canvasKeysDown && globalThis._canvasKeysDown[key]); }
function Canvas$key_pressed(key) { return !!(globalThis._canvasKeysPressed && globalThis._canvasKeysPressed[key]); }
function Canvas$start_app(init_fn, frame_fn) {
  globalThis._canvasApp = { initFn: init_fn, frameFn: frame_fn, jsMode: true, call: _call };
  return undefined;
}
|}

(* ---- Entry point ---- *)

let emit_exports ctx =
  emit ctx "var _mml_exports = {\"_result\": _last_val, \"_call\": _call, \"_pp\": _pp";
  List.iter (fun (mml_name, js_name) ->
    emit ctx (Printf.sprintf ", %s: %s" (escape_js_string mml_name) js_name)
  ) (List.rev ctx.top_level_exports);
  emit ctx "};\n";
  emit ctx "if (typeof globalThis !== \"undefined\") globalThis._mmlExports = _mml_exports;\n"

let compile_program type_env (program : Typechecker.tprogram) : string =
  let ctx = create_ctx type_env in
  (* Emit runtime *)
  emit ctx js_runtime;
  emit ctx js_builtins;
  emit ctx "// --- Compiled MiniML ---\n";
  emit ctx "let _last_val;\n";
  (* Compile all declarations *)
  List.iter (compile_decl ctx) program;
  (* Print last value if not unit *)
  emit ctx "if (_last_val !== undefined) println(_pp(_last_val));\n";
  emit_exports ctx;
  Buffer.contents ctx.buf

let compile_program_with_stdlib type_env
    (stdlib_programs : (Types.type_env * Typechecker.tprogram) list)
    (user_program : Typechecker.tprogram) : string =
  let ctx = create_ctx type_env in
  emit ctx js_runtime;
  emit ctx js_builtins;
  emit ctx "// --- Stdlib ---\n";
  List.iter (fun (_te, prog) ->
    (* Compile each stdlib declaration, rolling back on failure *)
    List.iter (fun decl ->
      let saved_pos = Buffer.length ctx.buf in
      let saved_indent = ctx.indent in
      try compile_decl ctx decl
      with Codegen_error _ ->
        (* Roll back partial output *)
        let full = Buffer.contents ctx.buf in
        Buffer.clear ctx.buf;
        Buffer.add_string ctx.buf (String.sub full 0 saved_pos);
        ctx.indent <- saved_indent
    ) prog
  ) stdlib_programs;
  emit ctx "// --- Compiled MiniML ---\n";
  emit ctx "let _last_val;\n";
  (* Snapshot exports before user program so we only export user bindings *)
  let stdlib_exports = ctx.top_level_exports in
  ctx.top_level_exports <- [];
  List.iter (compile_decl ctx) user_program;
  emit ctx "if (_last_val !== undefined) println(_pp(_last_val));\n";
  (* Only export user-program bindings, not stdlib internals *)
  let user_exports = ctx.top_level_exports in
  ctx.top_level_exports <- user_exports;
  ignore stdlib_exports;
  emit_exports ctx;
  Buffer.contents ctx.buf
