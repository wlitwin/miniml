(** Native code generation: typed AST -> LLVM IR text.

    Walks a [Typechecker.tprogram] and emits LLVM IR as a string.
    Phase 1: arithmetic, comparisons, booleans, let bindings, if/else,
    while loops, non-closure functions, globals, mutable locals, strings,
    and floats. *)

open Interpreter

(* ---- Value representation helpers ---- *)

(** Tag an integer as a MiniML tagged int: (n << 1) | 1 *)
let tag_int (n : int) : string =
  string_of_int ((n lsl 1) lor 1)

(** The unit value: tagged 0 = 1 *)
let unit_value = "1"

let true_value = "3"   (* tagged 1 *)
let false_value = "1"  (* tagged 0 = same as unit *)

(* ---- Target triple detection ---- *)

let run_cmd cmd =
  try
    let ic = Unix.open_process_in cmd in
    let result = String.trim (input_line ic) in
    ignore (Unix.close_process_in ic);
    result
  with _ -> ""

let detect_target_triple () =
  let arch = match run_cmd "uname -m" with "" -> "x86_64" | s -> s in
  let os = match run_cmd "uname -s" with "" -> "Linux" | s -> s in
  match os, arch with
  | "Darwin", arch ->
    let ver = match run_cmd "sw_vers -productVersion" with "" -> "11.0" | s -> s in
    let major = match String.split_on_char '.' ver with v :: _ -> v | [] -> "11" in
    Printf.sprintf "%s-apple-macosx%s.0.0"
      (if arch = "arm64" then "arm64" else "x86_64") major
  | _, "aarch64" -> "aarch64-unknown-linux-gnu"
  | _, _ -> "x86_64-unknown-linux-gnu"

(* ---- Codegen context ---- *)

type var_info =
  | Local of string         (* alloca register *)
  | MutLocal of string      (* alloca, assignable *)
  | MutRefCell of string    (* alloca holding heap ptr to 1-word cell, for closure-captured mutables *)
  | Global of string        (* @mml_g_<name> *)
  | MutGlobal of string     (* @mml_g_<name>, assignable *)
  | Func of string * int    (* llvm name, arity *)
  | FuncLocal of string * int * string  (* llvm name, arity, alloca holding pre-allocated closure *)

type codegen_ctx = {
  mutable ir: Ir_emit.t;
  fn_buf: Buffer.t;
  mutable scopes: (string, var_info) Hashtbl.t list;
  mutable current_label: string;
  mutable label_counter: int;
  mutable fn_counter: int;
  mutable str_counter: int;
  mutable float_counter: int;
  mutable string_globals: string list;
  mutable float_globals: string list;
  mutable extern_decls: (string * string * string list) list;
  mutable global_decls: string list;
  mutable loop_stack: (string * string) list;
  mutable result_type: Types.ty;
  mutable has_print_output: bool;
  mutable variant_defs: (string * Types.variant_def) list;
  mutable constructors: (string * (string * int)) list;
  type_env: Types.type_env;
  generated_wrappers: (string, unit) Hashtbl.t;
  mutable fold_break_depth: int;  (* >0 when inside fold callback, persists across function boundaries *)
  mutable or_pattern_allocas: (string * string) list;  (* shared allocas for or-pattern bindings: (name, alloca_ptr) *)
  mutable current_dict_name: string option;  (* dict currently being compiled, for self-reference *)
}

let create_ctx type_env =
  let tbl = Hashtbl.create 16 in
  (* Initialize variant_defs and constructors from type_env *)
  let variant_defs = List.map (fun (name, _nparams, vdef, _is_gadt) ->
    (name, vdef)
  ) type_env.Types.variants in
  let constructors = List.map (fun (name, info) ->
    let vdef = match List.assoc_opt info.Types.ctor_type_name variant_defs with
      | Some vdef -> vdef
      | None -> []
    in
    let tag = let rec find_tag i = function
      | [] -> 0
      | (cname, _) :: _ when cname = name -> i
      | _ :: rest -> find_tag (i + 1) rest
    in find_tag 0 vdef in
    (name, (info.ctor_type_name, tag))
  ) type_env.Types.constructors in
  {
    ir = Ir_emit.create ();
    fn_buf = Buffer.create 4096;
    scopes = [tbl];
    current_label = "entry";
    label_counter = 0;
    fn_counter = 0;
    str_counter = 0;
    float_counter = 0;
    string_globals = [];
    float_globals = [];
    extern_decls = [];
    global_decls = [];
    loop_stack = [];
    result_type = Types.TUnit;
    has_print_output = false;
    variant_defs;
    constructors;
    type_env;
    generated_wrappers = Hashtbl.create 16;
    fold_break_depth = 0;
    or_pattern_allocas = [];
    current_dict_name = None;
  }

let push_scope ctx =
  ctx.scopes <- Hashtbl.create 8 :: ctx.scopes

let pop_scope ctx =
  match ctx.scopes with
  | _ :: rest -> ctx.scopes <- rest
  | [] -> failwith "pop_scope: empty scope stack"

let bind_var ctx name info =
  match ctx.scopes with
  | top :: _ -> Hashtbl.replace top name info
  | [] -> failwith "bind_var: empty scope stack"

let lookup_var ctx name =
  let rec search = function
    | [] -> None
    | scope :: rest ->
      match Hashtbl.find_opt scope name with
      | Some v -> Some v
      | None -> search rest
  in
  search ctx.scopes

let fresh_label ctx prefix =
  let n = ctx.label_counter in
  ctx.label_counter <- n + 1;
  Printf.sprintf "%s_%d" prefix n

let add_extern ctx name ret_ty param_tys =
  if not (List.exists (fun (n, _, _) -> n = name) ctx.extern_decls) then
    ctx.extern_decls <- (name, ret_ty, param_tys) :: ctx.extern_decls

(* ---- String escaping for LLVM IR ---- *)

let llvm_escape_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
    let code = Char.code c in
    if code >= 32 && code < 127 && c <> '"' && c <> '\\' then
      Buffer.add_char buf c
    else
      Printf.bprintf buf "\\%02X" code
  ) s;
  buf

let llvm_encode_le_i64 n =
  let buf = Buffer.create 24 in
  let v = Int64.of_int n in
  for i = 0 to 7 do
    let byte = Int64.to_int (Int64.logand (Int64.shift_right_logical v (i * 8)) 0xFFL) in
    Printf.bprintf buf "\\%02X" byte
  done;
  Buffer.contents buf

(* ---- Float formatting for LLVM IR ---- *)

let llvm_format_float f =
  (* LLVM requires hex double format for exact representation *)
  let bits = Int64.bits_of_float f in
  Printf.sprintf "0x%LX" bits

(* ---- Header word tags (must match runtime.h) ---- *)

let mml_hdr_cons    = 0x00
let mml_hdr_tuple   = 0x01
let mml_hdr_record  = 0x02
let mml_hdr_closure = 0x03
let mml_hdr_variant = 0x04
let mml_hdr_array   = 0x05
(* let mml_hdr_string  = 0x06 *)  (* strings allocated by runtime *)
(* let mml_hdr_float   = 0x07 *)  (* floats allocated by runtime *)
(* let mml_hdr_avl     = 0x08 *)  (* AVL nodes allocated by runtime *)
let mml_hdr_pair    = 0x09
let mml_hdr_ref     = 0x0A
let mml_hdr_polyvar = 0x0B

let make_header ?(size=0) tag =
  string_of_int ((size lsl 16) lor tag)

(* ---- Heap allocation helper ---- *)

let emit_alloc ctx nbytes header =
  add_extern ctx "mml_alloc" "ptr" ["i64"; "i64"];
  Ir_emit.emit_call ctx.ir ~ret_ty:"ptr" ~name:"mml_alloc"
    ~args:[("i64", string_of_int nbytes); ("i64", header)]

(* ---- Constructor tag lookup ---- *)

let tag_for_constructor ctx name =
  let short = match String.rindex_opt name '.' with
    | Some i -> String.sub name (i + 1) (String.length name - i - 1)
    | None -> name
  in
  match List.assoc_opt short ctx.constructors with
  | Some (_, tag) -> tag
  | None ->
    failwith (Printf.sprintf "native codegen: unknown constructor %s (not yet implemented)" name)

let is_newtype_ctor ctx name =
  let short = match String.rindex_opt name '.' with
    | Some i -> String.sub name (i + 1) (String.length name - i - 1)
    | None -> name
  in
  match List.assoc_opt short ctx.constructors with
  | Some (type_name, _tag) -> List.mem type_name ctx.type_env.Types.newtypes
  | None -> false

(* ---- Type substitution for class instance types ---- *)

(** Substitute TGen indices with concrete types from an instance definition.
    Class method types use TGen(0..N-1) for class type params.
    inst_tys provides the concrete types for each index. *)
let rec subst_tgens_in_ty inst_tys ty =
  match ty with
  | Types.TGen i when i < List.length inst_tys -> List.nth inst_tys i
  | Types.TArrow (a, e, r) ->
    Types.TArrow (subst_tgens_in_ty inst_tys a, e, subst_tgens_in_ty inst_tys r)
  | Types.TTuple ts -> Types.TTuple (List.map (subst_tgens_in_ty inst_tys) ts)
  | Types.TList t -> Types.TList (subst_tgens_in_ty inst_tys t)
  | Types.TArray t -> Types.TArray (subst_tgens_in_ty inst_tys t)
  | _ -> ty

(* ---- Record field index computation ---- *)

let field_index_from_type ty field_name =
  let row = match Types.repr ty with
    | Types.TRecord row -> row
    | _ -> failwith (Printf.sprintf "native codegen: field %s access on non-record (typeclass dict access requires Phase 3)" field_name)
  in
  let fields = Types.record_row_to_fields row in
  let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
  let rec find i = function
    | [] -> failwith (Printf.sprintf "native codegen: field %s not found in record type" field_name)
    | (name, _) :: _ when name = field_name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 sorted

let record_fields_from_type ty =
  let row = match Types.repr ty with
    | Types.TRecord row -> row
    | _ -> failwith "native codegen: record update on non-record"
  in
  let fields = Types.record_row_to_fields row in
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

(* ---- Closure helpers ---- *)

(** Allocate a fresh closure struct: {fn_ptr, arity, 0, captures...} *)
let emit_make_closure ctx ~fn_name ~arity ~captures =
  let n_captures = List.length captures in
  let total_slots = 3 + n_captures in
  let ptr = emit_alloc ctx (total_slots * 8) (make_header mml_hdr_closure) in
  (* Store fn_ptr at offset 0 *)
  let fn_ptr_i64 = Ir_emit.emit_ptrtoint ctx.ir
    ~value:(Printf.sprintf "@%s" fn_name) in
  let slot0 = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:fn_ptr_i64 ~ptr:slot0;
  (* Store arity at offset 1 *)
  let slot1 = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:(string_of_int arity) ~ptr:slot1;
  (* Store num_applied=0 at offset 2 *)
  let slot2 = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:2 in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:"0" ~ptr:slot2;
  (* Store captures at offset 3+ *)
  List.iteri (fun i cap_val ->
    let slot = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:(3 + i) in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:cap_val ~ptr:slot
  ) captures;
  Ir_emit.emit_ptrtoint ctx.ir ~value:ptr

(** Call a closure value with N args via mml_applyN (N<=3) or mml_apply *)
let emit_closure_apply ctx closure_val arg_vals =
  let n = List.length arg_vals in
  if n <= 3 then begin
    (* Use convenience wrapper mml_apply1/2/3 *)
    let apply_name = Printf.sprintf "mml_apply%d" n in
    let param_tys = List.init (n + 1) (fun _ -> "i64") in
    add_extern ctx apply_name "i64" param_tys;
    let all_args = ("i64", closure_val) :: List.map (fun v -> ("i64", v)) arg_vals in
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:apply_name ~args:all_args
  end else begin
    (* Pack args into a stack array and call mml_apply(closure, args, n) *)
    let arr_ptr = Ir_emit.emit_alloca_array ctx.ir ~ty:"i64" ~count:n in
    List.iteri (fun i v ->
      let slot = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:arr_ptr ~index:i in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:slot
    ) arg_vals;
    add_extern ctx "mml_apply" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_apply"
      ~args:[("i64", closure_val); ("ptr", arr_ptr); ("i64", string_of_int n)]
  end

(** Check if a variable name is captured by any lambda in the body expression *)
let rec is_captured_in_closures name (body : Typechecker.texpr) =
  let found = ref false in
  let rec scan (e : Typechecker.texpr) =
    if !found then ()
    else match e.expr with
    | TEFun _ ->
      let (params, fn_body) = let rec collect ps e =
        match e.Typechecker.expr with
        | TEFun (p, b, _) -> collect (p :: ps) b
        | _ -> (List.rev ps, e)
      in collect [] e in
      (* Check if 'name' is free in this lambda *)
      let param_set = List.fold_left (fun s p ->
        let tbl = Hashtbl.create 4 in Hashtbl.replace tbl p (); ignore s; tbl
      ) (Hashtbl.create 4) params in
      ignore param_set;
      (* Simple check: is name mentioned in fn_body and not a parameter? *)
      if not (List.mem name params) && mentions_var name fn_body then
        found := true;
      (* Also scan inside the lambda for nested captures *)
      scan fn_body
    | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEUnit
    | TEByte _ | TERune _ -> ()
    | TEVar _ -> ()
    | TEBinop (_, a, b) | TESeq (a, b) | TEApp (a, b) -> scan a; scan b
    | TEUnop (_, a) | TEReturn a | TEBreak a -> scan a
    | TEIf (c, t, e) -> scan c; scan t; scan e
    | TELet (n, _, init, body) ->
      scan init;
      if n <> name then scan body  (* if shadowed, don't scan body *)
    | TELetRec (n, _, init, body) ->
      if n <> name then (scan init; scan body)
    | TELetRecAnd (bindings, body) ->
      let names = List.map fst bindings in
      List.iter (fun (_, e) -> scan e) bindings;
      if not (List.mem name names) then scan body
    | TELetMut (n, init, body) ->
      scan init;
      if n <> name then scan body
    | TEAssign (_, e) -> scan e
    | TEWhile (c, b) -> scan c; scan b
    | TEContinueLoop -> ()
    | TETuple es -> List.iter scan es
    | TERecord fields -> List.iter (fun (_, e) -> scan e) fields
    | TERecordUpdate (base, overrides) -> scan base; List.iter (fun (_, e) -> scan e) overrides
    | TERecordUpdateIdx (base, pairs) -> scan base; List.iter (fun (i, v) -> scan i; scan v) pairs
    | TEField (e, _) -> scan e
    | TEFieldAssign (e1, _, e2) -> scan e1; scan e2
    | TEConstruct (_, arg) -> Option.iter scan arg
    | TEForLoop e | TEFoldContinue e -> scan e
    | TEIndex (e1, e2) -> scan e1; scan e2
    | TEMatch (scrut, arms, _) ->
      scan scrut;
      List.iter (fun (_, guard, body) ->
        Option.iter scan guard; scan body
      ) arms
    | TEPerform (_op, arg_e) -> scan arg_e
    | TEResume (k_e, v_e) -> scan k_e; scan v_e
    | TEHandle (body_e, arms) ->
      scan body_e;
      (* Handler arms are compiled as separate functions — variables used
         in arm bodies are effectively captured like in closures *)
      List.iter (fun arm -> match arm with
        | Typechecker.THReturn (n, e) ->
          if n <> name && mentions_var name e then found := true
          else scan e
        | Typechecker.THOp (_, arg, k, e) ->
          if arg <> name && k <> name && mentions_var name e then found := true
          else scan e
        | Typechecker.THOpProvide (_, arg, e) ->
          if arg <> name && mentions_var name e then found := true
          else scan e
        | Typechecker.THOpTry (_, arg, e) ->
          if arg <> name && mentions_var name e then found := true
          else scan e
      ) arms
    | _ -> ()
  in
  scan body;
  !found

and mentions_var name (e : Typechecker.texpr) =
  match e.expr with
  | TEVar n -> n = name
  | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEUnit
  | TEByte _ | TERune _ -> false
  | TEBinop (_, a, b) | TESeq (a, b) | TEApp (a, b) -> mentions_var name a || mentions_var name b
  | TEUnop (_, a) | TEReturn a | TEBreak a -> mentions_var name a
  | TEIf (c, t, e) -> mentions_var name c || mentions_var name t || mentions_var name e
  | TELet (n, _, init, body) -> mentions_var name init || (n <> name && mentions_var name body)
  | TELetRec (n, _, init, body) -> (n <> name && mentions_var name init) || (n <> name && mentions_var name body)
  | TELetMut (n, init, body) -> mentions_var name init || (n <> name && mentions_var name body)
  | TEAssign (n, e) -> n = name || mentions_var name e
  | TEFun (p, body, _) -> p <> name && mentions_var name body
  | TEWhile (c, b) -> mentions_var name c || mentions_var name b
  | TEContinueLoop -> false
  | TETuple es -> List.exists (mentions_var name) es
  | TERecord fields -> List.exists (fun (_, e) -> mentions_var name e) fields
  | TERecordUpdate (base, overrides) -> mentions_var name base || List.exists (fun (_, e) -> mentions_var name e) overrides
  | TERecordUpdateIdx (base, pairs) -> mentions_var name base || List.exists (fun (i, v) -> mentions_var name i || mentions_var name v) pairs
  | TEField (e, _) -> mentions_var name e
  | TEFieldAssign (e1, _, e2) -> mentions_var name e1 || mentions_var name e2
  | TEConstruct (_, arg) -> Option.is_some arg && mentions_var name (Option.get arg)
  | TEMatch (scrut, arms, _) ->
    mentions_var name scrut || List.exists (fun (_, guard, body) ->
      (match guard with Some g -> mentions_var name g | None -> false) || mentions_var name body
    ) arms
  | TELetRecAnd (bindings, body) ->
    List.exists (fun (_, e) -> mentions_var name e) bindings || mentions_var name body
  | TEForLoop e | TEFoldContinue e -> mentions_var name e
  | TEIndex (e1, e2) -> mentions_var name e1 || mentions_var name e2
  | TEPerform (_op, arg_e) -> mentions_var name arg_e
  | TEResume (k_e, v_e) -> mentions_var name k_e || mentions_var name v_e
  | TEHandle (body_e, arms) ->
    mentions_var name body_e ||
    List.exists (fun arm -> match arm with
      | Typechecker.THReturn (n, e) -> n <> name && mentions_var name e
      | Typechecker.THOp (_, arg, k, e) ->
        arg <> name && k <> name && mentions_var name e
      | Typechecker.THOpProvide (_, arg, e) ->
        arg <> name && mentions_var name e
      | Typechecker.THOpTry (_, arg, e) ->
        arg <> name && mentions_var name e
    ) arms
  | _ -> false

(* ---- Expression codegen ---- *)

let rec emit_expr (ctx : codegen_ctx) (expr : Typechecker.texpr) : string =
  match expr.expr with
  | TEInt n ->
    tag_int n
  | TEByte n -> tag_int n
  | TERune n -> tag_int n
  | TEBool true ->
    true_value
  | TEBool false ->
    false_value
  | TEUnit ->
    unit_value

  | TEString s ->
    let name = Printf.sprintf ".str.%d" ctx.str_counter in
    ctx.str_counter <- ctx.str_counter + 1;
    let escaped = Buffer.contents (llvm_escape_string s) in
    let slen = String.length s in
    let header_prefix = llvm_encode_le_i64 0x06 in (* MML_HDR_STRING *)
    let len_prefix = llvm_encode_le_i64 (slen * 2) in  (* stored shifted; MML_STR_LEN shifts back *)
    let total = 8 + 8 + slen + 1 in (* header + length prefix + data + NUL *)
    let decl = Printf.sprintf "@%s = private unnamed_addr constant [%d x i8] c\"%s%s%s\\00\", align 8"
      name total header_prefix len_prefix escaped in
    ctx.string_globals <- decl :: ctx.string_globals;
    (* Return pointer past the 8-byte header *)
    let gep = Ir_emit.emit_gep ctx.ir ~ty:"i8" ~ptr:(Printf.sprintf "@%s" name) ~index:8 in
    Ir_emit.emit_ptrtoint ctx.ir ~value:gep

  | TEFloat f ->
    let name = Printf.sprintf ".float.%d" ctx.float_counter in
    ctx.float_counter <- ctx.float_counter + 1;
    let decl = Printf.sprintf "@%s = private unnamed_addr constant { i64, double } { i64 %d, double %s }"
      name 0x07 (* MML_HDR_FLOAT *) (llvm_format_float f) in
    ctx.float_globals <- decl :: ctx.float_globals;
    (* Return pointer past the 8-byte header to the double *)
    let gep = Ir_emit.emit_gep ctx.ir ~ty:"i8" ~ptr:(Printf.sprintf "@%s" name) ~index:8 in
    Ir_emit.emit_ptrtoint ctx.ir ~value:gep

  | TEUnop (Ast.Neg, e) ->
    let ty = Types.repr e.ty in
    (match ty with
     | Types.TFloat ->
       let v = emit_expr ctx e in
       let d = emit_unbox_float ctx v in
       let neg = Ir_emit.emit_fneg ctx.ir ~value:d in
       emit_box_float ctx neg
     | _ ->
       (* Integer negation: untag, negate, retag *)
       (match e.expr with
        | TEInt n -> tag_int (-n)
        | _ ->
          let v = emit_expr ctx e in
          let untagged = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:v ~rhs:"1" in
          let negated = Ir_emit.emit_binop ctx.ir ~op:"sub" ~ty:"i64" ~lhs:"0" ~rhs:untagged in
          let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:negated ~rhs:"1" in
          Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"))

  | TEUnop (Ast.Not, e) ->
    let v = emit_expr ctx e in
    (* Flip between true (3) and false (1): xor with 2 *)
    Ir_emit.emit_binop ctx.ir ~op:"xor" ~ty:"i64" ~lhs:v ~rhs:"2"

  | TEUnop (Ast.Lnot, e) ->
    (* Bitwise not: untag, not (xor -1), retag *)
    let v = emit_expr ctx e in
    let untagged = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:v ~rhs:"1" in
    let notted = Ir_emit.emit_binop ctx.ir ~op:"xor" ~ty:"i64" ~lhs:untagged ~rhs:"-1" in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:notted ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"

  | TEBinop (op, e1, e2) ->
    emit_binop ctx op e1 e2

  | TEApp (fn_expr, arg_expr) ->
    emit_app ctx fn_expr arg_expr

  | TESeq (e1, e2) ->
    ignore (emit_expr ctx e1);
    emit_expr ctx e2

  | TEVar name ->
    emit_var ctx name expr.ty

  | TELet (name, _scheme, init, body) ->
    emit_let ctx name init body false

  | TELetRec (name, _scheme, fn_expr, body) ->
    emit_letrec ctx name fn_expr body

  | TELetRecAnd (bindings, body) ->
    emit_letrec_and ctx bindings body

  | TELetMut (name, init, body) ->
    emit_let ctx name init body true

  | TEAssign (name, value_expr) ->
    let v = emit_expr ctx value_expr in
    (match lookup_var ctx name with
     | Some (MutLocal ptr) ->
       Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr;
       unit_value
     | Some (MutRefCell alloca_ptr) ->
       (* MutRefCell: load heap pointer, then store value through it *)
       let heap_ptr = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:alloca_ptr in
       let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:heap_ptr in
       Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr;
       unit_value
     | Some (MutGlobal gname) ->
       Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:(Printf.sprintf "@%s" gname);
       unit_value
     | _ -> failwith (Printf.sprintf "native codegen: cannot assign to %s" name))

  | TEIf (cond, then_e, else_e) ->
    emit_if ctx cond then_e else_e

  | TEWhile (cond, body) ->
    emit_while ctx cond body

  | TEBreak value_expr ->
    let v = emit_expr ctx value_expr in
    (match ctx.loop_stack with
     | (_, exit_label) :: _ ->
       (* While loop break *)
       Ir_emit.emit_br ctx.ir ~label:exit_label;
       let dead = fresh_label ctx "dead" in
       Ir_emit.emit_label ctx.ir dead;
       ctx.current_label <- dead;
       v  (* value doesn't matter, dead code *)
     | [] when ctx.fold_break_depth > 0 ->
       (* Fold break: set flag and return from callback *)
       add_extern ctx "mml_fold_break" "void" ["i64"];
       Ir_emit.emit_call_void ctx.ir ~name:"mml_fold_break" ~args:[("i64", v)];
       Ir_emit.emit_ret ctx.ir "i64" v;
       let dead = fresh_label ctx "dead" in
       Ir_emit.emit_label ctx.ir dead;
       ctx.current_label <- dead;
       v
     | [] -> failwith "native codegen: break outside loop")

  | TEContinueLoop ->
    (match ctx.loop_stack with
     | (cond_label, _) :: _ ->
       (* While loop continue *)
       Ir_emit.emit_br ctx.ir ~label:cond_label;
       let dead = fresh_label ctx "dead" in
       Ir_emit.emit_label ctx.ir dead;
       ctx.current_label <- dead;
       unit_value
     | [] when ctx.fold_break_depth > 0 ->
       (* Fold continue: return unit from callback to continue iteration *)
       Ir_emit.emit_ret ctx.ir "i64" unit_value;
       let dead = fresh_label ctx "dead" in
       Ir_emit.emit_label ctx.ir dead;
       ctx.current_label <- dead;
       unit_value
     | [] -> failwith "native codegen: continue outside loop")

  | TEFun _ ->
    emit_lambda_as_closure ctx expr

  | TEReturn e ->
    let v = emit_expr ctx e in
    if ctx.fold_break_depth > 0 then begin
      (* Inside a fold callback: use early return mechanism *)
      add_extern ctx "mml_set_early_return" "void" ["i64"];
      Ir_emit.emit_call_void ctx.ir ~name:"mml_set_early_return" ~args:[("i64", v)];
      Ir_emit.emit_ret ctx.ir "i64" v
    end else
      Ir_emit.emit_ret ctx.ir "i64" v;
    let dead = fresh_label ctx "dead" in
    Ir_emit.emit_label ctx.ir dead;
    ctx.current_label <- dead;
    v

  | TETuple exprs ->
    emit_tuple ctx exprs

  | TERecord fields ->
    emit_record ctx fields

  | TEField (record_expr, field_name) ->
    emit_field ctx record_expr field_name expr.ty

  | TERecordUpdate (base, overrides) ->
    emit_record_update ctx base overrides

  | TERecordUpdateIdx (base, pairs) ->
    emit_record_update_idx ctx base pairs

  | TEFieldAssign (record_expr, field_name, value_expr) ->
    emit_field_assign ctx record_expr field_name value_expr

  | TEConstruct (name, arg) ->
    emit_construct ctx name arg

  | TEMatch (scrutinee, arms, _partial) ->
    emit_match ctx scrutinee arms

  | TENil ->
    unit_value

  | TECons (hd_expr, tl_expr) ->
    let hd_val = emit_expr ctx hd_expr in
    let tl_val = emit_expr ctx tl_expr in
    let ptr = emit_alloc ctx 16 (make_header mml_hdr_cons) in
    let hd_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:hd_val ~ptr:hd_ptr;
    let tl_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:tl_val ~ptr:tl_ptr;
    Ir_emit.emit_ptrtoint ctx.ir ~value:ptr

  | TEArray elems ->
    let n = List.length elems in
    if n = 0 then unit_value  (* empty array = MML_UNIT *)
    else begin
      let vals = List.map (emit_expr ctx) elems in
      (* Allocate flat buffer: [length (i64)] [elem_0] ... [elem_{n-1}] *)
      let total_bytes = (n + 1) * 8 in
      let ptr = emit_alloc ctx total_bytes (make_header mml_hdr_array) in
      (* Store length (raw, not tagged) *)
      let len_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:(string_of_int n) ~ptr:len_ptr;
      (* Store each element *)
      List.iteri (fun i v ->
        let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:(i + 1) in
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:elem_ptr
      ) vals;
      Ir_emit.emit_ptrtoint ctx.ir ~value:ptr
    end

  | TEForLoop fold_expr ->
    ctx.fold_break_depth <- ctx.fold_break_depth + 1;
    let result = emit_for_loop_expr ctx fold_expr in
    ctx.fold_break_depth <- ctx.fold_break_depth - 1;
    (* Check for early return (return inside fold callback) *)
    add_extern ctx "mml_check_early_return" "i64" [];
    let flag = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_check_early_return" ~args:[] in
    let has_return = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:flag ~rhs:"0" in
    let early_ret = fresh_label ctx "early_ret" in
    let no_ret = fresh_label ctx "no_early_ret" in
    Ir_emit.emit_condbr ctx.ir ~cond:has_return ~if_true:early_ret ~if_false:no_ret;
    Ir_emit.emit_label ctx.ir early_ret;
    ctx.current_label <- early_ret;
    add_extern ctx "mml_get_early_return" "i64" [];
    let ret_val = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_get_early_return" ~args:[] in
    Ir_emit.emit_ret ctx.ir "i64" ret_val;
    Ir_emit.emit_label ctx.ir no_ret;
    ctx.current_label <- no_ret;
    result

  | TEFoldContinue value_expr ->
    (* TEFoldContinue acts like 'return' from the fold callback *)
    let v = emit_expr ctx value_expr in
    Ir_emit.emit_ret ctx.ir "i64" v;
    let dead = fresh_label ctx "dead" in
    Ir_emit.emit_label ctx.ir dead;
    ctx.current_label <- dead;
    v

  | TEIndex (base_expr, idx_expr) ->
    let base_val = emit_expr ctx base_expr in
    let idx_val = emit_expr ctx idx_expr in
    let base_ty = Types.repr base_expr.ty in
    (match base_ty with
     | Types.TString ->
       add_extern ctx "mml_string_get_byte" "i64" ["i64"; "i64"];
       Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_get_byte"
         ~args:[("i64", idx_val); ("i64", base_val)]
     | Types.TArray _ ->
       add_extern ctx "mml_array_get" "i64" ["i64"; "i64"];
       Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_get"
         ~args:[("i64", idx_val); ("i64", base_val)]
     | _ ->
       add_extern ctx "mml_list_nth" "i64" ["i64"; "i64"];
       Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_nth"
         ~args:[("i64", idx_val); ("i64", base_val)])

  | TEPerform (op_name, arg_expr) ->
    let arg_val = emit_expr ctx arg_expr in
    let op_str = emit_c_string ctx op_name in
    add_extern ctx "mml_perform_op" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_perform_op"
      ~args:[("i64", op_str); ("i64", arg_val)]

  | TEHandle (body_expr, arms) ->
    emit_handle ctx body_expr arms

  | TEResume (k_expr, val_expr) ->
    let k_val = emit_expr ctx k_expr in
    let v_val = emit_expr ctx val_expr in
    add_extern ctx "mml_resume_continuation" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_resume_continuation"
      ~args:[("i64", k_val); ("i64", v_val)]

(* ---- Variable lookup ---- *)

and emit_var ctx name expr_ty =
  match lookup_var ctx name with
  | Some (Local ptr) ->
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr
  | Some (MutLocal ptr) ->
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr
  | Some (MutRefCell alloca_ptr) ->
    (* MutRefCell: load heap pointer, then load value through it *)
    let heap_ptr = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:alloca_ptr in
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:heap_ptr in
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr
  | Some (Global gname) ->
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:(Printf.sprintf "@%s" gname)
  | Some (MutGlobal gname) ->
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:(Printf.sprintf "@%s" gname)
  | Some (Func (llvm_name, arity)) ->
    emit_func_as_closure ctx llvm_name arity
  | Some (FuncLocal (_, _, alloca_ptr)) ->
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:alloca_ptr
  | None ->
    (* Check for known builtins/operators *)
    (match emit_builtin_as_value ctx name expr_ty with
     | Some v -> v
     | None ->
       if is_dict_or_inst name then
         emit_or_cache_builtin_dict ctx name
       else
         failwith (Printf.sprintf "native codegen: unbound variable %s (not yet implemented)" name))

(* ---- Let bindings ---- *)

and emit_let ctx name init body is_mutable =
  (* Check if init is a function — emit as named function if possible *)
  (match init.Typechecker.expr with
   | TEFun _ when not is_mutable ->
     let (params, fn_body) = flatten_fun init in
     if params <> [] then begin
       let fn_id = ctx.fn_counter in
       ctx.fn_counter <- ctx.fn_counter + 1;
       let llvm_name = Printf.sprintf "mml_f_%s_%d" (sanitize_name name) fn_id in
       let free = free_vars_of_fun ~ctx:(Some ctx) params fn_body in
       if free = [] then begin
         (* No captures: emit as direct function + pre-allocate closure *)
         let arity = List.length params in
         emit_named_function ctx llvm_name params fn_body;
         (* Create wrapper and pre-allocate one closure for value use *)
         let wrapper_name = Printf.sprintf "%s_wrap" llvm_name in
         if not (Hashtbl.mem ctx.generated_wrappers wrapper_name) then begin
           Hashtbl.replace ctx.generated_wrappers wrapper_name ();
           emit_func_wrapper ctx wrapper_name llvm_name arity
         end;
         let closure_val = emit_make_closure ctx ~fn_name:wrapper_name ~arity ~captures:[] in
         let ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
         Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:closure_val ~ptr;
         push_scope ctx;
         bind_var ctx name (FuncLocal (llvm_name, arity, ptr));
         let result = emit_expr ctx body in
         pop_scope ctx;
         result
       end else begin
         (* Has captures: emit as closure function, bind as Local (closure value) *)
         let free_with_info = List.map (fun n ->
           match lookup_var ctx n with
           | Some info -> (n, info)
           | None -> failwith (Printf.sprintf "native codegen: free var %s not found" n)
         ) free in
         let closure_fn_name = Printf.sprintf "%s_cls" llvm_name in
         emit_closure_function ctx closure_fn_name params fn_body free_with_info;
         let captures = List.map (fun (n, info) ->
           emit_capture_value ctx n info
         ) free_with_info in
         let closure_val = emit_make_closure ctx
           ~fn_name:closure_fn_name
           ~arity:(List.length params)
           ~captures in
         let ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
         Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:closure_val ~ptr;
         push_scope ctx;
         bind_var ctx name (Local ptr);
         let result = emit_expr ctx body in
         pop_scope ctx;
         result
       end
     end else
       emit_let_value ctx name init body is_mutable
   | _ ->
     emit_let_value ctx name init body is_mutable)

and emit_let_value ctx name init body is_mutable =
  let v = emit_expr ctx init in
  if is_mutable && is_captured_in_closures name body then begin
    (* Mutable variable captured by a closure: use heap ref cell *)
    let cell_ptr = emit_alloc ctx 8 (make_header mml_hdr_ref) in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:cell_ptr;
    (* Store the heap pointer in an alloca *)
    let cell_i64 = Ir_emit.emit_ptrtoint ctx.ir ~value:cell_ptr in
    let alloca_ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:cell_i64 ~ptr:alloca_ptr;
    push_scope ctx;
    bind_var ctx name (MutRefCell alloca_ptr);
    let result = emit_expr ctx body in
    pop_scope ctx;
    result
  end else begin
    let ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr;
    push_scope ctx;
    bind_var ctx name (if is_mutable then MutLocal ptr else Local ptr);
    let result = emit_expr ctx body in
    pop_scope ctx;
    result
  end

(* ---- Recursive let bindings ---- *)

(* Emit a placeholder heap block for recursive value binding. Returns the i64
   value (ptrtoint of the allocated block). Also returns the number of data bytes
   so memcpy can be used for backpatching. *)
and emit_rec_placeholder ctx te =
  let rec go te =
    match te.Typechecker.expr with
    | Typechecker.TECons _ ->
      let ptr = emit_alloc ctx 16 (make_header mml_hdr_cons) in
      let hd_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:hd_ptr;
      let tl_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:tl_ptr;
      (Ir_emit.emit_ptrtoint ctx.ir ~value:ptr, 16)
    | Typechecker.TETuple es ->
      let n = List.length es in
      let nbytes = n * 8 in
      let ptr = emit_alloc ctx nbytes (make_header ~size:n mml_hdr_tuple) in
      for i = 0 to n - 1 do
        let p = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:p
      done;
      (Ir_emit.emit_ptrtoint ctx.ir ~value:ptr, nbytes)
    | Typechecker.TERecord fields ->
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
      let n = List.length sorted in
      let nbytes = n * 8 in
      let ptr = emit_alloc ctx nbytes (make_header ~size:n mml_hdr_record) in
      for i = 0 to n - 1 do
        let p = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:p
      done;
      (Ir_emit.emit_ptrtoint ctx.ir ~value:ptr, nbytes)
    | Typechecker.TEConstruct (name, payload_opt) ->
      if is_newtype_ctor ctx name then begin
        match payload_opt with
        | Some inner -> go inner
        | None -> failwith "native codegen: cannot create placeholder for nullary newtype"
      end else begin
        match payload_opt with
        | Some _ ->
          let tag = if String.length name > 0 && name.[0] = '`' then
            Hashtbl.hash (String.sub name 1 (String.length name - 1)) land 0x3FFFFFFF
          else tag_for_constructor ctx name
          in
          let hdr_tag = if String.length name > 0 && name.[0] = '`'
            then mml_hdr_polyvar else mml_hdr_variant in
          let ptr = emit_alloc ctx 16 (make_header hdr_tag) in
          let tag_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
          Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:(tag_int tag) ~ptr:tag_ptr;
          let payload_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
          Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:payload_ptr;
          (Ir_emit.emit_ptrtoint ctx.ir ~value:ptr, 16)
        | None ->
          failwith "native codegen: cannot create placeholder for nullary variant"
      end
    | Typechecker.TEArray es ->
      let n = List.length es in
      let total_bytes = (n + 1) * 8 in
      let ptr = emit_alloc ctx total_bytes (make_header mml_hdr_array) in
      let len_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:(string_of_int n) ~ptr:len_ptr;
      for i = 1 to n do
        let p = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:p
      done;
      (Ir_emit.emit_ptrtoint ctx.ir ~value:ptr, total_bytes)
    | Typechecker.TELet (_, _, _, body) -> go body
    | Typechecker.TESeq (_, body) -> go body
    | _ -> failwith "native codegen: cannot create placeholder for recursive value"
  in
  go te

(* Backpatch a placeholder by copying nbytes from computed into placeholder *)
and emit_backpatch ctx placeholder_val computed_val nbytes =
  let p_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:placeholder_val in
  let c_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:computed_val in
  add_extern ctx "memcpy" "ptr" ["ptr"; "ptr"; "i64"];
  ignore (Ir_emit.emit_call ctx.ir ~ret_ty:"ptr" ~name:"memcpy"
    ~args:[("ptr", p_ptr); ("ptr", c_ptr); ("i64", string_of_int nbytes)])

and emit_letrec ctx name fn_expr body =
  (* Check if it's a function we can emit *)
  let (params, inner_body) = flatten_fun fn_expr in
  if params = [] then begin
    (* Non-function recursive value binding: placeholder + backpatch *)
    push_scope ctx;
    let (placeholder_val, nbytes) = emit_rec_placeholder ctx fn_expr in
    let alloca = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:placeholder_val ~ptr:alloca;
    bind_var ctx name (Local alloca);
    let computed_val = emit_expr ctx fn_expr in
    emit_backpatch ctx placeholder_val computed_val nbytes;
    let result = emit_expr ctx body in
    pop_scope ctx;
    result
  end else begin
  let fn_id = ctx.fn_counter in
  ctx.fn_counter <- ctx.fn_counter + 1;
  let llvm_name = Printf.sprintf "mml_f_%s_%d" (sanitize_name name) fn_id in
  let free = free_vars_of_fun ~ctx:(Some ctx) (name :: params) inner_body in
  if free = [] then begin
    push_scope ctx;
    bind_var ctx name (Func (llvm_name, List.length params));
    emit_named_function ctx llvm_name params inner_body;
    let result = emit_expr ctx body in
    pop_scope ctx;
    result
  end else begin
    (* Closure with captures: emit function that loads captures from env *)
    push_scope ctx;
    (* For each captured variable, get its value and track if it's mutable *)
    let capture_infos = List.map (fun v ->
      match lookup_var ctx v with
      | Some (MutLocal ptr) ->
        (* Mutable: capture the alloca pointer as i64 *)
        let ptr_i64 = Ir_emit.emit_ptrtoint ctx.ir ~value:ptr in
        (v, ptr_i64, `Mutable)
      | Some (MutRefCell alloca_ptr) ->
        (* MutRefCell: capture the heap pointer from the alloca *)
        let heap_ptr = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:alloca_ptr in
        (v, heap_ptr, `RefCell)
      | _ ->
        (* Immutable: capture the value *)
        (v, emit_var ctx v Types.TUnit, `Immutable)
    ) free in
    let capture_vals = List.map (fun (_, v, _) -> v) capture_infos in
    let arity = List.length params in
    (* Build the closure *)
    let closure_val = emit_make_closure ctx ~fn_name:llvm_name ~arity ~captures:capture_vals in
    (* Bind name as a local holding the closure *)
    let closure_alloca = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:closure_val ~ptr:closure_alloca;
    bind_var ctx name (Local closure_alloca);
    (* Emit the function with capture loading from env *)
    let capture_modes = List.map (fun (_, _, m) -> m) capture_infos in
    emit_named_function_with_captures ctx llvm_name params inner_body name free capture_modes;
    let result = emit_expr ctx body in
    pop_scope ctx;
    result
  end
  end

(* ---- Mutual recursive let bindings ---- *)

and emit_letrec_and ctx bindings body =
  push_scope ctx;
  let all_names = List.map fst bindings in
  (* Classify each binding as function or value *)
  let binding_info = List.map (fun (name, fn_expr) ->
    let (params, _) = flatten_fun fn_expr in
    if params <> [] then `Func (name, fn_expr)
    else `Value (name, fn_expr)
  ) bindings in
  (* Phase 1: Register all names. Functions get Func bindings, values get placeholders. *)
  let value_infos = ref [] in
  List.iter (fun info ->
    match info with
    | `Func (name, fn_expr) ->
      let (params, _) = flatten_fun fn_expr in
      let fn_id = ctx.fn_counter in
      ctx.fn_counter <- ctx.fn_counter + 1;
      let llvm_name = Printf.sprintf "mml_f_%s_%d" (sanitize_name name) fn_id in
      bind_var ctx name (Func (llvm_name, List.length params))
    | `Value (name, fn_expr) ->
      let (placeholder_val, nbytes) = emit_rec_placeholder ctx fn_expr in
      let alloca = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:placeholder_val ~ptr:alloca;
      bind_var ctx name (Local alloca);
      value_infos := (name, fn_expr, placeholder_val, nbytes) :: !value_infos
  ) binding_info;
  let value_infos = List.rev !value_infos in
  (* Phase 2: Emit all functions *)
  List.iter (fun info ->
    match info with
    | `Func (name, fn_expr) ->
      let (params, inner_body) = flatten_fun fn_expr in
      let llvm_name = match lookup_var ctx name with
        | Some (Func (n, _)) -> n | _ -> failwith "expected Func" in
      let free = free_vars_of_fun ~ctx:(Some ctx) (all_names @ params) inner_body in
      if free <> [] then
        failwith (Printf.sprintf "native codegen: closure captures free variables: %s (Phase 3)"
          (String.concat ", " free));
      emit_named_function ctx llvm_name params inner_body
    | `Value _ -> ()
  ) binding_info;
  (* Phase 3: Compile and backpatch all value bindings *)
  List.iter (fun (_, fn_expr, placeholder_val, nbytes) ->
    let computed_val = emit_expr ctx fn_expr in
    emit_backpatch ctx placeholder_val computed_val nbytes
  ) value_infos;
  let result = emit_expr ctx body in
  pop_scope ctx;
  result

(* ---- Binary operations ---- *)

(** Find a custom typeclass instance for a concrete type and class name.
    Returns the dict name if found. Used to dispatch binops through custom instances. *)
and find_custom_instance ctx class_name concrete_ty =
  List.find_opt (fun (inst : Types.instance_def) ->
    inst.inst_class = class_name &&
    inst.inst_constraints = [] &&
    (match inst.inst_tys with
     | [ty] -> Types.match_partial_inst [ty] [Some concrete_ty]
     | _ -> false)
  ) ctx.type_env.Types.instances

(** Dispatch a binary operation through a typeclass dictionary *)
and emit_dict_dispatch_binop ctx dict_name method_name e1 e2 =
  let dict_val = emit_var ctx dict_name Types.TUnit in
  let dict_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:dict_val in
  let idx = dict_method_index ctx method_name in
  let method_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:dict_ptr ~index:idx in
  let method_closure = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:method_ptr in
  let a = emit_expr ctx e1 in
  let b = emit_expr ctx e2 in
  add_extern ctx "mml_apply2" "i64" ["i64"; "i64"; "i64"];
  Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_apply2"
    ~args:[("i64", method_closure); ("i64", a); ("i64", b)]

and emit_binop ctx op e1 e2 =
  match op with
  | Ast.And -> emit_short_circuit_and ctx e1 e2
  | Ast.Or -> emit_short_circuit_or ctx e1 e2
  | Ast.Concat -> emit_concat ctx e1 e2
  | Ast.Pipe ->
    (* e1 |> e2 is sugar for e2(e1) *)
    emit_app ctx e2 e1
  | _ ->
    let ty1 = Types.repr e1.ty in
    (* Check for custom typeclass instance dispatch *)
    let class_info = match op with
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> Some ("Num", match op with
          | Ast.Add -> "+" | Ast.Sub -> "-" | Ast.Mul -> "*" | Ast.Div -> "/" | _ -> "+")
      | Ast.Eq -> Some ("Eq", "=") | Ast.Neq -> Some ("Eq", "<>")
      | Ast.Lt -> Some ("Ord", "<") | Ast.Gt -> Some ("Ord", ">")
      | Ast.Le -> Some ("Ord", "<=") | Ast.Ge -> Some ("Ord", ">=")
      | _ -> None
    in
    let custom_dispatch = match class_info with
      | Some (class_name, method_name) ->
        (match ty1 with
         | Types.TInt | Types.TFloat | Types.TString | Types.TBool | Types.TUnit -> None
         | _ ->
           (match find_custom_instance ctx class_name ty1 with
            | Some inst -> Some (inst.Types.inst_dict_name, method_name)
            | None -> None))
      | None -> None
    in
    (match custom_dispatch with
     | Some (dict_name, method_name) ->
       emit_dict_dispatch_binop ctx dict_name method_name e1 e2
     | None ->
       (match ty1 with
        | Types.TFloat -> emit_float_binop ctx op e1 e2
        | Types.TString -> emit_string_binop ctx op e1 e2
        | _ -> emit_int_binop ctx op e1 e2))

and emit_divmod_zero_check ctx divisor =
  emit_divmod_zero_check_ir ctx ctx.ir divisor

and emit_divmod_zero_check_ir ctx ir divisor =
  add_extern ctx "mml_check_div_zero" "void" ["i64"];
  Ir_emit.emit_call_void ir ~name:"mml_check_div_zero" ~args:[("i64", divisor)]

and emit_int_binop ctx op e1 e2 =
  let a = emit_expr ctx e1 in
  let b = emit_expr ctx e2 in
  match op with
  (* Arithmetic on tagged ints *)
  | Ast.Add ->
    (* (a + b) - 1: both have tag bit 1, adding gives 2 tag bits, subtract 1 *)
    let sum = Ir_emit.emit_binop ctx.ir ~op:"add" ~ty:"i64" ~lhs:a ~rhs:b in
    Ir_emit.emit_binop ctx.ir ~op:"sub" ~ty:"i64" ~lhs:sum ~rhs:"1"
  | Ast.Sub ->
    (* (a - b) + 1: tag bits cancel, add 1 back *)
    let diff = Ir_emit.emit_binop ctx.ir ~op:"sub" ~ty:"i64" ~lhs:a ~rhs:b in
    Ir_emit.emit_binop ctx.ir ~op:"add" ~ty:"i64" ~lhs:diff ~rhs:"1"
  | Ast.Mul ->
    (* untag both, multiply, retag *)
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    let prod = Ir_emit.emit_binop ctx.ir ~op:"mul" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:prod ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
  | Ast.Div ->
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    emit_divmod_zero_check ctx ub;
    let quot = Ir_emit.emit_binop ctx.ir ~op:"sdiv" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:quot ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
  | Ast.Mod ->
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    emit_divmod_zero_check ctx ub;
    let rem = Ir_emit.emit_binop ctx.ir ~op:"srem" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:rem ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"

  (* Comparison ops: tagged ints preserve ordering *)
  | Ast.Lt ->
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"slt" ~ty:"i64" ~lhs:a ~rhs:b in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Gt ->
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"sgt" ~ty:"i64" ~lhs:a ~rhs:b in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Le ->
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"sle" ~ty:"i64" ~lhs:a ~rhs:b in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Ge ->
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"sge" ~ty:"i64" ~lhs:a ~rhs:b in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Eq ->
    let ty1 = Types.repr e1.ty in
    let emit_eq_call name args =
      add_extern ctx name "i64" (List.map fst args);
      Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name ~args
    in
    (match ty1 with
     | Types.TInt | Types.TBool | Types.TUnit | Types.TByte | Types.TRune ->
       let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:a ~rhs:b in
       Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
     | Types.TString ->
       emit_eq_call "mml_string_eq" [("i64", a); ("i64", b)]
     | Types.TFloat ->
       let da = emit_unbox_float ctx a in
       let db = emit_unbox_float ctx b in
       let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"oeq" ~lhs:da ~rhs:db in
       Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
     | Types.TArray _ ->
       emit_eq_call "mml_array_eq" [("i64", a); ("i64", b)]
     | Types.TList _ ->
       emit_eq_call "mml_list_eq" [("i64", a); ("i64", b)]
     | Types.TTuple ts ->
       let n = List.length ts in
       add_extern ctx "mml_tuple_eq" "i64" ["i64"; "i64"; "i64"];
       Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_tuple_eq"
         ~args:[("i64", a); ("i64", b); ("i64", tag_int n)]
     | Types.TRecord _ ->
       emit_eq_call "mml_record_eq" [("i64", a); ("i64", b)]
     | _ ->
       emit_eq_call "mml_structural_eq" [("i64", a); ("i64", b)])
  | Ast.Neq ->
    let ty1 = Types.repr e1.ty in
    let emit_neq_via_eq name args =
      add_extern ctx name "i64" (List.map fst args);
      let eq = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name ~args in
      let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:eq ~rhs:true_value in
      Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:false_value ~if_false:true_value
    in
    (match ty1 with
     | Types.TInt | Types.TBool | Types.TUnit | Types.TByte | Types.TRune ->
       let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:a ~rhs:b in
       Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
     | Types.TString ->
       emit_neq_via_eq "mml_string_eq" [("i64", a); ("i64", b)]
     | Types.TFloat ->
       let da = emit_unbox_float ctx a in
       let db = emit_unbox_float ctx b in
       let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"une" ~lhs:da ~rhs:db in
       Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
     | Types.TArray _ ->
       emit_neq_via_eq "mml_array_eq" [("i64", a); ("i64", b)]
     | Types.TList _ ->
       emit_neq_via_eq "mml_list_eq" [("i64", a); ("i64", b)]
     | Types.TTuple ts ->
       let n = List.length ts in
       add_extern ctx "mml_tuple_eq" "i64" ["i64"; "i64"; "i64"];
       let eq = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_tuple_eq"
         ~args:[("i64", a); ("i64", b); ("i64", tag_int n)] in
       let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:eq ~rhs:true_value in
       Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:false_value ~if_false:true_value
     | Types.TRecord _ ->
       emit_neq_via_eq "mml_record_eq" [("i64", a); ("i64", b)]
     | _ ->
       emit_neq_via_eq "mml_structural_eq" [("i64", a); ("i64", b)])

  (* Bitwise ops *)
  | Ast.Land ->
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    let result = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:result ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
  | Ast.Lor ->
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    let result = Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:result ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
  | Ast.Lxor ->
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    let result = Ir_emit.emit_binop ctx.ir ~op:"xor" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:result ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
  | Ast.Lsl ->
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    let result = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:result ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
  | Ast.Lsr ->
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    let result = Ir_emit.emit_binop ctx.ir ~op:"lshr" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:result ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"

  | _ ->
    failwith (Printf.sprintf "native codegen: int binop not yet implemented")

(* ---- Float binary operations ---- *)

and emit_float_binop ctx op e1 e2 =
  let a = emit_expr ctx e1 in
  let b = emit_expr ctx e2 in
  match op with
  | Ast.Add ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let result = Ir_emit.emit_float_binop ctx.ir ~op:"fadd" ~lhs:da ~rhs:db in
    emit_box_float ctx result
  | Ast.Sub ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let result = Ir_emit.emit_float_binop ctx.ir ~op:"fsub" ~lhs:da ~rhs:db in
    emit_box_float ctx result
  | Ast.Mul ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let result = Ir_emit.emit_float_binop ctx.ir ~op:"fmul" ~lhs:da ~rhs:db in
    emit_box_float ctx result
  | Ast.Div ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let result = Ir_emit.emit_float_binop ctx.ir ~op:"fdiv" ~lhs:da ~rhs:db in
    emit_box_float ctx result
  | Ast.Lt ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"olt" ~lhs:da ~rhs:db in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Gt ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"ogt" ~lhs:da ~rhs:db in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Le ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"ole" ~lhs:da ~rhs:db in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Ge ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"oge" ~lhs:da ~rhs:db in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Eq ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"oeq" ~lhs:da ~rhs:db in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | Ast.Neq ->
    let da = emit_unbox_float ctx a in
    let db = emit_unbox_float ctx b in
    let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"one" ~lhs:da ~rhs:db in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | _ ->
    failwith "native codegen: unsupported float binop"

(* ---- String binary operations ---- *)

and emit_string_binop ctx op e1 e2 =
  let a = emit_expr ctx e1 in
  let b = emit_expr ctx e2 in
  match op with
  | Ast.Eq ->
    add_extern ctx "mml_string_eq" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_eq"
      ~args:[("i64", a); ("i64", b)]
  | Ast.Neq ->
    add_extern ctx "mml_string_eq" "i64" ["i64"; "i64"];
    let eq = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_eq"
      ~args:[("i64", a); ("i64", b)] in
    (* Flip bool *)
    Ir_emit.emit_binop ctx.ir ~op:"xor" ~ty:"i64" ~lhs:eq ~rhs:"2"
  | Ast.Lt | Ast.Gt | Ast.Le | Ast.Ge ->
    add_extern ctx "mml_string_compare" "i64" ["i64"; "i64"];
    let cmp_val = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_compare"
      ~args:[("i64", a); ("i64", b)] in
    let cmp_op = match op with
      | Ast.Lt -> "slt" | Ast.Gt -> "sgt"
      | Ast.Le -> "sle" | Ast.Ge -> "sge" | _ -> assert false in
    (* Compare result (tagged int: -1/0/1) against tagged 0 *)
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:cmp_op ~ty:"i64" ~lhs:cmp_val ~rhs:unit_value in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | _ ->
    failwith "native codegen: unsupported string binop"

(* ---- String concat ---- *)

and emit_concat ctx e1 e2 =
  let a = emit_expr ctx e1 in
  let b = emit_expr ctx e2 in
  add_extern ctx "mml_string_concat" "i64" ["i64"; "i64"];
  Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_concat"
    ~args:[("i64", a); ("i64", b)]

(* ---- Short-circuit boolean operators ---- *)

and emit_short_circuit_and ctx e1 e2 =
  let a_val = emit_expr ctx e1 in
  let is_false = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:a_val ~rhs:false_value in
  let and_rhs = fresh_label ctx "and_rhs" in
  let and_short = fresh_label ctx "and_short" in
  let and_done = fresh_label ctx "and_done" in
  Ir_emit.emit_condbr ctx.ir ~cond:is_false ~if_true:and_short ~if_false:and_rhs;

  Ir_emit.emit_label ctx.ir and_rhs;
  ctx.current_label <- and_rhs;
  let b_val = emit_expr ctx e2 in
  let rhs_end = ctx.current_label in
  Ir_emit.emit_br ctx.ir ~label:and_done;

  Ir_emit.emit_label ctx.ir and_short;
  Ir_emit.emit_br ctx.ir ~label:and_done;

  Ir_emit.emit_label ctx.ir and_done;
  ctx.current_label <- and_done;
  Ir_emit.emit_phi ctx.ir ~ty:"i64"
    ~incoming:[(false_value, and_short); (b_val, rhs_end)]

and emit_short_circuit_or ctx e1 e2 =
  let a_val = emit_expr ctx e1 in
  let is_false = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:a_val ~rhs:false_value in
  let or_rhs = fresh_label ctx "or_rhs" in
  let or_short = fresh_label ctx "or_short" in
  let or_done = fresh_label ctx "or_done" in
  Ir_emit.emit_condbr ctx.ir ~cond:is_false ~if_true:or_rhs ~if_false:or_short;

  Ir_emit.emit_label ctx.ir or_rhs;
  ctx.current_label <- or_rhs;
  let b_val = emit_expr ctx e2 in
  let rhs_end = ctx.current_label in
  Ir_emit.emit_br ctx.ir ~label:or_done;

  Ir_emit.emit_label ctx.ir or_short;
  Ir_emit.emit_br ctx.ir ~label:or_done;

  Ir_emit.emit_label ctx.ir or_done;
  ctx.current_label <- or_done;
  Ir_emit.emit_phi ctx.ir ~ty:"i64"
    ~incoming:[(true_value, or_short); (b_val, rhs_end)]

(* ---- If/else ---- *)

and emit_if ctx cond then_e else_e =
  let cond_val = emit_expr ctx cond in
  let is_false = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:cond_val ~rhs:false_value in
  let then_label = fresh_label ctx "then" in
  let else_label = fresh_label ctx "else" in
  let merge_label = fresh_label ctx "merge" in
  Ir_emit.emit_condbr ctx.ir ~cond:is_false ~if_true:else_label ~if_false:then_label;

  Ir_emit.emit_label ctx.ir then_label;
  ctx.current_label <- then_label;
  let then_val = emit_expr ctx then_e in
  let then_end = ctx.current_label in
  Ir_emit.emit_br ctx.ir ~label:merge_label;

  Ir_emit.emit_label ctx.ir else_label;
  ctx.current_label <- else_label;
  let else_val = emit_expr ctx else_e in
  let else_end = ctx.current_label in
  Ir_emit.emit_br ctx.ir ~label:merge_label;

  Ir_emit.emit_label ctx.ir merge_label;
  ctx.current_label <- merge_label;
  Ir_emit.emit_phi ctx.ir ~ty:"i64"
    ~incoming:[(then_val, then_end); (else_val, else_end)]

(* ---- While loops ---- *)

and emit_while ctx cond body =
  let cond_label = fresh_label ctx "while_cond" in
  let body_label = fresh_label ctx "while_body" in
  let exit_label = fresh_label ctx "while_exit" in

  Ir_emit.emit_br ctx.ir ~label:cond_label;

  Ir_emit.emit_label ctx.ir cond_label;
  ctx.current_label <- cond_label;
  let cond_val = emit_expr ctx cond in
  let is_false = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:cond_val ~rhs:false_value in
  Ir_emit.emit_condbr ctx.ir ~cond:is_false ~if_true:exit_label ~if_false:body_label;

  Ir_emit.emit_label ctx.ir body_label;
  ctx.current_label <- body_label;
  ctx.loop_stack <- (cond_label, exit_label) :: ctx.loop_stack;
  ignore (emit_expr ctx body);
  ctx.loop_stack <- List.tl ctx.loop_stack;
  Ir_emit.emit_br ctx.ir ~label:cond_label;

  Ir_emit.emit_label ctx.ir exit_label;
  ctx.current_label <- exit_label;
  unit_value

(* ---- Tuples ---- *)

and emit_tuple ctx exprs =
  let vals = List.map (emit_expr ctx) exprs in
  let n = List.length vals in
  let ptr = emit_alloc ctx (n * 8) (make_header ~size:n mml_hdr_tuple) in
  List.iteri (fun i v ->
    let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:elem_ptr
  ) vals;
  Ir_emit.emit_ptrtoint ctx.ir ~value:ptr

(* ---- Records ---- *)

and emit_record ctx fields =
  let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
  let vals = List.map (fun (_, e) -> emit_expr ctx e) sorted in
  let n = List.length vals in
  (* Size stored in header word at ptr[-1], no separate size slot needed *)
  let ptr = emit_alloc ctx (n * 8) (make_header ~size:n mml_hdr_record) in
  List.iteri (fun i v ->
    let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:elem_ptr
  ) vals;
  Ir_emit.emit_ptrtoint ctx.ir ~value:ptr

(** Lazily generate a built-in typeclass dictionary record for an unbound __dict variable.
    Looks up the instance in the type_env, finds the class methods, generates operator
    closures for each method, packages them as a record, and caches as a global.
    Falls back to unit_value if the instance can't be generated (unknown methods, etc.). *)
and emit_or_cache_builtin_dict ctx dict_name =
  (* Check if already cached from a previous call *)
  match lookup_var ctx dict_name with
  | Some _ -> emit_var ctx dict_name Types.TUnit
  | None when ctx.current_dict_name = Some dict_name ->
    (* Self-referencing dict (e.g. recursive fold in tree Iter instance).
       Load from the global which will be initialized by the time the recursive call runs. *)
    let gname = ensure_global ctx dict_name in
    bind_var ctx dict_name (Global gname);
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:(Printf.sprintf "@%s" gname)
  | None ->
    let result = match List.find_opt (fun inst ->
      inst.Types.inst_dict_name = dict_name
    ) ctx.type_env.Types.instances with
    | Some inst when inst.Types.inst_constraints = [] ->
      (match List.find_opt (fun (c : Types.class_def) ->
        c.Types.class_name = inst.Types.inst_class
      ) ctx.type_env.Types.classes with
      | Some class_def ->
        let sorted_methods = List.sort (fun (a, _) (b, _) -> String.compare a b)
          class_def.Types.class_methods in
        let n = List.length sorted_methods in
        if n = 0 then None
        else begin
          (* Save fn_buf position and ctx.ir so we can roll back on failure *)
          let fn_buf_len = Buffer.length ctx.fn_buf in
          let saved_ir = ctx.ir in
          (try
            let method_closures = List.map (fun (method_name, method_ty) ->
              let concrete_ty = subst_tgens_in_ty inst.Types.inst_tys method_ty in
              emit_operator_closure ctx method_name concrete_ty
            ) sorted_methods in
            let ptr = emit_alloc ctx (n * 8) (make_header ~size:n mml_hdr_record) in
            List.iteri (fun i closure_val ->
              let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
              Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:closure_val ~ptr:elem_ptr
            ) method_closures;
            let dict_val = Ir_emit.emit_ptrtoint ctx.ir ~value:ptr in
            (* Cache as a global so subsequent lookups find it *)
            let gname = ensure_global ctx dict_name in
            Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:dict_val
              ~ptr:(Printf.sprintf "@%s" gname);
            bind_var ctx dict_name (Global gname);
            Some dict_val
          with Failure _ ->
            (* Restore ctx.ir in case emit_operator_wrapper changed it *)
            ctx.ir <- saved_ir;
            (* Roll back any partial wrapper functions written to fn_buf *)
            let contents = Buffer.contents ctx.fn_buf in
            Buffer.clear ctx.fn_buf;
            Buffer.add_string ctx.fn_buf (String.sub contents 0 fn_buf_len);
            None)
        end
      | None -> None)
    | _ -> None  (* constrained instance or not found *)
    in
    match result with
    | Some v -> v
    | None -> unit_value

and dict_method_index ctx field_name =
  match List.find_opt (fun (c : Types.class_def) ->
    List.exists (fun (name, _) -> name = field_name) c.class_methods
  ) ctx.type_env.Types.classes with
  | Some class_def ->
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) class_def.class_methods in
    let rec find i = function
      | [] -> failwith (Printf.sprintf "native codegen: method %s not found in class" field_name)
      | (name, _) :: _ when name = field_name -> i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 sorted
  | None ->
    failwith (Printf.sprintf "native codegen: field %s not found in any class or record type" field_name)

and emit_field ctx (record_expr : Typechecker.texpr) field_name _expr_ty =
  let v = emit_expr ctx record_expr in
  let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:v in
  (* Check if the record type has an open row (polymorphic) and we have evidence *)
  let has_open_row row =
    let rec check r = match Types.rrow_repr r with
      | Types.RRow (_, _, tail) -> check tail
      | Types.RVar { contents = Types.RUnbound _ } -> true
      | _ -> false
    in check row
  in
  let ev_param = match Types.repr record_expr.ty with
    | Types.TRecord row when has_open_row row ->
      (* Look for evidence parameter __ev_{field}_r{N} in scope *)
      let rec try_rgen i =
        if i > 10 then None
        else
          let name = Printf.sprintf "__ev_%s_r%d" field_name i in
          match lookup_var ctx name with
          | Some _ -> Some name
          | None -> try_rgen (i + 1)
      in
      try_rgen 0
    | _ -> None
  in
  match ev_param with
  | Some ev_name ->
    (* Use dynamic field index from evidence parameter *)
    let tagged_idx = emit_var ctx ev_name Types.TInt in
    let idx = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:tagged_idx ~rhs:"1" in
    let elem_ptr = Ir_emit.emit_gep_dynamic ctx.ir ~ty:"i64" ~ptr ~index:idx in
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:elem_ptr
  | None ->
    let idx = match Types.repr record_expr.ty with
      | Types.TRecord row ->
        let fields = Types.record_row_to_fields row in
        let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
        let rec find i = function
          | [] -> failwith (Printf.sprintf "native codegen: field %s not found in record type" field_name)
          | (name, _) :: _ when name = field_name -> i
          | _ :: rest -> find (i + 1) rest
        in
        find 0 sorted
      | _ ->
        (* Typeclass dictionary field access: dict TEVars are typed as TUnit after
           transform_constraints, so we look up the class definition to find the
           method's position in the alphabetically-sorted method list. *)
        dict_method_index ctx field_name
    in
    let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:idx in
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:elem_ptr

and emit_record_update ctx base overrides =
  let base_val = emit_expr ctx base in
  let base_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:base_val in
  let fields = record_fields_from_type base.ty in
  let n = List.length fields in
  (* Size stored in header word at ptr[-1], no separate size slot needed *)
  let new_ptr = emit_alloc ctx (n * 8) (make_header ~size:n mml_hdr_record) in
  (* Copy all fields from base *)
  for i = 0 to n - 1 do
    let src = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:base_ptr ~index:i in
    let v = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:src in
    let dst = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:new_ptr ~index:i in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:dst
  done;
  (* Override specified fields *)
  List.iter (fun (name, expr) ->
    let v = emit_expr ctx expr in
    let idx = field_index_from_type base.ty name in
    let dst = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:new_ptr ~index:idx in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:dst
  ) overrides;
  Ir_emit.emit_ptrtoint ctx.ir ~value:new_ptr

and emit_record_update_idx ctx base pairs =
  let base_val = emit_expr ctx base in
  let base_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:base_val in
  (* Read header from base_ptr[-1] and extract size (header >> 16) *)
  let hdr_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:base_ptr ~index:(-1) in
  let hdr = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:hdr_ptr in
  let n = Ir_emit.emit_binop ctx.ir ~op:"lshr" ~ty:"i64" ~lhs:hdr ~rhs:"16" in
  (* Allocate n*8 bytes with same header *)
  let alloc_bytes = Ir_emit.emit_binop ctx.ir ~op:"mul" ~ty:"i64" ~lhs:n ~rhs:"8" in
  add_extern ctx "mml_alloc" "ptr" ["i64"; "i64"];
  let new_ptr = Ir_emit.emit_call ctx.ir ~ret_ty:"ptr" ~name:"mml_alloc"
    ~args:[("i64", alloc_bytes); ("i64", hdr)] in
  (* memcpy all fields from base to new *)
  let nbytes = alloc_bytes in
  add_extern ctx "memcpy" "ptr" ["ptr"; "ptr"; "i64"];
  ignore (Ir_emit.emit_call ctx.ir ~ret_ty:"ptr" ~name:"memcpy"
    ~args:[("ptr", new_ptr); ("ptr", base_ptr); ("i64", nbytes)]);
  (* Overwrite fields at dynamic indices *)
  List.iter (fun (idx_expr, val_expr) ->
    let idx_val = emit_expr ctx idx_expr in
    let val_v = emit_expr ctx val_expr in
    (* Evidence params are tagged ints — untag *)
    let idx_untagged = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:idx_val ~rhs:"1" in
    let dst = Ir_emit.emit_gep_dynamic ctx.ir ~ty:"i64" ~ptr:new_ptr ~index:idx_untagged in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:val_v ~ptr:dst
  ) pairs;
  Ir_emit.emit_ptrtoint ctx.ir ~value:new_ptr

and emit_field_assign ctx record_expr field_name value_expr =
  let rec_val = emit_expr ctx record_expr in
  let v = emit_expr ctx value_expr in
  let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:rec_val in
  let idx = field_index_from_type record_expr.ty field_name in
  let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:idx in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:elem_ptr;
  unit_value

(* ---- Variant constructors ---- *)

and emit_construct ctx name arg =
  if is_newtype_ctor ctx name then begin
    (* Newtype constructor: erased at runtime *)
    match arg with
    | Some e -> emit_expr ctx e
    | None -> tag_int 0  (* unit *)
  end else begin
    let tag = if String.length name > 0 && name.[0] = '`' then
      Hashtbl.hash (String.sub name 1 (String.length name - 1)) land 0x3FFFFFFF
    else
      tag_for_constructor ctx name
    in
    let tag_val = tag_int tag in
    (match arg with
     | None ->
       (* No-arg constructors are bare tagged ints *)
       tag_val
     | Some e ->
       let payload = emit_expr ctx e in
       let hdr_tag = if String.length name > 0 && name.[0] = '`'
         then mml_hdr_polyvar else mml_hdr_variant in
       let ptr = emit_alloc ctx 16 (make_header hdr_tag) in
       let tag_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
       Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:tag_val ~ptr:tag_ptr;
       let payload_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
       Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:payload ~ptr:payload_ptr;
       Ir_emit.emit_ptrtoint ctx.ir ~value:ptr)
  end

(* ---- Pattern matching ---- *)

and emit_match ctx scrutinee arms =
  let scrut_val = emit_expr ctx scrutinee in
  (* Store scrutinee in an alloca for pattern binding *)
  let scrut_ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:scrut_val ~ptr:scrut_ptr;
  let match_done = fresh_label ctx "match_done" in
  let match_fail = fresh_label ctx "match_fail" in
  let n = List.length arms in
  (* Generate labels for each arm *)
  let arm_labels = List.init n (fun i -> fresh_label ctx (Printf.sprintf "arm_%d" i)) in
  let arm_fail_labels = List.init n (fun i ->
    if i < n - 1 then List.nth arm_labels (i + 1)
    else match_fail
  ) in
  (* Result alloca *)
  let result_ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:result_ptr;
  (* Branch to first arm *)
  Ir_emit.emit_br ctx.ir ~label:(List.hd arm_labels);
  (* Compile each arm *)
  let body_ends = ref [] in
  List.iteri (fun i (pattern, guard, body) ->
    let arm_label = List.nth arm_labels i in
    let fail_label = List.nth arm_fail_labels i in
    Ir_emit.emit_label ctx.ir arm_label;
    ctx.current_label <- arm_label;
    push_scope ctx;
    let scrut = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:scrut_ptr in
    emit_pattern ctx scrut pattern fail_label scrutinee.ty;
    (* Guard check *)
    (match guard with
     | Some guard_expr ->
       let guard_val = emit_expr ctx guard_expr in
       let is_false = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:guard_val ~rhs:false_value in
       let guard_ok = fresh_label ctx "guard_ok" in
       Ir_emit.emit_condbr ctx.ir ~cond:is_false ~if_true:fail_label ~if_false:guard_ok;
       Ir_emit.emit_label ctx.ir guard_ok;
       ctx.current_label <- guard_ok
     | None -> ());
    (* Compile body *)
    let body_val = emit_expr ctx body in
    let body_end = ctx.current_label in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:body_val ~ptr:result_ptr;
    Ir_emit.emit_br ctx.ir ~label:match_done;
    pop_scope ctx;
    body_ends := (body_val, body_end) :: !body_ends
  ) arms;
  (* Match failure *)
  Ir_emit.emit_label ctx.ir match_fail;
  ctx.current_label <- match_fail;
  emit_match_panic ctx scrutinee.loc;
  (* Match done *)
  Ir_emit.emit_label ctx.ir match_done;
  ctx.current_label <- match_done;
  Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:result_ptr

and emit_match_panic ctx loc =
  let msg = Printf.sprintf "non-exhaustive match at line %d" loc.Token.line in
  let name = Printf.sprintf ".str.%d" ctx.str_counter in
  ctx.str_counter <- ctx.str_counter + 1;
  let escaped = Buffer.contents (llvm_escape_string msg) in
  let len = String.length msg + 1 in
  let decl = Printf.sprintf "@%s = private unnamed_addr constant [%d x i8] c\"%s\\00\""
    name len escaped in
  ctx.string_globals <- decl :: ctx.string_globals;
  let ptr = Ir_emit.emit_ptrtoint ctx.ir ~value:(Printf.sprintf "@%s" name) in
  add_extern ctx "mml_panic" "void" ["i64"];
  Ir_emit.emit_call_void ctx.ir ~name:"mml_panic" ~args:[("i64", ptr)];
  Ir_emit.emit_unreachable ctx.ir

(* ---- Pattern compilation ---- *)

and collect_pattern_names pat =
  match pat with
  | Ast.PatVar name -> [name]
  | Ast.PatAs (p, name) -> name :: collect_pattern_names p
  | Ast.PatTuple ps -> List.concat_map collect_pattern_names ps
  | Ast.PatCons (p1, p2) -> collect_pattern_names p1 @ collect_pattern_names p2
  | Ast.PatConstruct (_, Some p) -> collect_pattern_names p
  | Ast.PatPolyVariant (_, Some p) -> collect_pattern_names p
  | Ast.PatRecord fps -> List.concat_map (fun (_, p) -> collect_pattern_names p) fps
  | Ast.PatOr (p1, _) -> collect_pattern_names p1
  | Ast.PatAnnot (p, _) -> collect_pattern_names p
  | Ast.PatArray ps -> List.concat_map collect_pattern_names ps
  | Ast.PatMap entries -> List.concat_map (fun (_, vp) -> collect_pattern_names vp) entries
  | _ -> []

and emit_pattern ctx val_reg pattern fail_label scrutinee_ty =
  match pattern with
  | Ast.PatWild -> ()
  | Ast.PatUnit -> ()
  | Ast.PatVar name ->
    (match List.assoc_opt name ctx.or_pattern_allocas with
     | Some ptr ->
       (* Inside or-pattern: store to pre-allocated shared alloca *)
       Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:val_reg ~ptr
     | None ->
       let ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
       Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:val_reg ~ptr;
       bind_var ctx name (Local ptr))
  | Ast.PatInt n ->
    let expected = tag_int n in
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:val_reg ~rhs:expected in
    let cont = fresh_label ctx "pat_ok" in
    Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:fail_label ~if_false:cont;
    Ir_emit.emit_label ctx.ir cont;
    ctx.current_label <- cont
  | Ast.PatBool b ->
    let expected = if b then true_value else false_value in
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:val_reg ~rhs:expected in
    let cont = fresh_label ctx "pat_ok" in
    Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:fail_label ~if_false:cont;
    Ir_emit.emit_label ctx.ir cont;
    ctx.current_label <- cont
  | Ast.PatString s ->
    let sval = emit_expr ctx { expr = TEString s; ty = Types.TString; loc = Token.dummy_loc } in
    add_extern ctx "mml_string_eq" "i64" ["i64"; "i64"];
    let eq = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_eq"
      ~args:[("i64", val_reg); ("i64", sval)] in
    let is_false = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:eq ~rhs:false_value in
    let cont = fresh_label ctx "pat_ok" in
    Ir_emit.emit_condbr ctx.ir ~cond:is_false ~if_true:fail_label ~if_false:cont;
    Ir_emit.emit_label ctx.ir cont;
    ctx.current_label <- cont
  | Ast.PatTuple pats ->
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
    List.iteri (fun i sub_pat ->
      let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
      let elem = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:elem_ptr in
      let elem_ty = match Types.repr scrutinee_ty with
        | Types.TTuple tys when i < List.length tys -> List.nth tys i
        | _ -> Types.TUnit
      in
      emit_pattern ctx elem sub_pat fail_label elem_ty
    ) pats
  | Ast.PatConstruct (name, sub_pat) when is_newtype_ctor ctx name ->
    (* Newtype constructor: erased at runtime — match sub-pattern directly *)
    (match sub_pat with
     | Some p ->
       let payload_ty = ctor_payload_ty ctx name in
       emit_pattern ctx val_reg p fail_label payload_ty
     | None -> ())
  | Ast.PatConstruct (name, sub_pat) ->
    let tag = if String.length name > 0 && name.[0] = '`' then
      Hashtbl.hash (String.sub name 1 (String.length name - 1)) land 0x3FFFFFFF
    else
      tag_for_constructor ctx name
    in
    let expected_tag = tag_int tag in
    (match sub_pat with
     | None ->
       (* No-arg constructor: value is a bare tagged int *)
       let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:val_reg ~rhs:expected_tag in
       let cont = fresh_label ctx "pat_ctor_ok" in
       Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:fail_label ~if_false:cont;
       Ir_emit.emit_label ctx.ir cont;
       ctx.current_label <- cont
     | Some p ->
       (* With-arg constructor: value is a heap pointer [tag, payload] *)
       (* First check it's a pointer (low bit clear) *)
       let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64" ~lhs:val_reg ~rhs:"1" in
       let is_tagged = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:low_bit ~rhs:"0" in
       let check_tag = fresh_label ctx "pat_ctor_check" in
       Ir_emit.emit_condbr ctx.ir ~cond:is_tagged ~if_true:fail_label ~if_false:check_tag;
       Ir_emit.emit_label ctx.ir check_tag;
       ctx.current_label <- check_tag;
       let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
       let tag_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
       let actual_tag = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:tag_ptr in
       let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:actual_tag ~rhs:expected_tag in
       let cont = fresh_label ctx "pat_ctor_ok" in
       Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:fail_label ~if_false:cont;
       Ir_emit.emit_label ctx.ir cont;
       ctx.current_label <- cont;
       let payload_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
       let payload = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:payload_ptr in
       let payload_ty = ctor_payload_ty ctx name in
       emit_pattern ctx payload p fail_label payload_ty)
  | Ast.PatRecord field_pats ->
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
    List.iter (fun (fname, sub_pat) ->
      let idx = field_index_from_type scrutinee_ty fname in
      let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:idx in
      let elem = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:elem_ptr in
      let field_ty = field_ty_from_type scrutinee_ty fname in
      emit_pattern ctx elem sub_pat fail_label field_ty
    ) field_pats
  | Ast.PatAs (pat, name) ->
    let ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:val_reg ~ptr;
    bind_var ctx name (Local ptr);
    emit_pattern ctx val_reg pat fail_label scrutinee_ty
  | Ast.PatOr (p1, p2) ->
    let try_p2 = fresh_label ctx "pat_or_p2" in
    (* Pre-allocate shared allocas for any variables bound by the or-pattern *)
    let bound_names = collect_pattern_names p1 in
    let shared = List.map (fun name ->
      let ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
      (name, ptr)
    ) bound_names in
    let saved_or_allocas = ctx.or_pattern_allocas in
    ctx.or_pattern_allocas <- shared @ ctx.or_pattern_allocas;
    (* Try p1 *)
    emit_pattern ctx val_reg p1 try_p2 scrutinee_ty;
    let p1_ok = fresh_label ctx "pat_or_ok" in
    Ir_emit.emit_br ctx.ir ~label:p1_ok;
    (* Try p2 *)
    Ir_emit.emit_label ctx.ir try_p2;
    ctx.current_label <- try_p2;
    emit_pattern ctx val_reg p2 fail_label scrutinee_ty;
    ctx.or_pattern_allocas <- saved_or_allocas;
    Ir_emit.emit_br ctx.ir ~label:p1_ok;
    Ir_emit.emit_label ctx.ir p1_ok;
    ctx.current_label <- p1_ok;
    (* Bind shared allocas so the match arm body can use them *)
    List.iter (fun (name, ptr) ->
      bind_var ctx name (Local ptr)
    ) shared
  | Ast.PatAnnot (pat, _) ->
    emit_pattern ctx val_reg pat fail_label scrutinee_ty
  | Ast.PatFloat f ->
    let fval = emit_expr ctx { expr = TEFloat f; ty = Types.TFloat; loc = Token.dummy_loc } in
    let da = emit_unbox_float ctx val_reg in
    let db = emit_unbox_float ctx fval in
    let cmp = Ir_emit.emit_fcmp ctx.ir ~cmp:"one" ~lhs:da ~rhs:db in
    let cont = fresh_label ctx "pat_ok" in
    Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:fail_label ~if_false:cont;
    Ir_emit.emit_label ctx.ir cont;
    ctx.current_label <- cont
  | Ast.PatNil ->
    (* Nil = tagged int (low bit 1). Fail if value is a pointer (cons). *)
    let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64"
      ~lhs:val_reg ~rhs:"1" in
    let is_pointer = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64"
      ~lhs:low_bit ~rhs:"0" in
    let cont = fresh_label ctx "pat_nil_ok" in
    Ir_emit.emit_condbr ctx.ir ~cond:is_pointer ~if_true:fail_label ~if_false:cont;
    Ir_emit.emit_label ctx.ir cont;
    ctx.current_label <- cont
  | Ast.PatCons (hd_pat, tl_pat) ->
    (* Cons = pointer (low bit 0). Fail if nil. *)
    let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64"
      ~lhs:val_reg ~rhs:"1" in
    let is_nil = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64"
      ~lhs:low_bit ~rhs:"0" in
    let cont = fresh_label ctx "pat_cons_ok" in
    Ir_emit.emit_condbr ctx.ir ~cond:is_nil ~if_true:fail_label ~if_false:cont;
    Ir_emit.emit_label ctx.ir cont;
    ctx.current_label <- cont;
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
    let hd_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
    let hd_val = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:hd_ptr in
    let tl_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
    let tl_val = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:tl_ptr in
    let elem_ty = match Types.repr scrutinee_ty with
      | Types.TList t -> t
      | _ -> Types.TUnit
    in
    emit_pattern ctx hd_val hd_pat fail_label elem_ty;
    emit_pattern ctx tl_val tl_pat fail_label scrutinee_ty
  | Ast.PatArray pats ->
    let n = List.length pats in
    if n = 0 then begin
      (* Empty array = MML_UNIT (tagged int). Fail if pointer. *)
      let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64"
        ~lhs:val_reg ~rhs:"1" in
      let is_pointer = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64"
        ~lhs:low_bit ~rhs:"0" in
      let cont = fresh_label ctx "pat_arr_empty_ok" in
      Ir_emit.emit_condbr ctx.ir ~cond:is_pointer ~if_true:fail_label ~if_false:cont;
      Ir_emit.emit_label ctx.ir cont;
      ctx.current_label <- cont
    end else begin
      (* Non-empty array: must be a pointer, check length matches *)
      let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64"
        ~lhs:val_reg ~rhs:"1" in
      let is_int = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64"
        ~lhs:low_bit ~rhs:"0" in
      let cont1 = fresh_label ctx "pat_arr_ptr" in
      Ir_emit.emit_condbr ctx.ir ~cond:is_int ~if_true:fail_label ~if_false:cont1;
      Ir_emit.emit_label ctx.ir cont1;
      ctx.current_label <- cont1;
      let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
      let len_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
      let arr_len = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:len_ptr in
      let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64"
        ~lhs:arr_len ~rhs:(string_of_int n) in
      let cont2 = fresh_label ctx "pat_arr_len_ok" in
      Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:fail_label ~if_false:cont2;
      Ir_emit.emit_label ctx.ir cont2;
      ctx.current_label <- cont2;
      let elem_ty = match Types.repr scrutinee_ty with
        | Types.TArray t -> t
        | _ -> Types.TUnit
      in
      List.iteri (fun i sub_pat ->
        let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:(i + 1) in
        let elem = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:elem_ptr in
        emit_pattern ctx elem sub_pat fail_label elem_ty
      ) pats
    end
  | Ast.PatMap entries ->
    (* Maps are association lists. For each (key_pat, val_pat):
       call mml_assoc_has to check key exists, mml_assoc_get to extract value *)
    List.iter (fun (key_pat, val_pat) ->
      (* Emit key as a constant value *)
      let key_val = match key_pat with
        | Ast.PatInt n -> tag_int n
        | Ast.PatString s ->
          emit_expr ctx { expr = TEString s; ty = Types.TString; loc = Token.dummy_loc }
        | Ast.PatBool b -> if b then true_value else false_value
        | Ast.PatPin name ->
          (match lookup_var ctx name with
           | Some (Local ptr) | Some (MutLocal ptr) -> Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr
           | Some (Global gname) | Some (MutGlobal gname) -> Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:(Printf.sprintf "@%s" gname)
           | Some (MutRefCell ptr) ->
             let cell = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr in
             let cell_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:cell in
             Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:cell_ptr
           | Some (Func _) | Some (FuncLocal _) -> failwith "native codegen: cannot pin a function in map key"
           | None -> failwith (Printf.sprintf "native codegen: unbound pin variable in map key: %s" name))
        | _ -> failwith "native codegen: map pattern keys must be literals or pin patterns"
      in
      (* Check key exists in association list *)
      add_extern ctx "mml_assoc_has" "i64" ["i64"; "i64"];
      let has = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_assoc_has"
        ~args:[("i64", key_val); ("i64", val_reg)] in
      let is_false = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:has ~rhs:false_value in
      let cont = fresh_label ctx "pat_map_has" in
      Ir_emit.emit_condbr ctx.ir ~cond:is_false ~if_true:fail_label ~if_false:cont;
      Ir_emit.emit_label ctx.ir cont;
      ctx.current_label <- cont;
      (* Extract value from association list *)
      add_extern ctx "mml_assoc_get" "i64" ["i64"; "i64"];
      let extracted = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_assoc_get"
        ~args:[("i64", key_val); ("i64", val_reg)] in
      let val_ty = match Types.repr scrutinee_ty with
        | Types.TList (Types.TTuple [_; vt]) -> vt
        | _ -> Types.TUnit
      in
      emit_pattern ctx extracted val_pat fail_label val_ty
    ) entries
  | Ast.PatPolyVariant (name, sub_pat) ->
    let tag = Hashtbl.hash name land 0x3FFFFFFF in
    let expected_tag = tag_int tag in
    (match sub_pat with
     | None ->
       (* No-arg poly variant: bare tagged int *)
       let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:val_reg ~rhs:expected_tag in
       let cont = fresh_label ctx "pat_pv_ok" in
       Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:fail_label ~if_false:cont;
       Ir_emit.emit_label ctx.ir cont;
       ctx.current_label <- cont
     | Some p ->
       (* Poly variant with payload: heap pointer [tag, payload] *)
       let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64" ~lhs:val_reg ~rhs:"1" in
       let is_tagged = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:low_bit ~rhs:"0" in
       let check_tag = fresh_label ctx "pat_pv_check" in
       Ir_emit.emit_condbr ctx.ir ~cond:is_tagged ~if_true:fail_label ~if_false:check_tag;
       Ir_emit.emit_label ctx.ir check_tag;
       ctx.current_label <- check_tag;
       let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
       let tag_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
       let actual_tag = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:tag_ptr in
       let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:actual_tag ~rhs:expected_tag in
       let cont = fresh_label ctx "pat_pv_ok" in
       Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:fail_label ~if_false:cont;
       Ir_emit.emit_label ctx.ir cont;
       ctx.current_label <- cont;
       let payload_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
       let payload = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:payload_ptr in
       emit_pattern ctx payload p fail_label Types.TUnit)
  | Ast.PatPin name ->
    (* Pin pattern: compare scrutinee with the value of an existing variable *)
    let var_val = (match lookup_var ctx name with
     | Some (Local ptr) | Some (MutLocal ptr) -> Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr
     | Some (Global gname) | Some (MutGlobal gname) -> Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:(Printf.sprintf "@%s" gname)
     | Some (MutRefCell ptr) ->
       let cell = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr in
       let cell_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:cell in
       Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:cell_ptr
     | Some (Func _) | Some (FuncLocal _) -> failwith "native codegen: cannot pin a function"
     | None -> failwith (Printf.sprintf "native codegen: unbound pin variable: %s" name)
    ) in
    (* Use structural equality *)
    add_extern ctx "mml_structural_eq" "i64" ["i64"; "i64"];
    let eq = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_structural_eq"
      ~args:[("i64", val_reg); ("i64", var_val)] in
    let is_false = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:eq ~rhs:false_value in
    let cont = fresh_label ctx "pat_pin_ok" in
    Ir_emit.emit_condbr ctx.ir ~cond:is_false ~if_true:fail_label ~if_false:cont;
    Ir_emit.emit_label ctx.ir cont;
    ctx.current_label <- cont

and ctor_payload_ty ctx name =
  let short = match String.rindex_opt name '.' with
    | Some i -> String.sub name (i + 1) (String.length name - i - 1)
    | None -> name
  in
  match List.assoc_opt short ctx.constructors with
  | Some (type_name, _tag) ->
    (match List.assoc_opt type_name ctx.variant_defs with
     | Some vdef ->
       (match List.assoc_opt short vdef with
        | Some (Some ty) -> ty
        | _ -> Types.TUnit)
     | None -> Types.TUnit)
  | None -> Types.TUnit

and field_ty_from_type ty fname =
  match Types.repr ty with
  | Types.TRecord row ->
    let fields = Types.record_row_to_fields row in
    (match List.assoc_opt fname fields with
     | Some t -> t
     | None -> Types.TUnit)
  | _ -> Types.TUnit

(* ---- Float boxing/unboxing ---- *)

and emit_unbox_float ctx v =
  add_extern ctx "mml_unbox_float" "double" ["i64"];
  Ir_emit.emit_call ctx.ir ~ret_ty:"double" ~name:"mml_unbox_float"
    ~args:[("i64", v)]

and emit_box_float ctx d =
  add_extern ctx "mml_box_float" "i64" ["double"];
  Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_box_float"
    ~args:[("double", d)]

(* ---- For-loop fold dispatch ---- *)

and emit_fold_with_break_check ctx fold_expr =
  (* For folds through MiniML code that doesn't check mml_fold_broken:
     after the fold completes, check if a break occurred and use the break value *)
  let result = emit_expr ctx fold_expr in
  add_extern ctx "mml_check_fold_broken" "i64" [];
  add_extern ctx "mml_consume_fold_break" "i64" [];
  let was_broken = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_check_fold_broken" ~args:[] in
  let is_broken = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:was_broken ~rhs:"0" in
  let broken_label = fresh_label ctx "fold_was_broken" in
  let ok_label = fresh_label ctx "fold_ok" in
  let merge_label = fresh_label ctx "fold_merge" in
  Ir_emit.emit_condbr ctx.ir ~cond:is_broken ~if_true:broken_label ~if_false:ok_label;
  Ir_emit.emit_label ctx.ir broken_label;
  ctx.current_label <- broken_label;
  let break_val = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_consume_fold_break" ~args:[] in
  Ir_emit.emit_br ctx.ir ~label:merge_label;
  Ir_emit.emit_label ctx.ir ok_label;
  ctx.current_label <- ok_label;
  Ir_emit.emit_br ctx.ir ~label:merge_label;
  Ir_emit.emit_label ctx.ir merge_label;
  ctx.current_label <- merge_label;
  Ir_emit.emit_phi ctx.ir ~ty:"i64" ~incoming:[(break_val, broken_label); (result, ok_label)]

(** Dispatch for-in loop fold expressions.
    Intercepts fold calls on known collection types (list, array) and routes
    to the C runtime breakable fold, bypassing the MiniML typeclass fold
    which doesn't check mml_fold_broken. *)
and emit_for_loop_expr ctx fold_expr =
  (* Try to extract the fold call pattern: fold callback init collection *)
  let rec extract_fold_app e =
    match e.Typechecker.expr with
    | TEApp (f, arg) ->
      let (base, args) = extract_fold_app f in
      (base, args @ [arg])
    | _ -> (e, [])
  in
  let (base, args) = extract_fold_app fold_expr in
  (* Check if base is a field access on a dict (typeclass fold dispatch) *)
  let is_fold_field = match base.Typechecker.expr with
    | TEField (_, "fold") -> true
    | _ -> false
  in
  if is_fold_field && List.length args = 3 then begin
    (* args = [callback, init, collection] *)
    let callback = List.nth args 0 in
    let init = List.nth args 1 in
    let coll = List.nth args 2 in
    let coll_ty = Types.repr coll.Typechecker.ty in
    (* Check if callback element type matches the list element type.
       For sets (('a * unit) list), the callback takes 'a not ('a * unit),
       so we must use the dict's fold instead of mml_list_fold_breakable. *)
    let cb_elem_matches_list = match coll_ty with
      | Types.TList elem_ty ->
        let cb_ty = Types.repr callback.Typechecker.ty in
        (match cb_ty with
         | Types.TArrow (_, _, ret) ->
           (match Types.repr ret with
            | Types.TArrow (cb_elem, _, _) ->
              Types.repr elem_ty = Types.repr cb_elem
            | _ -> true)
         | _ -> true)
      | _ -> true
    in
    let rt_fn = match coll_ty with
      | Types.TList _ when cb_elem_matches_list -> Some "mml_list_fold_breakable"
      | Types.TArray _ -> Some "mml_array_fold_breakable"
      | _ -> None
    in
    match rt_fn with
    | Some fn_name ->
      let cb_val = emit_expr ctx callback in
      let init_val = emit_expr ctx init in
      let coll_val = emit_expr ctx coll in
      add_extern ctx fn_name "i64" ["i64"; "i64"; "i64"];
      Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:fn_name
        ~args:[("i64", cb_val); ("i64", init_val); ("i64", coll_val)]
    | None ->
      (* Unknown collection type — fall through to normal eval with break check *)
      emit_fold_with_break_check ctx fold_expr
  end else
    emit_fold_with_break_check ctx fold_expr

(* ---- Function application ---- *)

and emit_app ctx fn_expr arg_expr =
  (* Flatten curried application: f a b → (f, [a; b]) *)
  let (f, args) = flatten_app fn_expr arg_expr in
  match f.Typechecker.expr with
  | TEVar name when is_print_name name ->
    emit_print_call ctx arg_expr
  | TEVar name ->
    emit_named_call ctx name f args
  | _ ->
    (* General function application: evaluate f, call as closure *)
    let closure_val = emit_expr ctx f in
    let arg_vals = List.map (emit_expr ctx) args in
    emit_closure_apply ctx closure_val arg_vals

and is_print_name = function
  | "print" | "Stdlib.print" -> true
  | _ -> false

and flatten_app fn_expr arg_expr =
  let rec collect fn args =
    match fn.Typechecker.expr with
    | TEApp (f, a) -> collect f (a :: args)
    | _ -> (fn, args)
  in
  collect fn_expr [arg_expr]

and emit_print_call ctx arg_expr =
  let arg_val = emit_expr ctx arg_expr in
  let ty = Types.repr arg_expr.ty in
  ctx.has_print_output <- true;
  let use_show = match ty with
    | Types.TInt | Types.TBool | Types.TString | Types.TFloat | Types.TUnit -> false
    | Types.TTuple _ | Types.TRecord _ | Types.TVariant _ | Types.TList _
    | Types.TPolyVariant _ | Types.TArray _
    | Types.TByte | Types.TRune -> true
    | _ -> false
  in
  if use_show then begin
    (* Generate show closure, call it, then print the resulting string *)
    let show_closure = emit_show_closure ctx
      (Types.TArrow (ty, Types.EffEmpty, Types.TString)) in
    add_extern ctx "mml_apply1" "i64" ["i64"; "i64"];
    let shown = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_apply1"
      ~args:[("i64", show_closure); ("i64", arg_val)] in
    add_extern ctx "mml_print_string" "i64" ["i64"];
    ignore (Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_print_string"
      ~args:[("i64", shown)])
  end else begin
    let fn_name = match ty with
      | Types.TInt -> "mml_print_int"
      | Types.TBool -> "mml_print_bool"
      | Types.TString -> "mml_print_string"
      | Types.TFloat -> "mml_print_float"
      | Types.TUnit -> "mml_print_unit"
      | _ -> "mml_print_value"
    in
    add_extern ctx fn_name "i64" ["i64"];
    ignore (Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:fn_name
      ~args:[("i64", arg_val)])
  end;
  unit_value

and emit_named_call ctx name f_expr args =
  (* Check for builtin functions first *)
  match name with
  | "string_of_int" | "Stdlib.string_of_int" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_of_int" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_of_int"
      ~args:[("i64", arg_val)]
  | "float_of_int" | "Stdlib.float_of_int" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_float_of_int" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_float_of_int"
      ~args:[("i64", arg_val)]
  | "int_of_float" | "Stdlib.int_of_float" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_int_of_float" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_int_of_float"
      ~args:[("i64", arg_val)]
  | "String.length" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_length" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_length"
      ~args:[("i64", arg_val)]
  | "failwith" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_panic_mml" "void" ["i64"];
    Ir_emit.emit_call_void ctx.ir ~name:"mml_panic_mml" ~args:[("i64", arg_val)];
    Ir_emit.emit_unreachable ctx.ir;
    let dead = fresh_label ctx "dead" in
    Ir_emit.emit_label ctx.ir dead;
    ctx.current_label <- dead;
    unit_value
  | "phys_equal" | "Stdlib.phys_equal" ->
    let a = emit_expr ctx (List.nth args 0) in
    let b = emit_expr ctx (List.nth args 1) in
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:a ~rhs:b in
    Ir_emit.emit_select ctx.ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
  | "String.of_byte" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_of_byte" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_of_byte"
      ~args:[("i64", arg_val)]
  (* Byte module builtins *)
  | "Byte.of_int" ->
    emit_expr ctx (List.hd args)  (* identity: bytes = tagged ints *)
  | "Byte.to_int" ->
    emit_expr ctx (List.hd args)  (* identity *)
  | "Byte.to_string" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_of_byte" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_of_byte"
      ~args:[("i64", arg_val)]
  (* Ref module builtins *)
  | "Ref.create" ->
    let init_val = emit_expr ctx (List.hd args) in
    let ptr = emit_alloc ctx 8 (make_header mml_hdr_ref) in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:init_val ~ptr;
    Ir_emit.emit_ptrtoint ctx.ir ~value:ptr
  | "Ref.get" ->
    let ref_val = emit_expr ctx (List.hd args) in
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:ref_val in
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr
  | "Ref.set" ->
    let ref_val = emit_expr ctx (List.nth args 0) in
    let new_val = emit_expr ctx (List.nth args 1) in
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:ref_val in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:new_val ~ptr;
    unit_value
  (* List module builtins *)
  | "List.hd" ->
    let list_val = emit_expr ctx (List.hd args) in
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:list_val in
    let hd_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:hd_ptr
  | "List.tl" ->
    let list_val = emit_expr ctx (List.hd args) in
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:list_val in
    let tl_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:tl_ptr
  | "List.rev" ->
    let list_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_list_rev" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_rev"
      ~args:[("i64", list_val)]
  | "List.length" ->
    let list_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_list_length" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_length"
      ~args:[("i64", list_val)]
  | "List.map" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_map" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_map"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.fold" | "List.fold_left" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let acc_val = emit_expr ctx (List.nth args 1) in
    let list_val = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_list_fold" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_fold"
      ~args:[("i64", fn_val); ("i64", acc_val); ("i64", list_val)]
  | "List.filter" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_filter" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_filter"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.find" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_find" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_find"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.find_map" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_find_map" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_find_map"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.exists" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_exists" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_exists"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.forall" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_forall" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_forall"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.iter" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_iter" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_iter"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.mapi" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_mapi" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_mapi"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.concat" ->
    let a = emit_expr ctx (List.nth args 0) in
    let b = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_concat" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_concat"
      ~args:[("i64", a); ("i64", b)]
  | "List.flatten" ->
    let list_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_list_flatten" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_flatten"
      ~args:[("i64", list_val)]
  | "List.sort" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_sort" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_sort"
      ~args:[("i64", fn_val); ("i64", list_val)]
  | "List.assoc_opt" ->
    let key_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_assoc_opt" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_assoc_opt"
      ~args:[("i64", key_val); ("i64", list_val)]
  | "List.fold_right" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let list_val = emit_expr ctx (List.nth args 1) in
    let acc_val = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_list_fold_right" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_fold_right"
      ~args:[("i64", fn_val); ("i64", list_val); ("i64", acc_val)]
  | "List.init" ->
    let n_val = emit_expr ctx (List.nth args 0) in
    let fn_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_list_init" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_init"
      ~args:[("i64", n_val); ("i64", fn_val)]
  | "List.map2" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let l1_val = emit_expr ctx (List.nth args 1) in
    let l2_val = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_list_map2" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_map2"
      ~args:[("i64", fn_val); ("i64", l1_val); ("i64", l2_val)]
  | "List.iter2" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let l1_val = emit_expr ctx (List.nth args 1) in
    let l2_val = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_list_iter2" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_list_iter2"
      ~args:[("i64", fn_val); ("i64", l1_val); ("i64", l2_val)]
  | "Array.copy" ->
    let arr_val = emit_expr ctx (List.nth args 0) in
    add_extern ctx "mml_array_copy" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_copy"
      ~args:[("i64", arr_val)]
  | "Array.init" ->
    let n_val = emit_expr ctx (List.nth args 0) in
    let fn_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_array_init" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_init"
      ~args:[("i64", n_val); ("i64", fn_val)]
  | "Array.map" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let arr_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_array_map" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_map"
      ~args:[("i64", fn_val); ("i64", arr_val)]
  | "Array.mapi" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let arr_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_array_mapi" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_mapi"
      ~args:[("i64", fn_val); ("i64", arr_val)]
  | "Array.iter" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let arr_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_array_iter" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_iter"
      ~args:[("i64", fn_val); ("i64", arr_val)]
  | "Array.fold" ->
    let fn_val = emit_expr ctx (List.nth args 0) in
    let acc_val = emit_expr ctx (List.nth args 1) in
    let arr_val = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_array_fold" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_fold"
      ~args:[("i64", fn_val); ("i64", acc_val); ("i64", arr_val)]
  | "array_get" | "Array.get" ->
    let arr_val = emit_expr ctx (List.nth args 0) in
    let idx_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_array_get" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_get"
      ~args:[("i64", idx_val); ("i64", arr_val)]
  | "array_set" | "Array.set" ->
    let arr_val = emit_expr ctx (List.nth args 0) in
    let idx_val = emit_expr ctx (List.nth args 1) in
    let new_val = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_array_set" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_set"
      ~args:[("i64", arr_val); ("i64", idx_val); ("i64", new_val)]
  | "array_length" | "Array.length" ->
    let arr_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_array_length" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_length"
      ~args:[("i64", arr_val)]
  | "Array.make" ->
    let n_val = emit_expr ctx (List.nth args 0) in
    let init_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_array_make" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_make"
      ~args:[("i64", n_val); ("i64", init_val)]
  | "Array.of_list" ->
    let lst_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_array_of_list" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_of_list"
      ~args:[("i64", lst_val)]
  | "Array.to_list" ->
    let arr_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_array_to_list" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_to_list"
      ~args:[("i64", arr_val)]
  (* String extra builtins *)
  | "String.get" ->
    let str_val = emit_expr ctx (List.nth args 0) in
    let idx_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_get_byte" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_get_byte"
      ~args:[("i64", idx_val); ("i64", str_val)]
  | "String.sub" ->
    let str_val = emit_expr ctx (List.nth args 0) in
    let start_val = emit_expr ctx (List.nth args 1) in
    let len_val = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_string_sub" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_sub"
      ~args:[("i64", str_val); ("i64", start_val); ("i64", len_val)]
  | "String.contains" ->
    let sub_val = emit_expr ctx (List.nth args 0) in
    let str_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_contains" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_contains"
      ~args:[("i64", str_val); ("i64", sub_val)]
  | "String.to_list" ->
    let str_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_to_list" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_to_list"
      ~args:[("i64", str_val)]
  | "String.rune_length" ->
    let str_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_rune_length" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_rune_length"
      ~args:[("i64", str_val)]
  | "String.of_bytes" ->
    let lst_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_of_bytes" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_of_bytes"
      ~args:[("i64", lst_val)]
  | "String.of_runes" ->
    let lst_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_of_runes" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_of_runes"
      ~args:[("i64", lst_val)]
  | "String.get_rune" ->
    let str_val = emit_expr ctx (List.nth args 0) in
    let idx_val = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_get_rune" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_get_rune"
      ~args:[("i64", idx_val); ("i64", str_val)]
  | "String.to_bytes" ->
    let str_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_to_bytes" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_to_bytes"
      ~args:[("i64", str_val)]
  | "String.to_runes" ->
    let str_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_to_runes" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_to_runes"
      ~args:[("i64", str_val)]
  (* Rune builtins *)
  | "Rune.to_int" | "__rune_to_int" ->
    emit_expr ctx (List.hd args)  (* identity: runes are tagged ints *)
  | "Rune.of_int" | "__rune_of_int" ->
    emit_expr ctx (List.hd args)  (* identity *)
  | "__rune_to_string" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_rune_to_string" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_rune_to_string"
      ~args:[("i64", arg_val)]
  | "Stdlib.mod" ->
    let a = emit_expr ctx (List.nth args 0) in
    let b = emit_expr ctx (List.nth args 1) in
    let ua = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:a ~rhs:"1" in
    let ub = Ir_emit.emit_binop ctx.ir ~op:"ashr" ~ty:"i64" ~lhs:b ~rhs:"1" in
    emit_divmod_zero_check ctx ub;
    let rem = Ir_emit.emit_binop ctx.ir ~op:"srem" ~ty:"i64" ~lhs:ua ~rhs:ub in
    let shifted = Ir_emit.emit_binop ctx.ir ~op:"shl" ~ty:"i64" ~lhs:rem ~rhs:"1" in
    Ir_emit.emit_binop ctx.ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
  | "copy_continuation" | "Stdlib.copy_continuation" ->
    let arg_val = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_copy_continuation" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_copy_continuation"
      ~args:[("i64", arg_val)]
  (* Math builtins *)
  | "__math_pow" ->
    let a = emit_expr ctx (List.nth args 0) in
    let b = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_math_pow" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_math_pow"
      ~args:[("i64", a); ("i64", b)]
  | "__math_sqrt" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_math_sqrt" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_math_sqrt"
      ~args:[("i64", a)]
  | "__math_floor" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_math_floor" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_math_floor"
      ~args:[("i64", a)]
  | "__math_ceil" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_math_ceil" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_math_ceil"
      ~args:[("i64", a)]
  | "__math_round" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_math_round" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_math_round"
      ~args:[("i64", a)]
  (* Byte builtins *)
  | "__byte_of_int" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_byte_of_int" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_byte_of_int"
      ~args:[("i64", a)]
  | "__byte_to_int" ->
    emit_expr ctx (List.hd args)  (* identity: bytes are tagged ints *)
  | "__byte_to_string" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_byte_to_string" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_byte_to_string"
      ~args:[("i64", a)]
  (* Format builtins *)
  | "__fmt_float" ->
    let prec = emit_expr ctx (List.nth args 0) in
    let v = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_fmt_float_str" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_fmt_float_str"
      ~args:[("i64", prec); ("i64", v)]
  | "__fmt_hex" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_fmt_hex" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_fmt_hex"
      ~args:[("i64", a)]
  | "__fmt_hex_upper" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_fmt_hex_upper" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_fmt_hex_upper"
      ~args:[("i64", a)]
  | "__fmt_oct" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_fmt_oct" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_fmt_oct"
      ~args:[("i64", a)]
  | "__fmt_bin" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_fmt_bin" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_fmt_bin"
      ~args:[("i64", a)]
  | "__fmt_zero_pad" ->
    let w = emit_expr ctx (List.nth args 0) in
    let s = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_fmt_zero_pad" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_fmt_zero_pad"
      ~args:[("i64", w); ("i64", s)]
  | "__fmt_pad_left" ->
    let w = emit_expr ctx (List.nth args 0) in
    let s = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_fmt_pad_left" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_fmt_pad_left"
      ~args:[("i64", w); ("i64", s)]
  | "__fmt_pad_right" ->
    let w = emit_expr ctx (List.nth args 0) in
    let s = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_fmt_pad_right" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_fmt_pad_right"
      ~args:[("i64", w); ("i64", s)]
  (* String extra builtins *)
  | "String.split" ->
    let delim = emit_expr ctx (List.nth args 0) in
    let str = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_split" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_split"
      ~args:[("i64", str); ("i64", delim)]
  | "String.trim" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_trim" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_trim"
      ~args:[("i64", a)]
  | "String.starts_with" ->
    let prefix = emit_expr ctx (List.nth args 0) in
    let str = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_starts_with" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_starts_with"
      ~args:[("i64", str); ("i64", prefix)]
  | "String.replace" ->
    let old_s = emit_expr ctx (List.nth args 0) in
    let new_s = emit_expr ctx (List.nth args 1) in
    let str = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_string_replace" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_replace"
      ~args:[("i64", str); ("i64", old_s); ("i64", new_s)]
  | "String.to_int" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_to_int" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_to_int"
      ~args:[("i64", a)]
  | "String.to_float" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_to_float" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_to_float"
      ~args:[("i64", a)]
  | "String.uppercase" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_uppercase" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_uppercase"
      ~args:[("i64", a)]
  | "String.lowercase" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_lowercase" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_lowercase"
      ~args:[("i64", a)]
  | "String.to_byte_array" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_to_byte_array" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_to_byte_array"
      ~args:[("i64", a)]
  | "String.of_byte_array" ->
    let a = emit_expr ctx (List.hd args) in
    add_extern ctx "mml_string_of_byte_array" "i64" ["i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_of_byte_array"
      ~args:[("i64", a)]
  | "String.make" ->
    let n = emit_expr ctx (List.nth args 0) in
    let ch = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_make" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_make"
      ~args:[("i64", n); ("i64", ch)]
  | "String.index_opt" ->
    let str = emit_expr ctx (List.nth args 0) in
    let sub = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_index_opt" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_index_opt"
      ~args:[("i64", str); ("i64", sub)]
  | "String.rindex_opt" ->
    let str = emit_expr ctx (List.nth args 0) in
    let sub = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_rindex_opt" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_rindex_opt"
      ~args:[("i64", str); ("i64", sub)]
  | "String.concat" ->
    let sep = emit_expr ctx (List.nth args 0) in
    let lst = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_concat_list" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_concat_list"
      ~args:[("i64", sep); ("i64", lst)]
  | "String.compare" ->
    let a = emit_expr ctx (List.nth args 0) in
    let b = emit_expr ctx (List.nth args 1) in
    add_extern ctx "mml_string_compare_str" "i64" ["i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_string_compare_str"
      ~args:[("i64", a); ("i64", b)]
  | "Array.sub" ->
    let arr = emit_expr ctx (List.nth args 0) in
    let start = emit_expr ctx (List.nth args 1) in
    let len = emit_expr ctx (List.nth args 2) in
    add_extern ctx "mml_array_sub" "i64" ["i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_array_sub"
      ~args:[("i64", arr); ("i64", start); ("i64", len)]
  | _ ->
    (* Look up user-defined function *)
    (match lookup_var ctx name with
     | Some (Func (llvm_name, arity)) | Some (FuncLocal (llvm_name, arity, _)) ->
       let n_args = List.length args in
       if n_args = arity then begin
         (* Exact call: direct call *)
         let arg_vals = List.map (emit_expr ctx) args in
         let arg_pairs = List.map (fun v -> ("i64", v)) arg_vals in
         Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:llvm_name ~args:arg_pairs
       end else begin
         (* Partial or over-application: wrap as closure, call via mml_applyN *)
         let closure_val = emit_var ctx name f_expr.Typechecker.ty in
         let arg_vals = List.map (emit_expr ctx) args in
         emit_closure_apply ctx closure_val arg_vals
       end
     | Some (Local _ | MutLocal _ | MutRefCell _ | Global _ | MutGlobal _) ->
       (* Variable holds a closure value — indirect call *)
       let closure_val = emit_var ctx name f_expr.Typechecker.ty in
       let arg_vals = List.map (emit_expr ctx) args in
       emit_closure_apply ctx closure_val arg_vals
     | None ->
       (* Try as builtin/operator *)
       (match emit_builtin_as_value ctx name f_expr.Typechecker.ty with
        | Some closure_val ->
          let arg_vals = List.map (emit_expr ctx) args in
          emit_closure_apply ctx closure_val arg_vals
        | None ->
          failwith (Printf.sprintf "native codegen: unbound function %s (not yet implemented)" name)))

(* ---- Closure creation helpers ---- *)

(** Generate a wrapper function that takes (ptr %env, args...) and calls a non-env function *)
and emit_func_wrapper ctx wrapper_name real_name arity =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  if arity <= 8 then begin
    (* Fast path: individual parameters *)
    let params = ("ptr", "%env") ::
      List.init arity (fun i -> ("i64", Printf.sprintf "%%a%d" i)) in
    Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_name ~params;
    Ir_emit.emit_label fn_ir "entry";
    let call_args = List.init arity (fun i ->
      ("i64", Printf.sprintf "%%a%d" i)
    ) in
    let result = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:real_name ~args:call_args in
    Ir_emit.emit_ret fn_ir "i64" result
  end else begin
    (* High arity: array-based convention (env, args_ptr) *)
    let params = [("ptr", "%env"); ("ptr", "%args")] in
    Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_name ~params;
    Ir_emit.emit_label fn_ir "entry";
    let call_args = List.init arity (fun i ->
      let ptr = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%args" ~index:i in
      let v = Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr in
      ("i64", v)
    ) in
    let result = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:real_name ~args:call_args in
    Ir_emit.emit_ret fn_ir "i64" result
  end;
  Ir_emit.emit_define_end fn_ir;

  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Wrap a known non-env function as a closure value *)
and emit_func_as_closure ctx llvm_name arity =
  let wrapper_name = Printf.sprintf "%s_wrap" llvm_name in
  if not (Hashtbl.mem ctx.generated_wrappers wrapper_name) then begin
    Hashtbl.replace ctx.generated_wrappers wrapper_name ();
    emit_func_wrapper ctx wrapper_name llvm_name arity
  end;
  emit_make_closure ctx ~fn_name:wrapper_name ~arity ~captures:[]

(** Emit an anonymous lambda as a closure value *)
and emit_lambda_as_closure ctx (expr : Typechecker.texpr) =
  let (params, body) = flatten_fun expr in
  let arity = List.length params in
  if arity = 0 then
    failwith "native codegen: zero-arity lambda";
  let fn_name = Printf.sprintf "mml_anon_%d" ctx.fn_counter in
  ctx.fn_counter <- ctx.fn_counter + 1;
  let free = free_vars_of_fun ~ctx:(Some ctx) params body in
  if free = [] then begin
    (* No captures: emit function with env param (ignored), wrap in closure *)
    emit_closure_function ctx fn_name params body [];
    emit_make_closure ctx ~fn_name ~arity ~captures:[]
  end else begin
    (* Has captures: emit function that loads captures from env *)
    let free_with_info = List.map (fun name ->
      match lookup_var ctx name with
      | Some info -> (name, info)
      | None -> failwith (Printf.sprintf "native codegen: free var %s not found" name)
    ) free in
    emit_closure_function ctx fn_name params body free_with_info;
    let captures = List.map (fun (name, info) ->
      emit_capture_value ctx name info
    ) free_with_info in
    emit_make_closure ctx ~fn_name ~arity ~captures
  end

(** Emit the value to capture for a free variable *)
and emit_capture_value ctx _name info =
  match info with
  | Local ptr -> Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr
  | MutLocal ptr -> Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr
  | MutRefCell alloca_ptr ->
    (* Capture the heap pointer itself (not the value) *)
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:alloca_ptr
  | Global gname -> Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:(Printf.sprintf "@%s" gname)
  | MutGlobal gname -> Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:(Printf.sprintf "@%s" gname)
  | Func (llvm_name, arity) ->
    emit_func_as_closure ctx llvm_name arity
  | FuncLocal (_, _, alloca_ptr) ->
    Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:alloca_ptr

(** Emit a function that takes (ptr %env, params...) and loads captures from env *)
and emit_closure_function ctx fn_name params body free_with_info =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  let arity = List.length params in
  let high_arity = arity > 8 in
  (* Deduplicate parameter names (e.g. two _ params in fold callback) *)
  let seen = Hashtbl.create 8 in
  let unique_params = List.map (fun p ->
    let base = sanitize_name p in
    if Hashtbl.mem seen base then begin
      let n = Hashtbl.find seen base in
      Hashtbl.replace seen base (n + 1);
      (p, Printf.sprintf "param_%s_%d" base n)
    end else begin
      Hashtbl.replace seen base 1;
      (p, Printf.sprintf "param_%s" base)
    end
  ) params in
  let all_params =
    if high_arity then
      [("ptr", "%env"); ("ptr", "%args")]
    else
      ("ptr", "%env") ::
      List.map (fun (_, llvm_p) -> ("i64", Printf.sprintf "%%%s" llvm_p)) unique_params
  in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:fn_name ~params:all_params;
  Ir_emit.emit_label fn_ir "entry";
  ctx.current_label <- "entry";
  ctx.loop_stack <- [];

  (* Copy Func/FuncLocal/Global/MutGlobal from outer scope *)
  let fn_scope = Hashtbl.create 16 in
  List.iter (fun scope ->
    Hashtbl.iter (fun name info ->
      match info with
      | Func _ | FuncLocal _ | Global _ | MutGlobal _ ->
        if not (Hashtbl.mem fn_scope name) then
          Hashtbl.replace fn_scope name info
      | Local _ | MutLocal _ | MutRefCell _ -> ()
    ) scope
  ) saved_scopes;
  ctx.scopes <- [fn_scope];

  (* Load captures from env struct at offsets 3+ *)
  List.iteri (fun i (name, info) ->
    let env_slot = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%env" ~index:(3 + i) in
    let cap_val = Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr:env_slot in
    (match info with
     | MutLocal _ | MutRefCell _ | MutGlobal _ ->
       (* Mutable capture: cap_val is the heap ref cell pointer.
          Store in alloca and bind as MutRefCell *)
       let alloca_ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
       Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr:alloca_ptr;
       bind_var ctx name (MutRefCell alloca_ptr)
     | _ ->
       let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
       Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr;
       bind_var ctx name (Local ptr))
  ) free_with_info;

  (* Bind parameters *)
  if high_arity then begin
    (* High arity: unpack params from %args array *)
    List.iteri (fun i (p, _) ->
      let arg_ptr = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%args" ~index:i in
      let arg_val = Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr:arg_ptr in
      let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
      Ir_emit.emit_store fn_ir ~ty:"i64" ~value:arg_val ~ptr;
      bind_var ctx p (Local ptr)
    ) unique_params
  end else
    List.iter (fun (p, llvm_p) ->
      let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
      Ir_emit.emit_store fn_ir ~ty:"i64"
        ~value:(Printf.sprintf "%%%s" llvm_p) ~ptr;
      bind_var ctx p (Local ptr)
    ) unique_params;

  let body_result = emit_expr ctx body in
  Ir_emit.emit_ret fn_ir "i64" body_result;
  Ir_emit.emit_define_end fn_ir;

  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Generate a unary runtime wrapper function (env, arg) -> call rt_fn(arg) *)
and emit_rt_unary_wrapper ctx wrapper_name rt_fn_name =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in
  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;
  let params = [("ptr", "%env"); ("i64", "%a0")] in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_name ~params;
  Ir_emit.emit_label fn_ir "entry";
  add_extern ctx rt_fn_name "i64" ["i64"];
  let result = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:rt_fn_name
    ~args:[("i64", "%a0")] in
  Ir_emit.emit_ret fn_ir "i64" result;
  Ir_emit.emit_define_end fn_ir;
  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';
  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Generate a binary runtime wrapper function (env, a0, a1) -> call rt_fn(a0, a1) *)
and emit_rt_binary_wrapper ctx wrapper_name rt_fn_name =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in
  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;
  let params = [("ptr", "%env"); ("i64", "%a0"); ("i64", "%a1")] in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_name ~params;
  Ir_emit.emit_label fn_ir "entry";
  add_extern ctx rt_fn_name "i64" ["i64"; "i64"];
  let result = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:rt_fn_name
    ~args:[("i64", "%a0"); ("i64", "%a1")] in
  Ir_emit.emit_ret fn_ir "i64" result;
  Ir_emit.emit_define_end fn_ir;
  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';
  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Generate a ternary runtime wrapper function (env, a0, a1, a2) -> call rt_fn(a0, a1, a2) *)
and emit_rt_ternary_wrapper ctx wrapper_name rt_fn_name =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in
  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;
  let params = [("ptr", "%env"); ("i64", "%a0"); ("i64", "%a1"); ("i64", "%a2")] in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_name ~params;
  Ir_emit.emit_label fn_ir "entry";
  add_extern ctx rt_fn_name "i64" ["i64"; "i64"; "i64"];
  let result = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:rt_fn_name
    ~args:[("i64", "%a0"); ("i64", "%a1"); ("i64", "%a2")] in
  Ir_emit.emit_ret fn_ir "i64" result;
  Ir_emit.emit_define_end fn_ir;
  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';
  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Generate a show closure for the given type *)
and emit_show_closure ctx ty =
  let arg_ty = match ty with
    | Types.TArrow (arg, _, _) -> Types.repr arg
    | _ -> Types.TUnit
  in
  match arg_ty with
  | Types.TList elem_ty ->
    let show_elem = emit_show_closure ctx (Types.TArrow (elem_ty, Types.EffEmpty, Types.TString)) in
    let fn_id = ctx.fn_counter in
    ctx.fn_counter <- ctx.fn_counter + 1;
    let wrapper_name = Printf.sprintf "mml_op_show_list_%d" fn_id in
    emit_show_compound_wrapper ctx wrapper_name "mml_show_list" 1;
    emit_make_closure ctx ~fn_name:wrapper_name ~arity:1 ~captures:[show_elem]
  | Types.TArray elem_ty ->
    let show_elem = emit_show_closure ctx (Types.TArrow (elem_ty, Types.EffEmpty, Types.TString)) in
    let fn_id = ctx.fn_counter in
    ctx.fn_counter <- ctx.fn_counter + 1;
    let wrapper_name = Printf.sprintf "mml_op_show_array_%d" fn_id in
    emit_show_compound_wrapper ctx wrapper_name "mml_show_array" 1;
    emit_make_closure ctx ~fn_name:wrapper_name ~arity:1 ~captures:[show_elem]
  | Types.TTuple tys ->
    (* Build a MML list of show closures, one per element *)
    let show_fns_list = List.fold_right (fun ety acc ->
      let show_fn = emit_show_closure ctx (Types.TArrow (Types.repr ety, Types.EffEmpty, Types.TString)) in
      let cell_ptr = emit_alloc ctx 16 (make_header mml_hdr_cons) in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:show_fn ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:0);
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:acc ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:1);
      Ir_emit.emit_ptrtoint ctx.ir ~value:cell_ptr
    ) tys (tag_int 0) in
    let fn_id = ctx.fn_counter in
    ctx.fn_counter <- ctx.fn_counter + 1;
    let wrapper_name = Printf.sprintf "mml_op_show_tuple_%d" fn_id in
    emit_show_compound_wrapper ctx wrapper_name "mml_show_tuple" 1;
    emit_make_closure ctx ~fn_name:wrapper_name ~arity:1 ~captures:[show_fns_list]
  | Types.TVariant (type_name, ty_args) ->
    (match List.assoc_opt type_name ctx.variant_defs with
    | None ->
      (* Unknown variant — fall through to generic *)
      let wrapper_key = "mml_op_show_generic" in
      if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
        Hashtbl.replace ctx.generated_wrappers wrapper_key ();
        emit_rt_unary_wrapper ctx wrapper_key "mml_show_value"
      end;
      add_extern ctx "mml_show_value" "i64" ["i64"];
      emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[]
    | Some vdef ->
      (* Instantiate TGen references with concrete type args *)
      let instantiate_ty ty =
        let args = Array.of_list ty_args in
        let rec go t = match Types.repr t with
          | Types.TGen i when i < Array.length args -> args.(i)
          | Types.TTuple ts -> Types.TTuple (List.map go ts)
          | Types.TVariant (n, ts) -> Types.TVariant (n, List.map go ts)
          | Types.TList t' -> Types.TList (go t')
          | Types.TArray t' -> Types.TArray (go t')
          | _ -> t
        in
        go ty
      in
      (* Build a MML list of (tag, name_str, show_fn_or_unit) triples *)
      let ctors_list = List.fold_right (fun (i, (ctor_name, payload_ty)) acc ->
        let tag_val = tag_int i in
        let name_str = emit_expr ctx { Typechecker.expr = TEString ctor_name;
          ty = Types.TString; loc = Token.dummy_loc } in
        let show_fn = match payload_ty with
          | None -> unit_value
          | Some pty ->
            let pty' = instantiate_ty (Types.repr pty) in
            emit_show_closure ctx (Types.TArrow (pty', Types.EffEmpty, Types.TString))
        in
        let triple_ptr = emit_alloc ctx 24 (make_header ~size:3 mml_hdr_tuple) in
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:tag_val ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:triple_ptr ~index:0);
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:name_str ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:triple_ptr ~index:1);
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:show_fn ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:triple_ptr ~index:2);
        let triple_val = Ir_emit.emit_ptrtoint ctx.ir ~value:triple_ptr in
        let cell_ptr = emit_alloc ctx 16 (make_header mml_hdr_cons) in
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:triple_val ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:0);
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:acc ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:1);
        Ir_emit.emit_ptrtoint ctx.ir ~value:cell_ptr
      ) (List.mapi (fun i c -> (i, c)) vdef) (tag_int 0) in
      let fn_id = ctx.fn_counter in
      ctx.fn_counter <- ctx.fn_counter + 1;
      let wrapper_name = Printf.sprintf "mml_op_show_variant_%d" fn_id in
      emit_show_compound_wrapper ctx wrapper_name "mml_show_variant" 1;
      emit_make_closure ctx ~fn_name:wrapper_name ~arity:1 ~captures:[ctors_list])
  | Types.TRecord row ->
    (* Collect fields from the record row *)
    let fields = Types.record_row_to_fields row in
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    (* Build a MML list of (name_str, show_fn) pairs *)
    let fields_list = List.fold_right (fun (fname, fty) acc ->
      let name_str = emit_expr ctx { Typechecker.expr = TEString fname;
        ty = Types.TString; loc = Token.dummy_loc } in
      let show_fn = emit_show_closure ctx (Types.TArrow (Types.repr fty, Types.EffEmpty, Types.TString)) in
      (* Create a 2-tuple: [name_str, show_fn] *)
      let pair_ptr = emit_alloc ctx 16 (make_header mml_hdr_pair) in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:name_str ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:pair_ptr ~index:0);
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:show_fn ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:pair_ptr ~index:1);
      let pair_val = Ir_emit.emit_ptrtoint ctx.ir ~value:pair_ptr in
      (* Cons onto the list *)
      let cell_ptr = emit_alloc ctx 16 (make_header mml_hdr_cons) in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:pair_val ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:0);
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:acc ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:1);
      Ir_emit.emit_ptrtoint ctx.ir ~value:cell_ptr
    ) sorted (tag_int 0) in
    (* Generate wrapper: mml_show_record(fields_list, value) *)
    let fn_id = ctx.fn_counter in
    ctx.fn_counter <- ctx.fn_counter + 1;
    let wrapper_name = Printf.sprintf "mml_op_show_record_%d" fn_id in
    emit_show_compound_wrapper ctx wrapper_name "mml_show_record" 1;
    emit_make_closure ctx ~fn_name:wrapper_name ~arity:1 ~captures:[fields_list]
  | Types.TPolyVariant row ->
    (* Collect tags from the poly variant row *)
    let rec collect_tags acc = function
      | Types.PVRow (name, payload_ty, rest) ->
        let tag = Hashtbl.hash name land 0x3FFFFFFF in
        collect_tags ((tag, name, payload_ty) :: acc) rest
      | Types.PVVar { contents = Types.PVLink r } -> collect_tags acc r
      | _ -> List.rev acc
    in
    let tags = collect_tags [] row in
    (* Build a MML list of (hash, name_string) pairs at compile time *)
    let names_list = List.fold_right (fun (tag, name, _) acc ->
      (* Create the name string constant *)
      let name_str = emit_expr ctx { Typechecker.expr = TEString name;
        ty = Types.TString; loc = Token.dummy_loc } in
      let hash_val = tag_int tag in
      (* Create a 2-tuple: [hash, name_str] *)
      let pair_ptr = emit_alloc ctx 16 (make_header mml_hdr_pair) in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:hash_val ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:pair_ptr ~index:0);
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:name_str ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:pair_ptr ~index:1);
      let pair_val = Ir_emit.emit_ptrtoint ctx.ir ~value:pair_ptr in
      (* Cons onto the list *)
      let cell_ptr = emit_alloc ctx 16 (make_header mml_hdr_cons) in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:pair_val ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:0);
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:acc ~ptr:(Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:1);
      Ir_emit.emit_ptrtoint ctx.ir ~value:cell_ptr
    ) tags (tag_int 0) (* nil = tagged int 0 *) in
    (* Create a show closure for payloads — try to use a specific type if all payloads share one *)
    let payload_tys = List.filter_map (fun (_, _, pty) -> pty) tags in
    let payload_ty = match payload_tys with
      | [ty] -> Types.repr ty
      | _ -> Types.TUnit  (* multiple/no payload types — generic fallback below *)
    in
    let show_payload = if payload_ty = Types.TUnit && payload_tys <> [] then
      (* Mixed payload types: use mml_show_value *)
      let wrapper_key = "mml_op_show_generic" in
      if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
        Hashtbl.replace ctx.generated_wrappers wrapper_key ();
        emit_rt_unary_wrapper ctx wrapper_key "mml_show_value"
      end;
      add_extern ctx "mml_show_value" "i64" ["i64"];
      emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[]
    else
      emit_show_closure ctx (Types.TArrow (payload_ty, Types.EffEmpty, Types.TString))
    in
    (* Generate wrapper: mml_show_polyvariant(names_list, show_payload, value) *)
    let fn_id = ctx.fn_counter in
    ctx.fn_counter <- ctx.fn_counter + 1;
    let wrapper_name = Printf.sprintf "mml_op_show_pv_%d" fn_id in
    emit_show_compound_wrapper ctx wrapper_name "mml_show_polyvariant" 2;
    emit_make_closure ctx ~fn_name:wrapper_name ~arity:1 ~captures:[names_list; show_payload]
  | _ ->
    let (rt_fn, wrapper_key) = match arg_ty with
      | Types.TInt -> ("mml_string_of_int", "mml_op_show_int")
      | Types.TFloat -> ("mml_string_of_float", "mml_op_show_float")
      | Types.TBool -> ("mml_string_of_bool", "mml_op_show_bool")
      | Types.TString -> ("mml_identity", "mml_op_show_string")
      | Types.TUnit -> ("mml_show_unit", "mml_op_show_unit")
      | Types.TByte -> ("mml_show_byte", "mml_op_show_byte")
      | Types.TRune -> ("mml_show_rune", "mml_op_show_rune")
      | _ -> ("mml_show_value", "mml_op_show_generic")  (* fallback: runtime heuristic *)
    in
    if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
      Hashtbl.replace ctx.generated_wrappers wrapper_key ();
      emit_rt_unary_wrapper ctx wrapper_key rt_fn
    end;
    emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[]

(** Generate a closure wrapper for compound show: loads N captures from env and calls rt_fn *)
and emit_show_compound_wrapper ctx wrapper_name rt_fn_name n_captures =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in
  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;
  let params = [("ptr", "%env"); ("i64", "%a0")] in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_name ~params;
  Ir_emit.emit_label fn_ir "entry";
  (* Load captures from closure env (slots 3, 4, ...) *)
  let cap_vals = List.init n_captures (fun i ->
    let cap_ptr = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%env" ~index:(3 + i) in
    Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr:cap_ptr
  ) in
  (* Call runtime function: rt_fn(cap0, cap1, ..., a0) *)
  let all_tys = List.init (n_captures + 1) (fun _ -> "i64") in
  add_extern ctx rt_fn_name "i64" all_tys;
  let args = List.map (fun v -> ("i64", v)) cap_vals @ [("i64", "%a0")] in
  let result = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:rt_fn_name ~args in
  Ir_emit.emit_ret fn_ir "i64" result;
  Ir_emit.emit_define_end fn_ir;
  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';
  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Generate a fold closure — dispatches to array or list fold based on type *)
and emit_fold_closure ctx ty =
  (* fold has type: (acc -> elem -> acc) -> acc -> coll -> acc
     The collection type is the 3rd argument *)
  let coll_ty = match ty with
    | Types.TArrow (_, _, Types.TArrow (_, _, Types.TArrow (ct, _, _))) -> Types.repr ct
    | _ -> Types.TUnit
  in
  let (rt_fn, wrapper_key) = match coll_ty with
    | Types.TArray _ -> ("mml_array_fold_breakable", "mml_op_fold_array")
    | _ -> ("mml_list_fold_breakable", "mml_op_fold_list")
  in
  if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
    Hashtbl.replace ctx.generated_wrappers wrapper_key ();
    emit_rt_ternary_wrapper ctx wrapper_key rt_fn
  end;
  emit_make_closure ctx ~fn_name:wrapper_key ~arity:3 ~captures:[]

(** Generate an at (index) closure — type-specialized *)
and emit_at_closure ctx ty =
  let coll_ty = match ty with
    | Types.TArrow (_, _, Types.TArrow (ct, _, _)) -> Types.repr ct
    | _ -> Types.TUnit
  in
  let is_map_index = match coll_ty with
    | Types.TVariant ("map", _) -> true
    | _ -> false
  in
  let (rt_fn, wrapper_key) = match coll_ty with
    | _ when is_map_index -> ("mml_assoc_get", "mml_op_at_map")
    | Types.TArray _ -> ("mml_array_get", "mml_op_at_array")
    | Types.TList _ -> ("mml_list_nth", "mml_op_at_list")
    | Types.TString -> ("mml_string_get_byte", "mml_op_at_string")
    | _ -> ("mml_list_nth", "mml_op_at_generic")  (* fallback *)
  in
  if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
    Hashtbl.replace ctx.generated_wrappers wrapper_key ();
    emit_rt_binary_wrapper ctx wrapper_key rt_fn
  end;
  emit_make_closure ctx ~fn_name:wrapper_key ~arity:2 ~captures:[]

(** Generate an operator closure based on the operator name and result type *)
and emit_operator_closure ctx op_name expr_ty =
  let ty = Types.repr expr_ty in
  (* Handle 'show' operator specially — it's a 1-arity function *)
  if op_name = "fold" then
    emit_fold_closure ctx ty
  else if op_name = "at" then
    emit_at_closure ctx ty
  else if op_name = "show" then
    emit_show_closure ctx ty
  else
  (* Determine if int or float based on result type *)
  let is_float = match ty with
    | Types.TArrow (Types.TFloat, _, _) -> true
    | Types.TArrow (_, _, Types.TArrow (Types.TFloat, _, _)) -> true
    | _ ->
      (* Check first arg type *)
      (match ty with
       | Types.TArrow (arg_ty, _, _) ->
         (match Types.repr arg_ty with Types.TFloat -> true | _ -> false)
       | _ -> false)
  in
  let op_suffix = match op_name with
    | "+" -> "add" | "-" -> "sub" | "*" -> "mul" | "/" -> "div"
    | "mod" -> "mod" | "neg" -> "neg" | "not" -> "not"
    | "<" -> "lt" | ">" -> "gt" | "<=" -> "lte" | ">=" -> "gte"
    | "=" -> "eq" | "<>" -> "neq" | "land" -> "land" | "lor" -> "lor"
    | "lxor" -> "lxor" | "lsl" -> "lsl" | "lsr" -> "lsr" | "lnot" -> "lnot"
    | s -> sanitize_name s
  in
  let wrapper_key = Printf.sprintf "mml_op_%s_%s"
    (if is_float then "float" else "int")
    op_suffix in
  if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
    Hashtbl.replace ctx.generated_wrappers wrapper_key ();
    emit_operator_wrapper ctx wrapper_key op_name is_float
  end;
  let arity = match op_name with "not" | "neg" | "lnot" -> 1 | _ -> 2 in
  emit_make_closure ctx ~fn_name:wrapper_key ~arity ~captures:[]

(** Generate an operator wrapper function *)
and emit_operator_wrapper ctx wrapper_name op_name is_float =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  let arity = match op_name with "not" | "neg" | "lnot" -> 1 | _ -> 2 in
  let params = ("ptr", "%env") ::
    List.init arity (fun i -> ("i64", Printf.sprintf "%%a%d" i)) in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_name ~params;
  Ir_emit.emit_label fn_ir "entry";

  let result =
    if arity = 1 then begin
      match op_name with
      | "not" -> Ir_emit.emit_binop fn_ir ~op:"xor" ~ty:"i64" ~lhs:"%a0" ~rhs:"2"
      | "neg" when is_float ->
        let d = Ir_emit.emit_call fn_ir ~ret_ty:"double" ~name:"mml_unbox_float"
          ~args:[("i64", "%a0")] in
        let neg_d = Ir_emit.emit_float_binop fn_ir ~op:"fsub"
          ~lhs:"0.0" ~rhs:d in
        add_extern ctx "mml_box_float" "i64" ["double"];
        Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:"mml_box_float"
          ~args:[("double", neg_d)]
      | "neg" ->
        let untagged = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let negated = Ir_emit.emit_binop fn_ir ~op:"sub" ~ty:"i64" ~lhs:"0" ~rhs:untagged in
        let shifted = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:negated ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
      | "lnot" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let r = Ir_emit.emit_binop fn_ir ~op:"xor" ~ty:"i64" ~lhs:ua ~rhs:"-1" in
        let s = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:r ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:s ~rhs:"1"
      | _ -> failwith (Printf.sprintf "native codegen: unknown unary operator %s" op_name)
    end else if is_float then begin
      let da = emit_unbox_float ctx "%a0" in
      let db = emit_unbox_float ctx "%a1" in
      let fop, is_cmp = match op_name with
        | "+" -> "fadd", false | "-" -> "fsub", false
        | "*" -> "fmul", false | "/" -> "fdiv", false
        | "<" -> "olt", true | ">" -> "ogt", true
        | "<=" -> "ole", true | ">=" -> "oge", true
        | "=" -> "oeq", true | "<>" -> "one", true
        | _ -> failwith (Printf.sprintf "native codegen: unknown float operator %s" op_name)
      in
      if is_cmp then begin
        let cmp = Ir_emit.emit_fcmp fn_ir ~cmp:fop ~lhs:da ~rhs:db in
        Ir_emit.emit_select fn_ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
      end else begin
        let r = Ir_emit.emit_float_binop fn_ir ~op:fop ~lhs:da ~rhs:db in
        emit_box_float ctx r
      end
    end else begin
      (* Integer operators - use tagged arithmetic *)
      match op_name with
      | "+" ->
        let sum = Ir_emit.emit_binop fn_ir ~op:"add" ~ty:"i64" ~lhs:"%a0" ~rhs:"%a1" in
        Ir_emit.emit_binop fn_ir ~op:"sub" ~ty:"i64" ~lhs:sum ~rhs:"1"
      | "-" ->
        let diff = Ir_emit.emit_binop fn_ir ~op:"sub" ~ty:"i64" ~lhs:"%a0" ~rhs:"%a1" in
        Ir_emit.emit_binop fn_ir ~op:"add" ~ty:"i64" ~lhs:diff ~rhs:"1"
      | "*" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let ub = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a1" ~rhs:"1" in
        let prod = Ir_emit.emit_binop fn_ir ~op:"mul" ~ty:"i64" ~lhs:ua ~rhs:ub in
        let shifted = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:prod ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
      | "/" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let ub = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a1" ~rhs:"1" in
        emit_divmod_zero_check_ir ctx fn_ir ub;
        let quot = Ir_emit.emit_binop fn_ir ~op:"sdiv" ~ty:"i64" ~lhs:ua ~rhs:ub in
        let shifted = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:quot ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
      | "mod" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let ub = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a1" ~rhs:"1" in
        emit_divmod_zero_check_ir ctx fn_ir ub;
        let rem = Ir_emit.emit_binop fn_ir ~op:"srem" ~ty:"i64" ~lhs:ua ~rhs:ub in
        let shifted = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:rem ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:shifted ~rhs:"1"
      | "<" ->
        let cmp = Ir_emit.emit_icmp fn_ir ~cmp:"slt" ~ty:"i64" ~lhs:"%a0" ~rhs:"%a1" in
        Ir_emit.emit_select fn_ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
      | ">" ->
        let cmp = Ir_emit.emit_icmp fn_ir ~cmp:"sgt" ~ty:"i64" ~lhs:"%a0" ~rhs:"%a1" in
        Ir_emit.emit_select fn_ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
      | "<=" ->
        let cmp = Ir_emit.emit_icmp fn_ir ~cmp:"sle" ~ty:"i64" ~lhs:"%a0" ~rhs:"%a1" in
        Ir_emit.emit_select fn_ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
      | ">=" ->
        let cmp = Ir_emit.emit_icmp fn_ir ~cmp:"sge" ~ty:"i64" ~lhs:"%a0" ~rhs:"%a1" in
        Ir_emit.emit_select fn_ir ~cond:cmp ~ty:"i64" ~if_true:true_value ~if_false:false_value
      | "=" ->
        (* Use structural equality for typeclass operators *)
        add_extern ctx "mml_structural_eq" "i64" ["i64"; "i64"];
        Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:"mml_structural_eq"
          ~args:[("i64", "%a0"); ("i64", "%a1")]
      | "<>" ->
        add_extern ctx "mml_structural_eq" "i64" ["i64"; "i64"];
        let eq = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:"mml_structural_eq"
          ~args:[("i64", "%a0"); ("i64", "%a1")] in
        let cmp = Ir_emit.emit_icmp fn_ir ~cmp:"eq" ~ty:"i64" ~lhs:eq ~rhs:true_value in
        Ir_emit.emit_select fn_ir ~cond:cmp ~ty:"i64" ~if_true:false_value ~if_false:true_value
      | "land" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let ub = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a1" ~rhs:"1" in
        let r = Ir_emit.emit_binop fn_ir ~op:"and" ~ty:"i64" ~lhs:ua ~rhs:ub in
        let s = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:r ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:s ~rhs:"1"
      | "lor" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let ub = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a1" ~rhs:"1" in
        let r = Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:ua ~rhs:ub in
        let s = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:r ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:s ~rhs:"1"
      | "lxor" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let ub = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a1" ~rhs:"1" in
        let r = Ir_emit.emit_binop fn_ir ~op:"xor" ~ty:"i64" ~lhs:ua ~rhs:ub in
        let s = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:r ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:s ~rhs:"1"
      | "lsl" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let ub = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a1" ~rhs:"1" in
        let r = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:ua ~rhs:ub in
        let s = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:r ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:s ~rhs:"1"
      | "lsr" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let ub = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a1" ~rhs:"1" in
        let r = Ir_emit.emit_binop fn_ir ~op:"lshr" ~ty:"i64" ~lhs:ua ~rhs:ub in
        let s = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:r ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:s ~rhs:"1"
      | "lnot" ->
        let ua = Ir_emit.emit_binop fn_ir ~op:"ashr" ~ty:"i64" ~lhs:"%a0" ~rhs:"1" in
        let r = Ir_emit.emit_binop fn_ir ~op:"xor" ~ty:"i64" ~lhs:ua ~rhs:"-1" in
        let s = Ir_emit.emit_binop fn_ir ~op:"shl" ~ty:"i64" ~lhs:r ~rhs:"1" in
        Ir_emit.emit_binop fn_ir ~op:"or" ~ty:"i64" ~lhs:s ~rhs:"1"
      | _ ->
        failwith (Printf.sprintf "native codegen: unknown int operator %s" op_name)
    end
  in
  Ir_emit.emit_ret fn_ir "i64" result;
  Ir_emit.emit_define_end fn_ir;

  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Try to emit a builtin name as a closure value *)
and emit_builtin_as_value ctx name expr_ty =
  match name with
  | "not" ->
    let wrapper_key = "mml_op_int_not" in
    if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
      Hashtbl.replace ctx.generated_wrappers wrapper_key ();
      emit_operator_wrapper ctx wrapper_key "not" false
    end;
    Some (emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[])
  | "^" ->
    let wrapper_key = "mml_op_concat" in
    if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
      Hashtbl.replace ctx.generated_wrappers wrapper_key ();
      emit_concat_wrapper ctx wrapper_key
    end;
    Some (emit_make_closure ctx ~fn_name:wrapper_key ~arity:2 ~captures:[])
  | "print" | "Stdlib.print" ->
    Some (emit_print_closure ctx expr_ty)
  | "string_of_int" | "Stdlib.string_of_int" ->
    let wrapper_key = "mml_op_string_of_int" in
    if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
      Hashtbl.replace ctx.generated_wrappers wrapper_key ();
      emit_func_wrapper ctx wrapper_key "mml_string_of_int" 1
    end;
    add_extern ctx "mml_string_of_int" "i64" ["i64"];
    Some (emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[])
  | "float_of_int" | "Stdlib.float_of_int" ->
    let wrapper_key = "mml_op_float_of_int" in
    if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
      Hashtbl.replace ctx.generated_wrappers wrapper_key ();
      emit_func_wrapper ctx wrapper_key "mml_float_of_int" 1
    end;
    add_extern ctx "mml_float_of_int" "i64" ["i64"];
    Some (emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[])
  | "int_of_float" | "Stdlib.int_of_float" ->
    let wrapper_key = "mml_op_int_of_float" in
    if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
      Hashtbl.replace ctx.generated_wrappers wrapper_key ();
      emit_func_wrapper ctx wrapper_key "mml_int_of_float" 1
    end;
    add_extern ctx "mml_int_of_float" "i64" ["i64"];
    Some (emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[])
  | "__show_value" ->
    let wrapper_key = "mml_op_show_value" in
    if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
      Hashtbl.replace ctx.generated_wrappers wrapper_key ();
      emit_rt_unary_wrapper ctx wrapper_key "mml_show_value"
    end;
    add_extern ctx "mml_show_value" "i64" ["i64"];
    Some (emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[])
  | "show" ->
    (* Bare 'show' as a value — create a show closure based on the function type *)
    Some (emit_show_closure ctx expr_ty)
  (* Typeclass primitive externs — map to emit_operator_closure *)
  | "__num_add_int" | "__num_add_float" -> Some (emit_operator_closure ctx "+" expr_ty)
  | "__num_sub_int" | "__num_sub_float" -> Some (emit_operator_closure ctx "-" expr_ty)
  | "__num_mul_int" | "__num_mul_float" -> Some (emit_operator_closure ctx "*" expr_ty)
  | "__num_div_int" | "__num_div_float" -> Some (emit_operator_closure ctx "/" expr_ty)
  | "__num_neg_int" | "__num_neg_float" -> Some (emit_operator_closure ctx "neg" expr_ty)
  | "__eq_int" | "__eq_float" | "__eq_string" | "__eq_bool" | "__eq_byte" | "__eq_rune" ->
    Some (emit_operator_closure ctx "=" expr_ty)
  | "__neq_int" | "__neq_float" | "__neq_string" | "__neq_bool" | "__neq_byte" | "__neq_rune" ->
    Some (emit_operator_closure ctx "<>" expr_ty)
  | "__lt_int" | "__lt_float" | "__lt_string" | "__lt_byte" | "__lt_rune" ->
    Some (emit_operator_closure ctx "<" expr_ty)
  | "__gt_int" | "__gt_float" | "__gt_string" | "__gt_byte" | "__gt_rune" ->
    Some (emit_operator_closure ctx ">" expr_ty)
  | "__le_int" | "__le_float" | "__le_string" | "__le_byte" | "__le_rune" ->
    Some (emit_operator_closure ctx "<=" expr_ty)
  | "__ge_int" | "__ge_float" | "__ge_string" | "__ge_byte" | "__ge_rune" ->
    Some (emit_operator_closure ctx ">=" expr_ty)
  | "__band_int" -> Some (emit_operator_closure ctx "land" expr_ty)
  | "__bor_int" -> Some (emit_operator_closure ctx "lor" expr_ty)
  | "__bxor_int" -> Some (emit_operator_closure ctx "lxor" expr_ty)
  | "__bshl_int" -> Some (emit_operator_closure ctx "lsl" expr_ty)
  | "__bshr_int" -> Some (emit_operator_closure ctx "lsr" expr_ty)
  | "__bnot_int" -> Some (emit_operator_closure ctx "lnot" expr_ty)
  | "__show_int" | "__show_float" | "__show_bool" | "__show_string"
  | "__show_unit" | "__show_byte" | "__show_rune" ->
    Some (emit_operator_closure ctx "show" expr_ty)
  | "__index_at_array" | "__index_at_string" ->
    Some (emit_operator_closure ctx "at" expr_ty)
  | _ -> None

(** Generate a string concat wrapper function *)
and emit_concat_wrapper ctx wrapper_name =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_name
    ~params:[("ptr", "%env"); ("i64", "%a0"); ("i64", "%a1")];
  Ir_emit.emit_label fn_ir "entry";
  add_extern ctx "mml_string_concat" "i64" ["i64"; "i64"];
  let result = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:"mml_string_concat"
    ~args:[("i64", "%a0"); ("i64", "%a1")] in
  Ir_emit.emit_ret fn_ir "i64" result;
  Ir_emit.emit_define_end fn_ir;

  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Generate a print closure using the appropriate print function for the type *)
and emit_print_closure ctx expr_ty =
  (* Determine argument type from the function type: print has type T -> unit *)
  let arg_ty = match Types.repr expr_ty with
    | Types.TArrow (arg, _, _) -> Types.repr arg
    | _ -> Types.TUnit  (* unknown arg type — fall back to generic print *)
  in
  let (print_fn, wrapper_key) = match arg_ty with
    | Types.TInt -> ("mml_print_int", "mml_op_print_int")
    | Types.TBool -> ("mml_print_bool", "mml_op_print_bool")
    | Types.TString -> ("mml_print_string", "mml_op_print_string")
    | Types.TFloat -> ("mml_print_float", "mml_op_print_float")
    | Types.TUnit -> ("mml_print_unit", "mml_op_print_unit")
    | _ -> ("mml_print_value", "mml_op_print")
  in
  if not (Hashtbl.mem ctx.generated_wrappers wrapper_key) then begin
    Hashtbl.replace ctx.generated_wrappers wrapper_key ();
    let saved_ir = ctx.ir in
    let saved_label = ctx.current_label in
    let saved_scopes = ctx.scopes in
    let saved_loop_stack = ctx.loop_stack in

    let fn_ir = Ir_emit.create () in
    ctx.ir <- fn_ir;

    Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:wrapper_key
      ~params:[("ptr", "%env"); ("i64", "%a0")];
    Ir_emit.emit_label fn_ir "entry";
    add_extern ctx print_fn "i64" ["i64"];
    let result = Ir_emit.emit_call fn_ir ~ret_ty:"i64" ~name:print_fn
      ~args:[("i64", "%a0")] in
    Ir_emit.emit_ret fn_ir "i64" result;
    Ir_emit.emit_define_end fn_ir;

    Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
    Buffer.add_char ctx.fn_buf '\n';

    ctx.ir <- saved_ir;
    ctx.current_label <- saved_label;
    ctx.scopes <- saved_scopes;
    ctx.loop_stack <- saved_loop_stack
  end;
  ctx.has_print_output <- true;
  emit_make_closure ctx ~fn_name:wrapper_key ~arity:1 ~captures:[]

(* ---- Function emission ---- *)

and flatten_fun (expr : Typechecker.texpr) : string list * Typechecker.texpr =
  let rec collect params e =
    match e.Typechecker.expr with
    | TEFun (param, body, _is_gen) -> collect (param :: params) body
    | _ -> (List.rev params, e)
  in
  collect [] expr

and sanitize_name name =
  (* Replace dots and other special chars for LLVM *)
  String.map (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
       (c >= '0' && c <= '9') || c = '_' then c
    else '_'
  ) name

and is_builtin_name = function
  | "string_of_int" | "Stdlib.string_of_int"
  | "float_of_int" | "Stdlib.float_of_int"
  | "int_of_float" | "Stdlib.int_of_float"
  | "String.length" | "String.of_byte" | "String.get"
  | "String.sub" | "String.contains" | "String.to_list"
  | "String.rune_length" | "String.of_bytes" | "String.of_runes"
  | "String.get_rune" | "String.to_bytes" | "String.to_runes"
  | "String.split" | "String.trim" | "String.starts_with"
  | "String.replace" | "String.to_int" | "String.to_float"
  | "String.uppercase" | "String.lowercase"
  | "String.to_byte_array" | "String.of_byte_array"
  | "String.make" | "String.index_opt" | "String.rindex_opt"
  | "String.concat" | "String.compare"
  | "Byte.of_int" | "Byte.to_int" | "Byte.to_string"
  | "__byte_of_int" | "__byte_to_int" | "__byte_to_string"
  | "Ref.create" | "Ref.get" | "Ref.set"
  | "List.hd" | "List.tl" | "List.rev" | "List.length" | "List.map"
  | "List.fold" | "List.fold_left"
  | "List.filter" | "List.find" | "List.find_map"
  | "List.exists" | "List.forall" | "List.iter" | "List.mapi"
  | "List.concat" | "List.flatten" | "List.sort"
  | "List.assoc_opt" | "List.fold_right" | "List.init"
  | "List.map2" | "List.iter2"
  | "array_get" | "array_set" | "array_length"
  | "Array.length" | "Array.get" | "Array.set" | "Array.make"
  | "Array.of_list" | "Array.to_list" | "Array.sub"
  | "Array.copy" | "Array.init" | "Array.map" | "Array.mapi" | "Array.iter"
  | "Array.fold"
  | "Rune.to_int" | "Rune.of_int"
  | "__rune_to_int" | "__rune_of_int" | "__rune_to_string"
  | "__math_pow" | "__math_sqrt" | "__math_floor" | "__math_ceil" | "__math_round"
  | "__fmt_float" | "__fmt_hex" | "__fmt_hex_upper" | "__fmt_oct" | "__fmt_bin"
  | "__fmt_zero_pad" | "__fmt_pad_left" | "__fmt_pad_right"
  | "Stdlib.mod"
  | "failwith"
  | "phys_equal" | "Stdlib.phys_equal"
  | "copy_continuation" | "Stdlib.copy_continuation"
  | "print" | "Stdlib.print"
  | "__show_value" | "show"
  | "size" | "get" | "set" | "has" | "remove" | "keys" | "values" | "of_list" -> true
  | _ -> false

and is_dict_or_inst name =
  let len = String.length name in
  len >= 7 && (String.sub name 0 7 = "__dict_" || String.sub name 0 7 = "__inst_")

and free_vars_of_fun ?(ctx=None) params body =
  let param_set = Hashtbl.create 16 in
  List.iter (fun p -> Hashtbl.replace param_set p ()) params;
  let free = ref [] in
  let bound = Hashtbl.create 16 in
  let is_known_global name =
    match ctx with
    | Some c ->
      (match lookup_var c name with
       | Some (Func _) | Some (FuncLocal _) | Some (Global _) | Some (MutGlobal _) -> true
       | _ -> false)
    | None -> false
  in
  let rec scan_expr (e : Typechecker.texpr) =
    match e.expr with
    | TEVar name ->
      if not (Hashtbl.mem param_set name) && not (Hashtbl.mem bound name)
         && not (is_known_global name)
         && not (is_builtin_name name) then
        if not (List.mem name !free) then begin
          if is_dict_or_inst name then begin
            (* Only capture dict variables that are actually in an enclosing scope
               (e.g., dict parameters). Skip built-in dicts that will be generated lazily. *)
            match ctx with
            | Some c when lookup_var c name <> None -> free := name :: !free
            | _ -> ()
          end else
            free := name :: !free
        end
    | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEUnit
    | TEByte _ | TERune _ -> ()
    | TEBinop (_, a, b) -> scan_expr a; scan_expr b
    | TEUnop (_, a) -> scan_expr a
    | TEApp (a, b) -> scan_expr a; scan_expr b
    | TESeq (a, b) -> scan_expr a; scan_expr b
    | TEIf (c, t, e) -> scan_expr c; scan_expr t; scan_expr e
    | TELet (n, _, init, body) ->
      scan_expr init;
      let was_bound = Hashtbl.mem bound n in
      Hashtbl.replace bound n ();
      scan_expr body;
      if not was_bound then Hashtbl.remove bound n
    | TELetRec (n, _, init, body) ->
      let was_bound = Hashtbl.mem bound n in
      Hashtbl.replace bound n ();
      scan_expr init;
      scan_expr body;
      if not was_bound then Hashtbl.remove bound n
    | TELetRecAnd (bindings, body) ->
      let prev = List.map (fun (n, _) -> (n, Hashtbl.mem bound n)) bindings in
      List.iter (fun (n, _) -> Hashtbl.replace bound n ()) bindings;
      List.iter (fun (_, e) -> scan_expr e) bindings;
      scan_expr body;
      List.iter (fun (n, was) -> if not was then Hashtbl.remove bound n) prev
    | TELetMut (n, init, body) ->
      scan_expr init;
      let was_bound = Hashtbl.mem bound n in
      Hashtbl.replace bound n ();
      scan_expr body;
      if not was_bound then Hashtbl.remove bound n
    | TEAssign (name, e) ->
      if not (Hashtbl.mem param_set name) && not (Hashtbl.mem bound name)
         && not (is_known_global name)
         && not (is_builtin_name name) then
        if not (List.mem name !free) then
          free := name :: !free;
      scan_expr e
    | TEFun (p, body, _) ->
      let was_param = Hashtbl.mem param_set p in
      Hashtbl.replace param_set p ();
      scan_expr body;
      if not was_param then Hashtbl.remove param_set p
    | TEWhile (c, b) -> scan_expr c; scan_expr b
    | TEBreak e -> scan_expr e
    | TEContinueLoop -> ()
    | TEReturn e -> scan_expr e
    | TETuple es -> List.iter scan_expr es
    | TERecord fields -> List.iter (fun (_, e) -> scan_expr e) fields
    | TERecordUpdate (base, overrides) ->
      scan_expr base; List.iter (fun (_, e) -> scan_expr e) overrides
    | TERecordUpdateIdx (base, pairs) ->
      scan_expr base; List.iter (fun (i, v) -> scan_expr i; scan_expr v) pairs
    | TEField (e, _) -> scan_expr e
    | TEFieldAssign (e1, _, e2) -> scan_expr e1; scan_expr e2
    | TEConstruct (_, arg) -> Option.iter scan_expr arg
    | TENil -> ()
    | TECons (hd, tl) -> scan_expr hd; scan_expr tl
    | TEArray elems -> List.iter scan_expr elems
    | TEForLoop e -> scan_expr e
    | TEFoldContinue e -> scan_expr e
    | TEIndex (e1, e2) -> scan_expr e1; scan_expr e2
    | TEMatch (scrut, arms, _) ->
      scan_expr scrut;
      List.iter (fun (pat, guard, body) ->
        scan_pattern pat;
        Option.iter scan_expr guard;
        scan_expr body
      ) arms
    | TEPerform (_op, arg_e) -> scan_expr arg_e
    | TEResume (k_e, v_e) -> scan_expr k_e; scan_expr v_e
    | TEHandle (body_e, arms) ->
      scan_expr body_e;
      List.iter (fun arm -> match arm with
        | Typechecker.THReturn (name, e) ->
          Hashtbl.replace bound name (); scan_expr e; Hashtbl.remove bound name
        | Typechecker.THOp (_, arg, k, e) ->
          Hashtbl.replace bound arg (); Hashtbl.replace bound k ();
          scan_expr e;
          Hashtbl.remove bound arg; Hashtbl.remove bound k
        | Typechecker.THOpProvide (_, arg, e) ->
          Hashtbl.replace bound arg (); scan_expr e; Hashtbl.remove bound arg
        | Typechecker.THOpTry (_, arg, e) ->
          Hashtbl.replace bound arg (); scan_expr e; Hashtbl.remove bound arg
      ) arms
  and scan_pattern = function
    | Ast.PatVar name ->
      Hashtbl.replace bound name ()
    | Ast.PatTuple pats -> List.iter scan_pattern pats
    | Ast.PatConstruct (_, sub) -> Option.iter scan_pattern sub
    | Ast.PatRecord fields -> List.iter (fun (_, p) -> scan_pattern p) fields
    | Ast.PatAs (p, name) ->
      Hashtbl.replace bound name (); scan_pattern p
    | Ast.PatOr (p1, p2) -> scan_pattern p1; scan_pattern p2
    | Ast.PatAnnot (p, _) -> scan_pattern p
    | Ast.PatCons (p1, p2) -> scan_pattern p1; scan_pattern p2
    | Ast.PatArray pats -> List.iter scan_pattern pats
    | Ast.PatMap entries -> List.iter (fun (_, vp) -> scan_pattern vp) entries
    | Ast.PatPolyVariant (_, sub) -> Option.iter scan_pattern sub
    | Ast.PatPin _ -> () (* pin references existing var, doesn't bind *)
    | _ -> ()
  in
  scan_expr body;
  List.rev !free

and emit_function ctx llvm_name (fn_expr : Typechecker.texpr) =
  let (params, body) = flatten_fun fn_expr in
  emit_named_function ctx llvm_name params body

and emit_named_function ctx llvm_name params body =
  (* Save main emitter state *)
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  (* Create fresh emitter for function body *)
  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  (* Deduplicate parameter names (e.g. two _ params in fold callback) *)
  let seen = Hashtbl.create 8 in
  let unique_params = List.map (fun p ->
    let base = sanitize_name p in
    if Hashtbl.mem seen base then begin
      let n = Hashtbl.find seen base in
      Hashtbl.replace seen base (n + 1);
      (p, Printf.sprintf "param_%s_%d" base n)
    end else begin
      Hashtbl.replace seen base 1;
      (p, Printf.sprintf "param_%s" base)
    end
  ) params in

  (* Emit function header *)
  let param_pairs = List.map (fun (_, llvm_p) ->
    ("i64", Printf.sprintf "%%%s" llvm_p)
  ) unique_params in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:llvm_name ~params:param_pairs;
  Ir_emit.emit_label fn_ir "entry";

  (* Set up scopes: copy Func/Global bindings from outer scope *)
  ctx.current_label <- "entry";
  ctx.loop_stack <- [];
  let fn_scope = Hashtbl.create 16 in
  List.iter (fun scope ->
    Hashtbl.iter (fun name info ->
      match info with
      | Func _ | FuncLocal _ | Global _ | MutGlobal _ ->
        if not (Hashtbl.mem fn_scope name) then
          Hashtbl.replace fn_scope name info
      | Local _ | MutLocal _ | MutRefCell _ -> ()
    ) scope
  ) saved_scopes;
  ctx.scopes <- [fn_scope];

  (* Bind parameters: alloca + store *)
  List.iter (fun (p, llvm_p) ->
    let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
    Ir_emit.emit_store fn_ir ~ty:"i64" ~value:(Printf.sprintf "%%%s" llvm_p) ~ptr;
    bind_var ctx p (Local ptr)
  ) unique_params;

  (* Compile body *)
  let body_result = emit_expr ctx body in
  Ir_emit.emit_ret fn_ir "i64" body_result;
  Ir_emit.emit_define_end fn_ir;

  (* Append function text to fn_buf *)
  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  (* Restore main emitter state *)
  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(** Emit a named function that loads captured variables from the env pointer.
    [self_name] is the name the function uses for recursive self-reference.
    [captures] is the list of free variable names to load from env slots 3, 4, ...
    [capture_modes] specifies how each capture is bound: `Immutable, `Mutable, or `RefCell.
    The function binds [self_name] to the env pointer (which IS the closure). *)
and emit_named_function_with_captures ctx llvm_name params body self_name captures capture_modes =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  (* Deduplicate parameter names *)
  let seen = Hashtbl.create 8 in
  let unique_params = List.map (fun p ->
    let base = sanitize_name p in
    if Hashtbl.mem seen base then begin
      let n = Hashtbl.find seen base in
      Hashtbl.replace seen base (n + 1);
      (p, Printf.sprintf "param_%s_%d" base n)
    end else begin
      Hashtbl.replace seen base 1;
      (p, Printf.sprintf "param_%s" base)
    end
  ) params in

  let arity = List.length params in
  let high_arity = arity > 8 in
  let param_pairs =
    if high_arity then
      [("ptr", "%env"); ("ptr", "%args")]
    else
      ("ptr", "%env") :: List.map (fun (_, llvm_p) ->
        ("i64", Printf.sprintf "%%%s" llvm_p)
      ) unique_params
  in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:llvm_name ~params:param_pairs;
  Ir_emit.emit_label fn_ir "entry";

  ctx.current_label <- "entry";
  ctx.loop_stack <- [];
  let fn_scope = Hashtbl.create 16 in
  List.iter (fun scope ->
    Hashtbl.iter (fun name info ->
      match info with
      | Func _ | FuncLocal _ | Global _ | MutGlobal _ ->
        if not (Hashtbl.mem fn_scope name) then
          Hashtbl.replace fn_scope name info
      | Local _ | MutLocal _ | MutRefCell _ -> ()
    ) scope
  ) saved_scopes;
  ctx.scopes <- [fn_scope];

  (* Bind self-reference: %env IS the closure, convert to i64 *)
  let self_i64 = Ir_emit.emit_ptrtoint fn_ir ~value:"%env" in
  let self_alloca = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
  Ir_emit.emit_store fn_ir ~ty:"i64" ~value:self_i64 ~ptr:self_alloca;
  bind_var ctx self_name (Local self_alloca);

  (* Load captured variables from env slots [3, 4, ...] *)
  List.iteri (fun i cap_name ->
    let cap_ptr = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%env" ~index:(3 + i) in
    let cap_val = Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr:cap_ptr in
    let mode = List.nth capture_modes i in
    match mode with
    | `Mutable ->
      (* Mutable capture: value is a pointer (as i64), bind as MutLocal *)
      let mut_ptr = Ir_emit.emit_inttoptr fn_ir ~value:cap_val in
      bind_var ctx cap_name (MutLocal mut_ptr)
    | `RefCell ->
      (* RefCell capture: value is a heap pointer, store in alloca *)
      let alloca = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
      Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr:alloca;
      bind_var ctx cap_name (MutRefCell alloca)
    | `Immutable ->
      let alloca = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
      Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr:alloca;
      bind_var ctx cap_name (Local alloca)
  ) captures;

  (* Bind parameters *)
  if high_arity then begin
    (* High arity: unpack params from %args array *)
    List.iteri (fun i (p, _) ->
      let arg_ptr = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%args" ~index:i in
      let arg_val = Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr:arg_ptr in
      let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
      Ir_emit.emit_store fn_ir ~ty:"i64" ~value:arg_val ~ptr;
      bind_var ctx p (Local ptr)
    ) unique_params
  end else
    List.iter (fun (p, llvm_p) ->
      let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
      Ir_emit.emit_store fn_ir ~ty:"i64" ~value:(Printf.sprintf "%%%s" llvm_p) ~ptr;
      bind_var ctx p (Local ptr)
    ) unique_params;

  (* Compile body *)
  let body_result = emit_expr ctx body in
  Ir_emit.emit_ret fn_ir "i64" body_result;
  Ir_emit.emit_define_end fn_ir;

  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack

(* ---- Handler arm compilation ---- *)

(** Emit a NUL-terminated C string constant (no MML length prefix).
    Returns the pointer as an i64. Used for operation names passed to runtime. *)
and emit_c_string ctx s =
  let name = Printf.sprintf ".cstr.%d" ctx.str_counter in
  ctx.str_counter <- ctx.str_counter + 1;
  let escaped = Buffer.contents (llvm_escape_string s) in
  let total = String.length s + 1 in (* data + NUL *)
  let decl = Printf.sprintf "@%s = private unnamed_addr constant [%d x i8] c\"%s\\00\""
    name total escaped in
  ctx.string_globals <- decl :: ctx.string_globals;
  Ir_emit.emit_ptrtoint ctx.ir ~value:(Printf.sprintf "@%s" name)

(** Compute free variables for a handler arm body.
    [bound_names] are the names bound by the arm (e.g. arg for provide/try, arg+k for full).
    Returns list of (name, var_info) pairs for variables that need to be captured. *)
and handler_arm_free_vars ctx bound_names body =
  let free = free_vars_of_fun ~ctx:(Some ctx) bound_names body in
  List.filter_map (fun name ->
    match lookup_var ctx name with
    | Some info -> Some (name, info)
    | None -> None
  ) free

(** Emit env array for a handler arm: allocate and store captured values.
    Returns "0" if no captures, otherwise an i64 pointer to the env array. *)
and emit_handler_env ctx free_with_info =
  match free_with_info with
  | [] -> "0"
  | captures ->
    let n = List.length captures in
    let env_ptr = emit_alloc ctx (n * 8) (make_header ~size:n mml_hdr_tuple) in
    List.iteri (fun i (name, info) ->
      let cap_val = emit_capture_value ctx name info in
      let slot = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:env_ptr ~index:i in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:cap_val ~ptr:slot
    ) captures;
    Ir_emit.emit_ptrtoint ctx.ir ~value:env_ptr

(** Emit a handler arm function: (ptr %env, i64 %arg) -> i64.
    Loads captures from env at offsets 0, 1, 2, ...
    [arg_name] is the parameter name bound to the perform argument.
    Returns the LLVM function name. *)
and emit_handler_arm_fn ctx arg_name body free_with_info =
  let fn_name = Printf.sprintf "mml_handler_arm_%d" ctx.fn_counter in
  ctx.fn_counter <- ctx.fn_counter + 1;

  (* Save emitter state *)
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  let params = [("ptr", "%env"); ("i64", "%arg"); ("i64", "%k")] in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:fn_name ~params;
  Ir_emit.emit_label fn_ir "entry";
  ctx.current_label <- "entry";
  ctx.loop_stack <- [];

  (* Copy Func/FuncLocal/Global/MutGlobal from outer scope *)
  let fn_scope = Hashtbl.create 16 in
  List.iter (fun scope ->
    Hashtbl.iter (fun name info ->
      match info with
      | Func _ | FuncLocal _ | Global _ | MutGlobal _ ->
        if not (Hashtbl.mem fn_scope name) then
          Hashtbl.replace fn_scope name info
      | Local _ | MutLocal _ | MutRefCell _ -> ()
    ) scope
  ) saved_scopes;
  ctx.scopes <- [fn_scope];

  (* Load captures from env at offsets 0, 1, 2, ... *)
  List.iteri (fun i (name, info) ->
    let env_slot = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%env" ~index:i in
    let cap_val = Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr:env_slot in
    (match info with
     | MutLocal _ | MutRefCell _ | MutGlobal _ ->
       (* Mutable capture: cap_val is the heap ref cell pointer *)
       let alloca_ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
       Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr:alloca_ptr;
       bind_var ctx name (MutRefCell alloca_ptr)
     | _ ->
       let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
       Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr;
       bind_var ctx name (Local ptr))
  ) free_with_info;

  (* Bind the arg parameter *)
  let arg_ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
  Ir_emit.emit_store fn_ir ~ty:"i64" ~value:"%arg" ~ptr:arg_ptr;
  bind_var ctx arg_name (Local arg_ptr);

  (* Compile body *)
  let body_result = emit_expr ctx body in
  Ir_emit.emit_ret fn_ir "i64" body_result;
  Ir_emit.emit_define_end fn_ir;

  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  (* Restore state *)
  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack;

  fn_name

(** Emit a full handler arm function: (ptr %env, i64 %arg, i64 %k) -> i64.
    Like emit_handler_arm_fn but also binds [k_name] to the continuation
    passed as the third parameter. *)
and emit_full_handler_arm_fn ctx arg_name k_name body free_with_info =
  let fn_name = Printf.sprintf "mml_handler_arm_%d" ctx.fn_counter in
  ctx.fn_counter <- ctx.fn_counter + 1;

  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  let params = [("ptr", "%env"); ("i64", "%arg"); ("i64", "%k")] in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:fn_name ~params;
  Ir_emit.emit_label fn_ir "entry";
  ctx.current_label <- "entry";
  ctx.loop_stack <- [];

  let fn_scope = Hashtbl.create 16 in
  List.iter (fun scope ->
    Hashtbl.iter (fun name info ->
      match info with
      | Func _ | FuncLocal _ | Global _ | MutGlobal _ ->
        if not (Hashtbl.mem fn_scope name) then
          Hashtbl.replace fn_scope name info
      | Local _ | MutLocal _ | MutRefCell _ -> ()
    ) scope
  ) saved_scopes;
  ctx.scopes <- [fn_scope];

  List.iteri (fun i (name, info) ->
    let env_slot = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%env" ~index:i in
    let cap_val = Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr:env_slot in
    (match info with
     | MutLocal _ | MutRefCell _ | MutGlobal _ ->
       let alloca_ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
       Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr:alloca_ptr;
       bind_var ctx name (MutRefCell alloca_ptr)
     | _ ->
       let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
       Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr;
       bind_var ctx name (Local ptr))
  ) free_with_info;

  (* Bind the arg parameter *)
  let arg_ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
  Ir_emit.emit_store fn_ir ~ty:"i64" ~value:"%arg" ~ptr:arg_ptr;
  bind_var ctx arg_name (Local arg_ptr);

  (* Bind the continuation from the %k parameter *)
  let k_ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
  Ir_emit.emit_store fn_ir ~ty:"i64" ~value:"%k" ~ptr:k_ptr;
  bind_var ctx k_name (Local k_ptr);

  let body_result = emit_expr ctx body in
  Ir_emit.emit_ret fn_ir "i64" body_result;
  Ir_emit.emit_define_end fn_ir;

  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack;

  fn_name

(** Emit a handler body thunk: (ptr %env) -> i64.
    Used for try handlers where the body must run inside a C function with setjmp.
    Returns the LLVM function name. *)
and emit_handler_body_thunk ctx body free_with_info =
  let fn_name = Printf.sprintf "mml_handler_body_%d" ctx.fn_counter in
  ctx.fn_counter <- ctx.fn_counter + 1;

  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let saved_loop_stack = ctx.loop_stack in

  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;

  let params = [("ptr", "%env")] in
  Ir_emit.emit_define_start fn_ir ~ret_ty:"i64" ~name:fn_name ~params;
  Ir_emit.emit_label fn_ir "entry";
  ctx.current_label <- "entry";
  ctx.loop_stack <- [];

  let fn_scope = Hashtbl.create 16 in
  List.iter (fun scope ->
    Hashtbl.iter (fun name info ->
      match info with
      | Func _ | FuncLocal _ | Global _ | MutGlobal _ ->
        if not (Hashtbl.mem fn_scope name) then
          Hashtbl.replace fn_scope name info
      | Local _ | MutLocal _ | MutRefCell _ -> ()
    ) scope
  ) saved_scopes;
  ctx.scopes <- [fn_scope];

  List.iteri (fun i (name, info) ->
    let env_slot = Ir_emit.emit_gep fn_ir ~ty:"i64" ~ptr:"%env" ~index:i in
    let cap_val = Ir_emit.emit_load fn_ir ~ty:"i64" ~ptr:env_slot in
    (match info with
     | MutLocal _ | MutRefCell _ | MutGlobal _ ->
       let alloca_ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
       Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr:alloca_ptr;
       bind_var ctx name (MutRefCell alloca_ptr)
     | _ ->
       let ptr = Ir_emit.emit_alloca fn_ir ~ty:"i64" in
       Ir_emit.emit_store fn_ir ~ty:"i64" ~value:cap_val ~ptr;
       bind_var ctx name (Local ptr))
  ) free_with_info;

  let body_result = emit_expr ctx body in
  Ir_emit.emit_ret fn_ir "i64" body_result;
  Ir_emit.emit_define_end fn_ir;

  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';

  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes;
  ctx.loop_stack <- saved_loop_stack;

  fn_name

(** Emit a TEHandle expression.
    Installs a handler on the runtime stack, runs the body, applies the return arm. *)
and emit_handle ctx body_expr arms =
  (* Partition arms *)
  let return_arm = ref None in
  let provide_ops = ref [] in
  let try_ops = ref [] in
  let full_ops = ref [] in
  List.iter (fun arm -> match arm with
    | Typechecker.THReturn (name, e) -> return_arm := Some (name, e)
    | Typechecker.THOpProvide (op, arg, e) ->
      provide_ops := (op, arg, e) :: !provide_ops
    | Typechecker.THOpTry (op, arg, e) ->
      try_ops := (op, arg, e) :: !try_ops
    | Typechecker.THOp (op, arg, k, e) ->
      full_ops := (op, arg, k, e) :: !full_ops
  ) arms;
  let provide_ops = List.rev !provide_ops in
  let try_ops = List.rev !try_ops in
  let full_ops = List.rev !full_ops in
  let has_try_arms = try_ops <> [] in
  let has_full_arms = full_ops <> [] in
  let needs_thunk = has_try_arms || has_full_arms in

  let all_ops = List.length provide_ops + List.length try_ops + List.length full_ops in

  (* Compile handler arm functions and env arrays *)
  let compiled_provide = List.map (fun (op, arg, body) ->
    let free = handler_arm_free_vars ctx [arg] body in
    let fn_name = emit_handler_arm_fn ctx arg body free in
    let env_val = emit_handler_env ctx free in
    (op, fn_name, env_val, "0") (* kind = PROVIDE = 0 *)
  ) provide_ops in
  let compiled_try = List.map (fun (op, arg, body) ->
    let free = handler_arm_free_vars ctx [arg] body in
    let fn_name = emit_handler_arm_fn ctx arg body free in
    let env_val = emit_handler_env ctx free in
    (op, fn_name, env_val, "1") (* kind = TRY = 1 *)
  ) try_ops in
  let compiled_full = List.map (fun (op, arg, k, body) ->
    let free = handler_arm_free_vars ctx [arg; k] body in
    let fn_name = emit_full_handler_arm_fn ctx arg k body free in
    let env_val = emit_handler_env ctx free in
    (op, fn_name, env_val, "2") (* kind = FULL = 2 *)
  ) full_ops in
  let compiled_ops = compiled_provide @ compiled_try @ compiled_full in

  (* Allocate handler struct *)
  add_extern ctx "mml_alloc_handler" "i64" ["i64"];
  let handler_val = Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:"mml_alloc_handler"
    ~args:[("i64", string_of_int all_ops)] in

  (* Set operations on the handler *)
  add_extern ctx "mml_handler_set_op" "void" ["i64"; "i64"; "i64"; "i64"; "i64"; "i64"];
  List.iteri (fun i (op, fn_name, env_val, kind_str) ->
    let op_str = emit_c_string ctx op in
    let fn_ptr = Ir_emit.emit_ptrtoint ctx.ir ~value:(Printf.sprintf "@%s" fn_name) in
    Ir_emit.emit_call_void ctx.ir ~name:"mml_handler_set_op"
      ~args:[("i64", handler_val); ("i64", string_of_int i);
             ("i64", op_str); ("i64", kind_str);
             ("i64", fn_ptr); ("i64", env_val)]
  ) compiled_ops;

  (* Compile return arm function if non-trivial *)
  let return_fn_name, return_env_val = match !return_arm with
    | None -> "0", "0"
    | Some (name, body) ->
      (* Check if it's identity: return x -> x *)
      let is_identity = match body.expr with
        | Typechecker.TEVar v -> v = name
        | _ -> false
      in
      if is_identity then "0", "0"
      else begin
        let free = handler_arm_free_vars ctx [name] body in
        let fn_name = emit_handler_arm_fn ctx name body free in
        let env_val = emit_handler_env ctx free in
        let fn_ptr = Ir_emit.emit_ptrtoint ctx.ir ~value:(Printf.sprintf "@%s" fn_name) in
        fn_ptr, env_val
      end
  in

  (* Set return arm *)
  add_extern ctx "mml_handler_set_return" "void" ["i64"; "i64"; "i64"];
  Ir_emit.emit_call_void ctx.ir ~name:"mml_handler_set_return"
    ~args:[("i64", handler_val); ("i64", return_fn_name); ("i64", return_env_val)];

  (* Push handler *)
  add_extern ctx "mml_push_handler" "void" ["i64"];
  Ir_emit.emit_call_void ctx.ir ~name:"mml_push_handler"
    ~args:[("i64", handler_val)];

  if not needs_thunk then begin
    (* Simple case: provide-only, just run body inline, pop handler, apply return arm *)
    let body_val = emit_expr ctx body_expr in

    (* Pop handler *)
    add_extern ctx "mml_pop_handler" "void" [];
    Ir_emit.emit_call_void ctx.ir ~name:"mml_pop_handler" ~args:[];

    (* Apply return arm *)
    (match !return_arm with
     | None -> body_val
     | Some (name, ret_body) ->
       let is_identity = match ret_body.expr with
         | Typechecker.TEVar v -> v = name
         | _ -> false
       in
       if is_identity then body_val
       else begin
         push_scope ctx;
         let ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
         Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:body_val ~ptr;
         bind_var ctx name (Local ptr);
         let result = emit_expr ctx ret_body in
         pop_scope ctx;
         result
       end)
  end else begin
    (* Has try or full arms: compile body as thunk, run via C runtime *)
    let body_free = handler_arm_free_vars ctx [] body_expr in
    let body_fn_name = emit_handler_body_thunk ctx body_expr body_free in
    let body_env_val = emit_handler_env ctx body_free in
    let body_fn_ptr = Ir_emit.emit_ptrtoint ctx.ir
      ~value:(Printf.sprintf "@%s" body_fn_name) in

    let runtime_fn = if has_full_arms then "mml_run_full_handler" else "mml_run_try_handler" in
    add_extern ctx runtime_fn "i64" ["i64"; "i64"; "i64"; "i64"; "i64"];
    Ir_emit.emit_call ctx.ir ~ret_ty:"i64" ~name:runtime_fn
      ~args:[("i64", handler_val);
             ("i64", body_fn_ptr); ("i64", body_env_val);
             ("i64", return_fn_name); ("i64", return_env_val)]
  end

(* ---- Declaration codegen ---- *)

(** Try to infer a concrete result type from an expression.
    Falls back to Types.repr of the expression's ty, but for unresolved
    type variables, tries to extract a better type from the expression structure
    (e.g., by looking at the return type of applied functions). *)
and infer_result_type (expr : Typechecker.texpr) : Types.ty =
  let ty = Types.repr expr.ty in
  let is_unresolved t = match t with
    | Types.TVar { contents = Types.Unbound _ } | Types.TGen _ -> true
    | _ -> false
  in
  if not (is_unresolved ty) then ty
  else
    (* Type is unresolved — try to get a better type from expression structure *)
    match expr.expr with
    | TEApp (fn_expr, _) ->
      let fn_ty = Types.repr fn_expr.ty in
      (match fn_ty with
       | Types.TArrow (_, _, ret_ty) ->
         let ret = Types.repr ret_ty in
         if not (is_unresolved ret) then ret
         else
           (* Return type also unresolved — try to resolve from dict method *)
           infer_type_from_dict_method fn_expr
       | _ -> ty)
    | TELet (_, _, _, body) | TESeq (_, body) ->
      infer_result_type body
    | TEIf (_, then_e, _) ->
      infer_result_type then_e
    | TEMatch (_, (_, _, first_body) :: _, _) ->
      infer_result_type first_body
    | _ -> ty

and infer_type_from_dict_method (fn_expr : Typechecker.texpr) : Types.ty =
  Types.repr fn_expr.ty

and emit_decl (ctx : codegen_ctx) (decl : Typechecker.tdecl) : unit =
  match decl with
  | TDLet ("_", expr) ->
    let v = emit_expr ctx expr in
    ctx.result_type <- infer_result_type expr;
    (* Store as last result for return *)
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:"%result_ptr";
    ()
  | TDLet (name, expr) ->
    (* Track dict name for self-referencing instances (e.g. recursive tree Iter fold) *)
    let is_dict = String.length name > 7 && String.sub name 0 7 = "__dict_" in
    if is_dict then ctx.current_dict_name <- Some name;
    (match expr.expr with
     | TEFun _ ->
       let (params, body) = flatten_fun expr in
       if params <> [] then begin
         let llvm_name = Printf.sprintf "mml_f_%s" (sanitize_name name) in
         let free = free_vars_of_fun ~ctx:(Some ctx) params body in
         if free <> [] then
           failwith (Printf.sprintf
             "native codegen: top-level function %s captures free variables: %s (Phase 3)"
             name (String.concat ", " free));
         bind_var ctx name (Func (llvm_name, List.length params));
         emit_named_function ctx llvm_name params body;
         ctx.result_type <- Types.TUnit;
         Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:"%result_ptr"
       end else begin
         (* Zero-param lambda: treat as regular value *)
         emit_global_let ctx name expr
       end
     | _ ->
       emit_global_let ctx name expr);
    if is_dict then ctx.current_dict_name <- None
  | TDLetMut (name, expr) ->
    let v = emit_expr ctx expr in
    let gname = ensure_global ctx name in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:(Printf.sprintf "@%s" gname);
    bind_var ctx name (MutGlobal gname);
    ctx.result_type <- Types.TUnit;
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:"%result_ptr"
  | TDLetRec (name, expr) ->
    let (params, body) = flatten_fun expr in
    if params = [] then begin
      (* Non-function recursive value binding *)
      let (placeholder_val, nbytes) = emit_rec_placeholder ctx expr in
      let gname = ensure_global ctx name in
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:placeholder_val ~ptr:(Printf.sprintf "@%s" gname);
      bind_var ctx name (Global gname);
      let computed_val = emit_expr ctx expr in
      emit_backpatch ctx placeholder_val computed_val nbytes;
      ctx.result_type <- Types.TUnit;
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:"%result_ptr"
    end else begin
      let llvm_name = Printf.sprintf "mml_f_%s" (sanitize_name name) in
      bind_var ctx name (Func (llvm_name, List.length params));
      let free = free_vars_of_fun ~ctx:(Some ctx) (name :: params) body in
      if free <> [] then
        failwith (Printf.sprintf
          "native codegen: top-level function %s captures free variables: %s (Phase 3)"
          name (String.concat ", " free));
      emit_named_function ctx llvm_name params body;
      ctx.result_type <- Types.TUnit;
      Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:"%result_ptr"
    end
  | TDLetRecAnd bindings ->
    let all_names = List.map fst bindings in
    (* Classify bindings *)
    let binding_info = List.map (fun (name, expr) ->
      let (params, _) = flatten_fun expr in
      if params <> [] then `Func (name, expr)
      else `Value (name, expr)
    ) bindings in
    (* Phase 1: Register names. Functions get Func bindings, values get placeholders. *)
    let value_infos = ref [] in
    List.iter (fun info ->
      match info with
      | `Func (name, expr) ->
        let (params, _) = flatten_fun expr in
        let llvm_name = Printf.sprintf "mml_f_%s" (sanitize_name name) in
        bind_var ctx name (Func (llvm_name, List.length params))
      | `Value (name, expr) ->
        let (placeholder_val, nbytes) = emit_rec_placeholder ctx expr in
        let gname = ensure_global ctx name in
        Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:placeholder_val ~ptr:(Printf.sprintf "@%s" gname);
        bind_var ctx name (Global gname);
        value_infos := (name, expr, placeholder_val, nbytes) :: !value_infos
    ) binding_info;
    let value_infos = List.rev !value_infos in
    (* Phase 2: Emit all functions *)
    List.iter (fun info ->
      match info with
      | `Func (name, expr) ->
        let (params, body) = flatten_fun expr in
        let llvm_name = match lookup_var ctx name with
          | Some (Func (n, _)) -> n | _ -> failwith "expected Func" in
        let free = free_vars_of_fun ~ctx:(Some ctx) (all_names @ params) body in
        if free <> [] then
          failwith (Printf.sprintf
            "native codegen: top-level function captures free variables: %s (Phase 3)"
            (String.concat ", " free));
        emit_named_function ctx llvm_name params body
      | `Value _ -> ()
    ) binding_info;
    (* Phase 3: Compile and backpatch value bindings *)
    List.iter (fun (_, expr, placeholder_val, nbytes) ->
      let computed_val = emit_expr ctx expr in
      emit_backpatch ctx placeholder_val computed_val nbytes
    ) value_infos;
    ctx.result_type <- Types.TUnit;
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:"%result_ptr"
  | TDExpr expr ->
    let v = emit_expr ctx expr in
    ctx.result_type <- infer_result_type expr;
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:"%result_ptr"
  | TDModule (_name, inner_decls) ->
    (* Per-declaration rollback: if one inner decl fails, others still work *)
    let main_ir = ctx.ir in
    List.iter (fun d ->
      let ir_buf_len = Buffer.length main_ir.Ir_emit.buf in
      let ir_next_reg = main_ir.Ir_emit.next_reg in
      let fn_buf_len = Buffer.length ctx.fn_buf in
      let saved_global_decls = ctx.global_decls in
      let saved_string_globals = ctx.string_globals in
      let saved_float_globals = ctx.float_globals in
      let saved_extern_decls = ctx.extern_decls in
      let saved_scope = Hashtbl.copy (List.hd ctx.scopes) in
      let saved_generated_wrappers = Hashtbl.copy ctx.generated_wrappers in
      let saved_ir = ctx.ir in
      let saved_label = ctx.current_label in
      let saved_scopes = ctx.scopes in
      let saved_loop_stack = ctx.loop_stack in
      try emit_decl ctx d
      with Failure msg ->
        Printf.eprintf "[native] module rollback: %s\n%!" msg;
        ctx.ir <- saved_ir;
        ctx.current_label <- saved_label;
        ctx.scopes <- saved_scopes;
        ctx.loop_stack <- saved_loop_stack;
        let contents = Buffer.contents main_ir.Ir_emit.buf in
        Buffer.clear main_ir.Ir_emit.buf;
        Buffer.add_string main_ir.Ir_emit.buf (String.sub contents 0 ir_buf_len);
        main_ir.Ir_emit.next_reg <- ir_next_reg;
        let fn_contents = Buffer.contents ctx.fn_buf in
        Buffer.clear ctx.fn_buf;
        Buffer.add_string ctx.fn_buf (String.sub fn_contents 0 fn_buf_len);
        ctx.global_decls <- saved_global_decls;
        ctx.string_globals <- saved_string_globals;
        ctx.float_globals <- saved_float_globals;
        ctx.extern_decls <- saved_extern_decls;
        let top = List.hd ctx.scopes in
        Hashtbl.reset top;
        Hashtbl.iter (fun k v -> Hashtbl.replace top k v) saved_scope;
        Hashtbl.reset ctx.generated_wrappers;
        Hashtbl.iter (fun k v -> Hashtbl.replace ctx.generated_wrappers k v) saved_generated_wrappers
    ) inner_decls
  | TDOpen alias_pairs ->
    List.iter (fun (short_name, qualified_name) ->
      match lookup_var ctx qualified_name with
      | Some info -> bind_var ctx short_name info
      | None -> ()  (* type-only export or unresolvable *)
    ) alias_pairs
  | TDType _ | TDClass _ | TDEffect _ | TDExtern _ ->
    ()

and ensure_global ctx name =
  let gname = Printf.sprintf "mml_g_%s" (sanitize_name name) in
  (* Check if global already declared *)
  let decl = Printf.sprintf "@%s = global i64 0" gname in
  if not (List.mem decl ctx.global_decls) then
    ctx.global_decls <- decl :: ctx.global_decls;
  gname

and emit_global_let ctx name expr =
  let v = emit_expr ctx expr in
  let gname = ensure_global ctx name in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:v ~ptr:(Printf.sprintf "@%s" gname);
  bind_var ctx name (Global gname);
  ctx.result_type <- Types.TUnit;
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:"%result_ptr"

and texpr_kind_name = function
  | Typechecker.TEInt _ -> "TEInt"
  | TEByte _ -> "TEByte"
  | TERune _ -> "TERune"
  | TEFloat _ -> "TEFloat"
  | TEBool _ -> "TEBool"
  | TEString _ -> "TEString"
  | TEUnit -> "TEUnit"
  | TEVar _ -> "TEVar"
  | TELet _ -> "TELet"
  | TELetRec _ -> "TELetRec"
  | TEFun _ -> "TEFun"
  | TEApp _ -> "TEApp"
  | TEIf _ -> "TEIf"
  | TEBinop _ -> "TEBinop"
  | TEUnop _ -> "TEUnop"
  | TETuple _ -> "TETuple"
  | TERecord _ -> "TERecord"
  | TEField _ -> "TEField"
  | TEMatch _ -> "TEMatch"
  | TESeq _ -> "TESeq"
  | TEPerform _ -> "TEPerform"
  | TEHandle _ -> "TEHandle"
  | TEResume _ -> "TEResume"
  | TEWhile _ -> "TEWhile"
  | TELetMut _ -> "TELetMut"
  | TEAssign _ -> "TEAssign"
  | TEBreak _ -> "TEBreak"
  | TEContinueLoop -> "TEContinueLoop"
  | TEReturn _ -> "TEReturn"
  | TELetRecAnd _ -> "TELetRecAnd"
  | TEForLoop _ -> "TEForLoop"
  | TEFoldContinue _ -> "TEFoldContinue"
  | TEIndex _ -> "TEIndex"
  | TERecordUpdateIdx _ -> "TERecordUpdateIdx"
  | _ -> "other"

(* ---- Result type tag ---- *)

let result_type_tag ty =
  match Types.repr ty with
  | Types.TInt -> 0
  | Types.TByte -> 6
  | Types.TBool -> 1
  | Types.TString -> 2
  | Types.TFloat -> 3
  | Types.TUnit -> 4
  | Types.TTuple _ | Types.TRecord _ | Types.TVariant _ | Types.TList _
  | Types.TArray _ | Types.TPolyVariant _ -> 5  (* compound *)
  | Types.TRune -> 7
  | _ -> 4 (* default to unit for unsupported types *)

(* ---- String replacement helper ---- *)

let replace_all s old_str new_str =
  let old_len = String.length old_str in
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i <= String.length s - old_len do
    if String.sub s !i old_len = old_str then begin
      Buffer.add_string buf new_str;
      i := !i + old_len
    end else begin
      Buffer.add_char buf s.[!i];
      i := !i + 1
    end
  done;
  while !i < String.length s do
    Buffer.add_char buf s.[!i];
    i := !i + 1
  done;
  Buffer.contents buf

(* ---- Compound result formatting ---- *)

let needs_format_result ty =
  match Types.repr ty with
  | Types.TTuple _ | Types.TRecord _ | Types.TVariant _ | Types.TList _
  | Types.TPolyVariant _ | Types.TArray _ -> true
  | _ -> false

(** Emit a @mml_format_result function that prints a value of the given type
    without a trailing newline. Generated in LLVM IR by codegen because the
    runtime doesn't know the type structure. *)
let rec emit_format_result ctx ty =
  let saved_ir = ctx.ir in
  let saved_label = ctx.current_label in
  let saved_scopes = ctx.scopes in
  let fn_ir = Ir_emit.create () in
  ctx.ir <- fn_ir;
  ctx.scopes <- [Hashtbl.create 8];
  Ir_emit.emit_define_start fn_ir ~ret_ty:"void" ~name:"mml_format_result"
    ~params:[("i64", "%param_val")];
  Ir_emit.emit_label fn_ir "entry";
  ctx.current_label <- "entry";
  (* Emit formatting code based on type *)
  emit_format_value ctx "%param_val" ty;
  Ir_emit.emit_ret_void fn_ir;
  Ir_emit.emit_define_end fn_ir;
  Buffer.add_string ctx.fn_buf (Ir_emit.contents fn_ir);
  Buffer.add_char ctx.fn_buf '\n';
  ctx.ir <- saved_ir;
  ctx.current_label <- saved_label;
  ctx.scopes <- saved_scopes

and emit_format_value ctx val_reg ty =
  match Types.repr ty with
  | Types.TInt ->
    add_extern ctx "mml_fmt_int" "void" ["i64"];
    Ir_emit.emit_call_void ctx.ir ~name:"mml_fmt_int" ~args:[("i64", val_reg)]
  | Types.TBool ->
    add_extern ctx "mml_fmt_bool" "void" ["i64"];
    Ir_emit.emit_call_void ctx.ir ~name:"mml_fmt_bool" ~args:[("i64", val_reg)]
  | Types.TString ->
    add_extern ctx "mml_fmt_string" "void" ["i64"];
    Ir_emit.emit_call_void ctx.ir ~name:"mml_fmt_string" ~args:[("i64", val_reg)]
  | Types.TFloat ->
    add_extern ctx "mml_fmt_float" "void" ["i64"];
    Ir_emit.emit_call_void ctx.ir ~name:"mml_fmt_float" ~args:[("i64", val_reg)]
  | Types.TUnit ->
    emit_format_str ctx "()"
  | Types.TTuple tys ->
    emit_format_str ctx "(";
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
    List.iteri (fun i elem_ty ->
      if i > 0 then emit_format_str ctx ", ";
      let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
      let elem = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:elem_ptr in
      emit_format_value ctx elem elem_ty
    ) tys;
    emit_format_str ctx ")"
  | Types.TRecord row ->
    let fields = Types.record_row_to_fields row in
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    emit_format_str ctx "{ ";
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
    List.iteri (fun i (fname, fty) ->
      if i > 0 then emit_format_str ctx "; ";
      emit_format_str ctx (fname ^ " = ");
      let elem_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:i in
      let elem = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:elem_ptr in
      emit_format_value ctx elem fty
    ) sorted;
    emit_format_str ctx " }"
  | Types.TVariant (type_name, ty_args) ->
    if List.mem type_name ctx.type_env.Types.newtypes then begin
      (* Newtype: erased at runtime, format using underlying type *)
      match List.assoc_opt type_name ctx.variant_defs with
      | Some [(_, Some underlying_ty)] ->
        let args = Array.of_list ty_args in
        let rec instantiate t = match Types.repr t with
          | Types.TGen i when i < Array.length args -> args.(i)
          | Types.TTuple ts -> Types.TTuple (List.map instantiate ts)
          | Types.TVariant (n, ts) -> Types.TVariant (n, List.map instantiate ts)
          | Types.TList t' -> Types.TList (instantiate t')
          | _ -> t
        in
        emit_format_value ctx val_reg (instantiate underlying_ty)
      | _ -> emit_format_variant ctx val_reg type_name ty_args
    end else
      emit_format_variant ctx val_reg type_name ty_args
  | Types.TList elem_ty ->
    emit_format_str ctx "[";
    let loop_head = fresh_label ctx "list_loop" in
    let loop_body = fresh_label ctx "list_body" in
    let loop_done = fresh_label ctx "list_done" in
    let cur_ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
    let first_ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:val_reg ~ptr:cur_ptr;
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:true_value ~ptr:first_ptr;
    Ir_emit.emit_br ctx.ir ~label:loop_head;
    Ir_emit.emit_label ctx.ir loop_head;
    ctx.current_label <- loop_head;
    let cur = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:cur_ptr in
    let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64" ~lhs:cur ~rhs:"1" in
    let is_nil = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:low_bit ~rhs:"0" in
    Ir_emit.emit_condbr ctx.ir ~cond:is_nil ~if_true:loop_done ~if_false:loop_body;
    Ir_emit.emit_label ctx.ir loop_body;
    ctx.current_label <- loop_body;
    (* Separator: print "; " unless first element *)
    let is_first = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:first_ptr in
    let not_first = Ir_emit.emit_icmp ctx.ir ~cmp:"eq" ~ty:"i64" ~lhs:is_first ~rhs:false_value in
    let sep_label = fresh_label ctx "list_sep" in
    let elem_label = fresh_label ctx "list_elem" in
    Ir_emit.emit_condbr ctx.ir ~cond:not_first ~if_true:sep_label ~if_false:elem_label;
    Ir_emit.emit_label ctx.ir sep_label;
    ctx.current_label <- sep_label;
    emit_format_str ctx "; ";
    Ir_emit.emit_br ctx.ir ~label:elem_label;
    Ir_emit.emit_label ctx.ir elem_label;
    ctx.current_label <- elem_label;
    (* Print element *)
    let cell_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:cur in
    let hd_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:0 in
    let hd_val = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:hd_ptr in
    emit_format_value ctx hd_val elem_ty;
    (* Advance to next *)
    let tl_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:cell_ptr ~index:1 in
    let tl_val = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:tl_ptr in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:tl_val ~ptr:cur_ptr;
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:false_value ~ptr:first_ptr;
    Ir_emit.emit_br ctx.ir ~label:loop_head;
    Ir_emit.emit_label ctx.ir loop_done;
    ctx.current_label <- loop_done;
    emit_format_str ctx "]"
  | Types.TArray elem_ty ->
    emit_format_str ctx "#[";
    (* Check if array is empty (MML_UNIT = tagged int) *)
    let is_empty = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64" ~lhs:val_reg ~rhs:"1" in
    let empty_check = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:is_empty ~rhs:"0" in
    let arr_nonempty = fresh_label ctx "arr_nonempty" in
    let arr_end = fresh_label ctx "arr_end" in
    Ir_emit.emit_condbr ctx.ir ~cond:empty_check ~if_true:arr_end ~if_false:arr_nonempty;
    Ir_emit.emit_label ctx.ir arr_nonempty;
    ctx.current_label <- arr_nonempty;
    (* Get length and data pointer *)
    let arr_ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
    let len_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr:arr_ptr ~index:0 in
    let arr_len = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:len_ptr in
    (* Loop i = 0..len-1 *)
    let idx_ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:"0" ~ptr:idx_ptr;
    let loop_head = fresh_label ctx "arr_loop" in
    let loop_body = fresh_label ctx "arr_body" in
    let loop_done = fresh_label ctx "arr_done" in
    Ir_emit.emit_br ctx.ir ~label:loop_head;
    Ir_emit.emit_label ctx.ir loop_head;
    ctx.current_label <- loop_head;
    let idx = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:idx_ptr in
    let cmp = Ir_emit.emit_icmp ctx.ir ~cmp:"slt" ~ty:"i64" ~lhs:idx ~rhs:arr_len in
    Ir_emit.emit_condbr ctx.ir ~cond:cmp ~if_true:loop_body ~if_false:loop_done;
    Ir_emit.emit_label ctx.ir loop_body;
    ctx.current_label <- loop_body;
    (* Print separator *)
    let not_first = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:idx ~rhs:"0" in
    let sep_label = fresh_label ctx "arr_sep" in
    let elem_label = fresh_label ctx "arr_elem" in
    Ir_emit.emit_condbr ctx.ir ~cond:not_first ~if_true:sep_label ~if_false:elem_label;
    Ir_emit.emit_label ctx.ir sep_label;
    ctx.current_label <- sep_label;
    emit_format_str ctx "; ";
    Ir_emit.emit_br ctx.ir ~label:elem_label;
    Ir_emit.emit_label ctx.ir elem_label;
    ctx.current_label <- elem_label;
    (* Load element: data starts at index 1 from base *)
    let offset = Ir_emit.emit_binop ctx.ir ~op:"add" ~ty:"i64" ~lhs:idx ~rhs:"1" in
    let elem_ptr = Ir_emit.emit_gep_dynamic ctx.ir ~ty:"i64" ~ptr:arr_ptr ~index:offset in
    let elem_val = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:elem_ptr in
    emit_format_value ctx elem_val elem_ty;
    (* Increment index *)
    let next_idx = Ir_emit.emit_binop ctx.ir ~op:"add" ~ty:"i64" ~lhs:idx ~rhs:"1" in
    Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:next_idx ~ptr:idx_ptr;
    Ir_emit.emit_br ctx.ir ~label:loop_head;
    Ir_emit.emit_label ctx.ir loop_done;
    ctx.current_label <- loop_done;
    Ir_emit.emit_br ctx.ir ~label:arr_end;
    Ir_emit.emit_label ctx.ir arr_end;
    ctx.current_label <- arr_end;
    emit_format_str ctx "]"

  | Types.TPolyVariant row ->
    (* Collect all tags from the poly variant row *)
    let rec collect_tags acc = function
      | Types.PVRow (name, payload_ty, rest) ->
        let tag = Hashtbl.hash name land 0x3FFFFFFF in
        collect_tags ((tag, name, payload_ty) :: acc) rest
      | Types.PVVar { contents = Types.PVLink r } -> collect_tags acc r
      | _ -> List.rev acc
    in
    let tags = collect_tags [] row in
    let done_label = fresh_label ctx "fmt_pv_done" in
    let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64" ~lhs:val_reg ~rhs:"1" in
    let is_tagged = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:low_bit ~rhs:"0" in
    let tagged_label = fresh_label ctx "fmt_pv_tagged" in
    let ptr_label = fresh_label ctx "fmt_pv_ptr" in
    Ir_emit.emit_condbr ctx.ir ~cond:is_tagged ~if_true:tagged_label ~if_false:ptr_label;
    (* Tagged int path: no-arg poly variants *)
    Ir_emit.emit_label ctx.ir tagged_label;
    ctx.current_label <- tagged_label;
    let default_tagged = fresh_label ctx "fmt_pv_tagged_default" in
    let tagged_cases = List.filter_map (fun (tag, name, payload_ty) ->
      match payload_ty with
      | None ->
        let label = fresh_label ctx (Printf.sprintf "fmt_pvt_%d" tag) in
        Some (tag_int_value tag, label, name)
      | Some _ -> None
    ) tags in
    Ir_emit.emit_switch ctx.ir ~value:val_reg ~default:default_tagged
      ~cases:(List.map (fun (tag, label, _) -> (tag, label)) tagged_cases);
    List.iter (fun (_, label, name) ->
      Ir_emit.emit_label ctx.ir label;
      ctx.current_label <- label;
      emit_format_str ctx ("`" ^ name);
      Ir_emit.emit_br ctx.ir ~label:done_label
    ) tagged_cases;
    Ir_emit.emit_label ctx.ir default_tagged;
    ctx.current_label <- default_tagged;
    emit_format_str ctx "<poly_variant>";
    Ir_emit.emit_br ctx.ir ~label:done_label;
    (* Pointer path: poly variants with payload *)
    Ir_emit.emit_label ctx.ir ptr_label;
    ctx.current_label <- ptr_label;
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
    let tag_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
    let tag_val = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:tag_ptr in
    let default_ptr = fresh_label ctx "fmt_pv_ptr_default" in
    let ptr_cases = List.filter_map (fun (tag, name, payload_ty) ->
      match payload_ty with
      | Some _ ->
        let label = fresh_label ctx (Printf.sprintf "fmt_pvp_%d" tag) in
        Some (tag_int_value tag, label, name, payload_ty)
      | None -> None
    ) tags in
    Ir_emit.emit_switch ctx.ir ~value:tag_val ~default:default_ptr
      ~cases:(List.map (fun (tag, label, _, _) -> (tag, label)) ptr_cases);
    List.iter (fun (_tag, label, name, payload_ty) ->
      Ir_emit.emit_label ctx.ir label;
      ctx.current_label <- label;
      (match payload_ty with
       | Some pty ->
         emit_format_str ctx ("`" ^ name ^ " ");
         let payload_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
         let payload = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:payload_ptr in
         let needs_parens = match Types.repr pty with
           | Types.TTuple _ | Types.TVariant _ | Types.TPolyVariant _ -> true
           | _ -> false
         in
         if needs_parens then emit_format_str ctx "(";
         emit_format_value ctx payload pty;
         if needs_parens then emit_format_str ctx ")"
       | None ->
         emit_format_str ctx ("`" ^ name));
      Ir_emit.emit_br ctx.ir ~label:done_label
    ) ptr_cases;
    Ir_emit.emit_label ctx.ir default_ptr;
    ctx.current_label <- default_ptr;
    emit_format_str ctx "<poly_variant>";
    Ir_emit.emit_br ctx.ir ~label:done_label;
    Ir_emit.emit_label ctx.ir done_label;
    ctx.current_label <- done_label

  | Types.TByte ->
    add_extern ctx "mml_fmt_byte" "void" ["i64"];
    Ir_emit.emit_call_void ctx.ir ~name:"mml_fmt_byte" ~args:[("i64", val_reg)]
  | Types.TRune ->
    add_extern ctx "mml_fmt_rune" "void" ["i64"];
    Ir_emit.emit_call_void ctx.ir ~name:"mml_fmt_rune" ~args:[("i64", val_reg)]

  | _ ->
    emit_format_str ctx "<unknown>"

and emit_format_str ctx s =
  let name = Printf.sprintf ".str.%d" ctx.str_counter in
  ctx.str_counter <- ctx.str_counter + 1;
  let escaped = Buffer.contents (llvm_escape_string s) in
  let len = String.length s + 1 in
  let decl = Printf.sprintf "@%s = private unnamed_addr constant [%d x i8] c\"%s\\00\""
    name len escaped in
  ctx.string_globals <- decl :: ctx.string_globals;
  let sptr = Ir_emit.emit_ptrtoint ctx.ir ~value:(Printf.sprintf "@%s" name) in
  add_extern ctx "mml_fmt_str" "void" ["i64"];
  Ir_emit.emit_call_void ctx.ir ~name:"mml_fmt_str" ~args:[("i64", sptr)]

and emit_format_variant ctx val_reg type_name ty_args =
  match List.assoc_opt type_name ctx.variant_defs with
  | None -> emit_format_str ctx "<variant>"
  | Some vdef ->
    (* Instantiate TGen references with concrete type args *)
    let instantiate_ty ty =
      let args = Array.of_list ty_args in
      let rec go t = match Types.repr t with
        | Types.TGen i when i < Array.length args -> args.(i)
        | Types.TTuple ts -> Types.TTuple (List.map go ts)
        | Types.TRecord _ -> t  (* keep as-is for now *)
        | Types.TVariant (n, ts) -> Types.TVariant (n, List.map go ts)
        | Types.TList t' -> Types.TList (go t')
        | _ -> t
      in
      go ty
    in
    let done_label = fresh_label ctx "fmt_done" in
    (* Check if value is a tagged int (no-arg constructor) vs pointer (with-arg constructor) *)
    let low_bit = Ir_emit.emit_binop ctx.ir ~op:"and" ~ty:"i64" ~lhs:val_reg ~rhs:"1" in
    let is_tagged = Ir_emit.emit_icmp ctx.ir ~cmp:"ne" ~ty:"i64" ~lhs:low_bit ~rhs:"0" in
    let tagged_label = fresh_label ctx "fmt_tagged" in
    let ptr_label = fresh_label ctx "fmt_ptr" in
    Ir_emit.emit_condbr ctx.ir ~cond:is_tagged ~if_true:tagged_label ~if_false:ptr_label;
    (* Tagged int path: no-arg constructors (None, True, False, etc.) *)
    Ir_emit.emit_label ctx.ir tagged_label;
    ctx.current_label <- tagged_label;
    let default_tagged = fresh_label ctx "fmt_tagged_default" in
    let tagged_cases = List.filter_map (fun (i, (ctor_name, payload_ty)) ->
      match payload_ty with
      | None ->
        let label = fresh_label ctx (Printf.sprintf "fmt_tag_%d" i) in
        Some (tag_int_value i, label, ctor_name)
      | Some _ -> None
    ) (List.mapi (fun i c -> (i, c)) vdef) in
    Ir_emit.emit_switch ctx.ir ~value:val_reg ~default:default_tagged
      ~cases:(List.map (fun (tag, label, _) -> (tag, label)) tagged_cases);
    List.iter (fun (_, label, ctor_name) ->
      Ir_emit.emit_label ctx.ir label;
      ctx.current_label <- label;
      emit_format_str ctx ctor_name;
      Ir_emit.emit_br ctx.ir ~label:done_label
    ) tagged_cases;
    Ir_emit.emit_label ctx.ir default_tagged;
    ctx.current_label <- default_tagged;
    emit_format_str ctx "<unknown>";
    Ir_emit.emit_br ctx.ir ~label:done_label;
    (* Pointer path: constructors with arguments (Some x, etc.) *)
    Ir_emit.emit_label ctx.ir ptr_label;
    ctx.current_label <- ptr_label;
    let ptr = Ir_emit.emit_inttoptr ctx.ir ~value:val_reg in
    let tag_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:0 in
    let tag_val = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:tag_ptr in
    let default_ptr = fresh_label ctx "fmt_ptr_default" in
    let ptr_cases = List.filter_map (fun (i, (ctor_name, payload_ty)) ->
      match payload_ty with
      | Some _ ->
        let label = fresh_label ctx (Printf.sprintf "fmt_ctor_%d" i) in
        Some (tag_int_value i, label, ctor_name, payload_ty)
      | None -> None
    ) (List.mapi (fun i c -> (i, c)) vdef) in
    Ir_emit.emit_switch ctx.ir ~value:tag_val ~default:default_ptr
      ~cases:(List.map (fun (tag, label, _, _) -> (tag, label)) ptr_cases);
    List.iter (fun (_tag, label, ctor_name, payload_ty) ->
      Ir_emit.emit_label ctx.ir label;
      ctx.current_label <- label;
      (match payload_ty with
       | Some pty ->
         let concrete_pty = instantiate_ty pty in
         emit_format_str ctx (ctor_name ^ " ");
         let payload_ptr = Ir_emit.emit_gep ctx.ir ~ty:"i64" ~ptr ~index:1 in
         let payload = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:payload_ptr in
         let needs_parens = match Types.repr concrete_pty with
           | Types.TTuple _ | Types.TVariant _ -> true
           | _ -> false
         in
         if needs_parens then emit_format_str ctx "(";
         emit_format_value ctx payload concrete_pty;
         if needs_parens then emit_format_str ctx ")"
       | None ->
         emit_format_str ctx ctor_name);
      Ir_emit.emit_br ctx.ir ~label:done_label
    ) ptr_cases;
    Ir_emit.emit_label ctx.ir default_ptr;
    ctx.current_label <- default_ptr;
    emit_format_str ctx "<unknown>";
    Ir_emit.emit_br ctx.ir ~label:done_label;
    Ir_emit.emit_label ctx.ir done_label;
    ctx.current_label <- done_label

and tag_int_value n = (n lsl 1) lor 1

(* ---- Top-level entry point ---- *)

let compile_program (program : Typechecker.tprogram) (type_env : Types.type_env) : string =
  let ctx = create_ctx type_env in

  (* Compile all declarations to the main IR emitter *)
  (* We'll build main function body first, then assemble the output *)
  Ir_emit.emit_define_start ctx.ir ~ret_ty:"i64" ~name:"mml_main" ~params:[];
  Ir_emit.emit_label ctx.ir "entry";
  ctx.current_label <- "entry";

  (* Alloca for result *)
  let result_ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:result_ptr;

  (* Compile declarations *)
  List.iter (fun decl ->
    try emit_decl ctx decl
    with Failure msg ->
      (* Re-raise with context for debugging *)
      failwith msg
  ) program;

  (* Load and return result *)
  let result = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:result_ptr in
  Ir_emit.emit_ret ctx.ir "i64" result;
  Ir_emit.emit_define_end ctx.ir;

  (* Always generate format_result function (needed by runtime linkage) *)
  emit_format_result ctx ctx.result_type;

  let main_body = Ir_emit.contents ctx.ir in

  (* Assemble final output *)
  let out = Buffer.create 8192 in

  (* Target triple *)
  Printf.bprintf out "target triple = \"%s\"\n\n" (detect_target_triple ());

  (* Extern declarations *)
  (* Always declare these *)
  let base_externs = [
    ("mml_print_int", "i64", ["i64"]);
    ("mml_print_bool", "i64", ["i64"]);
    ("mml_print_string", "i64", ["i64"]);
    ("mml_print_float", "i64", ["i64"]);
    ("mml_print_unit", "i64", ["i64"]);
    ("mml_print_value", "i64", ["i64"]);
    ("mml_box_float", "i64", ["double"]);
    ("mml_unbox_float", "double", ["i64"]);
    ("mml_alloc", "ptr", ["i64"; "i64"]);
  ] in
  let all_externs = base_externs @ ctx.extern_decls in
  let seen = Hashtbl.create 16 in
  List.iter (fun (name, ret_ty, param_tys) ->
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.replace seen name ();
      let pt_str = match param_tys with
        | [] -> ""
        | _ -> String.concat ", " param_tys
      in
      let attrs = if name = "mml_panic" || name = "mml_panic_mml" then " noreturn" else "" in
      Printf.bprintf out "declare %s @%s(%s)%s\n" ret_ty name pt_str attrs
    end
  ) all_externs;
  Buffer.add_char out '\n';

  (* String globals *)
  List.iter (fun decl ->
    Buffer.add_string out decl;
    Buffer.add_char out '\n'
  ) (List.rev ctx.string_globals);
  if ctx.string_globals <> [] then Buffer.add_char out '\n';

  (* Float globals *)
  List.iter (fun decl ->
    Buffer.add_string out decl;
    Buffer.add_char out '\n'
  ) (List.rev ctx.float_globals);
  if ctx.float_globals <> [] then Buffer.add_char out '\n';

  (* Global variable defs *)
  List.iter (fun decl ->
    Buffer.add_string out decl;
    Buffer.add_char out '\n'
  ) (List.rev ctx.global_decls);

  (* Result type global *)
  let tag = result_type_tag ctx.result_type in
  Printf.bprintf out "@mml_result_type = global i32 %d\n" tag;
  Buffer.add_char out '\n';

  (* Function definitions *)
  if Buffer.length ctx.fn_buf > 0 then begin
    Buffer.add_buffer out ctx.fn_buf;
  end;

  (* Main function — need to fix result_ptr references *)
  (* The first alloca in main will be %0. Replace %result_ptr with %0 *)
  let main_text = main_body in
  (* The alloca produced %0 as the register. The hardcoded "%result_ptr" refs
     need to become the actual register. Since result_ptr is the first alloca,
     it will be %0 *)
  let fixed_main = replace_all main_text "%result_ptr" result_ptr in
  Buffer.add_string out fixed_main;

  Buffer.contents out

let compile_program_with_stdlib (type_env : Types.type_env)
    (stdlib_programs : (Types.type_env * Typechecker.tprogram) list)
    (user_program : Typechecker.tprogram) : string =
  let ctx = create_ctx type_env in

  Ir_emit.emit_define_start ctx.ir ~ret_ty:"i64" ~name:"mml_main" ~params:[];
  Ir_emit.emit_label ctx.ir "entry";
  ctx.current_label <- "entry";

  let result_ptr = Ir_emit.emit_alloca ctx.ir ~ty:"i64" in
  Ir_emit.emit_store ctx.ir ~ty:"i64" ~value:unit_value ~ptr:result_ptr;

  (* Compile stdlib declarations with rollback-on-failure *)
  let main_ir = ctx.ir in
  List.iter (fun (_te, prog) ->
    List.iter (fun decl ->
      let ir_buf_len = Buffer.length main_ir.Ir_emit.buf in
      let ir_next_reg = main_ir.Ir_emit.next_reg in
      let fn_buf_len = Buffer.length ctx.fn_buf in
      let saved_global_decls = ctx.global_decls in
      let saved_string_globals = ctx.string_globals in
      let saved_float_globals = ctx.float_globals in
      let saved_extern_decls = ctx.extern_decls in
      let saved_scope = Hashtbl.copy (List.hd ctx.scopes) in
      let saved_generated_wrappers = Hashtbl.copy ctx.generated_wrappers in
      let saved_ir = ctx.ir in
      let saved_label = ctx.current_label in
      let saved_scopes = ctx.scopes in
      let saved_loop_stack = ctx.loop_stack in
      try emit_decl ctx decl
      with Failure msg ->
        Printf.eprintf "[native] stdlib rollback: %s\n%!" msg;
        (* Restore ctx fields that may have been swapped by nested function emission *)
        ctx.ir <- saved_ir;
        ctx.current_label <- saved_label;
        ctx.scopes <- saved_scopes;
        ctx.loop_stack <- saved_loop_stack;
        (* Rollback main IR buffer *)
        let contents = Buffer.contents main_ir.Ir_emit.buf in
        Buffer.clear main_ir.Ir_emit.buf;
        Buffer.add_string main_ir.Ir_emit.buf (String.sub contents 0 ir_buf_len);
        main_ir.Ir_emit.next_reg <- ir_next_reg;
        (* Rollback fn_buf *)
        let fn_contents = Buffer.contents ctx.fn_buf in
        Buffer.clear ctx.fn_buf;
        Buffer.add_string ctx.fn_buf (String.sub fn_contents 0 fn_buf_len);
        (* Rollback context lists *)
        ctx.global_decls <- saved_global_decls;
        ctx.string_globals <- saved_string_globals;
        ctx.float_globals <- saved_float_globals;
        ctx.extern_decls <- saved_extern_decls;
        (* Rollback scope *)
        let top = List.hd ctx.scopes in
        Hashtbl.reset top;
        Hashtbl.iter (fun k v -> Hashtbl.replace top k v) saved_scope;
        (* Rollback generated wrappers *)
        Hashtbl.reset ctx.generated_wrappers;
        Hashtbl.iter (fun k v -> Hashtbl.replace ctx.generated_wrappers k v) saved_generated_wrappers
    ) prog
  ) stdlib_programs;

  (* Compile user program *)
  List.iter (fun decl ->
    try emit_decl ctx decl
    with Failure msg -> failwith msg
  ) user_program;

  (* Load and return result *)
  let result = Ir_emit.emit_load ctx.ir ~ty:"i64" ~ptr:result_ptr in
  Ir_emit.emit_ret ctx.ir "i64" result;
  Ir_emit.emit_define_end ctx.ir;

  emit_format_result ctx ctx.result_type;

  let main_body = Ir_emit.contents ctx.ir in

  (* Assemble final output *)
  let out = Buffer.create 8192 in

  Printf.bprintf out "target triple = \"%s\"\n\n" (detect_target_triple ());

  let base_externs = [
    ("mml_print_int", "i64", ["i64"]);
    ("mml_print_bool", "i64", ["i64"]);
    ("mml_print_string", "i64", ["i64"]);
    ("mml_print_float", "i64", ["i64"]);
    ("mml_print_unit", "i64", ["i64"]);
    ("mml_print_value", "i64", ["i64"]);
    ("mml_box_float", "i64", ["double"]);
    ("mml_unbox_float", "double", ["i64"]);
    ("mml_alloc", "ptr", ["i64"; "i64"]);
  ] in
  let all_externs = base_externs @ ctx.extern_decls in
  let seen = Hashtbl.create 16 in
  List.iter (fun (name, ret_ty, param_tys) ->
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.replace seen name ();
      let pt_str = match param_tys with
        | [] -> ""
        | _ -> String.concat ", " param_tys
      in
      let attrs = if name = "mml_panic" || name = "mml_panic_mml" then " noreturn" else "" in
      Printf.bprintf out "declare %s @%s(%s)%s\n" ret_ty name pt_str attrs
    end
  ) all_externs;
  Buffer.add_char out '\n';

  List.iter (fun decl ->
    Buffer.add_string out decl;
    Buffer.add_char out '\n'
  ) (List.rev ctx.string_globals);
  if ctx.string_globals <> [] then Buffer.add_char out '\n';

  List.iter (fun decl ->
    Buffer.add_string out decl;
    Buffer.add_char out '\n'
  ) (List.rev ctx.float_globals);
  if ctx.float_globals <> [] then Buffer.add_char out '\n';

  List.iter (fun decl ->
    Buffer.add_string out decl;
    Buffer.add_char out '\n'
  ) (List.rev ctx.global_decls);

  let tag = result_type_tag ctx.result_type in
  Printf.bprintf out "@mml_result_type = global i32 %d\n" tag;
  Buffer.add_char out '\n';

  if Buffer.length ctx.fn_buf > 0 then begin
    Buffer.add_buffer out ctx.fn_buf;
  end;

  let main_text = main_body in
  let fixed_main = replace_all main_text "%result_ptr" result_ptr in
  Buffer.add_string out fixed_main;

  Buffer.contents out
