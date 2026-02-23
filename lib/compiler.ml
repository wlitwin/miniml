exception Compile_error of string

let optimize_enabled = ref true

type local = {
  name: string;
  slot: int;
  depth: int;
}

type upvalue_entry = {
  capture: Bytecode.capture;
  uv_name: string;
}

type state = {
  mutable locals: local list;
  mutable num_locals: int;
  mutable max_locals: int;
  mutable scope_depth: int;
  mutable upvalues: upvalue_entry list;
  mutable code: Bytecode.opcode Dynarray.t;
  mutable constants: Bytecode.value Dynarray.t;
  enclosing: state option;
  proto_name: string;
  arity: int;
  (* Global context *)
  global_names: string Dynarray.t;
  mutable_globals: (string, unit) Hashtbl.t;
  type_env: Types.type_env;
  mutable mutable_locals: string list;
  mutable loop_start: int option;
  mutable operand_depth: int;
  lines: int Dynarray.t;
  mutable current_line: int;
}

let error msg = raise (Compile_error msg)

let create_state enclosing arity global_names mutable_globals type_env name =
  { locals = [];
    num_locals = 0;
    max_locals = 0;
    scope_depth = 0;
    upvalues = [];
    code = Dynarray.create ();
    constants = Dynarray.create ();
    enclosing;
    proto_name = name;
    arity;
    global_names;
    mutable_globals;
    type_env;
    mutable_locals = [];
    loop_start = None;
    operand_depth = 0;
    lines = Dynarray.create ();
    current_line = 0;
  }

let emit s op =
  Dynarray.add_last s.code op;
  Dynarray.add_last s.lines s.current_line

let emit_idx s op =
  let idx = Dynarray.length s.code in
  Dynarray.add_last s.code op;
  Dynarray.add_last s.lines s.current_line;
  idx

let patch s idx op =
  Dynarray.set s.code idx op

let current_offset s =
  Dynarray.length s.code

let add_constant s v =
  (* Check for duplicate *)
  let len = Dynarray.length s.constants in
  let rec find i =
    if i >= len then begin
      Dynarray.add_last s.constants v;
      len
    end else if Dynarray.get s.constants i = v then i
    else find (i + 1)
  in
  find 0

let emit_constant s v =
  let idx = add_constant s v in
  emit s (Bytecode.CONST idx)

(* ---- Local variable management ---- *)

let allocate_local s name =
  let slot = s.num_locals in
  s.locals <- { name; slot; depth = s.scope_depth } :: s.locals;
  s.num_locals <- s.num_locals + 1;
  if s.num_locals > s.max_locals then
    s.max_locals <- s.num_locals;
  slot

let free_local s =
  match s.locals with
  | [] -> error "no locals to free"
  | _ :: rest ->
    s.locals <- rest;
    s.num_locals <- s.num_locals - 1

let find_local s name =
  let rec search = function
    | [] -> None
    | local :: _ when local.name = name -> Some local.slot
    | _ :: rest -> search rest
  in
  search s.locals

let rec remove_first_occurrence name = function
  | [] -> []
  | x :: rest when x = name -> rest
  | x :: rest -> x :: remove_first_occurrence name rest

let rec is_mutable_local s name =
  if List.mem name s.mutable_locals then true
  else match s.enclosing with
    | Some enc -> is_mutable_local enc name
    | None -> false

let is_mutable_global s name =
  Hashtbl.mem s.mutable_globals name

(* ---- Upvalue resolution ---- *)

let add_upvalue s capture name =
  (* Check if already captured *)
  let rec find i = function
    | [] ->
      s.upvalues <- s.upvalues @ [{ capture; uv_name = name }];
      List.length s.upvalues - 1
    | uv :: _ when uv.uv_name = name && uv.capture = capture -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 s.upvalues

let rec resolve_upvalue s name =
  match s.enclosing with
  | None -> None
  | Some enc ->
    match find_local enc name with
    | Some slot ->
      let idx = add_upvalue s (Bytecode.CaptureLocal slot) name in
      Some idx
    | None ->
      match resolve_upvalue enc name with
      | Some enc_uv_idx ->
        let idx = add_upvalue s (Bytecode.CaptureUpvalue enc_uv_idx) name in
        Some idx
      | None -> None

(* ---- Global variable management ---- *)

let find_global s name =
  let len = Dynarray.length s.global_names in
  let rec find i =
    if i >= len then None
    else if Dynarray.get s.global_names i = name then Some i
    else find (i + 1)
  in
  find 0

let find_or_add_global s name =
  match find_global s name with
  | Some i -> i
  | None ->
    let len = Dynarray.length s.global_names in
    Dynarray.add_last s.global_names name;
    len

(* ---- Tag management for variants ---- *)

let tag_for_constructor type_env name =
  match List.assoc_opt name type_env.Types.constructors with
  | None -> error (Printf.sprintf "unknown constructor: %s" name)
  | Some info ->
    let (_, _, variant_def, _) = List.find (fun (n, _, _, _) ->
      String.equal n info.ctor_type_name) type_env.Types.variants in
    (* Strip module prefix for matching: "M.Green" -> "Green" *)
    let short_name = match String.rindex_opt name '.' with
      | Some i -> String.sub name (i + 1) (String.length name - i - 1)
      | None -> name
    in
    let rec find_tag i = function
      | [] -> error (Printf.sprintf "constructor %s not found in type" name)
      | (cname, _) :: _ when cname = short_name -> i
      | _ :: rest -> find_tag (i + 1) rest
    in
    find_tag 0 variant_def

(* ---- Type class method resolution ---- *)

let extract_class_ty_args num_params schema_ty resolved_ty =
  let found = Hashtbl.create num_params in
  let rec go schema_ty resolved_ty =
    let resolved_ty = Types.repr resolved_ty in
    match schema_ty, resolved_ty with
    | Types.TGen i, ty when i < num_params ->
      if not (Hashtbl.mem found i) then
        Hashtbl.replace found i ty
    | Types.TArrow (s_a, _, s_r), Types.TArrow (r_a, _, r_r)
    | Types.TCont (s_a, _, s_r), Types.TCont (r_a, _, r_r) ->
      go s_a r_a; go s_r r_r
    | Types.TTuple ss, Types.TTuple rs when List.length ss = List.length rs ->
      List.iter2 go ss rs
    | Types.TList s, Types.TList r -> go s r
    | _ -> ()
  in
  go schema_ty resolved_ty;
  (* Return partial results — None for params not found in this method's type *)
  List.init num_params (fun i -> Hashtbl.find_opt found i)

(* ---- Builtin call helpers ---- *)

let emit_builtin_call1 s name =
  let gidx = find_or_add_global s name in
  emit s (Bytecode.GET_GLOBAL gidx);
  (* arg already on stack *)
  emit s (Bytecode.CALL 1)

let emit_builtin_call2 s name =
  (* Stack has [... arg1 arg2]. We need: push fn, push arg1, call, push arg2, call.
     But arg1 and arg2 are already on the stack in order. So we need to get the fn
     BEFORE pushing args. This helper assumes caller manages arg pushing. *)
  let gidx = find_or_add_global s name in
  emit s (Bytecode.GET_GLOBAL gidx)
  (* Caller must then push arg1, CALL 1, push arg2, CALL 1 *)

(* ---- Compilation ---- *)

let rec compile_expr tail s (te : Typechecker.texpr) =
  if te.loc.line > 0 then s.current_line <- te.loc.line;
  match te.expr with
  | Typechecker.TEInt n ->
    emit_constant s (Bytecode.VInt n)
  | Typechecker.TEFloat f ->
    emit_constant s (Bytecode.VFloat f)
  | Typechecker.TEBool b ->
    emit_constant s (Bytecode.VBool b)
  | Typechecker.TEString str ->
    emit_constant s (Bytecode.VString str)
  | Typechecker.TEByte n ->
    emit_constant s (Bytecode.VByte n)
  | Typechecker.TERune n ->
    emit_constant s (Bytecode.VRune n)
  | Typechecker.TEUnit ->
    emit_constant s (Bytecode.VUnit)
  | Typechecker.TENil ->
    emit s Bytecode.NIL
  | Typechecker.TEVar name ->
    (* Check if this is a class method that needs dictionary dispatch *)
    if find_local s name = None && resolve_upvalue s name = None && find_global s name = None then
      (* Try full name first; only strip module prefix if the method's class
         has a matching module prefix (prevents Set.of_list matching Map.of_list) *)
      let (class_opt, method_name) =
        match Types.find_method_class s.type_env.Types.classes name with
        | Some _ as result -> (result, name)
        | None ->
          match String.rindex_opt name '.' with
          | None -> (None, name)
          | Some i ->
            let mod_prefix = String.sub name 0 (i + 1) in
            let short = String.sub name (i + 1) (String.length name - i - 1) in
            let class_prefix = String.sub name 0 i in
            (match Types.find_method_class s.type_env.Types.classes short with
             | Some class_def when String.equal class_def.Types.class_name class_prefix ->
               (* Exact match: ClassName.method for global classes *)
               (Some class_def, short)
             | Some class_def when String.length class_def.Types.class_name > String.length mod_prefix
                 && String.sub class_def.Types.class_name 0 (String.length mod_prefix) = mod_prefix ->
               (* Module-prefixed match: Module.method for module-scoped classes *)
               (Some class_def, short)
             | _ -> (None, name))
      in
      match class_opt with
      | Some class_def ->
        let method_schema_ty = List.assoc method_name class_def.Types.class_methods in
        let num_params = List.length class_def.Types.class_params in
        let partial = extract_class_ty_args num_params method_schema_ty te.ty in
        (* Find instances matching all extracted type params *)
        (* Convert partial: unresolved tvars become None for matching *)
        let concrete_partial = List.map (fun opt ->
          match opt with
          | Some ty ->
            (match Types.repr ty with
             | Types.TVar { contents = Types.Unbound _ } -> None
             | _ -> Some (Types.repr ty))
          | None -> None
        ) partial in
        (* Use fundeps to improve partial type args *)
        let concrete_partial =
          if class_def.Types.class_fundeps <> [] then
            Types.improve_with_fundeps s.type_env.Types.instances class_def concrete_partial
          else concrete_partial
        in
        (* Find instances matching all concretely-resolved type params *)
        let matching = List.filter (fun (inst : Types.instance_def) ->
          String.equal inst.Types.inst_class class_def.Types.class_name &&
          List.length inst.inst_tys = num_params &&
          Types.match_partial_inst inst.inst_tys concrete_partial
        ) s.type_env.Types.instances in
        (match matching with
         | [inst] ->
           if inst.Types.inst_constraints = [] then begin
             let method_gidx = find_or_add_global s (inst.Types.inst_dict_name ^ "$" ^ method_name) in
             emit s (Bytecode.GET_GLOBAL method_gidx)
           end else begin
             let dict_gidx = find_or_add_global s inst.Types.inst_dict_name in
             emit s (Bytecode.GET_GLOBAL dict_gidx);
             emit s (Bytecode.FIELD method_name)
           end
         | [] ->
           (* For poly variant types, fall back to structural builtins *)
           let is_structural = List.exists (function
             | Some ty -> (match Types.repr ty with
               | Types.TPolyVariant _ | Types.TRecord _ -> true
               | _ -> false)
             | None -> false
           ) partial in
           if is_structural && String.equal class_def.class_name "Show" && String.equal method_name "show" then begin
             let gidx = find_or_add_global s "__show_value" in
             emit s (Bytecode.GET_GLOBAL gidx)
           end else begin
             let ty_strs = List.map (function
               | Some ty -> Types.pp_ty ty
               | None -> "_"
             ) partial in
             error (Printf.sprintf "no instance of %s for types %s"
               class_def.class_name (String.concat ", " ty_strs))
           end
         | _ ->
           (* Multiple matches: try specificity-based selection *)
           (match Types.most_specific_inst matching with
            | Some inst ->
              if inst.Types.inst_constraints = [] then begin
                let method_gidx = find_or_add_global s (inst.Types.inst_dict_name ^ "$" ^ method_name) in
                emit s (Bytecode.GET_GLOBAL method_gidx)
              end else begin
                let dict_gidx = find_or_add_global s inst.Types.inst_dict_name in
                emit s (Bytecode.GET_GLOBAL dict_gidx);
                emit s (Bytecode.FIELD method_name)
              end
            | None ->
                 error (Printf.sprintf "ambiguous instance for %s method %s"
                   class_def.class_name method_name)))
      | None ->
        compile_var_access s name
    else
      compile_var_access s name
  | Typechecker.TEBinop (op, e1, e2) ->
    compile_binop s op e1 e2
  | Typechecker.TEUnop (op, e) ->
    compile_unop s op e
  | Typechecker.TEApp (fn, arg) ->
    compile_expr false s fn;
    compile_expr false s arg;
    if tail then
      emit s (Bytecode.TAIL_CALL 1)
    else
      emit s (Bytecode.CALL 1)
  | Typechecker.TEFun (param, body, has_return) ->
    compile_function has_return s param body te.ty
  | Typechecker.TELet (name, _scheme, e1, e2) ->
    compile_expr false s e1;
    let was_mutable = List.mem name s.mutable_locals in
    if was_mutable then
      s.mutable_locals <- List.filter (fun n -> n <> name) s.mutable_locals;
    let slot = allocate_local s name in
    emit s (Bytecode.SET_LOCAL slot);
    compile_expr tail s e2;
    if was_mutable then
      s.mutable_locals <- name :: s.mutable_locals;
    free_local s
  | Typechecker.TELetMut (name, e1, e2) ->
    compile_expr false s e1;
    emit s Bytecode.MAKE_REF;
    let slot = allocate_local s name in
    s.mutable_locals <- name :: s.mutable_locals;
    emit s (Bytecode.SET_LOCAL slot);
    compile_expr tail s e2;
    s.mutable_locals <- remove_first_occurrence name s.mutable_locals;
    free_local s
  | Typechecker.TEWhile (cond, body) ->
    let enter_idx = emit_idx s (Bytecode.ENTER_LOOP 0) in
    let prev_loop_start = s.loop_start in
    let loop_start = current_offset s in
    s.loop_start <- Some loop_start;
    compile_expr false s cond;
    let exit_jump = emit_idx s (Bytecode.JUMP_IF_FALSE 0) in
    compile_expr false s body;
    emit s Bytecode.POP;
    emit s (Bytecode.JUMP loop_start);
    let exit_normal = current_offset s in
    patch s exit_jump (Bytecode.JUMP_IF_FALSE exit_normal);
    emit s Bytecode.EXIT_LOOP;
    let unit_idx = add_constant s Bytecode.VUnit in
    emit s (Bytecode.CONST unit_idx);
    let end_jump = emit_idx s (Bytecode.JUMP 0) in
    let break_target = current_offset s in
    (* break value already on stack from LOOP_BREAK *)
    let end_target = current_offset s in
    patch s enter_idx (Bytecode.ENTER_LOOP break_target);
    patch s end_jump (Bytecode.JUMP end_target);
    s.loop_start <- prev_loop_start
  | Typechecker.TEAssign (name, e) ->
    (match find_local s name with
     | Some slot ->
       (* stack: [new_value ref] -> SET_REF *)
       compile_expr false s e;
       emit s (Bytecode.GET_LOCAL slot);
       emit s Bytecode.SET_REF
     | None ->
       match resolve_upvalue s name with
       | Some idx ->
         compile_expr false s e;
         emit s (Bytecode.GET_UPVALUE idx);
         emit s Bytecode.SET_REF
       | None ->
         compile_expr false s e;
         let gidx = find_or_add_global s name in
         emit s (Bytecode.GET_GLOBAL gidx);
         emit s Bytecode.SET_REF);
    emit_constant s Bytecode.VUnit
  | Typechecker.TEFieldAssign (record_e, field, value_e) ->
    compile_expr false s record_e;
    compile_expr false s value_e;
    emit s (Bytecode.SET_FIELD field);
    emit_constant s Bytecode.VUnit
  | Typechecker.TELetRec (name, _scheme, e1, e2) ->
    compile_let_rec s name e1;
    compile_expr tail s e2;
    free_local s
  | Typechecker.TEIf (cond, then_e, else_e) ->
    compile_if tail s cond then_e else_e
  | Typechecker.TETuple exprs ->
    List.iter (compile_expr false s) exprs;
    emit s (Bytecode.MAKE_TUPLE (List.length exprs))
  | Typechecker.TERecord fields ->
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    let names = List.map fst sorted in
    List.iter (fun (_, e) -> compile_expr false s e) sorted;
    emit s (Bytecode.MAKE_RECORD names)
  | Typechecker.TERecordUpdate (base, overrides) ->
    compile_expr false s base;
    List.iter (fun (_, e) -> compile_expr false s e) overrides;
    let field_names = List.map fst overrides in
    emit s (Bytecode.RECORD_UPDATE field_names)
  | Typechecker.TERecordUpdateIdx (base, pairs) ->
    compile_expr false s base;
    List.iter (fun (idx_e, val_e) ->
      compile_expr false s idx_e;
      compile_expr false s val_e
    ) pairs;
    emit s (Bytecode.RECORD_UPDATE_DYN (List.length pairs))
  | Typechecker.TEField (e, field) ->
    compile_expr false s e;
    emit s (Bytecode.FIELD field)
  | Typechecker.TEIndex (base, idx) ->
    compile_expr false s base;
    compile_expr false s idx;
    emit s Bytecode.INDEX
  | Typechecker.TECons (hd, tl) ->
    compile_expr false s hd;
    compile_expr false s tl;
    emit s Bytecode.CONS
  | Typechecker.TEConstruct (name, arg) ->
    let tag = if String.length name > 0 && name.[0] = '`' then
      Hashtbl.hash (String.sub name 1 (String.length name - 1)) land 0x3FFFFFFF
    else
      tag_for_constructor s.type_env name
    in
    (* Use short name (without module prefix) for display in MAKE_VARIANT *)
    let short_name = match String.rindex_opt name '.' with
      | Some i -> String.sub name (i + 1) (String.length name - i - 1)
      | None -> name
    in
    (match arg with
     | Some e ->
       compile_expr false s e;
       emit s (Bytecode.MAKE_VARIANT (tag, short_name, true))
     | None ->
       emit s (Bytecode.MAKE_VARIANT (tag, short_name, false)))
  | Typechecker.TEMatch (scrut, arms, _partial) ->
    compile_match tail s scrut arms
  | Typechecker.TESeq (e1, e2) ->
    compile_expr false s e1;
    emit s Bytecode.POP;
    compile_expr tail s e2
  | Typechecker.TEPerform (op_name, arg_te) ->
    compile_expr false s arg_te;
    emit s (Bytecode.PERFORM op_name)
  | Typechecker.TEHandle (body_te, arms) ->
    compile_handle s body_te arms
  | Typechecker.TEResume (k_te, v_te) ->
    compile_expr false s k_te;
    compile_expr false s v_te;
    emit s Bytecode.RESUME
  | Typechecker.TEBreak value_te ->
    compile_expr false s value_te;
    emit s Bytecode.LOOP_BREAK
  | Typechecker.TEContinueLoop ->
    (match s.loop_start with
     | Some target -> emit s (Bytecode.LOOP_CONTINUE target)
     | None -> error "continue outside of loop")
  | Typechecker.TEFoldContinue value_te ->
    let depth_before = s.operand_depth in
    compile_expr false s value_te;
    let n_pops = depth_before in
    emit s (Bytecode.FOLD_CONTINUE n_pops)
  | Typechecker.TEForLoop fold_te ->
    let enter_idx = emit_idx s (Bytecode.ENTER_LOOP 0) in
    let prev_loop_start = s.loop_start in
    s.loop_start <- None;
    compile_expr false s fold_te;
    emit s Bytecode.EXIT_LOOP;
    let end_jump = emit_idx s (Bytecode.JUMP 0) in
    let break_target = current_offset s in
    (* break value already on stack from LOOP_BREAK *)
    let end_target = current_offset s in
    patch s enter_idx (Bytecode.ENTER_LOOP break_target);
    patch s end_jump (Bytecode.JUMP end_target);
    s.loop_start <- prev_loop_start
  | Typechecker.TEMap pairs ->
    List.iter (fun (k, v) ->
      compile_expr false s k;
      compile_expr false s v
    ) pairs;
    emit s (Bytecode.MAKE_MAP (List.length pairs))
  | Typechecker.TEArray elems ->
    List.iter (compile_expr false s) elems;
    emit s (Bytecode.MAKE_ARRAY (List.length elems))
  | Typechecker.TEReturn value_te ->
    compile_expr false s value_te;
    emit s Bytecode.FUNC_RETURN
  | Typechecker.TELetRecAnd (bindings, body) ->
    (* Use mutable ref cells so closures capture by reference.
       This allows mutual references: when a closure captures another
       function's local, it captures the ref cell. After all closures
       are compiled and stored in the ref cells, all cross-references
       are correct. *)
    let slots = List.map (fun (name, _) ->
      let unit_idx = add_constant s Bytecode.VUnit in
      emit s (Bytecode.CONST unit_idx);
      emit s Bytecode.MAKE_REF;
      let slot = allocate_local s name in
      s.mutable_locals <- name :: s.mutable_locals;
      emit s (Bytecode.SET_LOCAL slot);
      slot
    ) bindings in
    (* Compile each closure and store in ref cell *)
    List.iter2 (fun (_name, fn_te) slot ->
      compile_expr false s fn_te;
      emit s (Bytecode.GET_LOCAL slot);
      emit s Bytecode.SET_REF
    ) bindings slots;
    (* Compile the body (DEREF happens automatically via is_mutable_local) *)
    compile_expr tail s body;
    (* Clean up *)
    List.iter (fun (name, _) ->
      s.mutable_locals <- List.filter (fun n -> n <> name) s.mutable_locals
    ) bindings;
    List.iter (fun _ -> free_local s) bindings

and compile_var_access s name =
  (match find_local s name with
  | Some slot ->
    emit s (Bytecode.GET_LOCAL slot);
    if is_mutable_local s name then emit s Bytecode.DEREF
  | None ->
    match resolve_upvalue s name with
    | Some idx ->
      emit s (Bytecode.GET_UPVALUE idx);
      if is_mutable_local s name then emit s Bytecode.DEREF
    | None ->
      let gidx = find_or_add_global s name in
      emit s (Bytecode.GET_GLOBAL gidx);
      if is_mutable_global s name then emit s Bytecode.DEREF)

and compile_binop s op e1 e2 =
  match op with
  | Ast.And ->
    compile_expr false s e1;
    emit s Bytecode.DUP;
    let jump_idx = emit_idx s (Bytecode.JUMP_IF_FALSE 0) in
    emit s Bytecode.POP;
    compile_expr false s e2;
    let target = current_offset s in
    patch s jump_idx (Bytecode.JUMP_IF_FALSE target)
  | Ast.Or ->
    compile_expr false s e1;
    emit s Bytecode.DUP;
    let jump_idx = emit_idx s (Bytecode.JUMP_IF_TRUE 0) in
    emit s Bytecode.POP;
    compile_expr false s e2;
    let target = current_offset s in
    patch s jump_idx (Bytecode.JUMP_IF_TRUE target)
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
    let resolved = Types.repr e1.ty in
    (match resolved with
     | Types.TInt ->
       compile_expr false s e1; compile_expr false s e2;
       emit s (match op with
         | Ast.Add -> Bytecode.ADD | Ast.Sub -> Bytecode.SUB
         | Ast.Mul -> Bytecode.MUL | Ast.Div -> Bytecode.DIV
         | _ -> assert false)
     | Types.TFloat ->
       compile_expr false s e1; compile_expr false s e2;
       emit s (match op with
         | Ast.Add -> Bytecode.FADD | Ast.Sub -> Bytecode.FSUB
         | Ast.Mul -> Bytecode.FMUL | Ast.Div -> Bytecode.FDIV
         | _ -> assert false)
     | _ ->
       let method_name = match op with
         | Ast.Add -> "+" | Ast.Sub -> "-" | Ast.Mul -> "*" | Ast.Div -> "/"
         | _ -> assert false in
       compile_class_binop s "Num" method_name e1.ty e1 e2)
  | Ast.Mod ->
    compile_expr false s e1; compile_expr false s e2;
    emit s Bytecode.MOD
  | Ast.Eq | Ast.Neq ->
    let resolved = Types.repr e1.ty in
    let has_custom_instance class_name ty =
      List.exists (fun (inst : Types.instance_def) ->
        String.equal inst.inst_class class_name &&
        List.length inst.inst_tys = 1 &&
        Types.match_partial_inst inst.inst_tys [Some ty]
      ) s.type_env.Types.instances
    in
    let use_structural = match resolved with
      | Types.TInt | Types.TFloat | Types.TBool | Types.TString
      | Types.TByte | Types.TRune
      | Types.TUnit | Types.TList _ | Types.TTuple _ | Types.TVariant _
      | Types.TPolyVariant _ | Types.TMap _ | Types.TArray _
      | Types.TVar { contents = Types.Unbound _ } -> true
      | Types.TRecord _ -> not (has_custom_instance "Eq" resolved)
      | _ -> false in
    if use_structural then begin
      compile_expr false s e1; compile_expr false s e2;
      emit s (if op = Ast.Eq then Bytecode.EQ else Bytecode.NEQ)
    end else begin
      let method_name = if op = Ast.Eq then "=" else "<>" in
      compile_class_binop s "Eq" method_name e1.ty e1 e2
    end
  | Ast.Lt | Ast.Gt | Ast.Le | Ast.Ge ->
    let resolved = Types.repr e1.ty in
    let is_builtin = match resolved with
      | Types.TInt | Types.TFloat | Types.TString | Types.TByte | Types.TRune
      | Types.TVar { contents = Types.Unbound _ } -> true
      | _ -> false in
    if is_builtin then begin
      compile_expr false s e1; compile_expr false s e2;
      emit s (match op with
        | Ast.Lt -> Bytecode.LT | Ast.Gt -> Bytecode.GT
        | Ast.Le -> Bytecode.LE | Ast.Ge -> Bytecode.GE
        | _ -> assert false)
    end else begin
      let method_name = match op with
        | Ast.Lt -> "<" | Ast.Gt -> ">" | Ast.Le -> "<=" | Ast.Ge -> ">="
        | _ -> assert false in
      compile_class_binop s "Ord" method_name e1.ty e1 e2
    end
  | Ast.Concat ->
    let gidx = find_or_add_global s "^" in
    emit s (Bytecode.GET_GLOBAL gidx);
    compile_expr false s e1;
    emit s (Bytecode.CALL 1);
    compile_expr false s e2;
    emit s (Bytecode.CALL 1)
  | Ast.Land | Ast.Lor | Ast.Lxor | Ast.Lsl | Ast.Lsr ->
    let resolved = Types.repr e1.ty in
    (match resolved with
     | Types.TInt ->
       compile_expr false s e1; compile_expr false s e2;
       emit s (match op with
         | Ast.Land -> Bytecode.BAND | Ast.Lor -> Bytecode.BOR
         | Ast.Lxor -> Bytecode.BXOR | Ast.Lsl -> Bytecode.BSHL
         | Ast.Lsr -> Bytecode.BSHR | _ -> assert false)
     | _ ->
       let method_name = match op with
         | Ast.Land -> "land" | Ast.Lor -> "lor" | Ast.Lxor -> "lxor"
         | Ast.Lsl -> "lsl" | Ast.Lsr -> "lsr" | _ -> assert false in
       compile_class_binop s "Bitwise" method_name e1.ty e1 e2)
  | Ast.Pipe ->
    compile_expr false s e2;
    compile_expr false s e1;
    emit s (Bytecode.CALL 1)

and compile_class_binop (s : state) class_name method_name operand_ty e1 e2 =
  let conc_ty = Types.repr operand_ty in
  let class_def = match List.find_opt (fun (c : Types.class_def) ->
    String.equal c.class_name class_name) s.type_env.Types.classes with
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
  ) s.type_env.Types.instances in
  (match matching with
   | [inst] ->
     if inst.Types.inst_constraints = [] then begin
       let method_gidx = find_or_add_global s (inst.Types.inst_dict_name ^ "$" ^ method_name) in
       emit s (Bytecode.GET_GLOBAL method_gidx)
     end else begin
       let dict_gidx = find_or_add_global s inst.Types.inst_dict_name in
       emit s (Bytecode.GET_GLOBAL dict_gidx);
       emit s (Bytecode.FIELD method_name)
     end;
     compile_expr false s e1;
     emit s (Bytecode.CALL 1);
     compile_expr false s e2;
     emit s (Bytecode.CALL 1)
   | [] ->
     error (Printf.sprintf "no instance of %s for type %s"
       class_name (Types.pp_ty conc_ty))
   | _ ->
     error (Printf.sprintf "ambiguous instance for %s method %s"
       class_name method_name))

and compile_class_unop (s : state) class_name method_name operand_ty e =
  let conc_ty = Types.repr operand_ty in
  let class_def = match List.find_opt (fun (c : Types.class_def) ->
    String.equal c.class_name class_name) s.type_env.Types.classes with
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
  ) s.type_env.Types.instances in
  (match matching with
   | [inst] ->
     if inst.Types.inst_constraints = [] then begin
       let method_gidx = find_or_add_global s (inst.Types.inst_dict_name ^ "$" ^ method_name) in
       emit s (Bytecode.GET_GLOBAL method_gidx)
     end else begin
       let dict_gidx = find_or_add_global s inst.Types.inst_dict_name in
       emit s (Bytecode.GET_GLOBAL dict_gidx);
       emit s (Bytecode.FIELD method_name)
     end;
     compile_expr false s e;
     emit s (Bytecode.CALL 1)
   | [] ->
     error (Printf.sprintf "no instance of %s for type %s"
       class_name (Types.pp_ty conc_ty))
   | _ ->
     error (Printf.sprintf "ambiguous instance for %s method %s"
       class_name method_name))

and compile_unop s op (e : Typechecker.texpr) =
  match op with
  | Ast.Neg ->
    let resolved = Types.repr e.ty in
    (match resolved with
     | Types.TInt ->
       compile_expr false s e; emit s Bytecode.NEG
     | Types.TFloat ->
       compile_expr false s e; emit s Bytecode.FNEG
     | _ ->
       compile_class_unop s "Num" "neg" e.ty e)
  | Ast.Not -> compile_expr false s e; emit s Bytecode.NOT
  | Ast.Lnot ->
    let resolved = Types.repr e.ty in
    (match resolved with
     | Types.TInt ->
       compile_expr false s e; emit s Bytecode.BNOT
     | _ ->
       compile_class_unop s "Bitwise" "lnot" e.ty e)

and compile_if tail s cond then_e else_e =
  compile_expr false s cond;
  let else_jump = emit_idx s (Bytecode.JUMP_IF_FALSE 0) in
  compile_expr tail s then_e;
  let end_jump = emit_idx s (Bytecode.JUMP 0) in
  let else_target = current_offset s in
  patch s else_jump (Bytecode.JUMP_IF_FALSE else_target);
  compile_expr tail s else_e;
  let end_target = current_offset s in
  patch s end_jump (Bytecode.JUMP end_target)

and compile_function has_return s param_name body fn_ty =
  let param_ty_arity = count_arrows fn_ty in
  let _ = param_ty_arity in
  let sub = create_state
    (Some s) 1 s.global_names s.mutable_globals s.type_env param_name
  in
  (* Parameter is local 0 *)
  ignore (allocate_local sub param_name);
  if has_return then emit sub Bytecode.ENTER_FUNC;
  compile_expr (not has_return) sub body;
  if has_return then emit sub Bytecode.EXIT_FUNC;
  emit sub Bytecode.RETURN;
  let proto = finalize_proto sub in
  let proto_idx = add_constant s (Bytecode.VProto proto) in
  let captures = List.map (fun uv -> uv.capture) sub.upvalues in
  emit s (Bytecode.CLOSURE (proto_idx, captures))

and count_arrows ty = match Types.repr ty with
  | Types.TArrow (_, _, r) | Types.TCont (_, _, r) -> 1 + count_arrows r
  | _ -> 0

and finalize_proto s =
  Bytecode.{
    name = s.proto_name;
    arity = s.arity;
    num_locals = s.max_locals;
    code = Dynarray.to_array s.code;
    constants = Dynarray.to_array s.constants;
    line_table = Dynarray.to_array s.lines;
  }

and compile_let_rec s name fn_expr =
  (* Allocate slot for the recursive binding first *)
  let slot = allocate_local s name in
  (* Compile the function *)
  match fn_expr.Typechecker.expr with
  | Typechecker.TEFun (param, body, has_return) ->
    let sub = create_state
      (Some s) 1 s.global_names s.mutable_globals s.type_env name
    in
    ignore (allocate_local sub param);
    if has_return then emit sub Bytecode.ENTER_FUNC;
    compile_expr (not has_return) sub body;
    if has_return then emit sub Bytecode.EXIT_FUNC;
    emit sub Bytecode.RETURN;
    let proto = finalize_proto sub in
    let proto_idx = add_constant s (Bytecode.VProto proto) in
    let captures = List.map (fun uv -> uv.capture) sub.upvalues in
    (* Find which upvalue index is the self-reference *)
    let self_idx = List.find_map (fun (i, uv) ->
      if uv.uv_name = name then Some i else None
    ) (List.mapi (fun i uv -> (i, uv)) sub.upvalues) in
    (match self_idx with
     | Some si ->
       emit s (Bytecode.CLOSURE_REC (proto_idx, captures, si))
     | None ->
       (* Function doesn't actually reference itself *)
       emit s (Bytecode.CLOSURE (proto_idx, captures)));
    emit s (Bytecode.SET_LOCAL slot)
  | _ ->
    error "let rec must bind a function"

(* Single-pass pattern compilation: tests and binds simultaneously.
   Returns slot for each PatVar binding. Jumps to fail_jumps on mismatch. *)
and compile_match tail s scrut arms =
  compile_expr false s scrut;
  let scrut_slot = allocate_local s "_match_scrut" in
  emit s (Bytecode.SET_LOCAL scrut_slot);
  let end_jumps = ref [] in
  List.iter (fun (pat, guard, body) ->
    let saved_num_locals = s.num_locals in
    let saved_locals = s.locals in
    let fail_jumps = ref [] in
    compile_pattern s scrut_slot pat fail_jumps;
    (* Compile guard if present — guard failure acts like pattern failure *)
    (match guard with
     | Some guard_te ->
       compile_expr false s guard_te;
       fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps
     | None -> ());
    compile_expr tail s body;
    (* Restore locals to state before pattern bindings *)
    s.num_locals <- saved_num_locals;
    s.locals <- saved_locals;
    end_jumps := emit_idx s (Bytecode.JUMP 0) :: !end_jumps;
    (* Patch all fail jumps for this arm *)
    let target = current_offset s in
    List.iter (fun idx ->
      patch s idx (Bytecode.JUMP_IF_FALSE target)
    ) !fail_jumps
  ) arms;
  let loc_str = Printf.sprintf "line %d" scrut.loc.line in
  emit s (Bytecode.MATCH_FAIL loc_str);
  let end_target = current_offset s in
  List.iter (fun idx ->
    patch s idx (Bytecode.JUMP end_target)
  ) !end_jumps;
  free_local s (* free scrut_slot *)

(* Compile a pattern: test + bind in one pass.
   slot = local slot containing the value to match against.
   fail_jumps = accumulator for JUMP_IF_FALSE instructions to patch on failure. *)
and compile_pattern s slot pat fail_jumps =
  match pat with
  | Ast.PatWild -> ()
  | Ast.PatVar name ->
    let var_slot = allocate_local s name in
    emit s (Bytecode.GET_LOCAL slot);
    emit s (Bytecode.SET_LOCAL var_slot)
  | Ast.PatInt n ->
    emit s (Bytecode.GET_LOCAL slot);
    emit_constant s (Bytecode.VInt n);
    emit s Bytecode.EQ;
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps
  | Ast.PatFloat f ->
    emit s (Bytecode.GET_LOCAL slot);
    emit_constant s (Bytecode.VFloat f);
    emit s Bytecode.EQ;
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps
  | Ast.PatBool b ->
    emit s (Bytecode.GET_LOCAL slot);
    emit_constant s (Bytecode.VBool b);
    emit s Bytecode.EQ;
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps
  | Ast.PatString str ->
    emit s (Bytecode.GET_LOCAL slot);
    emit_constant s (Bytecode.VString str);
    emit s Bytecode.EQ;
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps
  | Ast.PatUnit -> ()
  | Ast.PatNil ->
    emit s (Bytecode.GET_LOCAL slot);
    emit s Bytecode.IS_NIL;
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps
  | Ast.PatCons (hd_pat, tl_pat) ->
    emit s (Bytecode.GET_LOCAL slot);
    emit s Bytecode.IS_CONS;
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps;
    let hd_slot = allocate_local s "_hd" in
    emit s (Bytecode.GET_LOCAL slot);
    emit s Bytecode.HEAD;
    emit s (Bytecode.SET_LOCAL hd_slot);
    let tl_slot = allocate_local s "_tl" in
    emit s (Bytecode.GET_LOCAL slot);
    emit s Bytecode.TAIL;
    emit s (Bytecode.SET_LOCAL tl_slot);
    compile_pattern s hd_slot hd_pat fail_jumps;
    compile_pattern s tl_slot tl_pat fail_jumps
  | Ast.PatConstruct (name, arg_pat) ->
    let tag = tag_for_constructor s.type_env name in
    emit s (Bytecode.GET_LOCAL slot);
    emit s (Bytecode.TAG_EQ tag);
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps;
    (match arg_pat with
     | Some sub_pat ->
       let payload_slot = allocate_local s "_payload" in
       emit s (Bytecode.GET_LOCAL slot);
       emit s Bytecode.VARIANT_PAYLOAD;
       emit s (Bytecode.SET_LOCAL payload_slot);
       compile_pattern s payload_slot sub_pat fail_jumps
     | None -> ())
  | Ast.PatTuple pats ->
    List.iteri (fun i sub_pat ->
      let elem_slot = allocate_local s "_tup" in
      emit s (Bytecode.GET_LOCAL slot);
      emit s (Bytecode.TUPLE_GET i);
      emit s (Bytecode.SET_LOCAL elem_slot);
      compile_pattern s elem_slot sub_pat fail_jumps
    ) pats
  | Ast.PatRecord field_pats ->
    List.iter (fun (fname, sub_pat) ->
      let field_slot = allocate_local s "_fld" in
      emit s (Bytecode.GET_LOCAL slot);
      emit s (Bytecode.FIELD fname);
      emit s (Bytecode.SET_LOCAL field_slot);
      compile_pattern s field_slot sub_pat fail_jumps
    ) field_pats
  | Ast.PatAs (inner_pat, name) ->
    compile_pattern s slot inner_pat fail_jumps;
    let var_slot = allocate_local s name in
    emit s (Bytecode.GET_LOCAL slot);
    emit s (Bytecode.SET_LOCAL var_slot)
  | Ast.PatOr (p1, p2) ->
    let saved_locals = s.locals in
    let saved_num_locals = s.num_locals in
    let or_fail_jumps = ref [] in
    compile_pattern s slot p1 or_fail_jumps;
    (* Record p1's named variable->slot bindings *)
    let p1_bindings = List.filter_map (fun (l : local) ->
      if l.slot >= saved_num_locals && l.name <> "" && l.name.[0] <> '_' then
        Some (l.name, l.slot)
      else None
    ) s.locals in
    let success_jump = emit_idx s (Bytecode.JUMP 0) in
    (* Patch p1 failures to try p2 *)
    let p2_target = current_offset s in
    List.iter (fun idx ->
      patch s idx (Bytecode.JUMP_IF_FALSE p2_target)
    ) !or_fail_jumps;
    (* Restore locals so p2 allocates fresh slots *)
    s.locals <- saved_locals;
    s.num_locals <- saved_num_locals;
    compile_pattern s slot p2 fail_jumps;
    (* After p2, remap any variables that ended up in different slots than p1.
       Use temporary slots to avoid clobbering when source and dest slots overlap. *)
    let remaps = List.filter_map (fun (name, p1_slot) ->
      match List.find_opt (fun (l : local) -> l.name = name && l.slot >= saved_num_locals) s.locals with
      | Some l when l.slot <> p1_slot -> Some (l.slot, p1_slot)
      | _ -> None
    ) p1_bindings in
    if remaps <> [] then begin
      (* First pass: copy all source values to temporary slots *)
      let temps = List.map (fun (src, _dst) ->
        let tmp = allocate_local s "_or_tmp" in
        emit s (Bytecode.GET_LOCAL src);
        emit s (Bytecode.SET_LOCAL tmp);
        tmp
      ) remaps in
      (* Second pass: copy from temps to destination slots *)
      List.iter2 (fun tmp (_src, dst) ->
        emit s (Bytecode.GET_LOCAL tmp);
        emit s (Bytecode.SET_LOCAL dst)
      ) temps remaps
    end;
    (* Patch success jump *)
    let success_target = current_offset s in
    patch s success_jump (Bytecode.JUMP success_target);
    (* Use p1's bindings as the canonical local slots *)
    let p2_only_locals = List.filter (fun (l : local) ->
      l.slot < saved_num_locals || List.exists (fun (n, _) -> n = l.name) p1_bindings
    ) s.locals in
    let remapped = List.map (fun (l : local) ->
      match List.assoc_opt l.name p1_bindings with
      | Some p1_slot when l.slot >= saved_num_locals -> { l with slot = p1_slot }
      | _ -> l
    ) p2_only_locals in
    s.locals <- remapped
  | Ast.PatArray pats ->
    (* Check length matches *)
    let al_gidx = find_or_add_global s "array_length" in
    emit s (Bytecode.GET_GLOBAL al_gidx);
    emit s (Bytecode.GET_LOCAL slot);
    emit s (Bytecode.CALL 1);
    emit_constant s (Bytecode.VInt (List.length pats));
    emit s Bytecode.EQ;
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps;
    (* Extract each element by index *)
    List.iteri (fun i sub_pat ->
      let elem_slot = allocate_local s "_arr_elem" in
      emit s (Bytecode.GET_LOCAL slot);
      emit_constant s (Bytecode.VInt i);
      emit s Bytecode.INDEX;
      emit s (Bytecode.SET_LOCAL elem_slot);
      compile_pattern s elem_slot sub_pat fail_jumps
    ) pats
  | Ast.PatMap entries ->
    let has_gidx = find_or_add_global s "__map_has" in
    let get_gidx = find_or_add_global s "__map_get" in
    List.iter (fun (key_pat, val_pat) ->
      let key_val = match key_pat with
        | Ast.PatInt n -> Bytecode.VInt n
        | Ast.PatString s -> Bytecode.VString s
        | Ast.PatBool b -> Bytecode.VBool b
        | Ast.PatFloat f -> Bytecode.VFloat f
        | _ -> error "map pattern keys must be literals"
      in
      (* Check key exists: __map_has(map, key) *)
      emit s (Bytecode.GET_GLOBAL has_gidx);
      emit s (Bytecode.GET_LOCAL slot);
      emit s (Bytecode.CALL 1);
      emit_constant s key_val;
      emit s (Bytecode.CALL 1);
      fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps;
      (* Extract value: __map_get(map, key) *)
      let val_slot = allocate_local s "_map_val" in
      emit s (Bytecode.GET_GLOBAL get_gidx);
      emit s (Bytecode.GET_LOCAL slot);
      emit s (Bytecode.CALL 1);
      emit_constant s key_val;
      emit s (Bytecode.CALL 1);
      emit s (Bytecode.SET_LOCAL val_slot);
      compile_pattern s val_slot val_pat fail_jumps
    ) entries
  | Ast.PatPolyVariant (tag, arg_pat) ->
    let num_tag = Hashtbl.hash tag land 0x3FFFFFFF in
    emit s (Bytecode.GET_LOCAL slot);
    emit s (Bytecode.TAG_EQ num_tag);
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps;
    (match arg_pat with
     | Some sub_pat ->
       let payload_slot = allocate_local s "_payload" in
       emit s (Bytecode.GET_LOCAL slot);
       emit s Bytecode.VARIANT_PAYLOAD;
       emit s (Bytecode.SET_LOCAL payload_slot);
       compile_pattern s payload_slot sub_pat fail_jumps
     | None -> ())
  | Ast.PatPin name ->
    emit s (Bytecode.GET_LOCAL slot);
    compile_var_access s name;
    emit s Bytecode.EQ;
    fail_jumps := emit_idx s (Bytecode.JUMP_IF_FALSE 0) :: !fail_jumps
  | Ast.PatAnnot (inner_pat, _) ->
    compile_pattern s slot inner_pat fail_jumps

and compile_handle s body_te arms =
  let return_arm = ref None in
  let op_arms = ref [] in
  List.iter (function
    | Typechecker.THReturn (name, handler_body) ->
      return_arm := Some (name, handler_body)
    | Typechecker.THOp (op_name, arg_name, k_name, handler_body) ->
      op_arms := (op_name, arg_name, k_name, handler_body) :: !op_arms
  ) arms;
  let op_arms = List.rev !op_arms in
  let (ret_name, ret_body) = match !return_arm with
    | Some arm -> arm
    | None -> error "handle expression missing return arm"
  in
  (* 1. Compile body as thunk closure (fun _ -> body) *)
  compile_function false s "_" body_te (Types.TArrow (Types.TUnit, Types.EffEmpty, Types.TUnit));
  (* 2. Compile return handler as closure (fun ret_name -> ret_body) *)
  compile_function false s ret_name ret_body (Types.TArrow (Types.TUnit, Types.EffEmpty, Types.TUnit));
  (* 3. For each op: push op_name string, then handler closure *)
  List.iter (fun (op_name, arg_name, k_name, handler_body) ->
    emit_constant s (Bytecode.VString op_name);
    compile_op_handler s arg_name k_name handler_body
  ) op_arms;
  (* 4. Emit HANDLE with number of op handlers *)
  emit s (Bytecode.HANDLE (List.length op_arms))

and compile_op_handler s arg_name k_name handler_body =
  let sub = create_state
    (Some s) 1 s.global_names s.mutable_globals s.type_env "__handler"
  in
  (* Parameter (tuple) is local 0 *)
  let pair_slot = allocate_local sub "__pair" in
  (* Destructure: arg = tuple_get 0 pair, k = tuple_get 1 pair *)
  let arg_slot = allocate_local sub arg_name in
  emit sub (Bytecode.GET_LOCAL pair_slot);
  emit sub (Bytecode.TUPLE_GET 0);
  emit sub (Bytecode.SET_LOCAL arg_slot);
  let k_slot = allocate_local sub k_name in
  emit sub (Bytecode.GET_LOCAL pair_slot);
  emit sub (Bytecode.TUPLE_GET 1);
  emit sub (Bytecode.SET_LOCAL k_slot);
  ignore (arg_slot, k_slot);
  (* Compile handler body *)
  compile_expr true sub handler_body;
  emit sub Bytecode.RETURN;
  let proto = finalize_proto sub in
  let proto_idx = add_constant s (Bytecode.VProto proto) in
  let captures = List.map (fun uv -> uv.capture) sub.upvalues in
  emit s (Bytecode.CLOSURE (proto_idx, captures))

(* ---- Top-level compilation ---- *)

let rec compile_decl s (decl : Typechecker.tdecl) =
  match decl with
  | Typechecker.TDType _ | Typechecker.TDClass _ | Typechecker.TDEffect _ -> ()
  | Typechecker.TDExtern (name, _) ->
    ignore (find_or_add_global s name)
  | Typechecker.TDLet (name, te) ->
    compile_expr false s te;
    Hashtbl.remove s.mutable_globals name;
    let gidx = find_or_add_global s name in
    emit s (Bytecode.DEF_GLOBAL gidx);
    (* For typeclass dictionaries, also define individual method globals.
       Only for unconstrained instances (constrained instances produce factory functions). *)
    if String.length name > 6 && String.sub name 0 6 = "__dict" then begin
      let inst_opt = List.find_opt (fun (inst : Types.instance_def) ->
        String.equal inst.inst_dict_name name
      ) s.type_env.Types.instances in
      match inst_opt with
      | Some inst when inst.Types.inst_constraints = [] ->
        let class_opt = List.find_opt (fun (c : Types.class_def) ->
          String.equal c.class_name inst.Types.inst_class
        ) s.type_env.Types.classes in
        (match class_opt with
         | Some class_def ->
           List.iter (fun (method_name, _) ->
             let mgidx = find_or_add_global s (name ^ "$" ^ method_name) in
             emit s (Bytecode.GET_GLOBAL gidx);
             emit s (Bytecode.FIELD method_name);
             emit s (Bytecode.DEF_GLOBAL mgidx)
           ) class_def.Types.class_methods
         | None -> ())
      | _ -> ()
    end
  | Typechecker.TDLetMut (name, te) ->
    compile_expr false s te;
    emit s Bytecode.MAKE_REF;
    Hashtbl.replace s.mutable_globals name ();
    let gidx = find_or_add_global s name in
    emit s (Bytecode.DEF_GLOBAL gidx)
  | Typechecker.TDLetRec (name, te) ->
    Hashtbl.remove s.mutable_globals name;
    let gidx = find_or_add_global s name in
    (* For top-level rec, the function can reference itself via GET_GLOBAL *)
    compile_expr false s te;
    emit s (Bytecode.DEF_GLOBAL gidx)
  | Typechecker.TDLetRecAnd bindings ->
    List.iter (fun (name, _) -> Hashtbl.remove s.mutable_globals name) bindings;
    (* Register all global slots first *)
    let gidxs = List.map (fun (name, _) -> find_or_add_global s name) bindings in
    (* Compile each function — they can reference each other via GET_GLOBAL *)
    List.iter2 (fun (_name, te) gidx ->
      compile_expr false s te;
      emit s (Bytecode.DEF_GLOBAL gidx)
    ) bindings gidxs
  | Typechecker.TDExpr te ->
    compile_expr false s te;
    emit s Bytecode.POP
  | Typechecker.TDModule (_name, inner_decls) ->
    (* Compile each inner decl — qualified names are used directly in TEVar
       by the typechecker, so no short-name aliases are needed *)
    List.iter (fun d -> compile_decl s d) inner_decls
  | Typechecker.TDOpen alias_pairs ->
    (* For each (short_name, qualified_name), create a runtime alias *)
    List.iter (fun (short_name, qualified_name) ->
      let src_idx = find_or_add_global s qualified_name in
      let dst_idx = find_or_add_global s short_name in
      emit s (Bytecode.GET_GLOBAL src_idx);
      emit s (Bytecode.DEF_GLOBAL dst_idx);
      (* Propagate mutability tracking for opened names *)
      if is_mutable_global s qualified_name then
        Hashtbl.replace s.mutable_globals short_name ()
      else
        Hashtbl.remove s.mutable_globals short_name
    ) alias_pairs

let compile_decl_last s (decl : Typechecker.tdecl) =
  match decl with
  | Typechecker.TDExpr te ->
    compile_expr false s te
    (* Don't POP: leave value on stack for HALT to return *)
  | _ -> compile_decl s decl

let compile_program_with_globals type_env global_names mutable_globals (program : Typechecker.tprogram) : Bytecode.compiled_program =
  let s = create_state None 0 global_names mutable_globals type_env "<main>" in
  let rec compile_all = function
    | [] -> ()
    | [last] -> compile_decl_last s last
    | decl :: rest -> compile_decl s decl; compile_all rest
  in
  compile_all program;
  emit s Bytecode.HALT;
  let main = finalize_proto s in
  let main = if !optimize_enabled then Optimize.optimize_proto main else main in
  let gn = Dynarray.to_array global_names in
  Bytecode.{
    main;
    global_names = gn;
  }

let compile_program type_env (program : Typechecker.tprogram) : Bytecode.compiled_program =
  compile_program_with_globals type_env (Dynarray.create ()) ((Hashtbl.create 8 : (string, unit) Hashtbl.t)) program
