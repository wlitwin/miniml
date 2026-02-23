exception Type_error of string * Token.loc

type texpr = {
  expr: texpr_kind;
  ty: Types.ty;
  loc: Token.loc;
}

and texpr_kind =
  | TEInt of int
  | TEFloat of float
  | TEBool of bool
  | TEString of string
  | TEByte of int
  | TERune of int
  | TEUnit
  | TEVar of string
  | TELet of string * Types.scheme option * texpr * texpr
  | TELetRec of string * Types.scheme option * texpr * texpr
  | TEFun of string * texpr * bool
  | TEApp of texpr * texpr
  | TEIf of texpr * texpr * texpr
  | TEBinop of Ast.binop * texpr * texpr
  | TEUnop of Ast.unop * texpr
  | TETuple of texpr list
  | TERecord of (string * texpr) list
  | TERecordUpdate of texpr * (string * texpr) list
  | TERecordUpdateIdx of texpr * (texpr * texpr) list  (* base, [(idx_expr, val_expr)] *)
  | TEField of texpr * string
  | TEIndex of texpr * texpr
  | TECons of texpr * texpr
  | TENil
  | TEConstruct of string * texpr option
  | TEMatch of texpr * (Ast.pattern * texpr option * texpr) list * bool
  | TELetMut of string * texpr * texpr
  | TEAssign of string * texpr
  | TEFieldAssign of texpr * string * texpr
  | TESeq of texpr * texpr
  | TEPerform of string * texpr
  | TEHandle of texpr * thandle_arm list
  | TEResume of texpr * texpr
  | TEWhile of texpr * texpr
  | TEBreak of texpr
  | TEContinueLoop
  | TEFoldContinue of texpr
  | TEForLoop of texpr
  | TELetRecAnd of (string * texpr) list * texpr
  | TEMap of (texpr * texpr) list
  | TEArray of texpr list
  | TEReturn of texpr

and thandle_arm =
  | THReturn of string * texpr
  | THOp of string * string * string * texpr

type tdecl =
  | TDLet of string * texpr
  | TDLetMut of string * texpr
  | TDLetRec of string * texpr
  | TDLetRecAnd of (string * texpr) list
  | TDType of string * Ast.type_def
  | TDExpr of texpr
  | TDClass of string
  | TDEffect of string
  | TDExtern of string * Types.scheme
  | TDModule of string * tdecl list
  | TDOpen of (string * string) list

type tprogram = tdecl list

type loop_info = WhileLoop | UnitLoop | FoldLoop of string

type ctx = {
  vars: (string * Types.scheme) list;
  mutable_vars: string list;
  type_env: Types.type_env;
  loop_info: loop_info option;
  current_module: string option;
  constraint_tvars: int list;
  current_eff: Types.eff;
  return_type: Types.ty option;
  inside_handler: bool;
  return_used: bool ref;
}

let current_loc = ref Token.{ line = 0; col = 0; offset = 0 }

let error msg = raise (Type_error (msg, !current_loc))

let rec strip_loc (expr : Ast.expr) =
  match expr with
  | Ast.ELoc (_, inner) -> strip_loc inner
  | e -> e

let try_unify t1 t2 =
  try Types.unify t1 t2
  with Types.Unify_error msg -> error msg

let try_unify_eff e1 e2 =
  try Types.unify_eff e1 e2
  with Types.Unify_error msg -> error msg

let try_subeffect source target =
  try Types.subeffect source target
  with Types.Unify_error msg -> error msg

let rec max_tgen_in_ty = function
  | Types.TGen i -> i
  | Types.TArrow (a, _, b) | Types.TCont (a, _, b) -> max (max_tgen_in_ty a) (max_tgen_in_ty b)
  | Types.TTuple ts -> List.fold_left (fun acc t -> max acc (max_tgen_in_ty t)) (-1) ts
  | Types.TList t -> max_tgen_in_ty t
  | Types.TRecord row -> max_tgen_in_rrow row
  | Types.TVariant (_, args) -> List.fold_left (fun acc t -> max acc (max_tgen_in_ty t)) (-1) args
  | Types.TMap (k, v) -> max (max_tgen_in_ty k) (max_tgen_in_ty v)
  | Types.TArray t -> max_tgen_in_ty t
  | _ -> -1

and max_tgen_in_rrow = function
  | Types.RRow (_, ty, tail) -> max (max_tgen_in_ty ty) (max_tgen_in_rrow tail)
  | Types.RGen _ -> -1
  | _ -> -1

let rec max_effgen_in_eff = function
  | Types.EffGen i -> i
  | Types.EffRow (_, params, tail) ->
    let ty_max = List.fold_left (fun acc t -> max acc (max_tgen_in_ty t)) (-1) params in
    max ty_max (max_effgen_in_eff tail)
  | _ -> -1

let max_effgen_in_ty = function
  | Types.TArrow (a, eff, b) | Types.TCont (a, eff, b) ->
    max (max (max_tgen_in_ty a) (max_tgen_in_ty b)) (max_effgen_in_eff eff)
  | _ -> -1

let lookup_var ctx level name =
  match List.assoc_opt name ctx.vars with
  | Some scheme -> Types.instantiate level scheme
  | None ->
    (* Try ClassName.method qualified access for typeclass methods *)
    match String.rindex_opt name '.' with
    | Some i ->
      let class_prefix = String.sub name 0 i in
      let method_name = String.sub name (i + 1) (String.length name - i - 1) in
      (match List.find_opt (fun cls ->
        String.equal cls.Types.class_name class_prefix &&
        List.exists (fun (m, _) -> String.equal m method_name) cls.class_methods
      ) ctx.type_env.Types.classes with
      | Some class_def ->
        let method_ty = List.assoc method_name class_def.Types.class_methods in
        let num_class_params = List.length class_def.Types.class_params in
        let max_gen = max_tgen_in_ty method_ty in
        let quant = max (max_gen + 1) num_class_params in
        let max_effgen = max_effgen_in_ty method_ty in
        let equant = if max_effgen >= 0 then max_effgen + 1 else 0 in
        let scheme = { Types.quant; equant; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = method_ty } in
        Types.instantiate level scheme
      | None -> error (Printf.sprintf "unbound variable: %s" name))
    | None -> error (Printf.sprintf "unbound variable: %s" name)

let extend_var ctx name scheme =
  { ctx with vars = (name, scheme) :: ctx.vars;
    mutable_vars = List.filter (fun n -> n <> name) ctx.mutable_vars }

let extend_var_mono ctx name ty =
  extend_var ctx name (Types.mono ty)

let extend_var_mutable ctx name scheme =
  { ctx with vars = (name, scheme) :: ctx.vars;
    mutable_vars = name :: List.filter (fun n -> n <> name) ctx.mutable_vars }

let resolve_type_alias type_env name =
  match List.assoc_opt name type_env.Types.type_aliases with
  | Some canonical -> canonical
  | None -> name

let find_variant_info type_env name =
  let name = resolve_type_alias type_env name in
  List.find_opt (fun (n, _, _, _) -> String.equal n name) type_env.Types.variants

let subst_tgens inst_tys ty =
  let arr = Array.of_list inst_tys in
  let rec go = function
    | Types.TGen i when i < Array.length arr -> arr.(i)
    | Types.TArrow (a, eff, b) -> Types.TArrow (go a, go_eff eff, go b)
    | Types.TCont (a, eff, b) -> Types.TCont (go a, go_eff eff, go b)
    | Types.TTuple ts -> Types.TTuple (List.map go ts)
    | Types.TList t -> Types.TList (go t)
    | Types.TArray t -> Types.TArray (go t)
    | Types.TRecord row -> Types.TRecord (go_rrow row)
    | Types.TVariant (name, args) -> Types.TVariant (name, List.map go args)
    | Types.TPolyVariant row -> Types.TPolyVariant (go_pv row)
    | Types.TMap (k, v) -> Types.TMap (go k, go v)
    | t -> t
  and go_eff = function
    | Types.EffRow (label, params, tail) -> Types.EffRow (label, List.map go params, go_eff tail)
    | e -> e
  and go_pv = function
    | Types.PVRow (tag, ty_opt, tail) -> Types.PVRow (tag, Option.map go ty_opt, go_pv tail)
    | r -> r
  and go_rrow = function
    | Types.RRow (name, ty, tail) -> Types.RRow (name, go ty, go_rrow tail)
    | r -> r
  in
  go ty

(* Replace EffEmpty in arrow positions with fresh effect variables.
   Used when expanding stored types (synonyms, constructor args) so
   that rigid EffEmpty doesn't block unification with effectful values. *)
let freshen_arrow_effects level ty =
  let rec go = function
    | Types.TArrow (a, eff, b) ->
      let eff' = match eff with
        | Types.EffEmpty -> Types.new_effvar level
        | e -> e
      in
      Types.TArrow (go a, eff', go b)
    | Types.TCont (a, eff, b) ->
      let eff' = match eff with
        | Types.EffEmpty -> Types.new_effvar level
        | e -> e
      in
      Types.TCont (go a, eff', go b)
    | Types.TTuple ts -> Types.TTuple (List.map go ts)
    | Types.TList t -> Types.TList (go t)
    | Types.TArray t -> Types.TArray (go t)
    | Types.TRecord row -> Types.TRecord (go_rrow row)
    | Types.TVariant (name, args) -> Types.TVariant (name, List.map go args)
    | Types.TPolyVariant row -> Types.TPolyVariant (go_pv row)
    | Types.TMap (k, v) -> Types.TMap (go k, go v)
    | t -> t
  and go_pv = function
    | Types.PVRow (tag, ty_opt, tail) -> Types.PVRow (tag, Option.map go ty_opt, go_pv tail)
    | r -> r
  and go_rrow = function
    | Types.RRow (name, ty, tail) -> Types.RRow (name, go ty, go_rrow tail)
    | r -> r
  in
  go ty

(* Freshen all unbound type variables in a type, replacing each unique TVar
   with a fresh one. This is needed for effect operation types where the
   operation has free type variables (like 'a in: op : string -> 'a) that
   should be instantiated freshly at each perform site. *)
let freshen_tvars level ty =
  let mapping = Hashtbl.create 4 in
  let rec go = function
    | Types.TVar { contents = Types.Unbound (id, _) } ->
      (match Hashtbl.find_opt mapping id with
       | Some fresh -> fresh
       | None ->
         let fresh = Types.new_tvar level in
         Hashtbl.replace mapping id fresh;
         fresh)
    | Types.TVar { contents = Types.Link t } -> go t
    | Types.TArrow (a, eff, b) -> Types.TArrow (go a, go_eff eff, go b)
    | Types.TCont (a, eff, b) -> Types.TCont (go a, go_eff eff, go b)
    | Types.TTuple ts -> Types.TTuple (List.map go ts)
    | Types.TList t -> Types.TList (go t)
    | Types.TArray t -> Types.TArray (go t)
    | Types.TRecord row -> Types.TRecord (go_rrow row)
    | Types.TVariant (name, args) -> Types.TVariant (name, List.map go args)
    | Types.TPolyVariant row -> Types.TPolyVariant (go_pv row)
    | Types.TMap (k, v) -> Types.TMap (go k, go v)
    | t -> t
  and go_eff = function
    | Types.EffVar { contents = Types.EffUnbound _ } as e -> e
    | Types.EffVar { contents = Types.EffLink e } -> go_eff e
    | Types.EffRow (label, params, tail) ->
      Types.EffRow (label, List.map go params, go_eff tail)
    | e -> e
  and go_pv = function
    | Types.PVRow (tag, ty_opt, tail) ->
      Types.PVRow (tag, Option.map go ty_opt, go_pv tail)
    | r -> r
  and go_rrow = function
    | Types.RRow (name, ty, tail) -> Types.RRow (name, go ty, go_rrow tail)
    | Types.RVar { contents = Types.RUnbound _ } -> Types.new_rvar level
    | Types.RVar { contents = Types.RLink r } -> go_rrow r
    | r -> r
  in
  go ty

let rec resolve_ty_annot_shared ctx level tvars (annot : Ast.ty_annot) : Types.ty =
  let rec go = function
    | Ast.TyVar name ->
      (match Hashtbl.find_opt tvars name with
       | Some tv -> tv
       | None ->
         let tv = Types.new_tvar level in
         Hashtbl.replace tvars name tv;
         tv)
    | Ast.TyName "int" -> Types.TInt
    | Ast.TyName "float" -> Types.TFloat
    | Ast.TyName "bool" -> Types.TBool
    | Ast.TyName "string" -> Types.TString
    | Ast.TyName "byte" -> Types.TByte
    | Ast.TyName "rune" -> Types.TRune
    | Ast.TyName "unit" -> Types.TUnit
    | Ast.TyName name ->
      let canonical = resolve_type_alias ctx.type_env name in
      (match List.find_opt (fun (n, _, _) -> String.equal n canonical) ctx.type_env.type_synonyms with
       | Some (_, 0, ty) -> freshen_tvars level (freshen_arrow_effects level ty)
       | Some (_, n, _) ->
         error (Printf.sprintf "type %s expects %d type argument(s)" name n)
       | None ->
         (match find_variant_info ctx.type_env name with
          | Some (_, 0, _, _) -> Types.TVariant (canonical, [])
          | Some (_, n, _, _) ->
            error (Printf.sprintf "type %s expects %d type argument(s)" name n)
          | None ->
            if List.assoc_opt canonical ctx.type_env.records <> None then
              let fields = List.assoc canonical ctx.type_env.records in
              if fields = [] then Types.TRecord Types.RWild
              else Types.TRecord (Types.fields_to_closed_row fields)
            else
              error (Printf.sprintf "unknown type: %s" name)))
    | Ast.TyArrow (a, b, eff_opt) ->
      let eff = match eff_opt with
        | None -> Types.new_effvar level          (* infer: today's behavior *)
        | Some Ast.EffAnnotPure -> Types.EffEmpty (* explicitly pure *)
        | Some (Ast.EffAnnotRow items) ->
          resolve_eff_items ctx level tvars items
      in
      Types.TArrow (go a, eff, go b)
    | Ast.TyTuple ts -> Types.TTuple (List.map go ts)
    | Ast.TyList t -> Types.TList (go t)
    | Ast.TyArray t -> Types.TArray (go t)
    | Ast.TyMap (k, v) -> Types.TMap (go k, go v)
    | Ast.TyApp (args, name) ->
      let canonical = resolve_type_alias ctx.type_env name in
      let arg_tys = List.map go args in
      (match List.find_opt (fun (n, _, _) -> String.equal n canonical) ctx.type_env.type_synonyms with
       | Some (_, np, ty) ->
         if List.length arg_tys <> np then
           error (Printf.sprintf "type %s expects %d type argument(s), got %d"
             name np (List.length arg_tys));
         freshen_tvars level (freshen_arrow_effects level (subst_tgens arg_tys ty))
       | None ->
         (match find_variant_info ctx.type_env name with
          | Some (_, np, _, _) ->
            if List.length arg_tys <> np then
              error (Printf.sprintf "type %s expects %d type argument(s), got %d"
                name np (List.length arg_tys));
            Types.TVariant (canonical, arg_tys)
          | None -> error (Printf.sprintf "unknown type constructor: %s" name)))
    | Ast.TyRecord (fields, is_open) ->
      let fields = List.map (fun (n, t) -> (n, go t)) fields in
      let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
      let tail = if is_open then Types.new_rvar level else Types.REmpty in
      Types.TRecord (List.fold_right (fun (n, t) acc -> Types.RRow (n, t, acc)) fields tail)
    | Ast.TyQualified (path, name) ->
      let qualified = String.concat "." path ^ "." ^ name in
      go (Ast.TyName qualified)
    | Ast.TyPolyVariant (kind, tags) ->
      let tail = match kind with
        | Ast.PVExact | Ast.PVUpper -> Types.PVEmpty
        | Ast.PVLower -> Types.new_pvvar level
      in
      let row = List.fold_right (fun (tag, payload_annot) acc ->
        Types.PVRow (tag, Option.map go payload_annot, acc)
      ) tags tail in
      Types.TPolyVariant row
    | Ast.TyWithEffect (ty, _eff) ->
      (* TyWithEffect should be handled by wrap_params_decl before reaching here.
         If it leaks through (e.g. no params), just resolve the inner type. *)
      go ty
  in
  go annot

(* Resolve effect annotation items into an eff row *)
and resolve_eff_items ctx level tvars (items : Ast.eff_item list) : Types.eff =
  let labels = ref [] in
  let row_var = ref None in
  List.iter (fun item ->
    match item with
    | Ast.EffLabel (name, param_annots) ->
      if !row_var <> None then
        error "effect labels must come before row variable";
      (* Validate effect exists *)
      let edef = match List.find_opt
        (fun e -> String.equal e.Types.effect_name name)
        ctx.type_env.Types.effects with
        | Some e -> e
        | None -> error (Printf.sprintf "unknown effect: %s" name)
      in
      let expected_params = List.length edef.Types.effect_params in
      let actual_params = List.length param_annots in
      if expected_params <> actual_params then
        error (Printf.sprintf "effect %s expects %d type parameter(s), got %d"
          name expected_params actual_params);
      labels := (name, param_annots) :: !labels
    | Ast.EffVar name ->
      if !row_var <> None then
        error "at most one effect row variable allowed";
      row_var := Some name
  ) items;
  let labels = List.rev !labels in
  (* Build the tail: row variable or fresh effvar (open row) *)
  let tail = match !row_var with
    | None -> Types.new_effvar level
    | Some _name -> Types.new_effvar level  (* treated as fresh in normal context *)
  in
  (* Build the eff row chain with resolved type params *)
  List.fold_right (fun (name, param_annots) acc ->
    let resolved_params = List.map (resolve_ty_annot_shared ctx level tvars) param_annots in
    Types.EffRow (name, resolved_params, acc)
  ) labels tail

let resolve_ty_annot ctx level (annot : Ast.ty_annot) : Types.ty =
  let tvars = Hashtbl.create 4 in
  resolve_ty_annot_shared ctx level tvars annot

(* Instantiate TGen placeholders in record fields as fresh type variables *)
let instantiate_record_fields level fields =
  let max_gen = List.fold_left (fun acc (_, ty) ->
    max acc (max_tgen_in_ty ty)
  ) (-1) fields in
  if max_gen < 0 then fields
  else
    let fresh = List.init (max_gen + 1) (fun _ -> Types.new_tvar level) in
    List.map (fun (n, ty) -> (n, freshen_arrow_effects level (subst_tgens fresh ty))) fields

(* Convert stored field list to an instantiated closed record row *)
let instantiate_record_row level fields =
  let fields = instantiate_record_fields level fields in
  Types.fields_to_closed_row fields

(* Forward references for module functions, resolved after module processing is defined *)
let find_module_in_env_ref : (Types.type_env -> string -> Types.module_info option) ref =
  ref (fun _ _ -> failwith "find_module_in_env not yet initialized")
let open_module_into_ctx_ref : (ctx -> string -> string list option -> ctx) ref =
  ref (fun _ _ _ -> failwith "open_module_into_ctx not yet initialized")
let exhaustiveness_check_ref : (ctx -> Types.ty -> (Ast.pattern * texpr option * texpr) list -> unit) ref =
  ref (fun _ _ _ -> ())

let mk te ty = { expr = te; ty; loc = !current_loc }

(* Extract params and return annotation from a wrapped EFun/EAnnot expression.
   This is used to recover explicit annotations for polymorphic recursion
   on expression-level let rec. *)
let rec extract_fn_info (expr : Ast.expr) =
  match expr with
  | Ast.ELoc (_, inner) -> extract_fn_info inner
  | Ast.EAnnot (inner, _) when (match inner with Ast.EFun _ | Ast.ELoc (_, Ast.EFun _) -> true | _ -> false) ->
    (* Outer annotation wrapping a function chain (effect annotation case) — skip *)
    extract_fn_info inner
  | Ast.EFun (param, body) ->
    let (params, ret, inner) = extract_fn_info body in
    (param :: params, ret, inner)
  | Ast.EAnnot (inner, annot) ->
    ([], Some annot, inner)
  | _ -> ([], None, expr)

(* Inside a module, qualify local variable references to avoid global name collisions.
   Walk up the module prefix hierarchy: Outer.Inner. -> Outer. -> top-level *)
let resolve_module_name ctx name =
  match ctx.current_module with
  | Some prefix ->
    let rec try_prefix p =
      let qualified = p ^ name in
      if List.exists (fun (n, _) -> n = qualified) ctx.vars then qualified
      else
        let p' = String.sub p 0 (max 0 (String.length p - 1)) in
        match String.rindex_opt p' '.' with
        | Some idx -> try_prefix (String.sub p' 0 (idx + 1))
        | None -> name
    in
    try_prefix prefix
  | None -> name

(* ==== Implicit typeclass constraint inference ==== *)

(* Find the type of the first TEVar usage of a name in a typed expression.
   Used to reconnect tvars disconnected by inner let generalization. *)
let find_var_type_in target_name te =
  let result = ref None in
  let rec go te =
    if Option.is_some !result then ()
    else match te.expr with
    | TEVar n when String.equal n target_name -> result := Some te.ty
    | TEVar _ | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEByte _
    | TERune _ | TEUnit | TENil | TEContinueLoop -> ()
    | TEApp (fn, arg) -> go fn; go arg
    | TEFun (_, body, _) | TEUnop (_, body) | TEField (body, _) | TEBreak body
    | TEFoldContinue body | TEForLoop body | TEReturn body | TEAssign (_, body)
    | TEPerform (_, body) -> go body
    | TELet (_, _, e1, e2) | TELetRec (_, _, e1, e2) | TELetMut (_, e1, e2)
    | TEBinop (_, e1, e2) | TECons (e1, e2) | TESeq (e1, e2)
    | TEWhile (e1, e2) | TEResume (e1, e2) | TEFieldAssign (e1, _, e2)
    | TEIndex (e1, e2) -> go e1; go e2
    | TEIf (c, t, e) -> go c; go t; go e
    | TETuple es | TEArray es -> List.iter go es
    | TEMap kvs -> List.iter (fun (k, v) -> go k; go v) kvs
    | TERecord fields -> List.iter (fun (_, e) -> go e) fields
    | TERecordUpdate (base, overrides) -> go base; List.iter (fun (_, e) -> go e) overrides
    | TERecordUpdateIdx (base, pairs) -> go base; List.iter (fun (i, v) -> go i; go v) pairs
    | TEConstruct (_, arg) -> Option.iter go arg
    | TEMatch (scr, arms, _) ->
      go scr; List.iter (fun (_, g, body) -> Option.iter go g; go body) arms
    | TEHandle (body, arms) ->
      go body; List.iter (fun arm -> match arm with
        | THReturn (_, e) -> go e | THOp (_, _, _, e) -> go e) arms
    | TELetRecAnd (bindings, body) ->
      List.iter (fun (_, e) -> go e) bindings; go body
  in
  go te;
  !result

(* Walk a typed expression to find typeclass method calls on polymorphic type
   variables. Returns the scheme with inferred constraints added. *)

let infer_implicit_constraints binding_name type_env vars te scheme =
  if scheme.Types.constraints <> [] || scheme.Types.quant = 0 then scheme
  else begin
    (* Walk scheme body (with TGen) alongside original type (with TVars)
       to build tvar_id -> TGen index mapping *)
    let id_map = Hashtbl.create 8 in
    let rec build_map s a =
      let a = Types.repr a in
      match s with
      | Types.TGen i ->
        (match a with
         | Types.TVar { contents = Types.Unbound (id, _) } ->
           Hashtbl.replace id_map id (Types.TGen i)
         | _ -> ())
      | Types.TArrow (s1, _, s2) ->
        (match a with Types.TArrow (a1, _, a2) -> build_map s1 a1; build_map s2 a2 | _ -> ())
      | Types.TCont (s1, _, s2) ->
        (match a with Types.TCont (a1, _, a2) -> build_map s1 a1; build_map s2 a2 | _ -> ())
      | Types.TTuple ss ->
        (match a with Types.TTuple aa when List.length ss = List.length aa ->
          List.iter2 build_map ss aa | _ -> ())
      | Types.TList s1 -> (match a with Types.TList a1 -> build_map s1 a1 | _ -> ())
      | Types.TArray s1 -> (match a with Types.TArray a1 -> build_map s1 a1 | _ -> ())
      | Types.TMap (sk, sv) ->
        (match a with Types.TMap (ak, av) -> build_map sk ak; build_map sv av | _ -> ())
      | Types.TVariant (_, ss) ->
        (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
          List.iter2 build_map ss aa | _ -> ())
      | Types.TPolyVariant srow ->
        (match a with Types.TPolyVariant arow -> build_map_pvrows srow arow | _ -> ())
      | Types.TRecord srow ->
        (match a with Types.TRecord arow -> build_map_rrows srow arow | _ -> ())
      | _ -> ()
    and build_map_rrows srow arow =
      let sf = Types.record_row_to_fields srow in
      let af = Types.record_row_to_fields arow in
      if List.length sf = List.length af then
        List.iter2 (fun (_, st) (_, at_) -> build_map st at_) sf af
    and build_map_pvrows srow arow =
      let rec collect_tags row =
        match row with
        | Types.PVRow (tag, ty_opt, tail) -> (tag, ty_opt) :: collect_tags tail
        | Types.PVVar { contents = Types.PVLink r } -> collect_tags r
        | _ -> []
      in
      let atags = collect_tags arow in
      let rec go row =
        match row with
        | Types.PVRow (stag, sty_opt, stail) ->
          (match List.assoc_opt stag atags with
           | Some aty_opt ->
             (match sty_opt, aty_opt with
              | Some sty, Some aty -> build_map sty aty
              | _ -> ())
           | None -> ());
          go stail
        | Types.PVVar { contents = Types.PVLink r } -> go r
        | _ -> ()
      in
      go srow
    in
    build_map scheme.Types.body te.ty;
    let found_multi = ref [] in
    let pending_caty = ref [] in
    let phantom_counter = ref scheme.Types.quant in
    let tvar_to_phantom = Hashtbl.create 4 in
    let set_add tbl key =
      if not (List.exists (fun k -> k = key) !tbl) then
        tbl := key :: !tbl
    in
    (* Propagate constraints from a constrained variable reference:
       map the inner scheme's TGens to actual types at this call site,
       then check if any constrained type args are TVars in our id_map *)
    let propagate_constrained_ref local_sch actual_ty =
      let call_map = Hashtbl.create 4 in
      let phantom_remap = Hashtbl.create 4 in
      let rec map_tgens s a =
        let a = Types.repr a in
        match s with
        | Types.TGen i ->
          if not (Hashtbl.mem call_map i) then Hashtbl.replace call_map i a
        | Types.TArrow (s1, _, s2) ->
          (match a with Types.TArrow (a1, _, a2) -> map_tgens s1 a1; map_tgens s2 a2 | _ -> ())
        | Types.TCont (s1, _, s2) ->
          (match a with Types.TCont (a1, _, a2) -> map_tgens s1 a1; map_tgens s2 a2 | _ -> ())
        | Types.TTuple ss ->
          (match a with Types.TTuple aa when List.length ss = List.length aa ->
            List.iter2 map_tgens ss aa | _ -> ())
        | Types.TList s1 -> (match a with Types.TList a1 -> map_tgens s1 a1 | _ -> ())
        | Types.TArray s1 -> (match a with Types.TArray a1 -> map_tgens s1 a1 | _ -> ())
        | Types.TMap (sk, sv) ->
          (match a with Types.TMap (ak, av) -> map_tgens sk ak; map_tgens sv av | _ -> ())
        | Types.TVariant (_, ss) ->
          (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
            List.iter2 map_tgens ss aa | _ -> ())
        | _ -> ()
      in
      map_tgens local_sch.Types.body actual_ty;
      List.iter (fun (cc : Types.class_constraint) ->
        let outer_tgens = List.filter_map (fun (ca : Types.class_arg) ->
          match ca with
          | CATGen tgen_idx ->
            (match Hashtbl.find_opt call_map tgen_idx with
             | Some (Types.TVar { contents = Types.Unbound (id, _) } as tv) ->
               (match Hashtbl.find_opt id_map id with
                | Some (Types.TGen outer_tgen) -> Some (Types.CATGen outer_tgen)
                | _ -> Some (Types.CATy tv))
             | Some _ -> None  (* Concrete type: resolve at inner level, don't propagate *)
             | None -> None)
          | CATy ty ->
            (* Check if this CATy tvar is in the outer id_map — promote to CATGen *)
            (match Types.repr ty with
             | Types.TVar { contents = Types.Unbound (id, _) } ->
               (match Hashtbl.find_opt id_map id with
                | Some (Types.TGen outer_tgen) -> Some (Types.CATGen outer_tgen)
                | _ -> Some ca)
             | _ -> Some ca)
          | CAWild -> Some Types.CAWild  (* propagate wildcard as-is *)
          | CAPhantom (inner_idx, _) ->
            (* Remap phantom index to avoid collision with outer TGen indices.
               Each call to propagate_constrained_ref gets fresh remapping so
               different calls to the same inner function get different phantoms,
               but constraints sharing the same inner phantom index stay linked. *)
            let (outer_idx, outer_tv) = match Hashtbl.find_opt phantom_remap inner_idx with
              | Some pair -> pair
              | None ->
                let idx = !phantom_counter in
                incr phantom_counter;
                let tv = Types.new_tvar 1 in
                Hashtbl.replace phantom_remap inner_idx (idx, tv);
                (idx, tv) in
            Some (Types.CAPhantom (outer_idx, outer_tv))
        ) cc.cc_args in
        if List.length outer_tgens = List.length cc.cc_args then
          set_add found_multi (cc.cc_class, outer_tgens)
      ) local_sch.Types.constraints
    in
    let rec walk locals te =
      (match te.expr with
       | TEVar name ->
         (* Check if this references a locally-bound constrained function *)
         (match List.assoc_opt name locals with
          | Some local_sch when local_sch.Types.constraints <> [] ->
            propagate_constrained_ref local_sch te.ty
          | _ -> ());
         (* Also check global constrained variables *)
         (match List.assoc_opt name vars with
          | Some var_sch when var_sch.Types.constraints <> [] ->
            propagate_constrained_ref var_sch te.ty
          | _ -> ());
         (* A TEVar is a class method if find_method_class matches AND the
            variable's scheme body in vars equals the class method's schema type
            (rejects local definitions that shadow class methods).
            Also handles module-qualified names like "Map.keys". *)
         let (class_match, method_name) =
           match Types.find_method_class type_env.Types.classes name with
           | Some class_def when not (String.equal name binding_name) ->
             let method_ty = List.assoc name class_def.Types.class_methods in
             (match List.assoc_opt name vars with
              | Some s -> if s.Types.body = method_ty then (Some class_def, name) else (None, name)
              | None -> (None, name))
           | _ ->
             (* Try stripping module prefix for qualified names like "Map.keys" *)
             (match String.rindex_opt name '.' with
              | None -> (None, name)
              | Some i ->
                let short = String.sub name (i + 1) (String.length name - i - 1) in
                let class_prefix = String.sub name 0 i in
                let mod_prefix = String.sub name 0 (i + 1) in
                (match Types.find_method_class type_env.Types.classes short with
                 | Some class_def when not (String.equal short binding_name)
                     && (String.equal class_def.Types.class_name class_prefix
                         || (String.length class_def.Types.class_name > String.length mod_prefix
                             && String.sub class_def.Types.class_name 0 (String.length mod_prefix) = mod_prefix)) ->
                   let method_ty = List.assoc short class_def.Types.class_methods in
                   (* Try both qualified and unqualified names in vars *)
                   let var_sch = match List.assoc_opt name vars with
                     | Some _ as r -> r
                     | None -> List.assoc_opt short vars
                   in
                   (match var_sch with
                    | Some s -> if s.Types.body = method_ty then (Some class_def, short) else (None, name)
                    | None -> (None, name))
                 | _ -> (None, name)))
         in
         (match class_match with
          | Some class_def ->
            let method_schema_ty = List.assoc method_name class_def.Types.class_methods in
            let num_params = List.length class_def.Types.class_params in
            let param_map = Hashtbl.create num_params in
            let rec go s r =
              let r = Types.repr r in
              match s, r with
              | Types.TGen i, _ when i < num_params ->
                if not (Hashtbl.mem param_map i) then Hashtbl.replace param_map i r
              | Types.TArrow (s1, _, s2), Types.TArrow (r1, _, r2)
              | Types.TCont (s1, _, s2), Types.TCont (r1, _, r2) -> go s1 r1; go s2 r2
              | Types.TTuple ss, Types.TTuple rs when List.length ss = List.length rs ->
                List.iter2 go ss rs
              | Types.TList s1, Types.TList r1 -> go s1 r1
              | Types.TArray s1, Types.TArray r1 -> go s1 r1
              | Types.TMap (sk, sv), Types.TMap (rk, rv) -> go sk rk; go sv rv
              | Types.TVariant (_, ss), Types.TVariant (_, rs) when List.length ss = List.length rs ->
                List.iter2 go ss rs
              | _ -> ()
            in
            go method_schema_ty te.ty;
            (* Build class_arg for each class param: CATGen for polymorphic, CATy for concrete *)
            let cc_args = List.init num_params (fun i ->
              match Hashtbl.find_opt param_map i with
              | Some (Types.TVar { contents = Types.Unbound (id, _) } as tv) ->
                (match Hashtbl.find_opt id_map id with
                 | Some (Types.TGen tgen_idx) -> Some (Types.CATGen tgen_idx)
                 | _ ->
                   (* Tvar not in id_map: not generalized (doesn't appear in function type).
                      If it's a fundep target, use CAPhantom so each call site gets a fresh tvar
                      but constraints sharing the same fundep target share the same phantom index.
                      CATy would store a mutable ref that gets permanently linked at first call. *)
                   let is_fundep_target = List.exists (fun (fd : Types.fundep) ->
                     List.mem i fd.fd_to
                   ) class_def.Types.class_fundeps in
                   if is_fundep_target then begin
                     let phantom_idx = match Hashtbl.find_opt tvar_to_phantom id with
                       | Some idx -> idx
                       | None ->
                         let idx = !phantom_counter in
                         incr phantom_counter;
                         Hashtbl.replace tvar_to_phantom id idx;
                         idx in
                     Some (Types.CAPhantom (phantom_idx, tv))
                   end
                   else Some (Types.CATy tv))
              | Some ty -> Some (Types.CATy ty)
              | None ->
                (* Param not in method type. If it's a fundep target, mark as CAWild
                   so the constraint is still recorded and resolved via fundep at call site. *)
                let is_fundep_target = List.exists (fun (fd : Types.fundep) ->
                  List.mem i fd.fd_to
                ) class_def.Types.class_fundeps in
                if is_fundep_target then Some Types.CAWild
                else None
            ) in
            (* If there are CATy entries containing unresolved tvars mixed with
               CAWild/CAPhantom, the CATy has definition-time tvars that won't match
               call-site tvars. Revert CAWild/CAPhantom to None so the constraint
               is dropped. Fully concrete CATy (e.g. CATy(TInt)) is fine. *)
            let has_nonconc_caty = List.exists (fun opt ->
              match opt with
              | Some (Types.CATy ty) -> not (Types.is_concrete ty)
              | _ -> false
            ) cc_args in
            let has_any_caty = List.exists (fun opt ->
              match opt with Some (Types.CATy _) -> true | _ -> false
            ) cc_args in
            let cc_args = if has_nonconc_caty then
              List.map (fun opt ->
                match opt with Some Types.CAWild -> None | Some (Types.CAPhantom _) -> None | _ -> opt
              ) cc_args
            else cc_args in
            (* Record if at least one param is a TGen and all params are resolved *)
            let has_tgen = List.exists (fun opt ->
              match opt with Some (Types.CATGen _) -> true | _ -> false
            ) cc_args in
            let has_caty = has_any_caty in
            let all_resolved = List.for_all (fun opt -> opt <> None) cc_args in
            let allow = has_tgen && all_resolved &&
              (not has_caty || match scheme.Types.body with Types.TArrow _ | Types.TCont _ -> true | _ -> false) in
            if allow then begin
              let cc_args = List.filter_map Fun.id cc_args in
              set_add found_multi (class_def.Types.class_name, cc_args)
            end else if (not has_tgen) && all_resolved then begin
              (* Defer CATy-only constraints: their Unbound tvars may link to
                 constraints found later in the walk (order-independent). *)
              let has_unbound_caty = List.exists (fun opt ->
                match opt with
                | Some (Types.CATy (Types.TVar { contents = Types.Unbound _ })) -> true
                | _ -> false
              ) cc_args in
              let is_arrow = match scheme.Types.body with Types.TArrow _ | Types.TCont _ -> true | _ -> false in
              if has_unbound_caty && is_arrow then begin
                let cc_args = List.filter_map Fun.id cc_args in
                set_add pending_caty (class_def.Types.class_name, cc_args)
              end
            end
          | None -> ())
       | _ -> ());
      walk_children locals te
    and walk_children locals te =
      match te.expr with
      | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEByte _
      | TERune _ | TEUnit | TEVar _ | TENil | TEContinueLoop -> ()
      | TELet (name, Some sch, _e1, e2) | TELetRec (name, Some sch, _e1, e2) ->
        (* Don't walk e1 — its class method calls are the inner function's
           responsibility. Walk e2 with the constrained binding in scope. *)
        walk ((name, sch) :: locals) e2
      | TELet (name, None, e1, e2) | TELetRec (name, None, e1, e2) ->
        (* Reconnect tvars disconnected by inner let generalization:
           e1.ty was generalized then instantiated for e2, creating separate tvars.
           Unify them so cross-constraint type flow works (e.g. Index result → Show). *)
        (match Types.repr e1.ty with
         | Types.TVar { contents = Types.Unbound _ } ->
           (match find_var_type_in name e2 with
            | Some inst_ty ->
              (match Types.repr inst_ty with
               | Types.TVar { contents = Types.Unbound _ } ->
                 (try Types.unify e1.ty inst_ty with _ -> ())
               | _ -> ())
            | None -> ())
         | _ -> ());
        walk locals e1; walk locals e2
      | TELetMut (_, e1, e2)
      | TEBinop (_, e1, e2) | TECons (e1, e2) | TESeq (e1, e2)
      | TEWhile (e1, e2) | TEResume (e1, e2)
      | TEFieldAssign (e1, _, e2) | TEIndex (e1, e2) ->
        walk locals e1; walk locals e2
      | TEApp (fn, arg) -> walk locals fn; walk locals arg
      | TEPerform (_, e) | TEUnop (_, e) | TEFun (_, e, _) | TEField (e, _) | TEBreak e
      | TEFoldContinue e | TEForLoop e | TEReturn e | TEAssign (_, e) ->
        walk locals e
      | TEIf (c, t, e) -> walk locals c; walk locals t; walk locals e
      | TETuple es | TEArray es -> List.iter (walk locals) es
      | TEMap kvs -> List.iter (fun (k, v) -> walk locals k; walk locals v) kvs
      | TERecord fields -> List.iter (fun (_, e) -> walk locals e) fields
      | TERecordUpdate (base, overrides) -> walk locals base; List.iter (fun (_, e) -> walk locals e) overrides
      | TERecordUpdateIdx (base, pairs) -> walk locals base; List.iter (fun (i, v) -> walk locals i; walk locals v) pairs
      | TEConstruct (_, arg) -> Option.iter (walk locals) arg
      | TEMatch (scrutinee, arms, _) ->
        walk locals scrutinee;
        List.iter (fun (_, guard, body) ->
          Option.iter (walk locals) guard; walk locals body) arms
      | TEHandle (body, arms) ->
        walk locals body;
        List.iter (fun arm -> match arm with
          | THReturn (_, e) -> walk locals e
          | THOp (_, _, _, e) -> walk locals e) arms
      | TELetRecAnd (bindings, body) ->
        List.iter (fun (_, e) -> walk locals e) bindings; walk locals body
    in
    walk [] te;
    (* Post-process: promote pending CATy-only constraints whose Unbound tvars
       appear in found_multi (as CATy or CAPhantom). When promoting, convert
       matching CATy entries to CAPhantom so they share the same phantom index
       and get fresh tvars at each call site instead of the definition-time tvar. *)
    let changed = ref true in
    while !changed do
      changed := false;
      let to_promote = List.fold_left (fun acc (cls, args) ->
        let dominated = List.exists (fun (ca : Types.class_arg) ->
          match ca with
          | Types.CATy (Types.TVar { contents = Types.Unbound (id, _) }) ->
            List.exists (fun (_c, fargs) ->
              List.exists (fun (fa : Types.class_arg) ->
                match fa with
                | Types.CATy (Types.TVar { contents = Types.Unbound (fid, _) }) -> fid = id
                | Types.CAPhantom (_, (Types.TVar { contents = Types.Unbound (fid, _) })) -> fid = id
                | _ -> false
              ) fargs
            ) !found_multi
          | _ -> false
        ) args in
        if dominated then (cls, args) :: acc else acc
      ) [] !pending_caty in
      List.iter (fun (cls, args) ->
        (* Convert CATy entries to CAPhantom when they match a phantom in found_multi *)
        let promoted_args = List.map (fun (ca : Types.class_arg) ->
          match ca with
          | Types.CATy (Types.TVar { contents = Types.Unbound (id, _) } as tv) ->
            let phantom_idx = ref None in
            List.iter (fun (_c, fargs) ->
              List.iter (fun (fa : Types.class_arg) ->
                match fa with
                | Types.CAPhantom (idx, Types.TVar { contents = Types.Unbound (fid, _) }) when fid = id ->
                  phantom_idx := Some idx
                | _ -> ()
              ) fargs
            ) !found_multi;
            (match !phantom_idx with
             | Some idx -> Types.CAPhantom (idx, tv)
             | None -> ca)
          | _ -> ca
        ) args in
        pending_caty := List.filter (fun k -> k <> (cls, args)) !pending_caty;
        set_add found_multi (cls, promoted_args);
        changed := true
      ) to_promote
    done;
    let cc_list = List.fold_left (fun acc (class_name, cc_args) ->
      let exists = List.exists (fun (cc : Types.class_constraint) ->
        String.equal cc.cc_class class_name && cc.cc_args = cc_args
      ) acc in
      if exists then acc
      else Types.{ cc_class = class_name; cc_args } :: acc
    ) [] !found_multi in
    if cc_list = [] then scheme
    else begin
      (* Sort: constraints with more CATGen entries first. These have more
         concrete type info from the function's TGen params, so their fundep
         resolutions propagate to dependent constraints that have only CATy. *)
      let cc_list = List.sort (fun (a : Types.class_constraint) (b : Types.class_constraint) ->
        let count_catgen args = List.length (List.filter (fun (ca : Types.class_arg) ->
          match ca with Types.CATGen _ -> true | _ -> false) args) in
        compare (count_catgen b.cc_args) (count_catgen a.cc_args)
      ) cc_list in
      { scheme with Types.constraints = cc_list }
    end
  end

(* Walk two types in parallel to map RGen indices to actual record rows.
   Returns a hashtable: rgen_idx -> full actual record row.
   Used at call sites to resolve record evidence (field offsets). *)
let build_rgen_map schema_ty actual_ty =
  let map = Hashtbl.create 4 in
  let rec walk s a =
    let a = Types.repr a in
    match s with
    | Types.TArrow (s1, _, s2) ->
      (match a with Types.TArrow (a1, _, a2) -> walk s1 a1; walk s2 a2 | _ -> ())
    | Types.TCont (s1, _, s2) ->
      (match a with Types.TCont (a1, _, a2) -> walk s1 a1; walk s2 a2 | _ -> ())
    | Types.TTuple ss ->
      (match a with Types.TTuple aa when List.length ss = List.length aa ->
        List.iter2 walk ss aa | _ -> ())
    | Types.TList s1 -> (match a with Types.TList a1 -> walk s1 a1 | _ -> ())
    | Types.TArray s1 -> (match a with Types.TArray a1 -> walk s1 a1 | _ -> ())
    | Types.TMap (sk, sv) ->
      (match a with Types.TMap (ak, av) -> walk sk ak; walk sv av | _ -> ())
    | Types.TVariant (_, ss) ->
      (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
        List.iter2 walk ss aa | _ -> ())
    | Types.TRecord srow ->
      (match a with Types.TRecord arow ->
        walk_rrows srow arow;
        let sf = Types.record_row_to_fields srow in
        let af = Types.record_row_to_fields arow in
        List.iter (fun (name, sty) ->
          match List.assoc_opt name af with
          | Some aty -> walk sty aty
          | None -> ()
        ) sf
      | _ -> ())
    | _ -> ()
  and walk_rrows srow arow =
    match srow with
    | Types.RGen i ->
      (* Map RGen(i) to the full actual row (all fields + tail) *)
      if not (Hashtbl.mem map i) then Hashtbl.replace map i arow
    | Types.RRow (_, _, stail) -> walk_rrows stail arow
    | _ -> ()
  in
  walk schema_ty actual_ty;
  map

(* Infer record evidence requirements for a polymorphic function.
   Detects TERecordUpdate nodes with open row tails and calls to functions
   with record evidence, then records which fields need offset evidence. *)
let infer_record_evidence vars te scheme =
  if scheme.Types.rquant = 0 then scheme
  else begin
    (* Build rvar_id -> RGen index mapping from scheme body and expression type *)
    let rvar_to_rgen = Hashtbl.create 4 in
    let rec walk_rmap s a =
      let a = Types.repr a in
      match s with
      | Types.TArrow (s1, _, s2) ->
        (match a with Types.TArrow (a1, _, a2) -> walk_rmap s1 a1; walk_rmap s2 a2 | _ -> ())
      | Types.TCont (s1, _, s2) ->
        (match a with Types.TCont (a1, _, a2) -> walk_rmap s1 a1; walk_rmap s2 a2 | _ -> ())
      | Types.TTuple ss ->
        (match a with Types.TTuple aa when List.length ss = List.length aa ->
          List.iter2 walk_rmap ss aa | _ -> ())
      | Types.TList s1 -> (match a with Types.TList a1 -> walk_rmap s1 a1 | _ -> ())
      | Types.TArray s1 -> (match a with Types.TArray a1 -> walk_rmap s1 a1 | _ -> ())
      | Types.TMap (sk, sv) ->
        (match a with Types.TMap (ak, av) -> walk_rmap sk ak; walk_rmap sv av | _ -> ())
      | Types.TVariant (_, ss) ->
        (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
          List.iter2 walk_rmap ss aa | _ -> ())
      | Types.TRecord srow ->
        (match a with Types.TRecord arow -> walk_rmap_rrows srow arow | _ -> ())
      | _ -> ()
    and walk_rmap_rrows srow arow =
      match srow, Types.rrow_repr arow with
      | Types.RGen i, Types.RVar { contents = Types.RUnbound (id, _) } ->
        Hashtbl.replace rvar_to_rgen id i
      | Types.RRow (_, sty, stail), Types.RRow (_, aty, atail) ->
        walk_rmap sty aty; walk_rmap_rrows stail atail
      | _ -> ()
    in
    walk_rmap scheme.Types.body te.ty;
    (* Collect evidence requirements *)
    let found_evidences = Hashtbl.create 4 in
    let add_evidence rgen_idx fields =
      let existing = match Hashtbl.find_opt found_evidences rgen_idx with
        | Some fs -> fs | None -> [] in
      let merged = List.fold_left (fun fs f ->
        if List.mem f fs then fs else f :: fs
      ) existing fields in
      Hashtbl.replace found_evidences rgen_idx merged
    in
    let rec get_row_tail row = match Types.rrow_repr row with
      | Types.RRow (_, _, tail) -> get_row_tail tail
      | tail -> tail
    in
    let rec walk te =
      (match te.expr with
       | TERecordUpdate (_base, overrides) ->
         (* Direct evidence: this function has a record update with open row *)
         (match Types.repr te.ty with
          | Types.TRecord row ->
            (match get_row_tail row with
             | Types.RVar { contents = Types.RUnbound (id, _) } ->
               (match Hashtbl.find_opt rvar_to_rgen id with
                | Some rgen_idx -> add_evidence rgen_idx (List.map fst overrides)
                | None -> ())
             | _ -> ())
          | _ -> ())
       | TEVar name ->
         (* Propagate evidence from called functions *)
         (match List.assoc_opt name vars with
          | Some callee_sch when callee_sch.Types.record_evidences <> [] ->
            let rgen_map = build_rgen_map callee_sch.Types.body te.ty in
            List.iter (fun (re : Types.record_evidence) ->
              match Hashtbl.find_opt rgen_map re.re_rgen with
              | Some arow ->
                let rec is_open r = match Types.rrow_repr r with
                  | Types.RRow (_, _, tail) -> is_open tail
                  | Types.RVar { contents = Types.RUnbound _ } -> true
                  | _ -> false
                in
                if is_open arow then
                  (match get_row_tail arow with
                   | Types.RVar { contents = Types.RUnbound (id, _) } ->
                     (match Hashtbl.find_opt rvar_to_rgen id with
                      | Some caller_rgen -> add_evidence caller_rgen re.re_fields
                      | None -> ())
                   | _ -> ())
              | None -> ()
            ) callee_sch.Types.record_evidences
          | _ -> ())
       | _ -> ());
      walk_children te
    and walk_children te =
      match te.expr with
      | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEByte _
      | TERune _ | TEUnit | TEVar _ | TENil | TEContinueLoop -> ()
      | TELet (_, _, e1, e2) | TELetRec (_, _, e1, e2) | TELetMut (_, e1, e2) ->
        walk e1; walk e2
      | TEBinop (_, e1, e2) | TECons (e1, e2) | TESeq (e1, e2)
      | TEWhile (e1, e2) | TEResume (e1, e2)
      | TEFieldAssign (e1, _, e2) | TEIndex (e1, e2) ->
        walk e1; walk e2
      | TEApp (fn, arg) -> walk fn; walk arg
      | TEPerform (_, e) | TEUnop (_, e) | TEFun (_, e, _) | TEField (e, _) | TEBreak e
      | TEFoldContinue e | TEForLoop e | TEReturn e | TEAssign (_, e) ->
        walk e
      | TEIf (c, t, e) -> walk c; walk t; walk e
      | TETuple es | TEArray es -> List.iter walk es
      | TEMap kvs -> List.iter (fun (k, v) -> walk k; walk v) kvs
      | TERecord fields -> List.iter (fun (_, e) -> walk e) fields
      | TERecordUpdate (base, overrides) -> walk base; List.iter (fun (_, e) -> walk e) overrides
      | TERecordUpdateIdx (base, pairs) -> walk base; List.iter (fun (i, v) -> walk i; walk v) pairs
      | TEConstruct (_, arg) -> Option.iter walk arg
      | TEMatch (scrutinee, arms, _) ->
        walk scrutinee;
        List.iter (fun (_, guard, body) -> Option.iter walk guard; walk body) arms
      | TEHandle (body, arms) ->
        walk body;
        List.iter (fun arm -> match arm with
          | THReturn (_, e) -> walk e
          | THOp (_, _, _, e) -> walk e) arms
      | TELetRecAnd (bindings, body) ->
        List.iter (fun (_, e) -> walk e) bindings; walk body
    in
    walk te;
    let ev_list = Hashtbl.fold (fun rgen_idx fields acc ->
      { Types.re_fields = List.sort String.compare fields; re_rgen = rgen_idx } :: acc
    ) found_evidences [] in
    if ev_list = [] then scheme
    else { scheme with Types.record_evidences = ev_list }
  end

(* Check if a scheme needs constraint/evidence transformation *)
let scheme_needs_xform (scheme : Types.scheme) =
  scheme.constraints <> [] || scheme.record_evidences <> []

(* Fire fundep improvements on class method calls within an expression.
   Called before generalization so fundep-determined type variables get
   resolved to concrete types and won't be over-generalized in let bindings.
   When ~vars is provided, also improves fundeps for constrained function calls
   (not just direct class method references). *)
let improve_fundeps_in_expr vars type_env te =
  let classes = type_env.Types.classes in
  let instances = type_env.Types.instances in
  let improve_constrained_call_with_scheme sch ty =
      (* Map scheme TGens to actual instantiated tvars *)
      let tgen_to_actual = Hashtbl.create sch.Types.quant in
      let rec map_tgens s a =
        let a = Types.repr a in
        match s with
        | Types.TGen i ->
          if not (Hashtbl.mem tgen_to_actual i) then Hashtbl.replace tgen_to_actual i a
        | Types.TArrow (s1, _, s2) ->
          (match a with Types.TArrow (a1, _, a2) -> map_tgens s1 a1; map_tgens s2 a2 | _ -> ())
        | Types.TCont (s1, _, s2) ->
          (match a with Types.TCont (a1, _, a2) -> map_tgens s1 a1; map_tgens s2 a2 | _ -> ())
        | Types.TTuple ss ->
          (match a with Types.TTuple aa when List.length ss = List.length aa ->
            List.iter2 map_tgens ss aa | _ -> ())
        | Types.TList s1 -> (match a with Types.TList a1 -> map_tgens s1 a1 | _ -> ())
        | Types.TArray s1 -> (match a with Types.TArray a1 -> map_tgens s1 a1 | _ -> ())
        | Types.TMap (sk, sv) ->
          (match a with Types.TMap (ak, av) -> map_tgens sk ak; map_tgens sv av | _ -> ())
        | Types.TVariant (_, ss) ->
          (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
            List.iter2 map_tgens ss aa | _ -> ())
        | _ -> ()
      in
      map_tgens sch.Types.body ty;
      (* For each constraint on a fundep class, try to resolve dependent type vars *)
      List.iter (fun (cc : Types.class_constraint) ->
        match List.find_opt (fun cls -> String.equal cls.Types.class_name cc.cc_class) classes with
        | None -> ()
        | Some class_def when class_def.Types.class_fundeps = [] -> ()
        | Some class_def ->
          let num_params = List.length class_def.Types.class_params in
          (* Build the partial type list for fundep improvement:
             resolve each class arg to a concrete type if possible *)
          let partial = List.map (fun (ca : Types.class_arg) ->
            match ca with
            | CATGen tgen_idx ->
              (match Hashtbl.find_opt tgen_to_actual tgen_idx with
               | Some (Types.TVar { contents = Types.Unbound _ }) -> None
               | Some t -> Some t
               | None -> None)
            | CATy ty ->
              (match Types.repr ty with
               | Types.TVar { contents = Types.Unbound _ } -> None
               | t -> Some t)
            | CAWild -> None  (* will be determined by fundep *)
            | CAPhantom _ -> None  (* will be determined by fundep *)
          ) cc.cc_args in
          let improved = Types.improve_with_fundeps instances class_def partial in
          (* Unify the improved types with unresolved tvars *)
          List.iteri (fun i opt ->
            if i < num_params then
            match opt, List.nth partial i with
            | Some resolved_ty, None ->
              let ca = List.nth cc.cc_args i in
              let tvar_opt = match ca with
                | Types.CATGen tgen_idx -> Hashtbl.find_opt tgen_to_actual tgen_idx
                | Types.CATy ty -> Some ty
                | Types.CAWild -> None
                | Types.CAPhantom _ -> None
              in
              (match tvar_opt with
               | Some (Types.TVar ({ contents = Types.Unbound _ } as r)) ->
                 (try Types.unify (Types.TVar r) resolved_ty with _ -> ())
               | _ -> ())
            | _ -> ()
          ) improved
      ) sch.Types.constraints
  in
  let improve_method_call name ty =
    let (class_opt, method_name) =
      match Types.find_method_class classes name with
      | Some _ as result -> (result, name)
      | None ->
        match String.rindex_opt name '.' with
        | None -> (None, name)
        | Some i ->
          let mod_prefix = String.sub name 0 (i + 1) in
          let short = String.sub name (i + 1) (String.length name - i - 1) in
          let class_prefix = String.sub name 0 i in
          (match Types.find_method_class classes short with
           | Some class_def when String.equal class_def.Types.class_name class_prefix ->
             (Some class_def, short)
           | Some class_def when String.length class_def.Types.class_name > String.length mod_prefix
               && String.sub class_def.Types.class_name 0 (String.length mod_prefix) = mod_prefix ->
             (Some class_def, short)
           | _ -> (None, name))
    in
    match class_opt with
    | Some class_def when class_def.Types.class_fundeps <> [] ->
      let method_schema_ty = List.assoc method_name class_def.Types.class_methods in
      let num_params = List.length class_def.Types.class_params in
      let found = Hashtbl.create num_params in
      let rec go s r =
        let r = Types.repr r in
        match s, r with
        | Types.TGen i, _ when i < num_params ->
          if not (Hashtbl.mem found i) then Hashtbl.replace found i r
        | Types.TArrow (s1, _, s2), Types.TArrow (r1, _, r2)
        | Types.TCont (s1, _, s2), Types.TCont (r1, _, r2) -> go s1 r1; go s2 r2
        | Types.TTuple ss, Types.TTuple rs when List.length ss = List.length rs ->
          List.iter2 go ss rs
        | Types.TList s1, Types.TList r1 -> go s1 r1
        | Types.TArray s1, Types.TArray r1 -> go s1 r1
        | Types.TMap (sk, sv), Types.TMap (rk, rv) -> go sk rk; go sv rv
        | Types.TVariant (_, ss), Types.TVariant (_, rs) when List.length ss = List.length rs ->
          List.iter2 go ss rs
        | _ -> ()
      in
      go method_schema_ty ty;
      let partial = List.init num_params (fun i ->
        match Hashtbl.find_opt found i with
        | Some (Types.TVar { contents = Types.Unbound _ }) -> None
        | Some t -> Some t
        | None -> None
      ) in
      let improved = Types.improve_with_fundeps instances class_def partial in
      List.iteri (fun i opt ->
        match opt, List.nth partial i with
        | Some resolved_ty, None ->
          (match Hashtbl.find_opt found i with
           | Some (Types.TVar ({ contents = Types.Unbound _ } as r)) ->
             (try Types.unify (Types.TVar r) resolved_ty with _ -> ())
           | _ -> ())
        | _ -> ()
      ) improved
    | _ -> ()
  in
  let rec walk locals te =
    (match te.expr with
     | TEVar name ->
       improve_method_call name te.ty;
       improve_constrained_call_local locals name te.ty
     | _ -> ());
    walk_children locals te
  and improve_constrained_call_local locals name ty =
    (* Check local let-bound constrained schemes first, then global vars *)
    let scheme =
      match List.assoc_opt name locals with
      | Some _ as s -> s
      | None ->
        match List.assoc_opt name vars with
        | Some s when s.Types.constraints <> [] -> Some s
        | _ -> None
    in
    match scheme with
    | None -> ()
    | Some sch -> improve_constrained_call_with_scheme sch ty
  and walk_children locals te =
    match te.expr with
    | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEByte _ | TERune _
    | TEUnit | TEVar _ | TENil -> ()
    | TELet (name, sch_opt, e1, e2) ->
      walk locals e1;
      let locals' = match sch_opt with
        | Some sch when sch.Types.constraints <> [] -> (name, sch) :: locals
        | _ -> locals
      in
      walk locals' e2
    | TELetRec (name, sch_opt, e1, e2) ->
      let locals' = match sch_opt with
        | Some sch when sch.Types.constraints <> [] -> (name, sch) :: locals
        | _ -> locals
      in
      walk locals' e1; walk locals' e2
    | TELetMut (_, e1, e2) ->
      walk locals e1; walk locals e2
    | TEFun (_, body, _) -> walk locals body
    | TEApp (f, a) -> walk locals f; walk locals a
    | TEIf (c, t, e) -> walk locals c; walk locals t; walk locals e
    | TEBinop (_, l, r) -> walk locals l; walk locals r
    | TEUnop (_, e) -> walk locals e
    | TETuple es -> List.iter (walk locals) es
    | TERecord fs -> List.iter (fun (_, e) -> walk locals e) fs
    | TERecordUpdate (base, overrides) -> walk locals base; List.iter (fun (_, e) -> walk locals e) overrides
    | TERecordUpdateIdx (base, pairs) -> walk locals base; List.iter (fun (i, v) -> walk locals i; walk locals v) pairs
    | TEField (e, _) -> walk locals e
    | TEIndex (e, i) -> walk locals e; walk locals i
    | TECons (h, t) -> walk locals h; walk locals t
    | TEConstruct (_, arg) -> Option.iter (walk locals) arg
    | TEMatch (s, arms, _) ->
      walk locals s; List.iter (fun (_, g, b) -> Option.iter (walk locals) g; walk locals b) arms
    | TEAssign (_, e) -> walk locals e
    | TEFieldAssign (e, _, v) -> walk locals e; walk locals v
    | TESeq (e1, e2) -> walk locals e1; walk locals e2
    | TEArray es -> List.iter (walk locals) es
    | TEMap kvs -> List.iter (fun (k, v) -> walk locals k; walk locals v) kvs
    | TEWhile (c, b) -> walk locals c; walk locals b
    | TEForLoop e -> walk locals e
    | TEReturn e -> walk locals e
    | TEBreak e -> walk locals e
    | TEContinueLoop | TEFoldContinue _ -> ()
    | TEPerform (_, e) -> walk locals e
    | TEHandle (e, arms) ->
      walk locals e; List.iter (fun arm -> match arm with
        | THReturn (_, body) -> walk locals body
        | THOp (_, _, _, body) -> walk locals body) arms
    | TEResume (e1, e2) -> walk locals e1; walk locals e2
    | TELetRecAnd (bindings, body) ->
      List.iter (fun (_, e) -> walk locals e) bindings; walk locals body
  in
  walk [] te

(* ---- Value restriction ---- *)

let rec is_syntactic_value (expr : Ast.expr) : bool =
  match expr with
  | Ast.EInt _ | Ast.EFloat _ | Ast.EBool _ | Ast.EString _
  | Ast.EByte _ | Ast.ERune _ | Ast.EUnit -> true
  | Ast.EVar _ -> true
  | Ast.EFun _ -> true
  | Ast.ENil -> true
  | Ast.EConstruct (_, None) -> true
  | Ast.EConstruct (_, Some e) -> is_syntactic_value e
  | Ast.ETuple es | Ast.EList es -> List.for_all is_syntactic_value es
  | Ast.ERecord fields -> List.for_all (fun (_, e) -> is_syntactic_value e) fields
  | Ast.ECons (a, b) -> is_syntactic_value a && is_syntactic_value b
  | Ast.EPolyVariant (_, None) -> true
  | Ast.EPolyVariant (_, Some e) -> is_syntactic_value e
  | Ast.EAnnot (e, _) | Ast.ECoerce (e, _) | Ast.ELoc (_, e) -> is_syntactic_value e
  | _ -> false

let mono_scheme ty =
  { Types.quant = 0; equant = 0; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = ty }

(* ---- Synthesize ---- *)

let rec synth ctx level (expr : Ast.expr) : texpr =
  match expr with
  | Ast.ELoc (loc, inner) -> current_loc := loc; synth ctx level inner
  | Ast.EInt n -> mk (TEInt n) Types.TInt
  | Ast.EFloat f -> mk (TEFloat f) Types.TFloat
  | Ast.EBool b -> mk (TEBool b) Types.TBool
  | Ast.EString s -> mk (TEString s) Types.TString
  | Ast.EByte n -> mk (TEByte n) Types.TByte
  | Ast.ERune n -> mk (TERune n) Types.TRune
  | Ast.EUnit -> mk TEUnit Types.TUnit
  | Ast.ENil ->
    mk TENil (Types.TList (Types.new_tvar level))
  | Ast.EVar name ->
    let ty = lookup_var ctx level name in
    let resolved_name = resolve_module_name ctx name in
    mk (TEVar resolved_name) ty
  | Ast.EBinop (op, e1, e2) -> synth_binop ctx level op e1 e2
  | Ast.EUnop (op, e) -> synth_unop ctx level op e
  | Ast.EApp (fn, arg) ->
    let fn_te = synth ctx level fn in
    let arg_te = synth ctx level arg in
    let ret_ty = Types.new_tvar level in
    let call_eff = Types.new_effvar level in
    try_unify fn_te.ty (Types.TArrow (arg_te.ty, call_eff, ret_ty));
    try_subeffect call_eff ctx.current_eff;
    mk (TEApp (fn_te, arg_te)) ret_ty
  | Ast.EFun (param, body) ->
    let param_ty = match param.annot with
      | Some annot -> resolve_ty_annot ctx level annot
      | None -> Types.new_tvar level
    in
    let body_eff = Types.new_effvar level in
    let ret_ty = Types.new_tvar level in
    let ctx' = if param.is_generated then
      (* Generated callback (e.g. for-loop): preserve outer return_type *)
      { ctx with current_eff = body_eff }
    else begin
      (* User function: fresh return scope *)
      let return_used = ref false in
      { ctx with current_eff = body_eff; return_type = Some ret_ty;
        inside_handler = false; return_used }
    end in
    let ctx' = extend_var_mono ctx' param.name param_ty in
    let body_te = synth ctx' level body in
    try_unify body_te.ty ret_ty;
    let has_return = if param.is_generated then false
      else !(ctx'.return_used) in
    mk (TEFun (param.name, body_te, has_return)) (Types.TArrow (param_ty, body_eff, ret_ty))
  | Ast.ELet (name, e1, e2) ->
    let e1_te = synth ctx (level + 1) e1 in
    improve_fundeps_in_expr ctx.vars ctx.type_env e1_te;
    let scheme = if is_syntactic_value e1 then Types.generalize level e1_te.ty
                 else mono_scheme e1_te.ty in
    let scheme = infer_implicit_constraints "" ctx.type_env ctx.vars e1_te scheme in
    let scheme = infer_record_evidence ctx.vars e1_te scheme in
    let stored_scheme = if scheme_needs_xform scheme then Some scheme else None in
    let ctx' = extend_var ctx name scheme in
    let e2_te = synth ctx' level e2 in
    mk (TELet (name, stored_scheme, e1_te, e2_te)) e2_te.ty
  | Ast.ELetMut (name, e1, e2) ->
    let e1_te = synth ctx (level + 1) e1 in
    (* Mutable variables must not be generalized (value restriction):
       each reference must share the same type, not get a fresh instantiation *)
    let scheme = { Types.quant = 0; equant = 0; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = e1_te.ty } in
    let ctx' = extend_var_mutable ctx name scheme in
    let e2_te = synth ctx' level e2 in
    mk (TELetMut (name, e1_te, e2_te)) e2_te.ty
  | Ast.EAssign (name, e) ->
    if not (List.mem name ctx.mutable_vars) then
      error (Printf.sprintf "variable %s is not mutable" name);
    let var_ty = lookup_var ctx level name in
    let e_te = check ctx level e var_ty in
    let resolved_name = resolve_module_name ctx name in
    mk (TEAssign (resolved_name, e_te)) Types.TUnit
  | Ast.EFieldAssign (record_expr, field, value_expr) ->
    let r_te = synth ctx level record_expr in
    (* Use row unification: unify with a record that has at least 'field' *)
    let field_ty = Types.new_tvar level in
    let row_tail = Types.new_rvar level in
    (* Try nominal resolution first for better errors *)
    (match Types.repr r_te.ty with
     | Types.TVar _ ->
       let candidates = List.filter (fun (_name, fields) ->
         List.mem_assoc field fields
       ) ctx.type_env.Types.records in
       (match candidates with
        | [(_name, fields)] ->
          let row = instantiate_record_row level fields in
          try_unify r_te.ty (Types.TRecord row)
        | _ ->
          try_unify r_te.ty (Types.TRecord (Types.RRow (field, field_ty, row_tail))))
     | _ ->
       try_unify r_te.ty (Types.TRecord (Types.RRow (field, field_ty, row_tail))));
    (* Now extract the field type from the unified record *)
    let actual_field_ty = match Types.repr r_te.ty with
      | Types.TRecord row ->
        let fields = Types.record_row_to_fields row in
        (match List.assoc_opt field fields with
         | Some ty -> ty
         | None -> error (Printf.sprintf "record has no field %s" field))
      | _ -> error "field assignment on non-record type"
    in
    if not (List.mem field ctx.type_env.mutable_fields) then
      error (Printf.sprintf "field %s is not mutable" field);
    let v_te = check ctx level value_expr actual_field_ty in
    mk (TEFieldAssign (r_te, field, v_te)) Types.TUnit
  | Ast.ELetRec (name, type_params, e1, e2) ->
    (match strip_loc e1 with
     | Ast.EFun _ | Ast.EAnnot (Ast.EFun _, _) ->
       if type_params <> [] then begin
         (* Polymorphic recursion: extract annotations, build scheme, bind polymorphically *)
         let (params, ret_annot, body) = extract_fn_info e1 in
         let (e1_te, scheme) = synth_poly_rec_fn ctx level name "" type_params params ret_annot body in
         let ctx' = extend_var ctx name scheme in
         let e2_te = synth ctx' level e2 in
         mk (TELetRec (name, None, e1_te, e2_te)) e2_te.ty
       end else begin
         let fn_var = Types.new_tvar (level + 1) in
         let ctx' = extend_var_mono ctx name fn_var in
         let e1_te = synth ctx' (level + 1) e1 in
         try_unify fn_var e1_te.ty;
         improve_fundeps_in_expr ctx.vars ctx.type_env e1_te;
         let scheme = Types.generalize level e1_te.ty in
         let scheme = infer_implicit_constraints name ctx.type_env ctx.vars e1_te scheme in
         let scheme = infer_record_evidence ctx.vars e1_te scheme in
         let stored_scheme = if scheme_needs_xform scheme then Some scheme else None in
         let ctx'' = extend_var ctx name scheme in
         let e2_te = synth ctx'' level e2 in
         mk (TELetRec (name, stored_scheme, e1_te, e2_te)) e2_te.ty
       end
     | _ -> error "let rec binding must be a function")
  | Ast.EIf (cond, then_e, else_e) ->
    let cond_te = check ctx level cond Types.TBool in
    let then_te = synth ctx level then_e in
    let else_te = check ctx level else_e then_te.ty in
    mk (TEIf (cond_te, then_te, else_te)) then_te.ty
  | Ast.ETuple exprs ->
    let tes = List.map (synth ctx level) exprs in
    let tys = List.map (fun te -> te.ty) tes in
    mk (TETuple tes) (Types.TTuple tys)
  | Ast.ERecord fields ->
    let typed_fields = List.map (fun (n, e) ->
      let te = synth ctx level e in (n, te)
    ) fields in
    let field_tys = List.map (fun (n, te) -> (n, te.ty)) typed_fields in
    let field_tys = List.sort (fun (a, _) (b, _) -> String.compare a b) field_tys in
    let row = Types.fields_to_closed_row field_tys in
    mk (TERecord typed_fields) (Types.TRecord row)
  | Ast.ERecordUpdate (base, overrides) ->
    let base_te = synth ctx level base in
    (* Ensure base is a record; if fully unknown, try nominal resolution *)
    (match Types.repr base_te.ty with
     | Types.TVar _ ->
       (* Try nominal resolution when base type is completely unknown *)
       let override_names = List.map fst overrides in
       let candidates = List.filter (fun (_name, rfields) ->
         List.for_all (fun f -> List.mem_assoc f rfields) override_names
       ) ctx.type_env.Types.records in
       (match candidates with
        | [(_, rfields)] ->
          let row = instantiate_record_row level rfields in
          try_unify base_te.ty (Types.TRecord row)
        | _ ->
          (* No unique nominal match — unify with open row containing override fields *)
          let row_tail = Types.new_rvar level in
          let row = List.fold_right (fun name acc ->
            Types.RRow (name, Types.new_tvar level, acc)
          ) override_names row_tail in
          try_unify base_te.ty (Types.TRecord row))
     | Types.TRecord _ -> ()
     | _ -> error "record update on non-record type");
    (* For each override, unify base with { field: 'a | ρ } to ensure field exists *)
    let override_field_tys = List.map (fun (name, _) ->
      let field_ty = Types.new_tvar level in
      let row_tail = Types.new_rvar level in
      try_unify base_te.ty (Types.TRecord (Types.RRow (name, field_ty, row_tail)));
      (* Extract actual field type after unification *)
      let actual_ty = match Types.repr base_te.ty with
        | Types.TRecord row ->
          let fields = Types.record_row_to_fields row in
          (match List.assoc_opt name fields with
           | Some ty -> ty
           | None -> field_ty)
        | _ -> field_ty
      in
      (name, actual_ty)
    ) overrides in
    (* Type-check each override value against the expected field type *)
    let typed_overrides = List.map2 (fun (name, e) (_, expected_ty) ->
      let te = check ctx level e expected_ty in
      (name, te)
    ) overrides override_field_tys in
    (* Check if row is closed or open *)
    let rec has_open_tail row = match Types.rrow_repr row with
      | Types.RRow (_, _, tail) -> has_open_tail tail
      | Types.RVar { contents = Types.RUnbound _ } -> true
      | _ -> false
    in
    let is_open = match Types.repr base_te.ty with
      | Types.TRecord row -> has_open_tail row
      | _ -> false
    in
    if is_open then
      (* Open row: emit TERecordUpdate — compiled to RECORD_UPDATE *)
      mk (TERecordUpdate (base_te, typed_overrides)) base_te.ty
    else begin
      (* Closed row: desugar to TELet + TERecord (all fields known) *)
      let field_tys = match Types.repr base_te.ty with
        | Types.TRecord row -> Types.record_row_to_fields row
        | _ -> error "record update on non-record type"
      in
      let tmp = "__rec_upd" in
      let tmp_ty = base_te.ty in
      let all_fields = List.map (fun (name, ty) ->
        match List.assoc_opt name typed_overrides with
        | Some te -> (name, te)
        | None -> (name, mk (TEField (mk (TEVar tmp) tmp_ty, name)) ty)
      ) field_tys in
      let record_row = Types.fields_to_closed_row field_tys in
      let record_expr = mk (TERecord all_fields) (Types.TRecord record_row) in
      mk (TELet (tmp, None, base_te, record_expr)) (Types.TRecord record_row)
    end
  | Ast.EField (e, field) ->
    let te = synth ctx level e in
    let field_ty = Types.new_tvar level in
    let row_tail = Types.new_rvar level in
    (* Try nominal resolution first when type is unknown *)
    (match Types.repr te.ty with
     | Types.TVar _ ->
       let candidates = List.filter (fun (_name, fields) ->
         List.mem_assoc field fields
       ) ctx.type_env.Types.records in
       (match candidates with
        | [(_name, fields)] ->
          let row = instantiate_record_row level fields in
          try_unify te.ty (Types.TRecord row)
        | _ ->
          (* Row unification: unify with { field: 'a; ..rho } *)
          try_unify te.ty (Types.TRecord (Types.RRow (field, field_ty, row_tail))))
     | _ ->
       (* Row unification handles known records and open records *)
       try_unify te.ty (Types.TRecord (Types.RRow (field, field_ty, row_tail))));
    (* Extract the actual field type from the unified record *)
    let actual_field_ty = match Types.repr te.ty with
      | Types.TRecord row ->
        let fields = Types.record_row_to_fields row in
        (match List.assoc_opt field fields with
         | Some ty -> ty
         | None -> field_ty)
      | _ -> field_ty
    in
    mk (TEField (te, field)) actual_field_ty
  | Ast.EIndex (base, idx) ->
    let te_base = synth ctx level base in
    (match Types.repr te_base.ty with
     | Types.TString ->
       let te_idx = check ctx level idx Types.TInt in
       mk (TEIndex (te_base, te_idx)) Types.TByte
     | Types.TArray t ->
       let te_idx = check ctx level idx Types.TInt in
       mk (TEIndex (te_base, te_idx)) t
     | _ ->
       (* Desugar to Index typeclass: at idx base *)
       let te = synth ctx level (Ast.EApp (Ast.EApp (Ast.EVar "at", idx), base)) in
       (* Apply fundep improvement: if the container type is concrete,
          the Index fundep ('c -> 'k 'v) can determine the result type *)
       (match List.find_opt (fun c -> String.equal c.Types.class_name "Index")
                ctx.type_env.Types.classes with
        | Some class_def when class_def.Types.class_fundeps <> [] ->
          let container_ty = Types.repr te_base.ty in
          let partial = [Some container_ty; None; None] in
          let improved = Types.improve_with_fundeps
            ctx.type_env.Types.instances class_def partial in
          (match List.nth_opt improved 2 with
           | Some (Some val_ty) -> try_unify te.ty val_ty
           | _ -> ())
        | _ -> ());
       te)
  | Ast.ECons (hd, tl) ->
    let hd_te = synth ctx level hd in
    let list_ty = Types.TList hd_te.ty in
    let tl_te = check ctx level tl list_ty in
    mk (TECons (hd_te, tl_te)) list_ty
  | Ast.EList (first :: rest) ->
    let first_te = synth ctx level first in
    let rest_tes = List.map (fun e -> check ctx level e first_te.ty) rest in
    let list_ty = Types.TList first_te.ty in
    mk (TECons (first_te,
      List.fold_right (fun te acc ->
        mk (TECons (te, acc)) list_ty
      ) rest_tes (mk TENil list_ty)
    )) list_ty
  | Ast.EList [] ->
    mk TENil (Types.TList (Types.new_tvar level))
  | Ast.EArray (first :: rest) ->
    let first_te = synth ctx level first in
    let rest_tes = List.map (fun e -> check ctx level e first_te.ty) rest in
    mk (TEArray (first_te :: rest_tes)) (Types.TArray first_te.ty)
  | Ast.EArray [] ->
    mk (TEArray []) (Types.TArray (Types.new_tvar level))
  | Ast.EConstruct (name, arg) ->
    synth_construct ctx level name arg
  | Ast.EMatch (scrut, arms, partial) ->
    synth_match None ctx level scrut arms partial
  | Ast.ESeq (e1, e2) ->
    let e1_te = synth ctx level e1 in
    let e2_te = synth ctx level e2 in
    mk (TESeq (e1_te, e2_te)) e2_te.ty
  | Ast.EAnnot (e, annot) ->
    let ty = resolve_ty_annot ctx level annot in
    let te = check ctx level e ty in
    te
  | Ast.EPolyVariant (tag, None) ->
    let tail = Types.new_pvvar level in
    let row = Types.PVRow (tag, None, tail) in
    mk (TEConstruct ("`" ^ tag, None)) (Types.TPolyVariant row)
  | Ast.EPolyVariant (tag, Some arg) ->
    let arg_te = synth ctx level arg in
    let tail = Types.new_pvvar level in
    let row = Types.PVRow (tag, Some arg_te.ty, tail) in
    mk (TEConstruct ("`" ^ tag, Some arg_te)) (Types.TPolyVariant row)
  | Ast.ECoerce (inner, target_annot) ->
    let inner_te = synth ctx level inner in
    let target_ty = resolve_ty_annot ctx level target_annot in
    try_unify inner_te.ty target_ty;
    { inner_te with ty = target_ty }
  | Ast.EPerform (op_name, arg) ->
    synth_perform ctx level op_name arg
  | Ast.EHandle (body, arms) ->
    synth_handle ctx level body arms
  | Ast.EWhile (cond, body) ->
    let ctx' = { ctx with loop_info = Some WhileLoop } in
    let cond_te = check ctx' level cond Types.TBool in
    let body_te = check ctx' level body Types.TUnit in
    mk (TEWhile (cond_te, body_te)) Types.TUnit
  | Ast.EFor (var_name, coll_expr, body_expr) ->
    (* Desugar: TEForLoop(fold (fun _ x -> body; ()) () coll) *)
    let ctx' = { ctx with loop_info = Some UnitLoop } in
    let desugared = Ast.EApp(Ast.EApp(Ast.EApp(Ast.EVar "fold",
      Ast.EFun({name="_"; annot=None; is_generated=true},
        Ast.EFun({name=var_name; annot=None; is_generated=true},
          Ast.ESeq(body_expr, Ast.EUnit)))),
      Ast.EUnit),
      coll_expr) in
    let fold_te = synth ctx' level desugared in
    mk (TEForLoop fold_te) Types.TUnit
  | Ast.EForFold (var_name, coll_expr, acc_name, init_expr, body_expr) ->
    (* Desugar: TEForLoop(fold (fun acc x -> body) init coll) *)
    let ctx' = { ctx with loop_info = Some (FoldLoop acc_name) } in
    let desugared = Ast.EApp(Ast.EApp(Ast.EApp(Ast.EVar "fold",
      Ast.EFun({name=acc_name; annot=None; is_generated=true},
        Ast.EFun({name=var_name; annot=None; is_generated=true},
          body_expr))),
      init_expr),
      coll_expr) in
    let fold_te = synth ctx' level desugared in
    mk (TEForLoop fold_te) fold_te.ty
  | Ast.EBreak value_opt ->
    (match ctx.loop_info with
     | None -> error "break outside of loop"
     | Some WhileLoop ->
       if value_opt <> None then error "break with value only allowed in fold loops";
       mk (TEBreak (mk TEUnit Types.TUnit)) Types.TUnit
     | Some UnitLoop ->
       if value_opt <> None then error "break with value only allowed in fold loops";
       mk (TEBreak (mk TEUnit Types.TUnit)) Types.TUnit
     | Some (FoldLoop acc_name) ->
       let v_te = match value_opt with
         | Some e -> synth ctx level e
         | None -> synth ctx level (Ast.EVar acc_name) in
       mk (TEBreak v_te) (Types.new_tvar level))
  | Ast.EContinueLoop ->
    (match ctx.loop_info with
     | None -> error "continue outside of loop"
     | Some WhileLoop -> mk TEContinueLoop Types.TUnit
     | Some UnitLoop -> mk (TEFoldContinue (mk TEUnit Types.TUnit)) Types.TUnit
     | Some (FoldLoop acc_name) ->
       let acc_te = synth ctx level (Ast.EVar acc_name) in
       mk (TEFoldContinue acc_te) (Types.new_tvar level))
  | Ast.EReturn e ->
    (match ctx.return_type with
     | None -> error "return outside of function"
     | Some ret_ty ->
       if ctx.inside_handler then
         error "Type error: return inside try/with is not supported";
       ctx.return_used := true;
       let e_te = synth ctx level e in
       try_unify e_te.ty ret_ty;
       mk (TEReturn e_te) (Types.new_tvar level))
  | Ast.EWhileLet (pat, scrutinee, body) ->
    (* Desugar: for true do match scrutinee with | pat -> body | _ -> break done *)
    let desugared = Ast.EWhile (Ast.EBool true,
      Ast.EMatch (scrutinee,
        [(pat, None, body);
         (Ast.PatWild, None, Ast.EBreak None)],
        false)) in
    synth ctx level desugared
  | Ast.EResume (k_expr, v_expr) ->
    let k_te = synth ctx level k_expr in
    let v_te = synth ctx level v_expr in
    let ret_ty = Types.new_tvar level in
    let call_eff = Types.new_effvar level in
    try_unify k_te.ty (Types.TCont (v_te.ty, call_eff, ret_ty));
    try_subeffect call_eff ctx.current_eff;
    mk (TEResume (k_te, v_te)) ret_ty
  | Ast.EMap [] ->
    let kt = Types.new_tvar level in
    let vt = Types.new_tvar level in
    mk (TEMap []) (Types.TMap (kt, vt))
  | Ast.EMap ((k1, v1) :: rest) ->
    let k1_te = synth ctx level k1 in
    let v1_te = synth ctx level v1 in
    let rest_tes = List.map (fun (k, v) ->
      let kt = check ctx level k k1_te.ty in
      let vt = check ctx level v v1_te.ty in
      (kt, vt)
    ) rest in
    mk (TEMap ((k1_te, v1_te) :: rest_tes)) (Types.TMap (k1_te.ty, v1_te.ty))
  | Ast.EMapTyped (mod_name, pairs) ->
    let pair_list = Ast.EList (List.map (fun (k, v) -> Ast.ETuple [k; v]) pairs) in
    let desugared = Ast.EApp (Ast.EVar (mod_name ^ ".of_list"), pair_list) in
    synth ctx level desugared
  | Ast.ESet elems ->
    let list_expr = Ast.EList elems in
    let desugared = Ast.EApp (Ast.EVar "Set.of_list", list_expr) in
    synth ctx level desugared
  | Ast.ECollTyped (mod_name, elems) ->
    let list_expr = Ast.EList elems in
    let desugared = Ast.EApp (Ast.EVar (mod_name ^ ".of_list"), list_expr) in
    synth ctx level desugared
  | Ast.ELocalOpen (mod_name, inner_expr) ->
    (* Look up module, desugar to let bindings for each pub var *)
    let minfo = match !find_module_in_env_ref ctx.type_env mod_name with
      | Some m -> m
      | None -> error (Printf.sprintf "unknown module: %s" mod_name)
    in
    (* Wrap inner_expr with let bindings: let x = M.x in let y = M.y in ... expr *)
    let desugared = List.fold_left (fun body (short, _scheme) ->
      let qualified = mod_name ^ "." ^ short in
      Ast.ELet (short, Ast.EVar qualified, body)
    ) inner_expr minfo.Types.mod_pub_vars in
    synth ctx level desugared
  | Ast.ELetRecAnd (bindings, body) ->
    let has_poly = List.exists (fun (_, tp, _) -> tp <> []) bindings in
    if has_poly then begin
      (* Polymorphic recursion path *)
      let infos = List.map (fun (name, type_params, fn_expr) ->
        let (params, ret_annot, inner_body) = extract_fn_info fn_expr in
        (name, type_params, params, ret_annot, inner_body)
      ) bindings in
      let tvars_list = List.map (fun (_, type_params, params, ret_annot, _) ->
        let shared_tvars = Hashtbl.create 4 in
        List.iter (fun tp ->
          Hashtbl.replace shared_tvars tp (Types.new_tvar (level + 1))
        ) type_params;
        let param_tys = List.map (fun (p : Ast.param) -> match p.annot with
          | Some annot -> resolve_ty_annot_shared ctx (level + 1) shared_tvars annot
          | None -> Types.new_tvar (level + 1)) params in
        let (ret_ty, body_eff) = match ret_annot with
          | Some (Ast.TyWithEffect (ty, eff_annot)) ->
            let ty' = resolve_ty_annot_shared ctx (level + 1) shared_tvars ty in
            let eff = match eff_annot with
              | Ast.EffAnnotPure -> Types.EffEmpty
              | Ast.EffAnnotRow items -> resolve_eff_items ctx (level + 1) shared_tvars items in
            (ty', eff)
          | Some annot ->
            (resolve_ty_annot_shared ctx (level + 1) shared_tvars annot,
             Types.new_effvar (level + 1))
          | None -> (Types.new_tvar (level + 1), Types.new_effvar (level + 1)) in
        let fn_ty = match List.rev param_tys with
          | [] -> ret_ty
          | last :: rest ->
            let inner = Types.TArrow (last, body_eff, ret_ty) in
            List.fold_left (fun acc pty -> Types.TArrow (pty, Types.EffEmpty, acc)) inner rest in
        (param_tys, ret_ty, body_eff, fn_ty)
      ) infos in
      let schemes = List.map (fun (_, _, _, fn_ty) -> Types.generalize level fn_ty) tvars_list in
      let ctx' = List.fold_left2 (fun ctx (name, _, _, _, _) scheme ->
        extend_var ctx name scheme) ctx infos schemes in
      let body_tes = List.map2 (fun (_, _, params, _, inner_body) (param_tys, ret_ty, body_eff, _) ->
        let inner_ctx = List.fold_left2 (fun c (p : Ast.param) ty ->
          extend_var_mono c p.name ty) { ctx' with current_eff = body_eff } params param_tys in
        let body_te = check inner_ctx (level + 1) inner_body ret_ty in
        match List.rev params, List.rev param_tys with
        | [], [] -> body_te
        | last_p :: rest_ps, last_pty :: rest_ptys ->
          let inner = mk (TEFun (last_p.Ast.name, body_te, false))
            (Types.TArrow (last_pty, body_eff, body_te.ty)) in
          List.fold_left2 (fun acc (p : Ast.param) pty ->
            mk (TEFun (p.name, acc, false)) (Types.TArrow (pty, Types.EffEmpty, acc.ty))
          ) inner rest_ps rest_ptys
        | _ -> assert false
      ) infos tvars_list in
      let ctx'' = List.fold_left2 (fun ctx (name, _, _, _, _) scheme ->
        extend_var ctx name scheme) ctx infos schemes in
      let body_te = synth ctx'' level body in
      let typed_bindings = List.map2 (fun (name, _, _, _, _) te -> (name, te)) infos body_tes in
      mk (TELetRecAnd (typed_bindings, body_te)) body_te.ty
    end else begin
      (* Original monomorphic path *)
      let fn_vars = List.map (fun (name, _, _) -> (name, Types.new_tvar (level + 1))) bindings in
      let ctx' = List.fold_left (fun ctx (name, tv) ->
        extend_var_mono ctx name tv
      ) ctx fn_vars in
      let body_tes = List.map2 (fun (_, _, fn_expr) (_, tv) ->
        let te = synth ctx' (level + 1) fn_expr in
        try_unify tv te.ty;
        te
      ) bindings fn_vars in
      let ctx'' = List.fold_left2 (fun ctx (name, _, _) (_, tv) ->
        let scheme = Types.generalize level tv in
        extend_var ctx name scheme
      ) ctx bindings fn_vars in
      let body_te = synth ctx'' level body in
      let typed_bindings = List.map2 (fun (name, _, _) te -> (name, te)) bindings body_tes in
      mk (TELetRecAnd (typed_bindings, body_te)) body_te.ty
    end

and synth_perform ctx level op_name arg =
  match Types.find_effect_op ctx.type_env op_name with
  | None -> error (Printf.sprintf "unknown effect operation: %s" op_name)
  | Some (effect_name, op_ty) ->
    (* Find the effect definition for type params *)
    let edef = List.find (fun e -> String.equal e.Types.effect_name effect_name)
      ctx.type_env.Types.effects in
    (* Create fresh type vars for each effect type parameter *)
    let fresh_params = List.map (fun _ -> Types.new_tvar level) edef.Types.effect_params in
    (* Freshen any free type variables that are NOT effect type parameters
       (e.g. 'a in: op : string -> 'a when 'a is not an effect type parameter).
       Must freshen BEFORE subst_tgens so effect param tvars stay linked. *)
    let concrete_op_ty = freshen_tvars level op_ty in
    (* Substitute TGen indices with fresh params *)
    let concrete_op_ty = if fresh_params = [] then concrete_op_ty
      else subst_tgens fresh_params concrete_op_ty in
    (match Types.repr concrete_op_ty with
     | Types.TArrow (param_ty, _, ret_ty) ->
       let arg_te = check ctx level arg param_ty in
       (* Add this effect to the ambient, with type params *)
       let fresh_tail = Types.new_effvar level in
       try_unify_eff ctx.current_eff (Types.EffRow (effect_name, fresh_params, fresh_tail));
       mk (TEPerform (op_name, arg_te)) ret_ty
     | _ -> error (Printf.sprintf "effect operation %s must have function type" op_name))

and synth_handle ctx level body arms =
  (* Body gets a fresh effect — it can perform effects that we'll handle *)
  let body_eff = Types.new_effvar level in
  let body_ctx = { ctx with current_eff = body_eff; inside_handler = true } in
  let body_te = synth body_ctx level body in
  let result_ty = Types.new_tvar level in

  (* Collect handled effect names from op arms *)
  let handled_effects = List.filter_map (fun arm ->
    match arm with
    | Ast.HOp (op_name, _, _, _) ->
      (match Types.find_effect_op ctx.type_env op_name with
       | Some (effect_name, _) -> Some effect_name
       | None -> error (Printf.sprintf "unknown effect operation in handler: %s" op_name))
    | _ -> None
  ) arms in

  (* For each handled effect, create fresh type params (shared across ops from same effect) *)
  let unique_effects = List.sort_uniq String.compare handled_effects in
  let effect_fresh_params = List.map (fun effect_name ->
    let edef = List.find (fun e -> String.equal e.Types.effect_name effect_name)
      ctx.type_env.Types.effects in
    let fresh_params = List.map (fun _ -> Types.new_tvar level) edef.Types.effect_params in
    (effect_name, fresh_params)
  ) unique_effects in

  (* Remove handled effects from body_eff, remainder flows to outer ambient *)
  let remaining_eff = List.fold_left (fun eff (effect_name, fresh_params) ->
    try Types.rewrite_row effect_name fresh_params eff
    with Types.Unify_error msg -> error msg
  ) (Types.eff_repr body_eff) effect_fresh_params in
  try_unify_eff remaining_eff ctx.current_eff;

  (* Type the handler arms *)
  let typed_arms = List.map (fun arm ->
    match arm with
    | Ast.HReturn (name, handler_body) ->
      let ctx' = extend_var_mono ctx name body_te.ty in
      let handler_te = check ctx' level handler_body result_ty in
      THReturn (name, handler_te)
    | Ast.HOp (op_name, arg_name, k_name, handler_body) ->
      (match Types.find_effect_op ctx.type_env op_name with
       | None -> error (Printf.sprintf "unknown effect operation in handler: %s" op_name)
       | Some (effect_name, op_ty) ->
         (* Substitute effect type params (TGen) with fresh vars *)
         let fresh_params = match List.assoc_opt effect_name effect_fresh_params with
           | Some p -> p | None -> [] in
         let concrete_op_ty = if fresh_params = [] then op_ty
           else subst_tgens fresh_params op_ty in
         let param_ty, ret_ty_op = match Types.repr concrete_op_ty with
           | Types.TArrow (a, _, b) -> a, b
           | _ -> error (Printf.sprintf "effect operation %s must have function type" op_name)
         in
         let k_ty = Types.TCont (ret_ty_op, ctx.current_eff, result_ty) in
         let ctx' = extend_var_mono ctx arg_name param_ty in
         let ctx' = extend_var_mono ctx' k_name k_ty in
         let handler_te = check ctx' level handler_body result_ty in
         THOp (op_name, arg_name, k_name, handler_te))
  ) arms in
  mk (TEHandle (body_te, typed_arms)) result_ty

and synth_binop ctx level op e1 e2 =
  match op with
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
    (* Unified: works for int, float, and custom types *)
    let te1 = synth ctx level e1 in
    let te2 = synth ctx level e2 in
    try_unify te1.ty te2.ty;
    let resolved = Types.repr te1.ty in
    (match resolved with
     | Types.TFloat ->
       mk (TEBinop (op, te1, te2)) Types.TFloat
     | Types.TVar { contents = Types.Unbound (id, _) } ->
       if List.mem id ctx.constraint_tvars then
         mk (TEBinop (op, te1, te2)) (Types.repr te1.ty)
       else begin
         try_unify te1.ty Types.TInt;
         mk (TEBinop (op, te1, te2)) Types.TInt
       end
     | Types.TInt ->
       mk (TEBinop (op, te1, te2)) Types.TInt
     | _ ->
       mk (TEBinop (op, te1, te2)) (Types.repr te1.ty))
  | Ast.Mod ->
    let te1 = check ctx level e1 Types.TInt in
    let te2 = check ctx level e2 Types.TInt in
    mk (TEBinop (op, te1, te2)) Types.TInt
  | Ast.Lt | Ast.Gt | Ast.Le | Ast.Ge ->
    let te1 = synth ctx level e1 in
    let te2 = synth ctx level e2 in
    try_unify te1.ty te2.ty;
    mk (TEBinop (op, te1, te2)) Types.TBool
  | Ast.Eq | Ast.Neq ->
    let te1 = synth ctx level e1 in
    let te2 = synth ctx level e2 in
    try_unify te1.ty te2.ty;
    mk (TEBinop (op, te1, te2)) Types.TBool
  | Ast.And | Ast.Or ->
    let te1 = check ctx level e1 Types.TBool in
    let te2 = check ctx level e2 Types.TBool in
    mk (TEBinop (op, te1, te2)) Types.TBool
  | Ast.Concat ->
    let te1 = check ctx level e1 Types.TString in
    let te2 = check ctx level e2 Types.TString in
    mk (TEBinop (op, te1, te2)) Types.TString
  | Ast.Land | Ast.Lor | Ast.Lxor | Ast.Lsl | Ast.Lsr ->
    let te1 = synth ctx level e1 in
    let te2 = synth ctx level e2 in
    try_unify te1.ty te2.ty;
    let resolved = Types.repr te1.ty in
    (match resolved with
     | Types.TInt ->
       mk (TEBinop (op, te1, te2)) Types.TInt
     | Types.TVar { contents = Types.Unbound (id, _) } ->
       if List.mem id ctx.constraint_tvars then
         mk (TEBinop (op, te1, te2)) (Types.repr te1.ty)
       else begin
         try_unify te1.ty Types.TInt;
         mk (TEBinop (op, te1, te2)) Types.TInt
       end
     | _ ->
       mk (TEBinop (op, te1, te2)) (Types.repr te1.ty))
  | Ast.Pipe ->
    let te1 = synth ctx level e1 in
    let te2 = synth ctx level e2 in
    let ret_ty = Types.new_tvar level in
    let call_eff = Types.new_effvar level in
    try_unify te2.ty (Types.TArrow (te1.ty, call_eff, ret_ty));
    try_subeffect call_eff ctx.current_eff;
    mk (TEBinop (op, te1, te2)) ret_ty

and synth_unop ctx level op e =
  match op with
  | Ast.Neg ->
    let te = synth ctx level e in
    let resolved = Types.repr te.ty in
    (match resolved with
     | Types.TFloat ->
       mk (TEUnop (op, te)) Types.TFloat
     | Types.TVar { contents = Types.Unbound (id, _) } ->
       if List.mem id ctx.constraint_tvars then
         mk (TEUnop (op, te)) (Types.repr te.ty)
       else begin
         try_unify te.ty Types.TInt;
         mk (TEUnop (op, te)) Types.TInt
       end
     | Types.TInt ->
       mk (TEUnop (op, te)) Types.TInt
     | _ ->
       mk (TEUnop (op, te)) te.ty)
  | Ast.Not ->
    let te = check ctx level e Types.TBool in
    mk (TEUnop (op, te)) Types.TBool
  | Ast.Lnot ->
    let te = synth ctx level e in
    let resolved = Types.repr te.ty in
    (match resolved with
     | Types.TInt ->
       mk (TEUnop (op, te)) Types.TInt
     | Types.TVar { contents = Types.Unbound _ } ->
       try_unify te.ty Types.TInt;
       mk (TEUnop (op, te)) Types.TInt
     | _ ->
       mk (TEUnop (op, te)) te.ty)

and qualify_ctor_name type_env name info =
  (* Qualify constructor name using ctor_type_name, but only if the qualified
     version exists in the type_env. This handles the case where constructors
     from other modules (e.g., Token.EQ via open Token) need qualification to
     avoid shadowing, while constructors from the current module's own types
     (e.g., Wrap in an opaque type) remain unqualified since they may not have
     qualified entries in the type_env. *)
  if String.contains name '.' then name
  else match String.rindex_opt info.Types.ctor_type_name '.' with
    | Some i ->
      let qualified = String.sub info.Types.ctor_type_name 0 (i + 1) ^ name in
      if List.assoc_opt qualified type_env.Types.constructors <> None then qualified
      else name
    | None -> name

and qualify_pattern type_env pat =
  let rec go pat = match pat with
    | Ast.PatConstruct (name, arg) ->
      let qname = match List.assoc_opt name type_env.Types.constructors with
        | Some info -> qualify_ctor_name type_env name info
        | None -> name
      in
      Ast.PatConstruct (qname, Option.map go arg)
    | Ast.PatTuple ps -> Ast.PatTuple (List.map go ps)
    | Ast.PatCons (hd, tl) -> Ast.PatCons (go hd, go tl)
    | Ast.PatOr (p1, p2) -> Ast.PatOr (go p1, go p2)
    | Ast.PatAs (p, n) -> Ast.PatAs (go p, n)
    | Ast.PatRecord fields -> Ast.PatRecord (List.map (fun (f, p) -> (f, go p)) fields)
    | Ast.PatArray ps -> Ast.PatArray (List.map go ps)
    | Ast.PatMap kvs -> Ast.PatMap (List.map (fun (k, v) -> (go k, go v)) kvs)
    | Ast.PatAnnot (p, a) -> Ast.PatAnnot (go p, a)
    | _ -> pat
  in go pat

and synth_construct ctx level name arg =
  match List.assoc_opt name ctx.type_env.constructors with
  | None -> error (Printf.sprintf "unknown constructor: %s" name)
  | Some info ->
    let qname = qualify_ctor_name ctx.type_env name info in
    (* Fresh tvars for universals (type params) + existentials *)
    let num_fresh = info.ctor_num_params + info.ctor_existentials in
    let all_fresh = List.init num_fresh (fun _ -> Types.new_tvar level) in
    let fresh_universals = List.filteri (fun i _ -> i < info.ctor_num_params) all_fresh in
    let result_ty = match info.ctor_return_ty_params with
      | None -> Types.TVariant (info.ctor_type_name, fresh_universals)
      | Some params ->
        (* GADT: substitute to get specific return type *)
        Types.TVariant (info.ctor_type_name, List.map (subst_tgens all_fresh) params)
    in
    (match info.ctor_arg_ty, arg with
     | None, None ->
       mk (TEConstruct (qname, None)) result_ty
     | Some expected_ty, Some arg_expr ->
       let concrete_ty = freshen_arrow_effects level (subst_tgens all_fresh expected_ty) in
       let arg_te = check ctx level arg_expr concrete_ty in
       mk (TEConstruct (qname, Some arg_te)) result_ty
     | None, Some _ ->
       error (Printf.sprintf "constructor %s takes no arguments" name)
     | Some expected_ty, None ->
       (* Constructor used as a function: return fun x -> Ctor x *)
       let concrete_ty = freshen_arrow_effects level (subst_tgens all_fresh expected_ty) in
       let param = "__x" in
       let param_var = mk (TEVar param) concrete_ty in
       let body = mk (TEConstruct (qname, Some param_var)) result_ty in
       mk (TEFun (param, body, false)) (Types.TArrow (concrete_ty, Types.EffEmpty, result_ty)))

(* Freeze all types in a texpr tree using deep_repr — used for GADT snapshot/restore *)
and freeze_texpr te =
  let ft = Types.deep_repr te.ty in
  let fe = match te.expr with
    | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEByte _ | TERune _
    | TEUnit | TEVar _ | TENil -> te.expr
    | TELet (n, s, e1, e2) -> TELet (n, s, freeze_texpr e1, freeze_texpr e2)
    | TELetRec (n, s, e1, e2) -> TELetRec (n, s, freeze_texpr e1, freeze_texpr e2)
    | TEFun (p, body, hr) -> TEFun (p, freeze_texpr body, hr)
    | TEApp (f, a) -> TEApp (freeze_texpr f, freeze_texpr a)
    | TEIf (c, t, e) -> TEIf (freeze_texpr c, freeze_texpr t, freeze_texpr e)
    | TEBinop (op, l, r) -> TEBinop (op, freeze_texpr l, freeze_texpr r)
    | TEUnop (op, e) -> TEUnop (op, freeze_texpr e)
    | TETuple es -> TETuple (List.map freeze_texpr es)
    | TERecord fs -> TERecord (List.map (fun (n, e) -> (n, freeze_texpr e)) fs)
    | TERecordUpdate (base, overrides) -> TERecordUpdate (freeze_texpr base, List.map (fun (n, e) -> (n, freeze_texpr e)) overrides)
    | TERecordUpdateIdx (base, pairs) -> TERecordUpdateIdx (freeze_texpr base, List.map (fun (i, v) -> (freeze_texpr i, freeze_texpr v)) pairs)
    | TEField (e, f) -> TEField (freeze_texpr e, f)
    | TEIndex (e, i) -> TEIndex (freeze_texpr e, freeze_texpr i)
    | TECons (h, t) -> TECons (freeze_texpr h, freeze_texpr t)
    | TEConstruct (n, arg) -> TEConstruct (n, Option.map freeze_texpr arg)
    | TEMatch (s, arms, p) ->
      TEMatch (freeze_texpr s, List.map (fun (pat, g, b) ->
        (pat, Option.map freeze_texpr g, freeze_texpr b)) arms, p)
    | TELetMut (n, e1, e2) -> TELetMut (n, freeze_texpr e1, freeze_texpr e2)
    | TEAssign (n, e) -> TEAssign (n, freeze_texpr e)
    | TEFieldAssign (e, f, v) -> TEFieldAssign (freeze_texpr e, f, freeze_texpr v)
    | TESeq (e1, e2) -> TESeq (freeze_texpr e1, freeze_texpr e2)
    | TEPerform (name, e) -> TEPerform (name, freeze_texpr e)
    | TEHandle (e, arms) ->
      TEHandle (freeze_texpr e, List.map (fun arm -> match arm with
        | THReturn (n, b) -> THReturn (n, freeze_texpr b)
        | THOp (eff, x, k, b) -> THOp (eff, x, k, freeze_texpr b)) arms)
    | TEResume (k, v) -> TEResume (freeze_texpr k, freeze_texpr v)
    | TEWhile (c, b) -> TEWhile (freeze_texpr c, freeze_texpr b)
    | TEBreak e -> TEBreak (freeze_texpr e)
    | TEContinueLoop -> TEContinueLoop
    | TEFoldContinue e -> TEFoldContinue (freeze_texpr e)
    | TEForLoop e -> TEForLoop (freeze_texpr e)
    | TELetRecAnd (binds, body) ->
      TELetRecAnd (List.map (fun (n, e) -> (n, freeze_texpr e)) binds, freeze_texpr body)
    | TEMap pairs -> TEMap (List.map (fun (k, v) -> (freeze_texpr k, freeze_texpr v)) pairs)
    | TEArray es -> TEArray (List.map freeze_texpr es)
    | TEReturn e -> TEReturn (freeze_texpr e)
  in
  { expr = fe; ty = ft; loc = te.loc }

(* GADT-aware pattern checking: creates fresh tvars for the ctor's universals + existentials,
   unifies the scrutinee type with the ctor's specific return type to establish local equations,
   then checks sub-patterns against the instantiated arg type *)
and check_pattern_gadt ctx level pat scrut_ty =
  match pat with
  | Ast.PatConstruct (name, arg_pat) ->
    (match List.assoc_opt name ctx.type_env.constructors with
     | None -> error (Printf.sprintf "unknown constructor in pattern: %s" name)
     | Some info ->
       let num_fresh = info.ctor_num_params + info.ctor_existentials in
       let all_fresh = List.init num_fresh (fun _ -> Types.new_tvar level) in
       (* Collect existential tvar refs (indices >= ctor_num_params) *)
       let existential_refs = List.filter_map (fun i ->
         if i >= info.ctor_num_params then
           match List.nth all_fresh i with
           | Types.TVar r -> Some r
           | _ -> None
         else None
       ) (List.init num_fresh Fun.id) in
       (* Build the specific return type for this GADT constructor *)
       let return_ty = match info.ctor_return_ty_params with
         | Some params ->
           Types.TVariant (info.ctor_type_name, List.map (subst_tgens all_fresh) params)
         | None ->
           (* Regular ctor in a GADT type — use universals only *)
           let fresh_universals = List.filteri (fun i _ -> i < info.ctor_num_params) all_fresh in
           Types.TVariant (info.ctor_type_name, fresh_universals)
       in
       (* Unify scrutinee with this ctor's return type — establishes local equations *)
       try_unify scrut_ty return_ty;
       let bindings = match info.ctor_arg_ty, arg_pat with
        | None, None -> []
        | Some arg_ty, Some p ->
          let concrete_ty = freshen_arrow_effects level (subst_tgens all_fresh arg_ty) in
          check_pattern ctx level p concrete_ty
        | None, Some _ -> error (Printf.sprintf "constructor %s takes no arguments" name)
        | Some _, None -> error (Printf.sprintf "constructor %s requires an argument" name)
       in
       (bindings, existential_refs))
  | _ -> (check_pattern ctx level pat scrut_ty, [])

and check_existential_escape existential_refs result_ty =
  (* Check that no existential type variable escaped into the result type.
     After arm body checking, existential tvars may have been unified with
     result_ty or parts of it. We detect escape by collecting all unbound tvar
     refs reachable from result_ty, then checking if any existential tvar ref,
     after following links, resolves to one of those refs. *)
  let rec collect_tvar_refs acc ty =
    match Types.repr ty with
    | Types.TVar ({ contents = Types.Unbound _ } as r) ->
      if List.exists (fun x -> (==) x r) acc then acc else r :: acc
    | Types.TArrow (a, _, b) | Types.TCont (a, _, b) -> collect_tvar_refs (collect_tvar_refs acc a) b
    | Types.TTuple ts -> List.fold_left collect_tvar_refs acc ts
    | Types.TList t -> collect_tvar_refs acc t
    | Types.TArray t -> collect_tvar_refs acc t
    | Types.TMap (k, v) -> collect_tvar_refs (collect_tvar_refs acc k) v
    | Types.TRecord row -> List.fold_left (fun a (_, t) -> collect_tvar_refs a t) acc (Types.record_row_to_fields row)
    | Types.TVariant (_, args) -> List.fold_left collect_tvar_refs acc args
    | Types.TPolyVariant row ->
      let rec collect_pv a = function
        | Types.PVRow (_, Some t, tail) -> collect_pv (collect_tvar_refs a t) tail
        | Types.PVRow (_, None, tail) -> collect_pv a tail
        | _ -> a
      in collect_pv acc row
    | _ -> acc
  in
  let result_refs = collect_tvar_refs [] (Types.deep_repr result_ty) in
  let has_escape = List.exists (fun exist_ref ->
    match Types.repr (Types.TVar exist_ref) with
    | Types.TVar ({ contents = Types.Unbound _ } as resolved_ref) ->
      (* Existential resolved to an unbound tvar — check if it's in result_ty *)
      List.exists (fun r -> (==) r resolved_ref) result_refs
    | resolved ->
      (* Existential was unified with a concrete type — check if any tvar in that
         type is also in result_ty *)
      let exist_refs = collect_tvar_refs [] resolved in
      List.exists (fun er -> List.exists (fun rr -> (==) er rr) result_refs) exist_refs
  ) existential_refs in
  if has_escape then
    error "existential type variable would escape the scope of its GADT match arm"

and synth_match expected_ty ctx level scrut arms partial =
  let scrut_te = synth ctx level scrut in
  (* Determine if scrutinee is a GADT type *)
  let gadt_info = match Types.repr scrut_te.ty with
    | Types.TVariant (name, params) ->
      (match find_variant_info ctx.type_env name with
       | Some (_, _, _, true) -> Some (name, params)
       | _ -> None)
    | _ -> None
  in
  match gadt_info with
  | Some (_gadt_name, scrut_params) ->
    (* GADT match — need snapshot/restore for local type equations *)
    let result_ty = match expected_ty with
      | Some ty -> ty
      | None -> Types.new_tvar level
    in
    (* Extract tvar refs from scrutinee params for snapshot/restore *)
    let get_tvar_ref ty = match Types.repr ty with
      | Types.TVar r -> Some r
      | _ -> None
    in
    let param_refs = List.filter_map (fun p ->
      match get_tvar_ref p with
      | Some r -> Some (r, !r)
      | None -> None
    ) scrut_params in
    (* Also save all params' current repr values for non-tvar params *)
    let save_snapshot () =
      List.map (fun (r, _) -> (r, !r)) param_refs
    in
    let restore_snapshot saved =
      List.iter (fun (r, v) -> r := v) saved
    in
    let check_gadt_arm (pat, guard, body) =
      let snapshot = save_snapshot () in
      (* For constructor patterns, we need GADT-aware pattern checking *)
      let ctor_name = match pat with
        | Ast.PatConstruct (name, _) -> Some name
        | _ -> None
      in
      let is_gadt_ctor = match ctor_name with
        | Some name ->
          (match List.assoc_opt name ctx.type_env.constructors with
           | Some info -> info.Types.ctor_return_ty_params <> None
           | None -> false)
        | None -> false
      in
      let (bindings, existential_refs) =
        if is_gadt_ctor then
          check_pattern_gadt ctx level pat scrut_te.ty
        else
          (check_pattern ctx level pat scrut_te.ty, [])
      in
      let ctx' = List.fold_left (fun c (n, t) -> extend_var_mono c n t) ctx bindings in
      let guard_te = match guard with
        | Some g -> Some (check ctx' level g Types.TBool)
        | None -> None
      in
      let body_te = check ctx' level body result_ty in
      (* Freeze all types in the arm before restoring *)
      let guard_te = Option.map freeze_texpr guard_te in
      let body_te = freeze_texpr body_te in
      (* Check existential escape *)
      if existential_refs <> [] then
        check_existential_escape existential_refs result_ty;
      (* Restore snapshot *)
      restore_snapshot snapshot;
      (qualify_pattern ctx.type_env pat, guard_te, body_te)
    in
    (match arms with
     | [] -> error "match expression has no arms"
     | first :: rest ->
       let first_arm = check_gadt_arm first in
       let rest_arms = List.map check_gadt_arm rest in
       let all_arms = first_arm :: rest_arms in
       (* Check exhaustiveness *)
       if not partial then
         !exhaustiveness_check_ref ctx (Types.repr scrut_te.ty) all_arms;
       mk (TEMatch (scrut_te, all_arms, partial)) result_ty)
  | None ->
    (* Regular ADT match — original logic *)
    let check_arm ctx (pat, guard, body) =
      let bindings = check_pattern ctx level pat scrut_te.ty in
      let ctx' = List.fold_left (fun c (n, t) -> extend_var_mono c n t) ctx bindings in
      let guard_te = match guard with
        | Some g ->
          let gte = check ctx' level g Types.TBool in
          Some gte
        | None -> None
      in
      let body_te = synth ctx' level body in
      (qualify_pattern ctx.type_env pat, guard_te, body_te)
    in
    match arms with
    | [] -> error "match expression has no arms"
    | first :: rest ->
      let (fp, fg, fb) = check_arm ctx first in
      let result_ty = fb.ty in
      let all_arms = (fp, fg, fb) ::
        List.map (fun arm ->
          let (p, g, b) = check_arm ctx arm in
          try_unify b.ty result_ty;
          (p, g, b)
        ) rest
      in
      (* Check exhaustiveness *)
      if not partial then
        !exhaustiveness_check_ref ctx (Types.repr scrut_te.ty) all_arms;
      mk (TEMatch (scrut_te, all_arms, partial)) result_ty

(* ---- Check ---- *)

and check ctx level (expr : Ast.expr) (expected : Types.ty) : texpr =
  match expr with
  | Ast.ELoc (loc, inner) -> current_loc := loc; check ctx level inner expected
  | Ast.EFun (param, body) when param.annot = None ->
    let arg_ty = Types.new_tvar level in
    let ret_ty = Types.new_tvar level in
    let fn_eff = Types.new_effvar level in
    try_unify expected (Types.TArrow (arg_ty, fn_eff, ret_ty));
    let return_used = ref false in
    let ctx' = extend_var_mono { ctx with current_eff = fn_eff; return_type = Some ret_ty; inside_handler = false; return_used } param.name (Types.repr arg_ty) in
    let body_te = check ctx' level body ret_ty in
    let has_return = if param.is_generated then false else !return_used in
    mk (TEFun (param.name, body_te, has_return)) (Types.TArrow (arg_ty, fn_eff, body_te.ty))
  | Ast.EMatch (scrut, arms, partial) ->
    synth_match (Some expected) ctx level scrut arms partial
  | _ ->
    let te = synth ctx level expr in
    try_unify te.ty expected;
    { te with ty = expected }

(* ---- Pattern checking ---- *)

and check_pattern ctx level (pat : Ast.pattern) (ty : Types.ty) : (string * Types.ty) list =
  match pat with
  | Ast.PatWild -> []
  | Ast.PatVar name -> [(name, ty)]
  | Ast.PatInt _ -> try_unify ty Types.TInt; []
  | Ast.PatFloat _ -> try_unify ty Types.TFloat; []
  | Ast.PatBool _ -> try_unify ty Types.TBool; []
  | Ast.PatString _ -> try_unify ty Types.TString; []
  | Ast.PatUnit -> try_unify ty Types.TUnit; []
  | Ast.PatTuple pats ->
    let tys = List.map (fun _ -> Types.new_tvar level) pats in
    try_unify ty (Types.TTuple tys);
    List.concat (List.map2 (check_pattern ctx level) pats tys)
  | Ast.PatCons (hd_pat, tl_pat) ->
    let elem_ty = Types.new_tvar level in
    try_unify ty (Types.TList elem_ty);
    let hd_bindings = check_pattern ctx level hd_pat elem_ty in
    let tl_bindings = check_pattern ctx level tl_pat ty in
    hd_bindings @ tl_bindings
  | Ast.PatNil ->
    let elem_ty = Types.new_tvar level in
    try_unify ty (Types.TList elem_ty);
    []
  | Ast.PatConstruct (name, arg_pat) ->
    (match List.assoc_opt name ctx.type_env.constructors with
     | None -> error (Printf.sprintf "unknown constructor in pattern: %s" name)
     | Some info ->
       let fresh_args = List.init info.ctor_num_params (fun _ -> Types.new_tvar level) in
       let expected_variant = Types.TVariant (info.ctor_type_name, fresh_args) in
       try_unify ty expected_variant;
       (match info.ctor_arg_ty, arg_pat with
        | None, None -> []
        | Some arg_ty, Some p ->
          let concrete_ty = freshen_arrow_effects level (subst_tgens fresh_args arg_ty) in
          check_pattern ctx level p concrete_ty
        | None, Some _ -> error (Printf.sprintf "constructor %s takes no arguments" name)
        | Some _, None -> error (Printf.sprintf "constructor %s requires an argument" name)))
  | Ast.PatRecord field_pats ->
    let field_tys = match Types.repr ty with
      | Types.TRecord row -> Types.record_row_to_fields row
      | _ ->
        (* Try to infer record type from field names in the pattern *)
        let pat_field_names = List.map fst field_pats in
        let candidates = List.filter (fun (_name, fields) ->
          List.for_all (fun fn -> List.mem_assoc fn fields) pat_field_names
        ) ctx.type_env.Types.records in
        (match candidates with
         | [(_name, fields)] ->
           let row = instantiate_record_row level fields in
           try_unify ty (Types.TRecord row);
           instantiate_record_fields level fields
         | [] ->
           (* No nominal record found. If the type is still unknown,
              construct an open record type from pattern fields and unify. *)
           (match Types.repr ty with
            | Types.TVar _ ->
              let row = List.fold_right (fun fname rest ->
                Types.RRow (fname, Types.new_tvar level, rest)
              ) pat_field_names (Types.new_rvar level) in
              try_unify ty (Types.TRecord row);
              Types.record_row_to_fields (match Types.repr ty with
                | Types.TRecord r -> r
                | _ -> error "record pattern used with non-record type")
            | _ -> error "record pattern used with non-record type")
         | _ -> error "ambiguous record pattern — annotate the type")
    in
    List.concat (List.map (fun (name, pat) ->
      match List.assoc_opt name field_tys with
      | Some fty -> check_pattern ctx level pat fty
      | None -> error (Printf.sprintf "record type has no field %s" name)
    ) field_pats)
  | Ast.PatAs (inner_pat, name) ->
    let bindings = check_pattern ctx level inner_pat ty in
    (name, ty) :: bindings
  | Ast.PatOr (p1, p2) ->
    let bindings1 = check_pattern ctx level p1 ty in
    let bindings2 = check_pattern ctx level p2 ty in
    let names1 = List.map fst bindings1 |> List.sort String.compare in
    let names2 = List.map fst bindings2 |> List.sort String.compare in
    if names1 <> names2 then
      error "or-pattern branches must bind the same variables";
    List.iter (fun (name, ty1) ->
      match List.assoc_opt name bindings2 with
      | Some ty2 -> try_unify ty1 ty2
      | None -> ()
    ) bindings1;
    bindings1
  | Ast.PatArray pats ->
    let elem_ty = Types.new_tvar level in
    try_unify ty (Types.TArray elem_ty);
    List.concat_map (fun p -> check_pattern ctx level p elem_ty) pats
  | Ast.PatMap entries ->
    let key_ty = Types.new_tvar level in
    let val_ty = Types.new_tvar level in
    try_unify ty (Types.TMap (key_ty, val_ty));
    List.concat_map (fun (kpat, vpat) ->
      check_pattern ctx level kpat key_ty @
      check_pattern ctx level vpat val_ty
    ) entries
  | Ast.PatPolyVariant (tag, None) ->
    let tail = Types.new_pvvar level in
    let row = Types.PVRow (tag, None, tail) in
    try_unify ty (Types.TPolyVariant row);
    []
  | Ast.PatPolyVariant (tag, Some sub_pat) ->
    let payload_ty = Types.new_tvar level in
    let tail = Types.new_pvvar level in
    let row = Types.PVRow (tag, Some payload_ty, tail) in
    try_unify ty (Types.TPolyVariant row);
    check_pattern ctx level sub_pat payload_ty
  | Ast.PatPin name ->
    (match List.assoc_opt name ctx.vars with
     | Some scheme ->
       let var_ty = Types.instantiate level scheme in
       try_unify ty var_ty;
       []
     | None -> error (Printf.sprintf "unbound variable in pin pattern: %s" name))
  | Ast.PatAnnot (inner_pat, annot) ->
    let annot_ty = resolve_ty_annot ctx level annot in
    try_unify ty annot_ty;
    check_pattern ctx level inner_pat ty

(* ---- Polymorphic recursion (locally abstract types) ---- *)
(* When a let rec has explicit type params via (type 'a ...), we bind the
   recursive name with a polymorphic scheme from the start, enabling
   recursive calls at different type instantiations. *)

and synth_poly_rec_fn ctx level name qualified_name type_params params ret_annot body =
  let shared_tvars = Hashtbl.create 4 in
  (* Pre-allocate tvars for locally abstract types at level+1 *)
  List.iter (fun tp ->
    Hashtbl.replace shared_tvars tp (Types.new_tvar (level + 1))
  ) type_params;
  (* Resolve param types *)
  let param_tys = List.map (fun (p : Ast.param) ->
    match p.annot with
    | Some annot -> resolve_ty_annot_shared ctx (level + 1) shared_tvars annot
    | None -> Types.new_tvar (level + 1)
  ) params in
  (* Resolve return type and optional effect annotation *)
  let (ret_ty, body_eff) = match ret_annot with
    | Some (Ast.TyWithEffect (ty, eff_annot)) ->
      let ty' = resolve_ty_annot_shared ctx (level + 1) shared_tvars ty in
      let eff = match eff_annot with
        | Ast.EffAnnotPure -> Types.EffEmpty
        | Ast.EffAnnotRow items -> resolve_eff_items ctx (level + 1) shared_tvars items
      in
      (ty', eff)
    | Some annot ->
      (resolve_ty_annot_shared ctx (level + 1) shared_tvars annot,
       Types.new_effvar (level + 1))
    | None ->
      (Types.new_tvar (level + 1), Types.new_effvar (level + 1))
  in
  (* Build function type: p1 -> p2 -> ... -> ret *)
  let fn_ty = match List.rev param_tys with
    | [] -> ret_ty
    | last :: rest ->
      let inner = Types.TArrow (last, body_eff, ret_ty) in
      List.fold_left (fun acc pty ->
        Types.TArrow (pty, Types.EffEmpty, acc)
      ) inner rest
  in
  (* Generalize to get polymorphic scheme *)
  let scheme = Types.generalize level fn_ty in
  (* Bind name polymorphically — this enables polymorphic recursion *)
  let ctx_with_self = extend_var ctx name scheme in
  let ctx_with_self = if qualified_name <> "" then extend_var ctx_with_self qualified_name scheme else ctx_with_self in
  (* Build inner context with params bound at their resolved types *)
  let inner_ctx = List.fold_left2 (fun c (p : Ast.param) ty ->
    extend_var_mono c p.name ty
  ) { ctx_with_self with current_eff = body_eff } params param_tys in
  (* Check body against declared return type *)
  let body_te = check inner_ctx (level + 1) body ret_ty in
  (* Build TEFun chain *)
  let te = match List.rev params, List.rev param_tys with
    | [], [] -> body_te
    | last_p :: rest_ps, last_pty :: rest_ptys ->
      let inner = mk (TEFun (last_p.Ast.name, body_te, false))
        (Types.TArrow (last_pty, body_eff, body_te.ty)) in
      List.fold_left2 (fun acc (p : Ast.param) pty ->
        mk (TEFun (p.name, acc, false)) (Types.TArrow (pty, Types.EffEmpty, acc.ty))
      ) inner rest_ps rest_ptys
    | _ -> assert false
  in
  (te, scheme)

(* ---- Exhaustiveness checking ---- *)

let rec normalize_pattern (pat : Ast.pattern) : Ast.pattern list =
  match pat with
  | Ast.PatAs (inner, _) -> normalize_pattern inner
  | Ast.PatOr (p1, p2) -> normalize_pattern p1 @ normalize_pattern p2
  | p -> [p]

let ctor_short_name name =
  match String.rindex_opt name '.' with
  | Some i -> String.sub name (i + 1) (String.length name - i - 1)
  | None -> name

let rec specialize_variant pats ctor_name =
  List.concat_map (fun pat ->
    match pat with
    | Ast.PatConstruct (name, sub) when String.equal (ctor_short_name name) ctor_name ->
      (match sub with
       | Some p -> normalize_pattern p
       | None -> [Ast.PatWild])
    | Ast.PatConstruct (_, _) -> []
    | Ast.PatWild | Ast.PatVar _ -> [Ast.PatWild]
    | Ast.PatAs (inner, _) -> specialize_variant (normalize_pattern inner) ctor_name
    | Ast.PatOr (p1, p2) ->
      specialize_variant (normalize_pattern p1) ctor_name @
      specialize_variant (normalize_pattern p2) ctor_name
    | _ -> []
  ) pats

let has_wildcard pats =
  List.exists (fun p ->
    match p with
    | Ast.PatWild | Ast.PatVar _ -> true
    | _ -> false
  ) pats

let rec uncovered_patterns type_env ty pats =
  (* Normalize all patterns: unwrap PatAs, flatten PatOr *)
  let pats = List.concat_map normalize_pattern pats in
  (* If any pattern is a wildcard/var, fully covered *)
  if has_wildcard pats then []
  else
    match Types.repr ty with
    | Types.TVariant (name, args) ->
      (match find_variant_info type_env name with
       | None -> []  (* unknown variant, skip *)
       | Some (_, _, ctors, is_gadt) ->
         (* For GADTs, filter out constructors whose return type is incompatible *)
         let reachable_ctors = if is_gadt then
           List.filter (fun (ctor_name, _) ->
             match List.assoc_opt ctor_name type_env.Types.constructors with
             | Some info ->
               (match info.Types.ctor_return_ty_params with
                | None -> true  (* regular ctor in GADT, always reachable *)
                | Some ret_params ->
                  (* Check compatibility: for each param position, check if the
                     ctor's return type param is compatible with the scrutinee's param *)
                  List.length ret_params = List.length args &&
                  List.for_all2 Types.types_compatible
                    (List.map (fun p -> subst_tgens args p) ret_params)
                    args)
             | None -> true
           ) ctors
         else ctors in
         List.concat_map (fun (ctor_name, ctor_arg_ty) ->
           let sub_pats = specialize_variant pats ctor_name in
           match ctor_arg_ty with
           | None ->
             if List.length sub_pats = 0 then [ctor_name]
             else []
           | Some arg_ty ->
             if List.length sub_pats = 0 then [ctor_name ^ " _"]
             else
               let sub_missing = uncovered_patterns type_env arg_ty sub_pats in
               List.map (fun m -> ctor_name ^ " (" ^ m ^ ")") sub_missing
         ) reachable_ctors)
    | Types.TBool ->
      let has_true = List.exists (fun p -> match p with Ast.PatBool true -> true | _ -> false) pats in
      let has_false = List.exists (fun p -> match p with Ast.PatBool false -> true | _ -> false) pats in
      let missing = ref [] in
      if not has_true then missing := "true" :: !missing;
      if not has_false then missing := "false" :: !missing;
      !missing
    | Types.TList _ ->
      let has_nil = List.exists (fun p -> match p with Ast.PatNil -> true | _ -> false) pats in
      let has_cons = List.exists (fun p -> match p with Ast.PatCons _ -> true | _ -> false) pats in
      let missing = ref [] in
      if not has_nil then missing := "[]" :: !missing;
      if not has_cons then missing := "_ :: _" :: !missing;
      !missing
    | Types.TArray _ ->
      (* Array patterns match specific lengths; find smallest uncovered length *)
      let lengths = List.filter_map (fun p ->
        match p with
        | Ast.PatArray elems -> Some (List.length elems)
        | _ -> None
      ) pats in
      let lengths_set = List.sort_uniq compare lengths in
      let rec find_missing n =
        if List.mem n lengths_set then find_missing (n + 1)
        else n
      in
      let missing_len = find_missing 0 in
      let missing_pat = if missing_len = 0 then "#[]"
        else "#[" ^ String.concat "; " (List.init missing_len (fun _ -> "_")) ^ "]"
      in
      [missing_pat]
    | Types.TMap _ -> ["_"]
    | Types.TInt | Types.TFloat | Types.TString -> ["_"]
    | Types.TUnit ->
      let has_unit = List.exists (fun p ->
        match p with Ast.PatUnit -> true | _ -> false
      ) pats in
      if has_unit then [] else ["()"]
    | Types.TTuple ts ->
      let tuple_pats = List.filter_map (fun p ->
        match p with Ast.PatTuple ps when List.length ps = List.length ts -> Some ps | _ -> None
      ) pats in
      if tuple_pats = [] then
        ["(" ^ String.concat ", " (List.init (List.length ts) (fun _ -> "_")) ^ ")"]
      else
        let rec check_positions i = function
          | [] -> []
          | elem_ty :: rest_tys ->
            let position_pats = List.map (fun ps -> List.nth ps i) tuple_pats in
            let sub_missing = uncovered_patterns type_env elem_ty position_pats in
            if sub_missing <> [] then
              let template = List.init (List.length ts) (fun j ->
                if j = i then List.hd sub_missing else "_"
              ) in
              ["(" ^ String.concat ", " template ^ ")"]
            else
              check_positions (i + 1) rest_tys
        in
        check_positions 0 ts
    | _ -> []  (* TRecord, TVar, TGen, TArrow — skip *)

let exhaustiveness_check ctx ty arms =
  (* Filter out guarded arms — guards make coverage uncertain *)
  let unguarded_pats = List.filter_map (fun (pat, guard, _body) ->
    match guard with
    | None -> Some pat
    | Some _ -> None
  ) arms in
  let missing = uncovered_patterns ctx.type_env ty unguarded_pats in
  if missing <> [] then
    error (Printf.sprintf "non-exhaustive match, missing: %s"
             (String.concat ", " missing))

let () = exhaustiveness_check_ref := exhaustiveness_check

(* ---- Type definition processing ---- *)

let process_type_def ctx type_params type_name (def : Ast.type_def) : ctx =
  let num_params = List.length type_params in
  match def with
  | Ast.TDVariant ctors ->
    let pre_ctx = { ctx with type_env = { ctx.type_env with
      variants = (type_name, num_params, [], false) :: ctx.type_env.variants;
    }} in
    (* Build a resolver that maps type param names to TGen indices *)
    let param_tbl = Hashtbl.create 4 in
    List.iteri (fun i name -> Hashtbl.replace param_tbl name i) type_params;
    let resolve_with_params annot =
      let tvars = Hashtbl.create 4 in
      let rec go = function
        | Ast.TyVar name ->
          (match Hashtbl.find_opt param_tbl name with
           | Some idx -> Types.TGen idx
           | None ->
             (match Hashtbl.find_opt tvars name with
              | Some tv -> tv
              | None ->
                let tv = Types.new_tvar 0 in
                Hashtbl.replace tvars name tv;
                tv))
        | Ast.TyName "int" -> Types.TInt
        | Ast.TyName "float" -> Types.TFloat
        | Ast.TyName "bool" -> Types.TBool
        | Ast.TyName "string" -> Types.TString
        | Ast.TyName "unit" -> Types.TUnit
        | Ast.TyName name ->
          let canonical = resolve_type_alias pre_ctx.type_env name in
          (match List.find_opt (fun (n, _, _) -> String.equal n canonical) pre_ctx.type_env.type_synonyms with
           | Some (_, 0, ty) -> ty
           | Some (_, n, _) ->
             error (Printf.sprintf "type %s expects %d type argument(s)" name n)
           | None ->
             (match find_variant_info pre_ctx.type_env name with
              | Some (_, 0, _, _) -> Types.TVariant (canonical, [])
              | Some (_, n, _, _) ->
                error (Printf.sprintf "type %s expects %d type argument(s)" name n)
              | None ->
                if List.assoc_opt canonical pre_ctx.type_env.records <> None then
                  let fields = List.assoc canonical pre_ctx.type_env.records in
                  Types.TRecord (Types.fields_to_closed_row fields)
                else
                  error (Printf.sprintf "unknown type: %s" name)))
        | Ast.TyArrow (a, b, _) -> Types.TArrow (go a, Types.EffEmpty, go b)
        | Ast.TyTuple ts -> Types.TTuple (List.map go ts)
        | Ast.TyList t -> Types.TList (go t)
        | Ast.TyArray t -> Types.TArray (go t)
        | Ast.TyMap (k, v) -> Types.TMap (go k, go v)
        | Ast.TyApp (args, tname) ->
          let canonical = resolve_type_alias pre_ctx.type_env tname in
          let arg_tys = List.map go args in
          (match List.find_opt (fun (n, _, _) -> String.equal n canonical) pre_ctx.type_env.type_synonyms with
           | Some (_, np, ty) ->
             if List.length arg_tys <> np then
               error (Printf.sprintf "type %s expects %d type argument(s), got %d"
                 tname np (List.length arg_tys));
             subst_tgens arg_tys ty
           | None ->
             (match find_variant_info pre_ctx.type_env tname with
              | Some (_, np, _, _) ->
                if List.length arg_tys <> np then
                  error (Printf.sprintf "type %s expects %d type argument(s), got %d"
                    tname np (List.length arg_tys));
                Types.TVariant (canonical, arg_tys)
              | None -> error (Printf.sprintf "unknown type constructor: %s" tname)))
        | Ast.TyRecord (fields, is_open) ->
          let fields = List.map (fun (n, t) -> (n, go t)) fields in
          let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
          let tail = if is_open then Types.new_rvar 0 else Types.REmpty in
          Types.TRecord (List.fold_right (fun (n, t) acc -> Types.RRow (n, t, acc)) fields tail)
        | Ast.TyQualified (path, name) ->
          let qualified = String.concat "." path ^ "." ^ name in
          go (Ast.TyName qualified)
        | Ast.TyPolyVariant (_kind, tags) ->
          let row = List.fold_right (fun (tag, payload_annot) acc ->
            Types.PVRow (tag, Option.map go payload_annot, acc)
          ) tags Types.PVEmpty in
          Types.TPolyVariant row
        | Ast.TyWithEffect (ty, _) -> go ty
      in
      go annot
    in
    let is_gadt = List.exists (fun (_, _, ret) -> ret <> None) ctors in
    (* Collect free type variable names from an annotation *)
    let rec collect_annot_vars acc = function
      | Ast.TyVar name -> if List.mem name acc then acc else name :: acc
      | Ast.TyArrow (a, b, _) -> collect_annot_vars (collect_annot_vars acc a) b
      | Ast.TyTuple ts -> List.fold_left collect_annot_vars acc ts
      | Ast.TyList t | Ast.TyArray t -> collect_annot_vars acc t
      | Ast.TyMap (k, v) -> collect_annot_vars (collect_annot_vars acc k) v
      | Ast.TyApp (args, _) -> List.fold_left collect_annot_vars acc args
      | Ast.TyRecord (fs, _) -> List.fold_left (fun a (_, t) -> collect_annot_vars a t) acc fs
      | Ast.TyQualified _ | Ast.TyName _ -> acc
      | Ast.TyPolyVariant (_, tags) ->
        List.fold_left (fun a (_, ty_opt) ->
          match ty_opt with Some t -> collect_annot_vars a t | None -> a
        ) acc tags
      | Ast.TyWithEffect (ty, _) -> collect_annot_vars acc ty
    in
    let process_ctor (name, arg_annot, ret_annot) =
      match ret_annot with
      | None ->
        (* Regular ADT constructor *)
        let arg_ty = Option.map resolve_with_params arg_annot in
        let info = Types.{ ctor_type_name = type_name; ctor_arg_ty = arg_ty;
                           ctor_num_params = num_params;
                           ctor_return_ty_params = None; ctor_existentials = 0 } in
        ((name, arg_ty), (name, info))
      | Some ret ->
        (* GADT constructor — validate return type and compute existentials *)
        (* 1. Validate return type is an application of this type *)
        let (ret_type_name, ret_type_args) = match ret with
          | Ast.TyName n -> (n, [])
          | Ast.TyApp (args, n) -> (n, args)
          | _ -> error (Printf.sprintf "GADT constructor %s: return type must be %s" name type_name)
        in
        let canonical_ret = resolve_type_alias pre_ctx.type_env ret_type_name in
        (* Also accept short name matching the last component of a qualified type_name *)
        let name_matches =
          String.equal canonical_ret type_name ||
          (match String.rindex_opt type_name '.' with
           | Some i ->
             let short = String.sub type_name (i + 1) (String.length type_name - i - 1) in
             String.equal ret_type_name short
           | None -> false)
        in
        if not name_matches then
          error (Printf.sprintf "GADT constructor %s: return type must be %s, got %s"
            name type_name ret_type_name);
        if List.length ret_type_args <> num_params then
          error (Printf.sprintf "GADT constructor %s: return type has %d type arg(s), expected %d"
            name (List.length ret_type_args) num_params);
        (* 2. Collect vars in return annotation and arg annotation *)
        let ret_vars = List.fold_left collect_annot_vars [] ret_type_args in
        let arg_vars = match arg_annot with
          | None -> []
          | Some a -> collect_annot_vars [] a
        in
        (* 3. Existentials: vars in arg but not in return, excluding type params *)
        let existentials = List.filter (fun v ->
          not (List.mem v ret_vars) && not (List.mem v type_params)
        ) arg_vars in
        let num_exist = List.length existentials in
        (* 4. Build extended param_tbl: type_params at 0..n-1, existentials at n..n+e-1 *)
        let gadt_tbl = Hashtbl.create 8 in
        List.iteri (fun i p -> Hashtbl.replace gadt_tbl p i) type_params;
        List.iteri (fun i e -> Hashtbl.replace gadt_tbl e (num_params + i)) existentials;
        (* 5. Resolve types using GADT param table *)
        let gadt_tvars = Hashtbl.create 4 in
        let rec gadt_resolve = function
          | Ast.TyVar vname ->
            (match Hashtbl.find_opt gadt_tbl vname with
             | Some idx -> Types.TGen idx
             | None ->
               (match Hashtbl.find_opt gadt_tvars vname with
                | Some tv -> tv
                | None ->
                  let tv = Types.new_tvar 0 in
                  Hashtbl.replace gadt_tvars vname tv;
                  tv))
          | Ast.TyName "int" -> Types.TInt
          | Ast.TyName "float" -> Types.TFloat
          | Ast.TyName "bool" -> Types.TBool
          | Ast.TyName "string" -> Types.TString
          | Ast.TyName "unit" -> Types.TUnit
          | Ast.TyName n ->
            let canonical = resolve_type_alias pre_ctx.type_env n in
            (match List.find_opt (fun (tn, _, _) -> String.equal tn canonical) pre_ctx.type_env.type_synonyms with
             | Some (_, 0, ty) -> ty
             | Some (_, np, _) -> error (Printf.sprintf "type %s expects %d type argument(s)" n np)
             | None ->
               (match find_variant_info pre_ctx.type_env n with
                | Some (_, 0, _, _) -> Types.TVariant (canonical, [])
                | Some (_, np, _, _) -> error (Printf.sprintf "type %s expects %d type argument(s)" n np)
                | None ->
                  if List.assoc_opt canonical pre_ctx.type_env.records <> None then
                    let fields = List.assoc canonical pre_ctx.type_env.records in
                    if fields = [] then Types.TRecord Types.RWild
                    else Types.TRecord (Types.fields_to_closed_row fields)
                  else error (Printf.sprintf "unknown type: %s" n)))
          | Ast.TyArrow (a, b, _) -> Types.TArrow (gadt_resolve a, Types.EffEmpty, gadt_resolve b)
          | Ast.TyTuple ts -> Types.TTuple (List.map gadt_resolve ts)
          | Ast.TyList t -> Types.TList (gadt_resolve t)
          | Ast.TyArray t -> Types.TArray (gadt_resolve t)
          | Ast.TyMap (k, v) -> Types.TMap (gadt_resolve k, gadt_resolve v)
          | Ast.TyApp (args, tname) ->
            let canonical = resolve_type_alias pre_ctx.type_env tname in
            let arg_tys = List.map gadt_resolve args in
            (match List.find_opt (fun (tn, _, _) -> String.equal tn canonical) pre_ctx.type_env.type_synonyms with
             | Some (_, np, ty) ->
               if List.length arg_tys <> np then
                 error (Printf.sprintf "type %s expects %d type argument(s), got %d" tname np (List.length arg_tys));
               subst_tgens arg_tys ty
             | None ->
               (match find_variant_info pre_ctx.type_env tname with
                | Some (_, np, _, _) ->
                  if List.length arg_tys <> np then
                    error (Printf.sprintf "type %s expects %d type argument(s), got %d" tname np (List.length arg_tys));
                  Types.TVariant (canonical, arg_tys)
                | None -> error (Printf.sprintf "unknown type constructor: %s" tname)))
          | Ast.TyRecord (fields, is_open) ->
            let fields = List.sort (fun (a,_) (b,_) -> String.compare a b)
              (List.map (fun (fn, ft) -> (fn, gadt_resolve ft)) fields) in
            let tail = if is_open then Types.new_rvar 0 else Types.REmpty in
            Types.TRecord (List.fold_right (fun (n, t) acc -> Types.RRow (n, t, acc)) fields tail)
          | Ast.TyQualified (path, n) ->
            let qualified = String.concat "." path ^ "." ^ n in
            gadt_resolve (Ast.TyName qualified)
          | Ast.TyPolyVariant (_kind, tags) ->
            let row = List.fold_right (fun (tag, payload_annot) acc ->
              Types.PVRow (tag, Option.map gadt_resolve payload_annot, acc)
            ) tags Types.PVEmpty in
            Types.TPolyVariant row
          | Ast.TyWithEffect (ty, _) -> gadt_resolve ty
        in
        let arg_ty = Option.map gadt_resolve arg_annot in
        let ret_params = List.map gadt_resolve ret_type_args in
        let info = Types.{ ctor_type_name = type_name; ctor_arg_ty = arg_ty;
                           ctor_num_params = num_params;
                           ctor_return_ty_params = Some ret_params;
                           ctor_existentials = num_exist } in
        ((name, arg_ty), (name, info))
    in
    let results = List.map process_ctor ctors in
    let variant_def = List.map fst results in
    let ctor_infos = List.map snd results in
    let type_env = { ctx.type_env with
      variants = (type_name, num_params, variant_def, is_gadt) :: ctx.type_env.variants;
      constructors = ctor_infos @ ctx.type_env.constructors;
    } in
    { ctx with type_env }
  | Ast.TDRecord fields ->
    (* Pre-register the record type so recursive references resolve *)
    let pre_ctx = { ctx with type_env = { ctx.type_env with
      records = (type_name, []) :: ctx.type_env.records;
    }} in
    let param_tbl = Hashtbl.create 4 in
    List.iteri (fun i name -> Hashtbl.replace param_tbl name i) type_params;
    let typed_fields = List.map (fun (_is_mut, name, annot) ->
      let tvars = Hashtbl.create 4 in
      let rec resolve_field = function
        | Ast.TyVar vname ->
          (match Hashtbl.find_opt param_tbl vname with
           | Some idx -> Types.TGen idx
           | None ->
             (match Hashtbl.find_opt tvars vname with
              | Some tv -> tv
              | None ->
                let tv = Types.new_tvar 0 in
                Hashtbl.replace tvars vname tv;
                tv))
        | Ast.TyName "int" -> Types.TInt
        | Ast.TyName "float" -> Types.TFloat
        | Ast.TyName "bool" -> Types.TBool
        | Ast.TyName "string" -> Types.TString
        | Ast.TyName "unit" -> Types.TUnit
        | Ast.TyName n -> resolve_ty_annot pre_ctx 0 (Ast.TyName n)
        | Ast.TyArrow (a, b, _) -> Types.TArrow (resolve_field a, Types.EffEmpty, resolve_field b)
        | Ast.TyTuple ts -> Types.TTuple (List.map resolve_field ts)
        | Ast.TyList t -> Types.TList (resolve_field t)
        | Ast.TyArray t -> Types.TArray (resolve_field t)
        | Ast.TyMap (k, v) -> Types.TMap (resolve_field k, resolve_field v)
        | Ast.TyApp (args, n) ->
          let arg_tys = List.map resolve_field args in
          (match List.find_opt (fun (tn, _, _) -> String.equal tn n) pre_ctx.type_env.type_synonyms with
           | Some (_, np, ty) ->
             if List.length arg_tys <> np then
               error (Printf.sprintf "type %s expects %d arg(s), got %d" n np (List.length arg_tys));
             subst_tgens arg_tys ty
           | None ->
             (match find_variant_info pre_ctx.type_env n with
              | Some (_, np, _, _) ->
                if List.length arg_tys <> np then
                  error (Printf.sprintf "type %s expects %d arg(s), got %d" n np (List.length arg_tys));
                let canonical = resolve_type_alias pre_ctx.type_env n in
                Types.TVariant (canonical, arg_tys)
              | None -> error (Printf.sprintf "unknown type constructor: %s" n)))
        | Ast.TyRecord (fs, is_open) ->
          let fields = List.sort (fun (a,_) (b,_) -> String.compare a b)
            (List.map (fun (fn, ft) -> (fn, resolve_field ft)) fs) in
          let tail = if is_open then Types.new_rvar 0 else Types.REmpty in
          Types.TRecord (List.fold_right (fun (n, t) acc -> Types.RRow (n, t, acc)) fields tail)
        | Ast.TyQualified (path, n) ->
          let qualified = String.concat "." path ^ "." ^ n in
          resolve_field (Ast.TyName qualified)
        | Ast.TyPolyVariant (_kind, tags) ->
          let row = List.fold_right (fun (tag, payload_annot) acc ->
            Types.PVRow (tag, Option.map resolve_field payload_annot, acc)
          ) tags Types.PVEmpty in
          Types.TPolyVariant row
        | Ast.TyWithEffect (ty, _) -> resolve_field ty
      in
      (name, resolve_field annot)
    ) fields in
    let mut_fields = List.filter_map (fun (is_mut, name, _) ->
      if is_mut then Some name else None
    ) fields in
    let sorted_fields = List.sort (fun (a, _) (b, _) -> String.compare a b) typed_fields in
    let type_env = { ctx.type_env with
      records = (type_name, sorted_fields) :: ctx.type_env.records;
      mutable_fields = mut_fields @ ctx.type_env.mutable_fields;
      (* Also register as type synonym so parameterized record types
         can be used in type annotations like (string, int) Hashtbl.t *)
      type_synonyms = (type_name, num_params, Types.TRecord (Types.fields_to_closed_row sorted_fields)) :: ctx.type_env.type_synonyms;
    } in
    { ctx with type_env }
  | Ast.TDAlias annot ->
    let param_tbl = Hashtbl.create 4 in
    List.iteri (fun i name -> Hashtbl.replace param_tbl name i) type_params;
    let tvars = Hashtbl.create 4 in
    let rec resolve_alias = function
      | Ast.TyVar name ->
        (match Hashtbl.find_opt param_tbl name with
         | Some idx -> Types.TGen idx
         | None ->
           (match Hashtbl.find_opt tvars name with
            | Some tv -> tv
            | None ->
              let tv = Types.new_tvar 0 in
              Hashtbl.replace tvars name tv;
              tv))
      | Ast.TyName "int" -> Types.TInt
      | Ast.TyName "float" -> Types.TFloat
      | Ast.TyName "bool" -> Types.TBool
      | Ast.TyName "string" -> Types.TString
      | Ast.TyName "unit" -> Types.TUnit
      | Ast.TyName name ->
        let canonical = resolve_type_alias ctx.type_env name in
        (match List.find_opt (fun (n, _, _) -> String.equal n canonical) ctx.type_env.type_synonyms with
         | Some (_, 0, ty) -> ty
         | Some (_, n, _) -> error (Printf.sprintf "type %s expects %d type argument(s)" name n)
         | None ->
           (match find_variant_info ctx.type_env name with
            | Some (_, 0, _, _) -> Types.TVariant (canonical, [])
            | Some (_, n, _, _) -> error (Printf.sprintf "type %s expects %d type argument(s)" name n)
            | None ->
              if List.assoc_opt canonical ctx.type_env.records <> None then
                let fields = List.assoc canonical ctx.type_env.records in
                if fields = [] then Types.TRecord Types.RWild
                else Types.TRecord (Types.fields_to_closed_row fields)
              else
                error (Printf.sprintf "unknown type: %s" name)))
      | Ast.TyArrow (a, b, _) -> Types.TArrow (resolve_alias a, Types.EffEmpty, resolve_alias b)
      | Ast.TyTuple ts -> Types.TTuple (List.map resolve_alias ts)
      | Ast.TyList t -> Types.TList (resolve_alias t)
      | Ast.TyArray t -> Types.TArray (resolve_alias t)
      | Ast.TyMap (k, v) -> Types.TMap (resolve_alias k, resolve_alias v)
      | Ast.TyApp (args, tname) ->
        let canonical = resolve_type_alias ctx.type_env tname in
        let arg_tys = List.map resolve_alias args in
        (match List.find_opt (fun (n, _, _) -> String.equal n canonical) ctx.type_env.type_synonyms with
         | Some (_, np, ty) ->
           if List.length arg_tys <> np then
             error (Printf.sprintf "type %s expects %d type argument(s), got %d" tname np (List.length arg_tys));
           subst_tgens arg_tys ty
         | None ->
           (match find_variant_info ctx.type_env tname with
            | Some (_, np, _, _) ->
              if List.length arg_tys <> np then
                error (Printf.sprintf "type %s expects %d type argument(s), got %d" tname np (List.length arg_tys));
              Types.TVariant (canonical, arg_tys)
            | None -> error (Printf.sprintf "unknown type constructor: %s" tname)))
      | Ast.TyRecord (fields, is_open) ->
        let fields = List.map (fun (n, t) -> (n, resolve_alias t)) fields in
        let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
        let tail = if is_open then Types.new_rvar 0 else Types.REmpty in
        Types.TRecord (List.fold_right (fun (n, t) acc -> Types.RRow (n, t, acc)) fields tail)
      | Ast.TyQualified (path, name) ->
        let qualified = String.concat "." path ^ "." ^ name in
        resolve_alias (Ast.TyName qualified)
      | Ast.TyPolyVariant (_kind, tags) ->
        let row = List.fold_right (fun (tag, payload_annot) acc ->
          Types.PVRow (tag, Option.map resolve_alias payload_annot, acc)
        ) tags Types.PVEmpty in
        Types.TPolyVariant row
      | Ast.TyWithEffect (ty, _) -> resolve_alias ty
    in
    let resolved_ty = resolve_alias annot in
    let type_env = { ctx.type_env with
      type_synonyms = (type_name, num_params, resolved_ty) :: ctx.type_env.type_synonyms;
    } in
    { ctx with type_env }

let wrap_params_decl params ret_annot body =
  match ret_annot with
  | Some (Ast.TyWithEffect (ret_ty, eff_annot)) when params <> [] ->
    (* Effect annotation on return type: build full arrow type annotation *)
    let body = Ast.EAnnot (body, ret_ty) in
    let full_fun = List.fold_right (fun param body ->
      Ast.EFun (param, body)
    ) params body in
    (* Build the full arrow type with effect on the innermost arrow *)
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

(* ---- Type class processing ---- *)

let process_class_def ctx (class_name : string) (tyvar_names : string list)
    (fundep_annots : (string list * string list) list)
    (methods : (string * Ast.ty_annot) list) : ctx * tdecl =
  if List.exists (fun c -> String.equal c.Types.class_name class_name)
       ctx.type_env.Types.classes then
    error (Printf.sprintf "duplicate class definition: %s" class_name);
  let num_class_params = List.length tyvar_names in
  let method_tys = List.map (fun (mname, annot) ->
    let tvars = Hashtbl.create 4 in
    List.iteri (fun i name ->
      Hashtbl.replace tvars name (Types.TGen i)
    ) tyvar_names;
    let next_gen = ref num_class_params in
    (* Effect variable tracking for effect-polymorphic methods *)
    let eff_tvars : (string, Types.eff) Hashtbl.t = Hashtbl.create 2 in
    let next_effgen = ref 0 in
    let resolve_eff_var name =
      match Hashtbl.find_opt eff_tvars name with
      | Some eg -> eg
      | None ->
        let idx = !next_effgen in
        next_effgen := idx + 1;
        let eg = Types.EffGen idx in
        Hashtbl.replace eff_tvars name eg;
        eg
    in
    let rec resolve = function
      | Ast.TyVar name ->
        (match Hashtbl.find_opt tvars name with
         | Some tv -> tv
         | None ->
           (* Extra method-level type variable *)
           let idx = !next_gen in
           next_gen := idx + 1;
           Hashtbl.replace tvars name (Types.TGen idx);
           Types.TGen idx)
      | Ast.TyName "int" -> Types.TInt
      | Ast.TyName "float" -> Types.TFloat
      | Ast.TyName "bool" -> Types.TBool
      | Ast.TyName "string" -> Types.TString
      | Ast.TyName "unit" -> Types.TUnit
      | Ast.TyName name ->
        let canonical = resolve_type_alias ctx.type_env name in
        (match find_variant_info ctx.type_env name with
         | Some (_, 0, _, _) -> Types.TVariant (canonical, [])
         | Some (_, n, _, _) ->
           error (Printf.sprintf "type %s expects %d type argument(s)" name n)
         | None -> error (Printf.sprintf "unknown type: %s" name))
      | Ast.TyArrow (a, b, eff_opt) ->
        let eff = match eff_opt with
          | None -> Types.EffEmpty  (* default: class methods are pure *)
          | Some Ast.EffAnnotPure -> Types.EffEmpty
          | Some (Ast.EffAnnotRow items) ->
            resolve_class_eff_items items
        in
        Types.TArrow (resolve a, eff, resolve b)
      | Ast.TyTuple ts -> Types.TTuple (List.map resolve ts)
      | Ast.TyList t -> Types.TList (resolve t)
      | Ast.TyArray t -> Types.TArray (resolve t)
      | Ast.TyMap (k, v) -> Types.TMap (resolve k, resolve v)
      | Ast.TyApp (args, name) ->
        let canonical = resolve_type_alias ctx.type_env name in
        let arg_tys = List.map resolve args in
        (match find_variant_info ctx.type_env name with
         | Some (_, np, _, _) ->
           if List.length arg_tys <> np then
             error (Printf.sprintf "type %s expects %d type argument(s), got %d"
               name np (List.length arg_tys));
           Types.TVariant (canonical, arg_tys)
         | None -> error (Printf.sprintf "unknown type constructor: %s" name))
      | Ast.TyRecord (fields, is_open) ->
        let fields = List.sort (fun (a,_) (b,_) -> String.compare a b)
          (List.map (fun (n, t) -> (n, resolve t)) fields) in
        let tail = if is_open then Types.new_rvar 0 else Types.REmpty in
        Types.TRecord (List.fold_right (fun (n, t) acc -> Types.RRow (n, t, acc)) fields tail)
      | Ast.TyQualified (path, name) ->
        let qualified = String.concat "." path ^ "." ^ name in
        resolve (Ast.TyName qualified)
      | Ast.TyPolyVariant (_kind, tags) ->
        let row = List.fold_right (fun (tag, payload_annot) acc ->
          Types.PVRow (tag, Option.map resolve payload_annot, acc)
        ) tags Types.PVEmpty in
        Types.TPolyVariant row
      | Ast.TyWithEffect (ty, _) -> resolve ty
    and resolve_class_eff_items items =
      let labels = ref [] in
      let tail = ref Types.EffEmpty in
      List.iter (fun item ->
        match item with
        | Ast.EffLabel (name, param_annots) ->
          let resolved_params = List.map resolve param_annots in
          labels := (name, resolved_params) :: !labels
        | Ast.EffVar name ->
          tail := resolve_eff_var name
      ) items;
      List.fold_right (fun (name, params) acc ->
        Types.EffRow (name, params, acc)
      ) (List.rev !labels) !tail
    in
    let ty = resolve annot in
    let method_quant = !next_gen in
    let method_equant = !next_effgen in
    (mname, ty, method_quant, method_equant)
  ) methods in
  (* Convert string-based fundeps to index-based *)
  let class_fundeps = List.map (fun (from_names, to_names) ->
    let resolve_idx name =
      match List.find_index (fun n -> String.equal n name) tyvar_names with
      | Some i -> i
      | None -> error (Printf.sprintf "functional dependency type variable '%s not in class parameters" name)
    in
    Types.{ fd_from = List.map resolve_idx from_names;
            fd_to = List.map resolve_idx to_names }
  ) fundep_annots in
  let class_def = Types.{
    class_name;
    class_params = tyvar_names;
    class_methods = List.map (fun (m, t, _, _) -> (m, t)) method_tys;
    class_fundeps;
  } in
  let ctx = List.fold_left (fun ctx (mname, mty, mquant, mequant) ->
    let scheme = { Types.quant = mquant; equant = mequant; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = mty } in
    extend_var ctx mname scheme
  ) ctx method_tys in
  let type_env = { ctx.type_env with
    Types.classes = class_def :: ctx.type_env.classes;
  } in
  let ctx = { ctx with type_env } in
  (ctx, TDClass class_name)

let resolve_inst_tys ctx (annots : Ast.ty_annot list) =
  let tvars = Hashtbl.create 4 in
  let next_idx = ref 0 in
  let rec go = function
    | Ast.TyVar name ->
      (match Hashtbl.find_opt tvars name with
       | Some idx -> Types.TGen idx
       | None ->
         let idx = !next_idx in
         next_idx := idx + 1;
         Hashtbl.replace tvars name idx;
         Types.TGen idx)
    | Ast.TyName "int" -> Types.TInt
    | Ast.TyName "float" -> Types.TFloat
    | Ast.TyName "bool" -> Types.TBool
    | Ast.TyName "string" -> Types.TString
    | Ast.TyName "byte" -> Types.TByte
    | Ast.TyName "rune" -> Types.TRune
    | Ast.TyName "unit" -> Types.TUnit
    | Ast.TyName name ->
      let canonical = resolve_type_alias ctx.type_env name in
      (match find_variant_info ctx.type_env name with
       | Some (_, 0, _, _) -> Types.TVariant (canonical, [])
       | Some (_, n, _, _) ->
         error (Printf.sprintf "type %s expects %d type argument(s)" name n)
       | None ->
         if List.assoc_opt name ctx.type_env.records <> None then
           let fields = List.assoc name ctx.type_env.records in
           if fields = [] then Types.TRecord Types.RWild
           else Types.TRecord (Types.fields_to_closed_row fields)
         else
           error (Printf.sprintf "unknown type: %s" name))
    | Ast.TyArrow (a, b, _) -> Types.TArrow (go a, Types.EffEmpty, go b)
    | Ast.TyTuple ts -> Types.TTuple (List.map go ts)
    | Ast.TyList t -> Types.TList (go t)
    | Ast.TyArray t -> Types.TArray (go t)
    | Ast.TyMap (k, v) -> Types.TMap (go k, go v)
    | Ast.TyApp (args, name) ->
      let canonical = resolve_type_alias ctx.type_env name in
      let arg_tys = List.map go args in
      (match find_variant_info ctx.type_env name with
       | Some (_, np, _, _) ->
         if List.length arg_tys <> np then
           error (Printf.sprintf "type %s expects %d type argument(s), got %d"
             name np (List.length arg_tys));
         Types.TVariant (canonical, arg_tys)
       | None -> error (Printf.sprintf "unknown type constructor: %s" name))
    | Ast.TyRecord (fields, is_open) ->
      let fields = List.map (fun (n, t) -> (n, go t)) fields in
      let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
      let tail = if is_open then Types.new_rvar 0 else Types.REmpty in
      Types.TRecord (List.fold_right (fun (n, t) acc -> Types.RRow (n, t, acc)) fields tail)
    | Ast.TyQualified (path, name) ->
      let qualified = String.concat "." path ^ "." ^ name in
      go (Ast.TyName qualified)
    | Ast.TyPolyVariant (_kind, tags) ->
      let row = List.fold_right (fun (tag, payload_annot) acc ->
        Types.PVRow (tag, Option.map go payload_annot, acc)
      ) tags Types.PVEmpty in
      Types.TPolyVariant row
    | Ast.TyWithEffect (ty, _) -> go ty
  in
  let tys = List.map go annots in
  (tys, !next_idx, tvars)

let process_instance_def ctx level (class_name : string)
    (inst_ty_annots : Ast.ty_annot list)
    (constraints : Ast.constraint_ list)
    (methods : (string * Ast.param list * Ast.expr) list) : ctx * tdecl =
  let class_name = resolve_type_alias ctx.type_env class_name in
  let class_def = match List.find_opt
    (fun c -> String.equal c.Types.class_name class_name) ctx.type_env.Types.classes with
    | Some c -> c
    | None -> error (Printf.sprintf "unknown class: %s" class_name)
  in
  let num_class_params = List.length class_def.Types.class_params in
  (* Resolve instance types: TGen for storage, fresh tvars for typechecking *)
  let (stored_tys, num_inst_vars, inst_tvars) = resolve_inst_tys ctx inst_ty_annots in
  if List.length stored_tys <> num_class_params then
    error (Printf.sprintf "class %s expects %d type parameters, got %d"
      class_name num_class_params (List.length stored_tys));
  let dname = Types.dict_name class_name stored_tys in
  (* Create fresh tvars for typechecking method bodies *)
  let check_tvar_list = List.init num_inst_vars (fun _ -> Types.new_tvar (level + 1)) in
  let check_tys = List.map (fun t -> subst_tgens check_tvar_list t) stored_tys in
  if List.exists (fun i ->
    String.equal i.Types.inst_class class_name &&
    List.length i.Types.inst_tys = List.length stored_tys &&
    Types.match_partial_inst i.Types.inst_tys (List.map (fun t -> Some t) stored_tys)
  ) ctx.type_env.Types.instances then
    error (Printf.sprintf "duplicate instance %s for types %s"
      class_name (String.concat ", " (List.map Types.pp_ty stored_tys)));
  List.iter (fun (mname, _) ->
    if not (List.exists (fun (n, _, _) -> String.equal n mname) methods) then
      error (Printf.sprintf "instance %s %s missing method: %s"
        class_name (String.concat " " (List.map Types.pp_ty stored_tys)) mname)
  ) class_def.class_methods;
  let typed_methods = List.map (fun (mname, params, body) ->
    let method_schema_ty = match List.assoc_opt mname class_def.class_methods with
      | Some ty -> ty
      | None -> error (Printf.sprintf "%s is not a method of class %s" mname class_name)
    in
    let concrete_method_ty = subst_tgens check_tys method_schema_ty in
    (* Instantiate any remaining TGens (extra method-level type vars like 'c in fold) *)
    let max_tgen = max_tgen_in_ty concrete_method_ty in
    let max_effgen = max_effgen_in_ty concrete_method_ty in
    let quant = if max_tgen >= 0 then max_tgen + 1 else 0 in
    let equant = if max_effgen >= 0 then max_effgen + 1 else 0 in
    let concrete_method_ty =
      if quant > 0 || equant > 0 then
        Types.instantiate (level + 1) { Types.quant = quant; equant; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = concrete_method_ty }
      else
        concrete_method_ty
    in
    let full_body = wrap_params_decl params None body in
    let te = check ctx (level + 1) full_body concrete_method_ty in
    (mname, te)
  ) methods in
  let sorted_methods = List.sort (fun (a, _) (b, _) -> String.compare a b) typed_methods in
  let record_ty = Types.TRecord (Types.fields_to_closed_row (List.map (fun (n, te) -> (n, te.ty)) sorted_methods)) in
  let dict_expr = mk (TERecord sorted_methods) record_ty in
  (* Resolve instance constraints using the inst_tvars table (name -> TGen index) *)
  let inst_constraints = List.map (fun (cclass, tyvar_names) ->
    let cclass = resolve_type_alias ctx.type_env cclass in
    let cc_args = List.map (fun tvname ->
      match Hashtbl.find_opt inst_tvars tvname with
      | Some idx -> Types.CATGen idx
      | None -> error (Printf.sprintf "constraint type variable '%s not found in instance type parameters" tvname)
    ) tyvar_names in
    Types.{ cc_class = cclass; cc_args }
  ) constraints in
  let inst_def = Types.{
    inst_class = class_name;
    inst_tys = stored_tys;
    inst_dict_name = dname;
    inst_constraints;
  } in
  (* Check functional dependency consistency *)
  (try Types.check_fundep_consistency class_def inst_def ctx.type_env.Types.instances
   with Types.Unify_error msg -> error msg);
  let type_env = { ctx.type_env with
    Types.instances = inst_def :: ctx.type_env.instances;
  } in
  let ctx = { ctx with type_env } in
  (ctx, TDLet (dname, dict_expr))

(* ---- Constraint resolution for DLet/DLetRec ---- *)

(* Walk an annotation alongside a resolved type to extract tyvar name -> tvar id mapping *)
let extract_tyvar_ids annot ty =
  let (mapping : (string, int) Hashtbl.t) = Hashtbl.create 4 in
  let rec walk annot ty =
    match annot, Types.repr ty with
    | Ast.TyVar name, Types.TVar { contents = Types.Unbound (id, _) } ->
      if not (Hashtbl.mem mapping name) then
        Hashtbl.replace mapping name id
    | Ast.TyArrow (a1, a2, _), (Types.TArrow (t1, _, t2) | Types.TCont (t1, _, t2)) ->
      walk a1 t1; walk a2 t2
    | Ast.TyTuple anns, Types.TTuple tys when List.length anns = List.length tys ->
      List.iter2 walk anns tys
    | Ast.TyList a, Types.TList t -> walk a t
    | Ast.TyArray a, Types.TArray t -> walk a t
    | Ast.TyMap (k1, v1), Types.TMap (k2, v2) ->
      walk k1 k2; walk v1 v2
    | Ast.TyApp (args, _), Types.TVariant (_, tys) when List.length args = List.length tys ->
      List.iter2 walk args tys
    | Ast.TyWithEffect (a, _), _ -> walk a ty
    | _ -> ()
  in
  walk annot ty;
  mapping

(* Resolve AST constraints to class_constraints using param annotations and generalization *)
let resolve_let_constraints type_env level constraints params ret_annot te_ty shared_tvars =
  if constraints = [] then
    Types.generalize level te_ty
  else begin
    (* Build tyvar_name -> tvar_id mapping from param annotations *)
    let (name_to_tvar : (string, int) Hashtbl.t) = Hashtbl.create 4 in
    let merge_into tbl =
      Hashtbl.iter (fun k v ->
        if not (Hashtbl.mem name_to_tvar k) then
          Hashtbl.replace name_to_tvar k v
      ) tbl
    in
    (* Walk params alongside arrow type to extract tvar ids *)
    let rec walk_params params ty =
      match params, Types.repr ty with
      | p :: rest, (Types.TArrow (param_ty, _, ret_ty) | Types.TCont (param_ty, _, ret_ty)) ->
        (match p.Ast.annot with
         | Some annot -> merge_into (extract_tyvar_ids annot param_ty)
         | None -> ());
        walk_params rest ret_ty
      | _ -> ()
    in
    walk_params params te_ty;
    (* Also extract from return type annotation *)
    (match ret_annot with
     | Some annot ->
       let rec skip_arrows ty n =
         if n = 0 then ty
         else match Types.repr ty with
           | Types.TArrow (_, _, rest) | Types.TCont (_, _, rest) -> skip_arrows rest (n - 1)
           | _ -> ty
       in
       let ret_ty = skip_arrows te_ty (List.length params) in
       merge_into (extract_tyvar_ids annot ret_ty)
     | None -> ());
    (* Generalize with map to get tvar_id -> TGen index *)
    let (scheme, id_map) = Types.generalize_with_map level te_ty in
    (* For fundep-determined vars not in annotations, use phantom TGens *)
    let phantom_counter = ref scheme.Types.quant in
    let phantom_map = Hashtbl.create 4 in
    (* Resolve constraints *)
    let cc_list = List.map (fun (cclass, tyvar_names) ->
      let cclass = resolve_type_alias type_env cclass in
      let cc_args = List.map (fun tvname ->
        match Hashtbl.find_opt name_to_tvar tvname with
        | Some tvar_id ->
          (match Hashtbl.find_opt id_map tvar_id with
           | Some (Types.TGen idx) -> Types.CATGen idx
           | _ -> error (Printf.sprintf "constraint type variable '%s is not polymorphic" tvname))
        | None ->
          (* Not in annotations. Check shared_tvars for fundep-determined vars. *)
          (match Hashtbl.find_opt shared_tvars tvname with
           | Some tv ->
             (* Check if the tvar was generalized (e.g. after tree-walk unification) *)
             let tv_id = match Types.repr tv with
               | Types.TVar { contents = Types.Unbound (id, _) } -> Some id
               | _ -> None in
             let tgen_from_map = match tv_id with
               | Some id -> Hashtbl.find_opt id_map id
               | None -> None in
             (match tgen_from_map with
              | Some (Types.TGen idx) -> Types.CATGen idx
              | _ ->
                (* Allocate phantom TGen index, shared across constraints *)
                let idx = match Hashtbl.find_opt phantom_map tvname with
                  | Some idx -> idx
                  | None ->
                    let idx = !phantom_counter in
                    incr phantom_counter;
                    Hashtbl.replace phantom_map tvname idx;
                    idx in
                Types.CAPhantom (idx, tv))
           | None ->
             error (Printf.sprintf "constraint type variable '%s not found in function signature" tvname))
      ) tyvar_names in
      Types.{ cc_class = cclass; cc_args }
    ) constraints in
    { scheme with Types.constraints = cc_list }
  end

(* ---- Constrained function typechecking ---- *)
(* For functions with where clauses, we use a shared tvars table so all
   annotations share the same tvar for each type variable name. We also
   populate constraint_tvars on ctx so synth_binop doesn't default them to int. *)

(* After typechecking a constrained function body, walk the expression tree to
   unify constraint tvars (from shared_tvars) with the actual tvars created
   during body typechecking. This ensures phantom tvars share identity with the
   tvars used by class methods in the body, enabling xform_constrained_def to
   map phantom TGens to the correct body tvar_ids. *)
let unify_constraint_tvars type_env constraints shared_tvars body_te =
  let classes = type_env.Types.classes in
  let rec walk te =
    (match te.expr with
     | TEVar name ->
       (* Check if this is a class method *)
       let class_opt =
         match Types.find_method_class classes name with
         | Some _ as r -> r
         | None ->
           match String.rindex_opt name '.' with
           | None -> None
           | Some i ->
             let short = String.sub name (i + 1) (String.length name - i - 1) in
             Types.find_method_class classes short
       in
       (match class_opt with
        | Some class_def ->
          let method_name = match String.rindex_opt name '.' with
            | Some i -> String.sub name (i + 1) (String.length name - i - 1)
            | None -> name in
          (match List.assoc_opt method_name class_def.Types.class_methods with
           | Some method_schema_ty ->
             let num_params = List.length class_def.Types.class_params in
             let param_map = Hashtbl.create num_params in
             let rec go s r =
               let r = Types.repr r in
               match s, r with
               | Types.TGen i, _ when i < num_params ->
                 if not (Hashtbl.mem param_map i) then Hashtbl.replace param_map i r
               | Types.TArrow (s1, _, s2), Types.TArrow (r1, _, r2)
               | Types.TCont (s1, _, s2), Types.TCont (r1, _, r2) -> go s1 r1; go s2 r2
               | Types.TTuple ss, Types.TTuple rs when List.length ss = List.length rs ->
                 List.iter2 go ss rs
               | Types.TList s1, Types.TList r1 -> go s1 r1
               | Types.TArray s1, Types.TArray r1 -> go s1 r1
               | Types.TMap (sk, sv), Types.TMap (rk, rv) -> go sk rk; go sv rv
               | Types.TVariant (_, ss), Types.TVariant (_, rs)
                 when List.length ss = List.length rs ->
                 List.iter2 go ss rs
               | _ -> ()
             in
             go method_schema_ty te.ty;
             (* For each constraint matching this class, try to unify fundep-determined tvars *)
             List.iter (fun (cclass, tyvar_names) ->
               if String.equal cclass class_def.Types.class_name &&
                  List.length tyvar_names = num_params then begin
                 (* Check if "from" params match via physical equality *)
                 let from_match = List.for_all (fun (fd : Types.fundep) ->
                   List.for_all (fun fi ->
                     match Hashtbl.find_opt param_map fi with
                     | Some body_tv ->
                       let tvname = List.nth tyvar_names fi in
                       (match Hashtbl.find_opt shared_tvars tvname with
                        | Some constraint_tv ->
                          Types.repr body_tv == Types.repr constraint_tv
                        | None -> true)
                     | None -> true
                   ) fd.fd_from
                 ) class_def.Types.class_fundeps in
                 if from_match then
                   (* Unify "to" (fundep-determined) params *)
                   List.iter (fun (fd : Types.fundep) ->
                     List.iter (fun ti ->
                       match Hashtbl.find_opt param_map ti with
                       | Some body_tv ->
                         let tvname = List.nth tyvar_names ti in
                         (match Hashtbl.find_opt shared_tvars tvname with
                          | Some constraint_tv ->
                            (try Types.unify body_tv constraint_tv with _ -> ())
                          | None -> ())
                       | None -> ()
                     ) fd.fd_to
                   ) class_def.Types.class_fundeps
               end
             ) constraints
           | None -> ())
        | None -> ())
     | TEApp (f, x) -> walk f; walk x
     | TEFun (_, body, _) -> walk body
     | TELet (_, _, e1, e2) -> walk e1; walk e2
     | TELetRec (_, _, e1, e2) -> walk e1; walk e2
     | TEIf (c, t, e) -> walk c; walk t; walk e
     | TEBinop (_, a, b) -> walk a; walk b
     | TEUnop (_, a) -> walk a
     | TETuple es -> List.iter walk es
     | TERecord (fields) -> List.iter (fun (_, e) -> walk e) fields
     | TERecordUpdate (e, fields) -> walk e; List.iter (fun (_, e) -> walk e) fields
     | TERecordUpdateIdx (e, pairs) -> walk e; List.iter (fun (i, v) -> walk i; walk v) pairs
     | TEField (e, _) -> walk e
     | TEIndex (e1, e2) -> walk e1; walk e2
     | TECons (hd, tl) -> walk hd; walk tl
     | TEMatch (scrut, branches, _) ->
       walk scrut; List.iter (fun (_, guard, body) ->
         (match guard with Some g -> walk g | None -> ()); walk body) branches
     | TESeq (e1, e2) -> walk e1; walk e2
     | TELetMut (_, e1, e2) -> walk e1; walk e2
     | TEAssign (_, e) -> walk e
     | TEFieldAssign (e1, _, e2) -> walk e1; walk e2
     | TEConstruct (_, Some e) -> walk e
     | TEPerform (_, e) -> walk e
     | TEHandle (body, _) -> walk body
     | TEResume (e1, e2) -> walk e1; walk e2
     | TEWhile (cond, body) -> walk cond; walk body
     | TEForLoop body -> walk body
     | TEMap kvs -> List.iter (fun (k, v) -> walk k; walk v) kvs
     | TEArray es -> List.iter walk es
     | TEReturn e -> walk e
     | TEBreak e -> walk e
     | TEFoldContinue e -> walk e
     | TELetRecAnd (bindings, body) ->
       List.iter (fun (_, e) -> walk e) bindings; walk body
     | _ -> ()  (* TEInt, TEFloat, TEBool, TEString, TEByte, TERune, TEUnit, TENil, TEContinueLoop, TEConstruct None *)
    )
  in
  walk body_te

let synth_constrained_fn ctx level constraints params ret_annot body =
  let shared_tvars = Hashtbl.create 4 in
  (* Pre-allocate tvars for constraint type variables *)
  let constraint_tyvar_names =
    List.flatten (List.map (fun (_, names) -> names) constraints)
    |> List.sort_uniq String.compare in
  List.iter (fun tvname ->
    Hashtbl.replace shared_tvars tvname (Types.new_tvar (level + 1))
  ) constraint_tyvar_names;
  (* Collect constraint tvar IDs *)
  let constraint_tvar_ids = List.filter_map (fun tvname ->
    match Types.repr (Hashtbl.find shared_tvars tvname) with
    | Types.TVar { contents = Types.Unbound (id, _) } -> Some id
    | _ -> None
  ) constraint_tyvar_names in
  (* Resolve param types with shared tvars *)
  let param_tys = List.map (fun p ->
    match p.Ast.annot with
    | Some annot -> resolve_ty_annot_shared ctx (level + 1) shared_tvars annot
    | None -> Types.new_tvar (level + 1)
  ) params in
  (* Resolve return type with shared tvars *)
  let ret_ty_opt = match ret_annot with
    | Some annot -> Some (resolve_ty_annot_shared ctx (level + 1) shared_tvars annot)
    | None -> None
  in
  (* Build ctx with params and constraint_tvars *)
  let body_eff = Types.new_effvar (level + 1) in
  let inner_ctx = List.fold_left2 (fun c p ty ->
    extend_var_mono c p.Ast.name ty
  ) { ctx with constraint_tvars = constraint_tvar_ids @ ctx.constraint_tvars;
               current_eff = body_eff } params param_tys in
  (* Typecheck body *)
  let body_te = match ret_ty_opt with
    | Some ret_ty -> check inner_ctx (level + 1) body ret_ty
    | None -> synth inner_ctx (level + 1) body
  in
  (* Unify constraint tvars with body tvars for fundep-determined types *)
  unify_constraint_tvars ctx.type_env constraints shared_tvars body_te;
  (* Build TEFun chain — innermost arrow gets body_eff, outer arrows are pure *)
  let te = match List.rev params, List.rev param_tys with
    | [], [] -> body_te
    | last_p :: rest_ps, last_pty :: rest_ptys ->
      let inner = mk (TEFun (last_p.Ast.name, body_te, false))
        (Types.TArrow (last_pty, body_eff, body_te.ty)) in
      List.fold_left2 (fun acc p pty ->
        mk (TEFun (p.Ast.name, acc, false)) (Types.TArrow (pty, Types.EffEmpty, acc.ty))
      ) inner rest_ps rest_ptys
    | _ -> assert false
  in
  let scheme = resolve_let_constraints ctx.type_env level constraints params ret_annot te.ty shared_tvars in
  (te, scheme)

(* ---- Module processing ---- *)

let find_module_in_env type_env mod_name =
  (* Support dotted module paths like "Outer.Inner" *)
  match String.split_on_char '.' mod_name with
  | [] -> None
  | [name] -> List.assoc_opt name type_env.Types.modules
  | first :: rest ->
    (match List.assoc_opt first type_env.Types.modules with
     | None -> None
     | Some minfo ->
       let rec drill mi = function
         | [] -> Some mi
         | seg :: rest ->
           (match List.assoc_opt seg mi.Types.mod_submodules with
            | None -> None
            | Some sub -> drill sub rest)
       in
       drill minfo rest)

let open_module_into_ctx ctx mod_name names_opt =
  match find_module_in_env ctx.type_env mod_name with
  | None -> error (Printf.sprintf "unknown module: %s" mod_name)
  | Some minfo ->
    let filter name =
      match names_opt with
      | None -> true
      | Some names -> List.mem name names
    in
    (* Import pub vars (use extend_var_mutable for mutable ones) *)
    let ctx = List.fold_left (fun ctx (short, scheme) ->
      if filter short then
        if List.mem short minfo.Types.mod_pub_mutable_vars then
          extend_var_mutable ctx short scheme
        else
          extend_var ctx short scheme
      else ctx
    ) ctx minfo.Types.mod_pub_vars in
    (* Import pub constructors *)
    let type_env = List.fold_left (fun te (short, info) ->
      if filter short then
        { te with Types.constructors = (short, info) :: te.Types.constructors }
      else te
    ) ctx.type_env minfo.Types.mod_pub_constructors in
    (* Import pub types as short aliases *)
    let type_env = List.fold_left (fun te qname ->
      let short = match String.rindex_opt qname '.' with
        | Some i -> String.sub qname (i + 1) (String.length qname - i - 1)
        | None -> qname
      in
      if filter short then begin
        (* Copy variant/record under short name if not already present *)
        let te = match List.find_opt (fun (n, _, _, _) -> String.equal n qname) te.Types.variants with
          | Some (_, np, vdef, is_gadt) ->
            if List.exists (fun (n, _, _, _) -> String.equal n short) te.variants then te
            else { te with variants = (short, np, vdef, is_gadt) :: te.variants }
          | None -> te
        in
        let te = match List.assoc_opt qname te.Types.records with
          | Some fields ->
            if List.assoc_opt short te.records <> None then te
            else { te with records = (short, fields) :: te.records }
          | None -> te
        in
        let te = match List.find_opt (fun (n, _, _) -> String.equal n qname) te.Types.type_synonyms with
          | Some (_, np, expanded) ->
            if List.exists (fun (n, _, _) -> String.equal n short) te.type_synonyms then te
            else { te with type_synonyms = (short, np, expanded) :: te.type_synonyms }
          | None -> te
        in
        (* Add type alias so short name resolves to qualified name *)
        let te = if String.equal short qname then te
          else { te with Types.type_aliases = (short, qname) :: te.Types.type_aliases }
        in
        te
      end else te
    ) type_env minfo.Types.mod_pub_types in
    (* Import submodules *)
    let type_env = List.fold_left (fun te (sub_name, sub_info) ->
      if filter sub_name then
        { te with Types.modules = (sub_name, sub_info) :: te.Types.modules }
      else te
    ) type_env minfo.Types.mod_submodules in
    (* Import class aliases *)
    let type_env = List.fold_left (fun te qname ->
      let short = match String.rindex_opt qname '.' with
        | Some i -> String.sub qname (i + 1) (String.length qname - i - 1)
        | None -> qname
      in
      if filter short then
        { te with Types.type_aliases = (short, qname) :: te.Types.type_aliases }
      else te
    ) type_env minfo.Types.mod_pub_classes in
    { ctx with type_env }

let () = find_module_in_env_ref := find_module_in_env
let () = open_module_into_ctx_ref := open_module_into_ctx

(* ---- Deriving generation ---- *)

let rec collect_tyvars_annot = function
  | Ast.TyVar s -> [s]
  | Ast.TyArrow (a, b, _) -> collect_tyvars_annot a @ collect_tyvars_annot b
  | Ast.TyTuple ts -> List.concat_map collect_tyvars_annot ts
  | Ast.TyList t | Ast.TyArray t -> collect_tyvars_annot t
  | Ast.TyApp (args, _) -> List.concat_map collect_tyvars_annot args
  | Ast.TyMap (k, v) -> collect_tyvars_annot k @ collect_tyvars_annot v
  | Ast.TyRecord (fs, _) -> List.concat_map (fun (_, t) -> collect_tyvars_annot t) fs
  | Ast.TyName _ | Ast.TyQualified _ -> []
  | Ast.TyPolyVariant (_, tags) ->
    List.concat_map (fun (_, ty_opt) ->
      match ty_opt with Some t -> collect_tyvars_annot t | None -> []
    ) tags
  | Ast.TyWithEffect (ty, _) -> collect_tyvars_annot ty

let concat_str_exprs = function
  | [] -> Ast.EString ""
  | [e] -> e
  | first :: rest ->
    List.fold_left (fun acc e -> Ast.EBinop (Ast.Concat, acc, e)) first rest

let make_inst_ty type_params name =
  if type_params = [] then Ast.TyName name
  else Ast.TyApp (List.map (fun p -> Ast.TyVar p) type_params, name)

let make_constraints class_name type_params annots =
  let all_vars = List.concat_map (fun ann -> collect_tyvars_annot ann) annots in
  let unique_vars = List.sort_uniq String.compare all_vars in
  let param_vars = List.filter (fun v -> List.mem v type_params) unique_vars in
  List.map (fun v -> (class_name, [v])) param_vars

let gen_show_variant type_params name ctors =
  let inst_ty = make_inst_ty type_params name in
  let annots = List.filter_map (fun (_, a, _) -> a) ctors in
  let constraints = make_constraints "Show" type_params annots in
  let arms = List.map (fun (ctor_name, arg_annot, _) ->
    match arg_annot with
    | None ->
      (Ast.PatConstruct (ctor_name, None), None, Ast.EString ctor_name)
    | Some (Ast.TyTuple tys) ->
      let n = List.length tys in
      let vars = List.init n (fun i -> Printf.sprintf "__v%d" i) in
      let pat = Ast.PatConstruct (ctor_name,
        Some (Ast.PatTuple (List.map (fun v -> Ast.PatVar v) vars))) in
      let parts = [Ast.EString (ctor_name ^ "(")] @
        List.concat (List.mapi (fun i v ->
          let sv = Ast.EApp (Ast.EVar "show", Ast.EVar v) in
          if i = 0 then [sv] else [Ast.EString ", "; sv]
        ) vars) @ [Ast.EString ")"] in
      (pat, None, concat_str_exprs parts)
    | Some _ ->
      let pat = Ast.PatConstruct (ctor_name, Some (Ast.PatVar "__v0")) in
      let body = concat_str_exprs [
        Ast.EString (ctor_name ^ "(");
        Ast.EApp (Ast.EVar "show", Ast.EVar "__v0");
        Ast.EString ")"] in
      (pat, None, body)
  ) ctors in
  let body = Ast.EMatch (Ast.EVar "__x", arms, false) in
  ("Show", [inst_ty], constraints,
   [("show", [Ast.{name = "__x"; annot = None; is_generated = false}], body)])

let gen_show_record type_params name fields =
  let inst_ty = make_inst_ty type_params name in
  let annots = List.map (fun (_, _, a) -> a) fields in
  let constraints = make_constraints "Show" type_params annots in
  let parts = [Ast.EString "{ "] @
    List.concat (List.mapi (fun i (_, fname, _) ->
      let sf = Ast.EApp (Ast.EVar "show", Ast.EField (Ast.EVar "__x", fname)) in
      if i = 0 then [Ast.EString (fname ^ " = "); sf]
      else [Ast.EString ("; " ^ fname ^ " = "); sf]
    ) fields) @ [Ast.EString " }"] in
  let body = concat_str_exprs parts in
  ("Show", [inst_ty], constraints,
   [("show", [Ast.{name = "__x"; annot = None; is_generated = false}], body)])

let gen_eq_variant type_params name ctors =
  let inst_ty = make_inst_ty type_params name in
  let annots = List.filter_map (fun (_, a, _) -> a) ctors in
  let constraints = make_constraints "Eq" type_params annots in
  let and_exprs = function
    | [] -> Ast.EBool true
    | [e] -> e
    | first :: rest -> List.fold_left (fun a e -> Ast.EBinop (Ast.And, a, e)) first rest
  in
  let arms = List.map (fun (ctor_name, arg_annot, _) ->
    match arg_annot with
    | None ->
      let pat = Ast.PatTuple [
        Ast.PatConstruct (ctor_name, None);
        Ast.PatConstruct (ctor_name, None)] in
      (pat, None, Ast.EBool true)
    | Some (Ast.TyTuple tys) ->
      let n = List.length tys in
      let vs = List.init n (fun i -> Printf.sprintf "__v%d" i) in
      let ws = List.init n (fun i -> Printf.sprintf "__w%d" i) in
      let lp = Ast.PatConstruct (ctor_name,
        Some (Ast.PatTuple (List.map (fun v -> Ast.PatVar v) vs))) in
      let rp = Ast.PatConstruct (ctor_name,
        Some (Ast.PatTuple (List.map (fun v -> Ast.PatVar v) ws))) in
      let eqs = List.map2 (fun v w ->
        Ast.EBinop (Ast.Eq, Ast.EVar v, Ast.EVar w)) vs ws in
      (Ast.PatTuple [lp; rp], None, and_exprs eqs)
    | Some _ ->
      let lp = Ast.PatConstruct (ctor_name, Some (Ast.PatVar "__v0")) in
      let rp = Ast.PatConstruct (ctor_name, Some (Ast.PatVar "__w0")) in
      (Ast.PatTuple [lp; rp], None, Ast.EBinop (Ast.Eq, Ast.EVar "__v0", Ast.EVar "__w0"))
  ) ctors in
  let arms = arms @ [(Ast.PatWild, None, Ast.EBool false)] in
  let eq_body = Ast.EMatch (Ast.ETuple [Ast.EVar "__a"; Ast.EVar "__b"], arms, false) in
  let neq_body = Ast.EUnop (Ast.Not,
    Ast.EMatch (Ast.ETuple [Ast.EVar "__a"; Ast.EVar "__b"],
      List.map (fun (p, g, e) -> (p, g, e)) arms, false)) in
  ("Eq", [inst_ty], constraints,
   [("=", [Ast.{name="__a"; annot=None; is_generated=false}; Ast.{name="__b"; annot=None; is_generated=false}], eq_body);
    ("<>", [Ast.{name="__a"; annot=None; is_generated=false}; Ast.{name="__b"; annot=None; is_generated=false}], neq_body)])

let gen_eq_record type_params name fields =
  let inst_ty = make_inst_ty type_params name in
  let annots = List.map (fun (_, _, a) -> a) fields in
  let constraints = make_constraints "Eq" type_params annots in
  let eqs = List.map (fun (_, fname, _) ->
    Ast.EBinop (Ast.Eq,
      Ast.EField (Ast.EVar "__a", fname),
      Ast.EField (Ast.EVar "__b", fname))
  ) fields in
  let eq_body = match eqs with
    | [] -> Ast.EBool true
    | [e] -> e
    | first :: rest -> List.fold_left (fun a e -> Ast.EBinop (Ast.And, a, e)) first rest
  in
  let neq_body = Ast.EUnop (Ast.Not, eq_body) in
  ("Eq", [inst_ty], constraints,
   [("=", [Ast.{name="__a"; annot=None; is_generated=false}; Ast.{name="__b"; annot=None; is_generated=false}], eq_body);
    ("<>", [Ast.{name="__a"; annot=None; is_generated=false}; Ast.{name="__b"; annot=None; is_generated=false}], neq_body)])

let generate_derived_instance type_params name def class_name =
  match class_name, def with
  | _, Ast.TDVariant ctors when List.exists (fun (_, _, ret) -> ret <> None) ctors ->
    error (Printf.sprintf "cannot derive %s for GADT type %s" class_name name)
  | "Show", Ast.TDVariant ctors -> Some (gen_show_variant type_params name ctors)
  | "Show", Ast.TDRecord fields -> Some (gen_show_record type_params name fields)
  | "Eq", Ast.TDVariant ctors -> Some (gen_eq_variant type_params name ctors)
  | "Eq", Ast.TDRecord fields -> Some (gen_eq_record type_params name fields)
  | _ -> None

let rec process_module_def ctx level mod_name (items : Ast.module_decl list) =
  let prefix = match ctx.current_module with
    | None -> mod_name ^ "."
    | Some parent -> parent ^ mod_name ^ "."
  in
  (* Sub-context: inherits parent, has current_module set *)
  let sub_ctx = { ctx with current_module = Some prefix } in
  (* Accumulators for module_info *)
  let pub_vars = ref [] in
  let pub_mutable_vars = ref [] in
  let pub_types = ref [] in
  let opaque_types = ref [] in
  let pub_constructors = ref [] in
  let all_instances = ref [] in
  let submodules = ref [] in
  let pub_classes = ref [] in
  let typed_decls = ref [] in
  (* Process each module body item *)
  let sub_ctx = List.fold_left (fun sub_ctx (item : Ast.module_decl) ->
    let (sub_ctx', tdecl) = check_module_item sub_ctx level prefix item
      pub_vars pub_mutable_vars pub_types opaque_types pub_constructors all_instances submodules pub_classes typed_decls in
    typed_decls := tdecl :: !typed_decls;
    sub_ctx'
  ) sub_ctx items in
  (* Build module_info *)
  let minfo = Types.{
    mod_name;
    mod_pub_vars = !pub_vars;
    mod_pub_mutable_vars = !pub_mutable_vars;
    mod_pub_types = !pub_types;
    mod_opaque_types = !opaque_types;
    mod_pub_constructors = !pub_constructors;
    mod_instances = !all_instances;
    mod_submodules = !submodules;
    mod_pub_classes = !pub_classes;
  } in
  (* Add module to outer type_env, starting from sub_ctx to preserve inner registrations *)
  let outer_type_env = { sub_ctx.type_env with
    Types.modules = (mod_name, minfo) :: ctx.type_env.Types.modules;
  } in
  (* Don't leak type aliases to outer scope *)
  let outer_type_env = { outer_type_env with
    Types.type_aliases = ctx.type_env.Types.type_aliases;
  } in
  (* Also add all qualified pub vars to outer ctx *)
  let outer_vars = List.fold_left (fun vars (short, scheme) ->
    let qualified = prefix ^ short in
    (qualified, scheme) :: vars
  ) ctx.vars !pub_vars in
  (* Add qualified pub constructors *)
  let outer_type_env = List.fold_left (fun te (short, info) ->
    let qualified = prefix ^ short in
    { te with Types.constructors = (qualified, info) :: te.Types.constructors }
  ) outer_type_env !pub_constructors in
  (* Add instances to outer env — deduplicate since sub_ctx already has them *)
  let existing_dict_names = List.map (fun i -> i.Types.inst_dict_name) outer_type_env.Types.instances in
  let new_insts = List.filter (fun inst ->
    not (List.mem inst.Types.inst_dict_name existing_dict_names)
  ) !all_instances in
  let outer_type_env = { outer_type_env with
    Types.instances = new_insts @ outer_type_env.Types.instances;
  } in
  (* Add qualified pub mutable vars to outer mutable_vars *)
  let outer_mutable_vars = List.fold_left (fun mvs short ->
    let qualified = prefix ^ short in
    qualified :: mvs
  ) ctx.mutable_vars !pub_mutable_vars in
  let outer_ctx = { ctx with
    vars = outer_vars;
    mutable_vars = outer_mutable_vars;
    type_env = outer_type_env;
  } in
  (outer_ctx, TDModule (mod_name, List.rev !typed_decls))

and check_module_item sub_ctx level prefix (item : Ast.module_decl)
    pub_vars pub_mutable_vars pub_types opaque_types pub_constructors all_instances submodules pub_classes typed_decls =
  let decl = item.decl in
  match decl with
  | Ast.DModule (inner_name, inner_items) ->
    let (sub_ctx', tdecl) = process_module_def sub_ctx level inner_name inner_items in
    (* If pub, export submodule and its pub vars *)
    (match item.vis with
     | Ast.Public ->
       (match find_module_in_env sub_ctx'.type_env inner_name with
        | Some inner_info ->
          submodules := (inner_name, inner_info) :: !submodules;
          (* Also export inner module's pub vars so they're visible as Outer.Inner.f *)
          List.iter (fun (short, scheme) ->
            pub_vars := (inner_name ^ "." ^ short, scheme) :: !pub_vars
          ) inner_info.Types.mod_pub_vars;
          (* Export inner module's pub constructors *)
          List.iter (fun (short, info) ->
            pub_constructors := (inner_name ^ "." ^ short, info) :: !pub_constructors
          ) inner_info.Types.mod_pub_constructors
        | None -> ());
     | _ -> ());
    (sub_ctx', tdecl)
  | Ast.DType (type_params, name, def, deriving) ->
    let qualified_name = prefix ^ name in
    (* Add type alias BEFORE processing so GADT return types can resolve short names *)
    let sub_ctx_with_alias = { sub_ctx with type_env = { sub_ctx.type_env with
      Types.type_aliases = (name, qualified_name) :: sub_ctx.type_env.Types.type_aliases
    }} in
    (* Process type under qualified name *)
    let sub_ctx' = process_type_def sub_ctx_with_alias type_params qualified_name def in
    (match item.vis with
     | Ast.Public ->
       pub_types := qualified_name :: !pub_types;
       (* Export constructors with qualified type name *)
       (match def with
        | Ast.TDVariant ctors ->
          List.iter (fun (ctor_name, _, _) ->
            match List.assoc_opt ctor_name sub_ctx'.type_env.Types.constructors with
            | Some info ->
              let qual_info = { info with Types.ctor_type_name = qualified_name } in
              pub_constructors := (ctor_name, qual_info) :: !pub_constructors
            | None -> ()
          ) ctors
        | Ast.TDRecord _ | Ast.TDAlias _ -> ())
     | Ast.Opaque ->
       pub_types := qualified_name :: !pub_types;
       opaque_types := qualified_name :: !opaque_types
       (* Don't export constructors *)
     | Ast.Private -> ());
    (* Process deriving — use short name so type resolution works *)
    let sub_ctx' = List.fold_left (fun ctx cls ->
      match generate_derived_instance type_params name def cls with
      | Some (class_name, inst_tys, constraints, methods) ->
        let (ctx', tdecl) = process_instance_def ctx level class_name inst_tys constraints methods in
        typed_decls := tdecl :: !typed_decls;
        all_instances := ctx'.type_env.Types.instances;
        ctx'
      | None ->
        error (Printf.sprintf "cannot derive %s for type %s" cls name)
    ) sub_ctx' deriving in
    (sub_ctx', TDType (qualified_name, def))
  | Ast.DLet (name, params, ret_annot, constraints, body) ->
    let qualified_name = prefix ^ name in
    let (te, scheme) =
      if constraints <> [] then
        synth_constrained_fn sub_ctx level constraints params ret_annot body
      else begin
        let full_body = wrap_params_decl params ret_annot body in
        let te = synth sub_ctx (level + 1) full_body in
        improve_fundeps_in_expr sub_ctx.vars sub_ctx.type_env te;
        let scheme =
          if params = [] && not (is_syntactic_value body) then mono_scheme te.ty
          else resolve_let_constraints Types.empty_type_env level [] params ret_annot te.ty (Hashtbl.create 0) in
        let scheme = infer_implicit_constraints "" sub_ctx.type_env sub_ctx.vars te scheme in
        let scheme = infer_record_evidence sub_ctx.vars te scheme in
        (te, scheme)
      end
    in
    let sub_ctx' = extend_var sub_ctx name scheme in
    let sub_ctx' = extend_var sub_ctx' qualified_name scheme in
    (match item.vis with
     | Ast.Public -> pub_vars := (name, scheme) :: !pub_vars
     | _ -> ());
    (sub_ctx', TDLet (qualified_name, te))
  | Ast.DLetMut (name, body) ->
    let qualified_name = prefix ^ name in
    let te = synth sub_ctx (level + 1) body in
    let scheme = mono_scheme te.ty in
    let sub_ctx' = extend_var_mutable sub_ctx name scheme in
    let sub_ctx' = extend_var_mutable sub_ctx' qualified_name scheme in
    (match item.vis with
     | Ast.Public ->
       pub_vars := (name, scheme) :: !pub_vars;
       pub_mutable_vars := name :: !pub_mutable_vars
     | _ -> ());
    (sub_ctx', TDLetMut (qualified_name, te))
  | Ast.DLetRec (name, type_params, params, ret_annot, constraints, body) ->
    let qualified_name = prefix ^ name in
    let (fn_te, scheme) =
      if type_params <> [] then
        synth_poly_rec_fn sub_ctx level name qualified_name type_params params ret_annot body
      else if constraints <> [] then begin
        let fn_var = Types.new_tvar (level + 1) in
        let sub_ctx_self = extend_var_mono sub_ctx name fn_var in
        let sub_ctx_self = extend_var_mono sub_ctx_self qualified_name fn_var in
        let (te, scheme) = synth_constrained_fn sub_ctx_self level constraints params ret_annot body in
        try_unify fn_var te.ty;
        (te, scheme)
      end else begin
        let full_body = wrap_params_decl params ret_annot body in
        let fn_var = Types.new_tvar (level + 1) in
        let sub_ctx' = extend_var_mono sub_ctx name fn_var in
        let sub_ctx' = extend_var_mono sub_ctx' qualified_name fn_var in
        let fn_te = synth sub_ctx' (level + 1) full_body in
        try_unify fn_var fn_te.ty;
        improve_fundeps_in_expr sub_ctx.vars sub_ctx.type_env fn_te;
        let scheme = resolve_let_constraints Types.empty_type_env level [] params ret_annot fn_te.ty (Hashtbl.create 0) in
        let scheme = infer_implicit_constraints name sub_ctx.type_env sub_ctx.vars fn_te scheme in
        let scheme = infer_record_evidence sub_ctx.vars fn_te scheme in
        (fn_te, scheme)
      end
    in
    let sub_ctx'' = extend_var sub_ctx name scheme in
    let sub_ctx'' = extend_var sub_ctx'' qualified_name scheme in
    (match item.vis with
     | Ast.Public -> pub_vars := (name, scheme) :: !pub_vars
     | _ -> ());
    (sub_ctx'', TDLetRec (qualified_name, fn_te))
  | Ast.DClass (name, tyvars, fundeps, methods) ->
    let qualified = prefix ^ name in
    let (sub_ctx', tdecl) = process_class_def sub_ctx qualified tyvars fundeps methods in
    (* Add alias so unqualified name resolves inside the module *)
    let sub_ctx' = { sub_ctx' with type_env = { sub_ctx'.type_env with
      Types.type_aliases = (name, qualified) :: sub_ctx'.type_env.Types.type_aliases }} in
    (match item.vis with
     | Ast.Public ->
       pub_classes := qualified :: !pub_classes;
       (* Export class methods as pub vars *)
       List.iter (fun (mname, _) ->
         match List.assoc_opt mname sub_ctx'.vars with
         | Some scheme -> pub_vars := (mname, scheme) :: !pub_vars
         | None -> ()
       ) methods
     | _ -> ());
    (sub_ctx', tdecl)
  | Ast.DInstance (class_name, inst_ty_annots, constraints, methods) ->
    let (sub_ctx', tdecl) = process_instance_def sub_ctx level class_name inst_ty_annots constraints methods in
    (* Instances are always public *)
    let new_insts = List.filter (fun inst ->
      not (List.exists (fun prev ->
        String.equal prev.Types.inst_dict_name inst.Types.inst_dict_name
      ) sub_ctx.type_env.Types.instances)
    ) sub_ctx'.type_env.Types.instances in
    all_instances := new_insts @ !all_instances;
    (sub_ctx', tdecl)
  | Ast.DEffect (name, type_params, ops) ->
    let tvars = Hashtbl.create 4 in
    List.iteri (fun i p -> Hashtbl.replace tvars p (Types.TGen i)) type_params;
    let resolved_ops = List.map (fun (op_name, annot) ->
      let ty = resolve_ty_annot_shared sub_ctx 0 tvars annot in
      (op_name, ty)
    ) ops in
    let effect_def = Types.{
      effect_name = name;
      effect_params = type_params;
      effect_ops = resolved_ops;
    } in
    let type_env = { sub_ctx.type_env with effects = effect_def :: sub_ctx.type_env.effects } in
    let sub_ctx' = { sub_ctx with type_env } in
    (sub_ctx', TDEffect name)
  | Ast.DExtern (name, ty_annot) ->
    let qualified_name = prefix ^ name in
    let tvars = Hashtbl.create 4 in
    let ty = resolve_ty_annot_shared sub_ctx 1 tvars ty_annot in
    let scheme = Types.generalize 0 ty in
    let sub_ctx' = extend_var sub_ctx name scheme in
    let sub_ctx' = extend_var sub_ctx' qualified_name scheme in
    (match item.vis with
     | Ast.Public -> pub_vars := (name, scheme) :: !pub_vars
     | _ -> ());
    (sub_ctx', TDExtern (qualified_name, scheme))
  | Ast.DExpr expr ->
    let te = synth sub_ctx level expr in
    (sub_ctx, TDExpr te)
  | Ast.DOpen (mod_name, names_opt) ->
    let sub_ctx' = open_module_into_ctx sub_ctx mod_name names_opt in
    (* Inside a module, create qualified aliases so opened names resolve correctly *)
    let minfo = match find_module_in_env sub_ctx.type_env mod_name with
      | Some m -> m | None -> error (Printf.sprintf "unknown module: %s" mod_name)
    in
    let class_method_names = List.concat_map (fun qname ->
      match List.find_opt (fun (c : Types.class_def) ->
        String.equal c.class_name qname
      ) sub_ctx.type_env.Types.classes with
      | Some cls -> List.map fst cls.class_methods | None -> []
    ) minfo.Types.mod_pub_classes in
    let filter name = match names_opt with
      | None -> true | Some names -> List.mem name names
    in
    let alias_pairs = List.filter_map (fun (short, scheme) ->
      if filter short && not (List.mem short class_method_names) then begin
        let qualified_dst = prefix ^ short in
        let qualified_src = mod_name ^ "." ^ short in
        (* Add module-qualified name to vars so EVar resolution finds it *)
        ignore scheme;
        Some (qualified_dst, qualified_src)
      end else None
    ) minfo.Types.mod_pub_vars in
    (* Also extend vars with module-qualified names for EVar resolution *)
    let sub_ctx' = List.fold_left (fun ctx (short, scheme) ->
      if filter short && not (List.mem short class_method_names) then
        extend_var ctx (prefix ^ short) scheme
      else ctx
    ) sub_ctx' minfo.Types.mod_pub_vars in
    (sub_ctx', TDOpen alias_pairs)
  | Ast.DLetRecAnd bindings ->
    let has_poly = List.exists (fun (_, tp, _, _, _, _) -> tp <> []) bindings in
    if has_poly then begin
      let shared_tvars_list = List.map (fun (_, type_params, params, ret_annot, _, _) ->
        let shared_tvars = Hashtbl.create 4 in
        List.iter (fun tp ->
          Hashtbl.replace shared_tvars tp (Types.new_tvar (level + 1))
        ) type_params;
        let param_tys = List.map (fun (p : Ast.param) ->
          match p.annot with
          | Some annot -> resolve_ty_annot_shared sub_ctx (level + 1) shared_tvars annot
          | None -> Types.new_tvar (level + 1)
        ) params in
        let (ret_ty, body_eff) = match ret_annot with
          | Some (Ast.TyWithEffect (ty, eff_annot)) ->
            let ty' = resolve_ty_annot_shared sub_ctx (level + 1) shared_tvars ty in
            let eff = match eff_annot with
              | Ast.EffAnnotPure -> Types.EffEmpty
              | Ast.EffAnnotRow items -> resolve_eff_items sub_ctx (level + 1) shared_tvars items
            in (ty', eff)
          | Some annot ->
            (resolve_ty_annot_shared sub_ctx (level + 1) shared_tvars annot,
             Types.new_effvar (level + 1))
          | None ->
            (Types.new_tvar (level + 1), Types.new_effvar (level + 1))
        in
        let fn_ty = match List.rev param_tys with
          | [] -> ret_ty
          | last :: rest ->
            let inner = Types.TArrow (last, body_eff, ret_ty) in
            List.fold_left (fun acc pty ->
              Types.TArrow (pty, Types.EffEmpty, acc)
            ) inner rest
        in
        (shared_tvars, param_tys, ret_ty, body_eff, fn_ty)
      ) bindings in
      let schemes = List.map (fun (_, _, _, _, fn_ty) ->
        Types.generalize level fn_ty
      ) shared_tvars_list in
      (* Bind both short and qualified names *)
      let sub_ctx' = List.fold_left2 (fun ctx (name, _, _, _, _, _) scheme ->
        let qualified_name = prefix ^ name in
        let ctx = extend_var ctx name scheme in
        extend_var ctx qualified_name scheme
      ) sub_ctx bindings schemes in
      let body_tes = List.map2 (fun (_, _, params, _, _, body) (_, param_tys, ret_ty, body_eff, _) ->
        let inner_ctx = List.fold_left2 (fun c (p : Ast.param) ty ->
          extend_var_mono c p.name ty
        ) { sub_ctx' with current_eff = body_eff } params param_tys in
        let body_te = check inner_ctx (level + 1) body ret_ty in
        match List.rev params, List.rev param_tys with
        | [], [] -> body_te
        | last_p :: rest_ps, last_pty :: rest_ptys ->
          let inner = mk (TEFun (last_p.Ast.name, body_te, false))
            (Types.TArrow (last_pty, body_eff, body_te.ty)) in
          List.fold_left2 (fun acc (p : Ast.param) pty ->
            mk (TEFun (p.name, acc, false)) (Types.TArrow (pty, Types.EffEmpty, acc.ty))
          ) inner rest_ps rest_ptys
        | _ -> assert false
      ) bindings shared_tvars_list in
      let sub_ctx'' = List.fold_left2 (fun ctx (name, _, _, _, _, _) scheme ->
        let qualified_name = prefix ^ name in
        let ctx = extend_var ctx name scheme in
        extend_var ctx qualified_name scheme
      ) sub_ctx bindings schemes in
      let typed_bindings = List.map2 (fun (name, _, _, _, _, _) te ->
        let qualified_name = prefix ^ name in
        (qualified_name, te)
      ) bindings body_tes in
      (* Track visibility *)
      List.iter (fun (name, _, _, _, _, _) ->
        let scheme = List.assoc name (List.map2 (fun (n, _, _, _, _, _) s -> (n, s)) bindings schemes) in
        (match item.vis with
         | Ast.Public -> pub_vars := (name, scheme) :: !pub_vars
         | _ -> ())
      ) bindings;
      (sub_ctx'', TDLetRecAnd typed_bindings)
    end else begin
      let fn_vars = List.map (fun (name, _, _, _, _, _) ->
        (name, Types.new_tvar (level + 1))
      ) bindings in
      (* Bind both short and qualified names for self-recursion *)
      let sub_ctx' = List.fold_left (fun ctx (name, tv) ->
        let qualified_name = prefix ^ name in
        let ctx = extend_var_mono ctx name tv in
        extend_var_mono ctx qualified_name tv
      ) sub_ctx fn_vars in
      let body_tes = List.map2 (fun (name, _, params, ret_annot, constraints, body) (_, tv) ->
        ignore name;
        if constraints <> [] then
          let (te, _scheme) = synth_constrained_fn sub_ctx' level constraints params ret_annot body in
          try_unify tv te.ty;
          te
        else begin
          let full_body = wrap_params_decl params ret_annot body in
          let te = synth sub_ctx' (level + 1) full_body in
          try_unify tv te.ty;
          te
        end
      ) bindings fn_vars in
      let sub_ctx'' = List.fold_left2 (fun ctx (name, _, _, _, _, _) (_, tv) ->
        let qualified_name = prefix ^ name in
        let scheme = Types.generalize level tv in
        let ctx = extend_var ctx name scheme in
        extend_var ctx qualified_name scheme
      ) sub_ctx bindings fn_vars in
      let typed_bindings = List.map2 (fun (name, _, _, _, _, _) te ->
        let qualified_name = prefix ^ name in
        (qualified_name, te)
      ) bindings body_tes in
      (* Track visibility *)
      List.iter2 (fun (name, _, _, _, _, _) (_, tv) ->
        let scheme = Types.generalize level tv in
        (match item.vis with
         | Ast.Public -> pub_vars := (name, scheme) :: !pub_vars
         | _ -> ())
      ) bindings fn_vars;
      (sub_ctx'', TDLetRecAnd typed_bindings)
    end
  | Ast.DTypeAnd type_defs ->
    (* Pass 1: register type aliases and placeholders *)
    let sub_ctx' = List.fold_left (fun ctx (type_params, name, def, _deriving) ->
      let qualified_name = prefix ^ name in
      let num_params = List.length type_params in
      let ctx = { ctx with type_env = { ctx.type_env with
        Types.type_aliases = (name, qualified_name) :: ctx.type_env.Types.type_aliases
      }} in
      match def with
      | Ast.TDVariant _ ->
        { ctx with type_env = { ctx.type_env with
          variants = (qualified_name, num_params, [], false) :: ctx.type_env.variants;
        }}
      | Ast.TDRecord _ ->
        { ctx with type_env = { ctx.type_env with
          records = (qualified_name, []) :: ctx.type_env.records;
        }}
      | Ast.TDAlias _ -> ctx
    ) sub_ctx type_defs in
    (* Pass 2a: process record types first so their fields are populated
       before variant constructors reference them *)
    let sub_ctx' = List.fold_left (fun ctx (type_params, name, def, _deriving) ->
      match def with
      | Ast.TDRecord _ ->
        let qualified_name = prefix ^ name in
        process_type_def ctx type_params qualified_name def
      | _ -> ctx
    ) sub_ctx' type_defs in
    (* Pass 2b: process variant and alias types *)
    let sub_ctx' = List.fold_left (fun ctx (type_params, name, def, _deriving) ->
      match def with
      | Ast.TDRecord _ -> ctx
      | _ ->
        let qualified_name = prefix ^ name in
        process_type_def ctx type_params qualified_name def
    ) sub_ctx' type_defs in
    (* Visibility *)
    List.iter (fun (_type_params, name, def, _deriving) ->
      let qualified_name = prefix ^ name in
      match item.vis with
      | Ast.Public ->
        pub_types := qualified_name :: !pub_types;
        (match def with
         | Ast.TDVariant ctors ->
           List.iter (fun (ctor_name, _, _) ->
             match List.assoc_opt ctor_name sub_ctx'.type_env.Types.constructors with
             | Some info ->
               let qual_info = { info with Types.ctor_type_name = qualified_name } in
               pub_constructors := (ctor_name, qual_info) :: !pub_constructors
             | None -> ()
           ) ctors
         | Ast.TDRecord _ | Ast.TDAlias _ -> ())
      | Ast.Opaque ->
        pub_types := qualified_name :: !pub_types;
        opaque_types := qualified_name :: !opaque_types
      | Ast.Private -> ()
    ) type_defs;
    (* Handle deriving for each type *)
    let sub_ctx' = List.fold_left (fun ctx (type_params, name, def, deriving) ->
      List.fold_left (fun ctx cls ->
        match generate_derived_instance type_params name def cls with
        | Some (class_name, inst_tys, constraints, methods) ->
          let (ctx', tdecl) = process_instance_def ctx level class_name inst_tys constraints methods in
          typed_decls := tdecl :: !typed_decls;
          all_instances := ctx'.type_env.Types.instances;
          ctx'
        | None ->
          error (Printf.sprintf "cannot derive %s for type %s" cls name)
      ) ctx deriving
    ) sub_ctx' type_defs in
    (* Return: push all but last TDType to typed_decls, return last *)
    let type_decls = List.map (fun (_tp, name, def, _d) ->
      TDType (prefix ^ name, def)
    ) type_defs in
    let rec push_all_but_last = function
      | [] -> failwith "empty DTypeAnd"
      | [last] -> last
      | hd :: tl -> typed_decls := hd :: !typed_decls; push_all_but_last tl
    in
    let last_tdecl = push_all_but_last type_decls in
    (sub_ctx', last_tdecl)

let process_open ctx mod_name names_opt =
  let minfo = match find_module_in_env ctx.type_env mod_name with
    | Some m -> m
    | None -> error (Printf.sprintf "unknown module: %s" mod_name)
  in
  (* Collect class method names to skip (they have no runtime globals) *)
  let class_method_names =
    List.concat_map (fun qname ->
      match List.find_opt (fun (c : Types.class_def) ->
        String.equal c.class_name qname
      ) ctx.type_env.Types.classes with
      | Some cls -> List.map fst cls.class_methods
      | None -> []
    ) minfo.Types.mod_pub_classes
  in
  let alias_pairs = ref [] in
  let filter name =
    match names_opt with
    | None -> true
    | Some names -> List.mem name names
  in
  List.iter (fun (short, _scheme) ->
    if filter short && not (List.mem short class_method_names) then begin
      let qualified = mod_name ^ "." ^ short in
      alias_pairs := (short, qualified) :: !alias_pairs
    end
  ) minfo.Types.mod_pub_vars;
  let ctx' = open_module_into_ctx ctx mod_name names_opt in
  (ctx', TDOpen !alias_pairs)

(* ---- Top-level program checking ---- *)

let check_decl ctx level (decl : Ast.decl) : ctx * tdecl list =
  match decl with
  | Ast.DType (type_params, name, def, deriving) ->
    let ctx' = process_type_def ctx type_params name def in
    let (ctx', derived_tdecls) = List.fold_left (fun (ctx, acc) cls ->
      match generate_derived_instance type_params name def cls with
      | Some (class_name, inst_tys, constraints, methods) ->
        let (ctx', tdecl) = process_instance_def ctx level class_name inst_tys constraints methods in
        (ctx', tdecl :: acc)
      | None ->
        error (Printf.sprintf "cannot derive %s for type %s" cls name)
    ) (ctx', []) deriving in
    (ctx', TDType (name, def) :: List.rev derived_tdecls)
  | Ast.DLet (name, params, ret_annot, constraints, body) ->
    if constraints <> [] then begin
      let (te, scheme) = synth_constrained_fn ctx level constraints params ret_annot body in
      let ctx' = extend_var ctx name scheme in
      (ctx', [TDLet (name, te)])
    end else begin
      let eff = Types.new_effvar level in
      let eff_ctx = { ctx with current_eff = eff } in
      let full_body = wrap_params_decl params ret_annot body in
      let te = synth eff_ctx (level + 1) full_body in
      improve_fundeps_in_expr ctx.vars ctx.type_env te;
      let scheme =
        if params = [] && not (is_syntactic_value body) then mono_scheme te.ty
        else resolve_let_constraints Types.empty_type_env level [] params ret_annot te.ty (Hashtbl.create 0) in
      let scheme = infer_implicit_constraints "" ctx.type_env ctx.vars te scheme in
      let scheme = infer_record_evidence ctx.vars te scheme in
      let ctx' = extend_var ctx name scheme in
      (ctx', [TDLet (name, te)])
    end
  | Ast.DLetMut (name, body) ->
    let eff = Types.new_effvar level in
    let eff_ctx = { ctx with current_eff = eff } in
    let te = synth eff_ctx (level + 1) body in
    let scheme = mono_scheme te.ty in
    let ctx' = extend_var_mutable ctx name scheme in
    (ctx', [TDLetMut (name, te)])
  | Ast.DLetRec (name, type_params, params, ret_annot, constraints, body) ->
    if type_params <> [] then begin
      let (te, scheme) = synth_poly_rec_fn ctx level name "" type_params params ret_annot body in
      let ctx' = extend_var ctx name scheme in
      (ctx', [TDLetRec (name, te)])
    end else if constraints <> [] then begin
      let fn_var = Types.new_tvar (level + 1) in
      let ctx_with_self = extend_var_mono ctx name fn_var in
      let (te, scheme) = synth_constrained_fn ctx_with_self level constraints params ret_annot body in
      try_unify fn_var te.ty;
      let ctx' = extend_var ctx name scheme in
      (ctx', [TDLetRec (name, te)])
    end else begin
      let eff = Types.new_effvar level in
      let eff_ctx = { ctx with current_eff = eff } in
      let full_body = wrap_params_decl params ret_annot body in
      let fn_var = Types.new_tvar (level + 1) in
      let ctx' = extend_var_mono eff_ctx name fn_var in
      let fn_te = synth ctx' (level + 1) full_body in
      try_unify fn_var fn_te.ty;
      improve_fundeps_in_expr ctx.vars ctx.type_env fn_te;
      let scheme = resolve_let_constraints Types.empty_type_env level [] params ret_annot fn_te.ty (Hashtbl.create 0) in
      let scheme = infer_implicit_constraints name ctx.type_env ctx.vars fn_te scheme in
      let scheme = infer_record_evidence ctx.vars fn_te scheme in
      let ctx'' = extend_var ctx name scheme in
      (ctx'', [TDLetRec (name, fn_te)])
    end
  | Ast.DExpr expr ->
    let eff = Types.new_effvar level in
    let ctx' = { ctx with current_eff = eff } in
    let te = synth ctx' level expr in
    (* Enforce no unhandled effects at top level *)
    (try Types.unify_eff (Types.eff_repr eff) Types.EffEmpty
     with Types.Unify_error _ ->
       error (Printf.sprintf "expression has unhandled effects: %s" (Types.pp_eff eff)));
    (ctx, [TDExpr te])
  | Ast.DClass (name, tyvars, fundeps, methods) ->
    let (ctx', td) = process_class_def ctx name tyvars fundeps methods in
    (ctx', [td])
  | Ast.DInstance (class_name, inst_ty_annots, constraints, methods) ->
    let (ctx', td) = process_instance_def ctx level class_name inst_ty_annots constraints methods in
    (ctx', [td])
  | Ast.DEffect (name, type_params, ops) ->
    let tvars = Hashtbl.create 4 in
    List.iteri (fun i p -> Hashtbl.replace tvars p (Types.TGen i)) type_params;
    let resolved_ops = List.map (fun (op_name, annot) ->
      let ty = resolve_ty_annot_shared ctx 0 tvars annot in
      (op_name, ty)
    ) ops in
    let effect_def = Types.{
      effect_name = name;
      effect_params = type_params;
      effect_ops = resolved_ops;
    } in
    let type_env = { ctx.type_env with effects = effect_def :: ctx.type_env.effects } in
    let ctx = { ctx with type_env } in
    (ctx, [TDEffect name])
  | Ast.DExtern (name, ty_annot) ->
    let tvars = Hashtbl.create 4 in
    let ty = resolve_ty_annot_shared ctx 1 tvars ty_annot in
    let scheme = Types.generalize 0 ty in
    let ctx = extend_var ctx name scheme in
    (ctx, [TDExtern (name, scheme)])
  | Ast.DModule (name, items) ->
    let (ctx', td) = process_module_def ctx level name items in
    (ctx', [td])
  | Ast.DOpen (mod_name, names_opt) ->
    let (ctx', td) = process_open ctx mod_name names_opt in
    (ctx', [td])
  | Ast.DLetRecAnd bindings ->
    (* Check if any binding has type_params — if so, use polymorphic scheme for those *)
    let has_poly = List.exists (fun (_, tp, _, _, _, _) -> tp <> []) bindings in
    if has_poly then begin
      (* Build schemes for all bindings, bind all polymorphically, then check all *)
      let shared_tvars_list = List.map (fun (_, type_params, params, ret_annot, _, _) ->
        let shared_tvars = Hashtbl.create 4 in
        List.iter (fun tp ->
          Hashtbl.replace shared_tvars tp (Types.new_tvar (level + 1))
        ) type_params;
        let param_tys = List.map (fun (p : Ast.param) ->
          match p.annot with
          | Some annot -> resolve_ty_annot_shared ctx (level + 1) shared_tvars annot
          | None -> Types.new_tvar (level + 1)
        ) params in
        let (ret_ty, body_eff) = match ret_annot with
          | Some (Ast.TyWithEffect (ty, eff_annot)) ->
            let ty' = resolve_ty_annot_shared ctx (level + 1) shared_tvars ty in
            let eff = match eff_annot with
              | Ast.EffAnnotPure -> Types.EffEmpty
              | Ast.EffAnnotRow items -> resolve_eff_items ctx (level + 1) shared_tvars items
            in (ty', eff)
          | Some annot ->
            (resolve_ty_annot_shared ctx (level + 1) shared_tvars annot,
             Types.new_effvar (level + 1))
          | None ->
            (Types.new_tvar (level + 1), Types.new_effvar (level + 1))
        in
        let fn_ty = match List.rev param_tys with
          | [] -> ret_ty
          | last :: rest ->
            let inner = Types.TArrow (last, body_eff, ret_ty) in
            List.fold_left (fun acc pty ->
              Types.TArrow (pty, Types.EffEmpty, acc)
            ) inner rest
        in
        (shared_tvars, param_tys, ret_ty, body_eff, fn_ty)
      ) bindings in
      (* Build schemes and bind all names *)
      let schemes = List.map (fun (_, _, _, _, fn_ty) ->
        Types.generalize level fn_ty
      ) shared_tvars_list in
      let ctx' = List.fold_left2 (fun ctx (name, _, _, _, _, _) scheme ->
        extend_var ctx name scheme
      ) ctx bindings schemes in
      (* Check all bodies *)
      let body_tes = List.map2 (fun (_, _, params, _, _, body) (_, param_tys, ret_ty, body_eff, _) ->
        let inner_ctx = List.fold_left2 (fun c (p : Ast.param) ty ->
          extend_var_mono c p.name ty
        ) { ctx' with current_eff = body_eff } params param_tys in
        let body_te = check inner_ctx (level + 1) body ret_ty in
        match List.rev params, List.rev param_tys with
        | [], [] -> body_te
        | last_p :: rest_ps, last_pty :: rest_ptys ->
          let inner = mk (TEFun (last_p.Ast.name, body_te, false))
            (Types.TArrow (last_pty, body_eff, body_te.ty)) in
          List.fold_left2 (fun acc (p : Ast.param) pty ->
            mk (TEFun (p.name, acc, false)) (Types.TArrow (pty, Types.EffEmpty, acc.ty))
          ) inner rest_ps rest_ptys
        | _ -> assert false
      ) bindings shared_tvars_list in
      let ctx'' = List.fold_left2 (fun ctx (name, _, _, _, _, _) scheme ->
        extend_var ctx name scheme
      ) ctx bindings schemes in
      let typed_bindings = List.map2 (fun (name, _, _, _, _, _) te -> (name, te)) bindings body_tes in
      (ctx'', [TDLetRecAnd typed_bindings])
    end else begin
      (* Original path: monomorphic recursion *)
      let fn_vars = List.map (fun (name, _, _, _, _, _) ->
        (name, Types.new_tvar (level + 1))
      ) bindings in
      let ctx' = List.fold_left (fun ctx (name, tv) ->
        extend_var_mono ctx name tv
      ) ctx fn_vars in
      let body_tes = List.map2 (fun (name, _, params, ret_annot, constraints, body) (_, tv) ->
        ignore name;
        if constraints <> [] then
          let (te, _scheme) = synth_constrained_fn ctx' level constraints params ret_annot body in
          try_unify tv te.ty;
          te
        else begin
          let full_body = wrap_params_decl params ret_annot body in
          let te = synth ctx' (level + 1) full_body in
          try_unify tv te.ty;
          te
        end
      ) bindings fn_vars in
      let ctx'' = List.fold_left2 (fun ctx (name, _, _, _, _, _) (_, tv) ->
        let scheme = Types.generalize level tv in
        extend_var ctx name scheme
      ) ctx bindings fn_vars in
      let typed_bindings = List.map2 (fun (name, _, _, _, _, _) te -> (name, te)) bindings body_tes in
      (ctx'', [TDLetRecAnd typed_bindings])
    end
  | Ast.DTypeAnd type_defs ->
    (* First pass: register all type names as placeholders *)
    let ctx' = List.fold_left (fun ctx (type_params, name, def, _deriving) ->
      let num_params = List.length type_params in
      match def with
      | Ast.TDVariant _ ->
        let type_env = { ctx.type_env with
          variants = (name, num_params, [], false) :: ctx.type_env.variants;
        } in
        { ctx with type_env }
      | Ast.TDRecord _ ->
        let type_env = { ctx.type_env with
          records = (name, []) :: ctx.type_env.records;
        } in
        { ctx with type_env }
      | Ast.TDAlias _ -> ctx
    ) ctx type_defs in
    (* Second pass: process records first, then variants/aliases *)
    let ctx' = List.fold_left (fun ctx (type_params, name, def, _deriving) ->
      match def with
      | Ast.TDRecord _ -> process_type_def ctx type_params name def
      | _ -> ctx
    ) ctx' type_defs in
    let ctx'' = List.fold_left (fun ctx (type_params, name, def, _deriving) ->
      match def with
      | Ast.TDRecord _ -> ctx
      | _ -> process_type_def ctx type_params name def
    ) ctx' type_defs in
    (* Handle deriving for each type *)
    let (ctx_final, derived_tdecls) = List.fold_left (fun (ctx, acc) (type_params, name, def, deriving) ->
      let (ctx', new_decls) = List.fold_left (fun (ctx, acc) cls ->
        match generate_derived_instance type_params name def cls with
        | Some (class_name, inst_tys, constraints, methods) ->
          let (ctx', tdecl) = process_instance_def ctx level class_name inst_tys constraints methods in
          (ctx', tdecl :: acc)
        | None ->
          error (Printf.sprintf "cannot derive %s for type %s" cls name)
      ) (ctx, []) deriving in
      (ctx', List.rev new_decls @ acc)
    ) (ctx'', []) type_defs in
    let type_decls = List.map (fun (_tp, name, def, _d) -> TDType (name, def)) type_defs in
    (ctx_final, type_decls @ List.rev derived_tdecls)

let empty_ctx = { vars = []; mutable_vars = []; type_env = Types.empty_type_env; loop_info = None; current_module = None; constraint_tvars = []; current_eff = Types.EffEmpty; return_type = None; inside_handler = false; return_used = ref false }

(* ==== Constraint transformation pass ==== *)
(* Rewrites the typed AST to insert dictionary passing for typeclass constraints *)

type xform_ctx = {
  xf_type_env: Types.type_env;
  xf_schemes: (string * Types.scheme) list;
  (* Inside constrained bodies: tvar_id -> (dict_param_name, class_name) *)
  xf_constrained: (int * string * string) list;
  (* Inside record-evidence bodies: rvar_id -> (field_name, evidence_param_name) *)
  xf_record_evidence: (int * string * string) list;
  (* Locally bound names that shadow typeclass methods *)
  xf_locals: string list;
}

(* Walk two types in parallel, extracting TGen index -> actual type mappings *)
let build_tgen_map schema_ty actual_ty =
  let map = Hashtbl.create 4 in
  let rec walk s a =
    let a = Types.repr a in
    match s with
    | Types.TGen i ->
      if not (Hashtbl.mem map i) then Hashtbl.replace map i a
    | Types.TArrow (s1, _, s2) ->
      (match a with Types.TArrow (a1, _, a2) -> walk s1 a1; walk s2 a2 | _ -> ())
    | Types.TCont (s1, _, s2) ->
      (match a with Types.TCont (a1, _, a2) -> walk s1 a1; walk s2 a2 | _ -> ())
    | Types.TTuple ss ->
      (match a with Types.TTuple aa when List.length ss = List.length aa ->
        List.iter2 walk ss aa | _ -> ())
    | Types.TList s1 ->
      (match a with Types.TList a1 -> walk s1 a1 | _ -> ())
    | Types.TArray s1 ->
      (match a with Types.TArray a1 -> walk s1 a1 | _ -> ())
    | Types.TMap (sk, sv) ->
      (match a with Types.TMap (ak, av) -> walk sk ak; walk sv av | _ -> ())
    | Types.TVariant (_, ss) ->
      (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
        List.iter2 walk ss aa | _ -> ())
    | Types.TRecord srow ->
      (match a with Types.TRecord arow ->
        let sf = Types.record_row_to_fields srow in
        let af = Types.record_row_to_fields arow in
        (* Match by field name — schema may have fewer fields than actual due to open row *)
        List.iter (fun (sname, sty) ->
          match List.assoc_opt sname af with
          | Some aty -> walk sty aty
          | None -> ()
        ) sf
      | _ -> ())
    | Types.TPolyVariant srow ->
      (match a with Types.TPolyVariant arow -> walk_pvrows srow arow | _ -> ())
    | _ -> ()
  and walk_pvrows srow arow =
    (* Collect actual row tags into an assoc list *)
    let rec collect_tags row =
      match row with
      | Types.PVRow (tag, ty_opt, tail) -> (tag, ty_opt) :: collect_tags tail
      | Types.PVVar { contents = Types.PVLink r } -> collect_tags r
      | _ -> []
    in
    let atags = collect_tags arow in
    let rec go row =
      match row with
      | Types.PVRow (stag, sty_opt, stail) ->
        (match List.assoc_opt stag atags with
         | Some aty_opt ->
           (match sty_opt, aty_opt with
            | Some sty, Some aty -> walk sty aty
            | _ -> ())
         | None -> ());
        go stail
      | Types.PVVar { contents = Types.PVLink r } -> go r
      | _ -> ()
    in
    go srow
  in
  walk schema_ty actual_ty;
  map

(* Check if a tvar_id is constrained for a given class *)
let find_constrained_dict xctx tvar_id class_name =
  List.find_map (fun (tid, dparam, cls) ->
    if tid = tvar_id && String.equal cls class_name then Some dparam
    else None
  ) xctx.xf_constrained

(* Resolve a dictionary argument for a constraint at a call site *)
let rec resolve_dict_arg xctx (cc : Types.class_constraint) tgen_map =
  let arg_types = List.map (fun (ca : Types.class_arg) ->
    match ca with
    | CATGen idx ->
      (match Hashtbl.find_opt tgen_map idx with
       | Some ty -> Some (Types.repr ty)
       | None -> None)
    | CATy ty -> Some (Types.freshen_unbound ty)
    | CAWild -> Some (Types.new_tvar 1)  (* fresh tvar, resolved by fundep improvement below *)
    | CAPhantom (idx, _) ->
      (* Like CATGen but create fresh tvar if not in tgen_map, and cache for sharing *)
      (match Hashtbl.find_opt tgen_map idx with
       | Some ty -> Some (Types.repr ty)
       | None ->
         let tv = Types.new_tvar 1 in
         Hashtbl.replace tgen_map idx tv;
         Some tv)
  ) cc.cc_args in
  (* If any arg is unresolved, use runtime fallback for known classes *)
  let has_unresolved = List.exists (fun opt -> opt = None) arg_types in
  if has_unresolved then begin
    if String.equal cc.cc_class "Show" then
      mk (TERecord [("show", mk (TEVar "__show_value") Types.TUnit)]) Types.TUnit
    else
      error (Printf.sprintf "could not resolve constraint type argument for %s" cc.cc_class)
  end else
  let arg_types = List.filter_map Fun.id arg_types in
  (* Apply fundep improvement: if some args are concrete and class has fundeps,
     try to resolve remaining unbound tvar args *)
  let arg_types =
    let has_unbound = List.exists (fun ty ->
      match ty with Types.TVar { contents = Types.Unbound _ } -> true | _ -> false
    ) arg_types in
    if has_unbound then
      let class_opt = List.find_opt (fun (cls : Types.class_def) ->
        String.equal cls.class_name cc.cc_class && cls.class_fundeps <> []
      ) xctx.xf_type_env.Types.classes in
      match class_opt with
      | Some class_def ->
        let partial = List.map (fun ty ->
          match Types.repr ty with
          | Types.TVar { contents = Types.Unbound _ } -> None
          | t -> Some t
        ) arg_types in
        let improved = Types.improve_with_fundeps
          xctx.xf_type_env.Types.instances class_def partial in
        (* Unify newly resolved args *)
        List.iter2 (fun orig_ty imp ->
          match imp, Types.repr orig_ty with
          | Some resolved, Types.TVar ({ contents = Types.Unbound _ } as r) ->
            (try Types.unify (Types.TVar r) resolved with _ -> ())
          | _ -> ()
        ) arg_types improved;
        List.map Types.repr arg_types
      | None -> arg_types
    else arg_types
  in
  (* Check if any arg type is a constrained tvar *)
  let constrained_match = List.find_map (fun ty ->
    match ty with
    | Types.TVar { contents = Types.Unbound (id, _) } ->
      find_constrained_dict xctx id cc.cc_class
    | _ -> None
  ) arg_types in
  match constrained_match with
  | Some dparam -> mk (TEVar dparam) Types.TUnit
  | None ->
    (* Check for unbound tvars without constrained match — use runtime fallback *)
    let has_unbound_tvar = List.exists (fun ty ->
      match ty with Types.TVar { contents = Types.Unbound _ } -> true | _ -> false
    ) arg_types in
    if has_unbound_tvar then begin
      if String.equal cc.cc_class "Show" then
        mk (TERecord [("show", mk (TEVar "__show_value") Types.TUnit)]) Types.TUnit
      else
        error (Printf.sprintf "no instance of %s for types %s"
          cc.cc_class (String.concat ", " (List.map Types.pp_ty arg_types)))
    end else begin
    (* All concrete — find instance *)
    let partial = List.map (fun ty -> Some ty) arg_types in
    let matching = List.filter (fun (inst : Types.instance_def) ->
      String.equal inst.inst_class cc.cc_class &&
      List.length inst.inst_tys = List.length arg_types &&
      Types.match_partial_inst inst.inst_tys partial
    ) xctx.xf_type_env.Types.instances in
    (match matching with
     | [inst] ->
       if inst.inst_constraints <> [] then
         resolve_factory_dict xctx inst arg_types
       else
         mk (TEVar inst.inst_dict_name) Types.TUnit
     | [] ->
       let is_structural = List.exists (fun ty ->
         match Types.repr ty with
         | Types.TPolyVariant _ | Types.TRecord _ -> true
         | _ -> false
       ) arg_types in
       if is_structural && String.equal cc.cc_class "Show" then
         mk (TERecord [("show", mk (TEVar "__show_value") Types.TUnit)]) Types.TUnit
       else
         error (Printf.sprintf "no instance of %s for types %s"
           cc.cc_class (String.concat ", " (List.map Types.pp_ty arg_types)))
     | _ ->
       (* Try specificity-based selection *)
       (match Types.most_specific_inst matching with
        | Some inst ->
          if inst.inst_constraints <> [] then
            resolve_factory_dict xctx inst arg_types
          else
            mk (TEVar inst.inst_dict_name) Types.TUnit
        | None ->
          error (Printf.sprintf "ambiguous instance for %s"  cc.cc_class)))
    end

(* Resolve a factory dict: apply the factory with sub-dicts *)
and resolve_factory_dict xctx inst arg_types =
  (* Build TGen mapping from inst.inst_tys to arg_types *)
  let sub_map = Hashtbl.create 4 in
  List.iter2 (fun inst_ty actual_ty ->
    let rec walk s a =
      let a = Types.repr a in
      match s with
      | Types.TGen i ->
        if not (Hashtbl.mem sub_map i) then Hashtbl.replace sub_map i a
      | Types.TArrow (s1, _, s2) ->
        (match a with Types.TArrow (a1, _, a2) -> walk s1 a1; walk s2 a2 | _ -> ())
      | Types.TCont (s1, _, s2) ->
        (match a with Types.TCont (a1, _, a2) -> walk s1 a1; walk s2 a2 | _ -> ())
      | Types.TTuple ss ->
        (match a with Types.TTuple aa when List.length ss = List.length aa ->
          List.iter2 walk ss aa | _ -> ())
      | Types.TList s1 ->
        (match a with Types.TList a1 -> walk s1 a1 | _ -> ())
      | Types.TArray s1 ->
        (match a with Types.TArray a1 -> walk s1 a1 | _ -> ())
      | Types.TMap (sk, sv) ->
        (match a with Types.TMap (ak, av) -> walk sk ak; walk sv av | _ -> ())
      | Types.TVariant (_, ss) ->
        (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
          List.iter2 walk ss aa | _ -> ())
      | _ -> ()
    in
    walk inst_ty actual_ty
  ) inst.inst_tys arg_types;
  (* Apply factory with sub-dicts *)
  let base = mk (TEVar inst.inst_dict_name) Types.TUnit in
  List.fold_left (fun fn sub_cc ->
    let sub_dict = resolve_dict_arg xctx sub_cc sub_map in
    mk (TEApp (fn, sub_dict)) Types.TUnit
  ) base inst.inst_constraints

let rec pat_bound_names = function
  | Ast.PatVar name -> [name]
  | Ast.PatTuple pats | Ast.PatArray pats -> List.concat_map pat_bound_names pats
  | Ast.PatCons (p1, p2) -> pat_bound_names p1 @ pat_bound_names p2
  | Ast.PatConstruct (_, Some p) | Ast.PatPolyVariant (_, Some p) -> pat_bound_names p
  | Ast.PatRecord fields -> List.concat_map (fun (_, p) -> pat_bound_names p) fields
  | Ast.PatAs (p, name) -> name :: pat_bound_names p
  | Ast.PatOr (p1, _) -> pat_bound_names p1
  | Ast.PatMap pairs -> List.concat_map (fun (_, v) -> pat_bound_names v) pairs
  | Ast.PatAnnot (p, _) -> pat_bound_names p
  | Ast.PatWild | Ast.PatInt _ | Ast.PatFloat _ | Ast.PatBool _
  | Ast.PatString _ | Ast.PatUnit | Ast.PatNil | Ast.PatPin _
  | Ast.PatConstruct (_, None) | Ast.PatPolyVariant (_, None) -> []

(* Main expression rewriter *)
let rec xform_expr xctx te =
  match te.expr with
  | TEVar name ->
    let te = xform_class_method xctx name te in
    xform_constrained_ref xctx name te
  | TEBinop (op, e1, e2) ->
    let e1' = xform_expr xctx e1 in
    let e2' = xform_expr xctx e2 in
    xform_binop xctx op e1' e2' te.ty
  | TEUnop (op, e) ->
    let e' = xform_expr xctx e in
    xform_unop xctx op e' te.ty
  | TEApp (fn, arg) ->
    let fn' = xform_expr xctx fn in
    let arg' = xform_expr xctx arg in
    mk (TEApp (fn', arg')) te.ty
  | TEFun (param, body, hr) ->
    let xctx' = { xctx with xf_locals = param :: xctx.xf_locals } in
    mk (TEFun (param, xform_expr xctx' body, hr)) te.ty
  | TELet (name, Some scheme, e1, e2) ->
    let e1' = xform_constrained_def xctx scheme e1 in
    let xctx' = { xctx with xf_schemes = (name, scheme) :: xctx.xf_schemes } in
    mk (TELet (name, Some scheme, e1', xform_expr xctx' e2)) te.ty
  | TELet (name, None, e1, e2) ->
    let xctx' = { xctx with xf_locals = name :: xctx.xf_locals } in
    mk (TELet (name, None, xform_expr xctx e1, xform_expr xctx' e2)) te.ty
  | TELetRec (name, Some scheme, e1, e2) ->
    (* Add scheme before processing e1 so recursive calls can resolve dicts *)
    let xctx' = { xctx with xf_schemes = (name, scheme) :: xctx.xf_schemes } in
    let e1' = xform_constrained_def xctx' scheme e1 in
    mk (TELetRec (name, Some scheme, e1', xform_expr xctx' e2)) te.ty
  | TELetRec (name, None, e1, e2) ->
    let xctx' = { xctx with xf_locals = name :: xctx.xf_locals } in
    mk (TELetRec (name, None, xform_expr xctx' e1, xform_expr xctx' e2)) te.ty
  | TELetMut (name, e1, e2) ->
    let xctx' = { xctx with xf_locals = name :: xctx.xf_locals } in
    mk (TELetMut (name, xform_expr xctx e1, xform_expr xctx' e2)) te.ty
  | TEWhile (cond, body) ->
    mk (TEWhile (xform_expr xctx cond, xform_expr xctx body)) te.ty
  | TELetRecAnd (bindings, body) ->
    let names = List.map fst bindings in
    let xctx' = { xctx with xf_locals = names @ xctx.xf_locals } in
    let bindings' = List.map (fun (n, e) -> (n, xform_expr xctx' e)) bindings in
    mk (TELetRecAnd (bindings', xform_expr xctx' body)) te.ty
  | TEIf (c, t, e) ->
    mk (TEIf (xform_expr xctx c, xform_expr xctx t, xform_expr xctx e)) te.ty
  | TETuple es ->
    mk (TETuple (List.map (xform_expr xctx) es)) te.ty
  | TERecord fields ->
    mk (TERecord (List.map (fun (n, e) -> (n, xform_expr xctx e)) fields)) te.ty
  | TERecordUpdate (base, overrides) ->
    let base' = xform_expr xctx base in
    let overrides' = List.map (fun (n, e) -> (n, xform_expr xctx e)) overrides in
    (* Check if we have evidence for this update's row variable *)
    let rec get_row_tail row = match Types.rrow_repr row with
      | Types.RRow (_, _, tail) -> get_row_tail tail
      | tail -> tail
    in
    let has_evidence = match Types.repr base'.ty with
      | Types.TRecord row ->
        (match get_row_tail row with
         | Types.RVar { contents = Types.RUnbound (id, _) } ->
           List.exists (fun (rid, _, _) -> rid = id) xctx.xf_record_evidence
         | _ -> false)
      | _ -> false
    in
    if has_evidence then begin
      let row = match Types.repr base'.ty with Types.TRecord r -> r | _ -> assert false in
      let rvar_id = match get_row_tail row with
        | Types.RVar { contents = Types.RUnbound (id, _) } -> id | _ -> assert false in
      let idx_val_pairs = List.map (fun (name, e) ->
        let ev_param = List.find_map (fun (rid, fname, pname) ->
          if rid = rvar_id && String.equal fname name then Some pname else None
        ) xctx.xf_record_evidence in
        match ev_param with
        | Some pname -> (mk (TEVar pname) Types.TInt, e)
        | None -> error (Printf.sprintf "missing record evidence for field %s" name)
      ) overrides' in
      mk (TERecordUpdateIdx (base', idx_val_pairs)) te.ty
    end else
      mk (TERecordUpdate (base', overrides')) te.ty
  | TERecordUpdateIdx (base, pairs) ->
    mk (TERecordUpdateIdx (xform_expr xctx base, List.map (fun (i, v) -> (xform_expr xctx i, xform_expr xctx v)) pairs)) te.ty
  | TEField (e, name) ->
    mk (TEField (xform_expr xctx e, name)) te.ty
  | TECons (hd, tl) ->
    mk (TECons (xform_expr xctx hd, xform_expr xctx tl)) te.ty
  | TEConstruct (name, arg) ->
    mk (TEConstruct (name, Option.map (xform_expr xctx) arg)) te.ty
  | TEMatch (scrutinee, arms, partial) ->
    let arms' = List.map (fun (pat, guard, body) ->
      let pvars = pat_bound_names pat in
      let xctx' = { xctx with xf_locals = pvars @ xctx.xf_locals } in
      (pat, Option.map (xform_expr xctx') guard, xform_expr xctx' body)
    ) arms in
    mk (TEMatch (xform_expr xctx scrutinee, arms', partial)) te.ty
  | TEAssign (name, e) ->
    mk (TEAssign (name, xform_expr xctx e)) te.ty
  | TEFieldAssign (r, f, e) ->
    mk (TEFieldAssign (xform_expr xctx r, f, xform_expr xctx e)) te.ty
  | TESeq (e1, e2) ->
    mk (TESeq (xform_expr xctx e1, xform_expr xctx e2)) te.ty
  | TEPerform (name, e) ->
    mk (TEPerform (name, xform_expr xctx e)) te.ty
  | TEHandle (body, arms) ->
    let arms' = List.map (fun arm -> match arm with
      | THReturn (n, e) ->
        let xctx' = { xctx with xf_locals = n :: xctx.xf_locals } in
        THReturn (n, xform_expr xctx' e)
      | THOp (op, p, k, e) ->
        let xctx' = { xctx with xf_locals = k :: p :: xctx.xf_locals } in
        THOp (op, p, k, xform_expr xctx' e)
    ) arms in
    mk (TEHandle (xform_expr xctx body, arms')) te.ty
  | TEResume (k, e) ->
    mk (TEResume (xform_expr xctx k, xform_expr xctx e)) te.ty
  | TEMap pairs ->
    mk (TEMap (List.map (fun (k, v) -> (xform_expr xctx k, xform_expr xctx v)) pairs)) te.ty
  | TEArray es ->
    mk (TEArray (List.map (xform_expr xctx) es)) te.ty
  | TEIndex (base, idx) ->
    mk (TEIndex (xform_expr xctx base, xform_expr xctx idx)) te.ty
  | TEBreak e -> mk (TEBreak (xform_expr xctx e)) te.ty
  | TEContinueLoop -> te
  | TEFoldContinue e -> mk (TEFoldContinue (xform_expr xctx e)) te.ty
  | TEForLoop e -> mk (TEForLoop (xform_expr xctx e)) te.ty
  | TEReturn e -> mk (TEReturn (xform_expr xctx e)) te.ty
  | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEByte _ | TERune _ | TEUnit | TENil -> te

(* Replace class method TEVar with dict field access when type arg is a constrained tvar *)
and xform_class_method xctx name te =
  (* If name is locally bound, don't resolve as class method *)
  if List.mem name xctx.xf_locals then te else
  (* Try the full name first; only strip module prefix if the method's class
     has a matching module prefix (prevents Set.of_list matching Map.of_list) *)
  let (class_opt, method_name) =
    match Types.find_method_class xctx.xf_type_env.Types.classes name with
    | Some _ as result -> (result, name)
    | None ->
      match String.rindex_opt name '.' with
      | None -> (None, name)
      | Some i ->
        let mod_prefix = String.sub name 0 (i + 1) in
        let short = String.sub name (i + 1) (String.length name - i - 1) in
        let class_prefix = String.sub name 0 i in
        (match Types.find_method_class xctx.xf_type_env.Types.classes short with
         | Some class_def when String.equal class_def.Types.class_name class_prefix ->
           (Some class_def, short)
         | Some class_def when String.length class_def.Types.class_name > String.length mod_prefix
             && String.sub class_def.Types.class_name 0 (String.length mod_prefix) = mod_prefix ->
           (Some class_def, short)
         | _ -> (None, name))
  in
  match class_opt with
  | Some class_def ->
    let method_schema_ty = List.assoc method_name class_def.Types.class_methods in
    (* Check if the current binding is a user override rather than the original class method.
       The original class method's scheme body equals method_schema_ty exactly;
       a user-defined override will have a different body. *)
    let is_overridden = match List.assoc_opt name xctx.xf_schemes with
      | Some s -> s.Types.body <> method_schema_ty
      | None -> false
    in
    if is_overridden then te else
    let num_params = List.length class_def.Types.class_params in
    let found = Hashtbl.create num_params in
    let rec go s r =
      let r = Types.repr r in
      match s, r with
      | Types.TGen i, _ when i < num_params ->
        if not (Hashtbl.mem found i) then Hashtbl.replace found i r
      | Types.TArrow (s1, _, s2), Types.TArrow (r1, _, r2)
      | Types.TCont (s1, _, s2), Types.TCont (r1, _, r2) -> go s1 r1; go s2 r2
      | Types.TTuple ss, Types.TTuple rs when List.length ss = List.length rs ->
        List.iter2 go ss rs
      | Types.TList s1, Types.TList r1 -> go s1 r1
      | Types.TArray s1, Types.TArray r1 -> go s1 r1
      | Types.TMap (sk, sv), Types.TMap (rk, rv) -> go sk rk; go sv rv
      | Types.TVariant (_, ss), Types.TVariant (_, rs) when List.length ss = List.length rs ->
        List.iter2 go ss rs
      | _ -> ()
    in
    go method_schema_ty te.ty;
    (* Phase 1: If inside a constrained body, check if type arg is a constrained tvar *)
    let constrained_match = if xctx.xf_constrained <> [] then
      let exact_match = List.find_map (fun i ->
        match Hashtbl.find_opt found i with
        | Some (Types.TVar { contents = Types.Unbound (id, _) }) ->
          find_constrained_dict xctx id class_def.Types.class_name
        | _ -> None
      ) (List.init num_params Fun.id) in
      (* Fallback: if exact ID didn't match but the type arg is an unbound TVar
         (e.g. generalized inside let rec), find a constraint for this class.
         Only if there's exactly one matching constraint to avoid ambiguity. *)
      (match exact_match with
       | Some _ -> exact_match
       | None ->
         let has_unbound_arg = List.exists (fun i ->
           match Hashtbl.find_opt found i with
           | Some (Types.TVar { contents = Types.Unbound _ }) -> true
           | _ -> false
         ) (List.init num_params Fun.id) in
         if has_unbound_arg then
           let matching_constraints = List.filter (fun (_tid, _dp, cls) ->
             String.equal cls class_def.Types.class_name
           ) xctx.xf_constrained in
           match matching_constraints with
           | [(_tid, dp, _cls)] -> Some dp
           | _ -> None
         else None)
    else None in
    (match constrained_match with
     | Some dp ->
       mk (TEField (mk (TEVar dp) Types.TUnit, method_name)) te.ty
     | None ->
       (* Phase 2: Check if concrete type matches a factory instance *)
       let type_args = List.init num_params (fun i -> Hashtbl.find_opt found i) in
       (* Build partial args: unbound type vars become None wildcards *)
       let partial_args = List.map (fun opt ->
         match opt with
         | Some ty -> (match Types.repr ty with
           | Types.TVar { contents = Types.Unbound _ } -> None
           | _ -> Some ty)
         | None -> None
       ) type_args in
       let has_info = List.exists (fun opt -> opt <> None) partial_args in
       if has_info then begin
         let matching = List.filter (fun (inst : Types.instance_def) ->
           String.equal inst.inst_class class_def.Types.class_name &&
           List.length inst.inst_tys = List.length partial_args &&
           Types.match_partial_inst inst.inst_tys partial_args
         ) xctx.xf_type_env.Types.instances in
         let all_concrete = List.for_all (fun opt -> opt <> None) partial_args in
         let resolved = match matching with
           | [inst] -> Some inst
           | _ :: _ -> Types.most_specific_inst matching
           | [] -> None
         in
         match resolved with
         | Some inst when inst.inst_constraints <> [] && all_concrete ->
           let arg_types = List.filter_map Fun.id type_args in
           let factory = resolve_factory_dict xctx inst arg_types in
           mk (TEField (factory, method_name)) te.ty
         | Some inst when inst.inst_constraints = [] ->
           mk (TEField (mk (TEVar inst.inst_dict_name) Types.TUnit, method_name)) te.ty
         | _ -> te
       end else te)
  | None -> te

(* Insert dict args and evidence args when referencing a constrained function *)
and xform_constrained_ref xctx name te =
  match List.assoc_opt name xctx.xf_schemes with
  | Some scheme when scheme_needs_xform scheme ->
    let result = ref te in
    (* Pass dict args for typeclass constraints *)
    if scheme.Types.constraints <> [] then begin
      let tgen_map = build_tgen_map scheme.Types.body te.ty in
      let dict_args = List.map (fun cc ->
        resolve_dict_arg xctx cc tgen_map
      ) scheme.Types.constraints in
      result := List.fold_left (fun fn dict_arg ->
        mk (TEApp (fn, dict_arg)) Types.TUnit
      ) !result dict_args
    end;
    (* Pass evidence args for record field offsets *)
    if scheme.Types.record_evidences <> [] then begin
      let rgen_map = build_rgen_map scheme.Types.body te.ty in
      let ev_args = List.concat_map (fun (re : Types.record_evidence) ->
        resolve_record_evidence_args xctx rgen_map re
      ) scheme.Types.record_evidences in
      result := List.fold_left (fun fn ev_arg ->
        mk (TEApp (fn, ev_arg)) Types.TUnit
      ) !result ev_args
    end;
    !result
  | _ -> te

(* Resolve record evidence arguments at a call site *)
and resolve_record_evidence_args xctx rgen_map (re : Types.record_evidence) =
  match Hashtbl.find_opt rgen_map re.re_rgen with
  | Some arow ->
    let fields = Types.record_row_to_fields arow in
    let sorted_names = List.map fst (List.sort (fun (a, _) (b, _) -> String.compare a b) fields) in
    let rec is_closed r = match Types.rrow_repr r with
      | Types.RRow (_, _, tail) -> is_closed tail
      | Types.REmpty -> true
      | _ -> false
    in
    if is_closed arow then
      (* Concrete: compute index for each field *)
      List.map (fun field_name ->
        let idx = ref (-1) in
        List.iteri (fun i n -> if String.equal n field_name then idx := i) sorted_names;
        if !idx >= 0 then mk (TEInt !idx) Types.TInt
        else error (Printf.sprintf "field %s not found in concrete record for evidence" field_name)
      ) re.re_fields
    else begin
      (* Still polymorphic: forward evidence from context *)
      let rec get_tail r = match Types.rrow_repr r with
        | Types.RRow (_, _, tail) -> get_tail tail
        | tail -> tail
      in
      match get_tail arow with
      | Types.RVar { contents = Types.RUnbound (id, _) } ->
        List.map (fun field_name ->
          match List.find_map (fun (rid, fname, pname) ->
            if rid = id && String.equal fname field_name then Some pname else None
          ) xctx.xf_record_evidence with
          | Some pname -> mk (TEVar pname) Types.TInt
          | None -> error (Printf.sprintf "no evidence for field %s in polymorphic context" field_name)
        ) re.re_fields
      | _ -> error "expected open row for evidence forwarding"
    end
  | None ->
    error (Printf.sprintf "internal error: no record evidence for fields [%s]"
      (String.concat ", " re.re_fields))

(* Rewrite binops where operand is a constrained tvar *)
and xform_binop xctx op e1 e2 result_ty =
  let class_info = match op with
    | Ast.Add -> Some ("Num", "+") | Ast.Sub -> Some ("Num", "-")
    | Ast.Mul -> Some ("Num", "*") | Ast.Div -> Some ("Num", "/")
    | Ast.Eq -> Some ("Eq", "=") | Ast.Neq -> Some ("Eq", "<>")
    | Ast.Lt -> Some ("Ord", "<") | Ast.Gt -> Some ("Ord", ">")
    | Ast.Le -> Some ("Ord", "<=") | Ast.Ge -> Some ("Ord", ">=")
    | Ast.Land -> Some ("Bitwise", "land") | Ast.Lor -> Some ("Bitwise", "lor")
    | Ast.Lxor -> Some ("Bitwise", "lxor") | Ast.Lsl -> Some ("Bitwise", "lsl")
    | Ast.Lsr -> Some ("Bitwise", "lsr")
    | _ -> None
  in
  match class_info with
  | Some (class_name, method_name) when xctx.xf_constrained <> [] ->
    (match Types.repr e1.ty with
     | Types.TVar { contents = Types.Unbound (id, _) } ->
       (match find_constrained_dict xctx id class_name with
        | Some dparam ->
          let dict_ref = mk (TEVar dparam) Types.TUnit in
          let method_ref = mk (TEField (dict_ref, method_name)) Types.TUnit in
          let app1 = mk (TEApp (method_ref, e1)) Types.TUnit in
          mk (TEApp (app1, e2)) result_ty
        | None -> mk (TEBinop (op, e1, e2)) result_ty)
     | _ -> mk (TEBinop (op, e1, e2)) result_ty)
  | _ -> mk (TEBinop (op, e1, e2)) result_ty

(* Rewrite unops where operand is a constrained tvar *)
and xform_unop xctx op e result_ty =
  let class_info = match op with
    | Ast.Neg -> Some ("Num", "negate")
    | _ -> None
  in
  match class_info with
  | Some (class_name, method_name) when xctx.xf_constrained <> [] ->
    (match Types.repr e.ty with
     | Types.TVar { contents = Types.Unbound (id, _) } ->
       (match find_constrained_dict xctx id class_name with
        | Some dparam ->
          let dict_ref = mk (TEVar dparam) Types.TUnit in
          let method_ref = mk (TEField (dict_ref, method_name)) Types.TUnit in
          mk (TEApp (method_ref, e)) result_ty
        | None -> mk (TEUnop (op, e)) result_ty)
     | _ -> mk (TEUnop (op, e)) result_ty)
  | _ -> mk (TEUnop (op, e)) result_ty

(* Transform a constrained function definition *)
and xform_constrained_def xctx scheme te =
  let tgen_map = build_tgen_map scheme.Types.body te.ty in
  (* Build one dict param per constraint (not per cc_arg) *)
  let (all_constrained, dict_param_names) =
    List.fold_left (fun (entries, params) (cc : Types.class_constraint) ->
      (* Collect all tvar_ids for this constraint's cc_args *)
      let tvar_entries = List.filter_map (fun (ca : Types.class_arg) ->
        match ca with
        | CATGen tgen_idx ->
          (match Hashtbl.find_opt tgen_map tgen_idx with
           | Some (Types.TVar { contents = Types.Unbound (id, _) }) ->
             Some (id, tgen_idx)
           | _ -> None)
        | CATy ty ->
          (* Also create dict params for unbound CATy tvars — these are
             internal type variables determined by fundeps at the call site *)
          (match Types.repr ty with
           | Types.TVar { contents = Types.Unbound (id, _) } ->
             Some (id, 1000 + id)
           | _ -> None)
        | CAWild -> None  (* fundep-determined, no tvar to track *)
        | CAPhantom (_, tv) ->
          (* Fundep-determined from annotation. After tree-walk unification,
             Types.repr follows the link to the body's actual tvar. *)
          (match Types.repr tv with
           | Types.TVar { contents = Types.Unbound (id, _) } ->
             Some (id, 1000 + id)
           | _ -> None)
      ) cc.cc_args in
      match tvar_entries with
      | [] ->
        (* All args concrete (resolved by fundep improvement). Create an unused
           dict param to maintain positional alignment with call-site dict args. *)
        let dparam = Printf.sprintf "__dict_%s_resolved_%d" cc.cc_class (List.length params) in
        (entries, dparam :: params)
      | _ ->
        let dparam = Printf.sprintf "__dict_%s_%s" cc.cc_class
          (String.concat "_" (List.map (fun (_, idx) -> string_of_int idx) tvar_entries)) in
        (* All tvars for this constraint map to the same dict param *)
        let new_entries = List.map (fun (id, _) -> (id, dparam, cc.cc_class)) tvar_entries in
        (new_entries @ entries, dparam :: params)
    ) ([], []) scheme.Types.constraints in
  let dict_param_names = List.rev dict_param_names in
  (* Build record evidence params: one param per (field, rgen) *)
  let rgen_map = build_rgen_map scheme.Types.body te.ty in
  let (all_record_ev, ev_param_names) =
    List.fold_left (fun (entries, params) (re : Types.record_evidence) ->
      List.fold_left (fun (entries, params) field_name ->
        let pname = Printf.sprintf "__ev_%s_r%d" field_name re.re_rgen in
        (* Find the rvar_id for this RGen in the actual type *)
        let rvar_entries = match Hashtbl.find_opt rgen_map re.re_rgen with
          | Some arow ->
            let rec get_tail r = match Types.rrow_repr r with
              | Types.RRow (_, _, tail) -> get_tail tail
              | tail -> tail
            in
            (match get_tail arow with
             | Types.RVar { contents = Types.RUnbound (id, _) } -> [(id, field_name, pname)]
             | _ -> [])
          | None -> []
        in
        (rvar_entries @ entries, pname :: params)
      ) (entries, params) re.re_fields
    ) ([], []) scheme.Types.record_evidences in
  let ev_param_names = List.rev ev_param_names in
  let xctx = { xctx with
    xf_constrained = all_constrained @ xctx.xf_constrained;
    xf_record_evidence = all_record_ev @ xctx.xf_record_evidence;
  } in
  let rewritten = xform_expr xctx te in
  (* Wrap with TEFun params: dict params first, then evidence params *)
  let all_params = dict_param_names @ ev_param_names in
  List.fold_right (fun dparam body ->
    mk (TEFun (dparam, body, false)) (Types.TArrow (Types.TUnit, Types.EffEmpty, body.ty))
  ) all_params rewritten

(* Transform a constrained instance definition *)
let xform_constrained_inst xctx inst_def dict_expr =
  let class_def = match Types.find_method_class
    xctx.xf_type_env.Types.classes
    (fst (List.hd (match dict_expr.expr with TERecord fs -> fs | _ -> assert false)))
  with
    | Some cd when String.equal cd.Types.class_name inst_def.Types.inst_class -> cd
    | _ ->
      (* Find class def directly *)
      match List.find_opt (fun c ->
        String.equal c.Types.class_name inst_def.Types.inst_class
      ) xctx.xf_type_env.Types.classes with
      | Some cd -> cd
      | None -> error (Printf.sprintf "class %s not found" inst_def.inst_class)
  in
  (* Extract instance TGen -> tvar_id mapping from method types *)
  let class_param_map = Hashtbl.create 4 in
  let inst_tvar_map = Hashtbl.create 4 in
  (match dict_expr.expr with
   | TERecord fields ->
     List.iter (fun (mname, mte) ->
       match List.assoc_opt mname class_def.Types.class_methods with
       | Some class_method_ty ->
         let rec walk_class s a =
           let a = Types.repr a in
           match s with
           | Types.TGen i -> if not (Hashtbl.mem class_param_map i) then
               Hashtbl.replace class_param_map i a
           | Types.TArrow (s1, _, s2) ->
             (match a with Types.TArrow (a1, _, a2) ->
               walk_class s1 a1; walk_class s2 a2 | _ -> ())
           | Types.TCont (s1, _, s2) ->
             (match a with Types.TCont (a1, _, a2) ->
               walk_class s1 a1; walk_class s2 a2 | _ -> ())
           | Types.TTuple ss ->
             (match a with Types.TTuple aa when List.length ss = List.length aa ->
               List.iter2 walk_class ss aa | _ -> ())
           | Types.TList s1 ->
             (match a with Types.TList a1 -> walk_class s1 a1 | _ -> ())
           | Types.TArray s1 ->
             (match a with Types.TArray a1 -> walk_class s1 a1 | _ -> ())
           | Types.TMap (sk, sv) ->
             (match a with Types.TMap (ak, av) ->
               walk_class sk ak; walk_class sv av | _ -> ())
           | Types.TVariant (_, ss) ->
             (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
               List.iter2 walk_class ss aa | _ -> ())
           | _ -> ()
         in
         walk_class class_method_ty mte.ty
       | None -> ()
     ) fields;
     List.iteri (fun class_idx stored_ty ->
       match Hashtbl.find_opt class_param_map class_idx with
       | Some actual_ty ->
         let rec walk_inst s a =
           let a = Types.repr a in
           match s with
           | Types.TGen i -> if not (Hashtbl.mem inst_tvar_map i) then
               (match a with
                | Types.TVar { contents = Types.Unbound (id, _) } ->
                  Hashtbl.replace inst_tvar_map i id
                | _ -> ())
           | Types.TArrow (s1, _, s2) ->
             (match a with Types.TArrow (a1, _, a2) ->
               walk_inst s1 a1; walk_inst s2 a2 | _ -> ())
           | Types.TCont (s1, _, s2) ->
             (match a with Types.TCont (a1, _, a2) ->
               walk_inst s1 a1; walk_inst s2 a2 | _ -> ())
           | Types.TTuple ss ->
             (match a with Types.TTuple aa when List.length ss = List.length aa ->
               List.iter2 walk_inst ss aa | _ -> ())
           | Types.TList s1 ->
             (match a with Types.TList a1 -> walk_inst s1 a1 | _ -> ())
           | Types.TArray s1 ->
             (match a with Types.TArray a1 -> walk_inst s1 a1 | _ -> ())
           | Types.TMap (sk, sv) ->
             (match a with Types.TMap (ak, av) ->
               walk_inst sk ak; walk_inst sv av | _ -> ())
           | Types.TVariant (_, ss) ->
             (match a with Types.TVariant (_, aa) when List.length ss = List.length aa ->
               List.iter2 walk_inst ss aa | _ -> ())
           | _ -> ()
         in
         walk_inst stored_ty actual_ty
       | None -> ()
     ) inst_def.inst_tys
   | _ -> ());
  (* Build constrained_tvars for instance: one dict per constraint *)
  let (all_constrained, dict_param_names) =
    List.fold_left (fun (entries, params) (cc : Types.class_constraint) ->
      let tvar_entries = List.filter_map (fun (ca : Types.class_arg) ->
        match ca with
        | CATGen tgen_idx ->
          (match Hashtbl.find_opt inst_tvar_map tgen_idx with
           | Some tvar_id -> Some (tvar_id, tgen_idx)
           | None -> None)
        | CATy _ -> None
        | CAWild -> None
        | CAPhantom _ -> None
      ) cc.cc_args in
      match tvar_entries with
      | [] -> (entries, params)
      | _ ->
        let dparam = Printf.sprintf "__dict_%s_%s" cc.cc_class
          (String.concat "_" (List.map (fun (_, idx) -> string_of_int idx) tvar_entries)) in
        let new_entries = List.map (fun (id, _) -> (id, dparam, cc.cc_class)) tvar_entries in
        (new_entries @ entries, dparam :: params)
    ) ([], []) inst_def.inst_constraints in
  let dict_param_names = List.rev dict_param_names in
  let xctx = { xctx with xf_constrained = all_constrained @ xctx.xf_constrained } in
  let rewritten = xform_expr xctx dict_expr in
  List.fold_right (fun dparam body ->
    mk (TEFun (dparam, body, false)) (Types.TArrow (Types.TUnit, Types.EffEmpty, body.ty))
  ) dict_param_names rewritten

(* Post-typechecking fundep improvement: delegates to improve_fundeps_in_expr
   which is also used during type inference before let-generalization. *)
let apply_fundep_improvement type_env vars =
  fun tprog ->
    List.iter (fun tdecl -> match tdecl with
      | TDLet (_, te) | TDLetRec (_, te) | TDExpr te | TDLetMut (_, te) ->
        improve_fundeps_in_expr vars type_env te
      | TDLetRecAnd bindings ->
        List.iter (fun (_, te) -> improve_fundeps_in_expr vars type_env te) bindings
      | TDModule (_, decls) ->
        List.iter (fun d -> match d with
          | TDLet (_, te) | TDLetRec (_, te) | TDExpr te | TDLetMut (_, te) ->
            improve_fundeps_in_expr vars type_env te
          | TDLetRecAnd bindings ->
            List.iter (fun (_, te) -> improve_fundeps_in_expr vars type_env te) bindings
          | _ -> ()
        ) decls
      | _ -> ()
    ) tprog

(* Transform the entire typed program *)
let transform_constraints ctx (tprog : tprogram) : tprogram =
  (* Apply fundep-based type improvement before constraint transformation *)
  apply_fundep_improvement ctx.type_env ctx.vars tprog;
  let xctx = {
    xf_type_env = ctx.type_env;
    xf_schemes = ctx.vars;
    xf_constrained = [];
    xf_record_evidence = [];
    xf_locals = [];
  } in
  let xform_decl xctx tdecl =
    match tdecl with
    | TDLet (name, te) ->
      (match List.assoc_opt name xctx.xf_schemes with
       | Some scheme when scheme_needs_xform scheme ->
         TDLet (name, xform_constrained_def xctx scheme te)
       | _ ->
         let inst = List.find_opt (fun (i : Types.instance_def) ->
           String.equal i.inst_dict_name name && i.inst_constraints <> []
         ) xctx.xf_type_env.Types.instances in
         (match inst with
          | Some inst_def ->
            TDLet (name, xform_constrained_inst xctx inst_def te)
          | None ->
            TDLet (name, xform_expr xctx te)))
    | TDLetRec (name, te) ->
      (match List.assoc_opt name xctx.xf_schemes with
       | Some scheme when scheme_needs_xform scheme ->
         TDLetRec (name, xform_constrained_def xctx scheme te)
       | _ -> TDLetRec (name, xform_expr xctx te))
    | TDLetRecAnd bindings ->
      TDLetRecAnd (List.map (fun (n, te) ->
        (match List.assoc_opt n xctx.xf_schemes with
         | Some scheme when scheme_needs_xform scheme ->
           (n, xform_constrained_def xctx scheme te)
         | _ -> (n, xform_expr xctx te))
      ) bindings)
    | TDExpr te -> TDExpr (xform_expr xctx te)
    | TDLetMut (name, te) -> TDLetMut (name, xform_expr xctx te)
    | other -> other
  in
  let decl_names tdecl =
    match tdecl with
    | TDLet (name, _) | TDLetRec (name, _) | TDLetMut (name, _) -> [name]
    | TDLetRecAnd bindings -> List.map fst bindings
    | _ -> []
  in
  let open_schemes xctx tdecl =
    (* When TDOpen creates aliases (dst, src), propagate schemes from src to dst *)
    match tdecl with
    | TDOpen alias_pairs ->
      List.fold_left (fun schemes (dst, src) ->
        match List.assoc_opt src schemes with
        | Some scheme -> (dst, scheme) :: schemes
        | None -> schemes
      ) xctx.xf_schemes alias_pairs
    | _ -> xctx.xf_schemes
  in
  let rec xform_decls xctx = function
    | [] -> []
    | tdecl :: rest ->
      let tdecl' = match tdecl with
        | TDModule (name, decls) ->
          TDModule (name, xform_decls xctx decls)
        | _ -> xform_decl xctx tdecl
      in
      let names = decl_names tdecl in
      let xctx' = { xctx with
        xf_locals = names @ xctx.xf_locals;
        xf_schemes = open_schemes xctx tdecl';
      } in
      tdecl' :: xform_decls xctx' rest
  in
  xform_decls xctx tprog

let check_program (program : Ast.program) : tprogram * Types.type_env =
  let ctx, decls = List.fold_left (fun (ctx, decls) decl ->
    let ctx', tdecls = check_decl ctx 0 decl in
    (ctx', List.rev_append tdecls decls)
  ) (empty_ctx, []) program in
  (List.rev decls, ctx.type_env)

let check_program_in_ctx (ctx : ctx) (program : Ast.program) : ctx * tprogram =
  let ctx, decls = List.fold_left (fun (ctx, decls) decl ->
    let ctx', tdecls = check_decl ctx 0 decl in
    (ctx', List.rev_append tdecls decls)
  ) (ctx, []) program in
  (ctx, List.rev decls)
