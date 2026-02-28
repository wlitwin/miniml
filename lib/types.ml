exception Unify_error of string

(* ---- Type variables ---- *)

type tvar =
  | Unbound of int * int   (* id, level *)
  | Link of ty

and ty =
  | TInt
  | TFloat
  | TBool
  | TString
  | TByte
  | TRune
  | TUnit
  | TArrow of ty * eff * ty
  | TCont of ty * eff * ty    (* continuation: arg_ty * eff * result_ty *)
  | TTuple of ty list
  | TList of ty
  | TRecord of record_row
  | TVariant of string * ty list
  | TArray of ty
  | TPolyVariant of pvrow
  | TVar of tvar ref
  | TGen of int    (* quantified variable index in a scheme *)

(* ---- Effect rows ---- *)

and eff =
  | EffVar of effvar ref     (* effect row variable *)
  | EffEmpty                 (* pure — no effects *)
  | EffRow of string * ty list * eff   (* effect label, type params, tail *)
  | EffGen of int            (* quantified effect var index in a scheme *)

and effvar =
  | EffUnbound of int * int  (* id, level *)
  | EffLink of eff           (* linked by unification *)

(* ---- Polymorphic variant rows ---- *)

and pvrow =
  | PVRow of string * ty option * pvrow  (* tag, optional payload type, tail *)
  | PVVar of pvvar ref                   (* row variable *)
  | PVEmpty                              (* closed row — exact type *)
  | PVGen of int                         (* quantified row variable *)

and pvvar =
  | PVUnbound of int * int               (* id, level *)
  | PVLink of pvrow                      (* linked by unification *)

(* ---- Record rows ---- *)

and record_row =
  | RRow of string * ty * record_row     (* field name, type, tail *)
  | RVar of rvar ref                     (* row variable — "and more fields" *)
  | REmpty                               (* closed — exact record *)
  | RGen of int                          (* quantified row variable *)
  | RWild                                (* recursive self-reference — unifies with any row *)

and rvar =
  | RUnbound of int * int               (* id, level *)
  | RLink of record_row                 (* linked by unification *)

type class_arg =
  | CATGen of int    (* polymorphic: TGen index, resolved at call site *)
  | CATy of ty       (* concrete: fixed type, checked at call site *)
  | CAWild           (* fundep-determined: not in method type, resolved via fundep at call site *)
  | CAPhantom of int * ty  (* fundep-determined from annotation: unique index + actual tvar *)

type class_constraint = {
  cc_class: string;     (* class name, e.g. "Show" *)
  cc_args: class_arg list;  (* one per class param: TGen index or concrete type *)
}

type record_evidence = {
  re_fields: string list;    (* field names needing offset evidence *)
  re_rgen: int;              (* which RGen row variable this applies to *)
}

type scheme = {
  quant: int;    (* number of quantified type variables *)
  equant: int;   (* number of quantified effect variables *)
  pvquant: int;  (* number of quantified poly variant row variables *)
  rquant: int;   (* number of quantified record row variables *)
  constraints: class_constraint list;  (* typeclass constraints *)
  record_evidences: record_evidence list;  (* field offset evidence for row-polymorphic updates *)
  body: ty;      (* body containing TGen 0..quant-1 and EffGen 0..equant-1 *)
}

let mono ty = { quant = 0; equant = 0; pvquant = 0; rquant = 0; constraints = []; record_evidences = []; body = ty }

(* Global counter for fresh type variable ids *)
let next_id = ref 0

let fresh_id () =
  let id = !next_id in
  next_id := id + 1;
  id

let new_tvar level =
  TVar (ref (Unbound (fresh_id (), level)))

let new_effvar level =
  EffVar (ref (EffUnbound (fresh_id (), level)))

let new_pvvar level =
  PVVar (ref (PVUnbound (fresh_id (), level)))

let new_rvar level =
  RVar (ref (RUnbound (fresh_id (), level)))

(* Follow links to find the representative type (with path compression) *)
let rec repr = function
  | TVar ({ contents = Link ty } as r) ->
    let ty = repr ty in
    r := Link ty;
    ty
  | ty -> ty

(* Check if a type is fully concrete (no unresolved type variables) *)
let rec is_concrete ty =
  match repr ty with
  | TVar { contents = Unbound _ } -> false
  | TVar { contents = Link _ } -> assert false  (* repr already followed *)
  | TArrow (a, eff, r) | TCont (a, eff, r) -> is_concrete a && is_concrete_eff eff && is_concrete r
  | TTuple ts -> List.for_all is_concrete ts
  | TList t | TArray t -> is_concrete t
  | TVariant (_, args) -> List.for_all is_concrete args
  | TInt | TFloat | TBool | TString | TByte | TRune | TUnit | TGen _ -> true
  | TRecord row -> is_concrete_rrow row
  | TPolyVariant row -> is_concrete_pv row
and is_concrete_eff = function
  | EffVar { contents = EffLink eff } -> is_concrete_eff eff
  | EffVar { contents = EffUnbound _ } -> false
  | EffRow (_, tys, tail) -> List.for_all is_concrete tys && is_concrete_eff tail
  | EffEmpty -> true
  | EffGen _ -> true
and is_concrete_rrow = function
  | RRow (_, ty, tail) -> is_concrete ty && is_concrete_rrow tail
  | RVar { contents = RLink row } -> is_concrete_rrow row
  | RVar { contents = RUnbound _ } -> false
  | REmpty -> true
  | RGen _ | RWild -> true
and is_concrete_pv = function
  | PVRow (_, Some ty, tail) -> is_concrete ty && is_concrete_pv tail
  | PVRow (_, None, tail) -> is_concrete_pv tail
  | PVVar { contents = PVLink row } -> is_concrete_pv row
  | PVVar { contents = PVUnbound _ } -> false
  | PVEmpty -> true
  | PVGen _ -> true

(* Follow links for effect rows (with path compression) *)
let rec eff_repr = function
  | EffVar ({ contents = EffLink eff } as r) ->
    let eff = eff_repr eff in
    r := EffLink eff;
    eff
  | eff -> eff

(* Follow links for poly variant rows (with path compression) *)
let rec pv_repr = function
  | PVVar ({ contents = PVLink row } as r) ->
    let row = pv_repr row in
    r := PVLink row;
    row
  | row -> row

(* Follow links for record rows (with path compression) *)
let rec rrow_repr = function
  | RVar ({ contents = RLink row } as r) ->
    let row = rrow_repr row in
    r := RLink row;
    row
  | row -> row

let fields_to_closed_row fields =
  List.fold_right (fun (n, t) acc -> RRow (n, t, acc)) fields REmpty

let record_row_to_fields row =
  let rec collect r = match rrow_repr r with
    | RRow (n, t, tail) -> (n, t) :: collect tail
    | _ -> []
  in collect row

(* ---- Variant/record type environment ---- *)

type variant_def = (string * ty option) list

type ctor_info = {
  ctor_type_name: string;
  ctor_arg_ty: ty option;
  ctor_num_params: int;
  ctor_return_ty_params: ty list option;  (* None = ADT, Some = GADT return type params *)
  ctor_existentials: int;                  (* existential type vars in this constructor *)
}

type fundep = {
  fd_from: int list;   (* indices of determining params *)
  fd_to: int list;     (* indices of determined params *)
}

type class_def = {
  class_name: string;
  class_params: string list;
  class_methods: (string * ty) list;  (* types use TGen 0..N-1 for class params *)
  class_fundeps: fundep list;
}

type instance_def = {
  inst_class: string;
  inst_tys: ty list;
  inst_dict_name: string;
  inst_constraints: class_constraint list;
}

type effect_def = {
  effect_name: string;
  effect_params: string list;   (* type parameter names, e.g. ["a"] for State 'a *)
  effect_ops: (string * ty) list;
}

type module_info = {
  mod_name: string;
  mod_pub_vars: (string * scheme) list;
  mod_pub_mutable_vars: string list;
  mod_pub_types: string list;
  mod_opaque_types: string list;
  mod_newtypes: string list;
  mod_pub_constructors: (string * ctor_info) list;
  mod_instances: instance_def list;
  mod_submodules: (string * module_info) list;
  mod_pub_classes: string list;
}

type type_env = {
  variants: (string * int * variant_def * bool) list;  (* name, num_params, ctors, is_gadt *)
  constructors: (string * ctor_info) list;
  records: (string * (string * ty) list) list;
  classes: class_def list;
  instances: instance_def list;
  effects: effect_def list;
  mutable_fields: string list;
  modules: (string * module_info) list;
  type_aliases: (string * string) list;  (* short_name -> qualified_name *)
  type_synonyms: (string * int * ty) list;  (* name, num_params, expanded_ty *)
  newtypes: string list;  (* type names whose single constructor is erased at runtime *)
  hidden_types: string list;  (* module-private type names not accessible from outer scope *)
  hidden_ctor_types: string list;  (* type names whose constructors are hidden (private + opaque) *)
}

let empty_type_env = {
  variants = [];
  constructors = [];
  records = [];
  classes = [];
  instances = [];
  effects = [];
  mutable_fields = [];
  modules = [];
  type_aliases = [];
  type_synonyms = [];
  newtypes = [];
  hidden_types = [];
  hidden_ctor_types = [];
}

let find_effect_op type_env op_name =
  List.find_map (fun (edef : effect_def) ->
    List.find_map (fun (name, ty) ->
      if String.equal name op_name then Some (edef.effect_name, ty)
      else None
    ) edef.effect_ops
  ) type_env.effects

let rec ty_to_str = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TByte -> "byte"
  | TRune -> "rune"
  | TUnit -> "unit"
  | TVariant (name, []) -> name
  | TVariant (name, args) ->
    String.concat "_" (List.map ty_to_str args) ^ "_" ^ name
  | TList t -> ty_to_str t ^ "_list"
  | TArray t -> ty_to_str t ^ "_array"
  | TTuple ts -> String.concat "_" (List.map ty_to_str ts) ^ "_tup"
  | TPolyVariant _ -> "polyvar"
  | TGen i -> Printf.sprintf "g%d" i
  | TArrow _ | TCont _ -> "arrow"
  | TVar _ -> "tvar"
  | _ -> "ty_other"

let dict_name class_name tys =
  let ty_strs = List.map ty_to_str tys in
  Printf.sprintf "__dict_%s_%s" class_name (String.concat "__" ty_strs)

let find_method_class classes method_name =
  List.find_opt (fun cls ->
    List.exists (fun (m, _) -> String.equal m method_name) cls.class_methods
  ) classes

(* ---- Occurs check and level adjustment ---- *)

let rec occurs_check id level ty =
  match repr ty with
  | TVar { contents = Unbound (id2, _) } when id = id2 ->
    raise (Unify_error "occurs check: infinite type")
  | TVar ({ contents = Unbound (id2, level2) } as r) ->
    (* Adjust level: if the other var is at a deeper level, pull it up *)
    if level2 > level then r := Unbound (id2, level)
  | TVar { contents = Link _ } -> assert false (* repr should have resolved *)
  | TArrow (a, eff, b) | TCont (a, eff, b) ->
    occurs_check id level a;
    occurs_check_eff_for_ty id level eff;
    occurs_check id level b
  | TTuple ts -> List.iter (occurs_check id level) ts
  | TList t -> occurs_check id level t
  | TArray t -> occurs_check id level t
  | TRecord row -> occurs_check_rrow_for_ty id level row
  | TVariant (_, args) -> List.iter (occurs_check id level) args
  | TPolyVariant row -> occurs_check_pv_for_ty id level row
  | TInt | TFloat | TBool | TString | TByte | TRune | TUnit | TGen _ -> ()

(* Walk poly variant rows checking for type variable id *)
and occurs_check_pv_for_ty id level row =
  match pv_repr row with
  | PVRow (_, ty_opt, tail) ->
    (match ty_opt with Some t -> occurs_check id level t | None -> ());
    occurs_check_pv_for_ty id level tail
  | _ -> ()

(* Walk record rows checking for type variable id *)
and occurs_check_rrow_for_ty id level row =
  match rrow_repr row with
  | RRow (_, ty, tail) ->
    occurs_check id level ty;
    occurs_check_rrow_for_ty id level tail
  | _ -> ()

(* Walk effect rows checking for type variable id (for occurs check on type vars) *)
and occurs_check_eff_for_ty id level eff =
  match eff_repr eff with
  | EffRow (_, params, tail) ->
    List.iter (occurs_check id level) params;
    occurs_check_eff_for_ty id level tail
  | _ -> () (* eff_repr resolved links; only EffVar(Unbound), EffEmpty, EffGen remain *)

(* Occurs check for effect variables *)
let rec occurs_check_eff id level eff =
  match eff_repr eff with
  | EffVar { contents = EffUnbound (id2, _) } when id = id2 ->
    raise (Unify_error "occurs check: infinite effect row")
  | EffVar ({ contents = EffUnbound (id2, level2) } as r) ->
    if level2 > level then r := EffUnbound (id2, level)
  | EffVar { contents = EffLink _ } -> assert false
  | EffRow (_, _params, tail) -> occurs_check_eff id level tail
  | EffEmpty | EffGen _ -> ()

(* Occurs check for polymorphic variant row variables *)
let rec occurs_check_pv id level row =
  match pv_repr row with
  | PVVar { contents = PVUnbound (id2, _) } when id = id2 ->
    raise (Unify_error "occurs check: infinite polymorphic variant row")
  | PVVar ({ contents = PVUnbound (id2, level2) } as r) ->
    if level2 > level then r := PVUnbound (id2, level)
  | PVVar { contents = PVLink _ } -> assert false
  | PVRow (_, ty_opt, tail) ->
    (match ty_opt with Some t -> occurs_check id level t | None -> ());
    occurs_check_pv id level tail
  | PVEmpty | PVGen _ -> ()

(* Occurs check for record row variables *)
let rec occurs_check_rrow id level row =
  match rrow_repr row with
  | RVar { contents = RUnbound (id2, _) } when id = id2 ->
    raise (Unify_error "occurs check: infinite record row")
  | RVar ({ contents = RUnbound (id2, level2) } as r) ->
    if level2 > level then r := RUnbound (id2, level)
  | RVar { contents = RLink _ } -> assert false
  | RRow (_, ty, tail) ->
    occurs_check id level ty;
    occurs_check_rrow id level tail
  | REmpty | RGen _ | RWild -> ()

(* ---- Pretty printing ---- *)

let pp_synonyms : (string * int * ty) list ref = ref []
let pp_ty_arg_ref : (ty -> string) ref = ref (fun _ -> "?")

let try_match_synonym ty =
  let synonyms = !pp_synonyms in
  if synonyms = [] then None
  else
    List.find_map (fun (name, num_params, pattern) ->
      let bindings = Array.make num_params None in
      let rec go pat actual =
        let actual = repr actual in
        match pat, actual with
        | TGen i, _ when i < num_params ->
          (match bindings.(i) with
           | None -> bindings.(i) <- Some actual; true
           | Some prev -> prev = actual)
        | TInt, TInt | TFloat, TFloat | TBool, TBool
        | TString, TString | TUnit, TUnit | TByte, TByte | TRune, TRune -> true
        | TArrow (p1, _pe, p2), TArrow (a1, _ae, a2)
        | TCont (p1, _pe, p2), TCont (a1, _ae, a2) -> go p1 a1 && go p2 a2
        | TList p1, TList a1 -> go p1 a1
        | TArray p1, TArray a1 -> go p1 a1
        | TTuple ps, TTuple acts when List.length ps = List.length acts ->
          List.for_all2 go ps acts
        | TVariant (pn, ps), TVariant (an, acts)
          when String.equal pn an && List.length ps = List.length acts ->
          List.for_all2 go ps acts
        | TVar r1, TVar r2 when r1 == r2 -> true
        | TGen i, TGen j when i = j -> true
        | _ -> false
      in
      if go pattern ty && Array.for_all (fun x -> x <> None) bindings then
        Some (name, Array.to_list (Array.map (fun x -> Option.get x) bindings))
      else
        None
    ) synonyms

let rec pp_eff eff =
  match eff_repr eff with
  | EffEmpty -> ""
  | EffVar { contents = EffUnbound (id, _) } ->
    "'" ^ String.make 1 (Char.chr (Char.code 'e' + id mod 22))
  | EffRow (label, params, tail) ->
    let param_str = match params with
      | [] -> ""
      | ps -> " " ^ String.concat " " (List.map !pp_ty_arg_ref ps)
    in
    let tail_str = match eff_repr tail with
      | EffEmpty -> ""
      | EffVar { contents = EffUnbound _ } -> ", " ^ pp_eff tail
      | EffRow _ -> ", " ^ pp_eff tail
      | _ -> ""
    in
    label ^ param_str ^ tail_str
  | EffGen id ->
    "'" ^ String.make 1 (Char.chr (Char.code 'e' + id mod 22))
  | EffVar { contents = EffLink _ } -> assert false

let eff_is_trivial eff =
  match eff_repr eff with
  | EffEmpty -> true
  | EffVar { contents = EffUnbound _ } -> true
  | EffGen _ -> true
  | _ -> false

let rec pp_ty ty =
  let ty = repr ty in
  match try_match_synonym ty with
  | Some (name, args) ->
    (* For arrow types, if the argument also matches a synonym, prefer decomposing
       the arrow to avoid false matches (e.g., 'a seq -> unit shown as ('a -> unit) seq) *)
    (match ty with
     | (TArrow (a, _, _) | TCont (a, _, _)) when try_match_synonym (repr a) <> None ->
       pp_ty_raw ty
     | _ -> pp_type_con name args)
  | None -> pp_ty_raw ty

and pp_type_con name = function
  | [] -> name
  | [arg] ->
    (match repr arg with
     | TArrow _ | TCont _ | TTuple _ -> "(" ^ pp_ty arg ^ ")"
     | _ -> pp_ty arg) ^ " " ^ name
  | args ->
    "(" ^ String.concat ", " (List.map pp_ty args) ^ ") " ^ name

(* Check if a type needs parens when used as an arrow argument or type con arg.
   Types that collapse to a synonym don't need parens even if their raw form is an arrow. *)
and needs_parens ty =
  let ty = repr ty in
  match try_match_synonym ty with
  | Some _ -> false
  | None -> match ty with TArrow _ | TCont _ | TTuple _ -> true | _ -> false

and pp_ty_arg ty =
  if needs_parens ty then "(" ^ pp_ty ty ^ ")" else pp_ty ty

and pp_ty_raw ty =
  match ty with
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TByte -> "byte"
  | TRune -> "rune"
  | TUnit -> "unit"
  | TArrow (a, eff, r) ->
    let a_str = match repr a with
      | TArrow _ when try_match_synonym (repr a) = None ->
        "(" ^ pp_ty a ^ ")"
      | _ -> pp_ty a
    in
    if eff_is_trivial eff then
      a_str ^ " -> " ^ pp_ty r
    else
      a_str ^ " -> " ^ pp_ty r ^ " / " ^ pp_eff eff
  | TCont (a, eff, r) ->
    let inner =
      let a_str = match repr a with
        | TArrow _ | TCont _ when try_match_synonym (repr a) = None ->
          "(" ^ pp_ty a ^ ")"
        | _ -> pp_ty a
      in
      if eff_is_trivial eff then
        a_str ^ " -> " ^ pp_ty r
      else
        a_str ^ " -> " ^ pp_ty r ^ " / " ^ pp_eff eff
    in
    "continuation(" ^ inner ^ ")"
  | TTuple ts ->
    String.concat " * " (List.map (fun t ->
      match repr t with
      | (TArrow _ | TCont _) when try_match_synonym (repr t) = None ->
        "(" ^ pp_ty t ^ ")"
      | _ -> pp_ty t
    ) ts)
  | TList t -> pp_ty_arg t ^ " list"
  | TArray t -> pp_ty_arg t ^ " array"
  | TRecord row ->
    let rec collect r = match rrow_repr r with
      | RRow (name, ty, tail) -> (name, ty) :: collect tail
      | _ -> []
    in
    let rec is_open r = match rrow_repr r with
      | RRow (_, _, tail) -> is_open tail
      | RVar { contents = RUnbound _ } | RGen _ | RWild -> true
      | _ -> false
    in
    let fields_str = String.concat "; " (List.map (fun (n, t) ->
      n ^ ": " ^ pp_ty t
    ) (collect row)) in
    if is_open row then
      "{ " ^ fields_str ^ (if fields_str = "" then ".." else "; ..") ^ " }"
    else
      "{ " ^ fields_str ^ " }"
  | TVariant (name, []) -> name
  | TVariant (name, [arg]) -> pp_ty_arg arg ^ " " ^ name
  | TVariant (name, args) ->
    "(" ^ String.concat ", " (List.map pp_ty args) ^ ") " ^ name
  | TPolyVariant row ->
    let rec collect_tags r =
      match pv_repr r with
      | PVRow (tag, ty_opt, tail) -> (tag, ty_opt) :: collect_tags tail
      | _ -> []
    in
    let is_open = match pv_repr row with
      | PVVar _ -> true
      | PVRow _ ->
        let rec check r = match pv_repr r with
          | PVRow (_, _, tail) -> check tail
          | PVVar _ -> true
          | _ -> false
        in check row
      | _ -> false
    in
    let tags = collect_tags row in
    let tag_strs = List.map (fun (tag, ty_opt) ->
      match ty_opt with
      | None -> "`" ^ tag
      | Some t -> "`" ^ tag ^ " of " ^ pp_ty t
    ) tags in
    let marker = if is_open then "> " else "" in
    "[" ^ marker ^ String.concat " | " tag_strs ^ "]"
  | TVar { contents = Unbound (id, _) } ->
    "'" ^ String.make 1 (Char.chr (Char.code 'a' + id mod 26))
  | TVar { contents = Link _ } -> assert false
  | TGen id ->
    "'" ^ String.make 1 (Char.chr (Char.code 'a' + id mod 26))

let () = pp_ty_arg_ref := pp_ty_arg

(* ---- Unification ---- *)

let unify_error t1 t2 =
  raise (Unify_error (Printf.sprintf "cannot unify %s with %s"
    (pp_ty t1) (pp_ty t2)))

(* Effect row unification (Remy-style) *)
let rec unify_eff e1 e2 =
  let e1 = eff_repr e1 in
  let e2 = eff_repr e2 in
  if e1 == e2 then ()
  else match e1, e2 with
  | EffVar ({ contents = EffUnbound (id, level) } as r), eff
  | eff, EffVar ({ contents = EffUnbound (id, level) } as r) ->
    occurs_check_eff id level eff;
    r := EffLink eff
  | EffEmpty, EffEmpty -> ()
  | EffRow (label1, params1, tail1), EffRow (label2, params2, tail2)
    when String.equal label1 label2 && List.length params1 = List.length params2 ->
    List.iter2 unify params1 params2;
    unify_eff tail1 tail2
  | EffRow (label1, params1, tail1), _ ->
    let tail2 = rewrite_row label1 params1 e2 in
    unify_eff tail1 tail2
  | _, EffRow _ ->
    unify_eff e2 e1
  | EffGen i, EffGen j when i = j -> ()
  | _ ->
    raise (Unify_error (Printf.sprintf "cannot unify effects %s and %s"
      (pp_eff e1) (pp_eff e2)))

(* Find label in row, unify params, and return the row without it *)
and rewrite_row label params_to_unify eff =
  match eff_repr eff with
  | EffRow (l, found_params, tail) when String.equal l label ->
    if List.length params_to_unify = List.length found_params then
      List.iter2 unify params_to_unify found_params;
    tail
  | EffRow (l, params, tail) ->
    EffRow (l, params, rewrite_row label params_to_unify tail)
  | EffVar ({ contents = EffUnbound (id, level) } as r) ->
    let new_tail = new_effvar level in
    let new_row = EffRow (label, params_to_unify, new_tail) in
    occurs_check_eff id level new_row;
    r := EffLink new_row;
    new_tail
  | EffEmpty ->
    raise (Unify_error (Printf.sprintf "effect %s not handled" label))
  | _ ->
    raise (Unify_error (Printf.sprintf "cannot rewrite effect row for %s" label))

(* Subeffect: source's effects must be present in target.
   Unlike unify_eff, EffEmpty is a sub of anything (pure is always ok). *)
and subeffect source target =
  let source = eff_repr source in
  let target = eff_repr target in
  if source == target then ()
  else match source with
  | EffEmpty -> ()
  | EffVar ({ contents = EffUnbound _ }) ->
    unify_eff source target
  | EffRow (label, params, tail) ->
    let target_tail = rewrite_row label params target in
    subeffect (eff_repr tail) target_tail
  | EffGen _ -> ()
  | _ -> unify_eff source target

(* Polymorphic variant row unification (Remy-style) *)
and unify_pv_row r1 r2 =
  let r1 = pv_repr r1 in
  let r2 = pv_repr r2 in
  if r1 == r2 then ()
  else match r1, r2 with
  | PVVar ({ contents = PVUnbound (id, level) } as r), row
  | row, PVVar ({ contents = PVUnbound (id, level) } as r) ->
    occurs_check_pv id level row;
    r := PVLink row
  | PVEmpty, PVEmpty -> ()
  | PVRow (tag1, ty1, tail1), PVRow (tag2, ty2, tail2) when String.equal tag1 tag2 ->
    unify_pv_payload ty1 ty2;
    unify_pv_row tail1 tail2
  | PVRow (tag1, ty1, tail1), _ ->
    let (ty2, tail2) = rewrite_pv_row tag1 ty1 r2 in
    unify_pv_payload ty1 ty2;
    unify_pv_row tail1 tail2
  | _, PVRow _ ->
    unify_pv_row r2 r1
  | PVGen i, PVGen j when i = j -> ()
  | _ ->
    raise (Unify_error "cannot unify polymorphic variant types")

and unify_pv_payload ty1 ty2 =
  match ty1, ty2 with
  | None, None -> ()
  | Some t1, Some t2 -> unify t1 t2
  | _ -> raise (Unify_error "polymorphic variant payload mismatch")

(* Find tag in pvrow and return its payload type and the remaining row *)
and rewrite_pv_row tag default_payload row =
  match pv_repr row with
  | PVRow (t, ty_opt, tail) when String.equal t tag -> (ty_opt, tail)
  | PVRow (t, ty_opt, tail) ->
    let (found, rest) = rewrite_pv_row tag default_payload tail in
    (found, PVRow (t, ty_opt, rest))
  | PVVar ({ contents = PVUnbound (id, level) } as r) ->
    let new_tail = new_pvvar level in
    let new_row = PVRow (tag, default_payload, new_tail) in
    occurs_check_pv id level new_row;
    r := PVLink new_row;
    (default_payload, new_tail)
  | PVEmpty ->
    raise (Unify_error (Printf.sprintf "tag `%s not in closed polymorphic variant type" tag))
  | _ ->
    raise (Unify_error (Printf.sprintf "cannot rewrite polymorphic variant row for `%s" tag))

(* Record row unification (Remy-style) *)
and unify_record_row r1 r2 =
  let r1 = rrow_repr r1 in
  let r2 = rrow_repr r2 in
  if r1 == r2 then ()
  else match r1, r2 with
  | RVar ({ contents = RUnbound (id, level) } as r), row
  | row, RVar ({ contents = RUnbound (id, level) } as r) ->
    occurs_check_rrow id level row;
    r := RLink row
  | RWild, _ | _, RWild -> () (* recursive self-reference — accept any row *)
  | REmpty, REmpty -> ()
  | RRow (f1, ty1, tail1), RRow (f2, ty2, tail2) when String.equal f1 f2 ->
    unify ty1 ty2;
    unify_record_row tail1 tail2
  | RRow (f1, ty1, tail1), _ ->
    let (ty2, tail2) = rewrite_record_row f1 r2 in
    unify ty1 ty2;
    unify_record_row tail1 tail2
  | _, RRow _ -> unify_record_row r2 r1
  | RGen i, RGen j when i = j -> ()
  | _ -> raise (Unify_error "cannot unify record types")

and rewrite_record_row field row =
  match rrow_repr row with
  | RRow (f, ty, tail) when String.equal f field -> (ty, tail)
  | RRow (f, ty, tail) ->
    let (found, rest) = rewrite_record_row field tail in
    (found, RRow (f, ty, rest))
  | RVar ({ contents = RUnbound (id, level) } as r) ->
    let new_tail = new_rvar level in
    let field_ty = new_tvar level in
    let new_row = RRow (field, field_ty, new_tail) in
    occurs_check_rrow id level new_row;
    r := RLink new_row;
    (field_ty, new_tail)
  | RWild ->
    (* Recursive self-reference — allow any field *)
    let field_ty = new_tvar 0 in
    (field_ty, RWild)
  | REmpty -> raise (Unify_error (Printf.sprintf "record has no field %s" field))
  | _ -> raise (Unify_error (Printf.sprintf "cannot rewrite record row for %s" field))

and unify t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () (* physical equality — same ref *)
  else match t1, t2 with
  | TVar ({ contents = Unbound (id, level) } as r), ty
  | ty, TVar ({ contents = Unbound (id, level) } as r) ->
    occurs_check id level ty;
    r := Link ty
  | TArrow (a1, e1, r1), TArrow (a2, e2, r2)
  | TCont (a1, e1, r1), TCont (a2, e2, r2) ->
    unify a1 a2;
    unify_eff e1 e2;
    unify r1 r2
  | TTuple ts1, TTuple ts2 ->
    if List.length ts1 <> List.length ts2 then
      unify_error t1 t2;
    List.iter2 unify ts1 ts2
  | TList t1', TList t2' ->
    unify t1' t2'
  | TArray t1', TArray t2' ->
    unify t1' t2'
  | TRecord r1, TRecord r2 ->
    unify_record_row r1 r2
  | TInt, TInt | TFloat, TFloat | TBool, TBool
  | TString, TString | TByte, TByte | TRune, TRune | TUnit, TUnit -> ()
  | TVariant (a, args_a), TVariant (b, args_b) when String.equal a b ->
    if List.length args_a <> List.length args_b then unify_error t1 t2;
    List.iter2 unify args_a args_b
  | TPolyVariant r1, TPolyVariant r2 ->
    unify_pv_row r1 r2
  | _ -> unify_error t1 t2

(* ---- Generalization and instantiation ---- *)

(* Generalize: replace unbound type variables at levels > `level` with TGen,
   and unbound effect variables with EffGen *)
let generalize level ty =
  let (id_map : (int, int) Hashtbl.t) = Hashtbl.create 8 in
  let counter = ref 0 in
  let (eff_id_map : (int, int) Hashtbl.t) = Hashtbl.create 4 in
  let ecounter = ref 0 in
  let (pv_id_map : (int, int) Hashtbl.t) = Hashtbl.create 4 in
  let pvcounter = ref 0 in
  let (r_id_map : (int, int) Hashtbl.t) = Hashtbl.create 4 in
  let rcounter = ref 0 in
  let rec go ty =
    match repr ty with
    | TVar { contents = Unbound (id, level') } when level' > level ->
      (match Hashtbl.find_opt id_map id with
       | Some gen_id -> TGen gen_id
       | None ->
         let gen_id = !counter in
         counter := gen_id + 1;
         Hashtbl.replace id_map id gen_id;
         TGen gen_id)
    | TVar { contents = Unbound _ } as t -> t
    | TVar { contents = Link _ } -> assert false
    | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
    | TCont (a, eff, r) -> TCont (go a, go_eff eff, go r)
    | TTuple ts -> TTuple (List.map go ts)
    | TList t -> TList (go t)
    | TArray t -> TArray (go t)
    | TRecord row -> TRecord (go_rrow row)
    | TVariant (name, args) -> TVariant (name, List.map go args)
    | TPolyVariant row -> TPolyVariant (go_pv row)
    | (TInt | TFloat | TBool | TString | TByte | TRune | TUnit | TGen _) as t -> t
  and go_eff eff =
    match eff_repr eff with
    | EffVar { contents = EffUnbound (id, level') } when level' > level ->
      (match Hashtbl.find_opt eff_id_map id with
       | Some gen_id -> EffGen gen_id
       | None ->
         let gen_id = !ecounter in
         ecounter := gen_id + 1;
         Hashtbl.replace eff_id_map id gen_id;
         EffGen gen_id)
    | EffVar { contents = EffUnbound _ } as e -> e
    | EffVar { contents = EffLink _ } -> assert false
    | EffRow (label, params, tail) -> EffRow (label, List.map go params, go_eff tail)
    | EffEmpty -> EffEmpty
    | EffGen _ as e -> e
  and go_pv row =
    match pv_repr row with
    | PVVar { contents = PVUnbound (id, level') } when level' > level ->
      (match Hashtbl.find_opt pv_id_map id with
       | Some gen_id -> PVGen gen_id
       | None ->
         let gen_id = !pvcounter in
         pvcounter := gen_id + 1;
         Hashtbl.replace pv_id_map id gen_id;
         PVGen gen_id)
    | PVVar { contents = PVUnbound _ } as r -> r
    | PVVar { contents = PVLink _ } -> assert false
    | PVRow (tag, ty_opt, tail) -> PVRow (tag, Option.map go ty_opt, go_pv tail)
    | PVEmpty -> PVEmpty
    | PVGen _ as r -> r
  and go_rrow row =
    match rrow_repr row with
    | RVar { contents = RUnbound (id, level') } when level' > level ->
      (match Hashtbl.find_opt r_id_map id with
       | Some gen_id -> RGen gen_id
       | None ->
         let gen_id = !rcounter in
         rcounter := gen_id + 1;
         Hashtbl.replace r_id_map id gen_id;
         RGen gen_id)
    | RVar { contents = RUnbound _ } as r -> r
    | RVar { contents = RLink _ } -> assert false
    | RRow (name, ty, tail) -> RRow (name, go ty, go_rrow tail)
    | REmpty -> REmpty
    | RGen _ as r -> r
    | RWild -> RWild
  in
  let body = go ty in
  { quant = !counter; equant = !ecounter; pvquant = !pvcounter; rquant = !rcounter; constraints = []; record_evidences = []; body }

(* Generalize and return the tvar_id -> TGen index mapping *)
let generalize_with_map level ty =
  let counter = ref 0 in
  let (id_map : (int, ty) Hashtbl.t) = Hashtbl.create 8 in
  let ecounter = ref 0 in
  let (eff_id_map : (int, eff) Hashtbl.t) = Hashtbl.create 4 in
  let pvcounter = ref 0 in
  let (pv_id_map : (int, pvrow) Hashtbl.t) = Hashtbl.create 4 in
  let rcounter = ref 0 in
  let (r_id_map : (int, record_row) Hashtbl.t) = Hashtbl.create 4 in
  let rec go ty =
    match repr ty with
    | TVar { contents = Unbound (id, l) } when l > level ->
      (match Hashtbl.find_opt id_map id with
       | Some gen -> gen
       | None ->
         let gen = TGen !counter in
         incr counter;
         Hashtbl.replace id_map id gen;
         gen)
    | TVar { contents = Unbound _ } | TVar { contents = Link _ } -> ty
    | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
    | TCont (a, eff, r) -> TCont (go a, go_eff eff, go r)
    | TTuple ts -> TTuple (List.map go ts)
    | TList t -> TList (go t)
    | TArray t -> TArray (go t)
    | TRecord row -> TRecord (go_rrow row)
    | TVariant (name, args) -> TVariant (name, List.map go args)
    | TPolyVariant row -> TPolyVariant (go_pv row)
    | (TInt | TFloat | TBool | TString | TByte | TRune | TUnit | TGen _) as t -> t
  and go_eff eff =
    match eff_repr eff with
    | EffVar { contents = EffUnbound (id, l) } when l > level ->
      (match Hashtbl.find_opt eff_id_map id with
       | Some gen -> gen
       | None ->
         let gen = EffGen !ecounter in
         incr ecounter;
         Hashtbl.replace eff_id_map id gen;
         gen)
    | EffVar { contents = EffUnbound _ } | EffVar { contents = EffLink _ } -> eff
    | EffRow (label, params, tail) -> EffRow (label, List.map go params, go_eff tail)
    | EffEmpty -> EffEmpty
    | EffGen _ as e -> e
  and go_pv row =
    match pv_repr row with
    | PVVar { contents = PVUnbound (id, l) } when l > level ->
      (match Hashtbl.find_opt pv_id_map id with
       | Some gen -> gen
       | None ->
         let gen = PVGen !pvcounter in
         incr pvcounter;
         Hashtbl.replace pv_id_map id gen;
         gen)
    | PVVar { contents = PVUnbound _ } | PVVar { contents = PVLink _ } -> row
    | PVRow (tag, ty_opt, tail) -> PVRow (tag, Option.map go ty_opt, go_pv tail)
    | PVEmpty -> PVEmpty
    | PVGen _ as r -> r
  and go_rrow row =
    match rrow_repr row with
    | RVar { contents = RUnbound (id, l) } when l > level ->
      (match Hashtbl.find_opt r_id_map id with
       | Some gen -> gen
       | None ->
         let gen = RGen !rcounter in
         incr rcounter;
         Hashtbl.replace r_id_map id gen;
         gen)
    | RVar { contents = RUnbound _ } | RVar { contents = RLink _ } -> row
    | RRow (name, ty, tail) -> RRow (name, go ty, go_rrow tail)
    | REmpty -> REmpty
    | RGen _ as r -> r
    | RWild -> RWild
  in
  let body = go ty in
  ({ quant = !counter; equant = !ecounter; pvquant = !pvcounter; rquant = !rcounter; constraints = []; record_evidences = []; body }, id_map)

(* Instantiate and return TGen index -> fresh tvar mapping *)
let instantiate_with_mapping level (s : scheme) =
  if s.quant = 0 && s.equant = 0 && s.pvquant = 0 && s.rquant = 0 then (s.body, [])
  else begin
    let vars = Array.init s.quant (fun _ -> new_tvar level) in
    let evars = Array.init s.equant (fun _ -> new_effvar level) in
    let pvvars = Array.init s.pvquant (fun _ -> new_pvvar level) in
    let rvars = Array.init s.rquant (fun _ -> new_rvar level) in
    let shared_eff = lazy (new_effvar level) in
    let mapping = List.init s.quant (fun i -> (i, vars.(i))) in
    let rec go = function
      | TGen id when id < s.quant -> vars.(id)
      | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
      | TCont (a, eff, r) -> TCont (go a, go_eff eff, go r)
      | TTuple ts -> TTuple (List.map go ts)
      | TList t -> TList (go t)
      | TArray t -> TArray (go t)
      | TRecord row -> TRecord (go_rrow row)
      | TVariant (name, args) -> TVariant (name, List.map go args)
      | TPolyVariant row -> TPolyVariant (go_pv row)
      | t -> t
    and go_eff = function
      | EffGen id when id < s.equant -> evars.(id)
      | EffRow (label, params, tail) -> EffRow (label, List.map go params, go_eff tail)
      | EffEmpty -> Lazy.force shared_eff
      | e -> e
    and go_pv = function
      | PVGen id when id < s.pvquant -> pvvars.(id)
      | PVRow (tag, ty_opt, tail) -> PVRow (tag, Option.map go ty_opt, go_pv tail)
      | r -> r
    and go_rrow = function
      | RGen id when id < s.rquant -> rvars.(id)
      | RRow (name, ty, tail) -> RRow (name, go ty, go_rrow tail)
      | r -> r
    in
    (go s.body, mapping)
  end

(* Instantiate: replace TGen with fresh type variables *)
let instantiate level (s : scheme) =
  if s.quant = 0 && s.equant = 0 && s.pvquant = 0 && s.rquant = 0 then s.body
  else begin
    let vars = Array.init s.quant (fun _ -> new_tvar level) in
    let evars = Array.init s.equant (fun _ -> new_effvar level) in
    let pvvars = Array.init s.pvquant (fun _ -> new_pvvar level) in
    let rvars = Array.init s.rquant (fun _ -> new_rvar level) in
    let shared_eff = lazy (new_effvar level) in
    let rec go = function
      | TGen id when id < s.quant -> vars.(id)
      | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
      | TCont (a, eff, r) -> TCont (go a, go_eff eff, go r)
      | TTuple ts -> TTuple (List.map go ts)
      | TList t -> TList (go t)
      | TArray t -> TArray (go t)
      | TRecord row -> TRecord (go_rrow row)
      | TVariant (name, args) -> TVariant (name, List.map go args)
      | TPolyVariant row -> TPolyVariant (go_pv row)
      | TVar { contents = Link ty } -> go ty
      | t -> t
    and go_eff = function
      | EffGen id when id < s.equant -> evars.(id)
      | EffRow (label, params, tail) -> EffRow (label, List.map go params, go_eff tail)
      | EffEmpty -> Lazy.force shared_eff
      | e -> e
    and go_pv = function
      | PVGen id when id < s.pvquant -> pvvars.(id)
      | PVRow (tag, ty_opt, tail) -> PVRow (tag, Option.map go ty_opt, go_pv tail)
      | r -> r
    and go_rrow = function
      | RGen id when id < s.rquant -> rvars.(id)
      | RRow (name, ty, tail) -> RRow (name, go ty, go_rrow tail)
      | r -> r
    in
    go s.body
  end

(* ---- Deep repr (for GADT snapshot/restore) ---- *)

let rec deep_repr ty =
  match repr ty with
  | TArrow (a, eff, b) -> TArrow (deep_repr a, deep_repr_eff eff, deep_repr b)
  | TCont (a, eff, b) -> TCont (deep_repr a, deep_repr_eff eff, deep_repr b)
  | TTuple ts -> TTuple (List.map deep_repr ts)
  | TList t -> TList (deep_repr t)
  | TArray t -> TArray (deep_repr t)
  | TRecord row -> TRecord (deep_repr_rrow row)
  | TVariant (name, args) -> TVariant (name, List.map deep_repr args)
  | TPolyVariant row -> TPolyVariant (deep_repr_pv row)
  | t -> t

and deep_repr_eff eff =
  match eff_repr eff with
  | EffRow (label, params, tail) -> EffRow (label, List.map deep_repr params, deep_repr_eff tail)
  | e -> e

and deep_repr_pv row =
  match pv_repr row with
  | PVRow (tag, ty_opt, tail) -> PVRow (tag, Option.map deep_repr ty_opt, deep_repr_pv tail)
  | r -> r

and deep_repr_rrow row =
  match rrow_repr row with
  | RRow (name, ty, tail) -> RRow (name, deep_repr ty, deep_repr_rrow tail)
  | r -> r

(* ---- Non-destructive type compatibility (for GADT exhaustiveness) ---- *)

let rec types_compatible t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  match t1, t2 with
  | TVar _, _ | _, TVar _ -> true
  | TInt, TInt | TFloat, TFloat | TBool, TBool | TString, TString
  | TByte, TByte | TRune, TRune | TUnit, TUnit -> true
  | TArrow (a1, _, r1), TArrow (a2, _, r2)
  | TCont (a1, _, r1), TCont (a2, _, r2) ->
    types_compatible a1 a2 && types_compatible r1 r2
  | TTuple ts1, TTuple ts2 ->
    List.length ts1 = List.length ts2 && List.for_all2 types_compatible ts1 ts2
  | TList t1, TList t2 -> types_compatible t1 t2
  | TArray t1, TArray t2 -> types_compatible t1 t2
  | TRecord r1, TRecord r2 ->
    let f1 = record_row_to_fields r1 in
    let f2 = record_row_to_fields r2 in
    List.length f1 = List.length f2 &&
    List.for_all2 (fun (n1, t1) (n2, t2) -> String.equal n1 n2 && types_compatible t1 t2) f1 f2
  | TVariant (a, args_a), TVariant (b, args_b) ->
    String.equal a b && List.length args_a = List.length args_b &&
    List.for_all2 types_compatible args_a args_b
  | TPolyVariant r1, TPolyVariant r2 -> pvrows_compatible r1 r2
  | _ -> false

and pvrows_compatible r1 r2 =
  let rec collect r =
    match pv_repr r with
    | PVRow (tag, ty_opt, tail) -> (tag, ty_opt) :: collect tail
    | _ -> []
  in
  let tags1 = collect r1 and tags2 = collect r2 in
  List.length tags1 = List.length tags2 &&
  List.for_all2 (fun (t1, ty1) (t2, ty2) ->
    String.equal t1 t2 &&
    match ty1, ty2 with
    | None, None -> true
    | Some a, Some b -> types_compatible a b
    | _ -> false
  ) tags1 tags2

(* ---- Subtyping (kept for record width subtyping) ---- *)

let rec subtype t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  match t1, t2 with
  | TInt, TInt | TFloat, TFloat | TBool, TBool | TString, TString
  | TByte, TByte | TRune, TRune | TUnit, TUnit -> true
  | TVariant (a, args_a), TVariant (b, args_b) ->
    String.equal a b &&
    List.length args_a = List.length args_b &&
    List.for_all2 subtype args_a args_b
  | TArrow (a1, _e1, r1), TArrow (a2, _e2, r2)
  | TCont (a1, _e1, r1), TCont (a2, _e2, r2) ->
    subtype a2 a1 && subtype r1 r2
  | TRecord r1, TRecord r2 ->
    let f1 = record_row_to_fields r1 in
    let f2 = record_row_to_fields r2 in
    List.for_all (fun (f, t2') ->
      match List.assoc_opt f f1 with
      | Some t1' -> subtype t1' t2'
      | None -> false
    ) f2
  | TList t1', TList t2' -> subtype t1' t2'
  | TArray t1', TArray t2' -> subtype t1' t2'
  | TTuple ts1, TTuple ts2 ->
    List.length ts1 = List.length ts2 &&
    List.for_all2 subtype ts1 ts2
  | TVar r1, TVar r2 when r1 == r2 -> true
  | _ -> false

let rec equal_ty t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  match t1, t2 with
  | TInt, TInt | TFloat, TFloat | TBool, TBool | TString, TString
  | TByte, TByte | TRune, TRune | TUnit, TUnit -> true
  | TVariant (a, args_a), TVariant (b, args_b) ->
    String.equal a b &&
    List.length args_a = List.length args_b &&
    List.for_all2 equal_ty args_a args_b
  | TArrow (a1, _e1, r1), TArrow (a2, _e2, r2)
  | TCont (a1, _e1, r1), TCont (a2, _e2, r2) ->
    equal_ty a1 a2 && equal_ty r1 r2
  | TRecord r1, TRecord r2 ->
    let f1 = record_row_to_fields r1 in
    let f2 = record_row_to_fields r2 in
    List.length f1 = List.length f2 &&
    List.for_all2 (fun (n1, t1) (n2, t2) -> String.equal n1 n2 && equal_ty t1 t2) f1 f2
  | TList t1', TList t2' -> equal_ty t1' t2'
  | TArray t1', TArray t2' -> equal_ty t1' t2'
  | TTuple ts1, TTuple ts2 ->
    List.length ts1 = List.length ts2 &&
    List.for_all2 equal_ty ts1 ts2
  | TVar r1, TVar r2 -> r1 == r2
  | _ -> false

(* Deep-copy a type, replacing unbound TVars with fresh copies.
   Used to prevent mutation of shared type structures (e.g. stored schemes)
   when the copy is passed to functions that may call unify. *)
let freshen_unbound ty =
  let tbl = Hashtbl.create 4 in
  let rec go ty =
    match repr ty with
    | TVar ({ contents = Unbound (id, level) }) ->
      (match Hashtbl.find_opt tbl id with
       | Some tv -> tv
       | None ->
         let tv = new_tvar level in
         Hashtbl.replace tbl id tv;
         tv)
    | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
    | TCont (a, eff, r) -> TCont (go a, go_eff eff, go r)
    | TTuple ts -> TTuple (List.map go ts)
    | TList t -> TList (go t)
    | TArray t -> TArray (go t)
    | TRecord row -> TRecord (go_rrow row)
    | TPolyVariant row -> TPolyVariant (go_pv row)
    | TVariant (name, args) -> TVariant (name, List.map go args)
    | t -> t
  and go_eff eff =
    match eff_repr eff with
    | EffRow (label, tys, tail) -> EffRow (label, List.map go tys, go_eff tail)
    | e -> e  (* EffVar (linked followed by eff_repr), EffEmpty — leave as-is *)
  and go_rrow = function
    | RRow (name, ty, tail) -> RRow (name, go ty, go_rrow tail)
    | r -> r
  and go_pv = function
    | PVRow (tag, payload, tail) -> PVRow (tag, Option.map go payload, go_pv tail)
    | p -> p  (* PVVar, PVEmpty, PVGen *)
  in
  go ty

let match_partial_inst inst_tys partial =
  let (bindings : (int, ty) Hashtbl.t) = Hashtbl.create 4 in
  let rec match_one inst conc =
    match inst with
    | TGen i ->
      (match Hashtbl.find_opt bindings i with
       | Some prev ->
         if equal_ty prev conc then true
         else (try unify prev conc; true with Unify_error _ -> false)
       | None -> Hashtbl.replace bindings i conc; true)
    | TInt -> (match repr conc with TInt -> true | _ -> false)
    | TFloat -> (match repr conc with TFloat -> true | _ -> false)
    | TBool -> (match repr conc with TBool -> true | _ -> false)
    | TString -> (match repr conc with TString -> true | _ -> false)
    | TUnit -> (match repr conc with TUnit -> true | _ -> false)
    | TList a -> (match repr conc with TList b -> match_one a b | _ -> false)
    | TArray a -> (match repr conc with TArray b -> match_one a b | _ -> false)
    | TTuple ts1 ->
      (match repr conc with
       | TTuple ts2 when List.length ts1 = List.length ts2 ->
         List.for_all2 match_one ts1 ts2
       | _ -> false)
    | TArrow (a1, _e1, a2) ->
      (match repr conc with
       | TArrow (b1, _e2, b2) -> match_one a1 b1 && match_one a2 b2
       | _ -> false)
    | TCont (a1, _e1, a2) ->
      (match repr conc with
       | TCont (b1, _e2, b2) -> match_one a1 b1 && match_one a2 b2
       | _ -> false)
    | TRecord r1 ->
      (match repr conc with
       | TRecord r2 ->
         let f1 = record_row_to_fields r1 in
         let f2 = record_row_to_fields r2 in
         List.length f1 = List.length f2 &&
         List.for_all2 (fun (n1,t1) (n2,t2) ->
           String.equal n1 n2 && match_one t1 t2) f1 f2
       | _ -> false)
    | TVariant (a, args_inst) ->
      (match repr conc with
       | TVariant (b, args_conc) when String.equal a b &&
           List.length args_inst = List.length args_conc ->
         List.for_all2 match_one args_inst args_conc
       | _ -> false)
    | _ -> equal_ty inst conc
  in
  List.length inst_tys = List.length partial &&
  List.for_all2 (fun inst_ty found_opt ->
    match found_opt with
    | None -> true
    | Some conc_ty -> match_one inst_ty conc_ty
  ) inst_tys partial

let inst_specificity (inst : instance_def) =
  let rec count = function
    | TGen _ -> 0
    | TInt | TFloat | TBool | TString | TByte | TRune | TUnit -> 1
    | TArrow (a, _, b) | TCont (a, _, b) -> count a + count b
    | TTuple ts -> List.fold_left (fun acc t -> acc + count t) 0 ts
    | TList a | TArray a -> count a
    | TRecord row -> List.fold_left (fun acc (_, t) -> acc + count t) 0 (record_row_to_fields row)
    | TVariant (_, ts) -> List.fold_left (fun acc t -> acc + count t) 0 ts
    | TPolyVariant _ -> 0
    | TVar { contents = Link t } -> count t
    | TVar { contents = Unbound _ } -> 0
  in
  List.fold_left (fun acc ty -> acc + count ty) 0 inst.inst_tys

let most_specific_inst insts =
  match insts with
  | [] -> None
  | [x] -> Some x
  | _ ->
    let scored = List.map (fun i -> (inst_specificity i, i)) insts in
    let sorted = List.sort (fun (a, _) (b, _) -> compare b a) scored in
    match sorted with
    | (s1, i1) :: (s2, _) :: _ when s1 > s2 -> Some i1
    | _ -> None

let find_instance instances class_name tys =
  List.find_opt (fun inst ->
    String.equal inst.inst_class class_name &&
    match_partial_inst inst.inst_tys (List.map (fun t -> Some t) tys)
  ) instances

(* ---- Functional dependency utilities ---- *)

let check_fundep_consistency class_def new_inst existing_instances =
  List.iter (fun fd ->
    List.iter (fun existing ->
      if not (String.equal existing.inst_class new_inst.inst_class) then ()
      else if List.length existing.inst_tys <> List.length new_inst.inst_tys then ()
      else begin
        (* Check if determining positions are structurally equal *)
        let from_match = List.for_all (fun i ->
          let ex_ty = List.nth existing.inst_tys i in
          let new_ty = List.nth new_inst.inst_tys i in
          equal_ty ex_ty new_ty
        ) fd.fd_from in
        if from_match then begin
          (* Determining types match — determined types must also match *)
          let to_match = List.for_all (fun i ->
            let ex_ty = List.nth existing.inst_tys i in
            let new_ty = List.nth new_inst.inst_tys i in
            equal_ty ex_ty new_ty
          ) fd.fd_to in
          if not to_match then
            let from_strs = List.map (fun i -> pp_ty (List.nth new_inst.inst_tys i)) fd.fd_from in
            let to_strs_new = List.map (fun i -> pp_ty (List.nth new_inst.inst_tys i)) fd.fd_to in
            let to_strs_ex = List.map (fun i -> pp_ty (List.nth existing.inst_tys i)) fd.fd_to in
            raise (Unify_error (Printf.sprintf
              "functional dependency violation in class %s: type(s) %s determine %s, but existing instance has %s"
              class_def.class_name
              (String.concat ", " from_strs)
              (String.concat ", " to_strs_new)
              (String.concat ", " to_strs_ex)))
        end
      end
    ) existing_instances
  ) class_def.class_fundeps

let improve_with_fundeps instances class_def partial =
  List.fold_left (fun partial fd ->
    (* Check if all determining positions are known *)
    let from_known = List.for_all (fun i ->
      i < List.length partial && List.nth partial i <> None
    ) fd.fd_from in
    if not from_known then partial
    else begin
      (* Build a partial that only has fd_from positions filled *)
      let from_partial = List.mapi (fun i opt ->
        if List.mem i fd.fd_from then opt else None
      ) partial in
      (* Find instances matching on the determining positions *)
      let matching = List.filter (fun inst ->
        String.equal inst.inst_class class_def.class_name &&
        List.length inst.inst_tys = List.length partial &&
        match_partial_inst inst.inst_tys from_partial
      ) instances in
      match matching with
      | [inst] ->
        (* Build TGen substitution from the match *)
        let (subst : (int, ty) Hashtbl.t) = Hashtbl.create 4 in
        List.iter (fun i ->
          let inst_ty = List.nth inst.inst_tys i in
          let conc_ty = Option.get (List.nth partial i) in
          let rec extract_bindings pat conc =
            match pat, repr conc with
            | TGen g, ty -> Hashtbl.replace subst g ty
            | TList a, TList b -> extract_bindings a b
            | TArray a, TArray b -> extract_bindings a b
            | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
              List.iter2 extract_bindings ts1 ts2
            | TVariant (_, args1), TVariant (_, args2) when List.length args1 = List.length args2 ->
              List.iter2 extract_bindings args1 args2
            | TArrow (a1, _, r1), TArrow (a2, _, r2)
            | TCont (a1, _, r1), TCont (a2, _, r2) ->
              extract_bindings a1 a2; extract_bindings r1 r2
            | _ -> ()
          in
          extract_bindings inst_ty conc_ty
        ) fd.fd_from;
        (* Apply substitution to fill fd_to positions *)
        let rec apply_subst ty =
          match ty with
          | TGen g -> (match Hashtbl.find_opt subst g with Some t -> t | None -> ty)
          | TList a -> TList (apply_subst a)
          | TArray a -> TArray (apply_subst a)
          | TTuple ts -> TTuple (List.map apply_subst ts)
          | TVariant (n, args) -> TVariant (n, List.map apply_subst args)
          | TArrow (a, e, r) -> TArrow (apply_subst a, e, apply_subst r)
          | TCont (a, e, r) -> TCont (apply_subst a, e, apply_subst r)
          | _ -> ty
        in
        List.mapi (fun i opt ->
          if opt <> None then opt
          else if List.mem i fd.fd_to then
            Some (apply_subst (List.nth inst.inst_tys i))
          else opt
        ) partial
      | _ -> partial
    end
  ) partial class_def.class_fundeps
