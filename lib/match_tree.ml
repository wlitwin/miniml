(* Match tree compilation: Maranget's algorithm for compiling pattern matching
   to good decision trees with DAG sharing.

   Reference: Luc Maranget, "Compiling Pattern Matching to Good Decision Trees"
   (ML Workshop 2008)

   This module transforms TEMatch nodes into TEMatchTree nodes in the typed AST.
   The decision tree IR is backend-agnostic: each backend just walks it emitting
   switches, loads, and branches. *)

open Match_tree_types

(* ---- Pattern matrix ---- *)

(* Normalized pattern cell for the matrix *)
type pattern_cell =
  | PCWild
  | PCConstructor of string * int * pattern_cell option
      (* name, tag, optional sub-pattern for payload *)
  | PCPolyVariant of string * int * pattern_cell option
      (* tag name, hash tag, optional sub-pattern *)
  | PCBool of bool
  | PCInt of int
  | PCFloat of float
  | PCString of string
  | PCNil
  | PCCons of pattern_cell * pattern_cell
  | PCTuple of pattern_cell list
  | PCRecord of (string * int * pattern_cell) list  (* name, index, sub-pat *)
  | PCArray of pattern_cell list
  | PCMapHas of map_key * pattern_cell  (* key, value sub-pat *)
  | PCPin of string
  | PCOr of pattern_cell * binding list * pattern_cell * binding list

(* A row in the pattern matrix *)
type row = {
  pats: pattern_cell list;
  arm_idx: int;
  guard: Typechecker.texpr option;
  bindings: binding list;
}

(* ---- Helpers ---- *)

let tag_for_constructor type_env name =
  match List.assoc_opt name type_env.Types.constructors with
  | None -> failwith (Printf.sprintf "match_tree: unknown constructor: %s" name)
  | Some info ->
    let (_, _, variant_def, _) = List.find (fun (n, _, _, _) ->
      String.equal n info.ctor_type_name) type_env.Types.variants in
    let short_name = match String.rindex_opt name '.' with
      | Some i -> String.sub name (i + 1) (String.length name - i - 1)
      | None -> name
    in
    let rec find_tag i = function
      | [] -> failwith (Printf.sprintf "match_tree: constructor %s not found in type" name)
      | (cname, _) :: _ when cname = short_name -> i
      | _ :: rest -> find_tag (i + 1) rest
    in
    find_tag 0 variant_def

let is_newtype_ctor type_env name =
  match List.assoc_opt name type_env.Types.constructors with
  | Some info -> List.mem info.Types.ctor_type_name type_env.Types.newtypes
  | None -> false

let all_constructors_for type_env name =
  match List.assoc_opt name type_env.Types.constructors with
  | None -> None
  | Some info ->
    let type_name = info.Types.ctor_type_name in
    match List.find_opt (fun (n, _, _, _) -> String.equal n type_name) type_env.Types.variants with
    | Some (_, _, variant_def, _) -> Some variant_def
    | None -> None

let record_fields_of_ty type_env ty =
  ignore type_env;
  let ty = Types.repr ty in
  match ty with
  | Types.TRecord row ->
    let rec collect = function
      | Types.RRow (name, _fty, tail) -> name :: collect tail
      | Types.RVar { contents = Types.RLink row } -> collect row
      | _ -> []
    in
    let fields = collect row in
    List.mapi (fun i name -> (name, i)) fields
  | _ -> []

(* ---- Pattern normalization ---- *)

let rec normalize_pattern type_env occ occ_ty (pat : Ast.pattern) : pattern_cell * binding list =
  match pat with
  | Ast.PatWild | Ast.PatUnit -> (PCWild, [])
  | Ast.PatVar name ->
    (PCWild, [{ var_name = name; bind_occ = occ; bind_ty = occ_ty }])
  | Ast.PatInt n -> (PCInt n, [])
  | Ast.PatFloat f -> (PCFloat f, [])
  | Ast.PatBool b -> (PCBool b, [])
  | Ast.PatString s -> (PCString s, [])
  | Ast.PatNil -> (PCNil, [])
  | Ast.PatCons (hd, tl) ->
    let elem_ty = match Types.repr occ_ty with
      | Types.TList t -> t
      | _ -> occ_ty
    in
    let (hd_cell, hd_binds) = normalize_pattern type_env (occ @ [AConsHead]) elem_ty hd in
    let (tl_cell, tl_binds) = normalize_pattern type_env (occ @ [AConsTail]) occ_ty tl in
    (PCCons (hd_cell, tl_cell), hd_binds @ tl_binds)
  | Ast.PatTuple pats ->
    let elem_tys = match Types.repr occ_ty with
      | Types.TTuple ts -> ts
      | _ -> List.map (fun _ -> occ_ty) pats
    in
    let cells_binds = List.mapi (fun i pat ->
      let ety = if i < List.length elem_tys then List.nth elem_tys i else occ_ty in
      normalize_pattern type_env (occ @ [ATupleField i]) ety pat
    ) pats in
    let cells = List.map fst cells_binds in
    let binds = List.concat_map snd cells_binds in
    (PCTuple cells, binds)
  | Ast.PatRecord fields ->
    let field_index_map = record_fields_of_ty type_env occ_ty in
    let field_ty name = match Types.repr occ_ty with
      | Types.TRecord row ->
        let rec find = function
          | Types.RRow (n, ty, _) when n = name -> ty
          | Types.RRow (_, _, tail) -> find tail
          | Types.RVar { contents = Types.RLink row } -> find row
          | _ -> occ_ty
        in
        find row
      | _ -> occ_ty
    in
    let cells_binds = List.map (fun (name, pat) ->
      let idx = match List.assoc_opt name field_index_map with
        | Some i -> i | None -> 0
      in
      let fty = field_ty name in
      let (cell, binds) = normalize_pattern type_env (occ @ [ARecordField (name, idx)]) fty pat in
      ((name, idx, cell), binds)
    ) fields in
    let cells = List.map fst cells_binds in
    let binds = List.concat_map snd cells_binds in
    (PCRecord cells, binds)
  | Ast.PatArray pats ->
    let elem_ty = match Types.repr occ_ty with
      | Types.TArray t -> t
      | _ -> occ_ty
    in
    let cells_binds = List.mapi (fun i pat ->
      normalize_pattern type_env (occ @ [AArrayElem i]) elem_ty pat
    ) pats in
    let cells = List.map fst cells_binds in
    let binds = List.concat_map snd cells_binds in
    (PCArray cells, binds)
  | Ast.PatConstruct (name, arg) when is_newtype_ctor type_env name ->
    (match arg with
     | None -> (PCWild, [])
     | Some p ->
       let underlying_ty = match List.assoc_opt name type_env.Types.constructors with
         | Some info -> (match info.Types.ctor_arg_ty with Some t -> t | None -> occ_ty)
         | None -> occ_ty
       in
       normalize_pattern type_env occ underlying_ty p)
  | Ast.PatConstruct (name, arg) ->
    let tag = tag_for_constructor type_env name in
    let payload_ty = match List.assoc_opt name type_env.Types.constructors with
      | Some info -> (match info.Types.ctor_arg_ty with Some t -> t | None -> Types.TUnit)
      | None -> Types.TUnit
    in
    let (sub_cell, sub_binds) = match arg with
      | None -> (None, [])
      | Some p ->
        let (cell, binds) = normalize_pattern type_env (occ @ [AVariantPayload]) payload_ty p in
        (Some cell, binds)
    in
    (PCConstructor (name, tag, sub_cell), sub_binds)
  | Ast.PatPolyVariant (tag_name, arg) ->
    let hash_tag = Types.polyvar_tag tag_name in
    let payload_ty = match Types.repr occ_ty with
      | Types.TPolyVariant row ->
        let rec find = function
          | Types.PVRow (n, ty_opt, _) when n = tag_name ->
            (match ty_opt with Some t -> t | None -> Types.TUnit)
          | Types.PVRow (_, _, tail) -> find tail
          | Types.PVVar { contents = Types.PVLink row } -> find row
          | _ -> Types.TUnit
        in
        find row
      | _ -> Types.TUnit
    in
    let (sub_cell, sub_binds) = match arg with
      | None -> (None, [])
      | Some p ->
        let (cell, binds) = normalize_pattern type_env (occ @ [AVariantPayload]) payload_ty p in
        (Some cell, binds)
    in
    (PCPolyVariant (tag_name, hash_tag, sub_cell), sub_binds)
  | Ast.PatAs (inner, name) ->
    let (cell, binds) = normalize_pattern type_env occ occ_ty inner in
    let as_bind = { var_name = name; bind_occ = occ; bind_ty = occ_ty } in
    (cell, as_bind :: binds)
  | Ast.PatOr (p1, p2) ->
    let (cell1, binds1) = normalize_pattern type_env occ occ_ty p1 in
    let (cell2, binds2) = normalize_pattern type_env occ occ_ty p2 in
    (PCOr (cell1, binds1, cell2, binds2), [])
  | Ast.PatMap entries ->
    let rec build_map_cells = function
      | [] -> (PCWild, [])
      | (key_pat, val_pat) :: rest ->
        let mk = match key_pat with
          | Ast.PatInt n -> MKInt n
          | Ast.PatString s -> MKString s
          | Ast.PatBool b -> MKBool b
          | Ast.PatFloat f -> MKFloat f
          | Ast.PatPin name -> MKPin name
          | _ -> failwith "match_tree: map pattern key must be a literal or pin"
        in
        let val_ty = occ_ty in
        let val_occ = occ @ [AMapValue mk] in
        let (val_cell, val_binds) = normalize_pattern type_env val_occ val_ty val_pat in
        let (_rest_cell, rest_binds) = build_map_cells rest in
        (PCMapHas (mk, val_cell), val_binds @ rest_binds)
    in
    build_map_cells entries
  | Ast.PatPin name ->
    (PCPin name, [])
  | Ast.PatAnnot (inner, _) ->
    normalize_pattern type_env occ occ_ty inner

(* ---- Matrix operations ---- *)

let build_matrix type_env scrutinee_ty arms =
  List.mapi (fun arm_idx (pat, guard, _body) ->
    let (cell, bindings) = normalize_pattern type_env [] scrutinee_ty pat in
    { pats = [cell]; arm_idx; guard; bindings }
  ) arms

let rec expand_or_rows rows =
  let find_or_col pats =
    let rec go i = function
      | [] -> None
      | PCOr (p1, b1, p2, b2) :: _ -> Some (i, p1, b1, p2, b2)
      | _ :: rest -> go (i + 1) rest
    in
    go 0 pats
  in
  List.concat_map (fun row ->
    match find_or_col row.pats with
    | Some (i, p1, binds1, p2, binds2) ->
      let replace pats p = List.mapi (fun j c -> if j = i then p else c) pats in
      let row1 = { row with pats = replace row.pats p1; bindings = row.bindings @ binds1 } in
      let row2 = { row with pats = replace row.pats p2; bindings = row.bindings @ binds2 } in
      expand_or_rows [row1] @ expand_or_rows [row2]
    | None -> [row]
  ) rows

(* Head constructor classification *)
type head_ctor =
  | HCConstructor of string * int * int  (* name, tag, arity: 0 or 1 *)
  | HCPolyVariant of string * int * int
  | HCBool of bool
  | HCInt of int
  | HCFloat of float
  | HCString of string
  | HCNil
  | HCCons
  | HCArrayLen of int
  | HCTuple of int
  | HCRecord
  | HCMapHas of map_key
  | HCPin of string
  | HCWild

let head_of cell =
  match cell with
  | PCWild -> HCWild
  | PCConstructor (name, tag, arg) ->
    HCConstructor (name, tag, (if arg = None then 0 else 1))
  | PCPolyVariant (name, hash, arg) ->
    HCPolyVariant (name, hash, (if arg = None then 0 else 1))
  | PCBool b -> HCBool b
  | PCInt n -> HCInt n
  | PCFloat f -> HCFloat f
  | PCString s -> HCString s
  | PCNil -> HCNil
  | PCCons _ -> HCCons
  | PCTuple pats -> HCTuple (List.length pats)
  | PCRecord _ -> HCRecord
  | PCArray pats -> HCArrayLen (List.length pats)
  | PCMapHas (mk, _) -> HCMapHas mk
  | PCPin name -> HCPin name
  | PCOr _ -> HCWild  (* or-patterns are expanded before head extraction *)

let arity_of = function
  | HCConstructor (_, _, a) -> a
  | HCPolyVariant (_, _, a) -> a
  | HCBool _ | HCInt _ | HCFloat _ | HCString _ | HCNil | HCPin _ -> 0
  | HCCons -> 2
  | HCTuple n -> n
  | HCRecord -> 0
  | HCArrayLen n -> n
  | HCMapHas _ -> 1
  | HCWild -> 0

(* Specialize: given a column and a head constructor, produce the specialized matrix *)
let specialize_row col head row =
  let cell = List.nth row.pats col in
  let h = head_of cell in
  let arity = arity_of head in
  let remove_col pats = List.filteri (fun i _ -> i <> col) pats in
  let wild_subs = List.init arity (fun _ -> PCWild) in
  match h with
  | HCWild ->
    Some { row with pats = wild_subs @ (remove_col row.pats) }
  | _ when h = head ->
    let sub_pats = match cell with
      | PCConstructor (_, _, Some p) -> [p]
      | PCConstructor (_, _, None) -> []
      | PCPolyVariant (_, _, Some p) -> [p]
      | PCPolyVariant (_, _, None) -> []
      | PCCons (hd, tl) -> [hd; tl]
      | PCTuple pats -> pats
      | PCArray pats -> pats
      | PCMapHas (_, vp) -> [vp]
      | _ -> []
    in
    Some { row with pats = sub_pats @ (remove_col row.pats) }
  | _ ->
    None

let specialize col head rows =
  List.filter_map (specialize_row col head) rows

let default_matrix col rows =
  List.filter_map (fun row ->
    let cell = List.nth row.pats col in
    match head_of cell with
    | HCWild -> Some { row with pats = List.filteri (fun i _ -> i <> col) row.pats }
    | _ -> None
  ) rows

let distinct_heads col rows =
  let heads = List.filter_map (fun row ->
    let cell = List.nth row.pats col in
    match head_of cell with
    | HCWild -> None
    | h -> Some h
  ) rows in
  List.sort_uniq compare heads

(* Record specialization *)
let specialize_record col fields row =
  let cell = List.nth row.pats col in
  let remove_col pats = List.filteri (fun i _ -> i <> col) pats in
  match cell with
  | PCRecord field_pats ->
    let sub_pats = List.map (fun (name, _, _) ->
      match List.find_opt (fun (n, _, _) -> n = name) field_pats with
      | Some (_, _, p) -> p
      | None -> PCWild
    ) fields in
    Some { row with pats = sub_pats @ (remove_col row.pats) }
  | PCWild ->
    let wild_subs = List.init (List.length fields) (fun _ -> PCWild) in
    Some { row with pats = wild_subs @ (remove_col row.pats) }
  | _ -> None

(* ---- Column selection heuristic ---- *)

let select_column rows =
  if rows = [] then 0
  else
    let num_cols = List.length (List.hd rows).pats in
    if num_cols <= 1 then 0
    else
      let score col =
        let heads = distinct_heads col rows in
        let num_heads = List.length heads in
        let num_wilds = List.length (List.filter (fun row ->
          head_of (List.nth row.pats col) = HCWild
        ) rows) in
        (num_heads * 1000) - num_wilds
      in
      let best = ref 0 in
      let best_score = ref (score 0) in
      for col = 1 to num_cols - 1 do
        let s = score col in
        if s > !best_score then begin
          best := col;
          best_score := s
        end
      done;
      !best

(* ---- Occurrence tracking ---- *)

type col_info = {
  col_occ: occurrence;
  col_ty: Types.ty;
}

let expand_col_info type_env col_infos col head =
  let parent = List.nth col_infos col in
  let remove_col lst = List.filteri (fun i _ -> i <> col) lst in
  let new_cols = match head with
    | HCConstructor (name, _, arity) ->
      if arity = 0 then []
      else
        let payload_ty = match List.assoc_opt name type_env.Types.constructors with
          | Some info -> (match info.Types.ctor_arg_ty with Some t -> t | None -> Types.TUnit)
          | None -> Types.TUnit
        in
        [{ col_occ = parent.col_occ @ [AVariantPayload]; col_ty = payload_ty }]
    | HCPolyVariant (tag_name, _, arity) ->
      if arity = 0 then []
      else
        let payload_ty = match Types.repr parent.col_ty with
          | Types.TPolyVariant row ->
            let rec find = function
              | Types.PVRow (n, ty_opt, _) when n = tag_name ->
                (match ty_opt with Some t -> t | None -> Types.TUnit)
              | Types.PVRow (_, _, tail) -> find tail
              | Types.PVVar { contents = Types.PVLink row } -> find row
              | _ -> Types.TUnit
            in
            find row
          | _ -> Types.TUnit
        in
        [{ col_occ = parent.col_occ @ [AVariantPayload]; col_ty = payload_ty }]
    | HCCons ->
      let elem_ty = match Types.repr parent.col_ty with
        | Types.TList t -> t
        | _ -> parent.col_ty
      in
      [{ col_occ = parent.col_occ @ [AConsHead]; col_ty = elem_ty };
       { col_occ = parent.col_occ @ [AConsTail]; col_ty = parent.col_ty }]
    | HCTuple n ->
      let elem_tys = match Types.repr parent.col_ty with
        | Types.TTuple ts -> ts
        | _ -> List.init n (fun _ -> parent.col_ty)
      in
      List.mapi (fun i _ ->
        let ty = if i < List.length elem_tys then List.nth elem_tys i else parent.col_ty in
        { col_occ = parent.col_occ @ [ATupleField i]; col_ty = ty }
      ) (List.init n (fun _ -> ()))
    | HCArrayLen n ->
      let elem_ty = match Types.repr parent.col_ty with
        | Types.TArray t -> t
        | _ -> parent.col_ty
      in
      List.init n (fun i ->
        { col_occ = parent.col_occ @ [AArrayElem i]; col_ty = elem_ty }
      )
    | HCMapHas mk ->
      let val_ty = parent.col_ty in  (* best approximation; map value type *)
      [{ col_occ = parent.col_occ @ [AMapValue mk]; col_ty = val_ty }]
    | HCBool _ | HCInt _ | HCFloat _ | HCString _ | HCNil | HCPin _ -> []
    | HCWild -> []
    | HCRecord -> []
  in
  new_cols @ (remove_col col_infos)

let expand_col_info_record col_infos col fields parent_ty _type_env =
  let parent = List.nth col_infos col in
  let remove_col lst = List.filteri (fun i _ -> i <> col) lst in
  let field_ty name = match Types.repr parent_ty with
    | Types.TRecord row ->
      let rec find = function
        | Types.RRow (n, ty, _) when n = name -> ty
        | Types.RRow (_, _, tail) -> find tail
        | Types.RVar { contents = Types.RLink row } -> find row
        | _ -> parent_ty
      in
      find row
    | _ -> parent_ty
  in
  let new_cols = List.map (fun (name, idx, _) ->
    { col_occ = parent.col_occ @ [ARecordField (name, idx)];
      col_ty = field_ty name }
  ) fields in
  new_cols @ (remove_col col_infos)

(* ---- Check if all constructors are covered ---- *)

let is_complete_signature type_env heads occ_ty =
  match heads with
  | [] -> false
  | (HCBool _) :: _ ->
    List.exists (fun h -> h = HCBool true) heads &&
    List.exists (fun h -> h = HCBool false) heads
  | (HCNil | HCCons) :: _ ->
    List.exists (fun h -> h = HCNil) heads &&
    List.exists (fun h -> h = HCCons) heads
  | (HCConstructor (name, _, _)) :: _ ->
    (match all_constructors_for type_env name with
     | None -> false
     | Some variant_def ->
       List.for_all (fun (ctor_name, _) ->
         List.exists (fun h -> match h with
           | HCConstructor (n, _, _) ->
             let short = match String.rindex_opt n '.' with
               | Some i -> String.sub n (i + 1) (String.length n - i - 1)
               | None -> n
             in
             short = ctor_name
           | _ -> false
         ) heads
       ) variant_def)
  | (HCTuple _) :: _ -> true
  | (HCRecord) :: _ -> true
  | _ ->
    ignore occ_ty;
    false

(* ---- Head constructor to test conversion ---- *)

let head_to_test = function
  | HCConstructor (name, tag, _) -> TConstructor (name, tag)
  | HCPolyVariant (name, hash, _) -> TPolyVariant (name, hash)
  | HCBool b -> TBoolLit b
  | HCInt n -> TIntLit n
  | HCFloat f -> TFloatLit f
  | HCString s -> TStringLit s
  | HCNil -> TNil
  | HCCons -> TCons
  | HCArrayLen n -> TArrayLen n
  | HCMapHas mk -> TMapHasKey mk
  | HCPin name -> TPin name
  | HCTuple _ | HCRecord | HCWild -> failwith "match_tree: no test for irrefutable/wild"

(* ---- Core algorithm ---- *)

let rec compile type_env col_infos rows loc =
  let rows = expand_or_rows rows in
  match rows with
  | [] ->
    DFail loc
  | first_row :: _ ->
    let all_wild = List.for_all (fun p -> head_of p = HCWild) first_row.pats in
    if all_wild || first_row.pats = [] then
      match first_row.guard with
      | None ->
        DLeaf { arm_idx = first_row.arm_idx; bindings = first_row.bindings }
      | Some guard ->
        let remaining = List.tl rows in
        let on_false = compile type_env col_infos remaining loc in
        DGuard {
          arm_idx = first_row.arm_idx;
          guard;
          bindings = first_row.bindings;
          on_true = DLeaf { arm_idx = first_row.arm_idx; bindings = first_row.bindings };
          on_false;
        }
    else begin
      let col = select_column rows in
      let col_info = List.nth col_infos col in

      (* Check for record patterns — special handling *)
      let has_record = List.exists (fun row ->
        match List.nth row.pats col with PCRecord _ -> true | _ -> false
      ) rows in

      if has_record then
        compile_record type_env col_infos rows col loc
      else

      let heads = distinct_heads col rows in
      let has_tuple = List.exists (fun h -> match h with HCTuple _ -> true | _ -> false) heads in

      if has_tuple && List.length heads = 1 then begin
        (* Irrefutable tuple: expand without testing *)
        let head = List.hd heads in
        let specialized = specialize col head rows in
        let new_col_infos = expand_col_info type_env col_infos col head in
        compile type_env new_col_infos specialized loc
      end
      else begin
        let complete = is_complete_signature type_env heads col_info.col_ty in

        let cases = List.map (fun head ->
          let specialized = specialize col head rows in
          let new_col_infos = expand_col_info type_env col_infos col head in
          let sub_tree = compile type_env new_col_infos specialized loc in
          let test = head_to_test head in
          (test, [], sub_tree)
        ) heads in

        let default =
          if complete then None
          else
            let def_rows = default_matrix col rows in
            if def_rows = [] then Some (DFail loc)
            else Some (compile type_env (List.filteri (fun i _ -> i <> col) col_infos) def_rows loc)
        in

        DSwitch {
          occ = col_info.col_occ;
          occ_ty = col_info.col_ty;
          cases;
          default;
        }
      end
    end

and compile_record type_env col_infos rows col loc =
  let fields = List.find_map (fun row ->
    match List.nth row.pats col with
    | PCRecord fields -> Some fields
    | _ -> None
  ) rows in
  match fields with
  | None ->
    compile type_env col_infos rows loc
  | Some fields ->
    let parent_ty = (List.nth col_infos col).col_ty in
    let specialized = List.filter_map (specialize_record col fields) rows in
    let new_col_infos = expand_col_info_record col_infos col fields parent_ty type_env in
    compile type_env new_col_infos specialized loc

(* ---- Entry point ---- *)

let compile_match type_env (scrutinee : Typechecker.texpr) arms match_kind loc =
  let scrutinee_ty = scrutinee.ty in
  let matrix = build_matrix type_env scrutinee_ty arms in
  let col_infos = [{ col_occ = []; col_ty = scrutinee_ty }] in
  let tree = compile type_env col_infos matrix loc in
  let match_arms = Array.of_list (List.map (fun (_, _, body) ->
    { arm_body = body }
  ) arms) in
  {
    scrutinee;
    scrutinee_ty;
    match_arms;
    tree;
    loc;
    match_kind;
  }

(* ---- AST lowering pass ---- *)

let rec lower_texpr type_env (te : Typechecker.texpr) : Typechecker.texpr =
  match te.expr with
  | Typechecker.TEMatch (scrutinee, arms, match_kind) ->
    let scrutinee' = lower_texpr type_env scrutinee in
    let arms' = List.map (fun (pat, guard, body) ->
      (pat, Option.map (lower_texpr type_env) guard, lower_texpr type_env body)
    ) arms in
    let cm = compile_match type_env scrutinee' arms' match_kind te.loc in
    { te with expr = Typechecker.TEMatchTree cm }
  | _ ->
    let expr' = Typechecker.map_texpr_children (lower_texpr type_env) te in
    { te with expr = expr' }

let rec lower_program type_env (program : Typechecker.tprogram) : Typechecker.tprogram =
  List.map (fun decl -> match decl with
    | Typechecker.TDLet (n, e) -> Typechecker.TDLet (n, lower_texpr type_env e)
    | Typechecker.TDLetMut (n, e) -> Typechecker.TDLetMut (n, lower_texpr type_env e)
    | Typechecker.TDLetRec (n, e) -> Typechecker.TDLetRec (n, lower_texpr type_env e)
    | Typechecker.TDExpr e -> Typechecker.TDExpr (lower_texpr type_env e)
    | Typechecker.TDLetRecAnd binds ->
      Typechecker.TDLetRecAnd (List.map (fun (n, e) -> (n, lower_texpr type_env e)) binds)
    | Typechecker.TDModule (name, decls, schemes) ->
      Typechecker.TDModule (name, lower_program type_env decls, schemes)
    | Typechecker.TDEffect _ | Typechecker.TDType _
    | Typechecker.TDClass _ | Typechecker.TDExtern _
    | Typechecker.TDOpen _ -> decl
  ) program
