(* Typed AST optimization pass.

   Runs after typechecking and match tree lowering, before compilation.

   Optimizations:
   1. Dead let elimination — remove unused let bindings
   2. Single-use let inlining — substitute bindings used exactly once
   3. Constant folding — evaluate constant expressions at compile time

   All transformations respect variable shadowing in binding forms:
   TELet, TELetRec, TEFun, TELetMut, TELetRecAnd, TEMatch, TEMatchTree, TEHandle. *)

open Typechecker

(* Name-sets here are small, scope-local (mutable-var names, pure-fn names) and
   used PERSISTENTLY — `ss_add` returns a NEW set, so adding a name for a child
   scope never disturbs the parent's set. They are plain immutable string lists
   rather than Set.Make(String): this file is shared with the self-hosted
   compiler via tools/ocaml_to_mml, whose translator has no functor support, and
   the rest of the translatable compiler likewise avoids functors (roadmap #3b).
   The lists are tiny, so the O(n) membership cost is immaterial. *)
let ss_empty : string list = []
let ss_mem (x : string) (s : string list) = List.mem x s
let ss_add (x : string) (s : string list) = if List.mem x s then s else x :: s
let ss_union (a : string list) (b : string list) =
  List.fold_left (fun acc x -> ss_add x acc) a b

(* === Purity analysis === *)

(* An expression is "pure" if evaluating it has no observable side effects
   and cannot diverge. Pure expressions can safely be reordered or deleted. *)
(* Check if all top-level arrows in a scheme body have EffEmpty,
   and the final return type is not unit. Unit-returning externs are
   almost always called for side effects (mutation, IO) even when the
   effect system doesn't track it (e.g. Array.set uses local mutation). *)
let rec all_arrows_pure ty =
  match Types.repr ty with
  | Types.TArrow (_, eff, ret) -> (
      match Types.eff_repr eff with
      | Types.EffEmpty -> all_arrows_pure ret
      | _ -> false)
  | Types.TUnit -> false
  | _ -> true

(* Collect names of pure extern functions from the typed program *)
let collect_pure_externs program =
  let set = ref ss_empty in
  let check name (scheme : Types.scheme) =
    if all_arrows_pure scheme.Types.body then set := ss_add name !set
  in
  List.iter
    (fun decl ->
      match decl with
      | TDExtern (name, scheme) -> check name scheme
      | TDModule (_, decls, _) ->
          List.iter
            (fun d ->
              match d with
              | TDExtern (name, scheme) -> check name scheme
              | _ -> ())
            decls
      | _ -> ())
    program;
  !set

let rec is_pure ~mutables ~pure_fns te =
  match te.expr with
  | TEInt _ | TEFloat _ | TEBool _ | TEString _ | TEByte _ | TERune _ | TEUnit
  | TENil | TEContinueLoop ->
      true
  | TEVar name -> not (ss_mem name mutables)
  | TEFun _ -> true
  | TETuple es | TEArray es -> List.for_all (is_pure ~mutables ~pure_fns) es
  | TERecord fields ->
      List.for_all (fun (_, e) -> is_pure ~mutables ~pure_fns e) fields
  | TERecordUpdate (base, fields) ->
      is_pure ~mutables ~pure_fns base
      && List.for_all (fun (_, e) -> is_pure ~mutables ~pure_fns e) fields
  | TECons (h, t) ->
      is_pure ~mutables ~pure_fns h && is_pure ~mutables ~pure_fns t
  | TEConstruct (_, None) -> true
  | TEConstruct (_, Some e) -> is_pure ~mutables ~pure_fns e
  (* A record field read is NOT pure for inlining purposes: the field may be
     mutable, so its value can change if the record is mutated between the
     binding site and the use site. Treating it as pure let the single-use-let
     inliner move the read across a mutation (e.g. `let saved = s.f in s.f := x;
     ... saved ...`), reading the already-updated value. This mirrors the
     conservative treatment of function application below. *)
  | TEField (_, _) -> false
  | TEBinop (op, l, r) -> (
      (* Division/modulo can raise *)
      match op with
      | Ast.Div | Ast.Mod -> false
      | _ -> is_pure ~mutables ~pure_fns l && is_pure ~mutables ~pure_fns r)
  | TEUnop (_, e) -> is_pure ~mutables ~pure_fns e
  | TEApp _ -> (
      (* Only treat calls to known-pure externs as pure. MiniML-defined functions
       may have hidden mutation (mutable record fields) not tracked by the
       effect system, so we conservatively treat them as impure.
       Walk the curried application spine to find the head variable. *)
      let rec get_head e =
        match e.expr with TEApp (f, _) -> get_head f | _ -> e
      in
      let head = get_head te in
      match head.expr with
      | TEVar name when ss_mem name pure_fns ->
          let rec all_args_pure e =
            match e.expr with
            | TEApp (f, arg) ->
                is_pure ~mutables ~pure_fns arg && all_args_pure f
            | _ -> is_pure ~mutables ~pure_fns e
          in
          all_args_pure te
      | _ -> false)
  | _ -> false

(* === Pattern binding check === *)

let rec pattern_binds name = function
  | Ast.PatVar n -> String.equal n name
  | Ast.PatAs (p, n) -> String.equal n name || pattern_binds name p
  | Ast.PatTuple ps | Ast.PatArray ps -> List.exists (pattern_binds name) ps
  | Ast.PatCons (h, t) -> pattern_binds name h || pattern_binds name t
  | Ast.PatConstruct (_, Some p)
  | Ast.PatPolyVariant (_, Some p)
  | Ast.PatAnnot (p, _) ->
      pattern_binds name p
  | Ast.PatRecord fields ->
      List.exists (fun (_, p) -> pattern_binds name p) fields
  | Ast.PatOr (p1, p2) -> pattern_binds name p1 || pattern_binds name p2
  | Ast.PatMap entries ->
      List.exists
        (fun (k, v) -> pattern_binds name k || pattern_binds name v)
        entries
  | Ast.PatSet elems -> List.exists (pattern_binds name) elems
  | _ -> false

(* === Match tree binding name collection === *)

(* Collect all variable names bound anywhere in a decision tree.
   Used conservatively: if our target name appears in ANY tree binding,
   we skip the arm bodies (overcounting is safe, undercounting is not). *)
let collect_tree_binding_names tree =
  let names = ref ss_empty in
  let add binds =
    List.iter
      (fun b -> names := ss_add b.Match_tree_types.var_name !names)
      binds
  in
  let rec walk = function
    | Match_tree_types.DSwitch { cases; default; _ } ->
        List.iter
          (fun (_, binds, sub) ->
            add binds;
            walk sub)
          cases;
        Option.iter walk default
    | Match_tree_types.DGuard { bindings; on_true; on_false; _ } ->
        add bindings;
        walk on_true;
        walk on_false
    | Match_tree_types.DLeaf { bindings; _ } -> add bindings
    | Match_tree_types.DFail _ -> ()
  in
  walk tree;
  !names

(* === Pin reference counting in match trees === *)

(* Count references to [name] via TPin tests and MKPin map keys in a tree *)
let count_pin_refs name tree =
  let n = ref 0 in
  let check_map_key = function
    | Match_tree_types.MKPin s when String.equal s name -> incr n
    | _ -> ()
  in
  let check_test = function
    | Match_tree_types.TPin s when String.equal s name -> incr n
    | Match_tree_types.TMapHasKey mk -> check_map_key mk
    | _ -> ()
  in
  let check_access = function
    | Match_tree_types.AMapValue mk -> check_map_key mk
    | _ -> ()
  in
  let check_occ occ = List.iter check_access occ in
  let check_binding b = check_occ b.Match_tree_types.bind_occ in
  let rec walk = function
    | Match_tree_types.DSwitch { occ; cases; default; _ } ->
        check_occ occ;
        List.iter
          (fun (test, binds, sub) ->
            check_test test;
            List.iter check_binding binds;
            walk sub)
          cases;
        Option.iter walk default
    | Match_tree_types.DGuard { bindings; on_true; on_false; _ } ->
        List.iter check_binding bindings;
        walk on_true;
        walk on_false
    | Match_tree_types.DLeaf { bindings; _ } -> List.iter check_binding bindings
    | Match_tree_types.DFail _ -> ()
  in
  walk tree;
  !n

(* Check if a variable is referenced by name in pin patterns or map key pins
   anywhere in an expression. Such variables must remain as named bindings
   because pins reference them at runtime by name, not by value. *)
let has_pin_reference name te =
  let found = ref false in
  let rec go te =
    if !found then ()
    else
      match te.expr with
      | TEMatchTree cm ->
          if count_pin_refs name cm.tree > 0 then found := true
          else iter_texpr_children go te
      | _ -> iter_texpr_children go te
  in
  go te;
  !found

(* === Free variable use counting === *)

(* Count free occurrences of [name] in [te], correctly handling shadowing.
   Stops descending into scopes that re-bind [name]. *)
let count_free_uses name te =
  let n = ref 0 in
  let rec go te =
    match te.expr with
    | TEVar v when String.equal v name -> incr n
    | TELet (bind_name, _, rhs, body) ->
        go rhs;
        if not (String.equal bind_name name) then go body
    | TELetRec (bind_name, _, rhs, body) ->
        (* letrec: name is in scope for both rhs and body *)
        if not (String.equal bind_name name) then (
          go rhs;
          go body)
    | TEFun (param, body, _) -> if not (String.equal param name) then go body
    | TELetMut (bind_name, rhs, body) ->
        go rhs;
        if not (String.equal bind_name name) then go body
    | TELetRecAnd (binds, body) ->
        let bound = List.map fst binds in
        if not (List.mem name bound) then begin
          List.iter (fun (_, e) -> go e) binds;
          go body
        end
    | TEMatch (scrut, arms, _) ->
        go scrut;
        List.iter
          (fun (pat, guard, body) ->
            if not (pattern_binds name pat) then begin
              Option.iter go guard;
              go body
            end)
          arms
    | TEMatchTree cm ->
        go cm.Match_tree_types.scrutinee;
        let tree_names = collect_tree_binding_names cm.tree in
        if not (ss_mem name tree_names) then begin
          (* Name not shadowed by any match binding — safe to walk everything *)
          Array.iter (fun arm -> go arm.Match_tree_types.arm_body) cm.match_arms;
          let rec walk_guards = function
            | Match_tree_types.DGuard { guard; on_true; on_false; _ } ->
                go guard;
                walk_guards on_true;
                walk_guards on_false
            | Match_tree_types.DSwitch { cases; default; _ } ->
                List.iter (fun (_, _, sub) -> walk_guards sub) cases;
                Option.iter walk_guards default
            | _ -> ()
          in
          walk_guards cm.tree
        end
        (* If name IS in tree_names, conservatively assume it might be used
         (overcounting is safe — prevents inlining of a possibly-shadowed binding) *)
    | TEHandle (body, arms) ->
        go body;
        List.iter
          (fun arm ->
            match arm with
            | THReturn (n_bind, b) ->
                if not (String.equal n_bind name) then go b
            | THOp { arg; k; body; _ } ->
                if (not (String.equal arg name)) && not (String.equal k name)
                then go body
            | THOpProvide (_, x, b) | THOpTry (_, x, b) ->
                if not (String.equal x name) then go b)
          arms
    | _ -> iter_texpr_children go te
  in
  go te;
  !n

(* === Substitution === *)

(* Substitute free occurrences of [name] with [repl] in [te].
   Respects shadowing: stops at binders that re-bind [name]. *)
let subst name repl te =
  let rec go te =
    match te.expr with
    | TEVar v when String.equal v name -> repl
    | TELet (bind_name, scheme, rhs, body) ->
        let rhs' = go rhs in
        if String.equal bind_name name then
          { te with expr = TELet (bind_name, scheme, rhs', body) }
        else { te with expr = TELet (bind_name, scheme, rhs', go body) }
    | TELetRec (bind_name, scheme, rhs, body) ->
        if String.equal bind_name name then te
        else { te with expr = TELetRec (bind_name, scheme, go rhs, go body) }
    | TEFun (param, body, hr) ->
        if String.equal param name then te
        else { te with expr = TEFun (param, go body, hr) }
    | TELetMut (bind_name, rhs, body) ->
        let rhs' = go rhs in
        if String.equal bind_name name then
          { te with expr = TELetMut (bind_name, rhs', body) }
        else { te with expr = TELetMut (bind_name, rhs', go body) }
    | TELetRecAnd (binds, body) ->
        let bound = List.map fst binds in
        if List.mem name bound then te
        else
          {
            te with
            expr =
              TELetRecAnd (List.map (fun (n, e) -> (n, go e)) binds, go body);
          }
    | TEMatch (scrut, arms, mk) ->
        let scrut' = go scrut in
        let arms' =
          List.map
            (fun (pat, guard, body) ->
              if pattern_binds name pat then (pat, guard, body)
              else (pat, Option.map go guard, go body))
            arms
        in
        { te with expr = TEMatch (scrut', arms', mk) }
    | TEMatchTree cm ->
        let scrutinee' = go cm.Match_tree_types.scrutinee in
        let tree_names = collect_tree_binding_names cm.tree in
        if ss_mem name tree_names then
          (* Name shadowed by match binding — only substitute in scrutinee *)
          {
            te with
            expr =
              TEMatchTree { cm with Match_tree_types.scrutinee = scrutinee' };
          }
        else begin
          (* Safe to substitute everywhere *)
          let match_arms' =
            Array.map
              (fun arm ->
                { Match_tree_types.arm_body = go arm.Match_tree_types.arm_body })
              cm.match_arms
          in
          let rec map_guards = function
            | Match_tree_types.DGuard g ->
                Match_tree_types.DGuard
                  {
                    g with
                    guard = go g.guard;
                    on_true = map_guards g.on_true;
                    on_false = map_guards g.on_false;
                  }
            | Match_tree_types.DSwitch sw ->
                Match_tree_types.DSwitch
                  {
                    sw with
                    cases =
                      List.map
                        (fun (t, b, sub) -> (t, b, map_guards sub))
                        sw.cases;
                    default = Option.map map_guards sw.default;
                  }
            | other -> other
          in
          {
            te with
            expr =
              TEMatchTree
                {
                  cm with
                  scrutinee = scrutinee';
                  match_arms = match_arms';
                  tree = map_guards cm.tree;
                };
          }
        end
    | TEHandle (body, arms) ->
        let body' = go body in
        let arms' =
          List.map
            (fun arm ->
              match arm with
              | THReturn (n_bind, b) ->
                  if String.equal n_bind name then arm
                  else THReturn (n_bind, go b)
              | THOp { op_name; arg; k; body } ->
                  if String.equal arg name || String.equal k name then arm
                  else THOp { op_name; arg; k; body = go body }
              | THOpProvide (eff, x, b) ->
                  if String.equal x name then arm else THOpProvide (eff, x, go b)
              | THOpTry (eff, x, b) ->
                  if String.equal x name then arm else THOpTry (eff, x, go b))
            arms
        in
        { te with expr = TEHandle (body', arms') }
    | _ -> { te with expr = map_texpr_children go te }
  in
  go te

(* === Constant folding === *)

let try_const_fold ~pure_fns te =
  match te.expr with
  (* Integer arithmetic *)
  | TEBinop (Ast.Add, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEInt (a + b) }
  | TEBinop (Ast.Sub, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEInt (a - b) }
  | TEBinop (Ast.Mul, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEInt (a * b) }
  | TEBinop (Ast.Div, { expr = TEInt a; _ }, { expr = TEInt b; _ }) when b <> 0
    ->
      { te with expr = TEInt (a / b) }
  | TEBinop (Ast.Mod, { expr = TEInt a; _ }, { expr = TEInt b; _ }) when b <> 0
    ->
      { te with expr = TEInt (a mod b) }
  (* Float arithmetic *)
  | TEBinop (Ast.Add, { expr = TEFloat a; _ }, { expr = TEFloat b; _ }) ->
      { te with expr = TEFloat (a +. b) }
  | TEBinop (Ast.Sub, { expr = TEFloat a; _ }, { expr = TEFloat b; _ }) ->
      { te with expr = TEFloat (a -. b) }
  | TEBinop (Ast.Mul, { expr = TEFloat a; _ }, { expr = TEFloat b; _ }) ->
      { te with expr = TEFloat (a *. b) }
  | TEBinop (Ast.Div, { expr = TEFloat a; _ }, { expr = TEFloat b; _ })
    when b <> 0.0 ->
      { te with expr = TEFloat (a /. b) }
  (* Integer comparison *)
  | TEBinop (Ast.Eq, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEBool (a = b) }
  | TEBinop (Ast.Neq, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEBool (a <> b) }
  | TEBinop (Ast.Lt, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEBool (a < b) }
  | TEBinop (Ast.Gt, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEBool (a > b) }
  | TEBinop (Ast.Le, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEBool (a <= b) }
  | TEBinop (Ast.Ge, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEBool (a >= b) }
  (* Boolean operations *)
  | TEBinop (Ast.And, { expr = TEBool a; _ }, { expr = TEBool b; _ }) ->
      { te with expr = TEBool (a && b) }
  | TEBinop (Ast.Or, { expr = TEBool a; _ }, { expr = TEBool b; _ }) ->
      { te with expr = TEBool (a || b) }
  | TEBinop (Ast.Eq, { expr = TEBool a; _ }, { expr = TEBool b; _ }) ->
      { te with expr = TEBool (a = b) }
  | TEBinop (Ast.Neq, { expr = TEBool a; _ }, { expr = TEBool b; _ }) ->
      { te with expr = TEBool (a <> b) }
  (* String operations *)
  | TEBinop (Ast.Concat, { expr = TEString a; _ }, { expr = TEString b; _ }) ->
      { te with expr = TEString (a ^ b) }
  | TEBinop (Ast.Eq, { expr = TEString a; _ }, { expr = TEString b; _ }) ->
      { te with expr = TEBool (String.equal a b) }
  | TEBinop (Ast.Neq, { expr = TEString a; _ }, { expr = TEString b; _ }) ->
      { te with expr = TEBool (not (String.equal a b)) }
  (* Unary operations *)
  | TEUnop (Ast.Not, { expr = TEBool b; _ }) ->
      { te with expr = TEBool (not b) }
  | TEUnop (Ast.Neg, { expr = TEInt n; _ }) -> { te with expr = TEInt (-n) }
  | TEUnop (Ast.Neg, { expr = TEFloat f; _ }) ->
      { te with expr = TEFloat (-.f) }
  | TEUnop (Ast.Lnot, { expr = TEInt n; _ }) ->
      { te with expr = TEInt (lnot n) }
  (* Bitwise operations *)
  | TEBinop (Ast.Land, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEInt (a land b) }
  | TEBinop (Ast.Lor, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEInt (a lor b) }
  | TEBinop (Ast.Lxor, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEInt (a lxor b) }
  | TEBinop (Ast.Lsl, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEInt (a lsl b) }
  | TEBinop (Ast.Lsr, { expr = TEInt a; _ }, { expr = TEInt b; _ }) ->
      { te with expr = TEInt (a lsr b) }
  (* Constant-condition if *)
  | TEIf ({ expr = TEBool true; _ }, then_branch, _) -> then_branch
  | TEIf ({ expr = TEBool false; _ }, _, else_branch) -> else_branch
  (* Identity operations *)
  | TEBinop (Ast.Add, e, { expr = TEInt 0; _ })
  | TEBinop (Ast.Add, { expr = TEInt 0; _ }, e) ->
      e
  | TEBinop (Ast.Sub, e, { expr = TEInt 0; _ }) -> e
  | TEBinop (Ast.Mul, e, { expr = TEInt 1; _ })
  | TEBinop (Ast.Mul, { expr = TEInt 1; _ }, e) ->
      e
  | TEBinop (Ast.Concat, e, { expr = TEString ""; _ })
  | TEBinop (Ast.Concat, { expr = TEString ""; _ }, e) ->
      e
  (* Boolean short-circuit with known values *)
  | TEBinop (Ast.And, ({ expr = TEBool false; _ } as e), _) -> e
  | TEBinop (Ast.And, { expr = TEBool true; _ }, e) -> e
  | TEBinop (Ast.Or, ({ expr = TEBool true; _ } as e), _) -> e
  | TEBinop (Ast.Or, { expr = TEBool false; _ }, e) -> e
  (* Multiply by zero — only if other side is pure *)
  | TEBinop (Ast.Mul, ({ expr = TEInt 0; _ } as zero), other)
    when is_pure ~mutables:ss_empty ~pure_fns other ->
      zero
  | TEBinop (Ast.Mul, other, ({ expr = TEInt 0; _ } as zero))
    when is_pure ~mutables:ss_empty ~pure_fns other ->
      zero
  (* No fold possible *)
  | _ -> te

(* === Main optimization pass === *)

(* Bottom-up optimization: optimize children first, then apply transforms
   at the current node. Re-optimizes after each successful transform since
   inlining/elimination can expose new opportunities. *)
let rec optimize_expr ~mutables ~pure_fns te =
  (* First optimize children (bottom-up).
     Special case: TELetMut extends the mutable set for its body. *)
  let te =
    match te.expr with
    | TELetMut (name, rhs, body) ->
        let rhs' = optimize_expr ~mutables ~pure_fns rhs in
        let body' =
          optimize_expr ~mutables:(ss_add name mutables) ~pure_fns body
        in
        { te with expr = TELetMut (name, rhs', body') }
    | _ ->
        {
          te with
          expr = map_texpr_children (optimize_expr ~mutables ~pure_fns) te;
        }
  in
  (* Now try optimizations at this node *)
  match te.expr with
  | TELet (name, _scheme, rhs, body) ->
      let uses = count_free_uses name body in
      if uses = 0 && not (has_pin_reference name body) then begin
        (* Dead let: drop the binding *)
        if is_pure ~mutables ~pure_fns rhs then
          optimize_expr ~mutables ~pure_fns body
        else
          (* Keep rhs for its side effects *)
          optimize_expr ~mutables ~pure_fns { te with expr = TESeq (rhs, body) }
      end
      else if uses = 1 && is_pure ~mutables ~pure_fns rhs then
        (* Single-use inline: substitute rhs for the single occurrence.
         But only if the variable isn't referenced by name in pin patterns
         or map key pins — those reference the variable by name at runtime
         and can't be substituted. *)
        if not (has_pin_reference name body) then
          optimize_expr ~mutables ~pure_fns (subst name rhs body)
        else try_const_fold ~pure_fns te
      else try_const_fold ~pure_fns te
  | _ -> try_const_fold ~pure_fns te

(* === Top-level declaration optimization === *)

let rec optimize_decl ~mutables ~pure_fns decl =
  match decl with
  | TDLet (n, e) -> TDLet (n, optimize_expr ~mutables ~pure_fns e)
  | TDLetMut (n, e) -> TDLetMut (n, optimize_expr ~mutables ~pure_fns e)
  | TDLetRec (n, e) -> TDLetRec (n, optimize_expr ~mutables ~pure_fns e)
  | TDExpr e -> TDExpr (optimize_expr ~mutables ~pure_fns e)
  | TDLetRecAnd binds ->
      TDLetRecAnd
        (List.map
           (fun (n, e) -> (n, optimize_expr ~mutables ~pure_fns e))
           binds)
  | TDModule (name, decls, schemes) ->
      TDModule
        (name, List.map (optimize_decl ~mutables ~pure_fns) decls, schemes)
  | (TDType _ | TDClass _ | TDEffect _ | TDExtern _ | TDOpen _) as d -> d

let optimize_program ?(stdlib_programs = []) (program : tprogram) : tprogram =
  (* Collect all top-level mutable names (including inside modules) *)
  let rec collect_mutables acc = function
    | [] -> acc
    | TDLetMut (name, _) :: rest -> collect_mutables (ss_add name acc) rest
    | TDModule (_, decls, _) :: rest ->
        let acc = collect_mutables acc decls in
        collect_mutables acc rest
    | _ :: rest -> collect_mutables acc rest
  in
  let mutables = collect_mutables ss_empty program in
  (* Collect known-pure extern functions from this program and all stdlib programs *)
  let pure_fns =
    List.fold_left
      (fun acc p -> ss_union acc (collect_pure_externs p))
      (collect_pure_externs program)
      stdlib_programs
  in
  List.map (optimize_decl ~mutables ~pure_fns) program
