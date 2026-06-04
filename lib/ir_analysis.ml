(** Shared static analyses over the normalized IR (the typed AST after
    [Pipeline.lower]). These are pure properties of [Typechecker.texpr] /
    [Match_tree_types] / [Types] — no backend specifics — so every backend may
    read them instead of re-deriving its own. See docs/ir-contract.md (the IR
    these analyse) and docs/effect-lowering.md (which consumes [needs_cps]).

    History: the effect/tail-resume predicates below were prototyped inside
    [Js_codegen]; they are hoisted here unchanged (Phase 3 [#11]) so the JS
    backend's selective-CPS lowering and any future consumer share one
    definition. *)

(* ---- Effect rows: the CPS calling-convention property ---------------------

   [needs_cps ty] — does a continuation ever need to thread through a function
   of this type? True iff some arrow in the (possibly curried) type carries a
   non-empty latent effect row: such a function can, dynamically, reach a
   [perform], so on emit-js it must take the CPS calling convention (a trailing
   continuation) rather than direct style. Pure functions (empty / undetermined
   rows) stay direct. This is the function-level companion to the handler-arm
   classification (THOpTry/THOpProvide/THOp); see docs/effect-lowering.md §3.

   An effect *variable* that is still unbound after resolution carries no
   determined effects (pure). A *generalized* effect var ([EffGen]) is
   effect-polymorphic: conservatively treated as possibly-effectful, because at
   a call site it may instantiate to a non-empty row (the dual-compilation case
   in docs/effect-lowering.md §5). *)
let rec eff_is_pure (e : Types.eff) =
  match Types.eff_repr e with
  | Types.EffEmpty -> true
  | Types.EffVar { contents = Types.EffUnbound _ } -> true
  | Types.EffVar { contents = Types.EffLink e } -> eff_is_pure e (* defensive; eff_repr resolves links *)
  | Types.EffRow _ -> false
  | Types.EffGen _ -> false

let rec needs_cps (ty : Types.ty) =
  match Types.repr ty with
  | Types.TArrow (_, eff, ret) | Types.TCont (_, eff, ret) ->
      (not (eff_is_pure eff)) || needs_cps ret
  | _ -> false

(* ---- Effect / tail-resume analysis (hoisted from Js_codegen) ------------- *)

let rec expr_has_perform_with ~check_perform ~enter_funs
    (te : Typechecker.texpr) =
  let go = expr_has_perform_with ~check_perform ~enter_funs in
  match te.expr with
  (* The perform's ARGUMENT can itself contain performs (of other ops) — it
     must be visited too. Missing this made `perform simple_op (... perform
     full_op ...)` invisible to the body-needs-CPS check, so the body compiled
     direct-style with identity continuations (BUG-14, same disease as BUG-6). *)
  | Typechecker.TEPerform (op_name, arg) -> check_perform op_name || go arg
  | Typechecker.TEResume _ -> true
  | Typechecker.TELet (_, _, e1, e2) | Typechecker.TESeq (e1, e2) ->
      go e1 || go e2
  | Typechecker.TEIf (cond, e1, e2) -> go cond || go e1 || go e2
  | Typechecker.TEMatch (scrut, arms, _) ->
      go scrut
      || List.exists
           (fun (_, g, body) ->
             go body || match g with Some g -> go g | None -> false)
           arms
  | Typechecker.TEMatchTree cm ->
      go cm.Match_tree_types.scrutinee
      || Array.exists
           (fun arm -> go arm.Match_tree_types.arm_body)
           cm.match_arms
      ||
      let rec check_tree = function
        | Match_tree_types.DGuard { guard; on_true; on_false; _ } ->
            go guard || check_tree on_true || check_tree on_false
        | Match_tree_types.DSwitch { cases; default; _ } -> (
            List.exists (fun (_, _, sub) -> check_tree sub) cases
            || match default with Some d -> check_tree d | None -> false)
        | Match_tree_types.DLeaf _ | Match_tree_types.DFail _ -> false
      in
      check_tree cm.tree
  | Typechecker.TELetRec (_, _, fn_e, body) -> go fn_e || go body
  | Typechecker.TELetRecAnd (bindings, body) ->
      List.exists (fun (_, e) -> go e) bindings || go body
  | Typechecker.TEFun (_, body, _) -> if enter_funs then go body else false
  | Typechecker.TEApp (fn, arg) -> go fn || go arg
  | Typechecker.TEHandle (body, arms) ->
      go body
      || List.exists
           (fun arm ->
             match arm with
             | Typechecker.THReturn (_, e)
             | Typechecker.THOp { body = e; _ }
             | Typechecker.THOpProvide (_, _, e)
             | Typechecker.THOpTry (_, _, e) ->
                 go e)
           arms
  | Typechecker.TELetMut (_, e1, e2) -> go e1 || go e2
  | Typechecker.TEWhile { tw_cond; tw_body; tw_step } ->
      go tw_cond || go tw_body
      || (match tw_step with Some s -> go s | None -> false)
  | Typechecker.TECons (e1, e2) -> go e1 || go e2
  | Typechecker.TETuple es -> List.exists go es
  | Typechecker.TERecord fields -> List.exists (fun (_, e) -> go e) fields
  | Typechecker.TEBinop (_, e1, e2) -> go e1 || go e2
  | Typechecker.TEUnop (_, e) -> go e
  (* The match must stay EXHAUSTIVE: a missed form here makes its performs
     invisible, so the whole handle body silently compiles in direct style
     with identity continuations (this was half of BUG-6). *)
  | Typechecker.TEAssign (_, e) -> go e
  | Typechecker.TEFieldAssign (r, _, v) -> go r || go v
  | Typechecker.TEField (e, _) -> go e
  | Typechecker.TEIndex (b, i) -> go b || go i
  | Typechecker.TEConstruct (_, Some e) -> go e
  | Typechecker.TEConstruct (_, None) -> false
  | Typechecker.TEArray es -> List.exists go es
  | Typechecker.TERecordUpdate (base, overrides) ->
      go base || List.exists (fun (_, e) -> go e) overrides
  | Typechecker.TERecordUpdateIdx (base, pairs) ->
      go base || List.exists (fun (i, v) -> go i || go v) pairs
  | Typechecker.TEBreak e -> go e
  | Typechecker.TEReturn e -> go e
  | Typechecker.TEFoldContinue e -> go e
  | Typechecker.TEForLoop e ->
      (* The for-in desugar wraps the loop BODY in a callback lambda, but
         that lambda always runs as part of evaluating the loop — so performs
         inside it are evaluation effects even under ~enter_funs:false (where
         ordinary lambdas are treated as inert values). *)
      expr_has_perform_with ~check_perform ~enter_funs:true e
  | Typechecker.TEContinueLoop -> false
  | Typechecker.TEInt _ | Typechecker.TEFloat _ | Typechecker.TEBool _
  | Typechecker.TEString _ | Typechecker.TEByte _ | Typechecker.TERune _
  | Typechecker.TEUnit | Typechecker.TENil | Typechecker.TEVar _ ->
      false

let expr_has_perform te =
  expr_has_perform_with ~check_perform:(fun _ -> true) ~enter_funs:true te

let expr_has_unhandled_perform handled_ops te =
  expr_has_perform_with
    ~check_perform:(fun op -> not (List.mem op handled_ops))
    ~enter_funs:true te

(* Can *evaluating* this expression perform an effect? Unlike
   [expr_has_perform] this does not look inside lambdas: evaluating a lambda
   only builds a closure — its body runs when it is called, not here. Used by
   the ANF lifting below to decide which subexpressions need to be threaded
   through real continuations. *)
let eval_can_perform te =
  expr_has_perform_with ~check_perform:(fun _ -> true) ~enter_funs:false te

(* Does this loop body contain a `break`/`continue` targeting THIS loop that sits
   inside a handler body? A handler body compiles to a separate JS function (the
   try/with IIFE or a CPS `_trampoline(function(){...})`), so a literal JS
   `break;`/`continue;` emitted there is illegal ("no surrounding iteration
   statement"). Such a loop must instead use throw-based break/continue that the
   loop catches. The scan stops at nested loops/functions, which capture their
   own break/continue. *)
let rec loop_ctl_escapes_handler ~in_handler (te : Typechecker.texpr) =
  let go = loop_ctl_escapes_handler ~in_handler in
  match te.expr with
  | Typechecker.TEBreak _ | Typechecker.TEContinueLoop -> in_handler
  (* Boundaries: a nested loop/function captures its own break/continue. *)
  | Typechecker.TEFun _ | Typechecker.TEWhile _ | Typechecker.TEForLoop _ ->
      false
  | Typechecker.TEHandle (body, arms) ->
      let g = loop_ctl_escapes_handler ~in_handler:true in
      g body
      || List.exists
           (fun arm ->
             match arm with
             | Typechecker.THReturn (_, e)
             | Typechecker.THOp { body = e; _ }
             | Typechecker.THOpProvide (_, _, e)
             | Typechecker.THOpTry (_, _, e) ->
                 g e)
           arms
  | Typechecker.TELet (_, _, e1, e2) | Typechecker.TESeq (e1, e2) ->
      go e1 || go e2
  | Typechecker.TEIf (cond, e1, e2) -> go cond || go e1 || go e2
  | Typechecker.TEMatch (scrut, arms, _) ->
      go scrut
      || List.exists
           (fun (_, g, body) ->
             go body || match g with Some g -> go g | None -> false)
           arms
  | Typechecker.TEMatchTree cm ->
      go cm.Match_tree_types.scrutinee
      || Array.exists (fun arm -> go arm.Match_tree_types.arm_body) cm.match_arms
      ||
      let rec check_tree = function
        | Match_tree_types.DGuard { guard; on_true; on_false; _ } ->
            go guard || check_tree on_true || check_tree on_false
        | Match_tree_types.DSwitch { cases; default; _ } -> (
            List.exists (fun (_, _, sub) -> check_tree sub) cases
            || match default with Some d -> check_tree d | None -> false)
        | Match_tree_types.DLeaf _ | Match_tree_types.DFail _ -> false
      in
      check_tree cm.tree
  | Typechecker.TELetMut (_, e1, e2) -> go e1 || go e2
  | Typechecker.TELetRec (_, _, _, body) -> go body
  | Typechecker.TELetRecAnd (_, body) -> go body
  | _ -> false

let loop_body_escapes_loop_ctl te =
  loop_ctl_escapes_handler ~in_handler:false te

(* Do any DGuard guard expressions in a decision tree contain a perform/resume?
   Guards run in non-tail (dispatch) position, so a resume there must be trampolined.
   Leaves are skipped (they reference arm bodies, checked separately as tail). *)
let rec decision_guards_have_perform = function
  | Match_tree_types.DGuard { guard; on_true; on_false; _ } ->
      expr_has_perform guard
      || decision_guards_have_perform on_true
      || decision_guards_have_perform on_false
  | Match_tree_types.DSwitch { cases; default; _ } ->
      List.exists (fun (_, _, sub) -> decision_guards_have_perform sub) cases
      || (match default with Some d -> decision_guards_have_perform d | None -> false)
  | Match_tree_types.DLeaf _ | Match_tree_types.DFail _ -> false

(* Check if all TEResume in body are in tail position (safe to skip _trampoline).
   A resume (or perform, which likewise returns a trampoline bounce) in a NON-tail
   position must be trampolined to resolve its value, so it makes this false. Every
   non-tail sub-position (let/seq RHS, if-condition, match scrutinee, match guards,
   decision-tree guards) is checked with [expr_has_perform]; tail sub-positions
   (bodies/branches/arms) are checked recursively. *)
let rec all_resumes_are_tail (te : Typechecker.texpr) =
  match te.expr with
  | Typechecker.TEResume _ -> true
  | Typechecker.TESeq (e1, e2) ->
      (not (expr_has_perform e1)) && all_resumes_are_tail e2
  | Typechecker.TELet (_, _, e1, e2) ->
      (not (expr_has_perform e1)) && all_resumes_are_tail e2
  | Typechecker.TELetRec (_, _, _, e2) -> all_resumes_are_tail e2
  | Typechecker.TELetMut (_, e1, e2) ->
      (not (expr_has_perform e1)) && all_resumes_are_tail e2
  | Typechecker.TEIf (cond, then_e, else_e) ->
      (not (expr_has_perform cond))
      && all_resumes_are_tail then_e && all_resumes_are_tail else_e
  | Typechecker.TEMatch (scrut, arms, _) ->
      (not (expr_has_perform scrut))
      && List.for_all
           (fun (_, g, body) ->
             (match g with Some g -> not (expr_has_perform g) | None -> true)
             && all_resumes_are_tail body)
           arms
  | Typechecker.TEMatchTree cm ->
      (not (expr_has_perform cm.Match_tree_types.scrutinee))
      && (not (decision_guards_have_perform cm.Match_tree_types.tree))
      && Array.for_all
           (fun arm -> all_resumes_are_tail arm.Match_tree_types.arm_body)
           cm.match_arms
  | _ ->
      (* Any other node (binop/app/tuple/cons/record/unop/...) computes a value
         from its sub-expressions, so any resume inside it is in NON-tail
         position and must be trampolined. Safe (true) iff it contains none. *)
      not (expr_has_perform te)
