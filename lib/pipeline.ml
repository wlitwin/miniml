(* Pipeline — the single path from a typechecked (and constraint-transformed)
   program to backend-ready IR.

   Every compilation entry point (bytecode VM, emit-js, emit-json, native)
   MUST go through [lower] rather than calling the individual passes, so that:
   - no entry point can forget, reorder, or duplicate a pass
   - the IR invariants below are checked at every pass boundary

   The pass sequence is:
     Typechecker.classify_handlers_with   (handler arm classification)
     Match_tree.lower_program             (pattern matches -> decision trees)
     Texpr_opt.optimize_program           (dead-let / inline / const-fold)

   Invariants validated at pass boundaries (always on — a validator walk is
   O(n) and cheap relative to the passes themselves):
   - after match-tree lowering: no TEMatch nodes remain anywhere in the
     program (backends only handle TEMatchTree)
   - after optimization: lowering invariants still hold (the optimizer must
     not reintroduce pre-lowering forms)

   NOTE: the self-hosted compiler (self_host/main.mml) currently runs a
   DIFFERENT pipeline — classify + lower only, no texpr_opt (texpr_opt.ml is
   not in the translation set). Tracked as a structural divergence to resolve
   (roadmap #3b). *)

exception Invariant_violation of string

(* Plain (non-format) message — the validators pass a fixed string. Kept simple
   so the self-hosted translation needs only Printf.sprintf (which ocaml_to_mml
   lowers to string interpolation), not Printf.ksprintf. *)
let violation stage msg =
  raise
    (Invariant_violation
       (Printf.sprintf "IR invariant violated after %s: %s" stage msg))

(* Walk every texpr in the tree (including match-tree guards/arms and handler
   arms, via map_texpr_children) and check no TEMatch survived lowering. *)
let rec check_no_tematch stage (te : Typechecker.texpr) : unit =
  (match te.Typechecker.expr with
  | Typechecker.TEMatch _ ->
      violation stage "TEMatch node survived match-tree lowering"
  | _ -> ());
  ignore
    (Typechecker.map_texpr_children
       (fun child ->
         check_no_tematch stage child;
         child)
       te)

let rec validate_lowered stage (program : Typechecker.tprogram) : unit =
  List.iter
    (fun decl ->
      match decl with
      | Typechecker.TDLet (_, e)
      | Typechecker.TDLetMut (_, e)
      | Typechecker.TDLetRec (_, e)
      | Typechecker.TDExpr e ->
          check_no_tematch stage e
      | Typechecker.TDLetRecAnd binds ->
          List.iter (fun (_, e) -> check_no_tematch stage e) binds
      | Typechecker.TDModule (_, decls, _) -> validate_lowered stage decls
      | Typechecker.TDEffect _ | Typechecker.TDType _ | Typechecker.TDClass _
      | Typechecker.TDExtern _ | Typechecker.TDOpen _ ->
          ())
    program

(* Lower a typechecked, constraint-transformed program to backend-ready IR.

   [inline_handlers]: whether all-THOpTry/all-THOpProvide handler bodies may
   run inline so `return` can escape them (true for every current backend —
   see Typechecker.classify_handlers_with).
   [stdlib_programs]: previously-lowered stdlib programs, used by the
   optimizer for cross-program extern/usage information. *)
let lower ?(inline_handlers = true) ?(stdlib_programs = []) type_env
    (typed_program : Typechecker.tprogram) : Typechecker.tprogram =
  let p = Typechecker.classify_handlers_with inline_handlers typed_program in
  let p = Match_tree.lower_program type_env p in
  validate_lowered "match-tree lowering" p;
  let p = Texpr_opt.optimize_program ~stdlib_programs p in
  validate_lowered "texpr optimization" p;
  p
