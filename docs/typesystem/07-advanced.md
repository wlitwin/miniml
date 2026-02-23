# Advanced Topics

Part 7 in a series on the MiniML type system implementation. This final
chapter covers algebraic effects with row-polymorphic effect typing,
modules, mutual recursion, deriving, type synonyms, type annotation
resolution, and GADTs.


## Algebraic Effects

Algebraic effects provide structured control flow -- a way for a computation
to "perform" an operation and have a surrounding handler decide what that
operation does. They generalize exceptions (which abort) by offering a
continuation that can resume the computation. MiniML tracks effects using
a row-polymorphic effect system, so the typechecker knows which effects a
function may perform and enforces that all effects are handled before
reaching the top level.

### Effect Declarations

An effect is declared with a name and a list of operations. Each operation
has a type annotation that must be a function type (parameter to result):

```ocaml
(* AST node *)
| DEffect of string * string list * (string * ty_annot) list
  (* effect_name, type_params, [(op_name, op_type)] *)
```

The second element is a list of type parameter names. A non-parameterized
effect like `Console` has an empty list. A parameterized effect like
`State 'a` has `["a"]`.

During typechecking (`check_decl`), the type parameters are resolved and
the operations are processed with `TGen` indices for the effect's type
parameters. An `effect_def` is registered in the type environment:

```ocaml
| Ast.DEffect (name, type_params, ops) ->
  let num_params = List.length type_params in
  let param_tbl = Hashtbl.create 4 in
  List.iteri (fun i p -> Hashtbl.replace param_tbl p i) type_params;
  let resolved_ops = List.map (fun (op_name, annot) ->
    let ty = resolve_ty_annot_with_tgens ctx 0 param_tbl annot in
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
```

Operation types for parameterized effects contain `TGen` indices
corresponding to the effect's type parameters. For example, `State 'a`
with operation `get : unit -> 'a` stores `get` as
`TArrow(TUnit, EffEmpty, TGen 0)` -- the `TGen 0` refers to the effect's
first type parameter `'a`.

The `effect_def` type in `lib/types.ml`:

```ocaml
type effect_def = {
  effect_name: string;
  effect_params: string list;   (* e.g. ["a"] for State 'a *)
  effect_ops: (string * ty) list;
}
```

Effects are looked up by operation name rather than by effect name. The
`find_effect_op` function scans all registered effects for a matching
operation:

```ocaml
let find_effect_op type_env op_name =
  List.find_map (fun (edef : effect_def) ->
    List.find_map (fun (name, ty) ->
      if String.equal name op_name then
        Some (edef.effect_name, ty, edef.effect_params)
      else None
    ) edef.effect_ops
  ) type_env.effects
```

This means operation names must be globally unique across all effects.

### Explicit Effect Annotations

Effects are fully inferred by default, but users can optionally write
explicit effect annotations on function types using `/` syntax. The AST
representation for type annotations has been extended to support this:

```ocaml
type ty_annot =
  | ...
  | TyArrow of ty_annot * ty_annot * eff_annot option  (* a -> b with optional effect *)
  | TyWithEffect of ty_annot * eff_annot               (* return type / effect on let decls *)

and eff_annot =
  | EffAnnotPure                           (* / pure *)
  | EffAnnotRow of eff_item list           (* / Effect1, Effect2, ... *)

and eff_item =
  | EffLabel of string * ty_annot list     (* Effect or Effect int *)
  | EffVar of string                       (* 'e -- an effect variable *)
```

`TyArrow` is now a 3-tuple: the parameter type, the return type, and an
optional effect annotation. When the effect annotation is `None`, the
effect is inferred (a fresh effect variable is created). When present,
it constrains the function's latent effect.

`TyWithEffect` is used on return type annotations for `let` declarations,
allowing the user to annotate the effect of the innermost arrow. For
example, `let f (x: int) : string / Console = ...` means the function
body may perform `Console` effects.

In `resolve_ty_annot_shared`, the effect annotation is handled as follows:

```ocaml
| Ast.TyArrow (a, b, eff_opt) ->
  let a_ty = go a in
  let b_ty = go b in
  let eff = match eff_opt with
    | None -> Types.new_effvar level          (* infer: fresh effect variable *)
    | Some Ast.EffAnnotPure -> Types.EffEmpty  (* / pure *)
    | Some (Ast.EffAnnotRow items) ->
      let tail = Types.new_effvar level in
      List.fold_right (fun item acc ->
        match item with
        | Ast.EffLabel (name, args) ->
          let arg_tys = List.map go args in
          Types.EffRow (name, arg_tys, acc)
        | Ast.EffVar v ->
          (* look up or create effect variable *)
          ...
      ) items tail
  in
  Types.TArrow (a_ty, eff, b_ty)
```

When `eff_opt` is `None`, a fresh effect variable is created (the
default inferred behavior). `EffAnnotPure` maps to `EffEmpty`, meaning
the function is guaranteed pure. `EffAnnotRow` builds an `EffRow` chain
from the listed effect labels, each carrying its type arguments, with a
fresh effect variable as the open tail.

The `wrap_params_decl` function handles `TyWithEffect` on return type
annotations by building a full arrow type annotation with the effect
placed on the innermost arrow:

```ocaml
(* When return annotation is TyWithEffect(ret_ty, eff_annot),
   the innermost arrow in the curried chain gets eff_annot *)
| Ast.TyWithEffect (ret_annot, eff_annot) ->
  (* Build TyArrow(last_param, ret_annot, Some eff_annot) for the
     innermost arrow, outer arrows get None (inferred) *)
```

This means `let f (x: int) (y: int) : string / Console` produces
a type like `int -> int -[Console|e]-> string` -- only the innermost
arrow carries the annotated effect.

### Effect Row Types

Every function arrow in MiniML carries an effect annotation:
`TArrow of ty * eff * ty`. The middle component is the function's
**latent effect** -- the effects it may perform when applied.

The effect type is defined in `lib/types.ml`:

```ocaml
and eff =
  | EffVar of effvar ref       (* effect row variable *)
  | EffEmpty                   (* pure — no effects *)
  | EffRow of string * ty list * eff  (* effect label + type params + tail *)
  | EffGen of int              (* quantified effect var in scheme *)

and effvar =
  | EffUnbound of int * int    (* id, level — same as type vars *)
  | EffLink of eff             (* linked by unification *)
```

Effect rows are linked lists of labels terminated by either `EffEmpty`
(a closed, pure row) or `EffVar` (an open row that can accumulate more
effects). The `ty list` in `EffRow` carries type parameters for
parameterized effects. A non-parameterized effect like `Console` has an
empty list; a parameterized effect like `State int` has `[TInt]`.
Multiple effects are nested:
`EffRow("State", [TInt], EffRow("IO", [], EffVar(e)))`. Row unification
handles order-independence using Remy-style rewriting (see below).

The scheme type carries `equant: int` for quantified effect variables,
alongside the existing `quant` for type variables:

```ocaml
type scheme = {
  quant: int;    (* number of quantified type variables *)
  equant: int;   (* number of quantified effect variables *)
  constraints: class_constraint list;
  body: ty;      (* body containing TGen 0..quant-1 and EffGen 0..equant-1 *)
}
```

Fresh effect variables are created with `new_effvar`, which mirrors
`new_tvar`:

```ocaml
let new_effvar level =
  EffVar (ref (EffUnbound (fresh_id (), level)))
```

And `eff_repr` follows links with path compression, just like `repr`
for types:

```ocaml
let rec eff_repr = function
  | EffVar ({ contents = EffLink eff } as r) ->
    let eff = eff_repr eff in
    r := EffLink eff;
    eff
  | eff -> eff
```

### Effect Flow Through Inference

The typechecker context carries `current_eff: Types.eff` -- the ambient
effect variable for the current function scope. This is the effect row
that `perform` writes into and that `handle` reads from.

```ocaml
type ctx = {
  vars: (string * Types.scheme) list;
  mutable_vars: (string * Types.scheme) list;
  type_env: Types.type_env;
  loop_info: loop_info option;   (* WhileLoop | UnitLoop | FoldLoop of string *)
  current_module: string option;
  constraint_tvars: int list;
  current_eff: Types.eff;
}
```

**EFun**: Creates a fresh `body_eff`, installs it as `current_eff` in
the body's context, and produces a `TArrow(param_ty, body_eff, body_ty)`:

```ocaml
| Ast.EFun (param, body) ->
  let param_ty = match param.annot with
    | Some annot -> resolve_ty_annot ctx level annot
    | None -> Types.new_tvar level
  in
  let body_eff = Types.new_effvar level in
  let ctx' = extend_var_mono { ctx with current_eff = body_eff } param.name param_ty in
  let body_te = synth ctx' level body in
  mk (TEFun (param.name, body_te)) (Types.TArrow (param_ty, body_eff, body_te.ty))
```

**EApp**: Creates a fresh `call_eff`, unifies it with the function's
latent effect, then subeffects it into the ambient. The subeffect
relation (rather than plain unification) is what allows pure functions
to be called in any context:

```ocaml
| Ast.EApp (fn, arg) ->
  let fn_te = synth ctx level fn in
  let arg_te = synth ctx level arg in
  let ret_ty = Types.new_tvar level in
  let call_eff = Types.new_effvar level in
  try_unify fn_te.ty (Types.TArrow (arg_te.ty, call_eff, ret_ty));
  try_subeffect call_eff ctx.current_eff;
  mk (TEApp (fn_te, arg_te)) ret_ty
```

**EPerform**: Unifies `ctx.current_eff` with
`EffRow(effect_name, type_params, fresh)` -- this adds the effect label
and its type parameters to the ambient row. For parameterized effects,
`synth_perform` creates fresh type variables for the effect's type
parameters and substitutes them into the operation's parameter and return
types (replacing `TGen` indices):

```ocaml
and synth_perform ctx level op_name arg =
  match Types.find_effect_op ctx.type_env op_name with
  | None -> error (Printf.sprintf "unknown effect operation: %s" op_name)
  | Some (effect_name, op_ty, effect_params) ->
    let num_params = List.length effect_params in
    let fresh_params = List.init num_params (fun _ -> Types.new_tvar level) in
    let op_ty = if num_params > 0 then subst_tgens fresh_params op_ty else op_ty in
    (match Types.repr op_ty with
     | Types.TArrow (param_ty, _, ret_ty) ->
       let arg_te = check ctx level arg param_ty in
       let fresh_tail = Types.new_effvar level in
       try_unify_eff ctx.current_eff
         (Types.EffRow (effect_name, fresh_params, fresh_tail));
       mk (TEPerform (op_name, arg_te)) ret_ty
     | _ -> error (Printf.sprintf "effect operation %s must have function type" op_name))
```

**EHandle**: The body gets a fresh `body_eff`. Handled effects are
removed from the row via `rewrite_row`. Remaining effects flow to the
outer ambient. The continuation `k` is precisely typed:

```ocaml
and synth_handle ctx level body arms =
  let body_eff = Types.new_effvar level in
  let body_ctx = { ctx with current_eff = body_eff } in
  let body_te = synth body_ctx level body in
  let result_ty = Types.new_tvar level in

  (* Collect handled effect names from op arms *)
  let handled_effects = List.filter_map (fun arm ->
    match arm with
    | Ast.HOp (op_name, _, _, _) ->
      (match Types.find_effect_op ctx.type_env op_name with
       | Some (effect_name, _, _) -> Some effect_name
       | None -> error (Printf.sprintf "unknown effect operation in handler: %s" op_name))
    | _ -> None
  ) arms in

  (* Remove handled effects from body_eff, remainder flows to outer ambient *)
  let remaining_eff = List.fold_left (fun eff effect_name ->
    try Types.rewrite_row effect_name eff
    with Types.Unify_error msg -> error msg
  ) (Types.eff_repr body_eff) (List.sort_uniq String.compare handled_effects) in
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
       | None -> error (...)
       | Some (_, op_ty, effect_params) ->
         let num_params = List.length effect_params in
         let fresh_params = List.init num_params (fun _ -> Types.new_tvar level) in
         let op_ty = if num_params > 0
           then subst_tgens fresh_params op_ty else op_ty in
         let param_ty, ret_ty_op = match Types.repr op_ty with
           | Types.TArrow (a, _, b) -> a, b
           | _ -> error (...) in
         let k_ty = Types.TArrow (ret_ty_op, ctx.current_eff, result_ty) in
         let ctx' = extend_var_mono ctx arg_name param_ty in
         let ctx' = extend_var_mono ctx' k_name k_ty in
         let handler_te = check ctx' level handler_body result_ty in
         THOp (op_name, arg_name, k_name, handler_te))
  ) arms in
  mk (TEHandle (body_te, typed_arms)) result_ty
```

The key detail in the handler: the continuation `k` is typed as
`TArrow(ret_ty_op, ctx.current_eff, result_ty)`. It takes the
operation's return type, carries the *handler's* ambient effect (not
the body's), and produces the handler's result type.

For parameterized effects, `synth_handle` creates fresh type variables
for each handled effect's type parameters and substitutes them into the
operation types (replacing `TGen` indices), just as `synth_perform` does.
This ensures that the handler's argument binding and continuation have
correctly instantiated types.

### Row Unification

`unify_eff` follows Remy-style row unification. The algorithm:

1. Follow links with `eff_repr` on both sides.
2. Physical equality check -- if same ref cell, done.
3. If either side is a variable: occurs-check + level adjustment, then link.
4. Both `EffEmpty`: done.
5. Same head label: unify type params pairwise, then unify tails recursively.
6. Different head labels: call `rewrite_row` to find/move the label, then unify tails.

```ocaml
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
    when String.equal label1 label2 ->
    List.iter2 unify params1 params2;
    unify_eff tail1 tail2
  | EffRow (label1, _, tail1), _ ->
    let tail2 = rewrite_row label1 e2 in
    unify_eff tail1 tail2
  | _, EffRow _ ->
    unify_eff e2 e1
  | EffGen i, EffGen j when i = j -> ()
  | _ ->
    raise (Unify_error (Printf.sprintf "cannot unify effects %s and %s"
      (pp_eff e1) (pp_eff e2)))
```

`rewrite_row label eff` finds a label in a row and returns the row with
it removed. This is the core operation that makes row order irrelevant:

```ocaml
and rewrite_row label eff =
  match eff_repr eff with
  | EffRow (l, _, tail) when String.equal l label -> tail
  | EffRow (l, params, tail) ->
    EffRow (l, params, rewrite_row label tail)
  | EffVar ({ contents = EffUnbound (id, level) } as r) ->
    let new_tail = new_effvar level in
    let new_row = EffRow (label, [], new_tail) in
    occurs_check_eff id level new_row;
    r := EffLink new_row;
    new_tail
  | EffEmpty ->
    raise (Unify_error (Printf.sprintf "cannot rewrite effect row for %s" label))
```

The cases:
- `EffRow(label, _, tail)`: found it -- return `tail` (the row without the label).
- `EffRow(other, params, tail)`: wrong label -- keep `other` with its params and recurse into the tail.
- `EffVar(EffUnbound)`: open row -- link the variable to `EffRow(label, [], fresh)` and return `fresh`. This is how an open row "absorbs" a new label.
- `EffEmpty`: closed row that does not contain the label -- error.

Effect unification also occurs during type unification. When two
`TArrow` types are unified, their effect components are unified too:

```ocaml
| TArrow (a1, e1, r1), TArrow (a2, e2, r2) ->
  unify a1 a2;
  unify_eff e1 e2;
  unify r1 r2
```

### Subeffect Relation

The `subeffect` function is used in `EApp` to allow pure functions to be
called in any effect context. Without it, every pure builtin would need
to be effect-polymorphic. `subeffect source target`:

```ocaml
let rec subeffect source target =
  let source = eff_repr source in
  let target = eff_repr target in
  if source == target then ()
  else match source with
  | EffEmpty -> ()
  | EffVar ({ contents = EffUnbound _ }) ->
    unify_eff source target
  | EffRow (label, _params, tail) ->
    let target_tail = rewrite_row label target in
    subeffect (eff_repr tail) target_tail
  | EffGen _ -> ()
  | _ -> unify_eff source target
```

- If source is `EffEmpty`, do nothing -- pure is always OK.
- If source is `EffRow(label, params, tail)`, rewrite the label into the target row and recurse on the tails.
- If source is an unbound variable, fall through to `unify_eff` (the variable could be anything).
- If source is `EffGen`, do nothing (quantified effects from a scheme are abstract).
- Otherwise, fall through to `unify_eff`.

The asymmetry is the point: `EffEmpty` is a sub-effect of anything, but
not vice versa. A function that performs `Console` effects cannot be
silently treated as pure.

### Effect-Polymorphic Class Methods

Type class methods can have effect variables (like `'e`) in their types,
allowing methods to be effect-polymorphic. For example, a `Monad` class
might have a `bind` method whose callback can perform arbitrary effects.

During class definition processing, effect variables in method type
annotations are generalized alongside type variables. Effect variables
receive `EffGen` indices in the method's scheme, just as type variables
receive `TGen` indices. The scheme's `equant` field records the number
of quantified effect variables.

When an instance is processed, the method's `EffGen` indices are
instantiated with fresh effect variables, just as `TGen` indices are
instantiated with fresh type variables. This ensures that each use of an
instance method gets its own effect context. The instance processing
code handles `EffGen` substitution in the same pass that substitutes
`TGen`:

```ocaml
(* During instance method type instantiation *)
let go_eff = function
  | EffGen id when id < s.equant -> evars.(id)
  | ...
```

This mechanism allows class methods to express effect constraints. A
method typed as `'a -> 'a / 'e` in a class definition will have its
`'e` quantified as `EffGen 0`. Each instance and each call site gets a
fresh effect variable for `'e`, preserving effect polymorphism across
different uses.

### Effect Freshening in Type Expansion

Stored types (type synonyms, constructor arguments, schemes) contain
`EffEmpty` for arrows because annotations do not specify effects.
When these types are expanded or instantiated, `EffEmpty` arrows must
be replaced with fresh effect variables so that they do not block
unification with effectful values.

**`freshen_arrow_effects`** is applied at synonym expansion and
constructor instantiation points:

```ocaml
let freshen_arrow_effects level ty =
  let rec go = function
    | Types.TArrow (a, eff, b) ->
      let eff' = match eff with
        | Types.EffEmpty -> Types.new_effvar level
        | e -> e
      in
      Types.TArrow (go a, eff', go b)
    | Types.TTuple ts -> Types.TTuple (List.map go ts)
    | Types.TList t -> Types.TList (go t)
    | Types.TArray t -> Types.TArray (go t)
    | Types.TRecord fs -> Types.TRecord (List.map (fun (n, t) -> (n, go t)) fs)
    | Types.TVariant (name, args) -> Types.TVariant (name, List.map go args)
    | Types.TMap (k, v) -> Types.TMap (go k, go v)
    | t -> t
  in
  go ty
```

It only replaces `EffEmpty` -- if the effect is already a variable or
a row, it is left alone. This function is called in several places:
- Synonym expansion in `resolve_ty_annot_shared` (both zero-param and
  parameterized synonyms via `subst_tgens`)
- Constructor instantiation in `synth_construct` and `check_pattern`
- Record field instantiation in `instantiate_record_fields`

**`instantiate`** replaces `EffEmpty` with a *shared* fresh `EffVar`
per scheme instantiation. This is critical for higher-order functions.
Consider `fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a`. The
callback's effect must flow into `fold`'s own latent effect. By
replacing all `EffEmpty` in the instantiated type with the same fresh
variable, the callback's effect and `fold`'s latent effect become
linked:

```ocaml
let instantiate level (s : scheme) =
  if s.quant = 0 && s.equant = 0 then s.body
  else begin
    let vars = Array.init s.quant (fun _ -> new_tvar level) in
    let evars = Array.init s.equant (fun _ -> new_effvar level) in
    let shared_eff = lazy (new_effvar level) in
    let rec go = function
      | TGen id -> vars.(id)
      | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
      | ...
    and go_eff = function
      | EffGen id when id < s.equant -> evars.(id)
      | EffRow (label, params, tail) -> EffRow (label, List.map go params, go_eff tail)
      | EffEmpty -> Lazy.force shared_eff
      | e -> e
    in
    go s.body
  end
```

The `shared_eff` is lazy so it is only allocated if there are `EffEmpty`
positions in the scheme body. Every `EffEmpty` in the same instantiation
maps to the same variable, linking all the arrows together.

### Top-Level Enforcement

At the top level, `DExpr` declarations get a fresh ambient effect that
is unified with `EffEmpty` after typechecking. If unhandled effects
remain, the typechecker reports an error:

```ocaml
| Ast.DExpr expr ->
  let eff = Types.new_effvar level in
  let ctx' = { ctx with current_eff = eff } in
  let te = synth ctx' level expr in
  (try Types.unify_eff (Types.eff_repr eff) Types.EffEmpty
   with Types.Unify_error _ ->
     error (Printf.sprintf "expression has unhandled effects: %s" (Types.pp_eff eff)));
  (ctx, [TDExpr te])
```

`DLet`, `DLetRec`, and `DLetMut` declarations follow the same pattern:
each gets a fresh ambient effect via `new_effvar`, and the expression
is typechecked in a context with that effect as `current_eff`. The
effect is not explicitly closed for let-bindings (it gets generalized
into the scheme), but for `DExpr` it must be empty.

### Native Break/Continue via Control Stack

MiniML's loops use native break and continue support via the VM's
control stack rather than effect handlers. The compiler wraps loop
bodies in `ENTER_LOOP`/`EXIT_LOOP` instructions that push and pop
control stack frames. Break compiles to `TEBreak`, which emits
`LOOP_BREAK` -- the VM unwinds the control stack to the nearest
`ENTER_LOOP` frame.

For `for` and `for/fold` loops, the typed AST wraps the desugared
`fold` call in `TEForLoop`. The compiler emits the same
`ENTER_LOOP`/`EXIT_LOOP` frame around the fold call. Break and
continue within the loop body produce native control flow instructions
rather than effect operations.

Continue has two typed representations:

- `TEContinueLoop` for while loops -- restarts the iteration with no
  value.
- `TEFoldContinue value_te` for for/fold loops -- passes a value back
  as the new accumulator for the next iteration.

This design avoids the overhead of allocating effect handlers and
continuations for loop control flow, and makes break/continue
semantics independent of the algebraic effect system.

### Typing `resume`

The `EResume (k_expr, v_expr)` node represents resuming a
continuation in an effect handler:

```ocaml
| Ast.EResume (k_expr, v_expr) ->
  let k_te = synth ctx level k_expr in
  let v_te = synth ctx level v_expr in
  let result_ty = Types.new_tvar level in
  ignore k_te.ty;
  mk (TEResume (k_te, v_te)) result_ty
```

The continuation `k` is precisely typed at the handler site as
`TArrow(ret_ty_op, ctx.current_eff, result_ty)` -- it takes the
operation's return type, carries the handler's ambient effect, and
produces the handler's result type. The `EResume` node itself
still uses a fresh result type and ignores `k_te.ty` because the
continuation is opaque at the *call* site; the precise typing is
enforced at the handler where `k` is bound.

### Constrained Functions

`synth_constrained_fn` handles functions with `where` clauses
(typeclass constraints). It gives the body a fresh `body_eff` and
places it on the innermost arrow. Outer arrows (from curried
parameters) get `EffEmpty` because partial application is pure:

```ocaml
let synth_constrained_fn ctx level constraints params ret_annot body =
  ...
  let body_eff = Types.new_effvar (level + 1) in
  let inner_ctx = List.fold_left2 (fun c p ty ->
    extend_var_mono c p.Ast.name ty
  ) { ctx with constraint_tvars = constraint_tvar_ids @ ctx.constraint_tvars;
               current_eff = body_eff } params param_tys in
  ...
  (* Build TEFun chain -- innermost arrow gets body_eff, outer arrows are pure *)
  let te = match List.rev params, List.rev param_tys with
    | [], [] -> body_te
    | last_p :: rest_ps, last_pty :: rest_ptys ->
      let inner = mk (TEFun (last_p.Ast.name, body_te))
        (Types.TArrow (last_pty, body_eff, body_te.ty)) in
      List.fold_left2 (fun acc p pty ->
        mk (TEFun (p.Ast.name, acc)) (Types.TArrow (pty, Types.EffEmpty, acc.ty))
      ) inner rest_ps rest_ptys
    | _ -> assert false
  in
  ...
```

This means `let f (x: 'a) (y: 'a) : 'a where Num 'a = x + y` produces
a type like `'a -[pure]-> 'a -[e]-> 'a` -- applying `f` to one argument
is pure, but applying the result to a second argument may perform effects
from the body.


## Modules

Modules provide namespacing. A module groups declarations together, controls
visibility with `pub`/`opaque`/`private`, and makes public members accessible
via qualified names like `MyModule.my_function`.

### Module Structure

A module is declared with `DModule`, containing a list of declarations each
tagged with a visibility:

```ocaml
| DModule of string * module_decl list

and module_decl = {
  vis: visibility;   (* Public | Private | Opaque *)
  decl: decl;
}
```

### Processing a Module

The `process_module_def` function handles module typechecking:

```ocaml
let rec process_module_def ctx level mod_name (items : Ast.module_decl list) =
  let prefix = match ctx.current_module with
    | None -> mod_name ^ "."
    | Some parent -> parent ^ mod_name ^ "."
  in
  let sub_ctx = { ctx with current_module = Some prefix } in
```

It creates a **qualified name prefix** -- for a top-level module `Foo`, the
prefix is `"Foo."`. For a nested module `Foo.Bar`, the prefix is
`"Foo.Bar."`. A sub-context is created with `current_module` set so that
inner items know their fully qualified path.

Each declaration in the module body is processed with `check_module_item`,
which handles all declaration kinds: `DLet`, `DLetRec`, `DType`, `DClass`,
`DInstance`, `DEffect`, nested `DModule`, and `DOpen`.

### Visibility and the `module_info` Type

After processing all items, a `module_info` record is built:

```ocaml
type module_info = {
  mod_name: string;
  mod_pub_vars: (string * scheme) list;
  mod_pub_types: string list;
  mod_opaque_types: string list;
  mod_pub_constructors: (string * ctor_info) list;
  mod_instances: instance_def list;
  mod_submodules: (string * module_info) list;
  mod_pub_classes: string list;
}
```

Each field tracks what is public:

- `mod_pub_vars`: value bindings marked `pub`. These are the functions and
  values accessible as `Module.name`.
- `mod_pub_types`: type names marked `pub` or `opaque`. Stored as fully
  qualified names (e.g. `"MyModule.my_type"`).
- `mod_opaque_types`: types marked `opaque`. The type name is visible
  outside the module, but its constructors are not exported.
- `mod_pub_constructors`: variant constructors from public types.
- `mod_instances`: all typeclass instances (instances are always public).
- `mod_submodules`: nested modules that were marked `pub`.
- `mod_pub_classes`: typeclass definitions marked `pub`.

Private declarations (`vis = Private`) are visible inside the module body
but not recorded in `module_info`. They exist in the sub-context during
processing but are not added to any `pub_*` accumulator.

Opaque types (`vis = Opaque`) make the type name visible but do not export
constructors:

```ocaml
| Ast.Opaque ->
  pub_types := qualified_name :: !pub_types;
  opaque_types := qualified_name :: !opaque_types
  (* Don't export constructors *)
```

### How Qualified Names Are Registered

When a `pub` let binding is processed in a module, it is added to the
outer context under its qualified name:

```ocaml
let outer_vars = List.fold_left (fun vars (short, scheme) ->
  let qualified = prefix ^ short in
  (qualified, scheme) :: vars
) ctx.vars !pub_vars in
```

So `pub let add x y = x + y` inside module `Math` registers
`"Math.add"` in the outer variable context.

### Type Aliases in Modules

When a type is defined inside a module, it is registered under its
qualified name (e.g. `"Math.vec"`). But code inside the module needs to
refer to it by the short name `vec`. This is handled by adding a type
alias:

```ocaml
let te = { te with Types.type_aliases = (name, qualified_name) :: te.Types.type_aliases } in
```

The `type_aliases` field maps short names to qualified names:

```ocaml
type_aliases: (string * string) list;  (* short_name -> qualified_name *)
```

When a type alias is encountered (e.g. during annotation resolution), the
function `resolve_type_alias` translates it:

```ocaml
let resolve_type_alias type_env name =
  match List.assoc_opt name type_env.Types.type_aliases with
  | Some canonical -> canonical
  | None -> name
```

These aliases are scoped -- they are removed when the module definition
ends so they do not leak into the outer scope:

```ocaml
let outer_type_env = { outer_type_env with
  Types.type_aliases = ctx.type_env.Types.type_aliases;
} in
```

### Module Lookup and Dotted Paths

The `find_module_in_env` function supports dotted module paths like
`Outer.Inner`:

```ocaml
let find_module_in_env type_env mod_name =
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
```

It splits the path on `.`, looks up the first segment in `type_env.modules`,
then drills into `mod_submodules` for each subsequent segment.

### `open` Statements

The `DOpen` declaration imports public names from a module into the current
scope. It supports both unqualified opens (`open Foo`) and selective opens
(`open Foo (bar, baz)`):

```ocaml
| DOpen of string * string list option
  (* module_name, None = open all, Some = selective *)
```

The `open_module_into_ctx` function handles the import:

1. **Variables**: each `pub` var in the module is added to the context
   under its short name.
2. **Constructors**: public constructors are imported so patterns can
   use them without qualification.
3. **Types**: public type names are duplicated under their short name
   in `variants`, `records`, or `type_synonyms`.
4. **Submodules**: public submodules are added to `type_env.modules` so
   they can be referenced without the parent prefix.
5. **Class aliases**: public class names get a short alias in
   `type_aliases`.

Selective opens filter every import step through a `filter` function
that checks membership in the provided name list.


## Mutual Recursion

MiniML supports two forms of mutual recursion: mutually recursive functions
(`let rec ... and ...`) and mutually recursive types (`type ... and ...`).

### Mutually Recursive Functions

The `DLetRecAnd` declaration defines multiple functions that can call each
other:

```ocaml
| DLetRecAnd of (string * param list * ty_annot option * constraint_ list * expr) list
```

The typechecking algorithm in `check_decl` follows three steps:

**Step 1 -- Create monomorphic type variables for all names:**

```ocaml
let fn_vars = List.map (fun (name, _, _, _, _) ->
  (name, Types.new_tvar (level + 1))
) bindings in
(* Add all names to context as monomorphic *)
let ctx' = List.fold_left (fun ctx (name, tv) ->
  extend_var_mono ctx name tv
) ctx fn_vars in
```

Every function name gets a fresh type variable at `level + 1`. All names
are added to the context with monomorphic (ungeneralized) types. This is
critical -- inside the bodies, mutual calls see each other at specific
types, not polymorphic schemes.

**Step 2 -- Typecheck all bodies:**

```ocaml
let body_tes = List.map2 (fun (name, params, ret_annot, constraints, body) (_, tv) ->
  ...
  let full_body = wrap_params_decl params ret_annot body in
  let te = synth ctx' (level + 1) full_body in
  try_unify tv te.ty;
  te
) bindings fn_vars in
```

Each body is typechecked in the shared context `ctx'` where all
names are visible. The inferred type is unified with the name's
type variable, which may link variables across different bodies
(e.g. if `f` calls `g`, unification propagates type information
from `g`'s variable into `f`'s body).

**Step 3 -- Generalize all together:**

```ocaml
let ctx'' = List.fold_left2 (fun ctx (name, _, _, _, _) (_, tv) ->
  let scheme = Types.generalize level tv in
  extend_var ctx name scheme
) ctx bindings fn_vars in
```

After all bodies are checked, all type variables are generalized at
the same time. This is simultaneous generalization -- it happens at
`level`, so any type variable at `level + 1` that remains unbound
becomes a `TGen`. This means the mutual group shares the same
generalization boundary.

Why does simultaneous generalization matter? If the functions were
generalized independently, a function could be used polymorphically
in a sibling's body before its type is fully determined. This would
be unsound. By keeping all names monomorphic during checking and
generalizing after, the typechecker prevents this.

The same three-step algorithm also appears in the expression form
`ELetRecAnd`:

```ocaml
| Ast.ELetRecAnd (bindings, body) ->
  let fn_vars = List.map (fun (name, _) -> (name, Types.new_tvar (level + 1))) bindings in
  let ctx' = List.fold_left (fun ctx (name, tv) ->
    extend_var_mono ctx name tv
  ) ctx fn_vars in
  let body_tes = List.map2 (fun (_, fn_expr) (_, tv) ->
    let te = synth ctx' (level + 1) fn_expr in
    try_unify tv te.ty;
    te
  ) bindings fn_vars in
  let ctx'' = List.fold_left2 (fun ctx (name, _) (_, tv) ->
    let scheme = Types.generalize level tv in
    extend_var ctx name scheme
  ) ctx fn_vars fn_vars in
  let body_te = synth ctx'' level body in
  ...
```

### Mutually Recursive Types

The `DTypeAnd` declaration defines types that reference each other:

```ocaml
| DTypeAnd of (string list * string * type_def * string list) list
```

The algorithm is a two-pass approach:

**First pass -- register placeholders:**

```ocaml
let ctx' = List.fold_left (fun ctx (type_params, name, def, _deriving) ->
  let num_params = List.length type_params in
  match def with
  | Ast.TDVariant _ ->
    let type_env = { ctx.type_env with
      variants = (name, num_params, []) :: ctx.type_env.variants;
    } in
    { ctx with type_env }
  | Ast.TDRecord _ ->
    let type_env = { ctx.type_env with
      records = (name, []) :: ctx.type_env.records;
    } in
    { ctx with type_env }
  | Ast.TDAlias _ -> ctx
) ctx type_defs in
```

Every type name is registered with an empty definition. For variants, an
entry with zero constructors is added to `type_env.variants`. For records,
an empty field list is added to `type_env.records`. Aliases are skipped
since they do not create nominal types.

This placeholder registration is necessary so that when processing the
constructors of type `A`, any reference to type `B` resolves instead of
producing an "unknown type" error.

**Second pass -- process full definitions:**

```ocaml
let ctx'' = List.fold_left (fun ctx (type_params, name, def, _deriving) ->
  process_type_def ctx type_params name def
) ctx' type_defs in
```

Now each type definition is fully processed using `process_type_def`, which
replaces the placeholder with the real constructors/fields. Since all names
are already in the environment, forward references work.

After both passes, any `deriving` clauses are processed for each type in
the group.

Note: mutual recursion for types and functions is not yet supported inside
modules:

```ocaml
| Ast.DLetRecAnd _ ->
  error "mutual recursion (and) not yet supported inside modules"
| Ast.DTypeAnd _ ->
  error "mutual type recursion (and) not yet supported inside modules"
```


## Deriving

The `deriving` mechanism automatically generates typeclass instances for
type definitions. When you write:

```
type color = Red | Green | Blue deriving Show, Eq
```

the typechecker generates `Show` and `Eq` instances without any
hand-written code.

### The Dispatch Function

The top-level dispatch is `generate_derived_instance`:

```ocaml
let generate_derived_instance type_params name def class_name =
  match class_name, def with
  | "Show", Ast.TDVariant ctors -> Some (gen_show_variant type_params name ctors)
  | "Show", Ast.TDRecord fields -> Some (gen_show_record type_params name fields)
  | "Eq", Ast.TDVariant ctors -> Some (gen_eq_variant type_params name ctors)
  | "Eq", Ast.TDRecord fields -> Some (gen_eq_record type_params name fields)
  | _ -> None
```

It returns an `Option` containing the class name, instance types,
constraints, and methods -- exactly what `process_instance_def` needs
to register the instance. If the class/def combination is unsupported,
it returns `None` and the typechecker raises an error.

### Deriving `Show`

#### For Variants

`gen_show_variant` generates a `show` method that pattern-matches on each
constructor:

```ocaml
let gen_show_variant type_params name ctors =
  let inst_ty = make_inst_ty type_params name in
  let annots = List.filter_map (fun (_, a) -> a) ctors in
  let constraints = make_constraints "Show" type_params annots in
  let arms = List.map (fun (ctor_name, arg_annot) ->
    match arg_annot with
    | None ->
      (Ast.PatConstruct (ctor_name, None), None, Ast.EString ctor_name)
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
   [("show", [Ast.{name = "__x"; annot = None}], body)])
```

For a constructor with no payload (like `Red`), the show method returns
the constructor name as a string. For a constructor with a payload (like
`Some(x)`), it calls `show` recursively on the payload and wraps it in
parentheses.

For constructors carrying tuples, each element is shown separately with
commas between them.

#### For Records

`gen_show_record` generates a show method that accesses each field:

```ocaml
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
   [("show", [Ast.{name = "__x"; annot = None}], body)])
```

For a record `{ x: int; y: string }`, this produces output like
`"{ x = 42; y = hello }"`.

### Deriving `Eq`

#### For Variants

`gen_eq_variant` generates both `=` and `<>` methods. It matches on a
tuple of both arguments:

```ocaml
let arms = List.map (fun (ctor_name, arg_annot) ->
  match arg_annot with
  | None ->
    let pat = Ast.PatTuple [
      Ast.PatConstruct (ctor_name, None);
      Ast.PatConstruct (ctor_name, None)] in
    (pat, None, Ast.EBool true)
  | Some _ ->
    let lp = Ast.PatConstruct (ctor_name, Some (Ast.PatVar "__v0")) in
    let rp = Ast.PatConstruct (ctor_name, Some (Ast.PatVar "__w0")) in
    (Ast.PatTuple [lp; rp], None,
     Ast.EBinop (Ast.Eq, Ast.EVar "__v0", Ast.EVar "__w0"))
) ctors in
let arms = arms @ [(Ast.PatWild, None, Ast.EBool false)] in
```

Each constructor gets an arm that compares corresponding payloads. The
wildcard at the end catches the case where the two values have different
constructors and returns `false`. The `<>` method is generated as the
boolean negation of the same match.

#### For Records

`gen_eq_record` compares every field with `=` and combines results with `&&`:

```ocaml
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
```

### How Constraints Are Propagated

When a type has type parameters, derived instances need constraints. For
example, `type 'a box = Box of 'a deriving Show` must generate a `Show`
instance constrained by `Show 'a`. The `make_constraints` helper
collects type variable names that appear in constructor/field annotations
and generates a constraint for each:

```ocaml
let make_constraints class_name type_params annots =
  let all_vars = List.concat_map (fun ann -> collect_tyvars_annot ann) annots in
  let unique_vars = List.sort_uniq String.compare all_vars in
  let param_vars = List.filter (fun v -> List.mem v type_params) unique_vars in
  List.map (fun v -> (class_name, [v])) param_vars
```

So for `type ('a, 'b) pair = Pair of 'a * 'b deriving Show`, the generated
instance would carry constraints `Show 'a` and `Show 'b`.

### Where Deriving Is Invoked

Deriving is processed immediately after the type definition, both at the
top level and inside modules:

```ocaml
(* In check_decl, after process_type_def *)
let (ctx', derived_tdecls) = List.fold_left (fun (ctx, acc) cls ->
  match generate_derived_instance type_params name def cls with
  | Some (class_name, inst_tys, constraints, methods) ->
    let (ctx', tdecl) = process_instance_def ctx level class_name inst_tys constraints methods in
    (ctx', tdecl :: acc)
  | None ->
    error (Printf.sprintf "cannot derive %s for type %s" cls name)
) (ctx', []) deriving in
```

The generated instance is fed directly to `process_instance_def`, the same
function used for hand-written instances. The typechecker does not
distinguish between derived and hand-written instances after this point.

### GADT Deriving Guard

GADT types cannot use `deriving`. The generated code for derived instances
relies on ordinary pattern matching where constructors share a uniform
return type, and GADT constructors violate that assumption. When a
`deriving` clause appears on a GADT type, the typechecker raises an error:

```
cannot derive Show for GADT type expr
```

The check is straightforward: if any constructor in the `TDVariant` list
has a non-`None` third element (the GADT return annotation), the type is
classified as a GADT and deriving is rejected.


## Type Synonyms and Aliases

MiniML supports two related features for naming types: **type synonyms**
(structural equivalences) and **type aliases** (name mappings for qualified
types).

### Type Synonyms

A type synonym is declared with the `TDAlias` form of `type_def`:

```ocaml
type type_def =
  | TDVariant of (string * ty_annot option * ty_annot option) list
  | TDRecord of (bool * string * ty_annot) list
  | TDAlias of ty_annot
```

Each `TDVariant` constructor is a 3-tuple: `(constructor_name, arg_annotation, gadt_return_annotation)`. The third element is `None` for ordinary variants and `Some ty_annot` for GADT constructors that specify a return type. See the [GADTs](#gadts-generalized-algebraic-data-types) section for details.

Writing `type name = string` creates a synonym: anywhere `name` appears,
it is treated as `string`.

Synonyms are stored in `type_env.type_synonyms`:

```ocaml
type_synonyms: (string * int * ty) list;  (* name, num_params, expanded_ty *)
```

Each entry records the synonym name, how many type parameters it takes,
and the expanded type (which may contain `TGen` indices for parameters).

### Processing a Type Synonym

In `process_type_def`, the `TDAlias` branch resolves the annotation into a
type using a custom resolver that maps type parameter names to `TGen`
indices:

```ocaml
| Ast.TDAlias annot ->
  let param_tbl = Hashtbl.create 4 in
  List.iteri (fun i name -> Hashtbl.replace param_tbl name i) type_params;
  ...
  let rec resolve_alias = function
    | Ast.TyVar name ->
      (match Hashtbl.find_opt param_tbl name with
       | Some idx -> Types.TGen idx
       | None -> ...)
    | Ast.TyName "int" -> Types.TInt
    ...
  in
  let resolved_ty = resolve_alias annot in
  let type_env = { ctx.type_env with
    type_synonyms = (type_name, num_params, resolved_ty) :: ctx.type_env.type_synonyms;
  } in
  { ctx with type_env }
```

For a parameterized synonym like `type 'a seq = 'a list`, the parameter
`'a` is mapped to `TGen 0`, and the stored expanded type is `TList (TGen 0)`.
When the synonym is later used as `int seq`, the `TGen 0` is substituted
with `TInt` to produce `TList TInt` (i.e. `int list`).

### How Synonyms Are Expanded

Synonym expansion happens during type annotation resolution. In
`resolve_ty_annot_shared`, when a named type is encountered, the checker
first resolves any alias, then looks up the name in `type_synonyms`:

```ocaml
| Ast.TyName name ->
  let canonical = resolve_type_alias ctx.type_env name in
  (match List.find_opt (fun (n, _, _) -> String.equal n canonical) ctx.type_env.type_synonyms with
   | Some (_, 0, ty) -> ty        (* zero-param synonym: return the type directly *)
   | Some (_, n, _) ->
     error (Printf.sprintf "type %s expects %d type argument(s)" name n)
   | None -> ...)                  (* not a synonym: check variants, records, etc. *)
```

For parameterized synonyms, the expansion happens in the `TyApp` branch
using `subst_tgens`:

```ocaml
| Ast.TyApp (args, name) ->
  let canonical = resolve_type_alias ctx.type_env name in
  let arg_tys = List.map go args in
  (match List.find_opt (fun (n, _, _) -> String.equal n canonical) ctx.type_env.type_synonyms with
   | Some (_, np, ty) ->
     if List.length arg_tys <> np then error (...);
     subst_tgens arg_tys ty
   ...)
```

The `subst_tgens` function replaces each `TGen i` in the synonym body with
the corresponding argument type:

```ocaml
let subst_tgens inst_tys ty =
  let arr = Array.of_list inst_tys in
  let rec go = function
    | Types.TGen i when i < Array.length arr -> arr.(i)
    | Types.TArrow (a, eff, b) -> Types.TArrow (go a, eff, go b)
    | Types.TTuple ts -> Types.TTuple (List.map go ts)
    ...
  in
  go ty
```

### Type Aliases (Qualified Name Mappings)

Type aliases are a separate mechanism from synonyms. They map short names
to qualified names and are stored in:

```ocaml
type_aliases: (string * string) list;  (* short_name -> qualified_name *)
```

Type aliases are primarily used for modules. When a type `vec` is defined
inside module `Math`, it is registered as `Math.vec` in `type_env.variants`
or `type_env.records`. An alias `("vec", "Math.vec")` is added so code
inside the module can refer to `vec` without qualification.

Aliases are also used when `open` imports a module's types, and for
class names defined in modules.

### Pretty-Printing with Synonyms

The pretty-printer (`pp_ty` in `lib/types.ml`) uses synonyms in reverse --
it tries to match a structural type against known synonym patterns to
display the user-friendly name:

```ocaml
let try_match_synonym ty =
  let synonyms = !pp_synonyms in
  ...
  List.find_map (fun (name, num_params, pattern) ->
    let bindings = Array.make num_params None in
    let rec go pat actual = ... in
    if go pattern ty && Array.for_all (fun x -> x <> None) bindings then
      Some (name, ...)
    else None
  ) synonyms
```

This ensures that if you define `type point = { x: float; y: float }`,
error messages say `point` rather than `{ x: float; y: float }`.


## Type Annotation Resolution

Type annotations are user-written type expressions (the `ty_annot` AST type)
that must be converted into internal `ty` values. The two core functions
for this are `resolve_ty_annot` and `resolve_ty_annot_shared`.

### The `ty_annot` AST

```ocaml
type ty_annot =
  | TyName of string                    (* int, string, my_type *)
  | TyVar of string                     (* 'a, 'b *)
  | TyArrow of ty_annot * ty_annot * eff_annot option  (* a -> b, optional effect *)
  | TyRecord of (string * ty_annot) list (* { x: int; y: string } *)
  | TyList of ty_annot                  (* int list *)
  | TyArray of ty_annot                 (* int array *)
  | TyTuple of ty_annot list            (* int * string *)
  | TyApp of ty_annot list * string     (* ('a, 'b) result *)
  | TyMap of ty_annot * ty_annot        (* (string, int) map *)
  | TyQualified of string list * string (* Module.Type *)
  | TyWithEffect of ty_annot * eff_annot  (* return type / effect on let decls *)
```

### `resolve_ty_annot`

The simpler form creates a fresh `tvars` hash table and delegates to
the shared version:

```ocaml
let resolve_ty_annot ctx level (annot : Ast.ty_annot) : Types.ty =
  let tvars = Hashtbl.create 4 in
  resolve_ty_annot_shared ctx level tvars annot
```

### `resolve_ty_annot_shared`

This is the workhorse. The `tvars` table maps type variable names (like
`"a"` from `'a`) to fresh type variables, ensuring that multiple occurrences
of `'a` in the same annotation refer to the same type variable:

```ocaml
let resolve_ty_annot_shared ctx level tvars (annot : Ast.ty_annot) : Types.ty =
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
      (* Check synonyms, then variants, then records *)
      ...
    | Ast.TyArrow (a, b, eff_opt) ->
      let eff = match eff_opt with
        | None -> Types.new_effvar level
        | Some Ast.EffAnnotPure -> Types.EffEmpty
        | Some (Ast.EffAnnotRow items) -> resolve_eff_annot items
      in
      Types.TArrow (go a, eff, go b)
    | Ast.TyTuple ts -> Types.TTuple (List.map go ts)
    | Ast.TyList t -> Types.TList (go t)
    | Ast.TyArray t -> Types.TArray (go t)
    | Ast.TyMap (k, v) -> Types.TMap (go k, go v)
    | Ast.TyApp (args, name) -> (* parameterized type: expand synonym or build TVariant *)
      ...
    | Ast.TyRecord fields ->
      let fields = List.map (fun (n, t) -> (n, go t)) fields in
      let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
      Types.TRecord fields
    | Ast.TyQualified (path, name) ->
      let qualified = String.concat "." path ^ "." ^ name in
      go (Ast.TyName qualified)
  in
  go annot
```

Key behaviors:

- **Type variables** (`TyVar`): first occurrence creates a fresh tvar at
  the given level; subsequent occurrences reuse the same tvar. This ensures
  `'a -> 'a` produces `TArrow(tv, tv)` where both sides share the same
  ref cell.
- **Primitive names**: `"int"`, `"float"`, etc. are hard-coded to their
  corresponding `TInt`, `TFloat`, etc.
- **Named types** (`TyName`): first resolved through `resolve_type_alias`
  (for module-qualified lookups), then checked against synonyms, variants,
  and records in that order.
- **Parameterized types** (`TyApp`): arguments are resolved first, then
  the synonym or variant is looked up. For synonyms, `subst_tgens` performs
  the substitution. For variants, a `TVariant(name, args)` is constructed.
- **Record types** (`TyRecord`): fields are resolved and sorted
  alphabetically to ensure structural comparison works.
- **Qualified types** (`TyQualified`): converted to a dotted string like
  `"Module.Type"` and re-resolved as `TyName`.

### The Shared Table for Constrained Functions

The "shared" in `resolve_ty_annot_shared` matters for constrained functions.
When a function has a `where` clause like:

```
let compare (x: 'a) (y: 'a) : bool where Eq 'a = x = y
```

all parameter annotations and the return type annotation must share the
same type variable for `'a`. The `synth_constrained_fn` function creates
a single shared hash table and passes it through all annotation resolution
calls:

```ocaml
let synth_constrained_fn ctx level constraints params ret_annot body =
  let shared_tvars = Hashtbl.create 4 in
  (* Pre-allocate tvars for constraint type variables *)
  let constraint_tyvar_names = ... in
  List.iter (fun tvname ->
    Hashtbl.replace shared_tvars tvname (Types.new_tvar (level + 1))
  ) constraint_tyvar_names;
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
  ...
```

The constraint type variable names are pre-allocated in the shared table
before any annotations are resolved. Their IDs are also recorded in
`ctx.constraint_tvars` so that the typechecker knows not to default these
variables to `int` during binary operator inference (e.g. `+` on a
constrained type should not be forced to `int`).


## GADTs (Generalized Algebraic Data Types)

GADTs let constructors constrain their parent type's type parameters. In
an ordinary variant, every constructor produces the same type -- `Some 42`
and `None` both have type `'a option` with the same `'a`. With GADTs, each
constructor can specify a *particular* instantiation of the parent type.
This enables type-safe evaluators, typed abstract syntax trees, existential
types, and type equality witnesses.

### Syntax

GADT constructors use OCaml-style syntax: the constructor name is followed
by `:` and a full type signature (argument types and return type):

```ocaml
type 'a expr =
  | IntLit : int -> int expr
  | BoolLit : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
```

`IntLit` always returns `int expr`, not `'a expr`. `BoolLit` always returns
`bool expr`. `Add` takes two `int expr` values and returns `int expr`. Only
`If` is polymorphic in the result -- it returns `'a expr` where `'a` matches
the branches.

A type can mix ordinary constructors (no `:`) with GADT constructors, but
in practice most GADT definitions annotate every constructor.

### AST Representation

The `TDVariant` definition carries the GADT return type as an optional third
element in each constructor tuple:

```ocaml
type type_def =
  | TDVariant of (string * ty_annot option * ty_annot option) list
  (*               name,   arg annotation,   GADT return type   *)
  | TDRecord of (bool * string * ty_annot) list
  | TDAlias of ty_annot
```

For ordinary constructors the third element is `None`. For GADT constructors
it is `Some ty_annot` where the annotation is the return type after the last
`->`.

### Type Processing (`process_type_def`)

When `process_type_def` encounters a variant, it checks whether any
constructor has a GADT return annotation. If so, the entire type is
processed in GADT mode. For each GADT constructor, the processing performs
several steps:

**1. Validate the return type.** The return type annotation must be an
application of the type currently being defined. For example, a constructor
in `type 'a expr` must return `<something> expr`. If the return type names
a different type, the typechecker rejects it.

**2. Extract return type parameters.** The type arguments in the return
annotation are resolved. For `IntLit : int -> int expr`, the return type
parameter is `int`. For `If : bool expr * 'a expr * 'a expr -> 'a expr`,
the return type parameter is `'a`.

**3. Identify existential type variables.** An existential type variable is
one that appears in the constructor's argument annotation but does NOT
appear in the return annotation. For example:

```ocaml
type any_show = AnyShow : 'a * ('a -> string) -> any_show
```

Here `'a` appears in the argument (`'a * ('a -> string)`) but not in the
return type (`any_show` has no parameters at all). So `'a` is existential.

**4. Assign TGen indices.** The type parameters of the parent type are
assigned `TGen` indices `0..num_params-1`. Existential type variables are
assigned indices starting from `num_params`. So for a type with one
parameter and one existential, the parameter gets `TGen 0` and the
existential gets `TGen 1`.

**5. Store in `ctor_info`.** The constructor info record is augmented with
two GADT-specific fields:

- `ctor_return_ty_params`: the resolved return type parameters for this
  constructor. For `IntLit : int -> int expr`, this would be `[TInt]`
  instead of the default `[TGen 0]`. For non-GADT constructors, this is
  `None`.
- `ctor_existentials`: the list of existential type variable `TGen` indices.
  For non-GADT constructors, this is empty.

### Construction (`synth_construct`)

When constructing a GADT value, the typechecker uses the constructor's
specific return type parameters instead of fresh type variables. In the
ordinary case, constructing `Some 42` would create fresh type variables for
all of `option`'s parameters and then unify them with the argument. For
GADTs, the return type is already determined by the constructor.

For `IntLit 42`:

1. The constructor `IntLit` has `ctor_return_ty_params = Some [TInt]`.
2. Instead of creating a fresh `'a` for `'a expr`, the typechecker uses
   `TInt` directly.
3. The result type is `int expr`, not `'a expr`.

This is what makes GADT construction precise -- the type flows from the
constructor, not from the context.

### Pattern Matching: Snapshot/Restore

GADT pattern matching is the most involved part of the implementation. The
core challenge is that matching on a GADT constructor reveals type
information that is only valid inside that match arm. Consider:

```ocaml
let eval : 'a expr -> 'a = fun e ->
  match e with
  | IntLit n -> n       (* here 'a = int *)
  | BoolLit b -> b      (* here 'a = bool *)
  | Add (l, r) -> eval l + eval r   (* here 'a = int *)
  | If (c, t, f) -> if eval c do eval t else eval f
```

In the `IntLit n` arm, the typechecker knows `'a = int` because `IntLit`
returns `int expr`. The body `n` (an `int`) is valid as the return type
`'a` because `'a` is locally refined to `int`. But this refinement must not
leak to other arms -- in the `BoolLit` arm, `'a = bool`.

The implementation uses a **snapshot/restore** mechanism on type variable
ref cells:

**Step 1 -- Snapshot.** Before checking each arm, save the current state of
the scrutinee's type parameter tvar ref cells. These are the ref cells that
represent the type variables in the scrutinee's type (e.g., the `'a` in
`'a expr`).

**Step 2 -- Unify.** Unify the scrutinee's type with the constructor's
specific return type. For the `IntLit` arm, this unifies `'a expr` with
`int expr`, which links the `'a` tvar ref cell to `TInt`. This creates
local type equations that are visible inside the arm body.

**Step 3 -- Check the arm body.** With the refined types in place, typecheck
the arm's body expression. In the `IntLit` arm, `n` has type `int` and the
return type `'a` is now `int`, so `n` typechecks as the return value.

**Step 4 -- Freeze the typed AST.** Before restoring, call `deep_repr` and
`freeze_texpr` on the typed AST of the arm body. This walks the typed
expression tree and resolves all tvar links to their current values. Without
this step, restoring the ref cells would retroactively change the types in
the already-checked arm body.

**Step 5 -- Restore.** Reset the saved tvar ref cells to their original
(pre-unification) state. This undoes the local type equations so the next
arm starts with a clean slate.

**Step 6 -- Check existential escape.** If the constructor introduces
existential type variables, verify that they do not appear in the arm's
result type. An existential is only meaningful inside the match arm and
must not leak into the surrounding context.

This snapshot/restore approach avoids the need for full constraint-based
type inference. It is sound because each arm is checked independently with
its own local type refinements, and the freezing step ensures that the
typed AST captures the correct types before the refinements are undone.

### Exhaustiveness Filtering

GADT-aware exhaustiveness checking must account for the fact that some
constructors are impossible for a given scrutinee type. When matching on a
value of type `int expr`, the `BoolLit` constructor is impossible because
`BoolLit` returns `bool expr` and `bool` does not unify with `int`.

The implementation uses a **non-destructive** compatibility check called
`types_compatible`. Unlike full unification, `types_compatible` does not
modify any ref cells -- it only asks "could these types unify?" without
actually performing the unification. This is used to filter the constructor
list before running the standard exhaustiveness algorithm.

For example, given a match on `int expr`:

1. `IntLit` returns `int expr` -- compatible with `int expr`. Keep it.
2. `BoolLit` returns `bool expr` -- `bool` vs `int` is incompatible. Filter
   it out.
3. `Add` returns `int expr` -- compatible. Keep it.
4. `If` returns `'a expr` -- `'a` is compatible with anything. Keep it.

The exhaustiveness checker then only requires arms for `IntLit`, `Add`, and
`If`. Omitting `BoolLit` is not a missing case -- it is provably
unreachable.

### Existential Types

Existential type variables are type variables that appear in a constructor's
argument type but not in its return type. They represent types that "exist"
inside the constructed value but whose identity is hidden from the outside.

```ocaml
type any_show = AnyShow : 'a * ('a -> string) -> any_show
```

The `'a` in `AnyShow` is existential. When you construct
`AnyShow (42, string_of_int)`, the `'a` is `int`, but the resulting value
has type `any_show` with no trace of `int`. The type has been "forgotten."

When you pattern-match on `AnyShow (x, f)`, the typechecker introduces a
fresh type variable for the existential `'a`. Inside the match arm, `x` has
some unknown type and `f` has type `<that same unknown type> -> string`.
You can apply `f x` to get a `string`, but you cannot use `x` as an `int`
-- the connection to `int` was erased at construction time.

The escape check prevents the existential from leaking:

```ocaml
match v with
| AnyShow (x, f) -> x   (* ERROR: existential type escapes *)
```

Returning `x` directly would expose the existential type variable in the
result type. The typechecker detects this and raises an error.

### Type Equality Witnesses

A common GADT pattern is the type equality witness:

```ocaml
type ('a, 'b) eq = Refl : ('a, 'a) eq
```

The `Refl` constructor can only be created when `'a` and `'b` are the same
type. Matching on `Refl` locally refines `'a` and `'b` to be equal, which
is useful for type-safe casting and proving type relationships:

```ocaml
let cast : ('a, 'b) eq -> 'a -> 'b = fun witness x ->
  match witness with
  | Refl -> x    (* 'a = 'b in this branch, so x : 'a is also x : 'b *)
```

This works through the same snapshot/restore mechanism: unifying the
scrutinee type `('a, 'b) eq` with `Refl`'s return type `('a, 'a) eq`
links `'b` to `'a` inside the arm, making the return of `x : 'a` valid
as `'b`.

### Deriving Guard

GADT types cannot use `deriving`. The pattern-matching code generated by
`deriving` assumes all constructors share a uniform return type, which
GADTs violate. If a `deriving` clause is present on a GADT type, the
typechecker raises an error:

```
cannot derive Show for GADT type expr
```

See the [Deriving](#deriving) section for more on the deriving mechanism.

### Module Support

GADTs work inside modules. A GADT defined in a module follows the same
rules as any other type definition -- it is registered under a qualified
name and constructors are exported based on visibility.

The one subtlety is constructor return types. Inside the module definition,
constructors use **short names** for the return type:

```ocaml
module Expr = struct
  type 'a t =
    | IntLit : int -> int t       (* "t", not "Expr.t" *)
    | BoolLit : bool -> bool t
end
```

The return type says `int t`, not `int Expr.t`, because the module body
has not closed yet and the short name `t` is in scope. The implementation
resolves this through the type alias mechanism: when processing a type
inside a module, a type alias mapping the short name to the qualified name
is added (e.g., `"t" -> "Expr.t"`). When the GADT return type annotation
is resolved, `resolve_type_alias` translates `t` to `Expr.t`, and the
constructor is correctly registered against the fully qualified type name.

This alias is scoped to the module body and removed when module processing
completes, so it does not leak into the outer scope. The same mechanism is
used for ordinary types in modules (see [Type Aliases in Modules](#type-aliases-in-modules)).
