# Type Inference

This is part 4 in a series on the MiniML type system implementation. It
assumes you have read the prior parts on type representation, type
variables, and unification. Where those chapters covered the *data
structures* and the *constraint solver*, this chapter covers the
*driver*: the recursive walk over the AST that generates constraints and
produces a fully typed tree.

All typechecker code lives in `lib/typechecker.ml` unless noted
otherwise. Types and unification live in `lib/types.ml`. The untyped AST
is defined in `lib/ast.ml`.

## Bidirectional Type Checking

The typechecker has two entry points for expressions: `synth` and
`check`. Together they form a *bidirectional* type checking system.

**synth** (synthesis mode) takes an expression and *produces* a type:

```ocaml
let rec synth ctx level (expr : Ast.expr) : texpr =
```

**check** (checking mode) takes an expression and an *expected* type,
and verifies that the expression has that type:

```ocaml
and check ctx level (expr : Ast.expr) (expected : Types.ty) : texpr =
```

Both return a `texpr` -- a typed AST node where every subexpression
carries its inferred type.

### When Each Mode Is Used

The general rule: use `synth` when you have no information about what
type an expression should have. Use `check` when context tells you what
type to expect.

Some concrete cases where `check` is used:

- **Function arguments with a known parameter type.** When the
  typechecker has already determined that a function expects an `int`,
  it checks the argument against `int` rather than synthesizing the
  argument's type and then unifying.
- **If/else branches.** The first branch is synthesized, and the
  second branch is checked against the first branch's type.
- **Type annotations.** An `EAnnot(e, ty)` resolves the annotation and
  checks `e` against the resulting type.
- **Operator operands with known types.** Boolean operators check both
  sides against `TBool`. String concatenation checks both sides against
  `TString`.

### The check Function

The `check` function is short. It has one special case and a fallback:

```ocaml
and check ctx level (expr : Ast.expr) (expected : Types.ty) : texpr =
  match expr with
  | Ast.ELoc (loc, inner) -> current_loc := loc; check ctx level inner expected
  | Ast.EFun (param, body) when param.annot = None ->
    let arg_ty = Types.new_tvar level in
    let ret_ty = Types.new_tvar level in
    let fn_eff = Types.new_effvar level in
    try_unify expected (Types.TArrow (arg_ty, fn_eff, ret_ty));
    let ctx' = extend_var_mono ctx param.name (Types.repr arg_ty) in
    let body_te = check ctx' level body ret_ty in
    mk (TEFun (param.name, body_te)) (Types.TArrow (arg_ty, fn_eff, body_te.ty))
  | _ ->
    let te = synth ctx level expr in
    try_unify te.ty expected;
    { te with ty = expected }
```

The special case for unannotated lambdas is the key win of bidirectional
checking. When you write `List.map (fun x -> x + 1) xs`, the
typechecker already knows from `List.map`'s type that the function
argument must be `'a -> 'b`. In check mode, it can *decompose* the
expected type `'a -> 'b` into a parameter type and a return type before
entering the body. This means the parameter `x` gets a type immediately,
without waiting for the body to constrain it.

The decomposition works by creating two fresh type variables, unifying
the expected type with `arg_ty -> ret_ty`, and then checking the body
against `ret_ty`. If the expected type is already an arrow, unification
fills in `arg_ty` and `ret_ty` immediately. If the expected type is
itself a variable, the unification just records the constraint for later.

The fallback case handles everything else: synthesize the type, then
unify with the expected type. This means `check` is always at least as
powerful as `synth` followed by unification, but the special cases
propagate type information *downward* into subexpressions more
effectively.

## Variable Lookup: EVar

Variable lookup is the simplest case in `synth`:

```ocaml
| Ast.EVar name ->
  let ty = lookup_var ctx level name in
  mk (TEVar name) ty
```

The `lookup_var` function finds the variable in the context and
instantiates its scheme:

```ocaml
let lookup_var ctx level name =
  match List.assoc_opt name ctx.vars with
  | Some scheme -> Types.instantiate level scheme
  | None -> error (Printf.sprintf "unbound variable: %s" name)
```

Two things happen here:

1. **Context lookup.** The `ctx.vars` association list is searched for
   the name. Because new bindings are prepended to the front, shadowing
   works automatically -- the most recent binding with the given name is
   found first.

2. **Instantiation.** The stored scheme is instantiated at the current
   level. Recall from the types chapter that a scheme like
   `{ quant = 1; body = TArrow(TGen 0, TGen 0) }` (the identity
   function) has `TGen` placeholders for quantified variables.
   Instantiation replaces each `TGen i` with a fresh `TVar`:

```ocaml
let instantiate level (s : scheme) =
  if s.quant = 0 then s.body
  else begin
    let vars = Array.init s.quant (fun _ -> new_tvar level) in
    let rec go = function
      | TGen id -> vars.(id)
      | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
      | TTuple ts -> TTuple (List.map go ts)
      | TList t -> TList (go t)
      (* ... other cases ... *)
      | t -> t
    in
    go s.body
  end
```

If the scheme is monomorphic (`quant = 0`), the body is returned
directly -- no allocation needed. Otherwise, an array of fresh type
variables is created, one per quantified position, and the body is
walked to replace every `TGen i` with `vars.(i)`.

This is why polymorphism works: each use of a polymorphic variable gets
its own fresh type variables. If `id` has the scheme `forall 'a. 'a -> 'a`,
then `id 1` instantiates to `'b -> 'b` (binding `'b = int`), and
`id "hello"` independently instantiates to `'c -> 'c` (binding
`'c = string`). The two uses do not interfere because they each got
different variables.

## Function Inference: EFun

When the typechecker encounters a lambda in synthesis mode:

```ocaml
| Ast.EFun (param, body) ->
  let param_ty = match param.annot with
    | Some annot -> resolve_ty_annot ctx level annot
    | None -> Types.new_tvar level
  in
  let ctx' = extend_var_mono ctx param.name param_ty in
  let body_te = synth ctx' level body in
  let body_eff = ctx'.current_eff in
  mk (TEFun (param.name, body_te)) (Types.TArrow (param_ty, body_eff, body_te.ty))
```

The steps:

1. **Parameter type.** If the parameter has a type annotation (e.g.
   `fun (x : int) -> ...`), resolve it to a `Types.ty`. Otherwise,
   create a fresh type variable. This is the classic "guess and check"
   approach: we do not know what type `x` has, so we create a variable
   and let the body constrain it.

2. **Extend context.** Add the parameter to the context as a
   monomorphic binding. Function parameters are never polymorphic --
   they get `mono param_ty`, not a generalized scheme. This is the
   *monomorphism of lambda-bound variables* in Hindley-Milner.

3. **Synthesize body.** Recursively infer the type of the body in the
   extended context.

4. **Build result.** The function's type is `TArrow(param_ty, body_ty)`.

Note that `extend_var_mono` uses the `Types.mono` helper, which wraps a
plain type in a scheme with `quant = 0`. This means that if you look up
the parameter inside the body, instantiation is a no-op -- you get back
the same (possibly still-unbound) type variable.

### Check Mode for Functions

The check-mode case was shown above in the `check` function. The key
difference: instead of creating a fresh variable for the parameter and
hoping the body constrains it, check mode *decomposes* the expected type.
If we know the function should have type `int -> string`, the parameter
gets `int` directly and the body is checked against `string`.

This matters for expressions like:

```
let f : int -> int = fun x -> x + 1
```

In synth mode, `x` would get a fresh variable, `x + 1` would constrain
it to `int`, and then the annotation would be checked. In check mode,
the annotation propagates *into* the lambda: `x` is `int` from the
start.

## Application Inference: EApp

Function application synthesizes the function, synthesizes the argument,
and uses unification to connect them:

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

The steps:

1. **Synthesize the function.** Get its type -- it might be an arrow
   type, or it might be a type variable that hasn't been constrained yet.

2. **Synthesize the argument.** Get its type independently.

3. **Create a fresh return type.** We do not know what the function
   returns yet.

4. **Unify.** The function's type must be an arrow from the argument
   type to the return type. This single unification call does several
   things at once:
   - If the function type is already `int -> string` and the argument is
     `int`, unification succeeds and `ret_ty` gets bound to `string`.
   - If the function type is a variable `'a`, it gets bound to
     `arg_ty -> ret_ty`, recording that it must be a function.
   - If the function expects `int` but the argument is `string`,
     unification fails and a type error is reported.

5. **Return.** The application node has the return type.

This is the standard Hindley-Milner approach. The single call to
`try_unify` with a constructed `TArrow` type is the bridge between "we
know this should be a function call" and "these are the constraints on
the types involved."

### The Pipe Operator

The pipe operator `|>` is syntactic sugar for reversed application. It
is handled as a binary operator:

```ocaml
| Ast.Pipe ->
  let te1 = synth ctx level e1 in
  let te2 = synth ctx level e2 in
  let ret_ty = Types.new_tvar level in
  let call_eff = Types.new_effvar level in
  try_unify te2.ty (Types.TArrow (te1.ty, call_eff, ret_ty));
  try_subeffect call_eff ctx.current_eff;
  mk (TEBinop (op, te1, te2)) ret_ty
```

The left operand is the argument. The right operand is the function. The
unification constraint is the same as `EApp` but with the sides flipped:
`e2` must be a function that accepts `e1`'s type.

## Let Binding Inference: ELet

Let bindings are where polymorphism enters the picture:

```ocaml
| Ast.ELet (name, e1, e2) ->
  let e1_te = synth ctx (level + 1) e1 in
  let scheme = Types.generalize level e1_te.ty in
  let ctx' = extend_var ctx name scheme in
  let e2_te = synth ctx' level e2 in
  mk (TELet (name, e1_te, e2_te)) e2_te.ty
```

The steps:

1. **Increase the level.** The binding expression `e1` is inferred at
   `level + 1`. This is critical for generalization: any type variables
   created at the deeper level that do not escape to the outer level can
   be generalized.

2. **Synthesize the binding.** Infer the type of `e1` at the deeper
   level.

3. **Generalize.** The `generalize` function walks the inferred type and
   replaces unbound type variables whose level is greater than `level`
   with `TGen` indices, producing a scheme:

```ocaml
let generalize level ty =
  let id_map = Hashtbl.create 8 in
  let counter = ref 0 in
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
    | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
    (* ... other cases ... *)
    | (TInt | TFloat | TBool | TString | TByte | TRune | TUnit | TGen _) as t -> t
  in
  let body = go ty in
  { quant = !counter; constraints = []; body }
```

   A variable at level `> level` was created during the inference of
   `e1` and did not escape (if it had escaped, the occurs check during
   unification would have adjusted its level down). Such variables are
   safe to generalize. Variables at level `<= level` were created in an
   outer scope and must not be generalized -- they may still be
   constrained by later code.

   The `id_map` hash table ensures that the same type variable always
   maps to the same `TGen` index. If the inferred type is `'a -> 'a`,
   both occurrences of `'a` get `TGen 0`.

4. **Extend context.** Add the name to the context with the generalized
   scheme.

5. **Synthesize the body.** Infer the type of `e2` at the original
   level. Every reference to `name` in `e2` will instantiate the scheme
   with fresh variables.

### Why Level + 1?

The level-based approach is the key optimization over the naive
"generalize everything not in the environment" strategy used in textbook
presentations. Instead of scanning the entire typing environment to find
which variables are free, we use the level as a cheap proxy: a variable
is generalizable if and only if its level is deep enough.

Consider:

```
let id = fun x -> x in
let a = id 1 in
let b = id "hello" in
(a, b)
```

When typechecking `fun x -> x` at level 2 (let is at level 1, so the
binding is at level 2), `x` gets a fresh variable at level 2. After
inference, the type is `'a -> 'a` where `'a` is at level 2. Since
`2 > 1`, it gets generalized. Each use of `id` then gets its own
instance.

## Let Rec: Recursive Functions

Recursive let bindings require special handling because the name being
defined must be in scope during the inference of its own body:

```ocaml
| Ast.ELetRec (name, e1, e2) ->
  (match strip_loc e1 with
   | Ast.EFun _ | Ast.EAnnot (Ast.EFun _, _) ->
     let fn_var = Types.new_tvar (level + 1) in
     let ctx' = extend_var_mono ctx name fn_var in
     let e1_te = synth ctx' (level + 1) e1 in
     try_unify fn_var e1_te.ty;
     let scheme = Types.generalize level e1_te.ty in
     let ctx'' = extend_var ctx name scheme in
     let e2_te = synth ctx'' level e2 in
     mk (TELetRec (name, e1_te, e2_te)) e2_te.ty
   | _ -> error "let rec binding must be a function")
```

The steps:

1. **Verify it is a function.** Recursive bindings must be lambda
   expressions. Non-function recursion (like `let rec x = x + 1`) is
   rejected.

2. **Create a type variable for the name.** Before typechecking the body
   `e1`, we create a fresh variable `fn_var` and add it to the context
   under the name being defined. This is the "guess" -- we do not know
   the function's type yet, so we use a variable.

3. **Synthesize the body.** The body can reference `name` and will see
   the fresh variable `fn_var`. When the function calls itself
   recursively, it uses this provisional type.

4. **Unify.** After inference, we unify `fn_var` with the actual
   inferred type of `e1`. This closes the loop: the type we guessed for
   the recursive reference must be consistent with the type we actually
   inferred.

5. **Generalize and continue.** Same as non-recursive let.

### Mutual Recursion: ELetRecAnd

Mutually recursive bindings follow the same pattern, extended to
multiple names:

```ocaml
| Ast.ELetRecAnd (bindings, body) ->
  let fn_vars = List.map (fun (name, _) ->
    (name, Types.new_tvar (level + 1))) bindings in
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
  let typed_bindings = List.map2 (fun (name, _) te ->
    (name, te)) bindings body_tes in
  mk (TELetRecAnd (typed_bindings, body_te)) body_te.ty
```

All names get fresh type variables. All names are added to the context.
All bodies are typechecked in the shared context. All are unified with
their provisional variables. All are generalized together.

## If/Do/Else: EIf

Conditional expressions are straightforward:

```ocaml
| Ast.EIf (cond, then_e, else_e) ->
  let cond_te = check ctx level cond Types.TBool in
  let then_te = synth ctx level then_e in
  let else_te = check ctx level else_e then_te.ty in
  mk (TEIf (cond_te, then_te, else_te)) then_te.ty
```

Three constraints are enforced:

1. The condition must be `bool`. This uses `check` mode -- we know the
   expected type, so we push it down.

2. The first branch (after `do`) is synthesized. Its type becomes the
   type of the whole expression.

3. The else-branch is checked against the first branch's type. This is
   where bidirectional checking shines: if the first branch returns `int`,
   the else-branch is checked against `int`, which propagates into any
   subexpressions.

If the two branches have incompatible types, the `check` on the
else-branch will fail during unification with a message like "cannot
unify int with string".

## Pattern Matching: EMatch

Match expressions are the most complex inference case. They combine
scrutinee inference, pattern checking, guard checking, branch unification,
and exhaustiveness analysis:

```ocaml
and synth_match ctx level scrut arms partial =
  let scrut_te = synth ctx level scrut in
  let check_arm ctx (pat, guard, body) =
    let bindings = check_pattern ctx level pat scrut_te.ty in
    let ctx' = List.fold_left (fun c (n, t) ->
      extend_var_mono c n t) ctx bindings in
    let guard_te = match guard with
      | Some g ->
        let gte = check ctx' level g Types.TBool in
        Some gte
      | None -> None
    in
    let body_te = synth ctx' level body in
    (pat, guard_te, body_te)
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
    if not partial then
      !exhaustiveness_check_ref ctx (Types.repr scrut_te.ty) all_arms;
    mk (TEMatch (scrut_te, all_arms, partial)) result_ty
```

Walking through the algorithm:

1. **Synthesize the scrutinee.** Infer the type of the value being
   matched.

2. **Check each arm.** For each `(pattern, guard, body)` triple:
   - Run `check_pattern` to verify the pattern is compatible with the
     scrutinee's type and extract variable bindings.
   - Extend the context with the pattern's bindings.
   - If there is a guard, check it against `bool`.
   - Synthesize the body in the extended context.

3. **Unify branches.** The first arm's body type becomes the result type.
   Every subsequent arm's body type is unified with it. All branches
   must agree.

4. **Exhaustiveness check.** If the match is not marked as partial, run
   the exhaustiveness checker to ensure all possible values are covered.

### Pattern Checking

The `check_pattern` function takes a pattern and a type, unifies the
pattern's structure with the type, and returns a list of variable
bindings:

```ocaml
and check_pattern ctx level (pat : Ast.pattern) (ty : Types.ty)
    : (string * Types.ty) list =
  match pat with
  | Ast.PatWild -> []
  | Ast.PatVar name -> [(name, ty)]
  | Ast.PatInt _ -> try_unify ty Types.TInt; []
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
    try_unify ty (Types.TList elem_ty); []
  (* ... more cases ... *)
```

Key design choices:

- **Wildcard** returns no bindings and imposes no constraints.
- **Variable** returns a single binding with whatever type the pattern
  is being checked against. The variable gets the full scrutinee type.
- **Literal patterns** (int, bool, string, unit) unify the scrutinee
  type with the literal's type and return no bindings.
- **Tuple patterns** create fresh variables for each component, unify
  the scrutinee with a tuple of those variables, then recursively check
  each sub-pattern.
- **Cons patterns** create a fresh variable for the element type, unify
  the scrutinee with a list of that type, then check the head pattern
  against the element type and the tail pattern against the full list
  type.

**Constructor patterns** are more involved because they must look up the
constructor in the type environment and instantiate its type parameters:

```ocaml
| Ast.PatConstruct (name, arg_pat) ->
  (match List.assoc_opt name ctx.type_env.constructors with
   | None -> error (Printf.sprintf "unknown constructor in pattern: %s" name)
   | Some info ->
     let fresh_args = List.init info.ctor_num_params
       (fun _ -> Types.new_tvar level) in
     let expected_variant = Types.TVariant (info.ctor_type_name, fresh_args) in
     try_unify ty expected_variant;
     (match info.ctor_arg_ty, arg_pat with
      | None, None -> []
      | Some arg_ty, Some p ->
        let concrete_ty = subst_tgens fresh_args arg_ty in
        check_pattern ctx level p concrete_ty
      | None, Some _ -> error (...)
      | Some _, None -> error (...)))
```

The constructor's type parameters are instantiated with fresh variables.
The scrutinee type is unified with the expected variant type. If the
constructor carries a payload, the `TGen` indices in the stored argument
type are substituted with the fresh variables, and the sub-pattern is
checked against the resulting concrete type.

**Or-patterns** check both alternatives against the same type and verify
they bind the same set of variables:

```ocaml
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
```

**Record patterns** try to determine the record type, either from the
scrutinee's record row or by searching the type environment for a record
that has the mentioned fields:

```ocaml
| Ast.PatRecord field_pats ->
  let field_tys = match Types.repr ty with
    | Types.TRecord row ->
      Types.record_row_to_fields row
    | _ ->
      let pat_field_names = List.map fst field_pats in
      let candidates = List.filter (fun (_name, fields) ->
        List.for_all (fun fn -> List.mem_assoc fn fields) pat_field_names
      ) ctx.type_env.Types.records in
      (match candidates with
       | [(_name, fields)] ->
         let fields = instantiate_record_fields level fields in
         let record_ty = Types.TRecord (Types.fields_to_closed_row fields) in
         try_unify ty record_ty;
         fields
       | [] -> error "record pattern used with non-record type"
       | _ -> error "ambiguous record pattern -- annotate the type")
  in
  (* ... check each field pattern ... *)
```

## Binary and Unary Operators

Operators are handled in dedicated helper functions. There are several
families with different typing strategies.

### Arithmetic Operators: +, -, *, /

These operators are overloaded for `int` and `float`:

```ocaml
and synth_binop ctx level op e1 e2 =
  match op with
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
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
```

The logic after unifying the two operands:

1. If either operand resolved to `float`, the result is `float`.
2. If the type is still an unbound variable, check whether it is under a
   typeclass constraint (the `constraint_tvars` list). If it is, leave
   it polymorphic -- the constraint will resolve it later via dictionary
   passing. If it is not constrained, default to `int`.
3. If the type resolved to `int`, the result is `int`.
4. For any other resolved type (a custom type with arithmetic via
   typeclasses), use the resolved type.

The `constraint_tvars` mechanism is how typeclass-constrained functions
work. When you write:

```
let double (x : 'a) : 'a = x + x where Num 'a
```

The type variable for `'a` gets its ID added to `constraint_tvars`.
When the typechecker encounters `x + x`, it sees that the operand type
variable is in the constraint list and does *not* default it to `int`.
This keeps `'a` polymorphic through generalization.

### Comparison Operators: <, >, <=, >=, ==, !=

These synthesize both operands, unify them (they must be the same type),
and return `bool`:

```ocaml
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
```

### Boolean Operators: &&, ||

Both operands must be `bool`, result is `bool`:

```ocaml
| Ast.And | Ast.Or ->
  let te1 = check ctx level e1 Types.TBool in
  let te2 = check ctx level e2 Types.TBool in
  mk (TEBinop (op, te1, te2)) Types.TBool
```

### String Concatenation: ^

Both operands must be `string`, result is `string`:

```ocaml
| Ast.Concat ->
  let te1 = check ctx level e1 Types.TString in
  let te2 = check ctx level e2 Types.TString in
  mk (TEBinop (op, te1, te2)) Types.TString
```

### Bitwise Operators: land, lor, lxor, lsl, lsr

Same overloading logic as arithmetic: default to `int` unless under a
typeclass constraint.

### Unary Operators

Negation is overloaded for `int` and `float` with the same constraint
tvar logic as addition. Logical not requires `bool`. Bitwise not
defaults to `int`:

```ocaml
and synth_unop ctx level op e =
  match op with
  | Ast.Neg ->
    let te = synth ctx level e in
    let resolved = Types.repr te.ty in
    (match resolved with
     | Types.TFloat -> mk (TEUnop (op, te)) Types.TFloat
     | Types.TVar { contents = Types.Unbound (id, _) } ->
       if List.mem id ctx.constraint_tvars then
         mk (TEUnop (op, te)) (Types.repr te.ty)
       else begin
         try_unify te.ty Types.TInt;
         mk (TEUnop (op, te)) Types.TInt
       end
     | Types.TInt -> mk (TEUnop (op, te)) Types.TInt
     | _ -> mk (TEUnop (op, te)) te.ty)
  | Ast.Not ->
    let te = check ctx level e Types.TBool in
    mk (TEUnop (op, te)) Types.TBool
  | Ast.Lnot ->
    let te = synth ctx level e in
    (* ... defaults to int ... *)
```

## Records

### Construction: ERecord

Record construction synthesizes each field value and builds a closed
record row type:

```ocaml
| Ast.ERecord fields ->
  let typed_fields = List.map (fun (n, e) ->
    let te = synth ctx level e in (n, te)
  ) fields in
  let field_tys = List.map (fun (n, te) -> (n, te.ty)) typed_fields in
  let row = Types.fields_to_closed_row field_tys in
  mk (TERecord typed_fields) (Types.TRecord row)
```

The `fields_to_closed_row` helper converts the field list into a chain
of `RRow` entries ending with `REmpty`, producing a closed record type.
Record literals always produce closed types -- the set of fields is
exactly what was written.

### Field Access: EField

Field access looks at the synthesized type of the expression. If it is
already a known record row, the field is looked up directly via
`rewrite_record_row`. If the type is still a variable, the typechecker
searches the type environment for registered record types that have
the field, or falls back to row-polymorphic structural typing:

```ocaml
| Ast.EField (e, field) ->
  let te = synth ctx level e in
  (match Types.repr te.ty with
   | Types.TRecord row ->
     (* Use row rewriting to find the field *)
     let (field_ty, _rest) = Types.rewrite_record_row field row in
     mk (TEField (te, field)) field_ty
   | Types.TVar _ ->
     let candidates = List.filter (fun (_name, fields) ->
       List.mem_assoc field fields
     ) ctx.type_env.Types.records in
     (match candidates with
      | [(_name, fields)] ->
        let fields = instantiate_record_fields level fields in
        let record_ty = Types.TRecord (Types.fields_to_closed_row fields) in
        try_unify te.ty record_ty;
        ...
      | _ ->
        (* Multiple or no candidates -- use structural typing *)
        let field_ty = Types.new_tvar level in
        let row = Types.RRow (field, field_ty, Types.new_rvar level) in
        try_unify te.ty (Types.TRecord row);
        mk (TEField (te, field)) field_ty)
   | _ -> error "field access on non-record type")
```

Three resolution strategies:

1. **Known record row.** Use `rewrite_record_row` to find the field in
   the row. If the row is open (ends with `RVar`), the field is added
   to the row automatically.
2. **Unique registered record.** If exactly one record type in the
   environment has this field, unify the expression's type with that
   record type. This also handles parametric record types through
   `instantiate_record_fields`, which replaces `TGen` placeholders with
   fresh variables.
3. **Structural typing fallback.** Create an open record row
   `{ field: 'a; .. }` (with a fresh `RVar` tail) and unify. The row
   variable allows the actual record to have additional fields.

### Field Assignment: EFieldAssign

Field assignment follows the same pattern as field access but
additionally checks that the field is declared mutable:

```ocaml
| Ast.EFieldAssign (record_expr, field, value_expr) ->
  let r_te = synth ctx level record_expr in
  let fields = (* ... resolve record type, same logic as EField ... *) in
  (match List.assoc_opt field fields with
   | Some field_ty ->
     if not (List.mem field ctx.type_env.mutable_fields) then
       error (Printf.sprintf "field %s is not mutable" field);
     let v_te = check ctx level value_expr field_ty in
     mk (TEFieldAssign (r_te, field, v_te)) Types.TUnit
   | None -> error (...))
```

The result type is always `unit` -- assignment is a side effect.

### Record Update: ERecordUpdate

Functional record update `{ r with x = 1 }` is desugared during
typechecking into a let binding and a fresh record construction:

```ocaml
| Ast.ERecordUpdate (base, overrides) ->
  let base_te = synth ctx level base in
  (* ... verify all override fields exist ... *)
  (* ... typecheck overrides against expected field types ... *)
  (* Desugar: let __rec_upd = base in { f1 = override1; f2 = __rec_upd.f2; ... } *)
```

The base expression is bound to a temporary variable. A new record is
built where overridden fields use the new values and non-overridden
fields are projected from the temporary.

## Constructors and Variants: EConstruct

Constructor application looks up the constructor in the type environment
and instantiates its type parameters:

```ocaml
and synth_construct ctx level name arg =
  match List.assoc_opt name ctx.type_env.constructors with
  | None -> error (Printf.sprintf "unknown constructor: %s" name)
  | Some info ->
    let fresh_args = List.init info.ctor_num_params
      (fun _ -> Types.new_tvar level) in
    let result_ty = Types.TVariant (info.ctor_type_name, fresh_args) in
    (match info.ctor_arg_ty, arg with
     | None, None ->
       mk (TEConstruct (name, None)) result_ty
     | Some expected_ty, Some arg_expr ->
       let concrete_ty = subst_tgens fresh_args expected_ty in
       let arg_te = check ctx level arg_expr concrete_ty in
       mk (TEConstruct (name, Some arg_te)) result_ty
     | None, Some _ ->
       error (Printf.sprintf "constructor %s takes no arguments" name)
     | Some expected_ty, None ->
       (* Constructor used as a function *)
       let concrete_ty = subst_tgens fresh_args expected_ty in
       let param = "__x" in
       let param_var = mk (TEVar param) concrete_ty in
       let body = mk (TEConstruct (name, Some param_var)) result_ty in
       mk (TEFun (param, body)) (Types.TArrow (concrete_ty, Types.EffEmpty, result_ty)))
```

The steps:

1. **Look up the constructor.** The `ctor_info` tells us which type it
   belongs to, what argument type it expects (if any, stored with `TGen`
   indices), and how many type parameters the parent type has.

2. **Create fresh type arguments.** If the type is `'a option`, one
   fresh variable is created.

3. **Build the result type.** For `Some`, this is
   `TVariant("option", [fresh_var])`.

4. **Handle the argument.** If the constructor takes an argument, the
   stored argument type (e.g. `TGen 0` for `Some`) is substituted with
   the fresh variables (using `subst_tgens`), yielding the concrete
   expected type. The argument expression is then checked against it.

5. **Constructor as function.** If a constructor that takes an argument
   is used without one (e.g. passing `Some` as a function argument), a
   lambda wrapper is synthesized: `fun __x -> Some __x`.

## Polymorphic Variants: EPolyVariant

Polymorphic variant construction is simpler than named constructors because
there is no type environment lookup -- the type is built structurally from
the tag:

```ocaml
| Ast.EPolyVariant (tag, None) ->
  let tail = new_pvvar level in
  let row = Types.PVRow (tag, None, tail) in
  mk (TEConstruct ("`" ^ tag, None)) (Types.TPolyVariant row)

| Ast.EPolyVariant (tag, Some arg) ->
  let arg_te = synth ctx level arg in
  let tail = new_pvvar level in
  let row = Types.PVRow (tag, Some arg_te.ty, tail) in
  mk (TEConstruct ("`" ^ tag, Some arg_te)) (Types.TPolyVariant row)
```

Each poly variant expression creates an open row type (ending with a fresh
`PVVar`). This means `` `Foo `` has type `` [> `Foo] `` -- a poly variant
with at least the `Foo` tag. The open tail allows this value to unify with
any poly variant type that includes `Foo`.

For tags with payloads, the argument is synthesized and its type is stored
in the row entry. `` `Bar 42 `` gets type `` [> `Bar of int] ``.

Note that poly variant expressions are compiled as `TEConstruct` nodes
with the tag name prefixed by a backtick. The compiler uses this prefix
to distinguish poly variant tags from named constructors and generates
hash-based tag values instead of declared constructor tags.

### Type Coercion: `:>`

Poly variant values can be widened using the `:>` coercion operator:

```
(`Foo : [`Foo | `Bar] :> [`Foo | `Bar | `Baz])
```

This asserts that the source type is a subtype of the target type (all
tags in the source are present in the target).

## Lists and Cons: ECons, ENil, EList

List construction and nil:

```ocaml
| Ast.ENil ->
  mk TENil (Types.TList (Types.new_tvar level))

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
```

- **Nil** gets `TList 'a` with a fresh variable. The element type is
  determined by context.
- **Cons** synthesizes the head, builds the list type from it, and
  checks the tail against that list type.
- **List literals** synthesize the first element, check all remaining
  elements against the first element's type, and desugar into a chain of
  cons cells.

## Sequences: ESeq

The semicolon operator evaluates two expressions in order. The first
expression's value is discarded:

```ocaml
| Ast.ESeq (e1, e2) ->
  let e1_te = synth ctx level e1 in
  let e2_te = synth ctx level e2 in
  mk (TESeq (e1_te, e2_te)) e2_te.ty
```

The first expression is synthesized but its type is not constrained to
be `unit`. The result type is the second expression's type. This is a
deliberate design choice -- some languages require the left side of `;`
to be `unit`, but MiniML does not enforce this at the type level.

## Mutable Bindings: ELetMut, EAssign

### Let Mut

Mutable variable introduction:

```ocaml
| Ast.ELetMut (name, e1, e2) ->
  let e1_te = synth ctx (level + 1) e1 in
  let scheme = { Types.quant = 0; constraints = []; body = e1_te.ty } in
  let ctx' = extend_var_mutable ctx name scheme in
  let e2_te = synth ctx' level e2 in
  mk (TELetMut (name, e1_te, e2_te)) e2_te.ty
```

The critical difference from `ELet`: **mutable variables are never
generalized**. The scheme always has `quant = 0`. This is the *value
restriction* -- since a mutable variable's contents can be changed, it
would be unsound to let different use sites see different instantiations.
If `let mut x = []` were generalized, you could do `x <- [1]` and then
`List.hd x = "hello"`, which would be a type error at runtime.

The `extend_var_mutable` helper adds the name to both `ctx.vars` (with
the monomorphic scheme) and `ctx.mutable_vars` (the list of names known
to be mutable).

### Assignment

Assignment checks that the target is mutable and that the value matches
the variable's type:

```ocaml
| Ast.EAssign (name, e) ->
  if not (List.mem name ctx.mutable_vars) then
    error (Printf.sprintf "variable %s is not mutable" name);
  let var_ty = lookup_var ctx level name in
  let e_te = check ctx level e var_ty in
  mk (TEAssign (name, e_te)) Types.TUnit
```

The value is checked against the variable's existing type (not
synthesized and unified -- this ensures assignment cannot change a
variable's type). The result is `unit`.

## Loops: While, While-let, For, For/Fold

### While

While loops check the condition against `bool` and the body against
`unit`. The context is updated with `loop_info = Some WhileLoop` so
that `break` and `continue` are valid inside the body:

```ocaml
| Ast.EWhile (cond, body) ->
  let ctx' = { ctx with loop_info = Some WhileLoop } in
  let cond_te = check ctx' level cond Types.TBool in
  let body_te = check ctx' level body Types.TUnit in
  mk (TEWhile (cond_te, body_te)) Types.TUnit
```

The whole expression has type `unit`. At the compiler level, while loops
emit `ENTER_LOOP`/`EXIT_LOOP` instructions that push and pop control
stack frames, enabling native `break` (via `LOOP_BREAK`) and `continue`
support without effect handlers.

### While-let

While-let loops are desugared into an infinite while loop with pattern
matching:

```ocaml
| Ast.EWhileLet (pat, scrutinee, body) ->
  let desugared = Ast.EWhile (Ast.EBool true,
    Ast.EMatch (scrutinee,
      [(pat, None, body);
       (Ast.PatWild, None, Ast.EBreak None)],
      false)) in
  synth ctx level desugared
```

`for let pat = expr do body end` becomes
`for true do match expr with | pat -> body | _ -> break end`. The
typechecker never sees a special while-let construct -- it just infers
the desugared form.

### For (Iterator For)

The iterator-style `for x in coll do body end` loop is desugared into
a `fold` call wrapped in `TEForLoop`:

```ocaml
| Ast.EFor (var_name, coll_expr, body_expr) ->
  let ctx' = { ctx with loop_info = Some UnitLoop } in
  let desugared = Ast.EApp(Ast.EApp(Ast.EApp(Ast.EVar "fold",
    Ast.EFun({name="_"; annot=None},
      Ast.EFun({name=var_name; annot=None},
        Ast.ESeq(body_expr, Ast.EUnit)))),
    Ast.EUnit),
    coll_expr) in
  let fold_te = synth ctx' level desugared in
  mk (TEForLoop fold_te) Types.TUnit
```

The desugaring strategy:

1. Set `loop_info = Some UnitLoop` so that `break` and `continue` know
   this is a unit loop.
2. Build a `fold` call that wraps the body in a function ignoring the
   accumulator (since this is a unit loop, not a fold loop). The
   accumulator is `()`.
3. Wrap the typed fold in `TEForLoop`. The compiler emits
   `ENTER_LOOP`/`EXIT_LOOP` around the fold, and `break` compiles to
   `TEBreak` which emits `LOOP_BREAK` -- a native control stack
   operation rather than an effect handler.

### For/Fold

The `EForFold` variant is similar but uses a named accumulator instead
of `()`:

```ocaml
| Ast.EForFold (var_name, coll_expr, acc_name, init_expr, body_expr) ->
  let ctx' = { ctx with loop_info = Some (FoldLoop acc_name) } in
  let desugared = Ast.EApp(Ast.EApp(Ast.EApp(Ast.EVar "fold",
    Ast.EFun({name=acc_name; annot=None},
      Ast.EFun({name=var_name; annot=None},
        body_expr))),
    init_expr),
    coll_expr) in
  let fold_te = synth ctx' level desugared in
  mk (TEForLoop fold_te) fold_te.ty
```

The result type is the fold's return type (the accumulator type), not
`unit`. Break with a value is allowed in fold loops, returning the
break value as the loop result.

### Break

The `EBreak` node compiles to `TEBreak`, which the compiler emits as
`LOOP_BREAK`. This is a native control stack operation -- the VM
unwinds the control stack to the nearest `ENTER_LOOP` frame. No effect
handler or `__Break` effect is involved:

```ocaml
| Ast.EBreak value_opt ->
  (match ctx.loop_info with
   | None -> error "break outside of loop"
   | Some WhileLoop ->
     mk (TEBreak (mk TEUnit Types.TUnit)) Types.TUnit
   | Some UnitLoop ->
     mk (TEBreak (mk TEUnit Types.TUnit)) Types.TUnit
   | Some (FoldLoop acc_name) ->
     let v_te = match value_opt with
       | Some e -> synth ctx level e
       | None -> synth ctx level (Ast.EVar acc_name) in
     mk (TEBreak v_te) v_te.ty)
```

In while and unit loops, break carries `()`. In fold loops, break
carries an explicit value or defaults to the current accumulator.

### Continue

Continue has two typed representations depending on the loop kind.
In while loops, `TEContinueLoop` restarts the loop iteration. In
for/fold loops, `TEFoldContinue` passes a value back as the new
accumulator:

```ocaml
| Ast.EContinueLoop ->
  (match ctx.loop_info with
   | None -> error "continue outside of loop"
   | Some WhileLoop -> mk TEContinueLoop Types.TUnit
   | Some UnitLoop -> mk (TEFoldContinue (mk TEUnit Types.TUnit)) Types.TUnit
   | Some (FoldLoop acc_name) ->
     let acc_te = synth ctx level (Ast.EVar acc_name) in
     mk (TEFoldContinue acc_te) acc_te.ty)
```

## Type Annotations: EAnnot

Type annotations resolve the annotation and switch to check mode:

```ocaml
| Ast.EAnnot (e, annot) ->
  let ty = resolve_ty_annot ctx level annot in
  let te = check ctx level e ty in
  te
```

The `resolve_ty_annot` function converts an `Ast.ty_annot` (the
syntactic annotation) into a `Types.ty`:

```ocaml
let resolve_ty_annot ctx level (annot : Ast.ty_annot) : Types.ty =
  let tvars = Hashtbl.create 4 in
  resolve_ty_annot_shared ctx level tvars annot
```

The shared implementation handles all annotation forms:

- `TyName "int"` becomes `Types.TInt`.
- `TyVar "a"` creates a fresh type variable (or reuses one if the same
  name appeared earlier in the same annotation). The `tvars` hash table
  ensures that `'a -> 'a` produces `TVar(x) -> TVar(x)` with the same
  variable in both positions.
- `TyArrow(a, b)` becomes `Types.TArrow(resolve a, resolve b)`.
- `TyApp(args, name)` looks up the type name in the environment. If it
  is a type synonym, the synonym is expanded with the arguments
  substituted in. If it is a variant, a `TVariant` is constructed.
- `TyRecord fields` produces a `TRecord` with sorted fields.

### Constrained Functions and Where Clauses

When a top-level function has a `where` clause (typeclass constraints),
a special path is taken that uses shared type variables across all
parameter annotations:

```ocaml
let synth_constrained_fn ctx level constraints params ret_annot body =
  let shared_tvars = Hashtbl.create 4 in
  (* Pre-allocate tvars for constraint type variables *)
  let constraint_tyvar_names = ... in
  List.iter (fun tvname ->
    Hashtbl.replace shared_tvars tvname (Types.new_tvar (level + 1))
  ) constraint_tyvar_names;
  let constraint_tvar_ids = ... in
  (* Resolve param types with shared tvars *)
  let param_tys = List.map (fun p ->
    match p.Ast.annot with
    | Some annot -> resolve_ty_annot_shared ctx (level + 1) shared_tvars annot
    | None -> Types.new_tvar (level + 1)
  ) params in
  (* Build ctx with params and constraint_tvars *)
  let inner_ctx = List.fold_left2 (fun c p ty ->
    extend_var_mono c p.Ast.name ty
  ) { ctx with constraint_tvars = constraint_tvar_ids @ ctx.constraint_tvars }
    params param_tys in
  (* Typecheck body *)
  let body_te = match ret_ty_opt with
    | Some ret_ty -> check inner_ctx (level + 1) body ret_ty
    | None -> synth inner_ctx (level + 1) body
  in
  (* ... build result ... *)
```

The shared tvars table ensures that if parameter annotations use `'a` in
multiple places, they all refer to the same type variable. The
constraint tvar IDs are added to the context so that arithmetic
operators on constrained type variables do not default to `int`.

## Tuples and Collections

### Tuples

Tuple construction synthesizes each element:

```ocaml
| Ast.ETuple exprs ->
  let tes = List.map (synth ctx level) exprs in
  let tys = List.map (fun te -> te.ty) tes in
  mk (TETuple tes) (Types.TTuple tys)
```

### Arrays

Array literals synthesize the first element and check the rest against
it (same strategy as list literals):

```ocaml
| Ast.EArray (first :: rest) ->
  let first_te = synth ctx level first in
  let rest_tes = List.map (fun e -> check ctx level e first_te.ty) rest in
  mk (TEArray (first_te :: rest_tes)) (Types.TArray first_te.ty)
| Ast.EArray [] ->
  mk (TEArray []) (Types.TArray (Types.new_tvar level))
```

### Maps

Map literals synthesize the first key-value pair and check the rest
against those types:

```ocaml
| Ast.EMap ((k1, v1) :: rest) ->
  let k1_te = synth ctx level k1 in
  let v1_te = synth ctx level v1 in
  let rest_tes = List.map (fun (k, v) ->
    let kt = check ctx level k k1_te.ty in
    let vt = check ctx level v v1_te.ty in
    (kt, vt)
  ) rest in
  mk (TEMap ((k1_te, v1_te) :: rest_tes)) (Types.TVariant ("map", [k1_te.ty; v1_te.ty]))
```

### Indexing

Indexing requires the index to be `int` and the base to be either a
string (returning `byte`) or an array (returning the element type):

```ocaml
| Ast.EIndex (base, idx) ->
  let te_idx = check ctx level idx Types.TInt in
  let te_base = synth ctx level base in
  let result_ty = match Types.repr te_base.ty with
    | Types.TString -> Types.TByte
    | Types.TArray t -> t
    | _ -> error "indexing requires string or array"
  in
  mk (TEIndex (te_base, te_idx)) result_ty
```

## Literals

Literals are the simplest cases. Each one produces a typed node with the
obvious type:

```ocaml
| Ast.EInt n -> mk (TEInt n) Types.TInt
| Ast.EFloat f -> mk (TEFloat f) Types.TFloat
| Ast.EBool b -> mk (TEBool b) Types.TBool
| Ast.EString s -> mk (TEString s) Types.TString
| Ast.EByte n -> mk (TEByte n) Types.TByte
| Ast.ERune n -> mk (TERune n) Types.TRune
| Ast.EUnit -> mk TEUnit Types.TUnit
```

No unification needed. No type variables created. The `mk` helper
builds a `texpr`:

```ocaml
let mk te ty = { expr = te; ty }
```

## Source Locations

Location nodes are transparent to inference. Both `synth` and `check`
simply record the location (for error messages) and recurse into the
inner expression:

```ocaml
| Ast.ELoc (loc, inner) -> current_loc := loc; synth ctx level inner
```

```ocaml
| Ast.ELoc (loc, inner) -> current_loc := loc; check ctx level inner expected
```

The `current_loc` ref is a global that the `error` function reads to
attach source positions to error messages.

## Putting It All Together

Here is how all the pieces connect for a non-trivial expression. Consider:

```
let map f xs =
  match xs with
  | [] -> []
  | x :: rest -> f x :: map f rest
```

**Step 1: ELetRec.** This is a recursive binding. A fresh variable
`'a` is created for `map` and added to the context at `level + 1`.

**Step 2: EFun (f).** In synth mode, `f` gets a fresh variable `'b`.
The context is extended with `f : 'b`.

**Step 3: EFun (xs).** Another fresh variable `'c`. Context extended
with `xs : 'c`.

**Step 4: EMatch.** The scrutinee `xs` is synthesized, yielding type
`'c`.

**Step 5: Pattern [] (PatNil).** Unifies `'c` with `'d list` (fresh
`'d`). Now `'c = 'd list`.

**Step 6: Body [].** `ENil` produces `'e list`. The first branch's
type is `'e list`.

**Step 7: Pattern x :: rest (PatCons).** Unifies `'c` with `'f list`
(fresh `'f`). Since `'c = 'd list`, this unifies `'d` with `'f`. Binds
`x : 'f` and `rest : 'f list`.

**Step 8: Body f x :: map f rest.**
- `f x` is an application: synth `f` gives `'b`, synth `x` gives `'f`,
  create fresh `'g`, unify `'b` with `'f -> 'g`. Now `'b = 'f -> 'g`.
- `map f rest`: synth `map` gives `'a` (the recursive variable), synth
  `f` gives `'f -> 'g`, synth `rest` gives `'f list`. Two applications
  constrain `'a = ('f -> 'g) -> 'f list -> 'h` for fresh `'h`.
- The cons `::` constrains: head is `'g`, tail is `'h`, so
  `'h = 'g list`.
- The second branch type is `'g list`. Unify with first branch `'e list`:
  `'e = 'g`.

**Step 9: Unify and generalize.** The recursive variable `'a` is
unified with the inferred type: `('f -> 'g) -> 'f list -> 'g list`.
All variables at the deeper level are generalized. The final scheme is:
`forall 'a 'b. ('a -> 'b) -> 'a list -> 'b list`.

Each use of `map` in the body of the let instantiates this scheme with
fresh variables, allowing it to be used at different type arguments.

## Summary

The inference algorithm is a recursive traversal of the AST with two
modes:

- **synth**: produce a type from the expression's structure. Used when
  there is no contextual type information.
- **check**: verify against an expected type. Used when context provides
  type information, enabling it to flow into subexpressions.

The key mechanisms:

1. **Fresh type variables** represent unknowns.
2. **Unification** constrains those unknowns based on how values are
   used.
3. **Levels** track the nesting depth of let bindings.
4. **Generalization** turns unconstrained variables at deep levels into
   quantified schemes.
5. **Instantiation** replaces quantified variables with fresh ones at
   each use site.

Every expression form maps to a specific pattern of variable creation,
unification calls, and context extension. The type system's power comes
from these simple pieces composed across the full AST.
