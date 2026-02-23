# Generalization, Instantiation, and Let-Polymorphism

This is part 4 in a series on the MiniML type system implementation.
It assumes you have read the prior parts on type representation,
unification, and inference.

All code lives in `lib/types.ml` (the `generalize`, `instantiate`, and
related functions) and `lib/typechecker.ml` (the `synth` cases for
`ELet`, `ELetRec`, `ELetRecAnd`, and the context-extension helpers).


## The Problem: Why We Need Generalization

Consider this program:

```
let id x = x
let n = id 42
let s = id "hello"
```

Without generalization, the typechecker would infer `id : 'a -> 'a` at
definition time, where `'a` is a single mutable type variable cell. When
it processes `id 42`, it would unify `'a` with `int`, setting the cell
to `Link TInt`. Now `id` has type `int -> int` -- permanently. When it
reaches `id "hello"`, it would try to unify `int` with `string` and
report an error.

This is not hypothetical. Lambda-bound variables behave exactly this
way in MiniML. If `id` were a lambda parameter rather than a
let-binding, it *would* be stuck at one type. The difference is that
`let` performs **generalization**: it freezes the inferred type into a
polymorphic **scheme**, replacing mutable type variables with immutable
`TGen` indices. Each subsequent use of `id` then gets its own fresh
copy of the type through **instantiation**. The two uses never
interfere because they have independent type variables.

This is called **let-polymorphism**, and it is the central mechanism
that makes Hindley-Milner type systems practical.


## Levels

Before explaining the generalization algorithm, we need to understand
**levels**. A level is an integer that tracks how deeply nested a
let-binding is. The top-level starts at level 0. Each time the
typechecker enters a let-binding's right-hand side, it increments the
level by 1.

### Why Levels Exist

When generalizing the type of a let-binding, we need to distinguish
two kinds of type variables:

1. **Local variables** -- created during the let-binding's own
   inference. These can safely be generalized because nothing outside
   the let-binding constrains them.

2. **Escaped variables** -- created at an outer scope and still
   reachable. These must not be generalized because outer code may
   still unify them with concrete types.

Levels encode this distinction. Every type variable carries the level
at which it was created:

```ocaml
type tvar =
  | Unbound of int * int   (* id, level *)
  | Link of ty

let new_tvar level =
  TVar (ref (Unbound (fresh_id (), level)))
```

When the typechecker enters a let-binding's right-hand side, it bumps
the level. Any type variable created inside that right-hand side gets
the higher level. When generalization runs, it looks at each unbound
variable and asks: "Is your level higher than the enclosing scope?" If
yes, the variable is local and can be generalized. If no, it belongs
to an outer scope and must be left alone.

### Level Incrementing in `synth`

Here is the `ELet` case from `lib/typechecker.ml`:

```ocaml
| Ast.ELet (name, e1, e2) ->
    let e1_te = synth ctx (level + 1) e1 in       (* <-- level + 1 *)
    let scheme = Types.generalize level e1_te.ty in
    let ctx' = extend_var ctx name scheme in
    let e2_te = synth ctx' level e2 in             (* <-- original level *)
    mk (TELet (name, e1_te, e2_te)) e2_te.ty
```

The right-hand side `e1` is synthesized at `level + 1`. Then
`generalize` is called with the *original* `level`. Any variable
created at `level + 1` (or deeper, from nested lets) will have a level
strictly greater than `level`, so it will be generalized. Any variable
created at `level` or shallower will be left as-is.

### Level Adjustment During Unification

There is a subtlety. A type variable might be created at a deep level,
but then unified with something at a shallower level. If the variable
"escapes" its scope, its level must be pulled up to prevent incorrect
generalization. This happens in the occurs check, which we covered in
the unification chapter:

```ocaml
| TVar ({ contents = Unbound (id2, level2) } as r) ->
    if level2 > level then r := Unbound (id2, level)
```

If an inner variable gets unified with an outer variable, the inner
one's level is adjusted down to match the outer one's level. After this
adjustment, generalization will correctly see it as belonging to the
outer scope.


## The Generalization Algorithm

Generalization walks a type tree and replaces every unbound type
variable whose level exceeds the current scope with a `TGen` index.
The steps are:

1. Create a hashtable mapping type variable IDs to `TGen` indices,
   and a counter starting at 0.
2. Recursively walk the type.
3. At each `Unbound(id, level')` where `level' > level`:
   - If `id` is already in the hashtable, return its `TGen` index.
     (This ensures the same variable gets the same index everywhere
     it appears.)
   - Otherwise, assign the next counter value, store it in the
     hashtable, and return `TGen counter`.
4. At each `Unbound(id, level')` where `level' <= level`: leave it
   untouched. This variable belongs to an outer scope.
5. At concrete types (`TInt`, `TArrow`, etc.): recurse into children.
6. Wrap the result in a scheme with `quant = counter` (the number of
   generalized variables).


## The `generalize` Function

Here is the complete implementation from `lib/types.ml`:

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
    | TVar { contents = Link _ } -> assert false
    | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
    | TTuple ts -> TTuple (List.map go ts)
    | TList t -> TList (go t)
    | TArray t -> TArray (go t)
    | TMap (k, v) -> TMap (go k, go v)
    | TRecord fields -> TRecord (List.map (fun (n, t) -> (n, go t)) fields)
    | TVariant (name, args) -> TVariant (name, List.map go args)
    | (TInt | TFloat | TBool | TString | TByte | TRune | TUnit | TGen _) as t -> t
  in
  let body = go ty in
  { quant = !counter; constraints = []; body }
```

Walking through the key parts:

**The `id_map` hashtable** ensures that each distinct type variable gets
a unique `TGen` index, and that the *same* variable always maps to the
*same* index. When generalizing `'a -> 'a`, the first encounter of `'a`
assigns `TGen 0`, and the second encounter looks up `'a` in the map and
finds `TGen 0` again. The result is `TArrow(TGen 0, TGen 0)` -- both
occurrences use the same index.

**The level check** (`level' > level`) is the guard that decides what
gets generalized. Variables at the current level or shallower are left
as live `TVar` cells. They remain part of the global inference state
and can still be unified later.

**The `Link` assert** (`assert false`) is safe because `repr` was
called at the top of `go`. After following all links, a `TVar` can
only be `Unbound`.

**The counter** tracks how many distinct variables were generalized.
This becomes the `quant` field of the resulting scheme, telling
instantiation how many fresh variables to create.

**The constraints field** is initialized to `[]`. When the function
has typeclass constraints, a separate path
(`resolve_let_constraints`) fills in the constraints after
generalization. More on this below.


## The `generalize_with_map` Variant

There is a second version of generalize that returns the `id_map`
hashtable alongside the scheme:

```ocaml
let generalize_with_map level ty =
  let counter = ref 0 in
  let id_map = Hashtbl.create 8 in
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
    | TTuple ts -> TTuple (List.map go ts)
    | TList t -> TList (go t)
    | TArray t -> TArray (go t)
    | TMap (k, v) -> TMap (go k, go v)
    | TRecord fields -> TRecord (List.map (fun (n, t) -> (n, go t)) fields)
    | TVariant (name, args) -> TVariant (name, List.map go args)
    | (TInt | TFloat | TBool | TString | TByte | TRune | TUnit | TGen _) as t -> t
  in
  let body = go ty in
  ({ quant = !counter; constraints = []; body }, id_map)
```

The algorithm is identical. The difference is the return type: it
produces `(scheme * (int, ty) Hashtbl.t)`, where the hashtable maps
each original **type variable ID** to the **`TGen` node** it was
replaced with.

### Why This Variant Exists

The mapping is needed for **typeclass constraint resolution**. When a
function like:

```
let show_pair (x : 'a) (y : 'b) : string = show x ^ ", " ^ show y
  where Show 'a, Show 'b
```

is generalized, the typechecker must figure out which `TGen` indices
correspond to the constrained type variables `'a` and `'b`. It
already knows the *tvar IDs* of the type variables backing `'a` and
`'b` (extracted from the parameter annotations). The `id_map` lets it
translate: "tvar ID 7 became `TGen 0`, tvar ID 9 became `TGen 1`."
This is used in `resolve_let_constraints` in `lib/typechecker.ml`:

```ocaml
let (scheme, id_map) = Types.generalize_with_map level te_ty in
let cc_list = List.map (fun (cclass, tyvar_names) ->
  let cc_args = List.map (fun tvname ->
    match Hashtbl.find_opt name_to_tvar tvname with
    | Some tvar_id ->
      (match Hashtbl.find_opt id_map tvar_id with
       | Some (Types.TGen idx) -> idx
       | _ -> error (...))
    | None -> error (...)
  ) tyvar_names in
  Types.{ cc_class = cclass; cc_args }
) constraints in
{ scheme with Types.constraints = cc_list }
```

It looks up each constraint type variable name (`'a`, `'b`) to get its
tvar ID, then uses the `id_map` to find the corresponding `TGen`
index. The resulting `class_constraint` records `cc_args = [0]` for
`Show 'a` and `cc_args = [1]` for `Show 'b`, tying each constraint to
the right quantified variable in the scheme.


## Instantiation

Instantiation is the reverse of generalization. When a polymorphic name
is used, each `TGen` is replaced with a fresh type variable so the use
site gets its own independent copy of the type.

Here is the implementation from `lib/types.ml`:

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
      | TArray t -> TArray (go t)
      | TMap (k, v) -> TMap (go k, go v)
      | TRecord fields -> TRecord (List.map (fun (n, t) -> (n, go t)) fields)
      | TVariant (name, args) -> TVariant (name, List.map go args)
      | TVar { contents = Link ty } -> go ty
      | t -> t
    in
    go s.body
  end
```

Walking through it:

**Short-circuit for monomorphic schemes.** If `quant = 0`, there are no
`TGen` nodes and nothing to replace, so the body is returned as-is.
This is the common case for lambda-bound variables.

**Fresh variable array.** `Array.init s.quant (fun _ -> new_tvar level)`
creates one fresh type variable per quantified position, all at the
current level. The array is indexed by `TGen` index: `TGen 0` maps to
`vars.(0)`, `TGen 1` to `vars.(1)`, and so on.

**The replacement walk.** `go` recursively traverses the scheme body.
When it hits `TGen id`, it returns the corresponding fresh variable
from the array. Crucially, the *same* `TGen` index always maps to the
*same* fresh variable. If the scheme body is
`TArrow(TGen 0, TGen 0)`, the result is `TArrow(v, v)` -- both
occurrences share the same `TVar` cell. Later unification on one
occurrence will be visible through the other.

**Where it is called.** Instantiation happens in `lookup_var`:

```ocaml
let lookup_var ctx level name =
  match List.assoc_opt name ctx.vars with
  | Some scheme -> Types.instantiate level scheme
  | None -> error (Printf.sprintf "unbound variable: %s" name)
```

Every time a variable is referenced, its scheme is instantiated at the
current level. This is what gives each use site its own type.


## The `instantiate_with_mapping` Variant

Like `generalize_with_map`, there is an instantiation variant that
returns the mapping from `TGen` indices to fresh variables:

```ocaml
let instantiate_with_mapping level (s : scheme) =
  if s.quant = 0 then (s.body, [])
  else begin
    let vars = Array.init s.quant (fun _ -> new_tvar level) in
    let mapping = List.init s.quant (fun i -> (i, vars.(i))) in
    let rec go = function
      | TGen id when id < s.quant -> vars.(id)
      | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
      | TTuple ts -> TTuple (List.map go ts)
      | TList t -> TList (go t)
      | TArray t -> TArray (go t)
      | TMap (k, v) -> TMap (go k, go v)
      | TRecord fields -> TRecord (List.map (fun (n, t) -> (n, go t)) fields)
      | TVariant (name, args) -> TVariant (name, List.map go args)
      | t -> t
    in
    (go s.body, mapping)
  end
```

It returns `(ty, (int * ty) list)`, where the list maps each `TGen`
index to the fresh `TVar` that replaced it. This is used during the
constraint transformation pass: when a constrained polymorphic
function is applied, the transformation needs to know which concrete
types ended up filling each quantified position, so it can look up
the right typeclass instance dictionary.


## How Let-Polymorphism Works End-to-End

Let us trace the complete lifecycle of a polymorphic binding. Consider:

```
let id x = x
let _ = id 42
let _ = id "hello"
```

### Step 1: Define `id`

The top-level `let id = fun x -> x` is processed by `check_decl`,
which calls `synth` at `level + 1`:

```
synth ctx (level=1) (EFun("x", EVar "x"))
```

The `EFun` case creates a fresh type variable for the parameter `x`:

```
param_ty = new_tvar 1   -->  TVar(ref(Unbound(0, 1)))
```

Call this variable `'a0` (id=0, level=1). The context is extended:

```
ctx' = extend_var_mono ctx "x" 'a0
```

Now the body `EVar "x"` is synthesized. `lookup_var` finds `"x"` in
the context with scheme `{ quant=0; body='a0 }` (monomorphic -- it was
added with `extend_var_mono`). Instantiation of a monomorphic scheme
is a no-op, so the body type is `'a0`.

The `EFun` case returns:

```
ty = TArrow('a0, 'a0)    i.e.  TArrow(TVar(ref(Unbound(0,1))), TVar(ref(Unbound(0,1))))
```

Note that both positions share the *same* `TVar` ref cell.

### Step 2: Generalize `id`

Back in `check_decl`, generalization runs at the enclosing level (0):

```
generalize 0 (TArrow('a0, 'a0))
```

The `go` function encounters `TVar(ref(Unbound(0, 1)))`. The level is
1, which is greater than 0 (the generalization level). This variable
is local, so it gets generalized:

```
id_map: { 0 -> 0 }     (tvar id 0 maps to TGen index 0)
counter: 1
```

When `go` encounters the same variable in the return position, it finds
id 0 already in the map and returns `TGen 0` again.

The resulting scheme:

```
{ quant = 1; constraints = []; body = TArrow(TGen 0, TGen 0) }
```

This is stored in the context: `ctx.vars = [("id", scheme); ...]`.

### Step 3: Use `id` at type `int`

The expression `id 42` triggers function application. The typechecker
first looks up `id`:

```
lookup_var ctx 0 "id"
  --> instantiate 0 { quant=1; body=TArrow(TGen 0, TGen 0) }
```

Instantiation creates one fresh variable (since `quant = 1`):

```
vars = [| TVar(ref(Unbound(5, 0))) |]     call this 'a5
```

(The ID 5 is hypothetical -- it is whatever `fresh_id()` returns next.)

The `go` function replaces `TGen 0` with `'a5` in both positions:

```
result: TArrow('a5, 'a5)
```

Now the application is typechecked. The argument `42` has type `TInt`.
A fresh return variable is created:

```
ret_ty = new_tvar 0   -->  'a6 (Unbound(6, 0))
```

Unification of `id`'s instantiated type with the application:

```
unify  TArrow('a5, 'a5)  with  TArrow(TInt, 'a6)
  unify 'a5 TInt       -->  'a5 := Link TInt
  unify 'a5 'a6        -->  repr 'a5 = TInt, so unify TInt 'a6
                        -->  'a6 := Link TInt
```

Result: `id 42` has type `TInt`.

### Step 4: Use `id` at type `string`

The expression `id "hello"` goes through the same process, but with
completely independent variables:

```
instantiate 0 { quant=1; body=TArrow(TGen 0, TGen 0) }
  vars = [| TVar(ref(Unbound(7, 0))) |]     call this 'a7

result: TArrow('a7, 'a7)
```

The argument `"hello"` has type `TString`. Unification:

```
unify  TArrow('a7, 'a7)  with  TArrow(TString, 'a8)
  unify 'a7 TString     -->  'a7 := Link TString
  unify 'a7 'a8         -->  repr 'a7 = TString, so 'a8 := Link TString
```

Result: `id "hello"` has type `TString`.

The two uses never interfered because instantiation gave each one its
own fresh copy of the type variable. Variable `'a5` became `int` and
variable `'a7` became `string`, independently.


## Monomorphic vs Polymorphic Bindings

MiniML has two ways to add a variable to the typing context:

```ocaml
let extend_var_mono ctx name ty =
  extend_var ctx name (Types.mono ty)
```

```ocaml
let extend_var ctx name scheme =
  { ctx with vars = (name, scheme) :: ctx.vars; ... }
```

`extend_var_mono` wraps the type in a monomorphic scheme (`quant = 0`).
When this variable is looked up later, instantiation is a no-op -- the
same type variable is returned. This means every use of the variable
shares the same type, and unification at one site affects all others.

`extend_var` (called with a generalized scheme) stores a polymorphic
scheme. Each lookup produces fresh variables, so different use sites
are independent.

### When Each Is Used

**Lambda parameters are monomorphic.** In the `EFun` case:

```ocaml
| Ast.EFun (param, body) ->
    let param_ty = ... Types.new_tvar level ... in
    let ctx' = extend_var_mono ctx param.name param_ty in
    ...
```

The parameter `x` in `fun x -> ...` is added with `extend_var_mono`.
Every occurrence of `x` in the body shares the same type variable. If
the body uses `x` as both an `int` and a `string`, that is a type
error.

**Let bindings are polymorphic.** In the `ELet` case:

```ocaml
| Ast.ELet (name, e1, e2) ->
    let e1_te = synth ctx (level + 1) e1 in
    let scheme = Types.generalize level e1_te.ty in
    let ctx' = extend_var ctx name scheme in
    ...
```

The right-hand side is inferred at a deeper level, then generalized.
The resulting scheme is stored with `extend_var`. Each use in `e2`
gets its own copy via instantiation.

**Pattern-match bindings are monomorphic.** In `synth_match`:

```ocaml
let ctx' = List.fold_left (fun c (n, t) -> extend_var_mono c n t) ctx bindings in
```

Variables introduced by pattern matching (like `x` in `Some x ->`) are
monomorphic within their branch.

The rule is: **generalization only happens at `let` boundaries.** This
is the defining characteristic of the Hindley-Milner system. Variables
introduced any other way (lambda parameters, pattern bindings, for-loop
variables) are monomorphic.


## Let Rec

Recursive bindings require special handling because the function's name
must be in scope during typechecking of its own body. Here is the
`ELetRec` case from `lib/typechecker.ml`:

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

The algorithm has three phases:

### Phase 1: Add the name monomorphically

```ocaml
let fn_var = Types.new_tvar (level + 1) in
let ctx' = extend_var_mono ctx name fn_var in
```

A fresh type variable is created for the function's type, and the name
is added to the context as monomorphic. This means that within the
function body, recursive calls see the function at a *single* type --
they cannot use it polymorphically. This is essential: the function's
type is not yet known, so it cannot be generalized yet.

### Phase 2: Typecheck the body, then unify

```ocaml
let e1_te = synth ctx' (level + 1) e1 in
try_unify fn_var e1_te.ty;
```

The body is typechecked with the function in scope. Then the
pre-allocated type variable is unified with the inferred type. This
ensures consistency: if the body says the function returns `int` but
recursive calls assume it returns `string`, unification catches the
contradiction.

### Phase 3: Generalize

```ocaml
let scheme = Types.generalize level e1_te.ty in
let ctx'' = extend_var ctx name scheme in
```

Only after the body is fully typechecked does generalization happen.
Now the function name is re-added to the context as a polymorphic
scheme. Code *after* the `let rec` can use the function at multiple
types.

### Why recursive calls are monomorphic

Consider:

```
let rec f x = f 42
```

During typechecking, `f` has the pre-allocated variable `'a`. The
parameter `x` gets variable `'b`. The body calls `f 42`, which unifies
`'a` with `int -> 'c`. Meanwhile, the overall function type is
`'b -> (type of body)`. Unification of `'a` with `'b -> 'c` forces
`'b = int` and the return type to `'c`.

If `f` were polymorphic during its own definition, a call like
`(f 42, f "hello")` inside the body would type check -- each call
would get independent variables. But this is unsound: the function
has not been fully defined yet, so we do not know whether it actually
*works* at multiple types. Requiring monomorphism during the definition
prevents this.

After generalization, the function is polymorphic for external callers.
This is the standard Hindley-Milner restriction on recursive
let-bindings.


## Mutual Recursion: `let rec ... and ...`

Mutual recursion extends the same pattern to multiple bindings. All
names must be in scope during typechecking of all bodies, and
generalization happens only after everything is checked. Here is the
expression-level case from `lib/typechecker.ml`:

```ocaml
| Ast.ELetRecAnd (bindings, body) ->
    (* Create type variables for all function names *)
    let fn_vars = List.map (fun (name, _) ->
      (name, Types.new_tvar (level + 1))
    ) bindings in
    (* Add all names to context as monomorphic *)
    let ctx' = List.fold_left (fun ctx (name, tv) ->
      extend_var_mono ctx name tv
    ) ctx fn_vars in
    (* Type-check all bodies *)
    let body_tes = List.map2 (fun (_, fn_expr) (_, tv) ->
      let te = synth ctx' (level + 1) fn_expr in
      try_unify tv te.ty;
      te
    ) bindings fn_vars in
    (* Generalize all together *)
    let ctx'' = List.fold_left2 (fun ctx (name, _) (_, tv) ->
      let scheme = Types.generalize level tv in
      extend_var ctx name scheme
    ) ctx fn_vars fn_vars in
    (* Type-check the continuation body *)
    let body_te = synth ctx'' level body in
    ...
```

The algorithm:

1. **Create fresh type variables** for every function in the binding
   group, all at `level + 1`.

2. **Add all names monomorphically.** Every function in the group sees
   all the others (and itself) at monomorphic types during
   typechecking.

3. **Typecheck all bodies.** Each body is synthesized at `level + 1`
   in the extended context. After inference, each body's type is
   unified with its pre-allocated variable.

4. **Generalize all together.** Each pre-allocated variable (which now
   contains the fully resolved type via unification) is generalized at
   `level`. The resulting schemes are added to the context.

5. **Typecheck the continuation.** The body after the `let rec ... and`
   sees all the functions as polymorphic.

### Example

```
let rec even n = if n = 0 do true else odd (n - 1)
and odd n = if n = 0 do false else even (n - 1)
```

Phase 1: `even : 'a`, `odd : 'b` (fresh variables at level 1).

Phase 2: Both are added monomorphically.

Phase 3: Typechecking `even`'s body unifies `'a` with `int -> bool`.
Typechecking `odd`'s body unifies `'b` with `int -> bool`.

Phase 4: Generalization. Both `'a` and `'b` are now `int -> bool`,
which contains no unbound type variables at level > 0. So both schemes
have `quant = 0` -- they are monomorphic. This is expected: `even` and
`odd` genuinely work at only one type.

If the functions were polymorphic (like mutually recursive identity
functions), the variables would remain unbound and would be generalized
into proper schemes.


## The Value Restriction: Mutable Bindings

MiniML implements a form of the **value restriction** for mutable
bindings. The issue: if a mutable variable were generalized, different
reads could instantiate it at different types, breaking type safety.

Consider:

```
let mut r = []
r <- [42]         -- r : int list
head r            -- still int list? Or could it be string list?
```

If `r` were given the scheme `forall 'a. 'a list`, then each use would
instantiate it with fresh variables. The assignment `r <- [42]` would
see one copy, and `head r` would see a different copy. They would not
share the same concrete type, making the mutation unsound.

MiniML prevents this in the `ELetMut` case:

```ocaml
| Ast.ELetMut (name, e1, e2) ->
    let e1_te = synth ctx (level + 1) e1 in
    (* Mutable variables must not be generalized (value restriction):
       each reference must share the same type, not get a fresh instantiation *)
    let scheme = { Types.quant = 0; constraints = []; body = e1_te.ty } in
    let ctx' = extend_var_mutable ctx name scheme in
    let e2_te = synth ctx' level e2 in
    mk (TELetMut (name, e1_te, e2_te)) e2_te.ty
```

The key line is the scheme construction: `quant = 0` is hardcoded.
Even though the right-hand side was synthesized at `level + 1` (and
its type variables *could* be generalized), the mutable binding is
forced to be monomorphic. The actual `generalize` function is never
called -- the scheme is constructed directly with zero quantified
variables.

This means `let mut r = []` gives `r` the type `'a list` where `'a`
is a live, unbound type variable. The first use that constrains it
(like `r <- [42]`) will set `'a` to `int`, and all subsequent uses
will see `int list`. This is safe because there is only one copy of
the type variable, shared across all uses.

Note the comment in the source: "each reference must share the same
type, not get a fresh instantiation." This is exactly the value
restriction.

Immutable let-bindings do not have this problem because they cannot be
mutated. If `let r = []` generalizes `r` to `forall 'a. 'a list`, each
use gets an independent `'b list`, but since `r` is never modified, the
different instantiations do not interfere.


## Constrained Generalization

When a function has typeclass constraints (a `where` clause), the
generalization process must record which quantified variables are
constrained and by which classes. This is handled by
`resolve_let_constraints` in `lib/typechecker.ml`.

### The Problem

A function like:

```
let show_list (xs : 'a list) : string = ...
  where Show 'a
```

has the type `'a list -> string` where `'a` must be an instance of
`Show`. After generalization, the scheme must carry this constraint so
that every call site can look up the appropriate `Show` dictionary.

### The Solution

The function `resolve_let_constraints` orchestrates the process:

```ocaml
let resolve_let_constraints ?(type_env=Types.empty_type_env) level
    constraints params ret_annot te_ty =
  if constraints = [] then
    Types.generalize level te_ty
  else begin
    let name_to_tvar = Hashtbl.create 4 in
    (* ... extract tvar IDs from param/return annotations ... *)
    let (scheme, id_map) = Types.generalize_with_map level te_ty in
    let cc_list = List.map (fun (cclass, tyvar_names) ->
      let cc_args = List.map (fun tvname ->
        match Hashtbl.find_opt name_to_tvar tvname with
        | Some tvar_id ->
          (match Hashtbl.find_opt id_map tvar_id with
           | Some (Types.TGen idx) -> idx
           | _ -> error (...))
        | None -> error (...)
      ) tyvar_names in
      Types.{ cc_class = cclass; cc_args }
    ) constraints in
    { scheme with Types.constraints = cc_list }
  end
```

The steps are:

1. **If there are no constraints**, fall back to plain `generalize`.
   This is the common case.

2. **Build a `name_to_tvar` mapping** from type variable names in the
   source annotations (like `'a`) to their internal tvar IDs. This is
   done by walking the parameter annotations alongside the inferred
   parameter types and extracting the tvar IDs from unbound variables.

3. **Call `generalize_with_map`** instead of plain `generalize`. This
   produces both the scheme and the `id_map` (tvar ID to `TGen` index).

4. **Resolve each constraint.** For each constraint like `Show 'a`:
   - Look up `'a` in `name_to_tvar` to get the tvar ID.
   - Look up the tvar ID in `id_map` to get the `TGen` index.
   - Record `{ cc_class = "Show"; cc_args = [idx] }`.

5. **Attach the constraints to the scheme.** The final scheme has
   both the quantified body and the constraint list.

The resulting scheme for `show_list` would be:

```
{ quant = 1;
  constraints = [{ cc_class = "Show"; cc_args = [0] }];
  body = TArrow(TList(TGen 0), TString) }
```

This says: "For all `'a` (quantified variable 0) such that `Show 'a`
holds, this function has type `'a list -> string`."

### Constrained Function Synthesis

Functions with constraints are typechecked through a dedicated path,
`synth_constrained_fn`, which pre-allocates shared type variables for
all constraint type variable names and passes them through the
parameter annotation resolution. This ensures that the type variable
for `'a` in a parameter annotation and the type variable for `'a` in
a constraint clause are the same cell:

```ocaml
let synth_constrained_fn ctx level constraints params ret_annot body =
  let shared_tvars = Hashtbl.create 4 in
  let constraint_tyvar_names =
    List.flatten (List.map (fun (_, names) -> names) constraints)
    |> List.sort_uniq String.compare in
  List.iter (fun tvname ->
    Hashtbl.replace shared_tvars tvname (Types.new_tvar (level + 1))
  ) constraint_tyvar_names;
  ...
```

By sharing a single tvar across all annotations and constraints that
use the same name, the system guarantees that generalization and
constraint resolution see consistent type variable IDs.


## Summary

The generalization/instantiation cycle is what makes MiniML's type
system polymorphic:

1. **Levels** track scope depth. Each let-binding's right-hand side is
   typechecked at an incremented level.

2. **Generalization** walks the inferred type and replaces unbound
   variables whose level exceeds the current scope with `TGen` indices,
   producing a scheme.

3. **Instantiation** replaces `TGen` indices with fresh type variables
   at each use site, so different uses get independent types.

4. **Monomorphic bindings** (lambda parameters, pattern variables,
   mutable lets) skip generalization. All uses share one type.

5. **Recursive bindings** add the function name monomorphically first,
   typecheck the body, then generalize afterward.

6. **Mutual recursion** extends this to groups: all names are added
   monomorphically, all bodies are checked, then all are generalized
   together.

7. **Mutable bindings** are never generalized, implementing the value
   restriction.

8. **Constrained generalization** uses `generalize_with_map` to track
   the mapping from tvar IDs to `TGen` indices, then attaches typeclass
   constraints to the scheme.

The next chapter covers type classes: how class and instance
declarations are processed, how the constraint transformation pass
rewrites the typed AST into dictionary-passing style, and how instance
resolution works.
