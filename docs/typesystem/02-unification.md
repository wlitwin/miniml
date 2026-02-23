# Unification

Part 3 in a series on the MiniML type system implementation. Assumes
you have read the prior parts on type representation and type variables.

## What Is Unification?

Unification is the process of making two types equal by finding a
substitution for type variables. It is the core algorithm that powers
type inference.

When the typechecker sees `f x` where `f : 'a -> int` and `x : string`,
unification discovers `'a = string` and records that binding. Unification
either succeeds -- binding type variables as a side effect -- or fails,
which means a type error.

The key insight: unification is not a one-shot check. It is a *constraint
solver*. Each call to `unify` adds information to the global substitution
(stored in mutable ref cells), and later calls see the results of earlier
ones. By the time inference finishes, every type variable has either been
bound to a concrete type or remains free (and gets generalized).

## The Algorithm

The `unify` function lives in `lib/types.ml`:

```ocaml
let rec unify t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then ()
  else match t1, t2 with
  | TVar ({ contents = Unbound (id, level) } as r), ty
  | ty, TVar ({ contents = Unbound (id, level) } as r) ->
    occurs_check id level ty;
    r := Link ty
  | TArrow (a1, e1, r1), TArrow (a2, e2, r2) ->
    unify a1 a2; unify_eff e1 e2; unify r1 r2
  | TTuple ts1, TTuple ts2 ->
    if List.length ts1 <> List.length ts2 then unify_error t1 t2;
    List.iter2 unify ts1 ts2
  | TList t1', TList t2' -> unify t1' t2'
  | TArray t1', TArray t2' -> unify t1' t2'
  | TMap (k1, v1), TMap (k2, v2) -> unify k1 k2; unify v1 v2
  | TRecord f1, TRecord f2 ->
    let (smaller, larger) =
      if List.length f1 <= List.length f2 then (f1, f2) else (f2, f1) in
    List.iter (fun (name, ty_s) ->
      match List.assoc_opt name larger with
      | Some ty_l -> unify ty_s ty_l
      | None -> unify_error t1 t2
    ) smaller
  | TInt, TInt | TFloat, TFloat | TBool, TBool
  | TString, TString | TByte, TByte | TRune, TRune | TUnit, TUnit -> ()
  | TVariant (a, args_a), TVariant (b, args_b) when String.equal a b ->
    if List.length args_a <> List.length args_b then unify_error t1 t2;
    List.iter2 unify args_a args_b
  | _ -> unify_error t1 t2
```

### Step 1: Follow Links (`repr`)

```ocaml
let t1 = repr t1 in
let t2 = repr t2 in
```

Both types are chased through the union-find via `repr`. A `TVar` is
either `Unbound` (unresolved) or `Link ty` (alias). `repr` follows links
to the representative and path-compresses. After this call, you are
guaranteed to have either a concrete type constructor or an `Unbound`
variable -- never a `Link`.

### Step 2: Physical Equality Check

```ocaml
if t1 == t2 then ()
```

OCaml's `==` checks physical equality -- same object in memory. If both
types are the same ref cell, they are trivially unified. This is both a
fast path and a guard against infinite loops when the same variable
appears on both sides.

### Step 3: Variable Binding

```ocaml
| TVar ({ contents = Unbound (id, level) } as r), ty
| ty, TVar ({ contents = Unbound (id, level) } as r) ->
    occurs_check id level ty;
    r := Link ty
```

This is the heart of unification. If either side is an unbound variable,
bind it to the other type. First run the occurs check (below), then set
the ref cell to `Link ty`. This single mutation is the moment a type
variable gets its value.

```
BEFORE                        AFTER
  'a                            'a
  +----------------+            +----------------+
  | Unbound(0, 1)  |            | Link --------->| TInt
  +----------------+            +----------------+
```

Any other type linked to this ref cell will now resolve to `TInt` via
`repr`. This is how information propagates through the type graph.

### Step 4: Structural Recursion

For compound types, recursively unify components:

- **Arrow types**: unify parameter with parameter, return with return.
  Unifying `'a -> bool` with `int -> 'b` gives `'a = int`, `'b = bool`.
- **Tuples**: must have same arity; unify element-wise.
- **Lists, arrays**: unify element types. `string list` unifies with
  `'a list` by binding `'a = string`.
- **Maps**: unify key types, then value types.
- **Variants**: must have same name and same number of type args.
  `int option` unifies with `'a option` but never with `int list`.

### Step 5: Primitives

```ocaml
| TInt, TInt | TFloat, TFloat | TBool, TBool
| TString, TString | TByte, TByte | TRune, TRune | TUnit, TUnit -> ()
```

Primitive types unify only with themselves. No implicit coercion:
`TInt` does not unify with `TFloat`.

### Step 6: Failure

```ocaml
| _ -> unify_error t1 t2
```

If nothing matches, raise `Unify_error`. The typechecker catches this
and reports it with source location.

## The Occurs Check

Called every time a variable is about to be bound. Two responsibilities.

```ocaml
let rec occurs_check id level ty =
  match repr ty with
  | TVar { contents = Unbound (id2, _) } when id = id2 ->
    raise (Unify_error "occurs check: infinite type")
  | TVar ({ contents = Unbound (id2, level2) } as r) ->
    if level2 > level then r := Unbound (id2, level)
  | TVar { contents = Link _ } -> assert false
  | TArrow (a, _eff, b) -> occurs_check id level a; occurs_check id level b
  | TTuple ts -> List.iter (occurs_check id level) ts
  | TList t -> occurs_check id level t
  | TArray t -> occurs_check id level t
  | TMap (k, v) -> occurs_check id level k; occurs_check id level v
  | TRecord fields -> List.iter (fun (_, t) -> occurs_check id level t) fields
  | TVariant (_, args) -> List.iter (occurs_check id level) args
  | TInt | TFloat | TBool | TString | TByte | TRune | TUnit | TGen _ -> ()
```

### Preventing Infinite Types

If the variable being bound appears *inside* the target type, the result
would be infinite. Example: `let f x = f` tries to unify `'b` with
`'a -> 'b`, making `'b = 'a -> 'b = 'a -> 'a -> 'b = ...`.

Without the check, the union-find would cycle and `repr` would loop:

```
  'b
  +-------------------+
  | Link ------------->  TArrow('a, 'b)
  +-------------------+         |
       ^                        |
       +------------------------+
       (cycle!)
```

The occurs check walks the target type. If it finds the same variable id,
it raises immediately -- before the `r := Link ty` mutation happens.

### Level Adjustment

```ocaml
| TVar ({ contents = Unbound (id2, level2) } as r) ->
    if level2 > level then r := Unbound (id2, level)
```

If a *different* unbound variable inside the target has a deeper level
than the variable being bound, pull it up (reduce its level).

Levels track `let`-nesting depth. Generalization (next chapter) only
generalizes variables deeper than the current scope. If a deep variable
escapes to a shallower scope via unification, its level must be adjusted
so it is not incorrectly generalized. Example:

```
let r =
  let x = ref None in    (* 'a at level 2 *)
  x                       (* 'a escapes to level 1 *)
```

Without adjustment, `'a` would be generalized, letting `r` be used as
both `int option ref` and `string option ref` -- unsound. Level
adjustment prevents this.

Intuition: **if a variable escapes its scope, it must not be generalized
at that scope.**

## Structural Record Subtyping

Most `unify` cases demand exact structural match. Records are the
exception -- MiniML implements *width subtyping* directly in `unify`:

```ocaml
| TRecord f1, TRecord f2 ->
    let (smaller, larger) =
      if List.length f1 <= List.length f2 then (f1, f2) else (f2, f1) in
    List.iter (fun (name, ty_s) ->
      match List.assoc_opt name larger with
      | Some ty_l -> unify ty_s ty_l
      | None -> unify_error t1 t2
    ) smaller
```

### How It Works

1. Identify which record has fewer fields (the "smaller" one).
2. Every field in the smaller must exist in the larger.
3. Matching fields are recursively unified.
4. Extra fields in the larger are ignored.

A record with more fields is accepted anywhere fewer are expected, as
long as the overlapping fields agree.

### Why This Is Useful

```
let get_x r = r.x       -- inferred: {x: 'a} -> 'a

get_x {x = 1; y = 2}           -- ok: has x
get_x {x = 1; y = 2; z = 3}   -- ok: has x
get_x {name = "hi"}            -- error: no x field
```

Similar to TypeScript's structural typing or Go's interfaces, but fully
type-inferred. You never write an interface declaration; the typechecker
discovers the minimal record type from usage.

### Example Walkthrough

Tracing `get_x {x = 1; y = 2}`:

1. `get_x` is instantiated to `{x: 'b} -> 'b`.
2. The argument has type `{x: int; y: int}`.
3. Unify parameter: `{x: 'b}` vs `{x: int; y: int}`.
4. Smaller is `{x: 'b}` (1 field), larger is `{x: int; y: int}` (2).
5. Field `x`: unify `'b` with `int` -- `'b = int`.
6. Field `y` in larger: not in smaller, ignored.
7. Return type `'b` is now `int`.

### Symmetry

The code picks the smaller record regardless of argument order, so
record unification is *symmetric*. Both of these succeed:

```
unify {x: int}          {x: int; y: bool}    -- ok
unify {x: int; y: bool} {x: int}             -- also ok
```

## Unification vs Subtyping

MiniML has both `unify` and a separate `subtype` function:

| Property      | `unify`                      | `subtype`                     |
|---------------|------------------------------|-------------------------------|
| Side effects  | Mutates ref cells            | Pure boolean, no mutation     |
| Used during   | Type inference               | Checks after inference        |
| Records       | Width subtyping              | Width subtyping               |
| Arrows        | Unifies both sides           | Contravariant in parameter    |
| Failure       | Raises `Unify_error`         | Returns `false`               |

`subtype` handles arrow variance correctly:

```ocaml
| TArrow (a1, _e1, r1), TArrow (a2, _e2, r2) ->
    subtype a2 a1 && subtype r1 r2    (* contravariant in parameter *)
```

Use `unify` when *discovering* types during inference. Use `subtype` when
comparing two known types without wanting to commit to variable bindings.

## Worked Example: Full Unification Trace

```
let f x y = if x do y + 1 else y * 2
```

**Setup.** Fresh variables: `x: 'a`, `y: 'b`, result: `'c` (all level 1).

**Step 1: `if x do ...`** -- condition must be `bool`:

```
unify 'a bool  -->  'a := Link bool
```

**Step 2: `y + 1`** -- `+` expects `int`:

```
unify 'b int  -->  'b := Link int
```

Result of `+` is `int`.

**Step 3: `y * 2`** -- `*` expects `int`, `repr 'b` is already `int`:

```
unify int int  -->  success (primitives match)
```

**Step 4: unify branches** -- both are `int`:

```
unify int int  -->  success
unify 'c int   -->  'c := Link int
```

**Final state:**

```
  'a                    'b                    'c
  +-----------+         +-----------+         +-----------+
  | Link bool |         | Link int  |         | Link int  |
  +-----------+         +-----------+         +-----------+
       |                     |                     |
       v                     v                     v
     TBool                 TInt                  TInt
```

The function `f` has type `bool -> int -> int`.

## Error Cases

### Unification Failure

```
let bad x = (x + 1, x ^ "hi")
```

From `x + 1`, unify `'a` with `int` -- `'a = int`. Then `x ^ "hi"`
tries to unify `'a` with `string`, but `repr 'a` is `int`:

```
unify int string  -->  no case matches  -->  Unify_error
```

The typechecker never explicitly checks "is this an int?". It sets up
constraints via `unify` and lets the algorithm discover contradictions.

### Infinite Type

```
let loop x = x x
```

`x : 'a`, and `x x` requires `'a` to be a function taking `'a`:

```
unify 'a ('a -> 'b)
  occurs_check: walks TArrow('a, 'b), finds 'a  -->  infinite type error
```

Without this, `'a = 'a -> 'b` would create a cycle and `repr` would
loop forever.

## Summary

Unification is a three-part algorithm:

1. **Chase links** (`repr`) to find the real type behind any variable.
2. **Bind variables** by mutating ref cells, propagating type information
   through the constraint graph.
3. **Recurse structurally** into compound types, demanding shapes match.

The occurs check prevents infinite types and adjusts levels for correct
generalization. Record width subtyping is the one place where unification
relaxes the "exact match" rule, enabling flexible structural typing.

The next chapter covers generalization and instantiation: how the
typechecker decides which type variables become polymorphic and which
stay monomorphic.
