# Part 2: Types and Representation

This chapter explains how MiniML represents types internally. If you have used
OCaml or Haskell, you already have intuition for what types *are* -- `int`,
`'a -> 'b`, `int list`, and so on. The question here is how those types are
encoded as data structures that the typechecker can create, compare, and
transform. Understanding the representation is the single most important
prerequisite for understanding inference, because every algorithm in later
chapters operates on these structures.

All code in this chapter lives in `lib/types.ml` unless noted otherwise.

## The Type Language

Types are represented by the OCaml algebraic type `ty`:

```ocaml
type ty =
  | TInt | TFloat | TBool | TString | TByte | TRune | TUnit
  | TArrow of ty * eff * ty
  | TTuple of ty list
  | TList of ty
  | TRecord of (string * ty) list
  | TVariant of string * ty list
  | TMap of ty * ty
  | TArray of ty
  | TVar of tvar ref
  | TGen of int
```

A value of type `ty` is a tree. Leaves are ground types like `TInt` or type
variables like `TVar _`. Internal nodes are type constructors like `TArrow` or
`TList` whose children are the types they are parameterized over. Walking
through each variant:

**Primitive types** -- `TInt`, `TFloat`, `TBool`, `TString`, `TByte`, `TRune`,
`TUnit`. These are leaves with no children. `42` has type `TInt`. `"hello"` has
type `TString`. `()` has type `TUnit`.

**TArrow(a, eff, b)** -- Function types. A 3-tuple: the parameter type, the
latent effect, and the return type. The effect (of type `eff`) describes which
effects the function may perform when applied -- see the `eff` type in the
[Advanced Topics](07-advanced.md) chapter. Functions are curried: a
two-argument function like `fun x y -> x + y` has type
`TArrow(TInt, eff1, TArrow(TInt, eff2, TInt))`, which you would write as
`int -> int -> int`. Arrow is right-associative in the tree: the return type
of the outer arrow is itself an arrow.

**TTuple(ts)** -- Product types. `(1, true, "hi")` has type
`TTuple [TInt; TBool; TString]`. The list always has at least two elements in
practice, but the representation does not enforce this.

**TList(t)** -- Homogeneous list types. `[1; 2; 3]` has type `TList TInt`.
`[true; false]` has type `TList TBool`. The empty list `[]` has type
`TList (TVar _)` where the variable is initially unconstrained and will be
unified with whatever element type context demands.

**TRecord(fields)** -- Structural record types. `{ name = "Alice"; age = 30 }`
has type `TRecord [("name", TString); ("age", TInt)]`. Fields are stored as an
association list of name-type pairs.

**TVariant(name, params)** -- Named algebraic data types. If you declare
`type 'a option = None | Some of 'a`, then `Some 42` has type
`TVariant("option", [TInt])` and `None` has type
`TVariant("option", [TVar _])` (with a fresh variable for the parameter). The
string is the type name, and the list holds the type arguments. A
non-parameterized type like `type color = Red | Green | Blue` uses an empty
parameter list: `TVariant("color", [])`.

**TMap(k, v)** -- Key-value map types. `Map.of_list [(1, "one"); (2, "two")]`
has type `TMap(TInt, TString)`.

**TArray(t)** -- Mutable array types. `[| 1; 2; 3 |]` has type
`TArray TInt`.

**TVar(ref)** -- Type inference variables. These are the central mechanism that
makes inference work. They are explained in detail in the next section.

**TGen(i)** -- Quantified type variables that appear inside type schemes
(polymorphic types). `TGen 0` stands for the first universally quantified
variable, `TGen 1` the second, and so on. These never appear in types during
active inference -- only in finalized schemes stored in the environment. They
are explained in the type schemes section below.

### Types are trees

To build intuition, consider the type `int -> int list -> int list`. As a `ty`
value this is:

```
TArrow
  TInt
  eff1
  TArrow
    TList
      TInt
    eff2
    TList
      TInt
```

Every operation the typechecker performs -- unification, generalization,
instantiation, occurs check -- is a recursive traversal of this tree.

## Type Variables and Union-Find

This is the key insight that makes Hindley-Milner type inference work. A type
variable is a mutable ref cell:

```ocaml
type tvar =
  | Unbound of int * int   (* id, level *)
  | Link of ty
```

A `TVar` holds a `ref` to a `tvar`, and a `tvar` is in one of two states:

- **Unbound(id, level)** -- The variable is not yet known to equal any type.
  The `id` is a globally unique integer (assigned from a counter). The `level`
  is used for generalization, which is covered in a later chapter. For now,
  think of it as "how deeply nested in let-bindings this variable was created."

- **Link(ty)** -- The variable has been unified with some other type. It now
  points to that type, essentially saying "I am the same as `ty`."

When the unifier discovers that two types must be equal (for instance, because a
function expecting `int` received a value whose type is an unknown variable),
it *mutates* the variable's ref cell from `Unbound` to `Link`:

```ocaml
(* inside the unify function *)
| TVar ({ contents = Unbound (id, level) } as r), ty ->
    occurs_check id level ty;
    r := Link ty
```

This is the "union" operation from the union-find data structure. The mutable
ref cell is doing double duty: it is both the representation of an unknown type
and the mechanism for recording what that type turns out to be.

Multiple variables can form chains. If variable `a` is unified with variable
`b`, and later `b` is unified with `int`, the structure looks like:

```
a: TVar(ref(Link(TVar(ref(Link(TInt))))))
                   ^-- b
```

Variable `a` links to `b`, and `b` links to `TInt`. Both `a` and `b` represent
`int`, but to discover this you have to follow the chain. This is what `repr`
does.

## Path Compression: `repr`

The `repr` function follows `Link` chains to find the representative
(canonical) type, and compresses the path as it goes:

```ocaml
let rec repr = function
  | TVar ({ contents = Link ty } as r) ->
    let ty = repr ty in
    r := Link ty;
    ty
  | ty -> ty
```

This is the standard union-find "find" operation with path compression. Walking
through the logic:

1. If the type is a `TVar` whose ref cell contains `Link ty`, recurse into `ty`
   to find the final representative.
2. Update the ref cell to point directly to the final representative (path
   compression). Next time anyone follows this variable, they reach the answer
   in one step instead of traversing the whole chain.
3. If the type is anything else -- a concrete type like `TInt`, an `Unbound`
   variable, or a compound type like `TArrow` -- return it as-is.

After calling `repr`, you are guaranteed to get either:
- A concrete type (`TInt`, `TArrow(...)`, `TList(...)`, etc.), or
- A `TVar` with `Unbound` -- a genuinely unknown type variable.

You never get a `TVar` with `Link`. This invariant is critical: almost every
function in the typechecker calls `repr` before inspecting a type with
pattern matching.

**Example.** Suppose we have three variables forming a chain:

```
Before repr:
  v1 -> Link(v2) -> Link(v3) -> Link(TInt)

After repr(v1):
  v1 -> Link(TInt)
  v2 -> Link(TInt)    (* compressed *)
  v3 -> Link(TInt)    (* was already direct *)
  returns TInt
```

Path compression makes repeated lookups nearly O(1) amortized. Without it,
chains could grow long and every type inspection would pay the cost of
traversal.

### Deep Resolution: `deep_repr`

While `repr` resolves the outermost `TVar` link chain, it does not recurse
into the children of compound types. A type like `TArrow(TVar(ref(Link TInt)),
eff, TVar(ref(Link TBool)))` would come back unchanged from `repr` -- the top-level
node is already a `TArrow`, not a `TVar`. But the children still contain
unresolved `TVar` links.

The `deep_repr` function resolves this by recursively walking the entire type
tree and following all `TVar` links at every level:

```ocaml
let rec deep_repr ty =
  match repr ty with
  | TArrow (a, eff, b) -> TArrow (deep_repr a, deep_repr_eff eff, deep_repr b)
  | TTuple ts -> TTuple (List.map deep_repr ts)
  | TList t -> TList (deep_repr t)
  | TArray t -> TArray (deep_repr t)
  | TMap (k, v) -> TMap (deep_repr k, deep_repr v)
  | TRecord fs -> TRecord (List.map (fun (n, t) -> (n, deep_repr t)) fs)
  | TVariant (name, args) -> TVariant (name, List.map deep_repr args)
  | t -> t
```

The result is a fully concrete type tree with no remaining `Link` indirections
anywhere in the structure. Unbound type variables remain as-is (they have no
link to follow), but every resolved variable is replaced by its final concrete
type.

This function is particularly important for GADT pattern matching. When the
typechecker processes a GADT match arm, it temporarily unifies type variables
to reflect the type equalities implied by the constructor (for example,
matching on `IntLit` forces `'a = int`). After typechecking the arm body, the
typechecker needs to capture the fully resolved result type *before* restoring
the type variable state to what it was prior to the arm. Calling `deep_repr` on
the arm's result type "freezes" it into a self-contained type tree that survives
the subsequent state rollback. Without `deep_repr`, the restored `TVar` cells
would lose the GADT-refined information and the result type would revert to its
pre-match state.

## Fresh Variables

Whenever the typechecker encounters an expression whose type is not yet known,
it creates a fresh type variable:

```ocaml
let next_id = ref 0

let fresh_id () =
  let id = !next_id in
  next_id := id + 1;
  id

let new_tvar level =
  TVar (ref (Unbound (fresh_id (), level)))
```

Each call to `new_tvar` produces a variable with a globally unique `id` and the
given `level`. The `id` ensures that the occurs check (which prevents infinite
types like `'a = 'a list`) can identify whether it has encountered the same
variable twice. The `level` controls which variables get generalized into
polymorphic type schemes -- but that mechanism belongs to a later chapter.

Fresh variables are created in many places during inference. Some examples:

- **Unannotated function parameters**: `fun x -> x + 1` -- the parameter `x`
  gets a fresh variable, which unification then forces to `TInt`.
- **Empty collections**: `[]` has type `TList(fresh_var)`. The element type is
  unknown until something constrains it.
- **Polymorphic instantiation**: When you use a polymorphic value like `id`,
  each usage creates fresh variables for its quantified positions (see
  instantiation below).

## Type Schemes (Polymorphic Types)

A type scheme represents a polymorphic type -- one with universally quantified
variables:

```ocaml
type scheme = {
  quant: int;
  constraints: class_constraint list;
  body: ty;
}
```

- `quant` is the number of quantified type variables.
- `body` is the type, where `TGen 0` through `TGen (quant - 1)` stand for the
  quantified positions.
- `constraints` lists typeclass constraints on the quantified variables (e.g.
  "the first type variable must be an instance of `Show`").

**Example.** The identity function `let id x = x` gets this scheme:

```ocaml
{ quant = 1; constraints = []; body = TArrow(TGen 0, EffEmpty, TGen 0) }
```

This represents the type `forall 'a. 'a -> 'a`. The `TGen 0` in both the
parameter and return position says "these are the same quantified variable."
The `EffEmpty` indicates the function is pure.

A two-parameter polymorphic function like `let const x y = x` gets:

```ocaml
{ quant = 2; constraints = []; body = TArrow(TGen 0, EffEmpty, TArrow(TGen 1, EffEmpty, TGen 0)) }
```

This represents `forall 'a 'b. 'a -> 'b -> 'a`. The return type uses `TGen 0`,
the same as the first parameter, capturing the fact that `const` returns its
first argument unchanged.

**Monomorphic types** have `quant = 0` and no `TGen` in the body. The helper
function `mono` wraps a plain type into a trivial scheme:

```ocaml
let mono ty = { quant = 0; constraints = []; body = ty }
```

A monomorphic scheme for `int -> bool` is simply
`{ quant = 0; constraints = []; body = TArrow(TInt, EffEmpty, TBool) }`.

**Why TGen instead of named variables?** Using de Bruijn-style indices
(`TGen 0`, `TGen 1`, ...) instead of names avoids alpha-equivalence issues. Two
schemes are the same polymorphic type if and only if they have the same `quant`
and structurally identical `body` trees. There is no need to worry about whether
one scheme calls its variable `'a` and another calls it `'b`.

**Instantiation** is the process of using a polymorphic scheme: each `TGen i`
is replaced with a fresh `TVar`. This happens every time you reference a
let-bound variable. If `id` has the scheme above and you write `id 42`, the
typechecker instantiates the scheme by replacing `TGen 0` with a fresh variable,
yielding the type `TVar(ref(Unbound(k, _))) -> TVar(ref(Unbound(k, _)))` (the
same variable in both positions). Unification with the argument `42 : int` then
sets that variable to `Link TInt`, producing `int -> int`.

## The Type Environment

The type environment tracks all named type definitions:

```ocaml
type type_env = {
  variants: (string * int * variant_def * bool) list;
    (* (type_name, num_params, constructors, is_gadt) *)
  constructors: (string * ctor_info) list;
  records: (string * (string * ty) list) list;
  classes: class_def list;
  instances: instance_def list;
  effects: effect_def list;
  mutable_fields: string list;
  modules: (string * module_info) list;
  type_aliases: (string * string) list;
  type_synonyms: (string * int * ty) list;
}
```

Each field serves a distinct purpose:

- **variants** -- Registered algebraic data types. Each entry is a 4-tuple of
  (type name, number of type parameters, list of constructors, is_gadt flag).
  The `is_gadt` flag is `true` when any constructor in the type uses GADT
  syntax (i.e. specifies an explicit return type). For
  `type 'a option = None | Some of 'a`, this stores
  `("option", 1, [("None", None); ("Some", Some (TGen 0))], false)`.
  For a GADT like `type 'a expr = IntLit : int -> int expr | ...`, the flag
  would be `true`.

- **constructors** -- A flat lookup table from constructor name to its metadata.
  When the typechecker sees `Some 42`, it looks up `"Some"` here to find which
  type it belongs to and what argument it expects.

- **records** -- Registered record types with their field lists. Each entry maps
  a type name to an association list of field names and types.

- **classes** -- Typeclass definitions (name, type parameters, method
  signatures).

- **instances** -- Typeclass instance declarations, recording which concrete
  types implement which classes.

- **effects** -- Algebraic effect definitions, mapping effect names to their
  operation signatures.

- **mutable_fields** -- The names of record fields declared with `mut`. This
  lets the typechecker reject assignments to immutable fields.

- **modules** -- Module definitions with their public interfaces, including
  exported values, types, constructors, and submodules.

- **type_aliases** -- Short names that map to fully qualified names. Used for
  module-qualified type resolution.

- **type_synonyms** -- Type aliases like `type name = string`. Each entry is
  (name, number of parameters, expanded type). The typechecker expands these
  during type annotation resolution.

**Constructor info** provides the details needed to typecheck pattern matching
and construction:

```ocaml
type ctor_info = {
  ctor_type_name: string;
  ctor_arg_ty: ty option;
  ctor_num_params: int;
  ctor_return_ty_params: ty list option;
  ctor_existentials: int;
}
```

- `ctor_type_name` -- The name of the variant type this constructor belongs to
  (e.g. `"option"` for `Some`).
- `ctor_arg_ty` -- The type of the constructor's argument, if it has one, using
  `TGen` for type parameters. `Some` has `Some (TGen 0)`; `None` has `None`.
- `ctor_num_params` -- How many type parameters the parent type has. For
  `option` this is 1. The typechecker uses this to know how many fresh variables
  to create when instantiating the constructor.
- `ctor_return_ty_params` -- For regular ADT constructors this is `None`. For
  GADT constructors that specify an explicit return type, this is
  `Some <params>` where `<params>` is the list of type arguments in the return
  type. These params may be concrete types (constraining the type parameter) or
  `TGen` references (leaving it polymorphic).
- `ctor_existentials` -- The number of existential type variables introduced by
  this constructor. For regular constructors this is `0`. Existential type
  variables are type variables that appear in the constructor's argument type
  but not in the parent type's parameter list. They receive `TGen` indices
  starting at `ctor_num_params`.

**GADT examples.** Consider the following cases:

- **Regular constructor** (`Some` from `type 'a option`):
  `ctor_return_ty_params = None`, `ctor_existentials = 0`. No GADT features
  are in use.

- **Return type refinement** (`IntLit : int -> int expr` from
  `type 'a expr = ...`): `ctor_return_ty_params = Some [TInt]`,
  `ctor_existentials = 0`. The parent type has one parameter (`'a`), and
  this constructor fixes it to `int`. When pattern matching on `IntLit`, the
  typechecker learns that `'a = int` in that branch.

- **Existential types** (`AnyShow : 'a * ('a -> string) -> any_show` from
  `type any_show = ...`): `ctor_return_ty_params = Some []`,
  `ctor_existentials = 1`. The parent type `any_show` has zero parameters, so
  the return type params list is empty. The `'a` in the argument type is
  existential -- it does not appear in the result type. It gets `TGen` index
  `ctor_num_params` (i.e. `TGen 0` here, since `any_show` has zero
  parameters). Inside a match branch for `AnyShow`, the existential is
  skolemized: it can be used consistently within the branch but cannot escape.

## The Typing Context

While `type_env` tracks type definitions, the typing context `ctx` tracks the
local typing state during inference. It is defined in `lib/typechecker.ml`:

```ocaml
type ctx = {
  vars: (string * Types.scheme) list;
  mutable_vars: string list;
  type_env: Types.type_env;
  loop_info: loop_info option;
  current_module: string option;
  constraint_tvars: int list;
}
```

- **vars** -- An association list mapping variable names to their type schemes.
  This is intentionally a list, not a map. When you bind a new variable, you
  prepend it: `(name, scheme) :: ctx.vars`. When you look up a variable, the
  first match wins. This means shadowing works for free -- a new binding with
  the same name simply appears earlier in the list and hides the old one. There
  is no need to remove the old entry.

- **mutable_vars** -- Which variable names were declared with `let mut`. The
  typechecker consults this list when it encounters an assignment expression to
  verify the target is mutable.

- **type_env** -- The type environment described above. Carried inside the
  context so that it is available everywhere during inference.

- **loop_info** -- Whether inference is currently inside a loop, and if
  so, what kind: `WhileLoop` for while loops, `UnitLoop` for iterator
  for loops, or `FoldLoop acc_name` for fold loops (with an accumulator
  variable name). This controls how `break` and `continue` expressions
  are typechecked.

- **current_module** -- If inference is inside a module definition, this holds
  the module name. Used for qualifying type names.

- **constraint_tvars** -- A list of type variable IDs that are under typeclass
  constraints. Normally, when the typechecker sees an arithmetic operator like
  `+` applied to an unknown type variable, it defaults that variable to `int`.
  But if the variable is constrained by a typeclass (for example, by a `Num`
  constraint in a `where` clause), defaulting would be wrong -- the variable
  must remain polymorphic. This list prevents that premature defaulting.

The context is threaded through inference functionally: functions receive a `ctx`
and return results, and when they need to extend it they create a new value with
updated fields. The exception is the type variables themselves, which are mutable
ref cells -- this is the controlled point of mutation in an otherwise functional
architecture.

## The Typed AST

The output of type inference is a typed AST where every expression node carries
its inferred type:

```ocaml
type texpr = {
  expr: texpr_kind;
  ty: Types.ty;
}
```

The `texpr_kind` type mirrors the source AST (`Ast.expr`) but with a `TE`
prefix: `TEInt`, `TEVar`, `TELet`, `TEFun`, `TEApp`, `TEMatch`, and so on.
The structure is the same -- only now every node is annotated with the `ty` that
inference determined.

For example, the expression `fun x -> x + 1` produces a `texpr` like:

```
{ expr = TEFun("x",
    { expr = TEBinop(Add,
        { expr = TEVar "x"; ty = TInt },
        { expr = TEInt 1;   ty = TInt });
      ty = TInt });
  ty = TArrow(TInt, EffEmpty, TInt) }
```

Every subexpression carries its type. The variable `x` has type `TInt` (resolved
by unification with the `+` operator). The function itself has type
`TArrow(TInt, EffEmpty, TInt)` -- the `EffEmpty` indicates it is pure.

The typed AST serves two consumers:

1. **The constraint transformation pass** -- Rewrites the typed AST to insert
   dictionary-passing for typeclass constraints, turning typeclass polymorphism
   into explicit function arguments.

2. **The compiler backend** -- Translates the typed AST into executable code,
   using the type annotations to select appropriate runtime representations and
   operations.

## Summary

The type representation has three layers:

1. **ty** -- The tree structure of types, built from primitives, type
   constructors (arrow, tuple, list, etc.), and type variables.

2. **tvar ref** -- Mutable cells that start as `Unbound` and get set to `Link`
   during unification. This is the union-find structure that makes inference
   incremental: you can create unknowns, constrain them later, and the
   constraints propagate through the shared references.

3. **scheme** -- Frozen polymorphic types where the quantified positions use
   `TGen` indices instead of mutable variables. Schemes live in the typing
   context and get instantiated (replacing `TGen` with fresh `TVar`) at each
   use site.

With these structures in hand, the next chapter covers unification: the
algorithm that takes two types and either makes them equal (by mutating type
variables) or reports an error.
