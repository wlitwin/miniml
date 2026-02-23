# Type Refinement: GADTs and Alternatives

> **Note:** This document is historical. GADTs have since been fully implemented
> using the snapshot/restore approach recommended below. See `docs/types.md` and
> `docs/typesystem/07-advanced.md` for current GADT documentation.

This document surveys type refinement approaches — mechanisms that let the typechecker learn more precise type information inside conditional branches. We evaluate each against MiniML's current type system (HM inference, type classes, structural record subtyping) and consider implementation feasibility.

## GADTs (Generalized Algebraic Data Types)

GADTs let constructors constrain the type parameters of their parent type. In standard ADTs, every constructor produces the same `'a t`. With GADTs, each constructor can fix the parameters to specific types:

```ocaml
type 'a expr =
  | IntLit : int -> int expr
  | BoolLit : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | Equal : int expr * int expr -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
```

When you match on `IntLit`, the typechecker learns `'a = int` inside that branch. This enables:

### Type-safe evaluators

```ocaml
let rec eval : type a. a expr -> a = function
  | IntLit n -> n          (* 'a ~ int, returning int is valid *)
  | BoolLit b -> b         (* 'a ~ bool *)
  | Add (l, r) -> eval l + eval r
  | Equal (l, r) -> eval l = eval r
  | If (c, t, e) -> if eval c do eval t else eval e
```

Without GADTs, `eval` must return a `value` sum type and do runtime tag checks.

### Type-level witnesses

```ocaml
type ('a, 'b) eq = Refl : ('a, 'a) eq
let cast : ('a, 'b) eq -> 'a -> 'b = fun Refl x -> x
```

### Existential types (implicitly)

Type variables that appear in the constructor argument but not the return type are existentially quantified:

```ocaml
type any_show = AnyShow : 'a * ('a -> string) -> any_show
```

### What it would take to add

**Syntax:** Add `: return_type` to constructor definitions.

**Data representation:** Extend `ctor_info` with a return type and existential count:

```ocaml
type ctor_info = {
  ctor_type_name: string;
  ctor_arg_ty: ty option;
  ctor_return_ty: ty list option;  (* None = standard ADT, Some = GADT *)
  ctor_num_params: int;
  ctor_existentials: int;
}
```

**Pattern matching (the hard part):** Currently, `check_pattern` creates fresh type variables for the variant's parameters and unifies the scrutinee with `TVariant(name, [fresh_vars])`. With GADTs, it must unify against the constructor's specific return type, producing local type equations scoped to that branch.

The fundamental problem: MiniML's unification is global and destructive (mutates ref cells via `Link`). GADT branches need local type equations. Two approaches:

- **Snapshot/restore:** Save the union-find state before each branch, restore after. Simple but fragile.
- **Local constraint environments:** Collect equations as constraints threaded through `synth`/`check`. Cleaner but requires plumbing constraints through the entire typechecker.

**Inference:** GADTs break complete inference. Functions matching on GADT constructors require type annotations. The typechecker must detect GADT matches and require annotations (or emit a clear error).

**Exhaustiveness:** Must filter impossible constructors — if the scrutinee is `int expr`, then `BoolLit` and `Equal` can't match (their return type doesn't unify with `int expr`).

**Type classes interaction:** The constraint transformation pass would need awareness of GADT refinements, since dictionary resolution may depend on refined types.

**Overall cost: High.** Touches nearly every part of the typechecker. Most valuable for self-hosting (typed ASTs, type-safe IR), less critical for application code.

## Flow Typing (Occurrence Typing)

Used by TypeScript, Kotlin, Dart, and Typed Racket. When a runtime check narrows a value's type, the typechecker tracks that narrowing in subsequent code.

```typescript
function f(x: string | number) {
  if (typeof x === "string") {
    x.toUpperCase()   // x is string here
  } else {
    x + 1             // x is number here
  }
}
```

The typechecker maintains a narrowing environment — a map from variables to refined types in the current branch. This is purely control-flow analysis, no new type syntax needed.

### What it gives you

Safe narrowing after `match`, `if`, null checks, and tag checks. The common case of "I checked what this is, now let me use it." Very intuitive — users don't learn anything new.

### What it doesn't give you

No type-level computation, no existentials, no way to express "this function's return type depends on which constructor was passed in." You can narrow within a branch, but you can't make the function's signature reflect the narrowing.

### In MiniML

Pattern matching on variants already gives implicit narrowing (matching `Some x` tells you `x` has the inner type). The gain would be narrowing in `if` branches:

```
if Option.is_some x do
  Option.get x    -- compiler knows x is Some _ here
end
```

### Implementation

Extend `ctx` to carry narrowing info per variable, updated at branch points. Decide which checks the compiler recognizes (match arms are easy, arbitrary boolean expressions are harder). Doesn't touch unification at all.

**Cost: Low to medium.** Incremental, doesn't affect core type machinery.

**Trade-off vs GADTs:** Much simpler, very user-friendly, but fundamentally limited to refining existing variables — can't express type-level relationships.

## Refinement Types

Used by Liquid Haskell, F*, and (partially) Ada. Types carry logical predicates:

```
type pos = { v: int | v > 0 }
type sorted_list = { v: int list | is_sorted v }
let head (xs: { v: 'a list | length v > 0 }) : 'a = ...
```

Types are augmented with logical formulas. The typechecker emits verification conditions and sends them to an SMT solver (Z3). The solver proves predicates are maintained across function boundaries.

### What it gives you

Extremely precise types. Prove array accesses are in bounds, division never hits zero, data structures maintain invariants. Way more expressive than GADTs for value-level properties.

### What it doesn't give you

Bad at the structural things GADTs handle (typed ASTs, type-safe heterogeneous data). Great at arithmetic/logical properties.

### Implementation

Requires an SMT solver dependency, a predicate language, a verification condition generator, and heuristics for inference vs. annotation.

**Cost: Very high.** Essentially bolting a theorem prover onto the typechecker.

**Trade-off vs GADTs:** Orthogonal strengths. Refinement types prove `array index < length` easily but can't express `eval : 'a expr -> 'a`. GADTs do the opposite.

## Dependent Types

Used by Idris, Agda, Lean, Coq. Types can depend on values:

```idris
data Vec : Nat -> Type -> Type where
  Nil  : Vec 0 a
  Cons : a -> Vec n a -> Vec (n + 1) a

append : Vec n a -> Vec m a -> Vec (n + m) a
```

Full dependent types (Agda/Coq) turn the language into a proof assistant — type checking is undecidable, you need termination proofs, programs become proofs.

Lightweight dependent types (Idris 2) are more practical. Most programs only need dependency on natural numbers and simple enums, not arbitrary terms.

### What it gives you

Everything GADTs give you, plus value-indexed types. Can express any invariant.

### Implementation

Requires unifying the term and type languages (or allowing type-level computation), adding type-level reduction, handling interaction with inference.

**Cost: Extremely high for full, high for lightweight.**

**Trade-off vs GADTs:** Strictly more powerful, but massive complexity increase. GADTs are the sweet spot for type-safe interpreters and existentials without going full dependent.

## First-Class Existential Types

Instead of getting existentials through GADTs, add them directly:

```
type any_show = exists 'a. ('a * ('a -> string))

let pack : any_show = pack (42, string_of_int)
let show_it (x: any_show) =
  let (v, f) = unpack x in f v
```

`pack` hides a type, `unpack` reveals it as a fresh skolem variable scoped to the unpacking expression. The typechecker ensures the hidden type never escapes.

### What it gives you

Heterogeneous collections, abstract types, module-like encapsulation at the value level. The "existential" half of GADTs without the "type refinement" half.

### What it doesn't give you

Type refinement. Can't write `eval : 'a expr -> 'a` because matching can't constrain type parameters.

### In MiniML

Type classes already cover much of this — `Show` abstracts over "anything showable." Direct existentials give the same without defining a class.

### Implementation

Needs skolemization (fresh rigid type variables that can't be unified) and an escape check (existential variable can't appear in the result type). Doesn't require changing unification.

**Cost: Medium.**

**Trade-off vs GADTs:** Much simpler, gives data-hiding, but none of the type-refinement.

## Union Types + Narrowing

TypeScript's full approach combines untagged union types with flow typing:

```typescript
type Value = string | number | boolean   // untagged — no wrapper

function len(x: string | string[]) {
  if (typeof x === "string") return x.length
  else return x.length  // different .length!
}
```

Untagged unions mean a value can be literally a string or a number with no wrapper. Combined with flow typing, you get narrowing without explicit destructuring.

### In MiniML

This would be a significant departure. Adding untagged unions means `int | string` with no runtime tag. Interacts badly with HM inference (unions make principal types harder) but naturally with structural record typing.

### Implementation

Union/intersection types change the type algebra. Inference with unions requires subtyping constraints rather than equality constraints — unification becomes constraint solving.

**Cost: High.** Fundamentally changes the type system.

## Recommendation for MiniML

Given the current type system (HM inference, type classes, structural records, bytecode compilation):

1. **Flow typing on variant matches** — lowest-hanging fruit. Already implicit in pattern matching; extending to `if` branches is incremental and doesn't touch unification. Immediately useful.

2. **First-class existentials** — next step if the "pack a value with its operations" pattern is needed. Pairs well with type classes, relatively contained implementation.

3. **GADTs** — worth it for self-hosting (typed IR, type-safe AST representations). Biggest undertaking, but the payoff is real if the compiler is written in its own language.

The pragmatic path: flow typing first (cheap and useful), existentials if needed, GADTs when self-hosting demands a typed AST.
