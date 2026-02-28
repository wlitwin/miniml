# The MiniML Language

MiniML is a statically-typed functional programming language that blends ML's powerful type inference with Haskell-style type classes and algebraic effects. Programs are fully type-checked at compile time with almost no annotations required — the compiler infers types through Hindley-Milner unification — while type classes provide ad-hoc polymorphism for operators and common interfaces.

What makes MiniML distinctive is how it combines features that rarely coexist in a single language: structural records with row polymorphism (like TypeScript), polymorphic variants (like OCaml), type classes with dictionary passing and functional dependencies (like Haskell), algebraic effects with first-class continuations (like Eff/Koka), and controlled mutability (like OCaml) — all unified under a clean, expression-oriented syntax.

## A Taste of the Language

```
-- A polymorphic binary tree with derived printing
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
  deriving Show

-- Insert into a binary search tree (requires Ord constraint)
let rec insert x (t : 'a tree) where Ord 'a =
  match t with
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (left, v, right) ->
    if x < v do Node (insert x left, v, right)
    else if x > v do Node (left, v, insert x right)
    else t
;;

-- Build a tree from a list using fold
let tree =
  for x in [5; 3; 7; 1; 4; 6; 8] with t = Leaf do
    insert x t
  end
;;

print $"tree: {show tree}"

-- Flatten to sorted list using algebraic effects
effect Yield =
  yield : int -> unit
end

let rec emit t =
  match t with
  | Leaf -> ()
  | Node (left, v, right) ->
    emit left;
    perform yield v;
    emit right
;;

let sorted =
  let mut result = [] in
  handle
    emit tree
  with
  | return _ -> List.rev result
  | yield v k ->
    result := v :: result;
    resume k ()
;;

print $"sorted: {show sorted}"
```

GADTs let you encode type-level invariants directly in constructors, and
locally abstract types enable polymorphic recursion over them:

```
type 'a expr =
  | IntLit : int -> int expr
  | BoolLit : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

let rec (type 'a) eval (e : 'a expr) : 'a =
  match e with
  | IntLit n -> n
  | BoolLit b -> b
  | Add (a, b) -> eval a + eval b
  | If (cond, t, f) -> if eval cond do eval t else eval f
```

## Key Features

- **Type inference** — Types are inferred; annotations are optional and only needed for disambiguation
- **Type classes** — `Num`, `Eq`, `Ord`, `Show`, `Iter`, `Index`, and more, with `where` constraints, `deriving`, and functional dependencies
- **GADTs** — Generalized Algebraic Data Types for encoding richer invariants in constructors, with polymorphic recursion via `(type 'a)` for type-safe evaluators, formatters, and more
- **Algebraic effects** — First-class effect handlers (`handle/with`, `try/with`, `provide/with`) with one-shot and multi-shot continuations; effects are tracked in the type system with optional explicit annotations (`/ IO`, `/ pure`) and unhandled effects are caught at compile time
- **Pattern matching** — Exhaustiveness-checked, with guards, or-patterns, as-patterns, pin patterns (`^x`), and destructuring
- **Newtypes** — Zero-cost type wrappers (`newtype email = Email of string`) with constructor erasure, `deriving`, and `opaque newtype` in modules for abstract types
- **Recursive values** — `let rec l = 1 :: 2 :: l` creates true cyclic data structures via pre-allocation and backpatching
- **Structural records** — Row polymorphism with width subtyping: a function expecting `{x: int; ..}` accepts any record with an `x` field
- **Polymorphic variants** — Structural variant types (`` `Foo ``, `` `Bar 42 ``) that work without type declarations, with exact, lower-bound, and upper-bound type annotations
- **Modules** — Namespacing with `pub`/private visibility, opaque types, selective `open`
- **Rich collections** — Lists, arrays, maps, and sets with literal syntax, iteration support, map update syntax (`#{m with k: v}`), and uniform indexing (`.[expr]`)
- **Controlled mutability** — `let mut` for mutable locals, `mut` fields in records, `:=` for assignment
- **Looping** — Unified `for` syntax: conditional loops, `for`/`in` with pattern destructuring (`for (k, v) in pairs do ... end`), fold loops, infinite loops, and while-let
- **Control flow** — `break`, `continue`, and `return` as loop control primitives
- **Mutual recursion** — `let rec ... and ...` for functions, `type ... and ...` for types
- **Lazy sequences** — Infinite sequences powered by algebraic effects under the hood
- **Pipe operator** — `value |> f |> g` for readable data transformation pipelines

## Documentation

- **[Syntax Reference](syntax.md)** — Complete guide to expressions, operators, patterns, and declarations
- **[Type System](types.md)** — Type inference, polymorphism, type classes, structural subtyping, and how it compares to OCaml and Haskell
- **[Algebraic Effects](effects.md)** — Effect declarations, handlers, continuations, and practical patterns
- **[Standard Library](stdlib.md)** — Built-in functions, type classes, and module reference
- **[Module System](modules.md)** — Defining modules, visibility, opaque types, and open declarations
- **[JavaScript VM & JS Backend](js-vm.md)** — Running MiniML in Node.js or the browser via the bytecode VM or standalone JS compilation
