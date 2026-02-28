# The MiniML Type System

MiniML has a unique flavor of Hindley-Milner type inference. It starts from the same foundation as OCaml and Haskell -- complete type inference with let-polymorphism -- but then combines features that rarely coexist: Haskell-style type classes for ad-hoc polymorphism, structural record subtyping for flexible data passing, algebraic effects instead of monads for side effects, and automatic deriving for common interfaces. The result is a language where you almost never write type annotations, yet get the full safety of a static type system.

This document is a deep dive into how the type system works, where it diverges from OCaml and Haskell, and what makes the combination distinctive.

## Type Inference

MiniML infers types through Hindley-Milner unification. Every expression gets a concrete type at compile time, but you rarely need to spell it out -- the compiler figures it out from how values are used.

```
let add x y = x + y            -- inferred: int -> int -> int
let greet name = "hello " ^ name  -- inferred: string -> string

let apply f x = f x            -- inferred: ('a -> 'b) -> 'a -> 'b
let compose f g x = f (g x)    -- inferred: ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
```

The inference engine works by assigning fresh type variables to unknown types, then unifying constraints as it walks the expression tree. When you write `x + y`, the compiler knows both `x` and `y` must be the same numeric type, and the result is that type too. When you write `f x`, it knows `f` must be a function whose parameter type matches `x`.

### Let-Polymorphism

Generalization happens at `let` boundaries. When a `let` binding's type contains unresolved type variables, those variables are quantified -- turned into polymorphic type parameters. This means the binding can be used at different types in the body:

```
let id x = x in
let a = id 5 in       -- id used at int -> int
let b = id true in     -- id used at bool -> bool
a                      -- works fine: a is int, b is bool
```

Without let-polymorphism, `id` would be locked to whichever type it was first used at. The key insight of Hindley-Milner is that generalization at `let` boundaries is both sound and complete -- you get maximum polymorphism without any annotations.

Internally, generalization replaces unbound type variables (those not constrained by the surrounding context) with quantified variables (`TGen` indices). When the binding is used, instantiation replaces those quantified variables with fresh type variables, allowing independent inference at each use site.

### Value Restriction for Mutability

Mutable variables are not generalized. This is the standard value restriction: since a mutable cell is shared across all uses, allowing it to be polymorphic would be unsound.

```
let mut x = [] in    -- x has type 'a list, but NOT polymorphic
x := [1];            -- fixes x to int list
x                    -- int list
```

If `x` were generalized, you could store an `int list` and read it back as a `string list`. MiniML prevents this by keeping mutable bindings monomorphic.

### Polymorphic Recursion

Standard let-polymorphism generalizes at `let` boundaries, but recursive functions are an exception: during type-checking of the body, the recursive binding is monomorphic. This means a `let rec` function cannot call itself at a different type instantiation — every recursive call must use the same types as the definition.

This is usually fine, but becomes a limitation with GADTs and other polymorphic data structures where recursion naturally changes the type parameter. MiniML supports polymorphic recursion through locally abstract types — the `(type 'a)` syntax. See the [Polymorphic Recursion with GADTs](#polymorphic-recursion-with-gadts) section for details.

## Basic Types

MiniML has the following primitive types:

| Type     | Description                | Examples                     |
|----------|----------------------------|------------------------------|
| `int`    | Integer (arbitrary size)   | `42`, `0`, `-7`              |
| `float`  | Floating-point             | `3.14`, `0.0`, `-1.5`       |
| `bool`   | Boolean                    | `true`, `false`              |
| `string` | UTF-8 string               | `"hello"`, `$"x = {x}"`     |
| `byte`   | Single byte (0-255)        | `#41`, byte literals         |
| `rune`   | Unicode code point         | `'a'`, `'\n'`               |
| `unit`   | The unit type              | `()`                         |

Compound types are built from these:

| Type                    | Description           | Examples                       |
|-------------------------|-----------------------|--------------------------------|
| `'a -> 'b`             | Function              | `fn x -> x + 1`               |
| `'a * 'b`              | Tuple                 | `(1, "hello")`, `(true, 3, 4)`|
| `'a list`              | Linked list           | `[1; 2; 3]`, `[]`             |
| `'a array`             | Mutable array         | `#[1; 2; 3]`, `#[]`           |
| `('k, 'v) map`         | Immutable map (newtype)| `#{"a": 1; "b": 2}`          |
| `'a set`               | Immutable set (newtype)| `#{1; 2; 3}`                  |
| `'a option`            | Optional value        | `Some 42`, `None`             |
| `{ x: int; y: string }`| Record                | `{ x = 1; y = "hi" }`         |

## Polymorphism

Type variables are written with a leading apostrophe: `'a`, `'b`, `'c`, etc. A function is polymorphic when its inferred type contains type variables:

```
let id x = x                -- 'a -> 'a
let fst (a, b) = a          -- 'a * 'b -> 'a
let const x y = x           -- 'a -> 'b -> 'a
```

Polymorphic functions can be used at any type that matches the shape:

```
let id x = x
;;
id 42         -- int
id "hello"    -- string
id [1; 2; 3]  -- int list
```

### Polymorphic Data Structures

Generic data structures work naturally:

```
let rec length xs =
  match xs with
  | [] -> 0
  | _ :: rest -> 1 + length rest
-- inferred: 'a list -> int

let rec map f xs =
  match xs with
  | [] -> []
  | x :: rest -> f x :: map f rest
-- inferred: ('a -> 'b) -> 'a list -> 'b list
```

Because `length` never inspects the elements, only the list structure, it works on any `'a list`. The compiler infers this automatically.

### Polymorphic Type Definitions

Type definitions can be parameterized with type variables:

```
type 'a option = None | Some of 'a
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
type ('a, 'b) either = Left of 'a | Right of 'b
```

The type parameters appear before the type name using the ML convention: `'a option` rather than `option<'a>`.

## Type Annotations

Type annotations are almost never required. When you do use them, the syntax is `(expr : type)` for expressions, and annotations can appear on function parameters, return types, and let bindings.

### On Function Parameters

```
let add (x: int) (y: int) = x + y
let greet (name: string) : string = "hello " ^ name
```

### On Expressions

Any expression can be annotated with a type:

```
let xs = ([] : int list)       -- annotate an empty list
let n = (42 : int)             -- redundant but valid
```

### With Type Variables

Annotations can use type variables. The same variable name refers to the same type within a binding:

```
let id (x: 'a) : 'a = x       -- explicitly polymorphic
let unwrap (x: int option) = match x with Some v -> v | None -> 0
```

### When Annotations Are Actually Needed

In practice, you need annotations in a few situations:

1. **Empty collections** -- The compiler cannot infer the element type of `[]` or `#[]` without context:
   ```
   let xs : int list = []
   let empty_arr : int array = #[]
   ```

2. **Ambiguous record fields** -- When multiple record types share a field name and the compiler cannot determine which type is intended:
   ```
   type point = { x: int; y: int }
   type vec = { x: float; y: float }
   let get_x (p: point) = p.x    -- annotation disambiguates
   ```

3. **GADT match expressions** -- Functions that pattern match on GADT types require a return type annotation so the compiler can verify that each branch is consistent with the type refinement introduced by the constructor (more on this in the GADTs section).

4. **Constrained functions** -- Functions with `where` clauses need annotations on the constrained type variables so the compiler can connect constraints to parameters (more on this in the Type Classes section).

### Effect Annotations

Functions can optionally annotate their return type with effects. The effect annotation goes after the return type, separated by `/`:

```
let greet (name: string) : string / IO =
  perform print ("hello " ^ name);
  name
```

This declares that `greet` returns a `string` and performs the `IO` effect. You can mark a function as explicitly pure with `/ pure`:

```
let add (x: int) (y: int) : int / pure = x + y
```

Multiple effects are comma-separated:

```
let stateful_io (x: int) : string / State int, IO =
  let s = perform get () in
  perform print (show s);
  show (s + x)
```

Effect annotations are entirely optional. Omitting the annotation means effects are inferred as before -- this is fully backward compatible. You only need effect annotations when you want to document or constrain the effects a function is allowed to perform. The compiler checks that the annotated effects match the actual effects in the function body.

## Type Definitions

### Variant Types (Algebraic Data Types)

Variant types define a closed set of cases, each optionally carrying data:

```
type color = Red | Green | Blue

type shape =
  | Circle of int
  | Rect of int * int

type 'a option = None | Some of 'a

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
```

Constructors are first-class values -- they can be passed as functions:

```
let xs = map Some [1; 2; 3]    -- [Some 1; Some 2; Some 3]
let apply f x = f x
apply Box 10                   -- Box(10)
```

Pattern matching on variants is exhaustiveness-checked:

```
type color = Red | Green | Blue

match c with
| Red -> 1
| Green -> 2
-- Error: non-exhaustive match, missing: Blue
```

### Generalized Algebraic Data Types (GADTs)

GADTs extend variant types by allowing each constructor to specify a full type signature for its return type, rather than always returning the same type. This lets different constructors produce different instantiations of the type parameters, enabling the compiler to refine types when pattern matching.

#### Syntax

GADT constructors use `:` instead of `of`, followed by a complete type signature:

```
type 'a expr =
  | IntLit : int -> int expr
  | BoolLit : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
```

Each constructor declares not just its argument types, but its specific return type. `IntLit` takes an `int` and produces an `int expr`, while `BoolLit` takes a `bool` and produces a `bool expr`. This is impossible with regular variants, where every constructor returns the same `'a expr`.

Regular constructors and GADT constructors cannot be mixed in the same type definition. If any constructor uses `:`, all constructors in that type must use `:`.

#### Type Refinement

The key power of GADTs is type refinement during pattern matching. When you match on a GADT constructor, the compiler learns something about the type parameter and narrows it within that branch:

```
let rec eval (e : 'a expr) : 'a =
  match e with
  | IntLit n -> n          -- 'a is refined to int, so n : int is a valid 'a
  | BoolLit b -> b         -- 'a is refined to bool, so b : bool is a valid 'a
  | Add (l, r) -> eval l + eval r    -- 'a is int, so (+) works
  | If (cond, t, f) ->
    if eval cond do eval t else eval f
```

In the `IntLit` branch, the compiler knows `'a = int` because `IntLit` returns `int expr`. This means the branch body must produce an `int`, and `n` (which is an `int`) satisfies the return type `'a`. Without GADTs, there is no way to write a type-safe evaluator like this -- you would need runtime type tags or unsafe casts.

#### Annotation Requirements

Type annotations are required on functions that match on GADTs. The compiler needs to know the return type of the match expression to verify that each branch is consistent with its refined type:

```
-- This works: return type 'a is annotated
let eval (e : 'a expr) : 'a = match e with ...

-- This fails: the compiler cannot infer the return type
let eval e = match e with ...
```

The annotation tells the compiler what role the type parameter plays in the result, which is essential for checking that each branch produces the right type after refinement.

#### Existential Types

When a GADT constructor introduces type variables in its arguments that do not appear in the return type, those variables are existential -- they represent a type that exists but is unknown to the code that unpacks the constructor:

```
type any_show = AnyShow : 'a * ('a -> string) -> any_show
```

Here `'a` appears in the constructor arguments but not in the return type `any_show`. This means you can pack any type into `AnyShow` along with a function that knows how to show it:

```
let items = [
  AnyShow (42, string_of_int);
  AnyShow ("hello", fn s -> s);
  AnyShow (true, string_of_bool)
]
```

When you unpack an `AnyShow`, the compiler knows the two components share the same type `'a`, but you cannot know what that type is -- you can only use the provided function on the provided value:

```
let show_any (a : any_show) : string =
  match a with
  | AnyShow (x, f) -> f x    -- f and x share the same existential 'a
```

#### Type Equality Witnesses

GADTs can encode type equality as a value. The classic pattern is the `eq` type:

```
type ('a, 'b) eq = Refl : ('a, 'a) eq
```

The only constructor `Refl` requires both type parameters to be the same. When you match on `Refl`, the compiler learns that `'a` and `'b` are equal and allows them to be used interchangeably:

```
let cast (proof : ('a, 'b) eq) (x : 'a) : 'b =
  match proof with
  | Refl -> x    -- 'a = 'b, so x : 'a is also x : 'b
```

This is useful for building type-safe APIs where equality between types needs to be witnessed at runtime.

#### Polymorphic Recursion with GADTs

A key limitation of standard Hindley-Milner inference is that recursive functions are monomorphic during their own definition — a recursive call must use the same types as the outer call. This is a problem for GADTs, where you often need to recurse at a different type instantiation.

Consider a GADT expression type with both `int` and `bool` branches:

```
type 'a expr =
  | IntLit : int -> int expr
  | BoolLit : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
```

A naive recursive evaluator fails because `eval cond` (at `bool expr`) conflicts with the outer type of `eval` (which is initially monomorphic):

```
-- This fails: eval is monomorphic, can't call at different types
let rec eval (e : 'a expr) : 'a =
  match e with
  | If (cond, t, f) -> if eval cond do eval t else eval f   -- error!
```

The solution is locally abstract types — the `(type 'a)` syntax after `rec`:

```
let rec (type 'a) eval (e : 'a expr) : 'a =
  match e with
  | IntLit n -> n
  | BoolLit b -> b
  | Add (a, b) -> eval a + eval b
  | If (cond, t, f) -> if eval cond do eval t else eval f
```

The `(type 'a)` annotation tells the compiler to generalize the recursive binding from the start. Instead of binding `eval` monomorphically (which would prevent calling it at different types), the compiler builds a polymorphic scheme from the type annotations and binds `eval` with that scheme. Each recursive call then instantiates `'a` independently — `eval cond` uses `'a = bool`, while `eval t` and `eval f` use whatever `'a` the outer call has.

This is the standard approach for polymorphic recursion in ML-family languages (OCaml uses the same `(type a)` syntax). Type annotations on the parameters and return type are required — the compiler needs to know the full type signature to build the polymorphic scheme before checking the function body.

Multiple type parameters are supported: `let rec (type 'a 'b) f ...`.

### Record Types

Record types define named fields with fixed types:

```
type point = { x: int; y: int }
type person = { name: string; age: int }
```

Records support mutable fields, declared with `mut`:

```
type counter = { mut count: int; label: string }

let c = { count = 0; label = "clicks" }
c.count := c.count + 1    -- mutate the field
```

Records also support functional update syntax:

```
let p = { x = 1; y = 2 }
let p2 = { p with x = 10 }    -- { x = 10; y = 2 }
```

### Type Aliases

Type aliases create alternative names for existing types:

```
type name = string
type point = int * int
type 'a pair = 'a * 'a
type ('a, 'b) assoc = ('a * 'b) list
```

Aliases are fully transparent -- they are interchangeable with the underlying type:

```
type age = int
let birthday (a: age) : age = a + 1
birthday 25    -- works: age is just int
```

### Parameterized Types

Types can take any number of type parameters:

```
type 'a option = None | Some of 'a         -- one parameter
type ('a, 'b) either = Left of 'a | Right of 'b  -- two parameters
type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree)  -- recursive
```

When used in annotations, parameters come before the type name:

```
int option            -- 'a = int
string list           -- 'a = string
(string, int) either  -- 'a = string, 'b = int
```

### Mutual Recursion

Types that reference each other can be defined together with `type ... and ...`:

```
type 'a tree = Leaf | Node of 'a * 'a forest
and 'a forest = Empty | Trees of 'a tree list
```

All types in the group are registered simultaneously, so forward references resolve correctly. This also works for functions with `let rec ... and ...`:

```
let rec is_even n =
  if n = 0 do true else is_odd (n - 1)
and is_odd n =
  if n = 0 do false else is_even (n - 1)
```

### Newtypes

Newtypes create a distinct nominal type with a single constructor that is
erased at runtime. They provide type safety at zero cost — the constructor
wrapper disappears during compilation, so the runtime representation is
identical to the underlying type.

```
newtype email = Email of string
newtype meters = Meters of float
newtype ('k, 'v) map = MMap of ('k * 'v) list
```

Newtypes are useful for:
- **Unit safety** — prevent mixing up values with the same underlying type
  (e.g., `meters` vs `seconds`)
- **Abstraction** — wrap an implementation detail behind a named type
- **Type class instances** — define class instances for the newtype without
  affecting the underlying type

Newtypes support `deriving Show, Eq`, custom type class instances, and
`opaque newtype` in modules (which hides the constructor, similar to
`opaque type` for regular variants).

Pattern matching works normally:

```
newtype age = Age of int
let birthday (Age n) = Age (n + 1)
```

The standard library uses newtypes for maps and sets:

```
newtype ('k, 'v) map = MMap of ('k * 'v) list
newtype 'a set = MSet of ('a, unit) map
```

## Structural Record Subtyping

This is one of MiniML's most distinctive features. Records use structural subtyping with width subtyping: a record with more fields is a subtype of a record with fewer fields. A function expecting `{ x: int }` will happily accept `{ x: int; y: string; z: bool }`.

```
let get_x (r: { x: int }) : int = r.x

let p = { x = 10; y = 20; z = 30 }
get_x p    -- works! p has all the fields get_x needs
```

This is fundamentally different from OCaml's nominal records, where each record type is distinct and incompatible even if the fields are identical. In MiniML, what matters is whether the record has the right fields with the right types -- not what the record type is named.

### How It Works

During unification, when two record types are unified, MiniML checks that every field in the smaller record exists in the larger record with a compatible type. Extra fields in the larger record are simply ignored:

```
-- This function only cares about the 'name' field
let greet r = "hello " ^ r.name

-- All of these work:
greet { name = "Alice" }
greet { name = "Bob"; age = 30 }
greet { name = "Carol"; age = 25; role = "admin" }
```

The subtype relation for records is:

- **Width subtyping**: `{ x: int; y: string }` is a subtype of `{ x: int }` -- extra fields are fine
- **Field type compatibility**: matching fields must have compatible types -- `{ x: int }` is not a subtype of `{ x: string }`
- **Arrow contravariance**: function subtyping is contravariant in the argument, so a function accepting a smaller record can be used where a function accepting a larger record is expected

### Compared to OCaml

In OCaml, records are nominal -- each `type t = { x: int }` declaration creates a distinct, incompatible type. Two record types with identical fields are not interchangeable. You need modules and functors to abstract over record shapes.

In MiniML, records are structural -- compatibility is determined purely by the fields present. This makes it trivial to write functions that work on "any record with a `name` field" without any special machinery.

### Compared to TypeScript

MiniML's structural records are similar in spirit to TypeScript's structural typing, but in a fully type-inferred setting. You get the flexibility of structural types without needing to declare interfaces.

### Row Polymorphism and Open Records

MiniML supports explicit row polymorphism through open record type annotations. The `..` suffix in a record type annotation indicates that the record may contain additional fields beyond those listed:

```
let get_x (r : {x: int; ..}) : int = r.x

get_x {x = 1; y = 2}               -- works
get_x {x = 10; y = 20; z = 30}     -- works
```

Without `..`, a record type annotation is closed and matches only records with exactly those fields. With `..`, the function accepts any record that has at least the specified fields with compatible types.

This is especially useful when writing functions that operate on a subset of fields from larger records:

```
let name_and_age (r : {name: string; age: int; ..}) : string =
  $"{r.name} is {r.age}"

name_and_age {name = "Alice"; age = 30; role = "admin"}
-- "Alice is 30"
```

Row polymorphism is implemented through evidence passing in the typechecker. When a function with an open record type is compiled, the compiler tracks which fields are accessed and threads proof that the record has those fields through the generated code. This allows open record constraints to work with polymorphic functions and type class dispatch without losing type safety.

In practice, when you write a function that just accesses record fields without a type annotation, MiniML already infers a structurally-typed record -- the `..` annotation simply makes this explicit.

## Polymorphic Variants

Polymorphic variants are structural variant types that do not require a `type` declaration. While named variants (`type color = Red | Green | Blue`) are nominal -- tied to a specific type definition -- polymorphic variants are structural, identified by their tag name alone.

### Syntax

Polymorphic variant values are written with a backtick prefix:

```
`Foo                    -- nullary tag
`Bar 42                 -- tag with an int payload
`Name "hello"           -- tag with a string payload
```

### Type Annotations

Polymorphic variant types are written in square brackets with backtick-prefixed tags:

```
[`Foo | `Bar]                     -- exact: only `Foo or `Bar
[`Foo of int | `Bar of string]    -- with payload types
[> `Foo | `Bar]                   -- lower bound (at least these tags)
[< `Foo | `Bar]                   -- upper bound (at most these tags)
```

The three kinds of bounds mirror OCaml's polymorphic variant types:

- **Exact** `[`A | `B]` -- the value is one of exactly the listed tags
- **Lower bound** `[> `A | `B]` -- the value is one of the listed tags, and the type may be widened to include more
- **Upper bound** `[< `A | `B]` -- the value is one of at most the listed tags (useful as function input types)

### Pattern Matching

Polymorphic variants can be pattern matched like regular constructors:

```
match `B 42 with
| `A -> 0
| `B n -> n + 1         -- 43
```

### Coercion

Use `:>` to widen a polymorphic variant to a type with more tags:

```
let x = `A in
(x :> [> `A | `B])      -- widen to include `B
```

### When to Use Polymorphic Variants

Polymorphic variants are useful when:

- You want lightweight, ad-hoc alternatives without declaring a named type
- You need types to compose across module boundaries without a shared definition
- You are working with open-ended sets of cases (like error types)

For well-defined, closed data types, named variants are still preferred -- they provide exhaustiveness checking, clearer error messages, and `deriving` support.

## Type Classes

MiniML borrows type classes from Haskell as its mechanism for ad-hoc polymorphism. Type classes let you define a set of operations and then provide implementations for different types. The compiler automatically selects the right implementation based on the types involved.

### Defining a Class

A class declaration specifies a set of method signatures parameterized by one or more type variables:

```
class Eq 'a =
  (=) : 'a -> 'a -> bool
  (<>) : 'a -> 'a -> bool
end

class Show 'a =
  show : 'a -> string
end

class Num 'a =
  (+) : 'a -> 'a -> 'a
  (-) : 'a -> 'a -> 'a
  (*) : 'a -> 'a -> 'a
  (/) : 'a -> 'a -> 'a
  neg : 'a -> 'a
end
```

Class methods can also include effect variables. This allows instances to be either pure or effectful depending on the type:

```
class Apply 'f =
  do_thing : 'a -> 'a / 'e
end
```

Here `'e` is an effect variable -- a concrete instance can fill it in with specific effects or with `pure`, so the same class can accommodate both pure and effectful implementations.

### Creating Instances

An instance provides implementations of a class's methods for a specific type:

```
instance Eq int =
  let (=) a b = a = b
  let (<>) a b = a <> b
end

instance Show int =
  let show x = string_of_int x
end
```

Once an instance exists, the class methods work on that type:

```
show 42        -- "42"   (uses Show int)
show true      -- "true" (uses Show bool)
3 + 4          -- 7      (uses Num int)
2.0 + 3.0      -- 5.0    (uses Num float)
```

### Custom Instances

You can define instances for your own types:

```
type vec2 = { x: int; y: int }

instance Num vec2 =
  let (+) a b = { x = a.x + b.x; y = a.y + b.y }
  let (-) a b = { x = a.x - b.x; y = a.y - b.y }
  let (*) a b = { x = a.x * b.x; y = a.y * b.y }
  let (/) a b = { x = a.x / b.x; y = a.y / b.y }
  let neg a = { x = 0 - a.x; y = 0 - a.y }
end

let v1 = { x = 1; y = 2 }
let v2 = { x = 3; y = 4 }
let v3 = v1 + v2    -- { x = 4; y = 6 }
```

Now `+`, `-`, `*`, and `/` all work on `vec2` values, using the standard operator syntax.

### Multi-Parameter Type Classes

Classes can take multiple type parameters, enabling relationships between types:

```
class Convert 'a 'b =
  convert : 'a -> 'b
end

instance Convert int string =
  let convert (x: int) = string_of_int x
end

convert 42    -- "42"
```

A more involved example -- the `Iter` class relates a collection type to its element type:

```
class Iter 'a 'b =
  fold : ('c -> 'b -> 'c) -> 'c -> 'a -> 'c
end

instance Iter ('a list) 'a =
  let fold f acc xs =
    let rec go a l = match l with
      | [] -> a
      | x :: rest -> go (f a x) rest
    in go acc xs
end
```

Note the extra type variable `'c` in `fold` -- class methods can introduce their own type variables beyond the class parameters.

### Functional Dependencies

Multi-parameter type classes can be ambiguous -- when calling a method, the compiler may not know which type parameter to use for instance selection. Functional dependencies solve this by declaring that some type parameters uniquely determine others.

A functional dependency is written with `where` after the type parameters in a class definition:

```
class Iter 'c 'e where 'c -> 'e =
  fold : ('a -> 'e -> 'a) -> 'a -> 'c -> 'a
end
```

The `'c -> 'e` dependency says: given the collection type `'c`, the element type `'e` is uniquely determined. So for `int list`, the element type must be `int` -- there is no ambiguity.

Without functional dependencies, calling `fold (+) 0 [1; 2; 3]` would be ambiguous because the compiler would not know whether `'e` is `int`, `string`, or some other type. With `'c -> 'e`, once `'c` is resolved to `int list`, the compiler knows `'e = int` and can select the right instance.

The built-in `Map` class uses a similar pattern:

```
class Map 'm 'k 'v where 'm -> 'k 'v =
  get : 'k -> 'm -> 'v option
  set : 'k -> 'v -> 'm -> 'm
  ...
end
```

Here the map type `'m` determines both the key type `'k` and value type `'v`. This allows `get "a" m` to work without ambiguity when `m : (string, int) map`.

The `Index` class also uses a functional dependency:

```
class Index 'c 'k 'v where 'c -> 'k 'v =
  at : 'k -> 'c -> 'v
end
```

This powers the `.[expr]` indexing syntax. For `arr.[i]`, the compiler knows the array type determines the key type (int) and element type, so the right `at` implementation is selected automatically.

### `where` Constraints

When writing a polymorphic function that uses class methods, you declare the required constraints with `where`:

```
let show_twice (x: 'a) : string where Show 'a =
  show x ^ show x

show_twice 42       -- "4242"
show_twice "hello"  -- "hellohello"
```

Multiple constraints are separated by commas:

```
let show_eq (a: 'a) (b: 'a) : string where Show 'a, Eq 'a =
  if a = b do show a
  else show a ^ " <> " ^ show b

show_eq 1 2    -- "1 <> 2"
show_eq 3 3    -- "3"
```

Constraints can also appear on type class instances themselves, enabling conditional instances:

```
type 'a box = Box of 'a

instance Show ('a box) where Show 'a =
  let show b = match b with
    | Box x -> "Box(" ^ show x ^ ")"
end

show (Box 42)         -- "Box(42)"
show (Box (Box 7))    -- "Box(Box(7))"
```

Here, `Show ('a box)` only exists when `Show 'a` exists. The constraint is propagated: showing a `Box` of a `Box` of an `int` works because each nested level has its own `Show` instance.

### `deriving`

For common type classes, MiniML can automatically generate instances from a type definition:

```
type color = Red | Green | Blue deriving Show, Eq

show Red           -- "Red"
Red = Blue         -- false
Green <> Green     -- false
```

Records work too:

```
type point = { x: int; y: int } deriving Show, Eq

show { x = 1; y = 2 }               -- "{ x = 1; y = 2 }"
{ x = 1; y = 2 } = { x = 1; y = 2 }  -- true
```

Parameterized types derive conditional instances:

```
type 'a box = Wrap of 'a deriving Show

show (Wrap 42)    -- "Wrap(42)"
```

The derived `Show` instance for `'a box` automatically requires `Show 'a`, so it works for any type that is itself showable.

Currently, `deriving` supports `Show` and `Eq`.

Note that `deriving` cannot be used with GADT types. Attempting to do so produces an error: "cannot derive X for GADT type Y". Because GADT constructors carry different type instantiations, there is no uniform way to auto-generate instances -- you need to write them by hand.

### Built-in Type Classes

MiniML comes with several built-in type classes:

| Class      | Methods                                   | Fundeps | Instances                                   |
|------------|-------------------------------------------|---------|---------------------------------------------|
| `Num`      | `(+)`, `(-)`, `(*)`, `(/)`, `neg`        | --      | `int`, `float`                              |
| `Eq`       | `(=)`, `(<>)`                             | --      | `int`, `float`, `string`, `bool`, `byte`, `rune` |
| `Ord`      | `(<)`, `(>)`, `(<=)`, `(>=)`             | --      | `int`, `float`, `string`, `byte`, `rune`    |
| `Show`     | `show`                                    | --      | `int`, `float`, `string`, `bool`, `unit`, `byte`, `rune`, `list`, `array`, `option`, tuples |
| `Bitwise`  | `land`, `lor`, `lxor`, `lsl`, `lsr`, `lnot` | --   | `int`                                       |
| `Iter`     | `fold`                                    | `'c -> 'e` | `list`, `array`, `map`, `set`            |
| `Map`      | `of_list`, `get`, `set`, `has`, `remove`, `size`, `keys`, `values`, `to_list` | `'m -> 'k 'v` | `('k, 'v) map` |
| `Index`    | `at`                                      | `'c -> 'k 'v` | `'a array`, `string`, `('k, 'v) map` |
| `Hash`     | `hash`                                    | --      | `int`, `string`, `bool`, `byte`, `rune`     |

The `Iter` class is particularly powerful because `for` loops desugar to `fold` calls. This means any type with an `Iter` instance automatically supports `for` loops:

```
type tree = Leaf | Node of (int * tree * tree)

instance Iter tree int =
  let fold f acc t =
    let rec go a tr = match tr with
      | Leaf -> a
      | Node payload ->
        let (v, left, right) = payload in
        go (f (go a left) v) right
    in go acc t
end

let my_tree = Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))

-- for loops now work on trees:
for x in my_tree with sum = 0 do sum + x end    -- 6
```

### How Dictionary Passing Works

Under the hood, type class instances are compiled as dictionary records. Each instance becomes a record containing the method implementations, and class method calls are compiled into dictionary lookups.

When you write:

```
instance Show int =
  let show x = string_of_int x
end
```

The compiler generates something like:

```
let __dict_Show_int = { show = fn x -> string_of_int x }
```

When you call `show 42`, the compiler:

1. Determines the argument type is `int`
2. Looks up the `Show` instance for `int`
3. Emits code to load `__dict_Show_int` and extract the `show` field
4. Applies it to the argument

For constrained functions, the dictionary is resolved at the call site where the concrete type is known. The compiler follows the chain of constraints: if you call a function with `where Show 'a` using an `int`, it resolves `Show int` and threads that dictionary to the function.

For conditional instances like `Show ('a box) where Show 'a`, the compiler constructs the outer dictionary at each call site by first resolving the inner constraint. Showing a `Box (Box 42)` resolves `Show int`, uses that to build `Show (int box)`, and then uses that to build `Show (int box box)`.

## How MiniML Compares

### vs OCaml

**Type classes instead of modules/functors.** OCaml uses modules and functors for ad-hoc polymorphism -- you define a module type (signature) and then implement it for different types. This is powerful but verbose. MiniML uses type classes instead, which are more concise and provide automatic dispatch:

```
-- MiniML: just works
show 42           -- compiler picks Show int
show "hello"      -- compiler picks Show string

-- OCaml equivalent requires explicit module passing or first-class modules
```

**Structural records instead of nominal records.** In OCaml, `type t = { x: int }` and `type u = { x: int }` are incompatible types. In MiniML, any record with an `x: int` field is compatible with any function expecting one. This eliminates a whole category of "wrapper type" boilerplate.

**Unified operators.** In OCaml, `+` is for integers and `+.` is for floats. In MiniML, `+` works on any type with a `Num` instance -- `int`, `float`, or your custom types. The type class system resolves the right implementation.

**Effects instead of monads.** Where OCaml uses `result` types or exceptions for error handling, MiniML offers algebraic effects with typed handlers and first-class continuations.

### vs Haskell

**Structural records.** Haskell records are nominal and notoriously awkward -- field names are global, updating records is verbose, and different record types cannot share field names without extensions. MiniML records are structural, support width subtyping, have clean update syntax (`{ r with x = 10 }`), and field names are scoped naturally.

**Effects instead of monads.** Where Haskell uses monads and monad transformers for side effects, MiniML uses algebraic effects. Instead of wrapping everything in `IO` or `State s`, you declare effect operations and handle them with handlers:

```
-- MiniML: direct-style side effects
effect State =
  get : unit -> int
  put : int -> unit
end

handle
  let x = perform get () in
  perform put (x + 1);
  perform get ()
with
| return x -> x
| get () k -> resume k 10
| put v k -> resume k ()
```

MiniML also supports optional explicit effect annotations (`let f (x: int) : string / IO = ...`), giving you the best of both worlds: full effect inference when you want concise code, and explicit annotations when you want to document or constrain a function's effects. Haskell's `IO` monad is all-or-nothing -- a function is either pure or in `IO` -- while MiniML lets you be as precise or as hands-off as you like.

**Simpler module system.** Haskell has no module system for grouping related definitions with visibility control (only the import/export list on the module declaration). MiniML has a straightforward module system with `pub`/private visibility, opaque types, and selective `open`.

**No typeclasses for everything.** Haskell tends to encode everything through type classes (Monad, Functor, Applicative, etc.). MiniML uses type classes more sparingly -- for operator overloading and common interfaces like `Show`, `Eq`, `Iter` -- and handles effects through a separate mechanism.

### The Unique Combination

What makes MiniML's type system distinctive is not any single feature, but how the pieces fit together:

- **ML inference** means you rarely write types, but everything is statically checked
- **Type classes** give you Haskell's ad-hoc polymorphism for operators and interfaces
- **Structural records** give you TypeScript's flexibility for data shapes
- **Algebraic effects** give you direct-style effectful programming without monadic ceremony, with effects tracked in the type system so unhandled effects are caught at compile time -- and effects can be explicitly annotated on function signatures or fully inferred, your choice
- **`deriving`** gives you automatic boilerplate generation for common patterns
- **`where` constraints** let you write generic algorithms that compose with the class system

The design philosophy is pragmatic: take the best ideas from ML, Haskell, and TypeScript, and combine them into something that feels simpler than any one of those languages while retaining their strengths.
