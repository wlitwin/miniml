# Module System

MiniML has a simple but effective module system for namespacing and encapsulation. Modules group related types, functions, and type class definitions under a single name, with explicit control over what is visible to the outside world.

## Defining Modules

A module is declared with `module Name = ... end`. Everything inside the module is scoped under `Name.`:

```
module Math =
  pub let pi = 3.14159
  pub let square x = x * x
  pub let double x = x + x
end

Math.square 5       -- 25
Math.double 7       -- 14
Math.pi             -- 3.14159
```

The module body can contain any of the following declarations:

- `let` and `let rec` bindings
- `let mut` mutable bindings
- `type` definitions (variants, records, aliases)
- `class` and `instance` declarations
- `effect` declarations
- `extern` declarations
- Nested `module` declarations
- `open` declarations

## Visibility: `pub`, `opaque`, and Private

By default, all bindings inside a module are **private** -- they cannot be accessed from outside. Use the `pub` keyword to make a declaration public.

```
module M =
  let helper x = x * 2         -- private: only visible inside M
  pub let double_add x y =     -- public: accessible as M.double_add
    helper x + helper y
end

M.double_add 3 4    -- 14
M.helper 3          -- type error: helper is not accessible
```

The `pub` keyword works on all declaration forms:

```
module Shapes =
  pub type color = Red | Green | Blue

  pub let to_int c = match c with
    | Red -> 0
    | Green -> 1
    | Blue -> 2

  pub class Drawable 'a =
    draw : 'a -> string
  end
end
```

### Opaque Types

The `opaque` keyword exports a type name without exposing its constructors. Code outside the module can refer to the type, but cannot construct or pattern-match on its values directly. This is how you build abstract data types:

```
module Token =
  opaque type t = Wrap of int
  pub let make x = Wrap x
  pub let get h = match h with
    | Wrap n -> n
end

Token.get (Token.make 42)   -- 42
Token.Wrap 42               -- type error: constructor is hidden
```

The module controls all creation and access, enforcing any invariants it needs.

### Opaque Newtypes

The `opaque` keyword also works with newtypes. This hides the constructor
while keeping the type visible — useful for zero-cost abstract types:

```
module UserId =
  opaque newtype t = UserId of int
  pub let make n = UserId n
  pub let to_int (UserId n) = n
  pub let compare (UserId a) (UserId b) = a - b
end

UserId.make 42              -- works
UserId.to_int (UserId.make 42)  -- 42
UserId.UserId 42            -- type error: constructor is hidden
```

Since newtypes have zero runtime overhead (the constructor is erased), opaque
newtypes give you type-safe abstraction with no performance cost.

### Instances Are Always Public

Type class instances defined inside a module are always exported, regardless of whether they have `pub`. This matches the behavior you would expect -- instances are global and incoherent if hidden:

```
module M =
  pub type wrapper = W of int
  instance Show wrapper =
    let show w = match w with
      | W n -> $"W({n})"
  end
end

show (M.W 42)    -- "W(42)"
```

## Accessing Module Members

Use dot notation to access public members of a module:

```
module A =
  pub let x = 1
end

module B =
  pub let x = 2
end

A.x + B.x    -- 3
```

This works for functions, values, type constructors, and type class methods:

```
module M =
  pub type color = Red | Green | Blue
  pub let to_int c = match c with
    | Red -> 0
    | Green -> 1
    | Blue -> 2
end

M.to_int M.Green    -- 1
```

### Qualified Type Annotations

Types defined in a module can be referenced in type annotations using the qualified name:

```
module Pt =
  type t = { x: int; y: int }
  pub let make (x: int) (y: int) : t = { x = x; y = y }
end

let get_y (p: Pt.t) : int = p.y in
get_y (Pt.make 10 20)    -- 20
```

This also works with parameterized types:

```
module Wrap =
  type 'a t = Val of 'a | Empty
  pub let make (x: 'a) : 'a t = Val x
  pub let get (w: 'a t) : int =
    match w with
    | Val x -> x
    | Empty -> 0
end

let unwrap (w: int Wrap.t) : int = Wrap.get w in
unwrap (Wrap.make 42)    -- 42
```

And with multiple type parameters:

```
module Pair =
  type ('a, 'b) t = P of 'a * 'b | None
  pub let make (x: 'a) (y: 'b) : ('a, 'b) t = P (x, y)
  pub let fst (p: ('a, 'b) t) : 'a =
    match p with
    | P (a, _) -> a
    | None -> 0
end

let f (p: (int, string) Pair.t) : int = Pair.fst p in
f (Pair.make 42 "hi")    -- 42
```

## Opening Modules

### Full Open

`open Module` brings all public members of the module into the current scope:

```
module M =
  pub let a = 10
  pub let b = 20
end

open M

a + b    -- 30
```

Opening also imports public types, constructors, type classes, and submodules.

### Selective Open

`open Module (name1, name2, ...)` brings only the listed names into scope:

```
module M =
  pub let a = 10
  pub let b = 20
end

open M (a)

a        -- 10
b        -- error: unbound variable b
```

This is useful when you only need a few items from a large module, or want to avoid name collisions.

### Local Open

`Module.(expr)` temporarily opens a module for the duration of a single expression:

```
module M =
  pub let x = 10
  pub let y = 20
end

M.(x + y)    -- 30
```

Outside the parentheses, `x` and `y` are not in scope. This is handy for one-off uses without polluting the surrounding namespace.

### Shadowing and Disambiguation

When an `open` brings a name that already exists in scope, the opened name shadows the existing one. You can still access the original via qualified access:

```
module M =
  pub let x = 10
end

open M

let x = 20

x + M.x    -- 30
```

## Nested Modules

Modules can be nested inside other modules. A nested module must be marked `pub` to be accessible from outside the parent:

```
module Outer =
  let secret = 100
  pub module Inner =
    pub let f x = x + secret
  end
end

Outer.Inner.f 5    -- 105
```

The inner module can access private bindings from the enclosing module (`secret` in this example), but outside code cannot access `Outer.secret` directly.

## Opening Inside Modules

A module body can `open` another module, bringing its public bindings into scope for the remainder of the module:

```
module A =
  pub let x = 10
end

module B =
  open A
  pub let y = x + 5
end

B.y    -- 15
```

## Recursive Functions in Modules

Recursive functions work inside modules with the usual `let rec` syntax:

```
module M =
  pub let rec fact n =
    if n <= 1 do 1
    else n * fact (n - 1)
end

M.fact 5    -- 120
```

## Type Classes in Modules

Type classes and instances can be defined inside modules. Mark a class `pub` to make it and its methods accessible outside:

```
module PP =
  pub class Pretty 'a =
    pretty : 'a -> string
  end
  instance Pretty int =
    let pretty x = "<" ^ string_of_int x ^ ">"
  end

  pub let show_pretty (x : 'a) : string where Pretty 'a =
    pretty x
end

PP.show_pretty 42    -- "<42>"
PP.pretty 42         -- "<42>"
```

When you open a module that contains type classes, the class and its instances are imported:

```
module MyMod =
  pub class Stringify 'a =
    to_str : 'a -> string
  end
  instance Stringify int =
    let to_str x = string_of_int x
  end
end

open MyMod
to_str 99    -- "99"
```

Different modules can define classes with the same name without conflict:

```
module A =
  pub class Processor 'a =
    process_a : 'a -> int
  end
  instance Processor int =
    let process_a x = x + 1
  end
end

module B =
  pub class Processor 'a =
    process_b : 'a -> int
  end
  instance Processor int =
    let process_b x = x * 10
  end
end

A.process_a 5 + B.process_b 3    -- 36
```

You can also add instances to a module-scoped class from outside the module using the qualified class name:

```
module PP =
  pub class Pretty 'a =
    pretty : 'a -> string
  end
  instance Pretty int =
    let pretty x = "[" ^ string_of_int x ^ "]"
  end
end

instance PP.Pretty string =
  let pretty s = "(" ^ s ^ ")"
end

PP.pretty "hello"    -- "(hello)"
```

## GADTs in Modules

GADT (Generalized Algebraic Data Type) types can be defined inside modules and accessed with qualified names, just like ordinary variant types:

```
module Expr =
  pub type 'a t =
    | IntLit : int -> int t
    | BoolLit : bool -> bool t

  pub let rec eval (e : int t) : int =
    match e with
    | IntLit n -> n
end

Expr.eval (Expr.IntLit 42)    -- 42
```

Inside the module, GADT constructors use short names for the return type (e.g., `int t` rather than `int Expr.t`). From outside, constructors and the type are accessed with the usual qualified syntax (`Expr.IntLit`, `int Expr.t`).

After opening the module, the constructors and type are available unqualified:

```
open Expr

eval (IntLit 42)    -- 42
```

## Standard Library Modules

MiniML ships with several built-in standard library modules that follow the same conventions. You can access them with qualified names or open them:

```
String.length "hello"       -- 5
String.split " " "a b c"   -- ["a"; "b"; "c"]
List.map (fn x -> x * 2) [1; 2; 3]    -- [2; 4; 6]
Stdlib.mod 10 3             -- 1
Stdlib.string_of_int 42     -- "42"
```

## Module Patterns

### Abstract Data Types

Use `opaque` types to hide implementation details and enforce invariants through smart constructors:

```
module Id =
  opaque type t = Id of int
  let mut counter = 0
  pub let next () =
    counter := counter + 1;
    Id counter
  pub let to_int id = match id with
    | Id n -> n
end

let id1 = Id.next ()
let id2 = Id.next ()
Id.to_int id1    -- 1
Id.to_int id2    -- 2
```

### Organizing Related Functions

Group related operations under a module to avoid name collisions and make intent clear:

```
module Vec2 =
  pub let make x y = (x, y)
  pub let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  pub let scale k (x, y) = (k * x, k * y)
  pub let dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2
end

let v = Vec2.add (Vec2.make 1 2) (Vec2.make 3 4)
```

### Encapsulating Type + Operations

A common pattern is to define a type alongside all operations on it in a single module:

```
module Stack =
  pub type 'a t = Empty | Push of 'a * 'a t
  pub let empty = Empty
  pub let push x s = Push (x, s)
  pub let pop s = match s with
    | Empty -> (None, Empty)
    | Push (x, rest) -> (Some x, rest)
  pub let is_empty s = match s with
    | Empty -> true
    | Push _ -> false
end

let s = Stack.push 3 (Stack.push 2 (Stack.push 1 Stack.empty))
let (top, rest) = Stack.pop s
```
