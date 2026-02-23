# MiniML Syntax Reference

A comprehensive reference for the MiniML language, a statically-typed functional
programming language with type inference, type classes, algebraic effects, and
modules.

---

## Table of Contents

1. [Comments](#comments)
2. [Literals](#literals)
3. [Let Bindings](#let-bindings)
4. [Functions](#functions)
5. [If/Else](#ifelse)
6. [Match Expressions](#match-expressions)
7. [Patterns](#patterns)
8. [Operators](#operators)
9. [Records](#records)
10. [Tuples](#tuples)
11. [Lists](#lists)
12. [Arrays](#arrays)
13. [Maps](#maps)
14. [Sets](#sets)
15. [Polymorphic Variants](#polymorphic-variants)
16. [Type Declarations](#type-declarations)
17. [Type Classes](#type-classes)
18. [Modules](#modules)
19. [Algebraic Effects](#algebraic-effects)
20. [For Loops](#for-loops)
21. [Sequences](#sequences)
22. [String Interpolation](#string-interpolation)
23. [Pipe Operator](#pipe-operator)
24. [Semicolons and Sequencing](#semicolons-and-sequencing)
25. [Do/End Blocks](#doend-blocks)
26. [Mutable State](#mutable-state)
27. [Type Annotations](#type-annotations)
28. [Extern Declarations](#extern-declarations)

---

## Comments

### Line comments

Line comments start with `--` and extend to the end of the line.

```
-- This is a line comment
let x = 42  -- inline comment
```

### Block comments

Block comments use `(* ... *)` and are nestable.

```
(* This is a block comment *)

(* Outer comment
   (* nested comment *)
   still inside outer comment *)
```

### Special comment annotations

The `-- @partial` annotation suppresses exhaustiveness checking on the
immediately following `match` expression.

```
-- @partial
match some_option with
| Some x -> x
```

---

## Literals

### Integers

Decimal, hexadecimal, and binary integer literals.

```
0
42
1000000
0xFF            -- hexadecimal (255)
0b1010          -- binary (10)
```

### Floats

Floating-point literals require a decimal point.

```
3.14
0.5
100.0
```

### Booleans

```
true
false
```

### Strings

Strings are enclosed in double quotes and support escape sequences.

```
"hello world"
"line one\nline two"
"tab\there"
"a backslash: \\"
"a quote: \""
```

Supported escapes: `\n`, `\t`, `\\`, `\"`.

### Raw strings

Raw strings use `{| ... |}` delimiters. No escape processing is performed.
Multiline raw strings are automatically dedented: leading and trailing blank
lines are stripped, and common leading whitespace is removed.

```
{|hello|}                       -- "hello"
{|no \n escapes|}               -- "no \\n escapes"
{|she said "hi"|}               -- she said "hi"

{|
    hello
    world
|}
-- produces "hello\nworld" (dedented)

{|
    line one
      indented
|}
-- produces "line one\n  indented" (relative indent preserved)
```

### Interpolated strings

Prefixed with `$`, interpolated strings embed expressions inside `{...}`.
Each interpolated expression is converted to a string via `show`.

```
$"hello {name}"
$"1 + 2 = {1 + 2}"
$"{42} and {true}"

-- Escaped braces
$"a \{ b \} c"              -- "a { b } c"
```

### Bytes

Byte literals are written as `#` followed by two hexadecimal digits, representing
a value from 0 to 255.

```
#41         -- byte 0x41 (ASCII 'A', decimal 65)
#00         -- null byte
#ff         -- byte 255
```

### Runes

Rune literals represent Unicode code points, delimited by single quotes.

```
'a'         -- code point 97
'A'         -- code point 65
```

Escape sequences in rune literals:

```
'\n'        -- newline (code point 10)
'\t'        -- tab (code point 9)
'\0'        -- null (code point 0)
'\\'        -- backslash
'\''        -- single quote
```

Runes support full UTF-8:

```
'?'         -- Unicode lambda (multi-byte)
```

### Unit

The unit value and its type:

```
()
```

---

## Let Bindings

### Basic let

`let ... in` binds a value in a local scope.

```
let x = 5 in x + 1
```

### Top-level let

Without `in`, a `let` at the top level declares a global binding. Top-level
declarations are separated by `;;`.

```
let x = 42
;;
let y = x + 1
```

### Let with parameters

Functions can be defined with `let` by listing parameters before `=`.

```
let add x y = x + y in add 3 4

let double (x: int) : int = x * 2
```

### Let rec

Recursive bindings use `let rec`.

```
let rec fact n =
  if n <= 1 do 1 else n * fact (n - 1)
in fact 10
```

### Polymorphic recursion (`let rec (type 'a)`)

When a recursive function needs to call itself at different type instantiations
(polymorphic recursion), use `(type 'a ...)` after `rec` to declare locally
abstract type parameters. This is required when working with GADTs where the
recursive call changes the type parameter.

```
let rec (type 'a) eval (e : 'a expr) : 'a =
  match e with
  | IntLit n -> n
  | BoolLit b -> b
  | Add (a, b) -> eval a + eval b
  | If (cond, t, f) -> if eval cond do eval t else eval f
```

Without `(type 'a)`, the recursive binding for `eval` would be monomorphic —
the recursive call `eval cond` (at `bool expr`) would conflict with
`eval a` (at `int expr`). The `(type 'a)` annotation tells the compiler to
generalize the recursive binding, allowing each call to instantiate `'a`
independently.

Multiple type parameters are supported:

```
let rec (type 'a 'b) process (x : ('a, 'b) fmt) : string = ...
```

This works at both expression level (with `in`) and at the top level.

### Mutual recursion (`let rec ... and ...`)

Mutually recursive functions are defined with `let rec ... and ...`. All
functions in the group can reference each other.

```
let rec is_even n =
  if n = 0 do true else is_odd (n - 1)
and is_odd n =
  if n = 0 do false else is_even (n - 1)
in is_even 10
```

This works at both expression level (with `in`) and at the top level:

```
let rec is_even n =
  if n = 0 do true else is_odd (n - 1)
and is_odd n =
  if n = 0 do false else is_even (n - 1)
```

### Let mut

Mutable variables use `let mut`. See [Mutable State](#mutable-state).

```
let mut x = 0 in
x := 5;
x
```

### Destructuring let

Patterns can appear on the left side of `let`:

```
let (a, b) = (1, 2) in a + b
let x :: rest = [1; 2; 3] in x
let {x; y} = {x = 10; y = 20} in x + y
let (_, second) = (1, 2) in second
```

Top-level destructuring is also supported:

```
let (x, y) = (10, 20)
;;
x + y
```

---

## Functions

### Lambda expressions

Anonymous functions are written with `fn`.

```
fn x -> x + 1
fn x y -> x + y
fn (x: int) -> x * 2
fn (x: int) (y: int) -> x + y
```

### Unit parameter

```
fn () -> 42
fn -> 42                        -- shorthand for fn () -> 42
```

### Pattern destructuring in parameters

Function parameters can be patterns:

```
fn (x, y) -> x + y
fn {x; y} -> x + y
fn ((a, b), c) -> a + b + c
```

With type annotations on destructured parameters:

```
fn ((a, b) : int * string) -> a
fn ({x; _} : point) -> x
```

### Multi-clause functions (lambda match)

The `fn | ...` syntax creates a function that pattern-matches on its argument:

```
let f = fn
  | Some x -> x
  | None -> 0

let head = fn
  | [] -> 0
  | x :: _ -> x
```

Guards and or-patterns work in lambda match:

```
fn
| x when x > 0 -> x
| _ -> 0

fn
| Some 1 | Some 2 -> 99
| Some x -> x
| None -> 0
```

### Named functions with let

```
let add x y = x + y
let double (x: int) : int = x * 2
let rec fact n = if n <= 1 do 1 else n * fact (n - 1)
```

### Where constraints

Type class constraints can be attached to function declarations:

```
let show_twice (x: 'a) : string where Show 'a =
  show x ^ show x

let show_eq (a: 'a) (b: 'a) : string where Show 'a, Eq 'a =
  if a = b do show a else show a ^ " <> " ^ show b
```

---

## If/Else

Conditional expressions. When both branches are present, they must have the
same type. The `do` keyword after the condition opens the then-branch;
semicolons can sequence multiple expressions without needing a `do ... end`
block.

```
if true do 1 else 2

if x > 0 do "positive"
else if x = 0 do "zero"
else "negative"

if condition do
  x := 1;
  y := 2;
  x + y
else 0
```

### If without else

Use `end` to close an `if` that has no `else` branch. The expression returns
`unit`.

```
if debug do
  print "trace";
  print msg
end
```

Note: `end` and `else` are mutually exclusive — `end` closes the `if`
expression, so `end else` is a parse error.

---

## Match Expressions

### Basic match

```
match expr with
| pattern1 -> expr1
| pattern2 -> expr2
```

The leading `|` before the first arm is optional.

```
match color with
| Red -> 0
| Green -> 1
| Blue -> 2
```

### Guards

Arms can have `when` guards:

```
match n with
| x when x > 0 -> "positive"
| x when x = 0 -> "zero"
| _ -> "negative"
```

### Partial annotation

The `-- @partial` annotation suppresses exhaustiveness checking:

```
-- @partial
match opt with
| Some x -> x
```

Without this annotation, non-exhaustive matches produce a type error listing
the missing cases.

---

## Patterns

### Wildcard

Matches anything, binds nothing.

```
_
```

### Variable

Matches anything, binds the value to the name.

```
x
name
```

### Literal patterns

```
42          -- integer
3.14        -- float
true        -- boolean
"hello"     -- string
```

### Unit pattern

```
()
```

### Tuple patterns

```
(a, b)
(a, b, c)
(_, second)
((a, b), c)
```

### List patterns

```
[]                  -- empty list
[x]                 -- single element
[a; b; c]           -- exact elements (desugars to cons chain)
x :: xs             -- head and tail (cons)
x :: y :: rest      -- first two and rest
```

### Array patterns

```
#[]                 -- empty array
#[x]                -- single element
#[a; b; c]          -- exact elements (length must match)
```

### Constructor patterns

```
None                -- nullary constructor
Some x              -- constructor with argument
Some (a, b)         -- constructor with tuple argument
Node (left, right)  -- constructor with payload
Module.Constructor  -- qualified constructor
```

GADT constructors are matched with the same pattern syntax as regular
constructors -- no special syntax is needed for pattern matching.

```
match e with
| IntLit n -> n
| BoolLit b -> if b do 1 else 0
| Add (a, b) -> eval a + eval b
```

### Record patterns

```
{x = a; y = b}     -- field binding
{x; y}              -- punning (binds x to field x, y to field y)
{x; _}              -- partial match with wildcard
{x = 1; y}          -- mix of literal and punning
```

### Map patterns

```
#{"key": value}
#{"a": a; "b": b}
#{1: v}
```

### Or-patterns

Multiple patterns sharing the same body, separated by `|`:

```
| 1 | 2 -> "small"
| Red | Green -> "warm"
| [] | [_] -> "short"
| (1, x) | (2, x) -> x
```

### As-patterns

Bind the entire matched value to a name while also destructuring:

```
| (a, b) as pair -> ...
| x :: _ as whole_list -> ...
| Some n as opt -> ...
```

### Cons patterns

```
x :: xs             -- head :: tail
a :: b :: rest      -- right-associative
```

### Pin patterns

Pin patterns match against the current value of an existing variable, rather
than binding a new variable. Written with a caret prefix `^`:

```
let x = 42 in
match 42 with
| ^x -> "matched"       -- matches because the value equals x
| _ -> "nope"

let tag = "ok" in
match ("ok", 42) with
| (^tag, n) -> n         -- pin in a tuple
| _ -> 0

let x = 1 in
match [1; 2; 3] with
| ^x :: _ -> "yes"       -- pin as list head
| _ -> "no"
```

Pin patterns are useful for matching against a value computed before the
`match`, without needing a `when` guard.

### Polymorphic variant patterns

```
`Foo                -- nullary polymorphic variant
`Bar n              -- polymorphic variant with payload
```

See [Polymorphic Variants](#polymorphic-variants) for full details.

---

## Operators

### Arithmetic

| Operator | Description    | Example    |
|----------|----------------|------------|
| `+`      | Addition       | `2 + 3`    |
| `-`      | Subtraction    | `10 - 4`   |
| `*`      | Multiplication | `3 * 7`    |
| `/`      | Division       | `15 / 4`   |
| `mod`    | Modulo         | `17 mod 5` |

Arithmetic operators are overloaded via the `Num` type class and work with
both `int` and `float` (and custom types with a `Num` instance).

```
2 + 3           -- 5
2.0 + 3.0       -- 5.0
```

### Unary operators

```
-(3)
-(3.5)
not flag
lnot bits
```

Unary operators (`-`, `not`, `lnot`) bind looser than field access but tighter
than function application. This means `not r.field` is `not (r.field)`, matching
OCaml behavior.

### Comparison

| Operator | Description      |
|----------|------------------|
| `=`      | Equal            |
| `<>`     | Not equal        |
| `<`      | Less than        |
| `>`      | Greater than     |
| `<=`     | Less or equal    |
| `>=`     | Greater or equal |

Comparison operators are overloaded via `Eq` and `Ord` type classes.

```
3 = 3           -- true
3 <> 4          -- true
"abc" < "def"   -- true
```

### Logical

| Operator | Description | Example             |
|----------|-------------|---------------------|
| `&&`     | Logical AND | `true && false`     |
| `\|\|`   | Logical OR  | `false \|\| true`   |
| `not`    | Logical NOT | `not true`          |

`&&` and `||` are short-circuiting.

### String concatenation

```
"hello" ^ " " ^ "world"
```

### Cons (list prepend)

```
1 :: [2; 3]         -- [1; 2; 3]
1 :: 2 :: []        -- [1; 2]
```

### Pipe

```
x |> f              -- equivalent to f x
x |> f |> g         -- equivalent to g (f x)
```

See [Pipe Operator](#pipe-operator) for details.

### Assignment

```
x := 5              -- assign to mutable variable
record.field := v   -- assign to mutable record field
```

### Bitwise operators

| Operator | Description          | Example      |
|----------|----------------------|--------------|
| `land`   | Bitwise AND          | `7 land 3`   |
| `lor`    | Bitwise OR           | `5 lor 3`    |
| `lxor`   | Bitwise XOR          | `5 lxor 3`   |
| `lnot`   | Bitwise NOT (unary)  | `lnot 0`     |
| `lsl`    | Logical shift left   | `1 lsl 3`    |
| `lsr`    | Logical shift right  | `8 lsr 2`    |

### Operators as values

Wrap any binary operator in parentheses to use it as a first-class function:

```
(+)                 -- fn x y -> x + y
(*)                 -- fn x y -> x * y
(^)                 -- fn x y -> x ^ y
(=)                 -- fn x y -> x = y
(<)                 -- fn x y -> x < y
(mod)               -- fn x y -> x mod y
(&&)                -- fn x y -> x && y
(||)                -- fn x y -> x || y
(not)               -- fn x -> not x
(land)              -- fn x y -> x land y
(lor)               -- fn x y -> x lor y
(lxor)              -- fn x y -> x lxor y
(lnot)              -- fn x -> lnot x
(lsl)               -- fn x y -> x lsl y
(lsr)               -- fn x y -> x lsr y
```

Examples:

```
let f = (+) in f 3 4            -- 7
fold (+) 0 [1; 2; 3; 4]        -- 10
List.map (fn x -> x + 1) xs    -- or equivalently with a lambda
```

### Operator precedence (highest to lowest)

| Precedence | Operators                    | Associativity |
|------------|------------------------------|---------------|
| 12-13      | `*`, `/`, `mod`, `land`, `lsl`, `lsr` | Left |
| 10-11      | `+`, `-`, `^`, `lor`, `lxor` | Left          |
| 9-8        | `::`                         | Right         |
| 6-7        | `=`, `<>`, `<`, `>`, `<=`, `>=` | Left       |
| 4-5        | `&&`                         | Right         |
| 2-3        | `\|\|`                       | Right         |
| 1-2        | `\|>`                        | Left          |

---

## Records

### Creation

```
{x = 1; y = 2}
{name = "Alice"; age = 30}
```

Trailing semicolons are allowed:

```
{x = 1; y = 2;}
```

### Field access

```
r.x
r.name
point.x + point.y
```

### Field accessors as functions

A dot followed by a field name (with a space before it) creates a first-class
accessor function. This works because of structural subtyping — the function
accepts any record with that field.

```
.name                       -- fn r -> r.name
.age                        -- fn r -> r.age
```

Field accessors are useful with higher-order functions:

```
type person = {name: string; age: int}

List.map .name people       -- extract all names
List.map .age people        -- extract all ages
```

Note: `r.x` (no space before the dot) is postfix field access, while `.field`
at the start of an expression or after a space is a field accessor function.

### Record update

Create a new record based on an existing one with some fields changed:

```
let r2 = {r with x = 10}
let r3 = {r with x = 10; y = 20}
```

The base expression can be any expression:

```
{make_record () with x = 10}
```

### Record types

```
type point = {x: int; y: int}
type person = {name: string; age: int}
```

### Open record types (row polymorphism)

Record type annotations can end with `..` to indicate an open record -- one
that may have additional fields beyond those listed. This makes row polymorphism
explicit in type annotations:

```
let get_x (r : {x: int; ..}) : int = r.x

get_x {x = 1; y = 2}           -- works: extra field y is fine
get_x {x = 10; y = 20; z = 30} -- works: extra fields y and z are fine
```

Without the `..`, a record type annotation is closed and matches only records
with exactly those fields. With `..`, the function accepts any record that has
at least the listed fields with compatible types.

### Mutable fields

Record fields can be declared mutable:

```
type counter = {mut count: int; label: string}

let c = {count = 0; label = "clicks"}
c.count := c.count + 1
```

Only fields declared `mut` in the type definition can be assigned to.

---

## Tuples

### Creation

```
(1, 2)
(1, "hello", true)
("a", 42)
```

### Destructuring

```
let (a, b) = (1, 2) in a + b
let (x, _, z) = (1, 2, 3) in x + z
```

### In function parameters

```
let add (a, b) = a + b
fn (x, y) -> x + y
```

### Built-in accessors

```
fst (1, 2)          -- 1
snd (1, 2)          -- 2
```

---

## Lists

### Creation

```
[]                  -- empty list
[1; 2; 3]           -- list with elements
[1]                 -- singleton
```

### Cons operator

```
0 :: [1; 2]         -- [0; 1; 2]
1 :: 2 :: 3 :: []   -- [1; 2; 3]
```

### Pattern matching

```
match xs with
| [] -> "empty"
| [x] -> "one element"
| x :: rest -> "head is " ^ show x
```

### Type annotations

```
([] : int list)
let xs : string list = ["a"; "b"]
```

### Standard library operations

With the standard library loaded:

```
List.length [1; 2; 3]                          -- 3
List.rev [1; 2; 3]                              -- [3; 2; 1]
List.map (fn x -> x + 1) [1; 2; 3]             -- [2; 3; 4]
List.filter (fn x -> x > 2) [1; 2; 3; 4]       -- [3; 4]
List.fold (fn acc x -> acc + x) 0 [1; 2; 3]    -- 6
List.concat [1; 2] [3; 4]                       -- [1; 2; 3; 4]
List.hd [10; 20]                                -- 10
List.tl [10; 20]                                -- [20]
List.nth [10; 20; 30] 1                         -- 20
List.find (fn x -> x > 2) [1; 2; 3]            -- Some 3
List.exists (fn x -> x = 3) [1; 2; 3]          -- true
List.forall (fn x -> x > 0) [1; 2; 3]          -- true
List.sort cmp_fn xs
List.flatten [[1; 2]; [3; 4]]                  -- [1; 2; 3; 4]
List.mapi (fn i x -> (i, x)) ["a"; "b"]
List.zip [1; 2] ["a"; "b"]                     -- [(1, "a"); (2, "b")]
List.is_empty []                                -- true
```

---

## Arrays

Arrays are fixed-size, mutable, and 0-indexed.

### Creation

```
#[]                 -- empty array
#[1; 2; 3]          -- array with elements
Array.make 5 0      -- array of 5 zeros
```

### Indexing

```
#[10; 20; 30].[1]   -- 20 (dot-bracket indexing)
Array.get arr 1      -- equivalent to arr.[1]
```

### Mutation

```
Array.set arr 0 99   -- set element at index 0

-- strings also support dot-bracket indexing (returns a byte):
"hello".[0]          -- byte #68
```

### Pattern matching

```
match arr with
| #[] -> "empty"
| #[x] -> "one"
| #[a; b; c] -> "three"
| _ -> "other"
```

### Type annotations

```
let f (a : int array) = Array.get a 0
```

### Standard library operations

```
Array.length #[1; 2; 3]        -- 3
Array.get arr i                 -- get element at index i
Array.set arr i v               -- set element at index i to v
Array.make n default            -- create array of size n
Array.copy arr                  -- shallow copy
Array.sub arr start len         -- sub-array
Array.to_list #[1; 2; 3]       -- [1; 2; 3]
Array.of_list [1; 2; 3]        -- #[1; 2; 3]
```

---

## Maps

Maps are immutable associative data structures with key-value pairs.

### Creation

```
#{}                             -- empty map
#{"a": 1; "b": 2}              -- string keys, int values
#{42: "answer"}                 -- int keys, string values
#{x: x + 1; x + 1: x + 2}     -- expression keys and values
```

### Operations

```
get "a" m                       -- returns Some v or None
set "b" 2 m                     -- returns new map with key added/updated
has "a" m                       -- true or false
remove "a" m                    -- returns new map without key
size m                          -- number of entries
keys m                          -- list of keys
values m                        -- list of values
to_list m                       -- list of (key, value) tuples
of_list [("a", 1); ("b", 2)]   -- create map from association list
```

### Map update

Create a new map based on an existing one with some keys added or changed,
using `#{ base with key: value }` syntax:

```
let m = #{"a": 1; "b": 2}
let m2 = #{m with "a": 10}            -- "a" updated to 10, "b" unchanged
let m3 = #{m with "c": 3}             -- adds new key "c"
let m4 = #{m with "a": 10; "b": 20}   -- update multiple keys
```

Map update desugars to nested `set` calls:
`#{m with "a": 10; "b": 20}` is equivalent to `set "b" 20 (set "a" 10 m)`.

### Pattern matching

```
match m with
| #{"x": x} -> x
| #{"a": a; "b": b} -> a + b
| _ -> 0
```

### Iteration

```
fold (fn acc p -> let (k, v) = p in acc + v) 0 my_map

for p in my_map do
  let (k, v) = p in
  print k
end
```

---

## Sets

Sets are built on top of maps (as `('a, unit) map`).

### Creation

```
Set.empty ()                    -- empty set
Set.singleton 42                -- {42}
Set.of_list [1; 2; 3]          -- set from list
#{1; 2; 3}                      -- set literal syntax
```

The `#{...}` syntax disambiguates: if elements are separated by `;` without
`:`, it is a set. If elements use `key: value` pairs, it is a map. An empty
`#{}` is a map.

### Typed collection literals

You can prefix `#` with a module name to create typed collections:

```
#Set{1; 2; 3}                   -- desugars to Set.of_list [1; 2; 3]
#Set{}                          -- desugars to Set.of_list []
#Array[1; 2; 3]                 -- desugars to Array.of_list [1; 2; 3]
#Array[]                        -- desugars to Array.of_list []
```

This works with any module that provides an `of_list` function. The `{...}`
form is for set-like collections and `[...]` is for list/array-like ones.

### Operations

```
Set.add 4 s                     -- returns new set with element added
Set.remove 2 s                  -- returns new set without element
Set.mem 2 s                     -- membership test
Set.size s                      -- number of elements
Set.to_list s                   -- list of elements
Set.union s1 s2                 -- set union
Set.inter s1 s2                 -- set intersection
Set.diff s1 s2                  -- set difference
Set.is_empty s                  -- emptiness test
Set.is_subset s1 s2             -- subset test
```

### Iteration

```
for x in Set.of_list [10; 20; 30] with acc = 0 do
  acc + x
end
```

---

## Polymorphic Variants

Polymorphic variants are structural variants -- they do not require a `type`
declaration. A polymorphic variant tag is written with a backtick prefix.

### Creation

```
`Foo                        -- nullary polymorphic variant
`Bar 42                     -- with an int payload
`Name "hello"               -- with a string payload
`Pair (1, 2)                -- with a tuple payload
```

### Pattern matching

```
match `B 42 with
| `A -> 0
| `B n -> n + 1             -- 43

let f x = if x do `True else `False
f true                      -- `True
```

### Type annotations

Polymorphic variant types are written with square brackets and backtick-prefixed
tags, separated by `|`:

```
[`Foo | `Bar]                        -- exact: only `Foo or `Bar
[`Foo of int | `Bar of string]       -- with payloads
[> `Foo | `Bar]                      -- lower bound (at least these tags)
[< `Foo | `Bar]                      -- upper bound (at most these tags)
```

Exact types match only the listed tags. Lower-bound types (`[> ...]`) accept
the listed tags and possibly more -- this is how polymorphic variant subtyping
works. Upper-bound types (`[< ...]`) guarantee no tags beyond those listed.

```
let f (x : [> `A | `B of int]) = match x with
  | `A -> 0
  | `B n -> n
  | _ -> -1

f (`B 7)                    -- 7
f `A                        -- 0
```

### Coercion

Use `:>` inside parentheses to widen a polymorphic variant to a larger type:

```
let x = `A in
(x :> [> `A | `B])          -- coerce to a wider type
```

### When to use polymorphic variants

Polymorphic variants are useful when you want lightweight, ad-hoc alternatives
without defining a named type. They compose well across module boundaries since
they are structural -- any two functions using `` `Ok `` and `` `Error `` are
automatically compatible, with no shared type definition needed.

For large, well-defined data types, named variants (`type color = Red | Green | Blue`)
are still preferred -- they give you exhaustiveness checking, clearer error
messages, and `deriving` support.

---

## Type Declarations

### Variant types

```
type color = Red | Green | Blue

type shape =
  | Circle of int
  | Rect of int * int
```

### Record types

```
type point = {x: int; y: int}

type counter = {mut count: int; label: string}
```

### Type aliases

```
type name = string
type point = int * int
```

### Parameterized types

```
type 'a option = None | Some of 'a
type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree)
type ('a, 'b) either = Left of 'a | Right of 'b
type 'a pair = 'a * 'a
type ('a, 'b) assoc = ('a * 'b) list
```

### GADTs (Generalized Algebraic Data Types)

GADT constructors use `:` followed by a full type signature instead of `of`.
The last type in the arrow chain is the return type, which specifies what type
parameters the variant has for that constructor. Everything before the last
arrow is the argument type (multiple arguments become a tuple).

```
type 'a type_name =
  | Ctor : arg_type -> return_type
  | Ctor : return_type    -- no-arg GADT constructor
```

A concrete example:

```
type 'a expr =
  | IntLit : int -> int expr
  | BoolLit : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
```

Here, `IntLit` can only produce an `int expr`, and `BoolLit` can only produce
a `bool expr`. The type checker uses these refined return types to narrow the
type parameter when pattern matching on a GADT constructor.

Regular variant constructors and GADT constructors cannot be mixed in the same
type declaration. If any constructor uses the `:` form, all constructors in
that type must use it.

### Mutual type recursion (`type ... and ...`)

Mutually recursive types are defined with `type ... and ...`. All types in the
group can reference each other.

```
type 'a tree = Leaf | Node of 'a * 'a forest
and 'a forest = Empty | Trees of 'a tree list
```

### Deriving

Automatically derive type class instances:

```
type color = Red | Green | Blue deriving Show
type point = {x: int; y: int} deriving Show, Eq
type 'a box = Wrap of 'a deriving Show

show Red                -- "Red"
show {x = 1; y = 2}    -- "{ x = 1; y = 2 }"
```

Supported derivable classes: `Show`, `Eq`.

Note: `deriving` is not supported for GADT types. Type class instances for
GADTs must be written manually.

---

## Type Classes

### Defining a class

```
class Show 'a =
  show : 'a -> string
end
```

Multi-method classes:

```
class Describable 'a =
  describe : 'a -> string
  label : 'a -> string
end
```

Multi-parameter classes:

```
class Convert 'a 'b =
  convert : 'a -> 'b
end

class Container 'c 'e =
  size : 'c -> int
  first : 'c -> 'e
end
```

### Functional dependencies

Multi-parameter type classes can declare functional dependencies using `where`
after the type parameters. A functional dependency `'a -> 'b` states that the
type `'a` uniquely determines `'b`, allowing the compiler to infer `'b`
automatically once `'a` is known.

```
class Container 'c 'e where 'c -> 'e =
  elements : 'c -> 'e list
end
```

This says: given the container type `'c`, the element type `'e` is uniquely
determined. So `Container (int list) int` is the only valid instance for
`int list` -- the compiler can infer `'e = int` from `'c = int list` alone.

Multiple dependencies are separated by commas:

```
class Convert 'a 'b 'c where 'a 'b -> 'c =
  convert : 'a -> 'b -> 'c
end
```

The built-in `Iter`, `Map`, and `Index` classes all use functional dependencies
to guide type inference. For example, `Index` has `'c -> 'k 'v`, meaning the
container type determines both the key and value types.

### Effect-polymorphic class methods

Class method signatures can include effect variables, allowing instances to
perform different effects:

```
class Apply 'f =
  do_thing : 'a -> 'a / 'e
end
```

The effect variable `'e` is polymorphic, so each instance can specify its own
effect (or none at all).

### Defining an instance

```
instance Show int =
  let show x = string_of_int x
end

instance Describable int =
  let describe x = string_of_int x
  let label _ = "integer"
end

instance Convert int string =
  let convert (x: int) = string_of_int x
end
```

### Operator instances

Instances for operator type classes define operators in parenthesized form:

```
instance Num vec2 =
  let (+) a b = {x = a.x + b.x; y = a.y + b.y}
  let (-) a b = {x = a.x - b.x; y = a.y - b.y}
  let (*) a b = {x = a.x * b.x; y = a.y * b.y}
  let (/) a b = {x = a.x / b.x; y = a.y / b.y}
  let neg a = {x = 0 - a.x; y = 0 - a.y}
end

instance Eq point =
  let (=) a b = a.px = b.px
  let (<>) a b = a.px <> b.px
end
```

### Constrained instances

Instance definitions can have type class constraints:

```
instance Show ('a box) where Show 'a =
  let show b = match b with
    | Box x -> "Box(" ^ show x ^ ")"
end
```

### Built-in type classes

| Class     | Methods                              | Description              |
|-----------|--------------------------------------|--------------------------|
| `Show`    | `show : 'a -> string`                | String representation    |
| `Eq`      | `(=)`, `(<>)`                        | Equality comparison      |
| `Ord`     | `(<)`, `(>)`, `(<=)`, `(>=)`         | Ordering comparison      |
| `Num`     | `(+)`, `(-)`, `(*)`, `(/)`, `neg`    | Arithmetic               |
| `Bitwise` | `land`, `lor`, `lxor`, `lsl`, `lsr`, `lnot` | Bitwise operations |
| `Iter`    | `fold`                               | Foldable collections     |
| `Map`     | `get`, `set`, `has`, `remove`, `size`, `keys`, `values`, `to_list`, `of_list` | Key-value collections |
| `Hash`    | `hash : 'a -> int`                   | Hash computation         |

### Using class methods

Class methods are called like regular functions. The instance is selected
based on the types of the arguments.

```
show 42                 -- "42"
show true               -- "true"
show [1; 2; 3]          -- "[1; 2; 3]"
fold (+) 0 [1; 2; 3]   -- 6
```

---

## Modules

### Definition

```
module M =
  pub let x = 42
  let secret = 99       -- private (default)
  pub let double n = n * 2
end
```

### Visibility

- `pub` -- publicly accessible outside the module
- (default) -- private, only visible within the module
- `opaque` -- for types: the type name is visible, but constructors are hidden

```
module M =
  pub let public_val = 1
  let private_val = 2
  opaque type token = Wrap of int
  pub let make x = Wrap x
  pub let unwrap t = match t with | Wrap n -> n
end
```

### Qualified access

```
M.x
M.double 5
Math.sqrt 16.0
Outer.Inner.f 5
```

### Open

Import all public bindings from a module into the current scope:

```
open M
```

Selective open imports only named bindings:

```
open M (x, double)
```

### Local open

Use module bindings within a parenthesized expression:

```
M.(x + y)
```

### Nested modules

```
module Outer =
  let secret = 100
  pub module Inner =
    pub let f x = x + secret
  end
end

Outer.Inner.f 5         -- 105
```

### Types in modules

Modules can contain type declarations, class definitions, and instances:

```
module M =
  pub type color = Red | Green | Blue
  pub let to_int c = match c with
    | Red -> 0 | Green -> 1 | Blue -> 2

  pub class Pretty 'a =
    pretty : 'a -> string
  end
  instance Pretty int =
    let pretty x = "<" ^ string_of_int x ^ ">"
  end
end

M.to_int M.Green        -- 1
M.pretty 42             -- "<42>"
```

### Qualified instance declarations

Instances for classes defined in other modules:

```
instance PP.Pretty string =
  let pretty s = "(" ^ s ^ ")"
end
```

---

## Algebraic Effects

### Declaring effects

```
effect Ask =
  ask : unit -> string
end

effect State =
  get : unit -> int
  put : int -> unit
end
```

### Parameterized effects

Effects can take type parameters, placed between the effect name and `=`.

```
effect State 'a =
  get : unit -> 'a
  put : 'a -> unit
end
```

This declares a family of effects parameterized by a type. For example,
`State int` and `State string` are distinct effects.

### Performing effects

```
perform ask ()
perform get_val 42
```

### Handling effects with handle/with

The `handle ... with` form provides full control over the continuation:

```
handle
  let x = perform get_val () in
  x + 1
with
| return x -> x
| get_val () k -> resume k 10
```

The `return` arm processes the final value. Effect operation arms receive the
argument and a continuation `k` which can be resumed with `resume k value`.

### Handler without resume (aborting)

If the continuation is not resumed, the handler acts like an exception:

```
handle
  perform abort 42;
  0
with
| return x -> x
| abort v k -> v
```

### Copying continuations

Continuations are one-shot by default. Use `copy_continuation` to create a reusable copy
for multi-shot semantics:

```
handle
  let x = perform choose () in
  if x do 1 else 0
with
| return x -> [x]
| choose () k ->
  let k2 = copy_continuation k in
  let a = resume k true in
  let b = resume k2 false in
  append a b
```

### Try/with (simplified exception-like form)

`try/with` is a simplified form that automatically passes through the return
value and does not expose the continuation:

```
try
  perform raise "oops"
with
| raise msg -> msg

try 42 with
| raise msg -> 0
```

Multiple operations can be handled:

```
try
  perform invalid_input "bad data"
with
| file_not_found path -> "missing: " ^ path
| invalid_input msg -> "invalid: " ^ msg
```

---

## For Loops

The `for` keyword provides a unified looping construct with several forms.

### Infinite loop

`for do ... end` loops forever (until a `break` or `return` is encountered).

```
let mut x = 0 in
for do
  x := x + 1;
  if x = 5 do break end
end;
x    -- 5
```

### Conditional loop

`for cond do ... end` loops while the condition is true. Replaces the old
`while` syntax. Returns `unit`.

```
let mut x = 0 in
for x < 5 do
  x := x + 1
end;
x    -- 5
```

### While-let loop

`for let pat = expr do ... end` loops as long as the pattern matches. This is
useful for consuming options, iterators, or any expression that may fail to
match.

```
let mut items = [1; 2; 3] in
for let x :: rest = items do
  print x;
  items := rest
end
```

### Foreach loop

`for x in coll do ... end` iterates over any collection that implements the
`Iter` type class. Returns `unit`.

```
for x in [1; 2; 3] do
  print x
end
```

### Pattern destructuring in for loops

The iteration variable in `for`/`in` loops can be a destructuring pattern.
Tuples, records, lists, and other patterns are supported:

```
let pairs = [(1, 10); (2, 20); (3, 30)]

-- Tuple destructuring
for (k, v) in pairs do
  print $"{k}: {v}"
end

-- Record destructuring
for {name; age} in people do
  print name
end
```

Pattern destructuring also works with fold loops:

```
for (k, v) in [(1, 10); (2, 20); (3, 30)] with sum = 0 do
  sum + k + v
end
-- evaluates to 66
```

The pattern is desugared to a match expression inside the loop body, so any
pattern that works in `match` works here.

### Fold loop

`for x in coll with acc = init do ... end` accumulates a value across
iterations. Returns the final accumulator value.

```
for x in [1; 2; 3] with acc = 0 do
  acc + x
end
-- evaluates to 6
```

```
for x in [1; 2; 3] with s = "" do
  s ^ string_of_int x
end
-- evaluates to "123"
```

The accumulator can be a tuple for tracking multiple values:

```
let result = for x in [1; 2; 3; 4; 5] with (sum, count) = (0, 0) do
  (sum + x, count + 1)
end
-- result is (15, 5)
```

### Break

`break` can be used in all loop forms to exit early.

In while and foreach loops, `break` returns `unit`. In fold loops, `break`
alone returns the current accumulator value. `break expr` can be used to return
a specific value from a fold loop.

```
for x in [1; 2; 3; 4; 5] do
  if x = 4 do break
  else print x
end

for x in [1; 2; 3; 4; 5] with acc = 0 do
  if x = 4 do break
  else acc + x
end
-- evaluates to 6

for x in [1; 2; 3; 4; 5] with acc = 0 do
  if x = 4 do break 99
  else acc + x
end
-- evaluates to 99
```

### Continue

`continue` skips the rest of the current iteration and proceeds to the next
one. Works in all loop forms.

```
for x in [1; 2; 3; 4; 5] do
  if x = 3 do continue end;
  print x
end
-- prints 1, 2, 4, 5
```

### Return

`return expr` performs an early return from the enclosing function. It uses the
VM's native control stack and works from within any loop form or nested closure.

```
let find_first lst =
  for x in lst do
    if x > 3 do return x end
  end;
  0

find_first [1; 2; 5; 3]    -- 5
```

### Custom iterables

Any type with an `Iter` instance can be used in for loops:

```
type tree = Leaf | Node of (int * tree * tree)

instance Iter tree int =
  let fold f acc t =
    let rec go a tr = match tr with
      | Leaf -> a
      | Node payload ->
        let (v, left, right) = payload in
        let a = go a left in
        let a = f a v in
        go a right
    in go acc t
end

for x in my_tree with sum = 0 do sum + x end
```

Arrays, lists, maps, and sets all support for loops out of the box.

---

## Sequences

Sequences (`'a seq`) are lazy, potentially infinite streams. They are defined
as `('a -> unit) -> unit` -- a function that calls a yield callback for each
element.

### Creation

```
Seq.range 1 5                   -- 1, 2, 3, 4
Seq.of_list [1; 2; 3]           -- from a list
Seq.repeat 42                   -- infinite stream of 42s
Seq.iterate 1 (fn x -> x * 2)  -- 1, 2, 4, 8, 16, ...
```

### Transformations

```
Seq.map (fn x -> x * 2) s
Seq.filter (fn x -> x > 3) s
Seq.take 5 s
Seq.take_while (fn x -> x < 10) s
Seq.drop 3 s
Seq.drop_while (fn x -> x < 3) s
Seq.flat_map (fn x -> Seq.range x (x + 3)) s
Seq.enumerate s                 -- pairs each element with its index
Seq.chunk 2 s                   -- groups elements into lists of size n
```

### Consumption

```
Seq.to_list s
Seq.fold (fn acc x -> acc + x) 0 s
Seq.each (fn x -> print x) s
Seq.count s
Seq.sum s
Seq.find (fn x -> x > 3) s     -- returns option
Seq.any (fn x -> x > 3) s      -- returns bool
Seq.all (fn x -> x < 10) s     -- returns bool
```

### Pipelines

Sequences compose naturally with the pipe operator:

```
Seq.iterate 1 (fn x -> x + 1)
  |> Seq.filter (fn x -> x > 3)
  |> Seq.map (fn x -> x * 10)
  |> Seq.take 3
  |> Seq.to_list
-- [40; 50; 60]
```

---

## String Interpolation

Interpolated strings are prefixed with `$`. Expressions inside `{...}` are
evaluated and converted to strings via `show`.

```
$"hello {name}"
$"result: {1 + 2 + 3}"
$"{42} and {true}"
```

Nested expressions of any complexity are supported:

```
let name = "world" in $"hello {name}"

$"sum = {List.fold (fn a b -> a + b) 0 [1; 2; 3]}"
```

Escape braces with backslash:

```
$"literal \{ braces \}"        -- "literal { braces }"
```

Standard string escapes also work:

```
$"line one\nline two"
```

### Format Specifiers

Add a colon after the expression to control formatting: `$"{expr:spec}"`.

| Specifier | Example | Result | Applies to |
|-----------|---------|--------|------------|
| `.Nf` | `$"{3.14159:.2f}"` | `"3.14"` | `float` |
| `x` | `$"{255:x}"` | `"ff"` | `int` |
| `X` | `$"{255:X}"` | `"FF"` | `int` |
| `o` | `$"{8:o}"` | `"10"` | `int` |
| `b` | `$"{10:b}"` | `"1010"` | `int` |
| `>N` | `$"{42:>10}"` | `"        42"` | any (right-align) |
| `<N` | `$"{"hi":<10}"` | `"hi        "` | any (left-align) |
| `0N` | `$"{42:08x}"` | `"0000002a"` | combined with above |

Specifiers compose — alignment/zero-padding can be combined with type specifiers:

```
$"{3.14159:>10.2f}"       -- "      3.14"
$"{42:08x}"               -- "0000002a"
```

Type annotations inside parentheses are not affected (the colon is only
recognized at brace/paren depth 0):

```
$"{(x : int)}"            -- type annotation, not a format spec
```

Format specifiers are type-safe — using `:x` on a float or `:.2f` on an int
produces a compile-time type error.

---

## Pipe Operator

The pipe operator `|>` passes the left-hand value as the last argument to the
right-hand function. It has the lowest precedence among binary operators,
so arithmetic binds tighter.

```
5 |> double                     -- double 5
1 + 2 |> fn x -> x * 10        -- 30 (pipes 3 into the lambda)

3 |> add1 |> double             -- double (add1 3)

5 |> add 3                      -- add 3 5 (partial application)

"hello" |> exclaim              -- exclaim "hello"
```

---

## Semicolons and Sequencing

### Expression sequencing

Semicolons sequence expressions. The value of the sequence is the last
expression. Earlier expressions are evaluated for their side effects.

```
print "hello"; print "world"; 42
```

### Top-level declaration separator

Double semicolons `;;` separate top-level declarations.

```
let x = 1
;;
let y = 2
;;
x + y
```

Within a `let ... in` chain, `;;` is not needed:

```
let x = 1 in
let y = 2 in
x + y
```

---

## Do/End Blocks

`do ... end` groups a sequence of expressions into a single expression.
Useful where you need a block in expression position (for example, in a
top-level `let` binding that runs side effects).

```
do 42 end

let _init = do
  print "hello";
  print "world"
end

do
  let x = 1 in
  let y = 2 in
  x + y
end
```

Note: `if ... do` allows semicolon-separated sequences in the **then**
branch, but **not** in the `else` branch — a `;` after an `else` expression
ends the entire `if`-expression. Use `do ... end` inside `else` if you need
multiple expressions:

```
if cond do
  a; b; c          -- semicolons work in the then-branch
else do
  x; y; z          -- need do...end for semicolons in else
end
```

See [If/Else](#ifelse).

---

## Mutable State

### Mutable variables

```
let mut x = 0 in
x := 5;
x
```

Top-level mutable bindings:

```
let mut counter = 0
;;
counter := counter + 1
;;
counter
```

### Assignment

The `:=` operator assigns to a mutable variable or mutable record field.
Assignment returns unit.

```
x := x + 1

record.field := new_value
```

### Mutable record fields

Fields declared `mut` can be assigned to:

```
type point = {mut x: int; y: int}

let p = {x = 1; y = 2} in
p.x := 10;
p.x                             -- 10
```

Attempting to assign to an immutable field or an immutable variable produces a
type error.

---

## Type Annotations

### Expression annotations

```
(expr : type)
([] : int list)
(None : int option)
```

### Parameter annotations

```
fn (x: int) -> x + 1
let add (x: int) (y: int) : int = x + y
```

### Return type annotations

```
let f (x: int) : string = show x
```

### Effect annotations

Return types can include effect annotations using `/ Effect` after the type.
This declares which effects a function may perform.

```
let f (x: int) : string / IO = ...
let g (x: int) : int / pure = ...
let h (x: int) : int / State int, IO = ...
```

Use `/ pure` to explicitly mark a function as performing no effects. Multiple
effects are separated by commas.

Effect annotations also work on arrow types:

```
int -> string / IO
(int -> int / IO) -> string
```

When an arrow type has an effect annotation, the effect applies to the
function's return. Parentheses can be used to annotate inner arrow types, as
shown in the second example above.

### Type syntax

| Syntax                | Description                  |
|-----------------------|------------------------------|
| `int`                 | Integer type                 |
| `float`               | Float type                   |
| `bool`                | Boolean type                 |
| `string`              | String type                  |
| `unit`                | Unit type                    |
| `byte`                | Byte type (0-255)            |
| `rune`                | Rune type (Unicode codepoint)|
| `'a`                  | Type variable                |
| `int -> string`       | Function type                |
| `int -> string / IO`  | Function type with effect annotation |
| `int * string`        | Tuple type                   |
| `int list`            | List type                    |
| `int array`           | Array type                   |
| `(string, int) map`   | Map type                     |
| `int option`          | Parameterized type           |
| `(int, string) either`| Multi-parameter type         |
| `{x: int; y: string}` | Record type (closed)        |
| `{x: int; ..}`        | Record type (open / row-polymorphic) |
| `` [`A \| `B of int] ``  | Polymorphic variant type (exact)  |
| `` [> `A \| `B] ``    | Polymorphic variant type (lower bound) |
| `` [< `A \| `B] ``    | Polymorphic variant type (upper bound) |
| `Module.typename`     | Qualified type name          |
| `int Module.t`        | Parameterized qualified type |

---

## Extern Declarations

The `extern` keyword declares a function that is implemented by the runtime VM
rather than in MiniML source code. This gives the type checker knowledge of
built-in functions (like `print`, `failwith`, or `String.length`) so they can
be used in source programs with full type checking.

### Basic syntax

```
extern <name> : <type>
```

The name is a lowercase identifier and the type is a standard type annotation.

```
extern print : 'a -> unit
extern failwith : string -> 'a
extern string_of_int : int -> string
```

### Module-qualified names

Extern declarations can use a dotted module path to declare functions that
belong to a module namespace:

```
extern String.length : string -> int
extern Array.get : 'a array -> int -> 'a
```

### Polymorphic types

Type variables in extern declarations are generalized automatically, so
polymorphic externals work the same way as polymorphic `let` bindings:

```
extern print : 'a -> unit          -- polymorphic: works with any type
extern fst : 'a * 'b -> 'a        -- multiple type variables
```

### Inside modules

Extern declarations can appear inside module definitions. Use `pub` to make
them visible outside the module:

```
module IO =
  pub extern read_line : unit -> string
  pub extern write : string -> unit
end

IO.read_line ()
```

### How it works

An `extern` declaration does not generate any code. It tells the compiler that
a function with the given name and type exists at runtime, provided by the VM.
The actual implementation is registered by the host environment (OCaml or
JavaScript) before the program runs. If a program references an extern that has
no registered implementation, it will fail at runtime.

---

## Keywords

The complete list of reserved keywords:

```
let     rec     in      if      else    do
end     fn      match   with    type    of
not     mod     true    false   class   instance
effect  perform handle  try     resume  extern
mut     for     break   when    where   module
pub     open    opaque  deriving and    return
continue land   lor     lxor    lnot    lsl
lsr
```
