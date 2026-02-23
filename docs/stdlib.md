# MiniML Standard Library Reference

This document covers every module and function available in the MiniML standard
library. All functions listed here are loaded automatically by `register_all`.

---

## Built-in Functions

These are top-level functions available without any module prefix.

### Printing

```
print : 'a -> unit
```
Prints a value followed by a newline. Works on any type.

```
show : 'a -> string  where Show 'a
```
Converts a value to its string representation. Requires a `Show` instance
(see Type Classes below).

### String Concatenation

```
(^) : string -> string -> string
```
Concatenates two strings.

```miniml
"hello" ^ " " ^ "world"   (* "hello world" *)
```

### Boolean Operators

```
(&&) : bool -> bool -> bool
(||) : bool -> bool -> bool
not  : bool -> bool
```
Logical AND, OR, and NOT.

### Numeric

```
mod : int -> int -> int
```
Integer modulo. Raises a runtime error on division by zero.

```
neg : 'a -> 'a  where Num 'a
```
Numeric negation (available via the `Num` type class).

### Error Handling

```
failwith : string -> 'a
```
Raises a runtime error with the given message. The return type `'a` means it
can appear in any context (it never returns). This is a hard crash — it cannot
be caught with `try/with`.

```miniml
failwith "something went wrong"    (* runtime error *)
```

### Type Conversions

```
float_of_int    : int -> float
int_of_float    : float -> int
string_of_int   : int -> string
string_of_float : float -> string
string_of_bool  : bool -> string
```
Convert between primitive types.

```miniml
string_of_int 42         (* "42" *)
float_of_int 3           (* 3.0 *)
int_of_float 3.7         (* 3 *)
string_of_float 2.5      (* "2.5" *)
string_of_bool true      (* "true" *)
```

### Continuation Copy

```
copy_continuation : 'a -> 'a
```
Copies a continuation value (used with algebraic effects). Creates a fresh
copy so that a one-shot continuation can be resumed multiple times.

---

## Type Classes

Type classes provide ad-hoc polymorphism. Methods from a class can be used on
any type that has a registered instance.

### Num

Numeric arithmetic operators. Overloaded for `int` and `float`.

```
class Num 'a =
  (+)  : 'a -> 'a -> 'a
  (-)  : 'a -> 'a -> 'a
  (*)  : 'a -> 'a -> 'a
  (/)  : 'a -> 'a -> 'a
  neg  : 'a -> 'a
end
```

**Instances:** `int`, `float`

```miniml
3 + 4          (* 7 *)
2.0 * 3.5      (* 7.0 *)
neg 5          (* -5 *)
```

### Eq

Equality comparison.

```
class Eq 'a =
  (=)  : 'a -> 'a -> bool
  (<>) : 'a -> 'a -> bool
end
```

**Instances:** `int`, `float`, `string`, `bool`, `byte`, `rune`

User-defined types can derive `Eq` automatically:
```miniml
type color = Red | Green | Blue deriving Eq
Red = Red     (* true *)
Red <> Blue   (* true *)
```

### Ord

Ordering comparison.

```
class Ord 'a =
  (<)  : 'a -> 'a -> bool
  (>)  : 'a -> 'a -> bool
  (<=) : 'a -> 'a -> bool
  (>=) : 'a -> 'a -> bool
end
```

**Instances:** `int`, `float`, `string`, `byte`, `rune`

### Show

Convert a value to a human-readable string.

```
class Show 'a =
  show : 'a -> string
end
```

**Instances:** `int`, `float`, `bool`, `string`, `unit`, `byte`, `rune`,
`'a list` (where `Show 'a`), `'a array` (where `Show 'a`),
`'a option` (where `Show 'a`), `('a * 'b)` (where `Show 'a, Show 'b`),
`('a * 'b * 'c)` (where `Show 'a, Show 'b, Show 'c`),
`('k, 'v) map`

User-defined types can derive `Show`:
```miniml
type point = { x: int; y: int } deriving Show
show { x = 10; y = 20 }   (* "{ x = 10; y = 20 }" *)
```

### Bitwise

Bitwise operations on integers.

```
class Bitwise 'a =
  land : 'a -> 'a -> 'a
  lor  : 'a -> 'a -> 'a
  lxor : 'a -> 'a -> 'a
  lsl  : 'a -> 'a -> 'a
  lsr  : 'a -> 'a -> 'a
  lnot : 'a -> 'a
end
```

**Instances:** `int`

```miniml
5 land 3     (* 1 *)
5 lor 3      (* 7 *)
1 lsl 4      (* 16 *)
lnot 0       (* -1 *)
```

### Iter

Foldable collections. Any type with an `Iter` instance can be used with `for`
loops and the `fold` function.

```
class Iter 'a 'b =
  fold : ('c -> 'b -> 'c) -> 'c -> 'a -> 'c
end
```

Here `'a` is the collection type and `'b` is the element type.

**Instances:** `'a list`, `'a array`, `('k, 'v) map` (iterates over
`('k * 'v)` pairs), `'a set` (iterates over elements)

```miniml
fold (fn acc x -> acc + x) 0 [1; 2; 3]   (* 6 *)

for x in [1; 2; 3] do print x end
for x in [1; 2; 3] with acc = 0 do acc + x end   (* 6 *)
```

### Map

Operations on key-value collections. These methods are available as top-level
functions (`get`, `set`, `has`, `remove`, `size`, `keys`, `values`, `of_list`,
`to_list`) and are dispatched through the `Map` type class.

```
class Map 'm 'k 'v =
  of_list : ('k * 'v) list -> 'm
  get     : 'k -> 'm -> 'v option
  set     : 'k -> 'v -> 'm -> 'm
  has     : 'k -> 'm -> bool
  remove  : 'k -> 'm -> 'm
  size    : 'm -> int
  keys    : 'm -> 'k list
  values  : 'm -> 'v list
  to_list : 'm -> ('k * 'v) list
end
```

**Instances:** `('k, 'v) map` (the built-in persistent map type)

```miniml
let m = #{"a": 1; "b": 2}
get "a" m                    (* Some 1 *)
set "c" 3 m                  (* #{"c": 3; "a": 1; "b": 2} *)
has "a" m                    (* true *)
remove "a" m                 (* #{"b": 2} *)
size m                       (* 2 *)
keys m                       (* ["a"; "b"] *)
values m                     (* [1; 2] *)
to_list m                    (* [("a", 1); ("b", 2)] *)
of_list [("x", 1); ("y", 2)]  (* #{"x": 1; "y": 2} *)
```

### Index

Uniform indexing for collections. The `at` function provides a single interface
for accessing elements by key or index. The `.[expr]` dot-bracket syntax
desugars to `at`.

```
class Index 'c 'k 'v where 'c -> 'k 'v =
  at : 'k -> 'c -> 'v
end
```

**Functional dependency:** `'c -> 'k 'v` — the container type determines both
the key type and value type.

**Instances:**
- `'a array` — indexed by `int`, returns `'a`
- `string` — indexed by `int`, returns `byte`
- `('k, 'v) map` — indexed by `'k`, returns `'v`

```miniml
#[10; 20; 30].[1]                   (* 20 *)
"hello".[0]                          (* byte #68 *)
#{"name": "alice"}. ["name"]         (* "alice" *)
at 1 #[10; 20; 30]                  (* 20 *)
at "x" #{"x": 42}                   (* 42 *)
```

Note: for arrays and strings, out-of-bounds access raises a runtime error.
For maps, `.[key]` returns the value directly (not wrapped in `option`), and
raises a runtime error if the key is not found. Use `get key m` for the safe
`option`-returning version.

### Hash

Hash function for use with `Hashtbl`.

```
class Hash 'a =
  hash : 'a -> int
end
```

**Instances:** `int`, `string`, `bool`, `byte`, `rune`

---

## String Module

Operations on UTF-8 encoded strings. The `length`, `sub`, and `get` functions
operate on raw bytes. Use `rune_length`, `get_rune`, `to_runes`, and
`of_runes` for Unicode-aware operations.

### Byte-Level Operations

```
String.length : string -> int
```
Returns the byte length of the string.

```
String.sub : string -> int -> int -> string
```
`String.sub s start len` extracts a substring starting at byte offset `start`
with byte length `len`. Raises a runtime error if out of bounds.

```
String.get : string -> int -> byte
```
Returns the byte at the given index. Raises a runtime error if out of bounds.

```
String.to_bytes : string -> byte list
```
Converts a string to a list of bytes.

```
String.of_bytes : byte list -> string
```
Constructs a string from a list of bytes.

```
String.to_byte_array : string -> byte array
```
Converts a string to an array of bytes. More efficient than `to_bytes` for bulk operations like hashing or buffer manipulation.

```
String.of_byte_array : byte array -> string
```
Constructs a string from an array of bytes. More efficient than `of_bytes` for bulk operations.

### Unicode (Rune) Operations

```
String.rune_length : string -> int
```
Returns the number of Unicode code points in the string.

```
String.get_rune : string -> int -> rune
```
Returns the rune at the given code-point index. Raises a runtime error if out
of bounds.

```
String.to_runes : string -> rune list
```
Decodes a string into a list of Unicode code points.

```
String.of_runes : rune list -> string
```
Encodes a list of Unicode code points into a UTF-8 string.

### Construction

```
String.of_byte : byte -> string
```
Converts a single byte to a one-character string.

```
String.make : int -> byte -> string
```
`String.make n b` creates a string of `n` copies of the byte `b`.

```miniml
String.of_byte #41              (* "A" *)
String.make 3 #2a               (* "***" *)
```

### Searching and Testing

```
String.contains : string -> string -> bool
```
`String.contains sub s` returns `true` if `s` contains the substring `sub`.

```
String.starts_with : string -> string -> bool
```
`String.starts_with prefix s` returns `true` if `s` begins with `prefix`.

```
String.index_opt : string -> byte -> int option
```
`String.index_opt s b` returns `Some i` where `i` is the index of the first
occurrence of byte `b` in `s`, or `None` if not found.

```
String.rindex_opt : string -> byte -> int option
```
`String.rindex_opt s b` returns `Some i` where `i` is the index of the last
occurrence of byte `b` in `s`, or `None` if not found.

```miniml
String.index_opt "hello" #6c       (* Some 2 *)
String.rindex_opt "hello" #6c      (* Some 3 *)
String.index_opt "hello" #78       (* None *)
```

### Splitting and Joining

```
String.split : string -> string -> string list
```
`String.split sep s` splits `s` on the separator `sep`. If `sep` is the empty
string, splits into individual characters.

```
String.concat : string -> string list -> string
```
`String.concat sep strings` joins a list of strings with the separator `sep`
between each pair.

```miniml
String.split "," "a,b,c"            (* ["a"; "b"; "c"] *)
String.split "" "hi"                 (* ["h"; "i"] *)
String.concat ", " ["a"; "b"; "c"]  (* "a, b, c" *)
String.concat "" ["x"; "y"; "z"]    (* "xyz" *)
```

### Transformations

```
String.trim : string -> string
```
Removes leading and trailing whitespace.

```
String.uppercase : string -> string
```
Converts all ASCII characters to uppercase.

```
String.lowercase : string -> string
```
Converts all ASCII characters to lowercase.

```
String.replace : string -> string -> string -> string
```
`String.replace old new s` replaces all occurrences of `old` with `new` in `s`.

```miniml
String.replace "world" "ocaml" "hello world"   (* "hello ocaml" *)
```

### Parsing

```
String.to_int : string -> int option
```
Parses an integer from a string. Returns `None` on failure.

```
String.to_float : string -> float option
```
Parses a float from a string. Returns `None` on failure.

```miniml
String.to_int "42"     (* Some 42 *)
String.to_int "abc"    (* None *)
```

---

## List Module

Persistent singly-linked lists. All operations are pure (no mutation).

### Basic Operations

```
List.length : 'a list -> int
```
Returns the number of elements.

```
List.hd : 'a list -> 'a
```
Returns the first element. Partial: fails on an empty list.

```
List.tl : 'a list -> 'a list
```
Returns all elements except the first. Partial: fails on an empty list.

```
List.nth : 'a list -> int -> 'a
```
Returns the element at the given zero-based index. Partial.

```
List.is_empty : 'a list -> bool
```
Returns `true` if the list is empty.

```
List.rev : 'a list -> 'a list
```
Reverses a list.

### Building Lists

```
List.concat : 'a list -> 'a list -> 'a list
```
Concatenates two lists.

```
List.flatten : 'a list list -> 'a list
```
Flattens a list of lists into a single list.

```
List.init : int -> (int -> 'a) -> 'a list
```
`List.init n f` creates a list `[f 0; f 1; ...; f (n-1)]`.

```
List.concat_map : ('a -> 'b list) -> 'a list -> 'b list
```
Maps each element to a list and concatenates the results.

```miniml
List.concat [1; 2] [3; 4]                      (* [1; 2; 3; 4] *)
List.flatten [[1; 2]; [3]; [4; 5]]             (* [1; 2; 3; 4; 5] *)
List.init 4 (fn i -> i * i)                    (* [0; 1; 4; 9] *)
List.concat_map (fn x -> [x; x * 10]) [1; 2]  (* [1; 10; 2; 20] *)
```

### Transformations

```
List.map : ('a -> 'b) -> 'a list -> 'b list
```
Applies a function to every element, returning a new list.

```
List.mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
```
Like `map`, but the function also receives the zero-based index.

```
List.filter : ('a -> bool) -> 'a list -> 'a list
```
Returns elements for which the predicate returns `true`.

```
List.fold : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
```
Left fold. `List.fold f init [x1; x2; x3]` computes `f (f (f init x1) x2) x3`.

```
List.fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
```
Right fold. `List.fold_right f [x1; x2; x3] init` computes `f x1 (f x2 (f x3 init))`.

```miniml
List.map (fn x -> x + 1) [1; 2; 3]                 (* [2; 3; 4] *)
List.filter (fn x -> x > 2) [1; 2; 3; 4]           (* [3; 4] *)
List.fold (fn acc x -> acc + x) 0 [1; 2; 3]        (* 6 *)
List.fold_right (fn x acc -> x :: acc) [1; 2; 3] [] (* [1; 2; 3] *)
List.mapi (fn i x -> i + x) [10; 20; 30]           (* [10; 21; 32] *)
```

### Searching

```
List.find : ('a -> bool) -> 'a list -> 'a option
```
Returns the first element matching the predicate, or `None`.

```
List.exists : ('a -> bool) -> 'a list -> bool
```
Returns `true` if any element matches the predicate.

```
List.forall : ('a -> bool) -> 'a list -> bool
```
Returns `true` if all elements match the predicate.

```
List.find_map : ('a -> 'b option) -> 'a list -> 'b option
```
Applies the function to each element and returns the first `Some` result, or
`None` if no element produces `Some`.

```
List.assoc_opt : 'a -> ('a * 'b) list -> 'b option
```
Looks up a key in an association list, returning `Some value` or `None`.

```miniml
List.find (fn x -> x > 2) [1; 2; 3; 4]    (* Some 3 *)
List.exists (fn x -> x = 3) [1; 2; 3]     (* true *)
List.forall (fn x -> x > 0) [1; 2; 3]     (* true *)
List.find_map (fn x -> if x > 2 do Some (x * 10) else None) [1; 2; 3]
                                            (* Some 30 *)
List.assoc_opt "b" [("a", 1); ("b", 2)]   (* Some 2 *)
```

### Combining

```
List.zip : 'a list -> 'b list -> ('a * 'b) list
```
Pairs elements from two lists. Stops at the shorter list.

```
List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
```
Applies a function to corresponding elements of two lists. Stops at the
shorter list.

```
List.iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
```
Applies a side-effecting function to corresponding elements of two lists.

```miniml
List.map2 (fn a b -> a + b) [1; 2; 3] [10; 20; 30]  (* [11; 22; 33] *)
```

### Sorting

```
List.sort : ('a -> 'a -> int) -> 'a list -> 'a list
```
Sorts a list using a comparison function. The comparator should return a
negative value if the first argument is less, positive if greater, and zero
if equal.

```miniml
List.sort (fn a b -> if a < b do 0 - 1 else if a > b do 1 else 0) [3; 1; 4; 1; 5]
(* [1; 1; 3; 4; 5] *)
```

---

## Array Module

Mutable fixed-size arrays.

```
Array.make : int -> 'a -> 'a array
```
`Array.make n v` creates an array of size `n` filled with value `v`.

```
Array.length : 'a array -> int
```
Returns the number of elements.

```
Array.get : 'a array -> int -> 'a
```
Returns the element at the given index. Raises a runtime error if out of bounds.

```
Array.set : 'a array -> int -> 'a -> unit
```
Sets the element at the given index. Raises a runtime error if out of bounds.

```
Array.to_list : 'a array -> 'a list
```
Converts the array to a list.

```
Array.of_list : 'a list -> 'a array
```
Creates an array from a list.

```
Array.copy : 'a array -> 'a array
```
Returns a shallow copy of the array.

```
Array.sub : 'a array -> int -> int -> 'a array
```
`Array.sub arr start len` extracts a sub-array. Raises a runtime error if out
of bounds.

```
Array.init : int -> (int -> 'a) -> 'a array
```
`Array.init n f` creates an array `#[f 0; f 1; ...; f (n-1)]`.

```
Array.map : ('a -> 'b) -> 'a array -> 'b array
```
Applies a function to every element, returning a new array.

```
Array.iter : ('a -> unit) -> 'a array -> unit
```
Applies a side-effecting function to each element.

```
Array.fold : ('b -> 'a -> 'b) -> 'b -> 'a array -> 'b
```
Left fold over the array.

```miniml
let a = Array.make 3 0            (* #[0; 0; 0] *)
Array.set a 1 42
Array.get a 1                     (* 42 *)

let b = Array.of_list [1; 2; 3]   (* #[1; 2; 3] *)
Array.to_list b                   (* [1; 2; 3] *)
Array.sub b 1 2                   (* #[2; 3] *)
Array.init 4 (fn i -> i * i)      (* #[0; 1; 4; 9] *)
Array.map (fn x -> x + 1) b       (* #[2; 3; 4] *)
Array.fold (fn acc x -> acc + x) 0 b  (* 6 *)
```

---

## Set Module

Immutable sets, implemented on top of the built-in map type.
The underlying representation is `type 'a set = ('a, unit) map`.

```
Set.empty : unit -> 'a set
```
Creates an empty set.

```
Set.singleton : 'a -> 'a set
```
Creates a set with a single element.

```
Set.of_list : 'a list -> 'a set
```
Creates a set from a list, removing duplicates.

```
Set.add : 'a -> 'a set -> 'a set
```
Adds an element to the set.

```
Set.remove : 'a -> 'a set -> 'a set
```
Removes an element from the set.

```
Set.mem : 'a -> 'a set -> bool
```
Tests membership.

```
Set.size : 'a set -> int
```
Returns the number of elements.

```
Set.to_list : 'a set -> 'a list
```
Converts the set to a list.

```
Set.union : 'a set -> 'a set -> 'a set
```
Returns the union of two sets.

```
Set.inter : 'a set -> 'a set -> 'a set
```
Returns the intersection of two sets.

```
Set.diff : 'a set -> 'a set -> 'a set
```
Returns the set difference (elements in the first set not in the second).

```
Set.is_empty : 'a set -> bool
```
Returns `true` if the set is empty.

```
Set.is_subset : 'a set -> 'a set -> bool
```
`Set.is_subset s1 s2` returns `true` if every element of `s1` is in `s2`.

Sets have an `Iter` instance, so they can be used with `for` loops and `fold`:

```miniml
let s = Set.of_list [1; 2; 3]
Set.mem 2 s                        (* true *)
Set.size (Set.union s (Set.of_list [3; 4]))   (* 4 *)

for x in Set.of_list [10; 20; 30] with acc = 0 do
  acc + x
end                                (* 60 *)
```

---

## Hashtbl Module

Mutable hash tables with automatic resizing. Requires `Hash` and `Eq` instances
on the key type.

The underlying representation is:
```miniml
type ('k, 'v) Hashtbl.t = { mut buckets: ('k * 'v) list array; mut size: int }
```

```
Hashtbl.create : int -> ('k, 'v) Hashtbl.t
```
Creates a hash table with the given initial capacity (minimum 16).

```
Hashtbl.set : ('k, 'v) Hashtbl.t -> 'k -> 'v -> unit  where Hash 'k, Eq 'k
```
Inserts or updates a key-value pair. Automatically rehashes when the load
factor exceeds 2.

```
Hashtbl.get : ('k, 'v) Hashtbl.t -> 'k -> 'v option  where Hash 'k, Eq 'k
```
Looks up a key, returning `Some value` or `None`.

```
Hashtbl.has : ('k, 'v) Hashtbl.t -> 'k -> bool  where Hash 'k, Eq 'k
```
Returns `true` if the key exists.

```
Hashtbl.remove : ('k, 'v) Hashtbl.t -> 'k -> unit  where Hash 'k, Eq 'k
```
Removes a key from the table.

```
Hashtbl.length : ('k, 'v) Hashtbl.t -> int
```
Returns the number of entries.

```
Hashtbl.clear : ('k, 'v) Hashtbl.t -> unit
```
Removes all entries.

```
Hashtbl.to_list : ('k, 'v) Hashtbl.t -> ('k * 'v) list
```
Returns all key-value pairs as a list.

```
Hashtbl.keys : ('k, 'v) Hashtbl.t -> 'k list
```
Returns all keys.

```
Hashtbl.values : ('k, 'v) Hashtbl.t -> 'v list
```
Returns all values.

```miniml
let t = Hashtbl.create 16
Hashtbl.set t "a" 1
Hashtbl.set t "b" 2
Hashtbl.get t "a"      (* Some 1 *)
Hashtbl.has t "b"      (* true *)
Hashtbl.length t       (* 2 *)
Hashtbl.remove t "a"
Hashtbl.keys t         (* ["b"] *)
```

---

## Enum Module

Combinators for working with lists. These complement `List` with higher-level
operations inspired by Elixir's `Enum` module.

### Aggregation

```
Enum.sum : int list -> int
```
Sums all elements.

```
Enum.count : ('a -> bool) -> 'a list -> int
```
Counts elements matching the predicate.

```
Enum.reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
```
Reduces a non-empty list with no initial accumulator. Partial: fails on an
empty list.

### Slicing

```
Enum.take : int -> 'a list -> 'a list
```
Takes the first `n` elements.

```
Enum.drop : int -> 'a list -> 'a list
```
Drops the first `n` elements.

```
Enum.take_while : ('a -> bool) -> 'a list -> 'a list
```
Takes elements while the predicate holds.

```
Enum.drop_while : ('a -> bool) -> 'a list -> 'a list
```
Drops elements while the predicate holds.

```
Enum.chunk : int -> 'a list -> 'a list list
```
Splits a list into chunks of size `n`. The last chunk may be smaller.

### Transformations

```
Enum.flat_map : ('a -> 'b list) -> 'a list -> 'b list
```
Maps then flattens.

```
Enum.reject : ('a -> bool) -> 'a list -> 'a list
```
Returns elements for which the predicate returns `false` (inverse of `filter`).

```
Enum.enumerate : 'a list -> (int * 'a) list
```
Pairs each element with its zero-based index.

```
Enum.dedup : 'a list -> 'a list
```
Removes consecutive duplicates.

```
Enum.uniq : 'a list -> 'a list
```
Removes all duplicates (preserving first occurrence).

```
Enum.scan : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
```
Like `fold`, but returns all intermediate accumulator values.

```
Enum.intersperse : 'a -> 'a list -> 'a list
```
Inserts a separator between every two elements.

```
Enum.zip_with : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
```
Zips two lists with a combining function.

### Iteration

```
Enum.each : ('a -> unit) -> 'a list -> unit
```
Applies a side-effecting function to each element.

### Joining

```
Enum.join : string -> string list -> string
```
Joins a list of strings with a separator.

```miniml
Enum.join ", " ["a"; "b"; "c"]   (* "a, b, c" *)
```

### Selection

```
Enum.min_by : ('a -> 'b) -> 'a list -> 'a
```
Returns the element with the minimum value of the given function. Partial.

```
Enum.max_by : ('a -> 'b) -> 'a list -> 'a
```
Returns the element with the maximum value of the given function. Partial.

### Grouping

```
Enum.group_by : ('a -> 'k) -> 'a list -> ('k, 'a list) map
```
Groups elements by a key function, returning a map from keys to lists.

```miniml
Enum.sum [1; 2; 3; 4; 5]                        (* 15 *)
Enum.take 3 [1; 2; 3; 4; 5]                     (* [1; 2; 3] *)
Enum.drop 2 [1; 2; 3; 4; 5]                     (* [3; 4; 5] *)
Enum.flat_map (fn x -> [x; x * 10]) [1; 2; 3]   (* [1; 10; 2; 20; 3; 30] *)
Enum.chunk 2 [1; 2; 3; 4; 5]                    (* [[1; 2]; [3; 4]; [5]] *)
Enum.enumerate ["a"; "b"; "c"]                   (* [(0, "a"); (1, "b"); (2, "c")] *)
Enum.group_by (fn x -> x / 10) [11; 12; 21; 22; 31]
  (* #{ 1: [11; 12]; 2: [21; 22]; 3: [31] } *)
```

---

## Seq Module

Lazy sequences backed by push-based iterators. A sequence is a function
`('a -> unit) -> unit` that calls a yield callback for each element. Sequences
can be infinite and are consumed lazily via effects for early termination.

```
type 'a seq = ('a -> unit) -> unit
```

### Constructors

```
Seq.range : int -> int -> int seq
```
`Seq.range start stop` produces integers from `start` (inclusive) to `stop`
(exclusive).

```
Seq.of_list : 'a list -> 'a seq
```
Creates a sequence from a list.

```
Seq.repeat : 'a -> 'a seq
```
Creates an infinite sequence that repeats a value.

```
Seq.iterate : 'a -> ('a -> 'a) -> 'a seq
```
`Seq.iterate seed f` produces `seed`, `f seed`, `f (f seed)`, ...

### Transformations

```
Seq.map : ('a -> 'b) -> 'a seq -> 'b seq
```
Transforms each element.

```
Seq.filter : ('a -> bool) -> 'a seq -> 'a seq
```
Keeps elements matching the predicate.

```
Seq.flat_map : ('a -> 'b seq) -> 'a seq -> 'b seq
```
Maps each element to a sequence and flattens.

```
Seq.enumerate : 'a seq -> (int * 'a) seq
```
Pairs each element with its zero-based index.

### Slicing

```
Seq.take : int -> 'a seq -> 'a seq
```
Takes the first `n` elements. Works on infinite sequences.

```
Seq.take_while : ('a -> bool) -> 'a seq -> 'a seq
```
Takes elements while the predicate holds.

```
Seq.drop : int -> 'a seq -> 'a seq
```
Drops the first `n` elements.

```
Seq.drop_while : ('a -> bool) -> 'a seq -> 'a seq
```
Drops elements while the predicate holds.

```
Seq.chunk : int -> 'a seq -> 'a list seq
```
Groups consecutive elements into lists of size `n`.

### Consumption

```
Seq.to_list : 'a seq -> 'a list
```
Collects all elements into a list. Do not call on infinite sequences without
first limiting with `take`.

```
Seq.fold : ('b -> 'a -> 'b) -> 'b -> 'a seq -> 'b
```
Left fold over the sequence.

```
Seq.each : ('a -> unit) -> 'a seq -> unit
```
Applies a side-effecting function to each element.

```
Seq.count : 'a seq -> int
```
Counts the number of elements.

```
Seq.sum : int seq -> int
```
Sums all elements.

### Searching

```
Seq.find : ('a -> bool) -> 'a seq -> 'a option
```
Returns the first element matching the predicate, or `None`.

```
Seq.any : ('a -> bool) -> 'a seq -> bool
```
Returns `true` if any element matches.

```
Seq.all : ('a -> bool) -> 'a seq -> bool
```
Returns `true` if all elements match.

```miniml
Seq.to_list (Seq.range 1 5)                          (* [1; 2; 3; 4] *)
Seq.to_list (Seq.take 5 (Seq.repeat 42))             (* [42; 42; 42; 42; 42] *)
Seq.to_list (Seq.take 5 (Seq.iterate 1 (fn x -> x * 2)))
                                                      (* [1; 2; 4; 8; 16] *)
Seq.sum (Seq.range 1 6)                               (* 15 *)

(* Pipeline style *)
Seq.iterate 1 (fn x -> x + 1)
  |> Seq.filter (fn x -> x > 3)
  |> Seq.map (fn x -> x * 10)
  |> Seq.take 3
  |> Seq.to_list                                      (* [40; 50; 60] *)
```

---

## Option Module

Operations on `'a option` values (`Some x` or `None`).

```
Option.map : ('a -> 'b) -> 'a option -> 'b option
```
Applies a function to the value inside `Some`, or returns `None`.

```
Option.bind : ('a -> 'b option) -> 'a option -> 'b option
```
Chains an option-returning function. Also available as `Option.flat_map`.

```
Option.flat_map : ('a -> 'b option) -> 'a option -> 'b option
```
Alias for `Option.bind`.

```
Option.unwrap : 'a option -> 'a
```
Extracts the value from `Some`. Partial: fails on `None`.

```
Option.unwrap_or : 'a -> 'a option -> 'a
```
Extracts the value from `Some`, or returns the default.

```
Option.is_some : 'a option -> bool
```
Returns `true` if the value is `Some`.

```
Option.is_none : 'a option -> bool
```
Returns `true` if the value is `None`.

```
Option.to_list : 'a option -> 'a list
```
Returns a singleton list for `Some`, or an empty list for `None`.

```miniml
Option.map (fn x -> x + 1) (Some 5)     (* Some 6 *)
Option.map (fn x -> x + 1) None         (* None *)
Option.unwrap_or 0 (Some 42)            (* 42 *)
Option.unwrap_or 0 None                 (* 0 *)
Option.is_some (Some 1)                 (* true *)
Option.to_list (Some 5)                 (* [5] *)
```

---

## Result Module

Operations on `('a, 'b) result` values (`Ok x` or `Err e`).

The result type is defined as:
```miniml
type ('a, 'b) result = Ok of 'a | Err of 'b
```

```
Result.map : ('a -> 'c) -> ('a, 'b) result -> ('c, 'b) result
```
Applies a function to the `Ok` value, passing `Err` through.

```
Result.bind : ('a -> ('c, 'b) result) -> ('a, 'b) result -> ('c, 'b) result
```
Chains a result-returning function.

```
Result.unwrap : ('a, 'b) result -> 'a
```
Extracts the `Ok` value. Partial: fails on `Err`.

```miniml
Result.map (fn x -> x + 1) (Ok 10)                  (* Ok 11 *)
Result.map (fn x -> x + 1) (Err "bad")              (* Err "bad" *)
Result.bind (fn x -> Ok (x + 1)) (Ok 10)            (* Ok 11 *)
Result.unwrap (Ok 99)                                (* 99 *)
```

---

## Math Module

Mathematical functions.

```
Math.abs : int -> int
```
Absolute value (integer).

```
Math.min : int -> int -> int
```
Returns the smaller of two integers.

```
Math.max : int -> int -> int
```
Returns the larger of two integers.

```
Math.pow : float -> float -> float
```
`Math.pow base exp` raises `base` to the power `exp`.

```
Math.sqrt : float -> float
```
Square root.

```
Math.floor : float -> int
```
Rounds down to the nearest integer.

```
Math.ceil : float -> int
```
Rounds up to the nearest integer.

```
Math.round : float -> int
```
Rounds to the nearest integer (ties round to even in some implementations).

```miniml
Math.abs (0 - 5)         (* 5 *)
Math.pow 2.0 10.0        (* 1024.0 *)
Math.sqrt 16.0           (* 4.0 *)
Math.floor 3.7           (* 3 *)
Math.ceil 3.2            (* 4 *)
Math.round 3.5           (* 4 *)
```

---

## Buffer Module

Growable byte buffers, implemented in MiniML source on top of byte arrays.

The underlying representation is:
```miniml
type buffer = { mut data: byte array; mut len: int }
```

```
Buffer.create : int -> buffer
```
Creates a buffer with the given initial capacity (minimum 16).

```
Buffer.length : buffer -> int
```
Returns the number of bytes currently stored.

```
Buffer.clear : buffer -> unit
```
Resets the buffer length to 0 without deallocating.

```
Buffer.add_byte : buffer -> byte -> unit
```
Appends a single byte.

```
Buffer.add_string : buffer -> string -> unit
```
Appends all bytes of a string.

```
Buffer.add_buffer : buffer -> buffer -> unit
```
Appends the contents of another buffer.

```
Buffer.contents : buffer -> string
```
Returns the accumulated contents as a string.

```
Buffer.grow : buffer -> int -> unit
```
Ensures there is room for `needed` additional bytes, resizing if necessary.
Typically called internally by the other `add_*` functions.

```miniml
let buf = Buffer.create 64
Buffer.add_string buf "hello"
Buffer.add_string buf " world"
Buffer.contents buf              (* "hello world" *)
Buffer.length buf                (* 11 *)
```

---

## Ref Module

First-class mutable references. A ref is a record with a single mutable field
`contents`.

The underlying representation is:
```miniml
type 'a t = { mut contents: 'a }
```

```
Ref.create : 'a -> 'a Ref.t
```
Creates a new ref with the given initial value.

```
Ref.get : 'a Ref.t -> 'a
```
Returns the current value.

```
Ref.set : 'a Ref.t -> 'a -> unit
```
Sets the value.

You can also access and mutate the `contents` field directly:

```miniml
let r = Ref.create 42
Ref.get r                  (* 42 *)
Ref.set r 99
r.contents                 (* 99 *)
r.contents := 0
Ref.get r                  (* 0 *)
```

---

## Dynarray Module

Growable dynamic arrays backed by a fixed-size array that is resized as needed.

The underlying representation is:
```miniml
type 'a t = { mut arr: 'a array; mut count: int }
```

```
Dynarray.create : int -> 'a -> 'a Dynarray.t
```
`Dynarray.create n default` creates a dynarray with initial capacity `n`
(minimum 16). The `default` value fills unused slots.

```
Dynarray.length : 'a Dynarray.t -> int
```
Returns the number of elements currently stored.

```
Dynarray.get : 'a Dynarray.t -> int -> 'a
```
Returns the element at the given index. Raises a runtime error if out of bounds.

```
Dynarray.set : 'a Dynarray.t -> int -> 'a -> unit
```
Sets the element at the given index. Raises a runtime error if out of bounds.

```
Dynarray.push : 'a Dynarray.t -> 'a -> unit
```
Appends an element, growing the backing array if necessary.

```
Dynarray.pop : 'a Dynarray.t -> 'a
```
Removes and returns the last element. Raises a runtime error if empty.

```
Dynarray.clear : 'a Dynarray.t -> unit
```
Resets the length to 0 without deallocating.

```
Dynarray.to_list : 'a Dynarray.t -> 'a list
```
Returns the elements as a list.

```
Dynarray.to_array : 'a Dynarray.t -> 'a array
```
Returns a copy of the elements as a fixed-size array.

```
Dynarray.grow : 'a Dynarray.t -> int -> 'a -> unit
```
Ensures there is room for `needed` additional elements, resizing if necessary.
The `default` value fills new slots. Typically called internally by `push`.

```miniml
let d = Dynarray.create 4 0
Dynarray.push d 10
Dynarray.push d 20
Dynarray.push d 30
Dynarray.length d             (* 3 *)
Dynarray.get d 1              (* 20 *)
Dynarray.pop d                (* 30 *)
Dynarray.to_list d            (* [10; 20] *)
```

---

## Fmt Module

Formatting utilities for converting integers to various bases and padding
strings.

```
Fmt.pad_left : int -> string -> string -> string
```
`Fmt.pad_left width pad_char s` pads `s` on the left to the given width
using `pad_char` (a single-character string).

```
Fmt.pad_right : int -> string -> string -> string
```
`Fmt.pad_right width pad_char s` pads `s` on the right.

```
Fmt.int_to_hex : int -> string
```
Converts an integer to a lowercase hexadecimal string.

```
Fmt.int_to_bin : int -> string
```
Converts an integer to a binary string.

```
Fmt.int_to_oct : int -> string
```
Converts an integer to an octal string.

```
Fmt.zero_pad : int -> string -> string
```
Pads a string with leading zeros to the given width. Shorthand for
`Fmt.pad_left width "0" s`.

```miniml
Fmt.int_to_hex 255         (* "ff" *)
Fmt.int_to_bin 10          (* "1010" *)
Fmt.int_to_oct 8           (* "10" *)
Fmt.zero_pad 4 "42"        (* "0042" *)
Fmt.pad_left 10 " " "hi"  (* "        hi" *)
```

---

## Byte Module

Operations on the `byte` type (0-255, representing a single raw byte).
Byte literals are written as `#xx` in hexadecimal (e.g. `#41` for ASCII `A`).

```
Byte.to_int : byte -> int
```
Converts a byte to its integer value.

```
Byte.of_int : int -> byte
```
Converts an integer to a byte (masks to low 8 bits).

```
Byte.to_string : byte -> string
```
Converts a byte to a single-character string.

```
Byte.is_alpha : byte -> bool
```
Returns `true` if the byte is an ASCII letter (A-Z or a-z).

```
Byte.is_digit : byte -> bool
```
Returns `true` if the byte is an ASCII digit (0-9).

```
Byte.is_space : byte -> bool
```
Returns `true` if the byte is ASCII whitespace (space, tab, newline, carriage return).

```
Byte.is_upper : byte -> bool
```
Returns `true` if the byte is an ASCII uppercase letter.

```
Byte.is_lower : byte -> bool
```
Returns `true` if the byte is an ASCII lowercase letter.

```
Byte.to_upper : byte -> byte
```
Converts an ASCII lowercase letter to uppercase. Returns the byte unchanged
if it is not lowercase.

```
Byte.to_lower : byte -> byte
```
Converts an ASCII uppercase letter to lowercase. Returns the byte unchanged
if it is not uppercase.

```miniml
Byte.to_int #41           (* 65 *)
Byte.to_string #41        (* "A" *)
Byte.is_alpha #41         (* true *)
Byte.is_digit #39         (* true *)
Byte.to_upper #61         (* #41 *)
```

---

## Rune Module

Operations on the `rune` type (Unicode code points). Rune literals are
written with single quotes (e.g. `'a'` for U+0061). Escape sequences are
supported: `'\n'`, `'\t'`, `'\0'`.

```
Rune.to_int : rune -> int
```
Returns the Unicode code point as an integer.

```
Rune.of_int : int -> rune
```
Creates a rune from a code point integer.

```
Rune.to_string : rune -> string
```
Encodes a rune as a UTF-8 string.

```
Rune.is_alpha : rune -> bool
```
Returns `true` if the rune is an ASCII letter.

```
Rune.is_digit : rune -> bool
```
Returns `true` if the rune is an ASCII digit.

```
Rune.is_space : rune -> bool
```
Returns `true` if the rune is ASCII whitespace.

```
Rune.is_upper : rune -> bool
```
Returns `true` if the rune is an ASCII uppercase letter.

```
Rune.is_lower : rune -> bool
```
Returns `true` if the rune is an ASCII lowercase letter.

```miniml
Rune.to_int 'a'            (* 97 *)
Rune.to_string 'a'         (* "a" *)
Rune.is_alpha 'a'          (* true *)
Rune.is_digit '5'          (* true *)
Rune.to_int '\n'           (* 10 *)
```

---

## IO Module

File and console I/O operations. These perform real side effects.

```
IO.read_file : string -> string
```
Reads the entire contents of a file as a string. Raises a runtime error if the
file cannot be read.

```
IO.write_file : string -> string -> unit
```
`IO.write_file path contents` writes `contents` to `path`, creating or
overwriting the file.

```
IO.append_file : string -> string -> unit
```
`IO.append_file path contents` appends `contents` to the file at `path`,
creating it if it does not exist.

```
IO.read_line : unit -> string
```
Reads a line from standard input. Returns an empty string at end of file.

```
IO.file_exists : string -> bool
```
Returns `true` if the file or directory exists.

```miniml
IO.write_file "/tmp/hello.txt" "Hello, world!\n"
let s = IO.read_file "/tmp/hello.txt"       (* "Hello, world!\n" *)
IO.file_exists "/tmp/hello.txt"             (* true *)
IO.append_file "/tmp/hello.txt" "More.\n"
```

---

## Sys Module

System-level operations.

```
Sys.args : unit -> string list
```
Returns the command-line arguments.

```
Sys.getenv : string -> string option
```
Looks up an environment variable. Returns `None` if not set.

```
Sys.exit : int -> unit
```
Exits the process with the given status code.

```
Sys.time : unit -> float
```
Returns the current wall-clock time as seconds since the Unix epoch.

```miniml
let args = Sys.args ()
match Sys.getenv "HOME" with
| Some home -> print home
| None -> print "HOME not set"
```

---

## Runtime Module

Dynamic code evaluation (native backend only; not available in the JS VM).

```
Runtime.eval : string -> unit
```
Evaluates a MiniML source string. Definitions become available in the current
scope.

```
Runtime.eval_file : string -> unit
```
Evaluates a MiniML source file. Definitions (including modules) become
available in the current scope.

```miniml
Runtime.eval "let x = 42"
Runtime.eval "print x"                (* prints 42 *)
Runtime.eval_file "my_module.ml"
```
