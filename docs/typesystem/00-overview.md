# How the MiniML Type System Works

## Introduction

MiniML is a statically-typed functional language with Hindley-Milner type inference,
Haskell-style type classes, row-polymorphic records, polymorphic variants, algebraic
effects, and GADTs (generalized algebraic data types). The type system infers types
for nearly
every expression with no annotations required -- you write `let f x = x + 1` and
the typechecker figures out that `f : int -> int`.

A type system serves three purposes:

1. **Error detection.** It catches mistakes at compile time rather than at runtime.
   Passing a string to a function expecting an int is a type error, not a segfault.
2. **Documentation.** Inferred types describe what a function accepts and returns.
   The REPL command `:type map` tells you `('a -> 'b) -> 'a list -> 'b list` without
   reading any source.
3. **Enabling compilation.** The compiler needs to know which variant tag a constructor
   belongs to, how many fields a record has, and which dictionary to pass for a type
   class method. Type information drives code generation.

If you have used OCaml or Haskell, you already know what these features feel like as
a user. This document series explains how they are implemented -- what data structures
represent types, how unification works, how `let`-polymorphism is achieved, and how
type class constraints are compiled away into dictionary passing.


## Architecture Overview

The typechecker sits between the parser and the compiler in MiniML's pipeline:

```
Source code
    |
    v
  Lexer / Parser    --> produces untyped AST (Ast.program)
    |
    v
  Typechecker        --> produces typed AST (Typechecker.tprogram)
    |
    v
  Compiler           --> produces bytecode
    |
    v
  VM                 --> executes bytecode
```

The typechecker itself performs **two passes** over the AST:

### Pass 1: Type Inference

The first pass walks the untyped AST (`Ast.expr` nodes) and produces a typed AST
(`texpr` nodes, where every subexpression carries its inferred type). This is where
the core type inference algorithm runs:

- **Unification** makes two types equal by mutating type variable cells.
- **Generalization** turns monomorphic types into polymorphic schemes at `let`
  boundaries.
- **Instantiation** replaces quantified variables with fresh type variables at use
  sites.
- **Constraint collection** records which type class constraints apply to which
  type variables.

The entry point is `check_program_in_ctx`, which folds over each top-level
declaration, threading the typing context (`ctx`) through:

```ocaml
let check_program_in_ctx (ctx : ctx) (program : Ast.program) : ctx * tprogram =
  let ctx, decls = List.fold_left (fun (ctx, decls) decl ->
    let ctx', tdecls = check_decl ctx 0 decl in
    (ctx', List.rev_append tdecls decls)
  ) (ctx, []) program in
  (ctx, List.rev decls)
```

### Pass 2: Constraint Transformation

The second pass walks the typed AST and rewrites it to implement dictionary passing
for type classes. A function with a constraint like `Show 'a =>` gets an extra
parameter for the dictionary; each call site gets an extra argument with the resolved
dictionary value:

```
(* Before transformation *)
let show_pair x y = show x ^ ", " ^ show y
(* After transformation -- conceptually *)
let show_pair __dict_Show_0 x y = __dict_Show_0.show x ^ ", " ^ __dict_Show_0.show y
```

The entry point is `transform_constraints`. After this pass, the typed AST contains
no unresolved type class references -- every method call has been replaced by a field
access on a concrete dictionary record. The compiler never sees type classes.

In the actual pipeline, these two passes are called in sequence:

```ocaml
let typed_program =
  Typechecker.check_program_in_ctx state.ctx program in
let typed_program =
  Typechecker.transform_constraints ctx' typed_program in
let compiled =
  Compiler.compile_program_with_globals ... typed_program ...
```


## Key Data Structures

The type system is built around a small set of mutually recursive data types defined
in `lib/types.ml` and `lib/typechecker.ml`. Here is a brief overview of each; later
chapters cover them in detail.

### `ty` -- The Type Language

Every type in MiniML is represented as a value of type `ty`:

```ocaml
type ty =
  | TInt | TFloat | TBool | TString | TByte | TRune | TUnit
  | TArrow of ty * eff * ty        (* function types: a -[eff]-> b *)
  | TCont of ty * eff * ty         (* continuation types: arg -[eff]-> result *)
  | TTuple of ty list              (* tuples: a * b * c *)
  | TList of ty                    (* lists: 'a list *)
  | TRecord of record_row          (* row-typed records: { x: int; y: string; .. } *)
  | TVariant of string * ty list   (* named variants: 'a option, ('k,'v) map, etc. *)
  | TArray of ty                   (* arrays: 'a array *)
  | TPolyVariant of pvrow          (* polymorphic variants: [`Foo | `Bar of int] *)
  | TVar of tvar ref               (* mutable type variable cell *)
  | TGen of int                    (* quantified variable in a scheme *)
```

The two most important constructors for understanding inference are `TVar` and
`TGen`. A `TVar` is a mutable cell used during inference -- it starts as an unknown
and gets filled in by unification. A `TGen` is a placeholder used inside polymorphic
type schemes to represent a universally quantified variable.

### `tvar` -- Type Variable States

Each type variable cell contains one of two states:

```ocaml
type tvar =
  | Unbound of int * int    (* id, level *)
  | Link of ty              (* resolved: points to another type *)
```

An `Unbound` variable has not yet been determined. Its `id` is unique and its `level`
tracks how deeply nested it was created (used by generalization to decide which
variables can be made polymorphic). A `Link` variable has been unified with some other
type -- following the link gives you the resolved type. This is a union-find structure:
the `repr` function chases links with path compression.

```ocaml
let rec repr = function
  | TVar ({ contents = Link ty } as r) ->
    let ty = repr ty in
    r := Link ty;       (* path compression *)
    ty
  | ty -> ty
```

### `scheme` -- Polymorphic Type Schemes

A scheme wraps a type body with information about how many variables are quantified
and what constraints apply:

```ocaml
type scheme = {
  quant: int;                              (* number of quantified type variables *)
  equant: int;                             (* number of quantified effect variables *)
  pvquant: int;                            (* number of quantified poly variant row variables *)
  rquant: int;                             (* number of quantified record row variables *)
  constraints: class_constraint list;      (* typeclass constraints *)
  record_evidences: record_evidence list;  (* field offset evidence for row-polymorphic updates *)
  body: ty;                                (* body containing TGen 0..quant-1 *)
}
```

For example, `map : ('a -> 'b) -> 'a list -> 'b list` is represented as:
```
{ quant = 2; equant = 0; pvquant = 0; rquant = 0;
  constraints = []; record_evidences = [];
  body = TArrow(TArrow(TGen 0, EffEmpty, TGen 1), EffEmpty,
         TArrow(TList(TGen 0), EffEmpty, TList(TGen 1))) }
```

A monomorphic type (like `int -> int`) is just a scheme with `quant = 0`.

### `eff` -- Effect Row Types

Function arrows carry an effect annotation describing which effects the function
may perform. Effects use row-polymorphic types:

```ocaml
type eff =
  | EffVar of effvar ref       (* effect row variable — unified like TVar *)
  | EffEmpty                   (* pure: no effects *)
  | EffRow of string * ty list * eff  (* one effect label + type params + tail row *)
  | EffGen of int              (* quantified effect variable in a scheme *)

and effvar =
  | EffUnbound of int * int    (* id, level *)
  | EffLink of eff             (* linked by unification *)
```

Multiple effects are represented as nested rows: a function performing both
`Console` and `State` effects has effect type
`EffRow("Console", [], EffRow("State", [], tail))`. The `ty list` in `EffRow`
carries type parameters for parameterized effects -- for example, `State int`
is represented as `EffRow("State", [TInt], tail)`.
Effect row variables allow effect polymorphism -- a higher-order function like `map`
can accept callbacks with any effects. Effects are fully inferred by default;
users can optionally write explicit effect annotations using `/` syntax.

### `pvrow` -- Polymorphic Variant Row Types

Polymorphic variants (e.g., `` `Foo ``, `` `Bar 42 ``) use Rémy-style row types,
similar to effects. A poly variant type is a chain of tags with an optional tail
variable for extensibility:

```ocaml
and pvrow =
  | PVRow of string * ty option * pvrow  (* tag, optional payload type, tail *)
  | PVVar of pvvar ref                   (* row variable — open variant *)
  | PVEmpty                              (* closed row — exact type *)
  | PVGen of int                         (* quantified row variable *)

and pvvar =
  | PVUnbound of int * int               (* id, level *)
  | PVLink of pvrow                      (* linked by unification *)
```

For example, `` [`Foo | `Bar of int] `` is
`PVRow("Foo", None, PVRow("Bar", Some TInt, PVEmpty))`. An open variant like
`` [> `Foo] `` uses a `PVVar` tail instead of `PVEmpty`, allowing additional tags.

### `record_row` -- Record Row Types

Records use row types for structural subtyping. Instead of a flat field list,
record types are represented as a chain of field entries with a row tail:

```ocaml
and record_row =
  | RRow of string * ty * record_row  (* field name, type, tail *)
  | RVar of rvar ref                  (* row variable — "and more fields" *)
  | REmpty                            (* closed — exact record *)
  | RGen of int                       (* quantified row variable *)
  | RWild                             (* recursive self-reference *)

and rvar =
  | RUnbound of int * int             (* id, level *)
  | RLink of record_row               (* linked by unification *)
```

A closed record `{ x: int; y: string }` is
`RRow("x", TInt, RRow("y", TString, REmpty))`. A function like `let get_x r = r.x`
infers an open record type `{ x: 'a; .. }` represented as
`RRow("x", TVar(...), RVar(...))`, where the `RVar` tail allows records with
additional fields. This enables row-polymorphic record operations -- a function
that accesses field `x` accepts any record containing that field.

### `texpr` -- Typed Expressions

The typed AST mirrors the untyped AST, but every node carries its inferred type:

```ocaml
type texpr = {
  expr: texpr_kind;
  ty: Types.ty;
}
```

For instance, where the parser produces `Ast.EApp(Ast.EVar "f", Ast.EInt 3)`, the
typechecker produces:
```
{ expr = TEApp(
    { expr = TEVar "f"; ty = TArrow(TInt, EffEmpty, TInt) },
    { expr = TEInt 3; ty = TInt });
  ty = TInt }
```

### `ctx` -- The Typing Context

The context threads through inference, accumulating variable bindings:

```ocaml
type ctx = {
  vars: (string * Types.scheme) list;   (* variable -> polymorphic scheme *)
  mutable_vars: string list;            (* names of mutable bindings *)
  type_env: Types.type_env;             (* global type definitions *)
  loop_info: loop_info option;          (* WhileLoop | UnitLoop | FoldLoop *)
  current_module: string option;        (* current module being typechecked *)
  constraint_tvars: int list;           (* tvar ids from typeclass constraints *)
}
```

When inference enters a `let` binding, it extends `ctx.vars` with the new name and
its generalized scheme. The context is an association list -- simple and functional.

### `type_env` -- Global Type Environment

While `ctx.vars` tracks value-level bindings, `type_env` tracks type-level
definitions that persist across declarations:

```ocaml
type type_env = {
  variants: (string * int * variant_def * bool) list;  (* variant types; bool is is_gadt *)
  constructors: (string * ctor_info) list;       (* constructor lookup table *)
  records: (string * (string * ty) list) list;   (* named record types *)
  classes: class_def list;                        (* type class definitions *)
  instances: instance_def list;                   (* type class instances *)
  effects: effect_def list;                       (* effect definitions *)
  mutable_fields: string list;                    (* fields declared mutable *)
  modules: (string * module_info) list;           (* loaded modules *)
  type_aliases: (string * string) list;           (* short -> qualified name *)
  type_synonyms: (string * int * ty) list;        (* type synonyms *)
  newtypes: string list;                           (* newtype names for constructor erasure *)
}
```

This is populated as `type`, `class`, `instance`, and `effect` declarations are
processed.

The `variants` field stores a 4-tuple per variant type. The first three elements
(name, number of type parameters, and constructor definitions) are present for all
ADTs. The fourth element, `is_gadt`, is `true` when the type was declared with
GADT syntax -- i.e., when at least one constructor specifies an explicit return type
whose type parameters differ from the declaration parameters.

### `ctor_info` -- Constructor Metadata

Each constructor in the `constructors` table carries information needed for type
inference and pattern matching:

```ocaml
type ctor_info = {
  ctor_type_name: string;                 (* which variant type this belongs to *)
  ctor_arg_ty: ty option;                 (* argument type, if any *)
  ctor_num_params: int;                   (* type parameters of the parent type *)
  ctor_return_ty_params: ty list option;  (* None = ADT, Some = GADT return type params *)
  ctor_existentials: int;                 (* existential type vars in this constructor *)
}
```

For a regular ADT constructor, `ctor_return_ty_params` is `None` and
`ctor_existentials` is `0`. For a GADT constructor, `ctor_return_ty_params` is
`Some params` where `params` are the type arguments in the constructor's explicit
return type, and `ctor_existentials` counts the type variables that appear in the
constructor's argument but not in its return type. These existential variables are
scoped to the pattern branch that matches the constructor.


## The Two Core Operations

The entire inference algorithm rests on two operations: unification and
generalization. Everything else -- checking function applications, inferring
`let`-bindings, resolving constructors -- is orchestrated around these two.

### Unification

Unification answers the question: "Can these two types be made equal?" It walks two
types in lockstep, and when it hits a type variable, it binds that variable to the
other type (by mutating the `tvar ref` cell to `Link`).

A simplified sketch:

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
  | TInt, TInt -> ()
  (* ... other ground types ... *)
  | _ -> raise (Unify_error "...")
```

The `occurs_check` prevents infinite types (like `'a = 'a -> 'a`) and adjusts
levels for correct generalization. The actual implementation also handles row-based
unification for records and polymorphic variants -- record rows and poly variant
rows are unified using Rémy-style row rewriting, enabling structural subtyping
where a record with fields `{x; y; z}` can unify with the open type `{x; y; ..}`.

### Generalization and Instantiation

**Generalization** happens at `let`-boundaries. After inferring the type of a
`let`-bound expression at an incremented level, any type variables that are still
`Unbound` at a level deeper than the current scope get replaced with `TGen` indices.
This is what gives `let`-polymorphism: the identity function `fun x -> x` gets
generalized to the scheme `forall 'a. 'a -> 'a`.

```ocaml
let generalize level ty =
  let id_map = Hashtbl.create 8 in
  let counter = ref 0 in
  let rec go ty =
    match repr ty with
    | TVar { contents = Unbound (id, level') } when level' > level ->
      (* This variable was created at a deeper level -- quantify it *)
      (match Hashtbl.find_opt id_map id with
       | Some gen_id -> TGen gen_id
       | None ->
         let gen_id = !counter in
         counter := gen_id + 1;
         Hashtbl.replace id_map id gen_id;
         TGen gen_id)
    | TVar { contents = Unbound _ } as t -> t  (* not deep enough *)
    | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
    (* ... recurse into other type constructors ... *)
  in
  let body = go ty in
  { quant = !counter; equant = ...; pvquant = ...; rquant = ...;
    constraints = []; record_evidences = []; body }
```

**Instantiation** is the reverse: when a polymorphic name is used, each `TGen` is
replaced with a fresh `Unbound` type variable so the use site gets its own copy of
the type:

```ocaml
let instantiate level (s : scheme) =
  if s.quant = 0 then s.body
  else begin
    let vars = Array.init s.quant (fun _ -> new_tvar level) in
    let rec go = function
      | TGen id -> vars.(id)
      | TArrow (a, eff, r) -> TArrow (go a, go_eff eff, go r)
      (* ... *)
      | t -> t
    in
    go s.body
  end
```

This is why you can use `map` at type `(int -> string) -> int list -> string list`
in one place and `(string -> int) -> string list -> int list` in another. Each use
gets fresh type variables that are independently unified with the surrounding
context.


## How It Fits Together: A Worked Example

Consider this MiniML program:

```
let id x = x
let n = id 42
let s = id "hello"
```

Here is what the typechecker does, step by step:

1. **`let id x = x`**: Enter `synth` for the `EFun`. Create a fresh type variable
   `'a` for `x`. The body is `EVar "x"`, which looks up `x` in the context and
   returns `'a`. So `id` has monomorphic type `'a -> 'a`. Now generalize: `'a` is
   `Unbound` at a deeper level, so it becomes `TGen 0`. The scheme is
   `{ quant=1; body=TArrow(TGen 0, EffEmpty, TGen 0) }`.

2. **`let n = id 42`**: Look up `id` and instantiate its scheme with a fresh variable
   `'b`, giving `'b -> 'b`. Synthesize `42` as `TInt`. Unify `'b -> 'b` with
   `TInt -> ?ret`, which makes `'b = TInt` and `?ret = TInt`. The result type of
   `id 42` is `TInt`.

3. **`let s = id "hello"`**: Look up `id` and instantiate again with a fresh `'c`,
   giving `'c -> 'c`. Synthesize `"hello"` as `TString`. Unify, get `'c = TString`.
   The result type is `TString`.

This is let-polymorphism in action: `id` was used at two different types because
each use site instantiated the scheme with independent type variables.


## Document Roadmap

This document gave a high-level overview. The rest of the series dives into each
component:

1. **[Types and Representation](01-types.md)** -- How types are represented internally.
   The `ty` type in detail, the union-find structure, `repr` and path compression,
   how `TGen` indices work in schemes.

2. **[Unification](02-unification.md)** -- The unification algorithm. Occurs check,
   level adjustment, structural record subtyping, error reporting.

3. **[Inference](03-inference.md)** -- How type inference works expression by
   expression. The `synth` and `check` functions (bidirectional type checking), how
   application, `if`, `match`, and other forms are handled.

4. **[Generalization](04-generalization.md)** -- Let-polymorphism and instantiation.
   Levels-based generalization, the value restriction for mutable bindings, how
   `let rec` is handled.

5. **[Type Classes](05-typeclasses.md)** -- Dictionary passing and constraint
   resolution. How classes and instances are registered, how the constraint
   transformation pass rewrites the typed AST, how constrained functions get
   dictionary parameters.

6. **[Pattern Matching](06-patterns.md)** -- Pattern checking and exhaustiveness.
   How patterns are typechecked against a scrutinee type, constructor resolution,
   record pattern inference, exhaustiveness analysis.

7. **[Advanced Topics](07-advanced.md)** -- GADTs (existential types, refined
   pattern matching, GADT type checking), effects and handlers, modules and
   visibility, mutual recursion (`let rec ... and`), deriving mechanisms, type
   synonyms and aliases.
