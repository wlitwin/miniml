# Part 6: Pattern Matching and Exhaustiveness Checking

This chapter explains how the MiniML typechecker handles pattern matching.
Pattern matching involves two distinct problems: typechecking each pattern
against a scrutinee type (so that bound variables get the right types), and
verifying that a set of patterns covers all possible values (exhaustiveness
checking). Both are implemented in `lib/typechecker.ml`.


## How Match Expressions Are Typechecked

A match expression has this shape in the AST:

```ocaml
(* From lib/ast.ml *)
| EMatch of expr * (pattern * expr option * expr) list * bool
```

The three components are: a scrutinee expression, a list of arms (each with a
pattern, an optional guard, and a body expression), and a boolean flag
indicating whether the match is "partial" (i.e., whether to skip
exhaustiveness checking). The guard is the `when` clause -- `| pat when cond -> body`.

The typechecking logic lives in `synth_match`:

```ocaml
and synth_match ?expected_ty ctx level scrut arms partial =
  let scrut_te = synth ctx level scrut in
  ...
```

The function accepts an optional `?expected_ty` parameter. For regular ADT
matches this parameter is unused -- the result type is synthesized from the
first arm's body and unified across the remaining arms. For GADT matches,
`expected_ty` is required to check each arm's body against a known result
type (see the GADT pattern matching section below).

The `check` function has a specific case for `EMatch` that propagates its
expected type into `synth_match`:

```ocaml
  | Ast.EMatch (scrut, arms, partial) ->
    synth_match ~expected_ty:expected ctx level scrut arms partial
```

In practice this means a GADT match must appear in a context where the
result type is known, typically under a type annotation on the enclosing
function (e.g., `let eval (e : int expr) : int = match e with ...`).

When the scrutinee is not a GADT type, `synth_match` follows the original
ADT logic:

```ocaml
  | None ->
    (* Regular ADT match -- original logic *)
    let check_arm ctx (pat, guard, body) =
      let bindings = check_pattern ctx level pat scrut_te.ty in
      let ctx' = List.fold_left (fun c (n, t) -> extend_var_mono c n t) ctx bindings in
      let guard_te = match guard with
        | Some g ->
          let gte = check ctx' level g Types.TBool in
          Some gte
        | None -> None
      in
      let body_te = synth ctx' level body in
      (pat, guard_te, body_te)
    in
    match arms with
    | [] -> error "match expression has no arms"
    | first :: rest ->
      let (fp, fg, fb) = check_arm ctx first in
      let result_ty = fb.ty in
      let all_arms = (fp, fg, fb) ::
        List.map (fun arm ->
          let (p, g, b) = check_arm ctx arm in
          try_unify b.ty result_ty;
          (p, g, b)
        ) rest
      in
      (* Check exhaustiveness *)
      if not partial then
        !exhaustiveness_check_ref ctx (Types.repr scrut_te.ty) all_arms;
      mk (TEMatch (scrut_te, all_arms, partial)) result_ty
```

Walking through the steps (for the regular ADT path):

1. **Synthesize the scrutinee.** The expression being matched is typechecked
   first, producing a typed expression `scrut_te` with a known type.

2. **Check each arm.** For every `(pattern, guard, body)` triple:
   - Call `check_pattern` to typecheck the pattern against the scrutinee's
     type. This returns a list of variable bindings -- names and their types.
   - Extend the typing context with those bindings (monomorphically -- pattern
     variables are not generalized).
   - If there is a guard expression, check it against `TBool` in the extended
     context.
   - Synthesize the body expression in the extended context.

3. **Unify arm bodies.** The first arm's body type becomes the "expected" result
   type. Every subsequent arm's body type is unified with it. This enforces that
   all branches produce the same type.

4. **Exhaustiveness.** If the match is not marked as partial, invoke the
   exhaustiveness checker. This is done through a forward reference
   (`exhaustiveness_check_ref`) because the exhaustiveness logic is defined
   after the main inference functions.

The result type of the entire match expression is the type of the first arm's
body (which, after unification, equals the type of every arm's body).


## The Pattern Type: What Forms Exist

The pattern AST is defined in `lib/ast.ml`:

```ocaml
type pattern =
  | PatWild
  | PatVar of string
  | PatInt of int
  | PatFloat of float
  | PatBool of bool
  | PatString of string
  | PatUnit
  | PatTuple of pattern list
  | PatCons of pattern * pattern
  | PatNil
  | PatConstruct of string * pattern option
  | PatRecord of (string * pattern) list
  | PatAs of pattern * string
  | PatOr of pattern * pattern
  | PatArray of pattern list
  | PatMap of (pattern * pattern) list
```

Each of these forms has a corresponding case in `check_pattern`.


## The `check_pattern` Function

`check_pattern` takes a pattern and the type it must match (the scrutinee
type), and returns a list of variable bindings `(string * ty) list`. These
bindings are the names and types that the pattern introduces into scope for
the arm's guard and body.

```ocaml
and check_pattern ctx level (pat : Ast.pattern) (ty : Types.ty)
    : (string * Types.ty) list =
```

The function works by case analysis on the pattern form.

### Wildcard and Variable Patterns

```ocaml
  | Ast.PatWild -> []
  | Ast.PatVar name -> [(name, ty)]
```

A wildcard matches any value and binds nothing. A variable pattern matches any
value and binds the variable name to the scrutinee type. These are the simplest
cases.

If you write `match x with _ -> ...`, the wildcard contributes no bindings.
If you write `match x with n -> ...`, the variable `n` gets the type of `x`.

### Literal Patterns

```ocaml
  | Ast.PatInt _ -> try_unify ty Types.TInt; []
  | Ast.PatFloat _ -> try_unify ty Types.TFloat; []
  | Ast.PatBool _ -> try_unify ty Types.TBool; []
  | Ast.PatString _ -> try_unify ty Types.TString; []
  | Ast.PatUnit -> try_unify ty Types.TUnit; []
```

Literal patterns match a specific value. They do not bind any variables.
Their job is to constrain the scrutinee type: an integer literal pattern
forces the scrutinee to be `int`, a boolean literal forces `bool`, and so on.

The `try_unify` call unifies the scrutinee type with the expected literal type.
If the scrutinee was a type variable, it gets resolved. If it was already a
concrete type that conflicts, a type error is raised.

For example, in `match x with 42 -> ...`, the call `try_unify ty TInt` either
resolves `x`'s type to `int` or fails if `x` was already known to be something
else.

### Tuple Patterns

```ocaml
  | Ast.PatTuple pats ->
    let tys = List.map (fun _ -> Types.new_tvar level) pats in
    try_unify ty (Types.TTuple tys);
    List.concat (List.map2 (check_pattern ctx level) pats tys)
```

A tuple pattern destructures a tuple value. The typechecker:

1. Creates a fresh type variable for each component of the pattern.
2. Unifies the scrutinee type with a tuple of those fresh variables. This
   forces the scrutinee to be a tuple with the right arity.
3. Recursively checks each sub-pattern against its corresponding component
   type, collecting all bindings.

If you write `match pair with (x, y) -> ...` where `pair : int * string`, then
`x` gets type `int` and `y` gets type `string`.

### List Patterns: Cons and Nil

```ocaml
  | Ast.PatCons (hd_pat, tl_pat) ->
    let elem_ty = Types.new_tvar level in
    try_unify ty (Types.TList elem_ty);
    let hd_bindings = check_pattern ctx level hd_pat elem_ty in
    let tl_bindings = check_pattern ctx level tl_pat ty in
    hd_bindings @ tl_bindings
  | Ast.PatNil ->
    let elem_ty = Types.new_tvar level in
    try_unify ty (Types.TList elem_ty);
    []
```

A cons pattern `hd :: tl` destructures a non-empty list. The typechecker
creates a fresh element type variable, unifies the scrutinee with
`TList elem_ty`, then checks the head pattern against `elem_ty` and the tail
pattern against the full list type `ty` (since the tail of a list has the same
type as the list itself).

A nil pattern `[]` forces the scrutinee to be a list but binds nothing.

### Constructor Patterns

```ocaml
  | Ast.PatConstruct (name, arg_pat) ->
    (match List.assoc_opt name ctx.type_env.constructors with
     | None -> error (Printf.sprintf "unknown constructor in pattern: %s" name)
     | Some info ->
       let fresh_args = List.init info.ctor_num_params
         (fun _ -> Types.new_tvar level) in
       let expected_variant = Types.TVariant (info.ctor_type_name, fresh_args) in
       try_unify ty expected_variant;
       (match info.ctor_arg_ty, arg_pat with
        | None, None -> []
        | Some arg_ty, Some p ->
          let concrete_ty = subst_tgens fresh_args arg_ty in
          check_pattern ctx level p concrete_ty
        | None, Some _ ->
          error (Printf.sprintf "constructor %s takes no arguments" name)
        | Some _, None ->
          error (Printf.sprintf "constructor %s requires an argument" name)))
```

Constructor patterns are the most complex case. This is the mechanism by which
pattern matching on variant types works. The next section explains it in
detail.

### Record Patterns

```ocaml
  | Ast.PatRecord field_pats ->
    let field_tys = match Types.repr ty with
      | Types.TRecord field_tys -> field_tys
      | _ ->
        let pat_field_names = List.map fst field_pats in
        let candidates = List.filter (fun (_name, fields) ->
          List.for_all (fun fn -> List.mem_assoc fn fields) pat_field_names
        ) ctx.type_env.Types.records in
        (match candidates with
         | [(_name, fields)] ->
           let fields = instantiate_record_fields level fields in
           let record_ty = Types.TRecord fields in
           try_unify ty record_ty;
           fields
         | [] -> error "record pattern used with non-record type"
         | _ -> error "ambiguous record pattern — annotate the type")
    in
    List.concat (List.map (fun (name, pat) ->
      match List.assoc_opt name field_tys with
      | Some fty -> check_pattern ctx level pat fty
      | None -> error (Printf.sprintf "record type has no field %s" name)
    ) field_pats)
```

Record patterns are explained in detail in the record pattern inference
section below.

### Alias Patterns

```ocaml
  | Ast.PatAs (inner_pat, name) ->
    let bindings = check_pattern ctx level inner_pat ty in
    (name, ty) :: bindings
```

An alias pattern `pat as name` checks the inner pattern against the scrutinee
type, then adds an extra binding for the alias name at the full scrutinee
type. This lets you destructure a value while also keeping a reference to
the whole thing:

```
match xs with
| (hd :: tl) as whole -> ...
```

Here, `hd` gets the element type, `tl` gets the list type, and `whole` also
gets the list type.

### Or-Patterns

```ocaml
  | Ast.PatOr (p1, p2) ->
    let bindings1 = check_pattern ctx level p1 ty in
    let bindings2 = check_pattern ctx level p2 ty in
    let names1 = List.map fst bindings1 |> List.sort String.compare in
    let names2 = List.map fst bindings2 |> List.sort String.compare in
    if names1 <> names2 then
      error "or-pattern branches must bind the same variables";
    List.iter (fun (name, ty1) ->
      match List.assoc_opt name bindings2 with
      | Some ty2 -> try_unify ty1 ty2
      | None -> ()
    ) bindings1;
    bindings1
```

Or-patterns are covered in their own section below.

### Array Patterns

```ocaml
  | Ast.PatArray pats ->
    let elem_ty = Types.new_tvar level in
    try_unify ty (Types.TArray elem_ty);
    List.concat_map (fun p -> check_pattern ctx level p elem_ty) pats
```

Array patterns work like tuple patterns but for arrays. A fresh element type
is created, the scrutinee is unified with `TArray elem_ty`, and each
sub-pattern is checked against the element type. Unlike tuples, all elements
share the same type.

### Map Patterns

```ocaml
  | Ast.PatMap entries ->
    let key_ty = Types.new_tvar level in
    let val_ty = Types.new_tvar level in
    try_unify ty (Types.TMap (key_ty, val_ty));
    List.concat_map (fun (kpat, vpat) ->
      check_pattern ctx level kpat key_ty @
      check_pattern ctx level vpat val_ty
    ) entries
```

Map patterns destructure map literals. Fresh type variables are created for the
key and value types, the scrutinee is unified with `TMap(key_ty, val_ty)`, and
each key-value pair's sub-patterns are checked against the corresponding types.


## Constructor Pattern Type Inference

When you write:

```
match x with
| Some v -> v
| None -> 0
```

How does the typechecker figure out that `x` has type `int option`? This is the
constructor pattern inference mechanism, and it is worth understanding step by
step.

### Constructor Lookup

Every constructor in MiniML is registered in the type environment's
`constructors` table when its parent type is declared. The table maps
constructor names to `ctor_info` records:

```ocaml
(* From lib/types.ml *)
type ctor_info = {
  ctor_type_name: string;                 (* e.g. "option" *)
  ctor_arg_ty: ty option;                 (* e.g. Some (TGen 0) for Some; None for None *)
  ctor_num_params: int;                   (* e.g. 1 for option *)
  ctor_return_ty_params: ty list option;  (* None = ADT, Some = GADT return type params *)
  ctor_existentials: int;                 (* existential type vars in this constructor *)
}
```

For regular ADTs, `ctor_return_ty_params` is `None` and `ctor_existentials`
is `0`. For GADT constructors, `ctor_return_ty_params` contains the type
parameters from the constructor's return type annotation (e.g., for
`IntLit : int -> int expr`, this would be `Some [TInt]`), and
`ctor_existentials` counts the number of type variables that appear in the
constructor's argument but not in its return type.

When the type `type 'a option = None | Some of 'a` is declared, the
environment gets two entries:

- `"None"` maps to `{ ctor_type_name = "option"; ctor_arg_ty = None; ctor_num_params = 1; ctor_return_ty_params = None; ctor_existentials = 0 }`
- `"Some"` maps to `{ ctor_type_name = "option"; ctor_arg_ty = Some (TGen 0); ctor_num_params = 1; ctor_return_ty_params = None; ctor_existentials = 0 }`

The `TGen 0` in `Some`'s argument type is a placeholder for the first type
parameter of `option`.

### Instantiation and Unification

When `check_pattern` encounters `PatConstruct("Some", Some pat)`, it proceeds
as follows:

1. **Look up the constructor.** Find `"Some"` in `ctx.type_env.constructors`.
   This gives the `ctor_info` record.

2. **Create fresh type variables for the parent type's parameters.** The
   `option` type has 1 parameter (`ctor_num_params = 1`), so the typechecker
   creates one fresh variable, say `'a`.

3. **Build the expected variant type.** This is `TVariant("option", ['a])` --
   i.e., `'a option`.

4. **Unify with the scrutinee type.** The call `try_unify ty expected_variant`
   unifies the scrutinee's type with `'a option`. If the scrutinee was
   previously unconstrained, it becomes `'a option`. If it was already known
   to be `int option`, then `'a` gets unified with `int`.

5. **Substitute into the argument type.** The constructor's argument type is
   `TGen 0`. The function `subst_tgens` replaces `TGen 0` with the fresh
   variable `'a`, producing the concrete argument type. Since `'a` may have
   been resolved by step 4, this gives the correct type for the sub-pattern.

6. **Recursively check the sub-pattern.** The pattern `v` inside `Some v` is
   checked against the concrete argument type.

### Worked Example

Consider this code:

```
let describe x =
  match x with
  | Some v -> string_of_int v
  | None -> "nothing"
```

At the point where `check_pattern` processes `Some v`:

- `x` has an unknown type `'b` (fresh variable from the function parameter).
- The constructor lookup for `Some` yields `ctor_type_name = "option"`,
  `ctor_arg_ty = Some (TGen 0)`, `ctor_num_params = 1`.
- A fresh variable `'c` is created.
- The expected variant is `TVariant("option", ['c])`.
- Unifying `'b` with `'c option` makes `'b = 'c option`.
- The argument type `TGen 0` is substituted to `'c`.
- The sub-pattern `v` is a `PatVar`, so it gets type `'c`.

Later, when `string_of_int v` is typechecked, `v : 'c` is unified with `int`
(since `string_of_int` expects `int`), which resolves `'c = int`. Through the
chain of unifications, `x` ends up with type `int option`.

### Constructors Without Arguments

For a constructor like `None` (no argument), the pattern is
`PatConstruct("None", None)`. The typechecker still creates fresh type
parameters and unifies, but there is no sub-pattern to check. The important
effect is that the scrutinee type gets constrained to be the right variant
type.

For non-parameterized types like `type color = Red | Green | Blue`, the fresh
arguments list is empty and the variant type is simply `TVariant("color", [])`.


## GADT Pattern Matching

When the scrutinee of a match expression is a GADT type, the typechecker
switches from the regular ADT path to a GADT-aware algorithm. The core
difference is that each GADT constructor can refine the scrutinee's type
parameters within its arm, producing **local type equations** that hold only
inside that arm. For example, matching on `IntLit : int -> int expr` in a
function typed `'a expr -> 'a` lets the typechecker know that `'a = int`
inside the `IntLit` arm.

### Detection

`synth_match` detects GADT types by inspecting the scrutinee's type after
synthesis:

```ocaml
let gadt_info = match Types.repr scrut_te.ty with
  | Types.TVariant (name, params) ->
    (match find_variant_info ctx.type_env name with
     | Some (_, _, _, true) -> Some (name, params)
     | _ -> None)
  | _ -> None
in
```

If the variant is registered as a GADT (the fourth element of the variant
info tuple is `true`), the GADT path is taken.

### The Snapshot/Restore Algorithm

The central challenge is that GADT type equations are **arm-local**: the
equation `'a = int` established by the `IntLit` arm must not leak into the
`BoolLit` arm (where `'a = bool`). The typechecker solves this with a
snapshot/restore mechanism on the scrutinee's type parameter TVar ref cells.

For each GADT arm matching a constructor C, the algorithm proceeds as
follows:

1. **Create fresh tvars.** Allocate fresh type variables for the
   constructor's universals (`ctor_num_params`) plus existentials
   (`ctor_existentials`).

2. **Substitute into `ctor_return_ty_params`.** Replace `TGen` placeholders
   in the constructor's return type parameters with the fresh tvars. This
   produces the constructor's specific return type parameters.

3. **Build the specific return type.** Construct
   `TVariant(type_name, specific_return_params)`.

4. **Save snapshot.** Record the current state of the scrutinee's type
   parameter TVar ref cells. These ref cells are what unification modifies.

5. **Unify scrutinee type with the specific return type.** This is the step
   that creates the local type equations. For instance, if the scrutinee is
   `'a expr` and the constructor returns `int expr`, unifying them sets
   `'a = int` by mutating the TVar ref cell for `'a`.

6. **Substitute into `ctor_arg_ty` and check the sub-pattern.** With the
   local equations in effect, the constructor's argument type is instantiated
   and the sub-pattern is checked against it.

7. **Check body against expected result type.** The arm's body is checked
   (not synthesized) against `expected_ty`. This is why GADT matches require
   a known result type -- the typechecker cannot synthesize a result type
   that differs per arm.

8. **Freeze the arm's typed AST.** Call `freeze_texpr` on the guard and
   body, which applies `deep_repr` to every `.ty` field in the typed AST
   tree. This resolves all TVar indirections into concrete types.

9. **Restore the snapshot.** Reset the TVar ref cells to their saved state,
   undoing the local type equations.

10. **Check for existential escape.** Verify that no existential type
    variable leaked into the result type.

The relevant code:

```ocaml
let check_gadt_arm (pat, guard, body) =
  let snapshot = save_snapshot () in
  let ctor_name = match pat with
    | Ast.PatConstruct (name, _) -> Some name
    | _ -> None
  in
  let is_gadt_ctor = match ctor_name with
    | Some name ->
      (match List.assoc_opt name ctx.type_env.constructors with
       | Some info -> info.Types.ctor_return_ty_params <> None
       | None -> false)
    | None -> false
  in
  let bindings =
    if is_gadt_ctor then
      check_pattern_gadt ctx level pat scrut_te.ty
    else
      check_pattern ctx level pat scrut_te.ty
  in
  let ctx' = List.fold_left (fun c (n, t) -> extend_var_mono c n t) ctx bindings in
  let guard_te = match guard with
    | Some g -> Some (check ctx' level g Types.TBool)
    | None -> None
  in
  let body_te = check ctx' level body result_ty in
  (* Freeze all types in the arm before restoring *)
  let guard_te = Option.map freeze_texpr guard_te in
  let body_te = freeze_texpr body_te in
  (* Check existential escape *)
  ...
  (* Restore snapshot *)
  restore_snapshot snapshot;
  (pat, guard_te, body_te)
in
```

### GADT-Aware Pattern Checking

Constructor patterns inside a GADT match are handled by
`check_pattern_gadt`, which differs from regular `check_pattern` in how it
instantiates the constructor:

```ocaml
and check_pattern_gadt ctx level pat scrut_ty =
  match pat with
  | Ast.PatConstruct (name, arg_pat) ->
    (match List.assoc_opt name ctx.type_env.constructors with
     | None -> error (Printf.sprintf "unknown constructor in pattern: %s" name)
     | Some info ->
       let num_fresh = info.ctor_num_params + info.ctor_existentials in
       let all_fresh = List.init num_fresh (fun _ -> Types.new_tvar level) in
       let return_ty = match info.ctor_return_ty_params with
         | Some params ->
           Types.TVariant (info.ctor_type_name,
             List.map (subst_tgens all_fresh) params)
         | None ->
           let fresh_universals =
             List.filteri (fun i _ -> i < info.ctor_num_params) all_fresh in
           Types.TVariant (info.ctor_type_name, fresh_universals)
       in
       try_unify scrut_ty return_ty;
       ...)
  | _ -> check_pattern ctx level pat scrut_ty
```

The key differences from regular constructor pattern checking:

- Fresh tvars are allocated for both universals **and** existentials (regular
  ADTs have no existentials).
- The return type is built from `ctor_return_ty_params`, not from bare fresh
  variables. This is what enables the type equations -- if
  `ctor_return_ty_params` is `[TInt]` for a one-parameter GADT, the
  resulting `TVariant("expr", [TInt])` is unified with the scrutinee
  `TVariant("expr", ['a])`, producing `'a = int`.
- Non-constructor patterns (wildcards, variables) fall through to the
  regular `check_pattern`.

### Why `freeze_texpr` Is Needed

The freeze step (step 8) is essential for correctness. Consider what happens
without it:

1. During step 5, unification links the TVar ref cell for `'a` to `TInt` by
   writing `Link TInt` into the ref.
2. The arm's body is typechecked. The typed AST nodes store types that
   contain references to the same TVar ref cell.
3. During step 9, the snapshot restore resets the TVar ref cell back to
   `Unbound('a, ...)`.

Without freezing, the typed AST would now contain TVar references that point
to the restored (unlinked) state. A type that was `int` during typechecking
would appear as `'a` after the restore. The typed AST would contain
**dangling type references** -- types that no longer reflect the equations
that were in effect when the arm was checked.

`freeze_texpr` prevents this by walking the entire typed AST tree and
replacing every `.ty` field with the result of `deep_repr`, which chases
all TVar indirections and builds a fully concrete type. After freezing, the
typed AST contains no TVar references to the scrutinee parameters, so the
subsequent restore does not affect it.

### Existential Escape Check

Existential type variables are type parameters of a GADT constructor that do
not appear in its return type. They represent types that are "hidden" inside
the constructor and cannot be observed from outside. The typechecker must
verify that no existential escapes into the match expression's result type:

```ocaml
and check_existential_escape _info result_ty =
  let rec collect_tvars acc ty =
    match Types.repr ty with
    | Types.TVar { contents = Types.Unbound (id, _) } ->
      if List.mem id acc then acc else id :: acc
    | Types.TArrow (a, _, b) -> collect_tvars (collect_tvars acc a) b
    | Types.TTuple ts -> List.fold_left collect_tvars acc ts
    | ...
    | _ -> acc
  in
  let result_tvars = collect_tvars [] (Types.deep_repr result_ty) in
  ignore result_tvars
```

This collects all unbound type variable IDs in the result type after
`deep_repr`. If any of them correspond to existential tvars from the
constructor, the existential has escaped. Currently the check is a
placeholder -- the structure is in place but does not raise an error, because
`deep_repr` combined with the snapshot restore makes escape effectively
impossible in practice (the existential tvars are fresh and scoped to the
arm).


## Record Pattern Inference

Record patterns face a problem that constructor patterns do not: the pattern
does not name its type. When you write `| Some v -> ...`, the constructor
`Some` tells the typechecker which variant type is involved. But when you
write `| { name; age } -> ...`, there is no type name -- just field names.

The `check_pattern` case for `PatRecord` handles this in two stages.

### Stage 1: Try the Scrutinee Type

If the scrutinee type is already known to be a record (because earlier
unification resolved it), the typechecker uses those field types directly:

```ocaml
let field_tys = match Types.repr ty with
  | Types.TRecord field_tys -> field_tys
  | _ -> (* fall through to inference *)
```

### Stage 2: Infer from Field Names

If the scrutinee type is not yet a known record (for example, it is still an
unbound type variable), the typechecker searches the registered record types
for a match:

```ocaml
let pat_field_names = List.map fst field_pats in
let candidates = List.filter (fun (_name, fields) ->
  List.for_all (fun fn -> List.mem_assoc fn fields) pat_field_names
) ctx.type_env.Types.records in
```

It looks for all registered record types whose fields include every field name
mentioned in the pattern. Three outcomes are possible:

- **Exactly one candidate.** The record type is unambiguous. The typechecker
  instantiates the record's field types (replacing any `TGen` placeholders
  with fresh variables) and unifies the scrutinee type with the resulting
  record type.

- **No candidates.** No registered record type has all the required fields.
  This is an error: `"record pattern used with non-record type"`.

- **Multiple candidates.** The field names are ambiguous -- they match more
  than one registered record type. This is an error:
  `"ambiguous record pattern -- annotate the type"`.

### Checking Fields

Once the field types are known (from either stage), each sub-pattern in the
record pattern is checked against its field's type:

```ocaml
List.concat (List.map (fun (name, pat) ->
  match List.assoc_opt name field_tys with
  | Some fty -> check_pattern ctx level pat fty
  | None -> error (Printf.sprintf "record type has no field %s" name)
) field_pats)
```

Note that record patterns do not require all fields to be mentioned. You can
match on `{ name }` even if the record type also has `age` and `email`
fields. The pattern only constrains the fields it mentions. This is consistent
with the structural record subtyping used elsewhere in the type system.


## Or-Patterns

An or-pattern `p1 | p2` matches a value if either alternative matches. The
typechecker must ensure that both alternatives are consistent.

The key constraint is that **both branches must bind exactly the same set of
variables, with the same types**. This is necessary because the arm's body
will use those variables, and it must work regardless of which alternative
matched.

```ocaml
  | Ast.PatOr (p1, p2) ->
    let bindings1 = check_pattern ctx level p1 ty in
    let bindings2 = check_pattern ctx level p2 ty in
    let names1 = List.map fst bindings1 |> List.sort String.compare in
    let names2 = List.map fst bindings2 |> List.sort String.compare in
    if names1 <> names2 then
      error "or-pattern branches must bind the same variables";
    List.iter (fun (name, ty1) ->
      match List.assoc_opt name bindings2 with
      | Some ty2 -> try_unify ty1 ty2
      | None -> ()
    ) bindings1;
    bindings1
```

The algorithm:

1. Check both `p1` and `p2` against the same scrutinee type `ty`.
2. Collect the variable names from each side and sort them.
3. If the sorted name lists are not equal, report an error. For example,
   `Some x | None` would fail because the left side binds `x` and the right
   side binds nothing.
4. For each variable that appears on both sides, unify its types. This ensures
   consistency -- if the left side infers `x : int` and the right side infers
   `x : string`, that is a type error.
5. Return the bindings from the first branch.

### Valid or-pattern examples

```
(* Both sides bind n with type int *)
match x with
| Some n | Ok n -> n

(* No bindings on either side -- always valid *)
match color with
| Red | Blue -> "cool"
| Green -> "warm"
```

### Invalid or-pattern examples

```
(* Error: left binds x, right binds nothing *)
match opt with
| Some x | None -> ...

(* Error: left binds a, right binds a and b *)
match pair with
| (a, _) | (a, b) -> ...
```


## Exhaustiveness Checking

After all arms of a match expression are typechecked, the exhaustiveness
checker verifies that the patterns cover every possible value of the scrutinee
type. A non-exhaustive match is a potential runtime crash: if a value reaches
the match and no pattern fits, the program has no branch to execute.

### The Top-Level Check

```ocaml
let exhaustiveness_check ctx ty arms =
  (* Filter out guarded arms — guards make coverage uncertain *)
  let unguarded_pats = List.filter_map (fun (pat, guard, _body) ->
    match guard with
    | None -> Some pat
    | Some _ -> None
  ) arms in
  let missing = uncovered_patterns ctx.type_env ty unguarded_pats in
  if missing <> [] then
    error (Printf.sprintf "non-exhaustive match, missing: %s"
             (String.concat ", " missing))
```

Two critical decisions are made here:

1. **Guarded arms are excluded.** An arm with a `when` guard cannot be
   counted as covering its pattern, because the guard might evaluate to
   `false` at runtime. Only unguarded arms contribute to exhaustiveness.

2. **The checker produces specific missing patterns.** Rather than a boolean
   "exhaustive or not", the function `uncovered_patterns` returns a list of
   string representations of values that are not covered. These appear in the
   error message, making it easy to see what is missing.

### Pattern Normalization

Before the main analysis, patterns are normalized to remove structural
wrappers:

```ocaml
let rec normalize_pattern (pat : Ast.pattern) : Ast.pattern list =
  match pat with
  | Ast.PatAs (inner, _) -> normalize_pattern inner
  | Ast.PatOr (p1, p2) -> normalize_pattern p1 @ normalize_pattern p2
  | p -> [p]
```

- `PatAs` is stripped: `(p as name)` is equivalent to `p` for coverage
  purposes, since the alias does not affect what values are matched.
- `PatOr` is flattened: `(p1 | p2)` is equivalent to having both `p1` and
  `p2` as separate patterns.

### Wildcard Detection

```ocaml
let has_wildcard pats =
  List.exists (fun p ->
    match p with
    | Ast.PatWild | Ast.PatVar _ -> true
    | _ -> false
  ) pats
```

If any pattern in the list is a wildcard or a variable (which matches
anything), the patterns are trivially exhaustive for any type.

### The `uncovered_patterns` Function

This is the main recursive analysis. It takes the type environment, the
scrutinee type, and the list of patterns, and returns a list of string
descriptions of uncovered cases:

```ocaml
let rec uncovered_patterns type_env ty pats =
  let pats = List.concat_map normalize_pattern pats in
  if has_wildcard pats then []
  else
    match Types.repr ty with
    ...
```

The function first normalizes and checks for wildcards. If no wildcard is
present, it dispatches on the scrutinee type.

### Variant Exhaustiveness

```ocaml
    | Types.TVariant (name, args) ->
      (match find_variant_info type_env name with
       | None -> []
       | Some (_, _, ctors, is_gadt) ->
         (* For GADTs, filter out constructors whose return type is incompatible *)
         let reachable_ctors = if is_gadt then
           List.filter (fun (ctor_name, _) ->
             match List.assoc_opt ctor_name type_env.Types.constructors with
             | Some info ->
               (match info.Types.ctor_return_ty_params with
                | None -> true
                | Some ret_params ->
                  List.length ret_params = List.length args &&
                  List.for_all2 Types.types_compatible
                    (List.map (fun p -> subst_tgens args p) ret_params)
                    args)
             | None -> true
           ) ctors
         else ctors in
         List.concat_map (fun (ctor_name, ctor_arg_ty) ->
           let sub_pats = specialize_variant pats ctor_name in
           match ctor_arg_ty with
           | None ->
             if List.length sub_pats = 0 then [ctor_name]
             else []
           | Some arg_ty ->
             if List.length sub_pats = 0 then [ctor_name ^ " _"]
             else
               let sub_missing = uncovered_patterns type_env arg_ty sub_pats in
               List.map (fun m -> ctor_name ^ " (" ^ m ^ ")") sub_missing
         ) reachable_ctors)
```

For variant types, the checker iterates over every constructor defined for
that type and asks: "Is this constructor covered by the patterns?" For GADT
types, an additional filtering step removes impossible constructors before
checking coverage (see "GADT Exhaustiveness Filtering" below).

The helper `specialize_variant` extracts the sub-patterns for a specific
constructor:

```ocaml
let rec specialize_variant pats ctor_name =
  List.concat_map (fun pat ->
    match pat with
    | Ast.PatConstruct (name, sub) when String.equal name ctor_name ->
      (match sub with
       | Some p -> normalize_pattern p
       | None -> [Ast.PatWild])
    | Ast.PatConstruct (_, _) -> []
    | Ast.PatWild | Ast.PatVar _ -> [Ast.PatWild]
    | Ast.PatAs (inner, _) ->
      specialize_variant (normalize_pattern inner) ctor_name
    | Ast.PatOr (p1, p2) ->
      specialize_variant (normalize_pattern p1) ctor_name @
      specialize_variant (normalize_pattern p2) ctor_name
    | _ -> []
  ) pats
```

The specialization works as follows:

- If a pattern explicitly matches this constructor, extract its sub-pattern
  (or a wildcard if the constructor takes no argument).
- If a pattern matches a *different* constructor, it contributes nothing for
  this constructor.
- If a pattern is a wildcard or variable, it implicitly covers this
  constructor, so contribute a wildcard sub-pattern.

For a constructor without arguments (like `None`), if `specialize_variant`
returns an empty list, the constructor is uncovered. For a constructor with
an argument (like `Some`), the checker recursively calls `uncovered_patterns`
on the sub-patterns to see if the argument is fully covered.

**Example.** Consider:

```
type 'a option = None | Some of 'a

match x with
| Some true -> ...
| None -> ...
```

The scrutinee type is `bool option`. The checker iterates:

- Constructor `None`: `specialize_variant` finds the `None` pattern, returns
  `[PatWild]`. Non-empty, so `None` is covered.
- Constructor `Some`: `specialize_variant` finds `Some true`, returns
  `[PatBool true]`. Non-empty, so recursion on argument type `bool` with
  patterns `[PatBool true]`. The boolean check (below) finds `true` covered
  but `false` missing. Returns `["false"]`.
  This becomes `["Some (false)"]`.

The error message: `non-exhaustive match, missing: Some (false)`.

### GADT Exhaustiveness Filtering

For GADT types, some constructors are **impossible** for a given scrutinee
type. A constructor is impossible when its return type cannot unify with the
scrutinee type. Before checking coverage, the exhaustiveness checker filters
these constructors out so they do not contribute to the "missing" list.

This filtering uses `types_compatible`, a non-destructive compatibility
check that does not modify any type variables (unlike `unify`, which is
destructive):

```ocaml
let rec types_compatible t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  match t1, t2 with
  | TVar _, _ | _, TVar _ -> true
  | TInt, TInt | TFloat, TFloat | TBool, TBool | TString, TString
  | TByte, TByte | TRune, TRune | TUnit, TUnit -> true
  | TArrow (a1, _, r1), TArrow (a2, _, r2) ->
    types_compatible a1 a2 && types_compatible r1 r2
  | TTuple ts1, TTuple ts2 ->
    List.length ts1 = List.length ts2 && List.for_all2 types_compatible ts1 ts2
  | TList t1, TList t2 -> types_compatible t1 t2
  | TArray t1, TArray t2 -> types_compatible t1 t2
  | TMap (k1, v1), TMap (k2, v2) ->
    types_compatible k1 k2 && types_compatible v1 v2
  | TVariant (a, args_a), TVariant (b, args_b) ->
    String.equal a b && List.length args_a = List.length args_b &&
    List.for_all2 types_compatible args_a args_b
  | _ -> false
```

The function is deliberately conservative: if either side is a type
variable (`TVar`), the types are considered compatible (since the variable
could be instantiated to anything). It only returns `false` when two
concrete types are structurally incompatible.

For each GADT constructor, the checker substitutes the scrutinee's type
parameters into the constructor's `ctor_return_ty_params` and tests
compatibility against the scrutinee's actual parameters. If they are
incompatible, the constructor is filtered out.

**Example.** Consider a GADT for type-safe expressions:

```
type 'a expr =
  | IntLit : int -> int expr
  | BoolLit : bool -> bool expr
  | Add : int expr * int expr -> int expr
```

When matching on a scrutinee of type `int expr`:

- `IntLit` returns `int expr`. Compatible with `int expr` -- kept.
- `BoolLit` returns `bool expr`. Checking `types_compatible TBool TInt`
  returns `false` -- filtered out.
- `Add` returns `int expr`. Compatible -- kept.

The match only needs to cover `IntLit` and `Add`. A `BoolLit` case is not
required and would in fact be dead code.

Without this filtering, every match on a specific GADT type would require
cases for all constructors, defeating the purpose of GADTs. The filtering
lets the exhaustiveness checker understand that not all constructors can
produce every instantiation of the type.

### Boolean Exhaustiveness

```ocaml
    | Types.TBool ->
      let has_true = List.exists (fun p ->
        match p with Ast.PatBool true -> true | _ -> false) pats in
      let has_false = List.exists (fun p ->
        match p with Ast.PatBool false -> true | _ -> false) pats in
      let missing = ref [] in
      if not has_true then missing := "true" :: !missing;
      if not has_false then missing := "false" :: !missing;
      !missing
```

Booleans have exactly two values. The checker looks for `PatBool true` and
`PatBool false` among the patterns. Whichever is missing gets reported.
If both are present (or a wildcard was present, caught earlier), the match
is exhaustive.

### List Exhaustiveness

```ocaml
    | Types.TList _ ->
      let has_nil = List.exists (fun p ->
        match p with Ast.PatNil -> true | _ -> false) pats in
      let has_cons = List.exists (fun p ->
        match p with Ast.PatCons _ -> true | _ -> false) pats in
      let missing = ref [] in
      if not has_nil then missing := "[]" :: !missing;
      if not has_cons then missing := "_ :: _" :: !missing;
      !missing
```

Lists have two structural cases: empty (`[]`) and non-empty (`_ :: _`). The
checker looks for `PatNil` and `PatCons` patterns. This is a shallow check --
it does not recurse into the cons sub-patterns to verify that all possible
list shapes are covered. In practice, this means matching on `[]` and
`hd :: tl` is considered exhaustive even though the `hd` and `tl` sub-patterns
might be refutable.

### Array Exhaustiveness

```ocaml
    | Types.TArray _ ->
      let lengths = List.filter_map (fun p ->
        match p with
        | Ast.PatArray elems -> Some (List.length elems)
        | _ -> None
      ) pats in
      let lengths_set = List.sort_uniq compare lengths in
      let rec find_missing n =
        if List.mem n lengths_set then find_missing (n + 1)
        else n
      in
      let missing_len = find_missing 0 in
      let missing_pat = if missing_len = 0 then "#[]"
        else "#[" ^ String.concat "; " (List.init missing_len (fun _ -> "_"))
             ^ "]"
      in
      [missing_pat]
```

Array patterns match specific lengths. Since arrays can have any length, a
finite set of length-specific patterns can never be exhaustive (unless there
is a wildcard). The checker finds the smallest length not covered and reports
it. Array matches almost always need a wildcard or variable catch-all.

### Other Types

```ocaml
    | Types.TMap _ -> ["_"]
    | Types.TInt | Types.TFloat | Types.TString -> ["_"]
    | _ -> []
```

Maps, integers, floats, and strings have effectively infinite value spaces.
Without a wildcard, they are always non-exhaustive. The checker reports `"_"`
as the missing pattern, indicating that a catch-all is needed.

Tuples, unit, records, and other structural types return `[]` (no missing
patterns) if they reach this point, because they are assumed to be
irrefutable if their sub-patterns are wildcards (and the wildcard check at the
top already handles the case where the top-level pattern is a catch-all).


## Guard Expressions

Match arms can include `when` guards:

```
match x with
| n when n > 0 -> "positive"
| n when n < 0 -> "negative"
| _ -> "zero"
```

Guards are typechecked as boolean expressions in the extended context that
includes the pattern's bindings. From `synth_match`:

```ocaml
let guard_te = match guard with
  | Some g ->
    let gte = check ctx' level g Types.TBool in
    Some gte
  | None -> None
in
```

The guard expression is checked (not synthesized) against `TBool`, ensuring it
is a well-typed boolean expression.

### Guards and Exhaustiveness

The critical interaction between guards and exhaustiveness is that **guarded
arms do not count toward coverage**. The exhaustiveness checker filters them
out:

```ocaml
let unguarded_pats = List.filter_map (fun (pat, guard, _body) ->
  match guard with
  | None -> Some pat
  | Some _ -> None
) arms
```

This is sound because a guard can always evaluate to `false`, leaving the arm
unmatched. Consider:

```
match b with
| true when some_condition () -> ...
| false -> ...
```

Even though the patterns `true` and `false` appear, the first arm is guarded.
If `some_condition()` returns `false`, then `true` values fall through. The
checker correctly reports this as non-exhaustive (missing `true`), because only
the unguarded `false` arm is considered.

The practical consequence: if you use guards, you typically need a catch-all
arm at the end to satisfy exhaustiveness.


## The Partial Match Flag

The `EMatch` node in the AST carries a boolean flag:

```ocaml
| EMatch of expr * (pattern * expr option * expr) list * bool
```

When this flag is `true`, the match is "partial" and the exhaustiveness check
is skipped:

```ocaml
if not partial then
  !exhaustiveness_check_ref ctx (Types.repr scrut_te.ty) all_arms;
```

This flag is used internally by derived code (such as auto-generated `Show`
and `Eq` instances) where the compiler constructs match expressions that are
known to be correct by construction, or where partial matching is intentional.
User-written match expressions always have this flag set to `false`.


## Summary

Pattern matching typechecking has two phases:

1. **Pattern checking.** The `check_pattern` function walks each pattern form,
   unifying the scrutinee type with the type implied by the pattern, and
   collecting variable bindings. Constructor patterns trigger a lookup in the
   type environment to determine the parent variant type and instantiate its
   type parameters. Record patterns use field names to infer which record type
   is involved when the scrutinee type is not yet known. For GADT types,
   `check_pattern_gadt` handles constructor patterns specially, creating fresh
   tvars for both universals and existentials, and building the return type
   from `ctor_return_ty_params` to establish local type equations via
   unification.

2. **GADT snapshot/restore.** When the scrutinee is a GADT type,
   `synth_match` uses a snapshot/restore mechanism to scope type equations to
   individual arms. Each arm's typed AST is frozen with `freeze_texpr` (which
   applies `deep_repr` to all type fields) before restoring the snapshot, so
   the typed AST retains the arm-local type information even after the
   snapshot is undone.

3. **Exhaustiveness analysis.** After all arms are typechecked, the
   `uncovered_patterns` function determines whether the patterns cover all
   possible values. It normalizes patterns (stripping aliases, flattening
   or-patterns), filters out guarded arms, and recursively analyzes coverage
   by type: variant types are checked constructor-by-constructor, booleans
   need `true` and `false`, lists need `[]` and `_ :: _`, and infinite types
   like `int` always require a wildcard. For GADT types, constructors whose
   return type is incompatible with the scrutinee type are filtered out using
   `types_compatible`, so only reachable constructors need coverage.

The design is pragmatic rather than theoretically complete. The exhaustiveness
checker does not perform deep analysis of nested patterns in lists, and it
does not track relationships between guards. These are deliberate trade-offs
that keep the implementation simple while catching the most common sources of
non-exhaustive matches.
