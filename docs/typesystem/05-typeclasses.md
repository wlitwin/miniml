# Part 5: Type Classes and Dictionary Passing

Type classes let you define interfaces that types can implement. You write
`class Show a` to declare that any type `a` can have a `show` method, then
`instance Show int` to provide the implementation for `int`. When you call
`show 42`, the typechecker figures out which implementation to use.

The key insight is that MiniML compiles type classes away entirely. By the time
code reaches the compiler, every method call has been replaced by a field access
on a concrete record value -- the "dictionary." There is no runtime dispatch,
no vtable lookup, no interface pointer. The typechecker performs two passes: the
first infers types and collects constraints, the second rewrites the typed AST
to thread dictionary records through as ordinary function parameters. The
compiler never sees type classes at all.

This chapter covers every step of that process: how classes and instances are
registered, how method calls create constraints, how `where` clauses introduce
dictionary parameters, how the constraint transformation pass rewrites the
typed AST, and how dictionaries are resolved at call sites.

All code lives in `lib/types.ml` and `lib/typechecker.ml` unless noted
otherwise.


## Class Definitions

A class declaration in MiniML looks like this:

```
class Show 'a =
  show : 'a -> string
end
```

This says: "any type `'a` that is an instance of `Show` must provide a function
`show` that takes an `'a` and returns a `string`." The parser turns this into:

```ocaml
(* From lib/ast.ml *)
| DClass of string * string list * (string list * string list) list * (string * ty_annot) list
  (* class_name, tyvar_names, fundeps as [(from_tyvars, to_tyvars)], [(method_name, method_type)] *)
```

So `class Show 'a = show : 'a -> string end` becomes
`DClass ("Show", ["a"], [], [("show", TyArrow(TyVar "a", TyName "string"))])`.

The third field is a list of functional dependencies. For classes without
functional dependencies (like `Show`), this is the empty list. For a class like
`class Index 'c 'k 'v where 'c -> 'k 'v`, it would be `[(["c"], ["k"; "v"])]`
-- meaning the container type determines the key and value types.

### How `process_class_def` stores a class

When the typechecker encounters a `DClass` declaration, it calls
`process_class_def`. This function does three things:

1. **Resolve method types.** Each type variable name in the class declaration
   (like `'a`) is mapped to a `TGen` index. The first class parameter becomes
   `TGen 0`, the second `TGen 1`, and so on. Method type annotations are
   resolved against this mapping, turning `'a -> string` into
   `TArrow(TGen 0, TString)`.

2. **Build the `class_def` record** and add it to the type environment.

3. **Add methods to the variable environment** as polymorphic schemes so they
   can be called like ordinary functions.

Here is the core of the function (from `lib/typechecker.ml`):

```ocaml
let process_class_def ctx (class_name : string) (tyvar_names : string list)
    (fundep_annots : (string list * string list) list)
    (methods : (string * Ast.ty_annot) list) : ctx * tdecl =
  if List.exists (fun c -> String.equal c.Types.class_name class_name)
       ctx.type_env.Types.classes then
    error (Printf.sprintf "duplicate class definition: %s" class_name);
  let num_class_params = List.length tyvar_names in
  let method_tys = List.map (fun (mname, annot) ->
    let tvars = Hashtbl.create 4 in
    List.iteri (fun i name ->
      Hashtbl.replace tvars name (Types.TGen i)
    ) tyvar_names;
    let next_gen = ref num_class_params in
    let rec resolve = function
      | Ast.TyVar name ->
        (match Hashtbl.find_opt tvars name with
         | Some tv -> tv
         | None ->
           (* Extra method-level type variable *)
           let idx = !next_gen in
           next_gen := idx + 1;
           Hashtbl.replace tvars name (Types.TGen idx);
           Types.TGen idx)
      | Ast.TyName "int" -> Types.TInt
      (* ... other base types ... *)
      | Ast.TyArrow (a, b) -> Types.TArrow (resolve a, Types.EffEmpty, resolve b)
      (* ... other type constructors ... *)
    in
    let ty = resolve annot in
    let method_quant = !next_gen in
    (mname, ty, method_quant)
  ) methods in
```

Notice that type variables beyond the class parameters are supported. If a
method like `fold` in class `Iter` uses an extra type variable `'c` that is
not a class parameter, it gets its own `TGen` index starting after the class
parameters. The `method_quant` tracks the total number of quantified variables
for that method's scheme.

The function then converts string-based fundeps to index-based, builds the
class definition record, and registers it:

```ocaml
  (* Convert string-based fundeps to index-based *)
  let class_fundeps = List.map (fun (from_names, to_names) ->
    let resolve_idx name =
      match List.find_index (fun n -> String.equal n name) tyvar_names with
      | Some (i, _) -> i
      | None -> error (Printf.sprintf "fundep refers to unknown type variable '%s" name)
    in
    Types.{ fd_from = List.map resolve_idx from_names;
            fd_to = List.map resolve_idx to_names }
  ) fundep_annots in
  let class_def = Types.{
    class_name;
    class_params = tyvar_names;
    class_methods = List.map (fun (m, t, _, _) -> (m, t)) method_tys;
    class_fundeps;
  } in
```

The `class_def` type (from `lib/types.ml`):

```ocaml
type fundep = {
  fd_from: int list;   (* indices of determining params *)
  fd_to: int list;     (* indices of determined params *)
}

type class_def = {
  class_name: string;
  class_params: string list;
  class_methods: (string * ty) list;  (* types use TGen 0..N-1 for class params *)
  class_fundeps: fundep list;
}
```

The `class_fundeps` field stores functional dependencies as index-based
references into `class_params`. For `class Index 'c 'k 'v where 'c -> 'k 'v`,
the fundep is `{ fd_from = [0]; fd_to = [1; 2] }` because `'c` is parameter 0,
`'k` is parameter 1, and `'v` is parameter 2.

Methods use `TGen` indices for class parameters, so `Show`'s `show` method is
stored as `TArrow(TGen 0, TString)` -- meaning "takes whatever the first class
parameter is instantiated to, returns string."

Finally, each method is added to the variable context as a polymorphic scheme:

```ocaml
  let ctx = List.fold_left (fun ctx (mname, mty, mquant) ->
    let scheme = { Types.quant = mquant; constraints = []; body = mty } in
    extend_var ctx mname scheme
  ) ctx method_tys in
```

For `Show`, this adds `show` to the context with scheme
`{ quant = 1; constraints = []; body = TArrow(TGen 0, TString) }`. This means
you can call `show` on any type -- the constraint resolution happens later.

### The dictionary record type

Each instance of a class produces a record whose fields are the method
implementations. For `Show`, the dictionary type is
`{ show: <concrete_type> -> string }`. For `Num`, it is
`{ +: int -> int -> int; -: int -> int -> int; ... }` (for the `int` instance).
The dictionary is a plain MiniML record value -- there is nothing special about
it at the value level.


## Instance Definitions

An instance declaration provides method implementations for a specific type:

```
instance Show int =
  let show x = string_of_int x
end
```

The parser produces:

```ocaml
(* From lib/ast.ml *)
| DInstance of string * ty_annot list * constraint_ list * (string * param list * expr) list
  (* class_name, instance_types, constraints, [(method_name, params, body)] *)
```

### How `process_instance_def` registers an instance

The function `process_instance_def` handles instance declarations. It performs
these steps:

**1. Look up the class definition.** If the class does not exist, it is a type
error:

```ocaml
let process_instance_def ctx level (class_name : string)
    (inst_ty_annots : Ast.ty_annot list)
    (constraints : Ast.constraint_ list)
    (methods : (string * Ast.param list * Ast.expr) list) : ctx * tdecl =
  let class_def = match List.find_opt
    (fun c -> String.equal c.Types.class_name class_name) ctx.type_env.Types.classes with
    | Some c -> c
    | None -> error (Printf.sprintf "unknown class: %s" class_name)
  in
```

**2. Resolve instance types.** The instance type annotations are resolved into
`ty` values using `resolve_inst_tys`. Type variables in the instance become
`TGen` indices for storage. For `instance Show int`, the stored type is simply
`[TInt]`. For `instance Show ('a list)`, it is `[TList (TGen 0)]` -- the `'a`
becomes `TGen 0`.

```ocaml
  let (stored_tys, num_inst_vars, inst_tvars) = resolve_inst_tys ctx inst_ty_annots in
```

The return value `stored_tys` uses `TGen` for type variables (for persistent
storage in the type environment), while `num_inst_vars` counts how many type
variables exist and `inst_tvars` maps their names to indices.

**3. Generate the dictionary name.** Each instance gets a unique global name
for its dictionary record:

```ocaml
  let dname = Types.dict_name class_name stored_tys in
```

The `dict_name` function (from `lib/types.ml`) produces names like
`__dict_Show_int`, `__dict_Show_g0_list`, or `__dict_Eq_string`:

```ocaml
let dict_name class_name tys =
  let ty_strs = List.map ty_to_str tys in
  Printf.sprintf "__dict_%s_%s" class_name (String.concat "__" ty_strs)
```

**4. Check for duplicate instances.** The typechecker rejects overlapping
instances:

```ocaml
  if List.exists (fun i ->
    String.equal i.Types.inst_class class_name &&
    List.length i.Types.inst_tys = List.length stored_tys &&
    Types.match_partial_inst i.Types.inst_tys (List.map (fun t -> Some t) stored_tys)
  ) ctx.type_env.Types.instances then
    error (Printf.sprintf "duplicate instance %s for types %s" ...);
```

**5. Check all required methods are provided:**

```ocaml
  List.iter (fun (mname, _) ->
    if not (List.exists (fun (n, _, _) -> String.equal n mname) methods) then
      error (Printf.sprintf "instance %s %s missing method: %s"
        class_name (String.concat " " (List.map Types.pp_ty stored_tys)) mname)
  ) class_def.class_methods;
```

**6. Typecheck each method body.** For each method, the typechecker takes the
class method's schema type (e.g., `TGen 0 -> TString` for `show`), substitutes
the instance's concrete types for the `TGen` indices, and checks the method
body against the resulting concrete type:

```ocaml
  let typed_methods = List.map (fun (mname, params, body) ->
    let method_schema_ty = match List.assoc_opt mname class_def.class_methods with
      | Some ty -> ty
      | None -> error (Printf.sprintf "%s is not a method of class %s" mname class_name)
    in
    let concrete_method_ty = subst_tgens check_tys method_schema_ty in
    let full_body = wrap_params_decl params None body in
    let te = check ctx (level + 1) full_body concrete_method_ty in
    (mname, te)
  ) methods in
```

For `instance Show int`, the schema type `TGen 0 -> TString` becomes
`TInt -> TString`, and the body `fun x -> string_of_int x` is checked against
that type.

**7. Build the dictionary record.** The typed method bodies are assembled into
a record expression:

```ocaml
  let sorted_methods = List.sort (fun (a, _) (b, _) -> String.compare a b) typed_methods in
  let record_ty = Types.TRecord (List.map (fun (n, te) -> (n, te.ty)) sorted_methods) in
  let dict_expr = mk (TERecord sorted_methods) record_ty in
```

Methods are sorted by name to ensure consistent field order.

**8. Register the instance.** An `instance_def` record is created and added to
the type environment:

```ocaml
  let inst_def = Types.{
    inst_class = class_name;
    inst_tys = stored_tys;
    inst_dict_name = dname;
    inst_constraints;
  } in
  let type_env = { ctx.type_env with
    Types.instances = inst_def :: ctx.type_env.instances;
  } in
```

The `instance_def` type (from `lib/types.ml`):

```ocaml
type instance_def = {
  inst_class: string;
  inst_tys: ty list;
  inst_dict_name: string;
  inst_constraints: class_constraint list;
}
```

The `inst_tys` field stores the types this instance covers, using `TGen` for
any type variables. The `inst_dict_name` is the name of the global variable
holding the dictionary record. The `inst_constraints` field lists any
constraints on the instance's type variables (covered in the factory
dictionaries section).

**9. Emit a `TDLet` for the dictionary.** The instance declaration is
transformed into an ordinary `let` binding:

```ocaml
  (ctx, TDLet (dname, dict_expr))
```

So `instance Show int` becomes, in the typed AST, something equivalent to:

```
let __dict_Show_int = { show = fun x -> string_of_int x }
```

This is the essence of dictionary passing -- instances are just record values.


## How Methods Are Typed

When you write `show 42`, the typechecker does not do anything special during
the first pass. It treats `show` as an ordinary polymorphic function. The
`synth` function handles `EVar "show"` the same way it handles any variable
lookup:

```ocaml
(* In synth, lib/typechecker.ml *)
| Ast.EVar name ->
  let ty = lookup_var ctx level name in
  mk (TEVar name) ty
```

The `lookup_var` function finds `show` in the context and calls
`Types.instantiate` on its scheme
`{ quant = 1; body = TArrow(TGen 0, TString) }`, producing a fresh type like
`TArrow('a, TString)` where `'a` is a new unbound type variable. Then the
application `show 42` unifies `'a` with `TInt`, giving the call expression
type `TString`.

At this point, the typed AST just says `TEVar "show"` with type
`int -> string`. No constraint has been recorded on the expression itself --
the type variable `'a` was simply unified with `TInt`.

The magic happens in the second pass (constraint transformation), which
detects that `show` is a class method and rewrites the call. But for functions
with `where` clauses, there is an important first-pass mechanism: when a type
variable is constrained but not yet resolved to a concrete type, the
typechecker needs to avoid defaulting it prematurely.


## Where Clauses and Constrained Functions

A `where` clause declares that a function's type variables satisfy certain
class constraints:

```
let f (x: 'a) = show x where Show 'a
```

This says: "`f` takes an `'a` and calls `show` on it, and `'a` must be an
instance of `Show`." The parser produces the constraints as a list of
`(class_name, tyvar_names)` pairs attached to the `DLet` declaration.

### `synth_constrained_fn`

Functions with constraints are typechecked by `synth_constrained_fn` instead of
the normal `synth` path. The key difference is that it uses a **shared type
variable table** to ensure all annotations referring to `'a` share the same
type variable, and it records which type variables are constrained so other
parts of the typechecker know not to default them.

```ocaml
let synth_constrained_fn ctx level constraints params ret_annot body =
  let shared_tvars = Hashtbl.create 4 in
  (* Pre-allocate tvars for constraint type variables *)
  let constraint_tyvar_names =
    List.flatten (List.map (fun (_, names) -> names) constraints)
    |> List.sort_uniq String.compare in
  List.iter (fun tvname ->
    Hashtbl.replace shared_tvars tvname (Types.new_tvar (level + 1))
  ) constraint_tyvar_names;
```

For `let f (x: 'a) = show x where Show 'a`, this creates a single fresh type
variable for `'a` and stores it in `shared_tvars`.

Next, it collects the IDs of these constraint type variables:

```ocaml
  let constraint_tvar_ids = List.filter_map (fun tvname ->
    match Types.repr (Hashtbl.find shared_tvars tvname) with
    | Types.TVar { contents = Types.Unbound (id, _) } -> Some id
    | _ -> None
  ) constraint_tyvar_names in
```

These IDs are added to `ctx.constraint_tvars`. This list is checked by
`synth_binop` and `synth_unop` -- when an arithmetic operator like `+` has
an operand whose type is a constrained type variable, the typechecker does not
default it to `int`:

```ocaml
(* In synth_binop, for Add/Sub/Mul/Div *)
| Types.TVar { contents = Types.Unbound (id, _) } ->
  if List.mem id ctx.constraint_tvars then
    mk (TEBinop (op, te1, te2)) (Types.repr te1.ty)
  else begin
    try_unify te1.ty Types.TInt;
    mk (TEBinop (op, te1, te2)) Types.TInt
  end
```

Without the `constraint_tvars` check, writing `let add (x: 'a) (y: 'a) = x + y
where Num 'a` would fail because `+` would try to unify `'a` with `int`.

The function then resolves parameter types using the shared table, typechecks
the body, builds the `TEFun` chain, and calls `resolve_let_constraints` to
produce the final scheme:

```ocaml
  let inner_ctx = List.fold_left2 (fun c p ty ->
    extend_var_mono c p.Ast.name ty
  ) { ctx with constraint_tvars = constraint_tvar_ids @ ctx.constraint_tvars } params param_tys in
  let body_te = match ret_ty_opt with
    | Some ret_ty -> check inner_ctx (level + 1) body ret_ty
    | None -> synth inner_ctx (level + 1) body
  in
  let te = List.fold_right2 (fun p pty acc ->
    mk (TEFun (p.Ast.name, acc)) (Types.TArrow (pty, Types.EffEmpty, acc.ty))
  ) params param_tys body_te in
  let scheme = resolve_let_constraints ~type_env:ctx.type_env level constraints params ret_annot te.ty in
  (te, scheme)
```

### `resolve_let_constraints`

This function takes the AST constraints (class name + type variable names),
maps each type variable name to its generalized `TGen` index, and produces a
scheme with `class_constraint` records attached:

```ocaml
type class_constraint = {
  cc_class: string;     (* class name, e.g. "Show" *)
  cc_args: int list;    (* TGen indices this constraint applies to *)
}
```

For `let f (x: 'a) = show x where Show 'a`, the resulting scheme is:

```
{ quant = 1;
  constraints = [{ cc_class = "Show"; cc_args = [0] }];
  body = TArrow(TGen 0, TString) }
```

This scheme says: "`f` is polymorphic over one type variable (index 0), that
variable must have a `Show` instance, and `f` takes that type and returns
`string`."


## The Constraint Transformation Pass

This is the heart of dictionary passing. After the first pass has produced a
typed AST where every expression carries its inferred type and every constrained
function has a scheme with `class_constraint` records, the second pass rewrites
the AST to make dictionaries explicit. The entry point is
`transform_constraints`:

```ocaml
let transform_constraints ctx (tprog : tprogram) : tprogram =
  let xctx = {
    xf_type_env = ctx.type_env;
    xf_schemes = ctx.vars;
    xf_constrained = [];
  } in
  List.map (fun tdecl -> match tdecl with
    | TDLet (name, te) ->
      (match List.assoc_opt name xctx.xf_schemes with
       | Some scheme when scheme.Types.constraints <> [] ->
         TDLet (name, xform_constrained_def xctx scheme te)
       | _ ->
         let inst = List.find_opt (fun (i : Types.instance_def) ->
           String.equal i.inst_dict_name name && i.inst_constraints <> []
         ) xctx.xf_type_env.Types.instances in
         (match inst with
          | Some inst_def ->
            TDLet (name, xform_constrained_inst xctx inst_def te)
          | None ->
            TDLet (name, xform_expr xctx te)))
    | TDLetRec (name, te) ->
      (match List.assoc_opt name xctx.xf_schemes with
       | Some scheme when scheme.Types.constraints <> [] ->
         TDLetRec (name, xform_constrained_def xctx scheme te)
       | _ -> TDLetRec (name, xform_expr xctx te))
    | TDExpr te -> TDExpr (xform_expr xctx te)
    (* ... modules handled similarly ... *)
    | other -> other
  ) tprog
```

For each top-level declaration, `transform_constraints` decides what kind of
rewriting is needed:

- **Constrained function definitions** (a `TDLet`/`TDLetRec` whose scheme has
  constraints): call `xform_constrained_def` to add dictionary parameters.
- **Constrained instance dictionaries** (a `TDLet` whose name matches an
  `instance_def` with constraints): call `xform_constrained_inst` to build a
  factory function.
- **Everything else**: walk with `xform_expr` to rewrite method calls and
  constrained references.

### The `xform_ctx` record

The transformation pass threads an `xform_ctx` (written `xctx` in the code)
through every rewriting function:

```ocaml
type xform_ctx = {
  xf_type_env: Types.type_env;
  xf_schemes: (string * Types.scheme) list;
  xf_constrained: (int * string * string) list;
    (* tvar_id, dict_param_name, class_name *)
}
```

- **`xf_type_env`** gives access to the full type environment -- class
  definitions, instance definitions, and all registered types. This is needed
  to look up instances and class methods.

- **`xf_schemes`** maps variable names to their type schemes. When the
  transformation encounters a reference to a constrained function, it looks up
  the scheme here to determine what dictionary arguments to insert.

- **`xf_constrained`** tracks which type variable IDs have dictionary
  parameters in scope. Inside a constrained function like
  `let f (x: 'a) = show x where Show 'a`, the transformation adds an entry
  `(tvar_id_of_a, "__dict_Show_0", "Show")` to this list. When the inner
  `show x` is rewritten, the transformation sees that the type of `x` is a
  tvar whose ID appears in `xf_constrained` and generates a field access on
  the dictionary parameter instead of looking up a concrete instance.

### `xform_constrained_def`: adding dictionary parameters

When a function has constraints, `xform_constrained_def` wraps it with extra
`TEFun` nodes for dictionary parameters:

```ocaml
let xform_constrained_def xctx scheme te =
  let tgen_map = build_tgen_map scheme.Types.body te.ty in
  let dict_params = List.concat_map (fun (cc : Types.class_constraint) ->
    List.filter_map (fun tgen_idx ->
      let dparam = Printf.sprintf "__dict_%s_%d" cc.cc_class tgen_idx in
      match Hashtbl.find_opt tgen_map tgen_idx with
      | Some (Types.TVar { contents = Types.Unbound (id, _) }) ->
        Some (id, dparam, cc.cc_class)
      | _ -> None
    ) cc.cc_args
  ) scheme.Types.constraints in
  let xctx = { xctx with xf_constrained = dict_params @ xctx.xf_constrained } in
  let rewritten = xform_expr xctx te in
  List.fold_right (fun (_tid, dparam, _cls) body ->
    mk (TEFun (dparam, body)) (Types.TArrow (Types.TUnit, Types.EffEmpty, body.ty))
  ) dict_params rewritten
```

This function:

1. Calls `build_tgen_map` to figure out which `TGen` index maps to which
   actual type (which at this point is still an unbound type variable, since
   the function is polymorphic).

2. For each constraint, generates a dictionary parameter name like
   `__dict_Show_0` (the class name plus the TGen index).

3. Records each `(tvar_id, param_name, class_name)` triple in `xf_constrained`
   so that inner expressions can resolve method calls.

4. Rewrites the function body with `xform_expr` (which can now see the
   dictionary parameters via `xf_constrained`).

5. Wraps the rewritten body with `TEFun` nodes for each dictionary parameter.

So `let f (x: 'a) = show x where Show 'a` becomes, conceptually:

```
let f __dict_Show_0 x = __dict_Show_0.show x
```

### `xform_expr`: the expression rewriter

The `xform_expr` function recursively walks the typed AST. For most node types,
it simply recurses into children. The interesting cases are `TEVar` (variable
references) and `TEBinop`/`TEUnop` (operators that might need class dispatch):

```ocaml
let rec xform_expr xctx te =
  match te.expr with
  | TEVar name ->
    let te = xform_class_method xctx name te in
    xform_constrained_ref xctx name te
  | TEBinop (op, e1, e2) ->
    let e1' = xform_expr xctx e1 in
    let e2' = xform_expr xctx e2 in
    xform_binop xctx op e1' e2' te.ty
  | TEUnop (op, e) ->
    let e' = xform_expr xctx e in
    xform_unop xctx op e' te.ty
  | TEApp (fn, arg) ->
    let fn' = xform_expr xctx fn in
    let arg' = xform_expr xctx arg in
    mk (TEApp (fn', arg')) te.ty
  (* ... all other cases recurse structurally ... *)
```

Every `TEVar` goes through two rewrites:

1. **`xform_class_method`**: checks if the variable is a class method and, if
   so, rewrites it to a dictionary field access.
2. **`xform_constrained_ref`**: checks if the variable references a constrained
   function and, if so, inserts dictionary arguments at the call site.

### `xform_class_method`: rewriting method calls

This is where `show x` becomes `__dict_Show_0.show x` (inside a constrained
body) or `__dict_Show_int.show x` (at a concrete call site). The function:

1. Checks whether the variable name is a class method by searching
   `xf_type_env.classes`:

```ocaml
and xform_class_method xctx name te =
  let (class_opt, method_name) =
    match Types.find_method_class xctx.xf_type_env.Types.classes name with
    | Some _ as result -> (result, name)
    | None -> (* try stripping module prefix *) ...
  in
```

The `find_method_class` function (from `lib/types.ml`) searches all registered
classes:

```ocaml
let find_method_class classes method_name =
  List.find_opt (fun cls ->
    List.exists (fun (m, _) -> String.equal m method_name) cls.class_methods
  ) classes
```

2. If it is a method, walks the class method's schema type alongside the
   actual instantiated type to find what types the class parameters were
   instantiated with:

```ocaml
  | Some class_def ->
    let method_schema_ty = List.assoc method_name class_def.Types.class_methods in
    let num_params = List.length class_def.Types.class_params in
    let found = Hashtbl.create num_params in
    let rec go s r =
      let r = Types.repr r in
      match s, r with
      | Types.TGen i, _ when i < num_params ->
        if not (Hashtbl.mem found i) then Hashtbl.replace found i r
      | Types.TArrow (s1, _, s2), Types.TArrow (r1, _, r2) -> go s1 r1; go s2 r2
      (* ... other structural cases ... *)
    in
    go method_schema_ty te.ty;
```

For `show` with schema type `TGen 0 -> TString` and actual type
`int -> string`, this maps `TGen 0 -> TInt`. For `show` with actual type
`'a -> string` (inside a constrained body), it maps `TGen 0` to the unbound
tvar for `'a`.

3. **Inside a constrained body** (when `xf_constrained` is non-empty): checks
   if the instantiated class parameter is a constrained type variable:

```ocaml
    let constrained_match = if xctx.xf_constrained <> [] then
      let exact_match = List.find_map (fun i ->
        match Hashtbl.find_opt found i with
        | Some (Types.TVar { contents = Types.Unbound (id, _) }) ->
          find_constrained_dict xctx id class_def.Types.class_name
        | _ -> None
      ) (List.init num_params Fun.id) in
      (* ... fallback for generalized tvars ... *)
    else None in
```

If the type argument is an unbound tvar whose ID appears in `xf_constrained`
for the right class, it returns the dictionary parameter name. The method call
is then rewritten to a field access:

```ocaml
    (match constrained_match with
     | Some dp ->
       mk (TEField (mk (TEVar dp) Types.TUnit, method_name)) te.ty
```

So `show x` (where `x : 'a` and `Show 'a` is constrained) becomes
`__dict_Show_0.show x` -- a field access on the dictionary parameter.

4. **At a concrete call site** (no matching constraint): looks up the instance
   for the concrete type:

```ocaml
     | None ->
       let partial_args = List.map (...) type_args in
       let matching = List.filter (fun (inst : Types.instance_def) ->
         String.equal inst.inst_class class_def.Types.class_name &&
         Types.match_partial_inst inst.inst_tys partial_args
       ) xctx.xf_type_env.Types.instances in
       match resolved with
       | Some inst when inst.inst_constraints = [] ->
         mk (TEField (mk (TEVar inst.inst_dict_name) Types.TUnit, method_name)) te.ty
```

So `show 42` becomes `__dict_Show_int.show 42`.

If the matched instance has constraints (a factory instance), the factory is
applied with the appropriate sub-dictionaries. This is covered in the factory
dictionaries section.

### `xform_constrained_ref`: inserting dictionary arguments at call sites

When you call a constrained function, the call site needs to pass dictionary
arguments. The function `xform_constrained_ref` handles this:

```ocaml
and xform_constrained_ref xctx name te =
  match List.assoc_opt name xctx.xf_schemes with
  | Some scheme when scheme.Types.constraints <> [] ->
    let tgen_map = build_tgen_map scheme.Types.body te.ty in
    let dict_args = List.map (fun cc ->
      resolve_dict_arg xctx cc tgen_map
    ) scheme.Types.constraints in
    List.fold_left (fun fn dict_arg ->
      mk (TEApp (fn, dict_arg)) Types.TUnit
    ) te dict_args
  | _ -> te
```

If the variable references a constrained function, this function:

1. Looks up the scheme for the function.
2. Builds a `TGen -> actual type` mapping by walking the schema type alongside
   the instantiated type.
3. Resolves each constraint to a dictionary argument using `resolve_dict_arg`.
4. Wraps the function reference in `TEApp` nodes for each dictionary argument.

So calling `f 42` where `f : 'a -> string where Show 'a` becomes
`(f __dict_Show_int) 42` -- the dictionary for `Show int` is passed as the
first argument.

### `xform_binop` and `xform_unop`: operator dispatch

Arithmetic and comparison operators are handled by built-in classes (`Num`,
`Eq`, `Ord`, `Bitwise`). Inside a constrained function body, operators on
constrained type variables are rewritten to method calls:

```ocaml
and xform_binop xctx op e1 e2 result_ty =
  let class_info = match op with
    | Ast.Add -> Some ("Num", "+") | Ast.Sub -> Some ("Num", "-")
    | Ast.Mul -> Some ("Num", "*") | Ast.Div -> Some ("Num", "/")
    | Ast.Eq -> Some ("Eq", "=") | Ast.Neq -> Some ("Eq", "<>")
    | Ast.Lt -> Some ("Ord", "<") | Ast.Gt -> Some ("Ord", ">")
    | Ast.Le -> Some ("Ord", "<=") | Ast.Ge -> Some ("Ord", ">=")
    | Ast.Land -> Some ("Bitwise", "land") | Ast.Lor -> Some ("Bitwise", "lor")
    (* ... *)
    | _ -> None
  in
  match class_info with
  | Some (class_name, method_name) when xctx.xf_constrained <> [] ->
    (match Types.repr e1.ty with
     | Types.TVar { contents = Types.Unbound (id, _) } ->
       (match find_constrained_dict xctx id class_name with
        | Some dparam ->
          let dict_ref = mk (TEVar dparam) Types.TUnit in
          let method_ref = mk (TEField (dict_ref, method_name)) Types.TUnit in
          let app1 = mk (TEApp (method_ref, e1)) Types.TUnit in
          mk (TEApp (app1, e2)) result_ty
        | None -> mk (TEBinop (op, e1, e2)) result_ty)
     | _ -> mk (TEBinop (op, e1, e2)) result_ty)
  | _ -> mk (TEBinop (op, e1, e2)) result_ty
```

So `x + y` where `x : 'a` and `Num 'a` is constrained becomes
`__dict_Num_0.(+) x y`. When the operands have concrete types (like `int`),
the operator is left as a `TEBinop` and handled directly by the compiler.


## Dictionary Resolution

When the transformation pass needs to find a dictionary at a call site, it
calls `resolve_dict_arg`. This function handles two fundamentally different
cases:

### Case 1: Constrained type variable

If the type argument is an unbound type variable that appears in
`xf_constrained`, the dictionary is the corresponding parameter -- it is
passed through from the enclosing function's parameter list:

```ocaml
let rec resolve_dict_arg xctx (cc : Types.class_constraint) tgen_map =
  let arg_types = List.map (fun idx ->
    match Hashtbl.find_opt tgen_map idx with
    | Some ty -> Types.repr ty
    | None -> error "could not resolve constraint type argument"
  ) cc.cc_args in
  let constrained_match = List.find_map (fun ty ->
    match ty with
    | Types.TVar { contents = Types.Unbound (id, _) } ->
      find_constrained_dict xctx id cc.cc_class
    | _ -> None
  ) arg_types in
  match constrained_match with
  | Some dparam -> mk (TEVar dparam) Types.TUnit
```

### Case 2: Concrete type

If all type arguments are concrete, `resolve_dict_arg` searches the instance
list for a matching instance:

```ocaml
  | None ->
    let partial = List.map (fun ty -> Some ty) arg_types in
    let matching = List.filter (fun (inst : Types.instance_def) ->
      String.equal inst.inst_class cc.cc_class &&
      List.length inst.inst_tys = List.length arg_types &&
      Types.match_partial_inst inst.inst_tys partial
    ) xctx.xf_type_env.Types.instances in
    (match matching with
     | [inst] ->
       if inst.inst_constraints <> [] then
         resolve_factory_dict xctx inst arg_types
       else
         mk (TEVar inst.inst_dict_name) Types.TUnit
```

If there is exactly one matching instance with no constraints, the result is
simply a reference to the instance's dictionary global (e.g.,
`TEVar "__dict_Show_int"`). If the instance has constraints (like
`instance Show ('a list) where Show 'a`), it calls `resolve_factory_dict`.

### Ambiguity and specificity

When multiple instances match, `resolve_dict_arg` tries several strategies:

1. **Specificity-based selection** via `most_specific_inst`.
2. **Int defaulting** as a last resort -- if one of the matching instances is
   for `int`, it is chosen.
3. If neither works, it reports an ambiguity error.


## Factory Dictionaries

Some instances are parametric. For example:

```
instance Show ('a list) where Show 'a =
  let show xs = "[" ^ fold (fn acc x ->
      if acc = "" do show x else acc ^ "; " ^ show x
    ) "" xs ^ "]"
end
```

This instance works for any `'a list` -- but only if `'a` itself has a `Show`
instance. The constraint `where Show 'a` means the dictionary for
`Show ('a list)` depends on the dictionary for `Show 'a`.

The typechecker handles this by making the dictionary a **factory function**:
instead of a plain record, it becomes a function that takes sub-dictionaries
and returns a record:

```
(* Conceptual representation *)
let __dict_Show_g0_list __dict_Show_0 = { show = fun xs -> ... }
```

### `xform_constrained_inst`

When `transform_constraints` encounters a `TDLet` whose name matches an
instance with constraints, it calls `xform_constrained_inst`. This function:

1. Walks the class method types alongside the actual method types to find
   which type variables in the instance correspond to which tvar IDs.

2. Builds `dict_params` from the instance constraints, mapping each constraint's
   `TGen` index to the corresponding tvar ID and dictionary parameter name.

3. Rewrites the dictionary record body with `xform_expr`, using the augmented
   `xf_constrained` list so inner method calls resolve to the factory's
   parameters.

4. Wraps the result in `TEFun` nodes for each dictionary parameter:

```ocaml
  let dict_params = List.concat_map (fun (cc : Types.class_constraint) ->
    List.filter_map (fun tgen_idx ->
      let dparam = Printf.sprintf "__dict_%s_%d" cc.cc_class tgen_idx in
      match Hashtbl.find_opt inst_tvar_map tgen_idx with
      | Some tvar_id -> Some (tvar_id, dparam, cc.cc_class)
      | None -> None
    ) cc.cc_args
  ) inst_def.inst_constraints in
  let xctx = { xctx with xf_constrained = dict_params @ xctx.xf_constrained } in
  let rewritten = xform_expr xctx dict_expr in
  List.fold_right (fun (_tid, dparam, _cls) body ->
    mk (TEFun (dparam, body)) (Types.TArrow (Types.TUnit, Types.EffEmpty, body.ty))
  ) dict_params rewritten
```

### `resolve_factory_dict`

At a call site where a factory instance is needed, `resolve_factory_dict`
constructs the call to the factory:

```ocaml
and resolve_factory_dict xctx inst arg_types =
  let sub_map = Hashtbl.create 4 in
  List.iter2 (fun inst_ty actual_ty ->
    let rec walk s a =
      let a = Types.repr a in
      match s with
      | Types.TGen i ->
        if not (Hashtbl.mem sub_map i) then Hashtbl.replace sub_map i a
      | Types.TList s1 ->
        (match a with Types.TList a1 -> walk s1 a1 | _ -> ())
      (* ... other structural cases ... *)
    in
    walk inst_ty actual_ty
  ) inst.inst_tys arg_types;
  let base = mk (TEVar inst.inst_dict_name) Types.TUnit in
  List.fold_left (fun fn sub_cc ->
    let sub_dict = resolve_dict_arg xctx sub_cc sub_map in
    mk (TEApp (fn, sub_dict)) Types.TUnit
  ) base inst.inst_constraints
```

This function:

1. Walks the instance's stored types alongside the actual concrete types to
   build a `TGen -> concrete type` mapping. For `Show ('a list)` applied at
   `Show (int list)`, this maps `TGen 0 -> TInt`.

2. Starts with a reference to the factory function (`TEVar "__dict_Show_g0_list"`).

3. For each constraint on the instance, recursively calls `resolve_dict_arg`
   to find the sub-dictionary, then wraps the factory in a `TEApp`.

So `show [1; 2; 3]` ultimately becomes:
```
(__dict_Show_g0_list __dict_Show_int).show [1; 2; 3]
```

The factory is called with the `int` dictionary to produce the `int list`
dictionary, then the `show` field is accessed.

### `build_tgen_map`: mapping schema types to actual types

Both `xform_constrained_def` and `xform_constrained_ref` use `build_tgen_map`
to find the correspondence between `TGen` indices in a schema type and the
actual types at a use site:

```ocaml
let build_tgen_map schema_ty actual_ty =
  let map = Hashtbl.create 4 in
  let rec walk s a =
    let a = Types.repr a in
    match s with
    | Types.TGen i ->
      if not (Hashtbl.mem map i) then Hashtbl.replace map i a
    | Types.TArrow (s1, _, s2) ->
      (match a with Types.TArrow (a1, _, a2) -> walk s1 a1; walk s2 a2 | _ -> ())
    | Types.TList s1 ->
      (match a with Types.TList a1 -> walk s1 a1 | _ -> ())
    (* ... other structural cases ... *)
  in
  walk schema_ty actual_ty;
  map
```

This parallel walk matches the structure of two types and, whenever the schema
has a `TGen i`, records the corresponding actual type. It is used everywhere
the transformation needs to know "what type did this polymorphic variable get
instantiated to at this particular use site?"


## Instance Selection

When multiple instances could match a type, the typechecker needs to pick the
best one. This is handled by `most_specific_inst`.

### `match_partial_inst`: testing if an instance matches

The `match_partial_inst` function (from `lib/types.ml`) checks whether an
instance's stored types can match a set of concrete types:

```ocaml
let match_partial_inst inst_tys partial =
  let bindings = Hashtbl.create 4 in
  let rec match_one inst conc =
    match inst with
    | TGen i ->
      (match Hashtbl.find_opt bindings i with
       | Some prev ->
         if equal_ty prev conc then true
         else (try unify prev conc; true with Unify_error _ -> false)
       | None -> Hashtbl.replace bindings i conc; true)
    | TInt -> (match repr conc with TInt -> true | _ -> false)
    (* ... other ground types ... *)
    | TList a -> (match repr conc with TList b -> match_one a b | _ -> false)
    (* ... other structural types ... *)
  in
  List.length inst_tys = List.length partial &&
  List.for_all2 (fun inst_ty found_opt ->
    match found_opt with
    | None -> true            (* wildcard: any type matches *)
    | Some conc_ty -> match_one inst_ty conc_ty
  ) inst_tys partial
```

The `partial` list can contain `None` entries for type arguments that are not
yet known, acting as wildcards.

### `inst_specificity`: scoring instances

When multiple instances match, specificity determines which wins. The
`inst_specificity` function counts how many concrete (non-variable) type nodes
appear in the instance's type pattern:

```ocaml
let inst_specificity (inst : instance_def) =
  let rec count = function
    | TGen _ -> 0
    | TInt | TFloat | TBool | TString | TByte | TRune | TUnit -> 1
    | TArrow (a, _, b) -> count a + count b
    | TList a | TArray a -> count a
    | TTuple ts -> List.fold_left (fun acc t -> acc + count t) 0 ts
    | TRecord fs -> List.fold_left (fun acc (_, t) -> acc + count t) 0 fs
    | TVariant (_, ts) -> List.fold_left (fun acc t -> acc + count t) 0 ts
    | TVar { contents = Link t } -> count t
    | TVar { contents = Unbound _ } -> 0
  in
  List.fold_left (fun acc ty -> acc + count ty) 0 inst.inst_tys
```

A `TGen` scores 0 (maximally general), while each concrete type node scores 1.
So `instance Show int` (specificity 1) beats `instance Show 'a` (specificity 0)
for the type `int`.

### `most_specific_inst`: picking a winner

```ocaml
let most_specific_inst insts =
  match insts with
  | [] -> None
  | [x] -> Some x
  | _ ->
    let scored = List.map (fun i -> (inst_specificity i, i)) insts in
    let sorted = List.sort (fun (a, _) (b, _) -> compare b a) scored in
    match sorted with
    | (s1, i1) :: (s2, _) :: _ when s1 > s2 -> Some i1
    | _ -> None
```

If there is a single winner with strictly higher specificity, it is chosen. If
two instances tie for the highest specificity, `None` is returned and the
typechecker falls back to other strategies (int defaulting) or reports an
ambiguity error.

### `find_instance`: direct lookup

For simple cases, `find_instance` does a direct search:

```ocaml
let find_instance instances class_name tys =
  List.find_opt (fun inst ->
    String.equal inst.inst_class class_name &&
    match_partial_inst inst.inst_tys (List.map (fun t -> Some t) tys)
  ) instances
```


## Deriving

MiniML supports `deriving` clauses on type definitions to auto-generate
instances. Currently `Show` and `Eq` can be derived:

```
type color = Red | Green | Blue deriving Show, Eq
```

### How deriving works

The `generate_derived_instance` function dispatches on the class name and type
definition form:

```ocaml
let generate_derived_instance type_params name def class_name =
  match class_name, def with
  | "Show", Ast.TDVariant ctors -> Some (gen_show_variant type_params name ctors)
  | "Show", Ast.TDRecord fields -> Some (gen_show_record type_params name fields)
  | "Eq", Ast.TDVariant ctors -> Some (gen_eq_variant type_params name ctors)
  | "Eq", Ast.TDRecord fields -> Some (gen_eq_record type_params name fields)
  | _ -> None
```

Each `gen_*` function synthesizes AST nodes for the instance methods. For
example, `gen_show_variant` generates a `match` expression that converts each
constructor to a string:

```ocaml
let gen_show_variant type_params name ctors =
  let inst_ty = make_inst_ty type_params name in
  let annots = List.filter_map (fun (_, a) -> a) ctors in
  let constraints = make_constraints "Show" type_params annots in
  let arms = List.map (fun (ctor_name, arg_annot) ->
    match arg_annot with
    | None ->
      (Ast.PatConstruct (ctor_name, None), None, Ast.EString ctor_name)
    | Some _ ->
      let pat = Ast.PatConstruct (ctor_name, Some (Ast.PatVar "__v0")) in
      let body = concat_str_exprs [
        Ast.EString (ctor_name ^ "(");
        Ast.EApp (Ast.EVar "show", Ast.EVar "__v0");
        Ast.EString ")"] in
      (pat, None, body)
  ) ctors in
  let body = Ast.EMatch (Ast.EVar "__x", arms, false) in
  ("Show", [inst_ty], constraints,
   [("show", [Ast.{name = "__x"; annot = None}], body)])
```

For `type color = Red | Green | Blue deriving Show`, this generates:

```
instance Show color =
  let show __x = match __x with
    | Red -> "Red"
    | Green -> "Green"
    | Blue -> "Blue"
end
```

### Constraints in derived instances

The `make_constraints` function examines the type parameters that appear in
constructor arguments to determine which constraint propagation is needed:

```ocaml
let make_constraints class_name type_params annots =
  let all_vars = List.concat_map (fun ann -> collect_tyvars_annot ann) annots in
  let unique_vars = List.sort_uniq String.compare all_vars in
  let param_vars = List.filter (fun v -> List.mem v type_params) unique_vars in
  List.map (fun v -> (class_name, [v])) param_vars
```

For `type 'a box = Box of 'a deriving Show`, this produces the constraint
`Show 'a`, generating:

```
instance Show ('a box) where Show 'a =
  let show __x = match __x with
    | Box __v0 -> "Box(" ^ show __v0 ^ ")"
end
```

The generated instance is then processed by `process_instance_def` like any
other instance -- it goes through the same typechecking and dictionary
generation machinery.


## Worked Example

Let us trace a complete example through every step of typing and
transformation. Consider this program:

```
class Show 'a =
  show : 'a -> string
end

instance Show int =
  let show x = string_of_int x
end

let f (x: 'a) = show x where Show 'a

f 42
```

### Step 1: Processing `class Show 'a`

`check_decl` sees `DClass ("Show", ["a"], [("show", TyArrow(TyVar "a", TyName "string"))])` and calls `process_class_def`.

The function creates a `class_def`:
```ocaml
{ class_name = "Show";
  class_params = ["a"];
  class_methods = [("show", TArrow(TGen 0, TString))] }
```

It adds this to `type_env.classes` and adds `show` to the variable context with
scheme:
```ocaml
{ quant = 1; constraints = []; body = TArrow(TGen 0, TString) }
```

The typed AST gets a `TDClass "Show"` node (which the compiler ignores -- it
produces no code).

### Step 2: Processing `instance Show int`

`check_decl` sees `DInstance ("Show", [TyName "int"], [], [("show", [{name="x"; annot=None}], body)])` and calls `process_instance_def`.

The function:
- Looks up `Show` in the class list.
- Resolves instance type: `stored_tys = [TInt]`.
- Generates dictionary name: `dict_name "Show" [TInt]` = `"__dict_Show_int"`.
- Checks no duplicate instance exists.
- Checks the `show` method is provided.
- Takes the schema type `TGen 0 -> TString`, substitutes `TInt` for `TGen 0`,
  gets `TInt -> TString`. Typechecks `fun x -> string_of_int x` against this.
- Builds dictionary record: `{ show = <typed body> }`.
- Registers `instance_def`:
  ```ocaml
  { inst_class = "Show"; inst_tys = [TInt];
    inst_dict_name = "__dict_Show_int"; inst_constraints = [] }
  ```
- Emits `TDLet ("__dict_Show_int", <record expression>)`.

### Step 3: Processing `let f (x: 'a) = show x where Show 'a`

`check_decl` sees the `DLet` with constraints `[("Show", ["a"])]` and calls
`synth_constrained_fn`.

**3a. Create shared type variables.**
A fresh tvar is created for `'a`, say with ID 17. The `shared_tvars` table maps
`"a" -> TVar(ref (Unbound(17, 1)))`.

**3b. Record constraint tvar IDs.**
`constraint_tvar_ids = [17]`. The inner context gets
`constraint_tvars = [17; ...]`.

**3c. Resolve parameter types.**
The annotation `(x: 'a)` is resolved using `shared_tvars`, giving `x` the type
`TVar(ref (Unbound(17, 1)))`.

**3d. Typecheck the body.**
`synth` processes `show x`:
- `EVar "show"` looks up `show`'s scheme and instantiates it, producing
  `TArrow('b, TString)` where `'b` is fresh (say ID 18).
- `EApp` unifies `TArrow('b, TString)` with `TArrow(typeof_x, ?ret)`.
  This unifies `'b` with `TVar(Unbound(17, 1))` (the tvar for `'a`). Now
  `'b` is linked to `'a`, and the return type is `TString`.

The result: `f` has type `'a -> TString` where `'a` is `TVar(Unbound(17, 1))`.

**3e. Generalize with constraints.**
`resolve_let_constraints` generalizes the type. Since `'a` (ID 17) is at a
deeper level, it becomes `TGen 0`. The constraint `("Show", ["a"])` is resolved:
- `"a"` maps to tvar ID 17 (from `shared_tvars`).
- ID 17 maps to `TGen 0` (from the generalization mapping).
- So the constraint becomes `{ cc_class = "Show"; cc_args = [0] }`.

The final scheme for `f`:
```ocaml
{ quant = 1;
  constraints = [{ cc_class = "Show"; cc_args = [0] }];
  body = TArrow(TGen 0, TString) }
```

The typed AST: `TDLet ("f", TEFun("x", TEApp(TEVar "show", TEVar "x")))`.

### Step 4: Processing `f 42`

`synth` handles `EApp(EVar "f", EInt 42)`:
- `EVar "f"` instantiates `f`'s scheme with a fresh tvar, say `'c` (ID 19),
  giving type `'c -> TString`.
- `EInt 42` has type `TInt`.
- Unify `'c -> TString` with `TInt -> ?ret`, giving `'c = TInt` and
  `?ret = TString`.

Typed AST: `TDExpr (TEApp(TEVar "f", TEInt 42))` with type `TString`.

### Step 5: Constraint transformation

`transform_constraints` processes the typed program. The `xctx` is initialized
with all schemes and the type environment.

**5a. `TDLet ("__dict_Show_int", ...)`** -- no constraints on this instance,
so it just runs `xform_expr` on the dictionary record. No methods inside need
rewriting (they call `string_of_int`, not `show`), so it passes through
unchanged.

**5b. `TDLet ("f", TEFun("x", TEApp(TEVar "show", TEVar "x")))`** -- `f` has
constraints, so `xform_constrained_def` is called.

`build_tgen_map` walks `TArrow(TGen 0, TString)` alongside the actual type
`TArrow(TVar(Unbound(17, ...)), TString)`, mapping `TGen 0 -> TVar(Unbound(17, ...))`.

`dict_params` is computed: for constraint `{ cc_class = "Show"; cc_args = [0] }`,
TGen 0 maps to an unbound tvar with ID 17, so:
```
dict_params = [(17, "__dict_Show_0", "Show")]
```

Now `xf_constrained = [(17, "__dict_Show_0", "Show")]`, and `xform_expr` is
called on the body.

Inside the body, `TEVar "show"` hits `xform_class_method`. The function finds
that `show` is a method of `Show`. It walks `TGen 0 -> TString` alongside the
actual type to find `TGen 0 = TVar(Unbound(17, ...))`. Since ID 17 is in
`xf_constrained` with class `Show`, it returns `"__dict_Show_0"`. The `TEVar "show"` is rewritten to:
```
TEField(TEVar "__dict_Show_0", "show")
```

The body becomes:
```
TEApp(TEField(TEVar "__dict_Show_0", "show"), TEVar "x")
```

The function is then wrapped with a `TEFun` for the dictionary parameter:
```
TEFun("__dict_Show_0", TEFun("x",
  TEApp(TEField(TEVar "__dict_Show_0", "show"), TEVar "x")))
```

**5c. `TDExpr (TEApp(TEVar "f", TEInt 42))`** -- `xform_expr` processes this.

`TEVar "f"` hits `xform_constrained_ref`. It finds `f`'s scheme has
constraint `Show` on `TGen 0`. `build_tgen_map` maps `TGen 0 -> TInt`.
`resolve_dict_arg` is called with class `Show` and type `TInt`.

Since `TInt` is concrete, it searches the instance list and finds
`{ inst_class = "Show"; inst_tys = [TInt]; inst_dict_name = "__dict_Show_int" }`.
No constraints on this instance, so the result is `TEVar "__dict_Show_int"`.

The call is rewritten to:
```
TEApp(TEApp(TEVar "f", TEVar "__dict_Show_int"), TEInt 42)
```

### Final transformed program

```
let __dict_Show_int = { show = fun x -> string_of_int x }

let f __dict_Show_0 x = __dict_Show_0.show x

(f __dict_Show_int) 42
```

The compiler sees ordinary records, field accesses, function parameters, and
function applications. There is no concept of "type class" or "constraint" left
in the AST. The dictionary passing has been compiled away completely.


## Built-In Classes

MiniML ships with several built-in classes registered via `lib/interp.ml`
before user code runs:

| Class | Type params | Fundeps | Methods |
|-------|-------------|---------|---------|
| `Num` | `'a` | | `(+) (-) (*) (/) : 'a -> 'a -> 'a`, `neg : 'a -> 'a` |
| `Eq` | `'a` | | `(=) (<>) : 'a -> 'a -> bool` |
| `Ord` | `'a` | | `(<) (>) (<=) (>=) : 'a -> 'a -> bool` |
| `Bitwise` | `'a` | | `land lor lxor lsl lsr : 'a -> 'a -> 'a`, `lnot : 'a -> 'a` |
| `Show` | `'a` | | `show : 'a -> string` |
| `Iter` | `'c 'e` | | `fold : ('a -> 'e -> 'a) -> 'a -> 'c -> 'a` |
| `Index` | `'c 'k 'v` | `'c -> 'k 'v` | `at : 'k -> 'c -> 'v` |
| `Map` | `'m 'k 'v` | | `of_list get set has remove size keys values to_list` |

Instances for primitive types (`int`, `float`, `string`, `bool`, `byte`,
`rune`) are registered as native externals. Compound-type instances (like
`Show ('a list)`) are defined as MiniML source code evaluated at startup via
`eval_setup`:

```ocaml
let state = eval_setup state
  "instance Show ('a list) where Show 'a = \
     let show xs = match xs with \
       | [] -> \"[]\" \
       | _ -> \"[\" ^ fold (fn acc x -> \
           if acc = \"\" do show x else acc ^ \"; \" ^ show x \
         ) \"\" xs ^ \"]\" \
     end"
```

These compound instances go through the full typechecker pipeline, including
constraint transformation to produce factory dictionaries.


## Functional Dependencies

Functional dependencies (fundeps) tell the typechecker that certain class
parameters are determined by others. This guides type inference so the compiler
can resolve ambiguous type variables automatically.

### Syntax and representation

A class with a functional dependency is declared with a `where` clause between
the type parameters and the `=`:

```
class Index 'c 'k 'v where 'c -> 'k 'v =
  at : 'k -> 'c -> 'v
end
```

This says: given the container type `'c`, the key type `'k` and value type `'v`
are uniquely determined. The parser stores fundeps as string-based pairs
`(["c"], ["k"; "v"])`, which `process_class_def` converts to index-based
`{ fd_from = [0]; fd_to = [1; 2] }`.

### How fundeps guide type inference

Fundeps are applied at three points during compilation:

**1. During type inference (`improve_fundeps_in_expr`).** Before
let-generalization, the typechecker fires fundep improvement on expressions.
When a class method is called (like `at`) and the determining parameters are
known (e.g., the container is `int array`), `improve_with_fundeps` searches
instances to find the determined types and unifies them with the corresponding
type variables. This prevents over-generalization -- without fundeps, the key
and value types would remain as free type variables and be generalized into the
scheme.

```ocaml
let improve_with_fundeps instances class_def partial =
  List.fold_left (fun partial fd ->
    (* Check if all determining positions are known *)
    let from_known = List.for_all (fun i ->
      i < List.length partial && List.nth partial i <> None
    ) fd.fd_from in
    if not from_known then partial
    else begin
      (* Find instances matching on the determining positions *)
      let matching = List.filter (fun inst -> ...) instances in
      match matching with
      | [inst] ->
        (* Exactly one match: fill in the determined positions *)
        List.mapi (fun i opt ->
          if List.mem i fd.fd_to then Some (List.nth inst.inst_tys i)
          else opt
        ) partial
      | _ -> partial
    end
  ) partial class_def.class_fundeps
```

**2. During constraint transformation (`resolve_dict_arg`).** When resolving
dictionary arguments for a constrained call, fundeps are applied to fill in
unresolved type positions. If a constraint has `CAWild` arguments (positions
not mentioned in the method type but determined by a fundep), improvement
resolves them to concrete types so instance lookup can succeed.

**3. Instance consistency checking (`check_fundep_consistency`).** When a new
instance is registered, the typechecker verifies it does not violate existing
fundep constraints. If two instances agree on all determining positions, they
must also agree on all determined positions:

```ocaml
let check_fundep_consistency class_def new_inst existing_instances =
  List.iter (fun fd ->
    List.iter (fun existing ->
      (* If determining types match, determined types must also match *)
      ...
    ) existing_instances
  ) class_def.class_fundeps
```

### The Index class: a fundep example

The `Index` class demonstrates how fundeps work in practice:

```
class Index 'c 'k 'v where 'c -> 'k 'v =
  at : 'k -> 'c -> 'v
end

instance Index ('a array) int 'a = ...
instance Index string int rune = ...
instance Index (('k, 'v) map) 'k 'v = ...
```

The fundep `'c -> 'k 'v` means: given the container type, the key and value
types are uniquely determined. When the typechecker sees `arr.[0]` (which
desugars to `at 0 arr`) and knows `arr : int array`, it:

1. Finds the `Index` class and its fundep `{ fd_from = [0]; fd_to = [1; 2] }`.
2. The container type (`int array`) matches parameter 0.
3. Searches instances: `Index ('a array) int 'a` matches with `'a = int`.
4. Fills in parameter 1 (`int`) and parameter 2 (`int`).
5. The result type is `int` -- no annotation needed.

Without the fundep, the typechecker would not know that the result type of
`arr.[0]` is `int` until much later (or not at all), potentially causing
ambiguity errors or requiring explicit type annotations.


## Summary

The type class implementation follows a clean two-pass architecture:

**Pass 1 (Type inference):**
- `process_class_def` registers class definitions (including functional
  dependencies) and adds methods to the variable context as polymorphic schemes.
- `process_instance_def` typechecks method bodies, builds dictionary records,
  registers instances in the type environment, and checks fundep consistency.
- `synth_constrained_fn` handles `where` clauses by pre-allocating shared type
  variables, recording their IDs in `constraint_tvars`, and producing schemes
  with `class_constraint` records.
- `improve_fundeps_in_expr` fires fundep improvement before let-generalization
  to resolve determined type variables.
- Method calls during this pass are treated as ordinary polymorphic function
  calls -- the type variable gets unified with the argument type but no
  dictionary is resolved yet.

**Pass 2 (Constraint transformation):**
- `transform_constraints` walks the typed program, dispatching each declaration
  to the appropriate handler.
- `xform_constrained_def` adds dictionary parameters to constrained functions.
- `xform_class_method` rewrites method references to dictionary field accesses.
- `xform_constrained_ref` inserts dictionary arguments at call sites.
- `resolve_dict_arg` finds the right dictionary -- either a parameter (for
  constrained tvars) or a concrete instance dictionary.
- `resolve_factory_dict` handles parametric instances by applying factory
  functions with sub-dictionaries.
- `xform_constrained_inst` wraps parametric instance dictionaries in factory
  functions.

After the second pass, the AST contains no type class references. Dictionaries
are plain records, method calls are field accesses, and constraint propagation
is ordinary function parameter passing. The compiler and VM never need to know
that type classes exist.
