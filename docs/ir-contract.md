# The Backend Contract: MiniML's Normalized IR

This document specifies the **intermediate representation** that every MiniML
backend consumes — the OCaml VM (bytecode), `--emit-js`, and the LLVM native
backend all lower the *same* IR. It is the structural companion to
[`semantics.md`](semantics.md): semantics.md says what each construct *means*
when evaluated (the oracle is its executable form); this document says what each
node *is*, what **invariants** hold over it, and what a backend may assume
without re-deriving.

> **Why this exists.** Three backends independently re-deriving "is this resume
> in tail position?" or "can this match arm bind a name the optimizer must not
> inline?" is how divergence bugs are born. The contract is the single place
> those guarantees are written down. Phase 3 items [#11] (hoist shared analyses)
> and [#12] (shared effect lowering) build directly on it: an analysis can only
> be hoisted into a shared IR property once the property it computes is part of
> the contract.

Audience: backend authors, and anyone adding an IR node or a lowering pass.

The authority chain is unchanged: **oracle > this document > backends**. Where a
backend disagrees with the oracle it has a bug; where this document disagrees
with the oracle, this document is wrong and should be fixed.

---

## 1. Shape of the pipeline

```
source
  │  lexer + parser                         → Ast.program          (surface syntax)
  │  typechecker                            → Typechecker.tprogram (typed AST)
  ▼
Typechecker.tprogram                        ← the typed AST; effects resolved into types
  │  Pipeline.lower:
  │    1. Typechecker.classify_handlers_with   THOp → THOpProvide | THOpTry | (THOp)
  │    2. Match_tree.lower_program             TEMatch → TEMatchTree
  │    3. Texpr_opt.optimize_program           dead-let / inline / const-fold
  ▼
Typechecker.tprogram  (NORMALIZED)          ← what every backend consumes
  │
  ├── lib/compiler.ml      → Bytecode → lib/vm.ml      (reference execution)
  ├── lib/js_codegen.ml    → JavaScript                (--emit-js, browser)
  └── lib/native/codegen.ml → LLVM IR → native binary
```

The IR is **one type used at two refinement levels**, not a stack of distinct
IRs. `Typechecker.tprogram` is the same OCaml type before and after
`Pipeline.lower`; lowering *narrows* which constructors may appear. The narrowed
form is the **normalized IR** and is the subject of this contract. "The IR" below
always means the normalized form unless stated otherwise.

`Pipeline.lower` (`lib/pipeline.ml:67`):

```ocaml
let lower ?(inline_handlers = true) ?(stdlib_programs = []) type_env
    (typed_program : Typechecker.tprogram) : Typechecker.tprogram =
  let p = Typechecker.classify_handlers_with inline_handlers typed_program in
  let p = Match_tree.lower_program type_env p in
  validate_lowered "match-tree lowering" p;
  let p = Texpr_opt.optimize_program ~stdlib_programs p in
  validate_lowered "texpr optimization" p;
  p
```

The two `validate_lowered` calls enforce the post-conditions in §6 structurally
— a backend never has to defend against a form the contract forbids, because
lowering aborts before any backend sees it.

---

## 2. The carrier: `texpr`

Every expression node is a `texpr` (`lib/typechecker.ml:3`):

```ocaml
type texpr = { expr : texpr_kind; ty : Types.ty; loc : Token.loc }
```

Three fields, three guarantees:

- **`expr`** — the node itself (§3).
- **`ty`** — the node's type, **fully resolved**. By the time the IR leaves the
  typechecker, every `ty` is ground or a generalized scheme variable
  (`TGen`/`EffGen`); there are no unresolved unification variables pointing into
  mutable typechecker state. Backends read `ty` for type-directed decisions
  (float display shapes, boxed vs. unboxed, native header tags) and may treat it
  as immutable.
- **`loc`** — source location, carried for diagnostics only. No lowering or
  codegen decision may depend on `loc`.

`ty` is **load-bearing, not decorative.** Two nodes with identical `expr` shape
but different `ty` can lower differently (e.g. a whole-number literal displayed
as `3` vs. `3.`; a structural-equality call dispatched by element type). A
backend that erases `ty` early will reintroduce the exact bugs the typed AST
exists to prevent.

The top level is a list of declarations (`lib/typechecker.ml:58`):

```ocaml
type tdecl =
  | TDLet of string * texpr
  | TDLetMut of string * texpr
  | TDLetRec of string * texpr
  | TDLetRecAnd of (string * texpr) list
  | TDType of string * Ast.type_def
  | TDExpr of texpr
  | TDClass of string
  | TDEffect of string
  | TDExtern of string * Types.scheme
  | TDModule of string * tdecl list * (string * Types.scheme) list
  | TDOpen of (string * string) list

type tprogram = tdecl list
```

`TDType`, `TDClass`, `TDEffect`, `TDExtern`, and `TDOpen` are **erased shells** by
the time codegen runs: they carry declaration metadata the typechecker already
consumed (type definitions, class/effect declarations, extern signatures, open
aliases) and have no runtime effect. Backends evaluate only the value-binding
decls (`TDLet`, `TDLetMut`, `TDLetRec`, `TDLetRecAnd`, `TDExpr`) and recurse into
`TDModule`. `TDExtern` is the one shell a backend *reads* — to learn the C/JS
name and arity of an external — but it emits no code of its own.

---

## 3. Expression nodes (`texpr_kind`)

The full set (`lib/typechecker.ml:5`). Each node's *evaluation* is specified in
semantics.md; here we give the **structural role** and any **contract invariant**
a backend may rely on.

### 3.1 Literals and variables

| Node | Args | Notes |
|---|---|---|
| `TEInt` | `int` | Exact within ±2⁵³; overflow is UB (semantics.md §1). |
| `TEFloat` | `float` | IEEE-754 double. |
| `TEBool` | `bool` | |
| `TEString` | `string` | Immutable byte string. |
| `TEByte` | `int` | 0–255. |
| `TERune` | `int` | Unicode scalar value. |
| `TEUnit` | — | The single `()` value. |
| `TEVar` | `string` | A name reference; resolved **at use time** (§5.1). |
| `TENil` | — | The empty list. |

### 3.2 Construction

| Node | Args | Notes |
|---|---|---|
| `TETuple` | `texpr list` | Fixed length ≥ 2; immutable. |
| `TEArray` | `texpr list` | Mutable, fixed length. |
| `TECons` | `texpr * texpr` | `head :: tail`. |
| `TERecord` | `(string * texpr) list` | Field order is **declaration order** (a cross-backend contract: native indexes fields positionally). |
| `TEConstruct` | `string * texpr option` | Variant constructor; tag is declaration index (semantics.md §1). `None` payload = nullary. Newtype constructors are erased here. |

### 3.3 Binding

| Node | Args | Notes |
|---|---|---|
| `TELet` | `string * scheme option * texpr * texpr` | Non-recursive. `scheme` present iff the binding is generalized (polymorphic). |
| `TELetRec` | `string * scheme option * texpr * texpr` | Single recursive binding. RHS is a function **or** a constructive value (§5.3). |
| `TELetRecAnd` | `(string * texpr) list * texpr` | Mutually recursive group; all names in scope in every RHS and the body. |
| `TELetMut` | `string * texpr * texpr` | Introduces a **mutable local** — a heap cell, not an SSA value (§5.2). |
| `TEFun` | `string * texpr * bool` | One parameter (curried). The `bool` is `has_return`: the body may execute `return e`, caught at this function's application boundary. |

### 3.4 Application and control

| Node | Args | Notes |
|---|---|---|
| `TEApp` | `texpr * texpr` | One argument (curried). Over-/under-application handled by the backend's arity protocol. |
| `TEIf` | `texpr * texpr * texpr` | Both branches present (surface `do/else/end`; a missing else is `TEUnit`). |
| `TEBinop` / `TEUnop` | `binop/unop * ...` | Primitive operators; see semantics.md for short-circuit `&&`/`||`. |
| `TESeq` | `texpr * texpr` | Evaluate left for effect, discard, yield right. |
| `TEReturn` | `texpr` | Early return; caught at the nearest enclosing `TEFun` with `has_return = true`. |
| `TEMatch` | `texpr * (pattern * texpr option * texpr) list * match_kind` | **Surface match — removed by lowering** (§6). Never reaches a backend. |
| `TEMatchTree` | `texpr compiled_match` | The lowered decision tree (§4). The only match form a backend sees. |

### 3.5 Field / index access and mutation

| Node | Args | Notes |
|---|---|---|
| `TEField` | `texpr * string` | Record field read. |
| `TEIndex` | `texpr * texpr` | Array/string index read. |
| `TEFieldAssign` | `texpr * string * texpr` | Record field write (`<-`). |
| `TEAssign` | `string * texpr` | Mutable-local write (`:=`); LHS is a `TELetMut` name (§5.2). |
| `TERecordUpdate` | `texpr * (string * texpr) list` | Functional update: new record, named fields replaced. |
| `TERecordUpdateIdx` | `texpr * (texpr * texpr) list` | Functional update by computed field index (row-polymorphic). |

### 3.6 Loops

| Node | Args | Notes |
|---|---|---|
| `TEWhile` | `{ tw_cond; tw_body; tw_step }` | `tw_step` runs before each re-test, **including after `continue`**. Result is the `break` value or unit. |
| `TEForLoop` | `texpr` | A `fold` application whose callback carries fold-control semantics (§5.4). |
| `TEBreak` | `texpr` | Caught at the nearest enclosing loop. |
| `TEContinueLoop` | — | `continue` in a `while`/unit loop. |
| `TEFoldContinue` | `texpr` | `continue` inside a `for` loop body (caught at the fold callback boundary). |

`TEContinueLoop` vs. `TEFoldContinue` is **not** redundant: a `while` loop catches
`continue` at the loop itself, but a `for` loop desugars to a fold and must catch
`continue` at the *callback* boundary so the fold keeps iterating. The
typechecker picks the right one from `ctx.loop_info` (`WhileLoop | UnitLoop |
FoldLoop`); a backend reads the node and need not re-derive the loop kind.

### 3.7 Effects

| Node | Args | Notes |
|---|---|---|
| `TEPerform` | `string * texpr` | Perform operation `op` with one argument. |
| `TEHandle` | `texpr * thandle_arm list` | Deep handler around a body (§4.2, §5.5). |
| `TEResume` | `texpr * texpr` | `resume k v` — resume a captured continuation. **One-shot** (§5.5). |

Effects are the IR's hardest contract and the locus of Phase 3 [#12]. Their
operational meaning is semantics.md §6–§12; their structural classification is
§4.2 below.

---

## 4. The two compound forms

Two `texpr_kind` constructors carry structured sub-IR rather than plain
sub-expressions: `TEMatchTree` and `TEHandle`. They are where most of the
contract's subtlety lives.

### 4.1 Match trees

`TEMatchTree` wraps a `compiled_match` (`lib/match_tree_types.ml:73`):

```ocaml
type 'expr compiled_match = {
  scrutinee : 'expr;
  scrutinee_ty : Types.ty;
  match_arms : 'expr match_arm array;   (* arm bodies, indexed *)
  tree : 'expr dtree;                   (* the decision tree *)
  loc : Token.loc;
  match_kind : Ast.match_kind;
}
```

The decision tree (`lib/match_tree_types.ml:48`):

```ocaml
type 'expr dtree =
  | DSwitch of {
      occ : occurrence;                                  (* what to test *)
      occ_ty : Types.ty;
      cases : (test * binding list * 'expr dtree) list;  (* per-test subtrees *)
      default : 'expr dtree option;
    }
  | DGuard of {
      arm_idx : int;
      guard : 'expr;
      bindings : binding list;
      on_true : 'expr dtree;
      on_false : 'expr dtree;
    }
  | DLeaf of { arm_idx : int; bindings : binding list }
  | DFail of Token.loc                                   (* inexhaustive *)
```

An **occurrence** is a path from the scrutinee to a sub-value
(`lib/match_tree_types.ml:18`): a list of `access` steps (`ATupleField`,
`ARecordField`, `AConsHead`, `AConsTail`, `AVariantPayload`, `AArrayElem`,
`AMapValue`). A backend walks the occurrence to materialize the value a `test`
applies to. **Occurrences are pure projections** — evaluating one has no side
effect and may be recomputed or cached freely.

**Contract invariants for match trees:**

1. **The scrutinee is evaluated exactly once.** `tree` and every occurrence
   project from that one value. A backend must bind the scrutinee to a temporary
   before walking the tree, never re-evaluate `scrutinee`.
2. **Tests within a `DSwitch` are mutually exclusive** and exhaustive together
   with `default`. A backend may compile a `DSwitch` to a jump table when the
   tests are dense integer/tag literals (the VM and native both do).
3. **`bindings` bind occurrences to names** (`{ var_name; bind_occ; bind_ty }`,
   `lib/match_tree_types.ml:45`). A binding's scope is its subtree (for
   `DSwitch`/`DGuard` cases) or its arm body (`DLeaf`). Bindings are **pin-safe**:
   see §5.6.
4. **`DGuard.on_false` is the fall-through** to the next candidate arm, already
   computed by lowering. A backend never re-orders arms; arm priority is baked
   into the tree.
5. **`DFail` marks inexhaustiveness.** Reaching it at runtime is a match failure
   (semantics.md). The typechecker emits `DFail` only where exhaustiveness could
   not be proven; a backend lowers it to its match-failure trap.
6. **`arm_idx` indexes `match_arms`.** A `DLeaf`/`DGuard` names an arm by index;
   the body lives in `match_arms.(arm_idx).arm_body`. Multiple leaves may share an
   arm index (the same body reached by different test paths) — a backend that
   emits arm bodies inline must guard against duplicating effects or code size,
   typically by emitting each arm once and jumping to it.

### 4.2 Handlers and their classification

A `TEHandle` carries a body and a list of arms (`lib/typechecker.ml:51`):

```ocaml
and thandle_arm =
  | THReturn   of string * texpr                                  (* return x -> e *)
  | THOp       of { op_name : string; arg : string; k : string; body : texpr }
  | THOpProvide of string * string * texpr      (* tail-resumptive: op arg -> value *)
  | THOpTry    of string * string * texpr       (* non-resuming:    op arg -> fallback *)
```

`Pipeline.lower`'s **first** pass classifies every `THOp` into one of three final
forms (`Typechecker.classify_arm`, `lib/typechecker.ml:8574`):

```ocaml
let classify_arm arm =
  match arm with
  | THOp { op_name; arg; k; body } ->
      if (not (handler_body_has_resume body))
         && (not (handler_body_has_perform body))
         && not (cont_used k body)
      then THOpTry (op_name, arg, body)
      else if handler_body_has_resume body
              && classify_all_branches_resume body
              && classify_all_resumes_tail body
      then THOpProvide (op_name, arg, strip_tail_resume body)
      else arm                                    (* stays THOp: general / fiber *)
  | _ -> arm
```

This yields a **three-way handler taxonomy** — the backbone of [#12]'s shared
effect lowering. Each class is a strictly cheaper lowering, and the
classification is a *property of the IR*, computed once, not a backend heuristic:

| Class | When | Continuation | Cheapest correct lowering |
|---|---|---|---|
| **`THOpTry`** | body never resumes, never performs, never names `k` | dropped | install a catch; run `fallback`; its value *is* the handle result (no return arm) |
| **`THOpProvide`** | every branch ends in `resume k V`, all in tail position | implicit, tail | run `value` with `arg` bound; feed it straight back to the body — **no continuation capture needed** |
| **`THOp`** | anything else (non-tail resume, conditional resume, `k` escapes, multishot) | first-class, reified | full continuation capture (fibers / CPS / native stack copy) |

**Contract invariants for handlers:**

1. **`THReturn` is the value path.** If the body completes normally with `v`, and a
   `THReturn (x, e)` arm exists, the result is `e` with `x = v`; with no return
   arm, the result is `v`. (The parser synthesizes `return x -> x` when the
   source omits it, so `THReturn` is effectively always present — but a backend
   must still handle its absence as identity.)
2. **`THOpTry` produces the handle result directly** — its fallback does **not**
   thread through `THReturn`. This matches the VM; semantics.md §6.
3. **`THOpProvide` is tail-resumptive sugar.** Lowering has already
   `strip_tail_resume`'d the body, so the carried `texpr` is the *value to resume
   with*, not a `resume` expression. A backend evaluates it with `arg` bound and
   resumes the body — no reified continuation, no fiber. This is the fast path
   for the overwhelmingly common "interpret one operation and continue" handler.
4. **`THOp` is the only class that needs a reified continuation**, and therefore
   the only one whose cost a backend cannot avoid. Deep semantics: the resumed
   continuation re-enters *this* handler (semantics.md §6.1, §11). The continuation
   is one-shot unless copied (§5.5).
5. **Handlers are deep, and an unhandled operation reinstalls the handler around
   the outer resumption** (semantics.md §6.1). A backend must keep the handler
   live across a pass-through perform — this is the [#25] "keep handlers installed
   around `resume k`" guarantee.
6. **The classification rewrite is unconditional** — it does not depend on the
   backend. What the `inline_handlers` flag controls is narrower: whether an
   all-`THOpTry` or all-`THOpProvide` handler may run its body **inline** (no
   fiber barrier), which in turn decides whether an early `return` is allowed to
   *escape* that handler. The bytecode VMs pass `inline_handlers = true` (they
   have `compile_handle_try`/`compile_handle_provide` inline lowerings, so a
   `return` crossing such a handler is fine); native passes `false` (every
   handler body is a fiber barrier there, so a `return` crossing one is a clean
   compile error rather than a silent miscompile — `lib/typechecker.ml:8624`).
   The flag changes *return-escape legality*, never *which arms get classified*.

`THOp` (unclassified) **does** survive into the normalized IR — `classify_arm`
returns the arm unchanged when it is genuinely general. So unlike `TEMatch` (§6),
`THOp` is a legal normalized-IR node. What lowering guarantees is that every
`THOp` that *could* be a `THOpProvide`/`THOpTry` already is — on every backend.

---

## 5. Cross-cutting invariants

These hold across many nodes and are the guarantees most often re-derived (and
mis-derived) per backend. They are the prime candidates for [#11].

### 5.1 Names resolve at use time

A `TEVar "f"` resolves through the lexical environment, falling back to the global
namespace **at the point of use**, not at closure creation (oracle
`lib/oracle.ml:199`, semantics.md §5). Consequences a backend must honor:

- A closure capturing a global name observes later redefinitions of that name.
- Mutual recursion and forward references work without a topological sort.
- Redefinition binds a **fresh cell**; an `open` alias sharing the old cell keeps
  its old value.

### 5.2 Mutable locals are heap cells

`TELetMut` does not introduce an SSA value; it introduces a **reference cell on the
heap**. `TEAssign` writes it, a bare `TEVar` reads it. This matters precisely at
the intersection with effects: when a continuation is captured and resumed (or
*copied* and resumed twice), the mutable local is **shared** across resumptions —
it is heap state, not control state (semantics.md §9, §11). The VM's `MAKE_REF`
lowering is the reference; emit-js boxes mutable locals in a one-slot object;
native allocates a ref cell. A backend that lowers `TELetMut` to a flat local
will silently diverge under multishot resume.

### 5.3 Recursive bindings: function or constructive value

`TELetRec`/`TELetRecAnd` RHSs are either functions or **constructive values** — a
value whose outermost constructor is statically known (`let rec xs = 1 :: xs`).
The spec lowering is *placeholder + in-place update* (oracle `lib/oracle.ml:644`,
semantics.md §9.2): allocate the outer shape, bind the name to it, evaluate the
RHS (whose self-references see the placeholder), then tie the knot by updating the
placeholder's fields. The typechecker's constructive-binding check guarantees the
shapes match; a backend may assume it and need not verify at runtime.

### 5.4 For-loops are folds with marked callbacks

`TEForLoop` wraps a `fold` application. The callback closure carries fold-control
semantics: a `TEFoldContinue` inside it is caught at the callback boundary (so the
fold keeps iterating), and a `TEBreak` escapes to the loop. The oracle marks the
callback `c_is_fold_cb` (`lib/oracle.ml:1077`); backends use the analogous
`fold_cont_pending` mechanism. The contract: **the callback boundary is a
control-flow catch point**, and — critically — when a `perform` surfaces inside
the callback, the catch wrapper must reinstall itself around the resumption (§5.5,
semantics.md §6.1).

### 5.5 Continuations are one-shot; copy before resume

A continuation captured by a `THOp` handler is **one-shot**: resuming it twice is a
runtime error (oracle `lib/oracle.ml:1134`). To resume more than once, `copy_continuation`
must be called **before** the first resume; it shares the resumption but takes a
fresh use-flag (oracle `lib/oracle.ml:1319`, semantics.md §12). Copying an
*already-resumed* continuation is an error — the oracle's closure-based
resumptions would "work," but fiber/native backends have already consumed the
stack, so the contract forbids it uniformly. **Heap state is shared across
copies; control state is duplicated** (§5.2).

### 5.6 The loop body is part of every perform's continuation

When a `perform` surfaces inside a `TEWhile` body (or a for-loop callback), the
remainder of *that iteration and all following iterations* is part of the captured
continuation. Structurally this means the loop's outcome-handler must re-wrap
itself around the resumption (oracle `after_body`, `lib/oracle.ml:814`;
semantics.md §6.1). A backend that drops the loop from the continuation breaks
deep-handler arm-value chaining (this was BUG-13). This is the single most
error-prone effect/loop interaction and is called out as its own invariant for
that reason.

### 5.7 Pin patterns block inlining and elimination

A name bound by a match (`binding.var_name`) or referenced by a pin test
(`TPin`/`MKPin`, `lib/match_tree_types.ml`) is looked up *by name at runtime* by
the match machinery. The optimizer (`Texpr_opt`) therefore must **not** inline or
dead-eliminate a binding a pin pattern references — doing so removes the name the
runtime looks for. This invariant is enforced inside `Texpr_opt`, but a backend
performing its own local rewrites must respect it too.

---

## 6. Post-conditions of `Pipeline.lower`

After `Pipeline.lower`, a backend may rely on the following structurally — each is
enforced by `validate_lowered` and the lowering passes:

1. **No `TEMatch`.** Every surface match is a `TEMatchTree`. A backend implements
   decision trees only, never the surface match-arm list. (This is the one node
   the contract *guarantees* absent; `THOp`, by contrast, may legally remain — §4.2.)
2. **Handlers classified.** Every arm that *qualifies* as `THOpProvide`/`THOpTry`
   has been rewritten (unconditionally, on every backend); residual `THOp` arms
   are genuinely general. The `inline_handlers` flag affects only whether `return`
   may escape an inline-lowered try/provide handler (§4.2), not the rewrite.
3. **Optimized.** Dead non-effectful lets removed, single-use pure lets inlined,
   constants folded — subject to the pin-safety (§5.7) and purity (mutable reads
   and effects are not pure) guards. A backend must not assume the *absence* of
   any particular optimization, only that what remains is semantically equal to
   the un-optimized IR.
4. **Types resolved (§2).** No node's `ty` points into live unification state.

Lowering is **idempotent in shape**: running it twice changes nothing (matches are
already trees, handlers already classified). Backends and golden-IR tests ([#13])
may rely on this.

---

## 7. What is *not* in the IR

The contract is also defined by its omissions. A backend must supply these itself;
the IR deliberately does not encode them, because they are lowering decisions, not
semantics:

- **Closure conversion / free-variable capture.** The IR has lexical `TEVar`s;
  computing captured environments is per-backend.
- **Arity / calling convention.** `TEApp` is one-argument-at-a-time; how a backend
  batches curried applications, boxes arguments, or handles over-application is its
  own protocol.
- **Continuation representation.** Fibers (VM/native) vs. CPS + trampoline
  (emit-js) are backend choices. The IR says *which handler class* (§4.2); the
  backend chooses *how*.
- **Memory layout.** Header words, tag encoding, boxed/unboxed floats — native's
  concern; the IR gives only `ty` and the declaration-order/declaration-index
  contracts (§3.2).
- **Effect rows as runtime data.** Effects are resolved into `Types.ty`
  (`TArrow`/`TCont` carry an `eff`; `lib/types.ml:30`) for *typechecking*. By the
  time the IR reaches a backend, effects survive only as the handler
  classification (§4.2) and the structure of `TEPerform`/`TEHandle`/`TEResume`.
  No backend reads effect rows. **[#12] may change this**: a selective-CPS calling
  convention would lift effect rows into an IR-level property that decides which
  functions take continuation parameters — at which point effect rows become part
  of *this* contract.

---

## 8. Relationship to the other docs

| Doc | Answers |
|---|---|
| [`semantics.md`](semantics.md) | *What does it mean to evaluate this?* (the oracle, in prose) |
| **this doc** | *What is the IR a backend consumes, and what may it assume?* |
| [`effects.md`](effects.md) | The effect system from the language user's view. |
| [`compiler.md`](compiler.md) | The VM bytecode lowering specifically. |
| [`js-backend.md`](js-backend.md) | The emit-js lowering specifically. |

When these conflict, the order of authority is: oracle (executable) → semantics.md
→ this document → individual backend docs.
