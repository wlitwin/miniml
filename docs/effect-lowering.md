# Effect Lowering and Selective CPS (design)

This is a **design document**, not a description of shipped behavior. It
specifies how MiniML should close the remaining effect-system correctness gap
(BUG-11) and, in doing so, defines the one IR property that Phase 3 [#11] should
hoist. It is the effect-system counterpart to [`ir-contract.md`](ir-contract.md)
(IR structure) and [`semantics.md`](semantics.md) (operational meaning) — read
those first; this builds on both.

Status: proposed. Authority for *behavior* remains the oracle; this document
proposes how one backend should *lower* that behavior. Nothing here changes the
semantics — only how emit-js realizes them.

---

## 1. The problem, stated precisely

Every backend must implement deep, multishot, copyable continuations
(`semantics.md` §6, §11, §12). Two backends do it with **fibers**: the handled
body runs on a separate stack, `perform` captures that stack, `resume` switches
back to it, `copy_continuation` clones it. Because a fiber captures the *entire*
call stack, a `perform` works no matter how deeply it is nested — including
inside a lambda passed to a higher-order function:

```
let apply f x = f x
handle
  apply (fn x -> perform op x) 10   (* the perform is under apply's frame *)
with | op v k -> resume k (v + 1)
```

On the OCaml VM and native, `apply`'s frame is on the fiber stack, so the
continuation captured at `perform op x` spans `apply` and everything after the
`handle` body up to the handler. **Correct on both.** The oracle agrees (its
`apply` is an ordinary recursive call; the `Perform` outcome propagates up
through it). So this program is correct on three of the four execution engines.

**emit-js is the exception, and the only backend with the bug.** It has no
fibers. It realizes continuations by compiling effectful code in
continuation-passing style and running it under a trampoline. The trampoline is
installed **per effectful function**: each function whose body can perform emits

```js
function f(args) { return _trampoline(function() { /* CPS body */ }); }
```

(`lib/js_codegen.ml:1571`). The continuation a `perform` captures lives *inside
that function's own trampoline*. When `f` is the callback handed to `apply`
(itself just `f(x)` from JS's view), `apply` calls it, gets a plain value back,
and returns — the captured continuation never reaches past the lambda's
trampoline into `apply`'s caller or the rest of the `handle` body. A single
trivial resume happens to survive (the arm resumes immediately with a value),
but **multishot resume, and any resume whose continuation must include code
after the HOF call, silently loses those frames.**

This is **BUG-11's general case**. It is emit-js (and therefore playground)
only; native, the OCaml VM, and the oracle are correct. The differential fuzzer
documents the boundary by *avoiding* it: `diff_test/generator.ml` refuses to put
a `perform` inside any `List.map` / `List.filter` / `List.fold` / immediately-
applied-lambda callback (the "AVOIDANCE (bugs/BUG-11)" comments). The for-in
slice was already closed by giving for-in loops a native CPS lowering
(`compile_forloop_cps`); what remains is the **general** case: arbitrary
user-or-stdlib HOFs.

### Why this is worth doing

- **Correctness.** It is the last known semantic divergence in the effect
  system. Closing it lets the fuzzer drop its avoidance ledger for effectful
  callbacks — turning a class of programs the generator must dodge into a class
  it differentially tests.
- **Performance.** The fix is the *selective* part: pure code keeps paying
  nothing, effectful code pays only for the continuation it actually needs. The
  same property could later replace fibers in the native/VM full-handler path
  (the "no-fiber general lowering" idea), though that is out of scope here (§8).

---

## 2. The constraint that picks the solution

Four lowerings could give emit-js cross-HOF continuations. Three are ruled out
by hard requirements; one survives.

| Approach | Ruled out by |
|---|---|
| **JS generators / `async`** (`perform` = `yield`) | **Multishot.** A generator object cannot be cloned, so `copy_continuation` + a second `resume` is unimplementable. Generators are also one suspension point per frame — every frame on the path would have to be a generator anyway (the same transitive problem as CPS, minus the multishot ability). |
| **Whole-program CPS** (compile *everything* in CPS) | **Performance.** Pure code — the overwhelming majority — would pay continuation-threading cost for nothing. This is exactly what today's direct-style-by-default backend exists to avoid. |
| **A userland fiber/stack machine in JS** | **Complexity + speed.** It re-implements the VM in JS; the JS VM was just retired ([#13c]) for being slower and a second thing to maintain. |
| **Selective CPS** (CPS only where effects flow; direct elsewhere) | — survives — |

Selective CPS is forced by the intersection of two requirements: continuations
must be **multishot and copyable** (so they must be reified as re-invocable JS
closures — CPS, not generators), and **pure code must stay direct** (so the CPS
must be *selective*, applied only along paths effects actually travel). CPS
closures are multishot by construction (a JS closure can be called any number of
times), which is precisely why the current backend already supports
`copy_continuation` for *syntactically visible* effects. The work is to extend
that same mechanism **across call boundaries**, including opaque HOFs.

---

## 3. The new shared IR property: `needs_cps`

The decision "does a continuation ever need to cross this function?" is a pure
function of the IR — specifically of the **effect row** on the function's type
(`Types.eff`; `ir-contract.md` §7). This is the property [#11] should hoist into
a shared analysis (`Ir_analysis`), because it is computed from the typed AST and
is backend-independent even though emit-js is its first (and, initially, only)
consumer.

**Definition.** A function value needs the CPS calling convention iff its
**latent effect row is non-empty** — i.e. calling it can, dynamically, reach a
`perform`. Concretely, for a function whose type is `τ₁ -[ρ]-> τ₂`:

```
needs_cps(f)  ⟺  ρ is not EffEmpty
```

after `Types.repr` resolution. Three cases:

1. **`ρ = EffEmpty` (pure).** A continuation can never pass through `f`: its
   body cannot perform and it cannot call an effectful callback (it has none in
   its row). Compile direct-style, as today. No `k`.
2. **`ρ` a concrete non-empty row** (e.g. `{op}`). `f` can perform, directly or
   transitively. It needs a CPS variant.
3. **`ρ` an effect *variable*** (effect-polymorphic, e.g.
   `List.map : ('a -[e]-> 'b) -> 'a list -[e]-> 'b list`). `f`'s
   effectfulness is decided **at each call site** by what `e` instantiates to.
   It needs *both* variants (§5).

This is intentionally the *same* question the handler classifier already answers
locally for handler arms, lifted to whole functions. The classifier
(`THOpTry`/`THOpProvide`/`THOp`, `ir-contract.md` §4.2) decides how a *handler*
captures; `needs_cps` decides which *functions on the path to a perform* must
thread the capture. They compose: try/provide handlers still reify no
continuation; only the **full (`THOp`)** class needs a real continuation, and
`needs_cps` is what carries that continuation across HOF frames to reach it.

**Why it belongs in `Ir_analysis`, not js_codegen.** It is defined over
`Types.eff`, has no JS in it, and is exactly the kind of "computed once, read by
backends" property [#10]'s contract promised. emit-js reads it to pick a calling
convention; a future native no-fiber path (§8) would read the identical
property. Native and the VM ignore it today (fibers don't need it) — which is
fine: a shared property may have one consumer now and more later.

---

## 4. The calling convention

Two conventions coexist; `needs_cps` selects between them per function and per
call site.

### 4.1 Direct (pure) — unchanged

`f(a, b)` returns its value. This is today's default and stays byte-for-byte the
same for every pure function. **No pure code regresses.**

### 4.2 CPS (effectful)

An effectful function gets a variant taking an explicit trailing continuation:

```js
function f$cps(a, b, k) { /* body compiled by compile_cps, tail-calling k */ }
```

- The body is lowered with the existing `compile_cps` transform, but its tail
  positions call **`k(v)`** instead of `return v`.
- `perform op arg` lowers to: invoke the nearest matching handler's arm with
  `arg` and a continuation `k′ = (v) => «rest of this CPS computation» then k`.
  Because every effectful frame threads the *same* `k` lineage, `k′` spans
  **all** CPS frames between the perform and the handler — including HOFs (§5).
- `resume k v` calls the reified continuation closure; multishot = call it
  again; `copy_continuation` returns a fresh one-shot wrapper over the same
  closure with its own `_used` flag (the mechanism already in place — this
  design changes *what* the closure spans, not how resume/copy work).

### 4.3 The boundary: one trampoline, at the top

The crucial change from today: the trampoline moves **out** of each function and
**up** to the effectful entry point (the `handle` site, which is where a CPS
region begins). Instead of N self-contained per-function trampolines that each
sever continuations at their own edge, a CPS region runs under **one**
trampoline, and `f$cps(args, k)` calls thread `k` across every frame within it.
Stack safety is preserved (the single trampoline still bounces); continuations
are preserved (they are plain closures threaded through, not trapped inside a
callee's trampoline).

Direct ↔ CPS transitions:

- **CPS calls pure:** an effectful function calling a pure one just calls it
  directly and continues — a pure call cannot perform, so no `k` is needed
  across it. `let r = pure_g(x) in «k-threaded rest»`.
- **Pure calls CPS:** can only happen at a CPS *region boundary* — i.e. a
  `handle` whose body invokes effectful code. The `handle` lowering opens the
  trampoline and supplies the initial `k` (the handler's return path). Outside
  any handler, an effectful call is a type error (unhandled effect), so this
  transition is always handler-mediated.

---

## 5. Effect-polymorphic HOFs — the crux

`List.map : ('a -[e]-> 'b) -> 'a list -[e]-> 'b list` is the hard case, and the
reason BUG-11 is "general." Its own effect row *is* `e`: it performs nothing
itself, but it is as effectful as the callback it is given.

- Called with a **pure** callback (`e = EffEmpty`): the direct `List.map` is
  correct and must stay — no continuation threads through a pure map.
- Called with an **effectful** callback (`e = {op}`): the continuation captured
  inside the callback must thread through `map`'s recursion/iteration and out
  the other side. `map` itself must be CPS: `List.map$cps(f$cps, xs, k)`, whose
  body calls `f$cps(x, k′)` per element and threads `k` through the recursion.

So an effect-polymorphic function needs **both** variants — *dual compilation* —
and the **call site selects** by the instantiated effect row of the function
argument (available on the typed AST: the argument's `ty` is `τ₁ -[ρ]-> τ₂` with
`ρ` resolved at the call site, `ir-contract.md` §2).

### 5.1 Generating the CPS twin

No HOF needs hand-written CPS code. `List.map`, `fold`, `iter`, `filter` are
ordinary MiniML functions in `stdlib/*.mml`; their CPS twins are produced by the
**same `compile_cps` transform** applied to their bodies, with the callback call
inside compiled as a CPS call (`f$cps(x, k′)`). The recursion in `map` becomes a
CPS recursive call threading `k`. Nothing about HOFs is special — they fall out
of the general rule "compile the CPS twin of any function whose effect row can be
non-empty, and call the twin from CPS context."

### 5.2 On-demand, transitive generation

Emitting a CPS twin of *every* function would roughly double code for
effectful-reachable code and waste it on functions never called in CPS context.
Instead, generate twins **on demand** by worklist:

1. Seed: every `handle` body is a CPS region; the effectful calls directly in it
   need twins of their callees.
2. Closure: generating `f$cps` may call `g$cps`, `h$cps`, … — add each callee
   whose row is non-empty (or polymorphic and instantiated non-empty here) to
   the worklist.
3. Fixpoint: stop when no new twin is requested. Pure functions, and
   effect-polymorphic functions only ever called purely, never get a twin.

This keeps the dual-compilation cost proportional to code actually reachable
under a handler.

---

## 6. How this composes with what already works

This design is **layered under** the existing handler classification, not a
replacement for it. The fast paths are untouched:

| Handler class | Continuation reified? | Lowering (unchanged) |
|---|---|---|
| `THOpTry` (non-resuming) | no | catch / early exit |
| `THOpProvide` (tail-resume) | no | inline; run arm on current frame |
| `THOp` (general) | **yes** | **selective CPS (this doc) on emit-js; fibers on native/VM** |

Only the `THOp` class needs a real continuation, and only emit-js needs help
carrying it across HOF frames. So selective CPS activates exactly when: a full
handler is in scope **and** the perform is reached through a function call whose
row is non-empty. Programs with only try/provide handlers, or with performs
syntactically visible in the handler body (no intervening effectful call), are
already correct and stay on their current paths.

For-in loops are the precedent that already works: `compile_forloop_cps` gives a
for-in body under a handler a native CPS loop instead of an opaque fold call —
exactly this design, specialized to the one HOF the compiler knew about. This
generalizes that specialization to all HOFs via the calling convention rather
than per-HOF special-casing, and the for-in path can eventually be re-expressed
as an instance of it.

---

## 7. Implementation plan (gated increments)

Each step is independently gated (`make check` green) and independently
valuable. Stop-anywhere ordering, smallest first:

1. **[#11] Hoist `needs_cps`.** Add `Ir_analysis` with the effect-row predicate
   (§3) and the existing pure post-typecheck analyses it depends on. js_codegen
   consumes `Ir_analysis.needs_cps`. No behavior change yet — pure refactor
   establishing the property. *(This is the concrete, non-speculative content of
   [#11], now that [#12]'s design names exactly what it must compute.)*
2. **Thread `k` across direct effectful calls.** Make an effectful function call
   in CPS context call the callee's CPS twin with the current continuation,
   instead of wrapping the callee in its own trampoline. Move the trampoline to
   the handler boundary. Re-enable the fuzzer's *immediately-applied-lambda*
   avoidance (`gen_int_lambda_app`) — the simplest cross-frame case.
3. **Dual-compile effect-polymorphic HOFs + call-site selection (§5).** The twin
   worklist; select direct vs `$cps` by the argument's instantiated row.
   Re-enable the `List.map` / `List.filter` / `List.fold` avoidance entries one
   at a time, each behind a green fuzzer run.
4. **User-defined `Iter` instances.** The for-in fallback that currently drops to
   an opaque fold (`js_codegen.ml:3099` "the remaining BUG-11 gap") routes
   through the same CPS-twin mechanism. Closes the for-in fallback.
5. **Retire the avoidance ledger.** With all entries re-enabled, delete the
   BUG-11 avoidance comments in `generator.ml`; the fuzzer now differentially
   tests effectful callbacks. Add explicit `effects.tests` cases (below).

After step 5, BUG-11 has no remaining general case and the tracker item closes.

---

## 8. Scope and non-goals

- **emit-js / playground only.** Native, the OCaml VM, and the oracle are
  already correct (fibers / direct recursion) and are **not touched**. This is a
  single-backend lowering change behind a backend-independent IR property.
- **Not the native no-fiber path.** The same `needs_cps` property could later
  drive a CPS (no-fiber) lowering of native's full-handler class, for the perf
  reasons in the handler-perf arc. That is a *possible future second consumer*
  of the property, explicitly **out of scope** here: native full handlers are
  correct and, since the #15 pooled-fiber work, fast. Don't gate this design on
  it.
- **Not a re-architecture of try/provide.** Those classes reify no continuation
  and are already optimal on every backend (`ir-contract.md` §4.2). Selective
  CPS is strictly about the `THOp` class on emit-js.
- **The "canonical per-class lowering" framing of [#12]** (tracker wording) is
  *already substantially realized*: the handler **classification** is a shared
  IR property all backends read. What this design adds is the missing
  **function-level** property (`needs_cps`) and emit-js's primitive for the full
  class. The remaining cross-backend consolidation (factoring each backend's
  try/provide/full emit into a shared driver that calls per-backend primitives)
  is a robustness refactor with no behavior change and no open bug — worthwhile,
  but lower priority than closing BUG-11, and tracked separately.

---

## 9. Risks and open questions

- **Arity protocol.** A CPS twin has arity *n+1* (the trailing `k`). The runtime
  `_call`/`_partial` arity helpers, and `function_arities`, must treat
  `f$cps` as its own arity. Partial application of an effectful function
  (`let g = f a in g$cps b k`?) needs care: partials must remember they are CPS.
  This is the most likely source of subtle bugs — design the twin's arity
  bookkeeping explicitly before step 2.
- **Mutual recursion through HOFs.** A worklist over twins must handle cycles
  (`f$cps` → `g$cps` → `f$cps`); standard fixpoint with a "twin already
  requested" set.
- **Code size.** Dual compilation grows output for effectful-reachable code.
  On-demand generation (§5.2) bounds it to reachable code, but worst-case
  (everything under a handler) is ~2×. Acceptable for correctness; measure on
  the self-hosted compiler (the largest emit-js program).
- **First-class effectful functions stored in data.** If an effectful function
  is put in a list/record and called later, the call site may not statically
  know it is effectful. Resolution: the *value's type* still carries the row, so
  the call site's `ty` decides; a function stored at an effect-polymorphic type
  and later called in CPS context requests the twin. Verify the typed AST
  preserves enough row information at indirect call sites — if not, the
  conservative fallback is to compile such calls in CPS (correct, slightly less
  selective).
- **Interaction with `copy_continuation`'s used-flag** across HOF frames: the
  one-shot guard must sit on the reified `k`, which now spans more frames —
  confirm the existing `_used` wrapper still wraps the *outermost* continuation,
  not a per-frame fragment.

---

## 10. Test plan

- **Differential fuzzer is the oracle.** Each re-enabled avoidance entry (§7
  steps 2–4) must survive a full fuzz run (`make test-fuzz`, then a larger
  `make fuzz COUNT=…`) with emit-js agreeing with oracle/VM. A divergence is a
  real bug, reproducible by seed.
- **Targeted `effects.tests` cases**, mirroring the constructs the ledger
  avoided, each exercising a *non-trivial* resume (so the trampoline boundary
  actually matters — unlike the existing `apply` test whose arm resumes with a
  constant):
  - multishot `resume` inside a `List.map` callback (the continuation must
    re-run the rest of the map *and* the code after it);
  - `copy_continuation` of a continuation captured inside a HOF callback, then
    two resumes;
  - an effectful callback to a *user-defined* HOF (not stdlib) and to a
    user-defined `Iter` instance;
  - nested HOFs (`List.map` of a function that itself folds with an effectful
    callback) to stress transitive twin generation.
- **No new skip markers.** The whole point is to *remove* avoidance; any test
  that must be skipped on emit-js after this work indicates the design is
  incomplete, not that a skip is warranted.

---

## 11. Relationship to the other docs

| Doc | Role |
|---|---|
| [`semantics.md`](semantics.md) | What effects *mean* (the oracle). Unchanged by this design. |
| [`ir-contract.md`](ir-contract.md) | The IR these lowerings consume; §4.2 handler classes, §7 effect rows. |
| [`effects.md`](effects.md) | The effect system for language users. |
| [`js-backend.md`](js-backend.md) | The emit-js backend; this design refines its CPS section. |
| **this doc** | How emit-js should lower the full-handler class across HOF boundaries, and the `needs_cps` IR property that drives it. |

Authority order is unchanged: oracle → semantics.md → ir-contract.md → this
design → backend docs.
