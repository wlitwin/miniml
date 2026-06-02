# MiniML Operational Semantics

This document is the prose form of MiniML's evaluation semantics. The
**executable** form is the reference interpreter (the *oracle*) in
`lib/oracle.ml`; every rule here is implemented there and tested against every
backend by `make test-oracle`. Where this document and the oracle disagree, the
oracle is authoritative. Where a backend and the oracle disagree, the backend
has a bug.

The semantics are described for the typed AST as it exists **after**
typechecking and constraint transformation but **before** handler
classification, match-tree lowering, and optimization. Backends implement
*lowerings* of these semantics; nothing below depends on any lowering decision
(fibers, CPS, decision trees, ...).

Audience: backend authors, and anyone deciding "what is the right behavior?"
when implementations disagree.

---

## 1. Values and the heap

A MiniML value is one of:

| Value | Notes |
|---|---|
| `int`, `float`, `bool`, `string`, `byte`, `rune`, `unit` | immutable scalars |
| tuple | immutable, fixed length |
| list | a chain of **cons cells** ending in nil |
| record | named fields; fields are mutable slots (`<-` assignment) |
| variant | constructor name + tag + optional payload |
| array | fixed length, mutable elements |
| closure | function value: code + captured environment |
| continuation | a suspended computation captured by an effect handler |
| ref | a single mutable cell |

**The heap is a graph, not a tree.** Cons cells and variant payloads are heap
objects that can (only) become cyclic through recursive value bindings
(§9.2). Records, arrays, and refs are mutable through ordinary language
features. Tuples and scalars are immutable.

**Constructor tags are declaration indexes.** The runtime tag of a variant
constructor is its zero-based position within its type declaration
(`type t = A | B of int | C` gives A=0, B=1, C=2). This is a cross-backend
contract: tag-based hashing, the VM's `JUMP_TABLE` dispatch, and native's
header words all rely on it.

**Newtype constructors do not exist at runtime.** A constructor declared with
`newtype` is erased at construction (the payload IS the value) and
transparently unwrapped in patterns — including when the wrapped value is
itself a variant (`Wrap (Some x)` matches the value `Some 42` directly).

**Variant display names are unqualified**: `Result.Ok 42` prints as `Ok 42`.

**Floats stringify via C's `%g` with precision 6** (six significant digits;
scientific notation when the decimal exponent is < -4 or ≥ 6; trailing zeros
stripped; two-digit exponent: `1.78034e+09`). There are exactly two variants,
and they differ only for values whose `%g` form has no `.`/`e` (whole floats,
inf, nan):

| Variant | Used by | `3.0` | `inf` |
|---|---|---|---|
| **bare** | `show`, `string_of_float`, `$"{f}"` interpolation | `3` | `inf` |
| **display** | final values, `print`, values nested in structures | `3.` | `inf` |

Display appends a `.` only when the result could otherwise be read as an int —
so `inf`/`nan` never get one. `print [3.0]` is `[3.]` (display) but
`show [3.0]` is `[3]` (bare): `print` of a structure formats with display
semantics, it is *not* `print (show ...)`. Locked by
cross_test/tests/float_format.tests on every backend.

---

## 2. The evaluation model

Evaluation is strict (call-by-value), left-to-right, big-step. Evaluating an
expression produces an **outcome**:

- **Done v** — the expression produced value `v`.
- **Perform op arg k** — evaluation stopped at `perform op arg`; `k` is the
  rest of the computation up to the nearest enclosing handler for `op`
  (the continuation).
- **Control c** — non-local control flow (`return` / `break` / `continue`)
  travelling to the boundary that owns it.

Every compound expression threads outcomes through its sub-expressions: a
`Perform` or control signal in a sub-expression suspends/aborts the whole
expression, extending the continuation so that resuming re-runs the rest of
it. This single rule is what makes effect handlers and non-local control
compose with everything else.

**Evaluation order** is left-to-right everywhere: function before argument,
tuple/list/record/array elements in source order, left operand before right.

---

## 3. Scoping: locals and globals

There are exactly two kinds of bindings:

**Local bindings** (`let` inside an expression, function parameters, pattern
bindings) are **lexically scoped and early-bound**: a closure captures the
bindings visible at its creation site, and those captures never change
(except via `let mut` assignment, which mutates the binding's cell).

**Top-level bindings** (declarations at program/module top level) live in a
single mutable **global namespace** and are **late-bound**: references are
resolved at *use* time, not closure-creation time. This is the semantics of
every backend's resolve-global-by-slot mechanism, and it has two observable
consequences:

1. *Self-reference through globals just works.* An instance dictionary's
   methods can call the dictionary's own name (recursive class methods), and a
   top-level `let rec f` needs no special mechanism — the body's reference to
   `f` resolves when `f` is first called, by which time it is defined.
2. *A definition may reference a later definition*, as long as the reference
   is only **followed** (called/read) after that later definition has been
   evaluated. Reading a global before its definition is a runtime error
   ("unbound variable").

Each global *name* has its own binding. **Redefining a name rebinds that name
only** — it never mutates what other names (e.g. `open` aliases, §14) resolve
to:

```
module M = pub let x = 10 end;;
open M;;        -- `x` now resolves to M.x's binding
let x = 20;;    -- rebinds `x`; M.x is untouched
x + M.x         -- 30, not 40
```

Locals shadow globals. Module members are globals under their qualified names
(`List.fold`); `open M` adds aliases (§14).

---

## 4. Functions and application

`fn x -> body` evaluates to a closure capturing the current local environment.
Multi-parameter functions are nested single-parameter closures (currying);
partial application is therefore free.

Applying a closure binds the parameter as a new local and evaluates the body.
Two boundary rules apply at every application (see §5 for the signals):

- A function whose source **lexically contains `return`** catches the
  `return` signal: `return v` makes that application produce `v`.
- A **fold callback** (a for-loop body, §6.2) catches the fold-`continue`
  signal.

Applying a non-function is a runtime error. Applying a continuation resumes it
(§12).

---

## 5. Non-local control: `return`, `break`, `continue`

These are **signals** that travel outward through enclosing expressions until
caught at the boundary that owns them:

| Signal | Caught by |
|---|---|
| `return v` | the nearest enclosing **function application whose function lexically contains `return`** |
| `break v` | the nearest enclosing loop |
| `continue` (while loop) | the nearest enclosing while loop — runs the step expression, then re-tests the condition |
| `continue` (for-in loop) | the current iteration's fold callback (§6.2) |

The "lexically contains `return`" rule is the load-bearing one: stdlib
functions (whose source has no `return`) pass the signal through. That is why
`return` inside a `for x in xs do ... end` body — which is desugared into a
callback passed to a stdlib fold — returns from the **user's enclosing
function**, not from the fold or the callback.

Signals pass through handler frames unchanged (a `return` inside a `try/with`
body returns from the enclosing function, unwinding the handler).

---

## 6. Loops

### 6.1 `while` (and C-style `for`)

The condition is evaluated each iteration; `true` runs the body, `false` ends
the loop with value `unit`. `break v` ends the loop with value `v`.
`continue` skips to the next iteration — running the step expression first,
if the loop has one. An effect performed in the body suspends the whole loop;
resuming continues the iteration and then keeps looping.

The loop is part of the continuation of **every** perform inside it, not just
the first one: when a resumed iteration performs again (a second perform in
the same iteration, or a later iteration's), that new perform's continuation
still includes the rest of the loop. Consequently deep-handler arm-value
chaining (§11) works across iterations: for a loop that performs once per
iteration over n iterations with arm `op arg k -> (resume k v) + c`, the
handle result is `final_body_value + n*c` — each nested arm invocation's
value becomes the previous `resume`'s value. (BUG-13 regression: the oracle
used to drop the rest of the loop from the continuation of performs that
surfaced *during* a resumption.)

### 6.2 `for x in collection` loops

For-in loops are desugared (by the typechecker) into an application of the
collection's `Iter` instance fold, with the loop body as a callback closure.
The semantics that make this transparent to the user:

- Closures appearing as direct arguments in that fold application chain are
  **fold callbacks** (the marking propagates through curried parameters).
- `continue` inside the body produces the fold-continue signal, caught at the
  fold-callback boundary: the callback returns that value for the current
  iteration.
- `break v` inside the body propagates **through** the callback and the
  stdlib fold frames (they don't catch it) up to the for-loop itself, which
  catches it and produces `v`.
- `return v` propagates through everything up to the user's enclosing
  function (§5).

---

## 7. Pattern matching

Match arms are tried top to bottom; the first pattern that matches (and whose
guard, if any, evaluates to `true`) selects the arm. A guard that fails resumes
trying subsequent arms. If no arm matches, it is a runtime error
(non-exhaustive match — the typechecker warns statically, the runtime enforces
dynamically).

Pattern-specific rules:

- **Constructor patterns** compare the final segment of possibly-qualified
  names (`Mod.Ctor` matches `Ctor`).
- **Newtype constructor patterns** are erased: `Wrap p` matches the underlying
  value against `p` directly (§1).
- **Polymorphic variant** values carry a backtick in their name; patterns
  don't. Comparison strips it.
- **Pin patterns** (`^x`) match by structural equality (§13) against the
  current value of the existing binding `x`.
- **Or-patterns** try left then right; both sides must bind the same names.
- **Map patterns** (`#{key: p}`): every listed key must be present in the map
  and its value must match `p`; extra keys are allowed. (Maps are
  erased-newtype association lists at runtime.)
- **Set patterns**: every element pattern must match some element; extra
  elements are allowed.
- **Record patterns** match listed fields only; extra fields are allowed.

---

## 8. Records and polymorphic record update

Record fields preserve **declaration order** for construction and printing.

`{ r with field = v }` (named update) produces a **new** record; the original
is unmodified. Field assignment `r.field <- v` mutates in place.

**Polymorphic record update** (updating a field of a record whose concrete
type isn't statically known) compiles to update-by-evidence-index. The
cross-backend contract: **evidence indexes are positions in the record's
field names sorted alphabetically** — not declaration order. (This contract
is what `js_codegen`'s `Object.keys(r).sort()[idx]` and the typechecker's
record-evidence machinery agree on; any new backend must honor it.)

---

## 9. Recursive bindings

### 9.1 Recursive functions

`let rec f = fn ... -> body` (and mutual recursion via `and`): the function
body's references to `f` resolve to the final closure. Locally, the closure
captures a cell that is filled after the closure is created; at top level,
late-bound globals (§3) make this automatic.

### 9.2 Recursive values

`let rec xs = 1 :: xs` (cyclic lists), `let rec t = Node (1, t)` (cyclic
variants), and their mutual (`and`) forms build **cyclic heap values**. The
semantics is *placeholder + in-place update*:

1. The right-hand side must be **constructive**: its outermost expression
   (looking through `let` and sequencing) is a data constructor — cons,
   tuple, record, variant constructor, or array literal. The typechecker
   rejects non-constructive bindings (`let rec x = x + 1`).
2. A placeholder heap cell of that constructor's shape is allocated and the
   name is bound to it.
3. The right-hand side is evaluated; its references to the name see the
   placeholder.
4. The placeholder's fields are overwritten with the computed value's fields,
   tying the knot.

For mutual recursion, all placeholders are allocated and bound first, then
each binding is evaluated and updated.

The typechecker separately rejects recursive bindings that would require
infinite *types* (recursive tuples and records); cyclic values exist only for
lists and (nominal) variants.

Note: structural operations on cyclic values (printing, equality, conversion)
do not terminate, on every backend. This is by design — the spec is "the walk
diverges", not "cycles are detected".

---

## 10. Operators and typeclass dispatch

Operators on **primitive types** (int/float arithmetic and comparison, string
`^` and comparison, bool logic, int bitwise) have their primitive meaning.
`&&` and `||` short-circuit. Division/modulo by zero is a runtime error.

Operators on **other types** dispatch through the operand's typeclass
instance, selected by the operand's *static* type:

| Operators | Class |
|---|---|
| `+ - * /`, unary `-` | `Num` |
| `= <>` on records *with a custom Eq instance* | `Eq` (otherwise structural §13) |
| `< > <= >=` on records/variants *with an Ord instance* | `Ord` (containers compare structurally) |
| `land lor lxor lsl lsr lnot` on non-int | `Bitwise` |

Constraint transformation rewrites *most* class-method uses into dictionary
accesses before evaluation; the table above is the rule for operator
expressions that survive to runtime (operands whose types have instances but
weren't rewritten — e.g. concrete record types).

`x |> f` is exactly `f x`.

---

## 11. Effects and handlers

This is the heart of the language. The semantics is **deep handlers** with
the textbook formulation:

```
handle BODY with
| return x -> RET
| op arg k -> ARM
...
```

- If BODY completes with value `v`: evaluate RET with `x = v`. If there is no
  return arm, the handle expression produces `v`.
- If BODY performs `op` and this handler has an arm for `op`: evaluate ARM
  with `arg` bound to the performed argument and `k` bound to a
  **continuation** representing "the rest of BODY up to and including this
  handler" (deep semantics: when `k` is resumed, this same handler is back in
  place around the rest of BODY, and BODY's eventual completion goes through
  the return arm).
- If BODY performs an operation this handler does **not** handle: the perform
  passes through to the next enclosing handler, but this handler is
  re-installed around the continuation — so when the outer handler resumes,
  this handler is live again.
- Non-local control signals (§5) pass through handlers unchanged.

Sugared arm forms:

- **Provide** (`op arg -> resume-with-VALUE` style, written without naming
  `k`): equivalent to an arm that immediately resumes with VALUE.
- **Try** (`op arg -> FALLBACK`, non-resuming): the continuation is dropped;
  FALLBACK's value becomes the handle expression's result directly (it does
  *not* go through the return arm).

An effect performed with **no enclosing handler** for it is a runtime error
("Unhandled effect: op").

Top-level declarations cannot perform unhandled effects; `return`/`break`/
`continue` cannot escape to the top level.

---

## 12. Continuations

- Continuations captured by handler arms are **one-shot**: resuming a
  continuation that has already been resumed is a runtime error
  ("continuation already resumed").
- `copy_continuation k` returns a continuation with the **same** resumption
  but a fresh use-flag — the copy can be resumed once more. This is the
  multishot escape hatch.
- **Copy before resuming.** Copying an *already-resumed* continuation is a
  runtime error ("cannot copy an already resumed continuation"): its
  resumption is spent, and a copy would let the same one-shot resumption run
  twice. Beware of evaluation order here — in
  `(resume k a) + (resume (copy_continuation k) b)` the copy is evaluated
  *after* the left operand has consumed `k`, so it is an error; bind the copy
  first (`let k2 = copy_continuation k in ...`).
- **Multishot semantics** (resuming a continuation and its copies multiple
  times): control state is **copied** — each resume re-runs the rest of the
  computation independently. The **heap is shared** — refs, mutable record
  fields, and arrays mutated by one resume are seen by later resumes.
  (Equivalently: continuations close over the heap by reference and over the
  control stack by value.)
- Resuming with `resume k v` makes the suspended `perform` expression
  evaluate to `v`.

---

## 13. Equality, comparison, and identity

**Structural equality** (`=`, `<>`): scalars by value; tuples/lists/arrays/
records element-wise; variants by constructor name then payload; refs by
contents. Closures, builtins, and continuations compare by **identity**
(a function only equals itself).

**Ordering** (`<` etc., and `compare`): scalars by value; tuples/lists/arrays
lexicographically; variants by constructor **name** (alphabetically), then
payload; records field-wise in declaration order. Ordering on functions is an
error.

**Physical equality** (`phys_equal`): identity for boxed values (records,
arrays, cons cells, tuples, variant payloads, refs, strings); value equality
for immediates (int, bool, byte, rune, unit). This mirrors pointer comparison
in every backend.

---

## 14. Modules and `open`

Module member declarations are ordinary top-level bindings under qualified
names (`M.x`). `open M` creates an **alias** for each exported member: the
short name resolves to the qualified name's binding cell *as of the open*, so
mutable module state stays aliased (assigning through one name is visible
through the other). A later **redefinition** of the short name rebinds it and
breaks the association (§3) — it does not write through to the module member.

Type, class, and effect declarations have no runtime content.

---

## 15. Builtins

Builtin functions (externs) are part of the language's *library* semantics,
not its evaluation semantics. Their specification is the OCaml reference
implementations in `lib/interp.ml` / `lib/builtins.ml`; their signatures are
declared once in `stdlib/builtins.mml` (both compilers load it). Every backend
runtime must implement them with identical observable behavior — including
arithmetic edge cases (see the string-hash and `String.to_int` postmortems:
"behavioral divergence" includes integer overflow semantics, not just library
logic).

Builtins observable rules the oracle enforces:

- Builtins are curried like ordinary functions (partial application works).
- `print` carries the `IO` effect.
- Identity-sensitive and mutating builtins (`phys_equal`, `Array.set`,
  `copy_continuation`) operate on the caller's actual values, never copies.

---

## Appendix: how this document relates to the test suite

- `make test-oracle` runs every cross-test against `lib/oracle.ml`. A failure
  means either the oracle (and possibly this document) is wrong, or the test's
  expected value encodes a backend bug.
- `make check` (the pre-merge gate) runs every suite on every backend. The
  oracle is one of the gates: backends and spec cannot drift silently.
- When adding a language feature: implement it in the oracle first, write
  cross-tests against the oracle's behavior, then implement the lowerings.
