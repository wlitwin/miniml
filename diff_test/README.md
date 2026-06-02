# Differential Runner

Runs one MiniML program on multiple backends and reports whether they agree on
its observable behavior. **No expected value is needed** — agreement across
independent implementations is the test. Any disagreement is a bug somewhere:
in a backend lowering, in the oracle (the spec), or a divergence the language
definition has to settle.

This is the comparison engine for differential fuzzing (productionization
roadmap, Phase 2). The fuzzer generates programs; this decides whether the
backends agree on them.

## Usage

```bash
# Do all backends agree on this program?
make diff FILE=prog.mml

# Restrict backends / change the per-backend timeout
make diff FILE=prog.mml BACKENDS=oracle,vm,emit-js TIMEOUT=5

# Smoke test (also part of `make check`): agreement on the smoke programs,
# and disagreement DETECTION on a deliberately nondeterministic one
make test-diff

# Run every test case's source from .tests files (ignores their expected
# values — agreement is the check)
dune exec diff_test/diff_runner.exe -- --tests cross_test/tests/effects.tests
```

## Backends

| Name | What runs |
|------|-----------|
| `oracle` | lib/oracle.ml — the reference interpreter (the executable spec); always listed first so reports read "X differs from the oracle" |
| `vm` | bytecode on the OCaml VM |
| `emit-js` | js_codegen output on node |
| `native` | LLVM IR → clang → binary |

`--fast` = oracle, vm, emit-js (skips native's ~1s clang invocation per program
— the tier for high-throughput fuzzing).

## How results are compared

Each backend's run is normalized to a **result category**:

- `output: <s>` — the program's observable output (prints + final value, the
  same notion the cross-test runners check). Two outputs agree iff the strings
  are identical.
- `rejected` — frontend rejection (lex/parse/type error). Unanimous by
  construction since all backends share the frontend.
- `runtime-error` — the program ran and failed. Error *messages* are not
  compared (backends word them differently); the *category* is.
- `backend-error` — backend-specific compilation failure. Always a finding when
  other backends run the program.
- `timeout` / `crash` — resource limits, signals, infinite loops.

The verdict is category-based agreement; for `output`, string equality.

## Isolation

Every backend run happens in a **forked subprocess** with a wall-clock timeout:
a hung backend can't hang the runner, a crashing backend (segfault, stack
overflow) is reported as `crash` instead of taking the runner down, and the
parent's interpreter state stays pristine (stdlib is compiled once and
inherited copy-on-write — a full 4-backend run costs ~2s, dominated by clang).

## The fuzzer

```bash
# 100 random programs through the fast tier (oracle, vm, emit-js)
make fuzz

# Reproducible run / replay a specific finding
make fuzz COUNT=50 SEED=1000
make fuzz COUNT=1 SEED=1014        # regenerates exactly that program

# All four backends (native adds ~1s/program)
make fuzz FULL=1
```

`diff_test/generator.ml` synthesizes random well-typed MiniML programs:
type-directed (so they typecheck by construction), terminating by construction
(bounded recursion/loops only), seeded (same seed → same program, byte for
byte), and biased toward the historically bug-rich areas — handlers with
non-tail/multishot resume, control-flow escape, mutable state across resumes,
numeric formatting, multi-print framing, pattern matching with guards.

Disagreeing programs are saved to `fuzz_failures/seed_N.mml` with a
`.verdict.txt` report. Known-divergent constructs are excluded from generation
so every disagreement is a new finding; each exclusion references the bug that
justifies it (see generator.ml comments: BUG-3, BUG-4, BUG-5).

The fuzzer is not in the `make check` gate yet: it finds the still-open BUG-6
in any sizable run. Gate integration (a fixed-seed regression run) lands once
BUG-6 is fixed.

## The shrinker

```bash
# Minimize a disagreeing program; print a ready-to-paste .tests block
make shrink FILE=fuzz_failures/seed_1014.mml TESTNAME="non-tail perform repro"

# Fuzz with automatic minimization of every finding
make fuzz COUNT=100 SHRINK=1
```

`diff_test/shrinker.ml` delta-debugs the source text: removes declarations,
removes handler/match arms, replaces parenthesized subexpressions with
literals — keeping any change that preserves the exact DISAGREEMENT SIGNATURE
(same dissenting backends, same result categories, everyone else still
agreeing), iterating to a fixpoint. Typical result: a 2.5KB fuzz program
shrinks to a ~10-line repro in a few seconds (most candidates are ill-typed
and rejected in-process before any backend runs).

The signature check runs ALL backends of the original run — restricting it to
the dissenters is unsound (the shrink can drift onto a different bug; this was
learned the hard way: deleting a return arm turned a BUG-6 repro into a BUG-4
one).

Harvest workflow: `make shrink FILE=... TESTNAME=...` prints a .tests block
with the reference's expected value and TODO skip markers for each dissenting
backend; assign the tracked bug number and paste it into cross_test/tests/.
Two BUG-6 repros in effects.tests were landed exactly this way.

## Track record

Found by the differential runner in its first hour (2026-06-01), before any
fuzzing existed:

1. emit-js `print` wrote no trailing newline (FIXED; locked by core.tests +
   smoke/print_value.mml). Invisible to cross tests because the format
   couldn't express multi-line expected output.
2. Float formatting diverges on JS backends (%g vs toString) — FIXED (BUG-1);
   locked by float_format.tests.
3. Native missing `Sys.time` — tracked as BUG-2.

Found by the fuzzer in its first 50 generated programs (2026-06-01):

4. `handle` without a return arm: 3-way divergence — VM rejects it, native and
   emit-js run it with different semantics (BUG-4).
5. Typeclass instance resolution fails for multishot `+` when the return arm
   is a match expression — compilers reject, oracle runs (BUG-5).
6. emit-js: wrong handle result for non-tail performs / multishot resumes
   (BUG-6) — the historical CPS-lowering bug class.
7. VM: "EXIT_LOOP: no control entry" crash — loop-control state not copied
   with continuations, breaks multishot resume inside for-in loops (BUG-7).
