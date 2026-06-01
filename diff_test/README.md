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

## Track record

Found in its first hour (2026-06-01), before any fuzzing existed:

1. emit-js `print` wrote no trailing newline (fixed; locked by core.tests +
   smoke/print_value.mml). Invisible to cross tests because the format
   couldn't express multi-line expected output.
2. Float formatting diverges on JS backends (%g vs toString) — tracked as
   BUG-1.
3. Native missing `Sys.time` — tracked as BUG-2.
