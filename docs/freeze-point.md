# The Path-B Freeze Point

This document marks the milestone where MiniML became its own production
toolchain (roadmap #14 "Path B", #16), and fixes the roles of the two compilers
going forward. It is the prose counterpart of the decision in
`roadmap/phase-4-self-host`.

## Status: Path B is reached

The shipping artifact is now MiniML, compiling itself, natively:

- **Self-host fixpoint.** The native `mmlc` (the self-host compiler built as a
  native binary) compiles its own ~26k-line source into `mmlc2`; gen-2 and gen-3
  IR are byte-identical. The compiler reproduces itself.
- **Full parity.** The self-host compiler agrees with the OCaml reference on the
  entire cross-test corpus across every backend (`make check`: 2242 tests × VM,
  emit-js, native, oracle, parity, playground), and `ir-parity` confirms the two
  agree at the *lowered-IR* level, not just on output values.
- **A real toolchain.** `self_host/mml.mml` is a native `mml` binary —
  `run` / `build` / `check` / `fmt` / `get` — written in MiniML over the
  reusable compiler driver (`self_host/driver.mml`). `mml fmt` is byte-identical
  to the OCaml reference formatter.
- **MiniML-native tooling.** The formatter, package-manager data layer
  (semver/sumfile/manifest/deps), git/fs fetch layer, and project/build system
  all run as MiniML, on emit-js and native, gated.

In short: **what ships is MiniML.** The capability question is settled.

## Roles after the freeze point

| Component | Role |
|---|---|
| `self_host/` + the native `mml` | **Production toolchain.** What users run; what ships. |
| `lib/oracle.ml` | **The executable spec.** Authoritative for evaluation semantics (`docs/semantics.md` is its prose form). New language semantics land here first. |
| `lib/` reference compiler | **Bootstrap + dev source + differential oracle.** It (a) bootstraps the self-host, (b) is the source the self-host is mechanically translated from (`make translate-all`), and (c) is the independent implementation the gate diffs against. |
| `tools/ocaml_to_mml` | **The projection.** Keeps `self_host/` faithful to `lib/` by construction. |

The "freeze" is therefore not a deletion. It is a **commitment of intent**: the
OCaml reference is no longer an end in itself — it exists to *specify*,
*bootstrap*, and *cross-check* the MiniML production compiler. No feature is
"done" until it exists in the production (MiniML) toolchain.

## What "frozen" means, and how it is enforced

- **No reference-only language features.** Anything that changes the language
  must reach the self-host. Because the self-host is translated from `lib/`, this
  is automatic for compiler changes; the gate makes it non-optional:
  - `parity` — self-host bytecode vs the OCaml VM over the corpus.
  - `ir-parity` — the *lowered IR* of both compilers, diffed over the whole
    corpus; catches value-invisible faithfulness drift.
  - `oracle` — every backend checked against the executable spec.
  - `native-selfhost-build` — the self-host drives clang/MPS to a real binary.
- **The hand-maintained surface is small and gated.** `self_host/main.mml`,
  `self_host/driver.mml`, and `self_host/mml.mml` are written by hand (an entry
  and a driver are inherently per-host); everything else under `self_host/` is
  generated. Each hand-written tool has a `*-selfhost` gate stage compiling it on
  emit-js and native, and the formatter additionally has byte-parity
  (`fmt-selfhost-parity`).

Nothing merges unless `make check` is green, so the two compilers cannot silently
diverge.

## The bootstrap chain (today)

```
OCaml reference (lib/, bin/main.exe)
   │  make translate-all        (ocaml_to_mml: lib/*.ml → self_host/*.mml)
   ▼
self_host/ (MiniML)
   │  bin/main.exe --emit-json  → js/compiler.json   (self-host on the OCaml VM)
   │  bin/main.exe --emit-native self_host concat → mmlc   (native self-host)
   ▼
mmlc / mml      (the production compiler + toolchain, native)
   │  mmlc compiles its own source → mmlc2 ; gen2 == gen3   (fixpoint)
```

The OCaml reference is the seed. Everything downstream is MiniML.

## The cutover (TAKEN — soft cut, 2026-06-13)

The canonical Path-B end state — the one Go reached after its own C→Go
translator — is to **stop translating and maintain the compiler directly in
MiniML**, demoting the OCaml reference to bootstrap-and-spec only. After the two
gating prerequisites below were demonstrated (the independent test net and the
OCaml-free bootstrap), we took the cutover as a **soft cut**:

- `self_host/` is now the **source of truth** — edit it directly. `translate-all`
  is removed from the build (the self-host-compile / playground targets compile
  `self_host/` as-is); it survives only as an explicit, deliberately-run
  *re-sync-from-reference* tool that OVERWRITES the translated modules from the
  frozen `lib/`.
- The `lib/` reference COMPILER is **frozen** (the oracle `lib/oracle.ml` stays
  live as the spec; `lib/` also still bootstraps and cross-checks).
- The differential gates (`parity`/`ir-parity`/`translate`) are **kept, not
  deleted** — they are green at the cut and now serve as a *frozen-reference
  regression net*: free regression detection that stays valid until a deliberate
  `self_host/` change first legitimately diverges from the frozen `lib/`, at
  which point the diverged stage is retired. Keeping the net while it costs
  nothing is strictly better than destroying it at the cut.
- The forward authority is the **self-host-direct** verification:
  `make test-selfhost-native` (corpus through `mmlc`, expected values),
  `make bootstrap-ocaml-free` (node-only rebuild), the `playground` stage
  (self-host emit-js), and `oracle` + `fuzz`.

This was a soft cut precisely because the cost of a hard cut is not capability —
it is the **differential safety net**, and the soft cut keeps that net for free
until divergence forces each piece off. The prerequisites that had to be paid
first:

Prerequisites before pulling the trigger:

1. **An independent test story that does not lean on translate-parity.** Today
   `parity`/`ir-parity`/`translate` derive their authority from "the self-host
   matches the reference." Post-cutover that is gone. We need confidence from:
   the cross-tests' *expected values* (they assert concrete results, not just
   agreement), the `oracle` as the semantic spec, and cross-backend agreement
   *within* the self-host (emit-js vs native vs the bytecode VM).

   **Status (in progress).** The independent net's three self-host legs are now
   demonstrated against the *expected values* of the whole corpus, none of them
   diffing against the reference:
   - **emit-js** — the `playground` gate stage (self-host compiler → JS → node),
     2242/2242.
   - **bytecode** — `parity` runs the self-host bytecode over the corpus
     (today it also cross-checks the reference VM, but the corpus's expected
     values stand on their own).
   - **native** — NEW: `make test-selfhost-native` builds `mmlc` (the self-host
     compiler as a native binary) and runs the corpus *through it* (the
     production native path), checking expected values — **2139 passed, 0 failed**
     (the 103 skipped are type-error tests; type-error detection is the shared
     frontend, covered by the legs above + `oracle`). Implemented as an
     `MMLC_BIN` mode in `native_test/runner.ml`.

   Plus the `oracle` (executable spec) over the corpus and `fuzz` (differential
   over generated programs). Remaining for this prerequisite: a periodic
   self-host-native run in CI (it is heavy — clang per test — so it is on-demand,
   not in the fast `make check`), and a written accounting of the one residual
   class translate-parity uniquely caught (value-*invisible* self-host codegen
   drift not exercised by the corpus) — mitigated by `fuzz` + corpus breadth.
2. **Bootstrap-seed management.** Once `self_host/` is edited past what the
   frozen OCaml parser accepts, it can only be built by a *prior* `mml`. Decide
   the seed: a checked-in `compiler.json` / `mmlc` binary, a reproducible
   "build the seed from the last reference-compatible revision" recipe, or a
   staged bootstrap. This is the operational crux.

   **Status (in progress).** An **OCaml-free bootstrap works today**:
   `make bootstrap-ocaml-free` builds the production compiler `mmlc` using only
   **node + the checked-in emit-js seed (`js/compiler_native.js`) + clang** — no
   OCaml. The `mmlc` it produces emits native IR **byte-identical** to the
   OCaml-VM-hosted build, so the self-host compiler is host-independent (this is
   what the emit-js dynamic-shift fix unblocked — see the commit log). The seed
   regenerates itself by the previous seed
   (`node js/compiler_native.js --emit-js <self-host> -o js/compiler_native.js`),
   so the loop needs no OCaml. CAVEAT: that emit-js seed is not yet a
   *byte-identical* fixpoint — successive generations differ by a handful of
   bytes, traced entirely to **non-ASCII characters (em-dashes) in prelude
   comments and a few error-message strings**, which the tracked emit-js UTF-16
   string gap degrades one step per generation. It is cosmetic — the compiler is
   fully functional at every generation — but a clean reproducible-build fixpoint
   wants either that UTF-16 gap fixed or the self-host sources + prelude
   ASCII-ified. The native `mmlc` path already *is* a clean fixpoint
   (gen2 == gen3, byte-identical IR). Decide the canonical seed artifact
   (checked-in `compiler_native.js`, regenerated via the previous seed) and add a
   periodic CI check of `make bootstrap-ocaml-free`.
3. **The oracle stays.** `lib/oracle.ml` (and the `lib/` frontend types it
   shares) remain the spec — the cutover demotes the reference *backend/VM as the
   dev source*, it does not delete `lib/`. Keep `lib/` buildable as the oracle.
4. **Compiler-dev ergonomics in MiniML.** Editing the compiler in `self_host/`
   directly wants the MiniML LSP (hover/go-to-def/diagnostics) at a level that
   makes it as pleasant as the OCaml/merlin workflow that got us here.
5. **Gate rework.** `translate` and `ir-parity` lose meaning (no projection to
   verify); `parity` becomes "two independently-maintained implementations agree"
   (still valuable if `lib/` is kept) or is retired. Re-architect `make check`
   around the self-host-direct model before, not after.

**Decision criterion.** Cut over when (1)–(2) are demonstrably in place — i.e.
when retiring the translate-parity net provably does not lower coverage, and the
bootstrap is reproducible from a trusted seed. Until then, the translate workflow
is a feature, not scaffolding: it is a faithfulness guarantee and keeps
OCaml-grade tooling under the compiler. The freeze point above is the honest
milestone; the cutover is the next deliberate phase.
