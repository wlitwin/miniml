Here's the compilation pipeline:

```
  Source string
      │
      ▼
  ┌──────────┐
  │ 1. LEXER │  lib/lexer.ml
  │          │  string → Token.token list
  └────┬─────┘
       │
       ▼
  ┌───────────┐
  │ 2. PARSER │  lib/parser.ml
  │           │  token list → Ast.program (= Ast.decl list)
  └────┬──────┘
       │
       ▼
  ┌────────────────┐
  │ 3. TYPECHECKER │  lib/typechecker.ml
  │                │  Ast.program → tprogram (= tdecl list of texpr)
  │  3a. synth/    │  Hindley-Milner inference with unification
  │      check     │  Attaches Types.ty to every node
  │                │
  │  3b. fundep    │  improve_fundeps_in_expr (before generalize)
  │      improve   │  apply_fundep_improvement (whole-program post-pass)
  │                │
  │  3c. transform │  transform_constraints
  │      constraints│  Rewrites typeclass calls → dictionary field accesses
  └────┬───────────┘
       │
       ├────────────────────┬──────────────────────┐
       ▼                    ▼                      ▼
  ┌──────────────┐  ┌───────────────┐  ┌────────────────────┐
  │ 4a. COMPILER │  │ 4b. JS CODEGEN│  │ 4c. NATIVE CODEGEN │
  │  (bytecode)  │  │               │  │                    │
  │  lib/compiler│  │  Direct-style │  │  lib/native/       │
  │  .ml         │  │  + CPS/tramp  │  │  codegen.ml        │
  └────┬─────────┘  └───────┬───────┘  └────────┬───────────┘
       │                    │                    │
       ▼                    ▼                    ▼
  ┌──────────────┐  ┌───────────────┐  ┌────────────────────┐
  │ OPTIMIZER    │  │ JS OUTPUT     │  │ NATIVE OUTPUT      │
  │  lib/optimize│  │ Standalone .js│  │ tprogram → LLVM IR │
  │  .ml         │  │ --emit-js     │  │ → clang → binary   │
  └────┬─────────┘  └───────────────┘  │ mmlc / --emit-ir   │
       │                               └────────────────────┘
       ├──────────────────────┐
       ▼                      ▼
  ┌──────────────┐   ┌───────────────┐
  │ SERIALIZE    │   │ VM            │
  │ --emit-json  │   │ lib/vm.ml     │
  │ --emit-binary│   │ (OCaml)       │
  └──────────────┘   └───────────────┘
```

The pipeline branches after typechecking into three backends:

- **Bytecode backend** (`--emit-json`, `--emit-binary`, or default): Compiles to stack-based bytecode, optionally optimized, then either executed directly by the OCaml VM, or serialized to JSON (`--emit-json`) or a compact binary format (`--emit-binary`) for later execution (`--run-json`, `--run-binary`).
- **JS codegen backend** (`--emit-js`): Compiles typed AST directly to standalone JavaScript. Uses direct-style compilation for most code, with CPS transformation and trampolining for algebraic effects. The output is a self-contained `.js` file that runs in Node.js or the browser without a VM.
- **Native backend** (`mmlc` binary, or `--emit-ir`): Compiles typed AST to LLVM IR text (`lib/native/codegen.ml`), then invokes clang to produce a native binary linked against a C runtime (`native_rt/runtime.c`). Use `--emit-ir` to inspect the generated LLVM IR without compiling. Orchestrated by `lib/native/driver.ml`.

  Key IRs:

- Ast.expr — untyped, sugar included (EFor, EAnnot, etc.)
- texpr — typed, desugared (every node has ty: Types.ty), constraints transformed to explicit dict passing
- Bytecode.opcode — stack-based instructions (CONST, ADD, CALL, CLOSURE, JUMP_IF_FALSE, etc.)
- LLVM IR text — emitted by native codegen, consumed by clang

## Optimization Passes

After stage 4a (bytecode compilation), the bytecode goes through a peephole optimizer (`lib/optimize.ml`) before being serialized or executed. The optimizer is enabled by default and can be disabled with `--no-optimize` in the self-hosted compiler.

The optimizer uses a three-phase architecture:

1. **Mark** — Scan the bytecode linearly, tagging each instruction as `Keep`, `Remove`, or `ReplaceWith`
2. **Rewrite** — Build new code/line-table arrays from the marked instructions, computing an offset mapping
3. **Fixup** — Remap all jump targets using the offset mapping

The optimizer is applied recursively to nested prototypes in constants before optimizing the parent.

### Pass 1: Dead Code After TAIL_CALL

Removes unreachable instructions after a `TAIL_CALL` or `TAIL_CALL_N` up to the next jump target. Since tail calls never return, any code between them and the next reachable point is dead.

```
TAIL_CALL 1
RETURN        ← removed (dead)
FUNC_RETURN   ← removed (dead)
```

### Pass 2: Redundant JUMP Removal

Removes `JUMP` instructions that target the immediately following instruction. These are generated by the compiler for certain control flow patterns (e.g., empty else branches).

```
JUMP 5        ← removed (target is next instruction)
[offset 5]
```

### Pass 3: SET_LOCAL/GET_LOCAL Pair Elimination

Detects `SET_LOCAL n; GET_LOCAL n` pairs (store then immediately reload the same slot) and optimizes them based on liveness analysis:

- **Dead slot** (slot `n` is never read again before being overwritten): remove both instructions, leaving the value on the stack
- **Live slot**: rewrite to `DUP; SET_LOCAL n` — keeps the value on the stack while also storing it

The liveness analysis is conservative: it scans forward on the straight-line path and bails out at any control flow (jumps, returns, loop constructs).

```
-- Before (dead slot):       -- Before (live slot):
SET_LOCAL 3                  SET_LOCAL 3
GET_LOCAL 3                  GET_LOCAL 3
                             ... later reads slot 3 ...
-- After (dead slot):        -- After (live slot):
(both removed, value stays   DUP
 on stack)                   SET_LOCAL 3
```

### Pass 4: Dead Store Elimination

Removes stores to local slots that are never read:

- **DUP; SET_LOCAL n** where slot `n` is dead: remove both instructions
- **SET_LOCAL n** where slot `n` is dead: replace with `POP`

### Pass 5: Superinstructions

Fuses common two-instruction sequences into single specialized opcodes. This reduces dispatch overhead in the VM's instruction loop:

| Pattern | Superinstruction |
|---------|-----------------|
| `GET_LOCAL n; CALL k` | `GET_LOCAL_CALL(n, k)` |
| `GET_LOCAL n; TUPLE_GET i` | `GET_LOCAL_TUPLE_GET(n, i)` |
| `GET_LOCAL n; FIELD f` | `GET_LOCAL_FIELD(n, f)` |
| `GET_GLOBAL i; CALL k` | `GET_GLOBAL_CALL(i, k)` |
| `GET_GLOBAL i; FIELD f` | `GET_GLOBAL_FIELD(i, f)` |

### Pass 6: Jump Tables

Detects chains of `GET_LOCAL; TAG_EQ; JUMP_IF_FALSE` (generated by match expressions on variants) and replaces them with a single `JUMP_TABLE` instruction for O(1) dispatch. Only applies when the tag range is dense (range <= 2x the number of arms) and all tags are unique (no nested patterns or guards). Additionally validates that no external jumps target positions within the chain.

```
-- Before:                    -- After:
GET_LOCAL 0                   GET_LOCAL 0
TAG_EQ 0                     JUMP_TABLE(min=0, [addr0, addr1, addr2], default)
JUMP_IF_FALSE next1
...
GET_LOCAL 0
TAG_EQ 1
JUMP_IF_FALSE next2
...
```

### Pass 7: Push/Pop Cancellation

Removes `pure_push; POP` pairs where the push has no side effects (e.g., `CONST`, `GET_LOCAL`, `NIL`). The pushed value is immediately discarded, so both instructions are dead.

### Post-Rewrite: Jump Threading

After the mark/rewrite/fixup phases, a separate pass threads chains of unconditional jumps. If a `JUMP` target is itself a `JUMP`, the target is updated to the final destination. Additionally, `JUMP → RETURN` sequences are replaced with a direct `RETURN`. This only applies at control flow depth 0 (outside loops) to preserve loop frame semantics.

## Future Optimization Opportunities

There are two natural insertion points for further optimization:

1. **Between 3c and 4 — typed AST optimization** (pass over tprogram/texpr). This is the richest IR since you have full type info + tree structure. Good for constant folding, dead branch elimination, match simplification, inlining simple functions, and let flattening.

2. **After 4 — additional bytecode peephole passes**. Good for constant propagation through the stack and further dead code elimination.
