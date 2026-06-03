# Cross-VM Test Framework

Tests in this directory run on **every** backend (OCaml VM, emit-js,
playground, native, parity, and the oracle reference interpreter), ensuring all
backends produce identical results.

## Running Tests

```bash
# The full pre-merge gate (every suite, every backend)
make check

# Individual backends (fastest first)
make test-ocaml        # OCaml VM
make test-emit-js      # JS codegen + node
make test-playground   # self-host compiler emit-js path
make test-native       # LLVM native binaries
make test-oracle       # the reference interpreter (executable spec)
make test-parity       # self-host compiler bytecode
```

## The Parsers

The `.tests` format has exactly **two** parser implementations — any format
change must update both, and only these:

- `cross_test/test_format.ml` — OCaml; used by cross_test/runner.ml,
  native_test/runner.ml, compiler_test/parity_runner.ml, diff_test/diff_runner.ml
- `cross_test/test_parser.js` — JS; used by run_emit_js.js,
  run_playground.js

## Adding Tests

Create a `.tests` file in `cross_test/tests/`. Both runners auto-discover every `.tests` file in that directory.

### File Format

```
=== Section Header ===

--- test: name of test
source code
--- expect: expected output

--- test: another test
more source code
--- expect-type-error

--- test: runtime failure
bad code
--- expect-runtime-error: substring to match
```

**Directives:**

| Directive | Meaning |
|-----------|---------|
| `=== ... ===` | Section header (ignored, for readability) |
| `--- test: <name>` | Start a new test case |
| `--- expect: <value>` | Expected `pp_value` output |
| `--- expect-type-error` | Compilation should fail with a type error |
| `--- expect-runtime-error: <str>` | Runtime error containing `<str>` |
| `--- skip-<backend>: <reason>` | Skip on that backend; the reason **must reference a tracked bug** |
| `--- expect-<backend>: <value>` | Backend-specific expected output (documented, tracked divergences only) |

Backend names in directives: `native`, `emit-js`, `playground` (each runner
checks for its own name; others' directives are ignored).

### Expected Values

The `--- expect:` value is compared against the interpreter's `pp_value` output. A few things to know:

- Strings are **unquoted**: `pp_value (VString "hello")` produces `hello`, not `"hello"`.
- Integers, bools, floats print naturally: `42`, `true`, `3.14`.
- Lists use semicolons: `[1; 2; 3]`.
- Tuples use commas: `(1, true)`.

If the expected value has **significant trailing whitespace**, wrap it in double quotes:

```
--- test: left-aligned padding
let s = "hi" in $"{s:<10}"
--- expect: "hi        "
```

The outer quotes are stripped by the runner. This avoids editors silently trimming trailing spaces.

Inside the quoted form, `\n` means a **newline** and `\\` a literal backslash.
This is how multi-line output (e.g. consecutive `print` calls, or print output
followed by a final value) is expressed:

```
--- test: consecutive prints are separate lines
print "a";;
print "b"
--- expect: "a\nb"

--- test: print output then final value
print "result is";;
42
--- expect: "result is\n42"
```

### Multi-line Source

Source code can span multiple lines. Everything between `--- test:` and the next `--- expect*` directive is collected as the source:

```
--- test: multi-line example
let double x = x * 2
;;
let quad x = double (double x)
;;
quad 3
--- expect: 12
```

### Example

A minimal test file (`cross_test/tests/arithmetic.tests`):

```
=== Arithmetic ===

--- test: addition
1 + 2
--- expect: 3

--- test: division by zero
1 / 0
--- expect-runtime-error: Division by zero
```
