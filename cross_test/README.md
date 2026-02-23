# Cross-VM Test Framework

Tests in this directory are compiled once and run on **both** the OCaml VM and the JS VM, ensuring all backends produce identical results.

## Running Tests

```bash
# Both VMs (recommended)
cross_test/run_all.sh

# OCaml VM only (also runs as part of `dune runtest`)
dune exec ./cross_test/runner.exe

# JS VM only
node cross_test/run_js.js
```

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
