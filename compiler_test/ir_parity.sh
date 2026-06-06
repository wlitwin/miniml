#!/usr/bin/env bash
# IR parity check (roadmap #13): for each program in the corpus, dump the lowered
# IR (`--emit-ir`) from BOTH the OCaml reference compiler and the self-hosted
# compiler, and assert the two are byte-identical (after trailing-newline
# normalization). A divergence means the two compilers lower the same program to
# structurally/typewise different IR — exactly what value parity can miss.
#
# The self-hosted compiler runs as bytecode on the OCaml VM via --run-json.
# Usage: compiler_test/ir_parity.sh [corpus_dir]
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
CORPUS="${1:-compiler_test/ir_corpus}"
COMPILER_JSON="js/compiler.json"

if [ ! -f "$COMPILER_JSON" ]; then
  echo "Missing $COMPILER_JSON — run 'make self-host-compile-js' first." >&2
  exit 1
fi

REF="dune exec --no-build bin/main.exe --"
pass=0
fail=0
failed_names=""

for f in "$CORPUS"/*.mml; do
  name="$(basename "$f")"
  o="$($REF --emit-ir "$f" 2>/dev/null)"
  s="$($REF --run-json "$COMPILER_JSON" --emit-ir "$f" 2>/dev/null)"
  # Normalize trailing newlines (self-host `print` adds one; OCaml `print_string` does not).
  o="${o%$'\n'}"; s="${s%$'\n'}"
  if [ "$o" = "$s" ]; then
    pass=$((pass + 1))
    echo "  PASS  $name"
  else
    fail=$((fail + 1))
    failed_names="$failed_names $name"
    echo "  FAIL  $name"
    diff <(printf '%s' "$o") <(printf '%s' "$s") | head -12 | sed 's/^/        /'
  fi
done

echo "=============================="
if [ "$fail" -eq 0 ]; then
  echo "IR parity: $pass/$((pass + fail)) programs — both compilers agree."
  exit 0
else
  echo "IR parity: $fail/$((pass + fail)) FAILED:$failed_names"
  exit 1
fi
