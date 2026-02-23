#!/usr/bin/env bash
# Run cross-VM tests on both OCaml and JS VMs
# Usage: run_all.sh [-t "test name" ...]
set -e

DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$DIR/.." && pwd)"

echo "Building..."
(cd "$ROOT" && dune build cross_test/runner.exe compiler_test/parity_runner.exe 2>&1)

echo ""
echo "============ OCaml VM ============"
"$ROOT/_build/default/cross_test/runner.exe" "$DIR"/tests/*.tests "$@"
ocaml_exit=$?

echo ""
echo "============ JS VM ============"
node "$DIR/run_js.js" "$DIR"/tests/*.tests "$@"
js_exit=$?

echo ""
echo "============ Compiler Parity ============"
"$ROOT/_build/default/compiler_test/parity_runner.exe" "$DIR"/tests/*.tests "$@"
parity_exit=$?

echo ""
if [ $ocaml_exit -eq 0 ] && [ $js_exit -eq 0 ] && [ $parity_exit -eq 0 ]; then
  echo "All cross-VM and parity tests passed."
else
  echo "Some tests failed."
  exit 1
fi
