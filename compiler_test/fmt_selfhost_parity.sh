#!/bin/sh
# Self-hosted formatter parity gate.
#
# The formatter has been translated into self_host/ (utf8, cst, cst_build,
# formatter). This checks that the SELF-HOST formatter, compiled to JS and run
# on node, produces BYTE-IDENTICAL output to the OCaml reference formatter over
# a real MiniML corpus (stdlib/ + self_host/). Any divergence is a translation
# faithfulness bug — the same class ir-parity catches for the compiler.
#
# Files the OCaml formatter can't format (Unsupported construct / unparseable)
# are skipped, exactly as compiler_test/format_runner.ml skips them.
set -e
cd "$(dirname "$0")/.."

# MML / MMLFMT are overridable so the `make check` gate can pass pre-built
# binaries (avoiding dune-build-lock contention with the parallel suites).
# Standalone, they default to `dune exec`, which builds on demand.
MML="${MML:-dune exec bin/main.exe --}"
MMLFMT="${MMLFMT:-dune exec bin/mml.exe -- fmt}"
[ -n "${MML_PREBUILT:-}" ] || dune build bin/main.exe bin/mml.exe 2>/dev/null

# A tiny entry over the translated formatter: read a file, write its formatting
# to raw stdout (IO.write — byte-exact, no pp framing).
harness=/tmp/fmt_selfhost_harness.mml
cat > "$harness" <<'MML'
let () =
  match Sys.args () with
  | _ :: path :: _ -> IO.write (Formatter.format_source (IO.read_file path))
  | _ -> IO.write_err "usage: fmt <file>\n"
MML

FRONT="self_host/token.mml self_host/ast.mml self_host/bytecode.mml self_host/types.mml \
  self_host/match_tree_types.mml self_host/lexer.mml self_host/parser.mml \
  self_host/utf8.mml self_host/cst.mml self_host/cst_build.mml self_host/formatter.mml"

tool=/tmp/fmt_selfhost_tool.js
# shellcheck disable=SC2086
$MML --emit-js $FRONT "$harness" > "$tool"

pass=0; skip=0; fail=0
for f in stdlib/*.mml self_host/*.mml; do
  # Skip what the OCaml formatter itself can't handle (Unsupported / unparseable).
  if ! $MMLFMT "$f" > /tmp/fmt_oc.txt 2>/dev/null; then
    skip=$((skip + 1)); continue
  fi
  # The emit-js harness auto-displays the program's final unit value as a
  # trailing "()" line; strip exactly that one artifact before comparing.
  node "$tool" "$f" 2>/dev/null | sed -e '${/^()$/d;}' > /tmp/fmt_sh.txt
  if diff -q /tmp/fmt_oc.txt /tmp/fmt_sh.txt > /dev/null 2>&1; then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    echo "DIVERGENCE: $f"
    diff /tmp/fmt_oc.txt /tmp/fmt_sh.txt | head -8
  fi
done

echo "self-host formatter parity: $pass passed, $fail failed ($skip skipped: unsupported/unparseable on the OCaml formatter)"
[ "$fail" -eq 0 ]
