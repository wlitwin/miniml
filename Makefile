# MiniML Interpreter — Makefile
# Usage: make help

.PHONY: all build clean test repl help \
        test-unit test-emit-js test-parity test-playground test-oracle test-translate test-all check \
        check-run-unit check-run-diff check-run-translate \
        check-run-emit-js check-run-playground check-run-native check-run-oracle check-run-parity check-run-ir-parity \
        check-run-fuzz test-fuzz check-run-cst test-cst check-run-fmt test-fmt \
        test-diff diff fuzz shrink \
        run emit-json emit-binary run-json run-binary \
        translate translate-all translate-diff \
        self-host-compile \
        playground playground-serve \
        mmlc emit-ir emit-ir-typed ir-parity ir-parity-full test-native native-compile native-run

# ── Build ──────────────────────────────────────────────────

all: build  ## Alias for build

build:  ## Build everything (OCaml compiler, tools, test runners)
	dune build

clean:  ## Clean build artifacts
	dune clean

# ── REPL & Run ─────────────────────────────────────────────

repl: build  ## Start the interactive REPL
	dune exec bin/main.exe

run: build  ## Run a MiniML file: make run FILE=program.mml
	@test -n "$(FILE)" || (echo "Usage: make run FILE=<file.mml>"; exit 1)
	dune exec bin/main.exe -- $(FILE)

# ── Emit / Bundle ──────────────────────────────────────────

emit-json: build  ## Compile to JSON bundle: make emit-json FILE=program.mml
	@if [ -n "$(FILE)" ]; then \
		dune exec bin/main.exe -- --emit-json $(FILE); \
	else \
		dune exec bin/main.exe -- --emit-json; \
	fi

emit-binary: build  ## Compile to binary bundle: make emit-binary FILE=program.mml
	@if [ -n "$(FILE)" ]; then \
		dune exec bin/main.exe -- --emit-binary $(FILE); \
	else \
		dune exec bin/main.exe -- --emit-binary; \
	fi

run-json: build  ## Run a JSON bundle: make run-json FILE=bundle.json
	@test -n "$(FILE)" || (echo "Usage: make run-json FILE=<bundle.json>"; exit 1)
	dune exec bin/main.exe -- --run-json $(FILE)

run-binary: build  ## Run a binary bundle: make run-binary FILE=bundle.mmlb
	@test -n "$(FILE)" || (echo "Usage: make run-binary FILE=<bundle.mmlb>"; exit 1)
	dune exec bin/main.exe -- --run-binary $(FILE)

# ── Tests ──────────────────────────────────────────────────

test: test-unit  ## Run OCaml unit tests (default)

test-unit: build  ## Run OCaml unit tests (dune test)
	dune test

test-ocaml: build  ## Run cross-VM tests on OCaml VM: make test-ocaml [FILTER="name"]
	dune exec cross_test/runner.exe -- cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

test-emit-js: build  ## Run cross-VM tests via --emit-js + node: make test-emit-js [FILTER="name"]
	node cross_test/run_emit_js.js cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

test-parity: self-host-compile-js ## Run compiler parity tests: make test-parity [FILTER="name"]
	dune exec compiler_test/parity_runner.exe -- cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

test-playground: self-host-compile-native-js  ## Run cross-VM tests via the playground path (self-host emit-js): make test-playground [FILTER="name"]
	node cross_test/run_playground.js cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

test-oracle: build  ## Run cross-VM tests with the Oracle reference interpreter: make test-oracle [FILTER="name"]
	dune exec cross_test/runner.exe -- --oracle cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

test-diff: build  ## Smoke-test the differential runner: agreement on smoke programs, disagreement detection
	dune exec diff_test/diff_runner.exe -- diff_test/smoke/effects.mml diff_test/smoke/data.mml diff_test/smoke/print_value.mml
	dune exec diff_test/diff_runner.exe -- --expect-disagree diff_test/smoke/nondeterministic.mml

# Fixed seed + count so the gate is deterministic: a failure here is a real,
# reproducible divergence (replay any seed with: make fuzz COUNT=1 SEED=n).
# Random-seed exploration stays a manual / scheduled activity (make fuzz).
test-fuzz: build  ## Fixed-seed differential fuzz gate: generated programs must agree on oracle/vm/emit-js
	dune exec diff_test/fuzz_runner.exe -- --count 50 --seed 1 --fast

fuzz: build  ## Differential fuzzing: make fuzz [COUNT=100] [SEED=n] [SIZE=25] [FULL=1] [TIMEOUT=10] [SHRINK=1]
	dune exec diff_test/fuzz_runner.exe -- --count $(or $(COUNT),100) $(if $(SEED),--seed $(SEED)) $(if $(SIZE),--size $(SIZE)) $(if $(FULL),--full,--fast) $(if $(TIMEOUT),--timeout $(TIMEOUT)) $(if $(SHRINK),--shrink)

shrink: build  ## Minimize a disagreeing program + emit a .tests block: make shrink FILE=prog.mml [BACKENDS=...] [TESTNAME="name"]
	@test -n "$(FILE)" || (echo "Usage: make shrink FILE=<prog.mml> [TESTNAME=\"test name\"]"; exit 1)
	dune exec diff_test/shrink_runner.exe -- $(if $(BACKENDS),--backends $(BACKENDS)) $(if $(TESTNAME),--test-name "$(TESTNAME)") $(FILE)

diff: build  ## Differential run: do all backends agree on a program? make diff FILE=prog.mml [BACKENDS=oracle,vm,emit-js,native] [TIMEOUT=10]
	@test -n "$(FILE)" || (echo "Usage: make diff FILE=<prog.mml> [BACKENDS=oracle,vm,emit-js,native]"; exit 1)
	dune exec diff_test/diff_runner.exe -- $(if $(BACKENDS),--backends $(BACKENDS)) $(if $(TIMEOUT),--timeout $(TIMEOUT)) $(FILE)

test-translate: build  ## Run translator tests (OCaml → MiniML)
	dune exec translate_test/runner.exe -- translate_test/tests/*.tests

test-cst: build  ## Lossless CST round-trip: lex+reconstruct the whole corpus, assert byte-identical (#17)
	dune exec compiler_test/cst_roundtrip_runner.exe

test-fmt: build  ## Formatter correctness: semantic-preservation + idempotence over the corpus (#21)
	dune exec compiler_test/format_runner.exe

test-all: test-unit test-ocaml test-translate  ## Run ALL local tests (unit + cross-VM + translator)
	@echo ""
	@echo "All tests passed."

test-all-backends: test-ocaml test-emit-js test-native  ## Run cross-tests on all backends (ocaml, emit-js, native)
	@echo ""
	@echo "All backends passed."

# ── The Gate ───────────────────────────────────────────────
# `make check` is THE pre-merge gate: every suite, every backend, both compilers.
# Ordered fastest-first so failures surface early.
#
#   Suite            Compiler    Execution
#   test-unit        ocaml-ref   OCaml unit tests + cross tests on OCaml VM
#   test-diff        —           differential runner smoke (agreement + detection)
#   test-translate   —           OCaml→MiniML translator tests
#   test-emit-js     ocaml-ref   cross tests via --emit-js + node
#   test-playground  self-host   cross tests via compiler_native.js + node (the web playground path)
#   test-native      ocaml-ref   cross tests as LLVM native binaries
#   test-oracle      —           cross tests on the reference interpreter (the executable spec, lib/oracle.ml)
#   test-parity      self-host   cross tests as bytecode on the OCaml VM
#   test-cst         —           lossless CST round-trip: lex+reconstruct the corpus byte-for-byte (#17)
#   test-fmt         —           formatter: semantic-preservation + idempotence over the corpus (#21)

# The gate runs every suite. Suites run CONCURRENTLY, CHECK_JOBS at a time
# (default 4, CHECK_JOBS=1 restores fully-sequential behavior), each logging
# to $(CHECK_LOG_DIR)/<suite>.log. A one-line PASS/FAIL summary with wall time
# prints as each suite finishes; failing suites' full logs are dumped after
# the parallel phase so they never interleave.
#
# All build artifacts (dune binaries, self-host compiler JS/JSON) are built
# serially UP FRONT: the per-suite commands below use the built binaries
# directly, so concurrent suites never contend on the dune lock and never
# race to regenerate self_host/ or js/ artifacts.
#
# Suites are listed slowest-first so the long poles start immediately.

CHECK_LOG_DIR := /tmp/mml-check-logs
CHECK_SUITES := parity emit-js native playground oracle fuzz unit translate diff ir-parity cst fmt native-selfhost native-selfhost-build
CHECK_JOBS ?= 4
CHECK_BIN := ./_build/default

check: build ## Full pre-merge gate: all suites x all backends x both compilers (parallel: CHECK_JOBS=n)
	@$(MAKE) --no-print-directory self-host-compile-js self-host-compile-native-js
	@rm -rf $(CHECK_LOG_DIR) && mkdir -p $(CHECK_LOG_DIR)
	@echo ""
	@echo "Running $(words $(CHECK_SUITES)) gate suites, $(CHECK_JOBS) at a time (full logs: $(CHECK_LOG_DIR)/)"
	@echo ""
	@start=$$(date +%s); \
	$(MAKE) --no-print-directory -j $(CHECK_JOBS) -k $(addprefix check-suite-,$(CHECK_SUITES)); \
	status=$$?; \
	end=$$(date +%s); \
	elapsed=$$((end-start)); \
	echo ""; \
	if [ $$status -eq 0 ]; then \
	  echo "=============================="; \
	  echo "  ALL GATES PASSED  ($$((elapsed/60))m$$((elapsed%60))s)"; \
	  echo "=============================="; \
	else \
	  for f in $(CHECK_LOG_DIR)/*.failed; do \
	    [ -e "$$f" ] || continue; \
	    suite=$$(basename $$f .failed); \
	    echo "------------------------------------------------------------------"; \
	    echo "  FAILED SUITE: $$suite (full log)"; \
	    echo "------------------------------------------------------------------"; \
	    cat $(CHECK_LOG_DIR)/$$suite.log; \
	  done; \
	  echo ""; \
	  echo "=============================="; \
	  echo "  GATE FAILED  ($$((elapsed/60))m$$((elapsed%60))s)"; \
	  echo "=============================="; \
	  exit 1; \
	fi

# One suite of the gate: run its command with output captured to a log file,
# print a one-line summary with timing. Failures mark a .failed file; the
# parent check target dumps those logs after the parallel phase.
check-suite-%:
	@start=$$(date +%s); \
	if $(MAKE) --no-print-directory check-run-$* > $(CHECK_LOG_DIR)/$*.log 2>&1; then \
	  end=$$(date +%s); \
	  summary=$$(grep -E "passed|agree" $(CHECK_LOG_DIR)/$*.log | tail -1); \
	  [ -n "$$summary" ] || summary="(no test output — results cached as up-to-date)"; \
	  printf "  PASS  %-12s %4ds   %s\n" "$*" "$$((end-start))" "$$summary"; \
	else \
	  end=$$(date +%s); \
	  touch $(CHECK_LOG_DIR)/$*.failed; \
	  printf "  FAIL  %-12s %4ds   (log: $(CHECK_LOG_DIR)/$*.log)\n" "$*" "$$((end-start))"; \
	  exit 1; \
	fi

# The suites' raw commands. These assume the artifacts are already built
# (check builds them up front); they invoke the binaries directly so parallel
# suites don't contend on the dune build lock. The CROSS_TEST_JOBS /
# NATIVE_TEST_JOBS caps keep total process count sane when suites overlap.
check-run-unit:
	dune test
check-run-diff:
	$(CHECK_BIN)/diff_test/diff_runner.exe diff_test/smoke/effects.mml diff_test/smoke/data.mml diff_test/smoke/print_value.mml
	$(CHECK_BIN)/diff_test/diff_runner.exe --expect-disagree diff_test/smoke/nondeterministic.mml
check-run-translate:
	$(CHECK_BIN)/translate_test/runner.exe translate_test/tests/*.tests
check-run-emit-js:
	CROSS_TEST_JOBS=$(or $(CROSS_TEST_JOBS),4) node cross_test/run_emit_js.js cross_test/tests/*.tests
check-run-playground:
	CROSS_TEST_JOBS=$(or $(CROSS_TEST_JOBS),4) node cross_test/run_playground.js cross_test/tests/*.tests
check-run-native:
	NATIVE_TEST_JOBS=$(or $(NATIVE_TEST_JOBS),4) $(CHECK_BIN)/native_test/runner.exe cross_test/tests/*.tests
check-run-oracle:
	$(CHECK_BIN)/cross_test/runner.exe --oracle cross_test/tests/*.tests
check-run-parity:
	$(CHECK_BIN)/compiler_test/parity_runner.exe cross_test/tests/*.tests
check-run-ir-parity:
	$(CHECK_BIN)/compiler_test/ir_parity_runner.exe cross_test/tests/*.tests
check-run-fuzz:
	$(CHECK_BIN)/diff_test/fuzz_runner.exe --count 50 --seed 1 --fast
check-run-cst:
	$(CHECK_BIN)/compiler_test/cst_roundtrip_runner.exe
check-run-fmt:
	$(CHECK_BIN)/compiler_test/format_runner.exe
check-run-native-selfhost:
	$(CHECK_BIN)/bin/main.exe --emit-js $(NATIVE_SELF_HOST_FILES) > /dev/null && echo "native self-host backend typecheck+compile passed"
# End-to-end: the self-hosted compiler (js/compiler.json, run on the OCaml VM) drives
# its own native backend + clang to a real binary, which must run and print correctly.
# Proves the unified self-host compiler emits working native executables (roadmap #16).
check-run-native-selfhost-build:
	@printf 'let rec fib n = if n < 2 do n else fib (n - 1) + fib (n - 2)\nlet () = print (string_of_int (fib 20))\n' > /tmp/mml_sh_native.mml
	$(CHECK_BIN)/bin/main.exe --run-json js/compiler.json --emit-native /tmp/mml_sh_native.mml -o /tmp/mml_sh_native_bin
	@out=$$(/tmp/mml_sh_native_bin); if [ "$$out" = "6765" ]; then echo "self-host native build passed (fib 20 = $$out)"; else echo "FAIL: expected 6765, got $$out"; exit 1; fi

# Run a specific cross-test file:
#   make test-file FILE=cross_test/tests/fundep_callsite.tests
test-file: build  ## Run a specific .tests file on the OCaml VM and emit-js: make test-file FILE=path/to/file.tests
	@test -n "$(FILE)" || (echo "Usage: make test-file FILE=<file.tests>"; exit 1)
	@echo "=== OCaml VM ==="
	dune exec cross_test/runner.exe -- $(FILE)
	@echo ""
	@echo "=== emit-js ==="
	node cross_test/run_emit_js.js $(FILE)

# ── Translation (OCaml → MiniML) ──────────────────────────

TRANSLATE_FILES = ast token bytecode types match_tree_types match_tree lexer typechecker ir_analysis texpr_opt pipeline ir_serialize optimize compiler parser serialize js_codegen
# Native backend modules (lib/native/*.ml). Translated into self_host/ and kept
# in sync, but NOT part of the playground bundle (SELF_HOST_FILES) — they are
# verified by the `native-selfhost` gate stage instead.
NATIVE_TRANSLATE_FILES = ir_emit codegen
TRANSLATOR = dune exec tools/ocaml_to_mml/main.exe --

translate: build  ## Translate a single file: make translate FILE=lib/ast.ml
	@test -n "$(FILE)" || (echo "Usage: make translate FILE=<lib/file.ml>"; exit 1)
	$(TRANSLATOR) $(FILE)

translate-all: build  ## Translate all target files to self_host/
	@for f in $(TRANSLATE_FILES); do \
		echo "Translating $$f.ml → self_host/$$f.mml"; \
		$(TRANSLATOR) lib/$$f.ml > self_host/$$f.mml; \
	done
	@for f in $(NATIVE_TRANSLATE_FILES); do \
		echo "Translating native/$$f.ml → self_host/$$f.mml"; \
		$(TRANSLATOR) lib/native/$$f.ml > self_host/$$f.mml; \
	done
	@echo "Done. Translated $(words $(TRANSLATE_FILES) $(NATIVE_TRANSLATE_FILES)) files."

translate-preview: build  ## Preview translations to /tmp (without overwriting self_host/)
	@for f in $(TRANSLATE_FILES); do \
		$(TRANSLATOR) lib/$$f.ml > /tmp/$$f.mml; \
		echo "$$f.ml → /tmp/$$f.mml ($$(wc -l < /tmp/$$f.mml | tr -d ' ') lines)"; \
	done

translate-diff: build  ## Show diff between translated output and existing self_host/
	@for f in $(TRANSLATE_FILES); do \
		echo "=== $$f.ml ==="; \
		diff <($(TRANSLATOR) lib/$$f.ml) self_host/$$f.mml || true; \
		echo ""; \
	done

translate-stats: build  ## Show translation stats (lines, TODOs per file)
	@total=0; todos=0; \
	printf "%-18s %6s  %s\n" "File" "Lines" "TODOs"; \
	printf "%-18s %6s  %s\n" "──────────────────" "──────" "─────"; \
	for f in $(TRANSLATE_FILES); do \
		lines=$$($(TRANSLATOR) lib/$$f.ml 2>&1 | wc -l | tr -d ' '); \
		td=$$($(TRANSLATOR) lib/$$f.ml 2>&1 | grep -c "TODO" || true); \
		printf "%-18s %6s  %s\n" "$$f.ml" "$$lines" "$$td"; \
		total=$$((total + lines)); \
		todos=$$((todos + td)); \
	done; \
	printf "%-18s %6s  %s\n" "──────────────────" "──────" "─────"; \
	printf "%-18s %6s  %s\n" "TOTAL" "$$total" "$$todos"

# ── Self-hosted compiler ───────────────────────────────────

SELF_HOST_FILES = self_host/token.mml self_host/ast.mml self_host/bytecode.mml \
                  self_host/types.mml self_host/match_tree_types.mml \
                  self_host/lexer.mml self_host/parser.mml \
                  self_host/typechecker.mml self_host/ir_analysis.mml \
                  self_host/match_tree.mml \
                  self_host/texpr_opt.mml self_host/pipeline.mml self_host/ir_serialize.mml \
                  self_host/optimize.mml self_host/compiler.mml \
                  self_host/serialize.mml self_host/js_codegen.mml \
                  self_host/ir_emit.mml self_host/codegen.mml self_host/main.mml

# The frontend modules the native backend depends on, in dependency order, then
# ir_emit and codegen. This is the minimal set codegen needs (it consumes a
# typed program; it does not lower match trees, optimize, or emit JS), used to
# typecheck the backend in-context — not to run it. Deliberately excludes
# match_tree/texpr_opt/pipeline/optimize/compiler/serialize/js_codegen/ir_analysis
# (unneeded, and one of them trips a separate ambiguous-typeclass-defaulting bug
# when compiled without main.mml).
NATIVE_SELF_HOST_FILES = self_host/token.mml self_host/ast.mml self_host/bytecode.mml \
                         self_host/types.mml self_host/match_tree_types.mml \
                         self_host/lexer.mml self_host/parser.mml self_host/typechecker.mml \
                         self_host/ir_emit.mml self_host/codegen.mml

native-selfhost-typecheck: build translate-all  ## Typecheck the self-hosted native backend in-context
	dune exec bin/main.exe -- --emit-js $(NATIVE_SELF_HOST_FILES) > /dev/null
	@echo "native self-host backend: typecheck + compile passed"

self-host-compile: build  ## Compile the self-hosted compiler to a JSON bundle
	dune exec bin/main.exe -- --emit-json $(SELF_HOST_FILES)

self-host-compile-js: build translate-all
	dune exec bin/main.exe -- --emit-json $(SELF_HOST_FILES) > js/compiler.json

self-host-compile-native-js: build translate-all  ## Compile self-hosted compiler to standalone JS
	dune exec bin/main.exe -- --emit-js $(SELF_HOST_FILES) > js/compiler_native.js

self-host-run: build  ## Compile self-hosted compiler and run a file through it
	@test -n "$(FILE)" || (echo "Usage: make self-host-run FILE=<program.mml>"; exit 1)
	dune exec bin/main.exe -- --emit-json $(SELF_HOST_FILES) > /tmp/selfhost_compiler.json
	dune exec bin/main.exe -- --run-json /tmp/selfhost_compiler.json $(FILE)

# ── Playground ────────────────────────────────────────────

playground: build translate-all  ## Build the playground site (self-hosted compiler as JS + stdlib bundle)
	@echo "Building js/stdlib_sources.js..."
	node js/build_stdlib_bundle.js
	@echo "Compiling self-hosted compiler to js/compiler_native.js..."
	dune exec bin/main.exe -- --emit-js $(SELF_HOST_FILES) > js/compiler_native.js
	@echo "Playground built. Serve with: make playground-serve"

playground-serve: playground  ## Build and serve the playground on localhost:8000
	@echo "Serving playground at http://localhost:8000/js/"
	python3 -m http.server 8000

DEMO_ROOT=~/data/code/wlitwin.github.io/demos/miniml

copy-to-demo: playground
	@echo "Copying to blog"
	cp -r docs/ $(DEMO_ROOT)/docs
	mkdir -p $(DEMO_ROOT)/js
	cp js/demo.html $(DEMO_ROOT)/js/.
	cp js/index.html $(DEMO_ROOT)/js/.
	cp js/favicon.svg $(DEMO_ROOT)/js/.
	cp js/compiler_native.js $(DEMO_ROOT)/js/.
	cp js/canvas_gui_demo.js $(DEMO_ROOT)/js/.
	cp js/stdlib_sources.js $(DEMO_ROOT)/js/.
	cp js/miniml-harness.js $(DEMO_ROOT)/js/.

# ── Native Compiler (LLVM IR backend) ─────────────────────

mmlc: build  ## Build the native compiler (mmlc)
	@echo "mmlc built: dune exec bin_native/main.exe"

native-compile: build  ## Compile a MiniML file to native: make native-compile FILE=program.mml
	@test -n "$(FILE)" || (echo "Usage: make native-compile FILE=<file.mml>"; exit 1)
	dune exec bin_native/main.exe -- $(FILE)

native-run: build  ## Compile and run a MiniML file natively: make native-run FILE=program.mml
	@test -n "$(FILE)" || (echo "Usage: make native-run FILE=<file.mml>"; exit 1)
	dune exec bin_native/main.exe -- $(FILE) -o /tmp/mml_native_out
	@/tmp/mml_native_out

emit-ir: build  ## Emit LLVM IR for a MiniML file: make emit-ir FILE=program.mml
	@test -n "$(FILE)" || (echo "Usage: make emit-ir FILE=<file.mml>"; exit 1)
	dune exec bin_native/main.exe -- --emit-ir $(FILE)

emit-ir-typed: build  ## Dump the lowered typed IR (S-expr) for a MiniML file: make emit-ir-typed FILE=program.mml
	@test -n "$(FILE)" || (echo "Usage: make emit-ir-typed FILE=<file.mml>"; exit 1)
	dune exec bin/main.exe -- --emit-ir $(FILE)

ir-parity: self-host-compile-js  ## Cross-compiler IR parity: both compilers must lower a curated corpus to identical IR (roadmap #13)
	bash compiler_test/ir_parity.sh

ir-parity-full: self-host-compile-js  ## Full-corpus cross-compiler IR parity (also a `make check` stage; tolerates a documented baseline of alpha-equivalent residuals — roadmap #13)
	dune exec compiler_test/ir_parity_runner.exe -- cross_test/tests/*.tests

test-native: build  ## Run native compiler tests: make test-native [FILTER="name"]
	dune exec native_test/runner.exe -- cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

# ── Help ───────────────────────────────────────────────────

help:  ## Show this help
	@echo "MiniML Interpreter"
	@echo ""
	@echo "Usage: make <target> [FILE=...]"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*## "}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "Examples:"
	@echo "  make repl                      Start the REPL"
	@echo "  make run FILE=hello.mml        Run a MiniML file"
	@echo "  make test-all                  Run all tests"
	@echo "  make test-file FILE=cross_test/tests/basic.tests"
	@echo "  make translate FILE=lib/ast.ml Translate one file to MiniML"
	@echo "  make translate-all             Translate all files to self_host/"
	@echo "  make translate-diff            Diff translations vs self_host/"
	@echo "  make emit-json FILE=prog.mml  Compile to JSON bundle"
