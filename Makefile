# MiniML Interpreter — Makefile
# Usage: make help

.PHONY: all build clean test repl help \
        test-unit test-cross test-js test-parity test-translate test-all \
        run emit-json emit-binary run-json run-binary \
        translate translate-all translate-diff \
        bundle self-host-compile \
        playground playground-serve \
        mmlc emit-ir test-native native-compile native-run

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

bundle: build  ## Build the browser JS bundle (js/miniml.bundle.js)
	node js/browser.js

# ── Tests ──────────────────────────────────────────────────

test: test-unit  ## Run OCaml unit tests (default)

test-unit: build  ## Run OCaml unit tests (dune test)
	dune test

test-cross: build  ## Run all cross-VM tests: make test-cross [FILTER="name"]
	bash cross_test/run_all.sh $(if $(FILTER),-t "$(FILTER)")

test-ocaml: build  ## Run cross-VM tests on OCaml VM: make test-ocaml [FILTER="name"]
	dune exec cross_test/runner.exe -- cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

test-js: build  ## Run cross-VM tests on JS VM: make test-js [FILTER="name"]
	node cross_test/run_js.js cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

test-parity: self-host-compile-js ## Run compiler parity tests: make test-parity [FILTER="name"]
	dune exec compiler_test/parity_runner.exe -- cross_test/tests/*.tests $(if $(FILTER),-t "$(FILTER)")

test-js-suite: build  ## Run the JS VM test suite (js/test.js)
	node js/test.js

test-translate: build  ## Run translator tests (OCaml → MiniML)
	dune exec translate_test/runner.exe -- translate_test/tests/*.tests

test-all: test-unit test-cross test-js-suite test-translate  ## Run ALL tests (unit + cross-VM + JS suite + translator)
	@echo ""
	@echo "All tests passed."

# Run a specific cross-test file:
#   make test-file FILE=cross_test/tests/fundep_callsite.tests
test-file: build  ## Run a specific .tests file on both VMs: make test-file FILE=path/to/file.tests
	@test -n "$(FILE)" || (echo "Usage: make test-file FILE=<file.tests>"; exit 1)
	@echo "=== OCaml VM ==="
	dune exec cross_test/runner.exe -- $(FILE)
	@echo ""
	@echo "=== JS VM ==="
	node cross_test/run_js.js $(FILE)

# ── Translation (OCaml → MiniML) ──────────────────────────

TRANSLATE_FILES = ast token bytecode types lexer typechecker optimize compiler parser serialize js_codegen
TRANSLATOR = dune exec tools/ocaml_to_mml/main.exe --

translate: build  ## Translate a single file: make translate FILE=lib/ast.ml
	@test -n "$(FILE)" || (echo "Usage: make translate FILE=<lib/file.ml>"; exit 1)
	$(TRANSLATOR) $(FILE)

translate-all: build  ## Translate all target files to self_host/
	@for f in $(TRANSLATE_FILES); do \
		echo "Translating $$f.ml → self_host/$$f.mml"; \
		$(TRANSLATOR) lib/$$f.ml > self_host/$$f.mml; \
	done
	@echo "Done. Translated $(words $(TRANSLATE_FILES)) files."

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
                  self_host/types.mml self_host/lexer.mml self_host/parser.mml \
                  self_host/typechecker.mml self_host/optimize.mml self_host/compiler.mml \
                  self_host/serialize.mml self_host/js_codegen.mml self_host/main.mml

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

playground: build bundle translate-all  ## Build the playground site (JS bundle + self-hosted compiler)
	@echo "Compiling self-hosted compiler to js/compiler.json..."
	dune exec bin/main.exe -- --emit-json $(SELF_HOST_FILES) > js/compiler.json
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
	cp js/compiler.json $(DEMO_ROOT)/js/.
	cp js/compiler_native.js $(DEMO_ROOT)/js/.
	cp js/canvas_gui_demo.js $(DEMO_ROOT)/js/.
	cp js/miniml.bundle.js $(DEMO_ROOT)/js/.
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
	@echo "  make test-cross                Run cross-VM tests"
	@echo "  make test-file FILE=cross_test/tests/basic.tests"
	@echo "  make translate FILE=lib/ast.ml Translate one file to MiniML"
	@echo "  make translate-all             Translate all files to self_host/"
	@echo "  make translate-diff            Diff translations vs self_host/"
	@echo "  make emit-json FILE=prog.mml  Compile to JSON bundle"
	@echo "  make bundle                    Build browser JS bundle"
