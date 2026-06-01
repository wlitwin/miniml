// Shared .tests file parser for the node-based cross-VM runners
// (run_emit_js.js, run_playground.js). Parses the `--- test:` / `--- expect:`
// format plus per-backend skip directives.

const fs = require("fs");

function parseTestFile(filename) {
  const content = fs.readFileSync(filename, "utf-8");
  const lines = content.split("\n");
  const tests = [];
  let state = null; // null | { name, sourceLines, skipEmitJs }

  for (const line of lines) {
    const trimmed = line.trim();

    if (trimmed.startsWith("--- test:")) {
      const name = trimmed.slice(9).trim();
      state = { name, sourceLines: [] };
    } else if (line.startsWith("--- skip-emit-js:")) {
      // Mark a test the JS (--emit-js) backend can't run (e.g. advanced effect
      // features it doesn't support). Mirrors --- skip-native for the native
      // runner. Applies to both the OCaml-ref emit-js path and the playground
      // (self-host emit-js) path, which share backend semantics.
      if (state) state.skipEmitJs = line.slice(17).trim();
    } else if (line.startsWith("--- skip-playground:")) {
      // Mark a test the playground path (SELF-HOST compiler running on the
      // emit-js runtime) can't run, while the OCaml-ref emit-js path can.
      // Used for known self-host-on-JS miscompilations; each marker should
      // reference a tracked bug.
      if (state) state.skipPlayground = line.slice(20).trim();
    } else if (line.startsWith("--- expect: ")) {
      if (state) {
        let expected = line.slice(12); // preserve trailing whitespace
        if (expected.length >= 2 && expected[0] === '"' && expected[expected.length - 1] === '"') {
          expected = expected.slice(1, -1);
        }
        tests.push({
          name: state.name,
          source: state.sourceLines.join("\n").trim(),
          skipEmitJs: state.skipEmitJs,
          skipPlayground: state.skipPlayground,
          expect: { type: "value", value: expected },
        });
        state = null;
      }
    } else if (trimmed.startsWith("--- expect-type-error:")) {
      if (state) {
        const substr = trimmed.slice(22).trim();
        if (substr === "") {
          tests.push({
            name: state.name,
            source: state.sourceLines.join("\n").trim(),
            skipEmitJs: state.skipEmitJs,
          skipPlayground: state.skipPlayground,
            expect: { type: "type-error" },
          });
        } else {
          tests.push({
            name: state.name,
            source: state.sourceLines.join("\n").trim(),
            skipEmitJs: state.skipEmitJs,
          skipPlayground: state.skipPlayground,
            expect: { type: "type-error-msg", substring: substr },
          });
        }
        state = null;
      }
    } else if (trimmed === "--- expect-type-error") {
      if (state) {
        tests.push({
          name: state.name,
          source: state.sourceLines.join("\n").trim(),
          skipEmitJs: state.skipEmitJs,
          skipPlayground: state.skipPlayground,
          expect: { type: "type-error" },
        });
        state = null;
      }
    } else if (trimmed.startsWith("--- expect-runtime-error:")) {
      if (state) {
        const substr = trimmed.slice(25).trim();
        tests.push({
          name: state.name,
          source: state.sourceLines.join("\n").trim(),
          skipEmitJs: state.skipEmitJs,
          skipPlayground: state.skipPlayground,
          expect: { type: "runtime-error", substring: substr },
        });
        state = null;
      }
    } else if (state) {
      if (trimmed.startsWith("===") && trimmed.endsWith("===")) continue;
      if (state.sourceLines.length > 0 || trimmed !== "") {
        state.sourceLines.push(line);
      }
    }
  }

  return tests;
}

// Parse runner CLI args: positional .tests files plus -t "name" filters.
function parseArgs(argv) {
  const fileArgs = [];
  const filters = [];
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "-t" && i + 1 < argv.length) {
      filters.push(argv[i + 1].toLowerCase());
      i++;
    } else {
      fileArgs.push(argv[i]);
    }
  }
  return { fileArgs, filters };
}

function makeFilter(filters) {
  return (name) => {
    if (filters.length === 0) return true;
    const lower = name.toLowerCase();
    return filters.some((f) => lower.includes(f));
  };
}

module.exports = { parseTestFile, parseArgs, makeFilter };
