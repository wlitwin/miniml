// The .tests file format — THE single JS implementation, shared by every
// node-based test runner (run_emit_js.js, run_playground.js).
// The single OCaml implementation is cross_test/test_format.ml. Any format
// change must update exactly those two files.
//
// Format (see test_format.ml for the full reference):
//   --- test: <name>
//   <MiniML source lines>
//   --- expect: <value>            (quoted form "..." preserves whitespace and
//                                   interprets \n and \\ escapes)
//   --- expect-type-error[: substr]
//   --- expect-runtime-error: substr
//
// Per-backend directives (between source and expectation):
//   --- skip-<backend>: <reason>     skip on that backend (must reference a bug)
//   --- expect-<backend>: <value>    backend-specific expected output
//
// Parsed tests expose: { name, source, expect, skips: {backend: reason},
// expectOverrides: {backend: value} }.

const fs = require("fs");

// Interpret a raw expected value: the quoted form preserves whitespace and
// supports \n (newline) and \\ (backslash) escapes; the bare form is literal.
function parseExpectedValue(raw) {
  if (raw.length >= 2 && raw[0] === '"' && raw[raw.length - 1] === '"') {
    const inner = raw.slice(1, -1);
    let out = "";
    for (let i = 0; i < inner.length; i++) {
      if (inner[i] === "\\" && i + 1 < inner.length) {
        const c = inner[i + 1];
        if (c === "n") {
          out += "\n";
          i++;
        } else if (c === "\\") {
          out += "\\";
          i++;
        } else {
          out += inner[i];
        }
      } else {
        out += inner[i];
      }
    }
    return out;
  }
  return raw;
}

// "--- skip-native: reason" with prefix "--- skip-" -> ["native", "reason"]
function parseBackendDirective(prefix, line) {
  if (!line.startsWith(prefix)) return null;
  const colon = line.indexOf(":", prefix.length);
  if (colon <= prefix.length) return null;
  const backend = line.slice(prefix.length, colon);
  const rest = line.slice(colon + 1).trim();
  return [backend, rest];
}

function parseTestFile(filename) {
  const content = fs.readFileSync(filename, "utf-8");
  const lines = content.split("\n");
  const tests = [];
  let state = null; // null | { name, sourceLines, skips, expectOverrides }

  const push = (expect) => {
    tests.push({
      name: state.name,
      source: state.sourceLines.join("\n").trim(),
      skips: state.skips,
      expectOverrides: state.expectOverrides,
      expect,
    });
    state = null;
  };

  for (const line of lines) {
    const trimmed = line.trim();

    if (trimmed.startsWith("--- test:")) {
      const name = trimmed.slice(9).trim();
      state = { name, sourceLines: [], skips: {}, expectOverrides: {} };
    } else if (line.startsWith("--- expect: ")) {
      if (state) push({ type: "value", value: parseExpectedValue(line.slice(12)) });
    } else if (trimmed.startsWith("--- expect-type-error:")) {
      if (state) {
        const substr = trimmed.slice(22).trim();
        if (substr === "") push({ type: "type-error" });
        else push({ type: "type-error-msg", substring: substr });
      }
    } else if (trimmed === "--- expect-type-error") {
      if (state) push({ type: "type-error" });
    } else if (trimmed.startsWith("--- expect-runtime-error:")) {
      if (state) push({ type: "runtime-error", substring: trimmed.slice(25).trim() });
    } else {
      const skip = parseBackendDirective("--- skip-", line);
      const override = skip ? null : parseBackendDirective("--- expect-", line);
      if (skip) {
        if (state) state.skips[skip[0]] = skip[1];
      } else if (override) {
        if (state) state.expectOverrides[override[0]] = parseExpectedValue(override[1]);
      } else if (state) {
        // Source line: skip section banners, blank lines before the first
        // source line, and unrecognized directives.
        if (trimmed.startsWith("===") && trimmed.endsWith("===")) continue;
        if (trimmed.startsWith("--- ")) continue;
        if (state.sourceLines.length > 0 || trimmed !== "") {
          state.sourceLines.push(line);
        }
      }
    }
  }

  return tests;
}

// Why this test should be skipped on [backend], or undefined.
function skipReason(tc, backend) {
  return tc.skips ? tc.skips[backend] : undefined;
}

// The expected value [backend] should check: its --- expect-<backend>:
// override when present, the base expectation otherwise.
function expectedFor(tc, backend) {
  if (tc.expectOverrides && backend in tc.expectOverrides) {
    return { type: "value", value: tc.expectOverrides[backend] };
  }
  return tc.expect;
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

module.exports = { parseTestFile, parseArgs, makeFilter, skipReason, expectedFor };
