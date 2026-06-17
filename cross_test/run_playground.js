#!/usr/bin/env node
// Cross-VM test runner — playground path (self-host emit-js)
//
// Validates the EXACT path the web playground's "JS compiler → JS output" mode
// uses: the self-hosted compiler compiled to JS (js/compiler_native.js)
// compiles each test program to JavaScript, which then runs in an isolated
// node process.
//
// This is the only automated coverage of self_host/js_codegen.mml: the parity
// runner (compiler_test/parity_runner.ml) exercises the self-host BYTECODE
// path on the OCaml VM, not the JS codegen. Run `make self-host-compile-native-js`
// (or `make test-playground`, which depends on it) to refresh compiler_native.js
// before running this.
//
// Concurrency model (see parallel.js): compilation runs IN-PROCESS and is
// synchronous JavaScript, so it is naturally serialized by the event loop —
// the shared globalThis._js* compiler state never interleaves. The compiled
// tests also execute IN-PROCESS, each in an isolated vm context. This runner
// spawns no child processes at all, which makes it immune to the
// environmental process kills (jetsam under memory pressure) that made gate
// runs flaky on a loaded machine.

const fs = require("fs");
const path = require("path");
const { parseTestFile, parseArgs, makeFilter, skipReason } = require("./test_parser");
const { runPool, runJsInProcess } = require("./parallel");

const ROOT = path.resolve(__dirname, "..");
const COMPILER_NATIVE = path.join(ROOT, "js", "compiler_native.js");

// --- Self-host compiler (in-process, with setup cache) --------------------

if (!fs.existsSync(COMPILER_NATIVE)) {
  console.error(
    `${COMPILER_NATIVE} not found — run 'make self-host-compile-native-js' first`
  );
  process.exit(1);
}

const compilerJs = fs.readFileSync(COMPILER_NATIVE, "utf-8");
// Parse the compiler once into a reusable Function. Each call re-runs it from
// scratch (fresh compiler state), but its MiniML-level stdlib setup is cached
// across calls via globalThis._mmlCompilerCache (the __cache_* externs) —
// exactly how the browser playground keeps repeat compiles fast.
const compilerFn = new Function(compilerJs);

// Synchronous: callers rely on the event loop to serialize compiles (the
// globalThis._js* state must never be shared by two in-flight compiles).
function selfhostCompile(source) {
  const out = [];
  globalThis._jsSysArgs = ["compiler", "--emit-js", "input.mml"];
  // The self-host compiler treats source as latin1 BYTES (UTF-8 held one byte
  // per char), matching how the real `mml` reads files — so hand it the bytes,
  // not a UTF-16 JS string (which would corrupt non-ASCII content).
  globalThis._jsReadFile = (f) => {
    if (f === "input.mml") return Buffer.from(source, "utf-8").toString("latin1");
    return fs.readFileSync(path.join(ROOT, f), "latin1"); // stdlib/*.mml
  };
  globalThis._jsOutput = (s) => out.push(s);
  try {
    compilerFn();
  } finally {
    globalThis._jsSysArgs = null;
    globalThis._jsReadFile = null;
    globalThis._jsOutput = null;
  }
  return out.join("");
}

// --- Running ---------------------------------------------------------------

function pass(name) {
  return { output: `  PASS: ${name}\n`, status: "pass", name };
}

function fail(name, msg) {
  return { output: `  FAIL: ${name}\n    ${msg}\n`, status: "fail", name };
}

// Compiled test JS runs in an isolated in-process vm context (see
// runJsInProcess) — no child processes anywhere in this runner, so it is
// immune to environmental process kills under memory pressure.
async function runTest(tc, idx) {
  switch (tc.expect.type) {
    case "value": {
      let jsCode;
      try {
        jsCode = selfhostCompile(tc.source);
      } catch (e) {
        return fail(tc.name, `compilation failed: ${(e.message || String(e)).split("\n")[0]}`);
      }
      const res = await runJsInProcess(jsCode, { timeout: 15000 });
      if (!res.ok) {
        return fail(
          tc.name,
          `runtime error:\n    ${res.stderr.trim()}\n    stdout: ${res.stdout.trim()}`
        );
      }
      const actual = res.stdout.replace(/\n$/, "");
      if (actual === tc.expect.value) return pass(tc.name);
      return fail(
        tc.name,
        `expected: ${JSON.stringify(tc.expect.value)}\n    actual:   ${JSON.stringify(actual)}`
      );
    }
    case "type-error": {
      // The self-host compiler reports errors by throwing; the message is the
      // raw typechecker/parser message (no "Type error:" prefix).
      try {
        selfhostCompile(tc.source);
        return fail(tc.name, "expected type error, but compilation succeeded");
      } catch (e) {
        return pass(tc.name);
      }
    }
    case "type-error-msg": {
      try {
        selfhostCompile(tc.source);
        return fail(tc.name, "expected type error, but compilation succeeded");
      } catch (e) {
        const msg = e.message || String(e);
        if (msg.includes(tc.expect.substring)) return pass(tc.name);
        return fail(
          tc.name,
          `expected error containing ${JSON.stringify(tc.expect.substring)}, got: ${msg.split("\n")[0]}`
        );
      }
    }
    case "runtime-error": {
      let jsCode;
      try {
        jsCode = selfhostCompile(tc.source);
      } catch (e) {
        // Compilation failure is acceptable for runtime-error tests if the
        // compiler catches the problem early (matches run_emit_js.js).
        return pass(tc.name);
      }
      const res = await runJsInProcess(jsCode, { timeout: 15000 });
      if (res.ok) return fail(tc.name, "expected runtime error, but succeeded");
      return pass(tc.name); // any error counts (messages differ across backends)
    }
    default:
      return fail(tc.name, `unknown expectation type: ${tc.expect.type}`);
  }
}

// --- Main -------------------------------------------------------------------

const { fileArgs, filters } = parseArgs(process.argv.slice(2));
const matchesFilter = makeFilter(filters);

let files;
if (fileArgs.length > 0) {
  files = fileArgs;
} else {
  const testsDir = path.join(__dirname, "tests");
  files = fs
    .readdirSync(testsDir)
    .filter((f) => f.endsWith(".tests"))
    .sort()
    .map((f) => path.join(testsDir, f));
}

if (files.length === 0) {
  console.error(
    'No test files found. Usage: run_playground.js [file.tests ...] [-t "name" ...]'
  );
  process.exit(1);
}

// Flatten every file's tests into one work list (see run_emit_js.js).
const items = [];
for (const file of files) {
  const tests = parseTestFile(file).filter((tc) => matchesFilter(tc.name));
  const header = `=== ${path.basename(file)} ===\n`;
  if (tests.length === 0) {
    items.push({
      kind: "empty-file",
      output:
        header + (filters.length > 0 ? "  (no matching tests)\n" : "") + "\n",
    });
    continue;
  }
  tests.forEach((tc, i) => {
    items.push({
      kind: "test",
      tc,
      prefix: i === 0 ? header : "",
      suffix: i === tests.length - 1 ? "\n" : "",
    });
  });
}

async function worker(item, idx) {
  if (item.kind === "empty-file") {
    return { output: item.output, status: "skip", name: null };
  }
  const tc = item.tc;
  const skip = skipReason(tc, "emit-js") || skipReason(tc, "playground");
  let result;
  if (skip) {
    result = { output: `  SKIP: ${tc.name} (${skip})\n`, status: "skip", name: tc.name };
  } else {
    result = await runTest(tc, idx);
  }
  result.output = item.prefix + result.output + item.suffix;
  return result;
}

(async () => {
  const t0 = Date.now();
  const results = await runPool(items, worker);

  let passed = 0;
  let failed = 0;
  let skipped = 0;
  const failures = [];
  for (const r of results) {
    if (!r || !r.name) continue;
    if (r.status === "pass") passed++;
    else if (r.status === "fail") {
      failed++;
      failures.push(r.name);
    } else if (r.status === "skip") skipped++;
  }

  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  console.log("==============================");
  console.log(
    `${passed}/${passed + failed} cross-VM tests passed (playground) in ${elapsed}s${failed > 0 ? ` (${failed} FAILED)` : ""}${skipped > 0 ? ` (${skipped} skipped)` : ""}`
  );
  if (failures.length > 0) {
    console.log("Failures:");
    for (const name of failures) {
      console.log(`  - ${name}`);
    }
    process.exit(1);
  }
})();
