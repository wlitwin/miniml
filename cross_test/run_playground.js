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

const { execFileSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const { parseTestFile, parseArgs, makeFilter, skipReason } = require("./test_parser");

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

function selfhostCompile(source) {
  const out = [];
  globalThis._jsSysArgs = ["compiler", "--emit-js", "input.mml"];
  globalThis._jsReadFile = (f) => {
    if (f === "input.mml") return source;
    return fs.readFileSync(path.join(ROOT, f), "utf-8"); // stdlib/*.mml
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

let passed = 0;
let failed = 0;
let skipped = 0;
const failures = [];

function fail(name, msg) {
  console.log(`  FAIL: ${name}`);
  console.log(`    ${msg}`);
  failed++;
  failures.push(name);
}

function pass(name) {
  console.log(`  PASS: ${name}`);
  passed++;
}

const tmpJs = `/tmp/playground_test_${process.pid}.js`;

// Run compiled JS in an isolated node process (for timeouts and crash
// isolation), returning { ok, stdout, stderr, status }.
function runCompiledJs(jsCode) {
  fs.writeFileSync(tmpJs, jsCode);
  try {
    const stdout = execFileSync("node", [tmpJs], {
      encoding: "utf-8",
      maxBuffer: 10 * 1024 * 1024,
      timeout: 15000,
      stdio: ["pipe", "pipe", "pipe"],
    });
    return { ok: true, stdout };
  } catch (e) {
    return {
      ok: false,
      stdout: e.stdout ? e.stdout.toString() : "",
      stderr: e.stderr ? e.stderr.toString() : e.message,
      status: e.status,
    };
  }
}

function runTest(tc) {
  switch (tc.expect.type) {
    case "value": {
      let jsCode;
      try {
        jsCode = selfhostCompile(tc.source);
      } catch (e) {
        fail(tc.name, `compilation failed: ${(e.message || String(e)).split("\n")[0]}`);
        return;
      }
      const res = runCompiledJs(jsCode);
      if (!res.ok) {
        fail(
          tc.name,
          `runtime error (exit ${res.status}):\n    stderr: ${res.stderr.trim()}\n    stdout: ${res.stdout.trim()}`
        );
        return;
      }
      const actual = res.stdout.replace(/\n$/, "");
      if (actual === tc.expect.value) pass(tc.name);
      else
        fail(
          tc.name,
          `expected: ${JSON.stringify(tc.expect.value)}\n    actual:   ${JSON.stringify(actual)}`
        );
      break;
    }
    case "type-error": {
      // The self-host compiler reports errors by throwing; the message is the
      // raw typechecker/parser message (no "Type error:" prefix).
      try {
        selfhostCompile(tc.source);
        fail(tc.name, "expected type error, but compilation succeeded");
      } catch (e) {
        pass(tc.name);
      }
      break;
    }
    case "type-error-msg": {
      try {
        selfhostCompile(tc.source);
        fail(tc.name, "expected type error, but compilation succeeded");
      } catch (e) {
        const msg = e.message || String(e);
        if (msg.includes(tc.expect.substring)) pass(tc.name);
        else
          fail(
            tc.name,
            `expected error containing ${JSON.stringify(tc.expect.substring)}, got: ${msg.split("\n")[0]}`
          );
      }
      break;
    }
    case "runtime-error": {
      let jsCode;
      try {
        jsCode = selfhostCompile(tc.source);
      } catch (e) {
        // Compilation failure is acceptable for runtime-error tests if the
        // compiler catches the problem early (matches run_emit_js.js).
        pass(tc.name);
        return;
      }
      const res = runCompiledJs(jsCode);
      if (res.ok) fail(tc.name, "expected runtime error, but succeeded");
      else pass(tc.name); // any non-zero exit counts (messages differ across backends)
      break;
    }
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

const t0 = Date.now();
for (const file of files) {
  console.log(`=== ${path.basename(file)} ===`);
  const tests = parseTestFile(file).filter((tc) => matchesFilter(tc.name));
  if (tests.length === 0 && filters.length > 0) {
    console.log("  (no matching tests)\n");
    continue;
  }
  for (const tc of tests) {
    const skip = skipReason(tc, "emit-js") || skipReason(tc, "playground");
    if (skip) {
      console.log(`  SKIP: ${tc.name} (${skip})`);
      skipped++;
      continue;
    }
    runTest(tc);
  }
  console.log();
}
try { fs.unlinkSync(tmpJs); } catch (_) {}

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
