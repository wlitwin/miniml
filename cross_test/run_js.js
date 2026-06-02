#!/usr/bin/env node
// Cross-VM test runner — JS VM backend
// Parses .t test files and runs each test case through the JS VM,
// comparing ppValue output against expected results.
//
// The slow step — spawning main.exe to compile each test to a bytecode
// bundle — runs concurrently on a worker pool (see parallel.js). The bundle
// is then executed in-process on the JS VM; that part is synchronous
// JavaScript, so VM runs are naturally serialized and globalThis._vmOutput
// never interleaves. Output is printed in source order — identical to a
// sequential run.

const fs = require("fs");
const os = require("os");
const path = require("path");
const { loadBundle } = require("../js/loader");
const vm = require("../js/vm");
const { runPool, execFileRetry } = require("./parallel");

const INTERPRETER = path.resolve(
  __dirname,
  "..",
  "_build",
  "default",
  "bin",
  "main.exe"
);

const TMP_DIR = fs.mkdtempSync(path.join(os.tmpdir(), "crossvm_test_"));
process.on("exit", () => {
  try { fs.rmSync(TMP_DIR, { recursive: true, force: true }); } catch (_) {}
});

// --- Parsing -------------------------------------------------------------
// The .tests parser is shared across all node runners: see test_parser.js.

const { parseTestFile } = require("./test_parser");

// --- Running -------------------------------------------------------------

function pass(name) {
  return { output: `  PASS: ${name}\n`, status: "pass", name };
}

function fail(name, msg) {
  return { output: `  FAIL: ${name}\n    ${msg}\n`, status: "fail", name };
}

// Async compile: spawn main.exe --emit-json; resolves to {ok, stdout, stderr}.
// Retries if the process is killed by the environment (see execFileRetry).
function compile(tmpFile) {
  return execFileRetry(INTERPRETER, ["--emit-json", tmpFile]);
}

// Run a compiled bundle on the in-process JS VM. Synchronous: no other
// worker's VM run can interleave with this one.
function runBundleForValue(name, json, expected) {
  const outputs = [];
  globalThis._vmOutput = (s) => outputs.push(s);

  let result;
  try {
    result = loadBundle(json);
  } catch (e) {
    globalThis._vmOutput = null;
    if (e instanceof vm.RuntimeError) {
      return fail(name, `expected value, got runtime error: ${e.message}`);
    }
    throw e;
  }
  globalThis._vmOutput = null;

  // Build actual output (same logic as js/test.js)
  let actual;
  if (outputs.length > 0 && result.tag === "unit") {
    actual = outputs.join("\n");
  } else if (outputs.length > 0) {
    actual = outputs.join("\n") + "\n" + vm.ppValue(result);
  } else {
    actual = vm.ppValue(result);
  }

  if (actual === expected) {
    return pass(name);
  }
  return fail(
    name,
    `expected: ${JSON.stringify(expected)}\n    actual:   ${JSON.stringify(actual)}`
  );
}

async function runTest(tc, idx) {
  const tmpFile = path.join(TMP_DIR, `test_${idx}.ml`);
  fs.writeFileSync(tmpFile, tc.source);

  try {
    switch (tc.expect.type) {
      case "value": {
        const c = await compile(tmpFile);
        if (!c.ok) {
          return fail(
            tc.name,
            `expected value, got compile error: ${(c.stderr || c.error.message).trim()}`
          );
        }
        return runBundleForValue(tc.name, c.stdout, tc.expect.value);
      }
      case "type-error": {
        // Type errors are caught at compilation time
        const c = await compile(tmpFile);
        if (c.ok) {
          return fail(tc.name, "expected type error, but compilation succeeded");
        }
        const stderr = c.stderr || c.error.message;
        if (stderr.includes("Type error") || stderr.includes("Type_error")) {
          return pass(tc.name);
        }
        return fail(tc.name, `expected type error, got: ${stderr.trim()}`);
      }
      case "type-error-msg": {
        // Type errors with substring matching
        const c = await compile(tmpFile);
        if (c.ok) {
          return fail(tc.name, "expected type error, but compilation succeeded");
        }
        const stderr = c.stderr || c.error.message;
        if (
          (stderr.includes("Type error") || stderr.includes("Type_error")) &&
          stderr.includes(tc.expect.substring)
        ) {
          return pass(tc.name);
        }
        return fail(
          tc.name,
          `expected type error containing ${JSON.stringify(tc.expect.substring)}, got: ${stderr.trim()}`
        );
      }
      case "runtime-error": {
        const c = await compile(tmpFile);
        if (!c.ok) {
          return fail(
            tc.name,
            `expected runtime error, got compile error: ${(c.stderr || c.error.message).trim()}`
          );
        }

        globalThis._vmOutput = () => {};
        try {
          loadBundle(c.stdout);
          globalThis._vmOutput = null;
          return fail(tc.name, "expected runtime error, but succeeded");
        } catch (e) {
          globalThis._vmOutput = null;
          if (
            e instanceof vm.RuntimeError &&
            e.message.includes(tc.expect.substring)
          ) {
            return pass(tc.name);
          }
          return fail(
            tc.name,
            `expected runtime error containing ${JSON.stringify(tc.expect.substring)}, got: ${e.message}`
          );
        }
      }
      default:
        return fail(tc.name, `unknown expectation type: ${tc.expect.type}`);
    }
  } catch (e) {
    return fail(tc.name, `exception: ${e.message}`);
  } finally {
    try {
      fs.unlinkSync(tmpFile);
    } catch (_) {}
  }
}

// --- Main ----------------------------------------------------------------

function findTestFiles(dir) {
  return fs
    .readdirSync(dir)
    .filter((f) => f.endsWith(".tests"))
    .sort()
    .map((f) => path.join(dir, f));
}

// Parse args: separate files from -t filters
const args = process.argv.slice(2);
const fileArgs = [];
const filters = [];
for (let i = 0; i < args.length; i++) {
  if (args[i] === "-t" && i + 1 < args.length) {
    filters.push(args[i + 1].toLowerCase());
    i++;
  } else {
    fileArgs.push(args[i]);
  }
}

let files;
if (fileArgs.length > 0) {
  files = fileArgs;
} else {
  const testsDir = path.join(__dirname, "tests");
  files = findTestFiles(testsDir);
}

if (files.length === 0) {
  console.error("No test files found. Usage: run_js.js [file.tests ...] [-t \"name\" ...]");
  process.exit(1);
}

function matchesFilter(name) {
  if (filters.length === 0) return true;
  const lower = name.toLowerCase();
  return filters.some((f) => lower.includes(f));
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
  const result = await runTest(item.tc, idx);
  result.output = item.prefix + result.output + item.suffix;
  return result;
}

(async () => {
  const results = await runPool(items, worker);

  let passed = 0;
  let failed = 0;
  const failures = [];
  for (const r of results) {
    if (!r || !r.name) continue;
    if (r.status === "pass") passed++;
    else if (r.status === "fail") {
      failed++;
      failures.push(r.name);
    }
  }

  console.log("==============================");
  console.log(
    `${passed}/${passed + failed} cross-VM tests passed (js)${failed > 0 ? ` (${failed} FAILED)` : ""}`
  );
  if (failures.length > 0) {
    console.log("Failures:");
    for (const name of failures) {
      console.log(`  - ${name}`);
    }
    process.exit(1);
  }
})();
