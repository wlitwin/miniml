#!/usr/bin/env node
// Cross-VM test runner — emit-js backend
// Parses .tests files and runs each test case through --emit-js + node,
// comparing printed output against expected results.
//
// Tests run concurrently on a worker pool (see parallel.js); each test spawns
// main.exe to compile (retried on environmental kills), then executes the
// compiled JS in an isolated in-process vm context — no node child processes.
// Output is printed in source order — identical to a sequential run.

const fs = require("fs");
const os = require("os");
const path = require("path");
const { parseTestFile, parseArgs, makeFilter, skipReason } = require("./test_parser");
const { runPool, execFileRetry, runJsInProcess } = require("./parallel");

const INTERPRETER = path.resolve(
  __dirname,
  "..",
  "_build",
  "default",
  "bin",
  "main.exe"
);

const TMP_DIR = fs.mkdtempSync(path.join(os.tmpdir(), "emit_js_test_"));
process.on("exit", () => {
  try { fs.rmSync(TMP_DIR, { recursive: true, force: true }); } catch (_) {}
});

// Promisified execFile that resolves to {ok, stdout, stderr, status} instead
// of rejecting, so callers can branch on success without try/catch noise.
// Retries if the process is killed by the environment (see execFileRetry).
function run(cmd, args, opts = {}) {
  return execFileRetry(cmd, args, opts);
}

// --- Running -------------------------------------------------------------

// Each runner returns {output, status} where status is "pass" | "fail" and
// output is the exact text a sequential runner would have printed.

function pass(name) {
  return { output: `  PASS: ${name}\n`, status: "pass", name };
}

function fail(name, msg) {
  return { output: `  FAIL: ${name}\n    ${msg}\n`, status: "fail", name };
}

async function runTest(tc, idx) {
  const tmpFile = path.join(TMP_DIR, `test_${idx}.mml`);
  fs.writeFileSync(tmpFile, tc.source);

  try {
    switch (tc.expect.type) {
      case "value": {
        // Compile to JS (main.exe is a child process — retried on
        // environmental kills and on lost-pipe empty output)
        let compile = await run(INTERPRETER, ["--emit-js", tmpFile]);
        for (
          let retry = 0;
          retry < 3 && compile.ok && compile.stdout === "";
          retry++
        ) {
          compile = await run(INTERPRETER, ["--emit-js", tmpFile]);
        }
        if (!compile.ok) {
          return fail(tc.name, `compilation failed: ${(compile.stderr || compile.error.message).trim()}`);
        }

        // Execute in an isolated in-process vm context (no child process)
        const exec = await runJsInProcess(compile.stdout, { timeout: 10000 });
        if (!exec.ok) {
          return fail(
            tc.name,
            `runtime error:\n    ${exec.stderr.trim()}\n    stdout: ${exec.stdout.trim()}`
          );
        }

        // Normalize: trim trailing newline
        const actual = exec.stdout.replace(/\n$/, "");

        if (actual === tc.expect.value) {
          return pass(tc.name);
        }
        return fail(
          tc.name,
          `expected: ${JSON.stringify(tc.expect.value)}\n    actual:   ${JSON.stringify(actual)}`
        );
      }
      case "type-error": {
        const compile = await run(INTERPRETER, ["--emit-js", tmpFile]);
        if (compile.ok) {
          return fail(tc.name, "expected type error, but compilation succeeded");
        }
        const stderr = compile.stderr || compile.error.message;
        if (stderr.includes("Type error") || stderr.includes("Type_error")) {
          return pass(tc.name);
        }
        return fail(tc.name, `expected type error, got: ${stderr.trim()}`);
      }
      case "type-error-msg": {
        const compile = await run(INTERPRETER, ["--emit-js", tmpFile]);
        if (compile.ok) {
          return fail(tc.name, "expected type error, but compilation succeeded");
        }
        const stderr = compile.stderr || compile.error.message;
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
        const compile = await run(INTERPRETER, ["--emit-js", tmpFile]);
        if (!compile.ok) {
          // Compilation failure is acceptable for runtime-error tests
          // if the compiler catches it early
          return pass(tc.name);
        }

        const exec = await runJsInProcess(compile.stdout, { timeout: 10000 });
        if (exec.ok) {
          return fail(tc.name, "expected runtime error, but succeeded");
        }
        // Accept any error as a runtime error
        // (error messages may differ between backends)
        return pass(tc.name);
      }
      default:
        return fail(tc.name, `unknown expectation type: ${tc.expect.type}`);
    }
  } catch (e) {
    return fail(tc.name, `exception: ${e.message}`);
  } finally {
    try { fs.unlinkSync(tmpFile); } catch (_) {}
  }
}

// --- Main ----------------------------------------------------------------

const { fileArgs, filters } = parseArgs(process.argv.slice(2));

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
  console.error("No test files found. Usage: run_emit_js.js [file.tests ...] [-t \"name\" ...]");
  process.exit(1);
}

const matchesFilter = makeFilter(filters);

// Flatten every file's tests into one work list. Each item knows whether it
// opens/closes its file's section so headers and spacing print exactly as the
// sequential runner did.
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
  const skip = skipReason(tc, "emit-js");
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
  const results = await runPool(items, worker);

  let passed = 0;
  let failed = 0;
  let skipped = 0;
  const failures = [];
  for (const r of results) {
    if (!r || !r.name) {
      continue; // empty-file placeholders
    }
    if (r.status === "pass") passed++;
    else if (r.status === "fail") {
      failed++;
      failures.push(r.name);
    } else if (r.status === "skip") skipped++;
  }

  console.log("==============================");
  console.log(
    `${passed}/${passed + failed} cross-VM tests passed (emit-js)${failed > 0 ? ` (${failed} FAILED)` : ""}${skipped > 0 ? ` (${skipped} skipped)` : ""}`
  );
  if (failures.length > 0) {
    console.log("Failures:");
    for (const name of failures) {
      console.log(`  - ${name}`);
    }
    process.exit(1);
  }
})();
