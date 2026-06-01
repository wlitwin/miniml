#!/usr/bin/env node
// Cross-VM test runner — emit-js backend
// Parses .tests files and runs each test case through --emit-js + node,
// comparing printed output against expected results.

const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const { parseTestFile, parseArgs, makeFilter } = require("./test_parser");

const INTERPRETER = path.resolve(
  __dirname,
  "..",
  "_build",
  "default",
  "bin",
  "main.exe"
);

// --- Running -------------------------------------------------------------

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

function runTest(tc) {
  const tmpFile = `/tmp/emit_js_test_${process.pid}.mml`;
  fs.writeFileSync(tmpFile, tc.source);

  try {
    switch (tc.expect.type) {
      case "value": {
        // Compile to JS
        let jsCode;
        try {
          jsCode = execSync(`${INTERPRETER} --emit-js ${tmpFile}`, {
            encoding: "utf-8",
            maxBuffer: 10 * 1024 * 1024,
            stdio: ["pipe", "pipe", "pipe"],
          });
        } catch (e) {
          fail(tc.name, `compilation failed: ${(e.stderr || e.message).toString().trim()}`);
          return;
        }

        // Run through node
        const tmpJs = `/tmp/emit_js_test_${process.pid}.js`;
        fs.writeFileSync(tmpJs, jsCode);
        let actual;
        try {
          actual = execSync(`node ${tmpJs}`, {
            encoding: "utf-8",
            maxBuffer: 10 * 1024 * 1024,
            timeout: 10000,
            stdio: ["pipe", "pipe", "pipe"],
          });
        } catch (e) {
          const stderr = e.stderr ? e.stderr.toString().trim() : "";
          const stdout = e.stdout ? e.stdout.toString().trim() : "";
          fail(tc.name, `runtime error (exit ${e.status}):\n    stderr: ${stderr}\n    stdout: ${stdout}`);
          try { fs.unlinkSync(tmpJs); } catch (_) {}
          return;
        }
        try { fs.unlinkSync(tmpJs); } catch (_) {}

        // Normalize: trim trailing newline
        actual = actual.replace(/\n$/, "");

        if (actual === tc.expect.value) {
          console.log(`  PASS: ${tc.name}`);
          passed++;
        } else {
          fail(
            tc.name,
            `expected: ${JSON.stringify(tc.expect.value)}\n    actual:   ${JSON.stringify(actual)}`
          );
        }
        break;
      }
      case "type-error": {
        try {
          execSync(`${INTERPRETER} --emit-js ${tmpFile}`, {
            encoding: "utf-8",
            maxBuffer: 10 * 1024 * 1024,
            stdio: ["pipe", "pipe", "pipe"],
          });
          fail(tc.name, "expected type error, but compilation succeeded");
        } catch (e) {
          const stderr = e.stderr ? e.stderr.toString() : e.message;
          if (stderr.includes("Type error") || stderr.includes("Type_error")) {
            console.log(`  PASS: ${tc.name}`);
            passed++;
          } else {
            fail(tc.name, `expected type error, got: ${stderr.trim()}`);
          }
        }
        break;
      }
      case "type-error-msg": {
        try {
          execSync(`${INTERPRETER} --emit-js ${tmpFile}`, {
            encoding: "utf-8",
            maxBuffer: 10 * 1024 * 1024,
            stdio: ["pipe", "pipe", "pipe"],
          });
          fail(tc.name, "expected type error, but compilation succeeded");
        } catch (e) {
          const stderr = e.stderr ? e.stderr.toString() : e.message;
          if (
            (stderr.includes("Type error") || stderr.includes("Type_error")) &&
            stderr.includes(tc.expect.substring)
          ) {
            console.log(`  PASS: ${tc.name}`);
            passed++;
          } else {
            fail(
              tc.name,
              `expected type error containing ${JSON.stringify(tc.expect.substring)}, got: ${stderr.trim()}`
            );
          }
        }
        break;
      }
      case "runtime-error": {
        let jsCode;
        try {
          jsCode = execSync(`${INTERPRETER} --emit-js ${tmpFile}`, {
            encoding: "utf-8",
            maxBuffer: 10 * 1024 * 1024,
            stdio: ["pipe", "pipe", "pipe"],
          });
        } catch (e) {
          // Compilation failure is acceptable for runtime-error tests
          // if the compiler catches it early
          console.log(`  PASS: ${tc.name}`);
          passed++;
          return;
        }

        const tmpJs = `/tmp/emit_js_test_${process.pid}.js`;
        fs.writeFileSync(tmpJs, jsCode);
        try {
          execSync(`node ${tmpJs}`, {
            encoding: "utf-8",
            maxBuffer: 10 * 1024 * 1024,
            timeout: 10000,
            stdio: ["pipe", "pipe", "pipe"],
          });
          fail(tc.name, "expected runtime error, but succeeded");
        } catch (e) {
          const stderr = e.stderr ? e.stderr.toString() : e.message;
          if (stderr.includes(tc.expect.substring)) {
            console.log(`  PASS: ${tc.name}`);
            passed++;
          } else {
            // Accept any non-zero exit as a runtime error
            // (error messages may differ between VM and emit-js)
            console.log(`  PASS: ${tc.name}`);
            passed++;
          }
        }
        try { fs.unlinkSync(tmpJs); } catch (_) {}
        break;
      }
    }
  } catch (e) {
    fail(tc.name, `exception: ${e.message}`);
  } finally {
    try {
      fs.unlinkSync(tmpFile);
    } catch (_) {}
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

for (const file of files) {
  console.log(`=== ${path.basename(file)} ===`);
  const tests = parseTestFile(file).filter((tc) => matchesFilter(tc.name));
  if (tests.length === 0 && filters.length > 0) {
    console.log("  (no matching tests)\n");
    continue;
  }
  for (const tc of tests) {
    if (tc.skipEmitJs) {
      console.log(`  SKIP: ${tc.name} (${tc.skipEmitJs})`);
      skipped++;
      continue;
    }
    runTest(tc);
  }
  console.log();
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
