#!/usr/bin/env node
// Cross-VM test runner — JS VM backend
// Parses .t test files and runs each test case through the JS VM,
// comparing ppValue output against expected results.

const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const { loadBundle } = require("../js/loader");
const vm = require("../js/vm");

const INTERPRETER = path.resolve(
  __dirname,
  "..",
  "_build",
  "default",
  "bin",
  "main.exe"
);

// --- Parsing -------------------------------------------------------------

function parseTestFile(filename) {
  const content = fs.readFileSync(filename, "utf-8");
  const lines = content.split("\n");
  const tests = [];
  let state = null; // null | { name, sourceLines }

  for (const line of lines) {
    const trimmed = line.trim();

    if (trimmed.startsWith("--- test:")) {
      const name = trimmed.slice(9).trim();
      state = { name, sourceLines: [] };
    } else if (line.startsWith("--- expect: ")) {
      if (state) {
        let expected = line.slice(12); // preserve trailing whitespace
        // Strip outer quotes if present (for whitespace-sensitive values)
        if (expected.length >= 2 && expected[0] === '"' && expected[expected.length - 1] === '"') {
          expected = expected.slice(1, -1);
        }
        tests.push({
          name: state.name,
          source: state.sourceLines.join("\n").trim(),
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
            expect: { type: "type-error" },
          });
        } else {
          tests.push({
            name: state.name,
            source: state.sourceLines.join("\n").trim(),
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
          expect: { type: "runtime-error", substring: substr },
        });
        state = null;
      }
    } else if (state) {
      // Skip section headers, collect source lines
      if (trimmed.startsWith("===") && trimmed.endsWith("===")) continue;
      if (state.sourceLines.length > 0 || trimmed !== "") {
        state.sourceLines.push(line);
      }
    }
  }

  return tests;
}

// --- Running -------------------------------------------------------------

let passed = 0;
let failed = 0;
const failures = [];

function fail(name, msg) {
  console.log(`  FAIL: ${name}`);
  console.log(`    ${msg}`);
  failed++;
  failures.push(name);
}

function runTest(tc) {
  const tmpFile = `/tmp/crossvm_test_${process.pid}.ml`;
  fs.writeFileSync(tmpFile, tc.source);

  try {
    switch (tc.expect.type) {
      case "value": {
        // Compile to JSON bundle
        const json = execSync(`${INTERPRETER} --emit-json ${tmpFile}`, {
          encoding: "utf-8",
          maxBuffer: 10 * 1024 * 1024,
        });

        // Capture output
        const outputs = [];
        globalThis._vmOutput = (s) => outputs.push(s);

        let result;
        try {
          result = loadBundle(json);
        } catch (e) {
          if (e instanceof vm.RuntimeError) {
            fail(
              tc.name,
              `expected value, got runtime error: ${e.message}`
            );
            globalThis._vmOutput = null;
            return;
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
        // Type errors are caught at compilation time
        try {
          execSync(`${INTERPRETER} --emit-json ${tmpFile}`, {
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
        // Type errors with substring matching
        try {
          execSync(`${INTERPRETER} --emit-json ${tmpFile}`, {
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
        const json = execSync(`${INTERPRETER} --emit-json ${tmpFile}`, {
          encoding: "utf-8",
          maxBuffer: 10 * 1024 * 1024,
        });

        globalThis._vmOutput = () => {};
        try {
          loadBundle(json);
          fail(tc.name, "expected runtime error, but succeeded");
        } catch (e) {
          if (
            e instanceof vm.RuntimeError &&
            e.message.includes(tc.expect.substring)
          ) {
            console.log(`  PASS: ${tc.name}`);
            passed++;
          } else {
            fail(
              tc.name,
              `expected runtime error containing ${JSON.stringify(tc.expect.substring)}, got: ${e.message}`
            );
          }
        }
        globalThis._vmOutput = null;
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

for (const file of files) {
  console.log(`=== ${path.basename(file)} ===`);
  const tests = parseTestFile(file).filter((tc) => matchesFilter(tc.name));
  if (tests.length === 0 && filters.length > 0) {
    console.log("  (no matching tests)\n");
    continue;
  }
  for (const tc of tests) {
    runTest(tc);
  }
  console.log();
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
