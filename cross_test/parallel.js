// Shared worker pool for cross-test runners.
//
// The per-test cost of the subprocess-based runners (run_emit_js.js,
// run_playground.js)
// is dominated by process spawns of main.exe / node, not by the tests
// themselves, so running tests concurrently gives a near-linear wall-clock
// speedup. Results are PRINTED IN SOURCE ORDER as they become available, so
// the output is byte-identical to a sequential run (same approach as
// native_test/runner.ml's fork pool).
//
// Concurrency: CROSS_TEST_JOBS env var, default max(1, cpus - 2).
// CROSS_TEST_JOBS=1 gives a fully sequential, deterministic-timing run.

const os = require("os");
const { execFile } = require("child_process");

function defaultJobs() {
  const env = parseInt(process.env.CROSS_TEST_JOBS || "", 10);
  if (Number.isFinite(env) && env > 0) return env;
  return Math.max(1, os.cpus().length - 2);
}

// execFile wrapper that retries when the child is KILLED BY THE ENVIRONMENT
// (SIGKILL — e.g. macOS jetsam under memory pressure while the gate shares the
// machine with other work). Being killed is not a test result; a real test
// failure (non-zero exit, error output) or a timeout (SIGTERM from the timeout
// option) is never retried. Resolves to {ok, stdout, stderr, status, error}.
function execFileRetry(cmd, args, opts = {}, retries = 3) {
  return new Promise((resolve) => {
    const attempt = (left) => {
      execFile(
        cmd,
        args,
        { encoding: "utf-8", maxBuffer: 10 * 1024 * 1024, ...opts },
        (error, stdout, stderr) => {
          if (
            error &&
            error.signal === "SIGKILL" &&
            !error.killed && // not our own timeout enforcement
            left > 0
          ) {
            // Killed by the environment: back off briefly and retry.
            setTimeout(() => attempt(left - 1), 500 * (retries - left + 1));
            return;
          }
          resolve({
            ok: !error,
            stdout: stdout || "",
            stderr: stderr || "",
            status: error ? error.code : 0,
            error,
          });
        }
      );
    };
    attempt(retries);
  });
}

// Run `worker(item, index)` over every item with bounded concurrency.
// The worker resolves to a result object; if it has a string `output` field,
// that text is streamed to stdout in item order. Returns all results in item
// order once every worker has finished.
async function runPool(items, worker, opts = {}) {
  const jobs = Math.max(1, opts.jobs || defaultJobs());
  const results = new Array(items.length);
  const done = new Array(items.length).fill(false);
  let nextToPrint = 0;
  let nextToStart = 0;

  function flush() {
    while (nextToPrint < items.length && done[nextToPrint]) {
      const r = results[nextToPrint];
      if (r && typeof r.output === "string" && r.output.length > 0) {
        process.stdout.write(r.output);
      }
      nextToPrint++;
    }
  }

  async function pump() {
    while (nextToStart < items.length) {
      const i = nextToStart++;
      try {
        results[i] = await worker(items[i], i);
      } catch (e) {
        // A worker must not throw; treat it as a failed result so one
        // unexpected exception doesn't abort the whole run.
        results[i] = {
          output: `  FAIL: ${items[i] && items[i].name ? items[i].name : `item ${i}`}\n    runner exception: ${e.message}\n`,
          status: "fail",
          name: items[i] && items[i].name,
        };
      }
      done[i] = true;
      flush();
    }
  }

  const pumps = [];
  for (let w = 0; w < Math.min(jobs, items.length); w++) pumps.push(pump());
  await Promise.all(pumps);
  flush();
  return results;
}

// Run compiled MiniML JS in an isolated in-process vm context instead of a
// child node process. No process spawn means no exposure to environmental
// kills (jetsam SIGKILLs, lost pipe output under memory pressure) — the
// failure modes that made gate runs flaky on a loaded machine. Each call gets
// a fresh context (fresh JS built-ins, no cross-test state).
//
// The sandbox passes through `require` and `process` so compiled IO/Sys
// builtins (fs reads/writes, getenv) behave exactly as they would in a child
// process. Output goes through the _jsOutput hook; stdout is reconstructed
// from the hook calls (print sends bare strings, println sends
// newline-terminated ones — both become one line each, matching what
// process.stdout.write would have produced).
//
// Resolves to { ok, stdout, stderr, status } — the same shape as a child
// process run, so callers can switch transparently.
const nodeVm = require("node:vm");

function runJsInProcess(jsCode, opts = {}) {
  const timeout = opts.timeout || 15000;
  const outputs = [];
  const sandbox = {
    _jsOutput: (s) => outputs.push(s.endsWith("\n") ? s : s + "\n"),
    require,
    process,
    console,
    Buffer, // node global, not a vm intrinsic — IO byte ops (Buffer.from/alloc) need it
  };
  const ctx = nodeVm.createContext(sandbox);
  // The program emits MiniML strings as latin1 byte strings (1 char = 1 byte,
  // holding UTF-8); reconstruct stdout as a byte stream and decode it as UTF-8,
  // exactly as a terminal would, so it matches the UTF-8-read expected value.
  const stdout = () =>
    Buffer.from(outputs.join(""), "latin1").toString("utf-8");
  try {
    nodeVm.runInContext(jsCode, ctx, { timeout });
    return Promise.resolve({ ok: true, stdout: stdout(), stderr: "", status: 0 });
  } catch (e) {
    return Promise.resolve({
      ok: false,
      stdout: stdout(),
      stderr: e && e.message ? e.message : String(e),
      status: 1,
    });
  }
}

module.exports = { runPool, defaultJobs, execFileRetry, runJsInProcess };
