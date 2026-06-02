//# allFunctionsCallOnLoad
// VM core — direct port of lib/vm.ml

class RuntimeError extends Error {
  constructor(msg) { super(msg); this.name = "RuntimeError"; }
}

function error(msg) { throw new RuntimeError(msg); }

// --- Value constructors ---
const VUNIT = { tag: "unit" };
const vint = (v) => ({ tag: "int", v });
const vfloat = (v) => ({ tag: "float", v });
const vbool = (v) => v ? VTRUE : VFALSE;
const VTRUE = { tag: "bool", v: true };
const VFALSE = { tag: "bool", v: false };
const vstring = (v) => ({ tag: "string", v });
const vbyte = (v) => ({ tag: "byte", v: v & 0xFF });
const vrune = (v) => ({ tag: "rune", v });
const vtuple = (vs) => ({ tag: "tuple", v: vs });
// Lists use linked cons cells: {tag:"list", hd, tl} for cons, VNIL for nil
const VNIL = Object.freeze({ tag: "list" });
function vlist(arr) {
  let result = VNIL;
  for (let i = arr.length - 1; i >= 0; i--) result = { tag: "list", hd: arr[i], tl: result };
  return result;
}
const vrecord = (fields) => ({ tag: "record", v: fields }); // fields: [[name, val], ...]
const vvariant = (tagN, name, payload) => ({ tag: "variant", tagN, name, payload });
const vclosure = (proto, upvalues) => ({ tag: "closure", proto, upvalues });
const vpartial = (closure, args) => ({ tag: "partial", closure, args });
const vexternal = (name, arity, fn, args) => ({ tag: "external", name, arity, fn, args: args || [] });
const vcontinuation = (fiber, returnHandler, opHandlers, bodyFiber, intermediateHandlers) =>
  ({ tag: "continuation", fiber, returnHandler, opHandlers,
     bodyFiber: bodyFiber || fiber, intermediateHandlers: intermediateHandlers || [],
     used: false });
const vref = (v) => ({ tag: "ref", v: [v] }); // use array for mutability
const varray = (elems) => ({ tag: "array", v: elems });
const vproto = (p) => ({ tag: "proto", v: p });

// --- Value accessors ---
function asInt(v) { if (v.tag === "int") return v.v; error(`expected int, got ${ppValue(v)}`); }
function asFloat(v) { if (v.tag === "float") return v.v; error(`expected float, got ${ppValue(v)}`); }
function asBool(v) { if (v.tag === "bool") return v.v; error(`expected bool, got ${ppValue(v)}`); }
function asString(v) { if (v.tag === "string") return v.v; error(`expected string, got ${ppValue(v)}`); }
function asClosure(v) { if (v.tag === "closure") return v; error(`expected function, got ${ppValue(v)}`); }
function asTuple(v) { if (v.tag === "tuple") return v.v; error(`expected tuple, got ${ppValue(v)}`); }
function asRecord(v) { if (v.tag === "record") return v.v; error(`expected record, got ${ppValue(v)}`); }
function asList(v) { if (v.tag === "list") return v; error(`expected list, got ${ppValue(v)}`); }
function listToArray(v) {
  const result = [];
  let cur = asList(v);
  while ("hd" in cur) { result.push(cur.hd); cur = cur.tl; }
  return result;
}
function asVariant(v) { if (v.tag === "variant") return v; error(`expected variant, got ${ppValue(v)}`); }
function asContinuation(v) { if (v.tag === "continuation") return v; error(`expected continuation, got ${ppValue(v)}`); }
function asByte(v) { if (v.tag === "byte") return v.v; error(`expected byte, got ${ppValue(v)}`); }
function asRune(v) { if (v.tag === "rune") return v.v; error(`expected rune, got ${ppValue(v)}`); }
function asArray(v) { if (v.tag === "array") return v.v; error(`expected array, got ${ppValue(v)}`); }

// --- Structural equality (matches vm.ml values_equal) ---
function valuesEqual(a, b) {
  if (a === b) return true;
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case "int": case "float": case "bool": case "byte": case "rune":
      return a.v === b.v;
    case "string":
      return a.v === b.v;
    case "unit":
      return true;
    case "tuple":
      if (a.v.length !== b.v.length) return false;
      for (let i = 0; i < a.v.length; i++) if (!valuesEqual(a.v[i], b.v[i])) return false;
      return true;
    case "list": {
      let ca = a, cb = b;
      while ("hd" in ca && "hd" in cb) {
        if (!valuesEqual(ca.hd, cb.hd)) return false;
        ca = ca.tl; cb = cb.tl;
      }
      return !("hd" in ca) && !("hd" in cb);
    }
    case "variant":
      if (a.tagN !== b.tagN) return false;
      if (a.payload === null && b.payload === null) return true;
      if (a.payload !== null && b.payload !== null) return valuesEqual(a.payload, b.payload);
      return false;
    case "record":
      if (a.v.length !== b.v.length) return false;
      for (let i = 0; i < a.v.length; i++) {
        if (a.v[i][0] !== b.v[i][0] || !valuesEqual(a.v[i][1], b.v[i][1])) return false;
      }
      return true;
    case "array":
      if (a.v.length !== b.v.length) return false;
      for (let i = 0; i < a.v.length; i++) if (!valuesEqual(a.v[i], b.v[i])) return false;
      return true;
    default:
      return false;
  }
}

// --- Structural comparison (matches vm.ml values_compare) ---
function valuesCompare(a, b) {
  if (a === b) return 0;
  if (a.tag !== b.tag) return 0;
  switch (a.tag) {
    case "int": case "byte": case "rune":
      return a.v < b.v ? -1 : a.v > b.v ? 1 : 0;
    case "float":
      return a.v < b.v ? -1 : a.v > b.v ? 1 : 0;
    case "string":
      return a.v < b.v ? -1 : a.v > b.v ? 1 : 0;
    case "bool":
      return (a.v === b.v) ? 0 : (a.v ? 1 : -1);
    case "unit":
      return 0;
    case "tuple":
      for (let i = 0; i < Math.min(a.v.length, b.v.length); i++) {
        const c = valuesCompare(a.v[i], b.v[i]);
        if (c !== 0) return c;
      }
      return a.v.length - b.v.length;
    case "list": {
      let ca = a, cb = b;
      while ("hd" in ca && "hd" in cb) {
        const c = valuesCompare(ca.hd, cb.hd);
        if (c !== 0) return c;
        ca = ca.tl; cb = cb.tl;
      }
      if (!("hd" in ca) && !("hd" in cb)) return 0;
      return ("hd" in ca) ? 1 : -1;
    }
    case "variant":
      if (a.tagN !== b.tagN) return a.tagN < b.tagN ? -1 : 1;
      if (a.payload === null && b.payload === null) return 0;
      if (a.payload === null) return -1;
      if (b.payload === null) return 1;
      return valuesCompare(a.payload, b.payload);
    default:
      return 0;
  }
}

// --- Structural hash (matches vm.ml value_hash) ---
function valueHash(v) {
  const FNV_OFFSET = 0x811c9dc5;
  const FNV_PRIME = 0x01000193;
  function mix(h, x) { return Math.imul(h ^ x, FNV_PRIME) | 0; }
  function mixInt(h, n) {
    h = mix(h, n & 0xff);
    h = mix(h, (n >>> 8) & 0xff);
    h = mix(h, (n >>> 16) & 0xff);
    h = mix(h, (n >>> 24) & 0xff);
    return h;
  }
  function hash(h, v) {
    switch (v.tag) {
      case "int": return mixInt(h, v.v);
      case "float": {
        // Hash the float bits via DataView
        const buf = new ArrayBuffer(8);
        new Float64Array(buf)[0] = v.v;
        const dv = new DataView(buf);
        h = mixInt(h, dv.getInt32(0, true));
        return mixInt(h, dv.getInt32(4, true));
      }
      case "bool": return mix(h, v.v ? 1 : 0);
      case "string":
        for (let i = 0; i < v.v.length; i++) h = mix(h, v.v.charCodeAt(i));
        return h;
      case "byte": return mixInt(h, v.v);
      case "rune": return mixInt(h, v.v);
      case "unit": return mix(h, 0);
      case "tuple":
        h = mix(h, 1);
        for (let i = 0; i < v.v.length; i++) h = hash(h, v.v[i]);
        return h;
      case "list": {
        h = mix(h, 2);
        let c = v;
        while ("hd" in c) { h = hash(h, c.hd); c = c.tl; }
        return h;
      }
      case "variant":
        h = mixInt(h, v.tagN);
        if (v.payload !== null) h = hash(h, v.payload);
        return h;
      case "record":
        h = mix(h, 3);
        for (let i = 0; i < v.v.length; i++) h = hash(h, v.v[i][1]);
        return h;
      case "array":
        h = mix(h, 4);
        for (let i = 0; i < v.v.length; i++) h = hash(h, v.v[i]);
        return h;
      default:
        return h;
    }
  }
  return hash(FNV_OFFSET, v);
}

// --- Pretty-print (matches bytecode.ml pp_value) ---
function runeToUtf8(cp) {
  if (cp < 0x80) return String.fromCharCode(cp);
  if (cp < 0x800) return String.fromCharCode(0xC0 | (cp >> 6), 0x80 | (cp & 0x3F));
  if (cp < 0x10000)
    return String.fromCharCode(0xE0 | (cp >> 12), 0x80 | ((cp >> 6) & 0x3F), 0x80 | (cp & 0x3F));
  return String.fromCodePoint(cp);
}

// OCaml/C "%g" with precision 6 — THE float-to-string spec for every backend
// (docs/semantics.md). %e style if the decimal exponent (after rounding to 6
// significant digits) is < -4 or >= 6, else %f style; trailing zeros stripped;
// two-digit exponent. Used bare by string_of_float / show; ppValue (display)
// appends "." when the result could otherwise be read as an int.
function formatFloat(f) {
  if (Number.isNaN(f)) return "nan";
  if (f === Infinity) return "inf";
  if (f === -Infinity) return "-inf";
  if (f === 0) return Object.is(f, -0) ? "-0" : "0";
  const es = f.toExponential(5);
  const X = parseInt(es.slice(es.indexOf("e") + 1), 10);
  if (X < -4 || X >= 6) {
    let mant = es.slice(0, es.indexOf("e"));
    if (mant.includes(".")) mant = mant.replace(/0+$/, "").replace(/\.$/, "");
    let digits = String(Math.abs(X));
    if (digits.length < 2) digits = "0" + digits;
    return mant + "e" + (X < 0 ? "-" : "+") + digits;
  }
  let s = f.toFixed(Math.max(0, 5 - X));
  if (s.includes(".")) s = s.replace(/0+$/, "").replace(/\.$/, "");
  return s;
}

function ppValue(v) {
  switch (v.tag) {
    case "int": return String(v.v);
    case "float": {
      // Display: %g plus "." unless unambiguous (decimal point, exponent,
      // inf/nan already present). Same rule as Bytecode.pp_value.
      const s = formatFloat(v.v);
      return /[.eni]/.test(s) ? s : s + ".";
    }
    case "bool": return v.v ? "true" : "false";
    case "string": return v.v;
    case "byte": return "#" + v.v.toString(16).padStart(2, '0');
    case "rune": return "'" + runeToUtf8(v.v) + "'";
    case "unit": return "()";
    case "tuple": return "(" + v.v.map(ppValue).join(", ") + ")";
    case "list": {
      const elems = [];
      let cur = v;
      while ("hd" in cur) { elems.push(ppValue(cur.hd)); cur = cur.tl; }
      return "[" + elems.join("; ") + "]";
    }
    case "record":
      return "{ " + v.v.map(([n, val]) => n + " = " + ppValue(val)).join("; ") + " }";
    case "variant":
      if (v.payload === null) return v.name;
      const pv = ppValue(v.payload);
      if (v.payload.tag === "tuple" || (v.payload.tag === "variant" && v.payload.payload !== null))
        return v.name + " (" + pv + ")";
      return v.name + " " + pv;
    case "closure": return "<fun>";
    case "partial": return "<fun>";
    case "external":
      if (v.args.length === 0) return `<external:${v.name}>`;
      return "<fun>";
    case "proto": return `<proto:${v.v.name}>`;
    case "continuation": return "<continuation>";
    case "ref": return `ref(${ppValue(v.v[0])})`;
    case "array": return "#[" + v.v.map(ppValue).join("; ") + "]";
    default: return "<unknown>";
  }
}

// --- Fiber ---
// Fiber stacks grow on demand (mirrors the OCaml VM's growable stacks). Every
// handle/try allocates a fresh fiber, so starting small instead of eagerly
// allocating a fixed 64K-slot array (~the old cost) makes handler installation
// dramatically cheaper; deep recursion grows by doubling. A generous cap turns
// runaway non-tail recursion into a clean "stack overflow" instead of an OOM.
const FIBER_STACK_INIT = 1024;
const FIBER_STACK_MAX = 16 * 1024 * 1024;

function makeFiber() {
  return { stack: new Array(FIBER_STACK_INIT).fill(VUNIT), sp: 0, frames: [], extraArgs: [] };
}

// Ensure fiber stack holds at least `needed` slots, growing (doubling) and
// filling new slots with VUNIT. JS arrays auto-grow on indexed write, but
// frame setup uses Array.fill(start,end) which does NOT extend a short array,
// so capacity must be ensured before filling a locals region.
function ensureCap(f, needed) {
  const cap = f.stack.length;
  if (needed > cap) {
    if (needed > FIBER_STACK_MAX) error("stack overflow");
    let newCap = cap;
    while (newCap < needed) newCap = Math.min(newCap * 2, FIBER_STACK_MAX);
    f.stack.length = newCap;
    f.stack.fill(VUNIT, cap, newCap);
  }
}

function copyFiber(f) {
  const newStack = f.stack.slice();
  const newFrames = f.frames.map(fr => ({
    closure: fr.closure,
    ip: fr.ip,
    baseSp: fr.baseSp,
  }));
  return { stack: newStack, sp: f.sp, frames: newFrames, extraArgs: f.extraArgs.map(a => a.slice()) };
}

// --- Top-level helper functions ---
// A handler entry is one of:
//   {kind:"full", returnHandler, ops, bodyFiber, parentFiber} — fiber-based, HANDLE
//   {kind:"try", fiber, frameDepth, stackDepth, control, ret, catch} — no-fiber
//     lowering of non-resumptive handlers, TRY_BEGIN
//   {kind:"provide", ops, fiber, frameDepth, stackDepth} — no-fiber lowering of
//     tail-resumptive handlers, PROVIDE (body inline; PERFORM calls the arm on the
//     current fiber and continues, reinstalling the marker when the arm returns).
function heOpNames(he) {
  return he.kind === "try"
    ? he.catch.map(([name]) => name)
    : he.ops.map(([name]) => name);
}

function findHandlerForFiber(vm, f) {
  // Only full handlers own a body fiber; try/provide markers complete inline.
  for (const he of vm.handlerStack) {
    if (he.kind === "full" && he.bodyFiber === f) return he;
  }
  return null;
}

function removeHandler(vm, he) {
  vm.handlerStack = vm.handlerStack.filter(h => h !== he);
}

// Drop inline try/provide markers opened at or above stackDepth on f (a non-local
// jump leaving an inline body without running its TRY_END/PROVIDE_END).
function dropTryMarkersAbove(vm, f, stackDepth) {
  vm.handlerStack = vm.handlerStack.filter(h =>
    !((h.kind === "try" || h.kind === "provide") &&
      h.fiber === f && h.stackDepth >= stackDepth)
  );
}

// Drop pending provide-resume entries on f whose recorded frame depth is at or above
// frameDepth (frames unwound non-locally past a mid-flight provide arm).
function dropProvideResumesAbove(vm, f, frameDepth) {
  if (vm.provideResumes.length > 0) {
    vm.provideResumes = vm.provideResumes.filter(
      ([rf, rd]) => !(rf === f && rd >= frameDepth)
    );
  }
}

// When a provide arm just returned (the current fiber's frame depth dropped back to
// where the arm was entered), reinstall the HProvide marker + intermediates that were
// lifted off for the arm, so the resumed body sees the handler again.
function reinstallProvideOnReturn(vm, fiber) {
  if (vm.provideResumes.length > 0) {
    const top = vm.provideResumes[vm.provideResumes.length - 1];
    if (top[0] === fiber && top[1] === fiber.frames.length) {
      vm.handlerStack = vm.handlerStack.concat(top[2]);
      vm.provideResumes.pop();
    }
  }
}

function internalCall(fiber, cls, arg) {
  const base = fiber.sp;
  const numLocals = cls.proto.num_locals;
  ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
  fiber.stack[base] = arg;
  fiber.sp = base + numLocals;
  fiber.frames.push({ closure: cls, ip: 0, baseSp: base });
}

// --- Opcode profiling (toggle with vm.profile = true) ---
const OP_COUNTS = new Uint32Array(85);

function resetProfile() { OP_COUNTS.fill(0); }

function dumpProfile(opcodeNames) {
  const entries = [];
  for (let i = 0; i < OP_COUNTS.length; i++) {
    if (OP_COUNTS[i] > 0) entries.push([i, OP_COUNTS[i]]);
  }
  entries.sort((a, b) => b[1] - a[1]);
  const total = entries.reduce((s, e) => s + e[1], 0);
  console.log(`\n--- opcode profile (${total.toLocaleString()} total) ---`);
  for (const [op, count] of entries.slice(0, 20)) {
    const name = (opcodeNames && opcodeNames[op]) || String(op);
    const pct = (count / total * 100).toFixed(1);
    console.log(`  ${name.padEnd(22)} ${count.toLocaleString().padStart(12)}  (${pct}%)`);
  }
}

// --- Multi-arity call helpers ---
function callWithArgs(vm, fiber, fnVal, args) {
  // Returns true if a frame was entered, false if result was pushed to stack
  if (fnVal.tag === "closure") {
    const arity = fnVal.proto.arity;
    const n = args.length;
    if (n === arity) {
      const base = fiber.sp;
      const numLocals = fnVal.proto.num_locals;
      ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
      for (let i = 0; i < n; i++) fiber.stack[base + i] = args[i];
      fiber.sp = base + numLocals;
      fiber.frames.push({ closure: fnVal, ip: 0, baseSp: base });
      return true;
    } else if (n < arity) {
      fiber.stack[fiber.sp++] = vpartial(fnVal, args);
      return false;
    } else {
      const useArgs = args.slice(0, arity);
      const extra = args.slice(arity);
      fiber.extraArgs.push(extra);
      const base = fiber.sp;
      const numLocals = fnVal.proto.num_locals;
      ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
      for (let i = 0; i < arity; i++) fiber.stack[base + i] = useArgs[i];
      fiber.sp = base + numLocals;
      fiber.frames.push({ closure: fnVal, ip: 0, baseSp: base });
      return true;
    }
  } else if (fnVal.tag === "partial") {
    return callWithArgs(vm, fiber, fnVal.closure, fnVal.args.concat(args));
  } else if (fnVal.tag === "external") {
    const all = fnVal.args.concat(args);
    if (all.length >= fnVal.arity) {
      const useArgs = all.slice(0, fnVal.arity);
      const remaining = all.slice(fnVal.arity);
      const result = fnVal.fn(useArgs);
      if (remaining.length === 0) { fiber.stack[fiber.sp++] = result; return false; }
      return callWithArgs(vm, fiber, result, remaining);
    } else {
      fiber.stack[fiber.sp++] = vexternal(fnVal.name, fnVal.arity, fnVal.fn, all);
      return false;
    }
  } else {
    error(`CALL_N: expected function, got ${ppValue(fnVal)}`);
  }
}

function processExtraArgs(vm, fiber, result) {
  // Returns [result, enteredFrame]
  while (fiber.extraArgs.length > 0) {
    const pending = fiber.extraArgs[fiber.extraArgs.length - 1];
    fiber.extraArgs.pop();
    if (result.tag === "closure") {
      const arity = result.proto.arity;
      const n = pending.length;
      if (n >= arity) {
        const useArgs = pending.slice(0, arity);
        const remaining = pending.slice(arity);
        if (remaining.length > 0) fiber.extraArgs.push(remaining);
        const base = fiber.sp;
        const numLocals = result.proto.num_locals;
        ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
        for (let i = 0; i < arity; i++) fiber.stack[base + i] = useArgs[i];
        fiber.sp = base + numLocals;
        fiber.frames.push({ closure: result, ip: 0, baseSp: base });
        return [result, true];
      } else {
        // Not enough args in this batch
        result = vpartial(result, pending);
        // continue to next batch
      }
    } else if (result.tag === "partial") {
      fiber.extraArgs.push(result.args.concat(pending));
      result = result.closure;
      // re-process
    } else if (result.tag === "external") {
      const all = result.args.concat(pending);
      if (all.length >= result.arity) {
        const useArgs = all.slice(0, result.arity);
        const remaining = all.slice(result.arity);
        if (remaining.length > 0) fiber.extraArgs.push(remaining);
        result = result.fn(useArgs);
        // continue - result might be callable
      } else {
        result = vexternal(result.name, result.arity, result.fn, all);
        // continue to next batch
      }
    } else {
      error(`expected function in over-application, got ${ppValue(result)}`);
    }
  }
  return [result, false];
}

// --- VM dispatch loop ---
function run(vm) {
  let fiber = vm.currentFiber;
  // const profiling = vm.profile;

  while (true) {
    const f = fiber.frames[fiber.frames.length - 1];
    const op = f.closure.proto.code[f.ip++];
    // if (profiling) OP_COUNTS[op[0]]++;

    switch (op[0]) {
      case 0: // CONST
        fiber.stack[fiber.sp++] = f.closure.proto.constants[op[1]];
        break;
      case 1: // POP
        fiber.sp--;
        break;
      case 2: // DUP
        fiber.stack[fiber.sp] = fiber.stack[fiber.sp - 1];
        fiber.sp++;
        break;
      case 3: // GET_LOCAL
        fiber.stack[fiber.sp++] = fiber.stack[f.baseSp + op[1]];
        break;
      case 4: // SET_LOCAL
        fiber.stack[f.baseSp + op[1]] = fiber.stack[--fiber.sp];
        break;
      case 5: // GET_UPVALUE
        fiber.stack[fiber.sp++] = f.closure.upvalues[op[1]];
        break;
      case 6: // MAKE_REF
        fiber.stack[fiber.sp - 1] = vref(fiber.stack[fiber.sp - 1]);
        break;
      case 7: { // DEREF
        const r = fiber.stack[fiber.sp - 1];
        if (r.tag !== "ref") error("DEREF on non-ref value");
        fiber.stack[fiber.sp - 1] = r.v[0];
        break;
      }
      case 8: { // SET_REF
        const r = fiber.stack[--fiber.sp];
        const v = fiber.stack[--fiber.sp];
        if (r.tag !== "ref") error("SET_REF on non-ref value");
        r.v[0] = v;
        break;
      }
      case 9: { // GET_GLOBAL
        const idx = op[1];
        const v = vm.globals.get(idx);
        if (v === undefined) {
          const name = idx < vm.globalNames.length ? vm.globalNames[idx] : "?";
          error(`undefined global: ${name}`);
        }
        fiber.stack[fiber.sp++] = v;
        break;
      }
      case 10: // DEF_GLOBAL
        vm.globals.set(op[1], fiber.stack[--fiber.sp]);
        break;
      case 11: { // ADD
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) + asInt(b));
        break;
      }
      case 12: { // SUB
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) - asInt(b));
        break;
      }
      case 13: { // MUL
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) * asInt(b));
        break;
      }
      case 14: { // DIV
        const b = fiber.stack[--fiber.sp];
        const bv = asInt(b);
        if (bv === 0) error("division by zero");
        fiber.stack[fiber.sp - 1] = vint(Math.trunc(asInt(fiber.stack[fiber.sp - 1]) / bv));
        break;
      }
      case 15: { // MOD
        const b = fiber.stack[--fiber.sp];
        const bv = asInt(b);
        if (bv === 0) error("modulo by zero");
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) % bv);
        break;
      }
      case 16: // NEG
        fiber.stack[fiber.sp - 1] = vint(-asInt(fiber.stack[fiber.sp - 1]));
        break;
      case 17: { // FADD
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vfloat(asFloat(fiber.stack[fiber.sp - 1]) + asFloat(b));
        break;
      }
      case 18: { // FSUB
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vfloat(asFloat(fiber.stack[fiber.sp - 1]) - asFloat(b));
        break;
      }
      case 19: { // FMUL
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vfloat(asFloat(fiber.stack[fiber.sp - 1]) * asFloat(b));
        break;
      }
      case 20: { // FDIV
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vfloat(asFloat(fiber.stack[fiber.sp - 1]) / asFloat(b));
        break;
      }
      case 21: // FNEG
        fiber.stack[fiber.sp - 1] = vfloat(-asFloat(fiber.stack[fiber.sp - 1]));
        break;
      case 22: { // EQ
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vbool(valuesEqual(fiber.stack[fiber.sp - 1], b));
        break;
      }
      case 23: { // NEQ
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vbool(!valuesEqual(fiber.stack[fiber.sp - 1], b));
        break;
      }
      case 24: { // LT
        const b = fiber.stack[--fiber.sp];
        const a = fiber.stack[fiber.sp - 1];
        let result;
        if (a.tag === "float" && b.tag === "float") result = a.v < b.v;
        else if (a.tag === "string" && b.tag === "string") result = a.v < b.v;
        else if (a.tag === "byte" && b.tag === "byte") result = a.v < b.v;
        else if (a.tag === "rune" && b.tag === "rune") result = a.v < b.v;
        else result = asInt(a) < asInt(b);
        fiber.stack[fiber.sp - 1] = vbool(result);
        break;
      }
      case 25: { // GT
        const b = fiber.stack[--fiber.sp];
        const a = fiber.stack[fiber.sp - 1];
        let result;
        if (a.tag === "float" && b.tag === "float") result = a.v > b.v;
        else if (a.tag === "string" && b.tag === "string") result = a.v > b.v;
        else if (a.tag === "byte" && b.tag === "byte") result = a.v > b.v;
        else if (a.tag === "rune" && b.tag === "rune") result = a.v > b.v;
        else result = asInt(a) > asInt(b);
        fiber.stack[fiber.sp - 1] = vbool(result);
        break;
      }
      case 26: { // LE
        const b = fiber.stack[--fiber.sp];
        const a = fiber.stack[fiber.sp - 1];
        let result;
        if (a.tag === "float" && b.tag === "float") result = a.v <= b.v;
        else if (a.tag === "string" && b.tag === "string") result = a.v <= b.v;
        else if (a.tag === "byte" && b.tag === "byte") result = a.v <= b.v;
        else if (a.tag === "rune" && b.tag === "rune") result = a.v <= b.v;
        else result = asInt(a) <= asInt(b);
        fiber.stack[fiber.sp - 1] = vbool(result);
        break;
      }
      case 27: { // GE
        const b = fiber.stack[--fiber.sp];
        const a = fiber.stack[fiber.sp - 1];
        let result;
        if (a.tag === "float" && b.tag === "float") result = a.v >= b.v;
        else if (a.tag === "string" && b.tag === "string") result = a.v >= b.v;
        else if (a.tag === "byte" && b.tag === "byte") result = a.v >= b.v;
        else if (a.tag === "rune" && b.tag === "rune") result = a.v >= b.v;
        else result = asInt(a) >= asInt(b);
        fiber.stack[fiber.sp - 1] = vbool(result);
        break;
      }
      case 28: // NOT
        fiber.stack[fiber.sp - 1] = vbool(!asBool(fiber.stack[fiber.sp - 1]));
        break;
      case 29: { // BAND
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) & asInt(b));
        break;
      }
      case 30: { // BOR
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) | asInt(b));
        break;
      }
      case 31: { // BXOR
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) ^ asInt(b));
        break;
      }
      case 32: // BNOT
        fiber.stack[fiber.sp - 1] = vint(~asInt(fiber.stack[fiber.sp - 1]));
        break;
      case 33: { // BSHL
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) << asInt(b));
        break;
      }
      case 34: { // BSHR
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) >>> asInt(b));
        break;
      }
      case 35: // JUMP
        f.ip = op[1];
        break;
      case 36: { // JUMP_IF_FALSE
        const v = fiber.stack[--fiber.sp];
        if (!asBool(v)) f.ip = op[1];
        break;
      }
      case 37: { // JUMP_IF_TRUE
        const v = fiber.stack[--fiber.sp];
        if (asBool(v)) f.ip = op[1];
        break;
      }
      case 38: { // CLOSURE
        const protoIdx = op[1];
        const captures = op[2];
        const fnProto = getProto(f.closure.proto, protoIdx);
        const upvalues = captures.map(cap => resolveCapture(fiber, f, cap));
        fiber.stack[fiber.sp++] = vclosure(fnProto, upvalues);
        break;
      }
      case 39: { // CLOSURE_REC
        const protoIdx = op[1];
        const captures = op[2];
        const selfIdx = op[3];
        const fnProto = getProto(f.closure.proto, protoIdx);
        const upvalues = captures.map(cap => resolveCapture(fiber, f, cap));
        const cls = vclosure(fnProto, upvalues);
        cls.upvalues[selfIdx] = cls; // circular reference for recursion
        fiber.stack[fiber.sp++] = cls;
        break;
      }
      case 40: { // CALL
        const arg = fiber.stack[--fiber.sp];
        const fnVal = fiber.stack[--fiber.sp];
        if (fnVal.tag === "closure") {
          if (fnVal.proto.arity === 1) {
            const base = fiber.sp;
            const numLocals = fnVal.proto.num_locals;
            ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
            fiber.stack[base] = arg;
            fiber.sp = base + numLocals;
            fiber.frames.push({ closure: fnVal, ip: 0, baseSp: base });
          } else {
            fiber.stack[fiber.sp++] = vpartial(fnVal, [arg]);
          }
        } else if (fnVal.tag === "partial") {
          const newArgs = fnVal.args.concat([arg]);
          if (newArgs.length === fnVal.closure.proto.arity) {
            const cls = fnVal.closure;
            const base = fiber.sp;
            const numLocals = cls.proto.num_locals;
            ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
            for (let i = 0; i < newArgs.length; i++) fiber.stack[base + i] = newArgs[i];
            fiber.sp = base + numLocals;
            fiber.frames.push({ closure: cls, ip: 0, baseSp: base });
          } else {
            fiber.stack[fiber.sp++] = vpartial(fnVal.closure, newArgs);
          }
        } else if (fnVal.tag === "external") {
          const newArgs = fnVal.args.concat([arg]);
          if (newArgs.length === fnVal.arity) {
            fiber.stack[fiber.sp++] = fnVal.fn(newArgs);
          } else {
            fiber.stack[fiber.sp++] = vexternal(fnVal.name, fnVal.arity, fnVal.fn, newArgs);
          }
        } else {
          error(`expected function, got ${ppValue(fnVal)}`);
        }
        break;
      }
      case 41: { // TAIL_CALL
        const arg = fiber.stack[--fiber.sp];
        const fnVal = fiber.stack[--fiber.sp];
        let tailResult = null;
        let tailEntered = false;
        if (fnVal.tag === "closure") {
          if (fnVal.proto.arity === 1) {
            const currentBaseSp = f.baseSp;
            const numLocals = fnVal.proto.num_locals;
            ensureCap(fiber, currentBaseSp + numLocals); fiber.stack.fill(VUNIT, currentBaseSp, currentBaseSp + numLocals);
            fiber.stack[currentBaseSp] = arg;
            fiber.sp = currentBaseSp + numLocals;
            fiber.frames[fiber.frames.length - 1] = { closure: fnVal, ip: 0, baseSp: currentBaseSp };
            tailEntered = true;
          } else {
            tailResult = vpartial(fnVal, [arg]);
          }
        } else if (fnVal.tag === "partial") {
          const newArgs = fnVal.args.concat([arg]);
          if (newArgs.length === fnVal.closure.proto.arity) {
            const cls = fnVal.closure;
            const currentBaseSp = f.baseSp;
            const numLocals = cls.proto.num_locals;
            ensureCap(fiber, currentBaseSp + numLocals); fiber.stack.fill(VUNIT, currentBaseSp, currentBaseSp + numLocals);
            for (let i = 0; i < newArgs.length; i++) fiber.stack[currentBaseSp + i] = newArgs[i];
            fiber.sp = currentBaseSp + numLocals;
            fiber.frames[fiber.frames.length - 1] = { closure: cls, ip: 0, baseSp: currentBaseSp };
            tailEntered = true;
          } else {
            tailResult = vpartial(fnVal.closure, newArgs);
          }
        } else if (fnVal.tag === "external") {
          const newArgs = fnVal.args.concat([arg]);
          if (newArgs.length === fnVal.arity) {
            tailResult = fnVal.fn(newArgs);
          } else {
            tailResult = vexternal(fnVal.name, fnVal.arity, fnVal.fn, newArgs);
          }
        } else {
          error(`expected function, got ${ppValue(fnVal)}`);
        }
        if (!tailEntered) {
          fiber.sp = f.baseSp;
          fiber.frames.pop();
          reinstallProvideOnReturn(vm, fiber);
          let result2 = tailResult;
          if (fiber.extraArgs.length > 0) {
            const [res, entered2] = processExtraArgs(vm, fiber, tailResult);
            result2 = res;
            if (entered2) break;
          }
          if (fiber.frames.length === 0) {
            const he = findHandlerForFiber(vm, fiber);
            if (he) {
              removeHandler(vm, he);
              fiber = he.parentFiber;
              vm.currentFiber = fiber;
              internalCall(fiber, asClosure(he.returnHandler), result2);
            } else {
              return result2;
            }
          } else {
            fiber.stack[fiber.sp++] = result2;
          }
        }
        break;
      }
      case 42: { // RETURN
        let result = fiber.stack[--fiber.sp];
        fiber.sp = fiber.frames[fiber.frames.length - 1].baseSp;
        fiber.frames.pop();
        reinstallProvideOnReturn(vm, fiber);
        if (fiber.extraArgs.length > 0) {
          const [res, entered] = processExtraArgs(vm, fiber, result);
          result = res;
          if (entered) break;
        }
        if (fiber.frames.length === 0) {
          const he = findHandlerForFiber(vm, fiber);
          if (he) {
            removeHandler(vm, he);
            fiber = he.parentFiber;
            vm.currentFiber = fiber;
            internalCall(fiber, asClosure(he.returnHandler), result);
          } else {
            return result;
          }
        } else {
          fiber.stack[fiber.sp++] = result;
        }
        break;
      }
      case 80: { // CALL_N
        const n = op[1];
        const args = new Array(n);
        for (let i = n - 1; i >= 0; i--) args[i] = fiber.stack[--fiber.sp];
        const fnVal83 = fiber.stack[--fiber.sp];
        callWithArgs(vm, fiber, fnVal83, args);
        break;
      }
      case 81: { // TAIL_CALL_N
        const n = op[1];
        const args = new Array(n);
        for (let i = n - 1; i >= 0; i--) args[i] = fiber.stack[--fiber.sp];
        const fnVal84 = fiber.stack[--fiber.sp];
        // Try to resolve the call
        let tcnEntered = false;
        let tcnResult = null;
        const resolve = (fn, remaining) => {
          if (fn.tag === "closure") {
            const arity = fn.proto.arity;
            const nArgs = remaining.length;
            if (nArgs === arity) {
              const baseSp = f.baseSp;
              const numLocals = fn.proto.num_locals;
              ensureCap(fiber, baseSp + numLocals); fiber.stack.fill(VUNIT, baseSp, baseSp + numLocals);
              for (let i = 0; i < nArgs; i++) fiber.stack[baseSp + i] = remaining[i];
              fiber.sp = baseSp + numLocals;
              fiber.frames[fiber.frames.length - 1] = { closure: fn, ip: 0, baseSp };
              tcnEntered = true;
            } else if (nArgs < arity) {
              tcnResult = vpartial(fn, remaining);
            } else {
              const useArgs = remaining.slice(0, arity);
              const extra = remaining.slice(arity);
              fiber.extraArgs.push(extra);
              const baseSp = f.baseSp;
              const numLocals = fn.proto.num_locals;
              ensureCap(fiber, baseSp + numLocals); fiber.stack.fill(VUNIT, baseSp, baseSp + numLocals);
              for (let i = 0; i < arity; i++) fiber.stack[baseSp + i] = useArgs[i];
              fiber.sp = baseSp + numLocals;
              fiber.frames[fiber.frames.length - 1] = { closure: fn, ip: 0, baseSp };
              tcnEntered = true;
            }
          } else if (fn.tag === "partial") {
            resolve(fn.closure, fn.args.concat(remaining));
          } else if (fn.tag === "external") {
            const all = fn.args.concat(remaining);
            if (all.length >= fn.arity) {
              const useArgs = all.slice(0, fn.arity);
              const rest = all.slice(fn.arity);
              const r = fn.fn(useArgs);
              if (rest.length === 0) tcnResult = r;
              else resolve(r, rest);
            } else {
              tcnResult = vexternal(fn.name, fn.arity, fn.fn, all);
            }
          } else {
            error(`TAIL_CALL_N: expected function, got ${ppValue(fn)}`);
          }
        };
        resolve(fnVal84, args);
        if (!tcnEntered) {
          fiber.sp = f.baseSp;
          fiber.frames.pop();
          reinstallProvideOnReturn(vm, fiber);
          let result2 = tcnResult;
          if (fiber.extraArgs.length > 0) {
            const [res, entered2] = processExtraArgs(vm, fiber, tcnResult);
            result2 = res;
            if (entered2) break;
          }
          if (fiber.frames.length === 0) {
            const he = findHandlerForFiber(vm, fiber);
            if (he) {
              removeHandler(vm, he);
              fiber = he.parentFiber;
              vm.currentFiber = fiber;
              internalCall(fiber, asClosure(he.returnHandler), result2);
            } else {
              return result2;
            }
          } else {
            fiber.stack[fiber.sp++] = result2;
          }
        }
        break;
      }
      case 46: { // MAKE_TUPLE
        const n = op[1];
        const values = new Array(n);
        for (let i = n - 1; i >= 0; i--) values[i] = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp++] = vtuple(values);
        break;
      }
      case 47: { // TUPLE_GET
        const idx = op[1];
        const tup = asTuple(fiber.stack[fiber.sp - 1]);
        if (idx >= tup.length) error("tuple index out of bounds");
        fiber.stack[fiber.sp - 1] = tup[idx];
        break;
      }
      case 48: { // MAKE_RECORD
        const fieldNames = op[1];
        const n = fieldNames.length;
        const values = new Array(n);
        for (let i = n - 1; i >= 0; i--) values[i] = fiber.stack[--fiber.sp];
        const fields = fieldNames.map((name, i) => [name, values[i]]);
        fiber.stack[fiber.sp++] = vrecord(fields);
        break;
      }
      case 49: { // FIELD
        const name = op[1];
        const rec = asRecord(fiber.stack[fiber.sp - 1]);
        const entry = rec.find(([n]) => n === name);
        if (!entry) error(`record has no field: ${name}`);
        fiber.stack[fiber.sp - 1] = entry[1];
        break;
      }
      case 50: { // SET_FIELD
        const name = op[1];
        const newVal = fiber.stack[--fiber.sp];
        const rec = asRecord(fiber.stack[--fiber.sp]);
        let found = false;
        for (let i = 0; i < rec.length; i++) {
          if (rec[i][0] === name) { rec[i] = [name, newVal]; found = true; break; }
        }
        if (!found) error(`record has no field: ${name}`);
        break;
      }
      case 72: { // RECORD_UPDATE
        const fieldNames = op[1];
        const n = fieldNames.length;
        const newVals = new Array(n);
        for (let i = n - 1; i >= 0; i--) newVals[i] = fiber.stack[--fiber.sp];
        const baseRec = asRecord(fiber.stack[--fiber.sp]);
        const copy = baseRec.map(([n, v]) => [n, v]);
        for (let i = 0; i < n; i++) {
          const name = fieldNames[i];
          const idx = copy.findIndex(([n]) => n === name);
          if (idx < 0) error(`record has no field: ${name}`);
          copy[idx] = [name, newVals[i]];
        }
        fiber.stack[fiber.sp++] = vrecord(copy);
        break;
      }
      case 73: { // RECORD_UPDATE_DYN
        const n = op[1];
        const pairs = new Array(n);
        for (let i = n - 1; i >= 0; i--) {
          const v = fiber.stack[--fiber.sp];
          const idx = asInt(fiber.stack[--fiber.sp]);
          pairs[i] = [idx, v];
        }
        const baseRec = asRecord(fiber.stack[--fiber.sp]);
        const copy = baseRec.map(([n, v]) => [n, v]);
        for (const [idx, v] of pairs) {
          if (idx < 0 || idx >= copy.length)
            error(`RECORD_UPDATE_DYN: index ${idx} out of bounds (record has ${copy.length} fields)`);
          copy[idx] = [copy[idx][0], v];
        }
        fiber.stack[fiber.sp++] = vrecord(copy);
        break;
      }
      case 51: { // MAKE_VARIANT
        const [, tagN, name, hasPayload] = op;
        if (hasPayload) {
          fiber.stack[fiber.sp - 1] = vvariant(tagN, name, fiber.stack[fiber.sp - 1]);
        } else {
          fiber.stack[fiber.sp++] = vvariant(tagN, name, null);
        }
        break;
      }
      case 52: { // CONS
        const tl = fiber.stack[--fiber.sp];
        const hd = fiber.stack[--fiber.sp];
        asList(tl); // validate
        fiber.stack[fiber.sp++] = { tag: "list", hd, tl };
        break;
      }
      case 53: // NIL
        fiber.stack[fiber.sp++] = VNIL;
        break;
      case 54: { // TAG_EQ
        const tagN = op[1];
        fiber.stack[fiber.sp - 1] = vbool(asVariant(fiber.stack[fiber.sp - 1]).tagN === tagN);
        break;
      }
      case 55: { // IS_NIL
        const v = asList(fiber.stack[fiber.sp - 1]);
        fiber.stack[fiber.sp - 1] = vbool(!("hd" in v));
        break;
      }
      case 56: { // IS_CONS
        const v = asList(fiber.stack[fiber.sp - 1]);
        fiber.stack[fiber.sp - 1] = vbool("hd" in v);
        break;
      }
      case 57: { // HEAD
        const v = asList(fiber.stack[fiber.sp - 1]);
        if (!("hd" in v)) error("head of empty list");
        fiber.stack[fiber.sp - 1] = v.hd;
        break;
      }
      case 58: { // TAIL
        const v = asList(fiber.stack[fiber.sp - 1]);
        if (!("hd" in v)) error("tail of empty list");
        fiber.stack[fiber.sp - 1] = v.tl;
        break;
      }
      case 59: { // VARIANT_PAYLOAD
        const v = asVariant(fiber.stack[fiber.sp - 1]);
        if (v.payload === null) error("variant has no payload");
        fiber.stack[fiber.sp - 1] = v.payload;
        break;
      }
      case 60: // MATCH_FAIL
        error(`non-exhaustive match at ${op[1]}`);
        break;
      case 61: { // PERFORM
        const opNameStr = op[1];
        const arg = fiber.stack[--fiber.sp];
        // Find the nearest handler (full or try) covering this op.
        let matchIdx = -1;
        let he = null;
        for (let i = vm.handlerStack.length - 1; i >= 0; i--) {
          const h = vm.handlerStack[i];
          if (heOpNames(h).includes(opNameStr)) { matchIdx = i; he = h; break; }
        }
        if (!he) error(`unhandled effect operation: ${opNameStr}`);
        if (he.kind === "try") {
          // No-fiber, non-resumptive handler: unwind to the TRY_BEGIN marker (on
          // he.fiber, current or an enclosing fiber) and jump to the op's catch.
          // The whole body is discarded, so restore the control/return snapshots.
          const catchIp = he.catch.find(([name]) => name === opNameStr)[1];
          vm.handlerStack = vm.handlerStack.slice(0, matchIdx);
          const tf = he.fiber;
          while (tf.frames.length > he.frameDepth) tf.frames.pop();
          tf.sp = he.stackDepth;
          tf.extraArgs = [];
          vm.controlStack = he.control.slice();
          vm.returnStack = he.ret.slice();
          fiber = tf;
          vm.currentFiber = tf;
          tf.stack[tf.sp++] = arg;
          tf.frames[tf.frames.length - 1].ip = catchIp;
        } else if (he.kind === "provide") {
          // No-fiber, tail-resumptive handler: call the op's arm closure
          // (fun arg -> value) on the CURRENT fiber and continue the body with its
          // result. Tail-resumption resumes exactly once, immediately, so no
          // continuation is reified and the fiber is irrelevant. Lift the matched
          // handler + inner intermediates off the stack while the arm runs; they are
          // reinstalled by reinstallProvideOnReturn when the arm returns.
          const arm = he.ops.find(([name]) => name === opNameStr)[1];
          const removed = vm.handlerStack.slice(matchIdx);
          vm.handlerStack = vm.handlerStack.slice(0, matchIdx);
          vm.provideResumes.push([fiber, fiber.frames.length, removed]);
          internalCall(fiber, asClosure(arm), arg);
        } else {
          const handlerFn = he.ops.find(([name]) => name === opNameStr)[1];
          // Collect intermediate handlers (pushed after matched = more inner)
          const intermediates = vm.handlerStack.slice(matchIdx + 1);
          const cont = vcontinuation(fiber, he.returnHandler, he.ops, he.bodyFiber, intermediates);
          const pair = vtuple([arg, cont]);
          // Remove matched handler and all intermediates
          vm.handlerStack = vm.handlerStack.slice(0, matchIdx);
          fiber = he.parentFiber;
          vm.currentFiber = fiber;
          internalCall(fiber, asClosure(handlerFn), pair);
        }
        break;
      }
      case 62: { // HANDLE
        const nOps = op[1];
        const ops = [];
        for (let i = 0; i < nOps; i++) {
          const handlerClosure = fiber.stack[--fiber.sp];
          const opStr = asString(fiber.stack[--fiber.sp]);
          ops.push([opStr, handlerClosure]);
        }
        const returnHandler = fiber.stack[--fiber.sp];
        const bodyThunk = fiber.stack[--fiber.sp];
        const bodyFiber = makeFiber();
        const he = {
          kind: "full",
          returnHandler,
          ops,
          bodyFiber,
          parentFiber: fiber,
        };
        vm.handlerStack.push(he);
        fiber = bodyFiber;
        vm.currentFiber = fiber;
        internalCall(fiber, asClosure(bodyThunk), VUNIT);
        break;
      }
      case 63: { // RESUME
        const v = fiber.stack[--fiber.sp];
        const cont = asContinuation(fiber.stack[--fiber.sp]);
        if (cont.used) error("continuation already resumed");
        cont.used = true;
        const bodyFiber = cont.fiber;
        ensureCap(bodyFiber, bodyFiber.sp + 1);
        bodyFiber.stack[bodyFiber.sp++] = v;
        // Reinstall caught handler with original body fiber
        const he = {
          kind: "full",
          returnHandler: cont.returnHandler,
          ops: cont.opHandlers,
          bodyFiber: cont.bodyFiber,
          parentFiber: fiber,
        };
        // Reinstall caught handler then intermediates (innermost last = pushed last)
        vm.handlerStack.push(he, ...cont.intermediateHandlers);
        fiber = bodyFiber;
        vm.currentFiber = fiber;
        break;
      }
      case 64: { // ENTER_LOOP
        const breakTarget = op[1];
        vm.controlStack.push({
          breakIp: breakTarget,
          fiber: fiber,
          frameDepth: fiber.frames.length,
          stackDepth: fiber.sp,
        });
        break;
      }
      case 65: { // EXIT_LOOP
        if (vm.controlStack.length === 0) error("EXIT_LOOP: no control entry");
        vm.controlStack.pop();
        break;
      }
      case 66: { // LOOP_BREAK
        const breakValue = fiber.stack[--fiber.sp];
        if (vm.controlStack.length === 0) error("LOOP_BREAK: no control entry");
        const ce = vm.controlStack.pop();
        const cf = ce.fiber;
        while (cf.frames.length > ce.frameDepth) cf.frames.pop();
        // Drop inline try/provide markers opened inside the loop body (break skips
        // TRY_END/PROVIDE_END).
        dropTryMarkersAbove(vm, cf, ce.stackDepth);
        dropProvideResumesAbove(vm, cf, ce.frameDepth);
        cf.sp = ce.stackDepth;
        vm.currentFiber = cf;
        fiber = cf;
        fiber.stack[fiber.sp++] = breakValue;
        fiber.frames[fiber.frames.length - 1].ip = ce.breakIp;
        break;
      }
      case 67: { // LOOP_CONTINUE
        const target = op[1];
        if (vm.controlStack.length === 0) error("LOOP_CONTINUE: no control entry");
        const ce = vm.controlStack[vm.controlStack.length - 1];
        // Drop inline try/provide markers opened in the body (continue skips
        // TRY_END/PROVIDE_END).
        dropTryMarkersAbove(vm, ce.fiber, ce.stackDepth);
        dropProvideResumesAbove(vm, ce.fiber, ce.frameDepth);
        ce.fiber.sp = ce.stackDepth;
        f.ip = target;
        break;
      }
      case 68: { // FOLD_CONTINUE
        const continueValue = fiber.stack[--fiber.sp];
        // Restore sp to frame base (cleans up locals + temps)
        fiber.sp = fiber.frames[fiber.frames.length - 1].baseSp;
        // Pop the fold callback frame and push result
        if (fiber.frames.length <= 1) error("FOLD_CONTINUE: no frame to return from");
        fiber.frames.pop();
        fiber.stack[fiber.sp++] = continueValue;
        break;
      }
      case 83: { // TRY_BEGIN
        vm.handlerStack.push({
          kind: "try",
          fiber: fiber,
          frameDepth: fiber.frames.length,
          stackDepth: fiber.sp,
          control: vm.controlStack.slice(),
          ret: vm.returnStack.slice(),
          catch: op[1],
        });
        break;
      }
      case 84: { // TRY_END
        // Normal completion of a try body: pop the nearest try-marker.
        for (let i = vm.handlerStack.length - 1; i >= 0; i--) {
          if (vm.handlerStack[i].kind === "try") {
            vm.handlerStack.splice(i, 1);
            break;
          }
        }
        break;
      }
      case 85: { // PROVIDE
        // Install a no-fiber tail-resumptive handler; the body runs inline. Pop the
        // n (opName, armClosure) pairs pushed by the compiler.
        const nOps85 = op[1];
        const ops85 = [];
        for (let i = 0; i < nOps85; i++) {
          const arm = fiber.stack[--fiber.sp];
          const opStr = asString(fiber.stack[--fiber.sp]);
          ops85.push([opStr, arm]);
        }
        vm.handlerStack.push({
          kind: "provide",
          ops: ops85,
          fiber: fiber,
          frameDepth: fiber.frames.length,
          stackDepth: fiber.sp,
        });
        break;
      }
      case 86: { // PROVIDE_END
        // Normal completion of a provide body: pop the nearest provide-marker.
        for (let i = vm.handlerStack.length - 1; i >= 0; i--) {
          if (vm.handlerStack[i].kind === "provide") {
            vm.handlerStack.splice(i, 1);
            break;
          }
        }
        break;
      }
      case 44: { // ENTER_FUNC
        vm.returnStack.push({
          fiber: fiber,
          frameDepth: fiber.frames.length,
        });
        break;
      }
      case 45: { // EXIT_FUNC
        if (vm.returnStack.length === 0) error("EXIT_FUNC: no return entry");
        vm.returnStack.pop();
        break;
      }
      case 43: { // FUNC_RETURN
        const result = fiber.stack[--fiber.sp];
        // Find and remove the return entry for this fiber
        let reIdx = -1;
        for (let i = vm.returnStack.length - 1; i >= 0; i--) {
          if (vm.returnStack[i].fiber === fiber) { reIdx = i; break; }
        }
        if (reIdx === -1) error("FUNC_RETURN: no return entry");
        const re = vm.returnStack[reIdx];
        vm.returnStack.splice(reIdx, 1);
        const targetDepth = re.frameDepth;
        // Unwind frames back to the target function
        while (fiber.frames.length > targetDepth) fiber.frames.pop();
        // Clean up control_stack entries above target depth
        vm.controlStack = vm.controlStack.filter(ce =>
          !(ce.fiber === fiber && ce.frameDepth >= targetDepth)
        );
        // Drop inline try/provide markers opened inside the returned-from frames (a
        // `return` inside such a body skips its TRY_END/PROVIDE_END).
        vm.handlerStack = vm.handlerStack.filter(h =>
          !((h.kind === "try" || h.kind === "provide") &&
            h.fiber === fiber && h.frameDepth >= targetDepth)
        );
        dropProvideResumesAbove(vm, fiber, targetDepth);
        // Restore stack pointer and pop the target function's frame
        const baseSp = fiber.frames[fiber.frames.length - 1].baseSp;
        fiber.sp = baseSp;
        fiber.frames.pop();
        // Clear any extra_args from over-applications within the unwound frames
        fiber.extraArgs = [];
        if (fiber.frames.length === 0) {
          const he = findHandlerForFiber(vm, fiber);
          if (he) {
            removeHandler(vm, he);
            fiber = he.parentFiber;
            vm.currentFiber = fiber;
            internalCall(fiber, asClosure(he.returnHandler), result);
          } else {
            return result;
          }
        } else {
          fiber.stack[fiber.sp++] = result;
        }
        break;
      }
      case 69: { // MAKE_ARRAY
        const n = op[1];
        const elems = [];
        for (let i = 0; i < n; i++) {
          elems.unshift(fiber.stack[--fiber.sp]);
        }
        fiber.stack[fiber.sp++] = varray(elems);
        break;
      }
      case 70: { // INDEX
        const idx = asInt(fiber.stack[--fiber.sp]);
        const base = fiber.stack[--fiber.sp];
        if (base.tag === "string") {
          const s = base.v;
          // Get byte at index (matching OCaml's string byte indexing)
          const encoder = new TextEncoder();
          const bytes = encoder.encode(s);
          if (idx < 0 || idx >= bytes.length)
            error(`string index out of bounds: ${idx} (length ${bytes.length})`);
          fiber.stack[fiber.sp++] = vbyte(bytes[idx]);
        } else if (base.tag === "array") {
          if (idx < 0 || idx >= base.v.length)
            error(`array index out of bounds: ${idx} (length ${base.v.length})`);
          fiber.stack[fiber.sp++] = base.v[idx];
        } else {
          error("index operation requires string or array");
        }
        break;
      }
      case 74: { // GET_LOCAL_CALL
        const fnVal = fiber.stack[--fiber.sp];
        const arg = fiber.stack[f.baseSp + op[1]];
        if (fnVal.tag === "closure") {
          if (fnVal.proto.arity === 1) {
            const base = fiber.sp;
            const numLocals = fnVal.proto.num_locals;
            ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
            fiber.stack[base] = arg;
            fiber.sp = base + numLocals;
            fiber.frames.push({ closure: fnVal, ip: 0, baseSp: base });
          } else {
            fiber.stack[fiber.sp++] = vpartial(fnVal, [arg]);
          }
        } else if (fnVal.tag === "partial") {
          const newArgs = fnVal.args.concat([arg]);
          if (newArgs.length === fnVal.closure.proto.arity) {
            const cls = fnVal.closure;
            const base = fiber.sp;
            const numLocals = cls.proto.num_locals;
            ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
            for (let i = 0; i < newArgs.length; i++) fiber.stack[base + i] = newArgs[i];
            fiber.sp = base + numLocals;
            fiber.frames.push({ closure: cls, ip: 0, baseSp: base });
          } else {
            fiber.stack[fiber.sp++] = vpartial(fnVal.closure, newArgs);
          }
        } else if (fnVal.tag === "external") {
          const newArgs = fnVal.args.concat([arg]);
          if (newArgs.length === fnVal.arity) {
            fiber.stack[fiber.sp++] = fnVal.fn(newArgs);
          } else {
            fiber.stack[fiber.sp++] = vexternal(fnVal.name, fnVal.arity, fnVal.fn, newArgs);
          }
        } else {
          error(`expected function, got ${ppValue(fnVal)}`);
        }
        break;
      }
      case 75: { // GET_LOCAL_TUPLE_GET
        const tup = asTuple(fiber.stack[f.baseSp + op[1]]);
        const idx = op[2];
        if (idx >= tup.length) error("tuple index out of bounds");
        fiber.stack[fiber.sp++] = tup[idx];
        break;
      }
      case 76: { // GET_LOCAL_FIELD
        const rec = asRecord(fiber.stack[f.baseSp + op[1]]);
        const name = op[2];
        const entry = rec.find(([n]) => n === name);
        if (!entry) error(`record has no field: ${name}`);
        fiber.stack[fiber.sp++] = entry[1];
        break;
      }
      case 77: { // JUMP_TABLE
        const minTag = op[1];
        const targets = op[2];
        const defaultTarget = op[3];
        const v = asVariant(fiber.stack[--fiber.sp]);
        const idx = v.tagN - minTag;
        if (idx >= 0 && idx < targets.length) {
          f.ip = targets[idx];
        } else {
          f.ip = defaultTarget;
        }
        break;
      }
      case 78: { // GET_GLOBAL_CALL
        const fnVal = fiber.stack[--fiber.sp];
        const gidx = op[1];
        const arg = vm.globals.get(gidx);
        if (arg === undefined) {
          const name = gidx < vm.globalNames.length ? vm.globalNames[gidx] : "?";
          error(`undefined global: ${name}`);
        }
        if (fnVal.tag === "closure") {
          if (fnVal.proto.arity === 1) {
            const base = fiber.sp;
            const numLocals = fnVal.proto.num_locals;
            ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
            fiber.stack[base] = arg;
            fiber.sp = base + numLocals;
            fiber.frames.push({ closure: fnVal, ip: 0, baseSp: base });
          } else {
            fiber.stack[fiber.sp++] = vpartial(fnVal, [arg]);
          }
        } else if (fnVal.tag === "partial") {
          const newArgs = fnVal.args.concat([arg]);
          if (newArgs.length === fnVal.closure.proto.arity) {
            const cls = fnVal.closure;
            const base = fiber.sp;
            const numLocals = cls.proto.num_locals;
            ensureCap(fiber, base + numLocals); fiber.stack.fill(VUNIT, base, base + numLocals);
            for (let i = 0; i < newArgs.length; i++) fiber.stack[base + i] = newArgs[i];
            fiber.sp = base + numLocals;
            fiber.frames.push({ closure: cls, ip: 0, baseSp: base });
          } else {
            fiber.stack[fiber.sp++] = vpartial(fnVal.closure, newArgs);
          }
        } else if (fnVal.tag === "external") {
          const newArgs = fnVal.args.concat([arg]);
          if (newArgs.length === fnVal.arity) {
            fiber.stack[fiber.sp++] = fnVal.fn(newArgs);
          } else {
            fiber.stack[fiber.sp++] = vexternal(fnVal.name, fnVal.arity, fnVal.fn, newArgs);
          }
        } else {
          error(`expected function, got ${ppValue(fnVal)}`);
        }
        break;
      }
      case 79: { // GET_GLOBAL_FIELD
        const gidx = op[1];
        const name = op[2];
        const val = vm.globals.get(gidx);
        if (val === undefined) {
          const gname = gidx < vm.globalNames.length ? vm.globalNames[gidx] : "?";
          error(`undefined global: ${gname}`);
        }
        const rec = asRecord(val);
        const entry = rec.find(([n]) => n === name);
        if (!entry) error(`record has no field: ${name}`);
        fiber.stack[fiber.sp++] = entry[1];
        break;
      }
      case 82: { // UPDATE_REC
        const placeholder = fiber.stack[--fiber.sp];
        const computed = fiber.stack[--fiber.sp];
        if (placeholder.tag === "list" && computed.tag === "list") {
          placeholder.hd = computed.hd;
          placeholder.tl = computed.tl;
        } else if (placeholder.tag === "tuple" && computed.tag === "tuple") {
          for (let i = 0; i < computed.v.length; i++) placeholder.v[i] = computed.v[i];
        } else if (placeholder.tag === "record" && computed.tag === "record") {
          for (let i = 0; i < computed.v.length; i++) placeholder.v[i] = computed.v[i];
        } else if (placeholder.tag === "array" && computed.tag === "array") {
          for (let i = 0; i < computed.v.length; i++) placeholder.v[i] = computed.v[i];
        } else if (placeholder.tag === "variant" && computed.tag === "variant") {
          placeholder.tagN = computed.tagN;
          placeholder.name = computed.name;
          placeholder.payload = computed.payload;
        } else {
          error("UPDATE_REC: type mismatch between placeholder and computed value");
        }
        break;
      }
      case 71: // HALT
        return fiber.sp > 0 ? fiber.stack[fiber.sp - 1] : VUNIT;
      default:
        error(`unknown opcode: ${op[0]}`);
    }
  }
}

function getProto(currentProto, protoIdx) {
  const c = currentProto.constants[protoIdx];
  if (c.tag !== "proto") error(`expected prototype at constant ${protoIdx}`);
  return c.v;
}

function resolveCapture(fiber, frame, cap) {
  if (cap[0] === "local") return fiber.stack[frame.baseSp + cap[1]];
  if (cap[0] === "upvalue") return frame.closure.upvalues[cap[1]];
  error(`unknown capture type: ${cap[0]}`);
}

// --- Execute a prototype in a VM ---
function executeProto(vm, proto) {
  const fiber = makeFiber();
  const closure = vclosure(proto, []);
  const numLocals = proto.num_locals;
  ensureCap(fiber, numLocals);
  fiber.stack.fill(VUNIT, 0, numLocals);
  fiber.sp = numLocals;
  fiber.frames.push({ closure, ip: 0, baseSp: 0 });
  vm.currentFiber = fiber;
  try {
    return run(vm);
  } catch (e) {
    if (e instanceof RuntimeError) {
      const fiber = vm.currentFiber;
      const f = fiber.frames.length > 0 ? fiber.frames[fiber.frames.length - 1] : null;
      if (f) {
        const ip = Math.max(0, f.ip - 1);
        const lt = f.closure.proto.line_table;
        const line = lt && ip < lt.length ? lt[ip] : 0;
        if (line > 0) throw new RuntimeError(`[line ${line}] ${e.message}`);
      }
    }
    throw e;
  }
}

// --- Create a fresh VM ---
function createVM(globalNames) {
  return {
    currentFiber: makeFiber(),
    handlerStack: [],
    controlStack: [],
    returnStack: [],
    provideResumes: [],
    globals: new Map(),
    globalNames,
  };
}

// --- Call a closure with one argument on an existing VM ---
function callClosure(vmInst, closure, arg) {
  const fiber = makeFiber();
  const numLocals = closure.proto.num_locals;
  fiber.stack.fill(VUNIT, 0, numLocals);
  fiber.stack[0] = arg;
  fiber.sp = numLocals;
  fiber.frames.push({ closure, ip: 0, baseSp: 0 });
  vmInst.currentFiber = fiber;
  vmInst.handlerStack = [];
  vmInst.controlStack = [];
  vmInst.returnStack = [];
  vmInst.provideResumes = [];
  return run(vmInst);
}

module.exports = {
  RuntimeError,
  error,
  VUNIT, VTRUE, VFALSE, VNIL,
  vint, vfloat, vbool, vstring, vbyte, vrune,
  vtuple, vlist, vrecord, vvariant, vclosure, vexternal,
  vcontinuation, vref, varray, vproto,
  asInt, asFloat, asBool, asString, asClosure, asTuple,
  asRecord, asList, asVariant, asContinuation, asByte, asRune,
  asArray, listToArray,
  valuesEqual, valuesCompare, valueHash, ppValue, formatFloat, runeToUtf8,
  makeFiber, copyFiber,
  run, executeProto, createVM, callClosure,
  resetProfile, dumpProfile,
};
