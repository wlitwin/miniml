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
const vlist = (vs) => ({ tag: "list", v: vs });
const VNIL = vlist([]);
const vrecord = (fields) => ({ tag: "record", v: fields }); // fields: [[name, val], ...]
const vvariant = (tagN, name, payload) => ({ tag: "variant", tagN, name, payload });
const vclosure = (proto, upvalues) => ({ tag: "closure", proto, upvalues });
const vexternal = (name, arity, fn, args) => ({ tag: "external", name, arity, fn, args: args || [] });
const vcontinuation = (fiber, returnHandler, opHandlers) =>
  ({ tag: "continuation", fiber, returnHandler, opHandlers, used: false });
const vref = (v) => ({ tag: "ref", v: [v] }); // use array for mutability
const vmap = (pairs) => ({ tag: "map", v: pairs }); // pairs: [[k, v], ...]
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
function asList(v) { if (v.tag === "list") return v.v; error(`expected list, got ${ppValue(v)}`); }
function asVariant(v) { if (v.tag === "variant") return v; error(`expected variant, got ${ppValue(v)}`); }
function asContinuation(v) { if (v.tag === "continuation") return v; error(`expected continuation, got ${ppValue(v)}`); }
function asByte(v) { if (v.tag === "byte") return v.v; error(`expected byte, got ${ppValue(v)}`); }
function asRune(v) { if (v.tag === "rune") return v.v; error(`expected rune, got ${ppValue(v)}`); }
function asMap(v) { if (v.tag === "map") return v.v; error(`expected map, got ${ppValue(v)}`); }
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
    case "list":
      if (a.v.length !== b.v.length) return false;
      for (let i = 0; i < a.v.length; i++) if (!valuesEqual(a.v[i], b.v[i])) return false;
      return true;
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

// --- Pretty-print (matches bytecode.ml pp_value) ---
function runeToUtf8(cp) {
  if (cp < 0x80) return String.fromCharCode(cp);
  if (cp < 0x800) return String.fromCharCode(0xC0 | (cp >> 6), 0x80 | (cp & 0x3F));
  if (cp < 0x10000)
    return String.fromCharCode(0xE0 | (cp >> 12), 0x80 | ((cp >> 6) & 0x3F), 0x80 | (cp & 0x3F));
  return String.fromCodePoint(cp);
}

function ppValue(v) {
  switch (v.tag) {
    case "int": return String(v.v);
    case "float": {
      let s = String(v.v);
      if (!s.includes('.') && !s.includes('e') && !s.includes('E') && isFinite(v.v)) s += ".";
      return s;
    }
    case "bool": return v.v ? "true" : "false";
    case "string": return v.v;
    case "byte": return "#" + v.v.toString(16).padStart(2, '0');
    case "rune": return "'" + runeToUtf8(v.v) + "'";
    case "unit": return "()";
    case "tuple": return "(" + v.v.map(ppValue).join(", ") + ")";
    case "list": return "[" + v.v.map(ppValue).join("; ") + "]";
    case "record":
      return "{ " + v.v.map(([n, val]) => n + " = " + ppValue(val)).join("; ") + " }";
    case "variant":
      if (v.payload === null) return v.name;
      const pv = ppValue(v.payload);
      if (v.payload.tag === "tuple" || (v.payload.tag === "variant" && v.payload.payload !== null))
        return v.name + " (" + pv + ")";
      return v.name + " " + pv;
    case "closure": return "<fun>";
    case "external":
      if (v.args.length === 0) return `<external:${v.name}>`;
      return "<fun>";
    case "proto": return `<proto:${v.v.name}>`;
    case "continuation": return "<continuation>";
    case "ref": return `ref(${ppValue(v.v[0])})`;
    case "map": {
      const isSet = v.v.every(([_, val]) => val.tag === "unit");
      if (isSet) return "#{" + v.v.map(([k]) => ppValue(k)).join("; ") + "}";
      return "#{" + v.v.map(([k, val]) => ppValue(k) + ": " + ppValue(val)).join("; ") + "}";
    }
    case "array": return "#[" + v.v.map(ppValue).join("; ") + "]";
    default: return "<unknown>";
  }
}

// --- Fiber ---
const STACK_SIZE = 4096;

function makeFiber() {
  return { stack: new Array(STACK_SIZE), sp: 0, frames: [] };
}

function copyFiber(f) {
  const newStack = f.stack.slice();
  const newFrames = f.frames.map(fr => ({
    closure: fr.closure,
    ip: fr.ip,
    locals: fr.locals.slice(),
    baseSp: fr.baseSp,
  }));
  return { stack: newStack, sp: f.sp, frames: newFrames };
}

// --- Top-level helper functions ---
function findHandler(vm, opName) {
  for (const he of vm.handlerStack) {
    for (const [name] of he.ops) {
      if (name === opName) return he;
    }
  }
  return null;
}

function findHandlerForFiber(vm, f) {
  for (const he of vm.handlerStack) {
    if (he.bodyFiber === f) return he;
  }
  return null;
}

function removeHandler(vm, he) {
  vm.handlerStack = vm.handlerStack.filter(h => h !== he);
}

function internalCall(fiber, cls, arg) {
  const newLocals = new Array(cls.proto.num_locals).fill(VUNIT);
  newLocals[0] = arg;
  fiber.frames.push({ closure: cls, ip: 0, locals: newLocals, baseSp: fiber.sp });
}

// --- Opcode profiling (toggle with vm.profile = true) ---
const OP_COUNTS = new Uint32Array(81);

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
        fiber.stack[fiber.sp++] = f.locals[op[1]];
        break;
      case 4: // SET_LOCAL
        f.locals[op[1]] = fiber.stack[--fiber.sp];
        break;
      case 5: // GET_UPVALUE
        fiber.stack[fiber.sp++] = f.closure.upvalues[op[1]];
        break;
      case 6: // SET_UPVALUE
        f.closure.upvalues[op[1]] = fiber.stack[--fiber.sp];
        break;
      case 7: // MAKE_REF
        fiber.stack[fiber.sp - 1] = vref(fiber.stack[fiber.sp - 1]);
        break;
      case 8: { // DEREF
        const r = fiber.stack[fiber.sp - 1];
        if (r.tag !== "ref") error("DEREF on non-ref value");
        fiber.stack[fiber.sp - 1] = r.v[0];
        break;
      }
      case 9: { // SET_REF
        const r = fiber.stack[--fiber.sp];
        const v = fiber.stack[--fiber.sp];
        if (r.tag !== "ref") error("SET_REF on non-ref value");
        r.v[0] = v;
        break;
      }
      case 10: { // GET_GLOBAL
        const idx = op[1];
        const v = vm.globals.get(idx);
        if (v === undefined) {
          const name = idx < vm.globalNames.length ? vm.globalNames[idx] : "?";
          error(`undefined global: ${name}`);
        }
        fiber.stack[fiber.sp++] = v;
        break;
      }
      case 11: // SET_GLOBAL
        vm.globals.set(op[1], fiber.stack[--fiber.sp]);
        break;
      case 12: // DEF_GLOBAL
        vm.globals.set(op[1], fiber.stack[--fiber.sp]);
        break;
      case 13: { // ADD
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) + asInt(b));
        break;
      }
      case 14: { // SUB
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) - asInt(b));
        break;
      }
      case 15: { // MUL
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) * asInt(b));
        break;
      }
      case 16: { // DIV
        const b = fiber.stack[--fiber.sp];
        const bv = asInt(b);
        if (bv === 0) error("division by zero");
        fiber.stack[fiber.sp - 1] = vint(Math.trunc(asInt(fiber.stack[fiber.sp - 1]) / bv));
        break;
      }
      case 17: { // MOD
        const b = fiber.stack[--fiber.sp];
        const bv = asInt(b);
        if (bv === 0) error("modulo by zero");
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) % bv);
        break;
      }
      case 18: // NEG
        fiber.stack[fiber.sp - 1] = vint(-asInt(fiber.stack[fiber.sp - 1]));
        break;
      case 19: { // FADD
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vfloat(asFloat(fiber.stack[fiber.sp - 1]) + asFloat(b));
        break;
      }
      case 20: { // FSUB
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vfloat(asFloat(fiber.stack[fiber.sp - 1]) - asFloat(b));
        break;
      }
      case 21: { // FMUL
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vfloat(asFloat(fiber.stack[fiber.sp - 1]) * asFloat(b));
        break;
      }
      case 22: { // FDIV
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vfloat(asFloat(fiber.stack[fiber.sp - 1]) / asFloat(b));
        break;
      }
      case 23: // FNEG
        fiber.stack[fiber.sp - 1] = vfloat(-asFloat(fiber.stack[fiber.sp - 1]));
        break;
      case 24: { // EQ
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vbool(valuesEqual(fiber.stack[fiber.sp - 1], b));
        break;
      }
      case 25: { // NEQ
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vbool(!valuesEqual(fiber.stack[fiber.sp - 1], b));
        break;
      }
      case 26: { // LT
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
      case 27: { // GT
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
      case 28: { // LE
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
      case 29: { // GE
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
      case 30: // NOT
        fiber.stack[fiber.sp - 1] = vbool(!asBool(fiber.stack[fiber.sp - 1]));
        break;
      case 31: { // BAND
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) & asInt(b));
        break;
      }
      case 32: { // BOR
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) | asInt(b));
        break;
      }
      case 33: { // BXOR
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) ^ asInt(b));
        break;
      }
      case 34: // BNOT
        fiber.stack[fiber.sp - 1] = vint(~asInt(fiber.stack[fiber.sp - 1]));
        break;
      case 35: { // BSHL
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) << asInt(b));
        break;
      }
      case 36: { // BSHR
        const b = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp - 1] = vint(asInt(fiber.stack[fiber.sp - 1]) >>> asInt(b));
        break;
      }
      case 37: // JUMP
        f.ip = op[1];
        break;
      case 38: { // JUMP_IF_FALSE
        const v = fiber.stack[--fiber.sp];
        if (!asBool(v)) f.ip = op[1];
        break;
      }
      case 39: { // JUMP_IF_TRUE
        const v = fiber.stack[--fiber.sp];
        if (asBool(v)) f.ip = op[1];
        break;
      }
      case 40: { // CLOSURE
        const protoIdx = op[1];
        const captures = op[2];
        const fnProto = getProto(f.closure.proto, protoIdx);
        const upvalues = captures.map(cap => resolveCapture(f, cap));
        fiber.stack[fiber.sp++] = vclosure(fnProto, upvalues);
        break;
      }
      case 41: { // CLOSURE_REC
        const protoIdx = op[1];
        const captures = op[2];
        const selfIdx = op[3];
        const fnProto = getProto(f.closure.proto, protoIdx);
        const upvalues = captures.map(cap => resolveCapture(f, cap));
        const cls = vclosure(fnProto, upvalues);
        cls.upvalues[selfIdx] = cls; // circular reference for recursion
        fiber.stack[fiber.sp++] = cls;
        break;
      }
      case 42: { // CALL
        const arg = fiber.stack[--fiber.sp];
        const fnVal = fiber.stack[--fiber.sp];
        if (fnVal.tag === "closure") {
          const baseSp = fiber.sp;
          const newLocals = new Array(fnVal.proto.num_locals).fill(VUNIT);
          newLocals[0] = arg;
          fiber.frames.push({ closure: fnVal, ip: 0, locals: newLocals, baseSp });
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
      case 43: { // TAIL_CALL
        const arg = fiber.stack[--fiber.sp];
        const fnVal = fiber.stack[--fiber.sp];
        if (fnVal.tag === "closure") {
          const currentBaseSp = f.baseSp;
          const newLocals = new Array(fnVal.proto.num_locals).fill(VUNIT);
          newLocals[0] = arg;
          fiber.frames[fiber.frames.length - 1] = { closure: fnVal, ip: 0, locals: newLocals, baseSp: currentBaseSp };
        } else if (fnVal.tag === "external") {
          const newArgs = fnVal.args.concat([arg]);
          let result;
          if (newArgs.length === fnVal.arity) {
            result = fnVal.fn(newArgs);
          } else {
            result = vexternal(fnVal.name, fnVal.arity, fnVal.fn, newArgs);
          }
          // Proper tail return: pop frame and return result
          fiber.sp = f.baseSp;
          fiber.frames.pop();
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
        } else {
          error(`expected function, got ${ppValue(fnVal)}`);
        }
        break;
      }
      case 44: { // RETURN
        const result = fiber.stack[--fiber.sp];
        fiber.frames.pop();
        if (fiber.frames.length === 0) {
          // Last frame — check for handler
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
      case 48: { // MAKE_TUPLE
        const n = op[1];
        const values = new Array(n);
        for (let i = n - 1; i >= 0; i--) values[i] = fiber.stack[--fiber.sp];
        fiber.stack[fiber.sp++] = vtuple(values);
        break;
      }
      case 49: { // TUPLE_GET
        const idx = op[1];
        const tup = asTuple(fiber.stack[fiber.sp - 1]);
        if (idx >= tup.length) error("tuple index out of bounds");
        fiber.stack[fiber.sp - 1] = tup[idx];
        break;
      }
      case 50: { // MAKE_RECORD
        const fieldNames = op[1];
        const n = fieldNames.length;
        const values = new Array(n);
        for (let i = n - 1; i >= 0; i--) values[i] = fiber.stack[--fiber.sp];
        const fields = fieldNames.map((name, i) => [name, values[i]]);
        fiber.stack[fiber.sp++] = vrecord(fields);
        break;
      }
      case 51: { // FIELD
        const name = op[1];
        const rec = asRecord(fiber.stack[fiber.sp - 1]);
        const entry = rec.find(([n]) => n === name);
        if (!entry) error(`record has no field: ${name}`);
        fiber.stack[fiber.sp - 1] = entry[1];
        break;
      }
      case 52: { // SET_FIELD
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
      case 75: { // RECORD_UPDATE
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
      case 76: { // RECORD_UPDATE_DYN
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
      case 53: { // MAKE_VARIANT
        const [, tagN, name, hasPayload] = op;
        if (hasPayload) {
          fiber.stack[fiber.sp - 1] = vvariant(tagN, name, fiber.stack[fiber.sp - 1]);
        } else {
          fiber.stack[fiber.sp++] = vvariant(tagN, name, null);
        }
        break;
      }
      case 54: { // CONS
        const tl = fiber.stack[--fiber.sp];
        const hd = fiber.stack[--fiber.sp];
        const tlList = asList(tl);
        fiber.stack[fiber.sp++] = vlist([hd, ...tlList]);
        break;
      }
      case 55: // NIL
        fiber.stack[fiber.sp++] = VNIL;
        break;
      case 56: { // TAG_EQ
        const tagN = op[1];
        fiber.stack[fiber.sp - 1] = vbool(asVariant(fiber.stack[fiber.sp - 1]).tagN === tagN);
        break;
      }
      case 57: // IS_NIL
        fiber.stack[fiber.sp - 1] = vbool(asList(fiber.stack[fiber.sp - 1]).length === 0);
        break;
      case 58: // IS_CONS
        fiber.stack[fiber.sp - 1] = vbool(asList(fiber.stack[fiber.sp - 1]).length > 0);
        break;
      case 59: { // HEAD
        const l = asList(fiber.stack[fiber.sp - 1]);
        if (l.length === 0) error("head of empty list");
        fiber.stack[fiber.sp - 1] = l[0];
        break;
      }
      case 60: { // TAIL
        const l = asList(fiber.stack[fiber.sp - 1]);
        if (l.length === 0) error("tail of empty list");
        fiber.stack[fiber.sp - 1] = vlist(l.slice(1));
        break;
      }
      case 61: { // VARIANT_PAYLOAD
        const v = asVariant(fiber.stack[fiber.sp - 1]);
        if (v.payload === null) error("variant has no payload");
        fiber.stack[fiber.sp - 1] = v.payload;
        break;
      }
      case 62: // MATCH_FAIL
        error(`non-exhaustive match at ${op[1]}`);
        break;
      case 63: { // PERFORM
        const opNameStr = op[1];
        const arg = fiber.stack[--fiber.sp];
        const he = findHandler(vm, opNameStr);
        if (!he) error(`unhandled effect operation: ${opNameStr}`);
        const handlerFn = he.ops.find(([name]) => name === opNameStr)[1];
        const cont = vcontinuation(fiber, he.returnHandler, he.ops);
        const pair = vtuple([arg, cont]);
        removeHandler(vm, he);
        fiber = he.parentFiber;
        vm.currentFiber = fiber;
        internalCall(fiber, asClosure(handlerFn), pair);
        break;
      }
      case 64: { // HANDLE
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
      case 65: { // RESUME
        const v = fiber.stack[--fiber.sp];
        const cont = asContinuation(fiber.stack[--fiber.sp]);
        if (cont.used) error("continuation already resumed");
        cont.used = true;
        const bodyFiber = cont.fiber;
        bodyFiber.stack[bodyFiber.sp++] = v;
        const he = {
          returnHandler: cont.returnHandler,
          ops: cont.opHandlers,
          bodyFiber,
          parentFiber: fiber,
        };
        vm.handlerStack.push(he);
        fiber = bodyFiber;
        vm.currentFiber = fiber;
        break;
      }
      case 66: { // ENTER_LOOP
        const breakTarget = op[1];
        vm.controlStack.push({
          breakIp: breakTarget,
          fiber: fiber,
          frameDepth: fiber.frames.length,
          stackDepth: fiber.sp,
        });
        break;
      }
      case 67: { // EXIT_LOOP
        if (vm.controlStack.length === 0) error("EXIT_LOOP: no control entry");
        vm.controlStack.pop();
        break;
      }
      case 68: { // LOOP_BREAK
        const breakValue = fiber.stack[--fiber.sp];
        if (vm.controlStack.length === 0) error("LOOP_BREAK: no control entry");
        const ce = vm.controlStack.pop();
        const cf = ce.fiber;
        while (cf.frames.length > ce.frameDepth) cf.frames.pop();
        cf.sp = ce.stackDepth;
        vm.currentFiber = cf;
        fiber = cf;
        fiber.stack[fiber.sp++] = breakValue;
        fiber.frames[fiber.frames.length - 1].ip = ce.breakIp;
        break;
      }
      case 69: { // LOOP_CONTINUE
        const target = op[1];
        if (vm.controlStack.length === 0) error("LOOP_CONTINUE: no control entry");
        const ce = vm.controlStack[vm.controlStack.length - 1];
        ce.fiber.sp = ce.stackDepth;
        f.ip = target;
        break;
      }
      case 70: { // FOLD_CONTINUE
        const nPops = op[1];
        const continueValue = fiber.stack[--fiber.sp];
        for (let i = 0; i < nPops; i++) {
          if (fiber.sp > 0) fiber.sp--;
        }
        fiber.stack[fiber.sp++] = continueValue;
        // Execute RETURN from fold callback
        const result = fiber.stack[--fiber.sp];
        if (fiber.frames.length <= 1) error("FOLD_CONTINUE: no frame to return from");
        fiber.frames.pop();
        fiber.stack[fiber.sp++] = result;
        break;
      }
      case 46: { // ENTER_FUNC
        vm.returnStack.push({
          fiber: fiber,
          frameDepth: fiber.frames.length,
        });
        break;
      }
      case 47: { // EXIT_FUNC
        if (vm.returnStack.length === 0) error("EXIT_FUNC: no return entry");
        vm.returnStack.pop();
        break;
      }
      case 45: { // FUNC_RETURN
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
        // Restore stack pointer and pop the target function's frame
        const baseSp = fiber.frames[fiber.frames.length - 1].baseSp;
        fiber.sp = baseSp;
        fiber.frames.pop();
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
      case 71: { // MAKE_MAP
        const n = op[1];
        const pairs = [];
        for (let i = 0; i < n; i++) {
          const v = fiber.stack[--fiber.sp];
          const k = fiber.stack[--fiber.sp];
          pairs.unshift([k, v]);
        }
        fiber.stack[fiber.sp++] = vmap(pairs);
        break;
      }
      case 72: { // MAKE_ARRAY
        const n = op[1];
        const elems = [];
        for (let i = 0; i < n; i++) {
          elems.unshift(fiber.stack[--fiber.sp]);
        }
        fiber.stack[fiber.sp++] = varray(elems);
        break;
      }
      case 73: { // INDEX
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
      case 77: { // GET_LOCAL_CALL
        const fnVal = fiber.stack[--fiber.sp];
        const arg = f.locals[op[1]];
        if (fnVal.tag === "closure") {
          const baseSp = fiber.sp;
          const newLocals = new Array(fnVal.proto.num_locals).fill(VUNIT);
          newLocals[0] = arg;
          fiber.frames.push({ closure: fnVal, ip: 0, locals: newLocals, baseSp });
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
      case 78: { // GET_LOCAL_TUPLE_GET
        const tup = asTuple(f.locals[op[1]]);
        const idx = op[2];
        if (idx >= tup.length) error("tuple index out of bounds");
        fiber.stack[fiber.sp++] = tup[idx];
        break;
      }
      case 79: { // GET_LOCAL_FIELD
        const rec = asRecord(f.locals[op[1]]);
        const name = op[2];
        const entry = rec.find(([n]) => n === name);
        if (!entry) error(`record has no field: ${name}`);
        fiber.stack[fiber.sp++] = entry[1];
        break;
      }
      case 80: { // JUMP_TABLE
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
      case 74: // HALT
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

function resolveCapture(frame, cap) {
  if (cap[0] === "local") return frame.locals[cap[1]];
  if (cap[0] === "upvalue") return frame.closure.upvalues[cap[1]];
  error(`unknown capture type: ${cap[0]}`);
}

// --- Execute a prototype in a VM ---
function executeProto(vm, proto) {
  const fiber = makeFiber();
  const closure = vclosure(proto, []);
  const locals = new Array(proto.num_locals).fill(VUNIT);
  fiber.frames.push({ closure, ip: 0, locals, baseSp: 0 });
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
    globals: new Map(),
    globalNames,
  };
}

// --- Call a closure with one argument on an existing VM ---
function callClosure(vmInst, closure, arg) {
  const fiber = makeFiber();
  const locals = new Array(closure.proto.num_locals).fill(VUNIT);
  locals[0] = arg;
  fiber.frames.push({ closure, ip: 0, locals, baseSp: 0 });
  vmInst.currentFiber = fiber;
  vmInst.handlerStack = [];
  vmInst.controlStack = [];
  vmInst.returnStack = [];
  return run(vmInst);
}

module.exports = {
  RuntimeError,
  error,
  VUNIT, VTRUE, VFALSE, VNIL,
  vint, vfloat, vbool, vstring, vbyte, vrune,
  vtuple, vlist, vrecord, vvariant, vclosure, vexternal,
  vcontinuation, vref, vmap, varray, vproto,
  asInt, asFloat, asBool, asString, asClosure, asTuple,
  asRecord, asList, asVariant, asContinuation, asByte, asRune,
  asMap, asArray,
  valuesEqual, ppValue, runeToUtf8,
  makeFiber, copyFiber,
  run, executeProto, createVM, callClosure,
  resetProfile, dumpProfile,
};
