// Loader: deserializes JSON bundle and runs it

const vm = require("./vm");
const { builtins } = require("./builtins");

function deserializeValue(j) {
  switch (j.t) {
    case "i": return vm.vint(j.v);
    case "f": return vm.vfloat(j.v);
    case "b": return vm.vbool(j.v);
    case "s": return vm.vstring(j.v);
    case "y": return vm.vbyte(j.v);
    case "r": return vm.vrune(j.v);
    case "u": return vm.VUNIT;
    case "p": return vm.vproto(deserializePrototype(j.v));
    case "T": return vm.vtuple(j.v.map(deserializeValue));
    case "L": return vm.vlist(j.v.map(deserializeValue));
    case "V": {
      const payload = j.payload === null ? null : deserializeValue(j.payload);
      return vm.vvariant(j.tag, j.name, payload);
    }
    default:
      vm.error(`unknown value type: ${j.t}`);
  }
}

const OPCODE_NAMES = [
  "CONST", "POP", "DUP", "GET_LOCAL", "SET_LOCAL", "GET_UPVALUE",
  "MAKE_REF", "DEREF", "SET_REF", "GET_GLOBAL", "DEF_GLOBAL",
  "ADD", "SUB", "MUL", "DIV", "MOD", "NEG",
  "FADD", "FSUB", "FMUL", "FDIV", "FNEG",
  "EQ", "NEQ", "LT", "GT", "LE", "GE", "NOT",
  "BAND", "BOR", "BXOR", "BNOT", "BSHL", "BSHR",
  "JUMP", "JUMP_IF_FALSE", "JUMP_IF_TRUE",
  "CLOSURE", "CLOSURE_REC",
  "CALL", "TAIL_CALL", "RETURN", "FUNC_RETURN", "ENTER_FUNC", "EXIT_FUNC",
  "MAKE_TUPLE", "TUPLE_GET",
  "MAKE_RECORD", "FIELD", "SET_FIELD", "MAKE_VARIANT",
  "CONS", "NIL", "TAG_EQ", "IS_NIL", "IS_CONS", "HEAD", "TAIL",
  "VARIANT_PAYLOAD", "MATCH_FAIL",
  "PERFORM", "HANDLE", "RESUME",
  "ENTER_LOOP", "EXIT_LOOP", "LOOP_BREAK", "LOOP_CONTINUE", "FOLD_CONTINUE",
  "MAKE_ARRAY", "INDEX", "HALT",
  "RECORD_UPDATE", "RECORD_UPDATE_DYN",
  "GET_LOCAL_CALL", "GET_LOCAL_TUPLE_GET", "GET_LOCAL_FIELD", "JUMP_TABLE",
  "GET_GLOBAL_CALL", "GET_GLOBAL_FIELD",
  "CALL_N", "TAIL_CALL_N",
  "UPDATE_REC",
  "TRY_BEGIN", "TRY_END",
  "PROVIDE", "PROVIDE_END",
];

// Map string opcode names to numeric tags for the VM switch
const OPCODE_TO_NUM = {};
OPCODE_NAMES.forEach((name, idx) => OPCODE_TO_NUM[name] = idx);

function convertCode(code) {
  for (let i = 0; i < code.length; i++) {
    code[i][0] = OPCODE_TO_NUM[code[i][0]];
  }
  return code;
}

function deserializePrototype(j) {
  return {
    name: j.name,
    arity: j.arity,
    num_locals: j.num_locals,
    code: convertCode(j.code),
    constants: j.constants.map(deserializeValue),
    line_table: j.line_table || [],
  };
}

function makeExternal(name, arity, fn) {
  return vm.vexternal(name, arity, (args) => fn(args));
}

function registerBuiltins(vmInst, bundle) {
  const nativeGlobals = bundle.native_globals;
  for (const [idxStr, info] of Object.entries(nativeGlobals)) {
    const idx = parseInt(idxStr, 10);
    if (info.type === "external") {
      const b = builtins[info.name];
      if (!b) {
        // Skip unknown builtins (e.g. Runtime.eval)
        // Register a stub that errors
        vmInst.globals.set(idx, makeExternal(info.name, info.arity, () => {
          vm.error(`unimplemented builtin: ${info.name}`);
        }));
        continue;
      }
      vmInst.globals.set(idx, makeExternal(info.name, b.arity, b.fn));
    } else if (info.type === "dict") {
      // Typeclass dictionary: record of externals
      const fields = Object.entries(info.fields).map(([fieldName, fieldInfo]) => {
        const b = builtins[fieldInfo.name];
        if (!b) {
          return [fieldName, makeExternal(fieldInfo.name, fieldInfo.arity, () => {
            vm.error(`unimplemented builtin: ${fieldInfo.name}`);
          })];
        }
        return [fieldName, makeExternal(fieldInfo.name, b.arity, b.fn)];
      });
      vmInst.globals.set(idx, vm.vrecord(fields));
    }
  }
}

function loadBundle(jsonString) {
  const bundle = JSON.parse(jsonString);
  const globalNames = bundle.global_names;
  const vmInst = vm.createVM(globalNames);

  // Register native builtins
  registerBuiltins(vmInst, bundle);

  // Register any user-declared externs that match builtins but aren't in native_globals
  for (let i = 0; i < globalNames.length; i++) {
    if (!vmInst.globals.has(i) && builtins[globalNames[i]]) {
      const b = builtins[globalNames[i]];
      vmInst.globals.set(i, makeExternal(globalNames[i], b.arity, b.fn));
    }
  }

  // Run setup prototypes (rebuild source-compiled stdlib)
  for (const setupJson of bundle.setup) {
    const proto = deserializePrototype(setupJson);
    vm.executeProto(vmInst, proto);
  }

  // Store vmInst so canvas apps can call closures after loadBundle returns
  globalThis._vmInstance = vmInst;

  // Run main prototype
  const mainProto = deserializePrototype(bundle.main);
  return vm.executeProto(vmInst, mainProto);
}

// --- Binary MMLB deserializer ---

// Tags for opcodes that take a single u32 operand
const U32_OPCODES = new Set([
  0,  // CONST
  3,  // GET_LOCAL
  4,  // SET_LOCAL
  5,  // GET_UPVALUE
  9,  // GET_GLOBAL
  10, // DEF_GLOBAL
  35, // JUMP
  36, // JUMP_IF_FALSE
  37, // JUMP_IF_TRUE
  40, // CALL
  41, // TAIL_CALL
  46, // MAKE_TUPLE
  47, // TUPLE_GET
  54, // TAG_EQ
  62, // HANDLE
  64, // ENTER_LOOP
  67, // LOOP_CONTINUE
  68, // FOLD_CONTINUE
  69, // MAKE_ARRAY
  73, // RECORD_UPDATE_DYN
  80, // CALL_N
  81, // TAIL_CALL_N
  85, // PROVIDE
]);

function loadBundleBinary(arrayBuffer) {
  const dv = new DataView(arrayBuffer);
  let pos = 0;

  function readU8() { const v = dv.getUint8(pos); pos += 1; return v; }
  function readU32() { const v = dv.getUint32(pos, true); pos += 4; return v; }
  function readI64() { const v = dv.getBigInt64(pos, true); pos += 8; return Number(v); }
  function readF64() { const v = dv.getFloat64(pos, true); pos += 8; return v; }

  // Header
  const magic = String.fromCharCode(readU8(), readU8(), readU8(), readU8());
  if (magic !== "MMLB") throw new Error(`invalid binary bundle magic: ${magic}`);
  const version = readU32();
  if (version !== 1) throw new Error(`unsupported binary bundle version: ${version}`);

  // String table
  const strCount = readU32();
  const strs = new Array(strCount);
  const decoder = new TextDecoder("utf-8");
  for (let i = 0; i < strCount; i++) {
    const len = readU32();
    strs[i] = decoder.decode(new Uint8Array(arrayBuffer, pos, len));
    pos += len;
  }

  // Global names
  const numNames = readU32();
  const globalNames = new Array(numNames);
  for (let i = 0; i < numNames; i++) {
    globalNames[i] = strs[readU32()];
  }

  // Create VM
  const vmInst = vm.createVM(globalNames);

  // Native globals
  const numNatives = readU32();
  for (let i = 0; i < numNatives; i++) {
    const idx = readU32();
    const kind = readU8();
    if (kind === 0) {
      // external
      const name = strs[readU32()];
      const arity = readU32();
      const b = builtins[name];
      if (b) {
        vmInst.globals.set(idx, makeExternal(name, b.arity, b.fn));
      } else {
        vmInst.globals.set(idx, makeExternal(name, arity, () => {
          vm.error(`unimplemented builtin: ${name}`);
        }));
      }
    } else if (kind === 1) {
      // dict
      const numFields = readU32();
      const fields = [];
      for (let j = 0; j < numFields; j++) {
        const fieldName = strs[readU32()];
        const extName = strs[readU32()];
        const extArity = readU32();
        const b = builtins[extName];
        if (b) {
          fields.push([fieldName, makeExternal(extName, b.arity, b.fn)]);
        } else {
          fields.push([fieldName, makeExternal(extName, extArity, () => {
            vm.error(`unimplemented builtin: ${extName}`);
          })]);
        }
      }
      vmInst.globals.set(idx, vm.vrecord(fields));
    }
  }

  // Register any user-declared externs that match builtins but aren't in native_globals
  for (let i = 0; i < globalNames.length; i++) {
    if (!vmInst.globals.has(i) && builtins[globalNames[i]]) {
      const b = builtins[globalNames[i]];
      vmInst.globals.set(i, makeExternal(globalNames[i], b.arity, b.fn));
    }
  }

  function readCaptures() {
    const count = readU32();
    const caps = new Array(count);
    for (let i = 0; i < count; i++) {
      const kind = readU8();
      const idx = readU32();
      caps[i] = kind === 0 ? ["local", idx] : ["upvalue", idx];
    }
    return caps;
  }

  function readOpcode() {
    const tag = readU8();

    // Simple u32 operand opcodes
    if (U32_OPCODES.has(tag)) {
      return [tag, readU32()];
    }

    switch (tag) {
      // No-operand opcodes
      case 1: case 2: case 6: case 7: case 8:
      case 11: case 12: case 13: case 14: case 15: case 16:
      case 17: case 18: case 19: case 20: case 21:
      case 22: case 23: case 24: case 25: case 26: case 27: case 28:
      case 29: case 30: case 31: case 32: case 33: case 34:
      case 42: case 43: case 44: case 45:
      case 52: case 53: case 55: case 56: case 57: case 58:
      case 59: case 63: case 65: case 66: case 70: case 71:
      case 84: // TRY_END
      case 86: // PROVIDE_END
        return [tag];

      case 38: { // CLOSURE
        const protoIdx = readU32();
        const caps = readCaptures();
        return [38, protoIdx, caps];
      }
      case 39: { // CLOSURE_REC
        const protoIdx = readU32();
        const caps = readCaptures();
        const self = readU32();
        return [39, protoIdx, caps, self];
      }
      case 48: { // MAKE_RECORD
        const count = readU32();
        const fieldNames = new Array(count);
        for (let i = 0; i < count; i++) fieldNames[i] = strs[readU32()];
        return [48, fieldNames];
      }
      case 49: // FIELD
        return [49, strs[readU32()]];
      case 50: // SET_FIELD
        return [50, strs[readU32()]];
      case 51: { // MAKE_VARIANT
        const tagN = readU32();
        const vname = strs[readU32()];
        const hasPayload = readU8() !== 0;
        return [51, tagN, vname, hasPayload];
      }
      case 60: // MATCH_FAIL
        return [60, strs[readU32()]];
      case 61: // PERFORM
        return [61, strs[readU32()]];
      case 72: { // RECORD_UPDATE
        const count = readU32();
        const fieldNames = new Array(count);
        for (let i = 0; i < count; i++) fieldNames[i] = strs[readU32()];
        return [72, fieldNames];
      }
      case 74: { // GET_LOCAL_CALL
        const slot = readU32();
        const arity = readU32();
        return [74, slot, arity];
      }
      case 75: { // GET_LOCAL_TUPLE_GET
        const slot = readU32();
        const idx = readU32();
        return [75, slot, idx];
      }
      case 76: { // GET_LOCAL_FIELD
        const slot = readU32();
        const name = strs[readU32()];
        return [76, slot, name];
      }
      case 77: { // JUMP_TABLE
        const minTag = readU32();
        const tableSize = readU32();
        const targets = new Array(tableSize);
        for (let i = 0; i < tableSize; i++) targets[i] = readU32();
        const defaultTarget = readU32();
        return [77, minTag, targets, defaultTarget];
      }
      case 78: { // GET_GLOBAL_CALL
        const idx = readU32();
        const arity = readU32();
        return [78, idx, arity];
      }
      case 79: { // GET_GLOBAL_FIELD
        const idx = readU32();
        const name = strs[readU32()];
        return [79, idx, name];
      }
      case 83: { // TRY_BEGIN
        const count = readU32();
        const catchTable = new Array(count);
        for (let i = 0; i < count; i++) {
          const op = strs[readU32()];
          const ip = readU32();
          catchTable[i] = [op, ip];
        }
        return [83, catchTable];
      }
      default:
        throw new Error(`unknown opcode tag: ${tag}`);
    }
  }

  function readValue() {
    const tag = readU8();
    switch (tag) {
      case 0: return vm.vint(readI64());
      case 1: return vm.vfloat(readF64());
      case 2: return vm.vbool(readU8() !== 0);
      case 3: return vm.vstring(strs[readU32()]);
      case 4: return vm.vbyte(readU8());
      case 5: return vm.vrune(readU32());
      case 6: return vm.VUNIT;
      case 7: return vm.vproto(readPrototype());
      case 8: {
        const count = readU32();
        const vs = new Array(count);
        for (let i = 0; i < count; i++) vs[i] = readValue();
        return vm.vtuple(vs);
      }
      case 9: {
        const count = readU32();
        const vs = new Array(count);
        for (let i = 0; i < count; i++) vs[i] = readValue();
        return vm.vlist(vs);
      }
      case 10: {
        const tagN = readU32();
        const vname = strs[readU32()];
        const hasPayload = readU8() !== 0;
        const payload = hasPayload ? readValue() : null;
        return vm.vvariant(tagN, vname, payload);
      }
      default:
        throw new Error(`unknown value tag: ${tag}`);
    }
  }

  function readPrototype() {
    const name = strs[readU32()];
    const arity = readU32();
    const numLocals = readU32();
    const codeLen = readU32();
    const code = new Array(codeLen);
    for (let i = 0; i < codeLen; i++) code[i] = readOpcode();
    const constLen = readU32();
    const constants = new Array(constLen);
    for (let i = 0; i < constLen; i++) constants[i] = readValue();
    const linesLen = readU32();
    const lineTable = new Array(linesLen);
    for (let i = 0; i < linesLen; i++) lineTable[i] = readU32();
    return { name, arity, num_locals: numLocals, code, constants, line_table: lineTable };
  }

  // Setup protos
  const numSetup = readU32();
  for (let i = 0; i < numSetup; i++) {
    const proto = readPrototype();
    vm.executeProto(vmInst, proto);
  }

  // Store vmInst so canvas apps can call closures after loadBundle returns
  globalThis._vmInstance = vmInst;

  // Main proto
  const mainProto = readPrototype();
  return vm.executeProto(vmInst, mainProto);
}

module.exports = { loadBundle, loadBundleBinary, deserializeValue, deserializePrototype };
