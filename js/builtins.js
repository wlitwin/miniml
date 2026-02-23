// JS implementations of native builtins — matches interp.ml + std.ml externals

const vm = require("./vm");

function stringToUtf8Bytes(s) {
  return new TextEncoder().encode(s);
}

function utf8BytesToString(bytes) {
  return new TextDecoder().decode(bytes instanceof Uint8Array ? bytes : new Uint8Array(bytes));
}

// Build builtins map: ext_name -> { arity, fn }
const builtins = {};

function reg(name, arity, fn) {
  builtins[name] = { arity, fn };
}

// --- Core builtins (interp.ml) ---

reg("mod", 2, (args) => {
  const b = vm.asInt(args[1]);
  if (b === 0) vm.error("modulo by zero");
  return vm.vint(vm.asInt(args[0]) % b);
});

reg("print", 1, (args) => {
  const output = vm.ppValue(args[0]);
  if (typeof globalThis._vmOutput === "function") {
    globalThis._vmOutput(output);
  } else {
    console.log(output);
  }
  return vm.VUNIT;
});

reg("failwith", 1, (args) => {
  vm.error(vm.asString(args[0]));
});

reg("^", 2, (args) =>
  vm.vstring(vm.asString(args[0]) + vm.asString(args[1])));

reg("&&", 2, (args) =>
  vm.vbool(vm.asBool(args[0]) && vm.asBool(args[1])));

reg("||", 2, (args) =>
  vm.vbool(vm.asBool(args[0]) || vm.asBool(args[1])));

reg("not", 1, (args) =>
  vm.vbool(!vm.asBool(args[0])));

reg("phys_equal", 2, (args) =>
  vm.vbool(args[0] === args[1]));

reg("float_of_int", 1, (args) =>
  vm.vfloat(vm.asInt(args[0])));

reg("int_of_float", 1, (args) =>
  vm.vint(Math.trunc(vm.asFloat(args[0]))));

reg("string_of_int", 1, (args) =>
  vm.vstring(String(vm.asInt(args[0]))));

reg("string_of_float", 1, (args) => {
  let f = vm.asFloat(args[0]);
  // Match OCaml's %g format
  let s = formatFloat(f);
  return vm.vstring(s);
});

reg("string_of_bool", 1, (args) =>
  vm.vstring(vm.asBool(args[0]) ? "true" : "false"));

// Map pattern matching helpers
reg("__map_has", 2, (args) => {
  const pairs = vm.asMap(args[0]);
  const key = args[1];
  return vm.vbool(pairs.some(([k]) => vm.valuesEqual(k, key)));
});

reg("__map_get", 2, (args) => {
  const pairs = vm.asMap(args[0]);
  const key = args[1];
  const entry = pairs.find(([k]) => vm.valuesEqual(k, key));
  if (!entry) vm.error("key not found in map");
  return entry[1];
});

// Array operations
reg("array_get", 2, (args) => {
  const arr = vm.asArray(args[0]);
  const idx = vm.asInt(args[1]);
  if (idx < 0 || idx >= arr.length)
    vm.error(`array index ${idx} out of bounds (length ${arr.length})`);
  return arr[idx];
});

reg("array_length", 1, (args) =>
  vm.vint(vm.asArray(args[0]).length));

// Byte primitives
reg("__byte_to_int", 1, (args) =>
  vm.vint(vm.asByte(args[0])));

reg("__byte_of_int", 1, (args) =>
  vm.vbyte(vm.asInt(args[0]) & 0xFF));

reg("__byte_to_string", 1, (args) => {
  const b = vm.asByte(args[0]);
  return vm.vstring(String.fromCharCode(b));
});

// Rune primitives
reg("__rune_to_int", 1, (args) =>
  vm.vint(vm.asRune(args[0])));

reg("__rune_of_int", 1, (args) =>
  vm.vrune(vm.asInt(args[0])));

reg("__rune_to_string", 1, (args) => {
  const cp = vm.asRune(args[0]);
  return vm.vstring(vm.runeToUtf8(cp));
});

// Math primitives
reg("__math_pow", 2, (args) =>
  vm.vfloat(Math.pow(vm.asFloat(args[0]), vm.asFloat(args[1]))));

reg("__math_sqrt", 1, (args) =>
  vm.vfloat(Math.sqrt(vm.asFloat(args[0]))));

reg("__math_floor", 1, (args) =>
  vm.vint(Math.floor(vm.asFloat(args[0]))));

reg("__math_ceil", 1, (args) =>
  vm.vint(Math.ceil(vm.asFloat(args[0]))));

reg("__math_round", 1, (args) =>
  vm.vint(Math.round(vm.asFloat(args[0]))));

reg("__math_sin", 1, (args) =>
  vm.vfloat(Math.sin(vm.asFloat(args[0]))));

reg("__math_cos", 1, (args) =>
  vm.vfloat(Math.cos(vm.asFloat(args[0]))));

reg("__math_abs_float", 1, (args) =>
  vm.vfloat(Math.abs(vm.asFloat(args[0]))));

// --- Format specifier builtins (interp.ml __fmt_*) ---

reg("__fmt_float", 2, (args) => {
  const prec = vm.asInt(args[0]);
  const f = vm.asFloat(args[1]);
  return vm.vstring(f.toFixed(prec));
});

reg("__fmt_hex", 1, (args) =>
  vm.vstring(vm.asInt(args[0]).toString(16)));

reg("__fmt_hex_upper", 1, (args) =>
  vm.vstring(vm.asInt(args[0]).toString(16).toUpperCase()));

reg("__fmt_oct", 1, (args) =>
  vm.vstring(vm.asInt(args[0]).toString(8)));

reg("__fmt_bin", 1, (args) =>
  vm.vstring(vm.asInt(args[0]).toString(2)));

reg("__fmt_zero_pad", 2, (args) => {
  const width = vm.asInt(args[0]);
  const s = vm.asString(args[1]);
  return vm.vstring(s.padStart(width, "0"));
});

reg("__fmt_pad_left", 2, (args) => {
  const width = vm.asInt(args[0]);
  const s = vm.asString(args[1]);
  return vm.vstring(s.padStart(width, " "));
});

reg("__fmt_pad_right", 2, (args) => {
  const width = vm.asInt(args[0]);
  const s = vm.asString(args[1]);
  return vm.vstring(s.padEnd(width, " "));
});

// Copy (continuation)
reg("copy_continuation", 1, (args) => {
  const cont = vm.asContinuation(args[0]);
  return {
    tag: "continuation",
    fiber: vm.copyFiber(cont.fiber),
    returnHandler: cont.returnHandler,
    opHandlers: cont.opHandlers,
    used: false,
  };
});

// Show value (polymorphic)
reg("__show_value", 1, (args) => vm.vstring(vm.ppValue(args[0])));

// --- Index class instances ---

reg("index_at_array", 2, (args) => {
  const idx = vm.asInt(args[0]);
  const arr = vm.asArray(args[1]);
  if (idx < 0 || idx >= arr.length)
    vm.error(`array index out of bounds: ${idx} (length ${arr.length})`);
  return arr[idx];
});

reg("index_at_string", 2, (args) => {
  const idx = vm.asInt(args[0]);
  const s = vm.asString(args[1]);
  if (idx < 0 || idx >= s.length)
    vm.error(`string index out of bounds: ${idx} (length ${s.length})`);
  return vm.vbyte(s.charCodeAt(idx));
});

reg("index_at_map", 2, (args) => {
  const key = args[0];
  const pairs = vm.asMap(args[1]);
  for (const [k, v] of pairs) {
    if (vm.valuesEqual(k, key)) return v;
  }
  vm.error(`key not found: ${vm.ppValue(key)}`);
});

// --- Num class instances ---

reg("num_add_int", 2, (args) =>
  vm.vint(vm.asInt(args[0]) + vm.asInt(args[1])));
reg("num_sub_int", 2, (args) =>
  vm.vint(vm.asInt(args[0]) - vm.asInt(args[1])));
reg("num_mul_int", 2, (args) =>
  vm.vint(vm.asInt(args[0]) * vm.asInt(args[1])));
reg("num_div_int", 2, (args) => {
  const b = vm.asInt(args[1]);
  if (b === 0) vm.error("division by zero");
  return vm.vint(Math.trunc(vm.asInt(args[0]) / b));
});
reg("num_neg_int", 1, (args) =>
  vm.vint(-vm.asInt(args[0])));

reg("num_add_float", 2, (args) =>
  vm.vfloat(vm.asFloat(args[0]) + vm.asFloat(args[1])));
reg("num_sub_float", 2, (args) =>
  vm.vfloat(vm.asFloat(args[0]) - vm.asFloat(args[1])));
reg("num_mul_float", 2, (args) =>
  vm.vfloat(vm.asFloat(args[0]) * vm.asFloat(args[1])));
reg("num_div_float", 2, (args) =>
  vm.vfloat(vm.asFloat(args[0]) / vm.asFloat(args[1])));
reg("num_neg_float", 1, (args) =>
  vm.vfloat(-vm.asFloat(args[0])));

// --- Eq class instances ---

reg("eq_int", 2, (args) =>
  vm.vbool(vm.asInt(args[0]) === vm.asInt(args[1])));
reg("neq_int", 2, (args) =>
  vm.vbool(vm.asInt(args[0]) !== vm.asInt(args[1])));

reg("eq_float", 2, (args) =>
  vm.vbool(vm.asFloat(args[0]) === vm.asFloat(args[1])));
reg("neq_float", 2, (args) =>
  vm.vbool(vm.asFloat(args[0]) !== vm.asFloat(args[1])));

reg("eq_string", 2, (args) =>
  vm.vbool(vm.asString(args[0]) === vm.asString(args[1])));
reg("neq_string", 2, (args) =>
  vm.vbool(vm.asString(args[0]) !== vm.asString(args[1])));

reg("eq_bool", 2, (args) =>
  vm.vbool(vm.asBool(args[0]) === vm.asBool(args[1])));
reg("neq_bool", 2, (args) =>
  vm.vbool(vm.asBool(args[0]) !== vm.asBool(args[1])));

reg("eq_byte", 2, (args) =>
  vm.vbool(vm.asByte(args[0]) === vm.asByte(args[1])));
reg("neq_byte", 2, (args) =>
  vm.vbool(vm.asByte(args[0]) !== vm.asByte(args[1])));

reg("eq_rune", 2, (args) =>
  vm.vbool(vm.asRune(args[0]) === vm.asRune(args[1])));
reg("neq_rune", 2, (args) =>
  vm.vbool(vm.asRune(args[0]) !== vm.asRune(args[1])));

// --- Ord class instances ---

reg("lt_int", 2, (args) =>
  vm.vbool(vm.asInt(args[0]) < vm.asInt(args[1])));
reg("gt_int", 2, (args) =>
  vm.vbool(vm.asInt(args[0]) > vm.asInt(args[1])));
reg("le_int", 2, (args) =>
  vm.vbool(vm.asInt(args[0]) <= vm.asInt(args[1])));
reg("ge_int", 2, (args) =>
  vm.vbool(vm.asInt(args[0]) >= vm.asInt(args[1])));

reg("lt_float", 2, (args) =>
  vm.vbool(vm.asFloat(args[0]) < vm.asFloat(args[1])));
reg("gt_float", 2, (args) =>
  vm.vbool(vm.asFloat(args[0]) > vm.asFloat(args[1])));
reg("le_float", 2, (args) =>
  vm.vbool(vm.asFloat(args[0]) <= vm.asFloat(args[1])));
reg("ge_float", 2, (args) =>
  vm.vbool(vm.asFloat(args[0]) >= vm.asFloat(args[1])));

reg("lt_string", 2, (args) =>
  vm.vbool(vm.asString(args[0]) < vm.asString(args[1])));
reg("gt_string", 2, (args) =>
  vm.vbool(vm.asString(args[0]) > vm.asString(args[1])));
reg("le_string", 2, (args) =>
  vm.vbool(vm.asString(args[0]) <= vm.asString(args[1])));
reg("ge_string", 2, (args) =>
  vm.vbool(vm.asString(args[0]) >= vm.asString(args[1])));

reg("lt_byte", 2, (args) =>
  vm.vbool(vm.asByte(args[0]) < vm.asByte(args[1])));
reg("gt_byte", 2, (args) =>
  vm.vbool(vm.asByte(args[0]) > vm.asByte(args[1])));
reg("le_byte", 2, (args) =>
  vm.vbool(vm.asByte(args[0]) <= vm.asByte(args[1])));
reg("ge_byte", 2, (args) =>
  vm.vbool(vm.asByte(args[0]) >= vm.asByte(args[1])));

reg("lt_rune", 2, (args) =>
  vm.vbool(vm.asRune(args[0]) < vm.asRune(args[1])));
reg("gt_rune", 2, (args) =>
  vm.vbool(vm.asRune(args[0]) > vm.asRune(args[1])));
reg("le_rune", 2, (args) =>
  vm.vbool(vm.asRune(args[0]) <= vm.asRune(args[1])));
reg("ge_rune", 2, (args) =>
  vm.vbool(vm.asRune(args[0]) >= vm.asRune(args[1])));

// --- Bitwise class instances ---

reg("band_int", 2, (args) =>
  vm.vint(vm.asInt(args[0]) & vm.asInt(args[1])));
reg("bor_int", 2, (args) =>
  vm.vint(vm.asInt(args[0]) | vm.asInt(args[1])));
reg("bxor_int", 2, (args) =>
  vm.vint(vm.asInt(args[0]) ^ vm.asInt(args[1])));
reg("bshl_int", 2, (args) =>
  vm.vint(vm.asInt(args[0]) << vm.asInt(args[1])));
reg("bshr_int", 2, (args) =>
  vm.vint(vm.asInt(args[0]) >>> vm.asInt(args[1])));
reg("bnot_int", 1, (args) =>
  vm.vint(~vm.asInt(args[0])));

// --- Show class instances ---

reg("show_int", 1, (args) =>
  vm.vstring(String(vm.asInt(args[0]))));

reg("show_float", 1, (args) =>
  vm.vstring(formatFloat(vm.asFloat(args[0]))));

reg("show_bool", 1, (args) =>
  vm.vstring(vm.asBool(args[0]) ? "true" : "false"));

reg("show_string", 1, (args) => args[0]);

reg("show_unit", 1, () => vm.vstring("()"));

reg("show_byte", 1, (args) =>
  vm.vstring("#" + vm.asByte(args[0]).toString(16).padStart(2, "0")));

reg("show_rune", 1, (args) =>
  vm.vstring(vm.ppValue(args[0])));

reg("show_map", 1, (args) =>
  vm.vstring(vm.ppValue(args[0])));

// --- Map class instances ---

reg("map_of_list", 1, (args) => {
  const lst = vm.asList(args[0]);
  const pairs = lst.map((v) => {
    const t = vm.asTuple(v);
    return [t[0], t[1]];
  });
  return vm.vmap(pairs);
});

reg("map_get", 2, (args) => {
  const key = args[0];
  const pairs = vm.asMap(args[1]);
  const entry = pairs.find(([k]) => vm.valuesEqual(k, key));
  if (entry) return vm.vvariant(1, "Some", entry[1]);
  return vm.vvariant(0, "None", null);
});

reg("map_set", 3, (args) => {
  const k = args[0], v = args[1];
  const pairs = vm.asMap(args[2]);
  const updated = [[k, v], ...pairs.filter(([k2]) => !vm.valuesEqual(k2, k))];
  return vm.vmap(updated);
});

reg("map_has", 2, (args) => {
  const key = args[0];
  const pairs = vm.asMap(args[1]);
  return vm.vbool(pairs.some(([k]) => vm.valuesEqual(k, key)));
});

reg("map_remove", 2, (args) => {
  const key = args[0];
  const pairs = vm.asMap(args[1]);
  return vm.vmap(pairs.filter(([k]) => !vm.valuesEqual(k, key)));
});

reg("map_size", 1, (args) =>
  vm.vint(vm.asMap(args[0]).length));

reg("map_keys", 1, (args) =>
  vm.vlist(vm.asMap(args[0]).map(([k]) => k)));

reg("map_values", 1, (args) =>
  vm.vlist(vm.asMap(args[0]).map(([, v]) => v)));

reg("map_to_list", 1, (args) =>
  vm.vlist(vm.asMap(args[0]).map(([k, v]) => vm.vtuple([k, v]))));

// --- String module (std.ml register_string) ---

reg("String.length", 1, (args) => {
  const bytes = stringToUtf8Bytes(vm.asString(args[0]));
  return vm.vint(bytes.length);
});

reg("String.sub", 3, (args) => {
  const s = vm.asString(args[0]);
  const bytes = stringToUtf8Bytes(s);
  const start = vm.asInt(args[1]);
  const len = vm.asInt(args[2]);
  if (start < 0 || len < 0 || start + len > bytes.length)
    vm.error(`String.sub: index out of bounds`);
  return vm.vstring(utf8BytesToString(bytes.slice(start, start + len)));
});

reg("String.split", 2, (args) => {
  const sep = vm.asString(args[0]);
  const s = vm.asString(args[1]);
  const parts = s.split(sep);
  return vm.vlist(parts.map(vm.vstring));
});

reg("String.trim", 1, (args) =>
  vm.vstring(vm.asString(args[0]).trim()));

reg("String.starts_with", 2, (args) => {
  const prefix = vm.asString(args[0]);
  const s = vm.asString(args[1]);
  return vm.vbool(s.startsWith(prefix));
});

reg("String.contains", 2, (args) => {
  const sub = vm.asString(args[0]);
  const s = vm.asString(args[1]);
  return vm.vbool(s.includes(sub));
});

reg("String.replace", 3, (args) => {
  const old = vm.asString(args[0]);
  const rep = vm.asString(args[1]);
  const s = vm.asString(args[2]);
  return vm.vstring(s.split(old).join(rep));
});

reg("String.to_int", 1, (args) => {
  const s = vm.asString(args[0]);
  const n = parseInt(s, 10);
  if (isNaN(n) || String(n) !== s)
    return vm.vvariant(0, "None", null);
  return vm.vvariant(1, "Some", vm.vint(n));
});

reg("String.to_float", 1, (args) => {
  const s = vm.asString(args[0]);
  const f = parseFloat(s);
  if (isNaN(f))
    return vm.vvariant(0, "None", null);
  return vm.vvariant(1, "Some", vm.vfloat(f));
});

reg("String.uppercase", 1, (args) =>
  vm.vstring(vm.asString(args[0]).toUpperCase()));

reg("String.lowercase", 1, (args) =>
  vm.vstring(vm.asString(args[0]).toLowerCase()));

reg("String.get", 2, (args) => {
  const s = vm.asString(args[0]);
  const idx = vm.asInt(args[1]);
  const bytes = stringToUtf8Bytes(s);
  if (idx < 0 || idx >= bytes.length)
    vm.error(`String.get: index ${idx} out of bounds (length ${bytes.length})`);
  return vm.vbyte(bytes[idx]);
});

reg("String.to_bytes", 1, (args) => {
  const bytes = stringToUtf8Bytes(vm.asString(args[0]));
  return vm.vlist(Array.from(bytes).map(vm.vbyte));
});

reg("String.of_bytes", 1, (args) => {
  const lst = vm.asList(args[0]);
  const bytes = new Uint8Array(lst.map(vm.asByte));
  return vm.vstring(utf8BytesToString(bytes));
});

reg("String.to_byte_array", 1, (args) => {
  const bytes = stringToUtf8Bytes(vm.asString(args[0]));
  return vm.varray(Array.from(bytes).map(vm.vbyte));
});

reg("String.of_byte_array", 1, (args) => {
  const arr = vm.asArray(args[0]);
  const bytes = new Uint8Array(arr.map(vm.asByte));
  return vm.vstring(utf8BytesToString(bytes));
});

reg("String.to_runes", 1, (args) => {
  const s = vm.asString(args[0]);
  const runes = [];
  for (const ch of s) {
    runes.push(vm.vrune(ch.codePointAt(0)));
  }
  return vm.vlist(runes);
});

reg("String.of_runes", 1, (args) => {
  const lst = vm.asList(args[0]);
  return vm.vstring(lst.map((r) => vm.runeToUtf8(vm.asRune(r))).join(""));
});

reg("String.get_rune", 2, (args) => {
  const s = vm.asString(args[0]);
  const idx = vm.asInt(args[1]);
  const chars = [...s]; // iterate by codepoints
  if (idx < 0 || idx >= chars.length)
    vm.error(`String.get_rune: index ${idx} out of bounds (length ${chars.length})`);
  return vm.vrune(chars[idx].codePointAt(0));
});

reg("String.of_byte", 1, (args) => {
  const b = vm.asByte(args[0]);
  return vm.vstring(String.fromCharCode(b));
});

reg("String.rune_length", 1, (args) => {
  const s = vm.asString(args[0]);
  return vm.vint([...s].length);
});

reg("String.make", 2, (args) => {
  const n = vm.asInt(args[0]);
  const b = vm.asByte(args[1]);
  return vm.vstring(String.fromCharCode(b).repeat(n));
});

reg("String.index_opt", 2, (args) => {
  const s = vm.asString(args[0]);
  const b = vm.asByte(args[1]);
  const bytes = stringToUtf8Bytes(s);
  const idx = bytes.indexOf(b);
  return idx >= 0 ? vm.vvariant(1, "Some", vm.vint(idx)) : vm.vvariant(0, "None", null);
});

reg("String.rindex_opt", 2, (args) => {
  const s = vm.asString(args[0]);
  const b = vm.asByte(args[1]);
  const bytes = stringToUtf8Bytes(s);
  const idx = bytes.lastIndexOf(b);
  return idx >= 0 ? vm.vvariant(1, "Some", vm.vint(idx)) : vm.vvariant(0, "None", null);
});

reg("String.concat", 2, (args) => {
  const sep = vm.asString(args[0]);
  const lst = vm.asList(args[1]);
  const strs = lst.map(v => vm.asString(v));
  return vm.vstring(strs.join(sep));
});

reg("String.compare", 2, (args) => {
  const a = vm.asString(args[0]);
  const b = vm.asString(args[1]);
  return vm.vint(a < b ? -1 : a > b ? 1 : 0);
});

// --- Array module (std.ml register_array) ---

reg("Array.make", 2, (args) => {
  const n = vm.asInt(args[0]);
  const v = args[1];
  return vm.varray(new Array(n).fill(v));
});

reg("Array.get", 2, (args) => {
  const arr = vm.asArray(args[0]);
  const idx = vm.asInt(args[1]);
  if (idx < 0 || idx >= arr.length)
    vm.error(`array index ${idx} out of bounds (length ${arr.length})`);
  return arr[idx];
});

reg("Array.set", 3, (args) => {
  const arr = vm.asArray(args[0]);
  const idx = vm.asInt(args[1]);
  const v = args[2];
  if (idx < 0 || idx >= arr.length)
    vm.error(`array index ${idx} out of bounds (length ${arr.length})`);
  arr[idx] = v;
  return vm.VUNIT;
});

reg("Array.length", 1, (args) =>
  vm.vint(vm.asArray(args[0]).length));

reg("Array.to_list", 1, (args) =>
  vm.vlist(Array.from(vm.asArray(args[0]))));

reg("Array.of_list", 1, (args) =>
  vm.varray(vm.asList(args[0]).slice()));

reg("Array.copy", 1, (args) =>
  vm.varray(vm.asArray(args[0]).slice()));

reg("Array.sub", 3, (args) => {
  const arr = vm.asArray(args[0]);
  const start = vm.asInt(args[1]);
  const len = vm.asInt(args[2]);
  return vm.varray(arr.slice(start, start + len));
});

// --- Canvas builtins (browser only, safe to register in Node) ---

reg("Canvas.init", 2, (args) => {
  const w = vm.asInt(args[0]);
  const h = vm.asInt(args[1]);
  if (typeof globalThis._canvasInit === "function") {
    globalThis._canvasInit(w, h);
  }
  return vm.VUNIT;
});

reg("Canvas.clear", 1, (args) => {
  const color = vm.asString(args[0]);
  const ctx = globalThis._canvasCtx;
  if (ctx) {
    ctx.fillStyle = color;
    ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
  }
  return vm.VUNIT;
});

reg("Canvas.fill_rect", 5, (args) => {
  const ctx = globalThis._canvasCtx;
  if (ctx) {
    ctx.fillStyle = vm.asString(args[4]);
    ctx.fillRect(vm.asFloat(args[0]), vm.asFloat(args[1]),
                 vm.asFloat(args[2]), vm.asFloat(args[3]));
  }
  return vm.VUNIT;
});

reg("Canvas.stroke_rect", 5, (args) => {
  const ctx = globalThis._canvasCtx;
  if (ctx) {
    ctx.strokeStyle = vm.asString(args[4]);
    ctx.lineWidth = 1;
    ctx.strokeRect(vm.asFloat(args[0]), vm.asFloat(args[1]),
                   vm.asFloat(args[2]), vm.asFloat(args[3]));
  }
  return vm.VUNIT;
});

reg("Canvas.fill_circle", 4, (args) => {
  const ctx = globalThis._canvasCtx;
  if (ctx) {
    ctx.fillStyle = vm.asString(args[3]);
    ctx.beginPath();
    ctx.arc(vm.asFloat(args[0]), vm.asFloat(args[1]),
            vm.asFloat(args[2]), 0, 2 * Math.PI);
    ctx.fill();
  }
  return vm.VUNIT;
});

reg("Canvas.draw_text", 4, (args) => {
  const ctx = globalThis._canvasCtx;
  if (ctx) {
    ctx.fillStyle = vm.asString(args[3]);
    ctx.textBaseline = "top";
    ctx.fillText(vm.asString(args[0]), vm.asFloat(args[1]), vm.asFloat(args[2]));
  }
  return vm.VUNIT;
});

reg("Canvas.set_font", 1, (args) => {
  const ctx = globalThis._canvasCtx;
  if (ctx) {
    ctx.font = vm.asString(args[0]);
  }
  return vm.VUNIT;
});

reg("Canvas.mouse_x", 1, (_args) =>
  vm.vfloat(globalThis._canvasMouseX || 0));

reg("Canvas.mouse_y", 1, (_args) =>
  vm.vfloat(globalThis._canvasMouseY || 0));

reg("Canvas.mouse_down", 1, (_args) =>
  vm.vbool(!!globalThis._canvasMouseDown));

reg("Canvas.mouse_clicked", 1, (_args) =>
  vm.vbool(!!globalThis._canvasMouseClicked));

reg("Canvas.key_down", 1, (args) =>
  vm.vbool(!!globalThis._canvasKeysDown[vm.asString(args[0])]));

reg("Canvas.key_pressed", 1, (args) =>
  vm.vbool(!!globalThis._canvasKeysPressed[vm.asString(args[0])]));

reg("Canvas.start_app", 2, (args) => {
  globalThis._canvasApp = {
    initFn: vm.asClosure(args[0]),
    frameFn: vm.asClosure(args[1]),
  };
  return vm.VUNIT;
});

// --- IO module ---

reg("IO.read_file", 1, (args) => {
  if (typeof globalThis._vmReadFile === "function") {
    return vm.vstring(globalThis._vmReadFile(vm.asString(args[0])));
  }
  const fs = require("fs");
  return vm.vstring(fs.readFileSync(vm.asString(args[0]), "utf-8"));
});

reg("IO.write_file", 2, (args) => {
  const fs = require("fs");
  fs.writeFileSync(vm.asString(args[0]), vm.asString(args[1]));
  return vm.VUNIT;
});

reg("IO.append_file", 2, (args) => {
  const fs = require("fs");
  fs.appendFileSync(vm.asString(args[0]), vm.asString(args[1]));
  return vm.VUNIT;
});

reg("IO.read_line", 1, () => {
  // Not easily supported in sync Node.js
  vm.error("IO.read_line not supported in JS VM");
});

reg("IO.file_exists", 1, (args) => {
  const fs = require("fs");
  return vm.vbool(fs.existsSync(vm.asString(args[0])));
});

// --- Sys module ---

reg("Sys.args", 1, () => {
  if (globalThis._vmArgs) {
    return vm.vlist(globalThis._vmArgs.map(vm.vstring));
  }
  return vm.vlist(process.argv.slice(2).map(vm.vstring));
});

reg("Sys.getenv", 1, (args) => {
  const name = vm.asString(args[0]);
  const val = process.env[name];
  if (val === undefined) return vm.vvariant(0, "None", null);
  return vm.vvariant(1, "Some", vm.vstring(val));
});

reg("Sys.exit", 1, (args) => {
  process.exit(vm.asInt(args[0]));
});

reg("Sys.time", 1, () =>
  vm.vfloat(Date.now() / 1000));

// --- Runtime.eval/eval_file (stubs for JS) ---

reg("Runtime.eval", 1, () => {
  vm.error("Runtime.eval not supported in JS VM");
});

reg("Runtime.eval_file", 1, () => {
  vm.error("Runtime.eval_file not supported in JS VM");
});

// --- Helper: OCaml %g-like float formatting ---
function formatFloat(f) {
  if (!isFinite(f)) {
    if (f !== f) return "nan";
    return f > 0 ? "inf" : "-inf";
  }
  // %g uses shortest of %e and %f with 6 significant digits, trims trailing zeros
  let s = f.toPrecision(6);
  // Remove trailing zeros after decimal point
  if (s.includes('.')) {
    s = s.replace(/0+$/, '');
    s = s.replace(/\.$/, '');
  }
  // Handle exponential notation
  if (s.includes('e+')) s = s.replace('e+0', 'e+').replace('e+', 'e+');
  if (s.includes('e-')) s = s.replace('e-0', 'e-');
  // Match OCaml's %g: -0 -> -0
  if (Object.is(f, -0)) return "-0";
  return s;
}

module.exports = { builtins, formatFloat };
