"use strict";
function _call(f, args) {
  while (args.length > 0) {
    const a = f._arity !== undefined ? f._arity : f.length;
    if (a === 0) { f = f(); continue; }
    if (args.length < a) return _partial(f, a, args);
    const taken = args.splice(0, a);
    f = f.apply(null, taken);
  }
  return f;
}
function _partial(fn, arity, args) {
  const p = function() {
    return _call(fn, args.concat(Array.from(arguments)));
  };
  p._arity = arity - args.length;
  return p;
}
function _eq(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return a === b;
  if (typeof a !== "object") return false;
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!_eq(a[i], b[i])) return false;
    return true;
  }
  if ("_arr" in a && "_arr" in b) {
    if (a._arr.length !== b._arr.length) return false;
    for (let i = 0; i < a._arr.length; i++) if (!_eq(a._arr[i], b._arr[i])) return false;
    return true;
  }
  if ("_hd" in a && "_hd" in b) {
    let ca = a, cb = b;
    while (ca !== null && cb !== null) {
      if (!("_hd" in ca) || !("_hd" in cb)) return ca === cb;
      if (!_eq(ca._hd, cb._hd)) return false;
      ca = ca._tl; cb = cb._tl;
    }
    return ca === cb;
  }
  if ("_tag" in a) return a._tag === b._tag && _eq(a._val, b._val);
  if ("_ref" in a) return _eq(a._ref, b._ref);
  const ka = Object.keys(a), kb = Object.keys(b);
  if (ka.length !== kb.length) return false;
  for (const k of ka) if (!_eq(a[k], b[k])) return false;
  return true;
}
function _compare(a, b) {
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}
function _match_fail(loc) { throw new Error("Match failure at " + loc); }
function _pp(v) {
  if (v === undefined) return "()";
  if (v === null) return "[]";
  if (typeof v === "number") {
    if (!Number.isInteger(v)) {
      const s = String(v);
      return s.includes(".") || s.includes("e") ? s : s + ".";
    }
    return String(v);
  }
  if (typeof v === "boolean") return String(v);
  if (typeof v === "string") return v;
  if (Array.isArray(v)) return "(" + v.map(_pp).join(", ") + ")";
  if (v !== null && typeof v === "object") {
    if ("_arr" in v) return "#[" + v._arr.map(_pp).join("; ") + "]";
    if ("_hd" in v) {
      const r = [];
      let c = v;
      while (c !== null && typeof c === "object" && "_hd" in c) { r.push(_pp(c._hd)); c = c._tl; }
      return "[" + r.join("; ") + "]";
    }
    if ("_tag" in v) {
      if (v._val !== undefined) {
        const pv = _pp(v._val);
        if (Array.isArray(v._val) || (typeof v._val === "object" && v._val !== null && "_tag" in v._val && v._val._val !== undefined))
          return v._name + " (" + pv + ")";
        return v._name + " " + pv;
      }
      return v._name;
    }
    if ("_ref" in v) return "ref(" + _pp(v._ref) + ")";
    if (v instanceof Map) {
      const entries = Array.from(v.entries());
      const isSet = entries.every(([_,val]) => val === undefined);
      if (isSet) return "#{" + entries.map(([k]) => _pp(k)).join("; ") + "}";
      return "#{" + entries.map(([k,val]) => _pp(k) + ": " + _pp(val)).join("; ") + "}";
    }
    return "{ " + Object.entries(v).map(([k,val]) => k + " = " + _pp(val)).join("; ") + " }";
  }
  return String(v);
}
let _h = {};
function _bounce(fn) { fn._tramp = true; return fn; }
const _trampolining = new Set();
function _trampoline(fn, tag) {
  if (tag !== undefined && _trampolining.has(tag)) return fn();
  if (tag !== undefined) _trampolining.add(tag);
  try {
    let result = fn();
    while (typeof result === 'function' && result._tramp) {
      result = result();
    }
    return result;
  } finally { if (tag !== undefined) _trampolining.delete(tag); }
}
// --- Builtins ---
function print(v) {
  const s = (typeof v === "string") ? v : _pp(v);
  if (typeof globalThis._jsOutput === "function") globalThis._jsOutput(s);
  else if (typeof process !== "undefined") process.stdout.write(s);
  return undefined;
}
function println(v) {
  const s = (typeof v === "string") ? v : _pp(v);
  if (typeof globalThis._jsOutput === "function") globalThis._jsOutput(s + "\n");
  else if (typeof process !== "undefined") process.stdout.write(s + "\n");
  return undefined;
}
function string_of_int(n) { return String(n); }
function int_of_string(s) { const n = parseInt(s, 10); if (isNaN(n)) throw new Error("int_of_string: " + s); return n; }
function float_of_int(n) { return n; }
function int_of_float(f) { return Math.trunc(f); }
function float_of_string(s) { const f = parseFloat(s); if (isNaN(f)) throw new Error("float_of_string: " + s); return f; }
function string_of_float(f) {
  const s = String(f);
  return s.includes(".") || s.includes("e") ? s : s + ".";
}
function string_of_bool(b) { return String(b); }
function failwith(msg) { throw new Error(msg); }
const not = (b) => !b;
function $caret(a, b) { return a + b; }
function $mod(a, b) { return a % b; }
function __show_value(v) { return _pp(v); }
function copy_continuation(k) { return k; }
const ignore = (_) => undefined;
function fst(t) { return t[0]; }
function snd(t) { return t[1]; }
function string_length(s) { return s.length; }
function string_sub(s, start, len) { return s.substring(start, start + len); }
function string_get(s, i) { return s.charCodeAt(i); }
function string_contains(s, sub) { return s.includes(sub); }
function string_concat(sep, parts) {
  const arr = [];
  let c = parts;
  while (c !== null) { arr.push(c._hd); c = c._tl; }
  return arr.join(sep);
}
function array_length(a) { return a._arr.length; }
function array_get(a, i) { return a._arr[i]; }
function array_set(a, i, v) { a._arr[i] = v; return undefined; }
function array_make(n, v) { return {_arr: new Array(n).fill(v)}; }
function array_of_list(lst) {
  const arr = [];
  let c = lst;
  while (c !== null) { arr.push(c._hd); c = c._tl; }
  return {_arr: arr};
}
function array_to_list(arr) {
  const a = arr._arr;
  let result = null;
  for (let i = a.length - 1; i >= 0; i--) result = {_hd: a[i], _tl: result};
  return result;
}
function array_copy(a) { return {_arr: a._arr.slice()}; }
function array_sub(a, start, len) { return {_arr: a._arr.slice(start, start + len)}; }
function __math_pow(a, b) { return Math.pow(a, b); }
function __math_sqrt(x) { return Math.sqrt(x); }
function __math_floor(x) { return Math.floor(x); }
function __math_ceil(x) { return Math.ceil(x); }
function __math_round(x) { return Math.round(x); }
function __math_abs(x) { return Math.abs(x); }
function __math_sin(x) { return Math.sin(x); }
function __math_cos(x) { return Math.cos(x); }
function __math_abs_float(x) { return Math.abs(x); }
function __byte_to_char(b) { return String.fromCharCode(b); }
function __byte_to_int(b) { return b; }
function __byte_of_int(n) { return n & 0xFF; }
function __byte_to_string(b) { return String.fromCharCode(b); }
function __char_to_byte(s) { return s.charCodeAt(0); }
function __rune_to_string(cp) {
  return String.fromCodePoint(cp);
}
function __string_to_runes(s) {
  const cps = Array.from(s).map(c => c.codePointAt(0));
  let result = null;
  for (let i = cps.length - 1; i >= 0; i--) result = {_hd: cps[i], _tl: result};
  return result;
}
function __map_has(m, k) { return m.has(k); }
function __map_get(m, k) { return m.get(k); }
function __rune_to_int(r) { return r; }
function __rune_of_int(n) { return n; }
function __int_to_hex(n) { return n.toString(16); }
function __int_to_oct(n) { return n.toString(8); }
function __int_to_bin(n) { return n.toString(2); }
function __fmt_float(prec, f) { return f.toFixed(prec); }
function __fmt_hex(n) { return n.toString(16); }
function __fmt_hex_upper(n) { return n.toString(16).toUpperCase(); }
function __fmt_oct(n) { return n.toString(8); }
function __fmt_bin(n) { return n.toString(2); }
function __fmt_zero_pad(width, s) { return s.padStart(width, "0"); }
function __fmt_pad_left(width, s) { return s.padStart(width, " "); }
function __fmt_pad_right(width, s) { return s.padEnd(width, " "); }
function __sys_time() { return Date.now() / 1000.0; }
function __sys_exit(code) { if (typeof process !== "undefined") process.exit(code); throw new Error("exit: " + code); }
// --- Module extern stubs ---
// String module
function String$length(s) { return s.length; }
function String$sub(s, start, len) {
  if (start < 0 || len < 0 || start + len > s.length)
    throw new Error("String.sub: index out of bounds");
  return s.substring(start, start + len);
}
function String$split(delim, input) {
  if (delim.length === 0) {
    let r = null;
    for (let i = input.length - 1; i >= 0; i--) r = {_hd: input[i], _tl: r};
    return r;
  }
  const parts = input.split(delim);
  let r = null;
  for (let i = parts.length - 1; i >= 0; i--) r = {_hd: parts[i], _tl: r};
  return r;
}
function String$trim(s) { return s.trim(); }
function String$starts_with(prefix, s) { return s.startsWith(prefix); }
function String$contains(sub, s) { return s.includes(sub); }
function String$replace(old_s, new_s, input) {
  if (old_s.length === 0) return input;
  return input.split(old_s).join(new_s);
}
function String$to_int(s) { const n = parseInt(s,10); return isNaN(n) ? {_tag:0,_name:"None"} : {_tag:1,_name:"Some",_val:n}; }
function String$to_float(s) { const f = parseFloat(s); return isNaN(f) ? {_tag:0,_name:"None"} : {_tag:1,_name:"Some",_val:f}; }
function String$uppercase(s) { return s.toUpperCase(); }
function String$lowercase(s) { return s.toLowerCase(); }
function String$get(s, i) {
  if (i < 0 || i >= s.length) throw new Error("String.get: index " + i + " out of bounds (length " + s.length + ")");
  return s.charCodeAt(i);
}
function String$to_bytes(s) {
  let r = null;
  for (let i = s.length - 1; i >= 0; i--) r = {_hd: s.charCodeAt(i), _tl: r};
  return r;
}
function String$of_bytes(lst) {
  let r = "";
  let c = lst;
  while (c !== null) { r += String.fromCharCode(c._hd); c = c._tl; }
  return r;
}
function String$to_byte_array(s) {
  const a = new Array(s.length);
  for (let i = 0; i < s.length; i++) a[i] = s.charCodeAt(i);
  return {_arr: a};
}
function String$of_byte_array(a) {
  let r = "";
  for (let i = 0; i < a._arr.length; i++) r += String.fromCharCode(a._arr[i]);
  return r;
}
function String$to_runes(s) { return __string_to_runes(s); }
function String$of_runes(lst) {
  let r = "";
  let c = lst;
  while (c !== null) { r += String.fromCodePoint(c._hd); c = c._tl; }
  return r;
}
function String$get_rune(s, n) {
  const cps = Array.from(s);
  if (n < 0 || n >= cps.length) throw new Error("String.get_rune: index " + n + " out of bounds");
  return cps[n].codePointAt(0);
}
function String$of_byte(b) { return String.fromCharCode(b); }
function String$rune_length(s) { return Array.from(s).length; }
function String$make(n, b) {
  if (n < 0) throw new Error("String.make: negative length");
  return String.fromCharCode(b).repeat(n);
}
function String$index_opt(s, b) { const i=s.indexOf(String.fromCharCode(b)); return i<0?{_tag:0,_name:"None"}:{_tag:1,_name:"Some",_val:i}; }
function String$rindex_opt(s, b) { const i=s.lastIndexOf(String.fromCharCode(b)); return i<0?{_tag:0,_name:"None"}:{_tag:1,_name:"Some",_val:i}; }
function String$concat(sep, lst) { return string_concat(sep, lst); }
function String$compare(a, b) { return a < b ? -1 : a > b ? 1 : 0; }
// Array module
function Array$make(n, v) { return array_make(n, v); }
function Array$get(a, i) { return array_get(a, i); }
function Array$set(a, i, v) { return array_set(a, i, v); }
function Array$length(a) { return array_length(a); }
function Array$to_list(a) { return array_to_list(a); }
function Array$of_list(l) { return array_of_list(l); }
function Array$copy(a) { return array_copy(a); }
function Array$sub(a, s, l) { return array_sub(a, s, l); }
// IO module
function IO$read_file(path) {
  if (typeof globalThis._jsReadFile === "function") return globalThis._jsReadFile(path);
  if (typeof require !== "undefined") return require("fs").readFileSync(path,"utf8");
  throw new Error("IO.read_file: not available in this environment");
}
function IO$write_file(path, data) {
  if (typeof require !== "undefined") { require("fs").writeFileSync(path,data); return undefined; }
  throw new Error("IO.write_file: not available in this environment");
}
function IO$append_file(path, data) {
  if (typeof require !== "undefined") { require("fs").appendFileSync(path,data); return undefined; }
  throw new Error("IO.append_file: not available in this environment");
}
function IO$read_line(u) {
  if (typeof require !== "undefined") {
    const buf = Buffer.alloc(1);
    let line = "";
    const fd = require("fs").openSync("/dev/stdin", "rs");
    while (true) {
      const n = require("fs").readSync(fd, buf, 0, 1);
      if (n === 0 || buf[0] === 10) break;
      line += String.fromCharCode(buf[0]);
    }
    return line;
  }
  return "";
}
function IO$file_exists(path) {
  if (typeof require !== "undefined") return require("fs").existsSync(path);
  return false;
}
// Sys module
function Sys$args(u) {
  if (globalThis._jsSysArgs) {
    const a = globalThis._jsSysArgs;
    let r = null;
    for (let i = a.length - 1; i >= 0; i--) r = {_hd: a[i], _tl: r};
    return r;
  }
  if (typeof process !== "undefined") {
    const a = process.argv.slice(1);
    let r = null;
    for (let i = a.length - 1; i >= 0; i--) r = {_hd: a[i], _tl: r};
    return r;
  }
  return null;
}
function Sys$getenv(name) {
  if (typeof process !== "undefined") {
    const v = process.env[name];
    return v === undefined ? {_tag:0,_name:"None"} : {_tag:1,_name:"Some",_val:v};
  }
  return {_tag:0,_name:"None"};
}
function Sys$exit(code) { __sys_exit(code); }
function Sys$time(u) { return __sys_time(); }
// Runtime module (stubs — eval not supported in compiled JS)
function Runtime$eval(s) { throw new Error("Runtime.eval: not supported in compiled JS"); }
function Runtime$eval_file(s) { throw new Error("Runtime.eval_file: not supported in compiled JS"); }
// --- Typeclass Dictionaries ---
const __dict_Num_int = { $star: (a, b) => a * b, $plus: (a, b) => a + b, $minus: (a, b) => a - b, $slash: (a, b) => (a / b) | 0, neg: (a) => -a };
const __dict_Num_float = { $star: (a, b) => a * b, $plus: (a, b) => a + b, $minus: (a, b) => a - b, $slash: (a, b) => a / b, neg: (a) => -a };
const __dict_Eq_int = { $lt$gt: (a, b) => a !== b, $eq: (a, b) => a === b };
const __dict_Eq_float = { $lt$gt: (a, b) => a !== b, $eq: (a, b) => a === b };
const __dict_Eq_string = { $lt$gt: (a, b) => a !== b, $eq: (a, b) => a === b };
const __dict_Eq_bool = { $lt$gt: (a, b) => a !== b, $eq: (a, b) => a === b };
const __dict_Eq_byte = { $lt$gt: (a, b) => a !== b, $eq: (a, b) => a === b };
const __dict_Eq_rune = { $lt$gt: (a, b) => a !== b, $eq: (a, b) => a === b };
const __dict_Ord_int = { $lt: (a, b) => a < b, $lt$eq: (a, b) => a <= b, $gt: (a, b) => a > b, $gt$eq: (a, b) => a >= b };
const __dict_Ord_float = { $lt: (a, b) => a < b, $lt$eq: (a, b) => a <= b, $gt: (a, b) => a > b, $gt$eq: (a, b) => a >= b };
const __dict_Ord_string = { $lt: (a, b) => a < b, $lt$eq: (a, b) => a <= b, $gt: (a, b) => a > b, $gt$eq: (a, b) => a >= b };
const __dict_Ord_byte = { $lt: (a, b) => a < b, $lt$eq: (a, b) => a <= b, $gt: (a, b) => a > b, $gt$eq: (a, b) => a >= b };
const __dict_Ord_rune = { $lt: (a, b) => a < b, $lt$eq: (a, b) => a <= b, $gt: (a, b) => a > b, $gt$eq: (a, b) => a >= b };
const __dict_Bitwise_int = { land: (a, b) => a & b, lnot: (a) => ~a, lor: (a, b) => a | b, lsl: (a, b) => a << b, lsr: (a, b) => a >> b, lxor: (a, b) => a ^ b };
const __dict_Show_int = { show: (a) => String(a) };
const __dict_Show_float = { show: string_of_float };
const __dict_Show_bool = { show: (a) => String(a) };
const __dict_Show_string = { show: (a) => a };
const __dict_Show_unit = { show: (_) => "()" };
const __dict_Show_byte = { show: (a) => "#" + a.toString(16).padStart(2, "0") };
const __dict_Show_rune = { show: (a) => String.fromCodePoint(a) };
// --- Canvas builtins (browser only) ---
function Canvas$init(w, h) {
  if (typeof globalThis._canvasInit === "function") globalThis._canvasInit(w, h);
  return undefined;
}
function Canvas$clear(color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.fillStyle = color; ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height); }
  return undefined;
}
function Canvas$fill_rect(x, y, w, h, color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.fillStyle = color; ctx.fillRect(x, y, w, h); }
  return undefined;
}
function Canvas$stroke_rect(x, y, w, h, color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.strokeStyle = color; ctx.lineWidth = 1; ctx.strokeRect(x, y, w, h); }
  return undefined;
}
function Canvas$fill_circle(x, y, r, color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.fillStyle = color; ctx.beginPath(); ctx.arc(x, y, r, 0, 2*Math.PI); ctx.fill(); }
  return undefined;
}
function Canvas$draw_text(text, x, y, color) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.fillStyle = color; ctx.textBaseline = "top"; ctx.fillText(text, x, y); }
  return undefined;
}
function Canvas$set_font(font) {
  const ctx = globalThis._canvasCtx;
  if (ctx) { ctx.font = font; }
  return undefined;
}
function Canvas$mouse_x(_) { return globalThis._canvasMouseX || 0; }
function Canvas$mouse_y(_) { return globalThis._canvasMouseY || 0; }
function Canvas$mouse_down(_) { return !!globalThis._canvasMouseDown; }
function Canvas$mouse_clicked(_) { return !!globalThis._canvasMouseClicked; }
function Canvas$key_down(key) { return !!(globalThis._canvasKeysDown && globalThis._canvasKeysDown[key]); }
function Canvas$key_pressed(key) { return !!(globalThis._canvasKeysPressed && globalThis._canvasKeysPressed[key]); }
function Canvas$start_app(init_fn, frame_fn) {
  globalThis._canvasApp = { initFn: init_fn, frameFn: frame_fn, jsMode: true, call: _call };
  return undefined;
}
// --- Stdlib ---
function _t0(f, acc, xs) {
  function go(a, l) {
    while (true) {
      let _t2;
      const _t1 = l;
      _t3: {
        if (_t1 === null) {
          _t2 = a;
          break _t3;
        }
        if (_t1 !== null) {
          const x = _t1._hd;
          const rest = _t1._tl;
          const _t4 = f(a, x);
          const _t5 = rest;
          a = _t4;
          l = _t5;
          continue;
          _t2 = undefined;
          break _t3;
        }
        _match_fail("line 14");
      }
      return _t2;
    }
  }
  const _t6 = go(acc, xs);
  return _t6;
}
const __dict_Iter_g0_list__g0 = ({fold: _t0});
function _t7(f, acc, arr) {
  function go(a, i) {
    while (true) {
      let _t8;
      if ((i === array_length(arr))) {
        _t8 = a;
      } else {
        const _t9 = f(a, array_get(arr, i));
        const _t10 = (i + 1);
        a = _t9;
        i = _t10;
        continue;
        _t8 = undefined;
      }
      return _t8;
    }
  }
  const _t11 = go(acc, 0);
  return _t11;
}
const __dict_Iter_g0_array__g0 = ({fold: _t7});
function __dict_Show_g0_list(__dict_Show_0) {
  function _t12(xs) {
    let _t14;
    const _t13 = xs;
    _t15: {
      if (_t13 === null) {
        _t14 = "[]";
        break _t15;
      }
      if (true) {
        function _t16(acc, x) {
          let _t17;
          if ((acc === "")) {
            _t17 = __dict_Show_0.show(x);
          } else {
            _t17 = ((acc + "; ") + __dict_Show_0.show(x));
          }
          return _t17;
        }
        _t14 = (("[" + __dict_Iter_g0_list__g0.fold(_t16, "", xs)) + "]");
        break _t15;
      }
      _match_fail("line 32");
    }
    return _t14;
  }
  return ({show: _t12});
}
function __dict_Show_g0_array(__dict_Show_0) {
  function _t18(arr) {
    let _t19;
    if ((array_length(arr) === 0)) {
      _t19 = "#[]";
    } else {
      function _t20(acc, x) {
        let _t21;
        if ((acc === "")) {
          _t21 = __dict_Show_0.show(x);
        } else {
          _t21 = ((acc + "; ") + __dict_Show_0.show(x));
        }
        return _t21;
      }
      _t19 = (("#[" + __dict_Iter_g0_array__g0.fold(_t20, "", arr)) + "]");
    }
    return _t19;
  }
  return ({show: _t18});
}
function __dict_Show_g0_option(__dict_Show_0) {
  function _t22(opt) {
    let _t24;
    const _t23 = opt;
    _t25: {
      if (_t23._tag === 0) {
        _t24 = "None";
        break _t25;
      }
      if (_t23._tag === 1) {
        const x = _t23._val;
        _t24 = ("Some " + __dict_Show_0.show(x));
        break _t25;
      }
      _match_fail("line 32");
    }
    return _t24;
  }
  return ({show: _t22});
}
function __dict_Show_g0_g1_tup(__dict_Show_0, __dict_Show_1) {
  function _t26(p) {
    let _t28;
    const _t27 = p;
    _t29: {
      if (true) {
        const a = _t27[0];
        const b = _t27[1];
        _t28 = (((("(" + __dict_Show_0.show(a)) + ", ") + __dict_Show_1.show(b)) + ")");
        break _t29;
      }
      _match_fail("line 32");
    }
    return _t28;
  }
  return ({show: _t26});
}
function __dict_Show_g0_g1_g2_tup(__dict_Show_0, __dict_Show_1, __dict_Show_2) {
  function _t30(p) {
    let _t32;
    const _t31 = p;
    _t33: {
      if (true) {
        const a = _t31[0];
        const b = _t31[1];
        const c = _t31[2];
        _t32 = (((((("(" + __dict_Show_0.show(a)) + ", ") + __dict_Show_1.show(b)) + ", ") + __dict_Show_2.show(c)) + ")");
        break _t33;
      }
      _match_fail("line 32");
    }
    return _t32;
  }
  return ({show: _t30});
}
function _t34(f, acc, m) {
  function go(a, l) {
    while (true) {
      let _t36;
      const _t35 = l;
      _t37: {
        if (_t35 === null) {
          _t36 = a;
          break _t37;
        }
        if (_t35 !== null) {
          const x = _t35._hd;
          const rest = _t35._tl;
          const _t38 = f(a, x);
          const _t39 = rest;
          a = _t38;
          l = _t39;
          continue;
          _t36 = undefined;
          break _t37;
        }
        _match_fail("line 6");
      }
      return _t36;
    }
  }
  const _t40 = go(acc, __dict_Map_g0_g1_map__g0__g1.to_list(m));
  return _t40;
}
const __dict_Iter_g1_g0_map__g1_g0_tup = ({fold: _t34});
if (typeof String$length === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.length"]) var String$length = globalThis._mmlExterns["String.length"]; else throw new Error("extern " + "String.length" + " not provided"); }
if (typeof String$sub === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.sub"]) var String$sub = globalThis._mmlExterns["String.sub"]; else throw new Error("extern " + "String.sub" + " not provided"); }
if (typeof String$split === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.split"]) var String$split = globalThis._mmlExterns["String.split"]; else throw new Error("extern " + "String.split" + " not provided"); }
if (typeof String$trim === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.trim"]) var String$trim = globalThis._mmlExterns["String.trim"]; else throw new Error("extern " + "String.trim" + " not provided"); }
if (typeof String$starts_with === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.starts_with"]) var String$starts_with = globalThis._mmlExterns["String.starts_with"]; else throw new Error("extern " + "String.starts_with" + " not provided"); }
if (typeof String$contains === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.contains"]) var String$contains = globalThis._mmlExterns["String.contains"]; else throw new Error("extern " + "String.contains" + " not provided"); }
if (typeof String$replace === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.replace"]) var String$replace = globalThis._mmlExterns["String.replace"]; else throw new Error("extern " + "String.replace" + " not provided"); }
if (typeof String$to_int === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.to_int"]) var String$to_int = globalThis._mmlExterns["String.to_int"]; else throw new Error("extern " + "String.to_int" + " not provided"); }
if (typeof String$to_float === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.to_float"]) var String$to_float = globalThis._mmlExterns["String.to_float"]; else throw new Error("extern " + "String.to_float" + " not provided"); }
if (typeof String$uppercase === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.uppercase"]) var String$uppercase = globalThis._mmlExterns["String.uppercase"]; else throw new Error("extern " + "String.uppercase" + " not provided"); }
if (typeof String$lowercase === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.lowercase"]) var String$lowercase = globalThis._mmlExterns["String.lowercase"]; else throw new Error("extern " + "String.lowercase" + " not provided"); }
if (typeof String$get === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.get"]) var String$get = globalThis._mmlExterns["String.get"]; else throw new Error("extern " + "String.get" + " not provided"); }
if (typeof String$to_bytes === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.to_bytes"]) var String$to_bytes = globalThis._mmlExterns["String.to_bytes"]; else throw new Error("extern " + "String.to_bytes" + " not provided"); }
if (typeof String$of_bytes === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.of_bytes"]) var String$of_bytes = globalThis._mmlExterns["String.of_bytes"]; else throw new Error("extern " + "String.of_bytes" + " not provided"); }
if (typeof String$to_byte_array === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.to_byte_array"]) var String$to_byte_array = globalThis._mmlExterns["String.to_byte_array"]; else throw new Error("extern " + "String.to_byte_array" + " not provided"); }
if (typeof String$of_byte_array === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.of_byte_array"]) var String$of_byte_array = globalThis._mmlExterns["String.of_byte_array"]; else throw new Error("extern " + "String.of_byte_array" + " not provided"); }
if (typeof String$to_runes === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.to_runes"]) var String$to_runes = globalThis._mmlExterns["String.to_runes"]; else throw new Error("extern " + "String.to_runes" + " not provided"); }
if (typeof String$of_runes === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.of_runes"]) var String$of_runes = globalThis._mmlExterns["String.of_runes"]; else throw new Error("extern " + "String.of_runes" + " not provided"); }
if (typeof String$get_rune === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.get_rune"]) var String$get_rune = globalThis._mmlExterns["String.get_rune"]; else throw new Error("extern " + "String.get_rune" + " not provided"); }
if (typeof String$of_byte === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.of_byte"]) var String$of_byte = globalThis._mmlExterns["String.of_byte"]; else throw new Error("extern " + "String.of_byte" + " not provided"); }
if (typeof String$rune_length === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.rune_length"]) var String$rune_length = globalThis._mmlExterns["String.rune_length"]; else throw new Error("extern " + "String.rune_length" + " not provided"); }
if (typeof String$make === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.make"]) var String$make = globalThis._mmlExterns["String.make"]; else throw new Error("extern " + "String.make" + " not provided"); }
if (typeof String$index_opt === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.index_opt"]) var String$index_opt = globalThis._mmlExterns["String.index_opt"]; else throw new Error("extern " + "String.index_opt" + " not provided"); }
if (typeof String$rindex_opt === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.rindex_opt"]) var String$rindex_opt = globalThis._mmlExterns["String.rindex_opt"]; else throw new Error("extern " + "String.rindex_opt" + " not provided"); }
if (typeof String$concat === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.concat"]) var String$concat = globalThis._mmlExterns["String.concat"]; else throw new Error("extern " + "String.concat" + " not provided"); }
if (typeof String$compare === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["String.compare"]) var String$compare = globalThis._mmlExterns["String.compare"]; else throw new Error("extern " + "String.compare" + " not provided"); }
function String$iter(f, s) {
  const n_41 = String$length(s);
  function go(i) {
    while (true) {
      let _t43;
      if ((i >= n_41)) {
        _t43 = undefined;
      } else {
        f(String$get(s, i));
        const _t44 = (i + 1);
        i = _t44;
        continue;
        _t43 = undefined;
      }
      return _t43;
    }
  }
  const _t45 = go(0);
  const _t42 = _t45;
  return _t42;
}
function List$fold(f, acc, xs) {
  function go(a, l) {
    while (true) {
      let _t47;
      const _t46 = l;
      _t48: {
        if (_t46 === null) {
          _t47 = a;
          break _t48;
        }
        if (_t46 !== null) {
          const x = _t46._hd;
          const rest = _t46._tl;
          const _t49 = f(a, x);
          const _t50 = rest;
          a = _t49;
          l = _t50;
          continue;
          _t47 = undefined;
          break _t48;
        }
        _match_fail("line 259");
      }
      return _t47;
    }
  }
  const _t51 = go(acc, xs);
  return _t51;
}
function List$length(xs) {
  function _t52(acc, _) {
    return (acc + 1);
  }
  return List$fold(_t52, 0, xs);
}
function List$rev(xs) {
  function _t53(acc, x) {
    return ({_hd: x, _tl: acc});
  }
  return List$fold(_t53, null, xs);
}
function List$hd(xs) {
  let _t55;
  const _t54 = xs;
  _t56: {
    if (_t54 !== null) {
      const x = _t54._hd;
      _t55 = x;
      break _t56;
    }
    _match_fail("line 259");
  }
  return _t55;
}
function List$tl(xs) {
  let _t58;
  const _t57 = xs;
  _t59: {
    if (_t57 !== null) {
      const rest = _t57._tl;
      _t58 = rest;
      break _t59;
    }
    _match_fail("line 259");
  }
  return _t58;
}
function List$nth(xs, n) {
  function go(l, i) {
    while (true) {
      let _t61;
      const _t60 = l;
      _t62: {
        if (_t60 !== null) {
          const x = _t60._hd;
          const rest = _t60._tl;
          let _t63;
          if ((i === 0)) {
            _t63 = x;
          } else {
            const _t64 = rest;
            const _t65 = (i - 1);
            l = _t64;
            i = _t65;
            continue;
            _t63 = undefined;
          }
          _t61 = _t63;
          break _t62;
        }
        _match_fail("line 259");
      }
      return _t61;
    }
  }
  const _t66 = go(xs, n);
  return _t66;
}
function List$concat(a, b) {
  function _t67(acc, x) {
    return ({_hd: x, _tl: acc});
  }
  return List$fold(_t67, b, List$rev(a));
}
function List$is_empty(xs) {
  let _t69;
  const _t68 = xs;
  _t70: {
    if (_t68 === null) {
      _t69 = true;
      break _t70;
    }
    if (true) {
      _t69 = false;
      break _t70;
    }
    _match_fail("line 259");
  }
  return _t69;
}
function List$flatten(xss) {
  function _t71(acc, xs) {
    return List$concat(acc, xs);
  }
  return List$fold(_t71, null, xss);
}
function List$map(f, xs) {
  function go(acc, l) {
    while (true) {
      let _t73;
      const _t72 = l;
      _t74: {
        if (_t72 === null) {
          _t73 = List$rev(acc);
          break _t74;
        }
        if (_t72 !== null) {
          const x = _t72._hd;
          const rest = _t72._tl;
          const _t75 = ({_hd: f(x), _tl: acc});
          const _t76 = rest;
          acc = _t75;
          l = _t76;
          continue;
          _t73 = undefined;
          break _t74;
        }
        _match_fail("line 259");
      }
      return _t73;
    }
  }
  const _t77 = go(null, xs);
  return _t77;
}
function List$filter(f, xs) {
  function go(acc, l) {
    while (true) {
      let _t79;
      const _t78 = l;
      _t80: {
        if (_t78 === null) {
          _t79 = List$rev(acc);
          break _t80;
        }
        if (_t78 !== null) {
          const x = _t78._hd;
          const rest = _t78._tl;
          let _t81;
          if (f(x)) {
            const _t82 = ({_hd: x, _tl: acc});
            const _t83 = rest;
            acc = _t82;
            l = _t83;
            continue;
            _t81 = undefined;
          } else {
            const _t84 = acc;
            const _t85 = rest;
            acc = _t84;
            l = _t85;
            continue;
            _t81 = undefined;
          }
          _t79 = _t81;
          break _t80;
        }
        _match_fail("line 259");
      }
      return _t79;
    }
  }
  const _t86 = go(null, xs);
  return _t86;
}
function List$find(f, xs) {
  function go(l) {
    while (true) {
      let _t88;
      const _t87 = l;
      _t89: {
        if (_t87 === null) {
          _t88 = ({_tag: 0, _name: "None"});
          break _t89;
        }
        if (_t87 !== null) {
          const x = _t87._hd;
          const rest = _t87._tl;
          let _t90;
          if (f(x)) {
            _t90 = ({_tag: 1, _name: "Some", _val: x});
          } else {
            const _t91 = rest;
            l = _t91;
            continue;
            _t90 = undefined;
          }
          _t88 = _t90;
          break _t89;
        }
        _match_fail("line 259");
      }
      return _t88;
    }
  }
  const _t92 = go(xs);
  return _t92;
}
function List$exists(f, xs) {
  function go(l) {
    while (true) {
      let _t94;
      const _t93 = l;
      _t95: {
        if (_t93 === null) {
          _t94 = false;
          break _t95;
        }
        if (_t93 !== null) {
          const x = _t93._hd;
          const rest = _t93._tl;
          let _t96;
          if (f(x)) {
            _t96 = true;
          } else {
            const _t97 = rest;
            l = _t97;
            continue;
            _t96 = undefined;
          }
          _t94 = _t96;
          break _t95;
        }
        _match_fail("line 259");
      }
      return _t94;
    }
  }
  const _t98 = go(xs);
  return _t98;
}
function List$forall(f, xs) {
  function go(l) {
    while (true) {
      let _t100;
      const _t99 = l;
      _t101: {
        if (_t99 === null) {
          _t100 = true;
          break _t101;
        }
        if (_t99 !== null) {
          const x = _t99._hd;
          const rest = _t99._tl;
          let _t102;
          if (f(x)) {
            const _t103 = rest;
            l = _t103;
            continue;
            _t102 = undefined;
          } else {
            _t102 = false;
          }
          _t100 = _t102;
          break _t101;
        }
        _match_fail("line 259");
      }
      return _t100;
    }
  }
  const _t104 = go(xs);
  return _t104;
}
function List$zip(xs, ys) {
  function go(acc, a, b) {
    while (true) {
      let _t106;
      const _t105 = a;
      _t107: {
        if (_t105 === null) {
          _t106 = List$rev(acc);
          break _t107;
        }
        if (_t105 !== null) {
          const x = _t105._hd;
          const ra = _t105._tl;
          let _t109;
          const _t108 = b;
          _t110: {
            if (_t108 !== null) {
              const y = _t108._hd;
              const rb = _t108._tl;
              const _t111 = ({_hd: [x, y], _tl: acc});
              const _t112 = ra;
              const _t113 = rb;
              acc = _t111;
              a = _t112;
              b = _t113;
              continue;
              _t109 = undefined;
              break _t110;
            }
            _match_fail("line 259");
          }
          _t106 = _t109;
          break _t107;
        }
        _match_fail("line 259");
      }
      return _t106;
    }
  }
  const _t114 = go(null, xs, ys);
  return _t114;
}
function List$mapi(f, xs) {
  function go(i, acc, l) {
    while (true) {
      let _t116;
      const _t115 = l;
      _t117: {
        if (_t115 === null) {
          _t116 = List$rev(acc);
          break _t117;
        }
        if (_t115 !== null) {
          const x = _t115._hd;
          const rest = _t115._tl;
          const _t118 = (i + 1);
          const _t119 = ({_hd: f(i, x), _tl: acc});
          const _t120 = rest;
          i = _t118;
          acc = _t119;
          l = _t120;
          continue;
          _t116 = undefined;
          break _t117;
        }
        _match_fail("line 259");
      }
      return _t116;
    }
  }
  const _t121 = go(0, null, xs);
  return _t121;
}
function List$sort(cmp, xs) {
  function insert(x, sorted) {
    let _t123;
    const _t122 = sorted;
    _t124: {
      if (_t122 === null) {
        _t123 = ({_hd: x, _tl: null});
        break _t124;
      }
      if (_t122 !== null) {
        const y = _t122._hd;
        const rest = _t122._tl;
        let _t125;
        if ((cmp(x, y) < 1)) {
          _t125 = ({_hd: x, _tl: sorted});
        } else {
          _t125 = ({_hd: y, _tl: insert(x, rest)});
        }
        _t123 = _t125;
        break _t124;
      }
      _match_fail("line 259");
    }
    return _t123;
  }
  function _t127(acc, x) {
    return insert(x, acc);
  }
  const _t126 = List$fold(_t127, null, xs);
  return _t126;
}
function List$fold_right(f, xs, acc) {
  function go(l) {
    let _t129;
    const _t128 = l;
    _t130: {
      if (_t128 === null) {
        _t129 = acc;
        break _t130;
      }
      if (_t128 !== null) {
        const x = _t128._hd;
        const rest = _t128._tl;
        _t129 = f(x, go(rest));
        break _t130;
      }
      _match_fail("line 259");
    }
    return _t129;
  }
  const _t131 = go(xs);
  return _t131;
}
function List$find_map(f, xs) {
  function go(l) {
    while (true) {
      let _t133;
      const _t132 = l;
      _t134: {
        if (_t132 === null) {
          _t133 = ({_tag: 0, _name: "None"});
          break _t134;
        }
        if (_t132 !== null) {
          const x = _t132._hd;
          const rest = _t132._tl;
          let _t136;
          const _t135 = f(x);
          _t137: {
            if (_t135._tag === 1) {
              const result = _t135;
              _t136 = result;
              break _t137;
            }
            if (_t135._tag === 0) {
              const _t138 = rest;
              l = _t138;
              continue;
              _t136 = undefined;
              break _t137;
            }
            _match_fail("line 259");
          }
          _t133 = _t136;
          break _t134;
        }
        _match_fail("line 259");
      }
      return _t133;
    }
  }
  const _t139 = go(xs);
  return _t139;
}
function List$assoc_opt(key, xs) {
  function go(l) {
    while (true) {
      let _t141;
      const _t140 = l;
      _t142: {
        if (_t140 === null) {
          _t141 = ({_tag: 0, _name: "None"});
          break _t142;
        }
        if (_t140 !== null) {
          const k = _t140._hd[0];
          const v = _t140._hd[1];
          const rest = _t140._tl;
          let _t143;
          if (_eq(k, key)) {
            _t143 = ({_tag: 1, _name: "Some", _val: v});
          } else {
            const _t144 = rest;
            l = _t144;
            continue;
            _t143 = undefined;
          }
          _t141 = _t143;
          break _t142;
        }
        _match_fail("line 259");
      }
      return _t141;
    }
  }
  const _t145 = go(xs);
  return _t145;
}
function List$init(n, f) {
  function go(i, acc) {
    while (true) {
      let _t146;
      if ((i < 0)) {
        _t146 = acc;
      } else {
        const _t147 = (i - 1);
        const _t148 = ({_hd: f(i), _tl: acc});
        i = _t147;
        acc = _t148;
        continue;
        _t146 = undefined;
      }
      return _t146;
    }
  }
  const _t149 = go((n - 1), null);
  return _t149;
}
function List$concat_map(f, xs) {
  return List$flatten(List$map(f, xs));
}
function List$iter2(f, xs, ys) {
  function go(a, b) {
    while (true) {
      let _t151;
      const _t150 = a;
      _t152: {
        if (_t150 === null) {
          _t151 = undefined;
          break _t152;
        }
        if (_t150 !== null) {
          const x = _t150._hd;
          const ra = _t150._tl;
          let _t154;
          const _t153 = b;
          _t155: {
            if (_t153 !== null) {
              const y = _t153._hd;
              const rb = _t153._tl;
              f(x, y);
              const _t156 = ra;
              const _t157 = rb;
              a = _t156;
              b = _t157;
              continue;
              _t154 = undefined;
              break _t155;
            }
            _match_fail("line 259");
          }
          _t151 = _t154;
          break _t152;
        }
        _match_fail("line 259");
      }
      return _t151;
    }
  }
  const _t158 = go(xs, ys);
  return _t158;
}
function List$map2(f, xs, ys) {
  function go(acc, a, b) {
    while (true) {
      let _t160;
      const _t159 = a;
      _t161: {
        if (_t159 === null) {
          _t160 = List$rev(acc);
          break _t161;
        }
        if (_t159 !== null) {
          const x = _t159._hd;
          const ra = _t159._tl;
          let _t163;
          const _t162 = b;
          _t164: {
            if (_t162 !== null) {
              const y = _t162._hd;
              const rb = _t162._tl;
              const _t165 = ({_hd: f(x, y), _tl: acc});
              const _t166 = ra;
              const _t167 = rb;
              acc = _t165;
              a = _t166;
              b = _t167;
              continue;
              _t163 = undefined;
              break _t164;
            }
            _match_fail("line 259");
          }
          _t160 = _t163;
          break _t161;
        }
        _match_fail("line 259");
      }
      return _t160;
    }
  }
  const _t168 = go(null, xs, ys);
  return _t168;
}
function List$fold2(f, acc, xs, ys) {
  function go(a, l1, l2) {
    while (true) {
      let _t170;
      const _t169 = l1;
      _t171: {
        if (_t169 === null) {
          _t170 = a;
          break _t171;
        }
        if (_t169 !== null) {
          const x = _t169._hd;
          const r1 = _t169._tl;
          let _t173;
          const _t172 = l2;
          _t174: {
            if (_t172 !== null) {
              const y = _t172._hd;
              const r2 = _t172._tl;
              const _t175 = f(a, x, y);
              const _t176 = r1;
              const _t177 = r2;
              a = _t175;
              l1 = _t176;
              l2 = _t177;
              continue;
              _t173 = undefined;
              break _t174;
            }
            _match_fail("line 259");
          }
          _t170 = _t173;
          break _t171;
        }
        _match_fail("line 259");
      }
      return _t170;
    }
  }
  const _t178 = go(acc, xs, ys);
  return _t178;
}
function List$forall2(f, xs, ys) {
  function go(a, b) {
    while (true) {
      let _t180;
      const _t179 = a;
      _t181: {
        if (_t179 === null) {
          _t180 = true;
          break _t181;
        }
        if (_t179 !== null) {
          const x = _t179._hd;
          const ra = _t179._tl;
          let _t183;
          const _t182 = b;
          _t184: {
            if (_t182 !== null) {
              const y = _t182._hd;
              const rb = _t182._tl;
              let _t185;
              if (f(x, y)) {
                const _t186 = ra;
                const _t187 = rb;
                a = _t186;
                b = _t187;
                continue;
                _t185 = undefined;
              } else {
                _t185 = false;
              }
              _t183 = _t185;
              break _t184;
            }
            _match_fail("line 259");
          }
          _t180 = _t183;
          break _t181;
        }
        _match_fail("line 259");
      }
      return _t180;
    }
  }
  const _t188 = go(xs, ys);
  return _t188;
}
function List$iteri(f, xs) {
  function go(i, l) {
    while (true) {
      let _t190;
      const _t189 = l;
      _t191: {
        if (_t189 === null) {
          _t190 = undefined;
          break _t191;
        }
        if (_t189 !== null) {
          const x = _t189._hd;
          const rest = _t189._tl;
          f(i, x);
          const _t192 = (i + 1);
          const _t193 = rest;
          i = _t192;
          l = _t193;
          continue;
          _t190 = undefined;
          break _t191;
        }
        _match_fail("line 259");
      }
      return _t190;
    }
  }
  const _t194 = go(0, xs);
  return _t194;
}
function List$mem_assoc(key, xs) {
  function go(l) {
    while (true) {
      let _t196;
      const _t195 = l;
      _t197: {
        if (_t195 === null) {
          _t196 = false;
          break _t197;
        }
        if (_t195 !== null) {
          const k = _t195._hd[0];
          const rest = _t195._tl;
          let _t198;
          if (_eq(k, key)) {
            _t198 = true;
          } else {
            const _t199 = rest;
            l = _t199;
            continue;
            _t198 = undefined;
          }
          _t196 = _t198;
          break _t197;
        }
        _match_fail("line 259");
      }
      return _t196;
    }
  }
  const _t200 = go(xs);
  return _t200;
}
function List$assoc(key, xs) {
  let _t202;
  const _t201 = List$assoc_opt(key, xs);
  _t203: {
    if (_t201._tag === 1) {
      const v = _t201._val;
      _t202 = v;
      break _t203;
    }
    if (_t201._tag === 0) {
      _t202 = failwith("List.assoc: not found");
      break _t203;
    }
    _match_fail("line 259");
  }
  return _t202;
}
function List$iter(f, xs) {
  function go(xs) {
    while (true) {
      let _t205;
      const _t204 = xs;
      _t206: {
        if (_t204 === null) {
          _t205 = undefined;
          break _t206;
        }
        if (_t204 !== null) {
          const x = _t204._hd;
          const rest = _t204._tl;
          f(x);
          const _t207 = rest;
          xs = _t207;
          continue;
          _t205 = undefined;
          break _t206;
        }
        _match_fail("line 259");
      }
      return _t205;
    }
  }
  const _t208 = go(xs);
  return _t208;
}
function List$mem(x, xs) {
  function go(xs) {
    while (true) {
      let _t210;
      const _t209 = xs;
      _t211: {
        if (_t209 === null) {
          _t210 = false;
          break _t211;
        }
        if (_t209 !== null) {
          const y = _t209._hd;
          const rest = _t209._tl;
          let _t212;
          if (_eq(x, y)) {
            _t212 = true;
          } else {
            const _t213 = rest;
            xs = _t213;
            continue;
            _t212 = undefined;
          }
          _t210 = _t212;
          break _t211;
        }
        _match_fail("line 259");
      }
      return _t210;
    }
  }
  const _t214 = go(xs);
  return _t214;
}
function List$rev_append(xs, ys) {
  function go(xs, acc) {
    while (true) {
      let _t216;
      const _t215 = xs;
      _t217: {
        if (_t215 === null) {
          _t216 = acc;
          break _t217;
        }
        if (_t215 !== null) {
          const x = _t215._hd;
          const rest = _t215._tl;
          const _t218 = rest;
          const _t219 = ({_hd: x, _tl: acc});
          xs = _t218;
          acc = _t219;
          continue;
          _t216 = undefined;
          break _t217;
        }
        _match_fail("line 259");
      }
      return _t216;
    }
  }
  const _t220 = go(xs, ys);
  return _t220;
}
function List$nth_opt(xs, n) {
  function go(xs, i) {
    while (true) {
      let _t222;
      const _t221 = xs;
      _t223: {
        if (_t221 === null) {
          _t222 = ({_tag: 0, _name: "None"});
          break _t223;
        }
        if (_t221 !== null) {
          const x = _t221._hd;
          const rest = _t221._tl;
          let _t224;
          if ((i === 0)) {
            _t224 = ({_tag: 1, _name: "Some", _val: x});
          } else {
            const _t225 = rest;
            const _t226 = (i - 1);
            xs = _t225;
            i = _t226;
            continue;
            _t224 = undefined;
          }
          _t222 = _t224;
          break _t223;
        }
        _match_fail("line 259");
      }
      return _t222;
    }
  }
  const _t227 = go(xs, n);
  return _t227;
}
function List$find_index(f, xs) {
  function go(xs, i) {
    while (true) {
      let _t229;
      const _t228 = xs;
      _t230: {
        if (_t228 === null) {
          _t229 = ({_tag: 0, _name: "None"});
          break _t230;
        }
        if (_t228 !== null) {
          const x = _t228._hd;
          const rest = _t228._tl;
          let _t231;
          if (f(x)) {
            _t231 = ({_tag: 1, _name: "Some", _val: i});
          } else {
            const _t232 = rest;
            const _t233 = (i + 1);
            xs = _t232;
            i = _t233;
            continue;
            _t231 = undefined;
          }
          _t229 = _t231;
          break _t230;
        }
        _match_fail("line 259");
      }
      return _t229;
    }
  }
  const _t234 = go(xs, 0);
  return _t234;
}
function List$filteri(f, xs) {
  function go(xs, i, acc) {
    while (true) {
      let _t236;
      const _t235 = xs;
      _t237: {
        if (_t235 === null) {
          _t236 = List$rev(acc);
          break _t237;
        }
        if (_t235 !== null) {
          const x = _t235._hd;
          const rest = _t235._tl;
          let _t238;
          if (f(i, x)) {
            const _t239 = rest;
            const _t240 = (i + 1);
            const _t241 = ({_hd: x, _tl: acc});
            xs = _t239;
            i = _t240;
            acc = _t241;
            continue;
            _t238 = undefined;
          } else {
            const _t242 = rest;
            const _t243 = (i + 1);
            const _t244 = acc;
            xs = _t242;
            i = _t243;
            acc = _t244;
            continue;
            _t238 = undefined;
          }
          _t236 = _t238;
          break _t237;
        }
        _match_fail("line 259");
      }
      return _t236;
    }
  }
  const _t245 = go(xs, 0, null);
  return _t245;
}
function List$filter_map(f, xs) {
  function go(xs, acc) {
    while (true) {
      let _t247;
      const _t246 = xs;
      _t248: {
        if (_t246 === null) {
          _t247 = List$rev(acc);
          break _t248;
        }
        if (_t246 !== null) {
          const x = _t246._hd;
          const rest = _t246._tl;
          let _t250;
          const _t249 = f(x);
          _t251: {
            if (_t249._tag === 1) {
              const y = _t249._val;
              const _t252 = rest;
              const _t253 = ({_hd: y, _tl: acc});
              xs = _t252;
              acc = _t253;
              continue;
              _t250 = undefined;
              break _t251;
            }
            if (_t249._tag === 0) {
              const _t254 = rest;
              const _t255 = acc;
              xs = _t254;
              acc = _t255;
              continue;
              _t250 = undefined;
              break _t251;
            }
            _match_fail("line 259");
          }
          _t247 = _t250;
          break _t248;
        }
        _match_fail("line 259");
      }
      return _t247;
    }
  }
  const _t256 = go(xs, null);
  return _t256;
}
function List$sort_uniq(cmp, xs) {
  const sorted_257 = List$sort(cmp, xs);
  function dedup(xs) {
    while (true) {
      let _t260;
      const _t259 = xs;
      _t261: {
        if (_t259 === null) {
          _t260 = null;
          break _t261;
        }
        if (_t259 !== null && _t259._tl === null) {
          const x = _t259._hd;
          _t260 = ({_hd: x, _tl: null});
          break _t261;
        }
        if (_t259 !== null && _t259._tl !== null) {
          const x = _t259._hd;
          const y = _t259._tl._hd;
          const rest = _t259._tl._tl;
          let _t262;
          if ((cmp(x, y) === 0)) {
            const _t263 = ({_hd: y, _tl: rest});
            xs = _t263;
            continue;
            _t262 = undefined;
          } else {
            _t262 = ({_hd: x, _tl: dedup(({_hd: y, _tl: rest}))});
          }
          _t260 = _t262;
          break _t261;
        }
        _match_fail("line 259");
      }
      return _t260;
    }
  }
  const _t264 = dedup(sorted_257);
  const _t258 = _t264;
  return _t258;
}
if (typeof Array$make === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Array.make"]) var Array$make = globalThis._mmlExterns["Array.make"]; else throw new Error("extern " + "Array.make" + " not provided"); }
if (typeof Array$get === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Array.get"]) var Array$get = globalThis._mmlExterns["Array.get"]; else throw new Error("extern " + "Array.get" + " not provided"); }
if (typeof Array$set === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Array.set"]) var Array$set = globalThis._mmlExterns["Array.set"]; else throw new Error("extern " + "Array.set" + " not provided"); }
if (typeof Array$length === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Array.length"]) var Array$length = globalThis._mmlExterns["Array.length"]; else throw new Error("extern " + "Array.length" + " not provided"); }
if (typeof Array$to_list === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Array.to_list"]) var Array$to_list = globalThis._mmlExterns["Array.to_list"]; else throw new Error("extern " + "Array.to_list" + " not provided"); }
if (typeof Array$of_list === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Array.of_list"]) var Array$of_list = globalThis._mmlExterns["Array.of_list"]; else throw new Error("extern " + "Array.of_list" + " not provided"); }
if (typeof Array$copy === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Array.copy"]) var Array$copy = globalThis._mmlExterns["Array.copy"]; else throw new Error("extern " + "Array.copy" + " not provided"); }
if (typeof Array$sub === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Array.sub"]) var Array$sub = globalThis._mmlExterns["Array.sub"]; else throw new Error("extern " + "Array.sub" + " not provided"); }
function Array$init(n, f) {
  let _t265;
  if ((n <= 0)) {
    _t265 = {_arr: []};
  } else {
    const arr_266 = Array$make(n, f(0));
    function go(i) {
      while (true) {
        let _t268;
        if ((i >= n)) {
          _t268 = arr_266;
        } else {
          Array$set(arr_266, i, f(i));
          const _t269 = (i + 1);
          i = _t269;
          continue;
          _t268 = undefined;
        }
        return _t268;
      }
    }
    const _t270 = go(1);
    const _t267 = _t270;
    _t265 = _t267;
  }
  return _t265;
}
function Array$map(f, arr) {
  const n_271 = Array$length(arr);
  let _t273;
  if ((n_271 === 0)) {
    _t273 = {_arr: []};
  } else {
    const result_274 = Array$make(n_271, f(Array$get(arr, 0)));
    function go(i) {
      while (true) {
        let _t276;
        if ((i >= n_271)) {
          _t276 = result_274;
        } else {
          Array$set(result_274, i, f(Array$get(arr, i)));
          const _t277 = (i + 1);
          i = _t277;
          continue;
          _t276 = undefined;
        }
        return _t276;
      }
    }
    const _t278 = go(1);
    const _t275 = _t278;
    _t273 = _t275;
  }
  const _t272 = _t273;
  return _t272;
}
function Array$iter(f, arr) {
  const n_279 = Array$length(arr);
  function go(i) {
    while (true) {
      let _t281;
      if ((i >= n_279)) {
        _t281 = undefined;
      } else {
        f(Array$get(arr, i));
        const _t282 = (i + 1);
        i = _t282;
        continue;
        _t281 = undefined;
      }
      return _t281;
    }
  }
  const _t283 = go(0);
  const _t280 = _t283;
  return _t280;
}
function Array$iteri(f, arr) {
  const n_284 = Array$length(arr);
  function go(i) {
    while (true) {
      let _t286;
      if ((i >= n_284)) {
        _t286 = undefined;
      } else {
        f(i, Array$get(arr, i));
        const _t287 = (i + 1);
        i = _t287;
        continue;
        _t286 = undefined;
      }
      return _t286;
    }
  }
  const _t288 = go(0);
  const _t285 = _t288;
  return _t285;
}
function Array$forall(f, arr) {
  const n_289 = Array$length(arr);
  function go(i) {
    while (true) {
      let _t291;
      if ((i >= n_289)) {
        _t291 = true;
      } else {
        let _t292;
        if (f(Array$get(arr, i))) {
          const _t293 = (i + 1);
          i = _t293;
          continue;
          _t292 = undefined;
        } else {
          _t292 = false;
        }
        _t291 = _t292;
      }
      return _t291;
    }
  }
  const _t294 = go(0);
  const _t290 = _t294;
  return _t290;
}
function Array$fold(f, acc, arr) {
  const n_295 = Array$length(arr);
  function go(i, a) {
    while (true) {
      let _t297;
      if ((i >= n_295)) {
        _t297 = a;
      } else {
        const _t298 = (i + 1);
        const _t299 = f(a, Array$get(arr, i));
        i = _t298;
        a = _t299;
        continue;
        _t297 = undefined;
      }
      return _t297;
    }
  }
  const _t300 = go(0, acc);
  const _t296 = _t300;
  return _t296;
}
if (typeof IO$read_file === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["IO.read_file"]) var IO$read_file = globalThis._mmlExterns["IO.read_file"]; else throw new Error("extern " + "IO.read_file" + " not provided"); }
if (typeof IO$write_file === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["IO.write_file"]) var IO$write_file = globalThis._mmlExterns["IO.write_file"]; else throw new Error("extern " + "IO.write_file" + " not provided"); }
if (typeof IO$append_file === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["IO.append_file"]) var IO$append_file = globalThis._mmlExterns["IO.append_file"]; else throw new Error("extern " + "IO.append_file" + " not provided"); }
if (typeof IO$read_line === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["IO.read_line"]) var IO$read_line = globalThis._mmlExterns["IO.read_line"]; else throw new Error("extern " + "IO.read_line" + " not provided"); }
if (typeof IO$file_exists === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["IO.file_exists"]) var IO$file_exists = globalThis._mmlExterns["IO.file_exists"]; else throw new Error("extern " + "IO.file_exists" + " not provided"); }
if (typeof Sys$args === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Sys.args"]) var Sys$args = globalThis._mmlExterns["Sys.args"]; else throw new Error("extern " + "Sys.args" + " not provided"); }
if (typeof Sys$getenv === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Sys.getenv"]) var Sys$getenv = globalThis._mmlExterns["Sys.getenv"]; else throw new Error("extern " + "Sys.getenv" + " not provided"); }
if (typeof Sys$exit === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Sys.exit"]) var Sys$exit = globalThis._mmlExterns["Sys.exit"]; else throw new Error("extern " + "Sys.exit" + " not provided"); }
if (typeof Sys$time === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Sys.time"]) var Sys$time = globalThis._mmlExterns["Sys.time"]; else throw new Error("extern " + "Sys.time" + " not provided"); }
function Math$abs(x) {
  let _t301;
  if ((x < 0)) {
    _t301 = (0 - x);
  } else {
    _t301 = x;
  }
  return _t301;
}
function Math$min(a, b) {
  let _t302;
  if ((a < b)) {
    _t302 = a;
  } else {
    _t302 = b;
  }
  return _t302;
}
function Math$max(a, b) {
  let _t303;
  if ((a > b)) {
    _t303 = a;
  } else {
    _t303 = b;
  }
  return _t303;
}
function Math$pow(a, b) {
  return __math_pow(a, b);
}
function Math$sqrt(x) {
  return __math_sqrt(x);
}
function Math$floor(x) {
  return __math_floor(x);
}
function Math$ceil(x) {
  return __math_ceil(x);
}
function Math$round(x) {
  return __math_round(x);
}
function Result$map(f, r) {
  let _t305;
  const _t304 = r;
  _t306: {
    if (_t304._tag === 0) {
      const v = _t304._val;
      _t305 = ({_tag: 0, _name: "Ok", _val: f(v)});
      break _t306;
    }
    if (_t304._tag === 1) {
      const e = _t304._val;
      _t305 = ({_tag: 1, _name: "Err", _val: e});
      break _t306;
    }
    _match_fail("line 14");
  }
  return _t305;
}
function Result$bind(f, r) {
  let _t308;
  const _t307 = r;
  _t309: {
    if (_t307._tag === 0) {
      const v = _t307._val;
      _t308 = f(v);
      break _t309;
    }
    if (_t307._tag === 1) {
      const e = _t307._val;
      _t308 = ({_tag: 1, _name: "Err", _val: e});
      break _t309;
    }
    _match_fail("line 14");
  }
  return _t308;
}
function Result$unwrap(r) {
  let _t311;
  const _t310 = r;
  _t312: {
    if (_t310._tag === 0) {
      const v = _t310._val;
      _t311 = v;
      break _t312;
    }
    _match_fail("line 14");
  }
  return _t311;
}
function Byte$to_int(b) {
  return __byte_to_int(b);
}
function Byte$of_int(n) {
  return __byte_of_int(n);
}
function Byte$to_string(b) {
  return __byte_to_string(b);
}
function Byte$is_alpha(b) {
  const n_313 = Byte$to_int(b);
  const _t314 = (((n_313 >= 65) && (n_313 <= 90)) || ((n_313 >= 97) && (n_313 <= 122)));
  return _t314;
}
function Byte$is_digit(b) {
  const n_315 = Byte$to_int(b);
  const _t316 = ((n_315 >= 48) && (n_315 <= 57));
  return _t316;
}
function Byte$is_space(b) {
  const n_317 = Byte$to_int(b);
  const _t318 = ((((n_317 === 32) || (n_317 === 9)) || (n_317 === 10)) || (n_317 === 13));
  return _t318;
}
function Byte$is_upper(b) {
  const n_319 = Byte$to_int(b);
  const _t320 = ((n_319 >= 65) && (n_319 <= 90));
  return _t320;
}
function Byte$is_lower(b) {
  const n_321 = Byte$to_int(b);
  const _t322 = ((n_321 >= 97) && (n_321 <= 122));
  return _t322;
}
function Byte$to_upper(b) {
  const n_323 = Byte$to_int(b);
  let _t325;
  if (((n_323 >= 97) && (n_323 <= 122))) {
    _t325 = Byte$of_int((n_323 - 32));
  } else {
    _t325 = b;
  }
  const _t324 = _t325;
  return _t324;
}
function Byte$to_lower(b) {
  const n_326 = Byte$to_int(b);
  let _t328;
  if (((n_326 >= 65) && (n_326 <= 90))) {
    _t328 = Byte$of_int((n_326 + 32));
  } else {
    _t328 = b;
  }
  const _t327 = _t328;
  return _t327;
}
function Rune$to_int(r) {
  return __rune_to_int(r);
}
function Rune$of_int(n) {
  return __rune_of_int(n);
}
function Rune$to_string(r) {
  return __rune_to_string(r);
}
function Rune$is_alpha(r) {
  const n_329 = Rune$to_int(r);
  const _t330 = (((n_329 >= 65) && (n_329 <= 90)) || ((n_329 >= 97) && (n_329 <= 122)));
  return _t330;
}
function Rune$is_digit(r) {
  const n_331 = Rune$to_int(r);
  const _t332 = ((n_331 >= 48) && (n_331 <= 57));
  return _t332;
}
function Rune$is_space(r) {
  const n_333 = Rune$to_int(r);
  const _t334 = ((((n_333 === 32) || (n_333 === 9)) || (n_333 === 10)) || (n_333 === 13));
  return _t334;
}
function Rune$is_upper(r) {
  const n_335 = Rune$to_int(r);
  const _t336 = ((n_335 >= 65) && (n_335 <= 90));
  return _t336;
}
function Rune$is_lower(r) {
  const n_337 = Rune$to_int(r);
  const _t338 = ((n_337 >= 97) && (n_337 <= 122));
  return _t338;
}
function Set$empty(_) {
  return new Map([]);
}
function Set$singleton(__dict_Map_0, x) {
  return __dict_Map_0.set(x, undefined, new Map([]));
}
function Set$of_list(__dict_Map_0, xs) {
  function _t339(acc, x) {
    return __dict_Map_0.set(x, undefined, acc);
  }
  return List$fold(_t339, new Map([]), xs);
}
function Set$add(__dict_Map_0, elem, s) {
  return __dict_Map_0.set(elem, undefined, s);
}
function Set$remove(elem, s) {
  return __dict_Map_g0_g1_map__g0__g1.remove(elem, s);
}
function Set$mem(elem, s) {
  return __dict_Map_g0_g1_map__g0__g1.has(elem, s);
}
function Set$size(s) {
  return __dict_Map_g0_g1_map__g0__g1.size(s);
}
function Set$to_list(s) {
  return __dict_Map_g0_g1_map__g0__g1.keys(s);
}
function Set$union(__dict_Map_0, s1, s2) {
  function _t340(acc, x) {
    return __dict_Map_0.set(x, undefined, acc);
  }
  return List$fold(_t340, s2, __dict_Map_0.keys(s1));
}
function Set$inter(__dict_Map_0, s1, s2) {
  function _t341(acc, x) {
    let _t342;
    if (__dict_Map_0.has(x, s2)) {
      _t342 = __dict_Map_0.set(x, undefined, acc);
    } else {
      _t342 = acc;
    }
    return _t342;
  }
  return List$fold(_t341, Set$empty(undefined), __dict_Map_0.keys(s1));
}
function Set$diff(__dict_Map_0, s1, s2) {
  function _t343(acc, x) {
    let _t344;
    if ((!__dict_Map_0.has(x, s2))) {
      _t344 = __dict_Map_0.set(x, undefined, acc);
    } else {
      _t344 = acc;
    }
    return _t344;
  }
  return List$fold(_t343, Set$empty(undefined), __dict_Map_0.keys(s1));
}
function Set$is_empty(s) {
  return (Set$size(s) === 0);
}
function Set$is_subset(s1, s2) {
  function _t345(x) {
    return __dict_Map_g0_g1_map__g0__g1.has(x, s2);
  }
  return List$forall(_t345, __dict_Map_g0_g1_map__g0__g1.keys(s1));
}
function _t346(f, acc, s) {
  function go(a, l) {
    while (true) {
      let _t348;
      const _t347 = l;
      _t349: {
        if (_t347 === null) {
          _t348 = a;
          break _t349;
        }
        if (_t347 !== null) {
          const x = _t347._hd;
          const rest = _t347._tl;
          const _t350 = f(a, x);
          const _t351 = rest;
          a = _t350;
          l = _t351;
          continue;
          _t348 = undefined;
          break _t349;
        }
        _match_fail("line 45");
      }
      return _t348;
    }
  }
  const _t352 = go(acc, Set$to_list(s));
  return _t352;
}
const __dict_Iter_g0_unit_map__g0 = ({fold: _t346});
function Enum$reduce(f, xs) {
  let _t354;
  const _t353 = xs;
  _t355: {
    if (_t353 !== null) {
      const x = _t353._hd;
      const rest = _t353._tl;
      _t354 = List$fold(f, x, rest);
      break _t355;
    }
    _match_fail("line 118");
  }
  return _t354;
}
function Enum$sum(xs) {
  function _t356(a, b) {
    return (a + b);
  }
  return List$fold(_t356, 0, xs);
}
function Enum$count(f, xs) {
  function _t357(acc, x) {
    let _t358;
    if (f(x)) {
      _t358 = (acc + 1);
    } else {
      _t358 = acc;
    }
    return _t358;
  }
  return List$fold(_t357, 0, xs);
}
function Enum$take(n, xs) {
  function go(i, acc, l) {
    while (true) {
      let _t359;
      if ((i >= n)) {
        _t359 = List$rev(acc);
      } else {
        let _t361;
        const _t360 = l;
        _t362: {
          if (_t360 === null) {
            _t361 = List$rev(acc);
            break _t362;
          }
          if (_t360 !== null) {
            const x = _t360._hd;
            const rest = _t360._tl;
            const _t363 = (i + 1);
            const _t364 = ({_hd: x, _tl: acc});
            const _t365 = rest;
            i = _t363;
            acc = _t364;
            l = _t365;
            continue;
            _t361 = undefined;
            break _t362;
          }
          _match_fail("line 118");
        }
        _t359 = _t361;
      }
      return _t359;
    }
  }
  const _t366 = go(0, null, xs);
  return _t366;
}
function Enum$drop(n, xs) {
  function go(i, l) {
    while (true) {
      let _t367;
      if ((i >= n)) {
        _t367 = l;
      } else {
        let _t369;
        const _t368 = l;
        _t370: {
          if (_t368 === null) {
            _t369 = null;
            break _t370;
          }
          if (_t368 !== null) {
            const rest = _t368._tl;
            const _t371 = (i + 1);
            const _t372 = rest;
            i = _t371;
            l = _t372;
            continue;
            _t369 = undefined;
            break _t370;
          }
          _match_fail("line 118");
        }
        _t367 = _t369;
      }
      return _t367;
    }
  }
  const _t373 = go(0, xs);
  return _t373;
}
function Enum$take_while(f, xs) {
  function go(acc, l) {
    while (true) {
      let _t375;
      const _t374 = l;
      _t376: {
        if (_t374 === null) {
          _t375 = List$rev(acc);
          break _t376;
        }
        if (_t374 !== null) {
          const x = _t374._hd;
          const rest = _t374._tl;
          let _t377;
          if (f(x)) {
            const _t378 = ({_hd: x, _tl: acc});
            const _t379 = rest;
            acc = _t378;
            l = _t379;
            continue;
            _t377 = undefined;
          } else {
            _t377 = List$rev(acc);
          }
          _t375 = _t377;
          break _t376;
        }
        _match_fail("line 118");
      }
      return _t375;
    }
  }
  const _t380 = go(null, xs);
  return _t380;
}
function Enum$drop_while(f, xs) {
  function go(l) {
    while (true) {
      let _t382;
      const _t381 = l;
      _t383: {
        if (_t381 === null) {
          _t382 = null;
          break _t383;
        }
        if (_t381 !== null) {
          const x = _t381._hd;
          const rest = _t381._tl;
          let _t384;
          if (f(x)) {
            const _t385 = rest;
            l = _t385;
            continue;
            _t384 = undefined;
          } else {
            _t384 = l;
          }
          _t382 = _t384;
          break _t383;
        }
        _match_fail("line 118");
      }
      return _t382;
    }
  }
  const _t386 = go(xs);
  return _t386;
}
function Enum$flat_map(f, xs) {
  return List$flatten(List$map(f, xs));
}
function Enum$each(f, xs) {
  function _t387(_, x) {
    return f(x);
  }
  return List$fold(_t387, undefined, xs);
}
function Enum$reject(f, xs) {
  function _t388(x) {
    return (!f(x));
  }
  return List$filter(_t388, xs);
}
function Enum$enumerate(xs) {
  function _t389(i, x) {
    return [i, x];
  }
  return List$mapi(_t389, xs);
}
function Enum$join(sep, xs) {
  let _t391;
  const _t390 = xs;
  _t392: {
    if (_t390 === null) {
      _t391 = "";
      break _t392;
    }
    if (_t390 !== null) {
      const first = _t390._hd;
      const rest = _t390._tl;
      function _t393(acc, x) {
        return ((acc + sep) + x);
      }
      _t391 = List$fold(_t393, first, rest);
      break _t392;
    }
    _match_fail("line 118");
  }
  return _t391;
}
function Enum$chunk(n, xs) {
  function go(acc, l) {
    while (true) {
      let _t395;
      const _t394 = l;
      _t396: {
        if (_t394 === null) {
          _t395 = List$rev(acc);
          break _t396;
        }
        if (true) {
          const _t397 = ({_hd: Enum$take(n, l), _tl: acc});
          const _t398 = Enum$drop(n, l);
          acc = _t397;
          l = _t398;
          continue;
          _t395 = undefined;
          break _t396;
        }
        _match_fail("line 118");
      }
      return _t395;
    }
  }
  const _t399 = go(null, xs);
  return _t399;
}
function Enum$dedup(xs) {
  function go(prev, acc, l) {
    while (true) {
      let _t401;
      const _t400 = l;
      _t402: {
        if (_t400 === null) {
          _t401 = List$rev(acc);
          break _t402;
        }
        if (_t400 !== null) {
          const x = _t400._hd;
          const rest = _t400._tl;
          let _t403;
          if (_eq(x, prev)) {
            const _t404 = prev;
            const _t405 = acc;
            const _t406 = rest;
            prev = _t404;
            acc = _t405;
            l = _t406;
            continue;
            _t403 = undefined;
          } else {
            const _t407 = x;
            const _t408 = ({_hd: x, _tl: acc});
            const _t409 = rest;
            prev = _t407;
            acc = _t408;
            l = _t409;
            continue;
            _t403 = undefined;
          }
          _t401 = _t403;
          break _t402;
        }
        _match_fail("line 118");
      }
      return _t401;
    }
  }
  let _t412;
  const _t411 = xs;
  _t413: {
    if (_t411 === null) {
      _t412 = null;
      break _t413;
    }
    if (_t411 !== null) {
      const x = _t411._hd;
      const rest = _t411._tl;
      _t412 = go(x, ({_hd: x, _tl: null}), rest);
      break _t413;
    }
    _match_fail("line 118");
  }
  const _t410 = _t412;
  return _t410;
}
function Enum$uniq(xs) {
  function _t414(acc, x) {
    function _t415(y) {
      return _eq(y, x);
    }
    let _t416;
    if (List$exists(_t415, acc)) {
      _t416 = acc;
    } else {
      _t416 = ({_hd: x, _tl: acc});
    }
    return _t416;
  }
  return List$rev(List$fold(_t414, null, xs));
}
function Enum$scan(f, init, xs) {
  function _t417(acc, x) {
    let _t419;
    const _t418 = acc;
    _t420: {
      if (_t418 === null) {
        _t419 = ({_hd: f(init, x), _tl: null});
        break _t420;
      }
      if (_t418 !== null) {
        const prev = _t418._hd;
        _t419 = ({_hd: f(prev, x), _tl: acc});
        break _t420;
      }
      _match_fail("line 118");
    }
    return _t419;
  }
  return List$rev(List$fold(_t417, ({_hd: init, _tl: null}), xs));
}
function Enum$intersperse(sep, xs) {
  let _t422;
  const _t421 = xs;
  _t423: {
    if (_t421 === null) {
      _t422 = null;
      break _t423;
    }
    if (_t421 !== null) {
      const first = _t421._hd;
      const rest = _t421._tl;
      function _t424(acc, x) {
        return List$concat(acc, ({_hd: sep, _tl: ({_hd: x, _tl: null})}));
      }
      _t422 = List$fold(_t424, ({_hd: first, _tl: null}), rest);
      break _t423;
    }
    _match_fail("line 118");
  }
  return _t422;
}
function Enum$zip_with(f, xs, ys) {
  function _t425(p) {
    let _t427;
    const _t426 = p;
    _t428: {
      if (true) {
        const a = _t426[0];
        const b = _t426[1];
        _t427 = f(a, b);
        break _t428;
      }
      _match_fail("line 118");
    }
    return _t427;
  }
  return List$map(_t425, List$zip(xs, ys));
}
function Enum$min_by(f, xs) {
  let _t430;
  const _t429 = xs;
  _t431: {
    if (_t429 !== null) {
      const x = _t429._hd;
      const rest = _t429._tl;
      function _t432(best, y) {
        let _t433;
        if ((f(y) < f(best))) {
          _t433 = y;
        } else {
          _t433 = best;
        }
        return _t433;
      }
      _t430 = List$fold(_t432, x, rest);
      break _t431;
    }
    _match_fail("line 118");
  }
  return _t430;
}
function Enum$max_by(f, xs) {
  let _t435;
  const _t434 = xs;
  _t436: {
    if (_t434 !== null) {
      const x = _t434._hd;
      const rest = _t434._tl;
      function _t437(best, y) {
        let _t438;
        if ((f(y) > f(best))) {
          _t438 = y;
        } else {
          _t438 = best;
        }
        return _t438;
      }
      _t435 = List$fold(_t437, x, rest);
      break _t436;
    }
    _match_fail("line 118");
  }
  return _t435;
}
function Enum$group_by(__dict_Map_1, __dict_Map_1_2, f, xs) {
  function _t439(m, x) {
    const k_440 = f(x);
    let _t443;
    const _t442 = __dict_Map_1_2.get(k_440, m);
    _t444: {
      if (_t442._tag === 0) {
        _t443 = __dict_Map_1_2.set(k_440, ({_hd: x, _tl: null}), m);
        break _t444;
      }
      if (_t442._tag === 1) {
        const vs = _t442._val;
        _t443 = __dict_Map_1_2.set(k_440, List$concat(vs, ({_hd: x, _tl: null})), m);
        break _t444;
      }
      _match_fail("line 118");
    }
    const _t441 = _t443;
    return _t441;
  }
  return List$fold(_t439, new Map([]), xs);
}
function Seq$range(start, stop, _mml$yield) {
  function go(i) {
    while (true) {
      let _t445;
      if ((i >= stop)) {
        _t445 = undefined;
      } else {
        _mml$yield(i);
        const _t446 = (i + 1);
        i = _t446;
        continue;
        _t445 = undefined;
      }
      return _t445;
    }
  }
  const _t447 = go(start);
  return _t447;
}
function Seq$of_list(xs, _mml$yield) {
  function go(l) {
    while (true) {
      let _t449;
      const _t448 = l;
      _t450: {
        if (_t448 === null) {
          _t449 = undefined;
          break _t450;
        }
        if (_t448 !== null) {
          const x = _t448._hd;
          const rest = _t448._tl;
          _mml$yield(x);
          const _t451 = rest;
          l = _t451;
          continue;
          _t449 = undefined;
          break _t450;
        }
        _match_fail("line 134");
      }
      return _t449;
    }
  }
  const _t452 = go(xs);
  return _t452;
}
function Seq$repeat(x, _mml$yield) {
  function go(u) {
    while (true) {
      _mml$yield(x);
      const _t453 = u;
      u = _t453;
      continue;
      return undefined;
    }
  }
  const _t454 = go(0);
  return _t454;
}
function Seq$iterate(seed, step, _mml$yield) {
  function go(x) {
    while (true) {
      _mml$yield(x);
      const _t455 = step(x);
      x = _t455;
      continue;
      return undefined;
    }
  }
  const _t456 = go(seed);
  return _t456;
}
function Seq$map(f, s, _mml$yield) {
  function _t457(x) {
    return _mml$yield(f(x));
  }
  return s(_t457);
}
function Seq$filter(f, s, _mml$yield) {
  function _t458(x) {
    let _t459;
    if (f(x)) {
      _t459 = _mml$yield(x);
    } else {
      _t459 = undefined;
    }
    return _t459;
  }
  return s(_t458);
}
function Seq$take(n, s, _mml$yield) {
  return _trampoline(function() {
    let i_460 = 0;
    const _t461 = (function() {
      const _t462 = _h;
      _h = Object.assign({}, _t462, {
        "__seq_stop": function(_, __k) { throw {_e: "__seq_stop", _v: _}; },
      });
      try {
        function _t463(x) {
          return _trampoline(function() {
            if ((i_460 >= n)) {
              return _h["__seq_stop"](undefined, function(_t464) {
                return _bounce(function() {
                  return _t464;
                });
              });
            } else {
              _mml$yield(x);
              return (i_460 = (i_460 + 1), undefined);
            }
          });
        }
        const __x_465 = s(_t463);
        return __x_465;
      } catch (_exc) {
        if (_exc && _exc._e === "__seq_stop") {
          const _ = _exc._v;
          return undefined;
        } else { throw _exc; }
      } finally { _h = _t462; }
    })();
    return _t461;
  }, Seq$take);
}
function Seq$take_while(f, s, _mml$yield) {
  return _trampoline(function() {
    const _t466 = (function() {
      const _t467 = _h;
      _h = Object.assign({}, _t467, {
        "__seq_stop": function(_, __k) { throw {_e: "__seq_stop", _v: _}; },
      });
      try {
        function _t468(x) {
          return _trampoline(function() {
            if (f(x)) {
              return _mml$yield(x);
            } else {
              return _h["__seq_stop"](undefined, function(_t469) {
                return _bounce(function() {
                  return _t469;
                });
              });
            }
          });
        }
        const __x_470 = s(_t468);
        return __x_470;
      } catch (_exc) {
        if (_exc && _exc._e === "__seq_stop") {
          const _ = _exc._v;
          return undefined;
        } else { throw _exc; }
      } finally { _h = _t467; }
    })();
    return _t466;
  }, Seq$take_while);
}
function Seq$drop(n, s, _mml$yield) {
  let i_471 = 0;
  function _t473(x) {
    let _t474;
    if ((i_471 >= n)) {
      _t474 = _mml$yield(x);
    } else {
      _t474 = (i_471 = (i_471 + 1), undefined);
    }
    return _t474;
  }
  const _t472 = s(_t473);
  return _t472;
}
function Seq$drop_while(f, s, _mml$yield) {
  let dropping_475 = true;
  function _t477(x) {
    let _t478;
    if (dropping_475) {
      let _t479;
      if (f(x)) {
        _t479 = undefined;
      } else {
        (dropping_475 = false, undefined);
        _t479 = _mml$yield(x);
      }
      _t478 = _t479;
    } else {
      _t478 = _mml$yield(x);
    }
    return _t478;
  }
  const _t476 = s(_t477);
  return _t476;
}
function Seq$flat_map(f, s, _mml$yield) {
  function _t480(x) {
    return f(x, _mml$yield);
  }
  return s(_t480);
}
function Seq$enumerate(s, _mml$yield) {
  let i_481 = 0;
  function _t483(x) {
    _mml$yield([i_481, x]);
    return (i_481 = (i_481 + 1), undefined);
  }
  const _t482 = s(_t483);
  return _t482;
}
function Seq$chunk(n, s, _mml$yield) {
  let buf_484 = null;
  let count_486 = 0;
  function _t488(x) {
    (buf_484 = List$concat(buf_484, ({_hd: x, _tl: null})), undefined);
    (count_486 = (count_486 + 1), undefined);
    let _t489;
    if ((count_486 >= n)) {
      _mml$yield(buf_484);
      (buf_484 = null, undefined);
      _t489 = (count_486 = 0, undefined);
    } else {
      _t489 = undefined;
    }
    return _t489;
  }
  s(_t488);
  let _t490;
  if ((count_486 > 0)) {
    _t490 = _mml$yield(buf_484);
  } else {
    _t490 = undefined;
  }
  const _t487 = _t490;
  const _t485 = _t487;
  return _t485;
}
function Seq$to_list(s) {
  let acc_491 = null;
  function _t493(x) {
    return (acc_491 = ({_hd: x, _tl: acc_491}), undefined);
  }
  s(_t493);
  const _t492 = List$rev(acc_491);
  return _t492;
}
function Seq$fold(f, init, s) {
  let acc_494 = init;
  function _t496(x) {
    return (acc_494 = f(acc_494, x), undefined);
  }
  s(_t496);
  const _t495 = acc_494;
  return _t495;
}
function Seq$each(f, s) {
  return s(f);
}
function Seq$count(s) {
  let n_497 = 0;
  function _t499(_) {
    return (n_497 = (n_497 + 1), undefined);
  }
  s(_t499);
  const _t498 = n_497;
  return _t498;
}
function Seq$sum(s) {
  let total_500 = 0;
  function _t502(x) {
    return (total_500 = (total_500 + x), undefined);
  }
  s(_t502);
  const _t501 = total_500;
  return _t501;
}
function Seq$find(f, s) {
  return _trampoline(function() {
    let result_503 = ({_tag: 0, _name: "None"});
    const _t504 = (function() {
      const _t505 = _h;
      _h = Object.assign({}, _t505, {
        "__seq_stop": function(_, __k) { throw {_e: "__seq_stop", _v: _}; },
      });
      try {
        function _t506(x) {
          return _trampoline(function() {
            if (f(x)) {
              (result_503 = ({_tag: 1, _name: "Some", _val: x}), undefined);
              return _h["__seq_stop"](undefined, function(_t507) {
                return _bounce(function() {
                  return _t507;
                });
              });
            } else {
              return undefined;
            }
          });
        }
        const __x_508 = s(_t506);
        return __x_508;
      } catch (_exc) {
        if (_exc && _exc._e === "__seq_stop") {
          const _ = _exc._v;
          return undefined;
        } else { throw _exc; }
      } finally { _h = _t505; }
    })();
    const _t509 = _t504;
    return result_503;
  }, Seq$find);
}
function Seq$any(f, s) {
  return _trampoline(function() {
    let result_510 = false;
    const _t511 = (function() {
      const _t512 = _h;
      _h = Object.assign({}, _t512, {
        "__seq_stop": function(_, __k) { throw {_e: "__seq_stop", _v: _}; },
      });
      try {
        function _t513(x) {
          return _trampoline(function() {
            if (f(x)) {
              (result_510 = true, undefined);
              return _h["__seq_stop"](undefined, function(_t514) {
                return _bounce(function() {
                  return _t514;
                });
              });
            } else {
              return undefined;
            }
          });
        }
        const __x_515 = s(_t513);
        return __x_515;
      } catch (_exc) {
        if (_exc && _exc._e === "__seq_stop") {
          const _ = _exc._v;
          return undefined;
        } else { throw _exc; }
      } finally { _h = _t512; }
    })();
    const _t516 = _t511;
    return result_510;
  }, Seq$any);
}
function Seq$all(f, s) {
  return _trampoline(function() {
    let result_517 = true;
    const _t518 = (function() {
      const _t519 = _h;
      _h = Object.assign({}, _t519, {
        "__seq_stop": function(_, __k) { throw {_e: "__seq_stop", _v: _}; },
      });
      try {
        function _t520(x) {
          return _trampoline(function() {
            if (f(x)) {
              return undefined;
            } else {
              (result_517 = false, undefined);
              return _h["__seq_stop"](undefined, function(_t521) {
                return _bounce(function() {
                  return _t521;
                });
              });
            }
          });
        }
        const __x_522 = s(_t520);
        return __x_522;
      } catch (_exc) {
        if (_exc && _exc._e === "__seq_stop") {
          const _ = _exc._v;
          return undefined;
        } else { throw _exc; }
      } finally { _h = _t519; }
    })();
    const _t523 = _t518;
    return result_517;
  }, Seq$all);
}
function Option$map(f, opt) {
  let _t525;
  const _t524 = opt;
  _t526: {
    if (_t524._tag === 1) {
      const x = _t524._val;
      _t525 = ({_tag: 1, _name: "Some", _val: f(x)});
      break _t526;
    }
    if (_t524._tag === 0) {
      _t525 = ({_tag: 0, _name: "None"});
      break _t526;
    }
    _match_fail("line 37");
  }
  return _t525;
}
function Option$bind(f, opt) {
  let _t528;
  const _t527 = opt;
  _t529: {
    if (_t527._tag === 1) {
      const x = _t527._val;
      _t528 = f(x);
      break _t529;
    }
    if (_t527._tag === 0) {
      _t528 = ({_tag: 0, _name: "None"});
      break _t529;
    }
    _match_fail("line 37");
  }
  return _t528;
}
function Option$unwrap(opt) {
  let _t531;
  const _t530 = opt;
  _t532: {
    if (_t530._tag === 1) {
      const x = _t530._val;
      _t531 = x;
      break _t532;
    }
    _match_fail("line 37");
  }
  return _t531;
}
function Option$unwrap_or(_mml$default, opt) {
  let _t534;
  const _t533 = opt;
  _t535: {
    if (_t533._tag === 1) {
      const x = _t533._val;
      _t534 = x;
      break _t535;
    }
    if (_t533._tag === 0) {
      _t534 = _mml$default;
      break _t535;
    }
    _match_fail("line 37");
  }
  return _t534;
}
function Option$is_some(opt) {
  let _t537;
  const _t536 = opt;
  _t538: {
    if (_t536._tag === 1) {
      _t537 = true;
      break _t538;
    }
    if (_t536._tag === 0) {
      _t537 = false;
      break _t538;
    }
    _match_fail("line 37");
  }
  return _t537;
}
function Option$is_none(opt) {
  let _t540;
  const _t539 = opt;
  _t541: {
    if (_t539._tag === 1) {
      _t540 = false;
      break _t541;
    }
    if (_t539._tag === 0) {
      _t540 = true;
      break _t541;
    }
    _match_fail("line 37");
  }
  return _t540;
}
function Option$to_list(opt) {
  let _t543;
  const _t542 = opt;
  _t544: {
    if (_t542._tag === 1) {
      const x = _t542._val;
      _t543 = ({_hd: x, _tl: null});
      break _t544;
    }
    if (_t542._tag === 0) {
      _t543 = null;
      break _t544;
    }
    _match_fail("line 37");
  }
  return _t543;
}
function Option$iter(f, opt) {
  let _t546;
  const _t545 = opt;
  _t547: {
    if (_t545._tag === 1) {
      const x = _t545._val;
      _t546 = f(x);
      break _t547;
    }
    if (_t545._tag === 0) {
      _t546 = undefined;
      break _t547;
    }
    _match_fail("line 37");
  }
  return _t546;
}
function Option$flat_map(f, opt) {
  let _t549;
  const _t548 = opt;
  _t550: {
    if (_t548._tag === 1) {
      const x = _t548._val;
      _t549 = f(x);
      break _t550;
    }
    if (_t548._tag === 0) {
      _t549 = ({_tag: 0, _name: "None"});
      break _t550;
    }
    _match_fail("line 37");
  }
  return _t549;
}
function Buffer$create(n) {
  let _t551;
  if ((n < 16)) {
    _t551 = 16;
  } else {
    _t551 = n;
  }
  return ({data: Array$make(_t551, 0), len: 0});
}
function Buffer$length(buf) {
  return buf.len;
}
function Buffer$clear(buf) {
  return (buf.len = 0, undefined);
}
function Buffer$grow(buf, needed) {
  const cap_552 = Array$length(buf.data);
  let _t554;
  if (((buf.len + needed) > cap_552)) {
    const new_cap_555 = Math$max((cap_552 * 2), (buf.len + needed));
    const new_data_557 = Array$make(new_cap_555, 0);
    function copy(i) {
      while (true) {
        let _t559;
        if ((i < buf.len)) {
          Array$set(new_data_557, i, Array$get(buf.data, i));
          const _t560 = (i + 1);
          i = _t560;
          continue;
          _t559 = undefined;
        } else {
          _t559 = undefined;
        }
        return _t559;
      }
    }
    copy(0);
    const _t561 = (buf.data = new_data_557, undefined);
    const _t558 = _t561;
    const _t556 = _t558;
    _t554 = _t556;
  } else {
    _t554 = undefined;
  }
  const _t553 = _t554;
  return _t553;
}
function Buffer$add_byte(buf, b) {
  Buffer$grow(buf, 1);
  Array$set(buf.data, buf.len, b);
  return (buf.len = (buf.len + 1), undefined);
}
function Buffer$add_string(buf, s) {
  const bytes_562 = String$to_byte_array(s);
  const n_564 = Array$length(bytes_562);
  Buffer$grow(buf, n_564);
  function copy(i) {
    while (true) {
      let _t566;
      if ((i < n_564)) {
        Array$set(buf.data, (buf.len + i), Array$get(bytes_562, i));
        const _t567 = (i + 1);
        i = _t567;
        continue;
        _t566 = undefined;
      } else {
        _t566 = undefined;
      }
      return _t566;
    }
  }
  copy(0);
  const _t568 = (buf.len = (buf.len + n_564), undefined);
  const _t565 = _t568;
  const _t563 = _t565;
  return _t563;
}
function Buffer$sub(buf, pos, len) {
  return String$of_byte_array(Array$sub(buf.data, pos, len));
}
function Buffer$truncate(buf, n) {
  return (buf.len = n, undefined);
}
function Buffer$contents(buf) {
  return String$of_byte_array(Array$sub(buf.data, 0, buf.len));
}
function Buffer$add_buffer(dst, src) {
  const n_569 = src.len;
  Buffer$grow(dst, n_569);
  function copy(i) {
    while (true) {
      let _t571;
      if ((i < n_569)) {
        Array$set(dst.data, (dst.len + i), Array$get(src.data, i));
        const _t572 = (i + 1);
        i = _t572;
        continue;
        _t571 = undefined;
      } else {
        _t571 = undefined;
      }
      return _t571;
    }
  }
  copy(0);
  const _t573 = (dst.len = (dst.len + n_569), undefined);
  const _t570 = _t573;
  return _t570;
}
function Fmt$pad_left(n, c, s) {
  const len_574 = String$length(s);
  let _t576;
  if ((len_574 >= n)) {
    _t576 = s;
  } else {
    function pad(acc, remaining) {
      while (true) {
        let _t577;
        if ((remaining <= 0)) {
          _t577 = (acc + s);
        } else {
          const _t578 = (acc + c);
          const _t579 = (remaining - 1);
          acc = _t578;
          remaining = _t579;
          continue;
          _t577 = undefined;
        }
        return _t577;
      }
    }
    const _t580 = pad("", (n - len_574));
    _t576 = _t580;
  }
  const _t575 = _t576;
  return _t575;
}
function Fmt$pad_right(n, c, s) {
  const len_581 = String$length(s);
  let _t583;
  if ((len_581 >= n)) {
    _t583 = s;
  } else {
    function pad(acc, remaining) {
      while (true) {
        let _t584;
        if ((remaining <= 0)) {
          _t584 = (s + acc);
        } else {
          const _t585 = (acc + c);
          const _t586 = (remaining - 1);
          acc = _t585;
          remaining = _t586;
          continue;
          _t584 = undefined;
        }
        return _t584;
      }
    }
    const _t587 = pad("", (n - len_581));
    _t583 = _t587;
  }
  const _t582 = _t583;
  return _t582;
}
function Fmt$int_to_hex(n) {
  const digits_588 = "0123456789abcdef";
  let _t590;
  if ((n === 0)) {
    _t590 = "0";
  } else {
    function go(num, acc) {
      while (true) {
        let _t591;
        if ((num === 0)) {
          _t591 = acc;
        } else {
          const d_592 = (num % 16);
          let _t594;
          if ((d_592 < 0)) {
            _t594 = (d_592 + 16);
          } else {
            _t594 = d_592;
          }
          const d_595 = _t594;
          const ch_597 = String$sub(digits_588, d_595, 1);
          const _t599 = ((num / 16) | 0);
          const _t600 = (ch_597 + acc);
          num = _t599;
          acc = _t600;
          continue;
          const _t598 = undefined;
          const _t596 = _t598;
          const _t593 = _t596;
          _t591 = _t593;
        }
        return _t591;
      }
    }
    let _t602;
    if ((n < 0)) {
      _t602 = (0 - n);
    } else {
      _t602 = n;
    }
    const abs_n_603 = _t602;
    const hex_605 = go(abs_n_603, "");
    let _t607;
    if ((n < 0)) {
      _t607 = ("-" + hex_605);
    } else {
      _t607 = hex_605;
    }
    const _t606 = _t607;
    const _t604 = _t606;
    const _t601 = _t604;
    _t590 = _t601;
  }
  const _t589 = _t590;
  return _t589;
}
function Fmt$int_to_bin(n) {
  let _t608;
  if ((n === 0)) {
    _t608 = "0";
  } else {
    function go(num, acc) {
      while (true) {
        let _t609;
        if ((num === 0)) {
          _t609 = acc;
        } else {
          let _t610;
          if (((num % 2) === 0)) {
            _t610 = "0";
          } else {
            _t610 = "1";
          }
          const bit_611 = _t610;
          const _t613 = ((num / 2) | 0);
          const _t614 = (bit_611 + acc);
          num = _t613;
          acc = _t614;
          continue;
          const _t612 = undefined;
          _t609 = _t612;
        }
        return _t609;
      }
    }
    let _t616;
    if ((n < 0)) {
      _t616 = (0 - n);
    } else {
      _t616 = n;
    }
    const abs_n_617 = _t616;
    const bin_619 = go(abs_n_617, "");
    let _t621;
    if ((n < 0)) {
      _t621 = ("-" + bin_619);
    } else {
      _t621 = bin_619;
    }
    const _t620 = _t621;
    const _t618 = _t620;
    const _t615 = _t618;
    _t608 = _t615;
  }
  return _t608;
}
function Fmt$int_to_oct(n) {
  let _t622;
  if ((n === 0)) {
    _t622 = "0";
  } else {
    function go(num, acc) {
      while (true) {
        let _t623;
        if ((num === 0)) {
          _t623 = acc;
        } else {
          const d_624 = (num % 8);
          let _t626;
          if ((d_624 < 0)) {
            _t626 = (d_624 + 8);
          } else {
            _t626 = d_624;
          }
          const d_627 = _t626;
          const _t629 = ((num / 8) | 0);
          const _t630 = (__dict_Show_int.show(d_627) + acc);
          num = _t629;
          acc = _t630;
          continue;
          const _t628 = undefined;
          const _t625 = _t628;
          _t623 = _t625;
        }
        return _t623;
      }
    }
    let _t632;
    if ((n < 0)) {
      _t632 = (0 - n);
    } else {
      _t632 = n;
    }
    const abs_n_633 = _t632;
    const oct_635 = go(abs_n_633, "");
    let _t637;
    if ((n < 0)) {
      _t637 = ("-" + oct_635);
    } else {
      _t637 = oct_635;
    }
    const _t636 = _t637;
    const _t634 = _t636;
    const _t631 = _t634;
    _t622 = _t631;
  }
  return _t622;
}
function Fmt$zero_pad(width, s) {
  return Fmt$pad_left(width, "0", s);
}
function _t638(n) {
  return n;
}
const __dict_Hash_int = ({hash: _t638});
function _t639(s) {
  const bytes_640 = String$to_byte_array(s);
  function _t642(h, b) {
    return ((h * 31) + Byte$to_int(b));
  }
  const _t641 = __dict_Iter_g0_array__g0.fold(_t642, 5381, bytes_640);
  return _t641;
}
const __dict_Hash_string = ({hash: _t639});
function _t643(b) {
  let _t644;
  if (b) {
    _t644 = 1;
  } else {
    _t644 = 0;
  }
  return _t644;
}
const __dict_Hash_bool = ({hash: _t643});
function _t645(b) {
  return Byte$to_int(b);
}
const __dict_Hash_byte = ({hash: _t645});
function _t646(r) {
  return Rune$to_int(r);
}
const __dict_Hash_rune = ({hash: _t646});
function Hashtbl$create(n) {
  let _t647;
  if ((n < 16)) {
    _t647 = 16;
  } else {
    _t647 = n;
  }
  const cap_648 = _t647;
  const _t649 = ({buckets: Array$make(cap_648, null), size: 0});
  return _t649;
}
function Hashtbl$clear(tbl) {
  const cap_650 = Array$length(tbl.buckets);
  (tbl.buckets = Array$make(cap_650, null), undefined);
  const _t651 = (tbl.size = 0, undefined);
  return _t651;
}
function Hashtbl$length(tbl) {
  return tbl.size;
}
function Hashtbl$bucket_index(__dict_Hash_0, tbl, key) {
  const h_652 = __dict_Hash_0.hash(key);
  let _t654;
  if ((h_652 < 0)) {
    _t654 = (0 - h_652);
  } else {
    _t654 = h_652;
  }
  const h_655 = _t654;
  const _t656 = (h_655 % Array$length(tbl.buckets));
  const _t653 = _t656;
  return _t653;
}
function Hashtbl$to_list(tbl) {
  const cap_657 = Array$length(tbl.buckets);
  function collect(i, acc) {
    while (true) {
      let _t659;
      if ((i >= cap_657)) {
        _t659 = acc;
      } else {
        const bucket_660 = Array$get(tbl.buckets, i);
        const _t662 = (i + 1);
        const _t663 = List$concat(bucket_660, acc);
        i = _t662;
        acc = _t663;
        continue;
        const _t661 = undefined;
        _t659 = _t661;
      }
      return _t659;
    }
  }
  const _t664 = collect(0, null);
  const _t658 = _t664;
  return _t658;
}
function Hashtbl$rehash(tbl, hash_fn) {
  const entries_665 = Hashtbl$to_list(tbl);
  const new_cap_667 = (Array$length(tbl.buckets) * 2);
  (tbl.buckets = Array$make(new_cap_667, null), undefined);
  (tbl.size = 0, undefined);
  function _t669(_, __p1) {
    let _t671;
    const _t670 = __p1;
    _t672: {
      if (true) {
        const k = _t670[0];
        const v = _t670[1];
        const h_673 = hash_fn(k);
        let _t675;
        if ((h_673 < 0)) {
          _t675 = (0 - h_673);
        } else {
          _t675 = h_673;
        }
        const h_676 = _t675;
        const idx_678 = (h_676 % new_cap_667);
        const bucket_680 = Array$get(tbl.buckets, idx_678);
        Array$set(tbl.buckets, idx_678, ({_hd: [k, v], _tl: bucket_680}));
        const _t681 = (tbl.size = (tbl.size + 1), undefined);
        const _t679 = _t681;
        const _t677 = _t679;
        const _t674 = _t677;
        _t671 = _t674;
        break _t672;
      }
      _match_fail("line 106");
    }
    return _t671;
  }
  const _t668 = List$fold(_t669, undefined, entries_665);
  const _t666 = _t668;
  return _t666;
}
function Hashtbl$set(__dict_Hash_1, __dict_Eq_1, tbl, key, value) {
  const idx_682 = _call(Hashtbl$bucket_index, [__dict_Hash_1, tbl, key]);
  const bucket_684 = Array$get(tbl.buckets, idx_682);
  function replace(__x) {
    let _t687;
    const _t686 = __x;
    _t688: {
      if (_t686 === null) {
        _t687 = ({_hd: [key, value], _tl: null});
        break _t688;
      }
      if (_t686 !== null) {
        const k = _t686._hd[0];
        const v = _t686._hd[1];
        const rest = _t686._tl;
        let _t689;
        if (_call(__dict_Eq_1.$eq, [k, key])) {
          _t689 = ({_hd: [key, value], _tl: rest});
        } else {
          _t689 = ({_hd: [k, v], _tl: replace(rest)});
        }
        _t687 = _t689;
        break _t688;
      }
      _match_fail("line 106");
    }
    return _t687;
  }
  const new_bucket_691 = replace(bucket_684);
  const grew_693 = (List$length(new_bucket_691) > List$length(bucket_684));
  Array$set(tbl.buckets, idx_682, new_bucket_691);
  let _t695;
  if (grew_693) {
    (tbl.size = (tbl.size + 1), undefined);
    let _t696;
    if ((tbl.size > (Array$length(tbl.buckets) * 2))) {
      _t696 = Hashtbl$rehash(tbl, __dict_Hash_1.hash);
    } else {
      _t696 = undefined;
    }
    _t695 = _t696;
  } else {
    _t695 = undefined;
  }
  const _t694 = _t695;
  const _t692 = _t694;
  const _t690 = _t692;
  const _t685 = _t690;
  const _t683 = _t685;
  return _t683;
}
function Hashtbl$get(__dict_Hash_1, __dict_Eq_1, tbl, key) {
  const idx_697 = _call(Hashtbl$bucket_index, [__dict_Hash_1, tbl, key]);
  const bucket_699 = Array$get(tbl.buckets, idx_697);
  function find(__x) {
    while (true) {
      let _t702;
      const _t701 = __x;
      _t703: {
        if (_t701 === null) {
          _t702 = ({_tag: 0, _name: "None"});
          break _t703;
        }
        if (_t701 !== null) {
          const k = _t701._hd[0];
          const v = _t701._hd[1];
          const rest = _t701._tl;
          let _t704;
          if (_call(__dict_Eq_1.$eq, [k, key])) {
            _t704 = ({_tag: 1, _name: "Some", _val: v});
          } else {
            const _t705 = rest;
            __x = _t705;
            continue;
            _t704 = undefined;
          }
          _t702 = _t704;
          break _t703;
        }
        _match_fail("line 106");
      }
      return _t702;
    }
  }
  const _t706 = find(bucket_699);
  const _t700 = _t706;
  const _t698 = _t700;
  return _t698;
}
function Hashtbl$has(__dict_Hash_0, __dict_Eq_0, tbl, key) {
  let _t708;
  const _t707 = _call(Hashtbl$get, [__dict_Hash_0, __dict_Eq_0, tbl, key]);
  _t709: {
    if (_t707._tag === 1) {
      _t708 = true;
      break _t709;
    }
    if (_t707._tag === 0) {
      _t708 = false;
      break _t709;
    }
    _match_fail("line 106");
  }
  return _t708;
}
function Hashtbl$remove(__dict_Hash_0, __dict_Eq_0, tbl, key) {
  const idx_710 = _call(Hashtbl$bucket_index, [__dict_Hash_0, tbl, key]);
  const bucket_712 = Array$get(tbl.buckets, idx_710);
  function _t714(__p2) {
    let _t716;
    const _t715 = __p2;
    _t717: {
      if (true) {
        const k = _t715[0];
        _t716 = _call(__dict_Eq_0.$lt$gt, [k, key]);
        break _t717;
      }
      _match_fail("line 106");
    }
    return _t716;
  }
  const new_bucket_718 = List$filter(_t714, bucket_712);
  let _t720;
  if ((List$length(new_bucket_718) < List$length(bucket_712))) {
    _t720 = (tbl.size = (tbl.size - 1), undefined);
  } else {
    _t720 = undefined;
  }
  _t720;
  const _t719 = Array$set(tbl.buckets, idx_710, new_bucket_718);
  const _t713 = _t719;
  const _t711 = _t713;
  return _t711;
}
function Hashtbl$find(__dict_Hash_1, __dict_Eq_1, tbl, key) {
  let _t722;
  const _t721 = _call(Hashtbl$get, [__dict_Hash_1, __dict_Eq_1, tbl, key]);
  _t723: {
    if (_t721._tag === 1) {
      const v = _t721._val;
      _t722 = v;
      break _t723;
    }
    if (_t721._tag === 0) {
      _t722 = failwith("Hashtbl.find: key not found");
      break _t723;
    }
    _match_fail("line 106");
  }
  return _t722;
}
function Hashtbl$fold(f, tbl, acc) {
  const entries_724 = Hashtbl$to_list(tbl);
  function _t726(a, __p3) {
    let _t728;
    const _t727 = __p3;
    _t729: {
      if (true) {
        const k = _t727[0];
        const v = _t727[1];
        _t728 = f(k, v, a);
        break _t729;
      }
      _match_fail("line 106");
    }
    return _t728;
  }
  const _t725 = List$fold(_t726, acc, entries_724);
  return _t725;
}
function Hashtbl$iter(f, tbl) {
  const entries_730 = Hashtbl$to_list(tbl);
  function _t732(__p4) {
    let _t734;
    const _t733 = __p4;
    _t735: {
      if (true) {
        const k = _t733[0];
        const v = _t733[1];
        _t734 = f(k, v);
        break _t735;
      }
      _match_fail("line 106");
    }
    return _t734;
  }
  const _t731 = List$iter(_t732, entries_730);
  return _t731;
}
function Hashtbl$keys(tbl) {
  function _t736(__p5) {
    let _t738;
    const _t737 = __p5;
    _t739: {
      if (true) {
        const k = _t737[0];
        _t738 = k;
        break _t739;
      }
      _match_fail("line 106");
    }
    return _t738;
  }
  return List$map(_t736, Hashtbl$to_list(tbl));
}
function Hashtbl$values(tbl) {
  function _t740(__p6) {
    let _t742;
    const _t741 = __p6;
    _t743: {
      if (true) {
        const v = _t741[1];
        _t742 = v;
        break _t743;
      }
      _match_fail("line 106");
    }
    return _t742;
  }
  return List$map(_t740, Hashtbl$to_list(tbl));
}
function Ref$create(v) {
  return ({contents: v});
}
function Ref$get(r) {
  return r.contents;
}
function Ref$set(r, v) {
  return (r.contents = v, undefined);
}
function Dynarray$create(n, _mml$default) {
  let _t744;
  if ((n < 16)) {
    _t744 = 16;
  } else {
    _t744 = n;
  }
  return ({arr: Array$make(_t744, _mml$default), count: 0});
}
function Dynarray$length(d) {
  return d.count;
}
function Dynarray$get(d, i) {
  let _t745;
  if (((i < 0) || (i >= d.count))) {
    _t745 = failwith("Dynarray.get: index out of bounds");
  } else {
    _t745 = Array$get(d.arr, i);
  }
  return _t745;
}
function Dynarray$set(d, i, v) {
  let _t746;
  if (((i < 0) || (i >= d.count))) {
    _t746 = failwith("Dynarray.set: index out of bounds");
  } else {
    _t746 = Array$set(d.arr, i, v);
  }
  return _t746;
}
function Dynarray$grow(d, needed, _mml$default) {
  const cap_747 = Array$length(d.arr);
  let _t749;
  if (((d.count + needed) > cap_747)) {
    const new_cap_750 = Math$max((cap_747 * 2), (d.count + needed));
    const new_arr_752 = Array$make(new_cap_750, _mml$default);
    function copy(i) {
      while (true) {
        let _t754;
        if ((i < d.count)) {
          Array$set(new_arr_752, i, Array$get(d.arr, i));
          const _t755 = (i + 1);
          i = _t755;
          continue;
          _t754 = undefined;
        } else {
          _t754 = undefined;
        }
        return _t754;
      }
    }
    copy(0);
    const _t756 = (d.arr = new_arr_752, undefined);
    const _t753 = _t756;
    const _t751 = _t753;
    _t749 = _t751;
  } else {
    _t749 = undefined;
  }
  const _t748 = _t749;
  return _t748;
}
function Dynarray$empty(_) {
  return ({arr: {_arr: []}, count: 0});
}
function Dynarray$push(d, v) {
  Dynarray$grow(d, 1, v);
  Array$set(d.arr, d.count, v);
  return (d.count = (d.count + 1), undefined);
}
function Dynarray$pop(d) {
  let _t757;
  if ((d.count === 0)) {
    _t757 = failwith("Dynarray.pop: empty");
  } else {
    _t757 = (d.count = (d.count - 1), undefined);
  }
  _t757;
  return Array$get(d.arr, d.count);
}
function Dynarray$clear(d) {
  return (d.count = 0, undefined);
}
function Dynarray$to_list(d) {
  function collect(i, acc) {
    while (true) {
      let _t758;
      if ((i < 0)) {
        _t758 = acc;
      } else {
        const _t759 = (i - 1);
        const _t760 = ({_hd: Dynarray$get(d, i), _tl: acc});
        i = _t759;
        acc = _t760;
        continue;
        _t758 = undefined;
      }
      return _t758;
    }
  }
  const _t761 = collect((d.count - 1), null);
  return _t761;
}
function Dynarray$to_array(d) {
  return Array$sub(d.arr, 0, d.count);
}
function compare(a, b) {
  let _t762;
  if ((a < b)) {
    _t762 = (-1);
  } else {
    let _t763;
    if ((a > b)) {
      _t763 = 1;
    } else {
      _t763 = 0;
    }
    _t762 = _t763;
  }
  return _t762;
}
function int_of_string(s) {
  let _t765;
  const _t764 = String$to_int(s);
  _t766: {
    if (_t764._tag === 1) {
      const n = _t764._val;
      _t765 = n;
      break _t766;
    }
    if (_t764._tag === 0) {
      _t765 = failwith((("int_of_string: invalid argument \"" + __dict_Show_string.show(s)) + "\""));
      break _t766;
    }
    _match_fail("line 27");
  }
  return _t765;
}
function float_of_string(s) {
  let _t768;
  const _t767 = String$to_float(s);
  _t769: {
    if (_t767._tag === 1) {
      const f = _t767._val;
      _t768 = f;
      break _t769;
    }
    if (_t767._tag === 0) {
      _t768 = failwith((("float_of_string: invalid argument \"" + __dict_Show_string.show(s)) + "\""));
      break _t769;
    }
    _match_fail("line 27");
  }
  return _t768;
}
function max(a, b) {
  let _t770;
  if ((a > b)) {
    _t770 = a;
  } else {
    _t770 = b;
  }
  return _t770;
}
function min(a, b) {
  let _t771;
  if ((a < b)) {
    _t771 = a;
  } else {
    _t771 = b;
  }
  return _t771;
}
function fst(__p7) {
  let _t773;
  const _t772 = __p7;
  _t774: {
    if (true) {
      const a = _t772[0];
      _t773 = a;
      break _t774;
    }
    _match_fail("line 27");
  }
  return _t773;
}
function snd(__p8) {
  let _t776;
  const _t775 = __p8;
  _t777: {
    if (true) {
      const b = _t775[1];
      _t776 = b;
      break _t777;
    }
    _match_fail("line 27");
  }
  return _t776;
}
function list_find(f, xs) {
  let _t779;
  const _t778 = List$find(f, xs);
  _t780: {
    if (_t778._tag === 1) {
      const x = _t778._val;
      _t779 = x;
      break _t780;
    }
    if (_t778._tag === 0) {
      _t779 = failwith("list_find: not found");
      break _t780;
    }
    _match_fail("line 27");
  }
  return _t779;
}
if (typeof Runtime$eval === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Runtime.eval"]) var Runtime$eval = globalThis._mmlExterns["Runtime.eval"]; else throw new Error("extern " + "Runtime.eval" + " not provided"); }
if (typeof Runtime$eval_file === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Runtime.eval_file"]) var Runtime$eval_file = globalThis._mmlExterns["Runtime.eval_file"]; else throw new Error("extern " + "Runtime.eval_file" + " not provided"); }
// --- Compiled MiniML ---
let _last_val;
if (typeof Canvas$init === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.init"]) var Canvas$init = globalThis._mmlExterns["Canvas.init"]; else throw new Error("extern " + "Canvas.init" + " not provided"); }
if (typeof Canvas$clear === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.clear"]) var Canvas$clear = globalThis._mmlExterns["Canvas.clear"]; else throw new Error("extern " + "Canvas.clear" + " not provided"); }
if (typeof Canvas$fill_rect === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.fill_rect"]) var Canvas$fill_rect = globalThis._mmlExterns["Canvas.fill_rect"]; else throw new Error("extern " + "Canvas.fill_rect" + " not provided"); }
if (typeof Canvas$stroke_rect === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.stroke_rect"]) var Canvas$stroke_rect = globalThis._mmlExterns["Canvas.stroke_rect"]; else throw new Error("extern " + "Canvas.stroke_rect" + " not provided"); }
if (typeof Canvas$fill_circle === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.fill_circle"]) var Canvas$fill_circle = globalThis._mmlExterns["Canvas.fill_circle"]; else throw new Error("extern " + "Canvas.fill_circle" + " not provided"); }
if (typeof Canvas$draw_text === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.draw_text"]) var Canvas$draw_text = globalThis._mmlExterns["Canvas.draw_text"]; else throw new Error("extern " + "Canvas.draw_text" + " not provided"); }
if (typeof Canvas$set_font === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.set_font"]) var Canvas$set_font = globalThis._mmlExterns["Canvas.set_font"]; else throw new Error("extern " + "Canvas.set_font" + " not provided"); }
if (typeof Canvas$mouse_x === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.mouse_x"]) var Canvas$mouse_x = globalThis._mmlExterns["Canvas.mouse_x"]; else throw new Error("extern " + "Canvas.mouse_x" + " not provided"); }
if (typeof Canvas$mouse_y === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.mouse_y"]) var Canvas$mouse_y = globalThis._mmlExterns["Canvas.mouse_y"]; else throw new Error("extern " + "Canvas.mouse_y" + " not provided"); }
if (typeof Canvas$mouse_down === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.mouse_down"]) var Canvas$mouse_down = globalThis._mmlExterns["Canvas.mouse_down"]; else throw new Error("extern " + "Canvas.mouse_down" + " not provided"); }
if (typeof Canvas$mouse_clicked === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.mouse_clicked"]) var Canvas$mouse_clicked = globalThis._mmlExterns["Canvas.mouse_clicked"]; else throw new Error("extern " + "Canvas.mouse_clicked" + " not provided"); }
if (typeof Canvas$start_app === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["Canvas.start_app"]) var Canvas$start_app = globalThis._mmlExterns["Canvas.start_app"]; else throw new Error("extern " + "Canvas.start_app" + " not provided"); }
if (typeof __math_sin === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["__math_sin"]) var __math_sin = globalThis._mmlExterns["__math_sin"]; else throw new Error("extern " + "__math_sin" + " not provided"); }
if (typeof __math_cos === "undefined") { if (globalThis._mmlExterns && globalThis._mmlExterns["__math_cos"]) var __math_cos = globalThis._mmlExterns["__math_cos"]; else throw new Error("extern " + "__math_cos" + " not provided"); }
const pi = 3.1415926500000002;
function button(x, y, w, h, label) {
  const mx_781 = Canvas$mouse_x(undefined);
  const my_783 = Canvas$mouse_y(undefined);
  const hover_785 = ((((mx_781 >= x) && (mx_781 < (x + w))) && (my_783 >= y)) && (my_783 < (y + h)));
  let _t787;
  if (hover_785) {
    _t787 = "#3D4555";
  } else {
    _t787 = "#2A3040";
  }
  Canvas$fill_rect(x, y, w, h, _t787);
  let _t788;
  if (hover_785) {
    _t788 = Canvas$stroke_rect(x, y, w, h, "#FFCC66");
  } else {
    _t788 = undefined;
  }
  _t788;
  Canvas$set_font("15px monospace");
  let _t789;
  if (hover_785) {
    _t789 = "#FFCC66";
  } else {
    _t789 = "#CCCAC2";
  }
  Canvas$draw_text(label, (x + 10.0), (y + 8.0), _t789);
  const _t786 = (hover_785 && Canvas$mouse_clicked(undefined));
  const _t784 = _t786;
  const _t782 = _t784;
  return _t782;
}
function init(_) {
  Canvas$init(500, 400);
  return ({count: 0, frame: 0, message: "Click the buttons!"});
}
function frame(s) {
  Canvas$clear("#1a202c");
  Canvas$set_font("bold 24px sans-serif");
  Canvas$draw_text("MiniML Canvas Demo", 20.0, 16.0, "#FFCC66");
  Canvas$set_font("20px monospace");
  Canvas$draw_text(("Count: " + __dict_Show_int.show(s.count)), 20.0, 60.0, "#E2E8F0");
  const inc_790 = button(20.0, 92.0, 100.0, 32.0, "  + Add");
  const dec_792 = button(130.0, 92.0, 100.0, 32.0, "  - Sub");
  const rst_794 = button(240.0, 92.0, 100.0, 32.0, "  Reset");
  const bar_w_796 = 340.0;
  const fill_798 = (float_of_int((s.count % 21)) / 20.0);
  let _t800;
  if ((fill_798 < 0.0)) {
    _t800 = (0.0 - fill_798);
  } else {
    _t800 = fill_798;
  }
  const fill_801 = _t800;
  Canvas$fill_rect(20.0, 140.0, bar_w_796, 16.0, "#2D3748");
  let _t803;
  if ((s.count >= 0)) {
    _t803 = "#48BB78";
  } else {
    _t803 = "#FC8181";
  }
  Canvas$fill_rect(20.0, 140.0, (bar_w_796 * fill_801), 16.0, _t803);
  Canvas$stroke_rect(20.0, 140.0, bar_w_796, 16.0, "#4A5568");
  const t_804 = (float_of_int(s.frame) / 60.0);
  const cx_806 = 250.0;
  const cy_808 = 250.0;
  Canvas$stroke_rect((cx_806 - 80.0), (cy_808 - 80.0), 160.0, 160.0, "#2D3748");
  Canvas$fill_circle((cx_806 + (70.0 * __math_cos((t_804 * 2.0)))), (cy_808 + (70.0 * __math_sin((t_804 * 2.0)))), 8.0, "#63B3ED");
  Canvas$fill_circle((cx_806 + (70.0 * __math_cos(((t_804 * 2.0) + 2.0939999999999999)))), (cy_808 + (70.0 * __math_sin(((t_804 * 2.0) + 2.0939999999999999)))), 8.0, "#FC8181");
  Canvas$fill_circle((cx_806 + (70.0 * __math_cos(((t_804 * 2.0) + 4.1890000000000001)))), (cy_808 + (70.0 * __math_sin(((t_804 * 2.0) + 4.1890000000000001)))), 8.0, "#68D391");
  const mx_810 = Canvas$mouse_x(undefined);
  const my_812 = Canvas$mouse_y(undefined);
  Canvas$fill_circle(mx_810, my_812, 6.0, "#FFCC66");
  Canvas$set_font("11px monospace");
  Canvas$draw_text((((("(" + __dict_Show_int.show(int_of_float(mx_810))) + ", ") + __dict_Show_int.show(int_of_float(my_812))) + ")"), (mx_810 + 10.0), (my_812 - 14.0), "#A0AEC0");
  Canvas$set_font("13px sans-serif");
  Canvas$draw_text(s.message, 20.0, 375.0, "#718096");
  let _t814;
  if (inc_790) {
    _t814 = (s.count + 1);
  } else {
    let _t815;
    if (dec_792) {
      _t815 = (s.count - 1);
    } else {
      let _t816;
      if (rst_794) {
        _t816 = 0;
      } else {
        _t816 = s.count;
      }
      _t815 = _t816;
    }
    _t814 = _t815;
  }
  const new_count_817 = _t814;
  let _t819;
  if (inc_790) {
    _t819 = ("Incremented to " + __dict_Show_int.show(new_count_817));
  } else {
    let _t820;
    if (dec_792) {
      _t820 = ("Decremented to " + __dict_Show_int.show(new_count_817));
    } else {
      let _t821;
      if (rst_794) {
        _t821 = "Reset to zero";
      } else {
        _t821 = s.message;
      }
      _t820 = _t821;
    }
    _t819 = _t820;
  }
  const new_msg_822 = _t819;
  const _t823 = ({count: new_count_817, frame: (s.frame + 1), message: new_msg_822});
  const _t818 = _t823;
  const _t813 = _t818;
  const _t811 = _t813;
  const _t809 = _t811;
  const _t807 = _t809;
  const _t805 = _t807;
  const _t802 = _t805;
  const _t799 = _t802;
  const _t797 = _t799;
  const _t795 = _t797;
  const _t793 = _t795;
  const _t791 = _t793;
  return _t791;
}
const __destruct = Canvas$start_app(init, frame);
if (_last_val !== undefined) println(_pp(_last_val));
var _mml_exports = {"_result": _last_val, "_call": _call, "_pp": _pp, "pi": pi, "button": button, "init": init, "frame": frame};
if (typeof globalThis !== "undefined") globalThis._mmlExports = _mml_exports;
