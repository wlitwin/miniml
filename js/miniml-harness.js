/**
 * MiniML Browser Harness
 *
 * Provides a clean API for running compiled MiniML JS on web pages.
 * Works with the output of `--emit-js` compilation.
 *
 * Usage:
 *   const app = MiniML.run(compiledJs, { onOutput: s => console.log(s) });
 *   app.call("add", 1, 2);  // call exported MiniML functions
 */

class MiniMLApp {
  constructor(exports, output) {
    this._exports = exports || {};
    this._call = exports ? exports._call : null;
    this._ppFn = exports ? exports._pp : String;
    this.output = output;
  }

  /** The program's final expression value (raw MiniML representation). */
  get result() {
    return this._exports._result;
  }

  /**
   * Call an exported MiniML function by name.
   * Handles partial application automatically.
   *
   *   app.call("add", 1, 2)       // fully applied
   *   app.call("add", 1)          // returns a partially applied function
   */
  call(name, ...args) {
    const fn = this._exports[name];
    if (fn === undefined) {
      throw new Error("MiniML: no export named '" + name + "'");
    }
    if (args.length === 0) return fn;
    return this._call(fn, args.slice());
  }

  /** Pretty-print a MiniML value as a string. */
  pp(value) {
    return this._ppFn(value);
  }

  /**
   * Convert a MiniML runtime value to idiomatic JavaScript.
   *
   * - unit (undefined) -> null
   * - empty list (null) -> []
   * - list ({_hd, _tl}) -> Array
   * - array ({_arr}) -> Array
   * - tuple (JS Array) -> Array
   * - variant ({_tag, _name, _val}) -> {tag, value}
   * - Map -> plain Object
   * - record -> plain Object
   * - number/string/bool -> pass-through
   */
  toJS(value) {
    if (value === undefined) return null;
    if (value === null) return [];
    if (typeof value !== "object") return value;
    if (Array.isArray(value)) return value.map(v => this.toJS(v));
    if ("_hd" in value) {
      const arr = [];
      let c = value;
      while (c !== null && typeof c === "object" && "_hd" in c) {
        arr.push(this.toJS(c._hd));
        c = c._tl;
      }
      return arr;
    }
    if ("_arr" in value) return value._arr.map(v => this.toJS(v));
    if ("_tag" in value) {
      return value._val !== undefined
        ? { tag: value._name, value: this.toJS(value._val) }
        : { tag: value._name };
    }
    if ("_ref" in value) return this.toJS(value._ref);
    if (value instanceof Map) {
      const obj = {};
      for (const [k, v] of value.entries()) obj[k] = this.toJS(v);
      return obj;
    }
    const obj = {};
    for (const [k, v] of Object.entries(value)) obj[k] = this.toJS(v);
    return obj;
  }

  /**
   * Convert a JavaScript value to MiniML runtime representation.
   * Arrays become MiniML lists by default. Use tuple() or array()
   * for other collection types.
   *
   * - null/undefined -> unit (undefined)
   * - Array -> MiniML list ({_hd, _tl} chain)
   * - plain Object -> MiniML record
   * - number/string/bool -> pass-through
   */
  fromJS(value) {
    if (value === null || value === undefined) return undefined;
    if (typeof value !== "object") return value;
    if (Array.isArray(value)) {
      let list = null;
      for (let i = value.length - 1; i >= 0; i--)
        list = { _hd: this.fromJS(value[i]), _tl: list };
      return list;
    }
    const rec = {};
    for (const [k, v] of Object.entries(value)) rec[k] = this.fromJS(v);
    return rec;
  }

  /** Create a MiniML list from JS values. Same as fromJS(arr). */
  list(arr) {
    let list = null;
    for (let i = arr.length - 1; i >= 0; i--)
      list = { _hd: this.fromJS(arr[i]), _tl: list };
    return list;
  }

  /** Create a MiniML tuple from JS values. */
  tuple(arr) {
    return arr.map(v => this.fromJS(v));
  }

  /** Create a MiniML array from JS values. */
  array(arr) {
    return { _arr: arr.map(v => this.fromJS(v)) };
  }

  /**
   * The unit value (MiniML's ()).
   * Useful when calling MiniML functions that take unit, e.g.:
   *   app.call("get_state", MiniMLApp.unit)
   */
  static get unit() { return undefined; }
}

const MiniML = {
  /**
   * Run compiled MiniML JavaScript and return an app handle.
   *
   * @param {string} js - The compiled JS (output of --emit-js)
   * @param {Object} [options]
   * @param {function} [options.onOutput] - Callback for print/println (receives string).
   *   If not provided, output is collected into app.output.
   * @param {Object} [options.externs] - Custom extern implementations.
   *   Keys are MiniML names like "Dom.set_text", values are JS functions.
   * @param {string[]} [options.args] - Arguments for Sys.args().
   * @param {function} [options.readFile] - Callback for IO.read_file (receives path, returns string).
   * @returns {MiniMLApp}
   */
  run(js, options) {
    options = options || {};
    const output = [];
    globalThis._jsOutput = options.onOutput || function(s) { output.push(s); };
    if (options.args) globalThis._jsSysArgs = options.args;
    if (options.readFile) globalThis._jsReadFile = options.readFile;
    if (options.externs) globalThis._mmlExterns = options.externs;

    try {
      new Function(js)();
      const exports = globalThis._mmlExports || {};
      return new MiniMLApp(exports, output);
    } finally {
      globalThis._jsOutput = null;
      globalThis._jsSysArgs = null;
      globalThis._jsReadFile = null;
      globalThis._mmlExterns = null;
      globalThis._mmlExports = null;
    }
  },

  /**
   * Run a canvas app with full animation loop support.
   *
   * @param {string} js - The compiled JS
   * @param {HTMLCanvasElement} canvas - The canvas element to draw on
   * @param {Object} [options] - Same as run(), plus:
   * @param {function} [options.onFrame] - Called each frame with the current state
   * @returns {{ app: MiniMLApp, stop: function }}
   */
  runCanvas(js, canvas, options) {
    options = options || {};

    // Set up canvas hooks
    globalThis._canvasMouseX = 0;
    globalThis._canvasMouseY = 0;
    globalThis._canvasMouseDown = false;
    globalThis._canvasMouseClicked = false;
    globalThis._canvasKeysDown = {};
    globalThis._canvasKeysPressed = {};

    globalThis._canvasInit = function(w, h) {
      const dpr = window.devicePixelRatio || 1;
      canvas.width = w * dpr;
      canvas.height = h * dpr;
      canvas.style.width = w + "px";
      canvas.style.height = h + "px";
      const ctx = canvas.getContext("2d");
      ctx.scale(dpr, dpr);
      globalThis._canvasCtx = ctx;
    };

    // Mouse events — edge detection for click/press (true for one frame)
    let mouseWasDown = false;
    let keysWereDown = {};

    function updatePointer(clientX, clientY) {
      const r = canvas.getBoundingClientRect();
      globalThis._canvasMouseX = clientX - r.left;
      globalThis._canvasMouseY = clientY - r.top;
    }
    function onMouseMove(e) { updatePointer(e.clientX, e.clientY); }
    function onMouseDown() { globalThis._canvasMouseDown = true; }
    function onMouseUp() { globalThis._canvasMouseDown = false; }
    function onTouchStart(e) {
      e.preventDefault();
      var t = e.touches[0];
      updatePointer(t.clientX, t.clientY);
      globalThis._canvasMouseDown = true;
    }
    function onTouchMove(e) {
      e.preventDefault();
      var t = e.touches[0];
      updatePointer(t.clientX, t.clientY);
    }
    function onTouchEnd(e) {
      e.preventDefault();
      globalThis._canvasMouseDown = false;
    }
    canvas.addEventListener("mousemove", onMouseMove);
    canvas.addEventListener("mousedown", onMouseDown);
    canvas.addEventListener("mouseup", onMouseUp);
    canvas.addEventListener("touchstart", onTouchStart, { passive: false });
    canvas.addEventListener("touchmove", onTouchMove, { passive: false });
    canvas.addEventListener("touchend", onTouchEnd, { passive: false });

    // Key events
    function onKeyDown(e) { globalThis._canvasKeysDown[e.key] = true; }
    function onKeyUp(e) { delete globalThis._canvasKeysDown[e.key]; }
    document.addEventListener("keydown", onKeyDown);
    document.addEventListener("keyup", onKeyUp);

    // Run the program
    const app = MiniML.run(js, options);

    let animId = null;
    let stopped = false;

    function stop() {
      stopped = true;
      if (animId !== null) cancelAnimationFrame(animId);
      canvas.removeEventListener("mousemove", onMouseMove);
      canvas.removeEventListener("mousedown", onMouseDown);
      canvas.removeEventListener("mouseup", onMouseUp);
      canvas.removeEventListener("touchstart", onTouchStart);
      canvas.removeEventListener("touchmove", onTouchMove);
      canvas.removeEventListener("touchend", onTouchEnd);
      document.removeEventListener("keydown", onKeyDown);
      document.removeEventListener("keyup", onKeyUp);
      globalThis._canvasCtx = null;
      globalThis._canvasApp = null;
    }

    // Start animation if Canvas.start_app was called
    if (globalThis._canvasApp) {
      const { initFn, frameFn, call } = globalThis._canvasApp;
      let state = call(initFn, [undefined]);

      function frame() {
        if (stopped) return;

        // Edge detection: clicked/pressed are true for exactly one frame
        const isDown = globalThis._canvasMouseDown;
        globalThis._canvasMouseClicked = isDown && !mouseWasDown;
        mouseWasDown = isDown;

        const pressed = {};
        for (const k in globalThis._canvasKeysDown) {
          if (!keysWereDown[k]) pressed[k] = true;
        }
        globalThis._canvasKeysPressed = pressed;
        keysWereDown = Object.assign({}, globalThis._canvasKeysDown);

        try {
          state = call(frameFn, [state]);
          if (options.onFrame) options.onFrame(state);
        } catch (e) {
          console.error("MiniML canvas frame error:", e);
          stop();
          return;
        }
        animId = requestAnimationFrame(frame);
      }
      animId = requestAnimationFrame(frame);
    }

    return { app: app, stop: stop };
  }
};

// Export for both browser and Node.js
if (typeof module !== "undefined" && module.exports) {
  module.exports = { MiniML: MiniML, MiniMLApp: MiniMLApp };
}
