# Browser Harness (`miniml-harness.js`)

The harness provides a clean API for embedding compiled MiniML JavaScript on web pages. It handles hook setup/teardown, extern injection, and value conversion.

Programs are compiled with `--emit-js` to produce a standalone `.js` file, then loaded and executed via the harness. See the [JS Backend docs](js-vm.md) for compilation details.

## Quick Start

```html
<script src="miniml-harness.js"></script>
<script>
  fetch("myapp.js").then(r => r.text()).then(js => {
    const app = MiniML.run(js, {
      onOutput: s => document.getElementById("out").textContent += s
    });
  });
</script>
```

## `MiniML.run(js, options)`

Executes compiled MiniML JS and returns a `MiniMLApp` handle.

**Options:**

| Option | Type | Description |
|--------|------|-------------|
| `onOutput` | `(s: string) => void` | Callback for `print`/`println`. If omitted, output collects into `app.output`. |
| `externs` | `{[name: string]: Function}` | Custom extern implementations (see [Custom Externs](#custom-externs)) |
| `args` | `string[]` | Arguments for `Sys.args()` |
| `readFile` | `(path: string) => string` | Callback for `IO.read_file` |

**Returns** a `MiniMLApp` with:

| Property / Method | Description |
|-------------------|-------------|
| `app.result` | The program's final expression value (raw MiniML representation) |
| `app.output` | Array of printed strings (only populated if `onOutput` was not provided) |
| `app.call(name, ...args)` | Call an exported MiniML function by name, with partial application |
| `app.pp(value)` | Pretty-print any MiniML value as a string |
| `app.toJS(value)` | Convert MiniML runtime value to idiomatic JavaScript |
| `app.fromJS(value)` | Convert JavaScript value to MiniML representation (arrays become lists) |
| `app.list(arr)` | Create a MiniML list from a JS array (same as `fromJS`) |
| `app.tuple(arr)` | Create a MiniML tuple from a JS array |
| `app.array(arr)` | Create a MiniML array from a JS array |

## Exported Functions

All top-level `let` bindings from the MiniML program are automatically exported. Call them via `app.call()`:

```
(* mylib.mml *)
let add x y = x + y
let greet name = "Hello, " ^ name ^ "!"
let rec fib n = if n <= 1 do n else fib (n - 1) + fib (n - 2)
```

```javascript
const app = MiniML.run(compiledJs);
app.call("add", 3, 4);         // 7
app.call("greet", "World");    // "Hello, World!"
app.call("fib", 10);           // 55

// Partial application works automatically
const add5 = app.call("add", 5);
app.call(add5, 3);             // 8 — but note: use _call for raw closures
```

## Custom Externs

You can provide JavaScript functions callable from MiniML via `extern` declarations:

```
(* MiniML side *)
extern Dom.set_text : string -> string -> unit
extern Dom.get_value : string -> string

let () =
  let name = Dom.get_value "#name-input" in
  Dom.set_text "#greeting" ("Hello, " ^ name ^ "!")
```

```javascript
// JavaScript side
MiniML.run(compiledJs, {
  externs: {
    "Dom.set_text": (selector, text) => {
      document.querySelector(selector).textContent = text;
    },
    "Dom.get_value": (selector) => {
      return document.querySelector(selector).value;
    }
  }
});
```

If a required extern is not provided, the program throws: `"extern Dom.set_text not provided"`.

## Value Conversion

`app.toJS()` converts MiniML runtime values to idiomatic JavaScript:

| MiniML | JS |
|--------|-----|
| `unit` | `null` |
| `[1; 2; 3]` (list) | `[1, 2, 3]` (Array) |
| `#[1; 2; 3]` (array) | `[1, 2, 3]` (Array) |
| `(1, "hi")` (tuple) | `[1, "hi"]` (Array) |
| `Some 42` (variant) | `{tag: "Some", value: 42}` |
| `None` (variant) | `{tag: "None"}` |
| `{x = 1; y = 2}` (record) | `{x: 1, y: 2}` (Object) |
| `int`, `float`, `string`, `bool` | pass-through |

`app.fromJS()` converts in the opposite direction: JS arrays become MiniML **lists**, plain objects become records, and primitives pass through. Since JS arrays are ambiguous (they could be a list, tuple, or array in MiniML), use the explicit helpers when you need a specific type:

```javascript
app.fromJS([1, 2, 3])   // MiniML list:  [1; 2; 3]
app.list([1, 2, 3])     // MiniML list:  [1; 2; 3]  (same thing)
app.tuple([1, "hi"])     // MiniML tuple: (1, "hi")
app.array([1, 2, 3])    // MiniML array: #[1; 2; 3]
```

For variant values (like `Some 42` or `None`), construct them from the MiniML side by exporting helper functions:

```
(* MiniML side *)
let some x = Some x
let none = None
```

```javascript
// JavaScript side
app.call("some", 42)    // Some 42
app.call("none")        // None
```

## `MiniML.runCanvas(js, canvas, options)`

Convenience method for canvas apps. Sets up mouse/keyboard listeners, runs the program, and starts the `requestAnimationFrame` loop if `Canvas.start_app` was called.

```html
<canvas id="game"></canvas>
<script>
  fetch("snake.js").then(r => r.text()).then(js => {
    const { app, stop } = MiniML.runCanvas(js, document.getElementById("game"));
    document.getElementById("stopBtn").onclick = stop;
  });
</script>
```

Returns `{ app: MiniMLApp, stop: Function }`. Call `stop()` to end the animation loop and clean up event listeners.

## Demo

See [`demo.html`](demo.html) for a complete working example that runs the Canvas GUI sample via `MiniML.runCanvas()` — view page source to see the integration code.
