# JavaScript Backend

MiniML programs run in Node.js or the browser by compiling to standalone
JavaScript with `--emit-js`. The output is self-contained — all builtins are
inlined, and no runtime library or VM is needed.

> **History**: MiniML previously also shipped a JavaScript bytecode VM
> (`js/vm.js`) that interpreted `--emit-json` bundles in the browser. It was
> retired (roadmap #13c) once `--emit-js` became the consolidated browser
> path: the compiled-JS output is ~20× faster, and maintaining two JS
> execution strategies meant fixing every VM bug twice. Bytecode and the
> OCaml VM remain the reference execution path on the OCaml side
> (`make test-ocaml`, `make test-parity`).

## Quick Start

### Node.js

```bash
# Compile a program directly to JavaScript
dune exec bin/main.exe -- --emit-js program.mml > program.js

# Run it — no VM needed
node program.js
```

### Browser

```bash
# Build the playground (self-hosted compiler as JS + stdlib sources bundle)
make playground

# Serve and open it
make playground-serve     # http://localhost:8000/js/
```

## Compilation Strategy

- **Direct-style** for most expressions: function calls, let bindings,
  pattern matching, loops, and record/tuple operations compile to
  straightforward JavaScript.
- **CPS transformation** for code inside effect handlers: when the compiler
  detects `handle/with` blocks, it switches to continuation-passing style so
  that `resume k v` can capture and re-run continuations (including
  multishot resume via `copy_continuation`).
- **Trampolining** for stack safety: CPS code uses `_bounce(fn)` /
  `_trampoline(fn)` to avoid blowing the JS call stack on deep recursion or
  long-running effect handlers.
- **Type-directed display**: the compiler emits "float shapes" describing
  where floats live in displayed values, so nested whole floats print as
  `3.` even though MiniML ints and floats are both untagged JS numbers.

### Partial Application

The generated code supports partial application via a `_call(fn, args)`
helper that checks arity at runtime. Calling a function with fewer arguments
than it expects returns a new closure that captures the partial arguments.

## Building

```bash
# Compile a program to standalone JS
dune exec bin/main.exe -- --emit-js program.mml > program.js
node program.js

# Compile the self-hosted compiler itself to standalone JS
make self-host-compile-native-js
# This produces js/compiler_native.js
```

## Browser Harness

For embedding compiled MiniML JS on web pages, see the
[Browser Harness documentation](harness.md). The harness
(`js/miniml-harness.js`) provides `MiniML.run()` and `MiniML.runCanvas()`
APIs with exported function calling, custom externs, and value conversion.

## File Overview

| File | Description |
|------|-------------|
| `lib/js_codegen.ml` | OCaml JS codegen backend (`--emit-js`) |
| `self_host/js_codegen.mml` | The same backend, mechanically translated for the self-hosted compiler |
| `bin/main.ml` | `--emit-js` CLI flag |
| `js/miniml-harness.js` | Browser harness for embedding compiled MiniML JS |
| `js/demo.html` | Harness demo page (Canvas GUI sample) |
| `js/canvas_gui_demo.js` | Compiled Canvas GUI sample for the demo page |
| `js/index.html` | Browser playground page |
| `js/compiler_native.js` | Self-hosted compiler as standalone JS (generated, tracked) |
| `js/compiler.json` | Self-hosted compiler as bytecode JSON (generated, for `make test-parity` on the OCaml VM) |
| `js/stdlib_sources.js` | Embedded stdlib sources for the browser compiler (generated) |
| `js/build_stdlib_bundle.js` | Build script for `stdlib_sources.js` |

## Capturing Print Output

The `--emit-js` output checks for hooks before falling back to Node.js APIs:

```javascript
const outputs = [];
globalThis._jsOutput = (s) => outputs.push(s);
new Function(compiledJs)();
globalThis._jsOutput = null;
```

Other hooks:

- `globalThis._jsReadFile(path)` — intercept `IO.read_file` calls (the
  browser playground serves `stdlib/*.mml` and the user's source this way)
- `globalThis._jsSysArgs` — array of strings returned by `Sys.args()`

## Limitations

- **IO/Sys**: File operations use Node.js APIs by default. In the browser
  they are routed through the `globalThis` hooks above, or throw if no hook
  is set.
- **Runtime.eval**: Not supported in compiled JS (requires an interpreter).
- **Integer semantics**: integers are exact within ±2⁵³; overflow beyond
  that is undefined behavior (see `semantics.md` §1).

## Running Tests

```bash
make test-emit-js      # the cross-suite compiled with the OCaml compiler
make test-playground   # the same suite compiled with the self-hosted compiler
                       # (exactly the browser playground's path)
```

## Canvas API

Canvas drawing and input functions are available for building interactive
graphical applications in the browser. They are registered in Node.js but
error if called without a canvas context.

To use them, declare the functions as externs in your MiniML program:

```
extern Canvas.init : int -> int -> unit
extern Canvas.clear : string -> unit
extern Canvas.fill_rect : float -> float -> float -> float -> string -> unit
extern Canvas.stroke_rect : float -> float -> float -> float -> string -> unit
extern Canvas.fill_circle : float -> float -> float -> string -> unit
extern Canvas.draw_text : string -> float -> float -> string -> unit
extern Canvas.set_font : string -> unit
extern Canvas.mouse_x : unit -> float
extern Canvas.mouse_y : unit -> float
extern Canvas.mouse_down : unit -> bool
extern Canvas.mouse_clicked : unit -> bool
extern Canvas.key_down : string -> bool
extern Canvas.key_pressed : string -> bool
extern Canvas.start_app : (unit -> 'a) -> ('a -> 'a) -> unit
```

### Drawing Functions

| Function | Description |
|----------|-------------|
| `Canvas.init w h` | Initialize canvas with given width and height (pixels) |
| `Canvas.clear color` | Fill entire canvas with a color (e.g., `"#1a202c"`) |
| `Canvas.fill_rect x y w h color` | Draw a filled rectangle |
| `Canvas.stroke_rect x y w h color` | Draw a rectangle outline (1px stroke) |
| `Canvas.fill_circle x y radius color` | Draw a filled circle |
| `Canvas.draw_text text x y color` | Draw text at position (uses `textBaseline = "top"`) |
| `Canvas.set_font font` | Set the font (e.g., `"bold 16px sans-serif"`) |

### Input Functions

| Function | Description |
|----------|-------------|
| `Canvas.mouse_x ()` | Current mouse X position (float) |
| `Canvas.mouse_y ()` | Current mouse Y position (float) |
| `Canvas.mouse_down ()` | True while mouse button is held |
| `Canvas.mouse_clicked ()` | True for exactly one frame per click |
| `Canvas.key_down key` | True while the key is held (e.g., `"ArrowUp"`, `"a"`) |
| `Canvas.key_pressed key` | True for exactly one frame per key press |

Key names use the standard `KeyboardEvent.key` values (e.g., `"ArrowUp"`,
`"ArrowDown"`, `" "` for space, `"a"` through `"z"` for letter keys).

### App Lifecycle

`Canvas.start_app init_fn frame_fn` registers an interactive application
using a two-phase architecture:

1. **Setup phase**: The MiniML program calls `Canvas.start_app`, which stores
   the two closures and returns. Execution finishes normally.
2. **Animation phase**: After execution completes, the playground detects the
   stored closures and starts a `requestAnimationFrame` loop:
   - Calls `init_fn ()` once to get the initial state
   - Calls `frame_fn state` each frame, threading the returned state to the
     next frame

The closures are plain JavaScript functions, called via the captured `_call`
helper (which handles partial application).

```
let init () =
  Canvas.init 400 300;
  { count = 0 }

let frame state =
  Canvas.clear "#1a202c";
  Canvas.draw_text $"Count: {state.count}" 20.0 20.0 "#FFCC66";
  { count = state.count + 1 }

let () = Canvas.start_app init frame
```

### Additional Math Functions

These complement the `Math` stdlib module with trigonometric functions:

```
extern __math_sin : float -> float
extern __math_cos : float -> float
```

## Web Playground

The web playground (`js/index.html`) runs the self-hosted MiniML compiler
entirely in the browser. No server is required.

### How It Works

A single compile-then-execute pipeline:

1. **Compile**: `compiler_native.js` (the self-hosted compiler compiled to
   standalone JS) runs via `new Function`, reading the user's source and the
   embedded stdlib (`js/stdlib_sources.js`) through `_jsReadFile`, and emits
   JavaScript through `_jsOutput`.
2. **Execute**: the emitted JavaScript runs via `new Function`, with output
   captured through `_jsOutput`.

If the program called `Canvas.start_app`, the playground starts the
animation loop with direct JS function calls.

### Features

- **Syntax highlighting** with Ayu Mirage color scheme
- **Sample programs** including interactive canvas demos and a Snake game
- **Timing display** — shows compilation and execution time separately
- **Docs tab** — built-in documentation viewer (renders markdown from `docs/`)
- **Keyboard shortcuts** — Ctrl/Cmd+Enter to run, Tab for indentation

### Rebuilding

```bash
make playground
```

This builds `js/compiler_native.js` (the self-hosted compiler as JS) and
`js/stdlib_sources.js` (the embedded stdlib sources).
