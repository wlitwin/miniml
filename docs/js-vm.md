# JavaScript VM & JS Backend

MiniML programs can run in Node.js or the browser via two execution paths:

1. **Bytecode + JS VM**: Compile to bytecode JSON, execute with the JavaScript VM
2. **Standalone JS**: Compile directly to a self-contained `.js` file via `--emit-js`

```
                        Bytecode path                    JS codegen path
                        ─────────────                    ───────────────
OCaml Compiler          JS VM                            Direct execution
┌────────────────┐      ┌──────────────────┐
│ Source → Parse  │      │ Load JSON bundle │
│ → Typecheck    ├─JSON─→│ Register builtins│──→ Output
│ → Compile      │      │ Execute bytecode │
│ → Serialize    │      └──────────────────┘
│                │
│ → JS codegen   ├─.js──→ new Function() or node ──→ Output
└────────────────┘
```

## Quick Start

### Bytecode + VM (Node.js)

```bash
# Compile a program to a JSON bundle
dune exec bin/main.exe -- --emit-json program.mml > bundle.json

# Run it
node -e "require('./js/loader').loadBundle(require('fs').readFileSync('bundle.json','utf-8'))"
```

### Standalone JS (Node.js)

```bash
# Compile directly to JavaScript
dune exec bin/main.exe -- --emit-js program.mml > program.js

# Run it — no VM needed
node program.js
```

The `--emit-js` output is a self-contained JavaScript file with all builtins inlined. It uses direct-style compilation for most code, with CPS transformation and trampolining for algebraic effects.

### Browser

```bash
# Build the playground (compiles both compiler variants)
make playground

# Open the playground
make playground-serve
```

The playground includes the self-hosted compiler (both bytecode and native JS variants), syntax highlighting, sample programs, a 4-mode compilation dropdown, and a built-in docs viewer. See [Web Playground](#web-playground) below.

## JSON Bundle Format

The `--emit-json` flag produces a JSON bundle containing everything needed to run the program:

```json
{
  "version": 1,
  "global_names": ["print", "Stdlib.print", "String.length", ...],
  "native_globals": { ... },
  "setup": [ <prototype>, ... ],
  "main": <prototype>
}
```

### Fields

| Field | Description |
|-------|-------------|
| `version` | Format version (currently 1) |
| `global_names` | Array of all global variable names, indexed by slot |
| `native_globals` | Map of slot index → native function or typeclass dictionary |
| `setup` | Array of prototypes that rebuild the source-compiled stdlib |
| `main` | The main program prototype |

### Prototypes

A prototype represents a compiled function:

```json
{
  "name": "factorial",
  "arity": 1,
  "num_locals": 2,
  "code": [["CONST",0], ["GET_LOCAL",0], ["ADD"], ["RETURN"]],
  "constants": [{"t":"i","v":42}, {"t":"p","v":{...}}]
}
```

Opcodes are encoded as string-tagged arrays: `["CONST",0]`, `["ADD"]`, `["CLOSURE",2,[["local",1],["upvalue",3]]]`.

### Constant Value Tags

| Tag | Type | Example |
|-----|------|---------|
| `i` | int | `{"t":"i","v":42}` |
| `f` | float | `{"t":"f","v":3.14}` |
| `b` | bool | `{"t":"b","v":true}` |
| `s` | string | `{"t":"s","v":"hello"}` |
| `y` | byte | `{"t":"y","v":65}` |
| `r` | rune | `{"t":"r","v":955}` |
| `u` | unit | `{"t":"u"}` |
| `p` | prototype | `{"t":"p","v":{...}}` |
| `T` | tuple | `{"t":"T","v":[...]}` |
| `L` | list | `{"t":"L","v":[...]}` |
| `V` | variant | `{"t":"V","tag":1,"name":"Some","payload":{...}}` |

### Native Globals

Native globals describe builtins that need JS reimplementations:

```json
{
  "0": { "type": "external", "name": "print", "arity": 1 },
  "54": {
    "type": "dict",
    "fields": {
      "+": { "name": "num_add_int", "arity": 2 },
      "-": { "name": "num_sub_int", "arity": 2 }
    }
  }
}
```

- `external`: A single native function
- `dict`: A typeclass dictionary (record of native functions)

## How It Works

The stdlib has two kinds of functions:

1. **Native functions** (OCaml `VExternal`) — These need JS reimplementations. Examples: `print`, `String.length`, `Array.make`, arithmetic operators. The JS VM registers equivalent JS functions at the same global slots.

2. **Source-compiled functions** — These are written in MiniML source and compiled to bytecode. Examples: `List.map`, `Seq.range`, `Enum.join`. Their bytecode is captured during stdlib initialization and included in the `setup` array. The JS VM replays these setup prototypes to rebuild the stdlib state.

When loading a bundle, the JS VM:

1. Creates a fresh VM with the global name table
2. Registers all native builtins at their slot indices
3. Executes each setup prototype in order (rebuilds source-compiled stdlib)
4. Executes the main prototype

## JS Codegen Backend (`--emit-js`)

The `--emit-js` flag compiles a typed MiniML program directly to a standalone JavaScript file. Unlike the bytecode path, the output does not need a VM to run — it is self-contained with all builtins inlined.

### Compilation Strategy

- **Direct-style** for most expressions: function calls, let bindings, pattern matching, loops, and record/tuple operations compile to straightforward JavaScript
- **CPS transformation** for code inside effect handlers: when the compiler detects `handle/with` blocks, it switches to continuation-passing style so that `resume k ()` can capture and restore the continuation
- **Trampolining** for stack safety: CPS code uses `_bounce(fn)` / `_trampoline(fn)` to avoid blowing the JS call stack on deep recursion or long-running effect handlers

### Partial Application

The generated code supports partial application via a `_call(fn, args)` helper that checks arity at runtime. Calling a function with fewer arguments than it expects returns a new closure that captures the partial arguments.

### Building

```bash
# Compile a program to standalone JS
dune exec bin/main.exe -- --emit-js program.mml > program.js
node program.js

# Compile the self-hosted compiler itself to standalone JS
make self-host-compile-native-js
# This produces js/compiler_native.js
```

## Browser Harness

For embedding compiled MiniML JS on web pages, see the [Browser Harness documentation](harness.md). The harness (`js/miniml-harness.js`) provides `MiniML.run()` and `MiniML.runCanvas()` APIs with exported function calling, custom externs, and value conversion.

## File Overview

| File | Description |
|------|-------------|
| `lib/serialize.ml` | OCaml bytecode-to-JSON serializer |
| `lib/js_codegen.ml` | OCaml JS codegen backend (`--emit-js`) |
| `lib/interp.ml` | Capture mode for stdlib setup bytecodes |
| `bin/main.ml` | `--emit-json` and `--emit-js` CLI flags |
| `js/vm.js` | VM core: value types, dispatch loop, fibers, effects |
| `js/builtins.js` | JS implementations of ~120 native functions |
| `js/loader.js` | JSON deserialization and bundle execution |
| `js/miniml-harness.js` | Browser harness for embedding compiled MiniML JS |
| `js/demo.html` | Harness demo page (Canvas GUI sample) |
| `js/canvas_gui_demo.js` | Compiled Canvas GUI sample for the demo page |
| `js/index.html` | Browser playground page |
| `js/compiler.json` | Self-hosted compiler as bytecode JSON (for Bytecode modes) |
| `js/compiler_native.js` | Self-hosted compiler as standalone JS (for JS modes) |
| `js/test.js` | Test harness (107 tests) |
| `js/browser.js` | Build script for browser bundle |
| `js/miniml.bundle.js` | Generated browser bundle (~97 KB) |

## Capturing Print Output

Both execution paths support output hooks via `globalThis`:

### Bytecode VM hooks

The VM routes `print` calls through `globalThis._vmOutput` if set:

```javascript
const outputs = [];
globalThis._vmOutput = (s) => outputs.push(s);
const result = MiniML.loadBundle(jsonString);
globalThis._vmOutput = null;
// outputs now contains all printed lines
```

Other VM hooks:
- `globalThis._vmReadFile(path)` — intercept `IO.read_file` calls
- `globalThis._vmArgs` — array of strings returned by `Sys.args()`

### JS codegen hooks

The `--emit-js` output checks for hooks before falling back to Node.js APIs:

```javascript
const outputs = [];
globalThis._jsOutput = (s) => outputs.push(s);
new Function(compiledJs)();
globalThis._jsOutput = null;
```

Other JS codegen hooks:
- `globalThis._jsReadFile(path)` — intercept `IO.read_file` calls
- `globalThis._jsSysArgs` — array of strings returned by `Sys.args()`

## Browser API

After loading `miniml.bundle.js`, the `MiniML` global provides:

| Function / Property | Description |
|---------------------|-------------|
| `loadBundle(jsonString)` | Execute a JSON bundle, returns result value |
| `loadBundleBinary(arrayBuffer)` | Execute a binary `.mmlb` bundle |
| `callClosure(vmInst, closure, arg)` | Call a closure with one argument on an existing VM instance |
| `ppValue(value)` | Format a value as a string |
| `RuntimeError` | Error class for runtime errors |
| `VUNIT` | The unit value |
| `STDLIB_SOURCES` | Embedded stdlib source files (for the self-hosted compiler) |
| `resetProfile()` | Clear opcode execution counters |
| `dumpProfile()` | Print opcode execution statistics |

The `callClosure` function creates a fresh fiber on the given VM and executes the closure. This is used by the canvas animation loop to call frame functions after the main program has finished.

## Limitations

- **IO/Sys**: File operations (`IO.read_file`, `IO.write_file`) use Node.js APIs by default. In the browser, they are routed through `globalThis` hooks (`_vmReadFile`/`_jsReadFile`) or throw errors if no hook is set.
- **Runtime.eval**: Not supported in the JS VM or JS codegen output (requires the OCaml compiler).
- **Integer semantics**: JS uses IEEE 754 doubles (safe integers up to 2^53) vs OCaml's 63-bit integers. This is fine for most programs.
- **Compilation**: Both `--emit-json` and `--emit-js` output can be produced by the OCaml compiler or the self-hosted compiler (running in the JS VM or as standalone JS).

## Running Tests

```bash
# Build the OCaml compiler first
dune build

# Run JS VM tests (107 tests covering all major features)
node js/test.js

# Run OCaml tests (622 tests)
dune test
```

## Adding New Builtins

When a new native function is added to the OCaml side:

1. Add it to `interp.ml` or `std.ml` (OCaml implementation)
2. The serializer automatically includes it in `native_globals`
3. Add a matching JS implementation in `js/builtins.js`:

   ```javascript
   reg("Module.function_name", arity, (args) => {
     // implementation
     return vm.vstring("result");
   });
   ```

4. Regenerate the browser bundle: `node js/browser.js`

## Canvas API

Canvas drawing and input functions are available for building interactive graphical applications. They work in both execution paths (bytecode VM and `--emit-js` standalone JS). These are browser-only — they are registered in Node.js but will error if called without a canvas context.

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

Key names use the standard `KeyboardEvent.key` values (e.g., `"ArrowUp"`, `"ArrowDown"`, `"ArrowLeft"`, `"ArrowRight"`, `" "` for space, `"a"` through `"z"` for letter keys).

### App Lifecycle

`Canvas.start_app init_fn frame_fn` registers an interactive application using a two-phase architecture:

1. **Setup phase**: The MiniML program calls `Canvas.start_app` which stores the two closures and returns. The VM/JS execution finishes normally.
2. **Animation phase**: After execution completes, the playground detects the stored closures and starts a `requestAnimationFrame` loop:
   - Calls `init_fn ()` once to get the initial state
   - Calls `frame_fn state` each frame, threading the returned state to the next frame

In bytecode VM mode, the playground uses `MiniML.callClosure` to invoke the closures. In JS codegen mode, the closures are plain JavaScript functions and are called via the captured `_call` helper (which handles partial application).

This pattern enables interactive applications without making the synchronous VM async:

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

The stdlib already provides `Math.sqrt`, `Math.pow`, `Math.floor`, `Math.ceil`, `Math.round`, `Math.abs`, `Math.min`, and `Math.max`.

## Web Playground

The web playground (`js/index.html`) runs the self-hosted MiniML compiler entirely in the browser. No server is required.

### Compilation Modes

The playground supports four compilation modes, selectable via a dropdown:

| Mode | Compiler | Output | Execution |
|------|----------|--------|-----------|
| **JS → JS** (default) | `compiler_native.js` via `new Function` | standalone JS | `new Function(js)()` |
| **JS → VM** | `compiler_native.js` via `new Function` | bytecode JSON | JS VM (`MiniML.loadBundle`) |
| **Bytecode → VM** | `compiler.json` via JS VM | bytecode JSON | JS VM (`MiniML.loadBundle`) |
| **Bytecode → JS** | `compiler.json` via JS VM | standalone JS | `new Function(js)()` |

**JS → JS** is the default and fastest mode — both the compiler and user program run as native JavaScript.

### How It Works

Each mode follows a two-stage compile-then-execute model. The compile stage uses either the JS compiler (`compiler_native.js`, the self-hosted compiler compiled to standalone JS) or the bytecode compiler (`compiler.json`, run through the JS VM). The execute stage runs the output through either `MiniML.loadBundle()` (for bytecode JSON) or `new Function()` (for standalone JS).

Both compiler variants read the user's source code and stdlib files via `globalThis` hooks (`_vmReadFile`/`_jsReadFile`) and write output via `_vmOutput`/`_jsOutput`.

If the program called `Canvas.start_app`, the playground detects the registered closures and starts an animation loop — using `MiniML.callClosure` in VM output modes or direct JS function calls in JS output modes.

### Features

- **Syntax highlighting** with Ayu Mirage color scheme
- **18 sample programs** including interactive canvas demos and a Snake game
- **4-mode compilation dropdown** — choose between JS or bytecode compiler, VM or JS execution
- **Optimization toggle** — passes `--no-optimize` to the self-hosted compiler when unchecked
- **Timing display** — shows compilation and execution time separately
- **Docs tab** — built-in documentation viewer (renders markdown from `docs/`)
- **Keyboard shortcuts** — Ctrl/Cmd+Enter to run, Tab for indentation

### Rebuilding

After modifying `js/vm.js`, `js/builtins.js`, or `js/loader.js`:

```bash
node js/browser.js
```

This regenerates `js/miniml.bundle.js` by combining all JS sources, stubbing Node.js-only builtins, and embedding stdlib sources for the self-hosted compiler.

To rebuild the compiler variants for the playground:

```bash
make playground
```

This builds both `js/compiler.json` (bytecode) and `js/compiler_native.js` (standalone JS).
