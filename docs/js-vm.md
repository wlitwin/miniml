# JavaScript VM

The JavaScript VM lets you run MiniML programs in Node.js or the browser. Programs are compiled to bytecode by the OCaml compiler, serialized to JSON, and executed by the JS VM.

```
OCaml Compiler                    JS VM
┌──────────────────────┐          ┌──────────────────────┐
│ Source → Parse →     │          │ Load JSON bundle     │
│ Typecheck → Compile  │── JSON ──│ Register JS builtins │
│ → Serialize          │  bundle  │ Replay stdlib setup  │
└──────────────────────┘          │ Execute main program │
                                  └──────────────────────┘
```

## Quick Start

### Node.js

```bash
# Compile a program to a JSON bundle
dune exec bin/main.exe -- --emit-json program.mml > bundle.json

# Run it
node -e "require('./js/loader').loadBundle(require('fs').readFileSync('bundle.json','utf-8'))"
```

### Browser

```bash
# Generate the browser bundle (one-time)
node js/browser.js

# Compile a program
dune exec bin/main.exe -- --emit-json program.ml > js/sample_bundle.json

# Open the playground
open js/index.html
```

The playground includes the self-hosted compiler, syntax highlighting, sample programs, and a built-in docs viewer. See [Web Playground](#web-playground) below.

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

## File Overview

| File | Description |
|------|-------------|
| `lib/serialize.ml` | OCaml bytecode-to-JSON serializer |
| `lib/interp.ml` | Capture mode for stdlib setup bytecodes |
| `bin/main.ml` | `--emit-json` CLI flag |
| `js/vm.js` | VM core: value types, dispatch loop, fibers, effects |
| `js/builtins.js` | JS implementations of ~120 native functions |
| `js/loader.js` | JSON deserialization and bundle execution |
| `js/index.html` | Browser playground page |
| `js/test.js` | Test harness (107 tests) |
| `js/browser.js` | Build script for browser bundle |
| `js/miniml.bundle.js` | Generated browser bundle (~97 KB) |

## Capturing Print Output

The VM routes `print` calls through `globalThis._vmOutput` if set. This lets you capture output in tests or the browser:

```javascript
const outputs = [];
globalThis._vmOutput = (s) => outputs.push(s);
const result = MiniML.loadBundle(jsonString);
globalThis._vmOutput = null;
// outputs now contains all printed lines
```

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

- **IO/Sys**: File operations (`IO.read_file`, `IO.write_file`) are Node.js only. In the browser, they throw errors.
- **Runtime.eval**: Not supported in the JS VM (requires the OCaml compiler).
- **Integer semantics**: JS uses IEEE 754 doubles (safe integers up to 2^53) vs OCaml's 63-bit integers. This is fine for most programs.
- **Compilation**: JSON bundles can be produced by either the OCaml compiler (`--emit-json`) or the self-hosted compiler running in the JS VM (used by the web playground).

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

The JS VM provides canvas drawing and input functions for building interactive graphical applications. These are browser-only — they are registered in Node.js but will error if called without a canvas context.

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

1. **Setup phase**: The MiniML program calls `Canvas.start_app` which stores the two closures and returns. The VM finishes normally.
2. **Animation phase**: After execution completes, the playground detects the stored closures and starts a `requestAnimationFrame` loop:
   - Calls `init_fn ()` once to get the initial state
   - Calls `frame_fn state` each frame, threading the returned state to the next frame

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

### How It Works

The playground uses a two-stage execution model:

```
User code ──→ Self-hosted compiler ──→ JSON bundle ──→ JS VM ──→ Output
              (runs in JS VM)          (in memory)
```

1. **Compile**: The self-hosted compiler (`compiler.json`) is loaded as a JSON bundle and executed by the JS VM. It reads the user's source code via `globalThis._vmReadFile` and writes the compiled JSON to `globalThis._vmOutput`.
2. **Run**: The compiled JSON is passed to `MiniML.loadBundle()` which deserializes and executes it.
3. **Canvas** (optional): If the program called `Canvas.start_app`, the playground detects the registered closures and starts an animation loop using `MiniML.callClosure`.

### Features

- **Syntax highlighting** with Ayu Mirage color scheme
- **18 sample programs** including interactive canvas demos and a Snake game
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
