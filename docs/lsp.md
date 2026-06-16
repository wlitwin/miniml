# MiniML Language Server

A language server for MiniML (`.mml`), speaking the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) over stdio.
The production server is written **in MiniML**: a thin JSON-RPC layer
(`self_host/lsp.mml`) on the in-MiniML JSON library (`self_host/json.mml`),
built on the in-MiniML analysis library (`self_host/analysis.mml`), and shipped
as the `lsp` subcommand of the self-hosted `mml` binary (roadmap #16c — part of
the Path-B cutover where MiniML, not OCaml, is the source of truth). An OCaml
reference implementation (`lib/{analysis,lsp}.ml` + `bin/lsp_main.ml`) is kept
as a differential cross-check; it covers a subset of the capabilities below.

## Features

| Capability | Notes |
|---|---|
| **Diagnostics** (live) | Published on open/change. Recovers from errors — one broken declaration doesn't blank the file, and every syntax error plus every declaration's first type error is reported, along with warnings. |
| **Hover** | The inferred type at the cursor (e.g. `compose` → `(int -> int) -> (int -> int) -> int -> int`). |
| **Go to definition** | Top-level definitions and local bindings (let-ins, recursion), and qualified `Module.member` targets cross-file via a workspace symbol index. Shadowing resolves to the innermost binder. |
| **Completion** | Names in scope: the standard library, the file's own top-level and local bindings, and keywords. The editor filters by the typed prefix. |
| **Find references** / **document highlight** | All uses of the symbol under the cursor, in the file (highlight) or across the document (references). |
| **Rename** | Rename a binding and all its references, with a prepare step that validates the cursor is on a renameable identifier. |
| **Document & workspace symbols** | Outline of a file's definitions, and a workspace-wide symbol search. |
| **Formatting** | Whole-document formatting via the same canonical formatter as `mml fmt` (byte-faithful with the CLI). |
| **Inlay hints** & **semantic tokens** | Inferred-type inlay hints and semantic-token classification for richer highlighting. |

## Build

```sh
dune build
```

The server is the `lsp` subcommand of the all-in-one `mml` binary. The
production binary is the **self-hosted** one — build it once with:

```sh
make mml        # native-compiles self_host/ into ./mml (compiler + fmt + pkg + LSP)
./mml lsp       # reads Content-Length-framed JSON-RPC on stdin, replies on stdout
```

Put `./mml` somewhere on your `PATH` (e.g. `cp ./mml ~/.local/bin/`) so editors
can find it by name. (The OCaml reference build `_build/default/bin/mml.exe lsp`
and the standalone `_build/default/bin/lsp_main.exe` are equivalent entry points
for development.) Point your editor's generic LSP client at `mml lsp`.

## Editor setup

### Neovim (built-in LSP)

```lua
vim.filetype.add({ extension = { mml = "mml" } })
vim.api.nvim_create_autocmd("FileType", {
  pattern = "mml",
  callback = function(args)
    vim.lsp.start({
      name = "miniml",
      cmd = { "mml", "lsp" },   -- `mml` on $PATH (or an absolute path to ./mml)
      root_dir = vim.fs.dirname(vim.fs.find({ "mml.mod", ".git" }, { upward = true })[1]),
    })
  end,
})
```

### Zed

Zed attaches language servers through an **extension** (it doesn't take a raw LSP
command in `settings.json` the way VS Code does). A minimal dev extension that
launches `mml lsp` lives in [`editors/zed/`](../editors/zed/). To use it:

1. `make mml` and put `./mml` on your `PATH` (`cp ./mml ~/.local/bin/`).
2. In Zed, run **`zed: install dev extension`** (command palette) and pick the
   `editors/zed/` directory. Zed builds the extension (needs the Rust
   `wasm32-wasip1` target: `rustup target add wasm32-wasip1`).
3. Open a `.mml` file. Diagnostics, hover, go-to-definition and completion come
   from `mml lsp`.

See `editors/zed/README.md` for the version caveats (the `zed_extension_api`
version pins to your Zed release).

### VS Code

Use any generic LSP-client extension (or a few lines with
`vscode-languageclient`):

```js
const serverOptions = {
  command: "mml",          // on $PATH (or an absolute path to ./mml)
  args: ["lsp"],
  transport: TransportKind.stdio,
};
const clientOptions = { documentSelector: [{ scheme: "file", language: "mml" }] };
new LanguageClient("miniml", "MiniML", serverOptions, clientOptions).start();
```

### Anything else

The server needs no arguments. Configure the client to launch
`mml lsp`, use stdio transport, and associate the `.mml` extension.

## Command-line equivalents

The same analysis is available without an editor (the CLI shells over the same
library):

```sh
mml check file.mml                 # diagnostics, one per line; exit 1 on error
mml fmt   file.mml                 # format to stdout (-w to rewrite in place)
mml run   file.mml                 # typecheck, compile and run
```

## Known limitations

- **Go to definition on a function parameter** returns nothing (parameters carry
  no source position in the desugared typed tree). Qualified `Module.member`
  targets ARE resolved cross-file via the workspace symbol index (it scans
  `self_host/`, `stdlib/` and the root's `*.mml` once per session).
- **Completion** is not scope-filtered at the cursor (it offers every local name
  in the file); the editor's prefix filter handles most of the noise.
- **Full-document sync** only (the whole text is re-analysed on each change).
- Lexer errors are reported singly (no lexer-level recovery).
