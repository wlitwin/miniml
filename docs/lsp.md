# MiniML Language Server

A language server for MiniML (`.mml`), speaking the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/) over stdio. It
is built on the reference compiler's analysis library
(`lib/analysis.ml`); the server itself is a thin JSON-RPC layer
(`lib/lsp.ml` + `bin/lsp_main.ml`).

## Features

| Capability | Notes |
|---|---|
| **Diagnostics** (live) | Published on open/change. Recovers from errors — one broken declaration doesn't blank the file, and every syntax error plus every declaration's first type error is reported, along with warnings. |
| **Hover** | The inferred type at the cursor (e.g. `compose` → `(int -> int) -> (int -> int) -> int -> int`). |
| **Go to definition** | Top-level definitions and local bindings (let-ins, recursion). Shadowing resolves to the innermost binder. |
| **Completion** | Names in scope: the standard library, the file's own top-level and local bindings, and keywords. The editor filters by the typed prefix. |

## Build

```sh
dune build
```

The server executable is then at:

```
_build/default/bin/lsp_main.exe
```

It reads `Content-Length`-framed JSON-RPC from stdin and writes framed responses
to stdout — point your editor's generic LSP client at it for the `mml` language.

## Editor setup

### Neovim (built-in LSP)

```lua
vim.filetype.add({ extension = { mml = "mml" } })
vim.api.nvim_create_autocmd("FileType", {
  pattern = "mml",
  callback = function(args)
    vim.lsp.start({
      name = "miniml",
      cmd = { "/ABSOLUTE/PATH/TO/_build/default/bin/lsp_main.exe" },
      root_dir = vim.fs.dirname(vim.fs.find({ "dune-project" }, { upward = true })[1]),
    })
  end,
})
```

### VS Code

Use any generic LSP-client extension (or a few lines with
`vscode-languageclient`):

```js
const serverOptions = {
  command: "/ABSOLUTE/PATH/TO/_build/default/bin/lsp_main.exe",
  transport: TransportKind.stdio,
};
const clientOptions = { documentSelector: [{ scheme: "file", language: "mml" }] };
new LanguageClient("miniml", "MiniML", serverOptions, clientOptions).start();
```

### Anything else

The server needs no arguments. Configure the client to launch
`lsp_main.exe`, use stdio transport, and associate the `.mml` extension.

## Command-line equivalents

The same analysis is available without an editor (the CLI shells over the same
library):

```sh
dune exec bin/main.exe -- --check  file.mml          # diagnostics, one per line; exit 1 on error
dune exec bin/main.exe -- --hover  file.mml LINE COL  # inferred type at a 1-based position
```

## Known limitations

- **Go to definition on a function parameter** returns nothing (parameters carry
  no source position in the desugared typed tree). Module-member and qualified
  (`M.foo`) targets are not yet resolved.
- **Completion** is not scope-filtered at the cursor (it offers every local name
  in the file); the editor's prefix filter handles most of the noise.
- **Full-document sync** only (the whole text is re-analysed on each change).
- Lexer errors are reported singly (no lexer-level recovery).
