# MiniML — Zed extension (dev)

A minimal Zed extension that attaches the in-MiniML language server (`mml lsp`)
to `.mml` files: diagnostics, hover types, go-to-definition (incl. cross-file
`Module.member`) and completion.

Zed launches language servers through extensions, not a raw command in
`settings.json` — hence this small crate.

## Install

1. **Build the server** and put it on your `PATH`:
   ```sh
   make mml                     # → ./mml  (from the repo root)
   cp ./mml ~/.local/bin/       # anywhere on $PATH
   mml lsp </dev/null           # sanity check: should start and wait on stdin
   ```
2. **Add the Rust wasm target** Zed builds extensions with:
   ```sh
   rustup target add wasm32-wasip1
   ```
3. In Zed, open the command palette and run **`zed: install dev extension`**,
   then select this `editors/zed/` directory. Zed compiles the extension.
4. Open a `.mml` file. The server starts automatically.

## Caveats (this is a starting scaffold)

- **`zed_extension_api` version** (`Cargo.toml`) pins to your Zed release. If the
  dev-extension build fails, bump/lower it to match — see
  <https://zed.dev/docs/extensions/languages>. The `language_servers` table key
  in `extension.toml` and the `Worktree`/`Command` API in `src/lib.rs` can also
  drift between Zed versions; adjust to the version the docs show.
- **No syntax highlighting.** No tree-sitter grammar is bundled, so MiniML
  registers without highlighting; the LSP features still work. Add a grammar to
  `languages/mml/config.toml` later if you want highlighting (or if a future Zed
  makes `grammar` mandatory).
- **Root detection.** The server scans the workspace root for `*.mml` (plus
  `self_host/` and `stdlib/`) to build its cross-file symbol index; open the
  project folder, not a single loose file, for cross-file go-to-def.

## Simpler editors

Neovim and VS Code take a raw LSP command (`mml lsp`) directly — no extension
needed. See [`docs/lsp.md`](../../docs/lsp.md).
