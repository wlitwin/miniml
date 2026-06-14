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

## Syntax highlighting (tree-sitter)

A tree-sitter grammar lives at [`../tree-sitter-miniml/`](../tree-sitter-miniml/),
and the highlight queries at `languages/mml/highlights.scm`. Zed builds the
grammar by **cloning it from git**, so wire `extension.toml`'s `[grammars.miniml]`
to a pushed commit:

1. `git push origin main`
2. `git rev-parse HEAD` → put that sha in `extension.toml` as `rev`.
3. Reinstall the dev extension (`zed: install dev extension`).

(`repository` defaults to `github.com/wlitwin/miniml` and `path` to the grammar's
subdirectory — change `repository` if you use a different remote.)

To iterate on the grammar locally: `cd ../tree-sitter-miniml && npm install &&
node_modules/.bin/tree-sitter generate && node_modules/.bin/tree-sitter parse <file.mml>`.

## Caveats (this is a starting scaffold)

- **`zed_extension_api` version** (`Cargo.toml`) pins to your Zed release. If the
  dev-extension build fails, bump/lower it to match — see
  <https://zed.dev/docs/extensions/languages>. The `language_servers` table key
  in `extension.toml` and the `Worktree`/`Command` API in `src/lib.rs` can also
  drift between Zed versions; adjust to the version the docs show.
- **Highlighting on the big machine-translated compiler files** (e.g.
  `self_host/typechecker.mml`) is imperfect — their extreme expression nesting
  hits tree-sitter parse errors in places — but token highlighting (keywords,
  strings, comments, types, constructors, modules) still works there, and
  hand-written MiniML highlights cleanly. The grammar is a solid v1 to refine.
- **Root detection.** The server scans the workspace root for `*.mml` (plus
  `self_host/` and `stdlib/`) to build its cross-file symbol index; open the
  project folder, not a single loose file, for cross-file go-to-def.

## Simpler editors

Neovim and VS Code take a raw LSP command (`mml lsp`) directly — no extension
needed. See [`docs/lsp.md`](../../docs/lsp.md).
