//! Zed extension that wires the MiniML language server (`mml lsp`).
//!
//! It does the minimum: tell Zed how to launch the language server for the
//! MiniML language declared in `extension.toml` / `languages/mml/config.toml`.
//! The server binary is the self-hosted `mml` tool (build with `make mml`, then
//! put `./mml` on your `$PATH`); we run it as `mml lsp`.

use zed_extension_api::{self as zed, Command, LanguageServerId, Result, Worktree};

struct MinimlExtension;

impl zed::Extension for MinimlExtension {
    fn new() -> Self {
        MinimlExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &Worktree,
    ) -> Result<Command> {
        // Find `mml` on the worktree's PATH. Build it with `make mml` and copy
        // it somewhere on PATH (e.g. `cp ./mml ~/.local/bin/`).
        let command = worktree.which("mml").ok_or_else(|| {
            "could not find `mml` on $PATH — run `make mml` and put `./mml` on your PATH".to_string()
        })?;

        Ok(Command {
            command,
            args: vec!["lsp".to_string()],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(MinimlExtension);
