#!/usr/bin/env python3
# End-to-end check of the in-MiniML LSP stdio transport (self_host/lsp.mml's
# `serve`): pipe a Content-Length-framed JSON-RPC session into the native server
# binary and verify it replies with correctly framed responses. Argv[1] is the
# compiled `Lsp.serve` binary. Exits 0 on success, 1 on failure.
import subprocess, sys

def frame(body: str) -> bytes:
    b = body.encode("utf-8")
    return b"Content-Length: " + str(len(b)).encode() + b"\r\n\r\n" + b

binary = sys.argv[1]
session = [
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}',
    '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///b.mml","text":"let n = 42"}}}',
    '{"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///b.mml"},"position":{"line":0,"character":8}}}',
    '{"jsonrpc":"2.0","method":"exit"}',
]
data = b"".join(frame(m) for m in session)

try:
    p = subprocess.run([binary], input=data, capture_output=True, timeout=120)
except Exception as e:
    print(f"FAIL lsp-e2e: could not run server: {e}")
    sys.exit(1)

out = p.stdout.decode("utf-8", errors="replace")
checks = [
    ("content-length framing", "Content-Length:" in out),
    ("crlf separators", "\r\n\r\n" in out),
    ("initialize capabilities", '"hoverProvider":true' in out),
    ("hover result int", '"value":"int"' in out),
    ("clean exit", p.returncode == 0),
]
bad = [name for name, ok in checks if not ok]
if bad:
    print("FAIL lsp-e2e:", ", ".join(bad))
    print("--- stdout (first 300) ---")
    print(repr(out[:300]))
    sys.exit(1)
print("self-host LSP stdio transport e2e passed (framed initialize/hover/exit)")
sys.exit(0)
