(* MiniML language server entry point (roadmap #22): the stdio transport lives
   in Interpreter.Lsp.serve; this is just the executable wrapper. The same server
   is reachable as `mml lsp`. *)
let () = Interpreter.Lsp.serve stdin stdout
