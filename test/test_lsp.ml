open Test_helpers
module L = Interpreter.Lsp
module J = Interpreter.Deserialize

let srv = L.create ()
let send msg = L.handle srv (J.parse_json msg)

let one msg =
  match send msg with
  | [ r ] -> r
  | rs -> failwith (Printf.sprintf "expected 1 message, got %d" (List.length rs))

let field = J.json_field
let jstr = J.json_string
let jbool = J.json_bool
let jlist = J.json_list

let () =
  Printf.printf "=== LSP Tests (#22) ===\n";

  test "initialize advertises hoverProvider" (fun () ->
      let r = one {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|} in
      let caps = field "capabilities" (field "result" r) in
      if not (jbool (field "hoverProvider" caps)) then failwith "no hoverProvider");

  test "didOpen of a clean doc publishes empty diagnostics" (fun () ->
      let r =
        one
          {|{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///a.mml","text":"let x = 1"}}}|}
      in
      if jstr (field "method" r) <> "textDocument/publishDiagnostics" then
        failwith "not a publishDiagnostics";
      if jlist (field "diagnostics" (field "params" r)) <> [] then
        failwith "expected no diagnostics");

  test "didOpen of a broken doc publishes a diagnostic" (fun () ->
      let r =
        one
          {|{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///b.mml","text":"let y = no_such_name"}}}|}
      in
      let diags = jlist (field "diagnostics" (field "params" r)) in
      if List.length diags < 1 then failwith "expected a diagnostic");

  test "didChange republishes diagnostics" (fun () ->
      let r =
        one
          {|{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///b.mml"},"contentChanges":[{"text":"let y = 1"}]}}|}
      in
      if jlist (field "diagnostics" (field "params" r)) <> [] then
        failwith "fixed doc should have no diagnostics");

  test "hover returns the inferred type" (fun () ->
      ignore
        (send
           {|{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///c.mml","text":"let n = 42"}}}|});
      (* 0-based: line 0, character 8 = the `4` of `42` *)
      let r =
        one
          {|{"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///c.mml"},"position":{"line":0,"character":8}}}|}
      in
      let value = jstr (field "value" (field "contents" (field "result" r))) in
      if value <> "int" then failwith ("hover got: " ^ value));

  test "shutdown responds, exit is silent" (fun () ->
      let r = one {|{"jsonrpc":"2.0","id":9,"method":"shutdown","params":null}|} in
      if field "result" r <> J.JNull then failwith "shutdown result not null";
      if send {|{"jsonrpc":"2.0","method":"exit"}|} <> [] then
        failwith "exit should produce no messages");

  print_summary ()
