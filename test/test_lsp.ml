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

  test "initialize advertises definitionProvider" (fun () ->
      let r = one {|{"jsonrpc":"2.0","id":3,"method":"initialize","params":{}}|} in
      let caps = field "capabilities" (field "result" r) in
      if not (jbool (field "definitionProvider" caps)) then failwith "no definitionProvider");

  test "definition returns a Location" (fun () ->
      ignore
        (send
           {|{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///d.mml","text":"let foo = 1\nlet bar = foo"}}}|});
      (* 0-based line 1, char 10 = the `foo` use on line 2 *)
      let r =
        one
          {|{"jsonrpc":"2.0","id":4,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///d.mml"},"position":{"line":1,"character":10}}}|}
      in
      let result = field "result" r in
      if jstr (field "uri" result) <> "file:///d.mml" then failwith "wrong uri";
      let start = field "start" (field "range" result) in
      (* def `foo` is at 0-based line 0, char 4 *)
      if J.json_int (field "line" start) <> 0 || J.json_int (field "character" start) <> 4
      then failwith "wrong definition position");

  test "utf16<->byte column: ascii is identity" (fun () ->
      let line = "let x = 1" in
      if L.utf16_to_byte_col line 4 <> 5 then failwith "ascii u16->byte";
      if L.byte_col_to_utf16 line 5 <> 4 then failwith "ascii byte->u16");

  test "utf16<->byte column: multibyte char counts as one unit" (fun () ->
      (* "a é b": é is 2 UTF-8 bytes but 1 UTF-16 unit; 'b' is u16 3, byte 4 *)
      let line = "a\xc3\xa9 b" in
      if L.utf16_to_byte_col line 3 <> 5 then
        failwith (Printf.sprintf "u16->byte: %d" (L.utf16_to_byte_col line 3));
      if L.byte_col_to_utf16 line 5 <> 3 then
        failwith (Printf.sprintf "byte->u16: %d" (L.byte_col_to_utf16 line 5)));

  test "completion returns items" (fun () ->
      ignore
        (send
           {|{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///e.mml","text":"let mine = 1"}}}|});
      let r =
        one
          {|{"jsonrpc":"2.0","id":5,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///e.mml"},"position":{"line":0,"character":11}}}|}
      in
      let items = jlist (field "result" r) in
      if List.length items < 1 then failwith "no completion items";
      let labels = List.map (fun it -> jstr (field "label" it)) items in
      if not (List.mem "mine" labels) then failwith "own decl not offered");

  test "shutdown responds, exit is silent" (fun () ->
      let r = one {|{"jsonrpc":"2.0","id":9,"method":"shutdown","params":null}|} in
      if field "result" r <> J.JNull then failwith "shutdown result not null";
      if send {|{"jsonrpc":"2.0","method":"exit"}|} <> [] then
        failwith "exit should produce no messages");

  print_summary ()
