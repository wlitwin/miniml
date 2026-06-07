open Test_helpers
module A = Interpreter.Analysis

let st = get_stdlib_state ()

let def_is (el, ec) src ~line ~col () =
  match A.definition st src ~line ~col with
  | Some (l, c) when l = el && c = ec -> ()
  | Some (l, c) -> failwith (Printf.sprintf "got (%d,%d), want (%d,%d)" l c el ec)
  | None -> failwith (Printf.sprintf "got None, want (%d,%d)" el ec)

let def_none src ~line ~col () =
  match A.definition st src ~line ~col with
  | None -> ()
  | Some (l, c) -> failwith (Printf.sprintf "got (%d,%d), want None" l c)

let () =
  Printf.printf "=== Go-to-definition Tests (#22) ===\n";

  (* a top-level use jumps to the declaring name token *)
  test "top-level definition" (def_is (1, 5) "let foo = 1\nlet bar = foo" ~line:2 ~col:11);

  (* a local let-in binding wins over anything outer *)
  test "local let-in binding" (def_is (1, 9) "let f = let x = 1 in x + x" ~line:1 ~col:22);

  test "local shadows a top-level of the same name"
    (def_is (2, 9) "let x = 1\nlet f = let x = 2 in x" ~line:2 ~col:22);

  (* a recursive self-reference resolves to the top-level name *)
  test "recursive self-reference" (def_is (1, 9) "let rec loop n = loop (n - 1)" ~line:1 ~col:18);

  (* go-to-def survives a broken sibling declaration (recovery) *)
  test "resolves across a broken sibling"
    (def_is (2, 5) "let bad = )\nlet foo = 1\nlet z = foo" ~line:3 ~col:9);

  (* a use of a type name jumps to its declaration *)
  test "type name definition"
    (def_is (1, 6) "type color = Red\nlet c : color = Red" ~line:2 ~col:9);

  (* parameters carry no position yet -> no definition (rather than a bad jump) *)
  test "parameter has no definition" (def_none "let f x = x + 1" ~line:1 ~col:11);

  (* whitespace / unknown name -> none *)
  test "whitespace is none" (def_none "let foo = 1" ~line:1 ~col:4);

  print_summary ()
