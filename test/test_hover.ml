open Test_helpers
module A = Interpreter.Analysis

let st = get_analysis_ctx ()

let hover_is expected src ~line ~col () =
  match A.hover st src ~line ~col with
  | Some t when t = expected -> ()
  | Some t -> failwith (Printf.sprintf "got %S want %S" t expected)
  | None -> failwith (Printf.sprintf "got None, want %S" expected)

let hover_none src ~line ~col () =
  match A.hover st src ~line ~col with
  | None -> ()
  | Some t -> failwith (Printf.sprintf "expected None, got %S" t)

let () =
  Printf.printf "=== Hover Tests (#22) ===\n";

  (* literals *)
  test "hover int literal" (hover_is "int" "let n = 42" ~line:1 ~col:9);
  test "hover bool literal" (hover_is "bool" "let b = true" ~line:1 ~col:9);
  test "hover string literal" (hover_is "string" "let s = \"hi\"" ~line:1 ~col:9);

  (* a variable use carries its inferred type *)
  test "hover var use is int" (hover_is "int" "let f x = x + 1" ~line:1 ~col:11);

  (* nothing to hover on whitespace *)
  test "hover on space is None" (hover_none "let n = 42" ~line:1 ~col:4);

  (* hover works even when ANOTHER declaration has an error (recovery) *)
  test "hover survives a broken sibling decl"
    (hover_is "int" "let bad = )\nlet good = 7" ~line:2 ~col:12);

  print_summary ()
