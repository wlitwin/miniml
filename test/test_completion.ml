open Test_helpers
module A = Interpreter.Analysis

let st = get_stdlib_state ()

let () =
  Printf.printf "=== Completion Tests (#22) ===\n";

  let cs = A.completions st "let myFunc x = x + 1\nlet myValue = 7" in
  let labels = List.map fst cs in
  let has n = List.mem n labels in

  test "includes the file's own top-level definitions" (fun () ->
      if not (has "myFunc" && has "myValue") then failwith "user names missing");

  test "includes standard-library names" (fun () ->
      if not (has "print") then failwith "no stdlib 'print'");

  test "includes keywords" (fun () ->
      if not (has "match" && has "let" && has "fn") then failwith "keywords missing");

  test "excludes operators and qualified names" (fun () ->
      if List.exists (fun l -> String.contains l '.' || l = "+" || l = "^") labels then
        failwith "an operator/qualified name leaked into completions");

  test "has no duplicate labels" (fun () ->
      let sorted = List.sort compare labels in
      let rec dup = function a :: b :: _ when a = b -> Some a | _ :: r -> dup r | [] -> None in
      match dup sorted with Some d -> failwith ("duplicate: " ^ d) | None -> ());

  test "every item is a non-empty identifier" (fun () ->
      if List.exists (fun l -> not (A.is_ident_like l)) labels then
        failwith "a non-identifier completion slipped through");

  print_summary ()
