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

  test "includes local bindings (params, let-ins, patterns, loops)" (fun () ->
      let ls =
        List.map fst
          (A.completions st
             "let f myParam =\n  let myLocal = 1 in\n  match myParam with | Some myMatched -> myMatched | None -> 0")
      in
      let has n = List.mem n ls in
      if not (has "myParam" && has "myLocal" && has "myMatched") then
        failwith "a local binding was not offered");

  test "local completions survive a type-broken declaration" (fun () ->
      (* the decl doesn't typecheck (myParam is unbound-typed nonsense) but it
         parses, so its locals are still offered *)
      let ls = List.map fst (A.completions st "let g zzLocal = zzLocal + nope ()") in
      if not (List.mem "zzLocal" ls) then failwith "local not offered in broken decl");

  print_summary ()
