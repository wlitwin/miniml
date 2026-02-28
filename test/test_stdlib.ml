open Test_helpers

let () =
  Printf.printf "=== String Module Tests ===\n";

  test "String.length" (fun () ->
    expect_stdlib_int {| String.length "hello" |} 5);

  test "String.length empty" (fun () ->
    expect_stdlib_int {| String.length "" |} 0);

  test "String.sub" (fun () ->
    expect_stdlib_string {| String.sub "hello world" 6 5 |} "world");

  test "String.split" (fun () ->
    expect_stdlib_value {| String.split "," "a,b,c" |}
      (Interpreter.Bytecode.VList [
        Interpreter.Bytecode.VString "a";
        Interpreter.Bytecode.VString "b";
        Interpreter.Bytecode.VString "c"]));

  test "String.trim" (fun () ->
    expect_stdlib_string {| String.trim "  hello  " |} "hello");

  test "String.starts_with" (fun () ->
    expect_stdlib_bool {| String.starts_with "he" "hello" |} true);

  test "String.starts_with false" (fun () ->
    expect_stdlib_bool {| String.starts_with "wo" "hello" |} false);

  test "String.contains" (fun () ->
    expect_stdlib_bool {| String.contains "llo" "hello" |} true);

  test "String.contains false" (fun () ->
    expect_stdlib_bool {| String.contains "xyz" "hello" |} false);

  test "String.replace" (fun () ->
    expect_stdlib_string {| String.replace "world" "ocaml" "hello world" |} "hello ocaml");

  test "String.to_int Some" (fun () ->
    expect_stdlib_value {| String.to_int "42" |}
      (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VInt 42))));

  test "String.to_int None" (fun () ->
    expect_stdlib_value {| String.to_int "abc" |}
      (Interpreter.Bytecode.VVariant (0, "None", None)));

  test "String.to_float Some" (fun () ->
    expect_stdlib_value {| String.to_float "3.14" |}
      (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VFloat 3.14))));

  test "String.uppercase" (fun () ->
    expect_stdlib_string {| String.uppercase "hello" |} "HELLO");

  test "String.lowercase" (fun () ->
    expect_stdlib_string {| String.lowercase "HELLO" |} "hello");

  Printf.printf "\n=== List Module Tests ===\n";

  test "List.length" (fun () ->
    expect_stdlib_int {| List.length [1; 2; 3] |} 3);

  test "List.length empty" (fun () ->
    expect_stdlib_int {| List.length [] |} 0);

  test "List.rev" (fun () ->
    expect_stdlib_value {| List.rev [1; 2; 3] |}
      (Interpreter.Bytecode.VList [VInt 3; VInt 2; VInt 1]));

  test "List.hd" (fun () ->
    expect_stdlib_int {| List.hd [10; 20; 30] |} 10);

  test "List.tl" (fun () ->
    expect_stdlib_value {| List.tl [1; 2; 3] |}
      (Interpreter.Bytecode.VList [VInt 2; VInt 3]));

  test "List.nth" (fun () ->
    expect_stdlib_int {| List.nth [10; 20; 30] 1 |} 20);

  test "List.concat" (fun () ->
    expect_stdlib_value {| List.concat [1; 2] [3; 4] |}
      (Interpreter.Bytecode.VList [VInt 1; VInt 2; VInt 3; VInt 4]));

  test "List.is_empty true" (fun () ->
    expect_stdlib_bool {| List.is_empty [] |} true);

  test "List.is_empty false" (fun () ->
    expect_stdlib_bool {| List.is_empty [1] |} false);

  test "List.flatten" (fun () ->
    expect_stdlib_value {| List.flatten [[1; 2]; [3]; [4; 5]] |}
      (Interpreter.Bytecode.VList [VInt 1; VInt 2; VInt 3; VInt 4; VInt 5]));

  test "List.map" (fun () ->
    expect_stdlib_value {| List.map (fn x -> x + 1) [1; 2; 3] |}
      (Interpreter.Bytecode.VList [VInt 2; VInt 3; VInt 4]));

  test "List.filter" (fun () ->
    expect_stdlib_value {| List.filter (fn x -> x > 2) [1; 2; 3; 4] |}
      (Interpreter.Bytecode.VList [VInt 3; VInt 4]));

  test "List.fold" (fun () ->
    expect_stdlib_int {| List.fold (fn acc x -> acc + x) 0 [1; 2; 3] |} 6);

  test "List.find Some" (fun () ->
    expect_stdlib_value {| List.find (fn x -> x > 2) [1; 2; 3; 4] |}
      (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VInt 3))));

  test "List.find None" (fun () ->
    expect_stdlib_value {| List.find (fn x -> x > 10) [1; 2; 3] |}
      (Interpreter.Bytecode.VVariant (0, "None", None)));

  test "List.exists true" (fun () ->
    expect_stdlib_bool {| List.exists (fn x -> x = 3) [1; 2; 3] |} true);

  test "List.exists false" (fun () ->
    expect_stdlib_bool {| List.exists (fn x -> x = 5) [1; 2; 3] |} false);

  test "List.forall true" (fun () ->
    expect_stdlib_bool {| List.forall (fn x -> x > 0) [1; 2; 3] |} true);

  test "List.forall false" (fun () ->
    expect_stdlib_bool {| List.forall (fn x -> x > 1) [1; 2; 3] |} false);

  test "List.mapi" (fun () ->
    expect_stdlib_value {| List.mapi (fn i x -> i + x) [10; 20; 30] |}
      (Interpreter.Bytecode.VList [VInt 10; VInt 21; VInt 32]));

  test "List.sort" (fun () ->
    expect_stdlib_value {|
      List.sort (fn a b -> if a < b do 0 - 1 else if a > b do 1 else 0) [3; 1; 4; 1; 5]
    |} (Interpreter.Bytecode.VList [VInt 1; VInt 1; VInt 3; VInt 4; VInt 5]));

  Printf.printf "\n=== Array Module Tests ===\n";

  test "Array.make" (fun () ->
    expect_stdlib_value {| Array.make 3 0 |}
      (Interpreter.Bytecode.VArray [|VInt 0; VInt 0; VInt 0|]));

  test "Array.length" (fun () ->
    expect_stdlib_int {| Array.length #[1; 2; 3] |} 3);

  test "Array.get" (fun () ->
    expect_stdlib_int {| Array.get #[10; 20; 30] 1 |} 20);

  test "Array.to_list" (fun () ->
    expect_stdlib_value {| Array.to_list #[1; 2; 3] |}
      (Interpreter.Bytecode.VList [VInt 1; VInt 2; VInt 3]));

  test "Array.of_list" (fun () ->
    expect_stdlib_value {| Array.of_list [1; 2; 3] |}
      (Interpreter.Bytecode.VArray [|VInt 1; VInt 2; VInt 3|]));

  test "Array.copy" (fun () ->
    expect_stdlib_int {|
      let a = #[1; 2; 3] in
      let b = Array.copy a in
      Array.set b 0 99;
      Array.get a 0
    |} 1);

  test "Array.sub" (fun () ->
    expect_stdlib_value {| Array.sub #[1; 2; 3; 4; 5] 1 3 |}
      (Interpreter.Bytecode.VArray [|VInt 2; VInt 3; VInt 4|]));

  Printf.printf "\n=== Math Module Tests ===\n";

  test "Math.abs positive" (fun () ->
    expect_stdlib_int {| Math.abs 5 |} 5);

  test "Math.abs negative" (fun () ->
    expect_stdlib_int {| Math.abs (0 - 5) |} 5);

  test "Math.min" (fun () ->
    expect_stdlib_int {| Math.min 3 7 |} 3);

  test "Math.max" (fun () ->
    expect_stdlib_int {| Math.max 3 7 |} 7);

  test "Math.pow" (fun () ->
    expect_stdlib_float {| Math.pow 2.0 10.0 |} 1024.0);

  test "Math.sqrt" (fun () ->
    expect_stdlib_float {| Math.sqrt 16.0 |} 4.0);

  test "Math.floor" (fun () ->
    expect_stdlib_int {| Math.floor 3.7 |} 3);

  test "Math.ceil" (fun () ->
    expect_stdlib_int {| Math.ceil 3.2 |} 4);

  test "Math.round" (fun () ->
    expect_stdlib_int {| Math.round 3.5 |} 4);

  test "Math.round down" (fun () ->
    expect_stdlib_int {| Math.round 3.4 |} 3);

  Printf.printf "\n=== IO Module Tests ===\n";

  test "IO.file_exists true" (fun () ->
    expect_stdlib_bool {| IO.file_exists "/tmp" |} true);

  test "IO.file_exists false" (fun () ->
    expect_stdlib_bool {| IO.file_exists "nonexistent_file_xyz" |} false);

  test "IO.write and read file" (fun () ->
    let _ = run_stdlib {| IO.write_file "/tmp/_miniml_test.txt" "hello stdlib" |} in
    expect_stdlib_string {| IO.read_file "/tmp/_miniml_test.txt" |} "hello stdlib");

  test "IO.append_file" (fun () ->
    let _ = run_stdlib {| IO.write_file "/tmp/_miniml_test2.txt" "aaa" |} in
    let _ = run_stdlib {| IO.append_file "/tmp/_miniml_test2.txt" "bbb" |} in
    expect_stdlib_string {| IO.read_file "/tmp/_miniml_test2.txt" |} "aaabbb");

  Printf.printf "\n=== Sys Module Tests ===\n";

  test "Sys.getenv None" (fun () ->
    expect_stdlib_value {| Sys.getenv "__NONEXISTENT_VAR__" |}
      (Interpreter.Bytecode.VVariant (0, "None", None)));

  test "Sys.time returns float" (fun () ->
    let result = run_stdlib {| Sys.time () |} in
    match result with
    | Interpreter.Bytecode.VFloat f when f > 0.0 -> ()
    | v -> failwith (Printf.sprintf "expected positive float, got %s"
        (Interpreter.Bytecode.pp_value v)));

  Printf.printf "\n=== Result Module Tests ===\n";

  test "Result Ok" (fun () ->
    expect_stdlib_value {| Ok 42 |}
      (Interpreter.Bytecode.VVariant (0, "Ok", Some (Interpreter.Bytecode.VInt 42))));

  test "Result Err" (fun () ->
    expect_stdlib_value {| Err "fail" |}
      (Interpreter.Bytecode.VVariant (1, "Err", Some (Interpreter.Bytecode.VString "fail"))));

  test "Result.map Ok" (fun () ->
    expect_stdlib_value {|
      let r = Ok 10 in
      Result.map (fn x -> x + 1) r
    |} (Interpreter.Bytecode.VVariant (0, "Ok", Some (Interpreter.Bytecode.VInt 11))));

  test "Result.map Err" (fun () ->
    expect_stdlib_value {|
      let r : (int, string) Result.t = Err "bad" in
      Result.map (fn x -> x + 1) r
    |} (Interpreter.Bytecode.VVariant (1, "Err", Some (Interpreter.Bytecode.VString "bad"))));

  test "Result.bind Ok" (fun () ->
    expect_stdlib_value {|
      let r = Ok 10 in
      Result.bind (fn x -> Ok (x + 1)) r
    |} (Interpreter.Bytecode.VVariant (0, "Ok", Some (Interpreter.Bytecode.VInt 11))));

  test "Result.bind Err" (fun () ->
    expect_stdlib_value {|
      let r : (int, string) Result.t = Err "bad" in
      Result.bind (fn x -> Ok (x + 1)) r
    |} (Interpreter.Bytecode.VVariant (1, "Err", Some (Interpreter.Bytecode.VString "bad"))));

  test "Result.unwrap Ok" (fun () ->
    expect_stdlib_int {| Result.unwrap (Ok 99) |} 99);

  Printf.printf "\n=== Runtime Module Tests ===\n";

  test "Runtime.eval basic" (fun () ->
    expect_stdlib_value {| Runtime.eval "1 + 1" |} Interpreter.Bytecode.VUnit);

  test "Runtime.eval defines and uses binding" (fun () ->
    expect_stdlib_value {|
      Runtime.eval "let eval_x = 42";
      Runtime.eval "let eval_y = eval_x + 1"
    |} Interpreter.Bytecode.VUnit);

  test "Runtime.eval sees parent scope" (fun () ->
    expect_stdlib_value {|
      let x = 42;;
      Runtime.eval "let z = x + 1"
    |} Interpreter.Bytecode.VUnit);

  test "Runtime.eval defines type" (fun () ->
    expect_stdlib_value {|
      Runtime.eval "type dir = Up | Down";
      Runtime.eval "let d = Up"
    |} Interpreter.Bytecode.VUnit);

  test "Runtime.eval_file" (fun () ->
    let _ = run_stdlib {| IO.write_file "/tmp/_miniml_eval_test.ml" "let eval_loaded = 99" |} in
    expect_stdlib_value {|
      Runtime.eval_file "/tmp/_miniml_eval_test.ml"
    |} Interpreter.Bytecode.VUnit);

  test "Runtime.eval_file module available in REPL" (fun () ->
    let _ = run_stdlib
      {| IO.write_file "/tmp/_miniml_mod_test.ml"
           "module TestMod = pub let add x y = x + y end" |} in
    let state = get_stdlib_state () in
    Interpreter.Interp.eval_state := Some state;
    let (state2, _, _) = Interpreter.Interp.eval_repl state
      {| Runtime.eval_file "/tmp/_miniml_mod_test.ml" |} in
    let (_, result, _) = Interpreter.Interp.eval_repl state2
      {| TestMod.add 3 4 |} in
    assert (result = Interpreter.Bytecode.VInt 7));

  test "Runtime.eval module available in REPL" (fun () ->
    let state = get_stdlib_state () in
    Interpreter.Interp.eval_state := Some state;
    let (state2, _, _) = Interpreter.Interp.eval_repl state
      {| Runtime.eval "module DynMod = pub let double x = x * 2 end" |} in
    let (_, result, _) = Interpreter.Interp.eval_repl state2
      {| DynMod.double 21 |} in
    assert (result = Interpreter.Bytecode.VInt 42));

  Printf.printf "\n=== Byte Tests ===\n";

  test "byte to_int" (fun () -> expect_stdlib_int {|Byte.to_int #41|} 65);
  test "byte of_int to_int" (fun () -> expect_stdlib_int {|Byte.to_int (Byte.of_int 97)|} 97);
  test "byte to_string" (fun () -> expect_stdlib_string {|Byte.to_string #41|} "A");
  test "byte is_alpha true" (fun () -> expect_stdlib_bool {|Byte.is_alpha #41|} true);
  test "byte is_alpha false" (fun () -> expect_stdlib_bool {|Byte.is_alpha #30|} false);
  test "byte is_digit true" (fun () -> expect_stdlib_bool {|Byte.is_digit #39|} true);
  test "byte is_digit false" (fun () -> expect_stdlib_bool {|Byte.is_digit #41|} false);
  test "byte is_space true" (fun () -> expect_stdlib_bool {|Byte.is_space #20|} true);
  test "byte is_upper true" (fun () -> expect_stdlib_bool {|Byte.is_upper #41|} true);
  test "byte is_lower true" (fun () -> expect_stdlib_bool {|Byte.is_lower #61|} true);
  test "byte to_upper" (fun () -> expect_stdlib_int {|Byte.to_int (Byte.to_upper #61)|} 65);
  test "byte to_lower" (fun () -> expect_stdlib_int {|Byte.to_int (Byte.to_lower #41)|} 97);
  test "byte equality" (fun () -> expect_bool {|#41 = #41|} true);
  test "byte inequality" (fun () -> expect_bool {|#41 <> #42|} true);
  test "byte less than" (fun () -> expect_bool {|#41 < #42|} true);
  test "byte show" (fun () -> expect_string {|show #41|} "#41");

  Printf.printf "\n=== Rune Tests ===\n";

  test "rune to_int" (fun () -> expect_stdlib_int {|Rune.to_int 'a'|} 97);
  test "rune of_int to_int" (fun () -> expect_stdlib_int {|Rune.to_int (Rune.of_int 65)|} 65);
  test "rune to_string" (fun () -> expect_stdlib_string {|Rune.to_string 'a'|} "a");
  test "rune escape newline" (fun () -> expect_stdlib_int {|Rune.to_int '\n'|} 10);
  test "rune escape tab" (fun () -> expect_stdlib_int {|Rune.to_int '\t'|} 9);
  test "rune escape null" (fun () -> expect_stdlib_int {|Rune.to_int '\0'|} 0);
  test "rune is_alpha true" (fun () -> expect_stdlib_bool {|Rune.is_alpha 'a'|} true);
  test "rune is_digit true" (fun () -> expect_stdlib_bool {|Rune.is_digit '5'|} true);
  test "rune is_space true" (fun () -> expect_stdlib_bool {|Rune.is_space ' '|} true);
  test "rune is_upper true" (fun () -> expect_stdlib_bool {|Rune.is_upper 'A'|} true);
  test "rune is_lower true" (fun () -> expect_stdlib_bool {|Rune.is_lower 'a'|} true);
  test "rune equality" (fun () -> expect_bool {|'a' = 'a'|} true);
  test "rune inequality" (fun () -> expect_bool {|'a' <> 'b'|} true);
  test "rune less than" (fun () -> expect_bool {|'a' < 'b'|} true);
  test "rune show" (fun () -> expect_string {|show 'a'|} "'a'");

  Printf.printf "\n=== Set Tests ===\n";

  test "Set.empty" (fun () ->
    expect_stdlib_int {|Set.size (Set.empty ())|} 0);
  test "Set.singleton" (fun () ->
    expect_stdlib_int {|Set.size (Set.singleton 42)|} 1);
  test "Set.of_list" (fun () ->
    expect_stdlib_int {|Set.size (Set.of_list [1; 2; 3])|} 3);
  test "Set.of_list dedup" (fun () ->
    expect_stdlib_int {|Set.size (Set.of_list [1; 1; 2; 2; 3])|} 3);
  test "Set.add" (fun () ->
    expect_stdlib_int {|Set.size (Set.add 4 (Set.of_list [1; 2; 3]))|} 4);
  test "Set.add duplicate" (fun () ->
    expect_stdlib_int {|Set.size (Set.add 2 (Set.of_list [1; 2; 3]))|} 3);
  test "Set.remove" (fun () ->
    expect_stdlib_int {|Set.size (Set.remove 2 (Set.of_list [1; 2; 3]))|} 2);
  test "Set.mem true" (fun () ->
    expect_stdlib_bool {|Set.mem 2 (Set.of_list [1; 2; 3])|} true);
  test "Set.mem false" (fun () ->
    expect_stdlib_bool {|Set.mem 5 (Set.of_list [1; 2; 3])|} false);
  test "Set.to_list" (fun () ->
    expect_stdlib_value {|Set.to_list (Set.of_list [3; 1; 2])|}
      (VList [VInt 3; VInt 1; VInt 2]));
  test "Set.union" (fun () ->
    expect_stdlib_int {|Set.size (Set.union (Set.of_list [1; 2]) (Set.of_list [2; 3]))|} 3);
  test "Set.inter" (fun () ->
    expect_stdlib_int {|Set.size (Set.inter (Set.of_list [1; 2; 3]) (Set.of_list [2; 3; 4]))|} 2);
  test "Set.diff" (fun () ->
    expect_stdlib_int {|Set.size (Set.diff (Set.of_list [1; 2; 3]) (Set.of_list [2; 3; 4]))|} 1);
  test "Set.is_empty true" (fun () ->
    expect_stdlib_bool {|Set.is_empty (Set.empty ())|} true);
  test "Set.is_empty false" (fun () ->
    expect_stdlib_bool {|Set.is_empty (Set.of_list [1])|} false);
  test "Set.is_subset true" (fun () ->
    expect_stdlib_bool {|Set.is_subset (Set.of_list [1; 2]) (Set.of_list [1; 2; 3])|} true);
  test "Set.is_subset false" (fun () ->
    expect_stdlib_bool {|Set.is_subset (Set.of_list [1; 4]) (Set.of_list [1; 2; 3])|} false);
  test "Set.fold_for" (fun () ->
    expect_stdlib_int {|
      for x in Set.of_list [10; 20; 30] with acc = 0 do
        acc + x
      end
    |} 60);
  test "Set.for_loop" (fun () ->
    expect_stdlib_int {|
      let mut total = 0;;
      for x in Set.of_list [10; 20; 30] do
        total := total + x
      end;;
      total
    |} 60);
  test "Set.for_loop_print" (fun () ->
    expect_stdlib_value
      {|for x in Set.of_list [10; 20; 30] do print x end|}
      Interpreter.Bytecode.VUnit);

  Printf.printf "\n=== Enum Tests ===\n";

  test "Enum.sum" (fun () ->
    expect_stdlib_int {|Enum.sum [1; 2; 3; 4; 5]|} 15);
  test "Enum.count" (fun () ->
    expect_stdlib_int {|Enum.count (fn x -> x > 2) [1; 2; 3; 4; 5]|} 3);
  test "Enum.take" (fun () ->
    expect_stdlib_value {|Enum.take 3 [1; 2; 3; 4; 5]|}
      (VList [VInt 1; VInt 2; VInt 3]));
  test "Enum.drop" (fun () ->
    expect_stdlib_value {|Enum.drop 2 [1; 2; 3; 4; 5]|}
      (VList [VInt 3; VInt 4; VInt 5]));
  test "Enum.take_while" (fun () ->
    expect_stdlib_value {|Enum.take_while (fn x -> x < 4) [1; 2; 3; 4; 5]|}
      (VList [VInt 1; VInt 2; VInt 3]));
  test "Enum.drop_while" (fun () ->
    expect_stdlib_value {|Enum.drop_while (fn x -> x < 3) [1; 2; 3; 4; 5]|}
      (VList [VInt 3; VInt 4; VInt 5]));
  test "Enum.flat_map" (fun () ->
    expect_stdlib_value {|Enum.flat_map (fn x -> [x; x * 10]) [1; 2; 3]|}
      (VList [VInt 1; VInt 10; VInt 2; VInt 20; VInt 3; VInt 30]));
  test "Enum.each" (fun () ->
    expect_stdlib_int {|
      let mut total = 0;;
      Enum.each (fn x -> total := total + x) [1; 2; 3];;
      total
    |} 6);
  test "Enum.reject" (fun () ->
    expect_stdlib_value {|Enum.reject (fn x -> x > 3) [1; 2; 3; 4; 5]|}
      (VList [VInt 1; VInt 2; VInt 3]));
  test "Enum.enumerate" (fun () ->
    expect_stdlib_value {|Enum.enumerate ["a"; "b"; "c"]|}
      (VList [VTuple [|VInt 0; VString "a"|]; VTuple [|VInt 1; VString "b"|]; VTuple [|VInt 2; VString "c"|]]));
  test "Enum.join" (fun () ->
    expect_stdlib_string {|Enum.join ", " ["a"; "b"; "c"]|} "a, b, c");
  test "Enum.join empty" (fun () ->
    expect_stdlib_string {|Enum.join ", " []|} "");
  test "Enum.chunk" (fun () ->
    expect_stdlib_value {|Enum.chunk 2 [1; 2; 3; 4; 5]|}
      (VList [VList [VInt 1; VInt 2]; VList [VInt 3; VInt 4]; VList [VInt 5]]));
  test "Enum.dedup" (fun () ->
    expect_stdlib_value {|Enum.dedup [1; 1; 2; 2; 3; 1]|}
      (VList [VInt 1; VInt 2; VInt 3; VInt 1]));
  test "Enum.uniq" (fun () ->
    expect_stdlib_value {|Enum.uniq [1; 2; 1; 3; 2; 4]|}
      (VList [VInt 1; VInt 2; VInt 3; VInt 4]));
  test "Enum.scan" (fun () ->
    expect_stdlib_value {|Enum.scan (fn a b -> a + b) 0 [1; 2; 3]|}
      (VList [VInt 0; VInt 1; VInt 3; VInt 6]));
  test "Enum.intersperse" (fun () ->
    expect_stdlib_value {|Enum.intersperse 0 [1; 2; 3]|}
      (VList [VInt 1; VInt 0; VInt 2; VInt 0; VInt 3]));
  test "Enum.zip_with" (fun () ->
    expect_stdlib_value {|Enum.zip_with (fn a b -> a + b) [1; 2; 3] [10; 20; 30]|}
      (VList [VInt 11; VInt 22; VInt 33]));
  test "Enum.min_by" (fun () ->
    expect_stdlib_value {|Enum.min_by (fn x -> x) [3; 1; 4; 1; 5]|}
      (VInt 1));
  test "Enum.max_by" (fun () ->
    expect_stdlib_value {|Enum.max_by (fn x -> x) [3; 1; 4; 1; 5]|}
      (VInt 5));
  test "Enum.reduce" (fun () ->
    expect_stdlib_int {|
      @partial
      match Enum.reduce (fn a b -> a + b) [1; 2; 3; 4] with x -> x
    |} 10);
  test "Enum.group_by" (fun () ->
    expect_stdlib_int {|
      let groups = Enum.group_by (fn x -> x / 10) [11; 12; 21; 22; 31];;
      @partial
      match get 1 groups with Some xs -> List.length xs
    |} 2);

  Printf.printf "\n=== Seq Tests ===\n";

  test "Seq.range to_list" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.range 1 5)|}
      (VList [VInt 1; VInt 2; VInt 3; VInt 4]));
  test "Seq.of_list roundtrip" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.of_list [1; 2; 3])|}
      (VList [VInt 1; VInt 2; VInt 3]));
  test "Seq.map" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.map (fn x -> x * 2) (Seq.range 1 4))|}
      (VList [VInt 2; VInt 4; VInt 6]));
  test "Seq.filter" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.filter (fn x -> x > 2) (Seq.range 1 6))|}
      (VList [VInt 3; VInt 4; VInt 5]));
  test "Seq.take from infinite" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.take 5 (Seq.repeat 42))|}
      (VList [VInt 42; VInt 42; VInt 42; VInt 42; VInt 42]));
  test "Seq.take_while" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.take_while (fn x -> x < 4) (Seq.range 1 10))|}
      (VList [VInt 1; VInt 2; VInt 3]));
  test "Seq.drop" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.drop 3 (Seq.range 1 6))|}
      (VList [VInt 4; VInt 5]));
  test "Seq.iterate take" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.take 5 (Seq.iterate 1 (fn x -> x * 2)))|}
      (VList [VInt 1; VInt 2; VInt 4; VInt 8; VInt 16]));
  test "Seq.fold" (fun () ->
    expect_stdlib_int {|Seq.fold (fn a b -> a + b) 0 (Seq.range 1 5)|} 10);
  test "Seq.count" (fun () ->
    expect_stdlib_int {|Seq.count (Seq.range 1 6)|} 5);
  test "Seq.sum" (fun () ->
    expect_stdlib_int {|Seq.sum (Seq.range 1 6)|} 15);
  test "Seq.find Some" (fun () ->
    expect_stdlib_value {|Seq.find (fn x -> x > 3) (Seq.range 1 10)|}
      (VVariant (1, "Some", Some (VInt 4))));
  test "Seq.find None" (fun () ->
    expect_stdlib_value {|Seq.find (fn x -> x > 100) (Seq.range 1 10)|}
      (VVariant (0, "None", None)));
  test "Seq.any true" (fun () ->
    expect_stdlib_bool {|Seq.any (fn x -> x > 3) (Seq.range 1 10)|} true);
  test "Seq.any false" (fun () ->
    expect_stdlib_bool {|Seq.any (fn x -> x > 100) (Seq.range 1 10)|} false);
  test "Seq.all true" (fun () ->
    expect_stdlib_bool {|Seq.all (fn x -> x < 10) (Seq.range 1 10)|} true);
  test "Seq.all false" (fun () ->
    expect_stdlib_bool {|Seq.all (fn x -> x < 5) (Seq.range 1 10)|} false);
  test "Seq.flat_map" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.flat_map (fn x -> Seq.range x (x + 3)) (Seq.of_list [1; 10]))|}
      (VList [VInt 1; VInt 2; VInt 3; VInt 10; VInt 11; VInt 12]));
  test "Seq.enumerate" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.enumerate (Seq.of_list ["a"; "b"; "c"]))|}
      (VList [VTuple [|VInt 0; VString "a"|]; VTuple [|VInt 1; VString "b"|]; VTuple [|VInt 2; VString "c"|]]));
  test "Seq.drop_while" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.drop_while (fn x -> x < 3) (Seq.range 1 6))|}
      (VList [VInt 3; VInt 4; VInt 5]));
  test "Seq.chunk" (fun () ->
    expect_stdlib_value {|Seq.to_list (Seq.chunk 2 (Seq.range 1 6))|}
      (VList [VList [VInt 1; VInt 2]; VList [VInt 3; VInt 4]; VList [VInt 5]]));
  test "Seq.each" (fun () ->
    expect_stdlib_int {|
      let mut total = 0;;
      Seq.each (fn x -> total := total + x) (Seq.range 1 4);;
      total
    |} 6);
  test "Seq.pipeline" (fun () ->
    expect_stdlib_value {|
      Seq.iterate 1 (fn x -> x + 1)
        |> Seq.filter (fn x -> x > 3)
        |> Seq.map (fn x -> x * 10)
        |> Seq.take 3
        |> Seq.to_list
    |}
      (VList [VInt 40; VInt 50; VInt 60]));
  test "Seq.nested take" (fun () ->
    expect_stdlib_value {|
      Seq.to_list (Seq.take 3 (Seq.take 5 (Seq.range 1 100)))
    |}
      (VList [VInt 1; VInt 2; VInt 3]));

  Printf.printf "\n=== Ref Module Tests ===\n";

  test "Ref.create and get" (fun () ->
    expect_stdlib_int {| Ref.get (Ref.create 42) |} 42);

  test "Ref.set" (fun () ->
    expect_stdlib_int {|
      let r = Ref.create 10 in
      Ref.set r 99;
      Ref.get r
    |} 99);

  test "Ref.contents field access" (fun () ->
    expect_stdlib_int {|
      let r = Ref.create 42 in
      r.contents
    |} 42);

  test "Ref.contents field mutation" (fun () ->
    expect_stdlib_int {|
      let r = Ref.create 0 in
      r.contents := 77;
      r.contents
    |} 77);

  Printf.printf "\n=== Dynarray Module Tests ===\n";

  test "Dynarray create and length" (fun () ->
    expect_stdlib_int {| Dynarray.length (Dynarray.create 4 0) |} 0);

  test "Dynarray push and length" (fun () ->
    expect_stdlib_int {|
      let d = Dynarray.create 4 0 in
      Dynarray.push d 10;
      Dynarray.push d 20;
      Dynarray.push d 30;
      Dynarray.length d
    |} 3);

  test "Dynarray get" (fun () ->
    expect_stdlib_int {|
      let d = Dynarray.create 4 0 in
      Dynarray.push d 10;
      Dynarray.push d 20;
      Dynarray.push d 30;
      Dynarray.get d 1
    |} 20);

  test "Dynarray set" (fun () ->
    expect_stdlib_int {|
      let d = Dynarray.create 4 0 in
      Dynarray.push d 10;
      Dynarray.push d 20;
      Dynarray.set d 0 99;
      Dynarray.get d 0
    |} 99);

  test "Dynarray pop" (fun () ->
    expect_stdlib_int {|
      let d = Dynarray.create 4 0 in
      Dynarray.push d 10;
      Dynarray.push d 20;
      Dynarray.push d 30;
      Dynarray.pop d
    |} 30);

  test "Dynarray pop reduces length" (fun () ->
    expect_stdlib_int {|
      let d = Dynarray.create 4 0 in
      Dynarray.push d 10;
      Dynarray.push d 20;
      let _ = Dynarray.pop d in
      Dynarray.length d
    |} 1);

  test "Dynarray to_list" (fun () ->
    expect_stdlib_value {|
      let d = Dynarray.create 4 0 in
      Dynarray.push d 1;
      Dynarray.push d 2;
      Dynarray.push d 3;
      Dynarray.to_list d
    |} (Interpreter.Bytecode.VList [
      Interpreter.Bytecode.VInt 1;
      Interpreter.Bytecode.VInt 2;
      Interpreter.Bytecode.VInt 3;
    ]));

  test "Dynarray grow beyond capacity" (fun () ->
    expect_stdlib_int {|
      let d = Dynarray.create 2 0 in
      Dynarray.push d 1;
      Dynarray.push d 2;
      Dynarray.push d 3;
      Dynarray.push d 4;
      Dynarray.push d 5;
      Dynarray.push d 6;
      Dynarray.push d 7;
      Dynarray.push d 8;
      Dynarray.push d 9;
      Dynarray.push d 10;
      Dynarray.push d 11;
      Dynarray.push d 12;
      Dynarray.push d 13;
      Dynarray.push d 14;
      Dynarray.push d 15;
      Dynarray.push d 16;
      Dynarray.push d 17;
      Dynarray.get d 16
    |} 17);

  test "Dynarray clear" (fun () ->
    expect_stdlib_int {|
      let d = Dynarray.create 4 0 in
      Dynarray.push d 1;
      Dynarray.push d 2;
      Dynarray.push d 3;
      Dynarray.clear d;
      Dynarray.length d
    |} 0);

  test "Dynarray to_array" (fun () ->
    expect_stdlib_value {|
      let d = Dynarray.create 4 0 in
      Dynarray.push d 10;
      Dynarray.push d 20;
      Dynarray.to_array d
    |} (Interpreter.Bytecode.VArray [|
      Interpreter.Bytecode.VInt 10;
      Interpreter.Bytecode.VInt 20;
    |]));

  Printf.printf "\n=== New List Function Tests ===\n";

  test "List.find_map found" (fun () ->
    expect_stdlib_value {|
      List.find_map (fn x -> if x > 3 do Some (x * 10) else None) [1; 2; 3; 4; 5]
    |} (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VInt 40))));

  test "List.find_map not found" (fun () ->
    expect_stdlib_value {|
      List.find_map (fn x -> if x > 10 do Some x else None) [1; 2; 3]
    |} (Interpreter.Bytecode.VVariant (0, "None", None)));

  test "List.assoc_opt found" (fun () ->
    expect_stdlib_value {|
      List.assoc_opt "b" [("a", 1); ("b", 2); ("c", 3)]
    |} (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VInt 2))));

  test "List.assoc_opt not found" (fun () ->
    expect_stdlib_value {|
      List.assoc_opt "z" [("a", 1); ("b", 2)]
    |} (Interpreter.Bytecode.VVariant (0, "None", None)));

  test "List.fold_right" (fun () ->
    expect_stdlib_value {|
      List.fold_right (fn x acc -> x :: acc) [1; 2; 3] [4; 5]
    |} (Interpreter.Bytecode.VList [
      Interpreter.Bytecode.VInt 1; Interpreter.Bytecode.VInt 2;
      Interpreter.Bytecode.VInt 3; Interpreter.Bytecode.VInt 4;
      Interpreter.Bytecode.VInt 5]));

  test "List.init" (fun () ->
    expect_stdlib_value {|
      List.init 5 (fn i -> i * i)
    |} (Interpreter.Bytecode.VList [
      Interpreter.Bytecode.VInt 0; Interpreter.Bytecode.VInt 1;
      Interpreter.Bytecode.VInt 4; Interpreter.Bytecode.VInt 9;
      Interpreter.Bytecode.VInt 16]));

  test "List.concat_map" (fun () ->
    expect_stdlib_value {|
      List.concat_map (fn x -> [x; x * 10]) [1; 2; 3]
    |} (Interpreter.Bytecode.VList [
      Interpreter.Bytecode.VInt 1; Interpreter.Bytecode.VInt 10;
      Interpreter.Bytecode.VInt 2; Interpreter.Bytecode.VInt 20;
      Interpreter.Bytecode.VInt 3; Interpreter.Bytecode.VInt 30]));

  test "List.map2" (fun () ->
    expect_stdlib_value {|
      List.map2 (fn a b -> a + b) [1; 2; 3] [10; 20; 30]
    |} (Interpreter.Bytecode.VList [
      Interpreter.Bytecode.VInt 11; Interpreter.Bytecode.VInt 22;
      Interpreter.Bytecode.VInt 33]));

  test "List.iter2" (fun () ->
    expect_stdlib_int {|
      let mut sum = 0 in
      List.iter2 (fn a b -> sum := sum + a + b) [1; 2; 3] [10; 20; 30];
      sum
    |} 66);

  Printf.printf "\n=== New Array Function Tests ===\n";

  test "Array.init" (fun () ->
    expect_stdlib_value {|
      Array.init 4 (fn i -> i * 3)
    |} (Interpreter.Bytecode.VArray [|
      Interpreter.Bytecode.VInt 0; Interpreter.Bytecode.VInt 3;
      Interpreter.Bytecode.VInt 6; Interpreter.Bytecode.VInt 9|]));

  test "Array.init zero" (fun () ->
    expect_stdlib_value {|
      Array.init 0 (fn i -> i)
    |} (Interpreter.Bytecode.VArray [||]));

  test "Array.map" (fun () ->
    expect_stdlib_value {|
      Array.map (fn x -> x + 1) #[10; 20; 30]
    |} (Interpreter.Bytecode.VArray [|
      Interpreter.Bytecode.VInt 11; Interpreter.Bytecode.VInt 21;
      Interpreter.Bytecode.VInt 31|]));

  test "Array.fold" (fun () ->
    expect_stdlib_int {|
      Array.fold (fn acc x -> acc + x) 0 #[1; 2; 3; 4; 5]
    |} 15);

  test "Array.iter" (fun () ->
    expect_stdlib_int {|
      let mut total = 0 in
      Array.iter (fn x -> total := total + x) #[10; 20; 30];
      total
    |} 60);

  Printf.printf "\n=== New String Function Tests ===\n";

  test "String.make" (fun () ->
    expect_stdlib_string {|
      String.make 5 (Byte.of_int 65)
    |} "AAAAA");

  test "String.make zero" (fun () ->
    expect_stdlib_string {|
      String.make 0 (Byte.of_int 65)
    |} "");

  test "String.index_opt found" (fun () ->
    expect_stdlib_value {|
      String.index_opt "hello" (Byte.of_int 108)
    |} (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VInt 2))));

  test "String.index_opt not found" (fun () ->
    expect_stdlib_value {|
      String.index_opt "hello" (Byte.of_int 122)
    |} (Interpreter.Bytecode.VVariant (0, "None", None)));

  test "String.rindex_opt" (fun () ->
    expect_stdlib_value {|
      String.rindex_opt "hello world" (Byte.of_int 111)
    |} (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VInt 7))));

  test "String.concat" (fun () ->
    expect_stdlib_string {|
      String.concat ", " ["hello"; "world"; "test"]
    |} "hello, world, test");

  test "String.concat empty sep" (fun () ->
    expect_stdlib_string {|
      String.concat "" ["a"; "b"; "c"]
    |} "abc");

  print_summary ()
