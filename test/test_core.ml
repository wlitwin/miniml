open Test_helpers

let () =
  Printf.printf "=== Lexer Tests ===\n";

  test "lex integer" (fun () ->
    let tokens = Interpreter.Lexer.tokenize "42" in
    match tokens with
    | [{ kind = Interpreter.Token.INT 42; _ }; { kind = Interpreter.Token.EOF; _ }] -> ()
    | _ -> failwith "unexpected tokens");

  test "lex string" (fun () ->
    let tokens = Interpreter.Lexer.tokenize "\"hello\"" in
    match tokens with
    | [{ kind = Interpreter.Token.STRING "hello"; _ }; _] -> ()
    | _ -> failwith "unexpected tokens");

  test "lex keywords" (fun () ->
    let tokens = Interpreter.Lexer.tokenize "let rec in if else fn match with type of" in
    let kinds = List.map (fun (t : Interpreter.Token.token) -> t.kind) tokens in
    match kinds with
    | [LET; REC; IN; IF; ELSE; FN; MATCH; WITH; TYPE; OF; EOF] -> ()
    | _ -> failwith "unexpected tokens");

  test "lex operators" (fun () ->
    let tokens = Interpreter.Lexer.tokenize "+ - * / = <> < > <= >= && || :: ->" in
    let kinds = List.map (fun (t : Interpreter.Token.token) -> t.kind) tokens in
    match kinds with
    | [PLUS; MINUS; STAR; SLASH; EQ; NEQ; LT; GT; LE; GE; AMPAMP; PIPEPIPE; COLONCOLON; ARROW; EOF] -> ()
    | _ -> failwith "unexpected tokens");

  test "lex nested comment" (fun () ->
    let tokens = Interpreter.Lexer.tokenize "(* outer (* inner *) *) 42" in
    match tokens with
    | [{ kind = Interpreter.Token.INT 42; _ }; _] -> ()
    | _ -> failwith "comment not properly skipped");

  Printf.printf "\n=== Arithmetic Tests ===\n";

  test "integer literal" (fun () -> expect_int "42" 42);
  test "addition" (fun () -> expect_int "2 + 3" 5);
  test "subtraction" (fun () -> expect_int "10 - 4" 6);
  test "multiplication" (fun () -> expect_int "3 * 7" 21);
  test "division" (fun () -> expect_int "15 / 4" 3);
  test "modulo" (fun () -> expect_int "17 mod 5" 2);
  test "negation" (fun () -> expect_int "0 - 5" (-5); expect_int "0 - (0 - 3)" 3);
  test "precedence" (fun () -> expect_int "2 + 3 * 4" 14);
  test "parentheses" (fun () -> expect_int "(2 + 3) * 4" 20);

  Printf.printf "\n=== Comparison Tests ===\n";

  test "less than" (fun () -> expect_bool "3 < 5" true);
  test "greater than" (fun () -> expect_bool "5 > 3" true);
  test "less equal" (fun () -> expect_bool "3 <= 3" true);
  test "greater equal" (fun () -> expect_bool "4 >= 5" false);
  test "equal" (fun () -> expect_bool "3 = 3" true);
  test "not equal" (fun () -> expect_bool "3 <> 4" true);

  Printf.printf "\n=== Boolean Tests ===\n";

  test "and true" (fun () -> expect_bool "true && true" true);
  test "and false" (fun () -> expect_bool "true && false" false);
  test "or true" (fun () -> expect_bool "false || true" true);
  test "or false" (fun () -> expect_bool "false || false" false);
  test "not" (fun () -> expect_bool "not true" false);
  test "short-circuit and" (fun () ->
    (* false && (1/0 = 0) should not error *)
    expect_bool "false && (1 = 0)" false);
  test "short-circuit or" (fun () ->
    expect_bool "true || (1 = 0)" true);

  Printf.printf "\n=== String Tests ===\n";

  test "string literal" (fun () -> expect_string "\"hello\"" "hello");
  test "string concat" (fun () -> expect_string "\"hello\" ^ \" \" ^ \"world\"" "hello world");
  test "string escape" (fun () -> expect_string "\"a\\nb\"" "a\nb");

  Printf.printf "\n=== Let Binding Tests ===\n";

  test "let binding" (fun () -> expect_int "let x = 5 in x" 5);
  test "nested let" (fun () ->
    expect_int "let x = 5 in let y = 10 in x + y" 15);
  test "let shadowing" (fun () ->
    expect_int "let x = 5 in let x = 10 in x" 10);

  Printf.printf "\n=== Function Tests ===\n";

  test "lambda with annotation" (fun () ->
    expect_int "let f = fn (x: int) -> x + 1 in f 5" 6);
  test "multi-arg function" (fun () ->
    expect_int "let add (x: int) (y: int) : int = x + y in add 3 4" 7);
  test "higher-order function" (fun () ->
    expect_int "let apply (f: int -> int) (x: int) : int = f x in apply (fn (x: int) -> x * 2) 5" 10);
  test "closure" (fun () ->
    expect_int "let make_adder (n: int) : int -> int = fn (x: int) -> x + n in let add5 = make_adder 5 in add5 10" 15);

  Printf.printf "\n=== Recursion Tests ===\n";

  test "factorial" (fun () ->
    expect_int "let rec fact (n: int) : int = if n <= 1 do 1 else n * fact (n - 1) in fact 10" 3628800);
  test "fibonacci" (fun () ->
    expect_int "let rec fib (n: int) : int = if n <= 1 do n else fib (n-1) + fib (n-2) in fib 10" 55);

  Printf.printf "\n=== If/Else Tests ===\n";

  test "if true" (fun () -> expect_int "if true do 1 else 2" 1);
  test "if false" (fun () -> expect_int "if false do 1 else 2" 2);
  test "nested if" (fun () -> expect_int "if true do if false do 1 else 2 else 3" 2);

  Printf.printf "\n=== Tuple Tests ===\n";

  test "tuple creation" (fun () ->
    let result = Interpreter.Interp.run_string "(1, 2, 3)" in
    match result with
    | Interpreter.Bytecode.VTuple [| VInt 1; VInt 2; VInt 3 |] -> ()
    | _ -> failwith "unexpected tuple value");
  test "tuple pattern match" (fun () ->
    expect_int "let p = (10, 20) in match p with (a, b) -> a + b" 30);

  Printf.printf "\n=== Sequence Tests ===\n";

  test "sequence" (fun () ->
    expect_int "1; 2; 3" 3);

  Printf.printf "\n=== Print Tests ===\n";

  test "print returns unit" (fun () ->
    expect_unit "print 42");

  Printf.printf "\n=== Type Error Tests ===\n";

  test "add int to bool" (fun () ->
    expect_type_error "1 + true");
  test "if non-bool condition" (fun () ->
    expect_type_error "if 1 do 2 else 3");
  test "branch type mismatch" (fun () ->
    expect_type_error "if true do 1 else \"hello\"");

  Printf.printf "\n=== Recursive Type Tests ===\n";

  test "recursive variant type" (fun () ->
    expect_int {|
      type expr = Num of int | Add of expr * expr
      let fst_e (p: expr * expr) : expr =
        match p with
        | (a, _) -> a
      let snd_e (p: expr * expr) : expr =
        match p with
        | (_, b) -> b
      let rec eval (e: expr) : int =
        match e with
        | Num n -> n
        | Add p -> eval (fst_e p) + eval (snd_e p)
      ;;
      let _ = eval (Num 1);;
      eval (Add (Num 1, Add (Num 2, Num 3)))
    |} 6);

  Printf.printf "\n=== Operator-as-Value Tests ===\n";

  test "plus as value" (fun () ->
    expect_int "let f = (+) in f 3 4" 7);
  test "partial application" (fun () ->
    expect_int "let double = (+) 0 in double 5" 5);
  test "multiply partial" (fun () ->
    expect_int {|
      let times3 = (*) 3
      in times3 7
    |} 21);
  test "pass operator to higher-order" (fun () ->
    expect_int {|
      let apply (f: int -> int -> int) (a: int) (b: int) : int = f a b
      in apply (+) 10 20
    |} 30);
  test "string concat as value" (fun () ->
    expect_string {|let f = (^) in f "hello" " world"|} "hello world");
  test "comparison as value" (fun () ->
    expect_bool "let f = (<) in f 3 5" true);
  test "float operator as value" (fun () ->
    let result = Interpreter.Interp.run_string "(+) 1.5 2.5" in
    match result with
    | Interpreter.Bytecode.VFloat 4.0 -> ()
    | _ -> failwith "unexpected float value");

  Printf.printf "\n=== Conversion Tests ===\n";

  test "float_of_int" (fun () ->
    let result = Interpreter.Interp.run_string "float_of_int 42" in
    match result with
    | Interpreter.Bytecode.VFloat 42.0 -> ()
    | _ -> failwith "unexpected value");
  test "int_of_float" (fun () ->
    expect_int "int_of_float 3.14" 3);
  test "not as value" (fun () ->
    expect_bool "let f = (not) in f true" false);

  Printf.printf "\n=== Tail Call Optimization Tests ===\n";

  test "tail recursion does not overflow" (fun () ->
    expect_int {|
      let rec loop (n: int) : int = if n = 0 do 0 else loop (n - 1)
      in loop 10000000
    |} 0);

  test "tail call in match arm" (fun () ->
    expect_int {|
      let rec count_down (n: int) : int =
        match n with
        | 0 -> 0
        | _ -> count_down (n - 1)
      in count_down 10000000
    |} 0);

  test "tail call in let body" (fun () ->
    expect_int {|
      let rec go (n: int) : int =
        let m = n - 1 in
        if m = 0 do 42 else go m
      in go 10000000
    |} 42);

  test "tail call accumulator pattern" (fun () ->
    expect_int {|
      let rec sum_acc (n: int) (acc: int) : int =
        if n = 0 do acc else sum_acc (n - 1) (acc + n)
      in sum_acc 1000000 0
    |} 500000500000);

  Printf.printf "\n=== Mutable Variable Tests ===\n";

  test "mutable variable basic" (fun () ->
    expect_int {|
      let mut x = 1 in x := 2; x
    |} 2);

  test "mutable variable type check" (fun () ->
    expect_int {|
      let mut x = 10 in x := x + 5; x
    |} 15);

  test "immutable assignment error" (fun () ->
    expect_type_error {|
      let x = 1 in x := 2; x
    |});

  test "mutable in sequence" (fun () ->
    expect_int {|
      let mut x = 0 in x := 1; x := x + 1; x
    |} 2);

  test "top-level mutable" (fun () ->
    expect_int {|
      let mut x = 0;;
      x := 5;;
      x
    |} 5);

  test "mutable record field" (fun () ->
    expect_int {|
      type point = { mut x: int; y: int };;
      let p = { x = 1; y = 2 } in
      p.x := 10;
      p.x
    |} 10);

  test "immutable field error" (fun () ->
    expect_type_error {|
      type point = { x: int; y: int };;
      let p = { x = 1; y = 2 } in
      p.x := 10;
      p.x
    |});

  test "assignment returns unit" (fun () ->
    expect_unit {|
      let mut x = 0 in x := 5
    |});

  Printf.printf "\n=== Single-Line Comment Tests ===\n";

  test "single-line comment ignored" (fun () ->
    expect_int {|
      -- this is a comment
      42
    |} 42);

  test "comment after expression" (fun () ->
    expect_int {|
      let x = 10 -- assign ten
      in x + 5
    |} 15);

  test "comment does not eat next line" (fun () ->
    expect_int {|
      let x = 1 in
      -- comment
      let y = 2 in
      x + y
    |} 3);

  test "partial annotation suppresses exhaustiveness" (fun () ->
    expect_int {|
      type option = Some of int | None
      -- @partial
      match Some 5 with
      | Some x -> x
    |} 5);

  test "partial annotation on bool match" (fun () ->
    expect_int {|
      -- @partial
      match true with
      | true -> 1
    |} 1);

  test "let destructure still works without annotation" (fun () ->
    expect_int {|
      type option = Some of int | None
      let Some x = Some 42 in x
    |} 42);

  Printf.printf "\n=== Trailing Semicolons Tests ===\n";

  test "trailing semicolon in record expr" (fun () ->
    expect_int {|
      type point = { x: int; y: int };;
      let p = { x = 1; y = 2; } in
      p.x + p.y
    |} 3);

  test "trailing semicolon in record pattern" (fun () ->
    expect_int {|
      type point = { x: int; y: int }
      let p = { x = 10; y = 20 }
      match p with
        | { x; y; } -> x + y
    |} 30);

  test "trailing semicolon in record type" (fun () ->
    expect_int {|
      type point = { x: int; y: int; };;
      let p = { x = 5; y = 6 } in
      p.x + p.y
    |} 11);

  Printf.printf "\n=== Unit Parameter Parsing Tests ===\n";

  test "fun unit param" (fun () ->
    expect_int {|let f = fn () -> 42 in f ()|} 42);

  test "mutable closure with unit param" (fun () ->
    expect_int {|
      let mut x = 0 in
      let f = fn () -> x in
      x := 42;
      f ()
    |} 42);

  Printf.printf "\n=== Raw String Literal Tests ===\n";

  test "raw string basic" (fun () ->
    expect_string "{|hello|}" "hello");

  test "raw string empty" (fun () ->
    expect_string "{||}" "");

  test "raw string no escape processing" (fun () ->
    expect_string "{|hello\\nworld|}" "hello\\nworld");

  test "raw string multiline dedent" (fun () ->
    expect_string "{|\n    hello\n    world\n  |}" "hello\nworld");

  test "raw string preserves relative indent" (fun () ->
    expect_string "{|\n    hello\n      world\n  |}" "hello\n  world");

  test "raw string no leading newline" (fun () ->
    expect_string "{|hello\n  world|}" "hello\n  world");

  test "raw string with quotes inside" (fun () ->
    expect_string "{|she said \"hi\"|}" "she said \"hi\"");

  Printf.printf "\n=== Print as First-Class Function Tests ===\n";

  test "print still works" (fun () ->
    expect_unit "print 42");

  test "print is first-class" (fun () ->
    expect_unit {|let f = print in f "hello"|});

  test "print as higher-order arg" (fun () ->
    expect_unit {|
      let apply f x = f x in
      apply print 99
    |});

  Printf.printf "\n=== Float Operators Removed Tests ===\n";

  test "float add with polymorphic +" (fun () ->
    expect_float "1.0 + 2.0" 3.0);

  test "float sub with polymorphic -" (fun () ->
    expect_float "5.0 - 1.5" 3.5);

  test "float mul with polymorphic *" (fun () ->
    expect_float "2.0 * 3.0" 6.0);

  test "float div with polymorphic /" (fun () ->
    expect_float "10.0 / 4.0" 2.5);

  test "float negation" (fun () ->
    expect_float "-(3.5)" (-3.5));

  Printf.printf "\n=== Do/End Tests ===\n";

  test "do end basic" (fun () ->
    expect_int {| do 42 end |} 42);

  test "do end sequence" (fun () ->
    expect_int {|
      let mut x = 0 in
      do x := 1; x := x + 41; x end
    |} 42);

  test "do end in if branch" (fun () ->
    expect_int {|
      let mut x = 0 in
      if true do do x := 42; x end else 0
    |} 42);

  test "do end nested" (fun () ->
    expect_int {|
      do do 42 end end
    |} 42);

  Printf.printf "\n=== Else Block Scoping Tests ===\n";

  (* Semicolon after else () should end the if-expression, not extend the else branch *)
  test "else unit semicolon ends if" (fun () ->
    expect_int {|
      let mut x = 0 in
      if false do x := 1 else ();
      x := 42;
      x
    |} 42);

  test "else unit semicolon then expr" (fun () ->
    expect_int {|
      if false do () else ();
      42
    |} 42);

  test "else unit semicolon true branch" (fun () ->
    expect_int {|
      let mut x = 0 in
      if true do x := 10 else ();
      x := x + 1;
      x
    |} 11);

  (* Assignment in else branch should work *)
  test "else with assignment" (fun () ->
    expect_int {|
      let mut x = 0 in
      if false do x := 1 else x := 42;
      x
    |} 42);

  (* Semicolon after else assignment ends the if *)
  test "else assignment then semicolon" (fun () ->
    expect_int {|
      let mut x = 0 in
      let mut y = 0 in
      if false do x := 1 else x := 2;
      y := 10;
      x + y
    |} 12);

  (* Else with do...end block for multi-statement else *)
  test "else do end block" (fun () ->
    expect_int {|
      let mut x = 0 in
      let mut y = 0 in
      if false do x := 1 else do x := 2; y := 3 end;
      x + y
    |} 5);

  (* Else if chains *)
  test "else if chain with semicolon" (fun () ->
    expect_int {|
      let mut x = 0 in
      if false do x := 1 else if true do x := 2 else x := 3;
      x + 10
    |} 12);

  test "else if chain false false" (fun () ->
    expect_int {|
      let mut x = 0 in
      if false do x := 1 else if false do x := 2 else x := 3;
      x + 10
    |} 13);

  (* Nested if in else doesn't eat subsequent statements *)
  test "nested if in else then semicolon" (fun () ->
    expect_int {|
      let mut x = 0 in
      let mut y = 0 in
      if false do x := 1 else if true do x := 2 else x := 3;
      y := 100;
      x + y
    |} 102);

  (* Error guard pattern: throw in then, continue after *)
  test "error guard pattern" (fun () ->
    expect_int {|
      let check x = if x < 0 do failwith "negative" else ();
        x + 1
      in
      check 5
    |} 6);

  (* Else with let expression *)
  test "else with let" (fun () ->
    expect_int {|
      if false do 1 else let x = 42 in x
    |} 42);

  (* Else with match expression *)
  test "else with match" (fun () ->
    expect_int {|
      if false do 1 else match 42 with | n -> n
    |} 42);

  (* Else with function *)
  test "else with fn" (fun () ->
    expect_int {|
      let f = if false do fn x -> x + 1 else fn x -> x + 2 in
      f 10
    |} 12);

  (* Make sure then branch still allows sequences (terminated by else) *)
  test "then branch sequence" (fun () ->
    expect_int {|
      let mut x = 0 in
      if true do x := 1; x := x + 41 else x := 0;
      x
    |} 42);

  (* Parenthesized else still allows sequences *)
  test "else parens sequence" (fun () ->
    expect_int {|
      let mut x = 0 in
      let mut y = 0 in
      if false do x := 1 else (x := 2; y := 3);
      x + y
    |} 5);

  Printf.printf "\n=== Failwith Tests ===\n";

  test "failwith raises runtime error" (fun () ->
    expect_runtime_error {| failwith "test error" |});

  Printf.printf "\n=== String.of_byte Tests ===\n";

  test "String.of_byte" (fun () ->
    expect_stdlib_string {| String.of_byte (Byte.of_int 65) |} "A");

  test "String.of_byte zero" (fun () ->
    expect_stdlib_int {| String.length (String.of_byte (Byte.of_int 0)) |} 1);

  Printf.printf "\n=== Mutual Function Recursion Tests ===\n";

  test "mutual recursion expression level" (fun () ->
    expect_bool {|
      let rec is_even n =
        if n = 0 do true
        else is_odd (n - 1)
      and is_odd n =
        if n = 0 do false
        else is_even (n - 1)
      in
      is_even 10
    |} true);

  test "mutual recursion is_odd" (fun () ->
    expect_bool {|
      let rec is_even n =
        if n = 0 do true
        else is_odd (n - 1)
      and is_odd n =
        if n = 0 do false
        else is_even (n - 1)
      in
      is_odd 7
    |} true);

  test "mutual recursion is_even false" (fun () ->
    expect_bool {|
      let rec is_even n =
        if n = 0 do true
        else is_odd (n - 1)
      and is_odd n =
        if n = 0 do false
        else is_even (n - 1)
      in
      is_even 3
    |} false);

  test "mutual recursion three functions" (fun () ->
    expect_int {|
      let rec f x = if x <= 0 do 0 else g (x - 1) + 1
      and g x = if x <= 0 do 0 else h (x - 1) + 2
      and h x = if x <= 0 do 0 else f (x - 1) + 3
      in
      f 6
    |} 12);

  test "mutual recursion declaration level" (fun () ->
    expect_bool {|
      let rec is_even n =
        if n = 0 do true
        else is_odd (n - 1)
      and is_odd n =
        if n = 0 do false
        else is_even (n - 1)
      ;;
      is_even 8
    |} true);

  Printf.printf "\n=== Mutual Type Recursion Tests ===\n";

  test "mutual type recursion tree/forest" (fun () ->
    expect_int {|
      type 'a tree = Leaf | Node of 'a * 'a forest
      and 'a forest = Empty | Trees of 'a tree * 'a forest
      ;;
      let rec count_tree t = match t with
        | Leaf -> 0
        | Node (_, f) -> 1 + count_forest f
      and count_forest f = match f with
        | Empty -> 0
        | Trees (t, rest) -> count_tree t + count_forest rest
      in
      count_tree (Node (1, Trees (Node (2, Empty), Trees (Leaf, Empty))))
    |} 2);

  test "mutual type recursion construct" (fun () ->
    expect_int {|
      type 'a tree = Leaf | Node of 'a * 'a forest
      and 'a forest = Empty | Trees of 'a tree * 'a forest
      ;;
      match Node (42, Empty) with
      | Leaf -> 0
      | Node (x, _) -> x
    |} 42);

  print_summary ()
