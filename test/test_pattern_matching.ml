open Test_helpers

let () =
  Printf.printf "=== Let Destructuring Tests ===\n";

  test "tuple destructure" (fun () ->
    expect_int {|
      let (x, y) = (1, 2) in x + y
    |} 3);

  test "nested tuple destructure" (fun () ->
    expect_int {|
      let ((a, b), c) = ((1, 2), 3) in a + b + c
    |} 6);

  test "list cons destructure" (fun () ->
    expect_int {|
      let x :: rest = [1; 2; 3] in x
    |} 1);

  test "variant destructure" (fun () ->
    expect_int {|
      type option = Some of int | None
      let Some x = Some 42 in x
    |} 42);

  test "record destructure" (fun () ->
    expect_int {|
      let {x = a; y = b} = {x = 10; y = 20} in a + b
    |} 30);

  test "record destructure punning" (fun () ->
    expect_int {|
      let {x; y} = {x = 10; y = 20} in x + y
    |} 30);

  test "record destructure punning mixed" (fun () ->
    expect_int {|
      let {x; y = b} = {x = 10; y = 20} in x + b
    |} 30);

  test "record destructure punning in match" (fun () ->
    expect_int {|
      match {x = 1; y = 2} with {x; y} -> x + y
    |} 3);

  test "wildcard in destructure" (fun () ->
    expect_int {|
      let (_, y) = (1, 2) in y
    |} 2);

  test "top-level tuple destructure" (fun () ->
    expect_int {|
      let (x, y) = (10, 20)
      ;;
      x + y
    |} 30);

  test "top-level nested destructure" (fun () ->
    expect_int {|
      let (a, b, c) = (1, 2, 3)
      ;;
      a + b + c
    |} 6);

  test "unit destructure" (fun () ->
    expect_int {|
      let () = () in 42
    |} 42);

  Printf.printf "\n=== As-Pattern Tests ===\n";

  test "basic as-pattern tuple" (fun () ->
    expect_int {|
      match (1, 2) with
      | (a, b) as _p -> a + b
    |} 3);

  test "as-pattern cons binding whole list" (fun () ->
    expect_int {|
      match [1; 2; 3] with
      | x :: _ as _lst -> x
      | [] -> 0
    |} 1);

  test "as-pattern with variant" (fun () ->
    expect_int {|
      match Some 42 with
      | Some n as _opt -> n
      | None -> 0
    |} 42);

  test "as-pattern uses whole value" (fun () ->
    expect_int {|
      let rec len xs = match xs with
        | [] -> 0
        | _ :: rest -> 1 + len rest
      ;;
      match [10; 20; 30] with
      | _ :: _ as whole -> len whole
      | [] -> 0
    |} 3);

  test "as-pattern with or-pattern in parens" (fun () ->
    expect_int {|
      type color = Red | Green | Blue
      ;;
      match Green with
      | (Red | Green) as _c -> 1
      | Blue -> 2
    |} 1);

  Printf.printf "\n=== Or-Pattern Tests ===\n";

  test "or-pattern literals" (fun () ->
    expect_string {|
      let classify n = match n with
        | 1 | 2 -> "small"
        | 3 | 4 -> "medium"
        | _ -> "big"
      ;;
      classify 2
    |} "small");

  test "or-pattern constructors" (fun () ->
    expect_string {|
      type color = Red | Green | Blue
      ;;
      let temp c = match c with
        | Red | Green -> "warm"
        | Blue -> "cool"
      ;;
      temp Green
    |} "warm");

  test "or-pattern structural" (fun () ->
    expect_string {|
      let describe xs = match xs with
        | [] | [_] -> "short"
        | _ -> "long"
      ;;
      describe [1]
    |} "short");

  test "or-pattern with bindings" (fun () ->
    expect_int {|
      match (1, 10) with
      | (1, x) | (2, x) -> x
      | _ -> 0
    |} 10);

  test "or-pattern second branch matches" (fun () ->
    expect_string {|
      let classify n = match n with
        | 1 | 2 -> "small"
        | _ -> "big"
      ;;
      classify 5
    |} "big");

  Printf.printf "\n=== Guard Tests ===\n";

  test "basic guard" (fun () ->
    expect_string {|
      let classify n = match n with
        | x when x > 0 -> "positive"
        | x when x = 0 -> "zero"
        | _ -> "negative"
      ;;
      classify 5
    |} "positive");

  test "guard zero" (fun () ->
    expect_string {|
      let classify n = match n with
        | x when x > 0 -> "positive"
        | x when x = 0 -> "zero"
        | _ -> "negative"
      ;;
      classify 0
    |} "zero");

  test "guard negative" (fun () ->
    expect_string {|
      let classify n = match n with
        | x when x > 0 -> "positive"
        | x when x = 0 -> "zero"
        | _ -> "negative"
      ;;
      classify (0 - 3)
    |} "negative");

  test "guard with destructuring" (fun () ->
    expect_int {|
      match (3, 1) with
      | (a, b) when a > b -> a
      | (a, b) when a < b -> b
      | _ -> 0
    |} 3);

  test "guard with cons pattern" (fun () ->
    expect_int {|
      match [15; 2; 3] with
      | x :: _ when x > 10 -> x
      | _ -> 0
    |} 15);

  test "guard fallthrough to next arm" (fun () ->
    expect_int {|
      match [5; 2; 3] with
      | x :: _ when x > 10 -> x
      | _ -> 0
    |} 0);

  test "or-pattern with guard" (fun () ->
    expect_string {|
      let check n = match n with
        | 1 | 2 when true -> "yes"
        | _ -> "no"
      ;;
      check 2
    |} "yes");

  Printf.printf "\n=== Exhaustiveness Checking Tests ===\n";

  (* Should pass: exhaustive patterns *)
  test "exhaustive variant all constructors" (fun () ->
    expect_int {|
      type color = Red | Green | Blue
      match Red with
      | Red -> 1
      | Green -> 2
      | Blue -> 3
    |} 1);

  test "exhaustive variant with wildcard" (fun () ->
    expect_int {|
      type color = Red | Green | Blue
      match Green with
      | Red -> 1
      | _ -> 0
    |} 0);

  test "exhaustive bool" (fun () ->
    expect_int {|
      match true with
      | true -> 1
      | false -> 0
    |} 1);

  test "exhaustive option" (fun () ->
    expect_int {|
      type option = Some of int | None
      match Some 5 with
      | Some x -> x
      | None -> 0
    |} 5);

  test "exhaustive list nil and cons" (fun () ->
    expect_int {|
      match [1; 2] with
      | [] -> 0
      | x :: _ -> x
    |} 1);

  test "exhaustive or-pattern covers all" (fun () ->
    expect_int {|
      type color = Red | Green | Blue
      match Blue with
      | Red | Green -> 1
      | Blue -> 2
    |} 2);

  test "exhaustive variant with payload" (fun () ->
    expect_int {|
      type expr = Num of int | Add of int | Neg
      match Num 42 with
      | Num n -> n
      | Add n -> n
      | Neg -> 0
    |} 42);

  test "exhaustive with variable pattern" (fun () ->
    expect_int {|
      type color = Red | Green | Blue
      match Red with
      | x -> 1
    |} 1);

  (* Should error: non-exhaustive patterns *)
  test "non-exhaustive variant missing constructor" (fun () ->
    expect_type_error_msg {|
      type color = Red | Green | Blue
      match Red with
      | Red -> 1
      | Green -> 2
    |} "Blue");

  test "non-exhaustive bool missing false" (fun () ->
    expect_type_error_msg {|
      match true with
      | true -> 1
    |} "false");

  test "non-exhaustive list missing cons" (fun () ->
    expect_type_error_msg {|
      match [1] with
      | [] -> 0
    |} "_ :: _");

  test "non-exhaustive list missing nil" (fun () ->
    expect_type_error_msg {|
      match [1] with
      | x :: _ -> x
    |} "[]");

  test "non-exhaustive option missing None" (fun () ->
    expect_type_error_msg {|
      type option = Some of int | None
      match Some 1 with
      | Some x -> x
    |} "None");

  test "non-exhaustive guard does not count" (fun () ->
    expect_type_error_msg {|
      match true with
      | x when x -> 1
    |} "non-exhaustive");

  test "non-exhaustive multiple missing" (fun () ->
    expect_type_error_msg {|
      type color = Red | Green | Blue | Yellow
      match Red with
      | Red -> 1
    |} "Green");

  Printf.printf "\n=== Fn Pattern Destructuring Tests ===\n";

  test "fn tuple destructure" (fun () ->
    expect_int {|
      let add = fn (x, y) -> x + y in
      add (10, 32)
    |} 42);

  test "fn record destructure" (fun () ->
    expect_int {|
      type point = { x: int; y: int }
      let get_x = fn {x; y} -> x in
      get_x {x = 42; y = 0}
    |} 42);

  test "fn nested tuple destructure" (fun () ->
    expect_int {|
      let f = fn ((a, b), c) -> a + b + c in
      f ((10, 20), 12)
    |} 42);

  test "fn tuple destructure with other params" (fun () ->
    expect_int {|
      let f = fn x (a, b) -> x + a + b in
      f 10 (20, 12)
    |} 42);

  test "fn unit destructure" (fun () ->
    expect_int {|
      let f = fn () -> 42 in
      f ()
    |} 42);

  test "fn annotated record destructure" (fun () ->
    expect_type_error {|
      let z = {x = 10; y = "hello"}
      let get_x = fn ({x} : {x: int}) -> x in
      get_x z
    |});

  test "fn annotated record destructure with wildcard" (fun () ->
    expect_int {|
      type point = { x: int; y: int }
      let p = { x = 42; y = 99 }
      let get_x = fn ({x; _} : point) -> x in
      get_x p
    |} 42);

  test "fn annotated tuple destructure" (fun () ->
    expect_int {|
      let f = fn ((a, b) : int * string) -> a in
      f (42, "hi")
    |} 42);

  test "fn record destructure subtyping" (fun () ->
    expect_type_error {|
      type has_x = { x: int }
      let get_x = fn ({x} : has_x) -> x in
      get_x {x = 42; y = "extra"}
    |});

  test "fn record wildcard pattern" (fun () ->
    expect_int {|
      type point = { x: int; y: int }
      let p = { x = 10; y = 32 }
      match p with | {x; _} -> x
    |} 10);

  Printf.printf "\n=== Array Pattern Matching Tests ===\n";

  test "array pattern basic" (fun () ->
    expect_int {|
      match #[1; 2; 3] with
        | #[a; b; c] -> a + b + c
        | _ -> 0
    |} 6);

  test "array pattern single" (fun () ->
    expect_int {|
      match #[42] with
        | #[x] -> x
        | _ -> 0
    |} 42);

  test "array pattern length mismatch" (fun () ->
    expect_int {|
      match #[1; 2] with
        | #[a; b; c] -> 0
        | #[a; b] -> a + b
        | _ -> -1
    |} 3);

  test "array pattern empty" (fun () ->
    expect_bool {|
      match #[] with
        | #[] -> true
        | _ -> false
    |} true);

  test "array pattern nested" (fun () ->
    expect_int {|
      match #[1; 2] with
        | #[1; x] -> x * 10
        | _ -> 0
    |} 20);

  Printf.printf "\n=== Map Pattern Matching Tests ===\n";

  test "map pattern basic" (fun () ->
    expect_int {|
      match #{"x": 1; "y": 2} with
        | #{"x": x} -> x
        | _ -> 0
    |} 1);

  test "map pattern multiple keys" (fun () ->
    expect_int {|
      match #{"a": 1; "b": 2} with
        | #{"a": a; "b": b} -> a + b
        | _ -> 0
    |} 3);

  test "map pattern missing key fallthrough" (fun () ->
    expect_int {|
      match #{"x": 1} with
        | #{"y": y} -> y
        | _ -> 0
    |} 0);

  test "map pattern int keys" (fun () ->
    expect_int {|
      match #{1: 10; 2: 20} with
        | #{1: v} -> v
        | _ -> 0
    |} 10);

  Printf.printf "\n=== Array/Map Exhaustiveness Tests ===\n";

  test "non-exhaustive array pattern" (fun () ->
    expect_type_error_msg {|
      match #[1; 2] with
        | #[a; b] -> a + b
    |} "non-exhaustive match, missing: #[]");

  test "non-exhaustive array skips covered length" (fun () ->
    expect_type_error_msg {|
      match #[1] with
        | #[] -> 0
        | #[x] -> x
    |} "non-exhaustive match, missing: #[_; _]");

  test "array pattern with wildcard is exhaustive" (fun () ->
    expect_int {|
      match #[1; 2; 3] with
        | #[a; b; c] -> a + b + c
        | _ -> 0
    |} 6);

  test "non-exhaustive map pattern" (fun () ->
    expect_type_error_msg {|
      match #{"x": 1} with
        | #{"x": x} -> x
    |} "non-exhaustive match, missing: _");

  test "map pattern with wildcard is exhaustive" (fun () ->
    expect_int {|
      match #{"x": 1} with
        | #{"x": x} -> x
        | _ -> 0
    |} 1);

  Printf.printf "\n=== Lambda (fn) Tests ===\n";

  test "fn basic lambda" (fun () ->
    expect_int {|(fn x -> x + 1) 5|} 6);

  test "fn multi-param lambda" (fun () ->
    expect_int {|(fn x y -> x + y) 3 4|} 7);

  test "fn lambda match option" (fun () ->
    expect_int {|
      let f = fn | Some x -> x | None -> 0 in
      f (Some 42)
    |} 42);

  test "fn lambda match option none" (fun () ->
    expect_int {|
      let f = fn | Some x -> x | None -> 0 in
      f None
    |} 0);

  test "fn lambda match list" (fun () ->
    expect_int {|
      let f = fn | [] -> 0 | x :: _ -> x in
      f [10; 20; 30]
    |} 10);

  test "fn lambda match list empty" (fun () ->
    expect_int {|
      let f = fn | [] -> 0 | x :: _ -> x in
      f []
    |} 0);

  test "fn lambda match with map" (fun () ->
    expect_stdlib_int {|
      List.map (fn | Some x -> x | None -> 0) [Some 1; None; Some 3]
        |> List.fold (fn acc x -> acc + x) 0
    |} 4);

  test "fn lambda match non-exhaustive" (fun () ->
    expect_type_error_msg {|
      fn | true -> 1
    |} "non-exhaustive match, missing: false");

  test "fn lambda match with guard" (fun () ->
    expect_int {|
      let f = fn | x when x > 0 -> x | _ -> 0 in
      f 5
    |} 5);

  test "fn lambda match guard fallthrough" (fun () ->
    expect_int {|
      let f = fn | x when x > 0 -> x | _ -> 0 in
      f (-3)
    |} 0);

  test "fn lambda match or-pattern" (fun () ->
    expect_int {|
      let f = fn | Some 1 | Some 2 -> 99 | Some x -> x | None -> 0 in
      f (Some 2)
    |} 99);

  test "fn no-param lambda" (fun () ->
    expect_int {|(fn -> 5)()|} 5);

  test "fn no-param lambda bound" (fun () ->
    expect_int {|let f = fn -> 42 in f ()|} 42);

  test "fn no-param lambda unit body" (fun () ->
    expect_unit {|(fn -> ())()|});

  test "fn no-param lambda has unit -> type" (fun () ->
    expect_type_error {|(fn -> 5) 3|});

  print_summary ()
