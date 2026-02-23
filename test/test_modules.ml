open Test_helpers

let () =
  Printf.printf "=== Module Tests ===\n";

  (* Basic module with pub/private *)
  test "module pub access" (fun () ->
    expect_int {|
      module M =
        pub let x = 42
        let secret = 99
      end
      ;;
      M.x
    |} 42);

  test "module private hidden" (fun () ->
    expect_type_error {|
      module M =
        pub let x = 42
        let secret = 99
      end
      ;;
      M.secret
    |});

  (* Qualified access *)
  test "qualified access" (fun () ->
    expect_int {|
      module Math =
        pub let double x = x + x
        pub let square x = x * x
      end
      ;;
      Math.double 5 + Math.square 3
    |} 19);

  (* Open all *)
  test "open all" (fun () ->
    expect_int {|
      module M =
        pub let a = 10
        pub let b = 20
      end
      ;;
      open M
      ;;
      a + b
    |} 30);

  (* Selective open *)
  test "selective open" (fun () ->
    expect_int {|
      module M =
        pub let a = 10
        pub let b = 20
      end
      ;;
      open M (a)
      ;;
      a
    |} 10);

  test "selective open excludes" (fun () ->
    expect_type_error {|
      module M =
        pub let a = 10
        pub let b = 20
      end
      ;;
      open M (a)
      ;;
      b
    |});

  (* Local open *)
  test "local open" (fun () ->
    expect_int {|
      module M =
        pub let x = 10
        pub let y = 20
      end
      ;;
      M.(x + y)
    |} 30);

  (* Nested module *)
  test "nested module" (fun () ->
    expect_int {|
      module Outer =
        let secret = 100
        pub module Inner =
          pub let f x = x + secret
        end
      end
      ;;
      Outer.Inner.f 5
    |} 105);

  (* Type in module *)
  test "pub type in module" (fun () ->
    expect_int {|
      module M =
        pub type color = Red | Green | Blue
        pub let to_int c = match c with
          | Red -> 0
          | Green -> 1
          | Blue -> 2
      end
      ;;
      M.to_int M.Green
    |} 1);

  (* Opaque type *)
  test "opaque type visible" (fun () ->
    expect_int {|
      module M =
        opaque type token = Wrap of int
        pub let make x = Wrap x
        pub let get h = match h with
          | Wrap n -> n
      end
      ;;
      M.get (M.make 42)
    |} 42);

  test "opaque type constructor hidden" (fun () ->
    expect_type_error {|
      module M =
        opaque type token = Wrap of int
        pub let make x = Wrap x
      end
      ;;
      M.Wrap 42
    |});

  (* Module with pub function using private helper *)
  test "pub function uses private helper" (fun () ->
    expect_int {|
      module M =
        let helper x = x * 2
        pub let double_add x y = helper x + helper y
      end
      ;;
      M.double_add 3 4
    |} 14);

  (* Instance in module always public *)
  test "instance in module is public" (fun () ->
    expect_string {|
      module M =
        pub type wrapper = W of int
        instance Show wrapper =
          let show w = match w with
            | W n -> $"W({n})"
        end
      end
      ;;
      show (M.W 42)
    |} "W(42)");

  (* Stdlib qualified access *)
  test "Stdlib.mod works" (fun () ->
    expect_int {|Stdlib.mod 10 3|} 1);

  test "Stdlib.string_of_int" (fun () ->
    expect_string {|Stdlib.string_of_int 42|} "42");

  (* Shadowing + disambiguation *)
  test "shadowing with qualified disambiguation" (fun () ->
    expect_int {|
      module M =
        pub let x = 10
      end
      ;;
      open M
      ;;
      let x = 20
      ;;
      x + M.x
    |} 30);

  (* Multiple modules *)
  test "multiple modules" (fun () ->
    expect_int {|
      module A =
        pub let x = 1
      end
      ;;
      module B =
        pub let x = 2
      end
      ;;
      A.x + B.x
    |} 3);

  (* Module with recursive function *)
  test "module with rec function" (fun () ->
    expect_int {|
      module M =
        pub let rec fact n = if n <= 1 do 1 else n * fact (n - 1)
      end
      ;;
      M.fact 5
    |} 120);

  (* Open in module body *)
  test "open inside module" (fun () ->
    expect_int {|
      module A =
        pub let x = 10
      end
      ;;
      module B =
        open A
        pub let y = x + 5
      end
      ;;
      B.y
    |} 15);

  Printf.printf "\n=== Module-Scoped Record Type Tests ===\n";

  test "module record type annotation" (fun () ->
    expect_int {|
      module Pt =
        type t = { x: int; y: int }
        pub let make (x: int) (y: int) : t = { x = x; y = y }
        pub let get_x (p: t) : int = p.x
      end
      Pt.get_x (Pt.make 10 20)
    |} 10);

  test "module record type qualified annotation" (fun () ->
    expect_int {|
      module Pt =
        type t = { x: int; y: int }
        pub let make (x: int) (y: int) : t = { x = x; y = y }
      end
      let get_y (p: Pt.t) : int = p.y in
      get_y (Pt.make 10 20)
    |} 20);

  Printf.printf "\n=== Parameterized Qualified Type Tests ===\n";

  test "parameterized qualified type annotation" (fun () ->
    expect_int {|
      module Wrap =
        type 'a t = Val of 'a | Empty
        pub let make (x: 'a) : 'a t = Val x
        pub let get (w: 'a t) : int =
          match w with
          | Val x -> x
          | Empty -> 0
      end
      let unwrap (w: int Wrap.t) : int = Wrap.get w in
      unwrap (Wrap.make 42)
    |} 42);

  test "multi-param qualified type annotation" (fun () ->
    expect_int {|
      module Pair =
        type ('a, 'b) t = P of 'a * 'b | None
        pub let make (x: 'a) (y: 'b) : ('a, 'b) t = P (x, y)
        pub let fst (p: ('a, 'b) t) : 'a =
          match p with
          | P (a, _) -> a
          | None -> 0
      end
      let f (p: (int, string) Pair.t) : int = Pair.fst p in
      f (Pair.make 42 "hi")
    |} 42);

  Printf.printf "\n=== Record Field Inference Tests ===\n";

  test "record field inference basic" (fun () ->
    expect_int {|
      type point = { x: int; y: int }
      let get_x p = p.x in
      get_x { x = 42; y = 0 }
    |} 42);

  test "record field inference in function" (fun () ->
    expect_int {|
      type point = { x: int; y: int }
      let sum p = p.x + p.y in
      sum { x = 20; y = 22 }
    |} 42);

  test "ambiguous field resolves structurally" (fun () ->
    expect_string {|
      type a = { name: string; x: int }
      type b = { name: string; y: int }
      let f p = p.name in
      f { name = "hi"; x = 1 }
    |} "hi");

  Printf.printf "\n=== Field Accessor Tests ===\n";

  test "field accessor getter" (fun () ->
    expect_int {|
      type point = {x: int; y: int}
      let p = {x = 10; y = 20} in
      .x p
    |} 10);

  test "field accessor getter y" (fun () ->
    expect_int {|
      type point = {x: int; y: int}
      .y {x = 10; y = 20}
    |} 20);

  test "field accessor in map" (fun () ->
    expect_stdlib_string {|
      type point = {x: int; y: int}
      let pts = [{x=1;y=2}; {x=3;y=4}; {x=5;y=6}] in
      show (List.map .x pts)
    |} "[1; 3; 5]");

  test "field accessor passed to higher-order" (fun () ->
    expect_stdlib_int {|
      type point = {x: int; y: int}
      let pts = [{x=10;y=1}; {x=20;y=2}; {x=30;y=3}] in
      List.fold (fn acc p -> acc + .x p) 0 pts
    |} 60);

  test "field accessor structural subtyping" (fun () ->
    expect_int {|
      type big = {a: int; b: int; c: int}
      .a {a = 99; b = 0; c = 0}
    |} 99);

  test "field accessor polymorphic ambiguous" (fun () ->
    expect_int {|
      type a = {x: int; y: int}
      type b = {x: string; z: bool}
      .x {x = 42; y = 0}
    |} 42);

  test "field accessor polymorphic different types" (fun () ->
    expect_string {|
      type a = {x: int; y: int}
      type b = {x: string; z: bool}
      .x {x = "hello"; z = true}
    |} "hello");

  test "field accessor postfix still works" (fun () ->
    expect_int {|
      type point = {x: int; y: int}
      let p = {x = 42; y = 0} in
      p.x
    |} 42);

  Printf.printf "\n=== Pipe Operator Tests ===\n";

  test "basic pipe" (fun () ->
    expect_int {|1 |> fn x -> x + 1|} 2);

  test "pipe with named function" (fun () ->
    expect_int {|
      let double x = x * 2
      ;;
      5 |> double
    |} 10);

  test "chained pipes" (fun () ->
    expect_int {|
      let add1 x = x + 1
      ;;
      let double x = x * 2
      ;;
      3 |> add1 |> double
    |} 8);

  test "pipe precedence lower than arithmetic" (fun () ->
    expect_int {|1 + 2 |> fn x -> x * 10|} 30);

  test "pipe with partial application" (fun () ->
    expect_int {|
      let add a b = a + b
      ;;
      5 |> add 3
    |} 8);

  test "pipe with string" (fun () ->
    expect_string {|
      let exclaim s = s ^ "!"
      ;;
      "hello" |> exclaim
    |} "hello!");

  Printf.printf "\n=== String Interpolation Tests ===\n";

  test "basic interpolation" (fun () ->
    expect_string {|$"value is {42}"|} "value is 42");

  test "interpolation with variable" (fun () ->
    expect_string {|let x = 10 in $"x = {x}"|} "x = 10");

  test "interpolation with expression" (fun () ->
    expect_string {|$"{1 + 2 + 3}"|} "6");

  test "multiple interpolations" (fun () ->
    expect_string {|$"{42} and {true}"|} "42 and true");

  test "interpolation no expressions" (fun () ->
    expect_string {|$"hello world"|} "hello world");

  test "interpolation escaped braces" (fun () ->
    expect_string {|$"a \{ b \} c"|} "a { b } c");

  test "interpolation with show string" (fun () ->
    expect_string {|let name = "world" in $"hello {name}"|} "hello world");

  Printf.printf "\n=== Format Specifier Tests ===\n";

  test "format float .2f" (fun () ->
    expect_string {|$"{3.14159:.2f}"|} "3.14");

  test "format float .0f" (fun () ->
    expect_string {|$"{3.14159:.0f}"|} "3");

  test "format float .4f" (fun () ->
    expect_string {|$"{2.5:.4f}"|} "2.5000");

  test "format hex lowercase" (fun () ->
    expect_string {|$"{255:x}"|} "ff");

  test "format hex uppercase" (fun () ->
    expect_string {|$"{255:X}"|} "FF");

  test "format octal" (fun () ->
    expect_string {|$"{8:o}"|} "10");

  test "format binary" (fun () ->
    expect_string {|$"{10:b}"|} "1010");

  test "format binary zero" (fun () ->
    expect_string {|$"{0:b}"|} "0");

  test "format zero-pad hex" (fun () ->
    expect_string {|$"{42:08x}"|} "0000002a");

  test "format right-align" (fun () ->
    expect_string {|$"{42:>10}"|} "        42");

  test "format left-align" (fun () ->
    expect_string {|let s = "hi" in $"{s:<10}"|} "hi        ");

  test "format combined align and float" (fun () ->
    expect_string {|$"{3.14159:>10.2f}"|} "      3.14");

  test "format no spec backward compat" (fun () ->
    expect_string {|$"{1 + 2}"|} "3");

  test "format spec with variable" (fun () ->
    expect_string {|let pi = 3.14159 in $"pi = {pi:.2f}"|} "pi = 3.14");

  test "format colon inside parens preserved" (fun () ->
    expect_string {|$"{(42 : int)}"|} "42");

  test "format type error float spec on int" (fun () ->
    expect_type_error {|$"{42:.2f}"|});

  test "format type error hex on float" (fun () ->
    expect_type_error {|$"{3.14:x}"|});

  test "format mixed literal and formatted" (fun () ->
    expect_string {|$"hex: {255:x}, dec: {255}"|} "hex: ff, dec: 255");

  (* Mutual recursion inside modules *)
  test "module let rec and" (fun () ->
    expect_int {|
      module M =
        pub let rec is_even n =
          if n = 0 do true else is_odd (n - 1)
        and is_odd n =
          if n = 0 do false else is_even (n - 1)
      end
      ;;
      if M.is_even 10 do 1 else 0
    |} 1);

  test "module let rec and odd" (fun () ->
    expect_int {|
      module M =
        pub let rec is_even n =
          if n = 0 do true else is_odd (n - 1)
        and is_odd n =
          if n = 0 do false else is_even (n - 1)
      end
      ;;
      if M.is_odd 7 do 1 else 0
    |} 1);

  test "module let rec and private" (fun () ->
    expect_int {|
      module M =
        let rec helper n =
          if n = 0 do 0 else n + helper2 (n - 1)
        and helper2 n =
          if n = 0 do 0 else n + helper (n - 1)
        pub let compute n = helper n
      end
      ;;
      M.compute 5
    |} 15);

  test "module let rec and with open" (fun () ->
    expect_int {|
      module M =
        pub let rec f n =
          if n <= 0 do 0 else n + g (n - 1)
        and g n =
          if n <= 0 do 0 else f (n - 1)
      end
      ;;
      open M
      ;;
      f 4
    |} 6);

  test "module let rec and three functions" (fun () ->
    expect_int {|
      module M =
        pub let rec a n =
          if n <= 0 do 1 else b (n - 1)
        and b n =
          if n <= 0 do 2 else c (n - 1)
        and c n =
          if n <= 0 do 3 else a (n - 1)
      end
      ;;
      M.a 0 + M.a 1 + M.a 2 + M.a 3
    |} 7);

  print_summary ()
