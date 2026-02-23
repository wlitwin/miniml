open Test_helpers

let () =
  Printf.printf "=== Polymorphism Tests ===\n";

  test "identity function" (fun () ->
    expect_int "let id x = x in id 5" 5);
  test "polymorphic identity" (fun () ->
    expect_int "let id x = x in let a = id 5 in let _ = id true in a" 5);
  test "polymorphic identity bool" (fun () ->
    expect_bool "let id x = x in let _ = id 5 in id true" true);
  test "unannotated function" (fun () ->
    expect_int "let f x = x + 1 in f 10" 11);
  test "unannotated multi-arg" (fun () ->
    expect_int "let add x y = x + y in add 3 4" 7);
  test "inferred higher-order" (fun () ->
    expect_int "let apply f x = f x in apply (fn x -> x + 1) 5" 6);
  test "generic list map" (fun () ->
    let result = Interpreter.Interp.run_string {|
      let rec map f xs =
        match xs with
        | [] -> []
        | x :: rest -> f x :: map f rest
      in map (fn x -> x + 1) [1; 2; 3]
    |} in
    match result with
    | Interpreter.Bytecode.VList [VInt 2; VInt 3; VInt 4] -> ()
    | _ -> failwith "unexpected list value");
  test "generic list length" (fun () ->
    expect_int {|
      let rec length xs =
        match xs with
        | [] -> 0
        | _ :: rest -> 1 + length rest
      in length [10; 20; 30; 40]
    |} 4);
  test "polymorphic length on strings" (fun () ->
    expect_int {|
      let rec length xs =
        match xs with
        | [] -> 0
        | _ :: rest -> 1 + length rest
      in length ["a"; "b"; "c"]
    |} 3);
  test "generic fold" (fun () ->
    expect_int {|
      let rec fold f acc xs =
        match xs with
        | [] -> acc
        | x :: rest -> fold f (f acc x) rest
      in fold (fn a b -> a + b) 0 [1; 2; 3; 4; 5]
    |} 15);
  test "inferred let rec" (fun () ->
    expect_int {|
      let rec fact n =
        if n <= 1 do 1 else n * fact (n - 1)
      in fact 5
    |} 120);
  test "empty list unifies" (fun () ->
    expect_int {|
      let rec length xs =
        match xs with
        | [] -> 0
        | _ :: rest -> 1 + length rest
      in length []
    |} 0);
  test "tyvar annotation" (fun () ->
    expect_int "let id (x: 'a) : 'a = x in id 42" 42);

  Printf.printf "\n=== Polymorphic Instance Tests ===\n";

  test "fold on int list" (fun () ->
    expect_int {|
      fold (+) 0 [1; 2; 3]
    |} 6);

  test "fold on string list" (fun () ->
    expect_string {|
      fold (^) "" ["a"; "b"; "c"]
    |} "abc");

  Printf.printf "\n=== Type Alias Tests ===\n";

  test "basic type alias" (fun () ->
    expect_string {|
      type name = string;;
      let greet (n: name) : name = "hello " ^ n;;
      greet "world"
    |} "hello world");
  test "alias to tuple" (fun () ->
    expect_int {|
      type point = int * int;;
      let p : point = (3, 4);;
      let (x, y) = p;;
      x + y
    |} 7);
  test "parameterized type alias" (fun () ->
    expect_int {|
      type 'a pair = 'a * 'a;;
      let p : int pair = (10, 20);;
      let (a, b) = p;;
      a + b
    |} 30);
  test "multi-param type alias" (fun () ->
    expect_int {|
      type ('a, 'b) assoc = ('a * 'b) list;;
      let xs : (string, int) assoc = [("a", 1); ("b", 2)];;
      let rec len l = match l with | [] -> 0 | _ :: t -> 1 + len t;;
      len xs
    |} 2);
  test "alias to variant type" (fun () ->
    expect_int {|
      type 'a maybe = 'a option;;
      let x : int maybe = Some 42;;
      match x with
      | Some v -> v
      | None -> 0
    |} 42);
  test "alias in function signature" (fun () ->
    expect_int {|
      type age = int;;
      let birthday (a: age) : age = a + 1;;
      birthday 25
    |} 26);
  test "alias wrong arg count error" (fun () ->
    expect_type_error {|
      type 'a box = 'a list;;
      let x : box = [1; 2];;
      x
    |});

  Printf.printf "\n=== Parameterized Type Tests ===\n";

  test "basic parameterized type" (fun () ->
    expect_int {|
      type 'a option = None | Some of 'a;;
      match Some 42 with
        Some v -> v
        | None -> 0
    |} 42);

  test "polymorphic parameterized usage" (fun () ->
    expect_bool {|
      type 'a option = None | Some of 'a;;
      let is_some x = match x with Some _ -> true | None -> false;;
      is_some (Some 42) && is_some (Some "hello")
    |} true);

  test "recursive parameterized type" (fun () ->
    expect_int {|
      type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree);;
      let rec sum t = match t with
        Leaf n -> n
        | Node p -> sum (match p with (l, _) -> l) + sum (match p with (_, r) -> r)
      ;;
      sum (Node (Node (Leaf 1, Leaf 2), Leaf 3))
    |} 6);

  test "multi-param type" (fun () ->
    expect_int {|
      type ('a, 'b) either = Left of 'a | Right of 'b;;
      let get_left e = match e with Left v -> v | Right _ -> 0;;
      get_left (Left 42)
    |} 42);

  test "parameterized type annotation" (fun () ->
    expect_int {|
      type 'a option = None | Some of 'a;;
      let unwrap (x : int option) = match x with Some v -> v | None -> 0;;
      unwrap (Some 99)
    |} 99);

  test "iter on parameterized tree" (fun () ->
    expect_int {|
      type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree);;
      instance Iter ('a tree) 'a =
        let fold f acc t = match t with
          Leaf v -> f acc v
          | Node p -> let left = match p with (l, _) -> l in
                      let right = match p with (_, r) -> r in
                      fold f (fold f acc left) right
      end
      let t = Node (Node (Leaf 1, Leaf 2), Leaf 3);;
      for x in t with sum = 0 do sum + x end
    |} 6);

  print_summary ()
