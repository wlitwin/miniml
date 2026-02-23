open Test_helpers

let () =
  Printf.printf "=== Type Class Tests ===\n";

  test "basic class and instance" (fun () ->
    expect_string "show 42" "42");

  test "multiple instances" (fun () ->
    expect_string {|show 42 ^ " " ^ show true|} "42 true");

  test "class method in annotated function" (fun () ->
    expect_string {|
      let show_int (x: int) : string = show x
      ;;
      show_int 99
    |} "99");

  test "show float instance" (fun () ->
    expect_string "show 3.14" "3.14");

  test "class method with let binding" (fun () ->
    expect_string "let s = show 100 in s" "100");

  test "multi-method class" (fun () ->
    expect_string {|
      class Describable 'a =
        describe : 'a -> string
        label : 'a -> string
      end
      instance Describable int =
        let describe x = string_of_int x
        let label _ = "integer"
      end
      label 0 ^ ": " ^ describe 42
    |} "integer: 42");

  test "no instance error" (fun () ->
    try
      let _ = Interpreter.Interp.run_string {|
        class Printable 'a =
          display : 'a -> string
        end
        display 42
      |} in
      failwith "expected error"
    with Interpreter.Interp.Error _ -> ());

  test "duplicate instance error" (fun () ->
    try
      let _ = Interpreter.Interp.run_string {|
        class Printable 'a =
          display : 'a -> string
        end
        instance Printable int =
          let display x = string_of_int x
        end
        instance Printable int =
          let display x = string_of_int x
        end
      |} in
      failwith "expected error"
    with Interpreter.Interp.Error _ -> ());

  test "instance method type mismatch" (fun () ->
    try
      let _ = Interpreter.Interp.run_string {|
        class Printable 'a =
          display : 'a -> string
        end
        instance Printable int =
          let display x = x + 1
        end
      |} in
      failwith "expected type error"
    with Interpreter.Interp.Error _ -> ());

  test "class method passed to higher-order" (fun () ->
    let result = Interpreter.Interp.run_string {|
      let rec map f xs =
        match xs with
        | [] -> []
        | x :: rest -> f x :: map f rest
      in map show [1; 2; 3]
    |} in
    match result with
    | Interpreter.Bytecode.VList [VString "1"; VString "2"; VString "3"] -> ()
    | _ -> failwith "unexpected value");

  Printf.printf "\n=== Multi-Parameter Type Class Tests ===\n";

  test "multi-param class basic" (fun () ->
    expect_string {|
      class Convert 'a 'b =
        convert : 'a -> 'b
      end
      instance Convert int string =
        let convert (x: int) = string_of_int x
      end
      convert 42
    |} "42");

  test "multi-param class both params in method" (fun () ->
    expect_int {|
      class Container 'c 'e =
        size : 'c -> int
        first : 'c -> 'e
      end
      instance Container (int list) int =
        let size (xs: int list) =
          let rec len acc ys = match ys with
            | [] -> acc
            | _ :: rest -> len (acc + 1) rest
          in len 0 xs
        let first (xs: int list) = match xs with
          | x :: _ -> x
          | [] -> 0
      end
      first [10; 20; 30] + size [1; 2; 3]
    |} 13);

  test "multi-param class with extra method tyvar" (fun () ->
    expect_int {|
      class Fold 'a 'e =
        fold_left : ('b -> 'e -> 'b) -> 'b -> 'a -> 'b
      end
      instance Fold (int list) int =
        let fold_left f init xs =
          let rec go acc ys = match ys with
            | [] -> acc
            | y :: rest -> go (f acc y) rest
          in go init xs
      end
      fold_left (fn a b -> a + b) 0 [1; 2; 3; 4; 5]
    |} 15);

  test "multi-param fold to string" (fun () ->
    expect_string {|
      class Fold 'a 'e =
        fold_left : ('b -> 'e -> 'b) -> 'b -> 'a -> 'b
      end
      instance Fold (int list) int =
        let fold_left f init xs =
          let rec go acc ys = match ys with
            | [] -> acc
            | y :: rest -> go (f acc y) rest
          in go init xs
      end
      fold_left (fn acc n -> acc ^ string_of_int n) "" [1; 2; 3]
    |} "123");

  Printf.printf "\n=== Operator Typeclass Tests ===\n";

  (* Unified + for float *)
  test "float addition with +" (fun () ->
    expect_float {|2.0 + 3.0|} 5.0);

  test "float subtraction with -" (fun () ->
    expect_float {|10.0 - 4.0|} 6.0);

  test "float multiplication with *" (fun () ->
    expect_float {|3.0 * 2.0|} 6.0);

  test "float division with /" (fun () ->
    expect_float {|10.0 / 4.0|} 2.5);

  test "float negation" (fun () ->
    expect_float {|-(3.5)|} (-3.5));

  (* Bitwise operators *)
  test "bitwise land" (fun () ->
    expect_int {|7 land 3|} 3);

  test "bitwise lor" (fun () ->
    expect_int {|5 lor 3|} 7);

  test "bitwise lxor" (fun () ->
    expect_int {|5 lxor 3|} 6);

  test "bitwise lsl" (fun () ->
    expect_int {|1 lsl 3|} 8);

  test "bitwise lsr" (fun () ->
    expect_int {|8 lsr 2|} 2);

  test "bitwise lnot" (fun () ->
    expect_int {|lnot 0|} (-1));

  (* First-class operators through typeclasses *)
  test "first-class (+) int" (fun () ->
    expect_int {|let f = (+) in f 3 4|} 7);

  test "first-class (*) int" (fun () ->
    expect_int {|let f = (*) in f 5 6|} 30);

  test "first-class (=) int" (fun () ->
    expect_bool {|let f = (=) in f 3 3|} true);

  test "first-class (<) int" (fun () ->
    expect_bool {|let f = (<) in f 3 5|} true);

  test "first-class (land)" (fun () ->
    expect_int {|let f = (land) in f 7 3|} 3);

  (* fold with (+) *)
  test "fold with (+)" (fun () ->
    expect_int {|fold (+) 0 [1; 2; 3; 4]|} 10);

  (* String comparison *)
  test "string less than" (fun () ->
    expect_bool {|"abc" < "def"|} true);

  test "string greater than" (fun () ->
    expect_bool {|"xyz" > "abc"|} true);

  test "string le equal" (fun () ->
    expect_bool {|"abc" <= "abc"|} true);

  test "string ge" (fun () ->
    expect_bool {|"abc" >= "abd"|} false);

  (* Custom Num instance *)
  test "custom Num instance" (fun () ->
    expect_int {|
      type vec2 = { x: int; y: int };;
      instance Num vec2 =
        let (+) a b = { x = a.x + b.x; y = a.y + b.y }
        let (-) a b = { x = a.x - b.x; y = a.y - b.y }
        let (*) a b = { x = a.x * b.x; y = a.y * b.y }
        let (/) a b = { x = a.x / b.x; y = a.y / b.y }
        let neg a = { x = 0 - a.x; y = 0 - a.y }
      end
      let v1 = { x = 1; y = 2 };;
      let v2 = { x = 3; y = 4 };;
      let v3 = v1 + v2;;
      v3.x + v3.y
    |} 10);

  (* Custom Eq instance *)
  test "custom Eq instance" (fun () ->
    expect_bool {|
      type point = { px: int; py: int };;
      instance Eq point =
        let (=) a b = a.px = b.px
        let (<>) a b = a.px <> b.px
      end
      let p1 = { px = 1; py = 2 };;
      let p2 = { px = 1; py = 99 };;
      p1 = p2
    |} true);

  (* Custom Ord instance *)
  test "custom Ord instance" (fun () ->
    expect_bool {|
      type score = { val_: int };;
      instance Ord score =
        let (<) a b = a.val_ < b.val_
        let (>) a b = a.val_ > b.val_
        let (<=) a b = a.val_ <= b.val_
        let (>=) a b = a.val_ >= b.val_
      end
      let s1 = { val_ = 10 };;
      let s2 = { val_ = 20 };;
      s1 < s2
    |} true);

  (* Structural equality still works on built-in types *)
  test "structural equality list" (fun () ->
    expect_bool {|[1; 2; 3] = [1; 2; 3]|} true);

  test "structural equality tuple" (fun () ->
    expect_bool {|(1, "hello") = (1, "hello")|} true);

  test "structural inequality" (fun () ->
    expect_bool {|[1; 2] <> [1; 3]|} true);

  Printf.printf "\n=== Typeclass Constraint Tests ===\n";

  test "basic constrained function" (fun () ->
    expect_string {|
      let show_twice (x: 'a) : string where Show 'a = show x ^ show x
      ;;
      show_twice 42
    |} "4242");

  test "constrained function with string" (fun () ->
    expect_string {|
      let show_twice (x: 'a) : string where Show 'a = show x ^ show x
      ;;
      show_twice "hi"
    |} "hihi");

  test "multiple constraints" (fun () ->
    expect_string {|
      let show_eq (a: 'a) (b: 'a) : string where Show 'a, Eq 'a =
        if a = b do show a else show a ^ " <> " ^ show b
      ;;
      show_eq 1 2
    |} "1 <> 2");

  test "multiple constraints equal" (fun () ->
    expect_string {|
      let show_eq (a: 'a) (b: 'a) : string where Show 'a, Eq 'a =
        if a = b do show a else show a ^ " <> " ^ show b
      ;;
      show_eq 3 3
    |} "3");

  test "constrained function calling constrained function" (fun () ->
    expect_string {|
      let show_twice (x: 'a) : string where Show 'a = show x ^ show x
      ;;
      let show_four (x: 'a) : string where Show 'a = show_twice x ^ show_twice x
      ;;
      show_four 1
    |} "1111");

  test "constrained Num operator" (fun () ->
    expect_int {|
      let add_twice (x: 'a) : 'a where Num 'a = x + x
      ;;
      add_twice 5
    |} 10);

  test "constrained Num with float" (fun () ->
    expect_float {|
      let add_twice (x: 'a) : 'a where Num 'a = x + x
      ;;
      add_twice 2.5
    |} 5.0);

  test "constrained instance (dictionary factory)" (fun () ->
    expect_string {|
      type 'a box = Box of 'a
      ;;
      instance Show ('a box) where Show 'a =
        let show b =
          match b with
          | Box x -> "Box(" ^ show x ^ ")"
      end
      show (Box 42)
    |} "Box(42)");

  test "constrained instance nested" (fun () ->
    expect_string {|
      type 'a box = Box of 'a
      ;;
      instance Show ('a box) where Show 'a =
        let show b =
          match b with
          | Box x -> "Box(" ^ show x ^ ")"
      end
      show (Box (Box 7))
    |} "Box(Box(7))");

  Printf.printf "\n=== Deriving Tests ===\n";

  test "deriving Show simple variant" (fun () ->
    expect_string {|
      type color = Red | Green | Blue deriving Show
      show Red
    |} "Red");

  test "deriving Show variant with payload" (fun () ->
    expect_string {|
      type shape = Circle of int | Rect of int * int deriving Show
      show (Circle 5)
    |} "Circle(5)");

  test "deriving Show variant multi-field" (fun () ->
    expect_string {|
      type shape = Circle of int | Rect of int * int deriving Show
      show (Rect (3, 4))
    |} "Rect(3, 4)");

  test "deriving Show record" (fun () ->
    expect_string {|
      type point = { x: int; y: int } deriving Show
      show { x = 10; y = 20 }
    |} "{ x = 10; y = 20 }");

  test "deriving Show parameterized type" (fun () ->
    expect_string {|
      type 'a box = Wrap of 'a deriving Show
      show (Wrap 42)
    |} "Wrap(42)");

  test "deriving Eq simple variant" (fun () ->
    expect_bool {|
      type color = Red | Green | Blue deriving Eq
      Red = Red
    |} true);

  test "deriving Eq variant different" (fun () ->
    expect_bool {|
      type color = Red | Green | Blue deriving Eq
      Red = Blue
    |} false);

  test "deriving Eq variant with payload" (fun () ->
    expect_bool {|
      type shape = Circle of int | Rect of int * int deriving Eq
      Circle 5 = Circle 5
    |} true);

  test "deriving Eq variant payload different" (fun () ->
    expect_bool {|
      type shape = Circle of int | Rect of int * int deriving Eq
      Circle 5 = Circle 3
    |} false);

  test "deriving Eq record" (fun () ->
    expect_bool {|
      type point = { x: int; y: int } deriving Eq
      { x = 1; y = 2 } = { x = 1; y = 2 }
    |} true);

  test "deriving Eq record different" (fun () ->
    expect_bool {|
      type point = { x: int; y: int } deriving Eq
      { x = 1; y = 2 } = { x = 1; y = 3 }
    |} false);

  test "deriving Show and Eq" (fun () ->
    expect_string {|
      type color = Red | Green | Blue deriving Show, Eq
      show Red
    |} "Red");

  test "deriving Show and Eq both work" (fun () ->
    expect_string {|
      type color = Red | Green | Blue deriving Show, Eq
      if Red = Red do show Green else show Blue
    |} "Green");

  test "deriving Eq neq method" (fun () ->
    expect_bool {|
      type color = Red | Green | Blue deriving Eq
      Red <> Blue
    |} true);

  Printf.printf "\n=== Module-Scoped Type Class Tests ===\n";

  test "class in module doesn't leak to global" (fun () ->
    expect_stdlib_int {|
      module Foo =
        pub class Computable 'a =
          compute : 'a -> int
        end
        instance Computable int =
          let compute x = x * 2
        end
      end

      Foo.compute 21
    |} 42);

  test "module class instance works" (fun () ->
    expect_stdlib_string {|
      module MyMod =
        pub class Showable 'a =
          display : 'a -> string
        end
        instance Showable int =
          let display x = "int:" ^ string_of_int x
        end
      end

      MyMod.display 42
    |} "int:42");

  test "open module imports class" (fun () ->
    expect_stdlib_string {|
      module MyMod =
        pub class Stringify 'a =
          to_str : 'a -> string
        end
        instance Stringify int =
          let to_str x = string_of_int x
        end
      end

      open MyMod
      to_str 99
    |} "99");

  test "two modules same class name no conflict" (fun () ->
    expect_stdlib_int {|
      module A =
        pub class Processor 'a =
          process_a : 'a -> int
        end
        instance Processor int =
          let process_a x = x + 1
        end
      end

      module B =
        pub class Processor 'a =
          process_b : 'a -> int
        end
        instance Processor int =
          let process_b x = x * 10
        end
      end

      A.process_a 5 + B.process_b 3
    |} 36);

  test "constrained function inside module" (fun () ->
    expect_stdlib_string {|
      module PP =
        pub class Pretty 'a =
          pretty : 'a -> string
        end
        instance Pretty int =
          let pretty x = "<" ^ string_of_int x ^ ">"
        end

        pub let show_pretty (x : 'a) : string where Pretty 'a =
          pretty x
      end

      PP.show_pretty 42
    |} "<42>");

  test "qualified instance declaration" (fun () ->
    expect_stdlib_string {|
      module PP =
        pub class Pretty 'a =
          pretty : 'a -> string
        end
        instance Pretty int =
          let pretty x = "[" ^ string_of_int x ^ "]"
        end
      end

      instance PP.Pretty string =
        let pretty s = "(" ^ s ^ ")"
      end
      PP.pretty "hello"
    |} "(hello)");

  test "module class browse" (fun () ->
    let _ = run_stdlib {|
      module TestMod =
        pub class Render 'a =
          render : 'a -> string
        end
        instance Render int =
          let render x = string_of_int x
        end
      end
      ()
    |} in
    let state = match !(Interpreter.Interp.eval_state) with
      | Some s -> s | None -> failwith "no state" in
    let browse = Interpreter.Interp.browse_module state "TestMod" in
    assert (contains_substring browse "class Render");
    assert (contains_substring browse "render"));

  Printf.printf "\n=== Show Compound Types Tests ===\n";

  test "show int list" (fun () ->
    expect_stdlib_string {|show [1; 2; 3]|} "[1; 2; 3]");

  test "show string list" (fun () ->
    expect_stdlib_string {|show ["a"; "b"; "c"]|} "[a; b; c]");

  test "show bool list" (fun () ->
    expect_stdlib_string {|show [true; false]|} "[true; false]");

  test "show empty list" (fun () ->
    expect_stdlib_string {|show ([] : int list)|} "[]");

  test "show int array" (fun () ->
    expect_stdlib_string {|show #[10; 20; 30]|} "#[10; 20; 30]");

  test "show empty array" (fun () ->
    expect_stdlib_string {|show (#[] : int array)|} "#[]");

  test "show option some" (fun () ->
    expect_stdlib_string {|show (Some 42)|} "Some 42");

  test "show option none" (fun () ->
    expect_stdlib_string {|show (None : int option)|} "None");

  test "show int pair" (fun () ->
    expect_stdlib_string {|show (1, 2)|} "(1, 2)");

  test "show string pair" (fun () ->
    expect_stdlib_string {|show ("a", "b")|} "(a, b)");

  test "show triple" (fun () ->
    expect_stdlib_string {|show (1, true, "x")|} "(1, true, x)");

  test "show map" (fun () ->
    expect_stdlib_string {|show #{"a": 1}|} {|#{a: 1}|});

  test "show set" (fun () ->
    expect_stdlib_string {|show (Set.of_list [1; 2; 3])|} "#{3; 2; 1}");

  test "show nested list" (fun () ->
    expect_stdlib_string {|show [[1; 2]; [3; 4]]|} "[[1; 2]; [3; 4]]");

  Printf.printf "\n=== Functional Dependency Tests ===\n";

  test "fundep class parsing and method use" (fun () ->
    expect_int {|
      class Collection 'c 'e where 'c -> 'e =
        count : 'c -> int
        head : 'c -> 'e
      end
      instance Collection (int list) int =
        let count xs =
          let rec len acc ys = match ys with
            | [] -> acc
            | _ :: rest -> len (acc + 1) rest
          in len 0 xs
        let head xs = match xs with
          | x :: _ -> x
          | [] -> 0
      end
      head [10; 20; 30] + count [1; 2; 3; 4]
    |} 14);

  test "fundep consistency violation" (fun () ->
    try
      let _ = Interpreter.Interp.run_string {|
        class Assoc 'a 'b where 'a -> 'b =
          assoc_val : 'a -> 'b
        end
        instance Assoc int string =
          let assoc_val x = string_of_int x
        end
        instance Assoc int bool =
          let assoc_val x = x > 0
        end
      |} in
      failwith "expected fundep violation error"
    with Interpreter.Interp.Error msg ->
      assert (contains_substring msg "functional dependency violation"));

  test "fundep with multiple deps" (fun () ->
    expect_string {|
      class BiMap 'a 'b 'c where 'a -> 'b, 'a -> 'c =
        left : 'a -> 'b
        right : 'a -> 'c
      end
      instance BiMap int string bool =
        let left x = string_of_int x
        let right x = x > 0
      end
      if right 5 do left 42 else "no"
    |} "42");

  test "builtin Iter fundep works" (fun () ->
    expect_int {|fold (+) 0 [1; 2; 3; 4; 5]|} 15);

  test "builtin Map fundep works" (fun () ->
    expect_int {|
      let m = of_list [("a", 1); ("b", 2)];;
      match get "a" m with
        | Some v -> v
        | None -> 0
    |} 1);

  test "Map higher-order with fundep" (fun () ->
    expect_int {|
      let lookup key m = match get key m with
        | Some v -> v
        | None -> 0;;
      lookup "x" #{"x": 42}
    |} 42);

  Printf.printf "\n=== Instance Method Annotation Tests ===\n";

  test "instance method with full annotations" (fun () ->
    expect_int {|
      class Updatable 'a =
        update_x : 'a -> int -> 'a
      end
      type point = { x: int; y: int }
      instance Updatable point =
        let update_x (p: point) (n: int) : point = { p with x = n }
      end
      (update_x { x = 0; y = 42 } 10).x + (update_x { x = 0; y = 42 } 10).y
    |} 52);

  test "instance method with return annotation only" (fun () ->
    expect_int {|
      class Updatable 'a =
        update_x : 'a -> int -> 'a
      end
      type point = { x: int; y: int }
      instance Updatable point =
        let update_x p n : point = { p with x = n }
      end
      (update_x { x = 0; y = 42 } 10).x
    |} 10);

  test "instance method with param annotation only" (fun () ->
    expect_string {|
      class Stringify 'a =
        stringify : 'a -> string
      end
      instance Stringify int =
        let stringify (x: int) = string_of_int x
      end
      stringify 42
    |} "42");

  print_summary ()
