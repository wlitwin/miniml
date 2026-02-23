open Test_helpers

let () =
  Printf.printf "\n=== GADT Tests ===\n";

  (* Basic GADT declaration and construction *)
  test "GADT basic declaration and construct" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
      IntLit 42 |> fn (IntLit n) -> n
    |} 42);

  test "GADT construct bool" (fun () ->
    expect_bool {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
      BoolLit true |> fn (BoolLit b) -> b
    |} true);

  test "GADT let function with match" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
      let f (e : int expr) : int =
        match e with
        | IntLit n -> n
      ;;
      f (IntLit 5)
    |} 5);

  (* GADT pattern match with type refinement *)
  test "GADT eval with type refinement" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let eval (e : int expr) : int =
        match e with
        | IntLit n -> n
        | Add (a, b) ->
          let va : int = a |> fn (IntLit n) -> n in
          let vb : int = b |> fn (IntLit n) -> n in
          va + vb
      ;;
      eval (Add (IntLit 10, IntLit 20))
    |} 30);

  (* GADT with no-arg constructor *)
  test "GADT no-arg constructor" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Zero : int expr
      match (Zero : int expr) with
      | IntLit n -> n
      | Zero -> 0
    |} 0);

  (* Type equality witness *)
  test "GADT type equality witness (Refl)" (fun () ->
    expect_int {|
      type ('a, 'b) eq =
        | Refl : ('a, 'a) eq
      let cast (eq : (int, int) eq) (x : int) : int =
        match eq with
        | Refl -> x
      ;;
      cast Refl 42
    |} 42);

  (* Existential types *)
  test "GADT existential type" (fun () ->
    expect_string {|
      type any_show =
        | AnyShow : 'a * ('a -> string) -> any_show
      let to_s (n : int) : string = string_of_int n
      ;;
      let show_any (x : any_show) : string =
        match x with
        | AnyShow (v, f) -> f v
      ;;
      show_any (AnyShow (42, to_s))
    |} "42");

  (* Mixed GADT and regular constructors *)
  test "GADT mixed with regular constructors" (fun () ->
    expect_int {|
      type 'a tagged =
        | IntTag : int -> int tagged
        | Pair of 'a * 'a
      match (IntTag 7 : int tagged) with
      | IntTag n -> n
      | Pair (a, _) -> a
    |} 7);

  (* GADT constructor used as function *)
  test "GADT constructor as function" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
      let make_int = IntLit in
      make_int 99 |> fn (IntLit n) -> n
    |} 99);

  (* Error: deriving on GADT *)
  test "GADT deriving rejected" (fun () ->
    expect_type_error_msg {|
      type 'a expr =
        | IntLit : int -> int expr
        deriving Show
      ()
    |} "cannot derive Show for GADT");

  (* GADT with multiple type params *)
  test "GADT multiple type params" (fun () ->
    expect_int {|
      type ('a, 'b) pair_type =
        | IntStr : int * string -> (int, string) pair_type
        | BoolUnit : bool -> (bool, unit) pair_type
      match (IntStr (5, "hi") : (int, string) pair_type) with
      | IntStr (n, _) -> n
    |} 5);

  (* GADT recursive eval function *)
  test "GADT recursive eval" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec eval_int (e : int expr) : int =
        match e with
        | IntLit n -> n
        | Add (a, b) -> eval_int a + eval_int b
        | If (c, t, f) ->
          if eval_bool c do eval_int t else eval_int f
      and eval_bool (e : bool expr) : bool =
        match e with
        | BoolLit b -> b
        | If (c, t, f) ->
          if eval_bool c do eval_bool t else eval_bool f
      ;;
      eval_int (If (BoolLit true, Add (IntLit 1, IntLit 2), IntLit 0))
    |} 3);

  (* GADT: return type must match the declared type *)
  test "GADT wrong return type name error" (fun () ->
    expect_type_error {|
      type 'a expr =
        | IntLit : int -> int option
      ()
    |});

  (* GADT with partial match that's actually exhaustive *)
  test "GADT exhaustive subset" (fun () ->
    expect_int {|
      type 'a value =
        | VInt : int -> int value
        | VBool : bool -> bool value
        | VStr : string -> string value
      let get_int (v : int value) : int =
        match v with
        | VInt n -> n
      ;;
      get_int (VInt 42)
    |} 42);

  (* GADT existential with multiple constructors *)
  test "GADT existential heterogeneous list" (fun () ->
    expect_int {|
      type any =
        | Any : 'a -> any
      let rec count_anys (xs : any list) : int =
        match xs with
        | [] -> 0
        | _ :: rest -> 1 + count_anys rest
      ;;
      count_anys [Any 1; Any true; Any "hello"]
    |} 3);

  (* === GADT Edge Cases & Feature Interaction Tests === *)
  Printf.printf "\n=== GADT Edge Cases & Feature Interactions ===\n";

  (* --- GADTs + Effects --- *)

  test "GADT + effects: perform inside GADT match arm" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      effect Log =
        log : string -> unit
      end
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n ->
          perform log "lit";
          n
        | Add (a, b) ->
          perform log "add";
          eval a + eval b
      ;;
      handle
        eval (Add (IntLit 1, IntLit 2))
      with
      | return x -> x
      | log _ k -> resume k ()
    |} 3);

  test "GADT + effects: stateful GADT evaluator" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      effect Counter =
        tick : unit -> int
      end
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n -> let _ = perform tick () in n
        | Add (a, b) -> let _ = perform tick () in eval a + eval b
      ;;
      let mut count = 0 in
      let result = handle
        eval (Add (Add (IntLit 1, IntLit 2), IntLit 3))
      with
      | return x -> x
      | tick () k -> count := count + 1; resume k count
      in
      result + count
    |} 11);

  test "GADT + effects: existential with effectful function" (fun () ->
    expect_int {|
      type task =
        | Task : 'a * ('a -> int) -> task
      effect Log =
        log : int -> unit
      end
      let run_task (t : task) : int =
        match t with
        | Task (v, f) ->
          let r = f v in
          perform log r;
          r
      ;;
      let mut logged = 0 in
      handle
        run_task (Task (21, fn x -> x * 2))
      with
      | return x -> x + logged
      | log n k -> logged := logged + n; resume k ()
    |} 84);

  (* --- GADTs + Type Classes --- *)

  test "GADT + type classes: constrained function on GADT contents" (fun () ->
    expect_string {|
      type 'a box =
        | IntBox : int -> int box
        | StrBox : string -> string box
      let show_int_box (b : int box) : string =
        match b with
        | IntBox n -> show n
      ;;
      show_int_box (IntBox 42)
    |} "42");

  test "GADT + type classes: existential with Show constraint" (fun () ->
    expect_string {|
      type showable =
        | MkShowable : 'a * ('a -> string) -> showable
      let show_it (s : showable) : string =
        match s with
        | MkShowable (v, f) -> f v
      ;;
      let s1 = MkShowable (42, show) in
      let s2 = MkShowable (true, show) in
      show_it s1 ^ " " ^ show_it s2
    |} "42 true");

  test "GADT + type classes: custom class method on GADT-wrapped value" (fun () ->
    expect_string {|
      class Describe 'a =
        describe : 'a -> string
      end
      instance Describe int =
        let describe n = "int:" ^ show n
      end
      instance Describe bool =
        let describe b = "bool:" ^ show b
      end
      type 'a box =
        | Box : int -> int box
      let describe_box (b : int box) : string =
        match b with
        | Box n -> describe n
      ;;
      describe_box (Box 99)
    |} "int:99");

  (* --- GADTs + Modules --- *)

  test "GADT inside module" (fun () ->
    expect_int {|
      module Expr =
        pub type 'a t =
          | Lit : int -> int t
          | Add : int t * int t -> int t
        pub let rec eval (e : int t) : int =
          match e with
          | Lit n -> n
          | Add (a, b) -> eval a + eval b
      end
      ;;
      Expr.eval (Expr.Add (Expr.Lit 10, Expr.Lit 20))
    |} 30);

  test "GADT module with open" (fun () ->
    expect_int {|
      module E =
        pub type 'a expr =
          | IntLit : int -> int expr
          | Neg : int expr -> int expr
        pub let rec eval (e : int expr) : int =
          match e with
          | IntLit n -> n
          | Neg x -> 0 - eval x
      end
      ;;
      open E
      ;;
      eval (Neg (IntLit 5))
    |} (-5));

  (* --- GADTs + Pattern Matching Edge Cases --- *)

  test "GADT match with guard" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
      let eval (e : int expr) : int =
        match e with
        | IntLit n when n > 100 -> 100
        | IntLit n -> n
      ;;
      eval (IntLit 200) + eval (IntLit 5)
    |} 105);

  test "GADT match wildcard arm" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
        | Mul : int expr * int expr -> int expr
      let simplify (e : int expr) : int =
        match e with
        | IntLit n -> n
        | _ -> 0
      ;;
      simplify (IntLit 5) + simplify (Add (IntLit 1, IntLit 2))
    |} 5);

  test "GADT nested constructor pattern" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      match (Add (IntLit 3, IntLit 4) : int expr) with
      | Add (IntLit a, IntLit b) -> a + b
      | _ -> 0
    |} 7);

  test "GADT match with nested eval" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n -> n
        | Add (a, b) ->
          let x = eval a in
          let y = eval b in
          x + y
      ;;
      eval (Add (IntLit 10, IntLit 20))
    |} 30);

  (* --- GADTs + Higher-Order Functions --- *)

  test "GADT constructor in map" (fun () ->
    expect_stdlib_int {|
      type 'a expr =
        | IntLit : int -> int expr
      let rec sum_exprs (es : int expr list) : int =
        match es with
        | [] -> 0
        | e :: rest ->
          let n : int = match e with | IntLit n -> n in
          n + sum_exprs rest
      ;;
      let nums = [1; 2; 3; 4; 5] in
      let exprs = List.map IntLit nums in
      sum_exprs exprs
    |} 15);

  test "GADT with pipe operator" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n -> n
        | Add (a, b) -> eval a + eval b
      ;;
      Add (IntLit 10, IntLit 20) |> eval
    |} 30);

  test "GADT existential in list with fold" (fun () ->
    expect_stdlib_int {|
      type any_int =
        | Wrap : 'a * ('a -> int) -> any_int
      let unwrap (w : any_int) : int =
        match w with
        | Wrap (v, f) -> f v
      ;;
      let items = [
        Wrap (10, fn x -> x);
        Wrap ("hi", fn s -> String.length s);
        Wrap (true, fn b -> if b do 1 else 0)
      ] in
      List.fold (fn acc item -> acc + unwrap item) 0 items
    |} 13);

  (* --- GADTs + Closures/Lambdas --- *)

  test "GADT match inside lambda" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
      let f = fn (e : int expr) ->
        match e with
        | IntLit n -> n * 2
      ;;
      f (IntLit 21)
    |} 42);

  test "GADT closure captures GADT value" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
      let e : int expr = IntLit 42
      let f () : int =
        match e with
        | IntLit n -> n
      ;;
      f ()
    |} 42);

  (* --- GADTs + Mutable State --- *)

  test "GADT match result stored in mutable" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n -> n
        | Add (a, b) -> eval a + eval b
      ;;
      let mut result = 0 in
      result := eval (Add (IntLit 3, IntLit 7));
      result
    |} 10);

  test "GADT mutable accumulator" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      ;;
      let mut total = 0 in
      let add_eval (e : int expr) : unit =
        let n : int = match e with
          | IntLit n -> n
          | Add (_, _) -> 0
        in
        total := total + n
      in
      add_eval (IntLit 10);
      add_eval (IntLit 20);
      add_eval (IntLit 30);
      total
    |} 60);

  (* --- GADTs + Mutual Recursion --- *)

  test "GADT mutual recursion with even/odd" (fun () ->
    expect_bool {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | IsEven : int expr -> bool expr
      let rec eval_int (e : int expr) : int =
        match e with
        | IntLit n -> n
      and eval_bool (e : bool expr) : bool =
        match e with
        | BoolLit b -> b
        | IsEven ie ->
          let n = eval_int ie in
          n mod 2 = 0
      ;;
      eval_bool (IsEven (IntLit 4))
    |} true);

  (* --- Multiple Existentials --- *)

  test "GADT multiple existentials" (fun () ->
    expect_stdlib_int {|
      type packed =
        | Pack : 'a * 'b * ('a -> 'b -> int) -> packed
      let run (p : packed) : int =
        match p with
        | Pack (a, b, f) -> f a b
      ;;
      run (Pack (10, "hello", fn n s -> n + String.length s))
    |} 15);

  (* --- GADTs + String Interpolation --- *)

  test "GADT match with string interpolation" (fun () ->
    expect_string {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let rec show_expr (e : int expr) : string =
        match e with
        | IntLit n -> show n
        | Add (a, b) -> $"({show_expr a} + {show_expr b})"
      ;;
      show_expr (Add (IntLit 1, Add (IntLit 2, IntLit 3)))
    |} "(1 + (2 + 3))");

  (* --- GADT Type Safety / Error Cases --- *)

  test "GADT type mismatch: wrong arg count in return type" (fun () ->
    expect_type_error {|
      type 'a expr =
        | Bad : int -> (int, bool) expr
      ()
    |});

  test "GADT deriving Eq also rejected" (fun () ->
    expect_type_error_msg {|
      type 'a expr =
        | IntLit : int -> int expr
        deriving Eq
      ()
    |} "cannot derive Eq for GADT");

  (* --- GADTs + Exhaustiveness Edge Cases --- *)

  test "GADT exhaustive: all constructors for polymorphic match" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
      let size (e : int expr) : int =
        match e with
        | IntLit _ -> 1
        | Add (_, _) -> 2
      ;;
      size (IntLit 0) + size (Add (IntLit 1, IntLit 2))
    |} 3);

  (* --- GADTs + Effects + Type Classes (triple interaction) --- *)

  test "GADT + effects + show: effectful Show on GADT content" (fun () ->
    expect_string {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      effect Trace =
        trace : string -> unit
      end
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n ->
          perform trace (show n);
          n
        | Add (a, b) ->
          perform trace "+";
          eval a + eval b
      ;;
      let mut log = "" in
      let result = handle
        eval (Add (IntLit 3, IntLit 4))
      with
      | return x -> x
      | trace s k -> log := log ^ s ^ " "; resume k ()
      in
      log ^ "= " ^ show result
    |} "+ 3 4 = 7");

  (* --- GADT + List Operations --- *)

  test "GADT values in list operations" (fun () ->
    expect_stdlib_int {|
      type 'a expr =
        | IntLit : int -> int expr
      let eval (e : int expr) : int =
        match e with | IntLit n -> n
      ;;
      let exprs : int expr list = [IntLit 1; IntLit 2; IntLit 3; IntLit 4; IntLit 5] in
      List.map eval exprs |> List.fold (fn a b -> a + b) 0
    |} 15);

  (* --- GADT + Sequence/semicolons --- *)

  test "GADT match in sequence" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
      let e : int expr = IntLit 42
      let _ = "side effect" in
      match e with
      | IntLit n -> n
    |} 42);

  (* --- GADTs with deeply nested types --- *)

  test "GADT with list-typed constructor" (fun () ->
    expect_stdlib_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Sum : int expr list -> int expr
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n -> n
        | Sum es -> List.fold (fn acc e -> acc + eval e) 0 es
      ;;
      eval (Sum [IntLit 1; IntLit 2; IntLit 3])
    |} 6);

  test "GADT with function-typed constructor" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Apply : (int -> int) * int expr -> int expr
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n -> n
        | Apply (f, x) -> f (eval x)
      ;;
      eval (Apply ((fn x -> x * 2), IntLit 21))
    |} 42);

  (* --- GADT no type params (pure existential) --- *)

  test "GADT no type params: pure existential container" (fun () ->
    expect_stdlib_int {|
      type box =
        | Box : 'a * ('a -> int) -> box
      let unbox (b : box) : int =
        match b with
        | Box (v, f) -> f v
      ;;
      let b1 = Box (42, fn n -> n) in
      let b2 = Box ("hello", fn s -> String.length s) in
      unbox b1 + unbox b2
    |} 47);

  (* --- GADT + If expression in match body --- *)

  test "GADT match body with if-else" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
      let clamp (e : int expr) : int =
        match e with
        | IntLit n ->
          if n > 100 do 100
          else if n < 0 do 0
          else n
      ;;
      clamp (IntLit 150) + clamp (IntLit (-5)) + clamp (IntLit 42)
    |} 142);

  (* --- GADT + Tuple returns --- *)

  test "GADT match returning tuple" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let rec eval_counted (e : int expr) : int * int =
        match e with
        | IntLit n -> (n, 1)
        | Add (a, b) ->
          let (va, ca) = eval_counted a in
          let (vb, cb) = eval_counted b in
          (va + vb, ca + cb + 1)
      ;;
      let (value, count) = eval_counted (Add (IntLit 1, Add (IntLit 2, IntLit 3))) in
      value + count
    |} 11);

  (* --- GADT + let bindings inside match arms --- *)

  test "GADT let bindings in match arm" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let rec eval (e : int expr) : int =
        match e with
        | IntLit n ->
          let doubled = n * 2 in
          let halved = doubled / 2 in
          halved
        | Add (a, b) ->
          let left = eval a in
          let right = eval b in
          left + right
      ;;
      eval (Add (IntLit 10, IntLit 20))
    |} 30);

  (* --- GADT + multiple GADT types interacting --- *)

  test "GADT multiple GADT types" (fun () ->
    expect_int {|
      type 'a ty =
        | TInt : int ty
        | TBool : bool ty
      type 'a value =
        | VInt : int -> int value
        | VBool : bool -> bool value
      let default_val (t : int ty) : int =
        match t with
        | TInt -> 0
      ;;
      let get_val (v : int value) : int =
        match v with
        | VInt n -> n
      ;;
      default_val TInt + get_val (VInt 42)
    |} 42);

  (* --- GADT + partial application patterns --- *)

  test "GADT constructor partial application in pipeline" (fun () ->
    expect_stdlib_int {|
      type 'a box =
        | Box : int -> int box
      let unbox (b : int box) : int =
        match b with | Box n -> n
      ;;
      [1; 2; 3] |> List.map Box |> List.map unbox |> List.fold (fn a b -> a + b) 0
    |} 6);

  (* ====== Explicit effect annotations ====== *)

  test "explicit effect annotation: basic / IO" (fun () ->
    expect_int {|
      effect IO =
        print_io : string -> unit
      end
      let f (g : int -> string / IO) : string / IO = g 42
      handle
        f (fn x -> perform print_io "hello"; "done")
      with
      | return x -> 0
      | print_io _ k -> resume k ()
    |} 0);

  test "explicit effect annotation: / pure" (fun () ->
    expect_int {|
      let add (x: int) : int / pure = x + 1
      ;;
      add 41
    |} 42);

  test "explicit effect annotation: pure function prevents effects" (fun () ->
    expect_type_error {|
      effect IO =
        print_io : string -> unit
      end
      let f (x: int) : int / pure =
        perform print_io "bad";
        x
    |});

  test "explicit effect annotation: multiple effects" (fun () ->
    expect_int {|
      effect E1 =
        op1 : unit -> int
      end
      effect E2 =
        op2 : unit -> int
      end
      let f () : int / E1, E2 =
        perform op1 () + perform op2 ()
      handle
        handle
          f ()
        with
        | return x -> x
        | op1 () k -> resume k 10
      with
      | return x -> x
      | op2 () k -> resume k 20
    |} 30);

  test "explicit effect annotation: HOF with effectful callback" (fun () ->
    expect_int {|
      effect Counter =
        inc : unit -> unit
      end
      let apply_f (f : int -> int / Counter) (x : int) : int / Counter = f x
      let mut count = 0 in
      handle
        apply_f (fn x -> perform inc (); x * 2) 21
      with
      | return x -> x + count
      | inc () k -> count := count + 1; resume k ()
    |} 43);

  test "explicit effect annotation: inferred when omitted" (fun () ->
    expect_int {|
      effect Log =
        log : string -> unit
      end
      let f (x : int) : int =
        perform log "test";
        x + 1
      handle
        f 41
      with
      | return x -> x
      | log _ k -> resume k ()
    |} 42);

  (* ====== Parameterized effect declarations ====== *)

  test "parameterized effect: State 'a" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = 0 in
      handle
        let x = perform get () in
        perform put (x + 10);
        perform get ()
      with
      | return x -> x
      | get () k -> resume k st
      | put v k -> st := v; resume k ()
    |} 10);

  test "parameterized effect: two performs share type" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = 100 in
      handle
        perform put 42;
        perform get ()
      with
      | return x -> x
      | get () k -> resume k st
      | put v k -> st := v; resume k ()
    |} 42);

  test "parameterized effect: string state" (fun () ->
    expect_string {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = "hello" in
      handle
        perform put "world";
        perform get ()
      with
      | return x -> x
      | get () k -> resume k st
      | put v k -> st := v; resume k ()
    |} "world");

  test "parameterized effect: with explicit effect annotation" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let increment () : unit / State int =
        let x = perform get () in
        perform put (x + 1)
      let mut st = 0 in
      handle
        increment ();
        increment ();
        increment ();
        perform get ()
      with
      | return x -> x
      | get () k -> resume k st
      | put v k -> st := v; resume k ()
    |} 3);

  (* ====== Effect-polymorphic class methods ====== *)

  test "effect-polymorphic class method: basic" (fun () ->
    expect_int {|
      effect Counter =
        inc : unit -> unit
      end
      class Doer 'a =
        do_thing : 'a -> 'a / 'e
      end
      instance Doer int =
        let do_thing x = perform inc (); x + 1
      end
      let mut count = 0 in
      handle
        do_thing 41
      with
      | return x -> x + count
      | inc () k -> count := count + 1; resume k ()
    |} 43);

  (* ====== Error cases ====== *)

  test "error: unknown effect in annotation" (fun () ->
    expect_type_error_msg {|
      let f (x: int) : int / UnknownEffect = x
    |} "unknown effect");

  test "error: wrong param count for parameterized effect" (fun () ->
    expect_type_error_msg {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f (x: int) : int / State = x
    |} "expects 1 type parameter");

  test "error: extra params for non-parameterized effect" (fun () ->
    expect_type_error_msg {|
      effect IO =
        print_io : string -> unit
      end
      let f (x: int) : int / IO int = x
    |} "expects 0 type parameter");

  (* ====== Integration tests ====== *)

  test "effect annotations with GADTs" (fun () ->
    expect_int {|
      effect Log =
        log : string -> unit
      end
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let rec eval (e : int expr) : int / Log =
        match e with
        | IntLit n -> perform log "lit"; n
        | Add (a, b) -> perform log "add"; eval a + eval b
      handle
        eval (Add (IntLit 1, IntLit 2))
      with
      | return x -> x
      | log _ k -> resume k ()
    |} 3);

  test "effect annotations backward compat: existing code unchanged" (fun () ->
    expect_stdlib_int {|
      effect Ask =
        ask : unit -> string
      end
      handle
        let name = perform ask () in
        String.length name
      with
      | return x -> x
      | ask () k -> resume k "hello"
    |} 5);

  test "parameterized effect: non-parameterized still works" (fun () ->
    expect_int {|
      effect Counter =
        inc : unit -> unit
      end
      let mut n = 0 in
      handle
        perform inc ();
        perform inc ();
        perform inc ();
        42
      with
      | return x -> x + n
      | inc () k -> n := n + 1; resume k ()
    |} 45);

  test "effect annotation on function type in let binding" (fun () ->
    expect_int {|
      effect IO =
        print_io : string -> unit
      end
      let f : int -> int / IO = fn x ->
        perform print_io "hello";
        x + 1
      handle
        f 41
      with
      | return x -> x
      | print_io _ k -> resume k ()
    |} 42);

  test "effect annotation nested arrows" (fun () ->
    expect_int {|
      effect IO =
        print_io : string -> unit
      end
      let apply (f : int -> int / IO) (x : int) : int / IO = f x
      handle
        apply (fn x -> perform print_io "!"; x * 2) 21
      with
      | return x -> x
      | print_io _ k -> resume k ()
    |} 42);

  (* ====== Stress tests: effect annotation edge cases ====== *)

  (* --- Recursive functions with explicit effect annotations --- *)

  test "effect annot: recursive function with / IO" (fun () ->
    expect_int {|
      effect IO =
        emit : int -> unit
      end
      let rec countdown (n: int) : unit / IO =
        if n <= 0 do ()
        else do perform emit n; countdown (n - 1) end
      ;;
      let mut total = 0 in
      handle
        countdown 5
      with
      | return _ -> total
      | emit v k -> total := total + v; resume k ()
    |} 15);

  test "effect annot: mutually recursive with explicit effects" (fun () ->
    expect_int {|
      effect Trace =
        trace : string -> unit
      end
      let rec even (n: int) : bool / Trace =
        if n = 0 do (perform trace "even"; true)
        else odd (n - 1)
      and odd (n: int) : bool / Trace =
        if n = 0 do (perform trace "odd"; false)
        else even (n - 1)
      ;;
      let mut count = 0 in
      handle
        if even 4 do 1 else 0
      with
      | return x -> x + count
      | trace _ k -> count := count + 1; resume k ()
    |} 2);

  (* --- Curried functions: effect on innermost arrow only --- *)

  test "effect annot: curried function, effect on inner arrow" (fun () ->
    expect_int {|
      effect Log =
        log : string -> unit
      end
      let add (x: int) (y: int) : int / Log =
        perform log "adding";
        x + y
      ;;
      let mut logged = 0 in
      handle
        add 20 22
      with
      | return x -> x + logged
      | log _ k -> logged := logged + 1; resume k ()
    |} 43);

  test "effect annot: three-arg curried function" (fun () ->
    expect_int {|
      effect Tick =
        tick : unit -> unit
      end
      let f (a: int) (b: int) (c: int) : int / Tick =
        perform tick ();
        a + b + c
      ;;
      let mut n = 0 in
      handle f 10 20 12 with
      | return x -> x + n
      | tick () k -> n := n + 1; resume k ()
    |} 43);

  (* --- Nested handlers with explicit annotations at each level --- *)

  test "effect annot: nested handlers with different effects" (fun () ->
    expect_int {|
      effect E1 =
        op1 : unit -> int
      end
      effect E2 =
        op2 : unit -> int
      end
      let inner () : int / E1 = perform op1 ()
      let outer () : int / E2 =
        let x = handle inner () with
          | return v -> v
          | op1 () k -> resume k 10
        in
        x + perform op2 ()
      ;;
      handle outer () with
      | return x -> x
      | op2 () k -> resume k 32
    |} 42);

  test "effect annot: deeply nested handlers" (fun () ->
    expect_int {|
      effect A =
        get_a : unit -> int
      end
      effect B =
        get_b : unit -> int
      end
      effect C =
        get_c : unit -> int
      end
      let f () : int / A, B, C =
        perform get_a () + perform get_b () + perform get_c ()
      ;;
      handle
        handle
          handle f () with
          | return x -> x
          | get_a () k -> resume k 10
        with
        | return x -> x
        | get_b () k -> resume k 20
      with
      | return x -> x
      | get_c () k -> resume k 12
    |} 42);

  (* --- Parameterized effects with complex type params --- *)

  test "effect annot: parameterized effect with tuple type" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let swap () : unit / State (int * int) =
        let (a, b) = perform get () in
        perform put (b, a)
      ;;
      let mut st = (1, 42) in
      handle
        swap ();
        let (a, _) = perform get () in
        a
      with
      | return x -> x
      | get () k -> resume k st
      | put v k -> st := v; resume k ()
    |} 42);

  test "effect annot: two different parameterized effects" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      effect Reader 'a =
        ask : unit -> 'a
      end
      let compute () : int / State int, Reader int =
        let base = perform ask () in
        let current = perform get () in
        perform put (current + base);
        perform get ()
      ;;
      let mut st = 0 in
      handle
        handle compute () with
        | return x -> x
        | get () k -> resume k st
        | put v k -> st := v; resume k ()
      with
      | return x -> x
      | ask () k -> resume k 42
    |} 42);

  (* --- HOF with effectful callbacks and annotation unification --- *)

  test "effect annot: HOF applies effectful callback twice" (fun () ->
    expect_int {|
      effect Counter =
        bump : unit -> unit
      end
      let apply_twice (f : int -> int / Counter) (x : int) : int / Counter =
        f (f x)
      ;;
      let mut n = 0 in
      handle
        apply_twice (fn x -> perform bump (); x + 1) 40
      with
      | return x -> x + n
      | bump () k -> n := n + 1; resume k ()
    |} 44);

  test "effect annot: effectful callback in fold pattern" (fun () ->
    expect_int {|
      effect Log =
        log : int -> unit
      end
      let rec my_fold (f : int -> int -> int / Log) (acc : int) (xs : int list) : int / Log =
        match xs with
        | [] -> acc
        | x :: rest -> my_fold f (f acc x) rest
      ;;
      let mut sum_logged = 0 in
      handle
        my_fold (fn acc x -> perform log x; acc + x) 0 [1; 2; 3; 4; 5]
      with
      | return x -> x * 1000 + sum_logged
      | log v k -> sum_logged := sum_logged + v; resume k ()
    |} 15015);

  (* --- Effect annotations in let-in expressions --- *)

  test "effect annot: let-in with explicit effect on local function" (fun () ->
    expect_int {|
      effect E =
        ask : unit -> int
      end
      let result =
        let f (x: int) : int / E = x + perform ask () in
        handle f 10 with
        | return x -> x
        | ask () k -> resume k 32
      ;;
      result
    |} 42);

  (* --- Pure annotation edge cases --- *)

  test "effect annot: pure function calling another pure function" (fun () ->
    expect_int {|
      let double (x: int) : int / pure = x * 2
      let quad (x: int) : int / pure = double (double x)
      ;;
      quad 10
    |} 40);

  test "effect annot: pure prevents transitive effects" (fun () ->
    expect_type_error {|
      effect IO =
        print_io : string -> unit
      end
      let impure (x: int) : int =
        perform print_io "hi"; x
      let should_fail (x: int) : int / pure =
        impure x
    |});

  (* --- Return type is arrow type with effect --- *)

  test "effect annot: function returning effectful closure" (fun () ->
    expect_int {|
      effect IO =
        emit : int -> unit
      end
      let make_emitter (base: int) : int -> unit / IO =
        fn x -> perform emit (base + x)
      ;;
      let mut total = 0 in
      handle
        let f = make_emitter 40 in
        f 2;
        0
      with
      | return _ -> total
      | emit v k -> total := total + v; resume k ()
    |} 42);

  (* --- Effect annotation with partial param annotations --- *)

  test "effect annot: some params annotated, some not" (fun () ->
    expect_int {|
      effect IO =
        emit : int -> unit
      end
      let f x (y: int) : int / IO =
        perform emit (x + y);
        x + y
      ;;
      let mut v = 0 in
      handle f 20 22 with
      | return x -> x + v
      | emit n k -> v := n; resume k ()
    |} 84);

  (* --- Parameterized effect: State used in handler with explicit annotation --- *)

  test "effect annot: stateful counter with explicit State int" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let rec count_down (n: int) : int / State int =
        if n <= 0 do perform get ()
        else do
          let cur = perform get () in
          perform put (cur + 1);
          count_down (n - 1)
        end
      ;;
      let mut st = 0 in
      handle count_down 10 with
      | return x -> x
      | get () k -> resume k st
      | put v k -> st := v; resume k ()
    |} 10);

  (* --- Effect-polymorphic class method stress tests --- *)

  test "effect annot: class method with effect var, pure instance" (fun () ->
    expect_int {|
      class Transform 'a =
        xform : 'a -> 'a / 'e
      end
      instance Transform int =
        let xform x = x + 1
      end
      xform 41
    |} 42);

  test "effect annot: class method with effect var, effectful instance" (fun () ->
    expect_int {|
      effect Log =
        log : string -> unit
      end
      class Process 'a =
        process : 'a -> 'a / 'e
      end
      instance Process int =
        let process x = perform log "processing"; x * 2
      end
      let mut n = 0 in
      handle
        process 21
      with
      | return x -> x + n
      | log _ k -> n := n + 1; resume k ()
    |} 43);

  (* --- Effect annotation on type expression used as param type --- *)

  test "effect annot: explicit arrow with effect in param position" (fun () ->
    expect_int {|
      effect E =
        op : unit -> int
      end
      let run_with_handler (f : unit -> int / E) : int =
        handle f () with
        | return x -> x
        | op () k -> resume k 42
      ;;
      run_with_handler (fn () -> perform op ())
    |} 42);

  (* --- Parameterized effect error: type mismatch in state --- *)

  test "error: parameterized effect type mismatch" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f () : unit / State int =
        perform put "hello"
    |});

  test "error: two performs disagree on effect type parameter" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f () =
        perform put "hello";
        perform put 42
    |});

  test "error: get and put disagree on effect type parameter" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f () =
        perform put 42;
        let x = perform get () in
        x ^ " world"
    |});

  test "error: param effect mismatch via annotation on get" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f () : string / State int =
        perform get ()
    |});

  test "error: param effect handler resume type mismatch" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      handle
        perform put "hello"
      with
      | return x -> x
      | put v k -> resume k ()
      | get () k -> resume k 42
    |});

  test "error: param effect handler body uses both ops with mismatch" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      handle
        perform put "hello";
        let x = perform get () in
        x + 1
      with
      | return x -> x
      | put v k -> resume k ()
      | get () k -> resume k "world"
    |});

  test "two performs agree on effect type parameter" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = 0 in
      handle
        perform put 10;
        perform put 20;
        perform get ()
      with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 20);

  test "get and put share type parameter correctly" (fun () ->
    expect_string {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = "initial" in
      handle
        perform put "updated";
        perform get ()
      with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} "updated");

  test "param effect annotation matches operations" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f () : int / State int =
        perform put 10;
        perform get ()
      ;;
      let mut st = 0 in
      handle f () with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 10);

  test "param effect inferred from single perform" (fun () ->
    expect_string {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = "hi" in
      handle
        perform put "bye";
        perform get ()
      with
      | return x -> x ^ "!"
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} "bye!");

  test "param effect with tuple type parameter" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = (0, "none") in
      handle
        perform put (42, "answer");
        let (n, s) = perform get () in
        n
      with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 42);

  test "param effect with list type parameter" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = [] in
      handle
        perform put [1; 2; 3];
        let xs = perform get () in
        match xs with
        | x :: _ -> x
        | [] -> 0
      with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 1);

  test "param effect nested handlers different types" (fun () ->
    expect_int {|
      effect Counter =
        inc : unit -> unit
        count : unit -> int
      end
      effect Logger =
        log : string -> unit
      end
      let mut c = 0 in
      let mut logs = "" in
      handle
        handle
          let _ = perform inc () in
          let _ = perform inc () in
          let _ = perform inc () in
          perform log "done";
          perform count ()
        with
        | return x -> x
        | inc () k -> c := c + 1; resume k ()
        | count () k -> resume k c
      with
      | return x -> x
      | log msg k -> logs := logs ^ msg; resume k ()
    |} 3);

  test "error: param effect two puts disagree inside handler" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      handle
        perform put 42;
        perform put "hello"
      with
      | return x -> x
      | put v k -> resume k ()
      | get () k -> resume k 0
    |});

  test "param effect with explicit annotation on multi-arg function" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f (x : int) (y : int) : int / State int =
        perform put (x + y);
        perform get ()
      ;;
      let mut st = 0 in
      handle f 10 20 with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 30);

  test "param effect recursive function with annotation" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let rec countdown (n : int) : int / State int =
        if n <= 0 do
          perform get ()
        else
          let cur = perform get () in
          perform put (cur + n);
          countdown (n - 1)
      ;;
      let mut st = 0 in
      handle countdown 5 with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 15);

  test "two different param effects in same function" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      effect Reader 'a =
        ask : unit -> 'a
      end
      let f () : int / State int, Reader int =
        let n = perform ask () in
        let _ = perform put (n * 2) in
        perform get ()
      ;;
      let mut st = 0 in
      handle
        handle f () with
        | return x -> x
        | get () k -> resume k st
        | put v k -> st := v; resume k ()
      with
      | return x -> x
      | ask () k -> resume k 21
    |} 42);

  test "error: two different param effects confused type params" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      effect Reader 'a =
        ask : unit -> 'a
      end
      let f () =
        perform put "hello";
        let x : int = perform ask () in
        perform put x
    |});

  test "param effect with record type parameter" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = { x = 0; y = 0 } in
      handle
        perform put { x = 10; y = 20 };
        let p = perform get () in
        p.x + p.y
      with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 30);

  test "param effect operations in different branches" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f (b : bool) : int / State int =
        if b do
          let _ = perform put 1 in
          perform get ()
        else
          let _ = perform put 2 in
          perform get ()
      ;;
      let mut st = 0 in
      handle f true with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 1);

  test "param effect in lambda body" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = 0 in
      let f = fn x ->
        perform put x;
        perform get ()
      in
      handle f 99 with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 99);

  test "param effect with option type parameter" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let mut st = None in
      handle
        perform put (Some 42);
        match perform get () with
        | Some x -> x
        | None -> 0
      with
      | return x -> x
      | put v k -> st := v; resume k ()
      | get () k -> resume k st
    |} 42);

  test "error: param effect annotation wrong type for get return" (fun () ->
    expect_type_error {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f () : int / State string =
        let x = perform get () in
        x + 1
    |});

  test "param effect with multi-param effect" (fun () ->
    expect_int {|
      effect Map 'k 'v =
        lookup : 'k -> 'v
        store : ('k * 'v) -> unit
      end
      let mut tbl = [] in
      handle
        perform store ("x", 42);
        perform store ("y", 99);
        perform lookup "y"
      with
      | return x -> x
      | store p k2 ->
        let (k, v) = p in
        tbl := (k, v) :: tbl; resume k2 ()
      | lookup key k2 ->
        let result = fold (fn acc entry ->
          let (k, v) = entry in
          if k = key do Some v else acc
        ) None tbl in
        match result with
        | Some v -> resume k2 v
        | None -> resume k2 0
    |} 99);

  test "error: multi-param effect type mismatch on first param" (fun () ->
    expect_type_error {|
      effect Map 'k 'v =
        lookup : 'k -> 'v
        store : ('k * 'v) -> unit
      end
      let f () =
        perform store ("x", 42);
        perform store (1, 99)
    |});

  test "error: multi-param effect type mismatch on second param" (fun () ->
    expect_type_error {|
      effect Map 'k 'v =
        lookup : 'k -> 'v
        store : ('k * 'v) -> unit
      end
      let f () =
        perform store ("x", 42);
        perform store ("y", "hello")
    |});

  test "param effect annotation preserves constraint across sequence" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      let f () : unit / State int =
        perform put 1;
        perform put 2;
        perform put 3
      ;;
      let mut st = 0 in
      handle f () ; st with
      | return x -> x
      | put v k -> st := st + v; resume k ()
    |} 6);

  (* --- Multiple effects with param effects --- *)

  test "effect annot: mixed param and non-param effects" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      effect IO =
        emit : int -> unit
      end
      let f () : int / State int, IO =
        let x = perform get () in
        perform emit x;
        perform put (x + 1);
        perform get ()
      ;;
      let mut st = 10 in
      let mut emitted = 0 in
      handle f () with
      | return x -> x + emitted
      | get () k -> resume k st
      | put v k -> st := v; resume k ()
      | emit v k -> emitted := v; resume k ()
    |} 21);

  (* ======= Locally Abstract Types / Polymorphic Recursion ======= *)
  Printf.printf "\n=== Locally Abstract Types Tests ===\n";

  (* Basic polymorphic recursion with GADT *)
  test "LAT: basic GADT eval with polymorphic recursion" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval a + eval b
        | If (cond, then_, else_) ->
          if eval cond do eval then_ else eval else_
      ;;
      eval (If (BoolLit true, Add (IntLit 10, IntLit 20), IntLit 0))
    |} 30);

  test "LAT: GADT eval returns bool" (fun () ->
    expect_bool {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval a + eval b
        | If (cond, then_, else_) ->
          if eval cond do eval then_ else eval else_
      ;;
      eval (BoolLit false)
    |} false);

  (* Expression-level let rec with type params *)
  test "LAT: expression-level let rec" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let result =
        let rec (type 'a) eval (e : 'a expr) : 'a =
          match e with
          | IntLit n -> n
          | BoolLit b -> b
          | If (cond, t, f) -> if eval cond do eval t else eval f
        in
        eval (If (BoolLit true, IntLit 42, IntLit 0))
      ;;
      result
    |} 42);

  (* Multiple type params *)
  test "LAT: multiple type params" (fun () ->
    expect_int {|
      type ('a, 'b) either =
        | Left : 'a -> ('a, 'b) either
        | Right : 'b -> ('a, 'b) either
      type 'a list_expr =
        | Nil : 'a list_expr
        | Cons : 'a * 'a list_expr -> 'a list_expr
      let rec (type 'a) length (xs : 'a list_expr) : int =
        match xs with
        | Nil -> 0
        | Cons (_, rest) -> 1 + length rest
      ;;
      length (Cons (1, Cons (2, Cons (3, Nil))))
    |} 3);

  (* Polymorphic recursion: recursive call at different type instantiation *)
  test "LAT: recursive call at different type" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Neg : int expr -> int expr
        | Not : bool expr -> bool expr
      let rec (type 'a) size (e : 'a expr) : int =
        match e with
        | IntLit _ -> 1
        | BoolLit _ -> 1
        | Neg inner -> 1 + size inner
        | Not inner -> 1 + size inner
      ;;
      size (Neg (Neg (IntLit 0))) + size (Not (BoolLit true))
    |} 5);

  (* GADT format string — the motivating use case *)
  test "LAT: GADT format string" (fun () ->
    expect_string {|
      type ('a, 'b) fmt =
        | Lit : string * ('a, 'b) fmt -> (string, 'b) fmt
        | IntF : ('a, 'b) fmt -> (string, int -> 'b) fmt
        | StrF : ('a, 'b) fmt -> (string, string -> 'b) fmt
        | End : (string, string) fmt
      let rec (type 'a 'b) fmt_to_str (fmt : ('a, 'b) fmt) : string =
        match fmt with
        | Lit (s, rest) -> s ^ fmt_to_str rest
        | IntF rest -> "<int>" ^ fmt_to_str rest
        | StrF rest -> "<str>" ^ fmt_to_str rest
        | End -> ""
      ;;
      fmt_to_str (Lit ("hello ", IntF (Lit (" world ", StrF End))))
    |} "hello <int> world <str>");

  (* Declaration-level — regular let rec with type params *)
  test "LAT: declaration-level let rec" (fun () ->
    expect_int {|
      type 'a tree =
        | Leaf : int -> int tree
        | Node : 'a tree * 'a tree -> 'a tree
      let rec (type 'a) size (t : 'a tree) : int =
        match t with
        | Leaf _ -> 1
        | Node (l, r) -> size l + size r
      ;;
      size (Node (Leaf 1, Node (Leaf 2, Leaf 3)))
    |} 3);

  (* Without type params, regular let rec still works *)
  test "LAT: regular let rec unchanged" (fun () ->
    expect_int {|
      let rec fac n = if n <= 1 do 1 else n * fac (n - 1)
      ;;
      fac 5
    |} 120);

  (* Type params with effect annotation *)
  test "LAT: polymorphic recursion with effect" (fun () ->
    expect_int {|
      effect Log =
        log : string -> unit
      end
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
      let rec (type 'a) eval (e : 'a expr) : 'a / Log =
        match e with
        | IntLit n ->
          perform log "int";
          n
        | BoolLit b ->
          perform log "bool";
          b
        | Add (a, b) ->
          perform log "add";
          eval a + eval b
      ;;
      let mut count = 0 in
      let result = handle eval (Add (IntLit 3, IntLit 4)) with
        | return x -> x
        | log _ k -> count := count + 1; resume k ()
      in
      result + count
    |} 10);

  (* Error: type variable in type params not used should still work *)
  test "LAT: unused type param still compiles" (fun () ->
    expect_int {|
      let rec (type 'a) f (x : int) : int = if x <= 0 do 0 else f (x - 1)
      ;;
      f 5
    |} 0);

  (* ======= LAT Stress Tests ======= *)
  Printf.printf "\n=== LAT Stress Tests ===\n";

  (* Full GADT evaluator: int, bool, string return types, nested If *)
  test "LAT stress: full GADT evaluator with three return types" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | StrLit : string -> string expr
        | Add : int expr * int expr -> int expr
        | Eq : int expr * int expr -> bool expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
        | Concat : string expr * string expr -> string expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | StrLit s -> s
        | Add (a, b) -> eval a + eval b
        | Eq (a, b) -> eval a = eval b
        | If (cond, t, f) -> if eval cond do eval t else eval f
        | Concat (a, b) -> eval a ^ eval b
      ;;
      -- nested: if (3+4 = 7) then 100 else 0
      eval (If (Eq (Add (IntLit 3, IntLit 4), IntLit 7),
                IntLit 100,
                IntLit 0))
    |} 100);

  (* Deep nesting: if (if true then (1=1) else false) then 42 else 0 *)
  test "LAT stress: deeply nested GADT if-expressions" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | Eq : int expr * int expr -> bool expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
        | Not : bool expr -> bool expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval a + eval b
        | Eq (a, b) -> eval a = eval b
        | Not x -> not (eval x)
        | If (cond, t, f) -> if eval cond do eval t else eval f
      ;;
      -- if (not (if true then false else true)) then (1+2+3) else 0
      eval (If (Not (If (BoolLit true, BoolLit false, BoolLit true)),
                Add (IntLit 1, Add (IntLit 2, IntLit 3)),
                IntLit 0))
    |} 6);

  (* Polymorphic recursion called at multiple types in one body *)
  test "LAT stress: multiple type instantiations in same body" (fun () ->
    expect_string {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | StrLit : string -> string expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) to_s (e : 'a expr) : string =
        match e with
        | IntLit n -> string_of_int n
        | BoolLit b -> if b do "true" else "false"
        | StrLit s -> s
        | If (cond, t, f) ->
          "if(" ^ to_s cond ^ "," ^ to_s t ^ "," ^ to_s f ^ ")"
      ;;
      to_s (If (BoolLit true, IntLit 42, IntLit 0))
    |} "if(true,42,0)");

  (* Expression-level poly recursion used polymorphically after binding *)
  test "LAT stress: expression-level used at different types after binding" (fun () ->
    expect_string {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let result =
        let rec (type 'a) eval (e : 'a expr) : 'a =
          match e with
          | IntLit n -> n
          | BoolLit b -> b
          | Add (a, b) -> eval a + eval b
          | If (cond, t, f) -> if eval cond do eval t else eval f
        in
        -- use at int, bool, and int again
        let n = eval (Add (IntLit 10, IntLit 20)) in
        let b = eval (BoolLit true) in
        let n2 = eval (If (BoolLit false, IntLit 1, IntLit 2)) in
        string_of_int n ^ " " ^ (if b do "T" else "F") ^ " " ^ string_of_int n2
      ;;
      result
    |} "30 T 2");

  (* Poly recursion inside a module *)
  test "LAT stress: polymorphic recursion inside module" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      module Eval =
        pub let rec (type 'a) eval (e : 'a expr) : 'a =
          match e with
          | IntLit n -> n
          | BoolLit b -> b
          | Add (a, b) -> eval a + eval b
          | If (cond, t, f) -> if eval cond do eval t else eval f
      end
      ;;
      Eval.eval (If (BoolLit true, Add (IntLit 100, IntLit 23), IntLit 0))
    |} 123);

  (* GADT with existential + poly recursion *)
  test "LAT stress: existential GADT with poly recursion" (fun () ->
    expect_string {|
      type any_show =
        | AnyShow : 'a * ('a -> string) -> any_show
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Wrap : 'a expr * ('a -> string) -> any_show expr
      let rec (type 'a) eval_show (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Wrap (inner, f) -> AnyShow (eval_show inner, f)
      ;;
      let result = eval_show (Wrap (IntLit 42, string_of_int)) in
      match result with
      | AnyShow (v, f) -> f v
    |} "42");

  (* Two GADT type params: polymorphic recursion on a format type *)
  test "LAT stress: format string apply" (fun () ->
    expect_int {|
      type ('r, 'a) fmt =
        | FEnd : (string, string) fmt
        | FLit : string * ('r, 'a) fmt -> (string, 'a) fmt
        | FInt : ('r, 'a) fmt -> (string, int -> 'a) fmt
        | FStr : ('r, 'a) fmt -> (string, string -> 'a) fmt
      let rec (type 'r 'a) count_holes (f : ('r, 'a) fmt) : int =
        match f with
        | FEnd -> 0
        | FLit (_, rest) -> count_holes rest
        | FInt rest -> 1 + count_holes rest
        | FStr rest -> 1 + count_holes rest
      ;;
      count_holes (FLit ("x=", FInt (FLit (", s=", FStr FEnd))))
    |} 2);

  (* Poly recursion with effects: stateful GADT interpreter *)
  test "LAT stress: stateful GADT eval with effects" (fun () ->
    expect_int {|
      effect State 'a =
        get : unit -> 'a
        put : 'a -> unit
      end
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
        | GetState : int expr
        | SetState : int expr -> int expr
      let rec (type 'a) eval (e : 'a expr) : 'a / State int =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval a + eval b
        | If (cond, t, f) -> if eval cond do eval t else eval f
        | GetState -> perform get ()
        | SetState e ->
          let v = eval e in
          perform put v;
          v
      ;;
      let mut st = 10 in
      let result = handle
        -- set state to 3+4=7, then get state and add 100
        eval (Add (SetState (Add (IntLit 3, IntLit 4)), GetState))
      with
        | return x -> x
        | get () k -> resume k st
        | put v k -> st := v; resume k ()
      in
      result  -- 7 + 7 = 14
    |} 14);

  (* Poly recursion: multiple type refinements in one function *)
  test "LAT stress: each branch returns different refined type" (fun () ->
    expect_string {|
      type 'a val =
        | VInt : int -> int val
        | VBool : bool -> bool val
        | VStr : string -> string val
        | VNeg : int val -> int val
        | VNot : bool val -> bool val
        | VConcat : string val * string val -> string val
      let rec (type 'a) show_val (v : 'a val) : string =
        match v with
        | VInt n -> string_of_int n
        | VBool b -> if b do "true" else "false"
        | VStr s -> s
        | VNeg x -> $"neg({show_val x})"
        | VNot x -> $"not({show_val x})"
        | VConcat (a, b) -> $"concat({show_val a},{show_val b})"
      ;;
      show_val (VConcat (VStr "hello", VStr "world")) ^ " " ^
      show_val (VNeg (VInt 5)) ^ " " ^
      show_val (VNot (VBool true))
    |} "concat(hello,world) neg(5) not(true)");

  (* Poly recursion: function returns a function type via GADT *)
  test "LAT stress: GADT with function-typed constructor" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Lam : ('a expr -> 'b expr) -> ('a -> 'b) expr
        | App : ('a -> 'b) expr * 'a expr -> 'b expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Lam f -> fn x -> eval (f (IntLit x))
        | App (f, a) -> (eval f) (eval a)
        | If (cond, t, f) -> if eval cond do eval t else eval f
      ;;
      -- (\x -> x + 1) applied to 41
      let inc = Lam (fn (x : int expr) -> App (Lam (fn (y : int expr) ->
        IntLit ((fn (IntLit n) -> n) y + 1)), x)) in
      eval (App (inc, IntLit 41))
    |} 42);

  (* Poly recursion with multiple recursive calls at different types *)
  test "LAT stress: alternating int/bool recursive descent" (fun () ->
    expect_int {|
      type 'a tree =
        | ILeaf : int -> int tree
        | BLeaf : bool -> bool tree
        | INode : bool tree * int tree * int tree -> int tree
        | BNode : int tree * bool tree * bool tree -> bool tree
      let rec (type 'a) depth (t : 'a tree) : int =
        match t with
        | ILeaf _ -> 0
        | BLeaf _ -> 0
        | INode (cond, l, r) ->
          let d1 = depth cond in
          let d2 = depth l in
          let d3 = depth r in
          1 + (if d1 > d2 do (if d1 > d3 do d1 else d3)
               else (if d2 > d3 do d2 else d3))
        | BNode (sel, l, r) ->
          let d1 = depth sel in
          let d2 = depth l in
          let d3 = depth r in
          1 + (if d1 > d2 do (if d1 > d3 do d1 else d3)
               else (if d2 > d3 do d2 else d3))
      ;;
      -- depth 2: INode -> BNode/INode -> leaves
      depth (INode (BNode (ILeaf 0, BLeaf true, BLeaf false),
                    INode (BLeaf true, ILeaf 1, ILeaf 2),
                    ILeaf 3))
    |} 2);

  (* GADT + poly recursion computing actual values across type boundaries *)
  test "LAT stress: cross-type computation bool->int->string" (fun () ->
    expect_string {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | StrLit : string -> string expr
        | Add : int expr * int expr -> int expr
        | Eq : int expr * int expr -> bool expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
        | Concat : string expr * string expr -> string expr
        | IntToStr : int expr -> string expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | StrLit s -> s
        | Add (a, b) -> eval a + eval b
        | Eq (a, b) -> eval a = eval b
        | If (cond, t, f) -> if eval cond do eval t else eval f
        | Concat (a, b) -> eval a ^ eval b
        | IntToStr e -> string_of_int (eval e)
      ;;
      -- if (2+3 = 5) then "result=" ^ int_to_str(10+20) else "fail"
      eval (If (Eq (Add (IntLit 2, IntLit 3), IntLit 5),
                Concat (StrLit "result=", IntToStr (Add (IntLit 10, IntLit 20))),
                StrLit "fail"))
    |} "result=30");

  (* Poly recursion: two mutually reinforcing GADT types *)
  test "LAT stress: poly recursion over two related GADTs" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
        | Neg : int expr -> int expr
      -- count all nodes in an expression tree
      let rec (type 'a) count (e : 'a expr) : int =
        match e with
        | IntLit _ -> 1
        | BoolLit _ -> 1
        | If (c, t, f) -> 1 + count c + count t + count f
        | Neg x -> 1 + count x
      ;;
      -- also compute the result
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | If (c, t, f) -> if eval c do eval t else eval f
        | Neg x -> 0 - eval x
      ;;
      let expr = If (BoolLit true, Neg (IntLit 5), IntLit 10) in
      count expr + eval expr
    |} 0);

  (* Poly recursion with pipe operator *)
  test "LAT stress: poly recursion composed with pipe" (fun () ->
    expect_string {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval a + eval b
        | If (c, t, f) -> if eval c do eval t else eval f
      ;;
      let result =
        Add (IntLit 10, IntLit 20) |> eval |> string_of_int
      in
      let b = BoolLit true |> eval in
      result ^ " " ^ (if b do "yes" else "no")
    |} "30 yes");

  (* Poly recursion result stored in mutable, iterated *)
  test "LAT stress: poly recursion in loop with mutable accumulation" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | Add : int expr * int expr -> int expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | Add (a, b) -> eval a + eval b
      ;;
      let exprs = [
        IntLit 1;
        Add (IntLit 2, IntLit 3);
        Add (IntLit 10, Add (IntLit 20, IntLit 30))
      ] in
      for e in exprs with sum = 0 do
        sum + eval e
      end
    |} 66);

  (* Poly recursion with closure capture *)
  test "LAT stress: poly recursive function captured in closure" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval a + eval b
        | If (c, t, f) -> if eval c do eval t else eval f
      ;;
      -- capture eval in closures that use it at different types
      let eval_int = fn (e : int expr) -> eval e in
      let eval_bool = fn (e : bool expr) -> eval e in
      let n = eval_int (Add (IntLit 10, IntLit 20)) in
      let b = eval_bool (BoolLit true) in
      if b do n + 1 else n
    |} 31);

  (* Poly recursion: recursive call through helper function *)
  test "LAT stress: poly recursion with local helper" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | Mul : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval_int a + eval_int b
        | Mul (a, b) -> eval_int a * eval_int b
        | If (c, t, f) -> if eval c do eval t else eval f
      and eval_int (e : int expr) : int = eval e
      ;;
      eval (If (BoolLit true,
                Mul (Add (IntLit 2, IntLit 3), IntLit 4),
                IntLit 0))
    |} 20);

  (* GADT with string interpolation inside poly recursion *)
  test "LAT stress: string interpolation in poly recursive eval" (fun () ->
    expect_string {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) pretty (e : 'a expr) : string =
        match e with
        | IntLit n -> $"{n}"
        | BoolLit b -> if b do "true" else "false"
        | Add (a, b) -> $"({pretty a} + {pretty b})"
        | If (c, t, f) -> $"if {pretty c} then {pretty t} else {pretty f}"
      ;;
      pretty (If (BoolLit true, Add (IntLit 1, IntLit 2), IntLit 0))
    |} "if true then (1 + 2) else 0");

  (* Poly recursion: tail-recursive style with accumulator *)
  test "LAT stress: poly recursion with accumulator pattern" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) count_nodes (e : 'a expr) : int =
        match e with
        | IntLit _ -> 1
        | BoolLit _ -> 1
        | Add (a, b) -> 1 + count_nodes a + count_nodes b
        | If (c, t, f) -> 1 + count_nodes c + count_nodes t + count_nodes f
      ;;
      -- big expression: if (if true then false else true) then (1+2+3) else (4+(5+6))
      let expr = If (If (BoolLit true, BoolLit false, BoolLit true),
                     Add (IntLit 1, Add (IntLit 2, IntLit 3)),
                     Add (IntLit 4, Add (IntLit 5, IntLit 6))) in
      count_nodes expr
    |} 15);

  (* Poly recursion: two separate poly rec functions interacting *)
  test "LAT stress: two independent poly rec functions" (fun () ->
    expect_string {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
        | If : bool expr * 'a expr * 'a expr -> 'a expr
      let rec (type 'a) eval (e : 'a expr) : 'a =
        match e with
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval a + eval b
        | If (c, t, f) -> if eval c do eval t else eval f
      ;;
      let rec (type 'a) depth (e : 'a expr) : int =
        match e with
        | IntLit _ -> 0
        | BoolLit _ -> 0
        | Add (a, b) ->
          let da = depth a in
          let db = depth b in
          1 + (if da > db do da else db)
        | If (c, t, f) ->
          let dc = depth c in
          let dt = depth t in
          let df = depth f in
          let m1 = if dc > dt do dc else dt in
          1 + (if m1 > df do m1 else df)
      ;;
      let e = If (BoolLit true, Add (IntLit 1, IntLit 2), IntLit 0) in
      $"eval={eval e} depth={depth e}"
    |} "eval=3 depth=2");

  (* GADT with no-arg constructors across types *)
  test "LAT stress: GADT with zero-arg constructors at different types" (fun () ->
    expect_int {|
      type 'a val =
        | VTrue : bool val
        | VFalse : bool val
        | VZero : int val
        | VOne : int val
        | VUnit : unit val
        | VNot : bool val -> bool val
        | VSucc : int val -> int val
      let rec (type 'a) to_int (v : 'a val) : int =
        match v with
        | VTrue -> 1
        | VFalse -> 0
        | VZero -> 0
        | VOne -> 1
        | VUnit -> 0
        | VNot x -> 1 - to_int x
        | VSucc x -> 1 + to_int x
      ;;
      to_int (VSucc (VSucc (VSucc VZero))) +
      to_int (VNot VFalse) +
      to_int VUnit
    |} 4);

  (* Poly recursion with match guard *)
  test "LAT stress: poly recursion with match guards" (fun () ->
    expect_int {|
      type 'a expr =
        | IntLit : int -> int expr
        | BoolLit : bool -> bool expr
        | Add : int expr * int expr -> int expr
      let rec (type 'a) eval_clamp (e : 'a expr) : 'a =
        match e with
        | IntLit n when n > 100 -> 100
        | IntLit n when n < 0 -> 0
        | IntLit n -> n
        | BoolLit b -> b
        | Add (a, b) -> eval_clamp (IntLit (eval_clamp a + eval_clamp b))
      ;;
      eval_clamp (Add (IntLit 80, IntLit 50))
    |} 100);

  print_summary ()
