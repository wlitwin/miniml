open Test_helpers

let () =
  Printf.printf "=== List Tests ===\n";

  test "empty list" (fun () ->
    let result = Interpreter.Interp.run_string "([] : int list)" in
    match result with
    | Interpreter.Bytecode.VList [] -> ()
    | _ -> failwith "unexpected value");
  test "list literal" (fun () ->
    let result = Interpreter.Interp.run_string "[1; 2; 3]" in
    match result with
    | Interpreter.Bytecode.VList [VInt 1; VInt 2; VInt 3] -> ()
    | _ -> failwith "unexpected list value");
  test "cons operator" (fun () ->
    let result = Interpreter.Interp.run_string "0 :: [1; 2]" in
    match result with
    | Interpreter.Bytecode.VList [VInt 0; VInt 1; VInt 2] -> ()
    | _ -> failwith "unexpected list value");
  test "list sum" (fun () ->
    expect_int {|
      let rec sum (xs: int list) : int =
        match xs with
        | [] -> 0
        | x :: rest -> x + sum rest
      in sum [1; 2; 3; 4; 5]
    |} 15);
  test "list map" (fun () ->
    let result = Interpreter.Interp.run_string {|
      let rec map (f: int -> int) (xs: int list) : int list =
        match xs with
        | [] -> ([] : int list)
        | x :: rest -> f x :: map f rest
      in map (fn (x: int) -> x * 2) [1; 2; 3]
    |} in
    match result with
    | Interpreter.Bytecode.VList [VInt 2; VInt 4; VInt 6] -> ()
    | _ -> failwith "unexpected list value");

  Printf.printf "\n=== Record Tests ===\n";

  test "record creation" (fun () ->
    expect_int "let r = { x = 1; y = 2 } in r.x + r.y" 3);
  test "row polymorphism width" (fun () ->
    expect_int {|
      let get_x r = r.x in
      let p = { x = 10; y = 20 } in
      get_x p
    |} 10);

  test "closed record annotation rejects extra fields" (fun () ->
    expect_type_error {|
      let get_x (r: { x: int }) : int = r.x in
      let p = { x = 10; y = 20 } in
      get_x p
    |});

  test "open record annotation accepts extra fields" (fun () ->
    expect_int {|
      let get_x (r: { x: int; .. }) = r.x in
      get_x { x = 42; y = "hello" }
    |} 42);

  test "open record annotation multiple fields" (fun () ->
    expect_int {|
      let sum (r : { x: int; y: int; .. }) = r.x + r.y in
      sum { x = 10; y = 20; z = "extra" }
    |} 30);

  test "open record annotation rejects missing field" (fun () ->
    expect_type_error {|
      let get_x (r: { x: int; .. }) = r.x in
      get_x { y = 42 }
    |});

  Printf.printf "\n=== Record Update Tests ===\n";

  test "record update one field" (fun () ->
    expect_int {|
      let r = { x = 1; y = 2 } in
      let r2 = { r with x = 10 } in
      r2.x + r2.y
    |} 12);
  test "record update multiple fields" (fun () ->
    expect_int {|
      let r = { a = 1; b = 2; c = 3 } in
      let r2 = { r with a = 10; c = 30 } in
      r2.a + r2.b + r2.c
    |} 42);
  test "record update preserves original" (fun () ->
    expect_int {|
      let r = { x = 1; y = 2 } in
      let r2 = { r with x = 10 } in
      r.x + r2.x
    |} 11);
  test "record update expression base" (fun () ->
    expect_int {|
      let f x = { x = x; y = 2 } in
      let r = { f 1 with x = 10 } in
      r.x + r.y
    |} 12);
  test "record update all fields" (fun () ->
    expect_int {|
      let r = { x = 1; y = 2 } in
      let r2 = { r with x = 10; y = 20 } in
      r2.x + r2.y
    |} 30);
  test "record update nested" (fun () ->
    expect_int {|
      let r = { x = 1; y = 2; z = 3 } in
      let r2 = { { r with x = 10 } with y = 20 } in
      r2.x + r2.y + r2.z
    |} 33);
  test "record update unknown field error" (fun () ->
    expect_type_error {|
      let r = { x = 1; y = 2 } in
      { r with z = 3 }
    |});
  test "record update wrong type error" (fun () ->
    expect_type_error {|
      let r = { x = 1; y = 2 } in
      { r with x = true }
    |});
  test "record update non-record error" (fun () ->
    expect_type_error {|
      { 42 with x = 1 }
    |});

  Printf.printf "\n=== Row-Polymorphic Record Update Tests ===\n";

  test "row-polymorphic record update" (fun () ->
    expect_int {|
      let set_y r = { r with y = 10 } in
      let r1 = { x = 1; y = 2 } in
      let r2 = { x = 1; y = 2; z = 3 } in
      (set_y r1).y + (set_y r2).y
    |} 20);

  test "polymorphic update preserves unknown fields" (fun () ->
    expect_int {|
      let set_y r = { r with y = 10 } in
      let r = { x = 1; y = 2; z = 3 } in
      let r2 = set_y r in
      r2.x + r2.y + r2.z
    |} 14);

  test "polymorphic update with annotation" (fun () ->
    expect_int {|
      let set_y (r: { y: int; .. }) = { r with y = 10 } in
      (set_y { x = 1; y = 2 }).y
    |} 10);

  test "polymorphic update multiple fields" (fun () ->
    expect_int {|
      let reset r = { r with x = 0; y = 0 } in
      let r = { x = 5; y = 10; z = 15 } in
      let r2 = reset r in
      r2.x + r2.y + r2.z
    |} 15);

  test "polymorphic update in pipeline" (fun () ->
    expect_int {|
      let inc_x r = { r with x = r.x + 1 } in
      let r = { x = 0; y = 100 } in
      (inc_x (inc_x (inc_x r))).x
    |} 3);

  Printf.printf "\n=== Row-Polymorphic Update Cross-Feature Tests ===\n";

  (* --- Effects + polymorphic record updates --- *)

  test "poly update inside effect handler" (fun () ->
    expect_int {|
      effect Val =
        get_val : unit -> int
      end
      let set_y r = { r with y = 10 } in
      handle
        let v = perform get_val () in
        let r = { x = v; y = 0; z = 99 } in
        (set_y r).y + r.z
      with
      | return x -> x
      | get_val () k -> resume k 5
    |} 109);

  test "poly update function called across effect boundary" (fun () ->
    expect_int {|
      effect Transform =
        transform : { x: int; y: int; z: int } -> { x: int; y: int; z: int }
      end
      let set_y r = { r with y = 100 } in
      handle
        let r = { x = 1; y = 2; z = 3 } in
        let r2 = perform transform r in
        r2.x + r2.y + r2.z
      with
      | return x -> x
      | transform r k -> resume k (set_y r)
    |} 104);

  test "poly update with state effect" (fun () ->
    expect_int {|
      effect State =
        get : unit -> int
        put : int -> unit
      end
      let inc_field r = { r with counter = r.counter + 1 } in
      let mut state = 0 in
      handle
        let r = { counter = 0; label = 42 } in
        let r = inc_field r in
        let r = inc_field r in
        let r = inc_field r in
        perform put (r.counter);
        let s = perform get () in
        s + r.label
      with
      | return x -> x
      | get () k -> resume k state
      | put v k -> state := v; resume k ()
    |} 45);

  test "poly update resumed multiple times" (fun () ->
    expect_int {|
      effect Choose =
        choose : unit -> bool
      end
      let set_x r v = { r with x = v } in
      handle
        let r = { x = 0; y = 10 } in
        let b = perform choose () in
        let r2 = if b do set_x r 100 else set_x r 200 in
        r2.x + r2.y
      with
      | return x -> x
      | choose () k ->
        let k2 = copy_continuation k in
        let a = resume k true in
        let b = resume k2 false in
        a + b
    |} 320);

  (* --- Typeclasses + polymorphic record updates --- *)

  test "poly update with show constraint" (fun () ->
    expect_string {|
      let describe r v = { r with label = show v } in
      let r = { label = ""; extra = true } in
      (describe r 42).label
    |} "42");

  test "poly update with Num constraint" (fun () ->
    expect_int {|
      let add_to_field r n = { r with x = r.x + n } in
      let r = { x = 10; y = 20; z = 30 } in
      (add_to_field r 5).x
    |} 15);

  test "poly update used in typeclass instance" (fun () ->
    expect_string {|
      class Updatable 'a =
        update_name : 'a -> string -> 'a
      end
      type person = { name: string; age: int }
      instance Updatable person =
        let update_name p s = { p with name = s }
      end
      let p = { name = "Alice"; age = 30 } in
      (update_name p "Bob").name
    |} "Bob");

  test "poly update with derived Eq on record" (fun () ->
    expect_bool {|
      type point = { x: int; y: int } deriving Eq
      let move_x p dx = { p with x = p.x + dx } in
      let p1 = { x = 1; y = 2 } in
      let p2 = move_x p1 0 in
      p1 = p2
    |} true);

  (* --- Higher-order functions + polymorphic record updates --- *)

  test "poly update as higher-order argument" (fun () ->
    expect_int {|
      let apply_update f r = (f r).x in
      let set_x_to_99 r = { r with x = 99 } in
      apply_update set_x_to_99 { x = 0; y = 1; z = 2 }
    |} 99);

  test "poly update in fold over list of records" (fun () ->
    expect_int {|
      let set_y r v = { r with y = v } in
      let rec map f xs = match xs with
        | [] -> []
        | x :: rest -> f x :: map f rest
      in
      let records = [{ x = 1; y = 0 }; { x = 2; y = 0 }; { x = 3; y = 0 }] in
      let updated = map (fn r -> set_y r 10) records in
      let rec sum_y xs = match xs with
        | [] -> 0
        | r :: rest -> r.y + sum_y rest
      in
      sum_y updated
    |} 30);

  test "poly update composed" (fun () ->
    expect_int {|
      let set_x r v = { r with x = v } in
      let set_y r v = { r with y = v } in
      let r = { x = 0; y = 0; z = 42 } in
      let r = set_x (set_y r 10) 20 in
      r.x + r.y + r.z
    |} 72);

  test "poly update with closure over captured var" (fun () ->
    expect_int {|
      let make_setter v = fn r -> { r with x = v } in
      let set_to_5 = make_setter 5 in
      let set_to_10 = make_setter 10 in
      let r = { x = 0; y = 100 } in
      (set_to_5 r).x + (set_to_10 r).x
    |} 15);

  (* --- Pattern matching + polymorphic record updates --- *)

  test "poly update in match branch" (fun () ->
    expect_int {|
      type action = Inc | Dec | Reset
      let apply_action r (a: action) =
        match a with
        | Inc -> { r with x = r.x + 1 }
        | Dec -> { r with x = r.x - 1 }
        | Reset -> { r with x = 0 }
      in
      let r = { x = 10; y = 20 } in
      let r = apply_action r Inc in
      let r = apply_action r Inc in
      let r = apply_action r Dec in
      r.x + r.y
    |} 31);

  test "poly update with variant payload containing record" (fun () ->
    expect_int {|
      type wrapper = Wrap of { x: int; y: int; z: int }
      let update_wrapped (w: wrapper) =
        match w with
        | Wrap r -> Wrap { r with x = r.x * 2 }
      in
      match update_wrapped (Wrap { x = 5; y = 10; z = 15 }) with
      | Wrap r -> r.x + r.y + r.z
    |} 35);

  (* --- Recursion + polymorphic record updates --- *)

  test "poly update in recursive function" (fun () ->
    expect_int {|
      let rec inc_n_times r n =
        if n = 0 do r
        else inc_n_times { r with x = r.x + 1 } (n - 1)
      in
      let r = { x = 0; y = 42 } in
      (inc_n_times r 10).x + (inc_n_times r 10).y
    |} 52);

  test "poly update in mutual recursion" (fun () ->
    (* inc {x=0} 5 -> dec {x=2} 4 -> inc {x=1} 3 -> dec {x=3} 2 -> inc {x=2} 1 -> dec {x=4} 0 -> {x=4} *)
    expect_int {|
      let rec inc r n =
        if n = 0 do r
        else dec { r with x = r.x + 2 } (n - 1)
      and dec r n =
        if n = 0 do r
        else inc { r with x = r.x - 1 } (n - 1)
      in
      (inc { x = 0; y = 99 } 5).x
    |} 4);

  (* --- Loops + polymorphic record updates --- *)

  test "poly update in fold loop" (fun () ->
    expect_int {|
      let inc_x r = { r with x = r.x + 1 } in
      let r = for i in [1; 2; 3; 4; 5] with acc = { x = 0; y = 100 } do
        inc_x acc
      end in
      r.x + r.y
    |} 105);

  test "poly update in unit loop" (fun () ->
    expect_int {|
      let set_y r v = { r with y = v } in
      let mut r = { x = 1; y = 0; z = 99 } in
      for i in [10; 20; 30] do
        r := set_y r i
      end;
      r.y + r.z
    |} 129);

  (* --- Modules + polymorphic record updates --- *)

  test "poly update in module function" (fun () ->
    expect_int {|
      module Rec =
        pub let set_x r v = { r with x = v }
        pub let set_y r v = { r with y = v }
      end
      let r = { x = 0; y = 0; z = 42 } in
      let r = Rec.set_x r 10 in
      let r = Rec.set_y r 20 in
      r.x + r.y + r.z
    |} 72);

  test "poly update with module open" (fun () ->
    expect_int {|
      module RecOps =
        pub let inc_x r = { r with x = r.x + 1 }
        pub let inc_y r = { r with y = r.y + 1 }
      end
      open RecOps
      let r = { x = 0; y = 0; z = 50 } in
      let r = inc_x (inc_x (inc_y r)) in
      r.x + r.y + r.z
    |} 53);

  (* --- Mutable variables + polymorphic record updates --- *)

  test "poly update on mutable record" (fun () ->
    expect_int {|
      let set_x r v = { r with x = v } in
      let mut r = { x = 0; y = 10 } in
      r := set_x r 5;
      r := set_x r (r.x + 3);
      r.x + r.y
    |} 18);

  (* --- Pipe operator + polymorphic record updates --- *)

  test "poly update with pipe" (fun () ->
    expect_int {|
      let inc_x r = { r with x = r.x + 1 } in
      let double_y r = { r with y = r.y * 2 } in
      let r = { x = 0; y = 1; z = 100 } in
      let r = r |> inc_x |> inc_x |> double_y |> double_y in
      r.x + r.y + r.z
    |} 106);

  (* --- Tuples + polymorphic record updates --- *)

  test "poly update with tuple destructuring" (fun () ->
    expect_int {|
      let update_pair (r1, r2) =
        ({ r1 with x = 10 }, { r2 with y = 20 })
      in
      let (a, b) = update_pair ({ x = 0; y = 1 }, { x = 2; y = 0; z = 3 }) in
      a.x + b.y + b.z
    |} 33);

  (* --- Lists of records + polymorphic record updates --- *)

  test "poly update map over list" (fun () ->
    expect_int {|
      let zero_y r = { r with y = 0 } in
      let rec sum_x xs = match xs with
        | [] -> 0
        | r :: rest -> r.x + sum_x rest
      in
      let rec map f xs = match xs with
        | [] -> []
        | x :: rest -> f x :: map f rest
      in
      let records = [{ x = 1; y = 10 }; { x = 2; y = 20 }; { x = 3; y = 30 }] in
      let zeroed = map zero_y records in
      sum_x zeroed
    |} 6);

  (* --- Nested records + polymorphic update --- *)

  test "poly update on nested record access" (fun () ->
    expect_int {|
      let set_x r v = { r with x = v } in
      let outer = { inner = { x = 1; y = 2 }; z = 3 } in
      let updated_inner = set_x outer.inner 99 in
      updated_inner.x + updated_inner.y + outer.z
    |} 104);

  (* --- String interpolation + polymorphic record updates --- *)

  test "poly update with string interpolation" (fun () ->
    expect_string {|
      let set_name r n = { r with name = n } in
      let r = { name = "world"; extra = 42 } in
      let r = set_name r "Alice" in
      $"Hello, {r.name}!"
    |} "Hello, Alice!");

  (* --- Return + polymorphic record updates --- *)

  test "poly update with early return" (fun () ->
    expect_int {|
      let set_x r v = { r with x = v } in
      let f r =
        if r.x > 10 do return (set_x r 0) else ();
        set_x r (r.x + 100)
      in
      let r1 = f { x = 20; y = 5 } in
      let r2 = f { x = 3; y = 5 } in
      r1.x + r2.x
    |} 103);

  (* --- Multiple different record shapes through same poly function --- *)

  test "poly update same function many shapes" (fun () ->
    expect_int {|
      let set_x r v = { r with x = v } in
      let a = set_x { x = 0 } 1 in
      let b = set_x { x = 0; y = 2 } 10 in
      let c = set_x { x = 0; y = 2; z = 3 } 100 in
      let d = set_x { x = 0; w = 4; y = 5; z = 6 } 1000 in
      a.x + b.x + c.x + d.x
    |} 1111);

  (* --- Complex: effect + typeclass + poly update --- *)

  test "poly update with effect and show" (fun () ->
    expect_string {|
      effect Logger =
        log : string -> unit
      end
      let update_and_log r =
        let r2 = { r with x = r.x + 1 } in
        perform log (show r2.x);
        r2
      in
      let mut msgs = "" in
      handle
        let r = { x = 0; y = 99 } in
        let r = update_and_log r in
        let _ = update_and_log r in
        msgs
      with
      | return x -> x
      | log msg k ->
        msgs := msgs ^ msg ^ ",";
        resume k ()
    |} "1,2,");

  (* --- Polymorphic update with let rec and closures --- *)

  test "poly update in letrec with closure" (fun () ->
    expect_int {|
      let rec apply_n f r n =
        if n = 0 do r
        else apply_n f (f r) (n - 1)
      in
      let inc_x r = { r with x = r.x + 1 } in
      let inc_y r = { r with y = r.y + 1 } in
      let r = { x = 0; y = 0; z = 42 } in
      let r = apply_n inc_x r 3 in
      let r = apply_n inc_y r 7 in
      r.x + r.y + r.z
    |} 52);

  (* --- Polymorphic update on record with many fields --- *)

  test "poly update on large record" (fun () ->
    expect_int {|
      let set_c r v = { r with c = v } in
      let r = { a = 1; b = 2; c = 3; d = 4; e = 5; f = 6; g = 7; h = 8 } in
      let r = set_c r 100 in
      r.a + r.b + r.c + r.d + r.e + r.f + r.g + r.h
    |} 133);

  (* --- Chain of different poly update functions --- *)

  test "chain different poly updaters" (fun () ->
    expect_int {|
      let set_a r v = { r with a = v } in
      let set_b r v = { r with b = v } in
      let set_c r v = { r with c = v } in
      let r = { a = 0; b = 0; c = 0; extra = 999 } in
      let r = set_a r 10 in
      let r = set_b r 20 in
      let r = set_c r 30 in
      r.a + r.b + r.c + r.extra
    |} 1059);

  Printf.printf "\n=== Advanced Cross-Feature Tests ===\n";

  (* --- Typeclass constraint AND poly update in the same function --- *)

  test "poly update + show in same function" (fun () ->
    expect_string {|
      let label_value r =
        { r with label = show (r.value) }
      in
      let r = { value = 42; label = ""; tag = true } in
      (label_value r).label
    |} "42");

  test "poly update + show with different record shapes" (fun () ->
    expect_string {|
      let label_it r =
        { r with label = show (r.value) }
      in
      let a = label_it { value = 99; label = "" } in
      let b = label_it { value = 7; label = ""; extra = 0 } in
      a.label ^ "," ^ b.label
    |} "99,7");

  test "poly update + Eq constraint" (fun () ->
    expect_bool {|
      let set_if_eq r target replacement =
        if r.x = target do { r with x = replacement }
        else r
      in
      let r = set_if_eq { x = 5; y = 10 } 5 99 in
      r.x = 99
    |} true);

  (* --- Effect handler that returns a polymorphically-updated record --- *)

  test "effect handler returns poly-updated record" (fun () ->
    expect_int {|
      effect Incr =
        incr : int -> int
      end
      let update_x r =
        let new_x = perform incr (r.x) in
        { r with x = new_x }
      in
      handle
        let r = { x = 5; y = 100 } in
        let r = update_x r in
        r.x + r.y
      with
      | return x -> x
      | incr n k -> resume k (n + 10)
    |} 115);

  test "nested effect handlers with poly update" (fun () ->
    expect_int {|
      effect E1 = get1 : unit -> int end
      effect E2 = get2 : unit -> int end
      let set_x r v = { r with x = v } in
      let set_y r v = { r with y = v } in
      handle
        handle
          let r = { x = 0; y = 0; z = 42 } in
          let r = set_x r (perform get1 ()) in
          let r = set_y r (perform get2 ()) in
          r.x + r.y + r.z
        with
        | return x -> x
        | get2 () k -> resume k 20
      with
      | return x -> x
      | get1 () k -> resume k 10
    |} 72);

  (* --- Poly update in continuation-copying scenario --- *)

  test "poly update with multi-shot continuation" (fun () ->
    expect_int {|
      effect Amb = flip : unit -> bool end
      let set_x r v = { r with x = v } in
      handle
        let r = { x = 0; y = 1 } in
        let b = perform flip () in
        let r = if b do set_x r 10 else set_x r 20 in
        r.x + r.y
      with
      | return x -> x
      | flip () k ->
        let k2 = copy_continuation k in
        let a = resume k true in
        let b = resume k2 false in
        a + b
    |} 32);

  (* --- Module exporting poly update + typeclass interaction --- *)

  test "module poly update with typeclass outside" (fun () ->
    expect_string {|
      module Rec =
        pub let set_name r n = { r with name = n }
      end
      let r = { name = "old"; value = 42 } in
      let r = Rec.set_name r "new" in
      r.name ^ ":" ^ show (r.value)
    |} "new:42");

  test "module with multiple poly update functions" (fun () ->
    expect_int {|
      module PointOps =
        pub let move_x r dx = { r with x = r.x + dx }
        pub let move_y r dy = { r with y = r.y + dy }
        pub let move r dx dy =
          let r = { r with x = r.x + dx } in
          { r with y = r.y + dy }
      end
      let p = { x = 0; y = 0; z = 99 } in
      let p = PointOps.move p 10 20 in
      let p = PointOps.move_x p 5 in
      p.x + p.y + p.z
    |} 134);

  (* --- Poly update in fold over collection --- *)

  test "poly update fold accumulate record" (fun () ->
    expect_int {|
      let add_to_x r v = { r with x = r.x + v } in
      let result = for v in [1; 2; 3; 4; 5] with acc = { x = 0; y = 100 } do
        add_to_x acc v
      end in
      result.x + result.y
    |} 115);

  test "poly update fold with break" (fun () ->
    expect_int {|
      let add_to_x r v = { r with x = r.x + v } in
      let result = for v in [1; 2; 3; 100; 5] with acc = { x = 0; y = 42 } do
        if v > 50 do break
        else add_to_x acc v
      end in
      result.x + result.y
    |} 48);

  test "poly update fold with continue" (fun () ->
    expect_int {|
      let add_to_x r v = { r with x = r.x + v } in
      let result = for v in [1; 2; 3; 4; 5] with acc = { x = 0; y = 42 } do
        if v mod 2 = 0 do continue
        else add_to_x acc v
      end in
      result.x + result.y
    |} 51);

  (* --- Poly update + pattern matching on variants --- *)

  test "poly update in each match arm" (fun () ->
    expect_int {|
      type direction = Up | Down | Left | Right
      let move r (d: direction) =
        match d with
        | Up    -> { r with y = r.y + 1 }
        | Down  -> { r with y = r.y - 1 }
        | Left  -> { r with x = r.x - 1 }
        | Right -> { r with x = r.x + 1 }
      in
      let p = { x = 0; y = 0; name = "player" } in
      let p = move p Right in
      let p = move p Right in
      let p = move p Up in
      p.x + p.y
    |} 3);

  test "poly update with option pattern" (fun () ->
    expect_int {|
      let set_x_maybe r opt =
        match opt with
        | Some v -> { r with x = v }
        | None -> r
      in
      let r = { x = 0; y = 10; z = 20 } in
      let r = set_x_maybe r (Some 99) in
      let r = set_x_maybe r None in
      r.x + r.y + r.z
    |} 129);

  (* --- Poly update + recursion over variant data --- *)

  test "poly update driven by variant list" (fun () ->
    expect_int {|
      type cmd = SetX of int | SetY of int | AddX of int
      let rec apply_cmds r (cmds: cmd list) =
        match cmds with
        | [] -> r
        | cmd :: rest ->
          let r = match cmd with
            | SetX v -> { r with x = v }
            | SetY v -> { r with y = v }
            | AddX v -> { r with x = r.x + v }
          in
          apply_cmds r rest
      in
      let r = { x = 0; y = 0; tag = 999 } in
      let r = apply_cmds r [SetX 10; AddX 5; SetY 20; AddX 3] in
      r.x + r.y + r.tag
    |} 1037);

  (* --- Poly update with string interpolation and show --- *)

  test "poly update then interpolate fields" (fun () ->
    expect_string {|
      let rename r n = { r with name = n } in
      let r = { name = "x"; score = 42 } in
      let r = rename r "Alice" in
      $"{r.name} scored {r.score}"
    |} "Alice scored 42");

  (* --- Poly update with mutable + while loop --- *)

  test "poly update in while loop" (fun () ->
    expect_int {|
      let inc_x r = { r with x = r.x + 1 } in
      let mut r = { x = 0; y = 42 } in
      let mut i = 0 in
      for i < 10 do
        r := inc_x r;
        i := i + 1
      end;
      r.x + r.y
    |} 52);

  (* --- Poly update with deeply nested closures --- *)

  test "poly update with nested closures" (fun () ->
    expect_int {|
      let make_updater field_val =
        let offset = field_val * 2 in
        fn r -> { r with x = r.x + offset }
      in
      let f = make_updater 5 in
      let g = make_updater 3 in
      let r = { x = 0; y = 99 } in
      let r = f (g r) in
      r.x + r.y
    |} 115);

  (* --- Poly update returning from function early --- *)

  test "poly update with early return in loop" (fun () ->
    expect_int {|
      let find_and_update r (xs: int list) =
        for x in xs do
          if x > 10 do return { r with found = x } else ()
        end;
        { r with found = 0 - 1 }
      in
      let r = find_and_update { found = 0; extra = 42 } [1; 5; 15; 3] in
      r.found + r.extra
    |} 57);

  (* --- Poly update + try/with (effect as exception) --- *)

  test "poly update with try/with" (fun () ->
    expect_int {|
      effect Fail = fail : string -> 'a end
      let safe_update r v =
        if v < 0 do perform fail "negative"
        else { r with x = v }
      in
      let result =
        try
          let r = { x = 0; y = 42 } in
          safe_update r (0 - 5)
        with
        | fail msg -> { x = 999; y = 0 }
      in
      result.x + result.y
    |} 999);

  (* --- Multiple poly update functions with different field sets --- *)

  test "poly update functions touching different fields" (fun () ->
    expect_int {|
      let set_a r v = { r with a = v } in
      let set_b r v = { r with b = v } in
      let set_c r v = { r with c = v } in
      let set_d r v = { r with d = v } in
      let r = { a = 0; b = 0; c = 0; d = 0; sum = 0 } in
      let r = set_a r 1 in
      let r = set_b r 2 in
      let r = set_c r 3 in
      let r = set_d r 4 in
      let r = { r with sum = r.a + r.b + r.c + r.d } in
      r.sum
    |} 10);

  (* --- Poly update with list operations --- *)

  test "poly update in recursive list builder" (fun () ->
    expect_int {|
      let tag_item r i = { r with tag = i } in
      let rec build i =
        if i = 0 do []
        else tag_item { tag = 0; value = i * 10 } i :: build (i - 1)
      in
      let items = build 3 in
      let rec sum_tags xs = match xs with
        | [] -> 0
        | r :: rest -> r.tag + r.value + sum_tags rest
      in
      sum_tags items
    |} 66);

  (* --- Poly update function used polymorphically with two-field and ten-field records --- *)

  test "poly update extreme width difference" (fun () ->
    expect_int {|
      let set_x r v = { r with x = v } in
      let small = set_x { x = 0; y = 1 } 10 in
      let big = set_x { a = 1; b = 2; c = 3; d = 4; e = 5; f = 6; g = 7; x = 0 } 100 in
      small.x + big.x
    |} 110);

  (* --- Poly update with both record update AND field access in constraint --- *)

  test "poly update reads and writes same record" (fun () ->
    expect_int {|
      let double_x r = { r with x = r.x * 2 } in
      let r = { x = 7; y = 3 } in
      let r = double_x (double_x (double_x r)) in
      r.x + r.y
    |} 59);

  (* --- Poly update interacting with Eq on records --- *)

  test "poly update + structural equality check" (fun () ->
    expect_bool {|
      type pair = { x: int; y: int } deriving Eq
      let set_x r v = { r with x = v } in
      let a = { x = 1; y = 2 } in
      let b = set_x { x = 99; y = 2 } 1 in
      a = b
    |} true);

  (* --- Polymorphic update with higher-order + effects --- *)

  test "higher-order poly update + effect" (fun () ->
    expect_int {|
      effect Log = log : string -> unit end
      let apply_and_log f r =
        let r2 = f r in
        perform log (show (r2.x));
        r2
      in
      let set_x_to r = { r with x = 42 } in
      let mut logged = "" in
      handle
        let r = apply_and_log set_x_to { x = 0; y = 10 } in
        r.x + r.y
      with
      | return x -> x
      | log msg k -> logged := logged ^ msg; resume k ()
    |} 52);

  (* --- Poly update in recursive data processing --- *)

  test "poly update processes list of commands" (fun () ->
    expect_int {|
      let rec process r (cmds: int list) =
        match cmds with
        | [] -> r
        | n :: rest ->
          if n > 0 do
            process { r with x = r.x + n } rest
          else
            process { r with y = r.y + (0 - n) } rest
      in
      let r = process { x = 0; y = 0; z = 100 } [3; (0-2); 5; (0-1); 7] in
      r.x + r.y + r.z
    |} 118);

  Printf.printf "\n=== Variant Tests ===\n";

  test "variant creation and match" (fun () ->
    expect_int {|
      type color = Red | Green | Blue
      let to_int (c: color) : int =
        match c with
        | Red -> 0
        | Green -> 1
        | Blue -> 2
      ;;
      to_int Green
    |} 1);
  test "variant with payload" (fun () ->
    expect_int {|
      type opt = None | Some of int
      let get (o: opt) (d: int) : int =
        match o with
        | None -> d
        | Some v -> v
      ;;
      get (Some 42) 0
    |} 42);

  test "constructor as function" (fun () ->
    expect_int {|
      let f = Some;;
      match f 42 with
      | Some v -> v
      | None -> 0
    |} 42);

  test "constructor as function in map" (fun () ->
    expect_int {|
      let rec length xs = match xs with
        | [] -> 0
        | _ :: rest -> 1 + length rest
      ;;
      let rec map f xs = match xs with
        | [] -> []
        | x :: rest -> f x :: map f rest
      ;;
      length (map Some [1; 2; 3])
    |} 3);

  test "constructor as function passed to higher-order" (fun () ->
    expect_int {|
      type 'a box = Box of 'a;;
      let apply f x = f x;;
      match apply Box 10 with
      | Box v -> v
    |} 10);

  Printf.printf "\n=== Option Type Tests ===\n";

  test "option Some" (fun () ->
    expect_value {|Some 42|}
      (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VInt 42))));

  test "option None" (fun () ->
    expect_value {|None|}
      (Interpreter.Bytecode.VVariant (0, "None", None)));

  test "option pattern match Some" (fun () ->
    expect_int {|
      let x = Some 42;;
      match x with
        | Some n -> n
        | None -> 0
    |} 42);

  test "option pattern match None" (fun () ->
    expect_int {|
      match None with
        | Some n -> n
        | None -> 99
    |} 99);

  Printf.printf "\n=== Map Tests ===\n";

  test "empty map" (fun () ->
    expect_value {|#{}|}
      (Interpreter.Bytecode.VMap []));

  test "map get returns Some" (fun () ->
    expect_value {|
      let m = #{"a": 1; "b": 2};;
      get "a" m
    |}
      (Interpreter.Bytecode.VVariant (1, "Some", Some (Interpreter.Bytecode.VInt 1))));

  test "map get pattern match" (fun () ->
    expect_int {|
      let m = #{"a": 1; "b": 2};;
      match get "a" m with
        | Some v -> v
        | None -> 0
    |} 1);

  test "map get second key" (fun () ->
    expect_int {|
      let m = #{"a": 1; "b": 2};;
      match get "b" m with
        | Some v -> v
        | None -> 0
    |} 2);

  test "map get missing returns None" (fun () ->
    expect_value {|get "z" #{"a": 1}|}
      (Interpreter.Bytecode.VVariant (0, "None", None)));

  test "map set" (fun () ->
    expect_int {|
      let m = #{"a": 1};;
      let m2 = set "b" 2 m;;
      match get "b" m2 with
        | Some v -> v
        | None -> 0
    |} 2);

  test "map set overwrite" (fun () ->
    expect_int {|
      let m = #{"a": 1};;
      let m2 = set "a" 42 m;;
      match get "a" m2 with
        | Some v -> v
        | None -> 0
    |} 42);

  test "map has true" (fun () ->
    expect_bool {|has "a" #{"a": 1}|} true);

  test "map has false" (fun () ->
    expect_bool {|has "z" #{"a": 1}|} false);

  test "map size" (fun () ->
    expect_int {|size #{"a": 1; "b": 2}|} 2);

  test "map size empty" (fun () ->
    expect_int {|size #{}|} 0);

  test "map remove" (fun () ->
    expect_int {|
      let m = remove "a" #{"a": 1; "b": 2};;
      size m
    |} 1);

  test "map keys" (fun () ->
    expect_value {|keys #{"a": 1}|}
      (Interpreter.Bytecode.VList [Interpreter.Bytecode.VString "a"]));

  test "map values" (fun () ->
    expect_value {|values #{"a": 1}|}
      (Interpreter.Bytecode.VList [Interpreter.Bytecode.VInt 1]));

  test "map to_list" (fun () ->
    expect_value {|to_list #{"a": 1}|}
      (Interpreter.Bytecode.VList [
        Interpreter.Bytecode.VTuple [|
          Interpreter.Bytecode.VString "a";
          Interpreter.Bytecode.VInt 1
        |]
      ]));

  test "map int keys" (fun () ->
    expect_int {|
      match get 42 #{42: "answer"} with
        | Some v -> 1
        | None -> 0
    |} 1);

  test "map type error mixed values" (fun () ->
    expect_type_error {|#{"a": 1; "b": true}|});

  test "map type error mixed keys" (fun () ->
    expect_type_error {|#{1: "a"; "b": "c"}|});

  test "map immutable update" (fun () ->
    expect_int {|
      let m = #{"a": 1};;
      let _ = set "a" 99 m;;
      match get "a" m with
        | Some v -> v
        | None -> 0
    |} 1);

  test "map with expressions" (fun () ->
    expect_int {|
      let x = 10;;
      let m = #{x: x + 1; x + 1: x + 2};;
      match get 10 m with
        | Some v -> v
        | None -> 0
    |} 11);

  test "map higher-order" (fun () ->
    expect_int {|
      let lookup key m = match get key m with
        | Some v -> v
        | None -> 0;;
      lookup "x" #{"x": 42}
    |} 42);

  test "map of_list" (fun () ->
    expect_int {|
      let m = of_list [("a", 1); ("b", 2)];;
      match get "a" m with
        | Some v -> v
        | None -> 0
    |} 1);

  test "map typed syntax" (fun () ->
    expect_int {|
      let m = #{"x": 10; "y": 20};;
      match get "x" m with
        | Some v -> v
        | None -> 0
    |} 10);

  test "map typed syntax size" (fun () ->
    expect_int {|size #{"a": 1; "b": 2}|} 2);

  test "map typed empty" (fun () ->
    expect_int {|size #{}|} 0);

  Printf.printf "\n=== Array Tests ===\n";

  test "empty array" (fun () ->
    expect_value {| #[] |} (Interpreter.Bytecode.VArray [||]));

  test "array with elements" (fun () ->
    expect_value {| #[1; 2; 3] |}
      (Interpreter.Bytecode.VArray [|
        Interpreter.Bytecode.VInt 1;
        Interpreter.Bytecode.VInt 2;
        Interpreter.Bytecode.VInt 3
      |]));

  test "array_get" (fun () ->
    expect_int {| array_get #[10; 20; 30] 1 |} 20);

  test "array_length" (fun () ->
    expect_int {| array_length #[1; 2; 3] |} 3);

  test "array_length empty" (fun () ->
    expect_int {| array_length #[] |} 0);

  test "array out of bounds get" (fun () ->
    expect_runtime_error {| array_get #[1; 2] 5 |});


  test "array equality" (fun () ->
    expect_bool {| #[1; 2; 3] = #[1; 2; 3] |} true);

  test "array inequality" (fun () ->
    expect_bool {| #[1; 2] = #[1; 3] |} false);

  test "array pipe" (fun () ->
    expect_int {| #[1; 2; 3] |> array_length |} 3);

  test "array for loop" (fun () ->
    expect_unit {|
      for x in #[1; 2; 3] do
        print x
      end
    |});

  test "array fold for loop" (fun () ->
    expect_int {|
      for x in #[1; 2; 3] with acc = 0 do
        acc + x
      end
    |} 6);

  test "array type annotation" (fun () ->
    expect_int {|
      let f = fn (a : int array) -> array_get a 0 in
      f #[42; 1; 2]
    |} 42);

  Printf.printf "\n=== Generalized Collection Literal Tests ===\n";

  test "set literal" (fun () ->
    expect_stdlib_int {|Set.size #{1; 2; 3}|} 3);

  test "set literal mem" (fun () ->
    expect_stdlib_bool {|Set.mem 2 #{1; 2; 3}|} true);

  test "set literal dedup" (fun () ->
    expect_stdlib_int {|Set.size #{1; 2; 2; 3; 3; 3}|} 3);

  test "set literal single" (fun () ->
    expect_stdlib_int {|Set.size #{42}|} 1);

  test "empty braces still map" (fun () ->
    expect_stdlib_int {|size #{}|} 0);

  test "typed set explicit" (fun () ->
    expect_stdlib_int {|Set.size #Set{10; 20; 30}|} 3);

  test "typed set empty" (fun () ->
    expect_stdlib_int {|Set.size #Set{}|} 0);

  test "typed array bracket" (fun () ->
    expect_stdlib_int {|Array.length #Array[1; 2; 3]|} 3);

  test "typed array empty" (fun () ->
    expect_stdlib_int {|Array.length #Array[]|} 0);

  test "native array unchanged" (fun () ->
    expect_stdlib_int {|array_length #[10; 20; 30]|} 3);

  test "native map unchanged" (fun () ->
    expect_stdlib_int {|size #{"a": 1; "b": 2}|} 2);

  Printf.printf "\n=== Map Iteration Tests ===\n";

  test "map for loop" (fun () ->
    expect_stdlib_value {|
      for p in #{"x": 10} do
        let (k, _) = p in
        print k
      end
    |} Interpreter.Bytecode.VUnit);

  test "map fold" (fun () ->
    expect_stdlib_int {|
      fold (fn acc p -> let (_, v) = p in acc + v) 0 #{"x": 10; "y": 20}
    |} 30);

  Printf.printf "\n=== Array API Consolidation Tests ===\n";

  test "Array.make works" (fun () ->
    expect_stdlib_int {|Array.length (Array.make 5 0)|} 5);

  test "Array.set and get" (fun () ->
    expect_stdlib_int {|
      let a = Array.make 3 0 in
      let _ = Array.set a 1 42 in
      Array.get a 1
    |} 42);

  test "array_get still works" (fun () ->
    expect_int {| array_get #[10; 20; 30] 1 |} 20);

  test "array_length still works" (fun () ->
    expect_int {| array_length #[1; 2; 3] |} 3);

  Printf.printf "\n=== Indexing Tests ===\n";

  test "string indexing" (fun () -> expect_stdlib_int {|Byte.to_int ("hello".[0])|} 104);
  test "string indexing last" (fun () -> expect_stdlib_int {|Byte.to_int ("hello".[4])|} 111);
  test "array indexing" (fun () -> expect_int {|#[10; 20; 30].[1]|} 20);
  test "array indexing string" (fun () -> expect_string {|#["a"; "b"; "c"].[2]|} "c");
  test "string get" (fun () -> expect_stdlib_string {|Byte.to_string (String.get "hello" 0)|} "h");
  test "string to_bytes of_bytes roundtrip" (fun () ->
    expect_stdlib_string {|String.of_bytes (String.to_bytes "hi")|} "hi");
  test "string to_runes of_runes roundtrip" (fun () ->
    expect_stdlib_string {|String.of_runes (String.to_runes "abc")|} "abc");
  test "string get_rune" (fun () ->
    expect_stdlib_int {|Rune.to_int (String.get_rune "hello" 2)|} 108);
  test "string get_rune first" (fun () ->
    expect_stdlib_int {|Rune.to_int (String.get_rune "abc" 0)|} 97);
  test "string rune_length ascii" (fun () ->
    expect_stdlib_int {|String.rune_length "hello"|} 5);
  test "string rune_length empty" (fun () ->
    expect_stdlib_int {|String.rune_length ""|} 0);

  print_summary ()
