open Test_helpers

let () =
  Printf.printf "=== For Loop Tests ===\n";

  test "unit for basic" (fun () ->
    expect_int {|
      let mut n = 0 in
      for x in [1; 2; 3] do
        n := n + x
      end;
      n
    |} 6);

  test "unit for returns unit" (fun () ->
    expect_unit {|
      for x in [1; 2; 3] do x end
    |});

  test "unit for empty list" (fun () ->
    expect_unit {|
      for x in [] do x end
    |});

  test "fold for sum" (fun () ->
    expect_int {|
      for x in [1; 2; 3] with acc = 0 do
        acc + x
      end
    |} 6);

  test "fold for string concat" (fun () ->
    expect_string {|
      for x in [1; 2; 3] with s = "" do
        s ^ string_of_int x
      end
    |} "123");

  test "fold for empty list" (fun () ->
    expect_int {|
      let xs : int list = [] in
      for x in xs with acc = 0 do acc + x end
    |} 0);

  test "unit for break" (fun () ->
    expect_int {|
      let mut n = 0 in
      for x in [1; 2; 3; 4; 5] do
        if x = 4 do break
        else n := n + x
      end;
      n
    |} 6);

  test "fold for break" (fun () ->
    expect_int {|
      for x in [1; 2; 3; 4; 5] with acc = 0 do
        if x = 4 do break
        else acc + x
      end
    |} 6);

  test "nested for loops" (fun () ->
    expect_int {|
      let mut n = 0 in
      for x in [1; 2] do
        for y in [10; 20] do
          n := n + x * y
        end
      end;
      n
    |} 90);

  test "user Iter implementation" (fun () ->
    expect_int {|
      (* Define a binary tree type *)
      type tree = Leaf | Node of (int * tree * tree);;

      (* Make it iterable by implementing Iter — just define fold *)
      instance Iter tree int =
        let fold f acc t =
          let rec go a tr = match tr with
            | Leaf -> a
            | Node payload ->
              let (v, left, right) = payload in
              let a = go a left in
              let a = f a v in
              go a right
          in go acc t
      end

      let my_tree = Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf));;

      (* Now for loops just work: *)

      (* fold form — sum all elements *)
      for x in my_tree with sum = 0 do sum + x end
    |} 6
  );

  test "break outside loop error" (fun () ->
    expect_type_error {| break |});

  Printf.printf "\n=== While Loop Tests ===\n";

  test "while loop basic" (fun () ->
    expect_int {|
      let mut x = 0 in
      for x < 5 do
        x := x + 1
      end;
      x
    |} 5);

  test "while loop zero iterations" (fun () ->
    expect_int {|
      let mut x = 10 in
      for x < 5 do
        x := x + 1
      end;
      x
    |} 10);

  test "while loop returns unit" (fun () ->
    expect_unit {|
      let mut x = 0 in
      for x < 3 do
        x := x + 1
      end
    |});

  test "while loop with accumulation" (fun () ->
    expect_int {|
      let mut sum = 0 in
      let mut i = 1 in
      for i <= 10 do
        sum := sum + i;
        i := i + 1
      end;
      sum
    |} 55);

  Printf.printf "\n=== Infinite Loop Tests ===\n";

  test "infinite loop with break" (fun () ->
    expect_int {|
      let mut x = 0 in
      for do
        x := x + 1;
        if x = 5 do break else ()
      end;
      x
    |} 5);

  Printf.printf "\n=== While Break/Continue Tests ===\n";

  test "while break" (fun () ->
    expect_int {|
      let mut x = 0 in
      for true do
        x := x + 1;
        if x = 3 do break else ()
      end;
      x
    |} 3);

  test "while continue" (fun () ->
    expect_int {|
      let mut sum = 0 in
      let mut i = 0 in
      for i < 10 do
        i := i + 1;
        if i mod 2 = 0 do continue else ();
        sum := sum + i
      end;
      sum
    |} 25);

  test "while break and continue" (fun () ->
    expect_int {|
      let mut sum = 0 in
      let mut i = 0 in
      for i < 100 do
        i := i + 1;
        if i mod 2 = 0 do continue else ();
        if i > 10 do break else ();
        sum := sum + i
      end;
      sum
    |} 25);

  Printf.printf "\n=== While-let Tests ===\n";

  test "while-let basic" (fun () ->
    expect_int {|
      let mut xs = [1; 2; 3] in
      let mut sum = 0 in
      for let x :: rest = xs do
        sum := sum + x;
        xs := rest
      end;
      sum
    |} 6);

  test "while-let with option pattern" (fun () ->
    expect_int {|
      let mut n = Some 3 in
      let mut sum = 0 in
      for let Some x = n do
        sum := sum + x;
        n := if x > 1 do Some (x - 1) else None
      end;
      sum
    |} 6);

  Printf.printf "\n=== For-in Break/Continue Tests ===\n";

  test "for-in break" (fun () ->
    expect_int {|
      let mut sum = 0 in
      for x in [1; 2; 3; 4; 5] do
        if x = 4 do break else ();
        sum := sum + x
      end;
      sum
    |} 6);

  test "for-in continue" (fun () ->
    expect_int {|
      let mut sum = 0 in
      for x in [1; 2; 3; 4; 5] do
        if x mod 2 = 0 do continue else ();
        sum := sum + x
      end;
      sum
    |} 9);

  Printf.printf "\n=== Fold Break/Continue Tests ===\n";

  test "fold break with value" (fun () ->
    expect_int {|
      for x in [1; 2; 3; 4; 5] with acc = 0 do
        if x = 4 do break acc else acc + x
      end
    |} 6);

  test "fold break with default" (fun () ->
    expect_int {|
      for x in [1; 2; 3; 4; 5] with acc = 0 do
        if x = 4 do break else acc + x
      end
    |} 6);

  test "fold continue" (fun () ->
    expect_int {|
      for x in [1; 2; 3; 4; 5] with acc = 0 do
        if x mod 2 = 0 do continue else acc + x
      end
    |} 9);

  test "fold break with custom value" (fun () ->
    expect_int {|
      for x in [1; 2; 3; 4; 5] with acc = 0 do
        if x = 3 do break 100 else acc + x
      end
    |} 100);

  Printf.printf "\n=== Return Tests ===\n";

  test "return from function" (fun () ->
    expect_int {|
      let f x =
        if x > 0 do return (x * 2) else ();
        0 - 1
      in
      f 5
    |} 10);

  test "return from inside while loop" (fun () ->
    expect_int {|
      let f () =
        let mut i = 0 in
        for i < 10 do
          if i = 5 do return i else ();
          i := i + 1
        end;
        0 - 1
      in
      f ()
    |} 5);

  test "return from inside for-in loop" (fun () ->
    expect_int {|
      let f () =
        let mut result = 0 - 1 in
        for x in [1; 3; 4; 6] do
          if x mod 2 = 0 do (result := x; return result) else ()
        end;
        result
      in
      f ()
    |} 4);

  test "return from nested context" (fun () ->
    expect_int {|
      let f x =
        let y = x + 1 in
        if y > 5 do return y else ();
        y * 2
      in
      f 10
    |} 11);

  Printf.printf "\n=== Nested Loop Tests ===\n";

  test "nested for loops with break" (fun () ->
    expect_int {|
      let mut count = 0 in
      for x in [1; 2; 3] do
        for y in [10; 20; 30] do
          count := count + 1;
          if y = 20 do break else ()
        end
      end;
      count
    |} 6);

  test "nested while with break" (fun () ->
    expect_int {|
      let mut outer = 0 in
      let mut i = 0 in
      for i < 3 do
        let mut j = 0 in
        for j < 5 do
          if j = 2 do break else ();
          j := j + 1
        end;
        outer := outer + 1;
        i := i + 1
      end;
      outer
    |} 3);

  Printf.printf "\n=== Loop Error Cases ===\n";

  test "break outside of loop" (fun () ->
    expect_type_error_msg {|
      break
    |} "break outside of loop");

  test "continue outside of loop" (fun () ->
    expect_type_error_msg {|
      continue
    |} "continue outside of loop");

  test "break with value in while loop" (fun () ->
    expect_type_error_msg {|
      let mut x = 0 in
      for true do
        x := break 42
      end
    |} "break with value only allowed in fold loops");

  Printf.printf "\n=== Underscore Loop Variable Tests ===\n";

  test "for _ in fold" (fun () ->
    expect_int {|
      for _ in [1; 2; 3] with acc = 0 do
        acc + 1
      end
    |} 3);

  test "for _ in unit" (fun () ->
    expect_int {|
      let mut n = 0 in
      for _ in [10; 20; 30] do
        n := n + 1
      end;
      n
    |} 3);

  test "for _ in fold with break" (fun () ->
    expect_int {|
      for _ in [1; 2; 3; 4; 5] with acc = 0 do
        if acc >= 3 do break else acc + 1
      end
    |} 3);

  Printf.printf "\n=== For-In Pattern Tests ===\n";

  test "for-in tuple pattern" (fun () ->
    expect_int {|
      let pairs = [(1, 10); (2, 20); (3, 30)] in
      let mut sum = 0 in
      for (k, v) in pairs do
        sum := sum + k + v
      end;
      sum
    |} 66);

  test "for-in tuple pattern fold" (fun () ->
    expect_int {|
      let pairs = [(1, 10); (2, 20); (3, 30)] in
      for (k, v) in pairs with acc = 0 do
        acc + k * v
      end
    |} 140);

  test "for-in record pattern" (fun () ->
    expect_int {|
      let points = [{x = 1; y = 2}; {x = 3; y = 4}; {x = 5; y = 6}] in
      let mut sum = 0 in
      for {x; y} in points do
        sum := sum + x + y
      end;
      sum
    |} 21);

  test "for-in record pattern fold" (fun () ->
    expect_int {|
      let points = [{x = 1; y = 2}; {x = 3; y = 4}] in
      for {x; y} in points with acc = 0 do
        acc + x * y
      end
    |} 14);

  test "for-in nested tuple pattern" (fun () ->
    expect_int {|
      let triples = [(1, (2, 3)); (4, (5, 6))] in
      let mut sum = 0 in
      for (a, (b, c)) in triples do
        sum := sum + a + b + c
      end;
      sum
    |} 21);

  test "for-in cons pattern" (fun () ->
    expect_int {|
      let lists = [[1; 2; 3]; [4; 5; 6]] in
      let mut sum = 0 in
      for (x :: _) in lists do
        sum := sum + x
      end;
      sum
    |} 5);

  test "for-in constructor pattern" (fun () ->
    expect_int {|
      let opts = [Some 10; Some 20; Some 30] in
      for (Some x) in opts with acc = 0 do
        acc + x
      end
    |} 60);

  test "for-in pattern with break" (fun () ->
    expect_int {|
      let pairs = [(1, 10); (2, 20); (3, 30); (4, 40)] in
      for (k, v) in pairs with acc = 0 do
        if k = 3 do break else acc + v
      end
    |} 30);

  test "for-in pattern with continue" (fun () ->
    expect_int {|
      let pairs = [(1, 10); (2, 20); (3, 30); (4, 40)] in
      for (k, v) in pairs with acc = 0 do
        if k mod 2 = 0 do continue else acc + v
      end
    |} 40);

  test "for-in pattern with break value" (fun () ->
    expect_int {|
      let pairs = [(1, 10); (2, 20); (3, 30)] in
      for (k, v) in pairs with acc = 0 do
        if k = 2 do break 999 else acc + v
      end
    |} 999);

  test "for-in unit loop with tuple pattern" (fun () ->
    expect_int {|
      let mut sum = 0 in
      for (x, y) in [(1, 2); (3, 4); (5, 6)] do
        sum := sum + x * y
      end;
      sum
    |} 44);

  Printf.printf "\n=== Accumulator Pattern Tests ===\n";

  test "fold with tuple accumulator pattern" (fun () ->
    expect_int {|
      let xs = [1; 2; 3; 4; 5] in
      let result = for x in xs with (sum, count) = (0, 0) do
        (sum + x, count + 1)
      end in
      let (s, c) = result in
      s * 100 + c
    |} 1505);

  test "fold with tuple acc pattern break" (fun () ->
    expect_int {|
      let result = for x in [1; 2; 3; 4; 5] with (sum, count) = (0, 0) do
        if x = 4 do break else (sum + x, count + 1)
      end in
      let (s, c) = result in
      s * 100 + c
    |} 603);

  test "fold with tuple acc pattern continue" (fun () ->
    expect_int {|
      let result = for x in [1; 2; 3; 4; 5] with (sum, count) = (0, 0) do
        if x mod 2 = 0 do continue else (sum + x, count + 1)
      end in
      let (s, c) = result in
      s * 100 + c
    |} 903);

  test "fold with record accumulator pattern" (fun () ->
    expect_int {|
      let result = for x in [10; 20; 30] with {total; n} = {total = 0; n = 0} do
        {total = total + x; n = n + 1}
      end in
      result.total + result.n
    |} 63);

  test "both element and acc patterns" (fun () ->
    expect_int {|
      let pairs = [(1, 10); (2, 20); (3, 30)] in
      let result = for (k, v) in pairs with (ks, vs) = (0, 0) do
        (ks + k, vs + v)
      end in
      let (ks, vs) = result in
      ks + vs
    |} 66);

  test "nested pattern with fold" (fun () ->
    expect_int {|
      let data = [(1, (2, 3)); (4, (5, 6))] in
      for (a, (b, c)) in data with sum = 0 do
        sum + a + b + c
      end
    |} 21);

  test "tuple acc break returns current acc" (fun () ->
    expect_int {|
      let result = for x in [1; 2; 3] with (a, b) = (0, 0) do
        if x = 2 do break else (a + x, b + 1)
      end in
      let (a, b) = result in
      a * 10 + b
    |} 11);

  test "list pattern in for-in" (fun () ->
    expect_int {|
      let xss = [[1; 2]; [3; 4]; [5; 6]] in
      for [a; b] in xss with sum = 0 do
        sum + a + b
      end
    |} 21);

  print_summary ()
