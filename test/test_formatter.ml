open Test_helpers
module F = Interpreter.Formatter

let parse src =
  Interpreter.Parser.parse_program (Interpreter.Lexer.tokenize src)

(* Semantic preservation: format then reparse yields the same AST (modulo locs
   and the is_generated hint). *)
let preserves src () =
  let a = F.strip_program (parse src) in
  let b = F.strip_program (parse (F.format_source src)) in
  if a <> b then
    failwith (Printf.sprintf "AST changed.\n  fmt:\n%s" (F.format_source src))

(* Idempotence: a second format is a no-op. *)
let idempotent src () =
  let f1 = F.format_source src in
  let f2 = F.format_source f1 in
  if f1 <> f2 then
    failwith (Printf.sprintf "not idempotent.\n  f1:\n%s\n  f2:\n%s" f1 f2)

let both src () =
  preserves src ();
  idempotent src ()

(* The canonical output for a snippet is exactly [expected]. *)
let formats_to src expected () =
  let got = F.format_source src in
  if got <> expected then
    failwith (Printf.sprintf "expected:\n%S\ngot:\n%S" expected got)

let () =
  Printf.printf "=== Formatter Tests (#21) ===\n";

  (* Round-trip safety across a spread of constructs. *)
  List.iter
    (fun (name, src) -> test name (both src))
    [
      ("binops", "let x = 1 + 2 * 3 - 4");
      ("operator as value", "let s = fold (+) 0 [1; 2; 3]");
      ("keyword operator value", "let f = (land) in f 6 2");
      ("if-else no end", "let m = if a do b else c");
      ("if no-else has end", "let u = if a do b end");
      ("match", "let f x = match x with | 0 -> true | _ -> false");
      ("let-in chain", "let a = 1 in let b = 2 in a + b");
      ("unit param", "let f () = 42 in f ()");
      ("tuple destructure param", "let add (x, y) = x + y in add (10, 32)");
      ("field assign", "let p = { x = 1 } in p.x := 9; p.x");
      ("seq with match", "let mut x = 0 in (match a with | _ -> x := 1); x");
      ("nullary ctor args", "f None None");
      ("for-as-while", "let mut x = 0 in for x < 3 do x := x + 1 end; x");
      ("numeric for", "let _ = for i = 0; i < 3; i + 1 do print i end");
      ("record + update", "let r = { a = 1; b = 2 } in { r with a = 9 }");
      ("rune literal", "Rune.to_int 'a'");
      ("float", "let pi = 3.14 in pi + 0.0000123");
      ("type decl", "type ('a, 'b) pair = { fst : 'a; snd : 'b }");
      ("variant", "type color = Red | Green | Blue");
      ("extern operator", "extern (^) : string -> string -> string");
      ("effect arrow type", "extern print : 'a -> unit / IO");
      ("top-level mutable then assign", "let mut x = 0;; x := 5;; x");
      (* increment 2: declaration forms *)
      ("module", "module M =\n  pub let x = 42\n  let secret = 99\nend\n;;\nM.x");
      ("class", "class Eq 'a =\n  (=) : 'a -> 'a -> bool\n  (<>) : 'a -> 'a -> bool\nend");
      ("instance", "instance Show ('a option) where Show 'a =\n  let show o = match o with | None -> \"None\" | Some x -> show x\nend");
      ("effect", "effect State 'a =\n  get : unit -> 'a\n  put : 'a -> unit\nend");
      ("selective open", "module M =\n  pub let a = 1\n  pub let b = 2\nend\n;;\nopen M (a, b)");
      ("nested module", "module Outer =\n  module Inner =\n    pub let v = 1\n  end\nend");
      ("seq with match in middle", "let f x = (match x with | 0 -> () | _ -> ()); x");
      ("string with escapes", "let s = \"line1\\nline2\\ttab\\\\back\"");
      (* increment 3: remaining constructs *)
      ("GADT", "type 'a expr =\n  | IntLit : int -> int expr\n  | If : bool expr * 'a expr * 'a expr -> 'a expr");
      ("GADT fn-typed ctor", "type 'a t =\n  | Fn : ('a -> 'a) -> 'a t");
      ("map pattern", "let f m = match m with | #{\"a\": x} -> x | _ -> 0");
      ("set pattern", "let f s = match s with | #{1; 2} -> true | _ -> false");
      ("typed array", "Array.length #Array[1; 2; 3]");
      ("typed array empty", "Array.length #Array[]");
      ("typed set", "Set.size #Set{10; 20; 30}");
      ("polyvariant type", "let f (x : [> `A | `B of int ]) = x");
      ("expr let-rec-and", "let r = let rec ev x = od x and od x = ev x in ev 3 in r");
      ("LAT expr let rec", "let r = let rec (type 'a) id (x : 'a) : 'a = x in id 5");
      (* comment increment 1: top-level comments are preserved *)
      ("leading line comment", "-- doc for x\nlet x = 1");
      ("trailing line comment", "let x = 1 -- trailing\nlet y = 2");
      ("block comment", "(* a block *)\nlet x = 1");
      ("multi-line block comment", "(* line one\n   line two *)\nlet x = 1");
      ("stacked leading comments", "-- one\n-- two\nlet x = 1");
      ("tail comment at eof", "let x = 1\n-- bye");
      ("comment between decls", "let a = 1\n-- mid\nlet b = 2");
      (* comment increment 2: module-body comments are preserved too *)
      ("module-body leading comment", "module M =\n  -- doc\n  pub let x = 1\nend");
      ("module-body trailing comment", "module M =\n  pub let x = 1 -- note\n  pub let y = 2\nend");
      ("comment before module end", "module M =\n  pub let x = 1\n  -- last\nend");
      ("nested module comment", "module M =\n  module N =\n    -- deep\n    pub let z = 1\n  end\nend");
      ("comment on module decl itself", "-- the module\nmodule M =\n  pub let x = 1\nend");
      (* comment increment 3: inline (intra-declaration) statement comments *)
      ("inline comment in let-chain", "let f x =\n  -- base\n  let b = x in\n  b + 1");
      ("inline comment in sequence", "let f x =\n  print x;\n  -- then\n  x");
      ("inline comment before body", "let f x =\n  -- start\n  x + 1");
      ("inline comment in loop body", "let f xs =\n  for x in xs do\n    -- each\n    print x\n  end");
      ("inline comment in if branch", "let f x =\n  if x do\n    -- yes\n    1\n  else\n    -- no\n    2");
      ("trailing inline comment hoists", "let f x =\n  let a = x in -- note\n  a");
      ("multi-line inline comment", "let f x =\n  -- one\n  -- two\n  x");
      (* comment increment 4: class/instance/effect member comments *)
      ("class method comment", "class Hash 'a =\n  -- hashing\n  hash : 'a -> int\nend");
      ("instance method comment", "instance Show int =\n  -- render it\n  let show n = \"n\"\nend");
      ("effect op comment", "effect State 'a =\n  -- read\n  get : unit -> 'a\n  -- write\n  put : 'a -> unit\nend");
      ("multi-line instance method comment", "instance Hash int =\n  -- line one\n  -- line two\n  let hash n = n\nend");
      (* width-based wrapping: round-trips whether flat or broken *)
      ("short list flat", "let a = [1; 2; 3]");
      ("long list wraps", "let a = [100000; 200000; 300000; 400000; 500000; 600000; 700000; 800000; 900000]");
      ("long record wraps", "let r = { alpha = 1; beta = 2; gamma = 3; delta = 4; epsilon = 5; zeta = 6; eta = 7 }");
      ("long application wraps", "let v = some_function aaaaaaaaaa bbbbbbbbbb cccccccccc dddddddddd eeeeeeeeee ffffffffff");
      ("long tuple wraps", "let t = (aaaaaaaaaa, bbbbbbbbbb, cccccccccc, dddddddddd, eeeeeeeeee, ffffffffff, gggggg)");
    ];

  (* A couple of exact canonical-output checks (locks the style). *)
  test "canonical: simple let"
    (formats_to "let   x=1+2" "let x =\n  1 + 2\n");
  test "canonical: top-level decls get ;;"
    (formats_to "let a = 1\nlet b = 2"
       "let a =\n  1;;\n\nlet b =\n  2\n");
  test "canonical: leading comment sits above its decl"
    (formats_to "-- doc\nlet a = 1" "-- doc\nlet a =\n  1\n");
  test "canonical: trailing comment stays on the decl line"
    (formats_to "let a = 1 -- note\nlet b = 2"
       "let a =\n  1;; -- note\n\nlet b =\n  2\n");
  test "canonical: line comments are right-trimmed"
    (formats_to "-- trailing spaces   \nlet a = 1" "-- trailing spaces\nlet a =\n  1\n");
  (* paren pruning: precedence/associativity drop redundant parens *)
  test "canonical: precedence drops parens"
    (formats_to "let a = 1 + 2 * 3 - 4" "let a =\n  1 + 2 * 3 - 4\n");
  test "canonical: lower-precedence operand keeps parens"
    (formats_to "let a = (1 + 2) * 3" "let a =\n  (1 + 2) * 3\n");
  test "canonical: left-assoc same level drops parens"
    (formats_to "let a = x - y - z" "let a =\n  x - y - z\n");
  test "canonical: right operand of left-assoc keeps parens"
    (formats_to "let a = x - (y - z)" "let a =\n  x - (y - z)\n");
  test "canonical: cons is right-assoc, no parens"
    (formats_to "let a = 1 :: 2 :: 3 :: []" "let a =\n  1 :: 2 :: 3 :: []\n");
  (* width-based wrapping: a short collection stays on one line *)
  test "canonical: short list stays flat"
    (formats_to "let a = [ 1 ; 2 ; 3 ]" "let a =\n  [1; 2; 3]\n");
  test "canonical: width-overflowing list breaks, fits stays flat" (fun () ->
      (* short list: stays on the body line *)
      let short = F.format_source "let a = [1; 2; 3; 4; 5]" in
      if short <> "let a =\n  [1; 2; 3; 4; 5]\n" then failwith ("short: " ^ short);
      (* long list: breaks one element per line, every line within the width *)
      let long =
        F.format_source
          "let a = [111111; 222222; 333333; 444444; 555555; 666666; 777777; 888888; 999999; \
           101010; 202020; 303030; 404040; 505050; 606060]"
      in
      if not (List.length (String.split_on_char '\n' long) > 3) then
        failwith ("long did not wrap: " ^ long);
      List.iter
        (fun l -> if String.length l > 80 then failwith ("over width: " ^ l))
        (String.split_on_char '\n' long));

  print_summary ()
