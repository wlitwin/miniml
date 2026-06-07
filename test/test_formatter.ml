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
    ];

  (* A couple of exact canonical-output checks (locks the style). *)
  test "canonical: simple let"
    (formats_to "let   x=1+2" "let x =\n  1 + 2\n");
  test "canonical: top-level decls get ;;"
    (formats_to "let a = 1\nlet b = 2"
       "let a =\n  1;;\n\nlet b =\n  2\n");

  print_summary ()
