open Test_helpers
module Cst = Interpreter.Cst
module Cst_build = Interpreter.Cst_build
module Token = Interpreter.Token

(* Round-trip invariant: lexing then reassembling must reproduce the source
   byte-for-byte. Covers the lexically tricky constructs where a span bug would
   hide (comments, every string form, literals, operator spellings). *)
let roundtrips src () =
  let got = Cst.roundtrip src in
  if got <> src then
    failwith (Printf.sprintf "roundtrip mismatch\n  src     = %S\n  rebuilt = %S" src got)

let () =
  Printf.printf "=== CST Lossless Round-trip Tests (#17) ===\n";

  test "empty source" (roundtrips "");
  test "only whitespace" (roundtrips "   \n\t  \n");
  test "leading + trailing whitespace" (roundtrips "  \n  let x = 1  \n\n");
  test "simple expression" (roundtrips "let x = 1 + 2 * 3");

  (* Comments are trivia — recovered from inter-token gaps. *)
  test "block comment between tokens" (roundtrips "let (* a comment *) x = 1");
  test "nested block comment" (roundtrips "let x = (* outer (* inner *) still *) 1");
  test "comment-only source" (roundtrips "(* just a comment *)");
  test "comment with no trailing token" (roundtrips "let x = 1 (* trailing *)");
  test "leading comment then code" (roundtrips "(* header *)\nlet x = 1\n");

  (* Every string form keeps its exact source spelling. *)
  test "string with escapes" (roundtrips "let s = \"a\\tb\\nc\\\"d\"");
  test "string with unicode" (roundtrips "let s = \"héllo wörld\"");
  test "raw string" (roundtrips "let s = {|raw \"text\" with |whatever|}");
  test "interpolated string" (roundtrips "let s = $\"x = {x} and {y + 1}\"");
  test "adjacent strings" (roundtrips "[\"a\"; \"b\"; \"c\"]");

  (* Numeric / char-ish literals. *)
  test "int and float" (roundtrips "let a = 42 and b = 3.14 and c = 1e10");
  test "negative-looking" (roundtrips "let a = 0 - 5");

  (* Operators and punctuation spellings. *)
  test "operators" (roundtrips "a <= b && c >= d || e <> f");
  test "arrows and pipes" (roundtrips "fn x => x |> f :: rest");
  test "colons and assign" (roundtrips "x := y; let z : int = 0");
  test "polyvariant tag" (roundtrips "let t = `Ok 1");

  (* A multi-line, comment-rich program resembling real source. *)
  test "multi-line program"
    (roundtrips
       "(* module doc *)\n\
        let rec fib n =\n\
       \  if n < 2 do n  (* base case *)\n\
        else fib (n - 1) + fib (n - 2) (* recurse *)\n\
        end\n");

  (* The pieces stream: leading trivia + token text reassemble exactly, and the
     first token's leading trivia is the source prefix before it. *)
  test "pieces expose leading trivia" (fun () ->
      let src = "  (* c *) let x = 1" in
      let ps = Cst.of_source src in
      (match ps with
      | first :: _ ->
          if first.Cst.leading <> "  (* c *) " then
            failwith (Printf.sprintf "leading was %S" first.Cst.leading);
          if first.Cst.token.Token.kind <> Token.LET then
            failwith "first significant token should be LET"
      | [] -> failwith "no pieces");
      (* and EOF terminates the stream *)
      (match List.rev ps with
      | last :: _ when last.Cst.token.Token.kind = Token.EOF -> ()
      | _ -> failwith "last piece should be EOF"));

  Printf.printf "--- green tree (increment 2) ---\n";

  (* The flat green tree's to_source round-trips just like the piece stream. *)
  let green_roundtrips src () =
    let got = Cst.to_source (Cst.flat_of_source src) in
    if got <> src then
      failwith
        (Printf.sprintf "green to_source mismatch\n  src = %S\n  got = %S" src got)
  in
  test "green: empty" (green_roundtrips "");
  test "green: comment-rich program"
    (green_roundtrips "(* h *)\nlet x = 1 (* c *)\nlet y = $\"v={x}\"\n");
  test "green: raw + interp strings"
    (green_roundtrips "let a = {|raw|} and b = $\"{a}!\"");

  (* to_source is kind-agnostic: an arbitrary nesting of the same leaves over
     interior nodes reproduces the identical text. *)
  test "green: nesting is irrelevant to to_source" (fun () ->
      let src = "let x = 1 + 2" in
      let leaves = List.map (fun p -> Cst.Leaf p) (Cst.of_source src) in
      (* bracket the leaves arbitrarily under assorted node kinds *)
      let nested =
        match leaves with
        | a :: b :: rest ->
            Cst.Node
              ( Cst.SourceFile,
                [ Cst.Node (Cst.Decl, [ a; Cst.Node (Cst.Expr, [ b ]) ]) ]
                @ rest )
        | _ -> Cst.Node (Cst.SourceFile, leaves)
      in
      if Cst.to_source nested <> src then
        failwith (Printf.sprintf "nested to_source = %S" (Cst.to_source nested)));

  (* leaves / tokens recover the stream from the tree. *)
  test "green: tokens recovers significant tokens" (fun () ->
      let src = "let x = 1" in
      let t = Cst.flat_of_source src in
      let kinds = List.map (fun (tk : Token.token) -> tk.Token.kind) (Cst.tokens t) in
      match kinds with
      | [ Token.LET; Token.IDENT "x"; Token.EQ; Token.INT 1; Token.EOF ] -> ()
      | _ -> failwith "unexpected token kinds from tree");

  Printf.printf "--- parser-produced CST (increment 3) ---\n";

  let rec count_nodes kind = function
    | Cst.Leaf _ -> 0
    | Cst.Node (k, cs) ->
        (if k = kind then 1 else 0)
        + List.fold_left (fun acc c -> acc + count_nodes kind c) 0 cs
  in
  let parsed_roundtrips src () =
    let t = Cst_build.cst_of_source src in
    if Cst.to_source t <> src then
      failwith (Printf.sprintf "parsed to_source mismatch: %S" (Cst.to_source t))
  in

  test "parsed: round-trips simple decl" (parsed_roundtrips "let x = 1 + 2");
  test "parsed: round-trips comment-rich program"
    (parsed_roundtrips "(* h *)\nlet rec f n = if n < 2 do n else f (n - 1)\n");
  (* the one backtracking site: numeric-for and the for/while fallback *)
  test "parsed: numeric for round-trips"
    (parsed_roundtrips "let _ = for i = 0; i < 10; i + 1 do print i end");
  test "parsed: for-while fallback round-trips"
    (parsed_roundtrips "let _ = for x < 10 do set x = x + 1 end");

  (* The tree is genuinely structured: SourceFile root over Decl over Expr,
     not a degenerate flat list. *)
  test "parsed: tree is structured" (fun () ->
      let t = Cst_build.cst_of_source "let x = f 1 + 2" in
      (match t with
      | Cst.Node (Cst.SourceFile, _) -> ()
      | _ -> failwith "root should be SourceFile");
      if count_nodes Cst.Decl t < 1 then failwith "expected a Decl node";
      if count_nodes Cst.Expr t < 2 then failwith "expected nested Expr nodes");

  (* Increment 3b: binop precedence nesting + match-arm nodes. *)
  test "parsed: binop chain is left-nested (precede)" (fun () ->
      let src = "let r = 1 + 2 + 3" in
      let t = Cst_build.cst_of_source src in
      if Cst.to_source t <> src then failwith "binop chain not lossless";
      (* 3 operand atoms + 2 completed binop spans = >=5 Expr nodes, and they
         nest (the outer binop contains the inner one). *)
      if count_nodes Cst.Expr t < 5 then
        failwith (Printf.sprintf "expected >=5 Expr nodes, got %d" (count_nodes Cst.Expr t));
      (* deepest Expr nesting must exceed 2 (flat siblings would stay shallow) *)
      let rec depth = function
        | Cst.Leaf _ -> 0
        | Cst.Node (k, cs) ->
            let sub = List.fold_left (fun a c -> max a (depth c)) 0 cs in
            (if k = Cst.Expr then 1 else 0) + sub
      in
      if depth t < 3 then failwith "binop nesting too shallow — precede not wrapping");

  test "parsed: one MatchArm node per arm" (fun () ->
      let t =
        Cst_build.cst_of_source "let f x = match x with | 0 -> 0 | 1 -> 1 | _ -> 2"
      in
      let n = count_nodes Cst.MatchArm t in
      if n <> 3 then failwith (Printf.sprintf "expected 3 MatchArm nodes, got %d" n));

  (* Increment 3c: params, record fields, handler arms, fn-match arms. *)
  test "parsed: Param node per parameter" (fun () ->
      let t = Cst_build.cst_of_source "let f x (y : int) z = x" in
      let n = count_nodes Cst.Param t in
      if n <> 3 then failwith (Printf.sprintf "expected 3 Param nodes, got %d" n));

  test "parsed: RecordField nodes (expr)" (fun () ->
      let t = Cst_build.cst_of_source "let r = { a = 1; b = 2; c = 3 }" in
      let n = count_nodes Cst.RecordField t in
      if n <> 3 then failwith (Printf.sprintf "expected 3 RecordField nodes, got %d" n));

  test "parsed: RecordField nodes (type decl)" (fun () ->
      let t = Cst_build.cst_of_source "type point = { x : int; y : int }" in
      let n = count_nodes Cst.RecordField t in
      if n <> 2 then failwith (Printf.sprintf "expected 2 type RecordField nodes, got %d" n));

  test "parsed: HandlerArm nodes" (fun () ->
      let src =
        "let r = handle f () with | get _ k -> k 1 | put v k -> k () | return x -> x"
      in
      let t = Cst_build.cst_of_source src in
      let n = count_nodes Cst.HandlerArm t in
      if n <> 3 then failwith (Printf.sprintf "expected 3 HandlerArm nodes, got %d" n);
      if Cst.to_source t <> src then failwith "handler round-trip failed");

  test "parsed: fn-match arms are MatchArm nodes" (fun () ->
      let t = Cst_build.cst_of_source "let g = fn | 0 -> true | _ -> false" in
      let n = count_nodes Cst.MatchArm t in
      if n <> 2 then failwith (Printf.sprintf "expected 2 fn-match MatchArm nodes, got %d" n));

  (* Type and pattern atoms get their own nodes. *)
  test "parsed: type and pattern nodes present" (fun () ->
      let t =
        Cst_build.cst_of_source
          "let f (x : int) = match x with | 0 -> 0 | _ -> 1"
      in
      if count_nodes Cst.TypeExpr t < 1 then failwith "expected a TypeExpr node";
      if count_nodes Cst.Pattern t < 1 then failwith "expected a Pattern node");

  print_summary ()
