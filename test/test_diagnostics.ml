open Test_helpers
module D = Interpreter.Diagnostic
module A = Interpreter.Analysis

let st = get_analysis_ctx ()

(* The single diagnostic [src] should produce, or fail loudly. *)
let only src : D.t =
  match A.diagnostics st src with
  | [ d ] -> d
  | ds -> failwith (Printf.sprintf "expected 1 diagnostic, got %d" (List.length ds))

let clean src () =
  match A.diagnostics st src with
  | [] -> ()
  | ds -> failwith (Printf.sprintf "expected no diagnostics, got %d: %s" (List.length ds)
                      (String.concat " | " (List.map D.to_string ds)))

let code_is code src () =
  let d = only src in
  if d.D.code <> code then
    failwith (Printf.sprintf "code: got %S want %S (%s)" d.D.code code (D.to_string d))

let msg_has substr src () =
  let d = only src in
  if not (contains_substring d.D.message substr) then
    failwith (Printf.sprintf "message %S lacks %S" d.D.message substr)

let () =
  Printf.printf "=== Diagnostics Tests (#19) ===\n";

  (* clean sources produce nothing *)
  test "clean: simple let" (clean "let x = 1");
  test "clean: function" (clean "let add a b = a + b");

  (* each phase reports its code *)
  test "parse error code" (code_is "parse" "let x = ");
  test "parse error: stray token" (code_is "parse" "let x = )");
  test "type error: unbound var" (code_is "type" "let x = no_such_name_xyz");
  test "lex error: unterminated comment" (code_is "lex" "let x = 1 (* oops");

  (* messages survive the conversion *)
  test "unbound message mentions the name" (msg_has "no_such_name_xyz" "let x = no_such_name_xyz");

  (* spans: a parse error points at a real (positive) position covering >= 1 char *)
  test "span is well-formed" (fun () ->
      let d = only "let x = )" in
      let s = d.D.span in
      if not (s.D.lo.line >= 1 && s.D.lo.col >= 1) then failwith "bad lo";
      if not (s.D.hi.offset >= s.D.lo.offset) then failwith "hi before lo");

  (* span covers the offending token, not just its first byte *)
  test "span covers the token" (fun () ->
      (* `let 1 = x` — the `1` is an unexpected token; span should be width 1+ *)
      let d = only "let 1 = x" in
      if d.D.span.D.hi.offset <= d.D.span.D.lo.offset then
        failwith (Printf.sprintf "zero-width span: %s" (D.to_string d)));

  (* --- error recovery (#18): one broken declaration doesn't blank the file --- *)
  let codes src = List.map (fun (d : D.t) -> d.D.code) (A.diagnostics st src) in

  test "recovery: two independent parse errors both reported" (fun () ->
      (* `let a = )` recovers; `let c = *` is a second, separate parse error *)
      let cs = codes "let a = )\nlet b = 1\nlet c = *" in
      if List.length cs < 2 then
        failwith (Printf.sprintf "expected >=2 diagnostics, got %d" (List.length cs)));

  test "recovery: parse error AND type error both surface" (fun () ->
      let cs = codes "let a = )\nlet b = unbound_name_xyz" in
      if not (List.mem "parse" cs && List.mem "type" cs) then
        failwith (String.concat "," cs));

  test "recovery: a valid decl after a broken one is still analyzed" (fun () ->
      (* the type error proves the partial AST (with the broken decl skipped)
         was typechecked *)
      let cs = codes "let broken = )\nlet bad : int = \"a string\"" in
      if not (List.mem "type" cs) then failwith ("no type diag: " ^ String.concat "," cs));

  test "recovery still reports nothing for clean multi-decl source"
    (clean "let a = 1;;\nlet b = 2;;\nlet c = a + b");

  (* --- richer diagnostics: every type error, and warnings --- *)
  test "every type error is reported, not just the first" (fun () ->
      let ts =
        List.filter (fun (d : D.t) -> d.D.code = "type")
          (A.diagnostics st "let a : int = \"s\"\nlet b : bool = 5")
      in
      if List.length ts < 2 then
        failwith (Printf.sprintf "expected >=2 type errors, got %d" (List.length ts)));

  test "later declarations still see earlier good bindings" (fun () ->
      (* `a` is well-typed; `b` uses it; only the bad `c` errors *)
      clean "let a = 1\nlet b = a + 1" ());

  test "typechecker warnings surface as Warning diagnostics" (fun () ->
      match
        List.find_opt (fun (d : D.t) -> d.D.code = "warning")
          (A.diagnostics st "let f n = match n with | 0 -> 1 | 0 -> 2 | _ -> 3")
      with
      | Some d when d.D.severity = D.Warning -> ()
      | Some _ -> failwith "warning has wrong severity"
      | None -> failwith "no warning diagnostic");

  print_summary ()
