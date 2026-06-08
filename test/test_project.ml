open Test_helpers
module P = Interpreter.Project

let mk name source : P.unit_ =
  { P.name; path = String.uncapitalize_ascii name ^ ".mml"; source }

let names (us : P.unit_ list) = List.map (fun (u : P.unit_) -> u.P.name) us

let contains hay needle = contains_substring hay needle

let () =
  Printf.printf "=== Project / build Tests (#23) ===\n";

  (* file -> module name *)
  test "module name capitalizes the basename" (fun () ->
      if P.module_name_of "src/foo.mml" <> "Foo" then failwith "foo";
      if P.module_name_of "string_utils.mml" <> "String_utils" then failwith "snake");

  (* manifest *)
  test "manifest reads the module name" (fun () ->
      let m = Interpreter.Manifest.parse "module myproj\nmml 0.1\n" in
      if m.Interpreter.Manifest.name <> "myproj" then failwith m.Interpreter.Manifest.name);

  (* dependency inference: Name. and open Name, not a bare constructor *)
  test "deps come from qualified access and open" (fun () ->
      let ds = P.deps_of [ "A"; "B"; "C" ] (mk "X" "let v = A.f (open B) C") in
      (* A.f -> A; open B -> B; bare C (constructor-like) -> not a dep *)
      if not (List.mem "A" ds && List.mem "B" ds) then failwith "missing A/B";
      if List.mem "C" ds then failwith "bare C should not be a dependency");

  test "a module does not depend on itself" (fun () ->
      if P.deps_of [ "A" ] (mk "A" "let v = A.x") <> [] then failwith "self-dep");

  (* topological order: dependencies precede dependents, regardless of input order *)
  test "topological order" (fun () ->
      let a = mk "A" "pub let x = 1" in
      let b = mk "B" "pub let y = A.x" in
      let c = mk "C" "pub let z = B.y + A.x" in
      if names (P.topo_order [ c; a; b ]) <> [ "A"; "B"; "C" ] then
        failwith (String.concat "," (names (P.topo_order [ c; a; b ]))));

  test "dependency cycle is rejected" (fun () ->
      match P.topo_order [ mk "A" "let v = B.x"; mk "B" "let v = A.x" ] with
      | _ -> failwith "expected a cycle error"
      | exception P.Build_error msg -> if not (contains msg "cycle") then failwith msg);

  (* combined source: libraries wrapped as modules in order, entry last *)
  test "combined source wraps libs and appends the entry" (fun () ->
      let a = mk "A" "pub let x = 1" in
      let b = mk "B" "pub let y = A.x" in
      let proj = { P.name = "p"; entry = mk "Main" "let main = print B.y"; libs = [ b; a ]; tests = [] } in
      let src = P.combined_source proj in
      if not (contains src "module A =") then failwith "no module A";
      if not (contains src "module B =") then failwith "no module B";
      if not (contains src "let main = print B.y") then failwith "no entry";
      (* A must appear before B (dependency order) *)
      let idx s =
        let n = String.length s and m = String.length src in
        let r = ref (-1) in
        for i = 0 to m - n do if String.sub src i n = s then (if !r < 0 then r := i) done;
        !r
      in
      if idx "module A =" > idx "module B =" then failwith "A should precede B");

  (* line map: a combined-source line resolves back to (file, line) *)
  test "line map attributes combined lines to source files" (fun () ->
      let a = mk "A" "pub let x = 1\npub let y = 2" in
      let proj = { P.name = "p"; entry = mk "Main" "let main = print A.x"; libs = [ a ]; tests = [] } in
      let _, segs = P.combined_with_map proj in
      let check l ef el =
        let f, ln = P.map_line segs l in
        if not (contains f ef && ln = el) then
          failwith (Printf.sprintf "line %d -> %s:%d, want %s:%d" l f ln ef el)
      in
      (* combined line 1 is `module A =`; A's source begins at line 2 *)
      check 2 "a.mml" 1;
      check 3 "a.mml" 2);

  (* test discovery: top-level `let test_*` (incl. let rec), not other bindings *)
  test "test_decl_names finds the test functions" (fun () ->
      let u =
        mk "Foo_test"
          "let test_a () = true\nlet helper x = x\nlet test_b () = false\nlet rec test_c () = test_c ()"
      in
      let names = P.test_decl_names u in
      if not (List.mem "test_a" names && List.mem "test_b" names && List.mem "test_c" names) then
        failwith (String.concat "," names);
      if List.mem "helper" names then failwith "helper is not a test");

  (* `*_test.mml` is a test file, excluded from libs *)
  test "load separates test files from libraries" (fun () ->
      let write path s = let oc = open_out path in output_string oc s; close_out oc in
      let root = Filename.temp_dir "mml_tst_" "" in
      Fun.protect
        ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
        (fun () ->
          write (Filename.concat root "mml.mod") "module app\n";
          write (Filename.concat root "calc.mml") "pub let add a b = a + b\n";
          write (Filename.concat root "calc_test.mml") "let test_add () = Calc.add 1 1 = 2\n";
          write (Filename.concat root "main.mml") "let main = ()\n";
          let p = P.load root in
          if List.exists (fun (u : P.unit_) -> u.P.name = "Calc_test") p.P.libs then
            failwith "test file leaked into libs";
          if not (List.exists (fun (u : P.unit_) -> u.P.name = "Calc_test") p.P.tests) then
            failwith "test file not in tests";
          if not (List.exists (fun (u : P.unit_) -> u.P.name = "Calc") p.P.libs) then
            failwith "Calc lib missing"));

  (* a dependency's modules are gathered into the build (via a local replace) *)
  test "load resolves a replaced dependency's modules" (fun () ->
      let write path s = let oc = open_out path in output_string oc s; close_out oc in
      let root = Filename.temp_dir "mml_root_" "" in
      let dep = Filename.temp_dir "mml_dep_" "" in
      Fun.protect
        ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s %s" root dep)))
        (fun () ->
          write (Filename.concat dep "mml.mod") "module example.com/lib\n";
          write (Filename.concat dep "util.mml") "pub let f x = x\n";
          write (Filename.concat root "mml.mod")
            (Printf.sprintf
               "module app\nrequire example.com/lib v1.0.0\nreplace example.com/lib => %s\n" dep);
          write (Filename.concat root "main.mml") "let main = print (Util.f 1)\n";
          let p = P.load root in
          if not (List.exists (fun (u : P.unit_) -> u.P.name = "Util") p.P.libs) then
            failwith "dependency module Util was not gathered into the build"));

  print_summary ()
