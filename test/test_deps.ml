open Test_helpers
module S = Interpreter.Semver
module M = Interpreter.Manifest
module Dp = Interpreter.Deps

let v s = match S.parse s with Some x -> x | None -> failwith ("bad version " ^ s)
let req m ver : M.require = { module_path = m; version = v ver }
let manifest reqs : M.t = { name = "m"; mml = None; requires = reqs; replaces = [] }

(* synthetic registry: (module, "vX.Y.Z") -> its requirements *)
let load registry m ver : M.t =
  match List.assoc_opt (m, S.to_string ver) registry with
  | Some reqs -> manifest reqs
  | None -> manifest []

let sel result name = List.assoc_opt name result

let () =
  Printf.printf "=== Dependency / MVS Tests (#23) ===\n";

  (* semver *)
  test "semver parses and compares" (fun () ->
      if S.parse "v1.2.3" = None then failwith "parse v1.2.3";
      if S.parse "1.2.3" = None then failwith "parse 1.2.3";
      if S.parse "nope" <> None then failwith "bad parse accepted";
      if S.compare (v "v1.2.0") (v "v1.10.0") >= 0 then failwith "1.2 < 1.10";
      if S.compare (v "v2.0.0") (v "v1.9.9") <= 0 then failwith "2.0 > 1.9";
      if S.to_string (S.max (v "v1.0.0") (v "v1.0.1")) <> "v1.0.1" then failwith "max");

  (* manifest *)
  test "manifest parses module / require / replace / comments" (fun () ->
      let m =
        M.parse
          "module example.com/me/p\n\
           mml 0.1\n\
           require example.com/a v1.2.0   # a comment\n\
           require example.com/b v0.3.1\n\
           replace example.com/b => ../b\n"
      in
      if m.M.name <> "example.com/me/p" then failwith "name";
      if List.length m.M.requires <> 2 then failwith "requires";
      if M.replacement m "example.com/b" <> Some "../b" then failwith "replace");

  (* MVS: max of the minimums *)
  test "mvs selects the maximum required minimum" (fun () ->
      let registry =
        [
          (("A", "v1.0.0"), [ req "C" "v1.1.0" ]);
          (("B", "v1.0.0"), [ req "C" "v1.2.0" ]);
        ]
      in
      let result = Dp.mvs (manifest [ req "A" "v1.0.0"; req "B" "v1.0.0" ]) (load registry) in
      if sel result "C" <> Some (v "v1.2.0") then failwith "C should be v1.2.0");

  (* MVS: a transitive requirement bumps a direct one *)
  test "mvs follows transitive requirements" (fun () ->
      let registry = [ (("D", "v1.0.0"), [ req "A" "v1.5.0" ]) ] in
      let result = Dp.mvs (manifest [ req "A" "v1.0.0"; req "D" "v1.0.0" ]) (load registry) in
      if sel result "A" <> Some (v "v1.5.0") then failwith "A should be bumped to v1.5.0");

  (* MVS follows EVERY reachable version's requirements (Go semantics): a lower
     version that is required somewhere still contributes its dependencies *)
  test "mvs includes a reachable lower version's dependency" (fun () ->
      let registry =
        [
          (("A", "v1.0.0"), [ req "Mod" "v1.0.0" ]);
          (("B", "v1.0.0"), [ req "Mod" "v2.0.0" ]);
          (("Mod", "v1.0.0"), [ req "X" "v9.0.0" ]);
          (("Mod", "v2.0.0"), []);
        ]
      in
      let result = Dp.mvs (manifest [ req "A" "v1.0.0"; req "B" "v1.0.0" ]) (load registry) in
      if sel result "Mod" <> Some (v "v2.0.0") then failwith "Mod should be v2.0.0";
      if sel result "X" <> Some (v "v9.0.0") then failwith "X (from Mod v1.0.0) should be present");

  (* a diamond resolves to one version, deterministically *)
  test "mvs is deterministic on a diamond" (fun () ->
      let registry =
        [
          (("Left", "v1.0.0"), [ req "Base" "v1.1.0" ]);
          (("Right", "v1.0.0"), [ req "Base" "v1.3.0" ]);
        ]
      in
      let root = manifest [ req "Left" "v1.0.0"; req "Right" "v1.0.0" ] in
      let r1 = Dp.mvs root (load registry) and r2 = Dp.mvs root (load registry) in
      if r1 <> r2 then failwith "non-deterministic";
      if sel r1 "Base" <> Some (v "v1.3.0") then failwith "Base should be v1.3.0");

  print_summary ()
