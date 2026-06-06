(* IR parity runner (roadmap #13).

   For every test program in the cross_test corpus, dump the LOWERED IR
   (`Ir_serialize`) from BOTH compilers and assert they are identical:
   - OCaml reference: Interpreter.Interp.emit_ir, in-process.
   - Self-hosted: one `--batch-emit-ir` invocation of the compiler bytecode on the
     OCaml VM, emitting IR per file separated by ===BATCH-SEP=== (same protocol
     parity_runner uses for batch compilation).

   A divergence means the two compilers lower the same source to structurally or
   type-wise different IR — exactly what value parity can miss. Programs that fail
   to compile on either side are SKIPPED here (compile/error parity is
   parity_runner's job); only programs that compile on both are compared. *)

open Test_format

(* Normalize for comparison: the self-host `print` adds a trailing newline the
   OCaml ref's `print_string` does not, and batch segments are accumulated
   line-wise — so compare on String.trim. *)
let norm s = String.trim s

(* Known, ALPHA-EQUIVALENT residual divergences (roadmap #13): the two compilers
   emit dict-evidence params in a different (but consistent) order for nested /
   match-arm-local constrained `let`s — a cross-scope emission-order difference
   that is value-parity-clean. The gate tolerates exactly these and FAILS on any
   new divergence (or if one of these is fixed — then remove it here). *)
let baseline =
  [
    "data_structures.tests :: module with multiple poly update functions";
    "gap_check.tests :: gap11 let in match arm with index poly";
    "typeclasses.tests :: expression-level let show used multiple times";
  ]

(* ---- Self-hosted: batch emit-IR in one invocation ---- *)

let batch_selfhost_emit_ir ~output_fn ~argv prepared tests =
  let tmpfiles =
    List.map
      (fun tc ->
        let f = Filename.temp_file "ir_parity_" ".mml" in
        let oc = open_out f in
        output_string oc tc.source;
        close_out oc;
        f)
      tests
  in
  let manifest = Filename.temp_file "ir_parity_manifest_" ".txt" in
  let oc = open_out manifest in
  List.iter (fun f -> output_string oc (f ^ "\n")) tmpfiles;
  close_out oc;
  argv := [| "compiler.json"; "--batch-emit-ir"; manifest |];
  let output_parts = ref [] in
  let total = List.length tests in
  let compiled_count = ref 0 in
  (output_fn :=
     fun s ->
       output_parts := s :: !output_parts;
       if s = "===BATCH-SEP===" then begin
         incr compiled_count;
         Printf.printf "\r  Self-host emit-IR: %d/%d%!" !compiled_count total
       end);
  let batch_err = ref "" in
  let batch_ok =
    try
      let _ = Interpreter.Deserialize.run_prepared prepared in
      true
    with exn ->
      batch_err := Printexc.to_string exn;
      false
  in
  output_fn := print_endline;
  if total > 0 then Printf.printf "\r%s\r%!" (String.make 50 ' ');
  List.iter (fun f -> try Sys.remove f with _ -> ()) tmpfiles;
  (try Sys.remove manifest with _ -> ());
  if not batch_ok then begin
    Printf.eprintf "\nSelf-hosted compiler crashed during batch emit-IR after %d/%d.\n  Error: %s\n%!"
      !compiled_count total !batch_err;
    Array.make total None
  end
  else begin
    let raw = List.rev !output_parts in
    let segments = ref [] in
    let current = Buffer.create 4096 in
    List.iter
      (fun line ->
        if line = "===BATCH-SEP===" then begin
          segments := Buffer.contents current :: !segments;
          Buffer.clear current
        end
        else begin
          if Buffer.length current > 0 then Buffer.add_char current '\n';
          Buffer.add_string current line
        end)
      raw;
    let arr = Array.of_list (List.rev !segments) in
    (* Map COMPILE-ERROR segments to None (skipped), else Some ir. *)
    Array.map
      (fun seg ->
        let s = String.trim seg in
        if String.length s >= 14 && String.sub s 0 14 = "COMPILE-ERROR:" then None
        else Some s)
      arr
  end

(* ---- OCaml reference: in-process emit-IR ---- *)

let ocaml_emit_ir state source =
  try Some (Interpreter.Interp.emit_ir state ~source ())
  with _ -> None

(* ---- Arg parsing (mirrors parity_runner) ---- *)

let parse_args argv =
  let files = ref [] in
  let filters = ref [] in
  let i = ref 1 in
  while !i < Array.length argv do
    if argv.(!i) = "-t" && !i + 1 < Array.length argv then begin
      filters := String.lowercase_ascii argv.(!i + 1) :: !filters;
      i := !i + 2
    end
    else begin
      files := argv.(!i) :: !files;
      incr i
    end
  done;
  (List.rev !files, !filters)

let () =
  let compiler_json_path =
    if Sys.file_exists "js/compiler.json" then "js/compiler.json"
    else if Sys.file_exists "../js/compiler.json" then "../js/compiler.json"
    else (
      Printf.eprintf "Cannot find js/compiler.json (run 'make self-host-compile-js')\n";
      exit 1)
  in
  Printf.printf "Loading self-hosted compiler from %s...\n%!" compiler_json_path;
  let ic = open_in compiler_json_path in
  let compiler_json_str = In_channel.input_all ic in
  close_in ic;
  let compiler_parsed = Interpreter.Deserialize.parse_json compiler_json_str in
  let init_state =
    Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ())
  in
  let builtins = Interpreter.Interp.builtin_table init_state in
  let prepared = Interpreter.Deserialize.prepare_bundle compiler_parsed builtins in
  Printf.printf "Compiler ready.\n%!";
  let arg_files, filters = parse_args Sys.argv in
  let files =
    if arg_files <> [] then arg_files
    else
      try find_test_files "cross_test/tests"
      with _ -> ( try find_test_files "tests" with _ -> (
        Printf.eprintf "No test files found.\n"; exit 1))
  in
  let file_tests =
    List.map (fun file -> (file, filter_tests filters (parse_test_file file))) files
  in
  let all_tests = List.concat_map snd file_tests in
  Printf.printf "Emitting IR for %d tests with the self-hosted compiler...\n%!"
    (List.length all_tests);
  let selfhost_ir =
    batch_selfhost_emit_ir ~output_fn:init_state.Interpreter.Interp.output_fn
      ~argv:init_state.Interpreter.Interp.argv prepared all_tests
  in
  Printf.printf "Comparing IR...\n\n%!";
  (* A fresh OCaml-ref state for emit-IR (independent per program). *)
  let ref_state =
    Interpreter.Std.register_all (Interpreter.Interp.repl_state_init ())
  in
  let passed = ref 0 and failed = ref 0 and skipped = ref 0 in
  let failures = ref [] in
  let idx = ref 0 in
  List.iter
    (fun (file, tests) ->
      Printf.printf "=== %s ===\n%!" (Filename.basename file);
      List.iter
        (fun tc ->
          let sh = if !idx < Array.length selfhost_ir then selfhost_ir.(!idx) else None in
          incr idx;
          let oc = ocaml_emit_ir ref_state tc.source in
          match (oc, sh) with
          | Some o, Some s ->
              if norm o = norm s then incr passed
              else begin
                incr failed;
                failures := (Filename.basename file ^ " :: " ^ tc.name) :: !failures;
                Printf.printf "  FAIL  %s\n" tc.name
              end
          | _ ->
              (* one or both failed to compile — out of scope for IR parity *)
              incr skipped)
        tests)
    file_tests;
  Printf.printf "\n==============================\n";
  Printf.printf
    "IR parity: %d passed, %d failed, %d skipped (uncompilable on a side)\n"
    !passed !failed !skipped;
  let failed_set = !failures in
  let unexpected = List.filter (fun n -> not (List.mem n baseline)) failed_set in
  let fixed = List.filter (fun n -> not (List.mem n failed_set)) baseline in
  if unexpected <> [] then begin
    Printf.printf "NEW IR divergences (not in baseline) — these must be fixed:\n";
    List.iter (fun n -> Printf.printf "  - %s\n" n) (List.rev unexpected);
    exit 1
  end
  else if fixed <> [] then begin
    Printf.printf
      "These baselined divergences now AGREE — remove them from `baseline` in \
       compiler_test/ir_parity_runner.ml:\n";
    List.iter (fun n -> Printf.printf "  - %s\n" n) fixed;
    exit 1
  end
  else
    Printf.printf
      "OK — %d known alpha-equivalent residual(s), no new divergences.\n"
      (List.length baseline)
