(* Formatter correctness runner (roadmap #21, increment 1).

   For every parseable source in the corpus, check the two invariants that make
   an opinionated formatter safe to run on real code:
     - SEMANTIC PRESERVATION: parse (format src) is structurally equal to
       parse src (modulo source locations). The formatter must never change what
       the program means.
     - IDEMPOTENCE: format (format src) = format src. Output is a fixed point.

   Sources that don't parse on the reference, or that use a construct the
   formatter doesn't handle yet (Formatter.Unsupported), are SKIPPED — exactly
   like ir_parity_runner skips uncompilable programs. As the printer learns more
   constructs, the skip count drops. A genuine mismatch (semantics changed, or
   output not stable) FAILS the gate. *)

open Test_format
module F = Interpreter.Formatter

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let mml_files dir =
  if not (Sys.file_exists dir) then []
  else
    Array.to_list (Sys.readdir dir)
    |> List.filter (fun e -> Filename.check_suffix e ".mml")
    |> List.map (fun e -> Filename.concat dir e)
    |> List.sort String.compare

let resolve rel =
  if Sys.file_exists rel then Some rel
  else
    let up = Filename.concat ".." rel in
    if Sys.file_exists up then Some up else None

let parse_ast src =
  let tokens = Interpreter.Lexer.tokenize src in
  Interpreter.Parser.parse_program tokens

let () =
  let passed = ref 0 and failed = ref 0 in
  let skip_parse = ref 0 and skip_unsupported = ref 0 in
  let failures = ref [] in
  let check label src =
    (* Only consider sources that parse on the reference. *)
    match parse_ast src with
    | exception _ -> incr skip_parse
    | original -> (
        match F.format_program original with
        | exception F.Unsupported _ -> incr skip_unsupported
        | exception e ->
            incr failed;
            failures := (label, "format raised " ^ Printexc.to_string e) :: !failures
        | formatted -> (
            (* semantic preservation: reparse and compare modulo locs *)
            match parse_ast formatted with
            | exception e ->
                incr failed;
                (if Sys.getenv_opt "FMT_DEBUG" <> None && !failed <= 5000 then
                   Printf.printf "\n--- REPARSE FAIL [%s]: %s ---\nSRC:\n%s\nFMT:\n%s\n---\n"
                     label (Printexc.to_string e) src formatted);
                failures :=
                  (label, "reparse of formatted failed: " ^ Printexc.to_string e)
                  :: !failures
            | reparsed ->
                let a = F.strip_program original in
                let b = F.strip_program reparsed in
                if a <> b then begin
                  incr failed;
                  (if Sys.getenv_opt "FMT_DEBUG" <> None && !failed <= 5000 then
                     Printf.printf "\n--- SEM CHANGE [%s] ---\nSRC:\n%s\nFMT:\n%s\n---\n"
                       label src formatted);
                  failures := (label, "semantics changed (AST differs after format)") :: !failures
                end
                else
                  (* idempotence *)
                  match F.format_program reparsed with
                  | exception _ ->
                      incr failed;
                      failures := (label, "second format raised") :: !failures
                  | formatted2 ->
                      if formatted <> formatted2 then begin
                        incr failed;
                        failures := (label, "not idempotent") :: !failures
                      end
                      else incr passed))
  in
  (* Corpus: cross-test case sources + real .mml files. *)
  let tests_dir =
    match resolve "cross_test/tests" with Some d -> Some d | None -> resolve "tests"
  in
  (match tests_dir with
  | None -> ()
  | Some dir ->
      List.iter
        (fun file ->
          let base = Filename.basename file in
          List.iter
            (fun tc -> check (base ^ " :: " ^ tc.name) tc.source)
            (parse_test_file file))
        (find_test_files dir));
  List.iter
    (fun path -> check (Filename.basename path) (read_file path))
    (List.concat_map mml_files (List.filter_map resolve [ "stdlib"; "self_host" ]));
  List.iter
    (fun (label, why) -> Printf.printf "  FAIL  %s — %s\n" label why)
    (List.rev !failures);
  Printf.printf "\n==============================\n";
  Printf.printf
    "Formatter: %d passed, %d failed (%d skipped: %d unparseable, %d unsupported construct)\n"
    !passed !failed (!skip_parse + !skip_unsupported) !skip_parse !skip_unsupported;
  if !failed > 0 then exit 1
