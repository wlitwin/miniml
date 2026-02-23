(* Translator test runner
   Parses .tests files with OCaml input / expected MiniML output pairs,
   runs each through the OCaml-to-MiniML translator, and compares results. *)

type test_case = {
  name : string;
  source : string;
  expected : string;
}

(* --- Parsing ------------------------------------------------------------ *)

type parse_state =
  | Idle
  | CollectingSource of string
  | CollectingExpected of string * string  (* name, source *)

let starts_with s prefix =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let parse_test_file filename =
  let ic = open_in filename in
  let tests = ref [] in
  let state = ref Idle in
  let buf = Buffer.create 256 in
  let flush name source =
    let expected = String.trim (Buffer.contents buf) in
    tests := { name; source; expected } :: !tests;
    state := Idle;
    Buffer.clear buf
  in
  (try while true do
    let line = input_line ic in
    let trimmed = String.trim line in
    if starts_with trimmed "--- test:" then begin
      let name = String.trim (String.sub trimmed 9 (String.length trimmed - 9)) in
      state := CollectingSource name;
      Buffer.clear buf
    end
    else if starts_with trimmed "--- expect:" then begin
      match !state with
      | CollectingSource name ->
        let source = String.trim (Buffer.contents buf) in
        let rest = String.trim (String.sub trimmed 11 (String.length trimmed - 11)) in
        if rest <> "" then begin
          (* Single-line expected *)
          tests := { name; source; expected = rest } :: !tests;
          state := Idle;
          Buffer.clear buf
        end else begin
          state := CollectingExpected (name, source);
          Buffer.clear buf
        end
      | _ -> ()
    end
    else if trimmed = "--- end" then begin
      match !state with
      | CollectingExpected (name, source) -> flush name source
      | _ -> ()
    end
    else if starts_with trimmed "===" then
      () (* skip section headers *)
    else begin
      match !state with
      | CollectingSource _ ->
        if Buffer.length buf > 0 then Buffer.add_char buf '\n';
        Buffer.add_string buf line
      | CollectingExpected _ ->
        if Buffer.length buf > 0 then Buffer.add_char buf '\n';
        Buffer.add_string buf line
      | Idle -> ()
    end
  done with End_of_file -> ());
  close_in ic;
  List.rev !tests

(* --- Translator invocation ---------------------------------------------- *)

let translator_path = ref ""

let run_translator source =
  let tmpfile = Filename.temp_file "translate_test_" ".ml" in
  let oc = open_out tmpfile in
  output_string oc source;
  close_out oc;
  let cmd = Printf.sprintf "%s %s 2>&1"
    (Filename.quote !translator_path) (Filename.quote tmpfile) in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try while true do
    let line = input_line ic in
    if Buffer.length buf > 0 then Buffer.add_char buf '\n';
    Buffer.add_string buf line
  done with End_of_file -> ());
  let status = Unix.close_process_in ic in
  (try Sys.remove tmpfile with _ -> ());
  match status with
  | Unix.WEXITED 0 -> Ok (Buffer.contents buf)
  | Unix.WEXITED _ -> Error (Buffer.contents buf)
  | _ -> Error "translator killed by signal"

let strip_module_wrapper output =
  let lines = String.split_on_char '\n' output in
  (* Drop first line if it matches "module <Name> =" *)
  let lines = match lines with
    | first :: rest ->
      let t = String.trim first in
      if String.length t > 9
         && String.sub t 0 7 = "module "
         && t.[String.length t - 1] = '='
      then rest
      else first :: rest
    | [] -> []
  in
  (* Drop trailing empty lines, then drop "end" *)
  let lines = List.rev lines in
  let rec drop_empty = function
    | "" :: rest -> drop_empty rest
    | lines -> lines
  in
  let lines = drop_empty lines in
  let lines = match lines with
    | last :: rest when String.trim last = "end" -> rest
    | lines -> lines
  in
  let lines = List.rev (drop_empty lines) in
  (* Dedent by 2 spaces *)
  let dedent line =
    if String.length line >= 2 && String.sub line 0 2 = "  " then
      String.sub line 2 (String.length line - 2)
    else line
  in
  let lines = List.map dedent lines in
  String.concat "\n" lines

(* --- Running ------------------------------------------------------------- *)

let run_tests tests =
  let passed = ref 0 in
  let failed = ref 0 in
  let failures = ref [] in
  List.iter (fun tc ->
    flush stdout;
    match run_translator tc.source with
    | Error msg ->
      Printf.printf "  FAIL: %s\n    translator error: %s\n" tc.name msg;
      incr failed;
      failures := tc.name :: !failures
    | Ok raw_output ->
      let actual = String.trim (strip_module_wrapper raw_output) in
      let expected = String.trim tc.expected in
      if actual = expected then begin
        Printf.printf "  PASS: %s\n" tc.name;
        incr passed
      end else begin
        Printf.printf "  FAIL: %s\n" tc.name;
        let exp_lines = String.split_on_char '\n' expected in
        let act_lines = String.split_on_char '\n' actual in
        Printf.printf "    expected:\n";
        List.iter (fun l -> Printf.printf "      |%s|\n" l) exp_lines;
        Printf.printf "    actual:\n";
        List.iter (fun l -> Printf.printf "      |%s|\n" l) act_lines;
        incr failed;
        failures := tc.name :: !failures
      end
  ) tests;
  (!passed, !failed, !failures)

(* --- Main ---------------------------------------------------------------- *)

let () =
  (* Find translator binary *)
  let candidates = [
    "_build/default/tools/ocaml_to_mml/main.exe";
    Filename.concat (Sys.getcwd ()) "_build/default/tools/ocaml_to_mml/main.exe";
  ] in
  translator_path := (
    try List.find Sys.file_exists candidates
    with Not_found ->
      Printf.eprintf "Translator not found. Run 'dune build' first.\n";
      exit 1
  );
  let files =
    if Array.length Sys.argv > 1 then
      Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
    else begin
      Printf.eprintf "Usage: runner <file.tests> [...]\n";
      exit 1
    end
  in
  let total_passed = ref 0 in
  let total_failed = ref 0 in
  let all_failures = ref [] in
  List.iter (fun file ->
    Printf.printf "=== %s ===\n" (Filename.basename file);
    let tests = parse_test_file file in
    let (p, f, failures) = run_tests tests in
    total_passed := !total_passed + p;
    total_failed := !total_failed + f;
    all_failures := !all_failures @ failures
  ) files;
  Printf.printf "\n==============================\n";
  Printf.printf "%d/%d translator tests passed"
    !total_passed (!total_passed + !total_failed);
  if !total_failed > 0 then
    Printf.printf " (%d FAILED)" !total_failed;
  Printf.printf "\n";
  if !total_failed > 0 then begin
    Printf.printf "Failures:\n";
    List.iter (fun name -> Printf.printf "  - %s\n" name) !all_failures;
    exit 1
  end
