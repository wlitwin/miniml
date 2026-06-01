(* Differential runner CLI — run MiniML programs on multiple backends and
   report agreement/disagreement. See differential.ml for the engine.

   Usage:
     diff_runner [options] prog.mml ...           run each .mml file
     diff_runner [options] --tests file.tests ... run every test case's source
                                                  (expected values are ignored —
                                                  agreement IS the check)

   Options:
     --backends oracle,vm,emit-js,native   which backends (default: all)
     --fast                                 = --backends oracle,vm,emit-js
     --timeout SECONDS                      per-backend time limit (default 10)
     --expect-disagree                      invert the exit code: succeed only
                                            if a disagreement IS found (used by
                                            the smoke test to verify detection)

   Exit code: 0 if every program agrees across backends (or, with
   --expect-disagree, if at least one disagreement was found); 1 otherwise. *)

let usage () =
  prerr_endline
    "Usage: diff_runner [--backends LIST] [--fast] [--timeout SEC] \
     [--expect-disagree] [--tests] file ...";
  exit 2

type input = Program of string (* .mml path *) | Tests of string (* .tests path *)

let () =
  let backends = ref Differential.all_backends in
  let timeout = ref 10.0 in
  let expect_disagree = ref false in
  let inputs = ref [] in
  let tests_mode = ref false in
  let argv = Sys.argv in
  let i = ref 1 in
  while !i < Array.length argv do
    (match argv.(!i) with
    | "--backends" when !i + 1 < Array.length argv ->
        incr i;
        backends :=
          String.split_on_char ',' argv.(!i)
          |> List.map (fun name ->
                 match Differential.backend_of_string (String.trim name) with
                 | Some b -> b
                 | None ->
                     Printf.eprintf "Unknown backend: %s\n" name;
                     exit 2)
    | "--fast" -> backends := Differential.fast_backends
    | "--timeout" when !i + 1 < Array.length argv ->
        incr i;
        timeout := float_of_string argv.(!i)
    | "--expect-disagree" -> expect_disagree := true
    | "--tests" -> tests_mode := true
    | arg when String.length arg > 0 && arg.[0] = '-' ->
        Printf.eprintf "Unknown option: %s\n" arg;
        usage ()
    | file ->
        inputs :=
          (if !tests_mode then Tests file else Program file) :: !inputs);
    incr i
  done;
  let inputs = List.rev !inputs in
  if inputs = [] then usage ();

  (* Build interpreter state once; backend forks inherit it copy-on-write. *)
  let state = Differential.make_state () in

  let total = ref 0 in
  let agreements = ref 0 in
  let disagreements = ref [] in

  let run_one ~name source =
    incr total;
    let results =
      Differential.run_all ~backends:!backends ~state ~timeout:!timeout source
    in
    match Differential.verdict results with
    | Differential.Agree result ->
        incr agreements;
        Printf.printf "  AGREE  %s  [%s]\n%!" name
          (Differential.result_label result)
    | Differential.Disagree results ->
        disagreements := name :: !disagreements;
        Printf.printf "  DISAGREE  %s\n" name;
        List.iter
          (fun (b, r) ->
            Printf.printf "    %-8s  %s\n"
              (Differential.backend_name b)
              (Differential.pp_result r))
          results;
        let dissenting = Differential.dissenters results in
        Printf.printf "    --> %s differ%s from %s\n%!"
          (String.concat ", "
             (List.map Differential.backend_name dissenting))
          (if List.length dissenting = 1 then "s" else "")
          (Differential.backend_name (fst (List.hd results)))
  in

  List.iter
    (fun input ->
      match input with
      | Program path ->
          let ic = open_in path in
          let source = In_channel.input_all ic in
          close_in ic;
          Printf.printf "=== %s ===\n" (Filename.basename path);
          run_one ~name:(Filename.basename path) source
      | Tests path ->
          Printf.printf "=== %s ===\n" (Filename.basename path);
          let tests = Test_format.parse_test_file path in
          List.iter
            (fun tc ->
              run_one ~name:tc.Test_format.name tc.Test_format.source)
            tests)
    inputs;

  Printf.printf "\n==============================\n";
  Printf.printf "%d/%d programs agree across [%s]\n" !agreements !total
    (String.concat ", " (List.map Differential.backend_name !backends));
  if !disagreements <> [] then begin
    Printf.printf "Disagreements:\n";
    List.iter (Printf.printf "  - %s\n") (List.rev !disagreements)
  end;
  if !expect_disagree then
    if !disagreements <> [] then begin
      Printf.printf "(--expect-disagree: disagreement detected, as expected)\n";
      exit 0
    end
    else begin
      Printf.printf
        "(--expect-disagree: NO disagreement found — detection is broken?)\n";
      exit 1
    end
  else if !disagreements <> [] then exit 1
