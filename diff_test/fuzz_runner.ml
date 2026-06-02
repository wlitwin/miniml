(* Differential fuzzer (roadmap #8) — generate random well-typed MiniML
   programs and check that every backend agrees on their behavior.

   Usage:
     fuzz_runner [options]
       --count N       programs to generate and run        (default 100)
       --seed N        base seed; program i uses seed N+i  (default: from time,
                                                            printed for replay)
       --size N        expression-node budget per program  (default 25)
       --backends LIST / --fast / --full                   (default: fast tier)
       --timeout SEC   per-backend timeout                 (default 10)
       --out DIR       where to save disagreeing programs  (default fuzz_failures)
       --print-programs   dump every generated program (debugging the generator)
       --shrink        auto-minimize each disagreement (saves seed_N.min.mml too)

   Every program is reproducible: `fuzz_runner --seed S --count 1` regenerates
   exactly the program that seed S produced. Disagreements are written to
   <out>/seed_S.mml with a .verdict.txt report alongside.

   Exit code: 0 if every program agreed, 1 if any disagreement was found,
   2 on usage error. *)

let usage () =
  prerr_endline
    "Usage: fuzz_runner [--count N] [--seed N] [--size N] [--backends LIST | \
     --fast | --full] [--timeout SEC] [--out DIR] [--print-programs]";
  exit 2

let () =
  let count = ref 100 in
  let seed = ref (-1) in
  let size = ref 25 in
  let backends = ref Differential.fast_backends in
  let timeout = ref 10.0 in
  let out_dir = ref "fuzz_failures" in
  let print_programs = ref false in
  let auto_shrink = ref false in
  let argv = Sys.argv in
  let i = ref 1 in
  while !i < Array.length argv do
    (match argv.(!i) with
    | "--count" when !i + 1 < Array.length argv ->
        incr i;
        count := int_of_string argv.(!i)
    | "--seed" when !i + 1 < Array.length argv ->
        incr i;
        seed := int_of_string argv.(!i)
    | "--size" when !i + 1 < Array.length argv ->
        incr i;
        size := int_of_string argv.(!i)
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
    | "--full" -> backends := Differential.all_backends
    | "--timeout" when !i + 1 < Array.length argv ->
        incr i;
        timeout := float_of_string argv.(!i)
    | "--out" when !i + 1 < Array.length argv ->
        incr i;
        out_dir := argv.(!i)
    | "--print-programs" -> print_programs := true
    | "--shrink" -> auto_shrink := true
    | _ -> usage ());
    incr i
  done;

  (* A random base seed unless one was given — always printed so any run can
     be replayed exactly. *)
  let base_seed =
    if !seed >= 0 then !seed
    else (
      Random.self_init ();
      Random.int 1_000_000_000)
  in
  Printf.printf "fuzzing: %d programs, base seed %d, size %d, backends [%s]\n%!"
    !count base_seed !size
    (String.concat ", " (List.map Differential.backend_name !backends));

  (* Interpreter state built once; backend forks inherit it copy-on-write. *)
  let state = Differential.make_state () in

  let disagreements = ref [] in
  let category_counts = Hashtbl.create 8 in
  let bump cat =
    Hashtbl.replace category_counts cat
      (1 + Option.value ~default:0 (Hashtbl.find_opt category_counts cat))
  in

  let save_failure prog_seed program results =
    if not (Sys.file_exists !out_dir) then Unix.mkdir !out_dir 0o755;
    let base = Printf.sprintf "%s/seed_%d" !out_dir prog_seed in
    let oc = open_out (base ^ ".mml") in
    output_string oc program;
    close_out oc;
    let oc = open_out (base ^ ".verdict.txt") in
    Printf.fprintf oc "seed: %d\nsize: %d\nbackends: %s\n\n" prog_seed !size
      (String.concat ", " (List.map Differential.backend_name !backends));
    List.iter
      (fun (b, r) ->
        Printf.fprintf oc "--- %s ---\n%s\n\n"
          (Differential.backend_name b)
          (Differential.pp_result r))
      results;
    close_out oc;
    base ^ ".mml"
  in

  for idx = 0 to !count - 1 do
    let prog_seed = base_seed + idx in
    let program = Generator.generate ~seed:prog_seed ~size:!size in
    if !print_programs then
      Printf.printf "----- seed %d -----\n%s\n%!" prog_seed program;
    let results =
      Differential.run_all ~backends:!backends ~state ~timeout:!timeout program
    in
    match Differential.verdict results with
    | Differential.Agree result ->
        bump (Differential.result_label result);
        Printf.printf "  seed %d: agree [%s]\n%!" prog_seed
          (Differential.result_label result)
    | Differential.Disagree results ->
        let path = save_failure prog_seed program results in
        disagreements := prog_seed :: !disagreements;
        Printf.printf "  seed %d: DISAGREE -> %s\n" prog_seed path;
        (if !auto_shrink then
           try
             let minimized, checks =
               Shrinker.shrink ~state ~backends:!backends ~timeout:!timeout
                 ~log:(fun _ -> ())
                 program
             in
             let min_path =
               Printf.sprintf "%s/seed_%d.min.mml" !out_dir prog_seed
             in
             let oc = open_out min_path in
             output_string oc minimized;
             close_out oc;
             Printf.printf "    shrunk %d -> %d chars (%d checks) -> %s\n"
               (String.length program) (String.length minimized) checks min_path
           with Invalid_argument _ ->
             (* Disagreement vanished under the reduced backend set (flaky /
                timeout-dependent) — keep the unshrunk program. *)
             ());
        List.iter
          (fun (b, r) ->
            Printf.printf "    %-8s  %s\n"
              (Differential.backend_name b)
              (let s = Differential.pp_result r in
               if String.length s > 120 then String.sub s 0 120 ^ "..." else s))
          results;
        flush stdout
  done;

  Printf.printf "\n==============================\n";
  Printf.printf "%d/%d programs agree" (!count - List.length !disagreements)
    !count;
  Printf.printf " (";
  Printf.printf "%s"
    (String.concat ", "
       (Hashtbl.fold
          (fun cat n acc -> Printf.sprintf "%s: %d" cat n :: acc)
          category_counts []));
  Printf.printf ")\n";
  if !disagreements <> [] then begin
    Printf.printf "Disagreements (replay with --seed N --count 1):\n";
    List.iter (Printf.printf "  - seed %d\n") (List.rev !disagreements);
    exit 1
  end
