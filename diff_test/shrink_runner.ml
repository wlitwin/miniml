(* Shrink runner CLI (roadmap #9) — minimize a disagreeing program and emit a
   ready-to-paste cross-test block.

   Usage:
     shrink_runner [options] prog.mml
       --backends LIST | --fast | --full    (default: all backends)
       --timeout SEC                        per-backend timeout (default 10)
       --out FILE              write the minimized program (default <prog>.min.mml)
       --test-name NAME        also print a .tests block named NAME (harvest)

   The minimized program disagrees in exactly the same way as the input (same
   dissenting backends, same result categories). The .tests block uses the
   reference backend's behavior as the expected value and carries TODO skip
   markers for each dissenting backend — assign them a tracked bug number
   before landing the test (CLAUDE.md: skip directives must reference a bug). *)

let usage () =
  prerr_endline
    "Usage: shrink_runner [--backends LIST | --fast | --full] [--timeout SEC] \
     [--out FILE] [--test-name NAME] prog.mml";
  exit 2

let () =
  let backends = ref Differential.all_backends in
  let timeout = ref 10.0 in
  let out_file = ref None in
  let test_name = ref None in
  let input = ref None in
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
    | "--full" -> backends := Differential.all_backends
    | "--timeout" when !i + 1 < Array.length argv ->
        incr i;
        timeout := float_of_string argv.(!i)
    | "--out" when !i + 1 < Array.length argv ->
        incr i;
        out_file := Some argv.(!i)
    | "--test-name" when !i + 1 < Array.length argv ->
        incr i;
        test_name := Some argv.(!i)
    | arg when String.length arg > 0 && arg.[0] = '-' -> usage ()
    | file -> input := Some file);
    incr i
  done;
  let input = match !input with Some f -> f | None -> usage () in
  let source = In_channel.with_open_text input In_channel.input_all in

  let state = Differential.make_state () in

  Printf.printf "shrinking %s (%d chars) across [%s]...\n%!" input
    (String.length source)
    (String.concat ", " (List.map Differential.backend_name !backends));

  (* Show the original disagreement *)
  let initial =
    Differential.run_all ~backends:!backends ~state ~timeout:!timeout source
  in
  (match Differential.verdict initial with
  | Differential.Agree _ ->
      Printf.printf "Program does not disagree — nothing to shrink.\n";
      exit 0
  | Differential.Disagree results ->
      List.iter
        (fun (b, r) ->
          Printf.printf "  %-8s  %s\n"
            (Differential.backend_name b)
            (let s = Differential.pp_result r in
             if String.length s > 100 then String.sub s 0 100 ^ "..." else s))
        results);

  let minimized, checks =
    Shrinker.shrink ~state ~backends:!backends ~timeout:!timeout
      ~log:(fun msg -> Printf.printf "  %s\n%!" msg)
      source
  in

  Printf.printf "\nminimized: %d chars -> %d chars (%d differential checks)\n"
    (String.length source) (String.length minimized) checks;
  Printf.printf "----------------------------------------\n%s\n" minimized;
  Printf.printf "----------------------------------------\n";

  (* Final verdict on the minimized program, for the report / harvest *)
  let final =
    Differential.run_all ~backends:!backends ~state ~timeout:!timeout minimized
  in
  List.iter
    (fun (b, r) ->
      Printf.printf "  %-8s  %s\n"
        (Differential.backend_name b)
        (Differential.pp_result r))
    final;

  (* Write the minimized program *)
  let out =
    match !out_file with
    | Some f -> f
    | None -> Filename.remove_extension input ^ ".min.mml"
  in
  Out_channel.with_open_text out (fun oc -> output_string oc minimized);
  Printf.printf "\nwrote %s\n" out;

  (* Harvest: a ready-to-paste .tests block *)
  (match !test_name with
  | None -> ()
  | Some name ->
      let reference_result = snd (List.hd final) in
      let dissenters =
        match Differential.verdict final with
        | Differential.Disagree results -> Differential.dissenters results
        | Differential.Agree _ -> []
      in
      Printf.printf "\n--- ready-to-paste .tests block ---\n\n";
      Printf.printf "--- test: %s\n%s\n" name (String.trim minimized);
      List.iter
        (fun b ->
          Printf.printf
            "--- skip-%s: BUG-? (TODO: assign the tracked bug for this \
             divergence)\n"
            (Differential.backend_name b))
        dissenters;
      (match reference_result with
      | Differential.Output s -> (
          (* Multi-line output uses the quoted \n-escape form *)
          match String.index_opt s '\n' with
          | None -> Printf.printf "--- expect: %s\n" s
          | Some _ ->
              let escaped =
                String.concat "\\n" (String.split_on_char '\n' s)
              in
              Printf.printf "--- expect: \"%s\"\n" escaped)
      | Differential.RuntimeError _ ->
          Printf.printf "--- expect-runtime-error: <TODO: substring>\n"
      | Differential.Rejected _ ->
          Printf.printf "--- expect-type-error\n"
      | r ->
          Printf.printf "(reference result is %s — not expressible as a test)\n"
            (Differential.result_label r)));

  (* Exit 0: shrinking succeeded (the program still disagrees, by design) *)
  ()
