open Test_helpers
module F = Interpreter.Fetch
module S = Interpreter.Semver

let v s = match S.parse s with Some x -> x | None -> failwith "bad version"
let sh fmt = Printf.ksprintf (fun cmd -> Sys.command (cmd ^ " >/dev/null 2>&1")) fmt

let () =
  Printf.printf "=== Fetch / cache Tests (#23) ===\n";
  let repo = Filename.temp_dir "mml_repo_" "" in
  let cache = Filename.temp_dir "mml_cache_" "" in
  Fun.protect
    ~finally:(fun () -> ignore (sh "rm -rf %s %s" repo cache))
    (fun () ->
      let wf p s = let oc = open_out p in output_string oc s; close_out oc in
      wf (Filename.concat repo "lib.mml") "pub let x = 1\n";
      (* a local git repo with a tagged release; skip the suite if git is
         unavailable rather than failing the gate on environment *)
      let ok =
        sh "git -C %s -c init.defaultBranch=main init -q" repo = 0
        && sh "git -C %s -c user.email=t@t -c user.name=t add -A" repo = 0
        && sh "git -C %s -c user.email=t@t -c user.name=t commit -q -m c" repo = 0
        && sh "git -C %s tag v1.2.0" repo = 0
      in
      if not ok then Printf.printf "  SKIP: git not usable here\n"
      else begin
        Unix.putenv "MML_CACHE" cache;

        test "ensure fetches a module@version into the cache" (fun () ->
            let dir = F.ensure repo (v "v1.2.0") in
            if not (Sys.file_exists (Filename.concat dir "lib.mml")) then
              failwith "fetched tree missing lib.mml";
            if Sys.file_exists (Filename.concat dir ".git") then
              failwith ".git should be stripped from the cache");

        test "ensure is idempotent on a cache hit" (fun () ->
            let a = F.ensure repo (v "v1.2.0") in
            let b = F.ensure repo (v "v1.2.0") in
            if a <> b then failwith "cache path changed between calls");

        test "fetching a nonexistent version fails cleanly" (fun () ->
            match F.ensure repo (v "v9.9.9") with
            | _ -> failwith "expected a fetch failure"
            | exception F.Fetch_error _ -> ())
      end);
  print_summary ()
