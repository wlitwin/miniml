(* Fetching dependencies into a local module cache (roadmap #23). A module's
   import path IS its git URL (decentralized, no registry, as in Go) and a
   version IS a git tag (`v1.2.0`). Fetched modules are cached under
   $MML_CACHE (default ~/.mml/cache) keyed by path and version, so each
   module@version is downloaded at most once and builds are offline thereafter.

   No code runs at fetch time — only `git clone`; the fetched tree is data. *)

exception Fetch_error of string

let cache_root () : string =
  match Sys.getenv_opt "MML_CACHE" with
  | Some d -> d
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some h -> Filename.concat (Filename.concat h ".mml") "cache"
      | None -> Filename.concat (Filename.get_temp_dir_name ()) "mml-cache")

(* Where module@version lives in the cache: <cache>/<import/path>/<version>. *)
let cache_path (module_path : string) (version : Semver.t) : string =
  Filename.concat (Filename.concat (cache_root ()) module_path) (Semver.to_string version)

let rec mkdir_p (dir : string) : unit =
  if dir <> "" && dir <> "/" && dir <> "." && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

(* The git source for an import path: an existing local path is used directly
   (a local repo — handy for testing/dev); anything else is `https://<path>`. *)
let git_url (module_path : string) : string =
  if Sys.file_exists module_path then module_path else "https://" ^ module_path

let rm_rf path = ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote path)))

(* Ensure module@version is in the cache (fetching it if absent) and return its
   directory. Clones the tag shallowly, drops the `.git` directory to keep the
   cache lean, and moves it into place atomically. *)
let ensure (module_path : string) (version : Semver.t) : string =
  let dest = cache_path module_path version in
  if Sys.file_exists dest then dest
  else begin
    mkdir_p (Filename.dirname dest);
    let tmp = dest ^ ".tmp" in
    rm_rf tmp;
    let cmd =
      Printf.sprintf "git clone --quiet --depth 1 --branch %s -- %s %s >/dev/null 2>&1"
        (Filename.quote (Semver.to_string version))
        (Filename.quote (git_url module_path))
        (Filename.quote tmp)
    in
    if Sys.command cmd <> 0 then begin
      rm_rf tmp;
      raise
        (Fetch_error
           (Printf.sprintf "could not fetch %s@%s (git clone of %s failed)" module_path
              (Semver.to_string version) (git_url module_path)))
    end;
    rm_rf (Filename.concat tmp ".git");
    (try Sys.rename tmp dest with _ -> rm_rf tmp);
    dest
  end

(* A content hash of a fetched module tree, for mml.sum integrity. Every file's
   relative path and bytes are folded in, in sorted order, so it is
   deterministic and independent of read order. `h1:` tags the scheme (MD5 via
   the stdlib's Digest — no crypto dependency; strengthen to SHA-256 later). *)
let tree_hash (dir : string) : string =
  let rec files prefix =
    Sys.readdir (if prefix = "" then dir else Filename.concat dir prefix)
    |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun name ->
           let rel = if prefix = "" then name else Filename.concat prefix name in
           if (try Sys.is_directory (Filename.concat dir rel) with _ -> false) then files rel
           else [ rel ])
  in
  let buf = Buffer.create 4096 in
  List.iter
    (fun rel ->
      Buffer.add_string buf rel;
      Buffer.add_char buf '\n';
      let ic = open_in_bin (Filename.concat dir rel) in
      Buffer.add_string buf (In_channel.input_all ic);
      close_in ic;
      Buffer.add_char buf '\000')
    (List.sort String.compare (files ""));
  "h1:" ^ Digest.to_hex (Digest.string (Buffer.contents buf))

(* The greatest released (semver-tagged) version of a module, via
   `git ls-remote --tags` — for `mml get <url>` with no explicit version. *)
let latest_version (module_path : string) : Semver.t option =
  let ic =
    Unix.open_process_in
      (Printf.sprintf "git ls-remote --tags -- %s 2>/dev/null"
         (Filename.quote (git_url module_path)))
  in
  let best = ref None in
  (try
     while true do
       let line = input_line ic in
       match String.rindex_opt line '/' with
       | Some i ->
           let tag = String.sub line (i + 1) (String.length line - i - 1) in
           let tag =
             if Filename.check_suffix tag "^{}" then String.sub tag 0 (String.length tag - 3)
             else tag
           in
           (match Semver.parse tag with
           | Some v -> best := Some (match !best with Some b -> Semver.max b v | None -> v)
           | None -> ())
       | None -> ()
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  !best
