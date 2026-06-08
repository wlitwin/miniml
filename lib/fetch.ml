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
