(* Fetching dependencies into a local module cache (roadmap #23). A module's
   import path IS its git URL (decentralized, no registry, as in Go) and a
   version IS a git tag (`v1.2.0`). Fetched modules are cached under
   $MML_CACHE (default ~/.mml/cache) keyed by path and version, so each
   module@version is downloaded at most once and builds are offline thereafter.

   No code runs at fetch time — only `git clone`; the fetched tree is data.

   Written against the compiler-agnostic system-access surface (Path / Fs /
   Process / IO / Digest — the OCaml twins of the MiniML builtins) so it can be
   mechanically translated into the in-MiniML toolchain (Path B, roadmap #16):
   subprocesses run with an explicit argv via Process.run (no shell, no
   quoting), file IO goes through IO/Fs, paths through Path. *)

exception Fetch_error of string

let temp_dir () : string =
  match Sys.getenv_opt "TMPDIR" with Some d -> d | None -> "/tmp"

let cache_root () : string =
  match Sys.getenv_opt "MML_CACHE" with
  | Some d -> d
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some h -> Path.join (Path.join h ".mml") "cache"
      | None -> Path.join (temp_dir ()) "mml-cache")

(* Where module@version lives in the cache: <cache>/<import/path>/<version>. *)
let cache_path (module_path : string) (version : Semver.t) : string =
  Path.join (Path.join (cache_root ()) module_path) (Semver.to_string version)

let rec mkdir_p (dir : string) : unit =
  if dir <> "" && dir <> "/" && dir <> "." && not (IO.file_exists dir) then begin
    mkdir_p (Path.dirname dir);
    Fs.make_dir dir
  end

(* The git source for an import path: an existing local path is used directly
   (a local repo — handy for testing/dev); anything else is `https://<path>`. *)
let git_url (module_path : string) : string =
  if IO.file_exists module_path then module_path else "https://" ^ module_path

(* Recursive remove via `rm -rf` (no shell — argv-based, so no quoting). *)
let rm_rf (path : string) : unit = ignore (Process.run "rm" [ "-rf"; path ])

(* Ensure module@version is in the cache (fetching it if absent) and return its
   directory. Clones the tag shallowly, drops the `.git` directory to keep the
   cache lean, and moves it into place atomically. *)
let ensure (module_path : string) (version : Semver.t) : string =
  let dest = cache_path module_path version in
  if IO.file_exists dest then dest
  else begin
    mkdir_p (Path.dirname dest);
    let tmp = dest ^ ".tmp" in
    rm_rf tmp;
    let code, _out, _err =
      Process.run "git"
        [
          "clone"; "--quiet"; "--depth"; "1"; "--branch";
          Semver.to_string version; "--"; git_url module_path; tmp;
        ]
    in
    if code <> 0 then begin
      rm_rf tmp;
      raise
        (Fetch_error
           (Printf.sprintf "could not fetch %s@%s (git clone of %s failed)"
              module_path (Semver.to_string version) (git_url module_path)))
    end;
    rm_rf (Path.join tmp ".git");
    Fs.rename tmp dest;
    dest
  end

(* A content hash of a fetched module tree, for mml.sum integrity. Every file's
   relative path and bytes are folded in, in sorted order, so it is
   deterministic and independent of read order. `h1:` tags the scheme (MD5 via
   Digest — no crypto dependency; strengthen to SHA-256 later). *)
let tree_hash (dir : string) : string =
  let rec files prefix =
    Fs.read_dir (if prefix = "" then dir else Path.join dir prefix)
    |> List.sort String.compare
    |> List.concat_map (fun name ->
           let rel = if prefix = "" then name else Path.join prefix name in
           if Fs.is_directory (Path.join dir rel) then files rel else [ rel ])
  in
  let buf = Buffer.create 4096 in
  List.iter
    (fun rel ->
      Buffer.add_string buf rel;
      Buffer.add_char buf '\n';
      Buffer.add_string buf (IO.read_file (Path.join dir rel));
      Buffer.add_char buf '\000')
    (List.sort String.compare (files ""));
  "h1:" ^ Digest.md5 (Buffer.contents buf)

(* True when [s] ends with [suf] (the in-subset replacement for
   Filename.check_suffix). *)
let has_suffix (s : string) (suf : string) : bool =
  let ls = String.length s and lf = String.length suf in
  ls >= lf && String.sub s (ls - lf) lf = suf

(* The greatest released (semver-tagged) version of a module, via
   `git ls-remote --tags` — for `mml get <url>` with no explicit version. *)
let latest_version (module_path : string) : Semver.t option =
  let _code, out, _err =
    Process.run "git" [ "ls-remote"; "--tags"; "--"; git_url module_path ]
  in
  let best = ref None in
  List.iter
    (fun line ->
      match String.rindex_opt line '/' with
      | Some i ->
          let tag = String.sub line (i + 1) (String.length line - i - 1) in
          let tag =
            if has_suffix tag "^{}" then String.sub tag 0 (String.length tag - 3)
            else tag
          in
          (match Semver.parse tag with
          | Some v ->
              best := Some (match !best with Some b -> Semver.max b v | None -> v)
          | None -> ())
      | None -> ())
    (String.split_on_char '\n' out);
  !best
