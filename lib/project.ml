(* Multi-file project model + build (roadmap #23): the local module/build system
   the package manager sits on. A project is a directory with an `mml.mod`
   manifest and one or more `.mml` files. Each non-entry file is a MODULE — the
   file `foo.mml` becomes `module Foo`, its `pub` declarations its exports — and
   the entry `main.mml` is the program that uses them via the existing qualified
   syntax (`Foo.bar`, `open Foo`). No new language construct: a file is compiled
   by wrapping it as `module Foo = ... end`, reusing MiniML's in-file module
   system (which already does interface extraction and encapsulation).

   This OCaml-only build driver discovers the files, infers each module's
   dependencies from its references, orders them topologically, and produces the
   combined source the compiler builds. Per-module caching (incremental builds)
   and true separate codegen + linking are later increments toward full separate
   compilation; the module boundaries and interfaces established here are the
   foundation for both. *)

exception Build_error of string

type unit_ = { name : string; path : string; source : string }
type t = { name : string; entry : unit_; libs : unit_ list }

let read_file path =
  let ic = open_in_bin path in
  let s = In_channel.input_all ic in
  close_in ic;
  s

(* `foo.mml` -> module `Foo`; `string_utils.mml` -> `String_utils`. *)
let module_name_of (path : string) : string =
  String.capitalize_ascii (Filename.remove_extension (Filename.basename path))

(* The library modules of a directory (every `.mml` but `main.mml`). *)
let dir_units (dir : string) : unit_ list =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".mml" && f <> "main.mml")
  |> List.sort String.compare
  |> List.map (fun f ->
         let path = Filename.concat dir f in
         { name = module_name_of path; path; source = read_file path })

(* Resolve [m] to a local directory: the root manifest's `replace` (relative to
   the project root) for now. Remote fetch into a module cache is a later
   increment, so an un-replaced dependency is an error. *)
let locate (root : string) (mf : Manifest.t) (m : string) : string =
  match Manifest.replacement mf m with
  | Some p -> if Filename.is_relative p then Filename.concat root p else p
  | None ->
      raise
        (Build_error
           (Printf.sprintf
              "dependency %s is not available: add `replace %s => <local path>` \
               to mml.mod (remote fetch is not implemented yet)"
              m m))

(* Which project modules does [u] reference — `Name.` (qualified access) or
   `open Name`? Inferred from the token stream so it tolerates a not-yet-valid
   file. A bare `Name` (e.g. a constructor) is ignored to avoid false edges. *)
let deps_of (modules : string list) (u : unit_) : string list =
  match Lexer.tokenize u.source with
  | exception _ -> []
  | toks ->
      let arr = Array.of_list toks in
      let found = Hashtbl.create 16 in
      Array.iteri
        (fun i (t : Token.token) ->
          match t.kind with
          | Token.UIDENT name when name <> u.name && List.mem name modules ->
              let next_dot =
                i + 1 < Array.length arr && arr.(i + 1).Token.kind = Token.DOT
              in
              let prev_open = i > 0 && arr.(i - 1).Token.kind = Token.OPEN in
              if next_dot || prev_open then Hashtbl.replace found name ()
          | _ -> ())
        arr;
      Hashtbl.fold (fun k () acc -> k :: acc) found []

(* Topologically order the library modules so each precedes its dependents.
   Raises [Build_error] on a dependency cycle. *)
let topo_order (libs : unit_ list) : unit_ list =
  let names = List.map (fun (u : unit_) -> u.name) libs in
  let by_name = List.map (fun (u : unit_) -> (u.name, u)) libs in
  let dep_map = List.map (fun (u : unit_) -> (u.name, deps_of names u)) libs in
  let visited = Hashtbl.create 32 and on_stack = Hashtbl.create 32 in
  let order = ref [] in
  let rec visit name =
    if Hashtbl.mem visited name then ()
    else if Hashtbl.mem on_stack name then
      raise (Build_error (Printf.sprintf "module dependency cycle through %s" name))
    else begin
      Hashtbl.replace on_stack name ();
      List.iter visit (try List.assoc name dep_map with Not_found -> []);
      Hashtbl.remove on_stack name;
      Hashtbl.replace visited name ();
      order := List.assoc name by_name :: !order
    end
  in
  List.iter (fun (u : unit_) -> visit u.name) libs;
  List.rev !order

(* Load the project rooted at [dir]: parse its manifest, resolve its
   dependencies by minimal version selection, and gather every module — the
   project's own `.mml` files (with `main.mml` as the entry) plus each resolved
   dependency's library modules — into one set. A dependency contributes its
   modules to the build flat (referenced as `Foo.bar`), so a module-name
   collision is an error. *)
let load (dir : string) : t =
  let manifest_path = Filename.concat dir "mml.mod" in
  if not (Sys.file_exists manifest_path) then
    raise (Build_error (Printf.sprintf "no mml.mod manifest in %s" dir));
  let mf = Manifest.parse (read_file manifest_path) in
  if mf.Manifest.name = "" then
    raise (Build_error "mml.mod: missing a `module <name>` line");
  (* MVS: read each module@version's manifest from its resolved directory. *)
  let load_manifest m _v =
    let d = locate dir mf m in
    let p = Filename.concat d "mml.mod" in
    if Sys.file_exists p then Manifest.parse (read_file p)
    else { Manifest.name = m; mml = None; requires = []; replaces = [] }
  in
  let dep_units =
    Deps.mvs mf load_manifest
    |> List.concat_map (fun (m, _v) -> dir_units (locate dir mf m))
  in
  let own =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".mml")
    |> List.sort String.compare
    |> List.map (fun f ->
           let path = Filename.concat dir f in
           { name = module_name_of path; path; source = read_file path })
  in
  let entry, own_libs = List.partition (fun u -> Filename.basename u.path = "main.mml") own in
  let libs = dep_units @ own_libs in
  (* reject two modules with the same name (a dependency clashing with the
     project or another dependency) *)
  let seen = Hashtbl.create 32 in
  List.iter
    (fun (u : unit_) ->
      match Hashtbl.find_opt seen u.name with
      | Some other ->
          raise (Build_error (Printf.sprintf "module name %s defined by both %s and %s" u.name other u.path))
      | None -> Hashtbl.replace seen u.name u.path)
    libs;
  match entry with
  | [ e ] -> { name = mf.Manifest.name; entry = e; libs }
  | [] -> raise (Build_error "no entry point (expected main.mml)")
  | _ -> raise (Build_error "more than one main.mml")

(* The single source the compiler builds — each library wrapped as its module in
   dependency order, then the entry — together with a LINE MAP: a list of
   (combined-line, file) pairs marking where each unit's source begins, so an
   error reported in combined-source coordinates can be attributed back to the
   originating file and line. *)
let combined_with_map (p : t) : string * (int * string) list =
  let buf = Buffer.create 4096 in
  let line = ref 1 in
  let add s =
    Buffer.add_string buf s;
    String.iter (fun c -> if c = '\n' then incr line) s
  in
  let segs = ref [] in
  let emit ~wrap (u : unit_) =
    if wrap then add (Printf.sprintf "module %s =\n" u.name);
    (* the unit's source begins here, mapping its line 1 to combined line [!line] *)
    segs := (!line, u.path) :: !segs;
    add u.source;
    if not (String.length u.source > 0 && u.source.[String.length u.source - 1] = '\n')
    then add "\n";
    if wrap then add "end;;\n"
  in
  List.iter (emit ~wrap:true) (topo_order p.libs);
  emit ~wrap:false p.entry;
  (Buffer.contents buf, List.rev !segs)

let combined_source (p : t) : string = fst (combined_with_map p)

(* Map a 1-based line in the combined source back to (file, 1-based line). *)
let map_line (segs : (int * string) list) (l : int) : string * int =
  let rec go best = function
    | (start, file) :: rest -> go (if start <= l then Some (start, file) else best) rest
    | [] -> best
  in
  match go None segs with
  | Some (start, file) -> (file, l - start + 1)
  | None -> ( match segs with (_, f) :: _ -> (f, l) | [] -> ("?", l))

(* Whether [path] is a project directory (has an mml.mod). *)
let is_project (path : string) : bool =
  (try Sys.is_directory path with _ -> false)
  && Sys.file_exists (Filename.concat path "mml.mod")
