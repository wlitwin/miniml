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

(* Minimal manifest: a `module <name>` line names the project. Other lines
   (toolchain version, future `require`s) are ignored for now. *)
let parse_manifest (text : string) : string option =
  let name = ref None in
  List.iter
    (fun line ->
      match String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "") with
      | "module" :: n :: _ -> name := Some n
      | _ -> ())
    (String.split_on_char '\n' text);
  !name

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

(* Load the project rooted at [dir]: read its manifest and `.mml` files, with
   `main.mml` as the entry program and the rest as library modules. *)
let load (dir : string) : t =
  let manifest = Filename.concat dir "mml.mod" in
  if not (Sys.file_exists manifest) then
    raise (Build_error (Printf.sprintf "no mml.mod manifest in %s" dir));
  let name =
    match parse_manifest (read_file manifest) with
    | Some n -> n
    | None -> raise (Build_error "mml.mod: missing a `module <name>` line")
  in
  let units =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".mml")
    |> List.sort String.compare
    |> List.map (fun f ->
           let path = Filename.concat dir f in
           { name = module_name_of path; path; source = read_file path })
  in
  let entry, libs = List.partition (fun u -> Filename.basename u.path = "main.mml") units in
  match entry with
  | [ e ] -> { name; entry = e; libs }
  | [] -> raise (Build_error "no entry point (expected main.mml)")
  | _ -> raise (Build_error "more than one main.mml")

(* The single source the compiler builds: each library wrapped as its module, in
   dependency order, followed by the entry program. *)
let combined_source (p : t) : string =
  let buf = Buffer.create 4096 in
  List.iter
    (fun (u : unit_) ->
      Buffer.add_string buf (Printf.sprintf "module %s =\n" u.name);
      Buffer.add_string buf u.source;
      Buffer.add_string buf "\nend;;\n")
    (topo_order p.libs);
  Buffer.add_string buf p.entry.source;
  Buffer.add_char buf '\n';
  Buffer.contents buf

(* Whether [path] is a project directory (has an mml.mod). *)
let is_project (path : string) : bool =
  (try Sys.is_directory path with _ -> false)
  && Sys.file_exists (Filename.concat path "mml.mod")
