(* The `mml.mod` project manifest (roadmap #23). Go-style and line-oriented:

     module github.com/me/myproj
     mml 0.1
     require github.com/u/lib v1.2.0
     require example.com/other v0.3.1
     replace example.com/other => ../other     # local override (dev / testing)

   A module is identified by its import path (a repository URL — decentralized,
   no central registry, as in Go); versions are semver git tags. `require` pins
   the MINIMUM acceptable version of a dependency (minimal version selection
   chooses the actual build version); `replace` redirects a module to a local
   directory, bypassing fetching. Unrecognised lines and `#` comments are
   ignored. *)

type require = { module_path : string; version : Semver.t }

type t = {
  name : string;
  mml : string option; (* toolchain version, advisory for now *)
  requires : require list;
  replaces : (string * string) list; (* module path -> local path *)
}

let parse (text : string) : t =
  let name = ref "" and mml = ref None in
  let requires = ref [] and replaces = ref [] in
  List.iter
    (fun raw ->
      (* strip a trailing `# comment` *)
      let line = match String.index_opt raw '#' with Some i -> String.sub raw 0 i | None -> raw in
      match String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "") with
      | [ "module"; n ] -> name := n
      | [ "mml"; v ] -> mml := Some v
      | [ "require"; m; v ] -> (
          match Semver.parse v with
          | Some sv -> requires := { module_path = m; version = sv } :: !requires
          | None -> ())
      | [ "replace"; m; "=>"; p ] -> replaces := (m, p) :: !replaces
      | _ -> ())
    (String.split_on_char '\n' text);
  { name = !name; mml = !mml; requires = List.rev !requires; replaces = List.rev !replaces }

let replacement (m : t) (path : string) : string option = List.assoc_opt path m.replaces
