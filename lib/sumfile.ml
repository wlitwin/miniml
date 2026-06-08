(* The `mml.sum` checksum file (roadmap #23). One line per dependency:

     <module path> <version> h1:<hash>

   It records the content hash of each fetched module@version, like Go's go.sum:
   the first build records it (trust on first use); later builds verify the
   cached/fetched tree still matches, so a moved tag or tampered cache is caught.
   It is integrity, not version selection — MVS chooses versions from the
   manifests alone. *)

type entry = { module_path : string; version : string; hash : string }

let parse (text : string) : entry list =
  String.split_on_char '\n' text
  |> List.filter_map (fun line ->
         match String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "") with
         | [ m; v; h ] -> Some { module_path = m; version = v; hash = h }
         | _ -> None)

let to_string (entries : entry list) : string =
  entries
  |> List.sort (fun a b ->
         match String.compare a.module_path b.module_path with
         | 0 -> String.compare a.version b.version
         | c -> c)
  |> List.map (fun e -> Printf.sprintf "%s %s %s\n" e.module_path e.version e.hash)
  |> String.concat ""

let lookup (entries : entry list) (m : string) (v : string) : string option =
  List.find_map (fun e -> if e.module_path = m && e.version = v then Some e.hash else None) entries
