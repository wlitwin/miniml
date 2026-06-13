(* Dependency resolution by Minimal Version Selection (roadmap #23, Go's design).

   Each module declares the MINIMUM version of each dependency it needs. The
   build traverses the requirement graph from the main module — following the
   requirements of every (module, version) it reaches — and selects, for each
   module, the MAXIMUM of all those minimums. Deterministic and reproducible
   without a solver or lock file: the same manifests always pick the same
   versions. The trade-off (you stay on old versions until a requirement bumps
   them) is Go's, accepted deliberately.

   [mvs root load] returns the build list — one (module path, selected version)
   per dependency. [load m v] yields module [m]@[v]'s manifest so transitive
   requirements are followed; it is the only thing that needs the network/cache,
   so the algorithm here is pure and fully testable with a synthetic [load].
   Each (module, version) is visited once, so it terminates on a finite graph. *)

let mvs (root : Manifest.t) (load : string -> Semver.t -> Manifest.t) :
    (string * Semver.t) list =
  let selected : (string, Semver.t) Hashtbl.t = Hashtbl.create 16 in
  (* Key the visited-set by a single string "module@version" rather than a
     (string * string) tuple: MiniML's Hashtbl needs a Hash instance for its key
     type, and tuples have none (OCaml's Hashtbl hashes polymorphically). Module
     paths don't contain '@', so the join is unambiguous. *)
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let rec visit (m : Manifest.t) =
    List.iter
      (fun (r : Manifest.require) ->
        (match Hashtbl.find_opt selected r.module_path with
        | Some v -> Hashtbl.replace selected r.module_path (Semver.max v r.version)
        | None -> Hashtbl.replace selected r.module_path r.version);
        let key = r.module_path ^ "@" ^ Semver.to_string r.version in
        if not (Hashtbl.mem visited key) then begin
          Hashtbl.replace visited key ();
          visit (load r.module_path r.version)
        end)
      m.requires
  in
  visit root;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) selected []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
