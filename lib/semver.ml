(* Semantic versions for the package manager (roadmap #23). A version is
   MAJOR.MINOR.PATCH with an optional leading `v` (`v1.2.0`). Pre-release and
   build metadata are not modelled yet — minimal version selection only needs a
   total order on released versions. *)

type t = { major : int; minor : int; patch : int }

let parse (s : string) : t option =
  let s =
    if String.length s > 0 && s.[0] = 'v' then String.sub s 1 (String.length s - 1)
    else s
  in
  match String.split_on_char '.' s with
  | [ a; b; c ] -> (
      match (int_of_string_opt a, int_of_string_opt b, int_of_string_opt c) with
      | Some major, Some minor, Some patch
        when major >= 0 && minor >= 0 && patch >= 0 ->
          Some { major; minor; patch }
      | _ -> None)
  | _ -> None

(* Polymorphic [compare] (not Int.compare, which MiniML lacks); on ints it is
   the same total order, and is available in both OCaml and MiniML. *)
let compare (a : t) (b : t) : int =
  match compare a.major b.major with
  | 0 -> ( match compare a.minor b.minor with 0 -> compare a.patch b.patch | c -> c)
  | c -> c

let max (a : t) (b : t) : t = if compare a b >= 0 then a else b
let to_string (t : t) : string = Printf.sprintf "v%d.%d.%d" t.major t.minor t.patch
