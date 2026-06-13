(* Workspace symbol index + cross-file qualified go-to-def (Path-B cutover dev-ex:
   navigating the self-hosted compiler sources, which are heavily Module.member
   across many files). Tests Analysis.index_symbols / build_index / index_lookup /
   qualified_at directly. *)
open Test_helpers
module A = Interpreter.Analysis
module Tok = Interpreter.Token

(* index a single in-memory source and look a name up *)
let idx_of pairs = A.build_index pairs

let () =
  Printf.printf "=== Workspace Index / Cross-file Go-to-def Tests (#22) ===\n";

  (* index_symbols collects top-level names and module members (qualified) *)
  test "index_symbols: top-level + module member" (fun () ->
      let syms =
        A.index_symbols "let top = 1\nmodule Foo =\n  pub let bar = 2\n  let baz = 3\nend"
      in
      let has q = List.mem_assoc q syms in
      if not (has "top") then failwith "missing top-level 'top'";
      if not (has "Foo.bar") then failwith "missing member 'Foo.bar'";
      if not (has "Foo.baz") then failwith "missing member 'Foo.baz'";
      if not (has "Foo") then failwith "missing module name 'Foo'");

  (* member locations are precise (the name token, not the module header) *)
  test "index_symbols: member loc is the name token" (fun () ->
      let syms = A.index_symbols "module M =\n  pub let a = 1\n  pub let b = 2\nend" in
      match List.assoc_opt "M.b" syms with
      | Some (loc : Tok.loc) when loc.line = 3 -> ()
      | Some loc -> failwith (Printf.sprintf "M.b at line %d, want 3" loc.line)
      | None -> failwith "M.b not indexed");

  (* a let-in inside a member body is NOT mistaken for a member *)
  test "index_symbols: body let-in is not a member" (fun () ->
      let syms = A.index_symbols "module M =\n  pub let f = let inner = 1 in inner\nend" in
      if List.mem_assoc "M.inner" syms then failwith "body let-in leaked as a member";
      if not (List.mem_assoc "M.f" syms) then failwith "missing member M.f");

  (* build_index + index_lookup resolve a qualified name to its (file, loc) *)
  test "cross-file lookup resolves to the defining file" (fun () ->
      let idx =
        idx_of
          [
            ("types.mml", "module Types =\n  pub let repr = 1\nend");
            ("lexer.mml", "module Lexer =\n  pub let tokenize = 2\nend");
          ]
      in
      (match A.index_lookup idx "Types.repr" with
      | Some (f, (l : Tok.loc)) when Filename.basename f = "types.mml" && l.line = 2 -> ()
      | Some (f, l) -> failwith (Printf.sprintf "Types.repr -> %s:%d" f l.line)
      | None -> failwith "Types.repr not found");
      match A.index_lookup idx "Lexer.tokenize" with
      | Some (f, _) when Filename.basename f = "lexer.mml" -> ()
      | _ -> failwith "Lexer.tokenize not resolved to lexer.mml");

  (* qualified_at recognises the dotted path under the cursor *)
  test "qualified_at: cursor on the member segment" (fun () ->
      let src = "let () = Types.repr x" in
      (* cursor on 'repr' (offset of 'r' in "repr") *)
      let cursor =
        let i = ref 0 in
        (* find "repr" *)
        (try
           for k = 0 to String.length src - 4 do
             if String.sub src k 4 = "repr" then (i := k + 1; raise Exit)
           done
         with Exit -> ());
        !i
      in
      match A.qualified_at src cursor with
      | Some "Types.repr" -> ()
      | Some q -> failwith ("qualified_at got " ^ q)
      | None -> failwith "qualified_at got None");

  (* a bare (unqualified) identifier is not a qualified path *)
  test "qualified_at: bare name is None" (fun () ->
      match A.qualified_at "let foo = bar" 11 with
      | None -> ()
      | Some q -> failwith ("expected None, got " ^ q));

  print_summary ()
