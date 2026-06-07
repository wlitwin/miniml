(* Language Server Protocol server (roadmap #22), built on the analysis library.

   This module is the PURE protocol layer: [handle] takes one decoded JSON-RPC
   message and the server state and returns the JSON-RPC messages to send back
   (a response and/or notifications). Keeping it free of I/O makes it unit
   testable; the executable (bin/) is just a stdio framing loop around it.

   Reuses the compiler's own JSON value type and parser (Deserialize); this file
   adds the printer the parser lacked. OCaml-only — not part of the self-host. *)

module J = Deserialize

type json = J.json

(* --- JSON printing ------------------------------------------------------ *)

let escape (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let rec to_string (j : json) : string =
  match j with
  | J.JNull -> "null"
  | J.JBool b -> if b then "true" else "false"
  | J.JInt n -> string_of_int n
  | J.JFloat f -> Printf.sprintf "%.17g" f
  | J.JString s -> escape s
  | J.JArray xs -> "[" ^ String.concat "," (List.map to_string xs) ^ "]"
  | J.JObject fs ->
      "{"
      ^ String.concat ","
          (List.map (fun (k, v) -> escape k ^ ":" ^ to_string v) fs)
      ^ "}"

(* --- Convenience over the json accessors -------------------------------- *)

let field = J.json_field
let str = J.json_string
let int = J.json_int
let obj fs = J.JObject fs
let arr xs = J.JArray xs

(* --- Server state ------------------------------------------------------- *)

type t = {
  docs : (string, string) Hashtbl.t; (* open document uri -> full text *)
  analysis : Analysis.state;
}

(* Build a server. Loads the standard library once (expensive). *)
let create () : t = { docs = Hashtbl.create 16; analysis = Analysis.make_state () }

(* --- LSP <-> Diagnostic conversion -------------------------------------- *)

(* LSP positions are 0-based (line, character); the compiler is 1-based
   (line, col). Bytes vs UTF-16 code units coincide for ASCII (the common
   case); multibyte columns are a later refinement. *)
let lsp_pos (p : Diagnostic.pos) : json =
  obj [ ("line", J.JInt (p.line - 1)); ("character", J.JInt (p.col - 1)) ]

let lsp_range (s : Diagnostic.span) : json =
  obj [ ("start", lsp_pos s.lo); ("end", lsp_pos s.hi) ]

let lsp_severity = function
  | Diagnostic.Error -> 1
  | Diagnostic.Warning -> 2
  | Diagnostic.Information -> 3
  | Diagnostic.Hint -> 4

let lsp_diagnostic (d : Diagnostic.t) : json =
  obj
    [
      ("range", lsp_range d.span);
      ("severity", J.JInt (lsp_severity d.severity));
      ("code", J.JString d.code);
      ("source", J.JString "miniml");
      ("message", J.JString d.message);
    ]

(* --- Messages ----------------------------------------------------------- *)

let notification m params =
  obj [ ("jsonrpc", J.JString "2.0"); ("method", J.JString m); ("params", params) ]

let publish (srv : t) (uri : string) : json =
  let text = match Hashtbl.find_opt srv.docs uri with Some t -> t | None -> "" in
  let diags = Analysis.diagnostics srv.analysis text in
  notification "textDocument/publishDiagnostics"
    (obj [ ("uri", J.JString uri); ("diagnostics", arr (List.map lsp_diagnostic diags)) ])

let capabilities () : json =
  obj
    [
      ( "capabilities",
        obj
          [
            ("textDocumentSync", J.JInt 1) (* full document sync *);
            ("hoverProvider", J.JBool true);
            ("definitionProvider", J.JBool true);
          ] );
      ("serverInfo", obj [ ("name", J.JString "miniml-lsp") ]);
    ]

(* Handle one decoded JSON-RPC message; return the messages to send back. A
   request (it has an [id]) always gets exactly one response; notifications may
   produce notifications (e.g. publishDiagnostics) or nothing. *)
let handle (srv : t) (msg : json) : json list =
  let meth = str (field "method" msg) in
  let id = field "id" msg in
  let params = field "params" msg in
  let response result =
    obj [ ("jsonrpc", J.JString "2.0"); ("id", id); ("result", result) ]
  in
  let doc_uri () = str (field "uri" (field "textDocument" params)) in
  match meth with
  | "initialize" -> [ response (capabilities ()) ]
  | "initialized" | "exit" -> []
  | "shutdown" -> [ response J.JNull ]
  | "textDocument/didOpen" ->
      let uri = doc_uri () in
      Hashtbl.replace srv.docs uri (str (field "text" (field "textDocument" params)));
      [ publish srv uri ]
  | "textDocument/didChange" ->
      let uri = doc_uri () in
      (* full sync: the last content change carries the whole new text *)
      let text =
        match List.rev (J.json_list (field "contentChanges" params)) with
        | last :: _ -> str (field "text" last)
        | [] -> (match Hashtbl.find_opt srv.docs uri with Some t -> t | None -> "")
      in
      Hashtbl.replace srv.docs uri text;
      [ publish srv uri ]
  | "textDocument/didClose" ->
      let uri = doc_uri () in
      Hashtbl.remove srv.docs uri;
      (* clear diagnostics for the closed file *)
      [ notification "textDocument/publishDiagnostics"
          (obj [ ("uri", J.JString uri); ("diagnostics", arr []) ]) ]
  | "textDocument/hover" ->
      let uri = doc_uri () in
      let pos = field "position" params in
      let line = int (field "line" pos) + 1 and col = int (field "character" pos) + 1 in
      let result =
        match Hashtbl.find_opt srv.docs uri with
        | Some text -> (
            match Analysis.hover srv.analysis text ~line ~col with
            | Some ty ->
                obj
                  [
                    ( "contents",
                      obj [ ("kind", J.JString "plaintext"); ("value", J.JString ty) ] );
                  ]
            | None -> J.JNull)
        | None -> J.JNull
      in
      [ response result ]
  | "textDocument/definition" ->
      let uri = doc_uri () in
      let pos = field "position" params in
      let line = int (field "line" pos) + 1 and col = int (field "character" pos) + 1 in
      let result =
        match Hashtbl.find_opt srv.docs uri with
        | Some text -> (
            match Analysis.definition srv.analysis text ~line ~col with
            | Some (l, c) ->
                let p = obj [ ("line", J.JInt (l - 1)); ("character", J.JInt (c - 1)) ] in
                obj [ ("uri", J.JString uri); ("range", obj [ ("start", p); ("end", p) ]) ]
            | None -> J.JNull)
        | None -> J.JNull
      in
      [ response result ]
  | _ ->
      (* Unknown request -> null result (so the client isn't left waiting);
         unknown notification -> ignore. *)
      if id = J.JNull then [] else [ response J.JNull ]

(* Whether [msg] is the request/notification that ends the session. *)
let is_exit (msg : json) : bool = str (field "method" msg) = "exit"
