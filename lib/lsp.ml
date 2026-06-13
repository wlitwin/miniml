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
  mutable root : string option; (* workspace root path (from initialize) *)
  mutable index : Analysis.sym_index option; (* lazy cross-file symbol index *)
}

(* Build a server. Loads the standard library once (expensive). *)
let create () : t =
  { docs = Hashtbl.create 16; analysis = Analysis.make_state (); root = None; index = None }

(* file://… URI <-> path. The client sends file URIs; cross-file go-to-def must
   answer with one. Minimal: strip the scheme; no percent-decoding (paths here
   are plain ASCII). *)
let path_of_uri (uri : string) : string =
  let p = if String.length uri >= 7 && String.sub uri 0 7 = "file://" then
            String.sub uri 7 (String.length uri - 7) else uri in
  p
let uri_of_path (p : string) : string = "file://" ^ p

(* Build (once, lazily) the workspace symbol index by scanning the MiniML sources
   under the root: self_host/ and stdlib/ (where the compiler + library live) and
   the root's own *.mml. Cheap to skip when there is no root. *)
let workspace_index (srv : t) : Analysis.sym_index option =
  match srv.index with
  | Some _ as i -> i
  | None -> (
      match srv.root with
      | None -> None
      | Some root ->
          let read p = try Some (In_channel.with_open_bin p In_channel.input_all) with _ -> None in
          let mml_in dir =
            let d = Filename.concat root dir in
            match Sys.readdir d with
            | exception _ -> []
            | names ->
                names |> Array.to_list
                |> List.filter (fun f -> Filename.check_suffix f ".mml")
                |> List.filter_map (fun f ->
                       let path = Filename.concat d f in
                       match read path with Some s -> Some (path, s) | None -> None)
          in
          let files = mml_in "self_host" @ mml_in "stdlib" @ mml_in "." in
          let idx = Analysis.build_index files in
          srv.index <- Some idx;
          Some idx)

(* --- Position mapping (UTF-8 bytes <-> UTF-16 code units) ---------------- *)

(* LSP positions are 0-based (line, character) and `character` counts UTF-16
   CODE UNITS; the compiler works in 0-based lines and 1-based BYTE columns.
   These coincide for ASCII but diverge once a line contains a multibyte
   character (a unicode string/comment), so map through the document text. *)

(* Line [n] (0-based) of [text], without its trailing newline. *)
let nth_line (text : string) (n : int) : string =
  match List.nth_opt (String.split_on_char '\n' text) n with Some l -> l | None -> ""

let utf8_len b = if b < 0x80 then 1 else if b < 0xE0 then 2 else if b < 0xF0 then 3 else 4
(* A code point above the BMP (4-byte UTF-8) is a UTF-16 surrogate pair = 2 units. *)
let utf16_units b = if b < 0xF0 then 1 else 2

(* Byte column (1-based) in [line] of UTF-16 code unit [u16] (0-based). *)
let utf16_to_byte_col (line : string) (u16 : int) : int =
  let n = String.length line in
  let i = ref 0 and units = ref 0 in
  while !i < n && !units < u16 do
    let b = Char.code line.[!i] in
    units := !units + utf16_units b;
    i := !i + utf8_len b
  done;
  !i + 1

(* UTF-16 code unit (0-based) in [line] at byte column [col] (1-based). *)
let byte_col_to_utf16 (line : string) (col : int) : int =
  let target = col - 1 and n = String.length line in
  let i = ref 0 and units = ref 0 in
  while !i < target && !i < n do
    let b = Char.code line.[!i] in
    units := !units + utf16_units b;
    i := !i + utf8_len b
  done;
  !units

(* The compiler 1-based (line, byte col) as an LSP 0-based (line, utf16 char),
   mapping the column through [text]. *)
let lsp_pos (text : string) (p : Diagnostic.pos) : json =
  obj
    [
      ("line", J.JInt (p.line - 1));
      ("character", J.JInt (byte_col_to_utf16 (nth_line text (p.line - 1)) p.col));
    ]

let lsp_range (text : string) (s : Diagnostic.span) : json =
  obj [ ("start", lsp_pos text s.lo); ("end", lsp_pos text s.hi) ]

let lsp_severity = function
  | Diagnostic.Error -> 1
  | Diagnostic.Warning -> 2
  | Diagnostic.Information -> 3
  | Diagnostic.Hint -> 4

let lsp_diagnostic (text : string) (d : Diagnostic.t) : json =
  obj
    [
      ("range", lsp_range text d.span);
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
    (obj
       [ ("uri", J.JString uri); ("diagnostics", arr (List.map (lsp_diagnostic text) diags)) ])

let capabilities () : json =
  obj
    [
      ( "capabilities",
        obj
          [
            ("textDocumentSync", J.JInt 1) (* full document sync *);
            ("hoverProvider", J.JBool true);
            ("definitionProvider", J.JBool true);
            ("completionProvider", obj []);
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
  | "initialize" ->
      (* capture the workspace root for cross-file go-to-def (rootUri preferred,
         rootPath as a fallback; either may be absent) *)
      (match str (field "rootUri" params) with
      | "" -> (match str (field "rootPath" params) with "" -> () | p -> srv.root <- Some p)
      | u -> srv.root <- Some (path_of_uri u));
      [ response (capabilities ()) ]
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
      let result =
        match Hashtbl.find_opt srv.docs uri with
        | Some text -> (
            let line0 = int (field "line" pos) in
            let col = utf16_to_byte_col (nth_line text line0) (int (field "character" pos)) in
            match Analysis.hover srv.analysis text ~line:(line0 + 1) ~col with
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
      let result =
        match Hashtbl.find_opt srv.docs uri with
        | Some text -> (
            let line0 = int (field "line" pos) in
            let col = utf16_to_byte_col (nth_line text line0) (int (field "character" pos)) in
            (* a Location response from (target-file-text, target-uri, 1-based loc) *)
            let location tgt_text tgt_uri (l : int) (c : int) =
              let ch = byte_col_to_utf16 (nth_line tgt_text (l - 1)) c in
              let p = obj [ ("line", J.JInt (l - 1)); ("character", J.JInt ch) ] in
              obj [ ("uri", J.JString tgt_uri); ("range", obj [ ("start", p); ("end", p) ]) ]
            in
            (* Cross-file qualified go-to-def first: if the cursor is on a
               `Module.member` path and the workspace index has it, jump there
               (possibly another file). Otherwise the same-file resolver. *)
            let cursor = Analysis.offset_of_line_col text ~line:(line0 + 1) ~col in
            let xfile =
              match (Analysis.qualified_at text cursor, workspace_index srv) with
              | Some qual, Some idx -> Analysis.index_lookup idx qual
              | _ -> None
            in
            match xfile with
            | Some (path, (loc : Token.loc)) ->
                let tgt_text =
                  if path_of_uri uri = path then text
                  else (try In_channel.with_open_bin path In_channel.input_all with _ -> "")
                in
                location tgt_text (uri_of_path path) loc.line loc.col
            | None -> (
                match Analysis.definition srv.analysis text ~line:(line0 + 1) ~col with
                | Some (l, c) -> location text uri l c
                | None -> J.JNull))
        | None -> J.JNull
      in
      [ response result ]
  | "textDocument/completion" ->
      let uri = doc_uri () in
      let items =
        match Hashtbl.find_opt srv.docs uri with
        | Some text ->
            List.map
              (fun (label, kind) ->
                obj [ ("label", J.JString label); ("kind", J.JInt kind) ])
              (Analysis.completions srv.analysis text)
        | None -> []
      in
      [ response (arr items) ]
  | _ ->
      (* Unknown request -> null result (so the client isn't left waiting);
         unknown notification -> ignore. *)
      if id = J.JNull then [] else [ response J.JNull ]

(* Whether [msg] is the request/notification that ends the session. *)
let is_exit (msg : json) : bool = str (field "method" msg) = "exit"

(* --- stdio transport ---------------------------------------------------- *)

(* Read one CRLF-terminated header line (without the CRLF); None at EOF. *)
let read_header_line ic : string option =
  let b = Buffer.create 64 in
  let rec go () =
    match input_char ic with
    | '\r' -> (
        match input_char ic with
        | '\n' -> Some (Buffer.contents b)
        | c -> Buffer.add_char b '\r'; Buffer.add_char b c; go ())
    | '\n' -> Some (Buffer.contents b) (* tolerate a bare LF *)
    | c -> Buffer.add_char b c; go ()
    | exception End_of_file ->
        if Buffer.length b = 0 then None else Some (Buffer.contents b)
  in
  go ()

(* Read one Content-Length-framed message body, or None at EOF. *)
let read_message ic : string option =
  let content_length = ref (-1) in
  let rec headers () =
    match read_header_line ic with
    | None -> None
    | Some "" -> Some () (* blank line ends the header block *)
    | Some line ->
        (match String.index_opt line ':' with
        | Some i ->
            let key = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
            let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
            if key = "content-length" then
              content_length := Option.value ~default:(-1) (int_of_string_opt v)
        | None -> ());
        headers ()
  in
  match headers () with
  | None -> None
  | Some () ->
      if !content_length < 0 then None
      else begin
        let buf = Bytes.create !content_length in
        really_input ic buf 0 !content_length;
        Some (Bytes.to_string buf)
      end

(* Run the language server over [ic]/[oc] until EOF or `exit`. Builds the server
   (loads the standard library once), then loops: read a framed message,
   dispatch it through [handle], write each reply framed. *)
let serve (ic : in_channel) (oc : out_channel) : unit =
  set_binary_mode_in ic true;
  set_binary_mode_out oc true;
  let srv = create () in
  let send (j : json) =
    let s = to_string j in
    Printf.fprintf oc "Content-Length: %d\r\n\r\n%s" (String.length s) s;
    flush oc
  in
  let rec loop () =
    match read_message ic with
    | None -> ()
    | Some body ->
        (match Deserialize.parse_json body with
        | exception _ -> () (* skip an unparseable message *)
        | msg ->
            List.iter send (handle srv msg);
            if is_exit msg then exit 0);
        loop ()
  in
  loop ()
