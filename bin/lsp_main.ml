(* MiniML language server (roadmap #22): a stdio JSON-RPC framing loop around
   the pure protocol handler in Interpreter.Lsp. Reads `Content-Length`-framed
   messages from stdin, dispatches each to Lsp.handle, and writes the framed
   responses/notifications to stdout. All the logic lives in the library; this
   file is just the transport. *)

module Lsp = Interpreter.Lsp
module Deserialize = Interpreter.Deserialize

(* Read one CRLF-terminated header line (without the CRLF). None at EOF. *)
let read_header_line ic : string option =
  let b = Buffer.create 64 in
  let rec go () =
    match input_char ic with
    | '\r' -> ( match input_char ic with '\n' -> Some (Buffer.contents b) | c -> Buffer.add_char b '\r'; Buffer.add_char b c; go ())
    | '\n' -> Some (Buffer.contents b) (* tolerate bare LF *)
    | c -> Buffer.add_char b c; go ()
    | exception End_of_file -> if Buffer.length b = 0 then None else Some (Buffer.contents b)
  in
  go ()

(* Read one framed message body, or None at EOF. *)
let read_message ic : string option =
  let content_length = ref (-1) in
  let rec headers () =
    match read_header_line ic with
    | None -> None (* EOF before any header *)
    | Some "" -> Some () (* blank line ends the header block *)
    | Some line ->
        (match String.index_opt line ':' with
        | Some i ->
            let key = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
            let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
            if key = "content-length" then content_length := int_of_string_opt v |> Option.value ~default:(-1)
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

let () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  let srv = Lsp.create () in
  let send (j : Lsp.json) =
    let s = Lsp.to_string j in
    Printf.printf "Content-Length: %d\r\n\r\n%s" (String.length s) s;
    flush stdout
  in
  let rec loop () =
    match read_message stdin with
    | None -> ()
    | Some body ->
        (match Deserialize.parse_json body with
        | exception _ -> () (* skip an unparseable message *)
        | msg ->
            List.iter send (Lsp.handle srv msg);
            if Lsp.is_exit msg then exit 0);
        loop ()
  in
  loop ()
