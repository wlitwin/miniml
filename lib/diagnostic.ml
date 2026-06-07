(* Structured compiler diagnostics (roadmap #19).

   The compiler's three phases each raise a [(string * Token.loc)] exception
   (Lexer.Lex_error / Parser.Parse_error / Typechecker.Type_error) — a message
   and a single start point. This module is the structured, position-rich form
   the tooling consumes: a severity, a source SPAN (start..end, with byte offset
   and 1-based line/col), a stable code, and optional related-information notes.

   It is OCaml-only (not translated): the self-hosted compiler keeps raising the
   string exceptions; the reference compiler's library/LSP surface ([analysis.ml])
   converts them here. Keeping diagnostics protocol-agnostic — byte offsets plus
   compiler-convention line/col — lets the LSP map them to its own 0-based ranges
   at the boundary without this module depending on any protocol. *)

type severity = Error | Warning | Information | Hint

(* A source position: byte [offset] and the 1-based [line]/[col] it sits at (the
   compiler's convention; the LSP server converts to 0-based at its boundary). *)
type pos = { offset : int; line : int; col : int }

(* A half-open source span [lo, hi). For a single-token error the span covers
   that token; [lo = hi] is an empty (caret) span. *)
type span = { lo : pos; hi : pos }

type related = { rel_span : span; rel_message : string }

type t = {
  code : string; (* stable identifier, e.g. "parse", "type" *)
  severity : severity;
  span : span;
  message : string;
  related : related list;
}

(* The 1-based line/col of byte [offset] in [src], by a single left-to-right
   scan. Offsets past the end clamp to the end. *)
let pos_of_offset (src : string) (offset : int) : pos =
  let n = String.length src in
  let lim = if offset < 0 then 0 else if offset > n then n else offset in
  let line = ref 1 and col = ref 1 in
  for i = 0 to lim - 1 do
    if src.[i] = '\n' then begin
      incr line;
      col := 1
    end
    else incr col
  done;
  { offset = lim; line = !line; col = !col }

(* The end offset of the significant token that starts exactly at [start] in
   [src] (so a span covers the offending token, not just its first byte). Falls
   back to a one-byte span if the source doesn't lex or no token starts there. *)
let token_end (src : string) (start : int) : int =
  match Lexer.tokenize src with
  | exception _ -> start + 1
  | tokens -> (
      match
        List.find_opt (fun (t : Token.token) -> t.loc.offset = start) tokens
      with
      | Some t when t.end_offset > start -> t.end_offset
      | _ -> start + 1)

(* Build a span starting at [loc] and covering the token there. A locationless
   error ([loc.line = 0], as some type errors carry) yields an empty span at the
   start of the file — the caller still gets a well-formed diagnostic. *)
let span_at (src : string) (loc : Token.loc) : span =
  if loc.line <= 0 then
    let z = pos_of_offset src 0 in
    { lo = z; hi = z }
  else
    let lo = pos_of_offset src loc.offset in
    { lo; hi = pos_of_offset src (token_end src loc.offset) }

let make ?(severity = Error) ?(related = []) ~code ~span message =
  { code; severity; span; message; related }

let severity_name = function
  | Error -> "error"
  | Warning -> "warning"
  | Information -> "information"
  | Hint -> "hint"

(* A compact one-line rendering, for tests and CLI output:
   [code] severity L:C-L:C: message *)
let to_string (d : t) : string =
  Printf.sprintf "[%s] %s %d:%d-%d:%d: %s" d.code (severity_name d.severity)
    d.span.lo.line d.span.lo.col d.span.hi.line d.span.hi.col d.message
