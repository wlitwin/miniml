exception Lex_error of string * Token.loc

type t = {
  source: string;
  len: int;
  mutable cursor: int;
  mutable line: int;
  mutable col: int;
}

let create source =
  { source; len = String.length source; cursor = 0; line = 1; col = 1 }

let current_loc (l : t) : Token.loc =
  { line = l.line; col = l.col; offset = l.cursor }

let make_token l kind loc =
  Token.{ kind; loc; end_offset = l.cursor }

let at_end l = l.cursor >= l.len

let peek l =
  if at_end l then '\000'
  else l.source.[l.cursor]

let peek2 l =
  if l.cursor + 1 >= l.len then '\000'
  else l.source.[l.cursor + 1]

let peek3 l =
  if l.cursor + 2 >= l.len then '\000'
  else l.source.[l.cursor + 2]

let advance l =
  let c = l.source.[l.cursor] in
  l.cursor <- l.cursor + 1;
  if c = '\n' then begin
    l.line <- l.line + 1;
    l.col <- 1
  end else
    l.col <- l.col + 1;
  c

let error l msg =
  raise (Lex_error (msg, current_loc l))

let skip_whitespace l =
  while not (at_end l) && (peek l = ' ' || peek l = '\t' || peek l = '\n' || peek l = '\r') do
    ignore (advance l)
  done

let skip_comment l =
  (* Already consumed '(' '*' *)
  let depth = ref 1 in
  while !depth > 0 do
    if at_end l then error l "unterminated comment";
    if peek l = '(' && peek2 l = '*' then begin
      ignore (advance l); ignore (advance l);
      depth := !depth + 1
    end else if peek l = '*' && peek2 l = ')' then begin
      ignore (advance l); ignore (advance l);
      depth := !depth - 1
    end else
      ignore (advance l)
  done

let skip_whitespace_and_comments l =
  let changed = ref true in
  while !changed do
    changed := false;
    let p = l.cursor in
    skip_whitespace l;
    if not (at_end l) && peek l = '(' && peek2 l = '*' && peek3 l <> ')' then begin
      ignore (advance l); ignore (advance l);
      skip_comment l;
      changed := true
    end;
    if l.cursor <> p then changed := true
  done

let is_digit c = c >= '0' && c <= '9'
let is_hex_digit c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let hex_val c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else Char.code c - Char.code 'A' + 10
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_ident_start c = is_alpha c || c = '_'
let is_ident_char c = is_alpha c || is_digit c || c = '_' || c = '\''

let keywords = [
  "let", Token.LET;
  "rec", Token.REC;
  "in", Token.IN;
  "if", Token.IF;
  "else", Token.ELSE;
  "fn", Token.FN;
  "match", Token.MATCH;
  "with", Token.WITH;
  "type", Token.TYPE;
  "newtype", Token.NEWTYPE;
  "of", Token.OF;
  "not", Token.NOT;
  "mod", Token.MOD;
  "true", Token.TRUE;
  "false", Token.FALSE;
  "class", Token.CLASS;
  "instance", Token.INSTANCE;
  "effect", Token.EFFECT;
  "extern", Token.EXTERN;
  "perform", Token.PERFORM;
  "handle", Token.HANDLE;
  "try", Token.TRY;
  "provide", Token.PROVIDE;
  "resume", Token.RESUME;
  "return", Token.RETURN;
  "continue", Token.CONTINUE;
  "mut", Token.MUT;
  "for", Token.FOR;
  "do", Token.DO;
  "break", Token.BREAK;
  "when", Token.WHEN;
  "where", Token.WHERE;
  "module", Token.MODULE;
  "pub", Token.PUB;
  "open", Token.OPEN;
  "end", Token.END;
  "opaque", Token.OPAQUE;
  "and", Token.AND;
  "deriving", Token.DERIVING;
  "land", Token.LAND;
  "lor", Token.LOR;
  "lxor", Token.LXOR;
  "lnot", Token.LNOT;
  "lsl", Token.LSL;
  "lsr", Token.LSR;
]

let read_number l =
  let loc = current_loc l in
  let buf = Buffer.create 16 in
  let c = advance l in
  Buffer.add_char buf c;
  if c = '0' && not (at_end l) && (peek l = 'x' || peek l = 'X') then begin
    Buffer.add_char buf (advance l);
    while not (at_end l) && is_hex_digit (peek l) do
      Buffer.add_char buf (advance l)
    done;
    let n = int_of_string (Buffer.contents buf) in
    make_token l (INT n) loc
  end else if c = '0' && not (at_end l) && (peek l = 'b' || peek l = 'B') then begin
    Buffer.add_char buf (advance l);
    while not (at_end l) && (peek l = '0' || peek l = '1') do
      Buffer.add_char buf (advance l)
    done;
    let n = int_of_string (Buffer.contents buf) in
    make_token l (INT n) loc
  end else begin
    while not (at_end l) && is_digit (peek l) do
      Buffer.add_char buf (advance l)
    done;
    if not (at_end l) && peek l = '.' then begin
      Buffer.add_char buf (advance l);
      while not (at_end l) && is_digit (peek l) do
        Buffer.add_char buf (advance l)
      done;
      let f = float_of_string (Buffer.contents buf) in
      make_token l (FLOAT f) loc
    end else begin
      let n = int_of_string (Buffer.contents buf) in
      make_token l (INT n) loc
    end
  end

let read_string l =
  let loc = current_loc l in
  ignore (advance l); (* consume opening '"' *)
  let buf = Buffer.create 64 in
  while not (at_end l) && peek l <> '"' do
    if peek l = '\\' then begin
      ignore (advance l);
      if at_end l then error l "unterminated string";
      let c = advance l in
      match c with
      | 'n' -> Buffer.add_char buf '\n'
      | 't' -> Buffer.add_char buf '\t'
      | '\\' -> Buffer.add_char buf '\\'
      | '"' -> Buffer.add_char buf '"'
      | _ -> error l (Printf.sprintf "unknown escape: \\%c" c)
    end else
      Buffer.add_char buf (advance l)
  done;
  if at_end l then error l "unterminated string";
  ignore (advance l); (* consume closing '"' *)
  make_token l (STRING (Buffer.contents buf)) loc

let dedent_raw s =
  let lines = String.split_on_char '\n' s in
  (* Strip first line if blank *)
  let lines = match lines with
    | first :: rest when String.trim first = "" -> rest
    | _ -> lines
  in
  (* Strip last line if blank *)
  let lines = match List.rev lines with
    | last :: rest when String.trim last = "" -> List.rev rest
    | _ -> lines
  in
  (* Find minimum indent across all non-blank lines *)
  let indent = List.fold_left (fun acc line ->
    if String.trim line = "" then acc
    else
      let len = String.length line in
      let i = ref 0 in
      while !i < len && (line.[!i] = ' ' || line.[!i] = '\t') do incr i done;
      match acc with
      | None -> Some !i
      | Some prev -> Some (min prev !i)
  ) None lines in
  let indent = match indent with Some n -> n | None -> 0 in
  (* Remove indent from each line *)
  let lines = List.map (fun line ->
    let len = String.length line in
    if len >= indent then String.sub line indent (len - indent)
    else line
  ) lines in
  String.concat "\n" lines

let read_raw_string l =
  let loc = current_loc l in
  ignore (advance l); (* consume '{' *)
  ignore (advance l); (* consume '|' *)
  let buf = Buffer.create 256 in
  while not (at_end l) && not (peek l = '|' && peek2 l = '}') do
    Buffer.add_char buf (advance l)
  done;
  if at_end l then error l "unterminated raw string";
  ignore (advance l); (* consume '|' *)
  ignore (advance l); (* consume '}' *)
  let raw = Buffer.contents buf in
  make_token l (STRING (dedent_raw raw)) loc

let read_interp_string l =
  let loc = current_loc l in
  ignore (advance l); (* consume '$' *)
  ignore (advance l); (* consume '"' *)
  let parts = ref [] in
  let buf = Buffer.create 64 in
  let flush_lit () =
    if Buffer.length buf > 0 then begin
      parts := Token.IPLit (Buffer.contents buf) :: !parts;
      Buffer.clear buf
    end
  in
  while not (at_end l) && peek l <> '"' do
    if peek l = '\\' then begin
      ignore (advance l);
      if at_end l then error l "unterminated interpolated string";
      let c = advance l in
      match c with
      | 'n' -> Buffer.add_char buf '\n'
      | 't' -> Buffer.add_char buf '\t'
      | '\\' -> Buffer.add_char buf '\\'
      | '"' -> Buffer.add_char buf '"'
      | '{' -> Buffer.add_char buf '{'
      | '}' -> Buffer.add_char buf '}'
      | _ -> error l (Printf.sprintf "unknown escape: \\%c" c)
    end else if peek l = '{' then begin
      flush_lit ();
      ignore (advance l); (* consume '{' *)
      let expr_buf = Buffer.create 64 in
      let fmt_buf = Buffer.create 16 in
      let depth = ref 1 in
      let paren_depth = ref 0 in
      let in_fmt = ref false in
      while !depth > 0 do
        if at_end l then error l "unterminated interpolation expression";
        let c = peek l in
        if c = '{' then begin
          incr depth;
          Buffer.add_char (if !in_fmt then fmt_buf else expr_buf) (advance l)
        end else if c = '}' then begin
          decr depth;
          if !depth > 0 then
            Buffer.add_char (if !in_fmt then fmt_buf else expr_buf) (advance l)
          else ignore (advance l) (* consume closing '}' *)
        end else if c = '(' then begin
          incr paren_depth;
          Buffer.add_char (if !in_fmt then fmt_buf else expr_buf) (advance l)
        end else if c = ')' then begin
          if !paren_depth > 0 then decr paren_depth;
          Buffer.add_char (if !in_fmt then fmt_buf else expr_buf) (advance l)
        end else if c = ':' && !depth = 1 && !paren_depth = 0 && not !in_fmt then begin
          ignore (advance l); (* consume ':' *)
          in_fmt := true
        end else if c = '"' then begin
          (* Skip string literals inside expressions *)
          let buf = if !in_fmt then fmt_buf else expr_buf in
          Buffer.add_char buf (advance l);
          while not (at_end l) && peek l <> '"' do
            if peek l = '\\' then begin
              Buffer.add_char buf (advance l);
              if not (at_end l) then Buffer.add_char buf (advance l)
            end else
              Buffer.add_char buf (advance l)
          done;
          if not (at_end l) then Buffer.add_char buf (advance l)
        end else
          Buffer.add_char (if !in_fmt then fmt_buf else expr_buf) (advance l)
      done;
      let fmt = if !in_fmt then Some (Buffer.contents fmt_buf) else None in
      parts := Token.IPExpr (Buffer.contents expr_buf, fmt) :: !parts
    end else
      Buffer.add_char buf (advance l)
  done;
  if at_end l then error l "unterminated interpolated string";
  ignore (advance l); (* consume closing '"' *)
  flush_lit ();
  make_token l (INTERP_STRING (List.rev !parts)) loc

let read_ident_or_keyword l =
  let loc = current_loc l in
  let buf = Buffer.create 32 in
  while not (at_end l) && is_ident_char (peek l) do
    Buffer.add_char buf (advance l)
  done;
  let s = Buffer.contents buf in
  let kind = match List.assoc_opt s keywords with
    | Some kw -> kw
    | None ->
      if s = "_" then Token.UNDERSCORE
      else if Char.uppercase_ascii s.[0] = s.[0] && s.[0] <> '_' then Token.UIDENT s
      else Token.IDENT s
  in
  make_token l kind loc

let rec next_token l =
  skip_whitespace_and_comments l;
  if at_end l then
    make_token l EOF (current_loc l)
  else
    let loc = current_loc l in
    let c = peek l in
    match c with
    | '0'..'9' -> read_number l
    | '$' when not (at_end l) && peek2 l = '"' -> read_interp_string l
    | '"' -> read_string l
    | '\'' when peek2 l >= 'a' && peek2 l <= 'z' && peek3 l <> '\'' ->
      let loc = current_loc l in
      ignore (advance l);
      let buf = Buffer.create 8 in
      while not (at_end l) && is_ident_char (peek l) do
        Buffer.add_char buf (advance l)
      done;
      make_token l (TYVAR (Buffer.contents buf)) loc
    | _ when is_ident_start c -> read_ident_or_keyword l
    | '\'' ->
      let loc = current_loc l in
      ignore (advance l);
      if at_end l then error l "unterminated rune literal";
      let cp = if peek l = '\\' then begin
        ignore (advance l);
        if at_end l then error l "unterminated rune escape";
        let c = advance l in
        match c with
        | 'n' -> 10 | 't' -> 9 | '\\' -> 92 | '\'' -> 39 | '0' -> 0
        | _ -> error l (Printf.sprintf "unknown rune escape: \\%c" c)
      end else begin
        let b = Char.code (advance l) in
        if b < 0x80 then b
        else
          let nbytes =
            if b land 0xE0 = 0xC0 then 2
            else if b land 0xF0 = 0xE0 then 3
            else if b land 0xF8 = 0xF0 then 4
            else error l "invalid UTF-8 leading byte in rune literal"
          in
          let cp = ref (b land (0xFF lsr (nbytes + 1))) in
          for _ = 2 to nbytes do
            if at_end l then error l "unterminated UTF-8 rune literal";
            let cont = Char.code (advance l) in
            if cont land 0xC0 <> 0x80 then error l "invalid UTF-8 continuation byte";
            cp := !cp lsl 6 lor (cont land 0x3F)
          done;
          !cp
      end in
      if at_end l || peek l <> '\'' then
        error l "unterminated rune literal";
      ignore (advance l);
      make_token l (RUNE cp) loc
    | '(' -> ignore (advance l); make_token l LPAREN loc
    | ')' -> ignore (advance l); make_token l RPAREN loc
    | '#' ->
      ignore (advance l);
      if not (at_end l) && is_hex_digit (peek l)
         && l.cursor + 1 < l.len && is_hex_digit (peek2 l) then begin
        let c1 = peek l in
        ignore (advance l);
        let c2 = advance l in
        let v = hex_val c1 * 16 + hex_val c2 in
        make_token l (BYTE v) loc
      end else
        make_token l HASH loc
    | '{' ->
      if peek2 l = '|' then read_raw_string l
      else begin ignore (advance l); make_token l LBRACE loc end
    | '}' -> ignore (advance l); make_token l RBRACE loc
    | '[' -> ignore (advance l); make_token l LBRACKET loc
    | ']' -> ignore (advance l); make_token l RBRACKET loc
    | ',' -> ignore (advance l); make_token l COMMA loc
    | '.' ->
      ignore (advance l);
      if not (at_end l) && peek l = '.' then begin
        ignore (advance l);
        make_token l DOTDOT loc
      end else
        make_token l DOT loc
    | '^' -> ignore (advance l); make_token l CARET loc
    | '@' ->
      ignore (advance l);
      let buf = Buffer.create 16 in
      while not (at_end l) && (let c = peek l in
        (c >= 'a' && c <= 'z') || c = '_') do
        Buffer.add_char buf (advance l)
      done;
      let name = Buffer.contents buf in
      if name = "partial" then
        make_token l PARTIAL loc
      else
        error l (Printf.sprintf "unknown annotation @%s" name)
    | '+' -> ignore (advance l); make_token l PLUS loc
    | '*' -> ignore (advance l); make_token l STAR loc
    | '/' -> ignore (advance l); make_token l SLASH loc
    | '-' ->
      ignore (advance l);
      if not (at_end l) && peek l = '-' then begin
        (* Single-line comment *)
        ignore (advance l);
        while not (at_end l) && peek l <> '\n' do
          ignore (advance l)
        done;
        next_token l
      end else if not (at_end l) && peek l = '>' then begin
        ignore (advance l);
        make_token l ARROW loc
      end else
        make_token l MINUS loc
    | ';' ->
      ignore (advance l);
      if not (at_end l) && peek l = ';' then begin
        ignore (advance l);
        make_token l DOUBLE_SEMICOLON loc
      end else
        make_token l SEMICOLON loc
    | ':' ->
      ignore (advance l);
      if not (at_end l) && peek l = '=' then begin
        ignore (advance l);
        make_token l COLONEQUAL loc
      end else if not (at_end l) && peek l = ':' then begin
        ignore (advance l);
        make_token l COLONCOLON loc
      end else if not (at_end l) && peek l = '>' then begin
        ignore (advance l);
        make_token l COLONGT loc
      end else
        make_token l COLON loc
    | '=' ->
      ignore (advance l);
      if not (at_end l) && peek l = '>' then begin
        ignore (advance l);
        make_token l FATARROW loc
      end else
        make_token l EQ loc
    | '<' ->
      ignore (advance l);
      if not (at_end l) && peek l = '=' then begin
        ignore (advance l);
        make_token l LE loc
      end else if not (at_end l) && peek l = '>' then begin
        ignore (advance l);
        make_token l NEQ loc
      end else
        make_token l LT loc
    | '>' ->
      ignore (advance l);
      if not (at_end l) && peek l = '=' then begin
        ignore (advance l);
        make_token l GE loc
      end else
        make_token l GT loc
    | '&' ->
      ignore (advance l);
      if not (at_end l) && peek l = '&' then begin
        ignore (advance l);
        make_token l AMPAMP loc
      end else
        error l "unexpected '&', did you mean '&&'?"
    | '|' ->
      ignore (advance l);
      if not (at_end l) && peek l = '|' then begin
        ignore (advance l);
        make_token l PIPEPIPE loc
      end else if not (at_end l) && peek l = '>' then begin
        ignore (advance l);
        make_token l PIPEARROW loc
      end else
        make_token l PIPE loc
    | '`' ->
      ignore (advance l);
      if not (at_end l) && peek l >= 'A' && peek l <= 'Z' then begin
        let buf = Buffer.create 16 in
        while not (at_end l) && is_ident_char (peek l) do
          Buffer.add_char buf (advance l)
        done;
        make_token l (POLYTAG (Buffer.contents buf)) loc
      end else
        error l "expected uppercase identifier after backtick"
    | _ -> error l (Printf.sprintf "unexpected character: %C" c)

let tokenize source =
  let l = create source in
  let tokens = ref [] in
  let stop = ref false in
  while not !stop do
    let tok = next_token l in
    tokens := tok :: !tokens;
    if tok.kind = Token.EOF then stop := true
  done;
  List.rev !tokens
