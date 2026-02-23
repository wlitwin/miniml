type loc = {
  line: int;
  col: int;
  offset: int;
}

let dummy_loc = { line = 0; col = 0; offset = 0 }

type interp_part =
  | IPLit of string
  | IPExpr of string * string option

type token_kind =
  | INT of int
  | FLOAT of float
  | STRING of string
  | INTERP_STRING of interp_part list
  | BYTE of int
  | RUNE of int
  | TRUE
  | FALSE
  | IDENT of string
  | UIDENT of string
  | TYVAR of string
  | LET
  | REC
  | IN
  | IF
  | ELSE
  | FN
  | MATCH
  | WITH
  | TYPE
  | OF
  | CLASS
  | INSTANCE
  | EFFECT
  | EXTERN
  | PERFORM
  | HANDLE
  | TRY
  | CONTINUE
  | RESUME
  | RETURN
  | MUT
  | FOR
  | DO
  | BREAK
  | WHEN
  | WHERE
  | PARTIAL
  | NOT
  | HASH
  | FATARROW
  | MOD
  | MODULE
  | PUB
  | OPEN
  | END
  | OPAQUE
  | WHILE
  | AND
  | DERIVING
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | ARROW
  | COMMA
  | SEMICOLON
  | DOUBLE_SEMICOLON
  | COLON
  | DOT
  | DOTDOT
  | PIPE
  | PIPEARROW
  | UNDERSCORE
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | CARET
  | COLONCOLON
  | COLONEQUAL
  | EQ
  | NEQ
  | LT
  | GT
  | LE
  | GE
  | AMPAMP
  | PIPEPIPE
  | LAND
  | LOR
  | LXOR
  | LNOT
  | LSL
  | LSR
  | POLYTAG of string
  | COLONGT
  | EOF

type token = {
  kind: token_kind;
  loc: loc;
  end_offset: int;
}

let pp_token_kind = function
  | INT n -> Printf.sprintf "INT(%d)" n
  | FLOAT f -> Printf.sprintf "FLOAT(%g)" f
  | STRING s -> Printf.sprintf "STRING(%S)" s
  | INTERP_STRING _ -> "INTERP_STRING"
  | BYTE n -> Printf.sprintf "BYTE(#%02x)" n
  | RUNE n -> Printf.sprintf "RUNE(%d)" n
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | UIDENT s -> Printf.sprintf "UIDENT(%s)" s
  | TYVAR s -> Printf.sprintf "TYVAR('%s)" s
  | LET -> "LET"
  | REC -> "REC"
  | IN -> "IN"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | FN -> "FN"
  | MATCH -> "MATCH"
  | WITH -> "WITH"
  | TYPE -> "TYPE"
  | OF -> "OF"
  | CLASS -> "CLASS"
  | INSTANCE -> "INSTANCE"
  | EFFECT -> "EFFECT"
  | EXTERN -> "EXTERN"
  | PERFORM -> "PERFORM"
  | HANDLE -> "HANDLE"
  | TRY -> "TRY"
  | CONTINUE -> "CONTINUE"
  | RESUME -> "RESUME"
  | RETURN -> "RETURN"
  | MUT -> "MUT"
  | FOR -> "FOR"
  | DO -> "DO"
  | BREAK -> "BREAK"
  | WHEN -> "WHEN"
  | WHERE -> "WHERE"
  | PARTIAL -> "PARTIAL"
  | NOT -> "NOT"
  | HASH -> "HASH"
  | FATARROW -> "FATARROW"
  | MOD -> "MOD"
  | MODULE -> "MODULE"
  | PUB -> "PUB"
  | OPEN -> "OPEN"
  | END -> "END"
  | OPAQUE -> "OPAQUE"
  | WHILE -> "WHILE"
  | AND -> "AND"
  | DERIVING -> "DERIVING"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | ARROW -> "ARROW"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | DOUBLE_SEMICOLON -> "DOUBLE_SEMICOLON"
  | COLON -> "COLON"
  | DOT -> "DOT"
  | DOTDOT -> "DOTDOT"
  | PIPE -> "PIPE"
  | PIPEARROW -> "PIPEARROW"
  | UNDERSCORE -> "UNDERSCORE"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | CARET -> "CARET"
  | COLONCOLON -> "COLONCOLON"
  | COLONEQUAL -> "COLONEQUAL"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"
  | AMPAMP -> "AMPAMP"
  | PIPEPIPE -> "PIPEPIPE"
  | LAND -> "LAND"
  | LOR -> "LOR"
  | LXOR -> "LXOR"
  | LNOT -> "LNOT"
  | LSL -> "LSL"
  | LSR -> "LSR"
  | POLYTAG s -> Printf.sprintf "POLYTAG(`%s)" s
  | COLONGT -> "COLONGT"
  | EOF -> "EOF"

let pp_token t =
  Printf.sprintf "%s@%d:%d" (pp_token_kind t.kind) t.loc.line t.loc.col
