; Tree-sitter highlight queries for MiniML (Zed / nvim-treesitter capture names).

; ---- Comments ----
(line_comment) @comment
(block_comment) @comment

; ---- Literals ----
(integer_literal) @number
(float_literal) @number
(byte_literal) @number
(boolean_literal) @boolean
(unit_literal) @constant.builtin
(rune_literal) @string
(string_literal) @string
(escape_sequence) @string.escape
(interpolated_string) @string
(interpolation ["{" "}"] @punctuation.special)
(attribute) @attribute

; ---- Types ----
(type_variable) @type
(type_path (lower_identifier) @type)
(type_declaration name: (lower_identifier) @type)
(variant_constructor name: (uppercase_identifier) @constructor)
(constructor_path (uppercase_identifier) @constructor)
(deriving_clause (uppercase_identifier) @type)
(effect_declaration name: (uppercase_identifier) @type)
(class_declaration name: (uppercase_identifier) @type)

; ---- Modules ----
(module_declaration name: (uppercase_identifier) @module)
(module_path (uppercase_identifier) @module)
; the leading `Mod.` segments of a qualified path
(value_path (uppercase_identifier) @module)
(type_path (uppercase_identifier) @module)

; ---- Functions / values ----
(let_declaration name: (lower_identifier) @function)
(extern_declaration name: (lower_identifier) @function)
(effect_operation name: (lower_identifier) @function)
(class_method name: (lower_identifier) @function)
(application_expression function: (value_path (lower_identifier) @function.call))
(parameter (lower_identifier) @variable.parameter)
(field_expression (lower_identifier) @property)
(record_field_definition name: (lower_identifier) @property)
(record_field_value name: (lower_identifier) @property)

(value_path (lower_identifier) @variable)

; ---- Keywords ----
[
  "let" "rec" "in" "and" "mut"
  "type" "newtype" "of" "deriving"
  "module" "open" "end" "pub" "opaque"
  "class" "instance" "where"
  "extern"
  "effect" "perform" "handle" "resume" "return"
] @keyword

[
  "if" "do" "else" "match" "with" "fn" "for" "when" "break" "continue"
] @keyword.control

["true" "false"] @boolean

; ---- Operators & punctuation ----
[
  "=" "==" "<>" "!=" "<" ">" "<=" ">="
  "+" "-" "*" "/" "^" "::" "@" "|>" "&&" "||" ":=" "->"
  "+." "-." "*." "/."
  "mod" "land" "lor" "lxor" "lnot" "lsl" "lsr" "not"
] @operator

["(" ")" "[" "]" "{" "}"] @punctuation.bracket
[";" ";;" "," ":" "." "|" "/"] @punctuation.delimiter

(wildcard_pattern) @variable.builtin
