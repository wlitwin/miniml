/**
 * Tree-sitter grammar for MiniML (the language in this repo).
 *
 * Pragmatic, highlight-oriented: it nails the lexical layer (comments, strings +
 * interpolation, runes, bytes, numbers, type variables, operators, keywords) and
 * the declaration/expression structure that matters for syntax highlighting.
 * It is intentionally a bit permissive in expression nesting — tree-sitter
 * degrades gracefully, and highlighting only needs the token/structure shapes.
 *
 * Reference: docs/syntax.md and lib/lexer.ml / self_host/lexer.mml.
 */

const PREC = {
  seq: 1,
  assign: 2,
  pipe: 3,
  or: 4,
  and: 5,
  compare: 6,
  cons: 7,         // :: is right-assoc
  concat: 8,       // ^
  add: 9,
  mul: 10,
  unary: 12,
  app: 13,
  field: 14,
  annot: 1,
};

module.exports = grammar({
  name: 'miniml',

  extras: $ => [/[ \t\r\n]/, $.line_comment, $.block_comment],

  word: $ => $.lower_identifier,

  conflicts: $ => [
    [$._simple_expression, $._pattern],
    [$.tuple_expression, $.parenthesized_expression],
    [$.value_path, $.constructor_path],
    [$.type_path, $.constructor_path],
    [$.value_path, $.type_path],
    [$.value_path, $.constructor_path, $.type_path],
    [$.record_field_value, $.value_path],
    [$.record_expression, $.record_update_expression],
    [$.record_field_pattern, $.lower_identifier],
    [$._type_app, $.type_application],
    [$._type_no_arrow, $.type_application],
    [$.tuple_type, $.type_application],
    [$.function_type, $.type_application],
    [$._simple_type, $.type_application],
    [$.match_expression],
    [$.fn_expression],
    [$.handle_expression],
    [$.attributed_expression],
  ],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice($._declaration, ';;'),

    // ---- Comments ----
    line_comment: $ => token(seq('--', /[^\n]*/)),
    // Non-nested (* ... *) — a good approximation for highlighting. The body
    // alternatives forbid `(*` immediately followed by `)`, so the `(*)`
    // multiplication operator (e.g. `let (*) = ...` in stdlib/classes.mml) is
    // NOT swallowed as a comment start — mirrors the lexer's `peek3 <> )` guard
    // (self_host/lexer.mml). Tree-sitter's regex engine has no lookahead, hence
    // the explicit cases: star-run close `(**)`, star-run + content, or a first
    // char that is neither `)` nor `*`.
    block_comment: $ => token(seq('(*', /\*+\)|\*+[^*)]([^*]|\*+[^*)])*\*+\)|[^)*]([^*]|\*+[^*)])*\*+\)/)),

    // ---- Declarations ----
    _declaration: $ => choice(
      $.let_declaration,
      $.type_declaration,
      $.module_declaration,
      $.effect_declaration,
      $.class_declaration,
      $.instance_declaration,
      $.extern_declaration,
      $.open_declaration,
      $._expression,
    ),

    visibility: $ => choice('pub', 'opaque'),

    let_declaration: $ => seq(
      optional($.visibility),
      'let',
      optional('rec'),
      optional($.locally_abstract),
      optional('mut'),
      field('name', $._binding_name),
      repeat($.parameter),
      optional($.type_annotation),
      optional($.where_clause),
      '=',
      field('body', $._expression),
      optional(repeat(seq('and', optional('rec'), field('name', $._binding_name), repeat($.parameter), optional($.type_annotation), optional($.where_clause), '=', $._expression))),
    ),

    locally_abstract: $ => seq('(', 'type', repeat1($.type_variable), ')'),

    type_parameters: $ => choice(repeat1($.type_variable), seq('(', commaSep1($.type_variable), ')')),

    _binding_name: $ => choice($.lower_identifier, $.parenthesized_operator),

    parenthesized_operator: $ => seq('(', $._operator_symbol, ')'),

    parameter: $ => choice(
      $.lower_identifier,
      '_',
      '()',
      $.tuple_pattern,
      $.record_pattern,
      seq('(', $._pattern, optional($.type_annotation), ')'),
    ),

    type_declaration: $ => seq(
      optional($.visibility),
      choice('type', 'newtype'),
      optional($.type_parameters),
      field('name', $.lower_identifier),
      optional(seq('=', $._type_definition)),
      optional($.deriving_clause),
      repeat(seq('and', repeat($.type_variable), $.lower_identifier, '=', $._type_definition, optional($.deriving_clause))),
    ),

    deriving_clause: $ => seq('deriving', commaSep1($.uppercase_identifier)),

    _type_definition: $ => choice(
      $.variant_definition,
      $.record_definition,
      $._type,
    ),

    variant_definition: $ => seq(
      optional('|'),
      sep1('|', $.variant_constructor),
    ),

    variant_constructor: $ => seq(
      field('name', $.uppercase_identifier),
      optional(seq('of', choice($.record_definition, $._type))),
    ),

    record_definition: $ => seq(
      '{',
      commaOrSemiSep($.record_field_definition),
      '}',
    ),

    record_field_definition: $ => seq(
      optional('mut'),
      field('name', $.lower_identifier),
      ':',
      $._type,
    ),

    module_declaration: $ => seq(
      optional($.visibility),
      'module',
      field('name', $.uppercase_identifier),
      '=',
      repeat($._module_item),
      'end',
    ),

    _module_item: $ => choice($._declaration, ';;'),

    open_declaration: $ => seq('open', $.module_path),

    effect_declaration: $ => seq(
      optional($.visibility),
      'effect',
      field('name', $.uppercase_identifier),
      optional(repeat($.type_variable)),
      '=',
      repeat($.effect_operation),
      'end',
    ),

    effect_operation: $ => seq(
      field('name', $.lower_identifier),
      ':',
      $._type,
    ),

    class_declaration: $ => seq(
      optional($.visibility),
      'class',
      field('name', $.uppercase_identifier),
      optional($.type_parameters),
      optional($.where_clause),
      '=',
      repeat($.class_method),
      'end',
    ),

    where_clause: $ => seq('where', /[^=]*/),

    class_method: $ => seq(
      field('name', $._binding_name),   // operator methods too: `(*) : 'a -> 'a -> 'a`
      ':',
      $._type,
    ),

    instance_declaration: $ => seq(
      optional($.visibility),
      'instance',
      $.uppercase_identifier,
      repeat($._simple_type),
      optional($.where_clause),
      '=',
      repeat($._declaration),
      'end',
    ),

    extern_declaration: $ => seq(
      optional($.visibility),
      'extern',
      field('name', $._binding_name),
      ':',
      $._type,
    ),

    // ---- Types ----
    type_annotation: $ => seq(':', $._type),

    _type: $ => choice(
      $.function_type,
      $._type_no_arrow,
    ),

    function_type: $ => prec.right(seq(
      $._type_no_arrow,
      '->',
      $._type,
      optional(seq('/', $._effect_row)),
    )),

    _effect_row: $ => choice($.type_variable, $.uppercase_identifier, seq('{', commaSep($._type), '}')),

    _type_no_arrow: $ => choice(
      $.tuple_type,
      $._type_app,
    ),

    tuple_type: $ => prec.left(seq($._type_app, repeat1(seq('*', $._type_app)))),

    _type_app: $ => choice(
      $.type_application,
      $._simple_type,
    ),

    type_application: $ => prec.left(seq(
      choice($._type_app, seq('(', commaSep1($._type), ')')),
      $.type_path,
    )),

    _simple_type: $ => choice(
      $.type_variable,
      $.type_path,
      seq('(', $._type, ')'),
    ),

    type_path: $ => seq(repeat(seq($.uppercase_identifier, '.')), $.lower_identifier),

    type_variable: $ => /'[a-zA-Z_][a-zA-Z0-9_]*/,

    // ---- Expressions ----
    _expression: $ => choice(
      $.attributed_expression,
      $.let_in_expression,
      $.if_expression,
      $.match_expression,
      $.fn_expression,
      $.handle_expression,
      $.for_expression,
      $.sequence_expression,
      $._binary_expression,
    ),

    // an attribute like `@partial` / `@inline` prefixing an expression
    attribute: $ => token(seq('@', /[a-z_][a-zA-Z0-9_]*/)),
    attributed_expression: $ => prec.right(seq(repeat1($.attribute), $._expression)),

    sequence_expression: $ => prec.left(PREC.seq, seq($._binary_expression, ';', $._expression)),

    let_in_expression: $ => seq(
      'let',
      optional('rec'),
      optional('mut'),
      field('name', $._binding_name),
      repeat($.parameter),
      optional($.type_annotation),
      '=',
      field('value', $._expression),
      'in',
      field('body', $._expression),
    ),

    if_expression: $ => prec.right(seq(
      'if',
      field('condition', $._expression),
      'do',
      field('consequence', $._expression),
      optional(seq('else', field('alternative', $._expression))),
      optional('end'),
    )),

    match_expression: $ => seq(
      'match',
      field('value', $._expression),
      'with',
      optional('|'),
      sep1('|', $.match_arm),
    ),

    match_arm: $ => prec.right(seq(
      field('pattern', $._pattern),
      optional(seq('when', $._expression)),
      '->',
      field('body', $._expression),
    )),

    fn_expression: $ => prec.right(seq(
      'fn',
      choice(
        seq(repeat($.parameter), '->', field('body', $._expression)),
        seq('|', sep1('|', $.match_arm)),
      ),
    )),

    handle_expression: $ => seq(
      'handle',
      field('body', $._expression),
      'with',
      optional('|'),
      sep1('|', $.handle_arm),
    ),

    handle_arm: $ => prec.right(seq(
      choice(
        seq('return', $.lower_identifier),
        seq(field('op', $.lower_identifier), repeat($.parameter)),
      ),
      '->',
      field('body', $._expression),
    )),

    for_expression: $ => prec.right(seq(
      'for',
      $._expression,
      'do',
      $._expression,
      optional('end'),
    )),

    _binary_expression: $ => choice(
      $.binary_expression,
      $.assignment_expression,
      $._unary_expression,
    ),

    assignment_expression: $ => prec.right(PREC.assign, seq($._unary_expression, ':=', $._expression)),

    binary_expression: $ => {
      const table = [
        [PREC.pipe, '|>'],
        [PREC.or, choice('||')],
        [PREC.and, choice('&&')],
        [PREC.compare, choice('=', '==', '<>', '!=', '<', '>', '<=', '>=')],
        [PREC.concat, '^'],
        [PREC.add, choice('+', '-', '+.', '-.')],
        [PREC.mul, choice('*', '/', '*.', '/.', 'mod', 'land', 'lor', 'lxor', 'lsl', 'lsr')],
      ];
      const cons = prec.right(PREC.cons, seq($._unary_expression, '::', $._binary_expression));
      return choice(cons, ...table.map(([p, op]) =>
        prec.left(p, seq($._binary_expression, field('operator', op), $._binary_expression))));
    },

    _unary_expression: $ => choice(
      $.unary_expression,
      $._application_expression,
    ),

    unary_expression: $ => prec(PREC.unary, seq(choice('-', '-.', 'not', 'lnot'), $._unary_expression)),

    _application_expression: $ => choice(
      $.application_expression,
      $.perform_expression,
      $.resume_expression,
      $.return_expression,
      $.break_expression,
      $.continue_expression,
      $._postfix_expression,
    ),

    application_expression: $ => prec.left(PREC.app, seq(
      field('function', $._postfix_expression),
      repeat1($._postfix_expression),
    )),

    perform_expression: $ => prec.right(seq('perform', $.lower_identifier, optional($._postfix_expression))),
    resume_expression: $ => prec.right(seq('resume', $._postfix_expression, optional($._postfix_expression))),
    return_expression: $ => prec.right(seq('return', optional($._expression))),
    break_expression: $ => prec.right(seq('break', optional($._expression))),
    continue_expression: $ => prec.right(seq('continue', optional($._expression))),

    _postfix_expression: $ => choice(
      $.field_expression,
      $.index_expression,
      $._simple_expression,
    ),

    field_expression: $ => prec.left(PREC.field, seq($._postfix_expression, '.', $.lower_identifier)),
    index_expression: $ => prec.left(PREC.field, seq($._postfix_expression, '[', $._expression, ']')),

    _simple_expression: $ => choice(
      $.integer_literal,
      $.float_literal,
      $.byte_literal,
      $.rune_literal,
      $.string_literal,
      $.interpolated_string,
      $.boolean_literal,
      $.unit_literal,
      $.value_path,
      $.constructor_path,
      $.record_expression,
      $.record_update_expression,
      $.list_expression,
      $.tuple_expression,
      $.parenthesized_expression,
      $.parenthesized_operator,
    ),

    value_path: $ => prec.right(seq(repeat(seq($.uppercase_identifier, '.')), $.lower_identifier)),
    constructor_path: $ => prec.right(sep1('.', $.uppercase_identifier)),

    record_expression: $ => prec(2, seq(
      '{',
      commaOrSemiSep($.record_field_value),
      '}',
    )),

    record_update_expression: $ => prec(2, seq(
      '{',
      $._expression,
      'with',
      commaOrSemiSep($.record_field_value),
      '}',
    )),

    record_field_value: $ => choice(
      seq(field('name', $.lower_identifier), '=', $._expression),
      field('name', $.lower_identifier),
    ),

    list_expression: $ => seq('[', sep(';', $._expression), optional(';'), ']'),

    tuple_expression: $ => prec(1, seq('(', $._expression, repeat1(seq(',', $._expression)), ')')),

    parenthesized_expression: $ => seq('(', $._expression, optional($.type_annotation), ')'),

    // ---- Patterns ----
    _pattern: $ => choice(
      $.or_pattern,
      $.cons_pattern,
      $._simple_pattern,
    ),

    or_pattern: $ => prec.left(seq($._pattern, '|', $._pattern)),
    cons_pattern: $ => prec.right(seq($._simple_pattern, '::', $._pattern)),

    _simple_pattern: $ => choice(
      $.wildcard_pattern,
      $.lower_identifier,
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.byte_literal,
      $.rune_literal,
      $.boolean_literal,
      $.unit_literal,
      $.constructor_pattern,
      $.tuple_pattern,
      $.list_pattern,
      $.record_pattern,
      $.as_pattern,
      seq('(', $._pattern, optional($.type_annotation), ')'),
    ),

    wildcard_pattern: $ => '_',
    constructor_pattern: $ => prec.right(seq($.constructor_path, optional($._simple_pattern))),
    as_pattern: $ => prec.left(seq($._simple_pattern, 'as', $.lower_identifier)),
    tuple_pattern: $ => seq('(', $._pattern, repeat1(seq(',', $._pattern)), ')'),
    list_pattern: $ => seq('[', sep(';', $._pattern), ']'),
    record_pattern: $ => seq('{', commaOrSemiSep($.record_field_pattern), optional(';'), '}'),
    record_field_pattern: $ => choice(seq($.lower_identifier, '=', $._pattern), $.lower_identifier),

    // ---- Literals & lexemes ----
    integer_literal: $ => token(choice(/[0-9][0-9_]*/, /0[xX][0-9a-fA-F_]+/, /0[bB][01_]+/)),
    float_literal: $ => token(/[0-9][0-9_]*\.[0-9_]*([eE][+-]?[0-9]+)?/),
    byte_literal: $ => token(/#[0-9a-fA-F][0-9a-fA-F]?/),
    rune_literal: $ => token(seq("'", choice(/[^'\\]/, /\\./), "'")),
    boolean_literal: $ => choice('true', 'false'),
    unit_literal: $ => '()',

    string_literal: $ => seq('"', repeat(choice($.escape_sequence, $._string_char)), '"'),
    _string_char: $ => token.immediate(/[^"\\]+/),
    escape_sequence: $ => token.immediate(seq('\\', /./)),

    // Interpolated string $"...{expr}..." — highlight as a string with embedded code.
    interpolated_string: $ => seq(
      '$"',
      repeat(choice(
        $.interpolation,
        $.escape_sequence,
        token.immediate(/[^"\\{]+/),
      )),
      '"',
    ),
    interpolation: $ => seq('{', $._expression, optional(seq(':', /[^}]*/)), '}'),

    _operator_symbol: $ => choice(
      '+', '-', '*', '/', '^', '=', '==', '<>', '!=', '<', '>', '<=', '>=',
      '::', '@', '|>', '&&', '||', ':=', '+.', '-.', '*.', '/.',
      'mod', 'land', 'lor', 'lxor', 'lnot', 'lsl', 'lsr',
    ),

    module_path: $ => sep1('.', $.uppercase_identifier),

    lower_identifier: $ => /[a-z_][a-zA-Z0-9_']*/,
    uppercase_identifier: $ => /[A-Z][a-zA-Z0-9_']*/,
  },
});

function sep1(s, rule) { return seq(rule, repeat(seq(s, rule))); }
function sep(s, rule) { return optional(sep1(s, rule)); }
function commaSep1(rule) { return sep1(',', rule); }
function commaSep(rule) { return optional(commaSep1(rule)); }
function commaOrSemiSep(rule) { return optional(seq(rule, repeat(seq(choice(',', ';'), rule)))); }
