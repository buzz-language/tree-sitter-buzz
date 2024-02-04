const PREC = {
  NONE: 0,
  ASSIGNMENT: 1,
  IS_AS: 2,
  OR: 3,
  AND: 4,
  EQUALITY: 5,
  COMPARISON: 6,
  TERM: 7,
  NULL_COALESCING: 8,
  BITWISE: 9,
  SHIFT: 10,
  FACTOR: 11,
  UNARY: 12,
  CALL: 13,
  PRIMARY: 14,
  PREFIX: 15
}

module.exports = grammar({
  name: 'buzz',

  extras: $ => [/[\n]/, /\s/, /\|.*\n/],
  word: $ => $.identifier,

  conflicts: $ => [[$._variable_expression, $._type]],

  rules: {
    program: $ =>
      seq(optional($.shebang), repeat(choice($._declaration, $._statement))),

    shebang: _ => /#![^\n]*/,

    integer: $ => /[0-9]+[0-9_]*/,
    binary: $ => /0b[0-1]+[0-1_]*/,
    hexa: $ => /0x[a-fA-F0-9]+[a-fA-F0-9_]*/,
    float: $ => /[0-9]+[0-9_]*\.[0-9][0-9_]*/,
    char: $ => /'\\?.'/,

    // TODO: interpolation
    string: $ => choice(/"[^\n]*"/, /`.*`/),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    _declaration: $ =>
      choice(
        $._fun_declaration,
        $._object_declaration,
        $._protocol_declaration,
        $._enum_declaration,
        seq($._var_declaration, ';')
      ),

    _statement: $ =>
      choice(
        $._break_statement,
        $._continue_statement,
        $._do_until_statement,
        $._expression_statement,
        $._export_statement,
        $._for_statement,
        $._if_statement,
        $._import_statement,
        $._out_statement,
        $._return_statement,
        $._test_statement,
        $._throw_statement,
        $._try_statement,
        $._while_statement,
        $._zdef_statement,
        $._assignment
      ),

    // Some expression when alone can conflict with statements, so we forbid it like buzz would do and force at least a `_ = expr` notation
    _lone_expression: $ =>
      choice(
        $._and,
        $._anonymous_object_init,
        $._async_call,
        $._binary_expression,
        $._block_expression,
        $._call_expression,
        $._dot_expression,
        $._force_unwrap_expression,
        $._graceful_unwrap_expression,
        $._grouping_expression,
        $._is_expression,
        $._as_expression,
        $._list_expression,
        $._literal_expression,
        $.string, // TODO: string rule with interpolations
        $._map_expression,
        $._object_init_expression,
        $._or_expression,
        $._range_expression,
        $._resolve_expression,
        $._resume_expression,
        $._subscript_expression,
        $._type_expression,
        $._type_of_expression,
        $._unary_expression,
        $._variable_expression,
        $._yield_expression
      ),

    _expression_statement: $ => seq($._lone_expression, ';'),

    _expression: $ =>
      choice(
        $._and,
        $._anonymous_object_init,
        $._async_call,
        $._binary_expression,
        $._block_expression,
        $._call_expression,
        $._dot_expression,
        $._force_unwrap_expression,
        $._graceful_unwrap_expression,
        $._grouping_expression,
        $._is_expression,
        $._as_expression,
        $._list_expression,
        $._literal_expression,
        $.string, // TODO: string rule with interpolations
        $._map_expression,
        $._object_init_expression,
        $._or_expression,
        $._range_expression,
        $._resolve_expression,
        $._resume_expression,
        $._subscript_expression,
        $._type_expression,
        $._type_of_expression,
        $._unary_expression,
        $._variable_expression,
        $._yield_expression
      ),

    _assignment: $ =>
      prec.left(PREC.ASSIGNMENT, seq($._expression, '=', $._expression, ';')),

    _and: $ => prec.left(PREC.AND, seq($._expression, 'and', $._expression)),

    _anonymous_object_init: $ =>
      seq('.', '{', optional(comma_separated($._var_declaration)), '}'),

    _async_call: $ => prec(PREC.PREFIX, seq('&', $._call_expression)),

    _block_expression: $ => seq('from', $._block),

    _block: $ => seq('{', repeat(choice($._declaration, $._statement)), '}'),

    _break_statement: $ => seq('break', $._expression, ';'),

    _continue_statement: $ => seq('continue', $._expression, ';'),

    _dot_expression: $ =>
      prec.left(PREC.CALL, seq($._expression, '.', $.identifier)),

    _do_until_statement: $ =>
      seq('do', $._block, 'until', '(', $._expression, ')'),

    _export_statement: $ =>
      seq(
        'export',
        choice($.identifier, $._declaration),
        optional(seq('as', $.identifier)),
        ';'
      ),

    _for_statement: $ =>
      seq(
        'for',
        '(',
        optional(comma_separated($._var_declaration)),
        ';',
        optional(comma_separated($._expression)),
        ';',
        optional(comma_separated($._expression)),
        ')',
        $._block
      ),

    _force_unwrap_expression: $ =>
      prec.right(PREC.CALL, seq($._expression, '!')),

    _graceful_unwrap_expression: $ =>
      prec.right(PREC.CALL, seq($._expression, '?')),

    _grouping_expression: $ => seq('(', $._expression, ')'),

    _is_expression: $ =>
      prec.left(PREC.IS_AS, seq($._expression, 'is', $._type)),

    _as_expression: $ =>
      prec.left(PREC.IS_AS, seq($._expression, 'as', $._type)),

    _list_expression: $ =>
      seq('[', optional(comma_separated($._expression)), ']'),

    _literal_expression: $ =>
      prec(
        PREC.NONE,
        choice(
          'true',
          'false',
          'null',
          $.integer,
          $.float,
          $.binary,
          $.hexa,
          $.char
        )
      ),

    _map_expression: $ =>
      seq(
        '{',
        optional(comma_separated(seq($._expression, ':', $._expression))),
        '}'
      ),

    _or_expression: $ =>
      prec.left(PREC.OR, seq($._expression, 'or', $._expression)),

    _range_expression: $ =>
      prec.left(PREC.PRIMARY, seq($._expression, '..', $._expression)),

    _resolve_expression: $ =>
      prec.right(PREC.PRIMARY, seq('resolve', $._expression)),

    _resume_expression: $ =>
      prec.right(PREC.PRIMARY, seq('resume', $._expression)),

    _subscript_expression: $ =>
      prec.left(
        PREC.CALL,
        seq(
          $._expression,
          '[',
          $._expression,
          ']',
          optional(seq('=', $._expression))
        )
      ),

    _type_expression: $ => seq('<', $._type, '>'),

    _type_of_expression: $ =>
      prec.right(PREC.UNARY, seq('typeof', $._expression)),

    _variable_expression: $ => prec(PREC.NONE, $.identifier),

    _yield_expression: $ =>
      prec.right(PREC.PRIMARY, seq('yield', $._expression)),

    _if_statement: $ =>
      seq(
        'if',
        '(',
        $._expression,
        optional(seq('->', $.identifier)),
        ')',
        $._block,
        optional(seq('else', choice($._block, $._if_statement)))
      ),

    _inline_if_statement: $ =>
      seq(
        'if',
        '(',
        $._expression,
        ')',
        $._expression,
        optional(seq('else', choice($._expression, $._inline_if_statement)))
      ),

    _import_statement: $ =>
      seq(
        'import',
        optional(seq(comma_separated($.identifier), 'from')),
        $.string,
        optional(seq('as', comma_separated($.identifier))),
        ';'
      ),

    _out_statement: $ => seq('out', $._expression, ';'),

    _return_statement: $ => seq('return', $._expression, ';'),

    _test_statement: $ => seq('test', $.string, $._block),

    _throw_statement: $ => seq('throw', $._expression, ';'),

    _try_statement: $ =>
      seq(
        'try',
        $._block,
        seq(repeat($._catch_clause), optional($._catch_only_clause))
      ),

    _catch_clause: $ => seq('catch', '(', $._type, $.identifier, ')', $._block),

    _catch_only_clause: $ => seq('catch', $._block),

    _while_statement: $ => seq('while', '(', $._expression, ')', $._block),

    _zdef_statement: $ => seq('zdef', '(', $.string, ',', $.string, ')', ';'),

    _function_signature: $ =>
      seq(
        'fun',
        optional($.identifier),
        '(',
        optional(
          comma_separated(
            seq($._type, $.identifier, optional(seq('=', $._expression)))
          )
        ),
        ')',
        seq('>', $._type),
        optional(seq('*>', $._type)),
        optional(seq('!>', comma_separated($._type)))
      ),

    _function_type: $ =>
      prec.left(
        PREC.NONE,
        seq(
          'Function',
          optional($.identifier),
          '(',
          optional(
            comma_separated(
              seq($._type, $.identifier, optional(seq('=', $._expression)))
            )
          ),
          ')',
          seq('>', $._type),
          optional(seq('*>', $._type)),
          optional(seq('!>', comma_separated($._type)))
        )
      ),

    _fun_declaration: $ =>
      seq($._function_signature, choice($._block, seq('->', $._expression))),

    _object_declaration: $ =>
      seq(
        'object',
        optional(seq('(', comma_separated($.identifier), ')')),
        $.identifier,
        '{',
        repeat($._object_field),
        '}'
      ),

    _protocol_declaration: $ =>
      seq(
        'protocol',
        $.identifier,
        '{',
        repeat(seq($._function_signature, ';')),
        '}'
      ),

    _object_field: $ =>
      seq(
        optional('static'),
        choice(seq($._var_declaration, ','), $._fun_declaration)
      ),

    _enum_declaration: $ =>
      seq(
        'enum',
        optional(seq('(', $._type, ')')),
        $.identifier,
        '{',
        comma_separated(seq($.identifier, optional(seq('=', $._expression)))),
        '}'
      ),

    _type: $ =>
      prec.left(
        PREC.NONE,
        seq(
          choice(
            'pat',
            'ud',
            'int',
            'float',
            'str',
            'bool',
            'type',
            'any',
            seq('{', $._type, ':', $._type, '}'),
            seq('[', $._type, ']'),
            $._function_type,
            seq(optional(seq($.identifier, '.')), $.identifier),
            seq('obj', '{', optional(comma_separated($._var_declaration)), '}')
          ),
          optional('?')
        )
      ),

    _var_declaration: $ =>
      seq(
        choice(
          choice('var', 'const'),
          $._type,
          seq(choice('var', 'const'), $._type)
        ),
        $.identifier,
        optional(seq('=', $._expression))
      ),

    _binary_expression: $ =>
      choice(
        prec.left(PREC.COMPARISON, seq($._expression, '>', $._expression)),
        prec.left(PREC.COMPARISON, seq($._expression, '<', $._expression)),
        prec.left(PREC.COMPARISON, seq($._expression, '>=', $._expression)),
        prec.left(PREC.COMPARISON, seq($._expression, '<=', $._expression)),
        prec.left(PREC.TERM, seq($._expression, '+', $._expression)),
        prec.left(PREC.TERM, seq($._expression, '-', $._expression)),
        prec.left(PREC.FACTOR, seq($._expression, '*', $._expression)),
        prec.left(PREC.FACTOR, seq($._expression, '/', $._expression)),
        prec.left(PREC.FACTOR, seq($._expression, '%', $._expression)),
        prec.left(PREC.EQUALITY, seq($._expression, '==', $._expression)),
        prec.left(PREC.EQUALITY, seq($._expression, '!=', $._expression)),
        prec.left(
          PREC.NULL_COALESCING,
          seq($._expression, '??', $._expression)
        ),
        prec.left(PREC.SHIFT, seq($._expression, '>>', $._expression)),
        prec.left(PREC.SHIFT, seq($._expression, '<<', $._expression)),
        prec.left(PREC.BITWISE, seq($._expression, '^', $._expression)),
        prec.left(PREC.BITWISE, seq($._expression, '\\', $._expression)),
        prec.right(PREC.TERM, seq($._expression, '&', $._expression))
      ),

    _unary_expression: $ =>
      choice(
        prec.right(PREC.TERM, seq('-', $._expression)),
        prec.right(PREC.CALL, seq('!', $._expression)),
        prec.right(PREC.TERM, seq('~', $._expression))
      ),

    _argument_list: $ =>
      comma_separated(
        choice($._expression, seq($.identifier, ':', $._expression))
      ),

    _call_expression: $ =>
      prec.left(PREC.CALL, seq($._expression, '(', $._argument_list, ')')),

    _object_init_expression: $ =>
      prec.left(
        PREC.PRIMARY,
        seq(
          optional(seq($.identifier, '.')),
          $.identifier,
          '{',
          comma_separated(seq($.identifier, optional(seq('=', $._expression)))),
          '}'
        )
      )
  }
})

function comma_separated (rule) {
  return seq(rule, repeat(seq(',', rule)))
}
