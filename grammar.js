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

  conflicts: $ => [
    [$.declaration, $.lone_expression],
    [$.declaration, $.expression],
    [$.argument_list],
    [$.block, $.map_expression],
  ],

  extras: $ => [/[\n]/, /\s/, $.comment, $.doc_comment],
  word: $ => $.regular_identifier,

  rules: {
    program: $ =>
      seq(
        optional($.shebang),
        optional($.namespace),
        repeat(
          choice($.declaration, $.statement
          )
        )
      ),

    shebang: _ => /#![^\n]*/,

    doc_comment: _ => /\/\/\/.*\n/,
    comment: _ => /\/\/.*\n/,
    integer: _ => /[0-9]+[0-9_]*/,
    binary: _ => /0b[0-1]+[0-1_]*/,
    hexa: _ => /0x[a-fA-F0-9]+[a-fA-F0-9_]*/,
    double: _ => /[0-9]+[0-9_]*\.[0-9][0-9_]*/,
    char: _ => /'\\?.'/,
    free_identifier: _ => seq('@"', /[^"]+/, '"'),
    regular_identifier: _ => /[a-zA-Z_][a-zA-Z0-9_]*/,
    identifier: $ =>
      choice(
        $.regular_identifier,
        $.free_identifier,
      ),

    string_literal: $ =>
      choice(
        string_literal('"'),
        string_literal('`')
      ),

    string: $ =>
      choice(
        string($, '"'),
        string($, '`')
      ),

    pattern: _ =>
      seq(
        '$"',
        optional(
          repeat(
            choice(
              /[^"]+/,
              '\\"'
            )
          )
        ),
        '"'
      ),

    namespace: $ =>
      seq(
        "namespace",
        $.qualified_name,
        ";"
      ),

    declaration: $ =>
      choice(
        $.fun_declaration,
        seq('extern', $.function_signature, ';'),
        seq($.arrow_fun_declaration, ';'),
        $.object_declaration,
        $.protocol_declaration,
        $.enum_declaration,
        seq($.qualified_var_declaration, ';')
      ),

    statement: $ =>
      choice(
        $.break_statement,
        $.continue_statement,
        $.do_until_statement,
        $.expression_statement,
        $.export_statement,
        $.for_statement,
        $.foreach_statement,
        $.if_statement,
        $.import_statement,
        $.out_statement,
        $.return_statement,
        $.test_statement,
        $.throw_statement,
        $.try_statement,
        $.while_statement,
        $.zdef_statement,
        $.assignment
      ),

    // Some expression when alone can conflict with statements, so we forbid it like buzz would do and force at least a `_ = expr` notation
    lone_expression: $ =>
      choice(
        $.and,
        $.anonymous_object_init,
        $.async_call,
        $.binary_expression,
        $.block_expression,
        $.call_expression,
        $.dot_expression,
        $.force_unwrap_expression,
        $.graceful_unwrap_expression,
        $.grouping_expression,
        $.is_expression,
        $.as_expression,
        $.list_expression,
        $.literal_expression,
        $.string,
        $.pattern,
        $.map_expression,
        $.object_init_expression,
        $.or_expression,
        $.range_expression,
        $.resolve_expression,
        $.resume_expression,
        $.subscript_expression,
        $.type_expression,
        $.type_of_expression,
        $.unary_expression,
        $.variable_expression,
        $.yield_expression,
        $.arrow_fun_declaration
      ),

    expression_statement: $ => seq($.lone_expression, ';'),

    expression: $ =>
      choice(
        $.and,
        $.anonymous_object_init,
        $.async_call,
        $.binary_expression,
        $.block_expression,
        $.call_expression,
        $.dot_expression,
        $.force_unwrap_expression,
        $.graceful_unwrap_expression,
        $.grouping_expression,
        $.is_expression,
        $.as_expression,
        $.list_expression,
        $.literal_expression,
        $.string,
        $.pattern,
        $.map_expression,
        $.object_init_expression,
        $.or_expression,
        $.range_expression,
        $.resolve_expression,
        $.resume_expression,
        $.subscript_expression,
        $.type_expression,
        $.type_of_expression,
        $.unary_expression,
        $.variable_expression,
        $.yield_expression,
        $.fun_declaration,
        $.arrow_fun_declaration,
        $.if_expression,
      ),

    assignment: $ =>
      prec.left(PREC.ASSIGNMENT, seq($.expression, '=', $.expression, ';')),

    and: $ => prec.left(PREC.AND, seq($.expression, 'and', $.expression)),

    anonymous_object_init: $ =>
      seq(optional('mut'), '.', '{', optional(comma_separated($.var_declaration)), '}'),

    async_call: $ => prec(PREC.PREFIX, seq('&', $.call_expression)),

    block_expression: $ => seq('from', $.block),

    block: $ => seq('{', repeat(choice($.declaration, $.statement)), '}'),

    break_statement: $ => seq('break', optional($.identifier), ';'),

    continue_statement: $ => seq('continue', optional($.identifier), ';'),

    dot_expression: $ =>
      prec.left(PREC.CALL, seq($.expression, '.', $.identifier)),

    do_until_statement: $ =>
      seq('do', $.block, 'until', '(', $.expression, ')'),

    export_statement: $ =>
      seq(
        'export',
        choice($.identifier, $.declaration),
        optional(seq('as', $.identifier)),
        optional(';'),
      ),

    for_statement: $ =>
      seq(
        'for',
        '(',
        optional(comma_separated($.var_declaration)),
        ';',
        comma_separated($.expression),
        ';',
        optional(
          comma_separated(
            choice(
              $.expression,
              prec.left(PREC.ASSIGNMENT, seq($.expression, '=', $.expression))
            )
          )
        ),
        ')',
        optional(
          seq(':', field('label', $.identifier))
        ),
        $.block
      ),

    foreach_statement: $ =>
      seq(
        'foreach',
        '(',
        optional(seq($.identifier, ',')), $.identifier,
        'in',
        $.expression,
        ')',
        optional(
          seq(':', field('label', $.identifier))
        ),
        $.block,
      ),

    force_unwrap_expression: $ =>
      prec.right(PREC.CALL, seq($.expression, '!')),

    graceful_unwrap_expression: $ =>
      prec.right(PREC.CALL, seq($.expression, '?')),

    grouping_expression: $ => seq('(', $.expression, ')'),

    is_expression: $ =>
      prec.left(PREC.IS_AS, seq($.expression, 'is', $.type)),

    as_expression: $ =>
      prec.left(PREC.IS_AS, seq($.expression, 'as', optional('?'), $.type)),

    list_expression: $ =>
      seq(
        optional('mut'),
        '[',
        optional(
          seq(
            comma_separated($.expression),
            optional(','),
          ),
        ),
        ']'
      ),

    literal_expression: $ =>
      prec(
        PREC.NONE,
        choice(
          'this',
          'true',
          'false',
          'null',
          $.integer,
          $.double,
          $.binary,
          $.hexa,
          $.char
        )
      ),

    map_expression: $ =>
      seq(
        optional('mut'),
        '{',
        optional(
          seq(
            comma_separated(seq($.expression, ':', $.expression)),
            optional(','),
          )
        ),
        '}',
      ),

    or_expression: $ =>
      prec.left(PREC.OR, seq($.expression, 'or', $.expression)),

    range_expression: $ =>
      prec.left(PREC.PRIMARY, seq($.expression, '..', $.expression)),

    resolve_expression: $ =>
      prec.right(PREC.PRIMARY, seq('resolve', $.expression)),

    resume_expression: $ =>
      prec.right(PREC.PRIMARY, seq('resume', $.expression)),

    subscript_expression: $ =>
      prec.left(
        PREC.CALL,
        seq(
          $.expression,
          '[',
          optional("?"),
          $.expression,
          ']',
          optional(seq('=', $.expression))
        )
      ),

    type_expression: $ => seq('<', $.type, '>'),

    type_of_expression: $ =>
      prec.right(PREC.UNARY, seq('typeof', $.expression)),

    variable_expression: $ =>
      choice(
        prec.right(PREC.ASSIGNMENT, seq(
          $.qualified_name,
          seq(
            '=',
            $.expression,
          )
        )),
        $.qualified_name,
      ),

    yield_expression: $ =>
      prec.right(PREC.PRIMARY, seq('yield', $.expression)),

    if_expression: $ =>
      seq(
        'if',
        '(',
        $.expression,
        ')',
        $.expression,
        'else',
        $.expression,
      ),

    if_statement: $ =>
      seq(
        'if',
        '(',
        $.expression,
        optional(
          choice(
            seq('->', $.identifier),
            seq('as', $.identifier, ':', $.type)
          )
        ),
        ')',
        $.block,
        optional(seq('else', choice($.block, $.if_statement)))
      ),

    inline_if_statement: $ =>
      seq(
        'if',
        '(',
        $.expression,
        ')',
        $.expression,
        optional(seq('else', choice($.expression, $.inline_if_statement)))
      ),

    import_statement: $ =>
      seq(
        'import',
        optional(seq(comma_separated($.identifier), 'from')),
        $.string,
        optional(seq('as', comma_separated($.identifier))),
        ';'
      ),

    out_statement: $ => seq('out', $.expression, ';'),

    return_statement: $ => seq('return', $.expression, ';'),

    test_statement: $ => seq('test', $.string, $.block),

    throw_statement: $ => seq('throw', $.expression, ';'),

    try_statement: $ =>
      seq(
        'try',
        $.block,
        seq(
          repeat($.catch_clause),
          optional($.catch_only_clause)
        )
      ),

    catch_clause: $ => seq('catch', '(', $.identifier, ':', $.type, ')', $.block),

    catch_only_clause: $ => seq('catch', $.block),

    while_statement: $ =>
      seq(
        'while',
        '(', $.expression, ')',
        optional(
          seq(':', field('label', $.identifier))
        ),
        $.block
      ),

    zdef_statement: $ => seq('zdef', '(', $.string_literal, ',', $.string_literal, ')', ';'),

    function_signature: $ =>
      seq(
        'fun',
        optional(field('name', $.identifier)),
        optional(seq('::<', comma_separated($.identifier), '>')),
        '(',
        optional(
          comma_separated(
            seq(field('param_name', $.identifier), ':', $.type, optional(seq('=', $.expression)))
          ),
        ),
        ')',
        optional(seq('>', $.type)),
        optional(seq('*>', $.type)),
        optional(
          // choice(
          seq('!>', comma_separated($.type)),
          // seq('!>', '(', comma_separated($.type), ')'),
          // )
        )
      ),

    function_type: $ =>
      prec.left(
        PREC.NONE,
        seq(
          'fun',
          optional(seq(optional($.identifier), '::<', comma_separated($.type), '>')),
          '(',
          optional(
            comma_separated(
              seq($.identifier, ':', $.type, optional(seq('=', $.expression)))
            ),
          ),
          ')',
          optional(seq('>', $.type)),
          optional(seq('*>', $.type)),
          optional(seq('!>', optional('('), comma_separated($.type), optional(')')))
        )
      ),

    fun_declaration: $ =>
      seq($.function_signature, $.block),

    arrow_fun_declaration: $ =>
      seq($.function_signature, '=>', $.expression),

    object_declaration: $ =>
      seq(
        'object',
        optional(field('protocols', seq('<', comma_separated($.identifier), '>'))),
        field('name', $.identifier),
        optional(seq('::<', comma_separated($.identifier), '>')),
        '{',
        optional(
          repeat($.object_field),
        ),
        '}'
      ),

    protocol_declaration: $ =>
      seq(
        'protocol',
        field('name', $.identifier),
        '{',
        repeat(seq(optional('mut'), $.function_signature, ';')),
        '}'
      ),

    property_field: $ => seq(
      optional('final'),
      $.identifier,
      ':',
      $.type,
      optional(seq('=', $.expression)),
    ),

    static_property_field: $ => seq('static', $.property_field, ';'),

    instance_property_field: $ => seq($.property_field, ','),

    object_field: $ =>
      choice(
        $.static_property_field,
        $.instance_property_field,
        seq(optional(choice('mut', 'static')), $.fun_declaration),
        $.arrow_fun_declaration,
      ),

    enum_declaration: $ =>
      seq(
        'enum',
        optional(seq('<', $.type, '>')),
        field('name', $.identifier),
        '{',
        comma_separated(seq($.identifier, optional(seq('=', $.expression)))),
        optional(','),
        '}'
      ),

    type: $ =>
      prec.left(
        PREC.NONE,
        seq(
          choice(
            'rg',
            'pat',
            'ud',
            'int',
            'double',
            'str',
            'bool',
            'type',
            'any',
            'void',
            seq(optional('mut'), '{', $.type, ':', $.type, '}'),
            seq(optional('mut'), '[', $.type, ']'),
            $.function_type,
            seq(
              optional('mut'),
              prec.left(
                PREC.CALL,
                seq(
                  $.qualified_name,
                  optional(
                    seq('::<', comma_separated($.type), '>')
                  )
                )
              )
            ),
            seq(
              optional('mut'),
              'obj',
              '{',
              optional(
                seq(
                  comma_separated(
                    seq(optional($.identifier), ':', $.type),
                  ),
                  optional(','),
                ),
              ),
              '}'
            ),
            seq('fib', '<', $.type, ',', $.type, '>'),
          ),
          optional('?')
        )
      ),

    var_declaration: $ =>
      seq(
        $.identifier,
        choice(
          seq(':', $.type, '=', $.expression),
          seq(':', $.type),
          seq('=', $.expression),
        ),
      ),

    qualified_var_declaration: $ =>
      choice(
        seq(
          choice('var', 'final'),
          $.var_declaration,
        ),
        seq(
          '_',
          choice(
            seq(':', $.type, '=', $.expression),
            seq('=', $.expression),
          ),
        )
      ),

    binary_expression: $ =>
      choice(
        prec.left(PREC.COMPARISON, seq($.expression, '>', $.expression)),
        prec.left(PREC.COMPARISON, seq($.expression, '<', $.expression)),
        prec.left(PREC.COMPARISON, seq($.expression, '>=', $.expression)),
        prec.left(PREC.COMPARISON, seq($.expression, '<=', $.expression)),
        prec.left(PREC.TERM, seq($.expression, '+', $.expression)),
        prec.left(PREC.TERM, seq($.expression, '-', $.expression)),
        prec.left(PREC.FACTOR, seq($.expression, '*', $.expression)),
        prec.left(PREC.FACTOR, seq($.expression, '/', $.expression)),
        prec.left(PREC.FACTOR, seq($.expression, '%', $.expression)),
        prec.left(PREC.TERM, seq($.expression, '+=', $.expression)),
        prec.left(PREC.TERM, seq($.expression, '-=', $.expression)),
        prec.left(PREC.FACTOR, seq($.expression, '*=', $.expression)),
        prec.left(PREC.FACTOR, seq($.expression, '/=', $.expression)),
        prec.left(PREC.FACTOR, seq($.expression, '%=', $.expression)),
        prec.left(PREC.EQUALITY, seq($.expression, '==', $.expression)),
        prec.left(PREC.EQUALITY, seq($.expression, '!=', $.expression)),
        prec.left(
          PREC.NULL_COALESCING,
          seq($.expression, '??', $.expression)
        ),
        prec.left(PREC.SHIFT, seq($.expression, '>>', $.expression)),
        prec.left(PREC.SHIFT, seq($.expression, '<<', $.expression)),
        prec.left(PREC.BITWISE, seq($.expression, '^', $.expression)),
        prec.left(PREC.BITWISE, seq($.expression, '|', $.expression)),
        prec.right(PREC.TERM, seq($.expression, '&', $.expression)),
        prec.left(PREC.SHIFT, seq($.expression, '>>=', $.expression)),
        prec.left(PREC.SHIFT, seq($.expression, '<<=', $.expression)),
        prec.left(PREC.BITWISE, seq($.expression, '^=', $.expression)),
        prec.left(PREC.BITWISE, seq($.expression, '|=', $.expression)),
        prec.right(PREC.TERM, seq($.expression, '&=', $.expression))
      ),

    unary_expression: $ =>
      choice(
        prec.right(PREC.TERM, seq('-', $.expression)),
        prec.right(PREC.CALL, seq('!', $.expression)),
        prec.right(PREC.TERM, seq('~', $.expression))
      ),

    argument_list: $ =>
      comma_separated(
        choice($.expression, seq($.identifier, ':', $.expression))
      ),

    call_expression: $ =>
      prec.left(
        PREC.CALL,
        seq(
          $.expression,
          optional(
            seq(
              '::<',
              comma_separated($.type),
              '>'
            )
          ),
          '(',
          optional(seq($.argument_list, optional(','))),
          ')',
          optional(
            seq('catch', $.expression)
          )
        )
      ),

    object_init_expression: $ =>
      prec.left(
        PREC.PRIMARY,
        seq(
          optional('mut'),
          field('name', choice($.qualified_name, '.')),
          optional(seq('::<', comma_separated($.type), '>')),
          '{',
          optional(
            seq(
              comma_separated(
                choice(
                  prec.left(PREC.ASSIGNMENT, seq($.identifier, '=', $.expression)),
                  $.expression,
                )
              ),
              optional(','),
            )
          ),
          '}'
        )
      ),

    qualified_name: $ => seq(optional($.qualified), $.identifier),

    qualified: $ => prec.right(PREC.TERM, seq($.identifier, repeat(seq('\\', $.identifier)), '\\')),
  }
})

function comma_separated(rule) {
  return separated(rule, ',')
}

function separated(rule, separator) {
  return seq(rule, optional(repeat(seq(separator, rule))))
}

function string_literal(delimiter) {
  return seq(
    delimiter,
    optional(
      repeat(
        choice(
          new RustRegex('([^' + delimiter + '\\\\]|[\r\n])+'),
          choice(
            '\\n',
            '\\t',
            /\\[0-9]+/,
            '\\{',
            '\\\\',
            seq('\\', delimiter),
          ),
        )
      )
    ),
    delimiter
  );
}

function string($, delimiter) {
  return seq(
    delimiter,
    optional(
      repeat(
        choice(
          new RustRegex('([^\\\\' + delimiter + '{]|[\r\n])+'),
          choice(
            '\\n',
            '\\t',
            /\\[0-9]+/,
            '\\{',
            '\\\\',
            seq('\\', delimiter),
          ),
          seq(
            '{', $.expression, '}'
          )
        )
      )
    ),
    delimiter
  );
}
