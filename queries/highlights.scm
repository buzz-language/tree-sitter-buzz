;;; Highlighting for buzz

(function_signature
  name: (identifier) @function)

(call_expression
  (expression
    (variable_expression
      (qualified_name
        (identifier) @function))))

(call_expression
  (expression
    (dot_expression
      (identifier) @function)))

(function_signature
  param_name: (identifier) @variable.parameter)

(function_signature
  "fun" @keyword.function)

(function_type
  "fun" @keyword.function)

(object_declaration
  name: (identifier) @type)

(object_declaration
  protocols: (identifier) @type)

(protocol_declaration
  name: (identifier) @type)

(enum_declaration
  name: (identifier) @type.enum)

(anonymous_enum_case) @constant

(generic_type) @type

(object_init_expression
  name: (qualified_name) @type)

(type
  (qualified_name) @type)

[
  "this"
] @variable.builtin

((regular_identifier) @variable.builtin
  (#eq? @variable.builtin "this"))

[
  "true"
  "false"
] @constant.builtin.boolean

[
  "null"
] @constant.builtin

[
  ","
  "."
  ";"
] @punctuation.delimiter

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  "from"
  "mut"
  "any"
  "zdef"
] @keyword

[
  "if"
  "else"
  "?"
  "??"
  "is"
] @keyword.control.conditional

[
  (char)
  (string_content)
  (string_delimiter)
  (string_escape)
] @string

[
  (pattern)
] @string.regexp

[
  (integer)
  (hexa)
  (binary)
] @constant.numeric.integer

[
  (double)
] @constant.numeric.float

[
  (comment)
] @comment

[
  (doc_comment)
] @comment.block.documentation

[
  "test"
] @keyword.function

[
  "var"
  "final"
  "object"
  "obj"
  "enum"
  "protocol"
  "str"
  "bool"
  "int"
  "namespace"
  "double"
  "void"
] @keyword.storage.type

[
  "static"
  "extern"
  "mut"
] @keyword.storage.modifier

[
  "for"
  "foreach"
  "while"
  "do"
  "until"
  "in"
] @keyword.control.repeat

[
  "break"
  "continue"
  "return"
  "yield"
  "resume"
  "out"
  "resolve"
] @keyword.control.return

[
  "throw"
  "catch"
  "try"
] @keyword.control.exception

[
  "and"
  "or"
  "as"
] @keyword.control.operator

[
  "import"
  "export"
] @keyword.control.import

[
  "+"
  "+="
  "-="
  "-"
  "=="
  "!="
  "<="
  ">="
  "*"
  "*="
  "/"
  "/="
  "&"
  "&="
  "^"
  "^="
  "|"
  "|="
  ">>"
  ">>="
  "<<"
  ".."
  "typeof"
  "=>"
] @operator

[
  (qualified)
] @type
