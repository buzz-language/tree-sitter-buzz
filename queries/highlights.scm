;;; Highlighting for buzz

(function_signature
  name: (identifier) @function)

(function_signature
  param_name: (identifier) @variable.parameter)

(object_declaration
  name: (identifier) @constructor)

(object_declaration
  protocols: (identifier) @type)

(protocol_declaration
  name: (identifier) @constructor)

(enum_declaration
  name: (identifier) @type.enum)

(object_init_expression
  name: (qualified_name) @constructor)

[
  "this"
] @variable.builtin

[
  "true"
  "false"
] @constant.builtin.boolean

[
  "null"
] @constant.builtin

[
  ""
  "."
  "\\"
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
  (string)
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
  "fun"
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
  "return"
  "yield"
  "resume"
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
