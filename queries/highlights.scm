;;; Highlighting for buzz

; Keywords
; --------

[
  "true",
  "false"
] @constant.builtin.boolean

[
  "null"
] @constant.builtin

[
  ",",
  ".",
  "\\",
  ";"
] @punctuation.delimiter

[
  "("
  ")",
  "{",
  "}",
  "[",
  "]"
] @punctuation.bracket

[
  "if",
  "else",
  "?.",
  "??",
  "is"
] @keyword.control.conditional

(qualified @namespace)

[
  (string)
] @string

[
  "fun",
  "test"
] @keyword.function

[
  "var",
  "const",
  "object",
  "enum",
  "protocol",
  "str",
  "bool",
  "int",
  "float",
  "void"
] @keyword.storage.type

[
  "static",
  "extern"
] @keyword.storage.modifier

[
  "for",
  "foreach",
  "while",
  "do",
  "until",
  "in",
] @keyword.control.repeat

[
  "return",
  "yield",
  "resume"
] @keyword.control.return

[
  "throw",
  "catch",
  "try"
] @keyword.control.exception

[
  "and",
  "or",
  "as"
] @keyword.control.operator

[
  "import",
  "export",
] @keyword.control.import

[
  "+",
  "-",
  "==",
  "!=",
  "<=",
  ">=",
  "*",
  "/",
  "&",
  "^",
  "|",
  ">>",
  "<<",
  "..",
  "typeof",
  "=>"
] @operator

