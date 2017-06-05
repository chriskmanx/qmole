type token =
  | ARGUMENT of (string)
  | LIDENT of (string)
  | UIDENT of (string)
  | OPERATOR of (string)
  | INTEGER of (int64)
  | STAR
  | MINUS
  | DOT
  | SHARP
  | AT
  | DOLLAR
  | BANG
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | EOL

val argument_list_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string list
val argument_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val integer_list_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int list
val integer_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
val int64_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int64
val integer :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
val opt_integer_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int option
val opt_signed_integer_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int option
val opt_signed_int64_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int64 option
val identifier :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val identifier_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val identifier_or_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string option
val opt_identifier :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string option
val opt_identifier_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string option
val expression_list_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parser_aux.expression list
val break_argument_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Parser_aux.break_arg
val list_arguments_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Longident.t option * int option * int option
val end_of_line :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
val longident_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Longident.t
val opt_longident :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Longident.t option
val opt_longident_eol :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Longident.t option
