type token =
  | Description of (string * (string option))
  | See_url of (string)
  | See_file of (string)
  | See_doc of (string)
  | T_PARAM
  | T_AUTHOR
  | T_VERSION
  | T_SEE
  | T_SINCE
  | T_BEFORE
  | T_DEPRECATED
  | T_RAISES
  | T_RETURN
  | T_CUSTOM of (string)
  | EOF
  | Desc of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string * (string option)) option
val info_part2 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
val see_info :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Odoc_types.see_ref * string
