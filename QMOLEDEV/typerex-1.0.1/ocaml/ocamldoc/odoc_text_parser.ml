type token =
  | END
  | Title of (int * string option)
  | BOLD
  | EMP
  | CENTER
  | LEFT
  | RIGHT
  | ITALIC
  | CUSTOM of (string)
  | LIST
  | ENUM
  | ITEM
  | LINK
  | CODE
  | END_CODE
  | CODE_PRE
  | END_CODE_PRE
  | VERB
  | END_VERB
  | LATEX
  | Target of (string)
  | END_TARGET
  | LBRACE
  | ELE_REF
  | VAL_REF
  | TYP_REF
  | EXC_REF
  | MOD_REF
  | MODT_REF
  | CLA_REF
  | CLT_REF
  | ATT_REF
  | MET_REF
  | SEC_REF
  | MOD_LIST_REF
  | INDEX_LIST
  | SUPERSCRIPT
  | SUBSCRIPT
  | BEGIN_SHORTCUT_LIST_ITEM
  | BEGIN_SHORTCUT_ENUM_ITEM
  | SHORTCUT_LIST_ITEM
  | SHORTCUT_ENUM_ITEM
  | END_SHORTCUT_LIST
  | BLANK_LINE
  | EOF
  | Char of (string)

open Parsing;;
# 2 "ocaml/ocamldoc/odoc_text_parser.mly"
(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Odoc_types

let identchar =
  "[A-Z a-z_\192-\214\216-\246\248-\255'0-9]"
let blank = "[ \010\013\009\012]"

let remove_beginning_blanks s =
  Str.global_replace (Str.regexp ("^"^blank^"+")) "" s

let remove_trailing_blanks s =
  Str.global_replace (Str.regexp (blank^"+$")) "" s

let print_DEBUG s = print_string s; print_newline ()
# 78 "ocaml/ocamldoc/odoc_text_parser.ml"
let yytransl_const = [|
  257 (* END *);
  259 (* BOLD *);
  260 (* EMP *);
  261 (* CENTER *);
  262 (* LEFT *);
  263 (* RIGHT *);
  264 (* ITALIC *);
  266 (* LIST *);
  267 (* ENUM *);
  268 (* ITEM *);
  269 (* LINK *);
  270 (* CODE *);
  271 (* END_CODE *);
  272 (* CODE_PRE *);
  273 (* END_CODE_PRE *);
  274 (* VERB *);
  275 (* END_VERB *);
  276 (* LATEX *);
  278 (* END_TARGET *);
  279 (* LBRACE *);
  280 (* ELE_REF *);
  281 (* VAL_REF *);
  282 (* TYP_REF *);
  283 (* EXC_REF *);
  284 (* MOD_REF *);
  285 (* MODT_REF *);
  286 (* CLA_REF *);
  287 (* CLT_REF *);
  288 (* ATT_REF *);
  289 (* MET_REF *);
  290 (* SEC_REF *);
  291 (* MOD_LIST_REF *);
  292 (* INDEX_LIST *);
  293 (* SUPERSCRIPT *);
  294 (* SUBSCRIPT *);
  295 (* BEGIN_SHORTCUT_LIST_ITEM *);
  296 (* BEGIN_SHORTCUT_ENUM_ITEM *);
  297 (* SHORTCUT_LIST_ITEM *);
  298 (* SHORTCUT_ENUM_ITEM *);
  299 (* END_SHORTCUT_LIST *);
  300 (* BLANK_LINE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  258 (* Title *);
  265 (* CUSTOM *);
  277 (* Target *);
  301 (* Char *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\004\000\004\000\002\000\002\000\006\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\008\000\
\008\000\008\000\008\000\008\000\012\000\010\000\010\000\013\000\
\011\000\011\000\014\000\009\000\009\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\002\000\001\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\006\000\003\000\001\000\003\000\003\000\003\000\
\005\000\001\000\003\000\003\000\003\000\003\000\001\000\001\000\
\002\000\002\000\001\000\002\000\003\000\002\000\001\000\002\000\
\002\000\001\000\002\000\001\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\010\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\000\000\
\037\000\000\000\000\000\000\000\000\000\042\000\002\000\000\000\
\062\000\000\000\003\000\000\000\000\000\047\000\063\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\000\001\000\005\000\000\000\
\007\000\020\000\021\000\024\000\027\000\028\000\029\000\022\000\
\023\000\000\000\030\000\050\000\000\000\000\000\031\000\000\000\
\032\000\033\000\038\000\039\000\040\000\000\000\036\000\025\000\
\026\000\000\000\054\000\043\000\044\000\000\000\057\000\045\000\
\046\000\034\000\053\000\000\000\000\000\056\000\059\000\041\000\
\000\000\035\000"

let yydgoto = "\003\000\
\041\000\047\000\073\000\043\000\044\000\049\000\045\000\059\000\
\046\000\074\000\076\000\061\000\107\000\111\000"

let yysindex = "\023\000\
\001\000\097\255\000\000\097\255\097\255\097\255\097\255\097\255\
\097\255\097\255\097\255\002\255\002\255\213\254\213\254\213\254\
\213\254\213\254\213\254\049\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\213\254\
\000\000\097\255\097\255\097\255\097\255\000\000\000\000\213\254\
\000\000\026\000\000\000\097\255\213\254\000\000\000\000\000\000\
\097\255\026\255\027\255\028\255\029\255\030\255\031\255\036\255\
\037\255\097\255\011\255\002\255\002\255\022\255\038\255\025\255\
\024\255\023\255\032\255\033\255\213\254\043\255\044\255\052\255\
\020\255\049\000\042\255\050\000\000\000\000\000\000\000\065\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\067\255\000\000\000\000\213\254\213\254\000\000\097\255\
\000\000\000\000\000\000\000\000\000\000\068\255\000\000\000\000\
\000\000\097\255\000\000\000\000\000\000\097\255\000\000\000\000\
\000\000\000\000\000\000\069\255\097\255\000\000\000\000\000\000\
\070\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\
\000\000\000\000\000\000\090\000\000\000\000\000\000\000\000\000\
\085\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\085\255\086\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\000\000\000\052\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\087\255\088\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\042\000\255\255\048\000\011\000\000\000\073\000\254\255\
\003\000\244\255\002\000\000\000\000\000\000\000"

let yytablesize = 389
let yytable = "\042\000\
\039\000\040\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\062\000\091\000\048\000\058\000\060\000\060\000\
\063\000\064\000\065\000\066\000\067\000\068\000\095\000\001\000\
\002\000\078\000\082\000\083\000\084\000\085\000\086\000\087\000\
\071\000\072\000\070\000\075\000\088\000\089\000\096\000\097\000\
\098\000\099\000\077\000\103\000\104\000\060\000\040\000\080\000\
\109\000\113\000\055\000\058\000\105\000\100\000\101\000\040\000\
\090\000\093\000\094\000\048\000\106\000\092\000\060\000\060\000\
\092\000\114\000\040\000\115\000\117\000\120\000\122\000\102\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\110\000\006\000\048\000\051\000\049\000\
\052\000\004\000\081\000\079\000\069\000\118\000\116\000\092\000\
\092\000\000\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\075\000\014\000\015\000\119\000\
\016\000\000\000\017\000\121\000\018\000\019\000\000\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\000\000\000\000\000\000\038\000\040\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\000\000\014\000\015\000\000\000\
\016\000\000\000\017\000\000\000\018\000\019\000\000\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\000\000\000\000\000\000\038\000\040\000\060\000\060\000\
\060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
\060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
\060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
\060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
\060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
\060\000\060\000\004\000\108\000\112\000\055\000\058\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\004\000\004\000"

let yycheck = "\001\000\
\000\000\045\001\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\013\000\001\001\002\000\012\001\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\019\000\001\001\001\000\
\002\000\000\000\001\001\001\001\001\001\001\001\001\001\001\001\
\034\000\035\000\032\000\037\000\001\001\001\001\001\001\015\001\
\017\001\019\001\040\000\001\001\001\001\000\000\045\001\045\000\
\000\000\000\000\000\000\000\000\001\001\022\001\022\001\045\001\
\058\000\060\000\061\000\049\000\041\001\059\000\060\000\061\000\
\062\000\001\001\045\001\001\001\001\001\001\001\001\001\069\000\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\042\001\000\000\001\001\001\001\001\001\
\001\001\000\000\049\000\044\000\020\000\106\000\096\000\093\000\
\094\000\255\255\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\110\000\013\001\014\001\110\000\
\016\001\255\255\018\001\117\000\020\001\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\255\255\255\255\255\255\044\001\045\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\255\255\
\016\001\255\255\018\001\255\255\020\001\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\255\255\255\255\255\255\044\001\045\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\042\001\
\043\001\044\001\001\001\043\001\043\001\043\001\043\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\041\001\042\001\043\001"

let yynames_const = "\
  END\000\
  BOLD\000\
  EMP\000\
  CENTER\000\
  LEFT\000\
  RIGHT\000\
  ITALIC\000\
  LIST\000\
  ENUM\000\
  ITEM\000\
  LINK\000\
  CODE\000\
  END_CODE\000\
  CODE_PRE\000\
  END_CODE_PRE\000\
  VERB\000\
  END_VERB\000\
  LATEX\000\
  END_TARGET\000\
  LBRACE\000\
  ELE_REF\000\
  VAL_REF\000\
  TYP_REF\000\
  EXC_REF\000\
  MOD_REF\000\
  MODT_REF\000\
  CLA_REF\000\
  CLT_REF\000\
  ATT_REF\000\
  MET_REF\000\
  SEC_REF\000\
  MOD_LIST_REF\000\
  INDEX_LIST\000\
  SUPERSCRIPT\000\
  SUBSCRIPT\000\
  BEGIN_SHORTCUT_LIST_ITEM\000\
  BEGIN_SHORTCUT_ENUM_ITEM\000\
  SHORTCUT_LIST_ITEM\000\
  SHORTCUT_ENUM_ITEM\000\
  END_SHORTCUT_LIST\000\
  BLANK_LINE\000\
  EOF\000\
  "

let yynames_block = "\
  Title\000\
  CUSTOM\000\
  Target\000\
  Char\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 89 "ocaml/ocamldoc/odoc_text_parser.mly"
           ( _1 )
# 375 "ocaml/ocamldoc/odoc_text_parser.ml"
               : Odoc_types.text))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "ocaml/ocamldoc/odoc_text_parser.mly"
      ( [Raw ""] )
# 381 "ocaml/ocamldoc/odoc_text_parser.ml"
               : Odoc_types.text))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text_element_list) in
    Obj.repr(
# 94 "ocaml/ocamldoc/odoc_text_parser.mly"
                    ( _1 )
# 388 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text_element) in
    Obj.repr(
# 98 "ocaml/ocamldoc/odoc_text_parser.mly"
               ( [ _1 ] )
# 395 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'text_element) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'text_element_list) in
    Obj.repr(
# 99 "ocaml/ocamldoc/odoc_text_parser.mly"
                                 ( _1 :: _2 )
# 403 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'located_element) in
    Obj.repr(
# 103 "ocaml/ocamldoc/odoc_text_parser.mly"
                  ( [ _1 ] )
# 410 "ocaml/ocamldoc/odoc_text_parser.ml"
               : (int * int * Odoc_types.text_element) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'located_element) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (int * int * Odoc_types.text_element) list) in
    Obj.repr(
# 104 "ocaml/ocamldoc/odoc_text_parser.mly"
                                       ( _1 :: _2 )
# 418 "ocaml/ocamldoc/odoc_text_parser.ml"
               : (int * int * Odoc_types.text_element) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text_element) in
    Obj.repr(
# 108 "ocaml/ocamldoc/odoc_text_parser.mly"
               ( Parsing.symbol_start (), Parsing.symbol_end (), _1)
# 425 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'located_element))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( None )
# 431 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some RK_value )
# 437 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some RK_type )
# 443 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some RK_exception )
# 449 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some RK_module )
# 455 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "ocaml/ocamldoc/odoc_text_parser.mly"
           ( Some RK_module_type )
# 461 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some RK_class )
# 467 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some RK_class_type )
# 473 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some RK_attribute )
# 479 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some RK_method )
# 485 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "ocaml/ocamldoc/odoc_text_parser.mly"
          ( Some (RK_section []))
# 491 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'ele_ref_kind))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int * string option) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 127 "ocaml/ocamldoc/odoc_text_parser.mly"
                 ( let n, l_opt = _1 in Title (n, l_opt, _2) )
# 499 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 128 "ocaml/ocamldoc/odoc_text_parser.mly"
                ( Bold _2 )
# 506 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 129 "ocaml/ocamldoc/odoc_text_parser.mly"
                  ( Italic _2 )
# 513 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 130 "ocaml/ocamldoc/odoc_text_parser.mly"
                  ( Custom (_1, _2) )
# 521 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 131 "ocaml/ocamldoc/odoc_text_parser.mly"
               ( Emphasize _2 )
# 528 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 132 "ocaml/ocamldoc/odoc_text_parser.mly"
                       ( Superscript _2 )
# 535 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 133 "ocaml/ocamldoc/odoc_text_parser.mly"
                     ( Subscript _2 )
# 542 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 134 "ocaml/ocamldoc/odoc_text_parser.mly"
                  ( Center _2 )
# 549 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 135 "ocaml/ocamldoc/odoc_text_parser.mly"
                ( Left _2 )
# 556 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 136 "ocaml/ocamldoc/odoc_text_parser.mly"
                 ( Right _2 )
# 563 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 137 "ocaml/ocamldoc/odoc_text_parser.mly"
                ( List _2 )
# 570 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 138 "ocaml/ocamldoc/odoc_text_parser.mly"
                ( Enum _2 )
# 577 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 139 "ocaml/ocamldoc/odoc_text_parser.mly"
                       ( Code _2 )
# 584 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 140 "ocaml/ocamldoc/odoc_text_parser.mly"
                               ( CodePre _2 )
# 591 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ele_ref_kind) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 141 "ocaml/ocamldoc/odoc_text_parser.mly"
                           (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, _1, None)
     )
# 603 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'ele_ref_kind) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 146 "ocaml/ocamldoc/odoc_text_parser.mly"
                                          (
      let s2 = remove_beginning_blanks _3 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, _2, Some _5)
    )
# 616 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 152 "ocaml/ocamldoc/odoc_text_parser.mly"
                          (
      let s2 = remove_beginning_blanks _2 in
      let s3 = remove_trailing_blanks s2 in
      let l = Odoc_misc.split_with_blanks s3 in
      Module_list l
     )
# 628 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "ocaml/ocamldoc/odoc_text_parser.mly"
             ( Index_list )
# 634 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 159 "ocaml/ocamldoc/odoc_text_parser.mly"
                       ( Verbatim _2 )
# 641 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 160 "ocaml/ocamldoc/odoc_text_parser.mly"
                          ( Latex _2 )
# 648 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    Obj.repr(
# 161 "ocaml/ocamldoc/odoc_text_parser.mly"
                           ( Target (_1, _2) )
# 656 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 162 "ocaml/ocamldoc/odoc_text_parser.mly"
                           ( Link (_2, _4) )
# 664 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    Obj.repr(
# 163 "ocaml/ocamldoc/odoc_text_parser.mly"
             ( Newline )
# 670 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'shortcut_list) in
    Obj.repr(
# 164 "ocaml/ocamldoc/odoc_text_parser.mly"
                                                           ( List _2 )
# 677 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'shortcut_list) in
    Obj.repr(
# 165 "ocaml/ocamldoc/odoc_text_parser.mly"
                                             ( List _2 )
# 684 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'shortcut_enum) in
    Obj.repr(
# 166 "ocaml/ocamldoc/odoc_text_parser.mly"
                                                           ( Enum _2 )
# 691 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'shortcut_enum) in
    Obj.repr(
# 167 "ocaml/ocamldoc/odoc_text_parser.mly"
                                             ( Enum _2 )
# 698 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 168 "ocaml/ocamldoc/odoc_text_parser.mly"
         ( Raw _1 )
# 705 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'text_element))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 172 "ocaml/ocamldoc/odoc_text_parser.mly"
         ( [] (* A VOIR : un test pour voir qu'il n'y a que des blancs *) )
# 712 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 173 "ocaml/ocamldoc/odoc_text_parser.mly"
              ( _2 )
# 720 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 174 "ocaml/ocamldoc/odoc_text_parser.mly"
               ( _1 )
# 728 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'item) in
    Obj.repr(
# 175 "ocaml/ocamldoc/odoc_text_parser.mly"
       ( [ _1 ] )
# 735 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'item) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 176 "ocaml/ocamldoc/odoc_text_parser.mly"
            ( _1 :: _2 )
# 743 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    Obj.repr(
# 181 "ocaml/ocamldoc/odoc_text_parser.mly"
                  ( _2 )
# 750 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'shortcut_list2) in
    Obj.repr(
# 185 "ocaml/ocamldoc/odoc_text_parser.mly"
                         ( _1 :: _2 )
# 758 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'shortcut_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text) in
    Obj.repr(
# 186 "ocaml/ocamldoc/odoc_text_parser.mly"
       ( [ _1 ] )
# 765 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'shortcut_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'shortcut_list) in
    Obj.repr(
# 190 "ocaml/ocamldoc/odoc_text_parser.mly"
                                   ( _2 )
# 772 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'shortcut_list2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'text) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'shortcut_enum2) in
    Obj.repr(
# 194 "ocaml/ocamldoc/odoc_text_parser.mly"
                         ( _1 :: _2 )
# 780 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'shortcut_enum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'text) in
    Obj.repr(
# 195 "ocaml/ocamldoc/odoc_text_parser.mly"
       ( [ _1 ] )
# 787 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'shortcut_enum))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'shortcut_enum) in
    Obj.repr(
# 199 "ocaml/ocamldoc/odoc_text_parser.mly"
                                   ( _2 )
# 794 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'shortcut_enum2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 204 "ocaml/ocamldoc/odoc_text_parser.mly"
         ( _1 )
# 801 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'string) in
    Obj.repr(
# 205 "ocaml/ocamldoc/odoc_text_parser.mly"
              ( _1^_2 )
# 809 "ocaml/ocamldoc/odoc_text_parser.ml"
               : 'string))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry located_element_list *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Odoc_types.text)
let located_element_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : (int * int * Odoc_types.text_element) list)
;;
