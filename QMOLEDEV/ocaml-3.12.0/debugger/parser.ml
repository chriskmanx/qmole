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

open Parsing;;
# 17 "parser.mly"

open Int64ops
open Input_handling
open Longident
open Parser_aux

# 29 "parser.ml"
let yytransl_const = [|
  262 (* STAR *);
  263 (* MINUS *);
  264 (* DOT *);
  265 (* SHARP *);
  266 (* AT *);
  267 (* DOLLAR *);
  268 (* BANG *);
  269 (* LPAREN *);
  270 (* RPAREN *);
  271 (* LBRACKET *);
  272 (* RBRACKET *);
  273 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* ARGUMENT *);
  258 (* LIDENT *);
  259 (* UIDENT *);
  260 (* OPERATOR *);
  261 (* INTEGER *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\004\000\005\000\006\000\
\007\000\007\000\022\000\022\000\008\000\008\000\009\000\009\000\
\023\000\023\000\023\000\024\000\024\000\019\000\020\000\020\000\
\020\000\020\000\021\000\010\000\010\000\011\000\012\000\012\000\
\013\000\013\000\014\000\025\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\015\000\015\000\016\000\016\000\
\016\000\016\000\016\000\017\000\017\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\002\000\001\000\002\000\002\000\001\000\
\002\000\001\000\002\000\001\000\002\000\001\000\002\000\001\000\
\001\000\003\000\001\000\001\000\003\000\002\000\001\000\001\000\
\003\000\000\000\002\000\001\000\001\000\002\000\001\000\001\000\
\001\000\000\000\002\000\001\000\001\000\002\000\003\000\005\000\
\005\000\003\000\002\000\003\000\002\000\001\000\001\000\001\000\
\002\000\004\000\004\000\003\000\001\000\001\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
\055\000\002\000\000\000\056\000\000\000\057\000\005\000\000\000\
\058\000\000\000\059\000\008\000\060\000\000\000\061\000\010\000\
\000\000\014\000\062\000\000\000\000\000\063\000\012\000\016\000\
\028\000\029\000\064\000\000\000\065\000\031\000\066\000\032\000\
\033\000\067\000\000\000\068\000\017\000\020\000\019\000\037\000\
\000\000\000\000\000\000\069\000\046\000\036\000\000\000\000\000\
\000\000\048\000\070\000\047\000\000\000\024\000\000\000\071\000\
\000\000\053\000\000\000\072\000\073\000\000\000\074\000\000\000\
\075\000\001\000\003\000\004\000\006\000\007\000\009\000\013\000\
\011\000\015\000\030\000\035\000\038\000\043\000\000\000\000\000\
\000\000\045\000\000\000\049\000\000\000\027\000\000\000\022\000\
\044\000\018\000\021\000\042\000\039\000\000\000\000\000\000\000\
\000\000\052\000\000\000\000\000\000\000\050\000\051\000\041\000\
\040\000"

let yydgoto = "\022\000\
\025\000\028\000\030\000\033\000\035\000\037\000\039\000\043\000\
\046\000\057\000\053\000\055\000\058\000\060\000\068\000\075\000\
\080\000\040\000\085\000\081\000\082\000\048\000\070\000\071\000\
\072\000"

let yysindex = "\112\000\
\008\255\004\255\025\255\007\255\045\255\048\255\035\255\019\255\
\024\255\064\255\064\255\016\255\064\255\064\255\132\255\068\255\
\073\255\043\255\102\255\073\255\073\255\000\000\008\255\000\000\
\000\000\000\000\043\255\000\000\025\255\000\000\000\000\043\255\
\000\000\043\255\000\000\000\000\000\000\043\255\000\000\000\000\
\007\255\000\000\000\000\043\255\045\255\000\000\000\000\000\000\
\000\000\000\000\000\000\043\255\000\000\000\000\000\000\000\000\
\000\000\000\000\043\255\000\000\000\000\000\000\000\000\000\000\
\082\255\144\255\144\255\000\000\000\000\000\000\061\255\086\255\
\073\255\000\000\000\000\000\000\005\255\000\000\000\000\000\000\
\039\255\000\000\087\255\000\000\000\000\043\255\000\000\043\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\255\081\255\
\033\255\000\000\053\255\000\000\035\255\000\000\093\255\000\000\
\000\000\000\000\000\000\000\000\000\000\095\255\097\255\035\255\
\007\255\000\000\000\000\123\255\124\255\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\111\000\122\255\000\000\000\000\
\042\255\000\000\000\000\141\000\125\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\056\255\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\128\000\000\000\123\000\242\255\108\000\073\000\248\255\000\000\
\000\000\098\000\000\000\000\000\144\000\000\000\087\000\000\000\
\000\000\005\000\000\000\251\255\139\000\000\000\142\000\246\255\
\244\255"

let yytablesize = 276
let yytable = "\042\000\
\023\000\074\000\025\000\077\000\027\000\026\000\083\000\031\000\
\023\000\083\000\083\000\032\000\105\000\047\000\087\000\088\000\
\056\000\049\000\050\000\069\000\076\000\024\000\084\000\038\000\
\024\000\041\000\096\000\026\000\044\000\029\000\045\000\091\000\
\024\000\031\000\116\000\024\000\093\000\117\000\094\000\038\000\
\024\000\024\000\095\000\036\000\105\000\118\000\026\000\119\000\
\097\000\034\000\113\000\024\000\036\000\102\000\103\000\024\000\
\099\000\120\000\026\000\024\000\026\000\121\000\083\000\100\000\
\026\000\049\000\050\000\107\000\104\000\061\000\062\000\063\000\
\032\000\064\000\078\000\079\000\069\000\073\000\065\000\066\000\
\067\000\108\000\114\000\115\000\024\000\110\000\101\000\061\000\
\062\000\063\000\112\000\064\000\110\000\105\000\111\000\123\000\
\065\000\066\000\067\000\124\000\122\000\125\000\024\000\061\000\
\062\000\063\000\127\000\051\000\052\000\054\000\034\000\126\000\
\001\000\002\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\061\000\062\000\063\000\
\128\000\064\000\034\000\129\000\026\000\026\000\065\000\066\000\
\067\000\061\000\062\000\063\000\024\000\064\000\090\000\092\000\
\098\000\109\000\065\000\066\000\067\000\059\000\106\000\089\000\
\086\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\023\000\000\000\025\000\
\020\000\023\000\021\000\025\000\000\000\000\000\000\000\000\000\
\000\000\023\000\000\000\025\000"

let yycheck = "\008\000\
\000\000\016\000\000\000\016\000\001\001\001\000\017\000\003\000\
\001\001\020\000\021\000\005\001\008\001\009\000\020\000\021\000\
\012\000\002\001\003\001\015\000\016\000\017\001\018\000\005\001\
\017\001\007\001\041\000\023\000\005\001\005\001\007\001\027\000\
\017\001\029\000\002\001\017\001\032\000\005\001\034\000\005\001\
\017\001\017\001\038\000\005\001\008\001\013\001\005\001\015\001\
\044\000\005\001\014\001\017\001\005\001\066\000\067\000\017\001\
\052\000\005\001\017\001\017\001\005\001\009\001\073\000\059\000\
\009\001\002\001\003\001\073\000\008\001\002\001\003\001\004\001\
\005\001\006\001\002\001\003\001\072\000\010\001\011\001\012\001\
\013\001\077\000\002\001\003\001\017\001\081\000\005\001\002\001\
\003\001\004\001\086\000\006\001\088\000\008\001\008\001\003\001\
\011\001\012\001\013\001\005\001\109\000\005\001\017\001\002\001\
\003\001\004\001\121\000\010\000\011\000\012\000\000\000\120\000\
\001\000\002\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\002\001\003\001\004\001\
\014\001\006\001\017\001\016\001\000\000\017\001\011\001\012\001\
\013\001\002\001\003\001\004\001\017\001\006\001\023\000\029\000\
\045\000\081\000\011\001\012\001\013\001\014\000\072\000\021\000\
\019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\005\001\255\255\005\001\
\008\001\009\001\008\001\009\001\255\255\255\255\255\255\255\255\
\255\255\017\001\255\255\017\001"

let yynames_const = "\
  STAR\000\
  MINUS\000\
  DOT\000\
  SHARP\000\
  AT\000\
  DOLLAR\000\
  BANG\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  EOL\000\
  "

let yynames_block = "\
  ARGUMENT\000\
  LIDENT\000\
  UIDENT\000\
  OPERATOR\000\
  INTEGER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string list) in
    Obj.repr(
# 115 "parser.mly"
      ( _1::_2 )
# 252 "parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 117 "parser.mly"
      ( [] )
# 259 "parser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 121 "parser.mly"
      ( _1 )
# 267 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int list) in
    Obj.repr(
# 127 "parser.mly"
      ( (to_int _1) :: _2 )
# 275 "parser.ml"
               : int list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 129 "parser.mly"
      ( [] )
# 282 "parser.ml"
               : int list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 133 "parser.mly"
      ( to_int _1 )
# 290 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 137 "parser.mly"
      ( _1 )
# 298 "parser.ml"
               : int64))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 141 "parser.mly"
      ( to_int _1 )
# 305 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 145 "parser.mly"
      ( Some (to_int _1) )
# 313 "parser.ml"
               : int option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 147 "parser.mly"
      ( None )
# 320 "parser.ml"
               : int option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 151 "parser.mly"
      ( Some _1 )
# 328 "parser.ml"
               : 'opt_int64_eol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 153 "parser.mly"
      ( None )
# 335 "parser.ml"
               : 'opt_int64_eol))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 157 "parser.mly"
      ( Some (- _2) )
# 342 "parser.ml"
               : int option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int option) in
    Obj.repr(
# 159 "parser.mly"
      ( _1 )
# 349 "parser.ml"
               : int option))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 163 "parser.mly"
      ( Some (Int64.neg _2) )
# 356 "parser.ml"
               : int64 option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'opt_int64_eol) in
    Obj.repr(
# 165 "parser.mly"
      ( _1 )
# 363 "parser.ml"
               : int64 option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 170 "parser.mly"
                                ( Lident _1 )
# 370 "parser.ml"
               : 'longident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'module_path) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 171 "parser.mly"
                                ( Ldot(_1, _3) )
# 378 "parser.ml"
               : 'longident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 172 "parser.mly"
                                ( Lident _1 )
# 385 "parser.ml"
               : 'longident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 176 "parser.mly"
                                ( Lident _1 )
# 392 "parser.ml"
               : 'module_path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'module_path) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 177 "parser.mly"
                                ( Ldot(_1, _3) )
# 400 "parser.ml"
               : 'module_path))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'longident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 181 "parser.mly"
                                ( _1 )
# 408 "parser.ml"
               : Longident.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "parser.mly"
                                ( Some (Lident _1) )
# 415 "parser.ml"
               : Longident.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 185 "parser.mly"
                                ( Some (Lident _1) )
# 422 "parser.ml"
               : Longident.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'module_path) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 186 "parser.mly"
                                ( Some (Ldot(_1, _3)) )
# 430 "parser.ml"
               : Longident.t option))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "parser.mly"
                                ( None )
# 436 "parser.ml"
               : Longident.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Longident.t option) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 190 "parser.mly"
                                ( _1 )
# 444 "parser.ml"
               : Longident.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 193 "parser.mly"
                                ( _1 )
# 451 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 194 "parser.mly"
                                ( _1 )
# 458 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 197 "parser.mly"
                                ( _1 )
# 466 "parser.ml"
               : string))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 200 "parser.mly"
                                ( Some _1 )
# 473 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 201 "parser.mly"
                                ( None )
# 480 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 204 "parser.mly"
                                ( Some _1 )
# 487 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    Obj.repr(
# 205 "parser.mly"
                                ( None )
# 493 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string option) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 208 "parser.mly"
                                ( _1 )
# 501 "parser.ml"
               : string option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'longident) in
    Obj.repr(
# 213 "parser.mly"
                                               ( E_ident _1 )
# 508 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 214 "parser.mly"
                                                ( E_result )
# 514 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 215 "parser.mly"
                                                ( E_name (to_int _2) )
# 521 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int64) in
    Obj.repr(
# 216 "parser.mly"
                                                ( E_item(_1, (to_int _3)) )
# 529 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    Obj.repr(
# 217 "parser.mly"
                                                ( E_item(_1, (to_int _4)) )
# 537 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    Obj.repr(
# 218 "parser.mly"
                                                ( E_item(_1, (to_int _4)) )
# 545 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 219 "parser.mly"
                                                ( E_field(_1, _3) )
# 553 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 220 "parser.mly"
                                                ( E_field(_2, "contents") )
# 560 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 221 "parser.mly"
                                                ( _2 )
# 567 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Parser_aux.expression list) in
    Obj.repr(
# 227 "parser.mly"
                                                ( _1::_2 )
# 575 "parser.ml"
               : Parser_aux.expression list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 228 "parser.mly"
                                                ( [] )
# 582 "parser.ml"
               : Parser_aux.expression list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 234 "parser.mly"
                                                ( BA_none )
# 589 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 235 "parser.mly"
                                                ( BA_pc _1 )
# 596 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : unit) in
    Obj.repr(
# 236 "parser.mly"
                                                ( BA_function _1 )
# 604 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Longident.t option) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int option) in
    Obj.repr(
# 237 "parser.mly"
                                                ( BA_pos1 (_2, (to_int _3), _4))
# 613 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Longident.t option) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 238 "parser.mly"
                                                ( BA_pos2 (_2, _4) )
# 621 "parser.ml"
               : Parser_aux.break_arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Longident.t option) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int option) in
    Obj.repr(
# 245 "parser.mly"
      ( (_1, Some _2, _3) )
# 630 "parser.ml"
               : Longident.t option * int option * int option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Longident.t option) in
    Obj.repr(
# 247 "parser.mly"
      ( (_1, None, None) )
# 637 "parser.ml"
               : Longident.t option * int option * int option))
; (fun __caml_parser_env ->
    Obj.repr(
# 252 "parser.mly"
        ( stop_user_input () )
# 643 "parser.ml"
               : unit))
(* Entry argument_list_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry argument_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry integer_list_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry integer_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry int64_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry integer *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_integer_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_signed_integer_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_signed_int64_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry identifier *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry identifier_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry identifier_or_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_identifier *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_identifier_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry expression_list_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry break_argument_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry list_arguments_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry end_of_line *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry longident_eol *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_longident *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry opt_longident_eol *)
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
let argument_list_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : string list)
let argument_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : string)
let integer_list_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : int list)
let integer_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : int)
let int64_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 5 lexfun lexbuf : int64)
let integer (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 6 lexfun lexbuf : int)
let opt_integer_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 7 lexfun lexbuf : int option)
let opt_signed_integer_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 8 lexfun lexbuf : int option)
let opt_signed_int64_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 9 lexfun lexbuf : int64 option)
let identifier (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 10 lexfun lexbuf : string)
let identifier_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 11 lexfun lexbuf : string)
let identifier_or_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 12 lexfun lexbuf : string option)
let opt_identifier (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 13 lexfun lexbuf : string option)
let opt_identifier_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 14 lexfun lexbuf : string option)
let expression_list_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 15 lexfun lexbuf : Parser_aux.expression list)
let break_argument_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 16 lexfun lexbuf : Parser_aux.break_arg)
let list_arguments_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 17 lexfun lexbuf : Longident.t option * int option * int option)
let end_of_line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 18 lexfun lexbuf : unit)
let longident_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 19 lexfun lexbuf : Longident.t)
let opt_longident (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 20 lexfun lexbuf : Longident.t option)
let opt_longident_eol (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 21 lexfun lexbuf : Longident.t option)
