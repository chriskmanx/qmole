(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                           Tiphaine Turpin                              *)
(*                                                                        *)
(*  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Colorize
open OcamlTokenize
open Parser

let token2face token before after =
  match token with
    | LET | REC | AND | IN | VAL | EXTERNAL
    | TYPE | CONSTRAINT | MODULE | FUNCTOR
    | CLASS | VIRTUAL | METHOD | INHERIT | INITIALIZER
    | MUTABLE | PRIVATE| FUN | FUNCTION | AS | OF
    | EXCEPTION
      -> `font_lock `ftype


    | STRUCT | SIG | OBJECT | BEGIN | END
      -> `font_lock `keyword

    | OPEN | INCLUDE | ASSERT
    | QUOTE | TILDE | QUESTION
      -> `font_lock `variable_name

    | MATCH | TRY | WITH | WHEN | NEW
    | IF | THEN | ELSE
    | FOR | TO | DOWNTO | WHILE | DO | DONE
    | LAZY
    | MINUSGREATER | BAR | SHARP
    | AMPERAMPER | BARBAR | AMPERSAND | OR
      -> `font_lock `constant

    | COLONGREATER
    | LPAREN | RPAREN | LBRACE | RBRACE
    | LBRACKET | RBRACKET | COLONCOLON | LBRACKETBAR | BARRBRACKET

    | GREATERRBRACE | LBRACELESS
    | LBRACKETLESS | LBRACKETGREATER | GREATERRBRACKET

    | SEMI | COLON | COMMA | DOTDOT | INFIXOP1 _
    | PLUS | PLUSDOT | MINUS | MINUSDOT | STAR
    | LESS | GREATER | EQUAL
    | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _
    | INFIXOP0 _ | PREFIXOP _
    | COLONEQUAL | BANG | LESSMINUS
    | QUESTIONQUESTION
    | SEMISEMI

    | UNDERSCORE | DOT | BACKQUOTE
    | FLOAT _ | INT _ | INT32 _ | INT64 _ | NATIVEINT _
    | FALSE | TRUE

      -> raise Not_found

    | LIDENT ( "raise" | "failwith")
        -> `font_lock `comment
    | LIDENT _ ->
      (match classify_lid before after with
        | _, `arglabel -> `font_lock `variable_name
        | _ -> raise Not_found)
    | LABEL _ | OPTLABEL _ -> `font_lock `variable_name

    | UIDENT _
      -> `font_lock `function_name

    | STRING _ | CHAR _ -> `font_lock `string

    | COMMENT _
    | EOF -> raise Not_found

let _ =
  register_token2face "caml_like" (fun t ~before ~after  ->
    match t.token with
      | Token tok -> token2face tok before after
      | Comment ->
        (match classify_comment t.string with
          | `normal -> `font_lock `comment
          | `ocamldoc -> `caml `doccomment
          | `ocamldoc_stop -> `caml `stop)
      | Error e -> `typerex `error
      | White_space | Pad -> raise Not_found
  )

















