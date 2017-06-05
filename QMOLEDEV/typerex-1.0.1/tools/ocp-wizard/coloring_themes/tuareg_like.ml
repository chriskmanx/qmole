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
    | TYPE
    | MODULE | STRUCT | SIG
    | CLASS | OBJECT
    | END
    | FUNCTOR
    | BEGIN
    | CONSTRAINT
    | VIRTUAL | METHOD
    | INHERIT | INITIALIZER
    | OPEN | INCLUDE 
      -> `typerex `governing

    | WITH when match after with
        | (MODULE | TYPE) :: _ -> true
        | _ -> false
      -> `typerex `governing

    | NEW
    | MUTABLE | PRIVATE
    | FUN | FUNCTION | MATCH | TRY | WITH | WHEN | AS
    | IF | THEN | ELSE
    | FOR | TO | DOWNTO | WHILE | DO | DONE
    | EXCEPTION
    | ASSERT | LAZY
      -> `font_lock `keyword

    | OF
    | MINUSGREATER | BAR | SHARP | COLONGREATER
    | LPAREN | RPAREN | LBRACE | RBRACE
    | LBRACKET | RBRACKET | COLONCOLON | LBRACKETBAR | BARRBRACKET
    | GREATERRBRACE | LBRACELESS
    | LBRACKETLESS | LBRACKETGREATER | GREATERRBRACKET
    | SEMI | COLON | COMMA | DOTDOT | INFIXOP1 _
    | AMPERAMPER | BARBAR | AMPERSAND | OR
    | PLUS | PLUSDOT | MINUS | MINUSDOT | STAR
    | LESS | GREATER | EQUAL
    | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _
    | INFIXOP0 _ | PREFIXOP _
    | COLONEQUAL | BANG | LESSMINUS
    | QUOTE | TILDE | QUESTION | QUESTIONQUESTION
    | SEMISEMI
    | LIDENT ("ref" | "not")
        -> `typerex `operator

    | LIDENT ("parser" | "raise" | "failwith" | "exit")
        -> `font_lock `keyword

    | UNDERSCORE | DOT | BACKQUOTE
    | FLOAT _ | INT _ | INT32 _ | INT64 _ | NATIVEINT _
      -> raise Not_found

    | FALSE | TRUE -> `font_lock `constant
    | UIDENT _
      ->
      (match classify_uid before after with
        | `def, `exn -> `font_lock `variable_name
        | `def, `modname (`arg | `local) -> `font_lock `variable_name
        | _, `modname _ -> `font_lock `ftype
        (* TODO: functor arguments *)
        | _ -> raise Not_found)
    | LIDENT _ ->
      (match classify_lid before after with
        | _, `tvar -> `font_lock `ftype
        (* TODO: type references after ':' *)
        | `def, `field -> `font_lock `variable_name
        | `occ, `field -> raise Not_found
        | `def, `methodname -> `font_lock `function_name
        | `def, `value `func -> `font_lock `function_name
        | `def, `value `value -> `font_lock `variable_name
        | _, `tconstr -> `font_lock `ftype
        | _, `arglabel -> `font_lock `variable_name
        | _ -> raise Not_found)
    | LABEL _ | OPTLABEL _ -> `font_lock `variable_name

    | STRING _ | CHAR _ -> `font_lock `string

    | COMMENT _
    | EOF -> raise Not_found

let _ =
  register_token2face "tuareg_like" (fun t ~before ~after  ->
    match t.token with
      | Token tok -> token2face tok before after
      | Comment ->
        (match classify_comment t.string with
          | `normal -> `font_lock `comment
          | `ocamldoc | `ocamldoc_stop -> `font_lock `doc)
      | Error e -> `typerex `error
      | White_space | Pad -> raise Not_found
  )
