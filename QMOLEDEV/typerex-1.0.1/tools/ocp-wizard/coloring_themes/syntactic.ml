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

open OcpLang
open Colorize
open OcamlTokenize
open Parser

let governing = `ocp "governing"
let keyword = `ocp "keyword"
let open_include = `ocp "open-include"
let punctuation = `ocp "punctuation"
let op_field = `ocp "op-field"
let bar_case = `ocp "bar-case"
let func = `ocp "func"
let op = `ocp "op"
let op_bool = `ocp "op-bool"
let constant = `ocp "constant"
let string = `ocp "string"
let tag = `ocp "tag"
let type_def = `ocp "type-def"
let type_occ = `ocp "type-occ"
let type_variable = `ocp "type-variable"
let cstr_def = `ocp "cstr-def"
let cstr_occ = `ocp "cstr-occ"
let field_def = `ocp "field-def"
let field_occ = `ocp "field-occ"
let variant = `ocp "variant"
let val_def = `ocp "val-def"
let val_def_fun = `ocp "val-def-fun"
let val_occ_fun = `ocp "val-occ-fun"
let val_occ = `ocp "val-occ"
let method_def = `ocp "method-def"
let method_occ = `ocp "method-occ"
let mod_def = `ocp "mod-def"
let mod_occ = `ocp "mod-occ"
let builtin = `ocp "builtin"
let comment = `ocp "comment"
let doc = `ocp "doc"
let doc_title = `ocp "doc-title"
let doc_italic = `ocp "doc-italic"
let doc_bold = `ocp "doc-bold"
let doc_stop = `ocp "doc-stop"
let doc_keyword = `ocp "doc-keyword"
let error = `ocp "error"

let token2face token before after =
  match token with

    | LET | REC | AND | MODULE | TYPE | VAL ->
      (match
          classify_let ~max:4 ~after (),
          classify_module_type_val token ~before ~after
       with
        | _, (`with_constraint | `modtype_of | `tvar | `package)
        | `inner, _ -> keyword
        | _ -> governing)

    | EXTERNAL
    | EXCEPTION
    | STRUCT | SIG
    | CLASS | OBJECT
    | END | SEMISEMI
    | FUNCTOR
    | BEGIN
      -> governing

    | IN
    | OF | CONSTRAINT
    | VIRTUAL | METHOD
    | INHERIT | INITIALIZER | NEW
    | MUTABLE | PRIVATE
    | FUN | FUNCTION | MATCH | TRY | WITH | WHEN | AS
    | IF | THEN | ELSE
    | FOR | TO | DOWNTO | WHILE | DO | DONE
    | ASSERT
      -> keyword

    | BAR ->
      (match classify_bar ~before with
        | `matching -> bar_case
        | `other -> punctuation)

    | MINUSGREATER | SHARP | COLONGREATER
      -> func

    | LPAREN | RPAREN | LBRACE | RBRACE
    | LBRACKET | RBRACKET | COLONCOLON | LBRACKETBAR | BARRBRACKET
    | GREATERRBRACE | LBRACELESS
    | LBRACKETLESS | LBRACKETGREATER | GREATERRBRACKET
    | SEMI | COLON | COMMA | DOTDOT | INFIXOP1 _
    | UNDERSCORE
      -> punctuation

    | DOT | LESSMINUS | BANG | COLONEQUAL
      -> op_field

    | AMPERAMPER | BARBAR | AMPERSAND | OR
      -> op_bool

    | PLUS | PLUSDOT | MINUS | MINUSDOT | STAR
    | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _
    | LESS | GREATER | EQUAL | INFIXOP0 _ | PREFIXOP _
      -> op

    | OPEN | INCLUDE
      -> open_include

    | QUOTE | BACKQUOTE | TILDE | QUESTION
      -> tag

    | FALSE | TRUE | LAZY
      -> builtin
    | UIDENT _
      ->
      (match classify_uid ~before ~after with
        | `def, (`cstr | `exn) -> cstr_def
        | `occ, (`cstr | `exn) -> cstr_occ
        | `def, `modname _ -> mod_def
        | `occ, `modname _ -> mod_occ)
    | LIDENT _ | LABEL _ | OPTLABEL _ ->
      (match classify_lid ~before ~after with
        | _, `tvar -> type_variable
        | _, `variant -> variant
        | `def, `field -> field_def
        | `occ, `field -> field_occ
        | `def, `methodname -> method_def
        | `occ, `methodname -> method_occ
        | `def, `value `func -> val_def_fun
        | `def, `value `value -> val_def
        | `occ, `value `func -> val_occ_fun
        | `occ, `value `value
        | _, `arglabel -> val_occ
        | `def, `tconstr -> type_def
        | `occ, `tconstr -> type_occ)

    | STRING _
      -> string
    | CHAR _ | FLOAT _ | INT _ | INT32 _ | INT64 _ | NATIVEINT _
      -> constant

    | QUESTIONQUESTION
    | EOF
    | COMMENT _
      -> raise Not_found

open Odoc_types

let ocamldoc2faces b e contents =
  try
    let contents = String.sub contents 3 (String.length contents - 5) in
    let elts = parse_comment contents in
(*
    let elts =
      match (Odoc_comments.info_of_string contents).Odoc_types.i_desc with
        | Some elts -> elts
        | None -> []
    in
*)
    (b, b+3, doc) :: (e-2, e, doc) ::
      List.concat
      (List.map
         (function b', e', elt ->
           let b = b+3+b'
           and e = b+3+e' in
           try
(*             let open Odoc_types in *)
             match elt with
               | Raw _ -> [b, e, doc]
               | Title _ -> [b, e, doc_title]
               | Bold _ -> [b, e, doc_bold]
               | Emphasize _
               | Italic _ -> [b, e, doc_italic]
               | Latex _ -> [b, e, keyword]
               | Newline
               | Verbatim _ -> []
               | Code _
               | CodePre _
               | Center _
               | Left _
               | Right _
               | List _
               | Enum _
               | Block _
               | Link _
               | Ref _
               | Superscript _
               | Subscript _
               | Module_list _
               | Index_list
               | Custom _
               | Target _
                 -> [b, e, doc_keyword]
           with _ -> [])
         elts)
  with _ ->
    [b, e, doc]

let _ =
  register_token2faces "syntactic" (fun t ~before ~after b e ->
    match t.token with
      | Token tok -> [b, e, token2face tok before after]
      | Comment ->
        (match classify_comment t.string with
          | `normal -> [b, e, comment]
          | `ocamldoc -> ocamldoc2faces b e t.string
          | `ocamldoc_stop -> [b, e, doc_stop])
      | Error _ -> [b, e, error]
      | White_space | Pad -> raise Not_found
  )
