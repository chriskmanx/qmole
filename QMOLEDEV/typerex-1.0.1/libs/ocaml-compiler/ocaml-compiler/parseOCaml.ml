(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                 Thomas Gazagnaire, Fabrice Le Fessant                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open OcpSystem
open Location
open Parsetree

(* let comments = ref []  *)

type error =
    Lexer_Found_Unterminated_comment
  | Lexer_Found_Unterminated_string
  | Lexer_Found_Unterminated_string_in_comment
  | Lexer_Found_Illegal_character of char
  | Lexer_Found_Literal_overflow of string
  | Lexer_Found_Keyword_as_label of string
  | Lexer_Found_Illegal_escape of string

  | Parser_Found_Applicative_path (* raised when not -applicative-functors *)
  | Parser_Found_Parse_error
  | Parser_Found_Unclosed of string * Location.t * string

exception Error of error * Location.t



let error err loc = raise (Error (err, loc))

let wrap parsing_fun location lexbuf =
  try
    Lexer.init ();
    Location.input_name := location;
    Location.init lexbuf location;
    let ast = parsing_fun Lexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with
  | Lexer.Error(err, loc) ->
    let err = match err with
        Lexer.Unterminated_comment _ -> Lexer_Found_Unterminated_comment
      | Lexer.Unterminated_string -> Lexer_Found_Unterminated_string
      | Lexer.Unterminated_string_in_comment _ ->
        Lexer_Found_Unterminated_string_in_comment
      | Lexer.Illegal_character c ->
        Lexer_Found_Illegal_character c
      | Lexer.Literal_overflow kind ->
        Lexer_Found_Literal_overflow kind
      | Lexer.Keyword_as_label label ->
        Lexer_Found_Keyword_as_label label
      | Lexer.Illegal_escape s ->
        Lexer_Found_Illegal_escape s
    in
    error err loc
  | Syntaxerr.Error err ->
    begin match err with
        Syntaxerr.Applicative_path loc ->
          error (Parser_Found_Applicative_path) loc
      | Syntaxerr.Unclosed (loc1, s1, loc2, s2) ->
        error (Parser_Found_Unclosed (s1, loc1, s2)) loc2
      | Syntaxerr.Other loc ->
        error Parser_Found_Parse_error loc
    end
  | Parsing.Parse_error
  | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      error Parser_Found_Parse_error loc
;;

module Raw = struct

  let structure = wrap Parser.implementation
  let signature = wrap Parser.interface

  let structure_of_string location str =
    structure location ( Lexing.from_string str )

  let structure_of_file filename =
    let str = File.X.read_to_string filename in
    let lexbuf = Lexing.from_string str in
    structure (File.to_string filename) lexbuf

  let signature_of_string filename str =
    signature filename  ( Lexing.from_string str )

  let signature_of_file filename =
    let str = File.X.read_to_string filename in
    let lexbuf = Lexing.from_string str in
    signature (File.to_string filename) lexbuf


  let expression_of_string filename str =
    match structure_of_string filename str with
      | [{ pstr_desc = Pstr_eval e }] -> e
    | _                             -> failwith "expression_of_string"
end

open Format
  let report_error ppf err loc =
        let report ppf err =
          Location.print_error ppf loc;
          match err with
            | Parser_Found_Unclosed (opening, closing_loc, closing) ->
              fprintf ppf "Syntax error: '%s' expected@." closing;
              fprintf ppf "%aThis '%s' might be unmatched"
                Location.print_error loc opening
            | Parser_Found_Applicative_path ->
              fprintf ppf "Syntax error: applicative paths of the form F(X).t are not supported when the option -no-app-func is set."
            | Lexer_Found_Illegal_character c ->
              fprintf ppf "Illegal character (%s)" (Char.escaped c)
            | Lexer_Found_Illegal_escape s ->
              fprintf ppf "Illegal backslash escape in string or character (%s)" s
            | Lexer_Found_Unterminated_comment ->
              fprintf ppf "Comment not terminated"
            | Lexer_Found_Unterminated_string ->
              fprintf ppf "String literal not terminated"
            | Lexer_Found_Unterminated_string_in_comment ->
              fprintf ppf "This comment contains an unterminated string literal"
            | Lexer_Found_Keyword_as_label kwd ->
              fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
            | Lexer_Found_Literal_overflow ty ->
              fprintf ppf "Integer literal exceeds the range of representable integers of type %s" ty
            | Parser_Found_Parse_error ->
              fprintf ppf "Syntax error"
        in
        fprintf ppf "@[%a@]@." report err

module Safe = struct

  let error_wrap f =
    try
      let s = f () in
      s
    with Error (err, loc) ->
      report_error Format.err_formatter err loc;
      exit 2

  let structure filename lexbuf =
    error_wrap (fun _ -> Raw.structure filename lexbuf)
  let signature filename lexbuf =
    error_wrap (fun _ -> Raw.signature filename lexbuf)
  let structure_of_file filename = error_wrap
    (fun _ -> Raw.structure_of_file filename)
  let signature_of_file filename = error_wrap
    (fun _ -> Raw.signature_of_file filename)
  let structure_of_string filename str = error_wrap
    (fun _ -> Raw.structure_of_string filename str)
  let signature_of_string filename str = error_wrap
    (fun _ -> Raw.signature_of_string filename str)
end

open Lexing

let get_c_num loc = (* was in Location *)
  loc.loc_start.pos_cnum, loc.loc_end.pos_cnum

let contains loc pos =
  let a, b = get_c_num loc in
  a <= pos && pos <= b

let fresh () =
  { loc_start = dummy_pos; loc_end = dummy_pos; loc_ghost = true }

let comments = Lexer.comments
