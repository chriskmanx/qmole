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

(* We need to be able to catch all classical exceptions, raised during
 lexing and parsing.
*)
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

val report_error : Format.formatter -> error -> Location.t -> unit

module Raw : sig

  val structure : string -> Lexing.lexbuf -> Parsetree.structure
  val structure_of_string : string -> string -> Parsetree.structure
  val structure_of_file : File.t -> Parsetree.structure

  val signature : string -> Lexing.lexbuf -> Parsetree.signature
  val signature_of_string : string -> string -> Parsetree.signature
  val signature_of_file : File.t -> Parsetree.signature
  val expression_of_string : string -> string -> Parsetree.expression

end

module Safe : sig

(* same as [Raw] functions, but all errors are caught, an error message
  is printed on stderr, and the program exits. *)

  val structure : string -> Lexing.lexbuf -> Parsetree.structure
  val structure_of_string : string -> string -> Parsetree.structure
  val structure_of_file : File.t -> Parsetree.structure

  val signature : string -> Lexing.lexbuf -> Parsetree.signature
  val signature_of_string : string -> string -> Parsetree.signature
  val signature_of_file : File.t -> Parsetree.signature

end

val comments : unit -> (string * Location.t) list

(** {2 OCamlPro functions} *)

(** [get_c_num loc] returns the locations of first and last characters of [loc] *)
val get_c_num : Location.t -> int * int

(** [contains loc pos] retuns whether location [loc] contains the position [pos] *)
val contains : Location.t -> int -> bool

(** [fresh ()] returns a fresh dummy location *)
val fresh : unit -> Location.t
