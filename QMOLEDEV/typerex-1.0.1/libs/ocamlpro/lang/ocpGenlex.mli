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

(** Extension of the stdlib Genlex module *)

open Genlex

(** Convert a token to a string *)
val string_of_token : token -> string

(*

(** Abstract type for lexers *)
type t

(** [of_lines keywords name lines] generates a lexer from [lines] from
    file [name], using elements of [keywords] as keyword token.
    Optionaly a function can be given to [discard] some lines (usually
    comments) *)
val of_lines : string list -> string -> ?discard:(string -> bool) -> string list  -> t
*)


(** Exception ParseError of char_position * message *)
exception ParseError of int * string

(** [tokens_of_string lexer string] returns the list of tokens from
    the string using [lexer]. Raise ParseError in case of error. *)
val tokens_of_string : (char Stream.t -> token Stream.t) -> string -> token list

(** [tokens_of_string lexer string] returns the list of pairs (token,
    position) from the string using [lexer]. Raise ParseError
    in case of error. *)
val tokenlocs_of_string : (char Stream.t -> token Stream.t) -> string -> (token * int) list

