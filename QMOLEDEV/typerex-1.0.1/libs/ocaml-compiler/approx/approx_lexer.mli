(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(* ADMIN: fabrice *)

module Make(Tokens : Approx_tokens.Sig) : sig

  val init : unit -> unit
  val token : Lexing.lexbuf -> Tokens.token
  val token_pos : Lexing.lexbuf -> Tokens.token * (int * int)
  val comments : unit -> (int * int) list
  val token_locs : Lexing.lexbuf ->
    Tokens.token * (Lexing.position * Lexing.position)
  val token_locs_and_comments : Lexing.lexbuf ->
    Tokens.token * (Lexing.position * Lexing.position)
  val tokens_of_file :
    string -> (Tokens.token * (int * int)) list
  val tokens_of_string :
    string -> Tokens.token list
  val tokens_with_loc_of_string :
    string -> (Tokens.token * (int * int)) list
  val lines : unit -> (int * int) list
  val string_of_token : Tokens.token -> string
end

include Approx_tokens.Sig

val string_of_token : token -> string

val init : unit -> unit
val token : Lexing.lexbuf -> token
val token_pos : Lexing.lexbuf -> token * (int * int)
val comments : unit -> (int * int) list
val token_locs : Lexing.lexbuf ->
  token * (Lexing.position * Lexing.position)
val token_locs_and_comments : Lexing.lexbuf ->
  token * (Lexing.position * Lexing.position)
val tokens_of_file : string -> (token * (int * int)) list
val tokens_of_string : string -> token list
  val tokens_with_loc_of_string :
    string -> (token * (int * int)) list
val lines : unit -> (int * int) list
