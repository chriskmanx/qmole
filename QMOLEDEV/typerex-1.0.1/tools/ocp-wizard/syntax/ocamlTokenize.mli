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

(** Tokenized OCaml buffers *)

type extended_token =
  | Pad
  | Token of Parser.token
  | Error of Lexer.error
  | White_space
  | Comment

type token = private {
  token : extended_token;
  string : string;
  length : int; (* temporary *)
  mutable faces : (int * int * Face.face) list
    (** saving faces to detect face changes *)
}

val length : token -> int

val set_faces : token -> (int * int * Face.face) list -> unit

module OCamlToken : TokenBuffer.TOKEN
  with type token = token

(* 3.11 compatibility *)
module OCamlTokenBuffer : TokenBuffer.Make(OCamlToken).T
(*
module OCamlTokenBuffer : module type of TokenBuffer.Make(OCamlToken)
*)

val empty : unit -> OCamlTokenBuffer.tokenized_buffer
val reset : OCamlTokenBuffer.tokenized_buffer -> unit

(** Updates a tokenized buffer with a replacement, and returns the
    minimal token-aligned modified region, both expressed in chars and
    tokens. *)
val update_buffer :
  OCamlTokenBuffer.tokenized_buffer ->
  start:int -> old_length:int -> bool -> string -> int * int * int * int

val classify_comment : string -> [> `normal | `ocamldoc | `ocamldoc_stop ]
val parse_comment : string -> (int * int * Odoc_types.text_element) list

(** Return the (at most [max]) real tokens strictly before the given
    token position, from left to right. *)
val tokens_before :
  ?max:int -> OCamlTokenBuffer.tokenized_buffer -> int -> Parser.token list

(** The reverse order is often more relevant for matching *)
val rev_tokens_before :
  ?max:int -> OCamlTokenBuffer.tokenized_buffer -> int -> Parser.token list

(** Return the (at most [max]) real tokens from the given token
    position (included), from left to right. *)
val tokens_from :
  ?max:int -> OCamlTokenBuffer.tokenized_buffer -> int -> Parser.token list



