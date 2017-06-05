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

(** Syntax coloring *)

(** This module provides the general algorithmics for computing syntax
    coloring, using on-demand forward and backward search in the
    sequence of tokens. This primitive implementation is both costly
    and approximate (and hard to read and maintain), so it should be
    improved by storing a stack of opened constructs in the future.
    The actual coloring themes are implemented as plugins which
    registered themselves using one of the [register] functions (see
    directory coloring_themes). *)

(** [colors buffer start end_] colorizes the changed region \[start,
    end_\[ (in chars), and returns an enlarged region to fontify, and
    the faces and helps to apply. The argument region is always fully
    re-colorised, while tokens outside this region are only
    recolorised if they have changed. *)
val colors :
  OcamlTokenize.OCamlTokenBuffer.tokenized_buffer -> int -> int ->
  (int * int) *
    (Face.face * (int * int) list) list *
    ((int * int) * string) list


(** The following functions are made available for implementing color
    themes. *)

val classify_lid :
  before:Parser.token list -> after:Parser.token list ->
  [ `def | `occ ] *
  [ `field | `methodname | `tconstr | `tvar | `variant | `arglabel
  | `value of [ `func | `value ] ]

val classify_uid :
  before:Parser.token list -> after:Parser.token list ->
  [ `def | `occ ] * [ `cstr | `exn | `modname of [ `arg | `local | `top ] ]

val classify_let : ?max:int -> after:Parser.token list -> unit -> [ `top | `inner ]

val classify_module_type_val :
  Parser.token -> before:Parser.token list -> after:Parser.token list ->
  [ `governing | `with_constraint | `modtype_of | `tvar | `package]

val classify_bar : before:Parser.token list -> [`matching | `other]

(** Call this function to change the coloring theme. The provided
    function will be called for tokens which needs to be recolorized,
    and should either
    - return a face
    - raise an exception, which indicates no particular face. *)
val register_token2face :
  string ->
  (OcamlTokenize.token ->
   before:Parser.token list ->
   after:Parser.token list ->
   Face.face) ->
  unit

(** Same as [register_token2face], but the function takes two
    parameters (the beginning and ending positions of the token) and
    returns a list of (begin, end, face) tuples. *)
val register_token2faces :
  string ->
  (OcamlTokenize.token ->
   before:Parser.token list ->
   after:Parser.token list ->
   int -> int ->
   (int * int * Face.face) list) ->
  unit

(** Choose a theme *)
val set_theme : string -> unit
val list_themes : unit -> string list











