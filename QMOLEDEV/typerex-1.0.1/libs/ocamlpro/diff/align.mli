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

(** Sequnce alignment. *)

(** Given two character strings, cut them in whitespace-separated
    words which are then aligned. *)
val align_words :
  string -> string ->
  [ `subst of string * string | `u of string | `v of string ] list

(** Return a side-by-side view of the alignement, with '_' for
    wildcards. *)
val show :
  [ `subst of string * string | `u of string | `v of string ] list ->
  string * string
