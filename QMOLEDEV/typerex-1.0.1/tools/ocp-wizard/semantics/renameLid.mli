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

(** The core of renaming. *)

(** Rename all occurrences of any of a list of idents with the same
    name in a longident, given the environmemt in which this longident
    makes sense, and make sure that the new longident still reffers to
    the same thing (i.e., is not subject to masking by other
    equally-named elements). *)
val rename_in_lid :
  Env_untyped.path_sort -> Ident.t list -> string ->
  Env.t -> Env_untyped.path_sort -> Program.source_file_id ->
  Longident.t -> Longident.t option

(** Check that no existing occurrence of the new name appearing in a
    longident would be captured by one of the renamed idents if we
    applied the given renaming. *)
val check_lid :
  Env_untyped.path_sort -> Ident.t list -> string ->
  Env.t -> Env_untyped.path_sort -> Longident.t -> unit

val prune_lid : Env.t -> Env_untyped.path_sort -> Longident.t -> Longident.t option

(** Raised by [eliminate_open] if the elimination is not possible. *)
exception Cannot_eliminate

val eliminate_open :
  Path.t -> Longident.t -> Env.t-> Env_untyped.path_sort -> Longident.t -> Longident.t option
