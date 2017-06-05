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

(** Identifying idents which cannot be renamed independently of each
    other (due to signature matching in particular). *)

val restrict_propagation : bool ref

module ConstraintSet : Set.S
  with type elt = Types.signature * Types.signature

module IncludeSet : Set.S
  with type elt = Types.signature * Ident.t list

(** Collect binding constraints for a set of files, to indicate which
    idents must be renamed simultaneously. *)
val constrain_all_files :
  ?errors:exn list ref ->
  Env_untyped.path_sort -> string -> Program.program ->
  ConstraintSet.t * ConstraintSet.t * IncludeSet.t
  * (Ident.t * Ident.t) list

(** Raised by propagation if we go too far *)
exception EscapingRenaming of Ident.t list * Ident.t list

(** Return the minimal set of idents which may be renamed and contains
    a given id, as well as the "implicit" bindings of signature
    elements to those idents.
    If [mli_constraints] is given, we check only propagate those constraints,
    and raise an error if other constraints would propagate more idents. *)
val propagate :
  ?errors:exn list ref ->
  ?rename:bool -> Env_untyped.path_sort -> Ident.t ->
  ConstraintSet.t ->
  ?mli_constraints:ConstraintSet.t ->
  IncludeSet.t ->
  (Ident.t * Ident.t) list ->
  Ident.t list
  * ([ `certain | `maybe ] * Types.signature * Ident.t) list

(** Check the implicit bindings for capture. *)
val check_renamed_implicit_references :
  Env_untyped.path_sort -> Ident.t list -> string ->
  ([ `certain | `maybe ] * Types.signature * Ident.t) list -> unit

val check_other_implicit_references :
  Env_untyped.path_sort -> Ident.t list -> string ->
  ConstraintSet.t -> IncludeSet.t -> unit
