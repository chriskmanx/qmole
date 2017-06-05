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

(** Retrieving locations in typedtrees *)

open Asttypes

(** {2 Comparing ranges and locations} *)

(** Comparing a location and an absolute char range (starting at 0) *)

val contains : Location.t -> int * int -> bool
val included : int * int -> Location.t -> bool

(** Same with linenum (starting at 1) and colnum (starting at 0). *)

val contains_lc : Location.t -> (int * int) * (int * int) -> bool
val included_lc : (int * int) * (int * int) -> Location.t -> bool

val loc_contains_lc : Location.t -> Location.t -> bool
val loc_included_lc : Location.t -> Location.t -> bool

val contains_lc' : (int * int) * (int * int) -> (int * int) * (int * int) -> bool
val included_lc' : (int * int) * (int * int) -> (int * int) * (int * int) -> bool

(** {2 Looking for a subtree by location} *)

val node2loc : TypedtreeOps.node -> Location.t

(** Return the innermost or outermost subtree whose locations contains
    a given character number interval \[a, b\[.

    Warning: most node kinds are missing ! *)
val locate :
  [`outermost | `innermost] ->
  (Location.t -> bool) -> (Location.t -> Location.t -> bool) ->
  TypedtreeOps.node -> TypedtreeOps.node

(** Simliar to locate, but we return the first node along a path (in
    the sense of the given priority) for which the parameter function
    returns some result.

    Warning ! Implementation is not complete. *)
val locate_map :
  [`outermost | `innermost] ->
  (Location.t -> TypedtreeOps.node -> 'a option) ->
  (Location.t -> bool) -> (Location.t -> Location.t -> bool) ->
  TypedtreeOps.node -> 'a

(** {2 Specialized search for some specific kinds of nodes} *)

(** Return the ident defined at a given location. "*opt*" idents are
    discarded, because they have a wrong location which "captures"
    everything.

    Warning ! classes and class types are not yet implemented. *)
val ident_definition :
  (Location.t -> bool) -> (Location.t -> Location.t -> bool) ->
  TypedtreeOps.node ->
  Env_untyped.path_sort * Ident.t * string loc * Env_untyped.description

(** Return the ident defined by the declaration containing the given
    location (this only matches structure and signature items, and not
    local bindings). *)
val visible_ident_definition :
  [`outermost | `innermost] ->
  (Location.t -> bool) -> (Location.t -> Location.t -> bool) ->
  TypedtreeOps.node -> Env_untyped.path_sort * Ident.t * string loc

val locate_map_item :
  (Location.t ->
  [ `signature of Typedtree.signature * Typedtree.signature_item
  | `structure of Typedtree.structure * Typedtree.structure_item ] ->
   'a option) ->
  (Location.t -> bool) -> (Location.t -> Location.t -> bool) ->
  TypedtreeOps.node -> 'a

val locate_item :
  (Location.t -> bool) -> (Location.t -> Location.t -> bool) ->
  TypedtreeOps.node ->
  Location.t *
  [ `signature of Typedtree.signature * Typedtree.signature_item
  | `structure of Typedtree.structure * Typedtree.structure_item ]
