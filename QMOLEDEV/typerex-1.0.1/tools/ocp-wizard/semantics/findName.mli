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

(** Idents and Longidents locations in a program *)

open Program

(** Collect all the longident occurrences appearing in a file, with
    their location, sort, and lookup environment. *)
val get_lids :
  ?fname:string -> TypedtreeOps.node -> (Longident.t -> Location.t -> bool) ->
  (Longident.t Location.loc * Longident.t * (Env.t * Env_untyped.path_sort)) list

val collect_lids_in_program :
  ?errors:exn list ref -> (Longident.t -> Location.t -> bool) -> program ->
  (source_file_id * Location.t * Longident.t * Env.t * Env_untyped.path_sort) list

(** Return the lid at a given location. *)
val lid_of_loc :
  TypedtreeOps.node -> (Location.t -> bool) -> (Location.t -> Location.t -> bool) ->
  Longident.t Location.loc * Longident.t * (Env.t * Env_untyped.path_sort)

(** Find an ident's definition and return its kind and the located ident. *)
val find_ident_definition :
  Ident.t -> TypedtreeOps.node ->
  Env_untyped.path_sort * Ident.t * string Asttypes.loc

val find_ident_definitions :
  Ident.t -> TypedtreeOps.node ->
  (Env_untyped.path_sort * Ident.t * string Asttypes.loc) list

(** Return the location of a global ident (raise Not_found by default). *)
val id2loc : ?accept_none:bool -> program -> Ident.t -> Location.t

(** Classify a set of ident definitions according to whether they are
    in the current program (hence accessible and modifiable) or
    not. *)
val locate_defs :
  program -> Ident.t list ->
  [`ident_at of source_file_id * Location.t * string | `source_name of string] list
  * [`library of string | `predef | `unit_name] list
