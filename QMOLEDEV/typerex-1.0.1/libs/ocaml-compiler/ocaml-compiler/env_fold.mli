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

(** Folding over all identifiers (for analysis purpose) *)

val fold_values:
  (string -> Path.t -> Types.value_description -> 'a -> 'a) ->
  Longident.t option -> Env.t -> 'a -> 'a
val fold_types:
  (string -> Path.t -> Types.type_declaration -> 'a -> 'a) ->
  Longident.t option -> Env.t -> 'a -> 'a
val fold_constructors:
  (string -> Path.t -> Types.constructor_description -> 'a -> 'a) ->
  Longident.t option -> Env.t -> 'a -> 'a
val fold_labels:
  (string -> Path.t -> Types.label_description -> 'a -> 'a) ->
  Longident.t option -> Env.t -> 'a -> 'a

(** Persistent structures are only traversed if they are already loaded. *)
val fold_modules:
  (string -> Path.t -> Types.module_type -> 'a -> 'a) ->
  Longident.t option -> Env.t -> 'a -> 'a

val fold_modtypes:
  (string -> Path.t -> Types.modtype_declaration -> 'a -> 'a) ->
  Longident.t option -> Env.t -> 'a -> 'a
val fold_classs:
  (string -> Path.t -> Types.class_declaration -> 'a -> 'a) ->
  Longident.t option -> Env.t -> 'a -> 'a
val fold_cltypes:
  (string -> Path.t -> Types.class_type_declaration -> 'a -> 'a) ->
  Longident.t option -> Env.t -> 'a -> 'a
