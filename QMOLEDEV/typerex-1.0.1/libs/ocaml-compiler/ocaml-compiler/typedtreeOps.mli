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

(** Traversing and searching typedtrees *)

open Asttypes
open Typedtree
open Env_untyped

(** {2 The types of nodes and typedtrees} *)

(** The type of functions that can apply either to a structure or a
    signature. *)
type typedtree = [ `structure of structure | `signature of signature]

(** The common type for all typedtree nodes. *)
type node = [
  typedtree
| `value_description of value_description
| `type_declaration of type_declaration
| `exception_declaration of exception_declaration
| `pattern of pattern
| `expression of expression
| `package_type of package_type
| `signature_item of signature_item
| `modtype_declaration of modtype_declaration
| `module_type of module_type
| `module_expr of module_expr
| `with_constraint of with_constraint
| `class_expr of class_expr
| `class_signature of class_signature
| `class_declaration of class_declaration
| `class_description of class_description
| `class_type_declaration of class_type_declaration
| `class_type of class_type
| `class_type_field of class_type_field
| `core_type of core_type
| `core_field_type of core_field_type
| `class_structure of class_structure
| `class_field of class_field
| `structure_item of structure_item
| `binding of pattern * expression
| `bindings of rec_flag
]

(** Return the constructor name, as a string. *)
val node_kind : node -> string

(** {2 Generic iterators} *)

(** Traverse a typedtree, calling the provided enter and leave
    functions just before and just after each node, respectively. *)
val iterator : enter:(node -> unit) -> leave:(node -> unit) -> node -> unit

(** Find the innermost node for which some condition holds. *)
val find_map : [`outermost | `innermost] -> (node -> 'a option) -> node -> 'a

(** Find all nodes satisfying some condition. *)
val find_all_map : (node -> 'a option) -> node -> 'a list

(** {2 Node kind specific iterators} *)

val find_map_pattern :
  [`outermost | `innermost] -> (pattern -> 'a option) -> node -> 'a
val find_map_expression :
  [`outermost | `innermost] -> (expression -> 'a option) -> node -> 'a

(** {2 Iterating over ident references} *)

(** Warning ! classes and class types are not yet fully implemented. *)

val apply2paths :
  (Env.t -> path_sort -> Path.t -> Longident.t loc -> unit) ->
  node -> unit

val iter_paths:
  enter:(Env.t -> path_sort -> Path.t -> Longident.t loc -> unit) ->
  leave:(Env.t -> path_sort -> Path.t -> Longident.t loc -> unit) ->
  node -> unit

val find_all_map_paths :
  (Env.t -> path_sort -> Path.t -> Longident.t loc -> 'a option) ->
  node -> 'a list

val find_all_paths :
  ?keep_ghosts:bool -> node ->
  (Longident.t loc * Path.t * (Env.t * path_sort)) list

(** {2 Iterating over ident definitions} *)

(** Warning ! classes and class types are not yet fully implemented. *)

val apply2defs :
  (path_sort -> Ident.t -> string loc -> description -> unit) ->
  node -> unit

val iter_ident_definitions :
  enter:(path_sort -> Ident.t -> string loc -> description -> unit) ->
  leave:(path_sort -> Ident.t -> string loc -> description -> unit) ->
  node -> unit

val find_all_map_ident_definitions :
  (path_sort -> Ident.t -> string loc ->  description -> 'a option) ->
  node -> 'a list



