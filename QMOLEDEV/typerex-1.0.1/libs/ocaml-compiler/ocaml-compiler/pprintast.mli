(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                 Thomas Gazagnaire, Fabrice Le Fessant                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(* Printing code expressions *)

val print_structure : Format.formatter -> Parsetree.structure -> unit
val print_signature : Format.formatter -> Parsetree.signature -> unit

val string_of_expression : Parsetree.expression -> string
val signature_item :  Format.formatter -> Parsetree.signature_item -> unit

val value_description : Format.formatter -> Parsetree.value_description -> unit
val module_type : Format.formatter -> Parsetree.module_type -> unit

