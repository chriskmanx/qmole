(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant, Tiphaine Turpin            *)
(*                                                                        *)
(*    OCaml - Camlp4                                                      *)
(*      Daniel de Rauglaudre, Nicolas Pouillard, INRIA Rocquencourt       *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 2006-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU LGPL, with the special exception on linking described in      *)
(*  file LIDENSE                                                          *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Library General Public License for more details.                  *)
(**************************************************************************)

(** Reading camlp4 ( 3.12.* ) ASTs and mapping them to Parsetrees. *)

(** Magic numbers for camlp4 ASTs. These are the same since version
    3.10 ! *)

val camlp4_ast_impl_magic_number: string
val camlp4_ast_intf_magic_number: string

(** Reading functions, to call read the input after magic number. *)

val input_camlp4_ast_impl : in_channel -> string * Parsetree.structure
val input_camlp4_ast_intf : in_channel -> string * Parsetree.signature
