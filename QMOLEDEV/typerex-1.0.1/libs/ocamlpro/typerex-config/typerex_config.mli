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

(** Typerex static configuration *)

(** This module provides OCaml values for the configuration of TypeRex
    done when running [./configure]. *)

(** Absolute path to the OCaml standard library. *)
val ocamllib : string

(** Bytecode interpreter command. *)
val runner : string

(** Typerex version *)
val typerex_version : string

(** Typerex version args *)
val version : string * Arg.spec * Arg.doc

(** ... *)
