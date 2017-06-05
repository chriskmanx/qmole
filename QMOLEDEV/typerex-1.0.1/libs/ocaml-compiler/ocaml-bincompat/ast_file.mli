(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** This is an entry point for reading of OCaml ASTs for the supported
    versions ("3.12" and "current"), as well as Camlp4 ASTs for
    version 3.12 only. *)

exception Outdated_version

(* can raise Outdated_version if the version is not recognized *)
val read_interface : in_channel -> (string * Parsetree.signature) option
val read_implementation : in_channel -> (string * Parsetree.structure) option

(*
val write : ?version: Bincompat.version -> string -> t -> unit
*)
