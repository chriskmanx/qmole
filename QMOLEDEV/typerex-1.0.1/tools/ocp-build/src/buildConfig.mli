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

open BuildOCPTypes

(* Constants *)
(*
val ocpdir : string

val local_config_dir : string
val local_config_file : string

val global_config_dir : string
val global_config_file : string
val config_file_basename : string
  *)
(* Generate or read the configuration file *)
val generate_config_file : string -> unit
val load_config : string -> unit

(* These values are modified by reading the configuration *)
val ocamlc_cmd : string list source_option
val ocamllex_cmd : string list source_option
val ocamlyacc_cmd : string list source_option
val ocamldep_cmd : string list source_option
val ocamlopt_cmd : string list source_option
val mklib_cmd : BuildTypes.mklib_kind ref
val ar_cmd : string ref
val ranlib_cmd : string ref
val libdirs : (string * string) list ref



(* Misc *)
val get_stdout_lines : string list -> string list -> int * string list
