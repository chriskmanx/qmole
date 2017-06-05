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

val add_dependency_loader :
  string -> (string -> ( string * string list list) list) -> unit
val find_dependency_loader :
  string -> (string -> ( string * string list list) list)


val new_dir_id : BuildEngineTypes.build_context -> int
val new_file_id : BuildEngineTypes.build_context -> int
val new_rule_id : BuildEngineTypes.build_context -> int
val new_process_id : BuildEngineTypes.build_context -> int

val file_filename : BuildEngineTypes.build_file -> string
(* val print_indented_command : BuildEngineTypes.build_action -> unit *)


