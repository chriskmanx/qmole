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

exception MissingSourceWithNoBuildingRule of BuildEngineTypes.build_rule * string

val stats_command_executed : int ref
val stats_files_generated : int ref

(* [init targets] Initialize the build engine, by checking activating all the rules
 needed for the creation of the files [targets].
   raise MissingSourceWithNoBuildingRule (rule, filename) if a file is needed as
      a source and no rule is available for generating it.
*)
val init :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_file list -> unit

val fatal_errors : unit -> string list list
val errors : unit -> string list list

(* [parallel_loop ncores] Start the build process on [ncores] cores. *)
val parallel_loop :
  BuildEngineTypes.build_context -> int -> unit


val sanitize : BuildEngineTypes.build_context -> BuildEngineTypes.delete_orphans -> int
