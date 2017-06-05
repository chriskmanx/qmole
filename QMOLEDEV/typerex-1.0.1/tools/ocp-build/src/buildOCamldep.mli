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


(* [load_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_dependencies : string -> (string * string list list) list

(* [load_modules_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_modules_dependencies : BuildTypes.package_info -> BuildOCPTypes.source_options ->
  BuildEngineTypes.build_directory -> string list -> string -> (string * string list list) list


val modname_of_file : BuildOCPTypes.source_options -> string -> bool * string * string
