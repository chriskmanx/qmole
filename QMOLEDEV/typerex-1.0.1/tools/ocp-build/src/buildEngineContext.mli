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

open BuildEngineTypes

(* Add a new directory and all its sub-directories *)
val add_directory :
  build_context ->
  string -> build_directory

val find_directory : build_context -> string -> build_directory


(* [find_file dir basename] Find a file [basename] inside directory [dir].
   raise Not_found if the file could not be found. *)
val find_file : build_directory -> string ->   build_file
val find_dir : build_directory -> string -> build_directory



(*
  val new_rule :
(* context *)  build_context ->
(* location *)  string * int * string ->
(* main target *) build_file ->
(* actions *)  build_action list ->
  build_rule
*)

val add_file :
  build_context ->
  build_directory ->
  string -> build_file

val add_filename :
  build_context ->
  build_directory ->
  string -> build_file

val add_temp_file :
  build_context ->
  build_directory ->
  string -> build_file

val add_virtual_file :
  build_context ->
  build_directory ->
  string -> build_file

val create : string -> string -> build_context
