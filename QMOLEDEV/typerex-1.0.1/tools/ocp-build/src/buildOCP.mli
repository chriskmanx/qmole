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

val find_project : File.t -> string -> File.t
val open_project : File.t -> project

(* returns the number of errors while reading the files *)
val load_packages : project -> int

val save_project : File.t -> project -> unit
val scan_project : project -> unit

(* [find_package pj file] returns the list of packages in
   project [pj] containing [file] as a source.
*)
val find_package : project -> File.t -> package list
