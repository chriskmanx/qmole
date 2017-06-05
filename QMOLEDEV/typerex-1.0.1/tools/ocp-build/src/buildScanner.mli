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


val scan_directory_for_suffix :
 (* directory *) string -> (* extension *) string ->
  (string -> unit) -> unit

val scan_directory_for_files :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

val scan_directory_for_extensions :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

(*
val scan_directory_for_extensions2 :
 (* directory *) string ->
 (* extensions handlers *)
  (string ->  (* relative filename *)
   string ->  (* full filename *)
   unit) StringMap.t ->
  unit
*)

