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

(** Get all the lines of a files *)
val of_file : string -> string list

(** [file_of_lines name lines] saves the [lines] into the file [name] *)
val to_file : string -> string list -> unit

(** [iter_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iter : (string -> unit) -> string -> unit

(** [iteri_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iteri : (int -> string -> unit) -> string -> unit

(** [sub_lines filename off len] returns [len] lines of [filename], starting at [off] *)
val sub : string -> int -> int -> string list
