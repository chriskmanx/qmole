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

(** Extension of the stdlib Hashtbl module *)

open Hashtbl

(** Convert an hash-table into a list *)
val to_list : ('a, 'b) t -> ('a * 'b) list

(** Convert a list into an hash-table *)
val of_list : ('a * 'b) list -> ('a, 'b) t

(** Increments the integer value associated to a key *)
val incr : ('a, int) t ->  'a -> unit

(** Check whether a predicate holds on all key-value pairs of an
    hash-table *)
val for_all : ('a, 'b) t -> ('a -> 'b -> bool) -> bool

(** Check wether a predicate holds on at least one key-value pair of
    an hash-table *)
val exists : ('a, 'b) t -> ('a -> 'b -> bool) -> bool
