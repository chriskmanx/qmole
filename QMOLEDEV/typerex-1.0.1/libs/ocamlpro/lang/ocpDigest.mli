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

(** Extension of the stdlib Digest module *)

open Digest

(** Return the printable hexadecimal representation of the given digest. *)
val to_hex : t -> string

(** Return the digest corresponding to the printable hexadecimal representation. *)
val of_hex : string -> t

(** Return the digest by interpreting the string as a raw digest *)
val of_direct_string : string -> t

(** Return the string with the raw digest inside *)
val to_direct_string : t -> string


