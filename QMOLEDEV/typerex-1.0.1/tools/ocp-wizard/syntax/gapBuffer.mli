(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                           Tiphaine Turpin                              *)
(*                                                                        *)
(*  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** Gap buffers, to efficiently 'follow' an editor buffer. *)

(** Marks are used to indicate "positions" in the gap buffer which
    move according to deletions and insertions (and become invalid
    when their pointed character is deleted). *)
type mark

type gap_buffer = private {
  mutable buf : string;
  (** gap buffer of characters. *)
  mutable pre : int;
    (** start of the gap : pre <= post *)
  mutable post : int; (* end of the gap *)
    (** the string is [0, pre[ @ [post, String.length buf[, point is at pre *)
  mutable line : int;
    (** 1 + number of eols in [0, pre[ = line number of buf.[pre] counting from 1 *)
  mutable marks_before : mark list;
    (** A list of markers < pre, in decreasing order *)
  mutable marks_after : mark list
    (** A list of markers >= post, in increasing order *)
}

(** Create an empty gap-buffer with specified initial buffer size
    (0 is safe). *)
val create : int -> gap_buffer

val contents : gap_buffer -> string

(** [substring gb b e] returns the segment "[b,e[" of [gb]. *)
val substring : gap_buffer -> int -> int -> string

val dump : gap_buffer -> out_channel -> unit
val snapshot : gap_buffer -> out_channel -> unit

val forward : gap_buffer -> unit
val backward : gap_buffer -> unit
val goto : gap_buffer -> int -> unit

val insert : [ `after | `before ] -> gap_buffer -> char -> unit
val insert_string : [ `after | `before ] -> gap_buffer -> string -> unit
val delete : [ `after | `before ] -> gap_buffer -> int -> unit

(** Replace the [n] next chars by [s], and move point just after [s]. *)
val replace : gap_buffer -> int -> string -> unit

val clear : gap_buffer -> unit

val pos2pointer : gap_buffer -> int -> int
val pointer2pos : gap_buffer -> int -> int

(** Raised by mark2pos to indicate that the mark is no longer valid. *)
exception MarkDeleted

(** Insert a new mark in the gap buffer at the given position. *)
val mark : gap_buffer -> int -> mark

(** Return the current position of a previously inserted mark.

    @raise [MarkDeleted] if the character under the mark has been
    deleted (or replaced) or the mark has been deleted. *)
val mark2pos : gap_buffer -> mark -> int

(** Remove the given mark from the gap_buffer (multiple deletion is
    allowed). *)
val delete_mark : gap_buffer -> mark -> unit
