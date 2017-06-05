(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
** An array-based queue implementation
** Author: hwxi AT cs DOT bu DOT edu
** Time: March, 2011
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload DQ = "libats/ngc/SATS/deque_arr.sats"

(* ****** ****** *)

staload "libats/SATS/linqueue_arr.sats"

(* ****** ****** *)

assume QUEUE (
  a:viewt@ype, m:int, n:int
) = $DQ.DEQUE (a, m, n)

(* ****** ****** *)

implement queue_cap (q) = $DQ.deque_cap (q)
implement queue_size (q) = $DQ.deque_size (q)

implement queue_is_empty (q) = $DQ.deque_is_empty (q)
implement queue_isnot_empty (q) = $DQ.deque_isnot_empty (q)

implement queue_is_full (q) = $DQ.deque_is_full (q)
implement queue_isnot_full (q) = $DQ.deque_isnot_full (q)

(* ****** ****** *)

implement{a}
queue_initialize
  (q, m) = let
  val (pfgc, pfarr | parr) = array_ptr_alloc<a> (m)
in
  $DQ.deque_initialize<a> (pfgc, pfarr | q, m, parr)
end // end of [queue_initialize]

implement
queue_initialize_tsz
  {a} (q, m, tsz) = let
  val (pfgc, pfarr | parr) = array_ptr_alloc_tsz {a} (m, tsz)
in
  $DQ.deque_initialize_tsz {a} (pfgc, pfarr | q, m, parr, tsz)
end // end of [queue_initialize_tsz]

(* ****** ****** *)
//
// HX-2010-03-29:
// the function is given the external name:
// atslib_linqueue_arr_queue_uninitialize
//
implement
queue_uninitialize
  {a} (q) = () where {
  val (pfgc, pfarr | parr) = $DQ.deque_uninitialize (q)
  val () = array_ptr_free {a} (pfgc, pfarr | parr)
} // end of [queue_uninitialize]

implement
queue_uninitialize_vt
  {a} (q) = () where {
  val (pfgc, pfarr | parr) = $DQ.deque_uninitialize_vt (q)
  val () = array_ptr_free {a} (pfgc, pfarr | parr)
} // end of [queue_uninitialize_vt]

(* ****** ****** *)

implement{a}
queue_get_elt_at
  (q, i) = $DQ.deque_get_elt_at (q, i)
// end of [queue_get_elt_at]

implement{a}
queue_set_elt_at
  (q, i, x) = $DQ.deque_set_elt_at (q, i, x)
// end of [queue_set_elt_at]

(* ****** ****** *)

implement{a}
queue_insert
  (q, x) = $DQ.deque_insert_end (q, x)
// end of [queue_insert]

implement{a}
queue_insert_many
  (q, k, xs) = $DQ.deque_insert_end_many (q, k, xs)
// end of [queue_insert_many]

(* ****** ****** *)

implement{a}
queue_remove (q) = $DQ.deque_remove_beg (q)

implement{a}
queue_remove_many
  (q, k, xs) = $DQ.deque_remove_beg_many (q, k, xs)
// end of [queue_remove_many]

(* ****** ****** *)

implement{a}
queue_clear
  (q, n2) = $DQ.deque_clear_beg<a> (q, n2)
// end of [queue_clear]

implement
queue_clear_all (q) = $DQ.deque_clear_all (q)

(* ****** ****** *)

implement{a}
queue_copyout
  (q, i, k, xs) = $DQ.deque_copyout (q, i, k, xs)
// end of [queue_copyout]

(* ****** ****** *)

implement{a}
queue_update_capacity
  (q, m2) = () where {
  val (pfgc, pfarr | parr) = array_ptr_alloc<a> (m2)
  val (pfgc, pfarr | parr) =
    $DQ.deque_update_capacity<a> (pfgc, pfarr | q, m2, parr)
  val () = array_ptr_free {a} (pfgc, pfarr | parr)
} // end of [queue_update_capacity]

(* ****** ****** *)

(* end of [linqueue_arr.dats] *)
