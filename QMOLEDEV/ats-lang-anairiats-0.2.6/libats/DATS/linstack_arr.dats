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
**
** A array-based stack implementation
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2010 // based on a version done in October, 2008
**
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

staload "libats/SATS/linstack_arr.sats"

(* ****** ****** *)

assume STACK (
  a:viewt@ype, m:int, n:int
) = $DQ.DEQUE (a, m, n)

(* ****** ****** *)

implement stack_cap (s) = $DQ.deque_cap (s)
implement stack_size (s) = $DQ.deque_size (s)

implement stack_is_empty (s) = $DQ.deque_is_empty (s)
implement stack_isnot_empty (s) = $DQ.deque_isnot_empty (s)

implement stack_is_full (s) = $DQ.deque_is_full (s)
implement stack_isnot_full (s) = $DQ.deque_isnot_full (s)

(* ****** ****** *)

implement{a}
stack_initialize {m} (s, m) = let
  val (pfgc, pfarr | parr) = array_ptr_alloc<a> (m)
in
  $DQ.deque_initialize<a> {m} (pfgc, pfarr | s, m, parr)
end // end of [stack_initialize]

(* ****** ****** *)

implement
stack_uninitialize
  {a} {m,n} (s) = () where {
  val (pfgc, pfarr | parr) = $DQ.deque_uninitialize {a} (s)
  val () = array_ptr_free {a} (pfgc, pfarr | parr)
} // end of [stack_uninitialize]

implement
stack_uninitialize_vt
  {a} {m} (s) = () where {
  val (pfgc, pfarr | parr) = $DQ.deque_uninitialize_vt {a} (s)
  val () = array_ptr_free {a} (pfgc, pfarr | parr)
} // end of [stack_uninitialize_vt]

(* ****** ****** *)

implement{a}
stack_insert
  (s, x) = $DQ.deque_insert_end<a> (s, x)
// end of [stack_insert]

implement{a}
stack_remove (s) = $DQ.deque_remove_end<a> (s)

(* ****** ****** *)

implement{a}
stack_clear
  (s, n2) = $DQ.deque_clear_end<a> (s, n2)
// end of [stack_clear]

implement
stack_clear_all (s) = $DQ.deque_clear_all (s)

(* ****** ****** *)

implement{a}
stack_update_capacity
  (s, m2) = () where {
  val (pfgc, pfarr | parr) = array_ptr_alloc<a> (m2)
  val (pfgc, pfarr | parr) =
    $DQ.deque_update_capacity<a> (pfgc, pfarr | s, m2, parr)
  val () = array_ptr_free {a} (pfgc, pfarr | parr)
} // end of [stack_update_capacity]

(* ****** ****** *)

(* end of [linstack_arr.dats] *)
