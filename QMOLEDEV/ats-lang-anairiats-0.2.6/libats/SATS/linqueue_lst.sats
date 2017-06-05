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
** A list-based queue implementation
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: July, 2010 // based on a version done in October, 2008
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

%{#
#include "libats/CATS/linqueue_lst.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)
//
// HX: a: item type; n: current size
//
absviewt@ype
QUEUE (a:viewt@ype+, n: int) =
  $extype "atslib_linqueue_lst_QUEUE"
// end of [QUEUE]
typedef QUEUE0 (a:viewt@ype) = QUEUE (a, 0)?
viewtypedef QUEUE1 (a:viewt@ype) = [n:nat] QUEUE (a, n)

(* ****** ****** *)

fun{a:viewt@ype}
queue_size {n:nat} (q: &QUEUE (a, n)):<> size_t n

fun queue_is_empty
  {a:viewt@ype} {n:nat} (q: &QUEUE (a, n)):<> bool (n <= 0)
// end of [queue_is_empty]

fun queue_isnot_empty
  {a:viewt@ype} {n:nat} (q: &QUEUE (a, n)):<> bool (n > 0)
// end of [queue_isnot_empty]

(* ****** ****** *)

fun queue_initialize
  {a:viewt@ype} (q: &QUEUE0 a >> QUEUE (a, 0)):<> void
fun{a:viewt@ype} queue_uninitialize
  {n:nat} (q: &QUEUE (a, n) >> QUEUE0 a):<> list_vt (a, n)

(* ****** ****** *)

fun{a:viewt@ype}
queue_insert (*last*)
  {n:nat} (q: &QUEUE (a, n) >> QUEUE (a, n+1), x: a):<> void
// end of [queue_insert]

fun{a:viewt@ype}
queue_remove (*first*)
  {n:nat | n > 0} (q: &QUEUE (a, n) >> QUEUE (a, n-1)):<> a
// end of [queue_remove]

(* ****** ****** *)

fun{a:viewt@ype}
queue_foreach_funenv
  {v:view} {vt:viewtype} {n:nat} (
  pf: !v
| q: &QUEUE (a, n), f: (!v | &a, !vt) -<fun> void, env: !vt
) :<> void // end of [queue_foreach_funenv]

fun{a:viewt@ype}
queue_foreach_fun
  {n:nat} (q: &QUEUE (a, n), f: (&a) -<fun> void):<> void
// end of [queue_foreach_fun]

fun{a:viewt@ype}
queue_foreach_vclo
  {v:view} {n:nat}
  (pf: !v | q: &QUEUE (a, n), f: &(!v | &a) -<clo> void):<> void
// end of [queue_foreach_vclo]

(* ****** ****** *)

(* end of [linqueue_lst.sats] *)
