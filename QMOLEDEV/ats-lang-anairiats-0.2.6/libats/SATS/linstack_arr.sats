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
** An array-based stack implementation
** Author: hwxi AT cs DOT bu DOT edu
** Time: March, 2011
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

%{#
#include "libats/CATS/linstack_arr.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)
//
// a: item type
// m: maximal capacity
// n: current size
//
absviewt@ype STACK
  (a:viewt@ype+, m:int, n:int)
  = $extype "atslib_linstack_arr_STACK"
// end of [STACK]
viewtypedef
STACK0 (a:viewt@ype) = [m,n:int] STACK (a, m, n)

(* ****** ****** *)

fun stack_cap
  {a:viewt@ype} {m,n:int} (s: &STACK (a, m, n)):<> size_t m
fun stack_size
  {a:viewt@ype} {m,n:int} (s: &STACK (a, m, n)):<> size_t n

(* ****** ****** *)

fun stack_is_empty
  {a:viewt@ype} {m,n:int} (s: &STACK (a, m, n)):<> bool (n <= 0)
fun stack_isnot_empty
  {a:viewt@ype} {m,n:int} (s: &STACK (a, m, n)):<> bool (n > 0)

fun stack_is_full
  {a:viewt@ype} {m,n:int} (s: &STACK (a, m, n)):<> bool (m <= n)
fun stack_isnot_full
  {a:viewt@ype} {m,n:int} (s: &STACK (a, m, n)):<> bool (m > n)

(* ****** ****** *)
//
// HX: initializing to a stack of capacity [m]
//
fun{a:viewt@ype}
stack_initialize {m:nat}
  (s: &STACK0(a)? >> STACK (a, m, 0), m: size_t m):<> void
// end of [linstackarr_initialize]

(* ****** ****** *)
//
// HX: uninitializing a stack of nonlinear elements
//
fun stack_uninitialize {a:t@ype}
  {m,n:int} {l:addr} (s: &STACK (a, m, n) >> STACK0(a)?):<> void
// end of [stack_uninitialize]

//
// HX: uninitializeing an empty stack of capacity [m]
//
fun stack_uninitialize_vt {a:viewt@ype}
  {m:int} {l:addr} (s: &STACK (a, m, 0) >> STACK0(a)?):<> void
// end of [stack_unintialize_vt]

(* ****** ****** *)

fun{a:viewt@ype}
stack_insert (*last*) // HX: stack_push
  {m,n:int | m > n}
  (s: &STACK (a, m, n) >> STACK (a, m, n+1), x: a):<> void
// end of [stack_insert]

fun{a:viewt@ype}
stack_remove (*first*) // HX: stack_pop
  {m,n:int | n > 0} (s: &STACK (a, m, n) >> STACK (a, m, n-1)):<> a
// end of [stack_remove]

(* ****** ****** *)

fun{a:t@ype}
stack_clear
  {m,n1:int} {n2:nat | n2 <= n1} (
  s: &STACK (a, m, n1) >> STACK (a, m, n1-n2), n2: size_t n2
) :<> void // end of [stack_clear]

fun stack_clear_all
  {a:t@ype} {m,n:int}
  (s: &STACK (a, m, n) >> STACK (a, m, 0)):<> void
// end of [stack_clear_all]

(* ****** ****** *)

fun{a:viewt@ype}
stack_update_capacity
  {m1,n:int}
  {m2:nat | n <= m2}
  {l:addr} (
  q: &STACK (a, m1, n) >> STACK (a, m2, n)
, m2: size_t (m2)
) : void // end of [stack_update_capcity]

(* ****** ****** *)

(* end of [linstack_arr.sats] *)
