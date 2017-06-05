(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
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

%{#
#include "libats/CATS/linqueue_arr.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)
//
// a: item type
// m: maximal capacity
// n: current size
//
absviewt@ype
QUEUE (
  a:viewt@ype+, m: int, n: int
) = $extype "atslib_linqueue_arr_QUEUE"
viewtypedef
QUEUE0 (a:viewt@ype) = [m,n:int] QUEUE (a, m, n)

(* ****** ****** *)

prfun queue_param_lemma
  {a:viewt@ype} {m,n:int}
  (x: &QUEUE (a, m, n)): [0 <= n; n <= m] void
// end of [queue_param_lemma]

(* ****** ****** *)

fun queue_cap
  {a:viewt@ype} {m,n:int} (q: &QUEUE (a, m, n)):<> size_t m
fun queue_size
  {a:viewt@ype} {m,n:int} (q: &QUEUE (a, m, n)):<> size_t n

(* ****** ****** *)

fun queue_is_empty
  {a:viewt@ype} {m,n:int} (q: &QUEUE (a, m, n)):<> bool (n <= 0)
fun queue_isnot_empty
  {a:viewt@ype} {m,n:int} (q: &QUEUE (a, m, n)):<> bool (n > 0)

fun queue_is_full
  {a:viewt@ype} {m,n:int} (q: &QUEUE (a, m, n)):<> bool (m <= n)
fun queue_isnot_full
  {a:viewt@ype} {m,n:int} (q: &QUEUE (a, m, n)):<> bool (m > n)

(* ****** ****** *)
//
// HX: initializing to a queue of capacity [m]
//
fun{a:viewt@ype}
queue_initialize {m:nat}
  (q: &QUEUE0(a)? >> QUEUE (a, m, 0), m: size_t m):<> void
// end of [queue_initialize]

//
// HX: initializing to a queue of capacity [m]
//
fun queue_initialize_tsz
  {a:viewt@ype} {m:nat} (
  q: &QUEUE0(a)? >> QUEUE (a, m, 0), m: size_t m, tsz: sizeof_t a
) :<> void
  = "atslib_linqueue_arr_queue_initialize_tsz"
// end of [queue_initialize_tsz]

(* ****** ****** *)
//
// HX: uninitializing a queue of nonlinear elements
//
fun queue_uninitialize
  {a:t@ype}
  {m,n:int}
  {l:addr} (
  q: &QUEUE (a, m, n) >> QUEUE0(a)?
) :<> void
  = "atslib_linqueue_arr_queue_uninitialize"
// end of [queue_uninitialize]

//
// HX: uninitializeing a queue that is empty
//
fun queue_uninitialize_vt
  {a:viewt@ype}
  {m:int}
  {l:addr} (
  q: &QUEUE (a, m, 0) >> QUEUE0(a)?
) :<> void
// end of [queue_uninitialize_vt]

(* ****** ****** *)

fun{a:t@ype}
queue_get_elt_at
  {m,n:int} {i:nat | i < n} (
  q: &QUEUE (a, m, n), i: size_t i
) :<> a // end of [queue_get_elt_at]

fun{a:t@ype}
queue_set_elt_at
  {m,n:int} {i:nat | i < n} (
  q: &QUEUE (a, m, n), i: size_t i, x: a
) :<> void // end of [queue_set_elt_at]

(* ****** ****** *)

fun{a:viewt@ype}
queue_insert (*last*)
  {m,n:int | m > n} (
  q: &QUEUE (a, m, n) >> QUEUE (a, m, n+1), x: a
) :<> void
// end of [queue_insert]

fun{a:viewt@ype}
queue_insert_many
  {m,n:int}
  {k:nat | n+k <= m} (
  q: &QUEUE (a, m, n) >> QUEUE (a, m, n+k)
, k: size_t k
, xs: &(@[a][k]) >> @[a?!][k]
) :<> void // end of [queue_insert_many]

(* ****** ****** *)

fun{a:viewt@ype}
queue_remove (*first*)
  {m,n:int | n > 0} (
  q: &QUEUE (a, m, n) >> QUEUE (a, m, n-1)
) :<> a // end of [queue_remove]

fun{a:viewt@ype}
queue_remove_many
  {m,n:int}
  {k:nat | k <= n} (
  q: &QUEUE (a, m, n) >> QUEUE (a, m, n-k)
, k: size_t k
, xs: &(@[a?][k]) >> @[a][k]
) :<> void // end of [queue_remove_many]

(* ****** ****** *)

fun{a:t@ype}
queue_clear
  {m,n1:int} {n2:nat | n2 <= n1} (
  q: &QUEUE (a, m, n1) >> QUEUE (a, m, n1-n2), n2: size_t n2
) :<> void // end of [queue_clear]

fun queue_clear_all
  {a:t@ype} {m,n:int}
  (s: &QUEUE (a, m, n) >> QUEUE (a, m, 0)):<> void
// end of [queue_clear_all]

(* ****** ****** *)

fun{a:t@ype}
queue_copyout
  {m,n:int}
  {i,k:nat | i+k <= n} (
  q: &QUEUE (a, m, n)
, i: size_t i
, k: size_t k
, xs: &(@[a?][k]) >> @[a][k]
) :<> void // end of [queue_copyout]

(* ****** ****** *)

fun{a:viewt@ype}
queue_update_capacity
  {m1,n:int}
  {m2:nat | n <= m2}
  {l:addr} (
  q: &QUEUE (a, m1, n) >> QUEUE (a, m2, n)
, m2: size_t (m2)
) : void // end of [queue_update_capcity]

(* ****** ****** *)

(* end of [linqueue_arr.sats] *)
