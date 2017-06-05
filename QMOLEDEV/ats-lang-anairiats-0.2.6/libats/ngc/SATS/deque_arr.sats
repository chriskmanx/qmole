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
** An array-based deque implementation
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

%{#
#include "libats/ngc/CATS/deque_arr.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)
//
// HX:
// a: item type
// m: maximal capacity
// n: current size
//
absviewt@ype
DEQUE (
  a:viewt@ype+, m: int, n: int
) = $extype"atslib_ngc_deque_arr_DEQUE"
viewtypedef
DEQUE0 (
  a:viewt@ype
) = [m,n:int] DEQUE (a, m, n)

(* ****** ****** *)

prfun deque_param_lemma
  {a:viewt@ype} {m,n:int}
  (x: &DEQUE (a, m, n)): [0 <= n; n <= m] void
// end of [deque_param_lemma]

(* ****** ****** *)

fun deque_cap {a:viewt@ype}
  {m,n:int} (q: &DEQUE (a, m, n)):<> size_t m
fun deque_size {a:viewt@ype}
  {m,n:int} (q: &DEQUE (a, m, n)):<> size_t n

(* ****** ****** *)

fun deque_is_empty {a:viewt@ype}
  {m,n:int} (q: &DEQUE (a, m, n)):<> bool (n <= 0)
fun deque_isnot_empty {a:viewt@ype}
  {m,n:int} (q: &DEQUE (a, m, n)):<> bool (n > 0)

fun deque_is_full {a:viewt@ype}
  {m,n:int} (q: &DEQUE (a, m, n)):<> bool (m <= n)
fun deque_isnot_full {a:viewt@ype}
  {m,n:int} (q: &DEQUE (a, m, n)):<> bool (m > n)

(* ****** ****** *)
//
// HX: initializing to a deque of capacity [m]
//
fun{a:viewt@ype}
deque_initialize
  {m:nat} {l:addr} (
  pfgc: free_gc_v (a?, m, l)
, pfarr: array_v (a?, m, l)
| q: &DEQUE0(a)? >> DEQUE (a, m, 0)
, m: size_t m, p: ptr l
) :<> void // end of [deque_initialize]

fun deque_initialize_tsz
  {a:viewt@ype}
  {m:nat} {l:addr} (
  pfgc: free_gc_v (a?, m, l)
, pfarr: array_v (a?, m, l)
| q: &DEQUE0(a)? >> DEQUE (a, m, 0)
, m: size_t m
, p: ptr l
, tsz: sizeof_t a
) :<> void
  = "atslib_ngc_deque_arr_deque_initialize_tsz"
// end of [deque_initialize_tsz]

(* ****** ****** *)
//
// HX: uninitializeing a deque of nonlinear elements
//
fun deque_uninitialize
  {a:t@ype}
  {m,n:int} (
  q: &DEQUE (a, m, n) >> DEQUE0(a)?
) :<> [l:addr] (
  free_gc_v (a?, m, l), array_v (a?, m, l)
| ptr l
) = "atslib_ngc_deque_arr_deque_uninitialize"
// end of [deque_uninitialize]

//
// HX: uninitializeing an empty deque of capacity [m]
//
fun deque_uninitialize_vt
  {a:viewt@ype}
  {m:int} (
  q: &DEQUE (a, m, 0) >> DEQUE0(a)?
) :<> [l:addr] (
  free_gc_v (a?, m, l), array_v (a?, m, l)
| ptr l
) // end of [deque_uninitialize_vt]

(* ****** ****** *)
//
// HX-2011-03-06:
// slightly unsafe but greatly convenient!
//
fun
deque_takeout_tsz
  {a:viewt@ype}
  {m,n:int}
  {i:nat | i < n} (
  q: &DEQUE (a, m, n)
, i: size_t i, tsz: sizeof_t a
) :<> [l:addr] (
  a @ l, a @ l -<lin,prf> void | ptr l
) = "atslib_ngc_deque_arr_deque_takeout_tsz"

(* ****** ****** *)

fun{a:t@ype}
deque_get_elt_at
  {m,n:int}
  {i:nat | i < n} (
  q: &DEQUE (a, m, n), i: size_t i
) :<> a // end of [deque_get_elt_at]

fun{a:t@ype}
deque_set_elt_at
  {m,n:int}
  {i:nat | i < n} (
  q: &DEQUE (a, m, n), i: size_t i, x: a
) :<> void // end of [deque_set_elt_at]

(* ****** ****** *)

fun{a:viewt@ype}
deque_insert_beg (*last*)
  {m,n:int | m > n} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, n+1), x: a
) :<> void // end of [deque_insert_beg]

(* ****** ****** *)

fun{a:viewt@ype}
deque_insert_end (*last*)
  {m,n:int | m > n} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, n+1), x: a
) :<> void // end of [deque_insert_end]

fun{a:viewt@ype}
deque_insert_end_many
  {m,n:int}
  {k:nat | n+k <= m} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, n+k)
, k: size_t k
, xs: &(@[a][k]) >> @[a?!][k]
) :<> void // end of [deque_insert_end_many]

fun
deque_insert_end_many_tsz
  {a:viewt@ype}
  {m,n:int}
  {k:nat | n+k <= m} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, n+k)
, k: size_t k
, xs: &(@[a][k]) >> @[a?!][k]
, tsz: sizeof_t (a)
) :<> void
  = "atslib_ngc_deque_arr_deque_insert_end_many_tsz"
// end of [deque_insert_end_many_tsz]
 
(* ****** ****** *)

fun{a:viewt@ype}
deque_remove_beg (*first*)
  {m,n:int | n > 0} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, n-1)
) :<> a // end of [deque_remove_beg]

fun{a:viewt@ype}
deque_remove_beg_many
  {m,n:int}
  {k:nat | k <= n} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, n-k)
, k: size_t k
, xs: &(@[a?][k]) >> @[a][k]
) :<> void // end of [deque_remove_beg_many]

fun
deque_remove_beg_many_tsz
  {a:viewt@ype}
  {m,n:int}
  {k:nat | k <= n} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, n-k)
, k: size_t k
, xs: &(@[a?][k]) >> @[a][k]
, tsz: sizeof_t a
) :<> void
  = "atslib_ngc_deque_arr_deque_remove_beg_many_tsz"
// end of [deque_remove_beg_many_tsz]
 
(* ****** ****** *)

fun{a:viewt@ype}
deque_remove_end (*first*)
  {m,n:int | n > 0} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, n-1)
) :<> a // end of [deque_remove_end]

(* ****** ****** *)

fun{a:t@ype}
deque_clear_beg
  {m,n1:int} {n2:nat | n2 <= n1} (
  q: &DEQUE (a, m, n1) >> DEQUE (a, m, n1-n2), n2: size_t n2
) :<> void // end of [deque_clear_beg]

fun{a:t@ype}
deque_clear_end
  {m,n1:int} {n2:nat | n2 <= n1} (
  q: &DEQUE (a, m, n1) >> DEQUE (a, m, n1-n2), n2: size_t n2
) :<> void // end of [deque_clear_end]

fun deque_clear_all
  {a:t@ype} {m,n:int} (
  q: &DEQUE (a, m, n) >> DEQUE (a, m, 0)
) :<> void // end of [deque_clear_all]

(* ****** ****** *)

fun
deque_copyout_tsz
  {a:t@ype}
  {m,n:int}
  {i,k:nat | i+k <= n} (
  q: &DEQUE (a, m, n)
, i: size_t i
, k: size_t k
, xs: &(@[a?][k]) >> @[a][k]
, tsz: sizeof_t a
) :<> void
  = "atslib_ngc_deque_arr_deque_copyout_tsz"
// end of [deque_copyout_beg_tsz]

fun{a:t@ype}
deque_copyout
  {m,n:int}
  {i,k:nat | i+k <= n} (
  q: &DEQUE (a, m, n)
, i: size_t i
, k: size_t k
, xs: &(@[a?][k]) >> @[a][k]
) :<> void // end of [deque_copyout]

(* ****** ****** *)

fun{a:viewt@ype}
deque_update_capacity
  {m1,n:int}
  {m2:nat | n <= m2}
  {l:addr} (
  pfgc: free_gc_v (a?, m2, l)
, pfarr: array_v (a?, m2, l)
| q: &DEQUE (a, m1, n) >> DEQUE (a, m2, n)
, m2: size_t (m2), parr: ptr l
) : [l:addr] (
  free_gc_v (a?, m1, l), array_v (a?!, m1, l)
| ptr l
) // end of [deque_update_capcity]

fun
deque_update_capacity_tsz
  {a:viewt@ype}
  {m1,n:int}
  {m2:nat | n <= m2}
  {l:addr} (
  pfgc: free_gc_v (a?, m2, l)
, pfarr: array_v (a?, m2, l)
| q: &DEQUE (a, m1, n) >> DEQUE (a, m2, n)
, m2: size_t (m2), parr: ptr l
, tsz: sizeof_t (a)
) : [l:addr] (
  free_gc_v (a?, m1, l), array_v (a?!, m1, l)
| ptr l
) = "atslib_ngc_deque_arr_deque_update_capacity_tsz"

(* ****** ****** *)

(* end of [deque_arr.sats] *)
