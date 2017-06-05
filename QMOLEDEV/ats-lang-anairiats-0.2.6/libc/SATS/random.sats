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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)
//
// Some simple random number generators
// For more realistic ones, please see the gsl_rng.h and gsl_randist.h
// in the GNU Science Library
//
(* ****** ****** *)

%{#
#include "libc/CATS/random.cats"
%} // end of [%{#]

(* ****** ****** *)
//
// typedef lint = long_int_t0ype // already defined elsewhere
//
// a seeding function
fun srand48 (li: lint): void = "atslib_srand48"
// using epoch time for the seed
fun srand48_with_time (): void = "atslib_srand48_with_time"
// using microsecond for the seed
fun srand48_with_gettimeofday (): int (*err*) // 0/-1: succ/fail

(* ****** ****** *)
//
// HX: the range of [drand48] is supposed to be [0.0, 1.0)
// However, it is actually [0.0, 1.0] // try 10 million times !!!
//
fun drand48 ():<!ref> double = "atslib_drand48"
fun lrand48 ():<!ref> lint = "atslib_lrand48" // signed [0, 2^31)
fun mrand48 ():<!ref> lint = "atslib_mrand48" // signed [-2^31, 2^31)

(* ****** ****** *)

abst@ype drand48_data = $extype"ats_drand48_data_type"

(* ****** ****** *)

fun srand48_r ( // the return is always 0
  seed: lint, buf: &drand48_data? >> drand48_data
) :<> int = "atslib_srand48_r" // end of [srand48_r]

(* ****** ****** *)

fun drand48_r ( // the return is always 0
  buf: &drand48_data, result: &double? >> double
) :<> int = "atslib_drand48_r" // end of [drand48_r]

fun lrand48_r ( // the return is always 0
  buf: &drand48_data, result: &lint? >> lint
) :<> int = "atslib_lrand48_r" // end of [lrand48_r]

fun mrand48_r ( // the return is always 0
  buf: &drand48_data, result: &lint? >> lint
) :<> int = "atslib_mrand48_r" // end of [mrand48_r]

(* ****** ****** *)
//
// HX: non-reentrant
//
fun randint {n:pos}
  (n: int n):<!ref> natLt n = "atslib_randint"
// end of [randint]

fun randsize {n:pos}
  (n: size_t n):<!ref> sizeLt n = "atslib_randsize"
// end of [randsize]

(* ****** ****** *)
//
// HX: [randperm] returns a randomly generated permutation
//
fun randperm {n:nat} (n: int n)
  :<!ref> [l:agz] (free_gc_v (int?, n, l), array_v (natLt n, n, l) | ptr l)
// end of [randperm]

(* ****** ****** *)
//
// HX: reentrant
//
fun randint_r {n:pos} (
  buf: &drand48_data, n: int n, res: &int? >> natLt n
) :<> void = "atslib_randint_r" // end of [randint_r]

fun randsize_r {n:pos} (
  buf: &drand48_data, n: size_t n, res: &size_t? >> sizeLt n
) :<> void = "atslib_randsize_r" // end of [randsize_r]

(* ****** ****** *)

(* end of [random.sats] *)
