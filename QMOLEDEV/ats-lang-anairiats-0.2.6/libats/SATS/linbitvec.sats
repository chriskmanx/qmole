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

%{#
#include "libats/CATS/linbitvec.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no dynamic loading

(* ****** ****** *)

abst@ype BITVEC (n:int) // an abstract type of unspecified size 

(* ****** ****** *)

typedef bit = [i:two] int (i) // i = 0 or 1

(* ****** ****** *)

fun bitvec_make {n:nat}
  (n: size_t n):<> [l:addr] (free_gc_v l, BITVEC (n) @ l | ptr l)
  = "atslib_linbitvec_bitvec_make"
// end of [bitvec_make]

fun bitvec_make_nil {n:nat}
  (n: size_t n):<> [l:addr] (free_gc_v l, BITVEC (n) @ l | ptr l)
  = "atslib_linbitvec_bitvec_make_nil"
// end of [bitvec_make_nil]

fun bitvec_make_all {n:nat}
  (n: size_t n):<> [l:addr] (free_gc_v l, BITVEC (n) @ l | ptr l)
  = "atslib_linbitvec_bitvec_make_all"
// end of [bitvec_make_all]

fun bitvec_free {n:nat} {l:addr}
  (pf_gc: free_gc_v l, pf_vec: BITVEC (n) @ l | p: ptr l):<> void  
  = "atslib_linbitvec_bitvec_free"
// end of [bitvec_free]

(* ****** ****** *)

fun bitvec_get_at {n,i:nat | i < n}
  (vec: &BITVEC n, i: size_t i):<> bit = "atslib_linbitvec_bitvec_get_at"
// end of [bitvec_get_at]

fun bitvec_set_at {n,i:nat | i < n}
  (vec: &BITVEC n, i: size_t i, b: bit):<> void = "atslib_linbitvec_bitvec_set_at"
// end of [bitvec_set_at]

overload [] with bitvec_get_at
overload [] with bitvec_set_at

(* ****** ****** *)

fun bitvec_is_nil {n:nat}
  (vec: &BITVEC n, n: size_t n): bool = "atslib_linbitvec_bitvec_is_nil"
// end of [bitvec_is_nil]

fun bitvec_isnot_nil {n:nat} (vec: &BITVEC n, n: size_t n): bool

(* ****** ****** *)

fun bitvec_is_all {n:nat}
  (vec: &BITVEC n, n: size_t n): bool = "atslib_linbitvec_bitvec_is_all"
// end of [linbitvec_bitvec_is_all]

fun bitvec_isnot_all {n:nat} (vec: &BITVEC n, n: size_t n): bool

(* ****** ****** *)

// vec1 = vec2 ?
fun bitvec_equal {n:nat}
  (vec1: &BITVEC n, vec2: &BITVEC n, n: size_t n):<> bool
  = "atslib_linbitvec_bitvec_equal"
// end of [bitvec_equal]

fun bitvec_notequal {n:nat}
  (vec1: &BITVEC n, vec2: &BITVEC n, n: size_t n):<> bool

(* ****** ****** *)

// vec1 <- vec2
fun bitvec_copy {n:nat}
  (vec1: &BITVEC n, vec2: &BITVEC n, n: size_t n):<> void
  = "atslib_linbitvec_bitvec_copy"
// end of [bitvec_copy]

(* ****** ****** *)

// complement operation
fun bitvec_neg {n:nat}
  (vec: &BITVEC n, n: size_t n): void
  = "atslib_linbitvec_bitvec_neg"
// end of [bitvec_neg]

fun bitvec_or {n:nat}
  (vec1: &BITVEC n, vec2: &BITVEC n, n: size_t n): void
  = "atslib_linbitvec_bitvec_or"
// end of [bitvec_or]

fun bitvec_and {n:nat}
  (vec1: &BITVEC n, vec2: &BITVEC n, n: size_t n): void
  = "atslib_linbitvec_bitvec_and"
// end of [bitvec_and]

fun bitvec_xor {n:nat}
  (vec1: &BITVEC n, vec2: &BITVEC n, n: size_t n): void
  = "atslib_linbitvec_bitvec_xor"
// end of [bitvec_xor]

fun bitvec_diff {n:nat}
  (vec1: &BITVEC n, vec2: &BITVEC n, n: size_t n): void
  = "atslib_linbitvec_bitvec_diff"
// end of [bitvec_diff]

(* ****** ****** *)

(* end of [linbitvec.sats] *)
