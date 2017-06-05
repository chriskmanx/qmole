(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [integer_ptr.sats] starts!\n"

#endif

(* ****** ****** *)

//
// intptr:
// signed integers of the pointer size
//

(* ****** ****** *)

abstype intptr_int_type (i:int) = intptr_type
stadef intptr = intptr_int_type
typedef intptr = intptr_type

(* ****** ****** *)

fun int_of_intptr
  (i: intptr):<> int = "atspre_int_of_intptr"
overload int_of with int_of_intptr

fun lint_of_intptr
  (i: intptr):<> lint = "atspre_lint_of_intptr"
overload lint_of with lint_of_intptr

(* ****** ****** *)

symintr intptr_of

fun intptr_of_int
  (i: int):<> intptr = "atspre_intptr_of_int"
overload intptr_of with intptr_of_int

fun intptr_of_lint
  (i: lint):<> intptr = "atspre_intptr_of_lint"
overload intptr_of with intptr_of_lint

(* ****** ****** *)

castfn ptr_of_intptr (i: intptr):<> ptr // "atspre_ptr_of_intptr"(removed)
overload ptr_of with ptr_of_intptr
castfn intptr_of_ptr (p: ptr):<> intptr // = "atspre_intptr_of_ptr"(removed)
overload intptr_of with intptr_of_ptr

(* ****** ****** *)

fun abs_intptr (i: intptr):<> intptr
  = "atspre_abs_intptr"
overload abs with abs_intptr

fun neg_intptr (i: intptr):<> intptr
  = "atspre_neg_intptr"
overload ~ with neg_intptr

fun succ_intptr (i: intptr):<> intptr
  = "atspre_succ_intptr"

and pred_intptr (i: intptr):<> intptr
  = "atspre_pred_intptr"

overload succ with succ_intptr
overload pred with pred_intptr

//

fun add_intptr_int (i: intptr, j: int):<> intptr
  = "atspre_add_intptr_int"
overload + with add_intptr_int

fun add_intptr_intptr (i: intptr, j: intptr):<> intptr
  = "atspre_add_intptr_intptr"
overload + with add_intptr_intptr

//

fun sub_intptr_int (i: intptr, j: int):<> intptr
  = "atspre_sub_intptr_int"
overload - with sub_intptr_int

fun sub_intptr_intptr (i: intptr, j: intptr):<> intptr
  = "atspre_sub_intptr_intptr"
overload - with sub_intptr_intptr

//

fun mul_intptr_int (i: intptr, j: int):<> intptr
  = "atspre_mul_intptr_int"
overload * with mul_intptr_int

fun mul_intptr_intptr (i: intptr, j: intptr):<> intptr
  = "atspre_mul_intptr_intptr"
overload * with mul_intptr_intptr

//

fun div_intptr_int (i: intptr, j: int):<> intptr
  = "atspre_div_intptr_int"
overload / with div_intptr_int

fun div_intptr_intptr (i: intptr, j: intptr):<> intptr
  = "atspre_div_intptr_intptr"
overload / with div_intptr_intptr

//

fun mod_intptr_int (i: intptr, j: int):<> intptr
  = "atspre_mod_intptr_int"
overload mod with mod_intptr_int

fun mod_intptr_intptr (i: intptr, j: intptr):<> intptr
  = "atspre_mod_intptr_intptr"
overload mod with mod_intptr_intptr

//

fun lt_intptr_intptr (i: intptr, j: intptr):<> bool
  = "atspre_lt_intptr_intptr"
overload < with lt_intptr_intptr

fun lte_intptr_intptr (i: intptr, j: intptr):<> bool
  = "atspre_lte_intptr_intptr"
overload <= with lte_intptr_intptr

//

fun gt_intptr_intptr (i: intptr, j: intptr):<> bool
  = "atspre_gt_intptr_intptr"
overload > with gt_intptr_intptr

fun gte_intptr_intptr (i: intptr, j: intptr):<> bool
  = "atspre_gte_intptr_intptr"
overload >= with gte_intptr_intptr

//

fun eq_intptr_intptr (i: intptr, j: intptr):<> bool
  = "atspre_eq_intptr_intptr"
overload = with eq_intptr_intptr

fun neq_intptr_intptr (i: intptr, j: intptr):<> bool
  = "atspre_neq_intptr_intptr"
overload <> with neq_intptr_intptr

//

fun max_intptr_intptr (i: intptr, j: intptr):<> intptr
  = "atspre_max_intptr_intptr"
overload max with max_intptr_intptr

fun min_intptr_intptr (i: intptr, j: intptr):<> intptr
  = "atspre_min_intptr_intptr"
overload min with min_intptr_intptr

(* ****** ****** *)

symintr fprint_intptr

fun fprint0_intptr (out: FILEref, x: intptr):<!exnref> void
  = "atspre_fprint_intptr"

fun fprint1_intptr {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: intptr):<!exnref> void
  = "atspre_fprint_intptr"

overload fprint_intptr with fprint0_intptr
overload fprint_intptr with fprint1_intptr
overload fprint with fprint_intptr

(* ****** ****** *)

fun print_intptr (i: intptr):<!ref> void = "atspre_print_intptr"
and prerr_intptr (i: intptr):<!ref> void = "atspre_prerr_intptr"

overload print with print_intptr
overload prerr with prerr_intptr

(* ****** ****** *)

fun intptr1_of_int1 {i:nat} (i: int i): intptr i
  = "atspre_intptr_of_int"

fun int1_of_intptr1 {i:nat} (i: intptr i): int i
  = "atspre_int_of_intptr"

(* ****** ****** *)

fun lt_intptr1_intptr1
  {i1,i2:nat} (i1: intptr i1, i2: intptr i2):<> bool (i1 < i2)
  = "atspre_lt_intptr_intptr"
overload < with lt_intptr1_intptr1

fun lte_intptr1_intptr1
  {i1,i2:nat} (i1: intptr i1, i2: intptr i2):<> bool (i1 <= i2)
  = "atspre_lte_intptr_intptr"
overload <= with lte_intptr1_intptr1

fun gt_intptr1_intptr1
  {i1,i2:nat} (i1: intptr i1, i2: intptr i2):<> bool (i1 > i2)
  = "atspre_gt_intptr_intptr"
overload > with gt_intptr1_intptr1

fun gte_intptr1_intptr1
  {i1,i2:nat} (i1: intptr i1, i2: intptr i2):<> bool (i1 >= i2)
  = "atspre_gte_intptr_intptr"
overload >= with gte_intptr1_intptr1

fun eq_intptr1_intptr1
  {i1,i2:nat} (i1: intptr i1, i2: intptr i2):<> bool (i1 == i2)
  = "atspre_eq_intptr_intptr"
overload = with eq_intptr1_intptr1

fun neq_intptr1_intptr1
  {i1,i2:nat} (i1: intptr i1, i2: intptr i2):<> bool (i1 <> i2)
  = "atspre_neq_intptr_intptr"
overload <> with neq_intptr1_intptr1

(* ****** ****** *)

//
// uintptr:
// unsigned integers of the pointer size
//

abstype uintptr_int_type (i:int) = uintptr_type
stadef uintptr = uintptr_int_type
typedef uintptr = uintptr_type

(* ****** ****** *)

symintr uintptr_of

fun uint_of_uintptr
  (u: uintptr):<> uint = "atspre_uint_of_uintptr"
overload uint_of with uint_of_uintptr

fun uintptr_of_int1
  {i:nat} (i: int i):<> uintptr = "atspre_uintptr_of_int1"
overload uintptr_of with uintptr_of_int1

fun uintptr_of_uint (u: uint):<> uintptr = "atspre_uintptr_of_uint"
overload uintptr_of with uintptr_of_uint

(* ****** ****** *)

fun ulint_of_uintptr
  (u: uintptr):<> ulint = "atspre_ulint_of_uintptr"

fun uintptr_of_ulint (u: ulint):<> uintptr
  = "atspre_uintptr_of_ulint"
overload uintptr_of with uintptr_of_ulint

(* ****** ****** *)

castfn ptr_of_uintptr (u: uintptr):<> ptr // = "atspre_ptr_of_uintptr"(removed)
overload ptr_of with ptr_of_uintptr
castfn uintptr_of_ptr (p: ptr):<> uintptr // = "atspre_uintptr_of_ptr"(removed)
overload uintptr_of with uintptr_of_ptr

(* ****** ****** *)

// arithmetic functions and comparison functions

fun succ_uintptr (u: uintptr):<> uintptr
  = "atspre_succ_uintptr"

and pred_uintptr (u: uintptr):<> uintptr
  = "atspre_pred_uintptr"

overload succ with succ_uintptr
overload pred with pred_uintptr

//

fun add_uintptr_uint (i: uintptr, j: uint):<> uintptr
  = "atspre_add_uintptr_uint"
overload + with add_uintptr_uint

fun add_uintptr_uintptr (i: uintptr, j: uintptr):<> uintptr
  = "atspre_add_uintptr_uintptr"
overload + with add_uintptr_uintptr

//

fun sub_uintptr_uint (i: uintptr, j: uint):<> uintptr
  = "atspre_sub_uintptr_uint"
overload - with sub_uintptr_uint

fun sub_uintptr_uintptr (i: uintptr, j: uintptr):<> uintptr
  = "atspre_sub_uintptr_uintptr"
overload - with sub_uintptr_uintptr

//

fun mul_uintptr_uint (i: uintptr, j: uint):<> uintptr
  = "atspre_mul_uintptr_uint"
overload * with mul_uintptr_uint

fun mul_uintptr_uintptr (i: uintptr, j: uintptr):<> uintptr
  = "atspre_mul_uintptr_uintptr"
overload * with mul_uintptr_uintptr

//

fun div_uintptr_uint (i: uintptr, j: uint):<> uintptr
  = "atspre_div_uintptr_uint"
overload / with div_uintptr_uint

fun div_uintptr_uintptr (i: uintptr, j: uintptr):<> uintptr
  = "atspre_div_uintptr_uintptr"
overload / with div_uintptr_uintptr

//

fun mod_uintptr_uint (i: uintptr, j: uint):<> uintptr
  = "atspre_mod_uintptr_uint"
overload mod with mod_uintptr_uint

fun mod_uintptr_uintptr (i: uintptr, j: uintptr):<> uintptr
  = "atspre_mod_uintptr_uintptr"
overload mod with mod_uintptr_uintptr

// comparision operations on uintptr

fun lt_uintptr_uintptr (i: uintptr, j: uintptr):<> bool
  = "atspre_lt_uintptr_uintptr"

and lte_uintptr_uintptr (i: uintptr, j: uintptr):<> bool
  = "atspre_lte_uintptr_uintptr"

fun gt_uintptr_uintptr (i: uintptr, j: uintptr):<> bool
  = "atspre_gt_uintptr_uintptr"

and gte_uintptr_uintptr (i: uintptr, j: uintptr):<> bool
  = "atspre_gte_uintptr_uintptr"

fun eq_uintptr_uintptr (i: uintptr, j: uintptr):<> bool
  = "atspre_eq_uintptr_uintptr"

and neq_uintptr_uintptr (i: uintptr, j: uintptr):<> bool
  = "atspre_neq_uintptr_uintptr"

overload < with lt_uintptr_uintptr
overload <= with lte_uintptr_uintptr
overload > with gt_uintptr_uintptr
overload >= with gte_uintptr_uintptr
overload = with eq_uintptr_uintptr
overload <> with neq_uintptr_uintptr

fun max_uintptr_uintptr (i: uintptr, j: uintptr):<> uintptr
  = "atspre_max_uintptr_uintptr"

and min_uintptr_uintptr (i: uintptr, j: uintptr):<> uintptr
  = "atspre_min_uintptr_uintptr"

overload max with max_uintptr_uintptr
overload min with min_uintptr_uintptr

(* ****** ****** *)

// bit operations

fun lnot_uintptr (u: uintptr):<> uintptr
  = "atspre_lnot_uintptr" (* bitwise *)
overload ~ with lnot_uintptr

fun land_uintptr_uintptr (u1: uintptr, u2: uintptr):<> uintptr
  = "atspre_land_uintptr_uintptr"

fun lor_uintptr_uintptr (u1: uintptr, u2: uintptr):<> uintptr
  = "atspre_lor_uintptr_uintptr"

fun lxor_uintptr_uintptr (u1: uintptr, u2: uintptr):<> uintptr
  = "atspre_lxor_uintptr_uintptr"

overload land with land_uintptr_uintptr
overload lor with lor_uintptr_uintptr
overload lxor with lxor_uintptr_uintptr

fun lsl_uintptr_int1 (u: uintptr, n: Nat):<> uintptr
  = "atspre_lsl_uintptr_int1"

and lsr_uintptr_int1 (u: uintptr, n: Nat):<> uintptr
  = "atspre_lsr_uintptr_int1"

overload << with lsl_uintptr_int1
overload >> with lsr_uintptr_int1

(* ****** ****** *)

symintr fprint_uintptr

fun fprint0_uintptr (out: FILEref, x: uintptr):<!exnref> void
  = "atspre_fprint_uintptr"

fun fprint1_uintptr {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: uintptr):<!exnref> void
  = "atspre_fprint_uintptr"

overload fprint_uintptr with fprint0_uintptr
overload fprint_uintptr with fprint1_uintptr
overload fprint with fprint_uintptr

(* ****** ****** *)

fun print_uintptr (u: uintptr):<!ref> void
  = "atspre_print_uintptr"

and prerr_uintptr (u: uintptr):<!ref> void
  = "atspre_prerr_uintptr"

overload print with print_uintptr
overload prerr with prerr_uintptr

(* ****** ****** *)

fun uintptr1_of_uint1 {i:nat} (u: uint i): uintptr i
  = "atspre_uintptr_of_uint"

fun uint1_of_uintptr1 {i:nat} (u: uintptr i): uint i
  = "atspre_uint_of_uintptr"

(* ****** ****** *)

fun lt_uintptr1_uintptr1
  {i1,i2:nat} (u1: uintptr i1, u2: uintptr i2):<> bool (i1 < i2)
  = "atspre_lt_uintptr_uintptr"
overload < with lt_uintptr1_uintptr1

fun lte_uintptr1_uintptr1
  {i1,i2:nat} (u1: uintptr i1, u2: uintptr i2):<> bool (i1 <= i2)
  = "atspre_lte_uintptr_uintptr"
overload <= with lte_uintptr1_uintptr1

fun gt_uintptr1_uintptr1
  {i1,i2:nat} (u1: uintptr i1, u2: uintptr i2):<> bool (i1 > i2)
  = "atspre_gt_uintptr_uintptr"
overload > with gt_uintptr1_uintptr1

fun gte_uintptr1_uintptr1
  {i1,i2:nat} (u1: uintptr i1, u2: uintptr i2):<> bool (i1 >= i2)
  = "atspre_gte_uintptr_uintptr"
overload >= with gte_uintptr1_uintptr1

fun eq_uintptr1_uintptr1
  {i1,i2:nat} (u1: uintptr i1, u2: uintptr i2):<> bool (i1 == i2)
  = "atspre_eq_uintptr_uintptr"
overload = with eq_uintptr1_uintptr1

fun neq_uintptr1_uintptr1
  {i1,i2:nat} (u1: uintptr i1, u2: uintptr i2):<> bool (i1 <> i2)
  = "atspre_neq_uintptr_uintptr"
overload <> with neq_uintptr1_uintptr1

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [integer_ptr.sats] finishes!\n"

#endif

(* end of [integer_ptr.sats] *)
