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
#print "Loading [integer_fixed.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

// signed and unsigned integers with fixed size

(* ****** ****** *)

typedef int8 = int8_t0ype

fun int8_of_int (i: int):<> int8 = "atspre_int8_of_int"
fun int_of_int8 (i: int8):<> int = "atspre_int_of_int8"

// ------ ------

fun abs_int8 (i: int8):<> int8 = "atspre_abs_int8"
overload abs with abs_int8

fun neg_int8 (i: int8):<> int8 = "atspre_neg_int8"
overload ~ with neg_int8

fun succ_int8 (i: int8):<> int8 = "atspre_succ_int8"
and pred_int8 (i: int8):<> int8 = "atspre_pred_int8"
overload succ with succ_int8
overload pred with pred_int8

fun add_int8_int8 (i: int8, j: int8):<> int8
 = "atspre_add_int8_int8"

and sub_int8_int8 (i: int8, j: int8):<> int8
  = "atspre_sub_int8_int8"

and mul_int8_int8 (i: int8, j: int8):<> int8
  = "atspre_mul_int8_int8"

and div_int8_int8 (i: int8, j: int8):<> int8
  = "atspre_div_int8_int8"

and mod_int8_int8 (i: int8, j: int8):<> int8
  = "atspre_mod_int8_int8"

overload + with add_int8_int8
overload - with sub_int8_int8
overload * with mul_int8_int8
overload / with div_int8_int8
overload mod with mod_int8_int8 

fun lt_int8_int8 (i: int8, j: int8):<> bool
  = "atspre_lt_int8_int8"

and lte_int8_int8 (i: int8, j: int8):<> bool
  = "atspre_lte_int8_int8"

fun gt_int8_int8 (i: int8, j: int8):<> bool
  = "atspre_gt_int8_int8"

and gte_int8_int8 (i: int8, j: int8):<> bool
  = "atspre_gte_int8_int8"

fun eq_int8_int8 (i: int8, j: int8):<> bool
  = "atspre_eq_int8_int8"

and neq_int8_int8 (i: int8, j: int8):<> bool
  = "atspre_neq_int8_int8"

overload < with lt_int8_int8
overload <= with lte_int8_int8
overload > with gt_int8_int8
overload >= with gte_int8_int8
overload = with eq_int8_int8
overload <> with neq_int8_int8

fun max_int8_int8 (i: int8, j: int8):<> int8
  = "atspre_max_int8_int8"

and min_int8_int8 (i: int8, j: int8):<> int8
  = "atspre_min_int8_int8"

overload max with max_int8_int8
overload min with min_int8_int8

(* ****** ****** *)

symintr fprint_int8

fun fprint0_int8 (out: FILEref, x: int8):<!exnref> void
  = "atspre_fprint_int8"
overload fprint_int8 with fprint0_int8
fun fprint1_int8 {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: int8):<!exnref> void
  = "atspre_fprint_int8"
overload fprint_int8 with fprint1_int8

overload fprint with fprint_int8

fun print_int8 (i: int8):<!ref> void = "atspre_print_int8"
and prerr_int8 (i: int8):<!ref> void = "atspre_prerr_int8"
overload print with print_int8
overload prerr with prerr_int8

(* ****** ****** *)

typedef uint8 = uint8_t0ype

fun uint8_of_uint (i: uint):<> uint8 = "atspre_uint8_of_uint"
fun uint_of_uint8 (i: uint8):<> uint = "atspre_uint_of_uint8"

// ------ ------

fun succ_uint8 (i: uint8):<> uint8 = "atspre_succ_uint8"
and pred_uint8 (i: uint8):<> uint8 = "atspre_pred_uint8"
overload succ with succ_uint8
overload pred with pred_uint8

fun add_uint8_uint8 (i: uint8, j: uint8):<> uint8
 = "atspre_add_uint8_uint8"

and sub_uint8_uint8 (i: uint8, j: uint8):<> uint8
  = "atspre_sub_uint8_uint8"

and mul_uint8_uint8 (i: uint8, j: uint8):<> uint8
  = "atspre_mul_uint8_uint8"

and div_uint8_uint8 (i: uint8, j: uint8):<> uint8
  = "atspre_div_uint8_uint8"

and mod_uint8_uint8 (i: uint8, j: uint8):<> uint8
  = "atspre_mod_uint8_uint8"

overload + with add_uint8_uint8
overload - with sub_uint8_uint8
overload * with mul_uint8_uint8
overload / with div_uint8_uint8
overload mod with mod_uint8_uint8 

fun lt_uint8_uint8 (i: uint8, j: uint8):<> bool
  = "atspre_lt_uint8_uint8"

and lte_uint8_uint8 (i: uint8, j: uint8):<> bool
  = "atspre_lte_uint8_uint8"

fun gt_uint8_uint8 (i: uint8, j: uint8):<> bool
  = "atspre_gt_uint8_uint8"

and gte_uint8_uint8 (i: uint8, j: uint8):<> bool
  = "atspre_gte_uint8_uint8"

fun eq_uint8_uint8 (i: uint8, j: uint8):<> bool
  = "atspre_eq_uint8_uint8"

and neq_uint8_uint8 (i: uint8, j: uint8):<> bool
  = "atspre_neq_uint8_uint8"

overload < with lt_uint8_uint8
overload <= with lte_uint8_uint8
overload > with gt_uint8_uint8
overload >= with gte_uint8_uint8
overload = with eq_uint8_uint8
overload <> with neq_uint8_uint8

fun max_uint8_uint8 (i: uint8, j: uint8):<> uint8
  = "atspre_max_uint8_uint8"

and min_uint8_uint8 (i: uint8, j: uint8):<> uint8
  = "atspre_min_uint8_uint8"

overload max with max_uint8_uint8
overload min with min_uint8_uint8

(* ****** ****** *)

symintr fprint_uint8

fun fprint0_uint8 (out: FILEref, x: uint8):<!exnref> void
  = "atspre_fprint_uint8"
overload fprint_uint8 with fprint0_uint8
fun fprint1_uint8 {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: uint8):<!exnref> void
  = "atspre_fprint_uint8"
overload fprint_uint8 with fprint1_uint8

overload fprint with fprint_uint8

fun print_uint8 (i: uint8):<!ref> void = "atspre_print_uint8"
and prerr_uint8 (i: uint8):<!ref> void = "atspre_prerr_uint8"
overload print with print_uint8
overload prerr with prerr_uint8

(* ****** ****** *)

// signed integer of size 16bit

typedef int16 = int16_t0ype

fun int16_of_int (i: int):<> int16 = "atspre_int16_of_int"
fun int_of_int16 (i: int16):<> int = "atspre_int_of_int16"

// ------ ------

fun abs_int16 (i: int16):<> int16 = "atspre_abs_int16"
overload abs with abs_int16

fun neg_int16 (i: int16):<> int16 = "atspre_neg_int16"
overload ~ with neg_int16

fun succ_int16 (i: int16):<> int16 = "atspre_succ_int16"
and pred_int16 (i: int16):<> int16 = "atspre_pred_int16" 
overload succ with succ_int16
overload pred with pred_int16

fun add_int16_int16 (i: int16, j: int16):<> int16
  = "atspre_add_int16_int16"

and sub_int16_int16 (i: int16, j: int16):<> int16
  = "atspre_sub_int16_int16"

and mul_int16_int16 (i: int16, j: int16):<> int16
  = "atspre_mul_int16_int16"

and div_int16_int16 (i: int16, j: int16):<> int16
  = "atspre_div_int16_int16"

and mod_int16_int16 (i: int16, j: int16):<> int16
  = "atspre_mod_int16_int16"

overload + with add_int16_int16
overload - with sub_int16_int16
overload * with mul_int16_int16
overload / with div_int16_int16
overload mod with mod_int16_int16

fun lt_int16_int16 (i: int16, j: int16):<> bool
  = "atspre_lt_int16_int16"

and lte_int16_int16 (i: int16, j: int16):<> bool
  = "atspre_lte_int16_int16"

fun gt_int16_int16 (i: int16, j: int16):<> bool
  = "atspre_gt_int16_int16"

and gte_int16_int16 (i: int16, j: int16):<> bool
  = "atspre_gte_int16_int16"

fun eq_int16_int16 (i: int16, j: int16):<> bool
  = "atspre_eq_int16_int16"

and neq_int16_int16 (i: int16, j: int16):<> bool
  = "atspre_neq_int16_int16"

overload < with lt_int16_int16
overload <= with lte_int16_int16
overload > with gt_int16_int16
overload >= with gte_int16_int16
overload = with eq_int16_int16
overload <> with neq_int16_int16

fun max_int16_int16 (i: int16, j: int16):<> int16
  = "atspre_max_int16_int16"

and min_int16_int16 (i: int16, j: int16):<> int16
  = "atspre_min_int16_int16"

overload max with max_int16_int16
overload min with min_int16_int16

(* ****** ****** *)

symintr fprint_int16

fun fprint0_int16 (out: FILEref, x: int16):<!exnref> void
  = "atspre_fprint_int16"
overload fprint_int16 with fprint0_int16
fun fprint1_int16 {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: int16):<!exnref> void
  = "atspre_fprint_int16"
overload fprint_int16 with fprint1_int16

overload fprint with fprint_int16

fun print_int16 (i: int16):<!ref> void = "atspre_print_int16"
and prerr_int16 (i: int16):<!ref> void = "atspre_prerr_int16"
overload print with print_int16
overload prerr with prerr_int16

(* ****** ****** *)

// unsigned integer of size 16bit

typedef uint16 = uint16_t0ype

fun uint16_of_int (i: int):<> uint16 = "atspre_uint16_of_int"
fun int_of_uint16 (u: uint16):<> int = "atspre_int_of_uint16"
fun uint16_of_uint (i: uint):<> uint16 = "atspre_uint16_of_uint"
fun uint_of_uint16 (i: uint16):<> uint = "atspre_uint_of_uint16"

// ------ ------

fun succ_uint16 (i: uint16):<> uint16 = "atspre_succ_uint16"
and pred_uint16 (i: uint16):<> uint16 = "atspre_pred_uint16"
overload succ with succ_uint16
overload pred with pred_uint16

fun add_uint16_uint16 (i: uint16, j: uint16):<> uint16
 = "atspre_add_uint16_uint16"

and sub_uint16_uint16 (i: uint16, j: uint16):<> uint16
  = "atspre_sub_uint16_uint16"

and mul_uint16_uint16 (i: uint16, j: uint16):<> uint16
  = "atspre_mul_uint16_uint16"

and div_uint16_uint16 (i: uint16, j: uint16):<> uint16
  = "atspre_div_uint16_uint16"

and mod_uint16_uint16 (i: uint16, j: uint16):<> uint16
  = "atspre_mod_uint16_uint16"

overload + with add_uint16_uint16
overload - with sub_uint16_uint16
overload * with mul_uint16_uint16
overload / with div_uint16_uint16
overload mod with mod_uint16_uint16 

fun lt_uint16_uint16 (i: uint16, j: uint16):<> bool
  = "atspre_lt_uint16_uint16"

and lte_uint16_uint16 (i: uint16, j: uint16):<> bool
  = "atspre_lte_uint16_uint16"

fun gt_uint16_uint16 (i: uint16, j: uint16):<> bool
  = "atspre_gt_uint16_uint16"

and gte_uint16_uint16 (i: uint16, j: uint16):<> bool
  = "atspre_gte_uint16_uint16"

fun eq_uint16_uint16 (i: uint16, j: uint16):<> bool
  = "atspre_eq_uint16_uint16"

and neq_uint16_uint16 (i: uint16, j: uint16):<> bool
  = "atspre_neq_uint16_uint16"

overload < with lt_uint16_uint16
overload <= with lte_uint16_uint16
overload > with gt_uint16_uint16
overload >= with gte_uint16_uint16
overload = with eq_uint16_uint16
overload <> with neq_uint16_uint16

fun max_uint16_uint16 (i: uint16, j: uint16):<> uint16
  = "atspre_max_uint16_uint16"

and min_uint16_uint16 (i: uint16, j: uint16):<> uint16
  = "atspre_min_uint16_uint16"

overload max with max_uint16_uint16
overload min with min_uint16_uint16

(* ****** ****** *)

symintr fprint_uint16

fun fprint0_uint16 (out: FILEref, x: uint16):<!exnref> void
  = "atspre_fprint_uint16"
overload fprint_uint16 with fprint0_uint16
fun fprint1_uint16 {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: uint16):<!exnref> void
  = "atspre_fprint_uint16"
overload fprint_uint16 with fprint1_uint16

overload fprint with fprint_uint16

fun print_uint16 (i: uint16):<!ref> void = "atspre_print_uint16"
and prerr_uint16 (i: uint16):<!ref> void = "atspre_prerr_uint16"
overload print with print_uint16
overload prerr with prerr_uint16

(* ****** ****** *)

// signed integer of size 32bit

typedef int32 = int32_t0ype

fun int32_of_int (i: int):<> int32 = "atspre_int32_of_int"
fun int_of_int32 (i: int32):<> int = "atspre_int_of_int32"

// ------ ------

fun abs_int32 (i: int32):<> int32 = "atspre_abs_int32"
overload abs with abs_int32

fun neg_int32 (i: int32):<> int32 = "atspre_neg_int32"
overload ~ with neg_int32

fun succ_int32 (i: int32):<> int32 = "atspre_succ_int32"
and pred_int32 (i: int32):<> int32 = "atspre_pred_int32"
overload succ with succ_int32
overload pred with pred_int32

fun add_int32_int32 (i: int32, j: int32):<> int32
  = "atspre_add_int32_int32"

and sub_int32_int32 (i: int32, j: int32):<> int32
  = "atspre_sub_int32_int32"

and mul_int32_int32 (i: int32, j: int32):<> int32
  = "atspre_mul_int32_int32"

and div_int32_int32 (i: int32, j: int32):<> int32
  = "atspre_div_int32_int32"

and mod_int32_int32 (i: int32, j: int32):<> int32
  = "atspre_mod_int32_int32"

overload + with add_int32_int32
overload - with sub_int32_int32
overload * with mul_int32_int32
overload / with div_int32_int32
overload mod with mod_int32_int32

fun lt_int32_int32 (i: int32, j: int32):<> bool
  = "atspre_lt_int32_int32"
and lte_int32_int32 (i: int32, j: int32):<> bool
  = "atspre_lte_int32_int32"
fun gt_int32_int32 (i: int32, j: int32):<> bool
  = "atspre_gt_int32_int32"
and gte_int32_int32 (i: int32, j: int32):<> bool
 = "atspre_gte_int32_int32"

fun eq_int32_int32 (i: int32, j: int32):<> bool
  = "atspre_eq_int32_int32"

and neq_int32_int32 (i: int32, j: int32):<> bool
  = "atspre_neq_int32_int32"

overload < with lt_int32_int32
overload <= with lte_int32_int32
overload > with gt_int32_int32
overload >= with gte_int32_int32
overload = with eq_int32_int32
overload <> with neq_int32_int32

fun compare_int32_int32 (i1: int32, i2: int32):<> Sgn
  = "atspre_compare_int32_int32"
overload compare with compare_int32_int32

fun max_int32_int32 (i: int32, j: int32):<> int32
  = "atspre_max_int32_int32"

and min_int32_int32 (i: int32, j: int32):<> int32
  = "atspre_min_int32_int32"

overload max with max_int32_int32
overload min with min_int32_int32

(* ****** ****** *)

symintr fprint_int32

fun fprint0_int32 (out: FILEref, x: int32):<!exnref> void
  = "atspre_fprint_int32"
overload fprint_int32 with fprint0_int32

fun fprint1_int32 {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: int32):<!exnref> void
  = "atspre_fprint_int32"
overload fprint_int32 with fprint1_int32

overload fprint with fprint_int32

fun print_int32 (i: int32):<!ref> void = "atspre_print_int32"
and prerr_int32 (i: int32):<!ref> void = "atspre_prerr_int32"
overload print with print_int32
overload prerr with prerr_int32

(* ****** ****** *)
//
// stringization
//
fun tostrptr_int32
  (i: int32):<> strptr1 = "atspre_tostrptr_int32"
overload tostrptr with tostrptr_int32
fun tostring_int32 (i: int32):<> string = "atspre_tostrptr_int32"
overload tostring with tostring_int32

(* ****** ****** *)

// unsigned integer of size 32bit

typedef uint32 = uint32_t0ype

fun uint32_of_uint (i: uint):<> uint32 = "atspre_uint32_of_uint"
fun uint_of_uint32 (i: uint32):<> uint = "atspre_uint_of_uint32"

// ------ ------

fun succ_uint32 (i: uint32):<> uint32 = "atspre_succ_uint32"
and pred_uint32 (i: uint32):<> uint32 = "atspre_pred_uint32"
overload succ with succ_uint32
overload pred with pred_uint32

fun add_uint32_uint32 (i: uint32, j: uint32):<> uint32
 = "atspre_add_uint32_uint32"

and sub_uint32_uint32 (i: uint32, j: uint32):<> uint32
  = "atspre_sub_uint32_uint32"

and mul_uint32_uint32 (i: uint32, j: uint32):<> uint32
  = "atspre_mul_uint32_uint32"

and div_uint32_uint32 (i: uint32, j: uint32):<> uint32
  = "atspre_div_uint32_uint32"

and mod_uint32_uint32 (i: uint32, j: uint32):<> uint32
  = "atspre_mod_uint32_uint32"

overload + with add_uint32_uint32
overload - with sub_uint32_uint32
overload * with mul_uint32_uint32
overload / with div_uint32_uint32
overload mod with mod_uint32_uint32 

fun lt_uint32_uint32 (i: uint32, j: uint32):<> bool
  = "atspre_lt_uint32_uint32"

and lte_uint32_uint32 (i: uint32, j: uint32):<> bool
  = "atspre_lte_uint32_uint32"

fun gt_uint32_uint32 (i: uint32, j: uint32):<> bool
  = "atspre_gt_uint32_uint32"

and gte_uint32_uint32 (i: uint32, j: uint32):<> bool
  = "atspre_gte_uint32_uint32"

fun eq_uint32_uint32 (i: uint32, j: uint32):<> bool
  = "atspre_eq_uint32_uint32"

and neq_uint32_uint32 (i: uint32, j: uint32):<> bool
  = "atspre_neq_uint32_uint32"

overload < with lt_uint32_uint32
overload <= with lte_uint32_uint32
overload > with gt_uint32_uint32
overload >= with gte_uint32_uint32
overload = with eq_uint32_uint32
overload <> with neq_uint32_uint32

fun max_uint32_uint32 (i: uint32, j: uint32):<> uint32
  = "atspre_max_uint32_uint32"

and min_uint32_uint32 (i: uint32, j: uint32):<> uint32
  = "atspre_min_uint32_uint32"

overload max with max_uint32_uint32
overload min with min_uint32_uint32

(* ****** ****** *)

symintr fprint_uint32

fun fprint0_uint32 (out: FILEref, x: uint32):<!exnref> void
  = "atspre_fprint_uint32"
overload fprint_uint32 with fprint0_uint32

fun fprint1_uint32 {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: uint32):<!exnref> void
  = "atspre_fprint_uint32"
overload fprint_uint32 with fprint1_uint32

overload fprint with fprint_uint32

fun print_uint32 (i: uint32):<!ref> void = "atspre_print_uint32"
and prerr_uint32 (i: uint32):<!ref> void = "atspre_prerr_uint32"
overload print with print_uint32
overload prerr with prerr_uint32

(* ****** ****** *)

// signed integer of size 64bit

typedef int64 = int64_t0ype

symintr int64_of
fun int64_of_int (i: int):<> int64 = "atspre_int64_of_int"
overload int64_of with int64_of_int
fun int64_of_lint (i: lint):<> int64 = "atspre_int64_of_lint"
overload int64_of with int64_of_lint
fun int64_of_llint (i: llint):<> int64 = "atspre_int64_of_llint"
overload int64_of with int64_of_llint

fun int_of_int64 (i: int64):<> int = "atspre_int_of_int64"

// ------ ------

fun abs_int64 (i: int64):<> int64 = "atspre_abs_int64"
overload abs with abs_int64

fun neg_int64 (i: int64):<> int64 = "atspre_neg_int64"
overload ~ with neg_int64

fun succ_int64 (i: int64):<> int64 = "atspre_succ_int64"
and pred_int64 (i: int64):<> int64 = "atspre_pred_int64"
overload succ with succ_int64
overload pred with pred_int64

fun add_int64_int64 (i: int64, j: int64):<> int64
  = "atspre_add_int64_int64"

and sub_int64_int64 (i: int64, j: int64):<> int64
  = "atspre_sub_int64_int64"

and mul_int64_int64 (i: int64, j: int64):<> int64
  = "atspre_mul_int64_int64"

and div_int64_int64 (i: int64, j: int64):<> int64
  = "atspre_div_int64_int64"

and mod_int64_int64 (i: int64, j: int64):<> int64
  = "atspre_mod_int64_int64"

overload + with add_int64_int64
overload - with sub_int64_int64
overload * with mul_int64_int64
overload / with div_int64_int64
overload mod with mod_int64_int64

fun lt_int64_int64 (i: int64, j: int64):<> bool
  = "atspre_lt_int64_int64"

and lte_int64_int64 (i: int64, j: int64):<> bool
  = "atspre_lte_int64_int64"

fun gt_int64_int64 (i: int64, j: int64):<> bool
  = "atspre_gt_int64_int64"

and gte_int64_int64 (i: int64, j: int64):<> bool
  = "atspre_gte_int64_int64"

fun eq_int64_int64 (i: int64, j: int64):<> bool
  = "atspre_eq_int64_int64"

and neq_int64_int64 (i: int64, j: int64):<> bool
  = "atspre_neq_int64_int64"

overload < with lt_int64_int64
overload <= with lte_int64_int64
overload > with gt_int64_int64
overload >= with gte_int64_int64
overload = with eq_int64_int64
overload <> with neq_int64_int64

fun compare_int64_int64 (i1: int64, i2: int64):<> Sgn
  = "atspre_compare_int64_int64"
overload compare with compare_int64_int64

fun max_int64_int64 (i: int64, j: int64):<> int64
  = "atspre_max_int64_int64"
and min_int64_int64 (i: int64, j: int64):<> int64
  = "atspre_min_int64_int64"

overload max with max_int64_int64
overload min with min_int64_int64

(* ****** ****** *)

symintr fprint_int64

fun fprint0_int64 (out: FILEref, x: int64):<!exnref> void
  = "atspre_fprint_int64"
overload fprint_int64 with fprint0_int64

fun fprint1_int64 {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: int64):<!exnref> void
  = "atspre_fprint_int64"
overload fprint_int64 with fprint1_int64

overload fprint with fprint_int64

fun print_int64 (i: int64):<!ref> void = "atspre_print_int64"
and prerr_int64 (i: int64):<!ref> void = "atspre_prerr_int64"
overload print with print_int64
overload prerr with prerr_int64

(* ****** ****** *)
//
// stringization
//
fun tostrptr_int64
  (i: int64):<> strptr1 = "atspre_tostrptr_int64"
overload tostrptr with tostrptr_int64
fun tostring_int64 (i: int64):<> string = "atspre_tostrptr_int64"
overload tostring with tostring_int64

(* ****** ****** *)

// unsigned integer of size 64bit

typedef uint64 = uint64_t0ype

symintr uint64_of
fun uint64_of_int1 {i:nat}
  (i: int i):<> uint64 = "atspre_uint64_of_int1"
overload uint64_of with uint64_of_int1
fun uint64_of_uint (i: uint):<> uint64 = "atspre_uint64_of_uint"
overload uint64_of with uint64_of_uint
fun uint64_of_ulint (i: ulint):<> uint64 = "atspre_uint64_of_ulint"
overload uint64_of with uint64_of_ulint
fun uint64_of_ullint (i: ullint):<> uint64 = "atspre_uint64_of_ullint"
overload uint64_of with uint64_of_ullint

fun uint_of_uint64 (i: uint64):<> uint = "atspre_uint_of_uint64"

// ------ ------

fun succ_uint64 (i: uint64):<> uint64 = "atspre_succ_uint64"
and pred_uint64 (i: uint64):<> uint64 = "atspre_pred_uint64"
overload succ with succ_uint64
overload pred with pred_uint64

fun add_uint64_uint64 (i: uint64, j: uint64):<> uint64
 = "atspre_add_uint64_uint64"

and sub_uint64_uint64 (i: uint64, j: uint64):<> uint64
  = "atspre_sub_uint64_uint64"

and mul_uint64_uint64 (i: uint64, j: uint64):<> uint64
  = "atspre_mul_uint64_uint64"

and div_uint64_uint64 (i: uint64, j: uint64):<> uint64
  = "atspre_div_uint64_uint64"

and mod_uint64_uint64 (i: uint64, j: uint64):<> uint64
  = "atspre_mod_uint64_uint64"

overload + with add_uint64_uint64
overload - with sub_uint64_uint64
overload * with mul_uint64_uint64
overload / with div_uint64_uint64
overload mod with mod_uint64_uint64 

fun lt_uint64_uint64 (i: uint64, j: uint64):<> bool
  = "atspre_lt_uint64_uint64"

and lte_uint64_uint64 (i: uint64, j: uint64):<> bool
  = "atspre_lte_uint64_uint64"

fun gt_uint64_uint64 (i: uint64, j: uint64):<> bool
  = "atspre_gt_uint64_uint64"

and gte_uint64_uint64 (i: uint64, j: uint64):<> bool
  = "atspre_gte_uint64_uint64"

fun eq_uint64_uint64 (i: uint64, j: uint64):<> bool
  = "atspre_eq_uint64_uint64"

and neq_uint64_uint64 (i: uint64, j: uint64):<> bool
  = "atspre_neq_uint64_uint64"

overload < with lt_uint64_uint64
overload <= with lte_uint64_uint64
overload > with gt_uint64_uint64
overload >= with gte_uint64_uint64
overload = with eq_uint64_uint64
overload <> with neq_uint64_uint64

fun max_uint64_uint64 (i: uint64, j: uint64):<> uint64
  = "atspre_max_uint64_uint64"

and min_uint64_uint64 (i: uint64, j: uint64):<> uint64
  = "atspre_min_uint64_uint64"

overload max with max_uint64_uint64
overload min with min_uint64_uint64

(* ****** ****** *)

symintr fprint_uint64

fun fprint0_uint64 (out: FILEref, x: uint64):<!exnref> void
  = "atspre_fprint_uint64"
overload fprint_uint64 with fprint0_uint64

fun fprint1_uint64 {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: uint64):<!exnref> void
  = "atspre_fprint_uint64"
overload fprint_uint64 with fprint1_uint64

overload fprint with fprint_uint64

fun print_uint64 (i: uint64):<!ref> void = "atspre_print_uint64"
and prerr_uint64 (i: uint64):<!ref> void = "atspre_prerr_uint64"
overload print with print_uint64
overload prerr with prerr_uint64

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [integer_fixed.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [integer_fixed.sats] *)
