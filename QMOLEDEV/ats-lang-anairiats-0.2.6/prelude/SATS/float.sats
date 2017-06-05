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
#print "Loading [float.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)
//
// some common functions on floating point numbers
//
(* ****** ****** *)

typedef lint = int_long_t0ype
typedef ulint = uint_long_t0ype

typedef llint = int_long_long_t0ype
typedef ullint = uint_long_long_t0ype

(* ****** ****** *)

abst@ype float_t0ype = $extype"ats_float_type"
stadef float = float_t0ype

abst@ype double_long_t0ype = $extype"ats_ldouble_type"
stadef ldouble = double_long_t0ype

(* ****** ****** *)
//
// floating point numbers of single precision
//
(* ****** ****** *)

fun int_of_float
  (f: float):<> int = "atspre_int_of_float"
overload int_of with int_of_float

fun lint_of_float
  (f: float):<> lint = "atspre_lint_of_float"
overload lint_of with lint_of_float

fun llint_of_float
  (f: float):<> llint = "atspre_llint_of_float"
overload llint_of with llint_of_float

(* ****** ****** *)

fun float_of_int
  (i: int):<> float = "atspre_float_of_int"
overload float_of with float_of_int

fun float_of_uint
  (u: uint):<> float = "atspre_float_of_uint"
overload float_of with float_of_uint

//

fun float_of_lint
  (i: lint):<> float = "atspre_float_of_lint"
overload float_of with float_of_lint

fun float_of_ulint
  (u: ulint):<> float = "atspre_float_of_ulint"
overload float_of with float_of_ulint

//

fun float_of_llint
  (i: llint):<> float = "atspre_float_of_llint"
overload float_of with float_of_llint

fun float_of_ullint
  (u: ullint):<> float = "atspre_float_of_ullint"
overload float_of with float_of_ullint

//

fun float_of_size
  (sz: size_t):<> float = "atspre_float_of_size"
overload float_of with float_of_size

(* ****** ****** *)

fun float_of_double
  (d: double):<> float = "atspre_float_of_double"
overload float_of with float_of_double

fun float_of_string
  (s: string):<> float = "atspre_float_of_string"
overload float_of with float_of_string

(* ****** ****** *)

fun abs_float (f: float):<> float = "atspre_abs_float"
overload abs with abs_float

fun neg_float (f: float):<> float = "atspre_neg_float"
overload ~ with neg_float

(* ****** ****** *)

fun succ_float (f: float):<> float = "atspre_succ_float"
and pred_float (f: float):<> float = "atspre_pred_float"
overload succ with succ_float
overload pred with pred_float

(* ****** ****** *)

fun add_float_float (f1: float, f2: float):<> float
  = "atspre_add_float_float"
overload + with add_float_float

fun sub_float_float (f1: float, f2: float):<> float
  = "atspre_sub_float_float"
overload - with sub_float_float

(* ****** ****** *)

fun mul_float_float
  (f1: float, f2: float):<> float = "atspre_mul_float_float"
and mul_int_float
  (i1: int, f2: float):<> float = "atspre_mul_int_float"
and mul_float_int
  (f1: float, i2: int):<> float = "atspre_mul_float_int"
overload * with mul_float_float
overload * with mul_int_float
overload * with mul_float_int

fun div_float_float
  (f1: float, f2: float):<> float = "atspre_div_float_float"
and div_float_int
  (f1: float, i2: int):<> float = "atspre_div_float_int"
and div_int_float
  (i1: int, f2: float):<> float = "atspre_div_int_float"
overload / with div_float_float
overload / with div_float_int
overload / with div_int_float

(* ****** ****** *)

fun lt_float_float (f1: float, f2: float):<> bool
  = "atspre_lt_float_float"
and lte_float_float (f1: float, f2: float):<> bool
  = "atspre_lte_float_float"
overload < with lt_float_float
overload <= with lte_float_float

fun gt_float_float (f1: float, f2: float):<> bool
  = "atspre_gt_float_float"
and gte_float_float (f1: float, f2: float):<> bool
  = "atspre_gte_float_float"
overload > with gt_float_float
overload >= with gte_float_float

fun eq_float_float (f1: float, f2: float):<> bool
  = "atspre_eq_float_float"
and neq_float_float (f1: float, f2: float):<> bool
  = "atspre_neq_float_float"
overload = with eq_float_float
overload <> with neq_float_float

fun compare_float_float (f1: float, f2: float):<> Sgn
  = "atspre_compare_float_float"
overload compare with compare_float_float

fun max_float_float (f1: float, f2: float):<> float
  = "atspre_max_float_float"
and min_float_float (f1: float, f2: float):<> float
  = "atspre_min_float_float"
overload max with max_float_float
overload min with min_float_float

(* ****** ****** *)

fun square_float
  (f: float):<> float = "atspre_square_float"
overload square with square_float

fun cube_float
  (f: float):<> float = "atspre_cube_float"
overload cube with cube_float

fun pow_float_int1 (f: float, n: Nat):<> float
overload pow with pow_float_int1

(* ****** ****** *)
//
// print functions for floats of single precision
//
(* ****** ****** *)

symintr fprint_float

fun fprint0_float (out: FILEref, x: float):<!exnref> void
  = "atspre_fprint_float"
overload fprint_float with fprint0_float

fun fprint1_float {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: float):<!exnref> void
  = "atspre_fprint_float"
overload fprint_float with fprint1_float

overload fprint with fprint_float

fun print_float (f: float):<!ref> void = "atspre_print_float"
and prerr_float (f: float):<!ref> void = "atspre_prerr_float"
overload print with print_float
overload prerr with prerr_float

(* ****** ****** *)
//
// stringization
//
fun tostrptr_float
  (f: float):<> strptr1 = "atspre_tostrptr_float"
overload tostrptr with tostrptr_float
fun tostring_float (f: float):<> string = "atspre_tostrptr_float"
overload tostring with tostring_float

(* ****** ****** *)

//
// floating point numbers of double precision
//

(* ****** ****** *)

fun int_of_double
  (d: double):<> int = "atspre_int_of_double"
overload int_of with int_of_double

fun lint_of_double
  (d: double):<> lint = "atspre_lint_of_double"
overload lint_of with lint_of_double

fun llint_of_double
  (d: double):<> llint = "atspre_llint_of_double"
overload llint_of with llint_of_double

(* ****** ****** *)

fun double_of_int
  (i: int):<> double = "atspre_double_of_int"
overload double_of with double_of_int

fun double_of_uint
  (u: uint):<> double = "atspre_double_of_uint"
overload double_of with double_of_uint

fun double_of_lint
  (i: lint):<> double = "atspre_double_of_lint"
overload double_of with double_of_lint

fun double_of_ulint
  (u: ulint):<> double = "atspre_double_of_ulint"
overload double_of with double_of_ulint

fun double_of_llint
  (i: llint):<> double = "atspre_double_of_llint"
overload double_of with double_of_llint

fun double_of_ullint
  (u: ullint):<> double = "atspre_double_of_ullint"
overload double_of with double_of_ullint

fun double_of_size
  (sz: size_t):<> double = "atspre_double_of_size"
overload double_of with double_of_size

(* ****** ****** *)

fun double_of_float (f: float):<> double
  = "atspre_double_of_float"
overload double_of with double_of_float

//
// HX: this function is based on [atof] in [stdlib.h]
//
fun double_of_string
  (s: string):<> double = "atspre_double_of_string"
overload double_of with double_of_string

(* ****** ****** *)

fun abs_double (d: double):<> double = "atspre_abs_double"
overload abs with abs_double

fun neg_double (d: double):<> double = "atspre_neg_double"
overload ~ with neg_double

fun succ_double (d: double):<> double = "atspre_succ_double"
and pred_double (d: double):<> double = "atspre_pred_double"
overload succ with succ_double
overload pred with pred_double

(* ****** ****** *)

fun add_double_double
  (d1: double, d2: double):<> double = "atspre_add_double_double"
and add_double_int (d1: double, i2: int):<> double = "atspre_add_double_int"
and add_int_double (i1: int, d2: double):<> double = "atspre_add_int_double"
overload + with add_double_double
overload + with add_double_int
overload + with add_int_double

fun sub_double_double
  (d1: double, d2: double):<> double = "atspre_sub_double_double"
and sub_double_int (d1: double, i2: int):<> double = "atspre_sub_double_int"
and sub_int_double (i1: int, d2: double):<> double = "atspre_sub_int_double"
overload - with sub_double_double
overload - with sub_double_int
overload - with sub_int_double

(* ****** ****** *)

fun mul_double_double
  (d1: double, d2: double):<> double = "atspre_mul_double_double"
and mul_double_int (d1: double, i2: int):<> double = "atspre_mul_double_int"
and mul_int_double (i1: int, d2: double):<> double = "atspre_mul_int_double"
overload * with mul_double_double
overload * with mul_double_int
overload * with mul_int_double

fun div_double_double
  (d1: double, d2: double):<> double = "atspre_div_double_double"
and div_double_int
  (d1: double, i2: int):<> double = "atspre_div_double_int"
and div_int_double
  (i1: int, d2: double):<> double = "atspre_div_int_double"
overload / with div_double_double
overload / with div_double_int
overload / with div_int_double

(* ****** ****** *)

fun lt_double_double (d1: double, d2: double):<> bool
  = "atspre_lt_double_double"
and lte_double_double (d1: double, d2: double):<> bool
  = "atspre_lte_double_double"
overload < with lt_double_double
overload <= with lte_double_double

fun gt_double_double (d1: double, d2: double):<> bool
  = "atspre_gt_double_double"
and gte_double_double (d1: double, d2: double):<> bool
  = "atspre_gte_double_double"
overload > with gt_double_double
overload >= with gte_double_double

fun eq_double_double (d1: double, d2: double):<> bool
  = "atspre_eq_double_double"
and neq_double_double (d1: double, d2: double):<> bool
  = "atspre_neq_double_double"
overload = with eq_double_double
overload <> with neq_double_double

fun compare_double_double (d1: double, d2: double):<> Sgn
  = "atspre_compare_double_double"
overload compare with compare_double_double

(* ****** ****** *)

fun max_double_double (d1: double, d2: double):<> double
  = "atspre_max_double_double"
and min_double_double (d1: double, d2: double):<> double
  = "atspre_min_double_double"
overload max with max_double_double
overload min with min_double_double

(* ****** ****** *)

fun square_double
  (d: double):<> double = "atspre_square_double"
overload square with square_double

fun cube_double
  (d: double):<> double = "atspre_cube_double"
overload cube with cube_double

fun pow_double_int1 (d: double, n: Nat):<> double
overload pow with pow_double_int1

(* ****** ****** *)
//
// print functions for floats of double precision
//
(* ****** ****** *)

symintr fprint_double

fun fprint0_double (out: FILEref, x: double):<!exnref> void
  = "atspre_fprint_double"
overload fprint_double with fprint0_double

fun fprint1_double {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: double):<!exnref> void
  = "atspre_fprint_double"
overload fprint_double with fprint1_double

overload fprint with fprint_double

fun print_double (f: double):<!ref> void = "atspre_print_double"
and prerr_double (f: double):<!ref> void = "atspre_prerr_double"
overload print with print_double
overload prerr with prerr_double

(* ****** ****** *)
//
// stringization
//
fun tostrptr_double
  (d: double):<> strptr1 = "atspre_tostrptr_double"
overload tostrptr with tostrptr_double
fun tostring_double (d: double):<> string = "atspre_tostrptr_double"
overload tostring with tostring_double

(* ****** ****** *)
//
// floating point numbers of long double precision
//
(* ****** ****** *)

fun int_of_ldouble
  (ld: ldouble):<> int = "atspre_int_of_ldouble"
overload int_of with int_of_ldouble

fun lint_of_ldouble
  (ld: ldouble):<> lint = "atspre_lint_of_ldouble"
overload lint_of with lint_of_ldouble

fun llint_of_ldouble
  (ld: ldouble):<> llint = "atspre_llint_of_ldouble"
overload llint_of with llint_of_ldouble

(* ****** ****** *)

fun ldouble_of_int
  (i: int):<> ldouble = "atspre_ldouble_of_int"
overload ldouble_of with ldouble_of_int

fun ldouble_of_lint
  (li: lint):<> ldouble = "atspre_ldouble_of_lint"
overload ldouble_of with ldouble_of_lint

fun ldouble_of_llint
  (lli: llint):<> ldouble = "atspre_ldouble_of_llint"
overload ldouble_of with ldouble_of_llint

(* ****** ****** *)

fun ldouble_of_float
  (f: float):<> ldouble = "atspre_ldouble_of_float"
overload ldouble_of with ldouble_of_float

fun ldouble_of_double
  (d: double):<> ldouble = "atspre_ldouble_of_double"
overload ldouble_of with ldouble_of_double

(* ****** ****** *)

fun abs_ldouble
  (ld: ldouble):<> ldouble = "atspre_abs_ldouble"
overload abs with abs_ldouble

fun neg_ldouble
  (ld: ldouble):<> ldouble = "atspre_neg_ldouble"
overload ~ with neg_ldouble

(* ****** ****** *)

fun succ_ldouble
  (ld: ldouble):<> ldouble = "atspre_succ_ldouble"
and pred_ldouble
  (ld: ldouble):<> ldouble = "atspre_pred_ldouble"
overload succ with succ_ldouble
overload pred with pred_ldouble

(* ****** ****** *)

fun add_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> ldouble
  = "atspre_add_ldouble_ldouble"
and sub_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> ldouble
  = "atspre_sub_ldouble_ldouble"
overload + with add_ldouble_ldouble
overload - with sub_ldouble_ldouble

(* ****** ****** *)

fun mul_ldouble_ldouble
  (ld1: ldouble, ld2: ldouble):<> ldouble
  = "atspre_mul_ldouble_ldouble"
and mul_int_ldouble (i1: int, ld2: ldouble):<> ldouble
  = "atspre_mul_int_ldouble"
and mul_ldouble_int (ld1: ldouble, i2: int):<> ldouble
  = "atspre_mul_ldouble_int"
overload * with mul_ldouble_ldouble
overload * with mul_int_ldouble
overload * with mul_ldouble_int

fun div_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> ldouble
  = "atspre_div_ldouble_ldouble"
and div_ldouble_int (ld1: ldouble, i2: int):<> ldouble
  = "atspre_div_ldouble_int"
overload / with div_ldouble_ldouble
overload / with div_ldouble_int

(* ****** ****** *)

fun lt_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> bool
  = "atspre_lt_ldouble_ldouble"
and lte_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> bool
  = "atspre_lte_ldouble_ldouble"
overload < with lt_ldouble_ldouble
overload <= with lte_ldouble_ldouble

fun gt_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> bool
  = "atspre_gt_ldouble_ldouble"
and gte_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> bool
  = "atspre_gte_ldouble_ldouble"
overload > with gt_ldouble_ldouble
overload >= with gte_ldouble_ldouble

fun eq_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> bool
  = "atspre_eq_ldouble_ldouble"
and neq_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> bool
  = "atspre_neq_ldouble_ldouble"
overload = with eq_ldouble_ldouble
overload <> with neq_ldouble_ldouble

fun compare_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> Sgn
  = "atspre_compare_ldouble_ldouble"
overload compare with compare_ldouble_ldouble

(* ****** ****** *)

fun max_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> ldouble
  = "atspre_max_ldouble_ldouble"
and min_ldouble_ldouble (ld1: ldouble, ld2: ldouble):<> ldouble
  = "atspre_min_ldouble_ldouble"
overload max with max_ldouble_ldouble
overload min with min_ldouble_ldouble

(* ****** ****** *)

fun square_ldouble
  (ld: ldouble):<> ldouble = "atspre_square_ldouble"
overload square with square_ldouble

fun cube_ldouble
  (ld: ldouble):<> ldouble = "atspre_cube_ldouble"
overload cube with cube_ldouble

fun pow_ldouble_int1 (ld: ldouble, n: Nat):<> ldouble
overload pow with pow_ldouble_int1

(* ****** ****** *)
//
// print functions for floats of long double precision
//
(* ****** ****** *)

symintr fprint_ldouble

fun fprint0_ldouble (out: FILEref, x: ldouble):<!exnref> void
  = "atspre_fprint_ldouble"
overload fprint_ldouble with fprint0_ldouble

fun fprint1_ldouble {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: ldouble):<!exnref> void
  = "atspre_fprint_ldouble"
overload fprint_ldouble with fprint1_ldouble

overload fprint with fprint_ldouble

fun print_ldouble (ld: ldouble):<!ref> void = "atspre_print_ldouble"
and prerr_ldouble (ld: ldouble):<!ref> void = "atspre_prerr_ldouble"
overload print with print_ldouble
overload prerr with prerr_ldouble

(* ****** ****** *)
//
// stringization
//
fun tostrptr_ldouble
  (ld: ldouble):<> strptr1 = "atspre_tostrptr_ldouble"
overload tostrptr with tostrptr_ldouble
fun tostring_ldouble (ld: ldouble):<> string = "atspre_tostrptr_ldouble"
overload tostring with tostring_ldouble

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [float.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(* end of [float.sats] *)
