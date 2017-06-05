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

(* Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *) // MPZ and MPQ
(* Author: Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu) *) // MPF

(* ****** ****** *)

%{#
#include "libc/CATS/gmp.cats"
%} // end of [%{#]

(* ****** ****** *)

// integral numbers
absviewt@ype mpz_viewt0ype = $extype"ats_mpz_viewt0ype"
stadef mpz_vt = mpz_viewt0ype

// rational numbers
absviewt@ype mpq_viewt0ype = $extype"ats_mpq_viewt0ype"
stadef mpq_vt = mpq_viewt0ype

// floating point numbers
absviewt@ype mpf_viewt0ype = $extype"ats_mpf_viewt0ype"
stadef mpf_vt = mpf_viewt0ype

(* ****** ****** *)

abst@ype mp_limb_t = $extype"mp_limb_t"
abst@ype mp_limb_signed_t = $extype"mp_limb_signed_t"

(* ****** ****** *)

abst@ype mp_exp_t = $extype"mp_exp_t" // int or lint

symintr mp_exp_t
castfn mp_exp_t_of_int (x: int):<> mp_exp_t
overload mp_exp_t with mp_exp_t_of_int
castfn mp_exp_t_of_lint (x: lint):<> mp_exp_t
overload mp_exp_t with mp_exp_t_of_lint

abst@ype mp_size_t = $extype"mp_size_t" // int or lint

symintr mp_size_t
castfn mp_size_t_of_int (x: int):<> mp_size_t
overload mp_size_t with mp_size_t_of_int
castfn mp_size_t_of_lint (x: lint):<> mp_size_t
overload mp_size_t with mp_size_t_of_lint

(* ****** ****** *)

typedef mp_base_t = intBtw (2, 36+1) // for outputing MP numbers

(* ****** ****** *)
//
// integral number operations
//
(* ****** ****** *)

// [x] is initialized with 0
fun mpz_init
  (x: &mpz_vt? >> mpz_vt):<> void = "mac#atslib_mpz_init" // macro!
// end of [mpz_init]

// [x] is initialized with 0 while given [n]-bit space
fun mpz_init2
  (x: &mpz_vt? >> mpz_vt, n: ulint) :<> void = "mac#atslib_mpz_init2"
// end of [mpz_init2]

// [x] is cleared
fun mpz_clear
  (x: &mpz_vt >> mpz_vt?):<> void = "mac#atslib_mpz_clear" // macro!
// end of [mpz_clear]

// [x] is reallocated; the original value of [x] is carried over
// if there is enough space or 0 is assigned to [x]
fun mpz_realloc2
  (x: &mpz_vt >> mpz_vt, n: ulint):<> void = "mac#atslib_mpz_realloc2"
// end of [mpz_realloc2]

(* ****** ****** *)

symintr mpz_get

fun mpz_get_int (x: &mpz_vt):<> int = "mac#atslib_mpz_get_int"
overload mpz_get with mpz_get_int
fun mpz_get_uint (x: &mpz_vt):<> uint = "mac#atslib_mpz_get_uint"
overload mpz_get with mpz_get_uint
fun mpz_get_lint (x: &mpz_vt):<> lint = "mac#atslib_mpz_get_lint"
overload mpz_get with mpz_get_lint
fun mpz_get_ulint (x: &mpz_vt):<> ulint = "mac#atslib_mpz_get_ulint"
overload mpz_get with mpz_get_ulint
fun mpz_get_double (x: &mpz_vt):<> double = "mac#atslib_mpz_get_double"
overload mpz_get with mpz_get_double
fun mpz_get_str // HX: a special case of the original namesake in GMP
  (base: mp_base_t, x: &mpz_vt):<> strptr1 = "atslib_mpz_get_str" // fun!
// end of [mpz_get_str]

(* ****** ****** *)

symintr mpz_set

// x := y
fun mpz_set_mpz
  (x: &mpz_vt, y: &mpz_vt):<> void = "mac#atslib_mpz_set_mpz"
overload mpz_set with mpz_set_mpz
fun mpz_set_int (x: &mpz_vt, y: int):<> void = "mac#atslib_mpz_set_int"
overload mpz_set with mpz_set_int
fun mpz_set_uint (x: &mpz_vt, y: uint):<> void = "mac#atslib_mpz_set_uint"
overload mpz_set with mpz_set_uint
fun mpz_set_lint (x: &mpz_vt, y: lint):<> void = "mac#atslib_mpz_set_lint"
overload mpz_set with mpz_set_lint
fun mpz_set_ulint (x: &mpz_vt, y: ulint):<> void = "mac#atslib_mpz_set_ulint"
overload mpz_set with mpz_set_ulint
fun mpz_set_double
  (x: &mpz_vt, y: double):<> void = "mac#atslib_mpz_set_double"
overload mpz_set with mpz_set_double
fun mpz_set_mpq (x: &mpz_vt, y: &mpq_vt):<> void = "mac#atslib_mpz_set_mpq"
overload mpz_set with mpz_set_mpq
fun mpz_set_mpf (x: &mpz_vt, y: &mpf_vt):<> void = "mac#atslib_mpz_set_mpf"
overload mpz_set with mpz_set_mpf

//
// HX: the function returns 0 if the string is valid, or -1 otherwise.
//
fun mpz_set_str
  (x: &mpz_vt, s: string, base: mp_base_t):<> int = "mac#atslib_mpz_set_str" // macro
// end of [mpz_set_str]
fun mpz_set_str_exn
  (x: &mpz_vt, s: string, base: mp_base_t):<> void = "atslib_mpz_set_str_exn" // fun!
// end of [mpz_set_str_exn]

(* ****** ****** *)

symintr mpz_init_set

// x := y
fun mpz_init_set_mpz (x: &mpz_vt? >> mpz_vt, y: &mpz_vt):<> void
  = "mac#atslib_mpz_init_set_mpz"
overload mpz_init_set with mpz_init_set_mpz
fun mpz_init_set_int (x: &mpz_vt? >> mpz_vt, y: int):<> void
  = "mac#atslib_mpz_init_set_int"
overload mpz_init_set with mpz_init_set_int
fun mpz_init_set_uint (x: &mpz_vt? >> mpz_vt, y: uint):<> void
  = "mac#atslib_mpz_init_set_uint"
overload mpz_init_set with mpz_init_set_uint
fun mpz_init_set_lint (x: &mpz_vt? >> mpz_vt, y: lint):<> void
  = "mac#atslib_mpz_init_set_lint"
overload mpz_init_set with mpz_init_set_lint
fun mpz_init_set_ulint (x: &mpz_vt? >> mpz_vt, y: ulint):<> void
  = "mac#atslib_mpz_init_set_ulint"
overload mpz_init_set with mpz_init_set_ulint
fun mpz_init_set_double (x: &mpz_vt? >> mpz_vt, y: double):<> void
  = "mac#atslib_mpz_init_set_double"
overload mpz_init_set with mpz_init_set_double
fun mpz_init_set_mpq
  (x: &mpz_vt? >> mpz_vt, y: &mpq_vt):<> void = "atslib_mpz_init_set_mpq" // fun!
overload mpz_init_set with mpz_init_set_mpq
fun mpz_init_set_mpf
  (x: &mpz_vt? >> mpz_vt, y: &mpf_vt):<> void = "atslib_mpz_init_set_mpf" // fun!
overload mpz_init_set with mpz_init_set_mpf

//
// the function returns 0 if the string is valid, or -1 otherwise.
//
fun mpz_init_set_str
  (x: &mpz_vt? >> mpz_vt, s: string, base: mp_base_t):<> int
  = "mac#atslib_mpz_init_set_str" // macro!
// end of [// end of [mpz_init_set_str]

// the function exits the string is invalid.
fun mpz_init_set_str_exn
  (x: &mpz_vt? >> mpz_vt, s: string, base: mp_base_t):<> void
  = "atslib_mpz_init_set_str_exn" // function!
// end of [mpz_init_set_str_exn]

(* ****** ****** *)

fun mpz_swap (dst1: &mpz_vt, dst2: &mpz_vt): void = "mac#atslib_mpz_swap"

(* ****** ****** *)

fun mpz_odd_p (src: &mpz_vt):<> bool = "mac#atslib_mpz_odd_p"
fun mpz_even_p (src: &mpz_vt):<> bool = "mac#atslib_mpz_even_p"

fun mpz_fits_int_p (src: &mpz_vt):<> bool = "mac#atslib_mpz_fits_int_p"
fun mpz_fits_uint_p (src: &mpz_vt):<> bool = "mac#atslib_mpz_fits_uint_p"
fun mpz_fits_lint_p (src: &mpz_vt):<> bool = "mac#atslib_mpz_fits_lint_p"
fun mpz_fits_ulint_p (src: &mpz_vt):<> bool = "mac#atslib_mpz_fits_ulint_p"
fun mpz_fits_sint_p (src: &mpz_vt):<> bool = "mac#atslib_mpz_fits_sint_p"
fun mpz_fits_usint_p (src: &mpz_vt):<> bool = "mac#atslib_mpz_fits_usint_p"

fun mpz_size (src: &mpz_vt): size_t = "mac#atslib_mpz_size"
fun mpz_sizeinbase (src: &mpz_vt, base: mp_base_t): size_t = "mac#atslib_mpz_sizeinbase"

(* ****** ****** *)

// negation

symintr mpz_neg

// x := -y
fun mpz_neg2 (x: &mpz_vt, y: &mpz_vt):<> void = "mac#atslib_mpz_neg2"
overload mpz_neg with mpz_neg2
// x := -x
fun mpz_neg1 (x: &mpz_vt):<> void = "atslib_mpz_neg1" // function!
overload mpz_neg with mpz_neg1

// absolute value

symintr mpz_abs

// x := | y |
fun mpz_abs2 (x: &mpz_vt, y: &mpz_vt):<> void = "mac#atslib_mpz_abs2"
overload mpz_abs with mpz_abs2
// x := | x |
fun mpz_abs1 (x: &mpz_vt):<> void = "atslib_mpz_abs1" // function!
overload mpz_abs with mpz_abs1

// addition

symintr mpz_add

// x := y + z
fun mpz_add3_mpz
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "mac#atslib_mpz_add3_mpz"
overload mpz_add with mpz_add3_mpz
fun mpz_add3_int
  (x: &mpz_vt, y: &mpz_vt, z: int):<> void = "mac#atslib_mpz_add3_int"
overload mpz_add with mpz_add3_int
fun mpz_add3_uint
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "mac#atslib_mpz_add3_uint"
overload mpz_add with mpz_add3_uint
fun mpz_add3_lint
  (x: &mpz_vt, y: &mpz_vt, z: lint):<> void = "atslib_mpz_add3_lint" // fun!
overload mpz_add with mpz_add3_lint
fun mpz_add3_ulint
  (x: &mpz_vt, y: &mpz_vt, z: ulint):<> void = "mac#atslib_mpz_add3_ulint"
overload mpz_add with mpz_add3_ulint

// x := x + y
fun mpz_add2_mpz
  (x: &mpz_vt, y: &mpz_vt):<> void = "atslib_mpz_add2_mpz"
overload mpz_add with mpz_add2_mpz
fun mpz_add2_int
  (x: &mpz_vt, y: int):<> void = "mac#atslib_mpz_add2_int" // !macro
overload mpz_add with mpz_add2_int
fun mpz_add2_uint
  (x: &mpz_vt, y: uint):<> void = "mac#atslib_mpz_add2_uint" // !macro
overload mpz_add with mpz_add2_uint
fun mpz_add2_lint
  (x: &mpz_vt, y: lint):<> void = "atslib_mpz_add2_lint"
overload mpz_add with mpz_add2_lint
fun mpz_add2_ulint
  (x: &mpz_vt, y: ulint):<> void = "atslib_mpz_add2_ulint"
overload mpz_add with mpz_add2_ulint

// subtraction

symintr mpz_sub

// x := y - z
fun mpz_sub3_mpz
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "mac#atslib_mpz_sub3_mpz"
overload mpz_sub with mpz_sub3_mpz  
fun mpz_sub3_int
  (x: &mpz_vt, y: &mpz_vt, z: int):<> void = "mac#atslib_mpz_sub3_int"
overload mpz_sub with mpz_sub3_int
fun mpz_sub3_uint
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "mac#atslib_mpz_sub3_uint"
overload mpz_sub with mpz_sub3_uint
fun mpz_sub3_lint
  (x: &mpz_vt, y: &mpz_vt, z: lint):<> void = "atslib_mpz_sub3_lint" // fun!
overload mpz_sub with mpz_sub3_lint  
fun mpz_sub3_ulint
  (x: &mpz_vt, y: &mpz_vt, z: ulint):<> void = "mac#atslib_mpz_sub3_ulint"
overload mpz_sub with mpz_sub3_ulint  
fun mpz_ui_sub3
  (dst: &mpz_vt, src1: ulint, src2: &mpz_vt): void = "mac#atslib_mpz_ui_sub3"
// end of [mpz_ui_sub3] // HX: no overloading for this one

// x := x - y
fun mpz_sub2_mpz
  (x: &mpz_vt, y: &mpz_vt):<> void = "atslib_mpz_sub2_mpz"
overload mpz_sub with mpz_sub2_mpz
fun mpz_sub2_int
  (x: &mpz_vt, y: int):<> void = "mac#atslib_mpz_sub2_int" // !macro
overload mpz_sub with mpz_sub2_int
fun mpz_sub2_uint
  (x: &mpz_vt, y: uint):<> void = "mac#atslib_mpz_sub2_uint" // !macro
overload mpz_sub with mpz_sub2_uint
fun mpz_sub2_lint
  (x: &mpz_vt, y: lint):<> void = "atslib_mpz_sub2_lint"
overload mpz_sub with mpz_sub2_lint
fun mpz_sub2_ulint
  (x: &mpz_vt, y: ulint):<> void = "atslib_mpz_sub2_ulint"
overload mpz_sub with mpz_sub2_ulint

(* ****** ****** *)

// multiplication

symintr mpz_mul

// x := y * z
fun mpz_mul3_mpz
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "mac#atslib_mpz_mul3_mpz"
overload mpz_mul with mpz_mul3_mpz
fun mpz_mul3_int
  (x: &mpz_vt, y: &mpz_vt, z: int):<> void = "mac#atslib_mpz_mul3_int"
overload mpz_mul with mpz_mul3_int
fun mpz_mul3_uint
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "mac#atslib_mpz_mul3_uint"
overload mpz_mul with mpz_mul3_uint
fun mpz_mul3_lint
  (x: &mpz_vt, y: &mpz_vt, z: lint):<> void = "mac#atslib_mpz_mul3_lint"
overload mpz_mul with mpz_mul3_lint
fun mpz_mul3_ulint
  (x: &mpz_vt, y: &mpz_vt, z: ulint):<> void = "mac#atslib_mpz_mul3_ulint"
overload mpz_mul with mpz_mul3_ulint

// x := x * y
fun mpz_mul2_mpz
  (x: &mpz_vt, y: &mpz_vt):<> void = "atslib_mpz_mul2_mpz"
overload mpz_mul with mpz_mul2_mpz
fun mpz_mul2_int (x: &mpz_vt, y: int):<> void = "mac#atslib_mpz_mul2_int" // !mac
overload mpz_mul with mpz_mul2_int
fun mpz_mul2_uint (x: &mpz_vt, y: uint):<> void = "mac#atslib_mpz_mul2_uint" // !mac
overload mpz_mul with mpz_mul2_uint
fun mpz_mul2_lint (x: &mpz_vt, y: lint):<> void = "atslib_mpz_mul2_lint"
overload mpz_mul with mpz_mul2_lint
fun mpz_mul2_ulint (x: &mpz_vt, y: ulint):<> void = "atslib_mpz_mul2_ulint"
overload mpz_mul with mpz_mul2_ulint

// x := x * x
fun mpz_mul1_mpz (x: &mpz_vt):<> void = "atslib_mpz_mul1_mpz"
overload mpz_mul with mpz_mul1_mpz

(* ****** ****** *)

(*
**
** Author: Zhiqiang Ren (aren AT cs DOT bu DOT edu)
**
** Function: mpz_mul_2exp
** Input: arg1, arg2
** Output: res
** Return: void
** Description: Set res so that res = arg1 * (2 ^ arg2)
** Remarks: The same object can be passed for both res and arg1.
** Others:
**   It's up to an application to call functions like mpz_mul_2exp when appropriate.
**   General purpose functions like mpz_mul make no attempt to identify powers of two
**   or other special forms.
*)
fun mpz_mul_2exp
  (res: &mpz_vt, arg1: &mpz_vt, arg2: ulint):<> void = "mac#atslib_mpz_mul_2exp"
// end of [mpz_mul_2exp]

(* ****** ****** *)
//
// integer truncate division
//
symintr mpz_tdiv_qr

// (q, r) = n / d
fun mpz_tdiv4_qr_mpz
  (q: &mpz_vt, r: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "mac#atslib_mpz_tdiv4_qr_mpz"
// end of [mpz_tdiv4_qr_mpz]
overload mpz_tdiv_qr with mpz_tdiv4_qr_mpz

// (q, r) = n / d
fun mpz_tdiv4_qr_ulint
  (q: &mpz_vt, r: &mpz_vt, n: &mpz_vt, d: ulint):<> void = "mac#atslib_mpz_tdiv4_qr_ulint"
// end of [mpz_tdiv4_qr_ulint]
overload mpz_tdiv_qr with mpz_tdiv4_qr_ulint

symintr mpz_tdiv_q

// [q] := [n] / [d]
fun mpz_tdiv3_q_mpz
  (q: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "mac#atslib_mpz_tdiv3_q_mpz"
overload mpz_tdiv_q with mpz_tdiv3_q_mpz

// [q] := [n] / [d]
fun mpz_tdiv3_q_ulint
  (q: &mpz_vt, n: &mpz_vt, d: ulint):<> void = "mac#atslib_mpz_tdiv3_q_ulint"
overload mpz_tdiv_q with mpz_tdiv3_q_ulint

// [q] := [q] / [d]
fun mpz_tdiv2_q_mpz (q: &mpz_vt, d: &mpz_vt):<> void = "atslib_mpz_tdiv2_q_mpz"
overload mpz_tdiv_q with mpz_tdiv2_q_mpz

// [q] := [q] / [d]
fun mpz_tdiv2_q_ulint (q: &mpz_vt, d: ulint):<> void = "atslib_mpz_tdiv2_q_ulint"
overload mpz_tdiv_q with mpz_tdiv2_q_ulint

(* ****** ****** *)
//
// integer floor division
//
(*
**
** Author: Zhiqiang Ren (aren AT cs DOT bu DOT edu)
**
** Function: mpz_fdiv_qr
** Input: dividend, divisor
** Output: quot, rem
** Return: void
** Description:
**   Set quot and rem so that dividend = quot * divisor + rem
**   Rounds quot down towards negative infinity, and rem will
**   have the same sign as divisor, and 0 <= |rem| < |divisor|.
**   'f' stands for "floor". e.g. 5 = (-2) * (-3) + (-1); -5 = 1 * (-3) + (-2)
** Remarks:
**   The same object cannot be passed for both quot and rem, or the result will be
**   unpredictable. No other constraints on the pass of other arguments, e.g. the same
**   object can be passed to both quot and dividend.
*)

symintr mpz_fdiv_qr

fun mpz_fdiv4_qr_mpz
  (quot: &mpz_vt, rem: &mpz_vt, dividend: &mpz_vt, divisor: &mpz_vt):<> void
  = "mac#atslib_mpz_fdiv4_qr_mpz"
overload mpz_fdiv_qr with mpz_fdiv4_qr_mpz
fun mpz_fdiv4_qr_ulint
  (quot: &mpz_vt, rem: &mpz_vt, dividend: &mpz_vt, divisor: ulint):<> ulint
  = "mac#atslib_mpz_fdiv4_qr_ulint"
overload mpz_fdiv_qr with mpz_fdiv4_qr_ulint

symintr mpz_fdiv_q

// [q] := [n] / [d]
fun mpz_fdiv3_q_mpz
  (q: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "mac#atslib_mpz_fdiv3_q_mpz"
overload mpz_fdiv_q with mpz_fdiv3_q_mpz
fun mpz_fdiv3_q_ulint
  (q: &mpz_vt, n: &mpz_vt, d: ulint):<> ulint = "mac#atslib_mpz_fdiv3_q_ulint"
overload mpz_fdiv_q with mpz_fdiv3_q_ulint
fun mpz_fdiv2_q_mpz
  (q: &mpz_vt, d: &mpz_vt):<> void = "atslib_mpz_fdiv2_q_mpz"
overload mpz_fdiv_q with mpz_fdiv2_q_mpz
fun mpz_fdiv2_q_ulint
  (q: &mpz_vt, d: ulint):<> ulint = "atslib_mpz_fdiv2_q_ulint"
overload mpz_fdiv_q with mpz_fdiv2_q_ulint

symintr mpz_fdiv_r
fun mpz_fdiv3_r_mpz
  (r: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "mac#atslib_mpz_fdiv3_r_mpz"
overload mpz_fdiv_r with mpz_fdiv3_r_mpz
fun mpz_fdiv3_r_ulint
  (r: &mpz_vt, n: &mpz_vt, d: ulint):<> ulint = "mac#atslib_mpz_fdiv3_r_ulint"
overload mpz_fdiv_r with mpz_fdiv3_r_ulint
fun mpz_fdiv2_r_mpz
  (r: &mpz_vt, d: &mpz_vt):<> void = "atslib_mpz_fdiv2_r_mpz"
overload mpz_fdiv_r with mpz_fdiv2_r_mpz
fun mpz_fdiv2_r_ulint
  (r: &mpz_vt, d: ulint):<> ulint = "atslib_mpz_fdiv2_r_ulint"
overload mpz_fdiv_r with mpz_fdiv2_r_ulint

(* ****** ****** *)

symintr mpz_mod

fun mpz_mod3_mpz
  (r: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "mac#atslib_mpz_mod3_mpz"
overload mpz_mod with mpz_mod3_mpz
fun mpz_mod2_mpz (n: &mpz_vt, d: &mpz_vt):<> void = "mac#atslib_mpz_mod2_mpz"
overload mpz_mod with mpz_mod2_mpz

fun mpz_mod3_ulint
  (r: &mpz_vt, n: &mpz_vt, d: ulint):<> ulint = "mac#atslib_mpz_mod3_ulint"
overload mpz_mod with mpz_mod3_ulint
fun mpz_mod2_ulint (n: &mpz_vt, d: ulint):<> ulint = "mac#atslib_mpz_mod2_ulint"
overload mpz_mod with mpz_mod2_ulint

(* ****** ****** *)
//
// HX-2010-08-11:
// this one is much faster but it needs a proof that n is a multiple of d
//
symintr mpz_divexact
fun mpz_divexact3_mpz // q := n/d
  (q: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "mac#atslib_mpz_divexact3_mpz"
overload mpz_divexact with mpz_divexact3_mpz
fun mpz_divexact2_mpz // n := n/d
  (n: &mpz_vt, d: &mpz_vt):<> void = "mac#atslib_mpz_divexact2_mpz"
overload mpz_divexact with mpz_divexact2_mpz

(* ****** ****** *)
//
// HX: n = 0 mod (d)
//
fun mpz_divisible_p
  (n: &mpz_vt, d: &mpz_vt):<> int = "mac#atslib_mpz_divisible_p"
fun mpz_divisible_ui_p
  (n: &mpz_vt, d: ulint):<> int = "mac#atslib_mpz_divisible_ui_p"

//
// HX: n = c mod (d)
//
fun mpz_congruent_p
  (n: &mpz_vt, c: &mpz_vt, d: &mpz_vt):<> int = "mac#atslib_mpz_congruent_p"
fun mpz_congruent_ui_p
  (n: &mpz_vt, c: ulint, d: ulint):<> int = "mac#atslib_mpz_congruent_ui_p"

(* ****** ****** *)

symintr mpz_tdiv_q_2exp
fun mpz_tdiv3_q_2exp // dst = src1 tdiv src2
  (dst: mpz_vt, src1: mpz_vt, src2: ulint): void = "mac#atslib_tdiv3_q_2exp"
overload mpz_tdiv_q_2exp with mpz_tdiv3_q_2exp

symintr mpz_tdiv_r_2exp
fun mpz_tdiv3_r_2exp // dst = src1 tmod src2
  (dst: mpz_vt, src1: mpz_vt, src2: ulint): void = "mac#atslib_tdiv3_r_2exp"
overload mpz_tdiv_r_2exp with mpz_tdiv3_r_2exp

symintr mpz_fdiv_q_2exp
fun mpz_fdiv3_q_2exp // dst := src1 fdiv src2
  (dst: mpz_vt, src1: mpz_vt, src2: ulint): void = "mac#atslib_fdiv3_q_2exp"
overload mpz_fdiv_q_2exp with mpz_fdiv3_q_2exp

symintr mpz_fdiv_r_2exp
fun mpz_fdiv3_r_2exp // dst := src1 fmod src2
  (dst: mpz_vt, src1: mpz_vt, src2: ulint): void = "mac#atslib_fdiv3_r_2exp"
overload mpz_fdiv_r_2exp with mpz_fdiv3_r_2exp

(* ****** ****** *)

fun mpz_divisible_ui_2exp_p // n = 0 mod (2^b)
  (n: &mpz_vt, b: ulint):<> int = "mac#atslib_mpz_divisible_ui_2exp_p"
// end of [mpz_divisible_ui_2exp_p]

fun mpz_congruent_ui_2exp_p // n = c mod (2^b)
  (n: &mpz_vt, c: &mpz_vt, b: ulint):<> int = "mac#atslib_mpz_congruent_ui_2exp_p"
// end of [mpz_congruent_ui_2exp_p]

(* ****** ****** *)

symintr mpz_sqrt
fun mpz_sqrt2 // dst := sqrt (src)
  (dst: &mpz_vt, src: &mpz_vt): void = "mac#atslib_mpz_sqrt2"
overload mpz_sqrt with mpz_sqrt2
fun mpz_sqrt1 (dst: &mpz_vt): void = "atslib_mpz_sqrt1" // !function
overload mpz_sqrt with mpz_sqrt1

symintr mpz_sqrtrem
fun mpz_sqrtrem3
  (dst1: &mpz_vt, dst2: &mpz_vt, src: &mpz_vt): void = "mac#atslib_mpz_sqrtrem3"
overload mpz_sqrtrem with mpz_sqrtrem3

fun mpz_perfect_square_p (src: &mpz_vt): int = "mac#atslib_mpz_perfect_square_p"

(* ****** ****** *)

symintr mpz_powm
fun mpz_powm4_mpz
  (dst: &mpz_vt, base: &mpz_vt, exp: &mpz_vt, mod: &mpz_vt): void
  = "mac#atslib_mpz_powm4_mpz"
overload mpz_powm with mpz_powm4_mpz
fun mpz_powm4_ui
  (dst: &mpz_vt, base: &mpz_vt, exp: ulint, mod: &mpz_vt): void = "mac#atslib_mpz_powm4_ui"
overload mpz_powm with mpz_powm4_ui

(* ****** ****** *)

symintr mpz_pow_ui
fun mpz_pow3_ui
  (dst: &mpz_vt, src1: &mpz_vt, src2: ulint): void = "mac#atslib_mpz_pow3_ui"
overload mpz_pow_ui with mpz_pow3_ui
fun mpz_pow2_ui (dst: &mpz_vt, src2: ulint): void = "atslib_mpz_pow2_ui" // !fun
overload mpz_pow_ui with mpz_pow2_ui

(* ****** ****** *)

// add/mul combination

symintr mpz_addmul
fun mpz_addmul3_mpz
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "mac#atslib_mpz_addmul3_mpz"
overload mpz_addmul with mpz_addmul3_mpz
fun mpz_addmul3_uint
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "mac#atslib_mpz_addmul3_uint"
overload mpz_addmul with mpz_addmul3_uint
fun mpz_addmul3_ulint
  (x: &mpz_vt, y: &mpz_vt, z: ulint):<> void = "mac#atslib_mpz_addmul3_ulint"
overload mpz_addmul with mpz_addmul3_ulint

// sub/mul combination

symintr mpz_submul
fun mpz_submul3_mpz
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "mac#atslib_mpz_submul3_mpz"
overload mpz_submul with mpz_submul3_mpz
fun mpz_submul3_uint
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "mac#atslib_mpz_submul3_uint"
overload mpz_submul with mpz_submul3_uint
fun mpz_submul3_ulint
  (x: &mpz_vt, y: &mpz_vt, z: ulint):<> void = "mac#atslib_mpz_submul3_ulint"
overload mpz_submul with mpz_submul3_ulint

(* ****** ****** *)

// comparison functions

symintr mpz_cmp
fun mpz_cmp_mpz (x: &mpz_vt, y: &mpz_vt):<> int = "mac#atslib_mpz_cmp_mpz"
overload mpz_cmp with mpz_cmp_mpz
fun mpz_cmp_int (x: &mpz_vt, y: int):<> int = "mac#atslib_mpz_cmp_int"
overload mpz_cmp with mpz_cmp_int
fun mpz_cmp_uint (x: &mpz_vt, y: uint):<> int = "mac#atslib_mpz_cmp_uint"
overload mpz_cmp with mpz_cmp_uint
fun mpz_cmp_lint (x: &mpz_vt, y: lint):<> int = "mac#atslib_mpz_cmp_lint"
overload mpz_cmp with mpz_cmp_lint
fun mpz_cmp_ulint (x: &mpz_vt, y: ulint):<> int = "mac#atslib_mpz_cmp_ulint"
overload mpz_cmp with mpz_cmp_ulint
fun mpz_cmp_double (x: &mpz_vt, y: double):<> int = "mac#atslib_mpz_cmp_double"
overload mpz_cmp with mpz_cmp_double

symintr mpz_cmpabs
fun mpz_cmpabs_mpz (x: &mpz_vt, y: &mpz_vt):<> int = "mac#atslib_mpz_cmpabs_mpz"
overload mpz_cmpabs with mpz_cmpabs_mpz
fun mpz_cmpabs_uint (x: &mpz_vt, y: uint):<> int = "mac#atslib_mpz_cmpabs_uint"
overload mpz_cmpabs with mpz_cmpabs_uint
fun mpz_cmpabs_ulint (x: &mpz_vt, y: ulint):<> int = "mac#atslib_mpz_cmpabs_ulint"
overload mpz_cmpabs with mpz_cmpabs_ulint
fun mpz_cmpabs_double (x: &mpz_vt, y: double):<> int = "mac#atslib_mpz_cmpabs_double"
overload mpz_cmpabs with mpz_cmpabs_double

(* ****** ****** *)

fun mpz_sgn (x: &mpz_vt):<> Sgn = "mac#atslib_mpz_sgn"

(* ****** ****** *)

symintr mpz_gcd
fun mpz_gcd3_mpz
  (dst: &mpz_vt, src1: &mpz_vt, src2: &mpz_vt): void = "mac#atslib_mpz_gcd3_mpz"
overload mpz_gcd with mpz_gcd3_mpz
fun mpz_gcd2_mpz (dst: &mpz_vt, src2: &mpz_vt): void = "mac#atslib_mpz_gcd2_mpz"
overload mpz_gcd with mpz_gcd2_mpz
fun mpz_gcd3_ui
  (dst: &mpz_vt, src1: &mpz_vt, src2: ulint): ulint = "mac#atslib_mpz_gcd3_ui"
overload mpz_gcd with mpz_gcd3_ui
fun mpz_gcd2_ui (dst: &mpz_vt, src2: ulint): ulint = "mac#atslib_mpz_gcd2_ui"
overload mpz_gcd with mpz_gcd2_ui

fun mpz_gcdext // for given a and b, g, s and t are computed s.t. g = a*s + b*t
  (g: &mpz_vt, s: &mpz_vt, t: &mpz_vt, a: &mpz_vt, b: &mpz_vt) : void = "mac#atslib_mpz_gcdext"
// end of [mpz_gcdext]

(* ****** ****** *)

symintr mpz_lcm
fun mpz_lcm3_mpz
  (dst: &mpz_vt, src1: &mpz_vt, src2: &mpz_vt): void = "mac#atslib_mpz_lcm3_mpz"
overload mpz_lcm with mpz_lcm3_mpz
fun mpz_lcm2_mpz (dst: &mpz_vt, src2: &mpz_vt): void = "mac#atslib_mpz_lcm2_mpz"
overload mpz_lcm with mpz_lcm2_mpz
fun mpz_lcm3_ui
  (dst: &mpz_vt, src1: &mpz_vt, src2: ulint): void = "mac#atslib_mpz_lcm3_ui"
overload mpz_lcm with mpz_lcm3_ui
fun mpz_lcm2_ui (dst: &mpz_vt, src2: ulint): void = "mac#atslib_mpz_lcm2_ui"
overload mpz_lcm with mpz_lcm2_ui

(* ****** ****** *)

symintr mpz_invert
fun mpz_invert3
  (dst: &mpz_vt, src1: &mpz_vt, src2: &mpz_vt): int = "mac#atslib_mpz_invert3"
// end of [mpz_invert3]
overload mpz_invert with mpz_invert3

(* ****** ****** *)
//
// various number-theoretic functions
//

symintr mpz_nextprime
fun mpz_nextprime1
  (dst: &mpz_vt): void = "mac#atslib_mpz_nextprime1" // !mac
overload mpz_nextprime with mpz_nextprime1
fun mpz_nextprime2
  (dst: &mpz_vt, src: &mpz_vt): void = "mac#atslib_mpz_nextprime2"
overload mpz_nextprime with mpz_nextprime2

// HX: Note that jacobi (a, b) is only defined for b that is odd
fun mpz_jacobi (a: &mpz_vt, b: &mpz_vt): int = "mac#atslib_mpz_jacobi"
fun mpz_legendre (a: &mpz_vt, b: &mpz_vt): int = "mac#atslib_mpz_legendre"

symintr mpz_kronecker
fun mpz_kronecker_mpz
  (a: &mpz_vt, b: &mpz_vt): int = "mac#atslib_mpz_kronecker_mpz"
overload mpz_kronecker with mpz_kronecker_mpz
fun mpz_kronecker_si (a: &mpz_vt, b: lint): int = "mac#atslib_mpz_kronecker_si"
overload mpz_kronecker with mpz_kronecker_si
fun mpz_kronecker_ui (a: &mpz_vt, b: ulint): int = "mac#atslib_mpz_kronecker_ui"
overload mpz_kronecker with mpz_kronecker_ui
fun mpz_si_kronecker (a: lint, b: &mpz_vt): int = "mac#atslib_mpz_kronecker_si"
fun mpz_ui_kronecker (a: ulint, b: &mpz_vt): int = "mac#atslib_mpz_kronecker_ui"

fun mpz_fac_ui (x: &mpz_vt, n: ulint): void = "mac#atslib_mpz_fac_ui"

symintr mpz_bin_ui
fun mpz_bin3_ui (dst: &mpz_vt, n: &mpz_vt, k: ulint): void = "mac#atslib_mpz_bin3_ui"
overload mpz_bin_ui with mpz_bin3_ui
fun mpz_bin2_ui (dst: &mpz_vt, n: &mpz_vt, k: ulint): void = "mac#atslib_mpz_bin2_ui"
overload mpz_bin_ui with mpz_bin2_ui
fun mpz_bin_uiui (dst: &mpz_vt, n: ulint, k: ulint): void = "mac#atslib_mpz_bin_uiui"

fun mpz_fib_ui (x: &mpz_vt, n: ulint): void = "mac#atslib_mpz_fib_ui"
fun mpz_fib2_ui (x1: &mpz_vt, x2: &mpz_vt, n: ulint): void = "mac#atslib_mpz_fib2_ui"

symintr mpz_remove
fun mpz_remove3
  (dst: &mpz_vt, src1: &mpz_vt, src2: &mpz_vt): void = "mac#atslib_mpz_remove3"
overload mpz_remove with mpz_remove3
fun mpz_remove2 (dst: &mpz_vt, src2: &mpz_vt): void = "atslib_mpz_remove2" // !fun
overload mpz_remove with mpz_remove2

(* ****** ****** *)
//
// some MPZ input/output/print functions
//

fun mpz_inp_str {m:file_mode} (
  pf_mode: file_mode_lte (m, r)
| x: &mpz_vt, file: &FILE m, base: mp_base_t
) : size_t = "mac#atslib_mpz_inp_str"
// end of [mpz_inp_str]

fun mpz_out_str {m:file_mode} (
  pf_mode: file_mode_lte (m, w)
| file: &FILE m, base: mp_base_t, x: &mpz_vt
) : size_t = "mac#atslib_mpz_out_str"
// end of [mpz_out_str]

fun fprint0_mpz
  (out: FILEref, x: &mpz_vt): void = "atslib_fprint_mpz"
overload fprint with fprint0_mpz

fun fprint1_mpz {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: &mpz_vt): void
  = "atslib_fprint_mpz"
overload fprint with fprint1_mpz

fun print_mpz (x: &mpz_vt) : void
overload print with print_mpz
fun prerr_mpz (x: &mpz_vt) : void
overload prerr with prerr_mpz

fun tostrptr_mpz (x: &mpz_vt):<> strptr1
overload tostrptr with tostrptr_mpz
fun tostring_mpz (x: &mpz_vt):<> string
overload tostring with tostring_mpz

(* ****** ****** *)

fun mpz_inp_raw {m:file_mode} (
  pf_mode: file_mode_lte (m, r) | x: &mpz_vt, file: &FILE m
) : size_t = "mac#atslib_mpz_inp_raw" // returns 0 for error
// end of [mpz_inp_raw]

fun mpz_out_raw {m:file_mode} (
  pf_mode: file_mode_lte (m, w) | file: &FILE m, x: &mpz_vt
) : size_t = "mac#atslib_mpz_out_raw" // returns 0 for error
// end of [mpz_out_raw]

(* ****** ****** *)
//
//
// rational number operations
//
//
(* ****** ****** *)

fun mpq_canonicalize (x: &mpq_vt):<> void = "mac#atslib_mpq_canonicalize"

(* ****** ****** *)

// [x] is initialized with 0
fun mpq_init
  (x: &mpq_vt? >> mpq_vt):<> void = "mac#atslib_mpq_init" // macro!
// end of [mpq_init]

fun mpq_clear (x: &mpq_vt >> mpq_vt?): void = "mac#atslib_mpq_clear"

(* ****** ****** *)

fun mpq_get_d (src: &mpq_vt): double = "mac#atslib_mpq_get_d"
fun mpq_get_num (dst: &mpz_vt, src: &mpq_vt):<> void = "mac#atslib_mpq_get_num"
fun mpq_get_den (dst: &mpz_vt, src: &mpq_vt):<> void = "mac#atslib_mpq_get_den"
fun mpq_get_str // HX: a special case of the original namesake in GMP
  (base: mp_base_t, x: &mpq_vt):<> strptr1 = "atslib_mpq_get_str" // fun!
// end of [mpq_get_str]

(* ****** ****** *)

fun mpq_numref (dst: &mpq_vt)
  :<> [l:addr] (mpz_vt @ l, mpz_vt @ l -<lin,prf> void | ptr l)
  = "mac#atslib_mpq_numref"
fun mpq_denref (dst: &mpq_vt)
  :<> [l:addr] (mpz_vt @ l, mpz_vt @ l -<lin,prf> void | ptr l)
  = "mac#atslib_mpq_denref"

(* ****** ****** *)

symintr mpq_set

fun mpq_set_mpq
  (dst: &mpq_vt, src: &mpq_vt): void = "mac#atslib_mpq_set_mpq"
overload mpq_set with mpq_set_mpq
fun mpq_set_mpz
  (dst: &mpq_vt, src: &mpz_vt): void = "mac#atslib_mpq_set_mpz"
overload mpq_set with mpq_set_mpz
fun mpq_set_si
  (dst: &mpq_vt, src1: lint, src2: ulint): void = "mac#atslib_mpq_set_si"
overload mpq_set with mpq_set_si
fun mpq_set_ui
  (dst: &mpq_vt, src1: ulint, src2: ulint): void = "mac#atslib_mpq_set_ui"
overload mpq_set with mpq_set_ui
fun mpq_set_d
  (dst: &mpq_vt, src: double): void = "mac#atslib_mpq_set_d"
overload mpq_set with mpq_set_d
fun mpq_set_mpf
  (dst: &mpq_vt, src: &mpf_vt): void = "mac#atslib_mpq_set_mpf"
overload mpq_set with mpq_set_mpf

// HX: may need to call [mpq_canonicalize]
fun mpq_set_num (dst: &mpq_vt, src: &mpz_vt):<> void = "mac#atslib_mpq_set_num"
fun mpq_set_den (dst: &mpq_vt, src: &mpz_vt):<> void = "mac#atslib_mpq_set_den"

(* ****** ****** *)

symintr mpq_neg

// x := -y
fun mpq_neg2 (x: &mpq_vt, y: &mpq_vt):<> void = "mac#atslib_mpq_neg2"
overload mpq_neg with mpq_neg2

// x := -x
fun mpq_neg1 (x: &mpq_vt):<> void = "atslib_mpq_neg1" // function!
overload mpq_neg with mpq_neg1

(* ****** ****** *)

symintr mpq_inv

// x := -y
fun mpq_inv2 (x: &mpq_vt, y: &mpq_vt):<> void = "mac#atslib_mpq_inv2"
overload mpq_inv with mpq_inv2
// x := -x
fun mpq_inv1 (x: &mpq_vt):<> void = "atslib_mpq_inv1" // function!
overload mpq_inv with mpq_inv1

(* ****** ****** *)

symintr mpq_add
fun mpq_add3_mpq (dst: &mpq_vt, src1: &mpq_vt, src2: &mpq_vt): void
  = "mac#atslib_mpq_add3_mpq"
overload mpq_add with mpq_add3_mpq
fun mpq_add2_mpq // dst := dst + src2
  (dst: &mpq_vt, src2: &mpq_vt): void = "atslib_mpq_add2_mpq" // fun!
overload mpq_add with mpq_add2_mpq

symintr mpq_sub
fun mpq_sub3_mpq (dst: &mpq_vt, src1: &mpq_vt, src2: &mpq_vt): void
  = "mac#atslib_mpq_sub3_mpq"
overload mpq_sub with mpq_sub3_mpq
fun mpq_sub2_mpq // dst := dst - src2
  (dst: &mpq_vt, src2: &mpq_vt): void = "atslib_mpq_sub2_mpq" // fun!
overload mpq_sub with mpq_sub2_mpq

symintr mpq_mul
fun mpq_mul3_mpq (dst: &mpq_vt, src1: &mpq_vt, src2: &mpq_vt): void
  = "mac#atslib_mpq_mul3_mpq"
overload mpq_mul with mpq_mul3_mpq
fun mpq_mul2_mpq // dst := dst * src2
  (dst: &mpq_vt, src2: &mpq_vt): void = "atslib_mpq_mul2_mpq" // fun!
overload mpq_mul with mpq_mul2_mpq

symintr mpq_div
fun mpq_div3_mpq (dst: &mpq_vt, src1: &mpq_vt, src2: &mpq_vt): void
  = "mac#atslib_mpq_div3_mpq"
overload mpq_div with mpq_div3_mpq
fun mpq_div2_mpq // dst := dst / src2
  (dst: &mpq_vt, src2: &mpq_vt): void = "atslib_mpq_div2_mpq" // fun!
overload mpq_div with mpq_div2_mpq

(* ****** ****** *)

// HX-2010-08-11: implemented in ATS
// x := x + p/q
fun mpq_incby (x: &mpq_vt, p: ulint, q: ulint): void
// x := x - p/q
fun mpq_decby (x: &mpq_vt, p: ulint, q: ulint): void

(* ****** ****** *)

// HX-2010-08-11: implemented in ATS
symintr mpq_pow_ui
fun mpq_pow3_ui (dst: &mpq_vt, src1: &mpq_vt, src2: ulint): void
overload mpq_pow_ui with mpq_pow3_ui
fun mpq_pow2_ui (dst: &mpq_vt, src2: ulint): void
overload mpq_pow_ui with mpq_pow2_ui

(* ****** ****** *)

fun mpq_equal
  (src1: &mpq_vt, src2: &mpq_vt):<> bool = "mac#atslib_mpq_equal"
// end of [mpq_equal]

symintr mpq_cmp
fun mpq_cmp_mpq (x: &mpq_vt, y: &mpq_vt):<> int = "mac#atslib_mpq_cmp_mpq"
overload mpq_cmp with mpq_cmp_mpq
fun mpq_cmp_uint (x: &mpq_vt, y: uint):<> int = "mac#atslib_mpq_cmp_uint"
overload mpq_cmp with mpq_cmp_uint
fun mpq_cmp_ulint (x: &mpq_vt, y: ulint):<> int = "mac#atslib_mpq_cmp_ulint"
overload mpq_cmp with mpq_cmp_ulint

(* ****** ****** *)

fun mpq_sgn (x: &mpq_vt):<> Sgn = "mac#atslib_mpq_sgn"

(* ****** ****** *)

(*
//
// some MPQ input/output/print functions
//
*)
fun mpq_inp_str {m:file_mode} (
    pf_mode: file_mode_lte (m, r) | x: &mpq_vt, file: &FILE m, base: mp_base_t
  ) : size_t = "mac#atslib_mpq_inp_str"
// end of [mpq_inp_str]

fun mpq_out_str {m:file_mode} (
    pf_mode: file_mode_lte (m, w) | file: &FILE m, base: mp_base_t, x: &mpq_vt
  ) : size_t = "mac#atslib_mpq_out_str"
// end of [mpq_out_str]

fun fprint0_mpq
  (out: FILEref, x: &mpq_vt): void = "atslib_fprint_mpq"
overload fprint with fprint0_mpq

fun fprint1_mpq {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: &mpq_vt): void
  = "atslib_fprint_mpq"
overload fprint with fprint1_mpq

fun print_mpq (x: &mpq_vt) : void
overload print with print_mpq
fun prerr_mpq (x: &mpq_vt) : void
overload prerr with prerr_mpq

(* ****** ****** *)
//
//
// floating number operations
//
//
(* ****** ****** *)
//
// HX-2010-07-28: [mpf_set_default_prec] is used to make sure that
//
sta mpf_set_default_prec : bool // [mpf_set_default_prec] is called

fun mpf_get_default_prec
  {mpf_set_default_prec} (): ulint = "mac#atslib_mpf_get_default_prec"
// end of [mpf_get_default_prec]

fun mpf_set_default_prec
  (prec: ulint): [mpf_set_default_prec] void = "mac#atslib_mpf_set_default_prec"
// end of [mpf_set_default_prec]

(* ****** ****** *)
//
// HX-2010-07-28:
// [mpf_init] must be called after [mpf_set_default_prec] is called
//
fun mpf_init {mpf_set_default_prec}
  (x: &mpf_vt? >> mpf_vt):<> void = "mac#atslib_mpf_init"
// end of [mpf_init]

fun mpf_init2
  (x: &mpf_vt? >> mpf_vt, prec: ulint): void = "mac#atslib_mpf_init2"
// end of [mpf_init2]

fun mpf_clear (x: &mpf_vt >> mpf_vt?): void = "mac#atslib_mpf_clear"

(* ****** ****** *)

fun mpf_get_prec (x: &mpf_vt): ulint = "mac#atslib_mpf_get_prec"

fun mpf_set_prec (x: &mpf_vt, prec: ulint): void = "mac#atslib_mpf_set_prec"

fun mpf_set_prec_raw // HX: a low-level function
  (dst: &mpf_vt, prec: ulint): void = "mac#atslib_mpf_set_prec_raw"
// end of [mpf_set_prec_raw]

(* ****** ****** *)

fun mpf_get_d (src: &mpf_vt): double = "mac#atslib_mpf_get_d"
fun mpf_get_d_2exp
  (exp: &lint, src: &mpf_vt): double = "mac#atslib_mpf_get_d_2exp"
fun mpf_get_si (src: &mpf_vt): lint = "mac#atslib_mpf_get_si"
fun mpf_get_ui (src: &mpf_vt): ulint = "mac#atslib_mpf_get_ui"

fun mpf_get_str ( // HX: a special case of the original namesake in GMP
    exp: &mp_exp_t? >> mp_exp_t, base: mp_base_t, ndigit: size_t, x: &mpf_vt
  ) : strptr1 = "atslib_mpf_get_str" // function!
// end of [mpf_get_str]

(* ****** ****** *)

symintr mpf_set

fun mpf_set_mpf
  (dst: &mpf_vt, src: &mpf_vt): void = "mac#atslib_mpf_set_mpf"
overload mpf_set with mpf_set_mpf
fun mpf_set_mpz
  (dst: &mpf_vt, src: &mpz_vt): void = "mac#atslib_mpf_set_mpz"
overload mpf_set with mpf_set_mpz
fun mpf_set_si (dst: &mpf_vt, src: lint): void = "mac#atslib_mpf_set_si"
overload mpf_set with mpf_set_si
fun mpf_set_ui
  (dst: &mpf_vt, src: ulint): void = "mac#atslib_mpf_set_ui"
overload mpf_set with mpf_set_ui
fun mpf_set_mpq
  (dst: &mpf_vt, src: &mpq_vt): void = "mac#atslib_mpf_set_mpq"
overload mpf_set with mpf_set_mpq
fun mpf_set_d (dst: &mpf_vt, src: double): void = "mac#atslib_mpf_set_d"
overload mpf_set with mpf_set_d

//
// HX: the function returns 0 if the string is valid, or -1 otherwise.
//
fun mpf_set_str // succ/fail: 0/-1
  (dst: &mpf_vt, str: string, base: int): int = "mac#atslib_mpf_set_str"
overload mpf_set with mpf_set_str

fun mpf_set_str_exn
  (dst: &mpf_vt, str: string, base: int): void = "atslib_mpf_set_str_exn" // !fun
// end of [mpf_set_str_exn]

(* ****** ****** *)

symintr mpf_init_set

// dst := src
fun mpf_init_set_mpf {mpf_set_default_prec}
  (dst: &mpf_vt? >> mpf_vt, src: &mpf_vt): void = "mac#atslib_mpf_init_set_mpf"
overload mpf_init_set with mpf_init_set_mpf
fun mpf_init_set_si {mpf_set_default_prec}
  (dst: &mpf_vt? >> mpf_vt, src: lint): void = "mac#atslib_mpf_init_set_si"
overload mpf_init_set with mpf_init_set_si
fun mpf_init_set_ui {mpf_set_default_prec}
  (dst: &mpf_vt? >> mpf_vt, src: ulint): void = "mac#atslib_mpf_init_set_ui"
overload mpf_init_set with mpf_init_set_ui
fun mpf_init_set_d {mpf_set_default_prec}
  (dst: &mpf_vt? >> mpf_vt, src: double): void = "mac#atslib_mpf_init_set_d"
overload mpf_init_set with mpf_init_set_d
fun mpf_init_set_str {mpf_set_default_prec}
  (rop: &mpf_vt? >> mpf_vt, str: string, base: int): int(*err*)
  = "mac#atslib_mpf_init_set_str" // macro
overload mpf_init_set with mpf_init_set_str

(* ****** ****** *)

fun mpf_swap (dst1: &mpf_vt, dst2: &mpf_vt): void = "mac#atslib_mpf_swap"

(* ****** ****** *)

fun mpf_ceil (dst: &mpf_vt, src: &mpf_vt):<> void = "mac#atslib_mpf_ceil"
fun mpf_floor (dst: &mpf_vt, src: &mpf_vt):<> void = "mac#atslib_mpf_floor"
fun mpf_trunc (dst: &mpf_vt, src: &mpf_vt):<> void = "mac#atslib_mpf_trunc"

fun mpf_integer_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_integer_p"
fun mpf_int_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_int_p"
fun mpf_uint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_uint_p"
fun mpf_lint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_lint_p"
fun mpf_ulint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_ulint_p"
fun mpf_sint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_sint_p"
fun mpf_usint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_usint_p"

fun mpf_fits_int_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_fits_int_p"
fun mpf_fits_uint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_fits_uint_p"
fun mpf_fits_lint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_fits_lint_p"
fun mpf_fits_ulint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_fits_ulint_p"
fun mpf_fits_sint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_fits_sint_p"
fun mpf_fits_usint_p (src: &mpf_vt):<> bool = "mac#atslib_mpf_fits_usint_p"

(* ****** ****** *)

symintr mpf_neg

// x := -y
fun mpf_neg2 (x: &mpf_vt, y: &mpf_vt): void = "mac#atslib_mpf_neg2"
overload mpf_neg with mpf_neg2
// x := -x
fun mpf_neg1 (x: &mpf_vt): void = "atslib_mpf_neg1" // !function
overload mpf_neg with mpf_neg1

(* ****** ****** *)

symintr mpf_abs

// x := |y|
fun mpf_abs2 (x: &mpf_vt, y: &mpf_vt): void = "mac#atslib_mpf_abs2"
overload mpf_abs with mpf_abs2
// x := |x|
fun mpf_abs1 (x: &mpf_vt): void = "atslib_mpf_abs1" // !function
overload mpf_abs with mpf_abs1

(* ****** ****** *)

symintr mpf_add

fun mpf_add3_mpf
  (dst: &mpf_vt, src1: &mpf_vt, src2: &mpf_vt): void = "mac#atslib_mpf_add3_mpf"
overload mpf_add with mpf_add3_mpf
fun mpf_add3_ui
  (dst: &mpf_vt, src1: &mpf_vt, src2: ulint): void = "mac#atslib_mpf_add3_ui"
overload mpf_add with mpf_add3_ui

fun mpf_add2_mpf
  (dst: &mpf_vt, src: &mpf_vt): void = "atslib_mpf_add2_mpf" // fun!
overload mpf_add with mpf_add2_mpf
fun mpf_add2_ui (dst: &mpf_vt, src: ulint): void = "atslib_mpf_add2_ui" // fun!
overload mpf_add with mpf_add2_ui

(* ****** ****** *)

symintr mpf_sub

fun mpf_sub3_mpf (dst: &mpf_vt, src1: &mpf_vt, src2: &mpf_vt): void
  = "mac#atslib_mpf_sub_mpf"
overload mpf_sub with mpf_sub3_mpf
fun mpf_sub3_ui
  (dst: &mpf_vt, src1: &mpf_vt, src2: ulint): void = "mac#atslib_mpf_sub3_ui"
overload mpf_sub with mpf_sub3_ui
fun mpf_ui_sub3
  (dst: &mpf_vt, src1: ulint, src2: &mpf_vt): void = "mac#atslib_mpf_ui_sub3"
// end of [mpf_ui_sub3]

fun mpf_sub2_mpf (dst: &mpf_vt, src2: &mpf_vt): void = "atslib_mpf_sub2" // !fun
overload mpf_sub with mpf_sub2_mpf
fun mpf_sub2_ui (dst: &mpf_vt, src2: ulint): void = "atslib_mpf_sub2_ui" // !fun
overload mpf_sub with mpf_sub2_ui

// HX-2010-08-08: no overloading for this one
fun mpf_ui_sub2 (dst: &mpf_vt, src1: ulint): void = "atslib_mpf_ui_sub2" // !fun

(* ****** ****** *)

symintr mpf_mul
fun mpf_mul3_mpf
  (dst: &mpf_vt, src1: &mpf_vt, src2: &mpf_vt): void = "mac#atslib_mpf_mul3_mpf"
overload mpf_mul with mpf_mul3_mpf
fun mpf_mul3_ui
  (dst: &mpf_vt, src1: &mpf_vt, src2: ulint): void = "mac#atslib_mpf_mul3_ui"
overload mpf_mul with mpf_mul3_ui
fun mpf_mul2_mpf
  (dst: &mpf_vt, src: &mpf_vt): void = "atslib_mpf_mul2_mpf" // !function
overload mpf_mul with mpf_mul2_mpf
fun mpf_mul2_ui (dst: &mpf_vt, src: &mpf_vt): void = "atslib_mpf_mul2_ui" // !fun
overload mpf_mul with mpf_mul2_ui

(* ****** ****** *)

symintr mpf_div
fun mpf_div3_mpf (dst: &mpf_vt, src1: &mpf_vt, src2: &mpf_vt): void
  = "mac#atslib_mpf_div3_mpf"
overload mpf_div with mpf_div3_mpf
fun mpf_div3_ui
  (dst: &mpf_vt, src1: &mpf_vt, src2: ulint): void = "mac#atslib_mpf_div3_ui"
overload mpf_div with mpf_div3_ui
fun mpf_ui_div3
  (dst: &mpf_vt, src1: ulint, src2: &mpf_vt): void = "mac#atslib_mpf_ui_div3"
// end of [mpf_ui_div3]
fun mpf_div2_mpf
  (dst: &mpf_vt, src2: &mpf_vt): void = "atslib_mpf_div2" // !function
overload mpf_div with mpf_div2_mpf
fun mpf_div2_ui (dst: &mpf_vt, src2: ulint): void = "atslib_mpf_div2_ui" // !fun
overload mpf_div with mpf_div2_ui

// HX-2010-08-08: no overloading for this one
fun mpf_ui_div2 (dst: &mpf_vt, src1: ulint): void = "atslib_mpf_ui_div2" // !fun

(* ****** ****** *)

symintr mpf_sqrt
fun mpf_sqrt2_mpf // dst := sqrt (src)
  (dst: &mpf_vt, src: &mpf_vt): void = "mac#atslib_mpf_sqrt2_mpf"
overload mpf_sqrt with mpf_sqrt2_mpf
fun mpf_sqrt2_ui (dst: &mpf_vt, src: ulint): void = "mac#atslib_mpf_sqrt2_ui"
overload mpf_sqrt with mpf_sqrt2_ui
// dst := sqrt (dst)
fun mpf_sqrt1_mpf (dst: &mpf_vt): void = "atslib_mpf_sqrt1_mpf" // !fun
overload mpf_sqrt with mpf_sqrt1_mpf

(* ****** ****** *)

symintr mpf_pow_ui
fun mpf_pow3_ui // dst := src1^^src2
  (dst: &mpf_vt, src1: &mpf_vt, src2: ulint): void = "mac#atslib_mpf_pow3_ui"
overload mpf_pow_ui with mpf_pow3_ui
// dst := src1^^src2
fun mpf_pow2_ui (dst: &mpf_vt, src2: ulint): void = "atslib_mpf_pow2_ui" // !fun
overload mpf_pow_ui with mpf_pow2_ui

(* ****** ****** *)

symintr mpf_mul_2exp
fun mpf_mul3_2exp (
  dst: &mpf_vt, src1: &mpf_vt, src2: ulint
) : void = "mac#atslib_mpf_mul3_2exp"
overload mpf_mul_2exp with mpf_mul3_2exp
fun mpf_mul2_2exp
  (dst: &mpf_vt, src2: ulint): void = "atslib_mpf_mul2_2exp" // !function
overload mpf_mul_2exp with mpf_mul2_2exp

(* ****** ****** *)

symintr mpf_div_2exp
fun mpf_div3_2exp (
  dst: &mpf_vt, src1: &mpf_vt, src2: ulint
) : void = "mac#atslib_mpf_div3_2exp"
overload mpf_div_2exp with mpf_div3_2exp
fun mpf_div2_2exp (
  dst: &mpf_vt, src2: ulint
) : void = "atslib_mpf_div2_2exp" // !function
overload mpf_div_2exp with mpf_div2_2exp

(* ****** ****** *)

fun mpf_eq (
  src1: &mpf_vt, src2: &mpf_vt, src3: ulint
) :<> bool = "mac#atslib_mpf_eq"
// end of [mpf_eq]

symintr mpf_cmp
fun mpf_cmp_mpf (
  src1: &mpf_vt, src2: &mpf_vt
) :<> int = "mac#atslib_mpf_cmp_mpf"
overload mpf_cmp with mpf_cmp_mpf
fun mpf_cmp_d (
  src1: &mpf_vt, src2: double
) :<> int = "mac#atslib_mpf_cmp_d"
overload mpf_cmp with mpf_cmp_d
fun mpf_cmp_ui
  (src1: &mpf_vt, src2: ulint):<> int = "mac#atslib_mpf_cmp_ui"
overload mpf_cmp with mpf_cmp_ui
fun mpf_cmp_si
  (src1: &mpf_vt, src2: lint):<> int = "mac#atslib_mpf_cmp_si"
overload mpf_cmp with mpf_cmp_si

(* ****** ****** *)

fun mpf_sgn
  (x: &mpf_vt):<> Sgn = "mac#atslib_mpf_sgn"
// end of [mpf_sgn]

(* ****** ****** *)

fun mpf_reldiff (
  dst: &mpf_vt, src1: &mpf_vt, src2: &mpf_vt
) : void = "mac#atslib_mpf_reldiff"
// end of [mpf_reldiff]

(* ****** ****** *)

(*
//
// some MPF input/output/print functions
//
*)
fun mpf_inp_str {m:file_mode} (
  pf_mode: file_mode_lte (m, r)
| x: &mpf_vt, file: &FILE m, base: mp_base_t
) : size_t = "mac#atslib_mpf_inp_str"
// end of [mpf_inp_str]

fun mpf_out_str {m:file_mode} (
  pf_mode: file_mode_lte (m, w)
| file: &FILE m, base: mp_base_t, ndigit: size_t, x: &mpf_vt
) : size_t = "mac#atslib_mpf_out_str"
// end of [mpf_out_str]

fun fprint0_mpf
  (out: FILEref, x: &mpf_vt, ndigit: size_t): void = "atslib_fprint_mpf"
fun fprint1_mpf {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: &mpf_vt, ndigit: size_t): void
  = "atslib_fprint_mpf"
fun print_mpf (x: &mpf_vt, ndigit: size_t) : void = "atslib_print_mpf"
fun prerr_mpf (x: &mpf_vt, ndigit: size_t) : void = "atslib_prerr_mpf"

(* ****** ****** *)
//
//
// low-level MPN functions
//
// HX-2010-08-13:
// These low-level functions should probably be supported. Any volunteer?
//
(* ****** ****** *)
//
//
// random number generators for MPZ, MPQ and MPF
//
//
(* ****** ****** *)

absviewt@ype gmp_randstate_vt = $extype"ats_gmp_randstate_viewt0ype"

fun gmp_randclear (
  state: &gmp_randstate_vt >> gmp_randstate_vt?
) : void = "mac#atslib_gmp_randclear"
// end of [gmp_randclear]

fun gmp_randinit_default (
  state: &gmp_randstate_vt? >> gmp_randstate_vt
) : void = "mac#atslib_gmp_randinit_default"
// end of [gmp_randinit_default]

fun gmp_randinit_lc_2exp (
  state: &gmp_randstate_vt? >> gmp_randstate_vt
, a: &mpz_vt, c: ulint, m2exp: ulint
) : void
  = "mac#atslib_gmp_randinit_lc_2exp"
// end of [gmp_randinit_lc_2exp]

fun gmp_randinit_lc_2exp_size (
  state: &gmp_randstate_vt? >> gmp_randstate_vt, _size: ulint
) : int(*err*)
  = "mac#atslib_gmp_randinit_lc_2exp_size"
// end of [gmp_randinit_lc_2exp_size]

(* ****** ****** *)

symintr gmp_randseed
fun gmp_randseed_mpz (
  state: &gmp_randstate_vt, seed: &mpz_vt
) : void = "mac#atslib_gmp_randseed_mpz"
overload gmp_randseed with gmp_randseed_mpz
fun gmp_randseed_ui (
  state: &gmp_randstate_vt, seed: ulint
) : void = "mac#atslib_gmp_randseed_ui"
overload gmp_randseed with gmp_randseed_ui

(* ****** ****** *)

fun mpz_urandomb (
  dst: &mpz_vt, state: &gmp_randstate_vt, nbit: ulint
) : void = "mac#atslib_mpz_urandomb"
// end of [mpz_urandomb]

fun mpz_urandomm ( // each generated X belongs to [0, range)
  dst: &mpz_vt, state: &gmp_randstate_vt, range: &mpz_vt
) : void = "mac#atslib_mpz_urandomm"
// end of [mpz_urandomm]

fun mpz_rrandomb (
  dst: &mpz_vt, state: &gmp_randstate_vt, nbit: ulint
) : void = "mac#atslib_mpz_rrandomb"
// end of [mpz_rrandomb]

fun mpz_random (dst: &mpz_vt, max_size: mp_size_t): void = "mac#atslib_mpz_random"
fun mpz_random2 (dst: &mpz_vt, max_size: mp_size_t): void = "mac#atslib_mpz_random2"

(* ****** ****** *)
//
// HX-2010-08-08: a negative number is generated if [max_size] is negative
//
fun mpf_random2 (
  dst: &mpf_vt, max_size: mp_size_t, exp: mp_exp_t
) : void = "mac#atslib_mpf_random2"
// end of [mpf_random2]

fun mpf_urandomb (
  dst: &mpf_vt, state: &gmp_randstate_vt, nbit: ulint
) : void = "mac#atslib_mpf_urandomb"
// end of [mpf_urandomb]

(* ****** ****** *)

(* end of [gmp.sats] *)
