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

%{#
#include "libc/CATS/complex.cats"
%} // end of [%{#}

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

(*
** complex numbers of single precision
*)

abst@ype ccmplx_t0ype = $extype"ats_fcomplex_type"
typedef ccmplx = ccmplx_t0ype
symintr ccmplx_of

(* ****** ****** *)

fun fprint_ccmplx {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: ccmplx):<!exnref> void
  = "atslib_fprint_ccmplx"
overload fprint with fprint_ccmplx

fun print_ccmplx (c: ccmplx):<!exnref> void
and prerr_ccmplx (c: ccmplx):<!exnref> void
overload print with print_ccmplx
overload prerr with prerr_ccmplx

(* ****** ****** *)

val ccmplx_imag_unit : ccmplx
  = "atslib_ccmplx_imag_unit" // imaginary unit
// end of [val]

(* ****** ****** *)

fun ccmplx_of_int (i: int):<> ccmplx
  = "atslib_ccmplx_of_int"
overload ccmplx_of with ccmplx_of_int

fun ccmplx_of_float (f: float):<> ccmplx
  = "atslib_ccmplx_of_float"
overload ccmplx_of with ccmplx_of_float

fun ccmplx_make_cart (f1: float, f2: float):<> ccmplx
  = "atslib_ccmplx_make_cart"

fun ccmplx_make_polar (f1: float, f2: float):<> ccmplx
  = "atslib_ccmplx_make_polar"

(* ****** ****** *)

fun crealf (c: ccmplx):<> float = "atslib_crealf"
fun ccmplx_real (c: ccmplx):<> float = "atslib_crealf"

fun cimagf (c: ccmplx):<> float = "atslib_cimagf"
fun ccmplx_imag (c: ccmplx):<> float = "atslib_cimagf"

(* ****** ****** *)

fun neg_ccmplx
  (c: ccmplx):<> ccmplx = "atslib_neg_ccmplx"
overload ~ with neg_ccmplx

fun add_ccmplx_ccmplx (c1: ccmplx, c2: ccmplx):<> ccmplx
  = "atslib_add_ccmplx_ccmplx"
overload + with add_ccmplx_ccmplx

fun sub_ccmplx_ccmplx (c1: ccmplx, c2: ccmplx):<> ccmplx
  = "atslib_sub_ccmplx_ccmplx"
overload - with sub_ccmplx_ccmplx

fun mul_ccmplx_ccmplx (c1: ccmplx, c2: ccmplx):<> ccmplx
  = "atslib_mul_ccmplx_ccmplx"
overload * with mul_ccmplx_ccmplx

fun div_ccmplx_ccmplx (c1: ccmplx, c2: ccmplx):<> ccmplx
  = "atslib_div_ccmplx_ccmplx"
overload / with div_ccmplx_ccmplx

(* ****** ****** *)

fun eq_ccmplx_ccmplx (c1: ccmplx, c2: ccmplx):<> bool
  = "atslib_eq_ccmplx_ccmplx"
overload = with eq_ccmplx_ccmplx

fun neq_ccmplx_ccmplx (c1: ccmplx, c2: ccmplx):<> bool
  = "atslib_neq_ccmplx_ccmplx"
overload <> with neq_ccmplx_ccmplx

(* ****** ****** *)

fun cabsf (c: ccmplx):<> float = "atslib_cabsf"
fun abs_ccmplx (c: ccmplx):<> float = "atslib_cabsf"
overload abs with abs_ccmplx

fun csqrtf (c: ccmplx):<> ccmplx = "atslib_csqrtf"
fun sqrt_ccmplx (c: ccmplx):<> ccmplx = "atslib_csqrtf"
overload sqrt with sqrt_ccmplx

(* ****** ****** *)

fun cargf (c: ccmplx):<> float = "atslib_cargf"
fun arg_ccmplx (c: ccmplx):<> float = "atslib_cargf"

fun conjf (c: ccmplx):<> ccmplx = "atslib_conjf"
fun conj_ccmplx (c: ccmplx):<> ccmplx = "atslib_conjf"

(* ****** ****** *)

fun csinf (c: ccmplx):<> ccmplx = "atslib_csinf"
fun sin_ccmplx (c: ccmplx):<> ccmplx = "atslib_csinf"

fun ccosf (c: ccmplx):<> ccmplx = "atslib_ccosf"
fun cos_ccmplx (c: ccmplx):<> ccmplx = "atslib_ccosf"

fun ctanf (c: ccmplx):<> ccmplx = "atslib_ctanf"
fun tan_ccmplx (c: ccmplx):<> ccmplx = "atslib_ctanf"

(* ****** ****** *)

fun casinf (c: ccmplx):<> ccmplx = "atslib_casinf"
fun asin_ccmplx (c: ccmplx):<> ccmplx = "atslib_casinf"

fun cacosf (c: ccmplx):<> ccmplx = "atslib_cacosf"
fun acos_ccmplx (c: ccmplx):<> ccmplx = "atslib_cacosf"

fun catanf (c: ccmplx):<> ccmplx = "atslib_catanf"
fun atan_ccmplx (c: ccmplx):<> ccmplx = "atslib_catanf"

(* ****** ****** *)

fun csinhf (c: ccmplx):<> ccmplx = "atslib_csinhf"
fun sinh_ccmplx (c: ccmplx):<> ccmplx = "atslib_csinhf"

fun ccoshf (c: ccmplx):<> ccmplx = "atslib_ccoshf"
fun cosh_ccmplx (c: ccmplx):<> ccmplx = "atslib_ccoshf"

fun ctanhf (c: ccmplx):<> ccmplx = "atslib_ctanhf"
fun tanh_ccmplx (c: ccmplx):<> ccmplx = "atslib_ctanhf"

(* ****** ****** *)

fun casinhf (c: ccmplx):<> ccmplx = "atslib_casinhf"
fun asinh_ccmplx (c: ccmplx):<> ccmplx = "atslib_casinhf"

fun cacoshf (c: ccmplx):<> ccmplx = "atslib_cacoshf"
fun acosh_ccmplx (c: ccmplx):<> ccmplx = "atslib_cacoshf"

fun catanhf (c: ccmplx):<> ccmplx = "atslib_catanhf"
fun atanh_ccmplx (c: ccmplx):<> ccmplx = "atslib_catanhf"

(* ****** ****** *)

fun cexpf (c: ccmplx):<> ccmplx = "atslib_cexpf"
fun exp_ccmplx (c: ccmplx):<> ccmplx = "atslib_cexpf"

fun clogf (c: ccmplx):<> ccmplx = "atslib_clogf"
fun log_ccmplx (c: ccmplx):<> ccmplx = "atslib_clogf"

fun cpowf (c1: ccmplx, c2: ccmplx):<> ccmplx
  = "atslib_cpowf"
fun pow_ccmplx_ccmplx (c1: ccmplx, c2: ccmplx):<> ccmplx
  = "atslib_cpowf"
overload pow with pow_ccmplx_ccmplx
fun pow_ccmplx_float (c1: ccmplx, c2: float):<> ccmplx
  = "mac#atslib_pow_ccmplx_float" // macro!
overload pow with pow_ccmplx_float

(* ****** ****** *)

fun cprojf (c: ccmplx):<> float = "atslib_cprojf"

(* ****** ****** *)

(*
** complex numbers of double precision
*)

(* ****** ****** *)

abst@ype zcmplx_t0ype = $extype"ats_dcomplex_type"
typedef zcmplx = zcmplx_t0ype
symintr zcmplx_of

(* ****** ****** *)

fun fprint_zcmplx {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: zcmplx):<!exnref> void
  = "atslib_fprint_zcmplx"
overload fprint with fprint_zcmplx

fun print_zcmplx (z: zcmplx):<!exnref> void
and prerr_zcmplx (z: zcmplx):<!exnref> void
overload print with print_zcmplx
overload prerr with prerr_zcmplx

(* ****** ****** *)

val zcmplx_imag_unit : zcmplx
  = "atslib_zcmplx_imag_unit" // imaginary unit
// end of [val]

(* ****** ****** *)

fun zcmplx_of_int (i: int):<> zcmplx
  = "atslib_zcmplx_of_int"
overload zcmplx_of with zcmplx_of_int

fun zcmplx_of_double (d: double):<> zcmplx
  = "atslib_zcmplx_of_double"
overload zcmplx_of with zcmplx_of_double

fun zcmplx_make_cart (d1: double, d2: double):<> zcmplx
  = "atslib_zcmplx_make_cart"

fun zcmplx_make_polar (d1: double, d2: double):<> zcmplx
  = "atslib_zcmplx_make_polar"

(* ****** ****** *)

fun creal (z: zcmplx):<> double = "atslib_creal"
fun zcmplx_real (z: zcmplx):<> double = "atslib_creal"

fun cimag (z: zcmplx):<> double = "atslib_cimag"
fun zcmplx_imag (z: zcmplx):<> double = "atslib_cimag"

(* ****** ****** *)

fun neg_zcmplx
  (z: zcmplx):<> zcmplx = "atslib_neg_zcmplx"
overload ~ with neg_zcmplx

fun add_zcmplx_zcmplx (z1: zcmplx, z2: zcmplx):<> zcmplx
  = "atslib_add_zcmplx_zcmplx"
overload + with add_zcmplx_zcmplx

fun sub_zcmplx_zcmplx (z1: zcmplx, z2: zcmplx):<> zcmplx
  = "atslib_sub_zcmplx_zcmplx"
overload - with sub_zcmplx_zcmplx

fun mul_zcmplx_zcmplx (z1: zcmplx, z2: zcmplx):<> zcmplx
  = "atslib_mul_zcmplx_zcmplx"
overload * with mul_zcmplx_zcmplx

fun div_zcmplx_zcmplx (z1: zcmplx, z2: zcmplx):<> zcmplx
  = "atslib_div_zcmplx_zcmplx"
overload / with div_zcmplx_zcmplx

(* ****** ****** *)

fun eq_zcmplx_zcmplx (c1: zcmplx, c2: zcmplx):<> bool
  = "atslib_eq_zcmplx_zcmplx"
overload = with eq_zcmplx_zcmplx

fun neq_zcmplx_zcmplx (c1: zcmplx, c2: zcmplx):<> bool
  = "atslib_neq_zcmplx_zcmplx"
overload <> with neq_zcmplx_zcmplx

(* ****** ****** *)

fun cabs (z: zcmplx):<> double = "atslib_cabs"
fun abs_zcmplx (z: zcmplx):<> double = "atslib_cabs"
overload abs with abs_zcmplx

fun csqrt (z: zcmplx):<> zcmplx = "atslib_csqrt"
fun sqrt_zcmplx (z: zcmplx):<> zcmplx = "atslib_csqrt"
overload sqrt with sqrt_zcmplx

(* ****** ****** *)

fun carg (z: zcmplx):<> double = "atslib_carg"
fun arg_zcmplx (z: zcmplx):<> double = "atslib_carg"

fun conj (z: zcmplx):<> zcmplx = "atslib_conj"
fun conj_zcmplx (z: zcmplx):<> zcmplx = "atslib_conj"

(* ****** ****** *)

fun csin (z: zcmplx):<> zcmplx = "atslib_csin"
fun sin_zcmplx (z: zcmplx):<> zcmplx = "atslib_csin"

fun ccos (z: zcmplx):<> zcmplx = "atslib_ccos"
fun cos_zcmplx (z: zcmplx):<> zcmplx = "atslib_ccos"

fun ctan (z: zcmplx):<> zcmplx = "atslib_ctan"
fun tan_zcmplx (z: zcmplx):<> zcmplx = "atslib_ctan"

(* ****** ****** *)

fun casin (z: zcmplx):<> zcmplx = "atslib_casin"
fun asin_zcmplx (z: zcmplx):<> zcmplx = "atslib_casin"

fun cacos (z: zcmplx):<> zcmplx = "atslib_cacos"
fun acos_zcmplx (z: zcmplx):<> zcmplx = "atslib_cacos"

fun catan (z: zcmplx):<> zcmplx = "atslib_catan"
fun atan_zcmplx (z: zcmplx):<> zcmplx = "atslib_catan"

(* ****** ****** *)

fun csinh (z: zcmplx):<> zcmplx = "atslib_csinh"
fun sinh_zcmplx (z: zcmplx):<> zcmplx = "atslib_csinh"

fun ccosh (z: zcmplx):<> zcmplx = "atslib_ccosh"
fun cosh_zcmplx (z: zcmplx):<> zcmplx = "atslib_ccosh"

fun ctanh (z: zcmplx):<> zcmplx = "atslib_ctanh"
fun tanh_zcmplx (z: zcmplx):<> zcmplx = "atslib_ctanh"

(* ****** ****** *)

fun casinh (z: zcmplx):<> zcmplx = "atslib_casinh"
fun asinh_zcmplx (z: zcmplx):<> zcmplx = "atslib_casinh"

fun cacosh (z: zcmplx):<> zcmplx = "atslib_cacosh"
fun acosh_zcmplx (z: zcmplx):<> zcmplx = "atslib_cacosh"

fun catanh (z: zcmplx):<> zcmplx = "atslib_catanh"
fun atanh_zcmplx (z: zcmplx):<> zcmplx = "atslib_catanh"

(* ****** ****** *)

fun cexp (z: zcmplx):<> zcmplx = "atslib_cexp"
fun exp_zcmplx (z: zcmplx):<> zcmplx = "atslib_cexp"

fun clog (z: zcmplx):<> zcmplx = "atslib_clog"
fun log_zcmplx (z: zcmplx):<> zcmplx = "atslib_clog"

fun cpow (z1: zcmplx, z2: zcmplx):<> zcmplx
  = "atslib_cpow"
fun pow_zcmplx_zcmplx (z1: zcmplx, z2: zcmplx):<> zcmplx
  = "atslib_cpow"
overload pow with pow_zcmplx_zcmplx
fun pow_zcmplx_double (z1: zcmplx, z2: double):<> zcmplx
  = "mac#atslib_pow_zcmplx_double" // macro!
overload pow with pow_zcmplx_double

(* ****** ****** *)

fun cproj (z: zcmplx):<> double = "atslib_cproj"

(* ****** ****** *)

(* end of [complex.sats] *)
