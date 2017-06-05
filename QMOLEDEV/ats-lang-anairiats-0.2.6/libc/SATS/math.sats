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
#include "libc/CATS/math.cats"
%} // end of [%{#]

(* ****** ****** *)

(*
#define ATS_STALOADFLAG 0 // there is no need for staloading
*)

(* ****** ****** *)

macdef M_E = 2.7182818284590452354	// e
macdef M_PI = 3.14159265358979323846	// pi
macdef M_PI_2 = 1.57079632679489661923	// pi/2
macdef M_PI_4 = 0.78539816339744830962	// pi/4

(* ****** ****** *)
//
// HX-2011-08-01:
// each floating point number can be put in one of these categories:
//
macdef FP_NAN = $extval (int, "FP_NAN")
macdef FP_INFINITE = $extval (int, "FP_INFINITE")
macdef FP_ZERO = $extval (int, "FP_ZERO")
macdef FP_SUBNORMAL = $extval (int, "FP_SUBNORMAL")
macdef FP_NORMAL = $extval (int, "FP_NORMAL")

fun{a:t@ype} fpclassify (x: a):<> int
fun fpclassify_double
  (x: double):<> int = "mac#atslib_fpclassify"
fun fpclassify_float
  (x: float):<> int = "mac#atslib_fpclassify"
fun fpclassify_ldouble
  (x: ldouble):<> int = "mac#atslib_fpclassify"

fun{a:t@ype} isfinite (x: a):<> int
fun isfinite_double
  (x: double):<> int = "mac#atslib_isfinite"
fun isfinite_float
  (x: float):<> int = "mac#atslib_isfinite"
fun isfinite_ldouble
  (x: ldouble):<> int = "mac#atslib_isfinite"

fun{a:t@ype} isnormal (x: a):<> int
fun isnormal_double
  (x: double):<> int = "mac#atslib_isnormal"
fun isnormal_float
  (x: float):<> int = "mac#atslib_isnormal"
fun isnormal_ldouble
  (x: ldouble):<> int = "mac#atslib_isnormal"

(* ****** ****** *)
//
// HX: BSD provision
//
fun{a:t@ype} isinftmp (x: a):<> int
fun isinf (d: double):<> int = "mac#atslib_isinf"
fun isinff (f: float):<> int = "mac#atslib_isinff"
fun isinfl (ld: ldouble):<> int = "mac#atslib_isinfl"

fun{a:t@ype} isnantmp (x: a):<> int
fun isnan (d: double):<> int = "mac#atslib_isnan"
fun isnanf (f: float):<> int = "mac#atslib_isnanf"
fun isnanl (ld: ldouble):<> int = "mac#atslib_isnanl"

(* ****** ****** *)

fun{a:t@ype} ceiltmp (x: a):<> a
fun ceil (d: double):<> double = "mac#atslib_ceil"
fun ceilf (f: float):<> float = "mac#atslib_ceilf"
fun ceill (ld: ldouble):<> ldouble = "mac#atslib_ceill"

(* ****** ****** *)

fun{a:t@ype} floortmp (x: a):<> a
fun floor (d: double):<> double = "mac#atslib_floor"
fun floorf (f: float):<> float = "mac#atslib_floorf"
fun floorl (ld: ldouble):<> ldouble = "mac#atslib_floorl"

(* ****** ****** *)

fun{a:t@ype} roundtmp (x: a):<> a
fun round (d: double):<> double = "mac#atslib_round"
fun roundf (f: float):<> float = "mac#atslib_roundf"
fun roundl (ld: ldouble):<> ldouble = "mac#atslib_roundl"

(* ****** ****** *)

fun{a:t@ype} trunctmp (x: a):<> a
fun trunc (d: double):<> double = "mac#atslib_trunc"
fun truncf (f: float):<> float = "mac#atslib_truncf"
fun truncl (ld: ldouble):<> ldouble = "mac#atslib_truncl"

(* ****** ****** *)

fun{a:t@ype} fmodtmp (x1: a, x2: a):<> a
fun fmod (d1: double, d2: double):<> double = "mac#atslib_fmod"
fun fmodf (f1: float, f2: float):<> float = "mac#atslib_fmodf"
fun fmodl (ld1: ldouble, ld2: ldouble):<> ldouble = "mac#atslib_fmodl"

(* ****** ****** *)

fun{a:t@ype} fmaxtmp (x1: a, x2: a):<> a
fun fmax (d1: double, d2: double):<> double = "mac#atslib_fmax"
fun fmaxf (f1: float, f2: float):<> float = "mac#atslib_fmaxf"
fun fmaxl (ld1: ldouble, ld2: ldouble):<> ldouble = "mac#atslib_fmaxl"

fun{a:t@ype} fmintmp (x1: a, x2: a):<> a
fun fmin (d1: double, d2: double):<> double = "mac#atslib_fmin"
fun fminf (f1: float, f2: float):<> float = "mac#atslib_fminf"
fun fminl (ld1: ldouble, ld2: ldouble):<> ldouble = "mac#atslib_fminl"

fun{a:t@ype} fdimtmp (x1: a, x2: a):<> a
fun fdim (d1: double, d2: double):<> double = "mac#atslib_fdim"
fun fdimf (f1: float, f2: float):<> float = "mac#atslib_fdimf"
fun fdiml (ld1: ldouble, ld2: ldouble):<> ldouble = "mac#atslib_fdiml"
//
// HX: fma (x, y, z) = x * y + z
//
fun{a:t@ype} fmatmp (x1: a, x2: a, x3: a):<> a
fun fma (d1: double, d2: double, d3: double):<> double = "mac#atslib_fma"
fun fmaf (f1: float, f2: float, f3: float):<> float = "mac#atslib_fmaf"
fun fmal (ld1: ldouble, ld2: ldouble, ld3: ldouble):<> ldouble = "mac#atslib_fmal"

(* ****** ****** *)
//
// HX: already available in [prelude/SATS/float.sats]
//
fun{a:t@ype} sqrttmp (x: a):<> a
fun sqrt (d: double):<> double = "mac#atslib_sqrt"
fun sqrtf (f: float):<> float = "mac#atslib_sqrtf"
fun sqrtl (ld: ldouble):<> ldouble = "mac#atslib_sqrtl"

fun{a:t@ype} cbrttmp (x: a):<> a
fun cbrt (d: double):<> double = "mac#atslib_cbrt"
fun cbrtf (f: float):<> float = "mac#atslib_cbrtf"
fun cbrtl (ld: ldouble):<> ldouble = "mac#atslib_cbrtl"

fun{a:t@ype} powtmp (x1: a, x2: a):<> a
fun pow (d1: double, d2: double):<> double = "mac#atslib_pow"
fun powf (f1: float, f2: float):<> float = "mac#atslib_powf"
fun powl (ld1: ldouble, ld2: ldouble):<> ldouble = "mac#atslib_powl"

(* ****** ****** *)

fun{a:t@ype} exptmp (x: a):<> a
fun exp (d: double):<> double = "mac#atslib_exp"
fun expf (f: float):<> float = "mac#atslib_expf"
fun expl (ld: ldouble):<> ldouble = "mac#atslib_expl"

(* ****** ****** *)

fun{a:t@ype} logtmp (x: a):<> a
fun log (d: double):<> double = "mac#atslib_log"
fun logf (f: float):<> float = "mac#atslib_logf"
fun logl (ld: ldouble):<> ldouble = "mac#atslib_logl"

fun{a:t@ype} log10tmp (x: a):<> a
fun log10 (d: double):<> double = "mac#atslib_log10"
fun log10f (f: float):<> float = "mac#atslib_log10f"
fun log10l (ld: ldouble):<> ldouble = "mac#atslib_log10l"

(* ****** ****** *)

fun{a:t@ype} asintmp (x: a):<> a
fun asin (d: double):<> double = "mac#atslib_asin"
fun asinf (f: float):<> float = "mac#atslib_asinf"
fun asinl (ld: ldouble):<> ldouble = "mac#atslib_asinl"

fun{a:t@ype} acostmp (x: a):<> a
fun acos (d: double):<> double = "mac#atslib_acos"
fun acosf (f: float):<> float = "mac#atslib_acosf"
fun acosl (ld: ldouble):<> ldouble = "mac#atslib_acosl"

fun{a:t@ype} atantmp (x: a):<> a
fun atan (d: double):<> double = "mac#atslib_atan"
fun atanf (f: float):<> float = "mac#atslib_atanf"
fun atanl (ld: ldouble):<> ldouble = "mac#atslib_atanl"

fun{a:t@ype} atan2tmp (x1: a, x2: a):<> a
fun atan2 (d1: double, d2: double):<> double = "mac#atslib_atan2"
fun atan2f (f1: float, f2: float):<> float = "mac#atslib_atan2f"
fun atan2l (ld1: ldouble, ld2: ldouble):<> ldouble = "mac#atslib_atan2l"

(* ****** ****** *)

fun{a:t@ype} asinhtmp (x: a):<> a
fun asinh (d: double):<> double = "mac#atslib_asinh"
fun asinhf (f: float):<> float = "mac#atslib_asinhf"
fun asinhl (ld: ldouble):<> ldouble = "mac#atslib_asinhl"

fun{a:t@ype} acoshtmp (x: a):<> a
fun acosh (d: double):<> double = "mac#atslib_acosh"
fun acoshf (f: float):<> float = "mac#atslib_acoshf"
fun acoshl (ld: ldouble):<> ldouble = "mac#atslib_acoshl"

(* ****** ****** *)

fun{a:t@ype} sintmp (x: a):<> a
fun sin (d: double):<> double = "mac#atslib_sin"
fun sinf (f: float):<> float = "mac#atslib_sinf"
fun sinl (ld: ldouble):<> ldouble = "mac#atslib_sinl"

fun{a:t@ype} costmp (x: a):<> a
fun cos (d: double):<> double = "mac#atslib_cos"
fun cosf (f: float):<> float = "mac#atslib_cosf"
fun cosl (ld: ldouble):<> ldouble = "mac#atslib_cosl"

fun{a:t@ype} tantmp (x: a):<> a
fun tan (d: double):<> double = "mac#atslib_tan"
fun tanf (f: float):<> float = "mac#atslib_tanf"
fun tanl (ld: ldouble):<> ldouble = "mac#atslib_tanl"

(* ****** ****** *)

fun{a:t@ype} sinhtmp (x: a):<> a
fun sinh (d: double):<> double = "mac#atslib_sinh"
fun sinhf (f: float):<> float = "mac#atslib_sinhf"
fun sinhl (ld: ldouble):<> ldouble = "mac#atslib_sinhl"

fun{a:t@ype} coshtmp (x: a):<> a
fun cosh (d: double):<> double = "mac#atslib_cosh"
fun coshf (f: float):<> float = "mac#atslib_coshf"
fun coshl (ld: ldouble):<> ldouble = "mac#atslib_coshl"

fun{a:t@ype} tanhtmp (x: a):<> a
fun tanh (d: double):<> double = "mac#atslib_tanh"
fun tanhf (f: float):<> float = "mac#atslib_tanhf"
fun tanhl (ld: ldouble):<> ldouble = "mac#atslib_tanhl"

(* ****** ****** *)

(* end of [math.sats] *)
