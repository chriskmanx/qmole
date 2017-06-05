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
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

staload "libc/SATS/math.sats"

(* ****** ****** *)
//
// HX: fpclassify, isfinite and isnormal are macros
//
implement fpclassify<float> (x) = fpclassify_float (x)
implement fpclassify<double> (x) = fpclassify_double (x)
implement fpclassify<ldouble> (x) = fpclassify_ldouble (x)

implement isfinite<float> (x) = isfinite_float (x)
implement isfinite<double> (x) = isfinite_double (x)
implement isfinite<ldouble> (x) = isfinite_ldouble (x)

implement isnormal<float> (x) = isnormal_float (x)
implement isnormal<double> (x) = isnormal_double (x)
implement isnormal<ldouble> (x) = isnormal_ldouble (x)

// HX: BSD function
implement isinftmp<float> (x) = isinff (x)
implement isinftmp<double> (x) = isinf (x)
implement isinftmp<ldouble> (x) = isinfl (x)

// HX: BSD function
implement isnantmp<float> (x) = isnanf (x)
implement isnantmp<double> (x) = isnan (x)
implement isnantmp<ldouble> (x) = isnanl (x)

(* ****** ****** *)

implement ceiltmp<double> (x) = ceil (x)
implement ceiltmp<float> (x) = ceilf (x)
implement ceiltmp<ldouble> (x) = ceill (x)

implement floortmp<double> (x) = floor (x)
implement floortmp<float> (x) = floorf (x)
implement floortmp<ldouble> (x) = floorl (x)

(* ****** ****** *)

implement roundtmp<double> (x) = round (x)
implement roundtmp<float> (x) = roundf (x)
implement roundtmp<ldouble> (x) = roundl (x)

implement trunctmp<double> (x) = trunc (x)
implement trunctmp<float> (x) = truncf (x)
implement trunctmp<ldouble> (x) = truncl (x)

(* ****** ****** *)

implement fmodtmp<double> (x1, x2) = fmod (x1, x2)
implement fmodtmp<float> (x1, x2) = fmodf (x1, x2)
implement fmodtmp<ldouble> (x1, x2) = fmodl (x1, x2)

(* ****** ****** *)

implement fmaxtmp<double> (x1, x2) = fmax (x1, x2)
implement fmaxtmp<float> (x1, x2) = fmaxf (x1, x2)
implement fmaxtmp<ldouble> (x1, x2) = fmaxl (x1, x2)

implement fmintmp<double> (x1, x2) = fmin (x1, x2)
implement fmintmp<float> (x1, x2) = fminf (x1, x2)
implement fmintmp<ldouble> (x1, x2) = fminl (x1, x2)

implement fdimtmp<double> (x1, x2) = fdim (x1, x2)
implement fdimtmp<float> (x1, x2) = fdimf (x1, x2)
implement fdimtmp<ldouble> (x1, x2) = fdiml (x1, x2)

implement fmatmp<double> (x1, x2, x3) = fma (x1, x2, x3)
implement fmatmp<float> (x1, x2, x3) = fmaf (x1, x2, x3)
implement fmatmp<ldouble> (x1, x2, x3) = fmal (x1, x2, x3)

(* ****** ****** *)

implement sqrttmp<double> (x) = sqrt (x)
implement sqrttmp<float> (x) = sqrtf (x)
implement sqrttmp<ldouble> (x) = sqrtl (x)

implement cbrttmp<double> (x) = cbrt (x)
implement cbrttmp<float> (x) = cbrtf (x)
implement cbrttmp<ldouble> (x) = cbrtl (x)

implement powtmp<double> (x1, x2) = pow (x1, x2)
implement powtmp<float> (x1, x2) = powf (x1, x2)
implement powtmp<ldouble> (x1, x2) = powl (x1, x2)

(* ****** ****** *)

implement exptmp<double> (x) = exp (x)
implement exptmp<float> (x) = expf (x)
implement exptmp<ldouble> (x) = expl (x)

(* ****** ****** *)

implement logtmp<double> (x) = log (x)
implement logtmp<float> (x) = logf (x)
implement logtmp<ldouble> (x) = logl (x)

implement log10tmp<double> (x) = log10 (x)
implement log10tmp<float> (x) = log10f (x)
implement log10tmp<ldouble> (x) = log10l (x)

(* ****** ****** *)

implement asintmp<double> (x) = asin (x)
implement asintmp<float> (x) = asinf (x)
implement asintmp<ldouble> (x) = asinl (x)

implement acostmp<double> (x) = acos (x)
implement acostmp<float> (x) = acosf (x)
implement acostmp<ldouble> (x) = acosl (x)

implement atantmp<double> (x) = atan (x)
implement atantmp<float> (x) = atanf (x)
implement atantmp<ldouble> (x) = atanl (x)

implement atan2tmp<float> (x1, x2) = atan2f (x1, x2)
implement atan2tmp<double> (x1, x2) = atan2 (x1, x2)
implement atan2tmp<ldouble> (x1, x2) = atan2l (x1, x2)

(* ****** ****** *)

implement asinhtmp<double> (x) = asinh (x)
implement asinhtmp<float> (x) = asinhf (x)
implement asinhtmp<ldouble> (x) = asinhl (x)

implement acoshtmp<double> (x) = acosh (x)
implement acoshtmp<float> (x) = acoshf (x)
implement acoshtmp<ldouble> (x) = acoshl (x)

(* ****** ****** *)

implement sintmp<double> (x) = sin (x)
implement sintmp<float> (x) = sinf (x)
implement sintmp<ldouble> (x) = sinl (x)

implement costmp<double> (x) = cos (x)
implement costmp<float> (x) = cosf (x)
implement costmp<ldouble> (x) = cosl (x)

implement tantmp<double> (x) = tan (x)
implement tantmp<float> (x) = tanf (x)
implement tantmp<ldouble> (x) = tanl (x)

(* ****** ****** *)

implement sinhtmp<double> (x) = sinh (x)
implement sinhtmp<float> (x) = sinhf (x)
implement sinhtmp<ldouble> (x) = sinhl (x)

implement coshtmp<double> (x) = cosh (x)
implement coshtmp<float> (x) = coshf (x)
implement coshtmp<ldouble> (x) = coshl (x)

implement tanhtmp<double> (x) = tanh (x)
implement tanhtmp<float> (x) = tanhf (x)
implement tanhtmp<ldouble> (x) = tanhl (x)

(* ****** ****** *)

(* end of [math.hats] *)
