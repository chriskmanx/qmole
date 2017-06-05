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
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
**
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

(*
**
** An interface for various common funtion on numbers
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

(* ****** ****** *)

// HX: this is originally done for the ATS/CBLAS package

(* ****** ****** *)

staload MATH = "libc/SATS/math.sats"

(* ****** ****** *)

staload CMPLX = "libc/SATS/complex.sats"
typedef ccmplx = $CMPLX.ccmplx
typedef zcmplx = $CMPLX.zcmplx

(* ****** ****** *)

staload "prelude/SATS/number.sats"

(* ****** ****** *)

implement print_typ<float> () = print "float"
implement print_typ<double> () = print "double"
implement print_typ<ccmplx> () = print "ccmplx"
implement print_typ<zcmplx> () = print "zcmplx"

(* ****** ****** *)

implement print_elt<float> (x) = print_float x
implement print_elt<double> (x) = print_double x
implement print_elt<ccmplx> (x) = $CMPLX.print_ccmplx x
implement print_elt<zcmplx> (x) = $CMPLX.print_zcmplx x

(* ****** ****** *)

implement of_int<float> (x) = float_of_int (x)
implement of_int<double> (x) = double_of_int (x)
implement of_int<ccmplx> (x) = $CMPLX.ccmplx_of_int (x)
implement of_int<zcmplx> (x) = $CMPLX.zcmplx_of_int (x)

(* ****** ****** *)

implement of_size<float> (x) = float_of_size (x)
implement of_size<double> (x) = double_of_size (x)

(* ****** ****** *)

implement of_double<float> (x) = float_of_double (x)
implement of_double<double> (x) = x
implement of_double<ccmplx> (x) = let
  val x = float_of_double x in $CMPLX.ccmplx_of_float (x)
end // end of [of_double]
implement of_double<zcmplx> (x) = $CMPLX.zcmplx_of_double (x)

(* ****** ****** *)

implement to_int<float> (x) = int_of_float (x)
implement to_int<double> (x) = int_of_double (x)

implement to_float<float> (x) = x
implement to_float<double> (x) = float_of_double (x)

implement to_double<float> (x) = double_of_float (x)
implement to_double<double> (x) = x

(* ****** ****** *)

implement abs<float,float> (x) = abs_float (x)
implement abs<double,double> (x) = abs_double (x)
implement abs<ccmplx,float> (x) = $CMPLX.abs_ccmplx (x)
implement abs<zcmplx,double> (x) = $CMPLX.abs_zcmplx (x)

(* ****** ****** *)

implement neg<float> (x) = neg_float (x)
implement neg<double> (x) = neg_double (x)
implement neg<ccmplx> (x) = $CMPLX.neg_ccmplx (x)
implement neg<zcmplx> (x) = $CMPLX.neg_zcmplx (x)

(* ****** ****** *)

implement add<float> (x1, x2) = add_float_float (x1, x2)
implement add<double> (x1, x2) = add_double_double (x1, x2)
implement add<ccmplx> (x1, x2) = $CMPLX.add_ccmplx_ccmplx (x1, x2)
implement add<zcmplx> (x1, x2) = $CMPLX.add_zcmplx_zcmplx (x1, x2)

implement sub<float> (x1, x2) = sub_float_float (x1, x2)
implement sub<double> (x1, x2) = sub_double_double (x1, x2)
implement sub<ccmplx> (x1, x2) = $CMPLX.sub_ccmplx_ccmplx (x1, x2)
implement sub<zcmplx> (x1, x2) = $CMPLX.sub_zcmplx_zcmplx (x1, x2)

implement mul<float> (x1, x2) = mul_float_float (x1, x2)
implement mul<double> (x1, x2) = mul_double_double (x1, x2)
implement mul<ccmplx> (x1, x2) = $CMPLX.mul_ccmplx_ccmplx (x1, x2)
implement mul<zcmplx> (x1, x2) = $CMPLX.mul_zcmplx_zcmplx (x1, x2)

implement div<float> (x1, x2) = div_float_float (x1, x2)
implement div<double> (x1, x2) = div_double_double (x1, x2)
implement div<ccmplx> (x1, x2) = $CMPLX.div_ccmplx_ccmplx (x1, x2)
implement div<zcmplx> (x1, x2) = $CMPLX.div_zcmplx_zcmplx (x1, x2)

(* ****** ****** *)

implement pow<float> (x, y) = $MATH.powf (x, y)
implement pow<double> (x, y) = $MATH.pow (x, y)
implement pow<ccmplx> (x, y) = $CMPLX.cpowf (x, y)
implement pow<zcmplx> (x, y) = $CMPLX.cpow (x, y)

(* ****** ****** *)

implement ceil<float> (x) = $MATH.ceilf (x)
implement ceil<double> (x) = $MATH.ceil (x)

implement floor<float> (x) = $MATH.floorf (x)
implement floor<double> (x) = $MATH.floor (x)

(* ****** ****** *)

implement sqrt<float> (a) = $MATH.sqrtf (a)
implement sqrt<double> (a) = $MATH.sqrt (a)
implement sqrt<ccmplx> (a) = $CMPLX.sqrt_ccmplx (a)
implement sqrt<zcmplx> (a) = $CMPLX.sqrt_zcmplx (a)

(* ****** ****** *)

implement scal<float,float> (x1, x2) = mul_float_float (x1, x2)
implement scal<float,ccmplx> (x1, x2) = let
  val x2_r = $CMPLX.ccmplx_real (x2) and x2_i = $CMPLX.ccmplx_imag (x2) in
  $CMPLX.ccmplx_make_cart (mul_float_float (x1, x2_r), mul_float_float (x1, x2_i))
end // end of ...
implement scal<ccmplx,ccmplx> (x1, x2) = $CMPLX.mul_ccmplx_ccmplx (x1, x2)

implement scal<double,double> (x1, x2) = mul_double_double (x1, x2)
implement scal<double,zcmplx> (x1, x2) = let
  val x2_r = $CMPLX.zcmplx_real (x2) and x2_i = $CMPLX.zcmplx_imag (x2) in
  $CMPLX.zcmplx_make_cart (mul_double_double (x1, x2_r), mul_double_double (x1, x2_i))
end // end of ...
implement scal<zcmplx,zcmplx> (x1, x2) = $CMPLX.mul_zcmplx_zcmplx (x1, x2)

(* ****** ****** *)

implement lt<float> (x1, x2) = x1 < x2
implement lt<double> (x1, x2) = x1 < x2

implement lte<float> (x1, x2) = x1 <= x2
implement lte<double> (x1, x2) = x1 <= x2

implement gt<float> (x1, x2) = x1 > x2
implement gt<double> (x1, x2) = x1 > x2

implement gte<float> (x1, x2) = x1 >= x2
implement gte<double> (x1, x2) = x1 >= x2

(* ****** ****** *)

implement signof<float> (x) =
  compare_float_float (x, (float_of)0.0)
implement signof<double> (x) = compare_double_double (x, 0.0)

implement compare<float> (x1, x2) = compare_float_float (x1, x2)
implement compare<double> (x1, x2) = compare_double_double (x1, x2)

(* ****** ****** *)

implement{a} min (x, y) = if lte<a> (x,y) then x else y
implement{a} max (x, y) = if gte<a> (x,y) then x else y

(* ****** ****** *)

implement eq<float> (x1, x2) = eq_float_float (x1, x2)
implement eq<double> (x1, x2) = eq_double_double (x1, x2)
implement eq<ccmplx> (x1, x2) = $CMPLX.eq_ccmplx_ccmplx (x1, x2)
implement eq<zcmplx> (x1, x2) = $CMPLX.eq_zcmplx_zcmplx (x1, x2)

implement neq<float> (x1, x2) = neq_float_float (x1, x2) 
implement neq<double> (x1, x2) = neq_double_double (x1, x2)
implement neq<ccmplx> (x1, x2) = $CMPLX.neq_ccmplx_ccmplx (x1, x2)
implement neq<zcmplx> (x1, x2) = $CMPLX.neq_zcmplx_zcmplx (x1, x2)

(* ****** ****** *)

implement cmplx_make_cart<float,ccmplx>
  (x1, x2) = $CMPLX.ccmplx_make_cart (x1, x2)
implement cmplx_make_cart<double,zcmplx>
  (x1, x2) = $CMPLX.zcmplx_make_cart (x1, x2)

(* ****** ****** *)

implement creal<float,ccmplx> (x) = $CMPLX.crealf (x)
implement creal<double,zcmplx> (x) = $CMPLX.creal (x)

implement cimag<float,ccmplx> (x) = $CMPLX.cimagf (x)
implement cimag<double,zcmplx> (x) = $CMPLX.cimag (x)

(* ****** ****** *)

implement conj<ccmplx> (x) = $CMPLX.conj_ccmplx (x)
implement conj<zcmplx> (x) = $CMPLX.conj_zcmplx (x)

(* ****** ****** *)

implement sin<float> (x) = $MATH.sinf (x)
implement sin<double> (x) = $MATH.sin (x)
implement sin<ccmplx> (x) = $CMPLX.sin_ccmplx (x)
implement sin<zcmplx> (x) = $CMPLX.sin_zcmplx (x)

implement cos<float> (x) = $MATH.cosf (x)
implement cos<double> (x) = $MATH.cos (x)
implement cos<ccmplx> (x) = $CMPLX.cos_ccmplx (x)
implement cos<zcmplx> (x) = $CMPLX.cos_zcmplx (x)

implement tan<float> (x) = $MATH.tanf (x)
implement tan<double> (x) = $MATH.tan (x)
implement tan<ccmplx> (x) = $CMPLX.tan_ccmplx (x)
implement tan<zcmplx> (x) = $CMPLX.tan_zcmplx (x)

(* ****** ****** *)

implement asin<float> (x) = $MATH.asinf (x)
implement asin<double> (x) = $MATH.asin (x)
implement asin<ccmplx> (x) = $CMPLX.casinf (x)
implement asin<zcmplx> (x) = $CMPLX.casin (x)

implement acos<float> (x) = $MATH.acosf (x)
implement acos<double> (x) = $MATH.acos (x)
implement acos<ccmplx> (x) = $CMPLX.cacosf (x)
implement acos<zcmplx> (x) = $CMPLX.cacos (x)

implement atan<float> (x) = $MATH.atanf (x)
implement atan<double> (x) = $MATH.atan (x)
implement atan<ccmplx> (x) = $CMPLX.catanf (x)
implement atan<zcmplx> (x) = $CMPLX.catan (x)

implement atan2<float> (x, y) = $MATH.atan2f (x, y)
implement atan2<double> (x, y) = $MATH.atan2 (x, y)

(* ****** ****** *)

(* end of [number.hats] *)
