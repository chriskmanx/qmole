(*
**
** An interface for ATS to interact with BLAS
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

staload C = "libc/SATS/complex.sats"
staload "contrib/clapack/SATS/f2c.sats"

(* ****** ****** *)

implement print_typ<real> () = print "real"
implement print_typ<doublereal> () = print "doublereal"
implement print_typ<complex> () = print "complex"
implement print_typ<doublecomplex> () = print "doublecomplex"

implement print_elt<real> (x) = print_float (float_of_real x)
implement print_elt<doublereal> (x) = print_double (double_of_doublereal x)
implement print_elt<complex> (x) = $C.print_ccmplx (ccmplx_of_complex x)
implement print_elt<doublecomplex> (x) = $C.print_zcmplx (zcmplx_of_doublecomplex x)

(* ****** ****** *)

implement of_int<real> (x) = (real_of_float (float_of_int x))
implement of_int<doublereal> (x) = doublereal_of_double (double_of_int x)
implement of_int<complex> (x) = complex_of_ccmplx ($C.ccmplx_of_int x)
implement of_int<doublecomplex> (x) = doublecomplex_of_zcmplx ($C.zcmplx_of_int x)

implement of_double<real> (x) =
  real_of_float (float_of_double x)
implement of_double<doublereal> (x) =
  doublereal_of_double (x)
implement of_double<complex> (x) = 
  complex_of_ccmplx ($C.ccmplx_of_float (float_of_double x))
implement of_double<doublecomplex> (x) =
  doublecomplex_of_zcmplx ($C.zcmplx_of_double x)

(* ****** ****** *)

implement abs<real,real> (x) = abs_real (x)
implement abs<doublereal,doublereal> (x) = abs_doublereal (x)
implement abs<complex,real> (x) = abs_complex (x)
implement abs<doublecomplex,doublereal> (x) = abs_doublecomplex (x)

(* ****** ****** *)

implement neg<real> (x) = neg_real (x)
implement neg<doublereal> (x) = neg_doublereal (x)
implement neg<complex> (x) = neg_complex (x)
implement neg<doublecomplex> (x) = neg_doublecomplex (x)

(* ****** ****** *)

implement add<real> (x1, x2) = add_real_real (x1, x2)
implement add<doublereal> (x1, x2) = add_doublereal_doublereal (x1, x2)
implement add<complex> (x1, x2) = add_complex_complex (x1, x2)
implement add<doublecomplex> (x1, x2) = add_doublecomplex_doublecomplex (x1, x2)

(* ****** ****** *)

implement sub<real> (x1, x2) = sub_real_real (x1, x2)
implement sub<doublereal> (x1, x2) = sub_doublereal_doublereal (x1, x2)
implement sub<complex> (x1, x2) = sub_complex_complex (x1, x2)
implement sub<doublecomplex> (x1, x2) = sub_doublecomplex_doublecomplex (x1, x2)

(* ****** ****** *)

implement mul<real> (x1, x2) = mul_real_real (x1, x2)
implement mul<doublereal> (x1, x2) = mul_doublereal_doublereal (x1, x2)
implement mul<complex> (x1, x2) = mul_complex_complex (x1, x2)
implement mul<doublecomplex> (x1, x2) = mul_doublecomplex_doublecomplex (x1, x2)

(* ****** ****** *)

implement div<real> (x1, x2) = div_real_real (x1, x2)
implement div<doublereal> (x1, x2) = div_doublereal_doublereal (x1, x2)
implement div<complex> (x1, x2) = div_complex_complex (x1, x2)
implement div<doublecomplex> (x1, x2) = div_doublecomplex_doublecomplex (x1, x2)

(* ****** ****** *)

implement lt<real> (x1, x2) = lt_real_real (x1, x2)
implement lt<doublereal> (x1, x2) = lt_doublereal_doublereal (x1, x2)

(* ****** ****** *)

implement lte<real> (x1, x2) = lte_real_real (x1, x2)
implement lte<doublereal> (x1, x2) = lte_doublereal_doublereal (x1, x2)

(* ****** ****** *)

implement gt<real> (x1, x2) = gt_real_real (x1, x2)
implement gt<doublereal> (x1, x2) = gt_doublereal_doublereal (x1, x2)

(* ****** ****** *)

implement gte<real> (x1, x2) = gte_real_real (x1, x2)
implement gte<doublereal> (x1, x2) = gte_doublereal_doublereal (x1, x2)

(* ****** ****** *)

implement eq<real> (x1, x2) = eq_real_real (x1, x2)
implement eq<doublereal> (x1, x2) = eq_doublereal_doublereal (x1, x2)
implement eq<complex> (x1, x2) = eq_complex_complex (x1, x2)
implement eq<doublecomplex> (x1, x2) = eq_doublecomplex_doublecomplex (x1, x2)

(* ****** ****** *)

implement neq<real> (x1, x2) = neq_real_real (x1, x2)
implement neq<doublereal> (x1, x2) = neq_doublereal_doublereal (x1, x2)
implement neq<complex> (x1, x2) = neq_complex_complex (x1, x2)
implement neq<doublecomplex> (x1, x2) = neq_doublecomplex_doublecomplex (x1, x2)

(* ****** ****** *)

implement to_integer<int> (x) =
  integer_of_lint (lint_of_int x)
// end of [to_integer<int>]

implement to_integer<real> (x) =
  integer_of_lint (lint_of_float (float_of_real x))
// end of [to_integer<real>]

implement to_integer<doublereal> (x) =
  integer_of_lint (lint_of_double (double_of_doublereal x))
// end of [to_integer<doublereal>]

implement to_integer<complex> (x) =
  integer_of_lint (lint_of_float ($C.crealf (ccmplx_of_complex x)))
// end of [to_integer<complex>]

implement to_integer<doublecomplex> (x) =
  integer_of_lint (lint_of_double ($C.creal (zcmplx_of_doublecomplex x)))
// end of [to_integer<doublecomplex>]

(* ****** ****** *)

(* end of [f2c.hats] *)
