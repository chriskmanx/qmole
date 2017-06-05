//
// some testing code for the ATS interface to BLAS
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

// This example demonstrates how to compute A * A^T

(* ****** ****** *)

local #include "contrib/cblas/HATS/cblas.hats" in (*empty*) end

(* ****** ****** *)

staload "libc/SATS/complex.sats"

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"
staload _(*anonymous*) = "libats/DATS/genarrays.dats"
staload "libats/SATS/fmatrix.sats"
staload _(*anonymous*) = "libats/DATS/fmatrix.dats"

(* ****** ****** *)

staload "contrib/cblas/SATS/cblas.sats"
staload _(*anonymous*) = "contrib/cblas/DATS/cblas.dats"

(* ****** ****** *)

extern fun{a:t@ype} of_double (x: double):<> a
implement of_double<float> (x: double) = float_of_double (x)
implement of_double<double> (x: double) = x
implement of_double<ccmplx> (x) = ccmplx_of_float (float_of_double x)
implement of_double<zcmplx> (x) = zcmplx_of_double (x)

(* ****** ****** *)

extern fun{a:t@ype} print_elt (x: a): void
implement print_elt<float> (x) = print x
implement print_elt<double> (x) = print x
implement print_elt<ccmplx> (x) = print x
implement print_elt<zcmplx> (x) = print x

(* ****** ****** *)

//
// C <- A * A^T
//

fun{t:t@ype} test {v:view}
  {ord:order} {ma,na:nat} {lda,ldc:pos} {la:addr} (
    pf: !v
  , pfa : GEMAT_v (t, ma, na, ord, lda, la) <= v
  | Order: CBLAS_ORDER_t ord
  , M: int ma, N: int na  
  , pA: ptr la, lda: int lda
  , C: &GEMAT (t, ma, ma, ord, ldc), ldc: int ldc
  ) : void = cblas_gemm__main<t> (
    pf
  , TRANDIM_N
  , pfa
  , TRANDIM_T
  , pfa
  | Order
  , CblasNoTrans, CblasTrans
  , M, M, N
  , of_double 1.0 (* alpha *)
  , pA, lda, pA, lda 
  , of_double 0.0 (* beta *)
  , C, ldc 
  )
// end of [test]

(* ****** ****** *)

fn{a:t@ype} print_fmatrix {m,n:nat} (
    M: &fmatrix (a, m, n), m: size_t m, n: size_t n
  ) : void =
  fmatrix_ptr_iforeach_fun<a>
    (M, pr, ORDERrow, m, n) where {
    val pr = lam (
      i: size_t, j: size_t, x: &a
    ) : void =<fun> $effmask_all let
      val () = if j > 0 then print ", "
      val () = if (i > 0 andalso j = 0) then print "\n" 
      val () = print_elt<a> (x)
    in
      // empty
    end // end of [val]
  } // end of [fmartrix_ptr_iforeach_fun]
// end of [print_fmatrix]

(* ****** ****** *)

typedef elt = float

(* ****** ****** *)

val () = () where {
  val [l:addr]
    (pfA_gc, pfA_arr | pA_arr) =
    array_ptr_alloc_tsz {elt} (6, sizeof<elt>)
  // end of [val]
  var fone = of_double (1.0)
  val () = array_ptr_initialize_elt_tsz
    {elt} (!pA_arr, 6, fone, sizeof<elt>)
  // end of [val]
  prval pf_3x2 = mul_make {3,2} ()
  prval pfA_fmat = fmatrix_v_of_array_v (pf_3x2, pfA_arr)
//
  val () = print "A =\n"
  val () = print_fmatrix<elt> (!pA_arr, 3, 2)
  val () = print_newline ()
//
  prval (pfA_gmat, fpfA_fmat) = GEMAT_v_of_fmatrix_v (pfA_fmat)
  var !pC with pfC_arr = @[elt][9](of_double 0.0)
  prval pf_3x3 = mul_make {3,3} ()
  prval pfC_fmat = fmatrix_v_of_array_v (pf_3x3, pfC_arr)
  prval (pfC_gmat, fpfC_fmat) = GEMAT_v_of_fmatrix_v (pfC_fmat)
//
  viewdef V = GEMAT_v (elt, 3, 2, col, 3, l)
  val () = test<elt> {V}
    (pfA_gmat, vsubr_refl {V} () | CblasColMajor, 3, 2, pA_arr, 3, !pC, 3)
  (* end of [val] *)
//
  prval pfA_fmat = fpfA_fmat (pfA_gmat)
  prval (pf2_3x2, pfA_arr) = array_v_of_fmatrix_v (pfA_fmat)
  prval () = mul_isfun (pf_3x2, pf2_3x2)
  val () = array_ptr_free (pfA_gc, pfA_arr | pA_arr)
//
  prval () = pfC_fmat := fpfC_fmat (pfC_gmat)
//
  val () = print "C =\n"
  val () = print_fmatrix<elt> (!pC, 3, 3)
  val () = print_newline ()
//
  prval (pf2_3x3, pfC2_arr) = array_v_of_fmatrix_v (pfC_fmat)
  prval () = mul_isfun (pf2_3x3, pf_3x3)
  prval () = pfC_arr := pfC2_arr
} // end of [main]

(* ****** ****** *)

implement main () = ()

(* ****** ****** *)

(* end of [gemm2_test.dats] *)
