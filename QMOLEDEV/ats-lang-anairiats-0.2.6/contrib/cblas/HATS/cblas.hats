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

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

#define ATS_CBLAS_DEBUG 1 // for controlling some debugging code

(* ****** ****** *)

staload C = "libc/SATS/complex.sats"
typedef ccmplx = $C.ccmplx
typedef zcmplx = $C.zcmplx

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"
staload "contrib/cblas/SATS/cblas.sats"

(* ****** ****** *)

//
// BLAS level 1
//

(* ****** ****** *)

//
// cblas_rotg: S, D, C, Z
//

implement cblas_rotg<float>
  (a, b, c, s) = cblas_srotg (a, b, c, s)
// end of [cblas_rotg<float>]

implement cblas_rotg<double>
  (a, b, c, s) = cblas_drotg (a, b, c, s)
// end of [cblas_rotg<double>]


// must test first
#if (0) // if it is to be uncommented

// ATLAS extension

implement cblas_rotg<ccmplx>
  (a, b, c, s) = cblas_crotg (a, b, c, s)
// end of [cblas_rotg<ccmplx>]

implement cblas_rotg<zcmplx>
  (a, b, c, s) = cblas_zrotg (a, b, c, s)
// end of [cblas_rotg<zcmplx>]

#endif // end of [#if 0]

(* ****** ****** *)

//
// cblas_rot: S, D, CS, ZD
//

implement cblas_rot<float>
  (N, X, incX, Y, incY, c, s) = cblas_srot (N, X, incX, Y, incY, c, s)
// end of [cblas_rot<float>]

implement cblas_rot<double>
  (N, X, incX, Y, incY, c, s) = cblas_drot (N, X, incX, Y, incY, c, s)
// end of [cblas_rot<double>]

(* ****** ****** *)

//
// cblas_rotmg: S, D
//

implement cblas_rotmg<float>
  (d1, d2, b1, b2, P) = cblas_srotmg (d1, d2, b1, b2, P)
// end of [cblas_rotmg<float>]

implement cblas_rotmg<double>
  (d1, d2, b1, b2, P) = cblas_drotmg (d1, d2, b1, b2, P)
// end of [cblas_rotmg<double>]

(* ****** ****** *)

//
// cblas_rotm: S, D
//

implement cblas_rotm<float>
  (N, X, incX, Y, incY, P) = cblas_srotm (N, X, incX, Y, incY, P)
// end of [cblas_rotm<float>]

implement cblas_rotm<double>
  (N, X, incX, Y, incY, P) = cblas_drotm (N, X, incX, Y, incY, P)
// end of [cblas_rotm<double>]

(* ****** ****** *)

//
// cblas_dot: S, D
//

implement cblas_dot<float>
   (N, X, incX, Y, incY) = cblas_sdot (N, X, incX, Y, incY)
// end of [cblas_dot<float>]

implement cblas_dot<double>
   (N, X, incX, Y, incY) = cblas_ddot (N, X, incX, Y, incY)
// end of [cblas_dot<double>]

(* ****** ****** *)

//
// cblas_dotu_sub: C, Z
// unconjugated dot product (call-by-reference on the return value)
//

implement cblas_dotu_sub<ccmplx>
   (N, X, incX, Y, incY, dotu) = cblas_cdotu_sub (N, X, incX, Y, incY, dotu)
// end of [cblas_dotu_sub<ccmplx>]

implement cblas_dotu_sub<zcmplx>
   (N, X, incX, Y, incY, dotu) = cblas_zdotu_sub (N, X, incX, Y, incY, dotu)
// end of [cblas_dotu_sub<zcmplx>]

(* ****** ****** *)

//
// cblas_dotc_sub: C, Z
// conjugated dot product (call-by-reference on the return value)
//

implement cblas_dotc_sub<ccmplx>
   (N, X, incX, Y, incY, dotu) = cblas_cdotc_sub (N, X, incX, Y, incY, dotu)
// end of [cblas_dotc_sub<ccmplx>]

implement cblas_dotc_sub<zcmplx>
   (N, X, incX, Y, incY, dotu) = cblas_zdotc_sub (N, X, incX, Y, incY, dotu)
// end of [cblas_dotc_sub<zcmplx>]

(* ****** ****** *)

//
// cblas_dotu: S, D, C, Z
//

implement cblas_dotu<float>
   (N, X, incX, Y, incY) = cblas_sdot (N, X, incX, Y, incY)
// end of [cblas_dot<float>]

implement cblas_dotu<double>
   (N, X, incX, Y, incY) = cblas_ddot (N, X, incX, Y, incY)
// end of [cblas_dot<double>]

implement cblas_dotu<ccmplx>
   (N, X, incX, Y, incY) = let
  var dotu : ccmplx // uninitialized
  val () = cblas_cdotu_sub (N, X, incX, Y, incY, dotu)
in
  dotu
end // of [cblas_dotu<ccmplx>]

implement cblas_dotu<zcmplx>
   (N, X, incX, Y, incY) = let
  var dotu : zcmplx // uninitialized
  val () = cblas_zdotu_sub (N, X, incX, Y, incY, dotu)
in
  dotu
end // of [cblas_dotu<zcmplx>]

(* ****** ****** *)

//
// cblas_dotc: S, D, C, Z
//

implement cblas_dotc<float>
   (N, X, incX, Y, incY) = cblas_sdot (N, X, incX, Y, incY)
// end of [cblas_dotc<float>]

implement cblas_dotc<double>
   (N, X, incX, Y, incY) = cblas_ddot (N, X, incX, Y, incY)
// end of [cblas_dotc<double>]

implement cblas_dotc<ccmplx>
   (N, X, incX, Y, incY) = let
  var dotc : ccmplx // uninitialized
  val () = cblas_cdotc_sub (N, X, incX, Y, incY, dotc)
in
  dotc
end // of [cblas_dotc<ccmplx>]

implement cblas_dotc<zcmplx>
   (N, X, incX, Y, incY) = let
  var dotc : zcmplx // uninitialized
  val () = cblas_zdotc_sub (N, X, incX, Y, incY, dotc)
in
  dotc
end // of [cblas_dotc<zcmplx>]

(* ****** ****** *)

//
// cblas_nrm2: S, D, SC, DZ
//

implement cblas_nrm2<float,float> (N, X, incX) = cblas_snrm2 (N, X, incX)
implement cblas_nrm2<double,double> (N, X, incX) = cblas_dnrm2 (N, X, incX)
implement cblas_nrm2<float,ccmplx> (N, X, incX) = cblas_scnrm2 (N, X, incX)
implement cblas_nrm2<double,zcmplx> (N, X, incX) = cblas_dznrm2 (N, X, incX)

(* ****** ****** *)

//
// cblas_asum: S, D, SC, DZ
//

implement cblas_asum<float,float>   (N, X, incX) = cblas_sasum  (N, X, incX)
implement cblas_asum<double,double> (N, X, incX) = cblas_dasum  (N, X, incX)
implement cblas_asum<ccmplx,float>  (N, X, incX) = cblas_scasum (N, X, incX)
implement cblas_asum<zcmplx,double> (N, X, incX) = cblas_dzasum (N, X, incX)

(* ****** ****** *)

//
// cblas_iamax: S, D, C, Z
//

implement cblas_iamax<float>  (N, X, incX) = cblas_isamax (N, X, incX)
implement cblas_iamax<double> (N, X, incX) = cblas_idamax (N, X, incX)
implement cblas_iamax<ccmplx> (N, X, incX) = cblas_icamax (N, X, incX)
implement cblas_iamax<zcmplx> (N, X, incX) = cblas_izamax (N, X, incX)

(* ****** ****** *)

//
// cblas_swap: S, D, C, Z
//

implement cblas_swap<float>
  (N, X, incX, Y, incY) = cblas_sswap (N, X, incX, Y, incY)
// end of [cblas_swap<float>]

implement cblas_swap<double>
  (N, X, incX, Y, incY) = cblas_dswap (N, X, incX, Y, incY)
// end of [cblas_swap<double>]

implement cblas_swap<ccmplx>
  (N, X, incX, Y, incY) = cblas_cswap (N, X, incX, Y, incY)
// end of [cblas_swap<ccmplx>]

implement cblas_swap<zcmplx>
  (N, X, incX, Y, incY) = cblas_zswap (N, X, incX, Y, incY)
// end of [cblas_swap<zcmplx>]

(* ****** ****** *)

//
// cblas_axpy: S, D, C, Z
//

implement cblas_axpy<float>
  (N, alpha, X, incX, Y, incY) = cblas_saxpy (N, alpha, X, incX, Y, incY)
// end of [cblas_axpy<float>]

implement cblas_axpy<double>
  (N, alpha, X, incX, Y, incY) = cblas_daxpy (N, alpha, X, incX, Y, incY)
// end of [cblas_axpy<double>]

implement cblas_axpy<ccmplx>
  (N, alpha, X, incX, Y, incY) = let
  var alpha: ccmplx = alpha in cblas_caxpy (N, alpha, X, incX, Y, incY)
end // end of [cblas_axpy<ccmplx>]

implement cblas_axpy<zcmplx>
  (N, alpha, X, incX, Y, incY) = let
  var alpha: zcmplx = alpha in cblas_zaxpy (N, alpha, X, incX, Y, incY)
end // end of [cblas_axpy<zcmplx>]

(* ****** ****** *)

//
// cblas_copy: S, D, C, Z
//

implement cblas_copy<float>
  (N, X, incX, Y, incY) = cblas_scopy (N, X, incX, Y, incY)
// end of [cblas_copy<float>]

implement cblas_copy<double>
  (N, X, incX, Y, incY) = cblas_dcopy (N, X, incX, Y, incY)
// end of [cblas_copy<double>]

implement cblas_copy<ccmplx>
  (N, X, incX, Y, incY) = cblas_ccopy (N, X, incX, Y, incY)
// end of [cblas_copy<ccmplx>]

implement cblas_copy<zcmplx>
  (N, X, incX, Y, incY) = cblas_zcopy (N, X, incX, Y, incY)
// end of [cblas_copy<zcmplx>]

(* ****** ****** *)

//
// cblas_scal: S, D, C, D, CS, ZD
//

implement cblas_scal<float,float>
  (N, alpha, X, incX) = cblas_sscal (N, alpha, X, incX)
// end of [cblas_scal<float,float>]

implement cblas_scal<double,double>
  (N, alpha, X, incX) = cblas_dscal (N, alpha, X, incX)
// end of [cblas_scal<double,double>]

implement cblas_scal<ccmplx,ccmplx> (N, alpha, X, incX) = let
  var alpha: ccmplx = alpha in cblas_cscal (N, alpha, X, incX)
end // end of [cblas_scal<ccmplx,ccmplx>]

implement cblas_scal<zcmplx,zcmplx> (N, alpha, X, incX) = let
  var alpha: zcmplx = alpha in cblas_zscal (N, alpha, X, incX)
end // end of [cblas_scal<zcmplx,zcmplx>]

implement cblas_scal<ccmplx,float>
  (N, alpha, X, incX) = cblas_csscal (N, alpha, X, incX)
// end of [cblas_scal<ccmplx,float>]

implement cblas_scal<zcmplx,double>
  (N, alpha, X, incX) = cblas_zdscal (N, alpha, X, incX)
// end of [cblas_scal<zcmplx,double>]

(* ****** ****** *)

//
// BLAS level 2
//

(* ****** ****** *)

//
// cblas_gemv: S, D, C, Z
//

implement cblas_gemv<float>
  (pfa | Order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY) =
  cblas_sgemv (
    pfa | Order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY
  ) // end of [cblas_sgemv]
// end of [cblas_gemv<float>]

implement cblas_gemv<double>
  (pfa | Order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY) =
  cblas_dgemv (
    pfa | Order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY
  ) // end of [cblas_dgemv]
// end of [cblas_gemv<double>]

implement cblas_gemv<ccmplx> (
    pfa
  | Order, TransA, M, N
  , alpha, A, lda, X, incX, beta, Y, incY
  ) = let
  var alpha: ccmplx = alpha and beta: ccmplx = beta
in
  cblas_cgemv (
    pfa | Order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY
  ) // end of [cblas_cgemv]
end // end of [cblas_gemv<ccmplx>]

implement cblas_gemv<zcmplx> (
    pfa
  | Order, TransA, M, N
  , alpha, A, lda, X, incX, beta, Y, incY
  ) = let
  var alpha: zcmplx = alpha and beta: zcmplx = beta
in
  cblas_zgemv (
    pfa | Order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY
  ) // end of [cblas_zgemv]
end // end of [cblas_gemv<zcmplx>]

(* ****** ****** *)

implement cblas_gbmv<float> (
    pfa
  | Order
  , TransA
  , M, N
  , KL, KU
  , alpha
  , A, lda
  , X, incX
  , beta
  , Y, incY
  ) = cblas_sgbmv (
    pfa
  | Order, TransA
  , M, N
  , KL, KU
  , alpha
  , A, lda
  , X, incX
  , beta
  , Y, incY
  ) // end of [cblas_sgbmv]
// end of [cblas_gbmv<float>]

implement cblas_gbmv<double> (
    pfa
  | Order
  , TransA
  , M, N
  , KL, KU
  , alpha
  , A, lda
  , X, incX
  , beta
  , Y, incY
  ) = cblas_dgbmv (
    pfa
  | Order, TransA
  , M, N
  , KL, KU
  , alpha
  , A, lda
  , X, incX
  , beta
  , Y, incY
  ) // end of [cblas_dgbmv]
// end of [cblas_gbmv<double>]

implement cblas_gbmv<ccmplx> (
    pfa
  | Order
  , TransA
  , M, N
  , KL, KU
  , alpha
  , A, lda
  , X, incX
  , beta
  , Y, incY
  ) = let
  var alpha: ccmplx = alpha and beta: ccmplx = beta
in
  cblas_cgbmv (
    pfa
  | Order, TransA
  , M, N
  , KL, KU
  , alpha
  , A, lda
  , X, incX
  , beta
  , Y, incY
  ) // end of [cblas_cgbmv]
end // end of [cblas_gbmv<ccmplx>]

implement cblas_gbmv<zcmplx> (
    pfa
  | Order
  , TransA
  , M, N
  , KL, KU
  , alpha
  , A, lda
  , X, incX
  , beta
  , Y, incY
  ) = let
  var alpha: zcmplx = alpha and beta: zcmplx = beta
in
  cblas_zgbmv (
    pfa
  | Order, TransA
  , M, N
  , KL, KU
  , alpha
  , A, lda
  , X, incX
  , beta
  , Y, incY
  ) // end of [cblas_zgbmv]
end // end of [cblas_gbmv<zcmplx>]

(* ****** ****** *)

//
// cblas_trmv: S, D, C, Z
//

implement cblas_trmv<float>
  (Order, Uplo, TransA, Diag, N, A, lda, X, incX) =
  cblas_strmv (Order, Uplo, TransA, Diag, N, A, lda, X, incX)
// end of [cblas_trmv<float>]

implement cblas_trmv<double>
  (Order, Uplo, TransA, Diag, N, A, lda, X, incX) =
  cblas_dtrmv (Order, Uplo, TransA, Diag, N, A, lda, X, incX)
// end of [cblas_trmv<double>]

implement cblas_trmv<ccmplx>
  (Order, Uplo, TransA, Diag, N, A, lda, X, incX) =
  cblas_ctrmv (Order, Uplo, TransA, Diag, N, A, lda, X, incX)
// end of [cblas_trmv<ccmplx>]

implement cblas_trmv<zcmplx>
  (Order, Uplo, TransA, Diag, N, A, lda, X, incX) =
  cblas_ztrmv (Order, Uplo, TransA, Diag, N, A, lda, X, incX)
// end of [cblas_trmv<zcmplx>]

(* ****** ****** *)

//
// cblas_tbmv: S, D, C, Z
//

implement cblas_tbmv<float>
  (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX) =
  cblas_stbmv (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX)
// end of [cblas_tbmv<float>]

implement cblas_tbmv<double>
  (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX) =
  cblas_dtbmv (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX)
// end of [cblas_tbmv<double>]

implement cblas_tbmv<ccmplx>
  (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX) =
  cblas_ctbmv (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX)
// end of [cblas_tbmv<ccmplx>]

implement cblas_tbmv<zcmplx>
  (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX) =
  cblas_ztbmv (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX)
// end of [cblas_tbmv<zcmplx>]

(* ****** ****** *)

//
// cblas_tpmv: S, D, C, Z
//

implement cblas_tpmv<float>
  (Order, Uplo, TransA, Diag, N, Ap, X, incX) =
  cblas_stpmv (Order, Uplo, TransA, Diag, N, Ap, X, incX)
// end of [cblas_tpmv<float>]

implement cblas_tpmv<double>
  (Order, Uplo, TransA, Diag, N, Ap, X, incX) =
  cblas_dtpmv (Order, Uplo, TransA, Diag, N, Ap, X, incX)
// end of [cblas_tpmv<double>]

implement cblas_tpmv<ccmplx>
  (Order, Uplo, TransA, Diag, N, Ap, X, incX) =
  cblas_ctpmv (Order, Uplo, TransA, Diag, N, Ap, X, incX)
// end of [cblas_tpmv<ccmplx>]

implement cblas_tpmv<zcmplx>
  (Order, Uplo, TransA, Diag, N, Ap, X, incX) =
  cblas_ztpmv (Order, Uplo, TransA, Diag, N, Ap, X, incX)
// end of [cblas_tpmv<zcmplx>]

(* ****** ****** *)

//
// cblas_trsv: S, D, C, Z
//

implement cblas_trsv<float>
  (Order, Uplo, TransA, Diag, N, A, lda, X, incX) =
  cblas_strsv (Order, Uplo, TransA, Diag, N, A, lda, X, incX)
// end of [cblas_trsv<float>]

implement cblas_trsv<double>
  (Order, Uplo, TransA, Diag, N, A, lda, X, incX) =
  cblas_dtrsv (Order, Uplo, TransA, Diag, N, A, lda, X, incX)
// end of [cblas_trsv<double>]

implement cblas_trsv<ccmplx>
  (Order, Uplo, TransA, Diag, N, A, lda, X, incX) =
  cblas_ctrsv (Order, Uplo, TransA, Diag, N, A, lda, X, incX)
// end of [cblas_trsv<ccmplx>]

implement cblas_trsv<zcmplx>
  (Order, Uplo, TransA, Diag, N, A, lda, X, incX) =
  cblas_ztrsv (Order, Uplo, TransA, Diag, N, A, lda, X, incX)
// end of [cblas_trsv<zcmplx>]

(* ****** ****** *)

//
// cblas_tbsv: S, D, C, Z
//

implement cblas_tbsv<float>
  (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX) =
  cblas_stbsv (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX)
// end of [cblas_tbsv<float>]

implement cblas_tbsv<double>
  (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX) =
  cblas_dtbsv (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX)
// end of [cblas_tbsv<double>]

implement cblas_tbsv<ccmplx>
  (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX) =
  cblas_ctbsv (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX)
// end of [cblas_tbsv<ccmplx>]

implement cblas_tbsv<zcmplx>
  (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX) =
  cblas_ztbsv (Order, Uplo, TransA, Diag, N, K, A, lda, X, incX)
// end of [cblas_tbsv<zcmplx>]

(* ****** ****** *)

//
// cblas_tpsv: S, D, C, Z
//

implement cblas_tpsv<float>
  (Order, Uplo, TransA, Diag, N, Ap, X, incX) =
  cblas_stpsv (Order, Uplo, TransA, Diag, N, Ap, X, incX)
// end of [cblas_tpsv<float>]

implement cblas_tpsv<double>
  (Order, Uplo, TransA, Diag, N, Ap, X, incX) =
  cblas_dtpsv (Order, Uplo, TransA, Diag, N, Ap, X, incX)
// end of [cblas_tpsv<double>]

implement cblas_tpsv<ccmplx>
  (Order, Uplo, TransA, Diag, N, Ap, X, incX) =
  cblas_ctpsv (Order, Uplo, TransA, Diag, N, Ap, X, incX)
// end of [cblas_tpsv<ccmplx>]

implement cblas_tpsv<zcmplx>
  (Order, Uplo, TransA, Diag, N, Ap, X, incX) =
  cblas_ztpsv (Order, Uplo, TransA, Diag, N, Ap, X, incX)
// end of [cblas_tpsv<zcmplx>]

(* ****** ****** *)

//
// cblas_symv: S, D
//

implement cblas_symv<float>
  (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY) =
  cblas_ssymv (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY)
// end of [cblas_symv<float>]

implement cblas_symv<double>
  (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY) =
  cblas_dsymv (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY)
// end of [cblas_symv<double>]

(* ****** ****** *)

//
// cblas_sbmv: S, D
//

implement cblas_sbmv<float>
  (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY) =
  cblas_ssbmv (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY)
// end of [cblas_sbmv<float>]

implement cblas_sbmv<double>
  (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY) =
  cblas_dsbmv (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY)
// end of [cblas_sbmv<double>]

(* ****** ****** *)

//
// cblas_spmv: S, D
//

implement cblas_spmv<float>
  (Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY) =
  cblas_sspmv(Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY)
// end of [cblas_spmv<float>]

implement cblas_spmv<double>
  (Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY) =
  cblas_dspmv(Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY)
// end of [cblas_spmv<double>]


(* ****** ****** *)

//
// cblas_ger: S, D
//

implement cblas_ger<float>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) =
  cblas_sger (Order, M, N, alpha, X, incX, Y, incY, A, lda)
// end of [cblas_ger<float>]

implement cblas_ger<double>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) =
  cblas_dger (Order, M, N, alpha, X, incX, Y, incY, A, lda)
// end of [cblas_ger<double>]

(* ****** ****** *)

//
// cblas_syr: S, D
//

implement cblas_syr<float>
  (Order, Uplo, N, alpha, X, incX, A, lda) =
  cblas_ssyr (Order, Uplo, N, alpha, X, incX, A, lda)
// end of [cblas_syr<float>]

implement cblas_syr<double>
  (Order, Uplo, N, alpha, X, incX, A, lda) =
  cblas_dsyr (Order, Uplo, N, alpha, X, incX, A, lda)
// end of [cblas_syr<double>]

(* ****** ****** *)

//
// cblas_syr2: S, D
//

implement cblas_syr2<float>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda) =
  cblas_ssyr2 (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda)
// end of [cblas_syr2<float>]

implement cblas_syr2<double>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda) =
  cblas_dsyr2 (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda)
// end of [cblas_syr2<double>]

(* ****** ****** *)

//
// cblas_spr: S, D
//

implement cblas_spr<float>
  (Order, Uplo, N, alpha, X, incX, Ap) =
  cblas_sspr (Order, Uplo, N, alpha, X, incX, Ap)
// end of [cblas_spr<float>]

implement cblas_spr<double>
  (Order, Uplo, N, alpha, X, incX, Ap) =
  cblas_dspr (Order, Uplo, N, alpha, X, incX, Ap)
// end of [cblas_spr<double>]

(* ****** ****** *)

//
// cblas_spr2: S, D
//

implement cblas_spr2<float>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A) =
  cblas_sspr2 (Order, Uplo, N, alpha, X, incX, Y, incY, A)
// end of [cblas_spr2<float>]

implement cblas_spr2<double>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A) =
  cblas_dspr2 (Order, Uplo, N, alpha, X, incX, Y, incY, A)
// end of [cblas_spr2<double>]

(* ****** ****** *)

//
// cblas_hemv: C, Z // extended with S, D
//

implement cblas_hemv<float>
  (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPfloat, view@ A)
  val () = cblas_ssymv
    (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY)
  prval () = view@ A := HEMAT_v_of_SYMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_hemv<float>]

implement cblas_hemv<double>
  (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPdouble, view@ A)
  val () = cblas_dsymv
    (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY)
  prval () = view@ A := HEMAT_v_of_SYMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_hemv<double>]

implement cblas_hemv<ccmplx>
  (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY) = let
  var alpha = alpha and beta = beta
in
  cblas_chemv (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY)
end // end of [cblas_symv<ccmplx>]

implement cblas_hemv<zcmplx>
  (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY) = let
  var alpha = alpha and beta = beta
in
  cblas_zhemv (Order, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY)
end // end of [cblas_symv<zcmplx>]

(* ****** ****** *)

//
// cblas_hbmv: C, Z // extended with S, D
//

implement cblas_hbmv<float>
  (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY) = let
  prval pf_mat = SBMAT_v_of_HBMAT_v (REALTYPfloat, view@ A)
  val () = cblas_ssbmv (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY)
  prval () = view@ A := HBMAT_v_of_SBMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_hbmv<float>]

implement cblas_hbmv<double>
  (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY) = let
  prval pf_mat = SBMAT_v_of_HBMAT_v (REALTYPdouble, view@ A)
  val () = cblas_dsbmv (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY)
  prval () = view@ A := HBMAT_v_of_SBMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_hbmv<double>]

implement cblas_hbmv<ccmplx>
  (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY) = let
  var alpha0 = alpha
  var beta0 = beta
in
  cblas_chbmv (Order, Uplo, N, K, alpha0, A, lda, X, incX, beta0, Y, incY)
end // end of [cblas_hbmv<ccmplx>]

implement cblas_hbmv<zcmplx>
  (Order, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY) = let
  var alpha0 = alpha
  var beta0 = beta
in
  cblas_zhbmv (Order, Uplo, N, K, alpha0, A, lda, X, incX, beta0, Y, incY)
end // end of [cblas_hbmv<zcmplx>]

(* ****** ****** *)

//
// cblas_hbmv: C, Z // extended with S, D
//

//
// cblas_hpmv: S, D, C, Z
//

implement cblas_hpmv<float>
  (Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY) = let
  prval pf_mat = SPMAT_v_of_HPMAT_v (REALTYPfloat, view@ Ap)
  val () = cblas_sspmv(Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY)
  prval () = view@ Ap := HPMAT_v_of_SPMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_hpmv<float>]

implement cblas_hpmv<double>
  (Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY) = let
  prval pf_mat = SPMAT_v_of_HPMAT_v (REALTYPdouble, view@ Ap)
  val () = cblas_dspmv(Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY)
  prval () = view@ Ap := HPMAT_v_of_SPMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_hpmv<double>]

implement cblas_hpmv<ccmplx>
  (Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY) = let
  var alpha = alpha and beta = beta
in
  cblas_chpmv(Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY)
end // end of [cblas_hpmv<ccmplx>]

implement cblas_hpmv<zcmplx>
  (Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY) = let
  var alpha = alpha and beta = beta
in
  cblas_zhpmv(Order, Uplo, N, alpha, Ap, X, incX, beta, Y, incY)
end // end of [cblas_hpmv<zcmplx>]

(* ****** ****** *)

//
// cblas_geru: C, Z // extended with S, D
//

implement cblas_geru<float>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) =
  cblas_sger (Order, M, N, alpha, X, incX, Y, incY, A, lda)
// end of [cblas_geru<float>]

implement cblas_geru<double>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) =
  cblas_dger (Order, M, N, alpha, X, incX, Y, incY, A, lda)
// end of [cblas_geru<double>]

implement cblas_geru<ccmplx>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) = let
  var alpha = alpha
in
  cblas_cgeru (Order, M, N, alpha, X, incX, Y, incY, A, lda)
end // end of [cblas_geru<ccmplx>]

implement cblas_geru<zcmplx>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) = let
  var alpha: zcmplx = alpha
in
  cblas_zgeru (Order, M, N, alpha, X, incX, Y, incY, A, lda)
end // end of [cblas_geru<zcmplx>]

(* ****** ****** *)

//
// cblas_gerc: C, Z // extended with S, D
//

implement cblas_gerc<float>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) =
  cblas_sger (Order, M, N, alpha, X, incX, Y, incY, A, lda)
// end of [cblas_gerc<float>]

implement cblas_gerc<double>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) =
  cblas_dger (Order, M, N, alpha, X, incX, Y, incY, A, lda)
// end of [cblas_gerc<double>]

implement cblas_gerc<ccmplx>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) = let
  var alpha = alpha
in
  cblas_cgerc (Order, M, N, alpha, X, incX, Y, incY, A, lda)
end // end of [cblas_gerc<ccmplx>]

implement cblas_gerc<zcmplx>
  (Order, M, N, alpha, X, incX, Y, incY, A, lda) = let
  var alpha: zcmplx = alpha
in
  cblas_zgerc (Order, M, N, alpha, X, incX, Y, incY, A, lda)
end // end of [cblas_gerc<zcmplx>]

(* ****** ****** *)

//
// cblas_her: C, Z // extended with S, D
//

implement cblas_her<float,float>
  (Order, Uplo, N, alpha, X, incX, A, lda) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPfloat, view@ A)
  val () = cblas_ssyr (Order, Uplo, N, alpha, X, incX, A, lda)
  prval () = view@ A := HEMAT_v_of_SYMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_her<float,float>]

implement cblas_her<double,double>
  (Order, Uplo, N, alpha, X, incX, A, lda) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPdouble, view@ A)
  val () = cblas_dsyr (Order, Uplo, N, alpha, X, incX, A, lda)
  prval () = view@ A := HEMAT_v_of_SYMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_her<double,double>]

implement cblas_her<float,ccmplx>
  (Order, Uplo, N, alpha, X, incX, A, lda) =
  cblas_cher (Order, Uplo, N, alpha, X, incX, A, lda)
// end of [cblas_her<float,ccmplx>]

implement cblas_her<double,zcmplx>
  (Order, Uplo, N, alpha, X, incX, A, lda) =
  cblas_zher (Order, Uplo, N, alpha, X, incX, A, lda)
// end of [cblas_her<double,zcmplx>]

(* ****** ****** *)

//
// cblas_her2: C, Z // extended with S, D
//

implement cblas_her2<float>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPfloat, view@ A)
  val () = cblas_ssyr2
    (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda)
  prval () = view@ A := HEMAT_v_of_SYMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_her2<float>]

implement cblas_her2<double>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPdouble, view@ A)
  val () = cblas_dsyr2
    (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda)
  prval () = view@ A := HEMAT_v_of_SYMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_her2<double>]

implement cblas_her2<ccmplx>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda) = let
  var alpha = alpha
in
  cblas_cher2 (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda)
end // end of [cblas_her2<ccmplx>]

implement cblas_her2<zcmplx>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda) = let
  var alpha = alpha
in
  cblas_zher2 (Order, Uplo, N, alpha, X, incX, Y, incY, A, lda)
end // end of [cblas_her2<zcmplx>]

(* ****** ****** *)

//
// cblas_hpr: C, Z // extended with S, D
//

implement cblas_hpr<float,float>
  (Order, Uplo, N, alpha, X, incX, A) = let
  prval pf_mat = SPMAT_v_of_HPMAT_v (REALTYPfloat, view@ A)
  val () = cblas_sspr (Order, Uplo, N, alpha, X, incX, A)
  prval () = view@ A := HPMAT_v_of_SPMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_hpr<float,float>]

implement cblas_hpr<double,double>
  (Order, Uplo, N, alpha, X, incX, A) = let
  prval pf_mat = SPMAT_v_of_HPMAT_v (REALTYPdouble, view@ A)
  val () = cblas_dspr (Order, Uplo, N, alpha, X, incX, A)
  prval () = view@ A := HPMAT_v_of_SPMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_hpr<double,double>]

implement cblas_hpr<float,ccmplx>
  (Order, Uplo, N, alpha, X, incX, Ap) =
  cblas_chpr (Order, Uplo, N, alpha, X, incX, Ap)
// end of [cblas_hpr<float,ccmplx>]

implement cblas_hpr<double,zcmplx>
  (Order, Uplo, N, alpha, X, incX, Ap) =
  cblas_zhpr (Order, Uplo, N, alpha, X, incX, Ap)
// end of [cblas_hpr<double,zcmplx>]

(* ****** ****** *)

//
// cblas_hpr2: C, Z // extended with S, D
//

implement cblas_hpr2<float>
  (Order, Uplo, N, alpha, X, incX, Y, incY, Ap) = let
  prval pf_mat = SPMAT_v_of_HPMAT_v (REALTYPfloat, view@ Ap)
  val () = cblas_sspr2 (Order, Uplo, N, alpha, X, incX, Y, incY, Ap)
  prval () = view@ Ap := HPMAT_v_of_SPMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_hpr2<float>]

implement cblas_hpr2<double>
  (Order, Uplo, N, alpha, X, incX, Y, incY, Ap) = let
  prval pf_mat = SPMAT_v_of_HPMAT_v (REALTYPdouble, view@ Ap)
  val () = cblas_dspr2 (Order, Uplo, N, alpha, X, incX, Y, incY, Ap)
  prval () = view@ Ap := HPMAT_v_of_SPMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_hpr2<double>]

implement cblas_hpr2<ccmplx>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A) = let
  var alpha = alpha
in
  cblas_chpr2 (Order, Uplo, N, alpha, X, incX, Y, incY, A)
end // end of [cblas_hpr2<ccmplx>]

implement cblas_hpr2<zcmplx>
  (Order, Uplo, N, alpha, X, incX, Y, incY, A) = let
  var alpha = alpha
in
  cblas_zhpr2 (Order, Uplo, N, alpha, X, incX, Y, incY, A)
end // end of [cblas_hpr2<zcmplx>]

(* ****** ****** *)

//
// BLAS level 3
//

(* ****** ****** *)

//
// cblas_gemm: S, D, C, Z
//

fn cblas_gemm_overlap_check
  (pA: ptr, pB: ptr, pC: ptr):<> void =
  if (pA = pC orelse pB = pC) then $effmask_all begin
    prerr "error(ATS/CBLAS): cblas_gemm";
    prerr ": overlapping between input and output is not allowed!";
    prerr_newline ();
    exit (1)
  end // end of [if]
// end of [cblas_gemm_overlap_check]

implement cblas_gemm<float> (
    pfa, pfb
  | Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) = let
#if ATS_CBLAS_DEBUG #then
  val () = cblas_gemm_overlap_check (&A, &B, &C)
#endif
in
  cblas_sgemm (
    pfa, pfb
  | Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) // end of [cblas_sgemm]
end // end of [cblas_gemm<float>]

implement cblas_gemm<double> (
    pfa, pfb
  | Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) = let
#if ATS_CBLAS_DEBUG #then
  val () = cblas_gemm_overlap_check (&A, &B, &C)
#endif
in
  cblas_dgemm (
    pfa, pfb
  | Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) // end of [cblas_dgemm]
end // end of [cblas_gemm<double>]

implement cblas_gemm<ccmplx> (
    pfa, pfb
  | Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) = let
#if ATS_CBLAS_DEBUG #then
  val () = cblas_gemm_overlap_check (&A, &B, &C)
#endif
  var alpha: ccmplx = alpha and beta: ccmplx = beta
in
  cblas_cgemm (
    pfa, pfb
  | Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) // end of [cblas_cgemm]
end // end of [cblas_gemm<ccmplx>]

implement cblas_gemm<zcmplx> (
    pfa, pfb
  | Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) = let
#if ATS_CBLAS_DEBUG #then
  val () = cblas_gemm_overlap_check (&A, &B, &C)
#endif
  var alpha: zcmplx = alpha and beta: zcmplx = beta
in
  cblas_zgemm (
    pfa, pfb
  | Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) // end of [cblas_zgemm]
end // end of [cblas_gemm<zcmplx>]

(* ****** ****** *)

//
// cblas_syrk: S, D, C, Z
//

implement cblas_syrk<float>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc) =
  cblas_ssyrk (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc)
// end of [cblas_syrk<float>]

implement cblas_syrk<double>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc) =
  cblas_dsyrk (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc)
// end of [cblas_syrk<double>]

implement cblas_syrk<ccmplx>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc) = let
  var alpha: ccmplx = alpha and beta: ccmplx = beta
in
  cblas_csyrk (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc)
end // end of [cblas_syrk<ccmplx>]

implement cblas_syrk<zcmplx>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc) = let
  var alpha: zcmplx = alpha and beta: zcmplx = beta
in
  cblas_zsyrk (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc)
end // end of [cblas_syrk<zcmplx>]

(* ****** ****** *)

//
// cblsa_syr2K: S, D, C, Z
//

implement cblas_syr2k<float>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc) =
  cblas_ssyr2k (
    pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  )
// end of [cblas_syr2k<float>]

implement cblas_syr2k<double>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc) =
  cblas_dsyr2k (
    pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  )
// end of [cblas_syr2k<double>]

implement cblas_syr2k<ccmplx>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc) = let
  var alpha: ccmplx = alpha and beta: ccmplx = beta
in
  cblas_csyr2k (
    pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  )
end // end of [cblas_syr2k<ccmplx>]

implement cblas_syr2k<zcmplx>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc) = let
  var alpha: zcmplx = alpha and beta: zcmplx = beta
in
  cblas_zsyr2k (
    pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  )
end // end of [cblas_syr2k<zcmplx>]

(* ****** ****** *)

//
// cblas_symm: S, D, C, Z
//

implement cblas_symm<float>
  (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc) =
  cblas_ssymm (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc)
// end of [cblas_symm<float>]

implement cblas_symm<double>
  (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc) =
  cblas_dsymm (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc)
// end of [cblas_symm<double>]

implement cblas_symm<ccmplx>
  (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc) = let
  var alpha: ccmplx = alpha and beta: ccmplx = beta
in
  cblas_csymm (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc)
end // end of [cblas_symm<ccmplx>]

implement cblas_symm<zcmplx>
  (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc) = let
  var alpha: zcmplx = alpha and beta: zcmplx = beta
in
  cblas_zsymm (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc)
end // end of [cblas_symm<zcmplx>]

(* ****** ****** *)

//
// cblas_trmm: S, D, C, Z
//

implement cblas_trmm<float>
  (pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb) =
  cblas_strmm (
    pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) // end of [cblas_strmm]
// end of [cblas_trmm<float>]

implement cblas_trmm<double>
  (pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb) =
  cblas_dtrmm (
    pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) // end of [cblas_dtrmm]
// end of [cblas_trmm<double>]

implement cblas_trmm<ccmplx> (
    pfa
  | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) = let
  var alpha: ccmplx = alpha
in
  cblas_ctrmm (
    pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) // end of [cblas_ctrmm]
end // end of [cblas_trmm<ccmplx>]

implement cblas_trmm<zcmplx> (
    pfa
  | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) = let
  var alpha: zcmplx = alpha
in
  cblas_ztrmm (
    pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) // end of [cblas_ztrmm]
end // end of [cblas_trmm<zcmplx>]

(* ****** ****** *)

//
// cblas_trsm: S, D, C, Z
//

implement cblas_trsm<float>
  (pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb) =
  cblas_strsm (
    pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) // end of [cblas_strsm]
// end of [cblas_trsm<float>]

implement cblas_trsm<double>
  (pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb) =
  cblas_dtrsm (
    pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) // end of [cblas_dtrsm]
// end of [cblas_trsm<double>]

implement cblas_trsm<ccmplx> (
    pfa
  | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) = let
  var alpha: ccmplx = alpha
in
  cblas_ctrsm (
    pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) // end of [cblas_ctrsm]
end // end of [cblas_trsm<ccmplx>]

implement cblas_trsm<zcmplx> (
    pfa
  | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) = let
  var alpha: zcmplx = alpha
in
  cblas_ztrsm (
    pfa | Order, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb
  ) // end of [cblas_ztrsm]
end // end of [cblas_trsm<zcmplx>]

(* ****** ****** *)

//
// cblas_hemm: C, Z // extended with S, D
//

implement cblas_hemm<float>
  (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPfloat, view@ A)
  val () = cblas_ssymm
    (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc)
  prval () = view@ A := HEMAT_v_of_SYMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_hemm<float>]

implement cblas_hemm<double>
  (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPdouble, view@ A)
  val () = cblas_dsymm
    (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc)
  prval () = view@ A := HEMAT_v_of_SYMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_hemm<double>]

implement cblas_hemm<ccmplx>
  (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc) = let
  var alpha: ccmplx = alpha and beta: ccmplx = beta
in
  cblas_chemm (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc)
end // end of [cblas_hemm<ccmplx>]

implement cblas_hemm<zcmplx>
  (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc) = let
  var alpha: zcmplx = alpha and beta: zcmplx = beta
in
  cblas_zhemm (pfa | Order, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc)
end // end of [cblas_hemm<zcmplx>]

(* ****** ****** *)

//
// cblas_herk: C, Z // extended with S, D
//

implement cblas_herk<float,float>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPfloat, view@ C)
  val () = cblas_ssyrk
    (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc)
  prval () = view@ C := HEMAT_v_of_SYMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_herk<float>]

implement cblas_herk<double,double>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPdouble, view@ C)
  val () = cblas_dsyrk
    (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc)
  prval () = view@ C := HEMAT_v_of_SYMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_herk<double>]

implement cblas_herk<ccmplx,float>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc) = begin
  cblas_cherk (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc)
end // end of [cblas_herk<ccmplx>]

implement cblas_herk<zcmplx,double>
  (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc) = begin
  cblas_zherk (pfa | Order, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc)
end // end of [cblas_herk<zcmplx>]

(* ****** ****** *)

//
// cblas_her2k: C, Z // extended with S, D
//

implement cblas_her2k<float,float> (
    pfa
  | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPfloat, view@ C)
  val () = cblas_ssyr2k (
    pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) // end of [val]
  prval () = view@ C := HEMAT_v_of_SYMAT_v (REALTYPfloat, pf_mat)
in
  // nothing
end // end of [cblas_her2k<ccmplx>]

implement cblas_her2k<double,double> (
    pfa
  | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) = let
  prval pf_mat = SYMAT_v_of_HEMAT_v (REALTYPdouble, view@ C)
  val () = cblas_dsyr2k (
    pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) // end of [val]
  prval () = view@ C := HEMAT_v_of_SYMAT_v (REALTYPdouble, pf_mat)
in
  // nothing
end // end of [cblas_her2k<ccmplx>]

implement cblas_her2k<ccmplx,float> (
    pfa
  | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) = let
  var alpha: ccmplx = alpha in
  cblas_cher2k (
    pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  )
end // end of [cblas_her2k<ccmplx>]

implement cblas_her2k<zcmplx,double> (
    pfa
  | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  ) = let
  var alpha: zcmplx = alpha in
  cblas_zher2k (
    pfa | Order, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc
  )
end // end of [cblas_her2k<zcmplx>]

(* ****** ****** *)

(* end of [cblas.hats] *)
