//
// some testing code for the ATS interface to BLAS
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

local #include "contrib/cblas/HATS/cblas.hats" in (*empty*) end

(* ****** ****** *)

staload "libc/SATS/complex.sats"
staload "libats/SATS/genarrays.sats"

(* ****** ****** *)

staload "contrib/cblas/SATS/cblas.sats"

(* ****** ****** *)

staload "libats/SATS/fmatrix.sats"
staload _(*anonymous*) = "libats/DATS/fmatrix.dats"

(* ****** ****** *)

(*
fun cblas_dgemm_instance2 {ma,na:nat} {lda,ldc:nat} (
    M: int ma, N: int na  
  , A: &GEMAT (double, ma, na, lda), lda: int lda
  , C: &GEMAT (double, na, na, ldc), ldc: int ldc
  ) : void = cblas_dgemm (
    TRANDIM_T
  , TRANDIM_N
  | CblasColMajor
  , CblasTrans, CblasNoTrans
  , N, N, M
  , 1.0 (* alpha *)
  , A, lda, A, lda 
  , 0.0 (* beta *)
  , C, ldc 
  ) // end of [cblas_dgemm]
// end of [cblas_gemm_instance2]
*)

(* ****** ****** *)

extern fun{a:t@ype} of_double (x: double):<> a

implement of_double<float> (x: double) = float_of_double (x)
implement of_double<double> (x: double) = x
implement of_double<ccmplx> (x) = ccmplx_of_float (float_of_double x)
implement of_double<zcmplx> (x) = zcmplx_of_double (x)

extern fun{a:t@ype} print_elt (x: a): void
implement print_elt<float> (x) = print x
implement print_elt<double> (x) = print x
implement print_elt<ccmplx> (x) = print x
implement print_elt<zcmplx> (x) = print x

(* ****** ****** *)

extern fun{a:t@ype}
mul_fmat_fmat {m,n,k:pos} (
   m: int m, n: int n, k: int k
 , A: &fmatrix (a, m, k), B: &fmatrix (a, k, n), C: &fmatrix (a, m, n)
 ) : void
// end of [mul_fmat_fmat]

(* ****** ****** *)

implement{a}
mul_fmat_fmat (m, n, k, A, B, C) = let
  stavar v1: view and v2: view
  prval (pfA, fpfA) = GEMAT_v_of_fmatrix_v {a} (view@ A)
  prval (pfB, fpfB) = GEMAT_v_of_fmatrix_v {a} (view@ B)
  prval pfAB = (pfA, pfB)
  prval (pfC, fpfC) = GEMAT_v_of_fmatrix_v {a} (view@ C)
  val alpha: a = of_double (1.0) and beta: a = of_double (0.0)
//
  viewdef v = (v1, v2)
  val () = cblas_gemm__main<a> {v} (
    pfAB
  , TRANDIM_N
  , vsubr_tup_2_0 ()
  , TRANDIM_N
  , vsubr_tup_2_1 ()
  | CblasColMajor
  , CblasNoTrans, CblasNoTrans
  , m, n, k
  , alpha
  , &A, m, &B, k
  , beta
  , C, m
  ) // end of [cblas_gemm__main<a>]
//
  prval (pfA: v1, pfB: v2) = pfAB
  prval () = view@ A := fpfA (pfA)
  prval () = view@ B := fpfB (pfB)
  prval () = view@ C := fpfC (pfC)
in
  // nothing
end // end of [mul_fmat_fmat]

(* ****** ****** *)

fn{a:t@ype} print_fmatrix {m,n:nat}
  (M: &fmatrix (a, m, n), m: size_t m, n: size_t n)
  : void = let
(*
  val pr = lam
    (i: int, j: int, x: a): void =<cloref1> let
    val () = if j > 0 then print ", "
    val () = if (i > 0 andalso j = 0) then print "\n" 
    val () = print_elt<a> (x)
  in
    // empty
  end // end of [val]  
  var i: int? and j: int? // uninitialized
  val () = for* (i:natLte m) =>
    (i := 0; i < m; i := i+1) begin
    for* (i: natLt m, j: natLte n) =>
      (j := 0; j < n; j := j+1) pr (i, j, M[i,m,j])
    // end of [for*]
  end // end of [val]
*)
//
  val () = fmatrix_ptr_iforeach_fun<a>
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
  } // end of [val]
//
in
  // empty
end // end of [print_fmatrix]

(* ****** ****** *)

typedef elt = double

(* ****** ****** *)

#define N 10
val (pf_NN | NN) = op imul2 (N, N)
prval () = mul_nat_nat_nat (pf_NN)
val NN_sz = size1_of_int1 (NN)

(* ****** ****** *)

val (
  pf1_gc, pf1_arr | p1_arr
) =
  array_ptr_alloc_tsz {elt} (NN_sz, sizeof<elt>)
// end of [val]
prval pf1_mat = fmatrix_v_of_array_v {elt?} (pf_NN, pf1_arr)
//
prval pf = unit_v ()
val () =
  fmatrix_ptr_initialize_vclo<elt>
    (pf | !p1_arr, N, N, !p_clo) where {
  var !p_clo = @lam (
      pf: !unit_v | x: &(elt?) >> elt, i: size_t, j: size_t
    ) : void =<clo> let
    val ij = (if i <= j then i else j): size_t
  in
    x := of_double<elt> (double_of_size (ij + 1))
    // x := zcmplx_make_cart (double_of i, double_of j)
  end (* end of [var] *)
}
prval unit_v () = pf
//
val () = print "M1=\n"
val () = print_fmatrix<elt> (!p1_arr, N, N)
val () = print_newline ()

(* ****** ****** *)

val (
  pf2_gc, pf2_arr | p2_arr
) =
  array_ptr_alloc_tsz {elt} (NN_sz, sizeof<elt>)
// end of [val]
prval pf2_mat = fmatrix_v_of_array_v {elt?} (pf_NN, pf2_arr)
//
prval pf = unit_v ()
val () =
  fmatrix_ptr_initialize_vclo<elt>
    (pf | !p2_arr, N, N, !p_clo) where {
  var !p_clo = @lam (
      pf: !unit_v | x: &(elt?) >> elt, i: size_t, j: size_t
    ) : void =<clo> begin
    x := of_double<elt> (0.0)
  end (* end of [var] *)
}
prval unit_v () = pf
//
val () = print "M2=\n"
val () = print_fmatrix<elt> (!p2_arr, N, N)
val () = print_newline ()

(* ****** ****** *)

val (pf1'_arr | p1'_arr) = __cast (!p1_arr) where {
  extern castfn __cast
    (A: &fmatrix (elt, N, N)): [l:addr] (fmatrix_v (elt, N, N, l) | ptr l)
} // end of [val]
val () = mul_fmat_fmat (N, N, N, !p1_arr, !p1'_arr, !p2_arr)


val () = print "M2=\n"
val () = print_fmatrix<elt> (!p2_arr, N, N)
val () = print_newline ()

(* ****** ****** *)

prval (pf1_NN, pf1_arr) = array_v_of_fmatrix_v {elt} (pf1_mat)
prval () = mul_isfun (pf_NN, pf1_NN)
val () = array_ptr_free (pf1_gc, pf1_arr | p1_arr)

prval (pf2_NN, pf2_arr) = array_v_of_fmatrix_v {elt} (pf2_mat)
prval () = mul_isfun (pf_NN, pf2_NN)
val () = array_ptr_free (pf2_gc, pf2_arr | p2_arr)

(* ****** ****** *)

(*
// HX: no longer needed
dynload "libc/DATS/complex.dats"
dynload "libats/DATS/genarrays.dats"
dynload "libats/DATS/fmatrix.dats"
*)

(* ****** ****** *)

implement main () = ()

(* ****** ****** *)

(* end of [gemm_test1.dats] *)
