//
// some testing code for the ATS interface to BLAS
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(*
** The code is translated from the C version written by
** Shivkumar Chandrasekaran that is attached at the end
** of the file.
*)

(* ****** ****** *)

local #include "contrib/cblas/HATS/cblas.hats" in (*empty*) end

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/matrix.dats"

(* ****** ****** *)

staload "libc/SATS/complex.sats"

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"
staload _(*anonymous*) = "libats/DATS/genarrays.dats"

(* ****** ****** *)

staload "contrib/cblas/SATS/cblas.sats"

(* ****** ****** *)

extern fun{a:t@ype} lu {n:nat} {ord:order} {lda:pos}
  (n: int n, A: &GEMAT (a, n, n, ord, lda), ord: ORDER ord, lda: size_t lda):<> void
// end of [lu]

(* ****** ****** *)

extern fun{a:t@ype} of_double (x: double):<> a
implement of_double<float> (x) = float_of_double (x)
implement of_double<double> (x) = x
implement of_double<ccmplx> (x) = ccmplx_of_float (float_of_double x)
implement of_double<zcmplx> (x) = zcmplx_of_double (x)

(* ****** ****** *)

extern fun{a:t@ype} print_elt (x: a): void
implement print_elt<float> (x) = print x
implement print_elt<double> (x) = print x
implement print_elt<ccmplx> (x) = print x
implement print_elt<zcmplx> (x) = print x

(* ****** ****** *)

extern fun{a1,a2:t@ype} abs (x: a2):<> a1
implement abs<float,float> (x) = abs_float (x)
implement abs<double,double> (x) = abs_double (x)
implement abs<float,ccmplx> (x) = abs_ccmplx (x)
implement abs<double,zcmplx> (x) = abs_zcmplx (x)

(* ****** ****** *)

extern fun{a:t@ype} add (x1: a, x2: a):<> a
implement add<float> (x1, x2) = x1 + x2
implement add<double> (x1, x2) = x1 + x2
implement add<ccmplx> (x1, x2) = x1 + x2
implement add<zcmplx> (x1, x2) = x1 + x2

extern fun{a:t@ype} sub (x1: a, x2: a):<> a
implement sub<float> (x1, x2) = x1 - x2
implement sub<double> (x1, x2) = x1 - x2
implement sub<ccmplx> (x1, x2) = x1 - x2
implement sub<zcmplx> (x1, x2) = x1 - x2

extern fun{a:t@ype} mul (x1: a, x2: a):<> a
implement mul<float> (x1, x2) = x1 * x2
implement mul<double> (x1, x2) = x1 * x2
implement mul<ccmplx> (x1, x2) = x1 * x2
implement mul<zcmplx> (x1, x2) = x1 * x2

extern fun{a:t@ype} div (x1: a, x2: a):<> a
implement div<float> (x1, x2) = x1 / x2
implement div<double> (x1, x2) = x1 / x2
implement div<ccmplx> (x1, x2) = x1 / x2
implement div<zcmplx> (x1, x2) = x1 / x2

(* ****** ****** *)

#define i2sz size1_of_int1

(* ****** ****** *)

extern
fun{a:t@ype} gemaxpy
  {ordA,ordB:order} {m,n:nat} {lda,ldb:pos} (
  OrderA: ORDER (ordA)
, OrderB: ORDER (ordB)
, M: size_t m, N: size_t n
, alpha: a
, A: &GEMAT (a, m, n, ordA, lda), lda: size_t lda
, B: &GEMAT (a, m, n, ordB, ldb), ldb: size_t ldb
) :<> void // end of [gemaxpy]

implement{a}
gemaxpy {ordA,ordB} {m,n} {lda,ldb}
    (OrderA, OrderB, M, N, alpha, A, lda, B, ldb) = let
  fun loop {m,n:nat} .<n>. (
    M: size_t m, N: size_t n
  , A: &GEMAT (a, m, n, ordA, lda)
  , B: &GEMAT (a, m, n, ordB, ldb)
  ) :<cloref> void = if N > 0 then let
    val (pfA1, pfA2, fpfA | pA1, pA2) =
      GEMAT_ptr_split1x2<a> (view@ A | OrderA, &A, lda, 1)
    val (pfB1, pfB2, fpfB | pB1, pB2) =
      GEMAT_ptr_split1x2<a> (view@ B | OrderB, &B, ldb, 1)
    prval (pfA1_inc, pfA1_V, fpfA1) = GEVEC_v_of_GEMAT_v_col {a} (pfA1)
    val dA = MATVECINC_get (pfA1_inc | ORDERcol, OrderA, lda)
    val dA = int1_of_size1 dA
    prval (pfB1_inc, pfB1_V, fpfB1) = GEVEC_v_of_GEMAT_v_col {a} (pfB1)
    val dB = MATVECINC_get (pfB1_inc | ORDERcol, OrderB, ldb)
    val dB = int1_of_size1 dB
    val M_i = int1_of_size1 M
    val () = cblas_axpy<a> (M_i, alpha, !pA1, dA, !pB1, dB)
    prval () = pfA1 := fpfA1 (pfA1_V)
    prval () = pfB1 := fpfB1 (pfB1_V)
    val () = loop (M, N-1, !pA2, !pB2)
    prval () = view@ A := fpfA (pfA1, pfA2)
    prval () = view@ B := fpfB (pfB1, pfB2)
  in
    // empty
  end // end of [if]
in
  loop (M, N, A, B)
end // end of [gemaxpy]

(* ****** ****** *)

extern
fun{a1,a2:t@ype} gemnrm2
  {ordA:order} {m,n:nat} {lda:pos} (
  OrderA: ORDER (ordA)
, M: size_t m, N: size_t n
, A: &GEMAT (a2, m, n, ordA, lda), lda: size_t lda
) :<> a1 // end of [gemnrm2]

implement{a1,a2}
gemnrm2 {ordA} {m,n} {lda}
    (OrderA, M, N, A, lda) = let
  fun loop {m,n:nat} .<n>. (
    M: size_t m, N: size_t n, A: &GEMAT (a2, m, n, col, lda), res: a1
  ) :<cloref> a1 = if N > 0 then let
    val (pfA1, pfA2, fpfA | pA1, pA2) =
      GEMAT_ptr_split1x2<a2> (view@ A | ORDERcol, &A, lda, 1)
    prval (pfA1_inc, pfA1_V, fpfA1) = GEVEC_v_of_GEMAT_v_col  (pfA1)
    prval MATVECINCcolcol () = pfA1_inc
    val M_i = int1_of_size1 M
    val res1 = cblas_nrm2<a1,a2> (M_i, !pA1, 1)
    prval () = pfA1 := fpfA1 (pfA1_V)
    val res = loop (M, N-1, !pA2, res \add res1)
    prval () = view@ A := fpfA (pfA1, pfA2)
  in
    res // loop exits
  end else begin
    res // loop exits
  end (* end of [if] *)
in
  case+ OrderA of
  | ORDERrow () => res where {
      prval TRANORDrowcol () = GEMAT_v_trans (view@ A)
      val res = loop (N, M, A, of_double<a1> (0.0))
      prval TRANORDcolrow () = GEMAT_v_trans (view@ A)      
    } // end of [ORDERrow]
  | ORDERcol () => loop (M, N, A, of_double<a1> (0.0))
end // end of [gemnrm2]

extern fun{a1,a2:t@ype} gemnrm2
  {ordA:order} {m,n:nat} {lda:pos} (
  OrderA: ORDER (ordA)
, M: size_t m, N: size_t n
, A: &GEMAT (a2, m, n, ordA, lda), lda: size_t lda
) :<> a1
implement{a1,a2}
gemnrm2 {ordA} {m,n} {lda}
    (OrderA, M, N, A, lda) = let
  fun loop {m,n:nat} .<n>. (
    M: size_t m, N: size_t n, A: &GEMAT (a2, m, n, col, lda), res: a1
  ) :<cloref> a1 = if N > 0 then let
    val (pfA1, pfA2, fpfA | pA1, pA2) =
      GEMAT_ptr_split1x2<a2> (view@ A | ORDERcol, &A, lda, 1)
    prval (pfA1_inc, pfA1_V, fpfA1) = GEVEC_v_of_GEMAT_v_col  (pfA1)
    prval MATVECINCcolcol () = pfA1_inc
    val M_i = int1_of_size1 M
    val res1 = cblas_nrm2<a1,a2> (M_i, !pA1, 1)
    prval () = pfA1 := fpfA1 (pfA1_V)
    val res = loop (M, N-1, !pA2, res \add res1)
    prval () = view@ A := fpfA (pfA1, pfA2)
  in
    res // loop exits
  end else begin
    res // loop exits
  end (* end of [if] *)
in
  case+ OrderA of
  | ORDERrow () => res where {
      prval TRANORDrowcol () = GEMAT_v_trans (view@ A)
      val res = loop (N, M, A, of_double<a1> (0.0))
      prval TRANORDcolrow () = GEMAT_v_trans (view@ A)      
    } // end of [ORDERrow]
  | ORDERcol () => loop (M, N, A, of_double<a1> (0.0))
end // end of [gemnrm2]

(* ****** ****** *)

implement{a}
lu {n} {ord} {lda}
  (n, A, ord, lda) = loop (view@ A | n, &A) where {
  fun loop {n:nat} {l:addr} .<n>.
    (pf_mat: !GEMAT_v (a, n, n, ord, lda, l) | n: int n, p_mat: ptr l)
    :<cloref> void =
    if n >= 2 then let
      val (pf11, pf12, pf21, pf22, fpf | p11, p12, p21, p22) =
        GEMAT_ptr_split2x2<a> (pf_mat | ord, p_mat, lda, 1, 1)
      // end of [val]  
//
      val _0 = size1_of_int1 0
      val A_0_0 = GEMAT_ptr_get_elt_at<a> (ord, !p11, lda, _0, _0)
      val n1 = n - 1
//
      var i: size_t // uninitialized
      val () = for*
        {i:nat | i < n} .<n-i>. (i: size_t i) =>
        (i := _0; i < n1; i := i+1) let
        val (pf, fpf | p) = GEMAT_ptr_takeout<a> (pf21 | ord, p21, lda, i, _0)
        // end of [val]  
        val () = !p := (!p \div A_0_0)
        prval () = pf21 := fpf (pf)
      in
        // nothing
      end // end of [val]
//
      val lda_i = int1_of_size1 lda
//
      val cbord = CBLAS_ORDER_of_ORDER (ord)
      val alpha: a = of_double (~1.0) and beta: a = of_double (1.0)
      val () = cblas_gemm<a> (
        TRANDIM_N
      , TRANDIM_N
      | cbord
      , CblasNoTrans
      , CblasNoTrans
      , n1, n1, 1
      , alpha
      , !p21, lda_i
      , !p12, lda_i
      , beta
      , !p22, lda_i
      ) // end of [val]
//
      val () = loop (pf22 | n1, p22)
      prval () = pf_mat := fpf (pf11, pf12, pf21, pf22)
    in
      // nothing
    end // end of [if]
  // end of [loop]  
} // end of [lu]

(* ****** ****** *)

fn{a:t@ype}
print_matrix {m,n:nat} (
    M: matrix (a, m, n), m: int m, n: int n
  ) : void = let
  val pr = lam
    (i: size_t, j: size_t, x: &a): void =<cloref1> let
    val () = if j > 0 then print ", "
    val () = if (i > 0 andalso j = 0) then print "\n" 
    val () = print_elt<a> (x)
  in
    // empty
  end // end of [val]  
  val () = matrix_iforeach_cloref<a> (M, pr, size1_of_int1 m, size1_of_int1 n) 
in
  // empty
end // end of [print_matrix]

(* ****** ****** *)

%{^
ats_void_type
__matrix_free (ats_ptr_type A) { ATS_FREE (A); return ; }
%} // end of [%{^]

(* ****** ****** *)

fun{a1,a2:t@ype} test
  {N:nat | N >= 1} (N: int N): void = () where {
//
  prval pf = unit_v ()
  val A =
    matrix_make_vclo<a2>
      (pf | N, N, !p_clo) where {
    val N = size1_of_int1 (N)
    var !p_clo = @lam (
        pf: !unit_v | i: size_t, j: size_t, x: &(a2?) >> a2
      ) : void =<clo> let
      val i = int_of_size i and j = int_of_size j
    in  
      x := of_double<a2> (double_of (min (i, j) + 2))
    end (* end of [var] *)
  }
  prval unit_v () = pf
  val (pf_A | p_A) = __cast (A) where {
    extern castfn __cast {nr,nc:nat}
      (_: matrix (a2, nr, nc)): [l:addr] (GEMAT (a2, nr, nc, row, nc) @ l | ptr l)
  } // end of [val]
//
  val () = if (N <= 10) then begin
    print "A =\n"; print_matrix<a2> (A, N, N); print_newline ()
  end // end of [val]
  val () = lu<a2> (N, !p_A, ORDERrow, i2sz N) // LU factorization
  val () = if (N <= 10) then begin
    print "L\U =\n"; print_matrix<a2> (A, N, N); print_newline ()
  end // end of [val]
//
(*
  var res: a1 = of_double<a1> (0.0)
  var i: int? and j: int? and k: int?
//
// upper part
//
  val () = for*
    (i: natLte N) =>
    (i := 0; i < N; i := i + 1) let
    val () = for*
      (i: natLt N, j: natLte N) =>
      (j := i; j < N; j := j + 1) let
      var tmp: a2 = of_double<a2> (0.0)
      val () = for*
        (i: natLt N, j: natLt N, k: natLte N) =>
        (k := 0; k < i; k := k + 1) let
        val () = tmp := add<a2> (tmp, mul<a2> (A[i,N,k], A[k,N,j]))
      in
        // nothing
      end // end of [val]
      val () = tmp := add<a2> (tmp,  A[i,N,j])
      val x = of_double<a2> (double_of (min(i,j) + 2))
      val () = res :=  add<a1> (res, abs<a1,a2> (tmp \sub x))
    in
      // nothing
    end // end of [val]
  in
    // nothing
  end // end of [val]  
//
// lower part
//
  val () = for*
    (j: natLte N) =>
    (j := 0; j < N-1; j := j+1) let
    val () = for*
      (i: natLte N, j: natLt (N-1)) =>
      (i := j+1; i < N; i := i + 1) let
      var tmp: a2 = of_double<a2> (0.0)
      val () = for*
        (i: natLt N, j: natLt (N-1), k: natLte N) =>
        (k := 0; k <= j; k := k + 1) let
        val () = tmp := add<a2> (tmp, A[i,N,k] \mul A[k,N,j])
      in
        // nothing
      end // end of [val]
      val x = of_double (double_of (min(i,j) + 2))
      val () = res := add<a1> (res, abs (tmp \sub x))
    in
      // nothing
    end // end of [val]
  in
    // nothing
  end // end of [val]
  val () = (print_matrix<a2> (A, N, N); print_newline ())
*)
//
  prval pf = unit_v ()
  // [A1] is a *unit* matrix
  val A1 =
    matrix_make_vclo<a2>
      (pf | N, N, !p_clo) where {
    val N = size1_of_int1 (N)
    var !p_clo = @lam (
        pf: !unit_v | i: size_t, j: size_t, x: &(a2?) >> a2
      ) : void =<clo> let
      val i = int_of_size i and j = int_of_size j
    in  
      if i = j then x := of_double<a2> (1.0) else x := of_double<a2> (0.0)
    end (* end of [var] *)
  }
  prval unit_v () = pf
  val (pf_A1 | p_A1) = __cast (A1) where {
    extern castfn __cast {nr,nc:nat}
      (_: matrix (a2, nr, nc)): [l:addr] (GEMAT (a2, nr, nc, row, nc) @ l | ptr l)
  } // end of [val]
//
  prval (pf_A_up_u, fpf_A) = TRMAT_v_of_GEMAT_v (pf_A, UPLOupper, DIAGnonunit)
  val () = cblas_trmm<a2> (
    SIDEDIM_R
  | CblasRowMajor
  , CblasRight
  , CblasUpper
  , CblasNoTrans
  , CblasNonUnit
  , N, N
  , of_double<a2> (1.0)
  , !p_A, N
  , !p_A1, N
  )
  prval () = (pf_A := fpf_A (pf_A_up_u))
  val () = if (N <= 10) then begin
    print "U =\n"; print_matrix<a2> (A1, N, N); print_newline ()
  end // end of [val]
//
  prval (pf_A_lo_nu, fpf_A) = TRMAT_v_of_GEMAT_v (pf_A, UPLOlower, DIAGunit)
  val () = cblas_trmm<a2> (
    SIDEDIM_L
  | CblasRowMajor
  , CblasLeft
  , CblasLower
  , CblasNoTrans
  , CblasUnit
  , N, N
  , of_double<a2> (1.0)
  , !p_A, N
  , !p_A1, N
  )
  prval () = (pf_A := fpf_A (pf_A_lo_nu))
  val () = if (N <= 10) then begin
    print "A =\n"; print_matrix<a2> (A1, N, N); print_newline ()
  end // end of [val]
//
  val () =
    matrix_iforeach_fun<a2>
      (A, f, N, N) where {
    val N = size1_of_int1 (N)
    fn f (
     i: sizeLt N, j: sizeLt N, x: &a2
    ) :<> void = let
      val i = int_of_size i and j = int_of_size j
    in
      x := of_double<a2> (double_of (min (i, j) + 2))
    end // end of [f]
  } // end of [val]
//
  local
//
  in
    val N = size1_of_int1 N
    val () = gemaxpy<a2>
      (ORDERrow, ORDERrow, N, N, of_double<a2> (~1.0), !p_A, N, !p_A1, N)
    // end of [val]
    val res = gemnrm2<a1,a2> (ORDERrow, N, N, !p_A1, N)
  end // end of [local]
  val () = () where {
    extern castfn __cast {nr,nc:nat} {l:addr}
      (_: GEMAT (a2, nr, nc, row, nc) @ l | _: ptr l): ptr l
    val _ = __cast (pf_A | p_A) and _ = __cast (pf_A1 | p_A1)
  } // end of [val]
//
  val () = () where {
    extern fun __matrix_free // a hack!!!
      (A: matrix (a2, N, N)): void = "__matrix_free" 
    // end of [extern fun]  
    val () = __matrix_free (A) and () = __matrix_free (A1) 
  }
  val () = begin
    print "size = "; print N; print " and error = "; print_elt<a1> res;
    print_newline ()
  end // end of [val]
} // end of [test]

(* ****** ****** *)

(*
// HX: no longer needed
dynload "libc/DATS/complex.dats"
dynload "libats/DATS/genarrays.dats"
*)

(* ****** ****** *)

implement main () = let
  #define N 10
//
  val () = (print "test<float,float>:"; print_newline ())
  val () = test<float,float> (N)
  val () = (print "================================================"; print_newline ())
//
  val () = (print "test<double,double>:"; print_newline ())
  val () = test<double,double> (N)
  val () = (print "================================================"; print_newline ())
//
  val () = (print "test<float,ccomplx>:"; print_newline ())
  val () = test<float,ccmplx> (N)
  val () = (print "================================================"; print_newline ())
//
  val () = (print "test<double,zcomplx>:"; print_newline ())
  val () = test<double,zcmplx> (N)
  val () = (print "================================================"; print_newline ())
//
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [test_lu.dats] *)

////

/*
  File: my_lu.c

  Author: Shivkumar Chandrasekaran

  Purpose: Mimic in ATS for testing CBLAS interface

  Compile: gcc -O3 my_lu.c -L/usr/local/atlas/lib -lcblas -latlas
       or: gcc -O3 my_lu.c -L/usr/local/atlas/lib -lpthread -lptcblas -latlas
  Output: If "./a.out 100" works correctly output should be
          double: size = 100, error = 0.000000, time = 0 seconds
	  float: ...

  $Id: my_lu.c,v 1.3 2009/06/20 23:31:25 shiv Exp shiv $
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <complex.h>
#include "/usr/local/atlas/include/cblas.h"

/* real lu */
#define lu(type, stype)					    \
void lu_ ## stype (int n, int lda, type a[]) {  	    \
                                                            \
  if (n <= 1) return;                                       \
                                                            \
  /* a[1:n-1;0] = a[1:n-1;0] / a[0,0] */                    \
  {                                                         \
    int i;                                                  \
    for (i = 1; i < n; i++) a[i*lda] /= a[0];               \
  };                                                        \
                                                            \
  /* a[1:n-1, 1:n-1] -= a[1:n-1,0] * a[0,1:n-1] / a[0,0] */ \
  cblas_ ## stype ## gemm (                                 \
               CblasRowMajor, CblasNoTrans, CblasNoTrans,   \
	       n-1, n-1, 1, -1, a+lda, lda, a+1, lda,       \
	       1, a+lda+1, lda);                            \
                                                            \
  lu_ ## stype (n-1, lda, a+lda+1);     		    \
  return;                                                   \
}

/* complex lu */
#define luc(type, stype)			            \
void lu_ ## stype (int n, int lda, type a[]) {  	    \
                                                            \
  static type one = 1+0I;          			    \
  static type minus_one = -1+0I;			    \
                                                            \
  if (n <= 1) return;                                       \
                                                            \
  /* a[1:n-1;0] = a[1:n-1;0] / a[0,0] */                    \
  {                                                         \
    int i;                                                  \
    for (i = 1; i < n; i++) a[i*lda] /= a[0];               \
  };                                                        \
                                                            \
  /* a[1:n-1, 1:n-1] -= a[1:n-1,0] * a[0,1:n-1] / a[0,0] */ \
  cblas_ ## stype ## gemm (                                 \
               CblasRowMajor, CblasNoTrans, CblasNoTrans,   \
	       n-1, n-1, 1, &minus_one, a+lda, lda,         \
               a+1, lda, &one, a+lda+1, lda);		    \
                                                            \
  lu_ ## stype (n-1, lda, a+lda+1);     		    \
  return;                                                   \
}

lu(double, d)
lu(float, s)
luc(double complex, z)
luc(float complex, c)


inline int min (int i, int j) {
  if (i < j)
    return i;
  else
    return j;
}


#define test(type, stype)                                         \
void test_ ## stype (int n) {                                     \
                                                                  \
  type a[n][n];                                                   \
  clock_t cl;                                                     \
                                                                  \
  /* a[i][j] = min(i,j) + 1 */                                    \
  {                                                               \
    int i, j;                                                     \
    for (i = 0; i < n; i++)                                       \
      for (j = i; j < n; j++)                                     \
	a[i][j] = a[j][i] = i+1;                                  \
  };                                                              \
                                                                  \
  cl = clock ();                                                  \
  lu_ ## stype (n, n, (type *) a);                                \
  cl = clock () - cl;                                             \
                                                                  \
  /* a = l * u */                                                 \
  {                                                               \
    int i, j, k;                                                  \
    type tmp;                                                     \
    double res = 0;                                               \
                                                                  \
    /* upper part */                                              \
    for (i = 0; i < n; i++)                                       \
      for (j = i; j < n; j++) {                                   \
	tmp = 0;                                                  \
	for (k = 0; k < i; k++) tmp += a[i][k] * a[k][j];         \
	tmp += a[i][j];                                           \
	res += abs (tmp - min(i,j) - 1);                          \
      };                                                          \
    /* lower part */                                              \
    for (j = 0; j < n-1; j++)                                     \
      for (i = j+1; i < n; i++) {                                 \
	tmp = 0;                                                  \
	for (k = 0; k <= j; k++) tmp += a[i][k] * a[k][j];        \
	res += abs (tmp - min(i,j) - 1);                          \
      };                                                          \
                                                                  \
    printf (#type ": size = %i, error = %f, time = %i seconds\n", \
	    n, res, cl / CLOCKS_PER_SEC);                         \
  };                                                              \
                                                                  \
  return;                                                         \
}

test(double, d)
test(float, s)
test(double complex, z)
test(float complex, c)


int main (int argc, char *argv[]) {
  int n = atoi (argv[1]);
  if (argc > 1) {
    test_d (n);
    test_s (n);
    test_z (n);
    test_c (n);
  } else
    printf ("Please specify matrix dimension; for example: %s 1000\n",
	    argv[0]);
  return 0;
}
