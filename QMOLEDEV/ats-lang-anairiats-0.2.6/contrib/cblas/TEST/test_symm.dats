//
// some testing code for the ATS interface to BLAS
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

local #include "contrib/cblas/HATS/cblas.hats" in (*empty*) end

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/matrix.dats"

(* ****** ****** *)

staload "libc/SATS/complex.sats"
staload "libats/SATS/genarrays.sats"

(* ****** ****** *)

staload "contrib/cblas/SATS/cblas.sats"

(* ****** ****** *)

extern fun{a:t@ype}
mul_gemat_symat {m,n:pos} (
   m: int m, n: int n
 , A: matrix (a, m, n), B: matrix (a, n, n), C: matrix (a, m, n)
 ) : void
// end of [mul_gemat_symat]

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

implement{a}
mul_gemat_symat (m, n, A, B, C) = let
  extern castfn __cast {nr,nc:nat}
    (_: matrix (a, nr, nc)): [l:addr] (GEMAT (a, nr, nc, row, nc) @ l | ptr l)
  val (pf_A | p_A) = __cast (A)
  val (pf_B | p_B) = __cast (B)
  val (pf_C | p_C) = __cast (C)
  extern castfn __cast {nr,nc:nat} {l:addr}
    (_: GEMAT (a, nr, nc, row, nc) @ l | _: ptr l): ptr l
  val alpha: a = of_double (1.0) and beta: a = of_double (0.0)
  prval (pf_B_up, fpf_B) = SYMAT_v_of_GEMAT_v (pf_B, UPLOupper)
  val () = cblas_symm<a> (
    SIDEDIM_R
  | CblasRowMajor
  , CblasRight // side
  , CblasUpper // uplo
  , m, n
  , alpha
  , !p_B, n
  , !p_A, n
  , beta
  , !p_C, n
  )
  prval () = pf_B := fpf_B (pf_B_up)
  val _ = __cast (pf_A | p_A)  
  val _ = __cast (pf_B | p_B)  
  val _ = __cast (pf_C | p_C)    
in
  // nothing
end // end of [mul_gemat_symat]

(* ****** ****** *)

fn{a:t@ype} print_matrix {m,n:nat} (
    M: matrix (a, m, n), m: int m, n: int n
  ) : void = let
  val pr = lam
    (i: size_t, j: size_t, x: &a): void =<cloref1> let
    val _0 = size_of_int1 0 
    val () = if j > _0 then print ", "
    val () = if (i > _0 andalso j = _0) then print "\n" 
    val () = print_elt<a> (x)
  in
    // empty
  end // end of [val]  
  val () = matrix_iforeach_cloref<a> (M, pr, size1_of_int1 m, size1_of_int1 n) 
in
  // empty
end // end of [print_matrix]

(* ****** ****** *)

typedef elt = double

(* ****** ****** *)

prval pf = unit_v ()
val M1 =
  matrix_make_vclo<elt>
    (pf | 10, 10, !p_clo) where {
  var !p_clo = @lam (
      pf: !unit_v | i: size_t, j: size_t, x: &(elt?) >> elt
    ) : void =<clo> let
    val i = int_of_size i and j = int_of_size j
  in
    x := of_double<elt> (double_of_int (1 + min (i, j)))
    // x := zcmplx_make_cart (double_of i, double_of j)
  end (* end of [var] *)
}
prval unit_v () = pf

val () = print "M1=\n"
val () = print_matrix<elt> (M1, 10, 10)
val () = print_newline ()

(* ****** ****** *)

prval pf = unit_v ()
val M2 =
  matrix_make_vclo<elt>
    (pf | 10, 10, !p_clo) where {
  var !p_clo = @lam (
      pf: !unit_v | i: size_t, j: size_t, x: &(elt?) >> elt
    ) : void =<clo> let
  in  
    x := of_double<elt> (0.0)
  end (* end of [var] *)
}
prval unit_v () = pf

val () = print "M2=\n"
val () = print_matrix<elt> (M2, 10, 10)
val () = print_newline ()

val () = mul_gemat_symat (10, 10, M1, M1, M2)

val () = print "M2=\n"
val () = print_matrix<elt> (M2, 10, 10)
val () = print_newline ()

(* ****** ****** *)

implement main () = ()

(* ****** ****** *)

(* end of [symm_test.dats] *)
