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

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload "prelude/SATS/number.sats"

(* ****** ****** *)

staload "libats/SATS/fmatrix.sats"
staload GA = "libats/SATS/genarrays.sats"
sortdef inc = $GA.inc
sortdef order = $GA.order
stadef row = $GA.row
stadef col = $GA.col
sortdef uplo = $GA.uplo
stadef upper = $GA.upper
stadef lower = $GA.lower
sortdef diag = $GA.diag
stadef unit = $GA.unit
stadef nonunit = $GA.nonunit
stadef GEMAT = $GA.GEMAT
stadef GEMAT_v = $GA.GEMAT_v

(* ****** ****** *)

staload "contrib/cblas/SATS/cblas.sats"
staload "contrib/cblas/SATS/cblas_extra.sats"

(* ****** ****** *)

#define sz2i int1_of_size1

(* ****** ****** *)

//
// filling a square matrix with identity diagnal
//
implement{a}
GEMAT_ptr_initialize_eye
  {ord} {m} {lda} (ord, m, A, lda) = let
  val _0 = of_int<a> (0)
  val () =
    $GA.GEMAT_ptr_initialize_elt<a> (ord, A, m, m, lda, _0)
  // end of [val]
  fun loop {i:nat | i <= m} .<m-i>.
    (A: &GEMAT (a, m, m, ord, lda), i: size_t i, _1: a):<cloref> void =
    if i < m then begin
      $GA.GEMAT_ptr_set_elt_at<a> (ord, A, lda, i, i, _1); loop (A, i+1, _1)
    end // end of [if]
  // end of [loop]
  val _1 =  of_int<a> (1)
in
  loop (A, 0, _1)
end // end of [GEMAT_ptr_initialize_eye]

(* ****** ****** *)

(*
** Y <- alpha * X + Y
*)
implement{a}
GEMAT_axpy // generalizing [cblas_axpy]
  {ord} {m,n} {ldx,ldy}
  (ord, m, n, alpha, X, ldx, Y, ldy) = let
  fun loop_row {lx,ly:addr} {m,n:nat} .<m>. (
      pfX_gmat: !GEMAT_v (a, m, n, row, ldx, lx)
    , pfY_gmat: !GEMAT_v (a, m, n, row, ldy, ly)
    | pX: ptr lx, pY: ptr ly, m: size_t m, n: size_t n
    ) :<cloref> void =
    if m > 0 then let
      prval (pfX_inc, pfX_gvec, fpfX_gmat) = $GA.GEMAT_v_uncons_row (pfX_gmat)
      prval $GA.MATVECINCrowrow () = pfX_inc
      prval (pfY_inc, pfY_gvec, fpfY_gmat) = $GA.GEMAT_v_uncons_row (pfY_gmat)
      prval $GA.MATVECINCrowrow () = pfY_inc
      val () = cblas_axpy<a> (sz2i n, alpha, !pX, 1(*incX*), !pY, 1(*incY*))
      prval () = pfX_gmat := fpfX_gmat (pfX_gvec)
      prval () = pfY_gmat := fpfY_gmat (pfY_gvec)
      val (pfX1_gmat, pfX2_gmat, fpfX_gmat | pX1, pX2) =
        $GA.GEMAT_ptr_split2x1<a> (pfX_gmat | $GA.ORDERrow, pX, ldx, 1)
      val (pfY1_gmat, pfY2_gmat, fpfY_gmat | pY1, pY2) =
        $GA.GEMAT_ptr_split2x1<a> (pfY_gmat | $GA.ORDERrow, pY, ldy, 1)
      val () = loop_row (pfX2_gmat, pfY2_gmat | pX2, pY2, m-1, n)
      prval () = pfX_gmat := fpfX_gmat (pfX1_gmat, pfX2_gmat)
      prval () = pfY_gmat := fpfY_gmat (pfY1_gmat, pfY2_gmat)
    in
      // nothing
    end // no else // end of [if]
in
  case+ ord of
  | $GA.ORDERrow () => loop_row (view@ X, view@ Y | &X, &Y, m, n)
  | $GA.ORDERcol () => let
      prval $GA.TRANORDcolrow () = $GA.GEMAT_v_trans {a} {col} (view@ X)
      prval $GA.TRANORDcolrow () = $GA.GEMAT_v_trans {a} {col} (view@ Y)
      val () = loop_row (view@ X, view@ Y | &X, &Y, n, m)
      prval $GA.TRANORDrowcol () = $GA.GEMAT_v_trans {a} {row} (view@ X)
      prval $GA.TRANORDrowcol () = $GA.GEMAT_v_trans {a} {row} (view@ Y)
    in
      // nothing
    end // end of [ORDERcol]
end // end of [GEMAT_axpy]

(* ****** ****** *)

(*
** A <- diag(D) * A
*)
implement{a1,a2}
GEMAT_scal_row
  {ord} {m,n} {lda}
  (ord, m, n, D, A, lda) = let
  fun scal_arr_arr {m,i:nat | i <= m} .<m-i>. (
      D: &(@[a2][m]), A: &(@[a1][m]), m: size_t m, i: size_t i
    ) :<> void =
    if i < m then begin
      A[i] := scal<a2,a1> (D[i], A[i]); scal_arr_arr (D, A, m, i+1)
    end
  // end of [scal_arr_arr]
  fun loop_col {m,n:nat} {la:addr} .<n>. (
      pf_gmat: !GEMAT_v (a1, m, n, col, lda, la)
    | D: &(@[a2][m]), pA: ptr la, lda: size_t lda, m: size_t m, n: size_t n
    ) :<> void =
    if n > 0 then let
      val (pfA1_gmat, pfA2_gmat, fpf_gmat | pA1, pA2) =
        $GA.GEMAT_ptr_split1x2<a1> (pf_gmat | $GA.ORDERcol, pA, lda, 1)
      prval (pf2_inc, pfA1_gvec, fpfA1_gmat) = $GA.GEVEC_v_of_GEMAT_v_col (pfA1_gmat)
      prval $GA.MATVECINCcolcol () = pf2_inc
      prval pfA1_arr = $GA.array_v_of_GEVEC_v (pfA1_gvec)
      val () = scal_arr_arr (D, !pA1, m, 0)
      prval () = pfA1_gvec := $GA.GEVEC_v_of_array_v (pfA1_arr)
      prval () = pfA1_gmat := fpfA1_gmat (pfA1_gvec)
      val () = loop_col (pfA2_gmat | D, pA2, lda, m, n-1)
      prval () = pf_gmat := fpf_gmat {a1} (pfA1_gmat, pfA2_gmat)
    in
      // nothing
    end // end of [if]
  // end of [loop_col]
in
  case+ ord of
  | $GA.ORDERrow () => let
       prval $GA.TRANORDrowcol () = $GA.GEMAT_v_trans {a1} {row} (view@ A)
       val () = GEMAT_scal_col ($GA.ORDERcol, n, m, A, lda, D)
       prval $GA.TRANORDcolrow () = $GA.GEMAT_v_trans {a1} {col} (view@ A)
    in
      // nothing
    end // end of [ORDERrow]
  | $GA.ORDERcol () => loop_col (view@ A | D, &A, lda, m, n)
end // end of [GEMAT_scal_row]

(* ****** ****** *)

(*
** A <- A * diag(D)
*)
implement{a1,a2}
GEMAT_scal_col
  {ord} {m,n} {lda}
  (ord, m, n, A, lda, D) = let
(*
** scal_arr_num (A, alpha, m) = cblas_scal (alpha, A, m, 1(*inc*))
*)
  fun scal_arr_num {m:nat} {la:addr} .<m>. (
      pfA: !array_v (a1, m, la) | pA: ptr la, alpha: a2, m: size_t m
    ) :<> void =
    if m > 0 then let
      prval (pfA1, pfA2) = array_v_uncons {a1} (pfA)
      val () = !pA := scal<a2,a1> (alpha, !pA)
      val () = scal_arr_num (pfA2 | pA+sizeof<a1>, alpha, m-1)
      prval () = pfA := array_v_cons {a1} (pfA1, pfA2)
    in
      // nothing
    end // end of [if]
  // end of [scal_arr_num]
  fun loop_col {m,n:nat} {la,ld:addr} .<n>. (
      pf_gmat: !GEMAT_v (a1, m, n, col, lda, la), pfD: !array_v (a2, n, ld)
    | pA: ptr la, pD: ptr ld, m: size_t m, n: size_t n
    ) :<cloref> void =
    if n > 0 then let
      val (pfA1_gmat, pfA2_gmat, fpf_gmat | pA1, pA2) =
        $GA.GEMAT_ptr_split1x2<a1> (pf_gmat | $GA.ORDERcol, pA, lda, 1)
      prval (pf2_inc, pfA1_gvec, fpfA1_gmat) = $GA.GEVEC_v_of_GEMAT_v_col (pfA1_gmat)
      prval $GA.MATVECINCcolcol () = pf2_inc
      prval pfA1_arr = $GA.array_v_of_GEVEC_v {a1} (pfA1_gvec)
      prval (pfD1, pfD2) = array_v_uncons {a2} (pfD)
      val () = scal_arr_num (pfA1_arr | pA1, !pD, m)
      prval () = pfA1_gvec := $GA.GEVEC_v_of_array_v (pfA1_arr)
      prval () = pfA1_gmat := fpfA1_gmat (pfA1_gvec)
      val () = loop_col (pfA2_gmat, pfD2 | pA2, pD+sizeof<a2>, m, n-1)
      prval () = pf_gmat := fpf_gmat {a1} (pfA1_gmat, pfA2_gmat)
      prval () = pfD := array_v_cons {a2} (pfD1, pfD2)
    in
      // nothing
    end // end of [if]
  // end of [loop_col]
in
  case+ ord of
  | $GA.ORDERrow () => let
       prval $GA.TRANORDrowcol () = $GA.GEMAT_v_trans {a1} {row} (view@ A)
       val () = GEMAT_scal_row ($GA.ORDERcol, n, m, D, A, lda)
       prval $GA.TRANORDcolrow () = $GA.GEMAT_v_trans {a1} {col} (view@ A)
    in
      // nothing
    end // end of [ORDERrow]
  | $GA.ORDERcol () => loop_col (view@ A, view@ D | &A, &D, m, n)
end // end of [GEMAT_scal_col]

(* ****** ****** *)

(* end of [cblas_extra.dats] *)
