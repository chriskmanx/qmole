(*
**
** Some functions provided for convenience
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

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"

(* ****** ****** *)
//
// SC: Fill square matrix with identity diagnal
//
fun{a:t@ype}
GEMAT_ptr_initialize_eye
  {ord:order} {m:nat} {lda:inc} (
  ord: ORDER ord, m: size_t m
, A: &GEMAT (a?, m, m, ord, lda) >> GEMAT (a, m, m, ord, lda)
, lda: size_t lda
) :<> void // end of [GEMAT_ptr_initialize_eye]

(* ****** ****** *)
//
// Y <- alpha * X + Y
//
fun{a:t@ype} GEMAT_axpy
  {ord:order} {m,n:nat} {ldx,ldy:inc} (
  ord: ORDER ord
, m: size_t m, n: size_t n
, alpha: a
, X: &GEMAT (a, m, n, ord, ldx), ldx: size_t ldx
, Y: &GEMAT (a, m, n, ord, ldy), ldy: size_t ldy
) :<> void // end of [GEMAT_axpy]

(* ****** ****** *)
//
// A <- diag(D) * A
//
fun{a1,a2:t@ype} // |a1| = a2
GEMAT_scal_row {ord:order} {m,n:nat} {lda:inc} (
  ord: ORDER ord, m: size_t m, n: size_t n
, D: &(@[a2][m]), A: &GEMAT (a1, m, n, ord, lda), lda: size_t lda
) :<> void // end of [[GEMAT_scal_row]

(* ****** ****** *)
//
// A <- A * diag(D)
//
fun{a1,a2:t@ype} // |a1| = a2
GEMAT_scal_col {ord:order} {m,n:nat} {lda:inc} (
  ord: ORDER ord, m: size_t m, n: size_t n
, A: &GEMAT (a1, m, n, ord, lda), lda: size_t lda, D: &(@[a2][n])
) :<> void // end of [[GEMAT_scal_col]

(* ****** ****** *)

(* end of [cblas_extra.sats] *)
