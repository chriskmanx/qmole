(*
**
** An interface for ATS to interact with LAPACK
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

%{#
#include "contrib/clapack/CATS/clapack.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "contrib/clapack/SATS/f2c.sats"

(* ****** ****** *)

staload "libc/SATS/complex.sats"
staload GA = "libats/SATS/genarrays.sats"

//

sortdef inc = $GA.inc // sort for leading dimensions

//

sortdef order = $GA.order
stadef row = $GA.row
stadef col = $GA.col

//

sortdef uplo = $GA.uplo
stadef upper = $GA.upper
stadef lower = $GA.lower

//

sortdef side = $GA.side
stadef left = $GA.left
stadef right = $GA.right

stadef sidedim_p = $GA.sidedim_p

//

sortdef transpose = $GA.transpose
stadef TPN = $GA.TPN
stadef TPT = $GA.TPT
stadef TPC = $GA.TPC

stadef trandim_p = $GA.trandim_p

//

sortdef diag = $GA.diag
stadef unit = $GA.unit()
stadef nonunit = $GA.nonunit()

(* ****** ****** *)

(*
** for column-major generic matrices:
*)
viewtypedef GEMAT
  (a:viewt@ype, m:int, n:int, lda:int) =
  $GA.GEMAT (a, m, n, col, lda)
// end of [GEMAT]

viewtypedef TRMAT
  (a:viewt@ype, n:int, ul: uplo, dg: diag, lda: int) =
  $GA.TRMAT (a, n, col, ul, dg, lda)
// end of [TRMAT]

(* ****** ****** *)

abst@ype CLAPACK_UPLO_t (uplo) = char

macdef ClapackUpper = $extval (CLAPACK_UPLO_t upper, "'U'")
macdef ClapackLower = $extval (CLAPACK_UPLO_t lower, "'L'")

abst@ype CLAPACK_UPLO_OPT_t = char (* upper, lower, none *)

macdef ClapackULN_U = $extval (CLAPACK_UPLO_OPT_t, "'U'")
macdef ClapackULN_L = $extval (CLAPACK_UPLO_OPT_t, "'L'")
macdef ClapackULN_N = $extval (CLAPACK_UPLO_OPT_t, "'N'")

(* ****** ****** *)

abst@ype CLAPACK_SIDE_t (side) = char

macdef ClapackLeft = $extval (CLAPACK_SIDE_t left, "'L'")
macdef ClapackRight = $extval (CLAPACK_SIDE_t right, "'R'")

(* ****** ****** *)

abst@ype CLAPACK_TRANSPOSE_t (transpose) = char

macdef ClapackNoTrans = $extval (CLAPACK_TRANSPOSE_t TPN, "'N'")
macdef ClapackTrans = $extval (CLAPACK_TRANSPOSE_t TPT, "'T'")
macdef ClapackConjTrans = $extval (CLAPACK_TRANSPOSE_t TPC, "'C'")

(* ****** ****** *)

abst@ype CLAPACK_DIAG_t (diag) = char
macdef ClapackUnit = $extval (CLAPACK_DIAG_t(unit), "'U'")
macdef ClapackNonUnit = $extval (CLAPACK_DIAG_t(nonunit), "'N'")

(* ****** ****** *)

abst@ype CLAPACK_NORM_t (char) = char
typedef
CLAPACK_NORM_t = [c:char] CLAPACK_NORM_t (c)

macdef ClapackNormMax = $extval (CLAPACK_NORM_t 'M', "'M'") // max of elt

macdef ClapackNormInf = $extval (CLAPACK_NORM_t 'I', "'I'") // max of row sums

macdef ClapackNormOne = $extval (CLAPACK_NORM_t 'O', "'O'") // max of col sums

// Frobenius: sqrt (sum of all elements squared)
macdef ClapackNormFrob = $extval (CLAPACK_NORM_t 'F', "'F'")

(* ****** ****** *)

(*
/*  CMACH   (input) CHARACTER*1 */
/*          Specifies the value to be returned by SLAMCH: */
/*          = 'E' or 'e',   SLAMCH := eps */
/*          = 'S' or 's ,   SLAMCH := sfmin */
/*          = 'B' or 'b',   SLAMCH := base */
/*          = 'P' or 'p',   SLAMCH := eps*base */
/*          = 'N' or 'n',   SLAMCH := t */
/*          = 'R' or 'r',   SLAMCH := rnd */
/*          = 'M' or 'm',   SLAMCH := emin */
/*          = 'U' or 'u',   SLAMCH := rmin */
/*          = 'L' or 'l',   SLAMCH := emax */
/*          = 'O' or 'o',   SLAMCH := rmax */

/*          where */

/*          eps   = relative machine precision */
/*          sfmin = safe minimum, such that 1/sfmin does not overflow */
/*          base  = base of the machine */
/*          prec  = eps*base */
/*          t     = number of (base) digits in the mantissa */
/*          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise */
/*          emin  = minimum exponent before (gradual) underflow */
/*          rmin  = underflow threshold - base**(emin-1) */
/*          emax  = largest exponent before overflow */
/*          rmax  = overflow threshold  - (base**emax)*(1-eps) */
*)

// lamach: S, D
fun{t:t@ype} lamch (cmach: char): t

fun slamch (c: char):<> real = "atsctrb_clapack_slamch"
fun dlamch (c: char):<> doublereal = "atsctrb_clapack_dlamch"

(*

//
// This is fine, too, but it is likely to be unnecessary ... (HX)
//
sortdef char_lamch = { c : char
 |  c == 'E' || c == 'e'
 || c == 'S' || c == 's'
 || c == 'B' || c == 'b'
 || c == 'P' || c == 'p'
 || c == 'N' || c == 'n'
 || c == 'R' || c == 'r'
 || c == 'M' || c == 'm'
 || c == 'U' || c == 'u'
 || c == 'L' || c == 'l'
 || c == 'O' || c == 'o'
}

// lamach: S, D // always returns a double!
fun{t:t@ype} lamch {c:char_lamch} (cmach: char c): t

// slamch returns a double!
fun slamch {c:char_lamch} (c: char c): doublereal = "atsctrb_clapack_slamch"
fun dlamch {c:char_lamch} (c: char c): doublereal = "atsctrb_clapack_dlamch"

*)

fun{t:t@ype} lamch_eps   ():<> t
fun{t:t@ype} lamch_sfmin ():<> t
fun{t:t@ype} lamch_base  ():<> t
fun{t:t@ype} lamch_prec  ():<> t
fun{t:t@ype} lamch_t     ():<> t
fun{t:t@ype} lamch_rnd   ():<> t
fun{t:t@ype} lamch_emin  ():<> t
fun{t:t@ype} lamch_rmin  ():<> t
fun{t:t@ype} lamch_emax  ():<> t
fun{t:t@ype} lamch_rmax  ():<> t

(* ****** ****** *)

// lange: S, D, C, Z

(*

// taken from [clapack.h]
doublereal dlange_(
  char *norm
, integer *m, integer *n
, doublereal *a, integer *lda
, doublereal *work
) ;

*)

dataview langework_v
  (t:t@ype+, c:char, m:int, l:addr) =
  | {c == 'I'} langework_v_some (t, c, m, l) of array_v (t?, m, l)
  | {c <> 'I'} langework_v_none (t, c, m, l) of ()
// end of [langework_v]

// |t1| = t2
typedef
lange_type
  (t1:t@ype, t2:t@ype) =
  {c:char} {m,n:nat} {lda:inc}
  {m1:pos | m <= m1} {l_work:addr} (
    (*pf: *) !langework_v (t2, c, m1, l_work)
  | (*norm: *) CLAPACK_NORM_t c
  , (*m:*) integer m, (*n:*) integer n
  , (*a:*) &GEMAT (t1, m, n, lda), (*lda:*) integer lda
  , (*p_work:*) ptr l_work
  ) -<fun> t2
// end of [lange_type]

fun{t1,t2:t@ype} lange: lange_type (t1, t2)
fun slange: lange_type (real, real) = "atsctrb_clapack_slange"
fun dlange: lange_type (doublereal, doublereal) = "atsctrb_clapack_dlange"
fun clange: lange_type (complex, real) = "atsctrb_clapack_clange"
fun zlange: lange_type (doublecomplex, doublereal) = "atsctrb_clapack_zlange"

(* ****** ****** *)

// |t1| = t2
fun{t1,t2:t@ype} lange_inf
  {m,n:nat} {lda:inc} {m1:pos | m <= m1} (
    m: integer m, n: integer n
  , a: &GEMAT (t1, m, n, lda), lda: integer lda
  , work: &(@[t2?][m1])
  ) :<> t2
// end of [lange_inf]

// |t1| = t2
fun{t1,t2:t@ype} lange_one {m,n:nat} {lda:inc} (
    m: integer m, n: integer n, a: &GEMAT (t1, m, n, lda), lda: integer lda
  ) :<> t2
// [end of [lange_one]

// |t1| = t2
fun{t1,t2:t@ype} lange_max {m,n:nat} {lda:inc} (
    m: integer m, n: integer n, a: &GEMAT (t1, m, n, lda), lda: integer lda
  ) :<> t2
// [end of [lange_max]

// |t1| = t2
fun{t1,t2:t@ype} lange_frob {m,n:nat} {lda:inc} (
    m: integer m, n: integer n, a: &GEMAT (t1, m, n, lda), lda: integer lda
  ) :<> t2
// [end of [lange_frob]

(* ****** ****** *)

//
// lacpy: S, D, C, Z
//

(*

// taken from [clapack.h]
/* Subroutine */
int dlacpy_(
  char *uplo
, integer *m, integer *n
, doublereal *a, integer *lda
, doublereal *b, integer *ldb
) ;

*)

typedef
lacpy_type (t:t@ype) =
  {m,n:nat} {lda,ldb:inc} (
  (*uln:*) CLAPACK_UPLO_OPT_t
, (*m:*) integer m, (*n:*) integer n
, (*a:*) &GEMAT (t, m, n, lda), (*lda:*) integer lda
, (*b:*) &GEMAT (t, m, n, ldb), (*ldb:*) integer ldb
) -<fun> void
// end of [typedef]

fun{t:t@ype} lacpy: lacpy_type (t)
fun slacpy: lacpy_type (real) = "atsctrb_clapack_slacpy"
fun dlacpy: lacpy_type (doublereal) = "atsctrb_clapack_dlacpy"
fun clacpy: lacpy_type (complex) = "atsctrb_clapack_clacpy"
fun zlacpy: lacpy_type (doublecomplex) = "atsctrb_clapack_zlacpy"

(* ****** ****** *)

absview LQMAT_err_v (
  a:t@ype+, m:int, n:int, k:int, lda: int, la:addr, ltau:addr
, err:int
) // end of [absview]

prfun LQMAT_err_v_elim
  {a:t@ype}
  {m,n,k:int} {lda:int} {la,ltau:addr}
  {err:int} (
    pflq: LQMAT_err_v (a, m, n, k, lda, la, ltau, err)
  ) : (
    GEMAT (a?, m, n, lda) @ la, array_v (a, k, ltau)
  )
// end of [LQMAT_err_v_elim]

viewdef LQMAT_v (
  a: t@ype, m: int, n: int, k:int, lda: int, la:addr, ltau:addr
) = LQMAT_err_v (a, m, n, k, lda, la, ltau, 0(*succ*))

prfun TRMAT_v_of_LQMAT_v {a:t@ype}
  {m,n,k:nat | m <= n} {lda:inc} {la,ltau:addr} (
    pf_lqmat: LQMAT_v (a, m, n, k, lda, la, ltau)
  ) :<prf> (
    TRMAT (a, m, lower, nonunit, lda) @ la
  , TRMAT (a, m, lower, nonunit, lda) @ la -<lin,prf> LQMAT_v (a, m, n, k, lda, la, ltau)
  )
// end of [TRMAT_v_of_LQMAT_v]

(* ****** ****** *)

//
// gelqf: S, D, C, Z
//

(*

// taken from [clapack.h]
/* Subroutine */
int dgelqf_(
  integer *m, integer *n
, doublereal *a, integer *lda
, doublereal *tau
, doublereal *work, integer *lwork
, integer *info
) ;

*)

typedef
gelqf_type (t:t@ype) =
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la,ltau:addr}
  {lwork:pos | lwork >= m} (
  GEMAT (t, m, n, lda) @ la    // a
, array_v (t?, mn, ltau) // tau
| integer m
, integer n
, ptr la, integer lda          // a, lda
, ptr ltau                     // tau
, &(@[t?][lwork]), integer lwork
) -<fun> [err:int] (
  LQMAT_err_v (t, m, n, mn, lda, la, ltau, err) | int err
) // end of [gelqf_type]

fun{t:t@ype} gelqf: gelqf_type (t)
fun sgelqf: gelqf_type (real) = "atsctrb_clapack_sgelqf"
fun dgelqf: gelqf_type (doublereal) = "atsctrb_clapack_dgelqf"
fun cgelqf: gelqf_type (complex) = "atsctrb_clapack_cgelqf"
fun zgelqf: gelqf_type (doublecomplex) = "atsctrb_clapack_zgelqf"

fun{t:t@ype} gelqf_exn
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la,ltau:addr}
  {lwork:pos | lwork >= m} (
  pfa: GEMAT (t, m, n, lda) @ la      // a
, pftau: array_v (t?, mn, ltau) // tau
| m: integer m
, n: integer n
, la: ptr la, lda: integer lda        // a, lda
, ltau: ptr ltau                      // tau
, work: &(@[t?][lwork]), lwork: integer lwork
) :<> [err:int] (
  LQMAT_v (t, m, n, mn, lda, la, ltau) | void
) // end of [gelqf_exn]

fun{t:t@ype} gelqf_work_query
  {m,n:nat} (m: integer m, n: integer n)
  :<> [lwork:int | lwork >= m] integer (lwork)

(* ****** ****** *)

//
// ormlq: S, D and unmlq: C, Z
//

(*

/* Subroutine */
int sormlq_(
  char *side
, char *trans
, integer *m, integer *n, integer *k
, real *a, integer *lda
, real *tau
, real *c, integer *ldc
, real *work, integer *lwork
, integer *info
) ;

// taken from [clapack.h]
/* Subroutine */
int cunmlq_(
  char *side
, char *trans
, integer *m, integer *n, integer *k
, complex *a, integer *lda
, complex *tau
, complex *c__, integer *ldc
, complex *work, integer *lwork
, integer *info
) ;

*)

typedef
unmlq_type (t:t@ype) =
  {m,n,k:nat}
  {ma,na:int} // k <= min(ma,na)
  {lda,ldc:inc}
  {lr:side} {tr:transpose}
  {lwork:pos | lwork >= m+n-na}
  {la,ltau:addr} (
  (*side:*) sidedim_p (lr, m, n, na)
, (*lqmat:*) !LQMAT_v (t, ma, na, k, lda, la, ltau)
| (*side:*) CLAPACK_SIDE_t lr
, (*trans:*) CLAPACK_TRANSPOSE_t tr
, (*m:*) integer m, (*n:*) integer n, (*k:*) integer k
, (*a:*) ptr la, (*lda:*) integer lda, (*tau:*) ptr ltau
, (*c__:*) &GEMAT (t, m, n, ldc), (*ldc:*) integer ldc
, (*work:*) &(@[t?][lwork]), (*lwork:*) integer lwork
) -<fun> int // end of [unmlq_type]

fun{t:t@ype} unmlq: unmlq_type (t)
fun cunmlq: unmlq_type (complex) = "atsctrb_clapack_cunmlq"
fun zunmlq: unmlq_type (doublecomplex) = "atsctrb_clapack_zunmlq"

fun{t:t@ype} unmlq_work_query
  {m,n,k:pos}
  {na:nat | k <= na}
  {lr:side}
  {tr:transpose} (
  pf: sidedim_p (lr, m, n, na)
| side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
, m: integer m, n: integer n, k: integer k
) :<> [lwork:pos | lwork >= m+n-na] integer (lwork)
// end of [unmlq_work_query]

(* ****** ****** *)

stadef ormlq_type = unmlq_type
fun{t:t@ype} ormlq: ormlq_type (t)
fun sormlq: ormlq_type (real) = "atsctrb_clapack_sormlq"
fun dormlq: ormlq_type (doublereal) = "atsctrb_clapack_dormlq"

fun{t:t@ype} ormlq_work_query
  {m,n,k:pos}
  {na:nat | k <= na}
  {lr:side}
  {tr:transpose} (
  pf: sidedim_p (lr, m, n, na)
| side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
, m: integer m, n: integer n, k: integer k
) :<> [lwork:pos | lwork >= m+n-na] integer (lwork)
// end of [ormlq_work_query]

(* ****** ****** *)

absview QLMAT_err_v (
  a: t@ype, m: int, n: int, k:int, lda: int, la:addr, ltau:addr
, err:int
) // end of [absview]

prfun QLMAT_err_v_elim
  {a:t@ype}
  {m,n,k:int} {lda:int} {la,ltau:addr}
  {err:int} (
  pflq: QLMAT_err_v (a, m, n, k, lda, la, ltau, err)
) : (
  GEMAT (a?, m, n, lda) @ la, array_v (a, k, ltau)
) // end of [QLMAT_err_v_elim]

viewdef QLMAT_v (
  a: t@ype, m: int, n: int, k:int, lda: int, la:addr, ltau:addr
) = QLMAT_err_v (a, m, n, k, lda, la, ltau, 0(*succ*))

prfun TRMAT_v_of_QLMAT_v
  {a:t@ype} {m,n,k:nat | m >= n}
  {lda:inc} {la,ltau:addr} {ofs:int} (
  pf_mul: MUL (m-n, sizeof a, ofs)
, pf_qlmat: QLMAT_v (a, m, n, k, lda, la, ltau)
) :<prf> (
  TRMAT (a, n, lower, nonunit, lda) @ la+ofs
, TRMAT (a, n, lower, nonunit, lda) @ la+ofs -<lin,prf> QLMAT_v (a, m, n, k, lda, la, ltau)
) // end of [TRMAT_v_of_QLMAT_v]

fun{a:t@ype}
  TRMAT_of_QLMAT
  {m,n,k:nat | m >= n}
  {lda:inc} {la,ltau:addr} (
  pf_qlmat: QLMAT_v (a, m, n, k, lda, la, ltau)
| m: integer m, n: integer n, pa: ptr la
) :<> [la_ofs:addr] ( // la_ofs: la + ofs
  TRMAT (a, n, lower, nonunit, lda) @ la_ofs
, TRMAT (a, n, lower, nonunit, lda) @ la_ofs -<lin,prf> QLMAT_v (a, m, n, k, lda, la, ltau)
| ptr la_ofs
) // end of [TRMAT_of_QLMAT]

(* ****** ****** *)

// geqlf: S, D, C, Z

(*

// taken from [clapack.h]
/* Subroutine */
int dgeqlf_(
  integer *m, integer *n
, doublereal *a, integer *lda
, doublereal *tau
, doublereal *work, integer *lwork
, integer *info
) ;

*)

typedef
geqlf_type (t:t@ype) =
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la,ltau:addr}
  {lwork:pos | lwork >= n} (
  (*a:*) GEMAT (t, m, n, lda) @ la
, (*tau*) array_v (t?, mn, ltau)
| (*m:*) integer m, (*n:*) integer n
, (*a:*) ptr la, (*lda:*) integer lda, (*tau:*) ptr ltau
, (*work:*) &(@[t?][lwork]), (*lwork:*) integer lwork
) -<fun> [err:int] (
  QLMAT_err_v (t, m, n, mn, lda, la, ltau, err) | int err
) // end of [geqlf_type]

fun{t:t@ype} geqlf: geqlf_type (t)
fun sgeqlf: geqlf_type (real) = "atsctrb_clapack_sgeqlf"
fun dgeqlf: geqlf_type (doublereal) = "atsctrb_clapack_dgeqlf"
fun cgeqlf: geqlf_type (complex) = "atsctrb_clapack_cgeqlf"
fun zgeqlf: geqlf_type (doublecomplex) = "atsctrb_clapack_zgeqlf"

fun{t:t@ype} geqlf_exn
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la,ltau:addr}
  {lwork:pos | lwork >= n} (
  pfa: GEMAT (t, m, n, lda) @ la
, pftau: array_v (t, mn, ltau)
| m: integer m, n: integer n
, a: ptr la, lda: integer lda, tau: ptr ltau
, work: &(@[t?][lwork]), lwork: integer lwork
) :<!exn> (
  QLMAT_v (t, m, n, mn, lda, la, ltau) | void
) // end of [geqlf_exn]

fun{t:t@ype}
geqlf_work_query
  {m,n:nat} (
  m: integer m, n: integer n
) :<> [lwork:int | lwork >= n] integer (lwork)

(* ****** ****** *)

//
// ormql: S, D and unmql: C, Z
//

(*

// taken from [clapack.h]
/* Subroutine */
int cunmql_(
  char *side
, char *trans
, integer *m, integer *n, integer *k
, complex *a, integer *lda
, complex *tau
, complex *c__, integer *ldc
, complex *work, integer *lwork
, integer *info
) ;

*)

typedef
unmql_type (t:t@ype) =
  {m,n,k:nat}
  {ma,na:int} // k <= min (ma,na)
  {lda,ldc:inc}
  {lr:side} {tr:transpose}
  {lwork:pos | lwork >= m+n-ma}
  {la,ltau:addr} (
  (*pf:*) sidedim_p (lr, m, n, ma)
, (*qlmat*) !QLMAT_v (t, ma, na, k, lda, la, ltau)
| (*side:*) CLAPACK_SIDE_t lr
, (*trans:*) CLAPACK_TRANSPOSE_t tr
, (*m:*) integer m, (*n:*) integer n, (*k:*) integer k
, (*a:*) ptr la, (*lda:*) integer lda, (*tau:*) ptr ltau
, (*c__:*) &GEMAT (t, m, n, ldc), (*ldc:*) integer ldc
, (*work:*) &(@[t?][lwork]), (*lwork:*) integer lwork
) -<fun> int // end of [unmql_type]

stadef ormql_type = unmql_type

fun{t:t@ype} ormql: ormql_type (t)
fun sormql: ormql_type (real) = "atsctrb_clapack_sormql"
fun dormql: ormql_type (doublereal) = "atsctrb_clapack_dormql"

fun{t:t@ype} unmql: unmql_type (t)
fun cunmql: unmql_type (complex) = "atsctrb_clapack_cunmql"
fun zunmql: unmql_type (doublecomplex) = "atsctrb_clapack_zunmql"

fun{t:t@ype} ormql_work_query
  {m,n,k:pos}
  {ma:nat | k <= ma}
  {lr:side}
  {tr:transpose} (
  pf: sidedim_p (lr, m, n, ma)
| side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
, m: integer m, n: integer n, k: integer k
) :<> [lwork:pos | lwork >= m+n-ma] integer (lwork)
// end of [ormql_work_query]

fun{t:t@ype} unmql_work_query
  {m,n,k:pos}
  {ma:nat | k <= ma}
  {lr:side}
  {tr:transpose} (
  pf: sidedim_p (lr, m, n, ma)
| side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
, m: integer m, n: integer n, k: integer k
) :<> [lwork:pos | lwork >= m+n-ma] integer (lwork)
// end of [unmql_work_query]

(* ****** ****** *)

absview QRMAT_err_v (
  a: t@ype, m: int, n: int, k:int, lda: int, la:addr, ltau:addr
, err:int
) // end of [absview]

prfun QRMAT_err_v_elim
  {a:t@ype}
  {m,n,k:int} {lda:int} {la,ltau:addr}
  {err:int} (
  pfqr: QRMAT_err_v (a, m, n, k, lda, la, ltau, err)
) : (
  GEMAT (a?, m, n, lda) @ la, array_v (a, k, ltau)
) // end of [QRMAT_err_v_elim]

viewdef QRMAT_v (
  a: t@ype, m: int, n: int, k:int, lda: int, la:addr, ltau:addr
) = QRMAT_err_v (a, m, n, k, lda, la, ltau, 0(*succ*))

prfun TRMAT_v_of_QRMAT_v {a:t@ype}
  {m,n,k:nat | m >= n} {lda:inc} {la,ltau:addr} (
  pf_qrmat: QRMAT_v (a, m, n, k, lda, la, ltau)
) :<prf> (
  $GA.TRMAT_v (a, n, col, upper, nonunit, lda, la)
, $GA.TRMAT_v (a, n, col, upper, nonunit, lda, la) -<lin,prf>
  QRMAT_v (a, m, n, k, lda, la, ltau)
) // end of [TRMAT_v_of_QRMAT_v]

(* ****** ****** *)

// geqrf: S, D, C, Z

(*

/* Subroutine */
int dgeqrf_(
  integer *m, integer *n
, doublereal *a, integer *lda
, doublereal *tau
, doublereal *work, integer *lwork
, integer *info
) ;

*)

typedef
geqrf_type (t:t@ype) =
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la,ltau:addr}
  {lwork:pos | lwork >= n} (
  GEMAT (t, m, n, lda) @ la    // a
, array_v (t?, mn, ltau) // tau
| integer m
, integer n
, ptr la, integer lda         // a, lda
, ptr ltau                    // tau
, &(@[t?][lwork]), integer lwork
) -<fun> [err:int] (
  QRMAT_err_v (t, m, n, mn, lda, la, ltau, err) | int err
) // end of [geqrf_type]

fun{t:t@ype} geqrf: geqrf_type (t)
fun sgeqrf: geqrf_type (real) = "atsctrb_clapack_sgeqrf"
fun dgeqrf: geqrf_type (doublereal) = "atsctrb_clapack_dgeqrf"
fun cgeqrf: geqrf_type (complex) = "atsctrb_clapack_cgeqrf"
fun zgeqrf: geqrf_type (doublecomplex) = "atsctrb_clapack_zgeqrf"

fun{t:t@ype} geqrf_exn
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la,ltau:addr}
  {lwork:pos | lwork >= n} (
  pfa: GEMAT (t, m, n, lda) @ la
, pftau: array_v (t, mn, ltau)
| m: integer m, n: integer n
, a: ptr la, lda: integer lda, tau: ptr ltau
, work: &(@[t?][lwork]), lwork: integer lwork
) : (
  QRMAT_v (t, m, n, mn, lda, la, ltau) | void
) // end of [geqrf_exn]

fun{t:t@ype} geqrf_work_query {m,n:nat}
  (m: integer m, n: integer n)
  :<> [lwork:int | lwork >= n] integer lwork

(* ****** ****** *)

typedef
unmqr_type (t:t@ype) =
  {m,n,k:nat}
  {ma,na:int} // k <= min (ma, na)
  {lda,ldc:inc}
  {lr:side} {tr:transpose}
  {lwork:pos | lwork >= m+n-ma}
  {la,ltau:addr} (
  (*pf:*) sidedim_p (lr, m, n, ma)
, (*qrmat*) !QRMAT_v (t, ma, na, k, lda, la, ltau)
| (*side:*) CLAPACK_SIDE_t lr
, (*trans:*) CLAPACK_TRANSPOSE_t tr
, (*m:*) integer m, (*n:*) integer n, (*k:*) integer k
, (*a:*) ptr la, (*lda:*) integer lda, (*tau:*) ptr ltau
, (*c:*) &GEMAT (t, m, n, ldc), (*ldc:*) integer ldc
, (*work:*) &(@[t?][lwork]), (*lwork:*) integer lwork
) -<fun> int // end of [unmqr_type]

stadef ormqr_type = unmqr_type

fun{t:t@ype} ormqr: ormqr_type (t)
fun sormqr: ormqr_type (real) = "atsctrb_clapack_sormqr"
fun dormqr: ormqr_type (doublereal) = "atsctrb_clapack_dormqr"

fun{t:t@ype} unmqr: unmqr_type (t)
fun cunmqr: unmqr_type (complex) = "atsctrb_clapack_cunmqr"
fun zunmqr: unmqr_type (doublecomplex) = "atsctrb_clapack_zunmqr"

fun{t:t@ype} ormqr_work_query
  {m,n,k:pos}
  {ma:nat | k <= ma}
  {lr:side}
  {tr:transpose} (
  pf: sidedim_p (lr, m, n, ma)
| side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
, m: integer m, n: integer n, k: integer k
) :<> [lwork:pos | lwork >= m+n-ma] integer (lwork)
// end of [ormqr_work_query]

fun{t:t@ype} unmqr_work_query
  {m,n,k:pos}
  {ma:nat | k <= ma}
  {lr:side}
  {tr:transpose} (
  pf: sidedim_p (lr, m, n, ma)
| side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
, m: integer m, n: integer n, k: integer k
) :<> [lwork:pos | lwork >= m+n-ma] integer (lwork)
// end of [unmqr_work_query]

(* ****** ****** *)

absview RQMAT_err_v (
  a: t@ype, m: int, n: int, k:int, lda: int, la:addr, ltau:addr
, err:int
) // end of [absview]

prfun RQMAT_err_v_elim
  {a:t@ype}
  {m,n,k:int} {lda:int} {la,ltau:addr}
  {err:int} (
  pfrq: RQMAT_err_v (a, m, n, k, lda, la, ltau, err)
) : (
  GEMAT (a?, m, n, lda) @ la, array_v (a, k, ltau)
) // end of [RQMAT_err_v_elim]

viewdef RQMAT_v (
  a: t@ype, m: int, n: int, k:int, lda: int, la:addr, ltau:addr
) = RQMAT_err_v (a, m, n, k, lda, la, ltau, 0(*succ*))

prfun TRMAT_v_of_RQMAT_v
  {a:t@ype}
  {m,n,k:nat | m <= n}
  {lda:inc} {la,ltau:addr}
  {ofs1,ofs:int} (
  pf_mul1: MUL (n-m, lda, ofs1)
, pf_mul: MUL (ofs1, sizeof a, ofs)
, pf_rqmat: RQMAT_v (a, m, n, k, lda, la, ltau)
) :<prf> (
  $GA.TRMAT_v (a, m, col, upper, nonunit, lda, la+ofs)
, $GA.TRMAT_v (a, m, col, upper, nonunit, lda, la+ofs) -<lin,prf>
  RQMAT_v (a, m, n, k, lda, la, ltau)
) // end of [TRMAT_v_of_RQMAT_v]

fun{a:t@ype}
  TRMAT_of_RQMAT
  {m,n,k:nat | m <= n}
  {lda:inc} {la,ltau:addr}
  {ofs:int} (
  pf_rqmat: RQMAT_v (a, m, n, k, lda, la, ltau)
| m: integer m, n: integer n, lda: integer lda, pa: ptr la
) :<> [l:addr] (
  $GA.TRMAT_v (a, m, col, upper, nonunit, lda, l)
, $GA.TRMAT_v (a, m, col, upper, nonunit, lda, l) -<lin,prf>
  RQMAT_v (a, m, n, k, lda, la, ltau)
| ptr l
) // end of [TRMAT_of_RQMAT]


(* ****** ****** *)

//
// gerqf: S, D, C, Z
//

(*

/* Subroutine */
int dgerqf_(
  integer *m, integer *n
, doublereal *a, integer *lda
, doublereal *tau
, doublereal *work, integer *lwork
, integer *info
) ;

*)

typedef
gerqf_type (t:t@ype) =
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la,ltau:addr}
  {lwork:pos | lwork >= m} (
  (*a*) GEMAT (t, m, n, lda) @ la
, (*tau*) array_v (t?, mn, ltau)
| (*m:*) integer m, (*n:*) integer n
, (*a:*) ptr la, (*lda:*) integer lda, (*tau :*) ptr ltau
, (*work:*) &(@[t?][lwork]), (*lwork:*) integer lwork
) -<fun> [err:int] (
  RQMAT_err_v (t, m, n, mn, lda, la, ltau, err) | int err
) // end of [gerqf_type]

fun{t:t@ype} gerqf: gerqf_type (t)
fun sgerqf: gerqf_type (real) = "atsctrb_clapack_sgerqf"
fun dgerqf: gerqf_type (doublereal) = "atsctrb_clapack_dgerqf"
fun cgerqf: gerqf_type (complex) = "atsctrb_clapack_cgerqf"
fun zgerqf: gerqf_type (doublecomplex) = "atsctrb_clapack_zgerqf"

fun{t:t@ype} gerqf_exn
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la,ltau:addr}
  {lwork:pos | lwork >= m} (
  pfa: GEMAT (t, m, n, lda) @ la
, pftau: array_v (t, mn, ltau)
| m: integer m, n: integer n
, a: ptr la, lda: integer lda, tau: ptr ltau
, work: &(@[t?][lwork]), lwork: integer lwork
) : (
  RQMAT_v (t, m, n, mn, lda, la, ltau) | void
) // end of [gerqf_exn]

fun{t:t@ype}
gerqf_work_query {m,n:nat}
  (m: integer m, n: integer n)
  :<> [lwork:int | lwork >= m] integer lwork
// end of [fun]

(* ****** ****** *)

//
// unmrq: C, Z
//

(*

/* Subroutine */
int cunmrq_(
  char *side
, char *trans
, integer *m, integer *n, integer *k
, complex *a, integer *lda
, complex *tau
, complex *c__, integer *ldc
, complex *work, integer *lwork
, integer *info
) ;

*)

typedef
unmrq_type (t:t@ype) =
  {m,n,k:nat}
  {ma,na:int} // na >= k
  {lda,ldc:inc}
  {lr:side} {tr:transpose}
  {lwork:pos | lwork >= m+n-na}
  {la,ltau:addr} (
  (*pf:*) sidedim_p (lr, m, n, na)
, (*rqmat*) !RQMAT_v (t, ma, na, k, lda, la, ltau)
| (*side:*) CLAPACK_SIDE_t lr
, (*trans:*) CLAPACK_TRANSPOSE_t tr
, (*m:*) integer m, (*n:*) integer n, (*k:*) integer k
, (*a:*) ptr la, (*lda:*) integer lda, (*tau:*) ptr ltau
, (*c__:*) &GEMAT (t, m, n, ldc), (*ldc:*) integer ldc
, (*work:*) &(@[t?][lwork]), (*lwork:*) integer lwork
) -<fun> int // end of [unmrq_type]

stadef ormrq_type = unmrq_type

fun{t:t@ype} unmrq: unmrq_type (t)
fun cunmrq: unmrq_type (complex) = "atsctrb_clapack_cunmrq"
fun zunmrq: unmrq_type (doublecomplex) = "atsctrb_clapack_zunmrq"

fun{t:t@ype} ormrq: ormrq_type (t)
fun sormrq: ormrq_type (real) = "atsctrb_clapack_sormrq"
fun dormrq: ormrq_type (doublereal) = "atsctrb_clapack_dormrq"

fun{t:t@ype} ormrq_work_query
  {m,n,k:pos}
  {na:nat | k <= na}
  {lr:side}
  {tr:transpose} (
    pf: sidedim_p (lr, m, n, na)
  | side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
  , m: integer m, n: integer n, k: integer k
  ) :<> [lwork:pos | lwork >= m+n-na] integer (lwork)
// end of [ormrq_work_query]

fun{t:t@ype} unmrq_work_query
  {m,n,k:pos}
  {na:nat | k <= na}
  {lr:side}
  {tr:transpose} (
  pf: sidedim_p (lr, m, n, na)
| side: CLAPACK_SIDE_t lr, trans: CLAPACK_TRANSPOSE_t tr
, m: integer m, n: integer n, k: integer k
) :<> [lwork:pos | lwork >= m+n-na] integer (lwork)
// end of [unmrq_work_query]

(* ****** ****** *)

// gels: S, D, C, Z

(*

/* Subroutine */
int dgels_(
  char *trans
, integer *m, integer *n
, integer *nrhs
, doublereal *a, integer *lda
, doublereal *b, integer *ldb
, doublereal *work, integer *lwork
, integer *info
) ;

*)

(*
** lwork >= 1
** lwork >= 2*min (m,n)
** lwork >= min (m,n) + nrhs
*)
absprop gels_lwork_p
  (tr: transpose, m: int, n: int, nrhs: int, lwork: int) // making the relation abstract
// end of [absprop]

prfun lemma_gels_lwork {tr:transpose} // implemented in [clapack.dats]
  {m,n,nrhs,lwork:nat | lwork >= 1; lwork >= 2*min(m,n); lwork >= min(m,n) + nrhs} ()
  : gels_lwork_p (tr, m, n, nrhs, lwork)
// end of [lemma_gels_lwork]

typedef
gels_type (t:t@ype) =
  {tr:transpose}
  {m,n:nat} {nrhs:nat} {lda,ldb:inc}
  {lwork:pos} (
  (*pf_lwork:*) gels_lwork_p (tr, m, n, nrhs, lwork)
| (*trans:*) CLAPACK_TRANSPOSE_t tr
, (*m:*) integer m, (*n:*) integer n, (*nrhs:*) integer nrhs
, (*a:*) &GEMAT (t, m, n, lda) >> GEMAT (t?, m, n, lda)
, (*lda:*) integer lda
, (*b:*) &GEMAT (t, max(m,n), nrhs, ldb)
, (*ldb:*) integer ldb
, (*work:*) &(@[t?][lwork]), (*lwork:*) integer lwork
) -<fun> int // end of [gels_type]

fun{t:t@ype} gels: gels_type (t) = "atsctrb_clapack_gels"
fun sgels : gels_type (real) = "atsctrb_clapack_sgels"
fun dgels : gels_type (doublereal) = "atsctrb_clapack_dgels"
fun cgels : gels_type (complex) = "atsctrb_clapack_cgels"
fun zgels : gels_type (doublecomplex) = "atsctrb_clapack_zgels"

fun{t:t@ype}
gels_work_query {tr:transpose} {m,n:pos} {nrhs:nat} (
  trans: CLAPACK_TRANSPOSE_t tr, m: integer m, n: integer n, nrhs: integer nrhs
) :<> [lwork:pos] (
  gels_lwork_p (tr, m, n, nrhs, lwork) | integer lwork
) // end of [gels_work_query]

(* ****** ****** *)

// trtrs: S, D, C, Z

(*

/* Subroutine */
int dtrtrs_(
  char *uplo
, char *trans
, char *diag
, integer *n
, integer *nrhs
, doublereal *a, integer *lda
, doublereal *b, integer *ldb
, integer *info
) ;

*)
  
typedef
trtrs_type (t:t@ype) =
  {ul:uplo}
  {tr:transpose}
  {dg:diag}
  {n:nat} {nrhs:nat}
  {lda,ldb:inc} (
  (*uplo:*) CLAPACK_UPLO_t ul
, (*trans:*) CLAPACK_TRANSPOSE_t tr
, (*diag:*) CLAPACK_DIAG_t dg
, (*n:*) integer n, (*nrhs:*) integer nrhs
, (*a:*) &TRMAT (t, n, ul, dg, lda), (*lda:*) integer lda
, (*b:*) &GEMAT (t, n, nrhs, ldb), (*ldb:*) integer ldb
) -<fun> int // end of [trtrs_type]

fun{t:t@ype} trtrs: trtrs_type (t)
fun strtrs: trtrs_type (real) = "atsctrb_clapack_strtrs"
fun dtrtrs: trtrs_type (doublereal) = "atsctrb_clapack_dtrtrs"
fun ctrtrs: trtrs_type (complex) = "atsctrb_clapack_ctrtrs"
fun ztrtrs: trtrs_type (doublecomplex) = "atsctrb_clapack_ztrtrs"

(* ****** ****** *)

absview LUMAT_err_v (
  a: t@ype, m: int, n: int, lda: int, la:addr, err:int
) // end of [absview]

prfun LUMAT_err_v_elim {a:t@ype}
  {m,n:int} {lda:int} {la:addr} {err:int}
  (pflu: LUMAT_err_v (a, m, n, lda, la, err)): GEMAT (a?, m, n, lda) @ la
// end of [LUMAT_err_v_elim]

prfun TRMAT_LU_v_of_LUMAT_v
  {a:t@ype} {m,n:nat | m <= n}
  {lda:inc} {la:addr} {err:nat} {ofs:int} (
  pf_ofs: MUL (n, sizeof a, ofs)
, pf_lumat: LUMAT_err_v (a, m, n, lda, la, err)
) :<prf> (
  TRMAT (a, n, lower, unit, lda) @ la
, TRMAT (a, m, lower, unit, lda) @ la -<lin,prf> LUMAT_err_v (a, m, n, lda, la, err)
) // end of [TRMAT_LU_v_of_LUMAT_v]

prfun TRMAT_UN_v_of_LUMAT_v
  {a:t@ype} {m,n:nat | m >= n}
  {lda:inc} {la:addr} {err:nat} (
  pf_lumat: LUMAT_err_v (a, m, n, lda, la, err)
) :<prf> (
  TRMAT (a, n, upper, nonunit, lda) @ la
, TRMAT (a, n, upper, nonunit, lda) @ la -<lin,prf> LUMAT_err_v (a, m, n, lda, la, err)
) // end of [TRMAT_UN_v_of_LUMAT_v]

fun{a:t@ype} LUMAT_ptr_split_skinny
  {m,n:nat | m >= n} {lda:inc} {la:addr} {err:nat} (
  pf_lumat: LUMAT_err_v (a, m, n, lda, la, err)
| la: ptr la, m: size_t m, n: size_t n, lda: size_t lda
) :<> [la_ofs:addr] (
  TRMAT (a, n, lower, unit, lda) @ la
, TRMAT (a, n, upper, nonunit, lda) @ la
, GEMAT (a, m-n, n, lda) @ la_ofs
, ( TRMAT (a, n, lower, unit, lda) @ la
  , TRMAT (a, n, upper, nonunit, lda) @ la
  , GEMAT (a, m-n, n, lda) @ la_ofs
  ) -<prf> LUMAT_err_v (a, m, n, lda, la, err)
| ptr la_ofs
) // end of [LUMAT_ptr_split_skinny]

(* ****** ****** *)

// getrf: S, D, C, Z

(*

/* Subroutine */
int dgetrf_(
  integer *m, integer *n
, doublereal *a, integer *lda
, integer *ipiv
, integer *info
) ;

*)

//
// SC-2010-11-16:
// getrf computes LU factorization with row pivoting: A = P*L*U
// The LU factors are overwritten on the input matrix A.
// The view at A is changed to LUMAT_err_v to reflect this.
// The original view at A can be recovered using LUMAT_err_v_elim.
// The permutation is stored in ipiv. Note that this uses Fortran indices,
// so 1 <= IPIV(i) <= m.
// If the returned error code is < 0, then IPIV is still uninitialized and
// so has no meaning. If the error code > 0, then the LU factorization did
// finish, but the U has a zero diagonal in position indicated by the error
// code (in Fortran index notation).
//
typedef
getrf_type (t:t@ype) =
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la:addr} (
  (*a*) GEMAT (t, m, n, lda) @ la
| (*m:*) integer m, (*n:*) integer n
, (*a:*) ptr la, (*lda:*) integer lda
, (*ipiv:*) &(@[integer?][mn]) >> @[integerBtwe(1,m)][mn]
) -<fun> [err:int] (
  LUMAT_err_v (t, m, n, lda, la, err) | int err
) // end of [getrf_type]

//

fun{t:t@ype} getrf: getrf_type (t)
fun sgetrf: getrf_type (real) = "atsctrb_clapack_sgetrf"
fun dgetrf: getrf_type (doublereal) = "atsctrb_clapack_dgetrf"
fun cgetrf: getrf_type (complex) = "atsctrb_clapack_cgetrf"
fun zgetrf: getrf_type (doublecomplex) = "atsctrb_clapack_zgetrf"

//
// SC-2010-11-16:
// getrf_exn is the same as getrf, except that it raises an exception
// of error < 0.
//
fun{t:t@ype} getrf_exn
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {la:addr} (
  pf: GEMAT (t, m, n, lda) @ la
| m: integer m, n: integer n
, a: ptr la, lda: integer lda
, ipiv: &(@[integer?][mn]) >> @[integerBtwe(1,m)][mn]
) :<!exn> [info:int | info >= 0] (
  LUMAT_err_v (t, m, n, lda, la, info) | int info
) // end of [getrf_exn]

(* ****** ****** *)

// laswp: S, D, C, Z

(*

/* Subroutine */
int dlaswp_(
  integer *n
, doublereal *a, integer *lda
, integer *k1, integer *k2
, integer *ipiv
, integer *incx
) ;

*)

typedef
laswp_type (t:t@ype) =
  {m,n:nat} {lda:inc}
  {k1,k2:nat | k1 <= k2 && k2 <= m}
  {k:nat | k2 <= k} {incx:int | incx == 1 || incx == ~1} (
  (*n:*) integer n
, (*a:*) &GEMAT (t, m, n, lda), (*lda:*) integer lda
, (*k1*) integer k1, (*k2*) integer k2
, (*ipiv:*) &(@[integer][k])
, (*incx*) integer incx
) -<> void // end of [laswp_type]

//

fun{t:t@ype} laswp: laswp_type (t)
fun slaswp: laswp_type (real) = "atsctrb_clapack_slaswp"
fun dlaswp: laswp_type (doublereal) = "atsctrb_clapack_dlaswp"
fun claswp: laswp_type (complex) = "atsctrb_clapack_claswp"
fun zlaswp: laswp_type (doublecomplex) = "atsctrb_clapack_zlaswp"

(* ****** ****** *)

// gesv: S, D, C, Z

(*

/* Subroutine */
int dgesv_ (
  integer *n, integer *nrhs
, doublereal *a, integer *lda
, integer *ipiv
, doublereal *b, integer *ldb
, integer *info
) ;

*)

//
// SC-2010-11-15:
// A := P*L*U; So the view at A is changed to LUMAT_err_v. To get back the
// original view (with uninitialized A) use LUMAT_err_v_elim. ipiv is
// assigned P. The solution to A * x = b, is returned in b. A must be a
// square matrix.
//

typedef
gesv_type (t:t@ype) =
  {n,nrhs:nat} {lda,ldb:inc} {la:addr} (
  (*a*) GEMAT (t, n, n, lda) @ la
| (*n:*) integer n, (*nrhs:*) integer nrhs
, (*a:*) ptr la, (*lda:*) integer lda
, (*ipiv:*) &(@[integer?][n]) >> @[integerBtwe(1,n)][n]
, (*b:*) &GEMAT (t, n, nrhs, ldb), (*ldb:*) integer ldb
) -<fun> [err:int] (
  LUMAT_err_v (t, n, n, lda, la, err) | int err
) // end of [gesv_type]

fun{t:t@ype} gesv: gesv_type (t)
fun sgesv: gesv_type (real) = "atsctrb_clapack_sgesv"
fun dgesv: gesv_type (doublereal) = "atsctrb_clapack_dgesv"
fun cgesv: gesv_type (complex) = "atsctrb_clapack_cgesv"
fun zgesv: gesv_type (doublecomplex) = "atsctrb_clapack_zgesv"

(* ****** ****** *)

(*

/* Subroutine */
int dgesvd_(
  char *jobu, char *jobvt
, integer *m, integer *n
, doublereal *a, integer *lda
, doublereal *s
, doublereal *u, integer *ldu
, doublereal *vt, integer *ldvt
, doublereal *work, integer *lwork
, integer *info
) ;

*)

(* ****** ****** *)

// gesvd: full SVD // JOBU = 'A' and JOBVT = 'A')

(*
** lwork >= 1
** lwork >= 3*min(m,n) + m
** lwork >= 3*min(m,n) + n
** lwork >= 5*min(m,n)
*)
absprop
gesvd_lwork_p (m:int, n:int, lwork:int)

prfun
lemma_gesvd_lwork // implemented in [clapack.dats]
  { m, n, lwork : nat
  | lwork >= 1
  ; lwork >= 3*min(m,n) + max(m,n)
  ; lwork >= 5*min(m,n)
  } ()
  : gesvd_lwork_p (m,n,lwork)
// end of [lemma_gesvd_lwork]

(*
** Full SVD
*)

typedef
gesvd_type (t1:t@ype, t2:t@ype) =
  {m,n:nat} {mn:int | mn==min(m,n)} {lda,ldu,ldvt:inc} {lwork:pos} (
  (*pf_lwork:*) gesvd_lwork_p (m, n, lwork)
| (*m:*)        integer m
, (*n:*)        integer n
, (*a:*)        &(GEMAT (t1, m, n, lda)) >> GEMAT(t1?, m, n, lda)
, (*lda:*)      integer lda
, (*s:*)        &(@[t2?][mn]) >> @[t2][mn]
, (*u:*)        &(GEMAT (t1?, m, m, ldu)) >> GEMAT (t1, m, m, ldu)
, (*ldu:*)      integer ldu
, (*vt:*)       &(GEMAT (t1?, n, n, ldvt)) >> GEMAT (t1, n, n, ldvt)
, (*ldvt:*)     integer ldvt
, (*work:*)     &(@[t1?][lwork])
, (*lwork:*)    integer lwork
) -<fun> int // end of [gesvd_type]

fun{t1,t2:t@ype} gesvd: gesvd_type (t1, t2)

fun{t1,t2:t@ype}
gesvd_work_query
  {m,n:pos} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_work_query]

(* ****** ****** *)

(*
** Econonmy SVD
*)

typedef
gesvd_econ_type (t1:t@ype, t2:t@ype) =
  {m,n:nat} {mn:int | mn==min(m,n)} {lda,ldu,ldvt:inc} {lwork:pos} (
  (*pf_lwork:*) gesvd_lwork_p (m, n, lwork)
| (*m:*)        integer m
, (*n:*)        integer n
, (*a:*)        &(GEMAT (t1, m, n, lda)) >> GEMAT(t1?, m, n, lda)
, (*lda:*)      integer lda
, (*s:*)        &(@[t2?][mn]) >> @[t2][mn]
, (*u:*)        &(GEMAT (t1?, m, mn, ldu)) >> GEMAT (t1, m, mn, ldu)
, (*ldu:*)      integer ldu
, (*vt:*)       &(GEMAT (t1?, mn, n, ldvt)) >> GEMAT (t1, mn, n, ldvt)
, (*ldvt:*)     integer ldvt
, (*work:*)     &(@[t1?][lwork])
, (*lwork:*)    integer lwork
) -<fun> int // end of [gesvd_econ_type]

fun{t1,t2:t@ype} gesvd_econ: gesvd_econ_type (t1, t2)

fun{t1,t2:t@ype}
gesvd_econ_work_query
  {m,n:pos} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_econ_work_query]

(* ****** ****** *)

(*
** Singular values only
*)

typedef
gesvd_sing_type (t1:t@ype, t2:t@ype) =
  {m,n:nat} {mn:int | mn==min(m,n)} {lda:inc} {lwork:pos} (
  (*pf_lwork:*) gesvd_lwork_p (m, n, lwork)
| (*m:*)        integer m
, (*n:*)        integer n
, (*a:*)        &(GEMAT (t1, m, n, lda)) >> GEMAT(t1?, m, n, lda)
, (*lda:*)      integer lda
, (*s:*)        &(@[t2?][mn]) >> @[t2][mn]
, (*work:*)     &(@[t1?][lwork])
, (*lwork:*)    integer lwork
) -<fun> int // end of [gesvd_sing_type]

fun{t1,t2:t@ype} gesvd_sing: gesvd_sing_type (t1, t2)

fun{t1,t2:t@ype}
gesvd_sing_work_query
  {m,n:pos} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_sing_work_query]

(* ****** ****** *)

(*
** Singular values and left singular vectors only: A -> U
*)

typedef
gesvd_left_type (t1:t@ype, t2:t@ype) =
  {m,n:nat | m >= n} {lda:inc} {lwork:pos} (
  (*pf_lwork:*) gesvd_lwork_p (m, n, lwork)
| (*m:*)        integer m
, (*n:*)        integer n
, (*a:*)        &(GEMAT (t1, m, n, lda))
, (*lda:*)      integer lda
, (*s:*)        &(@[t2?][n]) >> @[t2][n]
, (*work:*)     &(@[t1?][lwork])
, (*lwork:*)    integer lwork
) -<fun> int // end of [gesvd_left_type]

fun{t1,t2:t@ype} gesvd_left: gesvd_left_type (t1, t2)

fun{t1,t2:t@ype}
gesvd_left_work_query
  {m,n:pos} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_left_work_query]

(* ****** ****** *)

(*
** Singular values and right singular vectors only: A -> VT
*)

typedef
gesvd_right_type
  (t1:t@ype, t2:t@ype) =
  {m,n:nat | m <= n} {lda:inc} {lwork:pos} (
  (*pf_lwork:*) gesvd_lwork_p (m, n, lwork)
| (*m:*)        integer m
, (*n:*)        integer n
, (*a:*)        &(GEMAT (t1, m, n, lda))
, (*lda:*)      integer lda
, (*s:*)        &(@[t2?][m]) >> @[t2][m]
, (*work:*)     &(@[t1?][lwork])
, (*lwork:*)    integer lwork
) -<fun> int // end of [gesvd_right_type]

fun{t1,t2:t@ype} gesvd_right: gesvd_right_type (t1, t2)

fun{t1,t2:t@ype}
gesvd_right_work_query
  {m,n:pos} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_right_work_query]

(* ****** ****** *)

(*
** Economy SVD of skinny matrices: A -> U
*)

typedef
gesvd_skinny_type
  (t1:t@ype, t2:t@ype) =
  {m,n:nat | m >= n} {lda,ldvt:inc} {lwork:pos} (
  (*pf_lwork:*) gesvd_lwork_p (m, n, lwork)
| (*m:*)        integer m
, (*n:*)        integer n
, (*a:*)        &(GEMAT (t1, m, n, lda))
, (*lda:*)      integer lda
, (*s:*)        &(@[t2?][n]) >> @[t2][n]
, (*vt:*)       &(GEMAT (t1?, n, n, ldvt)) >> GEMAT (t1, n, n, ldvt)
, (*ldvt:*)     integer ldvt
, (*work:*)     &(@[t1?][lwork])
, (*lwork:*)    integer lwork
) -<fun> int // end of [gesvd_skinny_type]

fun{t1,t2:t@ype} gesvd_skinny: gesvd_skinny_type (t1, t2)

fun{t1,t2:t@ype}
gesvd_skinny_work_query
  {m,n:pos | m >= n} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_skinny_work_query]

(* ****** ****** *)

(*
** Only S and VT of skinny matrix
*)

fun{t1,t2:t@ype}
gesvd_skinny_right
  {m,n:nat | m >= n} {lda,ldvt:inc} {lwork:pos} (
  pf_lwork: gesvd_lwork_p (m, n, lwork)
| m: integer m
, n: integer n
, a: &(GEMAT (t1, m, n, lda)) >> GEMAT (t1?, m, n, lda)
, lda: integer lda
, s: &(@[t2?][n]) >> @[t2][n]
, vt: &(GEMAT (t1?, n, n, ldvt)) >> GEMAT (t1, n, n, ldvt)
, ldvt: integer ldvt
, work: &(@[t1?][lwork])
, lwork: integer lwork
) :<> int
// end of [gesvd_skinny_right]

fun{t1,t2:t@ype}
gesvd_skinny_right_work_query
  {m,n:pos} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_skinny_right_work_query]

(* ****** ****** *)

(*
** Economy SVD of fat matrices: A -> VT
*)

typedef
gesvd_fat_type
  (t1:t@ype, t2:t@ype) =
  {m,n:nat | m <= n} {lda,ldu:inc} {lwork:pos} (
  (*pf_lwork:*) gesvd_lwork_p (m, n, lwork)
| (*m:*)        integer m
, (*n:*)        integer n
, (*a:*)        &(GEMAT (t1, m, n, lda))
, (*lda:*)      integer lda
, (*s:*)        &(@[t2?][m]) >> @[t2][m]
, (*u:*)        &(GEMAT (t1?, m, m, ldu)) >> GEMAT (t1, m, m, ldu)
, (*ldu:*)      integer ldu
, (*work:*)     &(@[t1?][lwork])
, (*lwork:*)    integer lwork
) -<fun> int // end of [gesvd_fat_type]

fun{t1,t2:t@ype} gesvd_fat: gesvd_fat_type (t1, t2)

fun{t1,t2:t@ype}
gesvd_fat_work_query
  {m,n:pos} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_fat_work_query]

(* ****** ****** *)

(*
** Only U and S of fat matrix
*)

fun{t1,t2:t@ype}
gesvd_fat_left
  {m,n:nat | m <= n} {lda,ldu:inc} {lwork:pos} (
  pf_lwork: gesvd_lwork_p (m, n, lwork)
| m: integer m
, n: integer n
, a: &(GEMAT (t1, m, n, lda)) >> GEMAT (t1?, m, n, lda)
, lda: integer lda
, s: &(@[t2?][m]) >> @[t2][m]
, u: &(GEMAT (t1?, m, m, ldu)) >> GEMAT (t1, m, m, ldu)
, ldu: integer ldu
, work: &(@[t1?][lwork])
, lwork: integer lwork
) :<> int
// end of [gesvd_fat_left]

fun{t1,t2:t@ype}
gesvd_fat_left_work_query
  {m,n:pos | m <= n} (m: integer m, n: integer n)
  :<> [lwork:pos] (gesvd_lwork_p (m, n, lwork) | integer lwork)
// end of [gesvd_fat_left_work_query]

(* ****** ****** *)

(* end of [clapack.sats] *)
