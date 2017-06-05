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

%{#
#include "libc/CATS/complex.cats" // it needs to be loaded before [cblas.cats]
#include "contrib/cblas/CATS/cblas.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "libc/SATS/complex.sats"

(* ****** ****** *)

staload GA = "libats/SATS/genarrays.sats"
//
sortdef inc = $GA.inc
//
sortdef order = $GA.order
//
stadef row = $GA.row
stadef col = $GA.col
stadef ORDER = $GA.ORDER
//
sortdef uplo = $GA.uplo
stadef upper = $GA.upper
stadef lower = $GA.lower
stadef UPLO = $GA.UPLO
//
sortdef diag = $GA.diag
stadef unit = $GA.unit ()
stadef nonunit = $GA.nonunit ()
stadef DIAG = $GA.DIAG
//
sortdef transpose = $GA.transpose
stadef TPN = $GA.TPN
stadef TPT = $GA.TPT
stadef TPC = $GA.TPC
stadef TRANSPOSE = $GA.TRANSPOSE
//
sortdef side = $GA.side
stadef left = $GA.left
stadef right = $GA.right
stadef SIDE = $GA.SIDE
//
stadef GEVEC = $GA.GEVEC
stadef GEMAT = $GA.GEMAT
stadef GEMAT_v = $GA.GEMAT_v
stadef GBMAT = $GA.GBMAT
stadef TRMAT = $GA.TRMAT
stadef TBMAT = $GA.TBMAT
stadef TPMAT = $GA.TPMAT
stadef SYMAT = $GA.SYMAT
stadef SBMAT = $GA.SBMAT
stadef SPMAT = $GA.SPMAT
stadef HEMAT = $GA.HEMAT
stadef HBMAT = $GA.HBMAT
stadef HPMAT = $GA.HPMAT
//
stadef trandim_p = $GA.trandim_p
stadef sidedim_p = $GA.sidedim_p
//
(* ****** ****** *)

abst@ype CBLAS_ORDER_t (order) = int

(*
val CblasRowMajor : CBLAS_ORDER_t (row)
val CblasColMajor : CBLAS_ORDER_t (col)
*)

macdef CblasRowMajor = $extval (CBLAS_ORDER_t row, "CblasRowMajor")
macdef CblasColMajor = $extval (CBLAS_ORDER_t col, "CblasColMajor")

(*
// HX: it not implemented because it is not needed yet
fun eq_CBLAS_ORDER_ORDER {ord1,ord2:order}
  (x1: CBLAS_ORDER_t ord1, x2: CBLAS_ORDER_t ord2):<> bool (ord1==ord2)
  = "atsctrb_eq_CBLAS_ORDER_ORDER"
overload = with eq_CBLAS_ORDER_ORDER
*)

// implemented in [clbas.dats]
fun CBLAS_ORDER_of_ORDER {ord:order}
  (x: ORDER ord):<> CBLAS_ORDER_t ord

(*
// implemented in [clbas.dats]
fun OrderOfCblasOrder {ord:order}
  (x: CBLAS_ORDER_t ord):<> ORDER ord = "atsctrb_OrderOfCblasOrder"
// end of [OrderOfCblasOrder]
*)

(* ****** ****** *)

abst@ype CBLAS_UPLO_t (uplo) = int

(*
val CblasUpper : CBLAS_UPLO_t (upper)
val CblasLower : CBLAS_UPLO_t (lower)
*)

macdef CblasUpper = $extval (CBLAS_UPLO_t upper, "CblasUpper")
macdef CblasLower = $extval (CBLAS_UPLO_t lower, "CblasLower")

(*
fun eq_CBLAS_UPLO_UPLO {ul1,ul2:uplo}
  (x1: CBLAS_UPLO_t ul1, x2: CBLAS_UPLO_t ul2):<> bool (ul1==ul2)
  = "atsctrb_eq_CBLAS_UPLO_UPLO"
overload = with eq_CBLAS_UPLO_UPLO
*)

// implemented in [clbas.dats]
fun CBLAS_UPLO_of_UPLO {ul:uplo} (x: UPLO ul):<> CBLAS_UPLO_t ul

(* ****** ****** *)

abst@ype CBLAS_DIAG_t (diag) = int

(*
val CblasUnit : CBLAS_DIAG_t (unit)
val CblasNonUnit : CBLAS_DIAG_t (nonunit)
*)

macdef CblasUnit = $extval (CBLAS_DIAG_t(unit), "CblasUnit")
macdef CblasNonUnit = $extval (CBLAS_DIAG_t(nonunit), "CblasNonUnit")

(*
fun eq_CBLAS_DIAG_DIAG {dg1,dg2:diag}
  (x1: CBLAS_DIAG_t dg1, x2: CBLAS_DIAG_t dg2):<> bool (dg1==dg2)
  = "atsctrb_eq_CBLAS_DIAG_DIAG"
overload = with eq_CBLAS_DIAG_DIAG
*)

//
// HX: implemented in [clbas.dats]
//
fun CBLAS_DIAG_of_DIAG {dg:diag} (x: DIAG dg):<> CBLAS_DIAG_t dg

(* ****** ****** *)

abst@ype CBLAS_TRANSPOSE_t (transpose) = int

(*
val CblasNoTrans   : CBLAS_TRANSPOSE_t (N)
val CblasTrans     : CBLAS_TRANSPOSE_t (T)
val CblasConjTrans : CBLAS_TRANSPOSE_t (CT)
val AtlasConj      : CBLAS_TRANSPOSE_t (AC) // not used
*)

macdef CblasNoTrans =
  $extval (CBLAS_TRANSPOSE_t (TPN), "CblasNoTrans")
// end of [macdef]

macdef CblasTrans =
  $extval (CBLAS_TRANSPOSE_t (TPT), "CblasTrans")
// end of [macdef]

macdef CblasConjTrans =
  $extval (CBLAS_TRANSPOSE_t (TPC), "CblasConjTrans")
// end of [macdef]

(*
macdef AtlasConj =
  $extval (CBLAS_TRANSPOSE_t (AC), "AtlasConj")
// end of [macdef]
*)

(*
fun eq_CBLAS_TRANSPOSE_TRANSPOSE {tr1,tr2:transpose}
  (x1: CBLAS_TRANSPOSE_t tr1, x2: CBLAS_TRANSPOSE_t tr2):<> bool (tr1==tr2)
  = "atsctrb_eq_CBLAS_TRANSPOSE_TRANSPOSE"
overload = with eq_CBLAS_TRANSPOSE_TRANSPOSE
*)

fun // implemented in [clbas.dats]
CBLAS_TRANSPOSE_of_TRANSPOSE {tr:transpose}
  (x: TRANSPOSE tr):<> CBLAS_TRANSPOSE_t tr
// end of [fun]

(* ****** ****** *)

abst@ype CBLAS_SIDE_t (side) = int

(*
val CblasLeft  : CBLAS_SIDE_t (left)
val CblasRight : CBLAS_SIDE_t (right)
*)

macdef CblasLeft = $extval (CBLAS_SIDE_t left, "CblasLeft")
macdef CblasRight = $extval (CBLAS_SIDE_t right, "CblasRight")

(*
fun eq_CBLAS_SIDE_SIDE {lr1,lr2:side}
  (x1: CBLAS_SIDE_t lr1, x2: CBLAS_SIDE_t lr2):<> bool (lr1==lr2)
  = "atsctrb_eq_CBLAS_SIDE_SIDE"
overload = with eq_CBLAS_SIDE_SIDE
*)

fun // implemented in [clbas.dats]
CBLAS_SIDE_of_SIDE {lr:side} (x: SIDE lr):<> CBLAS_SIDE_t lr
// end of [CBLAS_SIDE_of_SIDE]

(* ****** ****** *)

//
// BLAS level 1
//

(* ****** ****** *)

//
// ROTG: S, D, C, Z
//

(*

void cblas_drotg
  (double *a, double *b, double *c, double *s);
// end of [cblas-drotg]

*)

//
// givens(c, s) [a b]' -> [a 0]
//
fun{t:t@ype}
cblas_rotg
  (a: &t, b: &t, c: &t? >> t, s: &t? >> t) :<> void
// end of [cblas_rotg]

fun cblas_srotg ( // givens(c, s) [a b]' -> [a 0]
  a: &float, b: &float
, c: &float? >> float, s: &float? >> float
) :<> void
  = "mac#atsctrb_cblas_srotg"
// end of [cblas_srotg]

fun cblas_drotg ( // givens(c, s) [a b]' -> [a 0]
  a: &double, b: &double
, c: &double? >> double, s: &double? >> double
) :<> void
  = "mac#atsctrb_cblas_drotg"
// end of [cblas_drotg]

fun cblas_crotg ( // givens(c, s) [a b]' -> [a 0]
  a: &ccmplx, b: &ccmplx
, c: &ccmplx? >> ccmplx, s: &ccmplx? >> ccmplx
) :<> void
  = "mac#atsctrb_cblas_crotg"
// end of [cblas_crotg]

fun cblas_zrotg ( // givens(c, s) [a b]' -> [a 0]
  a: &zcmplx, b: &zcmplx
, c: &zcmplx? >> zcmplx, s: &zcmplx? >> zcmplx
) :<> void
  = "mac#atsctrb_cblas_zrotg"
// end of [cblas_zrotg]

(* ****** ****** *)

//
// ROTMG: S, D
//

(*

void cblas_drotmg (
  double *d1, double *d2
, double *b1, const double b2
, double *P
) ;

*)

//
// mod_givens(P) [sqrt(d1)*b1 sqrt(d2)*b2]' -> [a 0]
//
fun{a:t@ype}
cblas_rotmg (
  d1: &a, d2: &a, b1: &a, b2: a, P: &GEVEC (a, 5, 1)
) :<> void // end of [cblas_rotmg]

fun cblas_srotmg (
  d1: &float, d2: &float
, b1: &float, b2: float
, P: &GEVEC (float, 5, 1)
) :<> void
  = "mac#atsctrb_cblas_srotmg"
// end of [cblas_srotmg]

fun cblas_drotmg (
  d1: &double, d2: &double
, b1: &double, b2: double
, P: &GEVEC (double, 5, 1)
) :<> void
  = "mac#atsctrb_cblas_drotmg"
// end of [cblas_srotmg]

(* ****** ****** *)

//
// ROT: S, D, CS, ZD
//

(*

void cblas_drot (
  const int N
, double *X, const int incX
, double *Y, const int incY
, const double c, const double s
) ;

void cblas_zdrot (
  const int N
, void *X, const int incX
, void *Y, const int incY
, const double c, const double s
) ;

*)

fun{a:t@ype}
cblas_rot // givens(c, s) [x y]'
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx), incX: int (dx)
, Y: &GEVEC (a, n, dy), incY: int (dy)
, c: a, s: a
) :<> void // end of [cblas_rot]

fun cblas_srot
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (float, n, dx), incX: int (dx)
, Y: &GEVEC (float, n, dy), incY: int (dy)
, c: float, s: float
) :<> void
  = "mac#atsctrb_cblas_srot"
// end of [cblas_srot]


fun cblas_drot
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (double, n, dx), incX: int (dx)
, Y: &GEVEC (double, n, dy), incY: int (dy)
, c: double, s: double
) :<> void
  = "mac#atsctrb_cblas_drot"
// end of [cblas_drot]

(*

// not in CBLAS proper
fun cblas_csrot
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (ccmplx, n, dx), incX: int (dx)
, Y: &GEVEC (ccmplx, n, dy), incY: int (dy)
, c: float, s: float
) :<> void
  = "mac#atsctrb_cblas_csrot"
// end of [cblas_csrot]

fun cblas_zdrot
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (zcmplx, n, dx), incX: int (dx)
, Y: &GEVEC (zcmplx, n, dy), incY: int (dy)
, c: double, s: double
) :<> void
  = "mac#atsctrb_cblas_zdrot"
// end of [cblas_zdrot]

*)

(* ****** ****** *)

//
// ROTM: S, D
//

(*

void cblas_drotm (
  const int N
, double *X, const int incX
, double *Y, const int incY
, const double *P
) ;

*)

fun{a:t@ype}
cblas_rotm // mod_givens(P) [x y]'
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx), incX: int (dx)
, Y: &GEVEC (a, n, dy), incY: int (dy)
, P: &GEVEC (a, 5, 1)
) :<> void // end of [cblas_rotm]

fun cblas_srotm
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (float, n, dx), incX: int (dx)
, Y: &GEVEC (float, n, dy), incY: int (dy)
, P: &GEVEC (float, 5, 1)
) :<> void
  = "mac#atsctrb_cblas_srotm"
// end of [cblas_srotm]

fun cblas_drotm
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (double, n, dx), incX: int (dx)
, Y: &GEVEC (double, n, dy), incY: int (dy)
, P: &GEVEC (double, 5, 1)
) :<> void
  = "mac#atsctrb_cblas_drotm"
// end of [cblas_drotm]

(* ****** ****** *)

//
// DOT: S, D
//

(*

// taken from [cblas.h]
double cblas_ddot(
  const int N
, const double *X, const int incX
, const double *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_dot // dot product
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx), incX: int (dx)
, Y: &GEVEC (a, n, dy), incY: int (dy)
) :<> a // end of [cblas_dot]

fun cblas_sdot // dot product for single precision
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (float, n, dx), incX: int (dx)
, Y: &GEVEC (float, n, dy), incY: int (dy)
) :<> float
  = "mac#atsctrb_cblas_sdot"

fun cblas_ddot // dot product for double precision
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (double, n, dx), incX: int (dx)
, Y: &GEVEC (double, n, dy), incY: int (dy)
) :<> double
  = "mac#atsctrb_cblas_ddot"
// end of [fun]

(* ****** ****** *)

(*

float  cblas_sdsdot(
  const int N, const float alpha
, const float *X, const int incX
, const float *Y, const int incY
) ;

double cblas_dsdot(
  const int N
, const float *X, const int incX
, const float *Y, const int incY
) ;

*)

// dot product for
fun cblas_sdsdot // single precision computed in double precision
  {n:nat} {dx,dy:inc} (
  N: int n
, alpha: float
, X: &GEVEC (float, n, dx), incX: int (dx)
, Y: &GEVEC (float, n, dy), incY: int (dy)
) :<> float
  = "mac#atsctrb_cblas_sdsdot"
// end of [cblas_sdsdot]

// dot product for
fun cblas_dsdot // single precision computed in double precision
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (float, n, dx), incX: int (dx)
, Y: &GEVEC (float, n, dy), incY: int (dy)
) :<> double
  = "mac#atsctrb_cblas_dsdot"
// end of [cblas_dsdot]

(* ****** ****** *)

//
// DOTU_SUB: C, Z
//

(*

void cblas_cdotu_sub(
  const int N
, const void *X, const int incX
, const void *Y, const int incY
, void *dotu
) ;

*)

fun{a:t@ype}
cblas_dotu_sub // dot product for complex
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx), incX: int (dx)
, Y: &GEVEC (a, n, dy), incY: int (dy)
, dotu: &a? >> a
) :<> void // end of [cblas_dotu_sub]

fun cblas_cdotu_sub // dot product for complex single precision
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (ccmplx, n, dx), incX: int (dx)
, Y: &GEVEC (ccmplx, n, dy), incY: int (dy)
, dotu: &ccmplx? >> ccmplx
) :<> void
  = "mac#atsctrb_cblas_cdotu_sub"
// end of [cblas_cdotu_sub]

fun cblas_zdotu_sub // dot product for complex double precision
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (zcmplx, n, dx), incX: int (dx)
, Y: &GEVEC (zcmplx, n, dy), incY: int (dy)
, dotu: &zcmplx? >> zcmplx
) :<> void
  = "mac#atsctrb_cblas_zdotu_sub"
// end of [cblas_zdotu_sub]

(* ****** ****** *)

//
// DOTU_SUB: C, Z
//

(*

void cblas_cdotc_sub (
  const int N
, const void *X, const int incX
, const void *Y, const int incY
, void *dotc
) ;

*)

fun{a:t@ype}
cblas_dotc_sub // conjugate dot product for complex
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx), incX: int (dx)
, Y: &GEVEC (a, n, dy), incY: int (dy)
, dotc: &a? >> a
) :<> void // end of [cblas_dotc_sub]

fun cblas_cdotc_sub // conjugate dot product for complex single precision
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (ccmplx, n, dx), incX: int (dx)
, Y: &GEVEC (ccmplx, n, dy), incY: int (dy)
, dotc: &ccmplx? >> ccmplx
) :<> void
  = "mac#atsctrb_cblas_cdotc_sub"
// end of [cblas_cdotc_sub]

fun cblas_zdotc_sub // conjugate dot product for complex double precision
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (zcmplx, n, dx), incX: int (dx)
, Y: &GEVEC (zcmplx, n, dy), incY: int (dy)
, dotc: &zcmplx? >> zcmplx
) :<> void
  = "mac#atsctrb_cblas_zdotc_sub"
// end of [cblas_zdotc_sub]

(* ****** ****** *)

//
// DOTU: C, Z // extended with S, D
//

fun{a:t@ype}
cblas_dotu // dot product unconjugated
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx), incX: int (dx)
, Y: &GEVEC (a, n, dy), incY: int (dy)
) :<> a // end of [cblas_dotu]

//
// DOTC: C, Z // extended with S, D
//

fun{a:t@ype}
cblas_dotc // dot product conjugated
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx), incX: int (dx)
, Y: &GEVEC (a, n, dy), incY: int (dy)
) :<> a // end of [cblas_dotc]

(* ****** ****** *)

//
// SNRM2: S, D, SC, DZ
//

(*

float  cblas_snrm2(const int N, const float *X, const int incX);
double cblas_dnrm2(const int N, const double *X, const int incX);
float  cblas_scnrm2(const int N, const void *X, const int incX);
double cblas_dznrm2(const int N, const void *X, const int incX);

*)

fun{a1,a2:t@ype}
cblas_nrm2 {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (a2, n, dx), incX: int dx
) :<> a1 // end of [atsctrb_cblas_nrm2]

fun cblas_snrm2
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (float, n, dx), incX: int dx
) :<> float
  = "mac#atsctrb_cblas_snrm2"
// end of [cblas_snrm2]

fun cblas_dnrm2
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (double, n, dx), incX: int dx
) :<> double
  = "mac#atsctrb_cblas_dnrm2"
// end of [cblas_dnrm2]

fun cblas_scnrm2
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> float
  = "mac#atsctrb_cblas_scnrm2"
// end of [cblas_scnrm2]

fun cblas_dznrm2
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> double
  = "mac#atsctrb_cblas_dznrm2"
// end of [cblas_dznrm2]

(* ****** ****** *)

//
// ASUM: S, D, SC, DZ
//

(*

float  cblas_sasum(const int N, const float *X, const int incX);
double cblas_dasum(const int N, const double *X, const int incX);
float  cblas_scasum(const int N, const void *X, const int incX);
double cblas_dzasum(const int N, const void *X, const int incX);

*)

fun{a1,a2:t@ype}
cblas_asum // 1-norm of a vector
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (a1, n, dx), incX: int dx
) :<> a2
// end of [atsctrb_cblas_asum]

fun cblas_sasum // 1-norm of a vector in single precision
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (float, n, dx), incX: int dx
) :<> float
  = "mac#atsctrb_cblas_sasum"
// end of [cblas_sasum]

fun cblas_dasum // 1-norm of a vector in double precision
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (double, n, dx), incX: int dx
) :<> double
  = "mac#atsctrb_cblas_dasum"
// end of [cblas_dasum]

fun cblas_scasum // 1-norm of a vector in single precision complex
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> float
  = "mac#atsctrb_cblas_scasum"
// end of [cblas_scasum]

fun cblas_dzasum // 1-norm of a vector in double precision complex
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> double
  = "mac#atsctrb_cblas_dzasum"
// end of [cblas_dzasum]

(* ****** ****** *)

//
// IAMAX: S, D, C, Z
//

(*

#define CBLAS_INDEX int
CBLAS_INDEX cblas_isamax(const int N, const float  *X, const int incX);
CBLAS_INDEX cblas_idamax(const int N, const double *X, const int incX);
CBLAS_INDEX cblas_icamax(const int N, const void   *X, const int incX);
CBLAS_INDEX cblas_izamax(const int N, const void   *X, const int incX);

*)

typedef CBLAS_INDEX = int // should it be made abstract?

fun{a:t@ype}
cblas_iamax // arg infty-norm of a vector
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (a, n, dx), incX: int dx
) :<> CBLAS_INDEX // end of [atsctrb_cblas_iamax]

fun cblas_isamax // arg infty-norm of a vector in single precision
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (float, n, dx), incX: int dx
) :<> CBLAS_INDEX
  = "mac#atsctrb_cblas_isamax"
// end of [fun]

fun cblas_idamax // arg infty-norm of a vector in double precision
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (double, n, dx), incX: int dx
) :<> CBLAS_INDEX
  = "mac#atsctrb_cblas_idamax"
// end of [fun]

fun cblas_icamax // arg infty-norm of a vector in single precision complex
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> CBLAS_INDEX
  = "mac#atsctrb_cblas_icamax"
// end of [fun]

fun cblas_izamax // arg infty-norm of a vector in double precision complex
  {n:nat} {dx:inc} (
  N: int n, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> CBLAS_INDEX
  = "mac#atsctrb_cblas_izamax"
// end of [fun]

(* ****** ****** *)

//
// SWAP: S, D, C, Z
//

(*

void cblas_dswap (
  const int N
, double *X, const int incX
, double *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_swap // X <-> Y
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy
) :<> void // end of [cblas_swap]

fun cblas_sswap
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (float, n, dx), incX: int dx
, Y: &GEVEC (float, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_sswap"
// end of [fun]

fun cblas_dswap
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (double, n, dx), incX: int dx
, Y: &GEVEC (double, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_dswap"
// end of [fun]

fun cblas_cswap
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_cswap"
// end of [fun]

fun cblas_zswap
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_zswap"
// end of [fun]

(* ****** ****** *)

//
// COPY: S, D, C, Z
//

(*

void cblas_dcopy (
  const int N
, const double *X
, const int incX
, double *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_copy // copy vector X to Y (Y <- X)
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (a, n, dx)
, incX: int dx
, Y: &GEVEC (a?, n, dy) >> GEVEC (a, n, dy)
, incY: int dy
) :<> void // end of [cblas_copy]

fun cblas_scopy
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (float, n, dx)
, incX: int dx
, Y: &GEVEC (float?, n, dy) >> GEVEC (float, n, dy)
, incY: int dy
) :<> void
  = "mac#atsctrb_cblas_scopy"
// end of [fun]

fun cblas_dcopy
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (double, n, dx)
, incX: int dx
, Y: &GEVEC (double?, n, dy) >> GEVEC (double, n, dy)
, incY: int dy
) :<> void
  = "mac#atsctrb_cblas_dcopy"
// end of [fun]

fun cblas_ccopy
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (ccmplx, n, dx)
, incX: int dx
, Y: &GEVEC (ccmplx?, n, dy) >> GEVEC (ccmplx, n, dy)
, incY: int dy
) :<> void
  = "mac#atsctrb_cblas_ccopy"
// end of [fun]

fun cblas_zcopy
  {n:nat} {dx,dy:inc} (
  N: int n
, X: &GEVEC (zcmplx, n, dx)
, incX: int dx
, Y: &GEVEC (zcmplx?, n, dy) >> GEVEC (zcmplx, n, dy)
, incY: int dy
) :<> void
  = "mac#atsctrb_cblas_zcopy"
// end of [fun]

(* ****** ****** *)

//
// AXPY: S, D, C, Z
//

(*

void cblas_daxpy (
  const int N
, const double alpha
, const double *X, const int incX
, double *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_axpy // Y <- alpha * X + Y
  {n:nat} {dx,dy:inc} (
  N: int n
, alpha: a
, X: &GEVEC (a, n, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy
) :<> void // end of [cblas_axpy]

fun cblas_saxpy
  {n:nat} {dx,dy:inc} (
  N: int n
, alpha: float
, X: &GEVEC (float, n, dx), incX: int dx
, Y: &GEVEC (float, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_saxpy"
// end of [fun]

fun cblas_daxpy
  {n:nat} {dx,dy:inc} (
  N: int n
, alpha: double
, X: &GEVEC (double, n, dx), incX: int dx
, Y: &GEVEC (double, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_daxpy"
// end of [fun]

fun cblas_caxpy
  {n:nat} {dx,dy:inc} (
  N: int n
, alpha: &ccmplx
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_caxpy"
// end of [fun]

fun cblas_zaxpy
  {n:nat} {dx,dy:inc} (
  N: int n
, alpha: &zcmplx
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_zaxpy"
// end of [fun]

(* ****** ****** *)

//
// SCAL: S, D, C, Z, CS, ZD
//

(*
void cblas_sscal(const int N, const float alpha, float *X, const int incX);
void cblas_dscal(const int N, const double alpha, double *X, const int incX);
void cblas_cscal(const int N, const void *alpha, void *X, const int incX);
void cblas_zscal(const int N, const void *alpha, void *X, const int incX);
void cblas_csscal(const int N, const float alpha, void *X, const int incX);
void cblas_zdscal(const int N, const double alpha, void *X, const int incX);
*)

fun{a1,a2:t@ype}
cblas_scal // X <- alpha * X
  {n:nat} {dx:inc} (
  N: int n, alpha: a2
, X: &GEVEC (a1, n, dx), incX: int (dx)
) :<> void // end of [cblas_scal]

fun cblas_sscal
  {n:nat} {dx:inc} (
  N: int n, alpha: float
, X: &GEVEC (float, n, dx), incX: int (dx)
) :<> void
  = "mac#atsctrb_cblas_sscal"
// end of [fun]

fun cblas_dscal
  {n:nat} {dx:inc} (
  N: int n, alpha: double
, X: &GEVEC (double, n, dx), incX: int (dx)
) :<> void
  = "mac#atsctrb_cblas_dscal"
// end of [fun]

fun cblas_cscal
  {n:nat} {dx:inc} (
  N: int n, alpha: &ccmplx
, X: &GEVEC (ccmplx, n, dx), incX: int (dx)
) :<> void
  = "mac#atsctrb_cblas_cscal"
// end of [fun]

fun cblas_zscal
  {n:nat} {dx:inc} (
  N: int n, alpha: &zcmplx
, X: &GEVEC (zcmplx, n, dx), incX: int (dx)
) :<> void
  = "mac#atsctrb_cblas_zscal"
// end of [fun]

fun cblas_csscal
  {n:nat} {dx:inc} (
  N: int n, alpha: float
, X: &GEVEC (ccmplx, n, dx), incX: int (dx)
) :<> void
  = "mac#atsctrb_cblas_csscal"
// end of [fun]

fun cblas_zdscal
  {n:nat} {dx:inc} (
  N: int n, alpha: double
, X: &GEVEC (zcmplx, n, dx), incX: int (dx)
) :<> void
  = "mac#atsctrb_cblas_zdscal"
// end of [fun]

(* ****** ****** *)

//
// BLAS level 2
//

(* ****** ****** *)

//
// GEMV: S, D, C, Z
//

(*

void cblas_dgemv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_TRANSPOSE TransA
, const int M, const int N
, const double alpha
, const double *A, const int lda
, const double *X, const int incX
, const double beta
, double *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_gemv
  {ord:order} {trA:transpose}
  {ma,na:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, alpha: a
, A: &GEMAT (a, ma, na, ord, lda), lda: int lda
, X: &GEVEC (a, nx, dx), incX: int dx
, beta: a
, Y: &GEVEC (a, ny, dy), incY: int dy
) :<> void // end of [cblas_gemv]

fun cblas_sgemv
  {ord:order} {trA:transpose}
  {ma,na:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, alpha: float
, A: &GEMAT (float, ma, na, ord, lda), lda: int lda
, X: &GEVEC (float, nx, dx), incX: int dx
, beta: float
, Y: &GEVEC (float, ny, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_sgemv"
// end of [fun]

fun cblas_dgemv
  {ord:order} {trA:transpose}
  {ma,na:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, alpha: double
, A: &GEMAT (double, ma, na, ord, lda), lda: int lda
, X: &GEVEC (double, nx, dx), incX: int dx
, beta: double
, Y: &GEVEC (double, ny, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_dgemv"
// end of [fun]

fun cblas_cgemv
  {ord:order} {trA:transpose}
  {ma,na:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, alpha: &ccmplx
, A: &GEMAT (ccmplx, ma, na, ord, lda), lda: int lda
, X: &GEVEC (ccmplx, nx, dx), incX: int dx
, beta: &ccmplx
, Y: &GEVEC (ccmplx, ny, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_cgemv"
// end of [fun]

fun cblas_zgemv
  {ord:order} {trA:transpose}
  {ma,na:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, alpha: &zcmplx
, A: &GEMAT (zcmplx, ma, na, ord, lda), lda: int lda
, X: &GEVEC (zcmplx, nx, dx), incX: int dx
, beta: &zcmplx
, Y: &GEVEC (zcmplx, ny, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_zgemv"
// end of [fun]

(* ****** ****** *)

//
// GBMV: S, D, C, Z
//

(*

void cblas_dgbmv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_TRANSPOSE TransA
, const int M, const int N
, const int KL, const int KU
, const double alpha
, const double *A, const int lda
, const double *X, const int incX
, const double beta
, double *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_gbmv
  {ord:order} {trA:transpose}
  {ma,na:nat} {kl,ku:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, KL: int kl, KU: int ku
, alpha: a
, A: &GBMAT (a, ma, na, ord, kl, ku, lda), lda: int lda
, X: &GEVEC (a, nx, dx), incX: int dx
, beta: a
, Y: &GEVEC (a, ny, dy), incY: int dy
) :<> void // end of [cblas_gbmv]

fun cblas_sgbmv
  {ord:order} {trA:transpose}
  {ma,na:nat} {kl,ku:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, KL: int kl, KU: int ku
, alpha: float
, A: &GBMAT (float, ma, na, ord, kl, ku, lda), lda: int lda
, X: &GEVEC (float, nx, dx), incX: int dx
, beta: float
, Y: &GEVEC (float, ny, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_sgbmv"
// end of [fun]

fun cblas_dgbmv
  {ord:order} {trA:transpose}
  {ma,na:nat} {kl,ku:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, KL: int kl, KU: int ku
, alpha: double
, A: &GBMAT (double, ma, na, ord, kl, ku, lda), lda: int lda
, X: &GEVEC (double, nx, dx), incX: int dx
, beta: double
, Y: &GEVEC (double, ny, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_dgbmv"
// end of [fun]

fun cblas_cgbmv
  {ord:order} {trA:transpose}
  {ma,na:nat} {kl,ku:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, KL: int kl, KU: int ku
, alpha: &ccmplx
, A: &GBMAT (ccmplx, ma, na, ord, kl, ku, lda), lda: int lda
, X: &GEVEC (ccmplx, nx, dx), incX: int dx
, beta: &ccmplx
, Y: &GEVEC (ccmplx, ny, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_cgbmv"
// end of [fun]

fun cblas_zgbmv
  {ord:order} {trA:transpose}
  {ma,na:nat} {kl,ku:nat} {lda:pos}
  {nx,ny:nat} {dx,dy:inc} (
  pfa: trandim_p (trA, ma, na, ny, nx)
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, M: int ma, N: int na
, KL: int kl, KU: int ku
, alpha: &zcmplx
, A: &GBMAT (zcmplx, ma, na, ord, kl, ku, lda), lda: int lda
, X: &GEVEC (zcmplx, nx, dx), incX: int dx
, beta: &zcmplx
, Y: &GEVEC (zcmplx, ny, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_zgbmv"
// end of [fun]

(* ****** ****** *)

//
// TRMV: S, D, C, Z
//

(*

void cblas_dtrmv(
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_DIAG Diag,
, const int N
, const double *A, const int lda
, double *X, const int incX
) ;

*)

fun{a:t@ype}
cblas_trmv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (a, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (a, n, dx), incX: int dx
) :<> void // end of [cblas_trmv]

fun cblas_strmv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (float, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (float, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_strmv"
// end of [fun]

fun cblas_dtrmv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (double, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (double, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_dtrmv"
// end of [fun]

fun cblas_ctrmv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (ccmplx, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ctrmv"
// end of [fun]

fun cblas_ztrmv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (zcmplx, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ztrmv"
// end of [fun]

(* ****** ****** *)

//
// TBMV: S, D, C, Z
//

(*

void cblas_dtbmv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_DIAG Diag
, const int N, const int K
, const double *A, const int lda
, double *X, const int incX
) ;

*)

fun{a:t@ype}
cblas_tbmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (a, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (a, n, dx), incX: int dx
) :<> void // end of [cblas_tbmv]

fun cblas_stbmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (float, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (float, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_stbmv"
// end of [fun]

fun cblas_dtbmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (double, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (double, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_dtbmv"
// end of [fun]

fun cblas_ctbmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (ccmplx, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ctbmv"
// end of [fun]

fun cblas_ztbmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (zcmplx, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ztbmv"
// end of [fun]

(* ****** ****** *)

//
// TPMV: S, D, C, Z
//

(*

void cblas_dtpmv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_DIAG Diag
, const int N
, const double *Ap
, double *X, const int incX
) ;

*)

fun{a:t@ype}
cblas_tpmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, Ap: &TPMAT (a, n, ord, ul, dg)
, X: &GEVEC (a, n, dx), incX: int dx
) :<> void // end of [cblas_tpmv]

fun cblas_stpmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, Ap: &TPMAT (float, n, ord, ul, dg)
, X: &GEVEC (float, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_stpmv"
// end of [fun]

fun cblas_dtpmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, Ap: &TPMAT (double, n, ord, ul, dg)
, X: &GEVEC (double, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_dtpmv"
// end of [fun]

fun cblas_ctpmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n
, Ap: &TPMAT (ccmplx, n, ord, ul, dg)
, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ctpmv"
// end of [fun]

fun cblas_ztpmv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n
, Ap: &TPMAT (zcmplx, n, ord, ul, dg)
, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ztpmv"
// end of [fun]

(* ****** ****** *)

//
// TRSV: S, D, C, Z
//

(*

void cblas_dtrsv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_DIAG Diag
, const int N
, const double *A, const int lda
, double *X, const int incX
) ;

*)

fun{a:t@ype}
cblas_trsv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (a, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (a, n, dx), incX: int dx
) :<> void // end of [cblas_trsv]

fun cblas_strsv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (float, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (float, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_strsv"
// end of [fun]

fun cblas_dtrsv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (double, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (double, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_dtrsv"
// end of [fun]

fun cblas_ctrsv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (ccmplx, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ctrsv"
// end of [fun]

fun cblas_ztrsv
  {ord:order} {ul:uplo}
  {trA:transpose} {dg:diag}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, A: &TRMAT (zcmplx, n, ord, ul, dg, lda), lda: int lda
, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ztrsv"
// end of [fun]

(* ****** ****** *)

//
// TBSV: S, D, C, Z
//

(*

void cblas_dtbsv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_DIAG Diag
, const int N, const int K
, const double *A, const int lda
, double *X, const int incX
) ;

*)

fun{a:t@ype}
cblas_tbsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (a, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (a, n, dx), incX: int dx
) :<> void // end of [cblas_tbsv]

fun cblas_stbsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (float, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (float, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_stbsv"
// end of [fun]

fun cblas_dtbsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (double, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (double, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_dtbsv"
// end of [fun]

fun cblas_ctbsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (ccmplx, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ctbsv"
// end of [fun]

fun cblas_ztbsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {k:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n, K: int k
, A: &TBMAT (zcmplx, n, ord, ul, dg, k, lda), lda: int lda
, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ztbsv"
// end of [fun]

(* ****** ****** *)

//
// TPSV: S, D, C, Z
//

(*

void cblas_dtpsv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_DIAG Diag
, const int N
, const double *Ap
, double *X, const int incX
) ;

*)

fun{a:t@ype}
cblas_tpsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, Ap: &TPMAT (a, n, ord, ul, dg)
, X: &GEVEC (a, n, dx), incX: int dx
) :<> void // end of [cblas_tpsv]

fun cblas_stpsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, Ap: &TPMAT (float, n, ord, ul, dg)
, X: &GEVEC (float, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_stpsv"
// end of [fun]

fun cblas_dtpsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, TransA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, N: int n
, Ap: &TPMAT (double, n, ord, ul, dg)
, X: &GEVEC (double, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_dtpsv"
// end of [fun]

fun cblas_ctpsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n
, Ap: &TPMAT (ccmplx, n, ord, ul, dg)
, X: &GEVEC (ccmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ctpsv"
// end of [fun]

fun cblas_ztpsv
  {ord:order} {ul:uplo} {dg:diag} {trA:transpose}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, TransA: CBLAS_TRANSPOSE_t trA
, Diag: CBLAS_DIAG_t dg
, N: int n
, Ap: &TPMAT (zcmplx, n, ord, ul, dg)
, X: &GEVEC (zcmplx, n, dx), incX: int dx
) :<> void
  = "mac#atsctrb_cblas_ztpsv"
// end of [fun]

(* ****** ****** *)

//
// SYMV: S, D
//

(*

void cblas_dsymv(
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const double alpha
, const double *A, const int lda
, const double *X, const int incX,
, const double beta
, double *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_symv
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a
, A: &SYMAT (a, n, ord, ul, lda), lda: int lda
, X: &GEVEC (a, n, dx), incX: int dx
, beta: a
, Y: &GEVEC (a, n, dy), incY: int dy
) :<> void // end of [cblas_symv]

fun cblas_ssymv
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: float
, A: &SYMAT (float, n, ord, ul, lda), lda: int lda
, X: &GEVEC (float, n, dx), incX: int dx
, beta: float
, Y: &GEVEC (float, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_ssymv"
// end of [fun]

fun cblas_dsymv
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: double
, A: &SYMAT (double, n, ord, ul, lda), lda: int lda
, X: &GEVEC (double, n, dx), incX: int dx
, beta: double
, Y: &GEVEC (double, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_dsymv"
// end of [fun]

(* ****** ****** *)

//
// SBMV: S, D
//

(*

void cblas_dsbmv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N, const int K
, const double alpha
, const double *A, const int lda
, const double *X, const int incX
, const double beta
, float *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_sbmv
  {ord:order} {ul:uplo}
  {n,k:nat | k < n} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, N: int n, K: int k
, alpha: a
, A: &SBMAT (a, n, ord, ul, k, lda), lda: int lda
, X: &GEVEC (a, n, dx), incX: int dx
, beta: a
, Y: &GEVEC (a, n, dy), incY: int dy
) :<> void // end of [cblas_sbmv]

fun cblas_ssbmv
  {ord:order} {ul:uplo}
  {n,k:nat | k < n} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, N: int n, K: int k
, alpha: float
, A: &SBMAT (float, n, ord, ul, k, lda), lda: int lda
, X: &GEVEC (float, n, dx), incX: int dx
, beta: float
, Y: &GEVEC (float, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_ssbmv"
// end of [cblas_ssbmv]

fun cblas_dsbmv
  {ord:order} {ul:uplo}
  {n,k:nat | k < n} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, N: int n, K: int k
, alpha: double
, A: &SBMAT (double, n, ord, ul, k, lda), lda: int lda
, X: &GEVEC (double, n, dx), incX: int dx
, beta: double
, Y: &GEVEC (double, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_dsbmv"
// end of [cblas_dsbmv]

(* ****** ****** *)

//
// SPMV: S, D // HPMV: C, Z
//

(*

void cblas_dspmv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const double alpha
, const double *Ap
, const double *X, const int incX
, const double beta
, double *Y, const int incY ) ;

*)

fun{a:t@ype}
cblas_spmv
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, N: int n
, alpha: a
, Ap: &SPMAT (a, n, ord, ul)
, X: &GEVEC (a, n, dx), incX: int dx
, beta: a
, Y: &GEVEC (a, n, dy), incY: int dy
) :<> void // end of [cblas_spmv]

fun cblas_sspmv
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, N: int n
, alpha: float
, Ap: &SPMAT (float, n, ord, ul)
, X: &GEVEC (float, n, dx), incX: int dx
, beta: float
, Y: &GEVEC (float, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_sspmv"
// end of [fun]

fun cblas_dspmv
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, N: int n
, alpha: double
, Ap: &SPMAT (double, n, ord, ul)
, X: &GEVEC (double, n, dx), incX: int dx
, beta: double
, Y: &GEVEC (double, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_dspmv"
// end of [fun]

(* ****** ****** *)

//
// GER: S, D
//

(*

void cblas_dger(
  const enum CBLAS_ORDER Order
, const int M, const int N
, const double alpha
, const double *X, const int incX
, const double *Y, const int incY
, double *A, const int lda
) ;

*)

fun{a:t@ype}
cblas_ger
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: a
, X: &GEVEC (a, m, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy  
, A: &GEMAT (a, m, n, ord, lda), lda: int lda
) :<> void // end of [cblas_ger]

fun cblas_sger
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: float
, X: &GEVEC (float, m, dx), incX: int dx
, Y: &GEVEC (float, n, dy), incY: int dy  
, A: &GEMAT (float, m, n, ord, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_sger"
// end of [cblas_sger]

fun cblas_dger
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: double
, X: &GEVEC (double, m, dx), incX: int dx
, Y: &GEVEC (double, n, dy), incY: int dy  
, A: &GEMAT (double, m, n, ord, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_dger"
// end of [cblas_dger]

(* ****** ****** *)

//
// SYR: S, D
//

(*

void cblas_dsyr (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const double alpha
, const double *X, const int incX
, double *A, const int lda
) ;

*)

fun{a:t@ype}
cblas_syr
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a
, X: &GEVEC (a, n, dx), incX: int dx
, A: &SYMAT (a, n, ord, ul, lda), lda: int lda
) :<> void // end of [cblas_syr]

fun cblas_ssyr
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: float
, X: &GEVEC (float, n, dx), incX: int dx
, A: &SYMAT (float, n, ord, ul, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_ssyr"
// end of [cblas_ssyr]

fun cblas_dsyr
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: double
, X: &GEVEC (double, n, dx), incX: int dx
, A: &SYMAT (double, n, ord, ul, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_dsyr"
// end of [cblas_dsyr]

(* ****** ****** *)

//
// SYR2: S, D
//

fun{a:t@ype}
cblas_syr2
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a
, X: &GEVEC (a, n, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy
, A: &SYMAT (a, n, ord, ul, lda), lda: int lda
) :<> void // end of [cblas_syr2]

fun cblas_ssyr2
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: float
, X: &GEVEC (float, n, dx), incX: int dx
, Y: &GEVEC (float, n, dy), incY: int dy
, A: &SYMAT (float, n, ord, ul, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_ssyr2"
// end of [cblas_ssyr2]

fun cblas_dsyr2
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: double
, X: &GEVEC (double, n, dx), incX: int dx
, Y: &GEVEC (double, n, dy), incY: int dy
, A: &SYMAT (double, n, ord, ul, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_dsyr2"
// end of [cblas_dsyr2]

(* ****** ****** *)

//
// SPR: S, D
//

(*

void cblas_dspr (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const double alpha
, const double *X, const int incX
, double *Ap
) ;

*)

fun{a:t@ype}
cblas_spr
  {ord:order} {ul:uplo}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a
, X: &GEVEC (a, n, dx), incX: int dx
, Ap: &SPMAT (a, n, ord, ul)
) :<> void // end of [cblas_spr]

fun cblas_sspr
  {ord:order} {ul:uplo}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: float
, X: &GEVEC (float, n, dx), incX: int dx
, Ap: &SPMAT (float, n, ord, ul)
) :<> void
  = "mac#atsctrb_cblas_sspr"
// end of [cblas_sspr]

fun cblas_dspr
  {ord:order} {ul:uplo}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: double
, X: &GEVEC (double, n, dx), incX: int dx
, Ap: &SPMAT (double, n, ord, ul)
) :<> void
  = "mac#atsctrb_cblas_dspr"
// end of [cblas_dspr]

(* ****** ****** *)

//
// SPR2: S, D
//

(*

void cblas_dspr2 (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const double alpha
, const double *X, const int incX
, const double *Y, const int incY
, double *A
) ;

*)

fun{a:t@ype}
cblas_spr2
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a
, X: &GEVEC (a, n, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy
, A: &SPMAT (a, n, ord, ul)
) :<> void // end of [cblas_spr2]

fun cblas_sspr2
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: float
, X: &GEVEC (float, n, dx), incX: int dx
, Y: &GEVEC (float, n, dy), incY: int dy
, A: &SPMAT (float, n, ord, ul)
) :<> void
  = "mac#atsctrb_cblas_sspr2"
// end of [cblas_sspr2]

fun cblas_dspr2
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: double
, X: &GEVEC (double, n, dx), incX: int dx
, Y: &GEVEC (double, n, dy), incY: int dy
, A: &SPMAT (double, n, ord, ul)
) :<> void
  = "mac#atsctrb_cblas_dspr2"
// end of [cblas_dspr2]

(* ****** ****** *)

//
// HEMV: C, Z // extended with S, D
//

(*

void cblas_chemv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const void *alpha
, const void *A, const int lda
, const void *X, const int incX
, const void *beta
, void *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_hemv
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a
, A: &HEMAT (a, n, ord, ul, lda), lda: int lda
, X: &GEVEC (a, n, dx), incX: int dx
, beta: a
, Y: &GEVEC (a, n, dy), incY: int dy
) :<> void // end of [cblas_hemv]

fun cblas_chemv
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: &ccmplx
, A: &HEMAT (ccmplx, n, ord, ul, lda), lda: int lda
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, beta: &ccmplx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_chemv"
// end of [fun]

fun cblas_zhemv
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: &zcmplx
, A: &HEMAT (zcmplx, n, ord, ul, lda), lda: int lda
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, beta: &zcmplx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_zhemv"
// end of [fun]

(* ****** ****** *)

//
// HBMV: C, Z // extended with S, D
//

(*

void cblas_chbmv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N, const int K
, const void *alpha
, const void *A, const int lda
, const void *X, const int incX
, const void *beta
, void *Y, const int incY
) ;

*)

fun{a:t@ype}
cblas_hbmv
  {ord:order} {ul:uplo}
  {n,k:nat | k < n} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, N: int n, K: int k
, alpha: a
, A: &HBMAT (a, n, ord, ul, k, lda), lda: int lda
, X: &GEVEC (a, n, dx), incX: int dx
, beta: a
, Y: &GEVEC (a, n, dy), incY: int dy
) :<> void // end of [cblas_hbmv]

fun cblas_chbmv
  {ord:order} {ul:uplo}
  {n,k:nat | k < n} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, N: int n, K: int k
, alpha: &ccmplx
, A: &HBMAT (ccmplx, n, ord, ul, k, lda), lda: int lda
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, beta: &ccmplx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_chbmv"
// end of [cblas_chbmv]

fun cblas_zhbmv
  {ord:order} {ul:uplo}
  {n,k:nat | k < n} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, N: int n, K: int k
, alpha: &zcmplx
, A: &HBMAT (zcmplx, n, ord, ul, k, lda), lda: int lda
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, beta: &zcmplx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_zhbmv"
// end of [cblas_zhbmv]

(* ****** ****** *)

//
// HPMV: C, Z // extended with S, D
//

(*

void cblas_chpmv (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const void *alpha
, const void *Ap
, const void *X, const int incX
, const void *beta, void *Y, const int incY ) ;

*)

fun{a:t@ype}
cblas_hpmv
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, N: int n
, alpha: a
, Ap: &HPMAT (a, n, ord, ul)
, X: &GEVEC (a, n, dx), incX: int dx
, beta: a
, Y: &GEVEC (a, n, dy), incY: int dy
) :<> void // end of [cblas_hpmv]

fun cblas_chpmv
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, N: int n
, alpha: &ccmplx
, Ap: &HPMAT (ccmplx, n, ord, ul)
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, beta: &ccmplx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_chpmv"
// end of [fun]

fun cblas_zhpmv
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t ord
, Uplo: CBLAS_UPLO_t ul
, N: int n
, alpha: &zcmplx
, Ap: &HPMAT (zcmplx, n, ord, ul)
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, beta: &zcmplx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy
) :<> void
  = "mac#atsctrb_cblas_zhpmv"
// end of [fun]

(* ****** ****** *)

//
// GERU: C, Z // extended with S, D
//

(*

void cblas_cgeru(
  const enum CBLAS_ORDER Order
, const int M, const int N
, const void *alpha
, const void *X, const int incX
, const void *Y, const int incY
, void *A, const int lda
) ;

*)

fun{a:t@ype}
cblas_geru
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: a
, X: &GEVEC (a, m, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy  
, A: &GEMAT (a, m, n, ord, lda), lda: int lda
) :<> void // end of [cblas_geru]

fun cblas_cgeru
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: &ccmplx
, X: &GEVEC (ccmplx, m, dx), incX: int dx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy  
, A: &GEMAT (ccmplx, m, n, ord, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_cgeru"
// end of [fun]

fun cblas_zgeru
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: &zcmplx
, X: &GEVEC (zcmplx, m, dx), incX: int dx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy  
, A: &GEMAT (zcmplx, m, n, ord, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_zgeru"
// end of [fun]

(* ****** ****** *)

//
// GERU: C, Z // extended with S, D
//

(*

void cblas_cgerc(
  const enum CBLAS_ORDER Order
, const int M, const int N
, const void *alpha
, const void *X, const int incX
, const void *Y, const int incY
, void *A, const int lda
) ;

*)

fun{a:t@ype}
cblas_gerc
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: a
, X: &GEVEC (a, m, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy  
, A: &GEMAT (a, m, n, ord, lda), lda: int lda
) :<> void // end of [cblas_gerc]

fun cblas_cgerc
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: &ccmplx
, X: &GEVEC (ccmplx, m, dx), incX: int dx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy  
, A: &GEMAT (ccmplx, m, n, ord, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_cgerc"
// end of [fun]

fun cblas_zgerc
  {ord:order} {m,n:nat}
  {dx,dy:inc} {lda:pos} (
  Order: CBLAS_ORDER_t (ord)
, M: int m, N: int n
, alpha: &zcmplx
, X: &GEVEC (zcmplx, m, dx), incX: int dx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy  
, A: &GEMAT (zcmplx, m, n, ord, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_zgerc"
// end of [fun]

(* ****** ****** *)

//
// HER: C, Z // extended with S, D
//

(*

void cblas_cher(
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const float alpha
, const void *X, const int incX
, void *A, const int lda
) ;

*)

fun{a1,a2:t@ype}
cblas_her
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a1
, X: &GEVEC (a2, n, dx), incX: int dx
, A: &HEMAT (a2, n, ord, ul, lda), lda: int lda
) :<> void // end of [cblas_her]

fun cblas_cher
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: float
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, A: &HEMAT (ccmplx, n, ord, ul, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_cher"
// end of [cblas_cher]

fun cblas_zher
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: double
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, A: &HEMAT (zcmplx, n, ord, ul, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_zher"
// end of [cblas_zher]

(* ****** ****** *)

//
// HER2: C, Z // extended with S, D
//

(*

void cblas_cher2 (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const void *alpha
, const void *X, const int incX
, const void *Y, const int incY
, void *A, const int lda
) ;

*)

fun{a:t@ype}
cblas_her2
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a
, X: &GEVEC (a, n, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy
, A: &HEMAT (a, n, ord, ul, lda), lda: int lda
) :<> void // end of [cblas_her2]

fun cblas_cher2
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: &ccmplx
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy
, A: &HEMAT (ccmplx, n, ord, ul, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_cher2"
// end of [cblas_cher2]

fun cblas_zher2
  {ord:order} {ul:uplo}
  {n:nat} {lda:pos} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: &zcmplx
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy
, A: &HEMAT (zcmplx, n, ord, ul, lda), lda: int lda
) :<> void
  = "mac#atsctrb_cblas_zher2"
// end of [cblas_zher2]

(* ****** ****** *)

//
// HPR: C, Z // extended with S, D
//

(*

void cblas_chpr (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const float alpha
, const void *X, const int incX
, void *A
) ;

*)

fun{a1,a2:t@ype}
cblas_hpr
  {ord:order} {ul:uplo}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a1
, X: &GEVEC (a2, n, dx), incX: int dx
, A: &HPMAT (a2, n, ord, ul)
) :<> void // end of [cblas_hpr]

fun cblas_chpr
  {ord:order} {ul:uplo}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: float
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, A: &HPMAT (ccmplx, n, ord, ul)
) :<> void
  = "mac#atsctrb_cblas_chpr"
// end of [cblas_chpr]

fun cblas_zhpr
  {ord:order} {ul:uplo}
  {n:nat} {dx:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: double
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, A: &HPMAT (zcmplx, n, ord, ul)
) :<> void
  = "mac#atsctrb_cblas_zhpr"
// end of [cblas_zhpr]

(* ****** ****** *)

//
// HPR2: C, Z // extended with S, D
//

(*

void cblas_chpr2 (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const int N
, const void *alpha
, const void *X, const int incX
, const void *Y, const int incY
, void *Ap
) ;

*)

fun{a:t@ype}
cblas_hpr2
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: a
, X: &GEVEC (a, n, dx), incX: int dx
, Y: &GEVEC (a, n, dy), incY: int dy
, Ap: &HPMAT (a, n, ord, ul)
) :<> void // end of [cblas_hpr2]

fun cblas_chpr2
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: &ccmplx
, X: &GEVEC (ccmplx, n, dx), incX: int dx
, Y: &GEVEC (ccmplx, n, dy), incY: int dy
, Ap: &HPMAT (ccmplx, n, ord, ul)
) :<> void
  = "mac#atsctrb_cblas_chpr2"
// end of [cblas_chpr2]

fun cblas_zhpr2
  {ord:order} {ul:uplo}
  {n:nat} {dx,dy:inc} (
  Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)  
, N: int n
, alpha: &zcmplx
, X: &GEVEC (zcmplx, n, dx), incX: int dx
, Y: &GEVEC (zcmplx, n, dy), incY: int dy
, Ap: &HPMAT (zcmplx, n, ord, ul)
) :<> void
  = "mac#atsctrb_cblas_zhpr2"
// end of [cblas_zhpr2]

(* ****** ****** *)

//
// BLAS level 3
// 

(* ****** ****** *)

//
// GEMM: S, D, C, Z
//

(*

// taken from [cblas.h]
void cblas_dgemm (
  const enum CBLAS_ORDER Order
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_TRANSPOSE TransB
, const int M, const int N, const int K
, const double alpha
, const double *A, const int lda
, const double *B, const int ldb
, const double beta
, double *C, const int ldc
) ;

*)

fun{t:t@ype}
cblas_gemm
  {ord:order} {trA,trB:transpose}
  {m,n,k:nat} {ma,na:nat} {mb,nb:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, m, k)
, pfb: trandim_p (trB, mb, nb, k, n)   
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, TransB: CBLAS_TRANSPOSE_t (trB)
, M: int m, N: int n, K: int k
, alpha: t
, A: &GEMAT (t, ma, na, ord, lda), lda: int lda
, B: &GEMAT (t, mb, nb, ord, ldb), ldb: int ldb
, beta: t
, C: &GEMAT (t, m, n, ord, ldc), ldc: int ldc
) :<> void = "atsctrb_cblas_gemm"

fun cblas_sgemm
  {ord:order} {trA,trB:transpose}
  {m,n,k:nat} {ma,na:nat} {mb,nb:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, m, k)
, pfb: trandim_p (trB, mb, nb, k, n)   
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t trA
, TransB: CBLAS_TRANSPOSE_t trB
, M: int m, N: int n, K: int k
, alpha: float
, A: &GEMAT (float, ma, na, ord, lda), lda: int lda
, B: &GEMAT (float, mb, nb, ord, ldb), ldb: int ldb
, beta: float
, C: &GEMAT (float, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_sgemm"
// end of [fun]

fun cblas_dgemm
  {ord:order} {trA,trB:transpose}
  {m,n,k:nat} {ma,na:nat} {mb,nb:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, m, k)
, pfb: trandim_p (trB, mb, nb, k, n)   
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t trA
, TransB: CBLAS_TRANSPOSE_t trB
, M: int m, N: int n, K: int k
, alpha: double
, A: &GEMAT (double, ma, na, ord, lda), lda: int lda
, B: &GEMAT (double, mb, nb, ord, ldb), ldb: int ldb
, beta: double
, C: &GEMAT (double, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_dgemm"
// end of [fun]

fun cblas_cgemm
  {ord:order} {trA,trB:transpose}
  {m,n,k:nat} {ma,na:nat} {mb,nb:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, m, k)
, pfb: trandim_p (trB, mb, nb, k, n)   
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t trA
, TransB: CBLAS_TRANSPOSE_t trB
, M: int m, N: int n, K: int k
, alpha: &ccmplx
, A: &GEMAT (ccmplx, ma, na, ord, lda), lda: int lda
, B: &GEMAT (ccmplx, mb, nb, ord, ldb), ldb: int ldb
, beta: &ccmplx
, C: &GEMAT (ccmplx, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_cgemm"
// end of [fun]

fun cblas_zgemm
  {ord:order} {trA,trB:transpose}
  {m,n,k:nat} {ma,na:nat} {mb,nb:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, m, k)
, pfb: trandim_p (trB, mb, nb, k, n)   
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t trA
, TransB: CBLAS_TRANSPOSE_t trB
, M: int m, N: int n, K: int k
, alpha: &zcmplx
, A: &GEMAT (zcmplx, ma, na, ord, lda), lda: int lda
, B: &GEMAT (zcmplx, mb, nb, ord, ldb), ldb: int ldb
, beta: &zcmplx
, C: &GEMAT (zcmplx, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_zgemm"
// end of [fun]

//
// HX: this one is correct but too cumbersome to use!
//
fun{t:t@ype}
cblas_gemm__main
  {v:view}
  {ord:order} {trA,trB:transpose}
  {m,n,k:nat} {ma,na:nat} {mb,nb:nat}
  {lda,ldb,ldc:pos} {la,lb:addr} (
  pf: !v
, pfa1: trandim_p (trA, ma, na, m, k)
, pfa2: GEMAT_v (t, ma, na, ord, lda, la) <= v // containment
, pfb1: trandim_p (trB, mb, nb, k, n)   
, pfb2: GEMAT_v (t, mb, nb, ord, ldb, lb) <= v // containment
| Order: CBLAS_ORDER_t ord
, TransA: CBLAS_TRANSPOSE_t (trA)
, TransB: CBLAS_TRANSPOSE_t (trB)
, M: int m, N: int n, K: int k
, alpha: t
, pA: ptr la, lda: int lda
, pB: ptr lb, ldb: int ldb
, beta: t
, C: &GEMAT (t, m, n, ord, ldc), ldc: int ldc
) :<> void = "atsctrb_cblas_gemm"

(* ****** ****** *)

//
// SYRK: S, D, C, Z
//

(*

void cblas_dsyrk (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE Trans
, const int N, const int K
, const double alpha
, const double *A, const int lda
, const double beta
, double *C, const int ldc
) ;

*)

fun{a:t@ype}
cblas_syrk
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: a
, A: &GEMAT (a, ma, na, ord, lda), lda: int lda
, beta: a
, C: &SYMAT (a, n, ord, ul, ldc), ldc: int ldc
) :<> void // end of [cblas_syrk]

fun cblas_ssyrk
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: float
, A: &GEMAT (float, ma, na, ord, lda), lda: int lda
, beta: float
, C: &SYMAT (float, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_ssyrk"
// end of [fun]

fun cblas_dsyrk
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: double
, A: &GEMAT (double, ma, na, ord, lda), lda: int lda
, beta: double
, C: &SYMAT (double, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_dsyrk"
// end of [fun]

fun cblas_csyrk
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: &ccmplx
, A: &GEMAT (ccmplx, ma, na, ord, lda), lda: int lda
, beta: &ccmplx
, C: &SYMAT (ccmplx, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_csyrk"
// end of [fun]

fun cblas_zsyrk
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: &zcmplx
, A: &GEMAT (zcmplx, ma, na, ord, lda), lda: int lda
, beta: &zcmplx
, C: &SYMAT (zcmplx, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_zsyrk"
// end of [fun]

(* ****** ****** *)

//
// SYRK2: S, D, C, Z
//

(*

void cblas_dsyr2k (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE Trans
, const int N, const int K
, const double alpha
, const double *A, const int lda
, const double *B, const int ldb
, const double beta
, double *C, const int ldc
) ;

*)

fun{a:t@ype}
cblas_syr2k
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: a
, A: &GEMAT (a, ma, na, ord, lda), lda: int lda
, B: &GEMAT (a, ma, na, ord, ldb), ldb: int ldb
, beta: a
, C: &SYMAT (a, n, ord, ul, ldc), ldc: int ldc
) :<> void // end of [cblas_syr2k]

fun cblas_ssyr2k
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: float
, A: &GEMAT (float, ma, na, ord, lda), lda: int lda
, B: &GEMAT (float, ma, na, ord, ldb), ldb: int ldb
, beta: float
, C: &SYMAT (float, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_ssyr2k"
// end of [fun]

fun cblas_dsyr2k
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: double
, A: &GEMAT (double, ma, na, ord, lda), lda: int lda
, B: &GEMAT (double, ma, na, ord, ldb), ldb: int ldb
, beta: double
, C: &SYMAT (double, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_dsyr2k"
// end of [fun]

fun cblas_csyr2k
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: &ccmplx
, A: &GEMAT (ccmplx, ma, na, ord, lda), lda: int lda
, B: &GEMAT (ccmplx, ma, na, ord, ldb), ldb: int ldb
, beta: &ccmplx
, C: &SYMAT (ccmplx, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_csyr2k"
// end of [fun]

fun cblas_zsyr2k
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: &zcmplx
, A: &GEMAT (zcmplx, ma, na, ord, lda), lda: int lda
, B: &GEMAT (zcmplx, ma, na, ord, ldb), ldb: int ldb
, beta: &zcmplx
, C: &SYMAT (zcmplx, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_zsyr2k"
// end of [fun]

(* ****** ****** *)

//
// SYMM: S, D, C, Z
//

(*

void cblas_dsymm (
  const enum CBLAS_ORDER Order
, const enum CBLAS_SIDE Side
, const enum CBLAS_UPLO Uplo
, const int M, const int N
, const double alpha
, const double *A, const int lda
, const double *B, const int ldb
, const double beta
, double *C, const int ldc
) ;

*)

fun{a:t@ype}
cblas_symm
  {ord:order} {lr:side} {ul:uplo}
  {m,n:nat} {na:nat} {lda,ldb,ldc:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t ord
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, M: int m, N: int n
, alpha: a
, A: &SYMAT (a, na, ord, ul, lda), lda: int lda
, B: &GEMAT (a, m, n, ord, ldb), ldb: int ldb
, beta: a
, C: &GEMAT (a, m, n, ord, ldc), ldc: int ldc
) :<> void // end of [cblas_symm]

fun cblas_ssymm
  {ord:order} {lr:side} {ul:uplo}
  {m,n:nat} {na:nat} {lda,ldb,ldc:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t ord
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, M: int m, N: int n
, alpha: float
, A: &SYMAT (float, na, ord, ul, lda), lda: int lda
, B: &GEMAT (float, m, n, ord, ldb), ldb: int ldb
, beta: float
, C: &GEMAT (float, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_ssymm"
// end of [fun]

fun cblas_dsymm
  {ord:order} {lr:side} {ul:uplo}
  {m,n:nat} {na:nat} {lda,ldb,ldc:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t ord
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, M: int m, N: int n
, alpha: double
, A: &SYMAT (double, na, ord, ul, lda), lda: int lda
, B: &GEMAT (double, m, n, ord, ldb), ldb: int ldb
, beta: double
, C: &GEMAT (double, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_dsymm"
// end of [fun]

fun cblas_csymm
  {ord:order} {lr:side} {ul:uplo}
  {m,n:nat} {na:nat} {lda,ldb,ldc:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t ord
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, M: int m, N: int n
, alpha: &ccmplx
, A: &SYMAT (ccmplx, na, ord, ul, lda), lda: int lda
, B: &GEMAT (ccmplx, m, n, ord, ldb), ldb: int ldb
, beta: &ccmplx
, C: &GEMAT (ccmplx, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_csymm"
// end of [fun]

fun cblas_zsymm
  {ord:order} {lr:side} {ul:uplo}
  {m,n:nat} {na:nat} {lda,ldb,ldc:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t ord
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, M: int m, N: int n
, alpha: &zcmplx
, A: &SYMAT (zcmplx, na, ord, ul, lda), lda: int lda
, B: &GEMAT (zcmplx, m, n, ord, ldb), ldb: int ldb
, beta: &zcmplx
, C: &GEMAT (zcmplx, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_zsymm"
// end of [fun]

(* ****** ****** *)

//
// TRMM: S, D, C, Z
//

(*

// B <- alpha A B or B <- alpha B A
void cblas_dtrmm (
  const enum CBLAS_ORDER Order
, const enum CBLAS_SIDE Side
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_DIAG Diag
, const int M, const int N
, const double alpha
, const double *A, const int lda
, double *B, const int ldb
) ;

*)

fun{a:t@ype}
cblas_trmm
  {ord:order} {lr:side}
  {ul:uplo} {trA:transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: a
, A: &TRMAT (a, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (a, m, n, ord, ldb), ldb: int ldb
) :<> void // end of [cblas_trmm]

fun cblas_strmm
  {ord:order} {lr:side}
  {ul:uplo} {trA:transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: float
, A: &TRMAT (float, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (float, m, n, ord, ldb), ldb: int ldb
) :<> void
  = "mac#atsctrb_cblas_strmm"

fun cblas_dtrmm
  {ord:order} {lr:side}
  {ul:uplo} {trA:transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: double
, A: &TRMAT (double, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (double, m, n, ord, ldb), ldb: int ldb
) :<> void
  = "mac#atsctrb_cblas_dtrmm"
// end of [fun]

fun cblas_ctrmm
  {ord:order} {lr:side}
  {ul:uplo} {trA:transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: &ccmplx
, A: &TRMAT (ccmplx, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (ccmplx, m, n, ord, ldb), ldb: int ldb
) :<> void
  = "mac#atsctrb_cblas_ctrmm"
// end of [fun]

fun cblas_ztrmm
  {ord:order} {lr:side}
  {ul:uplo} {trA:transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: &zcmplx
, A: &TRMAT (zcmplx, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (zcmplx, m, n, ord, ldb), ldb: int ldb
) :<> void
  = "mac#atsctrb_cblas_ztrmm"
// end of [fun]

(* ****** ****** *)

//
// TRSM: S, D, C, Z
//

(*

// B <- alpha A^{-1} B or B <- alpha B A^{-1}
void cblas_dtrsm (
  const enum CBLAS_ORDER Order
, const enum CBLAS_SIDE Side
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE TransA
, const enum CBLAS_DIAG Diag
, const int M, const int N
, const double alpha
, const double *A, const int lda
, double *B, const int ldb
) ;

*)

fun{a:t@ype}
cblas_trsm
  {ord:order} {lr:side}
  {ul:uplo} {trA: transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: a
, A: &TRMAT (a, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (a, m, n, ord, ldb), ldb: int ldb
) :<> void // end of [cblas_trmm]

fun cblas_strsm
  {ord:order} {lr:side}
  {ul:uplo} {trA: transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: float
, A: &TRMAT (float, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (float, m, n, ord, ldb), ldb: int ldb
) :<> void
  = "mac#atsctrb_cblas_strsm"
// end of [fun]

fun cblas_dtrsm
  {ord:order} {lr:side}
  {ul:uplo} {trA: transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: double
, A: &TRMAT (double, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (double, m, n, ord, ldb), ldb: int ldb
) :<> void
  = "mac#atsctrb_cblas_dtrsm"
// end of [fun]

fun cblas_ctrsm
  {ord:order} {lr:side}
  {ul:uplo} {trA: transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: &ccmplx
, A: &TRMAT (ccmplx, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (ccmplx, m, n, ord, ldb), ldb: int ldb
) :<> void
  = "mac#atsctrb_cblas_ctrsm"
// end of [fun]

fun cblas_ztrsm
  {ord:order} {lr:side}
  {ul:uplo} {trA: transpose} {dg:diag}
  {m,n:nat} {na:nat} {lda,ldb:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t (ord)
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, transA: CBLAS_TRANSPOSE_t (trA)
, Diag: CBLAS_DIAG_t (dg)
, M: int m, N: int n
, alpha: &zcmplx
, A: &TRMAT (zcmplx, na, ord, ul, dg, lda), lda: int lda
, B: &GEMAT (zcmplx, m, n, ord, ldb), ldb: int ldb
) :<> void
  = "mac#atsctrb_cblas_ztrsm"
// end of [fun]

(* ****** ****** *)

//
// HEMM: C, Z // extended with S, D, 
//

(*

void cblas_chemm(
  const enum CBLAS_ORDER Order
, const enum CBLAS_SIDE Side
, const enum CBLAS_UPLO Uplo
, const int M, const int N
, const void *alpha
, const void *A, const int lda
, const void *B, const int ldb
, const void *beta
, void *C, const int ldc
) ;

*)

fun{a:t@ype}
cblas_hemm
  {ord:order} {lr:side} {ul:uplo}
  {m,n:nat} {na:nat} {lda,ldb,ldc:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t ord
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, M: int m, N: int n
, alpha: a
, A: &HEMAT (a, na, ord, ul, lda), lda: int lda
, B: &GEMAT (a, m, n, ord, ldb), ldb: int ldb
, beta: a
, C: &GEMAT (a, m, n, ord, ldc), ldc: int ldc
) :<> void // end of [cblas_hemm]

fun cblas_chemm
  {ord:order} {lr:side} {ul:uplo}
  {m,n:nat} {na:nat} {lda,ldb,ldc:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t ord
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, M: int m, N: int n
, alpha: &ccmplx
, A: &HEMAT (ccmplx, na, ord, ul, lda), lda: int lda
, B: &GEMAT (ccmplx, m, n, ord, ldb), ldb: int ldb
, beta: &ccmplx
, C: &GEMAT (ccmplx, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_chemm"
// end of [fun]

fun cblas_zhemm
  {ord:order} {lr:side} {ul:uplo}
  {m,n:nat} {na:nat} {lda,ldb,ldc:pos} (
  pfa: sidedim_p (lr, m, n, na)
| Order: CBLAS_ORDER_t ord
, Side: CBLAS_SIDE_t (lr)
, Uplo: CBLAS_UPLO_t (ul)
, M: int m, N: int n
, alpha: &zcmplx
, A: &HEMAT (zcmplx, na, ord, ul, lda), lda: int lda
, B: &GEMAT (zcmplx, m, n, ord, ldb), ldb: int ldb
, beta: &zcmplx
, C: &GEMAT (zcmplx, m, n, ord, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_zhemm"
// end of [fun]

(* ****** ****** *)

//
// HERK: C, Z // extended with S, D
//

(*

void cblas_cherk (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE Trans
, const int N, const int K
, const float alpha
, const void *A, const int lda
, const float beta
, void *C, const int ldc
) ;

*)

fun{a1,a2:t@ype}
cblas_herk
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: a2
, A: &GEMAT (a1, ma, na, ord, lda), lda: int lda
, beta: a2
, C: &HEMAT (a1, n, ord, ul, ldc), ldc: int ldc
) :<> void
// end of [cblas_herk]

fun cblas_cherk
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: float
, A: &GEMAT (ccmplx, ma, na, ord, lda), lda: int lda
, beta: float
, C: &HEMAT (ccmplx, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_cherk"
// end of [fun]

fun cblas_zherk
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: double
, A: &GEMAT (zcmplx, ma, na, ord, lda), lda: int lda
, beta: double
, C: &HEMAT (zcmplx, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_zherk"
// end of [fun]

(* ****** ****** *)

//
// HER2K: C, Z // extended with S, D
//

(*

void cblas_cher2k (
  const enum CBLAS_ORDER Order
, const enum CBLAS_UPLO Uplo
, const enum CBLAS_TRANSPOSE Trans
, const int N, const int K
, const void *alpha
, const void *A, const int lda
, const void *B, const int ldb
, const void *beta
, void *C, const int ldc
) ;

*)

fun{a1,a2:t@ype}
cblas_her2k
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: a1
, A: &GEMAT (a1, ma, na, ord, lda), lda: int lda
, B: &GEMAT (a1, ma, na, ord, ldb), ldb: int ldb
, beta: a2
, C: &HEMAT (a1, n, ord, ul, ldc), ldc: int ldc
) :<> void
// end of [cblas_her2k]

fun cblas_cher2k
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: &ccmplx
, A: &GEMAT (ccmplx, ma, na, ord, lda), lda: int lda
, B: &GEMAT (ccmplx, ma, na, ord, ldb), ldb: int ldb
, beta: float
, C: &HEMAT (ccmplx, n, ord, ul, ldc), ldc: int ldc
) :<> void
 = "mac#atsctrb_cblas_cher2k"
// end of [fun]

fun cblas_zher2k
  {ord:order} {ul:uplo} {trA:transpose}
  {n,k:nat} {ma,na:nat} {lda,ldb,ldc:pos} (
  pfa: trandim_p (trA, ma, na, n, k)
| Order: CBLAS_ORDER_t (ord)
, Uplo: CBLAS_UPLO_t (ul)
, Trans: CBLAS_TRANSPOSE_t (trA)
, N: int n, K: int k
, alpha: &zcmplx
, A: &GEMAT (zcmplx, ma, na, ord, lda), lda: int lda
, B: &GEMAT (zcmplx, ma, na, ord, ldb), ldb: int ldb
, beta: double
, C: &HEMAT (zcmplx, n, ord, ul, ldc), ldc: int ldc
) :<> void
  = "mac#atsctrb_cblas_zher2k"
// end of [fun]

(* ****** ****** *)

(* end of [cblas.sats] *)
