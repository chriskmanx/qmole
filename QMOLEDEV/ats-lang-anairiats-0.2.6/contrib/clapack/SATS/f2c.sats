(*
**
** A f2c (fortran-to-c) interface for ATS
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
#include "contrib/clapack/CATS/f2c.cats"
%} // end of [%{#}

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "libc/SATS/complex.sats"

(* ****** ****** *)

fun{t:t@ype} print_typ (): void
fun{t:t@ype} print_elt (x: t): void

(* ****** ****** *)

fun{t:t@ype} of_int (x: int):<> t
fun{t:t@ype} of_double (x: double):<> t

(* ****** ****** *)

fun{t1,t2:t@ype} abs (x: t1):<> t2

fun{t:t@ype} neg (x: t):<> t

fun{t:t@ype} add (x1: t, x2: t):<> t
fun{t:t@ype} sub (x1: t, x2: t):<> t
fun{t:t@ype} mul (x1: t, x2: t):<> t
fun{t:t@ype} div (x1: t, x2: t):<> t

fun{t:t@ype} lt (x1: t, x2: t):<> bool
fun{t:t@ype} lte (x1: t, x2: t):<> bool

fun{t:t@ype} gt (x1: t, x2: t):<> bool
fun{t:t@ype} gte (x1: t, x2: t):<> bool

fun{t:t@ype} eq (x1: t, x2: t):<> bool
fun{t:t@ype} neq (x1: t, x2: t):<> bool

(* ****** ****** *)
//
// F2Creal = float // f2c.cats
//
abst@ype real = $extype"ats_float_type"

castfn real_of_float (x: float):<> real
castfn float_of_real (x: real):<> float

fun abs_real (x: real):<> real

fun neg_real (x: real):<> real
overload ~ with neg_real

fun add_real_real (x1: real, x2: real):<> real
  = "atsctrb_f2c_add_real_real"
overload + with add_real_real

fun sub_real_real (x1: real, x2: real):<> real
  = "atsctrb_f2c_sub_real_real"
overload - with sub_real_real

fun mul_real_real (x1: real, x2: real):<> real
  = "atsctrb_f2c_mul_real_real"
overload * with mul_real_real

fun div_real_real (x1: real, x2: real):<> real
  = "atsctrb_f2c_div_real_real"
overload / with div_real_real

fun lt_real_real (x1: real, x2: real):<> bool
  = "atsctrb_f2c_lt_real_real"
overload < with lt_real_real

fun lte_real_real (x1: real, x2: real):<> bool
  = "atsctrb_f2c_lte_real_real"
overload <= with lte_real_real

fun gt_real_real (x1: real, x2: real):<> bool
  = "atsctrb_f2c_gt_real_real"
overload > with gt_real_real

fun gte_real_real (x1: real, x2: real):<> bool
  = "atsctrb_f2c_gte_real_real"
overload >= with gte_real_real

fun eq_real_real (x1: real, x2: real):<> bool
  = "atsctrb_f2c_eq_real_real"
overload = with eq_real_real

fun neq_real_real (x1: real, x2: real):<> bool
  = "atsctrb_f2c_neq_real_real"
overload <> with neq_real_real

(* ****** ****** *)

// F2Cdoublereal = double // f2c.cats
abst@ype doublereal = $extype"ats_double_type"
castfn doublereal_of_double (x: double):<> doublereal
castfn double_of_doublereal (x: doublereal):<> double

fun abs_doublereal (x: doublereal):<> doublereal

fun neg_doublereal (x: doublereal):<> doublereal
overload ~ with neg_doublereal

fun add_doublereal_doublereal (x1: doublereal, x2: doublereal):<> doublereal
  = "atsctrb_f2c_add_doublereal_doublereal"
overload + with add_doublereal_doublereal

fun sub_doublereal_doublereal (x1: doublereal, x2: doublereal):<> doublereal
  = "atsctrb_f2c_sub_doublereal_doublereal"
overload - with sub_doublereal_doublereal

fun mul_doublereal_doublereal (x1: doublereal, x2: doublereal):<> doublereal
  = "atsctrb_f2c_mul_doublereal_doublereal"
overload * with mul_doublereal_doublereal

fun div_doublereal_doublereal (x1: doublereal, x2: doublereal):<> doublereal
  = "atsctrb_f2c_div_doublereal_doublereal"
overload / with div_doublereal_doublereal

fun lt_doublereal_doublereal (x1: doublereal, x2: doublereal):<> bool
  = "atsctrb_f2c_lt_doublereal_doublereal"
overload < with lt_doublereal_doublereal

fun lte_doublereal_doublereal (x1: doublereal, x2: doublereal):<> bool
  = "atsctrb_f2c_lte_doublereal_doublereal"
overload <= with lte_doublereal_doublereal

fun gt_doublereal_doublereal (x1: doublereal, x2: doublereal):<> bool
  = "atsctrb_f2c_gt_doublereal_doublereal"
overload > with gt_doublereal_doublereal

fun gte_doublereal_doublereal (x1: doublereal, x2: doublereal):<> bool
  = "atsctrb_f2c_gte_doublereal_doublereal"
overload >= with gte_doublereal_doublereal

fun eq_doublereal_doublereal (x1: doublereal, x2: doublereal):<> bool
  = "atsctrb_f2c_eq_doublereal_doublereal"
overload = with eq_doublereal_doublereal

fun neq_doublereal_doublereal (x1: doublereal, x2: doublereal):<> bool
  = "atsctrb_f2c_neq_doublereal_doublereal"
overload <> with neq_doublereal_doublereal

(* ****** ****** *)
//
// F2Ccomplex = ccmplx // f2c.cats
//
abst@ype complex = $extype"ats_fcomplex_type"

castfn complex_of_ccmplx (x: ccmplx):<> complex
  = "atsctrb_f2c_complex_of_ccmplx"
castfn ccmplx_of_complex (x: complex):<> ccmplx
  = "atsctrb_f2c_ccmplx_of_complex"

fun abs_complex (x: complex):<> real

fun neg_complex (x: complex):<> complex
overload ~ with neg_complex

fun add_complex_complex
  (x1: complex, x2: complex):<> complex
  = "atsctrb_f2c_add_complex_complex"
overload + with add_complex_complex

fun sub_complex_complex
  (x1: complex, x2: complex):<> complex
  = "atsctrb_f2c_sub_complex_complex"
overload - with sub_complex_complex

fun mul_complex_complex
  (x1: complex, x2: complex):<> complex
  = "atsctrb_f2c_mul_complex_complex"
overload * with mul_complex_complex

fun div_complex_complex
  (x1: complex, x2: complex):<> complex
  = "atsctrb_f2c_div_complex_complex"
overload / with div_complex_complex

fun eq_complex_complex (x1: complex, x2: complex):<> bool
  = "atsctrb_f2c_eq_complex_complex"
overload = with eq_complex_complex

fun neq_complex_complex (x1: complex, x2: complex):<> bool
  = "atsctrb_f2c_neq_complex_complex"
overload <> with neq_complex_complex

(* ****** ****** *)
//
// F2Cdoublecomplex = zcmplx // f2c.cats
//
abst@ype doublecomplex = $extype"ats_dcomplex_type"

castfn doublecomplex_of_zcmplx (x: zcmplx):<> doublecomplex
  = "atsctrb_f2c_doublecomplex_of_zcmplx"
castfn zcmplx_of_doublecomplex (x: doublecomplex):<> zcmplx
  = "atsctrb_f2c_zcmplx_of_doublecomplex"

fun abs_doublecomplex (x: doublecomplex):<> doublereal

fun neg_doublecomplex (x: doublecomplex):<> doublecomplex
overload ~ with neg_doublecomplex

fun add_doublecomplex_doublecomplex
  (x1: doublecomplex, x2: doublecomplex):<> doublecomplex
  = "atsctrb_f2c_add_doublecomplex_doublecomplex"
overload + with add_doublecomplex_doublecomplex

fun sub_doublecomplex_doublecomplex
  (x1: doublecomplex, x2: doublecomplex):<> doublecomplex
  = "atsctrb_f2c_sub_doublecomplex_doublecomplex"
overload - with sub_doublecomplex_doublecomplex

fun mul_doublecomplex_doublecomplex
  (x1: doublecomplex, x2: doublecomplex):<> doublecomplex
  = "atsctrb_f2c_mul_doublecomplex_doublecomplex"
overload * with mul_doublecomplex_doublecomplex

fun div_doublecomplex_doublecomplex
  (x1: doublecomplex, x2: doublecomplex):<> doublecomplex
  = "atsctrb_f2c_div_doublecomplex_doublecomplex"
overload / with div_doublecomplex_doublecomplex

fun eq_doublecomplex_doublecomplex
  (x1: doublecomplex, x2: doublecomplex):<> bool
  = "atsctrb_f2c_eq_doublecomplex_doublecomplex"
overload = with eq_doublecomplex_doublecomplex

fun neq_doublecomplex_doublecomplex
  (x1: doublecomplex, x2: doublecomplex):<> bool
  = "atsctrb_f2c_neq_doublecomplex_doublecomplex"
overload <> with neq_doublecomplex_doublecomplex

(* ****** ****** *)

abst@ype integer (i: int) =
  $extype"F2Cinteger" // defined to be long int
typedef integer = [i:int] integer (i)
typedef integerGt (n:int) = [i:int | i > n] integer (i)
typedef integerGte (n:int) = [i:int | i >= n] integer (i)
typedef integerBtw (l:int, u:int) = [i:int | l <= i; i < u] integer (i)
typedef integerBtwe (l:int, u:int) = [i:int | l <= i; i <= u] integer (i)

castfn integer_of_lint (x: lint):<> integer
castfn lint_of_integer (x: integer):<> lint

castfn size1_of_integer {i:nat} (i: integer i):<> size_t (i)
castfn integer_of_size1 {i:nat} (i: size_t i):<> integer (i)

castfn ssize1_of_integer {i:int} (i: integer i):<> ssize_t (i)
castfn integer_of_ssize1 {i:int} (i: ssize_t i):<> integer (i)

fun integer_of_int1
  {i:int} (i: int i):<> integer (i) = "atspre_lint_of_int"
// end of [integer_of_int1]

(* ****** ****** *)

fun add_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> integer (i+j)
  = "atsctrb_f2c_add_integer_integer"
overload + with add_integer_integer

fun add_int1_integer {i,j:int}
  (i: int i, j: integer j):<> integer (i+j)
  = "atsctrb_f2c_add_int1_integer"
overload + with add_int1_integer

fun add_integer_int1 {i,j:int}
  (i: integer i, j: int j):<> integer (i+j)
  = "atsctrb_f2c_add_integer_int1"
overload + with add_integer_int1

fun sub_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> integer (i-j)
  = "atsctrb_f2c_sub_integer_integer"
overload - with sub_integer_integer

fun mul_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> [ij:int] integer ij
  = "atsctrb_f2c_mul_integer_integer"
overload * with mul_integer_integer

(* ****** ****** *)

fun lt_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> bool (i < j)
  = "atsctrb_f2c_lt_integer_integer"
overload < with lt_integer_integer

fun lte_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> bool (i <= j)
  = "atsctrb_f2c_lte_integer_integer"
overload <= with lte_integer_integer

fun gt_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> bool (i < j)
  = "atsctrb_f2c_gt_integer_integer"
overload > with gt_integer_integer

fun gte_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> bool (i <= j)
  = "atsctrb_f2c_gte_integer_integer"
overload >= with gte_integer_integer

fun eq_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> bool (i == j)
  = "atsctrb_f2c_eq_integer_integer"
overload = with eq_integer_integer

fun neq_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> bool (i <> j)
  = "atsctrb_f2c_eq_integer_integer"
overload <> with neq_integer_integer

(* ****** ****** *)

fun max_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> integer (max(i,j))
  = "atsctrb_f2c_max_integer_integer"
overload max with max_integer_integer

fun min_integer_integer {i,j:int}
  (i: integer i, j: integer j):<> integer (min(i,j))
  = "atsctrb_f2c_min_integer_integer"
overload min with min_integer_integer

(* ****** ****** *)

fun fprint_integer (out: FILEref, i: integer): void
fun print_integer (i: integer): void
overload print with print_integer
fun prerr_integer (i: integer): void
overload prerr with prerr_integer

(* ****** ****** *)

fun{t:t@ype} to_integer (x: t):<> integer

(* ****** ****** *)

abst@ype uinteger (i:int) =
  $extype"F2Cuinteger" // defined to be unsigned long int
typedef uinteger = [i:nat] uinteger (i)

castfn uinteger_of_ulint (x: ulint):<> uinteger
castfn ulint_of_uinteger (x: uinteger):<> ulint

castfn size1_of_uinteger {i:nat} (i: uinteger i):<> size_t (i)
castfn uinteger_of_size1 {i:nat} (i: size_t i):<> uinteger (i)

fun uinteger_of_int1
  {i:nat} (i: int i):<> integer (i) = "atspre_ulint_of_int"
// end of [uinteger_of_int1]
fun uinteger_of_uint1
  {i:nat} (u: uint i):<> uinteger (i) = "atspre_ulint_of_uint"
// end of [uinteger_of_uint1]

(* ****** ****** *)

fun fprint_uinteger (out: FILEref, u: uinteger): void
fun print_uinteger (u: uinteger): void
overload print with print_uinteger
fun prerr_uinteger (u: uinteger): void
overload prerr with prerr_uinteger

(* ****** ****** *)

(* end of [f2c.sats] *)
