/*
**
** An interface for ATS to interact with LAPACK
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*/

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

/* ****** ****** */

#ifndef ATS_CONTRIB_F2C_CATS
#define ATS_CONTRIB_F2C_CATS

/* ****** ****** */

#include "libc/CATS/complex.cats"

/* ****** ****** */

/*
** #include "f2c.h"
*/
typedef long int F2Cinteger;
typedef unsigned long int F2Cuinteger;

typedef char *F2Caddress;
typedef short int F2Cshortint;

typedef float F2Creal;
typedef double F2Cdoublereal;

/*
typedef struct { F2Creal r, i; } F2Ccomplex;
*/
typedef ats_fcomplex_type F2Ccomplex;

/*
typedef struct { F2Cdoublereal r, i; } F2Cdoublecomplex;
*/
typedef ats_dcomplex_type F2Cdoublecomplex;

typedef long int F2Clogical;
typedef short int F2Cshortlogical;
typedef char F2Clogical1;
typedef char F2Cinteger1;

/* ****** ****** */

// real

//
// arithmetic operations
//

extern float absf (float x) ;

static inline
F2Creal atsctrb_f2c_abs_real
  (F2Creal x) { return (absf(x)) ; }
// end of [atsctrb_f2c_abs_real]

static inline
F2Creal atsctrb_f2c_neg_real
  (F2Creal x) { return (-x) ; }
// end of [atsctrb_f2c_neg_real]

static inline
F2Creal atsctrb_f2c_add_real_real
  (F2Creal x1, F2Creal x2) { return (x1 + x2) ; }
// end of [atsctrb_f2c_add_real_real]

static inline
F2Creal atsctrb_f2c_sub_real_real
  (F2Creal x1, F2Creal x2) { return (x1 - x2) ; }
// end of [atsctrb_f2c_sub_real_real]

static inline
F2Creal atsctrb_f2c_mul_real_real
  (F2Creal x1, F2Creal x2) { return (x1 * x2) ; }
// end of [atsctrb_f2c_mul_real_real]

static inline
F2Creal atsctrb_f2c_div_real_real
  (F2Creal x1, F2Creal x2) { return (x1 / x2) ; }
// end of [atsctrb_f2c_div_real_real]

//
// comparison operations
//

static inline
ats_bool_type
atsctrb_f2c_lt_real_real
  (F2Creal x1, F2Creal x2) {
  return (x1 < x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_lt_real_real]

static inline
ats_bool_type
atsctrb_f2c_lte_real_real
  (F2Creal x1, F2Creal x2) {
  return (x1 <= x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_lte_real_real]

static inline
ats_bool_type
atsctrb_f2c_gt_real_real
  (F2Creal x1, F2Creal x2) {
  return (x1 > x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_gt_real_real]

static inline
ats_bool_type
atsctrb_f2c_gte_real_real
  (F2Creal x1, F2Creal x2) {
  return (x1 >= x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_gte_real_real]

static inline
ats_bool_type
atsctrb_f2c_eq_real_real
  (F2Creal x1, F2Creal x2) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_eq_real_real]

static inline
ats_bool_type
atsctrb_f2c_neq_real_real
  (F2Creal x1, F2Creal x2) {
  return (x1 != x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_neq_real_real]

/* ****** ****** */

// doublereal

//
// arithmetic operations
//

static inline
F2Cdoublereal atsctrb_f2c_abs_doublereal
  (F2Cdoublereal x) { return (absf(x)) ; }
// end of [atsctrb_f2c_abs_doublereal]

static inline
F2Cdoublereal atsctrb_f2c_neg_doublereal
  (F2Cdoublereal x) { return (-x) ; }
// end of [atsctrb_f2c_neg_doublereal]

static inline
F2Cdoublereal atsctrb_f2c_add_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) { return (x1 + x2) ; }
// end of [atsctrb_f2c_add_doublereal_doublereal]

static inline
F2Cdoublereal atsctrb_f2c_sub_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) { return (x1 - x2) ; }
// end of [atsctrb_f2c_sub_doublereal_doublereal]

static inline
F2Cdoublereal atsctrb_f2c_mul_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) { return (x1 * x2) ; }
// end of [atsctrb_f2c_mul_doublereal_doublereal]

static inline
F2Cdoublereal atsctrb_f2c_div_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) { return (x1 / x2) ; }
// end of [atsctrb_f2c_div_doublereal_doublereal]

//
// comparison operations
//

static inline
ats_bool_type
atsctrb_f2c_lt_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) {
  return (x1 < x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_lt_doublereal_doublereal]

static inline
ats_bool_type
atsctrb_f2c_lte_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) {
  return (x1 <= x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_lte_doublereal_doublereal]

static inline
ats_bool_type
atsctrb_f2c_gt_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) {
  return (x1 > x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_gt_doublereal_doublereal]

static inline
ats_bool_type
atsctrb_f2c_gte_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) {
  return (x1 >= x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_gte_doublereal_doublereal]

static inline
ats_bool_type
atsctrb_f2c_eq_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_eq_doublereal_doublereal]

static inline
ats_bool_type
atsctrb_f2c_neq_doublereal_doublereal
  (F2Cdoublereal x1, F2Cdoublereal x2) {
  return (x1 != x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_f2c_neq_doublereal_doublereal]

/* ****** ****** */

#if (0)
static inline
F2Ccomplex
atsctrb_f2c_complex_of_ccmplx
  (ats_fcomplex_type x) { F2Ccomplex y ;
  y.r = crealf(x) ; y.i = cimagf(x) ; return y ;
} /* end of [atsctrb_complex_of_ccmplx] */

static inline
ats_fcomplex_type
atsctrb_f2c_ccmplx_of_complex
  (F2Ccomplex x) { return (x.r + x.i * I) ; }
/* end of [atsctrb_ccmplx_of_complex] */
#endif // [#if (0)]

#if (1)
static inline
F2Ccomplex
atsctrb_f2c_complex_of_ccmplx
  (ats_fcomplex_type x) { return *(F2Ccomplex*)&x ; }
/* end of [atsctrb_complex_of_ccmplx] */

static inline
ats_fcomplex_type
atsctrb_f2c_ccmplx_of_complex
  (F2Ccomplex x) { return *(ats_fcomplex_type*)&x ; }
/* end of [atsctrb_ccmplx_of_complex] */
#endif // [#if (1)]

/* ****** ****** */

// complex

//
// arithmetic operations
//

static inline
F2Ccomplex
atsctrb_f2c_add_complex_complex (F2Ccomplex x1, F2Ccomplex x2) {
  return atsctrb_f2c_complex_of_ccmplx (
    atsctrb_f2c_ccmplx_of_complex (x1) + atsctrb_f2c_ccmplx_of_complex (x2)
  ) ;
} /* end of [atsctrb_f2c_add_complex_complex] */

static inline
F2Ccomplex
atsctrb_f2c_sub_complex_complex (F2Ccomplex x1, F2Ccomplex x2) {
  return atsctrb_f2c_complex_of_ccmplx (
    atsctrb_f2c_ccmplx_of_complex (x1) - atsctrb_f2c_ccmplx_of_complex (x2)
  ) ;
} /* end of [atsctrb_f2c_sub_complex_complex] */

static inline
F2Ccomplex
atsctrb_f2c_mul_complex_complex (F2Ccomplex x1, F2Ccomplex x2) {
  return atsctrb_f2c_complex_of_ccmplx (
    atsctrb_f2c_ccmplx_of_complex (x1) * atsctrb_f2c_ccmplx_of_complex (x2)
  ) ;
} /* end of [atsctrb_f2c_mul_complex_complex] */

static inline
F2Ccomplex
atsctrb_f2c_div_complex_complex (F2Ccomplex x1, F2Ccomplex x2) {
  return atsctrb_f2c_complex_of_ccmplx (
    atsctrb_f2c_ccmplx_of_complex (x1) / atsctrb_f2c_ccmplx_of_complex (x2)
  ) ;
} /* end of [atsctrb_f2c_div_complex_complex] */

static inline
ats_bool_type
atsctrb_f2c_eq_complex_complex
  (F2Ccomplex x1, F2Ccomplex x2) {
  return (
    atsctrb_f2c_ccmplx_of_complex (x1) ==
    atsctrb_f2c_ccmplx_of_complex (x2)
  ? ats_true_bool
  : ats_false_bool
  ) ;
} /* end of [atsctrb_f2c_eq_complex_complex] */

static inline
ats_bool_type
atsctrb_f2c_neq_complex_complex
  (F2Ccomplex x1, F2Ccomplex x2) {
  return (
    atsctrb_f2c_ccmplx_of_complex (x1) !=
    atsctrb_f2c_ccmplx_of_complex (x2)
  ? ats_true_bool
  : ats_false_bool
  ) ;
} /* end of [atsctrb_f2c_neq_complex_complex] */

/* ****** ****** */

#if (0)
static inline
F2Cdoublecomplex
atsctrb_f2c_doublecomplex_of_zcmplx
  (ats_dcomplex_type x) { F2Cdoublecomplex y ;
  y.r = creal(x) ; y.i = cimag(x) ; return y ;
} /* end of [atsctrb_f2c_doublecomplex_of_zcmplx] */

static inline
ats_dcomplex_type
atsctrb_f2c_zcmplx_of_doublecomplex
  (F2Cdoublecomplex x) { return (x.r + x.i * I) ; }
/* end of [atsctrb_zcmplx_of_doublecomplex] */
#endif // [#if (0)]

#if (1)
static inline
F2Cdoublecomplex
atsctrb_f2c_doublecomplex_of_zcmplx
  (ats_dcomplex_type x) { return *(F2Cdoublecomplex*)&x ; }
/* end of [atsctrb_doublecomplex_of_zcmplx] */

static inline
ats_dcomplex_type
atsctrb_f2c_zcmplx_of_doublecomplex
  (F2Cdoublecomplex x) { return *(ats_dcomplex_type*)&x ; }
/* end of [atsctrb_zcmplx_of_doublecomplex] */
#endif // [#if (1)]

/* ****** ****** */

// doublecomplex

//
// arithmetic operations
//

static inline
F2Cdoublecomplex
atsctrb_f2c_add_doublecomplex_doublecomplex
  (F2Cdoublecomplex x1, F2Cdoublecomplex x2) {
  return atsctrb_f2c_doublecomplex_of_zcmplx (
    atsctrb_f2c_zcmplx_of_doublecomplex (x1)
  + atsctrb_f2c_zcmplx_of_doublecomplex (x2)
  ) ;
} /* end of [atsctrb_f2c_add_doublecomplex_doublecomplex] */

static inline
F2Cdoublecomplex
atsctrb_f2c_sub_doublecomplex_doublecomplex
  (F2Cdoublecomplex x1, F2Cdoublecomplex x2) {
  return atsctrb_f2c_doublecomplex_of_zcmplx (
    atsctrb_f2c_zcmplx_of_doublecomplex (x1)
  - atsctrb_f2c_zcmplx_of_doublecomplex (x2)
  ) ;
} /* end of [atsctrb_f2c_sub_doublecomplex_doublecomplex] */

static inline
F2Cdoublecomplex
atsctrb_f2c_mul_doublecomplex_doublecomplex
  (F2Cdoublecomplex x1, F2Cdoublecomplex x2) {
  return atsctrb_f2c_doublecomplex_of_zcmplx (
    atsctrb_f2c_zcmplx_of_doublecomplex (x1)
  * atsctrb_f2c_zcmplx_of_doublecomplex (x2)
  ) ;
} /* end of [atsctrb_f2c_mul_doublecomplex_doublecomplex] */

static inline
F2Cdoublecomplex
atsctrb_f2c_div_doublecomplex_doublecomplex
  (F2Cdoublecomplex x1, F2Cdoublecomplex x2) {
  return atsctrb_f2c_doublecomplex_of_zcmplx (
    atsctrb_f2c_zcmplx_of_doublecomplex (x1)
  / atsctrb_f2c_zcmplx_of_doublecomplex (x2)
  ) ;
} /* end of [atsctrb_f2c_div_doublecomplex_doublecomplex] */

static inline
ats_bool_type
atsctrb_f2c_eq_doublecomplex_doublecomplex
  (F2Cdoublecomplex x1, F2Cdoublecomplex x2) {
  return (
    atsctrb_f2c_ccmplx_of_doublecomplex (x1) ==
    atsctrb_f2c_ccmplx_of_doublecomplex (x2)
  ? ats_true_bool
  : ats_false_bool
  ) ;
} /* end of [atsctrb_f2c_eq_doublecomplex_doublecomplex] */

static inline
ats_bool_type
atsctrb_f2c_neq_doublecomplex_doublecomplex
  (F2Cdoublecomplex x1, F2Cdoublecomplex x2) {
  return (
    atsctrb_f2c_ccmplx_of_doublecomplex (x1) !=
    atsctrb_f2c_ccmplx_of_doublecomplex (x2)
  ? ats_true_bool
  : ats_false_bool
  ) ;
} /* end of [atsctrb_f2c_neq_doublecomplex_doublecomplex] */

/* ****** ****** */

static inline
F2Cinteger
atsctrb_f2c_add_integer_integer
  (F2Cinteger i, F2Cinteger j) { return (i + j); }
// end of [atsctrb_f2c_add_integer_integer]

static inline
F2Cinteger
atsctrb_f2c_add_int1_integer
  (ats_int_type i, F2Cinteger j) { return (i + j); }
// end of [atsctrb_f2c_add_int1_integer]

static inline
F2Cinteger
atsctrb_f2c_add_integer_int1
  (F2Cinteger i, ats_int_type j) { return (i + j); }
// end of [atsctrb_f2c_add_integer_int1]

static inline
F2Cinteger
atsctrb_f2c_sub_integer_integer
  (F2Cinteger i, F2Cinteger j) { return (i - j); }
// end of [atsctrb_f2c_sub_integer_integer]

static inline
F2Cinteger
atsctrb_f2c_mul_integer_integer
  (F2Cinteger i, F2Cinteger j) { return (i * j); }
// end of [atsctrb_f2c_mul_integer_integer]

/* ****** ****** */

static inline
ats_bool_type
atsctrb_f2c_lt_integer_integer
  (F2Cinteger i, F2Cinteger j) {
  return (i < j ? ats_true_bool : ats_false_bool);
} // end of [atsctrb_f2c_lt_integer_integer]

static inline
ats_bool_type
atsctrb_f2c_lte_integer_integer
  (F2Cinteger i, F2Cinteger j) {
  return (i <= j ? ats_true_bool : ats_false_bool);
} // end of [atsctrb_f2c_lte_integer_integer]

static inline
ats_bool_type
atsctrb_f2c_gt_integer_integer
  (F2Cinteger i, F2Cinteger j) {
  return (i > j ? ats_true_bool : ats_false_bool);
} // end of [atsctrb_f2c_gt_integer_integer]

static inline
ats_bool_type
atsctrb_f2c_gte_integer_integer
  (F2Cinteger i, F2Cinteger j) {
  return (i >= j ? ats_true_bool : ats_false_bool);
} // end of [atsctrb_f2c_gte_integer_integer]

static inline
ats_bool_type
atsctrb_f2c_eq_integer_integer
  (F2Cinteger i, F2Cinteger j) {
  return (i == j ? ats_true_bool : ats_false_bool);
} // end of [atsctrb_f2c_eq_integer_integer]

static inline
ats_bool_type
atsctrb_f2c_neq_integer_integer
  (F2Cinteger i, F2Cinteger j) {
  return (i != j ? ats_true_bool : ats_false_bool);
} // end of [atsctrb_f2c_neq_integer_integer]

/* ****** ****** */

static inline
F2Cinteger
atsctrb_f2c_max_integer_integer
  (F2Cinteger i, F2Cinteger j) { return (i >= j ? i : j); }
// end of [atsctrb_f2c_max_integer_integer]

static inline
F2Cinteger
atsctrb_f2c_min_integer_integer
  (F2Cinteger i, F2Cinteger j) { return (i <= j ? i : j); }
// end of [atsctrb_f2c_min_integer_integer]

/* ****** ****** */

#endif /* end of [ATS_CONTRIB_F2C_CATS] */

/* end of [f2c.cats] */
