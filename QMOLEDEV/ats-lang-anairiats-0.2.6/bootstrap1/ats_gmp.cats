/***********************************************************************/
/*                                                                     */
/*                        Applied Type System                          */
/*                                                                     */
/*                             Hongwei Xi                              */
/*                                                                     */
/***********************************************************************/

/*
** ATS/Anairiats - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*/

/* ****** ****** */

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// August 2008

/* ****** ****** */

/* ats_gmp.cats: in case [GMP] is unavailable */

/* ****** ****** */

#ifndef ATS_SRC_GMP_CATS
#define ATS_SRC_GMP_CATS

/* ****** ****** */

#include <stdio.h>
#include <stdlib.h>

/* ****** ****** */

#include "ats_types.h"

/* ****** ****** */

#include "prelude/CATS/basics.cats"

/* ****** ****** */

typedef ats_lint_type lint ;
typedef ats_llint_type mpz ;
typedef mpz *mpz_ptr ;

typedef mpz ats_mpz_viewt0ype ;

typedef ats_ref_type ats_mpz_ptr_type ;
typedef ats_ref_type ats_mpq_ptr_type ;
typedef ats_ref_type ats_mpf_ptr_type ;

/* ****** ****** */

static inline
lint mpz_get_si (mpz_ptr x) { return *x ; }

/* ****** ****** */

static inline
void mpz_init (mpz_ptr x) { return ; }

static inline
void mpz_init_set_si (mpz_ptr x, lint y) {
  *x = y ; return ;
}

static inline
int mpz_init_set_str (
  mpz_ptr x, char *nptr, int base
) {
  char *endptr ;
  *x = strtoll (nptr, &endptr, base) ;
  if (!endptr) return (-1) ;
  return 0 ;
} // end of [mpz_init_set_str]

/* ****** ****** */

static inline
int mpz_cmp (mpz_ptr x, mpz_ptr y) {
  return (*x - *y) ;
}

static inline
int mpz_cmp_si (mpz_ptr x, lint y) {
  return (*x - y) ;
}

static inline
void mpz_neg (mpz_ptr x, mpz_ptr y) {
  *x = -(*y) ; return ;
}

static inline
void mpz_add (mpz_ptr x, mpz_ptr y, mpz_ptr z) {
  *x = *y + *z ; return ;
}

static inline
void mpz_add_si (mpz_ptr x, mpz_ptr y, lint z) {
  *x = *y + z ; return ;
}

static inline
void mpz_sub (mpz_ptr x, mpz_ptr y, mpz_ptr z) {
  *x = *y - *z ; return ;
}

static inline
void mpz_sub_si (mpz_ptr x, mpz_ptr y, lint z) {
  *x = *y - z ; return ;
}

static inline
void mpz_mul (mpz_ptr x, mpz_ptr y, mpz_ptr z) {
  *x = *y * *z ; return ;
}

static inline
void mpz_mul_si (mpz_ptr x, mpz_ptr y, lint z) {
  *x = *y * z ; return ;
}

/* ****** ****** */

static inline
void mp_set_memory_functions () { return ; }

/* ****** ****** */

static inline
ats_void_type
atslib_mpz_init_set_int (
  ats_mpz_ptr_type x, ats_int_type y
) {
  mpz_init_set_si((mpz_ptr)x, y) ; return ;
} // end of [atslib_mpz_init_set_int]

static inline
ats_void_type
atslib_fprint_mpz (
  ats_ptr_type file, const ats_mpz_ptr_type x
) {
  fprintf ((FILE*)file, "%lld", *(mpz_ptr)x) ; return ;
} // end of [atslib_fprint_mpz]

/* ****** ****** */

#endif /* ATS_SRC_GMP_CATS */

/* end of [ats_gmp.cats] */
