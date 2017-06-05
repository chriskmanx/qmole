/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi.
**
** ATS is  free software;  you can redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
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

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

//
// for supporting SIMD on vectors of 2 doubles
//

/* ****** ****** */

#ifndef ATS_LIBC_SIMD_V2DF_CATS
#define ATS_LIBC_SIMD_V2DF_CATS

/* ****** ****** */

// for vectors of two doubles
typedef double v2df __attribute__((vector_size(16))) ;
typedef v2df ats_v2df_type ;

/* ****** ****** */

static
ats_v2df_type atslib_v2df_0_0 = { 0.0, 0.0 } ;

static
ats_v2df_type atslib_v2df_1_1 = { 1.0, 1.0 } ;

/* ****** ****** */

static inline
ats_v2df_type
atslib_v2df_make_double_double (
  ats_double_type d0, ats_double_type d1
) {
  v2df dd = { d0, d1 } ; return dd ;
} /* end of [ats_v2df_make_double_double] */

static inline
ats_v2df_type
atslib_v2df_make_int_int (
  ats_int_type i0, ats_int_type i1
) {
  v2df dd = { i0, i1 } ; return dd ;
} /* end of [ats_v2df_make_int_int] */

/* ****** ****** */

static inline
ats_double_type
atslib_v2df_get_fst (ats_v2df_type dd) { return ((double*)&dd)[0] ; }

static inline
ats_double_type
atslib_v2df_get_snd (ats_v2df_type dd) { return ((double*)&dd)[1] ; }

static inline
ats_v2df_type
atslib_add_v2df_v2df (
  ats_v2df_type dd1, ats_v2df_type dd2
) {
  return (dd1 + dd2) ;
} /* end of [atslib_add_v2df_v2df] */

static inline
ats_v2df_type
atslib_sub_v2df_v2df (
  ats_v2df_type dd1, ats_v2df_type dd2
) {
  return (dd1 - dd2) ;
} /* end of [atslib_sub_v2df_v2df] */

static inline
ats_v2df_type
atslib_mul_v2df_v2df (
  ats_v2df_type dd1, ats_v2df_type dd2
) { 
  return (dd1 * dd2) ;
} /* end of [atslib_mul_v2df_v2df] */

static inline
ats_v2df_type
atslib_div_v2df_v2df (
  ats_v2df_type dd1, ats_v2df_type dd2
) { 
  return (dd1 / dd2) ;
} /* end of [atslib_div_v2df_v2df] */

/* ****** ****** */

#endif /* ATS_LIBC_SIMD_V2DF_CATS */

/* end of [SIMD_v2df.cats] */
