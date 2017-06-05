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
** Copyright (C) 2002-2010 Hongwei Xi.
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

#ifndef ATS_LIBC_RANDOM_CATS
#define ATS_LIBC_RANDOM_CATS

/* ****** ****** */

#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

/* ****** ****** */

#include "ats_types.h"

/* ****** ****** */

typedef struct drand48_data ats_drand48_data_type ;

/* ****** ****** */

ATSinline()
ats_void_type
atslib_srand48
  (ats_lint_type seed) {
  srand48 ((long int)seed) ; return ;
} // end of [atslib_srand48]

ATSinline()
ats_void_type
atslib_srand48_with_time () {
  srand48 ((long int)(time ((time_t*)0))) ; return ;
} // end of [atslib_srand48_with_time]

/* ****** ****** */

ATSinline()
ats_double_type
atslib_drand48 () { return drand48() ; }

ATSinline()
ats_lint_type
atslib_lrand48 () { return lrand48() ; }

ATSinline()
ats_lint_type
atslib_mrand48 () { return mrand48() ; }

/* ****** ****** */

ATSinline()
ats_int_type
atslib_srand48_r
  (ats_lint_type seed, ats_ref_type buf) {
  return srand48_r (seed, (ats_drand48_data_type*)buf) ;
} // end of [atslib_srand48_r]

/* ****** ****** */

ATSinline()
ats_int_type
atslib_drand48_r (ats_ref_type buf, ats_ref_type res) {
  return drand48_r ((ats_drand48_data_type*)buf, (ats_double_type*)res) ;
} // end of [drand48_r]

ATSinline()
ats_int_type
atslib_lrand48_r (ats_ref_type buf, ats_ref_type res) {
  return lrand48_r ((ats_drand48_data_type*)buf, (ats_lint_type*)res) ;
} // end of [lrand48_r]

ATSinline()
ats_int_type
atslib_mrand48_r (ats_ref_type buf, ats_ref_type res) {
  return mrand48_r ((ats_drand48_data_type*)buf, (ats_lint_type*)res) ;
} // end of [lrand48_r]


/* ****** ****** */

#endif /* ATS_LIBC_RANDOM_CATS */

/* end of [random.cats] */
