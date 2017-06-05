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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// July 2007
//
/* ****** ****** */

/*
** ats_counter: a simple counter implementation
*/

/* ****** ****** */

#ifndef ATS_SRC_COUNTER_CATS
#define ATS_SRC_COUNTER_CATS

/* ****** ****** */

#include <stdio.h>

/* ****** ****** */

#include "ats_memory.h"
#include "ats_types.h"

/* ****** ****** */

typedef ats_int32_type atsopt_count_type ;
/*
typedef atsopt_count_type *atsopt_counter_type ;
*/

ATSinline()
ats_ptr_type
atsopt_counter_make () {
  ats_ptr_type cntr ;
  cntr = ATS_MALLOC (sizeof(atsopt_count_type)) ;
  *(atsopt_count_type*)cntr = 0 ; return cntr ;
} // end of [atsopt_counter_make]

ATSinline()
ats_void_type
atsopt_counter_inc
  (ats_ptr_type cntr) {
  *(atsopt_count_type*)cntr += 1 ; return ;
} // end of [atsopt_counter_inc]

ATSinline()
atsopt_count_type
atsopt_counter_get
  (ats_ptr_type cntr) {
  return *(atsopt_count_type*)cntr ;
} // end of [atsopt_counter_get]

ATSinline()
ats_void_type
atsopt_counter_set (
  ats_ptr_type cntr, atsopt_count_type cnt
) {
 *(atsopt_count_type*)cntr = cnt ; return ;
} // end of [atsopt_counter_set]

ATSinline()
ats_void_type
atsopt_counter_reset
  (ats_ptr_type cntr) {
  *(atsopt_count_type*)cntr = 0 ; return ;
} // end of [atsopt_counter_reset]

ATSinline()
atsopt_count_type
atsopt_counter_getinc
  (ats_ptr_type cntr) { 
  atsopt_count_type cnt ;
  cnt = *(atsopt_count_type*)cntr ;
  *(atsopt_count_type*)cntr += 1 ;
  return cnt ;
} // end of [atsopt_counter_getinc]

ATSinline()
atsopt_count_type
atsopt_counter_incget
  (ats_ptr_type cntr) { 
  *((atsopt_count_type*)cntr) += 1 ;
  return *(atsopt_count_type*)cntr ;
} // end of [atsopt_counter_incget]

/* ****** ****** */

ATSinline()
ats_int_type
atsopt_compare_count_count
  (atsopt_count_type cnt1, atsopt_count_type cnt2) {
  if (cnt1 < cnt2) return -1 ;
  if (cnt1 > cnt2) return  1 ;
  return 0 ;
} // end of [atsopt_compare_count_count]

/* ****** ****** */

ATSinline()
ats_uint_type
atsopt_count_hash (
  atsopt_count_type cnt
) {
  /* 2654435761 is the golden ration of 2^32 */
  return (2654435761UL * (ats_uint_type)cnt) ;
} // end of [atsopt_count_hash]

/* ****** ****** */

ATSinline()
ats_void_type
atsopt_fprint_count (
  ats_ptr_type out, atsopt_count_type cnt
) {
  fprintf ((FILE*)out, "%lli", (ats_llint_type)cnt) ; return ;
} // end of [atsopt_fprint_count]

/* ****** ****** */

extern ats_ptr_type atspre_tostringf (ats_ptr_type format, ...) ;

ATSinline()
ats_ptr_type
atsopt_tostring_count
  (atsopt_count_type cnt) {
  return atspre_tostringf ("%lli", (ats_llint_type)cnt) ;
} // end of [atsopt_tostring_count]

ATSinline()
ats_ptr_type
atsopt_tostring_prefix_count (
  ats_ptr_type pre, atsopt_count_type cnt
) {
  return atspre_tostringf ("%s%lli", (char*)pre, (ats_llint_type)cnt) ;
} // end of [atsopt_tostring_prefix_count]

/* ****** ****** */

#endif // ATS_SRC_COUNTER_CATS

/* end of [ats_counter.cats] */
