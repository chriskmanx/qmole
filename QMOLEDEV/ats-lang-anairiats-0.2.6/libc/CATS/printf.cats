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

#ifndef ATS_LIBC_PRINTF_CATS
#define ATS_LIBC_PRINTF_CATS

/* ****** ****** */

#include <stdio.h>

/* ****** ****** */

#include <stdarg.h>

/* ****** ****** */

ATSinline()
ats_int_type
atslib_vprintf (
  ats_ptr_type fmt
, ats_ref_type arg
) {
  return vprintf ((char*)fmt, *(va_list*)arg) ;
} /* end of [atslib_vprintf] */

ATSinline()
ats_int_type
atslib_vfprintf (
  ats_ptr_type out
, ats_ptr_type fmt
, ats_ref_type arg
) {
  return vfprintf ((FILE*)out, (char*)fmt, *(va_list*)arg) ;
} /* end of [atslib_vfprintf] */

/* ****** ****** */

#if (0)
//
// HX: [snprintf] is now implemented in $ATSHOME/libc/DATS/printf.dats
//
ATSinline()
ats_int_type
atslib_snprintf (
  ats_ptr_type buf
, ats_size_type sz
, ats_ptr_type fmt
, ...
) {
  int ntot ;
  va_list ap ;
  va_start(ap, fmt) ;
  ntot = vsnprintf (buf, sz, (char*)fmt, ap) ;
  va_end(ap) ;
  return ntot ;
} /* end of [atslib_snprintf] */
#endif // end of [#if(0)]

/* ****** ****** */

ATSinline()
ats_int_type
atslib_vsnprintf (
  ats_ptr_type buf
, ats_size_type sz
, ats_ptr_type fmt
, ats_ref_type arg
) {
  int n ;
  n = vsnprintf (buf, sz, (char*)fmt, *(va_list*)arg) ;
  return n ;
} /* end of [atslib_vsnprintf] */

/* ****** ****** */

#endif /* ATS_LIBC_PRINTF_CATS */

/* end of [printf.cats] */
