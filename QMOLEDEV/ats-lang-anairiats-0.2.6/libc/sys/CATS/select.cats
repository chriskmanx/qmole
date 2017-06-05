/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Power of Types!
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

#ifndef ATS_LIBC_SYS_SELECT_CATS
#define ATS_LIBC_SYS_SELECT_CATS

/* ****** ****** */

#include <sys/select.h>
typedef fd_set ats_fd_set_type ;
typedef struct timeval ats_timeval_type ;

/* ****** ****** */

ATSinline()
ats_void_type
atslib_FD_ZERO (
  ats_ref_type fdset
) {
  FD_ZERO ((fd_set*)fdset) ; return ;
} // end of [atslib_FD_ZERO]

ATSinline()
ats_void_type
atslib_FD_SET (
  ats_int_type fd, ats_ref_type fdset
) {
  FD_SET (fd, (fd_set*)fdset) ; return ;
} // end of [atslib_FD_SET]

ATSinline()
ats_void_type
atslib_FD_CLR (
  ats_int_type fd, ats_ref_type fdset
) {
  FD_CLR (fd, (fd_set*)fdset) ; return ;
} // end of [atslib_FD_CLR]

ATSinline()
ats_bool_type
atslib_FD_ISSET (
  ats_int_type fd, ats_ref_type fdset
) {
  return FD_ISSET (fd, (fd_set*)fdset) ;
} // end of [atslib_FD_ISSET]

/* ****** ****** */

#define atslib_select select
#define atslib_pselect pselect

/* ****** ****** */

#endif /* end of [ATS_LIBC_SYS_SELECT_CATS] */

/* end of [select.cats] */
