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

#ifndef ATS_LIBC_SYS_POLL_CATS
#define ATS_LIBC_SYS_POLL_CATS

/* ****** ****** */

#include <sys/poll.h>
typedef struct pollfd ats_pollfd_type ;

/* ****** ****** */

ATSinline()
ats_int_type
atslib_poll (
  ats_ref_type fds
, ats_int_type nfds // cast to nfds_t (unsigned long)
, ats_int_type timeout
) {
  return poll((ats_pollfd_type*)fds, nfds, timeout) ;
} // end of [atslib_poll]

/* ****** ****** */

#define atslib_poll poll
#define atslib_ppoll ppoll

/* ****** ****** */

#endif /* end of [ATS_LIBC_SYS_POLL_CATS] */

/* end of [poll.cats] */
