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

#ifndef ATS_LIBC_SYS_SOCKET_IN_CATS
#define ATS_LIBC_SYS_SOCKET_IN_CATS

/* ****** ****** */

#include <netinet/in.h>
#include <sys/socket.h>

/* ****** ****** */

#ifndef memset
extern void *memset (void *p, int c, size_t n) ;
#endif // end of [#ifndef]

/* ****** ****** */

ATSinline()
ats_void_type
atslib_sockaddr_in_init (
  ats_ptr_type sa0
, sa_family_t af
, in_addr_t inp
, in_port_t port
) {
  struct sockaddr_in *sa = sa0 ;
  (void)memset(sa, 0, sizeof (struct sockaddr_in)) ;
  sa->sin_family = af ;
  sa->sin_addr.s_addr = inp ;
  sa->sin_port = port ;
} // end of [sockaddr_in_init]

/* ****** ****** */

#endif /* ATS_LIBC_SYS_SOCKET_IN_CATS */

/* end of [socket_in.cats] */
