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

#ifndef ATS_LIBC_SYS_SOCKET_UN_CATS
#define ATS_LIBC_SYS_SOCKET_UN_CATS

/* ****** ****** */

#include <sys/un.h>
#include <sys/socket.h>

/* ****** ****** */

#ifndef strcpy
extern char *strcpy (char *dst, const char *src) ; // in [string.h]
#endif // end of [strcpy]

/* ****** ****** */

ATSinline()
ats_void_type
atslib_sockaddr_un_init (
  ats_ptr_type sa0
, sa_family_t af, ats_ptr_type name
) {
  struct sockaddr_un *sa = sa0 ;
  sa->sun_family = af ;
  (void)strcpy(&(sa->sun_path[0]), name) ;
  return ;
} // end of [sockaddr_un_init]

/* ****** ****** */

#endif /* ATS_LIBC_SYS_SOCKET_UN_CATS */

/* end of [socket_un.cats] */
