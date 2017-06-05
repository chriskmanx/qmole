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

#ifndef ATS_LIBC_SYS_SOCKET_CATS
#define ATS_LIBC_SYS_SOCKET_CATS

/* ****** ****** */

#include <errno.h>
#include <stdio.h> // for [perror]

/* ****** ****** */

#include <sys/socket.h>

/* ****** ****** */

#include "libc/CATS/fcntl.cats"

/* ****** ****** */

typedef int ats_socket_type ;

/* ****** ****** */

ATSinline()
ats_int_type
atslib_socket_family_type_err (
  sa_family_t af, ats_socket_type st
) {
  return socket(af, st, 0) ;
} // end of [atslib_socket_family_type_err]

/* ****** ****** */

#define atslib_connect_err connect

/* ****** ****** */

#define atslib_bind_err bind

/* ****** ****** */

#define atslib_listen_err listen

/* ****** ****** */

ATSinline()
ats_int_type
atslib_accept_null_err
  (ats_int_type sfd) {
  return accept(sfd, (struct sockaddr*)0, (socklen_t*)0) ;
} // end of [atslib_accept_null_err]

#define atslib_accept_err accept

/* ****** ****** */

#define atslib_socket_close_err atslib_close_err

/* ****** ****** */

#define atslib_shutdown_err shutdown

/* ****** ****** */

#define atslib_socket_read_err atslib_fildes_read_err
#define atslib_socket_write_err atslib_fildes_write_err
#define atslib_socket_read_all_err atslib_fildes_read_all_err
#define atslib_socket_write_all_err atslib_fildes_write_all_err

/* ****** ****** */
//
// HX: [fdopen] is declared in stdio.h
//
#define atslib_socket_fdopen_err fdopen

/* ****** ****** */

ATSinline()
ats_int_type
atslib_setsockopt (
  ats_int_type fd
, ats_int_type level
, ats_int_type option
, ats_ref_type value
, ats_size_type valen
) {
  return setsockopt(fd, level, option, (void*)value, (socklen_t)valen) ;
} // end of [setsockopt]

/* ****** ****** */

#define atslib_sockatmark sockatmark

/* ****** ****** */

#endif /* ATS_LIBC_SYS_SOCKET_CATS */

/* end of [socket.cats] */
