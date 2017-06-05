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

#ifndef ATS_LIBC_NETDB_CATS
#define ATS_LIBC_NETDB_CATS

/* ****** ****** */

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

/* ****** ****** */

typedef
struct addrinfo ats_addrinfo_type ;
typedef struct hostent ats_hostent_type ;

/* ****** ****** */
//
#define atslib_addrinfoptr_get_addr(x) (((ats_addrinfo_type*)x)->ai_addr)
#define atslib_addrinfoptr_get_addrlen(x) (((ats_addrinfo_type*)x)->ai_addrlen)
//
#define atslib_addrinfoptr_get_cannonname(x) (((ats_addrinfo_type*)x)->ai_cannonname)
//
#define atslib_addrinfoptr_get_family(x) (((ats_addrinfo_type*)x)->ai_family)
#define atslib_addrinfoptr_get_protocol(x) (((ats_addrinfo_type*)x)->ai_protocol)
#define atslib_addrinfoptr_get_socktype(x) (((ats_addrinfo_type*)x)->ai_socktype)
//
#define atslib_addrinfoptr_get_next(x) (((ats_addrinfo_type*)x)->ai_next)
//
/* ****** ****** */

#define atslib_getaddrinfo(node, port, hint, res) \
   getaddrinfo((char*)node, (char*)port, (ats_addrinfo_type*)hint, (ats_addrinfo_type**)res)
#define atslib_gai_strerror gai_strerror
#define atslib_freeaddrinfo freeaddrinfo

/* ****** ****** */

#define atslib_hostent_get_name(h) \
  (((ats_hostent_type*)h)->h_name)
#define atslib_hostent_get_aliases(h) \
  (((ats_hostent_type*)h)->h_aliases)
#define atslib_hostent_get_addr_list(h) \
  (((ats_hostent_type*)h)->h_addr_list)

#define atslib_sethostent sethostent
#define atslib_gethostent gethostent
#define atslib_endhostent endhostent

/* ****** ****** */

#define atslib_gethostbyname gethostbyname
#define atslib_gethostbyaddr gethostbyaddr

/* ****** ****** */

#define atslib_getnameinfo getnameinfo

/* ****** ****** */

#define atslib_gethostid gethostid
#define atslib_sethostid sethostid

/* ****** ****** */

#endif /* ATS_LIBC_NETDB_CATS */

/* end of [netdb.cats] */
