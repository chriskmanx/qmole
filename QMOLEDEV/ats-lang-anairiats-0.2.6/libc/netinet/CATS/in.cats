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

#ifndef ATS_LIBC_NETINET_IN_CATS
#define ATS_LIBC_NETINET_IN_CATS

/* ****** ****** */

#include <arpa/inet.h>
#include <netinet/in.h>

/* ****** ****** */

typedef struct sockaddr_in ats_sockaddr_in_type ;
#define atslib_socklen_in (sizeof(ats_sockaddr_in_type))

typedef struct sockaddr_in6 ats_sockaddr_in6_type ;
#define atslib_socklen_in6 (sizeof(ats_sockaddr_in6_type))

/* ****** ****** */

ATSinline()
in_port_t // in_port_nbo_t
atslib_in_port_nbo_of_int
  (ats_int_type n) {
  in_port_t nport = n ; return htons (nport) ;
} // end of [atslib_in_port_nbo_of_int]

/* ****** ****** */

ATSinline()
in_addr_t // in_addr_nbo_t
atslib_in_addr_nbo_of_hbo
  (in_addr_t addr_hbo) { return htonl (addr_hbo) ; }
/* end of [atslib_in_addr_nbo_of_hbo] */

/* ****** ****** */

typedef struct in_addr ats_in_addr_type ;

ATSinline()
in_addr_t // in_addr_nbo_t
atslib_in_addr_struct_get_s_addr
  (ats_in_addr_type inp) { return (inp.s_addr) ; }
/* end of [atslib_in_addr_struct_get_s_addr] */

/* ****** ****** */

#endif /* end of [ATS_LIBC_NETINET_IN_CATS] */

/* end of [in.cats] */
