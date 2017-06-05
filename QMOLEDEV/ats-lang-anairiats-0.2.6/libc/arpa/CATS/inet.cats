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

#ifndef ATS_LIBC_ARPA_INET_CATS
#define ATS_LIBC_ARPA_INET_CATS

/* ****** ****** */

#include <arpa/inet.h>
#include <netinet/in.h>

/* ****** ****** */

#ifndef EXIT_SUCCESS
#define	EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

/* ****** ****** */

extern void perror (const char *msg) ; // in [stdio.h]

/* ****** ****** */

#include "libc/netinet/CATS/in.cats"

/* ****** ****** */
//
// HX: implemented in [prelude/DATS/basics.dats]
//
extern
ats_void_type ats_exit_errmsg(ats_int_type n, ats_ptr_type msg) ;

/* ****** ****** */
//
#define atslib_htons htons
#define atslib_ntohs ntohs
//
#define atslib_htonl htonl
#define atslib_ntohl ntohl
//
/* ****** ****** */

ATSinline()
ats_bool_type
atslib_inet_aton_err (
  ats_ptr_type cp, ats_ref_type inp
) {
  int rtn ;
  rtn = inet_aton((char*)cp, (ats_in_addr_type*)inp) ;
  return (rtn ? ats_true_bool : ats_false_bool) ;
} // end of [atslib_inet_aton_err]

ATSinline()
ats_void_type
atslib_inet_aton_exn (
  ats_ptr_type cp, ats_ref_type inp
) {
  int rtn ;
  rtn = inet_aton((char*)cp, (ats_in_addr_type*)inp) ;
  if (rtn == 0) {
    ats_exit_errmsg(EXIT_FAILURE, "exit(ATS): [inet_aton] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_inet_aton_exn] */

/* ****** ****** */

#define atslib_inet_addr inet_addr
#define atslib_inet_network inet_network

/* ****** ****** */

#define atslib_inet_makeaddr inet_makeaddr

/* ****** ****** */

#define atslib_inet_ntoa inet_ntoa

/* ****** ****** */

#define atslib_inet4_pton(cp, inp) inet_pton(AF_INET4, cp, inp)
#define atslib_inet6_pton(cp, inp) inet_pton(AF_INET6, cp, inp)

/* ****** ****** */

#endif /* end of [ATS_LIBC_ARPA_INET_CATS] */

/* end of [in.cats] */
