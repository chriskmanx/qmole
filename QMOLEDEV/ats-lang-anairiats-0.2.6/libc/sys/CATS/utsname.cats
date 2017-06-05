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

#ifndef ATS_LIBC_SYS_UTSNAME_CATS
#define ATS_LIBC_SYS_UTSNAME_CATS

/* ****** ****** */

#include <sys/utsname.h>

/* ****** ****** */

typedef struct utsname ats_utsname_type ;

/* ****** ****** */

#define atslib_utsname_get_sysname(x) (&(((ats_utsname_type*)x)->sysname[0]))
#define atslib_utsname_get_nodename(x) (&(((ats_utsname_type*)x)->nodename[0]))
#define atslib_utsname_get_release(x) (&(((ats_utsname_type*)x)->release[0]))
#define atslib_utsname_get_version(x) (&(((ats_utsname_type*)x)->version[0]))
#define atslib_utsname_get_machine(x) (&(((ats_utsname_type*)x)->machine[0]))
#ifdef _GNU_SOURCE
#define atslib_utsname_get_domainname(x) (&(((ats_utsname_type*)x)->domainname[0]))
#endif // end of [_GNU_SOURCE]

/* ****** ****** */

#define atslib_uname uname

/* ****** ****** */

#endif /* end of [ATS_LIBC_SYS_UTSNAME_CATS] */

/* end of [utsname.cats] */
