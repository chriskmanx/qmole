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

#ifndef ATS_LIBC_SYS_RESOURCE_CATS
#define ATS_LIBC_SYS_RESOURCE_CATS

/* ****** ****** */

#include <sys/resource.h>

/* ****** ****** */

typedef struct rlimit ats_rlimit_type ;
typedef struct rusage ats_rusage_type ;

/* ****** ****** */

#define atslib_getrlimit getrlimit
#define atslib_setrlimit setrlimit

/* ****** ****** */

#define atslib_getrusage getrusage

/* ****** ****** */

#define atslib_getpriority getpriority
#define atslib_setpriority setpriority

/* ****** ****** */

#endif /* end of [ATS_LIBC_SYS_RESOURCE_CATS] */

/* end of [resource.cats] */
