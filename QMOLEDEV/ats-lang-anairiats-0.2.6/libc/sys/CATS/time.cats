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

#ifndef ATS_LIBC_SYS_TIME_CATS
#define ATS_LIBC_SYS_TIME_CATS

/* ****** ****** */

#include <sys/time.h>

/* ****** ****** */

typedef struct timeval ats_timeval_type ;
typedef struct timezone ats_timezone_type ;

/* ****** ****** */

#define atslib_gettimeofday_tv(tv) gettimeofday(tv, NULL)
#define atslib_gettimeofday_tz(tz) gettimeofday(NULL, tz)

#define atslib_settimeofday_tv(tv) settimeofday(tv, NULL)
#define atslib_settimeofday_tz(tz) settimeofday(NULL, tz)
#define atslib_settimeofday_tvtz(tv, tz) settimeofday(tv, tz)

/* ****** ****** */

#define atslib_utimes utimes
#define atslib_futimes futimes
#define atslib_futimesat futimesat

/* ****** ****** */

typedef struct itimerval ats_itimerval_type ;

#define atslib_getitimer getitimer
#define atslib_setitimer setitimer
#define atslib_setitimer_null(which, itval) setitimer(which, itval, NULL)

/* ****** ****** */

#endif /* end of [ATS_LIBC_SYS_TIME_CATS] */

/* end of [time.cats] */
