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

/*
** A linking error is issued if a user does not define
** [_ATS_MULTITHREAD] while triing to use them anyways
*/

/*
#ifndef _ATS_MULTITHREAD
#error "[pthread_uplock.cats]: _ATS_MULTITHREAD is undefined!"
#endif
*/

/* ****** ****** */

#ifndef ATS_LIBC_PTHREAD_UPLOCK_CATS
#define ATS_LIBC_PTHREAD_UPLOCK_CATS

/* ****** ****** */

#ifdef _ATS_MULTITHREAD

/* ****** ****** */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

/* ****** ****** */

// locks and tickets for uploading

typedef struct {
  pthread_mutex_t mutex_res; /* for resource protection */
} ats_pthread_uplock_t ; /* linear lock uploading */

typedef ats_pthread_uplock_t ats_pthread_upticket_t ;

/* ****** ****** */

ATSinline()
ats_void_type
atslib_pthread_uplock_destroy
  (ats_ptr_type p) {
  pthread_mutex_destroy (&((ats_pthread_uplock_t*)p)->mutex_res) ;
  ATS_FREE(p) ;
} // end of [atslib_pthread_uplock_destroy]

/* ****** ****** */

#endif // end of [#ifdef _ATS_MULTITHREAD]

/* ****** ****** */

#endif // end of [#ifndef ATS_LIBC_PTHREAD_UPLOCK_CATS]

/* ****** ****** */

/* end of [pthread_uplock.cats] */
