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

/*
** A linking error is issued if a user does not define
** [_ATS_MULTITHREAD] while triing to use them anyways
*/

/*
#ifndef _ATS_MULTITHREAD
#error "[pthread_upbarr.cats]: _ATS_MULTITHREAD is undefined!"
#endif
*/

/* ****** ****** */

#ifndef ATS_LIBC_PTHREAD_UPBARR_CATS
#define ATS_LIBC_PTHREAD_UPBARR_CATS

/* ****** ****** */

#ifdef _ATS_MULTITHREAD

/* ****** ****** */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

/* ****** ****** */

// barrs and tickets for uploading

typedef struct {
  int count ; /* outstanding tickets */
  pthread_cond_t cond_eqz; /* for signaling [count==0] */
  pthread_mutex_t mutex_res; /* for resource protection */
} ats_pthread_upbarr_t ; /* linear barr uploading */

typedef ats_pthread_upbarr_t ats_pthread_upticket_t ;

/* ****** ****** */

#endif // end of [#ifdef _ATS_MULTITHREAD]

/* ****** ****** */

#endif // end of [#ifndef ATS_LIBC_PTHREAD_UPBARR_CATS]

/* ****** ****** */

/* end of [pthread_upbarr.cats] */
