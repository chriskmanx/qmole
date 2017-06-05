/* ******************************************************************* */
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/* ******************************************************************* */

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

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/*
** A linker error is issued if a user does not define _ATS_MULTITHREAD and
** tries to use them anyways
*/

/* ****** ****** */

#ifndef ATS_LIBC_PTHREAD_CATS
#define ATS_LIBC_PTHREAD_CATS

/* ****** ****** */

#ifdef _ATS_MULTITHREAD
//
// #define THREAD_SAFE // HX: what is this?
//
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>
//
/* ****** ****** */

#define atslib_pthread_attr_init pthread_attr_init
#define atslib_pthread_attr_destroy pthread_attr_destroy

ATSinline()
ats_void_type
atslib_pthread_attr_init_exn
  (ats_ref_type p_attr) {
  if (pthread_attr_init((pthread_attr_t*)p_attr)) {
    ats_exit_errmsg (EXIT_FAILURE, "exit(ATS): [pthread_attr_init_exn] failed\n") ;
  } // end of [if]
  return ;  
} // end of [atslib_pthread_attr_init_exn]

ATSinline()
ats_void_type
atslib_pthread_attr_destroy_exn
  (ats_ref_type p_attr) {
  if (pthread_attr_destroy((pthread_attr_t*)p_attr)) {
    ats_exit_errmsg (EXIT_FAILURE, "exit(ATS): [pthread_attr_destroy_exn] failed\n") ;
  } // end of [if]
  return ;  
} // end of [atslib_pthread_attr_destroy_exn]

/* ****** ****** */

#define atslib_pthread_create pthread_create

/* ****** ****** */

#define atslib_pthread_self pthread_self

/* ****** ****** */

#define atslib_pthread_join pthread_join

/* ****** ****** */

#define atslib_pthread_exit pthread_exit

/* ****** ****** */

#define atslib_pthread_cancel pthread_cancel
#define atslib_pthread_testcancel pthread_testcancel

/* ****** ****** */

#define atslib_pthread_cleanup_pop pthread_cleanup_pop

/* ****** ****** */

#define atslib_pthread_mutex_lock pthread_mutex_lock
#define atslib_pthread_mutex_unlock pthread_mutex_unlock

/* ****** ****** */

#define atslib_pthread_cond_wait pthread_cond_wait
#define atslib_pthread_cond_signal pthread_cond_signal
#define atslib_pthread_cond_broadcast pthread_cond_broadcast

/* ****** ****** */

#endif // end of [_ATS_MULTITHREAD]

/* ****** ****** */

#endif // end of [ATS_LIBC_PTHREAD_CATS]

/* end of [pthread.cats] */
