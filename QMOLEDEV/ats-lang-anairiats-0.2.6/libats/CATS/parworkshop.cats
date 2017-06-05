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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*/

/* ****** ****** */

/*
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) 
**
*/

/* ****** ****** */

#ifndef ATS_LIBATS_PARWORKSHOP_CATS
#define ATS_LIBATS_PARWORKSHOP_CATS

/* ****** ****** */

#include "libc/CATS/pthread.cats"

/* ****** ****** */

#include "libats/CATS/linqueue_arr.cats"

/* ****** ****** */

typedef struct {
  pthread_mutex_t WSmut ;
//
  atslib_linqueue_arr_QUEUE WQ ;
//
  pthread_cond_t WQemp ;
  pthread_cond_t WQful ;
  ats_int_type nworker ;
  pthread_cond_t WSisz ; // nworker = 0
  ats_int_type npaused ;
  pthread_cond_t WSpaused ;
  pthread_cond_t WSequ1 ; // npaused = nworker
  ats_int_type nblocked ;
  pthread_cond_t WSequ2 ; // nblocked = nworker
  ats_fun_ptr_type fwork ;
  ats_int_type refcount ;
} atslib_parworkshop_WORKSHOP ;

/* ****** ****** */

ATSinline()
ats_fun_ptr_type
atslib_parworkshop_workshop_get_fwork
  (ats_ptr_type ws) {
  return ((atslib_parworkshop_WORKSHOP*)ws)->fwork ;
} // end of [atslib_parworkshop_workshop_get_fwork]

/* ****** ****** */

ATSinline()
ats_ptr_type
atslib_parworkshop_workshop_get_WSmut
  (ats_ptr_type ws) {
  return &((atslib_parworkshop_WORKSHOP*)ws)->WSmut ;
} // end of [atslib_parworkshop_workshop_get_WSmut]

/* ****** ****** */

ATSinline()
ats_ptr_type
atslib_parworkshop_workshop_get_WQemp
  (ats_ptr_type ws) {
  return &((atslib_parworkshop_WORKSHOP*)ws)->WQemp ;
} // end of [atslib_parworkshop_workshop_get_WQemp]

ATSinline()
ats_ptr_type
atslib_parworkshop_workshop_get_WQful
  (ats_ptr_type ws) {
  return &((atslib_parworkshop_WORKSHOP*)ws)->WQful ;
} // end of [atslib_parworkshop_workshop_get_WQful]

/* ****** ****** */

ATSinline()
ats_ptr_type
atslib_parworkshop_workshop_get_WSisz
  (ats_ptr_type ws) {
  return &((atslib_parworkshop_WORKSHOP*)ws)->WSisz ;
} // end of [atslib_parworkshop_workshop_get_WSisz]

ATSinline()
ats_ptr_type
atslib_parworkshop_workshop_get_WSpaused
  (ats_ptr_type ws) {
  return &((atslib_parworkshop_WORKSHOP*)ws)->WSpaused ;
} // end of [atslib_parworkshop_workshop_get_WSpaused]

ATSinline()
ats_ptr_type
atslib_parworkshop_workshop_get_WSequ1
  (ats_ptr_type ws) {
  return &((atslib_parworkshop_WORKSHOP*)ws)->WSequ1 ;
} // end of [atslib_parworkshop_workshop_get_WSequ1]

ATSinline()
ats_ptr_type
atslib_parworkshop_workshop_get_WSequ2
  (ats_ptr_type ws) {
  return &((atslib_parworkshop_WORKSHOP*)ws)->WSequ2 ;
} // end of [atslib_parworkshop_workshop_get_WSequ2]

/* ****** ****** */

ATSinline()
ats_ptr_type
atslib_parworkshop_workshop_acquire
  (ats_ptr_type ws) {
  pthread_mutex_lock (&((atslib_parworkshop_WORKSHOP*)ws)->WSmut) ;
  return ws ;
} // end of [atslib_parworkshop_workshop_acquire]

ATSinline()
ats_void_type
atslib_parworkshop_workshop_release
  (ats_ptr_type ws) {
  pthread_mutex_unlock (&((atslib_parworkshop_WORKSHOP*)ws)->WSmut) ;
  return ;
} // end of [atslib_parworkshop_workshop_release]

/* ****** ****** */

#endif /* ATS_LIBATS_PARWORKSHOP_CATS */

/* end of [parworkshop.cats] */ 
