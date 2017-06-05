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
** Copyright (C) 2002-2011 Hongwei Xi.
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
** Start Time: October, 2011
**
*/

/* ****** ****** */

#ifndef ATS_LIBATS_LINORDSET_RANDBST_CATS
#define ATS_LIBATS_LINORDSET_RANDBST_CATS

/* ****** ****** */
//
// HX: __USE_MISC must be turned on
//
#include <stdlib.h>

/* ****** ****** */

typedef struct {
  void (*free) (void *obj) ;
  double (*get_double) (void *obj) ;
  void (*setseed) (void *obj, unsigned long int seed) ;
} linordset_rngobj_struct ;

typedef linordset_rngobj_struct *linordset_rngobj ;

/* ****** ****** */

typedef struct {
  linordset_rngobj_struct rng ;
  struct drand48_data state ; // the state of RNG
} linordset_rngobj_struct_drand48 ;

static
ats_void_type
linordset_rngobj_free_drand48
  (void *obj) {
  ATS_FREE (obj) ;  return ;
} // end of [rngobj_free_drand48]

static
ats_double_type
linordset_rngobj_get_double_drand48 (
  void *obj
) {
  double result ;
  drand48_r (
    &((linordset_rngobj_struct_drand48*)obj)->state, &result
  ) ; return result;
} // end of [linordset_rngobj_get_double_drand48]

static
ats_void_type
linordset_rngobj_setseed_drand48 (
  void *obj, unsigned long int seed
) {
  srand48_r (
    seed, &((linordset_rngobj_struct_drand48*)obj)->state
  ) ; return ;
} // end of [linordset_rngobj_setseed_drand48]

ATSinline()
ats_ptr_type
atslib_linordset_rngobj_make_drand48 (
) {
  linordset_rngobj_struct *objrng ;
  linordset_rngobj_struct_drand48 *obj ;
  obj = ATS_MALLOC (sizeof(linordset_rngobj_struct_drand48)) ;
  objrng = &(obj->rng) ;
  objrng->free = &linordset_rngobj_free_drand48 ;
  objrng->get_double = &linordset_rngobj_get_double_drand48 ;
  objrng->setseed = &linordset_rngobj_setseed_drand48 ;
  return (obj) ;
} // end of [linordset_rngobj_make_drand48]

/* ****** ****** */

#endif /* ATS_LIBATS_LINORDSET_RANDBST_CATS */

/* end of [linordset_randbst.cats] */ 
