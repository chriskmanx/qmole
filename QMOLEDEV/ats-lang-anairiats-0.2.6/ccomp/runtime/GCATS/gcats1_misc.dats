(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
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
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: June 2008
//
(* ****** ****** *)

#include "gcats1.hats"

(* ****** ****** *)

staload "gcats1.sats"

(* ****** ****** *)

#define ATSOPT_NAMESPACE "gcats1_misc_"

(* ****** ****** *)

%{^

static
int the_stack_direction = 0 ;
//
// dir=1/-1 : upward/downward
//
static
int gc_stack_dir_get_inner (int *some_ptr) {
  int some_int ;
  if (&some_int > some_ptr) return 1 ; else return -1 ;
  return 0 ; /* deadcode */
}

static
int gc_stack_dir_get_outer () {
//
static volatile void* get_inner = &gc_stack_dir_get_inner ;
//
  int some_int ;
  if (!the_stack_direction)
    the_stack_direction = ((int(*)(int*))get_inner)(&some_int) ;
/*
  fprintf (stderr, "the_stack_direction = %i\n", the_stack_direction) ;
*/
  return the_stack_direction ;
}

ats_int_type
gc_stack_dir_get () { return gc_stack_dir_get_outer () ; }

/* ****** ****** */

#ifdef _ATS_MULTITHREAD
static __thread
ats_ptr_type the_stack_beg = (ats_ptr_type)0 ;
#else /* single thread */
static
ats_ptr_type the_stack_beg = (ats_ptr_type)0 ;
#endif

ats_void_type
gc_stack_beg_set (ats_int_type dir) {
  long int pagesize, pagemask ; uintptr_t beg ;

  if (the_stack_beg) return ; // already set

  // pagesize must be a power of 2
  pagesize = sysconf(_SC_PAGESIZE) ; // system configuration
/*
  fprintf(stderr, "gc_stack_beg_set: dir = %i\n", dir) ;
  fprintf(stderr, "gc_stack_beg_set: pagesize = %li\n", pagesize) ;
  fprintf(stderr, "gc_stack_beg_set: &pagesize = %p\n", &pagesize) ;
*/
  pagemask = ~(pagesize - 1) ; // 1...10...0

  if (dir > 0) {
    beg = (uintptr_t)(&pagesize) ;
    beg &= pagemask ;
  } else {
    beg = (uintptr_t)(&pagesize) + pagesize ;
    beg &= pagemask ;
    beg -= sizeof(freeitmlst) ;
  }

  the_stack_beg = (ats_ptr_type)beg ;

  return ;
}

ats_ptr_type
gc_stack_beg_get (
  // there is no argument for this function
) {
  if (!the_stack_beg) {
    fprintf (stderr, "GC Fatal Error: [gc_stack_beg_get]") ;
    fprintf (stderr, ": [the_stack_beg] is not yet set.\n") ;
    exit (1) ;
  }
  return the_stack_beg ;
} // end of [gc_stack_beg_get]

%} // end of [%{^]

(* ****** ****** *)

(* end of [gcats1_misc.dats] *)
