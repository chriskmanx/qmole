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

#ifndef ATS_LIBC_DLFCN_CATS
#define ATS_LIBC_DLFCN_CATS

/* ****** ****** */

#include <dlfcn.h>
#include <stdio.h>

extern void exit (int) ;

/* ****** ****** */

ATSinline()
ats_int_type
atslib_lor_dlopen_flag_dlopen_flagext
  (ats_int_type flag, ats_int_type ext) { return (flag | ext) ;
} // end of [atslib_lor_dlopen_flag_dlopen_flagext]

/* ****** ****** */

#define atslib_dlopen dlopen

ATSinline()
ats_ptr_type
atslib_dlopen_exn (
  ats_ptr_type filename, ats_int_type flag
) {
  void *p ; char *msg ;
  p = dlopen ((char*)filename, (int)flag) ;
  if (!p) {
    msg = dlerror () ; // HX: [msg] cannot be null
    fprintf (stderr, (ats_ptr_type)"exit(ATS): %s\n", msg) ;
    exit (1) ;
  } // end of [if]
  return p ;
} // end of [atslib_dlopen_exn]

/* ****** ****** */

#define atslib_dlclose dlclose

ATSinline()
ats_void_type
atslib_dlclose_exn
  (ats_ptr_type handle) {
  int err ; char *msg ;
  err = dlclose ((void*)handle) ;
  if (err != 0) {
    msg = dlerror () ; // HX: [msg] cannot be null
    fprintf (stderr, (ats_ptr_type)"exit(ATS): %s\n", msg) ;
    exit (1) ;
  } // end of [if]
  return ;
} // end of [atslib_dlclose_exn]

/* ****** ****** */

#define atslib_dlerror dlerror
#define atslib_dlsym dlsym

/* ****** ****** */

#endif /* ATS_LIBC_DLFCN_CATS */
