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

#ifndef ATS_LIBC_DIRENT_CATS
#define ATS_LIBC_DIRENT_CATS

/* ****** ****** */

#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdio.h> // for [perror]

/* ****** ****** */

#include "ats_types.h"

/* ****** ****** */

/*
typedef ino_t ats_ino_type ; // defined in sys/CATS/types.cats
*/

/* ****** ****** */

typedef DIR ats_DIR_type ;
typedef struct dirent ats_dirent_type ;

/* ****** ****** */
//
// HX: implemented in [prelude/DATS/basics.dats]
//
extern
ats_void_type
ats_exit_errmsg
  (ats_int_type n, ats_ptr_type msg) ;
// end of [ats_exit_errmsg]
//
// HX: implemented in [prelude/CATS/printf.cats]
//
extern
ats_void_type
atspre_exit_prerrf
  (ats_int_type code, ats_ptr_type fmt, ...) ;
// end of [atspre_exit_prerrf]

/* ****** ****** */

ATSinline()
ats_ino_type
atslib_dirent_get_d_ino
  (ats_ptr_type dir) { return ((ats_dirent_type*)dir)->d_ino ; }
// end of [atslib_dirent_get_d_ino]

ATSinline()
ats_ptr_type
atslib_dirent_get_d_name
  (ats_ptr_type dir) { return ((ats_dirent_type*)dir)->d_name ; }
// end of [atslib_dirent_get_d_name]

/* ****** ****** */

#define atslib_opendir_err opendir

ATSinline()
ats_ptr_type
atslib_opendir_exn (ats_ref_type path) {
  DIR* ret = opendir (path) ; if (!ret) {
    perror ("opendir") ;
    atspre_exit_prerrf (errno, "exit(ATS): [opendir(%s)] failed.\n", path) ;
  } // end of [if]
  return ret ;
} /* end of [atslib_opendir_exn] */

/* ****** ****** */

#define atslib_closedir_err closedir

ATSinline()
ats_void_type
atslib_closedir_exn (ats_ptr_type dir) {
  int err = closedir (dir) ; if (err < 0) {
    perror ("closedir") ;
    ats_exit_errmsg (errno, "exit(ATS): [closedir] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_closedir_exn] */

/* ****** ****** */

#define atslib_readdir readdir

ATSinline()
ats_int_type
atslib_readdir_r (
  ats_ptr_type dir, ats_ref_type ent, ats_ref_type ret
) {
  int err = readdir_r (
    (DIR*)dir, (ats_dirent_type*)ent, (ats_dirent_type**)ret
  ) ;
  return err ;
} /* end of [atslib_readdir_r] */

/* ****** ****** */

#define atslib_rewinddir rewinddir 
#define atslib_seekdir seekdir
#define atslib_telldir telldir

/* ****** ****** */

#endif /* ATS_LIBC_DIRENT_CATS */
