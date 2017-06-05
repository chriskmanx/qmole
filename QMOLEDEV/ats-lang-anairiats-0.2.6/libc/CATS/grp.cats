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

#ifndef ATS_LIBC_GRP_CATS
#define ATS_LIBC_GRP_CATS

/* ****** ****** */

#include <sys/types.h>
#include <grp.h>

/* ****** ****** */

typedef struct group ats_group_type ;

/* ****** ****** */

ATSinline()
ats_ptr_type
atslib_group_get_gr_name
  (ats_ptr_type gr) {
  return ((ats_group_type*)gr)->gr_name ;
} // end of [group_get_gr_name]

ATSinline()
ats_ptr_type
atslib_group_get_gr_passwd
  (ats_ptr_type gr) {
  return ((ats_group_type*)gr)->gr_passwd ;
} // end of [group_get_gr_passwd]

ATSinline()
ats_ptr_type
atslib_group_get_gr_mem
  (ats_ptr_type gr) {
  return ((ats_group_type*)gr)->gr_mem ;
} // end of [group_get_gr_mem]

/* ****** ****** */

#define atslib_getgrnam getgrnam
#define atslib_getgrgid getgrgid

/* ****** ****** */

#endif /* ATS_LIBC_GRP_CATS */

/* end of [grp.cats] */
