/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
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

#ifndef ATSCTRB_GLIB_GARRAY_CATS
#define ATSCTRB_GLIB_GARRAY_CATS

/* ****** ****** */

#include "glib/garray.h"

/* ****** ****** */

typedef GArray *GArray_ref ;

/* ****** ****** */

#define atsctrb_g_array_new g_array_new
#define atsctrb_g_array_sized_new g_array_sized_new
#define atsctrb_g_array_ref g_array_ref
#define atsctrb_g_array_unref g_array_unref

/* ****** ****** */

#define atsctrb_g_array_set_size g_array_set_size

/* ****** ****** */

ATSinline()
ats_void_type
atsctrb_g_array_free_true (
  ats_ptr_type array
) {
  g_array_free ((GArray*)array, TRUE) ; return ;
} // end of [atsctrb_g_array_free_true]

/* ****** ****** */

#define atsctrb_g_array_append_val(array, v) \
  g_array_append_val ((GArray*)(array), *(v))

#define atsctrb_g_array_prepend_val(array, v) \
  g_array_prepend_val ((GArray*)(array), *(v))

/* ****** ****** */

#define atsctrb_g_array_sort g_array_sort

/* ****** ****** */

ATSinline()
ats_ptr_type
atsctrb_g_array_takeout_tsz (
  ats_ptr_type array, gint i, ats_size_type tsz
) {
  return (&g_array_index((GArray*)array, char, 0)) + i * tsz ;
} // end of [atsctrb_g_array_takeout]

/* ****** ****** */

#endif /* ATSCTRB_GLIB_GARRAY_CATS */
