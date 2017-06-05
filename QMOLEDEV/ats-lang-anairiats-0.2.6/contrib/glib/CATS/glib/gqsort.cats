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

#ifndef ATSCTRB_GLIB_GQSORT_CATS
#define ATSCTRB_GLIB_GQSORT_CATS

/* ****** ****** */

#include "glib/gqsort.h"

/* ****** ****** */

static inline
ats_void_type
atsctrb_g_qsort_with_data (
  ats_ref_type pbase
, gint n
, ats_size_type size
, ats_fun_ptr_type compare_func
, ats_ptr_type env
) {
  g_qsort_with_data (
    (gconstpointer)pbase
  , (gint)n
  , (gsize)size
  , (GCompareDataFunc)compare_func
  , (gpointer)env
  ) ; return ;
} // end of [atsctrb_g_qsort_with_data]

/* ****** ****** */

#endif /* ATSCTRB_GLIB_GQSORT_CATS */
