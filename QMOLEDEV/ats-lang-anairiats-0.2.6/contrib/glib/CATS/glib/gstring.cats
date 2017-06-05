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

#ifndef ATSCTRB_GLIB_GSTRING_CATS
#define ATSCTRB_GLIB_GSTRING_CATS

/* ****** ****** */

#include "glib/gstring.h"

/* ****** ****** */

typedef GString *GString_ptr ;

/* ****** ****** */

ATSinline()
ats_ptr_type
atsctrb_g_string_get_str
  (ats_ptr_type string) {
  return ((GString*)string)->str ;
} // end of [atsctrb_g_string_get_str]

ATSinline()
gsize
atsctrb_g_string_get_len
  (ats_ptr_type string) {
  return ((GString*)string)->len ;
} // end of [atsctrb_g_string_get_len]

ATSinline()
gsize
atsctrb_g_string_get_allocated_len
  (ats_ptr_type string) {
  return ((GString*)string)->allocated_len ;
} // end of [atsctrb_g_string_get_allocated_len]

/* ****** ****** */

#define atsctrb_g_string_new_null() g_string_new ((char*)0)
#define atsctrb_g_string_new_init(init) g_string_new (init)

/* ****** ****** */

#define atsctrb_g_string_printf g_string_printf
#define atsctrb_g_string_append_printf g_string_append_printf

/* ****** ****** */

#define atsctrb_g_string_assign g_string_assign

/* ****** ****** */

#define atsctrb_g_string_append g_string_append
#define atsctrb_g_string_append_c g_string_append_c
#define atsctrb_g_string_append_len g_string_append_len

/* ****** ****** */

#define atsctrb_g_string_prepend g_string_prepend
#define atsctrb_g_string_prepend_c g_string_prepend_c
#define atsctrb_g_string_prepend_len g_string_prepend_len

/* ****** ****** */

#define atsctrb_g_string_insert g_string_insert
#define atsctrb_g_string_insert_c g_string_insert_c
#define atsctrb_g_string_insert_len g_string_insert_len

/* ****** ****** */

#define atsctrb_g_string_overwrite g_string_overwrite
#define atsctrb_g_string_overwrite_len g_string_overwrite_len

/* ****** ****** */

#define atsctrb_g_string_erase g_string_erase
#define atsctrb_g_string_truncate g_string_truncate
#define atsctrb_g_string_set_size g_string_set_size

/* ****** ****** */

#define atsctrb_g_string_free_true(x) g_string_free(x, TRUE)

/* ****** ****** */

#define atsctrb_g_string_hash g_new_string_hash
#define atsctrb_g_string_equal g_new_string_equal

/* ****** ****** */

#endif /* ATSCTRB_GLIB_GSTRING_CATS */
