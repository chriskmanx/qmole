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

#ifndef ATSCTRB_GLIB_GLIST_CATS
#define ATSCTRB_GLIB_GLIST_CATS

/* ****** ****** */

typedef GList *GList_ptr ;

/* ****** ****** */

ATSinline()
ats_bool_type
atsctrb_g_list_is_nil
  (ats_ptr_type list) {
  return (list == (ats_ptr_type)0 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_g_list_is_nil]

ATSinline()
ats_bool_type
atsctrb_g_list_isnot_nil
  (ats_ptr_type list) {
  return (list != (ats_ptr_type)0 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_g_list_isnot_nil]

/* ****** ****** */

ATSinline()
ats_ptr_type
atsctrb_g_list_new_nil () { return (GList_ptr)0 ; }

ATSinline()
ats_void_type
atsctrb_g_list_free_nil (ats_ptr_type xs) { return ; }

/* ****** ****** */

#define atsctrb_g_list_free g_list_free
#define atsctrb_g_list_free1 g_list_free1

#define atsctrb_g_list_next g_list_next
#define atsctrb_g_list_previous g_list_previous
#define atsctrb_g_list_last g_list_last
#define atsctrb_g_list_first g_list_first

#define atsctrb_g_list_append g_list_append
#define atsctrb_g_list_prepend g_list_prepend

#define atsctrb_g_list_length g_list_length

/* ****** ****** */

#define atsctrb_g_list_insert g_list_insert

ATSinline()
ats_ptr_type
atsctrb_g_list_insert_sorted (
  ats_ptr_type list, ats_ptr_type x, ats_fun_ptr_type cmp
) {
  return g_list_insert_sorted
    ((GList_ptr)list, (gpointer)x, (GCompareFunc)cmp) ;
} // end of [atsctrb_g_list_insert_sorted]

/* ****** ****** */

ATSinline()
ats_ptr_type
atsctrb_g_list_remove_current (
  ats_ptr_type list, ats_ref_type node
) {
  *(GList_ptr*)node = (GList_ptr)list ;
  return g_list_remove_link (list, list) ;
} // end of [atsctrb_g_list_remove_current]

/* ****** ****** */

#define atsctrb_g_list_concat g_list_concat
#define atsctrb_g_list_copy g_list_copy
#define atsctrb_g_list_reverse g_list_reverse

#define atsctrb_g_list_sort g_list_sort
#define atsctrb_g_list_sort_with_data g_list_sort_with_data

#define atsctrb_g_list_foreach g_list_foreach

#define atsctrb_g_list_nth g_list_nth
#define atsctrb_g_list_nth_data g_list_nth_data

#define atsctrb_g_list_index g_list_index

/* ****** ****** */

#endif /* ATSCTRB_GLIB_GLIST_CATS */
