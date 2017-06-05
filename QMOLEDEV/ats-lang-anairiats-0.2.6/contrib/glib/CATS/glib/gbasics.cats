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

#ifndef ATSCTRB_GLIB_GBASICS_CATS
#define ATSCTRB_GLIB_GBASICS_CATS

/* ****** ****** */

#include "glib/gtypes.h"

/* ****** ****** */

ATSinline()
gint atsctrb_add_gint_gint (gint x, gint y) { return (x + y) ; }
ATSinline()
gint atsctrb_sub_gint_gint (gint x, gint y) { return (x - y) ; }
ATSinline()
gint atsctrb_mul_gint_gint (gint x, gint y) { return (x * y) ; }
ATSinline()
gint atsctrb_div_gint_gint (gint x, gint y) { return (x / y) ; }

/* ****** ****** */

ATSinline()
ats_bool_type
atsctrb_lt_gint_gint
  (gint x, gint y) {
  return (x < y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_lt_gint_gint]

ATSinline()
ats_bool_type
atsctrb_lte_gint_gint
  (gint x, gint y) {
  return (x <= y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_lte_gint_gint]

ATSinline()
ats_bool_type
atsctrb_gt_gint_gint
  (gint x, gint y) {
  return (x > y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_gt_gint_gint]

ATSinline()
ats_bool_type
atsctrb_gte_gint_gint
  (gint x, gint y) {
  return (x >= y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_gte_gint_gint]

ATSinline()
ats_bool_type
atsctrb_eq_gint_gint
  (gint x, gint y) {
  return (x == y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_eq_gint_gint]

ATSinline()
ats_bool_type
atsctrb_neq_gint_gint
  (gint x, gint y) {
  return (x != y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_neq_gint_gint]

/* ****** ****** */

ATSinline()
guint atsctrb_add_guint_guint (guint x, guint y) { return (x + y) ; }
ATSinline()
guint atsctrb_sub_guint_guint (guint x, guint y) { return (x - y) ; }
ATSinline()
guint atsctrb_mul_guint_guint (guint x, guint y) { return (x * y) ; }
ATSinline()
guint atsctrb_div_guint_guint (guint x, guint y) { return (x / y) ; }

/* ****** ****** */

ATSinline()
ats_bool_type
atsctrb_lt_guint_guint
  (guint x, guint y) {
  return (x < y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_lt_guint_guint]

ATSinline()
ats_bool_type
atsctrb_lte_guint_guint
  (guint x, guint y) {
  return (x <= y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_lte_guint_guint]

ATSinline()
ats_bool_type
atsctrb_gt_guint_guint
  (guint x, guint y) {
  return (x > y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_gt_guint_guint]

ATSinline()
ats_bool_type
atsctrb_gte_guint_guint
  (guint x, guint y) {
  return (x >= y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_gte_guint_guint]

ATSinline()
ats_bool_type
atsctrb_eq_guint_guint
  (guint x, guint y) {
  return (x == y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_eq_guint_guint]

ATSinline()
ats_bool_type
atsctrb_neq_guint_guint
  (guint x, guint y) {
  return (x != y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_neq_guint_guint]

/* ****** ****** */

ATSinline()
ats_bool_type
atsctrb_lt_gint32_gint32
  (gint32 x, gint32 y) {
  return (x < y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_lt_gint32_gint32]

ATSinline()
ats_bool_type
atsctrb_lte_gint32_gint32
  (gint32 x, gint32 y) {
  return (x <= y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_lte_gint32_gint32]

ATSinline()
ats_bool_type
atsctrb_gt_gint32_gint32
  (gint32 x, gint32 y) {
  return (x > y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_gt_gint32_gint32]

ATSinline()
ats_bool_type
atsctrb_gte_gint32_gint32
  (gint32 x, gint32 y) {
  return (x >= y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_gte_gint32_gint32]

ATSinline()
ats_bool_type
atsctrb_eq_gint32_gint32
  (gint32 x, gint32 y) {
  return (x == y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_eq_gint32_gint32]

ATSinline()
ats_bool_type
atsctrb_neq_gint32_gint32
  (gint32 x, gint32 y) {
  return (x != y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_neq_gint32_gint32]

/* ****** ****** */

ATSinline()
ats_bool_type
atsctrb_lt_guint32_guint32
  (guint32 x, guint32 y) {
  return (x < y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_lt_guint32_guint32]

ATSinline()
ats_bool_type
atsctrb_lte_guint32_guint32
  (guint32 x, guint32 y) {
  return (x <= y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_lte_guint32_guint32]

ATSinline()
ats_bool_type
atsctrb_gt_guint32_guint32
  (guint32 x, guint32 y) {
  return (x > y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_gt_guint32_guint32]

ATSinline()
ats_bool_type
atsctrb_gte_guint32_guint32
  (guint32 x, guint32 y) {
  return (x >= y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_gte_guint32_guint32]

ATSinline()
ats_bool_type
atsctrb_eq_guint32_guint32
  (guint32 x, guint32 y) {
  return (x == y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_eq_guint32_guint32]

ATSinline()
ats_bool_type
atsctrb_neq_guint32_guint32
  (guint32 x, guint32 y) {
  return (x != y ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_neq_guint32_guint32]

/* ****** ****** */

#define atsctrb_GPOINTER_TO_INT GPOINTER_TO_INT
#define atsctrb_GINT_TO_POINTER GINT_TO_POINTER

/* ****** ****** */

//
// HX-2010-05-19:
// this one is implemented in $ATSHOME/prelude/CATS/string.cats
//
extern ats_int_type
atspre_compare_string_string
  (ats_ptr_type x1, ats_ptr_type x2) ;
// end of [atspre_compare_string_string]

#define atsctrb_compare_gstring_gstring \
  atspre_compare_string_string

ATSinline ()
ats_int_type
atsctrb_compare_gstring0_gstring0
  (ats_ptr_type x1, ats_ptr_type x2) {
  if (!x1) {
    if (!x2) return ats_true_bool ; else return ats_false_bool ;
  } // end of [if]
  if (!x2) return ats_false_bool ; // x1 != NULL
  return atspre_compare_string_string (x1, x2) ;
} // end of [atsctrb_compare_gstring0_gstring0]

/* ****** ****** */

#endif /* ATSCTRB_GLIB_GBASICS_CATS */
