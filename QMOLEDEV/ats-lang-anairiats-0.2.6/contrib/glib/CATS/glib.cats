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

#ifndef ATSCTRB_GLIB_CATS
#define ATSCTRB_GLIB_CATS

/* ****** ****** */

#include "glib.h"
//
#include "contrib/glib/CATS/glib/gbasics.cats"
//
#define atsctrb_g_strdup g_strdup
#define atsctrb_g_strndup g_strndup
#define atsctrb_g_strdup_printf g_strdup_printf
#define atsctrb_g_strnfill g_strnfill
//
#include "contrib/glib/CATS/glib/garray.cats"
#include "contrib/glib/CATS/glib/ghash.cats"
#include "contrib/glib/CATS/glib/glist.cats" // doubly-linked
#include "contrib/glib/CATS/glib/gmem.cats"
#include "contrib/glib/CATS/glib/gqsort.cats"
#include "contrib/glib/CATS/glib/grand.cats"
#include "contrib/glib/CATS/glib/gslist.cats" // singly-linked
#include "contrib/glib/CATS/glib/gstring.cats"
#include "contrib/glib/CATS/glib/gutils.cats"

/* ****** ****** */

#endif /* ATSCTRB_GLIB_CATS */
