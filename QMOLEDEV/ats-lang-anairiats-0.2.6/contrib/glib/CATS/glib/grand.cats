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

#ifndef ATSCTRB_GLIB_GRAND_CATS
#define ATSCTRB_GLIB_GRAND_CATS

/* ****** ****** */

#include "glib/grand.h"

/* ****** ****** */

typedef GRand *GRand_ptr ;

/* ****** ****** */

#define atsctrb_g_rand_new_with_seed g_rand_new_with_seed
#define atsctrb_g_rand_new_with_seed_array g_rand_new_with_seed_array

#define atsctrb_g_rand_new g_rand_new
#define atsctrb_g_rand_copy g_rand_copy
#define atsctrb_g_rand_free g_rand_free

#define atsctrb_g_rand_set_seed g_rand_set_seed
#define atsctrb_g_rand_set_seed_array g_rand_set_seed_array

#define atsctrb_g_rand_boolean g_rand_boolean
#define atsctrb_g_rand_int g_rand_int
#define atsctrb_g_rand_int_range g_rand_int_range
#define atsctrb_g_rand_double g_rand_double
#define atsctrb_g_rand_double_range g_rand_double_range

/* ****** ****** */

#define atsctrb_g_random_set_seed g_random_set_seed

#define atsctrb_g_random_boolean g_random_boolean
#define atsctrb_g_random_int g_random_int
#define atsctrb_g_random_int_range g_random_int_range
#define atsctrb_g_random_double g_random_double
#define atsctrb_g_random_double_range g_random_double_range

/* ****** ****** */

#endif /* ATSCTRB_GLIB_GRAND_CATS */
