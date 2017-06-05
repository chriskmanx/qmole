/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// June 2008
//
/* ****** ****** */

/* include some .h files */
#include "ats_config.h"
#include "ats_basics.h"
#include "ats_exception.h"
#include "ats_memory.h"
#include "ats_types.h"

/* ****** ****** */

/* include some .cats files */
#include "prelude/CATS/array.cats"
#include "prelude/CATS/basics.cats"
#include "prelude/CATS/bool.cats"
#include "prelude/CATS/byte.cats"
#include "prelude/CATS/char.cats"
#include "prelude/CATS/float.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/pointer.cats"
#include "prelude/CATS/printf.cats"
#include "prelude/CATS/reference.cats"
#include "prelude/CATS/string.cats"

/* ****** ****** */

#include "gcats1.cats"

#include "gcats1_top_dats.c"

#include "gcats1_misc_dats.c"

#include "gcats1_freeitmlst_dats.c"

#include "gcats1_chunk_dats.c"

#include "gcats1_globalentry_dats.c"

#ifdef _ATS_MULTITHREAD
#include "gcats1_multithread_dats.c"
#endif

#include "gcats1_marking_dats.c"

#include "gcats1_collecting_dats.c"

#include "gcats1_autops_dats.c"

#include "gcats1_manops_dats.c"

/* ****** ****** */

/* end of [gcats1_all.cats] */
