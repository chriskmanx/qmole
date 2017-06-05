/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*/

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)  */

/* ****** ****** */

#include <gdbm-ndbm.h>

/* ****** ****** */

#include "libc/gdbm/CATS/datum.cats"

/* ****** ****** */

#define atslib_ndbm_open dbm_open
#define atslib_ndbm_close dbm_close

#define atslib_ndbm_store dbm_store

#define atslib_ndbm_fetch dbm_fetch

#define atslib_ndbm_delete dbm_delete

#define atslib_ndbm_firstkey dbm_firstkey
#define atslib_ndbm_nextkey dbm_nextkey

#define atslib_ndbm_error dbm_error
#define atslib_ndbm_clearerr dbm_clearerr

#define atslib_ndbm_dirfno dbm_dirfno

#define atslib_ndbm_pagfno dbm_pagfno

#define atslib_ndbm_rdonly dbm_rdonly

/* ****** ****** */

/* end of [ndbm.cats] */
