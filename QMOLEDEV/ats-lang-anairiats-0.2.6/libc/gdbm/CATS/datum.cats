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

#include "libc/CATS/string.cats"

/* ****** ****** */

typedef datum ats_datum_type ;

/* ****** ****** */

ATSinline()
ats_bool_type
atslib_gdbm_datum_is_valid
  (ats_datum_type x) { return (x.dptr != NULL) ; }
// end of [atslib_gdbm_datum_is_valid]

ATSinline()
ats_ptr_type
atslib_gdbm_datum_takeout_ptr
  (ats_datum_type x) { return x.dptr ; }
// end of [atslib_gdbm_datum_takeout_ptr]

/* ****** ****** */

extern
ats_ptr_type atslib_strdup_gc (ats_ptr_type str) ;

ATSinline()
ats_datum_type
atslib_gdbm_datum_make0_string
  (char *str) {
  datum res ;
  res.dptr = str ;
  res.dsize = (int)(atslib_strlen(str) + 1) ; // HX: account for the trailing null char!
  return res ;
} // end of [atslib_gdbm_datum_make0_string]

ATSinline()
ats_datum_type
atslib_gdbm_datum_make1_string
  (char *str) {
  datum res ;
  res.dptr = (char*)atslib_strdup_gc(str) ;
  res.dsize = (int)(atslib_strlen(str) + 1) ; // HX: account for the trailing null char!
  return res ;
} // end of [atslib_gdbm_datum_make1_string]

/* ****** ****** */

ATSinline()
ats_void_type
atslib_gdbm_datum_free
  (ats_datum_type x) {
  if (x.dptr != NULL) ATS_FREE (x.dptr) ; return ;
} // end of [atslib_gdbm_datum_free]

/* ****** ****** */

/* end of [datum.cats] */
