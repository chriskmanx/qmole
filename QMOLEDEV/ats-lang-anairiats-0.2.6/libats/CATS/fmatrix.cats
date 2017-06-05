/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2009 Hongwei Xi.
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*/

/* ****** ****** */

/*
**
** An interface for ATS to interact with BLAS and LAPACK
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*/

/* ****** ****** */

#ifndef ATS_LIBATS_FMATRIX_CATS
#define ATS_LIBATS_FMATRIX_CATS

/* ****** ****** */

static inline
ats_ptr_type
atslib_fmatrix_ptr_takeout_tsz (
  ats_ptr_type base
, ats_size_type m
, ats_size_type i
, ats_size_type j
, ats_size_type tsz
) {
  // the representation is column-major
  return ((char*)base) + (i + j * m) * tsz ;
} /* end of [atslib_fmatrix_ptr_takeout_tsz] */

/* ****** ****** */

#endif /* [ATS_LIBATS_FMATRIX_CATS] */

/* end of [fmatrix.cats] */
