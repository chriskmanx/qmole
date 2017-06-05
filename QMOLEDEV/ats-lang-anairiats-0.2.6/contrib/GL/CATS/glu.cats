/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: December, 2009

/* ****** ****** */

#ifndef ATSCTRB_GL_GLU_CATS
#define ATSCTRB_GL_GLU_CATS

/* ****** ****** */

#include <stdio.h>

/* ****** ****** */

#include <GL/glu.h>

/* ****** ****** */

typedef GLUquadricObj ats_GLUquadricObj_type ;

/* ****** ****** */

#define atsctrb_gluCylinder gluCylinder

/* ****** ****** */

#define atsctrb_gluDeleteQuadric gluDeleteQuadric

/* ****** ****** */

#define atsctrb_gluDisk gluDisk

/* ****** ****** */

#define atsctrb_gluLookAt gluLookAt

/* ****** ****** */

#define atsctrb_gluNewQuadric gluNewQuadric

static inline
ats_ptr_type
atsctrb_gluNewQuadric_exn () {
  GLUquadricObj* p_obj ;
  p_obj = gluNewQuadric() ;
  if (!p_obj) {
    fprintf (stderr, "exit(ATSCTRB/glu): [gluNewQuadric] failed.\n") ;
    exit (1) ;
  } // end of [if]
  return p_obj ;
} // end of [atsctrb_gluNewQuadric_exn]

/* ****** ****** */

#define atsctrb_gluOrtho2D gluOrtho2D

/* ****** ****** */

#define atsctrb_gluPartialDisk gluPartialDisk

/* ****** ****** */

#define atsctrb_gluPerspective gluPerspective

/* ****** ****** */

#define atsctrb_gluQuadricDrawStyle gluQuadricDrawStyle
#define atsctrb_gluQuadricNormals gluQuadricNormals
#define atsctrb_gluQuadricOrientation gluQuadricOrientation
#define atsctrb_gluQuadricTexture gluQuadricTexture

/* ****** ****** */

#define atsctrb_gluSphere gluSphere

/* ****** ****** */

#define atsctrb_gluUnProject gluUnProject

/* ****** ****** */

#endif /* ATSCTRB_GL_GLU_CATS */

/* end of [glu.cats] */
