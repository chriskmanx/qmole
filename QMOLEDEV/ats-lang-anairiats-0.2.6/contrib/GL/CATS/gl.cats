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
//
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: December, 2009
//
/* ****** ****** */

#ifndef ATSCTRB_GL_GL_CATS
#define ATSCTRB_GL_GL_CATS

/* ****** ****** */

#include <stdio.h>

/* ****** ****** */

#include <GL/gl.h>

/* ****** ****** */

typedef GLboolean ats_GLboolean_type ;

typedef GLenum ats_GLenum_type ;
typedef GLbitfield ats_GLbitfield_type ;

typedef GLdouble ats_GLdouble_type ;
typedef GLfloat ats_GLfloat_type ;
typedef GLint ats_GLint_type ;
typedef GLshort ats_GLshort_type ;

typedef GLbyte ats_GLbyte_type ;
typedef GLubyte ats_GLubyte_type ;
typedef GLuint ats_GLuint_type ;
typedef GLushort ats_GLushort_type ;

typedef GLsizei ats_GLsizei_type ;

typedef GLclampd ats_GLclampd_type ;
typedef GLclampf ats_GLclampf_type ;

/* ****** ****** */

ATSinline()
ats_int_type
atsctrb_int_of_GLenum (GLenum x) { return x ; }

//

ATSinline()
ats_bool_type
atsctrb_eq_GLenum_GLenum
  (GLenum x1, GLenum x2) {
  return (x1 == x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_eq_GLenum_GLenum]

ATSinline()
ats_bool_type
atsctrb_neq_GLenum_GLenum
  (GLenum x1, GLenum x2) {
  return (x1 != x2 ? ats_true_bool : ats_false_bool) ;
} // end of [atsctrb_neq_GLenum_GLenum]

//

ATSinline()
ats_GLbyte_type
atsctrb_GLbyte_of_int (int x) { return x ; }

//

ATSinline()
ats_GLubyte_type
atsctrb_GLubyte_of_int (int x) { return x ; }

ATSinline()
ats_GLubyte_type
atsctrb_GLubyte_of_uint (unsigned int x) { return x ; }

//

ATSinline()
ats_GLshort_type
atsctrb_GLshort_of_int (int x) { return x ; }

//

ATSinline()
ats_GLushort_type
atsctrb_GLushort_of_int (int x) { return x ; }

ATSinline()
ats_GLushort_type
atsctrb_GLushort_of_uint (unsigned int x) { return x ; }

//

ATSinline()
ats_GLsizei_type
atsctrb_GLsizei_of_int (int x) { return x ; }

//

ATSinline()
ats_GLdouble_type
atsctrb_GLdouble_of_int (int x) { return x ; }

//

ATSinline()
ats_GLfloat_type
atsctrb_GLfloat_of_int (int x) { return x ; }

ATSinline()
ats_GLfloat_type
atsctrb_GLfloat_of_double (double x) { return x ; }

ATSinline()
ats_GLclampf_type
atsctrb_GLclampf_of_double (double x) { return x ; }

//

ATSinline()
ats_GLbitfield_type
atsctrb_lor_GLbitfield_GLbitfield
  (GLbitfield b1, GLbitfield b2) { return (b1 | b2) ; }
// end of [atsctrb_lor_GLbitfield_GLbitfield]

/* ****** ****** */

//
// Miscellaneous functions
//

/* ****** ****** */

#define atsctrb_glClearIndex glClearIndex
#define atsctrb_glClearColor glClearColor

/* ****** ****** */

#define atsctrb_glClear glClear
#define atsctrb_glIndexMask glIndexMask
#define atsctrb_glColorMask glColorMask

/* ****** ****** */

#define atsctrb_glAlphaFunc glAlphaFunc
#define atsctrb_glBlendFunc glBlendFunc
#define atsctrb_glLogicOp glLogicOp

/* ****** ****** */

#define atsctrb_glCullFace glCullFace
#define atsctrb_glFrontFace glFrontFace

/* ****** ****** */

#define atsctrb_glPointSize glPointSize

/* ****** ****** */

#define atsctrb_glLineWidth glLineWidth

/* ****** ****** */

#define atsctrb_glLineStipple glLineStipple

/* ****** ****** */

#define atsctrb_glPolygonMode glPolygonMode
#define atsctrb_glPolygonOffset glPolygonOffset
#define atsctrb_glPolygonStipple glPolygonStipple
#define atsctrb_glGetPolygonStipple glGetPolygonStipple
#define atsctrb_glEdgeFlag glEdgeFlag
#define atsctrb_glEdgeFlagv glEdgeFlagv
#define atsctrb_glScissor glScissor

/* ****** ****** */

#define atsctrb_glClipPlane glClipPlane
#define atsctrb_glGetClipPlane glGetClipPlane

/* ****** ****** */

#define atsctrb_glDrawBuffer glDrawBuffer
#define atsctrb_glReadBuffer glReadBuffer

/* ****** ****** */

#define atsctrb_glEnable glEnable
#define atsctrb_glDisable glDisable
#define atsctrb_glIsEnabled glIsEnabled

/*
// OpenGL 1.1
#define atsctrb_glEnableClientState glEnableClientState
#define atsctrb_glDisableClientState glDisableClientState
*/

/* ****** ****** */

#define atsctrb_glGetBooleanv glGetBooleanv
#define atsctrb_glGetDoublev glGetDoublev
#define atsctrb_glGetFloatv glGetFloatv
#define atsctrb_glGetIntegerv glGetIntegerv

/* ****** ****** */

#define atsctrb_glPushAttrib glPushAttrib
#define atsctrb_glPopAttrib glPopAttrib

/* ****** ****** */

#define atsctrb_glRenderMode glRenderMode
#define atsctrb_glGetError glGetError
#define atsctrb_glGetString glGetString
#define atsctrb_glFinish glFinish
#define atsctrb_glFlush glFlush
#define atsctrb_glHint glHint

/* ****** ****** */

// Depth Buffer

#define atsctrb_glClearDepth glClearDepth
#define atsctrb_glDepthFunc glDepthFunc
#define atsctrb_glDepthMask glDepthMask
#define atsctrb_glDepthRange glDepthRange

/* ****** ****** */

// Accumulation Buffer

#define atsctrb_glClearAccum glClearAccum
#define atsctrb_glAccum glAccum

/* ****** ****** */

//
// Transformation
//

/* ****** ****** */

#define atsctrb_glMatrixMode glMatrixMode

/* ****** ****** */

#define atsctrb_glOrtho glOrtho

/* ****** ****** */

#define atsctrb_glFrustum glFrustum

/* ****** ****** */

#define atsctrb_glViewport glViewport

/* ****** ****** */

#define atsctrb_glPopMatrix glPopMatrix
#define atsctrb_glPushMatrix glPushMatrix

/* ****** ****** */

#define atsctrb_glLoadIdentity glLoadIdentity
#define atsctrb_glLoadMatrixd glLoadMatrixd
#define atsctrb_glLoadMatrixf glLoadMatrixf

/* ****** ****** */

#define atsctrb_glMultMatrixd glMultMatrixd
#define atsctrb_glMultMatrixf glMultMatrixf

/* ****** ****** */

#define atsctrb_glRotated glRotated
#define atsctrb_glRotatef glRotatef

#define atsctrb_glScaled glScaled
#define atsctrb_glScalef glScalef

#define atsctrb_glTranslated glTranslated
#define atsctrb_glTranslatef glTranslatef

/* ****** ****** */

//
// Display Lists
//

#define atsctrb_glIsList glIsList

ATSinline()
GLuint
atsctrb_glGenList () { return glGenLists (1) ; }

ATSinline()
GLuint
atsctrb_glGenList_exn () {
  GLuint lst = glGenLists (1) ;
  if (lst == 0) {
    fprintf (stderr, "exit(ATS/GL): [glGenLists] failed.\n") ; exit (1) ;
  } // end of [if]
  return lst ;
} // end of [atsctrb_glGenList_exn]

#define atsctrb_glGenLists glGenLists

ATSinline()
GLuint
atsctrb_glNewList_new
  (GLuint lst, GLenum mode) {
  glNewList (lst, mode) ; return lst ;
} // end of [atsctrb_glNewList_new]
ATSinline()
ats_void_type
atsctrb_glNewList_clear
  (GLuint lst, GLenum mode) { glNewList (lst, mode) ; return ; }
// end of [atsctrb_glNewList_clear]

ATSinline()
ats_void_type
atsctrb_glDeleteList
  (GLuint lst) { glDeleteLists (lst, 1) ; return ; }
// end of [atsctrb_glDeleteList]

#define atsctrb_glEndList glEndList
#define atsctrb_glCallList glCallList
#define atsctrb_glListBase glListBase

/* ****** ****** */

#define atsctrb_glBegin glBegin
#define atsctrb_glEnd glEnd

/* ****** ****** */

//
// Drawing functions
//

#define atsctrb_glVertex2d glVertex2d
#define atsctrb_glVertex2f glVertex2f
#define atsctrb_glVertex2i glVertex2i
#define atsctrb_glVertex2s glVertex2s

#define atsctrb_glVertex2dv glVertex2dv
#define atsctrb_glVertex2fv glVertex2fv
#define atsctrb_glVertex2iv glVertex2iv
#define atsctrb_glVertex2sv glVertex2sv

/* ****** ****** */

#define atsctrb_glVertex3d glVertex3d
#define atsctrb_glVertex3f glVertex3f
#define atsctrb_glVertex3i glVertex3i
#define atsctrb_glVertex3s glVertex3s

#define atsctrb_glVertex3dv glVertex3dv
#define atsctrb_glVertex3fv glVertex3fv
#define atsctrb_glVertex3iv glVertex3iv
#define atsctrb_glVertex3sv glVertex3sv

/* ****** ****** */

#define atsctrb_glVertex4d glVertex4d
#define atsctrb_glVertex4f glVertex4f
#define atsctrb_glVertex4i glVertex4i
#define atsctrb_glVertex4s glVertex4s

#define atsctrb_glVertex4dv glVertex4dv
#define atsctrb_glVertex4fv glVertex4fv
#define atsctrb_glVertex4iv glVertex4iv
#define atsctrb_glVertex4sv glVertex4sv

/* ****** ****** */

#define atsctrb_glNormal3b glNormal3b
#define atsctrb_glNormal3d glNormal3d
#define atsctrb_glNormal3f glNormal3f
#define atsctrb_glNormal3i glNormal3i
#define atsctrb_glNormal3s glNormal3s

#define atsctrb_glNormal3bv glNormal3bv
#define atsctrb_glNormal3dv glNormal3dv
#define atsctrb_glNormal3fv glNormal3fv
#define atsctrb_glNormal3iv glNormal3iv
#define atsctrb_glNormal3sv glNormal3sv

/* ****** ****** */

#define atsctrb_glIndexd glIndexd
#define atsctrb_glIndexf glIndexf
#define atsctrb_glIndexi glIndexi
#define atsctrb_glIndexs glIndexs
#define atsctrb_glIndexub glIndexub

/* ****** ****** */

#define atsctrb_glColor3b glColor3b
#define atsctrb_glColor3d glColor3d
#define atsctrb_glColor3f glColor3f
#define atsctrb_glColor3i glColor3i
#define atsctrb_glColor3s glColor3s
#define atsctrb_glColor3ub glColor3ub
#define atsctrb_glColor3ui glColor3ui
#define atsctrb_glColor3us glColor3us

#define atsctrb_glColor3bv glColor3bv
#define atsctrb_glColor3dv glColor3dv
#define atsctrb_glColor3fv glColor3fv
#define atsctrb_glColor3iv glColor3iv
#define atsctrb_glColor3sv glColor3sv
#define atsctrb_glColor3ubv glColor3ubv
#define atsctrb_glColor3uiv glColor3uiv
#define atsctrb_glColor3usv glColor3usv


/* ****** ****** */

#define atsctrb_glColor4b glColor4b
#define atsctrb_glColor4d glColor4d
#define atsctrb_glColor4f glColor4f
#define atsctrb_glColor4i glColor4i
#define atsctrb_glColor4s glColor4s
#define atsctrb_glColor4ub glColor4ub
#define atsctrb_glColor4ui glColor4ui
#define atsctrb_glColor4us glColor4us

#define atsctrb_glColor4bv glColor4bv
#define atsctrb_glColor4dv glColor4dv
#define atsctrb_glColor4fv glColor4fv
#define atsctrb_glColor4iv glColor4iv
#define atsctrb_glColor4sv glColor4sv
#define atsctrb_glColor4ubv glColor4ubv
#define atsctrb_glColor4uiv glColor4uiv
#define atsctrb_glColor4usv glColor4usv

/* ****** ****** */

#define atsctrb_glTexCoord1d glTexCoord1d
#define atsctrb_glTexCoord1f glTexCoord1f
#define atsctrb_glTexCoord1i glTexCoord1i
#define atsctrb_glTexCoord1s glTexCoord1s

#define atsctrb_glTexCoord2d glTexCoord2d
#define atsctrb_glTexCoord2f glTexCoord2f
#define atsctrb_glTexCoord2i glTexCoord2i
#define atsctrb_glTexCoord2s glTexCoord2s

#define atsctrb_glTexCoord3d glTexCoord3d
#define atsctrb_glTexCoord3f glTexCoord3f
#define atsctrb_glTexCoord3i glTexCoord3i
#define atsctrb_glTexCoord3s glTexCoord3s

/* ****** ****** */

#define atsctrb_glRasterPos2d glRasterPos2d
#define atsctrb_glRasterPos2f glRasterPos2f
#define atsctrb_glRasterPos2i glRasterPos2i
#define atsctrb_glRasterPos2s glRasterPos2s

#define atsctrb_glRasterPos3d glRasterPos3d
#define atsctrb_glRasterPos3f glRasterPos3f
#define atsctrb_glRasterPos3i glRasterPos3i
#define atsctrb_glRasterPos3s glRasterPos3s

#define atsctrb_glRasterPos4d glRasterPos4d
#define atsctrb_glRasterPos4f glRasterPos4f
#define atsctrb_glRasterPos4i glRasterPos4i
#define atsctrb_glRasterPos4s glRasterPos4s

/* ****** ****** */

#define atsctrb_glRectd glRectd
#define atsctrb_glRectf glRectf

/* ****** ****** */

//
// Lighting
//

/* ****** ****** */

#define atsctrb_glShadeModel glShadeModel

#define atsctrb_glLightf glLightf
#define atsctrb_glLighti glLighti

#define atsctrb_glLightfv glLightfv
#define atsctrb_glLightiv glLightiv

#define atsctrb_glGetLightfv glGetLightfv
#define atsctrb_glGetLightiv glGetLightiv

/* ****** ****** */

#define atsctrb_glLightModelf glLightModelf
#define atsctrb_glLightModeli glLightModeli

#define atsctrb_glLightModelfv glLightModelfv
#define atsctrb_glLightModeliv glLightModeliv

/* ****** ****** */

#define atsctrb_glMaterialf glMaterialf
#define atsctrb_glMateriali glMateriali

#define atsctrb_glMaterialfv glMaterialfv
#define atsctrb_glMaterialiv glMaterialiv

#define atsctrb_glGetMaterialfv glGetMaterialfv
#define atsctrb_glGetMaterialiv glGetMaterialiv

#define atsctrb_glColorMaterial glColorMaterial

/* ****** ****** */

//
// Raster functions
//

#define atsctrb_glPixelZoom glPixelZoom

#define atsctrb_glPixelStoref glPixelStoref
#define atsctrb_glPixelStorei glPixelStorei

#define atsctrb_glBitmap glBitmap

#define atsctrb_glReadPixels glReadPixels
#define atsctrb_glDrawPixels glDrawPixels
#define atsctrb_glCopyPixels glCopyPixels

/* ****** ****** */

//
// Texture mapping
//

#define atsctrb_glTexParameterf glTexParameterf
#define atsctrb_glTexParameteri glTexParameteri

#define atsctrb_glTexEnvf glTexEnvf
#define atsctrb_glTexEnvi glTexEnvi

/* ****** ****** */

#define atsctrb_glTexImage1D glTexImage1D
#define atsctrb_glTexImage2D glTexImage2D

/* ****** ****** */

//
// OpenGL 1.1
//

ATSinline()
ats_void_type
atsctrb_glGenTexture
  (ats_ref_type texture) {
  glGenTextures(1, (GLuint*)texture) ; return ;
} // end of [atsctrb_glGenTexture]

#define atsctrb_glGenTextures glGenTextures

ATSinline()
ats_void_type
atsctrb_glDeleteTexture
  (ats_GLuint_type texture) {
  glDeleteTextures(1, (GLuint*)&texture) ; return ;
} // end of [atsctrb_glDeleteTexture]

#define atsctrb_glDeleteTextures glDeleteTextures

#define atsctrb_glBindTexture glBindTexture

/* ****** ****** */

#define atsctrb_glFogf glFogf
#define atsctrb_glFogi glFogi
#define atsctrb_glFogfv glFogfv
#define atsctrb_glFogiv glFogiv

/* ****** ****** */

#endif /* ATSCTRB_GL_GL_CATS */

/* end of [gl.cats] */
