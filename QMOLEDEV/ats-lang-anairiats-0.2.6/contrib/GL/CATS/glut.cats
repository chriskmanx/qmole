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

#ifndef ATSTRIB_GL_GLUT_CATS
#define ATSTRIB_GL_GLUT_CATS

/* ****** ****** */

#include <GL/glut.h>

/* ****** ****** */

/*
** Initialization functions, see fglut_init.c
*/
#define atsctrb_glutInit glutInit
#define atsctrb_glutInitWindowPosition glutInitWindowPosition
#define atsctrb_glutInitWindowSize glutInitWindowSize
#define atsctrb_glutInitDisplayMode glutInitDisplayMode
#define atsctrb_glutInitDisplayString glutInitDisplayString

/* ****** ****** */

/*
** Process loop function, see freeglut_main.c
*/
#define atsctrb_glutMainLoop glutMainLoop

/* ****** ****** */

/*
** Window management functions, see freeglut_window.c
*/
#define atsctrb_glutCreateWindow glutCreateWindow
#define atsctrb_glutCreateSubWindow glutCreateSubWindow
#define atsctrb_glutDestroyWindow atsctrb_glutDestroyWindow
#define atsctrb_glutGetWindow glutGetWindow
#define atsctrb_glutSetWindow glutSetWindow
#define atsctrb_glutSetWindowTitle glutSetWindowTitle
#define atsctrb_glutSetIconTitle glutSetIconTitle
#define atsctrb_glutReshapeWindow glutReshapeWindow
#define atsctrb_glutPositionWindow glutPositionWindow
#define atsctrb_glutShowWindow glutShowWindow
#define atsctrb_glutHideWindow glutHideWindow
#define atsctrb_glutIconifyWindow glutIconifyWindow
#define atsctrb_glutPushWindow glutPushWindow
#define atsctrb_glutPopWindow glutPopWindow
#define atsctrb_glutFullScreen glutFullScreen

/* ****** ****** */

/*
** Display-connected functions, see freeglut_display.c
*/
#define atsctrb_glutPostWindowRedisplay glutPostWindowRedisplay
#define atsctrb_glutPostRedisplay glutPostRedisplay
#define atsctrb_glutSwapBuffers glutSwapBuffers

/* ****** ****** */

// Global callback functions, see freeglut_callbacks.c

#define atsctrb_glutTimerFunc glutTimerFunc

/* ****** ****** */

#define atsctrb_glutIdleFunc glutIdleFunc

static inline
ats_void_type
atsctrb_glutIdleFunc_null () {
  glutIdleFunc ((void (*)(void))0) ; return ;
} // end of [atsctrb_glutIdleFunc_null]

/* ****** ****** */

/*
** Window-specific callback functions, see freeglut_callbacks.c
*/
#define atsctrb_glutKeyboardFunc glutKeyboardFunc
#define atsctrb_glutMouseFunc glutMouseFunc
#define atsctrb_glutSpecialFunc glutSpecialFunc
#define atsctrb_glutReshapeFunc glutReshapeFunc
#define atsctrb_glutVisibilityFunc glutVisibilityFunc
#define atsctrb_glutDisplayFunc glutDisplayFunc
#define atsctrb_glutMotionFunc glutMotionFunc
#define atsctrb_glutPassiveMotionFunc glutPassiveMotionFunc
#define atsctrb_glutEntryFunc glutEntryFunc

/* ****** ****** */

#define atsctrb_glutGet glutGet
#define atsctrb_glutDeviceGet glutDeviceGet
#define atsctrb_glutGetModifiers glutGetModifiers
#define atsctrb_glutLayerGet glutLayerGet

/* ****** ****** */

#define atsctrb_glutBitmapCharacter glutBitmapCharacter
#define atsctrb_glutBitmapWidth glutBitmapWidth
#define atsctrb_glutBitmapLength glutBitmapLength

#define atsctrb_glutStrokeCharacter glutStrokeCharacter
#define atsctrb_glutStrokeWidth glutStrokeWidth
#define atsctrb_glutStrokeLength glutStrokeLength

/* ****** ****** */

#define atsctrb_glutWireCube glutWireCube
#define atsctrb_glutSolidCube glutSolidCube

#define atsctrb_glutWireSphere glutWireSphere
#define atsctrb_glutSolidSphere glutSolidSphere

/* ****** ****** */

#define atsctrb_glutWireCone glutWireCone
#define atsctrb_glutSolidCone glutSolidCone

/* ****** ****** */

#define atsctrb_glutWireTorus glutWireTorus
#define atsctrb_glutSolidTorus glutSolidTorus

/* ****** ****** */

#define atsctrb_glutWireTeapot glutWireTeapot
#define atsctrb_glutSolidTeapot glutSolidTeapot

/* ****** ****** */

#define atsctrb_glutWireDodecahedron glutWireDodecahedron
#define atsctrb_glutSolidDodecahedron glutSolidDodecahedron

#define atsctrb_glutWireOctahedron glutWireOctahedron
#define atsctrb_glutSolidOctahedron glutSolidOctahedron

#define atsctrb_glutWireTetrahedron glutWireTetrahedron
#define atsctrb_glutSolidTetrahedron glutSolidTetrahedron

#define atsctrb_glutWireIcosahedron glutWireIcosahedron ()
#define atsctrb_glutSolidIcosahedron glutSolidIcosahedron ()

/* ****** ****** */

#endif /* ATSTRIB_GL_GLUT_CATS */

/* end of [glut.cats] */
