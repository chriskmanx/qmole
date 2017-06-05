(*
**
** A simple OpenGL example
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Summer, 2008
**
*)

(* ****** ****** *)

%{^
extern ats_void_type mainats (ats_int_type argc, ats_ptr_type argv) ;
%}

(* ****** ****** *)

staload "contrib/GL/SATS/gl.sats"
staload "contrib/GL/SATS/glu.sats"
staload "contrib/GL/SATS/glut.sats"

(* ****** ****** *)

%{^

static int day = 0 ;

static inline
ats_int_type day_get () { return day ; }

static inline
ats_void_type day_set (int x) { day = x ; return ; }

%}

extern fun day_get (): int = "day_get"
extern fun day_set (x: int): void = "day_set"

(* ****** ****** *)

extern fun initialize (): void = "initialize"
implement initialize () = let
  val () = glClearColor (0.0, 0.0, 0.0, 0.0)
  val () = glShadeModel (GL_FLAT)
in
  // empty
end // end of [initialize]

(* ****** ****** *)

extern fun display (): void = "display"
implement display () = let
  val () = glClear (GL_COLOR_BUFFER_BIT)
  val () = glColor3d (1.0, 1.0, 1.0)
  val (pf1_mat | ()) = glPushMatrix ()
  val () = glRotated (270.0, 1.0, 0.0, 0.0)
  val () = glutWireSphere ((GLdouble)1.0, (GLint)20, (GLint)16) // sun

  val day = day_get ()
  val mday = day mod 30 and yday = day

  val m_angle = (double_of mday / 30) * 360.0
  and y_angle = (double_of yday / 365) * 360.0

  val () = glRotated (y_angle, 0.0, 0.0, 1.0)
  val () = glTranslated (2.0, 0.0, 0.0)

  val (pf2_mat | ()) = glPushMatrix ()
  val () = glRotated (15.0, 0.0, 1.0, 0.0)
  val () = glutWireSphere ((GLdouble)0.25, (GLint)10, (GLint)8) // planet

  val () = glRotated (m_angle, 0.0, 0.0, 1.0)
  val () = glTranslated (0.5, 0.0, 0.0)  
  val () = glutWireSphere ((GLdouble)0.10, (GLint)5, (GLint)4) // planet

  val () = glPopMatrix (pf2_mat | (*none*))

  val () = glPopMatrix (pf1_mat | (*none*))
  val () = glutSwapBuffers ()
in
  // empty
end // end of [display]

(* ****** ****** *)

extern fun reshape (w: int, h: int): void = "reshape"
implement reshape (w, h) = let
  val () = glViewport (0, 0, w, h)
  val () = glMatrixMode (GL_PROJECTION)
  val () = glLoadIdentity ()
  val () = gluPerspective
    (60.0, (double_of w) / (double_of h), 1.0, 20.0)
  val () = glMatrixMode (GL_MODELVIEW)
  val () = glLoadIdentity ()
  val () = gluLookAt (0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
in
  // empty
end // end of [reshape]

(* ****** ****** *)

implement main_dummy () = ()

(* ****** ****** *)

%{$

void keyboard (unsigned char key, int x, int y) {
  switch (key) {
    case 'd':
      day = (day + 5) % 365 ; glutPostRedisplay () ; break ;
    case 'D':
      day = (day - 5) % 365 ; glutPostRedisplay () ; break ;
    case '\033': exit (0) ;
    default: break ;
  }
  return ;
}

ats_void_type mainats
  (ats_int_type argc, ats_ptr_type argv) {

  glutInit ((int*)&argc, (char**)argv) ;
  glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB) ;
  glutInitWindowSize (500, 500) ;
  glutInitWindowPosition (100, 100) ;
  glutCreateWindow("Planet") ;
  initialize () ;
  glutDisplayFunc (display) ;
  glutReshapeFunc (reshape) ;
  glutKeyboardFunc (keyboard) ;
  glutMainLoop () ;
  return ; /* deadcode */
}

%}

(* ****** ****** *)

(* end of [glPlanet.dats] *)
