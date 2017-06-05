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

extern fun initialize (): void = "initialize"
implement initialize () = let
  val () = glClearColor (0.0, 0.0, 0.0, 0.0)
  val () = glShadeModel (GL_FLAT)
in
  // empty
end // end of [initialize]

extern fun display (): void = "display"
implement display () = let
  var !p_eqn0 with pf_eqn0 = @[double](1.0, 0.0, 0.0, 0.0)
  var !p_eqn1 with pf_eqn1 = @[double](0.0, 1.0, 0.0, 0.0)
  val () = glClear (GL_COLOR_BUFFER_BIT)
  val () = glColor3d (1.0, 1.0, 1.0)
  val (pf_mat | ()) = glPushMatrix ()
  val () = glTranslated (0.0, 0.0, ~5.0)
//
  extern fun glClipPlane {l:addr}
    (pf: !array_v (double, 4, l) | plane: GLenum, eqn: ptr l): void
    = "mac#atsctrb_glClipPlane"
//
  val () = glRotated (315.0, 1.0, 0.0, 0.0)
  val () = glRotated (45.0, 0.0, 1.0, 0.0)
  val () = glClipPlane (pf_eqn0 | GL_CLIP_PLANE0, p_eqn0)
  val () = glEnable (GL_CLIP_PLANE0)
  val () = glClipPlane (pf_eqn1 | GL_CLIP_PLANE1, p_eqn1)
  val () = glEnable (GL_CLIP_PLANE1)

  val () = glRotated (90.0, 1.0, 0.0, 0.0)
  val () = glutWireSphere ((GLdouble)1.0, (GLint)40, (GLint)32)
  val () = glPopMatrix (pf_mat | (*none*))

  val () = glFlush ()
in
  // empty
end // end of [display]

(* ****** ****** *)

extern
fun reshape (w: int, h: int): void = "reshape"
implement reshape (w, h) = let
  val () = glViewport (0, 0, w, h)
  val () = glMatrixMode (GL_PROJECTION)
  val () = glLoadIdentity ()
  val () = gluPerspective
    (40.0, (double_of w) / (double_of h), 1.0, 20.0)
  val () = glMatrixMode (GL_MODELVIEW)
in
  // empty
end // end of [reshape]

(* ****** ****** *)

extern fun main_work (): void = "main_work"
implement main_work () = let
  val () = initialize ()
  val () = glutDisplayFunc (display)
  val () = glutReshapeFunc (reshape)
in
  glutMainLoop ()
end // end of [main_work]

(* ****** ****** *)

implement main_dummy () = ()

%{$

ats_void_type mainats
  (ats_int_type argc, ats_ptr_type argv) {

  glutInit ((int*)&argc, (char**)argv) ;
  glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB | GLUT_DEPTH) ;
  glutInitWindowSize (500, 500) ;
  glutInitWindowPosition (100, 100) ;
  glutCreateWindow(((char**)argv)[0]) ;
  main_work () ;
  return ; /* deadcode */
}

%}

(* ****** ****** *)

(* end of [glClipping.dats] *)
