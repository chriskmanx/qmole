(*
**
** A simple OpenGL example
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Summer, 2008
**
*)

//
// A simple example demonstrating the use of double buffering
//

(* ****** ****** *)

%{^
extern ats_void_type mainats (ats_int_type argc, ats_ptr_type argv) ;
%}

(* ****** ****** *)

staload "contrib/GL/SATS/gl.sats"
staload "contrib/GL/SATS/glut.sats"

(* ****** ****** *)

var spin: double = 0.0
val (pf_spin: vbox (double @ spin) | ()) =
  vbox_make_view_ptr (view@ spin | &spin)

extern fun initialize (): void = "initialize"
implement initialize () = let
  val () = glClearColor (0.0, 0.0, 0.0, 0.0)
  val () = glShadeModel (GL_FLAT)
in
  // empty
end // end of [initialize]

extern fun display (): void = "display"
implement display () = let
  val spin = let prval vbox (pf) = pf_spin in spin end
  val () = glClear (GL_COLOR_BUFFER_BIT)
  val (pf | ()) = glPushMatrix ()
  val () = glRotated (spin, 0.0, 0.0, 1.0)
  val () = glColor3d (1.0, 1.0, 1.0)
  val () = glRectd (~25.0, ~25.0, 25.0, 25.0)
  val () = glPopMatrix (pf | (*none*))
  val () = glutSwapBuffers ()
in
  // empty
end // end of [display]

extern fun spinDisplay (): void = "spinDisplay"
implement spinDisplay () = let
  var spin_v: double = begin
    let prval vbox (pf) = pf_spin in spin end
  end
  val () = spin_v := spin_v + 2.0
  val () = if spin_v > 360.0 then spin_v := spin_v - 360.0
  val () = begin
    let prval vbox (pf) = pf_spin in spin := spin_v end
  end
in
  glutPostRedisplay ()
end // end of [spinDisplay]

extern fun reshape (w: int, h: int): void = "reshape"
implement reshape (w, h) = let
  val () = glViewport (0, 0, w, h)
  val () = glMatrixMode (GL_PROJECTION)
  val () = glLoadIdentity ()
  val () = glOrtho (~50.0, 50.0, ~50.0, 50.0, ~1.0, 1.0)
  val () = glMatrixMode (GL_MODELVIEW)
  val () = glLoadIdentity ()
in
  // empty
end // end of [reshape]

(* ****** ****** *)

macdef int = int_of_GLenum

extern fun mouse
  (button: int, state: int, x: int, y: int): void = "mouse"
// end of [mouse]

implement mouse (button, state, x, y) = begin
  case+ 0 of
  | _ when (button = (int)GLUT_LEFT_BUTTON) => begin
      if (state = (int)GLUT_DOWN) then glutIdleFunc (spinDisplay)
    end
  | _ when (button = (int)GLUT_RIGHT_BUTTON) => begin
      if (state = (int)GLUT_DOWN) then glutIdleFunc_null ()
    end
  | _ => ()
end // end of [mouse]

extern fun main_work (): void = "main_work"
implement main_work () = let
  val () = initialize ()
  val () = glutDisplayFunc (display)
  val () = glutReshapeFunc (reshape)
  val () = glutMouseFunc (mouse)
in
  glutMainLoop ()
end // end of [main_work]

(* ****** ****** *)

implement main_dummy () = ()

(* ****** ****** *)

%{$

ats_void_type mainats
  (ats_int_type argc, ats_ptr_type argv) {
  glutInit ((int*)&argc, (char**)argv) ;
  glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB) ;
  glutInitWindowSize (500, 500) ;
  glutInitWindowPosition (100, 100) ;
  glutCreateWindow(((char**)argv)[0]) ;
  main_work () ;
  return ; /* deadcode as [main_work] never returns */
} /* end of [mainats] */

%}

(* ****** ****** *)

(* end of [glDoubleBuffer.dats] *)
