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

staload _(*anonymous*) = "prelude/DATS/reference.dats"

val leftFirst = ref_make_elt<bool> (false)

(* ****** ****** *)

extern fun initialize (): void = "initialize"
implement initialize () = let
  val () = glEnable (GL_BLEND)
  val () = glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  val () = glShadeModel (GL_FLAT)
  val () = glClearColor (0.0, 0.0, 0.0, 0.0)
in
  // empty
end // end of [initialize]

(* ****** ****** *)

fn drawLeftTriangle () = let
// drawing a yellow triangle on LHS
  val (pf | ()) = glBegin (GL_TRIANGLES)
  val () = glColor4d (1.0, 1.0, 0.0, 0.75) // yellow
  val () = glVertex3d (0.1, 0.9, 0.0)
  val () = glVertex3d (0.1, 0.1, 0.0)
  val () = glVertex3d (0.7, 0.5, 0.0)
  val () = glEnd (pf | (*none*))
in
  // empty
end // end of [drawleftTriangle]

fn drawRightTriangle () = let
// drawing a cyan triangle on RHS
  val (pf | ()) = glBegin (GL_TRIANGLES)
  val () = glColor4d (0.0, 1.0, 1.0, 0.75) // cyan
  val () = glVertex3d (0.9, 0.9, 0.0)
  val () = glVertex3d (0.3, 0.5, 0.0)
  val () = glVertex3d (0.9, 0.1, 0.0)
  val () = glEnd (pf | (*none*))
in
  // empty
end // end of [drawRightTriangle]

(* ****** ****** *)

extern fun display (): void = "display"
implement display () = let
  val () = glClear (GL_COLOR_BUFFER_BIT)
  val () = case+ 0 of
    | _ when !leftFirst => begin
        drawLeftTriangle () ; drawRightTriangle ()
      end
    | _ => begin
        drawRightTriangle () ; drawLeftTriangle ()
      end
in
  glFlush ()
end // end of [display]

(* ****** ****** *)

extern fun reshape (w: int, h: int): void = "reshape"
implement reshape (w, h) = let
  val () = glViewport (0, 0, w, h)
  val () = glMatrixMode (GL_PROJECTION)
  val () = glLoadIdentity ()
  val () = case+ 0 of
    | _ when (w <= h) => let
        val hw = (double_of h) / (double_of w)
      in
        gluOrtho2D (0.0, 1.0, 0.0, 1.0 * hw)
      end
    | _ (* w > h *) => let
        val wh = (double_of w) / (double_of h)
      in
        gluOrtho2D (0.0, 1.0 * wh, 0.0, 1.0)
      end
  // end of [val]
in
  // empty
end // end of [reshape]

(* ****** ****** *)

extern fun char_of_uchar (c: uchar): char = "char_of_uchar"

%{^

ats_char_type char_of_uchar (ats_uchar_type c) { return c ; }

%}

extern fun keyboard
  (key: uchar, x: int, y: int): void = "keyboard"

implement keyboard (key, x, y) = let
  val key = char_of_uchar key
in
  case+ key of
  | _ when (key = 't' orelse key = 'T') => let
      val () = !leftFirst := ~(!leftFirst)
    in
      glutPostRedisplay ()
    end
  | _ when (key = '\033') => exit (0)
  | _ => ()
end // end of [keyboard]

(* ****** ****** *)

implement main_dummy () = ()

(* ****** ****** *)

%{$

ats_void_type mainats
  (ats_int_type argc, ats_ptr_type argv) {
  glutInit (&argc, argv) ;
  glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB | GLUT_DEPTH) ;
  glutInitWindowSize (500, 500) ;
  glutInitWindowPosition (100, 100) ;
  glutCreateWindow(((char**)argv)[0]) ;
  initialize () ;
  glutDisplayFunc (display) ;
  glutReshapeFunc (reshape) ;
  glutKeyboardFunc (keyboard) ;
  glutMainLoop () ;
} /* end of [mainats] */

%}

(* ****** ****** *)

(* end of [glBlending1.dats] *)
