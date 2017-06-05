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

staload "libc/SATS/math.sats"
staload "contrib/GL/SATS/gl.sats"
staload "contrib/GL/SATS/glu.sats"
staload "contrib/GL/SATS/glut.sats"

(* ****** ****** *)

extern fun drawTorus
  {c,t:nat} (numc: int c, numt: int t): void = "drawTorus"

implement drawTorus {c,t} (numc, numt) = let
  val PI2 = M_PI + M_PI
  val angc = PI2 / numc
  val angt = PI2 / numt
  fun loop3
    {i,j,k:nat | i < c; j <= t; k <= 1}
    .<k+1>. (i: int i, j: int j, k: int k):<cloref1> void = let
    val c = double_of (mod_int_int (i + k, numc)) + 0.5
    val t = double_of (mod_int_int (j, numt))
    val cangc = c * angc and tangt = t * angt
    val cos_cangc = cos (cangc)
    val cos_tangt = cos (tangt)
    val x = (1.0 + 0.1 * cos_cangc) * cos_tangt
    val sin_cangc = sin (cangc)
    val sin_tangt = sin (tangt)
    val y = (1.0 + 0.1 * cos_cangc) * sin_tangt
    val z = 0.1 * sin_cangc
    val () = glVertex3d (x, y, z)
  in
    if k > 0 then loop3 (i, j, k-1) else ()
  end

  fun loop2 {i,j:nat | i < c; j <= t}
    .<t-j>. (i: int i, j: int j):<cloref1> void = let
      val () = loop3 (i, j, 1)
    in
      if j < numt then loop2 (i, j+1)
    end // end of [if]

  fun loop1 {i:nat | i <= c}
    .<c-i>. (i: int i):<cloref1> void =
    if i < numc then let
      val (pf | ()) = glBegin (GL_QUAD_STRIP)
      val () = loop2 (i, 0)
      val () = glEnd (pf | (*none*))
    in
      loop1 (i+1)
    end // end of [if]

in
  loop1 (0)
end // end of [drawTorus]

(* ****** ****** *)

#include "glList.dats"

(* ****** ****** *)

val torusLst_ref = glListRef_make ()

extern fun initialize (): void = "initialize"
implement initialize () = let
  val (pf_torusLst | torusLst) = glGenList_exn ()
  val (pf_list | torusLst) = glNewList (pf_torusLst | torusLst, GL_COMPILE)
  val () = drawTorus (8, 25)
  val () = glEndList (pf_list | (*none*))
  val () = glListRef_set (torusLst_ref, torusLst)
  val () = glShadeModel (GL_FLAT)
  val () = glClearColor (0.0, 0.0, 0.0, 0.0)
in
  // empty
end // end of [initialize]

(* ****** ****** *)

extern fun display (): void = "display"
implement display () = let
  val () = glClear (GL_COLOR_BUFFER_BIT)
  val () = glColor3d (1.0, 1.0, 1.0)
  val () = glCallListRef (torusLst_ref)
in
  glFlush ()
end // end of [display]

(* ****** ****** *)

extern fun reshape (w: int, h: int): void = "reshape"
implement reshape (w, h) = let
  val () = glViewport (0, 0, w, h)
  val () = glMatrixMode (GL_PROJECTION)
  val () = glLoadIdentity ()
  val wh = (double_of w) / (double_of h)
  val () = gluPerspective (30.0, wh, 1.0, 100.0)
  val () = glMatrixMode (GL_MODELVIEW)
  val () = glLoadIdentity ()
  val () = gluLookAt (0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
in
end // end of [reshape]

(* ****** ****** *)
 
%{^

ats_char_type char_of_uchar (ats_uchar_type c) { return c ; }

%}

extern fun char_of_uchar (c: uchar): char = "char_of_uchar"

extern fun keyboard
  (key: uchar, x: int, y: int): void = "keyboard"

implement keyboard (key, x, y) = let
  val key = char_of_uchar (key)
in
  case+ 0 of
  | _ when (key = 'x' orelse key = 'X') => begin
      glRotated (30.0, 1.0, 0.0, 0.0); glutPostRedisplay ()
    end
  | _ when (key = 'y' orelse key = 'Y') => begin
      glRotated (30.0, 0.0, 1.0, 0.0); glutPostRedisplay ()
    end
  | _ when (key = 'i' orelse key = 'I') => begin
      glLoadIdentity (); gluLookAt (0.0, 0.0, 10.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
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
  glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB) ;
  glutInitWindowSize (500, 500) ;
  glutInitWindowPosition (100, 100) ;
  glutCreateWindow(((char**)argv)[0]) ;
  initialize () ;
  glutReshapeFunc (reshape) ;
  glutKeyboardFunc (keyboard) ;
  glutDisplayFunc (display) ;
  glutMainLoop () ;
} /* end of [mainats] */

%}

(* ****** ****** *)

(* end of [glDisplayList1.dats] *)

