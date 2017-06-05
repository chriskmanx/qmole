(*
**
** A simple OpenGL example
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Summer, 2008
**
*)

(* ****** ****** *)

staload "libc/SATS/math.sats"
staload "libc/SATS/unistd.sats"

staload "contrib/GL/SATS/gl.sats"
staload "contrib/GL/SATS/glut.sats"

staload _(*anon*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

val frame_max = 8
val frame_cur_ref = ref_make_elt<int> (0)

val direction_ref = ref_make_elt<int> (1)

val iris_x_ofs_max = 1.5
val hair_move_angle1_max = M_PI / 3
val hair_move_angle2_max = M_PI / 2
val hair_move_angle3_max = 2 * M_PI / 3

val head_rotate_angle_max = 30.0

(* ****** ****** *)

fn draw_arc {n:int | n >= 1} (
    radius: double, ang_init: double, ang_delta: double, n: int n
  ) : void = let
  val theta = (ang_delta / n)
  fun loop {i:nat | i <= n}
    (i: int i, theta_i: double):<cloref1> void = let
    val () = glVertex3d
      (radius * cos (theta_i), radius * sin (theta_i), 0.0)
  in
    if i < n then loop (i + 1, theta_i + theta)
  end
  val (pf | ()) = glBegin (GL_LINE_STRIP)
  val () = loop (0, ang_init)
  val () = glEnd (pf | (*none*))
in
  // empty
end // end of [draw_arc]

(* ****** ****** *)

fn draw_polygon_eqlat
  {n:int | n >= 3} (knd: GLenum, radius: double, n: int n) = let
  val theta = (2 * M_PI) / n
  fun loop {i:nat | i <= n}
    (i: int i, theta_i: double):<cloref1> void =
    if i < n then let
      val () = glVertex3d
        (radius * cos (theta_i), radius * sin (theta_i), 0.0)
    in
      loop (i + 1, theta_i + theta)
    end
  val (pf | ()) = glBegin (knd)
  val () = loop (0, 0.0)
  val () = glEnd (pf | (*none*))
in
  // empty
end // end of [draw_polygon_eqlat]

(* ****** ****** *)

fn draw_eye (
    width: double
  , height: double
  , iris_x_ofs: double
  ) : void = let
  val r = height / 2
  val theta = atan2 (height, width)
  val theta2 = 2 * theta
  val theta4 = 2 * theta2
  val sin_theta = sin (theta)
  val dist = sqrt (height * height + width * width)
  val R2 = dist / (2 * sin_theta)
  val R = R2 / 2

  val (pf_push | ()) = glPushMatrix ()
  val () = glTranslated (0.0, R - r, 0.0)
  val () = draw_arc (R, 1.5 * M_PI - theta2, theta4, 16)
  val () = glPopMatrix (pf_push | (*none*))

  val (pf_push | ()) = glPushMatrix ()
  val () = glTranslated (0.0, r - R, 0.0)
  val () = draw_arc (R, 0.5 * M_PI - theta2, theta4, 16)
  val () = glPopMatrix (pf_push | (*none*))

  val (pf_push | ()) = glPushMatrix ()
  val () = glTranslated (iris_x_ofs, 0.0, 0.0)
  val () = glColor3d (0.0, 0.0, 0.0)
  val () = draw_polygon_eqlat (GL_POLYGON, r / 1.25, 16)

  val () = glColor3d (1.0, 1.0, 1.0)
  val () = glTranslated (iris_x_ofs / 2, 0.0, 0.0)
  val () = draw_polygon_eqlat (GL_POLYGON, r / 8, 16)
  val () = glColor3d (0.0, 0.0, 0.0)
  val () = glPopMatrix (pf_push | (*none*))
in
  // empty
end // end of [draw_eye]

(* ****** ****** *)

fn draw_face
  (face_radius: double, anim_ratio: double, sgn: int): void = let
  val hair_length = 4.0
  val hair_angle = M_PI / 3
  val mouth_angle = 0.425 * M_PI

  val (pf_push | ()) = glPushMatrix ()
  val () = glScaled (1.0625, 1.3125, 1.0)

  val () = begin
    if sgn > 0 then begin
      glColor3d (anim_ratio, 0.5, 1.0 - anim_ratio)
    end else begin
      glColor3d (1.0 - anim_ratio, 0.5, anim_ratio)
    end
  end // end of [val]

  val () = draw_polygon_eqlat (GL_POLYGON, face_radius, 64)
  val () = glColor3d (0.0, 0.0, 0.0)
  val (pf_beg | ()) = glBegin (GL_LINES)

  val x_hair = face_radius * cos (hair_angle)
  val y_hair = face_radius * sin (hair_angle)

  val (pf_push1 | ()) = glPushMatrix ()
  val ang1 = (
    if sgn > 0 then
      anim_ratio * hair_move_angle3_max
    else
      anim_ratio * hair_move_angle1_max
  ) : double
  val ang2 = anim_ratio * hair_move_angle2_max
  val ang3 = (
    if sgn > 0 then
      anim_ratio * hair_move_angle1_max
    else
      anim_ratio * hair_move_angle3_max
  ) : double
  val x_delta1 = sgn * hair_length * sin (ang1)
  val y_delta1 = hair_length * cos (ang1)
  val x_delta2 = sgn * hair_length * sin (ang2)
  val y_delta2 = hair_length * cos (ang2)
  val x_delta3 = sgn * hair_length * sin (ang3)
  val y_delta3 = hair_length * cos (ang3)
  val () = glVertex3d (~x_hair, y_hair, 0.0)
  val () = glVertex3d (~x_hair - x_delta1, y_hair + y_delta1, 0.0)
  val () = glVertex3d (0.0, face_radius, 0.0)
  val () = glVertex3d (~x_delta2, face_radius + y_delta2, 0.0)
  val () = glVertex3d (x_hair, y_hair, 0.0)
  val () = glVertex3d (x_hair - x_delta3, y_hair + y_delta3, 0.0)  
  val () = glPopMatrix (pf_push1 | (*none*))
  val () = glEnd (pf_beg | (*none*))
  val () = glPopMatrix (pf_push | (*none*))

  // eye drawing
  val iris_x_ofs = sgn * (anim_ratio * iris_x_ofs_max)
  val (pf_push | ()) = glPushMatrix ()
  val () = glTranslated (~4.0, 4.0, 0.0)
  val () = draw_eye (6.0, 3.0, iris_x_ofs)
  val () = glPopMatrix (pf_push | (*none*))

  val (pf_push | ()) = glPushMatrix ()
  val () = glTranslated (4.0, 4.0, 0.0)
  val () = draw_eye (6.0, 3.0, iris_x_ofs)
  val () = glPopMatrix (pf_push | (*none*))

  // mouth drawing
  val (pf_push | ()) = glPushMatrix ()
  val () =
    if sgn > 0 then let
      val () = glTranslated (0.0, ~4.0, 0.0)
      val () = draw_arc (2.5, 1.5 * M_PI - mouth_angle, 2 * mouth_angle, 32)
    in
      // empty
    end else let
      val () = glTranslated (0.0, ~7.0, 0.0)
      val () = draw_arc (2.5, 0.5 * M_PI - mouth_angle, 2 * mouth_angle, 32)
    in
      // empty
    end // end of [if]
  val () = glPopMatrix (pf_push | (*none*))
in
  // empty
end // end of [draw_face]

(* ****** ****** *)

local

fn glClearColor_black
  (): void = glClearColor (0.0, 0.0, 0.0, 0.0)
fn glClearColor_white
  (): void = glClearColor (1.0, 1.0, 1.0, 0.0)

in // in of [local]

extern
fun initialize
  (): void = "initialize"
// end of [initialize]
implement initialize () = let
  val () = glClearColor_white ()
  val () = glShadeModel (GL_FLAT)
in
  // empty
end // end of [initialize]

end // end of [local]

(* ****** ****** *)
//
extern
fun animate (): void = "animate"
//
// HX: this is a very crude way of doing animation!
//
implement animate () = let
  val frame = !frame_cur_ref
  val frame_new = begin
    if frame < frame_max then frame + 1 else 0
  end : int // end of [val]
  val () = if frame_new = 0 then let
    val dir = !direction_ref in !direction_ref := ~dir
  end // end of [val]
  val () = !frame_cur_ref := frame_new
  val () = usleep (100000) // one-tenth of a second
in
  glutPostRedisplay ()
end // end of [animate]

(* ****** ****** *)

extern
fun display
  (): void = "display"
// end of [display]
implement display () = let
  val face_radius = 10.0
  val frame = !frame_cur_ref
  val anim_ratio = (double_of frame) / frame_max
  val dir = !direction_ref

  val () = glClear (GL_COLOR_BUFFER_BIT)
  val () = glLineWidth (2.5)
  val () = glColor3d (0.0, 0.0, 0.0)

  val head_rotate_angle = anim_ratio * head_rotate_angle_max
  val x_ratio = cos (head_rotate_angle * M_PI / 180)

  val (pf | ()) = glPushMatrix ()
(*
  val () =
    if dir > 0 then begin
      glTranslatef (~15.0 * x_ratio, 0.0, 0.0)
    end else begin
      glTranslatef (~30.0 + 15.0 * x_ratio , 0.0, 0.0)
    end // end of [if]
*)
  val () = glTranslated (~20.0, 0.0, 0.0)
(*
  val () =
    if dir > 0 then begin
      glRotatef (~head_rotate_angle, 0.0, 0.0, 1.0)
    end else begin
      glRotatef (head_rotate_angle, 0.0, 0.0, 1.0)
    end
*)
  val () = draw_face (face_radius, anim_ratio, 1)
  val () = glPopMatrix (pf | (*none*))

  val (pf | ()) = glPushMatrix ()
(*
  val () =
    if dir > 0 then begin
      glTranslatef (15.0 * x_ratio, 0.0, 0.0)
    end else begin
      glTranslatef (30.0 - 15.0 * x_ratio, 0.0, 0.0)
    end
*)
  val () = glTranslated (20.0, 0.0, 0.0)
(*
  val () =
    if dir > 0 then begin
      glRotatef (head_rotate_angle, 0.0, 0.0, 1.0)
    end else begin
      glRotatef (~head_rotate_angle, 0.0, 0.0, 1.0)
    end
*)
  val () = draw_face (face_radius, anim_ratio, ~1)
  val () = glPopMatrix (pf | (*none*))
in
  glutSwapBuffers ()
end // end of [display]

(* ****** ****** *)

extern
fun reshape
  (w: int, h: int): void = "reshape"
// end of [reshape]

implement
reshape (w, h) = let
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

extern
fun keyboard
  (key: uchar, x: int, y: int): void = "keyboard"
// end of [keyboard]

implement
keyboard (key, x, y) = let
  val key = char_of_uchar (key)
in
  case+ 0 of
  | _ when (key = '\033') => exit (0) | _ => ()
end // end of [keyboard]

(* ****** ****** *)

macdef int = int_of_GLenum

val animation_status_ref = ref_make_elt<int> (0)

extern fun mouse
  (button: int, state: int, x: int, y: int): void = "mouse"
// end of [mouse]

implement
mouse (button, state, x, y) = begin case+ 0 of
  | _ when (button = (int)GLUT_LEFT_BUTTON) => begin
      if (state = (int)GLUT_DOWN) then let
        val status = !animation_status_ref
        val () = !animation_status_ref := 1 - status
      in
        if status = 0 then glutIdleFunc (animate) else glutIdleFunc_null ()
      end // end of [if]
    end // end of [_ when ...]
  | _ => ()
end // end of [mouse]

(* ****** ****** *)

%{^
extern
ats_void_type mainats (ats_int_type argc, ats_ptr_type argv) ;
%} // end of [%{^]
implement main_dummy () = ()

(* ****** ****** *)

%{$

ats_void_type
mainats (
  ats_int_type argc, ats_ptr_type argv
) {
  glutInit ((int*)&argc, (char**)argv) ;
  glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB) ;
  glutInitWindowSize (500, 500) ;
  glutInitWindowPosition (100, 100) ;
  glutCreateWindow(((char**)argv)[0]) ;
  initialize () ;
  glutDisplayFunc (display) ;
  glutReshapeFunc (reshape) ;
  glutKeyboardFunc (keyboard) ;
  glutMouseFunc (mouse) ;
  glutMainLoop () ;
  return ; /* deadcode */
} /* end of [mainats] */

%} // end of[%{$]

(* ****** ****** *)

(* end of [glFaces.dats] *)
