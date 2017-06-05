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
staload "contrib/GL/SATS/glut.sats"

staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

(*
extern fun initialize (): void = "initialize"
implement initialize () = let
  val () = glClearColor (0.0, 0.0, 0.0, 0.0)
  val () = glMatrixMode (GL_PROJECTION)
  val () = glLoadIdentity ()
  val () = glOrtho (~1.0, 1.0, ~1.0, 1.0, ~1.0, 1.0)
in
  // empty
end // end of [initialize]
*)

(* ****** ****** *)

viewdef float_3_v (l:addr) = array_v (float, 3, l)

fn normalize_float_3 {l:addr}
  (pf: !float_3_v l | p: ptr l): void = let
  val x0 = !p.[0] and x1 = !p.[1] and x2 = !p.[2]
  val mag = sqrtf (x0 * x0 + x1 * x1 + x2 * x2)
in
  !p.[0] := x0 / mag; !p.[1] := x1 / mag; !p.[2] := x2 / mag
end

fn normacrossprod_float_3 {l1,l2,l3:addr} (
    pf1: !float_3_v l1 , pf2: !float_3_v l2 , pf3: !float_3_v l3
  | p1: ptr l1, p2: ptr l2, p3: ptr l3
  ) : void = let
  val x0 = !p1.[0] and x1 = !p1.[1] and x2 = !p1.[2]
  val y0 = !p2.[0] and y1 = !p2.[1] and y2 = !p2.[2]
  val z0 = x1 * y2 - x2 * y1
  val z1 = x2 * y0 - x0 * y2
  val z2 = x0 * y1 - x1 * y0
  val mag = sqrtf (z0 * z0 + z1 * z1 + z2 * z2)
in
  !p3.[0] := z0 / mag; !p3.[1] := z1 / mag; !p3.[2] := z2 / mag
end

(* ****** ****** *)

extern
fun add_float_3_float_3 (
  v1: & @[float][3], v2: & @[float][3]
) : [l:addr] (free_gc_v (float?, 3, l), float_3_v l | ptr l)
  = "add_float_3_float_3"
overload + with add_float_3_float_3

%{^

ats_ptr_type
add_float_3_float_3
  (ats_ptr_type v1, ats_ptr_type v2) {
  int i ; float *v = ats_malloc_gc (3 * sizeof(float)) ;
  for (i = 0; i < 3; ++i) v[i] = ((float*)v1)[i] + ((float*)v2)[i] ;
  return v ;
} // end of [add_float_3_float_3]

%} // end of [%{^]

(* ****** ****** *)

fn drawTriangle {l1,l2,l3:addr} (
  pf1: !float_3_v l1 , pf2: !float_3_v l2 , pf3: !float_3_v l3
| p1: ptr l1, p2: ptr l2, p3: ptr l3
) : void = let
//
  extern fun glNormal3fv {l:addr}
    (pf: !float_3_v l | p: ptr l): void = "mac#atsctrb_glNormal3fv"
  extern fun glVertex3fv {l:addr}
    (pf: !float_3_v l | p: ptr l): void = "mac#atsctrb_glVertex3fv"
//
  val (pf | ()) = glBegin (GL_TRIANGLES)
  val () = glNormal3fv (pf1 | p1)
  val () = glVertex3fv (pf1 | p1)
  val () = glNormal3fv (pf2 | p2)
  val () = glVertex3fv (pf2 | p2)
  val () = glNormal3fv (pf3 | p3)
  val () = glVertex3fv (pf3 | p3)
  val () = glEnd (pf | (*none*))
//
in
  // empty
end // end of [drawTriangle]

extern
fun subdivide {n:nat} {l1,l2,l3:addr} (
  pf1: !float_3_v l1
, pf2: !float_3_v l2
, pf3: !float_3_v l3
| p1: ptr l1, p2: ptr l2, p3: ptr l3, n: int n
) : void = "subdivide"

implement
subdivide (
  pf1, pf2, pf3 | p1, p2, p3, n
) =
  if n > 0 then let
    val (pf12_gc, pf12 | p12) = !p1 + !p2
    val (pf23_gc, pf23 | p23) = !p2 + !p3
    val (pf31_gc, pf31 | p31) = !p3 + !p1
    val () = normalize_float_3 (pf12 | p12)
    val () = normalize_float_3 (pf23 | p23)
    val () = normalize_float_3 (pf31 | p31)
    val () = subdivide (pf1, pf12, pf31 | p1, p12, p31, n-1)
    val () = subdivide (pf2, pf23, pf12 | p2, p23, p12, n-1)
    val () = subdivide (pf3, pf31, pf23 | p3, p31, p23, n-1)
    val () = subdivide (pf12, pf23, pf31 | p12, p23, p31, n-1)
    val () = array_ptr_free {float} (pf12_gc, pf12 | p12)
    val () = array_ptr_free {float} (pf23_gc, pf23 | p23)
    val () = array_ptr_free {float} (pf31_gc, pf31 | p31)
  in
    // empty
  end else begin
    drawTriangle (pf1, pf2, pf3 | p1, p2, p3)
  end // end of [subdivide]

(* ****** ****** *)

typedef float_3 = array (float, 3)

(* ****** ****** *)

macdef X = $extval (float, "0.525731112119133606")
macdef Z = $extval (float, "0.850650808352039932")

typedef float_12_3 = array (float_3, 12)

val vdata00 = array_make_arrsz {float} $arrsz(~X,  0.0f,  Z)
val vdata01 = array_make_arrsz {float} $arrsz( X,  0.0f,  Z)
val vdata02 = array_make_arrsz {float} $arrsz(~X,  0.0f, ~Z)
val vdata03 = array_make_arrsz {float} $arrsz( X,  0.0f, ~Z)
val vdata10 = array_make_arrsz {float} $arrsz( 0.0f,  Z,  X)
val vdata11 = array_make_arrsz {float} $arrsz( 0.0f,  Z, ~X)
val vdata12 = array_make_arrsz {float} $arrsz( 0.0f, ~Z,  X)
val vdata13 = array_make_arrsz {float} $arrsz( 0.0f, ~Z, ~X)
val vdata20 = array_make_arrsz {float} $arrsz( Z,  X,  0.0f)
val vdata21 = array_make_arrsz {float} $arrsz(~Z,  X,  0.0f)
val vdata22 = array_make_arrsz {float} $arrsz( Z, ~X,  0.0f)
val vdata23 = array_make_arrsz {float} $arrsz(~Z, ~X,  0.0f)

val vdata
  : float_12_3 =
  array_make_arrsz {float_3} $arrsz(
  vdata00, vdata01, vdata02, vdata03
, vdata10, vdata11, vdata12, vdata13
, vdata20, vdata21, vdata22, vdata23
)

typedef nat12 = natLt (12)
typedef int_3 = array (nat12, 3)
typedef int_20_3 = array (int_3, 20)

val tind00 = array_make_arrsz {nat12} $arrsz(1, 4, 0)
val tind01 = array_make_arrsz {nat12} $arrsz(4, 9, 0)
val tind02 = array_make_arrsz {nat12} $arrsz(4, 5, 9)
val tind03 = array_make_arrsz {nat12} $arrsz(8, 5, 4)
val tind04 = array_make_arrsz {nat12} $arrsz(1, 8, 4)

val tind10 = array_make_arrsz {nat12} $arrsz(1, 10, 8)
val tind11 = array_make_arrsz {nat12} $arrsz(10, 3, 8)
val tind12 = array_make_arrsz {nat12} $arrsz(8, 3, 5)
val tind13 = array_make_arrsz {nat12} $arrsz(3, 2, 5)
val tind14 = array_make_arrsz {nat12} $arrsz(3, 7, 2)

val tind20 = array_make_arrsz {nat12} $arrsz(3, 10, 7)
val tind21 = array_make_arrsz {nat12} $arrsz(10, 6, 7)
val tind22 = array_make_arrsz {nat12} $arrsz(6, 11, 7)
val tind23 = array_make_arrsz {nat12} $arrsz(6, 0, 11)
val tind24 = array_make_arrsz {nat12} $arrsz(6, 1, 0)

val tind30 = array_make_arrsz {nat12} $arrsz(10, 1, 6)
val tind31 = array_make_arrsz {nat12} $arrsz(11, 0, 9)
val tind32 = array_make_arrsz {nat12} $arrsz(2, 11, 9)
val tind33 = array_make_arrsz {nat12} $arrsz(5, 2, 9)
val tind34 = array_make_arrsz {nat12} $arrsz(11, 2, 7)

val tindices
  : int_20_3 =
  array_make_arrsz {int_3}  $arrsz(
  tind00, tind01, tind02, tind03, tind04
, tind10, tind11, tind12, tind13, tind14
, tind20, tind21, tind22, tind23, tind24
, tind30, tind31, tind32, tind33, tind34
)

extern fun display (): void = "display"
implement display () = let
  val () = glClear (GL_COLOR_BUFFER_BIT lor GL_DEPTH_BUFFER_BIT)
  val () = glColor3d (1.0, 1.0, 1.0)
  val (pf | ()) = glBegin (GL_TRIANGLES)
  val () = loop (0) where {
    extern fun glNormal3fv (A: array (float, 3)): void
      = "mac#atsctrb_glNormal3fv"
    extern fun glVertex3fv (A: array (float, 3)): void
      = "mac#atsctrb_glVertex3fv"
    extern fun subdivide {n:nat} (
      A1: array (float, 3), A2: array (float, 3), A3: array (float, 3), n: int n
    ) : void = "subdivide"
    fun loop {i:nat | i <= 20} (i: int i): void =
      if i < 20 then let 
        val ti: int_3 = tindices[i]
        val vdata_ti_0 = vdata[ti[0]]
        val vdata_ti_1 = vdata[ti[1]]
        val vdata_ti_2 = vdata[ti[2]]
        val () = subdivide (vdata_ti_0, vdata_ti_1, vdata_ti_2, 4)
(*
        val () = glNormal3fv (vdata_ti_0)
        val () = glVertex3fv (vdata[ti[0]])
        val () = glNormal3fv (vdata_ti_1)
        val () = glVertex3fv (vdata[ti[1]])
        val () = glNormal3fv (vdata_ti_2)
        val () = glVertex3fv (vdata[ti[2]])
*)
      in
        loop (i+1)
      end // end of [if]
  } // end of [where]
  val () = glEnd (pf | (*none*))
  val () = glFlush ()
in
  // empty
end // end of [display]

(* ****** ****** *)

implement main_dummy () = ()

%{$

void initialize () {
  GLfloat mat_specular[] = { 1.0, 1.0, 1.0, 1.0 } ;
  GLfloat mat_shininess[] = { 50.0 } ;
  GLfloat light_position[] = { 1.0, 1.0, 1.0, 0.0 } ;
  GLfloat white_light[] = { 1.0, 1.0, 1.0, 1.0 } ;
  GLfloat lmodel_ambient[] = { 0.1, 0.1, 0.1, 1.0 } ;

  glClearColor (0.0, 0.0, 0.0, 0.0) ;
  glShadeModel (GL_SMOOTH) ;

  glMaterialfv (GL_FRONT, GL_SPECULAR, mat_specular) ;
  glMaterialfv (GL_FRONT, GL_SHININESS, mat_shininess) ;
  glLightfv (GL_LIGHT0, GL_POSITION, light_position) ;
  glLightfv (GL_LIGHT0, GL_DIFFUSE, white_light) ;
  glLightfv (GL_LIGHT0, GL_SPECULAR, white_light) ;
  glLightModelfv (GL_LIGHT_MODEL_AMBIENT, lmodel_ambient) ;

  glEnable (GL_LIGHTING) ;
  glEnable (GL_LIGHT0) ;
  glEnable (GL_DEPTH_TEST) ;
  return ;
}

ats_void_type
mainats (
  ats_int_type argc, ats_ptr_type argv
) {
  glutInit ((int*)&argc, (char**)argv) ;
  glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB) ;
  glutInitWindowSize (500, 500) ;
  glutInitWindowPosition (100, 100) ;
  glutCreateWindow("Icosahedron") ;
  initialize () ;
  glutDisplayFunc (display) ;
  glutMainLoop () ;
  return ; /* deadcode */
} // end of [mainats]

%} // end of [%{$]

(* ****** ****** *)

(* end of [glIcosahedron.dats] *)
