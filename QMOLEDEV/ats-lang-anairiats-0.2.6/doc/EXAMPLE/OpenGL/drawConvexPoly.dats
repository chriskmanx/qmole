//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: March, 2010
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

staload GA = "libats/SATS/genarrays.sats"
stadef GEVEC = $GA.GEVEC
stadef GEVEC_v = $GA.GEVEC_v

staload _(*anon*) = "libats/DATS/genarrays.dats"

(* ****** ****** *)

typedef colorType =
  $extype_struct "colorType" of { r= float, g= float, b= float }
// end of [colorType]

viewtypedef
polygonType (n:int) =
  $extype_struct "polygonType" of {
  n_verts= int n
, concave= int
, selected_vert= intLt n
, verts= ptr // verts[MAX_NUM_VERTS][2]
, color= colorType
, _rest = undefined_vt
} // end of [polygonType]

viewtypedef polygonType = [n:nat] polygonType n

extern
fun polygon_get_vertx {n:nat} {l:addr}
  (pf_poly: !polygonType n @ l | p_poly: ptr l)
  :<> [la:addr] (
  GEVEC_v (float, n, 2, la), minus (ptr l, GEVEC_v (float, n, 2, la))
| ptr la
) = "mac#polygon_get_vertx"
// end of [polygon_get_vertx]

extern
fun polygon_get_verty {n:nat} {l:addr}
  (pf_poly: !polygonType n @ l | p_poly: ptr l)
  :<> [la:addr] (
  GEVEC_v (float, n, 2, la), minus (ptr l, GEVEC_v (float, n, 2, la))
| ptr la
) = "mac#polygon_get_verty"
// end of [polygon_get_verty]

(* ****** ****** *)

#define i2sz size1_of_int1

(* ****** ****** *)

extern
fun moveVert {n:nat}
  (poly: &polygonType n, x: int, y: int):<!exn> void
  = "ext#_moveVert"
// end of[moveVert]

implement
moveVert (poly, x, y) = let
  val i = poly.selected_vert in if (i >= 0) then let
  val i = (i2sz)i
  val p_poly = &poly
  prval pf_poly = view@ poly
//
  val (pf_vertx, fpf_vertx | p_vertx) = polygon_get_vertx (pf_poly | p_poly)
  val () = $GA.GEVEC_ptr_set_elt_at<float> (!p_vertx, 2, i, (float_of)x)
  prval () = minus_addback (fpf_vertx, pf_vertx | p_poly)
//
  val (pf_verty, fpf_verty | p_verty) = polygon_get_verty (pf_poly | p_poly)
  val () = $GA.GEVEC_ptr_set_elt_at<float> (!p_verty, 2, i, (float_of)y)
  prval () = minus_addback (fpf_verty, pf_verty | p_poly)
//
  prval () = view@ poly := pf_poly
in
  // nothing
end // end of [if]
end // end of [moveVert]

(* ****** ****** *)

extern
fun selectVert {n:nat}
  (poly: &polygonType n, x: int, y: int):<> void = "ext#_selectVert"
// end of[selectVert]

implement selectVert
  {n} (poly, x, y) = let
  val n = poly.n_verts in if n > 0 then let
  val x = (float_of)x and y = (float_of)y
  val p_poly = &poly
  prval pf_poly = view@ poly
  val (pf_vertx, fpf_vertx | p_vertx) = polygon_get_vertx (pf_poly | p_poly)
  val (pf_verty, fpf_verty | p_verty) = polygon_get_verty (pf_poly | p_poly)
//
  var i: natLt n = 0
  val x0 = $GA.GEVEC_ptr_get_elt_at<float> (!p_vertx, 2, (i2sz)0)
  val y0 = $GA.GEVEC_ptr_get_elt_at<float> (!p_verty, 2, (i2sz)0)
  var delta: float = square (x0 - x) + square (y0 - y)
  var k: natLte n // uninitalized
  val () = for*
    {k:nat | k <= n} .<n-k>. (k: int k) =>
    (k := 1; k < n; k := k + 1) let
    val x1 = $GA.GEVEC_ptr_get_elt_at<float> (!p_vertx, 2, (i2sz)k)
    val y1 = $GA.GEVEC_ptr_get_elt_at<float> (!p_verty, 2, (i2sz)k)
    val delta1 = square (x1 - x) + square (y1 - y)
    val () = if :(k: int k) => (delta1 < delta) then (delta := delta1; i := k)
    // end of [val]
  in
    // continue
  end // end of [val]
//
  prval () = minus_addback (fpf_vertx, pf_vertx | p_poly)
  prval () = minus_addback (fpf_verty, pf_verty | p_poly)
  prval () = view@ poly := pf_poly
//
  // val () = printf ("selectVert: i = %i\n", @(i))
//
  val () = poly.selected_vert := i
in
  // nothing
end // end of [if]
end // end of [selectVert]

(* ****** ****** *)

staload "contrib/GL/SATS/gl.sats"

(* ****** ****** *)

extern
fun drawConvexPoly
  {n:nat} (poly: &polygonType n): void = "ext#_drawConvexPoly"
// end of [drawConvexPoly]

implement drawConvexPoly {n} (poly) = let
  val n = poly.n_verts
  val p_poly = &poly
  prval pf_poly = view@ poly
  val (pf_vertx, fpf_vertx | p_vertx) = polygon_get_vertx (pf_poly | p_poly)
  val (pf_verty, fpf_verty | p_verty) = polygon_get_verty (pf_poly | p_poly)
//
  val (pf_att | ()) = glPushAttrib(GL_CURRENT_BIT)
  val () = glColor3f(
    (GLfloat)poly.color.r, (GLfloat)poly.color.g, (GLfloat)poly.color.b
  ) // end of [val]
  val () = case+ 0 of
    | _ when n = 0 => ()
    | _ when n = 1 => let
        val (pf_beg | ()) = glBegin (GL_POINTS)
        val x0 = $GA.GEVEC_ptr_get_elt_at<float> (!p_vertx, 2, (i2sz)0)
        val y0 = $GA.GEVEC_ptr_get_elt_at<float> (!p_verty, 2, (i2sz)0)
        val () = glVertex2f ((GLfloat)x0, (GLfloat)y0)
        val () = glEnd (pf_beg | (*none*))
      in
        // nothing
      end // end of [n = 1]
    | _ when n = 2 => let
        val (pf_beg | ()) = glBegin (GL_POINTS)
        val x0 = $GA.GEVEC_ptr_get_elt_at<float> (!p_vertx, 2, (i2sz)0)
        val y0 = $GA.GEVEC_ptr_get_elt_at<float> (!p_verty, 2, (i2sz)0)
        val () = glVertex2f ((GLfloat)x0, (GLfloat)y0)
        val x1 = $GA.GEVEC_ptr_get_elt_at<float> (!p_vertx, 2, (i2sz)1)
        val y1 = $GA.GEVEC_ptr_get_elt_at<float> (!p_verty, 2, (i2sz)1)
        val () = glVertex2f ((GLfloat)x1, (GLfloat)y1)
        val () = glEnd (pf_beg | (*none*))
      in
        // nothing
      end // end of [n = 2]
    | _ when n >= 3 => let
        val (pf_beg | ()) = glBegin (GL_POLYGON)
        var i: sizeLte n // unintialized
        val () = for (i := (i2sz)0; i < n; i := i + 1) let
          val x = $GA.GEVEC_ptr_get_elt_at<float> (!p_vertx, 2, i)
          val y = $GA.GEVEC_ptr_get_elt_at<float> (!p_verty, 2, i)
        in
          glVertex2f ((GLfloat)x, (GLfloat)y)
        end // end of [for]
        val () = glEnd (pf_beg | (*none*))
      in
        // nothing
      end // end of [n >= 3]
    | _ => ()
  val () = glPopAttrib (pf_att | (*none*))
//
  prval () = minus_addback (fpf_vertx, pf_vertx | p_poly)
  prval () = minus_addback (fpf_verty, pf_verty | p_poly)
  prval () = view@ poly := pf_poly
in
  // nothing
end // end of [drawConvexPoly]

(* ****** ****** *)

(* end of [drawConvexPoly.dats] *)
