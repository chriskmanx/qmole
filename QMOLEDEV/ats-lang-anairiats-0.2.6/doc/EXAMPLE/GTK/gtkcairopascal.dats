(*
**
** A simple GTK/CAIRO example:
** Illustrating a famous theorm of Pascal's
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Summer, 2010
**
*)

(* ****** ****** *)

staload _ = "prelude/DATS/list_vt.dats"
staload _ = "prelude/DATS/reference.dats"

(* ****** ****** *)

staload "libc/SATS/math.sats"

macdef PI = M_PI
macdef _2PI = 2*PI

staload "libc/SATS/random.sats"
staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

typedef dbl = double

// y = k1 * x + b1
// y = k2 * x + b2

fun line_intersect (
    k1: dbl, b1: dbl, k2: dbl, b2: dbl
  ) : @(dbl, dbl) = (x, y) where {
  val x = (b1 - b2) / (k2 - k1); val y = k1 * x + b1
} // end of [line_intersect]

(* ****** ****** *)

fun sort {n:nat}
  (xs: list_vt (double, n)): list_vt (double, n) = let
  var !p_cmp = @lam (x1: &double, x2: &double): Sgn =<clo> compare (x1, x2)
in
  list_vt_mergesort<double> (xs, !p_cmp)
end // end of [sort]

(* ****** ****** *)

fun draw_colinear {l:agz} (
    cr: !cairo_ref l
  , x1: double, y1: double, x2: double, y2: double, x3: double, y3: double
  ) : void =
  if x1 <= x2 then
    if x1 <= x3 then let
      val () = cairo_move_to (cr, x1, y1)
    in
      if x2 <= x3 then cairo_line_to (cr, x3, y3) else cairo_line_to (cr, x2, y2)
    end else let
      val () = cairo_move_to (cr, x3, y3)
    in
      cairo_line_to (cr, x2, y2)
    end
  else // x2 < x1
    if x2 <= x3 then let
      val () = cairo_move_to (cr, x2, y2)
    in
      if x1 <= x3 then cairo_line_to (cr, x3, y3) else cairo_line_to (cr, x1, y1)
    end else let
      val () = cairo_move_to (cr, x3, y3)
    in
      cairo_line_to (cr, x1, y1)
    end 
  // end of [if]
(* end of [draw_colinear] *)

(* ****** ****** *)

#define :: list0_cons
#define nil list0_nil

fun genRandDoubles
  {n:nat} (n: int n): list_vt (double, n) =
  if n > 0 then let
    val x = drand48 () in list_vt_cons (x, genRandDoubles (n-1))
  end else list_vt_nil
// end of [genRandDoubles]

local
val () = srand48_with_time ()
in
val theVertexLst
  : ref (list0 double) = let
  val ts = genRandDoubles (6)
  val ts = sort (ts); val ts = list0_of_list_vt (ts)
in
  ref_make_elt<list0 double> (ts)
end // enbd of [theVertexLst]
end // end of [local]

(* ****** ****** *)

extern fun draw_pascal_theorem
  {l:agz} (cr: !cairo_ref l, W: double, H: double): void
// end of [draw_pascal_theorem]

implement
draw_pascal_theorem
  (cr, W, H) = () where {
  val ts = !theVertexLst
  val- t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: nil () = ts
  val a1 = _2PI * t1
  val x1 = cos a1 and y1 = sin a1
  val a2 = _2PI * t2
  val x2 = cos a2 and y2 = sin a2
  val a3 = _2PI * t3
  val x3 = cos a3 and y3 = sin a3
  val a4 = _2PI * t4
  val x4 = cos a4 and y4 = sin a4
  val a5 = _2PI * t5
  val x5 = cos a5 and y5 = sin a5
  val a6 = _2PI * t6
  val x6 = cos a6 and y6 = sin a6
//
  val k12 = (y1 - y2) / (x1 - x2)
  val b12 = y1 - k12 * x1
  val k23 = (y2 - y3) / (x2 - x3)
  val b23 = y2 - k23 * x2
  val k34 = (y3 - y4) / (x3 - x4)
  val b34 = y3 - k34 * x3
  val k45 = (y4 - y5) / (x4 - x5)
  val b45 = y4 - k45 * x4
  val k56 = (y5 - y6) / (x5 - x6)
  val b56 = y5 - k56 * x5
  val k61 = (y6 - y1) / (x6 - x1)
  val b61 = y6 - k61 * x6
//
  val (px1, py1) = line_intersect (k12, b12, k45, b45)
  val (px2, py2) = line_intersect (k23, b23, k56, b56)
  val (px3, py3) = line_intersect (k34, b34, k61, b61)
//
  val pxs = let
    #define nil list_vt_nil
    #define :: list_vt_cons
  in
    1.0 :: ~1.0 :: px1 :: px2 :: px3 :: nil ()
  end // end of [val]
  val qxs = sort (pxs)
  val ~list_vt_cons (qx1, qxs) = qxs
  val ~list_vt_cons (_qx2, qxs) = qxs
  val ~list_vt_cons (_qx3, qxs) = qxs
  val ~list_vt_cons (_qx4, qxs) = qxs
  val ~list_vt_cons (qx5, qxs) = qxs
  val ~list_vt_nil () = qxs
//
  val dx = qx5 - qx1
//
  val pys = let
    #define nil list_vt_nil
    #define :: list_vt_cons
  in
    1.0 :: ~1.0 :: py1 :: py2 :: py3 :: nil ()
  end // end of [val]
  val qys = sort (pys)
  val ~list_vt_cons (qy1, qys) = qys
  val ~list_vt_cons (_qy2, qys) = qys
  val ~list_vt_cons (_qy3, qys) = qys
  val ~list_vt_cons (_qy4, qys) = qys
  val ~list_vt_cons (qy5, qys) = qys
  val ~list_vt_nil () = qys
//
  val dy = qy5 - qy1
//
  val WH = min (W, H)
  val dxy = max (dx, dy)
  val alpha = WH / dxy
//
  val () = cairo_translate (cr, (W-WH)/2, (H-WH)/2)
//
  val () = cairo_scale (cr, alpha, alpha)
  val () = cairo_rectangle (cr, 0.0, 0.0, dxy, dxy)
  val () = cairo_set_source_rgb (cr, 1.0, 1.0, 1.0) // white color
  val () = cairo_fill (cr)
//
  val () = cairo_translate (
    cr, (dxy - dx) / 2 - (qx1/dx)*dx, (dxy - dy) / 2 - (qy1/dy)*dy
  ) // end of [val]
//
  val xc = 0.0 and yc = 0.0; val rad = 1.0
  val () = cairo_arc (cr, xc, yc, rad, 0.0, _2PI)
  val () = cairo_set_source_rgb (cr, 0.0, 0.0, 1.0) // blue color
  val () = cairo_fill (cr)
//
  val () = cairo_move_to (cr, x1, y1)
  val () = cairo_line_to (cr, x2, y2)
  val () = cairo_line_to (cr, x3, y3)
  val () = cairo_line_to (cr, x4, y4)
  val () = cairo_line_to (cr, x5, y5)
  val () = cairo_line_to (cr, x6, y6)
  val () = cairo_close_path (cr)
  val () = cairo_set_source_rgb (cr, 1.0, 1.0, 0.0) // yellow color
  val () = cairo_fill (cr)
//
  val () = cairo_set_line_width (cr, 0.01)
  val () = cairo_set_source_rgb (cr, 0.0, 0.0, 0.0) // black color
//
  val () = draw_colinear (cr, x1, y1, x2, y2, px1, py1)
  val () = draw_colinear (cr, x4, y4, x5, y5, px1, py1)
//
  val () = draw_colinear (cr, x2, y2, x3, y3, px2, py2)
  val () = draw_colinear (cr, x5, y5, x6, y6, px2, py2)
//
  val () = draw_colinear (cr, x3, y3, x4, y4, px3, py3)
  val () = draw_colinear (cr, x6, y6, x1, y1, px3, py3)
//
  val () = draw_colinear (cr, px1, py1, px2, py2, px3, py3)
//
  val () = cairo_stroke (cr)
//
} // end of [draw_pascal_theorem]

(* ****** ****** *)

staload "contrib/glib/SATS/glib.sats"
staload "contrib/glib/SATS/glib-object.sats"

(* ****** ****** *)

staload "contrib/GTK/SATS/gdk.sats"
staload "contrib/GTK/SATS/gtk.sats"
staload "contrib/GTK/SATS/gtkclassdec.sats"

(* ****** ****** *)

%{^
GtkWidget *the_drawingarea = NULL;
ats_ptr_type
the_drawingarea_get () {
  g_object_ref (G_OBJECT(the_drawingarea)); return the_drawingarea ;
}
ats_void_type
the_drawingarea_set (ats_ptr_type x) {
  g_object_ref(G_OBJECT(x)) ;
  if (the_drawingarea) g_object_unref (G_OBJECT(the_drawingarea));
  the_drawingarea = x ;
  return ;
} // end of [the_drawingarea_set]
%} // end of [%{^] 
extern fun the_drawingarea_get (): GtkDrawingArea_ref1 = "the_drawingarea_get"
extern fun the_drawingarea_set (x: !GtkDrawingArea_ref1): void = "the_drawingarea_set"

(* ****** ****** *)

fun fnext () = () where {
  val ts = genRandDoubles (6)
  val ts = sort (ts)
  val ts = list0_of_list_vt (ts) // no-op
  val () = !theVertexLst := ts
  val darea = the_drawingarea_get ()
  val (pf, fpf | p) = gtk_widget_getref_allocation (darea)
  val () = gtk_widget_queue_draw_area (darea, (gint)0, (gint)0, p->width, p->height)
  prval () = minus_addback (fpf, pf | darea)
  val () = g_object_unref (darea)
} // end of [fnext]

fun draw_main {l:agz} (
    cr: !cairo_ref l, W: int, H: int
  ) : void = () where {
  val W = (double_of)W
  and H = (double_of)H
  val () = draw_pascal_theorem (cr, W, H)
} // end of [draw_main]

(* ****** ****** *)

%{^
extern
ats_void_type
mainats (ats_int_type argc, ats_ptr_type argv) ;
%} // end of [%{^]

(* ****** ****** *)

fun fexpose
  {c:cls | c <= GtkDrawingArea} {l:agz}
  (darea: !gobjref (c, l), event: &GdkEvent): gboolean = let
  prval () = clstrans {c,GtkDrawingArea,GtkWidget} ()
  val (fpf_win | win) = gtk_widget_get_window (darea)
  val () = assert_errmsg (g_object_isnot_null (win), #LOCATION)
  val cr = gdk_cairo_create (win)
  prval () = minus_addback (fpf_win, win | darea)
  val (pf, fpf | p) = gtk_widget_getref_allocation (darea)
  val () = draw_main (cr, (int_of)p->width, (int_of)p->height)
  prval () = minus_addback (fpf, pf | darea)
  val () = cairo_destroy (cr)
in
  GFALSE // HX: what does this mean?
end // end of [fexpose]

(* ****** ****** *)

macdef gs = gstring_of_string

(* ****** ****** *)

extern fun main1 (): void = "main1"

implement main1 () = () where {
//
  val () = srand48_with_time ()
//
  val window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  val () =
    gtk_window_set_default_size (window, (gint)400, (gint)400)
  // end of [val]
  val (fpf_x | x) = (gs)"cairo: illustrating Pascal's theorem"
  val () = gtk_window_set_title (window, x)
  prval () = fpf_x (x)
  val _sid = g_signal_connect
    (window, (gsignal)"delete-event", G_CALLBACK (gtk_widget_destroy), (gpointer)null)
  val _sid = g_signal_connect
    (window, (gsignal)"destroy", G_CALLBACK (gtk_main_quit), (gpointer)null)
//
  val vbox0 = gtk_vbox_new (GFALSE(*homo*), (gint)10(*spacing*))
  val () = gtk_container_add (window, vbox0)
//
  val hbox1 = gtk_hbox_new (GFALSE, (gint)0)
  val () = gtk_box_pack_start (vbox0, hbox1, GFALSE, GFALSE, (guint)0)
  val () = g_object_unref (hbox1)
//
  val darea = gtk_drawing_area_new ()
  val () = the_drawingarea_set (darea)
  val () = gtk_box_pack_start (vbox0, darea, GTRUE, GTRUE, (guint)0)
  val _sid = g_signal_connect
    (darea, (gsignal)"expose-event", G_CALLBACK (fexpose), (gpointer)null)
  val () = g_object_unref (darea)
//
  val hsep = gtk_hseparator_new ()
  val () = gtk_box_pack_start (vbox0, hsep, GFALSE, GFALSE, (guint)0)
  val () = g_object_unref (hsep)
//
  val hbox1 = gtk_hbox_new (GFALSE(*homo*), (gint)5(*spacing*))
  val () = gtk_box_pack_start (vbox0, hbox1, GFALSE, GTRUE, (guint)10)
//
  val (fpf_x | x) = (gs)"_Close"
  val btn_close = gtk_button_new_with_mnemonic (x)
  prval () = fpf_x (x)
  val _sid = g_signal_connect
    (btn_close, (gsignal)"clicked", G_CALLBACK(gtk_main_quit), (gpointer_vt)window)
  // end of [val]
  val () = gtk_box_pack_end (hbox1, btn_close, GFALSE, GFALSE, (guint)4)
  val () = g_object_unref (btn_close)
//
  val (fpf_x | x) = (gs)"_Next"
  val btn_next = gtk_button_new_with_mnemonic (x)
  prval () = fpf_x (x)
  val _sid = g_signal_connect
    (btn_next, (gsignal)"clicked", G_CALLBACK(fnext), (gpointer)null)
  // end of [val]
  val () = gtk_box_pack_end (hbox1, btn_next, GFALSE, GFALSE, (guint)4)
  val () = g_object_unref (btn_next)
//
  val () = g_object_unref (hbox1)
  val () = g_object_unref (vbox0)
  val () = gtk_widget_show_all (window)
  val () = g_object_unref (window)
  val () = gtk_main ()
} // end of [main1]

(* ****** ****** *)

implement main_dummy () = ()

(* ****** ****** *)

%{$
ats_void_type
mainats (
  ats_int_type argc, ats_ptr_type argv
) {
  gtk_init ((int*)&argc, (char***)&argv) ; main1 () ; return ;
} // end of [mainats]
%} // end of [%{$]

(* ****** ****** *)

(* end of [gtkcairopascal.dats] *)
