(*
**
** A simple GTK/CAIRO example: a clock @ home
** The part of clock drawing is largely taken from
** a cairo example I did in December 2009
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: April, 2010
**
*)

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list.dats"
staload _(*anon*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

staload "libc/SATS/math.sats"
macdef PI = M_PI
staload "libc/SATS/random.sats"

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"
staload "contrib/cairo/SATS/cairo_extra.sats"

(* ****** ****** *)

stadef dbl = double
stadef cr = cairo_ref

(* ****** ****** *)

val theWidth = 50.0
val theHeight = 50.0
val theRadius = sqrt (theWidth*theWidth + theHeight*theHeight)
val theTimeInterval = 30

(* ****** ****** *)

typedef disc = @{
  x= dbl, y= dbl, r= dbl, vx= dbl, vy= dbl, txt= string
} // end of [disc]

fn draw_disc {l:agz}
  (cr: !cr l, d: &disc): void = let
  val x = d.x and y = d.y
  val r = d.r
  val rad = sqrt (x*x + y*y)
  val r1 = r/5 + 4*r/5 * (1 - rad / theRadius)
  val (pf | ()) = cairo_save (cr)
  val () = cairo_translate (cr, x, y)
  val () = cairo_arc (cr, 0.0, 0.0, r1, 0.0, 2*PI)
  val () = cairo_fill (cr)
  val w = 1.6*r1 and h = 1.6*r1
  val txt = d.txt
  val () = if txt <> "" then let
    val () = cairo_set_source_rgb (cr, 0.0, 0.5, 0.5) // dark gray
    val () = cairo_show_text_inbox (cr, w, h, txt)
  in
    // nothing
  end // end of [val]
  val () = cairo_restore (pf | cr)
in
  // nothing
end // end of [draw_hand]

(* ****** ****** *)

val theDiscLst: ref (List disc) = ref_make_elt (list_nil)

fn draw_theDiscLst
  {l:agz} (cr: !cr l): void = let
  fun loop {n:nat} .<n>.
    (cr: !cr l, ds: !list_vt (disc, n)): void = 
    case+ ds of
    | list_vt_cons (!p_d, !p_ds) => let
        val () = draw_disc (cr, !p_d)
        val () = loop (cr, !p_ds); prval () = fold@ (ds)
      in
        // nothing
      end // end of [list_cons]
    | list_vt_nil () => fold@ (ds)
  // end of [loop]
  val ds = !theDiscLst
  val (fpf_ds | ds) = __cast (ds) where {
    viewtypedef T = List_vt (disc)
    extern castfn __cast {a:type} (x: List disc):<> (T -<lin,prf> void | T)
  } // end of [val]
  val () = loop (cr, ds)
  prval () = fpf_ds (ds)
in
  // nothing
end // end of [draw_theDiscLst]

(* ****** ****** *)

extern fun draw_main {l:agz}
  (cr: !cairo_ref l, width: int, height: int): void
implement draw_main
  (cr, width, height) = () where {
  val w = (double_of)width
  val h = (double_of)height
  val mn = min (w, h)
  val xc = w / 2 and yc = h / 2
  val (pf0 | ()) = cairo_save (cr)
  val () = cairo_translate (cr, xc, yc)
  val alpha = mn / 100
  val () = cairo_scale (cr, alpha, alpha)
//
  val () = cairo_rectangle (cr, ~50.0, ~50.0, 100.0, 100.0)
  val () = cairo_set_source_rgb (cr, 1.0, 1.0, 1.0)
  val () = cairo_fill (cr)
//
  val () = cairo_set_source_rgb (cr, 1.0, 0.5, 0.5)
  val () = draw_theDiscLst (cr)
//
  val () = cairo_restore (pf0 | cr)
} // end of [draw_main]

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
  if (the_drawingarea == NULL) {
    fprintf (stderr, "exit(the_drawingarea_get): not initialized yet\n"); exit(1);
  } // end of [if]
  return the_drawingarea ;
} // end of [the_drawingarea_get]
ats_void_type
the_drawingarea_initset (ats_ptr_type x) {
  static int the_drawingarea_initset_flag = 0 ;
  if (the_drawingarea_initset_flag) {
    fprintf (stderr, "exit(the_drawingarea_initset): already initialized\n"); exit(1);
  } // end of [if]
  the_drawingarea_initset_flag = 1 ; the_drawingarea = x ; return ;
} // end of [the_drawingarea_initset]
%} // end of [%{^] 
extern
fun the_drawingarea_get (): [l:agz]
  (gobjref (GtkDrawingArea, l) -<lin,prf> void | gobjref (GtkDrawingArea, l))
  = "the_drawingarea_get"
// end of [the_drawingarea_get]
extern
fun the_drawingarea_initset
  (x: GtkDrawingArea_ref1): void = "the_drawingarea_initset"
// end of [the_drawingarea_initset]

(* ****** ****** *)

%{^
extern
ats_void_type
mainats (ats_int_type argc, ats_ptr_type argv) ;
%} // end of [%{^]

(* ****** ****** *)

fn discGen (): disc = let
  val r = 10 * drand48 ()
  val x = (theWidth - r) * drand48 ()
  val y = (theWidth - r) * drand48 ()
  val vx = 0.05 * drand48 ()
  val vy = 0.05 * drand48 ()
in
  @{ x= x, y= y, r= r, vx= vx, vy= vy, txt= "" }
end // end of [discGen]

fun fnext () = let
  val d = discGen ()
  val ds = !theDiscLst
(*
  val n = list_length (ds)
  val () = (print "fnext: n = "; print n; print_newline ())
*)
in
  !theDiscLst := list_cons (d, ds)
end // end of [fnext]

fun fprev () = let
  val ds = !theDiscLst
in
  case ds of
  | list_cons (d, ds) => !theDiscLst := ds
  | list_nil () => ()
end // end of [fprev]

(* ****** ****** *)

fn update_disc
  (d: &disc): void = let
  var xsgn: int = 1
  var ysgn: int = 1
  val r = d.r
  val w = theWidth - r
  val h = theHeight - r
  val vx = d.vx and vy = d.vy
  val dx = vx * theTimeInterval
  val dy = vy * theTimeInterval
  val x1 = d.x + dx
  val x1 = (
    if x1 > w then (xsgn := ~1; 2*w - x1)
    else if x1 < ~w then (xsgn := ~1; ~2*w - x1)
    else x1
  ) : double
  val y1 = d.y + dy
  val y1 = (
    if y1 > h then (ysgn := ~1; 2*h - y1)
    else if y1 < ~h then (ysgn := ~1; ~2*h - y1)
    else y1
  ) : double
in
  d.x := x1; d.y := y1; d.vx := xsgn * vx; d.vy := ysgn * vy
end // end of [update_disc]

fn update_theDiscLst (): void = let
  fun loop {n:nat} .<n>.
    (ds: !list_vt (disc, n)): void = 
    case+ ds of
    | list_vt_cons (!p_d, !p_ds) => let
        val () = update_disc (!p_d)
        val () = loop (!p_ds); prval () = fold@ (ds)
      in
        // nothing
      end // end of [list_cons]
    | list_vt_nil () => fold@ (ds)
  // end of [loop]
  val ds = !theDiscLst
  val (fpf_ds | ds) = __cast (ds) where {
    viewtypedef T = List_vt (disc)
    extern castfn __cast {a:type} (x: List disc):<> (T -<lin,prf> void | T)
  } // end of [val]
  val () = loop (ds)
  prval () = fpf_ds (ds)
in
  // nothing
end // end of [update_theDiscLst]

(* ****** ****** *)

fn fexpose
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

fn ftimeout
  (_: gpointer): gboolean = let
  val (fpf_darea | darea) = the_drawingarea_get ()
  val (fpf_win | win) = gtk_widget_get_window (darea)
in
  if g_object_isnot_null (win) then let
    val () = update_theDiscLst ()
    prval () = minus_addback (fpf_win, win | darea)
    val (pf, fpf | p) = gtk_widget_getref_allocation (darea)
    val () = gtk_widget_queue_draw_area (darea, (gint)0, (gint)0, p->width, p->height)
    prval () = minus_addback (fpf, pf | darea)
    prval () = fpf_darea (darea)
  in
    GTRUE
  end else let
    prval () = minus_addback (fpf_win, win | darea)
    prval () = fpf_darea (darea)
  in
    GFALSE // HX: terminating the timer
  end // end of [if]
end // end of [ftimeout]

(* ****** ****** *)

val () = let
  val ds = !theDiscLst
  val d = @{
    x= 45.0, y= 0.0, r= 12.0, vx= 0.023, vy= 0.037, txt= "Zoe"
  } // end of [val]
  val ds = list_cons (d, ds)
  val d = @{
    x= ~45.0, y= 0.0, r= 5.0, vx= 0.037, vy= 0.023, txt= "mom"
  } // end of [val]
  val ds = list_cons (d, ds)
  val d = @{
    x= 0.0, y= 15.0, r= 8.0, vx= 0.041, vy= 0.029, txt= "dad"
  } // end of [val]
  val ds = list_cons (d, ds)
  val d = @{
    x= 0.0, y= 15.0, r= 10.0, vx= 0.043, vy= 0.019, txt= "grandma"
  } // end of [val]
  val ds = list_cons (d, ds)
  val d = @{
    x= 0.0, y= ~25.0, r= 10.0, vx= 0.047, vy= 0.029, txt= "grandpa"
  } // end of [val]
  val ds = list_cons (d, ds)
in
  !theDiscLst := ds
end // end of [val]

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
  val (fpf_x | x) = (gs)"cairo: moving discs"
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
  val () = gtk_box_pack_start (vbox0, darea, GTRUE, GTRUE, (guint)0)
  val _sid = g_signal_connect
    (darea, (gsignal)"expose-event", G_CALLBACK (fexpose), (gpointer)null)
  val () = the_drawingarea_initset (darea)
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
  val (fpf_x | x) = (gs)"_Remove"
  val btn_next = gtk_button_new_with_mnemonic (x)
  prval () = fpf_x (x)
  val _sid = g_signal_connect
    (btn_next, (gsignal)"clicked", G_CALLBACK(fprev), (gpointer)null)
  // end of [val]
  val () = gtk_box_pack_end (hbox1, btn_next, GFALSE, GFALSE, (guint)4)
  val () = g_object_unref (btn_next)
//
  val (fpf_x | x) = (gs)"_Add"
  val btn_next = gtk_button_new_with_mnemonic (x)
  prval () = fpf_x (x)
  val _sid = g_signal_connect
    (btn_next, (gsignal)"clicked", G_CALLBACK(fnext), (gpointer)null)
  // end of [val]
  val () = gtk_box_pack_end (hbox1, btn_next, GFALSE, GFALSE, (guint)4)
  val () = g_object_unref (btn_next)
//
  val _rid = gtk_timeout_add ((guint32)theTimeInterval, ftimeout, (gpointer)null)
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

(* end of [gtkcairodisc.dats] *)
