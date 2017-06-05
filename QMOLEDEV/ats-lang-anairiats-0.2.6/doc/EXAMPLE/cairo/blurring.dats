//
// Cairo: Test text transform
// The code is directly translated from on the attached C code
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2010
//

(* ****** ****** *)

staload "libc/SATS/math.sats"

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

extern
fun blur_image_surface
  {l:agz} {r:int} (sf: !cairo_surface_ref l, radius: int r): void
  = "blur_image_surface"
// end of [blur_image_surface]

fun draw {l:agz}
  (cr: !cairo_ref l, w: int, h: int): void = () where {
//
  val mn = min (w, h)
  val PNGFILENAME = "data/romedalen.png"
  val image = cairo_image_surface_create_from_png (PNGFILENAME)
  val iw = cairo_image_surface_get_width (image)
  val iw = int1_of_int (iw)
  val ih = cairo_image_surface_get_height (image)
  val ih = int1_of_int (ih)
  val () = cairo_translate (cr, (w-iw)/2.0, (h-ih)/2.0)
//
  val () = blur_image_surface (image, max(iw,ih)/2)
//
  val () = cairo_set_source_surface (cr, image, 0.0, 0.0)
  val () = cairo_surface_destroy (image)
  val () = cairo_paint (cr)
} // end of [draw]

(* ****** ****** *)

staload "contrib/glib/SATS/glib.sats"
staload "contrib/glib/SATS/glib-object.sats"

(* ****** ****** *)

staload "contrib/GTK/SATS/gdk.sats"
staload "contrib/GTK/SATS/gtkclassdec.sats"
staload "contrib/GTK/SATS/gtk.sats"

(* ****** ****** *)

%{^
extern
ats_void_type mainats (ats_int_type argc, ats_ptr_type argv) ;
%} // end of [%{^]

(* ****** ****** *)

fun on_expose_event
  {c:cls | c <= GtkDrawingArea} {l:agz}
  (darea: !gobjref (c, l), event: &GdkEvent): gboolean = let
//
  prval () = clstrans {c,GtkDrawingArea,GtkWidget} ()
//
  val (fpf_win | win) = gtk_widget_get_window (darea)
  val () = assert_errmsg (g_object_isnot_null (win), #LOCATION)
  val cr = gdk_cairo_create (win)
  prval () = minus_addback (fpf_win, win | darea)
  val (pf, fpf | p) = gtk_widget_getref_allocation (darea)
  val () = draw (cr, (int_of)p->width, (int_of)p->height)
  prval () = minus_addback (fpf, pf | darea)
  val () = cairo_destroy (cr)
in
  GFALSE // HX: what does this mean?
end // end of [on_expose_event]

(* ****** ****** *)

extern fun main1 (): void = "main1"

implement main1 () = () where {
  val window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  val () = gtk_window_set_default_size (window, (gint)400, (gint)400)
//
  val (fpf_x | x) = (gstring_of_string)"cairo: blurring"
  val () = gtk_window_set_title (window, x)
  prval () = fpf_x (x)
//
  val darea = gtk_drawing_area_new ()
  val () = gtk_container_add (window, darea)
  val _sid = g_signal_connect
    (darea, (gsignal)"expose-event", G_CALLBACK (on_expose_event), (gpointer)null)
  val () = g_object_unref (darea)
  val _sid = g_signal_connect
    (window, (gsignal)"delete-event", G_CALLBACK (gtk_main_quit), (gpointer)null)
  val _sid = g_signal_connect
    (window, (gsignal)"destroy-event", G_CALLBACK (gtk_widget_destroy), (gpointer)null)
  val () = gtk_widget_show_all (window)
  val () = g_object_unref (window) // ref-count becomes 1!
  val () = gtk_main ()
} // end of [val]

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

(* end of [blurring.dats] *)
