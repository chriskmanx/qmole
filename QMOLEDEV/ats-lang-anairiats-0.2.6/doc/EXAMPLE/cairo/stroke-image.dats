//
// Cairo: Test stroke with an image source
// The code is directly translated from on the attached C code
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2010
//

(* ****** ****** *)

staload MATH = "libc/SATS/math.sats"
macdef PI = $MATH.M_PI
macdef sin = $MATH.sin and cos = $MATH.cos

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

#define PAD 10
#define SIZE 100
#define IMAGE_SIZE (SIZE-PAD*2)
#define LINE_WIDTH 10

(* ****** ****** *)

macdef double = double_of_int

fun draw {l:agz} (
    cr: !cairo_ref l, width: int, height: int
  ) : void = () where {
//
  val w = (double)width
  val h = (double)height
  val mn = min (w, h)
  val alpha = mn / SIZE
//
  val () = cairo_translate
    (cr, (w - mn)/2, (h - mn)/2)
  val () = cairo_scale (cr, alpha, alpha)
//
  val () = cairo_set_source_rgb (cr, 0.0, 0.0, 0.0)
  val ()= cairo_paint (cr)
  val image = cairo_image_surface_create
    (CAIRO_FORMAT_RGB24, IMAGE_SIZE, IMAGE_SIZE)
  val cr_image = cairo_create (image)
  val () = cairo_surface_destroy (image)
// Create the image
  val () = cairo_set_source_rgb (cr_image, 0.0, 0.0, 0.0)
  val () = cairo_paint (cr_image)
  val () = cairo_set_source_rgb (cr_image, 0.0, 1.0, 0.0)
  val () = cairo_set_line_width (cr_image, (double)LINE_WIDTH)
  val () = cairo_arc (
    cr_image
  , (double)IMAGE_SIZE/2, (double)IMAGE_SIZE/2
  , (double)IMAGE_SIZE/2 - (double)LINE_WIDTH/2
  , 0.0, 2*PI
  ) // end of [val]
  val () = cairo_stroke (cr_image)
// Now stroke with it
  val () = cairo_translate (cr, (double)PAD, (double)PAD)
  val (fpf_sf | sf) = cairo_get_target (cr_image)
  val () = cairo_set_source_surface (cr, sf, 0.0, 0.0)
  prval () = minus_addback (fpf_sf, sf | cr_image)
  val () = cairo_destroy (cr_image)
//
  val () = cairo_new_path (cr)
  val () = cairo_set_line_width (cr, (double)LINE_WIDTH)
  val () = cairo_arc (
    cr
  , (double)IMAGE_SIZE/2, (double)IMAGE_SIZE/2
  , (double)IMAGE_SIZE/2 - (double)LINE_WIDTH/2
  , 0.0, 2*PI
  ) // end of [val]
  val () = cairo_stroke (cr)
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
  val (fpf_x | x) = (gstring_of_string)"cairo: stroke-image"
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

(* end of [stroke-image.dats] *)

////

/*
 * Copyright © 2006 Mozilla Corporation
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of
 * Mozilla Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. Mozilla Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * MOZILLA CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL MOZILLA CORPORATION BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author: Vladimir Vukicevic <vladimir@pobox.com>
 */

#include "cairo-test.h"

static cairo_test_draw_function_t draw;

#define PAD 10
#define SIZE 100
#define IMAGE_SIZE (SIZE-PAD*2)
#define LINE_WIDTH 10

static const cairo_test_t test = {
    "stroke-image",
    "Test stroking with an image source, with a non-identity CTM",
    SIZE, SIZE,
    draw
};

static cairo_test_status_t
draw (cairo_t *cr, int width, int height)
{
    cairo_surface_t *image;
    cairo_t *cr_image;

    cairo_set_source_rgb (cr, 0, 0, 0);
    cairo_paint (cr);

    image = cairo_image_surface_create (CAIRO_FORMAT_RGB24, IMAGE_SIZE, IMAGE_SIZE);
    cr_image = cairo_create (image);
    cairo_surface_destroy (image);

    /* Create the image */
    cairo_set_source_rgb (cr_image, 0, 0, 0);
    cairo_paint (cr_image);
    cairo_set_source_rgb (cr_image, 0, 1, 0);
    cairo_set_line_width (cr_image, LINE_WIDTH);
    cairo_arc (cr_image, IMAGE_SIZE/2, IMAGE_SIZE/2, IMAGE_SIZE/2 - LINE_WIDTH/2, 0, M_PI * 2.0);
    cairo_stroke (cr_image);

    /* Now stroke with it */
    cairo_translate (cr, PAD, PAD);

    cairo_set_source_surface (cr, cairo_get_target (cr_image), 0, 0);
    cairo_destroy (cr_image);

    cairo_new_path (cr);
    cairo_set_line_width (cr, LINE_WIDTH);
    cairo_arc (cr, IMAGE_SIZE/2, IMAGE_SIZE/2, IMAGE_SIZE/2 - LINE_WIDTH/2, 0, M_PI * 2.0);
    cairo_stroke (cr);

    return CAIRO_TEST_SUCCESS;
}

int
main (void)
{
    return cairo_test (&test);
}
