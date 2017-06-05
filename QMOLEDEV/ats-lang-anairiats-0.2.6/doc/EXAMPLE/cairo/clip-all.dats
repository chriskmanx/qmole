//
// Cairo: Test clipping with everything clipped out
// The code is directly translated from on the attached C code
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2010
//

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

#define SIZE 10.0
#define CLIP_SIZE 2.0

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
  val () = cairo_rectangle (cr, 0.0, 0.0, SIZE, SIZE)
  val () = cairo_set_source_rgb (cr, 0.0, 0.0, 1.0) // blue color
  val () = cairo_fill (cr)
//
  val () = cairo_reset_clip (cr)
  val () = cairo_rectangle (cr, CLIP_SIZE, CLIP_SIZE, CLIP_SIZE, CLIP_SIZE)
  val () = cairo_clip (cr)
  val () = cairo_rectangle (cr, 3*CLIP_SIZE, 3*CLIP_SIZE, CLIP_SIZE, CLIP_SIZE)
  val () = cairo_clip (cr)
//
  val () = cairo_translate (cr, 0.5, 0.5)
  val () = cairo_reset_clip (cr)
  val () = cairo_rectangle (cr, CLIP_SIZE, CLIP_SIZE, CLIP_SIZE, CLIP_SIZE)
  val () = cairo_clip (cr)
  val () = cairo_rectangle (cr, 3*CLIP_SIZE, 3*CLIP_SIZE, CLIP_SIZE, CLIP_SIZE)
  val () = cairo_clip (cr)
//
  val () = cairo_rectangle (cr, 0.0, 0.0, SIZE, SIZE)
  val () = cairo_set_source_rgb (cr, 1.0, 1.0, 0.0) // yellow color
  val () = cairo_fill (cr)
//
  val () = cairo_select_font_face (
    cr, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL
  ) // end of [val]
//
  val () = cairo_move_to (cr, 0.0, SIZE)
  val () = cairo_show_text (cr, "cairo")
//
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
  val (fpf_x | x) = (gstring_of_string)"cairo: clip-all"
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

(* end of [clip-all.dats] *)

////

/*
 * Copyright © 2005 Novell, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of
 * Novell, Inc. not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. Novell, Inc. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NOVELL, INC. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL NOVELL, INC. BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author: Radek Doulík <rodo@novell.com>
 */

#include "cairo-test.h"

#define SIZE 10
#define CLIP_SIZE 2

static cairo_test_draw_function_t draw;

static const cairo_test_t test = {
    "clip-all",
    "Test clipping with everything clipped out",
    SIZE, SIZE,
    draw
};

static cairo_test_status_t
draw (cairo_t *cr, int width, int height)
{
    cairo_rectangle (cr, 0, 0, SIZE, SIZE);
    cairo_set_source_rgb (cr, 0, 0, 1);
    cairo_fill (cr);

    cairo_reset_clip (cr);
    cairo_rectangle (cr, CLIP_SIZE, CLIP_SIZE, CLIP_SIZE, CLIP_SIZE);
    cairo_clip (cr);
    cairo_rectangle (cr, 3*CLIP_SIZE, 3*CLIP_SIZE, CLIP_SIZE, CLIP_SIZE);
    cairo_clip (cr);

    cairo_translate (cr, .5, .5);

    cairo_reset_clip (cr);
    cairo_rectangle (cr, CLIP_SIZE, CLIP_SIZE, CLIP_SIZE, CLIP_SIZE);
    cairo_clip (cr);
    cairo_rectangle (cr, 3*CLIP_SIZE, 3*CLIP_SIZE, CLIP_SIZE, CLIP_SIZE);
    cairo_clip (cr);

    cairo_rectangle (cr, 0, 0, SIZE, SIZE);
    cairo_set_source_rgb (cr, 1, 1, 0);
    cairo_fill (cr);

    /* https://bugs.freedesktop.org/show_bug.cgi?id=13084 */
    cairo_select_font_face (cr,
	                    "Bitstream Vera Sans",
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_NORMAL);

    cairo_move_to (cr, 0., SIZE);
    cairo_show_text (cr, "cairo");


    return CAIRO_TEST_SUCCESS;
}

int
main (void)
{
    return cairo_test (&test);
}
