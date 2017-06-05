//
// Cairo: Test text transform
// The code is directly translated from on the attached C code
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2010
//

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/array.dats"

(* ****** ****** *)

staload "libc/SATS/math.sats"

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

#define PAD 5.0
#define SIZE 100.0
#define FONT_SIZE 32.0

(* ****** ****** *)

extern
fun cairo_show_text0 {l:agz}
  (cr: !cairo_ref l, utf8: string): void = "mac#atsctrb_cairo_show_text"
// end of [cairo_show_text]

(* ****** ****** *)

fun draw_text {l:agz}
  (cr: !cairo_ref l): void = () where {
  var tm: cairo_matrix_t // unintialized
//
// skew
//
  val () = cairo_matrix_init (tm, 1., 0., ~0.25, 1., 0., 0.)
  val () = cairo_matrix_scale (tm, FONT_SIZE, FONT_SIZE)
  val () = cairo_set_font_matrix (cr, tm)
  val () = cairo_new_path (cr)
  val () = cairo_move_to (cr, 50., SIZE-PAD)
  val () = cairo_show_text0 (cr, "A")
//
// rotate and scale
//
  val () = cairo_matrix_init_rotate (tm, M_PI / 2)
  val () = cairo_matrix_scale (tm, FONT_SIZE, 2*FONT_SIZE)
  val () = cairo_set_font_matrix (cr, tm)
//
  val () = cairo_new_path (cr)
  val () = cairo_move_to (cr, PAD, PAD + 25)
  val () = cairo_show_text0 (cr, "A")
//
  val () = cairo_matrix_init_rotate (tm, M_PI / 2)
  val () = cairo_matrix_scale (tm, FONT_SIZE * 2.0, FONT_SIZE)
  val () = cairo_set_font_matrix (cr, tm)
//
  val () = cairo_new_path (cr)
  val () = cairo_move_to (cr, PAD, PAD + 50)
  val () = cairo_show_text0 (cr, "A")
//
} // end of [draw_text]

(* ****** ****** *)

fun draw {l:agz}
  (cr: !cairo_ref l, w: int, h: int): void = () where {
//
  val mn = min (w, h)
  val alpha = mn/SIZE
  val () = cairo_translate (cr, (w-mn)/2., (h-mn)/2.)
  val () = cairo_scale (cr, alpha, alpha)
//
  val () = cairo_set_source_rgb (cr, 1., 1., 1.)
  val () = cairo_paint (cr)
  val () = cairo_set_source_rgb (cr, 0., 0., 0.)
  val () = cairo_select_font_face (
    cr, "Bitstream Vera Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL
  ) // end of [val]
  val () = draw_text (cr)
  val () = cairo_translate (cr, SIZE, SIZE)
  val () =  cairo_rotate (cr, M_PI)
//
  val PNGFILENAME = "DATA/romedalen.png"
  val image = cairo_image_surface_create_from_png (PNGFILENAME)
  val pattern = cairo_pattern_create_for_surface (image)
  val () = cairo_surface_destroy (image)
  val () = cairo_pattern_set_extend (pattern, CAIRO_EXTEND_REPEAT)
  val () = cairo_set_source (cr, pattern)
  val () = cairo_pattern_destroy (pattern)
  val () = draw_text (cr)
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

(* end of [text-transform.dats] *)

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

#define SIZE 100
#define PAD 5

#define FONT_SIZE 32.0

static const char png_filename[] = "romedalen.png";

static const cairo_test_t test = {
    "text-transform",
    "Test various applications of the font matrix",
    SIZE, SIZE,
    draw
};

static void
draw_text (cairo_t *cr)
{
    cairo_matrix_t tm;

    /* skew */
    cairo_matrix_init (&tm, 1, 0,
                       -0.25, 1,
                       0, 0);
    cairo_matrix_scale (&tm, FONT_SIZE, FONT_SIZE);
    cairo_set_font_matrix (cr, &tm);

    cairo_new_path (cr);
    cairo_move_to (cr, 50, SIZE-PAD);
    cairo_show_text (cr, "A");

    /* rotate and scale */
    cairo_matrix_init_rotate (&tm, M_PI / 2);
    cairo_matrix_scale (&tm, FONT_SIZE, FONT_SIZE * 2.0);
    cairo_set_font_matrix (cr, &tm);

    cairo_new_path (cr);
    cairo_move_to (cr, PAD, PAD + 25);
    cairo_show_text (cr, "A");

    cairo_matrix_init_rotate (&tm, M_PI / 2);
    cairo_matrix_scale (&tm, FONT_SIZE * 2.0, FONT_SIZE);
    cairo_set_font_matrix (cr, &tm);

    cairo_new_path (cr);
    cairo_move_to (cr, PAD, PAD + 50);
    cairo_show_text (cr, "A");
}

static cairo_test_status_t
draw (cairo_t *cr, int width, int height)
{
    const cairo_test_context_t *ctx = cairo_test_get_context (cr);
    cairo_pattern_t *pattern;

    cairo_set_source_rgb (cr, 1., 1., 1.);
    cairo_paint (cr);

    cairo_set_source_rgb (cr, 0., 0., 0.);

    cairo_select_font_face (cr, "Bitstream Vera Sans",
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_NORMAL);

    draw_text (cr);

    cairo_translate (cr, SIZE, SIZE);
    cairo_rotate (cr, M_PI);

    pattern = cairo_test_create_pattern_from_png (ctx, png_filename);
    cairo_pattern_set_extend (pattern, CAIRO_EXTEND_REPEAT);
    cairo_set_source (cr, pattern);
    cairo_pattern_destroy (pattern);

    draw_text (cr);

    return CAIRO_TEST_SUCCESS;
}

int
main (void)
{
    return cairo_test (&test);
}
