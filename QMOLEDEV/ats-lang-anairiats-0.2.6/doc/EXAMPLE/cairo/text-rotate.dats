//
// Cairo: Test text rotation
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

#define WIDTH  150.0
#define HEIGHT 150.0
#define NUM_TEXT 20
#define TEXT_SIZE 12.0

(* ****** ****** *)

fun draw {l:agz} (
    cr: !cairo_ref l, width: int, height: int
  ) : void = () where {
//
  val xalpha = width/WIDTH
  val yalpha = height/HEIGHT
  val () = cairo_scale (cr, xalpha, yalpha)
//
  val text = "cairo"
  val () = cairo_set_source_rgb (cr, 1., 1., 1.)
  val () = cairo_paint (cr)
  val () = cairo_select_font_face (
    cr, "Bitstream Vera Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL
  ) // end of [val]
  val () = cairo_set_font_size (cr, TEXT_SIZE)
  val font_options = cairo_font_options_create ()
  val () = cairo_get_font_options (cr, font_options)
  val () = cairo_font_options_set_hint_metrics (font_options, CAIRO_HINT_METRICS_OFF)
  val () = cairo_set_font_options (cr, font_options)
  val () = cairo_font_options_destroy (font_options)
  val () = cairo_set_source_rgb (cr, 0., 0., 0.)
  val () = cairo_translate (cr, WIDTH/2.0, HEIGHT/2.0)
  var extents: cairo_text_extents_t
  val () = cairo_text_extents (cr, text, extents)
  var x_off: double and y_off: double
  val () = if (NUM_TEXT = 1) then let
    val () = x_off := 0.0 and () = y_off := 0.0
  in
    // nothing
  end else let
    val () = x_off := floor (0.5 + (extents.height+1) / (2 * tan (M_PI/NUM_TEXT)))
    val () = y_off := ~floor (0.5 + extents.height / 2.0)
  in
    // nothing
  end // end of [if]
  var i: int // uninitialized
  val () = for (i := 0; i < NUM_TEXT; i := i+1) let
    val (pf_save | ()) = cairo_save (cr)
    val () = cairo_rotate (cr, 2*M_PI*i/NUM_TEXT)
    val () = cairo_set_line_width (cr, 1.0)
    val () = cairo_rectangle (cr, x_off - 0.5, y_off - 0.5, extents.width + 1, extents.height + 1)
    val () = cairo_set_source_rgb (cr, 1., 0., 0.)
    val () = cairo_stroke (cr)
    val () = cairo_move_to (cr, x_off - extents.x_bearing, y_off - extents.y_bearing)
    val () = cairo_set_source_rgb (cr, 0., 0., 0.)
    val () = cairo_text_path (cr, text)
    val () = cairo_fill (cr)
    val () = cairo_restore (pf_save | cr)
  in
    // nothing
  end // end of [val]
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

(* end of [text-rotate.dats] *)

////

/*
 * Copyright © 2004 Red Hat, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of
 * Red Hat, Inc. not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. Red Hat, Inc. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * RED HAT, INC. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL RED HAT, INC. BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author: Carl D. Worth <cworth@cworth.org>
 */

/* Bug history
 *
 * 2004-11-03 Steve Chaplin <stevech1097@yahoo.com.au>
 *
 *   Reported bug on mailing list:
 *
 *	From: Steve Chaplin <stevech1097@yahoo.com.au>
 *	To: cairo@cairographics.org
 *	Date: Thu, 04 Nov 2004 00:00:17 +0800
 *	Subject: [cairo] Rotated text bug on drawable target
 *
 * 	The attached file draws text rotated 90 degrees first to a PNG file and
 *	then to a drawable. The PNG file looks fine, the text on the drawable is
 *	unreadable.
 *
 *	Steve
 *
 * 2004-11-03 Carl Worth <cworth@cworth.org>
 *
 *   Looks like the major problems with this bug appeared in the great
 *   font rework between 0.1.23 and 0.2.0. And it looks like we need
 *   to fix the regression test suite to test the xlib target (since
 *   the bug does not show up in the png backend).
 *
 *   Hmm... Actually, things don't look perfect even in the PNG
 *   output. Look at how that 'o' moves around. It's particularly off
 *   in the case where it's rotated by PI.
 *
 *   And I'm still not sure about what to do for test cases with
 *   text--a new version of freetype will change everything. We may
 *   need to add a simple backend for stroked fonts and add a simple
 *   builtin font to cairo for pixel-perfect tests with text.
 *
 * 2005-08-23
 *
 *   It appears that the worst placement and glyph selection problems
 *   have now been resolved. In the past some letters were noticeably
 *   of a different size at some rotations, and there was a lot of
 *   drift away from the baseline. These problems do not appear
 *   anymore.
 *
 *   Another thing that helps is that we now have font options which
 *   we can use to disable hinting in order to get more repeatable
 *   results. I'm doing that in this test now.
 *
 *   There are still some subtle positioning problems which I'm
 *   assuming are due to the lack of finer-than-whole-pixel glyph
 *   positioning. I'm generating a reference image now by replacing
 *   cairo_show_text with cairo_text_path; cairo_fill. This will let
 *   us look more closely at the remaining positioning problems. (In
 *   particular, I want to make sure we're rounding as well as
 *   possible).
 *
 * 2007-02-21
 *
 *   Seems like all the "bugs" have been fixed and all remainint is
 *   missing support for subpixel glyph positioning.  Removing from
 *   XFAIL now.
 */

#include "cairo-test.h"

#define WIDTH  150
#define HEIGHT 150
#define NUM_TEXT 20
#define TEXT_SIZE 12

static cairo_test_draw_function_t draw;

static const cairo_test_t test = {
    "text-rotate",
    "Tests show_text under various rotations",
    WIDTH, HEIGHT,
    draw
};

/* Draw the word cairo at NUM_TEXT different angles */
static cairo_test_status_t
draw (cairo_t *cr, int width, int height)
{
    int i, x_off, y_off;
    cairo_text_extents_t extents;
    cairo_font_options_t *font_options;
    const char text[] = "cairo";

    /* paint white so we don't need separate ref images for
     * RGB24 and ARGB32 */
    cairo_set_source_rgb (cr, 1., 1., 1.);
    cairo_paint (cr);

    cairo_select_font_face (cr, "Bitstream Vera Sans",
			    CAIRO_FONT_SLANT_NORMAL,
			    CAIRO_FONT_WEIGHT_NORMAL);
    cairo_set_font_size (cr, TEXT_SIZE);

    font_options = cairo_font_options_create ();

    cairo_get_font_options (cr, font_options);
    cairo_font_options_set_hint_metrics (font_options, CAIRO_HINT_METRICS_OFF);

    cairo_set_font_options (cr, font_options);
    cairo_font_options_destroy (font_options);

    cairo_set_source_rgb (cr, 0, 0, 0);

    cairo_translate (cr, WIDTH/2.0, HEIGHT/2.0);

    cairo_text_extents (cr, text, &extents);

    if (NUM_TEXT == 1) {
	x_off = y_off = 0;
    } else {
	y_off = - floor (0.5 + extents.height / 2.0);
	x_off = floor (0.5 + (extents.height+1) / (2 * tan (M_PI/NUM_TEXT)));
    }

    for (i=0; i < NUM_TEXT; i++) {
	cairo_save (cr);
	cairo_rotate (cr, 2*M_PI*i/NUM_TEXT);
	cairo_set_line_width (cr, 1.0);
	cairo_rectangle (cr, x_off - 0.5, y_off - 0.5, extents.width + 1, extents.height + 1);
	cairo_set_source_rgb (cr, 1, 0, 0);
	cairo_stroke (cr);
	cairo_move_to (cr, x_off - extents.x_bearing, y_off - extents.y_bearing);
	cairo_set_source_rgb (cr, 0, 0, 0);
#if CAIRO_TEST_GENERATE_REFERENCE_IMAGE
	cairo_text_path (cr, text);
	cairo_fill (cr);
#else
	cairo_show_text (cr, text);
#endif
	cairo_restore (cr);
    }

    return CAIRO_TEST_SUCCESS;
}

int
main (void)
{
    return cairo_test (&test);
}
