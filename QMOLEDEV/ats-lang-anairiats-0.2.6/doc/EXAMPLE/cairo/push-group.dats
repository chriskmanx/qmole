//
// Cairo: Test cairo_push_group
// The code is directly translated from on the attached C code
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2010
//

(* ****** ****** *)

staload MATH = "libc/SATS/math.sats"
macdef PI = $MATH.M_PI

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

#define UNIT_SIZE 100.0
#define PAD 5.0
#define INNER_PAD 10.0

#define WIDTH (UNIT_SIZE + PAD) + PAD
#define HEIGHT (UNIT_SIZE + PAD) + PAD

(* ****** ****** *)

macdef double = double_of_int

(* ****** ****** *)

fun draw {l:agz} (
    cr: !cairo_ref l, width: int, height: int
  ) : void = () where {
//
  val w = (double)width
  val h = (double)height
  val xalpha =  w / WIDTH
  val yalpha =  h / HEIGHT
//
(*
//
// HX: 2010-05-17:
// cairo_scale (cr, xalpha, yalpha) cannot be called here! it looks
// like a bug in cairo.
//
  val () = (print "xalpha = "; print xalpha; print_newline ())
  val () = (print "yalpha = "; print yalpha; print_newline ())
*)
//
  val gradient = cairo_pattern_create_linear
    (UNIT_SIZE - 2*INNER_PAD, 0.0, UNIT_SIZE - 2*INNER_PAD, UNIT_SIZE - 2*INNER_PAD)
  val () = cairo_pattern_add_color_stop_rgba (gradient, 0.0, 0.3, 0.3, 0.3, 1.0)
  val () = cairo_pattern_add_color_stop_rgba (gradient, 1.0, 1.0, 1.0, 1.0, 1.0)
//
  var j: int // uninitalized
  val () = for (j := 0; j < 1; j := j + 1) let
    var i: int // unintialized
    val () = for (i := 0; i < 1; i := i + 1) let
      val (pf1 | ()) = cairo_save (cr)
      val x = (i * UNIT_SIZE) + (i + 1) * PAD
      val y = (j * UNIT_SIZE) + (j + 1) * PAD
      val () = cairo_translate (cr, xalpha * x, yalpha * y)
// draw a gradient background
      val (pf2 | ()) = cairo_save (cr)
      val () = cairo_scale (cr, xalpha, yalpha)
      val () = cairo_translate (cr, INNER_PAD, INNER_PAD)
      val () = cairo_new_path (cr)
      val () = cairo_rectangle
        (cr, 0.0, 0.0, UNIT_SIZE - 2*INNER_PAD, UNIT_SIZE - 2*INNER_PAD)
      val () = cairo_set_source (cr, gradient)
      val () = cairo_fill (cr)
      val () = cairo_restore (pf2 | cr)
//
      val (pf2 | ()) = cairo_save (cr)
      val () = cairo_scale (cr, xalpha, yalpha)
      val () = cairo_rectangle (cr, 0.0, 0.0, UNIT_SIZE, UNIT_SIZE)
      val () = cairo_clip (cr)
      val () = cairo_rectangle (cr, 0.0, 0.0, UNIT_SIZE, UNIT_SIZE)
      val () = cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 1.0)
      val () = cairo_set_line_width (cr, 2.0)
      val () = cairo_stroke (cr)
      val () = cairo_restore (pf2 | cr)
//
//    testing [cairo_push_group]
//
      val (pf2 | ()) = cairo_push_group (cr)
//
      val () = cairo_scale (cr, xalpha, yalpha)
//
//    draw a diamond
//
      val () = cairo_move_to (cr, UNIT_SIZE / 2, 0.0);
      val () = cairo_line_to (cr, UNIT_SIZE, UNIT_SIZE / 2);
      val () = cairo_line_to (cr, UNIT_SIZE / 2, UNIT_SIZE)
      val () = cairo_line_to (cr, 0.0, UNIT_SIZE / 2)
      val () = cairo_close_path (cr)
      val () = cairo_set_source_rgba (cr, 0.0, 0.0, 1.0, 1.0)
      val () = cairo_fill (cr)
//
//    draw a circle
//
      val () = cairo_arc (
        cr, UNIT_SIZE / 2, UNIT_SIZE / 2, UNIT_SIZE / 3.5, 0.0, 2*PI
      ) // end of [val]
      val () = cairo_set_source_rgba (cr, 1.0, 0.0, 0.0, 1.0)
      val () = cairo_fill (cr)
//
      val () = cairo_pop_group_to_source (pf2 | cr)
//
      val () = cairo_paint_with_alpha (cr, 0.5)
      val () = cairo_restore (pf1 | cr)
    in
      // nothing
    end // end of [val]
  in
    // nothing
  end // end of [val]
//
  val () = cairo_pattern_destroy (gradient)
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
ats_void_type
mainats (ats_int_type argc, ats_ptr_type argv) ;
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
  val (fpf_x | x) = (gstring_of_string)"cairo: Knockout Groups"
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

(* end of [push-group.dats] *)

////

/*
 * Copyright © 2005 Mozilla Corporation
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

#define UNIT_SIZE 100
#define PAD 5
#define INNER_PAD 10

#define WIDTH (UNIT_SIZE + PAD) + PAD
#define HEIGHT (UNIT_SIZE + PAD) + PAD

static cairo_test_draw_function_t draw;

static const cairo_test_t test = {
    "push-group",
    "Verify that cairo_push_group works.",
    WIDTH, HEIGHT,
    draw
};

static cairo_test_status_t
draw (cairo_t *cr, int width, int height)
{
    cairo_pattern_t *gradient;
    int i, j;

    gradient = cairo_pattern_create_linear (UNIT_SIZE - (INNER_PAD*2), 0,
                                            UNIT_SIZE - (INNER_PAD*2), UNIT_SIZE - (INNER_PAD*2));
    cairo_pattern_add_color_stop_rgba (gradient, 0.0, 0.3, 0.3, 0.3, 1.0);
    cairo_pattern_add_color_stop_rgba (gradient, 1.0, 1.0, 1.0, 1.0, 1.0);

    for (j = 0; j < 1; j++) {
        for (i = 0; i < 1; i++) {
            double x = (i * UNIT_SIZE) + (i + 1) * PAD;
            double y = (j * UNIT_SIZE) + (j + 1) * PAD;

            cairo_save (cr);

            cairo_translate (cr, x, y);

            /* draw a gradient background */
            cairo_save (cr);
            cairo_translate (cr, INNER_PAD, INNER_PAD);
            cairo_new_path (cr);
            cairo_rectangle (cr, 0, 0,
                             UNIT_SIZE - (INNER_PAD*2), UNIT_SIZE - (INNER_PAD*2));
            cairo_set_source (cr, gradient);
            cairo_fill (cr);
            cairo_restore (cr);

            /* clip to the unit size */
            cairo_rectangle (cr, 0, 0,
                             UNIT_SIZE, UNIT_SIZE);
            cairo_clip (cr);

            cairo_rectangle (cr, 0, 0,
                             UNIT_SIZE, UNIT_SIZE);
            cairo_set_source_rgba (cr, 0, 0, 0, 1);
            cairo_set_line_width (cr, 2);
            cairo_stroke (cr);

            /* start a group */
            cairo_push_group (cr);

            /* draw diamond */
            cairo_move_to (cr, UNIT_SIZE / 2, 0);
            cairo_line_to (cr, UNIT_SIZE    , UNIT_SIZE / 2);
            cairo_line_to (cr, UNIT_SIZE / 2, UNIT_SIZE);
            cairo_line_to (cr, 0            , UNIT_SIZE / 2);
            cairo_close_path (cr);
            cairo_set_source_rgba (cr, 0, 0, 1, 1);
            cairo_fill (cr);

            /* draw circle */
            cairo_arc (cr,
                       UNIT_SIZE / 2, UNIT_SIZE / 2,
                       UNIT_SIZE / 3.5,
                       0, M_PI * 2);
            cairo_set_source_rgba (cr, 1, 0, 0, 1);
            cairo_fill (cr);

            cairo_pop_group_to_source (cr);
            cairo_paint_with_alpha (cr, 0.5);

            cairo_restore (cr);
        }
    }

    cairo_pattern_destroy (gradient);

    return CAIRO_TEST_SUCCESS;
}

int
main (void)
{
    return cairo_test (&test);
}
