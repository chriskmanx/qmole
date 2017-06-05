//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010
//

(* ****** ****** *)

staload MATH = "libc/SATS/math.sats"
macdef PI = $MATH.M_PI
macdef sin = $MATH.sin and cos = $MATH.cos

(* ****** ****** *)

staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

fun oval_path {l:agz} (
    cr: !cairo_ref l
  , xc: double, yc: double
  , xr: double, yr: double
  ) : void = () where {
  val (pf | ()) = cairo_save (cr)
  val () = cairo_translate (cr, xc, yc)
  val () = cairo_scale (cr, 1.0, yr / xr)
  val () = cairo_move_to (cr, xr, 0.0)
  val () = cairo_arc (cr, 0.0, 0.0, xr, 0.0, 2 * PI)
  val () = cairo_close_path (cr)
  val () = cairo_restore (pf | cr)
} // end of [oval_path]

(* ****** ****** *)

#define CHECK_SIZE 32
#define fCHECK_SIZE 32.0

fun fill_checks {l:agz} (
    cr: !cairo_ref l, x: int, y: int, width: int, height: int
  ) : void = () where {
  #define i2d double_of_int
  val () = cairo_rectangle (cr, (i2d)x, (i2d)y, (i2d)width, (i2d)height)
  val () = cairo_set_source_rgb (cr, 0.4, 0.4, 0.4)
  val () = cairo_fill (cr)
  val x0 = x mod CHECK_SIZE and y0 = y mod CHECK_SIZE
  val () = loop1 (cr, y0, 0) where {
    fun loop1
      (cr: !cairo_ref l, y: int, b: int):<cloref1> void =
      if y < height then let
        val () = loop2 (cr, y, x0, b) in loop1 (cr, y+CHECK_SIZE, 1-b)
      end // end of [if]
    // end of [loop1]
    and loop2 (cr: !cairo_ref l, y: int, x: int, b: int):<cloref1> void =
      if x < width then let
        val () = if (b > 0) then
          cairo_rectangle (cr, (i2d)x, (i2d)y, fCHECK_SIZE, fCHECK_SIZE)
      in
        loop2 (cr, y, x + CHECK_SIZE, 1-b)
      end // end of [if]
    // end of [loop2]
 } // end of [val]
  val () = cairo_set_source_rgb (cr, 0.7, 0.7, 0.7)
  val () = cairo_fill (cr)
} // end of [fill_checks]

(* ****** ****** *)

fun draw_3circles {l:agz} (
    cr: !cairo_ref l, xc: double, yc: double, radius: double, alpha: double
  ) : void = () where {
//
  val subradius = radius * (2 / 3. - 0.1)
//
  val () = cairo_set_source_rgba (cr, 1., 0., 0., alpha)
  val () = oval_path (
    cr
  , xc + radius / 3. * cos (PI * (0.5))
  , yc - radius / 3. * sin (PI * (0.5))
  , subradius, subradius
  ) // end of [val]
  val () = cairo_fill (cr)
//
  val () = cairo_set_source_rgba (cr, 0., 1., 0., alpha)
  val () = oval_path (
    cr
  , xc + radius / 3. * cos (PI * (0.5 + 2/0.3))
  , yc - radius / 3. * sin (PI * (0.5 + 2/0.3))
  , subradius, subradius
  ) // end of [val]
  val () = cairo_fill (cr)
//
  val () = cairo_set_source_rgba (cr, 0., 0., 1., alpha);
  val () = oval_path (
    cr
  , xc + radius / 3. * cos (PI * (0.5 + 4/0.3))
  , yc - radius / 3. * sin (PI * (0.5 + 4/0.3))
  , subradius, subradius
  ) // end of [val]
  val () = cairo_fill (cr)
//
} // end of [draw_3circles]

(* ****** ****** *)

extern fun draw_main {l:agz}
  (cr: !cairo_ref l, width: int, height: int) : void = "draw_main"
implement draw_main
  (cr, width, height) = () where {
  val w = (double_of)width
  val h = (double_of)height
  val radius = min (w, h) / 2 - 10
  val xc = w / 2 and yc = h / 2

  val (fpf_sf | sf) = cairo_get_target (cr)
  val overlay = cairo_surface_create_similar (sf, CAIRO_CONTENT_COLOR_ALPHA, width, height)
  val punch = cairo_surface_create_similar (sf, CAIRO_CONTENT_ALPHA, width, height)
  val circles = cairo_surface_create_similar (sf, CAIRO_CONTENT_COLOR_ALPHA, width, height)
  prval () = minus_addback (fpf_sf, sf | cr)
  val () = fill_checks (cr, 0, 0, width, height)
//
  val cr_overlay = cairo_create (overlay)
  val () = cairo_set_source_rgb (cr_overlay, 0., 0., 0.)
  val () = oval_path (cr_overlay, xc, yc, radius, radius)
  val () = cairo_fill (cr_overlay) 
//
  val cr_punch = cairo_create (punch);
  val () = draw_3circles (cr_punch, xc, yc, radius, 1.0)
  val () = cairo_destroy (cr_punch)
//
  val () = cairo_set_operator (cr_overlay, CAIRO_OPERATOR_DEST_OUT)
  val () = cairo_set_source_surface (cr_overlay, punch, 0.0, 0.0)
  val () = cairo_paint (cr_overlay)
//
  val cr_circles = cairo_create (circles)
  val () = cairo_set_operator (cr_circles, CAIRO_OPERATOR_OVER)
  val () = draw_3circles (cr_circles, xc, yc, radius, 0.5)
  val () = cairo_destroy (cr_circles)
//
  val () = cairo_set_operator (cr_overlay, CAIRO_OPERATOR_ADD)
  val () = cairo_set_source_surface (cr_overlay, circles, 0.0, 0.0)
  val () = cairo_paint (cr_overlay)
//
  val () = cairo_destroy (cr_overlay)
  val () = cairo_set_source_surface (cr, overlay, 0.0, 0.0)
  val () = cairo_paint (cr)
//
  val () = cairo_surface_destroy (overlay)
  val () = cairo_surface_destroy (punch)
  val () = cairo_surface_destroy (circles)
} // end of [draw_main]

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
  val () = draw_main (cr, (int_of)p->width, (int_of)p->height)
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

(* end of [testcairo.dats] *)

////

/* testimage.c
 * Copyright (C) 2005  Red Hat, Inc.
 * Based on cairo-demo/X11/cairo-knockout.c
 *
 * Author: Owen Taylor
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <math.h>

#include <gtk/gtk.h>

static void
oval_path (cairo_t *cr,
           double xc, double yc,
           double xr, double yr)
{
  cairo_save (cr);

  cairo_translate (cr, xc, yc);
  cairo_scale (cr, 1.0, yr / xr);
  cairo_move_to (cr, xr, 0.0);
  cairo_arc (cr,
	     0, 0,
	     xr,
	     0, 2 * G_PI);
  cairo_close_path (cr);

  cairo_restore (cr);
}

/* Create a path that is a circular oval with radii xr, yr at xc,
 * yc.
 */
/* Fill the given area with checks in the standard style
 * for showing compositing effects.
 *
 * It would make sense to do this as a repeating surface,
 * but most implementations of RENDER currently have broken
 * implementations of repeat + transform, even when the
 * transform is a translation.
 */
static void
fill_checks (cairo_t *cr,
             int x,     int y,
             int width, int height)
{
  int i, j;
  
#define CHECK_SIZE 32

  cairo_rectangle (cr, x, y, width, height);
  cairo_set_source_rgb (cr, 0.4, 0.4, 0.4);
  cairo_fill (cr);

  /* Only works for CHECK_SIZE a power of 2 */
  j = x & (-CHECK_SIZE);
  
  for (; j < height; j += CHECK_SIZE)
    {
      i = y & (-CHECK_SIZE);
      for (; i < width; i += CHECK_SIZE)
	if ((i / CHECK_SIZE + j / CHECK_SIZE) % 2 == 0)
	  cairo_rectangle (cr, i, j, CHECK_SIZE, CHECK_SIZE);
    }

  cairo_set_source_rgb (cr, 0.7, 0.7, 0.7);
  cairo_fill (cr);
}

/* Draw a red, green, and blue circle equally spaced inside
 * the larger circle of radius r at (xc, yc)
 */
static void
draw_3circles (cairo_t *cr,
               double xc, double yc,
               double radius,
	       double alpha)
{
  double subradius = radius * (2 / 3. - 0.1);
    
  cairo_set_source_rgba (cr, 1., 0., 0., alpha);
  oval_path (cr,
	     xc + radius / 3. * cos (G_PI * (0.5)),
	     yc - radius / 3. * sin (G_PI * (0.5)),
	     subradius, subradius);
  cairo_fill (cr);
    
  cairo_set_source_rgba (cr, 0., 1., 0., alpha);
  oval_path (cr,
	     xc + radius / 3. * cos (G_PI * (0.5 + 2/.3)),
	     yc - radius / 3. * sin (G_PI * (0.5 + 2/.3)),
	     subradius, subradius);
  cairo_fill (cr);
    
  cairo_set_source_rgba (cr, 0., 0., 1., alpha);
  oval_path (cr,
	     xc + radius / 3. * cos (G_PI * (0.5 + 4/.3)),
	     yc - radius / 3. * sin (G_PI * (0.5 + 4/.3)),
	     subradius, subradius);
  cairo_fill (cr);
}

static void
draw (cairo_t *cr,
      int      width,
      int      height)
{
  cairo_surface_t *overlay, *punch, *circles;
  cairo_t *overlay_cr, *punch_cr, *circles_cr;

  /* Fill the background */
  double radius = 0.5 * (width < height ? width : height) - 10;
  double xc = width / 2.;
  double yc = height / 2.;

  overlay = cairo_surface_create_similar (cairo_get_target (cr),
					  CAIRO_CONTENT_COLOR_ALPHA,
					  width, height);
  if (overlay == NULL)
    return;

  punch = cairo_surface_create_similar (cairo_get_target (cr),
					CAIRO_CONTENT_ALPHA,
					width, height);
  if (punch == NULL)
    return;

  circles = cairo_surface_create_similar (cairo_get_target (cr),
					  CAIRO_CONTENT_COLOR_ALPHA,
					  width, height);
  if (circles == NULL)
    return;
    
  fill_checks (cr, 0, 0, width, height);

  /* Draw a black circle on the overlay
   */
  overlay_cr = cairo_create (overlay);
  cairo_set_source_rgb (overlay_cr, 0., 0., 0.);
  oval_path (overlay_cr, xc, yc, radius, radius);
  cairo_fill (overlay_cr);

  /* Draw 3 circles to the punch surface, then cut
   * that out of the main circle in the overlay
   */
  punch_cr = cairo_create (punch);
  draw_3circles (punch_cr, xc, yc, radius, 1.0);
  cairo_destroy (punch_cr);

  cairo_set_operator (overlay_cr, CAIRO_OPERATOR_DEST_OUT);
  cairo_set_source_surface (overlay_cr, punch, 0, 0);
  cairo_paint (overlay_cr);

  /* Now draw the 3 circles in a subgroup again
   * at half intensity, and use OperatorAdd to join up
   * without seams.
   */
  circles_cr = cairo_create (circles);
  
  cairo_set_operator (circles_cr, CAIRO_OPERATOR_OVER);
  draw_3circles (circles_cr, xc, yc, radius, 0.5);
  cairo_destroy (circles_cr);

  cairo_set_operator (overlay_cr, CAIRO_OPERATOR_ADD);
  cairo_set_source_surface (overlay_cr, circles, 0, 0);
  cairo_paint (overlay_cr);

  cairo_destroy (overlay_cr);

  cairo_set_source_surface (cr, overlay, 0, 0);
  cairo_paint (cr);

  cairo_surface_destroy (overlay);
  cairo_surface_destroy (punch);
  cairo_surface_destroy (circles);
}

static gboolean
on_expose_event (GtkWidget      *widget,
		 GdkEventExpose *event,
		 gpointer        data)
{
  cairo_t *cr;

  cr = gdk_cairo_create (widget->window);

  draw (cr, widget->allocation.width, widget->allocation.height);

  cairo_destroy (cr);

  return FALSE;
}

int
main (int argc, char **argv)
{
  GtkWidget *window, *darea;

  gtk_init (&argc, &argv);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  
  gtk_window_set_default_size (GTK_WINDOW (window), 400, 400);
  gtk_window_set_title (GTK_WINDOW (window), "cairo: Knockout Groups");

  darea = gtk_drawing_area_new ();
  gtk_container_add (GTK_CONTAINER (window), darea);

  g_signal_connect (darea, "expose-event",
		    G_CALLBACK (on_expose_event), NULL);
  g_signal_connect (window, "destroy-event",
		    G_CALLBACK (gtk_main_quit), NULL);

  gtk_widget_show_all (window);
  
  gtk_main ();

  return 0;
}
