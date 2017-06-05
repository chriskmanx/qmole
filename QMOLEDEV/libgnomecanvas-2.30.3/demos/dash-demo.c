/* This file is part of GNOME Canvas
 *
 * AUTHORS
 *     Sven Herzberg  <herzi@gnome-de.org>
 *
 * Copyright (C) 2006  Sven Herzberg
 *
 * This work is provided "as is"; redistribution and modification
 * in whole or in part, in any medium, physical or electronic is
 * permitted without restriction.
 * 		
 * This work is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 		
 * In no event shall the authors or contributors be liable for any
 * direct, indirect, incidental, special, exemplary, or consequential
 * damages (including, but not limited to, procurement of substitute
 * goods or services; loss of use, data, or profits; or business
 * interruption) however caused and on any theory of liability, whether
 * in contract, strict liability, or tort (including negligence or
 * otherwise) arising in any way out of the use of this software, even
 * if advised of the possibility of such damage.
 */

#include <gtk/gtk.h>

#include <libart_lgpl/art_vpath_dash.h>
#include <libgnomecanvas/libgnomecanvas.h>

int
main(int argc, char** argv)
{
	GtkWidget      * window;
	GtkWidget      * canvas;
	GnomeCanvasItem* ellipsis;
	gdouble          dashes[] = {
		10.0, 10.0, 20.0, 10.0
	};
	ArtVpathDash  dash = {
		5.0,
		4,
		dashes
	};

	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	g_signal_connect(window, "destroy",
			 G_CALLBACK(gtk_main_quit), NULL);
	canvas = gnome_canvas_new_aa();
	ellipsis = gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(canvas)),
					 GNOME_TYPE_CANVAS_ELLIPSE,
					 "x1", 10.0,
					 "y1", 10.0,
					 "x2", 90.0,
					 "y2", 90.0,
					 "outline-color", "darkred",
					 "dash", &dash,
					 NULL);
	gtk_container_add(GTK_CONTAINER(window), canvas);
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}

