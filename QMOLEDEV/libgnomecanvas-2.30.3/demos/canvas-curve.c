/*
 * canvas-curve.c: bezier curve demo.
 *
 * Copyright (C) 2002 Mark McLoughlin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * Authors:
 *     Mark McLoughlin <mark@skynet.ie>
 */

#include <config.h>
#include "canvas_demo.h"

static gboolean item_event   (GnomeCanvasItem *item,
			      GdkEvent        *event,
			      gpointer         dummy);

static gboolean canvas_event (GnomeCanvasItem *item,
			      GdkEvent        *event,
			      gpointer         dummy);

typedef enum {
	STATE_INIT = 0,
	STATE_FIRST_PRESS,
	STATE_FIRST_RELEASE,
	STATE_SECOND_PRESS
} State;

static State              current_state;
static GnomeCanvasItem   *current_item;
static GnomeCanvasPoints *current_points;

static void
draw_curve (GnomeCanvasItem *item,
	    gdouble          x,
	    gdouble          y)
{
	GnomeCanvasPathDef *path_def;
	GnomeCanvasGroup   *root;

	root = GNOME_CANVAS_GROUP (item->parent);

	switch (current_state) {
	case STATE_INIT: 
		g_assert (!current_item);

		if (!current_points)
			current_points = gnome_canvas_points_new (4);

		current_points->coords [0] = x;
		current_points->coords [1] = y;
		break;
	case STATE_FIRST_PRESS:
		current_points->coords [2] = x;
		current_points->coords [3] = y;

	        path_def = gnome_canvas_path_def_new ();

		gnome_canvas_path_def_moveto (
				path_def,
				current_points->coords [0], current_points->coords [1]);

		gnome_canvas_path_def_lineto (
				path_def,
				current_points->coords [2], current_points->coords [3]);

		if (current_item)
			g_object_set (G_OBJECT (current_item), "bpath", path_def, NULL);
		else {
			current_item = gnome_canvas_item_new (
						root,
						gnome_canvas_bpath_get_type(),
						"bpath", path_def,
						"outline_color", "blue",
						"width_pixels", 5,
						"cap_style", GDK_CAP_ROUND,
						NULL);

			g_signal_connect (G_OBJECT (current_item), "event",
					  G_CALLBACK (item_event), NULL);
		}

		gnome_canvas_path_def_unref (path_def);
		break;
	case STATE_FIRST_RELEASE:
		g_assert (current_item);

		current_points->coords [4] = x;
		current_points->coords [5] = y;

	        path_def = gnome_canvas_path_def_new ();

		gnome_canvas_path_def_moveto (
				path_def,
				current_points->coords [0], current_points->coords [1]);

		gnome_canvas_path_def_curveto (
				path_def,
				current_points->coords [4], current_points->coords [5],
				current_points->coords [4], current_points->coords [5],
				current_points->coords [2], current_points->coords [3]);

		g_object_set (G_OBJECT (current_item), "bpath", path_def, NULL);

		gnome_canvas_path_def_unref (path_def);
		break;
	case STATE_SECOND_PRESS:
		g_assert (current_item);

		current_points->coords [6] = x;
		current_points->coords [7] = y;

	        path_def = gnome_canvas_path_def_new ();

		gnome_canvas_path_def_moveto (
				path_def,
				current_points->coords [0], current_points->coords [1]);

		gnome_canvas_path_def_curveto (
				path_def,
				current_points->coords [4], current_points->coords [5],
				current_points->coords [6], current_points->coords [7],
				current_points->coords [2], current_points->coords [3]);

		g_object_set (G_OBJECT (current_item), "bpath", path_def, NULL);

		gnome_canvas_path_def_unref (path_def);

		current_item = NULL;
		break;
	default:
		g_assert_not_reached ();
		break;
	}
}

static gboolean
item_event (GnomeCanvasItem *item,
	    GdkEvent        *event,
	    gpointer         dummy)
{
	if (event->type == GDK_BUTTON_PRESS &&
	    event->button.button == 1 &&
	    event->button.state & GDK_SHIFT_MASK) {
		gtk_object_destroy (GTK_OBJECT (item));

		if (item == current_item) {
			current_item = NULL;
			current_state = STATE_INIT;
		}

		return TRUE;
	}

	return FALSE;
}

static gboolean
canvas_event (GnomeCanvasItem *item,
	      GdkEvent        *event,
	      gpointer         dummy)
{
	switch (event->type) {
	case GDK_BUTTON_PRESS:
		if (event->button.button != 1)
			break;

		switch (current_state) {
		case STATE_INIT:
			draw_curve (item, event->button.x, event->button.y);
			current_state = STATE_FIRST_PRESS;
			break;
		case STATE_FIRST_RELEASE:
			draw_curve (item, event->button.x, event->button.y);
			current_state = STATE_SECOND_PRESS;
			break;
		case STATE_SECOND_PRESS:
			draw_curve (item, event->button.x, event->button.y);
			current_state = STATE_INIT;
			break;
		default:
			g_warning ("shouldn't have reached here %d", current_state);
			break;
		}
		break;
	case GDK_BUTTON_RELEASE:
		if (event->button.button != 1)
			break;

		switch (current_state) {
		case STATE_FIRST_PRESS:
			draw_curve (item, event->button.x, event->button.y);
			current_state = STATE_FIRST_RELEASE;
			break;
		default:
			break;
		}
		break;
	case GDK_MOTION_NOTIFY:
		switch (current_state) {
		case STATE_FIRST_PRESS:
			draw_curve (item, event->motion.x, event->motion.y);
			break;
		default:
			break;
		}
		break;
	default:
		break;
	}

	return FALSE;
}

static GtkWidget *
create_canvas (gboolean aa)
{
	GnomeCanvasGroup *root;
	GnomeCanvasItem  *item;
	GtkWidget        *canvas = NULL;
	GtkWidget        *frame;

	gtk_widget_push_colormap (gdk_rgb_get_colormap ());

	if (aa)
		canvas = gnome_canvas_new_aa ();
	else
		canvas = gnome_canvas_new ();

	gtk_widget_set_size_request (canvas, 600, 250);
	gnome_canvas_set_scroll_region (GNOME_CANVAS (canvas), 0, 0, 600, 250);
	gtk_widget_show (canvas);

	root = gnome_canvas_root (GNOME_CANVAS (canvas));

	item = gnome_canvas_item_new (root,
				      gnome_canvas_rect_get_type (),
				      "outline_color", "black",
				      "fill_color", "white",
				      "x1", 0.0,
				      "y1", 0.0,
				      "x2", 600.0,
				      "y2", 250.0,
				      NULL);

	gnome_canvas_item_new (root,
			       gnome_canvas_text_get_type (),
			       "text", aa ? "AntiAlias" : "Non-AntiAlias",
			       "x", 270.0,
			       "y", 5.0,
			       "font", "Sans 12",
			       "anchor", GTK_ANCHOR_N,
			       "fill_color", "black",
			       NULL);

	gtk_widget_pop_colormap ();

	g_signal_connect (G_OBJECT (item), "event", G_CALLBACK (canvas_event), NULL);

	frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);

	gtk_container_add (GTK_CONTAINER (frame), canvas);

	return frame;
}

GtkWidget *
create_canvas_bezier_curve (void)
{
	GtkWidget *vbox;
	GtkWidget *label;
	GtkWidget *aa_canvas;
	GtkWidget *canvas;

	vbox = gtk_vbox_new (FALSE, 4);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), 4);
	gtk_widget_show (vbox);

	label = gtk_label_new ("Drag a line with button 1. Then mark 2 control points wth\n"
			       "button 1. Shift+click with button 1 to destroy the curve.\n");
	gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
	gtk_widget_show (label);

	canvas = create_canvas (FALSE);
	gtk_box_pack_start (GTK_BOX (vbox), canvas, TRUE, TRUE, 0);
	gtk_widget_show (canvas);

	aa_canvas = create_canvas (TRUE);
	gtk_box_pack_start (GTK_BOX (vbox), aa_canvas, TRUE, TRUE, 0);
	gtk_widget_show (aa_canvas);

	return vbox;
}
