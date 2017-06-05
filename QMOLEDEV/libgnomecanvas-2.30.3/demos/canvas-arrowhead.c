#include <config.h>
#include <stdio.h>
#include <math.h>
#include "canvas_demo.h"

#define LEFT    50.0
#define RIGHT  350.0
#define MIDDLE 150.0
#define DEFAULT_WIDTH   2
#define DEFAULT_SHAPE_A 8
#define DEFAULT_SHAPE_B 10
#define DEFAULT_SHAPE_C 3


static void
set_dimension (GnomeCanvas *canvas, char *arrow_name, char *text_name,
	       double x1, double y1, double x2, double y2, double tx, double ty, int dim)
{
	GnomeCanvasPoints *points;
	char buf[100];

	points = gnome_canvas_points_new (2);
	points->coords[0] = x1;
	points->coords[1] = y1;
	points->coords[2] = x2;
	points->coords[3] = y2;

	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), arrow_name),
			       "points", points,
			       NULL);
	sprintf (buf, "%d", dim);
	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), text_name),
			       "text", buf,
			       "x", tx,
			       "y", ty,
			       NULL);

	gnome_canvas_points_free (points);
}

static void
move_drag_box (GnomeCanvasItem *item, double x, double y)
{
	gnome_canvas_item_set (item,
			       "x1", x - 5.0,
			       "y1", y - 5.0,
			       "x2", x + 5.0,
			       "y2", y + 5.0,
			       NULL);
}

static void
set_arrow_shape (GnomeCanvas *canvas)
{
	int width;
	int shape_a, shape_b, shape_c;
	GnomeCanvasPoints *points;
	char buf[100];

	width = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (canvas), "width"));
	shape_a = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (canvas), "shape_a"));
	shape_b = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (canvas), "shape_b"));
	shape_c = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (canvas), "shape_c"));

	/* Big arrow */

	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "big_arrow"),
			       "width_pixels", 10 * width,
			       "arrow_shape_a", (double) (shape_a * 10),
			       "arrow_shape_b", (double) (shape_b * 10),
			       "arrow_shape_c", (double) (shape_c * 10),
			       NULL);

	/* Outline */

	points = gnome_canvas_points_new (5);
	points->coords[0] = RIGHT - 10 * shape_a;
	points->coords[1] = MIDDLE;
	points->coords[2] = RIGHT - 10 * shape_b;
	points->coords[3] = MIDDLE - 10 * (shape_c + width / 2.0);
	points->coords[4] = RIGHT;
	points->coords[5] = MIDDLE;
	points->coords[6] = points->coords[2];
	points->coords[7] = MIDDLE + 10 * (shape_c + width / 2.0);
	points->coords[8] = points->coords[0];
	points->coords[9] = points->coords[1];
	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "outline"),
			       "points", points,
			       NULL);
	gnome_canvas_points_free (points);

	/* Drag boxes */

	move_drag_box (g_object_get_data (G_OBJECT (canvas), "width_drag_box"),
		       LEFT,
		       MIDDLE - 10 * width / 2.0);

	move_drag_box (g_object_get_data (G_OBJECT (canvas), "shape_a_drag_box"),
		       RIGHT - 10 * shape_a,
		       MIDDLE);

	move_drag_box (g_object_get_data (G_OBJECT (canvas), "shape_b_c_drag_box"),
		       RIGHT - 10 * shape_b,
		       MIDDLE - 10 * (shape_c + width / 2.0));

	/* Dimensions */

	set_dimension (canvas, "width_arrow", "width_text",
		       LEFT - 10,
		       MIDDLE - 10 * width / 2.0,
		       LEFT - 10,
		       MIDDLE + 10 * width / 2.0,
		       LEFT - 15,
		       MIDDLE,
		       width);

	set_dimension (canvas, "shape_a_arrow", "shape_a_text",
		       RIGHT - 10 * shape_a,
		       MIDDLE + 10 * (width / 2.0 + shape_c) + 10,
		       RIGHT,
		       MIDDLE + 10 * (width / 2.0 + shape_c) + 10,
		       RIGHT - 10 * shape_a / 2.0,
		       MIDDLE + 10 * (width / 2.0 + shape_c) + 15,
		       shape_a);

	set_dimension (canvas, "shape_b_arrow", "shape_b_text",
		       RIGHT - 10 * shape_b,
		       MIDDLE + 10 * (width / 2.0 + shape_c) + 35,
		       RIGHT,
		       MIDDLE + 10 * (width / 2.0 + shape_c) + 35,
		       RIGHT - 10 * shape_b / 2.0,
		       MIDDLE + 10 * (width / 2.0 + shape_c) + 40,
		       shape_b);

	set_dimension (canvas, "shape_c_arrow", "shape_c_text",
		       RIGHT + 10,
		       MIDDLE - 10 * width / 2.0,
		       RIGHT + 10,
		       MIDDLE - 10 * (width / 2.0 + shape_c),
		       RIGHT + 15,
		       MIDDLE - 10 * (width / 2.0 + shape_c / 2.0),
		       shape_c);

	/* Info */

	sprintf (buf, "width: %d", width);
	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "width_info"),
			       "text", buf,
			       NULL);

	sprintf (buf, "arrow_shape_a: %d", shape_a);
	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "shape_a_info"),
			       "text", buf,
			       NULL);

	sprintf (buf, "arrow_shape_b: %d", shape_b);
	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "shape_b_info"),
			       "text", buf,
			       NULL);
	sprintf (buf, "arrow_shape_c: %d", shape_c);
	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "shape_c_info"),
			       "text", buf,
			       NULL);

	/* Sample arrows */

	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "sample_1"),
			       "width_pixels", width,
			       "arrow_shape_a", (double) shape_a,
			       "arrow_shape_b", (double) shape_b,
			       "arrow_shape_c", (double) shape_c,
			       NULL);
	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "sample_2"),
			       "width_pixels", width,
			       "arrow_shape_a", (double) shape_a,
			       "arrow_shape_b", (double) shape_b,
			       "arrow_shape_c", (double) shape_c,
			       NULL);
	gnome_canvas_item_set (g_object_get_data (G_OBJECT (canvas), "sample_3"),
			       "width_pixels", width,
			       "arrow_shape_a", (double) shape_a,
			       "arrow_shape_b", (double) shape_b,
			       "arrow_shape_c", (double) shape_c,
			       NULL);
}

static gint
highlight_box (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
	GdkCursor *fleur;

	switch (event->type) {
	case GDK_ENTER_NOTIFY:
		gnome_canvas_item_set (item,
				       "fill_color", "red",
				       NULL);
		break;

	case GDK_LEAVE_NOTIFY:
		if (!(event->crossing.state & GDK_BUTTON1_MASK))
			gnome_canvas_item_set (item,
					       "fill_color", NULL,
					       NULL);
		break;

	case GDK_BUTTON_PRESS:
		fleur = gdk_cursor_new (GDK_FLEUR);
		gnome_canvas_item_grab (item,
					GDK_POINTER_MOTION_MASK | GDK_BUTTON_RELEASE_MASK,
					fleur,
					event->button.time);
		gdk_cursor_unref (fleur);
		break;

	case GDK_BUTTON_RELEASE:
		gnome_canvas_item_ungrab (item, event->button.time);
		break;

	default:
		break;
	}

	return FALSE;
}

static void
create_drag_box (GnomeCanvasGroup *root, char *box_name, GCallback callback)
{
	GnomeCanvasItem *box;

	box = gnome_canvas_item_new (root,
				     gnome_canvas_rect_get_type (),
				     "fill_color", NULL,
				     "outline_color", "black",
				     "width_pixels", 0,
				     NULL);
	g_signal_connect (box, "event",
			  G_CALLBACK (highlight_box),
			  NULL);
	g_signal_connect (box, "event",
			  callback,
			  NULL);

	g_object_set_data (G_OBJECT (root->item.canvas), box_name, box);
}

static gint
width_event (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
	int y;
	int width;

	if ((event->type != GDK_MOTION_NOTIFY) || !(event->motion.state & GDK_BUTTON1_MASK))
		return FALSE;

	y = event->motion.y;
	width = (MIDDLE - y) / 5;
	if (width < 0)
		return FALSE;

	g_object_set_data (G_OBJECT (item->canvas), "width", GINT_TO_POINTER (width));
	set_arrow_shape (item->canvas);

	return FALSE;
}

static gint
shape_a_event (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
	int x;
	int shape_a;

	if ((event->type != GDK_MOTION_NOTIFY) || !(event->motion.state & GDK_BUTTON1_MASK))
		return FALSE;

	x = event->motion.x;
	shape_a = (RIGHT - x) / 10;
	if ((shape_a < 0) || (shape_a > 30))
		return FALSE;

	g_object_set_data (G_OBJECT (item->canvas), "shape_a", GINT_TO_POINTER (shape_a));
	set_arrow_shape (item->canvas);

	return FALSE;
}

static gint
shape_b_c_event (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
	int x, y;
	int width, shape_b, shape_c;
	int change;

	if ((event->type != GDK_MOTION_NOTIFY) || !(event->motion.state & GDK_BUTTON1_MASK))
		return FALSE;

	change = FALSE;

	x = event->motion.x;
	shape_b = (RIGHT - x) / 10;
	if ((shape_b >= 0) && (shape_b <= 30)) {
		g_object_set_data (G_OBJECT (item->canvas), "shape_b", GINT_TO_POINTER (shape_b));
		change = TRUE;
	}

	y = event->motion.y;
	width = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (item->canvas), "width"));
	shape_c = ((MIDDLE - 5 * width) - y) / 10;
	if (shape_c >= 0) {
		g_object_set_data (G_OBJECT (item->canvas), "shape_c", GINT_TO_POINTER (shape_c));
		change = TRUE;
	}

	if (change)
		set_arrow_shape (item->canvas);

	return FALSE;
}

static void
create_dimension (GnomeCanvasGroup *root, char *arrow_name, char *text_name, GtkAnchorType anchor)
{
	GnomeCanvasItem *item;

	item = gnome_canvas_item_new (root,
				      gnome_canvas_line_get_type (),
				      "fill_color", "black",
				      "first_arrowhead", TRUE,
				      "last_arrowhead", TRUE,
				      "arrow_shape_a", 5.0,
				      "arrow_shape_b", 5.0,
				      "arrow_shape_c", 3.0,
				      NULL);
	g_object_set_data (G_OBJECT (root->item.canvas), arrow_name, item);

	item = gnome_canvas_item_new (root,
				      gnome_canvas_text_get_type (),
				      "fill_color", "black",
				      "font", "Sans 12",
				      "anchor", anchor,
				      NULL);
	g_object_set_data (G_OBJECT (root->item.canvas), text_name, item);
}

static void
create_info (GnomeCanvasGroup *root, char *info_name, double x, double y)
{
	GnomeCanvasItem *item;

	item = gnome_canvas_item_new (root,
				      gnome_canvas_text_get_type (),
				      "x", x,
				      "y", y,
				      "fill_color", "black",
				      "font", "Sans 14",
				      "anchor", GTK_ANCHOR_NW,
				      NULL);
	g_object_set_data (G_OBJECT (root->item.canvas), info_name, item);
}

static void
create_sample_arrow (GnomeCanvasGroup *root, char *sample_name, double x1, double y1, double x2, double y2)
{
	GnomeCanvasItem *item;
	GnomeCanvasPoints *points;

	points = gnome_canvas_points_new (2);
	points->coords[0] = x1;
	points->coords[1] = y1;
	points->coords[2] = x2;
	points->coords[3] = y2;
	item = gnome_canvas_item_new (root,
				      gnome_canvas_line_get_type (),
				      "points", points,
				      "fill_color", "black",
				      "first_arrowhead", TRUE,
				      "last_arrowhead", TRUE,
				      NULL);
	g_object_set_data (G_OBJECT (root->item.canvas), sample_name, item);
	gnome_canvas_points_free (points);
}

GtkWidget *
create_canvas_arrowhead (void)
{
	GtkWidget *vbox;
	GtkWidget *w;
	GtkWidget *frame;
	GtkWidget *canvas;
	GnomeCanvasGroup *root;
	GnomeCanvasItem *item;
	GnomeCanvasPoints *points;

	vbox = gtk_vbox_new (FALSE, 4);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), 4);
	gtk_widget_show (vbox);

	w = gtk_label_new ("This demo allows you to edit arrowhead shapes.  Drag the little boxes\n"
			   "to change the shape of the line and its arrowhead.  You can see the\n"
			   "arrows at their normal scale on the right hand side of the window.");
	gtk_box_pack_start (GTK_BOX (vbox), w, FALSE, FALSE, 0);
	gtk_widget_show (w);

	w = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_box_pack_start (GTK_BOX (vbox), w, TRUE, TRUE, 0);
	gtk_widget_show (w);

	frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);
	gtk_container_add (GTK_CONTAINER (w), frame);
	gtk_widget_show (frame);

	canvas = gnome_canvas_new ();
	gtk_widget_set_size_request (canvas, 500, 350);
	gnome_canvas_set_scroll_region (GNOME_CANVAS (canvas), 0, 0, 500, 350);
	gtk_container_add (GTK_CONTAINER (frame), canvas);
	gtk_widget_show (canvas);

	root = gnome_canvas_root (GNOME_CANVAS (canvas));

	g_object_set_data (G_OBJECT (canvas), "width", GINT_TO_POINTER (DEFAULT_WIDTH));
	g_object_set_data (G_OBJECT (canvas), "shape_a", GINT_TO_POINTER (DEFAULT_SHAPE_A));
	g_object_set_data (G_OBJECT (canvas), "shape_b", GINT_TO_POINTER (DEFAULT_SHAPE_B));
	g_object_set_data (G_OBJECT (canvas), "shape_c", GINT_TO_POINTER (DEFAULT_SHAPE_C));

	/* Big arrow */

	points = gnome_canvas_points_new (2);
	points->coords[0] = LEFT;
	points->coords[1] = MIDDLE;
	points->coords[2] = RIGHT;
	points->coords[3] = MIDDLE;

	item = gnome_canvas_item_new (root,
				      gnome_canvas_line_get_type (),
				      "points", points,
				      "fill_color", "mediumseagreen",
				      "width_pixels", DEFAULT_WIDTH * 10,
				      "last_arrowhead", TRUE,
				      NULL);
	g_object_set_data (G_OBJECT (canvas), "big_arrow", item);
	gnome_canvas_points_free (points);

	/* Arrow outline */

	item = gnome_canvas_item_new (root,
				      gnome_canvas_line_get_type (),
				      "fill_color", "black",
				      "width_pixels", 2,
				      "cap_style", GDK_CAP_ROUND,
				      "join_style", GDK_JOIN_ROUND,
				      NULL);
	g_object_set_data (G_OBJECT (canvas), "outline", item);

	/* Drag boxes */

	create_drag_box (root, "width_drag_box", G_CALLBACK (width_event));
	create_drag_box (root, "shape_a_drag_box", G_CALLBACK (shape_a_event));
	create_drag_box (root, "shape_b_c_drag_box", G_CALLBACK (shape_b_c_event));

	/* Dimensions */

	create_dimension (root, "width_arrow", "width_text", GTK_ANCHOR_E);
	create_dimension (root, "shape_a_arrow", "shape_a_text", GTK_ANCHOR_N);
	create_dimension (root, "shape_b_arrow", "shape_b_text", GTK_ANCHOR_N);
	create_dimension (root, "shape_c_arrow", "shape_c_text", GTK_ANCHOR_W);

	/* Info */

	create_info (root, "width_info", LEFT, 260);
	create_info (root, "shape_a_info", LEFT, 280);
	create_info (root, "shape_b_info", LEFT, 300);
	create_info (root, "shape_c_info", LEFT, 320);

	/* Division line */

	points = gnome_canvas_points_new (2);
	points->coords[0] = RIGHT + 50;
	points->coords[1] = 0;
	points->coords[2] = points->coords[0];
	points->coords[3] = 1000;
	gnome_canvas_item_new (root, gnome_canvas_line_get_type (),
			       "points", points,
			       "fill_color", "black",
			       "width_pixels", 2,
			       NULL);

	/* Sample arrows */

	create_sample_arrow (root, "sample_1", RIGHT + 100, 30, RIGHT + 100, MIDDLE - 30);
	create_sample_arrow (root, "sample_2", RIGHT + 70, MIDDLE, RIGHT + 130, MIDDLE);
	create_sample_arrow (root, "sample_3", RIGHT + 70, MIDDLE + 30, RIGHT + 130, MIDDLE + 120);

	/* Done! */
	
	set_arrow_shape (GNOME_CANVAS (canvas));
	return vbox;
}
