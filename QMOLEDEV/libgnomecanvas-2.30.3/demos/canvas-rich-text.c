#include <config.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include "canvas_demo.h"

static void
setup_text (GnomeCanvasGroup *root)
{
	gnome_canvas_item_new (root,
			       gnome_canvas_rect_get_type (),
			       "x1", -90.0,
			       "y1", -50.0,
			       "x2", 110.0,
			       "y2", 50.0,
			       "fill_color", "green",
			       "outline_color", "green",
			       NULL);

	gnome_canvas_item_new (root,
			       gnome_canvas_rich_text_get_type (),
			       "x", -90.0,
			       "y", -50.0,
			       "width", 200.0,
			       "height", 100.0,
			       "text", 
			       "English is so boring because everyone uses it.\n"
			       "Here is something exciting:  "
			       "وقد بدأ ثلاث من أكثر المؤسسات تقدما في شبكة اكسيون برامجها كمنظمات لا تسعى للربح، ثم تحولت في السنوات الخمس الماضية إلى مؤسسات مالية منظمة، وباتت جزءا من النظام المالي في بلدانها، ولكنها تتخصص في خدمة قطاع المشروعات الصغيرة. وأحد أكثر هذه المؤسسات نجاحا هو »بانكوسول« في بوليفيا.\n"
			       "And here is some more plain, boring English.",
			       "grow_height", TRUE,
			       NULL);

	gnome_canvas_item_new (root,
			       gnome_canvas_ellipse_get_type (),
			       "x1", -5.0,
			       "y1", -5.0,
			       "x2", 5.0,
			       "y2", 5.0,
			       "fill_color", "white",
			       NULL);

	gnome_canvas_item_new (root,
			       gnome_canvas_rect_get_type (),
			       "x1", 100.0,
			       "y1", -30.0,
			       "x2", 200.0,
			       "y2", 30.0,
			       "fill_color", "yellow",
			       "outline_color", "yellow",
			       NULL);

	gnome_canvas_item_new (root,
			       gnome_canvas_rich_text_get_type (),
			       "x", 100.0,
			       "y", -30.0,
			       "width", 100.0,
			       "height", 60.0,
			       "text", "The quick brown fox jumped over the lazy dog.\n",
			       "cursor_visible", TRUE,
			       "cursor_blink", TRUE,
			       "grow_height", TRUE, 
			       NULL);

	gnome_canvas_item_new (root,
			       gnome_canvas_rect_get_type (),
			       "x1", 50.0,
			       "y1", 70.0,
			       "x2", 150.0,
			       "y2", 100.0,
			       "fill_color", "pink",
			       "outline_color", "pink",
			       NULL);

	gnome_canvas_item_new (root,
			       gnome_canvas_rich_text_get_type (),
			       "x", 50.0,
			       "y", 70.0,
			       "width", 100.0,
			       "height", 30.0,
			       "text", "This is a test.\nI enjoy tests a great deal\nThree lines!",
			       "cursor_visible", TRUE,
			       "cursor_blink", TRUE,
			       NULL);
}

GtkWidget *
create_canvas_rich_text (void)
{
	GtkWidget *vbox;
	GtkWidget *alignment;
	GtkWidget *frame;
	GtkWidget *canvas;
	GnomeCanvasGroup *root;

	vbox = gtk_vbox_new (FALSE, 4);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), 4);
	gtk_widget_show (vbox);

	alignment = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_box_pack_start (GTK_BOX (vbox), alignment, TRUE, TRUE, 0);
	gtk_widget_show (alignment);

	frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_IN);
	gtk_container_add (GTK_CONTAINER (alignment), frame);
	gtk_widget_show (frame);

	/* Create the canvas and board */

	canvas = gnome_canvas_new ();
	gtk_widget_set_size_request (canvas, 600, 450);
	gtk_container_add (GTK_CONTAINER (frame), canvas);
	gtk_widget_show (canvas);

	root = gnome_canvas_root (GNOME_CANVAS (canvas));

	setup_text (root);

	return vbox;
}
