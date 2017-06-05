#include <config.h>
#include <math.h>
#include "canvas_demo.h"

static gboolean
quit_cb (GtkWidget *widget, GdkEventAny *event, gpointer dummy)
{
	gtk_main_quit ();

	return TRUE;
}

static void
create_canvas (void)
{
	GtkWidget *app;
	GtkWidget *notebook;

/* 	gtk_debug_flags = GTK_DEBUG_OBJECTS; */

	app = gtk_window_new (GTK_WINDOW_TOPLEVEL);

	g_signal_connect (app, "delete_event",
			  G_CALLBACK (quit_cb), NULL);

	notebook = gtk_notebook_new ();
	gtk_widget_show (notebook);

	gtk_container_add (GTK_CONTAINER (app), notebook);

	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), create_canvas_primitives (0), gtk_label_new ("Primitives"));
    	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), create_canvas_primitives (1), gtk_label_new ("Antialias"));  
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), create_canvas_arrowhead (), gtk_label_new ("Arrowhead"));
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), create_canvas_fifteen (), gtk_label_new ("Fifteen"));
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), create_canvas_features (), gtk_label_new ("Features"));
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), create_canvas_rich_text (), gtk_label_new ("Rich Text"));
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), create_canvas_bezier_curve (), gtk_label_new ("Bezier Curve"));

	gtk_widget_show (app);
}

int
main (int argc, char *argv[])
{
	gtk_init (&argc, &argv);

	free (malloc (8)); /* encourage -lefence to link */

	create_canvas ();

	gtk_main ();

	return 0;
}
