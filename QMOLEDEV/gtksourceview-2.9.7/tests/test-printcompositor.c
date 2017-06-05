#include "config.h"
#include <string.h>

#include <gtk/gtk.h>
#include <gtksourceview/gtksourceprintcompositor.h>

static void
test_buffer_ref (void)
{
	GtkSourcePrintCompositor *compositor;
	GtkSourceBuffer *buffer = NULL;
	GtkSourceBuffer *buffer_original = NULL;

	buffer_original = gtk_source_buffer_new (NULL);

	compositor = gtk_source_print_compositor_new (buffer_original);
	buffer = gtk_source_print_compositor_get_buffer (compositor);
	g_assert (GTK_IS_SOURCE_BUFFER (buffer));

	g_object_unref (G_OBJECT (buffer_original));
	buffer = gtk_source_print_compositor_get_buffer (compositor);
	g_assert (GTK_IS_SOURCE_BUFFER (buffer));
}

static void
test_buffer_view_ref (void)
{
	GtkSourcePrintCompositor *compositor;
	GtkWidget *view = NULL;
	GtkSourceBuffer *buffer = NULL;

	view = gtk_source_view_new ();
	compositor = gtk_source_print_compositor_new_from_view (GTK_SOURCE_VIEW (view));
	buffer = gtk_source_print_compositor_get_buffer (compositor);
	g_assert (GTK_IS_SOURCE_BUFFER (buffer));

	gtk_widget_destroy (view);
	buffer = gtk_source_print_compositor_get_buffer (compositor);
	g_assert (GTK_IS_SOURCE_BUFFER (buffer));

	g_object_unref (G_OBJECT (compositor));
}

int
main (int argc, char** argv)
{
	gtk_test_init (&argc, &argv);

	g_test_add_func ("/PrintCompositor/buffer-ref", test_buffer_ref);
	g_test_add_func ("/PrintCompositor/buffer-view-ref", test_buffer_view_ref);

	return g_test_run();
}
