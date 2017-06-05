#include "config.h"
#include <string.h>
#include <stdlib.h>

#include <gtk/gtk.h>
#include <gtksourceview/gtksourcebuffer.h>
#include <gtksourceview/gtksourceview.h>

static void
test_get_buffer (void)
{
	GtkWidget* view;
	GtkSourceBuffer* buffer;

	view = gtk_source_view_new ();

	buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));

	g_assert (buffer != NULL);
	g_assert (GTK_SOURCE_IS_BUFFER (buffer));

	if (g_object_is_floating (view))
	{
		g_object_ref_sink (view);
	}

	/* Here we check if notify_buffer recreates the buffer while view is being
	 * destroyed, which causes assertion failure in GtkTextView's finalize ()
	 * function.
	 * Please see: https://bugzilla.gnome.org/show_bug.cgi?id=634510 */
	if (g_test_trap_fork (0, 0))
	{
		g_object_unref (view);
		exit (EXIT_SUCCESS);
	}
	g_test_trap_assert_passed ();
}

int
main (int argc, char** argv)
{
	gtk_test_init (&argc, &argv);

	g_test_add_func ("/Buffer/bug-634510", test_get_buffer);

	return g_test_run();
}
