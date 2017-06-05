#include <stdlib.h>

#include <gtk/gtk.h>

#include <glade/glade-xml.h>

int
main (int argc, char *argv[])
{
	GladeXML *xml;
	GLogLevelFlags fatal_mask;

	fatal_mask = g_log_set_always_fatal (G_LOG_FATAL_MASK);
	g_log_set_always_fatal (fatal_mask | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL);

	gtk_init (&argc, &argv);

	xml = glade_xml_new (GLADEFILE, NULL, NULL);

	g_assert (xml != NULL);

	if (getenv ("TEST_LIBGLADE_SHOW")) {
		GtkWidget *toplevel;
		toplevel = glade_xml_get_widget (xml, "window1");
		gtk_widget_show_all (toplevel);
		g_signal_connect (G_OBJECT (toplevel), "delete-event", G_CALLBACK (gtk_main_quit), NULL);
		gtk_main ();
	}

	g_object_unref (G_OBJECT (xml));

	return 0;
}
