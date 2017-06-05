/* -*- mode: C; c-basic-offset: 4 -*- */
#include <gmodule.h>
#include <gtk/gtk.h>
#include <glade/glade-xml.h>

G_MODULE_EXPORT GtkWidget *test_create(char *s1, char *s2, int i1, int i2, gpointer data);

G_MODULE_EXPORT GtkWidget *
test_create(char *s1, char *s2, int i1, int i2, gpointer data)
{
    return gtk_label_new("Custom Widget");
}

int
main(int argc, char **argv)
{
    GladeXML *xml;
    GLogLevelFlags fatal_mask;

    fatal_mask = g_log_set_always_fatal (G_LOG_FATAL_MASK);
    g_log_set_always_fatal (fatal_mask | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL);

    if (!gtk_init_check (&argc, &argv)) {
	g_message("Could not init gtk.  returning");
	return 77;
    }

    if (argc != 2) {
	g_print("test should be called with a glade2 file as an argument\n");
	return 1;
    }

    xml = glade_xml_new (argv[1], NULL, NULL);

    g_assert (xml != NULL);

    if (g_getenv ("TEST_LIBGLADE_SHOW")) {
	GtkWidget *toplevel;

	toplevel = glade_xml_get_widget (xml, "window1");
	gtk_widget_show_all (toplevel);
	g_signal_connect (G_OBJECT (toplevel), "delete-event", G_CALLBACK (gtk_main_quit), NULL);
	gtk_main ();
    }

    g_object_unref (G_OBJECT (xml));

    return 0;
}
