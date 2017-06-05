
#include "config.h"

#include "gcr/gcr.h"

#include <gtk/gtk.h>

#include <unistd.h>
#include <string.h>
#include <errno.h>

static void
chdir_base_dir (char* argv0)
{
	gchar *dir, *base;

	dir = g_path_get_dirname (argv0);
	if (chdir (dir) < 0)
		g_warning ("couldn't change directory to: %s: %s",
		           dir, g_strerror (errno));

	base = g_path_get_basename (dir);
	if (strcmp (base, ".libs") == 0) {
		if (chdir ("..") < 0)
			g_warning ("couldn't change directory to ..: %s",
			           g_strerror (errno));
	}

	g_free (base);
	g_free (dir);
}

static void
on_parser_parsed (GcrParser *parser, gpointer user_data)
{
	GcrSimpleCollection *collection = user_data;
	GcrRenderer *renderer;

	renderer = gcr_renderer_create (gcr_parser_get_parsed_label (parser),
	                                gcr_parser_get_parsed_attributes (parser));

	if (renderer) {
		gcr_simple_collection_add (collection, G_OBJECT (renderer));
		g_object_unref (renderer);
	}
}

static void
add_to_selector (GcrParser *parser, const gchar *path)
{
	GError *err = NULL;
	guchar *data;
	gsize n_data;

	if (!g_file_get_contents (path, (gchar**)&data, &n_data, NULL))
		g_error ("couldn't read file: %s", path);

	if (!gcr_parser_parse_data (parser, data, n_data, &err))
		g_error ("couldn't parse data: %s", err->message);

	g_free (data);
}

int
main (int argc, char *argv[])
{
	GcrCollection *collection;
	GcrComboSelector *selector;
	GtkDialog *dialog;
	GcrParser *parser;
	GObject *selected;
	int i;

	gtk_init (&argc, &argv);

	dialog = GTK_DIALOG (gtk_dialog_new ());
	g_object_ref_sink (dialog);

	collection = gcr_simple_collection_new ();
	selector = gcr_combo_selector_new (collection);

	gtk_widget_show (GTK_WIDGET (selector));
	gtk_container_add (GTK_CONTAINER (gtk_dialog_get_content_area (dialog)), GTK_WIDGET (selector));

	gtk_window_set_default_size (GTK_WINDOW (dialog), 550, 400);
	gtk_container_set_border_width (GTK_CONTAINER (dialog), 20);

	parser = gcr_parser_new ();
	g_signal_connect (parser, "parsed", G_CALLBACK (on_parser_parsed), collection);

	if (argc == 1) {
		chdir_base_dir (argv[0]);
		add_to_selector (parser, "files/ca-certificates.crt");
	} else {
		for (i = 1; i < argc; ++i)
			add_to_selector (parser, argv[i]);
	}

	g_object_unref (parser);
	g_object_unref (collection);

	gtk_dialog_run (dialog);

	selected = gcr_combo_selector_get_selected (selector);
	if (selected == NULL) {
		g_print ("nothing selected\n");
	} else {
		gchar *label;
		g_object_get (selected, "label", &label, NULL);
		g_print ("selected: %s\n", label);
	}

	gtk_widget_destroy (GTK_WIDGET (dialog));
	g_object_unref (dialog);

	return 0;
}
