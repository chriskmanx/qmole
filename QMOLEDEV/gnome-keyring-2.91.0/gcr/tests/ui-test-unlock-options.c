
#include "config.h"

#include "gcr-unlock-options-widget.h"

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
test_unlock_options (void)
{
	GcrUnlockOptionsWidget *unlock;
	GtkDialog *dialog;

	dialog = GTK_DIALOG (gtk_dialog_new ());
	g_object_ref_sink (dialog);

	unlock = GCR_UNLOCK_OPTIONS_WIDGET (gcr_unlock_options_widget_new ());
	gtk_widget_show (GTK_WIDGET (unlock));
	gtk_container_add (GTK_CONTAINER (gtk_dialog_get_content_area (dialog)), GTK_WIDGET (unlock));

	gcr_unlock_options_widget_set_sensitive (unlock, GCR_UNLOCK_OPTION_IDLE, FALSE,
	                                         "This is a test of the tooltip.");
	gcr_unlock_options_widget_set_sensitive (unlock, GCR_UNLOCK_OPTION_ALWAYS, TRUE, NULL);
	gcr_unlock_options_widget_set_label (unlock, GCR_UNLOCK_OPTION_IDLE, "Disabled label test");
	gcr_unlock_options_widget_set_ttl (unlock, 80);
	gcr_unlock_options_widget_set_choice (unlock, GCR_UNLOCK_OPTION_ALWAYS);

	gtk_window_set_default_size (GTK_WINDOW (dialog), 400, 400);
	gtk_dialog_run (dialog);

	g_printerr ("choice: %s\n",
	            gcr_unlock_options_widget_get_choice (unlock));

	g_printerr ("ttl: %u\n",
	            gcr_unlock_options_widget_get_ttl (unlock));

	g_printerr ("idle sensitive: %s\n",
	            gcr_unlock_options_widget_get_sensitive (unlock, GCR_UNLOCK_OPTION_IDLE) ? "T" : "F");

	g_printerr ("always sensitive: %s\n",
	            gcr_unlock_options_widget_get_sensitive (unlock, GCR_UNLOCK_OPTION_ALWAYS) ? "T" : "F");

	g_printerr ("label: %s\n",
	            gcr_unlock_options_widget_get_label (unlock, GCR_UNLOCK_OPTION_ALWAYS));

	g_object_unref (dialog);
}

int
main(int argc, char *argv[])
{
	gtk_init (&argc, &argv);

	chdir_base_dir (argv[0]);
	test_unlock_options ();

	return 0;
}
