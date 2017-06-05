
#include "config.h"

#include "gcr-certificate-details-widget.h"
#include "gcr-simple-certificate.h"

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
test_details (const gchar *path)
{
	GcrCertificateDetailsWidget *details;
	GcrCertificate *certificate;
	GtkDialog *dialog;
	guchar *data;
	gsize n_data;
	
	if (!g_file_get_contents (path, (gchar**)&data, &n_data, NULL))
		g_error ("couldn't read file: %s", path);
	
	certificate = gcr_simple_certificate_new (data, n_data);
	g_assert (certificate);
	g_free (data);
	
	dialog = GTK_DIALOG (gtk_dialog_new ());
	g_object_ref_sink (dialog);
	
	details = gcr_certificate_details_widget_new (certificate);
	gtk_widget_show (GTK_WIDGET (details));
	gtk_container_add (GTK_CONTAINER (gtk_dialog_get_content_area (dialog)), GTK_WIDGET (details));

	gtk_window_set_default_size (GTK_WINDOW (dialog), 400, 400);
	gtk_dialog_run (dialog);
	
	g_object_unref (dialog);
	g_object_unref (certificate);
	g_object_unref (details);
}

int
main(int argc, char *argv[])
{
	gtk_init (&argc, &argv);
	
	if (argc > 1) {
		test_details (argv[1]);
	} else {
		chdir_base_dir (argv[0]);
		test_details ("test-data/der-certificate.crt");
	}
	
	return 0;
}
