
#include "gcr-shooter.h"
#include "gcr.h"

static gpointer
load_gcr_test_file (const gchar *name, gsize *length)
{
	GError *error = NULL;
	gchar *contents;
	gchar *filename;

	filename = g_build_filename (TOPDIR, "gcr", "tests", "files", name, NULL);
	if (!g_file_get_contents (filename, &contents, length, &error))
		g_error ("couldn't read file: %s: %s", filename, error->message);
	g_free (filename);
	return contents;
}

static GcrShooterInfo *
create_certificate_widget (const gchar *name)
{
	GcrCertificate *certificate;
	GtkWidget *widget;
	GtkWidget *align;
	gchar *contents;
	gsize length;

	contents = load_gcr_test_file ("cacert.org.cer", &length);
	certificate = gcr_simple_certificate_new (contents, length);
	g_free (contents);

	widget = GTK_WIDGET (gcr_certificate_widget_new (certificate));
	g_object_unref (certificate);

	align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_container_add (GTK_CONTAINER (align), widget);

	return gcr_shooter_info_new (name, align, GCR_SHOOTER_LARGE);
}

static void
on_parser_key_parsed (GcrParser *parser, gpointer unused)
{
	GckAttributes **attrs = unused;
	*attrs = gck_attributes_ref (gcr_parser_get_parsed_attributes (parser));
}

static GcrShooterInfo *
create_key_widget (const gchar *name)
{
	GError *error = NULL;
	GckAttributes *attrs = NULL;
	GtkWidget *widget;
	GtkWidget *align;
	GcrParser *parser;
	gchar *contents;
	gsize length;

	contents = load_gcr_test_file ("der-dsa-1024.key", &length);
	parser = gcr_parser_new ();
	g_signal_connect (parser, "parsed", G_CALLBACK (on_parser_key_parsed), &attrs);
	if (!gcr_parser_parse_data (parser, contents, length, &error))
		g_error ("couldn't parse data: %s", error->message);
	g_object_unref (parser);
	g_free (contents);

	g_assert (attrs);
	widget = GTK_WIDGET (gcr_key_widget_new (attrs));
	gck_attributes_unref (attrs);

	align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_container_add (GTK_CONTAINER (align), widget);

	return gcr_shooter_info_new (name, align, GCR_SHOOTER_LARGE);
}

static GcrShooterInfo *
create_combo_selector (const gchar *name)
{
	GcrComboSelector *selector;
	GcrCertificate *certificate;
	GcrCollection *collection;
	GtkWidget *align;
	gchar *contents;
	gsize length;

	contents = load_gcr_test_file ("cacert.org.cer", &length);
	certificate = gcr_simple_certificate_new (contents, length);
	g_free (contents);

	collection = gcr_simple_collection_new ();
	gcr_simple_collection_add (GCR_SIMPLE_COLLECTION (collection), G_OBJECT (certificate));

	selector = gcr_combo_selector_new (collection);
	g_object_unref (collection);

	gcr_combo_selector_set_selected (selector, G_OBJECT (certificate));
	g_object_unref (certificate);

	align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_container_add (GTK_CONTAINER (align), GTK_WIDGET (selector));

	return gcr_shooter_info_new (name, align, GCR_SHOOTER_MEDIUM);
}

static GcrShooterInfo *
create_tree_selector (const gchar *name)
{
	GcrTreeSelector *selector;
	GcrCertificate *certificate;
	GcrCollection *collection;
	GtkWidget *align;
	gchar *contents;
	gsize length;
	GList *selected = NULL;

	collection = gcr_simple_collection_new ();
	selector = gcr_tree_selector_new (collection, GCR_CERTIFICATE_COLUMNS);

	contents = load_gcr_test_file ("cacert.org.cer", &length);
	certificate = gcr_simple_certificate_new (contents, length);
	g_free (contents);
	gcr_simple_collection_add (GCR_SIMPLE_COLLECTION (collection), G_OBJECT (certificate));
	selected = g_list_append (selected, certificate);
	gcr_tree_selector_set_selected (selector, selected);
	g_list_free (selected);
	g_object_unref (certificate);

	contents = load_gcr_test_file ("der-certificate-dsa.cer", &length);
	certificate = gcr_simple_certificate_new (contents, length);
	g_free (contents);
	gcr_simple_collection_add (GCR_SIMPLE_COLLECTION (collection), G_OBJECT (certificate));
	g_object_unref (certificate);

	g_object_unref (collection);

	align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_container_add (GTK_CONTAINER (align), GTK_WIDGET (selector));

	return gcr_shooter_info_new (name, align, GCR_SHOOTER_MEDIUM);
}

static GcrShooterInfo *
create_list_selector (const gchar *name)
{
	GcrListSelector *selector;
	GcrCertificate *certificate;
	GcrCollection *collection;
	GtkWidget *align;
	gchar *contents;
	gsize length;
	GList *selected = NULL;

	collection = gcr_simple_collection_new ();
	selector = gcr_list_selector_new (collection);

	contents = load_gcr_test_file ("cacert.org.cer", &length);
	certificate = gcr_simple_certificate_new (contents, length);
	g_free (contents);
	gcr_simple_collection_add (GCR_SIMPLE_COLLECTION (collection), G_OBJECT (certificate));
	selected = g_list_append (selected, certificate);
	gcr_list_selector_set_selected (selector, selected);
	g_list_free (selected);
	g_object_unref (certificate);

	contents = load_gcr_test_file ("der-certificate-dsa.cer", &length);
	certificate = gcr_simple_certificate_new (contents, length);
	g_free (contents);
	gcr_simple_collection_add (GCR_SIMPLE_COLLECTION (collection), G_OBJECT (certificate));
	g_object_unref (certificate);

	g_object_unref (collection);

	align = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
	gtk_container_add (GTK_CONTAINER (align), GTK_WIDGET (selector));

	return gcr_shooter_info_new (name, align, GCR_SHOOTER_MEDIUM);
}


GcrShooterInfo*
gcr_widgets_create (const gchar *name)
{
	g_assert (name);

	if (g_str_equal (name, "certificate-widget"))
		return create_certificate_widget (name);
	else if (g_str_equal (name, "key-widget"))
		return create_key_widget (name);
	else if (g_str_equal (name, "combo-selector"))
		return create_combo_selector (name);
	else if (g_str_equal (name, "tree-selector"))
		return create_tree_selector (name);
	else if (g_str_equal (name, "list-selector"))
		return create_list_selector (name);

	return NULL;
}
