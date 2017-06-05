#include "config.h"
#include <string.h>

#include <gtk/gtk.h>
#include <gtksourceview/gtksourcelanguagemanager.h>

static void
test_get_default (void)
{
	GtkSourceLanguageManager *lm1, *lm2;

	lm1 = gtk_source_language_manager_get_default ();
	lm2 = gtk_source_language_manager_get_default ();
	g_assert (lm1 == lm2);
}

static void
test_get_language (void)
{
	GtkSourceLanguageManager *lm;
	const gchar * const *ids;

	lm = gtk_source_language_manager_get_default ();
	ids = gtk_source_language_manager_get_language_ids (lm);
	g_assert (ids != NULL);

	while (*ids != NULL)
	{
		GtkSourceLanguage *lang1, *lang2;

		lang1 = gtk_source_language_manager_get_language (lm, *ids);
		g_assert (lang1 != NULL);
		g_assert (GTK_IS_SOURCE_LANGUAGE (lang1));
		g_assert_cmpstr (*ids, == , gtk_source_language_get_id (lang1));

		/* langs are owned by the manager */
		lang2 = gtk_source_language_manager_get_language (lm, *ids);
		g_assert (lang1 == lang2);

		++ids;
	}
}

static void
test_guess_language (void)
{
	GtkSourceLanguageManager *lm;
	GtkSourceLanguage *l;

	lm = gtk_source_language_manager_get_default ();

	if (g_test_trap_fork (0, G_TEST_TRAP_SILENCE_STDERR))
	{
		l = gtk_source_language_manager_guess_language (lm, NULL, NULL);
	}
	g_test_trap_assert_failed ();

	l = gtk_source_language_manager_guess_language (lm, "foo.abcdef", NULL);
	g_assert (l == NULL);

	l = gtk_source_language_manager_guess_language (lm, NULL, "image/png");
	g_assert (l == NULL);

	l = gtk_source_language_manager_guess_language (lm, "foo.c", NULL);
	g_assert_cmpstr (gtk_source_language_get_id (l), ==, "c");

	l = gtk_source_language_manager_guess_language (lm, NULL, "text/x-csrc");
	g_assert_cmpstr (gtk_source_language_get_id (l), ==, "c");

	l = gtk_source_language_manager_guess_language (lm, "foo.c", "text/x-csrc");
	g_assert_cmpstr (gtk_source_language_get_id (l), ==, "c");

	/* when in disagreement, glob wins */
	l = gtk_source_language_manager_guess_language (lm, "foo.c", "text/x-fortran");
	g_assert_cmpstr (gtk_source_language_get_id (l), ==, "c");

	/* when content type is a descendent of the mime matched by the glob, mime wins */
	l = gtk_source_language_manager_guess_language (lm, "foo.xml", "application/xslt+xml");
	g_assert_cmpstr (gtk_source_language_get_id (l), ==, "xslt");
}

int
main (int argc, char** argv)
{
	gtk_test_init (&argc, &argv);

	g_test_add_func ("/LanguageManager/get-default", test_get_default);
	g_test_add_func ("/LanguageManager/get-language", test_get_language);
	g_test_add_func ("/LanguageManager/guess-language", test_guess_language);

	return g_test_run();
}
