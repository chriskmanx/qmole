/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-pkix-parser.c: Test PKIX parser

   Copyright (C) 2007 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#include "gcr/gcr.h"
#include "gcr/gcr-internal.h"

#include "gck/gck.h"

#include <glib.h>
#include <gcrypt.h>

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
 * Each test looks like (on one line):
 *     void unit_test_xxxxx (CuTest* cu)
 *
 * Each setup looks like (on one line):
 *     void unit_setup_xxxxx (void);
 *
 * Each teardown looks like (on one line):
 *     void unit_teardown_xxxxx (void);
 *
 * Tests be run in the order specified here.
 */

typedef struct {
	GcrParser *parser;
	gchar* filedesc;
} Test;

static void
parsed_item (GcrParser *par, gpointer user_data)
{
	GckAttributes *attrs;
	const gchar *description;
	const gchar *label;
	Test *test = user_data;

	g_assert (GCR_IS_PARSER (par));
	g_assert (par == test->parser);

	attrs = gcr_parser_get_parsed_attributes (test->parser);
	g_assert (attrs);

	description = gcr_parser_get_parsed_description (test->parser);
	label = gcr_parser_get_parsed_label (test->parser);

	g_print ("parsed %s '%s' at: %s\n", description, label, test->filedesc);
}

static gboolean
authenticate (GcrParser *par, gint state, gpointer user_data)
{
	Test *test = user_data;

	g_assert (GCR_IS_PARSER (par));
	g_assert (par == test->parser);

	switch (state) {
	case 0:
		gcr_parser_add_password (test->parser, "booo");
		return TRUE;
	default:
		g_printerr ("decryption didn't work for: %s", test->filedesc);
		g_assert_not_reached ();
		return FALSE;
	};
}

static void
setup (Test *test, gconstpointer unused)
{
	test->parser = gcr_parser_new ();
	g_signal_connect (test->parser, "parsed", G_CALLBACK (parsed_item), test);
	g_signal_connect (test->parser, "authenticate", G_CALLBACK (authenticate), test);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->parser);
	g_free (test->filedesc);
}

static void
test_parse_all (Test *test, gconstpointer unused)
{
	gchar *contents;
	GError *err = NULL;
	gboolean result;
	const gchar *filename;
	gsize len;
	GDir *dir;

	dir = g_dir_open (SRCDIR "/files", 0, NULL);
	g_assert (dir);

	for (;;) {
		filename = g_dir_read_name (dir);
		if (!filename)
			break;
		if (filename[0] == '.')
			continue;

		g_free (test->filedesc);
		test->filedesc = g_build_filename (SRCDIR "/files", filename, NULL);

		if (g_file_test (test->filedesc, G_FILE_TEST_IS_DIR))
			continue;

		if (!g_file_get_contents (test->filedesc, &contents, &len, NULL))
			g_assert_not_reached ();

		result = gcr_parser_parse_data (test->parser, contents, len, &err);
		g_free (contents);

		if (!result) {
			g_warning ("couldn't parse file data: %s: %s",
			           filename, egg_error_message (err));
			g_error_free (err);
			g_assert_not_reached ();
		}
	}

	g_dir_close (dir);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("test-parser");

	g_test_add ("/gcr/parser/parse_all", Test, NULL, setup, test_parse_all, teardown);

	return g_test_run ();
}
