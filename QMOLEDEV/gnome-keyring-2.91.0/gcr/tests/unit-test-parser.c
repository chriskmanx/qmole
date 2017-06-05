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

#include "test-suite.h"

#include "egg/egg-error.h"
#include "egg/egg-secure-memory.h"

#include "gcr/gcr-parser.h"

#include "gck/gck.h"

#include <glib.h>
#include <gcrypt.h>

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

static GcrParser *parser = NULL;
static const gchar* filedesc = NULL;

static void
parsed_item (GcrParser *par, gpointer user_data)
{
	GckAttributes *attrs;
	const gchar *description;
	const gchar *label;
	
	g_assert (GCR_IS_PARSER (par));
	g_assert (par == parser);
	g_assert (par == user_data);
	
	attrs = gcr_parser_get_parsed_attributes (parser);
	description = gcr_parser_get_parsed_description (parser);
	label = gcr_parser_get_parsed_label (parser);
	
	g_print ("parsed %s '%s' at: %s\n", description, label, filedesc);
}

static gboolean
authenticate (GcrParser *par, gint state, gpointer user_data) 
{
	g_assert (GCR_IS_PARSER (par));
	g_assert (par == parser);
	g_assert (par == user_data);

	switch (state) {
	case 0:
		gcr_parser_add_password (parser, "booo");
		return TRUE;
	default:
		g_printerr ("decryption didn't work for: %s", filedesc);
		g_assert (FALSE);
		return FALSE;
	};
} 

DEFINE_SETUP(parser)
{
	parser = gcr_parser_new ();
	g_signal_connect (parser, "parsed", G_CALLBACK (parsed_item), parser);
	g_signal_connect (parser, "authenticate", G_CALLBACK (authenticate), parser);
}

DEFINE_TEARDOWN(parser)
{
	g_object_unref (parser);
	parser = NULL;
}

DEFINE_TEST(parse_all)
{
	guchar *contents;
	GError *err = NULL;
	gboolean result;
	const gchar *filename;
	gsize len;
	GDir *dir;
	
	dir = g_dir_open (testing_data_directory (), 0, NULL);
	g_assert (dir);

	for (;;) {
		filename = g_dir_read_name (dir);
		if (!filename)
			break;
		if (filename[0] == '.')
			continue;
		
		filedesc = filename;
		contents = testing_data_read (filename, &len);
		
		result = gcr_parser_parse_data (parser, contents, len, &err);
		if (!result) { 
			g_warning ("couldn't parse file data: %s: %s", 
			           filename, egg_error_message (err));
			g_error_free (err);
			g_assert (FALSE);
		}
	}
	
	g_dir_close (dir);
}
