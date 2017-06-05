/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "gcr/gcr.h"

#include <gtk/gtk.h>

#include <unistd.h>
#include <string.h>
#include <errno.h>

static gboolean
dump_certificate (GckAttributes *attrs, const gchar *filename)
{
	GckAttribute *attr;
	GcrCertificate *cert;
	gchar *subject;
	gulong klass;

	if (!gck_attributes_find_ulong (attrs, CKA_CLASS, &klass) ||
	    klass != CKO_CERTIFICATE)
		return FALSE;

	attr = gck_attributes_find (attrs, CKA_VALUE);
	if (!attr)
		return FALSE;

	cert = gcr_simple_certificate_new_static (attr->value, attr->length);
	subject = gcr_certificate_get_subject_dn (cert);
	g_print ("%s: parsed certificate: %s\n", filename, subject);
	g_free (subject);
	g_object_unref (cert);

	return TRUE;
}

static void
on_parser_parsed (GcrParser *parser, gpointer user_data)
{
	GckAttributes *attrs;
	const gchar **pointer_filename = (const gchar**)user_data;

	attrs = gcr_parser_get_parsed_attributes (parser);
	g_return_if_fail (attrs);

	if (!dump_certificate (attrs, *pointer_filename))
		g_print ("%s: parsed %s\n", *pointer_filename,
		         gcr_parser_get_parsed_description (parser));
}

int
main(int argc, char *argv[])
{
	GcrParser *parser;
	GError *error = NULL;
	gchar *contents;
	gsize len;
	GDir *dir;
	const gchar *filename;
	gchar *path;

	gtk_init (&argc, &argv);
	g_set_prgname ("frob-parser");

	if (argc != 2) {
		g_printerr ("usage: frob-parser directory\n");
		return 2;
	}

	dir = g_dir_open (argv[1], 0, &error);
	if (!dir) {
		g_printerr ("couldn't list directory: %s: %s\n", argv[1], error->message);
		g_clear_error (&error);
		return 1;
	}

	parser = gcr_parser_new ();
	g_signal_connect (parser, "parsed", G_CALLBACK (on_parser_parsed), &filename);

	for (;;) {
		filename = g_dir_read_name (dir);
		if (!filename)
			break;
		if (filename[0] == '.')
			continue;

		path = g_build_filename (argv[1], filename, NULL);
		g_file_get_contents (path, &contents, &len, &error);
		g_free (path);

		if (error) {
			g_printerr ("%s: couldn't read file: %s\n", filename, error->message);
			g_clear_error (&error);
			continue;
		}

		gcr_parser_parse_data (parser, contents, len, &error);
		g_free (contents);

		if (error) {
			g_warning ("%s: couldn't parse: %s\n", filename, error->message);
			g_clear_error (&error);
		}

		g_print ("%s: finished parsing\n", filename);
	}

	g_dir_close (dir);
	g_object_unref (parser);

	return 0;
}
