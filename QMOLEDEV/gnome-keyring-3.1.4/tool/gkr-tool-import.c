/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkr-tool-import.c: Command line key/certificate import

   Copyright (C) 2008 Stefan Walter

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

#include "gkr-tool.h"

#include "gck/gck.h"
#include "gcr/gcr.h"

#include "egg/egg-hex.h"

static gchar **import_files = NULL;

static GOptionEntry import_entries[] = {
	GKR_TOOL_BASIC_OPTIONS
	{ G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &import_files, "Filename", NULL },
	{ NULL }
};

static void
on_imported (GcrImporter *importer, GckObject *object)
{
	gulong attr_types[3];
	GckAttributes *attrs;
	GckAttribute *id;
	CK_OBJECT_CLASS klass;
	const gchar *message;
	GError *err = NULL;
	gchar *label, *hex;

	attr_types[0] = CKA_LABEL;
	attr_types[1] = CKA_CLASS;
	attr_types[2] = CKA_ID;

	attrs = gck_object_get_full (object, attr_types, G_N_ELEMENTS (attr_types), NULL, &err);
	if (attrs == NULL) {
		gkr_tool_handle_error (&err, "couldn't get imported object info");
		return;
	}

	if (!gck_attributes_find_string (attrs, CKA_LABEL, &label))
		label = g_strdup ("unknown");
	if (!gck_attributes_find_ulong (attrs, CKA_CLASS, &klass))
		klass = CKO_DATA;
	id = gck_attributes_find (attrs, CKA_ID);
	
	switch (klass) {
	case CKO_CERTIFICATE:
		message = "Imported certificate: %s\n";
		break;
	case CKO_DATA:
		message = "Imported data: %s\n";
		break;
	case CKO_PRIVATE_KEY:
		message = "Imported private key: %s\n";
		break;
	case CKO_PUBLIC_KEY:
		message = "Imported public key: %s\n";
		break;
	case CKO_SECRET_KEY:
		message = "Imported secret key: %s\n";
		break;
	default:
		message = "Imported object: %s\n";
		break;
	};
	
	g_print (message, label);

	if (id) {
		hex = egg_hex_encode (id->value, id->length);
		g_print ("\tID: %s\n", hex);
		g_free (hex);
	}

	gck_attributes_unref (attrs);
	g_free (label);
}

int
gkr_tool_import (int argc, char *argv[])
{
	GcrImporter *importer;
	GcrParser *parser;
	GError *error = NULL;
	GInputStream *input;
	gboolean res;
	GFile *file;
	gchar **imp;
	int ret = 0;
	
	ret = gkr_tool_parse_options (&argc, &argv, import_entries);
	if (ret != 0)
		return ret;
	
	if(!import_files || !*import_files) {
		gkr_tool_handle_error (NULL, "specify files to import");
		return 2;
	}
	
	importer = gcr_importer_new ();
	gcr_importer_set_prompt_behavior (importer, GCR_IMPORTER_PROMPT_NEEDED);
	
	if (!gkr_tool_mode_quiet) 
		g_signal_connect (importer, "imported", G_CALLBACK (on_imported), NULL);
	
	for (imp = import_files; *imp; ++imp) {
		file = g_file_new_for_commandline_arg (*imp);
		
		input = G_INPUT_STREAM (g_file_read (file, NULL, &error));
		g_object_unref (file);
		if (!input) {
			gkr_tool_handle_error (&error, "couldn't read file: %s", *imp);
			ret = 1;
		} else {
			parser = gcr_parser_new ();
			gcr_importer_listen (importer, parser);
			res = gcr_parser_parse_stream (parser, input, NULL, &error);
			g_object_unref (input);
			g_object_unref (parser);

			if (res == TRUE)
				res = gcr_importer_import (importer, NULL, &error);

			if (res == FALSE) {
				if (!error || error->code != GCR_ERROR_CANCELLED)
					gkr_tool_handle_error (&error, "couldn't import file: %s", *imp);
				ret = 1;
			}
		}
	}
	
	g_object_unref (importer);
	return ret;
}
