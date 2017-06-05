/* -*- Mode: C; c-set-style: gnu indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gnome-url.c
 * Copyright (C) 1998, James Henstridge <james@daa.com.au>
 * Copyright (C) 1999, 2000 Red Hat, Inc.
 * All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc.,  59 Temple Place - Suite 330, Cambridge, MA 02139, USA.
 */
/*
  @NOTATION@
 */

#include <config.h>
#include <string.h>
#include <glib.h>
#include <limits.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

#include "gnome-i18nP.h"

#include <gconf/gconf-client.h>

#include "gnome-exec.h"
#include "gnome-util.h"
#include "gnome-init.h"
#include "gnome-i18n.h"
#include "gnome-gconfP.h"

#include "gnome-url.h"

#include <popt.h>

#define URL_HANDLER_DIR      "/desktop/gnome/url-handlers/"
#define DEFAULT_HANDLER_PATH "/desktop/gnome/url-handlers/unknown/command"

/**
 * gnome_url_show
 * @url: The url to display. Should begin with the protocol to use (e.g.
 * "http:", "ghelp:", etc)
 * @error: Used to store any errors that result from trying to display the @url.
 *
 * Displays the given URL in an appropriate viewer. The appropriate viewer is
 * user definable. It is determined by extracting the protocol from the @url,
 * then seeing if the /desktop/gnome/url-handlers/&lt;protocol&gt;/command key
 * exists in the configuration database. It it does, this entry is used as the
 * template for the command. 
 *
 * If no protocol specific handler exists, the
 * /desktop/gnome/url-handlers/unknown/command key is used to determine the
 * viewer.
 *
 * Once a viewer is determined, it is called with the @url as a parameter. If
 * any errors occur, they are returned in the @error parameter. These errors
 * will either be in the %GNOME_URL_ERROR, %GNOME_SHELL_ERROR, or
 * %G_SPAWN_ERROR domains.
 *
 * Returns: %TRUE if everything went fine, %FALSE otherwise (in which case
 * @error will contain the actual error).
 */
gboolean
gnome_url_show (const gchar *url, GError **error)
{
	GConfClient *client;
	gint i;
	gchar *pos, *template;
	int argc;
	char **argv;
	gboolean ret;
	
	g_return_val_if_fail (url != NULL, FALSE);

	pos = strchr (url, ':');

	/* init our gconf stuff if necessary */
	gnome_gconf_lazy_init ();
	
	client = gconf_client_get_default ();

	if (pos != NULL) {
		gchar *protocol, *path;
		
		g_return_val_if_fail (pos >= url, FALSE);

		protocol = g_new (gchar, pos - url + 1);
		strncpy (protocol, url, pos - url);
		protocol[pos - url] = '\0';
		g_ascii_strdown (protocol, -1);

		path = g_strconcat (URL_HANDLER_DIR, protocol, "/command", NULL);
		template = gconf_client_get_string (client, path, NULL);

		if (template == NULL) {
			gchar* template_temp;
			
			template_temp = gconf_client_get_string (client,
								 DEFAULT_HANDLER_PATH,
								 NULL);
						
			/* Retry to get the right url handler */
			template = gconf_client_get_string (client, path, NULL);

			if (template == NULL) 
				template = template_temp;
			else
				g_free (template_temp);

		}
		
		g_free (path);
		g_free (protocol);

	} else {
		/* no ':' ? this shouldn't happen. Use default handler */
		template = gconf_client_get_string (client, 
						    DEFAULT_HANDLER_PATH, 
						    NULL);
	}

	g_object_unref (G_OBJECT (client));

	if (!g_shell_parse_argv (template,
				 &argc,
				 &argv,
				 error)) {
		g_free (template);
		return FALSE;
	}

	g_free (template);

	for (i = 0; i < argc; i++) {
		char *arg;

		if (strcmp (argv[i], "%s") != 0)
			continue;

		arg = argv[i];
		argv[i] = g_strdup (url);
		g_free (arg);
	}
	
	/* This can return some errors */
	ret = g_spawn_async (NULL /* working directory */,
			     argv,
			     NULL /* envp */,
			     G_SPAWN_SEARCH_PATH /* flags */,
			     NULL /* child_setup */,
			     NULL /* data */,
			     NULL /* child_pid */,
			     error);

	g_strfreev (argv);

	return ret;
}

/**
 * gnome_url_error_quark
 *
 * Returns: A quark representing gnome-url module errors.
 */
GQuark
gnome_url_error_quark (void)
{
	static GQuark error_quark = 0;

	if (error_quark == 0)
		error_quark =
			g_quark_from_static_string ("gnome-url-error-quark");

	return error_quark;
}
