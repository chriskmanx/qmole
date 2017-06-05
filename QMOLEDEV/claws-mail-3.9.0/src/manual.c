/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#include <glib.h>
#include <string.h>

#if HAVE_LOCALE_H
#  include <locale.h>
#endif

#if !defined(LC_MESSAGES) && defined(G_OS_WIN32)
#include <libintl.h>
#endif


#include "prefs_common.h"
#include "manual.h"
#include "utils.h"


static gchar *get_language()
{
	gchar *language;
	gchar *c;

#ifdef G_OS_WIN32
	language = g_strdup(gtk_set_locale());
#else
        /* FIXME: Why not using gtk_set_locale here too? -wk */
	language = g_strdup(setlocale(LC_MESSAGES, NULL));
#endif
        /* At least under W32 it is possible that gtk_set_locate
           returns NULL.  This is not documented but well, it happens
           and g_strdup is happy with a NULL argument.  We return a
           standard language string in this case. */
        if (!language)
                return g_strdup("en");
	if((c = strchr(language, ',')) != NULL)
		*c = '\0';
	if((c = strchr(language, '_')) != NULL)
		*c = '\0';

	return language;
}

static gchar *get_local_path_with_locale(gchar *rootpath)
{
	gchar *lang_str, *dir;

	lang_str = get_language();
	dir = g_strconcat(rootpath, G_DIR_SEPARATOR_S, 
			  lang_str, NULL);
	g_free(lang_str);
	if(!is_dir_exist(dir)) {
		g_free(dir);
		dir = g_strconcat(rootpath, G_DIR_SEPARATOR_S,
				  "en", NULL);
		if(!is_dir_exist(dir)) {
			g_free(dir);
			dir = NULL;
		}
	}

	return dir;
}

gboolean manual_available(ManualType type)
{
	gboolean ret = FALSE;
    	gchar *dir = NULL, *uri = NULL;
	
	switch (type) {
		case MANUAL_MANUAL_CLAWS:
			dir = get_local_path_with_locale(MANUALDIR);
			if (dir != NULL) {
				uri = g_strconcat(dir, G_DIR_SEPARATOR_S, MANUAL_HTML_INDEX, NULL);
				g_free(dir);
				if (is_file_exist(uri))
					ret = TRUE;
				else
					ret = FALSE;
				g_free(uri);
			}
			break;
		default:
			ret = FALSE;
	}

	return ret;
}

void manual_open(ManualType type, gchar *url_anchor)
{
	gchar *uri = NULL;
	gchar *dir;

	switch (type) {
		case MANUAL_MANUAL_CLAWS:
			dir = get_local_path_with_locale(MANUALDIR);
			if (dir != NULL) {
				gchar *tmp_anchor = NULL;
				if (url_anchor && *url_anchor != '\0')
					tmp_anchor = g_strconcat("#", url_anchor, NULL);
				uri = g_strconcat("file://",
						dir, G_DIR_SEPARATOR_S, MANUAL_HTML_INDEX,
						tmp_anchor,
						NULL);
				g_free(tmp_anchor);
				g_free(dir);
			} else {
				uri = g_strconcat(MANUAL_URI, NULL);
			}
			break;
		case MANUAL_FAQ_CLAWS:
			uri = g_strconcat(FAQ_URI, NULL);
			break;

		default:
			break;
	}
	open_uri(uri, prefs_common_get_uri_cmd());
	g_free(uri);
}

void manual_open_with_anchor_cb(GtkWidget *widget, gchar *url_anchor)
{
	manual_open(MANUAL_MANUAL_CLAWS, url_anchor);
}
