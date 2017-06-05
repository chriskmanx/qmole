
/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2008-2012 Ricardo Mones and the Claws Mail team
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

#include "utils.h"
#include "autofaces.h"

static gint get_content_for_any_face(gchar *buf, gint len, gchar *anyname, gint maxlen)
{
	FILE  *xfp;
	gchar *xfile;
	gint  lastc;

	xfile = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, AUTOFACES_DIR,
	                    G_DIR_SEPARATOR_S, anyname, NULL);
	buf[0] = '\0';
	if ((xfp = g_fopen(xfile, "rb")) == NULL) {
	        g_free(xfile);
		debug_print("header content file '%s' not found\n", anyname);
	        return -1;
	}
	if (fgets(buf, (len < maxlen)? len: maxlen, xfp) == NULL) {
	        fclose(xfp);
	        g_free(xfile);
		g_warning("header content file '%s' read failure\n", anyname);
	        return -2;
	}
	lastc = strlen(buf) - 1;        /* remove trailing \n */
	buf[lastc] = (buf[lastc] == '\n')? '\0': buf[lastc];
	fclose(xfp);
	g_free(xfile);

	return 0;
}

static gchar * get_any_face_filename_for_account(gchar *facetype, gchar *accountname)
{
	gchar *name = NULL;
	gchar *what = NULL;
	if (facetype == NULL || accountname == NULL) 
		return NULL;
	if (*facetype == '\0' || *accountname == '\0') 
		return NULL;
	what = name = g_strdup_printf("%s.%s", facetype, accountname);
	while (*what) {
		switch (*what) {
		case '/':
		case '\\':
		case '<':
		case '>':
		case ':':
		case '?':
		case '*':
			*what = '_';
			break;
		default:
			if (*what <= ' ') {
				*what = '_';
			}
			break;
		}
		++what;
	}
	return name;
}

gint get_default_xface(gchar *buf, gint len) {
	return get_content_for_any_face(buf, len, "xface", MAX_XFACE_LEN);
}

gint get_account_xface(gchar *buf, gint len, gchar *name) {
	gchar *filename = get_any_face_filename_for_account("xface", name);
	if (filename) {
		gint result = get_content_for_any_face(buf, len, filename, MAX_XFACE_LEN);
		g_free(filename);
		return result;
	}
	g_warning("header xface filename invalid\n");
	return -1;
}

gint get_default_face(gchar *buf, gint len) {
	return get_content_for_any_face(buf, len, "face", MAX_FACE_LEN);
}

gint get_account_face(gchar *buf, gint len, gchar *name) {
	gchar *filename = get_any_face_filename_for_account("face", name);
	if (filename) {
		gint result = get_content_for_any_face(buf, len, filename, MAX_FACE_LEN);
		g_free(filename);
		return result;
	}
	g_warning("header face filename invalid\n");
	return -1;
}

