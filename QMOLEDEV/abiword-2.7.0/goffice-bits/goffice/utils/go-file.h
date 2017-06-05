/*
 * go-file.h : 
 *
 * Copyright (C) 2004 Morten Welinder (terra@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#ifndef GO_FILE_H
#define GO_FILE_H

#include <glib.h>
#include <gsf/gsf.h>
#include <time.h>

G_BEGIN_DECLS

typedef struct _GOFilePermissions GOFilePermissions;

struct _GOFilePermissions {
	gboolean owner_read;
	gboolean owner_write;
	gboolean owner_execute;

	gboolean group_read;
	gboolean group_write;
	gboolean group_execute;

	gboolean others_read;
	gboolean others_write;
	gboolean others_execute;
};

typedef enum {
	GO_DOTDOT_SYNTACTIC,    /* Assume no symlinks.  */
	GO_DOTDOT_TEST,         /* Check.  */
	GO_DOTDOT_LEAVE         /* Leave alone.  */
} GODotDot;

#ifndef R_OK
#  define F_OK 0
#  define X_OK 1
#  define W_OK 2
#  define R_OK 4
#endif

char *go_filename_simplify (const char *filename, GODotDot dotdot, gboolean make_absolute);
char *go_url_simplify (const char *uri);

char *go_filename_from_uri (const char *uri);
char *go_filename_to_uri (const char *filename);

char *go_url_resolve_relative (const char *ref_uri, const char *rel_uri);
char *go_url_make_relative (const char *uri, const char *ref_uri);

char *go_shell_arg_to_uri (const char *arg);
char *go_basename_from_uri (const char *uri);
char *go_dirname_from_uri (const char *uri, gboolean brief);
gchar const **go_shell_argv_to_glib_encoding (gint argc, gchar const **argv);
void go_shell_argv_to_glib_encoding_free (void);

GsfInput  *go_file_open		(char const *uri, GError **err);
GsfOutput *go_file_create	(char const *uri, GError **err);
GSList	  *go_file_split_urls	(char const *data);

gchar     *go_file_get_owner_name (char const *uri);
gchar     *go_file_get_group_name (char const *uri);

GOFilePermissions *go_get_file_permissions (char const *uri);
void go_set_file_permissions (char const *uri, GOFilePermissions * file_permissions);

time_t go_file_get_date_accessed (char const *uri);
time_t go_file_get_date_modified (char const *uri);
time_t go_file_get_date_changed  (char const *uri);

gint	 go_file_access (char const *uri, gint mode);

gchar	*go_url_decode		(gchar const *text);
gchar	*go_url_encode		(gchar const *text, int type);
GError	*go_url_show		(gchar const *url);
gboolean go_url_check_extension (gchar const *uri,
				 gchar const *std_ext,
				 gchar **new_uri);
gchar	*go_get_mime_type	(gchar const *uri);
gchar	*go_get_mime_type_for_data	(gconstpointer data, int data_size);
gchar const	*go_mime_type_get_description	(gchar const *mime_type);

#ifndef HAVE_G_ACCESS
#ifdef G_OS_WIN32
#error "A glib with g_access is required for Win32"
#else
#define g_access access
#endif
#endif

G_END_DECLS

#endif /* GO_FILE_H */
