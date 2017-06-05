/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-file.c :
 *
 * Copyright (C) 2004 Morten Welinder (terra@gnome.org)
 * Copyright (C) 2004 Yukihiro Nakai  <nakai@gnome.gr.jp>
 * Copyright (C) 2003, Red Hat, Inc.
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

#include <goffice/goffice-config.h>
#include "go-file.h"
#include "go-glib-extras.h"
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-output-stdio.h>
#include <glib/gstdio.h>
#include <glib/gi18n-lib.h>
#ifdef GOFFICE_WITH_GNOME
#include <libgnomevfs/gnome-vfs-utils.h>
#include <libgnomevfs/gnome-vfs-mime-utils.h>
#include <libgnomevfs/gnome-vfs-mime-handlers.h>
#include <gsf-gnome/gsf-input-gnomevfs.h>
#include <gsf-gnome/gsf-output-gnomevfs.h>
#include <libgnome/gnome-url.h>
#elif defined G_OS_WIN32
#include <urlmon.h>
#include <io.h>
#endif

#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#ifdef HAVE_GRP_H
#include <grp.h>
#endif
#include <time.h>
/* ------------------------------------------------------------------------- */

/*
 * Convert an escaped URI into a filename.
 */
char *
go_filename_from_uri (const char *uri)
{
#ifdef GOFFICE_WITH_GNOME
	return gnome_vfs_get_local_path_from_uri (uri);
#else
	return g_filename_from_uri (uri, NULL, NULL);
#endif
}

/*
 * Convert a filename into an escaped URI.
 */
char *
go_filename_to_uri (const char *filename)
{
	char *simp, *uri;

	g_return_val_if_fail (filename != NULL, NULL);

	simp = go_filename_simplify (filename, GO_DOTDOT_TEST, TRUE);

#ifdef GOFFICE_WITH_GNOME
	uri = gnome_vfs_get_uri_from_local_path (simp);
#else
	uri = g_filename_to_uri (simp, NULL, NULL);
#endif
	g_free (simp);
	return uri;
}

char *
go_filename_simplify (const char *filename, GODotDot dotdot,
		      gboolean make_absolute)
{
	char *simp, *p, *q;

	g_return_val_if_fail (filename != NULL, NULL);

	if (make_absolute && !g_path_is_absolute (filename)) {
		/*
		 * FIXME: this probably does not work for "c:foo" on
		 * Win32.
		 */
		char *current_dir = g_get_current_dir ();
		simp = g_build_filename (current_dir, filename, NULL);
		g_free (current_dir);
	} else
		simp = g_strdup (filename);

	for (p = q = simp; *p;) {
		if (p != simp &&
		    G_IS_DIR_SEPARATOR (p[0]) &&
		    G_IS_DIR_SEPARATOR (p[1])) {
			/* "//" --> "/", except initially.  */
			p++;
			continue;
		}

		if (G_IS_DIR_SEPARATOR (p[0]) &&
		    p[1] == '.' &&
		    G_IS_DIR_SEPARATOR (p[2])) {
			/* "/./" -> "/".  */
			p += 2;
			continue;
		}

		if (G_IS_DIR_SEPARATOR (p[0]) &&
		    p[1] == '.' &&
		    p[2] == '.' &&
		    G_IS_DIR_SEPARATOR (p[3])) {
			if (p == simp) {
				/* "/../" --> "/" initially.  */
				p += 3;
				continue;
			} else if (p == simp + 1) {
				/* Nothing, leave "//../" initially alone.  */
			} else {
				/*
				 * "prefix/dir/../" --> "prefix/" if
				 * "dir" is an existing directory (not
				 * a symlink).
				 */
				gboolean isdir;

				switch (dotdot) {
				case GO_DOTDOT_SYNTACTIC:
					isdir = TRUE;
					break;
				case GO_DOTDOT_TEST: {
					struct stat statbuf;
					char savec = *q;
					/*
					 * Terminate the path so far so we can
					 * it.  Restore because "p" loops over
					 * the same.
					 */
					*q = 0;
					isdir = (g_lstat (simp, &statbuf) == 0) &&
						1;//S_ISDIR (statbuf.st_mode);
					*q = savec;
					break;
				}
				default:
					isdir = FALSE;
					break;
				}

				if (isdir) {
					do {
						g_assert (q != simp);
						q--;
					} while (!G_IS_DIR_SEPARATOR (*q));
					p += 3;
					continue;
				} else {
					/*
					 * Do nothing.
					 *
					 * Maybe the prefix does not
					 * exist, or maybe it is not
					 * a directory (for example
					 * because it is a symlink).
					 */
				}
			}
		}

		*q++ = *p++;
	}
	*q = 0;

	return simp;
}

/*
 * Simplify a potentially non-local path using only slashes.
 */
static char *
simplify_path (const char *uri)
{
	char *simp, *p, *q;

	simp = g_strdup (uri);

	for (p = q = simp; *p;) {
		if (p[0] == '/' && p[1] == '/') {
			/* "//" --> "/".  */
			p++;
			continue;
		}

		if (p[0] == '/' && p[1] == '.' && p[2] == '/') {
			/* "/./" -> "/".  */
			p += 2;
			continue;
		}

		if (p[0] == '/' && p[1] == '.' && p[2] == '.' && p[3] == '/') {
			if (p == simp) {
				/* "/../" --> "/" initially.  */
				p += 3;
				continue;
			} else {
				/* Leave alone */
			}
		}

		*q++ = *p++;
	}
	*q = 0;

	return simp;
}

static char *
simplify_host_path (const char *uri, size_t hstart)
{
	const char *slash = strchr (uri + hstart, '/');
	char *simp, *psimp;
	size_t pstart;

	if (!slash)
		return g_strdup (uri);

	pstart = slash + 1 - uri;
	psimp = simplify_path (slash + 1);
	simp = g_new (char, pstart + strlen (psimp) + 1);
	memcpy (simp, uri, pstart);
	strcpy (simp + pstart, psimp);
	g_free (psimp);
	return simp;
}

char *
go_url_simplify (const char *uri)
{
	char *simp, *p;

	g_return_val_if_fail (uri != NULL, NULL);

	if (g_ascii_strncasecmp (uri, "file:///", 8) == 0) {
		char *filename = go_filename_from_uri (uri);
		char *simp = filename ? go_filename_to_uri (filename) : NULL;
		g_free (filename);
		return simp;
	}

	if (g_ascii_strncasecmp (uri, "http://", 7) == 0)
		simp = simplify_host_path (uri, 7);
	else if (g_ascii_strncasecmp (uri, "https://", 8) == 0)
		simp = simplify_host_path (uri, 8);
	else if (g_ascii_strncasecmp (uri, "ftp://", 6) == 0)
		simp = simplify_host_path (uri, 6);
	else
		simp = g_strdup (uri);

	/* Lower-case protocol name.  */
	for (p = simp; g_ascii_isalpha (*p); p++)
		*p = g_ascii_tolower (*p);

	return simp;
}


/*
 * More or less the same as gnome_vfs_uri_make_full_from_relative.
 */
char *
go_url_resolve_relative (const char *ref_uri, const char *rel_uri)
{
	char *simp, *uri;

#ifdef GOFFICE_WITH_GNOME
	uri = gnome_vfs_uri_make_full_from_relative (ref_uri, rel_uri);
#else
	size_t len;

	g_return_val_if_fail (ref_uri != NULL, NULL);
	g_return_val_if_fail (rel_uri != NULL, NULL);

	len = strlen (ref_uri);

	/* FIXME: This doesn't work if rel_uri starts with a slash.  */

	uri = g_new (char, len + strlen (rel_uri) + 1);
	memcpy (uri, rel_uri, len + 1);
	while (len > 0 && uri[len - 1] != '/')
		len--;
	if (len == 0) {
		g_free (uri);
		return NULL;
	}

	strcpy (uri + len, rel_uri);
#endif

	simp = go_url_simplify (uri);
	g_free (uri);
	return simp;
}

static char *
make_rel (const char *uri, const char *ref_uri,
	  const char *uri_host, const char *slash)
{
	const char *p, *q;
	int n;
	GString *res;

	if (!slash)
		return NULL;

	if (uri_host != NULL &&
	    strncmp (uri_host, ref_uri + (uri_host - uri), slash - uri_host))
		return NULL;

	for (p = slash; *p; p++) {
		if (*p != ref_uri[p - uri])
			break;
		else if (*p == '/')
			slash = p;
	}
	/* URI components agree until slash.  */

	/* Find out the number of '/' in uri after slash.  */
	n = 0;
	q = slash;
	while (1) {
		q = strchr (q + 1, '/');
		if (q)
			n++;
		else
			break;
	}

	res = g_string_new (NULL);
	while (n-- > 0)
		g_string_append (res, "../");
	g_string_append (res, slash + 1);
	return g_string_free (res, FALSE);
}

char *
go_url_make_relative (const char *uri, const char *ref_uri)
{
	int i;

	/* Check that protocols are the same.  */
	for (i = 0; 1; i++) {
		char c = uri[i];
		char rc = ref_uri[i];

		if (c == 0)
			return NULL;

		if (c == ':') {
			if (rc == ':')
				break;
			return NULL;
		}

		if (g_ascii_tolower (c) != g_ascii_tolower (rc))
			return NULL;
	}

	if (g_ascii_strncasecmp (uri, "file:///", 8) == 0)
		return make_rel (uri, ref_uri, NULL, uri + 7);  /* Yes, 7.  */

	if (g_ascii_strncasecmp (uri, "http://", 7) == 0)
		return make_rel (uri, ref_uri, uri + 7, strchr (uri + 7, '/'));

	if (g_ascii_strncasecmp (uri, "https://", 8) == 0)
		return make_rel (uri, ref_uri, uri + 8, strchr (uri + 8, '/'));

	if (g_ascii_strncasecmp (uri, "ftp://", 6) == 0)
		return make_rel (uri, ref_uri, uri + 6, strchr (uri + 6, '/'));

	return NULL;
}

/*
 * Convert a shell argv entry (assumed already translated into filename
 * encoding) to an escaped URI.
 */
char *
go_shell_arg_to_uri (const char *arg)
{
	gchar *tmp;

	if (g_path_is_absolute (arg) || strchr (arg, ':') == NULL)
		return go_filename_to_uri (arg);

	tmp = go_filename_from_uri (arg);
	if (tmp) {
		/*
		 * Do the reverse translation to get a minimum of
		 * canonicalization.
		 */
		char *res = go_filename_to_uri (tmp);
		g_free (tmp);
		return res;
	}

#ifdef GOFFICE_WITH_GNOME
	{
		/*
		 * oink://     --> NULL
		 * http://     --> "http" URI
		 */
		GnomeVFSURI *uri = gnome_vfs_uri_new (arg);
		if (uri) {
			gnome_vfs_uri_unref (uri);
			return go_url_simplify (arg);
		}
	}
#endif

	/* Just assume it's a filename.  */
	return go_filename_to_uri (arg);
}

/**
 * go_basename_from_uri:
 * @uri :
 *
 * Decode the final path component.  Returns as UTF-8 encoded suitable
 * for display.
 **/
char *
go_basename_from_uri (const char *uri)
{
	char *res;

#ifdef GOFFICE_WITH_GNOME
	char *raw_uri = gnome_vfs_unescape_string (uri, G_DIR_SEPARATOR_S);
	char *basename = raw_uri ? g_path_get_basename (raw_uri) : NULL;
	g_free (raw_uri);
#else
	char *uri_basename = g_path_get_basename (uri);
	char *fake_uri = g_strconcat ("file:///", uri_basename, NULL);
	char *filename = go_filename_from_uri (fake_uri);
	char *basename = filename ? g_path_get_basename (filename) : NULL;
	g_free (uri_basename);
	g_free (fake_uri);
	g_free (filename);

#endif

	res = basename ? g_filename_display_name (basename) : NULL;
	g_free (basename);
	return res;
}

/**
 * go_dirname_from_uri:
 * @uri :
 * @brief: if TRUE, hide "file://" if present.
 *
 * Decode the all but the final path component.  Returns as UTF-8 encoded
 * suitable for display.
 **/
char *
go_dirname_from_uri (const char *uri, gboolean brief)
{
	char *dirname_utf8, *dirname;

#ifdef GOFFICE_WITH_GNOME
	char *raw_uri = gnome_vfs_unescape_string (uri, G_DIR_SEPARATOR_S);
	dirname = raw_uri ? g_path_get_dirname (raw_uri) : NULL;
	g_free (raw_uri);
#else
	char *uri_dirname = g_path_get_dirname (uri);
	dirname = uri_dirname ? go_filename_from_uri (uri_dirname) : NULL;
	dirname = dirname ? g_strconcat ("file://", dirname, NULL) : NULL;
	g_free (uri_dirname);
#endif

	if (brief && dirname &&
	    g_ascii_strncasecmp (dirname, "file:///", 8) == 0) {
		char *temp = g_strdup (dirname + 7);
		g_free (dirname);
		dirname = temp;
	}

	dirname_utf8 = dirname ? g_filename_display_name (dirname) : NULL;
	g_free (dirname);
	return dirname_utf8;
}

/* ------------------------------------------------------------------------- */

static gboolean
is_fd_uri (const char *uri, int *fd)
{
	unsigned long ul;
	char *end;

	if (g_ascii_strncasecmp (uri, "fd://", 5))
		return FALSE;
	uri += 5;
	if (!g_ascii_isdigit (*uri))
		return FALSE;  /* Space, for example.  */

	ul = strtoul (uri, &end, 10);
	if (*end != 0 || ul > INT_MAX)
		return FALSE;

	*fd = (int)ul;
	return TRUE;
}

/* ------------------------------------------------------------------------- */

static GsfInput *
open_plain_file (const char *path, GError **err)
{
	GsfInput *input = gsf_input_mmap_new (path, NULL);
	if (input != NULL)
		return input;
	/* Only report error if stdio fails too */
	return gsf_input_stdio_new (path, err);
}


/**
 * go_file_open :
 * @uri :
 * @err : #GError
 *
 * Try all available methods to open a file or return an error
 **/
GsfInput *
go_file_open (char const *uri, GError **err)
{
	char *filename;
	int fd;

	if (err != NULL)
		*err = NULL;
	g_return_val_if_fail (uri != NULL, NULL);

	if (uri[0] == G_DIR_SEPARATOR) {
		g_warning ("Got plain filename %s in go_file_open.", uri);
		return open_plain_file (uri, err);
	}

	filename = go_filename_from_uri (uri);
	if (filename) {
		GsfInput *result = open_plain_file (filename, err);
		g_free (filename);
		return result;
	}

	if (is_fd_uri (uri, &fd)) {
		int fd2 = dup (fd);
		FILE *fil = fd2 != -1 ? fdopen (fd2, "rb") : NULL;
		GsfInput *result = fil ? gsf_input_stdio_new_FILE (uri, fil, FALSE) : NULL;

		if (!result)
			g_set_error (err, gsf_output_error_id (), 0,
				     "Unable to read from %s", uri);
		return result;
	}

#ifdef GOFFICE_WITH_GNOME
	return gsf_input_gnomevfs_new (uri, err);
#else
	g_set_error (err, gsf_input_error (), 0,
		     "Invalid or non-supported URI");
	return NULL;
#endif
}

GsfOutput *
go_file_create (char const *uri, GError **err)
{
	char *filename;
	int fd;

	g_return_val_if_fail (uri != NULL, NULL);

	filename = go_filename_from_uri (uri);
	if (filename) {
		GsfOutput *result = gsf_output_stdio_new (filename, err);
		g_free (filename);
		return result;
	}

	if (is_fd_uri (uri, &fd)) {
		int fd2 = dup (fd);
		FILE *fil = fd2 != -1 ? fdopen (fd2, "wb") : NULL;
		GsfOutput *result = fil ? gsf_output_stdio_new_FILE (uri, fil, FALSE) : NULL;

		if (!result)
			g_set_error (err, gsf_output_error_id (), 0,
				     "Unable to write to %s", uri);
		return result;
	}

#ifdef GOFFICE_WITH_GNOME
	return gsf_output_gnomevfs_new (uri, err);
#else
	g_set_error (err, gsf_output_error_id (), 0,
		     "Invalid or non-supported URI");
	return NULL;
#endif
}

/* ------------------------------------------------------------------------- */
/* Adapted from gtkfilechooserdefault.c.  Unfortunately it is static there.  */

GSList *
go_file_split_urls (const char *data)
{
  GSList *uris;
  const char *p, *q;

  uris = NULL;

  p = data;

  /* We don't actually try to validate the URI according to RFC
   * 2396, or even check for allowed characters - we just ignore
   * comments and trim whitespace off the ends.  We also
   * allow LF delimination as well as the specified CRLF.
   *
   * We do allow comments like specified in RFC 2483.
   */
  while (p)
    {
      if (*p != '#')
	{
	  while (g_ascii_isspace (*p))
	    p++;

	  q = p;
	  while (*q && (*q != '\n') && (*q != '\r'))
	    q++;

	  if (q > p)
	    {
	      q--;
	      while (q > p && g_ascii_isspace (*q))
		q--;

	      if (q > p)
		uris = g_slist_prepend (uris, g_strndup (p, q - p + 1));
	    }
	}
      p = strchr (p, '\n');
      if (p)
	p++;
    }

  uris = g_slist_reverse (uris);
  return uris;
}

/*
 * Return the real name of the owner of a URI.  The result will be in
 * UTF-8 and the caller must free the result.
 */
gchar *
go_file_get_owner_name (char const *uri)
{
	gboolean error = FALSE;
	guint uid = 0;
	gboolean islocal = FALSE;
#ifdef HAVE_PWD_H
	struct passwd *password_info;
	const char *name;
	gsize namelen;
	char *nameutf8;
#endif

#ifdef GOFFICE_WITH_GNOME
	GnomeVFSFileInfo *file_info;

	GnomeVFSResult result;

        file_info = gnome_vfs_file_info_new ();
        result = gnome_vfs_get_file_info (uri, file_info,
                                          GNOME_VFS_FILE_INFO_FOLLOW_LINKS);

        if (result == GNOME_VFS_OK) {
		uid = file_info->uid;
		islocal = GNOME_VFS_FILE_INFO_LOCAL (file_info);
	} else
		error = TRUE;

	gnome_vfs_file_info_unref (file_info);
#else
	struct stat file_stat;
	char *filename = go_filename_from_uri (uri);
	int result = filename ? g_stat (filename, &file_stat) : -1;

	g_free (filename);
	if (result == 0) {
		uid = file_stat.st_uid;
		islocal = TRUE;
	} else
		error = TRUE;
#endif

	if (error)
		return NULL;

	if (!islocal) {
		/* xgettext: generic fake user name for non-local files. */
		return g_strdup (_("remote user"));
	}

#ifdef HAVE_PWD_H
	password_info = getpwuid (uid);

	if (password_info == NULL)
		return NULL;

	name = password_info->pw_gecos;
	(void) go_guess_encoding (name, strlen (name),
				  NULL, &nameutf8);
	if (!nameutf8)
		return NULL;
	namelen = strlen (nameutf8);

	/*
	 * What about magic characters used instead of user name?
	 */

	/* Strip comma characters at the end of the string.  */
	while (namelen > 0 && nameutf8[namelen - 1] == ',')
		nameutf8[--namelen] = 0;

	return nameutf8;
#else
	return NULL;
#endif
}

/*
 * Return the group name of the owner of a URI.  The result will be in
 * UTF-8 and the caller must free the result.
 */
gchar *
go_file_get_group_name (char const *uri)
{
	gboolean error = FALSE;
	guint gid = 0;
	gboolean islocal = FALSE;
#ifdef HAVE_GRP_H
	struct group *group_info;
	const char *name;
	char *nameutf8;
#endif

#ifdef GOFFICE_WITH_GNOME
	GnomeVFSFileInfo *file_info;

	GnomeVFSResult result;

        file_info = gnome_vfs_file_info_new ();
        result = gnome_vfs_get_file_info (uri, file_info,
                                          GNOME_VFS_FILE_INFO_FOLLOW_LINKS);

        if (result == GNOME_VFS_OK) {
		gid = file_info->gid;
		islocal = GNOME_VFS_FILE_INFO_LOCAL (file_info);
	} else
		error = TRUE;

	gnome_vfs_file_info_unref (file_info);
#else
	struct stat file_stat;
	char *filename = go_filename_from_uri (uri);
	int result = filename ? g_stat (filename, &file_stat) : -1;

	g_free (filename);

	if (result == 0) {
		gid = file_stat.st_gid;
		islocal = TRUE;
	} else
		error = TRUE;
#endif

	if (error)
		return NULL;

	if (!islocal) {
		/* xgettext: generic fake group name for non-local files. */
		return g_strdup (_("remote"));
	}

#ifdef HAVE_GRP_H
	group_info = getgrgid (gid);

	if (group_info == NULL)
		return NULL;

	name = group_info->gr_name;
	(void) go_guess_encoding (name, strlen (name),
				  NULL, &nameutf8);
	return nameutf8;
#else
	return NULL;
#endif
}

GOFilePermissions *
go_get_file_permissions (char const *uri)
{
	GOFilePermissions * file_permissions = NULL;

#if defined (GOFFICE_WITH_GNOME)
	GnomeVFSFileInfo *file_info;
	GnomeVFSResult result;

        file_info = gnome_vfs_file_info_new ();
        result = gnome_vfs_get_file_info (uri, file_info,
					  GNOME_VFS_FILE_INFO_GET_ACCESS_RIGHTS |
                                          GNOME_VFS_FILE_INFO_FOLLOW_LINKS);

        if (result == GNOME_VFS_OK) {
		file_permissions = g_new0 (GOFilePermissions, 1);

		/* Owner  Permissions */
		file_permissions->owner_read    = ((file_info->permissions & GNOME_VFS_PERM_USER_READ) != 0);
		file_permissions->owner_write   = ((file_info->permissions & GNOME_VFS_PERM_USER_WRITE) != 0);
		file_permissions->owner_execute = ((file_info->permissions & GNOME_VFS_PERM_USER_EXEC) != 0);

		/* Group  Permissions */
		file_permissions->group_read    = ((file_info->permissions & GNOME_VFS_PERM_GROUP_READ) != 0);
		file_permissions->group_write   = ((file_info->permissions & GNOME_VFS_PERM_GROUP_WRITE) != 0);
		file_permissions->group_execute = ((file_info->permissions & GNOME_VFS_PERM_GROUP_EXEC) != 0);

		/* Others Permissions */
		file_permissions->others_read    = ((file_info->permissions & GNOME_VFS_PERM_OTHER_READ) != 0);
		file_permissions->others_write   = ((file_info->permissions & GNOME_VFS_PERM_OTHER_WRITE) != 0);
		file_permissions->others_execute = ((file_info->permissions & GNOME_VFS_PERM_OTHER_EXEC) != 0);
	}

	gnome_vfs_file_info_unref (file_info);
#elif ! defined (G_OS_WIN32)
	struct stat file_stat;
	char *filename = go_filename_from_uri (uri);
	int result = filename ? g_stat (filename, &file_stat) : -1;

	g_free (filename);
	if (result == 0) {
		file_permissions = g_new0 (GOFilePermissions, 1);

		/* Owner  Permissions */
		file_permissions->owner_read    = ((file_stat.st_mode & S_IRUSR) != 0);
		file_permissions->owner_write   = ((file_stat.st_mode & S_IWUSR) != 0);
		file_permissions->owner_execute = ((file_stat.st_mode & S_IXUSR) != 0);

		/* Group  Permissions */
		file_permissions->group_read    = ((file_stat.st_mode & S_IRGRP) != 0);
		file_permissions->group_write   = ((file_stat.st_mode & S_IWGRP) != 0);
		file_permissions->group_execute = ((file_stat.st_mode & S_IXGRP) != 0);

		/* Others Permissions */
		file_permissions->others_read    = ((file_stat.st_mode & S_IROTH) != 0);
		file_permissions->others_write   = ((file_stat.st_mode & S_IWOTH) != 0);
		file_permissions->others_execute = ((file_stat.st_mode & S_IXOTH) != 0);
	}
#endif
	return file_permissions;
}

void
go_set_file_permissions (char const *uri, GOFilePermissions * file_permissions)
{
#if defined (GOFFICE_WITH_GNOME)
	GnomeVFSFileInfo *file_info;
	GnomeVFSResult result;

        file_info = gnome_vfs_file_info_new ();
	file_info->permissions = 0;

	/* Set owner permissions */
	if (file_permissions->owner_read == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_USER_READ;

	if (file_permissions->owner_write == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_USER_WRITE;

	if (file_permissions->owner_execute == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_USER_EXEC;

	/* Set group permissions */
	if (file_permissions->group_read == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_GROUP_READ;

	if (file_permissions->group_write == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_GROUP_WRITE;

	if (file_permissions->group_execute == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_GROUP_EXEC;

	/* Set others permissions */
	if (file_permissions->others_read == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_OTHER_READ;

	if (file_permissions->others_write == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_OTHER_WRITE;

	if (file_permissions->others_execute == TRUE)
		file_info->permissions |= GNOME_VFS_PERM_OTHER_EXEC;

	result = gnome_vfs_set_file_info (uri, file_info,
					  GNOME_VFS_FILE_INFO_GET_ACCESS_RIGHTS |
					  GNOME_VFS_FILE_INFO_FOLLOW_LINKS |
					  GNOME_VFS_SET_FILE_INFO_PERMISSIONS);

	if (result != GNOME_VFS_OK)
		g_warning ("Error setting permissions for '%s'.", uri);

	gnome_vfs_file_info_unref (file_info);
#elif ! defined (G_OS_WIN32)
	mode_t permissions = 0;
	int result;
	char *filename;

	/* Set owner permissions */
	if (file_permissions->owner_read == TRUE)
		permissions |= S_IRUSR;

	if (file_permissions->owner_write == TRUE)
		permissions |= S_IWUSR;

	if (file_permissions->owner_execute == TRUE)
		permissions |= S_IXUSR;

	/* Set group permissions */
	if (file_permissions->group_read == TRUE)
		permissions |= S_IRGRP;

	if (file_permissions->group_write == TRUE)
		permissions |= S_IWGRP;

	if (file_permissions->group_execute == TRUE)
		permissions |= S_IXGRP;

	/* Set others permissions */
	if (file_permissions->others_read == TRUE)
		permissions |= S_IROTH;

	if (file_permissions->others_write == TRUE)
		permissions |= S_IWOTH;

	if (file_permissions->others_execute == TRUE)
		permissions |= S_IXOTH;

	filename = go_filename_from_uri (uri);
	
#ifdef HAVE_G_CHMOD
	result = g_chmod (filename, permissions);
#else
	result = chmod (filename, permissions);
#endif

	g_free (filename);

	if (result != 0)
		g_warning ("Error setting permissions for %s.", uri);
#endif
}

typedef enum {
	GO_FILE_DATE_TYPE_ACCESSED = 0,
	GO_FILE_DATE_TYPE_MODIFIED,
	GO_FILE_DATE_TYPE_CHANGED
} GOFileDateType;

static time_t
go_file_get_date (char const *uri, GOFileDateType type)
{
	time_t tm = -1;

#ifdef GOFFICE_WITH_GNOME
	GnomeVFSFileInfo *file_info;

	GnomeVFSResult result;

        file_info = gnome_vfs_file_info_new ();
        result = gnome_vfs_get_file_info (uri, file_info,
                                          GNOME_VFS_FILE_INFO_FOLLOW_LINKS);

        if (result == GNOME_VFS_OK) {
		switch (type) {
			case GO_FILE_DATE_TYPE_ACCESSED:
				tm = file_info->atime;
				break;
			case GO_FILE_DATE_TYPE_MODIFIED:
				tm = file_info->mtime;
				break;
			case GO_FILE_DATE_TYPE_CHANGED:
				tm = file_info->ctime;
				break;
		}
	}

	gnome_vfs_file_info_unref (file_info);
#else
	struct stat file_stat;
	char *filename = go_filename_from_uri (uri);
	int result = filename ? g_stat (filename, &file_stat) : -1;

	g_free (filename);
	if (result == 0) {
		switch (type) {
			case GO_FILE_DATE_TYPE_ACCESSED:
				tm = file_stat.st_atime;
				break;
			case GO_FILE_DATE_TYPE_MODIFIED:
				tm = file_stat.st_mtime;
				break;
			case GO_FILE_DATE_TYPE_CHANGED:
				tm = file_stat.st_ctime;
				break;
		}
	}
#endif

	return tm;
}

time_t
go_file_get_date_accessed (char const *uri)
{
	return go_file_get_date (uri, GO_FILE_DATE_TYPE_ACCESSED);
}

time_t
go_file_get_date_modified (char const *uri)
{
	return go_file_get_date (uri, GO_FILE_DATE_TYPE_MODIFIED);
}

time_t
go_file_get_date_changed (char const *uri)
{
	return go_file_get_date (uri, GO_FILE_DATE_TYPE_CHANGED);
}

/* ------------------------------------------------------------------------- */

/*
 * go_url_decode: decode the result of go_url_encode.
 */
gchar*
go_url_decode (gchar const *text)
{
	GString *result;

	g_return_val_if_fail (text != NULL, NULL);
	g_return_val_if_fail (*text != '\0', NULL);

	result = g_string_new (NULL);
	while (*text) {
		unsigned char c = *text++;
		if (c == '%') {
			if (g_ascii_isxdigit (text[0]) && g_ascii_isxdigit (text[1])) {
				g_string_append_c (result,
						   (g_ascii_xdigit_value (text[0]) << 4) |
						   g_ascii_xdigit_value (text[1]));
				text += 2;
			} else {
				/* Bogus.  */
				return g_string_free (result, TRUE);
			}
		} else
			g_string_append_c (result, c);
	}

	return g_string_free (result, FALSE);
}

/**
 * go_url_encode: url-encode a string according to RFC 2368.
 */
gchar*
go_url_encode (gchar const *text, int type)
{
	const char *good;
	static const char hex[16] = "0123456789ABCDEF";
	GString* result;

	g_return_val_if_fail (text != NULL, NULL);
	g_return_val_if_fail (*text != '\0', NULL);

	switch (type) {
	case 0: /* mailto: */
		good = ".-_@";
		break;
	case 1: /* file: or http: */
		good = "!$&'()*+,-./:=@_";
		break;
	default:
		return NULL;
	}

	result = g_string_new (NULL);
	while (*text) {
		unsigned char c = *text++;
		if (g_ascii_isalnum (c) || strchr (good, c))
			g_string_append_c (result, c);
		else {
			g_string_append_c (result, '%');
			g_string_append_c (result, hex[c >> 4]);
			g_string_append_c (result, hex[c & 0xf]);
		}
	}
	return g_string_free (result, FALSE);
}

#ifndef GOFFICE_WITH_GNOME
static char *
check_program (char const *prog)
{
	if (NULL == prog)
		return NULL;
	if (g_path_is_absolute (prog)) {
		if (!g_file_test (prog, G_FILE_TEST_IS_EXECUTABLE))
			return NULL;
	} else if (!g_find_program_in_path (prog))
		return NULL;
	return g_strdup (prog);
}
#endif

GError *
go_url_show (gchar const *url)
{
#ifdef G_OS_WIN32
	ShellExecute (NULL, "open", url, NULL, NULL, SW_SHOWNORMAL);

	return NULL;
#else
	GError *err = NULL;
#ifdef GOFFICE_WITH_GNOME
	gnome_url_show (url, &err);
	return err;
#else
	guint8 *browser = NULL;
	guint8 *clean_url = NULL;

	/* 1) Check BROWSER env var */
	browser = check_program (getenv ("BROWSER"));

	if (browser == NULL) {
		static char const * const browsers[] = {
			"sensible-browser",	/* debian */
			"epiphany",		/* primary gnome */
			"galeon",		/* secondary gnome */
			"encompass",
			"firefox",
			"mozilla-firebird",
			"mozilla",
			"netscape",
			"konqueror",
			"xterm -e w3m",
			"xterm -e lynx",
			"xterm -e links"
		};
		unsigned i;
		for (i = 0 ; i < G_N_ELEMENTS (browsers) ; i++)
			if (NULL != (browser = check_program (browsers[i])))
				break;
  	}

	if (browser != NULL) {
		gint    argc;
		gchar **argv = NULL;
		char   *cmd_line = g_strconcat (browser, " %1", NULL);

		if (g_shell_parse_argv (cmd_line, &argc, &argv, &err)) {
			/* check for '%1' in an argument and substitute the url
			 * otherwise append it */
			gint i;
			char *tmp;

			for (i = 1 ; i < argc ; i++)
				if (NULL != (tmp = strstr (argv[i], "%1"))) {
					*tmp = '\0';
					tmp = g_strconcat (argv[i],
						(clean_url != NULL) ? (char const *)clean_url : url,
						tmp+2, NULL);
					g_free (argv[i]);
					argv[i] = tmp;
					break;
				}

			/* there was actually a %1, drop the one we added */
			if (i != argc-1) {
				g_free (argv[argc-1]);
				argv[argc-1] = NULL;
			}
			g_spawn_async (NULL, argv, NULL, G_SPAWN_SEARCH_PATH,
				NULL, NULL, NULL, &err);
			g_strfreev (argv);
		}
		g_free (cmd_line);
	}
	g_free (browser);
	g_free (clean_url);
	return err;
#endif
#endif
}

/**
 * go_url_check_extension
 * @uri     : Uri
 * @std_ext : Standard extension for the content type
 * @new_uri : New uri
 *
 * Modifies given @uri by adding the extension @std_ext if needed.
 * If no @std_ext is given or @uri already has some extension,
 * it just copies @uri.
 *
 * Value in new_uri:  newly allocated string which you should free after
 *                    use, containing (optionally) modified uri.
 *
 * Return Value:  FALSE if the uri has an extension not matching @std_ext
 */
gboolean
go_url_check_extension (gchar const *uri,
			gchar const *std_ext,
			gchar **new_uri)
{
	gchar *base;
	gchar *user_ext;
	gboolean res;

	g_return_val_if_fail (uri != NULL, FALSE);
	g_return_val_if_fail (new_uri != NULL, FALSE);

	res      = TRUE;
	base     = g_path_get_basename (uri);
	user_ext = strrchr (base, '.');
	if (std_ext != NULL && strlen (std_ext) > 0 && user_ext == NULL)
		*new_uri = g_strconcat (uri, ".", std_ext, NULL);
	else {
		if (user_ext != NULL && std_ext != NULL)
			res = !go_utf8_collate_casefold (user_ext + 1, std_ext);
		*new_uri = g_strdup (uri);
	}
	g_free (base);

	return res;
}

gchar *
go_get_mime_type (gchar const *uri)
{
#ifdef GOFFICE_WITH_GNOME
	return gnome_vfs_get_mime_type (uri);
#elif defined(G_OS_WIN32)
	LPWSTR wuri, mime_type;

	wuri = g_utf8_to_utf16 (uri, -1, NULL, NULL, NULL);
	if (wuri &&
	    FindMimeFromData (NULL, wuri, NULL, 0, NULL, 0, &mime_type, 0) == NOERROR)
	{
		g_free (wuri);
		return g_utf16_to_utf8 (mime_type, -1, NULL, NULL, NULL);
	}

	g_free (wuri);

	/* We try to determine mime using FindMimeFromData().
	 * However, we are not sure whether the functions will know about
	 * the necessary mime types. In the worst wase we fall back to
	 * "text/plain"
	 */
	return g_strdup ("text/plain");
#else
	return g_strdup ("application/octet-stream");
#endif
}

gchar
*go_get_mime_type_for_data	(gconstpointer data, int data_size)
{
#ifdef GOFFICE_WITH_GNOME
	return g_strdup (gnome_vfs_get_mime_type_for_data (data, data_size));
#else
	return g_strdup ("application/octet-stream");
#endif
}

gchar const
*go_mime_type_get_description	(gchar const *mime_type)
{
#ifdef GOFFICE_WITH_GNOME
	return gnome_vfs_mime_get_description (mime_type);
#else
	return mime_type;
#endif
}

/* ------------------------------------------------------------------------- */

#ifdef G_OS_WIN32
static gchar **saved_args;
static int saved_argc;
#endif

gchar const **
go_shell_argv_to_glib_encoding (gint argc, gchar const **argv)
{
#ifdef G_OS_WIN32
	gchar **args;
	gint i;

	args = g_new (gchar *, argc);
	if (G_WIN32_IS_NT_BASED ())
	{
		LPWSTR *wargs;
		gint narg;
		GIConv conv;

		wargs = CommandLineToArgvW (GetCommandLineW (), &narg);
		conv = g_iconv_open ("utf-8", "utf-16le");
		for (i = 0; i < narg; ++i)
			args[i] = g_convert_with_iconv ((const gchar *) wargs[i], wcslen (wargs[i]) << 1, conv, NULL, NULL, NULL);
		g_iconv_close (conv);
	}
	else
	{
		for (i = 0; i < argc; ++i)
			args[i] = g_locale_to_utf8 (argv[i], -1, NULL, NULL, NULL);
	}

	saved_args = args;
	saved_argc = argc;

	return (gchar const **) args;
#else
	return argv;
#endif
}

void
go_shell_argv_to_glib_encoding_free (void)
{
#ifdef G_OS_WIN32
	if (saved_args) {
		gint i;

		for (i = 0; i < saved_argc; ++i)
			g_free (saved_args[i]);
		g_free (saved_args);
	}
#endif
}

gint
go_file_access (char const *uri, gint mode)
{
	gint ret;
	gchar *filename;

	filename = go_filename_from_uri (uri);
	if (!filename)
		return -1;

#ifdef G_OS_WIN32
#  ifndef HAVE_G_ACCESS
#    error "A glib with g_access is required for Win32"
#  else
	/* FIXME FIXME FIXME Use Security API instead of checking file attributes only on NT-based environment */
	ret = g_access (filename, mode);
#  endif
#else
	ret = access (filename, mode);
#endif
	
	g_free (filename);
	
	return ret;
}
