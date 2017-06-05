/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkm-file-tracker.c - Watch for changes in a directory

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

#include "gkm-file-tracker.h"

#include "egg/egg-error.h"

#include <glib.h>
#include <glib/gstdio.h>

#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>

typedef struct _UpdateDescendants {
	GkmFileTracker *tracker;
	GHashTable *checks;
} UpdateDescendants;

struct _GkmFileTracker {
	GObject parent;

	/* Specification */
	GPatternSpec *include;
	GPatternSpec *exclude;
	gchar *directory_path;
	time_t directory_mtime;

	/* Matched files */
	GHashTable *files;
};

enum {
	FILE_ADDED,
	FILE_REMOVED,
	FILE_CHANGED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

G_DEFINE_TYPE (GkmFileTracker, gkm_file_tracker, G_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * HELPERS
 */

static void
copy_key_string (gpointer key, gpointer value, gpointer data)
{
	GHashTable *dest = (GHashTable*)data;
	g_hash_table_replace (dest, g_strdup (key), value);
}

static void
remove_files (gpointer key, gpointer value, gpointer data)
{
	GkmFileTracker *self = GKM_FILE_TRACKER (data);

	g_hash_table_remove (self->files, key);
	g_signal_emit (self, signals[FILE_REMOVED], 0, key);
}

static gboolean
update_file (GkmFileTracker *self, gboolean force_all, const gchar *path)
{
	time_t old_mtime;
	struct stat sb;

	if (stat (path, &sb) < 0) {
		if (errno != ENOENT && errno != ENOTDIR && errno != EPERM)
			g_warning ("couldn't stat file: %s: %s", path, g_strerror (errno));
		return FALSE;
	}

	old_mtime = GPOINTER_TO_UINT (g_hash_table_lookup (self->files, path));
	g_assert (old_mtime);

	/* See if it has actually changed */
	if (force_all || old_mtime != sb.st_mtime) {
		g_assert (g_hash_table_lookup (self->files, path));
		g_hash_table_insert (self->files, g_strdup (path), GUINT_TO_POINTER (sb.st_mtime));
		g_signal_emit (self, signals[FILE_CHANGED], 0, path);
	}

	return TRUE;
}

static void
update_each_file (gpointer key, gpointer unused, gpointer data)
{
	UpdateDescendants *ctx = (UpdateDescendants*)data;
	if (update_file (ctx->tracker, FALSE, key))
		g_hash_table_remove (ctx->checks, key);
}

static void
update_directory (GkmFileTracker *self, gboolean force_all, GHashTable *checks)
{
	UpdateDescendants uctx;
	struct stat sb;
	GError *err = NULL;
	const char *filename;
	gchar *file;
	GDir *dir;
	int ret, lasterr;

	g_assert (checks);
	g_assert (GKM_IS_FILE_TRACKER (self));

	if (!self->directory_path)
		return;

	if (stat (self->directory_path, &sb) < 0) {
		if (errno != ENOENT && errno != ENOTDIR && errno != EPERM)
			g_message ("couldn't stat directory: %s: %s",
			           self->directory_path, g_strerror (errno));
		return;
	}

	/* See if it was updated since last seen or not */
	if (!force_all && self->directory_mtime == sb.st_mtime) {

		uctx.checks = checks;
		uctx.tracker = self;

		/* Still need to check for individual file updates */
		g_hash_table_foreach (self->files, update_each_file, &uctx);
		return;
	}

	self->directory_mtime = sb.st_mtime;

	/* Actually list the directory */
	dir = g_dir_open (self->directory_path, 0, &err);
	if (dir == NULL) {
		if (errno != ENOENT && errno != ENOTDIR && errno != EPERM)
			g_message ("couldn't list keyrings at: %s: %s", self->directory_path,
			           egg_error_message (err));
		g_error_free (err);
		return;
	}

	while ((filename = g_dir_read_name (dir)) != NULL) {
		if (filename[0] == '.')
			continue;
		if (self->include && !g_pattern_match_string (self->include, filename))
			continue;
		if (self->exclude && g_pattern_match_string (self->exclude, filename))
			continue;

		file = g_build_filename (self->directory_path, filename, NULL);

		/* If we hadn't yet seen this, then add it */
		if (!g_hash_table_remove (checks, file)) {

			/* Get the last modified time for this one */
			ret = g_stat (file, &sb);
			lasterr = errno;

			/* Couldn't access the file */
			if (ret < 0) {
				g_message ("couldn't stat file: %s: %s", file, g_strerror (lasterr));

			} else {

				/* We don't do directories */
				if (!(sb.st_mode & S_IFDIR)) {
					g_hash_table_replace (self->files, g_strdup (file), GINT_TO_POINTER (sb.st_mtime));
					g_signal_emit (self, signals[FILE_ADDED], 0, file);
				}
			}

		/* Otherwise we already had it, see if it needs updating */
		} else {
			update_file (self, force_all, file);
		}

		g_free (file);
	}

	g_dir_close (dir);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gkm_file_tracker_init (GkmFileTracker *self)
{
	self->files = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
}

static void
gkm_file_tracker_finalize (GObject *obj)
{
	GkmFileTracker *self = GKM_FILE_TRACKER (obj);

	if (self->include)
		g_pattern_spec_free (self->include);
	if (self->exclude)
		g_pattern_spec_free (self->exclude);
	g_free (self->directory_path);

	g_hash_table_destroy (self->files);

	G_OBJECT_CLASS (gkm_file_tracker_parent_class)->finalize (obj);
}

static void
gkm_file_tracker_class_init (GkmFileTrackerClass *klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass*) klass;

	gkm_file_tracker_parent_class = g_type_class_peek_parent (klass);
	gobject_class->finalize = gkm_file_tracker_finalize;

	signals[FILE_ADDED] = g_signal_new ("file-added", GKM_TYPE_FILE_TRACKER,
			G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkmFileTrackerClass, file_added),
			NULL, NULL, g_cclosure_marshal_VOID__STRING,
			G_TYPE_NONE, 1, G_TYPE_STRING);

	signals[FILE_CHANGED] = g_signal_new ("file-changed", GKM_TYPE_FILE_TRACKER,
			G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkmFileTrackerClass, file_changed),
			NULL, NULL, g_cclosure_marshal_VOID__STRING,
			G_TYPE_NONE, 1, G_TYPE_STRING);

	signals[FILE_REMOVED] = g_signal_new ("file-removed", GKM_TYPE_FILE_TRACKER,
			G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkmFileTrackerClass, file_removed),
			NULL, NULL, g_cclosure_marshal_VOID__STRING,
			G_TYPE_NONE, 1, G_TYPE_STRING);
}

GkmFileTracker*
gkm_file_tracker_new (const gchar *directory, const gchar *include, const gchar *exclude)
{
	GkmFileTracker *self;
	const gchar *homedir;

	g_return_val_if_fail (directory, NULL);

	self = g_object_new (GKM_TYPE_FILE_TRACKER, NULL);

	/* TODO: Use properties */

	if (directory[0] == '~' && directory[1] == '/') {
		homedir = g_getenv ("HOME");
		if (!homedir)
			homedir = g_get_home_dir ();
		self->directory_path = g_build_filename (homedir, directory + 2, NULL);

	/* A relative or absolute path */
	} else {
		self->directory_path = g_strdup (directory);
	}

	self->include = include ? g_pattern_spec_new (include) : NULL;
	self->exclude = exclude ? g_pattern_spec_new (exclude) : NULL;

	return self;
}

void
gkm_file_tracker_refresh (GkmFileTracker *self, gboolean force_all)
{
	GHashTable *checks;

	g_return_if_fail (GKM_IS_FILE_TRACKER (self));

	/* Copy into our check set */
	checks = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
	g_hash_table_foreach (self->files, copy_key_string, checks);

	/* If only one volume, then just try and access it directly */
	update_directory (self, force_all, checks);

	/* Find any keyrings whose paths we didn't see */
	g_hash_table_foreach (checks, remove_files, self);
	g_hash_table_destroy (checks);
}
