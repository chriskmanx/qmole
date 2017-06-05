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

#ifndef PREFS_H
#define PREFS_H 1

#include <stdio.h>

#include <glib.h>

typedef struct _PrefFile	PrefFile;

struct _PrefFile {
	FILE *fp;
	FILE *orig_fp;
	gchar *path;
	gboolean writing;
};

PrefFile *prefs_read_open	(const gchar	*path);
PrefFile *prefs_write_open	(const gchar	*path);
gint prefs_file_close		(PrefFile	*pfile);
gint prefs_file_close_revert	(PrefFile	*pfile);
gboolean prefs_rc_is_readonly	(const gchar 	*rcfile);
gint prefs_set_block_label	(PrefFile       *pfile,
				 const gchar	*block_label);

#endif
