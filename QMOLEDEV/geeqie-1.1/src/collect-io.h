/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef COLLECT_IO_H
#define COLLECT_IO_H

typedef enum {
	COLLECTION_LOAD_NONE	= 0,
	COLLECTION_LOAD_APPEND	= 1 << 0,
	COLLECTION_LOAD_FLUSH	= 1 << 1,
	COLLECTION_LOAD_GEOMETRY= 1 << 2,
} CollectionLoadFlags;

gboolean collection_load(CollectionData *cd, const gchar *path, CollectionLoadFlags flags);

gboolean collection_load_begin(CollectionData *cd, const gchar *path, CollectionLoadFlags flags);
void collection_load_stop(CollectionData *cd);

void collection_load_thumb_idle(CollectionData *cd);

gboolean collection_save(CollectionData *cd, const gchar *path);

gboolean collection_load_only_geometry(CollectionData *cd, const gchar *path);


/* these are used to update collections contained in user's collection
 * folder when moving or renaming files.
 * also handles:
 *   deletes file when newpath == NULL
 *   adds file when oldpath == NULL
 */
void collect_manager_moved(FileData *fd);

/* add or removing from a specific collection */
void collect_manager_add(FileData *fd, const gchar *collection);
void collect_manager_remove(FileData *fd, const gchar *collection);

/* commit pending operations to disk */
void collect_manager_flush(void);

void collect_manager_notify_cb(FileData *fd, NotifyType type, gpointer data);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
