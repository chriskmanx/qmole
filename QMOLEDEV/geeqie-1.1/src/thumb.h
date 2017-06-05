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


#ifndef THUMB_H
#define THUMB_H


ThumbLoader *thumb_loader_new(gint width, gint height);
void thumb_loader_set_callbacks(ThumbLoader *tl,
				ThumbLoaderFunc func_done,
				ThumbLoaderFunc func_error,
				ThumbLoaderFunc func_progress,
				gpointer data);
void thumb_loader_set_cache(ThumbLoader *tl, gboolean enable_cache, gboolean local, gboolean retry_failed);

gboolean thumb_loader_start(ThumbLoader *tl, FileData *fd);
void thumb_loader_free(ThumbLoader *tl);

GdkPixbuf *thumb_loader_get_pixbuf(ThumbLoader *tl);

void thumb_notify_cb(FileData *fd, NotifyType type, gpointer data);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
