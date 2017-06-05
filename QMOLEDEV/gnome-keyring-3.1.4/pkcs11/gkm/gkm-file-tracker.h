/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkm-file-tracker.h - Watch for changes in a directory

   Copyright (C) 2008, Stefan Walter

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

#ifndef __GKM_FILE_TRACKER_H__
#define __GKM_FILE_TRACKER_H__

#include <glib-object.h>

#include "gkm-file-tracker.h"

G_BEGIN_DECLS

#define GKM_TYPE_FILE_TRACKER             (gkm_file_tracker_get_type ())
#define GKM_FILE_TRACKER(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_FILE_TRACKER, GkmFileTracker))
#define GKM_FILE_TRACKER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_FILE_TRACKER, GObject))
#define GKM_IS_FILE_TRACKER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_FILE_TRACKER))
#define GKM_IS_FILE_TRACKER_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_FILE_TRACKER))
#define GKM_FILE_TRACKER_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_FILE_TRACKER, GkmFileTrackerClass))

typedef struct _GkmFileTracker GkmFileTracker;
typedef struct _GkmFileTrackerClass GkmFileTrackerClass;

struct _GkmFileTrackerClass {
	GObjectClass parent_class;

	void (*file_added) (GkmFileTracker *locmgr, const gchar *path);
	void (*file_changed) (GkmFileTracker *locmgr, const gchar *path);
	void (*file_removed) (GkmFileTracker *locmgr, const gchar *path);
};

GType                    gkm_file_tracker_get_type             (void) G_GNUC_CONST;

GkmFileTracker*          gkm_file_tracker_new                  (const gchar *directory,
                                                                const gchar *include_pattern,
                                                                const gchar *exclude_pattern);

void                     gkm_file_tracker_refresh              (GkmFileTracker *self,
                                                                gboolean force_all);

G_END_DECLS

#endif /* __GKM_FILE_TRACKER_H__ */
