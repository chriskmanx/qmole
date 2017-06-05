/*
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef __GCR_VIEWER_H__
#define __GCR_VIEWER_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-types.h"

G_BEGIN_DECLS

#define GCR_TYPE_VIEWER                 (gcr_viewer_get_type())
#define GCR_VIEWER(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_VIEWER, GcrViewer))
#define GCR_IS_VIEWER(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_VIEWER))
#define GCR_VIEWER_GET_INTERFACE(inst)  (G_TYPE_INSTANCE_GET_INTERFACE ((inst), GCR_TYPE_VIEWER, GcrViewerIface))

typedef struct _GcrRenderer    GcrRenderer;
typedef struct _GcrViewer      GcrViewer;
typedef struct _GcrViewerIface GcrViewerIface;

struct _GcrViewerIface {
	GTypeInterface parent;

	void (*add_renderer) (GcrViewer *self, GcrRenderer *viewer);

	void (*remove_renderer) (GcrViewer *self, GcrRenderer *viewer);

	guint (*count_renderers) (GcrViewer *self);

	GcrRenderer* (*get_renderer) (GcrViewer *self, guint index_);

	/*< private >*/
	gpointer dummy1;
	gpointer dummy2;
	gpointer dummy3;
	gpointer dummy4;
};

GType                   gcr_viewer_get_type               (void);

GcrViewer*              gcr_viewer_new                    (void);

GcrViewer*              gcr_viewer_new_scrolled           (void);

void                    gcr_viewer_add_renderer           (GcrViewer *self,
                                                           GcrRenderer *renderer);

void                    gcr_viewer_remove_renderer        (GcrViewer *self,
                                                           GcrRenderer *renderer);

guint                   gcr_viewer_count_renderers        (GcrViewer *self);

GcrRenderer*            gcr_viewer_get_renderer           (GcrViewer *self,
                                                           guint index_);

G_END_DECLS

#endif /* __GCR_VIEWER_H__ */
