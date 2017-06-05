/*
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#ifndef GCR_MEMORY_ICON_H
#define GCR_MEMORY_ICON_H

#include <glib-object.h>
#include <gio/gio.h>

G_BEGIN_DECLS

#define GCR_MEMORY_ICON_COLUMNS            (_gcr_memory_icon_get_columns ())
#define GCR_TYPE_MEMORY_ICON               (_gcr_memory_icon_get_type ())
#define GCR_MEMORY_ICON(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_MEMORY_ICON, GcrMemoryIcon))
#define GCR_MEMORY_ICON_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_MEMORY_ICON, GcrMemoryIconClass))
#define GCR_IS_MEMORY_ICON(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_MEMORY_ICON))
#define GCR_IS_MEMORY_ICON_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_MEMORY_ICON))
#define GCR_MEMORY_ICON_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_MEMORY_ICON, GcrMemoryIconClass))

typedef struct _GcrMemoryIcon GcrMemoryIcon;
typedef struct _GcrMemoryIconClass GcrMemoryIconClass;
typedef struct _GcrMemoryIconPrivate GcrMemoryIconPrivate;

struct _GcrMemoryIcon {
	GObject parent;

	/*< private >*/
	GcrMemoryIconPrivate *pv;
};

struct _GcrMemoryIconClass {
	GObjectClass parent_class;
};

GType               _gcr_memory_icon_get_type             (void) G_GNUC_CONST;

GIcon*              _gcr_memory_icon_new                  (const gchar *image_type,
                                                           gconstpointer data,
                                                           gsize n_data);

GIcon*              _gcr_memory_icon_new_full             (const gchar *image_type,
                                                           gpointer data,
                                                           gsize n_data,
                                                           goffset offset,
                                                           GDestroyNotify destroy);

G_END_DECLS

#endif /* __GCR_MEMORY_ICON_H__ */
