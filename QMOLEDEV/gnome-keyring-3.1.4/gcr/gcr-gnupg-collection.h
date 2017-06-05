/*
 * gnome-keyring
 *
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

#ifndef GCR_GNUPG_COLLECTION_H
#define GCR_GNUPG_COLLECTION_H

#include "gcr.h"
#include "gcr-collection.h"

#include <glib-object.h>

G_BEGIN_DECLS

#define GCR_TYPE_GNUPG_COLLECTION               (_gcr_gnupg_collection_get_type ())
#define GCR_GNUPG_COLLECTION(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_GNUPG_COLLECTION, GcrGnupgCollection))
#define GCR_GNUPG_COLLECTION_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_GNUPG_COLLECTION, GcrGnupgCollectionClass))
#define GCR_IS_GNUPG_COLLECTION(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_GNUPG_COLLECTION))
#define GCR_IS_GNUPG_COLLECTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_GNUPG_COLLECTION))
#define GCR_GNUPG_COLLECTION_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_GNUPG_COLLECTION, GcrGnupgCollectionClass))

typedef struct _GcrGnupgCollection GcrGnupgCollection;
typedef struct _GcrGnupgCollectionClass GcrGnupgCollectionClass;
typedef struct _GcrGnupgCollectionPrivate GcrGnupgCollectionPrivate;

struct _GcrGnupgCollection {
	GObject parent;
	GcrGnupgCollectionPrivate *pv;
};

struct _GcrGnupgCollectionClass {
	GObjectClass parent_class;
};

GType               _gcr_gnupg_collection_get_type                (void);

GcrCollection*      _gcr_gnupg_collection_new                     (const gchar *directory);

void                _gcr_gnupg_collection_load_async              (GcrGnupgCollection *self,
                                                                   GCancellable *cancellable,
                                                                   GAsyncReadyCallback callback,
                                                                   gpointer user_data);

gboolean            _gcr_gnupg_collection_load_finish             (GcrGnupgCollection *self,
                                                                   GAsyncResult *result,
                                                                   GError **error);

G_END_DECLS

#endif /* GCR_GNUPG_COLLECTION_H */
