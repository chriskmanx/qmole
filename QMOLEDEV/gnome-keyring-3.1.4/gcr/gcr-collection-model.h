/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the
 * Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __GCR_COLLECTION_MODEL_H__
#define __GCR_COLLECTION_MODEL_H__

#include <gtk/gtk.h>

#include "gcr-collection.h"
#include "gcr-column.h"

#define GCR_TYPE_COLLECTION_MODEL               (gcr_collection_model_get_type ())
#define GCR_COLLECTION_MODEL(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_COLLECTION_MODEL, GcrCollectionModel))
#define GCR_COLLECTION_MODEL_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_COLLECTION_MODEL, GcrCollectionModelClass))
#define GCR_IS_COLLECTION_MODEL(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_COLLECTION_MODEL))
#define GCR_IS_COLLECTION_MODEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_COLLECTION_MODEL))
#define GCR_COLLECTION_MODEL_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_COLLECTION_MODEL, GcrCollectionModelClass))

typedef struct _GcrCollectionModel GcrCollectionModel;
typedef struct _GcrCollectionModelClass GcrCollectionModelClass;
typedef struct _GcrCollectionModelPrivate GcrCollectionModelPrivate;

struct _GcrCollectionModel {
	GObject parent;

	/*< private >*/
	GcrCollectionModelPrivate *pv;
};

struct _GcrCollectionModelClass {
	GObjectClass parent_class;
};

GType                 gcr_collection_model_get_type            (void);

GcrCollectionModel*   gcr_collection_model_new                 (GcrCollection *collection,
                                                                ...) G_GNUC_NULL_TERMINATED;

GcrCollectionModel*   gcr_collection_model_new_full            (GcrCollection *collection,
                                                                const GcrColumn *columns);

void                  gcr_collection_model_set_columns         (GcrCollectionModel *self,
                                                                const GcrColumn *columns);

GObject*              gcr_collection_model_object_for_iter     (GcrCollectionModel *self,
                                                                const GtkTreeIter *iter);

gboolean              gcr_collection_model_iter_for_object     (GcrCollectionModel *self,
                                                                GObject *object,
                                                                GtkTreeIter *iter);

gint                  gcr_collection_model_column_for_selected (GcrCollectionModel *self);

void                  gcr_collection_model_toggle_selected     (GcrCollectionModel *self,
                                                                GtkTreeIter *iter);

void                  gcr_collection_model_change_selected     (GcrCollectionModel *self,
                                                                GtkTreeIter *iter,
                                                                gboolean selected);

gboolean              gcr_collection_model_is_selected         (GcrCollectionModel *self,
                                                                GtkTreeIter *iter);

GList*                gcr_collection_model_get_selected_objects  (GcrCollectionModel *self);

void                  gcr_collection_model_set_selected_objects  (GcrCollectionModel *self,
                                                                  GList *selected);

#endif /* __GCR_COLLECTION_MODEL_H__ */
