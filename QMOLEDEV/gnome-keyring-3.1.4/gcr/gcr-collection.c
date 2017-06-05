/*
 * gnome-keyring
 *
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

#include "config.h"

#include "gcr-collection.h"

/**
 * SECTION:gcr-collection
 * @title: GcrCollection
 * @short_description: A collection of objects.
 *
 * A #GcrCollection is used to group a set of objects. This is an abstract
 * interface which can be used to determine which objects show up in a selector
 * or other user interface element.
 *
 * Use gcr_simple_collection_new() to create a concrete implementation of this
 * interface which you can add objects to.
 */

/**
 * GcrCollection:
 *
 * A #GcrCollection is used to group a set of objects.
 */

enum {
	ADDED,
	REMOVED,
	LAST_SIGNAL,
};

static guint signals[LAST_SIGNAL] = { 0 };

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

/* ---------------------------------------------------------------------------------
 * INTERFACE
 */

static void
gcr_collection_base_init (gpointer g_class)
{
	static volatile gsize initialized = 0;

	if (g_once_init_enter (&initialized)) {

		signals[ADDED] = g_signal_new ("added", GCR_TYPE_COLLECTION,
		                               G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GcrCollectionIface, added),
		                               NULL, NULL, g_cclosure_marshal_VOID__OBJECT,
		                               G_TYPE_NONE, 1, G_TYPE_OBJECT);

		signals[REMOVED] = g_signal_new ("removed", GCR_TYPE_COLLECTION,
		                                 G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET (GcrCollectionIface, removed),
		                                 NULL, NULL, g_cclosure_marshal_VOID__OBJECT,
		                                 G_TYPE_NONE, 1, G_TYPE_OBJECT);

		g_once_init_leave (&initialized, 1);
	}
}

GType
gcr_collection_get_type (void)
{
	static GType type = 0;
	if (!type) {
		static const GTypeInfo info = {
			sizeof (GcrCollectionIface),
			gcr_collection_base_init,               /* base init */
			NULL,             /* base finalize */
			NULL,             /* class_init */
			NULL,             /* class finalize */
			NULL,             /* class data */
			0,
			0,                /* n_preallocs */
			NULL,             /* instance init */
		};
		type = g_type_register_static (G_TYPE_INTERFACE, "GcrCollectionIface", &info, 0);
		g_type_interface_add_prerequisite (type, G_TYPE_OBJECT);
	}

	return type;
}


/* -----------------------------------------------------------------------------
 * PUBLIC
 */


/**
 * gcr_collection_get_length:
 * @self: The collection
 *
 * Get the number of objects in this collection.
 *
 * Returns: The number of objects.
 */
guint
gcr_collection_get_length (GcrCollection *self)
{
	g_return_val_if_fail (GCR_IS_COLLECTION (self), 0);
	g_return_val_if_fail (GCR_COLLECTION_GET_INTERFACE (self)->get_length, 0);
	return GCR_COLLECTION_GET_INTERFACE (self)->get_length (self);
}

/**
 * gcr_collection_get_objects:
 * @self: The collection
 *
 * Get a list of the objects in this collection.
 *
 * Returns: A list of the objects in this collection, which should be freed
 *     with g_list_free().
 */
GList*
gcr_collection_get_objects (GcrCollection *self)
{
	g_return_val_if_fail (GCR_IS_COLLECTION (self), 0);
	g_return_val_if_fail (GCR_COLLECTION_GET_INTERFACE (self)->get_objects, 0);
	return GCR_COLLECTION_GET_INTERFACE (self)->get_objects (self);
}

/**
 * gcr_collection_emit_added:
 * @self: The collection
 * @object: The object that was added
 *
 * Emit the GcrCollection::added signal for the given object. This function
 * is used by implementors of this interface.
 */
void
gcr_collection_emit_added (GcrCollection *self, GObject *object)
{
	g_return_if_fail (GCR_IS_COLLECTION (self));
	g_signal_emit (self, signals[ADDED], 0, object);
}

/**
 * gcr_collection_emit_removed:
 * @self: The collection
 * @object: The object that was removed
 *
 * Emit the GcrCollection::removed signal for the given object. This function
 * is used by implementors of this interface.
 */
void
gcr_collection_emit_removed (GcrCollection *self, GObject *object)
{
	g_return_if_fail (GCR_IS_COLLECTION (self));
	g_signal_emit (self, signals[REMOVED], 0, object);
}
