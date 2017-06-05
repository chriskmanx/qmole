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
#include "gcr-internal.h"
#include "gcr-simple-collection.h"

#include <string.h>

/**
 * SECTION:gcr-simple-collection
 * @title: GcrSimpleCollection
 * @short_description: A simple implementation of GcrCollection
 *
 * A simple implementation of #GcrCollection, which you can add and remove
 * objects from. Use gcr_simple_collection_add() to do this
 * gcr_simple_collection_remove().
 */

/**
 * GcrSimpleCollection:
 * @parent: The parent object
 *
 * A simple implementation of #GcrCollection.
 */

/**
 * GcrSimpleCollectionClass:
 * @parent_class: The parent class
 *
 * The class for #GcrSimpleCollection.
 */

struct _GcrSimpleCollectionPrivate {
	GHashTable *items;
};

static void gcr_collection_iface (GcrCollectionIface *iface);
G_DEFINE_TYPE_WITH_CODE (GcrSimpleCollection, gcr_simple_collection, G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (GCR_TYPE_COLLECTION, gcr_collection_iface));

#define UNUSED_VALUE  GUINT_TO_POINTER (1)

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gcr_simple_collection_init (GcrSimpleCollection *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_SIMPLE_COLLECTION, GcrSimpleCollectionPrivate);
	self->pv->items = g_hash_table_new_full (g_direct_hash, g_direct_equal, g_object_unref, NULL);
}

static void
gcr_simple_collection_dispose (GObject *obj)
{
	GcrSimpleCollection *self = GCR_SIMPLE_COLLECTION (obj);

	g_hash_table_remove_all (self->pv->items);

	G_OBJECT_CLASS (gcr_simple_collection_parent_class)->dispose (obj);
}

static void
gcr_simple_collection_finalize (GObject *obj)
{
	GcrSimpleCollection *self = GCR_SIMPLE_COLLECTION (obj);

	g_assert (self->pv->items);
	g_assert (g_hash_table_size (self->pv->items) == 0);
	g_hash_table_destroy (self->pv->items);
	self->pv->items = NULL;

	G_OBJECT_CLASS (gcr_simple_collection_parent_class)->finalize (obj);
}

static void
gcr_simple_collection_class_init (GcrSimpleCollectionClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	gobject_class->dispose = gcr_simple_collection_dispose;
	gobject_class->finalize = gcr_simple_collection_finalize;
	g_type_class_add_private (gobject_class, sizeof (GcrSimpleCollectionPrivate));
	_gcr_initialize ();
}

static guint
gcr_simple_collection_real_get_length (GcrCollection *coll)
{
	GcrSimpleCollection *self = GCR_SIMPLE_COLLECTION (coll);
	return g_hash_table_size (self->pv->items);
}

static GList*
gcr_simple_collection_real_get_objects (GcrCollection *coll)
{
	GcrSimpleCollection *self = GCR_SIMPLE_COLLECTION (coll);
	return g_hash_table_get_keys (self->pv->items);
}

static void
gcr_collection_iface (GcrCollectionIface *iface)
{
	iface->get_length = gcr_simple_collection_real_get_length;
	iface->get_objects = gcr_simple_collection_real_get_objects;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

/**
 * gcr_simple_collection_new:
 *
 * Create a new #GcrSimpleCollection.
 *
 * Returns: A newly allocated collection, which should be freed with
 *     g_object_unref().
 */
GcrCollection*
gcr_simple_collection_new (void)
{
	return g_object_new (GCR_TYPE_SIMPLE_COLLECTION, NULL);
}

/**
 * gcr_simple_collection_add:
 * @self: The collection
 * @object: The object to add
 *
 * Add an object to this collection
 */
void
gcr_simple_collection_add (GcrSimpleCollection *self, GObject *object)
{
	g_return_if_fail (GCR_IS_SIMPLE_COLLECTION (self));
	g_return_if_fail (G_IS_OBJECT (object));
	g_return_if_fail (!g_hash_table_lookup (self->pv->items, object));
	g_hash_table_insert (self->pv->items, g_object_ref (object), UNUSED_VALUE);
	gcr_collection_emit_added (GCR_COLLECTION (self), object);
}

/**
 * gcr_simple_collection_remove:
 * @self: The collection
 * @object: The object to remove from the collection
 *
 * Remove an object from the collection.
 */
void
gcr_simple_collection_remove (GcrSimpleCollection *self, GObject *object)
{
	g_return_if_fail (GCR_IS_SIMPLE_COLLECTION (self));
	g_return_if_fail (G_IS_OBJECT (object));
	g_return_if_fail (g_hash_table_lookup (self->pv->items, object));
	g_object_ref (object);
	g_hash_table_remove (self->pv->items, object);
	gcr_collection_emit_removed (GCR_COLLECTION (self), object);
	g_object_unref (object);
}

/**
 * gcr_simple_collection_contains:
 * @self: The collection
 * @object: The object to check
 *
 * Check if the collection contains a certain object.
 *
 * Returns: %TRUE if the collection contains the object.
 */
gboolean
gcr_simple_collection_contains (GcrSimpleCollection *self, GObject *object)
{
	g_return_val_if_fail (GCR_IS_SIMPLE_COLLECTION (self), FALSE);
	g_return_val_if_fail (G_IS_OBJECT (object), FALSE);
	return g_hash_table_lookup (self->pv->items, object) ? TRUE : FALSE;
}
