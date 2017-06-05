/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
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

#include "gkm-attributes.h"
#include "gkm-object.h"
#include "gkm-memory-store.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

struct _GkmMemoryStore {
	GkmStore parent;
	GHashTable *entries;
};

typedef struct _Revert {
	GHashTable *attributes;
	CK_ATTRIBUTE_TYPE type;
	CK_ATTRIBUTE_PTR attr;
} Revert;

G_DEFINE_TYPE (GkmMemoryStore, gkm_memory_store, GKM_TYPE_STORE);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
attribute_free (gpointer data)
{
	CK_ATTRIBUTE_PTR attr = data;
	if (attr) {
		g_free (attr->pValue);
		g_slice_free (CK_ATTRIBUTE, attr);
	}
}

static CK_ATTRIBUTE_PTR
attribute_dup (CK_ATTRIBUTE_PTR attr)
{
	CK_ATTRIBUTE_PTR copy;
	g_assert (attr);
	copy = g_slice_new (CK_ATTRIBUTE);
	copy->ulValueLen = attr->ulValueLen;
	copy->pValue = g_memdup (attr->pValue, copy->ulValueLen);
	copy->type = attr->type;
	return copy;
}

static void
object_gone (gpointer data, GObject *was_object)
{
	GkmMemoryStore *self;

	g_assert (GKM_IS_MEMORY_STORE (data));
	self = GKM_MEMORY_STORE (data);

	if (!g_hash_table_remove (self->entries, was_object))
		g_return_if_reached ();
}

static gboolean
remove_each_object (gpointer key, gpointer value, gpointer user_data)
{
	g_assert (GKM_IS_OBJECT (key));
	g_assert (GKM_IS_MEMORY_STORE (user_data));

	g_object_weak_unref (key, object_gone, user_data);
	return TRUE;
}

static gboolean
complete_set (GkmTransaction *transaction, GkmObject *object, Revert *revert)
{
	g_assert (GKM_IS_OBJECT (object));

	if (gkm_transaction_get_failed (transaction)) {
		if (revert->attr)
			g_hash_table_replace (revert->attributes, &(revert->attr->type), revert->attr);
		else
			g_hash_table_remove (revert->attributes, &(revert->type));

		gkm_object_notify_attribute (object, revert->type);

		revert->attr = NULL;
		revert->type = 0;
	}

	g_hash_table_unref (revert->attributes);
	attribute_free (revert->attr);
	g_slice_free (Revert, revert);
	return TRUE;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_memory_store_real_read_value (GkmStore *base, GkmObject *object, CK_ATTRIBUTE_PTR attr)
{
	GkmMemoryStore *self = GKM_MEMORY_STORE (base);
	GHashTable *attributes;
	CK_ATTRIBUTE_PTR at;

	attributes = g_hash_table_lookup (self->entries, object);
	if (attributes == NULL)
		return CKR_ATTRIBUTE_TYPE_INVALID;

	at = g_hash_table_lookup (attributes, &(attr->type));
	if (at == NULL)
		return CKR_ATTRIBUTE_TYPE_INVALID;

	g_assert (at->type == attr->type);

	/* Yes, we don't fill a buffer, just return pointer */
	attr->pValue = at->pValue;
	attr->ulValueLen = at->ulValueLen;

	return CKR_OK;
}

static void
gkm_memory_store_real_write_value (GkmStore *base, GkmTransaction *transaction,
                                   GkmObject *object, CK_ATTRIBUTE_PTR attr)
{
	GkmMemoryStore *self = GKM_MEMORY_STORE (base);
	GHashTable *attributes;
	CK_ATTRIBUTE_PTR at;
	Revert *revert;

	g_return_if_fail (!gkm_transaction_get_failed (transaction));

	attributes = g_hash_table_lookup (self->entries, object);
	if (attributes == NULL) {
		g_object_weak_ref (G_OBJECT (object), object_gone, self);
		attributes = g_hash_table_new_full (gkm_util_ulong_hash, gkm_util_ulong_equal,
		                                    NULL, attribute_free);
		g_hash_table_replace (self->entries, object, attributes);
	}

	/* No need to go any further if no change */
	at = g_hash_table_lookup (attributes, &(attr->type));
	if (at != NULL && gkm_attribute_equal (at, attr))
		return;

	revert = g_slice_new0 (Revert);
	revert->attributes = g_hash_table_ref (attributes);
	revert->type = attr->type;
	revert->attr = at;
	g_hash_table_steal (attributes, &(attr->type));
	gkm_transaction_add (transaction, object, (GkmTransactionFunc)complete_set, revert);

	attr = attribute_dup (attr);
	g_hash_table_replace (attributes, &(attr->type), attr);
	gkm_object_notify_attribute (object, attr->type);
}

static GObject*
gkm_memory_store_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmMemoryStore *self = GKM_MEMORY_STORE (G_OBJECT_CLASS (gkm_memory_store_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	return G_OBJECT (self);
}

static void
gkm_memory_store_init (GkmMemoryStore *self)
{
	self->entries = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, (GDestroyNotify)g_hash_table_unref);
}

static void
gkm_memory_store_dispose (GObject *obj)
{
	GkmMemoryStore *self = GKM_MEMORY_STORE (obj);

	g_hash_table_foreach_remove (self->entries, remove_each_object, self);

	G_OBJECT_CLASS (gkm_memory_store_parent_class)->dispose (obj);
}

static void
gkm_memory_store_finalize (GObject *obj)
{
	GkmMemoryStore *self = GKM_MEMORY_STORE (obj);

	g_assert (g_hash_table_size (self->entries) == 0);
	g_hash_table_destroy (self->entries);
	self->entries = NULL;

	G_OBJECT_CLASS (gkm_memory_store_parent_class)->finalize (obj);
}

static void
gkm_memory_store_set_property (GObject *obj, guint prop_id, const GValue *value,
                               GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_memory_store_get_property (GObject *obj, guint prop_id, GValue *value,
                               GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_memory_store_class_init (GkmMemoryStoreClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmStoreClass *store_class = GKM_STORE_CLASS (klass);

	gobject_class->constructor = gkm_memory_store_constructor;
	gobject_class->dispose = gkm_memory_store_dispose;
	gobject_class->finalize = gkm_memory_store_finalize;
	gobject_class->set_property = gkm_memory_store_set_property;
	gobject_class->get_property = gkm_memory_store_get_property;

	store_class->read_value = gkm_memory_store_real_read_value;
	store_class->write_value = gkm_memory_store_real_write_value;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmMemoryStore*
gkm_memory_store_new (void)
{
	return g_object_new (GKM_TYPE_MEMORY_STORE, NULL);
}
