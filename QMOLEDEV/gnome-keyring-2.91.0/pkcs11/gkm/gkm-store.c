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
#include "gkm-store.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

typedef struct _Schema {
	CK_ATTRIBUTE_TYPE type;
	gpointer default_value;
	gsize default_length;
	GkmStoreValidator validator;
	guint flags;
} Schema;

struct _GkmStorePrivate {
	GHashTable *schemas;
};

G_DEFINE_TYPE (GkmStore, gkm_store, G_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
schema_free (gpointer data)
{
	Schema *schema;

	if (data == NULL)
		return;

	schema = data;
	g_free (schema->default_value);
	g_slice_free (Schema, schema);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static GObject*
gkm_store_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmStore *self = GKM_STORE (G_OBJECT_CLASS (gkm_store_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	return G_OBJECT (self);
}

static void
gkm_store_init (GkmStore *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_STORE, GkmStorePrivate);
	self->pv->schemas = g_hash_table_new_full (gkm_util_ulong_hash, gkm_util_ulong_equal,
	                                           NULL, schema_free);
}

static void
gkm_store_dispose (GObject *obj)
{
	GkmStore *self = GKM_STORE (obj);

	g_hash_table_remove_all (self->pv->schemas);

	G_OBJECT_CLASS (gkm_store_parent_class)->dispose (obj);
}

static void
gkm_store_finalize (GObject *obj)
{
	GkmStore *self = GKM_STORE (obj);

	g_hash_table_destroy (self->pv->schemas);

	G_OBJECT_CLASS (gkm_store_parent_class)->finalize (obj);
}

static void
gkm_store_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_store_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_store_class_init (GkmStoreClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->constructor = gkm_store_constructor;
	gobject_class->dispose = gkm_store_dispose;
	gobject_class->finalize = gkm_store_finalize;
	gobject_class->set_property = gkm_store_set_property;
	gobject_class->get_property = gkm_store_get_property;

	g_type_class_add_private (klass, sizeof (GkmStorePrivate));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

gconstpointer
gkm_store_read_value (GkmStore *self, GkmObject *object,
                      CK_ATTRIBUTE_TYPE type, gsize *n_value)
{
	CK_ATTRIBUTE at;
	Schema *schema;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_STORE (self), NULL);
	g_return_val_if_fail (GKM_IS_OBJECT (object), NULL);
	g_return_val_if_fail (n_value, NULL);

	g_assert (GKM_STORE_GET_CLASS (self)->read_value);

	schema = g_hash_table_lookup (self->pv->schemas, &type);
	if (schema == NULL)
		return NULL;

	at.type = type;
	at.pValue = NULL;
	at.ulValueLen = 0;

	rv = GKM_STORE_GET_CLASS (self)->read_value (self, object, &at);
	if (rv == CKR_ATTRIBUTE_TYPE_INVALID || rv == CKR_USER_NOT_LOGGED_IN) {
		at.pValue = schema->default_value;
		at.ulValueLen = schema->default_length;
	} else if (rv != CKR_OK) {
		g_return_val_if_reached (NULL);
	}

	*n_value = at.ulValueLen;
	return at.pValue;
}

gchar*
gkm_store_read_string (GkmStore *self, GkmObject *object, CK_ATTRIBUTE_TYPE type)
{
	gconstpointer value;
	gsize n_value;

	g_return_val_if_fail (GKM_IS_STORE (self), NULL);
	g_return_val_if_fail (GKM_IS_OBJECT (object), NULL);

	value = gkm_store_read_value (self, object, type, &n_value);
	if (!value)
		return NULL;

	return g_strndup (value, n_value);
}

CK_RV
gkm_store_get_attribute (GkmStore *self, GkmObject *object, CK_ATTRIBUTE_PTR attr)
{
	CK_ATTRIBUTE at;
	Schema *schema;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_STORE (self), CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_OBJECT (object), CKR_GENERAL_ERROR);
	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);

	g_assert (GKM_STORE_GET_CLASS (self)->read_value);

	schema = g_hash_table_lookup (self->pv->schemas, &(attr->type));
	if (schema == NULL)
		return CKR_ATTRIBUTE_TYPE_INVALID;

	if (schema->flags & GKM_STORE_IS_INTERNAL)
		return CKR_ATTRIBUTE_TYPE_INVALID;

	if (schema->flags & GKM_STORE_IS_SENSITIVE)
		return CKR_ATTRIBUTE_SENSITIVE;

	at.type = attr->type;
	at.pValue = NULL;
	at.ulValueLen = 0;

	rv = GKM_STORE_GET_CLASS (self)->read_value (self, object, &at);
	if (rv == CKR_ATTRIBUTE_TYPE_INVALID) {
		at.pValue = schema->default_value;
		at.ulValueLen = schema->default_length;
	} else if (rv != CKR_OK) {
		return rv;
	}

	/*
	 * If we get an assert here, then the derived class is probably
	 * trying to fill the  buffer in the attribute passed. It should
	 * actually just be setting the pValue to its own buffers.
	 */
	g_assert (at.pValue || !at.ulValueLen);

	return gkm_attribute_set_data (attr, at.pValue, at.ulValueLen);
}

void
gkm_store_write_value (GkmStore *self, GkmTransaction *transaction,
                       GkmObject *object, CK_ATTRIBUTE_PTR attr)
{
	Schema *schema;

	g_return_if_fail (GKM_IS_STORE (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (GKM_IS_OBJECT (object));
	g_return_if_fail (attr);

	g_return_if_fail (!gkm_transaction_get_failed (transaction));
	g_assert (GKM_STORE_GET_CLASS (self)->write_value);

	schema = g_hash_table_lookup (self->pv->schemas, &(attr->type));
	if (schema == NULL) {
		gkm_transaction_fail (transaction, CKR_ATTRIBUTE_TYPE_INVALID);
		return;
	}

	GKM_STORE_GET_CLASS (self)->write_value (self, transaction, object, attr);
}

void
gkm_store_set_attribute (GkmStore *self, GkmTransaction *transaction,
                         GkmObject *object, CK_ATTRIBUTE_PTR attr)
{
	Schema *schema;
	CK_RV rv = CKR_OK;

	g_return_if_fail (GKM_IS_STORE (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (GKM_IS_OBJECT (object));
	g_return_if_fail (attr);

	g_return_if_fail (!gkm_transaction_get_failed (transaction));
	g_assert (GKM_STORE_GET_CLASS (self)->write_value);

	schema = g_hash_table_lookup (self->pv->schemas, &(attr->type));
	if (schema == NULL)
		rv = CKR_ATTRIBUTE_TYPE_INVALID;
	else if (schema->flags & GKM_STORE_IS_INTERNAL)
		rv = CKR_ATTRIBUTE_TYPE_INVALID;
	else if (schema->validator)
		rv = (schema->validator) (object, attr);

	if (rv != CKR_OK) {
		gkm_transaction_fail (transaction, rv);
		return;
	}

	GKM_STORE_GET_CLASS (self)->write_value (self, transaction, object, attr);
}

void
gkm_store_register_schema (GkmStore *self, CK_ATTRIBUTE_PTR attr,
                           GkmStoreValidator validator, guint flags)
{
	Schema *schema;

	g_return_if_fail (GKM_IS_STORE (self));
	g_return_if_fail (g_hash_table_lookup (self->pv->schemas, &(attr->type)) == NULL);
	g_return_if_fail (!attr->ulValueLen || attr->pValue);
	g_return_if_fail (attr->ulValueLen != (CK_ULONG)-1);

	schema = g_slice_new0 (Schema);
	schema->type = attr->type;
	schema->flags = flags;
	schema->validator = validator;
	schema->default_value = attr->pValue;
	schema->default_length = attr->ulValueLen;
	if (schema->default_value)
		schema->default_value = g_memdup (schema->default_value,
		                                  schema->default_length);

	g_hash_table_insert (self->pv->schemas, &(schema->type), schema);
}

gboolean
gkm_store_lookup_schema (GkmStore *self, CK_ATTRIBUTE_TYPE type, guint *flags)
{
	Schema *schema;

	g_return_val_if_fail (GKM_IS_STORE (self), FALSE);

	schema = g_hash_table_lookup (self->pv->schemas, &type);
	if (!schema)
		return FALSE;
	if (flags)
		*flags = schema->flags;
	return TRUE;
}
