/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#include "gkm-secret-data.h"
#include "gkm-secret-fields.h"
#include "gkm-secret-item.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-module.h"
#include "gkm/gkm-secret.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-transaction.h"

#include "pkcs11/pkcs11i.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_COLLECTION,
	PROP_FIELDS,
	PROP_SCHEMA
};

struct _GkmSecretItem {
	GkmSecretObject parent;
	GHashTable *fields;
	gchar *schema;
	GkmSecretCollection *collection;
};

G_DEFINE_TYPE (GkmSecretItem, gkm_secret_item, GKM_TYPE_SECRET_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static gboolean
complete_set_schema (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (obj);
	gchar *old_schema = user_data;

	if (gkm_transaction_get_failed (transaction)) {
		g_free (self->schema);
		self->schema = old_schema;
	} else {
		gkm_object_notify_attribute (GKM_OBJECT (obj), CKA_G_SCHEMA);
		g_object_notify (G_OBJECT (obj), "schema");
		gkm_secret_object_was_modified (GKM_SECRET_OBJECT (self));
		g_free (old_schema);
	}

	return TRUE;
}

static void
begin_set_schema (GkmSecretItem *self, GkmTransaction *transaction, gchar *schema)
{
	g_assert (GKM_IS_SECRET_OBJECT (self));
	g_assert (!gkm_transaction_get_failed (transaction));

	if (self->schema != schema) {
		gkm_transaction_add (transaction, self, complete_set_schema, self->schema);
		self->schema = schema;
	}
}

static gboolean
complete_set_secret (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (obj);

	if (!gkm_transaction_get_failed (transaction)) {
		gkm_object_notify_attribute (GKM_OBJECT (obj), CKA_VALUE);
		gkm_secret_object_was_modified (GKM_SECRET_OBJECT (self));
	}

	return TRUE;
}

static gboolean
complete_set_fields (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (obj);
	GHashTable *old_fields = user_data;

	if (gkm_transaction_get_failed (transaction)) {
		if (self->fields)
			g_hash_table_unref (self->fields);
		self->fields = old_fields;
	} else {
		gkm_object_notify_attribute (GKM_OBJECT (obj), CKA_G_FIELDS);
		g_object_notify (G_OBJECT (obj), "fields");
		gkm_secret_object_was_modified (GKM_SECRET_OBJECT (self));
		if (old_fields)
			g_hash_table_unref (old_fields);
	}

	return TRUE;
}

static void
begin_set_fields (GkmSecretItem *self, GkmTransaction *transaction, GHashTable *fields)
{
	g_assert (GKM_IS_SECRET_OBJECT (self));
	g_assert (!gkm_transaction_get_failed (transaction));

	gkm_transaction_add (transaction, self, complete_set_fields, self->fields);
	self->fields = fields;
}

static GkmObject*
factory_create_item (GkmSession *session, GkmTransaction *transaction,
                     CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmSecretCollection *collection = NULL;
	GkmSecretItem *item;
	GkmManager *m_manager;
	GkmManager *s_manager;
	CK_ATTRIBUTE *attr;
	gboolean is_token;
	gchar *identifier;

	g_return_val_if_fail (GKM_IS_TRANSACTION (transaction), NULL);
	g_return_val_if_fail (attrs || !n_attrs, NULL);

	/* See if a collection attribute was specified */
	attr = gkm_attributes_find (attrs, n_attrs, CKA_G_COLLECTION);
	if (attr == NULL) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	m_manager = gkm_module_get_manager (gkm_session_get_module (session));
	s_manager = gkm_session_get_manager (session);

	gkm_attribute_consume (attr);
	if (!gkm_attributes_find_boolean (attrs, n_attrs, CKA_TOKEN, &is_token))
		collection = gkm_secret_collection_find (session, attr, m_manager, s_manager, NULL);
	else if (is_token)
		collection = gkm_secret_collection_find (session, attr, m_manager, NULL);
	else
		collection = gkm_secret_collection_find (session, attr, s_manager, NULL);

	if (!collection) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
		return NULL;
	}

	/* If an ID was specified, then try and see if that ID already exists */
	if (gkm_attributes_find_string (attrs, n_attrs, CKA_ID, &identifier)) {
		item = gkm_secret_collection_get_item (collection, identifier);
		if (item == NULL) {
			gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
			return NULL;
		} else {
			gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (item),
			                                      FALSE, attrs, n_attrs);
			return g_object_ref (item);
		}
	}

	/* Create a new collection which will own the item */
	item = gkm_secret_collection_create_item (collection, transaction);
	gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (item),
	                                      TRUE, attrs, n_attrs);
	return g_object_ref (item);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static gboolean
gkm_secret_item_real_is_locked (GkmSecretObject *obj, GkmSession *session)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (obj);
	if (!self->collection)
		return TRUE;
	return gkm_secret_object_is_locked (GKM_SECRET_OBJECT (self->collection), session);
}

static CK_RV
gkm_secret_item_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (base);
	GkmSecretData *sdata;
	const gchar *identifier;
	const guchar *secret;
	gsize n_secret = 0;
	CK_RV rv;

	g_return_val_if_fail (self->collection, CKR_GENERAL_ERROR);

	switch (attr->type) {
	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_SECRET_KEY);

	case CKA_VALUE:
		sdata = gkm_secret_collection_unlocked_use (self->collection, session);
		if (sdata == NULL)
			return CKR_USER_NOT_LOGGED_IN;
		identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (self));
		secret = gkm_secret_data_get_raw (sdata, identifier, &n_secret);
		rv = gkm_attribute_set_data (attr, secret, n_secret);
		g_object_unref (sdata);
		return rv;

	case CKA_G_COLLECTION:
		g_return_val_if_fail (self->collection, CKR_GENERAL_ERROR);
		identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (self->collection));
		return gkm_attribute_set_string (attr, identifier);

	case CKA_G_FIELDS:
		if (!self->fields)
			return gkm_attribute_set_data (attr, NULL, 0);
		return gkm_secret_fields_serialize (attr, self->fields);

	case CKA_G_SCHEMA:
		return gkm_attribute_set_string (attr, self->schema);
	}

	return GKM_OBJECT_CLASS (gkm_secret_item_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_secret_item_real_set_attribute (GkmObject *base, GkmSession *session,
                                    GkmTransaction *transaction, CK_ATTRIBUTE_PTR attr)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (base);
	const gchar *identifier;
	GkmSecretData *sdata;
	GHashTable *fields;
	GkmSecret *secret;
	gchar *schema;
	CK_RV rv;

	if (!self->collection) {
		gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
		g_return_if_reached ();
	}

	/* Check that the object is not locked */
	if (!gkm_secret_collection_unlocked_have (self->collection, session)) {
		gkm_transaction_fail (transaction, CKR_USER_NOT_LOGGED_IN);
		return;
	}

	switch (attr->type) {
	case CKA_VALUE:
		sdata = gkm_secret_collection_unlocked_use (self->collection, session);
		g_return_if_fail (sdata);
		identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (self));
		secret = gkm_secret_new (attr->pValue, attr->ulValueLen);
		gkm_secret_data_set_transacted (sdata, transaction, identifier, secret);
		g_object_unref (secret);
		g_object_unref (sdata);
		if (!gkm_transaction_get_failed (transaction))
			gkm_transaction_add (transaction, self, complete_set_secret, NULL);
		return;

	case CKA_G_FIELDS:
		rv = gkm_secret_fields_parse (attr, &fields);
		if (rv != CKR_OK)
			gkm_transaction_fail (transaction, rv);
		else
			begin_set_fields (self, transaction, fields);
		return;

	case CKA_G_SCHEMA:
		rv = gkm_attribute_get_string (attr, &schema);
		if (rv != CKR_OK)
			gkm_transaction_fail (transaction, rv);
		else
			begin_set_schema (self, transaction, schema);
		return;
	}

	GKM_OBJECT_CLASS (gkm_secret_item_parent_class)->set_attribute (base, session, transaction, attr);
}

static void
gkm_secret_item_init (GkmSecretItem *self)
{

}

static GObject*
gkm_secret_item_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (G_OBJECT_CLASS (gkm_secret_item_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	g_return_val_if_fail (self->collection, NULL);

	return G_OBJECT (self);
}

static void
gkm_secret_item_set_property (GObject *obj, guint prop_id, const GValue *value,
                              GParamSpec *pspec)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (obj);

	switch (prop_id) {
	case PROP_COLLECTION:
		g_return_if_fail (!self->collection);
		self->collection = g_value_get_object (value);
		g_return_if_fail (self->collection);
		g_object_add_weak_pointer (G_OBJECT (self->collection),
		                           (gpointer*)&(self->collection));
		break;
	case PROP_FIELDS:
		gkm_secret_item_set_fields (self, g_value_get_boxed (value));
		break;
	case PROP_SCHEMA:
		gkm_secret_item_set_schema (self, g_value_get_string (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_secret_item_get_property (GObject *obj, guint prop_id, GValue *value,
                              GParamSpec *pspec)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (obj);

	switch (prop_id) {
	case PROP_COLLECTION:
		g_value_set_object (value, gkm_secret_item_get_collection (self));
		break;
	case PROP_FIELDS:
		g_value_set_boxed (value, gkm_secret_item_get_fields (self));
		break;
	case PROP_SCHEMA:
		g_value_set_string (value, gkm_secret_item_get_schema (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_secret_item_dispose (GObject *obj)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (obj);

	if (self->collection)
		g_object_remove_weak_pointer (G_OBJECT (self->collection),
		                              (gpointer*)&(self->collection));
	self->collection = NULL;

	G_OBJECT_CLASS (gkm_secret_item_parent_class)->dispose (obj);
}

static void
gkm_secret_item_finalize (GObject *obj)
{
	GkmSecretItem *self = GKM_SECRET_ITEM (obj);

	g_assert (!self->collection);

	if (self->fields)
		g_hash_table_unref (self->fields);
	self->fields = NULL;

	G_OBJECT_CLASS (gkm_secret_item_parent_class)->finalize (obj);
}

static void
gkm_secret_item_class_init (GkmSecretItemClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	GkmSecretObjectClass *secret_class = GKM_SECRET_OBJECT_CLASS (klass);

	gkm_secret_item_parent_class = g_type_class_peek_parent (klass);

	gobject_class->constructor = gkm_secret_item_constructor;
	gobject_class->dispose = gkm_secret_item_dispose;
	gobject_class->finalize = gkm_secret_item_finalize;
	gobject_class->set_property = gkm_secret_item_set_property;
	gobject_class->get_property = gkm_secret_item_get_property;

	gkm_class->get_attribute = gkm_secret_item_real_get_attribute;
	gkm_class->set_attribute = gkm_secret_item_real_set_attribute;

	secret_class->is_locked = gkm_secret_item_real_is_locked;

	g_object_class_install_property (gobject_class, PROP_COLLECTION,
	           g_param_spec_object ("collection", "Collection", "Item's Collection",
	                                GKM_TYPE_SECRET_COLLECTION, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_FIELDS,
	           g_param_spec_boxed ("fields", "Fields", "Item's fields",
	                               GKM_BOXED_SECRET_FIELDS, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_SCHEMA,
	           g_param_spec_string ("schema", "Schema", "Item's type or schema",
	                                NULL, G_PARAM_READWRITE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmFactory*
gkm_secret_item_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_SECRET_KEY;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_item
	};

	return &factory;
}

GkmSecretCollection*
gkm_secret_item_get_collection (GkmSecretItem *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_ITEM (self), NULL);
	return self->collection;
}

GHashTable*
gkm_secret_item_get_fields (GkmSecretItem *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_ITEM (self), NULL);
	if (self->fields == NULL)
		self->fields = gkm_secret_fields_new ();
	return self->fields;
}

void
gkm_secret_item_set_fields (GkmSecretItem *self, GHashTable *fields)
{
	g_return_if_fail (GKM_IS_SECRET_ITEM (self));

	if (fields)
		g_hash_table_ref (fields);
	if (self->fields)
		g_hash_table_unref (self->fields);
	self->fields = fields;

	g_object_notify (G_OBJECT (self), "fields");
	gkm_object_notify_attribute (GKM_OBJECT (self), CKA_G_FIELDS);
}

const gchar*
gkm_secret_item_get_schema (GkmSecretItem *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_ITEM (self), NULL);
	return self->schema;
}

void
gkm_secret_item_set_schema (GkmSecretItem *self, const gchar *schema)
{
	g_return_if_fail (GKM_IS_SECRET_ITEM (self));

	if (schema != self->schema) {
		g_free (self->schema);
		self->schema = g_strdup (schema);
		g_object_notify (G_OBJECT (self), "schema");
		gkm_object_notify_attribute (GKM_OBJECT (self), CKA_G_SCHEMA);
	}
}
