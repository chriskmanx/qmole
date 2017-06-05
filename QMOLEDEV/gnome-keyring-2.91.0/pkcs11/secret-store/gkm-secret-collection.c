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

#include "gkm-secret-binary.h"
#include "gkm-secret-collection.h"
#include "gkm-secret-data.h"
#include "gkm-secret-item.h"
#include "gkm-secret-textual.h"

#include "egg/egg-error.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-credential.h"
#include "gkm/gkm-secret.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-transaction.h"

#include <glib/gi18n.h>

#include "pkcs11/pkcs11i.h"

enum {
	PROP_0,
	PROP_FILENAME
};

struct _GkmSecretCollection {
	GkmSecretObject parent;
	GkmSecretData *sdata;
	GHashTable *items;
	gchar *filename;
	guint32 watermark;
	GArray *template;
};

G_DEFINE_TYPE (GkmSecretCollection, gkm_secret_collection, GKM_TYPE_SECRET_OBJECT);

/* Forward declarations */
static void add_item (GkmSecretCollection *, GkmTransaction *, GkmSecretItem *);
static void remove_item (GkmSecretCollection *, GkmTransaction *, GkmSecretItem *);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GkmDataResult
load_collection_and_secret_data (GkmSecretCollection *self, GkmSecretData *sdata,
                                 const gchar *path)
{
	GkmDataResult res;
	GError *error = NULL;
	guchar *data;
	gsize n_data;

	/* Read in the keyring */
	if (!g_file_get_contents (path, (gchar**)&data, &n_data, &error)) {
		g_message ("problem reading keyring: %s: %s",
		           path, egg_error_message (error));
		g_clear_error (&error);
		return GKM_DATA_FAILURE;
	}

	/* Try to load from an encrypted file, and otherwise plain text */
	res = gkm_secret_binary_read (self, sdata, data, n_data);
	if (res == GKM_DATA_UNRECOGNIZED)
		res = gkm_secret_textual_read (self, sdata, data, n_data);

	g_free (data);

	return res;
}

static GkmCredential*
lookup_unassociated_credential (GkmSession *session, CK_OBJECT_HANDLE handle)
{
	GkmObject *object;

	if (gkm_session_lookup_readable_object (session, handle, &object) != CKR_OK)
		return NULL;

	if (gkm_credential_get_object (GKM_CREDENTIAL (object)) != NULL)
		return NULL;

	return GKM_CREDENTIAL (object);
}

static gboolean
find_unlocked_credential (GkmCredential *cred, GkmObject *object, gpointer user_data)
{
	CK_OBJECT_HANDLE *result = user_data;

	g_return_val_if_fail (!*result, FALSE);

	if (gkm_credential_peek_data (cred, GKM_TYPE_SECRET_DATA)) {
		*result = gkm_object_get_handle (GKM_OBJECT (cred));
		return TRUE;
	}

	return FALSE;
}

static gboolean
find_unlocked_secret_data (GkmCredential *cred, GkmObject *object, gpointer user_data)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (object);
	GkmSecretData **result = user_data;

	g_return_val_if_fail (!*result, FALSE);

	*result = gkm_credential_pop_data (cred, GKM_TYPE_SECRET_DATA);
	if (*result) {
		g_return_val_if_fail (*result == self->sdata, FALSE);
		return TRUE;
	}

	return FALSE;
}

static void
track_secret_data (GkmSecretCollection *self, GkmSecretData *data)
{
	g_return_if_fail (GKM_IS_SECRET_COLLECTION (self));

	if (self->sdata)
		g_object_remove_weak_pointer (G_OBJECT (self->sdata),
		                              (gpointer*)&(self->sdata));
	self->sdata = data;
	if (self->sdata)
		g_object_add_weak_pointer (G_OBJECT (self->sdata),
		                           (gpointer*)&(self->sdata));
}

static void
each_value_to_list (gpointer key, gpointer value, gpointer user_data)
{
	GList **list = user_data;
	*list = g_list_prepend (*list, value);
}

static void
expose_each_item (gpointer key, gpointer value, gpointer user_data)
{
	gboolean expose = GPOINTER_TO_INT (user_data);
	gkm_object_expose (value, expose);
}

static gboolean
complete_add (GkmTransaction *transaction, GkmSecretCollection *self, GkmSecretItem *item)
{
	if (gkm_transaction_get_failed (transaction))
		remove_item (self, NULL, item);
	g_object_unref (item);
	return TRUE;
}

static void
add_item (GkmSecretCollection *self, GkmTransaction *transaction, GkmSecretItem *item)
{
	const gchar *identifier;
	guint32 number;

	g_assert (GKM_IS_SECRET_COLLECTION (self));
	g_assert (GKM_IS_SECRET_ITEM (item));

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (item));
	g_return_if_fail (identifier);

	/* Make note of the highest numeric identifier, for later use */
	number = strtoul (identifier, NULL, 10);
	if (number > self->watermark)
		self->watermark = number;

	g_hash_table_replace (self->items, g_strdup (identifier), g_object_ref (item));

	if (gkm_object_is_exposed (GKM_OBJECT (self)))
		gkm_object_expose_full (GKM_OBJECT (item), transaction, TRUE);
	if (transaction)
		gkm_transaction_add (transaction, self, (GkmTransactionFunc)complete_add,
		                     g_object_ref (item));

}

static gboolean
complete_remove (GkmTransaction *transaction, GkmSecretCollection *self, GkmSecretItem *item)
{
	if (gkm_transaction_get_failed (transaction))
		add_item (self, NULL, item);
	g_object_unref (item);
	return TRUE;
}

static void
remove_item (GkmSecretCollection *self, GkmTransaction *transaction, GkmSecretItem *item)
{
	const gchar *identifier;

	g_assert (GKM_IS_SECRET_COLLECTION (self));
	g_assert (GKM_IS_SECRET_ITEM (item));

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (item));
	g_return_if_fail (identifier);

	g_object_ref (item);

	g_hash_table_remove (self->items, identifier);

	gkm_object_expose_full (GKM_OBJECT (item), transaction, FALSE);
	if (transaction)
		gkm_transaction_add (transaction, self, (GkmTransactionFunc)complete_remove,
		                     g_object_ref (item));

	g_object_unref (item);
}

static GkmObject*
factory_create_collection (GkmSession *session, GkmTransaction *transaction,
                           CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmSecretCollection *collection = NULL;
	CK_OBJECT_HANDLE handle;
	CK_ATTRIBUTE *attr;
	GkmManager *manager;
	GkmModule *module;
	gchar *identifier = NULL;
	GkmSecretData *sdata;
	gchar *label = NULL;
	GkmCredential *cred;
	gboolean is_token;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_TRANSACTION (transaction), NULL);
	g_return_val_if_fail (attrs || !n_attrs, NULL);

	manager = gkm_manager_for_template (attrs, n_attrs, session);
	module = gkm_session_get_module (session);

	/* Must have a credential, which is not associated with an object yet */
	if (!gkm_attributes_find_ulong (attrs, n_attrs, CKA_G_CREDENTIAL, &handle)) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	cred = lookup_unassociated_credential (session, handle);
	if (cred == NULL) {
		gkm_transaction_fail (transaction, CKR_ATTRIBUTE_VALUE_INVALID);
		return NULL;
	}

	/* The identifier */
	attr = gkm_attributes_find (attrs, n_attrs, CKA_ID);
	if (attr != NULL) {
		gkm_attribute_consume (attr);
		rv = gkm_attribute_get_string (attr, &identifier);
		if (rv != CKR_OK) {
			gkm_transaction_fail (transaction, rv);
			return NULL;
		}

		/* Try to find the collection with that identifier */
		if (!gkm_attributes_find_boolean (attrs, n_attrs, CKA_TOKEN, &is_token))
			collection = gkm_secret_collection_find (session, attr,
			                                         gkm_module_get_manager (module),
			                                         gkm_session_get_manager (session), NULL);
		else if (is_token)
			collection = gkm_secret_collection_find (session, attr,
			                                         gkm_module_get_manager (module), NULL);
		else
			collection = gkm_secret_collection_find (session, attr,
			                                         gkm_session_get_manager (session), NULL);

		/* Already have one with this identifier? Just return that */
		if (collection != NULL) {
			gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (collection),
			                                      FALSE, attrs, n_attrs);
			return g_object_ref (collection);
		}
	}

	/* The label  */
	attr = gkm_attributes_find (attrs, n_attrs, CKA_LABEL);
	if (attr != NULL) {
		gkm_attribute_consume (attr);
		rv = gkm_attribute_get_string (attr, &label);
		if (rv != CKR_OK) {
			gkm_transaction_fail (transaction, rv);
			return NULL;
		}

		/* No identifier? Try to use label */
		if (identifier == NULL)
			identifier = g_utf8_strdown (label, -1);
	}

	if (!identifier || !identifier[0]) {
		g_free (identifier);
		identifier = g_strdup ("unnamed");
	}

	if (!label || !label[0]) {
		g_free (label);
		if (identifier) {
			label = g_strdup (identifier);
		} else {
			/* TRANSLATORS: This is the label for an keyring created without a label */
			label = g_strdup (_("Unnamed"));
		}
	}

	g_strdelimit (identifier, ":/\\<>|\t\n\r\v ", '_');
	collection = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                           "module", gkm_session_get_module (session),
	                           "identifier", identifier,
	                           "manager", manager,
	                           "label", label,
	                           NULL);

	g_free (identifier);
	g_free (label);

	gkm_credential_connect (cred, GKM_OBJECT (collection));
	sdata = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	gkm_credential_set_data (cred, GKM_TYPE_SECRET_DATA, sdata);
	gkm_secret_data_set_master (sdata, gkm_credential_get_secret (cred));
	track_secret_data (collection, sdata);
	g_object_unref (sdata);

	gkm_attributes_consume (attrs, n_attrs, CKA_G_CREDENTIAL, G_MAXULONG);
	gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (collection),
	                                      TRUE, attrs, n_attrs);
	return GKM_OBJECT (collection);
}

static gboolean
complete_master_password (GkmTransaction *transaction, GObject *object, gpointer user_data)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (object);
	GkmSecret *previous = user_data;

	if (gkm_transaction_get_failed (transaction)) {
		if (self->sdata)
			gkm_secret_data_set_master (self->sdata, previous);
	}

	if (previous)
		g_object_unref (previous);

	return TRUE;
}

static void
change_master_password (GkmSecretCollection *self, GkmTransaction *transaction,
                        GkmCredential *cred)
{
	GkmSecret *previous;

	g_assert (GKM_IS_SECRET_COLLECTION (self));
	g_assert (GKM_IS_TRANSACTION (transaction));
	g_assert (GKM_IS_CREDENTIAL (cred));

	if (!self->sdata) {
		gkm_transaction_fail (transaction, CKR_USER_NOT_LOGGED_IN);
		return;
	}

	previous = gkm_secret_data_get_master (self->sdata);
	if (previous != NULL)
		g_object_ref (previous);

	gkm_credential_connect (cred, GKM_OBJECT (self));
	gkm_credential_set_data (cred, GKM_TYPE_SECRET_DATA, self->sdata);
	gkm_secret_data_set_master (self->sdata, gkm_credential_get_secret (cred));

	gkm_transaction_add (transaction, self, complete_master_password, previous);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_secret_collection_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (base);
	const gchar *identifier;
	GkmSecret *master;

	switch (attr->type) {
	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_G_COLLECTION);
	case CKA_G_CREDENTIAL_TEMPLATE:
		return gkm_attribute_set_template (attr, self->template);
	case CKA_G_LOGIN_COLLECTION:
		identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (base));
		g_return_val_if_fail (identifier, CKR_GENERAL_ERROR);
		return gkm_attribute_set_bool (attr, g_str_equal (identifier, "login"));
	case CKA_TRUSTED:
		if (!self->sdata)
			return gkm_attribute_set_bool (attr, CK_FALSE);
		master = gkm_secret_data_get_master (self->sdata);
		return gkm_attribute_set_bool (attr, (master && !gkm_secret_is_trivially_weak (master)));
	}
	return GKM_OBJECT_CLASS (gkm_secret_collection_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_secret_collection_set_attribute (GkmObject *object, GkmSession *session,
                                     GkmTransaction *transaction, CK_ATTRIBUTE *attr)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (object);
	CK_OBJECT_HANDLE handle = 0;
	GkmCredential *cred;
	GArray *template;
	CK_RV rv;

	switch (attr->type) {
	case CKA_G_CREDENTIAL:
		gkm_credential_for_each (session, GKM_OBJECT (self),
		                         find_unlocked_credential, &handle);
		if (handle == 0) {
			gkm_transaction_fail (transaction, CKR_USER_NOT_LOGGED_IN);
			return;
		}
		rv = gkm_attribute_get_ulong (attr, &handle);
		if (rv != CKR_OK) {
			gkm_transaction_fail (transaction, rv);
			return;
		}
		cred = lookup_unassociated_credential (session, handle);
		if (cred == NULL) {
			gkm_transaction_fail (transaction, CKR_ATTRIBUTE_VALUE_INVALID);
			return;
		}
		change_master_password (self, transaction, cred);
		return;
	case CKA_G_CREDENTIAL_TEMPLATE:
		rv = gkm_attribute_get_template (attr, &template);
		if (rv != CKR_OK) {
			gkm_transaction_fail (transaction, rv);
			return;
		}
		gkm_template_free (self->template);
		self->template = template;
		return;
	};

	GKM_OBJECT_CLASS (gkm_secret_collection_parent_class)->set_attribute (object, session, transaction, attr);
}

static CK_RV
gkm_secret_collection_real_unlock (GkmObject *obj, GkmCredential *cred)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (obj);
	GkmDataResult res;
	GkmSecretData *sdata;
	GkmSecret *master;
	CK_RV rv;

	master = gkm_credential_get_secret (cred);

	/* Already unlocked, make sure pin matches */
	if (self->sdata) {
		if (!gkm_secret_equal (gkm_secret_data_get_master (self->sdata), master))
			return CKR_PIN_INCORRECT;

		/* Credential now tracks our secret data */
		gkm_credential_set_data (cred, GKM_TYPE_SECRET_DATA, self->sdata);
		return CKR_OK;
	}

	/* New secret data object, setup master password */
	sdata = g_object_new (GKM_TYPE_SECRET_DATA, NULL);
	gkm_secret_data_set_master (sdata, master);

	/* Load the data from a file, and decrypt if necessary */
	if (self->filename) {
		res = load_collection_and_secret_data (self, sdata, self->filename);

	/* No filename, password must be null */
	} else {
		if (gkm_secret_equals (master, NULL, 0))
			res = GKM_DATA_SUCCESS;
		else
			res = GKM_DATA_LOCKED;
	}

	switch (res) {
	case GKM_DATA_SUCCESS:
		gkm_credential_set_data (cred, GKM_TYPE_SECRET_DATA, sdata);
		track_secret_data (self, sdata);
		rv = CKR_OK;
		break;
	case GKM_DATA_LOCKED:
		rv = CKR_PIN_INCORRECT;
		break;
	case GKM_DATA_UNRECOGNIZED:
		g_message ("unrecognized or invalid keyring: %s", self->filename);
		rv = CKR_FUNCTION_FAILED;
		break;
	case GKM_DATA_FAILURE:
		g_message ("failed to read or parse keyring: %s", self->filename);
		rv = CKR_GENERAL_ERROR;
		break;
	default:
		g_assert_not_reached ();
	}

	g_object_unref (sdata);
	return rv;
}

static void
gkm_secret_collection_expose (GkmObject *base, gboolean expose)
{
	GKM_OBJECT_CLASS (gkm_secret_collection_parent_class)->expose_object (base, expose);
	g_hash_table_foreach (GKM_SECRET_COLLECTION (base)->items, expose_each_item, GINT_TO_POINTER (expose));
}

static gboolean
gkm_secret_collection_real_is_locked (GkmSecretObject *obj, GkmSession *session)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (obj);
	return !gkm_secret_collection_unlocked_have (self, session);
}

static void
gkm_secret_collection_init (GkmSecretCollection *self)
{
	CK_ULONG idle = 0;
	CK_ULONG after = 0;
	CK_BBOOL token = CK_TRUE;
	CK_ATTRIBUTE attrs[] = {
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_GNOME_TRANSIENT, &token, sizeof (token) },
		{ CKA_G_DESTRUCT_IDLE, &idle, sizeof (idle) },
		{ CKA_G_DESTRUCT_AFTER, &after, sizeof (after) },
	};

	self->items = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_object_unref);
	self->template = gkm_template_new (attrs, G_N_ELEMENTS (attrs));
}

static void
gkm_secret_collection_set_property (GObject *obj, guint prop_id, const GValue *value,
                                    GParamSpec *pspec)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (obj);

	switch (prop_id) {
	case PROP_FILENAME:
		gkm_secret_collection_set_filename (self, g_value_get_string (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_secret_collection_get_property (GObject *obj, guint prop_id, GValue *value,
                                    GParamSpec *pspec)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (obj);

	switch (prop_id) {
	case PROP_FILENAME:
		g_value_set_string (value, gkm_secret_collection_get_filename (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_secret_collection_dispose (GObject *obj)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (obj);

	track_secret_data (self, NULL);
	g_hash_table_remove_all (self->items);

	G_OBJECT_CLASS (gkm_secret_collection_parent_class)->dispose (obj);
}

static void
gkm_secret_collection_finalize (GObject *obj)
{
	GkmSecretCollection *self = GKM_SECRET_COLLECTION (obj);

	g_assert (self->sdata == NULL);

	g_hash_table_destroy (self->items);
	self->items = NULL;

	g_free (self->filename);
	self->filename = NULL;

	gkm_template_free (self->template);
	self->template = NULL;

	G_OBJECT_CLASS (gkm_secret_collection_parent_class)->finalize (obj);
}

static void
gkm_secret_collection_class_init (GkmSecretCollectionClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	GkmSecretObjectClass *secret_class = GKM_SECRET_OBJECT_CLASS (klass);

	gkm_secret_collection_parent_class = g_type_class_peek_parent (klass);

	gobject_class->set_property = gkm_secret_collection_set_property;
	gobject_class->get_property = gkm_secret_collection_get_property;
	gobject_class->dispose = gkm_secret_collection_dispose;
	gobject_class->finalize = gkm_secret_collection_finalize;

	gkm_class->get_attribute = gkm_secret_collection_get_attribute;
	gkm_class->set_attribute = gkm_secret_collection_set_attribute;
	gkm_class->unlock = gkm_secret_collection_real_unlock;
	gkm_class->expose_object = gkm_secret_collection_expose;

	secret_class->is_locked = gkm_secret_collection_real_is_locked;

	g_object_class_install_property (gobject_class, PROP_FILENAME,
	           g_param_spec_string ("filename", "Filename", "Collection filename (without path)",
	                                NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	gkm_secret_object_class_unique_identifiers (secret_class);
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmFactory*
gkm_secret_collection_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_G_COLLECTION;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_collection
	};

	return &factory;
}

const gchar*
gkm_secret_collection_get_filename (GkmSecretCollection *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), NULL);
	return self->filename;
}

void
gkm_secret_collection_set_filename (GkmSecretCollection *self, const gchar *filename)
{
	g_return_if_fail (GKM_IS_SECRET_COLLECTION (self));

	if (self->filename == filename)
		return;
	g_free (self->filename);
	self->filename = g_strdup (filename);
	g_object_notify (G_OBJECT (self), "filename");
}

GList*
gkm_secret_collection_get_items (GkmSecretCollection *self)
{
	GList *items = NULL;
	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), NULL);
	g_hash_table_foreach (self->items, each_value_to_list, &items);
	return items;
}

GkmSecretItem*
gkm_secret_collection_get_item (GkmSecretCollection *self, const gchar *identifier)
{
	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), NULL);
	g_return_val_if_fail (identifier, NULL);
	return g_hash_table_lookup (self->items, identifier);
}

gboolean
gkm_secret_collection_has_item (GkmSecretCollection *self, GkmSecretItem *item)
{
	const gchar *identifier;

	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), FALSE);
	g_return_val_if_fail (GKM_IS_SECRET_ITEM (item), FALSE);

	identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (item));
	return g_hash_table_lookup (self->items, identifier) == item;
}

GkmSecretCollection*
gkm_secret_collection_find (GkmSession *session, CK_ATTRIBUTE_PTR attr, ...)
{
	CK_OBJECT_CLASS klass = CKO_G_COLLECTION;
	GkmSecretCollection *result = NULL;
	GkmManager *manager;
	CK_ATTRIBUTE attrs[2];
	GList *objects;
	va_list va;

	g_assert (attr);

	attrs[0].type = CKA_CLASS;
	attrs[0].ulValueLen = sizeof (klass);
	attrs[0].pValue = &klass;
	attrs[1].type = CKA_ID;
	attrs[1].ulValueLen = attr->ulValueLen;
	attrs[1].pValue = attr->pValue;

	va_start (va, attr);
	while (!result && (manager = va_arg (va, GkmManager*)) != NULL) {
		objects = gkm_manager_find_by_attributes (manager, session, attrs, 2);
		if (objects && GKM_IS_SECRET_COLLECTION (objects->data))
			result = objects->data;
		g_list_free (objects);
	}
	va_end (va);

	return result;
}

GkmSecretItem*
gkm_secret_collection_new_item (GkmSecretCollection *self, const gchar *identifier)
{
	GkmSecretItem *item;

	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), NULL);
	g_return_val_if_fail (identifier, NULL);
	g_return_val_if_fail (!g_hash_table_lookup (self->items, identifier), NULL);

	item = g_object_new (GKM_TYPE_SECRET_ITEM,
	                     "module", gkm_object_get_module (GKM_OBJECT (self)),
	                     "manager", gkm_object_get_manager (GKM_OBJECT (self)),
	                     "collection", self,
	                     "identifier", identifier,
	                     NULL);

	add_item (self, NULL, item);
	g_object_unref (item);
	return item;
}

GkmSecretItem*
gkm_secret_collection_create_item (GkmSecretCollection *self, GkmTransaction *transaction)
{
	GkmSecretItem *item;
	gchar *identifier = NULL;

	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), NULL);
	g_return_val_if_fail (transaction, NULL);
	g_return_val_if_fail (!gkm_transaction_get_failed (transaction), NULL);

	do {
		g_free (identifier);
		identifier = g_strdup_printf ("%d", ++(self->watermark));
	} while (g_hash_table_lookup (self->items, identifier));

	item = g_object_new (GKM_TYPE_SECRET_ITEM,
	                     "module", gkm_object_get_module (GKM_OBJECT (self)),
	                     "manager", gkm_object_get_manager (GKM_OBJECT (self)),
	                     "collection", self,
	                     "identifier", identifier,
	                     NULL);

	g_free (identifier);
	add_item (self, transaction, item);
	g_object_unref (item);
	return item;
}

void
gkm_secret_collection_remove_item (GkmSecretCollection *self, GkmSecretItem *item)
{
	g_return_if_fail (GKM_IS_SECRET_COLLECTION (self));
	g_return_if_fail (GKM_IS_SECRET_ITEM (item));
	g_return_if_fail (gkm_secret_collection_has_item (self, item));

	remove_item (self, NULL, item);
}

void
gkm_secret_collection_destroy_item (GkmSecretCollection *self, GkmTransaction *transaction,
                                    GkmSecretItem *item)
{
	g_return_if_fail (GKM_IS_SECRET_COLLECTION (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (GKM_IS_SECRET_ITEM (item));
	g_return_if_fail (gkm_secret_collection_has_item (self, item));

	remove_item (self, transaction, item);
}

gboolean
gkm_secret_collection_unlocked_have (GkmSecretCollection *self, GkmSession *session)
{
	CK_OBJECT_HANDLE handle = 0;

	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), FALSE);
	g_return_val_if_fail (GKM_IS_SESSION (session), FALSE);

	/*
	 * Look for credential objects that this session has access
	 * to, and use those to find the secret data. If a secret data is
	 * found, it should match the one we are tracking in self->sdata.
	 */

	gkm_credential_for_each (session, GKM_OBJECT (self), find_unlocked_credential, &handle);
	return handle != 0;
}

GkmSecretData*
gkm_secret_collection_unlocked_use (GkmSecretCollection *self, GkmSession *session)
{
	GkmSecretData *sdata = NULL;

	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), NULL);
	g_return_val_if_fail (GKM_IS_SESSION (session), NULL);

	/*
	 * Look for credential objects that this session has access
	 * to, and use those to find the secret data. If a secret data is
	 * found, it should match the one we are tracking in self->sdata.
	 */

	gkm_credential_for_each (session, GKM_OBJECT (self),
	                         find_unlocked_secret_data, &sdata);

	return sdata;
}

void
gkm_secret_collection_unlocked_clear (GkmSecretCollection *self)
{
	/*
	 * TODO: This is a tough one to implement. I'm holding off and wondering
	 * if we don't need it, perhaps? As it currently stands, what needs to happen
	 * here is we need to find each and every credential that references the
	 * secret data for this collection and completely delete those objects.
	 */
	g_warning ("Clearing of secret data needs implementing");
	track_secret_data (self, NULL);
}

GkmDataResult
gkm_secret_collection_load (GkmSecretCollection *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), GKM_DATA_FAILURE);

	if (!self->filename)
		return GKM_DATA_SUCCESS;

	return load_collection_and_secret_data (self, self->sdata, self->filename);
}

void
gkm_secret_collection_save (GkmSecretCollection *self, GkmTransaction *transaction)
{
	GkmSecret *master;
	GkmDataResult res;
	guchar *data;
	gsize n_data;

	g_return_if_fail (GKM_IS_SECRET_COLLECTION (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (!gkm_transaction_get_failed (transaction));

	/* HACK: We can't save unless the secret data was loaded */
	if (!self->sdata) {
		gkm_transaction_fail (transaction, CKR_USER_NOT_LOGGED_IN);
		return;
	}

	/* Don't save ourselves if no filename */
	if (!self->filename)
		return;

	master = gkm_secret_data_get_master (self->sdata);
	if (master == NULL || gkm_secret_equals (master, NULL, 0))
		res = gkm_secret_textual_write (self, self->sdata, &data, &n_data);
	else
		res = gkm_secret_binary_write (self, self->sdata, &data, &n_data);

	switch (res) {
	case GKM_DATA_FAILURE:
	case GKM_DATA_UNRECOGNIZED:
		g_warning ("couldn't prepare to write out keyring: %s", self->filename);
		gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
		break;
	case GKM_DATA_LOCKED:
		g_warning ("locked error while writing out keyring: %s", self->filename);
		gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
		break;
	case GKM_DATA_SUCCESS:
		gkm_transaction_write_file (transaction, self->filename, data, n_data);
		g_free (data);
		break;
	default:
		g_assert_not_reached ();
	};
}

void
gkm_secret_collection_destroy (GkmSecretCollection *self, GkmTransaction *transaction)
{
	g_return_if_fail (GKM_IS_SECRET_COLLECTION (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (!gkm_transaction_get_failed (transaction));

	gkm_object_expose_full (GKM_OBJECT (self), transaction, FALSE);
	if (self->filename)
		gkm_transaction_remove_file (transaction, self->filename);
}

gint
gkm_secret_collection_get_lock_idle (GkmSecretCollection *self)
{
	gulong value;
	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), 0);
	if (!gkm_template_find_ulong (self->template, CKA_G_DESTRUCT_IDLE, &value))
		value = 0;
	return (gint)value;
}

void
gkm_secret_collection_set_lock_idle (GkmSecretCollection *self, gint lock_timeout)
{
	CK_ULONG value = (lock_timeout < 0) ? 0 : lock_timeout;
	CK_ATTRIBUTE attr = { CKA_G_DESTRUCT_IDLE, &value, sizeof (value) };
	g_return_if_fail (GKM_IS_SECRET_COLLECTION (self));
	gkm_template_set (self->template, &attr);
}

gint
gkm_secret_collection_get_lock_after (GkmSecretCollection *self)
{
	gulong value;
	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (self), 0);
	if (!gkm_template_find_ulong (self->template, CKA_G_DESTRUCT_AFTER, &value))
		value = 0;
	return (gint)value;
}

void
gkm_secret_collection_set_lock_after (GkmSecretCollection *self, gint lock_timeout)
{
	CK_ULONG value = (lock_timeout < 0) ? 0 : lock_timeout;
	CK_ATTRIBUTE attr = { CKA_G_DESTRUCT_AFTER, &value, sizeof (value) };
	g_return_if_fail (GKM_IS_SECRET_COLLECTION (self));
	gkm_template_set (self->template, &attr);
}
