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

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11i.h"

#include "gkm-attributes.h"
#include "gkm-credential.h"
#include "gkm-crypto.h"
#include "gkm-factory.h"
#include "gkm-manager.h"
#include "gkm-memory-store.h"
#include "gkm-session.h"
#include "gkm-sexp.h"
#include "gkm-sexp-key.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

enum {
	PROP_0,
	PROP_MODULE,
	PROP_SLOT_ID,
	PROP_APARTMENT,
	PROP_HANDLE,
	PROP_READ_ONLY,
	PROP_MANAGER,
	PROP_LOGGED_IN
};

struct _GkmSessionPrivate {

	CK_SESSION_HANDLE handle;
	CK_SLOT_ID slot_id;
	CK_ULONG apartment;

	GkmModule *module;
	GkmManager *manager;
	GkmStore *store;

	CK_USER_TYPE logged_in;
	gboolean read_only;

	CK_NOTIFY notify_callback;
	CK_VOID_PTR application_ptr;

	/* Objects owned by this session */
	GHashTable *objects;

	/* Used for operations */
	void (*current_operation) (GkmSession *self);
	GkmObject *current_object;
	GkmCredential *credential;

	/* Used for find operations */
	GArray *found_objects;

	/* Used for crypto operations */
	gpointer crypto_state;
	GDestroyNotify crypto_destroy;
	CK_MECHANISM_TYPE crypto_mechanism;
	CK_ATTRIBUTE_TYPE crypto_method;
};

G_DEFINE_TYPE (GkmSession, gkm_session, G_TYPE_OBJECT);

static void add_object (GkmSession *self, GkmTransaction *transaction, GkmObject *object);
static void remove_object (GkmSession *self, GkmTransaction *transaction, GkmObject *object);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
cleanup_crypto (GkmSession *self)
{
	g_assert (self->pv->current_operation == cleanup_crypto);

	if (self->pv->crypto_state && self->pv->crypto_destroy)
		(self->pv->crypto_destroy) (self->pv->crypto_state);
	self->pv->crypto_state = NULL;
	self->pv->crypto_destroy = NULL;
	self->pv->crypto_mechanism = 0;
	self->pv->crypto_method = 0;

	g_assert (GKM_IS_OBJECT (self->pv->current_object));
	if (self->pv->current_object)
		g_object_unref (self->pv->current_object);
	self->pv->current_object = NULL;

	if (self->pv->credential) {
		g_object_set_data (G_OBJECT (self->pv->credential), "owned-by-session", NULL);
		g_object_unref (self->pv->credential);
		self->pv->credential = NULL;
	}

	self->pv->current_operation = NULL;
}

static CK_RV
prepare_crypto (GkmSession *self, CK_MECHANISM_PTR mech,
                CK_ATTRIBUTE_TYPE method, CK_OBJECT_HANDLE handle)
{
	GkmObject *object;
	CK_MECHANISM_TYPE_PTR mechanisms;
	CK_ULONG n_mechanisms, i;
	gsize n_data;
	gboolean have;
	gulong key_type;
	CK_RV rv;

	g_assert (GKM_IS_SESSION (self));

	/* Cancel any current operation */
	if (self->pv->current_operation) {
		(self->pv->current_operation) (self);
		g_assert (!self->pv->current_operation);
	}

	g_assert (!self->pv->crypto_state);

	/* First find the object */
	rv = gkm_session_lookup_readable_object (self, handle, &object);
	if (rv != CKR_OK)
		return rv;

	/* Make sure it's a key */
	if (!gkm_object_get_attribute_ulong (object, self, CKA_KEY_TYPE, &key_type))
		return CKR_KEY_HANDLE_INVALID;

	/* Lookup the mechanisms this object can do */
	mechanisms = gkm_object_get_attribute_data (object, self, CKA_ALLOWED_MECHANISMS, &n_data);
	if (mechanisms)
		n_mechanisms = n_data / sizeof (CK_MECHANISM_TYPE);
	else
		n_mechanisms = 0;

	/* See if ours is represented */
	have = FALSE;
	for (i = 0; !have && i < n_mechanisms; ++i) {
		if (mechanisms[i] == mech->mechanism)
			have = TRUE;
	}

	g_free (mechanisms);

	if (have == FALSE)
		return CKR_KEY_TYPE_INCONSISTENT;

	/* Check that the object can do this method */
	if (!gkm_object_get_attribute_boolean (object, self, method, &have) || !have)
		return CKR_KEY_FUNCTION_NOT_PERMITTED;

	/* Track the cyrpto object */
	self->pv->current_object = object;
	g_object_ref (object);

	/* And note what we're setup for */
	self->pv->current_operation = cleanup_crypto;
	self->pv->crypto_mechanism = mech->mechanism;
	self->pv->crypto_method = method;

	return CKR_OK;
}

static CK_RV
process_crypto (GkmSession *self, CK_ATTRIBUTE_TYPE method, CK_BYTE_PTR bufone,
                CK_ULONG n_bufone, CK_BYTE_PTR buftwo, CK_ULONG_PTR n_buftwo)
{
	CK_RV rv = CKR_OK;

	g_assert (GKM_IS_SESSION (self));

	if (self->pv->current_operation != cleanup_crypto)
		return CKR_OPERATION_NOT_INITIALIZED;
	if (method != self->pv->crypto_method)
		return CKR_OPERATION_NOT_INITIALIZED;

	if (!bufone || !n_buftwo)
		rv = CKR_ARGUMENTS_BAD;

	if (rv == CKR_OK) {
		/* Load up the actual sexp we're going to use */
		if (!self->pv->crypto_state) {
			g_return_val_if_fail (GKM_IS_OBJECT (self->pv->current_object), CKR_GENERAL_ERROR);
			rv = gkm_crypto_prepare (self, self->pv->crypto_mechanism, self->pv->current_object);
		}
	}

	if (rv == CKR_OK) {
		g_assert (self->pv->crypto_mechanism);
		rv = gkm_crypto_perform (self, self->pv->crypto_mechanism, method,
		                         bufone, n_bufone, buftwo, n_buftwo);
	}

	/* Under these conditions the operation isn't complete */
	if (rv == CKR_BUFFER_TOO_SMALL || rv == CKR_USER_NOT_LOGGED_IN ||
	    (rv == CKR_OK && buftwo == NULL))
		return rv;

	cleanup_crypto (self);
	return rv;
}

static void
cleanup_found (GkmSession *self)
{
	g_assert (GKM_IS_SESSION (self));

	g_assert (self->pv->found_objects);
	g_array_free (self->pv->found_objects, TRUE);
	self->pv->found_objects = NULL;

	self->pv->current_operation = NULL;
}

static CK_RV
lookup_object_from_handle (GkmSession *self, CK_OBJECT_HANDLE handle,
                           gboolean writable, GkmObject **result)
{
	GkmManager *manager;
	GkmObject *object;
	gboolean is_private;
	gboolean is_token;
	gboolean is_modifiable;

	g_return_val_if_fail (result, CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_GENERAL_ERROR);

	if (handle == 0)
		return CKR_OBJECT_HANDLE_INVALID;

	/* Try looking up in the token manager */
	manager = gkm_module_get_manager (self->pv->module);
	object = gkm_manager_find_by_handle (manager, handle);
	is_token = TRUE;

	/* Try looking up in the session manager */
	if (object == NULL) {
		manager = gkm_session_get_manager (self);
		object = gkm_manager_find_by_handle (manager, handle);
		is_token = FALSE;
	}

	if (object == NULL)
		return CKR_OBJECT_HANDLE_INVALID;

	g_return_val_if_fail (manager, CKR_GENERAL_ERROR);

	/*
	 * Check that we're not accessing private objects on a
	 * non-logged in session
	 */
	if (self->pv->logged_in != CKU_USER) {
		if (!gkm_object_get_attribute_boolean (object, self, CKA_PRIVATE, &is_private))
			is_private = FALSE;
		if (is_private)
			return CKR_USER_NOT_LOGGED_IN;
	}

	/*
	 * If we're going to write to this object check that we're in a
	 * writable session and object is modifiable.
	 */
	if (writable) {
		if (is_token) {
			if (!gkm_object_is_transient (object))
				if (gkm_module_get_write_protected (self->pv->module))
					return CKR_TOKEN_WRITE_PROTECTED;
			if (self->pv->read_only)
				return CKR_SESSION_READ_ONLY;
		}
		if (!gkm_object_get_attribute_boolean (object, self, CKA_MODIFIABLE, &is_modifiable))
			is_modifiable = FALSE;
		if (!is_modifiable) /* What's a better return code in this case? */
			return CKR_ATTRIBUTE_READ_ONLY;
	}

	*result = object;
	return CKR_OK;
}


static gboolean
complete_remove (GkmTransaction *transaction, GkmSession *self, GkmObject *object)
{
	if (gkm_transaction_get_failed (transaction))
		add_object (self, NULL, object);
	g_object_unref (object);
	return TRUE;
}

static void
remove_object (GkmSession *self, GkmTransaction *transaction, GkmObject *object)
{
	g_assert (GKM_IS_SESSION (self));
	g_assert (GKM_IS_OBJECT (object));

	g_object_ref (object);

	gkm_object_expose_full (object, transaction, FALSE);
	if (!g_hash_table_remove (self->pv->objects, object))
		g_return_if_reached ();
	g_object_set (object, "store", NULL, NULL);

	if (transaction)
		gkm_transaction_add (transaction, self, (GkmTransactionFunc)complete_remove,
		                     g_object_ref (object));

	g_object_unref (object);
}

static gboolean
complete_add (GkmTransaction *transaction, GkmSession *self, GkmObject *object)
{
	if (gkm_transaction_get_failed (transaction))
		remove_object (self, NULL, object);
	g_object_unref (object);
	return TRUE;
}

static void
add_object (GkmSession *self, GkmTransaction *transaction, GkmObject *object)
{
	g_assert (GKM_IS_SESSION (self));
	g_assert (GKM_IS_OBJECT (object));

	/* Must not already be associated with a session or manager */
	g_return_if_fail (gkm_object_get_manager (object) == self->pv->manager);
	g_return_if_fail (g_object_get_data (G_OBJECT (object), "owned-by-session") == NULL);
	g_return_if_fail (g_hash_table_lookup (self->pv->objects, object) == NULL);

	g_hash_table_insert (self->pv->objects, object, g_object_ref (object));
	g_object_set_data (G_OBJECT (object), "owned-by-session", self);
	g_object_set (object, "store", self->pv->store, NULL);
	gkm_object_expose_full (object, transaction, TRUE);

	if (transaction)
		gkm_transaction_add (transaction, self, (GkmTransactionFunc)complete_add,
		                     g_object_ref (object));
}

static gboolean
attributes_find_boolean (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs,
                         CK_ATTRIBUTE_TYPE type, CK_BBOOL *value)
{
	CK_ULONG i;

	g_assert (attrs || !n_attrs);
	g_assert (value);

	for (i = 0; i < n_attrs; ++i) {
		if (attrs[i].type == type &&
		    attrs[i].pValue != NULL &&
		    attrs[i].ulValueLen == sizeof (CK_BBOOL)) {
			*value = *((CK_BBOOL*)attrs[i].pValue);
			return TRUE;
		}
	}

	return FALSE;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */


static GObject*
gkm_session_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmSession *self = GKM_SESSION (G_OBJECT_CLASS (gkm_session_parent_class)->constructor(type, n_props, props));
	CK_ATTRIBUTE attr;

	g_return_val_if_fail (self, NULL);

	/* Register store attributes */
	attr.type = CKA_LABEL;
	attr.pValue = "";
	attr.ulValueLen = 0;
	gkm_store_register_schema (self->pv->store, &attr, NULL, 0);

	return G_OBJECT (self);
}

static void
gkm_session_init (GkmSession *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_SESSION, GkmSessionPrivate);
	self->pv->objects = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, gkm_util_dispose_unref);
	self->pv->read_only = TRUE;

	/* Create the store and register attributes */
	self->pv->store = GKM_STORE (gkm_memory_store_new ());
}

static void
gkm_session_dispose (GObject *obj)
{
	GkmSession *self = GKM_SESSION (obj);

	/* Cleanup any current operation */
	if (self->pv->current_operation)
		(self->pv->current_operation) (self);
	g_assert (!self->pv->current_operation);

	if (self->pv->module)
		g_object_unref (self->pv->module);
	self->pv->module = NULL;

	if (self->pv->credential) {
		g_object_set_data (G_OBJECT (self->pv->credential), "owned-by-session", NULL);
		g_object_unref (self->pv->credential);
		self->pv->credential = NULL;
	}

	g_hash_table_remove_all (self->pv->objects);

	if (self->pv->manager)
		g_object_unref (self->pv->manager);
	self->pv->manager = NULL;

	G_OBJECT_CLASS (gkm_session_parent_class)->dispose (obj);
}

static void
gkm_session_finalize (GObject *obj)
{
	GkmSession *self = GKM_SESSION (obj);

	g_assert (self->pv->module == NULL);
	g_assert (self->pv->manager == NULL);

	g_hash_table_destroy (self->pv->objects);
	self->pv->objects = NULL;

	g_object_unref (self->pv->store);
	self->pv->store = NULL;

	G_OBJECT_CLASS (gkm_session_parent_class)->finalize (obj);
}

static void
gkm_session_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	GkmSession *self = GKM_SESSION (obj);

	switch (prop_id) {
	case PROP_MODULE:
		g_return_if_fail (!self->pv->module);
		self->pv->module = g_value_get_object (value);
		g_return_if_fail (self->pv->module);
		g_object_ref (self->pv->module);
		break;
	case PROP_MANAGER:
		g_return_if_fail (!self->pv->manager);
		self->pv->manager = g_value_get_object (value);
		g_return_if_fail (self->pv->manager);
		g_object_ref (self->pv->manager);
		break;
	case PROP_SLOT_ID:
		self->pv->slot_id = g_value_get_ulong (value);
		break;
	case PROP_APARTMENT:
		self->pv->apartment = g_value_get_ulong (value);
		break;
	case PROP_HANDLE:
		self->pv->handle = g_value_get_ulong (value);
		g_return_if_fail (self->pv->handle != 0);
		break;
	case PROP_READ_ONLY:
		self->pv->read_only = g_value_get_boolean (value);
		break;
	case PROP_LOGGED_IN:
		gkm_session_set_logged_in (self, g_value_get_ulong (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_session_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	GkmSession *self = GKM_SESSION (obj);

	switch (prop_id) {
	case PROP_MODULE:
		g_value_set_object (value, gkm_session_get_module (self));
		break;
	case PROP_MANAGER:
		g_value_set_object (value, gkm_session_get_manager (self));
		break;
	case PROP_SLOT_ID:
		g_value_set_ulong (value, gkm_session_get_slot_id (self));
		break;
	case PROP_APARTMENT:
		g_value_set_ulong (value, gkm_session_get_apartment (self));
		break;
	case PROP_HANDLE:
		g_value_set_ulong (value, gkm_session_get_handle (self));
		break;
	case PROP_READ_ONLY:
		g_value_set_boolean (value, gkm_session_get_read_only (self));
		break;
	case PROP_LOGGED_IN:
		g_value_set_ulong (value, gkm_session_get_logged_in (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_session_class_init (GkmSessionClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gkm_session_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GkmSessionPrivate));

	gobject_class->constructor = gkm_session_constructor;
	gobject_class->dispose = gkm_session_dispose;
	gobject_class->finalize = gkm_session_finalize;
	gobject_class->set_property = gkm_session_set_property;
	gobject_class->get_property = gkm_session_get_property;

	g_object_class_install_property (gobject_class, PROP_MODULE,
	         g_param_spec_object ("module", "Module", "Module this session belongs to",
	                              GKM_TYPE_MODULE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_MANAGER,
	         g_param_spec_object ("manager", "Manager", "Object manager for this session",
	                              GKM_TYPE_MANAGER, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_HANDLE,
	         g_param_spec_ulong ("handle", "Handle", "PKCS#11 session handle",
	                             0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_SLOT_ID,
	         g_param_spec_ulong ("slot-id", "Slot ID", "Slot ID this session is opened on",
	                             0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_APARTMENT,
	         g_param_spec_ulong ("apartment", "Apartment", "Apartment this session is opened on",
	                             0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_READ_ONLY,
	         g_param_spec_boolean ("read-only", "Read Only", "Whether a read-only session or not",
	                               TRUE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_LOGGED_IN,
	         g_param_spec_ulong ("logged-in", "Logged in", "Whether this session is logged in or not",
	                             0, G_MAXULONG, CKU_NONE, G_PARAM_READWRITE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmSession*
gkm_session_for_session_object (GkmObject *obj)
{
	g_return_val_if_fail (GKM_IS_OBJECT (obj), NULL);
	return GKM_SESSION (g_object_get_data (G_OBJECT (obj), "owned-by-session"));
}

CK_SESSION_HANDLE
gkm_session_get_handle (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), 0);
	return self->pv->handle;
}

CK_SLOT_ID
gkm_session_get_slot_id (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), 0);
	return self->pv->slot_id;
}

CK_ULONG
gkm_session_get_apartment (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), 0);
	return self->pv->apartment;
}

GkmModule*
gkm_session_get_module (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), NULL);
	g_return_val_if_fail (GKM_IS_MODULE (self->pv->module), NULL);
	return self->pv->module;
}

GkmManager*
gkm_session_get_manager (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), NULL);
	g_return_val_if_fail (GKM_IS_MANAGER (self->pv->manager), NULL);
	return self->pv->manager;
}

gulong
gkm_session_get_logged_in (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), FALSE);
	return self->pv->logged_in;
}

void
gkm_session_set_logged_in (GkmSession *self, gulong logged_in)
{
	g_return_if_fail (GKM_IS_SESSION (self));
	self->pv->logged_in = logged_in;
	g_object_notify (G_OBJECT (self), "logged-in");
}

gpointer
gkm_session_get_crypto_state (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), NULL);
	return self->pv->crypto_state;
}

void
gkm_session_set_crypto_state (GkmSession *self, gpointer state,
                              GDestroyNotify destroy)
{
	g_return_if_fail (GKM_IS_SESSION (self));
	if (self->pv->crypto_state != state) {
		if (self->pv->crypto_state && self->pv->crypto_destroy)
			(self->pv->crypto_destroy) (self->pv->crypto_state);
	}
	self->pv->crypto_state = state;
	self->pv->crypto_destroy = destroy;
}

gboolean
gkm_session_get_read_only (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), TRUE);
	return self->pv->read_only;
}

CK_RV
gkm_session_lookup_readable_object (GkmSession *self, CK_OBJECT_HANDLE handle,
                                    GkmObject **result)
{
	return lookup_object_from_handle (self, handle, FALSE, result);
}

CK_RV
gkm_session_lookup_writable_object (GkmSession *self, CK_OBJECT_HANDLE handle,
                                    GkmObject **result)
{
	return lookup_object_from_handle (self, handle, TRUE, result);
}

CK_RV
gkm_session_login_context_specific (GkmSession *self, CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	GkmCredential *cred;
	gboolean always_auth;
	gboolean is_private;
	GkmObject *object;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_GENERAL_ERROR);

	if (!self->pv->current_object)
		return CKR_OPERATION_NOT_INITIALIZED;

	object = self->pv->current_object;
	g_return_val_if_fail (GKM_IS_OBJECT (object), CKR_GENERAL_ERROR);

	if (!gkm_object_get_attribute_boolean (object, self, CKA_ALWAYS_AUTHENTICATE, &always_auth))
		always_auth = FALSE;
	if (!gkm_object_get_attribute_boolean (object, self, CKA_PRIVATE, &is_private))
		is_private = FALSE;

	/* A strange code, but that's what the spec says */
	if (always_auth == FALSE)
		return CKR_OPERATION_NOT_INITIALIZED;

	/* Double check that the object has what it takes */
	g_return_val_if_fail (is_private == TRUE, CKR_GENERAL_ERROR);

	/* Now create the strange object */
	rv = gkm_credential_create (self->pv->module, self->pv->manager,
	                            self->pv->current_object, pin, n_pin, &cred);
	if (rv != CKR_OK)
		return rv;

	if (self->pv->credential)
		g_object_unref (self->pv->credential);
	g_object_set_data (G_OBJECT (cred), "owned-by-session", self);
	self->pv->credential = cred;

	return CKR_OK;
}

void
gkm_session_add_session_object (GkmSession *self, GkmTransaction *transaction,
                                GkmObject *obj)
{
	g_return_if_fail (GKM_IS_SESSION (self));
	g_return_if_fail (gkm_session_for_session_object (obj) == NULL);

	if (transaction) {
		g_return_if_fail (GKM_IS_TRANSACTION (transaction));
		g_return_if_fail (!gkm_transaction_get_failed (transaction));
	}

	add_object (self, transaction, obj);
}

void
gkm_session_destroy_session_object (GkmSession *self, GkmTransaction *transaction,
                                    GkmObject *obj)
{
	g_return_if_fail (GKM_IS_SESSION (self));
	g_return_if_fail (gkm_session_for_session_object (obj) == self);

	if (transaction) {
		g_return_if_fail (GKM_IS_TRANSACTION (transaction));
		g_return_if_fail (!gkm_transaction_get_failed (transaction));
	}

	/* Don't actually destroy the credential */
	if (self->pv->credential && GKM_OBJECT (self->pv->credential) == obj)
		return;

	remove_object (self, transaction, obj);
}

GkmCredential*
gkm_session_get_credential (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), NULL);
	return self->pv->credential;
}

GkmObject*
gkm_session_create_object_for_factory (GkmSession *self, GkmFactory *factory,
                                       GkmTransaction *transaction,
                                       CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	GkmTransaction *owned = NULL;
	GkmObject  *object;

	g_return_val_if_fail (GKM_IS_SESSION (self), NULL);
	g_return_val_if_fail (factory && factory->func, NULL);
	g_return_val_if_fail (template || !count, NULL);

	/* The transaction for this whole dealio */
	if (!transaction)
		owned = transaction = gkm_transaction_new ();

	g_return_val_if_fail (GKM_IS_TRANSACTION (transaction), NULL);

	/*
	 * Duplicate the memory for the attributes (but not values) so we
	 * can 'consume' in the factory function
	 */
	template = g_memdup (template, count * sizeof (CK_ATTRIBUTE));

	/* Actually create the object */
	object = (factory->func) (self, transaction, template, count);

	/* A NULL result without a failure code, bad */
	if (object == NULL && !gkm_transaction_get_failed (transaction)) {
		g_warn_if_reached ();
		gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
	}

	g_free (template);

	if (owned)
		gkm_transaction_complete (transaction);

	/* Object is owned by module or session */
	if (gkm_transaction_get_failed (transaction)) {
		if (object)
			g_object_unref (object);
		object = NULL;
	}

	if (owned)
		g_object_unref (owned);

	return object;
}

GkmObject*
gkm_session_create_object_for_attributes (GkmSession *self, GkmTransaction *transaction,
                                          CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmFactory *factory;

	g_return_val_if_fail (GKM_IS_SESSION (self), NULL);

	/* Find out if we can create such an object */
	factory = gkm_module_find_factory (gkm_session_get_module (self), attrs, n_attrs);
	if (factory == NULL) {
		if (transaction)
			gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	return gkm_session_create_object_for_factory (self, factory, transaction, attrs, n_attrs);
}

void
gkm_session_complete_object_creation (GkmSession *self, GkmTransaction *transaction, GkmObject *object,
                                      gboolean add, CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	gboolean is_private;
	gulong i;

	g_return_if_fail (GKM_IS_SESSION (self));
	g_return_if_fail (GKM_IS_OBJECT (object));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (!gkm_transaction_get_failed (transaction));

	gkm_object_create_attributes (object, self, transaction, attrs, n_attrs);
	if (gkm_transaction_get_failed (transaction))
		return;

	/* See if we can create due to read-only */
	if (gkm_object_is_token (object)) {
		if (!gkm_object_is_transient (object) &&
		    gkm_module_get_write_protected (self->pv->module)) {
			gkm_transaction_fail (transaction, CKR_TOKEN_WRITE_PROTECTED);
			return;
		}
		else if (self->pv->read_only) {
			gkm_transaction_fail (transaction, CKR_SESSION_READ_ONLY);
			return;
		}
	}

	/* Can only create public objects unless logged in */
	if (gkm_session_get_logged_in (self) != CKU_USER &&
	    gkm_object_get_attribute_boolean (object, self, CKA_PRIVATE, &is_private) &&
	    is_private == TRUE) {
		gkm_transaction_fail (transaction, CKR_USER_NOT_LOGGED_IN);
		return;
	}

	/* Add the object to session or token */
	if (add && !gkm_transaction_get_failed (transaction)) {
		if (gkm_object_is_token (object))
			gkm_module_add_token_object (self->pv->module, transaction, object);
		else
			add_object (self, transaction, object);
	}

	/* Next go through and set all attributes that weren't used initially */
	gkm_attributes_consume (attrs, n_attrs, CKA_TOKEN, G_MAXULONG);
	for (i = 0; i < n_attrs && !gkm_transaction_get_failed (transaction); ++i) {
		if (!gkm_attribute_consumed (&attrs[i]))
			gkm_object_set_attribute (object, self, transaction, &attrs[i]);
	}

	/* Store the object */
	if (!gkm_transaction_get_failed (transaction)) {
		if (gkm_object_is_token (object))
			gkm_module_store_token_object (self->pv->module, transaction, object);
	}
}

/* -----------------------------------------------------------------------------
 * PKCS#11
 */

CK_RV
gkm_session_C_GetFunctionStatus (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	return CKR_FUNCTION_NOT_PARALLEL;
}

CK_RV
gkm_session_C_CancelFunction (GkmSession *self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	return CKR_FUNCTION_NOT_PARALLEL;
}

CK_RV
gkm_session_C_GetSessionInfo(GkmSession* self, CK_SESSION_INFO_PTR info)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!info)
		return CKR_ARGUMENTS_BAD;

	info->slotID = self->pv->slot_id;
	if (self->pv->logged_in == CKU_USER)
		info->state = self->pv->read_only ? CKS_RO_USER_FUNCTIONS : CKS_RW_USER_FUNCTIONS;
	else if (self->pv->logged_in == CKU_SO)
		info->state = CKS_RW_SO_FUNCTIONS;
	else
		info->state = self->pv->read_only ? CKS_RO_PUBLIC_SESSION : CKS_RW_PUBLIC_SESSION;
	info->flags = CKF_SERIAL_SESSION;
	if (!self->pv->read_only)
		info->flags |= CKF_RW_SESSION;
	info->ulDeviceError = 0;

	return CKR_OK;
}

CK_RV
gkm_session_C_GetOperationState (GkmSession* self, CK_BYTE_PTR operation_state,
                                 CK_ULONG_PTR operation_state_len)
{
	/* Nope, We don't bend that way */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_SetOperationState (GkmSession* self, CK_BYTE_PTR operation_state,
                                 CK_ULONG operation_state_len, CK_OBJECT_HANDLE encryption_key,
                                 CK_OBJECT_HANDLE authentication_key)
{
	/* Nope. We don't bend that way */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_CreateObject (GkmSession* self, CK_ATTRIBUTE_PTR template,
                            CK_ULONG count, CK_OBJECT_HANDLE_PTR new_object)
{
	GkmObject *object = NULL;
	GkmTransaction *transaction;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!new_object)
		return CKR_ARGUMENTS_BAD;
	if (!(!count || template))
		return CKR_ARGUMENTS_BAD;

	transaction = gkm_transaction_new ();

	object = gkm_session_create_object_for_attributes (self, transaction, template, count);

	rv = gkm_transaction_complete_and_unref (transaction);

	if (rv == CKR_OK) {
		g_assert (object);
		*new_object = gkm_object_get_handle (object);
		g_object_unref (object);
	}

	return rv;
}

CK_RV
gkm_session_C_CopyObject (GkmSession* self, CK_OBJECT_HANDLE object,
                          CK_ATTRIBUTE_PTR template, CK_ULONG count,
                          CK_OBJECT_HANDLE_PTR new_object)
{
	/*
	 * TODO: We need to implement this, initially perhaps only
	 * only for session objects.
	 */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_GetObjectSize (GkmSession* self, CK_OBJECT_HANDLE object, CK_ULONG_PTR size)
{
	/* TODO: Do we need to implement this? */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_GetAttributeValue (GkmSession* self, CK_OBJECT_HANDLE handle,
                                 CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	GkmObject *object;
	CK_ULONG i;
	CK_RV code, rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!(!count || template))
		return CKR_ARGUMENTS_BAD;

	rv = gkm_session_lookup_readable_object (self, handle, &object);
	if (rv != CKR_OK)
		return rv;

	rv = CKR_OK;

	for (i = 0; i < count; ++i) {
		code = gkm_object_get_attribute (object, self, &template[i]);

		/* Not a true error, keep going */
		if (code == CKR_ATTRIBUTE_SENSITIVE ||
		    code == CKR_ATTRIBUTE_TYPE_INVALID) {
			template[i].ulValueLen = (CK_ULONG)-1;
			rv = code;

		} else if(code == CKR_BUFFER_TOO_SMALL) {
			rv = code;

		/* Any other error aborts */
		} else if (code != CKR_OK) {
			rv = code;
			break;
		}
	}

	return rv;
}

CK_RV
gkm_session_C_SetAttributeValue (GkmSession* self, CK_OBJECT_HANDLE handle,
                                 CK_ATTRIBUTE_PTR template, CK_ULONG count)
{
	GkmObject *object = NULL;
	GkmTransaction *transaction;
	CK_ULONG i;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!(!count || template))
		return CKR_ARGUMENTS_BAD;

	rv = gkm_session_lookup_writable_object (self, handle, &object);
	if (rv != CKR_OK)
		return rv;

	/* The transaction for this whole dealio */
	transaction = gkm_transaction_new ();

	for (i = 0; i < count && !gkm_transaction_get_failed (transaction); ++i)
		gkm_object_set_attribute (object, self, transaction, &template[i]);

	/* Store the object */
	if (!gkm_transaction_get_failed (transaction) && gkm_object_is_token (object))
		gkm_module_store_token_object (self->pv->module, transaction, object);

	gkm_transaction_complete (transaction);
	rv = gkm_transaction_get_result (transaction);
	g_object_unref (transaction);

	return rv;
}

CK_RV
gkm_session_C_DestroyObject (GkmSession* self, CK_OBJECT_HANDLE handle)
{
	GkmObject *object;
	GkmSession *session;
	GkmTransaction *transaction;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);

	rv = gkm_session_lookup_writable_object (self, handle, &object);
	if (rv != CKR_OK)
		return rv;

	transaction = gkm_transaction_new ();

	/* Lookup the actual session that owns this object, if no session, then a token object */
	session = gkm_session_for_session_object (object);
	if (session != NULL)
		remove_object (session, transaction, object);
	else
		gkm_module_remove_token_object (self->pv->module, transaction, object);

	gkm_transaction_complete (transaction);
	rv = gkm_transaction_get_result (transaction);
	g_object_unref (transaction);

	if (rv == CKR_OK) {
		/* Check that it's really gone */
		g_return_val_if_fail (gkm_session_lookup_readable_object (self, handle, &object) ==
		                      CKR_OBJECT_HANDLE_INVALID, CKR_GENERAL_ERROR);
	}

	return rv;
}

CK_RV
gkm_session_C_FindObjectsInit (GkmSession* self, CK_ATTRIBUTE_PTR template,
                               CK_ULONG count)
{
	CK_BBOOL token = CK_FALSE;
	gboolean also_private;
	CK_RV rv = CKR_OK;
	GArray *found;
	gboolean all;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!(template || !count))
		return CKR_ARGUMENTS_BAD;

	/* Cancel any current operation */
	if (self->pv->current_operation) {
		(self->pv->current_operation) (self);
		g_assert (!self->pv->current_operation);
	}

	/* See whether this is token or not */
	all = !attributes_find_boolean (template, count, CKA_TOKEN, &token);

	/* An array of object handles */
	found = g_array_new (FALSE, TRUE, sizeof (CK_OBJECT_HANDLE));

	/* If not logged in, then skip private objects */
	also_private = gkm_session_get_logged_in (self) == CKU_USER;

	if (all || token) {
		rv = gkm_module_refresh_token (self->pv->module);
		if (rv == CKR_OK)
			rv = gkm_manager_find_handles (gkm_module_get_manager (self->pv->module),
			                               self, also_private, template, count, found);
	}

	if (rv == CKR_OK && (all || !token)) {
		rv = gkm_manager_find_handles (self->pv->manager, self, also_private,
		                               template, count, found);
	}

	if (rv != CKR_OK) {
		g_array_free (found, TRUE);
		return rv;
	}

	g_assert (!self->pv->current_operation);
	g_assert (!self->pv->found_objects);

	self->pv->found_objects = found;
	self->pv->current_operation = cleanup_found;

	return CKR_OK;
}

CK_RV
gkm_session_C_FindObjects (GkmSession* self, CK_OBJECT_HANDLE_PTR objects,
                           CK_ULONG max_count, CK_ULONG_PTR count)
{
	CK_ULONG n_objects, i;
	GArray *found;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!(objects || !max_count))
		return CKR_ARGUMENTS_BAD;
	if (!count)
		return CKR_ARGUMENTS_BAD;

	if (self->pv->current_operation != cleanup_found)
		return CKR_OPERATION_NOT_INITIALIZED;

	g_assert (self->pv->found_objects);
	found = self->pv->found_objects;

	n_objects = MIN (max_count, found->len);
	if (n_objects > 0) {
		for (i = 0; i < n_objects; ++i)
			objects[i] = g_array_index (found, CK_OBJECT_HANDLE, i);
		g_array_remove_range (found, 0, n_objects);
	}

	*count = n_objects;
	return CKR_OK;

}

CK_RV
gkm_session_C_FindObjectsFinal (GkmSession* self)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);

	if (self->pv->current_operation != cleanup_found)
		return CKR_OPERATION_NOT_INITIALIZED;

	cleanup_found (self);
	return CKR_OK;
}

CK_RV
gkm_session_C_EncryptInit (GkmSession *self, CK_MECHANISM_PTR mechanism,
                           CK_OBJECT_HANDLE key)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!mechanism)
		return CKR_ARGUMENTS_BAD;
	return prepare_crypto (self, mechanism, CKA_ENCRYPT, key);
}

CK_RV
gkm_session_C_Encrypt (GkmSession *self, CK_BYTE_PTR data, CK_ULONG data_len,
                       CK_BYTE_PTR encrypted_data, CK_ULONG_PTR encrypted_data_len)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	return process_crypto (self, CKA_ENCRYPT, data, data_len, encrypted_data, encrypted_data_len);
}

CK_RV
gkm_session_C_EncryptUpdate (GkmSession *self, CK_BYTE_PTR part,
                             CK_ULONG part_len, CK_BYTE_PTR encrypted_part,
                             CK_ULONG_PTR encrypted_part_len)
{
	/* Our keys don't support this incremental encryption */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_EncryptFinal (GkmSession *self, CK_BYTE_PTR last_part,
                            CK_ULONG_PTR last_part_len)
{
	/* Our keys don't support this incremental encryption */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DecryptInit (GkmSession *self, CK_MECHANISM_PTR mechanism,
                           CK_OBJECT_HANDLE key)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!mechanism)
		return CKR_ARGUMENTS_BAD;
	return prepare_crypto (self, mechanism, CKA_DECRYPT, key);
}

CK_RV
gkm_session_C_Decrypt (GkmSession *self, CK_BYTE_PTR enc_data,
                       CK_ULONG enc_data_len, CK_BYTE_PTR data, CK_ULONG_PTR data_len)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	return process_crypto (self, CKA_DECRYPT, enc_data, enc_data_len, data, data_len);
}

CK_RV
gkm_session_C_DecryptUpdate (GkmSession *self, CK_BYTE_PTR enc_part,
                             CK_ULONG enc_part_len, CK_BYTE_PTR part, CK_ULONG_PTR part_len)
{
	/* Our keys don't support this incremental decryption */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DecryptFinal (GkmSession *self, CK_BYTE_PTR last_part,
                            CK_ULONG_PTR last_part_len)
{
	/* Our keys don't support this incremental decryption */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DigestInit (GkmSession *self, CK_MECHANISM_PTR mechanism)
{
	/* We don't do digests */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_Digest (GkmSession *self, CK_BYTE_PTR data, CK_ULONG data_len,
                      CK_BYTE_PTR digest, CK_ULONG_PTR digest_len)
{
	/* We don't do digests */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DigestUpdate (GkmSession *self, CK_BYTE_PTR part, CK_ULONG part_len)
{
	/* We don't do digests */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DigestKey (GkmSession *self, CK_OBJECT_HANDLE key)
{
	/* We don't do digests */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DigestFinal (GkmSession *self, CK_BYTE_PTR digest,
                           CK_ULONG_PTR digest_len)
{
	/* We don't do digests */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_SignInit (GkmSession *self, CK_MECHANISM_PTR mechanism,
                        CK_OBJECT_HANDLE key)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!mechanism)
		return CKR_ARGUMENTS_BAD;
	return prepare_crypto (self, mechanism, CKA_SIGN, key);
}

CK_RV
gkm_session_C_Sign (GkmSession *self, CK_BYTE_PTR data, CK_ULONG data_len,
                    CK_BYTE_PTR signature, CK_ULONG_PTR signature_len)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	return process_crypto (self, CKA_SIGN, data, data_len, signature, signature_len);
}

CK_RV
gkm_session_C_SignUpdate (GkmSession *self, CK_BYTE_PTR part, CK_ULONG part_len)
{
	/* Our keys don't support incremental operations */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_SignFinal (GkmSession *self, CK_BYTE_PTR signature,
                         CK_ULONG_PTR signature_len)
{
	/* Our keys don't support incremental operations */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_SignRecoverInit (GkmSession *self, CK_MECHANISM_PTR mechanism,
                               CK_OBJECT_HANDLE key)
{
	/* TODO: Need to implement */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_SignRecover (GkmSession *self, CK_BYTE_PTR data, CK_ULONG data_len,
                           CK_BYTE_PTR signature, CK_ULONG_PTR signature_len)
{
	/* TODO: Need to implement */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_VerifyInit (GkmSession *self, CK_MECHANISM_PTR mechanism,
                          CK_OBJECT_HANDLE key)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!mechanism)
		return CKR_ARGUMENTS_BAD;
	return prepare_crypto (self, mechanism, CKA_VERIFY, key);
}

CK_RV
gkm_session_C_Verify (GkmSession *self, CK_BYTE_PTR data, CK_ULONG data_len,
                      CK_BYTE_PTR signature, CK_ULONG signature_len)
{
	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	return process_crypto (self, CKA_VERIFY, data, data_len, signature, &signature_len);
}

CK_RV
gkm_session_C_VerifyUpdate (GkmSession *self, CK_BYTE_PTR part, CK_ULONG part_len)
{
	/* Our keys don't support incremental operations */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_VerifyFinal (GkmSession *self, CK_BYTE_PTR signature,
                           CK_ULONG signature_len)
{
	/* Our keys don't support incremental operations */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_VerifyRecoverInit (GkmSession *self, CK_MECHANISM_PTR mechanism,
                                 CK_OBJECT_HANDLE key)
{
	/* TODO: Need to implement */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_VerifyRecover (GkmSession *self, CK_BYTE_PTR signature,
                             CK_ULONG signature_len, CK_BYTE_PTR data,
                             CK_ULONG_PTR data_len)
{
	/* TODO: Need to implement */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DigestEncryptUpdate (GkmSession *self, CK_BYTE_PTR part,
                                   CK_ULONG part_len, CK_BYTE_PTR enc_part,
                                   CK_ULONG_PTR enc_part_len)
{
	/* We don't support double operations */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DecryptDigestUpdate (GkmSession *self, CK_BYTE_PTR enc_part,
                                   CK_ULONG enc_part_len, CK_BYTE_PTR part,
                                   CK_ULONG_PTR part_len)
{
	/* We don't support double operations */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_SignEncryptUpdate (GkmSession *self, CK_BYTE_PTR part,
                                 CK_ULONG part_len, CK_BYTE_PTR enc_part,
				 CK_ULONG_PTR enc_part_len)
{
	/* We don't support double operations */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_DecryptVerifyUpdate (GkmSession *self, CK_BYTE_PTR enc_part,
                                   CK_ULONG enc_part_len, CK_BYTE_PTR part,
                                   CK_ULONG_PTR part_len)
{
	/* We don't support double operations */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_GenerateKey (GkmSession* self, CK_MECHANISM_PTR mechanism,
                           CK_ATTRIBUTE_PTR template, CK_ULONG count,
                           CK_OBJECT_HANDLE_PTR key)
{
	/* TODO: We need to implement this */
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_session_C_GenerateKeyPair (GkmSession* self, CK_MECHANISM_PTR mechanism,
                               CK_ATTRIBUTE_PTR pub_template, CK_ULONG pub_count,
                               CK_ATTRIBUTE_PTR priv_template, CK_ULONG priv_count,
                               CK_OBJECT_HANDLE_PTR pub_key, CK_OBJECT_HANDLE_PTR priv_key)
{
	GkmObject *pub = NULL;
	GkmObject *priv = NULL;
	GkmTransaction *transaction;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!mechanism)
		return CKR_ARGUMENTS_BAD;
	if (!(!pub_count || pub_template))
		return CKR_ARGUMENTS_BAD;
	if (!(!priv_count || priv_template))
		return CKR_ARGUMENTS_BAD;
	if (!pub_key || !priv_key)
		return CKR_ARGUMENTS_BAD;

	/*
	 * Duplicate the memory for the attributes (but not values) so we
	 * can 'consume' in the generator and create object functions.
	 */
	pub_template = g_memdup (pub_template, pub_count * sizeof (CK_ATTRIBUTE));
	priv_template = g_memdup (priv_template, priv_count * sizeof (CK_ATTRIBUTE));
	transaction = gkm_transaction_new ();

	/* Actually do the object creation */
	rv = gkm_crypto_generate_key_pair (self, mechanism->mechanism, pub_template, pub_count,
	                                   priv_template, priv_count, &pub, &priv);
	if (rv != CKR_OK)
		gkm_transaction_fail (transaction, rv);

	g_free (pub_template);
	g_free (priv_template);

	gkm_transaction_complete (transaction);
	rv = gkm_transaction_get_result (transaction);
	g_object_unref (transaction);

	if (rv == CKR_OK) {
		*pub_key = gkm_object_get_handle (pub);
		*priv_key = gkm_object_get_handle (priv);
	}

	/* Objects are owned by storage */
	if (pub != NULL)
		g_object_unref (pub);
	if (priv != NULL)
		g_object_unref (priv);

	return rv;
}

CK_RV
gkm_session_C_WrapKey (GkmSession* self, CK_MECHANISM_PTR mechanism,
                       CK_OBJECT_HANDLE wrapping_key, CK_OBJECT_HANDLE key,
                       CK_BYTE_PTR wrapped_key, CK_ULONG_PTR wrapped_key_len)
{
	GkmObject *wrapper = NULL;
	GkmObject *wrapped = NULL;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!mechanism)
		return CKR_ARGUMENTS_BAD;
	if (!wrapped_key_len)
		return CKR_ARGUMENTS_BAD;

	rv = gkm_session_lookup_readable_object (self, wrapping_key, &wrapper);
	if (rv == CKR_OBJECT_HANDLE_INVALID)
		return CKR_WRAPPING_KEY_HANDLE_INVALID;
	else if (rv != CKR_OK)
		return rv;

	rv = gkm_session_lookup_readable_object (self, key, &wrapped);
	if (rv == CKR_OBJECT_HANDLE_INVALID)
		return CKR_KEY_HANDLE_INVALID;
	else if (rv != CKR_OK)
		return rv;

	return gkm_crypto_wrap_key (self, mechanism, wrapper, wrapped,
	                            wrapped_key, wrapped_key_len);
}

CK_RV
gkm_session_C_UnwrapKey (GkmSession* self, CK_MECHANISM_PTR mechanism,
                         CK_OBJECT_HANDLE unwrapping_key, CK_BYTE_PTR wrapped_key,
                         CK_ULONG wrapped_key_len, CK_ATTRIBUTE_PTR template,
                         CK_ULONG count, CK_OBJECT_HANDLE_PTR key)
{
	GkmObject *wrapper = NULL;
	GkmObject *unwrapped = NULL;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!mechanism)
		return CKR_ARGUMENTS_BAD;
	if (!(!count || template))
		return CKR_ARGUMENTS_BAD;
	if (!key)
		return CKR_ARGUMENTS_BAD;

	rv = gkm_session_lookup_readable_object (self, unwrapping_key, &wrapper);
	if (rv == CKR_OBJECT_HANDLE_INVALID)
		return CKR_WRAPPING_KEY_HANDLE_INVALID;
	else if (rv != CKR_OK)
		return rv;

	/*
	 * Duplicate the memory for the attributes (but not values) so we
	 * can 'consume' in the generator and create object functions.
	 */
	template = g_memdup (template, count * sizeof (CK_ATTRIBUTE));

	rv = gkm_crypto_unwrap_key (self, mechanism, wrapper, wrapped_key,
	                            wrapped_key_len, template, count, &unwrapped);

	g_free (template);

	if (rv == CKR_OK) {
		*key = gkm_object_get_handle (unwrapped);
		g_object_unref (unwrapped);
	}

	return rv;
}

CK_RV
gkm_session_C_DeriveKey (GkmSession* self, CK_MECHANISM_PTR mechanism,
                         CK_OBJECT_HANDLE base_key, CK_ATTRIBUTE_PTR template,
                         CK_ULONG count, CK_OBJECT_HANDLE_PTR key)
{
	GkmObject *base = NULL;
	GkmObject *derived = NULL;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_SESSION (self), CKR_SESSION_HANDLE_INVALID);
	if (!mechanism)
		return CKR_ARGUMENTS_BAD;
	if (!(!count || template))
		return CKR_ARGUMENTS_BAD;
	if (!key)
		return CKR_ARGUMENTS_BAD;

	rv = gkm_session_lookup_readable_object (self, base_key, &base);
	if (rv != CKR_OK)
		return rv;

	/*
	 * Duplicate the memory for the attributes (but not values) so we
	 * can 'consume' in the generator and create object functions.
	 */
	template = g_memdup (template, count * sizeof (CK_ATTRIBUTE));

	/* Actually do the object creation */
	rv = gkm_crypto_derive_key (self, mechanism, base, template, count, &derived);

	g_free (template);

	if (rv == CKR_OK) {
		*key = gkm_object_get_handle (derived);
		g_object_unref (derived);
	}

	return rv;
}

CK_RV
gkm_session_C_SeedRandom (GkmSession* self, CK_BYTE_PTR seed, CK_ULONG seed_len)
{
	/* We don't have a RNG */
	return CKR_RANDOM_NO_RNG;
}

CK_RV
gkm_session_C_GenerateRandom (GkmSession* self, CK_BYTE_PTR random_data,
                              CK_ULONG random_len)
{
	/* We don't have a RNG */
	return CKR_RANDOM_NO_RNG;
}
