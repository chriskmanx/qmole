/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gkm-attributes.h"
#include "gkm-credential.h"
#include "gkm-secret.h"
#include "gkm-session.h"
#include "gkm-transaction.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11i.h"

enum {
	PROP_0,
	PROP_OBJECT,
	PROP_SECRET
};

struct _GkmCredentialPrivate {

	/* The object we authenticated */
	GkmObject *object;

	/* Secret which created this credential */
	GkmSecret *secret;

	/* Stored data */
	GType user_type;
	gpointer user_data;
};

G_DEFINE_TYPE (GkmCredential, gkm_credential, GKM_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GkmObject*
factory_create_credential (GkmSession *session, GkmTransaction *transaction,
                              CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	CK_OBJECT_HANDLE handle;
	GkmCredential *cred;
	CK_ATTRIBUTE *attr;
	GkmManager *manager;
	GkmModule *module;
	GkmObject *object = NULL;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_TRANSACTION (transaction), NULL);
	g_return_val_if_fail (attrs || !n_attrs, NULL);

	/* The handle is optional */
	if (gkm_attributes_find_ulong (attrs, n_attrs, CKA_G_OBJECT, &handle)) {
		rv = gkm_session_lookup_readable_object (session, handle, &object);
		if (rv != CKR_OK) {
			gkm_transaction_fail (transaction, rv);
			return NULL;
		}
	} else {
		object = NULL;
	}

	/* The value is optional */
	attr = gkm_attributes_find (attrs, n_attrs, CKA_VALUE);

	gkm_attributes_consume (attrs, n_attrs, CKA_VALUE, CKA_G_OBJECT, G_MAXULONG);

	module = gkm_session_get_module (session);
	manager = gkm_manager_for_template (attrs, n_attrs, session);
	rv = gkm_credential_create (module, manager, object,
	                            attr ? attr->pValue : NULL,
	                            attr ? attr->ulValueLen : 0, &cred);

	if (rv == CKR_OK) {
		gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (cred),
		                                      TRUE, attrs, n_attrs);
		return GKM_OBJECT (cred);
	} else {
		gkm_transaction_fail (transaction, rv);
		return NULL;
	}
}

static void
self_destruct (GkmCredential *self)
{
	GkmTransaction *transaction;
	CK_RV rv;

	g_assert (GKM_IS_CREDENTIAL (self));

	transaction = gkm_transaction_new ();

	/* Destroy ourselves */
	gkm_object_destroy (GKM_OBJECT (self), transaction);

	gkm_transaction_complete (transaction);
	rv = gkm_transaction_get_result (transaction);
	g_object_unref (transaction);

	if (rv != CKR_OK)
		g_warning ("Couldn't destroy credential object: (code %lu)", (gulong)rv);
}

static void
object_went_away (gpointer data, GObject *old_object)
{
	GkmCredential *self = data;
	g_return_if_fail (GKM_IS_CREDENTIAL (self));
	self->pv->object = NULL;
	self_destruct (self);
}

static void
clear_data (GkmCredential *self)
{
	if (!self->pv->user_data)
		return;
	if (G_TYPE_IS_BOXED (self->pv->user_type))
		g_boxed_free (self->pv->user_type, self->pv->user_data);
	else if (G_TYPE_IS_OBJECT (self->pv->user_type))
		g_object_unref (self->pv->user_data);
	else
		g_assert_not_reached ();
	self->pv->user_data = NULL;
	self->pv->user_type = 0;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_credential_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE *attr)
{
	GkmCredential *self = GKM_CREDENTIAL (base);
	CK_OBJECT_HANDLE handle;

	switch (attr->type) {

	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_G_CREDENTIAL);

	case CKA_PRIVATE:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_G_OBJECT:
		handle = self->pv->object ? gkm_object_get_handle (self->pv->object) : 0;
		return gkm_attribute_set_ulong (attr, handle);

	case CKA_VALUE:
		return CKR_ATTRIBUTE_SENSITIVE;
	};

	return GKM_OBJECT_CLASS (gkm_credential_parent_class)->get_attribute (base, session, attr);
}

static GObject*
gkm_credential_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmCredential *self = GKM_CREDENTIAL (G_OBJECT_CLASS (gkm_credential_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	return G_OBJECT (self);
}

static void
gkm_credential_init (GkmCredential *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_CREDENTIAL, GkmCredentialPrivate);
}

static void
gkm_credential_dispose (GObject *obj)
{
	GkmCredential *self = GKM_CREDENTIAL (obj);

	if (self->pv->object)
		g_object_weak_unref (G_OBJECT (self->pv->object), object_went_away, self);
	self->pv->object = NULL;

	clear_data (self);

	G_OBJECT_CLASS (gkm_credential_parent_class)->dispose (obj);
}

static void
gkm_credential_finalize (GObject *obj)
{
	GkmCredential *self = GKM_CREDENTIAL (obj);

	g_assert (!self->pv->object);
	g_assert (!self->pv->user_type);
	g_assert (!self->pv->user_data);

	G_OBJECT_CLASS (gkm_credential_parent_class)->finalize (obj);
}

static void
gkm_credential_set_property (GObject *obj, guint prop_id, const GValue *value,
                                GParamSpec *pspec)
{
	GkmCredential *self = GKM_CREDENTIAL (obj);
	GkmObject *object;

	switch (prop_id) {
	case PROP_OBJECT:
		object = g_value_get_object (value);
		if (object)
			gkm_credential_connect (self, object);
		else
			g_return_if_fail (!self->pv->object);
		break;
	case PROP_SECRET:
		gkm_credential_set_secret (self, g_value_get_object (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_credential_get_property (GObject *obj, guint prop_id, GValue *value,
                              GParamSpec *pspec)
{
	GkmCredential *self = GKM_CREDENTIAL (obj);

	switch (prop_id) {
	case PROP_OBJECT:
		g_value_set_object (value, gkm_credential_get_object (self));
		break;
	case PROP_SECRET:
		g_value_set_object (value, gkm_credential_get_secret (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_credential_class_init (GkmCredentialClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gkm_credential_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GkmCredentialPrivate));

	gobject_class->constructor = gkm_credential_constructor;
	gobject_class->dispose = gkm_credential_dispose;
	gobject_class->finalize = gkm_credential_finalize;
	gobject_class->set_property = gkm_credential_set_property;
	gobject_class->get_property = gkm_credential_get_property;

	gkm_class->get_attribute = gkm_credential_real_get_attribute;

	g_object_class_install_property (gobject_class, PROP_OBJECT,
	           g_param_spec_object ("object", "Object", "Object authenticated",
	                                GKM_TYPE_OBJECT, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_SECRET,
	           g_param_spec_object ("secret", "Secret", "Optiontal secret",
	                                GKM_TYPE_SECRET, G_PARAM_READWRITE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmFactory*
gkm_credential_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_G_CREDENTIAL;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_credential
	};

	return &factory;
}

CK_RV
gkm_credential_create (GkmModule *module, GkmManager *manager, GkmObject *object,
                       CK_UTF8CHAR_PTR pin, CK_ULONG n_pin, GkmCredential **result)
{
	GkmCredential *cred;
	GkmSecret *secret = NULL;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_MODULE (module), CKR_GENERAL_ERROR);
	g_return_val_if_fail (!object || GKM_IS_OBJECT (object), CKR_GENERAL_ERROR);
	g_return_val_if_fail (!manager || GKM_IS_MANAGER (manager), CKR_GENERAL_ERROR);
	g_return_val_if_fail (result, CKR_GENERAL_ERROR);

	secret = gkm_secret_new_from_login (pin, n_pin);
	cred = g_object_new (GKM_TYPE_CREDENTIAL,
	                     "module", module,
	                     "manager", manager,
	                     "secret", secret,
	                     "object", object,
	                     NULL);
	g_object_unref (secret);

	/* If we have an object, the unlock must work */
	if (object) {
		rv = gkm_object_unlock (object, cred);
		if (rv == CKR_OK)
			*result = cred;
		else
			g_object_unref (cred);

	/* Created credentials without object */
	} else {
		*result = cred;
		rv = CKR_OK;
	}

	return rv;
}

void
gkm_credential_connect (GkmCredential *self, GkmObject *object)
{
	g_return_if_fail (GKM_IS_CREDENTIAL (self));
	g_return_if_fail (GKM_IS_OBJECT (object));
	g_return_if_fail (self->pv->object == NULL);
	g_return_if_fail (GKM_OBJECT (self) != object);
	self->pv->object = object;
	g_object_weak_ref (G_OBJECT (self->pv->object), object_went_away, self);
}

GkmSecret*
gkm_credential_get_secret (GkmCredential *self)
{
	g_return_val_if_fail (GKM_IS_CREDENTIAL (self), NULL);
	return self->pv->secret;
}

void
gkm_credential_set_secret (GkmCredential *self, GkmSecret *secret)
{
	g_return_if_fail (GKM_IS_CREDENTIAL (self));

	if (secret) {
		g_return_if_fail (GKM_IS_SECRET (secret));
		g_object_ref (secret);
	}
	if (self->pv->secret)
		g_object_unref (self->pv->secret);
	self->pv->secret = secret;

	g_object_notify (G_OBJECT (self), "secret");
}

const gchar*
gkm_credential_get_password (GkmCredential *self, gsize *n_password)
{
	g_return_val_if_fail (GKM_IS_CREDENTIAL (self), NULL);
	g_return_val_if_fail (n_password, NULL);

	if (!self->pv->secret) {
		*n_password = 0;
		return NULL;
	}

	return gkm_secret_get_password (self->pv->secret, n_password);
}

GkmObject*
gkm_credential_get_object (GkmCredential *self)
{
	g_return_val_if_fail (GKM_IS_CREDENTIAL (self), NULL);
	return self->pv->object;
}

gpointer
gkm_credential_peek_data (GkmCredential *self, GType type)
{
	g_return_val_if_fail (GKM_IS_CREDENTIAL (self), NULL);
	if (!self->pv->user_data)
		return NULL;
	g_return_val_if_fail (type == self->pv->user_type, NULL);
	return self->pv->user_data;
}

gpointer
gkm_credential_pop_data (GkmCredential *self, GType type)
{
	gpointer data = NULL;
	g_return_val_if_fail (GKM_IS_CREDENTIAL (self), NULL);

	if (self->pv->user_data) {
		g_return_val_if_fail (type == self->pv->user_type, NULL);
		if (G_TYPE_IS_BOXED (self->pv->user_type))
			data = g_boxed_copy (self->pv->user_type, self->pv->user_data);
		else if (G_TYPE_IS_OBJECT (self->pv->user_type))
			data = g_object_ref (self->pv->user_data);
		else
			g_assert_not_reached ();
	}

	gkm_object_mark_used (GKM_OBJECT (self));
	return data;
}

void
gkm_credential_set_data (GkmCredential *self, GType type, gpointer data)
{
	g_return_if_fail (GKM_IS_CREDENTIAL (self));

	if (data) {
		g_return_if_fail (type);
		g_return_if_fail (G_TYPE_IS_BOXED (type) || G_TYPE_IS_OBJECT (type));
	}

	clear_data (self);

	if (data) {
		self->pv->user_type = type;
		if (G_TYPE_IS_BOXED (type))
			self->pv->user_data = g_boxed_copy (type, data);
		else if (G_TYPE_IS_OBJECT (type))
			self->pv->user_data = g_object_ref (data);
		else
			g_assert_not_reached ();
	}
}

gboolean
gkm_credential_for_each (GkmSession *session, GkmObject *object,
                         GkmCredentialFunc func, gpointer user_data)
{
	CK_OBJECT_HANDLE handle;
	CK_OBJECT_CLASS klass;
	CK_ATTRIBUTE attrs[2];
	GList *results, *l;
	GkmCredential *cred;
	gboolean ret;

	g_return_val_if_fail (GKM_IS_SESSION (session), FALSE);
	g_return_val_if_fail (GKM_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (func, FALSE);

	/* Do we have one right on the session */
	cred = gkm_session_get_credential (session);
	if (cred && gkm_credential_get_object (cred) == object) {
		g_object_ref (cred);
		ret = (func) (cred, object, user_data);
		g_object_unref (cred);
		if (ret)
			return TRUE;
	}

	klass = CKO_G_CREDENTIAL;
	attrs[0].type = CKA_CLASS;
	attrs[0].pValue = &klass;
	attrs[0].ulValueLen = sizeof (klass);

	handle = gkm_object_get_handle (object);
	attrs[1].type = CKA_G_OBJECT;
	attrs[1].pValue = &handle;
	attrs[1].ulValueLen = sizeof (handle);

	/* Find any on the session */
	results = gkm_manager_find_by_attributes (gkm_session_get_manager (session),
	                                          session, attrs, G_N_ELEMENTS (attrs));

	for (l = results; l; l = g_list_next (l)) {
		g_object_ref (l->data);
		ret = (func) (l->data, object, user_data);
		g_object_unref (l->data);
		if (ret)
			break;
	}

	g_list_free (results);

	if (l != NULL)
		return TRUE;

	/* Find any in the token */
	results = gkm_manager_find_by_attributes (gkm_module_get_manager (gkm_session_get_module (session)),
	                                          session, attrs, G_N_ELEMENTS (attrs));

	for (l = results; l; l = g_list_next (l)) {
		g_object_ref (l->data);
		ret = (func) (l->data, object, user_data);
		g_object_unref (l->data);
		if (ret)
			break;
	}

	g_list_free (results);

	return (l != NULL);
}
