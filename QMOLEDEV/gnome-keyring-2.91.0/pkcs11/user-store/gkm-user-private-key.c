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

#include "gkm-user-private-key.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-crypto.h"
#include "gkm/gkm-data-der.h"
#include "gkm/gkm-factory.h"
#include "gkm/gkm-manager.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-secret.h"
#include "gkm/gkm-serializable.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-sexp.h"
#include "gkm/gkm-util.h"

#include "egg/egg-secure-memory.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
};

struct _GkmUserPrivateKey {
	GkmPrivateXsaKey parent;

	guchar *private_data;
	gsize n_private_data;

	GkmSexp *private_sexp;
	gboolean is_encrypted;
	GkmSecret *login;
};

static void gkm_user_private_key_serializable (GkmSerializableIface *iface);

G_DEFINE_TYPE_EXTENDED (GkmUserPrivateKey, gkm_user_private_key, GKM_TYPE_PRIVATE_XSA_KEY, 0,
               G_IMPLEMENT_INTERFACE (GKM_TYPE_SERIALIZABLE, gkm_user_private_key_serializable));

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GkmObject*
factory_create_private_key (GkmSession *session, GkmTransaction *transaction,
                            CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmUserPrivateKey *key;
	GkmSexp *sexp;

	g_return_val_if_fail (attrs || !n_attrs, NULL);

	sexp = gkm_private_xsa_key_create_sexp (session, transaction, attrs, n_attrs);
	if (sexp == NULL)
		return NULL;

	key = g_object_new (GKM_TYPE_USER_PRIVATE_KEY, "base-sexp", sexp,
	                    "module", gkm_session_get_module (session),
	                    "manager", gkm_manager_for_template (attrs, n_attrs, session),
	                    NULL);
	g_return_val_if_fail (!key->private_sexp, NULL);
	key->private_sexp = gkm_sexp_ref (sexp);

	gkm_sexp_unref (sexp);

	gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (key),
	                                      TRUE, attrs, n_attrs);
	return GKM_OBJECT (key);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_user_private_key_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	switch (attr->type) {
	case CKA_ALWAYS_AUTHENTICATE:
		return gkm_attribute_set_bool (attr, FALSE);
	}

	return GKM_OBJECT_CLASS (gkm_user_private_key_parent_class)->get_attribute (base, session, attr);
}

static GkmSexp*
gkm_user_private_key_real_acquire_crypto_sexp (GkmSexpKey *base, GkmSession *unused)
{
	GkmUserPrivateKey *self = GKM_USER_PRIVATE_KEY (base);
	gcry_sexp_t sexp;
	GkmDataResult res;
	const gchar *password;
	gsize n_password;

	/* Non encrypted case */
	if (self->private_sexp)
		return gkm_sexp_ref (self->private_sexp);

	g_return_val_if_fail (self->login, NULL);
	g_return_val_if_fail (self->is_encrypted, NULL);

	password = gkm_secret_get_password (self->login, &n_password);
	res = gkm_data_der_read_private_pkcs8 (self->private_data, self->n_private_data,
	                                       password, n_password, &sexp);
	g_return_val_if_fail (res == GKM_DATA_SUCCESS, NULL);

	return gkm_sexp_new (sexp);
}

static void
gkm_user_private_key_init (GkmUserPrivateKey *self)
{

}

static void
gkm_user_private_key_dispose (GObject *obj)
{
	GkmUserPrivateKey *self = GKM_USER_PRIVATE_KEY (obj);

	if (self->login)
		g_object_unref (self->login);
	self->login = NULL;

	G_OBJECT_CLASS (gkm_user_private_key_parent_class)->dispose (obj);
}

static void
gkm_user_private_key_finalize (GObject *obj)
{
	GkmUserPrivateKey *self = GKM_USER_PRIVATE_KEY (obj);

	g_assert (self->login == NULL);

	g_free (self->private_data);
	self->private_data = NULL;

	if (self->private_sexp)
		gkm_sexp_unref (self->private_sexp);
	self->private_sexp = NULL;

	G_OBJECT_CLASS (gkm_user_private_key_parent_class)->finalize (obj);
}

static void
gkm_user_private_key_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_user_private_key_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_user_private_key_class_init (GkmUserPrivateKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	GkmSexpKeyClass *key_class = GKM_SEXP_KEY_CLASS (klass);

	gobject_class->dispose = gkm_user_private_key_dispose;
	gobject_class->finalize = gkm_user_private_key_finalize;
	gobject_class->set_property = gkm_user_private_key_set_property;
	gobject_class->get_property = gkm_user_private_key_get_property;

	gkm_class->get_attribute = gkm_user_private_key_real_get_attribute;

	key_class->acquire_crypto_sexp = gkm_user_private_key_real_acquire_crypto_sexp;
}

static gboolean
gkm_user_private_key_real_load (GkmSerializable *base, GkmSecret *login, const guchar *data, gsize n_data)
{
	GkmUserPrivateKey *self = GKM_USER_PRIVATE_KEY (base);
	GkmDataResult res;
	gcry_sexp_t sexp, pub;
	GkmSexp *wrapper;
	const gchar *password;
	gsize n_password;

	g_return_val_if_fail (GKM_IS_USER_PRIVATE_KEY (self), FALSE);
	g_return_val_if_fail (data, FALSE);

	res = gkm_data_der_read_private_pkcs8 (data, n_data, NULL, 0, &sexp);

	/* An unencrypted pkcs8 file */
	if (res == GKM_DATA_SUCCESS) {
		self->is_encrypted = FALSE;

	/* If it's locked, then use our token password */
	} else if (res == GKM_DATA_LOCKED) {
		self->is_encrypted = TRUE;

		if (!login) {
			g_message ("encountered private key but no private key present");
			return FALSE;
		}

		password = gkm_secret_get_password (login, &n_password);
		res = gkm_data_der_read_private_pkcs8 (data, n_data, password, n_password, &sexp);
	}

	switch (res) {
	case GKM_DATA_LOCKED:
		g_message ("private key is encrypted with wrong password");
		return FALSE;
	case GKM_DATA_FAILURE:
		g_message ("couldn't parse private key");
		return FALSE;
	case GKM_DATA_UNRECOGNIZED:
		g_message ("invalid or unrecognized private key");
		return FALSE;
	case GKM_DATA_SUCCESS:
		break;
	default:
		g_assert_not_reached();
	}

	/* Calculate a public key as our 'base' */
	if (!gkm_sexp_key_to_public (sexp, &pub))
		g_return_val_if_reached (FALSE);

	/* Keep the public part of the key around for answering queries */
	wrapper = gkm_sexp_new (pub);
	gkm_sexp_key_set_base (GKM_SEXP_KEY (self), wrapper);
	gkm_sexp_unref (wrapper);

	/* Encrypted private key, keep login and data */
	if (self->is_encrypted) {
		g_free (self->private_data);
		self->n_private_data = n_data;
		self->private_data = g_memdup (data, n_data);

		g_object_ref (login);
		if (self->login)
			g_object_unref (self->login);
		self->login = login;

		/* Don't need the private key any more */
		gcry_sexp_release (sexp);

	/* Not encrypted, just keep the parsed key */
	} else {
		wrapper = gkm_sexp_new (sexp);
		if (self->private_sexp)
			gkm_sexp_unref (self->private_sexp);
		self->private_sexp = wrapper;

		if (self->login)
			g_object_unref (login);
		self->login = NULL;
	}

	return TRUE;
}

static gboolean
gkm_user_private_key_real_save (GkmSerializable *base, GkmSecret *login, guchar **data, gsize *n_data)
{
	GkmUserPrivateKey *self = GKM_USER_PRIVATE_KEY (base);
	const gchar *password;
	gsize n_password;
	GkmSexp *sexp;
	guchar *key;

	g_return_val_if_fail (GKM_IS_USER_PRIVATE_KEY (self), FALSE);
	g_return_val_if_fail (data, FALSE);
	g_return_val_if_fail (n_data, FALSE);

	sexp = gkm_user_private_key_real_acquire_crypto_sexp (GKM_SEXP_KEY (self), NULL);
	g_return_val_if_fail (sexp, FALSE);

	password = gkm_secret_get_password (login, &n_password);
	if (password == NULL) {
		key = gkm_data_der_write_private_pkcs8_plain (gkm_sexp_get (sexp), n_data);

		/*
		 * Caller is expecting normal memory buffer, which makes sense since
		 * this is being written to disk, and won't be 'secure' anyway.
		 */
		*data = g_memdup (key, *n_data);
		egg_secure_free (key);
	} else {
		*data = gkm_data_der_write_private_pkcs8_crypted (gkm_sexp_get (sexp), password,
		                                                  n_password, n_data);
	}

	gkm_sexp_unref (sexp);
	return *data != NULL;
}

static void
gkm_user_private_key_serializable (GkmSerializableIface *iface)
{
	iface->extension = ".pkcs8";
	iface->load = gkm_user_private_key_real_load;
	iface->save = gkm_user_private_key_real_save;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmFactory*
gkm_user_private_key_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_PRIVATE_KEY;
	static CK_BBOOL token = CK_TRUE;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_private_key
	};

	return &factory;
}
