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

#include "gkm-ssh-openssh.h"
#include "gkm-ssh-private-key.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-credential.h"
#include "gkm/gkm-manager.h"
#include "gkm/gkm-module.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-sexp.h"
#include "gkm/gkm-util.h"

#include "pkcs11/pkcs11i.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_LABEL,
	PROP_PUBLIC_KEY
};

struct _GkmSshPrivateKey {
	GkmPrivateXsaKey parent;

	GkmSshPublicKey *pubkey;
	gchar *label;
	guchar *private_data;
	gsize n_private_data;

	gboolean is_encrypted;
};

G_DEFINE_TYPE (GkmSshPrivateKey, gkm_ssh_private_key, GKM_TYPE_PRIVATE_XSA_KEY);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static CK_RV
unlock_private_key (GkmSshPrivateKey *self, const gchar *password,
                    gssize n_password, GkmSexp **result)
{
	GkmDataResult res;
	gcry_sexp_t sexp;
	GkmSexp *wrapper;

	g_assert (GKM_IS_SSH_PRIVATE_KEY (self));

	res = gkm_ssh_openssh_parse_private_key (self->private_data,
	                                         self->n_private_data,
	                                         password, n_password, &sexp);

	switch (res) {
	case GKM_DATA_LOCKED:
		self->is_encrypted = TRUE;
		return CKR_PIN_INCORRECT;
	case GKM_DATA_FAILURE:
		g_message ("couldn't parse private SSH key: %s", self->label);
		return CKR_GENERAL_ERROR;
	case GKM_DATA_UNRECOGNIZED:
		g_message ("invalid or unrecognized private SSH key: %s", self->label);
		return CKR_FUNCTION_FAILED;
	case GKM_DATA_SUCCESS:
		break;
	default:
		g_assert_not_reached();
	}

	if (!password || !password[0])
		self->is_encrypted = FALSE;

	wrapper = gkm_sexp_new (sexp);
	*result = wrapper;

	return CKR_OK;
}

static void
realize_and_take_data (GkmSshPrivateKey *self, gcry_sexp_t sexp, gchar *comment,
                       guchar *private_data, gsize n_private_data)
{
	GkmSexp *wrapper;

	g_assert (GKM_IS_SSH_PRIVATE_KEY (self));

	/* The base public key gets setup. */
	wrapper = gkm_sexp_new (sexp);
	gkm_sexp_key_set_base (GKM_SEXP_KEY (self), wrapper);
	gkm_sexp_key_set_base (GKM_SEXP_KEY (self->pubkey), wrapper);
	gkm_sexp_unref (wrapper);

	/* Own the comment */
	gkm_ssh_public_key_set_label (self->pubkey, comment);
	gkm_ssh_private_key_set_label (self, comment);
	g_free (comment);

	/* Own the data */
	g_free (self->private_data);
	self->private_data = private_data;
	self->n_private_data = n_private_data;

	/* Try to parse the private data, and note if it's not actually encrypted */
	self->is_encrypted = TRUE;
	if (unlock_private_key (self, "", 0, &wrapper) == CKR_OK) {
		self->is_encrypted = FALSE;
		gkm_private_xsa_key_set_unlocked_private (GKM_PRIVATE_XSA_KEY (self), wrapper);
		gkm_sexp_unref (wrapper);
	}
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_ssh_private_key_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmSshPrivateKey *self = GKM_SSH_PRIVATE_KEY (base);
	gchar *digest;
	CK_RV rv;

	switch (attr->type) {
	case CKA_LABEL:
		return gkm_attribute_set_string (attr, self->label);

	/* COMPAT: Previous versions of gnome-keyring used this to save unlock passwords */
	case CKA_GNOME_INTERNAL_SHA1:
		if (!self->private_data)
			return CKR_ATTRIBUTE_TYPE_INVALID;
		digest = gkm_ssh_openssh_digest_private_key (self->private_data, self->n_private_data);
		rv = gkm_attribute_set_string (attr, digest);
		g_free (digest);
		return rv;
	}

	return GKM_OBJECT_CLASS (gkm_ssh_private_key_parent_class)->get_attribute (base, session, attr);
}

static CK_RV
gkm_ssh_private_key_unlock (GkmObject *base, GkmCredential *cred)
{
	GkmSshPrivateKey *self = GKM_SSH_PRIVATE_KEY (base);
	const gchar *password;
	GkmSexp *wrapper;
	gsize n_password;
	CK_RV rv;

	if (!self->is_encrypted)
		return CKR_OK;

	password = gkm_credential_get_password (cred, &n_password);
	rv = unlock_private_key (self, password, n_password, &wrapper);

	if (rv == CKR_OK) {
		gkm_private_xsa_key_set_locked_private (GKM_PRIVATE_XSA_KEY (self), cred, wrapper);
		gkm_sexp_unref (wrapper);
	}

	return rv;
}

static void
gkm_ssh_private_key_expose (GkmObject *base, gboolean expose)
{
	GKM_OBJECT_CLASS (gkm_ssh_private_key_parent_class)->expose_object (base, expose);
	gkm_object_expose (GKM_OBJECT (GKM_SSH_PRIVATE_KEY (base)->pubkey), expose);
}

static GObject*
gkm_ssh_private_key_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmSshPrivateKey *self = GKM_SSH_PRIVATE_KEY (G_OBJECT_CLASS (gkm_ssh_private_key_parent_class)->constructor(type, n_props, props));
	GkmObject *object;
	gchar *unique;

	g_return_val_if_fail (self, NULL);

	object = GKM_OBJECT (self);
	unique = g_strdup_printf ("%s.pub", gkm_object_get_unique (object));
	self->pubkey = gkm_ssh_public_key_new (gkm_object_get_module (object), unique);
	g_free (unique);

	return G_OBJECT (self);
}

static void
gkm_ssh_private_key_init (GkmSshPrivateKey *self)
{

}

static void
gkm_ssh_private_key_dispose (GObject *obj)
{
	GkmSshPrivateKey *self = GKM_SSH_PRIVATE_KEY (obj);

	if (self->pubkey)
		g_object_unref (self->pubkey);
	self->pubkey = NULL;

	G_OBJECT_CLASS (gkm_ssh_private_key_parent_class)->dispose (obj);
}

static void
gkm_ssh_private_key_finalize (GObject *obj)
{
	GkmSshPrivateKey *self = GKM_SSH_PRIVATE_KEY (obj);

	g_assert (self->pubkey == NULL);

	g_free (self->private_data);
	self->private_data = NULL;

	g_free (self->label);
	self->label = NULL;

	G_OBJECT_CLASS (gkm_ssh_private_key_parent_class)->finalize (obj);
}

static void
gkm_ssh_private_key_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	GkmSshPrivateKey *self = GKM_SSH_PRIVATE_KEY (obj);

	switch (prop_id) {
	case PROP_LABEL:
		gkm_ssh_private_key_set_label (self, g_value_get_string (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_ssh_private_key_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	GkmSshPrivateKey *self = GKM_SSH_PRIVATE_KEY (obj);

	switch (prop_id) {
	case PROP_LABEL:
		g_value_set_string (value, gkm_ssh_private_key_get_label (self));
		break;
	case PROP_PUBLIC_KEY:
		g_value_set_object (value, gkm_ssh_private_key_get_public_key (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_ssh_private_key_class_init (GkmSshPrivateKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gobject_class->constructor = gkm_ssh_private_key_constructor;
	gobject_class->dispose = gkm_ssh_private_key_dispose;
	gobject_class->finalize = gkm_ssh_private_key_finalize;
	gobject_class->set_property = gkm_ssh_private_key_set_property;
	gobject_class->get_property = gkm_ssh_private_key_get_property;

	gkm_class->get_attribute = gkm_ssh_private_key_get_attribute;
	gkm_class->unlock = gkm_ssh_private_key_unlock;
	gkm_class->expose_object = gkm_ssh_private_key_expose;

	g_object_class_install_property (gobject_class, PROP_LABEL,
	           g_param_spec_string ("label", "Label", "Object Label",
	                                "", G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_PUBLIC_KEY,
	           g_param_spec_object ("public-key", "Public Key", "Public key belonging to this private key",
	                                GKM_TYPE_SSH_PUBLIC_KEY, G_PARAM_READABLE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmSshPrivateKey*
gkm_ssh_private_key_new (GkmModule *module, const gchar *unique)
{
	return g_object_new (GKM_TYPE_SSH_PRIVATE_KEY, "unique", unique,
	                     "module", module, "manager", gkm_module_get_manager (module), NULL);
}

gboolean
gkm_ssh_private_key_parse (GkmSshPrivateKey *self, const gchar *public_path,
                           const gchar *private_path, GError **error)
{
	guchar *public_data, *private_data;
	gsize n_public_data, n_private_data;
	GkmDataResult res;
	gcry_sexp_t sexp;
	gchar *comment;

	g_return_val_if_fail (GKM_IS_SSH_PRIVATE_KEY (self), FALSE);
	g_return_val_if_fail (private_path, FALSE);
	g_return_val_if_fail (!error || !*error, FALSE);

	/* Read in the public key */
	if (!g_file_get_contents (public_path, (gchar**)&public_data, &n_public_data, error))
		return FALSE;

	/* Parse it */
	res = gkm_ssh_openssh_parse_public_key (public_data, n_public_data, &sexp, &comment);
	g_free (public_data);

	if (res == GKM_DATA_UNRECOGNIZED) {
		return FALSE;
	} else if (res != GKM_DATA_SUCCESS) {
		g_set_error_literal (error, GKM_DATA_ERROR, res, _("Couldn't parse public SSH key"));
		return FALSE;
	}

	/* Read in the private key */
	if (!g_file_get_contents (private_path, (gchar**)&private_data, &n_private_data, error)) {
		g_free (comment);
		gcry_sexp_release (sexp);
		return FALSE;
	}

	if (comment == NULL)
		comment = g_path_get_basename (private_path);

	realize_and_take_data (self, sexp, comment, private_data, n_private_data);
	return TRUE;
}

const gchar*
gkm_ssh_private_key_get_label (GkmSshPrivateKey *self)
{
	g_return_val_if_fail (GKM_IS_SSH_PRIVATE_KEY (self), NULL);
	return self->label;
}

void
gkm_ssh_private_key_set_label (GkmSshPrivateKey *self, const gchar *label)
{
	g_return_if_fail (GKM_IS_SSH_PRIVATE_KEY (self));
	g_free (self->label);
	self->label = g_strdup (label);
	g_object_notify (G_OBJECT (self), "label");
}

GkmSshPublicKey*
gkm_ssh_private_key_get_public_key (GkmSshPrivateKey *self)
{
	g_return_val_if_fail (GKM_IS_SSH_PRIVATE_KEY (self), NULL);
	return self->pubkey;
}
