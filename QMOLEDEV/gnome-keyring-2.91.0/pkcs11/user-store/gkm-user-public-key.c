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

#include "gkm-user-public-key.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-data-der.h"
#include "gkm/gkm-factory.h"
#include "gkm/gkm-serializable.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-util.h"

#include <glib/gi18n.h>

struct _GkmUserPublicKey {
	GkmPublicXsaKey parent;
};

static void gkm_user_public_key_serializable (GkmSerializableIface *iface);

G_DEFINE_TYPE_EXTENDED (GkmUserPublicKey, gkm_user_public_key, GKM_TYPE_PUBLIC_XSA_KEY, 0,
               G_IMPLEMENT_INTERFACE (GKM_TYPE_SERIALIZABLE, gkm_user_public_key_serializable));

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GkmObject*
factory_create_public_key (GkmSession *session, GkmTransaction *transaction,
                           CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmObject *object = NULL;
	GkmSexp *sexp;

	g_return_val_if_fail (attrs || !n_attrs, NULL);

	sexp = gkm_public_xsa_key_create_sexp (session, transaction, attrs, n_attrs);
	if (sexp != NULL) {
		object = g_object_new (GKM_TYPE_USER_PUBLIC_KEY, "base-sexp", sexp,
		                       "module", gkm_session_get_module (session),
		                       "manager", gkm_manager_for_template (attrs, n_attrs, session),
		                       NULL);
		gkm_sexp_unref (sexp);
		gkm_session_complete_object_creation (session, transaction, object,
		                                      TRUE, attrs, n_attrs);
	}

	return object;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gkm_user_public_key_init (GkmUserPublicKey *self)
{

}

static void
gkm_user_public_key_finalize (GObject *obj)
{
	/* GkmUserPublicKey *self = GKM_USER_PUBLIC_KEY (obj); */
	G_OBJECT_CLASS (gkm_user_public_key_parent_class)->finalize (obj);
}

static void
gkm_user_public_key_set_property (GObject *obj, guint prop_id, const GValue *value,
                                  GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_user_public_key_get_property (GObject *obj, guint prop_id, GValue *value,
                                  GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_user_public_key_class_init (GkmUserPublicKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->finalize = gkm_user_public_key_finalize;
	gobject_class->set_property = gkm_user_public_key_set_property;
	gobject_class->get_property = gkm_user_public_key_get_property;
}


static gboolean
gkm_user_public_key_real_load (GkmSerializable *base, GkmSecret *login, const guchar *data, gsize n_data)
{
	GkmUserPublicKey *self = GKM_USER_PUBLIC_KEY (base);
	GkmDataResult res;
	GkmSexp *wrapper;
	gcry_sexp_t sexp;

	g_return_val_if_fail (GKM_IS_USER_PUBLIC_KEY (self), FALSE);
	g_return_val_if_fail (data, FALSE);

	res = gkm_data_der_read_public_key (data, n_data, &sexp);

	switch (res) {
	case GKM_DATA_LOCKED:
		g_message ("public key is locked");
		return FALSE;
	case GKM_DATA_FAILURE:
		g_message ("couldn't parse public key");
		return FALSE;
	case GKM_DATA_UNRECOGNIZED:
		g_message ("invalid or unrecognized public key");
		return FALSE;
	case GKM_DATA_SUCCESS:
		break;
	default:
		g_assert_not_reached();
	}

	wrapper = gkm_sexp_new (sexp);
	gkm_sexp_key_set_base (GKM_SEXP_KEY (self), wrapper);
	gkm_sexp_unref (wrapper);

	return TRUE;
}

static gboolean
gkm_user_public_key_real_save (GkmSerializable *base, GkmSecret *login, guchar **data, gsize *n_data)
{
	GkmUserPublicKey *self = GKM_USER_PUBLIC_KEY (base);
	GkmSexp *wrapper;

	g_return_val_if_fail (GKM_IS_USER_PUBLIC_KEY (self), FALSE);
	g_return_val_if_fail (data, FALSE);
	g_return_val_if_fail (n_data, FALSE);

	wrapper = gkm_sexp_key_get_base (GKM_SEXP_KEY (self));
	g_return_val_if_fail (wrapper, FALSE);

	*data = gkm_data_der_write_public_key (gkm_sexp_get (wrapper), n_data);
	return *data != NULL;
}

static void
gkm_user_public_key_serializable (GkmSerializableIface *iface)
{
	iface->extension = ".pub";
	iface->load = gkm_user_public_key_real_load;
	iface->save = gkm_user_public_key_real_save;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmFactory*
gkm_user_public_key_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_PUBLIC_KEY;
	static CK_BBOOL token = CK_TRUE;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &token, sizeof (token) },
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_public_key
	};

	return &factory;
}
