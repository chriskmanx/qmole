/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11i.h"

#include "gkm-attributes.h"
#include "gkm-generic-key.h"
#include "gkm-session.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

#include "egg/egg-secure-memory.h"

struct _GkmGenericKey {
	GkmSecretKey parent;
	gpointer value;
	gsize n_value;
};

G_DEFINE_TYPE (GkmGenericKey, gkm_generic_key, GKM_TYPE_SECRET_KEY);

static const CK_MECHANISM_TYPE GKM_GENERIC_MECHANISMS[] = {
	CKM_G_HKDF_SHA256_DERIVE
};

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static CK_RV
attribute_set_check_value (GkmGenericKey *self, CK_ATTRIBUTE *attr)
{
	guchar buffer[20];

	g_assert (GKM_IS_GENERIC_KEY (self));
	g_assert (attr);

	/* Just asking for the length */
	if (!attr->pValue) {
		attr->ulValueLen = 3;
		return CKR_OK;
	}

	/* Just the a sha1 of the value */
	gcry_md_hash_buffer (GCRY_MD_SHA1, buffer, self->value, self->n_value);

	/* Use the first three bytes */
	return gkm_attribute_set_data (attr, buffer, 3);
}

static GkmObject*
factory_create_generic_key (GkmSession *session, GkmTransaction *transaction,
                            CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmGenericKey *key;
	GkmManager *manager;
	CK_ATTRIBUTE_PTR value;

	value = gkm_attributes_find (attrs, n_attrs, CKA_VALUE);
	if (value == NULL) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	if (gkm_attributes_find (attrs, n_attrs, CKA_VALUE_LEN)) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
		return NULL;
	}

	manager = gkm_manager_for_template (attrs, n_attrs, session);
	key = g_object_new (GKM_TYPE_GENERIC_KEY,
	                    "module", gkm_session_get_module (session),
	                    "manager", manager,
	                    NULL);

	key->value = egg_secure_alloc (value->ulValueLen);
	key->n_value = value->ulValueLen;
	memcpy (key->value, value->pValue, key->n_value);

	gkm_attribute_consume (value);

	gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (key),
	                                      TRUE, attrs, n_attrs);
	return GKM_OBJECT (key);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_generic_key_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE *attr)
{
	GkmGenericKey *self = GKM_GENERIC_KEY (base);

	switch (attr->type)
	{
	case CKA_KEY_TYPE:
		return gkm_attribute_set_ulong (attr, CKK_GENERIC_SECRET);

	case CKA_DERIVE:
		return gkm_attribute_set_bool (attr, CK_TRUE);

	case CKA_UNWRAP:
	case CKA_WRAP:
		return gkm_attribute_set_bool (attr, CK_FALSE);

	case CKA_VALUE:
		return gkm_attribute_set_data (attr, self->value, self->n_value);

	case CKA_VALUE_LEN:
		return gkm_attribute_set_ulong (attr, self->n_value);

	case CKA_CHECK_VALUE:
		return attribute_set_check_value (self, attr);

	case CKA_ALLOWED_MECHANISMS:
		return gkm_attribute_set_data (attr, (CK_VOID_PTR)GKM_GENERIC_MECHANISMS,
		                               sizeof (GKM_GENERIC_MECHANISMS));
	};

	return GKM_OBJECT_CLASS (gkm_generic_key_parent_class)->get_attribute (base, session, attr);
}

static gconstpointer
gkm_generic_key_get_key_value (GkmSecretKey *key, gsize *n_value)
{
	GkmGenericKey *self = GKM_GENERIC_KEY (key);
	*n_value = self->n_value;
	return self->value;
}

static void
gkm_generic_key_init (GkmGenericKey *self)
{

}

static void
gkm_generic_key_finalize (GObject *obj)
{
	GkmGenericKey *self = GKM_GENERIC_KEY (obj);

	if (self->value) {
		egg_secure_clear (self->value, self->n_value);
		egg_secure_free (self->value);
		self->value = NULL;
		self->n_value = 0;
	}

	G_OBJECT_CLASS (gkm_generic_key_parent_class)->finalize (obj);
}

static void
gkm_generic_key_class_init (GkmGenericKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	GkmSecretKeyClass *key_class = GKM_SECRET_KEY_CLASS (klass);

	gkm_generic_key_parent_class = g_type_class_peek_parent (klass);

	gobject_class->finalize = gkm_generic_key_finalize;

	gkm_class->get_attribute = gkm_generic_key_get_attribute;

	key_class->get_key_value = gkm_generic_key_get_key_value;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmFactory*
gkm_generic_key_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_SECRET_KEY;
	static CK_KEY_TYPE type = CKK_GENERIC_SECRET;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_KEY_TYPE, &type, sizeof (type) }
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_generic_key
	};

	return &factory;
}
