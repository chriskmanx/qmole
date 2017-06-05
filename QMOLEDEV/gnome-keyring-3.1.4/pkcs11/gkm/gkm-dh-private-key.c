/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Private License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Private License for more details.
 *
 * You should have received a copy of the GNU Lesser General Private
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "pkcs11/pkcs11.h"

#include "gkm-attributes.h"
#include "gkm-crypto.h"
#include "gkm-factory.h"
#include "gkm-dh-private-key.h"
#include "gkm-session.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

struct _GkmDhPrivateKey {
	GkmDhKey parent;
	gcry_mpi_t value;
};

G_DEFINE_TYPE (GkmDhPrivateKey, gkm_dh_private_key, GKM_TYPE_DH_KEY);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GkmObject*
factory_create_dh_private_key (GkmSession *session, GkmTransaction *transaction,
                               CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmManager *manager;
	gcry_mpi_t prime = NULL;
	gcry_mpi_t base = NULL;
	gcry_mpi_t value = NULL;
	CK_ATTRIBUTE_PTR idattr;
	GkmObject *object;

	if (!gkm_attributes_find_mpi (attrs, n_attrs, CKA_PRIME, &prime) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_BASE, &base) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_VALUE, &value)) {
		gcry_mpi_release (prime);
		gcry_mpi_release (base);
		gcry_mpi_release (value);
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	manager = gkm_manager_for_template (attrs, n_attrs, session);
	idattr = gkm_attributes_find (attrs, n_attrs, CKA_ID);

	object = GKM_OBJECT (gkm_dh_private_key_new (gkm_session_get_module (session),
	                                            manager, prime, base, value,
	                                            idattr ? g_memdup (idattr->pValue, idattr->ulValueLen) : NULL,
	                                            idattr ? idattr->ulValueLen : 0));
	gkm_attributes_consume (attrs, n_attrs, CKA_PRIME, CKA_BASE, CKA_VALUE, G_MAXULONG);

	gkm_session_complete_object_creation (session, transaction, object,
	                                      TRUE, attrs, n_attrs);
	return object;
}

/* -----------------------------------------------------------------------------
 * DH_PRIVATE_KEY
 */

static CK_RV
gkm_dh_private_key_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE* attr)
{
	GkmDhPrivateKey *self = GKM_DH_PRIVATE_KEY (base);

	switch (attr->type)
	{

	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_PRIVATE_KEY);

	case CKA_PRIVATE:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_SENSITIVE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_DECRYPT:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_SIGN:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_SIGN_RECOVER:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_DERIVE:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_UNWRAP:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_EXTRACTABLE:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_ALWAYS_SENSITIVE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_NEVER_EXTRACTABLE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_WRAP_WITH_TRUSTED:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_UNWRAP_TEMPLATE:
		return CKR_ATTRIBUTE_TYPE_INVALID;

	case CKA_ALWAYS_AUTHENTICATE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_VALUE:
		return gkm_attribute_set_mpi (attr, self->value);

	case CKA_VALUE_BITS:
		return gkm_attribute_set_ulong (attr, gcry_mpi_get_nbits (self->value));
	};

	return GKM_OBJECT_CLASS (gkm_dh_private_key_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_dh_private_key_init (GkmDhPrivateKey *self)
{

}

static void
gkm_dh_private_key_finalize (GObject *obj)
{
	GkmDhPrivateKey *self = GKM_DH_PRIVATE_KEY (obj);

	gcry_mpi_release (self->value);
	self->value = NULL;

	G_OBJECT_CLASS (gkm_dh_private_key_parent_class)->finalize (obj);
}

static void
gkm_dh_private_key_class_init (GkmDhPrivateKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gkm_dh_private_key_parent_class = g_type_class_peek_parent (klass);

	gobject_class->finalize = gkm_dh_private_key_finalize;

	gkm_class->get_attribute = gkm_dh_private_key_real_get_attribute;
}

/* -----------------------------------------------------------------------------
 * PRIVATE
 */

GkmFactory*
gkm_dh_private_key_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_PRIVATE_KEY;
	static CK_KEY_TYPE type = CKK_DH;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_KEY_TYPE, &type, sizeof (type) }
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_dh_private_key
	};

	return &factory;
}

GkmDhPrivateKey*
gkm_dh_private_key_new (GkmModule *module, GkmManager *manager,
                        gcry_mpi_t prime, gcry_mpi_t base, gcry_mpi_t value,
                        gpointer id, gsize n_id)
{
	GkmDhPrivateKey *key;

	key = g_object_new (GKM_TYPE_DH_PRIVATE_KEY,
	                    "manager", manager,
	                    "module", module,
	                    NULL);

	gkm_dh_key_initialize (GKM_DH_KEY (key), prime, base, id, n_id);
	key->value = value;
	return key;
}

gcry_mpi_t
gkm_dh_private_key_get_value (GkmDhPrivateKey *self)
{
	g_return_val_if_fail (GKM_IS_DH_PRIVATE_KEY (self), NULL);
	return self->value;
}
