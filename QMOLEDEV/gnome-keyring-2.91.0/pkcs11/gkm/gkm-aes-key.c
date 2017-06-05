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

#include "gkm-aes-mechanism.h"
#include "gkm-attributes.h"
#include "gkm-crypto.h"
#include "gkm-aes-key.h"
#include "gkm-session.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

#include "egg/egg-secure-memory.h"

struct _GkmAesKey {
	GkmSecretKey parent;
	gpointer value;
	gsize n_value;
};

G_DEFINE_TYPE (GkmAesKey, gkm_aes_key, GKM_TYPE_SECRET_KEY);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static int
algorithm_for_length (gsize length)
{
	switch (length) {
	case 16:
		return GCRY_CIPHER_AES128;
	case 24:
		return GCRY_CIPHER_AES192;
	case 32:
		return GCRY_CIPHER_AES256;
	default:
		return 0;
	}
}

static CK_RV
attribute_set_check_value (GkmAesKey *self, CK_ATTRIBUTE *attr)
{
	gcry_cipher_hd_t cih;
	gcry_error_t gcry;
	gpointer data;
	CK_RV rv;

	g_assert (GKM_IS_AES_KEY (self));
	g_assert (attr);

	/* Just asking for the length */
	if (!attr->pValue) {
		attr->ulValueLen = 3;
		return CKR_OK;
	}

	cih = gkm_aes_key_get_cipher (self, GCRY_CIPHER_MODE_ECB);
	if (cih == NULL)
		return CKR_FUNCTION_FAILED;

	/* Buffer of zeros */
	data = g_malloc0 (self->n_value);

	/* Encrypt it */
	gcry = gcry_cipher_encrypt (cih, data, self->n_value, NULL, 0);
	g_return_val_if_fail (gcry == 0, CKR_GENERAL_ERROR);

	/* Use the first three bytes */
	g_assert (self->n_value > 3);
	rv = gkm_attribute_set_data (attr, data, 3);

	gcry_cipher_close (cih);
	g_free (data);

	return rv;
}

static GkmObject*
factory_create_aes_key (GkmSession *session, GkmTransaction *transaction,
                        CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmAesKey *key;
	GkmManager *manager;
	CK_ATTRIBUTE_PTR value;

	value = gkm_attributes_find (attrs, n_attrs, CKA_VALUE);
	if (value == NULL) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	if (algorithm_for_length (value->ulValueLen) == 0) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
		return NULL;
	}

	manager = gkm_manager_for_template (attrs, n_attrs, session);
	key = g_object_new (GKM_TYPE_AES_KEY,
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
gkm_aes_key_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE *attr)
{
	GkmAesKey *self = GKM_AES_KEY (base);

	switch (attr->type)
	{
	case CKA_KEY_TYPE:
		return gkm_attribute_set_ulong (attr, CKK_AES);

	case CKA_UNWRAP:
	case CKA_WRAP:
		return gkm_attribute_set_bool (attr, CK_TRUE);

	case CKA_VALUE:
		return gkm_attribute_set_data (attr, self->value, self->n_value);

	case CKA_VALUE_LEN:
		return gkm_attribute_set_ulong (attr, self->n_value);

	case CKA_CHECK_VALUE:
		return attribute_set_check_value (self, attr);

	case CKA_ALLOWED_MECHANISMS:
		return gkm_attribute_set_data (attr, (CK_VOID_PTR)GKM_AES_MECHANISMS,
		                               sizeof (GKM_AES_MECHANISMS));
	};

	return GKM_OBJECT_CLASS (gkm_aes_key_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_aes_key_init (GkmAesKey *self)
{

}

static void
gkm_aes_key_finalize (GObject *obj)
{
	GkmAesKey *self = GKM_AES_KEY (obj);

	if (self->value) {
		egg_secure_clear (self->value, self->n_value);
		egg_secure_free (self->value);
		self->value = NULL;
		self->n_value = 0;
	}

	G_OBJECT_CLASS (gkm_aes_key_parent_class)->finalize (obj);
}

static void
gkm_aes_key_class_init (GkmAesKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gkm_aes_key_parent_class = g_type_class_peek_parent (klass);

	gobject_class->finalize = gkm_aes_key_finalize;

	gkm_class->get_attribute = gkm_aes_key_real_get_attribute;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmFactory*
gkm_aes_key_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_SECRET_KEY;
	static CK_KEY_TYPE type = CKK_AES;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_KEY_TYPE, &type, sizeof (type) }
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_aes_key
	};

	return &factory;
}

gsize
gkm_aes_key_get_block_size (GkmAesKey *self)
{
	int algorithm;

	g_return_val_if_fail (GKM_IS_AES_KEY (self), 0);

	algorithm = algorithm_for_length (self->n_value);
	g_return_val_if_fail (algorithm != 0, 0);

	return self->n_value;
}

gcry_cipher_hd_t
gkm_aes_key_get_cipher (GkmAesKey *self, int mode)
{
	gcry_cipher_hd_t cih;
	gcry_error_t gcry;
	int algorithm;

	g_return_val_if_fail (GKM_IS_AES_KEY (self), NULL);

	algorithm = algorithm_for_length (self->n_value);
	g_return_val_if_fail (algorithm != 0, NULL);

	gcry = gcry_cipher_open (&cih, algorithm, mode, 0);
	if (gcry != 0) {
		g_warning ("couldn't open %s cipher: %s",
		           gcry_cipher_algo_name (algorithm), gcry_strerror (gcry));
		return NULL;
	}

	/* Setup the key */
	gcry = gcry_cipher_setkey (cih, self->value, self->n_value);
	g_return_val_if_fail (gcry == 0, NULL);

	return cih;
}
