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

#include "gkm-attributes.h"
#include "gkm-factory.h"
#include "gkm-public-xsa-key.h"
#include "gkm-session.h"
#include "gkm-sexp.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

G_DEFINE_TYPE (GkmPublicXsaKey, gkm_public_xsa_key, GKM_TYPE_SEXP_KEY);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static CK_RV
return_modulus_bits (GkmPublicXsaKey *self, CK_ATTRIBUTE_PTR attr)
{
	gcry_sexp_t numbers;
	gcry_mpi_t mpi;
	int algorithm;
	CK_RV rv;

	if (!gkm_sexp_parse_key (gkm_sexp_get (gkm_sexp_key_get_base (GKM_SEXP_KEY (self))),
	                         &algorithm, NULL, &numbers))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	if (algorithm != GCRY_PK_RSA) {
		gcry_sexp_release (numbers);
		return CKR_ATTRIBUTE_TYPE_INVALID;
	}

	g_assert (numbers);
	if (!gkm_sexp_extract_mpi (numbers, &mpi, "n", NULL))
		g_return_val_if_reached (CKR_GENERAL_ERROR);

	gcry_sexp_release (numbers);
	rv = gkm_attribute_set_ulong (attr, gcry_mpi_get_nbits (mpi));
	gcry_mpi_release (mpi);

	return rv;
}

static CK_RV
create_rsa_public (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, gcry_sexp_t *skey)
{
	gcry_error_t gcry;
	gcry_mpi_t n = NULL;
	gcry_mpi_t e = NULL;
	CK_RV ret;

	if (!gkm_attributes_find_mpi (attrs, n_attrs, CKA_MODULUS, &n) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_PUBLIC_EXPONENT, &e)) {
		ret = CKR_TEMPLATE_INCOMPLETE;
		goto done;
	}

	gcry = gcry_sexp_build (skey, NULL,
	                        "(public-key (rsa (n %m) (e %m)))",
	                        n, e);

	if (gcry != 0) {
		g_message ("couldn't create RSA key from passed attributes: %s", gcry_strerror (gcry));
		ret = CKR_FUNCTION_FAILED;
		goto done;
	}

	gkm_attributes_consume (attrs, n_attrs, CKA_MODULUS, CKA_PUBLIC_EXPONENT, CKA_MODULUS_BITS, G_MAXULONG);
	ret = CKR_OK;

done:
	gcry_mpi_release (n);
	gcry_mpi_release (e);
	return ret;
}

static CK_RV
create_dsa_public (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, gcry_sexp_t *skey)
{
	gcry_error_t gcry;
	gcry_mpi_t p = NULL;
	gcry_mpi_t q = NULL;
	gcry_mpi_t g = NULL;
	gcry_mpi_t y = NULL;
	CK_RV ret;

	if (!gkm_attributes_find_mpi (attrs, n_attrs, CKA_PRIME, &p) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_SUBPRIME, &q) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_BASE, &g) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_VALUE, &y)) {
		ret = CKR_TEMPLATE_INCOMPLETE;
		goto done;
	}

	gcry = gcry_sexp_build (skey, NULL,
	                        "(public-key (dsa (p %m) (q %m) (g %m) (y %m)))",
	                        p, q, g, y);

	if (gcry != 0) {
		g_message ("couldn't create DSA key from passed attributes: %s", gcry_strerror (gcry));
		ret = CKR_FUNCTION_FAILED;
		goto done;
	}

	gkm_attributes_consume (attrs, n_attrs, CKA_PRIME, CKA_SUBPRIME,
	                        CKA_BASE, CKA_VALUE, G_MAXULONG);
	ret = CKR_OK;

done:
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);
	return ret;
}

static GkmObject*
factory_create_public_xsa_key (GkmSession *session, GkmTransaction *transaction,
                               CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmObject *object = NULL;
	GkmSexp *sexp;

	g_return_val_if_fail (GKM_IS_TRANSACTION (transaction), NULL);
	g_return_val_if_fail (attrs || !n_attrs, NULL);

	sexp = gkm_public_xsa_key_create_sexp (session, transaction, attrs, n_attrs);
	if (sexp != NULL) {
		object = g_object_new (GKM_TYPE_PUBLIC_XSA_KEY, "base-sexp", sexp,
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
 * PUBLIC_XSA_KEY
 */

static CK_RV
gkm_public_xsa_key_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE* attr)
{
	GkmPublicXsaKey *self = GKM_PUBLIC_XSA_KEY (base);
	gint algorithm;

	switch (attr->type)
	{

	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_PUBLIC_KEY);

	case CKA_ENCRYPT:
		algorithm = gkm_sexp_key_get_algorithm (GKM_SEXP_KEY (self));
		return gkm_attribute_set_bool (attr, algorithm == GCRY_PK_RSA);

	case CKA_VERIFY:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_VERIFY_RECOVER:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_WRAP:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_TRUSTED:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_WRAP_TEMPLATE:
		return CKR_ATTRIBUTE_TYPE_INVALID;

	case CKA_MODULUS_BITS:
		return return_modulus_bits (self, attr);

	case CKA_MODULUS:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_RSA, "n", attr);

	case CKA_PUBLIC_EXPONENT:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_RSA, "e", attr);

	case CKA_PRIME:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_DSA, "p", attr);

	case CKA_SUBPRIME:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_DSA, "q", attr);

	case CKA_BASE:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_DSA, "g", attr);

	/* DSA public value */
	case CKA_VALUE:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_DSA, "y", attr);
	};

	return GKM_OBJECT_CLASS (gkm_public_xsa_key_parent_class)->get_attribute (base, session, attr);
}

static GkmSexp*
gkm_public_xsa_key_acquire_crypto_sexp (GkmSexpKey *self, GkmSession *session)
{
	GkmSexp* sexp;

	sexp = gkm_sexp_key_get_base (self);
	if (sexp != NULL)
		gkm_sexp_ref (sexp);

	return sexp;
}

static void
gkm_public_xsa_key_init (GkmPublicXsaKey *self)
{

}

static void
gkm_public_xsa_key_class_init (GkmPublicXsaKeyClass *klass)
{
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	GkmSexpKeyClass *key_class = GKM_SEXP_KEY_CLASS (klass);

	gkm_public_xsa_key_parent_class = g_type_class_peek_parent (klass);

	gkm_class->get_attribute = gkm_public_xsa_key_real_get_attribute;

	key_class->acquire_crypto_sexp = gkm_public_xsa_key_acquire_crypto_sexp;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmSexp*
gkm_public_xsa_key_create_sexp (GkmSession *session, GkmTransaction *transaction,
                                CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	CK_KEY_TYPE type;
	gcry_sexp_t sexp;
	CK_RV ret;

	g_return_val_if_fail (GKM_IS_TRANSACTION (transaction), NULL);
	g_return_val_if_fail (attrs || !n_attrs, NULL);

	if (!gkm_attributes_find_ulong (attrs, n_attrs, CKA_KEY_TYPE, &type)) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	gkm_attributes_consume (attrs, n_attrs, CKA_KEY_TYPE, CKA_CLASS, G_MAXULONG);

	switch (type) {
	case CKK_RSA:
		ret = create_rsa_public (attrs, n_attrs, &sexp);
		break;
	case CKK_DSA:
		ret = create_dsa_public (attrs, n_attrs, &sexp);
		break;
	default:
		ret = CKR_ATTRIBUTE_VALUE_INVALID;
		break;
	};

	if (ret != CKR_OK) {
		gkm_transaction_fail (transaction, ret);
		return NULL;
	}

	g_return_val_if_fail (sexp, NULL);
	return gkm_sexp_new (sexp);
}

GkmFactory*
gkm_public_xsa_key_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_PUBLIC_KEY;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) }
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_public_xsa_key
	};

	return &factory;
}
