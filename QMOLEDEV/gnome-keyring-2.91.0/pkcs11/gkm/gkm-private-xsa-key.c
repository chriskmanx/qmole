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
 * You should have received a copy of the GNU Lesser General Private
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "pkcs11/pkcs11.h"

#include "gkm-attributes.h"
#include "gkm-credential.h"
#include "gkm-factory.h"
#include "gkm-private-xsa-key.h"
#include "gkm-session.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

struct _GkmPrivateXsaKeyPrivate {
	GkmSexp *sexp;
};

G_DEFINE_TYPE (GkmPrivateXsaKey, gkm_private_xsa_key, GKM_TYPE_SEXP_KEY);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */


static CK_RV
create_rsa_private (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, gcry_sexp_t *skey)
{
	gcry_error_t gcry;
	gcry_mpi_t n = NULL;
	gcry_mpi_t e = NULL;
	gcry_mpi_t d = NULL;
	gcry_mpi_t p = NULL;
	gcry_mpi_t q = NULL;
	gcry_mpi_t u = NULL;
	CK_RV ret;

	if (!gkm_attributes_find_mpi (attrs, n_attrs, CKA_MODULUS, &n) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_PUBLIC_EXPONENT, &e) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_PRIVATE_EXPONENT, &d) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_PRIME_1, &p) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_PRIME_2, &q)) {
		ret = CKR_TEMPLATE_INCOMPLETE;
		goto done;
	}

	/* Fix up the incoming key so gcrypt likes it */
	if (gcry_mpi_cmp (p, q) > 0)
		gcry_mpi_swap (p, q);

	/* Compute U.  */
	u = gcry_mpi_snew (gcry_mpi_get_nbits (n));
	gcry_mpi_invm (u, p, q);

	gcry = gcry_sexp_build (skey, NULL,
	                        "(private-key (rsa (n %m) (e %m) (d %m) (p %m) (q %m) (u %m)))",
	                        n, e, d, p, q, u);

	if (gcry != 0) {
		g_message ("couldn't create RSA key from passed attributes: %s", gcry_strerror (gcry));
		ret = CKR_FUNCTION_FAILED;
		goto done;
	}

	gkm_attributes_consume (attrs, n_attrs, CKA_MODULUS, CKA_PUBLIC_EXPONENT,
	                        CKA_PRIVATE_EXPONENT, CKA_PRIME_1, CKA_PRIME_2,
	                        CKA_EXPONENT_1, CKA_EXPONENT_2, CKA_COEFFICIENT, G_MAXULONG);
	ret = CKR_OK;

done:
	gcry_mpi_release (n);
	gcry_mpi_release (e);
	gcry_mpi_release (d);
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (u);
	return ret;
}

static CK_RV
create_dsa_private (CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, gcry_sexp_t *skey)
{
	gcry_error_t gcry;
	gcry_mpi_t p = NULL;
	gcry_mpi_t q = NULL;
	gcry_mpi_t g = NULL;
	gcry_mpi_t y = NULL;
	gcry_mpi_t value = NULL;
	CK_RV ret;

	if (!gkm_attributes_find_mpi (attrs, n_attrs, CKA_PRIME, &p) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_SUBPRIME, &q) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_BASE, &g) ||
	    !gkm_attributes_find_mpi (attrs, n_attrs, CKA_VALUE, &value)) {
		ret = CKR_TEMPLATE_INCOMPLETE;
		goto done;
	}

	/* Calculate the public part from the private */
	y = gcry_mpi_snew (gcry_mpi_get_nbits (value));
	g_return_val_if_fail (y, CKR_GENERAL_ERROR);
	gcry_mpi_powm (y, g, value, p);

	gcry = gcry_sexp_build (skey, NULL,
	                        "(private-key (dsa (p %m) (q %m) (g %m) (y %m) (x %m)))",
	                        p, q, g, y, value);

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
	gcry_mpi_release (value);
	return ret;
}

static GkmObject*
factory_create_private_xsa_key (GkmSession *session, GkmTransaction *transaction,
                            CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmPrivateXsaKey *key;
	GkmSexp *sexp;

	g_return_val_if_fail (GKM_IS_TRANSACTION (transaction), NULL);
	g_return_val_if_fail (attrs || !n_attrs, NULL);

	sexp = gkm_private_xsa_key_create_sexp (session, transaction, attrs, n_attrs);
	if (sexp == NULL)
		return NULL;

	key = g_object_new (GKM_TYPE_PRIVATE_XSA_KEY, "base-sexp", sexp,
	                    "module", gkm_session_get_module (session),
	                    "manager", gkm_manager_for_template (attrs, n_attrs, session),
	                    NULL);
	key->pv->sexp = sexp;

	gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (key),
	                                      TRUE, attrs, n_attrs);
	return GKM_OBJECT (key);
}

static gboolean
acquire_from_credential (GkmCredential *cred, GkmObject *object, gpointer user_data)
{
	GkmSexp **result = user_data;

	g_assert (result);
	g_assert (!*result);

	/* The sexp we stored on the credential */
	*result = gkm_credential_pop_data (cred, GKM_BOXED_SEXP);
	if (*result != NULL)
		return TRUE;

	return FALSE;
}

static gboolean
have_from_credential (GkmCredential *cred, GkmObject *object, gpointer unused)
{
	/* The sexp we stored on the credential */
	return gkm_credential_peek_data (cred, GKM_BOXED_SEXP) ? TRUE : FALSE;
}

/* -----------------------------------------------------------------------------
 * PRIVATE_XSA_KEY
 */

static CK_RV
gkm_private_xsa_key_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE* attr)
{
	GkmPrivateXsaKey *self = GKM_PRIVATE_XSA_KEY (base);
	gboolean have;
	gint algorithm;

	switch (attr->type) {
	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_PRIVATE_KEY);

	case CKA_PRIVATE:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_SENSITIVE:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_DECRYPT:
		algorithm = gkm_sexp_key_get_algorithm (GKM_SEXP_KEY (self));
		return gkm_attribute_set_bool (attr, algorithm == GCRY_PK_RSA);

	case CKA_SIGN:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_SIGN_RECOVER:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_UNWRAP:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_EXTRACTABLE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_ALWAYS_SENSITIVE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_NEVER_EXTRACTABLE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_WRAP_WITH_TRUSTED:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_UNWRAP_TEMPLATE:
		return CKR_ATTRIBUTE_TYPE_INVALID;

	case CKA_ALWAYS_AUTHENTICATE:
		have = self->pv->sexp ? TRUE : FALSE;
		if (!have && session)
			have = gkm_credential_for_each (session, base, have_from_credential, NULL);
		return gkm_attribute_set_bool (attr, !have);

	case CKA_MODULUS:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_RSA, "n", attr);

	case CKA_PUBLIC_EXPONENT:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_RSA, "e", attr);

	/* RSA private key parts */
	case CKA_PRIVATE_EXPONENT:
	case CKA_PRIME_1:
	case CKA_PRIME_2:
	case CKA_EXPONENT_1:
	case CKA_EXPONENT_2:
	case CKA_COEFFICIENT:
		return CKR_ATTRIBUTE_SENSITIVE;

	case CKA_PRIME:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_DSA, "p", attr);

	case CKA_SUBPRIME:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_DSA, "q", attr);

	case CKA_BASE:
		return gkm_sexp_key_set_part (GKM_SEXP_KEY (self), GCRY_PK_DSA, "g", attr);

	/* DSA private parts */
	case CKA_VALUE:
		return CKR_ATTRIBUTE_SENSITIVE;
	};

	return GKM_OBJECT_CLASS (gkm_private_xsa_key_parent_class)->get_attribute (base, session, attr);
}

static GkmSexp*
gkm_private_xsa_key_real_acquire_crypto_sexp (GkmSexpKey *base, GkmSession *session)
{
	GkmPrivateXsaKey *self = GKM_PRIVATE_XSA_KEY (base);
	GkmSexp *sexp = NULL;

	/* We have an unlocked private key here */
	if (self->pv->sexp)
		sexp = gkm_sexp_ref (self->pv->sexp);

	/* Find an credential, with an unlocked copy */
	else
		gkm_credential_for_each (session, GKM_OBJECT (self),
		                         acquire_from_credential, &sexp);

	return sexp;
}

static void
gkm_private_xsa_key_init (GkmPrivateXsaKey *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_PRIVATE_XSA_KEY, GkmPrivateXsaKeyPrivate);
}

static void
gkm_private_xsa_key_dispose (GObject *obj)
{
	GkmPrivateXsaKey *self = GKM_PRIVATE_XSA_KEY (obj);

	if (self->pv->sexp)
		gkm_sexp_unref (self->pv->sexp);
	self->pv->sexp = NULL;

	G_OBJECT_CLASS (gkm_private_xsa_key_parent_class)->dispose (obj);
}

static void
gkm_private_xsa_key_finalize (GObject *obj)
{
	GkmPrivateXsaKey *self = GKM_PRIVATE_XSA_KEY (obj);

	g_assert (self->pv->sexp == NULL);

	G_OBJECT_CLASS (gkm_private_xsa_key_parent_class)->finalize (obj);
}

static void
gkm_private_xsa_key_class_init (GkmPrivateXsaKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	GkmSexpKeyClass *key_class = GKM_SEXP_KEY_CLASS (klass);

	gkm_private_xsa_key_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GkmPrivateXsaKeyPrivate));

	gobject_class->dispose = gkm_private_xsa_key_dispose;
	gobject_class->finalize = gkm_private_xsa_key_finalize;

	gkm_class->get_attribute = gkm_private_xsa_key_real_get_attribute;

	key_class->acquire_crypto_sexp = gkm_private_xsa_key_real_acquire_crypto_sexp;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

void
gkm_private_xsa_key_set_unlocked_private (GkmPrivateXsaKey *self, GkmSexp *sexp)
{
	g_return_if_fail (GKM_IS_PRIVATE_XSA_KEY (self));
	g_return_if_fail (sexp);

	if (sexp)
		gkm_sexp_ref (sexp);
	if (self->pv->sexp)
		gkm_sexp_unref (self->pv->sexp);
	self->pv->sexp = sexp;
}

void
gkm_private_xsa_key_set_locked_private (GkmPrivateXsaKey *self, GkmCredential *cred,
                                        GkmSexp *sexp)
{
	g_return_if_fail (GKM_IS_PRIVATE_XSA_KEY (self));
	g_return_if_fail (GKM_IS_CREDENTIAL (cred));
	g_return_if_fail (gkm_credential_get_object (cred) == GKM_OBJECT (self));
	gkm_credential_set_data (cred, GKM_BOXED_SEXP, sexp);
}

GkmSexp*
gkm_private_xsa_key_create_sexp (GkmSession *session, GkmTransaction *transaction,
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
		ret = create_rsa_private (attrs, n_attrs, &sexp);
		break;
	case CKK_DSA:
		ret = create_dsa_private (attrs, n_attrs, &sexp);
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
gkm_private_xsa_key_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_PRIVATE_KEY;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) }
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_private_xsa_key
	};

	return &factory;
}
