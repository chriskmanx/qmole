/*
 * gnome-trustring
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

#include "gkm-attributes.h"
#include "gkm-certificate.h"
#include "gkm-certificate-trust.h"
#include "gkm-data-der.h"

#include "gkm-object.h"
#include "gkm-util.h"

#include "pkcs11/pkcs11g.h"
#include "pkcs11/pkcs11n.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_CERTIFICATE
};

struct _GkmCertificateTrustPrivate {
	GkmCertificate *certificate;
};

G_DEFINE_TYPE (GkmCertificateTrust, gkm_certificate_trust, GKM_TYPE_OBJECT);

#define PKIX_KEY_USAGE_DIGITAL_SIGNATURE 0x80
#define PKIX_KEY_USAGE_NON_REPUDIATION 0x40
#define PKIX_KEY_USAGE_KEY_ENCIPHERMENT 0x20
#define PKIX_KEY_USAGE_DATA_ENCIPHERMENT 0x10
#define PKIX_KEY_USAGE_KEY_AGREEMENT 0x08
#define PKIX_KEY_USAGE_KEY_CERT_SIGN 0x04
#define PKIX_KEY_USAGE_CRL_SIGN 0x02
#define PKIX_KEY_USAGE_ENCIPHER_ONLY 0x01

static GQuark OID_KEY_USAGE;

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
init_quarks (void)
{
	static volatile gsize quarks_inited = 0;

	if (g_once_init_enter (&quarks_inited)) {
		#define QUARK(name, value) \
			name = g_quark_from_static_string(value)

		QUARK (OID_KEY_USAGE, "2.5.29.15");

		#undef QUARK

		g_once_init_leave (&quarks_inited, 1);
	}
}

static CK_RV
has_key_usage (GkmCertificateTrust *self, guint check, CK_ULONG *val)
{
	GkmDataResult res;
	const guchar *extension;
	gsize n_extension;
	gulong usage;

	g_return_val_if_fail (self->pv->certificate, CKR_GENERAL_ERROR);
	*val = CKT_NETSCAPE_TRUST_UNKNOWN;

	/* Find out the key usage */
	extension = gkm_certificate_get_extension (self->pv->certificate, OID_KEY_USAGE,
	                                           &n_extension, NULL);
	if (!extension)
		return CKR_OK;

	res = gkm_data_der_read_key_usage (extension, n_extension, &usage);

	if (res != GKM_DATA_SUCCESS) {
		g_warning ("invalid key usage in certificate");
		return CKR_GENERAL_ERROR;
	}

	if ((usage & check) == check)
		*val = CKT_NETSCAPE_TRUSTED;
	else
		*val = CKT_NETSCAPE_UNTRUSTED;

	return CKR_OK;
}

static CK_RV
read_key_usage (GkmCertificateTrust *self, guint check, CK_ATTRIBUTE_PTR attr)
{
	CK_ULONG value;
	CK_RV rv;

	g_assert (GKM_IS_CERTIFICATE_TRUST (self));

	rv = has_key_usage (self, check, &value);
	if (rv == CKR_OK)
		rv = gkm_attribute_set_ulong (attr, value);
	return rv;
}

static CK_RV
has_enhanced_usage (GkmCertificateTrust *self, CK_ATTRIBUTE_TYPE type, CK_ULONG *val)
{
	gboolean bval;
	CK_ULONG nval;

	g_return_val_if_fail (self->pv->certificate, CKR_GENERAL_ERROR);

	/* Check if we have the purpose setup */
	if (!gkm_object_get_attribute_boolean (GKM_OBJECT (self->pv->certificate),
	                                       NULL, type, &bval))
		bval = FALSE;

	/* Don't have the purpose */
	if (bval != TRUE) {
		*val = CKT_NETSCAPE_UNTRUSTED;
		return CKR_OK;
	}

	/* Ascertain the trust in this certificate */
	if (!gkm_object_get_attribute_boolean (GKM_OBJECT (self->pv->certificate),
	                                       NULL, CKA_TRUSTED, &bval))
		bval = FALSE;

	if (bval != TRUE) {
		*val = CKT_NETSCAPE_TRUST_UNKNOWN;
		return CKR_OK;
	}

	/* See if we can delegate the purpase (ie: CA) */
	if (!gkm_object_get_attribute_ulong (GKM_OBJECT (self->pv->certificate),
	                                     NULL, CKA_CERTIFICATE_CATEGORY, &nval))
		nval = 0;

	/* 2 is a certificate authority in PKCS#11 */
	*val = (nval == 2) ? CKT_NETSCAPE_TRUSTED_DELEGATOR : CKT_NETSCAPE_TRUSTED;
	return CKR_OK;
}

static CK_RV
read_enhanced_usage (GkmCertificateTrust *self, CK_ATTRIBUTE_TYPE type,
                     CK_ATTRIBUTE_PTR attr)
{
	CK_ULONG value;
	CK_RV rv;

	g_assert (GKM_IS_CERTIFICATE_TRUST (self));

	rv = has_enhanced_usage (self, type, &value);
	if (rv == CKR_OK)
		rv = gkm_attribute_set_ulong (attr, value);
	return rv;
}

static CK_RV
hash_certificate (GkmCertificateTrust *self, int algo, CK_ATTRIBUTE_PTR result)
{
	guchar *hash;
	gsize n_hash;
	CK_RV rv;

	g_assert (GKM_IS_CERTIFICATE_TRUST (self));

	g_return_val_if_fail (self->pv->certificate, CKR_GENERAL_ERROR);

	hash = gkm_certificate_hash (self->pv->certificate, algo, &n_hash);
	g_return_val_if_fail (hash, CKR_GENERAL_ERROR);

	rv = gkm_attribute_set_data (result, hash, n_hash);
	g_free (hash);

	return rv;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_certificate_trust_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmCertificateTrust *self = GKM_CERTIFICATE_TRUST (base);

	switch (attr->type)
	{
	case CKA_PRIVATE:
		return gkm_attribute_set_bool (attr, CK_FALSE);

	case CKA_TRUST_STEP_UP_APPROVED:
		return gkm_attribute_set_bool (attr, CK_FALSE);

	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_NETSCAPE_TRUST);

	/* Key restrictions */
	case CKA_TRUST_DIGITAL_SIGNATURE:
		return read_key_usage (self, PKIX_KEY_USAGE_DIGITAL_SIGNATURE, attr);

	case CKA_TRUST_NON_REPUDIATION:
		return read_key_usage (self, PKIX_KEY_USAGE_NON_REPUDIATION, attr);

	case CKA_TRUST_KEY_ENCIPHERMENT:
		return read_key_usage (self, PKIX_KEY_USAGE_KEY_ENCIPHERMENT, attr);

	case CKA_TRUST_DATA_ENCIPHERMENT:
		return read_key_usage (self, PKIX_KEY_USAGE_DATA_ENCIPHERMENT, attr);

	case CKA_TRUST_KEY_AGREEMENT:
		return read_key_usage (self, PKIX_KEY_USAGE_KEY_AGREEMENT, attr);

	case CKA_TRUST_KEY_CERT_SIGN:
		return read_key_usage (self, PKIX_KEY_USAGE_KEY_CERT_SIGN, attr);

	case CKA_TRUST_CRL_SIGN:
		return read_key_usage (self, PKIX_KEY_USAGE_CRL_SIGN, attr);

	/* Various trust flags */
	case CKA_TRUST_SERVER_AUTH:
		return read_enhanced_usage (self, CKA_GNOME_PURPOSE_SERVER_AUTH, attr);

	case CKA_TRUST_CLIENT_AUTH:
		return read_enhanced_usage (self, CKA_GNOME_PURPOSE_CLIENT_AUTH, attr);

	case CKA_TRUST_CODE_SIGNING:
		return read_enhanced_usage (self, CKA_GNOME_PURPOSE_CODE_SIGNING, attr);

	case CKA_TRUST_EMAIL_PROTECTION:
		return read_enhanced_usage (self, CKA_GNOME_PURPOSE_EMAIL_PROTECTION, attr);

	case CKA_TRUST_IPSEC_END_SYSTEM:
		return read_enhanced_usage (self, CKA_GNOME_PURPOSE_IPSEC_END_SYSTEM, attr);

	case CKA_TRUST_IPSEC_TUNNEL:
		return read_enhanced_usage (self, CKA_GNOME_PURPOSE_IPSEC_TUNNEL, attr);

	case CKA_TRUST_IPSEC_USER:
		return read_enhanced_usage (self, CKA_GNOME_PURPOSE_IPSEC_USER, attr);

	case CKA_TRUST_TIME_STAMPING:
		return read_enhanced_usage (self, CKA_GNOME_PURPOSE_TIME_STAMPING, attr);

	case CKA_ID:
	case CKA_SUBJECT:
	case CKA_SERIAL_NUMBER:
	case CKA_ISSUER:
		g_return_val_if_fail (self->pv->certificate, CKR_GENERAL_ERROR);
		return gkm_object_get_attribute (GKM_OBJECT (self->pv->certificate), session, attr);

	case CKA_CERT_MD5_HASH:
		return hash_certificate (self, GCRY_MD_MD5, attr);
	case CKA_CERT_SHA1_HASH:
		return hash_certificate (self, GCRY_MD_SHA1, attr);

	default:
		break;
	};

	return GKM_OBJECT_CLASS (gkm_certificate_trust_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_certificate_trust_init (GkmCertificateTrust *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_CERTIFICATE_TRUST, GkmCertificateTrustPrivate);
}

static void
gkm_certificate_trust_finalize (GObject *obj)
{
	GkmCertificateTrust *self = GKM_CERTIFICATE_TRUST (obj);

	if (self->pv->certificate)
		g_object_remove_weak_pointer (G_OBJECT (self->pv->certificate), (gpointer*)&(self->pv->certificate));
	self->pv->certificate = NULL;

	G_OBJECT_CLASS (gkm_certificate_trust_parent_class)->finalize (obj);
}

static void
gkm_certificate_trust_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	GkmCertificateTrust *self = GKM_CERTIFICATE_TRUST (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		g_return_if_fail (!self->pv->certificate);
		self->pv->certificate = g_value_get_object (value);
		g_return_if_fail (self->pv->certificate);
		g_object_add_weak_pointer (G_OBJECT (self->pv->certificate), (gpointer*)&(self->pv->certificate));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_certificate_trust_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	GkmCertificateTrust *self = GKM_CERTIFICATE_TRUST (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		g_value_set_object (value, gkm_certificate_trust_get_certificate (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_certificate_trust_class_init (GkmCertificateTrustClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gobject_class->finalize = gkm_certificate_trust_finalize;
	gobject_class->set_property = gkm_certificate_trust_set_property;
	gobject_class->get_property = gkm_certificate_trust_get_property;

	gkm_class->get_attribute = gkm_certificate_trust_get_attribute;

	g_type_class_add_private (klass, sizeof (GkmCertificateTrustPrivate));

	g_object_class_install_property (gobject_class, PROP_CERTIFICATE,
	           g_param_spec_object ("certificate", "Certificate", "Certificate this trust belongs to",
	                                GKM_TYPE_CERTIFICATE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	init_quarks ();
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmCertificateTrust*
gkm_certificate_trust_new (GkmModule *module, GkmManager *manager, GkmCertificate *cert)
{
	return g_object_new (GKM_TYPE_CERTIFICATE_TRUST, "module", module,
	                     "manager", manager, "certificate", cert, NULL);
}

GkmCertificate*
gkm_certificate_trust_get_certificate (GkmCertificateTrust *self)
{
	g_return_val_if_fail (GKM_IS_CERTIFICATE_TRUST (self), NULL);
	g_return_val_if_fail (self->pv->certificate, NULL);
	return self->pv->certificate;
}
