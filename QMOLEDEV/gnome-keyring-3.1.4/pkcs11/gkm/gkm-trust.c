/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
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

#include "gkm-trust.h"

#include "gkm-attributes.h"
#include "gkm-object.h"
#include "gkm-oids.h"

#include "pkcs11/pkcs11n.h"
#include "pkcs11/pkcs11i.h"

#include <glib/gi18n.h>

G_DEFINE_TYPE (GkmTrust, gkm_trust, GKM_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static CK_RV
trust_get_usage (GkmTrust *self, const gchar *purpose, CK_ATTRIBUTE_PTR attr)
{
	GkmTrustLevel level;
	CK_ULONG trust;

	level = gkm_trust_get_level_for_purpose (self, purpose);

	switch (level) {
	case GKM_TRUST_UNKNOWN:
		trust = CKT_NETSCAPE_TRUST_UNKNOWN;
		break;
	case GKM_TRUST_DISTRUSTED:
		trust = CKT_NETSCAPE_UNTRUSTED;
		break;
	case GKM_TRUST_TRUSTED:
		trust = CKT_NETSCAPE_TRUSTED;
		break;
	case GKM_TRUST_ANCHOR:
		trust = CKT_NETSCAPE_TRUSTED_DELEGATOR;
		break;
	default:
		g_return_val_if_reached (CKR_GENERAL_ERROR);
	};

	return gkm_attribute_set_ulong (attr, trust);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_trust_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmTrust *self = GKM_TRUST (base);

	/*
	 * This object exposes a netscape compatible trust object. However the
	 * primary interface for dealing with trust is through GkmAssertion objects.
	 */

	switch (attr->type)
	{
	case CKA_PRIVATE:
		return gkm_attribute_set_bool (attr, CK_FALSE);
	case CKA_TRUST_STEP_UP_APPROVED:
		return gkm_attribute_set_bool (attr, CK_FALSE);
	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_NETSCAPE_TRUST);
	case CKA_MODIFIABLE:
		return gkm_attribute_set_bool (attr, CK_FALSE);

	/*
	 * TODO: Is it even useful to support overriding from certificate
	 * defaults? For now we just return unknown for all of them, and
	 * the caller should use whatever's in the certificate.
	 */
	case CKA_TRUST_DIGITAL_SIGNATURE:
	case CKA_TRUST_NON_REPUDIATION:
	case CKA_TRUST_KEY_ENCIPHERMENT:
	case CKA_TRUST_DATA_ENCIPHERMENT:
	case CKA_TRUST_KEY_AGREEMENT:
	case CKA_TRUST_KEY_CERT_SIGN:
	case CKA_TRUST_CRL_SIGN:
		return gkm_attribute_set_ulong (attr, CKT_NETSCAPE_TRUST_UNKNOWN);

	/* Various trust flags */
	case CKA_TRUST_SERVER_AUTH:
		return trust_get_usage (self, GKM_OID_EXTUSAGE_SERVER_AUTH, attr);
	case CKA_TRUST_CLIENT_AUTH:
		return trust_get_usage (self, GKM_OID_EXTUSAGE_CLIENT_AUTH, attr);
	case CKA_TRUST_CODE_SIGNING:
		return trust_get_usage (self, GKM_OID_EXTUSAGE_CODE_SIGNING, attr);
	case CKA_TRUST_EMAIL_PROTECTION:
		return trust_get_usage (self, GKM_OID_EXTUSAGE_EMAIL, attr);
	case CKA_TRUST_IPSEC_END_SYSTEM:
		return trust_get_usage (self, GKM_OID_EXTUSAGE_IPSEC_ENDPOINT, attr);
	case CKA_TRUST_IPSEC_TUNNEL:
		return trust_get_usage (self, GKM_OID_EXTUSAGE_IPSEC_TUNNEL, attr);
	case CKA_TRUST_IPSEC_USER:
		return trust_get_usage (self, GKM_OID_EXTUSAGE_IPSEC_USER, attr);
	case CKA_TRUST_TIME_STAMPING:
		return trust_get_usage (self, GKM_OID_EXTUSAGE_TIME_STAMPING, attr);

	/* Certificate reference values */
	case CKA_SUBJECT:
	case CKA_SERIAL_NUMBER:
	case CKA_ISSUER:
	case CKA_CERT_MD5_HASH:
	case CKA_CERT_SHA1_HASH:
		g_warning ("derived class should have provided these attributes");
		return CKR_ATTRIBUTE_TYPE_INVALID;

	default:
		break;
	};

	return GKM_OBJECT_CLASS (gkm_trust_parent_class)->get_attribute (base, session, attr);
}

static GkmTrustLevel
gkm_trust_real_get_trust_level (GkmTrust *self, const gchar *purpose)
{
	return GKM_TRUST_UNKNOWN;
}

static void
gkm_trust_init (GkmTrust *self)
{
	/* For future expansion */
	self->pv = NULL;
}

static void
gkm_trust_class_init (GkmTrustClass *klass)
{
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	gkm_class->get_attribute = gkm_trust_get_attribute;
	klass->get_trust_level = gkm_trust_real_get_trust_level;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmTrustLevel
gkm_trust_get_level_for_purpose (GkmTrust *self, const gchar *purpose)
{
	g_return_val_if_fail (GKM_IS_TRUST (self), GKM_TRUST_UNKNOWN);
	g_return_val_if_fail (purpose, GKM_TRUST_UNKNOWN);
	g_assert (GKM_TRUST_GET_CLASS (self)->get_trust_level);
	return GKM_TRUST_GET_CLASS (self)->get_trust_level (self, purpose);
}
