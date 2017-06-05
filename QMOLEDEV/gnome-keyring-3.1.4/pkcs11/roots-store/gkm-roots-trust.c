/*
 * gnome-trustring
 *
 * Copyright (C) 2008 Stefan Walter
 * Copyright (C) 2010 Collabora Ltd
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

#include "gkm-roots-trust.h"

#include "gkm/gkm-assertion.h"
#include "gkm/gkm-attributes.h"
#include "gkm/gkm-certificate.h"
#include "gkm/gkm-data-der.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-oids.h"
#include "gkm/gkm-util.h"

#include "pkcs11/pkcs11i.h"
#include "pkcs11/pkcs11n.h"
#include "pkcs11/pkcs11x.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_CERTIFICATE
};

struct _GkmRootsTrustPrivate {
	GkmCertificate *certificate;
	GList *assertions;
};

G_DEFINE_TYPE (GkmRootsTrust, gkm_roots_trust, GKM_TYPE_TRUST);

static GQuark OID_KEY_USAGE;
static GQuark OID_ENHANCED_USAGE;

/*
 * When a certificate doesn't explicitly supply the purposes it's allowed
 * to use, and gives carte blanche then we use the following list.
 */

const char *OID_KNOWN_PURPOSES[] = {
	GKM_OID_EXTUSAGE_SERVER_AUTH,
	GKM_OID_EXTUSAGE_CLIENT_AUTH,
	GKM_OID_EXTUSAGE_CODE_SIGNING,
	GKM_OID_EXTUSAGE_EMAIL,
	GKM_OID_EXTUSAGE_IPSEC_ENDPOINT,
	GKM_OID_EXTUSAGE_IPSEC_TUNNEL,
	GKM_OID_EXTUSAGE_IPSEC_USER,
	GKM_OID_EXTUSAGE_TIME_STAMPING,
	NULL
};
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

		QUARK (OID_ENHANCED_USAGE, "2.5.29.37");
		QUARK (OID_KEY_USAGE, "2.5.29.15");

		#undef QUARK

		g_once_init_leave (&quarks_inited, 1);
	}
}

static CK_RV
hash_certificate (GkmRootsTrust *self, int algo, CK_ATTRIBUTE_PTR result)
{
	guchar *hash;
	gsize n_hash;
	CK_RV rv;

	g_assert (GKM_ROOTS_IS_TRUST (self));

	g_return_val_if_fail (self->pv->certificate, CKR_GENERAL_ERROR);

	hash = gkm_certificate_hash (self->pv->certificate, algo, &n_hash);
	g_return_val_if_fail (hash, CKR_GENERAL_ERROR);

	rv = gkm_attribute_set_data (result, hash, n_hash);
	g_free (hash);

	return rv;
}

static CK_RV
full_certificate (GkmRootsTrust *self, CK_ATTRIBUTE_PTR result)
{
	gconstpointer data;
	gsize n_data;

	data = gkm_certificate_der_data (self->pv->certificate ,&n_data);
	g_return_val_if_fail (data, CKR_GENERAL_ERROR);

	return gkm_attribute_set_data (result, data, n_data);
}

static GQuark*
lookup_extended_usages (GkmRootsTrust *self)
{
	gconstpointer extension;
	gsize n_extension;
	GQuark *usages = NULL;
	GkmDataResult res;

	extension = gkm_certificate_get_extension (self->pv->certificate,
	                                           OID_ENHANCED_USAGE,
	                                           &n_extension, NULL);

	if (!extension)
		return NULL;

	/* Returns null terminated set of OID quarks */
	res = gkm_data_der_read_enhanced_usage (extension, n_extension, &usages);

	/* Failure: An empty set means nothing is trusted */
	if (res != GKM_DATA_SUCCESS) {
		g_message ("couldn't parse extended usage info in certificate");
		usages = g_new0 (GQuark, 1);
	}

	return usages;
}

static gboolean
is_certificate_authority (GkmCertificate *cert)
{
	gulong nval;

	if (!gkm_object_get_attribute_ulong (GKM_OBJECT (cert),
	                                     NULL, CKA_CERTIFICATE_CATEGORY, &nval))
		nval = 0;

	/* 2 is a certificate authority in PKCS#11 */
	return (nval == 2) ? TRUE : FALSE;
}

static void
build_linked_assertion (GkmRootsTrust *self, GkmTrustLevel level, const gchar *purpose)
{
	GkmAssertion *assertion;
	gulong type = 0;

	/* For now only have logic to create assertions early */
	g_return_if_fail (!gkm_object_is_exposed (GKM_OBJECT (self)));

	switch (level) {
	case GKM_TRUST_UNKNOWN:
		return;
	case GKM_TRUST_TRUSTED:
		type = CKT_X_PINNED_CERTIFICATE;
		break;
	case GKM_TRUST_DISTRUSTED:
		type = CKT_X_DISTRUSTED_CERTIFICATE;
		break;
	case GKM_TRUST_ANCHOR:
		type = CKT_X_ANCHORED_CERTIFICATE;
		break;
	default:
		g_assert_not_reached ();
		return;
	};

	assertion = gkm_assertion_new (GKM_TRUST (self), type, purpose, NULL);
	self->pv->assertions = g_list_prepend (self->pv->assertions, assertion);
}

static void
ensure_linked_assertions (GkmRootsTrust *self)
{
        GQuark *usages, *u;
        const gchar **p;
        GkmTrustLevel level;

        usages = lookup_extended_usages (self);

        if (is_certificate_authority (self->pv->certificate))
                level = GKM_TRUST_ANCHOR;
        else
                level = GKM_TRUST_TRUSTED;

        /* Build assertions for all the listed usages */
        if (usages) {
                for (u = usages; *u; ++u)
                        build_linked_assertion (self, level, g_quark_to_string (*u));

        /* Build assertions for all the known default purposes */
        } else {
                for (p = OID_KNOWN_PURPOSES; *p; ++p)
                        build_linked_assertion (self, level, *p);
        }
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_roots_trust_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmRootsTrust *self = GKM_ROOTS_TRUST (base);

	switch (attr->type)
	{
	case CKA_SUBJECT:
	case CKA_SERIAL_NUMBER:
	case CKA_ISSUER:
	case CKA_VALUE:
		g_return_val_if_fail (self->pv->certificate, CKR_GENERAL_ERROR);
		return gkm_object_get_attribute (GKM_OBJECT (self->pv->certificate), session, attr);

	case CKA_CERT_MD5_HASH:
		return hash_certificate (self, GCRY_MD_MD5, attr);
	case CKA_CERT_SHA1_HASH:
		return hash_certificate (self, GCRY_MD_SHA1, attr);
	case CKA_X_CERTIFICATE_VALUE:
		return full_certificate (self, attr);

	default:
		break;
	};

	return GKM_OBJECT_CLASS (gkm_roots_trust_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_roots_trust_expose_object (GkmObject *base, gboolean expose)
{
	GkmRootsTrust *self = GKM_ROOTS_TRUST (base);
	GList *l;

	/* Build all the assertions the first time around */
	if (expose && !self->pv->assertions)
		ensure_linked_assertions (self);

	GKM_OBJECT_CLASS (gkm_roots_trust_parent_class)->expose_object (base, expose);

	/* Now expose all the child assertions */
	for (l = self->pv->assertions; l; l = g_list_next (l))
		gkm_object_expose (l->data, expose);
}

static GkmTrustLevel
gkm_roots_trust_get_trust_level (GkmTrust *base, const gchar *purpose)
{
	GkmRootsTrust *self;
	GkmTrustLevel result;
	GQuark *usage, *usages;
	GQuark oid;

	self = GKM_ROOTS_TRUST (base);

	/*
	 * For root certificates we just believe whatever is in the
	 * certificate enhanced usage extension.
	 */

	usages = lookup_extended_usages (self);

	/* No enhanced usage noted, any are allowed */
	if (!usages) {
		result = GKM_TRUST_TRUSTED;

	} else {
		result = GKM_TRUST_DISTRUSTED;
		oid = g_quark_try_string (purpose);
		for (usage = usages; *usage; ++usage) {
			if (*usage == oid) {
				result = GKM_TRUST_TRUSTED;
				break;
			}
		}
	}

	g_free (usages);

	/* See if we can delegate the trust (ie: CA) */
	if (result == GKM_TRUST_TRUSTED && is_certificate_authority (self->pv->certificate))
		result = GKM_TRUST_ANCHOR;

	return result;
}

static void
gkm_roots_trust_init (GkmRootsTrust *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_ROOTS_TYPE_TRUST, GkmRootsTrustPrivate);
}

static void
gkm_roots_trust_dispose (GObject *obj)
{
	GkmRootsTrust *self = GKM_ROOTS_TRUST (obj);
	GList *l;

	for (l = self->pv->assertions; l; l = g_list_next (l)) {
		g_object_run_dispose (G_OBJECT (l->data));
		g_object_unref (l->data);
	}

	g_list_free (self->pv->assertions);
	self->pv->assertions = NULL;

	G_OBJECT_CLASS (gkm_roots_trust_parent_class)->dispose (obj);
}

static void
gkm_roots_trust_finalize (GObject *obj)
{
	GkmRootsTrust *self = GKM_ROOTS_TRUST (obj);

	if (self->pv->certificate)
		g_object_remove_weak_pointer (G_OBJECT (self->pv->certificate), (gpointer*)&(self->pv->certificate));
	self->pv->certificate = NULL;

	g_assert (!self->pv->assertions);

	G_OBJECT_CLASS (gkm_roots_trust_parent_class)->finalize (obj);
}

static void
gkm_roots_trust_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	GkmRootsTrust *self = GKM_ROOTS_TRUST (obj);

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
gkm_roots_trust_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	GkmRootsTrust *self = GKM_ROOTS_TRUST (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		g_value_set_object (value, gkm_roots_trust_get_certificate (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_roots_trust_class_init (GkmRootsTrustClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	GkmTrustClass *trust_class = GKM_TRUST_CLASS (klass);

	gobject_class->dispose = gkm_roots_trust_dispose;
	gobject_class->finalize = gkm_roots_trust_finalize;
	gobject_class->set_property = gkm_roots_trust_set_property;
	gobject_class->get_property = gkm_roots_trust_get_property;

	gkm_class->get_attribute = gkm_roots_trust_get_attribute;
	gkm_class->expose_object = gkm_roots_trust_expose_object;

	trust_class->get_trust_level = gkm_roots_trust_get_trust_level;

	g_type_class_add_private (klass, sizeof (GkmRootsTrustPrivate));

	g_object_class_install_property (gobject_class, PROP_CERTIFICATE,
	           g_param_spec_object ("certificate", "Certificate", "Certificate this trust belongs to",
	                                GKM_TYPE_CERTIFICATE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	init_quarks ();
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmRootsTrust*
gkm_roots_trust_new (GkmModule *module, GkmManager *manager, GkmCertificate *cert)
{
	return g_object_new (GKM_ROOTS_TYPE_TRUST, "module", module,
	                     "manager", manager, "certificate", cert, NULL);
}

GkmCertificate*
gkm_roots_trust_get_certificate (GkmRootsTrust *self)
{
	g_return_val_if_fail (GKM_ROOTS_IS_TRUST (self), NULL);
	g_return_val_if_fail (self->pv->certificate, NULL);
	return self->pv->certificate;
}
