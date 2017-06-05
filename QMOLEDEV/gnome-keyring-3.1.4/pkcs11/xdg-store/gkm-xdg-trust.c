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

#include "gkm-xdg-assertion.h"
#include "gkm-xdg-trust.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-byte-array.h"

#include "gkm/gkm-assertion.h"
#include "gkm/gkm-attributes.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-oids.h"
#include "gkm/gkm-serializable.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-transaction.h"
#include "gkm/gkm-util.h"

#include "pkcs11/pkcs11i.h"
#include "pkcs11/pkcs11n.h"
#include "pkcs11/pkcs11x.h"

#include <libtasn1.h>

#include <glib/gi18n.h>

struct _GkmXdgTrustPrivate {
	GHashTable *assertions;
	GNode *asn;
	gpointer data;
	gsize n_data;
};

/* From asn1-def-xdg.c */
extern const ASN1_ARRAY_TYPE xdg_asn1_tab[];

static void gkm_xdg_trust_serializable (GkmSerializableIface *iface);

G_DEFINE_TYPE_EXTENDED (GkmXdgTrust, gkm_xdg_trust, GKM_TYPE_TRUST, 0,
                        G_IMPLEMENT_INTERFACE (GKM_TYPE_SERIALIZABLE, gkm_xdg_trust_serializable));

static GQuark QDATA_ASSERTION_KEY = 0;

/* Forward declarations */
static void add_assertion_to_trust (GkmXdgTrust *self, GkmAssertion *assertion,
                                    GkmTransaction *transaction);

static void remove_assertion_from_trust (GkmXdgTrust *self, GkmAssertion *assertion,
                                         GkmTransaction *transaction);

/* -----------------------------------------------------------------------------
 * QUARKS
 */

static GQuark TRUST_UNKNOWN;
static GQuark TRUST_DISTRUSTED;
static GQuark TRUST_TRUSTED;
static GQuark TRUST_TRUSTED_ANCHOR;

static void
init_quarks (void)
{
	static volatile gsize quarks_inited = 0;

	if (g_once_init_enter (&quarks_inited)) {

		#define QUARK(name, value) \
			name = g_quark_from_static_string(value)

		QUARK (TRUST_UNKNOWN, "trustUnknown");
		QUARK (TRUST_DISTRUSTED, "distrusted");
		QUARK (TRUST_TRUSTED, "trusted");
		QUARK (TRUST_TRUSTED_ANCHOR, "trustedAnchor");

		#undef QUARK

		g_once_init_leave (&quarks_inited, 1);
	}
}

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static CK_RV
trust_get_der (GkmXdgTrust *self, const gchar *part, CK_ATTRIBUTE_PTR attr)
{
	GNode *node;
	gconstpointer element;
	gsize n_element;

	g_assert (GKM_XDG_IS_TRUST (self));

	node = egg_asn1x_node (self->pv->asn, "reference", "certReference", part, NULL);
	g_return_val_if_fail (node, CKR_GENERAL_ERROR);

	/* If the assertion doesn't contain this info ... */
	if (!egg_asn1x_have (node))
		return CKR_ATTRIBUTE_TYPE_INVALID;

	element = egg_asn1x_get_raw_element (node, &n_element);
	return gkm_attribute_set_data (attr, element, n_element);
}

static CK_RV
trust_get_integer (GkmXdgTrust *self, const gchar *part, CK_ATTRIBUTE_PTR attr)
{
	GNode *node;
	gpointer integer;
	gsize n_integer;
	CK_RV rv;

	g_assert (GKM_XDG_IS_TRUST (self));

	node = egg_asn1x_node (self->pv->asn, "reference", "certReference", part, NULL);
	g_return_val_if_fail (node, CKR_GENERAL_ERROR);

	/* If the assertion doesn't contain this info ... */
	if (!egg_asn1x_have (node))
		return CKR_ATTRIBUTE_TYPE_INVALID;

	integer = egg_asn1x_get_integer_as_raw (node, NULL, &n_integer);
	g_return_val_if_fail (integer, CKR_GENERAL_ERROR);

	rv = gkm_attribute_set_data (attr, integer, n_integer);
	g_free (integer);

	return rv;
}

static CK_RV
trust_get_hash (GkmXdgTrust *self, GChecksumType ctype, CK_ATTRIBUTE_PTR attr)
{
	GNode *cert;
	gconstpointer element;
	gsize n_element;

	cert = egg_asn1x_node (self->pv->asn, "reference", "certComplete", NULL);
	g_return_val_if_fail (cert, CKR_GENERAL_ERROR);

	/* If it's not stored, then this attribute is not present */
	if (!egg_asn1x_have (cert))
		return CKR_ATTRIBUTE_TYPE_INVALID;

	element = egg_asn1x_get_raw_element (cert, &n_element);
	g_return_val_if_fail (element, CKR_GENERAL_ERROR);

	return gkm_attribute_set_checksum (attr, ctype, element, n_element);
}

static CK_RV
trust_get_complete (GkmXdgTrust *self, CK_ATTRIBUTE_PTR attr)
{
	GNode *cert;
	gconstpointer element;
	gsize n_element;

	cert = egg_asn1x_node (self->pv->asn, "reference", "certComplete", NULL);
	g_return_val_if_fail (cert, CKR_GENERAL_ERROR);

	/* If it's not stored, then this attribute is not present */
	if (!egg_asn1x_have (cert))
		return CKR_ATTRIBUTE_TYPE_INVALID;

	element = egg_asn1x_get_raw_element (cert, &n_element);
	g_return_val_if_fail (element, CKR_GENERAL_ERROR);

	return gkm_attribute_set_data (attr, element, n_element);
}


static gboolean
validate_der (CK_ATTRIBUTE_PTR attr, const gchar *asn_type)
{
	GNode *asn;
	gboolean valid = TRUE;

	if (!attr->pValue || attr->ulValueLen == (CK_ULONG)-1)
		return FALSE;

	asn = egg_asn1x_create (pkix_asn1_tab, asn_type);
	g_return_val_if_fail (asn, FALSE);

	valid = egg_asn1x_decode (asn, attr->pValue, attr->ulValueLen);
	if (!valid)
		g_message ("failed to parse certificate passed to trust assertion: %s",
		           egg_asn1x_message (asn));

	/* Yes, this is an expensive check, but worthwhile */
	egg_asn1x_destroy (asn);
	return valid;
}

static gboolean
validate_integer (CK_ATTRIBUTE_PTR attr)
{
	return attr->pValue != NULL &&
	       attr->ulValueLen > 0 &&
	       attr->ulValueLen != (CK_ULONG)-1;
}

static GQuark
assertion_type_to_level_enum (CK_X_ASSERTION_TYPE type)
{
	switch (type) {
	case CKT_X_DISTRUSTED_CERTIFICATE:
		return TRUST_DISTRUSTED;
	case CKT_X_ANCHORED_CERTIFICATE:
		return TRUST_TRUSTED_ANCHOR;
	case CKT_X_PINNED_CERTIFICATE:
		return TRUST_TRUSTED;
	default:
		return 0;
	};
}

static gboolean
level_enum_to_assertion_type (GQuark level, CK_X_ASSERTION_TYPE *type)
{
	if (level == TRUST_DISTRUSTED)
		*type = CKT_X_DISTRUSTED_CERTIFICATE;
	else if (level == TRUST_TRUSTED_ANCHOR)
		*type = CKT_X_ANCHORED_CERTIFICATE;
	else if (level == TRUST_TRUSTED)
		*type = CKT_X_PINNED_CERTIFICATE;
	else if (level == TRUST_UNKNOWN)
		*type = 0;
	else
		return FALSE;
	return TRUE;
}

static void
check_and_unref_assertion (gpointer data)
{
	g_assert (GKM_IS_ASSERTION (data));
	g_assert (g_object_get_qdata (data, QDATA_ASSERTION_KEY) != NULL);
	g_object_run_dispose (data);
	g_object_unref (data);
}

static GHashTable*
create_assertions (void)
{
	return g_hash_table_new_full (egg_byte_array_hash, egg_byte_array_equal,
	                              (GDestroyNotify)g_byte_array_unref,
	                              check_and_unref_assertion);
}

static GkmAssertion*
create_assertion (GkmXdgTrust *self, GNode *asn)
{
	CK_X_ASSERTION_TYPE type = 0;
	GkmAssertion *assertion;
	GQuark level;
	gchar *purpose;
	gchar *peer;
	GNode *node;

	/* Get the trust level */
	level = egg_asn1x_get_enumerated (egg_asn1x_node (asn, "level", NULL));
	g_return_val_if_fail (level != 0, NULL);
	if (!level_enum_to_assertion_type (level, &type))
		g_message ("unsupported trust level %s in trust object", g_quark_to_string (level));
	else if (type == 0)
		return NULL;

	/* A purpose */
	purpose = egg_asn1x_get_string_as_utf8 (egg_asn1x_node (asn, "purpose", NULL), NULL);
	g_return_val_if_fail (purpose, NULL);

	/* A peer name */
	node = egg_asn1x_node (asn, "peer", NULL);
	if (egg_asn1x_have (node))
		peer = egg_asn1x_get_string_as_utf8 (node, NULL);
	else
		peer = NULL;

	assertion = g_object_new (GKM_XDG_TYPE_ASSERTION,
	                          "module", gkm_object_get_module (GKM_OBJECT (self)),
	                          "manager", gkm_object_get_manager (GKM_OBJECT (self)),
	                          "trust", self,
	                          "type", type,
	                          "purpose", purpose,
	                          "peer", peer,
	                          NULL);

	g_free (purpose);
	g_free (peer);

	return assertion;
}

static GByteArray*
create_assertion_key (const gchar *purpose, const gchar *peer)
{
	GByteArray *key;

	g_return_val_if_fail (purpose, NULL);

	key = g_byte_array_new ();
	g_byte_array_append (key, (void*)purpose, strlen (purpose));

	if (peer != NULL) {
		g_byte_array_append (key, (void*)"\0", 1);
		g_byte_array_append (key, (void*)peer, strlen (peer));
	}

	return key;
}

static GByteArray*
lookup_assertion_key (GkmAssertion *assertion)
{
	return g_object_get_qdata (G_OBJECT (assertion), QDATA_ASSERTION_KEY);
}

static GByteArray*
lookup_or_create_assertion_key (GkmAssertion *assertion)
{
	GByteArray *key;

	key = lookup_assertion_key (assertion);
	if (key == NULL) {
		key = create_assertion_key (gkm_assertion_get_purpose (assertion),
		                            gkm_assertion_get_peer (assertion));
		g_object_set_qdata_full (G_OBJECT (assertion), QDATA_ASSERTION_KEY,
		                         g_byte_array_ref (key), (GDestroyNotify)g_byte_array_unref);
	}

	return key;
}

static gboolean
complete_add_assertion (GkmTransaction *transaction, GObject *object, gpointer user_data)
{
	GkmAssertion *assertion = GKM_ASSERTION (user_data);
	GkmXdgTrust *self = GKM_XDG_TRUST (object);

	if (gkm_transaction_get_failed (transaction))
		remove_assertion_from_trust (self, assertion, NULL);

	g_object_unref (assertion);
	return TRUE;
}

static void
add_assertion_to_trust (GkmXdgTrust *self, GkmAssertion *assertion,
                        GkmTransaction *transaction)
{
	GByteArray *key;

	key = lookup_or_create_assertion_key (assertion);
	g_assert (key);

	g_hash_table_insert (self->pv->assertions, g_byte_array_ref (key), g_object_ref (assertion));
	gkm_object_expose (GKM_OBJECT (assertion), gkm_object_is_exposed (GKM_OBJECT (self)));

	if (transaction != NULL)
		gkm_transaction_add (transaction, self, complete_add_assertion, g_object_ref (assertion));
}

static gboolean
complete_remove_assertion (GkmTransaction *transaction, GObject *object, gpointer user_data)
{
	GkmXdgTrust *self = GKM_XDG_TRUST (object);
	GkmAssertion *assertion = GKM_ASSERTION (user_data);

	if (gkm_transaction_get_failed (transaction))
		add_assertion_to_trust (self, assertion, NULL);
	else
		g_object_run_dispose (G_OBJECT (assertion));

	g_object_unref (assertion);
	return TRUE;
}

static void
remove_assertion_from_trust (GkmXdgTrust *self, GkmAssertion *assertion,
                             GkmTransaction *transaction)
{
	GByteArray *key;

	key = lookup_assertion_key (assertion);
	g_assert (key);

	gkm_object_expose (GKM_OBJECT (assertion), FALSE);

	if (transaction == NULL) {
		if (!g_hash_table_remove (self->pv->assertions, key))
			g_return_if_reached ();
	} else {
		if (!g_hash_table_steal (self->pv->assertions, key))
			g_return_if_reached ();
		gkm_transaction_add (transaction, self, complete_remove_assertion, assertion);
	}
}

static gboolean
load_assertions (GkmXdgTrust *self, GNode *asn)
{
	gconstpointer element;
	GHashTable *assertions;
	GkmAssertion *assertion;
	gsize n_element;
	GByteArray *key;
	GNode *node;
	guint count, i;

	g_assert (self);
	g_assert (asn);

	assertions = self->pv->assertions;
	self->pv->assertions = create_assertions ();

	count = egg_asn1x_count (egg_asn1x_node (asn, "assertions", NULL));

	for (i = 0; i < count; ++i) {
		node = egg_asn1x_node (asn, "assertions", i + 1, NULL);
		g_return_val_if_fail (node, FALSE);

		/* We use the raw DER encoding as an assertion */
		element = egg_asn1x_get_raw_element (node, &n_element);
		g_return_val_if_fail (node, FALSE);

		/* Double check that this is valid, because it's how we hash */
		key = g_byte_array_new ();
		g_byte_array_append (key, element, n_element);

		/* Already have this assertion? */
		assertion = g_hash_table_lookup (assertions, key);
		if (assertion) {
			if (!g_hash_table_steal (assertions, key))
				g_assert_not_reached ();

		/* Create a new assertion */
		} else {
			assertion = create_assertion (self, node);
		}

		add_assertion_to_trust (self, assertion, NULL);
		g_byte_array_unref (key);
		g_object_unref (assertion);
	}

	/* Override the stored assertions and netscape trust */
	g_hash_table_remove_all (assertions);
	g_hash_table_unref (assertions);

	return TRUE;
}

static gboolean
save_assertion (GNode *asn, GkmAssertion *assertion)
{
	const gchar *purpose;
	const gchar *peer;
	GQuark level;

	level = assertion_type_to_level_enum (gkm_assertion_get_trust_type (assertion));
	purpose = gkm_assertion_get_purpose (assertion);
	peer = gkm_assertion_get_peer (assertion);

	if (!egg_asn1x_set_string_as_utf8 (egg_asn1x_node (asn, "purpose", NULL),
	                                   g_strdup (purpose), g_free) ||
	    !egg_asn1x_set_enumerated (egg_asn1x_node (asn, "level", NULL), level))
		g_return_val_if_reached (FALSE);

	if (peer && !egg_asn1x_set_string_as_utf8 (egg_asn1x_node (asn, "peer", NULL),
	                                           g_strdup (peer), g_free))
		g_return_val_if_reached (FALSE);

	return TRUE;
}

static gboolean
save_assertions (GkmXdgTrust *self, GNode *asn)
{
	GHashTableIter iter;
	GNode *pair, *node;
	gpointer value;

	g_assert (GKM_XDG_IS_TRUST (self));
	g_assert (asn);

	node = egg_asn1x_node (asn, "assertions", NULL);
	egg_asn1x_clear (node);

	g_hash_table_iter_init (&iter, self->pv->assertions);
	while (g_hash_table_iter_next (&iter, NULL, &value)) {
		pair = egg_asn1x_append (node);
		g_return_val_if_fail (pair, FALSE);
		save_assertion (pair, GKM_ASSERTION (value));
	}

	return TRUE;
}

static GkmXdgTrust*
create_trust_for_reference (GkmModule *module, GkmManager *manager,
                            CK_ATTRIBUTE_PTR serial, CK_ATTRIBUTE_PTR issuer)
{
	GkmXdgTrust *trust;
	GNode *asn, *ref, *node;

	asn = egg_asn1x_create (xdg_asn1_tab, "trust-1");
	g_return_val_if_fail (asn, NULL);

	ref = egg_asn1x_node (asn, "reference", NULL);
	node = egg_asn1x_node (ref, "certReference", NULL);

	egg_asn1x_set_choice (ref, node);
	egg_asn1x_set_integer_as_raw (egg_asn1x_node (node, "serialNumber", NULL),
	                              g_memdup (serial->pValue, serial->ulValueLen),
	                              serial->ulValueLen, g_free);

	egg_asn1x_set_raw_element (egg_asn1x_node (node, "issuer", NULL),
	                           g_memdup (issuer->pValue, issuer->ulValueLen),
	                           issuer->ulValueLen, g_free);

	trust = g_object_new (GKM_XDG_TYPE_TRUST, "module", module, "manager", manager, NULL);
	trust->pv->asn = asn;

	/* Encode it, so we have read access to all the data */
	trust->pv->data = egg_asn1x_encode (asn, NULL, &trust->pv->n_data);
	if (!trust->pv->data) {
		g_warning ("created invalid trust object: %s", egg_asn1x_message (asn));
		return NULL;
	}

	return trust;
}

static GkmXdgTrust*
create_trust_for_complete (GkmModule *module, GkmManager *manager,
                              CK_ATTRIBUTE_PTR cert)
{
	GkmXdgTrust *trust;
	GNode *asn, *ref, *node;

	asn = egg_asn1x_create (xdg_asn1_tab, "trust-1");
	g_return_val_if_fail (asn, NULL);

	ref = egg_asn1x_node (asn, "reference", NULL);
	node = egg_asn1x_node (ref, "certComplete", NULL);

	egg_asn1x_set_choice (ref, node);
	egg_asn1x_set_raw_element (node, g_memdup (cert->pValue, cert->ulValueLen),
	                           cert->ulValueLen, g_free);

	trust = g_object_new (GKM_XDG_TYPE_TRUST, "module", module, "manager", manager, NULL);
	trust->pv->asn = asn;

	/* Encode it, which validates, and so we have read access to all the data */
	trust->pv->data = egg_asn1x_encode (asn, NULL, &trust->pv->n_data);
	if (!trust->pv->data) {
		g_warning ("created invalid trust object: %s", egg_asn1x_message (asn));
		return NULL;
	}

	return trust;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_xdg_trust_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmXdgTrust *self = GKM_XDG_TRUST (base);

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

	/* Certificate reference values */
	case CKA_SUBJECT:
		return trust_get_der (self, "subject", attr);
	case CKA_SERIAL_NUMBER:
		return trust_get_integer (self, "serialNumber", attr);
	case CKA_ISSUER:
		return trust_get_der (self, "issuer", attr);
	case CKA_X_CERTIFICATE_VALUE:
		return trust_get_complete (self, attr);

	/* Certificate hash values */
	case CKA_CERT_MD5_HASH:
		return trust_get_hash (self, G_CHECKSUM_MD5, attr);
	case CKA_CERT_SHA1_HASH:
		return trust_get_hash (self, G_CHECKSUM_SHA1, attr);

	default:
		break;
	};

	return GKM_OBJECT_CLASS (gkm_xdg_trust_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_xdg_trust_expose_object (GkmObject *base, gboolean expose)
{
	GHashTableIter iter;
	gpointer value;

	GKM_OBJECT_CLASS (gkm_xdg_trust_parent_class)->expose_object (base, expose);

	g_hash_table_iter_init (&iter, GKM_XDG_TRUST (base)->pv->assertions);
	while (g_hash_table_iter_next (&iter, NULL, &value))
		gkm_object_expose (value, expose);
}

static GkmTrustLevel
gkm_xdg_trust_get_level (GkmTrust *base, const gchar *purpose)
{
	GkmXdgTrust *self = GKM_XDG_TRUST (base);
	GkmAssertion *assertion;
	GByteArray *key;
	gulong type;

	key = create_assertion_key (purpose, NULL);
	assertion = g_hash_table_lookup (self->pv->assertions, key);
	g_byte_array_unref (key);

	if (!assertion)
		return GKM_TRUST_UNKNOWN;

	type = gkm_assertion_get_trust_type (assertion);
	if (type == CKT_X_ANCHORED_CERTIFICATE)
		return GKM_TRUST_ANCHOR;
	else if (type == CKT_X_PINNED_CERTIFICATE)
		return GKM_TRUST_TRUSTED;
	else if (type == CKT_X_DISTRUSTED_CERTIFICATE)
		return GKM_TRUST_DISTRUSTED;
	else
		g_return_val_if_reached (GKM_TRUST_UNKNOWN);
}

static void
gkm_xdg_trust_init (GkmXdgTrust *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_XDG_TYPE_TRUST, GkmXdgTrustPrivate);
	self->pv->assertions = create_assertions ();
}

static void
gkm_xdg_trust_finalize (GObject *obj)
{
	GkmXdgTrust *self = GKM_XDG_TRUST (obj);

	if (self->pv->asn)
		egg_asn1x_destroy (self->pv->asn);
	self->pv->asn = NULL;

	if (self->pv->assertions)
		g_hash_table_destroy (self->pv->assertions);
	self->pv->assertions = NULL;

	G_OBJECT_CLASS (gkm_xdg_trust_parent_class)->finalize (obj);
}

static void
gkm_xdg_trust_class_init (GkmXdgTrustClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	GkmTrustClass *trust_class = GKM_TRUST_CLASS (klass);

	gobject_class->finalize = gkm_xdg_trust_finalize;
	gkm_class->get_attribute = gkm_xdg_trust_get_attribute;
	gkm_class->expose_object = gkm_xdg_trust_expose_object;
	trust_class->get_trust_level = gkm_xdg_trust_get_level;

	QDATA_ASSERTION_KEY = g_quark_from_static_string ("gkm-xdg-trust-assertion-key");
	g_type_class_add_private (klass, sizeof (GkmXdgTrustPrivate));

	init_quarks ();
}

static gboolean
gkm_xdg_trust_real_load (GkmSerializable *base, GkmSecret *login, gconstpointer data, gsize n_data)
{
	GkmXdgTrust *self = GKM_XDG_TRUST (base);
	GNode *asn = NULL;
	gpointer copy;

	g_return_val_if_fail (GKM_XDG_IS_TRUST (self), FALSE);
	g_return_val_if_fail (data, FALSE);

	if (n_data == 0)
		return FALSE;

	copy = g_memdup (data, n_data);

	asn = egg_asn1x_create (xdg_asn1_tab, "trust-1");
	g_return_val_if_fail (asn, FALSE);

	if (!egg_asn1x_decode (asn, copy, n_data)) {
		g_warning ("couldn't parse trust data: %s", egg_asn1x_message (asn));
		egg_asn1x_destroy (asn);
		g_free (copy);
		return FALSE;
	}

	/* Next parse out all the pairs */
	if (!load_assertions (self, asn)) {
		egg_asn1x_destroy (asn);
		g_free (copy);
		return FALSE;
	}

	/* Take ownership of this new data */
	g_free (self->pv->data);
	self->pv->data = copy;
	self->pv->n_data = n_data;
	egg_asn1x_destroy (self->pv->asn);
	self->pv->asn = asn;

	return TRUE;
}

static gboolean
gkm_xdg_trust_real_save (GkmSerializable *base, GkmSecret *login, gpointer *data, gsize *n_data)
{
	GkmXdgTrust *self = GKM_XDG_TRUST (base);

	g_return_val_if_fail (GKM_XDG_IS_TRUST (self), FALSE);
	g_return_val_if_fail (data, FALSE);
	g_return_val_if_fail (n_data, FALSE);
	g_return_val_if_fail (self->pv->asn, FALSE);

	if (!save_assertions (self, self->pv->asn))
		return FALSE;

	*data = egg_asn1x_encode (self->pv->asn, NULL, n_data);
	if (*data == NULL) {
		g_warning ("encoding trust failed: %s", egg_asn1x_message (self->pv->asn));
		return FALSE;
	}

	/* ASN.1 now refers to this data, take ownership */
	g_free (self->pv->data);
	self->pv->data = *data;
	self->pv->n_data = *n_data;

	/* Return a duplicate, since we own encoded */
	*data = g_memdup (*data, *n_data);
	return TRUE;
}

static void
gkm_xdg_trust_serializable (GkmSerializableIface *iface)
{
	iface->extension = ".trust";
	iface->load = gkm_xdg_trust_real_load;
	iface->save = gkm_xdg_trust_real_save;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmXdgTrust*
gkm_xdg_trust_create_for_assertion (GkmModule *module, GkmManager *manager,
                                    GkmTransaction *transaction,
                                    CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{

	CK_ATTRIBUTE_PTR serial, issuer, cert;
	GkmXdgTrust *trust;

	g_return_val_if_fail (GKM_IS_MODULE (module), NULL);
	g_return_val_if_fail (GKM_IS_MANAGER (manager), NULL);
	g_return_val_if_fail (attrs || !n_attrs, NULL);

	serial = gkm_attributes_find (attrs, n_attrs, CKA_SERIAL_NUMBER);
	issuer = gkm_attributes_find (attrs, n_attrs, CKA_ISSUER);
	cert = gkm_attributes_find (attrs, n_attrs, CKA_X_CERTIFICATE_VALUE);

	/* A trust object with just serial + issuer */
	if (serial != NULL && issuer != NULL) {
		if (cert != NULL) {
			gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
			return NULL;
		}
		if (!validate_der (issuer, "Name") || !validate_integer (serial)) {
			gkm_transaction_fail (transaction, CKR_ATTRIBUTE_VALUE_INVALID);
			return NULL;
		}

		trust = create_trust_for_reference (module, manager, serial, issuer);

	/* A trust object with a full certificate */
	} else if (cert != NULL) {
		if (serial != NULL || issuer != NULL) {
			gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
			return NULL;
		}
		if (!validate_der (cert, "Certificate")) {
			gkm_transaction_fail (transaction, CKR_ATTRIBUTE_VALUE_INVALID);
			return NULL;
		}

		trust = create_trust_for_complete (module, manager, cert);

	/* Not sure what this is */
	} else {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	gkm_attributes_consume (attrs, n_attrs, CKA_X_CERTIFICATE_VALUE, CKA_ISSUER,
	                        CKA_SERIAL_NUMBER, G_MAXULONG);

	return trust;
}

void
gkm_xdg_trust_replace_assertion (GkmXdgTrust *self, GkmAssertion *assertion,
                             GkmTransaction *transaction)
{
	GkmAssertion *previous;
	GByteArray *key;

	g_return_if_fail (GKM_XDG_IS_TRUST (self));
	g_return_if_fail (GKM_IS_ASSERTION (assertion));
	g_return_if_fail (!transaction || GKM_IS_TRANSACTION (transaction));

	/* Build up a key if we don't have one */
	key = lookup_or_create_assertion_key (assertion);

	/* Remove any previous assertion with this key */
	previous = g_hash_table_lookup (self->pv->assertions, key);
	if (previous != NULL)
		remove_assertion_from_trust (self, previous, transaction);
	add_assertion_to_trust (self, assertion, transaction);

	g_byte_array_unref (key);
}

void
gkm_xdg_trust_remove_assertion (GkmXdgTrust *self, GkmAssertion *assertion,
                                GkmTransaction *transaction)
{
	GByteArray *key;

	g_return_if_fail (GKM_XDG_IS_TRUST (self));
	g_return_if_fail (GKM_IS_ASSERTION (assertion));
	g_return_if_fail (!transaction || GKM_IS_TRANSACTION (transaction));

	key = lookup_assertion_key (assertion);
	g_return_if_fail (key);

	/* Assertion needs to be from this trust object */
	g_return_if_fail (g_hash_table_lookup (self->pv->assertions, key) == assertion);
	remove_assertion_from_trust (self, assertion, transaction);
}

gboolean
gkm_xdg_trust_have_assertion (GkmXdgTrust *self)
{
	g_return_val_if_fail (GKM_XDG_IS_TRUST (self), FALSE);
	return g_hash_table_size (self->pv->assertions);
}
