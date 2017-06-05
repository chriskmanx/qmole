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

#include "gkm/gkm-assertion.h"
#include "gkm/gkm-attributes.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-transaction.h"
#include "gkm/gkm-trust.h"
#include "gkm/gkm-util.h"

#include "pkcs11/pkcs11i.h"
#include "pkcs11/pkcs11n.h"
#include "pkcs11/pkcs11x.h"

#include <glib/gi18n.h>

G_DEFINE_TYPE (GkmXdgAssertion, gkm_xdg_assertion, GKM_TYPE_ASSERTION);

/* -----------------------------------------------------------------------------
 * QUARKS
 */

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GkmXdgTrust*
lookup_or_create_trust_object (GkmSession *session, GkmManager *manager,
                               GkmTransaction *transaction, CK_X_ASSERTION_TYPE type,
                               CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs, gboolean *created)
{
	CK_ATTRIBUTE_PTR serial, issuer, value;
	CK_ATTRIBUTE lookups[3];
	CK_OBJECT_CLASS klass;
	CK_ULONG n_lookups;
	GList *objects;
	GkmXdgTrust *trust;
	GkmModule *module;

	klass = CKO_NETSCAPE_TRUST;
	lookups[0].type = CKA_CLASS;
	lookups[0].pValue = &klass;
	lookups[0].ulValueLen = sizeof (klass);

	switch (type) {
	case CKT_X_ANCHORED_CERTIFICATE:
	case CKT_X_PINNED_CERTIFICATE:
		value = gkm_attributes_find (attrs, n_attrs, CKA_X_CERTIFICATE_VALUE);
		if (!value) {
			gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
			return NULL;
		}

		/* Attributes used for looking up trust object */
		memcpy (lookups + 1, value, sizeof (*value));
		n_lookups = 2;
		break;

	case CKT_X_DISTRUSTED_CERTIFICATE:
		serial = gkm_attributes_find (attrs, n_attrs, CKA_SERIAL_NUMBER);
		issuer = gkm_attributes_find (attrs, n_attrs, CKA_ISSUER);
		if (!serial || !issuer) {
			gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
			return NULL;
		}

		/* Attributes used for looking up trust object */
		memcpy (lookups + 1, issuer, sizeof (*issuer));
		memcpy (lookups + 2, serial, sizeof (*serial));
		n_lookups = 3;
		break;

	default:
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
		return NULL;
	};

	objects = gkm_manager_find_by_attributes (manager, session, lookups, n_lookups);
	module = gkm_session_get_module (session);

	/* Found a matching trust object for this assertion */
	if (objects) {
		g_return_val_if_fail (GKM_XDG_IS_TRUST (objects->data), NULL);
		trust = g_object_ref (objects->data);
		g_list_free (objects);

	/* Create a trust object for this assertion */
	} else {
		trust = gkm_xdg_trust_create_for_assertion (module, manager, transaction,
		                                            lookups, n_lookups);

		gkm_attributes_consume (attrs, n_attrs, CKA_X_CERTIFICATE_VALUE,
		                        CKA_ISSUER, CKA_SERIAL_NUMBER, G_MAXULONG);
		gkm_attributes_consume (lookups, n_lookups, CKA_X_CERTIFICATE_VALUE,
		                        CKA_ISSUER, CKA_SERIAL_NUMBER, G_MAXULONG);

		if (!gkm_transaction_get_failed (transaction))
			gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (trust),
			                                      TRUE, lookups, n_lookups);
	}

	return trust;
}

static GkmObject*
factory_create_assertion (GkmSession *session, GkmTransaction *transaction,
                          CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmAssertion *assertion;
	CK_X_ASSERTION_TYPE type;
	GkmManager *manager;
	gboolean created = FALSE;
	GkmXdgTrust *trust;
	gchar *purpose;
	gchar *peer;

	g_return_val_if_fail (attrs || !n_attrs, NULL);

	if (!gkm_attributes_find_ulong (attrs, n_attrs, CKA_X_ASSERTION_TYPE, &type)) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	if (!gkm_attributes_find_string (attrs, n_attrs, CKA_X_PURPOSE, &purpose)) {
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCOMPLETE);
		return NULL;
	}

	if (!gkm_attributes_find_string (attrs, n_attrs, CKA_X_PEER, &peer))
		peer = NULL;

	/* Try to find or create an appropriate trust object for this assertion */
	manager = gkm_manager_for_template (attrs, n_attrs, session);
	trust = lookup_or_create_trust_object (session, manager, transaction,
	                                       type, attrs, n_attrs, &created);

	/* Creating the trust object failed */
	if (trust == NULL) {
		g_return_val_if_fail (gkm_transaction_get_failed (transaction), NULL);
		g_free (purpose);
		g_free (peer);
		return NULL;
	}

	assertion = g_object_new (GKM_XDG_TYPE_ASSERTION,
	                          "module", gkm_session_get_module (session),
	                          "manager", manager,
	                          "trust", trust,
	                          "type", type,
	                          "purpose", purpose,
	                          "peer", peer,
	                          NULL);

	/* Add the assertion to the trust object */
	if (!gkm_transaction_get_failed (transaction)) {
		gkm_xdg_trust_replace_assertion (trust, GKM_ASSERTION (assertion), transaction);
		if (gkm_transaction_get_failed (transaction)) {
			gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);

		/* A new trust assertion */
		} else {
			gkm_attributes_consume (attrs, n_attrs, CKA_X_ASSERTION_TYPE, CKA_X_PURPOSE, G_MAXULONG);
			gkm_session_complete_object_creation (session, transaction, GKM_OBJECT (assertion),
			                                      TRUE, attrs, n_attrs);
		}
	}

	g_object_unref (trust);
	return GKM_OBJECT (assertion);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gkm_xdg_assertion_init (GkmXdgAssertion *self)
{
	self->pv = NULL;
}

static void
gkm_xdg_assertion_class_init (GkmXdgAssertionClass *klass)
{

}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */


GkmFactory*
gkm_xdg_assertion_get_factory (void)
{
	static CK_OBJECT_CLASS klass = CKO_X_TRUST_ASSERTION;

	static CK_ATTRIBUTE attributes[] = {
		{ CKA_CLASS, &klass, sizeof (klass) },
	};

	static GkmFactory factory = {
		attributes,
		G_N_ELEMENTS (attributes),
		factory_create_assertion
	};

	return &factory;
}
