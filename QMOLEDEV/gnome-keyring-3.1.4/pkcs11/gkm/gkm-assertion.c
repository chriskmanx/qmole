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

#include "gkm-assertion.h"
#include "gkm-attributes.h"
#include "gkm-object.h"
#include "gkm-trust.h"
#include "gkm-util.h"

#include "pkcs11/pkcs11i.h"
#include "pkcs11/pkcs11x.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_TRUST,
	PROP_TYPE,
	PROP_PURPOSE,
	PROP_PEER
};

struct _GkmAssertionPrivate {
	GkmTrust *trust;
	gulong type;
	gchar *purpose;
	gchar *peer;
};

G_DEFINE_TYPE (GkmAssertion, gkm_assertion, GKM_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */


/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_assertion_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmAssertion *self = GKM_ASSERTION (base);

	switch (attr->type)
	{
	case CKA_PRIVATE:
		return gkm_attribute_set_bool (attr, CK_FALSE);
	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_X_TRUST_ASSERTION);
	case CKA_MODIFIABLE:
		return gkm_attribute_set_bool (attr, CK_FALSE);

	/* Various trust flags */
	case CKA_X_ASSERTION_TYPE:
		return gkm_attribute_set_ulong (attr, self->pv->type);
	case CKA_X_PURPOSE:
		return gkm_attribute_set_string (attr, self->pv->purpose);
	case CKA_X_PEER:
		if (!self->pv->peer)
			return CKR_ATTRIBUTE_TYPE_INVALID;
		return gkm_attribute_set_string (attr, self->pv->peer);

	/* Certificate reference values */
	case CKA_SERIAL_NUMBER:
	case CKA_ISSUER:
	case CKA_X_CERTIFICATE_VALUE:
		return gkm_object_get_attribute (GKM_OBJECT (self->pv->trust), session, attr);

	default:
		break;
	};

	return GKM_OBJECT_CLASS (gkm_assertion_parent_class)->get_attribute (base, session, attr);
}

static GObject*
gkm_assertion_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmAssertion *self = GKM_ASSERTION (G_OBJECT_CLASS (gkm_assertion_parent_class)->constructor(type, n_props, props));

	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (self->pv->purpose, NULL);
	g_return_val_if_fail (self->pv->type, NULL);

	return G_OBJECT (self);
}

static void
gkm_assertion_init (GkmAssertion *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_ASSERTION, GkmAssertionPrivate);
}

static void
gkm_assertion_finalize (GObject *obj)
{
	GkmAssertion *self = GKM_ASSERTION (obj);

	if (self->pv->trust)
		g_object_remove_weak_pointer (G_OBJECT (self->pv->trust), (gpointer*)&(self->pv->trust));
	self->pv->trust = NULL;

	g_free (self->pv->purpose);
	self->pv->purpose = NULL;

	g_free (self->pv->peer);
	self->pv->peer = NULL;

	G_OBJECT_CLASS (gkm_assertion_parent_class)->finalize (obj);
}


static void
gkm_assertion_set_property (GObject *obj, guint prop_id, const GValue *value,
                            GParamSpec *pspec)
{
	GkmAssertion *self = GKM_ASSERTION (obj);

	switch (prop_id) {
	case PROP_TRUST:
		g_return_if_fail (!self->pv->trust);
		self->pv->trust = g_value_get_object (value);
		g_return_if_fail (self->pv->trust);
		g_object_add_weak_pointer (G_OBJECT (self->pv->trust), (gpointer*)&(self->pv->trust));
		break;
	case PROP_TYPE:
		self->pv->type = g_value_get_ulong (value);
		break;
	case PROP_PURPOSE:
		self->pv->purpose = g_value_dup_string (value);
		break;
	case PROP_PEER:
		self->pv->peer = g_value_dup_string (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_assertion_get_property (GObject *obj, guint prop_id, GValue *value,
                            GParamSpec *pspec)
{
	GkmAssertion *self = GKM_ASSERTION (obj);

	switch (prop_id) {
	case PROP_TRUST:
		g_value_set_object (value, gkm_assertion_get_trust_object (self));
		break;
	case PROP_TYPE:
		g_value_set_ulong (value, gkm_assertion_get_trust_type (self));
		break;
	case PROP_PURPOSE:
		g_value_set_string (value, gkm_assertion_get_purpose (self));
		break;
	case PROP_PEER:
		g_value_set_string (value, gkm_assertion_get_peer (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_assertion_class_init (GkmAssertionClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gobject_class->constructor = gkm_assertion_constructor;
	gobject_class->finalize = gkm_assertion_finalize;
	gobject_class->set_property = gkm_assertion_set_property;
	gobject_class->get_property = gkm_assertion_get_property;

	gkm_class->get_attribute = gkm_assertion_get_attribute;

	g_object_class_install_property (gobject_class, PROP_TRUST,
	         g_param_spec_object ("trust", "Trust", "Trust object this assertion belongs to",
	                              GKM_TYPE_TRUST, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_TYPE,
	         g_param_spec_ulong ("type", "Type", "PKCS#11 assertion type",
	                             0, G_MAXULONG, 0, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_PURPOSE,
	         g_param_spec_string ("purpose", "Purpose", "The purpose for the trust",
	                              NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_PEER,
	         g_param_spec_string ("peer", "Peer", "Optional peer this assertion applies to",
	                              NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_type_class_add_private (klass, sizeof (GkmAssertionPrivate));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmAssertion*
gkm_assertion_new (GkmTrust *trust, gulong type, const gchar *purpose, const gchar *peer)
{
	return g_object_new (GKM_TYPE_ASSERTION,
	                     "module", gkm_object_get_module (GKM_OBJECT (trust)),
	                     "manager", gkm_object_get_manager (GKM_OBJECT (trust)),
	                     "trust", trust,
	                     "type", type,
	                     "purpose", purpose,
	                     "peer", peer,
	                     NULL);
}

const gchar*
gkm_assertion_get_purpose (GkmAssertion *self)
{
	g_return_val_if_fail (GKM_IS_ASSERTION (self), NULL);
	return self->pv->purpose;
}

const gchar*
gkm_assertion_get_peer (GkmAssertion *self)
{
	g_return_val_if_fail (GKM_IS_ASSERTION (self), NULL);
	return self->pv->peer;
}

gulong
gkm_assertion_get_trust_type (GkmAssertion *self)
{
	g_return_val_if_fail (GKM_IS_ASSERTION (self), 0UL);
	return self->pv->type;
}

GkmTrust*
gkm_assertion_get_trust_object (GkmAssertion *self)
{
	g_return_val_if_fail (GKM_IS_ASSERTION (self), NULL);
	return self->pv->trust;
}
