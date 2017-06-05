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

#include "gkm-attributes.h"
#include "gkm-certificate.h"
#include "gkm-certificate-key.h"

#include "gkm-object.h"
#include "gkm-util.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_CERTIFICATE
};

struct _GkmCertificateKeyPrivate {
	GkmCertificate *certificate;
};

G_DEFINE_TYPE (GkmCertificateKey, gkm_certificate_key, GKM_TYPE_PUBLIC_XSA_KEY);

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_certificate_key_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmCertificateKey *self = GKM_CERTIFICATE_KEY (base);

	switch (attr->type) {
	case CKA_LABEL:
		if (self->pv->certificate)
			return gkm_object_get_attribute (GKM_OBJECT (self->pv->certificate), session, attr);
		return gkm_attribute_set_string (attr, "");
	}

	return GKM_OBJECT_CLASS (gkm_certificate_key_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_certificate_key_init (GkmCertificateKey *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_CERTIFICATE_KEY, GkmCertificateKeyPrivate);
}

static void
gkm_certificate_key_finalize (GObject *obj)
{
	GkmCertificateKey *self = GKM_CERTIFICATE_KEY (obj);

	if (self->pv->certificate)
		g_object_remove_weak_pointer (G_OBJECT (self->pv->certificate), (gpointer*)&(self->pv->certificate));
	self->pv->certificate = NULL;

	G_OBJECT_CLASS (gkm_certificate_key_parent_class)->finalize (obj);
}

static void
gkm_certificate_key_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	GkmCertificateKey *self = GKM_CERTIFICATE_KEY (obj);

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
gkm_certificate_key_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	GkmCertificateKey *self = GKM_CERTIFICATE_KEY (obj);

	switch (prop_id) {
	case PROP_CERTIFICATE:
		g_value_set_object (value, gkm_certificate_key_get_certificate (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_certificate_key_class_init (GkmCertificateKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gobject_class->finalize = gkm_certificate_key_finalize;
	gobject_class->set_property = gkm_certificate_key_set_property;
	gobject_class->get_property = gkm_certificate_key_get_property;

	gkm_class->get_attribute = gkm_certificate_key_get_attribute;

	g_type_class_add_private (klass, sizeof (GkmCertificateKeyPrivate));

	g_object_class_install_property (gobject_class, PROP_CERTIFICATE,
	           g_param_spec_object ("certificate", "Certificate", "Certificate this key belongs to",
	                                GKM_TYPE_CERTIFICATE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmCertificateKey*
gkm_certificate_key_new (GkmModule *module, GkmManager *manager, GkmCertificate *cert)
{
	return g_object_new (GKM_TYPE_CERTIFICATE_KEY, "module", module,
	                     "manager", manager, "certificate", cert, NULL);
}

GkmCertificate*
gkm_certificate_key_get_certificate (GkmCertificateKey *self)
{
	g_return_val_if_fail (GKM_IS_CERTIFICATE_KEY (self), NULL);
	g_return_val_if_fail (self->pv->certificate, NULL);
	return self->pv->certificate;
}
