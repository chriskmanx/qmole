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

#include "gkm-roots-certificate.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-certificate-trust.h"
#include "gkm/gkm-manager.h"
#include "gkm/gkm-module.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-sexp.h"
#include "gkm/gkm-util.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_PATH,
	PROP_NETSCAPE_TRUST,
};

struct _GkmRootsCertificate {
	GkmCertificate parent;
	GkmCertificateTrust *trust;
	gchar *path;
};

G_DEFINE_TYPE (GkmRootsCertificate, gkm_roots_certificate, GKM_TYPE_CERTIFICATE);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */


/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_roots_certificate_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmRootsCertificate *self = GKM_ROOTS_CERTIFICATE (base);
	CK_ULONG category;

	switch (attr->type) {
	case CKA_TRUSTED:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_CERTIFICATE_CATEGORY:
		if (!gkm_certificate_calc_category (GKM_CERTIFICATE (self), session, &category))
			return CKR_FUNCTION_FAILED;
		/* Unknown category, is CA by default in this slot */
		if (category == 0)
			category = 2;
		return gkm_attribute_set_ulong (attr, category);
	}

	return GKM_OBJECT_CLASS (gkm_roots_certificate_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_roots_certificate_expose_object (GkmObject *obj, gboolean expose)
{
	GKM_OBJECT_CLASS (gkm_roots_certificate_parent_class)->expose_object (obj, expose);
	gkm_object_expose (GKM_OBJECT (GKM_ROOTS_CERTIFICATE (obj)->trust), expose);
}

static void
gkm_roots_certificate_init (GkmRootsCertificate *self)
{

}

static GObject*
gkm_roots_certificate_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmRootsCertificate *self = GKM_ROOTS_CERTIFICATE (G_OBJECT_CLASS (gkm_roots_certificate_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	self->trust = gkm_certificate_trust_new (gkm_object_get_module (GKM_OBJECT (self)),
	                                         gkm_object_get_manager (GKM_OBJECT (self)),
	                                         GKM_CERTIFICATE (self));

	return G_OBJECT (self);
}

static void
gkm_roots_certificate_set_property (GObject *obj, guint prop_id, const GValue *value,
                                    GParamSpec *pspec)
{
	GkmRootsCertificate *self = GKM_ROOTS_CERTIFICATE (obj);

	switch (prop_id) {
	case PROP_PATH:
		g_return_if_fail (!self->path);
		self->path = g_value_dup_string (value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_roots_certificate_get_property (GObject *obj, guint prop_id, GValue *value,
                                    GParamSpec *pspec)
{
	GkmRootsCertificate *self = GKM_ROOTS_CERTIFICATE (obj);

	switch (prop_id) {
	case PROP_PATH:
		g_value_set_string (value, gkm_roots_certificate_get_path (self));
		break;
	case PROP_NETSCAPE_TRUST:
		g_value_set_object (value, gkm_roots_certificate_get_netscape_trust (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_roots_certificate_dispose (GObject *obj)
{
	GkmRootsCertificate *self = GKM_ROOTS_CERTIFICATE (obj);

	if (self->trust)
		g_object_unref (self->trust);
	self->trust = NULL;

	G_OBJECT_CLASS (gkm_roots_certificate_parent_class)->dispose (obj);
}

static void
gkm_roots_certificate_finalize (GObject *obj)
{
	GkmRootsCertificate *self = GKM_ROOTS_CERTIFICATE (obj);

	g_free (self->path);
	g_assert (!self->trust);

	G_OBJECT_CLASS (gkm_roots_certificate_parent_class)->finalize (obj);
}

static void
gkm_roots_certificate_class_init (GkmRootsCertificateClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gkm_roots_certificate_parent_class = g_type_class_peek_parent (klass);

	gobject_class->constructor = gkm_roots_certificate_constructor;
	gobject_class->dispose = gkm_roots_certificate_dispose;
	gobject_class->finalize = gkm_roots_certificate_finalize;
	gobject_class->set_property = gkm_roots_certificate_set_property;
	gobject_class->get_property = gkm_roots_certificate_get_property;

	gkm_class->get_attribute = gkm_roots_certificate_get_attribute;
	gkm_class->expose_object = gkm_roots_certificate_expose_object;

	g_object_class_install_property (gobject_class, PROP_PATH,
	           g_param_spec_string ("path", "Path", "Certificate origin path",
	                                "", G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_NETSCAPE_TRUST,
	           g_param_spec_object ("netscape-trust", "Netscape Trust", "Netscape trust object",
	                                GKM_TYPE_CERTIFICATE_TRUST, G_PARAM_READABLE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmRootsCertificate*
gkm_roots_certificate_new (GkmModule *module, const gchar *unique, const gchar *path)
{
	return g_object_new (GKM_TYPE_ROOTS_CERTIFICATE, "unique", unique, "path", path,
	                     "module", module, "manager", gkm_module_get_manager (module), NULL);
}

const gchar*
gkm_roots_certificate_get_path (GkmRootsCertificate *self)
{
	g_return_val_if_fail (GKM_IS_ROOTS_CERTIFICATE (self), "");
	return self->path;
}

GkmCertificateTrust*
gkm_roots_certificate_get_netscape_trust (GkmRootsCertificate *self)
{
	g_return_val_if_fail (GKM_IS_ROOTS_CERTIFICATE (self), NULL);
	g_return_val_if_fail (GKM_IS_CERTIFICATE_TRUST (self->trust), NULL);
	return self->trust;
}
