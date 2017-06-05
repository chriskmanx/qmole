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

#include "gkm-ssh-public-key.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-module.h"
#include "gkm/gkm-object.h"
#include "gkm/gkm-util.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_LABEL
};

struct _GkmSshPublicKey {
	GkmPublicXsaKey parent;
	gchar *label;
};

G_DEFINE_TYPE (GkmSshPublicKey, gkm_ssh_public_key, GKM_TYPE_PUBLIC_XSA_KEY);

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_ssh_public_key_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmSshPublicKey *self = GKM_SSH_PUBLIC_KEY (base);

	switch (attr->type) {
	case CKA_LABEL:
		return gkm_attribute_set_string (attr, self->label ? self->label : "");
	}

	return GKM_OBJECT_CLASS (gkm_ssh_public_key_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_ssh_public_key_init (GkmSshPublicKey *self)
{

}

static void
gkm_ssh_public_key_finalize (GObject *obj)
{
	GkmSshPublicKey *self = GKM_SSH_PUBLIC_KEY (obj);

	g_free (self->label);
	self->label = NULL;

	G_OBJECT_CLASS (gkm_ssh_public_key_parent_class)->finalize (obj);
}

static void
gkm_ssh_public_key_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	GkmSshPublicKey *self = GKM_SSH_PUBLIC_KEY (obj);

	switch (prop_id) {
	case PROP_LABEL:
		gkm_ssh_public_key_set_label (self, g_value_get_string (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_ssh_public_key_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	GkmSshPublicKey *self = GKM_SSH_PUBLIC_KEY (obj);

	switch (prop_id) {
	case PROP_LABEL:
		g_value_set_string (value, gkm_ssh_public_key_get_label (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_ssh_public_key_class_init (GkmSshPublicKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gobject_class->finalize = gkm_ssh_public_key_finalize;
	gobject_class->set_property = gkm_ssh_public_key_set_property;
	gobject_class->get_property = gkm_ssh_public_key_get_property;

	gkm_class->get_attribute = gkm_ssh_public_key_get_attribute;

	g_object_class_install_property (gobject_class, PROP_LABEL,
	           g_param_spec_string ("label", "Label", "Object Label",
	                                "", G_PARAM_READWRITE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmSshPublicKey*
gkm_ssh_public_key_new (GkmModule *module, const gchar *unique)
{
	return g_object_new (GKM_TYPE_SSH_PUBLIC_KEY, "unique", unique,
	                     "module", module, "manager", gkm_module_get_manager (module), NULL);
}

const gchar*
gkm_ssh_public_key_get_label (GkmSshPublicKey *self)
{
	g_return_val_if_fail (GKM_IS_SSH_PUBLIC_KEY (self), NULL);
	return self->label;
}

void
gkm_ssh_public_key_set_label (GkmSshPublicKey *self, const gchar *label)
{
	g_return_if_fail (GKM_IS_SSH_PUBLIC_KEY (self));
	g_free (self->label);
	self->label = g_strdup (label);
	g_object_notify (G_OBJECT (self), "label");
}
