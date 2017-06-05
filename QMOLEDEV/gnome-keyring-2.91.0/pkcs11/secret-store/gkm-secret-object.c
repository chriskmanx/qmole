/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#include "gkm-secret-object.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-transaction.h"

#include "pkcs11/pkcs11i.h"

#include <glib/gi18n.h>

enum {
	PROP_0,
	PROP_LABEL,
	PROP_IDENTIFIER,
	PROP_CREATED,
	PROP_MODIFIED
};

struct _GkmSecretObjectPrivate {
	gchar *identifier;
	gchar *label;
	glong created;
	glong modified;
};

G_DEFINE_TYPE (GkmSecretObject, gkm_secret_object, GKM_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static gboolean
complete_set_label (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmSecretObject *self = GKM_SECRET_OBJECT (obj);
	gchar *old_label = user_data;

	if (gkm_transaction_get_failed (transaction)) {
		g_free (self->pv->label);
		self->pv->label = old_label;
	} else {
		gkm_object_notify_attribute (GKM_OBJECT (obj), CKA_LABEL);
		g_object_notify (G_OBJECT (obj), "label");
		gkm_secret_object_was_modified (self);
		g_free (old_label);
	}

	return TRUE;
}

static void
begin_set_label (GkmSecretObject *self, GkmTransaction *transaction, gchar *label)
{
	g_assert (GKM_IS_SECRET_OBJECT (self));
	g_assert (!gkm_transaction_get_failed (transaction));

	gkm_transaction_add (transaction, self, complete_set_label, self->pv->label);
	self->pv->label = label;
}

static gchar*
register_identifier (GkmSecretObjectClass *klass, const gchar *identifier)
{
	gchar *result;
	gint i;

	g_assert (klass);
	g_assert (identifier);

	if (!klass->identifiers)
		return g_strdup (identifier);

	for (i = 0; i < G_MAXINT; ++i) {
		if (i == 0)
			result = g_strdup (identifier);
		else
			result = g_strdup_printf ("%s_%d", identifier, i);
		if (g_hash_table_lookup (klass->identifiers, result)) {
			g_free (result);
		} else {
			g_hash_table_insert (klass->identifiers, result, result);
			return result;
		}
	}

	g_assert_not_reached ();
}

static void
unregister_identifier (GkmSecretObjectClass *klass, gchar *identifier)
{
	g_assert (klass);
	g_assert (identifier);

	if (klass->identifiers)
		g_hash_table_remove (klass->identifiers, identifier);
	g_free (identifier);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_secret_object_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	GkmSecretObject *self = GKM_SECRET_OBJECT (base);

	switch (attr->type) {
	case CKA_MODIFIABLE:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_ID:
		return gkm_attribute_set_string (attr, gkm_secret_object_get_identifier (self));

	case CKA_LABEL:
		return gkm_attribute_set_string (attr, gkm_secret_object_get_label (self));

	case CKA_G_LOCKED:
		return gkm_attribute_set_bool (attr, gkm_secret_object_is_locked (self, session));

	case CKA_G_CREATED:
		return gkm_attribute_set_time (attr, gkm_secret_object_get_created (self));

	case CKA_G_MODIFIED:
		return gkm_attribute_set_time (attr, gkm_secret_object_get_modified (self));
	}

	return GKM_OBJECT_CLASS (gkm_secret_object_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_secret_object_set_attribute (GkmObject *base, GkmSession *session,
                                 GkmTransaction *transaction, CK_ATTRIBUTE_PTR attr)
{
	GkmSecretObject *self = GKM_SECRET_OBJECT (base);
	gchar *label;
	CK_RV rv;

	switch (attr->type) {

	case CKA_LABEL:
		/* Check that the object is not locked */
		if (gkm_secret_object_is_locked (self, session))
			rv = CKR_USER_NOT_LOGGED_IN;
		else
			rv = gkm_attribute_get_string (attr, &label);
		if (rv != CKR_OK)
			gkm_transaction_fail (transaction, rv);
		else
			begin_set_label (self, transaction, label);
		return;
	}

	GKM_OBJECT_CLASS (gkm_secret_object_parent_class)->set_attribute (base, session, transaction, attr);
}

static gboolean
gkm_secret_object_real_is_locked (GkmSecretObject *self, GkmSession *session)
{
	/* Derived classes override us */
	return FALSE;
}

static void
gkm_secret_object_init (GkmSecretObject *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_SECRET_OBJECT, GkmSecretObjectPrivate);
}

static GObject*
gkm_secret_object_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmSecretObject *self = GKM_SECRET_OBJECT (G_OBJECT_CLASS (gkm_secret_object_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	/* Must be created with an identifier */
	g_return_val_if_fail (self->pv->identifier, NULL);

	return G_OBJECT (self);
}

static void
gkm_secret_object_set_property (GObject *obj, guint prop_id, const GValue *value,
                                GParamSpec *pspec)
{
	GkmSecretObjectClass *klass = GKM_SECRET_OBJECT_GET_CLASS (obj);
	GkmSecretObject *self = GKM_SECRET_OBJECT (obj);
	const gchar *identifier;

	switch (prop_id) {
	case PROP_LABEL:
		gkm_secret_object_set_label (self, g_value_get_string (value));
		break;
	case PROP_IDENTIFIER:
		g_return_if_fail (!self->pv->identifier);
		identifier = g_value_get_string (value);
		g_return_if_fail (identifier);
		self->pv->identifier = register_identifier (klass, identifier);
		break;
	case PROP_CREATED:
		gkm_secret_object_set_created (self, g_value_get_long (value));
		break;
	case PROP_MODIFIED:
		gkm_secret_object_set_modified (self, g_value_get_long (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_secret_object_get_property (GObject *obj, guint prop_id, GValue *value,
                                    GParamSpec *pspec)
{
	GkmSecretObject *self = GKM_SECRET_OBJECT (obj);

	switch (prop_id) {
	case PROP_LABEL:
		g_value_set_string (value, gkm_secret_object_get_label (self));
		break;
	case PROP_IDENTIFIER:
		g_value_set_string (value, gkm_secret_object_get_identifier (self));
		break;
	case PROP_CREATED:
		g_value_set_long (value, gkm_secret_object_get_created (self));
		break;
	case PROP_MODIFIED:
		g_value_set_long (value, gkm_secret_object_get_modified (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_secret_object_finalize (GObject *obj)
{
	GkmSecretObjectClass *klass = GKM_SECRET_OBJECT_GET_CLASS (obj);
	GkmSecretObject *self = GKM_SECRET_OBJECT (obj);

	if (self->pv->identifier)
		unregister_identifier (klass, self->pv->identifier);
	self->pv->identifier = NULL;

	g_free (self->pv->label);
	self->pv->label = NULL;

	self->pv->created = 0;
	self->pv->modified = 0;

	G_OBJECT_CLASS (gkm_secret_object_parent_class)->finalize (obj);
}

static void
gkm_secret_object_class_init (GkmSecretObjectClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gkm_secret_object_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GkmSecretObjectPrivate));

	gobject_class->constructor = gkm_secret_object_constructor;
	gobject_class->finalize = gkm_secret_object_finalize;
	gobject_class->set_property = gkm_secret_object_set_property;
	gobject_class->get_property = gkm_secret_object_get_property;

	gkm_class->get_attribute = gkm_secret_object_get_attribute;
	gkm_class->set_attribute = gkm_secret_object_set_attribute;

	klass->is_locked = gkm_secret_object_real_is_locked;

	g_object_class_install_property (gobject_class, PROP_IDENTIFIER,
	           g_param_spec_string ("identifier", "Identifier", "Object Identifier",
	                                NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_LABEL,
	           g_param_spec_string ("label", "Label", "Object Label",
	                                "", G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (gobject_class, PROP_CREATED,
	           g_param_spec_long ("created", "Created", "Object Create Time",
	                              0, G_MAXLONG, 0, G_PARAM_READABLE));

	g_object_class_install_property (gobject_class, PROP_MODIFIED,
	           g_param_spec_long ("modified", "Modified", "Object Modify Time",
	                              0, G_MAXLONG, 0, G_PARAM_READABLE));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

void
gkm_secret_object_class_unique_identifiers (GkmSecretObjectClass *klass)
{
	if (!klass->identifiers)
		klass->identifiers = g_hash_table_new (g_str_hash, g_str_equal);
}

const gchar*
gkm_secret_object_get_identifier (GkmSecretObject *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_OBJECT (self), NULL);
	return self->pv->identifier;
}

const gchar*
gkm_secret_object_get_label (GkmSecretObject *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_OBJECT (self), NULL);
	return self->pv->label;
}

void
gkm_secret_object_set_label (GkmSecretObject *self, const gchar *label)
{
	g_return_if_fail (GKM_IS_SECRET_OBJECT (self));

	if (self->pv->label == label)
		return;

	g_free (self->pv->label);
	self->pv->label = g_strdup (label);
	g_object_notify (G_OBJECT (self), "label");
}

glong
gkm_secret_object_get_created (GkmSecretObject *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_OBJECT (self), 0);
	return self->pv->created;
}

void
gkm_secret_object_set_created (GkmSecretObject *self, glong when)
{
	g_return_if_fail (GKM_IS_SECRET_OBJECT (self));
	self->pv->created = when;
	g_object_notify (G_OBJECT (self), "created");
}

glong
gkm_secret_object_get_modified (GkmSecretObject *self)
{
	g_return_val_if_fail (GKM_IS_SECRET_OBJECT (self), 0);
	return self->pv->modified;
}

void
gkm_secret_object_set_modified (GkmSecretObject *self, glong when)
{
	g_return_if_fail (GKM_IS_SECRET_OBJECT (self));
	self->pv->modified = when;
	g_object_notify (G_OBJECT (self), "modified");
}

void
gkm_secret_object_was_modified (GkmSecretObject *self)
{
	GTimeVal tv;
	g_return_if_fail (GKM_IS_SECRET_OBJECT (self));
	g_get_current_time (&tv);
	gkm_secret_object_set_modified (self, tv.tv_sec);
}

gboolean
gkm_secret_object_is_locked (GkmSecretObject *self, GkmSession *session)
{
	g_return_val_if_fail (GKM_IS_SECRET_OBJECT (self), TRUE);
	g_return_val_if_fail (GKM_SECRET_OBJECT_GET_CLASS (self)->is_locked, TRUE);
	return GKM_SECRET_OBJECT_GET_CLASS (self)->is_locked (self, session);
}
