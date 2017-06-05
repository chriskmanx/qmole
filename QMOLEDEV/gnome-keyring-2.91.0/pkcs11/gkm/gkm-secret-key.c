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

#include "pkcs11/pkcs11.h"

#include "gkm-attributes.h"
#include "gkm-crypto.h"
#include "gkm-secret-key.h"
#include "gkm-session.h"
#include "gkm-util.h"

struct _GkmSecretKeyPrivate {
	gpointer id;
	gsize n_id;
};

G_DEFINE_TYPE (GkmSecretKey, gkm_secret_key, GKM_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

/* -----------------------------------------------------------------------------
 * PUBLIC_SECRET_KEY
 */

static CK_RV
gkm_secret_key_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE* attr)
{
	GkmSecretKey *self = GKM_SECRET_KEY (base);

	switch (attr->type)
	{
	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_SECRET_KEY);

	case CKA_SENSITIVE:
	case CKA_ENCRYPT:
	case CKA_DECRYPT:
	case CKA_SIGN:
	case CKA_VERIFY:
	case CKA_WRAP:
	case CKA_UNWRAP:
	case CKA_DERIVE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_EXTRACTABLE:
		return gkm_attribute_set_bool (attr, TRUE);

	case CKA_ALWAYS_SENSITIVE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_NEVER_EXTRACTABLE:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_WRAP_WITH_TRUSTED:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_TRUSTED:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_WRAP_TEMPLATE:
	case CKA_UNWRAP_TEMPLATE:
		return CKR_ATTRIBUTE_TYPE_INVALID;

	case CKA_START_DATE:
	case CKA_END_DATE:
		return gkm_attribute_set_empty (attr);

	case CKA_LOCAL:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_ID:
		return gkm_attribute_set_data (attr, self->pv->id, self->pv->n_id);

	case CKA_KEY_GEN_MECHANISM:
		return gkm_attribute_set_ulong (attr, CK_UNAVAILABLE_INFORMATION);
	};

	return GKM_OBJECT_CLASS (gkm_secret_key_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_secret_key_real_create_attributes (GkmObject *object, GkmSession *session, GkmTransaction *transaction,
                                       CK_ATTRIBUTE *attrs, CK_ULONG n_attrs)
{
	GkmSecretKey *self = GKM_SECRET_KEY (object);
	CK_ATTRIBUTE_PTR id;

	if (!self->pv->n_id) {
		id = gkm_attributes_find (attrs, n_attrs, CKA_ID);
		if (id == NULL) {
			self->pv->id = NULL;
			self->pv->n_id = 0;
		} else {
			self->pv->id = g_memdup (id->pValue, id->ulValueLen);
			self->pv->n_id = id->ulValueLen;
			gkm_attribute_consume (id);
		}
	}
}

static void
gkm_secret_key_init (GkmSecretKey *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_SECRET_KEY, GkmSecretKeyPrivate);
}

static void
gkm_secret_key_finalize (GObject *obj)
{
	GkmSecretKey *self = GKM_SECRET_KEY (obj);

	g_free (self->pv->id);
	self->pv->id = NULL;
	self->pv->n_id = 0;

	G_OBJECT_CLASS (gkm_secret_key_parent_class)->finalize (obj);
}

static void
gkm_secret_key_class_init (GkmSecretKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gkm_secret_key_parent_class = g_type_class_peek_parent (klass);

	gobject_class->finalize = gkm_secret_key_finalize;

	gkm_class->get_attribute = gkm_secret_key_real_get_attribute;
	gkm_class->create_attributes = gkm_secret_key_real_create_attributes;

	g_type_class_add_private (klass, sizeof (GkmSecretKeyPrivate));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */
