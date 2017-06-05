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
#include "gkm-dh-key.h"
#include "gkm-dh-mechanism.h"
#include "gkm-session.h"
#include "gkm-util.h"

struct _GkmDhKeyPrivate {
	gcry_mpi_t prime;
	gcry_mpi_t base;
	gpointer id;
	gsize n_id;
};

G_DEFINE_TYPE (GkmDhKey, gkm_dh_key, GKM_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

/* -----------------------------------------------------------------------------
 * PUBLIC_DH_KEY
 */

static CK_RV
gkm_dh_key_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE* attr)
{
	GkmDhKey *self = GKM_DH_KEY (base);

	switch (attr->type)
	{

	case CKA_KEY_TYPE:
		return gkm_attribute_set_ulong (attr, CKK_DH);

	case CKA_START_DATE:
	case CKA_END_DATE:
		return gkm_attribute_set_empty (attr);

	case CKA_LOCAL:
		return gkm_attribute_set_bool (attr, FALSE);

	case CKA_KEY_GEN_MECHANISM:
		return gkm_attribute_set_ulong (attr, CK_UNAVAILABLE_INFORMATION);

	case CKA_ALLOWED_MECHANISMS:
		return gkm_attribute_set_data (attr, (CK_VOID_PTR)GKM_DH_MECHANISMS,
		                               sizeof (GKM_DH_MECHANISMS));

	case CKA_ID:
		return gkm_attribute_set_data (attr, self->pv->id, self->pv->n_id);

	case CKA_SUBJECT:
		return gkm_attribute_set_empty (attr);

	case CKA_PRIME:
		return gkm_attribute_set_mpi (attr, self->pv->prime);

	case CKA_BASE:
		return gkm_attribute_set_mpi (attr, self->pv->base);
	};

	return GKM_OBJECT_CLASS (gkm_dh_key_parent_class)->get_attribute (base, session, attr);
}

static void
gkm_dh_key_init (GkmDhKey *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_DH_KEY, GkmDhKeyPrivate);
}

static void
gkm_dh_key_finalize (GObject *obj)
{
	GkmDhKey *self = GKM_DH_KEY (obj);

	gcry_mpi_release (self->pv->prime);
	self->pv->prime = NULL;

	gcry_mpi_release (self->pv->base);
	self->pv->base = NULL;

	g_free (self->pv->id);
	self->pv->id = NULL;
	self->pv->n_id = 0;

	G_OBJECT_CLASS (gkm_dh_key_parent_class)->finalize (obj);
}

static void
gkm_dh_key_class_init (GkmDhKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);

	gkm_dh_key_parent_class = g_type_class_peek_parent (klass);

	gobject_class->finalize = gkm_dh_key_finalize;

	gkm_class->get_attribute = gkm_dh_key_real_get_attribute;

	g_type_class_add_private (klass, sizeof (GkmDhKeyPrivate));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

void
gkm_dh_key_initialize (GkmDhKey *self, gcry_mpi_t prime, gcry_mpi_t base,
                       gpointer id, gsize n_id)
{
	g_return_if_fail (GKM_IS_DH_KEY (self));
	g_return_if_fail (base);
	g_return_if_fail (prime);
	g_return_if_fail (!self->pv->base);
	g_return_if_fail (!self->pv->prime);

	self->pv->base = base;
	self->pv->prime = prime;
	self->pv->id = id;
	self->pv->n_id = n_id;
}

gcry_mpi_t
gkm_dh_key_get_prime (GkmDhKey *self)
{
	g_return_val_if_fail (GKM_IS_DH_KEY (self), NULL);
	return self->pv->prime;
}
