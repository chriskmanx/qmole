/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "mock-locked-object.h"

#include "gkm/gkm-attributes.h"
#include "gkm/gkm-credential.h"

G_DEFINE_TYPE (MockLockedObject, mock_locked_object, GKM_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

/* -----------------------------------------------------------------------------
 * KEY
 */

static CK_RV
mock_locked_object_real_get_attribute (GkmObject *base, GkmSession *session, CK_ATTRIBUTE* attr)
{
	switch (attr->type) {
	case CKA_CLASS:
		return gkm_attribute_set_ulong (attr, CKO_DATA);
	case CKA_ALWAYS_AUTHENTICATE:
		return gkm_attribute_set_bool (attr, TRUE);
	};

	return GKM_OBJECT_CLASS (mock_locked_object_parent_class)->get_attribute (base, session, attr);
}

static CK_RV
mock_locked_object_real_unlock (GkmObject *base, GkmCredential *auth)
{
	const gchar *password;
	gsize n_password;

	password = gkm_credential_get_password (auth, &n_password);
	if (n_password == 4 && memcmp (password, "mock", 4) == 0)
		return CKR_OK;

	return CKR_USER_NOT_LOGGED_IN;
}

static void
mock_locked_object_init (MockLockedObject *self)
{

}

static void
mock_locked_object_class_init (MockLockedObjectClass *klass)
{
	GkmObjectClass *gkm_class = GKM_OBJECT_CLASS (klass);
	mock_locked_object_parent_class = g_type_class_peek_parent (klass);
	gkm_class->get_attribute = mock_locked_object_real_get_attribute;
	gkm_class->unlock = mock_locked_object_real_unlock;
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmObject*
mock_locked_object_new (GkmModule *module, GkmManager *manager)
{
	return g_object_new (MOCK_TYPE_LOCKED_OBJECT,
	                     "module", module,
	                     "manager", manager,
	                     NULL);
}
