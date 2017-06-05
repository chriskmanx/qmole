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

#include "gkd-secret-lock.h"
#include "gkd-secret-service.h"

#include "egg/egg-error.h"

#include "pkcs11/pkcs11i.h"

#include <gck/gck.h>

gboolean
gkd_secret_lock (GckObject *collection, DBusError *derr)
{
	GError *error = NULL;
	GList *objects, *l;
	GckAttributes *atts;
	GckSession *session;

	atts = gck_attributes_new ();
	gck_attributes_add_ulong (atts, CKA_CLASS, CKO_G_CREDENTIAL);
	gck_attributes_add_ulong (atts, CKA_G_OBJECT, gck_object_get_handle (collection));

	session = gck_object_get_session (collection);
	g_return_val_if_fail (session, FALSE);

	objects = gck_session_find_objects (session, atts, NULL, &error);

	gck_attributes_unref (atts);
	g_object_unref (session);

	if (error != NULL) {
		g_warning ("couldn't search for credential objects: %s", egg_error_message (error));
		dbus_set_error (derr, DBUS_ERROR_FAILED, "Couldn't lock collection");
		g_clear_error (&error);
		return FALSE;
	}

	for (l = objects; l; l = g_list_next (l)) {
		if (!gck_object_destroy (l->data, NULL, &error)) {
			g_warning ("couldn't destroy credential object: %s", egg_error_message (error));
			g_clear_error (&error);
		}
	}

	gck_list_unref_free (objects);
	return TRUE;
}
