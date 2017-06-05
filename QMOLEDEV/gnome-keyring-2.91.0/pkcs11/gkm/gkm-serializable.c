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

#include "gkm-serializable.h"

static void
gkm_serializable_base_init (gpointer gobject_class)
{
	static gboolean initialized = FALSE;
	if (!initialized) {
		/* Add properties and signals to the interface */


		initialized = TRUE;
	}
}

GType
gkm_serializable_get_type (void)
{
	static volatile gsize type_id__volatile = 0;

	if (g_once_init_enter (&type_id__volatile)) {
		static const GTypeInfo info = {
			sizeof (GkmSerializableIface),
			gkm_serializable_base_init,               /* base init */
			NULL,             /* base finalize */
			NULL,             /* class_init */
			NULL,             /* class finalize */
			NULL,             /* class data */
			0,
			0,                /* n_preallocs */
			NULL,             /* instance init */
		};

		GType type_id = g_type_register_static (G_TYPE_INTERFACE, "GkmSerializableIface", &info, 0);
		g_type_interface_add_prerequisite (type_id, G_TYPE_OBJECT);

		g_once_init_leave (&type_id__volatile, type_id);
	}

	return type_id__volatile;
}

gboolean
gkm_serializable_load (GkmSerializable *self, GkmSecret *login, const guchar *data, gsize n_data)
{
	g_return_val_if_fail (GKM_IS_SERIALIZABLE (self), FALSE);
	g_return_val_if_fail (GKM_SERIALIZABLE_GET_INTERFACE (self)->load, FALSE);
	return GKM_SERIALIZABLE_GET_INTERFACE (self)->load (self, login, data, n_data);
}

gboolean
gkm_serializable_save (GkmSerializable *self, GkmSecret *login, guchar **data, gsize *n_data)
{
	g_return_val_if_fail (GKM_IS_SERIALIZABLE (self), FALSE);
	g_return_val_if_fail (GKM_SERIALIZABLE_GET_INTERFACE (self)->save, FALSE);
	return GKM_SERIALIZABLE_GET_INTERFACE (self)->save (self, login, data, n_data);
}
