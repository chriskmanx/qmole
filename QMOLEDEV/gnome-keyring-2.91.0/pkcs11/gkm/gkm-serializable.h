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

#ifndef __GKM_SERIALIZABLE_H__
#define __GKM_SERIALIZABLE_H__

#include <glib-object.h>

#include "gkm-types.h"

G_BEGIN_DECLS

#define GKM_TYPE_SERIALIZABLE                 (gkm_serializable_get_type())
#define GKM_SERIALIZABLE(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_SERIALIZABLE, GkmSerializable))
#define GKM_IS_SERIALIZABLE(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_SERIALIZABLE))
#define GKM_SERIALIZABLE_GET_INTERFACE(inst)  (G_TYPE_INSTANCE_GET_INTERFACE ((inst), GKM_TYPE_SERIALIZABLE, GkmSerializableIface))

typedef struct _GkmSerializable      GkmSerializable;
typedef struct _GkmSerializableIface GkmSerializableIface;

struct _GkmSerializableIface {
	GTypeInterface parent;

	const gchar *extension;

	gboolean (*load) (GkmSerializable *self, GkmSecret *login, const guchar *data, gsize n_data);

	gboolean (*save) (GkmSerializable *self, GkmSecret *login, guchar **data, gsize *n_data);
};

GType                  gkm_serializable_get_type                          (void) G_GNUC_CONST;

gboolean               gkm_serializable_load                              (GkmSerializable *self,
                                                                           GkmSecret *login,
                                                                           const guchar *data,
                                                                           gsize n_data);

gboolean                gkm_serializable_save                             (GkmSerializable *self,
                                                                           GkmSecret *login,
                                                                           guchar** data,
                                                                           gsize *n_data);

G_END_DECLS

#endif /* __GKM_SERIALIZABLE_H__ */
