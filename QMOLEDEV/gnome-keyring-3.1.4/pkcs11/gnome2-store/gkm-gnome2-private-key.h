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

#ifndef __GKM_GNOME2_PRIVATE_KEY_H__
#define __GKM_GNOME2_PRIVATE_KEY_H__

#include <glib-object.h>

#include "gkm-gnome2-private-key.h"

#include "gkm/gkm-secret.h"
#include "gkm/gkm-private-xsa-key.h"

#define GKM_FACTORY_GNOME2_PRIVATE_KEY            (gkm_gnome2_private_key_get_factory ())

#define GKM_TYPE_GNOME2_PRIVATE_KEY               (gkm_gnome2_private_key_get_type ())
#define GKM_GNOME2_PRIVATE_KEY(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_GNOME2_PRIVATE_KEY, GkmGnome2PrivateKey))
#define GKM_GNOME2_PRIVATE_KEY_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_GNOME2_PRIVATE_KEY, GkmGnome2PrivateKeyClass))
#define GKM_IS_GNOME2_PRIVATE_KEY(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_GNOME2_PRIVATE_KEY))
#define GKM_IS_GNOME2_PRIVATE_KEY_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_GNOME2_PRIVATE_KEY))
#define GKM_GNOME2_PRIVATE_KEY_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_GNOME2_PRIVATE_KEY, GkmGnome2PrivateKeyClass))

typedef struct _GkmGnome2PrivateKey GkmGnome2PrivateKey;
typedef struct _GkmGnome2PrivateKeyClass GkmGnome2PrivateKeyClass;

struct _GkmGnome2PrivateKeyClass {
	GkmPrivateXsaKeyClass parent_class;
};

GType               gkm_gnome2_private_key_get_type               (void);

GkmFactory*         gkm_gnome2_private_key_get_factory            (void);

#endif /* __GKM_GNOME2_PRIVATE_KEY_H__ */
