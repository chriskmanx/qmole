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

#ifndef __GKM_SECRET_DATA_H__
#define __GKM_SECRET_DATA_H__

#include <glib-object.h>

#include "gkm-secret-types.h"

#include "gkm/gkm-types.h"

#define GKM_TYPE_SECRET_DATA               (gkm_secret_data_get_type ())
#define GKM_SECRET_DATA(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_SECRET_DATA, GkmSecretData))
#define GKM_SECRET_DATA_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_SECRET_DATA, GkmSecretDataClass))
#define GKM_IS_SECRET_DATA(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_SECRET_DATA))
#define GKM_IS_SECRET_DATA_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_SECRET_DATA))
#define GKM_SECRET_DATA_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_SECRET_DATA, GkmSecretDataClass))

typedef struct _GkmSecretDataClass GkmSecretDataClass;

struct _GkmSecretDataClass {
	GObjectClass parent_class;
};

GType                gkm_secret_data_get_type        (void);

GkmSecret*           gkm_secret_data_get_secret      (GkmSecretData *self,
                                                      const gchar *identifier);

const guchar*        gkm_secret_data_get_raw         (GkmSecretData *self,
                                                      const gchar *identifier,
                                                      gsize *n_result);

void                 gkm_secret_data_set_secret      (GkmSecretData *self,
                                                      const gchar *identifier,
                                                      GkmSecret *secret);

void                 gkm_secret_data_set_transacted  (GkmSecretData *self,
                                                      GkmTransaction *transaction,
                                                      const gchar *identifier,
                                                      GkmSecret *secret);

void                 gkm_secret_data_remove_secret   (GkmSecretData *self,
                                                      const gchar *identifier);

GkmSecret*           gkm_secret_data_get_master      (GkmSecretData *self);

void                 gkm_secret_data_set_master      (GkmSecretData *self,
                                                      GkmSecret *master);

#endif /* __GKM_SECRET_DATA_H__ */
