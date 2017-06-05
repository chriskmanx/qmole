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

#ifndef __GKM_SECRET_OBJECT_H__
#define __GKM_SECRET_OBJECT_H__

#include "gkm-secret-types.h"

#include "gkm/gkm-object.h"

#include <glib-object.h>

#define GKM_TYPE_SECRET_OBJECT               (gkm_secret_object_get_type ())
#define GKM_SECRET_OBJECT(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_SECRET_OBJECT, GkmSecretObject))
#define GKM_SECRET_OBJECT_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_SECRET_OBJECT, GkmSecretObjectClass))
#define GKM_IS_SECRET_OBJECT(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_SECRET_OBJECT))
#define GKM_IS_SECRET_OBJECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_SECRET_OBJECT))
#define GKM_SECRET_OBJECT_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_SECRET_OBJECT, GkmSecretObjectClass))

typedef struct _GkmSecretObjectClass GkmSecretObjectClass;
typedef struct _GkmSecretObjectPrivate GkmSecretObjectPrivate;

struct _GkmSecretObject {
	GkmObject parent;
	GkmSecretObjectPrivate *pv;
};
struct _GkmSecretObjectClass {
	GkmObjectClass parent_class;
	GHashTable *identifiers;

	gboolean (*is_locked) (GkmSecretObject *self, GkmSession *session);

};

GType                gkm_secret_object_get_type        (void);

const gchar*         gkm_secret_object_get_identifier  (GkmSecretObject *self);

const gchar*         gkm_secret_object_get_label       (GkmSecretObject *self);

void                 gkm_secret_object_set_label       (GkmSecretObject *self,
                                                        const gchar *label);

glong                gkm_secret_object_get_created     (GkmSecretObject *self);

void                 gkm_secret_object_set_created     (GkmSecretObject *self,
                                                        glong value);

glong                gkm_secret_object_get_modified    (GkmSecretObject *self);

void                 gkm_secret_object_set_modified    (GkmSecretObject *self,
                                                        glong value);

void                 gkm_secret_object_was_modified    (GkmSecretObject *self);

gboolean             gkm_secret_object_is_locked       (GkmSecretObject *self,
                                                        GkmSession *session);

void       gkm_secret_object_class_unique_identifiers  (GkmSecretObjectClass *klass);

gchar*     gkm_secret_object_anonymous_identifier      (void);

#endif /* __GKM_SECRET_OBJECT_H__ */
