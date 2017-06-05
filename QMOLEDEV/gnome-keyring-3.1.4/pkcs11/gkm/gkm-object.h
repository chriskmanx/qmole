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

#ifndef __GKM_OBJECT_H__
#define __GKM_OBJECT_H__

#include <glib-object.h>

#include "pkcs11/pkcs11.h"

#include "gkm-types.h"

#define GKM_TYPE_OBJECT               (gkm_object_get_type ())
#define GKM_OBJECT(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_OBJECT, GkmObject))
#define GKM_OBJECT_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_OBJECT, GkmObjectClass))
#define GKM_IS_OBJECT(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_OBJECT))
#define GKM_IS_OBJECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_OBJECT))
#define GKM_OBJECT_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_OBJECT, GkmObjectClass))

typedef struct _GkmObjectClass GkmObjectClass;
typedef struct _GkmObjectPrivate GkmObjectPrivate;

struct _GkmObject {
	GObject parent;
	GkmObjectPrivate *pv;
};

struct _GkmObjectClass {
	GObjectClass parent_class;

	/* signals ------------------------------------------------------------------ */

	void (*expose_object) (GkmObject *object, gboolean exposed);

	void (*notify_attribute) (GkmObject *object, CK_ATTRIBUTE_TYPE attr_type);

	/* virtual methods  --------------------------------------------------------- */

	CK_RV (*get_attribute) (GkmObject *object, GkmSession *session,
	                        CK_ATTRIBUTE *attr);

	void (*set_attribute) (GkmObject *object, GkmSession *session,
	                       GkmTransaction *transaction, CK_ATTRIBUTE *attr);

	void (*create_attributes) (GkmObject *object, GkmSession *session,
	                           GkmTransaction *transaction, CK_ATTRIBUTE *attrs, CK_ULONG n_attrs);

	CK_RV (*unlock) (GkmObject *self, GkmCredential *cred);
};

GType                  gkm_object_get_type               (void);

CK_OBJECT_HANDLE       gkm_object_get_handle             (GkmObject *self);

void                   gkm_object_set_handle             (GkmObject *self,
                                                          CK_OBJECT_HANDLE handle);

GkmModule*             gkm_object_get_module             (GkmObject *self);

GkmManager*            gkm_object_get_manager            (GkmObject *self);

const gchar*           gkm_object_get_unique             (GkmObject *self);

gboolean               gkm_object_is_token               (GkmObject *self);

gboolean               gkm_object_is_transient           (GkmObject *self);

void                   gkm_object_mark_used              (GkmObject *self);

CK_RV                  gkm_object_unlock                 (GkmObject *self,
                                                          GkmCredential *cred);

void                   gkm_object_destroy                (GkmObject *self,
                                                          GkmTransaction *transaction);

gboolean               gkm_object_is_exposed             (GkmObject *self);

void                   gkm_object_expose                 (GkmObject *self,
                                                          gboolean expose);

void                   gkm_object_expose_full            (GkmObject *self,
                                                          GkmTransaction *transaction,
                                                          gboolean expose);

gboolean               gkm_object_match                  (GkmObject *self,
                                                          GkmSession *session,
                                                          CK_ATTRIBUTE_PTR attr);

gboolean               gkm_object_match_all              (GkmObject *self,
                                                          GkmSession *session,
                                                          CK_ATTRIBUTE_PTR attrs,
                                                          CK_ULONG n_attrs);

CK_RV                  gkm_object_get_attribute          (GkmObject *self,
                                                          GkmSession *session,
                                                          CK_ATTRIBUTE_PTR attr);

void                   gkm_object_set_attribute          (GkmObject *self,
                                                          GkmSession *session,
                                                          GkmTransaction *transaction,
                                                          CK_ATTRIBUTE_PTR attr);

void                   gkm_object_create_attributes      (GkmObject *self,
                                                          GkmSession *session,
                                                          GkmTransaction *transaction,
                                                          CK_ATTRIBUTE_PTR attrs,
                                                          CK_ULONG n_attrs);

void                   gkm_object_notify_attribute       (GkmObject *self,
                                                          CK_ATTRIBUTE_TYPE attr_type);

gboolean               gkm_object_get_attribute_boolean  (GkmObject *self,
                                                          GkmSession *session,
                                                          CK_ATTRIBUTE_TYPE type,
                                                          gboolean *value);

gboolean               gkm_object_get_attribute_ulong    (GkmObject *self,
                                                          GkmSession *session,
                                                          CK_ATTRIBUTE_TYPE type,
                                                          gulong *value);

void*                  gkm_object_get_attribute_data     (GkmObject *self,
                                                          GkmSession *session,
                                                          CK_ATTRIBUTE_TYPE type,
                                                          gsize *n_data);

gboolean               gkm_object_has_attribute_ulong    (GkmObject *self,
                                                          GkmSession *session,
                                                          CK_ATTRIBUTE_TYPE type,
                                                          gulong value);

gboolean               gkm_object_has_attribute_boolean  (GkmObject *self,
                                                          GkmSession *session,
                                                          CK_ATTRIBUTE_TYPE type,
                                                          gboolean value);

#endif /* __GKM_OBJECT_H__ */
