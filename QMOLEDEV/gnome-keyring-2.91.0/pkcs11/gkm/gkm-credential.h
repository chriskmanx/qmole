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

#ifndef __GKM_CREDENTIAL_H__
#define __GKM_CREDENTIAL_H__

#include <glib-object.h>

#include "gkm-object.h"
#include "gkm-types.h"

#define GKM_FACTORY_CREDENTIAL            (gkm_credential_get_factory ())

#define GKM_TYPE_CREDENTIAL               (gkm_credential_get_type ())
#define GKM_CREDENTIAL(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKM_TYPE_CREDENTIAL, GkmCredential))
#define GKM_CREDENTIAL_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKM_TYPE_CREDENTIAL, GkmCredentialClass))
#define GKM_IS_CREDENTIAL(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKM_TYPE_CREDENTIAL))
#define GKM_IS_CREDENTIAL_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKM_TYPE_CREDENTIAL))
#define GKM_CREDENTIAL_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKM_TYPE_CREDENTIAL, GkmCredentialClass))

typedef struct _GkmCredentialClass GkmCredentialClass;
typedef struct _GkmCredentialPrivate GkmCredentialPrivate;

struct _GkmCredential {
	GkmObject parent;
	GkmCredentialPrivate *pv;
};

struct _GkmCredentialClass {
	GkmObjectClass parent_class;
};

GType                      gkm_credential_get_type               (void);

GkmFactory*                gkm_credential_get_factory            (void);

CK_RV                      gkm_credential_create                 (GkmModule *module,
                                                                  GkmManager *manager,
                                                                  GkmObject *object,
                                                                  CK_UTF8CHAR_PTR pin,
                                                                  CK_ULONG n_pin,
                                                                  GkmCredential **result);

void                       gkm_credential_connect                (GkmCredential *self,
                                                                  GkmObject *object);

GkmSecret*                 gkm_credential_get_secret             (GkmCredential *self);

void                       gkm_credential_set_secret             (GkmCredential *self,
                                                                  GkmSecret *login);

const gchar*               gkm_credential_get_password           (GkmCredential *self,
                                                                  gsize *n_password);

GkmObject*                 gkm_credential_get_object             (GkmCredential *self);

gpointer                   gkm_credential_peek_data              (GkmCredential *self,
                                                                  GType type);

gpointer                   gkm_credential_pop_data               (GkmCredential *self,
                                                                  GType type);

void                       gkm_credential_set_data               (GkmCredential *self,
                                                                  GType type,
                                                                  gpointer data);

typedef gboolean           (*GkmCredentialFunc)                  (GkmCredential *cred,
                                                                  GkmObject *object,
                                                                  gpointer user_data);


gboolean                   gkm_credential_for_each               (GkmSession *self,
                                                                  GkmObject *object,
                                                                  GkmCredentialFunc func,
                                                                  gpointer user_data);

#endif /* __GKM_CREDENTIAL_H__ */
