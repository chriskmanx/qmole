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

#ifndef __GKD_SECRET_CREATE_H__
#define __GKD_SECRET_CREATE_H__

#include <glib-object.h>

#include "gkd-secret-prompt.h"
#include "gkd-secret-types.h"

#include "gck/gck.h"

#define GKD_SECRET_TYPE_CREATE               (gkd_secret_create_get_type ())
#define GKD_SECRET_CREATE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKD_SECRET_TYPE_CREATE, GkdSecretCreate))
#define GKD_SECRET_CREATE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKD_SECRET_TYPE_CREATE, GkdSecretCreateClass))
#define GKD_SECRET_IS_CREATE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKD_SECRET_TYPE_CREATE))
#define GKD_SECRET_IS_CREATE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKD_SECRET_TYPE_CREATE))
#define GKD_SECRET_CREATE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKD_SECRET_TYPE_CREATE, GkdSecretCreateClass))

typedef struct _GkdSecretCreateClass GkdSecretCreateClass;

struct _GkdSecretCreateClass {
	GkdSecretPromptClass parent_class;
};

GType               gkd_secret_create_get_type                (void);

GkdSecretCreate*    gkd_secret_create_new                     (GkdSecretService *service,
                                                               const gchar *caller,
                                                               GckAttributes *attrs,
                                                               const gchar *alias);

GckObject*          gkd_secret_create_with_credential         (GckSession *session,
                                                               GckAttributes *attrs,
                                                               GckObject *cred,
                                                               GError **error);

gchar*              gkd_secret_create_with_secret             (GckAttributes *attrs,
                                                               GkdSecretSecret *master,
                                                               DBusError *derr);

#endif /* __GKD_SECRET_CREATE_H__ */
