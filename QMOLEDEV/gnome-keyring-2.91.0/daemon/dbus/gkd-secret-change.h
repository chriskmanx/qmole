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

#ifndef __GKD_SECRET_CHANGE_H__
#define __GKD_SECRET_CHANGE_H__

#include <glib-object.h>

#include "gkd-secret-prompt.h"
#include "gkd-secret-types.h"

#include "gck/gck.h"

#define GKD_SECRET_TYPE_CHANGE               (gkd_secret_change_get_type ())
#define GKD_SECRET_CHANGE(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKD_SECRET_TYPE_CHANGE, GkdSecretChange))
#define GKD_SECRET_CHANGE_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKD_SECRET_TYPE_CHANGE, GkdSecretChangeClass))
#define GKD_SECRET_IS_CHANGE(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKD_SECRET_TYPE_CHANGE))
#define GKD_SECRET_IS_CHANGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKD_SECRET_TYPE_CHANGE))
#define GKD_SECRET_CHANGE_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKD_SECRET_TYPE_CHANGE, GkdSecretChangeClass))

typedef struct _GkdSecretChangeClass GkdSecretChangeClass;

struct _GkdSecretChangeClass {
	GkdSecretPromptClass parent_class;
};

GType               gkd_secret_change_get_type                (void);

GkdSecretChange*    gkd_secret_change_new                     (GkdSecretService *service,
                                                               const gchar *caller,
                                                               const gchar *path);

gboolean            gkd_secret_change_with_secrets            (GckObject *collection,
                                                               GkdSecretSecret *original,
                                                               GkdSecretSecret *master,
                                                               DBusError *derr);

#endif /* __GKD_SECRET_CHANGE_H__ */
