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

#ifndef __GKD_SECRET_PROMPT_H__
#define __GKD_SECRET_PROMPT_H__

#include <glib-object.h>

#include "gkd-secret-types.h"

#include "ui/gku-prompt.h"

#include "gck/gck.h"

#include <dbus/dbus.h>

#define GKD_SECRET_TYPE_PROMPT               (gkd_secret_prompt_get_type ())
#define GKD_SECRET_PROMPT(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKD_SECRET_TYPE_PROMPT, GkdSecretPrompt))
#define GKD_SECRET_PROMPT_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKD_SECRET_TYPE_PROMPT, GkdSecretPromptClass))
#define GKD_SECRET_IS_PROMPT(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKD_SECRET_TYPE_PROMPT))
#define GKD_SECRET_IS_PROMPT_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKD_SECRET_TYPE_PROMPT))
#define GKD_SECRET_PROMPT_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKD_SECRET_TYPE_PROMPT, GkdSecretPromptClass))

typedef struct _GkdSecretPromptClass GkdSecretPromptClass;
typedef struct _GkdSecretPromptPrivate GkdSecretPromptPrivate;

struct _GkdSecretPrompt {
	GkuPrompt parent;
	GkdSecretPromptPrivate *pv;
};

struct _GkdSecretPromptClass {
	GkuPromptClass parent_class;

	/* virtual methods */
	void (*prompt_ready) (GkdSecretPrompt *self);
	void (*encode_result) (GkdSecretPrompt *self, DBusMessageIter *iter);
};

GType               gkd_secret_prompt_get_type                (void);

const gchar*        gkd_secret_prompt_get_caller              (GkdSecretPrompt *self);

GckSession*         gkd_secret_prompt_get_pkcs11_session      (GkdSecretPrompt *self);

GkdSecretService*   gkd_secret_prompt_get_service             (GkdSecretPrompt *self);

GkdSecretObjects*   gkd_secret_prompt_get_objects             (GkdSecretPrompt *self);

GkdSecretSession*   gkd_secret_prompt_get_session             (GkdSecretPrompt *self);

GkdSecretSecret*    gkd_secret_prompt_get_secret              (GkdSecretPrompt *self,
                                                               const gchar *password_type);

GckObject*          gkd_secret_prompt_lookup_collection       (GkdSecretPrompt *self,
                                                               const gchar *path);

void                gkd_secret_prompt_complete                (GkdSecretPrompt *self);

void                gkd_secret_prompt_dismiss                 (GkdSecretPrompt *self);

#endif /* __GKD_SECRET_PROMPT_H__ */
