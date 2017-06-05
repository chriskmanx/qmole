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

#ifndef __GKU_PROMPT_H__
#define __GKU_PROMPT_H__

#include <glib-object.h>

typedef enum {
	GKU_RESPONSE_FAILURE      = -1,
	GKU_RESPONSE_NONE         = 0,
	GKU_RESPONSE_NO           = 1,
	GKU_RESPONSE_OK           = 2,
	GKU_RESPONSE_OTHER        = 3,
} GkuResponse;

#define GKU_TYPE_PROMPT               (gku_prompt_get_type ())
#define GKU_PROMPT(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GKU_TYPE_PROMPT, GkuPrompt))
#define GKU_PROMPT_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GKU_TYPE_PROMPT, GkuPromptClass))
#define GKU_IS_PROMPT(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GKU_TYPE_PROMPT))
#define GKU_IS_PROMPT_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GKU_TYPE_PROMPT))
#define GKU_PROMPT_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GKU_TYPE_PROMPT, GkuPromptClass))

typedef struct _GkuPrompt GkuPrompt;
typedef struct _GkuPromptClass GkuPromptClass;
typedef struct _GkuPromptPrivate GkuPromptPrivate;

struct _GkuPrompt {
	GObject parent;
	GkuPromptPrivate *pv;
};

struct _GkuPromptClass {
	GObjectClass parent_class;

	/* signals */
	gboolean (*responded) (GkuPrompt *self);
	void (*completed) (GkuPrompt *self);
};

GType               gku_prompt_get_type               (void);

GkuPrompt*          gku_prompt_new                    (void);

void                gku_prompt_reset                  (GkuPrompt *prompt,
                                                       gboolean hard);

void                gku_prompt_set_title              (GkuPrompt *prompt,
                                                       const gchar *title);

void                gku_prompt_set_primary_text       (GkuPrompt *prompt,
                                                       const gchar *primary);

void                gku_prompt_set_secondary_text     (GkuPrompt *prompt,
                                                       const gchar *secondary);

void                gku_prompt_set_warning            (GkuPrompt *prompt,
                                                       const gchar *warning);

void                gku_prompt_set_window_id          (GkuPrompt *prompt,
                                                       const gchar *window_id);

void                gku_prompt_show_widget            (GkuPrompt *prompt,
                                                       const gchar *widget);

void                gku_prompt_hide_widget            (GkuPrompt *prompt,
                                                       const gchar *widget);

void                gku_prompt_select_widget          (GkuPrompt *prompt,
                                                       const gchar *widget);

gboolean            gku_prompt_has_response           (GkuPrompt *prompt);

gint                gku_prompt_get_response           (GkuPrompt *prompt);

gchar*              gku_prompt_get_password           (GkuPrompt *prompt,
                                                       const gchar *password_type);

gpointer            gku_prompt_get_transport_param    (GkuPrompt *prompt,
                                                       const gchar *name,
                                                       gsize *n_value);

void                gku_prompt_set_transport_param    (GkuPrompt *prompt,
                                                       const gchar *name,
                                                       gconstpointer value,
                                                       gsize n_value);

gboolean            gku_prompt_get_transport_password (GkuPrompt *self,
                                                       const gchar *password_type,
                                                       gpointer *parameter,
                                                       gsize *n_parameter,
                                                       gpointer *value,
                                                       gsize *n_value);

const gchar*        gku_prompt_get_unlock_choice      (GkuPrompt *self);

void                gku_prompt_set_unlock_choice      (GkuPrompt *self,
                                                       const gchar *option);

void                gku_prompt_set_unlock_sensitive   (GkuPrompt *self,
                                                       const gchar *option,
                                                       gboolean sensitive,
                                                       const gchar *reason);

guint               gku_prompt_get_unlock_ttl         (GkuPrompt *self);

void                gku_prompt_set_unlock_ttl         (GkuPrompt *self,
                                                       guint ttl);

void                gku_prompt_set_unlock_label       (GkuPrompt *self,
                                                       const gchar *option,
                                                       const gchar *label);

gboolean            gku_prompt_is_widget_selected     (GkuPrompt *prompt,
                                                       const gchar *widget);

typedef GkuPrompt*  (*GkuPromptAttentionFunc)             (gpointer user_data);

void                gku_prompt_request_attention_async    (const gchar *window_id,
                                                           GkuPromptAttentionFunc callback,
                                                           gpointer user_data,
                                                           GDestroyNotify destroy_notify);

void                gku_prompt_request_attention_sync     (const gchar *window_id,
                                                           GkuPromptAttentionFunc callback,
                                                           gpointer user_data,
                                                           GDestroyNotify destroy_notify);

#ifdef WITH_TESTABLE

void                gku_prompt_dummy_prepare_response      (void);

gboolean            gku_prompt_dummy_have_response         (void);

void                gku_prompt_dummy_queue_response        (const gchar *response);

void                gku_prompt_dummy_queue_ok_password     (const gchar *password);

void                gku_prompt_dummy_queue_ok_passwords    (const gchar *original,
                                                            const gchar *password);

void                gku_prompt_dummy_queue_auto_password   (const gchar *password);

void                gku_prompt_dummy_queue_no              (void);

#endif /* WITH_TESTABLE */

#endif /* __GKU_PROMPT_H__ */
