/*
 * Copyright (C) 2010 Stefan Walter
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

#if !defined (__GCR_H_INSIDE__) && !defined (GCR_COMPILATION)
#error "Only <gcr/gcr.h> can be included directly."
#endif

#ifndef __GCR_UNLOCK_OPTIONS_WIDGET_H__
#define __GCR_UNLOCK_OPTIONS_WIDGET_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-types.h"
#include "gcr-unlock-options.h"

G_BEGIN_DECLS

#define GCR_TYPE_UNLOCK_OPTIONS_WIDGET               (gcr_unlock_options_widget_get_type ())
#define GCR_UNLOCK_OPTIONS_WIDGET(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_UNLOCK_OPTIONS_WIDGET, GcrUnlockOptionsWidget))
#define GCR_UNLOCK_OPTIONS_WIDGET_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_UNLOCK_OPTIONS_WIDGET, GcrUnlockOptionsWidgetClass))
#define GCR_IS_UNLOCK_OPTIONS_WIDGET(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_UNLOCK_OPTIONS_WIDGET))
#define GCR_IS_UNLOCK_OPTIONS_WIDGET_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_UNLOCK_OPTIONS_WIDGET))
#define GCR_UNLOCK_OPTIONS_WIDGET_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_UNLOCK_OPTIONS_WIDGET, GcrUnlockOptionsWidgetClass))

typedef struct _GcrUnlockOptionsWidget GcrUnlockOptionsWidget;
typedef struct _GcrUnlockOptionsWidgetClass GcrUnlockOptionsWidgetClass;
typedef struct _GcrUnlockOptionsWidgetPrivate GcrUnlockOptionsWidgetPrivate;

/*
 * TODO: GcrUnlockOptionsWidget and GcrUnlockOptionsWidgetClass are hidden until
 * we can figure out what they should be derived from.
 */

GType                         gcr_unlock_options_widget_get_type               (void);

GtkWidget*                    gcr_unlock_options_widget_new                    (void);

const gchar*                  gcr_unlock_options_widget_get_choice             (GcrUnlockOptionsWidget *self);

void                          gcr_unlock_options_widget_set_choice             (GcrUnlockOptionsWidget *self,
                                                                                const gchar *option);

guint                         gcr_unlock_options_widget_get_ttl                (GcrUnlockOptionsWidget *self);

void                          gcr_unlock_options_widget_set_ttl                (GcrUnlockOptionsWidget *self,
                                                                                guint ttl);

const gchar*                  gcr_unlock_options_widget_get_label              (GcrUnlockOptionsWidget *self,
                                                                                const gchar *option);

void                          gcr_unlock_options_widget_set_label              (GcrUnlockOptionsWidget *self,
                                                                                const gchar *option,
                                                                                const gchar *text);

gboolean                      gcr_unlock_options_widget_get_sensitive          (GcrUnlockOptionsWidget *self,
                                                                                const gchar *option);

void                          gcr_unlock_options_widget_set_sensitive          (GcrUnlockOptionsWidget *self,
                                                                                const gchar *option,
                                                                                gboolean sensitive,
                                                                                const gchar *reason);

G_END_DECLS

#endif /* __GCR_UNLOCK_OPTIONS_WIDGET_H__ */
