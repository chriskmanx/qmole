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

#ifndef __GCR_KEY_WIDGET_H__
#define __GCR_KEY_WIDGET_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-types.h"

G_BEGIN_DECLS

#define GCR_TYPE_KEY_WIDGET               (gcr_key_widget_get_type ())
#define GCR_KEY_WIDGET(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_KEY_WIDGET, GcrKeyWidget))
#define GCR_KEY_WIDGET_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_KEY_WIDGET, GcrKeyWidgetClass))
#define GCR_IS_KEY_WIDGET(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_KEY_WIDGET))
#define GCR_IS_KEY_WIDGET_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_KEY_WIDGET))
#define GCR_KEY_WIDGET_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_KEY_WIDGET, GcrKeyWidgetClass))

typedef struct _GcrKeyWidget GcrKeyWidget;
typedef struct _GcrKeyWidgetClass GcrKeyWidgetClass;
typedef struct _GcrKeyWidgetPrivate GcrKeyWidgetPrivate;

/*
 * TODO: GcrKeyWidget and GcrKeyWidgetClass are hidden until
 * we can figure out what they should be derived from.
 */

GType                   gcr_key_widget_get_type               (void);

GcrKeyWidget*           gcr_key_widget_new                    (GckAttributes *attrs);

void                    gcr_key_widget_set_attributes         (GcrKeyWidget *self,
                                                               GckAttributes *attrs);

GckAttributes*          gcr_key_widget_get_attributes         (GcrKeyWidget *self);

G_END_DECLS

#endif /* __GCR_KEY_WIDGET_H__ */
