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

#ifndef __GCR_KEY_RENDERER_H__
#define __GCR_KEY_RENDERER_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-types.h"

G_BEGIN_DECLS

#define GCR_TYPE_KEY_RENDERER               (gcr_key_renderer_get_type ())
#define GCR_KEY_RENDERER(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_KEY_RENDERER, GcrKeyRenderer))
#define GCR_KEY_RENDERER_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_KEY_RENDERER, GcrKeyRendererClass))
#define GCR_IS_KEY_RENDERER(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_KEY_RENDERER))
#define GCR_IS_KEY_RENDERER_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_KEY_RENDERER))
#define GCR_KEY_RENDERER_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_KEY_RENDERER, GcrKeyRendererClass))

typedef struct _GcrKeyRenderer GcrKeyRenderer;
typedef struct _GcrKeyRendererClass GcrKeyRendererClass;
typedef struct _GcrKeyRendererPrivate GcrKeyRendererPrivate;

struct _GcrKeyRenderer {
	GObject parent;

	/*< private >*/
	GcrKeyRendererPrivate *pv;
};

struct _GcrKeyRendererClass {
	GObjectClass parent_class;
};

GType                   gcr_key_renderer_get_type             (void);

GcrKeyRenderer*         gcr_key_renderer_new                  (const gchar *label,
                                                               GckAttributes *attrs);

void                    gcr_key_renderer_set_attributes       (GcrKeyRenderer *self,
                                                               GckAttributes *attrs);

GckAttributes*          gcr_key_renderer_get_attributes       (GcrKeyRenderer *self);

G_END_DECLS

#endif /* __GCR_KEY_RENDERER_H__ */
