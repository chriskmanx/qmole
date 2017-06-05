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

#ifndef __GCR_DISPLAY_VIEW_H__
#define __GCR_DISPLAY_VIEW_H__

#include <glib-object.h>
#include <gtk/gtk.h>

#include "gcr-certificate.h"
#include "gcr-types.h"
#include "gcr-viewer.h"

G_BEGIN_DECLS

#define GCR_DISPLAY_VIEW_LINE_BREAK   0x2028

#define GCR_TYPE_DISPLAY_VIEW               (_gcr_display_view_get_type ())
#define GCR_DISPLAY_VIEW(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_DISPLAY_VIEW, GcrDisplayView))
#define GCR_DISPLAY_VIEW_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_DISPLAY_VIEW, GcrDisplayViewClass))
#define GCR_IS_DISPLAY_VIEW(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_DISPLAY_VIEW))
#define GCR_IS_DISPLAY_VIEW_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_DISPLAY_VIEW))
#define GCR_DISPLAY_VIEW_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_DISPLAY_VIEW, GcrDisplayViewClass))

typedef struct _GcrDisplayView GcrDisplayView;
typedef struct _GcrDisplayViewClass GcrDisplayViewClass;
typedef struct _GcrDisplayViewPrivate GcrDisplayViewPrivate;

struct _GcrDisplayView {
	GtkTextView parent;
	GcrDisplayViewPrivate *pv;
};

struct _GcrDisplayViewClass {
	GtkTextViewClass parent_class;
};

GType            _gcr_display_view_get_type                    (void);

GcrDisplayView*  _gcr_display_view_new                         (void);

void             _gcr_display_view_clear                       (GcrDisplayView *self,
                                                                GcrRenderer *renderer);

void             _gcr_display_view_append_value                (GcrDisplayView *self,
                                                                GcrRenderer *renderer,
                                                                const gchar *field,
                                                                const gchar *value,
                                                                gboolean monospace);

void             _gcr_display_view_append_hex                  (GcrDisplayView *self,
                                                                GcrRenderer *renderer,
                                                                const gchar *field,
                                                                gconstpointer value,
                                                                gsize n_value);

void             _gcr_display_view_append_title                (GcrDisplayView *self,
                                                                GcrRenderer *renderer,
                                                                const gchar *title);

void             _gcr_display_view_append_content              (GcrDisplayView *self,
                                                                GcrRenderer *renderer,
                                                                const gchar *content,
                                                                const gchar *details);

void             _gcr_display_view_start_details               (GcrDisplayView *self,
                                                                GcrRenderer *renderer);

void             _gcr_display_view_append_heading              (GcrDisplayView *self,
                                                                GcrRenderer *renderer,
                                                                const gchar *heading);

void             _gcr_display_view_append_fingerprint          (GcrDisplayView *self,
                                                                GcrRenderer *renderer,
                                                                const guchar *data,
                                                                gsize n_data,
                                                                const gchar *name,
                                                                GChecksumType type);

void             _gcr_display_view_set_icon                    (GcrDisplayView *self,
                                                                GcrRenderer *renderer,
                                                                GIcon *icon);

G_END_DECLS

#endif /* __GCR_DISPLAY_VIEW_H__ */
