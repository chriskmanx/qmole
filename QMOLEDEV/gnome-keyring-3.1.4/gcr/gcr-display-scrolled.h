/*
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#ifndef __GCR_DISPLAY_SCROLLED_H__
#define __GCR_DISPLAY_SCROLLED_H__

#include <glib-object.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GCR_TYPE_DISPLAY_SCROLLED               (_gcr_display_scrolled_get_type ())
#define GCR_DISPLAY_SCROLLED(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_DISPLAY_SCROLLED, GcrDisplayScrolled))
#define GCR_DISPLAY_SCROLLED_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_DISPLAY_SCROLLED, GcrDisplayScrolledClass))
#define GCR_IS_DISPLAY_SCROLLED(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_DISPLAY_SCROLLED))
#define GCR_IS_DISPLAY_SCROLLED_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_DISPLAY_SCROLLED))
#define GCR_DISPLAY_SCROLLED_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_DISPLAY_SCROLLED, GcrDisplayScrolledClass))

typedef struct _GcrDisplayScrolled GcrDisplayScrolled;
typedef struct _GcrDisplayScrolledClass GcrDisplayScrolledClass;
typedef struct _GcrDisplayScrolledPrivate GcrDisplayScrolledPrivate;

struct _GcrDisplayScrolled {
	GtkScrolledWindow parent;
	GcrDisplayScrolledPrivate *pv;
};

struct _GcrDisplayScrolledClass {
	GtkScrolledWindowClass parent_class;
};

GType                _gcr_display_scrolled_get_type                    (void);

GcrDisplayScrolled*  _gcr_display_scrolled_new                         (void);

G_END_DECLS

#endif /* __GCR_DISPLAY_SCROLLED_H__ */
