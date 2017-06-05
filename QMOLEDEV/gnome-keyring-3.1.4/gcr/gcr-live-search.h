/*
 * Copyright (C) 2011 Collabora Ltd.
 * Copyright (C) 2010 Collabora Ltd.
 * Copyright (C) 2007-2010 Nokia Corporation.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Authors: Felix Kaser <felix.kaser@collabora.co.uk>
 *          Xavier Claessens <xavier.claessens@collabora.co.uk>
 *          Claudio Saavedra <csaavedra@igalia.com>
 *          Stef Walter <stefw@collabora.co.uk>
 */

/* Code borrowed from Empathy */

#ifndef __GCR_LIVE_SEARCH_H__
#define __GCR_LIVE_SEARCH_H__

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GCR_TYPE_LIVE_SEARCH         (_gcr_live_search_get_type ())
#define GCR_LIVE_SEARCH(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GCR_TYPE_LIVE_SEARCH, GcrLiveSearch))
#define GCR_LIVE_SEARCH_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST ((k), GCR_TYPE_LIVE_SEARCH, GcrLiveSearchClass))
#define GCR_IS_LIVE_SEARCH(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), GCR_TYPE_LIVE_SEARCH))
#define GCR_IS_LIVE_SEARCH_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), GCR_TYPE_LIVE_SEARCH))
#define GCR_LIVE_SEARCH_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), GCR_TYPE_LIVE_SEARCH, GcrLiveSearchClass))

typedef struct _GcrLiveSearch      GcrLiveSearch;
typedef struct _GcrLiveSearchClass GcrLiveSearchClass;
typedef struct _GcrLiveSearchPrivate GcrLiveSearchPrivate;

struct _GcrLiveSearch {
	GtkBox parent;

	/* <private> */
	GcrLiveSearchPrivate *pv;
};

struct _GcrLiveSearchClass {
	GtkBoxClass parent_class;
};

GType             _gcr_live_search_get_type               (void) G_GNUC_CONST;

GtkWidget *       _gcr_live_search_new                    (GtkWidget *hook);

GtkWidget *       _gcr_live_search_get_hook_widget        (GcrLiveSearch *self);
void              _gcr_live_search_set_hook_widget        (GcrLiveSearch *self,
                                                           GtkWidget *hook);

const gchar *     _gcr_live_search_get_text               (GcrLiveSearch *self);
void              _gcr_live_search_set_text               (GcrLiveSearch *self,
                                                           const gchar *text);

gboolean          _gcr_live_search_match                  (GcrLiveSearch *self,
                                                           const gchar *string);

GPtrArray *       _gcr_live_search_strip_utf8_string      (const gchar *string);

gboolean          _gcr_live_search_match_words            (const gchar *string,
                                                           GPtrArray *words);

GPtrArray *       _gcr_live_search_get_words              (GcrLiveSearch *self);

/* Made public for unit tests */
gboolean          _gcr_live_search_match_string           (const gchar *string,
                                                           const gchar *prefix);

G_END_DECLS

#endif /* __GCR_LIVE_SEARCH_H__ */
