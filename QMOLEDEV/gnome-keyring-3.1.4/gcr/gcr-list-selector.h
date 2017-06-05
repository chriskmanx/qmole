/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#ifndef __GCR_LIST_SELECTOR_H__
#define __GCR_LIST_SELECTOR_H__

#include "gcr-types.h"

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GCR_TYPE_LIST_SELECTOR               (gcr_list_selector_get_type ())
#define GCR_LIST_SELECTOR(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_LIST_SELECTOR, GcrListSelector))
#define GCR_LIST_SELECTOR_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_LIST_SELECTOR, GcrListSelectorClass))
#define GCR_IS_LIST_SELECTOR(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_LIST_SELECTOR))
#define GCR_IS_LIST_SELECTOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_LIST_SELECTOR))
#define GCR_LIST_SELECTOR_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_LIST_SELECTOR, GcrListSelectorClass))

typedef struct _GcrListSelector GcrListSelector;
typedef struct _GcrListSelectorClass GcrListSelectorClass;
typedef struct _GcrListSelectorPrivate GcrListSelectorPrivate;

struct _GcrListSelector {
	GtkTreeView parent;

	/*< private >*/
	GcrListSelectorPrivate *pv;
};

struct _GcrListSelectorClass {

	/*< private >*/
	GtkTreeViewClass parent_class;
};

GType                    gcr_list_selector_get_type         (void);

GcrListSelector*         gcr_list_selector_new              (GcrCollection *collection);

GcrCollection*           gcr_list_selector_get_collection   (GcrListSelector *self);

const GcrColumn*         gcr_list_selector_get_columns      (GcrListSelector *self);

GList*                   gcr_list_selector_get_selected     (GcrListSelector *self);

void                     gcr_list_selector_set_selected     (GcrListSelector *self,
                                                             GList *selected);

G_END_DECLS

#endif /* __GCR_LIST_SELECTOR_H__ */
