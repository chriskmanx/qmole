/*
 * gnome-keyring
 *
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

#ifndef __GCR_TREE_SELECTOR_H__
#define __GCR_TREE_SELECTOR_H__

#include "gcr-types.h"

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GCR_TYPE_TREE_SELECTOR               (gcr_tree_selector_get_type ())
#define GCR_TREE_SELECTOR(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_TREE_SELECTOR, GcrTreeSelector))
#define GCR_TREE_SELECTOR_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_TREE_SELECTOR, GcrTreeSelectorClass))
#define GCR_IS_TREE_SELECTOR(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_TREE_SELECTOR))
#define GCR_IS_TREE_SELECTOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_TREE_SELECTOR))
#define GCR_TREE_SELECTOR_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_TREE_SELECTOR, GcrTreeSelectorClass))

typedef struct _GcrTreeSelector GcrTreeSelector;
typedef struct _GcrTreeSelectorClass GcrTreeSelectorClass;
typedef struct _GcrTreeSelectorPrivate GcrTreeSelectorPrivate;

struct _GcrTreeSelector {
	GtkTreeView parent;

	/*< private >*/
	GcrTreeSelectorPrivate *pv;
};

struct _GcrTreeSelectorClass {
	/*< private >*/
	GtkTreeViewClass parent_class;
};

GType                    gcr_tree_selector_get_type          (void);

GcrTreeSelector*         gcr_tree_selector_new               (GcrCollection *collection,
                                                              const GcrColumn *columns);

GcrCollection*           gcr_tree_selector_get_collection    (GcrTreeSelector *self);

const GcrColumn*         gcr_tree_selector_get_columns       (GcrTreeSelector *self);

GList*                   gcr_tree_selector_get_selected      (GcrTreeSelector *self);

void                     gcr_tree_selector_set_selected      (GcrTreeSelector *self,
                                                              GList *selected);

G_END_DECLS

#endif /* __GCR_TREE_SELECTOR_H__ */
