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

#ifndef __GCR_COMBO_SELECTOR_H__
#define __GCR_COMBO_SELECTOR_H__

#include "gcr-types.h"

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GCR_TYPE_COMBO_SELECTOR               (gcr_combo_selector_get_type ())
#define GCR_COMBO_SELECTOR(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_COMBO_SELECTOR, GcrComboSelector))
#define GCR_COMBO_SELECTOR_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_COMBO_SELECTOR, GcrComboSelectorClass))
#define GCR_IS_COMBO_SELECTOR(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_COMBO_SELECTOR))
#define GCR_IS_COMBO_SELECTOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_COMBO_SELECTOR))
#define GCR_COMBO_SELECTOR_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_COMBO_SELECTOR, GcrComboSelectorClass))

typedef struct _GcrComboSelector GcrComboSelector;
typedef struct _GcrComboSelectorClass GcrComboSelectorClass;
typedef struct _GcrComboSelectorPrivate GcrComboSelectorPrivate;

struct _GcrComboSelector {
	GtkComboBox parent;

	/*< private >*/
	GcrComboSelectorPrivate *pv;
};

struct _GcrComboSelectorClass {
	/*< private >*/
	GtkComboBoxClass parent_class;
};

GType                    gcr_combo_selector_get_type         (void);

GcrComboSelector*        gcr_combo_selector_new              (GcrCollection *collection);

GcrCollection*           gcr_combo_selector_get_collection   (GcrComboSelector *self);

const GcrColumn*         gcr_combo_selector_get_columns      (GcrComboSelector *self);

GObject*                 gcr_combo_selector_get_selected     (GcrComboSelector *self);

void                     gcr_combo_selector_set_selected     (GcrComboSelector *self,
                                                              GObject *selected);

G_END_DECLS

#endif /* __GCR_COMBO_SELECTOR_H__ */
