/* 
 * gnome-keyring
 * 
 * Copyright (C) 2008 Stefan Walter
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

#ifndef __GCR_IMPORT_DIALOG_H__
#define __GCR_IMPORT_DIALOG_H__

#include "gcr.h"

#include "gck/gck.h"

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define GCR_TYPE_IMPORT_DIALOG               (_gcr_import_dialog_get_type ())
#define GCR_IMPORT_DIALOG(obj)               (G_TYPE_CHECK_INSTANCE_CAST ((obj), GCR_TYPE_IMPORT_DIALOG, GcrImportDialog))
#define GCR_IMPORT_DIALOG_CLASS(klass)       (G_TYPE_CHECK_CLASS_CAST ((klass), GCR_TYPE_IMPORT_DIALOG, GcrImportDialogClass))
#define GCR_IS_IMPORT_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GCR_TYPE_IMPORT_DIALOG))
#define GCR_IS_IMPORT_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_TYPE ((klass), GCR_TYPE_IMPORT_DIALOG))
#define GCR_IMPORT_DIALOG_GET_CLASS(obj)     (G_TYPE_INSTANCE_GET_CLASS ((obj), GCR_TYPE_IMPORT_DIALOG, GcrImportDialogClass))

typedef struct _GcrImportDialog GcrImportDialog;
typedef struct _GcrImportDialogClass GcrImportDialogClass;
typedef struct _GcrImportDialogPrivate GcrImportDialogPrivate;

struct _GcrImportDialog {
	GtkDialog parent;
	GcrImportDialogPrivate *pv;
};

struct _GcrImportDialogClass {
	GtkDialogClass parent_class;
};

GType               _gcr_import_dialog_get_type               (void);

GcrImportDialog*    _gcr_import_dialog_new                    (void);

gboolean            _gcr_import_dialog_run                    (GcrImportDialog *self,
                                                               GtkWindow *parent);

GckSlot*            _gcr_import_dialog_get_selected_slot      (GcrImportDialog *self);

void                _gcr_import_dialog_set_selected_slot      (GcrImportDialog *self,
                                                               GckSlot *slot);

void                _gcr_import_dialog_show_selected_slot     (GcrImportDialog *self);

void                _gcr_import_dialog_hide_selected_slot     (GcrImportDialog *self);

const gchar*        _gcr_import_dialog_get_password           (GcrImportDialog *self);

void                _gcr_import_dialog_set_password           (GcrImportDialog *self,
                                                               const gchar *password);

void                _gcr_import_dialog_show_password          (GcrImportDialog *self);

void                _gcr_import_dialog_hide_password          (GcrImportDialog *self);

const gchar*        _gcr_import_dialog_get_primary_text       (GcrImportDialog *self);

void                _gcr_import_dialog_set_primary_text       (GcrImportDialog *self,
                                                               const gchar *text);

const gchar*        _gcr_import_dialog_get_secondary_text     (GcrImportDialog *self);

void                _gcr_import_dialog_set_secondary_text     (GcrImportDialog *self,
                                                               const gchar *text);

G_END_DECLS

#endif /* __GCR_IMPORT_DIALOG_H__ */
