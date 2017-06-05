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

#include "config.h"

#include "gcr-import-dialog.h"
#include "gcr-internal.h"

#include "egg/egg-entry-buffer.h"

enum {
	PROP_0,
	PROP_SELECTED_SLOT,
	PROP_PASSWORD,
	PROP_PRIMARY_TEXT,
	PROP_SECONDARY_TEXT
};

enum {
	COLUMN_SLOT,
	COLUMN_ICON,
	COLUMN_LABEL,
	N_COLUMNS
};

struct _GcrImportDialogPrivate {
	GtkBuilder *builder;
	GtkEntry *entry;
	GtkWidget *button;
	GtkComboBox *combo;
	GtkListStore *slots;
};

G_DEFINE_TYPE (GcrImportDialog, _gcr_import_dialog, GTK_TYPE_DIALOG);

/* -----------------------------------------------------------------------------
 * INTERNAL 
 */

static void
populate_slots (GcrImportDialog *self)
{
	GList *modules, *m;
	GList *slots, *s;
	GtkTreeIter iter;
	GckTokenInfo *info;
	gboolean added;
	
	g_assert (GCR_IS_IMPORT_DIALOG (self));

	if (self->pv->slots)
		return;

	self->pv->slots = gtk_list_store_new (N_COLUMNS, GCK_TYPE_SLOT, G_TYPE_STRING, G_TYPE_STRING);
	gtk_combo_box_set_model (self->pv->combo, GTK_TREE_MODEL (self->pv->slots));

	modules = _gcr_get_pkcs11_modules ();
	g_return_if_fail (modules);
	
	gtk_list_store_clear (self->pv->slots);
	
	added = FALSE;
	for (m = modules; m; m = g_list_next (m)) {

		g_return_if_fail (GCK_IS_MODULE (m->data));
		slots = gck_module_get_slots (m->data, TRUE);

		for (s = slots; s; s = g_list_next (s)) {
			info = gck_slot_get_token_info (s->data);
			if (!(info->flags & CKF_WRITE_PROTECTED)) {
				gtk_list_store_append (self->pv->slots, &iter);
				gtk_list_store_set (self->pv->slots, &iter, 
				                    COLUMN_LABEL, info->label,
				                    COLUMN_SLOT, s->data,
				                    -1);
				added = TRUE;
			}
		}

		gck_list_unref_free (slots);
	}
	
	if (added)
		gtk_combo_box_set_active (self->pv->combo, 0);
}

/* -----------------------------------------------------------------------------
 * OBJECT 
 */

static void
gcr_import_dialog_real_realize (GtkWidget *base)
{
	GcrImportDialog *self = GCR_IMPORT_DIALOG (base);
	if (gtk_widget_get_visible (GTK_WIDGET (self->pv->combo)))
		populate_slots (self);	
	GTK_WIDGET_CLASS (_gcr_import_dialog_parent_class)->realize (base);
}

static GObject* 
gcr_import_dialog_constructor (GType type, guint n_props, GObjectConstructParam *props) 
{
	GcrImportDialog *self = GCR_IMPORT_DIALOG (G_OBJECT_CLASS (_gcr_import_dialog_parent_class)->constructor(type, n_props, props));
	GtkCellRenderer *renderer;
	GtkEntryBuffer *buffer;
	GtkWidget *widget;

	g_return_val_if_fail (self, NULL);
	
	if (!gtk_builder_add_from_file (self->pv->builder, UIDIR "gcr-import-dialog.ui", NULL))
		g_return_val_if_reached (NULL);

	/* Fill in the dialog from builder */
	widget = GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "import-dialog"));
	g_return_val_if_fail (widget, FALSE);
	gtk_container_add (GTK_CONTAINER (gtk_dialog_get_content_area (GTK_DIALOG (self))), widget);

	/* Add a secure entry */
	buffer = egg_entry_buffer_new ();
	self->pv->entry = GTK_ENTRY (gtk_entry_new_with_buffer (buffer));
	g_object_unref (buffer);
	widget = GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "password-area"));
	gtk_container_add (GTK_CONTAINER (widget), GTK_WIDGET (self->pv->entry));
	gtk_widget_show (GTK_WIDGET (self->pv->entry));
	
	/* Initialize the combo box */
	self->pv->combo = GTK_COMBO_BOX (gtk_builder_get_object (self->pv->builder, "slot-combo"));
	renderer = gtk_cell_renderer_pixbuf_new ();
	gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (self->pv->combo), renderer, FALSE);
	gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (self->pv->combo), renderer, "icon-name", COLUMN_ICON);
	g_object_set (renderer, "xpad", 3, NULL);
	renderer = gtk_cell_renderer_text_new ();
	gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (self->pv->combo), renderer, TRUE);
	gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (self->pv->combo), renderer, "text", COLUMN_LABEL);

	/* Add our various buttons */
	gtk_dialog_add_button (GTK_DIALOG (self), GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
	self->pv->button = gtk_dialog_add_button (GTK_DIALOG (self), GTK_STOCK_OK, GTK_RESPONSE_OK);
	gtk_dialog_set_default_response (GTK_DIALOG (self), GTK_RESPONSE_OK);
	
	_gcr_import_dialog_show_password (self);
	
	return G_OBJECT (self);
}

static void
_gcr_import_dialog_init (GcrImportDialog *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_IMPORT_DIALOG, GcrImportDialogPrivate);
	self->pv->builder = gtk_builder_new ();
}

static void
gcr_import_dialog_dispose (GObject *obj)
{
	G_OBJECT_CLASS (_gcr_import_dialog_parent_class)->dispose (obj);
}

static void
gcr_import_dialog_finalize (GObject *obj)
{
	GcrImportDialog *self = GCR_IMPORT_DIALOG (obj);
	
	g_object_unref (self->pv->slots);
	self->pv->slots = NULL;
	
	g_object_unref (self->pv->builder);
	self->pv->builder = NULL;

	G_OBJECT_CLASS (_gcr_import_dialog_parent_class)->finalize (obj);
}

static void
gcr_import_dialog_set_property (GObject *obj, guint prop_id, const GValue *value, 
                                GParamSpec *pspec)
{
	GcrImportDialog *self = GCR_IMPORT_DIALOG (obj);
	
	switch (prop_id) {
	case PROP_SELECTED_SLOT:
		_gcr_import_dialog_set_selected_slot (self, g_value_get_object (value));
		break;
	case PROP_PASSWORD:
		_gcr_import_dialog_set_password (self, g_value_get_pointer (value));
		break;
	case PROP_PRIMARY_TEXT:
		_gcr_import_dialog_set_primary_text (self, g_value_get_string (value));
		break;
	case PROP_SECONDARY_TEXT:
		_gcr_import_dialog_set_secondary_text (self, g_value_get_string (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gcr_import_dialog_get_property (GObject *obj, guint prop_id, GValue *value, 
                           GParamSpec *pspec)
{
	GcrImportDialog *self = GCR_IMPORT_DIALOG (obj);
	
	switch (prop_id) {
	case PROP_SELECTED_SLOT:
		g_value_set_object (value, _gcr_import_dialog_get_selected_slot (self));
		break;
	case PROP_PASSWORD:
		g_value_set_pointer (value, (gpointer)_gcr_import_dialog_get_password (self));
		break;
	case PROP_PRIMARY_TEXT:
		g_value_set_string (value, _gcr_import_dialog_get_primary_text (self));
		break;
	case PROP_SECONDARY_TEXT:
		g_value_set_string (value, _gcr_import_dialog_get_secondary_text (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_import_dialog_class_init (GcrImportDialogClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
	
	gobject_class->constructor = gcr_import_dialog_constructor;
	gobject_class->dispose = gcr_import_dialog_dispose;
	gobject_class->finalize = gcr_import_dialog_finalize;
	gobject_class->set_property = gcr_import_dialog_set_property;
	gobject_class->get_property = gcr_import_dialog_get_property;
	
	widget_class->realize = gcr_import_dialog_real_realize;
    
	g_type_class_add_private (gobject_class, sizeof (GcrImportDialogPrivate));
	
	g_object_class_install_property (gobject_class, PROP_SELECTED_SLOT,
	           g_param_spec_object ("selected-slot", "Selected Slot", "Selected PKCS#11 slot",
	                                GCK_TYPE_SLOT, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_PASSWORD,
	           g_param_spec_pointer ("password", "Password", "Pointer to password",
	                                 G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_PRIMARY_TEXT,
	           g_param_spec_string ("primary-text", "Primary Text", "Primary dialog text",
	                                NULL, G_PARAM_READWRITE));
	
	g_object_class_install_property (gobject_class, PROP_SECONDARY_TEXT,
	           g_param_spec_string ("secondary-text", "Secondary Text", "Dialog secondary text",
	                                NULL, G_PARAM_READWRITE));

	_gcr_initialize ();
}

/* -----------------------------------------------------------------------------
 * PUBLIC 
 */

GcrImportDialog*
_gcr_import_dialog_new (void)
{
	GcrImportDialog *dialog = g_object_new (GCR_TYPE_IMPORT_DIALOG, NULL);
	return g_object_ref_sink (dialog);
}

gboolean
_gcr_import_dialog_run (GcrImportDialog *self, GtkWindow *parent)
{
	gboolean ret;
	
	g_return_val_if_fail (GCR_IS_IMPORT_DIALOG (self), FALSE);
	
	if (parent != NULL)
		gtk_window_set_transient_for (GTK_WINDOW (self), parent);
	
	ret = (gtk_dialog_run (GTK_DIALOG (self)) == GTK_RESPONSE_OK);
	
	if (parent != NULL)
		gtk_window_set_transient_for (GTK_WINDOW (self), NULL);

	gtk_widget_hide (GTK_WIDGET (self));
	return ret;
}

GckSlot*
_gcr_import_dialog_get_selected_slot (GcrImportDialog *self)
{
	GtkTreeIter iter;
	GckSlot *slot;
	
	g_return_val_if_fail (GCR_IMPORT_DIALOG (self), NULL);
	
	if (gtk_widget_get_visible (GTK_WIDGET (self->pv->combo)))
		populate_slots (self);
	else
		return NULL;
	
	if (!gtk_combo_box_get_active_iter (self->pv->combo, &iter))
		return NULL;
	
	gtk_tree_model_get (GTK_TREE_MODEL (self->pv->slots), &iter, COLUMN_SLOT, &slot, -1);
	
	/* We hold the reference to this */
	if (slot != NULL)
		g_object_unref (slot);
	
	return slot;
}

void
_gcr_import_dialog_set_selected_slot (GcrImportDialog *self, GckSlot *slot)
{
	GtkTreeIter iter;
	GckSlot *it_slot;
	gboolean matched;

	g_return_if_fail (GCR_IMPORT_DIALOG (self));

	if (gtk_widget_get_visible (GTK_WIDGET (self->pv->combo)))
		populate_slots (self);
	else
		g_return_if_reached ();

	if (slot == NULL) {
		gtk_combo_box_set_active (self->pv->combo, -1);
		return;
	}

	g_return_if_fail (GCK_IS_SLOT (slot));

	matched = FALSE;
	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (self->pv->slots), &iter)) {
		do {
			gtk_tree_model_get (GTK_TREE_MODEL (self->pv->slots), &iter, COLUMN_SLOT, &it_slot, -1);
			if (gck_slot_equal (it_slot, slot))
				matched = TRUE;
			g_object_unref (it_slot);
		} while (!matched && gtk_tree_model_iter_next (GTK_TREE_MODEL (self->pv->slots), &iter));
	}
	
	if (matched) {
		gtk_combo_box_set_active_iter (self->pv->combo, &iter);
	} else {
		gtk_combo_box_set_active (self->pv->combo, -1);
		g_return_if_reached ();
	}
}

void
_gcr_import_dialog_show_selected_slot (GcrImportDialog *self)
{
	g_return_if_fail (GCR_IS_IMPORT_DIALOG (self));
	gtk_widget_show (GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "slot-label")));
	gtk_widget_show (GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "slot-area")));
}

void
_gcr_import_dialog_hide_selected_slot (GcrImportDialog *self)
{
	g_return_if_fail (GCR_IS_IMPORT_DIALOG (self));
	gtk_widget_hide (GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "slot-label")));
	gtk_widget_hide (GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "slot-area")));
}

const gchar*
_gcr_import_dialog_get_password (GcrImportDialog *self)
{
	g_return_val_if_fail (GCR_IS_IMPORT_DIALOG (self), NULL);
	return gtk_entry_get_text (self->pv->entry);
}

void
_gcr_import_dialog_set_password (GcrImportDialog *self, const gchar *password)
{
	g_return_if_fail (GCR_IS_IMPORT_DIALOG (self));
	if (password == NULL)
		password = "";
	gtk_entry_set_text (self->pv->entry, password);
}

void
_gcr_import_dialog_show_password (GcrImportDialog *self)
{
	g_return_if_fail (GCR_IS_IMPORT_DIALOG (self));
	gtk_widget_show (GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "password-label")));
	gtk_widget_show (GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "password-area")));
	gtk_widget_grab_focus (GTK_WIDGET (self->pv->entry));
}

void
_gcr_import_dialog_hide_password (GcrImportDialog *self)
{
	g_return_if_fail (GCR_IS_IMPORT_DIALOG (self));
	gtk_widget_hide (GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "password-label")));
	gtk_widget_hide (GTK_WIDGET (gtk_builder_get_object (self->pv->builder, "password-area")));
	gtk_widget_grab_focus (self->pv->button);
}

const gchar*
_gcr_import_dialog_get_primary_text (GcrImportDialog *self)
{
	g_return_val_if_fail (GCR_IS_IMPORT_DIALOG (self), NULL);
	return gtk_label_get_text (GTK_LABEL (gtk_builder_get_object (self->pv->builder, "primary-text")));
}

void
_gcr_import_dialog_set_primary_text (GcrImportDialog *self, const gchar *text)
{
	gchar *label;
	
	g_return_if_fail (GCR_IS_IMPORT_DIALOG (self));
	
	if (text == NULL)
		text = "";
	
	label = g_markup_printf_escaped ("<span size='large' weight='bold'>%s</span>", text);
	gtk_label_set_markup (GTK_LABEL (gtk_builder_get_object (self->pv->builder, "primary-text")), label);
	g_free (label);
	
	g_object_notify (G_OBJECT (self), "primary-text");
}

const gchar*
_gcr_import_dialog_get_secondary_text (GcrImportDialog *self)
{
	g_return_val_if_fail (GCR_IS_IMPORT_DIALOG (self), NULL);
	return gtk_label_get_text (GTK_LABEL (gtk_builder_get_object (self->pv->builder, "secondary-text")));
}

void
_gcr_import_dialog_set_secondary_text (GcrImportDialog *self, const gchar *text)
{
	g_return_if_fail (GCR_IS_IMPORT_DIALOG (self));
	
	if (text == NULL)
		text = "";
	
	gtk_label_set_markup (GTK_LABEL (gtk_builder_get_object (self->pv->builder, "secondary-text")), text);
	g_object_notify (G_OBJECT (self), "primary-text");
}
