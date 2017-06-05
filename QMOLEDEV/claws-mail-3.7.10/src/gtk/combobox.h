/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Andrej Kacian and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __COMBOBOX_H__
#define __COMBOBOX_H__

#include <glib.h>
#include <gtk/gtk.h>

enum {
	COMBOBOX_TEXT,
	COMBOBOX_DATA,
	COMBOBOX_SENS
};


#define COMBOBOX_ADD(menu, label, data) 		 \
{ 								 \
	gtk_list_store_append(menu, &iter); \
	gtk_list_store_set(menu, &iter, \
			COMBOBOX_TEXT, label, \
			COMBOBOX_DATA, data, \
			COMBOBOX_SENS, TRUE, \
			-1); \
}

#define COMBOBOX_ADD_ESCAPED(menu, label, data) 		 \
{ 								 \
	gchar *tmp_esc = g_markup_printf_escaped("%s", label);	 \
	gtk_list_store_append(menu, &iter); 			 \
	gtk_list_store_set(menu, &iter, 			 \
			COMBOBOX_TEXT, (tmp_esc ? tmp_esc : ""), 		 \
			COMBOBOX_DATA, data, 				 \
			COMBOBOX_SENS, TRUE,				 \
			-1); 					 \
	g_free(tmp_esc);					 \
}

GtkWidget *combobox_text_new(const gboolean with_entry, const gchar *text, ...);

void combobox_select_by_data	(GtkComboBox 		*combobox,
				 gint			 data);
void combobox_select_by_text	(GtkComboBox 		*combobox,
				 const gchar		*data);

gint combobox_get_active_data	(GtkComboBox 		*combobox);

void combobox_unset_popdown_strings(GtkComboBox		*combobox);
void combobox_set_popdown_strings(GtkComboBox		*combobox,
				 GList       *list);
gboolean combobox_set_value_from_arrow_key(GtkComboBox *combobox,
				 guint keyval);
void combobox_set_sensitive	(GtkComboBox *combobox,
				 const guint index,
				 const gboolean sensitive);

#endif /* __COMBOBOX_H__ */
