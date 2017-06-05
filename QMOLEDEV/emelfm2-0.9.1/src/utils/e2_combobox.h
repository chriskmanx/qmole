/* $Id: e2_combobox.h 2658 2013-08-10 06:35:52Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/utils/e2_combobox.h
@brief GtkComboBox utilities header

This is the header file for the GtkComboBox utilities.
*/

#ifndef __E2_COMBOBOX_H__
#define __E2_COMBOBOX_H__

/**
@brief These flags influence the GtkComboBox creation in several ways.

These flags influence the GtkComboBox creation in several ways
when creating them with e2_combobox_get() or e2_combobox_add().
*/
typedef enum
{
	E2_COMBOBOX_HAS_ENTRY       = 1,		//the GtkComboBox has an entry
									//(you should not use menu-style in that case)
	E2_COMBOBOX_MENU_STYLE      = 1<<1,	//the drop down widget is a menu instead of a list
	E2_COMBOBOX_FOCUS_ON_CHANGE = 1<<2, //if the GtkComboBox emits the changed signal,
									//grab the focus for it
	E2_COMBOBOX_NO_AUTO_HISTORY = 1<<3, //prevent automatic update of history list
									//of GtkComboBox which has an entry
	E2_COMBOBOX_ALLOW_DOUBLE    = 1<<4,	//allow the same history entries to be added
									//more than once
	E2_COMBOBOX_CYCLE_HISTORY   = 1<<5, //if the GtkComboBox has an entry, the history
									//will (if necessary) be cycled upon pressing GDK_UP or GDK_DOWN
} E2_ComboBoxFlags;

void e2_combobox_activated_cb (GtkWidget *entry, gpointer data);

void e2_combobox_block_changed (GtkWidget *combo);
//void e2_combobox_unblock_changed (GtkWidget *combo);
void e2_combobox_set_active (GtkWidget *combo, gint num);
gchar *e2_combobox_get_active_text (GtkWidget *combo);
void e2_combobox_clear_value (GtkWidget *combo, const gchar *value,
	gboolean with_entry);
//void e2_combobox_save_history (GtkWidget *combo, GList **list);
void e2_combobox_prepend_history (GtkWidget *combo, const gchar *newtext,
	gint limit, gboolean multi);
void e2_combobox_append_history (GtkWidget *combo, GList *list);
void e2_combobox_append_history_counted (GtkWidget *combo, guint num, gchar **array);
void e2_combobox_append_history_strv (GtkWidget *combo, gchar **strv);
gboolean e2_combobox_has_history (GtkComboBox *combo);
//void e2_combobox_select_last (GtkWidget *combo);
//gchar *e2_combobox_last_text (GtkComboBox *combo);
gchar *e2_combobox_first_text (GtkComboBox *combo);
GtkWidget *e2_combobox_get (void (*activate_cb)(GtkEntry*,gpointer),
	gpointer activate_data, GList **history, E2_ComboBoxFlags flags);
GtkWidget *e2_combobox_add (GtkWidget *box, gboolean expand, guint padding,
	void (*activate_cb)(GtkEntry*,gpointer), gpointer activate_data,
	GList **history, E2_ComboBoxFlags flags);

#endif //ndef __E2_COMBOBOX_H__
