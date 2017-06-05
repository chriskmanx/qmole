/* $Id: e2_size_filter_dialog.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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

#include "emelfm2.h"
#include <math.h>
#include "e2_dialog.h"
#include "e2_filelist.h"

typedef struct _E2_SzFltDlgData
{
	GtkWidget *dialog;	//the created dialog
	GtkWidget *operation_combo;
	GtkWidget *size_entry;
	GtkWidget *units_combo;
//	GtkWidget *menu_item;	//copy of pointer to clicked filters menu item
//	gboolean itemstate;	//initial state of menu_item
//	gboolean blocked = FALSE;	//flag to prevent toggles when not wanted
	ViewInfo *view;		//data for filtered file pane
} E2_SzFltDlgData;

static void _e2_size_filter_dialog_response_cb (GtkDialog *dialog,
	gint response, E2_SzFltDlgData *rt);

/*static void _e2_size_filter_dialog_item_toggle_cb (void)
{
	gulong handler = g_signal_handler_find (menu_item, G_SIGNAL_MATCH_FUNC,
		0, 0, NULL, e2_size_filter_dialog_create_cb, NULL);
	g_signal_handler_block (menu_item, handler);
	NEEDCLOSEBGL
	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (menu_item), !itemstate);
	NEEDOPENBGL
	g_signal_handler_unblock (menu_item, handler);
}
*/
static void _e2_size_filter_dialog_ok (E2_SzFltDlgData *rt)
{
	gint index = gtk_combo_box_get_active (GTK_COMBO_BOX (rt->operation_combo));
	rt->view->size_filter.op = index;

	const gchar *s = gtk_entry_get_text (GTK_ENTRY (rt->size_entry));
	gdouble dsize = atof (s);
	index = gtk_combo_box_get_active (GTK_COMBO_BOX (rt->units_combo));
	if (index == 1 || index == 2)	//kb or Mb respectively
	  dsize *= pow (1024, index);
	rt->view->size_filter.size = (size_t)dsize;
	rt->view->size_filter.active = TRUE;
	//show the results
	e2_toolbar_toggle_filter_button (rt->view);
	e2_fileview_refilter_list (rt->view);
#ifdef E2_STATUS_DEMAND
	if (rt->view == curr_view)
    {
BGL?    OPENBGL
	    e2_window_update_status_bar (NULL);
        CLOSEBGL
    }
#endif
}
/**
@brief cleanup during the destruction of the view related to a dialog
@param object UNUSED the view-related object being destroyed
@param rt pointer to data struct for the dialog
@return
*/
static void _e2_size_filter_dialog_destroy_cb (
#ifdef USE_GTK3_0
	GtkWidget *object,
#else
	GtkObject *object,
#endif
	E2_SzFltDlgData *rt)
{
	g_signal_handlers_disconnect_by_func ((gpointer)rt->dialog,
		_e2_size_filter_dialog_response_cb, rt); //no double-handling
//	NEEDOPENBGL
	_e2_size_filter_dialog_response_cb (GTK_DIALOG (rt->dialog), 0, rt);
//	NEEDCLOSEBGL
}
/**
@brief dialog response callback

@param dialog the permissions-dialog
@param response the response for the clicked button
@param rt pointer to dialog data struct

@return
*/
static void _e2_size_filter_dialog_response_cb (GtkDialog *dialog,
	gint response, E2_SzFltDlgData *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_APPLY:
			gtk_widget_hide (rt->dialog);
			_e2_size_filter_dialog_ok (rt);
			break;
		case E2_RESPONSE_REMOVE:
			//update the cached copy of the state
			rt->view->size_filter.active = FALSE;
			gtk_widget_hide (rt->dialog);
			//show the results
			e2_toolbar_toggle_filter_button (rt->view);
			e2_fileview_refilter_list (rt->view);
#ifdef E2_STATUS_DEMAND
			if (rt->view == curr_view)
            {
                OPENBGL
				e2_window_update_status_bar (NULL);
                CLOSEBGL
            }
#endif
			break;
		default:
		  //the menu item will be re-toggled when dialog is destroyed
			break;
	}
	g_signal_handlers_disconnect_by_func ((gpointer)rt->view->treeview,
		_e2_size_filter_dialog_destroy_cb, rt);
	gtk_widget_destroy (rt->dialog);
	NEEDOPENBGL
	DEALLOCATE (E2_SzFltDlgData, rt);
}
/**
@brief handle Return keypresses in the size entry
@param entry UNUSED the entry widget for the combo box
@param rt pointer to dialog data struct
@return
*/
static void _e2_size_filter_dialog_activated_cb (GtkEntry *entry,
	E2_SzFltDlgData *rt)
{
//	NEEDOPENBGL
	_e2_size_filter_dialog_response_cb (GTK_DIALOG (rt->dialog),
		E2_RESPONSE_APPLY, rt);
//	NEEDCLOSEBGL
}
/**
@brief create file size filter dialog

The state of @a item when it arrives here is opposite to that
shown in the menu, when clicked

@param item the activated item in the filters menu
@param view data structure for the view to which the file list belongs

@return
*/
void e2_size_filter_dialog_create_cb (GtkCheckMenuItem *item, ViewInfo *view)
{
	GtkWidget *hbox;
	gchar size_string[32];
	E2_Button no_btn;

	//save local copies, for later use in other functions
//  menu_item = item;
//  itemstate = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM (item));
	E2_SzFltDlgData *rt = ALLOCATE (E2_SzFltDlgData);
	CHECKALLOCATEDWARN (rt, return;);
	rt->view = view;

	NEEDCLOSEBGL

	rt->dialog = e2_dialog_create (NULL, _("Display only the items which are:"),
		_("size filter"),
		(ResponseFunc)_e2_size_filter_dialog_response_cb, rt);
	hbox = e2_widget_add_box (
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog)),
#else
		GTK_DIALOG (rt->dialog)->vbox,
#endif
		TRUE, E2_PADDING, FALSE, FALSE, E2_PADDING);

	rt->operation_combo = e2_combobox_add (hbox, FALSE, 0, NULL, NULL, NULL,
		E2_COMBOBOX_MENU_STYLE);
	//the order of these matches the Operator enum in e2_fileview.h
	gchar *size_choices[3] = { _("smaller than"), _("equal to"), _("bigger than") };
	e2_combobox_append_history_counted (rt->operation_combo, 3, size_choices);

	gint index = view->size_filter.op;
	gtk_combo_box_set_active (GTK_COMBO_BOX (rt->operation_combo), index) ;	//0=1st entry index
//	gtk_widget_set_size_request (operation_combo, 150, 30);

	if (view->size_filter.size < (1 << 10))
	{
		g_snprintf (size_string, sizeof (size_string), "%ld",
			(gulong) view->size_filter.size);
		index = 0; 	//0=1st combo entry index
	}
	else if (view->size_filter.size < (1 << 20))
	{
		g_snprintf (size_string, sizeof (size_string), "%.2f",
			(gdouble)((gdouble)view->size_filter.size / (gdouble)(1 << 10)));
		index = 1;
	}
	else
	{
		g_snprintf (size_string, sizeof (size_string), "%.2f",
			(gdouble)((gdouble)view->size_filter.size / (gdouble)(1 << 20)));
		index = 2;
	}

	rt->size_entry = e2_widget_add_entry (hbox, size_string, FALSE, FALSE);
	gtk_widget_set_size_request (rt->size_entry, 100, 30);
#ifdef E2_ASSISTED
	GtkWidget *label = (GtkWidget *) g_object_get_data (G_OBJECT (rt->dialog),
		"e2-dialog-label");
	e2_widget_set_label_relations (label, rt->size_entry);
#endif
	//handle <Return> key-presses when the entry is focused
	g_signal_connect (G_OBJECT (rt->size_entry), "activate",
		G_CALLBACK (_e2_size_filter_dialog_activated_cb), rt);

	rt->units_combo = e2_combobox_add (hbox, FALSE, 0, NULL, NULL, NULL,
		E2_COMBOBOX_MENU_STYLE);
	gchar *units_names [3] = { _("bytes"), _("kbytes"), _("Mbytes") };
	e2_combobox_append_history_counted (rt->units_combo, 3, units_names);
	gtk_combo_box_set_active (GTK_COMBO_BOX (rt->units_combo), index) ;

	//cleanup if the dialog rt data becomes invalid
	g_signal_connect (
#ifdef USE_GTK3_0
	G_OBJECT (view->treeview),
#else
	GTK_OBJECT (view->treeview),
#endif
		"destroy", G_CALLBACK (_e2_size_filter_dialog_destroy_cb), rt);

	//now the buttons
	if (!gtk_check_menu_item_get_active (item))
		//user has just toggled off
		e2_dialog_add_defined_button (rt->dialog, &E2_BUTTON_REMOVE);
	e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);

	e2_dialog_show (rt->dialog, app.main_window, 0,
		&no_btn, &E2_BUTTON_APPLY, NULL);
	NEEDOPENBGL
}
