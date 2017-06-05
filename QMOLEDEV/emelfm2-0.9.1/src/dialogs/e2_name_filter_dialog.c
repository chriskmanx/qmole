/* $Id: e2_name_filter_dialog.c 2815 2013-10-13 07:00:55Z tpgww $

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

/**
@file src/dialogs/e2_name_filter_dialog.c
@brief dialog for setting up arrangements for name-filtering
to be applied to a filelist
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_dialog.h"
#include "e2_filelist.h"

typedef struct _E2_NmFltDlgData
{
	GtkWidget *dialog;
	GtkWidget *pattern_entry;
	GtkWidget *invert_check;
	GtkWidget *case_sensitive_check;
//	GtkWidget *menu_item;	//copy of pointer to clicked filters menu item
//	gboolean itemstate;	//initial state of menu_item
//	gboolean blocked = FALSE;	//flag to prevent toggles when not wanted
	ViewInfo *view;		//data for filtered file pane
} E2_NmFltDlgData;

static void _e2_name_filter_dialog_response_cb (GtkDialog *dialog,
	gint response, E2_NmFltDlgData *rt);

/*static void _e2_name_filter_dialog_item_toggle_cb (void)
{
	gulong handler = g_signal_handler_find (menu_item, G_SIGNAL_MATCH_FUNC,
	0, 0, NULL, e2_name_filter_dialog_create_cb, NULL);
	g_signal_handler_block (menu_item, handler);
	NEEDCLOSEBGL
	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (menu_item), !itemstate);
	NEEDOPENBGL
	g_signal_handler_unblock (menu_item, handler);
}
*/
/*
Called only from inside callback, with BGL closed
*/
static void _e2_name_filter_dialog_ok (E2_NmFltDlgData *data)
{
  const gchar *s = gtk_entry_get_text (GTK_ENTRY (data->pattern_entry));
  if (*s == '\0')
    e2_output_print_error (_("Invalid filename pattern"), FALSE);
  else
  {
	//filter pattern is saved as utf, as required for matching, later
//	g_strlcpy (view->name_filter.pattern, s,
//		sizeof (view->name_filter.pattern));
	//save cache data (actual, maybe != s)
	g_free (data->view->name_filter.patternptr);
//	data->view->name_filter.patternptr = g_strdup (view->name_filter.pattern);
	data->view->name_filter.patternptr = g_strdup (s);

	data->view->name_filter.invert_mask =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (data->invert_check));
#else
		GTK_TOGGLE_BUTTON (data->invert_check)->active;
#endif
	data->view->name_filter.case_sensitive =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (data->case_sensitive_check));
#else
		GTK_TOGGLE_BUTTON (data->case_sensitive_check)->active;
#endif
/*	if (! itemstate)
	{
		//we're staying filtered, so re-toggle the menu item
		NEEDOPENBGL
		_e2_name_filter_dialog_item_toggle_cb ();
		NEEDCLOSEBGL
	}
*/
    //and conform the cached copy
	data->view->name_filter.active = TRUE;
	//show the results
	e2_toolbar_toggle_filter_button (data->view);
	e2_fileview_refilter_list (data->view);
#ifdef E2_STATUS_DEMAND
	if (data->view == curr_view)
    {
	    OPENBGL
	    e2_window_update_status_bar (NULL);
        CLOSEBGL
    }
#endif
  }
}
/**
@brief cleanup during the destruction of the view related to a dialog
@param object UNUSED the view-related object being destroyed
@param rt pointer to data struct for the dialog
@return
*/
static void _e2_name_filter_dialog_destroy_cb (
#ifdef USE_GTK3_0
	GtkWidget *object,
#else
	GtkObject *object,
#endif
	E2_NmFltDlgData *rt)
{
	g_signal_handlers_disconnect_by_func ((gpointer)rt->dialog,
		_e2_name_filter_dialog_response_cb, rt); //no double-handling
//	NEEDOPENBGL
	_e2_name_filter_dialog_response_cb (GTK_DIALOG (rt->dialog), 0, rt);
//	NEEDCLOSEBGL
}
/**
@brief dialog response callback

@param dialog the permissions-dialog
@param response the response for the clicked button
@param rt pointer to dialog data struct

@return
*/
static void _e2_name_filter_dialog_response_cb (GtkDialog *dialog,
	gint response, E2_NmFltDlgData *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_APPLY:
			gtk_widget_hide (rt->dialog);
			_e2_name_filter_dialog_ok (rt);
			break;
		case E2_RESPONSE_REMOVE:
			//update the cached copy of the state
			rt->view->name_filter.active = FALSE;
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
		_e2_name_filter_dialog_destroy_cb, rt);
	gtk_widget_destroy (rt->dialog);
	NEEDOPENBGL
	DEALLOCATE (E2_NmFltDlgData, rt);
}
/**
@brief handle Return keypresses in the size entry
@param entry UNUSED the entry for the combo box
@param rt pointer to dialog data struct
@return
*/
static void _e2_name_filter_dialog_activated_cb (GtkEntry *entry,
	E2_NmFltDlgData *data)
{
//	NEEDOPENBGL
	_e2_name_filter_dialog_response_cb (GTK_DIALOG (data->dialog),
		E2_RESPONSE_APPLY, data);
//	NEEDCLOSEBGL
}
/**
@brief create file name filter dialog

The state of @a item when it arrives here is opposite to that shown in the menu,
when clicked

@param item the activated item in the filters menu
@param view data structure for the view to which the file list belongs

@return
*/
void e2_name_filter_dialog_create_cb (GtkCheckMenuItem *item, ViewInfo *view)
{
	E2_Button no_btn;
	E2_NmFltDlgData *rt = ALLOCATE (E2_NmFltDlgData);
	CHECKALLOCATEDWARN (rt, return;);
	rt->view = view;

	//save local copies, for later use in other functions
//	rt->menu_item = item;
//	rt->itemstate = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM (item));
	NEEDCLOSEBGL
	rt->dialog = e2_dialog_create (NULL, _("Display only the items named like:"),
		_("name filter"), (ResponseFunc)_e2_name_filter_dialog_response_cb, rt);
	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog));
#else
		GTK_DIALOG (rt->dialog)->vbox;
#endif
	rt->pattern_entry = e2_widget_add_entry (dialog_vbox, view->name_filter.patternptr,
		FALSE, FALSE);
#ifdef E2_ASSISTED
	GtkWidget *label = (GtkWidget *) g_object_get_data (G_OBJECT (rt->dialog),
		"e2-dialog-label");
	e2_widget_set_label_relations (label, rt->pattern_entry);
#endif
	//handle <Return> key-presses when the entry is focused
	g_signal_connect (G_OBJECT (rt->pattern_entry), "activate",
		G_CALLBACK (_e2_name_filter_dialog_activated_cb), rt);

	e2_widget_add_mid_label (dialog_vbox, _("example: *~,*.?"), 0.0, FALSE, 0);

	GtkWidget *table = e2_widget_add_table (dialog_vbox, 1, 2, FALSE, FALSE, 0);
	rt->invert_check = e2_button_add_toggle_to_table (table, _("Invert"),
		view->name_filter.invert_mask, NULL, NULL, 0, 1, 0, 1);
	e2_widget_set_safetip (rt->invert_check,
		_("Show files that DO NOT match the given mask"));
	rt->case_sensitive_check = e2_button_add_toggle_to_table (table, _("Case sensitive"),
		view->name_filter.case_sensitive, NULL, NULL, 1, 2, 0, 1);

	//cleanup if the dialog rt data becomes invalid
	g_signal_connect (
#ifdef USE_GTK3_0
	G_OBJECT (view->treeview),
#else
	GTK_OBJECT (view->treeview),
#endif
		"destroy", G_CALLBACK (_e2_name_filter_dialog_destroy_cb), rt);

	//now the buttons
	if (!gtk_check_menu_item_get_active (item))
		//user has just toggled off
		e2_dialog_add_defined_button (rt->dialog, &E2_BUTTON_REMOVE);
	e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);

	e2_dialog_show (rt->dialog, app.main_window, 0,
		&no_btn, &E2_BUTTON_APPLY, NULL);
	NEEDOPENBGL
}
