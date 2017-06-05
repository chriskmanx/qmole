/* $Id: e2_date_filter_dialog.c 2815 2013-10-13 07:00:55Z tpgww $

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
//#define _XOPEN_SOURCE // required for strptime in glibc-2
#include <time.h>
#include "e2_dialog.h"
#include "e2_filelist.h"

enum
{
	MODIFIED_SINCE,
	MODIFIED_BEFORE,
	ACCESSED_SINCE,
	ACCESSED_BEFORE,
	CHANGED_SINCE,
	CHANGED_BEFORE,
	MAXDATECHOICES
};

typedef struct _E2_DtFltDlgData
{
	GtkWidget *dialog;
	GtkWidget *date_entry;
	GtkWidget *operation_combo;
	gint date_index;	//the date format array index to use
//	GtkWidget *menu_item;	//copy of pointer to clicked filters menu item
//	gboolean itemstate;
//	gboolean blocked = FALSE;	//flag to prevent toggles when not wanted
	ViewInfo *view;		//data for filtered file pane
} E2_DtFltDlgData;

static void _e2_date_filter_dialog_response_cb (GtkDialog *dialog,
	gint response, E2_DtFltDlgData *rt);

//these need to be in same order as config data/dialog date options
gchar *date_format [6] =
{
	"%d/%m/%Y",	//default
	"%d/%m/%Y",	//standard
	"%m/%d/%Y",	//american
	"%Y-%m-%d", //ISO
	"%x",		//localised
	"%x"		//custom, NOT value of custom-format option, which may have something odd
};

/**
@brief determine time value according to absolute or relative string in @a entry
@param datestring user-entered string to parse
@param buf stack buffer for returning corrected string, or NULL
@param bufsize byte-length of @a buf
@param date_index relevant index (0-5) for date_format[] strings

@return the time-value corresponding to string in @a entry, or -1 upon error
*/
time_t e2_date_filter_dialog_get_time (const gchar *datestring,
	gchar *buf, gint bufsize, gint date_index)
{
	enum { RELHOURS, RELDAYS, RELMONTHS, RELYEARS, ABSDATE };
	gint datetype;
	struct tm tm_time;

	//FIXME this is too crude for parsing relative dates
	if (strstr (datestring, _("hour")) != NULL)
		datetype = RELHOURS;
	else if (strstr (datestring, _("day")) != NULL)
		datetype = RELDAYS;
	else if (strstr (datestring, _("month")) != NULL)
		datetype = RELMONTHS;
	else if (strstr (datestring, _("year")) != NULL)
		datetype = RELYEARS;
	else
		datetype = ABSDATE;

	switch (datetype)
	{
		case RELHOURS:
		case RELDAYS:
		case RELMONTHS:
		case RELYEARS:
		{
			glong count;
			const gchar *d = datestring;
			//find 1st digit
			while (*d != '\0' && *d < '0' && *d > '9') d++;
			if (*d != '\0')
			{
//				const gchar *tail;
				count = strtol (d, NULL/*&tail*/, 10);
				if (count == 0)
					count = 1;
				else if (count < 0)
					count = -count;
			}
			else
				count = 1;	//default

			time_t cutoff = time (NULL);
			if (datetype == RELHOURS)
				cutoff -= count * 3600;
			else if (datetype == RELDAYS)
				cutoff -= count * 86400;
			localtime_r (&cutoff, &tm_time);
			if (datetype == RELMONTHS)
			{
				while (count > 11)
				{
					count -= 12;
					tm_time.tm_year--;
				}
				if (tm_time.tm_mon >= count)
					tm_time.tm_mon -= count;
				else
				{
					tm_time.tm_year--;
					tm_time.tm_mon += (12-count);
				}
			}
			else if (datetype == RELYEARS)
				tm_time.tm_year -= count;
			tm_time.tm_sec = 0;
			tm_time.tm_min = 0;
			tm_time.tm_hour = 0;
		}
			break;
		default:
		{
			gchar *_datestring = g_strconcat (datestring, " 00:00:00", NULL);
			gchar *strf_withtime = g_strconcat (date_format [date_index],
				" %T", NULL);
			strptime (_datestring, strf_withtime, &tm_time);
			g_free (_datestring);
			g_free (strf_withtime);
		}
			break;
	}

	time_t result = mktime (&tm_time);

	if (result != (time_t)-1 && buf != NULL)
		strftime (buf, bufsize, date_format[date_index], &tm_time);

	return result;
}

/*static void _e2_date_filter_dialog_item_toggle_cb (void)
{
	gulong handler = g_signal_handler_find (menu_item, G_SIGNAL_MATCH_FUNC,
		0, 0, NULL, e2_date_filter_dialog_create_cb, NULL);
	NEEDCLOSEBGL
	g_signal_handler_block (menu_item, handler);
	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (menu_item), !itemstate);
	g_signal_handler_unblock (menu_item, handler);
	NEEDOPENBGL
}
*/
static void _e2_date_filter_dialog_ok (E2_DtFltDlgData *data)
{
	gchar date_buf[25];
	data->view->date_filter.time = e2_date_filter_dialog_get_time
		(gtk_entry_get_text(GTK_ENTRY (data->date_entry)),
		date_buf, sizeof(date_buf), data->date_index);
	if (data->view->date_filter.time == (time_t)-1)
	{
		//FIXME warning to user
		return;
	}
	gtk_entry_set_text (GTK_ENTRY (data->date_entry), date_buf);

	gint index = gtk_combo_box_get_active (GTK_COMBO_BOX (data->operation_combo));
	//see below for the strings provided to the combo
	switch (index)
	{
		case ACCESSED_SINCE:
			data->view->date_filter.op = GT;
			data->view->date_filter.time_type = ATIME;
			break;
		case ACCESSED_BEFORE:
			data->view->date_filter.op = LT;
			data->view->date_filter.time_type = ATIME;
			break;
		case MODIFIED_SINCE:
			data->view->date_filter.op = GT;
			data->view->date_filter.time_type = MTIME;
			break;
		case MODIFIED_BEFORE:
			data->view->date_filter.op = LT;
			data->view->date_filter.time_type = MTIME;
			break;
		case CHANGED_SINCE:
			data->view->date_filter.op = GT;
			data->view->date_filter.time_type = CTIME;
			break;
		case CHANGED_BEFORE:
			data->view->date_filter.op = LT;
			data->view->date_filter.time_type = CTIME;
			break;
		default:
			break;
	}

	//  if (! itemstate)
	 //we're staying filtered, so re-toggle the menu item
	//	_e2_date_filter_dialog_item_toggle_cb ();
	//and conform the cached copy
	data->view->date_filter.active = TRUE;

	//show the results
	if (index > -1)
	{
		e2_toolbar_toggle_filter_button (data->view);
		e2_fileview_refilter_list (data->view);
#ifdef E2_STATUS_DEMAND
	    if (data->view == curr_view)
		{
BGL?		OPENBGL
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
static void _e2_date_filter_dialog_destroy_cb (
#ifdef USE_GTK3_0
	GtkWidget *object,
#else
	GtkObject *object,
#endif
	E2_DtFltDlgData *rt)
{
	g_signal_handlers_disconnect_by_func ((gpointer)rt->dialog,
		_e2_date_filter_dialog_response_cb, rt); //no double-handling
	_e2_date_filter_dialog_response_cb (GTK_DIALOG (rt->dialog), 0, rt);
}
/**
@brief dialog response callback

@param dialog the permissions-dialog
@param response the response for the clicked button
@param rt pointer to dialog data struct

@return
*/
static void _e2_date_filter_dialog_response_cb (GtkDialog *dialog,
	gint response, E2_DtFltDlgData *rt)
{
	NEEDCLOSEBGL

	switch (response)
	{
		case E2_RESPONSE_APPLY:
			gtk_widget_hide (rt->dialog);
			_e2_date_filter_dialog_ok (rt);
			break;
		case E2_RESPONSE_REMOVE:
			//update the cached copy of the state
			rt->view->date_filter.active = FALSE;
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
			//the menu item will be re-toggled when dialog is destroyed ??
			break;
	}
	g_signal_handlers_disconnect_by_func ((gpointer)rt->view->treeview,
		_e2_date_filter_dialog_destroy_cb, rt);
	gtk_widget_destroy (rt->dialog);
	
	NEEDOPENBGL

	DEALLOCATE (E2_DtFltDlgData, rt);
}
/**
@brief handle activation (<Return> keypresses) in the query entry

@param entry UNUSED the entry widget for the combo box
@param data pointer to dialog data struct
@return
*/
static void _e2_date_filter_dialog_activated_cb (GtkEntry *entry,
	E2_DtFltDlgData *data)
{
	_e2_date_filter_dialog_response_cb (GTK_DIALOG (data->dialog),
		E2_RESPONSE_APPLY, data);
}
/**
@brief create file date filter dialog

The state of @a item when it arrives here is opposite to that shown in the menu,
when clicked

@param item the activated item in the filters menu
@param view data structure for the view to which the file list belongs

@return
*/
void e2_date_filter_dialog_create_cb (GtkCheckMenuItem *item, ViewInfo *view)
{
	GtkWidget *hbox;
	gchar date_string[16];
	gchar *utf;
	struct tm *tm_ptr;
	gint index;
	E2_Button no_btn;
	E2_DtFltDlgData *rt = ALLOCATE (E2_DtFltDlgData);
	CHECKALLOCATEDWARN (rt, return;);
	rt->view = view;

	//save local copies, for later use in other functions
//	rt->menu_item = item;
//	rt->itemstate = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM (item));

	NEEDCLOSEBGL

	rt->dialog = e2_dialog_create (NULL, _("Display only the items:"),
		_("date filter"),
		(ResponseFunc)_e2_date_filter_dialog_response_cb, rt);
	hbox = e2_widget_add_box (
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog)),
#else
		GTK_DIALOG (rt->dialog)->vbox,
#endif
		TRUE, E2_PADDING, FALSE, FALSE, E2_PADDING);

	rt->operation_combo = e2_combobox_add (hbox, FALSE, 0, NULL, NULL, NULL,
		E2_COMBOBOX_MENU_STYLE);
	//don't change the order of these - index value is used below and in callback
	gchar *date_choices [MAXDATECHOICES] = {
		_("modified since"), _("modified before"), _("accessed since"),
		_("accessed before"), _("changed since"), _("changed before") };
	e2_combobox_append_history_counted (rt->operation_combo, MAXDATECHOICES,
		date_choices);
	switch (view->date_filter.time_type)
	{
		case MTIME:
		  index = (view->date_filter.op == GT) ? MODIFIED_SINCE : MODIFIED_BEFORE;	//0 = 1st combo string
		  break;
		case ATIME:
		  index = (view->date_filter.op == GT) ? ACCESSED_SINCE : ACCESSED_BEFORE;
		  break;
		case CTIME:
		  index = (view->date_filter.op == GT) ? CHANGED_SINCE : CHANGED_BEFORE;
		  break;
		default:
		  index = MODIFIED_SINCE;
		  break;
	}
	gtk_combo_box_set_active (GTK_COMBO_BOX (rt->operation_combo), index) ;

	//get which date format to use
	rt->date_index = e2_option_int_get ("date-string");
	if (rt->date_index > 5)
		rt->date_index = 0;	//out of range, use default format (should never happen)

	tm_ptr = localtime (&(view->date_filter.time));
	strftime (date_string, sizeof (date_string), date_format[rt->date_index], tm_ptr);
	utf = e2_utf8_from_locale (date_string);
	rt->date_entry = e2_widget_add_entry (hbox, utf, FALSE, FALSE);
	g_free (utf);
	gtk_widget_set_size_request (rt->date_entry, 120, 30);
#ifdef E2_ASSISTED
	GtkWidget *label = (GtkWidget *) g_object_get_data (G_OBJECT (rt->dialog),
		"e2-dialog-label");
	e2_widget_set_label_relations (label, rt->date_entry);
#endif
	//handle <Return> key-presses when the entry is focused
	g_signal_connect (G_OBJECT (rt->date_entry), "activate",
		G_CALLBACK (_e2_date_filter_dialog_activated_cb), rt);

	//cleanup if the dialog rt data becomes invalid
	g_signal_connect (
#ifdef USE_GTK3_0
	G_OBJECT (view->treeview),
#else
	GTK_OBJECT (view->treeview),
#endif
		"destroy", G_CALLBACK (_e2_date_filter_dialog_destroy_cb), rt);

	//now the buttons
	if (!gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM (item)))
		//user has just toggled off
		e2_dialog_add_defined_button (rt->dialog, &E2_BUTTON_REMOVE);
	e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);

	e2_dialog_show (rt->dialog, app.main_window, 0,
		&no_btn, &E2_BUTTON_APPLY, NULL);
	NEEDOPENBGL
}
