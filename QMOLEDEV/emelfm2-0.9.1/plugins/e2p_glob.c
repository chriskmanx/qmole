/* $Id: e2p_glob.c 2978 2013-11-30 02:56:32Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark

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
@file plugins/e2p_glob.c
@brief plugin for selecting items by various user-specified rules
*/

#include "emelfm2.h"
#include <string.h>
#include <time.h>
#include <math.h>
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_filelist.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "glob"

//various dialog widgets ...
typedef struct _E2_FltDlgRuntime
{
	GtkWidget *usename_check;
	GtkWidget *name_label;
	GtkWidget *pattern_entry;
	GtkWidget *example_label;
	GtkWidget *case_sensitive_check;
	GtkWidget *invert_check;
	GtkWidget *usesize_check;
	GtkWidget *sizop_combo;
	GtkWidget *size_entry;
	GtkWidget *units_combo;
	GtkWidget *usedate_check;
	GtkWidget *datop_combo;
	GtkWidget *date_entry;
	GtkWidget *apply_button;
} E2_FltDlgRuntime;
//various parameters from dialog widgets ...
typedef struct _E2_FltDlgData
{
	struct
	{
		gboolean use;	//TRUE when name filtering is invoked
		const gchar *pattern;
		gboolean case_sensitive;
		gboolean invert_mask;
	} name_filter;
	struct
	{
		gboolean use;
		Operator op;
		size_t size;
	} size_filter;
	struct
	{
		gboolean use;
		gint time_type;	//one of MTIME, ATIME, CTIME
		Operator op;
		time_t time;	//time_t may be int or long int
	} date_filter;
} E2_FltDlgData;

//the order here needs to be consistent with the timetypes enum and operator enum
//in e2_fileview.h, and matched by the combo entries
//enum/2 = MTIME, ATIME, CTIME and enum%2 = GT, LT
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

static PluginIface iface;

static gboolean use_name, use_size, use_date;
static gint date_index;
static gchar *previous_pattern;
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

//these functions are essentially the same as the filtering functions in e2_filelist.c
/**
@brief decide whether an item should be selected, on the basis of its name

Item name string is localised. Obviously, @a info is expected to survive any
refresh that may occur.

@param info data structure for item being checked
@param data data structure for view being processed

@return TRUE if item 'passes' (i.e. to be selected)
*/
static gboolean _e2p_glob_match_name (FileInfo *info, E2_FltDlgData *data)
{
	gchar *s, *p, *utf, *freeme;
	gchar save;
	gboolean negated, matched, positive_check = FALSE, result = FALSE;

	p = (gchar *) data->name_filter.pattern;
	utf = F_FILENAME_FROM_LOCALE (info->filename);
	//maybe several patterns, separated by commas
	while ((s = strchr (p, ',')) != NULL)	//if always ascii ',', don't need g_utf8_strchr()
	{	//check each pattern that is followed by a ','
		save = *s;
		*s = '\0';
		while (p[0] == ' ')
			p += sizeof (gchar);
		if (p[0] == '!')
		{
			negated = !data->name_filter.invert_mask;
			p += sizeof (gchar);
		}
		else
		{
			negated = data->name_filter.invert_mask;
			if (p[0] == '\\' && p[1] == '!')
				p += sizeof (gchar);
		}
		if (!positive_check)
			positive_check = !negated;

		if (!data->name_filter.case_sensitive)
		{
		  freeme = g_utf8_strdown (utf, -1);
		  matched = g_pattern_match_simple (p, freeme);
		  g_free (freeme);
		}
		else
		  matched = g_pattern_match_simple (p, utf);

		*s = save;
		if (matched && negated)
		{
		  F_FREE (utf, info->filename);
		  return FALSE;
		}
		if (matched && !negated)
		  result = TRUE;	//but keep looking for any later exclude
		//if neither negated nor matched, we don't change result

		s += sizeof (gchar);	//pass the ','
		p = s;
	}
	//check the last (or only) pattern
	while (p[0] == ' ')
		p++;
	if (p[0] == '\0')
		return result;
	if (p[0] == '!')
	{
		negated = !data->name_filter.invert_mask;
		p++;
	}
	else
	{
		negated = data->name_filter.invert_mask;
		if (p[0] == '\\' && p[1] == '!')
			p++;
	}
	if (!positive_check)
		positive_check = !negated;

	if (!data->name_filter.case_sensitive)
	{
		freeme = g_utf8_strdown (utf, -1);
		matched = g_pattern_match_simple (p, freeme);
		g_free (freeme);
	}
	else
		matched = g_pattern_match_simple (p, utf);
	if (matched)
		result = !negated;
	//extra check for unmatched final check
	else if (negated && !positive_check)
		result = TRUE;

	F_FREE (utf, info->filename);
	return result;
}
/**
@brief decide whether an item should be selected, on the basis of its size

Item statbuf.st_size is compared with curent filters

@param info data structure for item being checked
@param data data structure for view being processed

@return TRUE if item 'passes' (i.e. to be selected)
*/
static gboolean _e2p_glob_match_size (FileInfo *info, E2_FltDlgData *data)
{
	switch (data->size_filter.op)
	{
		case GT:
		  return (info->statbuf.st_size > data->size_filter.size);
		  break;
		case LT:
		  return (info->statbuf.st_size < data->size_filter.size);
		  break;
		case EQ:
		  return (info->statbuf.st_size == data->size_filter.size);
		  break;
		default:
		  return TRUE;
		  break;
	}
}
/**
@brief decide whether an item should be selected, on the basis of one of its times

Item statbuf.st_atime is compared with curent filters

@param info data structure for item being checked
@param data data structure for view being processed

@return TRUE if item 'passes' (i.e. to be selected)
*/
static gboolean _e2p_glob_match_date (FileInfo *info, E2_FltDlgData *data)
{
	switch (data->date_filter.time_type)
	{
		case MTIME:
		  if (data->date_filter.op == GT)
		    return (difftime(data->date_filter.time, info->statbuf.st_mtime) <= 0);
		  else
		    return (difftime(data->date_filter.time, info->statbuf.st_mtime) > 0);
		  break;
		case ATIME:
		  if (data->date_filter.op == GT)
		    return (difftime(data->date_filter.time, info->statbuf.st_atime) <= 0);
		  else
		    return (difftime(data->date_filter.time, info->statbuf.st_atime) > 0);
		  break;
		case CTIME:
		  if (data->date_filter.op == GT)
		    return (difftime(data->date_filter.time, info->statbuf.st_ctime) <= 0);
		  else
		    return (difftime(data->date_filter.time, info->statbuf.st_ctime) > 0);
		  break;
		default:
		  return TRUE;
		  break;
	}
}
/**
@brief callback for each enable/disable toggle button
Adjusts the proceed-button sensitivity as needed
@param togglebutton the widget which toggled
@param data pointer to dialog data struct

@return
*/
static void _e2p_glob_toggle_cb (GtkToggleButton *togglebutton,
	E2_FltDlgRuntime *data)
{
	gboolean state;
	state =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (togglebutton);
#else
		togglebutton->active;
#endif
	NEEDCLOSEBGL
	if (togglebutton == (GtkToggleButton*)data->usename_check)
	{
		use_name = state; //cache for session
		gtk_widget_set_sensitive (data->name_label, state);
		gtk_widget_set_sensitive (data->pattern_entry, state);
		gtk_widget_set_sensitive (data->example_label, state);
		gtk_widget_set_sensitive (data->case_sensitive_check, state);
		gtk_widget_set_sensitive (data->invert_check, state);
	}
	else if (togglebutton == (GtkToggleButton*)data->usesize_check)
	{
		use_size = state; //cache for session
		gtk_widget_set_sensitive (data->sizop_combo, state);
		gtk_widget_set_sensitive (data->size_entry, state);
		gtk_widget_set_sensitive (data->units_combo, state);
	}
	else //data->usedate_check
	{
		use_date = state; //cache for session
		gtk_widget_set_sensitive (data->datop_combo, state);
		gtk_widget_set_sensitive (data->date_entry, state);
	}
	state =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(data->usename_check))
	 ||	gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(data->usesize_check))
	 ||	gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(data->usedate_check));
#else
		GTK_TOGGLE_BUTTON (data->usename_check)->active
	 || GTK_TOGGLE_BUTTON (data->usesize_check)->active
	 || GTK_TOGGLE_BUTTON (data->usedate_check)->active;
#endif
	gtk_widget_set_sensitive (data->apply_button, state);
	NEEDOPENBGL
}
/**
@brief callback for all glob-dialog responses

Only an OK response does anything.
This now works by borrowing some data slots from the *curr_view
data struct and then using the same filtering process as applies
to name filtering for filelists. Yeah, a hack, but that's been
debugged ...

@param dialog the dialog where the response was initiated, UNUSED
@param response the number assigned to the widget which triggered the response
@param data pointer to dialog data struct

@return
*/
static void _e2p_glob_response_cb (GtkDialog *dialog, gint response,
	E2_FltDlgRuntime *data)
{
	switch (response)
	{
		case E2_RESPONSE_APPLY:
		{
			const gchar *s;
			E2_FltDlgData choices;
			//assignment for complier-warning prevention only
			memset (&choices, '\0', sizeof (E2_FltDlgData));
			NEEDCLOSEBGL
			//get all the relevant parameters from the dialog
			choices.name_filter.use =
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (data->usename_check));
#else
				GTK_TOGGLE_BUTTON (data->usename_check)->active;
#endif
			if (choices.name_filter.use)
			{
				//filter pattern is utf
				s = gtk_entry_get_text (GTK_ENTRY (data->pattern_entry));
				if (*s == 0)
				{
					choices.name_filter.use = FALSE;
					e2_output_print_error (_("Invalid filename pattern"), FALSE);
				}
				else
				{
					if (previous_pattern != NULL)
						g_free (previous_pattern);
					previous_pattern = g_strdup (s);
					choices.name_filter.pattern = s;
					choices.name_filter.invert_mask =
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (data->invert_check));
#else
						GTK_TOGGLE_BUTTON (data->invert_check)->active;
#endif
					choices.name_filter.case_sensitive =
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (data->case_sensitive_check));
#else
						GTK_TOGGLE_BUTTON (data->case_sensitive_check)->active;
#endif
				}
			}
			choices.size_filter.use =
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (data->usesize_check));
#else
				GTK_TOGGLE_BUTTON (data->usesize_check)->active;
#endif
			if (choices.size_filter.use)
			{
				s = gtk_entry_get_text (GTK_ENTRY (data->size_entry));
				gdouble dsize = atof (s);
				gint index = gtk_combo_box_get_active (GTK_COMBO_BOX (data->units_combo));
				if (index == 1 || index == 2)
					dsize *= pow (1024, index);
				choices.size_filter.size = (size_t) dsize;
				choices.size_filter.op = gtk_combo_box_get_active
					(GTK_COMBO_BOX (data->sizop_combo));	//0=1st entry index, =
			}
			choices.date_filter.use =
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (data->usedate_check));
#else
				GTK_TOGGLE_BUTTON (data->usedate_check)->active;
#endif
			if (choices.date_filter.use)
			{
				gchar date_buf[25];
				choices.date_filter.time = e2_date_filter_dialog_get_time
					(gtk_entry_get_text(GTK_ENTRY (data->date_entry)),
					date_buf, sizeof(date_buf), date_index);
				if (choices.date_filter.time != (time_t)-1)
				{
					gtk_entry_set_text (GTK_ENTRY (data->date_entry), date_buf);
					gint index = gtk_combo_box_get_active (GTK_COMBO_BOX (data->datop_combo));
					//these are tricks consequent on the order of combo entries
					choices.date_filter.time_type = index / 2;
					choices.date_filter.op = index % 2;
				}
				else
				{
					//FIXME warning to user
					choices.date_filter.use = FALSE;
				}
			}
			if (choices.name_filter.use || choices.size_filter.use || choices.date_filter.use)
			{
				GtkTreeIter iter;
				GtkTreeModel *model = curr_view->model;
				if (gtk_tree_model_get_iter_first (model, &iter));
				{	//it's not empty
					FileInfo *info;
					GdkModifierType state;

					if (gtk_get_current_event_state (&state))
						state &= ~GDK_CONTROL_MASK;
					else
						state = 0;

					e2_filelist_disable_refresh ();
					e2_window_set_cursor (GDK_WATCH);
					//wait until any current re-creation is finished
					WAIT_FOR_REFRESH(curr_view)

					GtkTreeSelection *sel = curr_view->selection;
					if (state == 0)
						gtk_tree_selection_unselect_all (sel);	//start with clean slate

					do
					{
						gtk_tree_model_get (model, &iter, FINFO, &info, -1);
					//note that the pattern string may be altered, but is supposed to be constant ?
						gboolean selectme = FALSE;
						if (choices.name_filter.use)
							selectme = selectme || _e2p_glob_match_name (info, &choices);
						if (choices.size_filter.use)
							selectme = selectme || _e2p_glob_match_size (info, &choices);
						if (choices.date_filter.use)
							selectme = selectme || _e2p_glob_match_date (info, &choices);
						if (selectme)
							gtk_tree_selection_select_iter (sel, &iter);
					} while (gtk_tree_model_iter_next (model, &iter));

					e2_window_set_cursor (GDK_LEFT_PTR);
					e2_filelist_enable_refresh ();
				}
			}
			NEEDOPENBGL
		}
			break;
		default:
			break;
	}
}
/**
@brief create and run selection-filter dialog

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2p_glob (gpointer from, E2_ActionRuntime *art)
{
	GtkWidget *dialog;
	GtkWidget *dialog_vbox, *hbox;
	E2_FltDlgRuntime data;
	gint index;
	gchar *utf;

	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	//we check for selected item here, so prevent disruption
	e2_filelist_disable_one_refresh ((rt==curr_pane)?PANEACTIVE:PANEINACTIVE);

	dialog = e2_dialog_create (NULL, _("Select items:"), _("selection filter"),
		(ResponseFunc)_e2p_glob_response_cb, &data);
	dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (dialog));
#else
		GTK_DIALOG (dialog)->vbox;
#endif
	//the name-related things ...
	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, 0);
	data.usename_check = e2_button_add_toggle (hbox, TRUE, use_name, NULL,
		NULL, FALSE, 0, _e2p_glob_toggle_cb, &data);
	data.name_label = e2_widget_add_mid_label (hbox, _("Named like"), 0.0, FALSE, 0);
	data.pattern_entry = e2_widget_add_entry (hbox, "", TRUE, FALSE);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (data.name_label, data.pattern_entry);
#endif
	FileInfo *info = e2_fileview_get_selected_first_local (&rt->view, FALSE);
	if (info != NULL)
	{
		gchar *text, *s;
		utf = F_FILENAME_FROM_LOCALE (info->filename);
		if ((s = strrchr (utf, '.')) != NULL && (s > utf))	//not for hidden items
		  text = g_strconcat ("*",s, NULL);
		else
		  text = utf;
		gtk_entry_set_text (GTK_ENTRY(data.pattern_entry), text);
		if (text != utf)
			g_free (text);
		F_FREE (utf, info->filename);
	}
	else if (previous_pattern != NULL)
		gtk_entry_set_text (GTK_ENTRY(data.pattern_entry), previous_pattern);
	data.example_label = e2_widget_add_mid_label (hbox, _("example: *~,*.?"), 0.0, FALSE, 0);

	e2_filelist_enable_one_refresh ((rt==curr_pane)?PANEACTIVE:PANEINACTIVE);

	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, 0);
	data.invert_check = e2_button_add_toggle (hbox, TRUE, FALSE,
		_("_Invert"),
		_("Select items that DO NOT match the given mask"),
		FALSE, 20, NULL, NULL);
	data.case_sensitive_check = e2_button_add_toggle (hbox, TRUE, TRUE,
		_("Case _sensitive"),
		NULL,
		FALSE, 20, NULL, NULL);

	e2_widget_add_separator (dialog_vbox, TRUE, 1);

	//the size-related things ...
	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, 0);
	data.usesize_check = e2_button_add_toggle (hbox, TRUE, use_size, NULL,
		NULL, FALSE, 0, _e2p_glob_toggle_cb, &data);
	data.sizop_combo = e2_combobox_add (hbox, FALSE, 0, NULL, NULL, NULL,
	  E2_COMBOBOX_MENU_STYLE);
	//don't change the order of these - the index is used as an enumerator
	gchar *size_choices[3] = { _("smaller than"), _("equal to"), _("bigger than") };
	e2_combobox_append_history_counted (data.sizop_combo, 3, size_choices);
	gtk_combo_box_set_active (GTK_COMBO_BOX (data.sizop_combo), 0);	//0=1st entry index

	gchar size_string[32];

	if (info != NULL)
	{
		if (info->statbuf.st_size < (1 << 10))
		{
			g_snprintf (size_string, sizeof (size_string), "%ld",
				(gulong) info->statbuf.st_size);
			index = 0; 	//0=1st combo entry index, bytes
		}
		else if (info->statbuf.st_size < (1 << 20))
		{
			g_snprintf (size_string, sizeof (size_string), "%.2f",
				(gdouble)((gdouble)info->statbuf.st_size / (gdouble)(1 << 10)));
			index = 1;
		}
		else
		{
			g_snprintf (size_string, sizeof (size_string), "%.2f",
				(gdouble)((gdouble)info->statbuf.st_size / (gdouble)(1 << 20)));
			index = 2;
		}
	}
	else
	{
		size_string[0] = '\0';
		index = 0;
	}

	data.size_entry = e2_widget_add_entry (hbox, size_string, TRUE, FALSE);
//	gtk_widget_set_size_request (data.size_entry, 100, 30);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (data.sizop_combo, data.size_entry);
#endif

	data.units_combo = e2_combobox_add (hbox, FALSE, 0, NULL, NULL, NULL,
	  E2_COMBOBOX_MENU_STYLE);
	gchar *size_names[3] = { _("bytes"), _("kbytes"), _("Mbytes") };
	e2_combobox_append_history_counted (data.units_combo, 3, size_names);
	gtk_combo_box_set_active (GTK_COMBO_BOX (data.units_combo), index);

	e2_widget_add_separator (dialog_vbox, TRUE, 1);

	//the date-related things ...
	hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE, FALSE, 0);
	data.usedate_check = e2_button_add_toggle (hbox, TRUE, use_date, NULL,
		NULL, FALSE, 0, _e2p_glob_toggle_cb, &data);
	data.datop_combo = e2_combobox_add (hbox, FALSE, 0, NULL, NULL, NULL,
	  E2_COMBOBOX_MENU_STYLE);
	// don't change the order of these - index value is used in callback
	gchar *date_choices [MAXDATECHOICES] = {
		_("modified since"), _("modified before"), _("accessed since"),
		_("accessed before"),  _("changed since"), _("changed before") };
	e2_combobox_append_history_counted (data.datop_combo, MAXDATECHOICES,
		date_choices);
	gtk_combo_box_set_active (GTK_COMBO_BOX (data.datop_combo), MODIFIED_SINCE);

	gchar date_string[16];
	if (info != NULL)
	{
		//get & set which date format to use
		date_index = e2_option_int_get ("date-string");
		if (date_index > 5)
			date_index = 0;	//out of range, use default format (should never happen)

		struct tm *tm_ptr = localtime (&(info->statbuf.st_mtime));
		strftime (date_string, sizeof (date_string), date_format[date_index], tm_ptr);
		utf = e2_utf8_from_locale (date_string);
	}
	else
		utf = "";

	data.date_entry = e2_widget_add_entry (hbox, utf, TRUE, FALSE);
	if (info != NULL)
		g_free (utf);
//	gtk_widget_set_size_request (data.date_entry, 120, 30);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (data.datop_combo, data.date_entry);
#endif
	//start at the name entry
	gtk_editable_select_region (GTK_EDITABLE(data.pattern_entry), 0, -1);
	gtk_widget_grab_focus (data.pattern_entry);

	E2_Button local_btn;
	e2_button_derive (&local_btn, &E2_BUTTON_APPLY, BTN_YES_CONTINUE);

	e2_dialog_add_defined_button (dialog, &E2_BUTTON_CANCEL);
	data.apply_button = e2_dialog_add_defined_button (dialog, &local_btn);
	//set initial sensitivities
	NEEDOPENBGL
	_e2p_glob_toggle_cb ((GtkToggleButton*)data.usename_check, &data);
	_e2p_glob_toggle_cb ((GtkToggleButton*)data.usesize_check, &data);
	_e2p_glob_toggle_cb ((GtkToggleButton*)data.usedate_check, &data);
	NEEDCLOSEBGL

	e2_dialog_setup (dialog, app.main_window);
//	e2_dialog_resize (dialog, 1.3);	//really, we only want to make it wider ..
	//block until the user selects
	e2_dialog_run (dialog, app.main_window, E2_DIALOG_BLOCKED | E2_DIALOG_FREE);

	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(7),_("glob"),_e2p_glob,
		_("_Glob.."),
		_("Select items matching a specified pattern"),
		"plugin_"ANAME E2ICONTB)
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	if (previous_pattern != NULL)
		g_free (previous_pattern);
	return ret;
}
