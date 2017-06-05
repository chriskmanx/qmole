/* $Id: e2p_track.c 2840 2013-10-24 10:02:23Z tpgww $

Copyright (C) 2006-2013 tooar <tooar@emelfm2.net>

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
#include <string.h>
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_icons.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "track"

typedef struct _E2_TrackDlgData
{
	GtkWidget *dialog;
	GtkWidget *service_combo;
	GtkWidget *query_combo;
	GtkWidget *type_radio;	//1st member of search-type radio group
	GtkWidget *type_radio2;	//2nd member of search-type radio group
} E2_TrackDlgData;

//tracker stuff, from various tracker 0.6.0 headers
/*
enum {
	SERVICE_FILES,
	SERVICE_FOLDERS,
	SERVICE_DOCUMENTS,
	SERVICE_IMAGES,
	SERVICE_MUSIC,
	SERVICE_VIDEOS,
	SERVICE_TEXT_FILES,
	SERVICE_DEVELOPMENT_FILES,
	SERVICE_OTHER_FILES,
	SERVICE_VFS_FILES,
	SERVICE_VFS_FOLDERS,
	SERVICE_VFS_DOCUMENTS,
	SERVICE_VFS_IMAGES,
	SERVICE_VFS_MUSIC,
	SERVICE_VFS_VIDEOS,
	SERVICE_VFS_TEXT_FILES,
	SERVICE_VFS_DEVELOPMENT_FILES,
	SERVICE_VFS_OTHER_FILES,
	SERVICE_CONVERSATIONS,
	SERVICE_PLAYLISTS,
	SERVICE_APPLICATIONS,
	SERVICE_CONTACTS,
	SERVICE_EMAILS,
	SERVICE_EMAILATTACHMENTS,
	SERVICE_APPOINTMENTS,
	SERVICE_TASKS,
	SERVICE_BOOKMARKS,
	SERVICE_HISTORY,
	SERVICE_PROJECTS,
	MAXSERVICES
};
*/

#define ACTIVE_TRACKER_SERVICES 11

static PluginIface iface;

static gchar *cmd_str [ACTIVE_TRACKER_SERVICES] =
{	//these service names are hardcoded in tracker-files, and not translated
	//same order here as enum
//	NULL,	//not interested in Files
//	NULL,	//not interested in "Folders",
	"Documents",
	"Images",
	"Music",
	"Videos",
	"Text",
	"Development",
	"Other",
//	NULL,	//SERVICE_VFS_FILES,
//	NULL,	//SERVICE_VFS_FOLDERS,
//	NULL,	//SERVICE_VFS_DOCUMENTS,
//	NULL,	//SERVICE_VFS_IMAGES,
//	NULL,	//SERVICE_VFS_MUSIC,
//	NULL,	//SERVICE_VFS_VIDEOS,
//	NULL,	//SERVICE_VFS_TEXT_FILES,
//	NULL,	//SERVICE_VFS_DEVELOPMENT_FILES,
//	NULL,	//SERVICE_VFS_OTHER_FILES,
	"Conversations",	//"GaimConversations",
//	NULL,	//SERVICE_PLAYLISTS,
	"Applications",
//	NULL,	//SERVICE_CONTACTS,
	"Emails",	//"EvolutionEmails" "KMailEmails",
	"EmailAttachments",	//	"EvolutionAttachments" 	"KMailAttachments",
//	NULL,	//SERVICE_APPOINTMENTS,
//	NULL,	//SERVICE_TASKS,
//	NULL,	//SERVICE_BOOKMARKS,
//	NULL,	//SERVICE_HISTORY,
//	NULL,	//SERVICE_PROJECTS,
};

static gchar *object_names [ACTIVE_TRACKER_SERVICES] =
{ //these service strings are in same order as enum
//	N_("all files"),	//not actually interested in "Files", this is a proxy
//	NULL,	//not interested in "Folders",
	N_("office documents"),
	N_("images"),
	N_("music"),
	N_("videos"),
	N_("text files"),
	N_("development files"),
	N_("other files"),
//	NULL,	//SERVICE_VFS_FILES,
//	NULL,	//SERVICE_VFS_FOLDERS,
//	NULL,	//SERVICE_VFS_DOCUMENTS,
//	NULL,	//SERVICE_VFS_IMAGES,
//	NULL,	//SERVICE_VFS_MUSIC,
//	NULL,	//SERVICE_VFS_VIDEOS,
//	NULL,	//SERVICE_VFS_TEXT_FILES,
//	NULL,	//SERVICE_VFS_DEVELOPMENT_FILES,
//	NULL,	//SERVICE_VFS_OTHER_FILES,
	N_("conversations"),
//	NULL,	//N_("playlists") SERVICE_PLAYLISTS,
	N_("applications"),
//	NULL,	//N_("people") SERVICE_CONTACTS,
	N_("emails"),	//"EvolutionEmails" "KMailEmails",
	N_("email attachments"),	//"EvolutionAttachments" 	"KMailAttachments",
//	NULL,	//N_("appointments") SERVICE_APPOINTMENTS,
//	NULL,	//N_("tasks") SERVICE_TASKS,
//	NULL,	//N_("bookmarks") SERVICE_BOOKMARKS,
//	NULL,	//N_("notes")SERVICE_HISTORY,
//	NULL,	//N_("projects") SERVICE_PROJECTS,
};

static gint service_index = -1;	//undefined selection
static GList *query_history = NULL;

/**
@brief response callback for plugin selection dialog

@param dialog the dialog where the response was triggered
@param response the response assigned to the activated button widget
@param data ptr to plugins option data struct

@return
*/
static void _e2p_track_choose_response_cb (GtkDialog *dialog, gint response,
	E2_TrackDlgData *rt)
{
	if (response == E2_RESPONSE_USER1)
		return;	//ignore toggles of hidden-items
	NEEDCLOSEBGL
	if (response == GTK_RESPONSE_OK)
	{
		//returns localized string
		gchar *local = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		if (local != NULL)
		{	//no conversion error
			gchar *utf = F_FILENAME_FROM_LOCALE (local);
			gtk_entry_set_text (
#ifdef USE_GTK2_14
				GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->query_combo))),
#else
				GTK_ENTRY (GTK_BIN (rt->query_combo)->child),
#endif
				utf);
			g_free (local);
			F_FREE (utf, local);
		}
	}
	gtk_widget_destroy (GTK_WIDGET (dialog));
	gtk_widget_grab_focus (
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->query_combo))
#else
		GTK_BIN (rt->query_combo)->child
#endif
	);
	NEEDOPENBGL
}
/**
@brief bring up a system find-file window to choose a query

@param button UNUSED activated widget
@param rt ptr to dialog data struct

@return
*/
static void _e2p_track_choose_rdf_cb (GtkButton *button, E2_TrackDlgData *rt)
{
	NEEDCLOSEBGL
	//no need for vfs support here
	GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
		GTK_WINDOW (rt->dialog), GTK_FILE_CHOOSER_ACTION_OPEN, NULL, NULL);
	e2_dialog_setup_chooser (dialog,
		_("choose query"),
		NULL,	//CWD
		GTK_FILE_CHOOSER_ACTION_OPEN,
		FALSE,	//hide-hidden
		FALSE,	//single-selection
		GTK_RESPONSE_OK,	//default response
		STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
		STOCK_NAME_OPEN, GTK_RESPONSE_OK,
		NULL);

	GtkFileFilter *filter = gtk_file_filter_new ();
	gtk_file_filter_set_name (GTK_FILE_FILTER (filter), _("rdf"));
	gtk_file_filter_add_pattern (filter, "*.rdf");
	gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (dialog), filter);

/*	//hide the dialog's standard 'open' button
	GtkContainer *bbox = GTK_CONTAINER (
#ifdef USE_GTK2_14
		gtk_dialog_get_action_area (GTK_DIALOG (dialog));
#else
		GTK_DIALOG (dialog)->action_area);
#endif
	GList* children = gtk_container_get_children (bbox);
	GtkWidget *btn = children->data;
	gtk_widget_hide (btn);
	g_list_free (children);
	//add 2 buttons that we want
	E2_Button no_btn;
	e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_DEFAULT);

	e2_dialog_add_defined_button (dialog, &E2_BUTTON_?APPLY);
	e2_dialog_add_defined_button (dialog, &no_btn);
	e2_dialog_add_defined_button (dialog, &E2_BUTTON_APPLY);
*/
	g_signal_connect (G_OBJECT (dialog), "response",
		G_CALLBACK (_e2p_track_choose_response_cb), rt);

	e2_dialog_setup (dialog, rt->dialog);
	gtk_widget_show_all (dialog);
	NEEDOPENBGL
}
/*
 //get files with service (Documents, Music, Images, Videos, Text, Development, Other)
tracker-files -s service
 //get files with mime type(s)
tracker-files -m Mime1 [Mime2...]

 //search files for certain terms (ANDed)
tracker-search [OPTION...] search terms ... -
  -l, --limit=limit         limit the number of results showed
  -s, --service=service     search from a specific service

 //perform an rdf query and return results with specified metadata fields
tracker-query [OPTION...] RDFQueryFile [MetaDataField...] ...
  -s, --service=service             search from a specific service
  -t, --search-term=search-term     adds a fulltext search filter
  -k, --keyword=keyword             adds a keyword filter
*/
/**
@brief find and list matching items in output pane
This is called from a callback, BGL closed
@param rt pointer to data for dialog

@return
*/
static void _e2p_track_list_results (E2_TrackDlgData *rt)
{
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->query_combo));
#else
		GTK_BIN (rt->query_combo)->child;
#endif
	const gchar *find = gtk_entry_get_text (GTK_ENTRY (entry));
	gchar *command;
	gint result;

	if (
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->type_radio))
#else
		(GTK_TOGGLE_BUTTON (rt->type_radio))->active
#endif
	)	//search for service
	{
		gint type = gtk_combo_box_get_active (GTK_COMBO_BOX (rt->service_combo));
		if (type == -1)
			return;
		if (*find == '\0'
			|| !strcmp (find, "*")
			|| !strcmp (find, _("all")))
			command = g_strdup_printf ("tracker-files -s %s", cmd_str[service_index]);
		else
			command = g_strdup_printf ("tracker-search -s %s %s",
				cmd_str[service_index], find);
	}
	else if (
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->type_radio2))
#else
		(GTK_TOGGLE_BUTTON (rt->type_radio2))->active
#endif
	)	//search for mimetype
	{
		if (*find == '\0')
		{
			e2_output_print_end (&app.tab, FALSE);
			return;
		}
		command = g_strdup_printf ("tracker-files -m %s", find);
	}
	else
	{
		if (*find == '\0')
		{
			e2_output_print_end (&app.tab, FALSE);
			return;
		}
		command = g_strdup_printf ("tracker-query %s", find);
	}

	result = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, rt->dialog
#ifdef E2_COMMANDQ
	, TRUE
#endif
	);
	if (result == 0)
		e2_output_print_end (&app.tab, FALSE);

	g_free (command);
}
/**
@brief callback for dialog's "response" signal

@param dialog the dialog where the response was triggered
@param response the response for the clicked button
@param rt pointer to data for dialog

@return
*/
static void _e2p_track_response_cb (GtkDialog *dialog, gint response,
	E2_TrackDlgData *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_USER1:	//show help on using tracker
			e2_utils_show_help ("tracker plugin"); //no translation unless help doc is translated
			gtk_widget_grab_focus (
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->query_combo))
#else
				GTK_BIN (rt->query_combo)->child
#endif
			);
			break;
		case E2_RESPONSE_USER2:	//clear query entry
		{
			GtkWidget *entry =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->query_combo));
#else
				GTK_BIN (rt->query_combo)->child;
#endif
			gtk_entry_set_text (GTK_ENTRY (entry), "");
			gtk_widget_grab_focus (entry);
		}
			break;
		case E2_RESPONSE_FIND:
			//in case the user aborts during the search, update history stuff now
			service_index = gtk_combo_box_get_active (GTK_COMBO_BOX (rt->service_combo));
			GtkWidget *entry =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->query_combo));
#else
				GTK_BIN (rt->query_combo)->child;
#endif
			const gchar *find = gtk_entry_get_text (GTK_ENTRY (entry));
			if (*find != '\0')
				e2_list_update_history (&query_history, find, NULL, 30, FALSE);
			_e2p_track_list_results (rt);
			break;
		default:
			gtk_widget_destroy (rt->dialog);
			DEALLOCATE (E2_TrackDlgData, rt);
			break;
	}
	NEEDOPENBGL
}
/**
@brief handle activation (<Return> keypresses) in the query entry

@param entry UNUSED the entry widget for the combo box
@param rt pointer to dialog data struct
@return
*/
static void _e2p_track_query_activated_cb (GtkEntry *entry, E2_TrackDlgData *rt)
{
//	NEEDOPENBGL
	_e2p_track_response_cb (GTK_DIALOG (rt->dialog), E2_RESPONSE_FIND, rt);
//	NEEDCLOSEBGL
}
/**
@brief create and run dialog

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_track (gpointer from, E2_ActionRuntime *art)
{
	E2_TrackDlgData *rt = ALLOCATE (E2_TrackDlgData);
	CHECKALLOCATEDWARN (rt, return FALSE; );

	rt->dialog = e2_dialog_create (NULL, NULL, _("tracker query"),
		(ResponseFunc)_e2p_track_response_cb, rt);

	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog));
#else
		(GTK_DIALOG (rt->dialog))->vbox;
#endif
	GtkWidget *hbox = e2_widget_add_box (dialog_vbox, TRUE, E2_PADDING,
		FALSE, FALSE, E2_PADDING_SMALL);
//	rt->type_radio = e2_button_add_radio (hbox, NULL, NULL, FALSE, 0, NULL, NULL);
//	e2_widget_add_label (hbox, _("Search for"), 0.0, 0.5, FALSE, 0);
	rt->type_radio = e2_button_add_radio (hbox, _("_Search for"), NULL,
		TRUE, FALSE, 0, NULL, NULL);

	rt->service_combo = e2_combobox_add (hbox, FALSE, E2_PADDING,
		NULL, NULL, NULL, E2_COMBOBOX_MENU_STYLE);
	guint i;
	for (i = 0; i < ACTIVE_TRACKER_SERVICES; i++)
#ifdef USE_GTK2_24
		gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (rt->service_combo),
			gettext (object_names[i]));
#else
		gtk_combo_box_append_text (GTK_COMBO_BOX (rt->service_combo),
			gettext (object_names[i]));
#endif
	gtk_combo_box_set_active (GTK_COMBO_BOX (rt->service_combo), service_index);
//USELESS? 	e2_widget_set_safetip (rt->service_combo, _("Select information service"));
	e2_widget_add_label (hbox, _("which match:"), 0.0, 0.5, FALSE, 0);

	hbox = e2_widget_add_box (dialog_vbox, TRUE, E2_PADDING, FALSE, FALSE,
		E2_PADDING_SMALL);
	GSList *group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt->type_radio));
//	e2_button_add_radio (hbox, NULL, group, FALSE, 0, NULL, NULL);
//	e2_widget_add_label (hbox, _("Search for items whose _mimetype is any of:"), 0.0, 0.5, FALSE, 0);
	rt->type_radio2 = e2_button_add_radio (hbox,
		_("Search for items whose _mimetype is any of:"), group,
		FALSE, FALSE, 0, NULL, NULL);

	hbox = e2_widget_add_box (dialog_vbox, TRUE, E2_PADDING, FALSE, FALSE,
		E2_PADDING_SMALL);
//	group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt->type_radio));
	e2_button_add_radio (hbox, _("Search for items using this rdf query:"), group,
		FALSE, FALSE, 0, NULL, NULL);
	e2_button_add (hbox, FALSE, E2_PADDING_LARGE, _("_Select"), STOCK_NAME_OPEN,
			_("Open query-selection dialog"), _e2p_track_choose_rdf_cb, rt);

	rt->query_combo = e2_combobox_add (dialog_vbox, FALSE, E2_PADDING,
		(ActivateFunc)_e2p_track_query_activated_cb, rt, &query_history,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_FOCUS_ON_CHANGE | E2_COMBOBOX_CYCLE_HISTORY);

	GtkWidget *btn = e2_dialog_add_simple_button (rt->dialog, STOCK_NAME_HELP,
		_("_Help"), E2_RESPONSE_USER1);
	e2_widget_set_safetip (btn, _("Get help on constructing queries"));

	btn = e2_dialog_add_simple_button (rt->dialog, STOCK_NAME_CLEAR,
		_("C_lear"), E2_RESPONSE_USER2);
	e2_widget_set_safetip (btn, _("Clear the current query"));

	btn = e2_dialog_add_simple_button (rt->dialog, STOCK_NAME_FIND,
		_("_Find"), E2_RESPONSE_FIND);
	e2_widget_set_safetip (btn,
		_("Initiate a query using currently-specified criteria"));

	e2_dialog_set_negative_response (rt->dialog, GTK_RESPONSE_CLOSE);
	e2_dialog_show (rt->dialog, app.main_window, 0, &E2_BUTTON_CLOSE, NULL);
	gtk_widget_grab_focus (
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->query_combo))
#else
		GTK_BIN (rt->query_combo)->child
#endif
	);

	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONE_START(_A(1),_("track"),_e2p_track,
		_("_Track.."),
		_("Find items in the tracker database"),
		"plugin_"ANAME E2ICONTB)

	e2_cache_int_register ("track-plugin-type", &service_index, 0);
	e2_cache_list_register ("track-plugin-history", &query_history);

	PLUGINIT_ONE_END
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	if (ret)
	{
		//backup the cache data
		e2_cache_unregister ("track-plugin-type");
		e2_cache_unregister ("track-plugin-history");
	}
	return ret;
}
