/* $Id: e2_command_line.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation - either version 3, or (at your option) any
later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/utils/e2_command_line.c
@brief command line functions

This file contains functions for e2's command- and dir-lines.
*/

#include <string.h>
#include <pthread.h>
#include "e2_command_line.h"
#include "e2_complete.h"
#include "e2_filelist.h"
#include "e2_task.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif

//list of names of command and dir lines, used to ensure that each
//command- or dir-line has a unique name
static GList *line_names = NULL; //CHECKME cleaned only via _e2_command_line_destroy()?
extern GList *children;
extern const gchar *shellcmd;
extern pthread_mutex_t task_mutex;

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief sync command line tree model history with cachable history glist
This is called in the process of destroying a command-line (e.g. when rebuilding
toolbars) and when session is ending.
Expects the combo model to be still available
@param rt  pointer to command line runtime data struct

@return
*/
static void _e2_command_line_sync_history (E2_CommandLineRuntime *rt)
{
#ifdef DEBUG_MESSAGES
	//the model doesn't exist when the widget is being destroyed
	if (GTK_IS_TREE_MODEL(rt->model))
	{
#endif
		//free old history
		if (rt->history != NULL)
			e2_list_free_with_data (&rt->history);
		GtkTreeIter iter;
//		printd (DEBUG, "update history list for %s", rt->name);
		//fill with new state
		if (gtk_tree_model_get_iter_first (rt->model, &iter))
		{
			gint count = 0;
			//max == 0 means no limit
			gint max = (rt->original) ?
				e2_option_int_get ("command-line-history-max"):
				e2_option_int_get ("dir-line-history-max");
			gchar *value;
			do
			{
				gtk_tree_model_get (rt->model, &iter, 0, &value, -1);
				rt->history = g_list_append (rt->history, value);
				if (++count == max)
					break;
			} while (gtk_tree_model_iter_next (rt->model, &iter));
		}
#ifdef DEBUG_MESSAGES
	}
	else
		printd (WARN, "no history update for %s, model is destroyed", rt->name);
#endif
}
/* *
@brief clear and backup runtime data for a command/dir line
This is the 'sync data' function specified when command lines are created.
It must be called before the combo widget is destroyed, as the combo's model is
needed downstream
@param rt runtime data for the commandline being processed

@return
*/
/*ONLY 1 USE, INLINED THERE
static void _e2_command_line_destroy (E2_CommandLineRuntime *rt)
{
	line_names = g_list_remove (line_names, rt->name); //each name is unique
	e2_cache_unregister (rt->name);	//also zaps rt->history
	g_free (rt->name);
	app.command_lines = g_list_remove (app.command_lines, rt);
	DEALLOCATE (E2_CommandLineRuntime, rt);
} */
/**
@brief check if strings @a arg1 and @a arg2 match

@param arg1 string data for member of glist being scanned
@param arg2 string to be found in the glist

@return 0 if strings @a arg1 and @a arg2 are the same
*/
static gint _e2_command_line_match_list_str (const gchar *arg1, const gchar *arg2)
{
	return (strcmp (arg1, arg2));
}
/**
@brief get a unique commandline name beginning with @a private
@param private intended private name of commandline

@return newly-allocated string same as @a private, or @a private with appended digit
*/
static gchar *_e2_command_line_get_name (const gchar *private)
{
	GList *tmp = g_list_find_custom (line_names, private,
		(GCompareFunc)_e2_command_line_match_list_str);
	if (tmp == NULL)
		return (g_strdup (private));
	else
	{
		gint turn = 2;
		gchar *name = NULL;	//assigned to enable initial free()
		while (tmp != NULL)
		{
			g_free (name);
			name = g_strdup_printf ("%s %d", private, turn++);
			tmp = g_list_find_custom (line_names, name,
				(GCompareFunc)_e2_command_line_match_list_str);
		}
		return name;
	}
}
/**
@brief set command-line text to last-used history value, if required, or to ""

@param rt pointer to command line runtime data struct

@return
*/
static void _e2_command_line_update_entry (E2_CommandLineRuntime *rt)
{
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	if (e2_option_bool_get_direct (rt->opt_history_last))
	{
		GtkTreeIter iter;
		if (gtk_tree_model_get_iter_first (rt->model, &iter))
		{
			e2_combobox_set_active (rt->combo, 0);
			gtk_editable_set_position (GTK_EDITABLE (entry), -1);
		}
	}
	else
		gtk_entry_set_text (GTK_ENTRY (entry), "");
}
/**
@brief get pointer to rt data for first-recorded command line (as opposed to dir line)

This does nothing if @a rt does not point to NULL

@param rt pointer to location to store command line runtime data struct pointer

@return
*/
static void _e2_command_line_get_first (E2_CommandLineRuntime **rt)
{
	if (*rt == NULL)
	{
//		if (app.command_lines == NULL)
//			return;	//no command lines registered
//		else
//		{
			//find the first-registered command-line
			E2_CommandLineRuntime *cl_rt;
			GList *member;
			for (member = app.command_lines; member != NULL ; member = g_list_next (member))
			{
				cl_rt = (E2_CommandLineRuntime *) member->data;
				if (cl_rt->original)
				{
					*rt = cl_rt;
					break;
				}
			}
//		}
	}
}
/**
@brief determine command line pointer(s)

If @a rt != NULL, this function tries to find the corresponding entry widget.
Or if @a rt is NULL, tries to find the @a rt corresponding to @a widget.
But then, if @a entry is not actually an entry widget, @a rt is just set
to the one for the first-registered command line (as opposed to dir line) if any

@param widget pointer to widget to check/set (may be NULL)
@param rt pointer to the command line runtime object to check/set, or to NULL

@return  TRUE if @a entry and @a rt are set to valid values
*/
static gboolean _e2_command_line_get (GtkWidget **widget,
	E2_CommandLineRuntime **rt)
{
	//the runtime object has precedence
	if (*rt == NULL)
	{
		//if the entry really is an entry, get the pointer to the
		//runtime object
		if (GTK_IS_ENTRY (*widget))
		{
			*rt = g_object_get_data (G_OBJECT (*widget), "command-line-runtime");
			if (*rt != NULL)
				return TRUE;
			else
			{
				E2_CommandLineRuntime *clrt;
				GList *line;
				for (line = app.command_lines; line != NULL ; line = g_list_next (line))
				{
					clrt = line->data;
					if (*widget ==
#ifdef USE_GTK2_14
						gtk_bin_get_child (GTK_BIN (clrt->combo))
#else
						GTK_BIN (clrt->combo)->child
#endif
					)
						break;
				}
				if (line != NULL)
				{
					*rt = clrt;
					return TRUE;
				}
			}
		}
		else
		//can't find anything from widget, just try to find the first available command line
			_e2_command_line_get_first (rt);
		if (*rt == NULL)	//OOPS can't find anything !!
			return FALSE;
	}
	//set the real entry widget
	*widget =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN ((*rt)->combo));
#else
		GTK_BIN ((*rt)->combo)->child;
#endif
	return TRUE;
}
/**
@brief change the appearance of @a entry contents

This does nothing if option "dir-line-history-auto" is TRUE. Otherwise, has no
regard for the contents of @a entry.

@param entry the widget to alter
@param on TRUE to highlight, FALSE to normalise
@return
*/
void e2_command_line_highlight (GtkWidget *entry, gboolean on)
{
	if (e2_option_bool_get ("dir-line-history-auto"))
		return;
	if (on)
	{
#ifdef USE_GTK3_0
		GdkRGBA color;
		e2_option_color_get_RGBA ("color-positive", &color);
		gtk_widget_override_color (entry, GTK_STATE_NORMAL, &color);
#else
		gtk_widget_modify_text (entry, GTK_STATE_NORMAL,
			e2_option_color_get ("color-positive"));
#endif
	}
	else
	{
#ifdef USE_GTK3_0
		gtk_widget_override_color (entry, GTK_STATE_NORMAL, NULL);
#else
		gtk_widget_modify_text (entry, GTK_STATE_NORMAL, NULL);
#endif
	}
}
/**
@brief change appearance of @a entry according to whether @a newtext is in
 parent combo's history, and if option "dir-line-history-auto" is TRUE.

Also sets the active iter for the combo, if relevant.

@param entry the widget used for finding combo model
@param newtext the string to check
@return
*/
void e2_command_line_update_highlight (GtkWidget *entry, const gchar *newtext)
{
	GtkComboBox *combo = GTK_COMBO_BOX (
#ifdef USE_GTK2_14
		gtk_widget_get_parent (entry)
#else
		entry->parent
#endif
	);
	GtkTreeModel *model = gtk_combo_box_get_model (combo);
	GtkTreeIter iter;
	gboolean match = (gtk_tree_model_get_iter_first (model, &iter)
		  && e2_tree_find_iter_from_str_simple (model, 0, newtext, &iter, FALSE));
	e2_command_line_highlight (entry, match);
	if (match)
	{
//		printd (DEBUG, "Set dirline active iter");
		gtk_combo_box_set_active_iter (combo, &iter);
	}
}
/**
@brief change-dir helper function
This is initiated from within _e2_fileview_change_dir(), with BGL closed/on
@param path cleaned path of the dir that is now current, UTF-8 string
@param rt pointer to command line runtime data struct

@return
*/
static void _e2_command_line_change_dir (const gchar *path,
	E2_CommandLineRuntime *rt)
{
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	if (e2_option_bool_get_direct (rt->opt_history_last))
	{
		gtk_entry_set_text (GTK_ENTRY (entry), path);
		e2_command_line_update_highlight (entry, path);
		if (e2_option_bool_get ("dir-line-pathname-hint"))
			e2_widget_set_safetip (entry, path);
	}
	else
	{
		gtk_entry_set_text (GTK_ENTRY (entry), "");
		e2_command_line_highlight (entry, FALSE);
	}

	if (e2_option_bool_get ("dir-line-history-auto"))
		e2_combobox_prepend_history (rt->combo, path, -2,
			e2_option_bool_get ("dir-line-history-double"));
}

  /*********************/
 /***** callbacks *****/
/*********************/

#ifdef E2_MOUSECUSTOM
/**
@brief mouse button-event (press and release) callback
This is to block default handling of modified events, so that gestures etc are
not interfered with
@param entry the entry widget where the button was pressed
@param event pointer to gtk event data struct
@param user_data UNUSED pointer specified when cb was connected

@return TRUE if modifier key(s) were pressed
*/
static gboolean _e2_command_line_button_filter_cb (GtkWidget *entry,
	GdkEventButton *event, gpointer user_data)
{
	printd (DEBUG, "command line button filter cb");
	return ((event->state & E2_MODIFIER_MASK) != 0);
}
#else
/**
@brief mouse button-press callback
Double-left-clicks cause the focused entry widget to be activated,
or a directory selection dialog when <Ctrl> key and the middle-button
are pressed while the pointer is on a directory-line
@param entry the entry widget where the button was pressed
@param event pointer to gtk event data struct
@param rt pointer to commandline runtime data struct

@return TRUE if left-double or ctrl-middle button event
*/
static gboolean _e2_command_line_button_press_cb (GtkWidget *entry,
	GdkEventButton *event, E2_CommandLineRuntime *rt)
{
	printd (DEBUG, "command line button press cb");
	if (event->button == 1 && event->type == GDK_2BUTTON_PRESS)
	{
		if ((event->state & E2_MODIFIER_MASK) == 0)
		{
//			NEEDOPENBGL
			g_signal_emit_by_name (G_OBJECT (entry), "activate");
//			NEEDCLOSEBGL
			return TRUE;
		}
	}
	else if (event->button == 2 && event->type == GDK_BUTTON_PRESS && !rt->original)
	{
		if (event->state & GDK_CONTROL_MASK)
		{
			NEEDCLOSEBGL
			gboolean ret = e2_pane_choose_new_dir (rt->pane, entry);
			NEEDOPENBGL
			return ret;
		}
	}
	return FALSE;
}
#endif //def E2_MOUSECUSTOM
/**
@brief autocomplete dir-line entries when dir is mounted-local
This is a(nother) callback for dir-line key-press-event signals
@param entry pointer to the widget that received the keypress
@param event event data struct
@param rt pointer to data struct for the dir-line

@return TRUE if the key was non-modified and a textkey, and triggered completion
*/
static gboolean _e2_command_line_key_press_cb (GtkWidget *entry,
	GdkEventKey *event, E2_CommandLineRuntime *rt)
{
	gboolean res = FALSE;
	if ((event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK
			| GDK_MOD3_MASK | GDK_MOD4_MASK | GDK_MOD5_MASK	//CHECKME
#ifdef USE_GTK2_10
			| GDK_SUPER_MASK | GDK_HYPER_MASK | GDK_META_MASK
#endif
		)) == 0
		&& (event->keyval < 0xF000 || event->keyval > 0xFFFF))
	{
		guint pane = (rt->pane == &app.pane1) ? E2PANE1 : E2PANE2;
		NEEDCLOSEBGL
		res = e2_fs_complete_dir (entry, event->keyval, pane);
		NEEDOPENBGL
	}
	return res;
}
/**
@brief Another "activate" signal callback for [each] command line entry widget
It runs the command recorded in the dir line.
Note: e2_combobox_activated_cb() also applies, it updates history
@param entry the activated entry widget containing command to run
@param clrt callback user_data, as supplied to e2_combobox_get()

@return
*/
static void _e2_command_line_activate_cb (GtkEntry *entry,
	E2_CommandLineRuntime *clrt)
{
	printd (DEBUG, "_e2_command_line_activate_cb (entry:,user_data:)");
	NEEDCLOSEBGL
	const gchar *command_raw = gtk_entry_get_text (entry);
	if (command_raw != NULL && *command_raw != '\0')
	{
		e2_command_run ((gchar *)command_raw, E2_COMMAND_RANGE_DEFAULT, entry
#ifdef E2_COMMANDQ
		, FALSE
#endif
		);
		_e2_command_line_update_entry (clrt);
	}
	NEEDOPENBGL
}
/**
@brief The "activate" signal callback for each dirline entry widget
Opens the directory recorded in the dir line and if relevant, updates history,
using a cleaned path if possible
@param entry activated entry widget containing path to open
@param clrt callback user_data, as supplied to e2_combobox_get()

@return
*/
static void _e2_command_line_diractivate_cb (GtkEntry *entry,
	E2_CommandLineRuntime *clrt)
{
	printd (DEBUG, "_e2_command_line_diractivate_cb (entry:,clrt:)");
	NEEDCLOSEBGL
	const gchar *path_raw = gtk_entry_get_text (entry);
	if (path_raw != NULL && *path_raw != '\0')
	{
		gchar *path = e2_utf8_unescape (path_raw, ' ');
		path = e2_utils_path_clean (path);
		E2_PaneRuntime *prt = clrt->pane;
		if (prt->path == NULL || strcmp (prt->path, path) != 0)
		{
			//first-time for this place
//E2_VFSTMPOK no need to handle any URI here
			printd (DEBUG, "goto e2_pane_change_dir(%s)", path);
			e2_pane_change_dir (prt, path); //async change
		}
		else
		{
			//ensure shown version is clean
			gtk_entry_set_text (entry, path);
			//mark dirline text if appropriate
			e2_command_line_highlight (GTK_WIDGET (entry), TRUE);
			//update dirline history
			GtkWidget *combo =
#ifdef USE_GTK2_14
				gtk_widget_get_parent ((GtkWidget*)entry);
#else
				((GtkWidget*)entry)->parent;
#endif
			e2_combobox_prepend_history (combo, path, -2,
				e2_option_bool_get ("dir-line-history-double"));
		}
		g_free (path);
		if (e2_option_bool_get ("dir-line-focus-after-activate"))
		{
			if (prt == other_pane)
				e2_pane_activate_other ();
			gtk_widget_grab_focus (curr_view->treeview);
		}
	}
	NEEDOPENBGL
}

/**
@brief backup and cleanup during the destruction of a command line combobox widget
@param object UNUSED the object being processed
@param rt pointer to rt data struct for the command line
@return
*/
static void _e2_command_line_destroy_cb (
#ifdef USE_GTK3_0
	GtkWidget *object,
#else
	GtkObject *object,
#endif
	E2_CommandLineRuntime *rt)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
//	_e2_command_line_destroy (rt);
	line_names = g_list_remove (line_names, rt->name); //each name is unique
	e2_cache_unregister (rt->name);	//also zaps rt->history
	g_free (rt->name);
	app.command_lines = g_list_remove (app.command_lines, rt);
	DEALLOCATE (E2_CommandLineRuntime, rt);
}

  /*******************/
 /***** actions *****/
/*******************/

/**
@brief toggle focus between a dir line and the corresponding file pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the focus was changed
*/
static gboolean _e2_command_line_focus_dirline_action (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *pane = curr_pane;	//default
	//string with "1" (or anything else) which specifies the dir line to focus
	gchar *arg = (gchar *) art->data;
	if (arg != NULL)
	{
		g_strstrip (arg);
		if (*arg != '\0')
			pane = (!strcmp (arg, "2")) ? &app.pane2 : &app.pane1;	//no arg translation
	}
	//find the first-registered dir-line for the pane
	GList *list;
	for (list = app.command_lines; list != NULL ; list = g_list_next (list))
	{
		E2_CommandLineRuntime *clrt = (E2_CommandLineRuntime *) list->data;
		if (!clrt->original && clrt->pane == pane)
		{
			GtkWidget *child =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
			GTK_BIN (clrt->combo)->child;
#endif
#ifdef USE_GTK2_18
			if (gtk_widget_has_focus (child))
#else
			if (GTK_WIDGET_HAS_FOCUS (child))
#endif
				gtk_widget_grab_focus (curr_view->treeview);
			else
			{
				gtk_editable_set_position (GTK_EDITABLE (child), -1);
				gtk_widget_grab_focus (child);
			}
			return TRUE;
		}
	}
	return FALSE;	//no dirline found for the pane
}
#ifdef E2_MOUSECUSTOM
/**
@brief 'activate' a command line or dirline via a pointer-device operation

@param from the entry widget which was the focus of the pointer operation
@param art UNUSED action runtime data

@return TRUE if @a from is the right type ofd widget
*/
static gboolean _e2_command_line_activate_action (gpointer from, E2_ActionRuntime *art)
{
	if (GTK_IS_ENTRY (from))
	{
		GtkWidget *parent =
#ifdef USE_GTK2_14
			gtk_widget_get_parent (GTK_WIDGET(from));
#else
			GTK_WIDGET(from)->parent;
#endif
#ifdef USE_GTK3_0
		if (GTK_IS_COMBO_BOX (parent))
#else
		if (GTK_IS_COMBO_BOX_ENTRY (parent))
#endif
		{
			printd (DEBUG, "issue activate signal for combobox entry");
			NEEDOPENBGL
			g_signal_emit_by_name (G_OBJECT (from), "activate");
			NEEDCLOSEBGL
			return TRUE;
		}
	}
	return FALSE;
}
#endif
/**
@brief focus the first-logged command line

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the line is found
*/
static gboolean _e2_command_line_focus_action (gpointer from, E2_ActionRuntime *art)
{
	E2_CommandLineRuntime *rt = NULL;
	_e2_command_line_get_first (&rt);	//get the first line
	if (rt == NULL)
		return FALSE;
	GtkWidget *child =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	gtk_editable_set_position (GTK_EDITABLE (child), -1);
	gtk_widget_grab_focus (child);
	return TRUE;
}
/**
@brief toggle focus between first-logged command line and active pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the line is found
*/
static gboolean _e2_command_line_focus_toggle_action
	(gpointer from, E2_ActionRuntime *art)
{
	E2_CommandLineRuntime *rt = NULL;
	_e2_command_line_get_first (&rt); //gets the first line
	if (rt == NULL)
		return FALSE;
	GtkWidget *child =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
#ifdef USE_GTK2_18
	if (gtk_widget_has_focus (child))
#else
	if (GTK_WIDGET_HAS_FOCUS (child))
#endif
		gtk_widget_grab_focus (curr_view->treeview);
	else
	{
		gtk_editable_set_position (GTK_EDITABLE (child), -1);
		gtk_widget_grab_focus (child);
	}
	return TRUE;
}
/**
@brief clear contents of an entry, generally @a from

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_command_line_clear_action (gpointer from, E2_ActionRuntime *art)
{
	//from may be entry, button, menuitem etc
	GtkWidget *entry = (from == NULL) ? NULL : GTK_WIDGET (from);
	if (!GTK_IS_ENTRY (from))
	{
		//try to find the corresponding command line
		E2_CommandLineRuntime *rt = NULL;
		_e2_command_line_get (&entry, &rt);
		if (entry == NULL || entry == GTK_WIDGET (from))
		{
#if 1
			printd (WARN, "no suport yet for clear action initiated from random non-entry");
#else
			//FIXME get the entry from somewhere
			GList *line;
			entry = NULL;
			for (line = app.command_lines; line != NULL ; line = line->next)
			{
				rt = (E2_CommandLineRuntime *)line->data;
				if (rt->combo <shares some ancestor with 'from'>) ?? from = NULL ?
				{
					entry =
#ifdef USE_GTK2_14
						gtk_bin_get_child (GTK_BIN (rt->combo));
#else
						GTK_BIN (rt->combo)->child;
#endif
					break;
				}
			}
			if (entry == NULL)
#endif
				return FALSE;
		}
	}
	gtk_entry_set_text (GTK_ENTRY (entry), "");
	if (g_object_get_data (G_OBJECT (entry), "e2-dir-line") != NULL)
		e2_command_line_highlight (entry, FALSE);
	return TRUE;
}
/**
@brief clear whole command-line history

@param from the entry which was activated
@param art action runtime data

@return TRUE if there was a history
*/
static gboolean _e2_command_line_clearall_history_action
	(gpointer from, E2_ActionRuntime *art)
{
	GtkWidget *entry = (from == NULL) ? NULL : GTK_WIDGET (from);
	//find the command line for the entry
	E2_CommandLineRuntime *rt = NULL;
	_e2_command_line_get (&entry, &rt);

	if (rt->history != NULL)
	{
/*
#ifdef USE_GTK2_24
		gtk_combo_box_text_remove_all (GTK_COMBO_BOX_TEXT (rt->combo));
#else
		gint num = g_list_length (rt->history);
		for (; num > 0; num--)
			gtk_combo_box_remove_text (GTK_COMBO_BOX (rt->combo), 0);
#endif
*/
		gtk_list_store_clear (GTK_LIST_STORE (rt->model));
		e2_list_free_with_data (&rt->history);
		return TRUE;
	}
	return FALSE;
}

/**
@brief clear one or more items, each matching current commandline text, from command-line history

@param from the entry which was activated
@param art action runtime data

@return TRUE if there was a history with item(s) matching the current text
*/
static gboolean _e2_command_line_clear1_history_action
	(gpointer from, E2_ActionRuntime *art)
{
	gboolean retval = FALSE;
	GtkWidget *entry = (from == NULL) ? NULL : GTK_WIDGET (from);
	//find the command line for the entry
	E2_CommandLineRuntime *rt = NULL;
	_e2_command_line_get (&entry, &rt);

	const gchar *cmd = gtk_entry_get_text (
#ifdef USE_GTK2_14
		GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->combo)))
#else
		GTK_ENTRY (GTK_BIN (rt->combo)->child)
#endif
		);

	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (rt->model, &iter))
	{
		do
		{
			gboolean del = FALSE;
			gchar *hist;
			gtk_tree_model_get (rt->model, &iter, 0, &hist, -1);
			if (hist)
			{
				if (strcmp (hist, cmd) == 0)
				{
					retval = TRUE;
					del = gtk_list_store_remove (GTK_LIST_STORE (rt->model), &iter);
				}
				g_free (hist);
			}
			if (del)
				continue;
		} while (gtk_tree_model_iter_next (rt->model, &iter));
	}
	return retval;
}

/**
@brief insert selection in @a from into @a entry
If @a from is a filelist treeview, the name of each selected item is inserted
@param from the widget where the action was initiated
@param art action runtime data

@return TRUE if ...
*/
static gboolean _e2_command_line_insert_action
	(gpointer from, E2_ActionRuntime *art)
{
	gboolean retval;
	gint start, end;

	E2_CommandLineRuntime *clrt = NULL;
	_e2_command_line_get_first (&clrt); //gets the first line
	if (clrt == NULL)
		return FALSE;

	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
		GTK_BIN (clrt->combo)->child;
#endif
	GString *cmd = g_string_sized_new (128);

	if (GTK_IS_TEXT_VIEW (from)) //output
	{
		GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (from));
		GtkTextIter start, end;
		retval = gtk_text_buffer_get_selection_bounds (buffer, &start, &end);
		if (retval)
		{
			gchar *text = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
			cmd = g_string_assign (cmd, text);
			g_free (text);
		}
	}
	else if (GTK_IS_ENTRY (from) && GTK_WIDGET (from) != entry)
	{
		retval = gtk_editable_get_selection_bounds (GTK_EDITABLE (from), &start, &end);
		if (retval)
		{
			gchar *text = gtk_editable_get_chars (GTK_EDITABLE (from), start, end);
			cmd = g_string_assign (cmd, text);
			g_free (text);
		}
	}
	else //filelists, buttons, menus
	{
		const gchar *arg = NULL;
		E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, &arg);
		if (rt != NULL)
		{
			gboolean escape, quote;
			if (arg != NULL)
			{
				//find out from the action argument if the user wants quotes/escaping
//				gchar *tmp = g_utf8_strdown (arg, -1);
//OR			gchar *tmp = g_strdup (arg);
//FOR == 		check g_strstrip (tmp);
				quote = (strstr (arg, _A(120)) != NULL);	//_("quote"
				escape = (quote) ? FALSE : (strstr (arg, _A(115)) != NULL);	//_("escape"
//				g_free (tmp);
			}
			else
			{
				escape = quote = FALSE;
			}

#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "disable refresh, insert action");
#endif
			e2_filelist_disable_one_refresh ((rt==curr_pane)?PANEACTIVE:PANEINACTIVE);

			GList *base;
			base = e2_fileview_get_selected_local (&rt->view);
			if (base != NULL)
			{
				GList *tmp;
				for (tmp = base; tmp != NULL; tmp = tmp->next)
				{
					gchar *utf;
					//not DISPLAY, always dup, to avoid refresh race
					utf = D_FILENAME_FROM_LOCALE (((FileInfo *) tmp->data)->filename);
					cmd = g_string_append_c (cmd, ' ');
//tag E2_BADQUOTES
					if (escape)
					{
						gchar *qp = e2_utils_quote_string (utf);
						cmd = g_string_append (cmd, qp);
						g_free (qp);
					}
					else if (quote)
						g_string_append_printf (cmd, "\"%s\"", utf);
					else
						cmd = g_string_append (cmd, utf);
					g_free (utf);
				}
				g_list_free (base);
				retval = TRUE;
			}
			else
				retval = FALSE;

#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "enable refresh, insert action");
#endif
			e2_filelist_enable_one_refresh ((rt==curr_pane)?PANEACTIVE:PANEINACTIVE);
		}
		else
			retval = FALSE;
	}

	if (retval)
	{
		if (gtk_editable_get_selection_bounds (GTK_EDITABLE (entry), &start, &end))
			gtk_editable_delete_text (GTK_EDITABLE (entry), start, end);
		else
			start = gtk_editable_get_position (GTK_EDITABLE (entry));
		gtk_editable_insert_text (GTK_EDITABLE (entry), cmd->str, cmd->len, &start);
		gtk_editable_set_position (GTK_EDITABLE (entry), start);
	}
	g_string_free (cmd, TRUE);

	return retval;
}
/**
@brief complete the string in the command line associated with @a from
This is the mechanism for tab-completion, via a keybinding to the relevant
widget
@param from a pointer to the entry widget whose cntents are to be completed, or NULL
@param art pointer to action rt data

@return
*/
static gboolean _e2_command_line_complete_action
	(gpointer from, E2_ActionRuntime *art)
{
	//find which command line is active
	GtkWidget *entry = (from == NULL) ? NULL : GTK_WIDGET (from);
	E2_CommandLineRuntime *rt = NULL;
	_e2_command_line_get (&entry, &rt);
	if (rt == NULL)
		return FALSE;	//can't find what to work on

	gint pos = gtk_editable_get_position (GTK_EDITABLE (entry));	//characters, not bytes
	gchar *text = g_strdup (gtk_entry_get_text (GTK_ENTRY (entry)));	//could be ""

	//do macros/variables replacement CHECKME worth it ?
	printd (DEBUG, "_e2_command_line_complete_action, macro/var interpretation");
	gchar *tmp = text;
	gchar *freeme = e2_utils_expand_macros (text, NULL); //note %% changes to %
	if (freeme == NULL || freeme == GINT_TO_POINTER (0x1))
		freeme = tmp;
	text = e2_utils_replace_vars (freeme, FALSE);
	if (freeme != tmp)
		g_free (freeme);
	if (strcmp (tmp, text))
		pos = g_utf8_strlen (text, -1);
	g_free (tmp);

	gchar *arg = (gchar *)art->data;
	printd (DEBUG, "complete_action (rt:_,arg:%s,entry:_", arg);
	//find out what type of completion is wanted and relevant
	E2_CompleteFlags flags;
	if (!rt->original)
		flags = E2_COMPLETE_FLAG_DIRS;	//dir-lines just find dirs
	else
	{
		flags = E2_COMPLETE_FLAG_PATH;	//command-lines default to finding executables
		//but whitespace before cursor position implies we are completing an argument
		gchar *s = e2_utils_pass_whitespace (text);
		if (s != NULL)
		{
			gchar *p = e2_utils_bare_strchr (s, ' ');
			if (p == NULL)
				p = e2_utils_bare_strchr (s, '\t');
			if (p != NULL)
			{
				if (p < g_utf8_offset_to_pointer (text, pos))
				{
					flags &= ~E2_COMPLETE_FLAG_PATH;
					g_strstrip (arg);
					if (strstr (arg, _("all")) != NULL)	//_I("all")
						flags |= E2_COMPLETE_FLAG_ALL;
					else
					{
						if (strstr (arg, _A(114)) != NULL)
							flags |= E2_COMPLETE_FLAG_DIRS;
						if (strstr (arg, _A(117)) != NULL)
							flags |= E2_COMPLETE_FLAG_FILES;
						if (strstr (arg, _("mounts")) != NULL	//_I("mounts")
							//even if arg isn't 'mounts', add mountpoints completion
							//in case of entered [u]mount command
							|| g_strstr_len (s, sizeof(E2_MOUNTCOMMAND) + 3, E2_MOUNTCOMMAND) != NULL)
							flags |= E2_COMPLETE_FLAG_MOUNT;
/*NOT WORKING			if (strstr (arg, _("shell")) != NULL)
							flags |= E2_COMPLETE_FLAG_SHELL;
*/
						if (flags == 0)	//empty or unknown arg
							flags = E2_COMPLETE_FLAG_ALL;
					}
				}
			}
			else if (strstr (arg, _("shell")) != NULL)
				flags = E2_COMPLETE_FLAG_SHELL;	//shell completion ok for commmands too
		}
	}

	GList *found = NULL;
	gint num = e2_complete_str (&text, &pos, &found, flags, 0);
//	printd (DEBUG, "found %d - %s - %d", num, text, pos);

	if (num > 0)
	{
		// if it isn't a dir: append a <space>
		if ((e2_option_bool_get ("command-line-complete-append-space")) &&
			(num == 1) && (g_utf8_get_char (g_utf8_offset_to_pointer (text, pos - 1)) != G_DIR_SEPARATOR))
		{
			gchar *part1 = e2_utf8_ndup (text, pos);
			gchar *part2 = g_utf8_offset_to_pointer (text, pos);
			gchar *text_spaced = g_strconcat (part1, " ", *part2 != '\0' ? part2 : "" , NULL);
			g_free (part1);
			gtk_entry_set_text (GTK_ENTRY (entry), text_spaced);
			gtk_editable_set_position (GTK_EDITABLE (entry), pos +1);
			g_free (text_spaced);
		}
		else
		{
			gtk_entry_set_text (GTK_ENTRY (entry), text);
			gtk_editable_set_position (GTK_EDITABLE (entry), pos);
		}
		if (num > 1)
		{
			GList *tmp;
			for (tmp = found; tmp != NULL; tmp = g_list_next (tmp))
			{
				e2_output_print (&app.tab, (gchar *)tmp->data, NULL, TRUE, NULL);
			}
			e2_output_print_end (&app.tab, FALSE);
		}
	}
	e2_list_free_with_data (&found);
	g_free (text);
	return TRUE;
}
/**
@brief children-menu "activate" signal callback, prepends a pid string to the command line

@param item UNUSED the activated menu item
@param rt pointer to data for the command related to @a item, or NULL for "no children" item

@return
*/
static void _e2_commmand_line_child_menu_cb (GtkMenuItem *item, E2_TaskRuntime *rt)
{
	if (rt == NULL)
		return;	//no children
	pthread_mutex_lock (&task_mutex);
	GList *member = g_list_find (app.taskhistory, rt);
	pthread_mutex_unlock (&task_mutex);
	if (member != NULL)	//command data still exists
	{
		E2_CommandLineRuntime *clrt = NULL;
		_e2_command_line_get_first (&clrt); //in principle, there could be > 1 OK ??
		if (clrt != NULL)
		{
			NEEDCLOSEBGL
			GtkWidget *entry =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
				GTK_BIN (clrt->combo)->child;
#endif
			gint newlen;
			gint oldcursor = gtk_editable_get_position (GTK_EDITABLE (entry));
			if (oldcursor == 0 ||
#ifdef USE_GTK3_0
			(e2_utils_get_savedstate (app.main_window) & GDK_CONTROL_MASK) //command lines only in main window
#else
			(e2_utils_get_modifiers () & GDK_CONTROL_MASK)
#endif
			)
			{
				gint newpos = 0;
				gchar *newtext = g_strconcat (rt->pidstr, ":", NULL);
				newlen = strlen (newtext);
				gtk_editable_insert_text (GTK_EDITABLE (entry), newtext,
					newlen, &newpos);
				g_free (newtext);
			}
			else
			{
				newlen = strlen (rt->pidstr);
				gtk_editable_insert_text (GTK_EDITABLE (entry), rt->pidstr,
					newlen, &oldcursor);
			}
			gtk_editable_set_position (GTK_EDITABLE (entry), oldcursor+newlen);

			if (rt->status != E2_TASK_RUNNING)
			{
				gchar *msg = g_strdup_printf (_("Warning - process %s is not active"),
					rt->pidstr);
				e2_output_print (&app.tab, msg, NULL, TRUE, NULL);
				g_free (msg);
			}
			NEEDOPENBGL
		}
	}
}
/**
@brief create and pop up a menu of child processes
This essentially gets data for prepending to a command line
@param action_data ptr to data assigned when action struct created at session start NULL
@param from the thing that was activated to popup the menu, maybe a toolbar button
@param rt_data data suppplied to the action

@return TRUE if the action succeeded
*/
static gboolean _e2_command_line_insert_child_pid
	(gpointer from, E2_ActionRuntime *art)
{
	if (GTK_IS_BUTTON (from))
	{
		art->state &= ~GDK_MOD2_MASK;
		if (!ACTION_CLICK(art))
			return FALSE;
	}
	GtkWidget *menu = e2_menu_create_child_menu (E2_CHILD_ACTIVE,
		_e2_commmand_line_child_menu_cb);
	//determine the menu's popup position
	if (GTK_IS_BUTTON (from))
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_toolbar_set_menu_position, from, 1, 0);
	else
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_output_set_menu_position, app.tab.text, 0, 0);

	return TRUE;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief insert @a string into command line at current cursor position
@param newtext UTF-8 text to be inserted

@return TRUE if the commandline is found
*/
gboolean e2_command_line_insert (const gchar *newtext)
{
	E2_CommandLineRuntime *clrt = NULL;
	_e2_command_line_get_first (&clrt); //in principle, there could be > 1 OK ??
	if (clrt != NULL)
	{
		GtkWidget *entry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
			GTK_BIN (clrt->combo)->child;
#endif
		gint oldcursor = gtk_editable_get_position (GTK_EDITABLE (entry));
		gint newlen = g_utf8_strlen (newtext, -1);
		gtk_editable_insert_text (GTK_EDITABLE (entry), newtext, newlen, &oldcursor);
		gtk_editable_set_position (GTK_EDITABLE (entry), oldcursor + newlen);
		return TRUE;
	}
	return FALSE;
}
/**
@brief [re]register all key-event callbacks for a commandline entry
@param rt pointer to rt data for the commandline

@return
*/
void e2_command_line_register_keybindings (E2_CommandLineRuntime *rt)
{
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	//other "key-press-event" connection(s) have non-NULL data
	//also, if needed, could use bindings cb func
	guint id = g_signal_lookup ("key-press-event", GTK_TYPE_ENTRY);
	g_signal_handlers_disconnect_matched (G_OBJECT (entry),
		G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_DATA, id, 0, NULL, NULL, NULL);

	gchar *category = (rt->original) ?
		g_strconcat (_C(17),".",_C(23),".",_C(5),NULL):	//_(general.main.command-line"
		g_strconcat (_C(17),".",_C(23),".",_C(12),NULL);//_(general.main.dir-line"

	e2_keybinding_enrol (entry, category, (void(*)(E2_OptionSet*))NULL);
	g_free (category);
}
#ifdef E2_MOUSECUSTOM
/**
@brief [re]register all button-event callbacks for a commandline entry
This should be applied before 'normal' button-event callbacks are connected
@param rt pointer to rt data for the commandline

@return
*/
void e2_command_line_register_pointerbindings (E2_CommandLineRuntime *rt)
{
	GtkWidget *entry =
# ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
# else
		GTK_BIN (rt->combo)->child;
# endif
	gchar *category = (rt->original) ?
		g_strconcat (_C(17),".",_C(23),".",_C(5),NULL):	//_(general.main.command line"
		g_strconcat (_C(17),".",_C(23),".",_C(12),NULL);//_(general.main.dir line"
	//default bindings added centrally for core widgets, so NULL default-setter funcs here
	e2_mousebinding_enrol (entry, category, (void(*)(E2_OptionSet*))NULL);
# ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (entry, category, (void(*)(E2_OptionSet*))NULL);
# endif
	g_free (category);
}
#endif
/**
@brief create a data struct and widget for commandline or dirline

@param commands TRUE for a command line, FALSE for dir line
@param pane_rt NULL for a command line, pane data struct for a dir line

@return pointer to the created runtime data struct
*/
E2_CommandLineRuntime *e2_command_line_create (gboolean commands,
	E2_PaneRuntime *pane_rt)
{
	E2_CommandLineRuntime *rt = ALLOCATE (E2_CommandLineRuntime);
	CHECKALLOCATEDFATAL (rt);

	app.command_lines = g_list_append (app.command_lines, rt);

	//unique private name, for cacheing, matching
	rt->name = _e2_command_line_get_name ((commands) ? "command line" : "dir line");
	//append name to local name register to ensure uniqueness of each name
	line_names = g_list_prepend (line_names, rt->name);

//	rt->func = cb;
//	rt->flags = flags;
	rt->original = commands; //TRUE for commandline, FALSE for dirline
	rt->pane = pane_rt;	//pane rt for dirline, NULL for commandline

	rt->history = NULL;
//	rt->update_history = FALSE;
	E2_Cache *cache = e2_cache_list_register (rt->name, &rt->history);
	//this func can't backup model data during destruction - the model is gone already
	cache->sync_func = _e2_command_line_sync_history;
	cache->sync_data = rt;

	void (*activate_cb)(GtkEntry*, E2_CommandLineRuntime*);
	E2_ComboBoxFlags flags = E2_COMBOBOX_HAS_ENTRY;
	//sort out some options for command line or dir line;
	//the options have been registered in the general init function
	if (commands)
	{	//this is a command line
		if (e2_option_bool_get ("command-line-history-double"))
			flags |= E2_COMBOBOX_ALLOW_DOUBLE;
		if (e2_option_bool_get ("command-line-history-cycle"))
			flags |= E2_COMBOBOX_CYCLE_HISTORY;
		if (e2_option_bool_get ("command-line-menu-style"))
			flags |= E2_COMBOBOX_MENU_STYLE;
		flags |= E2_COMBOBOX_FOCUS_ON_CHANGE;
		activate_cb = _e2_command_line_activate_cb;
		//we need to access this often, thus we save the pointer to the option
		//set and will not have to look it up in the hashtable each time
		rt->opt_history_last = e2_option_get ("command-line-history-last");
	}
	else
	{	//this is a dir line
		if (!e2_option_bool_get ("dir-line-history-auto"))
			flags |= E2_COMBOBOX_NO_AUTO_HISTORY;
		if (e2_option_bool_get ("dir-line-history-double"))
			flags |= E2_COMBOBOX_ALLOW_DOUBLE;
		if (e2_option_bool_get ("dir-line-history-cycle"))
			flags |= E2_COMBOBOX_CYCLE_HISTORY;
		if (e2_option_bool_get ("dir-line-menu-style"))
			flags |= E2_COMBOBOX_MENU_STYLE;
		activate_cb = _e2_command_line_diractivate_cb;
		rt->opt_history_last = e2_option_get ("dir-line-history-last");
	}
	//actually create the comboboxentry and its history
	//(on gtk2, MAY need BGL closed )
	rt->combo = e2_combobox_get ((ActivateFunc)activate_cb, rt, &rt->history, flags);
	GtkWidget *child =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	rt->model = gtk_combo_box_get_model (GTK_COMBO_BOX (rt->combo));	//model is for a liststore

	if (commands)
	{
		//command line
		if (e2_option_bool_get ("command-line-history-last"))
		{
			if (e2_combobox_has_history (GTK_COMBO_BOX (rt->combo)))
			{
				//suspend "activate" signals connected when combo was created
				g_signal_handlers_block_matched (G_OBJECT (child),
					G_SIGNAL_MATCH_FUNC,
					0, 0, NULL, e2_combobox_activated_cb, NULL);
				g_signal_handlers_block_matched (G_OBJECT (child),
					G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA, 0, 0, NULL, activate_cb, rt);

				gtk_combo_box_set_active (GTK_COMBO_BOX (rt->combo), 0);

				g_signal_handlers_unblock_matched (G_OBJECT (child),
					G_SIGNAL_MATCH_FUNC,
					0, 0, NULL, e2_combobox_activated_cb, NULL);
				g_signal_handlers_unblock_matched (G_OBJECT (child),
					G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA, 0, 0, NULL, activate_cb, rt);
			}
		}
	}
	else
	{
		//dirlines manage history updates in a single callback
		//and are initialised later, by a cd or e2_toolbar_rebadge()
		g_signal_handlers_disconnect_matched (G_OBJECT (child),
			G_SIGNAL_MATCH_FUNC,
			0, 0, NULL, e2_combobox_activated_cb, NULL);
	}
	//arrange for backup/cleanup before complete destruction of combo
	g_signal_connect (
#ifdef USE_GTK3_0
	G_OBJECT (rt->combo),
#else
	GTK_OBJECT (rt->combo),
#endif
		"destroy", G_CALLBACK (_e2_command_line_destroy_cb), rt);

	//allow the entry to find its rt data
	g_object_set_data (G_OBJECT (child), "command-line-runtime", rt);
#ifdef E2_MOUSECUSTOM
	//non-NULL data here, if only to prevent disconnection in
	// e2_command_line_register_pointerbindings()
	g_signal_connect (G_OBJECT (child), "button-press-event",
		G_CALLBACK (_e2_command_line_button_filter_cb), rt);
	//see also ...register_all()
	e2_command_line_register_pointerbindings (rt);
# ifdef E2_PTRGESTURES
	g_signal_connect (G_OBJECT (child), "button-release-event",
		G_CALLBACK (_e2_command_line_button_filter_cb), rt);
# endif
#else
	g_signal_connect (G_OBJECT (child), "button-press-event",
		G_CALLBACK (_e2_command_line_button_press_cb), rt);
#endif
	if (commands)
	{	//this is a command line
		g_object_set_data (G_OBJECT (child), "e2-command-line", child); //make it recognisable
		if (e2_option_bool_get ("command-line-show-output-on-focus-in"))
			g_signal_connect (G_OBJECT (child), "focus-in-event",
				G_CALLBACK (e2_window_output_show), NULL);
		if (e2_option_bool_get ("command-line-hide-output-on-focus-out"))
			g_signal_connect (G_OBJECT (child), "focus-out-event",
				G_CALLBACK (e2_window_output_hide), NULL);
	}
	else
	{	//this is a dir line
		g_object_set_data (G_OBJECT (child), "e2-dir-line", child);
//see register_all
//		e2_command_line_register_keybindings (rt);
		g_signal_connect (G_OBJECT (child), "key-press-event",
			G_CALLBACK (_e2_command_line_key_press_cb), rt); //auto-completion
		//register to change_dir hook, if wanted
		//e2_hook_register (&rt->pane->hook_change_dir, func, data);
	}

	return rt;
}
/* *
@brief

@param rt pointer to command line runtime data struct

@return the commandline widget
*/
//static GtkWidget *_e2_command_line_create (E2_CommandLineRuntime *rt)
//{
//	return GTK_WIDGET (rt->combo);
//}
/**
@brief clear runtime data for all command/dir lines, no backup

@return
*/
void e2_command_line_clean_all (void)
{
	GList *member;
	for (member = app.command_lines; member != NULL; member = member->next)
	{
		E2_CommandLineRuntime *rt = (E2_CommandLineRuntime *)member->data;
		g_free (rt->name);
		e2_list_free_with_data (&rt->history); //CHECKME
		DEALLOCATE (E2_CommandLineRuntime, rt);
	}
}
/**
@brief change-dirline contents function
This is initiated with BGL closed/on
@param utfpath utf-8 string, path of the dir that is being opened, with trailer
@param rt pointer to data struct for the pane being changed

@return
*/
void e2_command_line_change_dir (gchar *utfpath, E2_PaneRuntime *rt)
{
	GList *member;
	for (member = app.command_lines; member != NULL; member = member->next)
	{
		E2_CommandLineRuntime *cl = (E2_CommandLineRuntime *)member->data;
		if (cl->pane == rt)
		{
			_e2_command_line_change_dir (utfpath, cl);
			break;
		}
	}
}
/**
@brief initialize command line actions

This function initializes the command line actions.
It is, and should only be, called once at startup.

@return
*/
void e2_command_line_actions_register (void)
{
	E2_Action actions[] =
	{
	{g_strconcat(_A(1),".",_A(27),NULL), (gboolean(*)(gpointer,E2_ActionRuntime*))NULL,
									FALSE,E2_ACTION_TYPE_COMMAND_LINE,
									E2_ACTION_EXCLUDE_MENU | E2_ACTION_EXCLUDE_ACCEL,
		//NULL data signals a command line as distinct from to dirline
									NULL, NULL},
	{g_strconcat(_A(1),".",_A(50),NULL), _e2_command_line_focus_action,        FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(1),".",_A(103),NULL),_e2_command_line_focus_toggle_action, FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
#ifdef E2_MOUSECUSTOM
	{g_strconcat(_A(1),".",_A(30),NULL), _e2_command_line_activate_action,     FALSE,E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_MENU | E2_ACTION_EXCLUDE_TOOLBAR | E2_ACTION_EXCLUDE_ACCEL, NULL, NULL},
#endif
	{g_strconcat(_A(1),".",_A(36),NULL), _e2_command_line_clear_action,        FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(1),".",_A(44),NULL), _e2_command_line_clear1_history_action,FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(1),".",_A(37),NULL), _e2_command_line_clearall_history_action,FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(1),".",_A(38),NULL), _e2_command_line_complete_action,     TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(1),".",_A(59),NULL), _e2_command_line_insert_action,       FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(2),".",_A(28),NULL), _e2_command_line_insert_child_pid,    FALSE,E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_MENU, NULL, NULL},
	{g_strconcat(_A(5),".",_A(36),NULL), _e2_command_line_clear_action,        FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(5),".",_A(103),NULL),_e2_command_line_focus_dirline_action,TRUE, E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_MENU | E2_ACTION_EXCLUDE_TOOLBAR, NULL, NULL},
#ifdef E2_MOUSECUSTOM
	//same as for commandline activation - ok ?
	{g_strconcat(_A(5),".",_A(30),NULL), _e2_command_line_activate_action,     FALSE,E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_MENU | E2_ACTION_EXCLUDE_TOOLBAR | E2_ACTION_EXCLUDE_ACCEL, NULL, NULL},
#endif
	};
	guint i, count = sizeof (actions)/sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&actions[i]);
}
/**
@brief initialize command line options

This function initializes the options for the various command lines.
It is, and should only be, called once at startup.

@return
*/
void e2_command_line_options_register (void)
{
	//none of these require a rebuild after change
	//command line
	gchar *group_name = g_strconcat(_C(6),".",_C(5),":",_C(26),NULL);  //_("commands.command line:misc"
	e2_option_bool_register ("command-line-history-last", group_name, _("show last"),
		_("If activated, the last-entered command will be displayed, instead of an empty line"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);

	group_name = g_strconcat(_C(6),".",_C(5),":",_C(18),NULL);  //_("commands.command line:history"
	e2_option_int_register ("command-line-history-max", group_name, _("maximum number of history entries"),
		_("This is the largest number of command-line history entries that will be recorded. Set to 0 for no limit"),
		NULL, 10, 0, 9999999,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
	e2_option_bool_register ("command-line-history-double", group_name, _("double entries"),
		_("This allows entries to be recorded more than once in the history list"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS);
	e2_option_bool_register ("command-line-history-cycle", group_name, _("cyclic list"),
		_("When scanning the history list, cycle from either end around to the other end, instead of stopping"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS);
	e2_option_bool_register ("command-line-menu-style",
		group_name, _("show as a menu"),
		_("If activated, the history entries will be presented as a menu. "
		"For most Gtk+2 themes, this will not be as attractive as the list view"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS);

	group_name =g_strconcat(_C(6),".",_C(5),":",_C(40),NULL); //_("commands.command line:tab completion"
	e2_option_bool_register ("command-line-complete-append-space",
		group_name, _("append space after unique items"),
		_("This appends a 'space' character to the end of a unique successful match of a file"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);

	//dir line
	group_name = g_strconcat(_C(33),".",_C(12),":",_C(26),NULL); //_("panes.directory line:misc"
	e2_option_bool_register ("dir-line-history-last",
		group_name, _("show last entry"),
		_("If activated, the last-entered directory will be displayed, instead of an empty line"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
	e2_option_bool_register ("dir-line-pathname-hint",
		group_name, _("show pathname as a tooltip"),
		_("If activated, the full directory pathname will display as a tooltip. "
		"This is useful when the path is too long for the normal display"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
	const gchar *opt_completions[] =
		{_("<Tab> only"), _("inserted"), _("selected"), NULL};
	e2_option_sel_register ("dir-line-completion", group_name, _("directory path completion"),
		_("This determines the mode of completion when keying a directory-path"),
		NULL, 1, opt_completions,
		E2_OPTION_FLAG_ADVANCED);

	group_name = g_strconcat(_C(33),".",_C(12),":",_C(18),NULL); //_("panes.directory line:history"
	e2_option_bool_register ("dir-line-history-auto", group_name, _("auto update"),
		_("If activated, all opened directories will be added to pane history, otherwise, manual addition is needed"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS | E2_OPTION_FLAG_FREEGROUP );
	e2_option_int_register ("dir-line-history-max", group_name, _("maximum number of entries"),
		_("This is the largest number of directory-line history entries that will be retained"),
		NULL, 10, 0, 999999,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("dir-line-history-double",
		group_name, _("repeated entries"),
		_("Allows entries to be recorded more than once in the history list"),
		"dir-line-history-auto", FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS);
	e2_option_bool_register ("dir-line-history-cycle",
		group_name, _("cyclic list"),
		_("When scanning a history list, cycle from either end around to the other end, instead of stopping"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS);
	e2_option_bool_register ("dir-line-menu-style",
		group_name, _("show as a menu"),
		_("If activated, the directory line history will be presented as a menu. "
		"For most for most Gtk+2 themes, this will not be as attractive as the list view"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS);
}
