/* $Id: e2_pane.c 3071 2014-02-17 20:27:33Z tpgww $

Copyright (C) 2004-2014 tooar <tooar@emelfm2.net>

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
@file src/e2_pane.c
@brief pane creation and action functions

includes actions on pane contents, but not on panes per se
*/
/**
\page panes the filelist panes

ToDo
\section treeviews
*/
/**
\page histories pane histories
\section logged data

Several data items are logged for each directory displayed in each pane during
a session.

There is a single hash-table (app.dir_history) which holds a E2_DirHistoryEntry
for each dir opened in the current session, in either pane. Each data item caches
the path of the dir, and other information to facilitate stateful interaction with
the dir during the session.

In both panes' runtime data is a list - opendirs. Effectively it's a list of paths
(UTF-8, absolute for current namespace, with trailing "/") of dirs previously
opened. This list is used for respective toolbars' forward/back buttons, and
related menus (right-click and hover), and for the history sub-menu in the main
filelist context menu for the pane.

Actually, this list shares data with app.dir_history, so no data cleanup is needed
when manipulating the list. Unlike app.dir_history, this list may have multiple
entries for the same dir, to represent the order in which dirs were opened. To
increase speed but at some cost to maintainability, new items in this list are
prepended.

app.dir_history and opendirs are manipulated in backend function _e2_fileview_change_dir().

In both panes' runtime data is an index - opendir_cur. This is a 0-based index
of 'current' position in the forward/back history. Since history items are
prepended to the opendirs list, index value 0 corresponds to the _last_ member of
the list, and so on. Moving forward in a history increases the index, but uses the
data at the previous position in the list. The converse applies to moving backward.
List-position index is (list-length - 1 - index). When maximally-forward in the
history, the index is (list-length - 1).

Actions for go-forward and go-backward cause change (if possible) of index, and
corresponding filelist display. Forward increases the index and thus uses data
from earlier in the list.

After forward/back move(s) to some index other than (list-length - 1), any dir
explicitly opened represents a 'branch' of the history. In such case, we assume
that the most recent move(s) were backwards, in which case, to preserve an
intuitive semblance of places to go back in future, any items for indices greater
than current (= list-positions before current) are reversed, the opened dir is added
to become the new last index, and made current (remember, items-order != index!)
As a special case (to avoid adjacent matching dirs, opening the dir for
(current-index + 1) omits addition of an item for the new dir.

Depending on configuration option "cache-history", some of the data in opendirs
lists may be cached between sessions. If "cache-history" is set, the number of
cached items is limited, to keep the history to a reasonably size. Currently the
limit is: current-index +/- 5, or less if there are not +/- 5 in the list. This is
applied immediately before the cache file is written - see e2_pane_trim_history()
usage.

\section menus

A menu can be created from opendirs list data for going forward (right click or
hover on a toolbar go-forward button), or going backward (as for going forward),
or for going anywhere (submenu of filelist context-menu).

A forward-menu shows items (if any) whose index is > opendir_cur i.e. are in the
list at positions before (list-len - 1 - opendir_cur). Any items whose dir is
the same as current is excluded. Any duplicate item is excluded. Most recent
first, these are displayed in index-order i.e. the reverse order to their list
positions.

A backward-menu is similar, it shows items indexed < opendir_cur and positioned
after (list-len - 1 - opendir_cur). Most-recent first, these are shown in the
same order as in the list.

A anywhere-menu shows all items in the list. Any duplicate item is excluded,
with priority to the current item (which is marked as such in the menu).
Backwards first, these are displayed in reverse order to their list positions.
*/

#include "emelfm2.h"
//#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include "e2_pane.h"
#include "e2_action.h"
#include "e2_dialog.h"
#ifdef E2_TREEDIALOG
#include "e2_tree_dialog.h"
#endif
#include "e2_context_menu.h"
#include "e2_filelist.h"

static void _e2_pane_change_dir (E2_PaneRuntime *rt, const gchar *newpath,
	gboolean history, gboolean hook);
static GtkWidget *_e2_pane_popup_forward (GtkWidget *from, E2_PaneRuntime *rt);
static GtkWidget *_e2_pane_popup_backward (GtkWidget *from, E2_PaneRuntime *rt);
static void _e2_pane_history_activated_cb (GtkMenuItem *menuitem, E2_PaneRuntime *rt);

extern guint last_selected_rows;
extern volatile gint p1dirty, p2dirty;

  /*****************/
 /***** utils *****/
/*****************/

#ifdef E2_VFS
/**
@brief setup the pane to which @a rt applies to show data for a specified namespace
Any error messages downstream expect BGL closed here
@a spacedescriptor can have various forms, from full-URI to "-1" - see plugin for details
@param rt pointer to pane data struct, NULL for active pane
@param spacedescriptor UTF-8 string to be interpreted into existing or new namespace

@return TRUE on successful completion or no change needed
*/
gboolean e2_pane_change_space_byuri (E2_PaneRuntime *rt, const gchar *spacedescriptor)
{
	if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.set_namedspace))
	{
		ViewInfo *view = (rt == NULL) ? curr_view : (ViewInfo *)rt;
		return (vfs.set_namedspace (view, spacedescriptor));
		//CHECKME last-used dir is opened when we change space ?
	}
	else
	{
		printd (WARN, "Cannot find vfs plugin");
		return FALSE;
	}
}
/**
@brief setup the pane to which @a rt applies to show data for a specified namespace
If @a utfpath has a NULL localpath, then the relevant history or vtab path will be used
@param rt pointer to pane data struct
@param localpath pointer to path and namespace data struct

@return TRUE on successful change or no change needed
*/
gboolean e2_pane_change_space (E2_PaneRuntime *rt, VPATH *utfpath)
{
	PlaceInfo *current = rt->view->spacedata;
	if (current == utfpath->spacedata)
		return TRUE;	//nothing to change
//E2_VFSTMP may want to change dir anyway
	if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.set_space))
	{
		ViewInfo *view = (rt == NULL) ? curr_view : (ViewInfo *)rt;
		if (!vfs.set_space (view, utfpath->spacedata, TRUE))
			return FALSE;
//E2_VFSTMP CHECKME last-used dir is opened when we change space ?
		if (utfpath->path == NULL)
			_e2_pane_change_dir (rt, "FIXME-HISTORY-ITEM", FALSE, TRUE);
		else
			_e2_pane_change_dir (rt, utfpath->path, TRUE, TRUE);
		return TRUE;
	}
	else
	{
		printd (WARN, "Cannot find vfs plugin");
		return FALSE;
	}
}
#endif //def E2_VFS
/* *
@brief adjust any update-history-flag in command line(s) present in the pane represented by @a rt
This is a mechanism for anything to specify whether to update dirline history
during a cd process
In principle, the flag read/write could be racy, but the risk seems way too low
to bother with a mutex
@param rt pointer to pane data struct, or NULL for active pane
@value new value for the flag
@return
*/
/*void e2_pane_flag_history (E2_PaneRuntime *rt, gboolean value)
{
	if (rt == NULL)
		rt = curr_pane;
	if (rt->toolbar.has_command_line)
	{
		GList *line;
		for (line = app.command_lines; line != NULL ; line = line->next)
		{
			E2_CommandLineRuntime *clrt = (E2_CommandLineRuntime *)line->data;
			if (clrt->pane == rt)
				clrt->update_history = value;
		}
	}
}
*/
/**
@brief set filepanes' properties to indicate which one is active

The "active-pane-signal" config option is checked, and: the column
headers will be re-coloured, or the filename column title will be
[un]bolded, or the treeviews' sensitivity will be toggled

@return
*/
void e2_pane_flag_active (void)
{
	E2_OptionSet *set;
#ifdef E2_SMALLSCREEN
	set = e2_option_get ("active-pane-tools");
	if (e2_option_bool_get_direct (set))
	{
		//process respective toolbar_container_box's to leave the dirline combo visible,
		//or toolbar_container's to hide that too
		gtk_widget_hide (other_pane->toolbar.toolbar_container);
		gtk_widget_show (curr_pane->toolbar.toolbar_container);
	}
	else
	{
#endif
		set = e2_option_get ("active-pane-signal");
		gint htype = e2_option_int_get_direct (set);
		switch (htype)
		{
			case 1:
			{  //set bold Filename-column header title
				gchar *new_title = gettext (e2_all_columns[FILENAME].title);
				gchar *new_title2 = g_strconcat ("<b>", new_title,"</b>", NULL);
				gtk_label_set_markup (curr_view->name_label, new_title2);
				g_free (new_title2);
				//set normal Filename-column header title
				gtk_label_set_text (other_view->name_label, new_title);
				break;
			}
			case 2:
			{  //make active pane sensitive
				gtk_widget_set_sensitive (curr_view->treeview, TRUE);
				//make inactive pane insensitive
				gtk_widget_set_sensitive (other_view->treeview, FALSE);
				break;
			}
			default:
			{
				//set all active columns' header color
#ifdef USE_GTK3_0
				GdkRGBA color;
				GdkRGBA *active_btncolor = &color;
				e2_option_color_get_RGBA ("color-active-pane", active_btncolor);
#else
				GdkColor *active_btncolor = e2_option_color_get ("color-active-pane");
#endif
				GList *base = gtk_tree_view_get_columns (GTK_TREE_VIEW (curr_view->treeview));
				GList *columns;
#ifdef USE_GTK2_14
				GtkWidget *header_button;
#endif
				for (columns = base; columns !=NULL; columns = columns->next)
				{
#ifdef USE_GTK2_14
					header_button = gtk_widget_get_ancestor (
						gtk_tree_view_column_get_widget ((GtkTreeViewColumn *)columns->data),
						GTK_TYPE_BUTTON);
# ifdef DEBUG_MESSAGES
					if (header_button == NULL)
						printd (ERROR, "missing column-header button");
					else
					{
# endif
# ifdef USE_GTK3_0
						gtk_widget_override_background_color (header_button,
# else //gtk 2.14-2.22
						gtk_widget_modify_bg (header_button,
# endif
#else //gtk < 2.14
						gtk_widget_modify_bg (((GtkTreeViewColumn *)columns->data)->button,
#endif
						GTK_STATE_NORMAL, active_btncolor);
#ifdef USE_GTK2_14
# ifdef DEBUG_MESSAGES
					}
# endif
#endif
				}
				g_list_free (base);
				//revert all inactive columns' header color to default
				base = gtk_tree_view_get_columns (GTK_TREE_VIEW (other_view->treeview));
				for (columns = base; columns != NULL; columns = columns->next)
				{
#ifdef USE_GTK2_14
					header_button = gtk_widget_get_ancestor (
						gtk_tree_view_column_get_widget ((GtkTreeViewColumn *)columns->data),
						GTK_TYPE_BUTTON);
# ifdef DEBUG_MESSAGES
					if (header_button == NULL)
						printd (ERROR, "missing column-header button");
					else
					{
# endif
# ifdef USE_GTK3_0
						gtk_widget_override_background_color (header_button,
# else //gtk 2.14-2.22
						gtk_widget_modify_bg (header_button,
# endif
#else //gtk < 2.14
						gtk_widget_modify_bg (((GtkTreeViewColumn *)columns->data)->button,
#endif
							GTK_STATE_NORMAL, NULL);
#ifdef USE_GTK2_14
# ifdef DEBUG_MESSAGES
					}
# endif
#endif
				}
				g_list_free (base);
				break;
			}
		}
#ifdef E2_SMALLSCREEN
	}
#endif
}
/**
@brief change active pane from the current one to the other one
Assumes BGL is closed on arrival here
@return
*/
void e2_pane_activate_other (void)
{
//	printd (DEBUG, "e2_pane_activate_other ()");
	E2_PaneRuntime *tmp = other_pane;
	other_pane = curr_pane;
	curr_pane = tmp;
	//force a status-line update
	last_selected_rows = -1;
#ifdef E2_STATUS_DEMAND
	OPENBGL
	e2_window_update_status_bar (NULL);
	CLOSEBGL
#endif
	e2_fileview_switch_views ();
	e2_window_set_title_path (app.main_window, curr_view); //change window title, if relevant
	e2_pane_flag_active ();	//change the status-indicator for the panes

//	gtk_widget_grab_focus (tmp->focus_widget);
#ifdef USE_GTK2_18
	if (gtk_widget_has_focus (other_view->treeview))
#else
	if (GTK_WIDGET_HAS_FOCUS (other_view->treeview))
#endif
		gtk_widget_grab_focus (curr_view->treeview);
	OPENBGL
	e2_hook_list_run (&app.hook_pane_focus_changed, tmp);
	CLOSEBGL
}
/**
@brief decide which pane is relevant, from available resources
@param from the widget which triggered an action
@param actiondata action data or action-runtime data of unspecified form
@param usedata store for start of non-pane data if @a actiondata is a string
 and may have > 1 part (comma- or space-separated, pane num first), else NULL

@return rt data for pane 1 or pane 2 or active pane if that's chosen or if can't
 decide 1 or 2, or if @a usedata is non-NULL and data can't be deciphered, NULL
*/
E2_PaneRuntime *e2_pane_get_runtime (gpointer from, gpointer actiondata,
	const gchar **usedata)
{
	guint which = E2PANECUR;
	if (usedata == NULL)
	{
		if (actiondata != NULL)
		{
			if ((E2_PaneRuntime *)actiondata == &app.pane1
				/*UNUSED || actiondata == GUINT_TO_POINTER (1)*/
				)
				which = E2PANE1;
			else if ((E2_PaneRuntime *)actiondata == &app.pane2
				/*UNUSED || actiondata == GUINT_TO_POINTER (2)*/
				)
				which = E2PANE2;
			else if (*(gchar *)actiondata == '1')
				which = E2PANE1;
			else if (*(gchar *)actiondata == '2')
				which = E2PANE2;
			else if (*(gchar *)actiondata == '0') //current
				from = NULL;	//no further checking
		}
	}
	else
	{
		//parse multi-part string data, with pane number (if any) first
		gchar *first = e2_utils_pass_whitespace ((gchar *) actiondata);
		if (first == NULL)
			*usedata = NULL;
		else
		{
			gchar num, sep;
			//is a relevant pane-number provided?
			if ((num = *first) < '0' || num > '2' ||
				!((sep = *(first + sizeof (gchar))) == ' ' || sep == ',' || sep == '\0')
				)
				*usedata = first;	//nope, assume data is first arg
			else
			{
				if (num == '1')
					which = E2PANE1;
				else if (num == '2')
					which = E2PANE2;
				else //num == '0'
					from = NULL;	//no further checking
				gchar *second = e2_utils_pass_whitespace (first + sizeof (gchar));
				if (second != NULL && *second == ',')
					second = e2_utils_pass_whitespace (second + sizeof (gchar));
				*usedata = second; //maybe NULL
			}
		}
	}

	//now try to interpret the originator
	if (which == E2PANECUR && from != NULL)
	{
		if (GTK_IS_TREE_VIEW (from))
		{
			if (GTK_WIDGET (from) == app.pane1.view.treeview)
				which = E2PANE1;
			if (GTK_WIDGET (from) == app.pane2.view.treeview)
				which = E2PANE2;
		}
		else if (GTK_IS_ENTRY (from))
		{
			E2_CommandLineRuntime *clrt = g_object_get_data (G_OBJECT (from),
				"command-line-runtime");
			if (clrt != NULL && !clrt->original)
			{
				if (clrt->pane == &app.pane1)
					which = E2PANE1;
				else //only 2 panes
					which = E2PANE2;
			}
		}
		else if (GTK_IS_BUTTON (from))
		{
			GtkWidget *bar = gtk_widget_get_ancestor (GTK_WIDGET (from), GTK_TYPE_TOOLBAR);
			if (bar != NULL)
			{
				E2_ToolbarData **thisbar;
				for (thisbar = app.bars; *thisbar != NULL; thisbar++)
				{
					E2_ToolbarRuntime *rt = (*thisbar)->rt;
					if (GTK_WIDGET(rt->toolbar) == bar)
					{
						if ((*thisbar)->type == E2_BAR_PANE1)
							which = E2PANE1;
						else if ((*thisbar)->type == E2_BAR_PANE2)
							which = E2PANE2;
						break;
					}
				}
			}
		}
/*		else if (GTK_IS_MENU_ITEM (from))
		{
			//FIGURE something out ?
		}
*/
	}

	switch (which)
	{
		case E2PANE1:
			return &app.pane1;
		case E2PANE2:
			return &app.pane2;
		default:
			return curr_pane;
	}
}
/**
@brief decide which pane is relevant for @button
@param button a toolbar button widget which triggered a callback

@return rt data for pane 1 or pane 2 or active pane if can't decide 1 or 2
*/
static E2_PaneRuntime *_e2_pane_get_button_runtime (GtkWidget *button)
{
	printd (DEBUG, "_e2_pane_get_button_runtime, button %x", GPOINTER_TO_INT(button));
	GtkWidget *bar = gtk_widget_get_ancestor (button, GTK_TYPE_TOOLBAR);
	if (bar != NULL)
	{
		E2_ToolbarData **thisbar;
		for (thisbar = app.bars; *thisbar != NULL; thisbar++)
		{
			E2_ToolbarRuntime *rt = (*thisbar)->rt;
			if (GTK_WIDGET(rt->toolbar) == bar)
			{
				if ((*thisbar)->type == E2_BAR_PANE1)
				{
					return &app.pane1;
				}
				else if ((*thisbar)->type == E2_BAR_PANE2)
				{
					return &app.pane2;
				}
				break;
			}
		}
	}
	return curr_pane;
}
/* *
@brief change-directory completion-watch timer-function

@param completed_flag store for flag to watch

@return FALSE when ready to shutdown timer
*/
/*static gboolean _e2_pane_cd_watch (E2_CDType *completed_flag)
{
	return (*completed_flag == CD_NOTFINISHED);
	//FIXME signal that this timer is being shutdown
}
*/
/* *
@brief synchronously change the directory for a specified pane

This is a wrapper for _e2_pane_change_dir(), with update of pane history list
and hooklist
It must be called with gtk's thread-lock closed

@param rt pointer to pane data struct, or NULL for current pane
@param path utf8 string containing actual or virtual path to directory to be opened
@param completed_flag store for flag to set to CD_SUCCESS if/when cd is completed

@return
*/
/*
void e2_pane_change_dir_sync (E2_PaneRuntime *rt, gchar *path,
	E2_CDType *completed_flag)
{
	*completed_flag = CD_NOTFINISHED;
	if (rt == NULL)
		rt = curr_pane;
	E2_Listman *data = (rt == &app.pane1) ?
		&app.pane1.view.listcontrols : &app.pane2.view.listcontrols;
	_e2_pane_change_dir (rt, path, completed_flag, TRUE, TRUE);
	LISTS_LOCK
	gchar *new = g_strdup (data->newpath); BAD, MAYBE NULL ALREADY
	LISTS_UNLOCK
	//FIXME wait for completion, without blocking the cd process
//	app.timers[?] = g_timeout_add (90, (GSourceFunc) _e2_pane_cd_watch, &completed_flag);
	//FIXME nicely block while the timer is still running
	while (TRUE)
	{
		gboolean wait;
		LISTS_LOCK
		wait = data->cd_requested || g_atomic_int_get (&data->cd_working)
			|| strcmp (new, rt->view->dir);
		LISTS_UNLOCK
		if (wait)
			usleep (90000)
		else
		{
			*completed_flag = CD_SUCCESS;
			break;
		}
	}
	g_free (new);
}
*/
/**
@brief if the dir shown in pane related to @a rt is accessible, open it, otherwise
  open a parent or other directory that is accessible
Used only at session-start so dir-history update is applied only if history is
not cached
This expects BGL closed (for showing any error message)
@param rt pointer to file-pane data struct

@return TRUE if rt->path was opened, FALSE if an alternative was opened
*/
gboolean e2_pane_goto_accessible_path (E2_PaneRuntime *rt)
{
	printd (DEBUG, "check that %s is accessible", rt->path);
#ifdef E2_VFSTMP
	//FIXME path for non-native dirs
#else
	gchar *checkpath = g_strdup (rt->path);	//don't clobber the rt->path string
#endif
#ifdef E2_VFSTMP
	//FIXME path check for non-mounted dirs not relevant ?
	//what about incomplete mount of fuse-dir
#endif
	gboolean retval = e2_fs_get_valid_path (&checkpath, TRUE E2_ERR_NONE());
	if (!retval)
	{
		gchar *msg = g_strdup_printf (_("Cannot access %s, going to %s instead"),
#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#else
			rt->path, checkpath);
#endif
		e2_output_print_error (msg, TRUE);
	}
	gboolean history = (rt->opendirs == NULL);	//start history if cached one not present
	_e2_pane_change_dir (rt, checkpath, history, TRUE);
	g_free (checkpath);
	return retval;
}
/* *
@brief for a specified pane, open the directory @a number places forward in the history list

The last entry in the list will be used if we try to go forward too far

@param rt  pointer to pane data struct
@param number integer no.of places in the history list to go forward

@return
*/
/*static void _e2_pane_go_forward_ (E2_PaneRuntime *rt, gint number)
{
NOTE - IF USED, THIS MAY NEED A BASE-INDEX CORRECTION (1 TO 0) AND LIST-DIRECTION SWAP
	printd (DEBUG, "_e2_pane_go_forward (rt:,number:%d", number);
	guint old = rt->opendir_cur;
	guint len = g_list_length (rt->history);
	rt->opendir_cur += number;
	if (rt->opendir_cur > g_list_length (rt->history))
		rt->opendir_cur = len;
	if (old != rt->opendir_cur)
	{
//		gchar *dir = F_FILENAME_TO_LOCALE ();
		gchar *dir =
			g_list_nth_data (rt->history, rt->opendir_cur - 1);
		_e2_pane_change_dir (rt, dir, NULL, FALSE, TRUE);
//		F_FREE (dir, );
	}
} */
/* *
@brief for a specified pane, open the directory @a number places back in the history list

The first entry in the list will be used if we try to go back too far

@param rt  pointer to pane data struct
@param number integer no.of places in the history list to go back

@return
*/
/*static void _e2_pane_go_back_ (E2_PaneRuntime *rt, gint number)
{
NOTE - IF USED, THIS MAY NEED A BASE-INDEX CORRECTION (1 TO 0) AND LIST-DIRECTION SWAP
	printd (DEBUG, "_e2_pane_go_back (rt:,number:%d", number);
	guint old = rt->opendir_cur;
	rt->opendir_cur -= number;
	if (rt->opendir_cur < 1)
		rt->opendir_cur = 1;
	if (old != rt->opendir_cur)
	{
		gchar *dir = g_list_nth_data (rt->history, rt->opendir_cur - 1);
		_e2_pane_change_dir (rt, dir, FALSE, TRUE);
	}
}*/
/**
@brief change the directory for a specified pane

This is a wrapper for _e2_pane_change_dir (), with update of
pane history list and hooklist
gtk's thread-lock (BGL) may be open or closed

@param rt pointer to pane data struct, or NULL for current pane
@param newpath utf8 string containing actual or virtual path to directory to be opened

@return
*/
void e2_pane_change_dir (E2_PaneRuntime *rt, const gchar *newpath)
{
	_e2_pane_change_dir (rt, newpath, TRUE, TRUE);
}
/**
@brief change directory displayed in a specified pane
Makes no assumption about whether BGL is active, but uses timer to force-open
BGL for downstream processing where needed
~ at start of path is interpreted to $HOME
Surrounding single- or double-quotes are removed
Redundant separators are removed, and one is added to the end, if not already
there
Any ".." at start or later in the path is corrected if possible (or if not,
ignored)
Does nothing if the path string is empty, shows error msg if path is unreachable
Session-start check for valid path done in main(), not here.
The supplied path string is unchanged.
New path is stored in heap @ rt->path.
New path is also stored in array @ rt->view->dir.

@param rt pointer to pane data struct, or NULL for current pane
@param newpath UTF-8 string containing space-relative path of directory to be opened
@param history TRUE to update pane history list
@param hook TRUE to run change-dir hook functions (if any, normally has at least update dirline)

@return
*/
static void _e2_pane_change_dir (E2_PaneRuntime *rt, const gchar *newpath,
	gboolean history, gboolean hook)
{
	gchar *freeme, *path, *currpath;
	printd (DEBUG, "_e2_pane_change_dir (rt:,path:\"%s\",update:%d,hook:%d)",
		newpath, history, hook);

	if (newpath == NULL)
		return;

	path = e2_utils_unquote_string (newpath);
	g_strchug (path);	//trailing whitespace may be deliberate

	if (*path == '\0')
	{
		//quick exit
		g_free (path);
		return;
	}

	//work on currently active file pane if argument is NULL
	if (rt == NULL)
		rt = curr_pane;

	//work with a copy of path string which might be in transition by a prior cd
	LISTS_LOCK
	currpath = g_strdup (rt->path);
	LISTS_UNLOCK

	if (!strcmp (path, "..") &&
		!strcmp (currpath, G_DIR_SEPARATOR_S)
#ifdef E2_VFSTMP
	//CHECKME how does updir from archive-root work here ?
#endif
		)
	{
		//ignore impossible change
		g_free (path);
		g_free (currpath);
		return;
	}
	//this fails in the event of trailing whitespace
	else if (path[0] == '.' &&
		(path[1] == '\0' //e2_utils_pass_whitespace (path+1) == NULL
		 || (path[1] == G_DIR_SEPARATOR && e2_utils_pass_whitespace (path+2) == NULL)))
	{
		g_free (path);
#ifdef E2_VFSTMP
		freeme = ?;
#else
		freeme = g_get_current_dir ();
#endif
		path = F_FILENAME_FROM_LOCALE (freeme);
		if (path != freeme)
			g_free (freeme);
	}
	else
	{
		//this may be a directly-entered cd, or path-completion may be turned off
		//so we interpret here too, but with limited "~" handling
		freeme = path;
		path = e2_utils_expand_macros (path, NULL);
		if (path == NULL || path == GINT_TO_POINTER (0x1))
			path = freeme;
		else
			g_free (freeme);
		freeme = path;
		path = e2_utils_replace_vars (path, TRUE);
		g_free (freeme);
	}

/* #ifndef E2_FILES_UTF8ONLY
	if (!app.utf8_filenames)
	{
		//this test not needed with only-utf8 file coding
		if (! g_utf8_validate (path, -1, NULL))
		{
			freeme = path;
			path = e2_utf8_filename_from_locale (path);
			g_free (freeme);
		}
	}
#endif */

#ifdef E2_VFSTMP
	//FIXME for changes up from a virtual root ...
	if ( strcmp (path, "..")
	  || strcmp (currpath, G_DIR_SEPARATOR_S))
	{
#endif
	//cleanup the new path string
	//these funcs both ensure a trailing /, which gets into rt->path
	//(see below) and onto dir lines
		if (e2_option_bool_get_direct (rt->opt_transparent)
			|| !strcmp (path, ".."))	//always handle cd .. properly
		{	//relative path segment(s) conversion
			//clean and convert the path string, if needed
			freeme = path;
#ifdef E2_VFSTMP
		//CHECKME confirm this is ok for non-local work-dir
#endif
			path = e2_utils_translate_relative_path (currpath, path);
			g_free (freeme);
		}
		else
		{
			if (!g_path_is_absolute (path))
			{
				freeme = path;
				path = g_strconcat (currpath, path, NULL);
				g_free (freeme);
			}
			//make sure the user didn't enter a 'dirty' path string
#ifdef E2_VFSTMP
//CHECKME confirm this is ok for non-local work-dir
			path = e2_utils_path_clean (path);
#else
			path = e2_utils_path_clean (path);
#endif
		}
#ifdef E2_VFSTMP
	}
#endif

	g_free (currpath);

	E2_Listman *data = (rt == &app.pane1) ?
		&app.pane1.view.listcontrols : &app.pane2.view.listcontrols;
	LISTS_LOCK
	data->view = (ViewInfo *)rt;
	data->history = history;
	data->hook = hook;
	//now signal we're ready to do a new cd
//	any prior data->newpath is copied and cleared downstream, when ready
	data->newpath = path;	//this is a copy
	/*we use a timer, not a thread directly, to control the cd process cuz:
		. we're happy to do the cd at a reasonably non-busy time
		. if gtk's "native" BGL mutex is in play, _must_ ensure that BGL is off,
		  for the downstream code that performs the cd
	  (BUT this does make it tough to wait for the end of any specific cd)
	  we check in the timer callback, not here, for any duplicated timer for this
	  same pane (or the other one), so as to eliminate any race and missed cd request */
	gint interval = (g_atomic_int_get (&data->cd_working) != 0) ? 90 :	//check about every 90 mS for completion of prior cd
		4; //short initial delay in case there's a following request for other pane
//	data->timer = with possible repeated timers for the same pane, the id is useless
	LISTS_UNLOCK
//#ifdef DEBUG_MESSAGES
//	guint tid =
//#endif
	g_timeout_add_full (G_PRIORITY_HIGH, interval,
		(GSourceFunc) e2_fileview_cd_manage, data, NULL);
//	printd (DEBUG, "initiated cd timer = %d", tid);
//	LISTS_UNLOCK
/*	//setup for completion-watching if requested
	if (completed_flag != NULL)
	{
		E2_CDwatch *data2 = ALLOCATE (E2_CDwatch);
		CHECKALLOCATEDWARN (data2, return;)
		data2->view = rt->view;
		data2->newpath = g_strdup (path);
		data2->repeats = 0;
		data2->completed_flag = completed_flag;
		//this timer stops after 100 callbacks = 10 seconds
		//CHECKME make this cancellable at session-end ?
		//data->watchtimer_id =
		g_timeout_add (100,
			(GSourceFunc) e2_fileview_cd_watch, data2);
	}
*/
}
/**
@brief show a chosen directory in a pane

@param rt pointer to pane data struct
@param entry entry widget of dirline for pane associated with @a rt

@return TRUE if the directory was changed successfully
*/
gboolean e2_pane_choose_new_dir (E2_PaneRuntime *rt, GtkWidget *entry)
{
#ifdef E2_VFSTMP
	//FIXME ensure this works with non-mounted dirs
#endif
	//create relevant path string
	const gchar *choice = gtk_entry_get_text (GTK_ENTRY (entry));
	gchar *dir;
	if (g_str_has_prefix (choice, G_DIR_SEPARATOR_S))
		dir = g_strdup (choice);
	else
	{
#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#else
		//path string might be involved in a current cd
		LISTS_LOCK
		dir = g_strconcat (rt->path, choice, NULL);
		LISTS_UNLOCK
#endif
	}
#ifdef E2_VFSTMP
	//FIXME handle vfs error reasonably
#endif
	e2_fs_get_valid_path (&dir, FALSE E2_ERR_NONE());

	gchar *newdir = NULL;
	e2_opendir_dialog_create (&rt->view, dir, &newdir);
	g_free (dir);
	if (newdir != NULL)
	{	//something was selected
		_e2_pane_change_dir (rt, newdir, TRUE, TRUE);
		g_free (newdir);
		return TRUE;
	}
	//user cancelled
	gtk_widget_grab_focus (entry);
	gtk_editable_select_region (GTK_EDITABLE (entry), -1, -1);	//unselect the entry contents
	return FALSE;
}
/* *
@brief timeout function for checking completion of fileview creation

@param userdata data specified when the timer was initialised, here, always NULL, unused

@return TRUE until update is completed, then FALSE
*/
/*gboolean _e2_pane_views_complete (gpointer userdata)
{
	if (!(curr_view->listing || other_view->listing))
	{
		printd (DEBUG, "finished waiting for fileview completion");
		CLOSEBGL
		e2_fileview_switch_views ();
		e2_pane_flag_active ();	//change the status-indicator for the panes
		gtk_widget_grab_focus (curr_view->treeview);
		OPENBGL
		//run this with BGL open/off
		e2_hook_list_run (&app.hook_pane_focus_changed, curr_pane);
		return FALSE;
	}
	return TRUE;  //continue waiting
}
*/
static GPtrArray *menued = NULL;

static void _e2_pane_history_menu_filter_init (void)
{
	if (menued == NULL)
		menued = g_ptr_array_sized_new (8);
	else
		g_ptr_array_set_size (menued, 0);
}

static void _e2_pane_history_menu_filter_finalise (void)
{
	if (menued == NULL)
	{
		g_ptr_array_free (menued, TRUE);
		menued = NULL;
	}
}
/**
@brief decide whether to include the dir whose path is data for @a node in a history menu
Mainly, this checks for duplicates. Also excludes _all_ items same as current path.
@param node a member of rt->opendirs
@param rt pointer to data struct for pane being processed
@return 0 if @a node has a duplicate string or a non-current path same as current,
 1 if it belongs to the current (displayed) dir, or 2 otherwise
*/
static guint _e2_pane_history_menu_filter (GList *node, E2_PaneRuntime *rt)
{
	if (strcmp ((gchar *)node->data, rt->path) == 0)
	{
		guint len = g_list_length (rt->opendirs);
		if (g_list_position (rt->opendirs, node) != len - 1 - rt->opendir_cur)
			return 0;
		else
			return 1;
	}
	guint retval;
	if (g_ptr_array_remove_fast (menued, node->data)) //hack for quick finding
		retval = 0;
	else
		retval = 2;
	g_ptr_array_add (menued, node->data);	//log it (again)
	return retval;
}
/**
@brief construct destroyable menu of all history for the pane associated with @rt

@param rt pointer to pane data struct

@return the menu widget (unshown), or NULL if no item is relevant
*/
GtkWidget *e2_pane_visited_menu (E2_PaneRuntime *rt)
{
	_e2_pane_history_menu_filter_init ();
	guint done = 0;

/* show all non-duplicated items in the list, with priority to the current item,
   in reverse order to their list positions = in history-order
	e.g. history-index  6 5 4 3 2 1 0
		 list-position  0 1 2 3 4 5 6
*/
	GtkWidget *menu = e2_menu_get ();
	HISTORY_LOCK
	GList *member = g_list_last (rt->opendirs);
	HISTORY_UNLOCK
	while (member != NULL)
	{
		guint choice = _e2_pane_history_menu_filter (member, rt);
		if (choice > 0)
		{	//an item we will display
			GtkWidget *item;
			if (choice == 1)
				item = gtk_radio_menu_item_new_with_label (NULL, (gchar *)member->data);
			else //==2
			{
				item = gtk_menu_item_new_with_label ((gchar *)member->data);
				g_signal_connect (G_OBJECT (item), "activate",
					G_CALLBACK (_e2_pane_history_activated_cb), rt);
			}
			gtk_widget_show_all (item);
			gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
			done = 1;
		}
		HISTORY_LOCK
		member = member->prev;
		HISTORY_UNLOCK
	}

	_e2_pane_history_menu_filter_finalise ();
	if (done == 0)
	{
		gtk_widget_destroy (menu);
		menu = NULL;
	}
	return menu;
}
/* *
@brief helper for toggle hidden action
Downstream functions require BGL closed
@param view pointer to data for view being changed

@return TRUE always
*/
/*static gboolean _e2_pane_toggle_hidden_ (ViewInfo *view)
{
	view->show_hidden = !view->show_hidden;
	e2_fileview_refilter_list (view);
#ifdef E2_STATUS_DEMAND
	if (view == curr_view)
	{
use a selection-change cb ?
    	OPENBGL
		e2_window_update_status_bar (NULL);
    	CLOSEBGL
	}
#endif
	//toggle the button
	E2_ToggleType num = (view == &app.pane1.view) ?
		E2_TOGGLE_PANE1HIDDEN : E2_TOGGLE_PANE2HIDDEN;
	e2_toolbar_button_toggle (toggles_array[num]);
	return TRUE;
}
*/

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief pane history goto-menu-item "activated" signal callback
Note that this is called after activation from a right-click menu. A hover-menu
click is handled by _e2_action_hover_buttonpress_cb(), which initiates activation
@param menuitem the selected item
@param rt pointer to pane data struct

@return
*/
static void _e2_pane_history_activated_cb (GtkMenuItem *menuitem, E2_PaneRuntime *rt)
{
#ifdef E2_VFSTMP
	//FIXME ensure this works with non-mounted dirs i.e. history is specific to the fs type/place
#endif
	GtkWidget* label = gtk_bin_get_child (GTK_BIN (menuitem));
	NEEDCLOSEBGL
	const gchar* dir = gtk_label_get_label (GTK_LABEL (label));
	NEEDOPENBGL
	printd (DEBUG, "Selected history menu item for %s", dir);
	_e2_pane_change_dir (rt, (gchar *)dir, TRUE, TRUE);
}

  /*******************/
 /***** actions *****/
/*******************/

/**
@brief refresh content of both pane views
Downstream functions expect gtk's BGL to be closed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the request was logged
*/
static gboolean _e2_pane_refresh_lists (gpointer from, E2_ActionRuntime *art)
{
	g_atomic_int_set (&p1dirty, 1);
	g_atomic_int_set (&p2dirty, 1);
#ifdef E2_FAM
# ifdef E2_VFSTMP
	gboolean withother = other_view->spacedata == NULL || other_view->spacedata->monitored;
	gboolean retval =
		(curr_view->spacedata == NULL || curr_view->spacedata->monitored) ?
		e2_filelist_request_refresh (curr_view->dir, !withother) : TRUE;
	if (withother)
		retval = e2_filelist_request_refresh (other_view->dir, TRUE) && retval;
# else
	//2 tests, to lessen race due to dir-swap during first call
	gboolean retval = e2_filelist_request_refresh (curr_view->dir, FALSE);
	retval = e2_filelist_request_refresh (other_view->dir, TRUE) || retval;
# endif
	return retval;
#else
	e2_filelist_check_dirty (GINT_TO_POINTER (1));
	return TRUE;
#endif
}
/**
@brief change active pane from the current one to the other one
This also toggles the visible filepane, if only 1 is shown
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_activate_other_action (gpointer from, E2_ActionRuntime *art)
{
	//if showing only 1 filepane, show the other one now
	if (app.window.panes_paned_ratio < 0.001)
		e2_window_adjust_pane_ratio ("1");
	else if (app.window.panes_paned_ratio > 0.999)
		e2_window_adjust_pane_ratio ("0");
	e2_pane_activate_other ();
	return TRUE;
}
/**
@brief make a specified pane active if not already so, or else just focus the pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the active pane was changed
*/
static gboolean _e2_pane_activate (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	if (rt != curr_pane)
	{
		e2_pane_activate_other ();
		return TRUE;
	}
	else
	{
		gtk_widget_grab_focus (curr_view->treeview);
		return FALSE;
	}
}
/**
@brief focus specified pane's file list, without changing active pane
This is primarily intended for the start of a chained a key-binding
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_focus (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	gtk_widget_grab_focus (rt->view.treeview);
	return TRUE;
}
/**
@brief action to go to top of specified pane file list
This is primarily intended for a custom key-binding
Downstream code expects BGL to be closed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if there is any item in the pane
*/
static gboolean _e2_pane_focus_top (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	if (gtk_tree_model_iter_n_children (rt->view.model, NULL) > 0)
	{
		e2_fileview_focus_row (&rt->view, 0, TRUE, TRUE, FALSE, TRUE);
		return TRUE;
	}
	return FALSE;
}
/**
@brief action to go to bottom of specified pane file list
This is primarily intended for a custom key-binding
Downstream code expects BGL to be closed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if there is any item in the pane
*/
static gboolean _e2_pane_focus_bottom (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	gint n = gtk_tree_model_iter_n_children (rt->view.model, NULL);
	if (n > 0)
	{
		e2_fileview_focus_row (&rt->view, n-1, TRUE, TRUE, FALSE, TRUE);
		return TRUE;
	}
	return FALSE;
}
/* *
@brief change the directory shown in a specified pane or the current one
The cd is asynchronous, so probably not done immediately
Downstream function expects BGL to be closed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
/*static gboolean _e2_pane_change_dir_action (gpointer from, E2_ActionRuntime *art)
{
	const gchar *newpath = NULL;
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, &newpath);
	if (rt == NULL || newpath == NULL)
		return FALSE;
	_e2_pane_change_dir (rt, newpath, TRUE, TRUE);
	return TRUE;
}
*/
/**
@brief goto the next dir in the history list for specified or current pane

@param from the button, menu item etc which was activated, maybe a toolbar button
@param art action runtime data

@return TRUE unless there's nowhere to goto
*/
static gboolean _e2_pane_go_forward (gpointer from, E2_ActionRuntime *art)
{
//	printd (DEBUG, "_e2_pane_go_forward action");
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	//history entries are prepended to list, forward in history = back in list
	HISTORY_LOCK
	guint len;
	if (rt->opendirs != NULL)
		len = g_list_length (rt->opendirs);
	else
		len = 0;
	if (len == 0 || rt->opendir_cur == len - 1)
	{
		HISTORY_UNLOCK
		return FALSE;	//nowhere forward to go
	}

	gboolean popup;
	if ((ACTION_BUTTON (art,3) && ACTION_MASK (art, 0))
	 || (ACTION_BUTTON (art,1) && ACTION_MASK (art, GDK_CONTROL_MASK))	//deprecated
	 || (art->data != NULL && strstr ((gchar *)art->data, _("ctrl")) != NULL))
		popup = TRUE;
/*	else if (GTK_IS_BUTTON (from) || GTK_IS_MENU_ITEM (from))
	{
		popup = ACTION_MASK (art, GDK_CONTROL_MASK);
	}
*/
	else
		popup = FALSE;

	if (popup)
	{
		HISTORY_UNLOCK
		GtkWidget *menu = _e2_pane_popup_forward (GTK_WIDGET (from), rt);
		if (menu != NULL)
		{
			guint32 event_time = gtk_get_current_event_time ();
			if (GTK_IS_BUTTON(from))
				gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
					(GtkMenuPositionFunc) e2_toolbar_set_menu_position, from, 1, event_time);
			else
				//FIXME
				gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, event_time);
		}
	}
	else
	{
		rt->opendir_cur++;
	//	gchar *dir = F_FILENAME_TO_LOCALE ();
		gchar *dir = g_list_nth_data (rt->opendirs, len - 1 - rt->opendir_cur);
		HISTORY_UNLOCK
		_e2_pane_change_dir (rt, dir, FALSE, TRUE);
	//	F_FREE (dir, );
	}

	return TRUE;
}
/**
@brief hover callback to create and pop up menu for going forward in history list

Un-grabbed display of the menu is handled upstream.

@param from the button, menu item etc which was hovered
@param data pointer to hover data specified when the action was established

@return
*/
static void _e2_pane_forward_hover (GtkWidget *from, E2HoverData *data)
{
	E2_PaneRuntime *rt = _e2_pane_get_button_runtime (from);
	data->menu = _e2_pane_popup_forward (from, rt); //menu not shown
}
/**
@brief create destroyable menu of directories for going forward in history list

@param from the button, menu item etc which was activated
@param rt pointer to pane data struct

@return the created menu widget (unshown), or NULL if no item is relevant
*/
static GtkWidget *_e2_pane_popup_forward (GtkWidget *from, E2_PaneRuntime *rt)
{
	if (rt->opendirs == NULL)
		return NULL;
	guint len = g_list_length (rt->opendirs);
	if (rt->opendir_cur == len - 1)
		return NULL;	//nowhere forward to go

	_e2_pane_history_menu_filter_init (); //setup for duplicate-filtering
	guint done = 0;

/*	display items whose index is > opendir_cur i.e. positioned in the list
	before (list-len - 1 - opendir_cur), and in index-order i.e. the reverse
	order of their list positions e.g.
	 history-index  6 5 4 3 2 1 0
	 list-position  0 1 2 3 4 5 6
		current-index       4
		show index            5 6
		list index            1 0
*/
	GtkWidget *menu = e2_menu_get ();
	HISTORY_LOCK
	GList *member = g_list_nth (rt->opendirs, len - 2 - rt->opendir_cur);
	HISTORY_UNLOCK
	while (member != NULL)
	{
		if (_e2_pane_history_menu_filter (member, rt) == 2)
		{	//a unique item != current dir
			GtkWidget *item;
			item = gtk_menu_item_new_with_label ((gchar *)member->data);
			gtk_widget_show_all (item);
			gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
			printd (DEBUG, "added history menu item for %s", (gchar *)member->data);
			g_signal_connect (G_OBJECT (item), "activate",
				G_CALLBACK (_e2_pane_history_activated_cb), rt);
			done = 1;
		}
		HISTORY_LOCK
		member = member->prev;
		HISTORY_UNLOCK
	}

	_e2_pane_history_menu_filter_finalise (); //cleanup
	if (done > 0)
		g_signal_connect (G_OBJECT (menu), "selection-done",
			G_CALLBACK (e2_menu_selection_done_cb), NULL);
	else
	{
		gtk_widget_destroy (menu);
		menu = NULL;
	}
	return menu;
}
/**
@brief goto the previous dir in the history list for specified or current pane

@param from the button, menu item etc which was activated, maybe a toolbar button
@param art action runtime data

@return TRUE unless there's nowhere to goto
*/
static gboolean _e2_pane_go_back (gpointer from, E2_ActionRuntime *art)
{
//	printd (DEBUG, "_e2_pane_go_back action");
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	//history entries are prepended to list, so back in history = forward in list
	HISTORY_LOCK
	guint len;
	if (rt->opendirs != NULL)
		len = g_list_length (rt->opendirs);
	else
		len = 0;
	if (len == 0 || rt->opendir_cur == 0)
	{
		HISTORY_UNLOCK
		return FALSE;
	}

	gboolean popup;
	if ((ACTION_BUTTON (art,3) && ACTION_MASK (art, 0))
	 || (ACTION_BUTTON (art,1) && ACTION_MASK (art, GDK_CONTROL_MASK))	//deprecated
	 || (art->data != NULL && strstr ((gchar *)art->data, _("ctrl")) != NULL))
		popup = TRUE;
/*	else if (GTK_IS_BUTTON (from) || GTK_IS_MENU_ITEM (from))
	{
		popup = ACTION_MASK (art, GDK_CONTROL_MASK);
	}
*/
	else
		popup = FALSE;

	if (popup)
	{
		HISTORY_UNLOCK
		GtkWidget *menu = _e2_pane_popup_backward (GTK_WIDGET (from), rt);
		if (menu != NULL)
		{
			guint32 event_time = gtk_get_current_event_time ();
			if (GTK_IS_BUTTON(from))
				gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
					(GtkMenuPositionFunc) e2_toolbar_set_menu_position, from, 1, event_time);
			else
			//FIXME
			gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 1, event_time);
		}
	}
	else
	{
		rt->opendir_cur--;
		gchar *dir = g_list_nth_data (rt->opendirs, len - 1 - rt->opendir_cur);
		HISTORY_UNLOCK
		_e2_pane_change_dir (rt, dir, FALSE, TRUE);
	}

	return TRUE;
}
/**
@brief hover-action callback to create menu for going backward in history list

Un-grabbed display of the menu is handled upstream

@param from the button, menu item etc which was hovered
@param data pointer to hover data struct specified when the action was established

@return
*/
static void _e2_pane_backward_hover (GtkWidget *from, E2HoverData *data)
{
	E2_PaneRuntime *rt = _e2_pane_get_button_runtime (from);
	data->menu = _e2_pane_popup_backward (from, rt); //menu not shown
}
/**
@brief create destroyable menu of directories for going backward in history list

@param rt pointer to pane data struct

@return created menu widget (unshown), or NULL if no item is relevant
*/
static GtkWidget *_e2_pane_popup_backward (GtkWidget *from, E2_PaneRuntime *rt)
{
	if (rt->opendir_cur == 0)
	{
//		printd (DEBUG, "can't go back, no backward-menu");
		return NULL;
	}
	if (rt->opendirs == NULL)
	{
//		printd (DEBUG, "no history, no backward-menu");
		return NULL;
	}

	_e2_pane_history_menu_filter_init (); //setup to eliminate duplicates
	guint done = 0;

/*	display items whose index is < opendir_cur i.e. positioned in the list
	after (list-len - 1 - opendir_cur), and in reverse-index-order i.e. the same
	order as their list positions e.g.
	 history-index  6 5 4 3 2 1 0
	 list-position  0 1 2 3 4 5 6
		current-index       4
		show index  3 2 1 0
		list index  3 4 5 6
*/
	guint len = g_list_length (rt->opendirs); //say, 7
//	printd (DEBUG, "current history-index %u", rt->opendir_cur); //say, 4
//#ifdef DEBUG_MESSAGES
//	guint curr = len - rt->opendir_cur; //(-1 + 1 cancelled) 3
//	printd (DEBUG, "list-length %u list-start-position %u", len, curr);
//#endif
	GtkWidget *menu = e2_menu_get ();
	HISTORY_LOCK
	GList *member = g_list_nth (rt->opendirs, len - rt->opendir_cur);
	HISTORY_UNLOCK
	while (member != NULL)
	{
		if (_e2_pane_history_menu_filter (member, rt) == 2)
		{	//a unique item != current dir
			GtkWidget *item = gtk_menu_item_new_with_label ((gchar *)member->data);
			gtk_widget_show_all (item);
			gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
			printd (DEBUG, "added history menu item for %s", (gchar *)member->data);
			g_signal_connect (G_OBJECT (item), "activate",
				G_CALLBACK (_e2_pane_history_activated_cb), rt);
//			printd (DEBUG, "list-index of displayed dir is %u", curr);
			done = 1;
		}
//		else
//			printd (DEBUG, "omit duplicate dir at list-index %u", curr);
		HISTORY_LOCK
		member = member->next;
		HISTORY_UNLOCK
//#ifdef DEBUG_MESSAGES
//		curr--;
//#endif
	}

	_e2_pane_history_menu_filter_finalise (); //cleanup
	if (done > 0)
		g_signal_connect (G_OBJECT (menu), "selection-done",
			G_CALLBACK (e2_menu_selection_done_cb), NULL);
	else
	{
//		printd (DEBUG, "empty backward-menu");
		gtk_widget_destroy (menu);
		menu = NULL;
	}
	return menu;
}
/**
@brief in a specified pane or current pane, open the parent of the current directory
The helper function prevents any change if at root dir already
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_go_up (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	_e2_pane_change_dir (rt, "..", TRUE, TRUE);
	return TRUE;
}
/**
@brief open pane other pane directory in this pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_mirror (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	E2_PaneRuntime *ort = (rt == &app.pane1) ? &app.pane2 : &app.pane1;
	LISTS_LOCK
	gchar *pathcopy = g_strdup (ort->path);
	LISTS_UNLOCK
	_e2_pane_change_dir (rt, pathcopy, TRUE, TRUE);
	g_free (pathcopy);
	return TRUE;
}
/**
@brief change 'other' pane to be the same as 'current' pane
Unqueued. Downstream function expects BGL to be closed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_sync (gpointer from, E2_ActionRuntime *art)
{
	_e2_pane_change_dir (other_pane, curr_pane->path, TRUE, TRUE);
	return TRUE;
}
/**
@brief show a specified directory in a pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if directory was changed
*/
static gboolean _e2_pane_goto (gpointer from, E2_ActionRuntime *art)
{
	const gchar *path;
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, &path);
	if (rt != NULL)
	{
		_e2_pane_change_dir (rt, path, TRUE, TRUE);
		return TRUE;
	}
	return FALSE;
}
/**
@brief show a chosen directory in a pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if directory was changed
*/
static gboolean _e2_pane_goto_choice (gpointer from, E2_ActionRuntime *art)
{
	E2_CommandLineRuntime *clrt;
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	GtkWidget *entry = NULL;
	if (GTK_IS_ENTRY (from))
	{
		clrt = g_object_get_data (G_OBJECT (from), "command-line-runtime");
		if (clrt != NULL && !clrt->original && clrt->pane == rt)
			entry = GTK_WIDGET (from);
	}
	if (entry == NULL)
	{	//try to find relevant entry
		GList *line;
		for (line = app.command_lines; line != NULL; line = line->next)
		{
			clrt = line->data;
			if (!clrt->original && clrt->pane == rt)
			{
				entry =
#ifdef USE_GTK2_14
					gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
					GTK_BIN (clrt->combo)->child;
#endif
				break;
			}
		}
	}
	if (entry == NULL)
		return FALSE;	//can't do anything

	return (e2_pane_choose_new_dir (rt, entry));
}
/**
@brief open the trash directory in the inactive pane
There is no hook update for the pane
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if trash directory is recorded
*/
static gboolean _e2_pane_goto_trash (gpointer from, E2_ActionRuntime *art)
{
	gchar *tp = e2_utils_get_trash_path (NULL, TRUE);	//UTF-8 string, "Trash/files/" at end
	if (tp != NULL)
	{
		//change the dir line entry, if possible
		E2_CommandLineRuntime *clrt;
		GList *line;
		for (line = app.command_lines; line != NULL ; line = g_list_next (line))
		{
			clrt = line->data;
			if (! clrt->original	//this is a dir line
				&& clrt->pane == other_pane)
			{
				GtkWidget *entry =
#ifdef USE_GTK2_14
					gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
					GTK_BIN (clrt->combo)->child;
#endif
				gtk_entry_set_text (GTK_ENTRY (entry), tp);
				break;
			}
		}
		_e2_pane_change_dir (other_pane, tp, TRUE, FALSE);	//NOTE no hook so no change of dirline !
		g_free (tp);
		return TRUE;
	}
	e2_output_print_error (_("No trash directory is available"), FALSE);
	return FALSE;
}
/**
@brief create and pop up adjacent to a toolbar button a destroyable filters menu for a specified pane

@param from the button, menu item etc which was activated, maybe a toolbar button
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_filter_menu_create (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	GtkWidget *menu = e2_menu_create_filter_menu (&rt->view);
	guint32 event_time = gtk_get_current_event_time ();
	if (GTK_IS_BUTTON(from))
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_toolbar_set_menu_position, from, 1, event_time);
	else
		//FIXME
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 1, event_time);

	return TRUE;
}
/**
@brief create and pop up near middle of pane filelist a destroyable filters-menu

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_filters_show (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	GtkWidget *menu = e2_menu_create_filter_menu (&rt->view);
	guint32 event_time = gtk_get_current_event_time ();
	gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
		(GtkMenuPositionFunc) e2_fileview_set_menu_position,
			rt->view.treeview, 0, event_time);
	return TRUE;
}
#ifdef E2_VFS
/**
@brief select-namespace action, where a string argument is supplied

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the change was successful
*/
static gboolean _e2_pane_select_space (gpointer from, E2_ActionRuntime *art)
{
	const gchar *name = NULL;
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, &name);
	if (rt == NULL || name == NULL)
		return FALSE;
	return (e2_pane_change_space_byuri (rt, name));
}
/**
@brief vfs menu-item callback
This must be here, not in vfs plugin, as it is one mechanism to load that plugin
Other callbacks are in the plugin
@param menu_item the activated menu item widget
@param view pointer to data struct for view to be changed

@return
*/
static void _e2_pane_vfs_dialog_cb (GtkMenuItem *menu_item, ViewInfo *view)
{
	if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.show_historydialog))
	{
		NEEDCLOSEBGL
		vfs.show_historydialog (view);
		NEEDOPENBGL
	}
	else
		printd (WARN, "Cannot find vfs plugin");
}
/**
@brief create and pop up destroyable menu of vfs places

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_vfs_menu_show (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	ViewInfo *view = rt->view;

	GtkWidget *item, *menu = e2_menu_get ();

	if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.create_placesmenu))	//note no cursor change in event of slow loading
	{
		gboolean history = FALSE;
		if ((ACTION_BUTTON (art,3) && ACTION_MASK (art,0))
		 || (ACTION_BUTTON (art,1) && ACTION_MASK (art,GDK_CONTROL_MASK)) //deprecated
		 || (art->data != NULL && strstr ((gchar *)art->data, _("ctrl")) != NULL))
			history = TRUE;
/*		else if (GTK_IS_BUTTON (from) || GTK_IS_MENU_ITEM (from))
		{
			if (ACTION_MASK (art,GDK_CONTROL_MASK))
				history = TRUE;
		}
*/
		vfs.create_placesmenu (menu, view, history);	//add plugin-specific items to menu
	}

	item = e2_menu_add (menu, _("_Edit places"), NULL, NULL,
		_e2_pane_vfs_dialog_cb, view);

	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	guint32 event_time = gtk_get_current_event_time ();
	if (art->action->data == NULL)	//CHECKME
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_fileview_set_menu_position,
				curr_view->treeview, 0, event_time);
	else
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_toolbar_set_menu_position, from, 1, event_time);
	return TRUE;
}
#endif
/**
@brief select all items in specified pane
The pane is made active, if it wasn't already
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_select_all (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	if (rt != curr_pane)
		e2_pane_activate_other ();
	e2_fileview_select_all (NULL, curr_view);
#ifdef E2_STATUS_DEMAND
use a selection-change cb ?
   	OPENBGL
	e2_window_update_status_bar (NULL);
	CLOSEBGL
#endif
	return TRUE;
}
/**
@brief select all items in specified pane which have same extension as first-selected item
The pane is made active, if it wasn't already
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the pane is not empty
*/
static gboolean _e2_pane_select_extension (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt;
	GtkTreeModel *model;
	GtkTreeIter iter;

	rt = e2_pane_get_runtime (from, art->data, NULL);
	if (rt != curr_pane)
		e2_pane_activate_other ();

	WAIT_FOR_REFRESH(curr_view)

	model = curr_view->model;
	if (gtk_tree_model_get_iter_first (model, &iter));
	{	//it's not empty
		FileInfo *info = e2_fileview_get_selected_first_local (curr_view, FALSE);
		if (info != NULL)
		{
			GtkTreeSelection *sel = curr_view->selection;
			gtk_tree_selection_unselect_all (sel);	//start with clean slate
			gboolean anycase = e2_option_bool_get ("anycase-filetypes");
			//get right-most extension
			gchar *base = info->filename;
			gchar *ext1 = strrchr (base, '.');
			if (ext1 == base)
				ext1 = NULL;	//ignore hidden-file indicator
			do
			{
				gchar *ext2;
				gtk_tree_model_get (model, &iter, FINFO, &info, -1);
				//get extension
				base = info->filename;
				ext2 = strrchr (base, '.');
				if (ext2 == base)
					ext2 = NULL;	//ignore hidden-file indicator
				if ((ext1 == NULL && ext2 == NULL)
					|| (ext1 != NULL && ext2 != NULL
						&& ((anycase && strcasecmp (ext1, ext2) == 0)
						 || (!anycase && strcmp (ext1, ext2) == 0))))
					gtk_tree_selection_select_iter (sel, &iter);
			} while (gtk_tree_model_iter_next (model, &iter));

			return TRUE;
		}
	}
	return FALSE;
}
/**
@brief invert selection state of all items in specified pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if there is any item in the pane
*/
static gboolean _e2_pane_invert_all (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	GtkTreeIter iter;
	GtkTreeModel *mdl = rt->view.model;
	if (gtk_tree_model_get_iter_first (mdl, &iter))
	{
		GtkTreeSelection *sel = rt->view.selection;
		do
		{
			if (gtk_tree_selection_iter_is_selected (sel, &iter))
				gtk_tree_selection_unselect_iter (sel, &iter);
			else
				gtk_tree_selection_select_iter (sel, &iter);
		} while (gtk_tree_model_iter_next (mdl, &iter));
#ifdef E2_STATUS_DEMAND
use a selection-change cb ?
       	OPENBGL
		e2_window_update_status_bar (NULL);
    	CLOSEBGL
#endif
		return TRUE;
	}
	return FALSE;
}
/**
@brief invert selection state of focused items in specified pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if there is any item in the pane
*/
static gboolean _e2_pane_invert_current (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	GtkTreeIter iter;
	if (gtk_tree_model_iter_nth_child (rt->view.model, &iter, NULL, rt->view.row))
	{
		GtkTreeSelection *sel = rt->view.selection;
		if (gtk_tree_selection_iter_is_selected (sel, &iter))
			gtk_tree_selection_unselect_iter (sel, &iter);
		else
			gtk_tree_selection_select_iter (sel, &iter);
#ifdef E2_STATUS_DEMAND
use a selection-change cb ?
    	OPENBGL
		e2_window_update_status_bar (NULL);
    	CLOSEBGL
#endif
		return TRUE;
	}
	return FALSE;
}
/**
@brief toggle display of hidden items in a specified pane
Downstream functions require BGL closed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_toggle_hidden (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	ViewInfo *view = (ViewInfo *)rt;
//	return (_e2_pane_toggle_hidden_ (view));
	view->show_hidden = !view->show_hidden;
	e2_fileview_refilter_list (view);
#ifdef E2_STATUS_DEMAND
	if (view == curr_view)
	{
use a selection-change cb ?
    	OPENBGL
		e2_window_update_status_bar (NULL);
       	CLOSEBGL
	}
#endif
	//toggle the button
	E2_ToggleType num = (view == &app.pane1.view) ?
		E2_TOGGLE_PANE1HIDDEN : E2_TOGGLE_PANE2HIDDEN;
	e2_toolbar_button_toggle (toggles_array[num]);
	return TRUE;
}
/**
@brief sort a visible column in specified pane
This is primarily intended for a key-binding
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE always
*/
static gboolean _e2_pane_sort (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	return (e2_fileview_sort_column (GPOINTER_TO_INT (art->action->data), &rt->view));
}

/**
@brief clear the whole dirline history, but not the actual entry string
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if a combo is found and processed
*/
static gboolean _e2_pane_clearall_dirline_history (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *prt = e2_pane_get_runtime (from, art->data, NULL);
	if (prt->toolbar.has_command_line)
	{
		//find the first-registered dir-line for the pane
		GList *list;
		for (list = app.command_lines; list != NULL ; list = g_list_next (list))
		{
			E2_CommandLineRuntime *clrt = (E2_CommandLineRuntime *) list->data;
			if (clrt->pane == prt && !clrt->original)
			{
				if (!e2_option_bool_get ("dir-line-history-auto"))
				{
					GtkWidget *entry =
#ifdef USE_GTK2_14
						gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
						GTK_BIN (clrt->combo)->child;
#endif
					e2_command_line_highlight (entry, FALSE);
				}
				gtk_list_store_clear (GTK_LIST_STORE (clrt->model));
				e2_list_free_with_data (&clrt->history);
				return TRUE;
			}
		}
	}
	return FALSE;
}

/**
@brief add the current dirline content to the corresponding history
@param from the dirline etc which was activated
@param art action runtime data

@return TRUE if a combo is found and processed
*/
static gboolean _e2_pane_add1_dirline_history (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *prt = e2_pane_get_runtime (from, art->data, NULL);
	if (prt->toolbar.has_command_line)
	{
		//find the first-registered dir-line for the pane
		GList *list;
		for (list = app.command_lines; list != NULL ; list = g_list_next (list))
		{
			E2_CommandLineRuntime *clrt = (E2_CommandLineRuntime *) list->data;
			if (clrt->pane == prt && !clrt->original)
			{
				if (prt->path != NULL)
				{
					GtkWidget *entry =
#ifdef USE_GTK2_14
						gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
						GTK_BIN (clrt->combo)->child;
#endif
					const gchar *path_raw = gtk_entry_get_text (GTK_ENTRY (entry));
					if (path_raw != NULL && *path_raw != '\0')
					{
						gchar *path = e2_utf8_unescape (path_raw, ' ');
						path = e2_utils_path_clean (path);
						gtk_entry_set_text (GTK_ENTRY (entry), path);
						gboolean same = (strcmp (path, prt->path) == 0);
						gboolean autoadd = e2_option_bool_get ("dir-line-history-auto");
						if (!autoadd && same)
							e2_command_line_highlight (entry, TRUE);
						//update dirline history
						if (!autoadd || !e2_option_bool_get ("dir-line-history-double"))
							e2_combobox_clear_value (clrt->combo, path, FALSE);
#ifdef USE_GTK2_24
						gtk_combo_box_text_prepend_text (GTK_COMBO_BOX_TEXT (clrt->combo), path);
#else
						gtk_combo_box_prepend_text (GTK_COMBO_BOX (clrt->combo), path);
#endif
						g_free (path);
						if (same)
							e2_combobox_set_active (clrt->combo, 0);

						return TRUE;
					}
					else
						break;
				}
				else
					break;
			}
		}
	}
	return FALSE;
}

/**
@brief clear the current dirline content from the corresponding history
This is primarily intended for a key-binding. Unlike the hard-coded binding, it
does not clear the actual entry string
@param from the dirline etc which was activated
@param art action runtime data

@return TRUE if a combo is found and processed
*/
static gboolean _e2_pane_clear1_dirline_history (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *prt = e2_pane_get_runtime (from, art->data, NULL);
	if (prt->toolbar.has_command_line)
	{
		//find the first-registered dir-line for the pane
		GList *list;
		for (list = app.command_lines; list != NULL ; list = g_list_next (list))
		{
			E2_CommandLineRuntime *clrt = (E2_CommandLineRuntime *) list->data;
			if (clrt->pane == prt && !clrt->original)
			{
				GtkWidget *entry =
#ifdef USE_GTK2_14
					gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
					GTK_BIN (clrt->combo)->child;
#endif
				e2_command_line_highlight (entry, FALSE);
				const gchar *this = gtk_entry_get_text (GTK_ENTRY (entry));
				e2_combobox_clear_value (clrt->combo, this, FALSE);
				return TRUE;
			}
		}
	}
	return FALSE;
}

/**
@brief clear the current forward-back history content
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_pane_clear_goto_history (gpointer from, E2_ActionRuntime *art)
{
	gboolean retval = FALSE;
	const gchar *arg = NULL;
	//art->data may be "1", "2", "1,2", "2,1" or "both"
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, &arg);
	if (rt != NULL)
	{
		//processed 1 or 2 or 1, or 2,
		rt->opendir_cur = 0;
		//do not clear the data, it's owned by app.dir_history
		g_list_free (rt->opendirs);
		rt->opendirs = NULL;
		retval = TRUE;
	}
	if (arg != NULL)
	{
		if (g_str_has_prefix (arg, "1")) //was 2,1
			rt = &app.pane1; //now do 1
		else if (g_str_has_prefix (arg, "2")) //was 1,2
			rt = &app.pane2; //now do 2
		else if (g_str_has_prefix (arg, _("both")) ||
				(strchr (arg, '1') != NULL && strchr (arg, '2') != NULL)) //some other permutation of both
		{
			rt = &app.pane1;
			rt->opendir_cur = 0;
			g_list_free (rt->opendirs);
			rt->opendirs = NULL;
			rt = &app.pane2;
		}
		else
			return FALSE;
		rt->opendir_cur = 0;
		g_list_free (rt->opendirs);
		rt->opendirs = NULL;
		retval = TRUE;
	}
	return retval;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief grab pointers to options previously registered

@param rt pointer to pane data struct

@return
*/
void e2_pane_create_option_data (E2_PaneRuntime *rt)
{
/*	gchar *option_name = (rt == &app.pane1) ?
		"panebar1" : "panebar2" ;	//do not translate
	rt->set = e2_option_get (option_name);
	option_name = g_strconcat (rt->name,"-use-startup-dir",NULL);
	rt->opt_use_startup = e2_option_get (option_name);
	g_free (option_name);
	option_name =  g_strconcat(rt->name,"-use-startup-dir-startup-dir",NULL);
	rt->opt_startup = e2_option_get (option_name);
	g_free (option_name);
	//create dependency
	rt->opt_startup->depends = rt->opt_use_startup->name;
*/
	rt->opt_transparent = e2_option_get ("transparent-dir-links");
}
/**
@brief create file pane and all its contents
This is used only at session-start
@param rt pointer to pane data struct

@return
*/
void e2_pane_create (E2_PaneRuntime *rt)
{
	gint num = (rt == &app.pane1) ? 1 : 2;
	rt->name = g_strdup_printf ("pane%d", num);
	rt->pane_sw = NULL;

	e2_pane_create_option_data (rt);
	//waiting until first-registration of a hook func may be too late
	g_hook_list_init (&rt->hook_change_dir, sizeof (GHook));
	g_hook_list_init (&rt->view.hook_refresh, sizeof (GHook));

#ifdef E2_VFSTMP
	//CHECKME path when non-mounted dir cached at session end
#endif
	e2_cache_str_register (rt->name, &rt->path, G_DIR_SEPARATOR_S);
	//do we need to over-ride the cached startup dir by a runtime option ?
	gchar *startdir = (num == 1) ? e2_cl_options.pane1_path :
		e2_cl_options.pane2_path ;
	if (startdir != NULL)
		startdir = D_FILENAME_FROM_LOCALE (startdir);	//don't worry about trailer
#ifdef E2_VFSTMP
	//CHECKME startup dir always local? set other paths accordingly
#endif
	else //or by a config option ?
	{
		gchar *option_name = g_strconcat (rt->name,"-use-startup-dir", NULL);
		if (e2_option_bool_get (option_name))
		{
			g_free (option_name);
			option_name =  g_strconcat (rt->name,"-use-startup-dir-startup-dir", NULL);
			startdir = e2_option_str_get (option_name);
			if (startdir != NULL)
				startdir = g_strdup (startdir);
//				startdir = F_FILENAME_FROM_LOCALE (startdir);
		}
		else
			startdir = NULL;
		g_free (option_name);
	}
	if (startdir != NULL)
	{
		g_free (rt->path);
		rt->path = startdir;
	}

	if (e2_option_bool_get ("cache-history"))
	{
		gchar *history_name = g_strconcat (rt->name, "-history", NULL);
		e2_cache_list_register (history_name, &rt->opendirs);
		g_free (history_name);
		if (rt->opendirs != NULL)
		{
			//populate hash with data from cache list
			GList *node;
			for (node = rt->opendirs; node != NULL; node = node->next)
			{
				gchar *dirpath = (gchar *)node->data;
				E2_DirHistoryEntry *hist_entry = g_hash_table_lookup (app.dir_history, dirpath);
				if (hist_entry == NULL)
				{//need a new entry for the history cache
					hist_entry = ALLOCATE0 (E2_DirHistoryEntry);
					CHECKALLOCATEDWARNT (hist_entry, );
					if (hist_entry != NULL)
					{
						g_strlcpy (hist_entry->path, dirpath, sizeof(hist_entry->path));
						hist_entry->selrow = -1;
#ifdef E2_VFSTMP
						hist_entry->spacedata = view->spacedata;
#endif
						g_hash_table_insert (app.dir_history, dirpath, hist_entry);
						node->data = hist_entry; //now safe to substitute shared data
					}
				}
				else
				{
					g_free (dirpath); //clean redundant standalone data
					node->data = hist_entry; //substitute shared data
				}
			}
		}
		history_name = g_strconcat (rt->name, "-current", NULL);
		e2_cache_int_register (history_name, (gint*)&rt->opendir_cur, 0);
		g_free (history_name);
		if (rt->opendirs == NULL)
			rt->opendir_cur = 0;
		else if (startdir == NULL)
		{
			g_free (rt->path);
			guint len = g_list_length (rt->opendirs);
			rt->path = g_strdup (g_list_nth_data (rt->opendirs, len - 1 - rt->opendir_cur));
		}
	}

	//register pane-specific actions
	E2_Action actions[] =
	{
	//non-NULL data signals a dirline instead of command line, and _is_ used
	{NULL,(gboolean(*)(gpointer,E2_ActionRuntime*))NULL,
									  FALSE,E2_ACTION_TYPE_COMMAND_LINE,
				  E2_ACTION_EXCLUDE_MENU | E2_ACTION_EXCLUDE_ACCEL, rt, NULL},
 	{NULL,_e2_pane_toggle_hidden,     FALSE,E2_ACTION_TYPE_ITEM, 0, rt, NULL},
 	{NULL,_e2_pane_filter_menu_create,FALSE,E2_ACTION_TYPE_ITEM, 0, rt, NULL},
#ifdef E2_VFS
 	{NULL,_e2_pane_vfs_menu_show,     FALSE,E2_ACTION_TYPE_ITEM, 0, rt, NULL},
#endif
	};
	//all name strings freed during action-cleanup
	actions[0].name = g_strconcat ((num == 1) ? _A(11) : _A(12),".",_A(27), NULL);
	actions[1].name = g_strdup ((num == 1) ?
		toggles_array [E2_TOGGLE_PANE1HIDDEN] : toggles_array [E2_TOGGLE_PANE2HIDDEN]);
	actions[2].name = g_strdup ((num == 1) ?
		toggles_array [E2_TOGGLE_PANE1FILTERS] : toggles_array [E2_TOGGLE_PANE2FILTERS]);
#ifdef E2_VFS
	actions[3].name = g_strdup ((num == 1) ?
		toggles_array [E2_TOGGLE_PANE1SPACE] : toggles_array [E2_TOGGLE_PANE2SPACE]);
#endif
	guint i, count = sizeof (actions)/sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&actions[i]);

	//after the actions are in place, fill in the pane contents
	e2_pane_create_part (rt);
}
/**
@brief part of pane creation, used also in re-creation

@param rt pointer to pane data struct

@return
*/
void e2_pane_create_part (E2_PaneRuntime *rt)
{
	//existing hooklists are cleared if appropriate in e2_window_recreate()
	rt->pane_sw = e2_fileview_create_list (&rt->view);
#ifdef USE_GTK3_0
	rt->inner_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
	rt->outer_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	rt->inner_box = gtk_vbox_new (FALSE, 0);
	rt->outer_box = gtk_hbox_new (FALSE, 0);
#endif
	gtk_box_pack_start (GTK_BOX (rt->inner_box), rt->pane_sw, TRUE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (rt->outer_box), rt->inner_box, TRUE, TRUE, 0);

	gtk_widget_show (rt->inner_box);
	gtk_widget_show (rt->outer_box);

	e2_toolbar_create (&rt->toolbar);

	if (e2_option_bool_get_direct (rt->toolbar.show))
	{
		//show full/split window button according to cached pane ratio
		if (rt == &app.pane1 && app.window.panes_paned_ratio > 0.999)
			e2_toolbar_toggle_button_set_state
				(toggles_array [E2_TOGGLE_PANE1FULL], TRUE);
		else if (rt == &app.pane2 && app.window.panes_paned_ratio < 0.001)
			e2_toolbar_toggle_button_set_state
				(toggles_array [E2_TOGGLE_PANE2FULL], TRUE);
		//show (un)hide toggle buttons which match corresponding cached state
		E2_ToggleType num = (rt == &app.pane1) ?
			E2_TOGGLE_PANE1HIDDEN : E2_TOGGLE_PANE2HIDDEN;
		e2_toolbar_toggle_button_set_state (toggles_array [num], rt->view.show_hidden);
		num = (rt == &app.pane1) ?
			E2_TOGGLE_PANE1FILTERS : E2_TOGGLE_PANE2FILTERS;
		gboolean filtered =
			(  rt->view.name_filter.active
			|| rt->view.size_filter.active
			|| rt->view.date_filter.active);
		//this needs to be inverted to show correct button
		e2_toolbar_toggle_button_set_state (toggles_array [num], !filtered);
		//set initial filter tooltip
		if (filtered)
			e2_toolbar_toggle_filter_button (&rt->view);
#ifdef E2_VFSTMP
		//FIXME
		gchar *tip = "faketip";
		e2_toolbar_update_vfs_toggle_button (rt->view, tip);
#endif
	}
}
/* *
@brief destroy pane data

@param rt pointer to pane data struct

@return
*/
/*static void _e2_pane_destroy (E2_PaneRuntime *rt)
{
	gchar *history_name = g_strconcat (rt->name, "-history", NULL);
	e2_cache_unregister (history_name);
	e2_list_free_with_data (&<GList used for history_name>);
	g_free (history_name);
	//pane toolbar may be outside rt->outer_box, so destroy it independently
	gtk_widget_destroy (rt->toolbar.toolbar_container);
//	gtk_widget_destroy (rt->pane_sw);
	gtk_widget_destroy (rt->outer_box);
	g_hook_list_clear (&rt->hook_change_dir);
	g_hook_list_clear (&rt->view->hook_refresh);
}*/
/* *
@brief destroy and re-create specified pane

@param rt pointer to pane data struct

@return
*/
/*void e2_pane_recreate (E2_PaneRuntime *rt)
{
	_e2_pane_destroy (rt);
	e2_pane_create_part (rt);
	e2_toolbar_rebadge (rt->toolbar);
}*/
/**
@brief limit the size of the history list to be cached for pane related to @a rt
The extent is limited to current-dir +/- 5 items max.
@param rt pointer to pane data struct
@return
*/
void e2_pane_trim_history (E2_PaneRuntime *rt)
{
	if (rt->opendirs != NULL)
	{
		GList *node;
		//process any excess 'forward' items at start of history-list
		guint len = g_list_length (rt->opendirs);
		gint limit = (gint)rt->opendir_cur + 5;
		if (limit < (gint) len - 1) //allow for 0-base
		{
			//split the list before the limiting item
			node = g_list_nth (rt->opendirs, len - 1 - limit);
			node->prev->next = NULL;
			node->prev = NULL;
			//data in the list owned by app.dir_history, don't free it separately
			g_list_free (rt->opendirs);
			rt->opendirs = node;
			len = limit + 1;
		}
		//now any excess 'backward' items at end of history-list
		limit = rt->opendir_cur - 5;
		if (limit > 0)
		{
			//split after limiting item
			node = g_list_nth (rt->opendirs, len - limit);
			node->prev->next = NULL;
			node->prev = NULL;
			g_list_free (node);
			rt->opendir_cur = rt->opendir_cur - limit;
		}
	}
}
/**
@brief register pane-related actions that don't have a pane-specifc action

Actions with pane-specific function are registered in e2_pane_create()

@return
*/
void e2_pane_actions_register (void)
{
	E2_Callback *hdata1 = MALLOCATE (E2_Callback);
	CHECKALLOCATEDWARN (hdata1, );
	hdata1->callback = _e2_pane_backward_hover;
	hdata1->data = NULL;
	E2_Callback *hdata2 = MALLOCATE (E2_Callback);
	CHECKALLOCATEDWARN (hdata2, );
	hdata2->callback = _e2_pane_forward_hover;
	hdata2->data = NULL;

	//all name-strings must be freeable
	E2_Action actions[] =
	{
	{g_strconcat(_A(14),".",_A(76),NULL),_e2_pane_refresh_lists,FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
	{g_strconcat(_A(14),".",_A(77),NULL),e2_filelist_enable_refresh_action,FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
	{g_strconcat(_A(14),".",_A(78),NULL),e2_filelist_disable_refresh_action,FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
	{g_strconcat(_A(14),".",_A(98),NULL),_e2_pane_activate_other_action,FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL},
	{g_strconcat(_A(14),".",_A(101),NULL),_e2_pane_sync,         FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(13),".",_A(25),NULL),_e2_pane_filters_show, TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	//CHECKME name
	//c.f. panes.switch doesn't choose a specific pane, pane.focus doesn't change active pane
	{g_strconcat(_A(13),".",_A(30), NULL),_e2_pane_activate,    TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(13),".",_A(50),NULL),_e2_pane_focus,        TRUE, E2_ACTION_TYPE_ITEM,
		E2_ACTION_EXCLUDE_MENU | E2_ACTION_EXCLUDE_TOOLBAR, //CHECKME exclusions
		NULL, NULL},
	{g_strconcat(_A(8),".",_A(28),NULL), NULL,                  TRUE, E2_ACTION_TYPE_HISTORY, E2_ACTION_EXCLUDE_TOGGLE, NULL, NULL},
	{g_strconcat(_A(13),".",_A(37),NULL),_e2_pane_clear_goto_history, TRUE, E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_TOGGLE, NULL, NULL},
	{g_strconcat(_A(13),".",_A(52),NULL),_e2_pane_go_back,      TRUE, E2_ACTION_TYPE_HOVER, E2_ACTION_EXCLUDE_TOGGLE, hdata1, NULL},
	{g_strconcat(_A(13),".",_A(53),NULL),_e2_pane_go_forward,   TRUE, E2_ACTION_TYPE_HOVER, E2_ACTION_EXCLUDE_TOGGLE, hdata2, NULL},
	{g_strconcat(_A(13),".",_A(54),NULL),_e2_pane_go_up,        TRUE, E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_TOGGLE, NULL, NULL},
	{g_strconcat(_A(13),".",_A(62),NULL),_e2_pane_mirror,       TRUE, E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_TOGGLE, NULL, NULL},
	{g_strconcat(_A(13),".",_A(67),NULL),_e2_pane_goto_choice,  TRUE, E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_TOGGLE, NULL, NULL},
	{g_strconcat(_A(13),".",_A(35),NULL),_e2_pane_goto,         TRUE, E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_TOGGLE, NULL, NULL},
	{g_strconcat(_A(18),".",_A(67),NULL),_e2_pane_goto_trash,   TRUE, E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_TOGGLE, NULL, NULL},
	//CHECKME ok with toggles in pane 1 & 2 ?
	{g_strconcat(_A(13),".",_A(87),NULL),_e2_pane_toggle_hidden,TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(13),".",_A(88),NULL),e2_context_menu_show_menu_action, TRUE,
		E2_ACTION_TYPE_ITEM, E2_ACTION_EXCLUDE_MENU | E2_ACTION_EXCLUDE_TOOLBAR,
		NULL, NULL},
#ifdef E2_TREEDIALOG
	{g_strconcat(_A(13),".",_A(106),NULL),e2_tree_dialog_show_action,TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
#endif
#ifdef E2_VFS
	{g_strconcat(_A(13),".",_A(125),NULL),_e2_pane_select_space,  TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
#endif
	{g_strconcat(_A(5),".",_A(37),NULL),_e2_pane_clearall_dirline_history, TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(5),".",_A(32),NULL),_e2_pane_add1_dirline_history, TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(5),".",_A(44),NULL),_e2_pane_clear1_dirline_history, TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},

	{g_strconcat(_A(7),".",_A(55),NULL),_e2_pane_focus_bottom,    TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(7),".",_A(56),NULL),_e2_pane_focus_top,       TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(7),".",_A(60),NULL),_e2_pane_invert_all,      TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(7),".",_A(104),NULL),_e2_pane_select_all,     TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(7),".",_A(105),NULL),_e2_pane_invert_current, TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(7),".",_A(83),NULL),_e2_pane_select_extension,TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	//CHECKME which exclusions for these sort-actions ?
	{g_strconcat(_A(7),".",_A(94),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (FILENAME), NULL},
	{g_strconcat(_A(7),".",_A(91),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (EXTENSION), NULL},
	{g_strconcat(_A(7),".",_A(96),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (SIZE), NULL},
	{g_strconcat(_A(7),".",_A(95),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (PERM), NULL},
	{g_strconcat(_A(7),".",_A(97),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (OWNER), NULL},
	{g_strconcat(_A(7),".",_A(92),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (GROUP), NULL},
	{g_strconcat(_A(7),".",_A(93),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (MODIFIED), NULL},
	{g_strconcat(_A(7),".",_A(89),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (ACCESSED), NULL},
	{g_strconcat(_A(7),".",_A(90),NULL),_e2_pane_sort,TRUE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (CHANGED), NULL},
	};

	guint i, count = sizeof (actions)/sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&actions[i]);
}
/**
@brief register pane-related config options
Panebar default contents are set here, too
@param num integer pane number (1 or 2)

@return
*/
void e2_pane_options_register (gint num)
{
	gint i; gchar *option_name, *group_name, *desc, *tip, *grey_me;
	E2_OptionFlags flags;
	//position these here to get paneN before paneN bar

	option_name = g_strdup_printf ("pane%d-use-startup-dir", num);
	group_name = g_strconcat ((num == 1) ? _C(29) :_C(31), ":", _C(38), NULL);    // _("paneN:startup"
	e2_option_bool_register (option_name, group_name,
		_("At startup, show a specific directory in this pane"),
		_("This causes the directory named below, instead of the last-opened directory, to be shown at session start"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREENAME | E2_OPTION_FLAG_FREEGROUP);
	gchar *dep = option_name;
	option_name = g_strconcat (option_name,"-startup-dir", NULL);
	desc = g_strdup_printf (_("pane %d startup directory:"), num);
	e2_option_str_register (option_name, group_name,
		desc,  _("This is the directory to show in this pane, at session start"),
		dep, (num == 1) ? "~" : G_DIR_SEPARATOR_S,   //pane1 default = home, pane2 default = root directory
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_FREEDESC);

	group_name = g_strconcat ((num == 1) ? _C(29) : _C(31), ":", _C(26), NULL);    // _("paneN:misc"
	if (num == 1)
	{
		e2_option_bool_register ("pane1-uses-other", group_name, _("this pane is like pane 2"),
			g_strdup_printf (
			_("This makes pane %d options (other than toolbar placement and content) apply to pane %d"),
			2,1),
			"!pane2-uses-other", FALSE,
			E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREEGROUP
			| E2_OPTION_FLAG_FREETIP | E2_OPTION_FLAG_BUILDPANES);
	}
	else
	{
		e2_option_bool_register ("pane2-uses-other", group_name, _("this pane is like pane 1"),
			g_strdup_printf (
			_("This makes pane %d options (other than toolbar placement and content) apply to pane %d"),
			1,2),
			"!pane1-uses-other", FALSE,
			E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREEGROUP
			| E2_OPTION_FLAG_FREETIP | E2_OPTION_FLAG_BUILDPANES);
	}

	group_name = g_strconcat ((num == 1) ? _C(29) : _C(31), ":", _C(3), NULL);    // _("paneN:columns"
	for (i = 0; i < MAX_COLUMNS; i++)
	{
		option_name = g_strdup_printf ("pane%d-show-column%d", num, i);
		desc = g_strdup_printf (_("show %s column"), gettext(e2_all_columns[i].title));
		tip = g_strdup_printf (_("This causes the the named column to be displayed in pane %d (duh ...)"), num);
		flags = E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREENAME | E2_OPTION_FLAG_FREEDESC
			| E2_OPTION_FLAG_FREETIP | E2_OPTION_FLAG_BUILDPANES;
		if (i > 0)
			grey_me = (num == 1) ? "!pane1-uses-other" : "!pane2-uses-other";
		else
		{
			flags |= E2_OPTION_FLAG_FREEGROUP;
			//disable hiding of col 0 = filename
			grey_me = g_strconcat ("!",option_name,NULL); //a 'depends' entry = !<self> disables any change
		}
		e2_option_bool_register (option_name, group_name, desc, tip, grey_me,
		TRUE, flags);
	}
	//this needs to be after at least one non-tree option, so that the
	//first-registered option is non-tree and dialog scrolling is setup properly
	e2_toolbar_panebar_register (num);	//setup the pane bar

	if (num == 1)
		e2_toolbar_options_register (E2_BAR_PANE1);  //set general toolbar options
	else
		e2_toolbar_options_register (E2_BAR_PANE2);
}
