/* $Id: e2_action.c 2915 2013-11-13 01:08:10Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/actions/e2_action.c
@brief action system

This file contains functions for the action system.
*/
/**
\page actions the action system

ToDo - description of how actions work

\section file_operations operations on selected items

ToDo
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_action.h"
#include "e2_action_option.h"
#include "e2_task.h"
#include "e2_filelist.h"
#include "e2_plugins.h"
#include "e2_filetype.h"
#include "e2_dialog.h"
#include "e2_mkdir_dialog.h"
#include "e2_complete.h"

//static
GHashTable *actions_hash;
//static GPtrArray *actions_array;
GtkTreeStore *actions_store;	//for action-names in config dialog combo renderers

  /*******************/
 /***** actions *****/
/*******************/

/**
@brief process a toggle-button click

This expects as data the key of the relevant toggle data struct in the toggles hash
Expects BGL closed.

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_action_do_toggle (gpointer from, E2_ActionRuntime *art)
{
	gboolean newstate, retval;
	gchar *hash_name = (gchar *) art->data;
	E2_ToggleData *ex = g_hash_table_lookup (toggles_hash, hash_name);
	//check if action name is in the array of "self-managed" toggles
	gint i;
	for (i = 0; i < E2_TOGGLE_COUNT; i++)
	{
		if (!strcmp (toggles_array [i], ex->true_action)
			&& !strcmp (ex->true_action, ex->false_action))
				break;
	}
	if (i < E2_TOGGLE_COUNT)
		//this is a self-mangaged toggle
		newstate = !ex->current_state;
	else
		newstate = e2_toolbar_button_toggle_custom (hash_name);
	//get the operation corresponding to the un-toggled state
	gchar *cc = g_strdup ((newstate) ? ex->false_action : ex->true_action);
	if (e2_action_check (cc) != NULL)
	{
		gchar *gap = e2_utils_find_whitespace (cc);
		if (gap != NULL)
		{
			*gap = '\0';
			gap = e2_utils_pass_whitespace (gap+1);
		}
		OPENBGL
		retval = e2_action_run_simple_from (cc, gap, from);
		CLOSEBGL
	}
	else
	{
		gint res = e2_command_run (cc, E2_COMMAND_RANGE_DEFAULT, from
#ifdef E2_COMMANDQ
		, TRUE
#endif
		);
		retval = (res == 0);
	}
	g_free (cc);
	return retval;
}
/**
@brief process a constructed custom command
This expects data string of the form "<custom command> realcmd [realargs]"
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if command returned 0
*/
static gboolean _e2_action_custom_command (gpointer from, E2_ActionRuntime *art)
{
	gchar *command = g_strdup ((gchar *)art->data);
	gint res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, from
#ifdef E2_COMMANDQ
	, TRUE
#endif
	);
	g_free (command);
	return (res == 0);
}
/**
@brief initiate configuration change

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_action_configure (gpointer from, E2_ActionRuntime *art)
{  //put this in config dialog file ?
//	gtk_widget_set_sensitive (app.main_window, FALSE);
	gchar *page = (gchar *)art->data;
	e2_config_dialog_create (page);
//	gtk_widget_set_sensitive (app.main_window, TRUE);
	return TRUE;
}
/**
@brief revert all configuration settings to defailt values

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_action_configure_default (gpointer from, E2_ActionRuntime *art)
{
	//dump old data, don't re-read the config file, recreate screen
	e2_option_refresh (FALSE, TRUE);
	return TRUE;
}
#ifdef E2_VFS
/**
@brief dummy action for plugins which do not really belong on a plugins menu

@param from the button, menu item etc which was activated
@param art action runtime data

@return FALSE always
*/
gboolean e2_action_inaction (gpointer from, E2_ActionRuntime *art)
{
	return FALSE;
}
#endif

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief actions treestore sort-function
This sorts on column 1 of the store
@param model the GtkTreeModel for the store being sorted
@param a a GtkTreeIter in @a model
@param b another GtkTreeIter in @a model
@param userdata UNUSED data specified when the compare func was assigned

@return <0, 0, >0 if a sorts before b, with b, or after b, respectively
*/
static gint _e2_action_tree_compare_cb (GtkTreeModel *model,
	GtkTreeIter *a, GtkTreeIter *b, gpointer userdata)
{
	gint ret;
	gchar *name1, *name2;
	gtk_tree_model_get (model, a, 1, &name1, -1);
	gtk_tree_model_get (model, b, 1, &name2, -1);
	if (name1 == NULL || name2 == NULL)
	{
		if (name1 == NULL && name2 == NULL)
			return 0;
		if (name1 == NULL)
		{
			g_free (name2);
			return -1;
		}
		else
		{
			g_free (name1);
			return 1;
		}
	}
	else
	{
		ret = g_utf8_collate (name1,name2);
		g_free(name1);
		g_free(name2);
	}
	return ret;
}
/**
@brief actions treestore visibility-function
This determines which actions are visible in a filter model for the store,
based on data stored in column 2
@param model the filtermodel for the actions store
@param iter pointer to data for row in @a model to be evaluated
@param data pointerised flags specified when @a model was created

@return TRUE if the row is visible
*/
static gboolean _e2_action_visible_cb (GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
	gboolean retval;
	E2_ACTION_EXCLUDE ex1 = GPOINTER_TO_INT (data);
	E2_ACTION_EXCLUDE ex2;
	gchar *name;

	gtk_tree_model_get (model, iter, 0, &name, 2, &ex2, -1);
	retval = !(ex1 & ex2 & 0xffff);
	//special cases
	if ((ex1 & E2_ACTION_INCLUDE_FILES) && name != NULL)
		retval = (retval && g_str_has_prefix (name, _A(6)));
	g_free (name);
//	g_free (child);
	return retval;
}
static GtkWidget *selitem = NULL;
/**
@brief "button-press-event" signal callback for a hover-menu widget
@param item the menu-item where the button was pressed
@param event button-event data
@param data pointer to data struct for the hover action

@return	TRUE if the event was processed
*/
static gboolean _e2_action_hover_buttonpress_cb (GtkWidget *item,
	GdkEventButton *event, E2HoverData *data)
{
	if (event->button == 1 && event->type == GDK_BUTTON_PRESS)
	{
		NEEDCLOSEBGL
		gtk_menu_shell_activate_item (GTK_MENU_SHELL (data->menu), item, TRUE);
#ifdef USE_GTK3_0
		gtk_widget_destroy (data->menu);
#else
		//data->menu will have been destroyed as consequence of activation
#endif
		NEEDOPENBGL
		data->menu = NULL;
		selitem = NULL;
		return TRUE;
	}
	return FALSE;
}
/**
@brief foreach func to apply button-press callback to each item in a hover menu
@param menuitem item in hover menu
@param data pointer to data struct for the hover action
@return
*/
static void _e2_action_hover_setup (GtkWidget *menuitem, E2HoverData *data)
{
	g_signal_connect (G_OBJECT (menuitem), "button-press-event",
		G_CALLBACK (_e2_action_hover_buttonpress_cb), data);
}
/**
@brief "motion-notify" event signal for entered history menu
@param window UNUSED the parent-window-widget of the hover menu
@param event motion-event data
@param data pointer to data struct for the hover action
@return FALSE always
*/
static gboolean _e2_action_hover_pointermove_cb (GtkWidget *window,
	GdkEventMotion *event, E2HoverData *data)
{
	NEEDCLOSEBGL
	GtkWidget *menuitem = gtk_get_event_widget ((GdkEvent*) event);
	if (selitem != menuitem)
	{
		//show selected item
#ifdef USE_GTK3_0
		//without activating the item on gtk3
		if (selitem != NULL)
			gtk_menu_item_deselect (GTK_MENU_ITEM(selitem));
		gtk_menu_item_select (GTK_MENU_ITEM(menuitem));
#endif
		selitem = menuitem;
#ifndef USE_GTK3_0
		gtk_menu_shell_select_item (GTK_MENU_SHELL (data->menu), menuitem);
#endif
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief "enter-notify-event" signal callback for a hover-menu parent-window widget
Kills any hover-timer, and sets up for menu selection
@param window the parent-window-widget of the hover menu
@param event crossing-event data
@param data pointer to data struct for the hover action

@return	FALSE always so the event is propagated
*/
static gboolean _e2_action_enter_menu_cb (GtkWidget *window, GdkEventCrossing *event,
	E2HoverData *data)
{
	//kill timer initiated when button was exited, prevent the popup menu from closing
	if (data->timer_id > 0)
	{
		g_source_remove (data->timer_id);
		data->timer_id = 0;
	}
	NEEDCLOSEBGL
#ifdef USE_GTK2_18
	if (G_LIKELY(!gtk_widget_has_grab (window)))
#else
	if (G_LIKELY(!GTK_WIDGET_HAS_GRAB (window)))
#endif
		gtk_grab_add (window); //apply keyboard and mouse focus
	NEEDOPENBGL
	g_signal_connect (G_OBJECT (window), "motion-notify-event",
		G_CALLBACK (_e2_action_hover_pointermove_cb), data);

	return FALSE;
}
/**
@brief "leave-notify-event" signal callback for a hover-menu parent-window widget
@param window the parent-window-widget of the hover menu
@param event crossing-event data
@param data pointer to data struct for the hover action

@return	FALSE always so the event is propagated
*/
static gboolean _e2_action_leave_menu_cb (GtkWidget *window, GdkEventCrossing *event,
	E2HoverData *data)
{
	if (data->timer_id > 0)
	{
		g_source_remove (data->timer_id);
		data->timer_id = 0;
	}
	printd (DEBUG, "In _e2_action_leave_menu_cb()");
	NEEDCLOSEBGL
#ifdef USE_GTK2_18
	if (G_LIKELY (gtk_widget_has_grab (window)))
#else
	if (G_LIKELY (GTK_WIDGET_HAS_GRAB (window)))
#endif
		gtk_grab_remove (window);
	gtk_widget_destroy (window);
	NEEDOPENBGL
	data->menu = NULL;
	selitem = NULL;
	return FALSE;
}
/**
@brief timer callback to implement a hover menu
@brief data pointer to hover data
@return FALSE to cancel the timer
*/
static gboolean _e2_action_do_hover_timeout (E2HoverData *data)
{
	//get rid of this ASAP in case of pending departure from hover-widget
	if (data->timer_id > 0)
	{
		g_source_remove (data->timer_id);
		data->timer_id = 0;
	}

	(*data->callback) (data->hovered, data); //e.g. _e2_pane_forward_hover
	if (GTK_IS_MENU (data->menu))
	{
		GtkWidget *popwin = gtk_widget_get_ancestor (data->menu, GTK_TYPE_WINDOW); //menu's parent, actually
		if (popwin != NULL)
		{
			gtk_window_set_transient_for (GTK_WINDOW (popwin), GTK_WINDOW (app.main_window));
			//setup to handle button-presses too
			gtk_container_foreach (GTK_CONTAINER(data->menu),
				(GtkCallback)_e2_action_hover_setup, data);
			g_signal_connect (G_OBJECT (popwin), "enter-notify-event",
				G_CALLBACK (_e2_action_enter_menu_cb), data);
			g_signal_connect (G_OBJECT (popwin), "leave-notify-event",
				G_CALLBACK (_e2_action_leave_menu_cb), data);
#ifdef USE_GTK2_18
			if (!gtk_widget_get_visible (data->menu))
#else
			if (!GTK_WIDGET_VISIBLE (data->menu))
#endif
			{
				gtk_widget_show (data->menu); //setup size for use when positioning ?
				gint x, y;
				gboolean push = TRUE;
				CLOSEBGL
				e2_toolbar_set_menu_position (GTK_MENU (data->menu), &x, &y, &push, data->hovered);
				gtk_window_move (GTK_WINDOW (popwin), x, y); //this might be ignored by WM ?
				gtk_widget_show_all (popwin);
				OPENBGL
			}
		}
		else
			printd (DEBUG, "No parent window for history menu");
	}
	return FALSE;
}
/**
@brief timer callback to cleanup after a hover
@brief data pointer to hover data
@return FALSE to cancel the timer
*/
static gboolean _e2_action_clear_hover_cb (E2HoverData *data)
{
	if (GTK_IS_WIDGET (data->menu))
	{
		CLOSEBGL
		GtkWidget *popwin = gtk_widget_get_toplevel (data->menu);
		gtk_widget_destroy (popwin);
		OPENBGL
	}
	data->menu = NULL;

	return FALSE;
}
/**
@brief "clicked" signal callback on a button which supports a hover-menu
If the button is clicked, we should respond to that, and abort any hover activity
@param button the clicked item
@param data hover data struct
@return
*/
static void _e2_action_abort_hover_cb (GtkButton *button, E2HoverData *data)
{
//	printd (DEBUG, "in function _e2_action_abort_hover_cb");
	if (data->timer_id > 0)
	{
		g_source_remove (data->timer_id);
		data->timer_id = 0;
	}

	NEEDCLOSEBGL
	if (GTK_IS_WIDGET (data->menu))
	{
		GtkWidget *popwin = gtk_widget_get_toplevel (data->menu);
		gtk_widget_destroy (popwin);
	}
	NEEDOPENBGL
	data->menu = NULL;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief setup for a potential hover process
@param widget the entered widget (for now at least, only a toolbar button)
@param event pointer to event data
@para data pointer to hover data struct

This is a "enter-notify-event" signal callback for all toolbar buttons based on
a E2_ACTION_TYPE_HOVER action.

@return	FALSE always so the event is propagated
*/
gboolean e2_action_start_hover_cb (GtkWidget *button, GdkEventCrossing *event, E2HoverData *data)
{
//	printd (DEBUG, "in function e2_action_start_hover_cb");
	if (data->timer_id > 0)
	{
		//we're re-entering the buttton from a menu
		g_source_remove (data->timer_id);
		data->timer_id = 0;
	}
//	NEEDCLOSEBGL
	if (data->menu == NULL)
	{
		//no popup if button is clicked
		g_signal_connect (G_OBJECT (button), "clicked", //not "button_press-event", that's blocked
			G_CALLBACK (_e2_action_abort_hover_cb), data);

		gint delay = 1000;
		GtkSettings* defs = gtk_settings_get_default ();
        g_object_get (G_OBJECT (defs), "gtk-menu-popup-delay", &delay, NULL);
		data->timer_id = g_timeout_add (delay, (GSourceFunc)_e2_action_do_hover_timeout, data);
	}
//	NEEDOPENBGL
	return FALSE;
}
/**
@brief abort an actual or potential hover process
@param widget the departed widget
@param event pointer to event data
@para data pointer to hover data struct

This is a "leave-notify-event" signal callback for all toolbar buttons based on
a E2_ACTION_TYPE_HOVER action, and for a popped-up menu widget.

@return	FALSE always so the event is propagated
*/
gboolean e2_action_end_hover_cb (GtkWidget *widget, GdkEventCrossing *event, E2HoverData *data)
{
//	printd (DEBUG, "in function e2_action_end_hover_cb");
	if (data->timer_id > 0)
	{
		g_source_remove (data->timer_id);
		data->timer_id = 0;
	}
//	NEEDCLOSEBGL
	if (GTK_IS_WIDGET (data->menu))
	{
		//allow short interval to move pointer from button into the menu or vice-versa, before killing it
		gint delay = 1000;
		GtkSettings* defs = gtk_settings_get_default ();
        g_object_get (G_OBJECT (defs), "gtk-menu-popdown-delay", &delay, NULL);
		if (delay < 1000)
			delay += 500;
		data->timer_id = g_timeout_add (delay, (GSourceFunc)_e2_action_clear_hover_cb, data);
	}
	else
		data->menu = NULL;

//	NEEDOPENBGL
	g_signal_handlers_disconnect_by_func ((gpointer)widget, _e2_action_abort_hover_cb, data);

	return FALSE;
}

/**
@brief option helper for action columns

This function is used to source a model for cell renderers in columns of type
E2_OPTION_TREE_TYPE_SEL in tree-option config-dialog treeviews

@param ex pointerised flags which specify categories of actions
		that should not be visible

@return GtkTreeFilterModel of all actions
*/
GtkTreeModel *e2_action_filter_store (gpointer ex)
{
	GtkTreeModel *filter = gtk_tree_model_filter_new (
		GTK_TREE_MODEL (actions_store), NULL);
	gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER (filter),
		(GtkTreeModelFilterVisibleFunc) _e2_action_visible_cb, ex, NULL);
	return filter;
}

gint entry_level = 0;

/**
@brief register 'public' action names, and corresponding parameters, for later use as needed.
It replaces any previously-registered action with the same name.
Also adds the action name etc to the options store, as a convenience for the option system
Action names need to be freed (= action removed from the hash) when the session ends, if not before.
Potentially re-entrant.
As another convenience, the non-constant name string used to register the action is freed at exit.
@param newaction pointer to action data to be copied and heaped

@return registered action E2_Action
*/
E2_Action *e2_action_register (const E2_Action *newaction)
{
	//unregister any previously registered actions with that name
	//(for convenience)

	entry_level++; //keep note of whether re-entrant

	//now allocate the action structure and initialize it from the
	//function parameters
#ifdef USE_GLIB2_10
	E2_Action *action = (E2_Action *) g_slice_alloc (sizeof (E2_Action));
//	E2_Action *action = ALLOCATE (E2_Action);
#else
	E2_Action *action = ALLOCATE (E2_Action);
#endif
	CHECKALLOCATEDFATAL (action);
	//action name is freed if the action is removed from the actions_hash table
	//because the names are used as keys
	*action = *newaction;
	//add the action to the static hash table and pointer array register
	g_hash_table_replace (actions_hash, action->name, action);
//	g_ptr_array_add (actions_array, action);

	//to get structured dropdown lists, find and register the 'parent' action, if any
	gboolean registered_parent = FALSE;
	E2_Action *parent = NULL;
	gchar *search = g_strdup (newaction->name);
	gchar *child = strrchr (search, '.');	//always ascii '.', don't need g_utf8_strrchr()
	if (child != NULL)
	{ //also register a new parent, if need be
		*child = '\0';
		parent = e2_action_get (search);
		if (parent == NULL)
		{
			E2_Action found =
			{search, NULL, FALSE, E2_ACTION_TYPE_DUMMY, 0, NULL, NULL};
			//re-entry !!
			parent = e2_action_register (&found);
			registered_parent = TRUE;
		}
	}

	//now add the action to the treestore
	GtkTreeIter iter;
	GtkTreeIter iter_parent;
	GtkTreeIter *parentptr;
	//setup store
	//(sortable, with filter-model for selection-type cell renderers in config dialog trees)
	if (parent == NULL)
		parentptr = NULL;
	else
	{
		parentptr = &iter_parent;
		if (!e2_tree_ref_to_iter (actions_store, parent->ref, parentptr))
			parentptr = NULL;
	}
	//CHECKME only columns 0 and 2 are used ?
#ifdef USE_GTK2_10
	gtk_tree_store_insert_with_values (actions_store, &iter, parentptr, -1,
#else
	gtk_tree_store_append (actions_store, &iter, parentptr);
	gtk_tree_store_set (actions_store, &iter,
#endif
		0, action->name, 1, (child == NULL) ? action->name : g_strstrip (child + 1),
		2, action->exclude, 3, action->type != E2_ACTION_TYPE_DUMMY, 4, TRUE, -1);
	//keep reference around to be able to to find out the parent iter
	//for possible children later on
	action->ref = e2_tree_iter_to_ref (actions_store, &iter);

	//cleanup
	if (!registered_parent)
		g_free (search);  //registered actions are kept, but this one was not needed
//	else if (child != NULL)
//		*child = '.';	//what about ':' ?
//	if (--entry_level == 0)  //revert re-entrant level, for next time
//		g_free (name);  //and this is not re-entrant, so free the original name-space
	entry_level--;
	return action;
}
/**
@brief unregister action named @a name
This is for plugin actions, essentially
@param name action name string

@return TRUE if the action was registered
*/
gboolean e2_action_unregister (const gchar *name)
{
	E2_Action *action = g_hash_table_lookup (actions_hash, name);
	if (action == NULL)
		return FALSE;

	GtkTreeIter iter, parent;
	if (e2_tree_ref_to_iter (actions_store, action->ref, &iter)
		&& gtk_tree_model_iter_parent (GTK_TREE_MODEL (actions_store),
			&parent, &iter)
		&& gtk_tree_model_iter_n_children (GTK_TREE_MODEL (actions_store),
			&parent) == 1)
		gtk_tree_store_remove (actions_store, &parent);

	g_hash_table_remove (actions_hash, name);
	return TRUE;
}
/**
@brief find an action by name

The action with the name @a name is looked up in the internal hash table
and a pointer to the action object is returned. The name should be the
complete name of the action including any parent categories. If the action
cannot be found, NULL is returned.

@param name the name of the action

@return pointer to an action object or NULL
*/
E2_Action *e2_action_get (const gchar *name)
{
	return (g_hash_table_lookup (actions_hash, name));
}
/**
@brief find an action by name, reverting to custom if not a registered action

@param taskname action or external command string
@param arg the argument to be supplied to the action or command, or NULL
@param use_arg pointer to store newly-allocated "real" argument string
@return pointer to an action object, or NULL if @a taskname is NULL
*/
E2_Action *e2_action_get_with_custom (const gchar *taskname, const gchar *arg, gchar **use_arg)
{
	if (taskname == NULL)
		return NULL;
	E2_Action *action = e2_action_get (taskname);
	if (action == NULL)
	{	//fallback to custom command
		action = e2_action_get (_A(20));
		if (arg == NULL || *arg == '\0')
			*use_arg = g_strdup (taskname);
		else
			*use_arg = g_strconcat (taskname, " ", arg, NULL);
	}
	else
	{
		if (arg == NULL)
			*use_arg = g_strdup ("");
		else
			*use_arg = g_strdup (arg);
	}
	return action;
}
/**
@brief check whether @a command is an action

@param command the command string

@return pointer to the action, or NULL if @a command not an action
*/
E2_Action* e2_action_check (gchar *command)
{
	if (command == NULL)	//this should never happen, but ...
		return NULL;
//#warning ignore compiler warning about unitialized usage of c
	gchar c;
	gchar *gap = e2_utils_find_whitespace (command);
	if (gap != NULL)
	{
		c = *gap;
		*gap = '\0';
	}
	E2_Action *action = e2_action_get (command);
	if (gap != NULL)
		*gap = c;
	if (action == NULL)
		return NULL;
	if (action->type == E2_ACTION_TYPE_DUMMY)
		return NULL;
	return action;
}
/**
@brief run a named 'task' (may be a command or an action)

This function is to run a certain action. First, the action specified by
@a name is looked up. Then a runtime object is created and the action is run.
Afterwards the runtime object is freed again. If the action could not be found,
an error message is printed to the output pane.
Downstream expects BGL open/off.

@param name action name or command, string
@param arg pointer to data for the command, or possibly arguments or more of the command, or NULL
@param from the widget activated to initiate the action (best if non-NULL)

@return  TRUE on success, FALSE on failure
*/
gboolean e2_action_run_simple_from (const gchar *name, gpointer arg, gpointer from)
{
	//CHECKME should these variables be volatilized ?
	if ((name == NULL || *name == '\0')	//chained keybindings may have no command
		&& (arg == NULL || *(gchar *)arg == '\0'))
		return FALSE;
	gchar *real_arg;
	E2_Action *action = e2_action_get_with_custom (name, arg, &real_arg);
	E2_ActionRuntime *rt = e2_action_pack_runtime (action, real_arg, g_free);
	gboolean retval = e2_action_run (from, rt);
	e2_action_free_runtime (rt);
	return retval;
}
/* *
@brief callback to run an action

This function wraps the real action run function, with thread unlocking
so that any such locking can be managed locally, without mutux reentrance.

@param from the item (GtkButton GtkMenuItem etc) activated to trigger the action
@param rt runtime data for the action

@return
*/
/*void e2_action_run_cb (gpointer from, E2_ActionRuntime *rt)
{
//	NEEDCLOSEBGLX
	OPENBGL
	e2_action_run (from, rt);
	CLOSEBGL
}*/
/**
@brief run a certain action

This function is the real action run function.
It may be used as callback for gtk signals, if downstream thread [un]locks
are managed.
There are several wrappers to simplify access.
Runtime data @a rt may contain additional user data.
@a from may be used by the action handler to determine the context.
Expects BGL open/off.

@param from the item activated to trigger the action (eg toolbar GtkButton, maybe NULL)
@param art runtime data for the action

@return TRUE on success of the action, FALSE on failure
*/
gboolean e2_action_run (gpointer from, E2_ActionRuntime *art)
{
	//some quick checks
	if (art == NULL || art->action == NULL || art->action->func == NULL)
	{
		printd (DEBUG, "cannot run action, it doesn't exist");
		return FALSE;
	}
	printd (DEBUG, "e2_action_run (from:, art:%s)", art->action->name);
	if (from == NULL)
		from = app.main_window;
	CLOSEBGL
	art->state = e2_action_get_current_state (from);
	gboolean (*func) (gpointer, E2_ActionRuntime *) = art->action->func;
	gboolean res = func (from, art);
	OPENBGL
	printd (DEBUG,"e2_action_run () ends");
	return res;
}
/**
@brief create an action runtime object

This function creates an action runtime object and initializes it with
supplied user data.

@param action the action to create the runtime object for, or command string for a "custom command"
@param data action user data
@param data_free function to free data when the runtime object is freed

@return newly allocated action runtime object that should be freed
*/
E2_ActionRuntime *e2_action_pack_runtime (E2_Action *action, gpointer data,
	void(*data_free)(gpointer))
{
//	printd (DEBUG, "action pack runtime for %s", action->name);
	E2_ActionRuntime *art = ALLOCATE (E2_ActionRuntime);
	CHECKALLOCATEDFATAL (art);
	art->action = action;
	art->data = data;
	art->data_free = (data == NULL) ? NULL : data_free;
	art->state = 0;	//usually updated when action is run
	return art;
}
/**
@brief free action runtime object

The runtime data is freed if a data free function has been supplied.
In addition the runtime object is freed and must not be used anymore after
calling this function.

@param rt the action runtime object to free

@return
*/
void e2_action_free_runtime (E2_ActionRuntime *rt)
{
//	printd (DEBUG, "e2_action_free_runtime");
	if (rt != NULL)
	{
		if (rt->data != NULL && rt->data_free != NULL)
			(*rt->data_free) (rt->data);

		DEALLOCATE (E2_ActionRuntime, rt);
	}
}
/**
@brief determine state flags for an action about to run
@param from pointer to the widget where the action was initiated

@return tne state flags
*/
E2_ActionState e2_action_get_current_state (gpointer from)
{
	E2_ActionState state = 0;
	guint btn;
	GdkEvent *event = gtk_get_current_event ();
	if (event != NULL)
	{
		if (!gdk_event_get_state (event, &state))
#ifdef USE_GTK3_0
		{
			GtkWidget *win = gtk_widget_get_toplevel (from);
			if (gtk_widget_is_toplevel (win))
				state = e2_utils_get_savedstate (win);
			else
				state = 0;
		}
#else
			state = e2_utils_get_modifiers ();
#endif

		switch (event->type)
		{
			case GDK_BUTTON_PRESS:
  			case GDK_2BUTTON_PRESS:
  			case GDK_3BUTTON_PRESS:
  			case GDK_BUTTON_RELEASE:	//normal for a "clicked" callback
				btn = ((GdkEventButton *)event)->button;
				break;
			case GDK_KEY_PRESS:
			case GDK_KEY_RELEASE:
				btn = 0;
			default:	//CHECKME dnd
				btn = 127;
				break;
		}
		gdk_event_free (event);
	}
	else
	{
#ifdef USE_GTK3_0
		GtkWidget *win = gtk_widget_get_toplevel (from);
		if (gtk_widget_is_toplevel (win))
			state = e2_utils_get_savedstate (win);
		else
			state = 0;
#else
		state = e2_utils_get_modifiers ();
#endif
		btn = 0;
	}
	ACTION_SETSTATE (state, btn);
	return state;
}
/**
@brief clear data for an entry in the toggles hash

@param ex pointer to hash table data item

@return
*/
static void _e2_action_free_toggle (E2_ToggleData *ex)
{
	GList *member;
	for (member = ex->boxes; member != NULL; member = member->next)
		DEALLOCATE (E2_ToggleBox, member->data);
	if (ex->true_action != NULL)
		g_free (ex->true_action);
	if (ex->false_action != NULL)
		g_free (ex->false_action);
	DEALLOCATE (E2_ToggleData, ex);
}
/**
@brief internal action system memory cleanup

@return
*/
static void _e2_action_clean1 (E2_Action *action)
{
	//action->name is the actions-hash key, therefore cleared by g_free
	if (G_UNLIKELY(action->type == E2_ACTION_TYPE_HOVER))
		DEMALLOCATE (E2_Callback, action->data);
//	if (action->data != NULL && action->cleaner != (GDestroyNotify)NULL)
//		(*action->cleaner) (data);
	GtkTreeIter iter;
	if (e2_tree_ref_to_iter (actions_store, action->ref, &iter))
		gtk_tree_store_remove (actions_store, &iter);
	gtk_tree_row_reference_free (action->ref);
#ifdef USE_GLIB2_10
	g_slice_free1 (sizeof (E2_Action), action);
//	DEALLOCATE (E2_Action, action);
#else
	DEALLOCATE (E2_Action, action);
#endif
}

/**
@brief internal action system initialization

This function is only called once at startup to initialize the action
system. Afterwards, it has no effect.
It also registers most actions (and "pseudo-actions" for toolbars). Some
other actions are registered in the pane create func.

@return
*/
void e2_actions_init (void)
{
	//ensure that this function is only performed once
	RUN_ONCE_CHECK ();

	//initialize local data structures that are used to keep track of
	//the action parameters
	//note - this needs to be done before any plugin is loaded
	actions_hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
		(GDestroyNotify) _e2_action_clean1);
//	actions_array = g_ptr_array_new ();
	actions_store = gtk_tree_store_new (5, G_TYPE_STRING, G_TYPE_STRING,
		G_TYPE_INT, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN);
	//CHECKME is this still necessary?
//	g_object_ref (actions_store);

	toggles_hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
		(GDestroyNotify) _e2_action_free_toggle);
	toggles_array [E2_TOGGLE_PANE1FULL] = g_strconcat (_A(11),".",_A(116),NULL);
	toggles_array [E2_TOGGLE_PANE2FULL] = g_strconcat (_A(12),".",_A(116),NULL);
	toggles_array [E2_TOGGLE_PANE1HIDDEN] = g_strconcat (_A(11),".",_A(87),NULL);
	toggles_array [E2_TOGGLE_PANE2HIDDEN] = g_strconcat (_A(12),".",_A(87),NULL);
	toggles_array [E2_TOGGLE_PANE1FILTERS] = g_strconcat (_A(11),".",_A(25),NULL);
	toggles_array [E2_TOGGLE_PANE2FILTERS] = g_strconcat (_A(12),".",_A(25),NULL);
	toggles_array [E2_TOGGLE_OUTPUTFULL] = g_strconcat (_A(10),".",_A(116),NULL);
	toggles_array [E2_TOGGLE_OUTPUTSHADE] = g_strconcat (_A(10),".",_A(86),NULL);
#ifdef E2_VFS
	toggles_array [E2_TOGGLE_PANE1SPACE] = g_strconcat (_A(11),".",_A(123),NULL);
	toggles_array [E2_TOGGLE_PANE2SPACE] = g_strconcat (_A(12),".",_A(123),NULL);
#endif
	//register most actions
	//note translated action labels needed here, i.e. before config loaded
	//all action names must be freeable
	E2_Action homeless_actions[] =
	{	//here we don't bother explicitly setting 3 trailing NULL parameters
		{ g_strconcat(_A(6),".",_A(23),NULL), (gboolean (*)(gpointer,E2_ActionRuntime*))NULL, FALSE, E2_ACTION_TYPE_FILE_ACTIONS,
			E2_ACTION_EXCLUDE_ACCEL | E2_ACTION_EXCLUDE_TOOLBAR | E2_ACTION_EXCLUDE_LAYOUT, },
		{ g_strconcat(_A(6),".",_A(26),NULL), (gboolean (*)(gpointer,E2_ActionRuntime*))NULL, FALSE, E2_ACTION_TYPE_FILE_HANDLERS,
			E2_ACTION_EXCLUDE_ACCEL | E2_ACTION_EXCLUDE_TOOLBAR | E2_ACTION_EXCLUDE_LAYOUT, },
		{ g_strconcat(_A(16),".",_A(28),NULL), (gboolean (*)(gpointer,E2_ActionRuntime*))NULL, FALSE, E2_ACTION_TYPE_PLUGINS,
			E2_ACTION_EXCLUDE_ACCEL | E2_ACTION_EXCLUDE_TOOLBAR, },
		{ g_strdup(_A(21)), (gboolean (*)(gpointer,E2_ActionRuntime*))NULL, FALSE, E2_ACTION_TYPE_SEPARATOR,
			E2_ACTION_EXCLUDE_ACCEL | E2_ACTION_EXCLUDE_LAYOUT, },
		{ g_strdup(_A(22)), (gboolean (*)(gpointer,E2_ActionRuntime*))NULL, FALSE, E2_ACTION_TYPE_SUBMENU,
			E2_ACTION_EXCLUDE_ACCEL | E2_ACTION_EXCLUDE_LAYOUT, },
		{ g_strconcat(_A(1),".",_A(75),NULL), e2_main_user_shutdown, E2_ACTION_TYPE_ITEM, FALSE, 0, },

		{ g_strconcat(_A(3),".",_C(34),NULL), e2_plugins_configure, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strdup(_A(20)), _e2_action_custom_command, TRUE, E2_ACTION_TYPE_ITEM,
			E2_ACTION_EXCLUDE_GENERAL, },
		{ g_strconcat(_A(17),".",_A(118),NULL), _e2_action_do_toggle, TRUE, E2_ACTION_TYPE_TOGGLE,
			E2_ACTION_EXCLUDE_ACCEL | E2_ACTION_EXCLUDE_TOGGLE, },
		{ g_strconcat(_A(17),".",_A(119),NULL), _e2_action_do_toggle, TRUE, E2_ACTION_TYPE_TOGGLE,
			E2_ACTION_EXCLUDE_ACCEL | E2_ACTION_EXCLUDE_TOGGLE, },

		{ g_strconcat(_A(3),".",_A(34),NULL), _e2_action_configure, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(3),".",_A(43),NULL), _e2_action_configure_default, FALSE, E2_ACTION_TYPE_ITEM, 0, },
	};

	guint i, count = sizeof (homeless_actions) / sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&homeless_actions[i]);
	//register most other actions
	// (some others are in pane create fn)
	e2_task_actions_register ();
	e2_output_actions_register();
//	e2_context_menu_actions_register();
	e2_bookmark_actions_register ();
	e2_filetype_actions_register ();
	e2_command_line_actions_register ();
	e2_command_actions_register ();
	e2_window_actions_register ();
	e2_pane_actions_register ();
//	e2_toolbar_actions_register ();
	e2_about_dialog_actions_register ();
	e2_edit_dialog_actions_register ();
	e2_view_dialog_actions_register ();
//	e2_search_dialog_actions_register ();
	e2_mkdir_dialog_actions_register ();
#ifdef WITH_KEYFAKE
	e2_keybinding_actions_register ();
#endif
#ifdef WITH_BUTTONFAKE
	e2_mousebinding_actions_register ();
#endif
	e2_action_option_actions_register ();
#ifdef E2_FS_MOUNTABLE
	e2_fs_mount_actions_register ();
#endif
	//sort the actions tree store
	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (actions_store);
	gtk_tree_sortable_set_sort_func (sortable, 0, _e2_action_tree_compare_cb,
		NULL, NULL);
	gtk_tree_sortable_set_sort_column_id (sortable, 0, GTK_SORT_ASCENDING);
}
/**
@brief setup array of translated action labels

No spaces in names, so action arguments can be separated by a space.
Some of these are the same as config dialog labels.
Array size defined in e2_action.h

@return
*/
void e2_action_setup_labels (void)
{   //CHECKME= which of these names can be rationalised ?
	//'parent' names
	action_labels[0] = _("bookmark");
	action_labels[1] = _("command");
	action_labels[2] = _("children");
	action_labels[3] = _("configure");
	action_labels[4] = _("dialog");
	action_labels[5] = _("dirline");
	action_labels[6] = _("file");
	action_labels[7] = _("filelist");
	action_labels[8] = _("history");
	action_labels[9] = _("option");
	action_labels[10] = _("output");  // = _C(28)
	action_labels[11] = _("pane1");  // ~ _C(29)
	action_labels[12] = _("pane2");  // ~ _C(31)
	action_labels[13] = _("pane");
	action_labels[14] = _("panes");  //= _C(33)
	action_labels[15] = _("pending");
	action_labels[16] = _("plugin");  //this is also used in menus - see #define PLUGIN in emelfm2.h
	action_labels[17] = _("toggle");
	action_labels[18] = _("trash");
	action_labels[19] = _("separator");  //not really an action, but not a config either
	action_labels[20] = _("<custom command>");
	action_labels[21] = _("<separator>");
	action_labels[22] = _("<submenu>");
	//'child' names
	action_labels[23] = _("<actions>");
	action_labels[24] = _("<bookmarks>");
	action_labels[25] = _("<filters>");
	action_labels[26] = _("<handlers>");
	action_labels[27] = _("<line>");
	action_labels[28] = _("<menu>");
	action_labels[29] = _("about");
	action_labels[30] = _("activate");
	action_labels[31] = _("add");
	action_labels[32] = _("add_history");
	action_labels[33] = _("adjust_ratio");
	action_labels[34] = _("application");
	action_labels[35] = _("change");
	action_labels[36] = _("clear");
	action_labels[37] = _("clear_history");
	action_labels[38] = _("complete");
	action_labels[39] = _("copy");
	action_labels[40] = _("copy_as");
	action_labels[41] = _("copy_merge");
	action_labels[42] = _("copy_with_time");
	action_labels[43] = _("default");
	action_labels[44] = _("del_history");
	action_labels[45] = _("delete");
	action_labels[46] = _("edit");
	action_labels[47] = _("edit_again");
	action_labels[48] = _("filetype");
	action_labels[49] = _("find");
	action_labels[50] = _("focus");
	action_labels[51] = _("fullscreen");
	action_labels[52] = _("go_back");
	action_labels[53] = _("go_forward");
	action_labels[54] = _("go_up");
	action_labels[55] = _("goto_bottom");
	action_labels[56] = _("goto_top");
	action_labels[57] = _("help");
	action_labels[58] = _("info");
	action_labels[59] = _("insert_selection");
	action_labels[60] = _("invert_selection");
	action_labels[61] = _("list");
	action_labels[62] = _("mirror");
	action_labels[63] = _("mkdir");
	action_labels[64] = _("mountpoints");
	action_labels[65] = _("move");
	action_labels[66] = _("move_as");
	action_labels[67] = _("open");
	action_labels[68] = _("open_in_other");
	action_labels[69] = _("open_with");
	action_labels[70] = _("owners");
	action_labels[71] = _("page_down");
	action_labels[72] = _("page_up");
	action_labels[73] = _("permissions");
	action_labels[74] = _("print");
	action_labels[75] = _("quit");
	action_labels[76] = _("refresh");
	action_labels[77] = _("refreshresume");
	action_labels[78] = _("refreshsuspend");
	action_labels[79] = _("rename");
	action_labels[80] = _("scroll_down");
	action_labels[81] = _("scroll_up");
	action_labels[82] = _("search");  //= _C(37)
	action_labels[83] = _("select_type");
	action_labels[84] = _("send");
	action_labels[85] = _("set");
	action_labels[86] = _("show");
	action_labels[87] = _("show_hidden");
	action_labels[88] = _("show_menu");
	action_labels[89] = _("sortaccesssed");
	action_labels[90] = _("sortchanged");
	action_labels[91] = _("sortext");
	action_labels[92] = _("sortgroup");
	action_labels[93] = _("sortmodified");
	action_labels[94] = _("sortname");
	action_labels[95] = _("sortpermission");
	action_labels[96] = _("sortsize");
	action_labels[97] = _("sortuser");
	action_labels[98] = _("switch");
	action_labels[99] = _("symlink");
	action_labels[100] = _("symlink_as");
	action_labels[101] = _("sync");
	action_labels[102] = _("toggle_direction");
	action_labels[103] = _("toggle_focus");
	action_labels[104] = _("toggle_select_all");
	action_labels[105] = _("toggle_selected");
#ifdef E2_TREEDIALOG
	action_labels[106] = _("tree");
#endif
	action_labels[107] = _("unpack");
#ifdef WITH_UNTRASH
	action_labels[108] = _("untrash");
#endif
	action_labels[109] = _("view");  //= _C(42)
	action_labels[110] = _("view_again");
	action_labels[111] = _("view_at");
	//these are action _parameter_ strings
	action_labels[112] = _("child");
	action_labels[113] = _("ctrl");
	action_labels[114] = _("dirs");
	action_labels[115] = _("escape");
	action_labels[116] = _("expand");
	action_labels[117] = _("files");
	action_labels[118] = _("off");
	action_labels[119] = _("on");
	action_labels[120] = _("quote");
	action_labels[121] = _("shift");
	action_labels[122] = _("top");
	//late entries, out of order
	//maybe more space here, see array definition in header file
#ifdef E2_VFS
	action_labels[123] = _("<spacemenu>");
	action_labels[124] = _("dummy");
	action_labels[125] = _("namespace");
	action_labels[126] = _("unpack_in_other");
#endif
#ifdef WITH_KEYFAKE
	action_labels[127] = _("key");
#endif
#if defined(WITH_KEYFAKE) || defined(WITH_BUTTONFAKE)
	action_labels[128] = _("fake");
#endif
#ifdef WITH_BUTTONFAKE
	action_labels[129] = _("button");
#endif
}
#define LAST_NAME_INDEX 129
#if LAST_NAME_INDEX > ALLOCATED_NAMES - 1
# warning must increase size of action names array
#endif
#undef LAST_NAME_INDEX

/*
//copy all actions, for help document
void build_list (gpointer key, gpointer value, GString *text)
{
	E2_Action *action = (E2_Action *) value;
	if (action->type != E2_ACTION_TYPE_DUMMY)
	{
		text = g_string_append (text, action->name);
		text = g_string_append_c (text, '\t');
		text = g_string_append_c (text, (action->has_arg) ? 'Y' : 'N');
		text = g_string_append_c (text, '\n');
	}
	return;
}
void e2_action_list_all (void)
{
	GString *cmds_list = g_string_new ("");
	g_hash_table_foreach (actions_hash, (GHFunc) build_list, cmds_list);

	GtkClipboard* clipper = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
	gtk_clipboard_set_text (clipper, cmds_list->str, cmds_list->len);
	ls e2*

	g_string_free (cmds_list, TRUE);
} */
