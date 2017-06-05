/* $Id: e2_menu.c 3068 2014-02-16 06:32:10Z tpgww $

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
@file src/utils/e2_menu.c
@brief Menu utility functions

This file contains utility and helper functions for menus.
*/

#include <string.h>
#include <pthread.h>

#include "emelfm2.h"
#include "e2_menu.h"
#include "e2_dialog.h"
#include "e2_plugins.h"
#include "e2_task.h"
#include "e2_filetype.h"
#ifdef E2_FS_MOUNTABLE
# include "e2_filelist.h"
#endif

extern pthread_mutex_t task_mutex;

/**
@brief clear data for each item in bookmarks menu
@param data data to be cleared
@param closure UNUSED
@return
*/
void e2_menu_mark_clear (gpointer data, GClosure *closure)
{
	E2_MarkData *mdata = data;
	printd (DEBUG, "e2_menu_mark_clear() for %s", mdata->mark_path);
	g_free (mdata->mark_path);
	if (mdata->tpath != NULL)
		gtk_tree_path_free (mdata->tpath);
	DEALLOCATE (E2_MarkData, data);
}

  /*******************/
 /**** callbacks ****/
/*******************/

/**
@brief timer callback to execute action corresponding to @a menu_item selected from a onetime menu
We do this outside the menu-item "activated" callback to avoid disrupting gtk's
menu-activation process.
@a menu_item must persist for at least the start of the action execution.
Associated action runtime data are stolen and cleared here.
@param menu_item the activated item

@return FALSE to remove the source
*/
static gboolean _e2_menu_action_deferred_cb (GtkWidget *menu_item)
{
	printd (DEBUG, "timer callback: e2_menu_action_deferred_cb");
	//don't want this action-data to be cleared with the menu
	E2_ActionRuntime *art = (E2_ActionRuntime *)g_object_steal_data
		(G_OBJECT (menu_item), "e2-actruntime");
	e2_action_run (menu_item, art);
	e2_action_free_runtime (art);
	e2_utils_fake_event (); //CHECKME does this actually help?
	return FALSE;
}
/**
@brief like _e2_menu_action_deferred_cb, but for a menu which is persistent, not onetime
@param menu_item the activated item
@return FALSE to remove the source
*/
static gboolean _e2_menu_action_deferred_cb2 (GtkWidget *menu_item)
{
	printd (DEBUG, "timer callback: e2_menu_action_deferred_cb2");
	E2_ActionRuntime *art = (E2_ActionRuntime *)g_object_get_data
		(G_OBJECT (menu_item), "e2-actruntime");
	e2_action_run (menu_item, art);
	e2_utils_fake_event (); //CHECKME does this actually help?
	return FALSE;
}
/**
@brief "activate" signal callback for an item in a onetime menu
Setup to run the specified action. To avoid disrupting gtk's menu-activation
process, we do not run the action here.
Note: depending on the menu-destruction process and whether the action is
synchronous, menu_item may not persist until the action is finished running,
but it must persist during the start of running i.e. possible race.
@param menu_item the activated item
@param data UNUSED pointer specified when callback was connected

@return
*/
void e2_menu_action_activated_cb (GtkMenuItem *menu_item, gpointer data)
{
	printd (DEBUG, "_e2_menu_action_activated_cb");
	//an idle-callback is no good !?
	g_timeout_add (20, (GSourceFunc)_e2_menu_action_deferred_cb, menu_item);
}
/**
@brief like _e2_menu_action_activated_cb, but for menus which are persistent, not onetime.
@param menu_item the activated item
@param data UNUSED pointer specified when callback was connected
*/
void e2_menu_action_activated_cb2 (GtkMenuItem *menu_item, gpointer data)
{
	printd (DEBUG, "e2_menu_action_activated_cb2");
	//an idle-callback is no good !?
	g_timeout_add (20, (GSourceFunc)_e2_menu_action_deferred_cb2, menu_item);
}
/**
@brief timer callback to cleanup a redundant menu widget
Assumes menu is hidden so that BGL is irrelevant
@param menu the widget to be cleared
@return FALSE to stop the timer
*/
gboolean e2_menu_destroy (GtkWidget *menu)
{
	printd (DEBUG, "timer callback: e2_menu_destroy, menu: %x", menu);
	gtk_widget_destroy (menu);
	return FALSE;
}
/**
@brief "selection-done" signal callback for @a menu, initiates its destruction

@param menu the menu widget
@param data UNUSED data specified when the callback was connected

@return
*/
void e2_menu_selection_done_cb (GtkWidget *menu, gpointer data)
{
	printd (DEBUG, "e2_menu_selection_done_cb, menu: %x", menu);
//	NEEDCLOSEBGL
	//don't destroy the menu immediately, gtk upstream and action processor
	//have not necessarily finished with it
#ifdef USE_GLIB2_14
	g_timeout_add_seconds (10,
#else
	g_timeout_add (10000,
#endif
		(GSourceFunc)e2_menu_destroy, menu);
//	NEEDOPENBGL
}
/**
@brief callback to

@param menu_item the activated menu-item widget
@param widget widget specified when the callback was connected

@return
*/
void e2_menu_control_cb (GtkMenuItem *menu_item, gpointer widget)
{
	NEEDCLOSEBGL
	e2_option_connect (widget, gtk_check_menu_item_get_active
		(GTK_CHECK_MENU_ITEM (menu_item)));
	NEEDOPENBGL
}
/**
@brief update set value when tied check menu item is activated
@param menu_item the item that was activated
@param set pointer to data for optionset tied to @a menu_item
@return
*/
static void _e2_menu_check_value_changed_cb (GtkWidget *menu_item, E2_OptionSet *set)
{
	GtkWidget *controller = g_object_get_data (G_OBJECT (menu_item),
		"e2-controller-widget");
	if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (controller),
		"e2-controller-blocked")))
	{
//		NEEDCLOSEBGL
		e2_option_bool_set_direct (set, gtk_check_menu_item_get_active
			(GTK_CHECK_MENU_ITEM (menu_item)));
//		NEEDOPENBGL
	}
}
/**
@brief set @a menu_item state if it's not blocked and not already at the desired state
This is a hook-function callack upon change of set data associated with @a menu_item
@param state pointerised TRUE/FALSE, the value to apply to @a menu_item
@param menu_item the widget to change

@return TRUE always
*/
static gboolean _e2_menu_check_change_value_hook (gpointer state, GtkWidget *menu_item)
{
	GtkWidget *controller = g_object_get_data (G_OBJECT (menu_item),
		"e2-controller-widget");
	if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (controller),
		"e2-controller-blocked")))
	{
		gboolean value = GPOINTER_TO_INT (state);
		gboolean current = gtk_check_menu_item_get_active
			(GTK_CHECK_MENU_ITEM (menu_item));
		if (value != current)
			gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (menu_item), value);
	}
	return TRUE;
}
/**
@brief add tied check item to @a menu

@param menu menu widget
@param set pointer to data for set whose value is tied to the check button
@param controller widget which may have "e2-controller-blocked" data to prevent set value being updated when item is activated, can be NULL
@param func callback function for handling menu item toggle, or NULL
@param data pointer to data to send to the callback

@return the menu item widget
*/
static GtkWidget *_e2_menu_add_tied_check (GtkWidget *menu, E2_OptionSet *set,
	GtkWidget *controller, void (*func)(GtkCheckMenuItem*,gpointer), gpointer data)
{
	GtkWidget *menu_item = e2_menu_add_check (menu, set->desc,
		e2_option_bool_get_direct (set), func, data);
	g_object_set_data (G_OBJECT (menu_item), "e2-controller-widget", controller);
	//this will update the set value when the check menu item is activated
	g_signal_connect (G_OBJECT (menu_item), "toggled",
		G_CALLBACK (_e2_menu_check_value_changed_cb), set);
	//this will update the check menu item when the config data value changes
	e2_option_attach_value_changed_simple (set, menu_item,
		(HookFunc)_e2_menu_check_change_value_hook, menu_item);

	e2_widget_set_safetip (menu_item, set->tip);

	//conform to dependent set if any
	e2_widget_handle_depends (menu_item, set);

	return menu_item;
}

  /****************/
 /**** public ****/
/****************/

/**
@brief add item to @a menu
Args of @a func should be (GtkMenuItem*,some pointer)
@param menu menu widget
@param label menu item text, optionally with mnemonic
@param icon custom iconfile path, or stock-icon identifier, or NULL or "" for no icon
@param tip tooltip string, or NULL
@param activate_cb callback function for handling menu item activation, or NULL
@param data pointer to data to send to @a activate_cb

@return the menu item widget
*/
GtkWidget *e2_menu_add (GtkWidget *menu, const gchar *labeltext, const gchar *icon,
	const gchar *tip, void (*activate_cb)(), gpointer data)
{
	GtkWidget *menu_item;
	gint choice;
	if (icon == NULL)
		choice = 2;	//no icon
	else
		choice = e2_option_sel_get ("menu-show-icons");
	if (choice == 0)	//theme
	{
#ifdef USE_GTK3_10
		choice = 2;
		printd (DEBUG, "Force themish label-only menu item, omit %s", icon);
#else
		//ensure that the "gtk-menu-images" property is registered - does this actually achieve anything ?
		static GtkWidget *dummy = NULL;
		if (dummy == NULL)
		{
			dummy = gtk_image_menu_item_new ();
			gtk_widget_destroy (dummy);
		}
		GtkSettings* defs = gtk_settings_get_default ();	//assume this always exists
		gboolean show;
		g_object_get (G_OBJECT (defs), "gtk-menu-images", &show, NULL);
		if (show)
			choice = 1;
#endif
	}
	if (choice == 1 && icon != NULL && *icon != '\0') //show icon
	{
#ifdef USE_GTK3_10
#warning GTK 3.10 deprecates menu-items that include an icon. No reasonable workaround is available.
#endif
		GtkWidget *image = e2_widget_get_icon (icon, GTK_ICON_SIZE_MENU);	//e2_option_int_get ("menu-isize") + 1);
		if (image != NULL)
		{
			printd (DEBUG, "Create imaged menu item using icon %s and label %s", icon, labeltext);
			menu_item = gtk_image_menu_item_new_with_mnemonic
				(labeltext != NULL ? labeltext : "");
			gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item), image);
#ifdef USE_GTK3_10
			gtk_image_menu_item_set_always_show_image ((GtkImageMenuItem*)menu_item, TRUE);
#endif
		}
		else
		{
			printd (DEBUG, "No icon '%s', fall back to labelled menu item %s", icon, labeltext);
			menu_item = gtk_menu_item_new_with_mnemonic
				(labeltext != NULL ? labeltext : _("Missing _image"));
		}
	}
	else
	{
		printd (DEBUG, "Icon not wanted or N/A, create labelled menu item %s", labeltext);
		menu_item = gtk_menu_item_new_with_mnemonic (labeltext);
	}

	//turn on markup for the label
	GList *member, *children = gtk_container_get_children (GTK_CONTAINER (menu_item));
	for  (member = children; member != NULL; member = member->next)
	{
		GtkWidget *child = (GtkWidget *)member->data;
		if (GTK_IS_LABEL (child))
		{
			gtk_label_set_use_markup (GTK_LABEL (child), TRUE);
			break;
		}
	}
	g_list_free (children);

	if (activate_cb != G_CALLBACK(NULL))
		g_signal_connect (G_OBJECT (menu_item), "activate",
			G_CALLBACK (activate_cb), data);

	gtk_menu_shell_append (GTK_MENU_SHELL (menu), menu_item);
	gtk_widget_show_all (menu_item);

	if (tip != NULL && *tip != '\0')
		e2_widget_set_safetip (menu_item, tip);
	return menu_item;
}
/**
@brief add action item to onetime @a menu

@param menu menu widget
@param label menu item text, optionally with mnemonic
@param icon custom iconfile path, or stock-icon identifier
@param tip tooltip string
@param action_name identifier of the form "_A(n)._A(m)", or external command
@param arg argument(s) string for the action, or NULL

@return the menu item widget, or NULL
*/
GtkWidget *e2_menu_add_action (GtkWidget *menu, const gchar *label, const gchar *icon,
	const gchar *tip, const gchar *action_name, const gchar *arg)
{
	gchar *real_arg;
	E2_Action *action = e2_action_get_with_custom (action_name, arg, &real_arg);
	E2_ActionRuntime *art = e2_action_pack_runtime (action, real_arg, g_free);
	if (art != NULL)
	{
		GtkWidget *item = e2_menu_add (menu, label, icon, tip,
			e2_menu_action_activated_cb, NULL);
		g_object_set_data_full (G_OBJECT(item), "e2-actruntime",
			art, (GDestroyNotify)e2_action_free_runtime);
		return item;
	}
	return NULL;
}
/**
@brief add check item to @a menu
Args of @a func should be (GtkCheckMenuItem*,some pointer)
@param menu menu widget
@param label menu item text, optionally with mnemonic
@param state initial state of the created item, T/F
@param func void* callback function for handling menu item selection, or NULL
@param data pointer to data to send to the callback

@return the menu item widget
*/
GtkWidget *e2_menu_add_check (GtkWidget *menu, const gchar *label, gboolean state,
	void (*func)(), gpointer data)
{
	GtkWidget *check = gtk_check_menu_item_new_with_mnemonic (label);
	if (menu != NULL)
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), check);
	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (check), state);
	if (func != NULL)
		g_signal_connect (G_OBJECT (check), "toggled", G_CALLBACK (func), data);
	gtk_widget_show (check);
	return check;
}
/**
@brief add radio item to @a menu
Args of @a func should be (GtkMenuItem*,some pointer)
@param menu menu widget, or NULL
@param group pointer to gslist of radio group
@param label menu item text, optionally with mnemonic
@param state initial state of the created item, T/F
@param func callback function for handling menu-item activation, or NULL
@param data pointer to data to send to the callback

@return the menu item widget
*/
GtkWidget *e2_menu_add_radio (GtkWidget *menu, GSList **group, const gchar *label,
	gboolean state, void (*func)(), gpointer data)
{
	GtkWidget *radio = gtk_radio_menu_item_new_with_mnemonic (*group, label);
	*group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (radio));
	if (menu != NULL)
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), radio);
	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (radio), state);
	if (func != NULL)
		g_signal_connect (G_OBJECT (radio), "activate", G_CALLBACK (func), data);
	gtk_widget_show (radio);
	return radio;
}
/**
@brief add separator to @a menu

@param menu menu widget, or NULL

@return the menu item widget
*/
GtkWidget *e2_menu_add_separator (GtkWidget *menu)
{
	GtkWidget *sep = gtk_separator_menu_item_new ();
	if (menu != NULL)
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), sep);
	gtk_widget_show (sep);
	return sep;
}
#ifndef USE_GTK3_4
/**
@brief add tear-off to @a menu

@param menu menu widget

@return the menu item widget
*/
GtkWidget *e2_menu_add_tear_off (GtkWidget *menu)
{
	GtkWidget *menu_item = gtk_tearoff_menu_item_new ();
	gtk_menu_shell_append (GTK_MENU_SHELL (menu), menu_item);
	gtk_widget_show (menu_item);
	return menu_item;
}
#endif
/**
@brief add sub-menu to @a menu
Note - no use to add a tooltip for item, it does not show
@param menu menu widget
@param label string with the name to appear in the menu
@param icon icon-file name string

@return the sub-menu item widget
*/
GtkWidget *e2_menu_add_submenu (GtkWidget *menu, const gchar *label, const gchar *icon)
{
	GtkWidget *menu_item = e2_menu_add (menu, label, icon, NULL, NULL, NULL);
	GtkWidget *submenu = e2_menu_get ();
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menu_item), submenu);
	gtk_widget_show (submenu);
	return submenu;
}
/**
@brief create toggle menu item and add it to onetime @a menu
This expects to be called twice in succession for each toggle-item.
None of the strings may be NULL
@param menu the menu to which the item will be added
@param label menu label string
@param onetime TRUE if @a menu will be destroyed after use
@param icon menu icon name
@param tip menu tip string
@param type string "toggle.on" or "toggle.off"
@param cmd toggle action, for creating a hash key

@return after the first of the pair, NULL: after the second, the menu item or NULL upon error
*/
GtkWidget *e2_menu_add_toggle (GtkWidget *menu, gboolean onetime,
	const gchar *label, const gchar *icon, const gchar *tip, const gchar *type, const gchar *cmd)
{
	static gboolean first = TRUE;
	static gboolean firststate;
	static gchar *firstlabel;
	static gchar *firsticon;
	static gchar *firsttip;
	static gchar *firstcmd; //NEVER FREE THIS
	if (label == NULL) label = "";
	if (icon == NULL) icon = "";
	if (tip == NULL) tip = "";
	if (cmd == NULL) cmd = "";
	if (first)
	{	//process 1st of a pair
		first = FALSE;
		//park until we get the paired name, so we can make hash key
		firststate = g_str_has_suffix (type, _A(119));	//_(on
		firstlabel = g_strdup (label);
		firsticon = g_strdup (icon);
		firsttip = g_strdup (tip);
		firstcmd = g_strdup (cmd);
		return NULL;
	}
	//process 2nd of the pair
	first = TRUE;
	//hash table key is the joined action strings
	gchar *hashkey = g_strconcat (firstcmd, ".", cmd, NULL);
	//get toggle data, if any
	E2_ToggleData *data = g_hash_table_lookup (toggles_hash, hashkey);
	if (data == NULL)
	{
		data = ALLOCATE (E2_ToggleData);	//deallocation when # cleared
		CHECKALLOCATEDFATAL (data);
		g_hash_table_insert (toggles_hash, g_strdup (hashkey), data);
		data->current_state = firststate;
		data->boxes = NULL;
		if (firststate)
		{
			data->true_action = firstcmd;	//action command string
			data->false_action = g_strdup (cmd); //ditto
		}
		else
		{
			data->false_action = firstcmd;
			data->true_action = g_strdup (cmd);
		}
	}

	const gchar *uselabel, *useicon, *usetip;
	if (data->current_state == firststate)
	{
		uselabel = firstlabel;
		useicon = firsticon;
		usetip = firsttip;
	}
	else
	{
		uselabel = label;
		useicon = icon;
		usetip = tip;
	}
	gchar *realarg;
	E2_Action *action = e2_action_get_with_custom (type, cmd, &realarg);
	E2_ActionRuntime *art = e2_action_pack_runtime (action, hashkey, g_free); //CHECKME realarg instead ?
	GtkWidget *item;
	if (art != NULL)
	{
		item = e2_menu_add (menu, uselabel, useicon, usetip,
			(onetime) ? e2_menu_action_activated_cb:e2_menu_action_activated_cb2,
			NULL);
		g_object_set_data_full (G_OBJECT (item), "e2-actruntime", art,
			(GDestroyNotify) e2_action_free_runtime);
	}
	else
		item = NULL;

	g_free (firstlabel);
	g_free (firsticon);
	g_free (firsttip);
	g_free (realarg);

	return item;
}
/**
@brief popup detroyable menu @a menu

@a menu is destroyed about 10 seconds after a selection is made.

@param menu menu widget
@param button event button
@param time event time

@return
*/
void e2_menu_popup (GtkWidget *menu, gint button, guint32 time)
{
	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);
	gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, button, time);
}
/* *
@brief

@param menu menu widget
@param x UNUSED
@param y UNUSED
@param button event button
@param time event time

@return
*/
/* UNUSED
void _e2_menu_popup (GtkWidget *menu, gint x, gint y, gint button, guint32 time)
{
	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);
	gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, button, time);
}
*/
/**
@brief add items for a series of config options to @a menu
CHECKME possible that something downstream needs BGL closed
@param controller "parent" widget for the created menu
@param menu menu widget to populate, or NULL to create a new one
@param set first of a NULL-terminated series of option data structs

@return @a menu or a new menu widget
*/
GtkWidget *e2_menu_create_options_menu (GtkWidget *controller, GtkWidget *menu,
	E2_OptionSet *set, ...)
{
	if (menu == NULL)
		menu = e2_menu_get ();
	va_list args;
	va_start (args, set);
	while (set != NULL)
	{
		void (*func)(GtkCheckMenuItem*,gpointer) = va_arg (args, gpointer);
		gpointer data = va_arg (args, gpointer);
		switch (set->type)
		{
			case E2_OPTION_TYPE_BOOL:
				_e2_menu_add_tied_check (menu, set, controller, func, data);
				break;
			case E2_OPTION_TYPE_SEL:
				e2_option_sel_add_menu_widget (controller, menu, set, func, data);
				break;
			default:
				break;
		}
		set = va_arg (args, E2_OptionSet *);
	}
	va_end (args);

	return menu;
}
/**
@brief add items to bookmarks menu, recursively if children exist

@param menu (sub)menu widget to which the items will be added
@param top_menu top-level bookmarks-menu widget, maybe same as @a menu
@param action pointer to data for the pseudo-action being processed
@param markflags flags indicating how mark is to be processed, E2_MARKFLAG_ACTIONS etc
@param panenum the pane whose path will be used when adding a mark (E2PANECUR, E2PANE1 or E2PANE2)
@param mdl pointer to config data treemodel for bookmarks
@param iter pointer to iter to be used for interrogating @a mdl

@return
*/
void e2_menu_add_bookmark_items (GtkWidget *menu, GtkWidget *top_menu,
	E2_Action *action, guint32 markflags, gint32 panenum,
	GtkTreeModel *mdl, GtkTreeIter *iter)
{
	do
	{
		GtkWidget *item;
		gchar *label, *icon, *tip, *markpath;

		gtk_tree_model_get (mdl, iter, 0, &label, 1, &icon, 2, &tip, 3, &markpath, -1);

		if (gtk_tree_model_iter_has_child (mdl, iter))
		{
			GtkTreeIter iter2;

			//items with sub-menu can't show tip etc
			item = e2_menu_add (menu, label, icon, NULL, NULL, NULL);
			GtkWidget *submenu = e2_menu_get ();
			gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);

			gtk_tree_model_iter_children (mdl, &iter2, iter);
			e2_menu_add_bookmark_items (submenu, top_menu, action, markflags,
				panenum, mdl, &iter2);	//recurse
			gtk_widget_show (submenu);
		}
		else
		{
			void (*activatecb)();

			E2_MarkData *mdata = ALLOCATE (E2_MarkData);
			CHECKALLOCATEDWARN (mdata, return;)

			mdata->pane_num = panenum;
			mdata->mark_flags = markflags;
			mdata->mark_path = e2_utils_replace_vars (markpath, TRUE);
			mdata->tpath = gtk_tree_model_get_path (mdl, iter);
			mdata->top_menu = top_menu;

			//don't support variables in label
			gchar *_tip = e2_utils_replace_vars (tip, FALSE);
			item = e2_menu_add (menu, label, icon, _tip, NULL, NULL);
			g_free (_tip);

			g_signal_connect_data (G_OBJECT (item), "button-press-event",
				G_CALLBACK (e2_bookmark_button_press_cb), mdata,
				(GClosureNotify) e2_menu_mark_clear, 0);
			E2_ActionRuntime *art = e2_action_pack_runtime (action,
				g_strdup_printf ("%d %s", panenum, mdata->mark_path), g_free); //data for add/open
			g_object_set_data_full (G_OBJECT (item), "e2-actruntime", art,
				(GDestroyNotify) e2_action_free_runtime);
			if (markflags & E2_MARKFLAG_ONETIME)
				activatecb = e2_menu_action_activated_cb;
			else
				activatecb = e2_menu_action_activated_cb2;
			//must connect_after to the activate signal, or else the
			//button-press-event callback will never occur
			g_signal_connect_after (G_OBJECT (item), "activate",
				G_CALLBACK (activatecb), NULL);
		}

		g_free (label);
		g_free (icon);
		g_free (tip);
		g_free (markpath);
	} while (gtk_tree_model_iter_next (mdl, iter));
}

/**
@brief add items for all relevant menu-enabled plugins, to @a menu
Assumes app.plugacts is in menu-order, if it exists
@param menu widget to which items are added
@param onetime TRUE if @a menu is destroyed after usage
@param is_selection TRUE if one or more items are selected in active pane

@return
*/
void e2_menu_create_plugins_menu (GtkWidget *menu, gboolean onetime, gboolean is_selection)
{
	if (app.plugacts != NULL)
	{
		guint i;
		gpointer *dp;
		void (*activatecb)(GtkMenuItem*, gpointer) = (onetime) ?
			e2_menu_action_activated_cb : e2_menu_action_activated_cb2;
		gchar *prefix = (is_selection) ? NULL : g_strconcat (_A(6), ".", NULL);

		for (i = 0, dp = app.plugacts->pdata; i < app.plugacts->len; i++, dp++)
		{
			PluginAction *pa = *((PluginAction **)dp);
			if (IS_SHOWN(pa) && (is_selection || !g_str_has_prefix (pa->aname, prefix)))
			{
				E2_ActionRuntime *art = e2_action_pack_runtime (pa->action, pa->action_data, NULL);
				if (art != NULL)
				{
					GtkWidget *item = e2_menu_add (menu, pa->label, pa->icon,
						pa->description, activatecb, NULL);
					g_object_set_data_full (G_OBJECT (item), "e2-actruntime", art,
						(GDestroyNotify) e2_action_free_runtime);
				}
			}
		}
		if (!is_selection)
			g_free (prefix);
	}
}
/**
@brief process custom-menu item

This is the callback for 'activated' signal for the menu item

@param item the activated menu item
@param data UNUSED data specified when callback was connected

@return
*/
static void _e2_menu_custom_cb (GtkMenuItem *item, gpointer data)
{
	gchar *cmd = (gchar *) g_object_get_data (G_OBJECT (item), "custom-command");
	NEEDCLOSEBGL
	e2_command_run (cmd, E2_COMMAND_RANGE_DEFAULT, item
#ifdef E2_COMMANDQ
	, FALSE
#endif
	);
	NEEDOPENBGL
	//FIXME some sort of termination process ?
}
/*static void _e2_menu_custom_finished_cb (GtkWidget *menu, gpointer data)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
}
*/
/**
@brief add items to a menu of custom commands

@param model config option tree model for custom menus
@param iter tree iter to use for interrogating @a model
@param menu the menu to which items are to be added

@return
*/
static void _e2_menu_add_custom_items (GtkTreeModel *model, GtkTreeIter *iter, GtkWidget *menu)
{
	GtkTreeIter iter2;
	gchar *icon, *label, *tip, *command;
	do
	{
		gtk_tree_model_get (model, iter, 1, &icon, 2, &label, 3, &tip, 4, &command, -1);
		if (gtk_tree_model_iter_children (model, &iter2, iter))
		{
			GtkWidget *submenu = e2_menu_add_submenu (menu, label, icon);
			_e2_menu_add_custom_items (model, &iter2, submenu);	//recurse
			g_free (command);
		}
		else
		{
			if (strcmp (command, _A(21)) != 0)
			{
				GtkWidget *menu_item = e2_menu_add (menu, label, icon, tip,
					_e2_menu_custom_cb, NULL);
				g_object_set_data_full (G_OBJECT (menu_item), "custom-command",
					command, (GDestroyNotify) g_free);
			}
			else
			{
				e2_menu_add_separator (menu);
				g_free (command);
			}
		}
		//cleanup except command
		g_free (icon);
		g_free (label);
		g_free (tip);
	} while (gtk_tree_model_iter_next (model, iter));
}
/**
@brief construct a menu of custom commands

@param name UTF8 string, name of custom menu to use

@return the menu item widget, or NULL
*/
GtkWidget *e2_menu_create_custom_menu (gchar *name)
{
	GtkTreeIter iter, iter2;
	E2_OptionSet *set = e2_option_get ("custom-menus");
	if (e2_tree_find_iter_from_str (set->ex.tree.model, 0, name, &iter, TRUE)
		&& gtk_tree_model_iter_children (set->ex.tree.model, &iter2, &iter))
	{
		GtkWidget *menu = e2_menu_get ();
		_e2_menu_add_custom_items (set->ex.tree.model, &iter2, menu);
		return menu;
	}
	return NULL;
}
/**
@brief construct but not pop up a onetime menu of running child processes,
  with items which upon activation callback to @a func
Args for @a func should be (GtkMenuItem*, some pointer)
This func is usable for button-menu and output context menu
Uses check-items (no mnemonics) to indicate which procesess are active
@param type enumerator for which processes to gather
@param activate_cb callback function for handling menu item activation

The arguments for @a activate_cb must be (GtkMenuItem*,E2_TaskRuntime*).

@return the menu widget, or NULL
*/
#define CHILDMENUWIDTH 30
GtkWidget *e2_menu_create_child_menu (E2_ChildMenuType type, void (*activate_cb)())
{
	gchar *label, *s;
	gboolean active, empty = TRUE;
	GtkWidget *item, *menu = e2_menu_get ();
	pthread_mutex_lock (&task_mutex);
	GList *member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		E2_TaskRuntime *rt = member->data;
		if (type == E2_CHILD_ALL ||
			(type == E2_CHILD_ACTIVETASKS &&
			 (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED)
			) ||
			(!rt->action &&	//for these menus, we're not interested in actions
			 ( (type == E2_CHILD_ACTIVE &&
				(rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED))
			|| (type == E2_CHILD_OUTPUT &&
				gtk_text_buffer_get_mark (app.tab.buffer, rt->pidstr) != NULL) )
		    )
		   )
		{
			if (type == E2_CHILD_ACTIVETASKS && rt->action)
			{
				E2_ActionTaskData *atask = &rt->ex.action;
				gchar *srcdir = F_FILENAME_FROM_LOCALE (atask->currdir);
				gchar *shortdir = e2_utils_str_shorten (srcdir, 20, E2_DOTS_START);
				const gchar *subject;
				if (g_str_has_prefix (atask->action->name, _A(6)))
				{	//this was a "file-action"
					subject =
					(atask->rt_data == NULL || *((gchar *)atask->rt_data) == '\0') ?
						_("<selected items>") : (gchar *)atask->rt_data;
				}
				else
					subject = "";	//results in a redundant trailing space in the label
				label = g_strdup_printf ("%s %s %s", atask->action->name, shortdir, subject);
				F_FREE (srcdir, atask->currdir);
				g_free (shortdir);
			}
			else if (!rt->action)
			{
				//ellipsize the middle of longish commands
				s = e2_utils_str_shorten (rt->ex.command.command,
					CHILDMENUWIDTH - 7, E2_DOTS_MIDDLE);
				label = g_strconcat (rt->pidstr, ": ", s, NULL);
				g_free (s);
			}
			else
				label = NULL;

			if (label != NULL)
			{
				empty = FALSE;

				if (type == E2_CHILD_ALL)
				{
					item = gtk_check_menu_item_new_with_label (label);
					active = (rt != NULL
						&& (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED));
					gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (item), active);
				}
				else
				{
					item = gtk_menu_item_new_with_label (label);
				}
				if (activate_cb != G_CALLBACK (NULL))
					g_signal_connect (G_OBJECT (item), "activate",
						G_CALLBACK (activate_cb), rt);
				gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
				g_free (label);
			}
		}
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
	if (empty)
	{
		if (type == E2_CHILD_OUTPUT)
		{
			gtk_widget_destroy (menu);
			menu = NULL;
		}
		else
		{
			e2_menu_add (menu, _("no children"), NULL, NULL, activate_cb, NULL);
		}
	}

	if (menu != NULL)
	{
		gtk_widget_show_all (menu);
		g_signal_connect (G_OBJECT (menu), "selection-done",
			G_CALLBACK (e2_menu_selection_done_cb), NULL);
	}

	return menu;
}
/**
@brief create but not pop up a onetime filters-menu for pane to which @a view belongs

@param view data struct for view to be changed

@return the menu widget
*/
GtkWidget *e2_menu_create_filter_menu (ViewInfo *view)
{
	GtkWidget *menu = e2_menu_get ();
	view->check_name = e2_menu_add_check (menu, _("_Name filter"),
		view->name_filter.active, e2_name_filter_dialog_create_cb, view);
	view->check_size = e2_menu_add_check (menu, _("_Size filter"),
		view->size_filter.active, e2_size_filter_dialog_create_cb, view);
	view->check_date = e2_menu_add_check (menu, _("_Date filter"),
		view->date_filter.active, e2_date_filter_dialog_create_cb, view);
	e2_menu_add_separator (menu);
	view->check_dirs = e2_menu_add_check (menu, _("_Directories too"),
		view->filter_directories, e2_fileview_filter_dirs_cb, view);
	if (view->name_filter.active
		|| view->size_filter.active
		|| view->date_filter.active)
	{
		e2_menu_add_separator (menu);
		e2_menu_add (menu, _("_Remove all filters"), NULL, NULL,
			e2_fileview_remove_filters_cb, view);
	}

	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	return menu;
}
#ifdef E2_FS_MOUNTABLE
/**
@brief handle a selection from a mountpoints menu
The state of @a item when it arrives here is opposite to that
shown in the menu, when clicked
@param item the selected menu item
@param from the widget where the mounts menu was initiated

@return
*/
static void _e2_menu_mount_cb (GtkCheckMenuItem *item, gpointer from)
{
//	gint result;
	gchar *mpoint;
	gchar *elsewhere = NULL;
	gboolean newstate = gtk_check_menu_item_get_active (item);
//#ifdef USE_GTK2_16
//	point = gtk_menu_item_get_label (GTK_MENU_ITEM (item));
//#else
	mpoint = g_object_get_data (G_OBJECT (item), "_mountpoint_");
//#endif
#if defined (E2_HAL) || defined (E2_DEVKIT)
	gboolean remove = e2_fs_mount_is_ejectable (mpoint);	//check this before unmount
#endif
	//CHECKME synchronous umount command to mimimize risk of CWD error
//tag E2_BADQUOTES
	gchar *qp = e2_utils_quote_string (mpoint);
	gchar *cmd = g_strconcat ((newstate) ? E2_MOUNTCOMMAND : "|"E2_UNMOUNTCOMMAND,
		" ", qp, NULL);

	NEEDCLOSEBGL

	if (!newstate //doing an unmount
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
		&& g_str_has_prefix (curr_view->dir, mpoint))
#endif
	{	//if possible, change CWD out of mountpoint so we don't ourself block the unmount
		gchar *s = strrchr (mpoint, G_DIR_SEPARATOR);	//assumes no trailer on mountpoint string
		if (s > mpoint+1) //not doing root dir
			elsewhere = g_strndup (mpoint, s - mpoint);
		else //can't cd
			elsewhere = g_strdup (G_DIR_SEPARATOR_S);

		e2_fs_get_valid_path (&elsewhere, TRUE E2_ERR_NONE());
		//if (
			e2_fs_chdir (elsewhere E2_ERR_NONE()); //)
/*
#ifdef USE_GLIB2_22
		if (!e2_fs_mount_gio_unmount (mpoint)) //try for removable, first
#endif
*/
//			result =
			e2_command_run_at (cmd, elsewhere, E2_COMMAND_RANGE_DEFAULT, from	//has temporary change CWD
#ifdef E2_COMMANDQ
			, FALSE
#endif
			);
		//else
			//FIXME warn user about blockage
	}
	else
	{
/*
#ifdef USE_GLIB2_22
		if ((newstate && !e2_fs_mount_gio_mount (mpoint)) //mount && not removable
		|| (!newstate && !e2_fs_mount_gio_unmount (mpoint))) //unmount && not removable
//#endif
*/
//			result =
			e2_command_run (cmd, E2_COMMAND_RANGE_DEFAULT, from
#ifdef E2_COMMANDQ
			, FALSE
#endif
			);
	}
	g_free (cmd);

	if (newstate) //doing a mount
	{
#ifdef E2_FAM
		e2_filelist_request_refresh (curr_view->dir, FALSE);
		e2_filelist_request_refresh (other_view->dir, TRUE);
#else
		e2_filelist_check_dirty (GINT_TO_POINTER (1));
#endif
	}
	else	//doing an unmount
	{
#if defined (E2_HAL) || defined (E2_DEVKIT)
		if (remove)
		{
			cmd = g_strdup_printf ("eject %s", qp);
			e2_command_run_at (cmd, elsewhere, E2_COMMAND_RANGE_DEFAULT, from
# ifdef E2_COMMANDQ
			, FALSE
# endif
			);
			g_free (cmd);
		}
#endif
		if (elsewhere != NULL)
		{
//E2_VFSTMP may change space too
//			e2_pane_change_dir (curr_pane, point);	//not needed, with inofify at least
			g_free (elsewhere);
		}
	}
	g_free (qp);

/*	if (!(newstate || result == 0))
	{	//change back to where we started
		e2_pane_change_dir (curr_pane, olddir);
		g_free (olddir);
	}
*/
	NEEDOPENBGL
}
/**
@brief add check item to @a menu

Can't use e2_menu_add_check() because we don't want mnemonics

@param menu menu widget
@param label menu item text
@param state initial state of the created item, T/F
@param func void* callback function for handling menu item selection, or NULL
@param data data for callback

@return the menu item widget
*/
static GtkWidget *_e2_menu_mount_add_check (GtkWidget *menu,  gchar *label,
	gboolean state, void (*func)(GtkCheckMenuItem*,gpointer), gpointer data)
{
	GtkWidget *check = gtk_check_menu_item_new_with_label (label);
	gtk_menu_shell_append (GTK_MENU_SHELL (menu), check);
	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (check), state);
	if (func != NULL)
		g_signal_connect (G_OBJECT (check), "toggled", G_CALLBACK (func), data);
	return check;
}

static GtkWidget *selectedmount = NULL;
/**
@brief "select" signal callback to log which item in mounts-menu is current
@param item the slected menu-item
@param user_data UNUSED
@return
*/
void _e2_menu_mount_select_cb (
#ifdef USE_GTK3_0
	GtkMenuItem *item,
#else
	GtkItem *item,
#endif
	gpointer user_data)
{
//	CLOSEBGL
	selectedmount = GTK_WIDGET(item);
//	OPENBGL
}
/**
@brief key press signal callback to insert into the commandline the path of the selected item in @a menu
@param menu the focused menu-widget when the key was pressed
@param event pointer to event data
@param user_data UNUSED
@return
*/
static gboolean _e2_menu_mount_keypress_cb (GtkWidget *menu,
	GdkEventKey *event, gpointer user_data)
{
	printd (DEBUG, "_e2_menu_mount_keypress_cb");
	if (event->keyval == GDK_Insert)
	{
		const gchar *path = g_object_get_data (G_OBJECT(selectedmount), "_mountpoint_");
		NEEDCLOSEBGL
		e2_command_line_insert (path);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief create and pop up a onetime mountpoints menu

@param from the button that was clicked to popup the menu, maybe a toolbar button
@param art action runtime data

@return TRUE if the action succeeded
*/
gboolean e2_menu_create_mounts_menu (gpointer from, E2_ActionRuntime *art)
{
	gchar *point;
	GList *mounts, *mountables, *member, *node;
	GtkWidget *item;

	if (GTK_IS_BUTTON (from))
	{
		art->state &= ~GDK_MOD2_MASK;
		if (!ACTION_CLICK(art))
			return FALSE;
	}

	GtkWidget *menu = e2_menu_get ();

	mounts = e2_fs_mount_get_mounts_list ();
	for (member = mounts ; member != NULL; member = member->next)
	{
		item = _e2_menu_mount_add_check (menu, (gchar *) member->data, TRUE,
				_e2_menu_mount_cb, from);
		g_signal_connect (G_OBJECT (item), "select",
			G_CALLBACK (_e2_menu_mount_select_cb), NULL);
		g_object_set_data_full (G_OBJECT (item), "_mountpoint_",
			member->data, g_free);
	}

	mountables = e2_fs_mount_get_mountable_list ();
	for (member = mountables ; member != NULL; member = member->next)
	{
		point = (gchar *) member->data;
		gboolean mounted = FALSE;
		for (node = mounts; node != NULL; node = node->next)
		{
			if (!strcmp (point, (gchar *) node->data))
			{
				mounted = TRUE;
				break;
			}
		}
		if (mounted)
			g_free (member->data);
		else
		{
			item = _e2_menu_mount_add_check (menu, (gchar *) member->data,
					FALSE, _e2_menu_mount_cb, from);
			g_signal_connect (G_OBJECT (item), "select",
				G_CALLBACK (_e2_menu_mount_select_cb), NULL);
			g_object_set_data_full (G_OBJECT (item), "_mountpoint_",
				member->data, g_free);
		}
	}

	g_list_free (mounts);	//its data are cleared with the menu
	g_list_free (mountables); //ditto

	g_signal_connect (G_OBJECT (menu), "key-press-event",
		G_CALLBACK(_e2_menu_mount_keypress_cb), NULL);
	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	gtk_widget_show_all (menu);
	if (GTK_IS_BUTTON (from))
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_toolbar_set_menu_position, from, 1, 0);
	else
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 1, 0);

	return TRUE;
}
#endif	//def E2_FS_MOUNTABLE

/**
@brief timer callback to execute action corresponding to item selected from a menu
We do this outside the manu-item "activated" callback to avoid disrupting gtk's
menu-activation process
@param cmd pointer to command-string (which may be cleared during menu destruction)

@return FALSE to remove the source
*/
static gboolean _e2_menu_run_filetype_action (const gchar *cmd)
{
	printd (DEBUG, "timer callback: context menu run filetype action");
	CLOSEBGL
	e2_filetype_exec_action (cmd);
	OPENBGL
	e2_utils_fake_event (); //CHECKME does this actually help?
	return FALSE;
}
/**
@brief setup to execute action corresponding to item selected from filetype tasks menu
This is the callback for handling a selection of a filetype action from the
context menu. To avoid disrupting gtk's menu-activation process, we do not run
the action here.
@param item the activated menu item widget
@param cmd pointer to command-string (which may be cleared during menu destruction)

@return
*/
static void _e2_menu_choose_filetype_action_cb (GtkMenuItem *item, const gchar *cmd)
{
	printd (DEBUG, "context menu choose filetype action cb");
//	CLOSEBGL
	//an idle-callback is no good !?
	g_timeout_add (100, (GSourceFunc) _e2_menu_run_filetype_action, (gpointer)cmd);
//	OPENBGL
}
/**
@brief helper for e2_menu_fill_desktop_actions()

@param menu the menu widget to which the action menu-items are to be added
@param key_file loaded data for a relevant .desktop file

@return
*/
static void _e2_menu_fill_desktop_actions (GtkWidget *menu, GKeyFile *key_file)
{
	gsize count = 0;
	gchar **actions = g_key_file_get_string_list (key_file, "Desktop Entry",
		"Actions", &count, NULL);
	if (count > 0)
	{
		gchar **iterator;
		for (iterator = actions; *iterator != NULL; iterator++)
		{
			gchar *group = g_strconcat ("Desktop Action ", *iterator, NULL);
			if (g_key_file_has_group (key_file, group))
			{
				gchar *command;
				command = g_key_file_get_string (key_file, group, "Exec", NULL);
				if (command != NULL)
				{
					gchar *label;
					GtkWidget *item;
					label = g_key_file_get_string (key_file, group, "Name", NULL);
					if (label == NULL)
						label = command;
					item = e2_menu_add (menu, label, NULL, NULL,
						_e2_menu_choose_filetype_action_cb, command);
					//cleanup during menu destruction, whether or not this item activated
					g_object_set_data_full (G_OBJECT(item), "action-cmd-key",
							command, g_free);
					if (label != command)
						g_free (label);
				}
			}
			g_free (group);
		}
		g_strfreev (actions);
	}
}
/**
@brief populate @a menu with items for the desktop actions for an executable file

@param menu the menu widget to which the action menu-items are to be added
@param appname localised name of an executable item, whose .desktop file is to be found and parsed

@return
*/
void e2_menu_add_desktop_actions (GtkWidget *menu, const gchar *appname)
{
	GKeyFile *key_file = g_key_file_new ();
	gchar *desktopname = e2_utils_strcat (appname, ".desktop");
	const gchar *udatadir = g_get_user_data_dir ();
	if (udatadir != NULL)
	{
		gchar *path = g_build_filename (udatadir, "applications", desktopname, NULL);
		if (g_key_file_load_from_file (key_file, path, G_KEY_FILE_NONE, NULL))
		{
			g_free (path);
			g_free (desktopname);
			_e2_menu_fill_desktop_actions (menu, key_file);
			g_key_file_free (key_file);
			return;
		}
		g_free (path);
	}
	const gchar* const *datadir = g_get_system_data_dirs ();
	while (*datadir != NULL)
	{
		gchar *path = g_build_filename (*datadir, "applications", desktopname, NULL);
		if (g_key_file_load_from_file (key_file, path, G_KEY_FILE_NONE, NULL))
		{
			g_free (path);
			_e2_menu_fill_desktop_actions (menu, key_file);
			break;
		}
		g_free (path);
		datadir++;
	}
	g_free (desktopname);
	g_key_file_free (key_file);
}
/**
@brief helper for e2_menu_fill_desktop_mime()

@param menu the menu widget to which the menu-items are to be added
@param key_file loaded data for a relevant .desktop file

@return
*/
static void _e2_menu_fill_desktop_mime (GtkWidget *menu, GKeyFile *key_file)
{
	gchar *command = g_key_file_get_string (key_file, "Desktop Entry", "Exec", NULL);
	if (command != NULL) //probably has appended " %X" where X is U,F...
	{
		gchar *sep = strchr (command, '%');
		if (sep != NULL)
		{
			*sep = '\0';
			while (*--sep == ' ') *sep = '\0';
		}
		gchar *label = g_key_file_get_string (key_file, "Desktop Entry", "Name", NULL);
		if (label == NULL)
			label = command;
		GtkWidget *item = e2_menu_add (menu, label, NULL, NULL,
			_e2_menu_choose_filetype_action_cb, command);
		//cleanup during menu destruction
		g_object_set_data_full (G_OBJECT(item), "action-cmd-key", command, g_free);
		if (label != command)
			g_free (label);
	}
}
/**
@brief populate @a menu with items which can open a file of type @a mimetype

@param menu the menu widget to which the menu-items are to be added
@param mimetype type of item as reported by xdg-mime command, may be compound
  like "text/plain; charset=whatever"

@return
*/
void e2_menu_add_desktop_mime (GtkWidget *menu, const gchar *mimetype)
{
	gchar *sep, *command, *desktop;
	sep = strchr (mimetype, ';');
	if (sep != NULL)
		*sep = '\0';
	command = e2_utils_strcat ("xdg-mime query default ", mimetype);
	if (sep != NULL)
		*sep = ';';

	if (e2_fs_get_command_output (command, (gpointer *) &desktop))
	{
		gchar *path;
		GKeyFile *key_file = g_key_file_new ();
		g_strstrip (desktop); //no trailing newline

		const gchar *udatadir = g_get_user_data_dir ();
		if (udatadir != NULL)
		{
			path = g_build_filename (udatadir, "applications", desktop, NULL);
			if (g_key_file_load_from_file (key_file, path,  G_KEY_FILE_NONE, NULL))
			{
				g_free (desktop);
				g_free (path);
				g_free (command);
				_e2_menu_fill_desktop_mime (menu, key_file);
				g_key_file_free (key_file);
				return;
			}
			g_free (path);
		}
		const gchar* const *datadir = g_get_system_data_dirs ();
		while (*datadir != NULL)
		{
			path = g_build_filename(*datadir, "applications", desktop, NULL);
			if (g_key_file_load_from_file (key_file, path, G_KEY_FILE_NONE, NULL))
			{
				g_free (path);
				_e2_menu_fill_desktop_mime (menu, key_file);
				break;
			}
			g_free (path);
			datadir++;
		}
		g_free (desktop);
		g_key_file_free (key_file);
	}
	g_free (command);
}
/**
@brief add system-defined handlers of an item into @a menu
If @a localpath is executable, then relevant action(s) from a matching .desktop
file (if any) are added to @a menu. If not executable, then item(s) for
applications which the system says can open @a localpath are added.
@param menu the menu widget to which the menu-items are to be added
@param localpath string, localised path of item to be evaluated

@return
*/
void e2_menu_add_filehandlers (GtkWidget *menu, const gchar *localpath)
{
	gboolean exec;
#ifdef E2_VFS
# ifdef E2_VFSTMP
	//CHECKME allow exec options for non-local items ?
# endif
	VPATH ddata = { localpath, NULL };
	if (!e2_fs_item_is_mounted (&data))
		exec = FALSE;
	else
		exec = e2_fs_is_exec2 (&ddata E2_ERR_NONE());
#else
	exec = e2_fs_is_exec2 (localpath E2_ERR_NONE());
#endif
	if (exec)
	{
		//add any relevant desktop entry actions
		gchar *appname = g_path_get_basename (localpath);
		e2_menu_add_desktop_actions (menu, appname);
		g_free (appname);
	}
	else
	{
		gchar *mimetype;
#ifdef E2_VFS
		mimetype = e2_utils_get_mimetype (&ddata);
#else
		mimetype = e2_utils_get_mimetype (localpath);
#endif
		if (mimetype != NULL)
		{
			e2_menu_add_desktop_mime (menu, mimetype);
			g_free (mimetype);
		}
	}
}
/**
@brief populate @a menu with items for the actions for a filetype
Each member of @a actions is like "command" or "label@command"
@param menu the menu widget to which the action menu-items are to be added
@param actions NULL-terminated array of utf8 strings, each a command for a filetype

@return
*/
static void _e2_menu_fill_filetype_actions_menu (GtkWidget *menu,
	const gchar **actions)
{
	//overhead, in case <system default> is present
	FileInfo *info = e2_fileview_get_selected_first_local (curr_view, FALSE);
	gchar *localpath = e2_utils_dircat (curr_view, info->filename, TRUE);

	while (*actions != NULL)
	{
		gchar *s, *sys, *cmd;
		GtkWidget *menu_item;

		if ((s = strchr (*actions, '@')) != NULL)  //if always ascii @, don't need utf8 scan
		{
			cmd = s + 1;
			sys = e2_task_system_command (cmd, localpath);
			if (sys != NULL)
				cmd = sys;
			*s = '\0';
			menu_item = e2_menu_add (menu, (gchar *)*actions, NULL, NULL,
				_e2_menu_choose_filetype_action_cb, cmd);
			*s = '@';	//revert to original form (this is the 'source' data)
		}
		else //should probably never happen
		{
			gchar *label;

			cmd = (gchar *)*actions;
			sys = e2_task_system_command (cmd, localpath);
			if (sys != NULL)
				cmd = sys;
			s = strchr (cmd, ' ');
			if (s != NULL)
				label = g_strndup (cmd, s-cmd);
			else
				label = cmd;
			menu_item = e2_menu_add (menu, label, NULL, NULL,
				_e2_menu_choose_filetype_action_cb, cmd);
			if (label != cmd)
				g_free (label);
		}
		//some open-with action code needs the command, from the menu item
		if (sys != NULL)
			g_object_set_data_full (G_OBJECT(menu_item), "action-cmd-key", sys, g_free);
		else
 			g_object_set_data_full (G_OBJECT(menu_item), "action-cmd-key", cmd, NULL);
		actions++;
	}

	g_free (localpath);
}
/**
@brief add to @a menu items based on filetypes data for the focused item in active pane

@param menu the menu widget to which the menu-items are to be added
@param view data struct for the filelist pane to which the menu applies

@return
*/
void e2_menu_add_filetype_items (GtkWidget *menu, ViewInfo *view)
{
	gboolean exec, freeext = TRUE;
	gchar *viewpath, *ext, *ext2;
	const gchar **actions = NULL;
	FileInfo *info;
	GtkTreeModel *mdl = view->model;
	GtkTreeIter iter;
	if (gtk_tree_model_iter_nth_child (mdl, &iter, NULL, view->row))
		gtk_tree_model_get (mdl, &iter, FINFO, &info, -1);
	else
		return;

	viewpath = F_FILENAME_TO_LOCALE (view->dir);
#ifdef E2_VFS
	VPATH ddata = { viewpath, view->spacedata };
	if (e2_fs_is_dir (&ddata, info))
#else
	if (e2_fs_is_dir (viewpath, info))
#endif
	{
		exec = FALSE;	//no special treatment for dirs with X permission
		ext = g_strconcat (".", _("<directory>"), NULL);
	}
	else
	{
		gchar *localpath = g_build_filename (viewpath, info->filename, NULL);
#ifdef E2_VFS
		ddata.path = localpath;
		ddata.spacedata = NULL;
# ifdef E2_VFSTMP
		//CHECKME allow exec options for non-local items ?
# endif
		if (view->spacedata != NULL)
			exec = FALSE;//CHECKME commands for non-local items ?
		else
			exec = e2_fs_is_exec2 (&ddata E2_ERR_NONE());
#else
		exec = e2_fs_is_exec2 (localpath E2_ERR_NONE());
#endif
		freeext = FALSE;
/*		//FIXME generalise detection of hidden items
		if (!e2_fs_is_hidden (&ddata, E2_ERR_NONE()))
			ext = info->filename; //look for extension from 1st byte
//USELESS else if (*info->filename != '.')
//			ext = info->filename;
//UNREAL else if (*info->filename == '\0')
//			ext = info->filename;
		else // *info->filename == '.', a *NIX-kind of hidden file
			ext = info->filename + sizeof (gchar); //look for extension from 2nd byte
*/
		ext = info->filename;
		if (*ext == '.')
			ext++;
		//assumes extension is that part of the name after the leftmost '.'
		ext = strchr (ext, '.');
		if (ext == NULL || *(ext + sizeof (gchar)) == '\0') //no extension
		{
			if (/*!exec &&*/ e2_fs_is_text (
#ifdef E2_VFS
				&ddata
#else
				localpath
#endif
				E2_ERR_NONE())) //runs 'file' again!!
				//fake text extension
				//too bad if this is not a recognised text extension in filetypes
				ext = ".txt";
		}
		else //(ext != NULL && *(ext + sizeof (gchar)) != '\0'
		{
			ext2 = ext;
			ext = F_DISPLAYNAME_FROM_LOCALE (ext);	//get utf
			if (ext != ext2)	//conversion actually happened
				freeext = TRUE;	//so free ext before exit
		}
		g_free (localpath);
	}
	F_FREE (viewpath, view->dir);

	if (ext != NULL)
	{
		ext2 = ext;	//remember what to free, if need be
		//check all possible extensions for a matching filetype
		do
		{
			//skip leading dot "."
			ext += sizeof (gchar);	//ascii '.' always single char
			actions = e2_filetype_get_actions (ext);
			if (actions != NULL)
			{
				_e2_menu_fill_filetype_actions_menu (menu, actions);
				break;
			}
		} while ((ext = strchr (ext, '.')) != NULL);	//always ascii '.', don't need g_utf8_strchr()

		if (freeext)
			g_free (ext2);
	}

	if (exec)
	{
		//add exec-filetype items unless item has been found in that type already
		const gchar **acts2 = e2_filetype_get_actions (_("<executable>"));
		if (actions != NULL //was a matching extension
			&& actions != acts2 && acts2 != NULL) //CHECKME this test
				_e2_menu_fill_filetype_actions_menu (menu, acts2);
		else if (acts2 != NULL)
			_e2_menu_fill_filetype_actions_menu (menu, acts2);
	}
}

#if 0 //def USE_GTK3_10
/**
@brief This is a "leave-notify-event" signal callback for all gtk 3.10+ menus

@return	FALSE always so the event is propagated
*/
gboolean _e2_menu_exited_cb (GtkWidget *menu, GdkEventCrossing *event, gpointer usedrdata)
{
	printd (DEBUG, "Exited menu");
	return FALSE;
}
/**
@brief get menu widget with some gtk3 workarounds applied
@return the created menu widget, unshown
*/
GtkWidget *e2_menu_get (void)
{
	GtkWidget *menu = gtk_menu_new ();
	g_signal_connect (G_OBJECT (menu), "leave-notify-event",
		G_CALLBACK (_e2_menu_exited_cb), NULL);
	return menu;
}
#endif

/**
@brief install default tree options for custom menus
This function is called only if the default is missing from the config file
This default set data is for example purposes only, the user must provide real
data
@param set pointer to set data
@return
*/
static void _e2_menu_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("custom-menus=<"),  //internal name
	g_strdup("fusemounts||||"),	//menu lookup name
	//these are examples only, no translation
	g_strconcat ("\t|vfs_on"E2ICONTB"|","_Mount","|","Mount tip","|","FTPDIR=/whatever;mount-command $FTPDIR",NULL),
	g_strconcat ("\t|vfs_off"E2ICONTB"|","_Unmount choices","||",NULL),
	g_strconcat ("\t\t||","_Unmount","|","Unount tip","|","fusermount -u $FTPDIR",NULL),
	g_strdup(">"),
	NULL);
}
/**
@brief setup tree option for custom menus

@return
*/
void e2_menu_custom_option_register (void)
{
	gchar *group_name = g_strconcat(_C(20) ,".",_C(9),NULL);  //_("interface.custom menus";
	E2_OptionSet *set = e2_option_tree_register ("custom-menus", group_name, _C(9), //no translation
		NULL, NULL, NULL, E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_BUILDBARS);
	e2_option_tree_add_column (set, _("Menu"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	//since this a a new addition to config files, put the icon before the label
	e2_option_tree_add_column (set, _("Icon"), E2_OPTION_TREE_TYPE_ICON, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Label"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Tooltip"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Command"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_TOGGLE));
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_menu_tree_defaults);
}
