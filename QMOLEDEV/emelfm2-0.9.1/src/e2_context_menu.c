/* $Id: e2_context_menu.c 3067 2014-02-16 06:29:27Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

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
/* 09-20-2000: Modified by Aurelien Gateau
            Added code for Named actions for filetypes
*/

/**
@file src/e2_context_menu.c
@brief file-pane context menu functions

Some of the functions are used in other contexts eg toolbar menus
Other context menus are handled in the same file as their 'context'.
*/
/**
\page menu the filelist context menu

ToDo -describe how this works
*/
/**
\page othermenu other context menus

ToDo
*/

#include "e2_context_menu.h"
#include <string.h>
#include "e2_filelist.h"
#include "e2_filetype.h"

  /*******************/
 /**** callbacks ****/
/*******************/

/*
static gboolean _e2_context_menu_item_click_cb (GtkWidget *menuitem,
	GdkEventButton *event, E2_ActionRuntime *art)
{
	printd (DEBUG, "item_click_cb (widget:_,event:_,rt:_)");
	if (event->button == 1 && event->type == GDK_BUTTON_PRESS
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		NEEDCLOSEBGLX
		gtk_widget_hide (app.context_menu);
		OPENBGL
		e2_action_run (menuitem, art);
		CLOSEBGL
		e2_action_free_runtime (art);
		return TRUE;
	}
	e2_action_free_runtime (art);
	return FALSE;
}
*/
/**
@brief context-menu "selection-done" signal callback
This will be called _before_ running the action initiated by the selected item,
but the big delay ensures menu persists until after any processing
@param menu the menu widget (= app.context_menu)
@param data data specified when the callback was connected

@return
*/
static void _e2_context_menu_selected_cb (GtkWidget *menu, gpointer data)
{
	printd (DEBUG, "_e2_context_menu_selected_cb, menu: %x", menu);
	//must not destroy the menu immediately, downstream and/or gtk upstream
	//have not necessarily finished with it
	//FIXME delay at least until downstream action has finished running + 1 sec
#ifdef USE_GLIB2_14
	g_timeout_add_seconds (5,
#else
	g_timeout_add (5000,
#endif
		(GSourceFunc)e2_menu_destroy, menu);
	app.context_menu = NULL;
}

  /*******************/
 /**** utilities ****/
/*******************/

/**
@brief populate context menu @a menu for the file pane represented by @a view, using data from @a model
Assumes that pane filelist is not being refreshed, upon arrival here, and that it
can't be refreshed while processing here
@param menu the context-menu widget
@param model model for context-menu data treestore
@param iter pointer to iter used to interrogate @a model
@param level the current depth in @a model
@param mtype enumerator for type of menu, 0=full ... 3=plugins only
@param view pointer to data struct for the filelist for which the menu is being created

@return
*/
static void _e2_context_menu_add_items (GtkWidget *menu, GtkTreeModel *model,
	GtkTreeIter *iter, gint mtype, ViewInfo *view)
{
	gboolean selected =
		(gtk_tree_selection_count_selected_rows (view->selection) > 0);
	if (mtype == 3)
	{	// shift+ctrl menu
		e2_menu_create_plugins_menu (menu, TRUE, selected);
		return;
	}

	gchar *prefix = g_strconcat (_A(6), ".", NULL);
	do
	{
		gchar *label, *icon, *command, *arg;
		gboolean _mtype;
		switch (mtype)
		{
			case 1:
				// shift menu
				gtk_tree_model_get (model, iter, 0, &label, 1, &icon, 2, &_mtype, 4, &command, 5, &arg, -1);
				break;
			case 2:
				// ctrl menu
				gtk_tree_model_get (model, iter, 0, &label, 1, &icon, 3, &_mtype, 4, &command, 5, &arg, -1);
				break;
			default:
				gtk_tree_model_get (model, iter, 0, &label, 1, &icon,             4, &command, 5, &arg, -1);
				break;
		}
		if (selected || !g_str_has_prefix (command, prefix))
		{
			if ((mtype != 0) && !_mtype)   //CHECKME should be _mtype
			{
				GtkTreeIter iter2;
				if (gtk_tree_model_iter_children (model, &iter2, iter))
					//recurse
					_e2_context_menu_add_items (menu, model, &iter2, mtype, view);
			}
			else
			{
				gchar *realarg;
				E2_Action *action = e2_action_get_with_custom (command, arg, &realarg);
				switch (action->type)
				{
					case E2_ACTION_TYPE_SUBMENU:
					{
						GtkWidget *submenu;
						if (arg == NULL || *arg == '\0') //no data == not a custom sub-menu
						{
							GtkTreeIter iter2;
							if (gtk_tree_model_iter_children (model, &iter2, iter))
							{
								submenu = e2_menu_get ();
								//recurse
								_e2_context_menu_add_items (submenu, model, &iter2, mtype, view);
							}
							else
								submenu = NULL;
						}
						else	//custom submenu
							submenu = e2_menu_create_custom_menu (arg);	//may be NULL

//						GtkWidget *item = e2_menu_add (menu, label, icon, NULL, NULL, NULL);
						if (submenu != NULL)
						{
							GList *children = gtk_container_get_children (GTK_CONTAINER (submenu));
							if (g_list_length (children) == 0)
								gtk_widget_destroy (submenu);
							else
							{
								GtkWidget *item = e2_menu_add (menu, label, icon, NULL, NULL, NULL);
								gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
							}
							g_list_free (children);
						}
					}
						break;
					case E2_ACTION_TYPE_BOOKMARKS:
					{
						GtkTreeIter iter;
						gint pane = E2PANECUR;	//CHECKME always ok for context menu ? action may be specific
						E2_OptionSet *set = e2_option_get ("bookmarks");
						if (arg != NULL && *arg != '\0')
						{
							if (e2_tree_find_iter_from_str (set->ex.tree.model, 0, arg, &iter, TRUE))
							{
								GtkTreeIter iter2;
								if (gtk_tree_model_iter_children (set->ex.tree.model, &iter2, &iter))
									e2_menu_add_bookmark_items (menu, menu, action,
										E2_MARKFLAG_ONETIME, pane, set->ex.tree.model, &iter2);
							}
						}
						else if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
								e2_menu_add_bookmark_items (menu, menu, action,
									E2_MARKFLAG_ONETIME, pane, set->ex.tree.model, &iter);
					}
						break;
					case E2_ACTION_TYPE_HISTORY:
					{
						E2_PaneRuntime *rt = (view == curr_view) ? curr_pane : other_pane;
						GtkWidget *submenu = e2_pane_visited_menu (rt);
						GtkWidget *item = e2_menu_add (menu, label, icon, NULL, NULL, NULL);
						gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
					}
					break;
					case E2_ACTION_TYPE_FILE_ACTIONS:
						e2_menu_add_filetype_items (menu, view);
						break;
					case E2_ACTION_TYPE_FILE_HANDLERS:
					{
						GtkTreeIter iter2;
						FileInfo *info;
						GtkTreeModel *mdl = view->model;
						if (gtk_tree_model_iter_nth_child (mdl, &iter2, NULL, view->row))
							gtk_tree_model_get (mdl, &iter2, FINFO, &info, -1);
						{
							gchar *infopath = F_FILENAME_TO_LOCALE (view->dir);
#ifdef E2_VFS
							VPATH sdata = { infopath, view->spacedata };
							if (!e2_fs_is_dir (&sdata, info))
#else
							if (!e2_fs_is_dir (infopath, info))
#endif
							{
								gchar *localpath = g_build_filename (infopath, info->filename, NULL);
								e2_menu_add_filehandlers (menu, localpath);
								g_free (localpath);
							}
							F_FREE (infopath, view->dir);
						}
					}
						break;
					case E2_ACTION_TYPE_PLUGINS:
						e2_menu_create_plugins_menu (menu, TRUE, selected);
						break;
					case E2_ACTION_TYPE_SEPARATOR:
					{
						GList *children = gtk_container_get_children (GTK_CONTAINER (menu));
						if (g_list_length (children) > 0)
							e2_menu_add_separator (menu);
						g_list_free (children);
					}
						break;
					case E2_ACTION_TYPE_TEAR_OFF_MENU:
#ifdef USE_GTK3_4
						e2_menu_add (menu, label, icon, NULL, NULL, NULL); //useless, really
#else
						e2_menu_add_tear_off (menu);
#endif
						break;
					case E2_ACTION_TYPE_TOGGLE:
						e2_menu_add_toggle (menu, TRUE, label, icon, NULL, command, arg);
						break;
					case E2_ACTION_TYPE_ITEM:
					{
						E2_ActionRuntime *art;
						art = e2_action_pack_runtime (action, realarg, g_free);
						if (art != NULL)
						{
							GtkWidget *item = e2_menu_add (menu, label, icon, NULL,
								e2_menu_action_activated_cb, NULL);
							g_object_set_data_full (G_OBJECT(item), "e2-actruntime",
								art, (GDestroyNotify)e2_action_free_runtime);
							//FIXME add tip to config data and from there to this item
						}
					}
						break;
					default:
						break;
				}
				if (action->type != E2_ACTION_TYPE_ITEM)
					g_free (realarg);
			}
		}
		//cleanup
		g_free (label);
		g_free (icon);
		g_free (command);
		g_free (arg);
	} while (gtk_tree_model_iter_next (model, iter));
	g_free (prefix);
}

  /******************/
 /***** public *****/
/******************/

/**
@brief create and pop up destroyable context-menu for the filelist pane associated with @a view
Returns immediately if the view is being refreshed or changed upon arrival here
@param button initiator enumerator, 0-menu-key or action, 1=L etc
@param time time at which event was triggered
@param type menu type enumerator 0=full menu, 1=Shft, 2=Ctrl, 3=Shft+Ctrl
@param view pointer to view data struct

@return
*/
void e2_context_menu_show (guint button, guint32 time, gint type, ViewInfo *view)
{
	GtkTreeIter iter;

	E2_ListChoice p = (view == curr_view) ? PANEACTIVE : PANEINACTIVE;
	if (g_atomic_int_get (&view->listcontrols.refresh_working) > 0 ||
		g_atomic_int_get (&view->listcontrols.cd_working) > 0)
		return;

	//one or more items in the menu may invoke downstream code which triggers
	//a filelist refresh - which we don't want until menu is complete
	e2_filelist_disable_one_refresh (p);

	E2_OptionSet *set = e2_option_get ("context-menu");
	if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
	{
		app.context_menu = e2_menu_get ();
		_e2_context_menu_add_items (app.context_menu, set->ex.tree.model, &iter,
			type, view);
		g_signal_connect (G_OBJECT (app.context_menu), "selection-done",
			G_CALLBACK (_e2_context_menu_selected_cb), NULL);

		if (button == 0)
			//this was a menu key press or specific action
			gtk_menu_popup (GTK_MENU (app.context_menu), NULL, NULL,
				(GtkMenuPositionFunc) e2_fileview_set_menu_position,
				view->treeview, 0, time);
		else
			//this was a button-3 click
			gtk_menu_popup (GTK_MENU (app.context_menu), NULL, NULL,
				NULL, NULL, button, time);
	}

	e2_filelist_enable_one_refresh (p);
}
/**
@brief show context-menu action
Create and show a context menu in the active pane
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
gboolean e2_context_menu_show_menu_action (gpointer from, E2_ActionRuntime *art)
{
	const gchar *arg = NULL;
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, &arg);
	if (rt == NULL)
		return FALSE;
	//swap active pane if needed
	if (rt != curr_pane)
	{
		if (GTK_IS_WIDGET (app.context_menu))
		{
			NEEDOPENBGL
			e2_menu_selection_done_cb (app.context_menu, NULL);
			NEEDCLOSEBGL
			app.context_menu = NULL;
		}
		e2_pane_activate_other ();
	}
	gint type = 0;
	if (ACTION_BUTTON (art,1) || ACTION_BUTTON (art,3))
	{
		if (ACTION_MASK (art,GDK_SHIFT_MASK))
			type = 1;
		else if (ACTION_MASK (art,GDK_CONTROL_MASK))
			type = 2;
//		else if (ACTION_MASK (art,GDK_MOD1_MASK))
//			type = ?;
	}
	if (type == 0 && arg != NULL)
	{
		gchar *tmp = g_strdup (arg);
		g_strstrip (tmp);
		if (!strcmp (tmp, _A(120)))	//_(shift
			type = 1;
		else if (!strcmp (tmp, _A(113)))	//_(ctrl
			type = 2;
		//CHECKME alt option here ?
		g_free (tmp);
	}
	e2_context_menu_show (0, 0, type, curr_view);

	return TRUE;
}
/**
@brief install default tree options for context menu
This function is called only if the default is missing from the config file
@param set pointer to set data
@return
*/
static void _e2_context_menu_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("context-menu=<"),  //internal name
	g_strconcat("||false|false|",_A(6),".",_A(23),"|",NULL),
	g_strconcat("||false|false|",_A(6),".",_A(26),"|",NULL),
	g_strconcat(_("Open _with.."),"|open_with"E2ICONTB"|false|false|",
		_A(6),".",_A(69),"|",NULL),
	g_strconcat(_("_View"),"|view"E2ICONTB"|false|false|",
		_A(6),".",_A(109),"|",NULL),
	g_strconcat(_("_Info"),"|gtk-dialog-info|false|false|",_A(6),".",_A(58),"|",NULL),
	g_strconcat("||false|false|",_A(21),"|",NULL),
	g_strconcat(_("_Actions"),"|gtk-execute|false|false|",_A(22),"|",NULL),
	g_strconcat("\t",_("_Copy"),"|gtk-copy|true|false|",_A(6),".",_A(39),"|",NULL),
	g_strconcat("\t",_("_Move"),"|move"E2ICONTB"|true|false|",_A(6),".",_A(65),"|",NULL),
	g_strconcat("\t",_("_Link"),"|symlink"E2ICONTB"|true|false|",_A(6),".",_A(99),"|",NULL),
	g_strconcat("\t",_("_Trash"),"|trash"E2ICONTB"|true|false|",_A(6),".",_A(18),"|",NULL),
	g_strconcat("\t","<span foreground='red'>",_("_Delete"),"</span>|delete"E2ICONTB"|true|false|",
		_A(6),".",_A(45),"|",NULL),
	g_strconcat("\t",_("_Rename.."),"|gtk-convert|true|false|",_A(6),".",_A(79),"|",NULL),
	g_strconcat("\t",_("Change _owners.."),"|owners"E2ICONTB"|true|false|",
		_A(6),".",_A(70),"|",NULL),
	g_strconcat("\t",_("Change _permissions.."),"|permissions"E2ICONTB"|true|false|",
		_A(6),".",_A(73),"|",NULL),
	g_strconcat("\t",_("Copy as.."),"|copy_as"E2ICONTB"|true|false|",_A(6),".",_A(40),"|",NULL),
	g_strconcat("\t",_("Move as.."),"|move_as"E2ICONTB"|true|false|",_A(6),".",_A(66),"|",NULL),
	g_strconcat("\t",_("Link as.."),"|symlink_as"E2ICONTB"|true|false|",_A(6),".",_A(100),"|",NULL),
	g_strconcat(_("_Plugins"),"|gtk-index|false|false|",_A(22),"|",NULL),
	g_strconcat("\t||false|false|",_A(16),".",_A(28),"|",NULL),
	g_strconcat("\t||false|false|",_A(21),"|",NULL),
	g_strconcat("\t",_("_Edit plugins.."),"|gtk-preferences|false|false|",_A(3),".",_C(34),"|",NULL),
	g_strconcat(_("_User commands"),"|user_commands"E2ICONTB"|false|false|",_A(22),"|",NULL),
	g_strconcat("\t",_("_Make new file.."),"|gtk-new|false|true|touch|'%{",_("Enter file name:"),"}'",NULL),	//_A(20)
	g_strconcat("\t",_("_Compare files"),"||false|true|>cmp|-s %f %F && echo \"",_("The files are identical"),"\"\\n \\|\\| echo \"",_("The files are different"),"\"\\n",NULL),  	//_A(20)
	g_strconcat("\t",_("Compare _directories"),"||false|true|diff|%d %D",NULL), 	//_A(20)
//	g_strconcat("\t",_("_Easytag"),"||false|true|easytag|%d &",NULL),  //_A(20) not common enough to be a default
	g_strconcat("\t",_("_Remove spaces"),"||false|true|>mv|%f `echo %f \\| sed -e 's/ //g'` 2>/dev/null &",NULL), 	//_A(20)
	g_strconcat("\t",_("_Split file.."),"||false|true|split|-b %{",_("Enter the piece-size (in kB):"),"}k %f %f_",NULL), //_A(20)
	g_strconcat("\t",_("Co_ncatenate files.."),"||false|true|cat|%f > '%{",_("Enter the name of the combined file:"),"}'",NULL), //_A(20)
	g_strconcat("\t",_("Show _usage"),"||false|true|du|-bs %f",NULL),
	g_strconcat("\t",_("_Free space"),"||false|true|>stat|-f %d \\| awk '/Blocks/ {printf \"%2.1f ", _("percent free"),"\",$5/$3*100}'",NULL),  //_A(20)
	g_strconcat("\t||false|true|",_A(21),"|",NULL),
	g_strconcat("\t",_("_Edit user commands.."),"|gtk-preferences|false|true|",_A(3),".",_A(34),"|",_C(8),NULL),
	g_strconcat(_("Ma_ke dir.."),"|gtk-open|false|false|",_A(1),".",_A(63),"|",NULL),  //same _ as main toolbar
	g_strconcat("||false|false|",_A(21),"|",NULL),
	g_strconcat(_("_Bookmarks"),"|bookmark"E2ICONTB"|false|false|",_A(22),"|",NULL),
	g_strconcat("\t",_("Add _top"),"|add_mark_top"E2ICONTB"|false|false|",_A(0),".",_A(31),"|",_A(122),NULL),
	g_strconcat("\t||false|false|",_A(0),".",_A(28),"|",NULL),
	g_strconcat("\t",_("Add _bottom"),"|add_mark_bottom"E2ICONTB"|false|false|",_A(0),".",_A(31),"|",NULL),
	g_strconcat("\t||false|false|",_A(21),"|",NULL),
	g_strconcat("\t",_("_Edit bookmarks.."),"|gtk-preferences|false|false|",_A(3),".",_C(1),"|",NULL),
	g_strconcat(_("_History"),"|gtk-jump-to|false|false|",_A(8),".",_A(28),"|",NULL),
	g_strconcat("||false|false|",_A(21),"|",NULL),
	g_strconcat(_("_Edit filetype.."),"|gtk-preferences|false|false|",_A(3),".",_A(48),"|",NULL),
	g_strdup(">"),	//allow this to be freed
	NULL);
}
/**
@brief register context-menu option

@return
*/
void e2_context_menu_options_register (void)
{
	//this option does not require a rebuild after config change
	gchar *group_name = g_strconcat (_C(33), ".", _C(8), NULL);
	E2_OptionSet *set = e2_option_tree_register ("context-menu", group_name, _C(8),  //_("context menu"
		NULL, NULL, NULL, E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
	e2_option_tree_add_column (set, _("Label"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Icon"), E2_OPTION_TREE_TYPE_ICON, 0, "",
		0, NULL, NULL);
	//"false" is public name, but is widely used internally, not translated
	e2_option_tree_add_column (set, _("Shift"), E2_OPTION_TREE_TYPE_BOOL, FALSE,"false",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Ctrl"), E2_OPTION_TREE_TYPE_BOOL, FALSE,"false",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Action"), E2_OPTION_TREE_TYPE_SEL, 0, _A(21),
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_MENU));
	e2_option_tree_add_column (set, _("Argument"), E2_OPTION_TREE_TYPE_SEL, 0, "",
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_MENU
		| E2_ACTION_EXCLUDE_TOGGLE));
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_context_menu_tree_defaults);
}
