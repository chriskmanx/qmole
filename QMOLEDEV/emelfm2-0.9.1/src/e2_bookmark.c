/* $Id: e2_bookmark.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelfm2.
emelfm2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/e2_bookmark.c
@brief bookmark functions
*/

#include "e2_bookmark.h"
#include <string.h>
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_icons.h"
#include "e2_option_tree.h"

static E2_OptionSet *bookmarks_set;

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief re-create all toolbars which have bookmarks
Way too comlicated to create mechanism to replace only specific
buttons and menus
@return
*/
static void _e2_bookmark_recreate_toolbars (void)
{
	E2_ToolbarData **thisbar;
	for (thisbar = app.bars; *thisbar != NULL; thisbar++)
	{
		if ((*thisbar)->rt->has_bookmarks) //no need to check here for bar presence
			e2_toolbar_recreate ((*thisbar)->rt);
	}
}
/**
@brief delete bookmark at @a iter
@param iter pointer to tree iter for row to be deleted
@return
*/
static void _e2_bookmark_delete_mark (GtkTreeIter *iter)
{
	ViewInfo *view = curr_view;
	e2_option_tree_del_direct (bookmarks_set, iter);
	_e2_bookmark_recreate_toolbars ();
	if (GTK_IS_MENU (app.context_menu))
		gtk_menu_popdown (GTK_MENU (app.context_menu));
	if (view != curr_view)
		e2_pane_activate_other ();
	gtk_widget_grab_focus (view->treeview);
}
/**
@brief refresh the bookmarks menus
@return
*/
static void _e2_bookmark_apply_new (void)
{
//FIXME = find a smarter way to do this
	_e2_bookmark_recreate_toolbars ();
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief (de)sensitize option tree buttons for selected option tree row
Config dialog page buttons are de-sensitized if the row is the toolbar 'parent'
@param selection pointer to selection
@param model UNUSED
@param path
@param path_currently_selected UNUSED
@param set data struct for the keybings option
@return TRUE always (the row is always selectable)
*/
static gboolean _e2_bookmark_tree_selection_check_cb (GtkTreeSelection *selection,
	GtkTreeModel *model, GtkTreePath *path,
	gboolean path_currently_selected, E2_OptionSet *set)
{
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (set->ex.tree.model, &iter, path))
	{
		gchar *label;
		gtk_tree_model_get (set->ex.tree.model, &iter, 0, &label, -1);
		gboolean result = strcmp (label, _C(41));
		g_free (label);
		GtkTreeView *view = gtk_tree_selection_get_tree_view (selection);
		NEEDCLOSEBGL
		e2_option_tree_adjust_buttons (view, result);
		NEEDOPENBGL
	}
	return TRUE;
}
/**
@brief ensure that cells in the same row as any category name are hidden

Checks whether @a iter in @a model has the name of the taskbar
'parent' in column 0. If so, the cell content is hidden,

@param model treemodel for the bookmarks option tree
@param iter pointer to iter with data for the current model row
@param cell cell renderer UNUSED
@param data pointer to data UNUSED
@return TRUE (cell is visible) if row is not the task bar parent
*/
static gboolean _e2_bookmark_visible_check_cb (GtkTreeModel *model,
	GtkTreeIter *iter, GtkCellRenderer *cell, gpointer data)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	gchar *label;
	gtk_tree_model_get (model, iter, 0, &label, -1);
	gboolean result = strcmp (label, _C(41));
	g_free (label);
	return result;
}
/**
@brief decide whether tree row is draggable
Checks whether column 0 of the current row has the string
which is the name of the "tool bar" 'parent' row.
If so, the row is not draggable
@param drag_source GtkTreeDragSource data struct
@param path tree path to a row on which user is initiating a drag
@return TRUE if the row can be dragged
*/
static gboolean _e2_bookmark_tree_draggable_check_cb (
	GtkTreeDragSource *drag_source, GtkTreePath *path)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	if (!GTK_IS_TREE_MODEL (drag_source))
		return TRUE;
	GtkTreeModel *model = GTK_TREE_MODEL (drag_source);
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (model, &iter, path))
	{
		gchar *label;
		gtk_tree_model_get (model, &iter, 0, &label, -1);
		gboolean result = strcmp (label, _C(41));
		g_free (label);
		return result;
	}
	return TRUE;
}
/**
@brief process result from delete-confirmation dialog
@param dialog UNUSED the confirmation-dialog
@param response the response from the dialog
@param ref treerow reference for the bookmark to be deleted
@return
*/
static void _e2_bookmark_confirm_response_cb (GtkDialog *dialog,
	gint response, GtkTreeRowReference *ref)
{
	NEEDCLOSEBGL
	if (response == GTK_RESPONSE_YES)
	{
		if (ref != NULL)
		{
			GtkTreePath *path = gtk_tree_row_reference_get_path (ref);
			if (path != NULL)
			{
				GtkTreeIter iter;
				if (gtk_tree_model_get_iter (bookmarks_set->ex.tree.model, &iter, path))
					_e2_bookmark_delete_mark (&iter);
				gtk_tree_path_free (path);
			}
		}
	}
	gtk_widget_destroy (GTK_WIDGET (dialog));
	NEEDOPENBGL
}
/**
@brief callback to add an item (directory) to the bookmarks list

The activated item @a widget may be a menu item (eg in the panebar bookmarks
sub-menu) or an actions-menu item (file pane or bookmarks widget). In the former
case, it will have been processed as an action, and will have a string argument
@a arg, with "pane1" or "pane2" or a specific UTF-8 path string, and possibly
including one of "top", "child" (othewise the add is treated as "after").
When @a widget is an actions-menu item, @a arg will be NULL.
This function needs to know which pane to use for the path to add to the list.
For menu callbacks, @a arg needs to include "1" or "2" as appropriate. For
context-menu callbacks, @a widget will have associated E2_MarkData* which
includes 0 (active), 1 or 2.

@param widget NULL if a bookmarks menu item widget was selected or running an
 action directly, else a context menu item widget
@param arg NULL, or string which indicates mark-directory and/or where in the marks
 list to put the new bookmark e.g. "child", mark directory may be like "N," where
 N = ''|0|1|2 followed by a comma
@return
*/
static void _e2_bookmark_add_cb (GtkMenuItem *widget, const gchar *arg)
{
	gint32 pane;
	gboolean before = FALSE;
	gboolean parent = FALSE;
	GtkTreePath *path;
	gchar *s = NULL;

	if (widget != NULL)
	{
		E2_MarkData *mdata = (E2_MarkData *) g_object_get_data (G_OBJECT (widget), "mark-data");
		if (mdata != NULL)
		{
			pane = mdata->pane_num;
			path = mdata->tpath;
		}
		else
		{
			pane = E2PANECUR;	//default to active pane
			path = NULL;
		}
	}
	else
	{
		pane = E2PANECUR;
		path = NULL;
	}
	//override pane if caller wants
	if (arg != NULL && *arg != '\0')
	{
		gchar *n;
		if (strstr (arg, _A(122)) != NULL)  //_(top
			before = TRUE;
		if (strstr (arg, _A(112)) != NULL) //_(child
		{
			before = TRUE;
			parent = TRUE;
		}

		if (strchr (arg, G_DIR_SEPARATOR) != NULL)
			pane = 9999;	//indicator that arg includes a path string
		else if ((s = e2_utils_bare_strchr ((gchar *)arg, '1')) != NULL)
		{
			n = e2_utils_pass_whitespace (s+1);
			pane = (n == NULL || *n == ',') ? E2PANE1 : 9999; //"1" or "1,[place]" signals pane 1
		}
		else if ((s = e2_utils_bare_strchr ((gchar *)arg, '2')) != NULL)
		{
			n = e2_utils_pass_whitespace (s+1);
			pane = (n == NULL || *n == ',') ? E2PANE2 : 9999; //"2" or "2,[place]" signals pane 2
		}
		else
		{
			n = e2_utils_pass_whitespace ((gchar *)arg);
			pane = (n == NULL || *n == ',') ? E2PANECUR : 9999; //"" or ",[place]" signals current pane
		}
	}

	//get the relevant dir to add to the marks list
	gchar *dirname;
	switch (pane)
	{
#ifdef E2_VFSTMP
	//FIXME dirs when not mounted local
#else
		case E2PANE1:
			dirname = app.pane1.view.dir;
			break;
		case E2PANE2:
			dirname = app.pane2.view.dir;
			break;
		case 9999:
			if (before)
			{
				//find and handle extra parameter and maybe ','
				//FIXME this is not robust enough
			}
			dirname = e2_utils_unquote_string (arg);
			if (dirname == NULL)
				return;
			g_strchug (dirname); //path may validly have trailing whitespace
			if (!g_path_is_absolute (dirname))
			{
				s = dirname;
				dirname = e2_utils_strcat (curr_view->dir, s);
				g_free (s);
			}
			break;
		default:
			dirname = curr_view->dir;
#endif
			break;
	}

	gchar *basename = g_path_get_basename (dirname);
	//double any '_' CHECKME what's this about ?
/*	if (strchr (basename, '_'))
	{
		gchar *freeme = basename;
		basename = e2_utils_str_replace (basename, "_", "__");
		g_free (freeme);
	} */
	void *options[4];
	options[0] = basename;	//menu label
	options[1] = STOCK_NAME_DIRECTORY;  //generic icon in case it's in taskbar
	//strip the trailing / for paths other than root
	gint len = strlen (dirname);
	if (len > 1)
	{
		if (pane != 9999)
		{
			s =  g_strdup (dirname); //non-NULL = freeme later
			*(s+len-1) = '\0';
		}
		else
			s = dirname;
		options[2] = s;
	}
	else
	{
		s = (pane != 9999) ? NULL : dirname; //non-NULL = freeme later
		options[2] = dirname;  //tip = path
	}
	options[3] = options[2]; //path
//	E2_OptionSet *set = e2_option_get ("bookmarks");  //internal name USE THE GLOBAL VALUE
	GtkTreeIter iter;
	GtkTreeIter iter2;
	if ((path != NULL) && gtk_tree_model_get_iter (bookmarks_set->ex.tree.model,
			&iter2, path))
		e2_option_tree_add (bookmarks_set, &iter, &iter2, !parent, before, 4, options);
	else
		//with no parent, the behaviour of "before" is vice versa
		e2_option_tree_add (bookmarks_set, &iter, NULL, FALSE, !before, 4, options);

	ViewInfo *view = curr_view;
	
	NEEDCLOSEBGL
	
	_e2_bookmark_recreate_toolbars (); //kludgy way to update bookmarks data in bars
	if (view != curr_view)
		e2_pane_activate_other ();
	gtk_widget_grab_focus (view->treeview);

	NEEDOPENBGL

	if (s != NULL)
		g_free (s);
	g_free (basename);
}
/**
@brief callback for deleting a bookmark (with any children)
Assumes BGL closed
@param widget UNUSED the widget which was activated
@param path treestore path of bookmark to be deleted
@return
*/
static void _e2_bookmark_delete_cb (GtkMenuItem *widget, GtkTreePath *path)
{
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (bookmarks_set->ex.tree.model, &iter, path))
	{
		NEEDCLOSEBGL
		gint children = gtk_tree_model_iter_n_children (bookmarks_set->ex.tree.model, &iter);
		if (e2_option_bool_get ("bookmarks-confirm-delete")
			|| ((children > 0) && (e2_option_bool_get ("bookmarks-confirm-multi-delete")))
		)
		{
			gchar *name;
			gtk_tree_model_get (bookmarks_set->ex.tree.model, &iter, 0, &name, -1);

			gchar *question;
			gchar *prompt = _("Are you sure that you want to delete the bookmark");
			//we fudge here on translating the trailing '?'
			if (children > 0)
				question = g_strdup_printf ("%s '<b>%s</b>' %s <b>%d %s</b> ?",
					prompt, name, _("and"), children, (children == 1) ? _("child") : _("children"));
			else
				question = g_strdup_printf ("%s '<b>%s</b>' ?", prompt, name);

			GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION, question,
				_("confirm bookmark delete"),
				(ResponseFunc)_e2_bookmark_confirm_response_cb,
				gtk_tree_row_reference_new (bookmarks_set->ex.tree.model, path));
			e2_dialog_set_negative_response (dialog, GTK_RESPONSE_NO);

			E2_Button no_btn;
			e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_KEEP);

			E2_Button yes_btn;
			e2_button_derive (&yes_btn, &E2_BUTTON_YES, BTN_YES_DELETE);

			e2_dialog_show (dialog, app.main_window, 0, &no_btn, &yes_btn, NULL);
			g_free (question);
			g_free (name);
		}
		else	//no confirmation
			_e2_bookmark_delete_mark (&iter); //changes active pane and focus-widget
		NEEDOPENBGL
	}
}
/**
@brief "selection-done" signal callback for items in bookmark-actions menu
@param menu the actions-menu widget
@param mdata data associated with the active imem of bookmarks (sub)menu
@return
*/
static void _e2_bookmark_change_selected_cb (GtkWidget *menu, E2_MarkData *mdata)
{
	e2_menu_selection_done_cb (menu, NULL); //clean up actions-menu
	if (mdata->top_menu != NULL)
	{
		NEEDCLOSEBGL
		gtk_menu_popdown (GTK_MENU (mdata->top_menu)); //hide marks-menu and all subs
		NEEDOPENBGL
		if (mdata->mark_flags & E2_MARKFLAG_ONETIME)
			e2_menu_selection_done_cb (mdata->top_menu, NULL); //clean up marks-menu
	}
}
/**
@brief "button-press-event" signal callback

Unmodified middle button will open mark in other pane, and make that pane active,
if the relevant options are in force.
Unmodified right button will create and pop up a destroyable context menu, if not
blocked (blocks will be in place for toolbar buttons, but not for (sub)menu items)
Context menu items 'add' and 'add-child' need to know which pane (directory) to
use for the mark

@param widget clicked button or menu item
@param event gdk event data struct
@param mdata pointer to bookmark data associated with a bookmark-menu item
@return TRUE (block further handlers) if certain btn 2 or 3 conditions are satisfied
*/
gboolean e2_bookmark_button_press_cb (GtkWidget *widget, GdkEventButton *event,
	E2_MarkData *mdata)
{
	printd (DEBUG, "e2_bookmark_button_press_cb");
#ifdef E2_MOUSECUSTOM
	if ((event->state & E2_MODIFIER_MASK) == 0)
	{
#endif
		if (event->button == 2)
		{
			if (e2_option_bool_get ("bookmarks-button2-other"))
			{
				gint other = (curr_pane == &app.pane1) ? 2 : 1;
				if (other != mdata->pane_num) //active pane's menu was clicked
				{
#ifdef E2_VFSTMP
	//FIXME bookmark parsing and setting for v-dirs
					if (0)
						e2_pane_change_space_byuri (other_pane, "some string");
#endif
					e2_pane_change_dir (other_pane, mdata->mark_path);	//mark string is UTF-8
					if (e2_option_bool_get ("bookmarks-focus-after-open"))
					{
						NEEDCLOSEBGL
						e2_pane_activate_other ();
						NEEDOPENBGL
					}
					return TRUE;
				}
			}
		}
		else if (event->button == 3)
		{
			if ((mdata->mark_flags & E2_MARKFLAG_ACTIONS) > 0)
			{	//actions-menu allowed for this widget
				gboolean parent;
				GtkWidget *menu, *menuitem;

				if (mdata->tpath != NULL)
				{
					GtkTreeIter iter;
					gtk_tree_model_get_iter (bookmarks_set->ex.tree.model, &iter, mdata->tpath);
					parent = gtk_tree_model_iter_has_child (bookmarks_set->ex.tree.model, &iter);
				}
				else
					parent = FALSE;

				menu = e2_menu_get ();
				//adding a child to an existing mark prevents its operation in the menu
				if (parent)
				{
					//must preface callback arg with ',' to ensure correct parsing
					gchar *arg = e2_utils_strcat (",", _A(112)); //_(child
					menuitem = e2_menu_add (menu, _("_Add child"), STOCK_NAME_INDENT,
						_("Bookmark the current directory a child of the selected item"),
						_e2_bookmark_add_cb, arg);
					g_object_set_data_full (G_OBJECT (menuitem), "__freeme", arg, g_free);
				}
				else
					menuitem = e2_menu_add (menu, _("_Add after"), STOCK_NAME_ADD,
						_("Bookmark the current directory after the selected item"),
						_e2_bookmark_add_cb, NULL);
				g_object_set_data (G_OBJECT (menuitem), "mark-data", mdata);

				e2_menu_add (menu, _("_Delete"), STOCK_NAME_DELETE,
					_("Delete the selected bookmark, and its children if any"),
					_e2_bookmark_delete_cb, mdata->tpath);

				g_signal_connect (G_OBJECT (menu), "selection-done",
					G_CALLBACK (_e2_bookmark_change_selected_cb), mdata);
				NEEDCLOSEBGL
				e2_menu_popup (menu, event->button, event->time);
				NEEDOPENBGL
				return TRUE;
			}
		}
#ifdef E2_MOUSECUSTOM
	}
#endif
	return FALSE;
}

  /*******************/
 /***** actions *****/
/*******************/

/**
@brief add item to bookmarks menu
@param from pointer to widget activated to initiate the action
@param art pointer to runtime data for the action
@return TRUE always
*/
static gboolean _e2_bookmark_add (gpointer from, E2_ActionRuntime *art)
{
/*	//this just checks syntax, really
	const gchar *position = NULL;
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, &position);
	if (rt == NULL)
		return FALSE;
*/
	NEEDOPENBGL
	_e2_bookmark_add_cb (NULL, (const gchar *) art->data);
	NEEDCLOSEBGL
	return TRUE;
}
/**
@brief remove item from bookmarks menu
@param from pointer to widget activated to initiate the action
@param art pointer to runtime data for the action
@return TRUE if the item was removed
*/
static gboolean _e2_bookmark_delete (gpointer from, E2_ActionRuntime *art)
{
	gchar *arg = (gchar *) art->data;
//	if (arg == NULL) arg = curr_viewe->dir without trailing /
	if (arg != NULL) //&& *arg != '\0')
	{
		//CHECKME handle "1" or "2" for pane dir ?
		gboolean retval = FALSE;
		gchar *clean = e2_utils_unquote_string (arg);
		if (clean == NULL)
			clean = arg;
		GtkTreeIter iter;
		while (e2_tree_find_iter_from_str (bookmarks_set->ex.tree.model, 3,
				clean, &iter, FALSE))
		{
			GtkTreePath *tpath;
			tpath = gtk_tree_model_get_path (bookmarks_set->ex.tree.model, &iter);
			NEEDOPENBGL
			_e2_bookmark_delete_cb (NULL, tpath);
			NEEDCLOSEBGL
			gtk_tree_path_free (tpath);
			retval = TRUE;
		}
		if (clean != arg)
			g_free (clean);
		return retval;
	}
	return FALSE;
}
/**
@brief construct and pop up destroyable bookmarks-menu, in specified pane
@param from pointer to widget activated to initiate the action
@param art pointer to runtime data for the action
@return TRUE if menu was created
*/
static gboolean _e2_bookmark_show (gpointer from, E2_ActionRuntime *art)
{
	if (GTK_IS_BUTTON (from))
	{
		art->state &= ~GDK_MOD2_MASK;
		if (!ACTION_CLICK(art))
			return FALSE;
	}
	E2_OptionSet *set = e2_option_get ("bookmarks");
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
	{
		gint32 pane;
		E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
		if (rt == curr_pane)	//CHECKME allow default regardless of actual pane ?
			pane = E2PANECUR;
		else
			pane = (rt == &app.pane1) ? E2PANE1 : E2PANE2;
		gchar *action_name = g_strconcat (_A(0),".",_A(28),NULL);
		E2_Action *action = e2_action_get (action_name);
		g_free (action_name);
		GtkWidget *menu = e2_menu_get ();
		e2_menu_add_bookmark_items (menu, menu, action,
			E2_MARKFLAG_ACTIONS | E2_MARKFLAG_ONETIME, pane, set->ex.tree.model, &iter);
		g_signal_connect (G_OBJECT (menu), "selection-done",
			G_CALLBACK (e2_menu_selection_done_cb), NULL);
		guint32 event_time = gtk_get_current_event_time ();
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_fileview_set_menu_position,
			rt->view.treeview, 0, event_time);
		return TRUE;
	}
	return FALSE;
}
/**
@brief open bookmark @a path, in pane @a num
Action data includes "0" (from "<widget>" action), "1" or "2" (pane number)
Action data includes path of directory to open, utf8 string
Downstream functions expect BGL to be closed
@param from pointer to widget activated to initiate the action
@param art pointer to runtime data for the action
@return TRUE if the cd is initiated
*/
static gboolean _e2_bookmark_open (gpointer from, E2_ActionRuntime *art)
{
	const gchar *newpath = NULL;
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, &newpath);
	if (rt == NULL || newpath == NULL)
		return FALSE;
	printd (DEBUG, "_e2_bookmark_open (path:%s)", newpath);
	//bookmark may be bad now ...
	if (e2_fs_cd_isok ((gchar *)newpath E2_ERR_NONE()))
	{
#ifdef E2_VFSTMP
		//FIXME handle case where pane is non-mounted
		if (0)
			e2_pane_change_space_byuri (rt, "someplace");
#endif
		e2_pane_change_dir (rt, newpath);
		if (rt != curr_pane && e2_option_bool_get ("bookmarks-focus-after-open"))
			e2_pane_activate_other ();
		return TRUE;
	}
	return FALSE;
}
/**
@brief open a dialog showing bookmarks config data
@param from pointer to widget activated to initiate the action
@param art pointer to runtime data for the action
@return TRUE if the dialog was created
*/
static gboolean _e2_bookmarks_configure (gpointer from, E2_ActionRuntime *art)
{
	return (
	e2_config_dialog_single ("bookmarks", _e2_bookmark_apply_new, TRUE) //internal name, no translation
	!= NULL);
}

  /******************/
 /***** public *****/
/******************/

/* this used only at session end, don't bother
void e2_bookmark_clean ()
{
//	e2_action_unregister (_A(24));	//_("<bookmark>"));
} */
/**
@brief register bookmark-related actions
@return
*/
void e2_bookmark_actions_register (void)
{
	E2_Action actions[] =
	{
	{g_strconcat(_A(0),".",_A(28),NULL), _e2_bookmark_open,      TRUE, E2_ACTION_TYPE_BOOKMARKS, E2_ACTION_EXCLUDE_ACCEL, NULL, NULL},
	{g_strconcat(_A(0),".",_A(31),NULL), _e2_bookmark_add,       TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(0),".",_A(45),NULL), _e2_bookmark_delete,    TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(13),".",_A(24),NULL),_e2_bookmark_show,      TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(3),".",_C(1),NULL),  _e2_bookmarks_configure,FALSE,E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	};
	guint i, count = sizeof (actions)/sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&actions[i]);
}
/**
@brief install default tree options for boookmarks
This function is called only if the default is missing from the config file
@param set pointer to set data
@return
*/
static void _e2_bookmark_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("bookmarks=<"),  //internal name
	g_strconcat(_C(41),"|||",NULL),	//needs to be same as arg to taskbar item - see e2_toolbar.c
	g_strconcat("\t","<b>",_("_home"),"</b>|gtk-home|$HOME|~",NULL),
//this next is too specific for a default value
//	g_strconcat("\t",_("down"),"||",_("/mnt/downloads"),"|"G_DIR_SEPARATOR_S"mnt"G_DIR_SEPARATOR_S"downloads",NULL),
	g_strconcat("\t",_("cdrom"),"|gtk-cdrom|"G_DIR_SEPARATOR_S"media"G_DIR_SEPARATOR_S"cdrom"
		"|"G_DIR_SEPARATOR_S"media"G_DIR_SEPARATOR_S"cdrom",NULL),
	g_strconcat(G_DIR_SEPARATOR_S"||",_("root"),"|"G_DIR_SEPARATOR_S,NULL),
	g_strconcat(_("home"),"||",_("Your home directory"),"|~",NULL),
	g_strconcat(_("media"),"||"G_DIR_SEPARATOR_S"media|"G_DIR_SEPARATOR_S"media",NULL),	//_I( no point in translating tip = path
	g_strconcat(_("mnt"),"||"G_DIR_SEPARATOR_S"mnt|"G_DIR_SEPARATOR_S"mnt",NULL),	//ditto
	g_strconcat(_("usr"),"||"G_DIR_SEPARATOR_S"usr|"G_DIR_SEPARATOR_S"usr",NULL),	//ditto
	g_strconcat(_("usr/local"),"||"G_DIR_SEPARATOR_S"usr"G_DIR_SEPARATOR_S"local"
		"|"G_DIR_SEPARATOR_S"usr"G_DIR_SEPARATOR_S"local",NULL),	//ditto
	g_strconcat(_("trash"),"||",_("default trash directory"),
		"|~"G_DIR_SEPARATOR_S".local"G_DIR_SEPARATOR_S"share"G_DIR_SEPARATOR_S"Trash"G_DIR_SEPARATOR_S"files",
		NULL),
	g_strdup(">"),
	NULL);
}
/**
@brief register bookmark-related options
@return
*/
void e2_bookmark_options_register (void)
{
//no screen rebuilds needed after any change to these options
//	E2_OptionSet * SET THE GLOBAL VALUE
	bookmarks_set = e2_option_tree_register ("bookmarks", _C(1), _C(1),  //_("bookmarks"
		NULL, _e2_bookmark_tree_selection_check_cb, _e2_bookmark_tree_draggable_check_cb,
		E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDBARS); //toolbar(s) typically include static marks-related items
	e2_option_tree_add_column (bookmarks_set, _("Label"), E2_OPTION_TREE_TYPE_STR, 0, G_DIR_SEPARATOR_S,
		0, NULL, NULL);
	e2_option_tree_add_column (bookmarks_set, _("Icon"), E2_OPTION_TREE_TYPE_ICON, 0, "",
		0, _e2_bookmark_visible_check_cb, NULL);
	e2_option_tree_add_column (bookmarks_set, _("Tooltip"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, _e2_bookmark_visible_check_cb, NULL);
	e2_option_tree_add_column (bookmarks_set, _("Path"), E2_OPTION_TREE_TYPE_STR, 0, G_DIR_SEPARATOR_S,
		0, _e2_bookmark_visible_check_cb, NULL);
	e2_option_tree_create_store (bookmarks_set);

	e2_option_tree_prepare_defaults (bookmarks_set, _e2_bookmark_tree_defaults);

	gchar* group_name = g_strconcat(_C(1),".",_C(27),":",_C(26),NULL);  //_("bookmarks.options:miscellaneous"
	e2_option_bool_register ("bookmarks-button2-other",
		group_name, _("open bookmark in other pane on middle-button click"),
		_("Clicking the middle mouse button on a bookmark will open it in the other file pane"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
	e2_option_bool_register ("bookmarks-focus-after-open",
		group_name, _("focus file pane after opening a bookmark in it"),
		_("After opening a bookmark in the inactive file pane, that pane will become the active one"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);

	group_name = g_strconcat(_C(1),".",_C(27),":",_C(7),NULL);  //_("bookmarks.options:confirmation"
	e2_option_bool_register ("bookmarks-confirm-delete",
		group_name, _("confirm any delete of a selected bookmark"),
		_("You will be asked to confirm, before deleting any bookmark"),
		"bookmarks-confirm-multi-delete", FALSE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
	e2_option_bool_register ("bookmarks-confirm-multi-delete",
		group_name, _("confirm any delete of multiple bookmarks"),
		_("You will be asked to confirm, before deleting any bookmark that has 'children'"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
}
