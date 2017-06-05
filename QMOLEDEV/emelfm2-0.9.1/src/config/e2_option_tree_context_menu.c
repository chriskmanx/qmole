/* $Id: e2_option_tree_context_menu.c 3030 2014-01-23 19:30:30Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 2004 Florian Zaehringer <flo.zaehringer@web.de>

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

/* Florian Zaehringer is the source of inspiration, and some of the code, here */

/**
@file src/config/e2_option_tree_context_menu.c
@brief context menu functions for config-dialog tree options
This file is #included in e2_option_tree.c
(i.e. this won't rebuild unless that is rebuilt)
*/

//prevent building of separate object file
#ifdef INCLUDED_IN_PARENT

#include "e2_plugins.h"

  /*******************/
 /***** private *****/
/*******************/

/**
@brief create treeview signature

This gets the name of the current option.
That string is used as a signature to confirm the correct treeview,
to enable pasting

@param treeview treeview to which the menu belongs

@return
*/
static const gchar *_e2_option_tree_menu_get_signature (GtkTreeView *treeview)
{
	GtkTreeModel *model = gtk_tree_view_get_model (treeview);
	E2_OptionSet *set = g_object_get_data (G_OBJECT (model), "e2-option-set");
	//all toolbars have same data format, so their data are interchangeable
	E2_ToolbarData **thisbar = app.bars;
	while (*thisbar != NULL)
	{
		if (!strcmp ((*thisbar)->name, set->name))
			return "toolbar";
		thisbar++;
	}
	return (set->name);
}
/**
@brief tree expand-all callback

@param widget the menu item widget which activated the callback
@param treeview the treeview to be collapsed

@return
*/
static void _e2_option_tree_expand_all_cb (GtkMenuItem *widget, GtkTreeView *treeview)
{
	NEEDCLOSEBGL
	gtk_widget_set_sensitive (GTK_WIDGET (widget), FALSE);
	//get the paired item
	gpointer m = g_object_get_data (G_OBJECT (widget), "e2-menu-pair");
	gtk_widget_set_sensitive (GTK_WIDGET (m), TRUE);

	gtk_tree_view_expand_all (treeview);
	NEEDOPENBGL
}
/**
@brief tree collapse-all callback

@param widget the menu item widget which activated the callback
@param treeview the treeview to be collapsed

@return
*/
static void _e2_option_tree_collapse_all_cb (GtkMenuItem *widget, GtkTreeView *treeview)
{
	NEEDCLOSEBGL
	gtk_widget_set_sensitive (GTK_WIDGET (widget), FALSE);
	//get the paired button
	gpointer m = g_object_get_data (G_OBJECT (widget), "e2-menu-pair");
	gtk_widget_set_sensitive (GTK_WIDGET (m), TRUE);

	gtk_tree_view_collapse_all (treeview);
	NEEDOPENBGL
}
/**
@brief context-menu copy callback

This is called in response to the copy and cut menu items
It works on all selected rows, and all their descendants
(though robust relation-checks among various selected rows
have NOT been implemented, as multiple selection is not
currently allowed)
A signature string for the treeview and a list of arrays,
each with a path string and a whole row contents, are added
to a hash table.
The table may contain entries for more than 1 treeview

@param menu_item UNUSED the activated menu widget
@param treeview where the action is to occur

@return
*/
static void _e2_option_tree_menu_copy_cb (GtkMenuItem *menu_item, GtkTreeView *treeview)
{
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	NEEDCLOSEBGL
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		GList *rowscopied = e2_tree_copy (treeview);
		const gchar *id_string = _e2_option_tree_menu_get_signature (treeview);
		printd (DEBUG, "adding to buffer: key %s and %d rows", id_string,
			g_list_length (rowscopied));
		g_hash_table_insert (tree_view_buffer_hash, (gchar*)id_string, rowscopied); //NOT replace
	}
	NEEDOPENBGL
}
/**
@brief context-menu paste callback

An id string for the treeview is first constructed, to ensure that the
paste is appropriate
Pasted row(s) are placed after the selected row in the
destination treeview, as child(ren) if the path depth is appropriate

@param menu_item the activated menu widget
@param treeview widget where the action is to occur

@return
*/
static void _e2_option_tree_menu_paste_cb (GtkMenuItem *menu_item, GtkTreeView *treeview)
{
	//we only paste after the 1st selected row
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	NEEDCLOSEBGL
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		//get data for the current page/view
		const gchar *id_string = _e2_option_tree_menu_get_signature (treeview);
		//get the buffered data for the current page
		GList *copied = g_hash_table_lookup (tree_view_buffer_hash, id_string);
		if (copied != NULL)
		{
			e2_tree_paste (copied, treeview);
			GtkTreeModel *mdl = gtk_tree_view_get_model (treeview);
			E2_OptionSet *set = g_object_get_data (G_OBJECT (mdl), "e2-option-set");
			e2_option_tree_flag_change (set);
		}
	}
	NEEDOPENBGL
}
/**
@brief context-menu cut callback

@param menu_item the activated menu widget
@param treeview widget where the action is to occur

@return
*/
static void _e2_option_tree_menu_cut_cb (GtkMenuItem *menu_item, GtkTreeView *treeview)
{
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	NEEDCLOSEBGL
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		NEEDOPENBGL
		_e2_option_tree_menu_copy_cb (menu_item, treeview);
		NEEDCLOSEBGL
		e2_tree_delete (treeview);
		GtkTreeModel *mdl = gtk_tree_view_get_model (treeview);
		E2_OptionSet *set = g_object_get_data (G_OBJECT (mdl), "e2-option-set");
		e2_option_tree_flag_change (set);
	}
	NEEDOPENBGL
}
/**
@brief helper to [un]select all values of a boolean field in config store for @a set
@param set config data for the set
@param colnum treestore column enum value
@param val the new value, T or F
@return
*/
static void _e2_option_tree_update_all_bool (E2_OptionSet *set, guint colnum, gboolean val)
{
	GtkTreeIter iter;
	GtkTreeModel *mdl = set->ex.tree.model;
	if (gtk_tree_model_get_iter_first (mdl, &iter))
	{
		NEEDCLOSEBGL
		do
		{
			gtk_tree_store_set (GTK_TREE_STORE(mdl), &iter, colnum, val, -1);
		} while (gtk_tree_model_iter_next (mdl, &iter));
		NEEDOPENBGL
	}
}
/**
@brief plugins context-menu load all callback

@param menu_item the activated menu widget
@param set the plugins config data set

@return 
*/
static void _e2_option_tree_loadall_cb (GtkMenuItem *menu_item, E2_OptionSet *set)
{
	_e2_option_tree_update_all_bool (set, LOAD_COL, TRUE);
}
/**
@brief plugins context-menu unload all callback

@param menu_item the activated menu widget
@param set the plugins config data set

@return 
*/
static void _e2_option_tree_unloadall_cb (GtkMenuItem *menu_item, E2_OptionSet *set)
{
	_e2_option_tree_update_all_bool (set, LOAD_COL, FALSE);
}
/**
@brief plugins context-menu all on menu callback

@param menu_item the activated menu widget
@param set the plugins config data set

@return 
*/
static void _e2_option_tree_menuall_cb (GtkMenuItem *menu_item, E2_OptionSet *set)
{
	_e2_option_tree_update_all_bool (set, MENU_COL, TRUE);
}
/**
@brief plugins context-menu none on menu callback

@param menu_item the activated menu widget
@param set the plugins config data set

@return 
*/
static void _e2_option_tree_unmenuall_cb (GtkMenuItem *menu_item, E2_OptionSet *set)
{
	_e2_option_tree_update_all_bool (set, MENU_COL, FALSE);
}
/**
@brief add items to tree-option context menu

@param menu the menu widget to which the items are to be added
@param treeview where the action is to occur
@param flags indicator of menu items to be created
@param set pointer to data struct for the option that is being edited

@return
*/
static void _e2_option_tree_populate_menu (GtkWidget *menu,
	GtkTreeView *treeview, E2_TreeContextMenuFlags flags, E2_OptionSet *set)
{
	E2_Nontet *n = e2_utils_nontet_new ();
	g_object_set_data_full (G_OBJECT (menu), "e2-config-menu", n,
		(GDestroyNotify) e2_utils_nontet_destroy);
	if (n == NULL)
		return;
	//menu items which modify store content need set for callback data
	if ((flags & E2_TREE_CONTEXT_CP_PST) || (flags & E2_TREE_CONTEXT_DEFAULT))
	{
		n->a = e2_menu_add (menu, _("Cu_t"), STOCK_NAME_CUT,
			_("Cut selected row and any descendant(s)"),
			_e2_option_tree_menu_cut_cb, treeview);

		if (e2_option_get_simple ("plugins") != set)
			n->b = e2_menu_add (menu, _("_Copy"), STOCK_NAME_COPY,
				_("Copy selected row and any descendant(s)"),
				_e2_option_tree_menu_copy_cb, treeview);

		n->c = e2_menu_add (menu, _("_Paste"), STOCK_NAME_PASTE,
			_("Paste previously copied or cut row(s) after current row"),
			_e2_option_tree_menu_paste_cb, treeview);

		e2_menu_add_separator (menu);
	}
	if (e2_option_get_simple ("plugins") != set)
	{
		if (flags & E2_TREE_CONTEXT_EXP_COL)
		{
			n->d = e2_menu_add (menu, _("_Expand"), STOCK_NAME_ZOOM_IN,
					_("Expand all rows on this page"),
					_e2_option_tree_expand_all_cb, treeview);
			gtk_widget_set_sensitive (GTK_WIDGET(n->d), FALSE);	//widget starts with rows expanded
			n->e = e2_menu_add (menu, _("C_ollapse"), STOCK_NAME_ZOOM_OUT,
					_("Collapse all rows on this page"),
					_e2_option_tree_collapse_all_cb, treeview);
			//set pointers to the paired buttons
			g_object_set_data (G_OBJECT (n->d), "e2-menu-pair", n->e);
			g_object_set_data (G_OBJECT (n->e), "e2-menu-pair", n->d);

			e2_menu_add_separator (menu); //more items added in the calling function
		}

		n->f = e2_menu_add (menu, _("_Add"), STOCK_NAME_ADD,
			_("Add a row after the current one"), (void(*)()) _e2_option_tree_add_below_cb, set);

		if (flags & E2_TREE_CONTEXT_EXP_COL)
			n->g = e2_menu_add (menu, _("Add c_hild"), STOCK_NAME_INDENT,
				_("Add a child to the selected row"), (void(*)()) _e2_option_tree_add_child_cb, set);
	}
	else //this menu is for the plugins config page
	{
		n->d = e2_menu_add (menu, _("_Load all"),
#ifdef USE_GTK2_10
		STOCK_NAME_SELECT_ALL,
#else
		STOCK_NAME_APPLY,
#endif
			_("Mark all listed plugins to be loaded"), (void(*)()) _e2_option_tree_loadall_cb, set);
		n->e = e2_menu_add (menu, _("Load _none"), STOCK_NAME_CLEAR,
			_("Mark all listed plugins to not be loaded"), (void(*)()) _e2_option_tree_unloadall_cb, set);
		n->f = e2_menu_add (menu, _("_Menu all"), STOCK_NAME_YES,
			_("Show all listed plugins in menu"), (void(*)()) _e2_option_tree_menuall_cb, set);
		n->g = e2_menu_add (menu, _("M_enu none"), STOCK_NAME_NO,
			_("Hide all listed plugins from menu"), (void(*)()) _e2_option_tree_unmenuall_cb, set);
		e2_menu_add_separator (menu);
		e2_menu_add (menu, _("_Select"), STOCK_NAME_INDEX,
			_("Select one or more plugins to be added"), (void(*)()) e2_confdlg_choose_plugins_cb, set);
	}

	e2_menu_add_separator (menu);
	n->h = e2_menu_add (menu, _("_Up"), STOCK_NAME_GO_UP,
		_("Move selected row up"), (void(*)()) _e2_option_tree_move_up_cb, set);

	n->i = e2_menu_add (menu, _("_Down"), STOCK_NAME_GO_DOWN,
		_("Move selected row down"), (void(*)()) _e2_option_tree_move_down_cb, set);

}

  /******************/
 /***** public *****/
/******************/

/**
@brief (de)sensitize tree-option context-menu items

This changes the sensitivity of the paste menu item, depending on
whether the current treeview's set name is a key in the hash buffer

@param menu the context-menu widget
@param treeview widget where the menu was initiated

@return
*/
void e2_option_tree_menu_set_sensitive (GtkWidget *menu, GtkWidget *treeview)
{
	E2_Nontet *m = g_object_get_data (G_OBJECT (menu), "e2-config-menu");
	if (m == NULL)
		return;
	gboolean state;
	//first deal with the paste item
	if (m->c != NULL)
	{
		const gchar *id_string = _e2_option_tree_menu_get_signature (
			GTK_TREE_VIEW (treeview));
		state = (g_hash_table_lookup (tree_view_buffer_hash, id_string) != NULL);
		//FIXME also check for valid path depth of currently-selected item ?
		gtk_widget_set_sensitive (m->c, state);
	}
	//now adjust other menu items to conform with the page buttons
	E2_Sextet *st = g_object_get_data (G_OBJECT (treeview), "e2-config-buttons");
	if (st == NULL)
		return;
	if (st->b != NULL)	//up
	{
#ifdef USE_GTK2_18
		state = gtk_widget_get_sensitive(st->b);
#else
		state = GTK_WIDGET_SENSITIVE(st->b);
#endif
		if (m->h != NULL)	//up
			gtk_widget_set_sensitive (m->h, state);
	}
	if (st->c != NULL)	//down
	{
#ifdef USE_GTK2_18
		state = gtk_widget_get_sensitive(st->c);
#else
		state = GTK_WIDGET_SENSITIVE(st->c);
#endif
		if (m->i != NULL)	//down
			gtk_widget_set_sensitive (m->i, state);
	}
	if (st->d != NULL)	//add
	{
#ifdef USE_GTK2_18
		state = gtk_widget_get_sensitive(st->d);
#else
		state = GTK_WIDGET_SENSITIVE(st->d);
#endif
		if (m->f != NULL)	//add
			gtk_widget_set_sensitive (m->f, state);
	}
	if (st->e != NULL)	//add child
	{
#ifdef USE_GTK2_18
		state = gtk_widget_get_sensitive(st->e);
#else
		state = GTK_WIDGET_SENSITIVE(st->e);
#endif
		if (m->g != NULL)	//add child
			gtk_widget_set_sensitive (m->g, state);
	}
	if (st->f != NULL)	//remove
	{
#ifdef USE_GTK2_18
		state = gtk_widget_get_sensitive(st->f);
#else
		state = GTK_WIDGET_SENSITIVE(st->f);
#endif
		if (m->a != NULL)	//cut
			gtk_widget_set_sensitive (m->a, state);
		if (m->b != NULL)	//copy
			gtk_widget_set_sensitive (m->b, state);
	}
}
/**
@brief free memory used by tree-option context menu hash table entry

Each entry in the hash buffer is a glist of value arrays, one array for each row
copied to the buffer.
The first member of each array records the row's (allocated) treeview-path.
Other members are taken from treemodel column data.
See _e2_tree_copy_branch() 

@param list  glist to be freed

@return
*/
void e2_option_tree_menu_hash_clean (GList *list)
{
	GList *member;
	for (member = list; member != NULL; member = member->next)
	{
		GArray *value_array = member->data;
		//clear treepath at start of array
		GValue *valptr = (GValue*)value_array->data; //&g_array_index (value_array, GValue, 0);
		GtkTreePath *path = (GtkTreePath *) g_value_get_pointer (valptr);
		gtk_tree_path_free (path);
		//free other resources in the array, hope for no leaks! 
		guint i;
		for (i = 0; i < value_array->len; i++, valptr++)
		{
			if (G_VALUE_TYPE (valptr) != 0)
				g_value_unset (valptr);
		}
		g_array_free (value_array, TRUE);
	}
	g_list_free (list);
}

#endif //def INCLUDED_IN_PARENT
