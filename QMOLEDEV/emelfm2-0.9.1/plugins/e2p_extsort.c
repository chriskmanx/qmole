/* $Id: e2p_extsort.c 2978 2013-11-30 02:56:32Z tpgww $

Copyright (C) 2003-2010 tooar <tooar@emelfm2.net>
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
@file plugins/e2p_sort_by_ext.c
@brief plugin to enable sorting of filelists by extension, instead of by name
This is effectively redundant, pressing <Control> before a sort-click achieves
the same effect
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_plugins.h"
#include "e2_filelist.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "extsort"

static PluginIface iface;

extern gint stored_col_order[2][MAX_COLUMNS];
extern gint displayed_col_order[2][MAX_COLUMNS];

/**
@brief sort active filepane by item-extension
Expects BGL on/closed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2p_sort_by_ext (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
	//replace the sort function for filename column of liststore
	//sortable uses base model, even with filter-model parent
    GtkTreeSortable *sortable = GTK_TREE_SORTABLE (rt->view.store);
	gint sortcolnow;
	GtkSortType sort_order = rt->view.sort_order;

	if (rt->view.extsort)
		//already extension-sorted, toggle direction
		rt->view.sort_order = (sort_order == GTK_SORT_ASCENDING) ?
			GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;
	else
		rt->view.extsort = TRUE;

	gtk_tree_sortable_get_sort_column_id (sortable, &sortcolnow, &sort_order);
	gtk_widget_hide (rt->view.sort_arrows[sortcolnow]);
	gtk_arrow_set (GTK_ARROW (rt->view.sort_arrows[FILENAME]),
		(rt->view.sort_order == GTK_SORT_ASCENDING) ?
			 GTK_ARROW_RIGHT : GTK_ARROW_LEFT, GTK_SHADOW_IN);
	gtk_widget_show (rt->view.sort_arrows[FILENAME]);

	gtk_tree_sortable_set_sort_func (sortable, FILENAME,
		(GtkTreeIterCompareFunc) e2_fileview_ext_sort, &sort_order, NULL);
	//do the sort
	gtk_tree_sortable_set_sort_column_id (sortable, FILENAME, rt->view.sort_order);
	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(7),_("sort_by_ext"),_e2p_sort_by_ext,
		_("Extension _sort"),
		_("Sort the active file pane by filename extension"),
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
	return ret;
}
