/* $Id: e2p_tag.c 2846 2013-10-26 03:31:23Z tpgww $

Copyright (C) 2009-2013 tooar <tooar@emelfm2.net>

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
@file plugins/e2p_tag.c
@brief plugin to log selected items and reselect them later
*/

#include "emelfm2.h"
#include "e2_plugins.h"
#include "e2_fileview.h"
#include "e2_filelist.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "tag"

static PluginIface iface;

#ifdef E2_VFSTMP
//	FIXME vfsiface->vdir_cache is for vpath*'s i.e path + spacedata
/*
static (GHashFunc) hash_func, ()
{
}
static (GEqualFunc) key_equal_func ()
{
}
static (GDestroyNotify) key_destroy_func ()
{
}
gpointer hash key (PlaceInfo *spacedata, gchar *path)
{
	return NULL;
}
*/
#endif

/**
@brief log the names of selected items, if any, in the specified pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_tag_selected (gpointer from, E2_ActionRuntime *art)
{
	E2_ListChoice p;
	E2_PaneRuntime *rt;
	ViewInfo *view;
	GtkTreeModel *model;
	GHashTable *selitems;
	GList *selpaths, *rowpath;
	E2_DirHistoryEntry *hist;

	rt = e2_pane_get_runtime (from, art->data, NULL);
	view = (ViewInfo *)rt;
#ifdef E2_VFS
	VPATH ddata = { view->dir, view->spacedata }; //CHECKME path encoding ?
	VfsIface *vi = (VfsIface*) e2_plugins_get_installed ("vfs"VERSION);
	VPATH *vdir = g_hash_table_lookup (vi->vdir_cache, &ddata);
	if (G_UNLIKELY(vdir == NULL))
		return FALSE;
#endif

	p = (rt == curr_pane) ? PANEACTIVE : PANEINACTIVE;
	e2_filelist_disable_one_refresh (p);
	WAIT_FOR_REFRESH(view)

	selpaths = gtk_tree_selection_get_selected_rows (view->selection, &model);
	if (selpaths == NULL)
	{
		e2_filelist_enable_one_refresh (p);
		return FALSE;
	}

#ifdef E2_VFS
	hist = g_hash_table_lookup (app.dir_history, vdir);
#else
	hist = g_hash_table_lookup (app.dir_history, view->dir);
#endif
	if (hist->selitems != NULL)
		g_hash_table_destroy (hist->selitems);
	hist->selitems = selitems =
		g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

	for (rowpath = selpaths; rowpath != NULL; rowpath = rowpath->next)
	{
		GtkTreeIter iter;
		GtkTreePath *path;
		FileInfo *info;

		path = (GtkTreePath *) rowpath->data;
		if (gtk_tree_model_get_iter (model, &iter, path))
		{
			gtk_tree_model_get (model, &iter, FINFO, &info, -1);
			//info doesn't persist
			g_hash_table_insert (selitems, g_strdup(info->filename), GINT_TO_POINTER(1));
		}
		gtk_tree_path_free (path);
	}
	g_list_free (selpaths);

	e2_filelist_enable_one_refresh (p);

	return TRUE;
}

/**
@brief re-select the 'tagged' items, if any, in the specified pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_retag (gpointer from, E2_ActionRuntime *art)
{
	E2_ListChoice p;
	E2_PaneRuntime *rt;
	ViewInfo *view;
	E2_DirHistoryEntry *hist;
	GtkTreeModel *model;
	GtkTreeIter iter;

	rt = e2_pane_get_runtime (from, art->data, NULL);
	view = (ViewInfo *)rt;

	//get history item for this dir
#ifdef E2_VFS
	vpath *vdir = g_hash_table_find (?, );
	if (G_UNLIKELY(vdir == NULL))
		return FALSE;
	hist = g_hash_table_lookup (app.dir_history, vdir);
#else
	hist = g_hash_table_lookup (app.dir_history, view->dir);
#endif
	if (G_UNLIKELY(hist == NULL || hist->selitems == NULL))
		return FALSE;	//should never happen

	p = (rt == curr_pane) ? PANEACTIVE : PANEINACTIVE;
	e2_filelist_disable_one_refresh (p);
	WAIT_FOR_REFRESH(view)

	model = view->model;
	if (gtk_tree_model_get_iter_first (model, &iter));
	{	//it's not empty now
		GHashTable *selitems;
		GtkTreeSelection *sel;

		selitems = hist->selitems;
		sel = view->selection;
		gtk_tree_selection_unselect_all (sel);	//start with clean slate
		do
		{
			FileInfo *info;

			gtk_tree_model_get (model, &iter, FINFO, &info, -1);
			//We only check for name, no other statbuf parameters are stored ATM
			if (g_hash_table_lookup(selitems, info->filename) != NULL)
				gtk_tree_selection_select_iter (sel, &iter);
		} while (gtk_tree_model_iter_next (model, &iter));
	}

	e2_filelist_enable_one_refresh (p);

	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_INTRO
	PLUGINIT_NUMBERED_ALLOCATE(2)
	
	PLUGINIT_NUMBERED_ACTION(1,_A(6),_("tag"),_e2p_tag_selected,
		_("_Tag"),
		_("Log the items selected in active pane"),
		"plugin_"ANAME E2ICONTB)
	PLUGINIT_NUMBERED_ACTION(2,_A(7),_("retag"),_e2p_retag,
		_("_Retag"),
		_("Re-select any items logged in active pane"),
		NULL)

	PLUGINIT_NUMBERED_END
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
