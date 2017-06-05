/* $Id: e2p_selmatch.c 2865 2013-10-27 09:18:13Z tpgww $

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
@file plugins/e2p_selmatch.c
@brief plugin for selecting active[-pane items which are selected in inactive pane
*/

#include "emelfm2.h"
#include "e2_plugins.h"
#include "e2_fileview.h"
#include "e2_filelist.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "selmatch"

static PluginIface iface;

static void _e2p_selsame_reselect (GtkTreePath *tp, GtkTreeSelection *sel)
{
	gtk_tree_selection_select_path (sel, tp);
	gtk_tree_path_free (tp);
}

/**
@brief iterate over active pane file list to check for matches
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_select_same (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt;
	ViewInfo *selview;
	GtkTreeModel *models;
	GtkTreeIter iters;

	rt = e2_pane_get_runtime (from, art->data, NULL);
	selview = (ViewInfo *)rt;
	WAIT_FOR_REFRESH(selview)
	models = selview->model;
	if (gtk_tree_model_get_iter_first (models, &iters))
	{	//it's not empty
		ViewInfo *othview;
		GtkTreeModel *modelo;
		GtkTreeSelection *sel;
		GList *selpaths, *rowpath;
		GHashTable *selitems;
		FileInfo *info;
		gboolean fullmatch, forward;
		const gchar *seps;

		e2_filelist_disable_refresh ();

		othview = (rt == curr_pane) ? &other_pane->view : &curr_pane->view;
		WAIT_FOR_REFRESH(othview)
		selpaths = gtk_tree_selection_get_selected_rows (othview->selection, &modelo);
		if (selpaths == NULL)
		{
			e2_filelist_enable_refresh ();
			return FALSE;
		}
		// art->action->data is NULL for full name-scan, non-NULL for partial
		fullmatch = (art->action->data == NULL);
		if (fullmatch)
		{	//warning prevention
			seps = NULL;
			forward = TRUE;
		}
		else
		{
			seps = e2_option_str_get ("selmatch-separators");
			if (seps != NULL && *seps == '\0')
			{
				fullmatch = TRUE;
				forward = TRUE;
			}
			else
				forward = e2_option_bool_get ("selmatch-start");
		}

		// Log the selected items, for quick lookup
		selitems = (fullmatch) ?
			g_hash_table_new (g_str_hash, g_str_equal):
			g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

		for (rowpath = selpaths; rowpath != NULL; rowpath = rowpath->next)
		{
			GtkTreePath *path;
			GtkTreeIter itero;
			path = (GtkTreePath *) rowpath->data;
			if (gtk_tree_model_get_iter (modelo, &itero, path))
			{
				gtk_tree_model_get (modelo, &itero, FINFO, &info, -1);
				if (fullmatch)
					g_hash_table_insert (selitems, info->filename, info); //no key dup, info must persist
				else
				{
					gchar c;
					gchar *target;
					if (forward)
					{
						for (target = info->filename; (c = *target) != '\0'; target++)
						{
							if (strchr (seps, c) != NULL) //assumes no UTF-8 chars
							{
								target = g_strndup (info->filename, target - info->filename);
								g_hash_table_insert (selitems, target, info);
								break;
							}
						}
						if (c == '\0')
							g_hash_table_insert (selitems, g_strdup (info->filename), info);
					}
					else
					{
						for (target = info->filename + strlen (info->filename) - 1 ;
							target >= info->filename; target--)
						{
							if (strchr (seps, *target) != NULL) //assumes no UTF-8 chars
							{
								target = g_strndup (info->filename, target - info->filename);
								g_hash_table_insert (selitems, target, info);
								break;
							}
						}
						if (target < info->filename)
							g_hash_table_insert (selitems, g_strdup (info->filename), info);
					}
				}
			}
			gtk_tree_path_free (path);
		}
		g_list_free (selpaths);

		sel = selview->selection;
		gtk_tree_selection_unselect_all (sel);	//start with clean slate
		do
		{
			gboolean partial;
			gchar *scan;

			gtk_tree_model_get (models, &iters, FINFO, &info, -1);
			//We only check for name, ignore other statbuf parameters
			partial = FALSE;
			scan = NULL;
			if (!fullmatch)
			{
				gchar c;
				if (forward)
				{
					for (scan = info->filename; (c = *scan) != '\0'; scan++)
					{
						if (strchr (seps, c) != NULL) //assumes no UTF-8 chars, too bad if different sep from other pane
						{
							partial = TRUE;
							break;
						}
					}
				}
				else
				{
					for (scan = info->filename + strlen (info->filename) - 1 ;
						scan >= info->filename; scan--)
					{
						c = *scan;
						if (strchr (seps, c) != NULL) //assumes no UTF-8 chars
						{
							partial = TRUE;
							break;
						}
					}
				}
			}
			if (partial)
				scan = g_strndup (info->filename, scan - info->filename);
			else
				scan = info->filename;
			if (g_hash_table_lookup (selitems, scan) != NULL)
				gtk_tree_selection_select_iter (sel, &iters);
			if (partial)
				g_free (scan);
		} while (gtk_tree_model_iter_next (models, &iters));

		g_hash_table_destroy (selitems);

		//ensure some part of selection is visible
		selpaths = gtk_tree_selection_get_selected_rows (sel, NULL);
		if (selpaths != NULL)
		{
			printd (DEBUG, "scroll to show selection");
			GtkTreePath *tpath = gtk_tree_path_copy ((GtkTreePath *)selpaths->data);
			//this kills other selections
			gtk_tree_view_set_cursor (GTK_TREE_VIEW (selview->treeview), tpath, NULL, FALSE);
			//so back again, and cleanup
			g_list_foreach (selpaths, (GFunc)_e2p_selsame_reselect, sel);
			g_list_free (selpaths);
			gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (selview->treeview), tpath,
					NULL, TRUE, 0.382, 0.0);
			gtk_tree_path_free (tpath);
		}

		e2_filelist_enable_refresh ();

		return TRUE;
	}
	return FALSE;
}

/**
@brief iterate over active pane file list to check for matches
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_select_like (gpointer from, E2_ActionRuntime *art)
{
	art->action->data = GINT_TO_POINTER (1); //non-NULL to trigger partial-matches
	return _e2p_select_same (from, art);
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	const gchar *aname2 = _("selmatchpart");

	PLUGINIT_INTRO
	PLUGINIT_NUMBERED_ALLOCATE(2)

	PLUGINIT_NUMBERED_ACTION(1,_A(7),_("selmatch"),_e2p_select_same,
		_("_Select same"),
		_("Select items whose whole name matches a selected item in the other pane"),
		"plugin_"ANAME E2ICONTB)
	PLUGINIT_NUMBERED_ACTION(2,_A(7),aname2,_e2p_select_like,
		_("Select _like"),
		_("Select items whose name partially matches a selected item in the other pane"),
		NULL)

	//if the above included init, and it succeeded, more init needed
	if (iface.refcount == 1)
	{
		E2_OptionSetupExtra ex;
		gchar *group = g_strconcat(_C(34),".",_C(27),":",aname2,NULL); //_("plugins.options:selmatchpart"
		memset (&ex, 0, sizeof (E2_OptionSetupExtra));
		ex.exbool = TRUE;
		E2_OptionSet *set = e2_plugins_option_register (E2_OPTION_TYPE_BOOL, "selmatch-start",
			group, _("match to first separator"),
			_("If enabled, name matching stops at the first instance of any specified separator, otherwise, at the last instance"),
			NULL, &ex, E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_ADVANCED);
		//because plugins are loaded after config data, config options need to
		//get any data from unknown-options data
		e2_option_transient_value_get (set);

		ex.exstr = ".";
		set = e2_plugins_option_register (E2_OPTION_TYPE_STR, "selmatch-separators",
			group, _("separator character(s)"),
			_("String comprising all chars considered to be a 'separator'"),
			NULL, &ex, E2_OPTION_FLAG_ADVANCED);
		e2_option_transient_value_get (set);
	}

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

	if (ret)
		ret = e2_plugins_option_unregister ("selmatch-start");
	if (ret)
		ret = e2_plugins_option_unregister ("selmatch-separators");

	return ret;
}
