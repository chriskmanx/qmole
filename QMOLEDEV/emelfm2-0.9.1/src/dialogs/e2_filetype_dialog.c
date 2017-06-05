/* $Id: e2_filetype_dialog.c 2911 2013-11-13 01:01:29Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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

#include "emelfm2.h"
#include <string.h>
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_filetype.h"
#include "e2_icons.h"

#ifndef OLDFTDLG
//no translation of this
#define E2_FTDLG_SIG "PANE%d-FTDLG"

  /*****************************/
 /*** edit filetypes dialog ***/
/*****************************/

/* this dialog uses the config treestore for the filetypes option,
and also 2 special-purpose treestores and 3 treeviews, respectively
for categories, extensions and commands. The latter 2 have no
children, but cannot be liststores coz the cut/copy/paste functions
work only on treestores.
Contents of the latter 2 are moved back and forth from the main config
treestore as needed, notably when a different category is selected */

//FIXME doesn't like config dialog filetypes being edited when this is open
//FIXME saving to invalid path in config treestore, create new cat & use its path

typedef enum
{
	E2_CATPANE,
	E2_EXTPANE,
	E2_CMDPANE,
	E2_PANECOUNT
} E2_FileTypeDlg_PaneID;

typedef struct _E2_FileTypeDlgRuntime
{
	E2_OptionSet *typeset;
	E2_FileTypeDlg_PaneID currentID;
	void *stores[E2_PANECOUNT];	//GtkTreeStores
	GtkWidget *views[E2_PANECOUNT];
	GtkTreePath *catpath;
	GHashTable *buffer_hash;	//for cut|copy data
} E2_FileTypeDlgRuntime;

static gboolean _e2_edftdlg_cancel_cb (GtkWidget *widget, E2_FileTypeDlgRuntime *rt);
static GtkWidget *filetypes_dialog = NULL;

/**
@brief save current-category data back into the config option treestore
Current category is already logged at rt->catpath
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_edftdlg_save_current_category (E2_FileTypeDlgRuntime *rt)
{
	GtkTreeModel *cfgmdl  = rt->typeset->ex.tree.model;
	GtkTreeIter cfg_iter;
	GtkTreePath *path = rt->catpath;
	if (path == NULL //there was no category
		|| !gtk_tree_model_get_iter (cfgmdl, &cfg_iter, path))
		return;

	GtkTreeModel *mdl;
	gchar *extension, *cmd_label, *cmd_string;
	GtkTreeIter iter, cfg_iter2, cfg_iter3;	//iters for local liststore & 3 levels of filetypes treestore
	gint cfgi, newi, cfgchildren, newchildren;

	//there should never be a case where the extensions and commands child-nodes
	//are missing, but ...
	if (gtk_tree_model_iter_children (cfgmdl, &cfg_iter2, &cfg_iter))
	{
		do
		{  // extension or command loop = level 2
			cfgchildren = gtk_tree_model_iter_n_children (cfgmdl, &cfg_iter2);
			gtk_tree_model_get (cfgmdl, &cfg_iter2, 1, &extension, -1);
			if (!strcmp (extension, _C(13)))  //extensions node found
			{
				g_free (extension);
				mdl = GTK_TREE_MODEL (rt->stores[E2_EXTPANE]);
				newchildren = gtk_tree_model_iter_n_children (mdl, NULL);
				//replace as many of old values as possible
				cfgi = 0;
				for (newi = 0; newi < newchildren; newi++)
				{
					gtk_tree_model_iter_nth_child (mdl, &iter, NULL, newi);
					gtk_tree_model_get (mdl, &iter, 0, &extension, -1);
					if (extension != NULL)
					{
						if (*extension != '\0')
						{
							if (cfgi == cfgchildren)
							{
								g_free (extension);
								break;
							}
							gtk_tree_model_iter_nth_child (cfgmdl, &cfg_iter3, &cfg_iter2, cfgi);
							gtk_tree_store_set (rt->stores[E2_CATPANE], &cfg_iter3, 1, extension, -1);
							cfgi++;
						}
						g_free (extension);
					}
				}
				//if we ran out of old children, need to add more
				for (;newi < newchildren; newi++)
				{
					gtk_tree_model_iter_nth_child (mdl, &iter, NULL, newi);
					gtk_tree_model_get (mdl, &iter, 0, &extension, -1);
					if (extension != NULL)
					{
						if (*extension != '\0')
						{
#ifdef USE_GTK2_10
							gtk_tree_store_insert_with_values (
								rt->stores[E2_CATPANE], &cfg_iter3, &cfg_iter2, -1,
#else
							//insert empty row into the config treestore, after other children
							gtk_tree_store_insert (rt->stores[E2_CATPANE], &cfg_iter3, &cfg_iter2, -1);
							//populate it
							gtk_tree_store_set (rt->stores[E2_CATPANE], &cfg_iter3,
#endif
								1, extension, -1);

						}
						g_free (extension);
					}
				}
			}
			else if (!strcmp (extension, _C(6)))  //commands node found
			{
				g_free (extension);
				mdl = GTK_TREE_MODEL (rt->stores[E2_CMDPANE]);
				newchildren = gtk_tree_model_iter_n_children (mdl, NULL);
				//replace as many of old values as possible
				cfgi = 0;
				for (newi = 0; newi < newchildren; newi++)
				{
					gtk_tree_model_iter_nth_child (mdl, &iter, NULL, newi);
					gtk_tree_model_get (mdl, &iter, 0, &cmd_label, 1, &cmd_string, -1);
					if (cmd_string != NULL)
					{
						if (*cmd_string != '\0')
						{
							if (cfgi == cfgchildren)
							{
								if (cmd_label != NULL)
									g_free (cmd_label);
								g_free (cmd_string);
								break;
							}
							if (cmd_label == NULL)
								cmd_label = g_strdup ("?");	//tolerate blank labels
							else if (*cmd_label == '\0')
							{
								g_free (cmd_label);
								cmd_label = g_strdup ("?");
							}
							gtk_tree_model_iter_nth_child (cfgmdl, &cfg_iter3, &cfg_iter2, cfgi);
							gtk_tree_store_set (rt->stores[E2_CATPANE], &cfg_iter3,
								1, cmd_label, 2, cmd_string, -1);
							cfgi++;
						}
						g_free (cmd_string);
					}
					g_free (cmd_label);
				}
				//if we ran out of old children, need to add more
				for (;newi < newchildren; newi++)
				{
					gtk_tree_model_iter_nth_child (mdl, &iter, NULL, newi);
					gtk_tree_model_get (mdl, &iter, 0, &cmd_label, 1, &cmd_string, -1);
					if (cmd_string != NULL && *cmd_string != '\0')
					{
						if (cmd_label == NULL)
							cmd_label = g_strdup ("?");	//tolerate blank labels
						else if (*cmd_label == '\0')
						{
							g_free (cmd_label);
							cmd_label = g_strdup ("?");
						}
#ifdef USE_GTK2_10
						gtk_tree_store_insert_with_values (
							rt->stores[E2_CATPANE], &cfg_iter3, &cfg_iter2, -1,
#else
						//insert empty row into the config treestore, after other children
						gtk_tree_store_insert (rt->stores[E2_CATPANE], &cfg_iter3,
							&cfg_iter2, -1);
						//populate it
						gtk_tree_store_set (rt->stores[E2_CATPANE], &cfg_iter3,
#endif
							1, cmd_label, 2, cmd_string, -1);
					}
					if (cmd_label != NULL)
						g_free (cmd_label);
					if (cmd_string != NULL)
						g_free (cmd_string);
				}
			}
			else
				continue;	//OOPS - what else is there ?
			//delete any redundant old children
			//(by repeatedly deleting the next child)
			newi = cfgi;	//re-use this variable
			for (; cfgi < cfgchildren; cfgi++)
			{
				gtk_tree_model_iter_nth_child (cfgmdl, &cfg_iter3, &cfg_iter2, newi);
				gtk_tree_store_remove (rt->stores[E2_CATPANE], &cfg_iter3);
			}
		} while (gtk_tree_model_iter_next (cfgmdl, &cfg_iter2));
	}
}
/**
@brief (re)populate extensions- and commands-view for the category at @a path in the categories-view

@a path is logged, so we can always find the
currently-displayed category item
Extensions and commands stores are cleared, so
anything there that we want to keep must already be saved
@param path ptr to treeview path of the category to be used
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_edftdlg_update_category (GtkTreePath *path,
	E2_FileTypeDlgRuntime *rt)
{
	gtk_tree_store_clear (rt->stores[E2_EXTPANE]);
	gtk_tree_store_clear (rt->stores[E2_CMDPANE]);

	GtkTreeIter iter;
	GtkTreeModel *cfgmdl = rt->typeset->ex.tree.model;
	if (gtk_tree_model_get_iter (cfgmdl, &iter, path))
	{
		GtkTreeIter newiter;	//for dialog stores
		GtkTreeIter iter2, iter3;	//for cfg levels
		gchar *extension, *cmd_label, *cmd_string;
		//should always be level-2 child nodes, but test anyway ...
		if (gtk_tree_model_iter_children (cfgmdl, &iter2, &iter))
		{
			do
			{  // extension or command loop = level 2
				gtk_tree_model_get (cfgmdl, &iter2, 1, &extension, -1);
				if (gtk_tree_model_iter_children (cfgmdl, &iter3, &iter2))
				{
					if (!strcmp (extension, _C(13)))  //extensions node found
					{
						g_free (extension);
						do
						{  //extension loop = level 3, fill the extensions store
							gtk_tree_model_get (cfgmdl, &iter3, 1, &extension, -1);
#ifdef USE_GTK2_10
							gtk_tree_store_insert_with_values (
								rt->stores[E2_EXTPANE], &newiter, NULL, -1,
#else
							//append empty row to the store
							gtk_tree_store_append (rt->stores[E2_EXTPANE], &newiter, NULL);
							//populate it
							gtk_tree_store_set (rt->stores[E2_EXTPANE], &newiter,
#endif
								0, extension, -1);
							g_free (extension);
						} while (gtk_tree_model_iter_next (cfgmdl, &iter3));
					}
					else if (!strcmp (extension, _C(6)))  //commands node found
					{
						g_free (extension);
						do
						{  //commands loop also = level 3, build the commands array
							gtk_tree_model_get (cfgmdl, &iter3, 1, &cmd_label, 2,
								&cmd_string, -1);
#ifdef USE_GTK2_10
							gtk_tree_store_insert_with_values (
								rt->stores[E2_CMDPANE], &newiter, NULL, -1,
#else
							//append empty row to the store
							gtk_tree_store_append (rt->stores[E2_CMDPANE], &newiter, NULL);
							//populate it
							gtk_tree_store_set (rt->stores[E2_CMDPANE], &newiter,
#endif
								0, cmd_label, 1, cmd_string, -1);
							g_free (cmd_label);
							g_free (cmd_string);
						} while (gtk_tree_model_iter_next (cfgmdl, &iter3));
					}
					else
					{ //OOPS
						printd (WARN, "un-recognised node in filetypes config");
					}
				}
				else
					g_free (extension); //no children, just cleanup
			} while (gtk_tree_model_iter_next (cfgmdl, &iter2));

		}
	}

	//remember where to find the category now displayed
	if (rt->catpath != NULL)
		gtk_tree_path_free (rt->catpath);
	rt->catpath = gtk_tree_path_copy (path);
}
/**
@brief treeview cell edited callback

@param renderer the renderer that was used
@param path_string string form of path of edited cell
@param new_text the replacement content for the edited cell
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_edftdlg_cell_edited_cb (GtkCellRendererText *renderer,
	gchar *path_string, gchar *new_text, E2_FileTypeDlgRuntime *rt)
{
	GtkTreeIter iter;
	if (new_text == NULL)
		return;	//the user did not finish editing a new cell
	gtk_tree_model_get_iter_from_string
		(GTK_TREE_MODEL (rt->stores[rt->currentID]), &iter, path_string);
	gpointer colptr = g_object_get_data (G_OBJECT (renderer), "col_num");
	gint colnum = GPOINTER_TO_INT (colptr);
	if (colnum == -1)	//this is the categories renderer
		colnum = 0;
	NEEDCLOSEBGL
	gtk_tree_store_set (rt->stores[rt->currentID], &iter, colnum, new_text, -1);
	NEEDOPENBGL
}
/**
@brief category treeview selection-changed callback

@param treeselection selection object for the clicked treeview widget
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_edftdlg_cat_sel_change_cb (GtkTreeSelection *treeselection,
	E2_FileTypeDlgRuntime *rt)
{
	GtkTreePath *path;
	NEEDCLOSEBGL
	gtk_tree_view_get_cursor (GTK_TREE_VIEW (rt->views[E2_CATPANE]), &path, NULL);
	if (path != NULL)
	{
		_e2_edftdlg_save_current_category (rt);
		_e2_edftdlg_update_category (path, rt);
		gtk_tree_path_free (path);
	}
	NEEDOPENBGL
}
/**
@brief determine which pane's store is current, and the first selected iter

@param store pointer to store for current-store pointer
@param iter pointer to iter to set to the first selected item in store

@return TRUE if iter is set
*/
static gboolean _e2_edftdlg_find_selected
	(E2_FileTypeDlgRuntime *rt, GtkTreeIter *iter)
{
	GtkTreeSelection *sel =
		gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->views[rt->currentID]));
	GtkTreeModel *mdl;
	if (rt->currentID == E2_CATPANE)
		//categories treeview is single-selection only
		return (gtk_tree_selection_get_selected (sel, &mdl, iter));

	gboolean retval;
	GList *selpaths = gtk_tree_selection_get_selected_rows (sel, &mdl);
	if (selpaths != NULL)
	{
		retval = gtk_tree_model_get_iter (mdl, iter, (GtkTreePath *)selpaths->data);
		g_list_foreach (selpaths, (GFunc) gtk_tree_path_free, NULL);
		g_list_free (selpaths);
	}
	else
		retval = FALSE;
	return retval;
}
/**
@brief process dialog pane change
This is a "focus-in-event" callback, applied to each treeview

@param treeview the newly-focused treeview widget
@param event ptr to event data struct
@param rt pointer to data struct for the dialog

@return FALSE always - allow other handlers
*/
static gboolean _e2_edftdlg_focus_in_cb (GtkWidget *treeview,
	GdkEventFocus *event, E2_FileTypeDlgRuntime *rt)
{
	GtkTreeIter iter;
	if (treeview == rt->views[E2_CATPANE])
		rt->currentID = E2_CATPANE;
	else if (treeview == rt->views[E2_EXTPANE])
		rt->currentID = E2_EXTPANE;
	else
		rt->currentID = E2_CMDPANE;
	
	NEEDCLOSEBGL
	if (!_e2_edftdlg_find_selected (rt, &iter))
	{
		GtkTreePath*path = gtk_tree_path_new_first ();
		//NOTE this is supposed to, but doesn't, make the cell editable
		//without a mouse-click !
		gtk_tree_view_set_cursor (GTK_TREE_VIEW (treeview), path, NULL, TRUE);
		gtk_tree_path_free (path);
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief insert a row into store and view

@param rt pointer to data struct for the dialog

@return
*/
static void _e2_edftdlg_insert_row (E2_FileTypeDlgRuntime *rt)
{
	GtkTreeIter iter, newiter;
	if (_e2_edftdlg_find_selected (rt, &iter))
		gtk_tree_store_insert_after (rt->stores[rt->currentID], &newiter, NULL, &iter);
	else
		gtk_tree_store_append (rt->stores[rt->currentID], &newiter, NULL);
	GtkTreeSelection *sel = gtk_tree_view_get_selection
		(GTK_TREE_VIEW (rt->views[rt->currentID]));
	if (rt->currentID == E2_CATPANE)
	{
		//provide a token category name
		gtk_tree_store_set (rt->stores[E2_CATPANE], &newiter, 0, _("new"), -1);
		 //create empty framework for the new category
#ifdef USE_GTK2_10
		gtk_tree_store_insert_with_values (rt->stores[E2_CATPANE],
			&iter, &newiter, -1, 1, _C(13), -1);
		gtk_tree_store_insert_with_values (rt->stores[E2_CATPANE],
			&iter, &newiter, -1, 1, _C(6), -1);
#else
		//add extensions node
		gtk_tree_store_append (rt->stores[E2_CATPANE], &iter, &newiter);
		gtk_tree_store_set (rt->stores[E2_CATPANE], &iter, 1, _C(13), -1);
		//add commands node
		gtk_tree_store_append (rt->stores[E2_CATPANE], &iter, &newiter);
		gtk_tree_store_set (rt->stores[E2_CATPANE], &iter, 1, _C(6), -1);
#endif
		GtkTreePath *path =
			gtk_tree_model_get_path (rt->typeset->ex.tree.model, &newiter);
		gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (rt->views[E2_CATPANE]),
			path, NULL, FALSE, 0, 0);
		gtk_tree_selection_select_path (sel, path);
		_e2_edftdlg_save_current_category (rt);
		_e2_edftdlg_update_category (path, rt);	//update rt->catpath & clear the views
		gtk_tree_path_free (path);
	}
	else	//not a new category
	{
//		gtk_list_store_set (rt->stores[[rt->currentID]], &newiter, 0, _("new"), -1);
		gtk_tree_selection_unselect_all (sel);
		gtk_tree_selection_select_iter (sel, &newiter);
	}
}
/**
@brief dialog response callback

@param dialog the dialog where the response was generated
@param response the response assigned to @a button
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_edftdlg_response_cb (GtkDialog *dialog, gint response,
	E2_FileTypeDlgRuntime *rt)
{
	GtkTreeIter iter;
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_APPLY:
			_e2_edftdlg_save_current_category (rt); //this might be dirty
			e2_option_tree_unbackup (rt->typeset, FALSE);	//get rid of old backup data
			g_hash_table_destroy (rt->buffer_hash);
			DEALLOCATE (E2_FileTypeDlgRuntime, rt);
			gtk_widget_destroy (filetypes_dialog);
			filetypes_dialog = NULL;
			e2_filetype_apply_allnew ();
			gtk_widget_grab_focus (curr_view->treeview);
			break;
		case E2_RESPONSE_MORE:
			_e2_edftdlg_save_current_category (rt); //this might be dirty
			e2_option_tree_unbackup (rt->typeset, FALSE);	//get rid of old backup data
			e2_filetype_apply_allnew ();	//convert config data to runtime format
			e2_option_tree_backup (rt->typeset);	//get fresh backup data
			break;
		case E2_RESPONSE_USER1:
			//move-up button pressed
			if (_e2_edftdlg_find_selected (rt, &iter))
			{
				GtkTreePath *path = gtk_tree_model_get_path
					(GTK_TREE_MODEL (rt->stores[rt->currentID]), &iter);
				if (gtk_tree_path_prev (path))
				{
					GtkTreeIter previter;
					gtk_tree_model_get_iter (GTK_TREE_MODEL (rt->stores[rt->currentID]),
						&previter, path);
					gtk_tree_store_swap (rt->stores[rt->currentID], &iter, &previter);
				}
				gtk_tree_path_free (path);
			}
			break;
		case E2_RESPONSE_USER2:
			//move-down button pressed
			if (_e2_edftdlg_find_selected (rt, &iter))
			{
				GtkTreeIter nextiter = iter;
				if (gtk_tree_model_iter_next
					(GTK_TREE_MODEL (rt->stores[rt->currentID]), &nextiter))
						gtk_tree_store_swap (rt->stores[rt->currentID], &iter, &nextiter);
			}
			break;
		case E2_RESPONSE_USER3:
			//insert-after button pressed
			_e2_edftdlg_insert_row (rt);
			break;
		case E2_RESPONSE_REMOVE:
			if (rt->currentID == E2_CATPANE)
			{
				if (_e2_edftdlg_find_selected (rt, &iter))
				{
					GtkTreePath *newpath = gtk_tree_model_get_path
							(GTK_TREE_MODEL (rt->stores[E2_CATPANE]), &iter);
					GtkTreeIter newiter = iter;	//this is where we goto after the removal
					//(if there's a next iter, when the current is deleted, next becomes current)
					gboolean allgone = !
					(gtk_tree_model_iter_next (rt->typeset->ex.tree.model, &newiter)
						|| gtk_tree_path_prev (newpath));

					//this deletes all children too
					gtk_tree_store_remove (rt->stores[E2_CATPANE], &iter);

					if (allgone)
					{	//nowhere else to show
						if (rt->catpath != NULL)
							gtk_tree_path_free (rt->catpath);
						rt->catpath = NULL;
						//clear the related views
						gtk_tree_store_clear (rt->stores[E2_EXTPANE]);
						gtk_tree_store_clear (rt->stores[E2_CMDPANE]);
					}
					else
					{
						gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (rt->views[E2_CATPANE]),
							newpath, NULL, FALSE, 0, 0);
						GtkTreeSelection *sel =
							gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->views[E2_CATPANE]));
						gtk_tree_selection_select_path (sel, newpath);
						_e2_edftdlg_update_category (newpath, rt);
					}
					gtk_tree_path_free (newpath);
				}

			}
			else
				e2_tree_delete (GTK_TREE_VIEW (rt->views[rt->currentID]));
			break;
		default:
			NEEDOPENBGL
			_e2_edftdlg_cancel_cb (NULL, rt);
			return;
	}
	NEEDOPENBGL
}
/**
series of context-menu callback functions

@param item the activated menu item widget
@param rt pointer to dialog data struct

@return
*/
static void _e2_edftdlg_menu_copy_cb (GtkMenuItem *item, E2_FileTypeDlgRuntime *rt)
{
	GtkTreeView *treeview = GTK_TREE_VIEW (rt->views[rt->currentID]);
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	NEEDCLOSEBGL
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		GList *copied = e2_tree_copy (treeview);
		gchar *id_string = g_strdup_printf (E2_FTDLG_SIG, rt->currentID);
		printd (DEBUG, "adding to buffer: key %s and %d rows", id_string,
			g_list_length (copied));
		g_hash_table_replace (rt->buffer_hash, id_string, copied);
	}
	NEEDOPENBGL
}
static void _e2_edftdlg_menu_cut_cb (GtkMenuItem *item, E2_FileTypeDlgRuntime *rt)
{
	GtkTreeView *treeview = GTK_TREE_VIEW (rt->views[rt->currentID]);
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	NEEDCLOSEBGL
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		NEEDOPENBGL
		_e2_edftdlg_menu_copy_cb (item, rt);
		NEEDCLOSEBGL
		e2_tree_delete (treeview);
	}
	NEEDOPENBGL
}
static void _e2_edftdlg_menu_paste_cb (GtkMenuItem *item, E2_FileTypeDlgRuntime *rt)
{
	NEEDCLOSEBGL
	//as a special case, if the store is empty, setup to add the data
	gboolean empty = (gtk_tree_model_iter_n_children
		(GTK_TREE_MODEL (rt->stores[rt->currentID]), NULL) == 0);
	if (empty)
		_e2_edftdlg_insert_row (rt);

	GtkTreeView *treeview = GTK_TREE_VIEW (rt->views[rt->currentID]);
	//we paste after the 1st selected item
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		gchar *id_string = g_strdup_printf (E2_FTDLG_SIG, rt->currentID);
		//get the buffered data, if any, for the current page
		GList *copied = g_hash_table_lookup (rt->buffer_hash, id_string);
		if (copied != NULL)
			e2_tree_paste (copied, treeview);
		g_free (id_string);
	}

	if (empty)
		e2_tree_delete (treeview);
	NEEDOPENBGL
}

static void _e2_edftdlg_menu_add_below_cb (GtkMenuItem *item, E2_FileTypeDlgRuntime *rt)
{
	NEEDCLOSEBGL
	_e2_edftdlg_insert_row (rt);
	NEEDOPENBGL
}

static void _e2_edftdlg_menu_move_up_cb (GtkMenuItem *item, E2_FileTypeDlgRuntime *rt)
{
//	NEEDOPENBGL
	_e2_edftdlg_response_cb (GTK_DIALOG (filetypes_dialog), E2_RESPONSE_USER1, rt);
//	NEEDCLOSEBGL
}

static void _e2_edftdlg_menu_move_down_cb (GtkMenuItem *item, E2_FileTypeDlgRuntime *rt)
{
//	NEEDOPENBGL
	_e2_edftdlg_response_cb (GTK_DIALOG (filetypes_dialog), E2_RESPONSE_USER2, rt);
//	NEEDCLOSEBGL
}
/**
@brief set popup menu position

This function is supplied when calling gtk_menu_popup(), to position
the displayed menu.
set @a push_in to TRUE for menu completely inside the screen,
FALSE for menu clamped to screen size

@param menu UNUSED the GtkMenu to be positioned
@param x	place to store gint representing the menu left
@param y  place to store gint representing the menu top
@param push_in place to store pushin flag
@param treeview the focused widget when the button was pressed

@return
*/
static void _e2_edftdlg_set_menu_position (GtkMenu *menu,
	gint *x, gint *y, gboolean *push_in, GtkWidget *treeview)
{
	gint left, top;
	gtk_window_get_position (GTK_WINDOW (filetypes_dialog), &left, &top);
	GtkAllocation alloc;
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (treeview, &alloc);
#else
	alloc = treeview->allocation;
#endif
	*x = left+ alloc.x + alloc.width/2;
	*y = top + alloc.y +alloc.height/2 - 30;
	*push_in = FALSE;
}
/**
@brief construct and pop up destroyable context-menu for the filetype-dialog

@param treeview widget where the action is to occur
@param button the clicked mouse button, or 0 for menu key
@param time event time
@param rt pointer to dialog data struct

@return
*/
static void _e2_edftdlg_menu_create (GtkWidget *treeview,
	guint button, guint32 time, E2_FileTypeDlgRuntime *rt)
{
	GtkWidget *menu = e2_menu_get ();
	e2_menu_add (menu, _("Cu_t"), STOCK_NAME_CUT,
		_("Cut selected row(s)"), _e2_edftdlg_menu_cut_cb, rt);
	e2_menu_add (menu, _("_Copy"), STOCK_NAME_COPY,
		_("Copy selected row(s)"), _e2_edftdlg_menu_copy_cb, rt);
	//check for valid stored content
	gchar *id_string = g_strdup_printf (E2_FTDLG_SIG, rt->currentID);
	gboolean state = (g_hash_table_lookup (rt->buffer_hash, id_string) != NULL);
	g_free (id_string);
	GtkWidget *item = e2_menu_add (menu, _("_Paste"), STOCK_NAME_PASTE,
		_("Paste previously copied or cut row(s) after current row"),
		_e2_edftdlg_menu_paste_cb, rt);
	gtk_widget_set_sensitive (item, state);
	e2_menu_add_separator (menu);
	e2_menu_add (menu, _("_Add"), STOCK_NAME_ADD,
		_("Add a row after the current one"), _e2_edftdlg_menu_add_below_cb, rt);
	e2_menu_add_separator (menu);
	e2_menu_add (menu, _("_Up"), STOCK_NAME_GO_UP,
		_("Move selected row up"), _e2_edftdlg_menu_move_up_cb, rt);
	e2_menu_add (menu, _("_Down"), STOCK_NAME_GO_DOWN,
		_("Move selected row down"), _e2_edftdlg_menu_move_down_cb, rt);

	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	if (button == 0)
		//this was a menu-key press
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
		(GtkMenuPositionFunc) _e2_edftdlg_set_menu_position, treeview, 0, time);
	else
		//this was a button-3 click
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, button, time);
}
/**
@brief treeview menu-button press callback

@param treeview the focused widget when the menu button was pressed
@param rt pointer to data struct for the dialog

@return TRUE
*/
static gboolean _e2_edftdlg_popup_menu_cb (GtkWidget *treeview,
	E2_FileTypeDlgRuntime *rt)
{
	guint32 event_time = gtk_get_current_event_time ();
	NEEDCLOSEBGL
	_e2_edftdlg_menu_create (treeview, 0, event_time, rt);
	NEEDOPENBGL
	return TRUE;
}
/**
@brief treeview button press callback

@param treeview the clicked treeview widget
@param event ptr to event data struct
@param rt pointer to data struct for the dialog

@return FALSE
*/
static gboolean _e2_edftdlg_button_press_cb (GtkWidget *treeview,
	GdkEventButton *event, E2_FileTypeDlgRuntime *rt)
{
	//FIXME find a way to cancel editing of a renderer that is clicked again
	NEEDCLOSEBGL
	if (event->button == 1 && event->type == GDK_BUTTON_PRESS //no multi-clicks
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		GtkTreePath *path;
		if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (treeview),
				event->x, event->y, &path, NULL, NULL, NULL))
		{
			if (treeview == rt->views[E2_CATPANE])
			{
				_e2_edftdlg_save_current_category (rt);
				_e2_edftdlg_update_category (path, rt);
			}
			gtk_tree_path_free (path);
		}
		//clicked a bare part of the view
		if (treeview == rt->views[E2_CATPANE])
			rt->currentID = E2_CATPANE;
		else if (treeview == rt->views[E2_EXTPANE])
			rt->currentID = E2_EXTPANE;
		else
			rt->currentID = E2_CMDPANE;
	}
	else if (event->button == 3
#ifdef E2_MOUSECUSTOM
			&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		_e2_edftdlg_menu_create (treeview, 3, event->time, rt);
		NEEDOPENBGL
		return TRUE;
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief abandon the dialog without making any change

@param widget the widget which was activated to trigger the cancel
@param rt pointer to data struct for the dialog

@return TRUE always - prevent other handlers
*/
static gboolean _e2_edftdlg_cancel_cb (GtkWidget *widget, E2_FileTypeDlgRuntime *rt)
{
	NEEDCLOSEBGL
	gtk_widget_destroy (filetypes_dialog);
	e2_option_tree_unbackup (rt->typeset, TRUE);	//undo everything
	gtk_widget_grab_focus (curr_view->treeview);
	NEEDOPENBGL
	filetypes_dialog = NULL;
	DEALLOCATE (E2_FileTypeDlgRuntime, rt);

	return TRUE;
}
/**
@brief if Escape key is pressed, abandon the dialog without making any change

@param widget the widget which was activated to trigger the cancel
@parm event pointer to event data struct
@param rt pointer to data struct for the dialog

@return TRUE if Esc key processed, else FALSE
*/
static gboolean _e2_edftdlg_key_press_cb (GtkWidget *widget, GdkEventKey *event,
	E2_FileTypeDlgRuntime *rt)
{
	if (event->keyval == GDK_Escape)
	{
//		NEEDOPENBGL
		//restore tree data and clean up
		return (_e2_edftdlg_cancel_cb (NULL, rt));
	}
	return FALSE;
}
/**
@brief create and show edit filetypes dialog
Assumes BGL is closed
@param open_category name of category to focus when dialog opens, or NULL
@return
*/
void e2_filetype_dialog_edit_create (gchar *open_category)
{
	//check if there is already one of these dialogs opened
	if (filetypes_dialog != NULL)
	{
		gtk_window_present (GTK_WINDOW (filetypes_dialog));
		return;
	}
	E2_OptionSet *set = e2_option_get_simple ("filetypes");
	if (set == NULL)
		return;	//can't do anything

	E2_FileTypeDlgRuntime *rt = ALLOCATE (E2_FileTypeDlgRuntime);
	CHECKALLOCATEDWARN (rt, return);
	rt->typeset = set;
	rt->catpath = NULL;
	//receptacle for cut/copied row(s) data generated from context menu
	rt->buffer_hash = g_hash_table_new_full (g_str_hash, g_str_equal,
		g_free, (GDestroyNotify) e2_option_tree_menu_hash_clean);

	filetypes_dialog = e2_dialog_create (NULL, NULL, _("edit filetypes"),
		(ResponseFunc)_e2_edftdlg_response_cb, rt);

	e2_option_connect (filetypes_dialog, FALSE);

	//main scrolled window
	GtkWidget *sw = e2_widget_add_sw (
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (filetypes_dialog)),
#else
		GTK_DIALOG (filetypes_dialog)->vbox,
#endif
		GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC, TRUE, E2_PADDING_SMALL);
	gtk_widget_set_size_request (sw, 1, 400);

#ifdef USE_GTK3_0
	GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
#endif
#ifdef USE_GTK3_8
	gtk_container_add (GTK_CONTAINER (sw), hbox);
#else
	gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (sw), hbox);
#endif
	gtk_viewport_set_shadow_type (
#ifdef USE_GTK2_14
		GTK_VIEWPORT (gtk_bin_get_child (GTK_BIN (sw))),
#else
		GTK_VIEWPORT (GTK_BIN (sw)->child),
#endif
		GTK_SHADOW_NONE);

	//categories scrolled window
	GtkWidget *cat_sw = e2_widget_get_sw (GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC,
		GTK_SHADOW_IN);
	gtk_box_pack_start (GTK_BOX (hbox), cat_sw, TRUE, TRUE, 0);
	//categories store and treeview
	rt->stores[E2_CATPANE] = GTK_TREE_STORE (set->ex.tree.model);
	rt->views[E2_CATPANE] = gtk_tree_view_new_with_model (set->ex.tree.model);
	gtk_tree_view_set_reorderable (GTK_TREE_VIEW (rt->views[E2_CATPANE]), TRUE);
#ifdef USE_GTK2_10
	gtk_tree_view_set_enable_tree_lines (GTK_TREE_VIEW (rt->views[E2_CATPANE]), TRUE);
#endif
	g_signal_connect (rt->views[E2_CATPANE], "popup-menu",
		G_CALLBACK (_e2_edftdlg_popup_menu_cb), rt);
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new ();
	g_object_set (G_OBJECT (renderer), "editable", TRUE, NULL);
	g_object_set_data (G_OBJECT (renderer), "col_num", GINT_TO_POINTER (-1));	//flag for categories renderer
	g_signal_connect (G_OBJECT (renderer), "edited", G_CALLBACK (_e2_edftdlg_cell_edited_cb), rt);
	gtk_tree_view_insert_column_with_attributes
		(GTK_TREE_VIEW (rt->views[E2_CATPANE]), -1, _("Categories"), renderer, "text", 0, NULL);
	//hide the tree expanders
#ifdef USE_GTK2_12
	gtk_tree_view_set_show_expanders (GTK_TREE_VIEW (rt->views[E2_CATPANE]), FALSE);
#else
	GtkTreeViewColumn *expander_col = gtk_tree_view_column_new ();
	gtk_tree_view_insert_column (GTK_TREE_VIEW (rt->views[E2_CATPANE]), expander_col, 1);
	gtk_tree_view_set_expander_column (GTK_TREE_VIEW (rt->views[E2_CATPANE]), expander_col);
	gtk_tree_view_column_set_visible (expander_col, FALSE);
#endif
	gtk_container_add (GTK_CONTAINER (cat_sw), rt->views[E2_CATPANE]);
	gtk_widget_set_size_request (cat_sw, 150, 1);

	//extensions scrolled window
	GtkWidget *ext_sw = e2_widget_get_sw (GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC,
		GTK_SHADOW_IN);
	gtk_box_pack_start (GTK_BOX (hbox), ext_sw, TRUE, TRUE, 0);
	//extensions liststore and treeview
	//store has 1 column, for displayed extensions
	rt->stores[E2_EXTPANE] = gtk_tree_store_new (1, G_TYPE_STRING);
	rt->views[E2_EXTPANE] = gtk_tree_view_new_with_model (GTK_TREE_MODEL (rt->stores[E2_EXTPANE]));
	g_object_unref (rt->stores[E2_EXTPANE]);	//destroy store with view
	gtk_tree_view_set_reorderable (GTK_TREE_VIEW (rt->views[E2_EXTPANE]), TRUE);
	g_signal_connect (rt->views[E2_EXTPANE], "popup-menu",
		G_CALLBACK (_e2_edftdlg_popup_menu_cb), rt);
	renderer = gtk_cell_renderer_text_new ();
	g_object_set (G_OBJECT (renderer), "editable", TRUE, NULL);
	g_object_set_data (G_OBJECT (renderer), "col_num", GINT_TO_POINTER (0));
	g_signal_connect (G_OBJECT (renderer), "edited", G_CALLBACK (_e2_edftdlg_cell_edited_cb), rt);
	gtk_tree_view_insert_column_with_attributes
		(GTK_TREE_VIEW (rt->views[E2_EXTPANE]), -1, _("Extensions"), renderer, "text", 0, NULL);
	gtk_container_add (GTK_CONTAINER (ext_sw), rt->views[E2_EXTPANE]);
	gtk_widget_set_size_request (ext_sw, 100, 1);
	GtkTreeSelection *selection = gtk_tree_view_get_selection
		(GTK_TREE_VIEW (rt->views[E2_EXTPANE]));
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

	//commands scrolled window
	GtkWidget *cmd_sw = e2_widget_get_sw (GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC,
		GTK_SHADOW_IN);
	gtk_box_pack_start (GTK_BOX (hbox), cmd_sw, TRUE, TRUE, 0);
	//commands liststore and treeview
	//store has columns for displayed label, command
	rt->stores[E2_CMDPANE] = gtk_tree_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
	rt->views[E2_CMDPANE] = gtk_tree_view_new_with_model (GTK_TREE_MODEL (rt->stores[E2_CMDPANE]));
	g_object_unref (rt->stores[E2_CMDPANE]);	//destroy store with view
	gtk_tree_view_set_reorderable (GTK_TREE_VIEW (rt->views[E2_CMDPANE]), TRUE);
	g_signal_connect (rt->views[E2_CMDPANE], "popup-menu",
		G_CALLBACK (_e2_edftdlg_popup_menu_cb), rt);
	renderer = gtk_cell_renderer_text_new ();
	g_object_set (G_OBJECT (renderer), "editable", TRUE, NULL);
	g_object_set_data (G_OBJECT (renderer), "col_num", GINT_TO_POINTER (0));
	g_signal_connect (G_OBJECT (renderer), "edited", G_CALLBACK (_e2_edftdlg_cell_edited_cb), rt);
	gtk_tree_view_insert_column_with_attributes
		(GTK_TREE_VIEW (rt->views[E2_CMDPANE]), 0, _("Labels"), renderer, "text", 0, NULL);
	GtkTreeViewColumn *column =
	gtk_tree_view_get_column (GTK_TREE_VIEW (rt->views[E2_CMDPANE]), 0);
	g_object_set (G_OBJECT (column), "resizable", TRUE, "min-width", 120, NULL);
	renderer = gtk_cell_renderer_text_new ();
	g_object_set (G_OBJECT (renderer), "editable", TRUE, NULL);
	g_object_set_data (G_OBJECT (renderer), "col_num", GINT_TO_POINTER (1));
	g_signal_connect (G_OBJECT (renderer), "edited", G_CALLBACK (_e2_edftdlg_cell_edited_cb), rt);
	gtk_tree_view_insert_column_with_attributes
		(GTK_TREE_VIEW (rt->views[E2_CMDPANE]), 1, _("Commands"), renderer, "text", 1, NULL);
	column = gtk_tree_view_get_column (GTK_TREE_VIEW (rt->views[E2_CMDPANE]), 1);
	g_object_set (G_OBJECT (column), "resizable", TRUE, NULL);

	gtk_container_add (GTK_CONTAINER (cmd_sw), rt->views[E2_CMDPANE]);
	gtk_widget_set_size_request (cmd_sw, 400, 1);
	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->views[E2_CMDPANE]));
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

	e2_option_tree_backup (set);	//keep a backup in case user cancels
	//fill the stores
	GtkTreePath *path;
	GtkTreeIter iter;
	if (open_category != NULL
		&& e2_tree_find_iter_from_str (set->ex.tree.model, 0, open_category, &iter, FALSE))
	{
		path = gtk_tree_model_get_path (set->ex.tree.model, &iter);
		gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (rt->views[E2_CATPANE]),
				path, NULL, FALSE, 0, 0);
		selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->views[E2_CATPANE]));
		gtk_tree_selection_select_path (selection, path);
		_e2_edftdlg_update_category (path, rt);
		if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (rt->stores[E2_CMDPANE]), &iter))
		{
			selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->views[E2_CMDPANE]));
			gtk_tree_selection_select_iter (selection, &iter);
		}
		gtk_widget_grab_focus (rt->views[E2_CMDPANE]);
	}
	else if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
	{
		path = gtk_tree_model_get_path (set->ex.tree.model, &iter);
		_e2_edftdlg_update_category (path, rt);
	}

	//handle things
	g_signal_connect (G_OBJECT (filetypes_dialog), "key-press-event",
		G_CALLBACK (_e2_edftdlg_key_press_cb), rt);
//	g_signal_connect (G_OBJECT (filetypes_dialog), "delete-event",
//		G_CALLBACK (_e2_edftdlg_cancel_cb), rt);

	g_signal_connect (G_OBJECT (rt->views[E2_CATPANE]), "button-press-event",
		G_CALLBACK (_e2_edftdlg_button_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->views[E2_EXTPANE]), "button-press-event",
		G_CALLBACK (_e2_edftdlg_button_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->views[E2_CMDPANE]), "button-press-event",
		G_CALLBACK (_e2_edftdlg_button_press_cb), rt);

//	g_signal_connect (G_OBJECT (rt->views[E2_CATPANE]), "cursor-changed",
//		G_CALLBACK (_e2_edftdlg_cat_cursor_move_cb), rt);
	selection = gtk_tree_view_get_selection
		(GTK_TREE_VIEW (rt->views[E2_CATPANE]));
	g_signal_connect (G_OBJECT (selection), "changed",
		G_CALLBACK (_e2_edftdlg_cat_sel_change_cb), rt);
	//traps for pane-changes by tab key or mouse click
	g_signal_connect (G_OBJECT (rt->views[E2_CATPANE]), "focus-in-event",
		G_CALLBACK (_e2_edftdlg_focus_in_cb), rt);
	g_signal_connect (G_OBJECT (rt->views[E2_EXTPANE]), "focus-in-event",
		G_CALLBACK (_e2_edftdlg_focus_in_cb), rt);
	g_signal_connect (G_OBJECT (rt->views[E2_CMDPANE]), "focus-in-event",
		G_CALLBACK (_e2_edftdlg_focus_in_cb), rt);

	//add buttons, custom and standard
	E2_Button local_btn = { _("_Up"), STOCK_NAME_GO_UP,
		_("Move the selected row one place up"),
		E2_BTN_TIPPED, E2_BTN_TIPPED, E2_RESPONSE_USER1 };
	e2_dialog_add_defined_button (filetypes_dialog, &local_btn);

	local_btn.label = _("_Down");
	local_btn.name = STOCK_NAME_GO_DOWN;
	local_btn.tip = _("Move the selected row one place down");
	local_btn.response = E2_RESPONSE_USER2;
	e2_dialog_add_defined_button (filetypes_dialog, &local_btn);

	local_btn.label = _("_Add");
	local_btn.name = STOCK_NAME_ADD;
	local_btn.tip = _("Add a row after the currently selected one");
	local_btn.response = E2_RESPONSE_USER3;
	e2_dialog_add_defined_button (filetypes_dialog, &local_btn);

//	e2_button_derive (&local_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);
	local_btn = E2_BUTTON_DISCARD;
	local_btn.showflags |= E2_BTN_DEFAULT;

	E2_BUTTON_APPLY.showflags &= ~E2_BTN_DEFAULT;	//CHECKME local copy ?
	e2_dialog_show (filetypes_dialog, app.main_window, 0,
		&E2_BUTTON_REMOVE, &E2_BUTTON_MORE, &local_btn, &E2_BUTTON_APPLY, NULL);
}

#endif	//ndef OLDFTDLG

  /***************************/
 /**** what-to-do dialog ****/
/***************************/

typedef struct _E2_FileTypeDlg2Runtime
{
	GtkWidget *view_btn;
	GtkWidget *open_btn;
	gchar *utfpath;	//UTF8 absolute path of item
} E2_FileTypeDlg2Runtime;

/**
@brief unknown filetype dialog response-signal callback

@param dialog the dialog where the response was triggered
@param response the number assigned the activated widget
@param store pointer to store for enumof user's choice

@return
*/
static void _e2_filetype_dialog_response_cb (GtkDialog *dialog, gint response,
	gint *store)
{
//	NEEDOPENBGL
//	NEEDCLOSEBGL
	printd (DEBUG, "response_cb (dialog:_,response:%d,rt:_)", response);
	*store = response;
}
/**
@brief create and show dialog for handling unknown or ambiguous filetype
@a newtype should be TRUE when @a filename does have an extension
@a newtype and @a ambig should not both be TRUE
Assumes BGL is closed
@param localpath absolute or relative path of item to be processed, localised string
@param text TRUE to include view or edit option in the dialog
@param ambig TRUE to enable choice to execute or open item
@param newtype TRUE to include a button to add a new filetype

@return enumerator signalling how @a filename was "actioned" via the dialog
*/
gint e2_filetype_dialog_create (VPATH *localpath, gboolean text, gboolean ambig,
	gboolean newtype)
{
	gint response;
	gint retval;
	gboolean addpath;
	gchar *usepath, *dir, *base, *title, *public, *message;
	struct stat sb;

	E2_FileTypeDlg2Runtime rt;

	usepath = VPSTR (localpath);
//	printd (DEBUG, "e2_filetype_dialog_create: %s", usepath);
	addpath = !g_path_is_absolute (usepath);
	if (addpath)
	{
	//FIXME race (unlikely) if dir goes and cd happens, worse if past virtual root
#ifdef E2_VFS
		if (localpath->spacedata == other_view->spacedata)
			usepath = e2_utils_dircat (other_view, usepath, TRUE);
		else
#endif
			usepath = e2_utils_dircat (curr_view, usepath, TRUE);
	}

	rt.utfpath = D_FILENAME_FROM_LOCALE (usepath);
	title = (ambig) ? _("ambiguous filetype") : _("unrecognised filetype");
	public = g_markup_escape_text (rt.utfpath, -1);
	dir = g_path_get_dirname (public);
	base = g_path_get_basename (public);
	message = g_strdup_printf (
		_("What would you like to do with\n<b>%s</b>\nin %s ?"), base, dir);
	GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION, message, title,
		(ResponseFunc)_e2_filetype_dialog_response_cb, &response);
	g_free (public);
	g_free (dir);
	g_free (base);
	g_free (message);

	//can always add to category, even if item does not presently exist
	if (newtype && !ambig)
		e2_dialog_add_custom_button_full (dialog, FALSE,
		GTK_RESPONSE_ACCEPT, _("_Add.."), "gtk-preferences",
		_("Create a new filetype for this extension, or add it to an existing filetype"),
		NULL, NULL);

#ifdef E2_VFS
	VPATH ddata = { usepath, localpath->spacedata };
	if (!e2_fs_stat (&ddata, &sb E2_ERR_NONE()))
#else
	if (!e2_fs_stat (usepath, &sb E2_ERR_NONE()))
#endif
	{
		if (S_ISREG (sb.st_mode))
		{
			if (sb.st_size == 0)
				rt.view_btn = e2_dialog_add_custom_button_full (dialog, FALSE,
					E2_RESPONSE_USER1, _("_Edit"), STOCK_NAME_EDIT,
					_("Edit the file"), NULL, NULL);
			else if (text)
				rt.view_btn = e2_dialog_add_custom_button_full (dialog, FALSE,
					E2_RESPONSE_USER2, _("_View"), "view"E2ICONTB,
					_("Read the file"), NULL, NULL);
		}
		if (ambig)
		{
			e2_dialog_add_custom_button_full (dialog, FALSE,
				E2_RESPONSE_USER5, _("_Run"), STOCK_NAME_EXECUTE,
					_("Execute the item"), NULL, NULL);
			rt.open_btn = e2_dialog_add_custom_button_full (dialog, TRUE,
				E2_RESPONSE_USER6, _("_Open"), STOCK_NAME_OPEN,	//FIXME better icon
					_("Open with the default application"), NULL, NULL);
		}
		else
			rt.open_btn = e2_dialog_add_custom_button_full (dialog, TRUE,
				E2_RESPONSE_USER3, _("_Open.."), "open_with"E2ICONTB,
					_("Enter a command with which to open the file"), NULL, NULL);
		gtk_widget_grab_focus (rt.open_btn);
	}

	//block until user selects something
	e2_dialog_show (dialog, NULL, E2_DIALOG_BLOCKED, &E2_BUTTON_CANCEL, NULL);

	if (addpath)
		g_free (usepath);

	gtk_widget_destroy (GTK_WIDGET (dialog));
	gtk_widget_grab_focus (curr_view->treeview);

//tag E2_BADQUOTES CHECKME used for action arg
	gchar *item = e2_utils_quote_string (rt.utfpath);
	gchar *action = NULL;

	switch (response)
	{
	/* these callbacks handle a specific filename, so are usable for output
	   pane selections as well as filelist selections
	   item is a quouted absolute UTF-8 path string */
		case GTK_RESPONSE_ACCEPT:	//add
			//open the filetypes config dialog with no extension selected
			e2_filetype_config_show (NULL);
			break;
		case E2_RESPONSE_USER1:	//edit
			action = g_strconcat (_A(6),".",_A(46), NULL);
			break;
		case E2_RESPONSE_USER2:	//view
			action = g_strconcat (_A(6),".",_A(109), NULL);
			break;
		case E2_RESPONSE_USER3:	//open with
			action = g_strconcat (_A(6),".",_A(69), NULL);	//Q'd action
			break;
		case E2_RESPONSE_USER5:	//execute
			//CHECKME no quotes in this case ?
			e2_command_run (item, E2_COMMAND_RANGE_DEFAULT, app.main_window //no better valid widget now
#ifdef E2_COMMANDQ
			, FALSE
#endif
			);
		default:
			break;
	}

	if (action != NULL)
	{
		OPENBGL
		e2_action_run_simple_from (action, item, dialog);
		CLOSEBGL
		g_free (action);
	}
	g_free (item);

	//send back "handled" type flag to caller
	switch (response)
	{
		case E2_RESPONSE_USER1:	//edit
		case E2_RESPONSE_USER2:	//view
		case E2_RESPONSE_USER3:	//open with
		case E2_RESPONSE_USER5:	//exec
			retval = E2_TYPE_HANDLED;
			break;
		case GTK_RESPONSE_ACCEPT:	//add
		case E2_RESPONSE_USER6:	//open with default (back in the calling code)
			retval = E2_TYPE_NOTHANDLED;
			break;
		default:	//this includes GTK_RESPONSE_DELETE_EVENT
			retval = E2_TYPE_CANCELLED;
			break;
	}
	return retval;
}
