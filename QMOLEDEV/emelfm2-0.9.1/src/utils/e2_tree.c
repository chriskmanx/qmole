/* $Id: e2_tree.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

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
#include "e2_tree.h"

/**
@brief try to find tree iter in @a model, at or after @a iter, which contains string @a search
This is a wrapper for the real search function. It performs several checks
The scan traverses @a model checking each iter and any children, depth-first
@param model treemodel to be interrogated
@param column index of the column in @a model to scan
@param search the string to be searched for
@param iter pointer to treeiter for model interrogation (start and match)
@param with_children TRUE if matched iter must have child(ren)

@return TRUE if the string was found
*/
gboolean e2_tree_find_iter_from_str (GtkTreeModel *model, gint column,
	const gchar *search, GtkTreeIter *iter, gboolean with_children)
{
	if (model == NULL)
		return FALSE;
	gint num_columns = gtk_tree_model_get_n_columns (model);
	if (column > (num_columns - 1))
	{
		printd (WARN, "bad call of e2_tree_find_iter_from_str");
		printd (WARN, "requesting column %d of %d columns", column + 1, num_columns);
		return FALSE;
	}
	if (gtk_tree_model_get_column_type (model, column) != G_TYPE_STRING)
	{
		printd (WARN, "bad call of e2_tree_find_iter_from_str");
		printd (WARN, "column %d is not of type string", column + 1);
		return FALSE;
	}

	if (gtk_tree_model_get_iter_first (model, iter))
		return e2_tree_find_iter_from_str_simple (model, column, search,
			iter, with_children);
	return FALSE;
}
/**
@brief starting from @a iter, check all iters at the same level as @a iter in
  @a model, and all their descendants, to find the first iter which contains @a search
@a iter is set to matching iter if any, or is unchanged if no match is found
@param model treemodel to be interrogated
@param column index of the column in @a model to scan
@param search the string to be searched for, may be NULL
@param iter pointer to initial treeiter for model interrogation
@param with_children TRUE if matched iter must have child(ren)

@return TRUE if the string was found
*/
gboolean e2_tree_find_iter_from_str_simple (GtkTreeModel *model, gint column,
	const gchar *search, GtkTreeIter *iter, gboolean with_children)
{
	GtkTreeIter backup;
	backup = *iter;
	do
	{
		GtkTreeIter iter2;
		gchar *value;
		gtk_tree_model_get (model, iter, column, &value, -1);
		if (value != NULL)
		{
			if (search && strcmp (value, search) == 0)
			{
				if (!with_children ||
					gtk_tree_model_iter_children (model, &iter2, iter))
				{
					g_free (value);
					return TRUE;
				}
			}
			g_free (value);
		}
		else if (search == NULL)
		{
			if (!with_children ||
				gtk_tree_model_iter_children (model, &iter2, iter))
					return TRUE;
		}

		if (gtk_tree_model_iter_children (model, &iter2, iter))
		{
			if (e2_tree_find_iter_from_str_simple (model, column, search,
					&iter2, with_children))
			{
				*iter = iter2;
				return TRUE;
			}
		}
	} while (gtk_tree_model_iter_next (model, iter));
	*iter = backup;
	return FALSE;
}
/**
@brief starting from @a iter, check all iters at the same level as @a iter in
@a model, to find the first one which contains string @a search in column @a colnum
@a iter is set to the matching iter, or is unchanged if no match is found
@param model treemodel to be interrogated
@param column index of the column in @a model to scan
@param search the string to be searched for
@param iter pointer to treeiter for model interrogation, start

@return TRUE if the string was found
*/
gboolean e2_tree_find_iter_from_str_same (GtkTreeModel *model, gint column,
	const gchar *search, GtkTreeIter *iter)
{
	GtkTreeIter backup;
	backup = *iter;
	do
	{
		gchar *value;
		gtk_tree_model_get (model, iter, column, &value, -1);
		if (value != NULL)
		{
			if (!strcmp (value, search))
			{
				g_free (value);
				return TRUE;
			}
			g_free (value);
		}
	} while (gtk_tree_model_iter_next (model, iter));
	*iter = backup;
	return FALSE;
}
//#ifdef E2_TREEINCREMENT
/**
@brief find or otherwise create an iter in @a model which corresponds to @a search
@a search is formatted like level1[.level2.level3 ...] Each "." (if any)
indicates a child of the previous segment
@a iter is set to the existing or newly-created iter in the model
@param model treemodel to be interrogated
@param column index of the column in @a model to scan
@param iter pointer to treeiter for model interrogation and result
@param search '.'-segmented string describing the iter to be found or created

@return TRUE if the iter was found, FALSE if it had to be created
*/
gboolean e2_tree_get_lowest_iter_for_str (GtkTreeModel *model, gint column,
	GtkTreeIter *iter, const gchar *search)
{
	gchar **split = g_strsplit (search, ".", -1);
	gint i;
	GtkTreeIter child;
	if (gtk_tree_model_get_iter_first (model, iter)
		&& e2_tree_find_iter_from_str_simple (model, column, split[0], iter, FALSE))
	{	//at least the first level exists
		for (i = 1; split[i] != NULL; i++)
		{
			if (*split[i] == '\0')
				continue;	//ignore it
			else if (gtk_tree_model_iter_children (model, &child, iter))
			{
				gboolean found = FALSE;
				*iter = child;
				do
				{
					gchar *value;
					gtk_tree_model_get (model, iter, column, &value, -1);
					found = (!strcmp (value, split[i]));
					g_free (value);
				} while (!found && gtk_tree_model_iter_next (model, iter));

				if (!found)
					break;
			}
			else
				break;
		}
		if (split[i] == NULL)
		{	//we found everything
			g_strfreev (split);
			return TRUE;
		}
	}
	else
		i = 0;	//we need to create all of the segments

	//populate new row(s)
	GtkTreeStore *store = GTK_TREE_STORE (model);
	for ( ; split[i] != NULL; i++)
	{
		if (*split[i] == '\0')
			continue;	//ignore it
		else if (i == 0)
			gtk_tree_store_append (store, &child, NULL);
		else
			gtk_tree_store_append (store, &child, iter);
		*iter = child;
		gtk_tree_store_set (store, iter, column, split[i], -1);
	}
	g_strfreev (split);
	return FALSE;
}
//#endif	//def E2_TREEINCREMENT
/* *
@brief starting from the first iter in @a model, check all iters to find the
first whose column @a column contains the last "."-separated segment of @a search.
@a search is formatted like level1[.level2.level3 ...] Each "." (if any)
indicates a child of the previous segment. If successful, @a iter is set to the
matching iter in the model. Otherwise, iter is undefined at exit
@param model treemodel to be interrogated
@param column index of the column in @a model to scan
@param iter pointer to treeiter for model interrogation and result
@param search string describing the iter to be searched for

@return TRUE if the string was found
*/
/*gboolean e2_tree_find_lowest_iter_for_str (GtkTreeModel *model, gint column,
	GtkTreeIter *iter, const gchar *search)
{
	if (!gtk_tree_model_get_iter_first (model, iter))
	{
//		*iter = ?;
		return FALSE;
	}

	gchar **split = g_strsplit (search, ".", -1);
	//get a place to start scanning
	gboolean retval =
	e2_tree_find_iter_from_str_simple (model, column, split[0], iter, FALSE);

	if (retval)
	{
		gint i;
		for (i = 1; split[i] != NULL; i++)	//we've already found 0
		{
			GtkTreeIter child;

			if (*split[i] == '\0')
			{
//				retval = FALSE;	//no valid empty iter-names in the store
//				break;
				continue; //tolerance
			}
			if (!gtk_tree_model_iter_children (model, &child, iter))
			{
				retval = FALSE;
				break;
			}
			gboolean found = FALSE;
			*iter = child;
			do
			{
				gchar *value;
				gtk_tree_model_get (model, iter, column, &value, -1);
				found = (!strcmp (value, split[i]));
				g_free (value);
			} while (!found && gtk_tree_model_iter_next (model, iter));

			if (!found)
			{
//				*iter = child; //we found this much
				retval = FALSE;
				break;
			}
		}
	}
	g_strfreev (split);
	return retval;
}
*/
/* *
@brief get string in column @a columm in the last row in @a model

@param model treemodel to be interrogated
@param column index of the column in @a model from which to get the return value

@return pointer to newly-allocated copy of the string from the model, or NULL if there's a problem
*/
/* UNUSED
gchar *e2_tree_get_last_string (GtkTreeModel *model, gint column)
{
	if (gtk_tree_model_get_column_type (model, column) != G_TYPE_STRING)
		return NULL;
	GtkTreeIter iter;
/ *FIXME: don't iterate through the whole model to get the last row
	GtkTreePath *path = gtk_tree_path_new_first ();
	while (gtk_tree_model_get_iter (model, &iter, path))
		gtk_tree_path_next (path);
	gtk_tree_path_prev (path);

	gchar *value = NULL;
	if (gtk_tree_model_get_iter (model, &iter, path))
		gtk_tree_model_get (model, &iter, column, &value, -1);
	gtk_tree_path_free (path);
	return value; * /
	GtkTreeIter *parent = NULL;	//initially check root node
	gint children;
	while ((children = gtk_tree_model_iter_n_children (model, parent)) > 0)
	{
		// go to last child at this level
		gtk_tree_model_iter_nth_child (model, &iter, parent, children-1);
		parent = &iter;	//after the root node, we use the actual
	}
	if (parent == NULL)
		return NULL;	//nothing in the model
	gchar *value;
	gtk_tree_model_get (model, &iter, column, &value, -1);
	return value;
} */
/* *
@brief determine the number of rows in treestore model @a model
This is mainly for debugging
@param model treemodel to be interrogated

@return the no. of rows
*/
/*guint e2_tree_store_count (GtkTreeModel *model)
{
	GtkTreeIter iter;
	if (!gtk_tree_model_get_iter_first (model, &iter))
		return 0;
	//CHECKME is there a way to start at the real root of the tree ?
	guint count = 0;
	GtkTreePath *path = gtk_tree_path_new_first ();
	while (gtk_tree_model_get_iter (model, &iter, path))
	{
		GNode *rootnode = iter.user_data;
		count += g_node_n_nodes (rootnode, G_TRAVERSE_ALL);
		gtk_tree_path_next (path);
	}
	gtk_tree_path_free (path);
	return count;
} */
/**
@brief create a tree row reference for @a iter in @a store

@param store pointer to treestore
@param iter pointer to treeiter to be referenced

@return the reference
*/
GtkTreeRowReference *e2_tree_iter_to_ref (GtkTreeStore *store, GtkTreeIter *iter)
{
	GtkTreePath *path = gtk_tree_model_get_path (GTK_TREE_MODEL (store), iter);
	GtkTreeRowReference *ref = gtk_tree_row_reference_new
		(GTK_TREE_MODEL (store), path);
	gtk_tree_path_free (path);
	return ref;
}
/**
@brief set @a iter to match @a ref for @a store

@param store pointer to treestore
@param ref pointer to treerow reference
@param iter pointer to treeiter to be set

@return TRUE if @a iter is valid
*/
gboolean e2_tree_ref_to_iter (GtkTreeStore *store, GtkTreeRowReference *ref,
	GtkTreeIter *iter)
{
	if (gtk_tree_row_reference_valid (ref))
	{
		GtkTreePath *path = gtk_tree_row_reference_get_path (ref);
		gboolean retval = gtk_tree_model_get_iter (GTK_TREE_MODEL (store),
			iter, path);
		gtk_tree_path_free (path);
		return retval;
	}
	return FALSE;
}
/**
@brief tree expand-all callback

@param widget UNUSED the widget which activated the callback
@param treeview the treeview to be collapsed

@return
*/
void e2_tree_expand_all_cb (GtkMenuItem *widget, GtkTreeView *treeview)
{
	NEEDCLOSEBGL
	gtk_tree_view_expand_all (treeview);
	NEEDOPENBGL
}
/**
@brief tree collapse-all callback

@param widget UNUSED the widget which activated the callback
@param treeview the treeview to be collapsed

@return
*/
void e2_tree_collapse_all_cb (GtkMenuItem *widget, GtkTreeView *treeview)
{
	NEEDCLOSEBGL
	gtk_tree_view_collapse_all (treeview);
	NEEDOPENBGL
}
/**
@brief find previous iter in a tree model
This is because gtk doesn't have one.
On entry, @a iter is assumed valid
Sets @a iter to point to the node preceding it at the current level.

@param model the tree model being interrogated
@param iter pointer to reference iter

@return TRUE if a prior node was found, and iter was set accordingly
*/
gboolean e2_tree_iter_previous (GtkTreeModel *model, GtkTreeIter *iter)
{
	GtkTreePath *path = gtk_tree_model_get_path (model, iter);
	gboolean retval = gtk_tree_path_prev (path);
	if (retval)
		retval = gtk_tree_model_get_iter (model, iter, path);
	gtk_tree_path_free (path);
	return retval;
}
/**
@brief add treerowreferences for row and its descendants to list

A depth-first search scan creates references for the specified iter and
all its descendants, appending each ref. to a specified glist.
It is recursive, to handle descendants

@param model the treemodel that is being processed
@param iter _pointer_ to tree iter to be processed
@param rowrefs **glist that holds the result

@return
*/
static void _e2_tree_reference_treerow
	(GtkTreeModel *model, GtkTreeIter *iter, GList **rowrefs)
{
	GtkTreePath *path = gtk_tree_model_get_path (model, iter);
	GtkTreeRowReference *ref;
	if ((ref = gtk_tree_row_reference_new (model, path)) != NULL)
		*rowrefs = g_list_append (*rowrefs, ref);
	gtk_tree_path_free (path);

	gint children;
	if ((children = gtk_tree_model_iter_n_children (model, iter)) > 0)
	{
		GtkTreeIter iter2;
		gint n;
		for (n=0; n<children; n++)
		{
			gtk_tree_model_iter_nth_child (model, &iter2, iter, n);
			_e2_tree_reference_treerow (model, &iter2, rowrefs);
		}
	}
}
/**
@brief delete selected row(s), if any, from @a treeview

@param treeview where the action is to occur

@return
*/
void e2_tree_delete (GtkTreeView *treeview)
{
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		GtkTreeModel *model;
		GList *tmp, *rowrefs = NULL;
		GList *selpaths = gtk_tree_selection_get_selected_rows (sel, &model);
		//identify a place to goto after the deletion
		//(current place, if we're at the start of the tree)
		GtkTreePath *newpath = gtk_tree_path_copy (selpaths->data);
		if (!gtk_tree_path_prev (newpath))
			if (gtk_tree_path_get_depth (newpath) > 1)
				gtk_tree_path_up (newpath);
		GtkTreePath *path;
		GtkTreeIter iter;

		//create treerowrefs for all selected rows
		for (tmp = selpaths; tmp!=NULL; tmp=tmp->next)
		{
			path = (GtkTreePath *) tmp->data;
			if (gtk_tree_model_get_iter (model, &iter, path))
				_e2_tree_reference_treerow (model, &iter, &rowrefs);
			gtk_tree_path_free (path);
		}
		g_list_free (selpaths);

		for (tmp = rowrefs; tmp != NULL; tmp = tmp->next)
		{
			path = gtk_tree_row_reference_get_path ((GtkTreeRowReference *) tmp->data);
			if (path != NULL)
			{
				if (gtk_tree_model_get_iter (model, &iter, path))
				{
					//gtk_tree_selection_unselect_path (sel, path);	//needed ?
					//this deletes all children too
					gtk_tree_store_remove (GTK_TREE_STORE (model), &iter);
				}
				gtk_tree_path_free (path);
			}
			gtk_tree_row_reference_free ((GtkTreeRowReference *) tmp->data);
		}
		g_list_free (rowrefs);

		//go to the new place
		if (!gtk_tree_model_get_iter (model, &iter, newpath)
			&& gtk_tree_model_get_iter_first (model, &iter))
		{
			gtk_tree_path_free (newpath);
			newpath = gtk_tree_model_get_path (model, &iter);
		}
		gtk_tree_view_set_cursor (treeview, newpath,
			gtk_tree_view_get_column (treeview, 0), FALSE);

		gtk_tree_path_free (newpath);
	}
}

/**
@brief add treemodel row, and all its descendants, to list

A recursive depth-first copy process puts the treepath and corresponding
column values for @a iter into an array, then appends that array to
the list @a rowscopy

@param model the treemodel that is being processed
@param iter pointer to tree iter for the row to be processed
@param columncount no. of columns in @a model
@param rowscopy store for pointer to list that holds the result

@return
*/
static void _e2_tree_copy_branch (GtkTreeModel *model, GtkTreeIter *iter,
	gint columncount, GList **rowscopy)
{
	//probably, this doesn't need to be a dynamic array
	GArray *row_array = g_array_sized_new (FALSE, TRUE, sizeof(GValue), columncount + 1);
	//glib doesn't actually clear stuff, so we do so
	memset (row_array->data, 0, sizeof(GValue)*(columncount + 1));
	//add path of copied node to the start of the buffer entry, to support proper pasting
	GValue *valptr = (GValue*)row_array->data;
	g_value_init (valptr, G_TYPE_POINTER);
	GtkTreePath *path = gtk_tree_model_get_path (model, iter);
	g_value_set_pointer (valptr, path);
	//add store-column values to the array
	gint i;
	for (i = 0; i < columncount ; i++)
		gtk_tree_model_get_value (model, iter, i, ++valptr);
	row_array->len = columncount + 1;
	//add this row to the buffer list
	*rowscopy = g_list_append (*rowscopy, row_array);
	//recurse to handle any child(ren)
	gint children = gtk_tree_model_iter_n_children (model, iter);
	if (children > 0)
	{
		GtkTreeIter iter2;
		gint n;
		for (n=0; n<children; n++)
		{
			gtk_tree_model_iter_nth_child (model, &iter2, iter, n);
			_e2_tree_copy_branch (model, &iter2, columncount, rowscopy);
		}
	}
}
/**
@brief create a list of selected rows in @a treeview

This is recursive, to handle descendants of any selected row, too

@param treeview the treeview to be processed

@return the list of arrays, or NULL if nothing is selected
*/
GList *e2_tree_copy (GtkTreeView *treeview)
{
	GList *rowscopied = NULL;
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		GtkTreeModel *model;
		GList *rowpath;
		GList *selpaths = gtk_tree_selection_get_selected_rows (sel, &model);
		gint columns = gtk_tree_model_get_n_columns (model);

		//copy the selected rows and their descendants
		for (rowpath = selpaths; rowpath != NULL; rowpath = rowpath->next)
		{
			GtkTreeIter iter;
			if (gtk_tree_model_get_iter (model, &iter, (GtkTreePath *) rowpath->data))
				_e2_tree_copy_branch (model, &iter, columns, &rowscopied);
			gtk_tree_path_free ((GtkTreePath *) rowpath->data);
		}

		g_list_free (selpaths);

		printd (DEBUG, "copied %d rows", g_list_length (rowscopied));
	}
	return rowscopied;
}

/*static gboolean _e2_tree_path_compare (GtkTreePath *a, GtkTreePath *b)
{
	gint path_equal = gtk_tree_path_compare (a, b);
	return !path_equal;
} */
/**
@brief paste tree rows into @a treeview

Row(s) are inserted after the 1st selected row in @a treeview,
as child(ren) if the path depth is appropriate
It is assumed here that the buffer hash has pastable data

@param rowscopied list of copied value-arrays
@param treeview the treeview to be processed

@return
*/
void e2_tree_paste (GList *rowscopied, GtkTreeView *treeview)
{
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	if (gtk_tree_selection_count_selected_rows (sel) > 0)
	{
		//get source-view path for the 1st buffered item, the
		//root of the copied branch (or one of several roots)
		GArray *value_array = rowscopied->data;
		GValue *value = &g_array_index (value_array, GValue, 0);
//		gchar *pathstring = g_value_dup_string (value);
//		GtkTreePath *path = gtk_tree_path_new_from_string (pathstring);
		GtkTreePath *rootpath = (GtkTreePath *) g_value_get_pointer (value);
		gint copyrootdepth = gtk_tree_path_get_depth (rootpath);
//		gtk_tree_path_free (path);
		//store for references (hence iters) needed to preserve relations
		//among pasted rows
		GHashTable *ref_hash = NULL;
		GtkTreeRowReference *ref;

		GtkTreeModel *model;
		GList *selpaths = gtk_tree_selection_get_selected_rows (sel, &model);
		gint columns = gtk_tree_model_get_n_columns (model);
		GList *tmp;
		GtkTreeIter local1;
		GtkTreeIter local2;
		GtkTreeIter *iter = &local1;
		GtkTreeIter *parent = &local2;

		//find a selected item where it's valid to paste
		for (tmp = selpaths; tmp != NULL; tmp = tmp->next)
		{
			if (gtk_tree_model_get_iter (model, iter, (GtkTreePath *) tmp->data))
			{
				gint thisdepth = gtk_tree_path_get_depth (tmp->data);
				if (thisdepth == copyrootdepth)
				{	//the selected row is a valid sibling of the buffered row(s)
					//paste the root of the copied branch
					printd (DEBUG, "pasting branch root, at depth %d", thisdepth);
/*					if (gtk_tree_model_iter_parent (model, parent, iter))
					{	//pasted row (iter) will be _pre_pended to parent's children
						gtk_tree_store_insert_after (GTK_TREE_STORE (model), iter, parent, NULL);
					}
					else //parent is the root node
					{ */
					//pasted row (iter) will be inserted after selected row
						gtk_tree_store_insert_after (GTK_TREE_STORE (model), parent, NULL, iter);
						iter = parent;
//					}
					break;
				}
				else if (thisdepth == copyrootdepth - 1)
				{	//the selected item is a parent of the buffered row(s)
					//pasted row (iter) will be appended to parent's children
					printd (DEBUG, "pasting branch root, at depth %d", thisdepth);
					gtk_tree_store_insert_before (GTK_TREE_STORE (model), parent, iter, NULL);
					iter = parent;
					break;
				}
			}
			//keep looking ...
		}
		if (tmp != NULL)
		{	//found a place where it's ok to paste
			//populate the newly-added "root" iter
			gint i;
			for (i = 0; i < columns; i++)
			{
				value = &g_array_index (value_array, GValue, i+1);
				gtk_tree_store_set_value (GTK_TREE_STORE (model), iter, i, value);
			}

			//remember how to find the parent of each pasted iter
			//the hash key (a path from the source treeview) is used
			//determine parent/child relations between pasted rows
			ref_hash = g_hash_table_new_full (g_str_hash, g_str_equal,
				g_free, (GDestroyNotify) gtk_tree_row_reference_free);
//			ref_hash = g_hash_table_new_full (g_direct_hash,
//				(GEqualFunc) _e2_tree_path_compare,
//				NULL,	//we don't want to damage the buffered paths
//				(GDestroyNotify) gtk_tree_row_reference_free);
			GtkTreePath *path = gtk_tree_model_get_path (model, iter);
			ref = gtk_tree_row_reference_new (model, path);
			gtk_tree_path_free (path);
//			g_hash_table_insert (ref_hash, pathstring, ref);
			gchar *rootstring = gtk_tree_path_to_string (rootpath);
			g_hash_table_insert (ref_hash, rootstring, ref);

			//now the rest of the list, if any
			//there may be descendant(s) and/or sibling(s) of the initial item
			GtkTreePath *src_path;
			gchar *ppstring;
			GList *tmp2 = rowscopied;
			while ((tmp2 = tmp2->next) != NULL)
			{
				value_array = tmp2->data;
				//get source-view path and find where it should sit in the new tree
				value = &g_array_index (value_array, GValue, 0);
//				pathstring = g_value_dup_string (value);
				src_path = (GtkTreePath *) g_value_get_pointer (value);
				path = gtk_tree_path_copy (src_path);
				//get former parent path string, by truncating the last segment
//				gchar *s = g_strrstr (pathstring, ":");
//				if (s != NULL)
//				{
//					*s = '\0';
				if (gtk_tree_path_up (path))
				{
					//try to find an already-pasted parent iter
					ppstring = gtk_tree_path_to_string (path);
					if (ppstring != NULL)
					{	//we're not at the root node now
						ref = g_hash_table_lookup (ref_hash, ppstring);
						gtk_tree_path_free (path);
						g_free (ppstring);
					}
					else
						ref = NULL;
					if (ref != NULL)
					{
						path = gtk_tree_row_reference_get_path (ref);
						gtk_tree_model_get_iter (model, parent, path);
						gtk_tree_path_free (path);
						//append row (iter) to parent's children
						gtk_tree_store_insert_before (
							GTK_TREE_STORE (model), iter, parent, NULL);
					}
					else
					{	//this is probably a sibling
						ref = g_hash_table_lookup (ref_hash, rootstring);
						path = gtk_tree_row_reference_get_path (ref);
						gtk_tree_model_get_iter (model, parent, path);
						gtk_tree_path_free (path);
						//append row (iter) to as a sibling
						gtk_tree_store_insert_after (
							GTK_TREE_STORE (model), iter, NULL, parent);
					}
				}
				else
				{
					//CHECKME probably can't be at a root already
					printd (DEBUG, "root node problem");
					continue;
				}
				//populate row
				for (i = 0; i < columns; i++)
				{
					value = &g_array_index (value_array, GValue, i+1);
					gtk_tree_store_set_value (GTK_TREE_STORE (model), iter, i, value);
				}
//				g_free (pathstring);
				//remember newly-added iter in case it's a parent
				path = gtk_tree_model_get_path (model, iter);
				ref = gtk_tree_row_reference_new (model, path);
				gtk_tree_path_free (path);
				ppstring = gtk_tree_path_to_string (src_path);
//				g_hash_table_insert (ref_hash, g_value_dup_string (value), ref);
				g_hash_table_insert (ref_hash, ppstring, ref);
			}
			//no change to (single row) selection, as all new rows are after
			if (ref_hash != NULL)
				g_hash_table_destroy (ref_hash);
		}
		g_list_foreach (selpaths, (GFunc) gtk_tree_path_free, NULL);
		g_list_free (selpaths);
	}
}
//#ifdef STORECOPY
/**
@brief helper function to recursively copy a treestore
@param srcmodel treemodel for store being copied
@param srcparent pointer to source treeiter which has been confirmed to have child(ren)
@param deststore the store to which data is being copied
@param destparent pointer to destination store treeiter
@param ncols no. of columns in @a model
@param colnums array of column indices
@param types array of GTypes corresponding to columns
@param values array in which to store values being transferred

@return
*/
static void _e2_tree_copy_descendants (
	GtkTreeModel *srcmodel, GtkTreeIter *srcparent,
	GtkTreeStore *deststore, GtkTreeIter *destparent,
	gint ncols, gint colnums[], GType types[], GValue values[])
{
	GtkTreeIter child;
	if (gtk_tree_model_iter_children (srcmodel, &child, srcparent))
	{
		gint i;
		GtkTreeIter dest;
		do
		{
			//read iter into pointers array
			for (i = 0; i < ncols; i++)
				gtk_tree_model_get_value (srcmodel, &child, i, &values[i]);
#ifdef USE_GTK2_10
			gtk_tree_store_insert_with_valuesv (deststore, &dest, destparent, -1,
				colnums, values, ncols);
			for (i = 0; i < ncols; i++)
				g_value_unset (&values[i]);
#else
			gtk_tree_store_append (deststore, &dest, destparent);
			for (i = 0; i < ncols; i++)
			{
				gtk_tree_store_set_value (deststore, &dest, i, &values[i]);
				g_value_unset (&values[i]);
			}
#endif
			if (gtk_tree_model_iter_has_child (srcmodel, &child))
				_e2_tree_copy_descendants (srcmodel, &child, deststore, &dest,
					ncols, colnums, types, values);
		} while (gtk_tree_model_iter_next (srcmodel, &child));
	}
}
/**
@brief copy a liststore or treestore
This is for when reffing is not enough e.g. need independent sorting in
attached views. Pointers to data are simply copied, so if the "source" data will
be cleared before fhe new store, then all such data must be replicated or otherwise
preserved by code elsewhere which understands what that data really is.
Officially, the new store will be unsorted, though in practice it will be the
same as the old store.
@param model treemodel of the store being copied
@param treetype TRUE for treestore, FALSE for liststore
@param newstore store for pointer to new store

@a return
*/
void e2_tree_store_copy (GtkTreeModel *model, gboolean treetype, gpointer *newstore)
{
	GtkTreeIter src;
	gint i, ncols = gtk_tree_model_get_n_columns (model);
	gint colnums[ncols];
	GType types[ncols];
	GValue values[ncols];

	for (i = 0; i < ncols; i++)
	{
		colnums[i] = i;
		types[i] = gtk_tree_model_get_column_type (model, i);
		memset (&values[i], 0, sizeof (GValue));
	}

	if (treetype)
	{
		GtkTreeStore *newtstore = gtk_tree_store_newv (ncols, types);
		if (gtk_tree_model_get_iter_first (model, &src))
			_e2_tree_copy_descendants (model, NULL, newtstore, NULL,
						ncols, colnums, types, values);

		*newstore = newtstore;
	}
	else
	{
		GtkListStore *newlstore = gtk_list_store_newv (ncols, types);
		if (gtk_tree_model_get_iter_first (model, &src))
		{
			GtkTreeIter dest;
			do
			{
				for (i = 0; i < ncols; i++)
					gtk_tree_model_get_value (model, &src, i, &values[i]);
				gtk_list_store_insert_with_valuesv (newlstore, &dest, -1, colnums, values, ncols);
				for (i = 0; i < ncols; i++)
					g_value_unset (&values[i]);
			} while (gtk_tree_model_iter_next (model, &src));
		}
		*newstore = newlstore;
	}
}
//#endif
