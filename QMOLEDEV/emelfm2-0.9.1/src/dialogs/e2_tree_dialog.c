/* $Id: e2_tree_dialog.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2007-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1996-2007 Steve Baker <ice@mama.indstate.edu>

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

#include "e2_tree_dialog.h"
#include "e2_icons.h"

#ifdef E2_TREEDIALOG

//enable code to produce a full-tree in one step
//still BUGGY (circular links not detected) and SLOW !!!
//#define ENABLE_FULL_TREE

/* NOTE limitation when ENABLE_FULL_TREE not defined:
due to the scan-depth limit = 2, combinations like
   hidden-dir(s)-only/hidden-dir(s)-only/visible-dirs
will result in those visible-dirs not being displayed in the dialog unless the
show-hidden option has been activated by the user
*/

//STRICT_HIDE if defined will prevent all display of hidden dirs, i.e. no
//showing of hidden ancestors of non-hidden dirs
#ifdef ENABLE_FULL_TREE
#define STRICT_HIDE
#else
//CHOOSEME
#define STRICT_HIDE
#endif

//enable extra messages (for debugging)
//#define WARNBADS

#include "e2_dialog.h"
#include "e2_fs.h"
#include <string.h>

enum { DIRNAME, DIRKEY, TXTSET, TXTCOLOR, DIRFLAGS, DIRCOLCOUNT };	//treestore column enumerator
enum {
	E2_TRDLG_NONE         = 0,
	E2_TRDLG_STORED       = 1,	    //don't re-store this item
	E2_TRDLG_SCANNED      = 1 << 1,	//children of this item have been read and stored
	E2_TRDLG_GRANDSCANNED = 1 << 2,	//grand-children of this item have been read and stored
	E2_TRDLG_HIDDENITEMS  = 1 << 3	//UNUSED this dir has hidden children (whether or not actually shown)
};

#ifdef ENABLE_FULL_TREE
/*typedef struct _inodata
{
  ino_t inode;
  dev_t device;
  struct _inodata *next;
} inodata;
*/
#endif

typedef struct _E2_TreeDialogRuntime
{
	GtkWidget *dialog;		//the displayed dialog
	GtkWidget *hiddenbtn;	//action-area hide-button
	GtkTreeStore *store;	//dirnames store
	GtkWidget *treeview;	//dirnames treeview
	gboolean show_hidden;	//TRUE to show hidden dirs in the treeview
#ifndef STRICT_HIDE
	gboolean strict_hide;	//TRUE to prevent display of hidden ancestors
#endif
	GHashTable *dirhash;	//for contructing dependencies when store is filled
	ViewInfo *view;			//data for displayed file pane
	gchar *opendir;			//path of dir from which to shallow-scan
	pthread_t threadID;		//ID of cancellable thread doing a treewalk
//	inodata *itable[256];	//NULL-terminated circular array of data structs, for preventing symlink circularity
} E2_TreeDialogRuntime;

static gboolean show_hidden = FALSE;	//session-static value to use in dialogs
#ifndef STRICT_HIDE
static gboolean strict_hide = TRUE;
#endif
static GdkColor *bad_color;	//static assuming config data won't change during a liststore fill
static GdkColor *hidden_color;
//static assuming last-closed window sets size for next one in this session only
static gint window_width = -1;
static gint window_height = -1;

#ifdef ENABLE_FULL_TREE
#define INOHASH(x) ((x)&255)
#endif

#ifndef ENABLE_FULL_TREE
static void _e2_treedlg_row_expanded_cb (GtkTreeView *treeview, GtkTreeIter *iter,
	GtkTreePath *path, E2_TreeDialogRuntime *rt);
#endif
static void _e2_treedlg_response_cb (GtkDialog *dialog, gint response,
	E2_TreeDialogRuntime *rt);
gchar *_e2_treedlg_make_path (GtkTreeModel *model, GtkTreePath *path);

  /*********************/
 /***** utilities *****/
/*********************/

#ifdef ENABLE_FULL_TREE
//the following two functions are derived from the 'tree' utility by Steve Baker

/* *
@brief log inode number of followed symlink to avoid re-following it

@param inode inode no. to be logged if not already in @a itable matched with @a device
@param device devide no. to be logged if not already in @a itable matched with @a inode
@param itable array of inode data structs to be updated

@return TRUE if inode already logged or was successfully added, FALSE after error
*/
/*static gboolean _e2_treedlg_saveino (ino_t inode, dev_t device, inodata *itable[])
{
	inodata *it, *ip, *pp;
	gint hp = INOHASH (inode);

	for (pp = ip = itable[hp]; ip != NULL; ip = ip->next)	//CHECKME test ?
	{
		if (ip->inode > inode)	//CHECKME test ?
			break;
		if (ip->inode == inode && ip->device >= device)	//CHECKME test ?
			break;
		pp = ip;
	}

	if (ip != NULL && ip->inode == inode && ip->device == device)
		return TRUE;

	it = MALLOCATE (inodata);
	CHECKALLOCATEDWARN (it, return FALSE;)
	it->inode = inode;
	it->device = device;
	it->next = ip;	//CHECKME
	if (ip == itable[hp])
		itable[hp] = it;
	else
		pp->next = it;

	return TRUE;
} */
/* *
@brief determine whether @a inode & @a device pair has been logged before in @a itable

@param inode inode no. to be checked
@param device devide no. to be checked
@param itable array of inode data structs to be interrogated

@return
*/
/*static gboolean _e2_treedlg_findino (ino_t inode, dev_t device, inodata *itable[])
{
	inodata *it;

	for (it = itable[INOHASH(inode)]; it != NULL; it = it->next)
	{
		if (it->inode > inode)
			break;
		if (it->inode == inode && it->device >= device) //CHECKME test ?
			break;
	}

	return (it != NULL && it->inode == inode && it->device == device);
} */
#endif	//def ENABLE_FULL_TREE
/**
@brief directory-tree sort-order comparison function

This compares 'sort-key' row data, which is utf-8 compatible
NB utf8 collate keys for ascii text are case insensitive !!

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param data UNUSED data specified when function was setup

@return integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_treedlg_name_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer data)
{
	//comppare path depths, > depth sorts after
	GtkTreePath *pa, *pb;
	pa = gtk_tree_model_get_path (model, a);
	pb = gtk_tree_model_get_path (model, b);
	gint da, db;
	da = gtk_tree_path_get_depth (pa);
	db = gtk_tree_path_get_depth (pb);
	gtk_tree_path_free (pa);
	gtk_tree_path_free (pb);

	if (da != db)
		return (da < db) ? -1 : 1;

	//same depth ...
	gchar *keya;
	gchar *keyb;
	gtk_tree_model_get (model, a, DIRKEY, &keya, -1);
	gtk_tree_model_get (model, b, DIRKEY, &keyb, -1);

	gint result = strcmp (keya, keyb);

	g_free (keya);
	g_free (keyb);

	return result;
}
#ifndef STRICT_HIDE
/**
@brief clean all hidden "leaf" items from the treestore to which @a model applies
Before first call, @a iter needs to be initialized externally, to the
assumed-single root iter
@param model treemodel for the filesystem-directories treestore
@param iter pointer to iter for store row to start the scan
@param table pointer to hash table with iter row references

@return
*/
static void _e2_treedlg_clean_hidden (GtkTreeModel *model, GtkTreeIter *iter,
	GHashTable *table)
{
	GtkTreeIter iter2, backup;
	gchar *value;
	do
	{
nextiter:
		if (gtk_tree_model_iter_children (model, &iter2, iter))
		{
			backup = iter2;
			_e2_treedlg_clean_hidden (model, &iter2, table);
			//in any path like .hidden1/.hidden2, .hidden2 will have been deleted
			//but .hidden1 will be left, inapppropriately
			//FIXME make this better
			if (gtk_tree_model_iter_children (model, &iter2, &backup))
				_e2_treedlg_clean_hidden (model, &backup, table);
		}
		else
		{
			gtk_tree_model_get (model, iter, DIRNAME, &value, -1);
			if (value == NULL)
				continue;

			if (!ITEM_ISHIDDEN (value))
			{
				g_free (value);
				continue;
			}
			g_free (value);

			GtkTreePath *tp = gtk_tree_model_get_path (model, iter);
			gchar *pathstr = _e2_treedlg_make_path (model, tp);
			if (pathstr != NULL)
			{
				g_hash_table_remove (table, pathstr);
				g_free (pathstr);
			}
			gtk_tree_path_free (tp);

			if (gtk_tree_store_remove (GTK_TREE_STORE (model), iter))
				goto nextiter;	//no need to increment iter
			break;	//finished all iters at this level
		}
	} while (gtk_tree_model_iter_next (model, iter));
}
#endif	//ndef STRICT_HIDE
/**
@brief check whether some or all of @a path is represented in @a model
This finds the treepath, if any, for deepest existing segment of @a path.
Any empty segment of @a path is ignored.
@a iter does not need to be initialized externally
@param model treemodel for the filesystem-directories treestore
@param iter pointer to iter to store row which corresponds to @a path
@param pathstr utf-8 string, absolute path of the directory whose row is wanted

@return TRUE if any match found (and then, *iter is valid)
*/
static gboolean _e2_treedlg_match_pathstring (GtkTreeModel *model,
	GtkTreeIter *iter, const gchar *pathstr)
{
	printd (DEBUG, "_e2_treedlg_match_pathstring: path: %s", pathstr);

	if (!gtk_tree_model_get_iter_first (model, iter))
		return FALSE;

	GtkTreeIter iter2;
	GtkTreeIter *current, *parent, *temp;
	parent = iter;
	current = &iter2;
	gchar **split = g_strsplit (pathstr, G_DIR_SEPARATOR_S, -1);
	gchar *iterator;
	guint i = 0;
	while ((iterator = split[i]) != NULL)
	{
		if (*iterator == '\0')
		{
			if (i == 0)
			//special-case one-only empty string which 'precedes' the path start
#ifdef E2_VFSTMP
//FIXME other roots
#endif
				iterator = G_DIR_SEPARATOR_S;
			else
			{	//other empty segments before the first non-empty one are just skipped here
				i++;
				continue;
			}
		}

		if (e2_tree_find_iter_from_str_same (model, DIRNAME, iterator, parent))
		{
			//skip intervening empty-segments here, so we can decide whether to
			//check for children
			do
			{
				i++;
			} while (split[i] != NULL && *split[i] == '\0');

			if (split[i] != NULL)
			{ //something else to process
				if (gtk_tree_model_iter_children (model, current, parent))
				{
					temp = current;
					current = parent;
					parent = temp;
				}
				else
					break;
			}
			else
				break;
		}
		else
			break;
	}
	g_strfreev (split);

	if (//retval &&
		(parent != iter))
		*iter = *parent;	//ensure correct return data

	return TRUE;	//retval;
}
/**
@brief expand, and scroll to, the treepath of @a pathsstr, or as much of it as is found
Any trailing separator in @a pathstr is ignored.
Any problem in the expansion causes the displayed tree to be collapsed to
level 1, and unscrolled.
The treestore model and treeview must be connected.
@param pathstr utf-8 string, absolute path of the directory whose row is to be shown
@param expanded TRUE if the path to be shown is also to be expanded
@param rt pointer to dialog data struct

@return
*/
static void _e2_treedlg_show_path (const gchar *pathstr, gboolean expanded, E2_TreeDialogRuntime *rt)
{
	GtkTreeIter iter;
	GtkTreeView *tvw = GTK_TREE_VIEW (rt->treeview);
	GtkTreeModel *model = GTK_TREE_MODEL (rt->store);
	GtkTreePath *tp = NULL;
	GtkTreeRowReference *ref;
	if ((ref = g_hash_table_lookup (rt->dirhash, pathstr)) != NULL) //may fail e.g. due to trailing /
		tp = gtk_tree_row_reference_get_path (ref);
	if (tp == NULL &&
		_e2_treedlg_match_pathstring (model, &iter, pathstr)) //find the closest available ancestor-path
			tp = gtk_tree_model_get_path (model, &iter);
	if (tp != NULL)
	{
		if (expanded)
			gtk_tree_view_expand_to_path (tvw, tp);
		else
		{
			GtkTreePath *parent = gtk_tree_path_copy (tp);
			gtk_tree_path_up (parent);
			gtk_tree_view_expand_to_path (tvw, parent);
			gtk_tree_path_free (parent);
		}
		//maybe gtk bug here ?? any vertical scroll < about 0.5 fails
		//?WAIT_FOR_EVENTS
		gtk_tree_view_scroll_to_cell (tvw, tp, NULL, TRUE, 0.5, 0.1);

		GtkTreeSelection *sel = gtk_tree_view_get_selection (tvw);
		gtk_tree_selection_select_path (sel, tp);
	}
	else //(can be an empty treestore when first called ??)
	{
		tp = gtk_tree_path_new_first ();
		gtk_tree_view_collapse_all (tvw);
		gtk_tree_view_expand_row (tvw, tp, FALSE);
	}
	gtk_tree_path_free (tp);
}
/**
@brief construct string form of path represented by @a path in @a model

@param model treemodel for the filesystem-directories treestore
@param path the tree path of the directory whose path is to be constructed

@return newly-allocated utf-8 absolute path string (no trailer), or NULL after error
*/
gchar *_e2_treedlg_make_path (GtkTreeModel *model, GtkTreePath *path)
{
	gint i, depth;
	gint *indices;
	gchar *freeme, *segment, *madepath;
	GtkTreeIter *current, *parent, *temp;
	GtkTreeIter iter1, iter2;

	depth = gtk_tree_path_get_depth (path);
	if (depth == 1)
		return (g_strdup (G_DIR_SEPARATOR_S));

	//start at root of model
	if (!gtk_tree_model_get_iter_first (model, &iter1))
		return NULL;
	parent = &iter1;
	current = &iter2;	//starts undefined

	indices = gtk_tree_path_get_indices (path);
	indices++;	//we start interating after the root node
	madepath = g_strdup ("");
	for (i = 1; i < depth; i++)
	{
		if (gtk_tree_model_iter_nth_child (model, current, parent, *indices))
		{
			gtk_tree_model_get (model, current, DIRNAME, &segment, -1);
			freeme = madepath;
			madepath = g_strconcat (madepath, G_DIR_SEPARATOR_S, segment, NULL);
			g_free (freeme);
			g_free (segment);
			temp = parent;
			parent = current;
			current = temp;
		}
		else
		{
			printd (WARN, "bad index in directories treemodel");
			g_free (madepath);
			madepath = NULL;
			break;
		}
		indices++;
	}
	return madepath;
}
/**
@brief add a node for @a parentpath to @a model, and likewise for any missing ancestor node(s)
This is called when an iter for @a parentpath is missing from the directories
treestore, because @a store is being populated for the first time, or
@a parentpath is hidden and previously, hidden dirs were ignored.
Empty path segments are ignored.
@param store GtkTreeStore for the directories data
@param parentpath utf-8 string, absolute path of directory to be added, no trailer
@param dirhash hash table to store path references

@return GtkTreePath of the added node
*/
static GtkTreePath *_e2_treedlg_infill (GtkTreeStore *store,
	const gchar *parentpath, GHashTable *dirhash)
{
	gchar *name, *key;
	GtkTreeIter iter1, iter2;
	GtkTreeIter *current, *parent, *temp;
	parent = &iter1;
	current = &iter2;
	GtkTreePath *tp;
	GtkTreeModel *model = GTK_TREE_MODEL (store);

	gchar **split = g_strsplit (parentpath, G_DIR_SEPARATOR_S, -1);
	guint i, count = g_strv_length (split);

	if (gtk_tree_model_get_iter_first (model, parent))
	{
		//find node, if any, for deepest existing segment of the parent path
		for (i = 0; i < count; i++)
		{
			name = split[i];
			if (*name == '\0')
			{
				if (i == 0)
#ifdef E2_VFSTMP
	//FIXME other virtual root ok ?
#endif
					name = G_DIR_SEPARATOR_S;
				else
					continue; //ignore empty segments other than the first one
			}
			if (e2_tree_find_iter_from_str_same (model, DIRNAME, name, parent))
			{
				if (i < count-1)	//not doing last segment now
				{
					do
					{
						i++;
					} while (split[i] != NULL && *split[i] == '\0');
					if (i <= count-1)
					{
						if (gtk_tree_model_iter_children (model, current, parent))
						{
							//re-use the iters
							temp = current;
							current = parent;
							parent = temp;
							i--	;	//allow the loop counter to work as expected
						}
						else
							break;
					}
				}
			}
			else
				break;
		}
	}
	else
		i = 0;	//nothing in store, so start afresh

	//from deepest existing node, add missing children
	for (; i < count; i++)
	{
		if (i == 0)
		{	//not even a root node found
#ifdef E2_VFSTMP
//FIXME other virtual root ok ?
#endif
			name = G_DIR_SEPARATOR_S;
			gtk_tree_store_insert_before (store, current, NULL, NULL);
		}
		else
		{
			name = split[i];
			if (*name == '\0')
				continue;	//ignore empty strings other than tree root
			//append new segment to parent's children
			gtk_tree_store_insert_before (store, current, parent, NULL);
		}
#ifdef USE_GTK2_8
		key = g_utf8_collate_key_for_filename (name, -1);
#else
		key = g_utf8_collate_key (name, -1);
#endif
		//populate it
		gtk_tree_store_set (store, current,
			DIRNAME, name,
			DIRKEY, key,
			TXTSET, FALSE,	//since we've found some descendant, each missing dir must be readable
			TXTCOLOR, NULL,
#ifndef ENABLE_FULL_TREE
			DIRFLAGS, E2_TRDLG_STORED,
#endif
			-1);

		g_free (key);

		//remember this new addition
		tp = gtk_tree_model_get_path (model, current);
		gchar *pathnow = _e2_treedlg_make_path (model, tp);	//FIXME handle possible NULL after error
		GtkTreeRowReference *ref = gtk_tree_row_reference_new (model, tp);
		g_hash_table_insert (dirhash, pathnow, ref);
		if (i == 0 && count == 2 && *split[1] == '\0')
		{	//just adding a root entry
			g_strfreev (split);
			return tp;
		}
		gtk_tree_path_free (tp);
		//re-use the iters
		temp = current;
		current = parent;
		parent = temp;
	}
	g_strfreev (split);
	tp = gtk_tree_model_get_path (model, parent);

	return tp;
}
/**
@brief callback function for recursive directory scanning
This is called for each directory and non-hanging link to a directory in the
filesystem tree.
Any hidden dir will be shown despite the flag for showing hidden items, if
such dir has non-hidden descendant(s).
The treewalk has been setup for breadth-first, dirs only operation
@param dirpath localised string, absolute path of discovered directory, no trailer
@param statptr pointer to struct stat with info about @a dirpath
@param status one of the treewalker-function status codes
@param rt pointer to data struct for the dialog

@return E2TW_CONTINUE on success, others as appropriate
*/
static E2_TwResult _e2_treedlg_twcb_scan (VPATH *dirpath,
	const struct stat *statptr, E2_TwStatus status, E2_TreeDialogRuntime *rt)
{
	E2_TwResult retval;
	gboolean colored;
	GdkColor *foreground;
	E2_ERR_DECLARE

	retval = E2TW_CONTINUE;	//default to ok result

	switch (status)
	{
		case E2TW_DRR:
		case E2TW_D:  //dir
#ifndef ENABLE_FULL_TREE
		case E2TW_DL:	//directory, not opened due to tree-depth limit (=2)
#endif
			if (!e2_fs_access (dirpath, R_OK E2_ERR_PTR()))	//want X_OK too ?
			{
				//add normal item to treestore, if needed
				colored = FALSE;
				foreground = NULL;	//default_color;
				break;
			}
#ifdef E2_VFS
			else
			{
#ifdef E2_VFSTMP
				//FIXME handle error
#endif
				E2_ERR_CLEAR
			}
#endif
#ifdef ENABLE_FULL_TREE
		case E2TW_DL:	//directory, not opened due to tree-depth limit
#endif
		case E2TW_DNR:	//unreadable directory has been handled when it's 1st reported, so color is correct
		case E2TW_DM:	//directory, not opened due to different file system
		case E2TW_NS:	//un-statable item (for which, error is reported upstream)
						//Can usefully return only CONTINUE or STOP to the treewalker in this case
						//NOTE hanging dirlinks are identified as non-dirs, and not reported here
			//add error item, if needed
			colored = TRUE;
			foreground = bad_color;
			break;
		case E2TW_DP:	//directory, all subdirs have been visited
#ifndef ENABLE_FULL_TREE
		{
			GtkTreeRowReference *ref = g_hash_table_lookup (rt->dirhash, dirpath);
			if (ref != NULL)	//maybe dir hidden and not stored
			{
				GtkTreeIter iter;
				GtkTreePath	*tp = gtk_tree_row_reference_get_path (ref);
				if (tp != NULL)
				{
					gtk_tree_model_get_iter (GTK_TREE_MODEL (rt->store), &iter, tp);
					gtk_tree_path_free (tp);
					guint iterflags;
					gtk_tree_model_get (GTK_TREE_MODEL (rt->store), &iter, DIRFLAGS, &iterflags, -1);
					iterflags |= E2_TRDLG_SCANNED;	//CHECKME used ??
					gtk_tree_store_set (rt->store, &iter, DIRFLAGS, iterflags, -1);
				}
				else
				{
					printd (DEBUG, "oops, at 1 failed to get iter for %s", dirpath);
					g_hash_table_remove (rt->dirhash, dirpath);
				}
			}

			retval = E2TW_FIXME;	//just a flag to prevent adding to treestore
		}
			break;
#else
			break;
#endif
//		case E2TW_DNR:	//unreadable directory has been handled when it's 1st reported, so color is correct
//			retval = E2TW_FIXME;
//			break;
/*never happen in this context
		case E2TW_F:  //file
		case E2TW_SL:  //link to non-dir
		case E2TW_SLN:  //bad link to non-dir
			retval = ??;
			break;
*/
		default:
			retval = E2TW_STOP;	//should never happen
			break;
	}

//FIXME use filter-model and just re-filter for hidden-items [un]display ?

	if (retval == E2TW_CONTINUE)
	{
		//at dialog startup, the root path is not yet scanned FIXME more elegant way to handle this
		if (!strcmp (dirpath, G_DIR_SEPARATOR_S))
			return retval;

		gchar *lastseg = strrchr (VPSTR (dirpath), G_DIR_SEPARATOR);
		if (lastseg != NULL)	//should always succeed
		{
#ifdef STRICT_HIDE
			if (rt->show_hidden || !ITEM_ISHIDDEN ((lastseg + sizeof (gchar))))
#else
			//unless hiding is strict, always add dirs and cleanup later
			if (!rt->strict_hide || !ITEM_ISHIDDEN ((lastseg + sizeof (gchar))))
#endif
			{
				//don't add an item if it's already there
#ifndef ENABLE_FULL_TREE
				guint iterflags = E2_TRDLG_NONE;
#endif
				GtkTreeIter iter, parent;
				GtkTreePath	*tp;
#ifndef ENABLE_FULL_TREE
				GtkTreeRowReference *ref = g_hash_table_lookup (rt->dirhash, dirpath);
				if (ref != NULL)
				{
					tp = gtk_tree_row_reference_get_path (ref);
					if (tp != NULL)
					{
						gtk_tree_model_get_iter (GTK_TREE_MODEL (rt->store), &iter, tp);
						gtk_tree_path_free (tp);
						gtk_tree_model_get (GTK_TREE_MODEL (rt->store), &iter, DIRFLAGS, &iterflags, -1);
						if (iterflags & E2_TRDLG_STORED)
							return retval;
					}
					else
					{
						printd (DEBUG, "oops, at 2 failed to get iter for %s", dirpath);
						g_hash_table_remove (rt->dirhash, dirpath);
					}
				}
#endif
				const gchar *parentpath;
				if (lastseg == VPSTR (dirpath))
				{
#ifdef E2_VFSTMP
//FIXME alternate tree roots ?
#endif
					parentpath = G_DIR_SEPARATOR_S;	//we're handling a 1st-level dir
				}
				else
				{
					*lastseg = '\0';
					parentpath = VPCSTR (dirpath);
				}
				//confirm that parent path is already in treestore
				if ((ref = g_hash_table_lookup (rt->dirhash, parentpath)) != NULL)
					tp = gtk_tree_row_reference_get_path (ref);
				else
					//initial entry, or probably hidden and not sofar displayed
					//due to FALSE show-hidden flag
					//add a node corresponding to parentpath
					tp = _e2_treedlg_infill (rt->store, parentpath, rt->dirhash);

				*lastseg = G_DIR_SEPARATOR;	//reinstate possibly-clobbered separator

				GtkTreeModel *model = GTK_TREE_MODEL (rt->store);
				if (gtk_tree_model_get_iter (model, &parent, tp))
				{
					gchar *local, *utf, *key;
					//store data for the item
					local = lastseg + sizeof(gchar);
					utf = F_FILENAME_FROM_LOCALE (local);
#ifdef USE_GTK2_8
					key = g_utf8_collate_key_for_filename (utf, -1);
#else
					key = g_utf8_collate_key (utf, -1);
#endif
					//append new segment to parent's children
#ifdef USE_GTK2_10
					gtk_tree_store_insert_with_values (rt->store, &iter, &parent, -1,
#else
					gtk_tree_store_insert_before (rt->store, &iter, &parent, NULL);
					//populate it
					gtk_tree_store_set (rt->store, &iter,
#endif
						DIRNAME, utf,
						DIRKEY, key,
						TXTSET, colored,
						TXTCOLOR, foreground,
#ifndef ENABLE_FULL_TREE
						DIRFLAGS, E2_TRDLG_STORED,
#endif
						-1);

					F_FREE (utf, local);
					g_free (key);

					//remember this new addition
					tp = gtk_tree_model_get_path (model, &iter);
					ref = gtk_tree_row_reference_new (model, tp);
					g_hash_table_insert (rt->dirhash, g_strdup (VPSTR(dirpath)), ref);
					gtk_tree_path_free (tp);
				}
				else
					printd (DEBUG, "directories treestore - missing iterator error");
			}
#ifndef ENABLE_FULL_TREE
			else
			{
				GdkColor *current;
				//get parent iter
				GtkTreeIter parent;
				const gchar *parentpath;
				if (lastseg == VPSTR (dirpath))
				{
#ifdef E2_VFSTMP
//FIXME alternate tree roots ?
#endif
					parentpath = G_DIR_SEPARATOR_S;	//we're handling a 1st-level dir
				}
				else
				{
					*lastseg = '\0';
					parentpath = VPCSTR (dirpath);
				}
				//confirm that parent path is already in treestore
				GtkTreePath	*tp;
				GtkTreeRowReference *ref;
				if ((ref = g_hash_table_lookup (rt->dirhash, parentpath)) != NULL)
					tp = gtk_tree_row_reference_get_path (ref);
				else
					//initial entry, or probably hidden and not sofar displayed
					//due to FALSE show-hidden flag
					//add a node corresponding to parentpath
					tp = _e2_treedlg_infill (rt->store, parentpath, rt->dirhash);

				*lastseg = G_DIR_SEPARATOR;	//reinstate possibly-clobbered separator

				GtkTreeModel *model = GTK_TREE_MODEL (rt->store);
				if (gtk_tree_model_get_iter (model, &parent, tp))
				{
					gtk_tree_model_get (GTK_TREE_MODEL(rt->store), &parent,
						TXTCOLOR, &current, -1);
					if (current == NULL)	//no assigned color yet
						gtk_tree_store_set (rt->store, &parent,
						TXTSET, TRUE, TXTCOLOR, hidden_color, -1);
				}
				gtk_tree_path_free (tp);
				retval = E2TW_SKIPSUB;
			}
#endif
		}
//		printd (DEBUG, "Processed directory %s", dirpath);
	}
	else if (retval == E2TW_FIXME)
	{
//		printd (DEBUG, "Ignored callback for %s", dirpath);
		retval = E2TW_CONTINUE;
	}
	else
		printd (DEBUG, "Error code for %s is %d", dirpath, errno);
	return retval;  //must report (some/all?) errors, else it keeps trying & hangs
}

#ifdef ENABLE_FULL_TREE
/**
@brief thread-function to initiate a full treewalk
@param rt pointer to dialog data
@return pointerised TRUE/FALSE reflecting the result of the treewalk
*/
static gpointer _e2_treedlg_do_full_scan (E2_TreeDialogRuntime *rt)
{
	e2_utils_block_thread_signals ();	//block all allowed signals to this thread
	pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
#ifdef E2_VFS
	VPATH ddata = { G_DIR_SEPARATOR_S, NULL };
#endif
	gboolean success =
#ifdef E2_VFS
		e2_fs_tw (&ddata, _e2_treedlg_twcb_scan, rt, -1,
#else
		e2_fs_tw (G_DIR_SEPARATOR_S, _e2_treedlg_twcb_scan, rt, -1,
#endif
#ifdef WARNBADS
			E2TW_QT |
#else
			E2TW_XQT |
#endif
			E2TW_ONLYDIR E2_ERR_NONE());
	return GINT_TO_POINTER (success);
}
#endif
/**
@brief thread-function to initiate a shallow (2-level) treewalk
@param rt pointer to dialog data
@return pointerised TRUE/FALSE reflecting the result of the treewalk
*/
static gpointer _e2_treedlg_do_shallow_scan (E2_TreeDialogRuntime *rt)
{
	e2_utils_block_thread_signals ();	//block all allowed signals to this thread
	pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
#ifdef E2_VFS
	VPATH ddata = { rt->opendir, rt->view->spacedata };
#endif
	gboolean success =
#ifdef E2_VFS
		e2_fs_tw (&ddata, _e2_treedlg_twcb_scan, rt, 2,
#else
		e2_fs_tw (rt->opendir, _e2_treedlg_twcb_scan, rt, 2,
#endif
#ifdef WARNBADS
			E2TW_QT |
#else
			E2TW_XQT |
#endif
			E2TW_ONLYDIR E2_ERR_NONE());

	return GINT_TO_POINTER (success);
}

/**
@brief populate empty treestore for directories-tree dialog
Directories data are obtained and stored for all dirs in @a startpath
This expects BGL to be closed/on
@param startpath absolute utf-8 path-string, or NULL to use displayed dir in the pane
@param rt pointer to data struct for the dialog

@return
*/
static void	_e2_treedlg_fill_store (gchar *startpath, E2_TreeDialogRuntime *rt)
{
	//once-off setup of text color
	bad_color = e2_option_color_get ("color-negative");
	hidden_color = e2_option_color_get ("color-positive");
	//setup hash for speedy path relations
	rt->dirhash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
		(GDestroyNotify) gtk_tree_row_reference_free);
//#ifdef ENABLE_FULL_TREE
//FIXME should be done in _e2_treedlg_row_expanded_cb ()
	//disconnect view from store
	g_object_ref (G_OBJECT (rt->store));
	gtk_tree_view_set_model (GTK_TREE_VIEW (rt->treeview), NULL);
//#ifdef ENABLE_FULL_TREE
	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (rt->store);
	gtk_tree_sortable_set_sort_column_id (sortable,
		GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
//#endif

#ifdef ENABLE_FULL_TREE
# ifdef E2_VFSTMP
	//FIXME vfs, and other namespaces
# endif
	gpointer result;
	OPENBGL
	if (pthread_create (&rt->threadID, NULL, _e2_treedlg_do_full_scan, rt) == 0)
	{
		pthread_join (thisID, &result);
		rt->threadID = 0;
		if (result == NULL)
			printd (WARN, "Error in tree-scan process");
	}
	else
	{
		result = NULL;
		printd (WARN, "Failed to create tree-scan thread");
	}
	CLOSEBGL
	if (result == NULL)
	{
		//FIXME do what ?
	}
#else
# ifndef STRICT_HIDE
	//when doing this expansion, we don't want to cleanup each level individually
	gboolean wasnot_hidden = (rt->strict_hide) ? FALSE : !rt->show_hidden;
	if (wasnot_hidden)
		rt->show_hidden = TRUE;
# endif //ndef STRICT_HIDE
	//process each root-dir child and mark its iter 'scanned'
	GtkTreeIter iter;
	//a new treestore has no root iter, and that isn't covered in the startpath
	//loop, so ...
	GtkTreePath *tp = _e2_treedlg_infill (rt->store,
# ifdef E2_VFSTMP
//FIXME other roots too ?
# endif
		G_DIR_SEPARATOR_S, rt->dirhash);
	gtk_tree_model_get_iter (GTK_TREE_MODEL (rt->store), &iter, tp);
	NEEDOPENBGL
	_e2_treedlg_row_expanded_cb (GTK_TREE_VIEW (rt->treeview), &iter, tp, rt);
	NEEDCLOSEBGL

	gchar *showpath = (startpath == NULL) ? rt->view->dir : startpath;
	gchar **split = g_strsplit (showpath, G_DIR_SEPARATOR_S, -1);
	guint i, count = g_strv_length (split);
	if (count > 1)	//should always pass for a valid startpath string
	{
		gchar *madepath = g_strdup ("");
		for (i = 1; i < count; i++)
		{
			if (*split[i] != '\0')
			{
				gchar *freeme = madepath;
				madepath = g_strconcat (madepath, G_DIR_SEPARATOR_S, split[i], NULL);
				g_free (freeme);
				GtkTreeRowReference *ref = g_hash_table_lookup (rt->dirhash, madepath);
				tp = (ref == NULL) ?	//should pass this test only in first loop
					_e2_treedlg_infill (rt->store, madepath, rt->dirhash):
					gtk_tree_row_reference_get_path (ref);
				if (tp != NULL)
				{
					gtk_tree_model_get_iter (GTK_TREE_MODEL (rt->store), &iter, tp);
					NEEDOPENBGL
					_e2_treedlg_row_expanded_cb (GTK_TREE_VIEW (rt->treeview), &iter, tp, rt);
					NEEDCLOSEBGL
					gtk_tree_path_free (tp);
				}
				else
				{
					printd (DEBUG, "oops, at 3 failed to get iter for %s", madepath);
					if (ref != NULL)
						g_hash_table_remove (rt->dirhash, madepath);
				}
			}
		}
		g_free (madepath);
	}
	g_strfreev (split);
# ifndef STRICT_HIDE
	if (wasnot_hidden)
	{	//cleanup erroneous hidden leaves
		gtk_tree_model_get_iter_first (GTK_TREE_MODEL (rt->store), &iter);	//no error check needed
		_e2_treedlg_clean_hidden (GTK_TREE_MODEL (rt->store), &iter, rt->dirhash);
		rt->show_hidden = FALSE;
	}
# endif
#endif

//#ifdef ENABLE_FULL_TREE
	gtk_tree_sortable_set_sort_column_id (sortable, DIRNAME, GTK_SORT_ASCENDING);
//#endif
//#ifdef ENABLE_FULL_TREE
//FIXME should be done in _e2_treedlg_row_expanded_cb ()
	gtk_tree_view_set_model (GTK_TREE_VIEW (rt->treeview), GTK_TREE_MODEL (rt->store));
	g_object_unref (G_OBJECT (rt->store));
#ifdef ENABLE_FULL_TREE
	g_hash_table_destroy (rt->dirhash);
	rt->dirhash = NULL;
#endif
}

  /********************/
 /*** context menu ***/
/********************/

/**
@brief set popup-menu position

This function is supplied when calling gtk_menu_popup(), to position
the displayed menu, after a menu-key press.
set @a push_in to TRUE for menu completely inside the screen,
FALSE for menu clamped to screen size

@param menu UNUSED the GtkMenu to be positioned
@param x	place to store gint representing the menu left
@param y  place to store gint representing the menu top
@param push_in place to store pushin flag
@param rt pointer to data struct for the dialog to which the menu applies

@return
*/
static void _e2_treedlg_set_menu_position (GtkMenu *menu,
	gint *x, gint *y, gboolean *push_in, E2_TreeDialogRuntime *rt)
{
	gint left, top;
	gtk_window_get_position (GTK_WINDOW (rt->dialog), &left, &top);
	GtkAllocation alloc;
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (rt->treeview, &alloc);
#else
	alloc = rt->treeview->allocation;
#endif
	*x = left + alloc.x + alloc.width/2;
	*y = top + alloc.y +alloc.height/2 - 30;
	*push_in = FALSE;
}
/**
@brief tree path copy callback

@param widget UNUSED the menu item widget which activated the callback
@param treeview treeview to which the menu belongs

@return
*/
static void _e2_treedlg_copy_cb (GtkMenuItem *widget, GtkTreeView *treeview)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	NEEDCLOSEBGL
	gtk_tree_selection_get_selected (sel, &model, &iter);
	NEEDOPENBGL
	GtkTreePath *path = gtk_tree_model_get_path (model, &iter);
	gchar *str = _e2_treedlg_make_path (model, path);
	if (str != NULL)
	{
		GtkClipboard *clip = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  		gtk_clipboard_set_text (clip, str, strlen (str));
		g_free (str);
	}
	gtk_tree_path_free (path);
}
/**
@brief tree collapse-all callback

@param widget UNUSED the menu-item widget which was selected
@param treeview treeview to which the menu belongs

@return
*/
static void _e2_treedlg_collapse_all_cb (GtkMenuItem *widget, GtkTreeView *treeview)
{
	GtkTreePath *tp;
	NEEDCLOSEBGL
	gtk_tree_view_collapse_all (treeview);
	tp = gtk_tree_path_new_first ();
	gtk_tree_view_expand_row (treeview, tp, FALSE);
	NEEDOPENBGL
	gtk_tree_path_free (tp);
}
/**
@brief treeview refresh callback
This can also be called directly, in which case BGL must be closed
@param widget UNUSED the menu item widget which activated the callback
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_treedlg_refresh_cb (GtkMenuItem *widget, E2_TreeDialogRuntime *rt)
{
	//FIXME use filter-model and just re-filter ?
	GtkTreeModel *model;
	GtkTreeIter iter;
	NEEDCLOSEBGL
	GtkTreeSelection *sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->treeview));
	if (gtk_tree_selection_get_selected (sel, &model, &iter))
	{
		GtkTreePath *tp = gtk_tree_model_get_path (model, &iter);
		gchar *pathstr = _e2_treedlg_make_path (model, tp);	//pathstr = utf-8
		gboolean exp = gtk_tree_view_row_expanded (GTK_TREE_VIEW (rt->treeview), tp);
#ifndef ENABLE_FULL_TREE
		g_hash_table_destroy (rt->dirhash);
#endif
		//disconnect view from model to minimise visual disruption and speedup
		g_object_ref (G_OBJECT (rt->store));
		gtk_tree_view_set_model (GTK_TREE_VIEW (rt->treeview), NULL);
		gtk_tree_store_clear (rt->store);
		_e2_treedlg_fill_store (pathstr, rt);	//re-attaches model
		//expand to former position
		_e2_treedlg_show_path (pathstr, exp, rt);
		g_object_unref (G_OBJECT (rt->store));
		gtk_tree_path_free (tp);
		g_free (pathstr);
	}
	NEEDOPENBGL
}
#ifndef STRICT_HIDE
/**
@brief toggle display of hidden ancestors of visible dirs
This is performed when the context-menu item 'strict hiding' is selected
@param menuitem the activated menu widget
@param rt ptr to data struct for the dialog

@return
*/
static void _e2_treedlg_toggle_strict_cb (GtkMenuItem *menuitem, E2_TreeDialogRuntime *rt)
{
//	NEEDCLOSEBGL
	rt->strict_hide = gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM (menuitem));
//	NEEDOPENBGL
	_e2_treedlg_refresh_cb (NULL, rt);
}
#endif
/**
@brief construct and pop up destroyable context-menu for the dialog

@param treeview the widget where the click happened
@param event_button which mouse button was clicked (0 for a menu key)
@param event_time time that the event happened (0 for a menu key)
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_treedlg_show_context_menu (GtkWidget *treeview,
	guint event_button, gint event_time, E2_TreeDialogRuntime *rt)
{
	GtkTreeView *tvw = GTK_TREE_VIEW (treeview);
	GtkTreeSelection *sel = gtk_tree_view_get_selection (tvw);
	GtkWidget *menu = e2_menu_get ();

	GtkWidget *item =
	e2_menu_add (menu, _("_Copy"), STOCK_NAME_COPY, _("Copy selected path"),
		_e2_treedlg_copy_cb, tvw);
	if (!gtk_tree_selection_get_selected (sel, NULL, NULL))
		gtk_widget_set_sensitive (item, FALSE);
	e2_menu_add (menu, _("C_ollapse"), STOCK_NAME_ZOOM_OUT, _("Collapse all paths"),
		_e2_treedlg_collapse_all_cb, tvw);
	e2_menu_add (menu, _("_Refresh"), STOCK_NAME_REFRESH, NULL,
		_e2_treedlg_refresh_cb, rt);
#ifndef STRICT_HIDE
	item = e2_menu_add_check (menu, _("_Strict hiding"), rt->strict_hide,
		_e2_treedlg_toggle_strict_cb, rt);
	if (!rt->show_hidden)
		e2_widget_set_safetip (item,
		_("If active, hidden ancestors of visible directories will not be displayed"));
	gtk_widget_set_sensitive (item, !rt->show_hidden);
#endif
	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	if (event_button == 0)
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) _e2_treedlg_set_menu_position, rt,
			event_button, event_time);
	else
		//this was a button-3 click
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			NULL, NULL, event_button, event_time);
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief mouse button press callback

@param treeview the widget where the button was pressed
@param event gdk event data
@param rt pointer to data struct for the dialog

@return TRUE (stop other handlers) for btn 3 press, else FALSE
*/
static gboolean _e2_treedlg_button_press_cb (GtkWidget *treeview,
	GdkEventButton *event, E2_TreeDialogRuntime *rt)
{
	printd (DEBUG, "callback: _e2_treedlg mouse button press");
	if (event->button == 3
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		NEEDCLOSEBGL
		_e2_treedlg_show_context_menu (treeview, 3, event->time, rt);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief treeview key-press callback

@param widget UNUSED the focused treeview widget when the key was pressed
@param event pointer to event data struct
@param rt pointer to data struct for the dialog

@return TRUE (stop other handlers) for menu key has, else FALSE
*/
/*static gboolean _e2_treedlg_key_press_cb (GtkWidget *treeview,
	GdkEventKey *event, E2_TreeDialogRuntime *rt)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	printd (DEBUG, "callback: _e2_treedlg key press");
	return FALSE;
} */
/**
@brief menu-button press callback

@param treeview the widget where the press happened
@param rt dialog runtime data struct

@return TRUE always
*/
static gboolean _e2_treedlg_popup_menu_cb (GtkWidget *treeview,
	E2_TreeDialogRuntime *rt)
{
	guint32 event_time = gtk_get_current_event_time ();
	NEEDCLOSEBGL
	_e2_treedlg_show_context_menu (treeview, 0, event_time, rt);
	NEEDOPENBGL
	return TRUE;
}
/**
@brief treeview row-activated callback
Activation is triggered when <Enter> is pressed or when a double-click happens
This causes the activated item to be opened in the corresponding filelist
@param treeview the widget where the button was pressed
@param path model path to the clicked row
@param col UNUSED clicked view column
@param view data struct for the view to be worked on

@return
*/
static void _e2_treedlg_row_activated_cb (
		GtkTreeView        *treeview,
		GtkTreePath        *path,
		GtkTreeViewColumn  *col,
		ViewInfo           *view)
{
	printd (DEBUG, "callback: _e2_treedlg_row_activated");
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_tree_view_get_model (treeview);
    if (gtk_tree_model_get_iter (model, &iter, path))
	{
		//get the activated path
		gchar *newpath = _e2_treedlg_make_path (model, path);
		if (newpath != NULL)
		{
			//show that in associated filepane
			E2_PaneRuntime *rt = (E2_PaneRuntime *)view;
//			NEEDCLOSEBGL
			e2_pane_change_dir (rt, newpath);
//			NEEDOPENBGL
			g_free (newpath);
		}
	}
}
#ifndef ENABLE_FULL_TREE
/**
@brief callback for treeview "row-expanded" signal, also called from _e2_treedlg_fill_store()
If the expanded row is not 'ready', check each child dir there. Any child not
'scanned', will be scanned and marked as such.
Downstream code expects @a treeview to be connected to a treemodel
This expects BGL to be closed, and the dialog window must exist
@param treeview the object on which the signal was emitted
@param iter the tree iter of the expanded row
@param path tree path that points to the row
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_treedlg_row_expanded_cb (GtkTreeView *treeview, GtkTreeIter *iter,
	GtkTreePath *path, E2_TreeDialogRuntime *rt)
{
	guint iterflags;
	GtkTreeModel *model = GTK_TREE_MODEL (rt->store);	//maybe disconnected from treeview
	gtk_tree_model_get (model, iter, DIRFLAGS, &iterflags, -1);
	if (!(iterflags & E2_TRDLG_GRANDSCANNED))
	{	//expanded row is not 'ready' to be expanded
		NEEDCLOSEBGL
//#ifdef USE_GTK2_14
//		if (gtk_widget_get_window (rt->dialog) != NULL)
//#else
//		if (GDK_IS_WINDOW (rt->dialog->window)) if window created after view, at opening
//#endif
			e2_dialog_set_cursor (rt->dialog, GDK_WATCH);
		//almost certainly ok without reference, as any addtions are later in the store, but ...
		GtkTreeRowReference *ref = gtk_tree_row_reference_new (model, path);
#ifdef DEBUG_MESSAGES
		if (ref == NULL)
			printd (WARN, "NULL rowref for path");
#endif
		//check each child dir there
		gchar *thisdir = _e2_treedlg_make_path (model, path);	//should never be NULL
//FIXME disconnect view from store without collapsing view
//	g_object_ref (G_OBJECT (rt->store));	//?lots of refs here no clobber-risk ?
//	gtk_tree_view_set_model (GTK_TREE_VIEW (rt->treeview), NULL);

		GtkTreeSortable *sortable = GTK_TREE_SORTABLE (rt->store);
		gint sortcol;
		GtkSortType sortorder;
		gtk_tree_sortable_get_sort_column_id (sortable, &sortcol, &sortorder);
		gtk_tree_sortable_set_sort_column_id (sortable,
			GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
#ifndef STRICT_HIDE
		//hack: when doing this expansion, must process all hidden ancestor items
		//but we really only want hidden ancestors, not hidden "leaves", so delete
		//leaves in function-call below
		gboolean wasnot_hidden = (rt->strict_hide) ? FALSE : !rt->show_hidden;
		if (wasnot_hidden)
			rt->show_hidden = TRUE;
#endif
		rt->opendir = thisdir;
		gpointer result;
		OPENBGL
		if (pthread_create (&rt->threadID, NULL,
			(void*(*)(void*))_e2_treedlg_do_shallow_scan, rt) == 0)
		{
			pthread_join (rt->threadID, &result);
			rt->threadID = 0;
			if (result == NULL)
				printd (WARN, "Error in tree-scan process");
		}
		else
		{
			result = NULL;
			printd (WARN, "Failed to create tree-scan thread");
		}
		CLOSEBGL
		if (result == NULL)
		{
			//FIXME do what ?
		}

		if (G_LIKELY(ref != NULL))
		{
			GtkTreePath *tp = gtk_tree_row_reference_get_path (ref);
			if (G_LIKELY(tp != NULL))
			{
				gtk_tree_model_get_iter (model, iter, tp);
				gtk_tree_path_free (tp);
				//mark expanded iter as 'ready'
				gtk_tree_model_get (model, iter, DIRFLAGS, &iterflags, -1);
				iterflags |= E2_TRDLG_GRANDSCANNED;
				gtk_tree_store_set (rt->store, iter, DIRFLAGS, iterflags, -1);
#ifndef STRICT_HIDE
				if (wasnot_hidden)
				{	//cleanup erroneous hidden leaves
					//FIXME handle .hidden1/.hidden2/displayed, .hidden2 is removed but then .hidden1 is kept
					_e2_treedlg_clean_hidden (GTK_TREE_MODEL (rt->store), iter, rt->dirhash);
					rt->show_hidden = FALSE;
				}
#endif
			}
			else
			{
				printd (DEBUG, "oops, at 4 failed to get iter for %s", thisdir);
				g_hash_table_remove (rt->dirhash, thisdir);
			}
			gtk_tree_row_reference_free (ref);
		}
		else
		{
			printd (DEBUG, "oops, at 5 failed to get iter for %s", thisdir);
			g_hash_table_remove (rt->dirhash, thisdir);
		}
		g_free (thisdir);
		//when filling whole store this will leave the sort column unsorted ??
		gtk_tree_sortable_set_sort_column_id (sortable, sortcol, GTK_SORT_ASCENDING);

//	gtk_tree_view_set_model (GTK_TREE_VIEW (rt->treeview), model);
//	g_object_unref (G_OBJECT (rt->store)); //see above

//#ifdef USE_GTK2_14
//		if (gtk_widget_get_window (rt->dialog) != NULL)
//#else
//		if (GDK_IS_WINDOW (rt->dialog->window))
//#endif
			e2_dialog_set_cursor (rt->dialog, GDK_LEFT_PTR);

		NEEDOPENBGL
	}
}
#endif //ndef ENABLE_FULL_TREE
/**
@brief backup and cleanup during the destruction of the view related to a dialog
@param object UNUSED the object being destroyed
@param rt pointer to data struct for the dialog
@return
*/
static void _e2_treedlg_destroy_cb (
#ifdef USE_GTK3_0
	GtkWidget *object,
#else
	GtkObject *object,
#endif
	E2_TreeDialogRuntime *rt)
{
	g_signal_handlers_disconnect_by_func ((gpointer)rt->dialog,
		_e2_treedlg_response_cb, rt); //no double-handling
	NEEDOPENBGL
	_e2_treedlg_response_cb (GTK_DIALOG (rt->dialog), 0, rt);
	NEEDCLOSEBGL
}
/**
@brief handle button click, window-close etc for directory-tree dialog
This is the callback for response signals emitted from @a dialog
@param dialog UNUSED the dialog where the response was generated
@param response the response returned from the dialog
@param rt pointer to data struct for the dialog

@return
*/
static void _e2_treedlg_response_cb (GtkDialog *dialog, gint response,
	E2_TreeDialogRuntime *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_USER1: //toggle display of hidden items
			rt->show_hidden = !rt->show_hidden;
			NEEDOPENBGL
			_e2_treedlg_refresh_cb (NULL, rt);	//do the content before changing button
			NEEDCLOSEBGL
			e2_button_set_image (rt->hiddenbtn, (rt->show_hidden) ?
				"hidden_noshow"E2ICONTB : "hidden_show"E2ICONTB);
			break;
		default:
			gtk_widget_hide (rt->dialog);
			if (rt->threadID != 0)
			{
				pthread_cancel (rt->threadID);
				usleep (20000);
			}

#ifndef ENABLE_FULL_TREE
			if (rt->dirhash != NULL)
				g_hash_table_destroy (rt->dirhash);
#endif
			show_hidden = rt->show_hidden;	//backups for later used this session
#ifndef STRICT_HIDE
			strict_hide = rt->strict_hide;
#endif
#ifdef USE_GTK2_18
			GtkAllocation alloc;
			gtk_widget_get_allocation (rt->dialog, &alloc);
			window_width = alloc.width;
			window_height = alloc.height;
#else
			window_width = rt->dialog->allocation.width;
			window_height = rt->dialog->allocation.height;
#endif
			//FIXME use relevant container box
			g_signal_handlers_disconnect_by_func ((gpointer)rt->view->treeview,
				_e2_treedlg_destroy_cb, rt);
			gtk_widget_destroy (rt->dialog);

			DEALLOCATE (E2_TreeDialogRuntime, rt);
			break;
	}
	NEEDOPENBGL
}

/**
@brief create and show filesystem tree with focus on dir associated with @a view
This is a thread function
@param view data struct for file pane with which the treeview is to be associated

@return NULL
*/
static gpointer _e2_tree_dialog_run (ViewInfo *view)
{
	printd (DEBUG, "create tree dialog");
	E2_TreeDialogRuntime *rt = ALLOCATE (E2_TreeDialogRuntime);
	CHECKALLOCATEDWARN (rt, return NULL;);

	rt->show_hidden = show_hidden;	//before dialog is filled CHECKME use view->show_hidden ?
#ifndef STRICT_HIDE
	rt->strict_hide = strict_hide;
#endif
//	memset (rt->itable, 0, sizeof(rt->itable));

	gchar *title = (view == &app.pane1.view) ? _("pane 1 navigator") : _("pane 2 navigator") ;
	CLOSEBGL
	rt->dialog = e2_dialog_create (NULL, NULL, title,
		(ResponseFunc)_e2_treedlg_response_cb, rt);
	OPENBGL
	//scrolled window for the treeview
	GtkWidget *sw = e2_widget_add_sw (
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog)),
#else
		GTK_DIALOG (rt->dialog)->vbox,
#endif
		GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC, TRUE, E2_PADDING_SMALL);
	//dirnames treestore has single node, 5 columns (4 hidden)
	//see enumerator at start of this file
	//2nd is for sortable name key, 3rd & 4th for re-coloring "bad" entries
	//5th for flags
	rt->store = gtk_tree_store_new (DIRCOLCOUNT,
		G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN, GDK_TYPE_COLOR, G_TYPE_UINT);
	rt->view = view;
	//dirnames treeview
	rt->treeview = gtk_tree_view_new_with_model (GTK_TREE_MODEL (rt->store));
	g_object_unref (rt->store);
	gtk_container_add (GTK_CONTAINER (sw), rt->treeview);

	GtkCellRenderer *renderer = gtk_cell_renderer_text_new ();
	gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (rt->treeview),
		DIRNAME, NULL, renderer,
		"text", DIRNAME,
		"foreground-set", TXTSET,
		"foreground-gdk", TXTCOLOR,
		NULL);

	//by default, type-ahead searching is enabled on column 0
//	gtk_tree_view_set_search_equal_func	(rt->treeview,
//		(GtkTreeViewSearchEqualFunc)_e2_fileview_match_filename, view, NULL);
#ifdef USE_GTK2_10
	gtk_tree_view_set_enable_tree_lines (GTK_TREE_VIEW (rt->treeview), TRUE);
#endif
//	gtk_tree_view_set_rules_hint (GTK_TREE_VIEW (rt->treeview), TRUE);

	GtkTreeSelection *selection =
	gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->treeview));
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (rt->store);
	gtk_tree_sortable_set_sort_func (sortable,
		DIRNAME, //gint sort_column_id
		_e2_treedlg_name_sort, //GtkTreeIterCompareFunc sort_func,
		NULL, //gpointer user_data,
		NULL); //GtkDestroyNotify destroy

	//relate initial size to last-used, or if first, to filepanes size
#ifdef USE_GTK2_18
	if (window_width == -1 || window_height == -1)
	{
		GtkAllocation alloc;
		gtk_widget_get_allocation (app.window.panes_paned, &alloc);
#endif
		if (window_width == -1)
			window_width =
#ifdef USE_GTK2_18
			MIN(300, alloc.width/2);
#else
			MIN(300, app.window.panes_paned->allocation.width/2);
#endif
		if (window_height == -1)
			window_height =
#ifdef USE_GTK2_18
			alloc.height;
#else
			app.window.panes_paned->allocation.height;
#endif
#ifdef USE_GTK2_18
	}
#endif
	gtk_window_resize (GTK_WINDOW(rt->dialog), window_width, window_height);

	g_signal_connect (G_OBJECT (rt->treeview), "popup-menu",
		G_CALLBACK (_e2_treedlg_popup_menu_cb), rt);
#ifdef E2_TRANSIENTBINDINGS
	//FIXME setup dialog-specific stuff
#endif
//	g_signal_connect_after (G_OBJECT (rt->treeview), "key-press-event",
//		G_CALLBACK (_e2_treedlg_key_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->treeview), "button-press-event",
		G_CALLBACK (_e2_treedlg_button_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->treeview), "row-activated",
		G_CALLBACK (_e2_treedlg_row_activated_cb), view);
#ifndef ENABLE_FULL_TREE
	g_signal_connect (G_OBJECT (rt->treeview),  "row-expanded",
		G_CALLBACK (_e2_treedlg_row_expanded_cb), rt);
#endif
	//dialog depends on valid ViewInfo* data
	g_signal_connect (
#ifdef USE_GTK3_0
	G_OBJECT (view->treeview), //FIXME use relevant container box
#else
	GTK_OBJECT (view->treeview),
#endif
		"destroy", G_CALLBACK(_e2_treedlg_destroy_cb), rt);

	rt->hiddenbtn = e2_dialog_add_custom_button_full
		(rt->dialog, FALSE, E2_RESPONSE_USER1,
		_("_Hidden"),
		(rt->show_hidden) ? "hidden_noshow"E2ICONTB : "hidden_show"E2ICONTB,
		_("Toggle display of hidden directories"), NULL, NULL);
	E2_BUTTON_CLOSE.showflags |= E2_BTN_DEFAULT; //set default button

	CLOSEBGL
	e2_dialog_show (rt->dialog, app.main_window, 0,	&E2_BUTTON_CLOSE, NULL);

	//get the dir names
	_e2_treedlg_fill_store (NULL, rt);
	//show and select the startup row corresponding to displayed dir
	_e2_treedlg_show_path (rt->view->dir, TRUE, rt);
	OPENBGL

	return NULL;
}

  /****************/
 /**** public ****/
/****************/

/**
@brief show tree dialog action
This creates a thread to produce the dialog, because the directories scan
can be slow

@param from UNUSED the button, menu item etc which was activated
@param art UNUSED action runtime data

@return TRUE
*/
gboolean e2_tree_dialog_show_action (gpointer from, E2_ActionRuntime *art)
{
	E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
#ifdef USE_GLIB2_32
	g_thread_new ("", (GThreadFunc) _e2_tree_dialog_run, &rt->view);
#else
	g_thread_create ((GThreadFunc) _e2_tree_dialog_run, &rt->view, FALSE, NULL);
#endif
	return TRUE;
}

#endif //def E2_TREEDIALOG
