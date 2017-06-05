/* $Id: e2_fileview.c 3057 2014-02-14 23:23:08Z tpgww $

Copyright (C) 2004-2014 tooar <tooar@emelfm2.net>.

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 3, or (at your option) any
later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/e2_fileview.c
@brief directory content view functions

This file contains functions related to creation of treeviews to display
directory content, plus related selections and sorting.
*/
/**
\page treeviews filelist treeviews

ToDo - description of how to work with the filelist treeviews

\section cell_rend cell-renderers

ToDo
*/

#include "emelfm2.h"
//#include <unistd.h>
#include <string.h>
#include <time.h>
#include <pthread.h>
#include "e2_dnd.h"
#include "e2_filelist.h"
#include "e2_context_menu.h"
#include "e2_option.h"
//#include "e2_tree_dialog.h"
#include "e2_dialog.h"
#include "e2_task.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif

//allow in-place renaming of items TOO ANNOYING AND CLUMSY
//(initiated by left-click on filename when line is not selected)
//#define EDIT_INPLACE

//do incremental filelist refreshes
//#define E2_NEWREFRESH

//tweak some BGL-related stuff
//#define SYNC_DEBUG

extern GtkTargetEntry target_table[];
extern guint n_targets;
extern time_t last_work_time;

//this holds, in fixed column order (used in model & config, cache data),
// the displayed column widths, a row for each pane
gint col_width_store[2][MAX_COLUMNS];
//this holds, in fixed column order (used in model & config, cache data),
// the displayed column order, a row for each pane
gint stored_col_order[2][MAX_COLUMNS];
//this holds column data from the above arrays, for cache reading/writing
//GList *cols_data;
//this holds, in displayed column order
// the fixed column order, a row for each pane
gint displayed_col_order[2][MAX_COLUMNS];

// Colors
//GdkColor LIST_COLOR;
#ifdef E2_SELTXT_RECOLOR
GdkColor selectedtext;
#endif

gboolean btn2_released;	//flag used to decide if middle-btn DnD menu is wanted

#ifndef E2_MOUSECUSTOM
//local copy of flag
static gboolean button2updir;
#endif
//number of last mouse button press, for distinguishing drags
static gboolean pane1_all = FALSE, pane2_all=FALSE;
//these are related to column dragging
static gint header_button;
static gboolean block;
#ifdef E2_ALTLEFTMOUSE
/*this provides a quick check whether the left btn has been pressed
set and cleared in button-press and -release callbacks, respectively*/
gboolean left_pressed;
/*flag for whether selection-by-dragging is underway
set TRUE in drag-begin cb, cleared in drag-data-get cb*/
gboolean drag_sel_now;
//max msec between clicks when checking for a doubleclick
//extern guint click_interval;
#endif

static gpointer _e2_fileview_change_dir (E2_Listman *cddata);
static gboolean _e2_fileview_treehash_free (GHashTable *data);

//functions for sorting of treeviews
static gint _e2_fileview_name_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);
#ifdef E2_EXTCOL
static gint _e2_fileview_extn_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);
#endif
static gint _e2_fileview_size_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);
static gint _e2_fileview_perm_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);
static gint _e2_fileview_user_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);
static gint _e2_fileview_group_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);
static gint _e2_fileview_mdate_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);
static gint _e2_fileview_adate_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);
static gint _e2_fileview_cdate_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction);

E2_Column e2_all_columns[MAX_COLUMNS] = {
	{N_("Filename"), 120, _e2_fileview_name_sort},
#ifdef E2_EXTCOL
	{N_("Extn"), 50, _e2_fileview_extn_sort},
#endif
	{N_("Size"), 80, _e2_fileview_size_sort},
	{N_("Permissions"), 80, _e2_fileview_perm_sort},
	{N_("Owner"), 60, _e2_fileview_user_sort},
	{N_("Group"), 60, _e2_fileview_group_sort},
	{N_("Modified"), 120, _e2_fileview_mdate_sort},
	{N_("Accessed"), 120, _e2_fileview_adate_sort},
	{N_("Changed"), 120, _e2_fileview_cdate_sort} };

/**
@brief item-name sort-order comparison function

This compares 'sort-key' row data, which is UTF-8 compatible
If the case-insensitive sorting option was in force when the data was loaded into
the model, the keys are 'case-folded' i.e. this is not so dynamic as we might want
Directories (and links to dirs) are placed before other things.
NB utf8 collate keys for ascii text are case insensitive !!

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_fileview_name_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	gchar *keya, *keyb;
	FileInfo *infoa, *infob;
	gint result;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, NAMEKEY, &keya, -1);
	da = ISDIR (infoa->statbuf.st_mode);

	if (da && strcmp (infoa->filename, "..") == 0)
		result = (order) ? -1 : 1;
	else
	{
		gtk_tree_model_get (model, b, FINFO, &infob, NAMEKEY, &keyb, -1);
		db = ISDIR (infob->statbuf.st_mode);

		if (db && strcmp (infob->filename, "..") == 0)
			result = (order) ? 1 : -1;
		else if (da == db)	//both are dirs or non-dirs
			result = strcmp (keya, keyb);
		else
			result = (da == order) ? -1 : 1;
		g_free (keyb);
	}
	g_free (keya);

	return result;
}

#ifdef E2_EXTCOL
//this uses crude strcmp for matching, as file extensions are fairly standard and
// in english if to be easily recognised!
static gint _e2_fileview_extn_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	gchar *exta, *extb, *keya, *keyb;
	FileInfo *infoa, *infob;
	gint result;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, EXTN, &exta, NAMEKEY, &keya, -1);
	da = ISDIR (infoa->statbuf.st_mode);

	if (da && strcmp (infoa->filename, "..") == 0)
		result = (order) ? -1 : 1;
	else
	{
		gtk_tree_model_get (model, b, FINFO, &infob, EXTN, &extb, NAMEKEY, &keyb, -1);
		db = ISDIR (infob->statbuf.st_mode);

		if (db && strcmp (infob->filename, "..") == 0)
			result = (order) ? 1 : -1;
		else if (da == db)	//both are dirs or non-dirs
		{
			result = strcmp (exta, extb); //CRUDE!!
			if (result == 0)
				result = strcmp (keya, keyb);
		}
		else
			result = (da == order) ? -1 : 1;
		g_free (extb);
		g_free (keyb);
	}
	g_free (exta);
	g_free (keya);

	return result;
}
#endif

/**
@brief item-extension sort-order comparison function

This compares 'sort-key' row data, which is UTF-8 compatible
Directories are placed before other things.

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
//FIXME = support case-insensitive sorting
gint e2_fileview_ext_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	gchar *namea, *nameb, *keya, *keyb;
	FileInfo *infoa, *infob;
	gboolean da, db, order;
	gint result;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FILENAME, &namea, FINFO, &infoa, NAMEKEY, &keya, -1);
	da = ISDIR (infoa->statbuf.st_mode);
	if (da && strcmp (infoa->filename, "..") == 0)
		result = (order) ? -1 : 1;
	else
	{
		gtk_tree_model_get (model, b, FILENAME, &nameb, FINFO, &infob, NAMEKEY, &keyb, -1);
		db = ISDIR (infob->statbuf.st_mode);
		if (db && strcmp (infoa->filename, ".."))
			result = (order) ? 1 : -1;
		else if (da)
		{
			if (db)
				result = strcmp (keya, keyb);
			else
				result = (order) ? -1 : 1;
		}
		else if (db)
			result = (order) ? 1 : -1;
		else
		{
			gchar *ea, *eb;
			ea = namea;
			while (*ea == '.')	//always ascii '.', don't need g_utf8_strrchr()
				ea++;
			eb = nameb;
			while (*eb == '.')	//always ascii '.', don't need g_utf8_strrchr()
				eb++;

			if ((ea = strrchr (ea, '.')) == NULL)
			{	// row a has NO extension.. check row b
				if ((strrchr (eb, '.')) != NULL)
					result = -1;
				else
					result = strcmp (keya, keyb);
			}
			else // row a HAS an extension.. check row b
				if ((eb = strrchr (eb, '.')) == NULL)
				result = 1;
			else // both have extensions
			{
				result = g_utf8_collate (ea, eb);
				//if they are the same, sort by name
				if (result == 0)
					result = strcmp (keya, keyb);
			}
		}
		g_free (nameb);
		g_free (keyb);
	}
	g_free (namea);
	g_free (keya);
	return result;
}


/**
@brief item-size sort-order comparison function

Directories are placed before other things.

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_fileview_size_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	FileInfo *infoa, *infob;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, -1);
	da = ISDIR (infoa->statbuf.st_mode);
	if (da && strcmp (infoa->filename, "..") == 0)
		return (order) ? -1 : 1;

	gtk_tree_model_get (model, b, FINFO, &infob, -1);
	db = ISDIR (infob->statbuf.st_mode);
	if (db && strcmp (infob->filename, "..") == 0)
		return (order) ? 1 : -1;

	if (da == db)	//both are dirs or non-dirs
	{
		//long-handed approach avoids 64-bit overflows
		if (infoa->statbuf.st_size > infob->statbuf.st_size)
			return 1;
		else if (infoa->statbuf.st_size < infob->statbuf.st_size)
			return -1;
		else
			return strcmp (infoa->filename, infob->filename);
	}
	else
		return (da == order) ? -1 : 1;
}
/**
@brief item-permission sort-order comparison function

Directories are placed before other things.

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_fileview_perm_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	FileInfo *infoa, *infob;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, -1);
	da = ISDIR (infoa->statbuf.st_mode);
	if (da && strcmp (infoa->filename, "..") == 0)
		return (order) ? -1 : 1;

	gtk_tree_model_get (model, b, FINFO, &infob, -1);
	db = ISDIR (infob->statbuf.st_mode);
	if (db && strcmp (infob->filename, "..") == 0)
		return (order) ? 1 : -1;

	if (da == db)	//both are dirs or non-dirs
	{
		//long-handed approach avoids 64-bit overflows
		if (infoa->statbuf.st_mode > infob->statbuf.st_mode)
			return 1;
		else if (infoa->statbuf.st_mode < infob->statbuf.st_mode)
			return -1;
		else
			return strcmp (infoa->filename, infob->filename);
	}
	else
		return (da == order) ? -1 : 1;
}
/**
@brief item-owner sort-order comparison function

Directories are placed before other things.

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_fileview_user_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	FileInfo *infoa, *infob;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, -1);
	da = ISDIR (infoa->statbuf.st_mode);
	if (da && strcmp (infoa->filename, "..") == 0)
		return (order) ? -1 : 1;

	gtk_tree_model_get (model, b, FINFO, &infob, -1);
	db = ISDIR (infob->statbuf.st_mode);
	if (db && strcmp (infob->filename, "..") == 0)
		return (order) ? 1 : -1;

	if (da == db)	//both are dirs or non-dirs
	{
		//long-handed approach avoids 64-bit overflows
		if (infoa->statbuf.st_uid > infob->statbuf.st_uid)
			return 1;
		else if (infoa->statbuf.st_uid < infob->statbuf.st_uid)
			return -1;
		else
			return strcmp (infoa->filename, infob->filename);
	}
	else
		return (da == order) ? -1 : 1;
}
/**
@brief item-group sort-order comparison function

Directories are placed before other things.

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_fileview_group_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	FileInfo *infoa, *infob;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, -1);
	da = ISDIR (infoa->statbuf.st_mode);
	if (da && strcmp (infoa->filename, "..") == 0)
		return (order) ? -1 : 1;

	gtk_tree_model_get (model, b, FINFO, &infob, -1);
	db = ISDIR (infob->statbuf.st_mode);
	if (db && strcmp (infob->filename, "..") == 0)
		return (order) ? 1 : -1;

	if (da == db)	//both are dirs or non-dirs
	{
		//long-handed approach avoids 64-bit overflows
		if (infoa->statbuf.st_gid > infob->statbuf.st_gid)
			return 1;
		else if (infoa->statbuf.st_gid < infob->statbuf.st_gid)
			return -1;
		else
			return strcmp (infoa->filename, infob->filename);
	}
	else
		return (da == order) ? -1 : 1;
}
/**
@brief item-acess-date sort-order comparison function

Directories are placed before other things.

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_fileview_adate_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	FileInfo *infoa, *infob;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, -1);
	da = ISDIR (infoa->statbuf.st_mode);
	if (da && strcmp (infoa->filename, "..") == 0)
		return (order) ? -1 : 1;

	gtk_tree_model_get (model, b, FINFO, &infob, -1);
	db = ISDIR (infob->statbuf.st_mode);
	if (db && strcmp (infob->filename, "..") == 0)
		return (order) ? 1 : -1;

	if (da == db)	//both are dirs or non-dirs
	{
		//long-handed approach avoids 64-bit overflows
		if (infoa->statbuf.st_atime > infob->statbuf.st_atime)
			return 1;
		else if (infoa->statbuf.st_atime < infob->statbuf.st_atime)
			return -1;
		else
			return strcmp (infoa->filename, infob->filename);
	}
	else
		return (da == order) ? -1 : 1;
}
/**
@brief item-modification-date sort-order comparison function

Directories are placed before other things.

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_fileview_mdate_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	FileInfo *infoa, *infob;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, -1);
	da = ISDIR (infoa->statbuf.st_mode);
	if (da && strcmp (infoa->filename, "..") == 0)
		return (order) ? -1 : 1;

	gtk_tree_model_get (model, b, FINFO, &infob, -1);
	db = ISDIR (infob->statbuf.st_mode);
	if (db && strcmp (infob->filename, "..") == 0)
		return (order) ? 1 : -1;

	if (da == db)	//both are dirs or non-dirs
	{
		//long-handed approach avoids 64-bit overflows
		if (infoa->statbuf.st_mtime > infob->statbuf.st_mtime)
			return 1;
		else if (infoa->statbuf.st_mtime < infob->statbuf.st_mtime)
			return -1;
		else
			return strcmp (infoa->filename, infob->filename);
	}
	else
		return (da == order) ? -1 : 1;
}

/**
@brief item-status-change-date sort-order comparison function

Directories are placed before other things.

@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param direction pointer to the views's sort direction

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2_fileview_cdate_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, GtkSortType *direction)
{
	FileInfo *infoa, *infob;
	gboolean da, db, order;

	order = (*direction == GTK_SORT_ASCENDING);  //TRUE for ascending, FALSE for descending

	gtk_tree_model_get (model, a, FINFO, &infoa, -1);
	da = ISDIR (infoa->statbuf.st_mode);
	if (da && strcmp (infoa->filename, "..") == 0)
		return (order) ? -1 : 1;

	gtk_tree_model_get (model, b, FINFO, &infob, -1);
	db = ISDIR (infob->statbuf.st_mode);
	if (db && strcmp (infob->filename, "..") == 0)
		return (order) ? 1 : -1;

	if (da == db)	//both are dirs or non-dirs
	{
		//long-handed approach avoids 64-bit overflows
		if (infoa->statbuf.st_ctime > infob->statbuf.st_ctime)
			return 1;
		else if (infoa->statbuf.st_ctime < infob->statbuf.st_ctime)
			return -1;
		else
			return strcmp (infoa->filename, infob->filename);
	}
	else
		return (da == order) ? -1 : 1;
}
/**
@brief sort treeview for @a view by column @a colnum

This supports sorting by keypress. Expects BGL on/closed.

@param colnum column-enumerator, FILENAME...CHANGED, or fake column EXTENSION
@param view data structure for the pane

@return
*/
gboolean e2_fileview_sort_column (gint colnum, ViewInfo *view)
{
	if ((colnum < FILENAME || colnum > CHANGED) && colnum != EXTENSION)
		return FALSE;

	gint realcol = (colnum == EXTENSION) ? FILENAME : colnum;
	GList *cols = gtk_tree_view_get_columns (GTK_TREE_VIEW (view->treeview));
	GtkTreeViewColumn *col = g_list_nth_data (cols, realcol);
	g_list_free (cols);
	gboolean visible;
	g_object_get (G_OBJECT (col), "visible", &visible, NULL);
	if (!visible)
		return FALSE;

	gint old_sortcol;
	GtkSortType old_order;
	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (view->store);
	gtk_tree_sortable_get_sort_column_id (sortable, &old_sortcol, &old_order);

	if (colnum == FILENAME)
	{
		if (view->extsort)
		{	//currently extension-sorted, do normal name-sorting now
			//counter the effects of the normal toggle
			gtk_tree_sortable_set_sort_column_id (sortable,
				GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
			gtk_tree_sortable_set_sort_func (sortable, FILENAME,
				e2_all_columns[FILENAME].sort_func,
				&view->sort_order, NULL);
		}
	}
	else if (colnum == EXTENSION)
	{	//do extension-sorting now
		if (!view->extsort)
		{	//not already that way
			//counter the effects of the normal toggle
			gtk_tree_sortable_set_sort_column_id (sortable,
				GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
			gtk_tree_sortable_set_sort_func (sortable, FILENAME,
				(GtkTreeIterCompareFunc) e2_fileview_ext_sort,
				&view->sort_order, NULL);
		}
	}

	if (old_sortcol == realcol)	//column is already sorted
		view->sort_order = (old_order == GTK_SORT_ASCENDING) ?
			GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;
	else	//this is a different sort column
		//1st click makes it ascending
		view->sort_order = GTK_SORT_ASCENDING;

	//set appropriate arrow type
	GtkArrowType arrow;
	if (colnum == EXTENSION)
		//FIXME do a more-intuitive arrow
		arrow = (view->sort_order == GTK_SORT_ASCENDING) ?
		GTK_ARROW_RIGHT : GTK_ARROW_LEFT;
	else
		arrow = (view->sort_order == GTK_SORT_ASCENDING) ?
		GTK_ARROW_DOWN : GTK_ARROW_UP;

	gtk_arrow_set (GTK_ARROW (view->sort_arrows[realcol]), arrow,
			//different shadow types don't do any good, really ...
//			(view->extsort) ? GTK_SHADOW_ETCHED_IN : GTK_SHADOW_OUT);
			GTK_SHADOW_NONE);
	if (old_sortcol != realcol)
	{	//column is not already sorted
		//replace sort indicator
		gtk_widget_hide (view->sort_arrows[old_sortcol]);
		gtk_widget_show (view->sort_arrows[realcol]);
	}
	//remember the column, for cacheing
	view->sort_column = realcol;
	//do the sort
	gtk_tree_sortable_set_sort_column_id (sortable, realcol, view->sort_order);
	return TRUE;
}

  /*****************/
 /*** callbacks ***/
/*****************/

/**
@brief cursor change signal callback

This records the row to which the cursor is moved

@param treeview the widget where the cursor moved
@param view data structure for the pane

@return FALSE, so related signals are not blocked
*/
static gboolean _e2_fileview_cursor_change_cb (GtkTreeView *treeview, ViewInfo *view)
{
	printd (DEBUG, "callback: cursor changed");
	GtkTreePath *path;
	NEEDCLOSEBGL
	gtk_tree_view_get_cursor (treeview, &path, NULL);
	if (path != NULL) //path can be invalid when last row of a filelist is deleted
	{
		view->row = *gtk_tree_path_get_indices (path);
		gtk_tree_path_free (path);
	}
	NEEDOPENBGL
	printd (DEBUG, "focus row is %d", view->row);
	return FALSE;
}
/**
@brief column-header button-press signal callback

This func records which button was pressed, so that when a "release" callback
occurs (which may be when a drag starts, not an actual release) we can check
whether it is still pressed.
This is a workaround for gtk's (2.4 at least) behaviour
It relies on detecting events on the undocumented column->button widget

@param header the column button header widget where the press occurred
@param event pointer to event data struct
@param view data structure for the view to which the file list belongs

@return FALSE, so related signals are not blocked
*/
static gboolean _e2_fileview_column_header_buttonpress_cb (
	GtkWidget *header, GdkEventButton *event, ViewInfo *view)
{
	printd (DEBUG, "callback: column header button press");
	NEEDCLOSEBGL
	e2_utils_generic_press_cb (header, event, view);
	NEEDOPENBGL
	header_button = event->button;
	return FALSE;
}
/**
@brief column-header button-release signal callback

This checks whether a release event is real. In gtk 2.4 at least, such an event
may be when a column-drag starts, not an actual release.

@param UNUSED header the column button widget where the release occurred
@param event pointed to event data struct
@param view data structure for the view to which the file list belongs

@return TRUE, if this is a 'bogus' release, else FALSE
*/
static gboolean _e2_fileview_column_header_buttonrel_cb (
	GtkWidget *header, GdkEventButton *event, ViewInfo *view)
{
	printd (DEBUG, "callback: column header button release");

	NEEDCLOSEBGL
	if (!e2_utils_check_release (event))
	{
		printd (DEBUG, "IGNORED: missing button-press event");
		NEEDOPENBGL
		return FALSE;	//no corresponding press event
	}

	if (view != curr_view)
		e2_pane_activate_other ();

	NEEDOPENBGL

	//check for whether button is really still pressed
	//gtk 2.4 emits a bogus extra release signal when column-drag occurs
	gboolean pressed = FALSE;
	if (!block && event->send_event)
	{
		pressed = TRUE;
		block = TRUE;
	}
	else if (block)
		block = FALSE;  //on the second pass, just unblock
//	else
//		pressed = FALSE;

	if (!pressed)
		header_button = -1;  //signal to "clicked" callback not to do anything
	return pressed;	//stop further action on this button if not released yet
}
/**
@brief column-click callback

This func updates the view sorting, by applying an ascending sort to a column
that is not sorted, or by toggling the sort direction of a column that is
already sorted.
Sort indicators are updated.
This is called when a column header is clicked, before any associated
"column-change" signal.

@param col the view column which was clicked
@param rt data structure for the pane to which the file list belongs

@return
*/
//FIXME <Control>-click is not always passed through to here
static void _e2_fileview_column_header_clicked_cb (GtkTreeViewColumn *col,
	ViewInfo *view)
{
	printd (DEBUG, "callback: column header click");
	if (header_button != -1)
		return;		//the button is still down, this is drag action

	NEEDCLOSEBGL
	//get which column was cliicked
	GList *cols = gtk_tree_view_get_columns (GTK_TREE_VIEW (view->treeview));
	gint clicked_col = g_list_index (cols, (gpointer) col);
	g_list_free (cols);

	//get colum-order array for the pane
	gint *order_array = (view == &app.pane1.view) ? displayed_col_order[0] : displayed_col_order[1];
	//get clicked-model-column number
	gint i = order_array[clicked_col];
/*	if (i == 0)
	{ //filename column  DO WE WANT TO MAKE THIS DYNAMIC BUT SLOWER ??
		//get a local copy of the case-sensitive flag
		E2_OptionSet *set = e2_option_get_simple ("namesort-case-sensitive");
		case_sensitive = _e2_option_bool_get (set);
	} */
	//get current-sorted-column details
	gint m;
	GtkSortType old_order;
	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (view->store);
	gtk_tree_sortable_get_sort_column_id (sortable, &m, &old_order);

	gboolean toggle = TRUE;
	gboolean extsort = FALSE;
	if (i == FILENAME)
	{
#ifdef USE_GTK3_0
		if (e2_utils_get_savedstate (app.main_window) & GDK_CONTROL_MASK) //filelists only in main window
#else
		if (e2_utils_get_modifiers () & GDK_CONTROL_MASK)
#endif
		{	//do extension-sorting
			extsort = TRUE;
			if (!view->extsort)
			{
				//counter the effects of the normal toggle
				if (m == i)
					toggle = FALSE;
				view->extsort = TRUE;
				gtk_tree_sortable_set_sort_column_id (sortable,
					GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
				gtk_tree_sortable_set_sort_func (sortable, FILENAME,
					(GtkTreeIterCompareFunc) e2_fileview_ext_sort,
					&view->sort_order, NULL);
			}
		}
		else
		{	//do normal name-sorting
			if (view->extsort)
			{
				//counter the effects of the normal toggle
				if (m == i)
					toggle = FALSE;
				view->extsort = FALSE;
				gtk_tree_sortable_set_sort_column_id (sortable,
					GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
				gtk_tree_sortable_set_sort_func (sortable, FILENAME,
					e2_all_columns[FILENAME].sort_func,
					&view->sort_order, NULL);
			}
		}
	}

	if (m == i)
	{	//column is already sorted
		if (toggle)
			view->sort_order = (old_order == GTK_SORT_ASCENDING) ?
				GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;
	}
	else
	{  //this is a different sort column
		//1st click makes it ascending
		view->sort_order = GTK_SORT_ASCENDING;
	}
	//set appropriate arrow type
	GtkArrowType arrow;
	if (extsort)
		//FIXME do a more-intuitive arrow
		arrow = (view->sort_order == GTK_SORT_ASCENDING) ? GTK_ARROW_RIGHT : GTK_ARROW_LEFT;
	else
		arrow = (view->sort_order == GTK_SORT_ASCENDING) ? GTK_ARROW_DOWN : GTK_ARROW_UP;

	gtk_arrow_set (GTK_ARROW (view->sort_arrows[i]), arrow,
			//different shadow types don't do any good, really ...
			GTK_SHADOW_NONE);
	if (m != i)
	{	//column is not already sorted
		//replace sort indicator
		gtk_widget_hide (view->sort_arrows[m]);
		gtk_widget_show (view->sort_arrows[i]);
	}
	//remember the column, for cacheing
	view->sort_column = i;
	//do the sort
	gtk_tree_sortable_set_sort_column_id (sortable, i, view->sort_order);

	gtk_widget_grab_focus (view->treeview);
	NEEDOPENBGL
}
/**
@brief column-change callback

This func refreshes the columns-order array for the pane, by iterating over pane
columns glist, grabbing each title like "4", converting it to corresponding integer.
It is called when a column is dropped to a different position and when the
window is closed (also, multiple times when the window is re-created - CHECKME why)

@param treeview the widget where the action occurred
@param view data structure for the view

@return
*/
static void _e2_fileview_col_change_cb (GtkTreeView *treeview, ViewInfo *view)
{
	printd (DEBUG, "callback: column change");
	gint *order_array = (view == &app.pane1.view) ?
		displayed_col_order[0] : displayed_col_order[1];
	NEEDCLOSEBGL
	GList *cols = gtk_tree_view_get_columns (treeview);
	GList *tmp;
	const gchar *title;
	gint colnum = 0;
	for (tmp = cols; tmp != NULL; tmp = tmp->next)
	{
		title = gtk_tree_view_column_get_title (tmp->data);
		order_array[colnum++] = atoi (title);
	}
	g_list_free (cols);

	//update order of "slave" pane if appropriate
	gint *slave_array;
	ViewInfo *slave_view;
	if (e2_option_bool_get ("pane2-uses-other")
		&& view == &app.pane1.view)
	{
		slave_array = displayed_col_order[1];
		slave_view = &app.pane2.view;
	}
	else if (e2_option_bool_get ("pane1-uses-other")
		&& view == &app.pane2.view)
	{
		slave_array = displayed_col_order[0];
		slave_view = &app.pane1.view;
	}
	else
	{
		NEEDOPENBGL
		return;
	}

	printd (DEBUG, "slave-view column change");
	g_signal_handlers_block_by_func (G_OBJECT (slave_view->treeview),
		_e2_fileview_col_change_cb, slave_view);
	cols = gtk_tree_view_get_columns (GTK_TREE_VIEW (slave_view->treeview));
	for (colnum = 0; colnum < MAX_COLUMNS; colnum++)
	{
		gint current_col = order_array[colnum];
		if (slave_array[colnum] != current_col)
		{
			gint j;
			GtkTreeViewColumn *prior_col, *moved_col;
			if (colnum == 0)
				prior_col = NULL;
			else
				prior_col = g_list_nth_data (cols, colnum-1);
			for (j = colnum+1; j < MAX_COLUMNS; j++)
			{
				if (slave_array[j] == current_col)
				{
					moved_col = g_list_nth_data (cols, j);
					gtk_tree_view_move_column_after (
						GTK_TREE_VIEW (slave_view->treeview),
							moved_col, prior_col);
					//this approach seems too complex, but several simpler tries failed !!
					g_list_free (cols);
					cols = gtk_tree_view_get_columns (GTK_TREE_VIEW (slave_view->treeview));
					j = colnum;
					for (tmp = g_list_nth (cols, j); tmp != NULL; tmp=tmp->next)
					{
						const gchar *title = gtk_tree_view_column_get_title (tmp->data);
						slave_array[j] = atoi(title);
						j++;
					}
					break;
				}
			}
		}
	}
	NEEDOPENBGL
	g_list_free (cols);
	g_signal_handlers_unblock_by_func (G_OBJECT (slave_view->treeview),
		_e2_fileview_col_change_cb, slave_view);
}
/**
@brief row-activated callback
Activation is triggered when <Enter> is pressed or when a double-click happens
This "executes" the clicked row item
view->row will have been set by the button-release callback

@param treeview the widget where the button was pressed
@param path model path to the clicked row
@param col UNUSED clicked view column
@param view UNUSED rt data for the view to be worked on

@return
*/
static void _e2_fileview_row_activated_cb (
		GtkTreeView        *treeview,
		GtkTreePath        *tpath,
		GtkTreeViewColumn  *col,
		ViewInfo           *view)
{
	printd (DEBUG, "callback: _e2_fileview_row_activated");
	GtkTreeIter iter;
	NEEDCLOSEBGL
	GtkTreeModel *model = gtk_tree_view_get_model (treeview);
    if (gtk_tree_model_get_iter (model, &iter, tpath))
	{
		gchar *localpath;
		FileInfo *info;
		gtk_tree_model_get (model, &iter, FINFO, &info, -1);
		//this may append "..", which will be parsed in the cd process
		localpath = e2_utils_dircat (view, info->filename, TRUE);
#ifdef E2_VFS
		VPATH sdata = { localpath, view->spacedata };
		e2_task_backend_open (&sdata, TRUE);
#else
		e2_task_backend_open (localpath, TRUE);
#endif
		g_free (localpath);
	}
	NEEDOPENBGL
}
/**
@brief mouse button press callback

This makes the clicked view active, if it wasn't already.
A button-1 press when there is nothing else selected logs
the clicked model row, if any, in case it's a drag start.
(For anything other than a drag, the row is logged whem the
cursor moves.) The gtk handler will select the row.
Double-clicks are detected (based on cb sequence) here, to
work around gtk's behaviour when its handler is aborted
after a single click
A button-2 press does an updir, if that option is in force,
or else logs the clicked model row, if any.
A button-3 press sets up the relevant context menu, and may
select the line or unselect everything, depending on options in force

@param treeview the widget where the button was pressed
@param event gdk event data
@param view rt data for the view to be worked on

@return TRUE (stop other handlers) for btn 2 (updir) or btn 3 press, else FALSE (allow other handlers)
*/
static gboolean _e2_fileview_button_press_cb (GtkWidget *treeview,
	GdkEventButton *event, ViewInfo *view)
{
	printd (DEBUG, "callback: mouse button %d press event type %d", event->button, event->type);
	NEEDCLOSEBGL
	//check that we're in the actual treeview (not its header) by making
	//sure that the window is correct
	GdkWindow *window = gtk_tree_view_get_bin_window (GTK_TREE_VIEW (treeview));
	if (window != event->window)
	{
		NEEDOPENBGL
		return FALSE;
	}
//	NEEDOPENBGL
	e2_utils_generic_press_cb (treeview, event, view);	//log data for release-checks
//	NEEDCLOSEBGL
	//make the clicked pane active, if it wasn't already
	if (view != curr_view)  // && !panechange_started)
		e2_pane_activate_other ();

	GtkTreePath *path;
	GtkTreeSelection *sel;
#ifdef E2_ALTLEFTMOUSE
//	gboolean oldleft = left_pressed;
	//set flag to assist processing any drag
	left_pressed = (event->button == 1
//#ifdef E2_MOUSECUSTOM
//FIXME	&& (event->state & ?) == 0
//#endif
//		&& event->type == GDK_BUTTON_PRESS
	  );
	if (left_pressed
	//we don't handle any 'modified' button-press (so that range-selection works as normal)
	//or a click within the specified double-click interval from the prior click
		&& (event->state & E2_MODIFIER_MASK) == 0
		)
	{
		//event-time for detecting double-clicks
/* because we block returns for selected items, double-clicks have to be detected
	and processed here, too
	instead of click-interval, test now relies on the normal cb sequence for doubles
	(pr, rl, pr, pr, rl) i.e. a press without an intervening release is a double (or triple!)
		static guint32 last_event_time = 0;
		guint32 interval = event->time - last_event_time;
		last_event_time = event->time;
		if (interval >= click_interval)	//time-based check for double-click (the 3rd pr will pass this test too)
		{ */
			/*if we've passed all the tests, and a selected item is clicked,
			we block gtk's deselection of other items until the button is
			released. Then, we'll know how to treat it*/
		if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (treeview),
			event->x, event->y, &path, NULL, NULL, NULL))
		{
//				sel = view->selection;
				if (gtk_tree_selection_path_is_selected (view->selection, path))
				{
					//sometimes the focus doesn't get transferred properly by gtk, so ...
					gtk_widget_grab_focus (view->treeview);

//					if (oldleft)	//this is a fake press, generated as part of a double-click
					if (event->type == GDK_2BUTTON_PRESS)
					{
						printd (DEBUG, "intercepted mouse double-click");
						//process it as a double-click
						NEEDOPENBGL
						_e2_fileview_row_activated_cb (GTK_TREE_VIEW (treeview), path, NULL, view);
						NEEDCLOSEBGL
					}
					gtk_tree_path_free (path);
					//note that returning TRUE blocks some normal treeview behaviour
					//like refocussing a selected item and repeated double-clicks
					//and allowing in-place editing
/*annoying, interferes with double-clicks
#ifdef EDIT_INPLACE
					NEEDOPENBGL
					return (event->type != GDK_BUTTON_PRESS &&
						gtk_tree_selection_count_selected_rows (view->selection) == 1);
#else */
					NEEDOPENBGL
					return TRUE;	//wait until release, to see what to do with the selected row
//#endif
				}
				gtk_tree_path_free (path);
			}
//		}
	}
	else if (left_pressed && event->type != GDK_BUTTON_PRESS)
	{
		NEEDOPENBGL
		return TRUE;	//kill left multi-clicks with mod key(s)
	}
	else
#endif
/*		if (event->button == 1 &&
			(event->type == GDK_2BUTTON_PRESS || event->type == GDK_3BUTTON_PRESS))
			)
		{	//double or triple left-click
		}
	else
*/		if (event->button == 2)
	{
		NEEDOPENBGL
		return TRUE;	//wait until release, to see whether to select the row
	}
	else if (event->button == 3)
	{
		gboolean atitem;
		atitem = gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (treeview),
				event->x, event->y, &path, NULL, NULL, NULL);
		if (
#ifdef E2_MOUSECUSTOM
			(event->state & E2_MODIFIER_MASK) == 0 &&
#endif
			e2_option_bool_get ("windows-right-click")
//			&& (view->tagged == NULL)
		   )
		{
			//if windows right click is enabled, we want to also select the item,
			//if not already done, and clear any other selected item(s) unless this
			//one is selected already
			sel = view->selection;
			if (atitem)
			{
				if (!gtk_tree_selection_path_is_selected (sel, path))
					gtk_tree_selection_unselect_all (sel);
				gtk_tree_selection_select_path (sel, path);
				//do this now because menu created before cursor-move cb
				view->row = *gtk_tree_path_get_indices (path);
				gtk_tree_path_free (path);
			}
			else if (! e2_option_bool_get ("windows-right-click-extra"))
				gtk_tree_selection_unselect_all (sel);
		}
		else if (atitem)
		{
			//do this now because menu created before cursor-move cb
			view->row = *gtk_tree_path_get_indices (path);
			gtk_tree_path_free (path);
		}
		//context menu
		gint type;
		guint modifiers = gtk_accelerator_get_default_mod_mask ();
		modifiers &= event->state;
		switch (modifiers)
		{
			case 0:
				type = 0;
				break;
			case GDK_SHIFT_MASK:
				type = 1;
				break;
			case GDK_CONTROL_MASK:
				type = 2;
				break;
			//<Alt> is used for gestures
			case (GDK_SHIFT_MASK | GDK_CONTROL_MASK):
				type = 3;
				break;
			default:
				type = -1;
				break;
		}
		if (type >= 0)
		{
			e2_context_menu_show (event->button, event->time, type, view);
			NEEDOPENBGL
			return TRUE;
		}
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief mouse button release callback

This checks if the click was a left click. If so, and it's not the
end of a drag, or an 'un-select' click, the clicked row is logged.
Detection of the end of a drag relies on an undocumented
check whether the release happened in the same pane as
was clicked.

@param treeview UNUSED where the button was _pressed_
@param event
@param view rt data for the view where the button was _pressed_

@return TRUE (stop other handlers) if updir initiated, else FALSE (allow other handlers)
*/
static gboolean _e2_fileview_button_release_cb (GtkWidget *treeview,
	GdkEventButton *event, ViewInfo *view)
{
	NEEDCLOSEBGL
	//check that we're in the actual treeview (not its header)
	//(this test will also fail at the end of a drag to the other pane)
	GdkWindow *window = gtk_tree_view_get_bin_window (GTK_TREE_VIEW (treeview));
	if (window != event->window)
	{
//		printd (DEBUG, "callback: fileview button release after column-header-separator click or drag");
		NEEDOPENBGL
		return FALSE;
	}
//	else
//		printd (DEBUG, "callback: fileview button release");

	gint drag = e2_utils_check_drag (event);
	if (drag == 0)
	{
		printd (DEBUG, "IGNORED: missing button-press event");
		NEEDOPENBGL
		return FALSE;	//no corresponding press event
	}
	//for releases, there is no distinction between 1- 2- or 3- buttons
#ifdef E2_ALTLEFTMOUSE
	if (event->button == 1
//#ifdef E2_MOUSECUSTOM
//FIXME	&& (event->state & E2_MODIFIER_MASK) == 0
//#endif
		)
	{
		left_pressed = FALSE;
		//we ignore releases when a modifier is pressed,
		//and normal-drag ends,
		//  i.e. event->send_event seems to be FALSE (ie the event was not sent explicitly)
		//and drag-select ends
		if ((event->state & E2_MODIFIER_MASK) == 0
			&& drag == 1 //event->send_event == 0
			&& !drag_sel_now)
		{	/*there was no drag-selection, or the drag did not cover
			anything but the clicked item
			we want an outcome same as from a normal button-press
			i.e. select just the clicked row and log it, if any */
			GtkTreePath *path;
			if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (view->treeview),
				event->x, event->y, &path, NULL, NULL, NULL))
			{	//we're releasing over a treeview-item (which must have been selected)
				GtkTreeSelection *sel = view->selection;
				if (gtk_tree_selection_count_selected_rows (sel) > 1)
				{
					gtk_tree_selection_unselect_all (sel);
					gtk_tree_selection_select_path (sel, path);
				}
				gtk_tree_path_free (path);
			}
		}
	}
	else
#endif
		if (event->button == 2
#ifdef E2_MOUSECUSTOM
		 && (event->state & E2_MODIFIER_MASK) == 0
#endif
			)
	{
//		printd (DEBUG, "callback: mouse button-2 release");
		//correctly distinguish drag releases, and normal clicks
		//event->send_event seems to be FALSE (ie the event was not sent explicitly)
		//for releases in the same pane as clicked??
		if (drag == 1)
		{	//not a drag release
#ifndef E2_MOUSECUSTOM
			if (button2updir)
			{
				printd (DEBUG, "up");
				E2_PaneRuntime *rt = (view == curr_view) ? curr_pane : other_pane;
				e2_pane_change_dir (rt, "..");
				NEEDOPENBGL
				return TRUE;
			}
#endif
			if ((event->state & GDK_CONTROL_MASK) == 0)
			{	//not a "toggle" click
				GtkTreePath *path;
				if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (view->treeview),
					event->x, event->y, &path, NULL, NULL, NULL))
				{
					// row was NOT selected when the mouse was clicked
					gtk_tree_selection_select_path (view->selection, path);
					gtk_tree_path_free (path);
				}
			}
		}
		else
			//set signal for DnD handler
			btn2_released = TRUE;
	}
	else
		btn2_released = FALSE;

	NEEDOPENBGL
	return FALSE;
}
/**
@brief set popup menu position

This function is supplied when calling gtk_menu_popup(), to position
the displayed menu.
set @a push_in to TRUE for menu completely inside the screen,
FALSE for menu clamped to screen size

@param menu UNUSED the GtkMenu to be positioned
@param x place to store gint representing the menu left
@param y place to store gint representing the menu top
@param push_in place to store pushin flag
@param treeview in focus when the menu key was pressed

@return
*/
void e2_fileview_set_menu_position (GtkMenu *menu,
	gint *x, gint *y, gboolean *push_in, GtkWidget *treeview)
{
	gint left, top;
	gtk_window_get_position (GTK_WINDOW (app.main_window), &left, &top);
	GtkAllocation alloc;
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (treeview, &alloc);
#else
	alloc = treeview->allocation;
#endif
	*x = left + alloc.x + alloc.width/2;
	*y = top + alloc.y +alloc.height/2 - 30;
	*push_in = FALSE;
}
/**
@brief menu button callback (does not apply for 'modified' menu button)

Sets up the relevant context menu

@param treeview UNUSED the widget in focus when the action was initiated
@param view UNUSED rt data for the view to be worked on

@return TRUE always
*/
/*static gboolean _e2_fileview_popup_cb (GtkWidget *treeview, ViewInfo *view)
{
	gint menu_type = 0;	//default to 'normal' context menu
	//FIXME this check irrelevant, as cb never used for menu btn + Ctrl/Shift
/ *	GdkModifierType state;
	if (gtk_get_current_event_state (&state))
	{
		if ((state & GDK_SHIFT_MASK) && (state & GDK_CONTROL_MASK)) menu_type = 3;
		else if (state & GDK_SHIFT_MASK) menu_type = 1;
		else if (state & GDK_CONTROL_MASK) menu_type = 2;
	} * /
	guint32 event_time = gtk_get_current_event_time ();
	//button '0' signals this is not a mouse-button click
	NEEDCLOSEBGL
	e2_context_menu_show (0, event_time, menu_type, view);
	NEEDOPENBGL
	return TRUE;
} */
/**
@brief treeview key-press callback

@param widget UNUSED the focused treeview widget when the key was pressed
@param event pointer to event data struct
@param view rt data for the view to be worked on

@return TRUE when we handle a menu-key
*/
static gboolean _e2_fileview_key_press_cb (GtkWidget *widget,
	GdkEventKey *event, ViewInfo *view)
{
	printd (DEBUG, "_e2_fileview_key_press_cb, key %u", event->keyval);
	if (event->keyval == GDK_Menu)
	{
		gint menu_type;
		switch (event->state & gtk_accelerator_get_default_mod_mask ())
		{
			default:
				menu_type = 0;	//'normal' context menu
				break;
			case GDK_SHIFT_MASK:
				menu_type = 1;
				break;
			case GDK_CONTROL_MASK:
				menu_type = 2;
				break;
		//<Alt> is not a valid modifier for mouse right-click, so we also ignore
		//that for the menu-button
			case (GDK_SHIFT_MASK | GDK_CONTROL_MASK):
				menu_type = 3;
				break;
		}
		NEEDCLOSEBGL
		//button 0 signals this is not a mouse-button click
		e2_context_menu_show (0, event->time, menu_type, view);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
#ifdef EDIT_INPLACE
/**
@brief disable filelist refreshing during a filename edit

@param renderer the renderer for the cell
@param editable the interface to @a renderer
@param path_string string form of gtk tree path to the row to be amended
@param view pointer to view data struct

@return
*/
static void _e2_fileview_edit_start_cb (GtkCellRenderer *renderer,
	GtkCellEditable *editable, gchar *path_string, ViewInfo *view)
{
	printd (DEBUG, "start cell edit cb");
//	NEEDCLOSEBGL
	e2_filelist_disable_one_refresh ((view == curr_view) ? PANEACTIVE : PANEINACTIVE);
	//disable keypressed that will interfere
	g_signal_handlers_block_by_func (G_OBJECT (view->treeview),
		_e2_fileview_key_press_cb, view);
//	NEEDOPENBGL
}
/**
@brief enable filelist refreshing after an aborted filename edit

@param renderer the renderer for the cell
@param view pointer to view data struct

@return
*/
static void _e2_fileview_edit_cancel_cb (GtkCellRenderer *renderer,
	ViewInfo *view)
{
	printd (DEBUG, "cancel cell edit cb");
//	NEEDCLOSEBGL
	g_signal_handlers_unblock_by_func (G_OBJECT (view->treeview),
		_e2_fileview_key_press_cb, view);
	e2_filelist_enable_one_refresh ((view == curr_view) ? PANEACTIVE : PANEINACTIVE);
//	NEEDOPENBGL
}
/**
@brief save edited text value in the underlying treestore

@param renderer the renderer for the cell
@param path_string string form of gtk tree path to the row to be amended
@param new_text replacement text string for the cell
@param view  pointer to view data struct

@return
*/
static void _e2_fileview_name_edited_cb (GtkCellRendererText *cell,
	gchar *path_string, gchar *new_text, ViewInfo *view)
{
	NEEDCLOSEBGL
	if (new_text != NULL)	//probably always TRUE
	{
		printd (DEBUG, "filename edited cb, new text is %s", new_text);
		GtkTreeIter iter;
		if (gtk_tree_model_get_iter_from_string (view->model, &iter,
			path_string))
		{
			gchar *oldname;
			gtk_tree_model_get (view->model, &iter, FILENAME, &oldname, -1);
			if (!( !strcmp (oldname, new_text)
				|| !strcmp (oldname, G_DIR_SEPARATOR_S)	//some things can't be changed
				|| !strcmp (oldname, "../")))
			{
				gchar *old, *new, *utfold, *utfnew, *replacement;
				DialogButtons choice;
				gint len;
				gboolean isdir, success;

				len = strlen (oldname) - sizeof (gchar);
				isdir = (*(oldname + len) == G_DIR_SEPARATOR); //ascii check ok
				if (isdir && len > 0)
					*(oldname + len) = '\0';	//strip trailer

				utfold = e2_utils_dircat (view, oldname, FALSE);
				old = F_FILENAME_TO_LOCALE (utfold);
				utfnew = e2_utils_dircat (view, new_text, FALSE);
				new = F_FILENAME_TO_LOCALE (utfnew);
				len = strlen (new) - sizeof (gchar);

				if (*(new + len) == G_DIR_SEPARATOR && len > 0) //ascii check ok
					*(new + len) = '\0';	//strip trailer

#ifdef E2_VFS
				VPATH ddata = { new, view->spacedata };
#endif
				if (e2_option_bool_get ("confirm-overwrite")
					&& !e2_fs_access2 (new E2_ERR_NONE()))
				{
					OPENBGL
					choice = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
						&ddata,
#else
						new,
#endif
						NONE);
					CLOSEBGL
				}
				else
					choice = OK;

				if (choice == OK)
				{
#ifdef E2_VFS
					VPATH sdata = { old, view->spacedata };
#endif
					OPENBGL
					//CHECKME what if a task-Q is is progress ?
					success = e2_task_backend_rename
#ifdef E2_VFS
						(&sdata , &ddata);
#else
						(old, new);
#endif
					CLOSEBGL
					if (success)
					{
						if (isdir && !g_str_has_suffix (new_text, G_DIR_SEPARATOR_S))
							replacement = g_strconcat (new_text, G_DIR_SEPARATOR_S, NULL);
						else if (!isdir && g_str_has_suffix (new_text, G_DIR_SEPARATOR_S))
						{
							replacement = g_strdup (new_text);
							*(replacement + strlen (replacement) - sizeof (gchar)) = '\0';
						}
						else
							replacement = (gchar *) new_text;

						GtkTreeIter child;
						gtk_tree_model_filter_convert_iter_to_child_iter
							(GTK_TREE_MODEL_FILTER (view->model), &child, &iter);
						gtk_list_store_set (view->store, &child, FILENAME, replacement, -1);

						if (replacement != new_text)
							g_free (replacement);
					}
					//editing works only on unselected rows and anyway,
					//doesn't stay selected after the next refresh
//					gtk_tree_selection_select_iter (view->selection, &iter);
//					if (success)
//						e2_filelist_request_refresh (view->dir, TRUE); CHECKME ok without E2_FAM ?
				}
				g_free (utfold);
				F_FREE (old, utfold);
				g_free (utfnew);
				F_FREE (new, utfnew);
			}
			g_free (oldname);
		}
	}
	NEEDOPENBGL
	g_signal_handlers_unblock_by_func (G_OBJECT (view->treeview),
		_e2_fileview_key_press_cb, view);
	e2_filelist_enable_one_refresh ((view == curr_view) ? PANEACTIVE : PANEINACTIVE);
}
#endif //def EDIT_INPLACE
/* *
@brief timer callback function which checks whether ok to proceed with a cd
@param busy flag to check
@return FALSE when ready to proceed
*/
/*static gboolean _e2_fileview_check_completion (gboolean *busy)
{
	if (*busy)
		return TRUE;
	gtk_main_quit (); FIXME local loop
	app.timers[REFRESHWAIT_T] = 0;
	return FALSE;
}
*/
// some filter menu callbacks (others are in filter-dialog files)
/**
@brief "toggled" signal callback for filters menu item
Downstream functions require BGL closed
@param widget toggled menu-item
@param view pointer to data for view to be [un]filtered
@return
*/
void e2_fileview_filter_dirs_cb (GtkCheckMenuItem *widget, ViewInfo *view)
{  //toggle cached flag to match the menu check button
//	view->filter_directories = gtk_check_menu_item_get_active (widget);
	view->filter_directories = !view->filter_directories;
	NEEDCLOSEBGL
	e2_fileview_refilter_list (view);
	NEEDOPENBGL
}
/**
@brief "activate" signal callback for remove-filters menu item
Downstream functions require BGL closed
@param widget activated menu item
@param view pointer to data for view to be [un]filtered
@return
*/
void e2_fileview_remove_filters_cb (GtkMenuItem *widget, ViewInfo *view)
{
	// turn off local flags
	view->name_filter.active = FALSE;
	view->size_filter.active = FALSE;
	view->date_filter.active = FALSE;
	view->filter_directories = FALSE;
	NEEDCLOSEBGL
	//show the results
	e2_toolbar_toggle_filter_button (view);
	e2_fileview_refilter_list (view);
	NEEDOPENBGL
}
/**
@brief initialise file filter flags to show everything

@param view data structure for view being processed

@return
*/
/*void e2_fileview_initialize_filters (ViewInfo *view)
{
	view->filter_directories = FALSE;

	view->name_filter.active = FALSE;
	g_strlcpy (view->name_filter.pattern, "*",
		      sizeof(view->name_filter.pattern));
	view->name_filter.case_sensitive = TRUE;

	view->size_filter.active = FALSE;
	view->size_filter.size = 0;
	view->size_filter.op = GT;

	view->date_filter.active = FALSE;
	view->date_filter.time = time(NULL);
	view->date_filter.time_type = ATIME;
	view->date_filter.op = GT;
} */
/**
@brief clear filter patterns for view associated with @a view

@param view rt data for the view, assigned when function was initiated

@return
*/
void e2_fileview_clear_filter_patterns (ViewInfo *view)
{
	E2_SelectPattern *patterninfo;
	GSList *member;
	if (view->name_filter.compiled_patterns == NULL)
		return;
	for (member = view->name_filter.compiled_patterns; member != NULL; member = member->next)
	{
		patterninfo = (E2_SelectPattern *)member->data;
		g_pattern_spec_free (patterninfo->pspec);
		DEMALLOCATE (E2_SelectPattern, patterninfo);
	}
	g_slist_free (view->name_filter.compiled_patterns);
	view->name_filter.compiled_patterns = NULL;
}
/**
@brief compile filter patterns for view associated with @a view

@param view rt data for the view, assigned when function was initiated

@return TRUE if the process was completed successfully
*/
static gboolean _e2_fileview_compile_filter_patterns (ViewInfo *view)
{
	if (view->name_filter.compiled_patterns != NULL)
		e2_fileview_clear_filter_patterns (view);

	E2_SelectPattern *patterninfo;
	GSList *members = NULL;
	gchar *s, *p, *freeme;
	gchar save;

	p = view->name_filter.patternptr;
	//maybe several patterns, separated by commas
	while ((s = strchr (p, ',')) != NULL)	//if always ascii ',', don't need g_utf8_strchr()
	{	//check each pattern that is followed by a ','
		while (p[0] == ' ')	//no \t check
			p++;
		if (p == s)
		{	//empty pattern
			p++;
			continue;
		}
		patterninfo = MALLOCATE (E2_SelectPattern);	//too small for slice
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (patterninfo, g_slist_free (members);return FALSE;)
#else
		if (patterninfo == NULL)
		{
			g_slist_free (members); //improper cleanup, too bad
			return FALSE;
		}
#endif
		members = g_slist_append (members, patterninfo);
		if (p[0] == '!')
		{
			patterninfo->negated = !view->name_filter.invert_mask;
			p += sizeof (gchar);
		}
		else
		{
			patterninfo->negated = view->name_filter.invert_mask;
			if (p[0] == '\\' && p[1] == '!')
				p += sizeof (gchar);
		}

		save = *s;
		*s = '\0';

		if (!view->name_filter.case_sensitive)
		{
			freeme = g_utf8_strdown (p, -1);
			patterninfo->pspec = g_pattern_spec_new (freeme);
			g_free (freeme);
		}
		else
			patterninfo->pspec = g_pattern_spec_new (p);

		*s = save;
		s += sizeof (gchar); //pass the ','
		p = s;
	}
	//now the last (or only) pattern
	while (p[0] == ' ')
		p++;
	if (p[0] == '\0')
	{
		view->name_filter.compiled_patterns = members;
		return (members != NULL);
	}

	patterninfo = MALLOCATE (E2_SelectPattern); //too small for slice
#if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (patterninfo, g_slist_free (members);return FALSE;)
#else
	if (patterninfo == NULL)
	{
		g_slist_free (members); //improper cleanup, too bad
		return FALSE;
	}
#endif
	members = g_slist_append (members, patterninfo);

	if (p[0] == '!')
	{
		patterninfo->negated = !view->name_filter.invert_mask;
		p++;
	}
	else
	{
		patterninfo->negated = view->name_filter.invert_mask;
		if (p[0] == '\\' && p[1] == '!')
			p++;
	}
	if (!view->name_filter.case_sensitive)
	{
		freeme = g_utf8_strdown (p, -1);
		patterninfo->pspec = g_pattern_spec_new (freeme);
		g_free (freeme);
	}
	else
		patterninfo->pspec = g_pattern_spec_new (p);

	view->name_filter.compiled_patterns = members;
	return TRUE;
}
static gchar *name_pattern = NULL;
static gboolean hide_updir = FALSE;	//for speed, set in refilter-list func
/**
@brief treeview row visibility function

@param model the child model of the GtkTreeModelFilter
@param iter pointer to a GtkTreeIter for the row in @a model whose visibility is to be determined
@param view rt data for the view, assigned when function was initiated

@return TRUE if the row is visible
*/
static gboolean _e2_fileview_filter_check (GtkTreeModel *model,
	GtkTreeIter *iter, ViewInfo *view)
{
	FileInfo *info;
	gtk_tree_model_get (model, iter, FINFO, &info, -1);
	gchar *name = info->filename;
	if (ITEM_ISHIDDEN (name)
		&& (
			//name[1] == '\0' || filtered out when dir read
			   (!view->show_hidden && (name[1] != '.' || name[2] != '\0'))
			|| (name[1] == '.' && name[2] == '\0' &&
				(hide_updir || (view->dir[0] == G_DIR_SEPARATOR && view->dir[1] == '\0')) //never show updir in root directory
			   )
			)
		)
#ifdef E2_VFSTMP
	//FIXME show updir in root of archive etc v-dirs
#endif
			return FALSE;

	if (!view->filter_directories)
	{
		gchar *infopath = F_FILENAME_TO_LOCALE (view->dir);
#ifdef E2_VFS
		VPATH sdata = { infopath, view->spacedata };
		gboolean dir = e2_fs_is_dir (&sdata, info);
#else
		gboolean dir = e2_fs_is_dir (infopath, info);
#endif
		F_FREE (infopath, view->dir);
		if (dir)
			return TRUE;
	}

	if (view->name_filter.active)
	{
		//hidden updir's are already refused, above
		if (name[0] != '.' || name[1] != '.' || name[2] != '\0')
		{
			GSList *member;
			E2_SelectPattern *patterninfo;
			gboolean negated = FALSE, matched = FALSE, positive_check = FALSE, result = FALSE;
			gchar *freeme;
			gchar *utf = D_FILENAME_FROM_LOCALE (info->filename);	//not DISPLAYNAME, always dup, to avoid refresh race

			for (member = view->name_filter.compiled_patterns; member != NULL;
				member= member->next)
			{
				patterninfo = (E2_SelectPattern *)member->data;
				negated = patterninfo->negated;
				if (!positive_check)
					positive_check = !negated;

				if (!view->name_filter.case_sensitive)
				{
				  freeme = g_utf8_strdown (utf, -1);
				  matched = g_pattern_match_string (patterninfo->pspec, freeme);
				  g_free (freeme);
				}
				else
				  matched = g_pattern_match_string (patterninfo->pspec, utf);

				if (matched && negated)
				{
				  g_free (utf);
				  return FALSE;
				}
				if (matched && !negated)
				  result = TRUE;	//but keep looking for any later exclude
				//if neither negated nor matched, we don't change result
			}
			g_free (utf);
			//extra check for unmatched final check
			if (!matched && negated && !positive_check)
				result = TRUE;
			if (!result)
				return FALSE;
		}
	}

	if (view->size_filter.active)
	{
		switch (view->size_filter.op)
		{
			case LT:
			  if (info->statbuf.st_size >= view->size_filter.size)
				  return FALSE;
			  break;
			case EQ:
			  if (info->statbuf.st_size != view->size_filter.size)
				  return FALSE;
			  break;
			default:
//			case GT:
			  if (info->statbuf.st_size <= view->size_filter.size)
				  return FALSE;
			  break;
		}
	}

	if (view->date_filter.active)
	{
		switch (view->date_filter.time_type)
		{
			case ATIME:
			  if (view->date_filter.op == GT)
			  {
				if (difftime (view->date_filter.time, info->statbuf.st_atime) > 0)
					return FALSE;
			  }
			  else
				if (difftime (view->date_filter.time, info->statbuf.st_atime) <= 0)
					return FALSE;
			  break;
			case CTIME:
			  if (view->date_filter.op == GT)
			  {
				if (difftime (view->date_filter.time, info->statbuf.st_ctime) > 0)
					return FALSE;
			  }
			  else
				if (difftime (view->date_filter.time, info->statbuf.st_ctime) <= 0)
					return FALSE;
			  break;
			default:
//			case MTIME:
			  if (view->date_filter.op == GT)
			  {
				if (difftime (view->date_filter.time, info->statbuf.st_mtime) > 0)
					return FALSE;
			  }
			  else
				if (difftime (view->date_filter.time, info->statbuf.st_mtime) <= 0)
					return FALSE;
			  break;
		}
	}
	return TRUE;
}
/**
@brief treeview row visibility function for unfiltered view
Assumes "." entry is filtered out when dir data is read
@param model the child model of the GtkTreeModelFilter
@param iter pointer to a GtkTreeIter for the row in @a model whose visibility is to be determined
@param viewdir ptr to view->dir

@return TRUE unless the row is ".." in the root directory
*/
static gboolean _e2_fileview_filter_none (GtkTreeModel *model,
	GtkTreeIter *iter, gchar *viewdir)
{
#ifdef E2_VFSTMP
	//FIXME show updir from root of archive etc v-dirs
#endif
	//do fastest checks in order, to filter updir item from root dir
	if (viewdir[1] == '\0' && viewdir[0] == G_DIR_SEPARATOR)	//ascii ok
	{
		FileInfo *info;
		gtk_tree_model_get (model, iter, FINFO, &info, -1);
		if (info->filename[2] == '\0' && info->filename[1] == '.'
			&& ITEM_ISHIDDEN(info->filename))
			return FALSE;
	}
	return TRUE;
}
/**
@brief update filters for treeview associated with @a view
This changes model, and attaches view to new model.
Treeview selected items and scroll position are preserved as far as possible.
Assumes view attached to a model/store. Assumes BGL closed.
When new store is created, view->filtered_before will be false
@param view rt data for the view

@return
*/
void e2_fileview_refilter_list (ViewInfo *view)
{
	hide_updir = !e2_option_bool_get ("show-updir-entry");	//cache, for speedier filtering
	gboolean filter_wanted = (
		 view->name_filter.active ||
		 view->size_filter.active ||
		 view->date_filter.active ||
		!view->show_hidden ||
		 hide_updir );
	//enable preservation of view scroll position
	gint curr_left, curr_top;
	e2_fileview_get_scroll_data (view, &curr_left, &curr_top);
	guint selnum = gtk_tree_selection_count_selected_rows (view->selection);
	//before any refilter, log current selection
	GHashTable *selnames = (selnum > 0) ?
		e2_fileview_log_selected_names (view) : NULL;

	if (filter_wanted)
	{
		//setup for filter func
		if (view->name_filter.active)
		{
			_e2_fileview_compile_filter_patterns (view);
			if (!view->name_filter.case_sensitive)
				name_pattern = g_utf8_strdown (view->name_filter.patternptr, -1);
			else
				name_pattern = g_strdup (view->name_filter.patternptr);
		}

		if (!view->filtered_before)
		{
			//gtk won't allow a changed visibility function, so must
			//replace the filtermodel to make such change
			//this combination of changes has been confirmed to 0-ref old
			//filtermodel, and leave all other relevant refcounts = 1
			view->model = gtk_tree_model_filter_new (GTK_TREE_MODEL (view->store), NULL);
			gtk_tree_view_set_model (GTK_TREE_VIEW (view->treeview), view->model);
			g_object_unref (G_OBJECT (view->model));

			gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER (view->model),
				(GtkTreeModelFilterVisibleFunc) _e2_fileview_filter_check, view, NULL);
		}

		gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (view->model));

		if (view->name_filter.active)
			g_free (name_pattern);
	}
	else //if (view->filtered_before)
	{
		//de-filter or apply unfiltered visibility func
		e2_fileview_clear_filter_patterns (view);
		view->model = gtk_tree_model_filter_new (GTK_TREE_MODEL (view->store), NULL);
		gtk_tree_view_set_model (GTK_TREE_VIEW (view->treeview), view->model);
		g_object_unref (G_OBJECT (view->model));
#ifdef E2_VFSTMP
		//FIXME for v-dir, display ".." in root of archive etc, needs view ptr
#else
		gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER (view->model),
			(GtkTreeModelFilterVisibleFunc) _e2_fileview_filter_none, view->dir, NULL);
#endif
		gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (view->model));
	}

	guint filtered_rows = gtk_tree_model_iter_n_children (view->model, NULL);
	if (filtered_rows > 0)
	{
		if (view->row > filtered_rows - 1)
			view->row = filtered_rows - 1;	//set to last line of list
		//if there was no selection, or the former selection is gone (after filter or deletion)
		//we want relative cursor-moves to stay in the same vicinity
		if (selnum == 0)
		{
			GtkTreePath *tpath = gtk_tree_path_new_from_indices (view->row, -1);
			//this "marks" the item, but mere selection is not enough for this to work
			printd (DEBUG, "While filtering, no selection now, put cursor at [un]selected row %i", view->row);
			gtk_tree_view_set_cursor (GTK_TREE_VIEW (view->treeview), tpath, NULL, FALSE);
			if (!e2_option_bool_get ("select-first-item"))
				gtk_tree_selection_unselect_path (view->selection, tpath);
			gtk_tree_path_free (tpath);
		}
		else
		{
			printd (DEBUG, "While filtering, still %i selected rows, no change to cursor at row %i", selnum, view->row);
#ifdef USE_GTK3_0
			//workaround gtk3 selection behaviour
			GtkTreePath *tpath;

			gtk_tree_view_get_cursor ((GtkTreeView*)view->treeview, &tpath, NULL);
			if (tpath != NULL)
			{
				printd (DEBUG, "Unselect cursor row");
				gtk_tree_selection_unselect_path (view->selection, tpath);
				gtk_tree_path_free (tpath);
			}
			else
				printd (DEBUG, "Cursor row treepath not found");
#endif

#if 0
			if (!e2_option_bool_get ("select-first-item"))
			{
//				GtkTreePath *tpath = gtk_tree_path_new_from_indices (view->row, -1);
				//this "marks" the item, but mere selection is not enough for this to work
				printd (DEBUG, "While filtering, unselected row %i", view->row);
//				gtk_tree_selection_unselect_path (view->selection, tpath);
//				gtk_tree_path_free (tpath);
			}
#endif
			printd (DEBUG, "Goto reselect filelist names");
			e2_fileview_reselect_names (view, selnames, TRUE);	//try to reselect items, and at least clean the hash
			selnames = NULL;
		}
		e2_fileview_scroll_to_position (view, curr_left, curr_top);
	}
	else
		view->row = 0;

	if (selnames != NULL)
		//setup to get rid of hash when there's more time
		g_idle_add_full (G_PRIORITY_LOW,
			(GSourceFunc) _e2_fileview_treehash_free, selnames, NULL);

	view->filtered_before = filter_wanted;
	view->filtercount++;	//signal to the world that something has changed
}
/** @brief decide whether type-ahead search matches a row

@param model the treemodel being searched
@param column UNUSED the search column (= FILENAME)
@param key the aggregate string of keys sofar pressed
@param iter an iter pointing the row of @a model that should be compared with @a key
@param view pointer to data struct for the view

@return FALSE when a match is found
*/
static gboolean _e2_fileview_match_filename (GtkTreeModel *model, gint column,
	const gchar *key, GtkTreeIter *iter, ViewInfo *view)
{
	gchar *itemname;
	gtk_tree_model_get (model, iter, FILENAME, &itemname, -1);
	gboolean match = g_str_has_prefix (itemname, key);
	if (match)
	{	//we have a match
		GtkTreePath *path = gtk_tree_model_get_path (model, iter);
		view->row = *gtk_tree_path_get_indices (path);
		if (view->row > 0)
			view->row++;	//CHECKME why is this needed ..
		//these selection-flags probably ineffective
		e2_fileview_focus_row (view, view->row, FALSE, FALSE, FALSE, TRUE);
		gtk_tree_path_free (path);
	}
	g_free (itemname);
	return !match;
}
/* *
@brief UNUSED find filename begining with specified char, in current view

@param ch character at the start of the name

@return the row number where the matching name was found, or -1 if not found
*/
/*static gint e2_fileview_find_filename_begining_with (gchar ch)
{
	gint i;
	//number of rows in the model
	gint n = gtk_tree_model_iter_n_children (curr_view->model, NULL);
	GtkTreeIter iter;
	GtkTreeModel *model = curr_view->model;
	gchar *name;
	// search from after the current row to the end of the store
	for (i = curr_view->row+1; i < n; i++)
	{
		if (! gtk_tree_model_iter_nth_child
			(model, &iter, NULL, i))
				continue;  //the specified row doesn't exist any more
		gtk_tree_model_get (model, &iter, FILENAME, &name, -1);

		if (name[0] == ch)
		{
			g_free (name);
			return i;
		}
		else
			g_free (name);
	}
	// search from the first row to the current row
	for (i = 0; i < curr_view->row; i++)
	{
		if (! gtk_tree_model_iter_nth_child
			(model, &iter, NULL, i))
				continue;  //the specified row doesn't exist any more
		gtk_tree_model_get (model, &iter, FILENAME, &name, -1);

		if (name[0] == ch)
		{
			g_free (name);
			return i;
		}
		else
			g_free (name);
	}
	return -1;
}
*/

// Public functions

/**
@brief display and optionally select a numbered row
The treeview is scrolled, and the cursor is moved, whether or not the focused
row is selected. Of course, the focused row may be selected by gtk, independently
of @a select_row, and current selection will probably be cleared by gtk,
independently of @a clear_selection.
This assumes that BGL is closed
@param view rt data for ther view to be worked on
@param row treeview index of the row that is to be focused
@param select_row TRUE if the focused row needs to be selected
@param clear_selection TRUE if whole selection for the view is to be un-selected first
@param center TRUE to place the focused row near the center of the view window
@param grab_focus TRUE if the treeview is to be given the focus

@return
*/
void e2_fileview_focus_row (ViewInfo *view,
		      gint row,
		      gboolean select_row,
		      gboolean clear_selection,
		      gboolean center,
		      gboolean grab_focus)
{
	if (view != NULL)
	{
		GtkTreeIter iter;
		GtkTreeModel *model = view->model;
			//gtk_tree_view_get_model (GTK_TREE_VIEW (view->treeview));
		if (! gtk_tree_model_iter_nth_child (model, &iter, NULL, row))
			return;  //the specified row doesn't exist

		GtkTreeView *tvw = GTK_TREE_VIEW (view->treeview);
		GtkTreePath *path = gtk_tree_model_get_path (model, &iter);
		gtk_tree_view_scroll_to_cell (tvw, path, NULL, center, 0.382, 0.0);
		//remember existing selection, if any, because moving the cursor kills
		//the selection (if that hasn't been done by gtk already)
		GList *selitems = (clear_selection) ? NULL :
	      gtk_tree_selection_get_selected_rows (view->selection, NULL);

		gtk_tree_view_set_cursor (tvw, path, NULL, FALSE); //selects just this row

		if (selitems != NULL)
		{
			GList *member;
			GtkTreePath *tp;
			for (member = selitems; member != NULL; member = member->next)
			{
				tp = (GtkTreePath *) member->data;
				gtk_tree_selection_select_path (view->selection, tp);
				gtk_tree_path_free (tp);
			}
			g_list_free (selitems);
		}

		if (!select_row)
			gtk_tree_selection_unselect_path (view->selection, path);

		if (grab_focus)
			gtk_widget_grab_focus (view->treeview);

		gtk_tree_path_free (path);
	}
}
/* *
@brief UNUSED select 1st row, in specified view, where the filename matches a specified name

model content update is suspended while this is performed

@param view rt data for the view to be worked on
@param filename string with name of file to be used for selection

@return
*/
/*void e2_fileview_select_row_by_filename (ViewInfo *view, gchar *filename)
{
	gint i;
	//number of rows in the model
	gint n = gtk_tree_model_iter_n_children (view->model, NULL);
	GtkTreeIter iter;
	GtkTreeModel *model = view->model;
	gchar *thisname;
	e2_filelist_disable_one_refresh ((view==curr_view)?PANEACTIVE:PANEINACTIVE);
	// search from after the current row to the end of the store
	for (i = 0; i < n; i++)
	{
		if (! gtk_tree_model_iter_nth_child
			(model, &iter, NULL, i))
				continue;  //the specified row doesn't exist any more
		gtk_tree_model_get (model, &iter, FILENAME, &thisname, -1);

		if (!strcmp (thisname, filename))
		{
			e2_fileview_focus_row (view, i, TRUE, FALSE, TRUE, TRUE);
			g_free (thisname);
			break;
		}
		else
			g_free (thisname);
	}
	e2_filelist_enable_one_refresh ((view==curr_view)?PANEACTIVE:PANEINACTIVE);
} */
/**
@brief get file info for 1st selected row
This includes a special-case mechanism for getting "..", which may be opened
or activated when updir entries are shown in filelists
@param view data stuct for the view to be worked on
@param updir TRUE to include "../" in the candidates

@return FileInfo data for the 1st selected row, or if none, then NULL
*/
FileInfo *e2_fileview_get_selected_first_local (ViewInfo *view, gboolean updir)
{
	FileInfo *info = NULL;
//	GtkTreeSelection *sel = view->selection;
	GtkTreeModel *model;
	GList *rowpath;
	GList *selpaths = gtk_tree_selection_get_selected_rows (view->selection, &model);
	for (rowpath = selpaths; rowpath != NULL; rowpath = rowpath->next)
	{
		GtkTreeIter iter;
		GtkTreePath *path = (GtkTreePath *) rowpath->data;
		if (gtk_tree_model_get_iter (model, &iter, path))
		{
			gtk_tree_model_get (model, &iter, FINFO, &info, -1);
			if (updir || strcmp (info->filename, ".."))
				break;
			//ignore selection of "../"
			info = NULL;	//in case nothing else is selected
		}
	}
	g_list_foreach (selpaths, (GFunc) gtk_tree_path_free, NULL);
	g_list_free (selpaths);

	return info;
}
/**
@brief get list of selected rows' info items

This is usable only when the data is used before any chance
of filelist refresh, which would invalidate the listed data pointers

@param view rt data for the view to be worked on

@return list of selected rows' info items, excluding ".."
*/
GList *e2_fileview_get_selected_local (ViewInfo *view)
{
/*	NEEDOPENBGL
	g_signal_emit_by_name (G_OBJECT (view->treeview), "end-selection");
	NEEDCLOSEBGL
*/
//	if (view->tagged != NULL)
//		return g_list_copy (view->tagged);

	GList *selectedrow_data = NULL;
	FileInfo *info;
	GtkTreePath *path;
	GtkTreeIter iter;
//	GtkTreeSelection *sel = view->selection;
	GtkTreeModel *model;
	GList *rowpath;
	GList *selpaths = gtk_tree_selection_get_selected_rows (view->selection, &model);

	for (rowpath = selpaths; rowpath != NULL; rowpath = rowpath->next)
	{
		path = (GtkTreePath *) rowpath->data;
		if (gtk_tree_model_get_iter (model, &iter, path))
		{
			gtk_tree_model_get (model, &iter, FINFO, &info, -1);
			if (strcmp (info->filename, ".."))	//CHECKME ok, even with localised string?
				selectedrow_data = g_list_append (selectedrow_data, info);
		}
		gtk_tree_path_free (path);
	}
	g_list_free (selpaths);
	return selectedrow_data;
}
/**
@brief  toggle selection of all items in a pane

This function selects or unselects all** items in a pane, depending on
the current state of the corresponding flag, pane1_all or pane2_all
 ** except '..' entries, (if displayed)
The pane will be activated, if it wasn't already.
The function handles clicks of the select-all toggle buttons

@param blah unused widget ptr (in case this is a callback)
@param view fileview for pane that is to be (un)selected

@return
*/
void e2_fileview_select_all (GtkWidget *blah, ViewInfo *view)
{
	if (curr_view != view)
		e2_pane_activate_other ();
	GtkTreeSelection *sel = curr_view->selection;
	gboolean *allnow = (view == &app.pane1.view) ? &pane1_all : &pane2_all ;
	if (*allnow)
	{
		gtk_tree_selection_unselect_all (sel);
		*allnow = FALSE;
	}
	else //select
	{
		gtk_tree_selection_select_all (sel);
		*allnow = TRUE;
	}
}
/**
@brief cleanup content of selected file info struct

foreach function for e2_fileview_clean_selected()

@param seldata ptr to the struct to be processed
@param user_data user data passed to g_ptr_array_foreach() UNUSED

@return
*/
void e2_fileview_selected1_clean (E2_SelectedItemInfo *seldata,
	gpointer user_data)
{
/*	g_free (seldata->filename);
#ifdef E2_INCLIST
	gtk_tree_row_reference_free (seldata->ref);
#else
	gtk_tree_path_free (seldata->path);
#endif */
	DEALLOCATE (E2_SelectedItemInfo, seldata);
}
/**
@brief cleanup pointer array of selected file info structs


@param selected ptr to the array to be freed

@return
*/
void e2_fileview_clean_selected (GPtrArray *selected)
{
	printd (DEBUG, "In e2_fileview_clean_selected() selected: %x (length = %d)", selected, selected->len);
//#ifndef USE_GLIB2_22
	g_ptr_array_foreach (selected, (GFunc) e2_fileview_selected1_clean, NULL);
//#endif
	g_ptr_array_free (selected, TRUE);
}
/**
@brief create pointer array of selected file info structs

This fn creates a pointer array, with pointers to structs containing selected
items' names and other info.
If found, ".." is filtered out. If found, any trailing / is removed.
Item name strings are localised.
Data is duplicated, so the selection is refresh-proof.
Treerow refs support incremental store changes as the selection is processed.
The returned array needs to be freed with its data.

@param view is data structure for the view to be interrogated

@return pointer to GPtrArray structure, or NULL if nothing selected
*/
GPtrArray *e2_fileview_get_selected (ViewInfo *view)
{
/*	if (view == curr_view)
	{
		NEEDOPENBGL
		g_signal_emit_by_name (G_OBJECT(view->treeview), "end-selection");
		NEEDCLOSEBGL
	}
*/
	GtkTreeModel *model;
	GList *selpaths = gtk_tree_selection_get_selected_rows (view->selection, &model);
	if (selpaths != NULL)
	{
		GList *rowpath;
		GtkTreeIter iter;
		FileInfo *info;
		E2_SelectedItemInfo *seldata;
		GPtrArray *array = g_ptr_array_sized_new (g_list_length(selpaths));

		for (rowpath = selpaths; rowpath != NULL; rowpath = rowpath->next)
		{
			if (gtk_tree_model_get_iter (model, &iter, rowpath->data))
			{
				gtk_tree_model_get (model, &iter, FINFO, &info, -1);
				if (strcmp (info->filename, ".."))
				{
					seldata = ALLOCATE (E2_SelectedItemInfo);
					CHECKALLOCATEDWARN (seldata, continue);
					memcpy (&seldata->filename, &info->filename, sizeof (seldata->filename));
/*#ifdef E2_INCLIST
					//get a reference in case the model changes during the operation on the selection
					seldata->ref = gtk_tree_row_reference_new (model, paths->data);
#else
					//this is only relevant if store data not changed on the fly
					seldata->path = gtk_tree_path_copy (paths->data);
#endif */
					g_ptr_array_add (array, seldata);
				}
				gtk_tree_path_free ((GtkTreePath *)rowpath->data);
			}
		}
		g_list_free (selpaths);

		if (array->len > 0)	//there was more than just a ".." selected
			return array;

		g_ptr_array_free (array, TRUE);
	}
	return NULL;
}
/**
@brief cleanup one dir-history item, during app.dir_history destruction

@param data ptr to the struct to be processed

@return
*/
void e2_fileview_clean1_history (gpointer data)
{
	E2_DirHistoryEntry *hist = (E2_DirHistoryEntry *)data;
	if (hist->selitems != NULL)
		g_hash_table_destroy (hist->selitems);
	DEALLOCATE (E2_DirHistoryEntry, hist);
}
/**
@brief get itemname for a specified view and row


@param view is data structure for the view to be interrogated
@param row number of the row to be interrogated REAL or FILTERED ?

@return pointer to duplicated string, or NULL if there's nothing found
*/
gchar *e2_fileview_get_row_name (ViewInfo *view, gint row)
{
	GtkTreeIter iter;
//	GtkTreeModel *model = GTK_TREE_MODEL (view->store); maybe filter
	GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (view->treeview));

	if (! gtk_tree_model_iter_nth_child (model, &iter, NULL, row))
		return NULL;
	gchar *name;
	gtk_tree_model_get (model, &iter, FILENAME, &name, -1);
	return name;
}
/**
@brief helper to set file-list font
@param view pointer to data struct for the view to be changed
@param fontstr name of font to apply to the filelist associated with @a view

@return
*/
static void _e2_fileview_set_font (ViewInfo *view, gchar *fontstr)
{
	GList* columns, *renderers, *base1, *base2;
	columns = base1 = gtk_tree_view_get_columns (GTK_TREE_VIEW (view->treeview));
	for (; columns != NULL ; columns = columns->next)
	{
		GtkTreeViewColumn *column = columns->data;
#ifdef USE_GTK3_0
		renderers = base2 = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (column));
#else
		renderers = base2 = gtk_tree_view_column_get_cell_renderers (column);
#endif
		for (; renderers != NULL ; renderers = renderers->next)
			g_object_set (renderers->data, "font", fontstr, NULL);
		g_list_free (base2);
	}
	g_list_free (base1);
}
/**
@brief set file-lists font
This is needed to change font after a config dialog that does
not rebuild the whole window
@return
*/
void e2_fileview_set_font (void)
{
	gchar *fontstr = (e2_option_bool_get ("custom-list-font")) ?
		e2_option_str_get ("list-font") : NULL;	//NULL forces default
	_e2_fileview_set_font (curr_view, fontstr);
	_e2_fileview_set_font (other_view, fontstr);
}
/**
@brief set background color of treeview line


@param view data struct for the view to process
@param iter pointer to iter for the row to be changed
@param color pointer to struct with data for the new color

@return
*/
void e2_fileview_set_row_background (ViewInfo *view, GtkTreeIter *iter,
	GdkColor *color)
{
	//set the attribute column for this row
	gtk_list_store_set (GTK_LIST_STORE (view->store), iter, BACKCOLOR, color, -1);
	view->lit = TRUE;
}
/**
@brief revert background color of treeview line


@param view data struct for the view to process
@param iter pointer to iter for the row to be changed

@return
*/
void e2_fileview_clear_row_background (ViewInfo *view, GtkTreeIter *iter)
{
	//restore the background-attribute column for this row
#ifdef E2_ASSISTED
	GdkColor *bc;
	if (e2_option_bool_get ("color-background-set"))
		bc = e2_option_color_get ("color-background");
	else
		bc = NULL;
	gtk_list_store_set (GTK_LIST_STORE (view->store), iter, BACKCOLOR, bc, -1);
#else
	gtk_list_store_set (GTK_LIST_STORE (view->store), iter, BACKCOLOR, NULL, -1);
#endif
	view->lit = FALSE;
}
/**
@brief translate array into another form sorted in index order

This is needed to convert the 'fixed' column order used in data models
and config/cache data, to variable order in accord with the displayed view columns
and vice versa

@param src_array pointer to array of ints that is to be translated
@param dest_array pointer to array of ints that stores the translated order
@param size the number of elements to be translated

@return
*/
void e2_fileview_translate_cols_array (gint *src_array, gint *dest_array,
	gint size)
{
	gint i;
	for (i = 0; i < size; i++)
		dest_array[src_array[i]] = i;
}
/**
@brief update columns data, that is not automatically updated, for caching

Columns order and widths are udated and stored in a list
List format is: pane1 {order, width, ...}, pane2 {order, width, ...}

@return
*/
void e2_fileview_update_col_cachedata (void)
{
	//get current column widths
	GtkTreeViewColumn *col, *last;
#ifdef USE_GLIB2_30
	const gchar *title;
#else
	G_CONST_RETURN gchar *title;
#endif
	gint i, lc;
	GList *columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (app.pane1.view.treeview));
	GList *base = columns;
	lc = 0;
	last = (GtkTreeViewColumn *)columns->data;
	for (; columns != NULL; columns=columns->next)
	{
		col = columns->data;
		title = gtk_tree_view_column_get_title (col);
		//title is "0" ... "7", for cols 0 ... 7
		i = atoi (title);
		col_width_store [0][i] = gtk_tree_view_column_get_width (col);
		//remember this one if it's a candidate for last-displayed column
		if (col_width_store [0][i] > 0)
		{
			lc = i;
			last = col;
		}
	}
	//for last-displayed column, cache the user-specified width, not the
	//expanded-to-fill width
	col_width_store [0][lc] = gtk_tree_view_column_get_fixed_width (last);
	g_list_free (base);

	columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (app.pane2.view.treeview));
	base = columns;
	lc = 0;
	last = (GtkTreeViewColumn *)columns->data;
	for (; columns != NULL; columns=columns->next)
	{
		col = columns->data;
		title = gtk_tree_view_column_get_title (col);
		i = atoi (title);
		col_width_store [1][i] = gtk_tree_view_column_get_width (col);
		//remember this one if it's a candidate for last-displayed column
		if (col_width_store [1][i] > 0)
		{
			lc = i;
			last = col;
		}
	}
	col_width_store [1][lc] = gtk_tree_view_column_get_fixed_width (last);
	g_list_free (base);
	//get columns displayed order
	e2_fileview_translate_cols_array (displayed_col_order[0], stored_col_order[0], MAX_COLUMNS);
	e2_fileview_translate_cols_array (displayed_col_order[1], stored_col_order[1], MAX_COLUMNS);

/*SUPERSEDED BY ARRAY CACHEING
	//setup for list-cache writing
	//start fresh list
	e2_list_free_with_data (&cols_data);
	gchar *buffer;
	gint j;
	for (j = 0; j < 2; j++)
	{ for (i = 0; i < MAX_COLUMNS; i++) {
		//data need to be strings, for the cache writer
		buffer = g_strdup_printf ("%d", stored_col_order[j][i]);
		cols_data = g_list_append (cols_data, buffer);
		buffer = g_strdup_printf ("%d", col_width_store[j][i]);
		cols_data = g_list_append (cols_data, buffer);
	}}
*/
}
/* *
@brief UNUSED find which view column shows filenames

Columns may be in any order
Scans column titles, looking for "0"

@param view data structure for the view to which the file list belongs

@return integer column number, 0 to MAX_COLUMNS-1
*/
/* gint e2_fileview_find_name_col (ViewInfo *view)
{
	GList *cols = gtk_tree_view_get_columns (GTK_TREE_VIEW (view->treeview));
	GList *base = cols;
#ifdef USE_GLIB2_30
	const gchar *title;
#else
	G_CONST_RETURN gchar *title;
#endif
	gint result = 0;
	for ( ; cols != NULL; cols = cols->next )
	{
		title = gtk_tree_view_column_get_title (cols->data);
		if (!strcmp (title, "0"))  break;
		result++;
	}
	g_list_free (base);
//	return (result < MAX_COLUMNS) ? result : -1;
	return result;
} */
/**
@brief exchange active and inactive panes

This function is called from e2_pane_activate_other (), i.e.
whenever we need to exchange curr_view and other_view.
It toggles pointers and changes colours
The latter is done by interrogating the selections in both file views,
and so this fn should be blocked if either view is still being
populated at the time this is called ?? CHECKME

@return
*/
void e2_fileview_switch_views (void)
{
	ViewInfo *temp;

#ifdef E2_VFSTMP
	//FIXME what dir to open ? other_view->dir may be virtual, as indicated by
	//other_view->spacedata
	//add some func like e2_fs_chdir (view);
	if (other_view->spacedata != NULL)
	{
		if (e2_fs_chdir_local (other_view->spacedata->workplace E2_ERR_NONE()))
			return;
	}
	else
#else
	if (!e2_fs_chdir (other_view->dir E2_ERR_NONE()))
#endif
		return;

	temp = other_view;
	other_view = curr_view;
	curr_view = temp;
}
/* *
@brief UNUSED swap view pointers

Exchange curr_view and other_view,
without doing anything else

@return
*/
/*void e2_fileview_switch_views_simple (void)
{
	ViewInfo *temp = other_view;
	other_view = curr_view;
	curr_view = temp;
}*/
/**
@brief check whether the filesystem associated with @a view uses case-sensitive paths/names

Also updates the cached unum representing this property

@param view pointer to view data struct

@return TRUE if paths/names are case-sensitive (usually the case)
*/
gboolean e2_fileview_is_cased (ViewInfo *view)
{
	if (view->case_sensitive_names == E2FSCASE_UNKNOWN)
	{
#ifdef E2_VFSTMP
		if (view->case_sensitive_names == E2FSCASE_VIRTUAL)
		{
			figure it out
		}
		else
		{
#endif
			gchar *s = F_FILENAME_TO_LOCALE (view->dir); //assume no racy cd
#ifdef E2_VFS
			VPATH localpath = { s, view->spacedata };
			view->case_sensitive_names = e2_fs_path_case (&localpath);
#else
			view->case_sensitive_names = e2_fs_path_case (s);
#endif
			F_FREE (s, view->dir);
#ifdef E2_VFSTMP
		}
#endif
	}
	return (view->case_sensitive_names == E2FSCASE_SENSITIVE ||
			view->case_sensitive_names == E2FSCASE_SENSITIVENOW);
}
/**
@brief get indices of top line and left column shown in a filelist treeview
This gets scroll info for treeview rebuilds
@param view data structure for the view to be changed
@param leftcol pointer to store for the left column index
@param toprow pointer to store for the top row index

@return
*/
void e2_fileview_get_scroll_data (ViewInfo *view, gint *leftcol, gint *toprow)
{
	GtkTreePath *startpath;
	GtkTreeViewColumn *startcol;
//	gint cell_x, cell_y;
	if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (view->treeview),
		0, 0, &startpath, &startcol, NULL, NULL))	//&cell_x, &cell_y))
	{
		*toprow = *gtk_tree_path_get_indices (startpath);
		printd (DEBUG, "path found, row is %d", *toprow);
		gtk_tree_path_free (startpath);
	}
	else	//liststore is empty
		*toprow = 0;
//gtk bug, startcol always is the one at index 0 (maybe fixed after 2.8.6)
	*leftcol = 0;
}
/**
@brief scroll a filelist treeview to top line @a toprow and left column @a leftcol
This is used in treeview rebuilds
It does nothing if the treeview doesn't exist yet- i.e. sometimes when threaded
Assumes BGL is closed
@param view data structure for the view to be changed
@param leftcol left column index
@param toprow top row index

@return TRUE if the treeview exists and scrolling was needed
*/
gboolean e2_fileview_scroll_to_position (ViewInfo *view, gint leftcol, gint toprow)
{
	if (GTK_IS_TREE_VIEW (view->treeview))
	{
		if (toprow > 0 || leftcol > 0)
		{
			GtkTreePath* path;
			GtkTreeView *tvw = GTK_TREE_VIEW (view->treeview);
			GtkTreeModel *model = gtk_tree_view_get_model (tvw);
			GtkTreeViewColumn* col = (leftcol > 0) ? gtk_tree_view_get_column (tvw, leftcol) : NULL;
			if (toprow > 0)
			{
				GtkTreeIter iter;
				if (gtk_tree_model_iter_nth_child (model, &iter, NULL, toprow))
					path = gtk_tree_model_get_path (model, &iter);
				else
					path = gtk_tree_path_new_from_indices (
						gtk_tree_model_iter_n_children (model, NULL) - 1, -1);
			}
			else
				path = gtk_tree_path_new_first ();

//			WAIT_FOR_EVENTS
			gtk_tree_view_scroll_to_cell (tvw, path, col, FALSE, 0, 0);
			gtk_tree_path_free (path);
			return TRUE;
		}
	}
	return FALSE;
}

#ifdef E2_VFS
/**
@brief construct a guint hash for a vpath, for use in processing a hash table

The hash can be passed to g_hash_table_new() as the @hash_func parameter, when
using vpaths as keys
@param v pointer to the vpath key

@return a guint hash value corresponding to the key
*/
guint e2_fileview_vdir_hash (gconstpointer v)
{
	// 31 bit hash function, same as g_str_hash
	VPATH *vp = (VPATH *)v;
	const char *p = VPCSTR (vp);
	guint h = *p;

	if (h)
	{
		for (p += 1; *p != '\0'; p++)
			h = (h << 5) - h + *p;
	}

	if (vp->spacedata != NULL)
	{
		guint i, count = sizeof (gpointer);
		for (i = 0, p = (const char *)vp->spacedata; i < count; i++, p++)
			h = (h << 5) - h + *p;
	}
	return h;
}
/**
@brief check for equivalence of hash-table keys @a v1 and @a v2
@param v1 a VPATH key to compare with @a v2
@param v2 a VPATH key to compare with @a v1
@return TRUE if the keys match
*/
gboolean e2_fileview_vdir_equal (gconstpointer v1, gconstpointer v2)
{
	VPATH *vp1 = (VPATH *)v1;
	VPATH *vp2 = (VPATH *)v2;
	if (vp1->spacedata != vp2->space_data)
		return FALSE;
	return (!strcmp(vp1->path), vp2->path));
}
/**
@brief clear a key from vpaths hash table
@param k the key
@return
*/
void e2_fieview_vdir_free (gpointer k)
{
	VPATH *vp = (VPATH *)k;
	g_free ((gchar*)vp->path);
	DEALLOCATE (vpath, vp);
}
#endif
/**
@brief idle callback, to clean up redundant tree hash tables when there's
 nothing better to do
@param data pointer to table to be destroyed
@return FALSE so that this is not called again
*/
static gboolean _e2_fileview_treehash_free (GHashTable *data)
{
	printd (DEBUG, "Destroy treeview hash");
	g_hash_table_destroy (data);
	return FALSE;
}
/**
@brief create and populate a hashtable of selected-item names for the pane in @a view

Table keys are item names direct from the treeview FILENAME column i.e.
UTF-8 encoding, dirs have trailing '/'. Table values are all NULL.
The table is used as part of the view refresh or recreate processes.

@param view data structure for view being processed

@return hash table, or NULL
*/
GHashTable *e2_fileview_log_selected_names (ViewInfo *view)
{
	GtkTreeModel *model;
	GList *selpaths = gtk_tree_selection_get_selected_rows (view->selection, &model);
	if (selpaths != NULL)
	{
		//something selected now
		GList *rowpath;

		GHashTable *namehash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

		for (rowpath = selpaths; rowpath != NULL; rowpath = rowpath->next)
		{
			GtkTreeIter iter;
			if (gtk_tree_model_get_iter (model, &iter, rowpath->data))
			{
				gchar *itemname;
				gtk_tree_model_get (model, &iter, FILENAME, &itemname, -1);
				g_hash_table_insert (namehash, itemname, NULL);
			}
			gtk_tree_path_free (rowpath->data);
		}
		g_list_free (selpaths);

		return namehash;
	}
	return NULL;
}
/**
@brief re-select items in list store for @a view

@a view ->selected_names is freed here

@param view data structure for view being processed
@param selnames hash table of item-names to be reselected, or NULL
@param clean whether to clear any selection data after processing

@return
*/
void e2_fileview_reselect_names (ViewInfo *view, GHashTable *selnames, gboolean clean)
{
	if (selnames != NULL)
	{
		printd (DEBUG, "Reselect former selected items");
		GtkTreeIter iter;
		//may be filter model
		GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (view->treeview));
		if (gtk_tree_model_get_iter_first (model, &iter))
		{
			guint more_hits = g_hash_table_size (selnames);
			GtkTreeSelection *sel = view->selection;
			do
			{
				gchar *itemname;
				gpointer key;
				gpointer value;

				gtk_tree_model_get (model, &iter, FILENAME, &itemname, -1);
				if (g_hash_table_lookup_extended (selnames, itemname,
						&key, &value))
				{
					gtk_tree_selection_select_iter (sel, &iter);
					more_hits--;
				}
				g_free (itemname);
			} while (more_hits > 0 && gtk_tree_model_iter_next (model, &iter));
		}

		if (clean)
			//setup to get rid of hash when there's more time
			g_idle_add_full (G_PRIORITY_LOW,
				(GSourceFunc) _e2_fileview_treehash_free, selnames, NULL);
	}
	else
		printd (DEBUG, "NO reselection - no former selected items");
}
/**
@brief rename an item in the filelist related to @a view

This supports incremental changes to the rows selected in treeview for @a view.
Used when renaming an item.

@param view data structure for view being processed
@param oldname item basename to be replaced in @a view 's liststore, localised string
@param newname item basename to be substituted in @a view 's liststore,localised string
@param oldutf item basename to be replaced in @a view 's liststore, UTF-8 string
@param newutf item basename to be substituted in @a view 's liststore, UTF-8 string
@return
*/
void e2_fileview_adjust_name (ViewInfo *view, const gchar *oldname,
	const gchar *newname, const gchar *oldutf, const gchar *newutf)
{
	gchar *oldpath = e2_utils_dircat (view, oldname, FALSE);
#ifdef E2_VFS
	VPATH ddata = { oldpath, view->spacedata };
	gboolean isdir = e2_fs_is_dir3 (&ddata E2_ERR_NONE());
#else
	gboolean isdir = e2_fs_is_dir3 (oldpath E2_ERR_NONE());
#endif
	g_free (oldpath);

	//append '/' to directory name, to match string in filelist liststore
	gchar *oldfull = (isdir) ?
		e2_utils_strcat (oldutf, G_DIR_SEPARATOR_S) : (gchar *)oldutf;

	GtkTreeIter iter;
	gtk_tree_model_get_iter_first (view->model, &iter);
	if (e2_tree_find_iter_from_str_simple (view->model, FILENAME,
		oldfull, &iter, FALSE))
	{
		GtkTreeIter *ip;

		if (GTK_IS_TREE_MODEL_FILTER (view->model))
		{
			GtkTreeIter child;
			gtk_tree_model_filter_convert_iter_to_child_iter
				(GTK_TREE_MODEL_FILTER (view->model), &child, &iter);
			ip = &child;
		}
		else
			ip = &iter;
		//update visible name
		gchar *newfull = (isdir) ?
			e2_utils_strcat (newutf, G_DIR_SEPARATOR_S) : (gchar *)newutf;
		gtk_list_store_set (view->store, ip, FILENAME, newfull, -1);
		if (isdir)
			g_free (newfull);
		//update info->filename, to make this match during filelist refresh
		FileInfo *info;
		gtk_tree_model_get (view->model, &iter, FINFO, &info, -1);
		g_strlcpy (info->filename, newname, sizeof (info->filename));
	}
	if (isdir)
		g_free (oldfull);
}
/**
@brief [re]register all key-binding callbacks for a filelist treeview
There are also generic keypress-event bindings with 'higher priority' than these
@param treeview the widget being processed
@param view pointer to view data corresponding to @a treeview
@return
*/
void e2_fileview_register_keybindings (GtkWidget *treeview, ViewInfo *view)
{
	//other "key-press-event" connection(s) have non-NULL data
	//also, if needed, bindings cb func is _e2_keybinding_key_press_cb()
	guint id = g_signal_lookup ("key-press-event", GTK_TYPE_TREE_VIEW);
	g_signal_handlers_disconnect_matched (G_OBJECT (treeview),
		G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_DATA, id, 0, NULL, NULL, NULL);
	gchar *category = g_strconcat(_C(17),".",_C(23),".",_C(33),NULL);  //_("general.main.panes
	e2_keybinding_enrol (treeview, category, (void(*)(E2_OptionSet*))NULL); //connection may be sync or async, see E2_IDLE_KEYSYNC
	g_free (category);
}
#ifdef E2_MOUSECUSTOM
/**
@brief [re]register all button-event callbacks for a filelist treeview
This should be applied before 'normal' button-event callbacks are connected
@param treeview the widget being processed
@return
*/
void e2_fileview_register_pointerbindings (GtkWidget *treeview)
{
	gchar *category = g_strconcat(_C(17),".",_C(23),".",_C(33),NULL);  //_("general.main.panes
	//pick up any custom button-bindings before the default press/release
	e2_mousebinding_enrol (treeview, category, (void(*)(E2_OptionSet*))NULL);
# ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (treeview, category, (void(*)(E2_OptionSet*))NULL);
# endif
	g_free (category);
}
#endif
/**
@brief update all items' info in the treeview associated with @a view

After updating the underlying liststore contents, item(s) are (re)selected as
appropriate, and the view is scrolled to the relevant position
This is used for cd's, but not for refreshes
This is not necessarily thread-safe TODO
Assumes BGL is closed upon arrival here
Directory whose content we want is named in view->dir (UTF-8)
Current store content is cleared after successful preliminary things, but before
new data is added, of course
Active filter(s) are applied, but no sorting.
Access-reporting by kernel-FAM is turned off while stat()'ing here.
If relevant, the text-color for selected items is updated here, rather than in
the selection function (too many un-necessary repeats, there)
NOTE sets view->total_items, and view->row if needed

@param view data structure for the view to which the file list belongs

@return TRUE if all was completed
*/
gboolean e2_fileview_prepare_list (ViewInfo *view)
{
	GtkTreeModel *mdl;
	GtkTreeSortable *sortable;
	GList *entries;	//list of item-names, then FileInfo's, created when reading dir

//	printd (DEBUG, "e2_fileview_prepare_list");
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, prepare list");
#endif
	e2_filelist_disable_one_refresh ((view==curr_view)?PANEACTIVE:PANEINACTIVE);
	//if dir is already monitored, prevent downstream stat()'s causing ACCESS
	//reports and hence a refresh
	e2_fs_FAM_less_monitor_dir (view->dir);

	e2_window_set_cursor (GDK_WATCH);
#ifdef E2_VFS
	VPATH data;
	data.spacedata = view->spacedata;
#endif
//	GObject *debug = G_OBJECT (view->model);
//#ifdef E2_NEWREFRESH
	if (view->listcontrols.refreshtype == E2_CHANGE)
//#else
//	if (view->listcontrols.refreshtype == E2_REFRESH || view->listcontrols.refreshtype == E2_CHANGE)
//#endif
	{
//========== EXPERIMENTAL STORE-COPY PROCESS
#ifdef STORECOPY
//CHECKME rationalise code; confirm that filtering; refreshing are ok
		if (!strcmp (curr_view->dir, other_view->dir)
			&& curr_view->spacedata == other_view->spacedata)
		{
			ViewInfo *otherview = (view == curr_view) ? other_view:curr_view;
			//match only older refreshes
//			if (view->refreshtype == E2_CHANGE || otherview->dir_mtime < time (NULL))
//			{
				printd (DEBUG, "copy other store");
				GtkListStore *newstore = e2_filelist_copy_store (otherview->store);
				g_object_ref (G_OBJECT (view->store));
				//this has same effect as unref existing filtermodel
				gtk_tree_view_set_model (GTK_TREE_VIEW (view->treeview), NULL);
				//arrange to cleanup the old store at an idle time
				gboolean newtimer = (app.used_stores == NULL);
				app.used_stores = g_slist_append (app.used_stores, view->store);
				if (newtimer)
				{
					printd (DEBUG, "setup to clear stores later");
					g_idle_add (e2_filelist_clear_old_stores, NULL);
				}

				view->store = newstore;
				view->model = gtk_tree_model_filter_new (GTK_TREE_MODEL (newstore), NULL);
				g_object_unref (G_OBJECT (newstore));

				//apply current filtering
				view->filtered_before = FALSE;	//trigger re-attachment of filter func
				e2_fileview_refilter_list (view);

				//continue the current sorting arrangements
				sortable = GTK_TREE_SORTABLE (newstore);
				//allow non-sorted display using GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
		//		gtk_tree_sortable_set_default_sort_func (sortable, NULL, NULL, NULL);
				gint array_row = (view == app.pane1.view) ? 0 : 1;
				gint *order_array = displayed_col_order[array_row];
				gint i, m;
				for (i = 0; i < MAX_COLUMNS; i++)
				{
					m = order_array[i];  //get the model/'external' index
#ifdef E2_EXTCOL
//nothing to change here?
#endif
					if (m == FILENAME && view->extsort)
						gtk_tree_sortable_set_sort_func (sortable, FILENAME,
							(GtkTreeIterCompareFunc) e2_fileview_ext_sort,
								&view->sort_order, NULL);
					else
						gtk_tree_sortable_set_sort_func (sortable, m,
							e2_all_columns[m].sort_func, &view->sort_order, NULL);
				}
				//sort the store using the current sort column & direction
				gtk_tree_sortable_set_sort_column_id (sortable, view->sort_column,
					view->sort_order);
				//make it fully ours
				gtk_tree_view_set_model (GTK_TREE_VIEW (view->treeview), view->model);
//				g_object_unref (G_OBJECT (view->model)); BAD
				//setup for status-line counters
				view->total_items = otherview->total_items;

				view->case_sensitive_names = otherview->case_sensitive_names;
				view->dir_mtime = otherview->dir_mtime;
				view->dir_ctime = otherview->dir_ctime;
				e2_window_set_cursor (GDK_LEFT_PTR);
				//reinstate standard FAM
				e2_fs_FAM_more_monitor_dir (view->dir);
				e2_filelist_enable_one_refresh ((view==curr_view)?PANEACTIVE:PANEINACTIVE);
				return TRUE;
//			}
//			printd (DEBUG, "time-test failed");
		}
//		else	//panes have different dirs, or some error in copying
//		{
			printd (DEBUG, "use fresh store");
#endif //def STORECOPY
//============
			gchar *local = D_FILENAME_TO_LOCALE (view->dir); //always dup, to avoid dirchange race
			//construct relevant items data for store-filler
			//FIXME if we can, just get relevant items for a refresh
			//get a fresh set of FileInfo data from source
	//	printd (DEBUG, "grab current directory data");
#ifdef E2_VFS
			data.path = local;
			OPENBGL
			entries = (GList *)e2_fs_dir_foreach (&data,
#else
			OPENBGL
			entries = (GList *)e2_fs_dir_foreach (local, //get a list of item-names
#endif
				E2_DIRWATCH_CHECK,	//this is irrelevant for non-local dirs
				NULL, NULL, NULL E2_ERR_NONE());
//			printd (DEBUG, "Read dir function returned list @ %x", entries);
			CLOSEBGL

			if (E2DREAD_FAILED (entries) || !e2_filelist_make_all_infos (local, &entries))
			{
				g_free (local);
				//any cleanup of entries (possibly with mixed data types) done downstream
				goto cleanup2;
			}
//			printd (DEBUG, "current directory data loaded");
			LISTS_LOCK
			gboolean abort = (view->listcontrols.cd_requested ||
								view->listcontrols.newpath != NULL);	//different dir wanted now
			LISTS_UNLOCK
			if (abort)
			{
				//stop now
				g_free (local);
				goto cleanup;
			}

			//even when FAM is working, some things (e.g. treedialog) must poll for
			//changes, so set times reflecting the completed poll
			struct stat sb;
#ifdef E2_VFS
			data.path = local;
			if (e2_fs_stat (&data, &sb E2_ERR_NONE()))	//through links
#else
			if (e2_fs_stat (local, &sb E2_ERR_NONE()))	//through links
#endif
			{
				printd (WARN, "Unable to stat directory: %s", view->dir);
				g_free (local);
				goto cleanup;
			}
			g_free (local);
			//FIXME update these only after store is populated successfully
			view->case_sensitive_names = E2FSCASE_UNKNOWN;	//after cd, can be replaced by cached value
			view->dir_mtime = sb.st_mtime;
			view->dir_ctime = sb.st_ctime;

/*#ifndef E2_NEWREFRESH
# ifdef E2_VFSTMP
			if (view->spacedata != NULL && view->spacedata->monitored)
			{ 	//v-dir is monitored
				//FIXME
			}
			else if (view->spacedata != NULL)
			{
# endif
# ifdef E2_FAM_KERNEL
			if (view->refreshtype == E2_REFRESH)
				e2_fs_FAM_clean_reports (view->dir);
		/ *	prior reports about the dir are now gone.
			Henceforth any report about the dir or an item in it
			will trigger a new refresh, even if the change that
			caused the report is shown in this refresh.
			The alternative is to do this at the end, and lose any
			change after an item is processed in this refresh * /
# endif	//which fam
# ifdef E2_VFSTMP
			}
# endif
#endif	//ndef E2_NEWREFRESH
*/
	//		mdl = GTK_TREE_MODEL (view->store);
			sortable = GTK_TREE_SORTABLE (view->store);
			//remember the current sorting arrangements
			gtk_tree_sortable_get_sort_column_id (sortable, &view->sort_column,
				&view->sort_order);
			//turn off model sorting before any rows are added
			//CHECKME do we really benefit from turning off sorting when the
			//view is detached from the model?
			gtk_tree_sortable_set_sort_column_id (sortable,
				GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, view->sort_order);
//============
//#ifdef STORECOPY
//		}	//end of block for experimental stuff
//#endif
//============
	}
	else	//doing a window re-creation
	{	//create new store with current items, in case content-format is changed
		entries = NULL;
		FileInfo *infoptr, *newinfo;
		GtkTreeIter iter;
		mdl = GTK_TREE_MODEL (view->store);
		if (gtk_tree_model_get_iter_first (mdl, &iter))
		{
			do
			{
				gtk_tree_model_get (mdl, &iter, FINFO, &infoptr, -1);
#ifdef USE_GLIB2_10
				newinfo = (FileInfo *) g_slice_alloc (sizeof(FileInfo));
				memmove (newinfo, infoptr, sizeof(FileInfo));
#else
				newinfo = (FileInfo *) g_memdup (infoptr, sizeof(FileInfo));
#endif
				entries = g_list_prepend (entries, newinfo);	//faster
			} while (gtk_tree_model_iter_next (mdl, &iter));
			entries = g_list_reverse (entries);
		}
	}

	gboolean retval;
	GtkListStore *store = e2_filelist_fill_store (entries, view);
	if (store != NULL)
	{	//there was a valid store created
//	printd (DEBUG, "new liststore ready");
		//can't re-assign the filter model to new store, must replace model
		//make sure the store and base model stay alive after the filtermodel goes
		g_object_ref (G_OBJECT (view->store));
		view->model = gtk_tree_model_filter_new (GTK_TREE_MODEL (store), NULL);
		gtk_tree_view_set_model (GTK_TREE_VIEW (view->treeview), view->model);
		g_object_unref (G_OBJECT (store));
		g_object_unref (G_OBJECT (view->model));
		//arrange to cleanup the old store at an idle time
		gboolean newtimer = (app.used_stores == NULL);
		app.used_stores = g_slist_append (app.used_stores, view->store);
		if (newtimer)
		{
			printd (DEBUG, "setup to clear stores later");
			g_idle_add (e2_filelist_clear_old_stores, NULL);
		}
		view->store = store;
		sortable = GTK_TREE_SORTABLE (store);
		//allow non-sorted display using GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
//		gtk_tree_sortable_set_default_sort_func (sortable, NULL, NULL, NULL);
		gint array_row = (view == &app.pane1.view) ? 0 : 1;
		gint *order_array = displayed_col_order[array_row];
		gint i, m;
		for (i = 0; i < MAX_COLUMNS; i++)
		{
			m = order_array[i];  //get the model/'external' index
#ifdef E2_EXTCOL
//nothing to change here?
#endif
			if (m == FILENAME && view->extsort)
				gtk_tree_sortable_set_sort_func (sortable, FILENAME,
					(GtkTreeIterCompareFunc) e2_fileview_ext_sort,
						&view->sort_order, NULL);
			else
				gtk_tree_sortable_set_sort_func (sortable, m,
					e2_all_columns[m].sort_func, &view->sort_order, NULL);
		}

//	printd (DEBUG, "resort new data");
		//sort the view using the current sort column & direction
		gtk_tree_sortable_set_sort_column_id (sortable, view->sort_column,
			view->sort_order);
//	printd (DEBUG, "resort finished");
		//setup for status-line counters
		//FIXME if entries list can be just the changed items
		//entries count adjusted for ".." ("." filtered from list)
		view->total_items = g_list_length (entries) - 1;	//may be -1
		//fuse-created dirs may not have any ".." entry, but we need one
/*
		gboolean addup = FALSE;
		FileInfo *infoptr;
		GtkTreeIter iter;
		//sorting will have placed any existing ".." at start of store
		//FIXME neater to do this in read-dir function
		if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (store), &iter))
		{
			gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, FINFO, &infoptr, -1);
			if (strcmp (infoptr->filename, ".."))
				addup = TRUE;
		}
		else
			addup = TRUE;
		if (addup)
		{
#ifdef USE_GLIB2_10
			infoptr = (FileInfo *) g_slice_alloc (sizeof (FileInfo));
//			infoptr = ALLOCATE (FileInfo);
#else
			infoptr = ALLOCATE (FileInfo);
#endif
			CHECKALLOCATEDFATAL (infoptr);
			g_strlcpy (infoptr->filename, "..", sizeof (infoptr->filename));
#ifdef E2_VFSTMP
			//FIXME properly handle root of v-dir
#endif
#ifdef E2_VFS
			VPATH fake = { G_DIR_SEPARATOR_S, view->spacedata };
			e2_fs_stat (&fake, &infoptr->statbuf E2_ERR_NONE());
#else
			e2_fs_stat (G_DIR_SEPARATOR_S, &infoptr->statbuf E2_ERR_NONE());
#endif
			gtk_list_store_insert_with_values (store, &iter, 0,
				FILENAME, "../",
#ifdef E2_EXTCOL
				EXTN, "",
#endif

				SIZE, "0",
				PERM, "drwxrwxrwx", //CHECKME translate ?
				OWNER, "",
				GROUP, "",
				MODIFIED, "",
				ACCESSED, "",
				CHANGED, "",
				NAMEKEY, g_utf8_collate_key ("..", -1),
				FINFO, infoptr,
				FORECOLOR, e2_option_color_get ("color-ft-dir"),
//				VISIBLE, TRUE,
				-1);
			view->total_items++;
		}
*/
		view->filtered_before = FALSE;	//trigger re-attachment of filter func
		e2_fileview_refilter_list (view);
//FIXME	view->dir_mtime = sb.st_mtime;
//		view->dir_ctime = sb.st_ctime;
		retval = TRUE;
	}
	else //liststore creation error
	{
//		FIXME warn user
		printd (WARN, "liststore creation failed for %s", view->dir);
cleanup:
		if ((gpointer)entries >= GUINT_TO_POINTER (E2DREAD_ELAST))
			g_list_foreach (entries, (GFunc) e2_filelist_cleaninfo, NULL);
cleanup2:
		retval = FALSE;
	}
	if ((gpointer)entries >= GUINT_TO_POINTER (E2DREAD_ELAST))
		g_list_free (entries);	//data in the list is in liststore pointers, freed later

	e2_window_set_cursor (GDK_LEFT_PTR);
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, prepare list");
#endif
	//reinstate standard FAM
	e2_fs_FAM_more_monitor_dir (view->dir);
	e2_filelist_enable_one_refresh ((view==curr_view)?PANEACTIVE:PANEINACTIVE);
//	printd (DEBUG, "end prepare list for %s", view->dir);
	return retval;
}
/* *
@brief change-directory completion-watch timer-function

@param data pointer to data relating to the cd being monitored

@return FALSE when ready to shutdown timer
*/
/*
gboolean e2_fileview_cd_watch (E2_CDwatch *data)
{
	if (app.timers[CDWAIT_T] > 0)
		return TRUE;	//the cd is still blocked
	//can't use E2_PaneRuntime* directly, due to include circularity problem
	E2_PaneRuntime *rt = (data->view == curr_view) ? curr_pane : other_pane;
	if (!cd_blocked && g_str_has_prefix (rt->path, data->newpath))
	{
		*data->completed_flag = CD_SUCCESS;
		g_free (data->newpath);
		DEALLOCATE (E2_CDwatch, data);
		return FALSE;
	}
	//stop after 100 tries = 10 seconds
	//FIXME use a timeout mechanism consistent with the one for non-local dir reading
	if (++data->repeats == 100)
	{
		*data->completed_flag = CD_TIMEOUT;
		g_free (data->newpath);
		DEALLOCATE (E2_CDwatch, data);
		return FALSE;
	}
	return TRUE;
}
*/
/**
@brief change-directory timer-function
This can be called for both dirs, at session start at least
It handles any requested cd for either pane
@param data pointer to cd data for the change

@return TRUE if prior cd is not complete, or FALSE to shutdown timer
*/
gboolean e2_fileview_cd_manage (E2_Listman *data)
{
	E2_Listman *workdata;
	gchar *dirnow;
	pthread_t thisID;
	pthread_attr_t attr;
	gboolean one_active, flag;
	guint fails = 0;

	LISTS_LOCK
	flag = app.pane1.view.listcontrols.cd_requested || app.pane2.view.listcontrols.cd_requested;
	data->cd_requested = TRUE;	//mark this one to block any repeats
	printd (DEBUG, "managing cd to %s", data->newpath);
	LISTS_UNLOCK
	if (flag)
	{
		//this is a timer callback which escaped the termination process below
		//or (unlikely) is for the former-other pane and happened to arrive here
		//at a useless moment
		printd (NOTICE, "aborted cd timer, another one has precedence");	//, data->timer);
		return FALSE;
	}
	//immediately stop the timer which called here, to prevent _any_ chance of
	//parallel instances
	while (g_source_remove_by_user_data (data))
	{
		printd (DEBUG, "shut down a cd-timer for this pane");
	}

	e2_utf8_set_name_conversion_if_requested ();

//#ifndef E2_STATUS_DEMAND
//	e2_window_disable_status_update ();	//prevents bad behaviour ??
//#endif
	//in case the active pane changes while we're here, get a snapshot
	one_active = (curr_view == &app.pane1.view);
	do
	{
		//try active pane first
		workdata = (one_active) ? &app.pane1.view.listcontrols : &app.pane2.view.listcontrols;
		LISTS_LOCK
		if (workdata->cd_requested)
		{
			dirnow = (one_active) ? app.pane1.view.dir : app.pane2.view.dir;
			if (strcmp (workdata->newpath, dirnow) == 0)
			{
				LISTS_UNLOCK
				workdata->cd_requested = FALSE;
			}
			else if (!(
				g_atomic_int_get (&workdata->cd_working)
			 || g_atomic_int_get (&workdata->refresh_working)
#ifdef E2_STATUS_BLOCK
			 || g_atomic_int_get (&app.status_working)
#endif
			))
			{
				LISTS_UNLOCK
				printd (NOTICE, "Changing active pane to %s", workdata->newpath);
				pthread_attr_init (&attr);
				pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);

				if (pthread_create (&thisID, &attr,
					(gpointer(*)(gpointer))_e2_fileview_change_dir, workdata) == 0)
				{
					printd (DEBUG,"change-dir-thread (ID=%lu) started", thisID);
					//do this here too in case the thread gets underway later
					LISTS_LOCK
					workdata->cd_requested = FALSE;
					LISTS_UNLOCK
				}
				else
					fails++;
				pthread_attr_destroy (&attr);
			}
			else	//busy
				if ( (one_active && g_atomic_int_get (&app.pane2.view.listcontrols.cd_working))
				  ||(!one_active && g_atomic_int_get (&app.pane1.view.listcontrols.cd_working)) )
			{
//				if ( (one_active && app.pane2.view.listcontrols.timer == 0)
//				  ||(!one_active && app.pane1.view.listcontrols.timer == 0) )
//				{	//there is no active timer for the other pane (should never happen)
					printd (DEBUG,"active pane blocakge, restarting timer");
					//data->timer =
					g_timeout_add_full (G_PRIORITY_HIGH, 90,
						(GSourceFunc) e2_fileview_cd_manage, data, NULL);
//				}
				LISTS_UNLOCK
				return FALSE;	//wait for a timer to call back here
			}
			else
			{
				LISTS_UNLOCK
			}
		}
		else
		{
			LISTS_UNLOCK
		}

		//then try inactive pane
		workdata = (one_active) ? &app.pane2.view.listcontrols : &app.pane1.view.listcontrols;
		LISTS_LOCK
		if (workdata->cd_requested)
		{
			dirnow = (one_active) ? app.pane2.view.dir : app.pane1.view.dir;
			if (strcmp (workdata->newpath, dirnow) == 0)
			{
				LISTS_UNLOCK
				workdata->cd_requested = FALSE;
			}
			else if (!(
			   g_atomic_int_get (&workdata->cd_working)
			|| g_atomic_int_get (&workdata->refresh_working)
#ifdef E2_STATUS_BLOCK
			|| g_atomic_int_get (&app.status_working)
#endif
			))
			{
				LISTS_UNLOCK
				printd (NOTICE, "Changing INactive pane to %s", workdata->newpath);
				pthread_attr_init (&attr);
				pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);
				if (pthread_create (&thisID, &attr,
					(gpointer(*)(gpointer))_e2_fileview_change_dir, workdata) == 0)
				{
					printd (DEBUG,"change-dir-thread (ID=%lu) started", thisID);
					LISTS_LOCK
					workdata->cd_requested = FALSE;
					LISTS_UNLOCK
				}
				else
					fails++;
				pthread_attr_destroy (&attr);
			}
			else	//busy
				if ( (one_active && g_atomic_int_get (&app.pane1.view.listcontrols.cd_working))
				  ||(!one_active && g_atomic_int_get (&app.pane2.view.listcontrols.cd_working)) )
			{
//				if ( (one_active && app.pane1.view.listcontrols.timer == 0)
//				  ||(!one_active && app.pane2.view.listcontrols.timer == 0) )
//				{	//there is no active timer for the other pane (should never happen)
					printd (DEBUG,"INactive pane blocakge, restarting timer");
					//data->timer =
					g_timeout_add_full (G_PRIORITY_HIGH, 90,
						(GSourceFunc) e2_fileview_cd_manage, data, NULL);
//				}
				LISTS_UNLOCK
				return FALSE;	//wait for a timer to call back here
			}
			else
			{
				LISTS_UNLOCK
			}
		}
		else
		{
			LISTS_UNLOCK
		}

		LISTS_LOCK
		flag = app.pane1.view.listcontrols.cd_requested || app.pane2.view.listcontrols.cd_requested;
		LISTS_UNLOCK
	} while (flag && fails < 2);

	if (flag && fails == 2)
	{
//		gchar *msg = ;
//		CLOSEBGL
//		e2_output_print_error (msg, FALSE);
//		OPENBGL
		printd (WARN, "CD-thread creation error");
	}

//#ifndef E2_STATUS_DEMAND
//	printd (DEBUG,"resume status checking");
//	e2_window_enable_status_update (-1);
//#endif
//	LISTS_LOCK
//	data->timer = 0;	//now it's ok to allow other timers
//	LISTS_UNLOCK

	return FALSE;
}
/**
@brief get new directory contents for a view, per contents of @a cddata
This is a thread-function.
Assumes that BGL ("big gtk lock", gdk mutex) is off/open upon arrival here.

@param cddata data structure with parameters for the view to be changed

@return NULL always
*/
static gpointer _e2_fileview_change_dir (E2_Listman *cddata)
{
	E2_DirHistoryEntry *hist_entry;
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, _e2_fileview_change_dir");
#endif
	e2_filelist_disable_one_refresh ((cddata == &app.pane1.view.listcontrols) ?
		PANE1:PANE2); //suspend overlapping refreshes ASAP
	e2_utils_block_thread_signals ();	//block all allowed signals to this thread
	LISTS_LOCK
	gchar *newpath = cddata->newpath;	//duplicate in case user cd's again, free it if cd fails
	//with repeated fast cd's, sometimes we arrive here with a NULL path still in place
	if (newpath == NULL)
	{
		LISTS_UNLOCK
		e2_filelist_enable_one_refresh ((cddata == &app.pane1.view.listcontrols) ?
			PANE1:PANE2);
		return NULL;
	}
	cddata->newpath = NULL;	//from now, non-NULL signals a new cd request
	//CHECKME ok to overlap calls, hence defer setting these flags ?
	g_atomic_int_set (&cddata->cd_working, 1);	//prevent re-entrant cd's
	cddata->cd_requested = FALSE;	//this may preceed the change in e2_fileview_cd_manage()
	ViewInfo *view = cddata->view;
	gboolean history = cddata->history;
	gboolean hook = cddata->hook;
	LISTS_UNLOCK
	printd (DEBUG, "_e2_fileview_change_dir() %s to %s", view->dir, newpath);
/*
#ifdef E2_VFSTMP
	//FIXME better check for usable dir
	//also, handle vfs flags and local dirs that only use async IO
	//some OS have MNT_SYNCHRONOUS and MNT_ASYNC in a statfs.f_flags
	if (view->spacedata == NULL)
	{	//not a v-dir
#endif
		if (e2_fs_dir_is_native (newpath, FALSE)) CHECKME parent-checking too?
			view->dirtype = FS_LOCAL;
		else
			view->dirtype = FS_FUSE;
#ifdef E2_VFSTMP
	}
#endif
*/
	E2_PaneRuntime *rt = (view == curr_view) ? curr_pane : other_pane;

#ifdef E2_VFSTMP
	//FIXME check that e2_fs_cd_isok works for v-dirs
#endif
	CLOSEBGL	//e2_fs_cd_isok () may display message
	if (! e2_fs_cd_isok (newpath E2_ERR_NONE())) //can't go there CHECKME no delay when non-mounted ?
	{
		OPENBGL
		g_free (newpath);
//		LISTS_LOCK
		g_atomic_int_set (&cddata->cd_working, 0);
//		LISTS_UNLOCK
		e2_filelist_enable_one_refresh ((view == &app.pane1.view) ? PANE1:PANE2);
//		e2_pane_flag_history (rt, FALSE); //zap any history update request
		return NULL;
	}
#ifndef SYNC_DEBUG
	OPENBGL
#endif

#ifndef E2_STATUS_BLOCK
	if (view == curr_view)
		e2_window_disable_status_update ();	//prevents bad behaviour ??
#endif

	//update row history for current dir
	//at session-start, view->dir = "", hence no match found in list
	//this update will be redundant if the cd fails or is aborted, but that's ok
	//we do it now anyway, to take up some time in case a competing activity is underway
#ifdef E2_VFSTMP
	//CHECKME confirm v-dir history list is still in place
	VPATH ddata = { view->dir, view->spacedata }; //CHECKME path encoding ?
	VfsIface *vi = (VfsIface*) e2_plugins_get_installed ("vfs"VERSION);
	VPATH *oldvdir = g_hash_table_lookup (vi->vdir_cache, &ddata);
	if (G_LIKELY(oldvdir != NULL))
		hist_entry = g_hash_table_lookup (app.dir_history, oldvdir);
	else
		hist_entry = NULL;
#else
	hist_entry = g_hash_table_lookup (app.dir_history, view->dir);
#endif
	if (G_LIKELY (hist_entry != NULL)) //should never fail
	{
		/*even though the sort-order and/or dir contents
		  might change before we re-visit here,
		  if there is a selection now, remember its start,
		  or else remember the start of the current display*/

		/*this func may be called during a window-rebuild, in which
		   case the old dir will the same as the new one, and the
		   selection and visible rect data will be meaningless*/

		GtkTreePath *path;
		if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (view->treeview),
			0, 0, &path, NULL, NULL, NULL))
		{
			hist_entry->toprow = *gtk_tree_path_get_indices (path);
			gtk_tree_path_free (path);
		}
		else	//dir is empty
			hist_entry->toprow = 0;

		GtkTreeSelection *sel = view->selection;
		GtkTreeModel *model;
		GList *selpaths = gtk_tree_selection_get_selected_rows (sel, &model);
		if (selpaths != NULL)
		{
			GtkTreeIter iter;
			path = (GtkTreePath *) selpaths->data;
			//get the 1st selected item's row
			hist_entry->selrow = *gtk_tree_path_get_indices (path);
//			if (0) TAGGING ALL OF SELECTION
//			{
#ifdef TAG_ALL
				GList *rowpaths;

				if (hist_entry->selitems != NULL)
					g_hash_table_destroy (hist_entry->selitems);
				hist_entry->selitems = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);

				for (rowpaths = selpaths; rowpaths != NULL; rowpaths = rowpaths->next)
				{
					FileInfo *info;

					path = (GtkTreePath *) rowpaths->data;
					if (gtk_tree_model_get_iter (model, &iter, path))
					{
						gtk_tree_model_get (model, &iter, FINFO, &info, -1);
						//info doesn't persist
						g_hash_table_insert (hist_entry->selitems, g_strdup(info->filename), GINT_TO_POINTER(1));
					}
					gtk_tree_path_free (path);
				}
//			}
//			else
//			{	//just 1st item's name
#else
				if (gtk_tree_model_get_iter (model, &iter, path))
				{
					FileInfo *info;
					gtk_tree_model_get (model, &iter, FINFO, &info, -1);
					g_strlcpy (hist_entry->firstname, info->filename,
						sizeof(hist_entry->firstname));
				}
				g_list_foreach (selpaths, (GFunc) gtk_tree_path_free, NULL);
#endif
//			}

			g_list_free (selpaths);
		}
		else
		{
			hist_entry->selrow = -1;
			*hist_entry->firstname = '\0';
//			if (TAGGING ALL OF SELECTION)
//			{
#ifdef TAG_ALL
				if (hist_entry->selitems != NULL)
				{
					g_hash_table_destroy (hist_entry->selitems);
					hist_entry->selitems = NULL;
				}
#endif
//			}
		}

		if (view->case_sensitive_names & E2FSCASE_NOCACHE)
			hist_entry->case_sensitive_names = E2FSCASE_UNKNOWN;
		else
			hist_entry->case_sensitive_names = view->case_sensitive_names;
	}

/*checks for overlapping cd/refresh/recreate now done externally
	//pause until any in-progess e2_fileview_prepare_list() is completed
	//CHECKME should this involve some test for actual-treeview-completion ?
	LISTS_LOCK
	if (cddata->listing)
	{
		LISTS_UNLOCK
		printd (DEBUG, "waiting for completion of another treeview update before changing dir");
		app.timers[REFRESHWAIT_T] = g_timeout_add (100,
			(GSourceFunc) _e2_fileview_check_completion, &cddata->listing);
		gtk_main ();
	}
	LISTS_UNLOCK
*/
	//now update the local path store, with backup in case of error
#ifdef E2_VFSTMP
	ddata.path = newpath;	//CHECKME path encoding ?
	ddata.spacedata = view->spacedata;
	CRAP TEST
	if (g_hash_table_lookup (vi->vdir_cache, &ddata) == NULL)
	{
		//CHECKME
		vpath *newvdir = ALLOCATE (vpath);
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (newvdir, FIXME;)
#else
		if (newvdir != NULL)
		{
#endif
			*newvdir = ddata;
			g_hash_table_insert (vi->vdir_cache, newvdir, GINT_TO_POINTER(1));
#if !(CHECKALLOCATEDWARN)
		}
#endif
	}
#endif
	gchar *oldpath = g_strdup (view->dir);
//E2_VFSTMPOK
	LISTS_LOCK
#ifdef E2_VFS
	view->vdir = newvdir;
//	view->spacedata = NO CHANGE;
#endif
	g_strlcpy (view->dir, newpath, sizeof (view->dir));
	cddata->refreshtype = E2_CHANGE;	//signal for list creator
	LISTS_UNLOCK

#ifndef SYNC_DEBUG
	CLOSEBGL
#endif

	//get the replacement filelist
	if (!e2_fileview_prepare_list (view))
	{
		OPENBGL
		printd (WARN, "prepare list failed");
#ifdef E2_VFS
		view->vdir = oldvdir;
//		view->spacedata = NO CHANGE;
#endif
		g_strlcpy (view->dir, oldpath, sizeof (view->dir));
		g_free (oldpath);
		g_free (newpath);
//		e2_pane_flag_history (rt, FALSE); //zap any history update request
	}
	else //prepare list succeeded
	{
//		printd (DEBUG, "prepare list succeeded");
		g_free (oldpath);
		//update window title to new directory if wanted
		e2_window_set_title_path (app.main_window, view);
		//update dirline to new path (and also dirline history if appropriate)
		if (rt->toolbar.has_command_line)
			e2_command_line_change_dir (newpath, rt); //expects BGL on
		//scroll to approximately the position from last time
		view->row = 0;	//default position
		GtkTreeModel *mdl = view->model;
		GtkTreeIter iter;
		if (gtk_tree_model_get_iter_first (mdl, &iter))
		{
#ifdef E2_VFSTMP
			//CHECKME confirm v-dir history list is still in place
			hist_entry = g_hash_table_lookup (app.dir_history, newvdir);
#else
			hist_entry = g_hash_table_lookup (app.dir_history, view->dir);
#endif
			gboolean flag = e2_option_bool_get ("select-first-item");
			if (G_LIKELY (hist_entry != NULL))
			{  //new dir is in history list
				GtkTreePath *path;

				view->case_sensitive_names = hist_entry->case_sensitive_names;

				if (hist_entry->selrow > -1)
				{
					//during unpacking of an archive etc all data from last time may not be there
					//don't select or scroll if not enough data now (CHECKME items - 1 instead ?
					gint items =
					//if (
						gtk_tree_model_iter_n_children (mdl, NULL);
					if (items	//0,1,... 1-based
						<= hist_entry->selrow //0,1,... 0-based
						)
					{
						hist_entry->selrow = -1;
						if (items > 0)
						{
							e2_fileview_focus_row (view, items-1, FALSE, FALSE, FALSE, FALSE);
						}
					}

					if (hist_entry->selrow > -1)
					{
						GtkTreeSelection *sel = view->selection; //FIXME share these variables
						GtkTreeModel *model = view->model;
//						if (RESELECT EVERYTHING POSSIBLE)
//						{
#ifdef TAG_ALL
							do
							{
								FileInfo *info;
								gtk_tree_model_get (model, &iter, FINFO, &info, -1);
								//We only check for name, no other statbuf parameters are stored ATM
								if (g_hash_table_lookup(hist_entry->selitems, info->filename) != NULL)
									gtk_tree_selection_select_iter (sel, &iter);
							} while (gtk_tree_model_iter_next (model, &iter));
//						}
//						else
//						{
#else
							do
							{
								FileInfo *info;
								gtk_tree_model_get (model, &iter, FINFO, &info, -1);
								//We only check for name, no other statbuf parameters are stored ATM
								if (strcmp(hist_entry->firstname, info->filename) == 0)
								{
									gtk_tree_selection_select_iter (sel, &iter);
									break;
								}
							} while (gtk_tree_model_iter_next (model, &iter));
#endif
//						}
						gint selrow;
						//get the 1st selected item's row
						GList *selpaths = gtk_tree_selection_get_selected_rows (sel, &model);
						if (selpaths != NULL)
						{
							path = (GtkTreePath *) selpaths->data;
							/*hist_entry->*/selrow = *gtk_tree_path_get_indices (path);
							g_list_foreach (selpaths, (GFunc) gtk_tree_path_free, NULL);
							g_list_free (selpaths);
						}
						else
							selrow = -1;

						//select the 1st row we selected last time
						//(tuff luck if there's been content change or sort-order change)
						//can't use e2_fileview_focus_row () as that is bad near end of list
						//move back past any filtered-out iter
						while (/*hist_entry->*/selrow > -1 &&
							!gtk_tree_model_iter_nth_child (mdl, &iter, NULL, /*hist_entry->*/selrow)
							  )
								/*hist_entry->*/selrow --;
						if (/*hist_entry->*/selrow > -1)
						{
							view->row = /*hist_entry->*/selrow;
							path = gtk_tree_model_get_path (mdl, &iter);
							if (view->row > 0)
								gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view->treeview),
										path, NULL, TRUE, 0.382, 0.0);
#ifndef TAG_ALL
							gtk_tree_view_set_cursor (GTK_TREE_VIEW (view->treeview),
									path, NULL, FALSE);
#endif
							gtk_tree_path_free (path);
						}
						else
						{
							view->row = 0;	//default value
						}
					}
				}
				else if (hist_entry->toprow > 0)
				{
					//goto (approximately) the top of the visible window as it was last time
					while (!gtk_tree_model_iter_nth_child (mdl, &iter, NULL, hist_entry->toprow)
						&& hist_entry->toprow > 0)
							hist_entry->toprow --;
					if (hist_entry->toprow > 0)
					{
						path = gtk_tree_model_get_path (mdl, &iter);
						//focus before scrolling, or else scroll doesn't work properly
						gtk_tree_view_set_cursor (GTK_TREE_VIEW (view->treeview),
								path, NULL, FALSE);
						gtk_tree_selection_unselect_path (view->selection, path);
						gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view->treeview),
							path, NULL, TRUE, 0.0, 0.0);
						gtk_tree_path_free (path);
					}
					else
						e2_fileview_focus_row (view, 0, FALSE, FALSE, FALSE, FALSE);
				}
				else //last time here, it was empty
					e2_fileview_focus_row (view, 0, flag, FALSE, FALSE, FALSE);
			}
			else //the new dir has never been opened in this session
				e2_fileview_focus_row (view, 0, flag, FALSE, FALSE, FALSE);
		}


#ifdef SYNC_DEBUG
		//don't wait for gtk to update at idle-time
		gdk_window_process_updates (
#ifdef USE_GTK2_14
			gtk_widget_get_window (view->treeview),
#else
			view->treeview->window,
#endif
			TRUE);
#endif
		OPENBGL

#ifdef E2_STATUS_REF
		extern gint statref_count;
#endif
		if (view == curr_view
#ifdef E2_STATUS_REF
			&& statref_count == 0
#endif
#ifdef E2_STATUS_BLOCK
			&& g_atomic_int_get (&app.status_working) == 0
#endif
		)
			e2_window_update_status_bar (NULL);	//expects BGL off

		//ancillary tasks

		//this is calculated instead of passed, as circularity prevents a
		//E2_PaneRuntime * in any declaration in e2_fileview.h
//		E2_PaneRuntime *rt = (view == curr_view) ? curr_pane : other_pane;
		//clear any pending refresh hanging around from the old dir
		//CHECKME kernel reports too ?
		g_atomic_int_set (&cddata->refresh_requested, 0);
		//update path data for cache etc
		//change FAM place ASAP
#ifdef E2_VFSTMP
		//v-dir monitoring when supported
		if (view->spacedata != NULL && view->spacedata->monitored)
		{
			LISTS_LOCK
			oldpath = rt->path;
			rt->path = newpath;	//DON'T FREE ME
			LISTS_UNLOCK
			//startup alteration-monitoring for the new dir
			//(or if need be, for its target if it's a link)
	//FIXME e2_vfs_?? (oldpath, rt);
			g_free (oldpath);
		}
		else if (view->spacedata == NULL)
		{	//local dir
#endif
#ifdef E2_FAM
			//startup alteration-monitoring for the new dir
			//(or if need be, for its target if it's a link)
			e2_fs_FAM_change (rt->path, rt);
#endif	//def E2_FAM
			//now it's safe to update the path store for cache etc
			LISTS_LOCK
			g_free (rt->path);
			rt->path = newpath;
			LISTS_UNLOCK
#ifdef E2_VFSTMP
		}
#endif
		last_work_time = time (NULL); //update the suspend-timer setting

		if (view == curr_view)
		{
			//run refocus hooklist (BGL open)
#ifdef E2_VFSTMP
			//CHECKME hook for v-dir
#else
			e2_hook_list_run (&app.hook_pane_focus_changed, curr_pane);
#endif
		}

		//run cd hooklist (BGL open)
		if (hook
#ifdef E2_VFSTMP
	//CHECKME mkdir dialog for v-dir's
			&& (view->spacedata != NULL
			|| view->spacedata->dirtype == FS_LOCAL
			|| view->spacedata->dirtype == FS_FUSE)
#endif
			)
		{
#ifdef E2_VFSTMP
			//FIXME hook process for v-dir's
#else
			e2_hook_list_run (&rt->hook_change_dir, newpath);	//not rt->path if that can be altered in any other thread
#endif
		}

#ifdef E2_VFSTMP
		//CHECKME relevant list for v-dir still in place
#endif
//		printd (DEBUG, "_e2_fileview_change_dir update history");
		//update goto-line-after-opening data
#ifdef E2_VFSTMP
		hist_entry = g_hash_table_lookup (app.dir_history, newvdir);
#else
		hist_entry = g_hash_table_lookup (app.dir_history, view->dir);
#endif
		if (hist_entry == NULL)
		{  //need a new entry for the history list
			hist_entry = ALLOCATE0 (E2_DirHistoryEntry);
			CHECKALLOCATEDWARNT (hist_entry, );
			if (hist_entry != NULL)
			{
				//just set the [v]path, for now - other data set when dir is departed
				g_strlcpy (hist_entry->path, view->dir, sizeof(hist_entry->path));
#ifdef E2_VFS
				hist_entry->spacedata = view->spacedata;
#endif
				g_hash_table_insert (app.dir_history,
#ifdef E2_VFS
					newvdir,
#else
					g_strdup (view->dir),
#endif
					hist_entry);
			}
		}

#ifdef E2_VFSTMP
		//CHECKME relevant list for v-dir still in place
#endif
		if (history)
		{	//update the pane's opened-dirs history
			//(apart from any other limitations, it should be a dir opened
			//in-session i.e. not at session-start, and not a history
			//forward/backward move
			GList *tmp;
			guint len;
			HISTORY_LOCK
			if (rt->opendirs != NULL)
			{
				/*reverse order of items before and including (i.e. open now or
				opened after) current place in history
				e.g. history-index  6 5 4 3 2 1 0
					 list-position  0 1 2 3 4 5 6
					current-index       4
                     open dir
		new-order of old indices    7 4 5 6 3 2 1 0
					new list       	0 1 2 3 4 5 6 7
				i.e. items for old indices >= current (list positions <= current)
				are reversed, prepended to items for positions after current,
				new one is prepended (unless that would make 2 adjacent items the
				same) and last one is made current
				*/
				len = g_list_length (rt->opendirs); //say 7
				tmp = g_list_nth (rt->opendirs, len - rt->opendir_cur - 1); //say item@2
				if (tmp->prev != NULL) //item@1 exists
				{	//split the list
					GList *tail = tmp->next; //say item@3
					if (tail != NULL)
					{
						tmp->next = NULL;
						tail->prev = NULL;
						rt->opendirs = g_list_reverse (rt->opendirs);
						rt->opendirs = g_list_concat (rt->opendirs, tail);
					}
				}
			}
			else
				len = 0;
			if (len == 0 ||
				//don't want 2 adjacent items the same
				strcmp ((gchar *)hist_entry, (gchar *)rt->opendirs->data) != 0)
			{
				rt->opendirs = g_list_prepend (rt->opendirs, hist_entry);
				len++;
			}
			//enforce total-length limit
			if (len > 40)
			{
				tmp = g_list_last (rt->opendirs);
				//do not clear the data, it's shared with app.dir_history
				rt->opendirs = g_list_delete_link (rt->opendirs, tmp);
				len--;
			}
			rt->opendir_cur = len - 1;
			HISTORY_UNLOCK
		}
	}

#ifndef E2_STATUS_BLOCK
	if (view == curr_view)
		e2_window_enable_status_update (-1);	//revert disable which prevents bad behaviour ??
#endif
//	LISTS_LOCK
	g_atomic_int_set (&view->listcontrols.cd_working, 0);
//	LISTS_UNLOCK

#ifdef DEBUG_MESSAGES
	if (app.reconvert_requested)
		printd (DEBUG, "repoint encoding conversion funcs if necessary, after CD");
	else
		printd (DEBUG, "NO need for encoding conversion change after CD");
#endif

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, change-dir");
#endif
	e2_filelist_enable_one_refresh ((view == &app.pane1.view) ? PANE1:PANE2);

	printd (DEBUG, "_e2_fileview_change_dir ends");
	return NULL;
}
/**
@brief create filelist for the pane related to @a view

This creates the backend list store, and scrolled window
containing a treeview associated with the store

@param view runtime data for view being processed

@return the 'parent' scrolled-window widget
*/
GtkWidget *e2_fileview_create_list (ViewInfo *view)
{
	//create liststore framework for the pane
	view->store = e2_filelist_make_store ();
	//setup column-order array
	gint array_row = (view == &app.pane1.view) ? 0 : 1;
	e2_fileview_translate_cols_array (stored_col_order[array_row],
		displayed_col_order[array_row], MAX_COLUMNS);
	gint *width_array = col_width_store[array_row];
	gint *order_array = displayed_col_order[array_row];
	//element of cache-name strings
	gchar *panename = (view == &app.pane1.view) ? app.pane1.name : app.pane2.name;

	gchar *fontstr = (e2_option_bool_get ("custom-list-font")) ?
		e2_option_str_get ("list-font") : NULL;	//NULL will cause default font
	/*now create tree view
	column order is determined by array displayed_col_order[].
	columns are named "0" to "7" for later searching */
	gchar id[] = {'0', '\0'};  //for column header titles, used to check which column

	//unfiltered model needed for getting the sortable
	GtkTreeModel *mdl = GTK_TREE_MODEL (view->store);
	view->model = gtk_tree_model_filter_new (mdl, NULL);
	GtkWidget *tvw = gtk_tree_view_new_with_model (view->model);
	g_object_unref (G_OBJECT (view->store));	//kill the ref from model creation
	g_object_unref (G_OBJECT (view->model));	//kill the ref from treeview creation
	view->filtered_before = FALSE;	//start with fresh filters
	//save it for others to use
	view->treeview = tvw;

	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (view->store);
	//allow unsorted display using GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
//	gtk_tree_sortable_set_default_sort_func (sortable, NULL, NULL, NULL);

	//set general treeview properties
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (tvw), TRUE);
	gtk_tree_view_set_rules_hint (GTK_TREE_VIEW (tvw),	//useless with managed backcolors
		e2_option_bool_get ("panes-hinted"));
//#ifdef USE_GTK2_10
//FIXME use this if it's as functional as E2_ALTLEFTMOUSE, seems not so
//	gtk_tree_view_set_rubber_banding (GTK_TREE_VIEW (tvw), TRUE);	//"drag-selection"
//#endif
	//cache the view's selection pointer CHECKME maybe a problem with filtermodel
	GtkTreeSelection *sel = view->selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (tvw));
	gtk_tree_selection_set_mode (sel, GTK_SELECTION_MULTIPLE);
#ifdef E2_SELTXT_RECOLOR
	if (e2_option_bool_get ("selectedtext-colorchange"))
		gtk_tree_selection_set_select_function (sel,
			(void *) _e2_fileview_selection_func, view, NULL);
#endif
/*	//when both panes are "same", make the slave-pane columns un-draggable
	gboolean fixed_order = (
		(e2_option_bool_get ("pane1-uses-other") && view == app.pane1.view)
		||
		(e2_option_bool_get ("pane2-uses-other") && view == app.pane2.view) );
*/
	gint i;
	//set columns' properties
	for (i = 0; i < MAX_COLUMNS; i++)
	{
		gint m;
		guint show_this;
		gchar *option_name;
		E2_OptionSet *set;
		GtkWidget *headerbox, *label;
		GtkArrowType arrow;
		GtkCellRenderer *renderer;
		GtkTreeViewColumn *column;

		m = order_array[i];  //get the model/'external' index
		gtk_tree_sortable_set_sort_func (sortable, m,
			e2_all_columns[m].sort_func, &view->sort_order, NULL);

		//get the column visibility
		option_name = g_strdup_printf ("%s-show-column%d", panename, m);
		// e2_option_bool_get  defaults to FALSE
		set = e2_option_get (option_name);

		show_this = (set != NULL && e2_option_bool_get_direct (set)) ? 1:0;
		g_free (option_name);

		//create header widget, to support styling and avoid gtk bug with arrows
#ifdef USE_GTK3_0
		headerbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 1);
#else
		headerbox = gtk_hbox_new (FALSE, 1);
#endif
		label = gtk_label_new (gettext(e2_all_columns[m].title)); //same title for each pane
		//expand = TRUE prevents col width < title width + sort indicator (if any)
		//CHECKME padding = 0 when label expand = FALSE (or else it crashes when col is too narrow to show it all)
		gtk_box_pack_start (GTK_BOX(headerbox), label, FALSE, FALSE, 0);

		gtk_widget_show_all (headerbox); //keep the arrow hidden for now

		if (m == FILENAME)
			//remember the filename column label in case we need to use it for active-pane flagging
			view->name_label = GTK_LABEL (label);
		//set the current sort arrow
		if (m == view->sort_column
				&& view->sort_order == GTK_SORT_DESCENDING)
			arrow = GTK_ARROW_UP;
		else
			arrow = GTK_ARROW_DOWN;
		view->sort_arrows[m] = gtk_arrow_new (arrow, GTK_SHADOW_IN);
		//this doesn't show the indicator at the end of the header ! (and different exp, fill don't help)
		gtk_box_pack_end (GTK_BOX(headerbox), view->sort_arrows[m], FALSE, FALSE, 0);

		renderer = gtk_cell_renderer_text_new ();
		g_object_set (G_OBJECT (renderer),
			"yalign", 0.0,
			"ypad", 0, //supposedly default, but this makes the rows closer
			"font", fontstr,	//this property not stored in model, as it doesn't change on the fly ...
			NULL);
//		gtk_cell_renderer_text_set_fixed_height_from_font
//			(GTK_CELL_RENDERER_TEXT (renderer), 1);
		//right-align the size column only
		if (m == SIZE)
			g_object_set (G_OBJECT (renderer),
				"xalign", 1.0,
				"xpad", 0,
				NULL);

  		id[0] = m+'0';
		column = gtk_tree_view_column_new_with_attributes (
			id, //this is for seaching & sorting, displayed title is in widget label
			renderer,
			"text", m,
			"foreground-gdk", FORECOLOR,
			"cell-background-gdk", BACKCOLOR,	//this column is generally NULL'd, except for drop-target highlighting
			NULL);

		gint thiswidth = width_array[m];
		//doesn't like 0 widths set by gtk when a col is hidden
		if (thiswidth == 0)
			thiswidth = e2_all_columns[m].size;

		/* It would be nice to allocate spare treeview space to the filename
			column (TRUE "expand" property) instead of the last-shown column.
			However we then also need to manage the column's "min-width" property
			so that the column isn't the first to be resized out of existence.
			Sadly there's no API for detecting only manual (dragged) column-width
			changes, or other reasonable work-around that we can find, to allow
			the minimum to be dynamically revised in accord with user's changes */

		g_object_set (column,
			"sizing", GTK_TREE_VIEW_COLUMN_FIXED,
			"resizable", TRUE,
			"fixed-width", thiswidth,
			"visible", show_this,
			"clickable", TRUE,
			"reorderable", TRUE,
			"widget", headerbox,
			NULL);

		gtk_tree_view_append_column (GTK_TREE_VIEW (tvw), column);

#ifdef EDIT_INPLACE
		if (m == FILENAME)
		{	//setup for in-cell name-changing
			g_object_set (G_OBJECT (renderer), "editable", TRUE, NULL);
			g_signal_connect (G_OBJECT (renderer), "edited",
				G_CALLBACK (_e2_fileview_name_edited_cb), view);
			g_signal_connect (G_OBJECT (renderer), "editing-started",
				G_CALLBACK (_e2_fileview_edit_start_cb), view);
			g_signal_connect (G_OBJECT (renderer), "editing-canceled",
				G_CALLBACK (_e2_fileview_edit_cancel_cb), view);
		}
#endif
		//workaround for distinguishing header clicks from drags
		//uses 'private' button, cuz (headerbox with event-box) doesn't work
#ifdef USE_GTK2_14
		GtkWidget *button = gtk_widget_get_ancestor (headerbox, GTK_TYPE_BUTTON);
# ifdef DEBUG_MESSAGES
		if (button == NULL)
			printd (ERROR, "missing column-header button");
		else
		{
# endif
		g_signal_connect (G_OBJECT (button), "button-press-event",
			G_CALLBACK (_e2_fileview_column_header_buttonpress_cb), view);
		g_signal_connect (G_OBJECT (button), "button-release-event",
			G_CALLBACK (_e2_fileview_column_header_buttonrel_cb), view);
# ifdef DEBUG_MESSAGES
		}
# endif
#else
		g_signal_connect (G_OBJECT (column->button), "button-press-event",
			G_CALLBACK (_e2_fileview_column_header_buttonpress_cb), view);
		g_signal_connect (G_OBJECT (column->button), "button-release-event",
			G_CALLBACK (_e2_fileview_column_header_buttonrel_cb), view);
#endif
		g_signal_connect (G_OBJECT (column), "clicked",
			G_CALLBACK (_e2_fileview_column_header_clicked_cb), view);
	}
	//setup inital sort parameters, ascending name col
//	view->sort_order = GTK_SORT_ASCENDING;
//	gtk_widget_show (view->sort_arrows[0]);
	//inital sort parameters set from cache
	gtk_tree_sortable_set_sort_column_id (sortable,
		view->sort_column, view->sort_order);
	gtk_widget_show (view->sort_arrows[view->sort_column]);

#ifndef E2_MOUSECUSTOM
	//FIXME do this once, not for both panes
	button2updir = e2_option_bool_get ("button2-updir");
#endif
	g_signal_connect (tvw, "columns-changed",
		G_CALLBACK(_e2_fileview_col_change_cb), view);

	//by default, type-ahead searching is enabled on column 0
	//disable default search if vim etc bindings used, (then need <Ctrl>f to start search) ?
	//gtk_tree_view_set_enable_search (GTK_TREE_VIEW (tvw), FALSE);
	gtk_tree_view_set_search_equal_func	(GTK_TREE_VIEW (tvw),
		(GtkTreeViewSearchEqualFunc)_e2_fileview_match_filename, view, NULL);
	//DnD connections
//	gtk_drag_source_set (tvw, GDK_BUTTON1_MASK, target_table, n_targets,	//-1 if XDS is last,
//		GDK_ACTION_COPY); //can't use 2 of this fn, it seems
	gtk_drag_source_set (tvw, GDK_BUTTON1_MASK | GDK_BUTTON2_MASK,
		target_table, n_targets,	//-1, //the last target (XDS) is supported for dest only
		GDK_ACTION_COPY | GDK_ACTION_MOVE | GDK_ACTION_LINK | GDK_ACTION_ASK);
	gtk_drag_dest_set (tvw, GTK_DEST_DEFAULT_MOTION | GTK_DEST_DEFAULT_DROP,
		target_table, n_targets,
		GDK_ACTION_COPY | GDK_ACTION_MOVE | GDK_ACTION_LINK | GDK_ACTION_ASK);

	g_signal_connect (G_OBJECT (tvw), "drag-begin",
		G_CALLBACK (e2_dnd_drag_begin_cb), view);
	g_signal_connect (G_OBJECT (tvw), "drag-data-get",
		G_CALLBACK (e2_dnd_drag_data_get_cb), view);
	g_signal_connect (G_OBJECT (tvw), "drag-motion",
		G_CALLBACK (e2_dnd_drag_motion_cb), view);
	g_signal_connect (G_OBJECT (tvw), "drag-leave",
		G_CALLBACK (e2_dnd_drag_leave_cb), view);
//needed if GTK_DEST_DEFAULT_DROP not set, above
//	g_signal_connect (G_OBJECT (tvw), "drag-drop",
//		 G_CALLBACK(e2_dnd_drag_drop_cb), view);
	g_signal_connect (G_OBJECT (tvw), "drag-data-received",
		 G_CALLBACK(e2_dnd_drag_data_received_cb), view);
	//FIXME do these once only
//	atom_text_uri_list = gdk_atom_intern (target_table[0,0], FALSE);
//	atom_text_plain = gdk_atom_intern (target_table[1,0], FALSE);
//	atom_XdndDirectSave0 = gdk_atom_intern (target_table[2,0], FALSE);

//	g_signal_connect (G_OBJECT (tvw), "drag-data-delete",
//		G_CALLBACK (e2_dnd_drag_delete_cb), view);

	//for E2_ALTLEFTMOUSE, double-clicks are handled in the button-press callback
	//BUT gtk issues this signal when <Enter> is pressed during a type-ahead
	//search, which we want
	g_signal_connect (tvw, "row-activated",
		G_CALLBACK (_e2_fileview_row_activated_cb), view);
	g_signal_connect (G_OBJECT (tvw), "key-press-event",
		G_CALLBACK (_e2_fileview_key_press_cb), view);
#ifdef E2_MOUSECUSTOM
	e2_fileview_register_pointerbindings (view->treeview);
#endif
	//these signals are used whether or not E2_MOUSECUSTOM applies
	//NOTE also emitted when a column-header-separator is pressed/released
	g_signal_connect (G_OBJECT (tvw), "button-press-event",
		G_CALLBACK (_e2_fileview_button_press_cb), view);
	g_signal_connect (G_OBJECT (tvw), "button-release-event",
		G_CALLBACK (_e2_fileview_button_release_cb), view);
	g_signal_connect (G_OBJECT (tvw), "cursor-changed",
		G_CALLBACK (_e2_fileview_cursor_change_cb), view);

	GtkWidget *sw = e2_widget_get_sw (GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC,
		GTK_SHADOW_ETCHED_IN);
	gtk_container_add (GTK_CONTAINER (sw), tvw);

	gtk_widget_show (tvw);
	gtk_widget_show (sw);

	return sw;
}
