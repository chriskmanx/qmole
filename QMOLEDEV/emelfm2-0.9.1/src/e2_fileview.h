/* $Id: e2_fileview.h 3018 2014-01-21 21:50:09Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

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

#ifndef __E2_FILEVIEW_H__
#define __E2_FILEVIEW_H__

#include "emelfm2.h"

//these are in same order as ascii <, =, >
typedef enum { LT, EQ, GT } Operator;
typedef enum { E2_REFRESH, E2_RECREATE, E2_CHANGE } E2_RefreshType;
typedef enum { CD_NOTFINISHED, CD_TIMEOUT, CD_SUCCESS } E2_CDType;
//filelist treestore column-number enumerator
enum { FILENAME = 0,
#ifdef E2_EXTCOL
	EXTN,
#endif
	SIZE, PERM, OWNER, GROUP, MODIFIED, ACCESSED, CHANGED, MAX_COLUMNS };
#define EXTENSION -1 //enum for sort action

//directory-read failure codes
enum
{
	E2DREAD_ENOENT,	//empty dir (not really an error)
	E2DREAD_NS,		//stat dir failed
	E2DREAD_DNR,	//dir not opened
	E2DREAD_DNCH,	//cannot cd into dir to poll its contents
	E2DREAD_DNF,	//read cancelled by user after timeout
	E2DREAD_ENOTH,	//failed to create thread when doing timed read
	E2DREAD_ENOMEM,	//not enough memory for storing data about read item
	E2DREAD_ELAST	//just an enumerator
};
#define E2DREAD_FAILED(ptr) (!(ptr == NULL || (gpointer)ptr > GUINT_TO_POINTER (E2DREAD_ENOMEM)))

//this is here to workaround build circularity
typedef enum
{
	E2FSCASE_UNKNOWN,
	E2FSCASE_SENSITIVE,
	E2FSCASE_ANY,	//insensitive
	E2FSCASE_NEVER,	//tried to discover, but unable to do so
	E2FSCASE_VIRTUAL,
	E2FSCASE_NOCACHE = 16,	//a bitfag set for removable devices
	E2FSCASE_SENSITIVENOW,	//values corresponding to fixed devices
	E2FSCASE_ANYNOW,
	E2FSCASE_NOTNOW,
	E2FSCASE_VIRTUALNOW
} E2_FSSensitive;

typedef struct _E2_Listman
{
	E2_RefreshType refreshtype; //whether a view update is a refresh, recreate or cd
	struct _E2_ViewInfo *view;	//use this when re-starting a cd after timeout
	gchar *newpath;	//stored copy of place to open, non-NULL implies cd requested
	gboolean history; //TRUE to update opened-dirs history after the cd is completed
	gboolean hook;	//TRUE to run cd hooklist after the cd is completed
	VOLATILE gboolean cd_requested;	//TRUE when we can't yet cd using this data
	VOLATILE gint cd_working;	//1 to prevent re-entrant cd of the same filelist
//	guint timer;	//id of timer running a cd process

	VOLATILE gint refresh_requested;	//1 when something has proposed that the view be refreshed
	VOLATILE gint refresh_working;	//1 when a view-refresh is in progress
	VOLATILE gint refresh_refcount;	//>0 when refresh for this pane is disabled (though backend checks are not)
} E2_Listman;

typedef struct _E2_ViewInfo
{
	GtkListStore *store;
	GtkTreeModel *model;	//may be a filter model or standard
	GtkWidget *treeview;
	GtkTreeSelection *selection;	//pointer to selection for this view's treeview
	gint sort_column;	//copy of current sort column, can be <0
	gboolean extsort;	//TRUE when name-sorting is by extension
	GtkSortType sort_order; //copy of current sort order
	GtkWidget *sort_arrows[MAX_COLUMNS];
	GtkLabel *name_label;	//for showing active pane by changing label content
//	E2_FSType dirtype;	//flags for type of namespace this view belongs to
	E2_FSSensitive case_sensitive_names; //cache, FIXME needs to be cleared when removable device changes
#ifdef E2_VFS
	VOLATILE PlaceInfo *spacedata;	//pointer to data in relevant member of vpaths cache, NULL for mounted dir
//	vpath vdir;		//pointer to data in relevant member of vpaths cache
#endif
	gchar dir[PATH_MAX];	//path shown in this view (UTF-8, with trailing "/")
							//typically same as corresponding (freeable) rt->path,
							// ? but with easier access for regular usage
//	GList *dir_history; //list of structs with focused line etc for each opened dir in the current namespace
						//data are shared with app.dir_history
//#ifndef E2_FAM
	time_t dir_mtime;
	time_t dir_ctime;
//#endif
	GtkWidget *check_dirs; //filter sub-menu toggle-widget
	GtkWidget *check_name; //filter sub-menu toggle-widget
	struct
	{
		gchar *patternptr;
		GSList *compiled_patterns;	//list of _E2_SelectPattern's
		gboolean invert_mask;
		gboolean case_sensitive;
		gboolean active;
	} name_filter;
	GtkWidget *check_size;
	struct
	{
		size_t size;
		Operator op;
		gboolean active;
	} size_filter;
	GtkWidget *check_date;
	struct
	{
		time_t time;	//time_t may be int or long int
		Operator op;
		enum {MTIME, ATIME, CTIME} time_type;
		gboolean active;
	} date_filter;

	gboolean filtered_before;	//state of prior call to filter function
	gboolean filter_directories;
	guint filtercount;	//bumped at each re-filter, for e.g. thumbs dialog, to poll for filter-changes
	gboolean show_hidden;  //whether to display hidden items in this view
	guint total_items;	//total displayed and undisplayed items in the dir - 2 (to ignore . and ..)
	gint row;			//0-based index of most-recently clicked row in the treeview
	gint drop_row;		//0-based index of most-recently moused row during a drag operation
	gboolean lit;		//TRUE if drop-row is highlighted
	gboolean convert;	//TRUE when any item's name has non-ASCII char(s), so conversion is needed for all
	E2_Listman listcontrols;
	gchar last_find[NAME_MAX]; //buffer for storing names used in find task
	GHookList hook_refresh;	//data for functions to run during post-refresh idle-time function
} ViewInfo;

typedef struct _E2_FileInfo
{
	gchar filename[NAME_MAX+1];  //item basename, localised string
#ifdef E2_VFS
	//in a collection, each item can have any path
	vpath *vdir; //pointer to hashed dir data for this item, or NULL if view->dir applies
				 //path string is localised, with no trailing separator
#endif
	struct stat statbuf;	//sometimes-customised form of fs statbuf
} FileInfo;

//enable automatic logging of selected items, and re-selection
//#define TAG_ALL

typedef struct _E2_DirHistoryEntry
{
	gchar path[PATH_MAX];	//must be at start of stuct (struct ptr sometimes used for lists of strings)
#ifdef E2_VFS
	PlaceInfo *spacedata;	//pointer to data in relevant member of vpaths cache
#endif
	gchar firstname [NAME_MAX+1]; //copy of info->filename for 1st selected item
	gint selrow;	//liststore index of 1st selected row, -1 if no selection
	gint toprow;	//liststore index of top visible row
	E2_FSSensitive case_sensitive_names;
	GHashTable *selitems; //for use by tag-plugin or with TAG_ALL
} E2_DirHistoryEntry;

typedef struct _E2_SelectedItemInfo
{
	gchar filename[NAME_MAX+1];  //localised string no trailing /
/*	FileInfo info;
	gchar *filename;  //selected item name (except no trailing /), utf-8 string
#ifdef E2_INCLIST
	//use this if the store data is changed on the fly
	GtkTreeRowReference *ref;
#else
	//this is used for un-selecting liststore items on the fly
	GtkTreePath *path;
#endif
*/
} E2_SelectedItemInfo;

typedef struct _E2_CDwatch
{
	ViewInfo *view;	//data for view being changed (can't use E2_PaneRuntime due to circular includes)
	gchar *newpath;	//place to be opened, utf8 with trailing separator
	gint repeats;	//current retry counter
//	guint watchtimer_id; //id of timer that is watching for completion
	E2_CDType *completed_flag;	//store for flag to set when completion detected
} E2_CDwatch;

typedef struct _E2_DRead
{
	pthread_t aid;	//read thread ID, 0 when stopped
	pthread_t mid;	//monitor thread ID, 0 when stopped
	GtkWidget *dialog;	//too-slow dialog widget, or NULL
} E2_DRead;

typedef struct _E2_SelectPattern
{
	GPatternSpec *pspec;
	gboolean negated;
} E2_SelectPattern;

#define WAIT_FOR_REFRESH(view) \
	while (TRUE) \
	{ \
		gboolean __busy; \
		__busy = g_atomic_int_get (&view->listcontrols.refresh_working) \
			  || g_atomic_int_get (&view->listcontrols.cd_working); \
		if (!__busy) \
			break; \
		usleep (100000); \
	}

gint e2_fileview_ext_sort (GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b,
	GtkSortType *direction);
gboolean e2_fileview_sort_column (gint colnum, ViewInfo *view);
void e2_fileview_refilter_list (ViewInfo *view);
void e2_fileview_filter_dirs_cb (GtkCheckMenuItem *widget, ViewInfo *view);
void e2_fileview_remove_filters_cb (GtkMenuItem *widget, ViewInfo *view);
void e2_fileview_initialize_filters (ViewInfo *view);

void e2_fileview_set_menu_position (GtkMenu *menu,
	gint *x, gint *y, gboolean *push_in, GtkWidget *treeview);
void e2_fileview_focus_row (ViewInfo *view, gint row,
	gboolean select_row, gboolean clear_selection,
	gboolean center, gboolean grab_focus);
//void e2_fileview_select_row_by_filename (ViewInfo *view, gchar *filename); unused
GHashTable *e2_fileview_log_selected_names (ViewInfo *view);
void e2_fileview_reselect_names (ViewInfo *view, GHashTable *selnames, gboolean clean);
void e2_fileview_adjust_name (ViewInfo *view, const gchar *oldname,
	const gchar *newname, const gchar *oldutf, const gchar *newutf);
void e2_fileview_translate_cols_array (gint *src_array,
	gint *dest_array, gint size);
void e2_fileview_switch_views (void);
//void e2_fileview_switch_views_simple (void);
void e2_fileview_set_font (void);
void e2_fileview_set_row_background (ViewInfo *view,
	GtkTreeIter *iter, GdkColor *color);
void e2_fileview_clear_row_background (ViewInfo *view,
	GtkTreeIter *iter);
void e2_fileview_update_col_cachedata (void);
gboolean e2_fileview_is_cased (ViewInfo *view);
void e2_fileview_get_scroll_data (ViewInfo *view, gint *leftcol, gint *toprow);
gboolean e2_fileview_scroll_to_position (ViewInfo *view, gint leftcol, gint toprow);
//gint e2_fileview_find_name_col (ViewInfo *view); unused
gboolean e2_fileview_prepare_list (ViewInfo *view);
void e2_fileview_scroll_list (GThread *viewthread);
gboolean e2_fileview_cd_watch (E2_CDwatch *data);
gboolean e2_fileview_cd_manage (E2_Listman *data);
GtkWidget *e2_fileview_create_list (ViewInfo *view) G_GNUC_MALLOC;
void e2_fileview_select_all (GtkWidget *widget, ViewInfo *view);
void e2_fileview_clean_selected (GPtrArray *selected);
void e2_fileview_clean1_history (gpointer data);
void e2_fileview_clear_filter_patterns (ViewInfo *view);
GPtrArray *e2_fileview_get_selected (ViewInfo *view) G_GNUC_MALLOC;
GList *e2_fileview_get_selected_local (ViewInfo *view) G_GNUC_MALLOC;
FileInfo *e2_fileview_get_selected_first_local (ViewInfo *view, gboolean updir);
gchar *e2_fileview_get_row_name (ViewInfo *view, gint row) G_GNUC_MALLOC;
void e2_fileview_register_keybindings (GtkWidget *treeview, ViewInfo *view);
#ifdef E2_MOUSECUSTOM
void e2_fileview_register_pointerbindings (GtkWidget *treeview);
#endif

#endif //ndef __E2_FILEVIEW_H__
