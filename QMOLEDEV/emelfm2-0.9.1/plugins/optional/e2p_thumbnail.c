/* $Id: e2p_thumbnail.c 2909 2013-11-06 04:39:14Z tpgww $

Copyright (C) 2007-2013 tooar <tooar@emelfm2.net>

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

/**
@file plugins/optional/e2p_thumbs.c
@brief image-viewer plugin
*/

/*TODO
selection transfers TO iconview? NO NEED
local sort funcs and icons-liststore rationalisation
bias initial rendering order to visible rectangle ?
bias re-rendering order to visible rectangle NO NEED
warn user about plugin-load failure ?
block plugin reload (old lib)
confirm efficient cleanups (how to look into pixbufs?)
efficient pixbuf saving ok?
DnD ?
*/

#include "emelfm2.h"
#include <string.h>

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "thumbnail"

#ifdef E2_THUMBLIB //from Makefile via build.h
//work with shared lib
# define THUMB_OBJECT
//setup relevant #defines in included library headers
# define GIMP_THUMB_DISABLE_DEPRECATED
# include <libthumbs/gimpthumb.h>
#else
# include "gimpthumb.c"
#endif
#include "e2_context_menu.h"
#include "e2_filelist.h"
#include "e2_task.h"
#include "e2_filetype.h"
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_icons.h"

#define E2_SOFTWARE_ID PROGNAME" file manager"
//#define E2_SCALEUP_SIZE 32

/*dialog data listore columns enumerator ...
 so that the same sort functions can be used for this store and the filelist
 store, we need to provide columns up to FINFO in this store, but provide
 different or non-uses for many of them */
enum
{
// FILENAME = 0 ... FINFO are enumerated in e2_fileview.h
// THUMBNAIL maybe unused
	PIXBUF = 1, THUMBNAIL, THUMBCOLCOUNT = FINFO+1
};

typedef struct _E2_ThumbDialogRuntime
{
	GtkWidget *dialog;		//the displayed dialog
	GtkWidget *iconview;	//images iconview
	GtkListStore *store;	//images data store
	GtkWidget *sortbtn;		//action-area sort-button
	ViewInfo *view;			//data for displayed file pane
	E2_OptionSet *clampset;	//holds value TRUE to clamp image display into range 32..128 px
	gboolean replicate;		//TRUE to cause iconview selections to be replicated in related treeview
	gint sort_type;			//to determine the correct sort func, corresponds to column in original store
	gint filtercount;		//for matching whether parent filelist has been re-filtered
	GtkSortType sort_order;
	gint blocked;			//0 when no refresh in progress, prevents recursive refreshing
	guint timer_id;			//id of refresh-deferral timer
#ifdef E2_VFSTMP
	//FIXME path when dir not mounted local
#else
	gchar *path;			//path of dir currently displayed, same format as view->dir
#endif
	GSList *oldstores;
} E2_ThumbDialogRuntime;

static PluginIface iface;

//active E2_ThumbDialogRuntime's to be cleaned up when plugin is unloaded
static GSList *thumbslist = NULL;

static GimpThumbConnection *handle;

//static gboolean show_hidden = FALSE;	//session-static value to use in dialogs
//static assuming last-closed window sets size for next one in this session only
static gint window_width = -1;
static gint window_height = -1;
static const gchar *aname;

#ifdef E2_TRANSIENTBINDINGS
#include "e2_keybinding.h"
static void _e2p_thumbs_keybindings (E2_OptionSet *set);
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
static void _e2p_thumbs_mousebindings (E2_OptionSet *set);
# ifdef E2_PTRGESTURES
static void	_e2p_thumbs_mousegestures (E2_OptionSet *set);
# endif
#endif
#endif
static void _e2p_thumbs_selection_change_cb (GtkIconView *iconview,
	E2_ThumbDialogRuntime *rt);
static void _e2p_thumbs_response_cb (GtkDialog *dialog, gint response,
	E2_ThumbDialogRuntime *rt);

  /*********************/
 /***** utilities *****/
/*********************/

/**
@brief create empty liststore to hold iconview data

@return the new store
*/
static GtkListStore *_e2p_thumbs_make_store (void)
{
 /* Most columns are for support data and are not displayed.
	FILENAME, NAMEKEY and FINFO are in the same positions as for the
	filelist stores, to allow the same sort functions to be used */
	GtkListStore *store = gtk_list_store_new (THUMBCOLCOUNT,
		G_TYPE_STRING,  //FILENAME displayed
		GDK_TYPE_PIXBUF, //PIXBUF displayed in place of SIZE
#if 0 //thumbnail data is needed
# ifdef THUMB_OBJECT
		GIMP_TYPE_THUMBNAIL, //THUMBNAIL in place of PERM
# else
		G_TYPE_POINTER, //THUMBNAIL in place of PERM
# endif
#else
		G_TYPE_POINTER, //PERM unused
#endif
#ifdef E2_EXTCOL
		G_TYPE_POINTER,	//unused column, placed here to preserve PIXBUF and FINFO values
#endif
		G_TYPE_POINTER, //OWNER unused
		G_TYPE_POINTER, //GROUP unused
		G_TYPE_POINTER, //MODIFIED unused
		G_TYPE_POINTER, //ACCESSED unused
		G_TYPE_POINTER, //CHANGED unused
		G_TYPE_STRING,  //NAMEKEY for i18n name sorts
		G_TYPE_POINTER  //FINFO pr to FileInfo for the item
		);
	return store;
}
/**
@brief empty and destroy list store

@param store pointer to the liststore to be killed

@return
*/
static void _e2p_thumbs_clear_store (GtkListStore *store)
{
	printd (DEBUG, "_e2p_thumbs_clear_store");
	GtkTreeModel *mdl = GTK_TREE_MODEL (store);	//FIXME BAD model warning
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (mdl, &iter))
	{	//it's not empty already
		//clear data in the store
		//CHECKME need to clear anything else?
		FileInfo *info;
		GdkPixbuf *pxb;
#if 0	//thumbnail data is needed
		GimpThumbnail *thumbnail;
#endif
		do
		{
			gtk_tree_model_get (mdl, &iter, PIXBUF, &pxb,
#if 0
			THUMBNAIL, &thumbnail,
#endif
			FINFO, &info, -1);
			DEALLOCATE (FileInfo, info);
//			if (pxb != NULL)
				g_object_unref (G_OBJECT (pxb));
			//CHECKME cache the pixbufs here instead of when filling the store
#if 0
			if (thumbnail != NULL)
# ifdef THUMB_OBJECT
				g_object_unref (G_OBJECT (thumbnail));
# else
				gimp_thumbnail_destroy (thumbnail);
# endif
#endif
		} while (gtk_tree_model_iter_next (mdl, &iter));
		gtk_list_store_clear (store);	//NEEDED ??
	}
	g_object_unref (G_OBJECT (store));
}
/**
@brief idle function to clear and eliminate old liststores

@param oldstores list of superseded liststores to be cleaned

@return FALSE, to stop the callbacks
*/
static gboolean _e2p_thumbs_clear_old_stores (GSList *oldstores)
{
#ifdef DEBUG_MESSAGES
	gint debug = g_slist_length (oldstores);
#endif
	GSList *member;
	for (member = oldstores; member != NULL; member = member->next)
	{
		_e2p_thumbs_clear_store ((GtkListStore *)member->data);
	}
	g_slist_free (oldstores);
	printd (DEBUG, "%d old images-liststore(s) cleared", debug);
	return FALSE;
}
/**
@brief create, populate and apply an iconview-compatible liststore
Rows are added for each relevant non-dir item in the corresponding filelist's
liststore.
The existing liststore is replaced, and queued for cleanup.
Expects BGL open/off
@param rt pointer to data struct for dialog

@return pointer to the liststore, or NULL if there's a problem
*/
static GtkListStore *_e2p_thumbs_replace_store (E2_ThumbDialogRuntime *rt)
{
	if (!g_atomic_int_compare_and_exchange (&rt->blocked, 0, 1)) //prevent re-entrance
		return NULL;
//	printd (DEBUG, "start store fill");
	GtkListStore *store = _e2p_thumbs_make_store ();
	if (store == NULL)
	{
		g_atomic_int_add (&rt->blocked, -1);
		return NULL;
	}

	E2_ListChoice pane = (rt->view == &app.pane1.view) ? PANE1 : PANE2;
	e2_filelist_disable_one_refresh (pane);

	//transfer data from filelist store to iconview store
	GtkTreeIter itert;
	GtkTreeModel *mdlt = rt->view->model;	//the filter-model

	if (gtk_tree_model_get_iter_first (mdlt, &itert))
	{
		FileInfo *info;
		GdkPixbuf *pxb;
		GimpThumbnail* thumbnail;
		GError *error;
		gboolean makepxb;
		gchar *filename, *namekey, *dlocal, *localpath;
		GtkTreeIter iteri;
		gboolean doscale = e2_option_bool_get_direct (rt->clampset);
		GimpThumbSize threshold = (doscale) ? e2_option_int_get ("thumb-limit") : 0;

		GtkTreeSortable *sortablei = GTK_TREE_SORTABLE (store);

		//limit FAM to prevent 'self-perpetuation' of refreshing
		e2_fs_FAM_less_monitor_dir (rt->view->dir);

		CLOSEBGL
		e2_dialog_set_cursor (rt->dialog, GDK_WATCH);

		//make new model unsorted to speed up addition
		gtk_tree_sortable_set_sort_column_id (sortablei,
			GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
		OPENBGL

		do
		{
			gtk_tree_model_get (mdlt, &itert,
				FILENAME, &filename,
				NAMEKEY, &namekey,
				FINFO, &info,	//FIXME handle refreshing which changes the address of info's
				-1);
			//do a bit of filtering to reduce irrelevant pixbuf attempts ...
			switch (info->statbuf.st_mode & S_IFMT)
			{
				case S_IFLNK:
/*			//no special treatment needed for links
#ifdef E2_VFSTMP
		//FIXME vfs
#else
					dlocal = D_FILENAME_TO_LOCALE (rt->view->dir); //always dup, to avoid dirchange race
#endif
					localpath = e2_utils_strcat (dlocal, info->filename);
					len = strlen (localpath) - sizeof(gchar);
					if (len > 0 && localpath[len] == G_DIR_SEPARATOR)
						localpath[len] = '\0';
			FIXME use e2_fs_walk_link ();
					len = e2_fs_readlink (localpath, target, sizeof (target));
					if (len <= 0)
					{
						g_free (dlocal);
						g_free (localpath);
						break;
					}
					//check the real target, looking thru chained links
					if (e2_fs_stat (target, &statbuf2 E2_ERR_NONE())	//stat failed
						|| !S_ISREG(statbuf2.st_mode))
					{
						g_free (dlocal);
						g_free (localpath);
						break;
					}
					makepxb = TRUE;
					break;
*/
				case S_IFREG:
#ifdef E2_VFSTMP
		//FIXME vfs
#else
					dlocal = D_FILENAME_TO_LOCALE (rt->view->dir); //always dup, to avoid dirchange race
#endif
					localpath = e2_utils_strcat (dlocal, info->filename);
					makepxb = TRUE;
					break;
				default:
					makepxb = FALSE;
					break;
			}

			if (makepxb)
			{
				GimpThumbState status;
				gint width, height;

				thumbnail = gimp_thumbnail_new ();
				error = NULL;
				if (!gimp_thumbnail_set_filename (thumbnail, localpath, &error))
				{
					if (error != NULL)
					{
						//FIXME warn the user
						g_error_free (error);
					}
					makepxb = FALSE;
				}

				if (makepxb)
				{
					if (doscale)
					{
						if (gdk_pixbuf_get_file_info (localpath, &width, &height) != NULL)
						{
							gboolean scaled;
							GimpThumbSize scaleto;
recreate:
/*							if (width < E2_SCALEUP_SIZE && height < E2_SCALEUP_SIZE)
							{
								scaled = TRUE;
								scaleto = E2_SCALEUP_SIZE;
							}
							else
*/
								if (width > threshold || height > threshold)
							{
								scaled = TRUE;
								scaleto = threshold;
							}
							else
							{
								scaled = FALSE;
								scaleto = MAX (width, height);
							}

							status = gimp_thumbnail_check_thumb (handle, thumbnail, scaleto); //ensure thumb is current
							if (status != GIMP_THUMB_STATE_OK)
							{
								if (scaled)
									pxb = gdk_pixbuf_new_from_file_at_scale (localpath,
										scaleto, scaleto, TRUE, &error);
								else
									pxb = gdk_pixbuf_new_from_file (localpath, &error);

								if (pxb != NULL)
								{
#ifdef DEBUG_MESSAGES
									if (scaled)
									{
										gint wide = gdk_pixbuf_get_width (pxb);
										printd (DEBUG, "width of scaled pixbuf %s is %u", localpath, wide);
										gint high = gdk_pixbuf_get_height (pxb);
										printd (DEBUG, "and width of scaled pixbuf %s is %u", localpath, high);
									}
#endif
//									local save >> into CWD/.thumblocal/normal, maybe not creatable ?! BUG?
//									if (!gimp_thumbnail_save_thumb_local (handle, thumbnail, pxb,
									//CHECKME save pixbuf later, so we can startup more quickly
									if (!gimp_thumbnail_save_thumb (handle, thumbnail, pxb,
										E2_SOFTWARE_ID, &error))
									{
										//warn the user
										if (error != NULL)
											g_error_free (error);
										g_object_unref (G_OBJECT (pxb));
										makepxb = FALSE;
									}
								}
								else	//pixbuf creation failed
								{
									//warn the user
									if (error != NULL)
										g_error_free (error);
									makepxb = FALSE;
								}
							}
							else	//a probably-valid cached image found
							{
								pxb = gimp_thumbnail_load_thumb (handle, thumbnail, scaleto, &error);
								if (pxb != NULL)
								{
									width = gdk_pixbuf_get_width (pxb);
									height = gdk_pixbuf_get_height (pxb);
									if (height > width)
										width = height;
									if (//width < E2_SCALEUP_SIZE ||
										(doscale && width > threshold)
									  || width != scaleto)
									{
										g_object_unref (G_OBJECT (pxb));
										height = width;
										goto recreate;
									}
								}
								else
								{
									//warn the user
									if (error != NULL)
										g_error_free (error);
									makepxb = FALSE;
								}
							}
						}
						else //no file info
						{
//							printd (DEBUG, "skipping %s, no pixbuf file info", localpath);
							//FIXME warn the user if it does seem to be an image
							makepxb = FALSE;
						}
					}
					else //un-scaled image wanted
					{
						pxb = gimp_thumbnail_load_thumb (handle, thumbnail,
														GIMP_THUMB_SIZE_LARGE, NULL); /* try for any size */
						if (pxb)
						{
							// check for unscaled
							if (G_LIKELY(gdk_pixbuf_get_file_info
								(thumbnail->thumb_filename, &width, &height) != NULL))
							{
								if (thumbnail->image_width == width && thumbnail->image_height == height)
									goto create;
							}
							g_object_unref (G_OBJECT (pxb));
						}

						error = NULL;
						pxb = gdk_pixbuf_new_from_file (localpath, &error);
						if (pxb == NULL)	// pixbuf creation failed
						{
							//warn the user
							if (error != NULL)
								g_error_free (error);
							makepxb = FALSE;
						}
						else if (G_LIKELY(gdk_pixbuf_get_file_info
								(localpath, &width, &height) != NULL))
						{
							if (width <= GIMP_THUMB_SIZE_LARGE && height <= GIMP_THUMB_SIZE_LARGE)
								gimp_thumbnail_save_thumb (handle, thumbnail, pxb,
										E2_SOFTWARE_ID, NULL);
#if 0 //thumbnail is needed
# ifdef THUMB_OBJECT
							//set non-standard thumbnail details
							g_object_set (G_OBJECT (thumbnail),
										  "image-state",      GIMP_THUMB_STATE_OK,
										  "image-filesize",   0,
										  "image-mtime",      0,
										  "image-mimetype",   NULL,
										  "image-width",      width,
										  "image-height",     height,
										  "image-type",       NULL,
										  "image-num-layers", 0,
										  "thumb-state",      GIMP_THUMB_STATE_FAILED, //?
										  NULL);
# else
							thumbnail->image_state = GIMP_THUMB_STATE_OK;
							thumbnail->image_filesize = 0;
							thumbnail->image_mtime = 0;
							thumbnail->image_mimetype = NULL;
							thumbnail->image_width = width;
							thumbnail->image_height = height;
							thumbnail->image_type = NULL;
							thumbnail->image_num_layers = 0;
							thumbnail->thumb_state = GIMP_THUMB_STATE_FAILED;
# endif
#endif //0
						}
					}
				}
create:
				if (makepxb)
				{
					//copy the FileInfo, in case a refresh clobbers the original
					FileInfo *info2 = ALLOCATE (FileInfo);
					CHECKALLOCATEDWARN (info2, return NULL;);
					*info2 = *info;
					//store the data
					gtk_list_store_insert_with_values (store, &iteri, -1,
						FILENAME, filename,
						PIXBUF, pxb,
#if 0 //thumbnail is needed
						THUMBNAIL, thumbnail,
#endif
						NAMEKEY, namekey,
						FINFO, info2,
						-1);
				}
#if 0 //thumbnail is needed
				else
#endif
					if (thumbnail != NULL)
#ifdef THUMB_OBJECT
					g_object_unref (G_OBJECT (thumbnail));
#else
					gimp_thumbnail_destroy (thumbnail);
#endif
				g_free (dlocal);
				g_free (localpath);
			}
			g_free (filename);
			g_free (namekey);
		} while (gtk_tree_model_iter_next (mdlt, &itert));

		//arrange to sort new store same as old one
		gtk_tree_sortable_set_sort_func (sortablei, FILENAME,
			e2_all_columns[rt->sort_type].sort_func, &rt->sort_order, NULL);
		//do the sort
		gtk_tree_sortable_set_sort_column_id (sortablei, FILENAME, rt->sort_order);

		g_object_ref (G_OBJECT (rt->store));	//preserve the store for proper cleanup in idle
		CLOSEBGL
		//connect view to new model
		gtk_icon_view_set_model (GTK_ICON_VIEW (rt->iconview), GTK_TREE_MODEL (store));

		e2_dialog_set_cursor (rt->dialog, GDK_LEFT_PTR);
		OPENBGL
		g_object_unref (G_OBJECT (store));	//kill the ref from model assignment

		//reinstate standard FAM
		e2_fs_FAM_more_monitor_dir (rt->view->dir);

		//arrange to cleanup old store, later
		//FIXME manage shared access to superseded-stores list
		rt->oldstores = g_slist_append (rt->oldstores, rt->store); //&& some test for cleanup not underway now)
		g_idle_add ((GSourceFunc) _e2p_thumbs_clear_old_stores, rt->oldstores);
		rt->store = store;
		rt->oldstores = NULL;	//start afresh
	}

//	rt->dir_mtime = rt->view->dir_mtime;	//update refresh guides
	rt->filtercount = rt->view->filtercount;
/*#ifdef E2_FAM
# ifdef E2_VFSTMP
	//FIXME
# else
#  ifdef E2_FAM_KERNEL
	//this is a bad hack to prevent extra refresh
	if (!ref)
		e2_fs_FAM_clean_reports (rt->view->dir);
#  endif
# endif
#endif */
	e2_filelist_enable_one_refresh (pane);

	g_atomic_int_set (&rt->blocked, 0);
	//	printd (DEBUG, "finish icons store fill");
	return store;
}
/**
@brief refresh icons-view liststore
The existing liststore is replaced, and queued for cleanup.
Expects BGL open/off
@param rt pointer to data struct for dialog

@return
*/
static void _e2p_thumbs_refresh_store (E2_ThumbDialogRuntime *rt)
{
	gchar *name;
	GtkTreePath *tp;
	GtkTreeModel *mdl = GTK_TREE_MODEL (rt->store);
	GtkTreeIter iter;
	GList *member, *selnames = NULL;
	GList *selpaths = gtk_icon_view_get_selected_items (GTK_ICON_VIEW (rt->iconview));
	if (g_list_length (selpaths) > 0)
	{
		//record currently-selected data so we can re-select in new store
		for (member = selpaths; member != NULL; member = member->next)
		{
			tp = (GtkTreePath *)member->data;
			gtk_tree_model_get_iter (mdl, &iter, tp);
			gtk_tree_model_get (mdl, &iter, FILENAME, &name, -1);
			selnames = g_list_append (selnames, name);
			gtk_tree_path_free (tp);
		}
		g_list_free (selpaths);
	}

	//disable dialog close-button while replacing the thumbnails liststore
	//Closing during this process, which may take a noticeable period, will likely
	//crash
	CLOSEBGL
	gtk_dialog_set_response_sensitive (GTK_DIALOG(rt->dialog), GTK_RESPONSE_CLOSE, FALSE);
	OPENBGL

	//update the store
	_e2p_thumbs_replace_store (rt);

	CLOSEBGL
	gtk_dialog_set_response_sensitive (GTK_DIALOG(rt->dialog), GTK_RESPONSE_CLOSE, TRUE);
	OPENBGL

	//reselect things FIXME do this smarter
	if (g_list_length (selnames) > 0)
	{
		mdl = GTK_TREE_MODEL (rt->store);
		if (gtk_tree_model_iter_n_children (mdl, NULL))
		{
			//reselect all paths where we can
			gtk_tree_model_get_iter_first (mdl, &iter);
			for (member = selnames; member != NULL; member = member->next)
			{
				name = (gchar *)member->data;
				if (e2_tree_find_iter_from_str_same (mdl, FILENAME, name, &iter))
				{
					tp = gtk_tree_model_get_path (mdl, &iter);
					CLOSEBGL
					gtk_icon_view_select_path (GTK_ICON_VIEW (rt->iconview), tp);
					OPENBGL
					gtk_tree_path_free (tp);
					gtk_tree_model_get_iter_first (mdl, &iter); //ready for next search
				}
				g_free (name);
			}
/*			//goto former position
			if (0) //FIXME
			{
				tp = (GtkTreePath *) selpaths->data;
				gtk_icon_view_scroll_to_path (GTK_ICON_VIEW (rt->iconview),
					tp, TRUE, 0.3, 0.5);
			} */
		}
		else
			g_list_foreach (selnames, (GFunc) g_free, NULL);
		g_list_free (selnames);
	}
}
/* *
@brief timer callback to do a store refresh if the dir looks dirty
@param rt pointer to dialog's data struct
@return TRUE unless the dialog is destroyed already
*/
/*static gboolean _e2p_thumbs_check_dirty (E2_ThumbDialogRuntime *rt)
{
	printd (DEBUG, "_e2p_thumbs_check_dirty");
	if (!GTK_IS_WIDGET (rt->dialog))
		return FALSE;
	static gboolean busy = FALSE;
	if (!busy)
	{
		//FIXME handle change of hidden/filter state: hooklist ?
#ifdef E2_VFSTMP
	//FIXME when dir is not mounted local
#endif
		if ((rt->dir_mtime < rt->view->dir_mtime || rt->filtercount != rt->view->filtercount)
			&& g_atomic_int_get (&rt->blocked) == 0
			&& strcmp (rt->path, rt->view->dir) == 0)
		{
			busy = TRUE;
			_e2p_thumbs_refresh_store (rt);
			busy = FALSE;
		}
	}
	return TRUE;
}
*/
/**
@brief timer callback to do a store refresh after blockage is removed
@param rt pointer to dialog's data struct
@return TRUE if the 'blockage' from the main-window process is still present, or a local busy flag is set
*/
static gboolean _e2p_thumbs_wait_to_refresh (E2_ThumbDialogRuntime *rt)
{
	if (GTK_IS_WIDGET (rt->dialog))
	{
		static gboolean working = FALSE;	//simple blocker instead of killing timer

		if (working
		 || g_atomic_int_get (&rt->blocked) //another blocker, set downstream
		 || g_atomic_int_get (&rt->view->listcontrols.refresh_working)
		 || g_atomic_int_get (&rt->view->listcontrols.cd_working))
			return TRUE; //wait more
		working = TRUE;
		printd (DEBUG, "deferred replication of icons liststore");
		_e2p_thumbs_refresh_store (rt);
		working = FALSE;
	}
	rt->timer_id = 0;
	return FALSE; //kill the timer
}
/**
@brief hook function to do a store refresh after filelist for @a view is refreshed
This is initiated from an idle-callback, so BGL is off
@param view UNUSED pointer to data struct for refreshed pane
@param rt pointer to dialog's data struct
@return TRUE unless the dialog is destroyed already
*/
static gboolean _e2p_thumbs_refresh_hook (ViewInfo *view,
	E2_ThumbDialogRuntime *rt)
{
	static gboolean working = FALSE;

	if (!GTK_IS_WIDGET (rt->dialog))
		return FALSE; //kill the callback
	//see API text for _e2_fileview_more_refresh() for advice about race-checks here
	if (g_atomic_int_get (&rt->view->listcontrols.refresh_working)
	 || g_atomic_int_get (&rt->view->listcontrols.cd_working))
	//something which will affect the display has started
		return TRUE;	//ignore this refresh, wait for the new process to finish
	if (working)
	{
		printd (DEBUG, "defer refresh until current refresh is finished");
		if (rt->timer_id == 0)
			rt->timer_id = g_timeout_add (300, (GSourceFunc)_e2p_thumbs_wait_to_refresh, rt);
	}
	else
	{
		working = TRUE;
		printd (DEBUG, "replicating REFRESH of icons liststore");
		_e2p_thumbs_refresh_store (rt);
		working = FALSE;
	}
	return TRUE;
}
/**
@brief hook function for app.paneX.hook_change_dir
This is initiated from cd thread, with BGL off/open
@param newpath UNUSED path of opened directory, utf-8 string
@param rt pointer to dialog's data struct
@return TRUE always
*/
static gboolean _e2p_thumbs_change_dir_hook (gchar *newpath, E2_ThumbDialogRuntime *rt)
{
	printd (DEBUG, "replicating CHANGE-DIR for icons liststore");
	//grab new path
	g_free (rt->path);
	rt->path = g_strdup (newpath);
	/* Hooklist is run towards end of _e2_fileview_change_dir() thread-func
	   In principle, another cd could have beein initiated while the current one
	   is being processed, but such can be ignored here */
	if (g_atomic_int_get (&rt->view->listcontrols.refresh_working)) //this is probably impossible here
	{
		printd (DEBUG, "defer cd until the current filelist refresh is finished");
		if (rt->timer_id == 0)
			rt->timer_id = g_timeout_add (300, (GSourceFunc)_e2p_thumbs_wait_to_refresh, rt);
	}
	else
		_e2p_thumbs_refresh_store (rt);

	return TRUE;
}
/**
@brief migrate iconview selection to associated filelist

@param rt pointer to dialog's data struct

@return TRUE if something was selected in the iconview
*/
static gboolean _e2p_thumbs_transfer_selection (E2_ThumbDialogRuntime *rt)
{
	gboolean retval = FALSE;
	GtkTreeSelection *listsel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->view->treeview));
	//TODO detect whatever needs to be unselected, without clearing the lot,
	// to protect selections created via other dialog(s)
	gtk_tree_selection_unselect_all (listsel);
	GList *sel = gtk_icon_view_get_selected_items (GTK_ICON_VIEW (rt->iconview));
	if (sel != NULL && g_list_length (sel) > 0)
	{
		//walk the file-list iters, selecting each one that matches
		GtkTreeIter listiter;
		GtkTreeModel *listmdl = gtk_tree_view_get_model (GTK_TREE_VIEW (rt->view->treeview)); //filter model
		if (gtk_tree_model_get_iter_first (listmdl, &listiter))
		{
			GtkTreeIter iter;
			GList *member;
			GtkTreeModel *mdl = GTK_TREE_MODEL (rt->store);
			for (member = sel; member != NULL; member = member->next)
			{
				gchar *name;
				GtkTreePath *tp;
				tp = (GtkTreePath *)member->data;
				gtk_tree_model_get_iter (mdl, &iter, tp);
				gtk_tree_model_get (mdl, &iter, FILENAME, &name, -1);
				if (e2_tree_find_iter_from_str_same (listmdl, FILENAME, name, &listiter))
				{
					gtk_tree_selection_select_iter (listsel, &listiter);
					gtk_tree_model_get_iter_first (listmdl, &listiter); //restart searching, order may be different
				}
				g_free (name);
				gtk_tree_path_free (tp);
			}
		}
		else
			g_list_foreach (sel, (GFunc) gtk_tree_path_free, NULL);

		g_list_free (sel);
		retval = TRUE;
	}
	//for safety-sake, ensure the main-window active pane matches the one we're changing
	if (rt->view != curr_view)
		e2_pane_activate_other();

	return retval;
}
/**
@brief iconview selection foreach function to reselect then clean each selected path
This is needed because selection is cleared when view is disconnected from model
@param data pointer to list item data, a gtk tree path
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_cleanpath (GtkTreePath *data, E2_ThumbDialogRuntime *rt)
{
	gtk_icon_view_select_path (GTK_ICON_VIEW (rt->iconview), data);
	gtk_tree_path_free (data);
}
/**
@brief rotate or flip selected thumbnails in accord with @a type
No change is made to cached data
@param type enumerator of the type of change, rotate + or -, default is flip vertical
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_transform (GdkPixbufRotation type, E2_ThumbDialogRuntime *rt)
{
	GList *selpaths = gtk_icon_view_get_selected_items (GTK_ICON_VIEW (rt->iconview));
	gboolean selection = (selpaths != NULL && g_list_length (selpaths) > 0);
	if (selection)
	{
		GtkTreePath *tp;
		GtkTreeIter iter;
		GdkPixbuf *oldpxb, *newpxb;
		GList *member;

		g_object_ref (G_OBJECT (rt->store));
		gtk_icon_view_set_model (GTK_ICON_VIEW (rt->iconview), NULL);

		for (member = selpaths; member != NULL; member = member->next)
		{
			tp = (GtkTreePath *)member->data;
			gtk_tree_model_get_iter (GTK_TREE_MODEL (rt->store), &iter, tp);
			gtk_tree_model_get (GTK_TREE_MODEL (rt->store), &iter,
				PIXBUF, &oldpxb, -1);
			switch (type)
			{
				case GDK_PIXBUF_ROTATE_CLOCKWISE:
					newpxb = gdk_pixbuf_rotate_simple (oldpxb, GDK_PIXBUF_ROTATE_CLOCKWISE);
					break;
				case GDK_PIXBUF_ROTATE_COUNTERCLOCKWISE:
					newpxb = gdk_pixbuf_rotate_simple (oldpxb, GDK_PIXBUF_ROTATE_COUNTERCLOCKWISE);
					break;
				default:
					newpxb = gdk_pixbuf_flip (oldpxb, FALSE);
					break;
			}
			if (newpxb != NULL)
			{
				g_object_unref (G_OBJECT (oldpxb));
				gtk_list_store_set (rt->store, &iter, PIXBUF, newpxb, -1);
			}
		}

		gtk_icon_view_set_model (GTK_ICON_VIEW (rt->iconview), GTK_TREE_MODEL (rt->store));
		g_object_unref (G_OBJECT (rt->store));

		g_signal_handlers_block_by_func (G_OBJECT (rt->iconview),
			_e2p_thumbs_selection_change_cb, rt);
		g_list_foreach (selpaths, (GFunc)_e2p_thumbs_cleanpath, rt);
		g_signal_handlers_unblock_by_func (G_OBJECT (rt->iconview),
			_e2p_thumbs_selection_change_cb, rt);
	}
	g_list_free (selpaths);
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
@param rt data struct for the dialog where the menu key was pressed

@return
*/
static void _e2p_thumbs_set_menu_position (GtkMenu *menu,
	gint *x, gint *y, gboolean *push_in, E2_ThumbDialogRuntime *rt)
{
	gint left, top;
	gtk_window_get_position (GTK_WINDOW (rt->dialog), &left, &top);
	GtkAllocation alloc;
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (rt->iconview, &alloc);
#else
	alloc = rt->iconview->allocation;
#endif
	*x = left + alloc.x + alloc.width/2;
	*y = top + alloc.y +alloc.height/2 - 30;
	*push_in = FALSE;
}
/**
@brief timer callback execute command corresponding to item selected from a menu
We do this outside the menu-item "activated" callback to avoid disrupting gtk's
menu-activation process
@param cmd allocated UTF-8 command string, free'd here

@return FALSE to remove the source
*/
static gboolean _e2p_thumbs_action_activated_cb2 (gchar *cmd)
{
	printd (DEBUG, "timer callback: _e2p_thumbs_action_activated_cb2");
	CLOSEBGL
	e2_command_run (cmd, E2_COMMAND_RANGE_DEFAULT, app.main_window
#ifdef E2_COMMANDQ
		, FALSE
#endif
		);
	OPENBGL
	g_free (cmd);
	e2_utils_fake_event (); //CHECKME does this actually help?
	return FALSE;
}
/**
@brief setup to execute action corresponding to item selected from filetype tasks menu
This is the callback for handling a selection of a filetype action from the
context menu. To avoid disrupting gtk's menu-activation process, we do not run
the command here.
@param item the activated menu item widget
@param rt data struct for the dialog to which @a item and its parent menu belong

@return
*/
static void _e2p_thumbs_menu_choose_filetype_action_cb (GtkMenuItem *item,
	E2_ThumbDialogRuntime *rt)
{
	printd (DEBUG, "_e2p_thumbs_menu_choose_filetype_action_cb");
	NEEDCLOSEBGL
	GList *sel = gtk_icon_view_get_selected_items (GTK_ICON_VIEW (rt->iconview));
	NEEDOPENBGL
	if (sel != NULL && g_list_length (sel) > 0)
	{
		GString *command;
		GList *member;
		GtkTreeModel *mdl = GTK_TREE_MODEL (rt->store);

		command = g_string_sized_new (512);
		command = g_string_assign (command,
			(gchar *)g_object_get_data (G_OBJECT(item), "action-cmd-key"));
		for (member = sel; member != NULL; member = member->next)
		{
			GtkTreeIter iter;
			gchar *name, *thispath, *qp;
			GtkTreePath *tp;
			tp = (GtkTreePath *)member->data;
			gtk_tree_model_get_iter (mdl, &iter, tp);
			gtk_tree_path_free (tp);
			gtk_tree_model_get (mdl, &iter, FILENAME, &name, -1);
			//FIXME there's a small chance that the dir may have changed
			//without syncing to the dialog content
			thispath = e2_utils_strcat (rt->view->dir, name);
			g_free (name);
//tag E2_BADQUOTES
			qp = e2_utils_quote_string (thispath);
			g_free (thispath);
			g_string_append_printf (command, " %s", qp);
			g_free (qp);
		}
		g_list_free (sel);
		//an idle-callback is no good !?
		g_timeout_add (100, (GSourceFunc)_e2p_thumbs_action_activated_cb2,
			g_string_free (command, FALSE));
	}
}
/**
@brief iconview un-select-all callback

@param widget UNUSED the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_unselect_all_cb (GtkMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	NEEDCLOSEBGL
	gtk_icon_view_unselect_all (GTK_ICON_VIEW (rt->iconview));
	NEEDOPENBGL
}
/**
@brief iconview manual-refresh callback

@param widget UNUSED the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_refresh_cb (GtkMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	if (g_atomic_int_get (&rt->view->listcontrols.refresh_working))
	{
		if (rt->timer_id == 0)
			rt->timer_id = g_timeout_add (300, (GSourceFunc)_e2p_thumbs_wait_to_refresh, rt);
	}
	else if (!g_atomic_int_get (&rt->view->listcontrols.cd_working))
	{
#ifndef LOCAL_BGL
//		NEEDCLOSEBGL
		OPENBGL
#endif
		_e2p_thumbs_refresh_store (rt);
#ifndef LOCAL_BGL
		CLOSEBGL
//		NEEDOPENBGL
#endif
	}
	//if cd is happening already, ignore the refresh request
}
/**
@brief rotate selected thumbnails 90 degrees clockwise
No change is made to cached data
@param widget UNUSWED the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_turn_clockwise_cb (GtkMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2p_thumbs_transform (GDK_PIXBUF_ROTATE_CLOCKWISE, rt);
	NEEDOPENBGL
}
/**
@brief rotate selected thumbnails 90 degrees anti-clockwise
No change is made to cached data
@param widget UNUSED the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_turn_anticlockwise_cb (GtkMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2p_thumbs_transform (GDK_PIXBUF_ROTATE_COUNTERCLOCKWISE, rt);
	NEEDOPENBGL
}
/**
@brief flip selected thumbnails top-to-bottom
No change is made to cached data
@param widget UNUSED the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_flip_cb (GtkMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2p_thumbs_transform (GDK_PIXBUF_ROTATE_NONE, rt);
	NEEDOPENBGL
}
/**
@brief iconview selection-replication callback

@param widget the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_toggle_replication_cb (GtkCheckMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	rt->replicate = gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM (widget));
	//FIXME other adjustments
	if (rt->replicate)
	{
		NEEDCLOSEBGL
		_e2p_thumbs_transfer_selection (rt);
		NEEDOPENBGL
	}
/*	else
	{
	}
*/
}
/**
@brief iconview clamp-image-size toggle callback

@param widget the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_toggle_clamp_cb (GtkCheckMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	gboolean state = gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM (widget));
	e2_option_bool_set_direct (rt->clampset, state);
//	NEEDOPENBGL
	_e2p_thumbs_refresh_cb (NULL, rt);
}
/**
@brief populate @a menu with items for the actions for a filetype
Can't use the standard function for this, it initiates commands using %f, which
may be wrong, and has no data for the callback
Each member of @a actions is like "command" or "label@command"
@param menu the menu widget to which the action menu-items are to be added
@param actions NULL-terminated array of utf8 strings, each a command for a filetype
@param rt data struct for the dialog where the menu key was pressed

@return
*/
static void _e2p_thumbs_menu_filetype_actions (GtkWidget *menu,
	const gchar **actions, E2_ThumbDialogRuntime *rt)
{
	gchar *s;
	GtkWidget *menu_item;

	while (*actions != NULL)
	{
		if ((s = strchr (*actions, '@')) != NULL)  //if always ASCII @, don't need g_utf8_strchr()
		{
			*s = '\0';
			menu_item = e2_menu_add (menu, (gchar *)*actions, NULL, NULL,
				_e2p_thumbs_menu_choose_filetype_action_cb, rt);
			*s = '@';	//revert to original form (this is the 'source' data)
			s++;	//point to command
		}
		else
		{
			s = (gchar *)*actions;
			menu_item = e2_menu_add (menu, s, NULL, NULL,
				_e2p_thumbs_menu_choose_filetype_action_cb, rt);
		}
		//some activation-code wants the command, from the menu item
		g_object_set_data (G_OBJECT(menu_item), "action-cmd-key", s);

		actions++;
	}
}
/**
@brief construct and pop up destroyable context-menu for this dialog
This provides a subset of the filelist context menu, plus a couple of things
@param iconview the widget where the click happened
@param event_button which mouse button was clicked (0 for a menu key)
@param event_time time that the event happened (0 for a menu key)
@param rt runtime struct for the displayed dialog

@return
*/
static void _e2p_thumbs_show_context_menu (GtkWidget *iconview,
	guint event_button, guint32 event_time, E2_ThumbDialogRuntime *rt)
{
	GtkIconView *ivw = GTK_ICON_VIEW (iconview);
	GList *selpaths = gtk_icon_view_get_selected_items (ivw);
	gboolean selection = (selpaths != NULL && g_list_length (selpaths) > 0);
	GtkWidget *item, *menu = e2_menu_get ();

	if (selection)
	{
		GtkTreeIter iter;
		GtkTreeModel *mdl = GTK_TREE_MODEL (rt->store);
		GtkTreePath *tpath = (GtkTreePath *) selpaths->data;
		if (gtk_tree_model_get_iter (mdl, &iter, tpath))
		{
			gchar *filename, *ext;
			const gchar **actions;
			gtk_tree_model_get (mdl, &iter, FILENAME, &filename, -1);
			ext = filename;
			while ((ext = strchr (ext, '.')) != NULL)
			{
				if (ext == filename)
				{	//hidden item, probably
					ext++;
					continue;
				}
				ext++;	//skip discovered dot, ascii '.'. always single char
				actions = e2_filetype_get_actions (ext);
				if (actions != NULL)
				{
					_e2p_thumbs_menu_filetype_actions (menu, actions, rt);
					break;
				}
			}
			g_free (filename);
		}
		gchar *aname = g_strconcat (_A(6),".",_A(69), NULL);
		e2_menu_add_action (menu, _("Open _with.."),"open_with"E2ICONTB, NULL,
			aname, NULL);
		e2_menu_add_separator (menu);

		e2_menu_add (menu, _("Rotate _+"), NULL,
			_("Rotate selected images quarter-turn clockwise"),
			_e2p_thumbs_turn_clockwise_cb, rt);
		e2_menu_add (menu, _("Rotate _-"), NULL,
			_("Rotate selected images quarter-turn anti-clockwise"),
			_e2p_thumbs_turn_anticlockwise_cb, rt);
		e2_menu_add (menu, _("_Flip"), NULL,
			_("Flip selected images top-to-bottom"),
			_e2p_thumbs_flip_cb, rt);
	}

	e2_menu_add (menu, _("_Refresh"), STOCK_NAME_REFRESH, NULL,
		_e2p_thumbs_refresh_cb, rt);

	if (selection)
	{
//		item =
		e2_menu_add (menu, _("_Unselect all"), STOCK_NAME_CLEAR, NULL,
			_e2p_thumbs_unselect_all_cb, rt);
//		if (!selection)
//			gtk_widget_set_sensitive (item, FALSE);
	}
	item =
	e2_menu_add_check (menu, _("Replicate _selection"),
		rt->replicate, _e2p_thumbs_toggle_replication_cb, rt);
	e2_widget_set_safetip (item,
		_("If activated, items selected in this window will also be selected in the associated filelist"));

	item =
	e2_menu_add_check (menu, _("_Clamp size"),
		e2_option_bool_get_direct (rt->clampset), _e2p_thumbs_toggle_clamp_cb, rt);
	gchar *tip = g_strdup_printf (
		_("If activated, thumbnails bigger than %d pixels will be scaled down"),
			e2_option_int_get ("thumb-limit"));
	e2_widget_set_safetip (item, tip);
	g_free (tip);
	//handle change of "thumb-limit" value while menu is displayed ?
/*	item =
	e2_menu_add_check (menu, _("_Hidden items"), rt->show_hidden,
		_e2_treedlg_toggle_strict_cb, rt);
	if (!rt->show_hidden)
		e2_widget_set_safetip (item,
			_("If activated, hidden image files will be displayed"));
	gtk_widget_set_sensitive (item, !rt->show_hidden);
*/

	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	if (event_button == 0)
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) _e2p_thumbs_set_menu_position, rt,
			0, event_time);
	else
		//this was a button-3 click
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			NULL, NULL, 3, event_time);

	if (selection)
		g_list_foreach (selpaths, (GFunc) gtk_tree_path_free, NULL);
	g_list_free (selpaths);
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief mouse button press callback
This is used with or without E2_MOUSECUSTOM, as showing context menu
is hardcoded, not an action
@param iconview the widget where the button was pressed
@param event gdk event data
@param view rt data for the view to be worked on

@return TRUE (stop other handlers) for btn 3 press, else FALSE
*/
static gboolean _e2p_thumbs_button_press_cb (GtkWidget *iconview,
	GdkEventButton *event, E2_ThumbDialogRuntime *rt)
{
	printd (DEBUG, "callback: _e2p_thumbs mouse button press");
	if (event->button == 3
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		NEEDCLOSEBGL
		_e2p_thumbs_show_context_menu (iconview, 3, event->time, rt);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/* *
@brief iconview key-press callback

@param iconview UNUSED the focused treeview widget when the key was pressed
@param event pointer to event data struct
@param rt pointer to dialog data struct

@return TRUE (stop other handlers) for menu key handled, else FALSE
*/
/*static gboolean _e2p_thumbs_key_press_cb (GtkWidget *iconview,
	GdkEventKey *event, E2_ThumbDialogRuntime *rt)
{
	printd (DEBUG, "callback: _e2p_thumbs key press");
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	return FALSE;
} */
/**
@brief menu-button press callback

@param iconview the widget where the press happened
@param rt dialog runtime data struct

@return TRUE always
*/
static gboolean _e2p_thumbs_popup_menu_cb (GtkWidget *iconview,
	E2_ThumbDialogRuntime *rt)
{
	guint32 event_time = gtk_get_current_event_time ();
	NEEDCLOSEBGL
	_e2p_thumbs_show_context_menu (iconview, 0, event_time, rt);
	NEEDOPENBGL
	return TRUE;
}
/**
@brief iconview item-activated callback
Activation is triggered when <Enter> is pressed or when a double-click happens
This causes the activated item to be opened
@param iconview the widget where the activation happened
@param path model path to the clicked item
@param view data struct for the view to be worked on

@return
*/
static void _e2p_thumbs_item_activated_cb (GtkIconView *iconview,
	GtkTreePath *tpath, ViewInfo *view)
{
	printd (DEBUG, "callback: _e2p_thumbs_item_activated");
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_icon_view_get_model (iconview);
    if (gtk_tree_model_get_iter (model, &iter, tpath))
	{
		//get the activated item
		gchar *localpath;
		FileInfo *info;
		gtk_tree_model_get (model, &iter, FINFO, &info, -1);
		localpath = e2_utils_dircat (view, info->filename, TRUE);
		NEEDCLOSEBGL
#ifdef E2_VFS
		VPATH ddata = { localpath, view->spacedata };
		e2_task_backend_open (&ddata, TRUE);
#else
		e2_task_backend_open (localpath, TRUE);
#endif
		NEEDOPENBGL
		g_free (localpath);
	}
}
/**
@brief iconview selection-changed callback
This allows selection to be migrated to the associated filelist treeview

@param iconview the widget where the activation happened
@param rt data struct for the dialog

@return
*/
static void _e2p_thumbs_selection_change_cb (GtkIconView *iconview,
	E2_ThumbDialogRuntime *rt)
{
	if (rt->replicate)
	{
		NEEDCLOSEBGL
		_e2p_thumbs_transfer_selection (rt);
		NEEDOPENBGL
	}
}
/**
@brief iconview sort-column callback

@param widget the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_change_sortcol_cb (GtkMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	gpointer col = g_object_get_data (G_OBJECT (widget), "sort-column");
	rt->sort_type = GPOINTER_TO_INT (col) - 1;
	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (rt->store);
	//arrange to sort new store same as old one
	gtk_tree_sortable_set_sort_func (sortable, FILENAME,
		e2_all_columns[rt->sort_type].sort_func, &rt->sort_order, NULL);
	NEEDCLOSEBGL
	gtk_tree_sortable_set_sort_column_id (sortable, FILENAME, rt->sort_order);
	NEEDOPENBGL
}
/**
@brief iconview sort-order callback

@param widget the menu item widget which activated the callback
@param rt pointer to dialog data struct

@return
*/
static void _e2p_thumbs_toggle_sortorder_cb (GtkMenuItem *widget, E2_ThumbDialogRuntime *rt)
{
	gboolean newchoice = gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM (widget));
	rt->sort_order = (newchoice) ? GTK_SORT_ASCENDING : GTK_SORT_DESCENDING;
	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (rt->store);
	NEEDCLOSEBGL
	gtk_tree_sortable_set_sort_column_id (sortable, FILENAME, rt->sort_order);
	e2_button_set_image (rt->sortbtn, (newchoice) ?
		STOCK_NAME_SORT_DESCENDING : STOCK_NAME_SORT_ASCENDING);
	NEEDOPENBGL
}
/**
@brief create but don't pop up a destroyable sort-options menu for the dialog

@param rt data struct for dialog

@return the menu widget
*/
static GtkWidget *_e2p_thumbs_create_sorting_menu (E2_ThumbDialogRuntime *rt)
{
	gint i;
	GtkWidget *item, *menu = e2_menu_get ();
	for (i = 0; i < MAX_COLUMNS; i++)
	{
		item =
		e2_menu_add_check (menu, gettext (e2_all_columns[i].title), (i == rt->sort_type),
			_e2p_thumbs_change_sortcol_cb, rt);
		//also pass the column no. (bumped to avoid 0=NULL)
		g_object_set_data (G_OBJECT (item), "sort-column", GINT_TO_POINTER (i+1));
	}
	item =
	e2_menu_add_check (menu, _("Ascending"), (rt->sort_order == GTK_SORT_ASCENDING),
		_e2p_thumbs_toggle_sortorder_cb, rt);
	e2_widget_set_safetip (item,
		_("If activated, items are displayed in ascending order"));

	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	return menu;
}
/**
@brief set popup menu position

This function is supplied when calling gtk_menu_popup(), to position
the displayed menu.
set @a push_in to TRUE for menu completely inside the screen,
FALSE for menu clamped to screen size

@param menu the GtkMenu to be positioned
@param x place to store gint representing the menu left
@param y place to store gint representing the menu top
@param push_in place to store pushin flag
@param button the activated dialog button

@return
*/
static void _e2p_thumbs_set_sortmenu_position (GtkMenu *menu, gint *x, gint *y,
	gboolean *push_in, GtkWidget *button)
{
	gint button_y;
	e2_utils_get_abs_pos (button, x, &button_y);
	GtkRequisition menu_size;
#ifdef USE_GTK3_0
	//TODO only height ? gtk_widget_get_preferred_height()
	gtk_widget_get_preferred_size (GTK_WIDGET (menu), NULL, &menu_size);
#else
	gtk_widget_size_request (GTK_WIDGET (menu), &menu_size);
#endif
	//place below or above button, left-aligned
	if (button_y - menu_size.height <= 2)	//> gdk_screen_height ())
#ifdef USE_GTK2_18
	{
		GtkAllocation alloc;
		gtk_widget_get_allocation (button, &alloc);
		*y = button_y + alloc.height + 2;
	}
#else
		*y = button_y + button->allocation.height + 2;
#endif
	else
		*y = button_y - menu_size.height - 2;
	*push_in = FALSE;
}
/**
@brief cleanup during the destruction of the view related to a dialog
@param object UNUSED the view-related object being destroyed
@param rt pointer to data struct for the dialog
@return
*/
static void _e2p_thumbs_destroy_cb (
#ifdef USE_GTK3_0
	GtkWidget *object,
#else
	GtkObject *object,
#endif
	E2_ThumbDialogRuntime *rt)
{
//	NEEDCLOSEBGL
	g_signal_handlers_disconnect_by_func ((gpointer)rt->dialog,
		_e2p_thumbs_response_cb, rt); //no double-handling
//	NEEDOPENBGL
	_e2p_thumbs_response_cb (GTK_DIALOG (rt->dialog), 0, rt);
}
/**
@brief handle button click, window-close etc for directory-tree dialog
This is the callback for response signals emitted from @a dialog
@param dialog UNUSED the dialog where the response was generated
@param response the response returned from the dialog
@param rt pointer to data struct for the dialog

@return
*/
static void _e2p_thumbs_response_cb (GtkDialog *dialog, gint response,
	E2_ThumbDialogRuntime *rt)
{
	switch (response)
	{
/*		case E2_RESPONSE_USER1: //toggle display of hidden items
			rt->show_hidden = !rt->show_hidden;
			_e2_treedlg_refresh_cb (NULL, rt);	//do the content before changing button
			NEEDCLOSEBGL
			e2_button_set_image (rt->hiddenbtn, (rt->show_hidden) ?
				"hidden_noshow"E2ICONTB : "hidden_show"E2ICONTB);
			NEEDOPENBGL
			break;
*/
		case E2_RESPONSE_USER2: //sort-button click
		{
			GtkWidget *menu = _e2p_thumbs_create_sorting_menu (rt);
			guint32 event_time = gtk_get_current_event_time ();
			NEEDCLOSEBGL
			gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
				(GtkMenuPositionFunc) _e2p_thumbs_set_sortmenu_position,
					rt->sortbtn, 1, event_time);
			NEEDOPENBGL
		}
			break;
		default:
			if (g_atomic_int_get (&rt->blocked))
			{
				if (response != GTK_RESPONSE_CANCEL)
					break;
					//TODO support a "cancel" UI and refresh-cancel-mechanism
			}

			if (rt->timer_id != 0)
				g_source_remove (rt->timer_id);
			NEEDCLOSEBGL
#ifdef USE_GTK2_18
			GtkAllocation alloc;
			gtk_widget_get_allocation (rt->dialog, &alloc);
			window_width = alloc.width;
			window_height = alloc.height;
#else
			window_width = rt->dialog->allocation.width;
			window_height = rt->dialog->allocation.height;
#endif
			//NOTE all bindings removed during destruction

			//to prevent leaks, ensure underlying store is not zapped with the dialog
			g_object_ref (G_OBJECT (rt->store));
			gtk_widget_destroy (rt->dialog);
			NEEDOPENBGL
			e2_hook_unregister ((rt->view == &app.pane1.view) ?
				&app.pane1.hook_change_dir : &app.pane2.hook_change_dir,
				(HookFunc)_e2p_thumbs_change_dir_hook, rt, TRUE);
			e2_hook_unregister (&rt->view->hook_refresh,
				(HookFunc)_e2p_thumbs_refresh_hook, rt, TRUE);

			g_signal_handlers_disconnect_by_func ((gpointer)rt->view->treeview,
				_e2p_thumbs_destroy_cb, rt);

			thumbslist = g_slist_remove (thumbslist, rt);

//			show_hidden = rt->show_hidden;	//backups for later use this session
			//FIXME manage shared access to this list
			rt->oldstores = g_slist_append (rt->oldstores, rt->store);
			//NOTE crash if the plugin is unloaded before this cb happens !
			g_idle_add ((GSourceFunc) _e2p_thumbs_clear_old_stores, rt->oldstores);
			DEALLOCATE (E2_ThumbDialogRuntime, rt);
			break;
	}
}

/**
@brief establish and show icons view for contents of dir associated with @a view

This is a thread function i.e. BGL open

@param view data struct for file pane with which the iconview is to be associated

@return NULL
*/
static gpointer _e2p_thumbs_dialog_run (ViewInfo *view)
{
	printd (DEBUG, "create images preview dialog");
	E2_ThumbDialogRuntime *rt = ALLOCATE (E2_ThumbDialogRuntime);
	CHECKALLOCATEDWARN (rt, return NULL;);

	//create empty liststore framework for the dialog
	rt->store = _e2p_thumbs_make_store ();
	if (rt->store == NULL)
	{
		//FIXME warn user
		DEALLOCATE (E2_ThumbDialogRuntime, rt);
		return NULL;
	}

//	rt->show_hidden = show_hidden;	//before dialog is filled CHECKME use view->show_hidden ?
	rt->replicate = TRUE;	//cause iconview selections to be replicated in related treeview
	rt->sort_type = view->sort_column;
	rt->sort_order = view->sort_order;
	rt->filtercount = view->filtercount;
	rt->blocked = 0;
/* no need for this, before the store is initially filled
	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (rt->store);
	gtk_tree_sortable_set_sort_func (sortable, FILENAME,
		e2_all_columns[rt->sort_type].sort_func, &rt->sort_order, NULL);
	//set initial sort arrangment before store is filled
	gtk_tree_sortable_set_sort_column_id (sortable, FILENAME, rt->sort_order);
*/
	rt->view = view;
	rt->path = g_strdup (view->dir);
	rt->clampset = e2_option_get ("thumb-scale");
	rt->oldstores = NULL;

	gchar *title = (view == &app.pane1.view) ? _("pane 1 images") : _("pane 2 images") ;
	CLOSEBGL
	rt->dialog = e2_dialog_create (NULL, NULL, title,
		(ResponseFunc)_e2p_thumbs_response_cb, rt);
	OPENBGL

	e2_dialog_set_negative_response (rt->dialog, GTK_RESPONSE_CLOSE); //override default

	//scrolled window for the treeview
	GtkWidget *sw = e2_widget_add_sw (
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog)),
#else
		GTK_DIALOG (rt->dialog)->vbox,
#endif
		GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC, TRUE, E2_PADDING_SMALL);

	/*now create iconsview */
//	gchar *fontstr = (e2_option_bool_get ("custom-list-font")) ?
//		e2_option_str_get ("list-font") : NULL;	//NULL will cause default font

	GtkTreeModel *mdl = GTK_TREE_MODEL (rt->store);
	//create iconview for the pane related to @a view
	rt->iconview = gtk_icon_view_new_with_model (mdl);
	gtk_container_add (GTK_CONTAINER (sw), rt->iconview);

	g_object_unref (G_OBJECT (rt->store));	//kill the ref from view creation

	//allow non-sorted display using GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
//	GtkTreeSortable *sortable = GTK_TREE_SORTABLE (mdl);
//	gtk_tree_sortable_set_default_sort_func (sortable, NULL, NULL, NULL);

	//set general iconview properties
	GtkIconView *iconview = GTK_ICON_VIEW (rt->iconview);
	gtk_icon_view_set_column_spacing (iconview, E2_PADDING_SMALL);
	gtk_icon_view_set_margin (iconview, E2_PADDING_SMALL);
	gtk_icon_view_set_text_column (iconview, FILENAME);
	gtk_icon_view_set_pixbuf_column (iconview, PIXBUF);

	gtk_icon_view_set_reorderable (iconview, TRUE);
	gtk_icon_view_set_selection_mode (iconview, GTK_SELECTION_MULTIPLE);
#ifdef USE_GTK2_12
//this is for pango >= 1.16, Gtk 2.10.8?
	PangoContext *context = gtk_widget_get_pango_context (rt->iconview);
	const PangoMatrix *matrix = pango_context_get_matrix (context);
	PangoGravity grav = pango_gravity_get_for_matrix (matrix);
	if PANGO_GRAVITY_IS_VERTICAL(grav)
	{
# ifdef USE_GTK3_0
		gtk_orientable_set_orientation (GTK_ORIENTABLE (iconview), GTK_ORIENTATION_HORIZONTAL);
# else
		gtk_icon_view_set_orientation (iconview, GTK_ORIENTATION_HORIZONTAL); //VERTICAL is the default
# endif
	}
#endif
//	gtk_icon_view_set_columns (iconview, gint columns);
//	gtk_icon_view_set_item_width (iconview, gint item_width);
//	gtk_icon_view_set_spacing (iconview, gint spacing);
//	gtk_icon_view_set_row_spacing (iconview, gint row_spacing);
//	gtk_icon_view_set_column_spacing (iconview, gint column_spacing);
//	gtk_icon_view_set_margin (iconview, gint margin);

//	gtk_icon_view_select_path (iconview, GtkTreePath *path);
//	gtk_icon_view_scroll_to_path (iconview, GtkTreePath *path,
//		gboolean use_align, gfloat row_align, gfloat col_align);

//	gtk_icon_view_set_cursor (iconview, GtkTreePath *path,
//		GtkCellRenderer *cell, gboolean start_editing);

#ifdef E2_TRANSIENTBINDINGS
	//add dialog-specific key bindings, before the key-press callback
	//(see also, dialog-general)
	gchar *category = g_strconcat (_C(17),".",_C(11), ".", aname, NULL);	//_(general.dialogs.thumb
	e2_keybinding_enrol (rt->iconview, category, _e2p_thumbs_keybindings);
# ifdef E2_MOUSECUSTOM
	e2_mousebinding_enrol (rt->iconview, category, _e2p_thumbs_mousebindings);
#  ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (rt->iconview, category, _e2p_thumbs_mousegestures);
#  endif
# endif
	g_free (category);
#endif
	//this cb does generic stuff, needed with or without E2_MOUSECUSTOM
	g_signal_connect (G_OBJECT (rt->iconview), "button-press-event",
		G_CALLBACK (_e2p_thumbs_button_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->iconview), "popup-menu",
		G_CALLBACK (_e2p_thumbs_popup_menu_cb), rt);
//	g_signal_connect_after (G_OBJECT (rt->iconview), "key-press-event",
//		G_CALLBACK (_e2_treedlg_key_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->iconview), "item-activated",
		G_CALLBACK (_e2p_thumbs_item_activated_cb), view);
	g_signal_connect (G_OBJECT (rt->iconview), "selection-changed",
		G_CALLBACK (_e2p_thumbs_selection_change_cb), rt);

/*	//by default, type-ahead searching is enabled on column 0
	gtk_tree_view_set_search_equal_func	(GTK_ICON_VIEW (tvw),
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
//		G_CALLBACK (e2_dnd_drag_drop_cb), view);

	g_signal_connect (G_OBJECT (tvw), "drag-data-received",
		 G_CALLBACK(e2_dnd_drag_data_received_cb), view);
	//FIXME do these once only
//	atom_text_uri_list = gdk_atom_intern (target_table[0,0], FALSE);
//	atom_text_plain = gdk_atom_intern (target_table[1,0], FALSE);
//	atom_XdndDirectSave0 = gdk_atom_intern (target_table[2,0], FALSE);

//	g_signal_connect (G_OBJECT (tvw), "drag-data-delete",
//		G_CALLBACK (e2_dnd_drag_delete_cb), view);

	//pick up any key-bindings before the general key-press
	//BUT CHECKME order probably does not matter

	g_signal_connect (G_OBJECT (rt->iconview), "cursor-changed",
		G_CALLBACK (_e2_fileview_cursor_change_cb), view);
*/
	//cleanup if the dialog rt data becomes invalid
	g_signal_connect (
#ifdef USE_GTK3_0
	G_OBJECT (view->treeview),
#else
	GTK_OBJECT (view->treeview),
#endif
		"destroy", G_CALLBACK (_e2p_thumbs_destroy_cb), rt);

	//relate initial size to last-used, or if first, to filepanes size
#ifdef USE_GTK2_18
	GtkAllocation alloc;
	gtk_widget_get_allocation (view->treeview, &alloc);
#endif
	if (window_width == -1)
		window_width =
#ifdef USE_GTK2_18
			alloc.width;
#else
			view->treeview->allocation.width;
#endif
	if (window_height == -1)
		window_height =
#ifdef USE_GTK2_18
			alloc.height;
#else
			view->treeview->allocation.height;
#endif
	gtk_window_resize (GTK_WINDOW (rt->dialog), window_width, window_height);
/*
	rt->hiddenbtn = e2_dialog_add_custom_button_full
		(rt->dialog, FALSE, E2_RESPONSE_USER1,
		_("_Hidden"),
		(rt->show_hidden) ? "hidden_noshow"E2ICONTB : "hidden_show"E2ICONTB,
		_("Toggle display of hidden directories"), NULL, NULL);
*/
	rt->sortbtn = e2_dialog_add_custom_button_full
		(rt->dialog, FALSE, E2_RESPONSE_USER2, _("_Sort"),
		(rt->sort_order == GTK_SORT_ASCENDING) ?
		STOCK_NAME_SORT_DESCENDING : STOCK_NAME_SORT_ASCENDING, NULL, NULL, NULL);

	E2_BUTTON_CLOSE.showflags |= E2_BTN_DEFAULT; //set default button
	e2_dialog_show (rt->dialog, app.main_window, E2_DIALOG_CLOSELOCK,
		&E2_BUTTON_CLOSE, NULL);

	//populate the actual data for the iconview, as soon as we can
	gboolean busy =
		g_atomic_int_get (&rt->view->listcontrols.refresh_working)
	 || g_atomic_int_get (&rt->view->listcontrols.cd_working);
	if (busy)
		rt->timer_id = g_timeout_add_full (G_PRIORITY_HIGH, 100,
			(GSourceFunc)_e2p_thumbs_wait_to_refresh, rt, NULL);
	else
	{
		rt->timer_id = 0;
		_e2p_thumbs_replace_store (rt);
	}

	//show and select the startup row corresponding to displayed dir
//	_e2_treedlg_show_path (rt->view.dir, TRUE, rt);

	e2_hook_register ((view == &app.pane1.view) ?
		&app.pane1.hook_change_dir : &app.pane2.hook_change_dir,
		(HookFunc)_e2p_thumbs_change_dir_hook, rt);
	e2_hook_register (&view->hook_refresh,
		(HookFunc)_e2p_thumbs_refresh_hook, rt);

	thumbslist = g_slist_prepend (thumbslist, rt);

	return NULL;
}
/**
@brief show tree dialog action
This creates a thread to produce the dialog, because the directories scan
can be slow

@param from UNUSED the button, menu item etc which was activated
@param art UNUSED action runtime data

@return TRUE if a gimpthumb handle is available
*/
static gboolean _e2p_thumbs_show_action (gpointer from, E2_ActionRuntime *art)
{
	if (handle == NULL)
		handle = gimp_thumbconnection_new (E2_SOFTWARE_ID, NULL);
	if (handle != NULL)
	{
		E2_PaneRuntime *rt = e2_pane_get_runtime (from, art->data, NULL);
#ifdef USE_GLIB2_32
		g_thread_new ("", (GThreadFunc) _e2p_thumbs_dialog_run, &rt->view);
#else
		g_thread_create ((GThreadFunc) _e2p_thumbs_dialog_run, &rt->view, FALSE, NULL);
#endif
		return TRUE;
	}
	return FALSE;
}

#ifdef E2_TRANSIENTBINDINGS
/**
@brief function to setup default key-bindings for icon-browser dialog
This is just to provide placeholders, the actual bindings are meaningless
@param set pointer to option data struct

@return
*/
static void _e2p_thumbs_keybindings (E2_OptionSet *set)
{
	//the key name strings are parsed by gtk, and no translation is possible
	e2_option_tree_setup_defaults (set,
	g_strdup("keybindings=<"),  //internal name
	//the column-0 category string(s) here need to match at least the lowest
	//treestore-iter of the full category name
//	g_strconcat(_C(17),"||||",NULL),  //_("general"
//	g_strconcat("\t",_C(11),"||||",NULL),  //_("dialogs"
	g_strconcat("\t\t",aname,"||||",NULL),  //_("thumbs"
	g_strconcat("\t\t\t|<Control>j","||",_A(127),".",_A(128),"|<Control>a",NULL),
	g_strconcat("\t\t\t|<Control>k","||",_A(127),".",_A(128),"|<Control>c",NULL),
	g_strdup(">"),
	NULL);
}
# ifdef E2_MOUSECUSTOM
static void _e2p_thumbs_mousebindings (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("mousebuttons=<"),  //internal name
/*	These lines go into category general.dialogs, so at least 2 leading tabs
	The button name strings are parsed by gtk, and no translation is possible
	columns: 0 cat, 1 button, 2 dblclick, 3 trplclick, [4 release,] 5 action, 6 action_data
	If releases handled, lines must have one extra (=6) separator
*/
	g_strconcat("\t\t",aname,"|||||",NULL),  //_("thumbs"
	g_strconcat("\t\t\t|<Control>4","|||",_A(127),".",_A(128),"|<Control>a",NULL),
	g_strconcat("\t\t\t|<Control>5","|||",_A(127),".",_A(128),"|<Control>c",NULL),
	g_strdup(">"),
	NULL);
}
#  ifdef E2_PTRGESTURES
static void _e2p_thumbs_mousegestures (E2_OptionSet *set)
{
	//the button name strings are parsed by gtk, and no translation is possible
	//if releases handled, lines must have one extra (=6) separator
	e2_option_tree_setup_defaults (set,
	//the column-0 category string(s) here need to match at least the lowest
	//treestore-iter of the full category name
	g_strdup("mousedrags=<"),  //internal name
//	g_strconcat(_C(17),"||||",NULL),  //_("general"
//	g_strconcat("\t",_C(11),"||||",NULL),  //_("dialogs"
	g_strconcat("\t\t",aname,"||||",NULL),  //_("thumbs"
	//MORE HERE
	g_strdup(">"),
	NULL);
}
#  endif
# endif
#endif

  /****************/
 /**** public ****/
/****************/

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount = 0 or 1, or NULL if memory allocation failed
*/
Plugin *init_plugin (E2PInit mode)
{
	aname = _("thumbnail"); //other uses too

	PLUGINIT_ONE_START(_A(7),aname,_e2p_thumbs_show_action,
		_("_Thumbnail.."),
		_("Display thumbnails of image files in the active pane"),
		"plugin_"ANAME E2ICONTB)

	E2_OptionSetupExtra ex;
	gchar *group = g_strconcat(_C(34),".",_C(27),":",aname,NULL); //_("plugins.options:thumbnail"
	memset (&ex, 0, sizeof (E2_OptionSetupExtra));
	ex.exbool = TRUE;
	E2_OptionSet *set = e2_plugins_option_register (E2_OPTION_TYPE_BOOL, "thumb-scale",
		group, _("limit thumbnail size"),
		_("If enabled, images larger than the size specified below will be scaled down"),
		NULL, &ex, E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_ADVANCED);
	//because plugins are loaded after config data, config options need to
	//get any data from unknown-options data
	e2_option_transient_value_get (set);

	ex.exint.def = GIMP_THUMB_SIZE_NORMAL;
	ex.exint.min = 16;
	ex.exint.max = 256;
	set = e2_plugins_option_register (E2_OPTION_TYPE_INT, "thumb-limit",
		group, _("largest thumbnail size"),
		_("Pixel-size threshold for images scaling"),
		"thumb-scale", &ex, E2_OPTION_FLAG_ADVANCED);
	e2_option_transient_value_get (set);

	PLUGINIT_ONE_END
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	if (thumbslist != NULL)
	{	//clean up open dialog(s)
		//we can't re-use the response cb as it has an idle cb which will be
		//unavailable after the unload now underway
		GSList *member;
		for (member = thumbslist; member != NULL; member = member->next)
		{
			E2_ThumbDialogRuntime *rt = (E2_ThumbDialogRuntime *)member->data;
			gtk_widget_destroy (rt->dialog);

			e2_hook_unregister ((rt->view == &app.pane1.view) ?
				&app.pane1.hook_change_dir : &app.pane2.hook_change_dir,
				(HookFunc)_e2p_thumbs_change_dir_hook, rt, TRUE);
			e2_hook_unregister (&rt->view->hook_refresh,
				(HookFunc)_e2p_thumbs_refresh_hook, rt, TRUE);
			g_signal_handlers_disconnect_by_func ((gpointer)rt->view->treeview,
				_e2p_thumbs_destroy_cb, rt);

			DEALLOCATE (E2_ThumbDialogRuntime, rt);
		}
		g_slist_free (thumbslist);
	}

#ifdef E2_TRANSIENTBINDINGS
	gchar *category = g_strconcat (_C(17), ".",_C(11), ".", aname, NULL);	//_(general.dialogs.thumbs
	e2_keybinding_disrol (NULL, category);
# ifdef E2_MOUSECUSTOM
	e2_mousebinding_disrol (NULL, category);
#  ifdef E2_PTRGESTURES
	e2_mousegesture_disrol (NULL, category);
#  endif
# endif
	g_free (category);
#endif

	PLUGIN_CLEAR_ACTIONS (p)
	if (ret)
		ret = e2_plugins_option_unregister ("thumb-scale");
	if (ret)
		ret = e2_plugins_option_unregister ("thumb-limit");
	if (ret && handle != NULL)
		gimp_thumbconnection_destroy (handle);

	return ret;
}
