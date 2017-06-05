/* $Id: e2_select_image_dialog.c 3002 2014-01-17 23:17:19Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/dialogs/e2_select_image_dialog.c
@brief Select image dialog

This file contains all functions needed to create a select-image
dialog, which is opened when an icon is clicked on a tree-option
page in the main configuration dialog
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_dialog.h"
#include "e2_select_image_dialog.h"
#include "e2_config_dialog.h"
#include "e2_icons.h"

enum
{
	PIXBUF_COL, TEXT_COL, PATH_COL
};

static gchar *_e2_sidlg_get_icon_dir (E2_SID_Runtime *rt);
static void	_e2_sidlg_fill_custom_store (VPATH *localpath, E2_SID_Runtime *rt);
static gint _e2_sidlg_show_current (gboolean checkboth, E2_SID_Runtime *rt);

//session-cache for initial custom-icons-dir (not cleared at session end)
#ifdef E2_VFSTMP
static VPATH initial_dir;
#else
static gchar *initial_dir;
#endif

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief setup object properties so it's possible to determine the currently selected icon or file

@param dialog the sid-dialog where the response was triggered
@param response the response for the clicked button
@param rt pointer to data for sid-dialog

@return
*/
static void _e2_sidlg_response_cb (GtkDialog *dialog, gint response, E2_SID_Runtime *rt)
{
	GtkTreePath *tpath;
	GList *selected;

	NEEDCLOSEBGL

	selected = (rt->page == 0) ?
		gtk_icon_view_get_selected_items (rt->customview):
		gtk_icon_view_get_selected_items (rt->stockview);

	if (selected != NULL)
		tpath = (GtkTreePath *)selected->data;	//single-mode selection means at most, 1 item
	else
		tpath = NULL;	//warning prevention only

	if (response == E2_RESPONSE_APPLY || response == E2_RESPONSE_MORE)
	{
		if (rt->page == 0)
		{	//working with a custom icon
			if (selected != NULL)
			{
				GtkTreeIter iter;
				gchar *fullname;
				gtk_tree_model_get_iter (rt->custommodel, &iter, tpath);
				gtk_tree_model_get (rt->custommodel, &iter, PATH_COL, &fullname, -1);
				g_object_set_data_full (G_OBJECT (dialog), "image", fullname,
					(GDestroyNotify) g_free);
			}
			else //signal that this is one to ignore
				g_object_set_data (G_OBJECT (dialog), "image", NULL);
		}
		else
		{	//working with a stock icon
			if (selected != NULL)
			{
				GtkTreeIter iter;
				gchar *icon, *fullname;
				gtk_tree_model_get_iter (rt->stockmodel, &iter, tpath);
				gtk_tree_model_get (rt->stockmodel, &iter, TEXT_COL, &icon, -1);
				fullname = g_strconcat ("gtk-", icon, NULL);
				g_object_set_data_full (G_OBJECT (dialog), "image", fullname,
					(GDestroyNotify) g_free);
				g_free (icon);
			}
			else
				g_object_set_data (G_OBJECT (dialog), "image", NULL);
		}
	}
	else if (response == E2_RESPONSE_REMOVE)
	{
		g_object_set_data_full (G_OBJECT (dialog), "image", g_strdup (""),
			(GDestroyNotify) g_free);
		if (selected != NULL)
			gtk_icon_view_unselect_path (
				(rt->page == 0) ? rt->customview : rt->stockview, tpath);
	}
	else //some other response
		g_object_set_data (G_OBJECT (dialog), "image", NULL); //ignore it

	if (selected != NULL)
	{
		gtk_tree_path_free (tpath);
		g_list_free (selected);
	}

	gtk_widget_set_sensitive (rt->rem_btn, (response != E2_RESPONSE_REMOVE));

	NEEDOPENBGL
}
/**
@brief issue ok response for @a dialog
This is a callback for item-activated signal in stock-icons view

@param iconview the object where the activation occurred
@param path treepath of the activated item
@param dialog the parent dialog widget

@return
*/
static void _e2_sidlg_activated_cb (GtkIconView *iconview, GtkTreePath *path,
	GtkWidget *dialog)
{
//	NEEDOPENBGL
	gtk_dialog_response (GTK_DIALOG (dialog), E2_RESPONSE_APPLY);
//	NEEDCLOSEBGL
}
/**
@brief notebook page-switch callback

@param notebook the object whose page changed
@param page UNUSED the notebook page which is now focused
@param page_num index of the new page
@param rt pointer to data for sid-dialog

@return
*/
static void _e2_sidlg_page_switch_cb (GtkNotebook *notebook,
#ifdef USE_GTK3_0
	GtkWidget *page,
#else
	GtkNotebookPage *page,
#endif
	guint page_num, E2_SID_Runtime *rt)
{
	rt->page = page_num;
	NEEDCLOSEBGL
	gtk_widget_set_sensitive (rt->dir_chooser, (page_num == 0));
	NEEDOPENBGL
}
/**
@brief cleanup sid-dialog data

@param dialog
@param rt pointer to data for sid-dialog

@return
*/
static void _e2_sidlg_destroy_cb (GtkWidget *dialog, E2_SID_Runtime *rt)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	g_object_set_data (G_OBJECT (rt->parent), rt->name, NULL);
	g_free (rt->name);
	g_free (rt->icon);
	DEALLOCATE (E2_SID_Runtime, rt);
}
/**
@brief destroy sid-dialog

@param rt pointer to data for sid-dialog

@return
*/
static void _e2_sidlg_destroy_cb2 (E2_SID_Runtime *rt)
{
	gtk_widget_destroy (rt->dialog);
}
/**
@brief change custom icons directory

@param chooser the selection object
@param rt pointer to data for sid-dialog

@return
*/
static void _e2_sidlg_dir_change_cb (GtkFileChooser *chooser, E2_SID_Runtime *rt)
{
	gchar *uri = gtk_file_chooser_get_uri (chooser);
	if (uri != NULL)
	{
		gchar *dirpath = g_filename_from_uri (uri, NULL, NULL);
		if (dirpath != NULL)
		{
			NEEDCLOSEBGL
			gtk_list_store_clear (GTK_LIST_STORE (rt->custommodel));
#ifdef E2_VFSTMP
			VPATH data;
			data.path = dirpath;
			data.spacedata = NULL;	//CHECKME ok?
			_e2_sidlg_fill_custom_store (&data, rt);
#else
			_e2_sidlg_fill_custom_store ((VPATH*)dirpath, rt);
#endif
			//show the relevant icon, if any
			_e2_sidlg_show_current (FALSE, rt);

			NEEDOPENBGL
			//remember for next session
			if (VPSTR (initial_dir) != NULL)
				g_free (VPSTR (initial_dir));
#ifdef E2_VFSTMP
			initial_dir = data;
#else
			initial_dir = dirpath;
#endif
		}
		g_free (uri);
	}
}

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief get the appropriate directory for icons

@param rt pointer to data for sid-dialog

@return newly-allocated string with the path to use, localised with trailing /
*/
static gchar *_e2_sidlg_get_icon_dir (E2_SID_Runtime *rt)
{
	gboolean useiconpath;
	gchar *iconpath, *freeme;
	gpointer dtype = g_object_get_data (G_OBJECT (rt->parent), "dialog-form");
	if (GPOINTER_TO_INT (dtype) == E2_CFGDLG_SINGLE)
	{	//doing a single-page dialog
		iconpath = e2_icons_get_custom_path (TRUE);
	}
	else
	{	//doing a full config dialog
		//so a relevant option may have been changed in this session
		E2_OptionSet *set = e2_option_get ("use-icon-dir");

		if (set->widget != NULL)	//this option is part of the dialog
			useiconpath = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (set->widget));
		else
			useiconpath = e2_option_bool_get_direct (set);
		if (useiconpath)
		{
			set = e2_option_get ("icon-dir");
			if (set->widget != NULL)
			{
				freeme = gtk_editable_get_chars (GTK_EDITABLE (set->widget), 0, -1);
				iconpath = D_FILENAME_TO_LOCALE (freeme);
				g_free (freeme);
				if (!g_str_has_suffix (iconpath, G_DIR_SEPARATOR_S))
				{
					freeme = iconpath;
					iconpath = g_strconcat (freeme, G_DIR_SEPARATOR_S, NULL);
					g_free (freeme);
				}
			}
			else
				iconpath = g_strdup (ICON_DIR G_DIR_SEPARATOR_S);	//localised
		}
		else
			iconpath = g_strdup (ICON_DIR G_DIR_SEPARATOR_S);
	}

	return iconpath;
}

/**
@brief fill liststore for custom icons

@param localpath pointer to icons-directory path data
@param rt pointer to data for sid-dialog

@return
*/
static void	_e2_sidlg_fill_custom_store (VPATH *localpath, E2_SID_Runtime *rt)
{
	GList *entries;
	//get all files
	entries = (GList *)e2_fs_dir_foreach (localpath,
					E2_DIRWATCH_NO,	//assume local icons, so fast read
					NULL, NULL, NULL E2_ERR_NONE());
	if (!E2DREAD_FAILED (entries))
	{
		GList *member;
		gint w, h;
#ifdef USE_GTK3_10
		if (!gtk_icon_size_lookup (GTK_ICON_SIZE_LARGE_TOOLBAR, &w, &h))
#else
		GtkSettings *s = gtk_settings_get_default ();
		if (!gtk_icon_size_lookup_for_settings (s, GTK_ICON_SIZE_LARGE_TOOLBAR, &w, &h))
#endif
		{
			w = h = 24;	//can't find useful size, use this default
		}

		entries = g_list_sort (entries, (GCompareFunc) strcmp);

		GtkListStore *store = GTK_LIST_STORE (rt->custommodel);
		for (member = entries; member != NULL; member = g_list_next (member))
		{
			GtkTreeIter iter;
			gchar *fullpath, *utf;
			GdkPixbuf *pix;
			 //CHECKME want all localised encoding
			fullpath = g_build_filename (VPSTR(localpath), (gchar*) member->data, NULL);
			pix = gdk_pixbuf_new_from_file_at_scale (fullpath, w, h, TRUE, NULL);
			if (pix != NULL)
			{
				utf = g_filename_to_utf8 ((gchar*) member->data, -1, NULL, NULL, NULL);
				if (utf == NULL)
					utf = g_strdup ("Name-error"); //don't bother translating
				gtk_list_store_insert_with_values (store, &iter, -1,
						PIXBUF_COL, pix,
						TEXT_COL, utf,
						PATH_COL, fullpath, //CHECKME localised encoding
						-1);
				g_object_unref (G_OBJECT (pix));
				g_free (utf);
			}
			g_free (fullpath);
			g_free (member->data);
		}
		g_list_free (entries);
	}
}
/**
@brief create and fill liststore for custom icons

@param rt pointer to data for sid-dialog

@return the store
*/
static GtkListStore *_e2_sidlg_create_custom_store (E2_SID_Runtime *rt)
{
	//store with columns for pixbuf, basename (utf-8), fullpath (local)
	//conform to column-number enum
	GtkListStore *store = gtk_list_store_new (3,
		GDK_TYPE_PIXBUF, G_TYPE_STRING, G_TYPE_STRING);

	rt->custommodel = GTK_TREE_MODEL (store);	//the filler wants this
	//FIXME populate this lot of icons at idle, if the current icon is not custom
	gchar *path = _e2_sidlg_get_icon_dir (rt);
#ifdef E2_VFS
	VPATH localpath;
	localpath.path = path;
	localpath.spacedata = NULL;	//local icon files
	_e2_sidlg_fill_custom_store (&localpath, rt);
#else
	_e2_sidlg_fill_custom_store (path, rt);
#endif
	g_free (path);

	return store;
}

#ifdef E2_ADD_STOCKS

#ifndef USE_GLIB2_16
typedef struct _E2_StockHashData
{
	GtkListStore *store;
	GtkStyle *style;
	GtkTextDirection dir;
} E2_StockHashData;

static void _e2_sidlg_stockhash_each (gpointer key, gpointer value, E2_StockHashData *data)
{
	const gchar *name = (const gchar*) key;
	GtkIconSet *iset = gtk_style_lookup_icon_set (data->style, name);
	if (iset != NULL)
	{
		GdkPixbuf *pxb = gtk_icon_set_render_icon (iset, style, dir,
			GTK_STATE_NORMAL, GTK_ICON_SIZE_LARGE_TOOLBAR, NULL, NULL);
		if (pxb != NULL)
		{
			GtkTreeIter iter;

			if (strncmp (name, "gtk-", 4) == 0)	//skip leading "gtk-"
				name += 4;
			gtk_list_store_insert_with_values (data->store, &iter, -1,
				PIXBUF_COL, pxb, TEXT_COL, name, -1);
			g_object_unref (G_OBJECT (pxb));
		}
		else
		{
			printd (DEBUG, "No themed pixbuf created for %s", name);
		}
	}
	else
	{
		printd (DEBUG, "No themed iconset found for %s", name);
	}
}
#endif //ndef USE_GLIB2_16

/**
@brief populate stock icons view
Assumes BGL closed, app.main_window exists (for gtk 3.10+)

@param store empty liststore widget in which to save icons' data
*/
static void _e2_sidlg_fill_stock_store (GtkListStore *store)
{
	extern GHashTable *cached_stocks;
#ifdef USE_GLIB2_16
	gpointer value;
	GHashTableIter hashiter;
#endif

#ifdef USE_GTK3_10
	gint psize = 24;
	gtk_icon_size_lookup (GTK_ICON_SIZE_LARGE_TOOLBAR, &psize, NULL);
	GtkIconTheme *thm =	gtk_icon_theme_get_for_screen
		(gtk_widget_get_screen (app.main_window));
#elif defined (USE_GTK3_0)
	GtkStyleContext *context = gtk_widget_get_style_context (app.main_window);
#else
	GtkStyle *style = gtk_widget_get_style (app.main_window);
	GtkTextDirection dir = gtk_widget_get_direction (app.main_window);
#endif

	e2_icons_cache_stocks (); //freshen cache (SLOW!)

	GtkTreeSortable *sort = GTK_TREE_SORTABLE (store);
//	CLOSEBGL if idle callback
	gtk_tree_sortable_set_sort_column_id (sort,
		GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);

#ifdef USE_GLIB2_16

	g_hash_table_iter_init (&hashiter, cached_stocks);
	while (g_hash_table_iter_next (&hashiter, NULL, &value))
	{
		E2_Stock *data = (E2_Stock *) value;
		gchar *stock = data->stock;
# ifdef USE_GTK3_10
		gchar *iname = data->name;
		if (iname == NULL)
			iname = data->stock;
		GtkIconInfo *inf = gtk_icon_theme_lookup_icon (thm, iname, psize,
			GTK_ICON_LOOKUP_GENERIC_FALLBACK | GTK_ICON_LOOKUP_USE_BUILTIN);
# elif defined(USE_GTK3_0)
		GtkIconSet *iset = gtk_style_context_lookup_icon_set (context, stock);
# else
		GtkIconSet *iset = gtk_style_lookup_icon_set (style, stock);
# endif

# ifdef USE_GTK3_10
		if (inf != NULL)
# else
		if (iset != NULL)
#endif
		{
			GdkPixbuf *pxb;
# ifdef USE_GTK3_10
			pxb = gtk_icon_info_load_icon (inf, NULL);
# elif defined(USE_GTK3_0)
			pxb = gtk_icon_set_render_icon_pixbuf (iset, context,
				GTK_ICON_SIZE_LARGE_TOOLBAR);
# else
			pxb = gtk_icon_set_render_icon (iset, style, dir, GTK_STATE_NORMAL,
				GTK_ICON_SIZE_LARGE_TOOLBAR, NULL, NULL);
# endif
			if (pxb != NULL)
			{
				GtkTreeIter iter;

				if (strncmp (stock, "gtk-", 4) == 0)	//skip leading "gtk-"
					stock += 4;
				gtk_list_store_insert_with_values (store, &iter, -1,
					PIXBUF_COL, pxb, TEXT_COL, stock, -1);
				g_object_unref (G_OBJECT (pxb));
			}
			else
			{
				printd (DEBUG, "No themed pixbuf created for %s", stock);
			}
# ifdef USE_GTK3_10
			g_object_unref (inf);
# endif
		}
		else
		{
			printd (DEBUG, "No themed iconset found for %s", stock);
		}
	}

#else //ndef USE_GLIB2_16

	E2_StockHashData data = { store, style, dir };
	g_hash_table_foreach (cached_stocks, (GHFunc)_e2_sidlg_stockhash_each, &data);

#endif //ndef USE_GLIB2_16

	gtk_tree_sortable_set_sort_column_id (sort, TEXT_COL, GTK_SORT_ASCENDING);
//	OPENBGL if idle callback

//	return FALSE; ditto
}

#else //ndef E2_ADD_STOCKS

static void _e2_sidlg_fill_stock_store (GtkListStore *store)
{
#ifdef USE_GTK3_10
	const gchar *stocks[] =
	{
		STOCK_NAME_ABOUT,
		STOCK_NAME_ADD,
		STOCK_NAME_APPLY,
		STOCK_NAME_BOLD,
		STOCK_NAME_CANCEL,
		STOCK_NAME_CAPS_LOCK_WARNING,
		STOCK_NAME_CDROM,
		STOCK_NAME_CLEAR,
		STOCK_NAME_CLOSE,
		STOCK_NAME_COLOR_PICKER,
		STOCK_NAME_CONNECT,
		STOCK_NAME_CONVERT,
		STOCK_NAME_COPY,
		STOCK_NAME_CUT,
		STOCK_NAME_DELETE,
		STOCK_NAME_DIALOG_AUTHENTICATION,
		STOCK_NAME_DIALOG_ERROR,
		STOCK_NAME_DIALOG_INFO,
		STOCK_NAME_DIALOG_QUESTION,
		STOCK_NAME_DIALOG_WARNING,
		STOCK_NAME_DIRECTORY,
		STOCK_NAME_DISCARD,
		STOCK_NAME_DISCONNECT,
		STOCK_NAME_DND,
		STOCK_NAME_DND_MULTIPLE,
		STOCK_NAME_EDIT,
		STOCK_NAME_EXECUTE,
		STOCK_NAME_FILE,
		STOCK_NAME_FIND_AND_REPLACE,
		STOCK_NAME_FIND,
		STOCK_NAME_FLOPPY,
		STOCK_NAME_FULLSCREEN,
		STOCK_NAME_GO_BACK,
		STOCK_NAME_GO_DOWN,
		STOCK_NAME_GO_FORWARD,
		STOCK_NAME_GOTO_BOTTOM,
		STOCK_NAME_GOTO_FIRST,
		STOCK_NAME_GOTO_LAST,
		STOCK_NAME_GOTO_TOP,
		STOCK_NAME_GO_UP,
		STOCK_NAME_HARDDISK,
		STOCK_NAME_HELP,
		STOCK_NAME_HOME,
		STOCK_NAME_INDENT,
		STOCK_NAME_INDEX,
		STOCK_NAME_INFO,
		STOCK_NAME_ITALIC,
		STOCK_NAME_JUMP_TO,
		STOCK_NAME_JUSTIFY_CENTER,
		STOCK_NAME_JUSTIFY_FILL,
		STOCK_NAME_JUSTIFY_LEFT,
		STOCK_NAME_JUSTIFY_RIGHT,
		STOCK_NAME_LEAVE_FULLSCREEN,
		STOCK_NAME_MEDIA_FORWARD,
		STOCK_NAME_MEDIA_NEXT,
		STOCK_NAME_MEDIA_PAUSE,
		STOCK_NAME_MEDIA_PLAY,
		STOCK_NAME_MEDIA_PREVIOUS,
		STOCK_NAME_MEDIA_RECORD,
		STOCK_NAME_MEDIA_REWIND,
		STOCK_NAME_MEDIA_STOP,
		STOCK_NAME_MISSING_IMAGE,
		STOCK_NAME_NETWORK,
		STOCK_NAME_NEW,
		STOCK_NAME_NO,
		STOCK_NAME_OK,
		STOCK_NAME_OPEN,
		STOCK_NAME_ORIENTATION_LANDSCAPE,
		STOCK_NAME_ORIENTATION_PORTRAIT,
		STOCK_NAME_ORIENTATION_REVERSE_LANDSCAPE,
		STOCK_NAME_ORIENTATION_REVERSE_PORTRAIT,
		STOCK_NAME_PAGE_SETUP,
		STOCK_NAME_PASTE,
		STOCK_NAME_PREFERENCES,
		STOCK_NAME_PRINT_ERROR,
		STOCK_NAME_PRINT,
		STOCK_NAME_PRINT_PAUSED,
		STOCK_NAME_PRINT_PREVIEW,
		STOCK_NAME_PRINT_REPORT,
		STOCK_NAME_PRINT_WARNING,
		STOCK_NAME_PROPERTIES,
		STOCK_NAME_QUIT,
		STOCK_NAME_REDO,
		STOCK_NAME_REFRESH,
		STOCK_NAME_REMOVE,
		STOCK_NAME_REVERT_TO_SAVED,
		STOCK_NAME_SAVE_AS,
		STOCK_NAME_SAVE,
		STOCK_NAME_SELECT_ALL,
		STOCK_NAME_SELECT_COLOR,
		STOCK_NAME_SELECT_FONT,
		STOCK_NAME_SORT_ASCENDING,
		STOCK_NAME_SORT_DESCENDING,
		STOCK_NAME_SPELL_CHECK,
		STOCK_NAME_STOP,
		STOCK_NAME_STRIKETHROUGH,
		STOCK_NAME_UNDELETE,
		STOCK_NAME_UNDERLINE,
		STOCK_NAME_UNDO,
		STOCK_NAME_UNINDENT,
		STOCK_NAME_YES,
		STOCK_NAME_ZOOM_100,
		STOCK_NAME_ZOOM_FIT,
		STOCK_NAME_ZOOM_IN,
		STOCK_NAME_ZOOM_OUT
	};
	gint psize = 24;
	gtk_icon_size_lookup (GTK_ICON_SIZE_LARGE_TOOLBAR, &psize, NULL);
	GtkIconTheme *thm =	gtk_icon_theme_get_for_screen
		(gtk_widget_get_screen (app.main_window));

	gint i, c = sizeof (stocks) / sizeof (stocks[0]);
	for (i = 0; i < c; i++)
	{
		const gchar *iname = stocks[i];
		GtkIconInfo *inf = gtk_icon_theme_lookup_icon (thm, iname, psize,
			GTK_ICON_LOOKUP_GENERIC_FALLBACK | GTK_ICON_LOOKUP_USE_BUILTIN);
		if (inf != NULL)
		{
			GdkPixbuf *pxb = gtk_icon_info_load_icon (inf, NULL);
			if (pxb != NULL)
			{
				GtkTreeIter iter;

				if (strncmp (iname, "gtk-", 4) == 0)	//skip leading "gtk-"
					iname += 4;
				gtk_list_store_insert_with_values (store, &iter, -1,
					PIXBUF_COL, pxb, TEXT_COL, iname, -1);
				g_object_unref (G_OBJECT (pxb));
			}
			else
				printd (DEBUG, "No themed pixbuf created for %s", iname);
			g_object_unref (inf);
		}
		else
			printd (DEBUG, "No registered icon for %s", iname);
	}
#else //ndef USE_GTK3_10
	GSList *ids = gtk_stock_list_ids ();
	if (ids != NULL)
	{
#ifdef USE_GTK3_0
		GtkStyleContext *context = gtk_widget_get_style_context (app.main_window);
#else
		GtkStyle *style = gtk_widget_get_style (app.main_window);
		GtkTextDirection dir = gtk_widget_get_direction (app.main_window);
#endif
		GSList *member;

		ids = g_slist_sort (ids, (GCompareFunc) strcmp);
		for (member = ids; member != NULL; member = g_slist_next (member))
		{
#ifdef USE_GTK3_0
			GtkIconSet *iset = gtk_style_context_lookup_icon_set (context, (const gchar*) member->data);
#else
			GtkIconSet *iset = gtk_style_lookup_icon_set (style, (const gchar*) member->data);
#endif
			if (iset != NULL)
			{
#ifdef USE_GTK3_0
				GdkPixbuf *pxb = gtk_icon_set_render_icon_pixbuf (iset, context,
					GTK_ICON_SIZE_LARGE_TOOLBAR);
#else
				GdkPixbuf *pxb = gtk_icon_set_render_icon (iset, style, dir,
					GTK_STATE_NORMAL, GTK_ICON_SIZE_LARGE_TOOLBAR, NULL, NULL);
#endif
				if (pxb != NULL)
				{
					GtkTreeIter iter;
					gtk_list_store_insert_with_values (store, &iter, -1,
						PIXBUF_COL, pxb,
						TEXT_COL, (gchar*) member->data + 4,	//skip leading "gtk-"
						-1);
					g_object_unref (G_OBJECT (pxb));
				}
			}
			g_free (member->data);
		}
		g_slist_free (ids);
	}
#endif //ndef USE_GTK3_10
}

#endif //ndef E2_ADD_STOCKS

/**
@brief create scrolled window showing stock icons

@param rt pointer to data for sid-dialog

@return
*/
static GtkWidget *_e2_sidlg_create_stock_icon_browser (E2_SID_Runtime *rt)
{
	//store with pixbuf and title, conforms to column-number enum
	GtkListStore *store = gtk_list_store_new (2, GDK_TYPE_PIXBUF, G_TYPE_STRING);
	//FIXME populate this lot of icons at idle, if the current icon is not stock
	//g_idle_add ((GSourceFunc)_e2_sidlg_fill_stock_store, store);
	_e2_sidlg_fill_stock_store (store);

	rt->stockmodel = GTK_TREE_MODEL (store);
	rt->stockview = GTK_ICON_VIEW (gtk_icon_view_new_with_model (rt->stockmodel));
	g_object_unref (G_OBJECT (store));

	gtk_icon_view_set_pixbuf_column (rt->stockview, PIXBUF_COL);
	gtk_icon_view_set_margin (rt->stockview, E2_PADDING);
//	gtk_icon_view_set_spacing (rt->stockview, E2_PADDING_XSMALL);
	gtk_icon_view_set_row_spacing (rt->stockview, E2_PADDING_SMALL);
	gtk_icon_view_set_column_spacing (rt->stockview, E2_PADDING_SMALL);

	//most of this bunch relevant only when icon titles are shown
	gtk_icon_view_set_text_column (rt->stockview, TEXT_COL);
	gint width;
	e2_widget_get_font_pixels (GTK_WIDGET (rt->stockview), &width, NULL);
	if (width == 0)
		width = 8;
#ifdef USE_GTK2_12
	PangoContext *context = gtk_widget_get_pango_context (GTK_WIDGET (rt->stockview));
	const PangoMatrix *matrix = pango_context_get_matrix (context);
	PangoGravity grav = pango_gravity_get_for_matrix (matrix);
	if PANGO_GRAVITY_IS_VERTICAL (grav)
	{
# ifdef USE_GTK3_0
		gtk_orientable_set_orientation (GTK_ORIENTABLE (rt->stockview), GTK_ORIENTATION_HORIZONTAL);
# else
		gtk_icon_view_set_orientation (rt->stockview, GTK_ORIENTATION_HORIZONTAL);
		//CHECKME how to set item height ?
# endif
	}
	else
	{
#endif
		gtk_icon_view_set_item_width (rt->stockview, width * 14); //trial-n-error, 14 seems ok
#ifdef USE_GTK2_12
	}
#endif

	g_signal_connect (G_OBJECT(rt->stockview), "item-activated",
		G_CALLBACK (_e2_sidlg_activated_cb), rt->dialog);

	GtkWidget *sw = e2_widget_get_sw_plain (GTK_POLICY_AUTOMATIC,
		GTK_POLICY_AUTOMATIC);

	gtk_container_add (GTK_CONTAINER (sw), GTK_WIDGET (rt->stockview));

	return sw;
}

/**
@brief create scrolled window showing stock icons

@param rt pointer to data for sid-dialog

@return
*/
static GtkWidget *_e2_sidlg_create_custom_icon_browser (E2_SID_Runtime *rt)
{
	GtkListStore *store = _e2_sidlg_create_custom_store (rt);
	rt->custommodel = GTK_TREE_MODEL (store);
	rt->customview = GTK_ICON_VIEW (gtk_icon_view_new_with_model (rt->custommodel));
	g_object_unref (G_OBJECT (store));

	gtk_icon_view_set_pixbuf_column (rt->customview, PIXBUF_COL);
	gtk_icon_view_set_margin (rt->customview, E2_PADDING);
//	gtk_icon_view_set_spacing (rt->customview, E2_PADDING_XSMALL);
	gtk_icon_view_set_row_spacing (rt->customview, E2_PADDING_SMALL);
	gtk_icon_view_set_column_spacing (rt->customview, E2_PADDING_SMALL);

	//most of this bunch relevant only when icon titles are shown
	gtk_icon_view_set_text_column (rt->customview, TEXT_COL);
	gint width;
	e2_widget_get_font_pixels (GTK_WIDGET (rt->customview), &width, NULL);
	if (width == 0)
		width = 8;
#ifdef USE_GTK2_12
	PangoContext *context = gtk_widget_get_pango_context (GTK_WIDGET (rt->customview));
	const PangoMatrix *matrix = pango_context_get_matrix (context);
	PangoGravity grav = pango_gravity_get_for_matrix (matrix);
	if PANGO_GRAVITY_IS_VERTICAL (grav)
	{
# ifdef USE_GTK3_0
		gtk_orientable_set_orientation (GTK_ORIENTABLE (rt->customview), GTK_ORIENTATION_HORIZONTAL);
# else
		gtk_icon_view_set_orientation (rt->customview, GTK_ORIENTATION_HORIZONTAL);
		//CHECKME how to set item height ?
# endif
	}
	else
	{
#endif
		gtk_icon_view_set_item_width (rt->customview, width * 10); //trial-n-error, 10 seems ok
#ifdef USE_GTK2_12
	}
#endif

	g_signal_connect (G_OBJECT(rt->customview), "item-activated",
		G_CALLBACK (_e2_sidlg_activated_cb), rt->dialog);

	GtkWidget *sw = e2_widget_get_sw_plain (GTK_POLICY_AUTOMATIC,
		GTK_POLICY_AUTOMATIC);

	gtk_container_add (GTK_CONTAINER (sw), GTK_WIDGET (rt->customview));

	return sw;
}

/**
@brief initialize notebook page
This checks stored strings, so expects both icon liststores to be already full of
strings at least
Sets rt->page, but does NOT change the notebook page accordingly
@param checkboth TRUE to check stock items as well as others
@param rt pointer to data for sid-dialog

@return relevant notebook page no.
*/
static gint _e2_sidlg_show_current (gboolean checkboth, E2_SID_Runtime *rt)
{
	GtkTreeIter iter;
	if (checkboth
		&& gtk_tree_model_get_iter_first (rt->stockmodel, &iter)
		&& e2_tree_find_iter_from_str_simple (rt->stockmodel, TEXT_COL,
				rt->icon + 4, //the stored data omits leading "gtk-"
				&iter, FALSE))
	{	//the selected item is a stock item
		//select & show the corresponding icon in the view
		GtkTreePath *path = gtk_tree_model_get_path (rt->stockmodel, &iter);
		if (path != NULL)
		{
			gtk_icon_view_set_cursor (rt->stockview, path, NULL, FALSE);
			gtk_icon_view_select_path (rt->stockview, path);
#ifdef USE_GTK2_8
			//older gtk just ensures visible
			gtk_icon_view_scroll_to_path (rt->stockview, path, TRUE, 0.5, 0.5);
#endif
			gtk_tree_path_free (path);
		}
		return 1;	//notebook page no.
	}
	else
	{ 	//the selected item is a custom-icon
		//point to the icon file, if possible
		if ((rt->icon != NULL) && (*rt->icon != '\0'))
		{
			//rt->icon has no path if the image file is in default dir, or it's a full path
			gint srchcol;

			if (g_path_is_absolute (rt->icon))
				srchcol = PATH_COL;	//look for full-path match
			else
				srchcol = TEXT_COL;	//look for name match CHECKME utf-8 encoding

			//CHECKME file/path encoding ?
			if (gtk_tree_model_get_iter_first (rt->custommodel, &iter)
				&& e2_tree_find_iter_from_str_simple (rt->custommodel, srchcol,
					rt->icon, &iter, FALSE))
			{
				GtkTreePath *path = gtk_tree_model_get_path (rt->custommodel, &iter);
				if (path != NULL)
				{
					gtk_icon_view_set_cursor (rt->customview, path, NULL, FALSE);
					gtk_icon_view_select_path (rt->customview, path);
#ifdef USE_GTK2_8
					//older gtk just ensures visible
					gtk_icon_view_scroll_to_path (rt->customview, path, TRUE, 0.5, 0.5);
#endif
					gtk_tree_path_free (path);
				}
			}
		}
		return 0;
	}
}

  /******************/
 /***** public *****/
/******************/

/**
@brief create icon-selection dialog
Assumes BGL closed
@param parent the main config-dialog widget
@param name dialog title string
@param icon gtk-stock-icon name, or path to icon file, taken from tree-option store
@param event data struct for the event which initiated dialog creation

@return the dialog widget, or NULL if error occurred
*/
GtkWidget *e2_sid_create (GtkWidget *parent, const gchar *name, gchar *icon, GdkEventButton *event)
{
	//get or create and inititialize select image dialog runtime object and
	//ensure that only one runtime object (and only one dialog) exists
	//for every parent widget
	E2_SID_Runtime *rt = g_object_get_data (G_OBJECT (parent), name);
	//the user may have double-clicked on an icon, or ...
	if (rt != NULL && rt->dialog != NULL &&
#ifdef USE_GTK2_18
		gtk_widget_get_visible (rt->dialog))
#else
		GTK_WIDGET_VISIBLE (rt->dialog))
#endif
	{	//the dialog already exists
		printd (NOTICE, "select image dialog already exists");
		if (rt->icon != NULL)
			g_free (rt->icon);
		rt->icon = g_strdup (icon);
		//make dialog show the icon
		_e2_sidlg_show_current (TRUE, rt);
		//present the window
		gtk_window_present (GTK_WINDOW (rt->dialog));
		gtk_notebook_set_current_page (rt->notebook, rt->page);

		return rt->dialog;
	}
	else
	{
		printd (NOTICE, "creating select image dialog");
		rt = ALLOCATE0 (E2_SID_Runtime);
		CHECKALLOCATEDWARN (rt, return NULL;)
		//set up runtime object
		rt->name = g_strdup (name);
		//CHECKME can icon be absolute path ?
		rt->icon = g_strdup (icon);
		rt->parent = parent;
	}

#ifdef USE_GTK3_10
	//icons population can take a while!
	GdkCursor *cursor = gdk_cursor_new (GDK_WATCH);
	gdk_window_set_cursor (gtk_widget_get_window (parent), cursor);
	g_object_unref (G_OBJECT (cursor));
	gdk_flush ();
#endif

	//create dialog widgets
	rt->dialog = e2_dialog_create (NULL, NULL, rt->name,
		(ResponseFunc)_e2_sidlg_response_cb, rt);
	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog));
#else
		GTK_DIALOG (rt->dialog)->vbox;
#endif
//	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);
	//when the dialog is destroyed, free the runtime object
	g_signal_connect (
#ifdef USE_GTK3_0
	G_OBJECT (rt->dialog),
#else
	GTK_OBJECT (rt->dialog),
#endif
		"destroy", G_CALLBACK (_e2_sidlg_destroy_cb), rt);
	//ensure this dialog is destroyed with the parent widget
	if (parent != NULL)
		g_object_set_data_full (G_OBJECT (parent), rt->name, rt,
			(GDestroyNotify) _e2_sidlg_destroy_cb2);

	if (
#ifdef E2_VFSTMP
		initial_dir.path != NULL
#else
		initial_dir != NULL
#endif
		&& (event->state & GDK_CONTROL_MASK))
	{
		//<Ctrl> press always opens default icons dir
#ifdef E2_VFSTMP
		g_free (initial_dir.path);
		initial_dir.path = NULL;
#else
		g_free (initial_dir);
		initial_dir = NULL;
#endif
	}
#ifdef E2_VFSTMP
	if (initial_dir.path == NULL)
		initial_dir.path = _e2_sidlg_get_icon_dir (rt);
//	CHECKME NULL initial_dir.spacedata is ok
#else
	if (initial_dir == NULL)
		initial_dir = _e2_sidlg_get_icon_dir (rt);
#endif

	//before any notebook page-switch cb, setup this button
	gchar *message = _("Choose icons directory");
	rt->dir_chooser = gtk_file_chooser_button_new (message,
		GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
	gtk_file_chooser_set_show_hidden (GTK_FILE_CHOOSER (rt->dir_chooser), TRUE);
	//CHECKME intitial_dir must be localised
	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (rt->dir_chooser), initial_dir);
	g_signal_connect (G_OBJECT (rt->dir_chooser), "current-folder-changed",
		G_CALLBACK (_e2_sidlg_dir_change_cb), rt);
	e2_widget_set_safetip (rt->dir_chooser, message);

	//a notebook is used to change between the icon views
	rt->notebook = GTK_NOTEBOOK (e2_widget_add_notebook
		(dialog_vbox, TRUE, E2_PADDING, _e2_sidlg_page_switch_cb, rt));
	//the custom icon browser is a scrolled window
	GtkWidget *sw = _e2_sidlg_create_custom_icon_browser (rt);
	GtkWidget *label = gtk_label_new_with_mnemonic (_("_other"));
	gtk_notebook_append_page_menu (rt->notebook, sw, label, label);
	//and the stock icon browser is another scrolled window
	sw = _e2_sidlg_create_stock_icon_browser (rt);
	label = gtk_label_new_with_mnemonic (_("_stock"));
	gtk_notebook_append_page_menu (rt->notebook, sw, label, label);

	//decide the initial notebook page, icon, file, and remember page, that's
	//set to 0 in page-switch cb triggered in gtk_widget_show_all()
	gint page = _e2_sidlg_show_current (TRUE, rt);

	//we don't put the chooser-button in dialog action-area, that's always homogenous
	//but we do make it look quite like it's in there
//	gint content_area_border;
//	gint button_spacing;
	gint action_area_border;

	gtk_widget_style_get (rt->dialog,
//						"content-area-border", &content_area_border,
//						"button-spacing", &button_spacing,
						"action-area-border", &action_area_border,
						NULL);
#ifdef USE_GTK3_0
	GtkWidget *bbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
	GtkWidget *bbox2 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	GtkWidget *bbox = gtk_hbox_new (FALSE, 0);
	GtkWidget *bbox2 = gtk_vbox_new (FALSE, 0);
#endif
	GtkWidget *action_area =
#ifdef USE_GTK2_14
		gtk_dialog_get_action_area (GTK_DIALOG (rt->dialog));
#else
		GTK_DIALOG (rt->dialog)->action_area;
#endif
	gtk_box_pack_start (GTK_BOX (bbox), bbox2, FALSE, TRUE, action_area_border);
	gtk_box_pack_start (GTK_BOX (bbox2), rt->dir_chooser, TRUE, TRUE, action_area_border);

	g_object_ref (G_OBJECT (action_area));
	gtk_container_remove (GTK_CONTAINER (dialog_vbox), action_area);
	gtk_box_pack_start (GTK_BOX (bbox), action_area, FALSE, FALSE, 0);
	g_object_unref (G_OBJECT (action_area));

	gtk_box_pack_end (GTK_BOX (dialog_vbox), bbox, FALSE, FALSE, 0);
	//move the replacement buttons-box below the separator
	gtk_box_reorder_child (GTK_BOX (dialog_vbox), bbox, 0);

	rt->rem_btn = e2_dialog_add_defined_button (rt->dialog, &E2_BUTTON_REMOVE);
	e2_widget_set_safetip (rt->rem_btn, _("Remove the current icon"));
	if (*icon == '\0')	//nothing to remove at present
		gtk_widget_set_sensitive (rt->rem_btn, FALSE);

	gtk_window_set_default_size (GTK_WINDOW (rt->dialog), -1, 350);

	E2_Button no_btn;
	e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_KEEP);
	e2_dialog_show (rt->dialog, parent, 0,
		&E2_BUTTON_MORE, &no_btn, &E2_BUTTON_APPLY, NULL); //sets notebook page to 0

	if (page != 0)
		gtk_notebook_set_current_page (rt->notebook, page);

#ifdef USE_GTK3_10
	//icons population can take a while!
	cursor = gdk_cursor_new (GDK_LEFT_PTR);
	gdk_window_set_cursor (gtk_widget_get_window (parent), cursor);
	g_object_unref (G_OBJECT (cursor));
	gdk_flush ();
#endif

	return rt->dialog;
}
