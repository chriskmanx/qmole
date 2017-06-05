/* $Id: e2_icons.c 3031 2014-01-24 21:07:28Z tpgww $

Copyright (C) 2013-2014 tooar <tooar@emelfm2.net>

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
@file src/utils/e2_icons.c
@brief icon-management functions
*/

#include "emelfm2.h"
#include "e2_icons.h"

#ifdef E2_ICONCACHE
/*
typedef enum
{
	E2_IMAGE_BMP,
	E2_IMAGE_SVG
} E2Imagetype;
*/
typedef struct _E2_Image
{
	GdkPixbuf *pixbuf;
	gint size;	//pixel size
//	E2Imagetype type;
//	gint refcount;
} E2_Image;

static gint iconsizes [GTK_ICON_SIZE_DIALOG+1];
//table of E2_Image's, keyed by allocated string like name|pxsize
static GHashTable *cached_icons = NULL;
#endif

//table of E2_Stock's keyed by constant stock string e.g. "gtk-about"
GHashTable *cached_stocks = NULL;

#ifdef E2_ICONCACHE

/**
@brief determine all icon pixel-sizes

assumes width = height
@return
*/
static void _e2_icons_sizes_init (void)
{
	GtkIconSize size;
/*	0 invalid/theme, 1 menu, 2 toolbar small, 3 toolbar large, 4 button, 5 dnd, 6 dialog
	GTK_ICON_SIZE_INVALID, GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR,
	GTK_ICON_SIZE_LARGE_TOOLBAR, GTK_ICON_SIZE_BUTTON, GTK_ICON_SIZE_DND,
	GTK_ICON_SIZE_DIALOG
*/
	gint high;
	gint defsizes[] = {18, 16, 18, 24, 20, 32, 48};	//default sizes
#ifndef USE_GTK3_10
	gchar *sizestr;
	GtkSettings* defs = gtk_settings_get_default ();

	g_object_get (G_OBJECT (defs), "gtk-icon-sizes", &sizestr, NULL);
	//sizestr = NULL or "gtk-menu=16,16;gtk-button=20,20 ..."
/*	if (sizestr == NULL)
	{ //this is one way to work around strange behaviour of gtk_icon_size_get_name ();
		gchar *names[] = {
			"", "menu", "toolbar-small", "toolbar-large", "button", "dnd", "dialog"
			};	//CHECKME
	} */
#endif

	//get the pixel-size corresponding to each GtkIconSize
	for (size = GTK_ICON_SIZE_MENU; size <= GTK_ICON_SIZE_DIALOG; size++)
	{
#ifdef USE_GTK3_10
		if (gtk_icon_size_lookup (size, NULL, &high))
			iconsizes [size] = high;
		else
			iconsizes [size] = defsizes [size];
#else
		if (sizestr == NULL || *sizestr == '\0')
		{
			if (gtk_icon_size_lookup_for_settings (defs, size, NULL, &high))
				iconsizes [size] = high;
			else
				iconsizes [size] = defsizes [size];
		}
		else
		{
			gchar *this = (gchar *) gtk_icon_size_get_name (size);
			if (this == NULL)
			{  //could be a malformed theme-descriptor string
				//and for gtk 2.6.7/8 at least - strange behaviour - GTK_ICON_SIZE_MENU
				//doesn't match here, but does match if we call the fn later!
				if (gtk_icon_size_lookup_for_settings (defs, size, NULL, &high))
					iconsizes [size] = high;
				else
					iconsizes [size] = defsizes [size];
			}
			else
			{
				gchar *s = strstr (sizestr, this);
				if (s != NULL)
				{	//the theme sets this icon size
					s += strlen (this) + 1;	//skip the descriptor and "="
					gchar *p = strchr (s, ',');	//always ascii
					s = g_strndup (s, (p-s));
					iconsizes [size] = atoi (s);
					g_free (s);
				}
				else
				{
					if (gtk_icon_size_lookup_for_settings (defs, size, NULL, &high))
						iconsizes [size] = high;
					else
						iconsizes [size] = defsizes [size];
				}
			}
		}
#endif //ndef Use_GTK3_10
	}

	//now set the default
/* gtk 2.6.8 spits warning about gtk-toolbar-icon-size property
	not existing, tho' API doco says it does !
	size = GTK_ICON_SIZE_INVALID;
	g_object_get (G_OBJECT (defs), "gtk-toolbar-icon-size", &size, NULL);
	if (size == GTK_ICON_SIZE_INVALID)
		size = GTK_ICON_SIZE_LARGE_TOOLBAR;
	iconsizes [0] = iconsizes [size]; */
	iconsizes [0] = iconsizes [GTK_ICON_SIZE_LARGE_TOOLBAR];
#ifndef USE_GTK3_10
	if (sizestr != NULL)
		g_free (sizestr);
#endif
}
/**
@brief get icon pixel size for icon size enumerator @a size

@param size GtkIconSize value

@return icon pixelsize
*/
gint e2_icons_get_pixsize (GtkIconSize size)
{
	return iconsizes [size];
}
/**
@brief get icon size enumerator for icon which is closest-below @a psize

@param psize icon pixel size

@return iconsize
*/
GtkIconSize e2_icons_get_size (gint psize)
{
	GtkIconSize i, isz = GTK_ICON_SIZE_MENU;
	gint psz = 0;
	for (i = GTK_ICON_SIZE_MENU; i <= GTK_ICON_SIZE_DIALOG; i++)
	{
		if (iconsizes [i] > psz && iconsizes [i] <= psize)
		{
			isz = i;
			psz = iconsizes [i];
		}
	}

	return isz;
}
/**
@brief cleanup helper for icon hash

@param data pointer to E2_Image struct for an icon
*/
static void _e2_icons_cache_remove (E2_Image *data)
{
	g_object_unref (G_OBJECT (data->pixbuf));
	DEMALLOCATE (E2_Image, data);
}
/**
@brief setup for icon caching
The destroy func assumes that tabled pixbufs have refcount 1 when
this is called
*/
void e2_icons_cache_init (void)
{
	if (cached_icons == NULL)
		cached_icons = g_hash_table_new_full (g_str_hash, g_str_equal,
			g_free, (GDestroyNotify) _e2_icons_cache_remove);
	_e2_icons_sizes_init ();
//	cached_stocks = g_hash_table_new_full (g_str_hash, g_str_equal, NULL,
//		(GDestroyNotify)_e2_icons_stock_clear);
}
/**
@brief helper func to clear cached icon pixbuf
@param key UNUSED key of hash table item being processed
@param value value of hash table item being processed
@param user_data UNUSED data specified when foreach was initiated
@return
*/
/*static void _e2_utils_image_clear (gpointer key,
	E2_Image *value, gpointer user_data)
{
	value->refcount--;
//	g_object_unref (G_OBJECT (value->pixbuf));
} */
/**
@brief clear all cached icon pixbufs from X memory
@return
*/
void e2_icons_cache_clear (void)
{
//	g_hash_table_foreach (cached_icons, (GHFunc) _e2_icons_cache_remove, NULL);
	g_hash_table_destroy (cached_icons);
//	g_hash_table_destroy (cached_stocks);
}
/**
@brief get current gtk theme
This may be called before main-window is created
@return
*/
static GtkIconTheme *_e2_icons_get_current_theme (void)
{
	GtkIconTheme *thm = (app.main_window) ?
		gtk_icon_theme_get_for_screen
			(gtk_widget_get_screen (app.main_window)) :
		gtk_icon_theme_get_default ();
	return thm;
}
/**
@brief get cached image for icon named @a name with size @a size

If not already cached, the relevant image will be created and added to the cache.

@param name gtk-stock-item name, or NULL for missing image icon, or localised custom-icon filename with or without path
@param size icon size enumerator
@param missing TRUE to return missing-image icon if no pixbuf available for @a name
@return pointer to cached GdkPixbuf (no extra refcount) for the image, or NULL if problem occurred
*/
GdkPixbuf *e2_icons_get_puxbuf (const gchar *name, GtkIconSize size, gboolean missing)
{
	const gchar *check;
	GdkPixbuf *pxb;

	gint psize = e2_icons_get_pixsize (size);

	if (name == NULL)
		name = STOCK_NAME_MISSING_IMAGE;	//revert to default icon image
	check = name;

	while (1)
	{
		//make a unique cache key, with separator for easier updates
		gchar *cachename = g_strdup_printf ("%s|%d", check, psize);
		E2_Image *cached = g_hash_table_lookup (cached_icons, cachename);
		if (cached != NULL)
		{
			g_free (cachename);
//			cached->refcount++;
//			g_object_ref (G_OBJECT (cached->pixbuf));
			return cached->pixbuf;
		}
		else
		{	//need to cache this icon
#ifdef E2_ADD_STOCKS
			gpointer stock;
			if (g_hash_table_lookup_extended (cached_stocks, check, NULL, &stock))
#else
			if (e2_icons_check_stock (check))
#endif
			{	//stock image
//				cached->type = E2_IMAGE_BMP;
#ifdef USE_GTK3_10
				GtkIconTheme *thm = _e2_icons_get_current_theme ();
# ifdef E2_ADD_STOCKS
				gchar *iname = ((E2_Stock*)stock)->name;
				if (iname == NULL)
					iname = ((E2_Stock*)stock)->stock;
				pxb = gtk_icon_theme_load_icon (thm, iname, psize,
					GTK_ICON_LOOKUP_GENERIC_FALLBACK | GTK_ICON_LOOKUP_USE_BUILTIN, NULL);
# else
				pxb = gtk_icon_theme_load_icon (thm, name, psize,
					GTK_ICON_LOOKUP_GENERIC_FALLBACK | GTK_ICON_LOOKUP_USE_BUILTIN, NULL);
# endif
#elif defined(USE_GTK3_0)
				pxb = gtk_icon_set_render_icon_pixbuf (
					gtk_icon_factory_lookup_default (name),
					gtk_widget_get_style_context (app.main_window),
					size);
#else
				pxb = gtk_icon_set_render_icon (
					gtk_icon_factory_lookup_default (name),
					gtk_rc_get_style (app.main_window),
					gtk_widget_get_default_direction (),
					GTK_STATE_NORMAL, size, NULL, NULL);
#endif
			}
			else //a missing stock icon, or a custom icon file
			{
				gchar *fullname;
				if (g_path_is_absolute (name))
					fullname = (gchar *)name; //we want a custom icon
				else
				{
					gchar *freeme = e2_icons_get_custom_path (TRUE);
					fullname = e2_utils_strcat (freeme, name); //maybe we want a custom icon
					g_free (freeme);
				}

				pxb = gdk_pixbuf_new_from_file_at_scale
						(fullname, psize, psize, FALSE, NULL);

				if (fullname != name)
					g_free (fullname);
			}

			if (pxb != NULL)
			{
				cached = MALLOCATE (E2_Image);	//too small for slice
				CHECKALLOCATEDWARN (cached, return NULL;)
				cached->size = psize;
				cached->pixbuf = pxb;
//				cached->refcount = 1;
				g_hash_table_insert (cached_icons, cachename, cached);
				return pxb;
			}

			g_free (cachename);
			if (strncmp (check, "gtk-", 4) == 0)
				check += 4; //try for partial-match
			else if (missing && strcmp (name, STOCK_NAME_MISSING_IMAGE) != 0)
			{
				check = STOCK_NAME_MISSING_IMAGE; //try for missing-icon pixbuf
				missing = FALSE; //don't repeat this stage
			}
			else
				break; //can't find anything
		}
	}

	return NULL;
}

#endif //def E2_ICONCACHE

#ifdef E2_ADD_STOCKS

/**
@brief cleanup helper for stock hash

@param data pointer to E2_Stock struct for an icon
*/
static void _e2_icons_stock_clear (E2_Stock *data)
{
	g_free (data->stock);
	if (data->name)
		g_free (data->name);
	if (data->freelabel)
		g_free (data->label);
	DEALLOCATE (E2_Stock, data);
}

#endif

/**
@brief register 'missing' stock icon(s) with gtk

This uses style-information related to app.main_window, so must be called after
that widget is created.
Assumes BGL is open/off.
If E2_ADD_STOCKS is not defined, it will still process just the gtk-discard icon.
*/
void e2_icons_register_stocks (void)
{
	printd (DEBUG, "register missing stock items");
#ifdef USE_GTK3_10
	const gchar *stockpath = ICON_DIR G_DIR_SEPARATOR_S "stock";
	//downstream assumes BGL open
# ifdef E2_VFS
	VPATH ddata = { stockpath, NULL };	//files in installation-dir must be local
	GList *stockfiles = (GList *)e2_fs_dir_foreach (&ddata,
# else
	GList *stockfiles = (GList *)e2_fs_dir_foreach (stockpath,
# endif
		E2_DIRWATCH_NO, NULL, NULL, NULL E2_ERR_NONE());
	if (!E2DREAD_FAILED (stockfiles))
	{
		GtkIconTheme *thm = _e2_icons_get_current_theme ();
		GList *member;
		for (member = stockfiles; member != NULL; member = member->next)
		{
			gchar *iconname = (gchar *) member->data; //iconname is like dialog-error_48.png
			gchar *s1 = strchr (iconname, '_');
			if (s1 != NULL)
			{
				*s1 = '\0';
				if (!gtk_icon_theme_has_icon (thm, iconname))
				{
					printd (DEBUG, "  %s was missing", iconname);
					*s1 = '_';
					gchar *fullpath = g_build_filename (stockpath, iconname, NULL);
					GdkPixbuf *pxb = gdk_pixbuf_new_from_file (fullpath, NULL);
					g_free (fullpath);
					if (pxb != NULL)
					{
						gchar *s2;
						gulong size = strtoul (s1+1, &s2, 10);
						if (s2 > s1+1)
						{
							*s1 = '\0';
							gtk_icon_theme_add_builtin_icon (iconname, (gint)size, pxb);
						}
					}
					//TODO add to theme somehow ?
				}
			}
			g_free (iconname);
		}
		g_list_free (stockfiles);
	}

#else //ndef USE_GTK3_10

#ifdef USE_GTK3_0
	GtkStyleContext *sc = gtk_widget_get_style_context (app.main_window);
	GtkIconSet *iset = gtk_style_context_lookup_icon_set (sc, GTK_STOCK_DISCARD);
#else //gtk 2
	GtkIconSet *iset = gtk_style_lookup_icon_set (
# ifdef USE_GTK2_14
		gtk_widget_get_style (app.main_window),
# else
		app.main_window->style,
# endif
		GTK_STOCK_DISCARD);
#endif

	if (iset == NULL)
	{
		GtkIconSource *isrc = gtk_icon_source_new ();
		gtk_icon_source_set_filename (isrc, ICON_DIR G_DIR_SEPARATOR_S "stock" G_DIR_SEPARATOR_S STOCK_NAME_DISCARD "_20.png"); //or ".svg");
		gtk_icon_source_set_direction_wildcarded (isrc, TRUE);
		gtk_icon_source_set_size_wildcarded (isrc, TRUE);

		iset = gtk_icon_set_new ();
		gtk_icon_set_add_source (iset, isrc);

		GtkIconFactory *extra_gtk_icons = gtk_icon_factory_new ();
		gtk_icon_factory_add (extra_gtk_icons, GTK_STOCK_DISCARD, iset);
		gtk_icon_factory_add_default (extra_gtk_icons);
		g_object_unref (G_OBJECT (extra_gtk_icons));	//ensure cleanup on gtk close
	}

#endif //ndef USE_GTK3_10
}

#ifdef USE_GTK3_10
# ifdef E2_ADD_STOCKS
/**
@brief "changed" signal callback for @a icon_theme
@param icon_theme
@param userdata UNUSED
*/
static void _e2_icons_themechange_cb (GtkIconTheme *icon_theme, gpointer userdata)
{
	printd (DEBUG, "icon theme changed");
}
# endif //def E2_ADD_STOCKS
#if 0
/**
Find largest avaiable icon-size for @a name, up to a limit of 24 pixels.
This is a helper for initial registration of stock items.
*/
static gint _e2_icons_match_size (const gchar *name, GtkIconTheme *thm)
{
	gint *allsizes = gtk_icon_theme_get_icon_sizes (thm, name);
	if (allsizes != NULL)
	{
		gint ret = 0;
		gint *p;
		for (p = allsizes; *p != '\0'; p++)
		{
			gint psize = *p;
			if (psize == -1)	//scalable
			{
				ret = 24;
				break;
			}
			else if (psize > ret && psize <= 24)
				ret = psize;
		}
		g_free (allsizes);
		return ret;
	}
	return 24; //default
}
#endif //0
#endif

#ifdef E2_ADD_STOCKS

/**
@brief cache name and label of 'stock' icons

Essentially this replicates data which are not accessible on gtk 3.10+.
This should be called after app.main_window is created.

@return
*/
void e2_icons_cache_stocks (void)
{
	if (cached_stocks != NULL)
		g_hash_table_destroy (cached_stocks);

	printd (DEBUG, "Setup cache of stock-items");
	cached_stocks = g_hash_table_new_full (g_str_hash, g_str_equal, NULL,
		(GDestroyNotify)_e2_icons_stock_clear);

	//not all stock-icons are normally used by this application, but any of them
	//may be selected during configuration
	E2_Stock stocks[] =
	{
	 { STOCK_NAME_ABOUT,		"help-about",				N_("_About"), FALSE },
	 { STOCK_NAME_ADD,			"list-add",					N_("_Add"), FALSE },
	 { STOCK_NAME_APPLY,		NULL,						N_("_Apply"), FALSE },
	 { STOCK_NAME_BOLD,			"format-text-bold",			N_("_Bold"), FALSE },
	 { STOCK_NAME_CANCEL,		NULL,						N_("_Cancel"), FALSE },
	 { STOCK_NAME_CAPS_LOCK_WARNING,"dialog-warning-symbolic",NULL, FALSE },
	 { STOCK_NAME_CDROM,		"media-optical",			NULL, FALSE }, //TODO
	 { STOCK_NAME_CLEAR,		"edit-clear",				N_("_Clear"), FALSE },
	 { STOCK_NAME_CLOSE,		"window-close",				N_("_Close"), FALSE },
	 { STOCK_NAME_COLOR_PICKER,	NULL,						NULL, FALSE },
	 { STOCK_NAME_CONNECT,		NULL,						N_("_Connect"), FALSE },
	 { STOCK_NAME_CONVERT,		NULL,						N_("_Convert"), FALSE },
	 { STOCK_NAME_COPY,			"edit-copy",				N_("_Copy"), FALSE },
	 { STOCK_NAME_CUT,			"edit-cut",					N_("_Cut"), FALSE },
	 { STOCK_NAME_DELETE,		"edit-delete",				N_("_Delete"), FALSE },
	 { STOCK_NAME_DIALOG_AUTHENTICATION, "dialog-password",	NULL, FALSE },
	 { STOCK_NAME_DIALOG_ERROR,	"dialog-error",				NULL, FALSE },
	 { STOCK_NAME_DIALOG_INFO,	"dialog-information",		NULL, FALSE },
	 { STOCK_NAME_DIALOG_QUESTION,"dialog-question",		NULL, FALSE },
	 { STOCK_NAME_DIALOG_WARNING,"dialog-warning",			NULL, FALSE },
	 { STOCK_NAME_DIRECTORY,	"folder",					N_("_Directory"), FALSE },
	 { STOCK_NAME_DISCARD,		NULL,						N_("_Discard"), FALSE },
	 { STOCK_NAME_DISCONNECT,	NULL,						N_("_Disconnect"), FALSE },
	 { STOCK_NAME_DND,			NULL,						NULL, FALSE },
	 { STOCK_NAME_DND_MULTIPLE,	NULL,						NULL, FALSE },
	 { STOCK_NAME_EDIT,			NULL,						N_("_Edit"), FALSE },
	 { STOCK_NAME_EXECUTE,		"system-run",				N_("_Execute"), FALSE },
	 { STOCK_NAME_FILE,			"text-x-generic",			N_("_File"), FALSE },
	 { STOCK_NAME_FIND_AND_REPLACE,"edit-find-replace",		N_("_Replace"), FALSE },
	 { STOCK_NAME_FIND,			"edit-find",				N_("_Find"), FALSE },
	 { STOCK_NAME_FLOPPY,		"media-floppy",				NULL, FALSE }, //TODO
	 { STOCK_NAME_FULLSCREEN,	"view-fullscreen",			N_("_Fullscreen"), FALSE },
	 { STOCK_NAME_GO_BACK,		"go-previous",				N_("_Back"), FALSE },
	 { STOCK_NAME_GO_DOWN,		"go-down",					N_("_Down"), FALSE },
	 { STOCK_NAME_GO_FORWARD,	"go-next",					N_("_Forward"), FALSE },
	 { STOCK_NAME_GOTO_BOTTOM,	"go-bottom",				N_("_Bottom"), FALSE },
	 { STOCK_NAME_GOTO_FIRST,	"go-first",					N_("_First"), FALSE },
	 { STOCK_NAME_GOTO_LAST,	"go-last",					N_("_Last"), FALSE },
	 { STOCK_NAME_GOTO_TOP,		"go-top",					N_("_Top"), FALSE },
	 { STOCK_NAME_GO_UP,		"go-up",					N_("_Up"), FALSE },
	 { STOCK_NAME_HARDDISK,		"drive-harddisk",			NULL, FALSE }, //TODO
	 { STOCK_NAME_HELP,			"help-browser",				N_("_Help"), FALSE },
	 { STOCK_NAME_HOME,			"go-home",					N_("_Home"), FALSE },
	 { STOCK_NAME_INDENT,		"format-indent-more",		N_("_Indent"), FALSE },
	 { STOCK_NAME_INDEX,		NULL,						N_("_Index"), FALSE },
	 { STOCK_NAME_INFO,			"dialog-information",		N_("_Information"), FALSE },
	 { STOCK_NAME_ITALIC,		"format-text-italic",		N_("_Italic"), FALSE },
	 { STOCK_NAME_JUMP_TO,		"go-jump",					N_("_Goto"), FALSE },
	 { STOCK_NAME_JUSTIFY_CENTER,"format-justify-center",	N_("_Centre"), FALSE },
	 { STOCK_NAME_JUSTIFY_FILL,	"format-justify-fill",		N_("_Fill"), FALSE },
	 { STOCK_NAME_JUSTIFY_LEFT,	"format-justify-left",		N_("_Left"), FALSE },
	 { STOCK_NAME_JUSTIFY_RIGHT,"format-justify-right",		N_("_Right"), FALSE },
	 { STOCK_NAME_LEAVE_FULLSCREEN,"view-restore",			NULL, FALSE },
	 { STOCK_NAME_MEDIA_FORWARD,"media-seek-forward",		N_("_Forward"), FALSE },
	 { STOCK_NAME_MEDIA_NEXT,	"media-skip-forward",		N_("_Next"), FALSE },
	 { STOCK_NAME_MEDIA_PAUSE,	"media-playback-pause",		N_("_Pause"), FALSE },
	 { STOCK_NAME_MEDIA_PLAY,	"media-playback-start",		N_("_Play"), FALSE },
	 { STOCK_NAME_MEDIA_PREVIOUS,"media-skip-backward",		N_("_Previous"), FALSE },
	 { STOCK_NAME_MEDIA_RECORD,	"media-record",				N_("_Record"), FALSE },
	 { STOCK_NAME_MEDIA_REWIND,	"media-seek-backward",		N_("Re_wind"), FALSE },
	 { STOCK_NAME_MEDIA_STOP,	"media-playback-stop",		N_("_Stop"), FALSE },
	 { STOCK_NAME_MISSING_IMAGE,"image-missing",			NULL, FALSE },
	 { STOCK_NAME_NETWORK,		"network-workgroup",		N_("_Network"), FALSE },
	 { STOCK_NAME_NEW,			"document-new",				N_("_New"), FALSE },
	 { STOCK_NAME_NO,			NULL,						N_("_No"), FALSE },
	 { STOCK_NAME_OK,			NULL,						N_("_OK"), FALSE },
	 { STOCK_NAME_OPEN,			"document-open",			N_("_Open"), FALSE },
	 { STOCK_NAME_ORIENTATION_LANDSCAPE,NULL,				N_("_Landscape"), FALSE },
	 { STOCK_NAME_ORIENTATION_PORTRAIT,NULL,				N_("_Portrait"), FALSE },
	 { STOCK_NAME_ORIENTATION_REVERSE_LANDSCAPE,NULL, 		NULL, FALSE },
	 { STOCK_NAME_ORIENTATION_REVERSE_PORTRAIT,NULL,		NULL, FALSE },
	 { STOCK_NAME_PAGE_SETUP,	"document-page-setup",		NULL, FALSE },
	 { STOCK_NAME_PASTE,		"edit-paste",				N_("_Paste"), FALSE },
	 { STOCK_NAME_PREFERENCES,	"preferences-system",		N_("_Preferences"), FALSE },
	 { STOCK_NAME_PRINT_ERROR,	"printer-error",			NULL, FALSE },
	 { STOCK_NAME_PRINT,		"document-print",			N_("_Print"), FALSE },
	 { STOCK_NAME_PRINT_PAUSED,	"printer-paused",			NULL, FALSE },
	 { STOCK_NAME_PRINT_PREVIEW,"document-print-preview",	N_("Pre_view"), FALSE },
	 { STOCK_NAME_PRINT_REPORT,	"printer-info",				N_("_Report"), FALSE },
	 { STOCK_NAME_PRINT_WARNING,"printer-warning",			NULL, FALSE },
	 { STOCK_NAME_PROPERTIES,	"document-properties",		N_("_Properties"), FALSE },
	 { STOCK_NAME_QUIT,			"application-exit",			N_("_Quit"), FALSE },
	 { STOCK_NAME_REDO,			"edit-redo",				N_("_Redo"), FALSE },
	 { STOCK_NAME_REFRESH,		"view-refresh",				N_("_Refresh"), FALSE },
	 { STOCK_NAME_REMOVE,		"list-remove",				N_("_Remove"), FALSE },
	 { STOCK_NAME_REVERT_TO_SAVED,"document-revert",		N_("_Revert"), FALSE },
	 { STOCK_NAME_SAVE_AS,		"document-save-as",			N_("_Save_as"), FALSE },
	 { STOCK_NAME_SAVE,			"document-save",			N_("_Save"), FALSE },
	 { STOCK_NAME_SELECT_ALL,	"edit-select-all",			N_("_All"), FALSE },
	 { STOCK_NAME_SELECT_COLOR,	NULL,						N_("_Color"), FALSE },
	 { STOCK_NAME_SELECT_FONT,	NULL,						N_("_Font"), FALSE },
	 { STOCK_NAME_SORT_ASCENDING,"view-sort-ascending",		N_("Sort_asc"), FALSE },
	 { STOCK_NAME_SORT_DESCENDING,"view-sort-descending",	N_("Sort_desc"), FALSE },
	 { STOCK_NAME_SPELL_CHECK,	"tools-check-spelling",		N_("_Spell"), FALSE },
	 { STOCK_NAME_STOP,			"process-stop",				N_("_Stop"), FALSE },
	 { STOCK_NAME_STRIKETHROUGH,"format-text-strikethrough",N_("_Strikeout"), FALSE },
	 { STOCK_NAME_UNDELETE,		NULL,						N_("_Undelete"), FALSE },
	 { STOCK_NAME_UNDERLINE,	"format-text-underline",	N_("_Underline"), FALSE },
	 { STOCK_NAME_UNDO,			"edit-undo",				N_("_Undo"), FALSE },
	 { STOCK_NAME_UNINDENT,		"format-indent-less",		N_("_Outdent"), FALSE },
	 { STOCK_NAME_YES,			NULL,						N_("_Yes"), FALSE },
	 { STOCK_NAME_ZOOM_100,		"zoom-original",			N_("_Original"), FALSE },
	 { STOCK_NAME_ZOOM_FIT,		"zoom-fit-best",			N_("_Fit"), FALSE },
	 { STOCK_NAME_ZOOM_IN,		"zoom-in",					N_("Zoom_in"), FALSE },
	 { STOCK_NAME_ZOOM_OUT,		"zoom-out",					N_("Zoom_out"), FALSE }
 	};

	E2_Stock *data;
	guint i, count = sizeof (stocks) / sizeof (E2_Stock);
	for (i = 0; i < count; i++)
	{
		data = ALLOCATE0 (E2_Stock);
		if (data == NULL)
		{
			//TODO warn user
			printd (WARN, "Memory allocation problem");
			return;
		}
		E2_Stock *current = &stocks[i];
//		printd (DEBUG, "cached stock icon key(%u) %s", i, current->stock);
		data->stock = g_strdup (current->stock);
		if (current->name != NULL)
			data->name = g_strdup (current->name);
		if (current->label != NULL)
			data->label = _(current->label); //constant
		if (data->label == current->label)	//nothing translated
		{
			data->label = g_strdup (current->label);
			data->freelabel = TRUE;
		}
		g_hash_table_insert (cached_stocks, data->stock, data);
	}

#ifdef USE_GTK3_10
	GtkIconTheme *thm = _e2_icons_get_current_theme ();
#endif

#if 0
	//NOTE themed icons are not necessarily comprehensive, and their names
	// will be inconsistent with old config data !!!
#ifndef USE_GTK3_10
# ifdef USE_GTK3_0
	GtkStyleContext *context = gtk_widget_get_style_context (app.main_window);
 #else
	GtkStyle *style = gtk_widget_get_style (app.main_window);
# endif
#endif
	GList *member, *themed = gtk_icon_theme_list_icons (thm, NULL);
	for (member = themed; member != NULL; member = member->next)
	{
		gchar *name = (gchar*) member->data;
#ifdef USE_GTK3_10
		gint psize = _e2_icons_match_size (name, thm);
		GtkIconInfo *info = gtk_icon_theme_lookup_icon (thm, name, psize,
			GTK_ICON_LOOKUP_GENERIC_FALLBACK | GTK_ICON_LOOKUP_USE_BUILTIN);
		if (info != NULL)
		{
			data = ALLOCATE0 (E2_Stock);
			if (data == NULL)
			{
				//TODO warn user
				printd (WARN, "Memory allocation problem");
				return;
			}
			data->stock = name;
			const gchar *label = gtk_icon_info_get_display_name (info);
			if (label != NULL)
			{
				data->label = g_strdup (label);
				data->freelabel = TRUE;
			}
			g_hash_table_insert (cached_stocks, name, data);
			g_object_unref (G_OBJECT(info));
		}
#else
		GtkIconSet *iset;
# ifdef USE_GTK3_0
		iset = gtk_style_context_lookup_icon_set (context, name);
# else
		iset = gtk_style_lookup_icon_set (style, name);
# endif
		if (iset != NULL)
		{
			data = ALLOCATE0 (E2_Stock);
			if (data == NULL)
			{
				//TODO warn user
				printd (WARN, "Memory allocation problem");
				return;
			}
			data->stock = name;
//			gchar *label = //TODO workaround lack of API for getting relevant label
//			label = gtk_icon_info_get_display_name (GtkIconInfo *icon_info);
//			printd (DEBUG, "cached themed-icon key %s", (gchar*)tmp->data);
			g_hash_table_insert (cached_stocks, name, data);
		}
#endif
	}
	g_list_free (themed);

#endif //0

#ifdef USE_GTK3_10
	g_signal_connect (G_OBJECT(thm), "changed",
		G_CALLBACK(_e2_icons_themechange_cb), NULL);
#endif
}
/**
@brief clear stock-icon-labels hash table
This is intended for session-end cleanup, the variable is not NULL'd.
*/
void e2_icons_clear_stocks (void)
{
	if (cached_stocks != NULL)
		g_hash_table_destroy (cached_stocks);
}
/**
@brief get label corresponding to @a name if that is a cached stock-item identifier

Used for constructing buttons.

@param name string with name of a 'registered' icon like "dialog-ok" or "gtk-no"
@return translated label if @a name represents a known stock-icon, or NULL otherwize
*/
const gchar *e2_icons_get_stock_label (const gchar *name)
{
	gpointer stock;

	if (g_hash_table_lookup_extended (cached_stocks, name, NULL, &stock))
		return (const gchar*) ((E2_Stock*)stock)->label;
	return NULL;
}
/**
@brief determine whether @a name represents a cached stock icon

@param name string with icon name, a file path/name or name of a 'registered'
 icon like "dialog-ok" or "gtk-no"
@return TRUE if @a name represents a stock-icon
*/
gboolean e2_icons_check_stock (const gchar *name)
{
	return g_hash_table_lookup_extended (cached_stocks, name, NULL, NULL);
}

#else //ndef E2_ADD_STOCKS

const gchar *e2_icons_get_stock_label (const gchar *name)
{
	if (name == NULL || *name == '\0')
		return NULL;
#ifdef USE_GTK3_10
	const gchar *stocks[] =
	{
		STOCK_NAME_ABOUT, N_("_About"),
		STOCK_NAME_ADD, N_("_Add"),
		STOCK_NAME_APPLY, N_("_Apply"),
		STOCK_NAME_BOLD, N_("_Bold"),
		STOCK_NAME_CANCEL, N_("_Cancel"),
		STOCK_NAME_CAPS_LOCK_WARNING, NULL,
		STOCK_NAME_CDROM, NULL , //TODO
		STOCK_NAME_CLEAR, N_("_Clear"),
		STOCK_NAME_CLOSE, N_("_Close"),
		STOCK_NAME_COLOR_PICKER, NULL,
		STOCK_NAME_CONNECT, N_("_Connect"),
		STOCK_NAME_CONVERT, N_("_Convert"),
		STOCK_NAME_COPY, N_("_Copy"),
		STOCK_NAME_CUT, N_("_Cut"),
		STOCK_NAME_DELETE, N_("_Delete"),
		STOCK_NAME_DIALOG_AUTHENTICATION, NULL,
		STOCK_NAME_DIALOG_ERROR, NULL,
		STOCK_NAME_DIALOG_INFO, NULL,
		STOCK_NAME_DIALOG_QUESTION, NULL,
		STOCK_NAME_DIALOG_WARNING, NULL,
		STOCK_NAME_DIRECTORY, N_("_Directory"),
		STOCK_NAME_DISCARD, N_("_Discard"),
		STOCK_NAME_DISCONNECT, N_("_Disconnect"),
		STOCK_NAME_DND, NULL,
		STOCK_NAME_DND_MULTIPLE, NULL,
		STOCK_NAME_EDIT, N_("_Edit"),
		STOCK_NAME_EXECUTE, N_("_Execute"),
		STOCK_NAME_FILE, N_("_File"),
		STOCK_NAME_FIND_AND_REPLACE, N_("_Replace"),
		STOCK_NAME_FIND, N_("_Find"),
		STOCK_NAME_FLOPPY, NULL,
		STOCK_NAME_FULLSCREEN, N_("_Fullscreen"),
		STOCK_NAME_GO_BACK, N_("_Back"),
		STOCK_NAME_GO_DOWN, N_("_Down"),
		STOCK_NAME_GO_FORWARD, N_("_Forward"),
		STOCK_NAME_GOTO_BOTTOM, N_("_Bottom"),
		STOCK_NAME_GOTO_FIRST, N_("_First"),
		STOCK_NAME_GOTO_LAST, N_("_Last"),
		STOCK_NAME_GOTO_TOP, N_("_Top"),
		STOCK_NAME_GO_UP, N_("_Up"),
		STOCK_NAME_HARDDISK, NULL,
		STOCK_NAME_HELP, N_("_Help"),
		STOCK_NAME_HOME, N_("_Home"),
		STOCK_NAME_INDENT, N_("_Indent"),
		STOCK_NAME_INDEX, N_("_Index"),
		STOCK_NAME_INFO, N_("_Information"),
		STOCK_NAME_ITALIC, N_("_Italic"),
		STOCK_NAME_JUMP_TO, N_("_Goto"),
		STOCK_NAME_JUSTIFY_CENTER, N_("_Centre"),
		STOCK_NAME_JUSTIFY_FILL, N_("_Fill"),
		STOCK_NAME_JUSTIFY_LEFT, N_("_Left"),
		STOCK_NAME_JUSTIFY_RIGHT, N_("_Right"),
		STOCK_NAME_LEAVE_FULLSCREEN, NULL,
		STOCK_NAME_MEDIA_FORWARD, N_("_Forward"),
		STOCK_NAME_MEDIA_NEXT, N_("_Next"),
		STOCK_NAME_MEDIA_PAUSE, N_("_Pause"),
		STOCK_NAME_MEDIA_PLAY, N_("_Play"),
		STOCK_NAME_MEDIA_PREVIOUS, N_("_Previous"),
		STOCK_NAME_MEDIA_RECORD, N_("_Record"),
		STOCK_NAME_MEDIA_REWIND, N_("Re_wind"),
		STOCK_NAME_MEDIA_STOP, N_("_Stop"),
		STOCK_NAME_MISSING_IMAGE, NULL,
		STOCK_NAME_NETWORK, N_("_Network"),
		STOCK_NAME_NEW, N_("_New"),
		STOCK_NAME_NO, N_("_No"),
		STOCK_NAME_OK, N_("_OK"),
		STOCK_NAME_OPEN, N_("_Open"),
		STOCK_NAME_ORIENTATION_LANDSCAPE, N_("_Landscape"),
		STOCK_NAME_ORIENTATION_PORTRAIT, N_("_Portrait"),
		STOCK_NAME_ORIENTATION_REVERSE_LANDSCAPE, NULL,
		STOCK_NAME_ORIENTATION_REVERSE_PORTRAIT, NULL,
		STOCK_NAME_PAGE_SETUP, NULL,
		STOCK_NAME_PASTE, N_("_Paste"),
		STOCK_NAME_PREFERENCES, N_("_Preferences"),
		STOCK_NAME_PRINT_ERROR, NULL,
		STOCK_NAME_PRINT, N_("_Print"),
		STOCK_NAME_PRINT_PAUSED, NULL,
		STOCK_NAME_PRINT_PREVIEW, N_("Pre_view"),
		STOCK_NAME_PRINT_REPORT, N_("_Report"),
		STOCK_NAME_PRINT_WARNING, NULL,
		STOCK_NAME_PROPERTIES, N_("_Properties"),
		STOCK_NAME_QUIT, N_("_Quit"),
		STOCK_NAME_REDO, N_("_Redo"),
		STOCK_NAME_REFRESH, N_("_Refresh"),
		STOCK_NAME_REMOVE, N_("_Remove"),
		STOCK_NAME_REVERT_TO_SAVED, N_("_Revert"),
		STOCK_NAME_SAVE_AS, N_("_Save_as"),
		STOCK_NAME_SAVE, N_("_Save"),
		STOCK_NAME_SELECT_ALL, N_("_All"),
		STOCK_NAME_SELECT_COLOR, N_("_Color"),
		STOCK_NAME_SELECT_FONT, N_("_Font"),
		STOCK_NAME_SORT_ASCENDING, N_("Sort_asc"),
		STOCK_NAME_SORT_DESCENDING, N_("Sort_desc"),
		STOCK_NAME_SPELL_CHECK, N_("_Spell"),
		STOCK_NAME_STOP, N_("_Stop"),
		STOCK_NAME_STRIKETHROUGH,N_("_Strikeout"),
		STOCK_NAME_UNDELETE, N_("_Undelete"),
		STOCK_NAME_UNDERLINE, N_("_Underline"),
		STOCK_NAME_UNDO, N_("_Undo"),
		STOCK_NAME_UNINDENT, N_("_Outdent"),
		STOCK_NAME_YES, N_("_Yes"),
		STOCK_NAME_ZOOM_100, N_("_Original"),
		STOCK_NAME_ZOOM_FIT, N_("_Fit"),
		STOCK_NAME_ZOOM_IN, N_("Zoom_in"),
		STOCK_NAME_ZOOM_OUT, N_("Zoom_out")
	};

	guint i, count = sizeof (stocks) / sizeof (stocks[0]);
	for (i = 0; i < count; i += 2)
	{
		if (strcmp (stocks[i], name) == 0)
		{
			if (stocks[i+1] != NULL)
				return _(stocks[i+1]);
			else
				break;
		}
	}
#else
	GtkStockItem item;

	if (gtk_stock_lookup (name, &item))
		return item.label;
#endif

	return NULL;
}
/**
@brief determine whether @a name represents a stock icon

@param name string with icon name, a file path/name or name of a 'registered'
 icon like "dialog-ok" or "gtk-no"
@return TRUE if @a name represents a stock-icon
*/
gboolean e2_icons_check_stock (const gchar *name)
{
	if (name == NULL || *name == '\0')
		return FALSE;
#ifndef USE_GTK3_10
	GtkStockItem item;

	if (gtk_stock_lookup (name, &item))
		return TRUE;
#endif
	GtkIconTheme *thm = _e2_icons_get_current_theme ();
	return (gtk_icon_theme_has_icon (thm, name));
}

#endif //ndef E2_ADD_STOCKS

/**
@brief get path of directory nominated to contain emelfm2 custom icons
No checking of the directory's accessibilty is done
@param withtrailer TRUE if the returned string needs to have a trailing path-separator

@return newly-allocated, localised, absolute path string
*/
gchar *e2_icons_get_custom_path (gboolean withtrailer)
{
	const gchar *path;
	gchar *localpath, *freeme;
	if (e2_option_bool_get ("use-icon-dir"))
	{
		path = e2_option_str_get ("icon-dir");
		localpath = D_FILENAME_TO_LOCALE (path);
		if (!g_path_is_absolute (localpath))
		{
			freeme = localpath;
			localpath = g_build_filename (ICON_DIR, localpath, NULL);
			g_free (freeme);
		}
/*	//default icons in config dir if that's usable
		if (!e2_fs_is_dir3 (localpath E2_ERR_NONE()))
			openpath = g_strconcat (
#ifdef E2_FAM
				e2_cl_options.config_dir,
#else
				g_get_home_dir (), G_DIR_SEPARATOR_S,
#endif
				_("icons"), NULL);
*/

		if (g_str_has_suffix (localpath, G_DIR_SEPARATOR_S))
		{
			if (!withtrailer)
				*(localpath + strlen (localpath) - sizeof(gchar)) = '\0';
		}
		else if (withtrailer)
		{
			freeme = localpath;
			localpath = e2_utils_strcat (freeme, G_DIR_SEPARATOR_S);
			g_free (freeme);
		}
	}
	else
	{
		path = (withtrailer) ? ICON_DIR G_DIR_SEPARATOR_S : ICON_DIR;	//localised
		localpath = g_strdup (path);
	}
	return localpath;
}
