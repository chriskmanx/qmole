/* $Id: e2_file_info_dialog.c 2993 2014-01-10 02:55:17Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

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
#include <unistd.h>
#include <langinfo.h>
#include <time.h>
#include <pwd.h>
#include <grp.h>
#ifdef E2_MAGIC
# include <magic.h>
# include <dlfcn.h>
#endif
#include "e2_dialog.h"
#include "e2_icons.h"

//max length of the file type string
#define FILE_TYPE_LENGTH 128

typedef struct _E2_FileInfoRuntime
{
	GtkWidget *dialog;
	GtkWidget *description;
	GtkWidget *target;
	GtkWidget *mime;
	GtkWidget *size;
	GtkWidget *user;
	GtkWidget *group;
	GtkWidget *permissions;
	GtkWidget *accessdate;
	GtkWidget *moddate;
	GtkWidget *changedate;
	const gchar *localpath; //path of item being displayed
} E2_FileInfoRuntime;

/**
@brief convert @a number to string with thousand-separators
@param number the number to be processed
@return allocated string, UTF-8 or compatible
*/
static gchar *_e2_file_info_dialog_prettify_number (guint64 number)
{
	guint64 whole;
	gchar *result;
	gchar tmp[16];

	gboolean expanded = FALSE;
	const gchar *comma = nl_langinfo (THOUSEP); //this may need conversion to UTF-8
	if (comma == NULL || *comma == '\0')
		comma = ",";

	GString *buf = g_string_new (NULL);
	while ((whole = number / 1000) > 0)
	{
		snprintf (tmp, 16, "%s%u", comma, (guint)number % 1000);
		buf = g_string_prepend (buf, tmp);
		number = whole;
		expanded = TRUE;
	}
	snprintf (tmp, 16, "%u", (guint)number);
	buf = g_string_prepend (buf, tmp);

	if (expanded)
	{
		result = e2_utf8_from_locale (buf->str);
		g_string_free (buf, TRUE);
	}
	else
		result = g_string_free (buf, FALSE);

	return result;
}
/**
@brief get description and mimetype of @a filepath, using libmagic and/or xdg-mime command
@a readable_type and/or @a mime_type are set to NULL if nothing valid is available
Assumes BGL closed
@param localpath data for item to be processed, including localised path string
@param readable_type store for pointer to item-description, allocated localised string
@param mime_type store for pointer to determined mime (and for text files, charset too) string

@return TRUE if both description and mime strings are populated successfully (which happens if file options are bad)
*/
static gboolean _e2_file_info_dialog_get_file_type (VPATH *localpath,
	gchar **readable_type, gchar **mime_type)
{
#ifdef E2_VFS
	if (e2_fs_item_is_mounted (localpath))
	{
#endif
#ifdef E2_MAGIC
		MagicIface ifc;
		*readable_type = NULL;
		*mime_type = NULL;

		if (e2_utils_fill_magic_iface (&ifc))
		{
			magic_t handle = ifc.open (
				MAGIC_PRESERVE_ATIME | MAGIC_RAW | MAGIC_ERROR | MAGIC_DEVICES);
			if (handle != NULL)
			{
				gchar *results;
				const gchar *msg;

				ifc.load (handle, NULL); //load failure will result in NULL msg
				msg = ifc.file (handle, VPCSTR (localpath));
				if (msg != NULL)
				{
					results = g_strdup (msg);
					g_strstrip (results);
					if (*results != '\0')
						*readable_type = results;
					else
						g_free (results);
				}
				else
				{
					msg = ifc.error (handle);
					if (msg != NULL)
						e2_output_print_error ((gchar *)msg, FALSE);
				}

				*mime_type = e2_utils_get_mimetype (localpath);

				if (*mime_type != NULL && g_str_has_prefix (*mime_type, "text/"))
				{
					//try to get encoding as well
					ifc.setflags (handle, MAGIC_PRESERVE_ATIME | MAGIC_RAW | MAGIC_ERROR
						| MAGIC_MIME_TYPE | MAGIC_MIME_ENCODING);
					msg = ifc.file (handle, VPCSTR (localpath));
					if (msg != NULL)
					{
						results = g_strdup (msg);
						g_strstrip (results);	//get rid of \n etc
						if (*results != '\0')
						{
							g_free (*mime_type);
							*mime_type = results;
						}
						else
							g_free (results);
					}
					else
					{
						msg = ifc.error (handle);
						if (msg != NULL)
							e2_output_print_error ((gchar *)msg, FALSE);
					}
				}

				ifc.close (handle);
				return TRUE;
			}
			dlclose (ifc.libhandle);
		}
//		e2_output_print_error ("?", FALSE);
		return FALSE;
#else
//tag E2_BADQUOTES
		gchar *qp = e2_utils_quote_string (VPCSTR (localpath));
		gchar *command = e2_utils_strcat ("file -bhnprs ", qp);
		gpointer results;
		if (!e2_fs_get_command_output (command, &results))
		{
			g_free (qp);
			g_free (command);
			*readable_type = NULL;
			*mime_type = NULL;
			return FALSE;
		}
		g_free (command);
		g_strstrip ((gchar *)results);	//get rid of \n etc
		*readable_type = results;

		*mime_type = e2_utils_get_mimetype (localpath);

		if (*mime_type != NULL && g_str_has_prefix (*mime_type, "text/"))
		{
			//try to get encoding as well
			command = e2_utils_strcat ("file -bhnpri ", qp);
			if (e2_fs_get_command_output (command, &results))
			{
				g_strstrip ((gchar *)results);	//get rid of \n etc
				if (*(gchar *)results != '\0')
				{
					g_free (*mime_type);
					*mime_type = results;
				}
				else
					g_free (results);
			}
			g_free (command);
		}
		g_free (qp);

		return TRUE;
#endif
#ifdef E2_VFS
	}
	else	//not a local item
	{
# ifdef E2_VFSTMP
		FIXME
# else
		*readable_type = NULL;
		*mime_type = NULL;
		return FALSE;
# endif
	}
#endif
}
/**
@brief set popup menu position

This function is supplied when calling gtk_menu_popup(), to position
the displayed menu.
set @a push_in to TRUE for menu completely inside the screen,
FALSE for menu clamped to screen size

@param menu the GtkMenu to be positioned
@param x pointer to store for left-position of @a menu
@param y pointer to store for top-position of @a menu
@param push_in pointer to store for pushin flag
@param dialog the dialog widget in focus when the menu key was pressed

@return
*/
static void _e2_file_info_dialog_set_menu_position (GtkWidget *menu,
	gint *x, gint *y, gboolean *push_in, GtkWidget *dialog)
{
	gint left, top;
	gtk_window_get_position (GTK_WINDOW (dialog), &left, &top);
	GtkAllocation alloc;
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (dialog, &alloc);
#else
	alloc = dialog->allocation;
#endif
	*x = left + alloc.x + alloc.width/2;
	*y = top + alloc.y + alloc.height/2;
	*push_in = FALSE;
}
/**
@brief copy dialog data to clipboard

@param menuitem UNUSED the activated widget
@param rt runtime struct to work on

@return
*/
static void _e2_file_info_dialog_copy_cb (GtkMenuItem *menuitem,
	E2_FileInfoRuntime *rt)
{
	//some format massaging is needed ...
	gchar *utf = F_FILENAME_FROM_LOCALE (rt->localpath);
	const gchar *target;

	NEEDCLOSEBGL

	if (rt->target != NULL)
		target = gtk_label_get_text (GTK_LABEL(rt->target));
	else
		target = "";
	gchar *typelabel;
	const gchar *type = gtk_label_get_text (GTK_LABEL(rt->description));
	if (g_str_has_prefix (type, _("Type:")))
		typelabel = "";
	else
		typelabel = g_strconcat (_("Type:"), " ", NULL);

	GString *text = g_string_sized_new (512);
	g_string_printf (text, "%s %s\n" "%s%s%s\n",
		_("Item:"), utf, typelabel, type, target);
	if (rt->mime != NULL)
	{
		//the mime string may have 2 lines, if so the 2nd is encoding
		const gchar *mime = gtk_label_get_text (GTK_LABEL(rt->mime));
		const gchar *s = strchr (mime, '\n');
		if (s == NULL)
			g_string_append_printf (text, "%s %s\n", _("Mime:"), mime);
		else
		{
			gchar *freeme = g_strndup (mime, s - mime);
			g_string_append_printf (text, "%s %s\n" "%s %s\n",
				_("Mime:"), freeme, _("Encoding:"), s + sizeof (gchar));
			g_free (freeme);
		}
	}

	NEEDOPENBGL

	g_string_append_printf (text,
 		"%s %s\n" "%s %s\n" "%s %s\n" "%s %s\n" "%s %s\n" "%s %s\n" "%s %s\n",
		_("Size:"), gtk_label_get_text (GTK_LABEL(rt->size)),
		_("User:"), gtk_label_get_text (GTK_LABEL(rt->user)),
		_("Group:"), gtk_label_get_text (GTK_LABEL(rt->group)),
		_("Permissions:"), gtk_label_get_text (GTK_LABEL(rt->permissions)),
		_("Accessed:"), gtk_label_get_text (GTK_LABEL(rt->accessdate)),
		_("Content modified:"), gtk_label_get_text (GTK_LABEL(rt->moddate)),
		_("Property changed:"), gtk_label_get_text (GTK_LABEL(rt->changedate))
	);

	GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
	gtk_clipboard_set_text (cb, text->str, text->len);
	F_FREE (utf, rt->localpath);
	if (*typelabel != '\0')
		g_free (typelabel);
	g_string_free (text, TRUE);
}
/**
@brief construct and pop up destroyable context-menu for @a dialog

@param textview the textview widget where the click happened
@param event_button which mouse button was clicked (0 for a menu key)
@param event_time time that the event happened (0 for a menu key)
@param rt runtime struct to work on

@return
*/
static void _e2_file_info_dialog_show_context_menu (GtkWidget *dialog,
	guint event_button, guint32 event_time, E2_FileInfoRuntime *rt)
{
	GtkWidget *menu = e2_menu_get ();
	e2_menu_add (menu, _("_Copy"), STOCK_NAME_COPY,
		_("Copy displayed data"), _e2_file_info_dialog_copy_cb, rt);
	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);
	if (event_button == 0)
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) _e2_file_info_dialog_set_menu_position,
			rt->dialog, 0, event_time);
	else
		//this was a button-3 click
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			NULL, NULL, event_button, event_time);
}
/**
@brief menu-button press callback

@param textview the textview widget where the press happened
@param rt dialog runtime data struct

@return TRUE always
*/
static gboolean _e2_file_info_dialog_popup_menu_cb (GtkWidget *dialog,
	E2_FileInfoRuntime *rt)
{
	guint32 event_time = gtk_get_current_event_time ();
	NEEDCLOSEBGL
	_e2_file_info_dialog_show_context_menu (dialog, 0, event_time, rt);
	NEEDOPENBGL
	return TRUE;
}
/**
@brief mouse button press callback

@param textview the widget where the button was pressed
@param event gdk event data
@param rt rt data for the dialog

@return TRUE (stop other handlers) for btn 3 press, else FALSE (allow other handlers)
*/
static gboolean _e2_file_info_dialog_button_press_cb (GtkWidget *dialog,
	GdkEventButton *event, E2_FileInfoRuntime *rt)
{
	if (event->button == 3
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		NEEDCLOSEBGL
		_e2_file_info_dialog_show_context_menu (dialog, 3, event->time, rt);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief update total count of items in a directory
This is a callback for a treewalk function
@param localpath absolute path of item reported by the walker, localised string
@param statptr pointer to struct stat with data about @a localpath
@param status code from the walker, indicating what type of report it is
@param twdata pointer to integer to update

@return E2TW_CONTINUE always
*/
static E2_TwResult
_e2_file_info_dialog_count_twcb (VPATH *localpath, const struct stat *statptr,
	E2_TwStatus status, guint64 *twdata)
{
	switch (status)
	{
		case E2TW_DL:	//dir, not opened due to tree-depth limit (reported upstream)
		case E2TW_DM:	//dir, not opened due to different file system (reported upstream)
		case E2TW_D:	//dir (don't care about its reported size)
		case E2TW_DRR:	//dir now readable
		case E2TW_DNR:	//unreadable dir (reported upstream)
		{
			gchar *fullpath = VPSTR (localpath);
			gchar *base = strrchr (fullpath, G_DIR_SEPARATOR);
			if (base != NULL)
				base++;
			else
				base = fullpath;
			if (*base == '.' && *(base+1) == '\0') //".." items are filtered out upstream
				break;
		}
		case E2TW_F:	//not dir or link
		case E2TW_SL:	//symbolic link
		case E2TW_SLN:	//symbolic link naming non-existing file
		case E2TW_NS:	//un-statable item (reported upstream)
			*twdata += 1;
//		case E2TW_DP:	//dir, finished
		default:
			break;
	}
	return E2TW_CONTINUE;
}
/**
@brief update total size of items in a directory
This is a callback for a treewalk function
@param localpath absolute path of item reported by the walker, localised string
@param statptr pointer to struct stat with data about @a localpath
@param status code from the walker, indicating what type of report it is
@param twdata pointer to integer to update

@return E2TW_CONTINUE always
*/
static E2_TwResult
_e2_file_info_dialog_usage_twcb (VPATH *localpath, const struct stat *statptr,
	E2_TwStatus status, guint64 *twdata)
{
	switch (status)
	{
		//don't bother filtering out "." directories, if any
		case E2TW_DL:	//dir, not opened due to tree-depth limit (reported upstream)
		case E2TW_DM:	//dir, not opened due to different file system (reported upstream)
		case E2TW_D:	//dir (don't care about its reported size)
		case E2TW_DRR:	//dir now readable
		case E2TW_DNR:	//unreadable dir (reported upstream)
		case E2TW_F:	//not dir or link
		case E2TW_SL:	//symbolic link
		case E2TW_SLN:	//symbolic link naming non-existing file
			*twdata += statptr->st_size;
//		case E2TW_NS:	//un-statable item (reported upstream)
//		case E2TW_DP:	//dir, finished
		default:
			break;
	}
	return E2TW_CONTINUE;
}
/**
@brief create and run file info dialog
Expects BGL off/open
@param localpath localised path string of item to be processed
@param multi TRUE when the dialog needs buttons consistent with >1 item being processed

@return enumerator for pressed dialog button
*/
DialogButtons e2_file_info_dialog_run (VPATH *localpath, gboolean multi)
{
	GtkWidget *dialog_vbox;
	GtkWidget *table;
	GString *label_text = g_string_sized_new (NAME_MAX + 32);  //this is plenty big for all uses here
	gchar *s1, *s2;
	gchar *mime;
	gchar date_string[48];
    struct stat statbuf;
	struct tm *tm_ptr;
	struct passwd *pw_buf;
	struct group *grp_buf;
	E2_FileInfoRuntime rt;

//tag DEBUGfreeze;
	//maybe locking gtk out of X during dialog setup will help
	//CHECKME effects of child-signals received due to file-command pipe ?
//	CLOSEBGL

	if (e2_fs_lstat (localpath, &statbuf E2_ERR_NONE()))
	{
		printd (ERROR, "Cannot stat item %s", VPCSTR (localpath));
		//CHECKME explain to user ?
//tag DEBUGfreeze;
//		OPENBGL
		return CANCEL;
	}

	//must prevent the default dialog response, it stuffs up Q cleanups
	CLOSEBGL
	rt.dialog = e2_dialog_create (NULL, NULL, _("file info"),
		DUMMY_RESPONSE_CB, NULL);
	OPENBGL
	dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt.dialog));
#else
		GTK_DIALOG (rt.dialog)->vbox;
#endif
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);

	rt.localpath = VPSTR (localpath);	//store data for copying
	s1 = g_filename_display_basename (VPSTR (localpath));
	s2 = g_markup_escape_text (s1, -1);
	g_string_printf (label_text, "<b>%s</b>", s2);
	e2_widget_add_mid_label (dialog_vbox, label_text->str, 0.5, TRUE, 0);
	g_free (s1);
	g_free (s2);
	e2_widget_add_separator (dialog_vbox, TRUE, E2_PADDING);
	mime = NULL;

	if (S_ISREG (statbuf.st_mode))
	{
		gchar *readable = NULL;
		//CHECKME
		if (statbuf.st_dev == 0 || statbuf.st_size > statbuf.st_blocks * statbuf.st_blksize)
		{
			s2 = _("Virtual file");
		}
		else
		{
			//for regular files, get type-data from 'file' command
			if (_e2_file_info_dialog_get_file_type (localpath, &readable, &mime)
				&& readable != NULL	//pipe open may succeed with bad option
				&& *readable != '\0')
					s2 = e2_utf8_from_locale (readable);	//freeme later
			else
			{
				if (readable != NULL)	//ensure s2 etc not freed later
				{
					g_free (readable);
					readable = NULL;
				}
				s2 = _("Unknown");
			}
		}

		GdkPixbuf *pixbuf = NULL;
		if (mime != NULL && *mime != '\0')
		{
			gchar *m2 = g_strdup (mime);
			//mime string returned by libmagic is of the form "text/plain; <stuff>"
			//from freedesktop.org, icon name is of the form media-subtype (e.g. text-plain).
			//If no specific icon is found, fallback to media-x-generic (e.g. text-x-generic).
			s1 = strchr (m2, ';');
			if (s1 != NULL)
				*s1 = '\0';
			s1 = strchr (m2, '/');
			if (s1 != NULL)
				*s1 = '-';
			GtkIconTheme *icon_theme = gtk_icon_theme_get_for_screen
				(gtk_widget_get_screen (app.main_window));
			gint psize = e2_icons_get_pixsize (GTK_ICON_SIZE_LARGE_TOOLBAR);
			pixbuf = gtk_icon_theme_load_icon (icon_theme, m2, psize,
				GTK_ICON_LOOKUP_GENERIC_FALLBACK | GTK_ICON_LOOKUP_USE_BUILTIN,
				NULL);
			if (pixbuf == NULL)
			{
				if (s1 != NULL)
				{
					*s1 = '\0';
					gchar *inm = g_strconcat (m2, "-x-generic", NULL);
					pixbuf = gtk_icon_theme_load_icon (icon_theme, inm, psize,
						GTK_ICON_LOOKUP_GENERIC_FALLBACK | GTK_ICON_LOOKUP_USE_BUILTIN,
						NULL);
					g_free (inm);
					if (pixbuf == NULL)
					{	//fallback to anything that looks half-reasonable !
						s1++;	//pass the \0
						GList *inames = gtk_icon_theme_list_icons (icon_theme, NULL);
						GList *member;
						for (member=inames; member != NULL; member = member->next)
						{
							inm = (gchar *)member->data;
		//					printd (DEBUG, "candidate name = %s", inm);
							if (strstr (inm, s1) != NULL)
							{
								pixbuf = gtk_icon_theme_load_icon (icon_theme, inm, psize,
									GTK_ICON_LOOKUP_GENERIC_FALLBACK | GTK_ICON_LOOKUP_USE_BUILTIN,
									NULL);
								break;
							}
						}
						e2_list_free_with_data (&inames);
					}
				}
			}
			g_free (m2);
		}
		if (pixbuf != NULL)
		{
			GtkWidget *image = gtk_image_new_from_pixbuf (pixbuf);
			GtkWidget *hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE,
				FALSE, E2_PADDING_LARGE);
			gtk_box_pack_start (GTK_BOX (hbox), image, FALSE, FALSE, 0);
			//CHECKME escape label ?
			rt.description =
			e2_widget_add_mid_label (hbox, s2, 0, TRUE, 0);
			g_object_unref (G_OBJECT (pixbuf));
		}
		else
		{
			g_string_printf (label_text, "%s %s", _("Type:"), s2);
			//CHECKME escape label ?
			rt.description = e2_widget_add_mid_label (dialog_vbox,
				label_text->str, 0, TRUE, 0);
		}
		if (readable != NULL)
		{
			g_free (readable);
			g_free (s2);
		}
	}
	//links to dirs reported as dirs by (e2_fs_is_dir3 (localpath))
	else if (S_ISDIR (statbuf.st_mode))
	{
		guint64 total;
		const gchar *dir =  _("Directory");

		total = 0;
		e2_fs_tw (localpath, _e2_file_info_dialog_count_twcb, &total, 1, E2TW_QT | E2TW_PHYS E2_ERR_NONE());
		if (total > 1)
		{
			gchar *countstr = _e2_file_info_dialog_prettify_number (total);
			total = 0;
			e2_fs_tw (localpath, _e2_file_info_dialog_usage_twcb, &total, -1, E2TW_PHYS E2_ERR_NONE());
			s1 = _e2_file_info_dialog_prettify_number (total);
			s2 = g_strdup_printf (
				_("%s containing %s item(s) which occupy %s byte(s)"), dir, countstr, s1);
			g_free (countstr);
			g_free (s1);
		}
		else
			s2 = g_strdup_printf ("%s - %s", dir, _("empty"));

		GtkWidget *image = e2_widget_get_icon (STOCK_NAME_DIRECTORY,
						GTK_ICON_SIZE_LARGE_TOOLBAR);
		if (image != NULL)
		{
			GtkWidget *hbox = e2_widget_add_box (dialog_vbox, TRUE, 0, FALSE,
				FALSE, E2_PADDING_LARGE);
			gtk_box_pack_start (GTK_BOX (hbox), image, FALSE, FALSE, 0);
			//CHECKME escape label ?
			rt.description =
			e2_widget_add_mid_label (hbox, s2, 0, TRUE, 0);
		}
		else
		{
			g_string_printf (label_text, "%s %s", _("Type:"), s2);
			rt.description =
			e2_widget_add_mid_label (dialog_vbox, label_text->str, 0, TRUE, 0);
		}
		g_free (s2);
		//TODO report the device holding the dir
	}
	else // not regular or directory
	{
		if (S_ISLNK (statbuf.st_mode))
			s2 = _("Symbolic Link");
		else if (S_ISCHR (statbuf.st_mode))
			s2 = _("Character Device");
		else if (S_ISBLK (statbuf.st_mode))
			s2 = _("Block Device");
		else if (S_ISFIFO (statbuf.st_mode))
			s2 = _("FIFO Pipe");
		else if (S_ISSOCK (statbuf.st_mode))
			s2 = _("Socket");
		else
			s2 = _("Unknown");

		g_string_printf (label_text, "%s %s", _("Type:"), s2);
		rt.description =
		e2_widget_add_mid_label (dialog_vbox, label_text->str, 0, TRUE, 0);
	}

	if (S_ISLNK (statbuf.st_mode))
	{
		gchar target[PATH_MAX];
		gchar *filename = g_strdup (VPSTR (localpath));
		gint len = strlen (filename) - sizeof(gchar);
		if (len > 0 && filename[len] == G_DIR_SEPARATOR)
			filename[len] = '\0';
#ifdef E2_VFS
		VPATH ddata = { filename, localpath->spacedata };
		len = e2_fs_readlink (&ddata, target, sizeof (target) E2_ERR_NONE());
#else
		len = e2_fs_readlink (filename, target, sizeof (target) E2_ERR_NONE());
#endif
		if (len > 0)
		{
			s1 = F_DISPLAYNAME_FROM_LOCALE (target);
			s2 = g_markup_escape_text (s1, -1);
			label_text = g_string_assign (label_text, " ");
			g_string_append_printf (label_text, _("to %s"), s2);
			F_FREE (s1, target);
			g_free (s2);
			//stat() fails for relative targets, so ...
			if (g_str_has_prefix (target, "."G_DIR_SEPARATOR_S))
			{
				memmove (target, (target + 2 * sizeof (gchar)), len - 1);
				len -= 2;
			}
			if (g_str_has_prefix (target, ".."G_DIR_SEPARATOR_S))
			{
				s1 = F_FILENAME_FROM_LOCALE (target);
#ifdef E2_VFSTMP
				//FIXME which CWD for the baseline
#else
				s2 = e2_utils_translate_relative_path (curr_view->dir, s1);
#endif
				F_FREE (s1, target);
				s1 = F_FILENAME_TO_LOCALE (s2);
				g_strlcpy (target, s1, sizeof(target));
				//the translation adds a trailing separator, which lstat assumes to be a dir match ..
				len = strlen (target);
				target[len-1] = '\0';
				F_FREE (s1, s2);
				g_free (s2);
			}
			//FIXME get intelligent absolute path
			if (!g_path_is_absolute (target))
			{
				gchar *local;
#ifdef E2_VFSTMP
				local = ; FIXME
#else
				local = D_FILENAME_TO_LOCALE (curr_view->dir); //always dup, to avoid dirchange race
#endif
				gchar *t2 = g_build_filename (local, target, NULL);
				g_strlcpy (target, t2, sizeof (target));
				g_free (local);
				g_free (t2);
			}
			E2_ERR_DECLARE
			//check the real target, without looking thru chained links
			struct stat statbuf2;	//don't disturb the size etc data for the link itelf
#ifdef E2_VFS
			ddata.path = target;
			if (e2_fs_lstat (&ddata, &statbuf2 E2_ERR_PTR()) && E2_ERR_IS (ENOENT))
#else
			if (e2_fs_lstat (target, &statbuf2 E2_ERR_PTR()) && E2_ERR_IS (ENOENT))
#endif
				g_string_append_printf (label_text, "\n %s",_("(which is <b>missing</b>)"));
			else if (!strcmp (localpath, target))
				g_string_append_printf (label_text, "\n %s",_("(which is <b>itself</b>)"));
			else
			{
				if (e2_fs_walk_link (&filename E2_ERR_NONE()))
				{
					printd (DEBUG, "final target is %s", filename);
					if (strcmp (filename, target))
					{
						s1 = D_FILENAME_TO_LOCALE (curr_view->dir); //always dup, to avoid dirchange race
#ifdef E2_VFS
						VPATH data2 = { s1, localpath->spacedata };
						ddata.path = filename;
						s2 = e2_utils_create_relative_path (&ddata, &data2);
#else
						s2 = e2_utils_create_relative_path (filename, s1);
#endif
						g_free (s1);
						s1 = F_FILENAME_FROM_LOCALE (s2);
						label_text = g_string_append (label_text, "\n ");
						g_string_append_printf (label_text, _("and ultimately to %s"), s1);
						F_FREE (s1, s2);
						g_free (s2);
					}
				}
			}

			E2_ERR_CLEAR

			rt.target = e2_widget_add_mid_label (dialog_vbox,
				label_text->str, 0, TRUE, 0);
		}
		else
			rt.target = e2_widget_add_mid_label (dialog_vbox,
				_("(Cannot resolve the link)"), 0, TRUE, 0);

		g_free (filename);
	}
	else
		rt.target = NULL;	//don't use this label when copying

	table = e2_widget_add_table (dialog_vbox, 10, 2, FALSE, TRUE, E2_PADDING);
#ifdef USE_GTK3_2
	gtk_grid_set_column_spacing (GTK_GRID (table), E2_PADDING);
#else
	gtk_table_set_col_spacings (GTK_TABLE (table), E2_PADDING);
#endif
	if (mime != NULL)
	{
		if (g_str_has_prefix (mime, "text/"))
		{
			s2 = strstr (mime, " charset");
			if (s2 != NULL)
			{
				s1 = g_utf8_strup (s2 + 9, -1);	//uppercase encoding
				if (*(s2-1) == ';')
					s2--;
				*s2 = '\0';
				s2 = g_strconcat (mime, "\n", s1, NULL);
				g_free (s1);
				s1 = g_strconcat (_("Mime:"), "\n", _("Encoding:"), NULL);
			}
			else
			{
				s1 = _("Mime:");
				s2 = mime;
			}
		}
		else
		{
			s1 = _("Mime:");
			s2 = mime;
		}
		e2_widget_add_mid_label_to_table (table, s1, 0.0, 0,1,0,1);
		rt.mime = e2_widget_add_mid_label_to_table (table, s2, 0.0, 1,2,0,1);
		if (s2 != mime)
		{
			g_free (s1);
			g_free (s2);
		}
	}
	else
		rt.mime = NULL;

	e2_widget_add_mid_label_to_table (table, _("Size:"), 0.0, 0,1,1,2);

	s1 = _e2_file_info_dialog_prettify_number (statbuf.st_size);
	g_string_printf (label_text, _("%s bytes"), s1);
	g_free (s1);

	rt.size =
	e2_widget_add_mid_label_to_table (table, label_text->str, 0.0, 1,2,1,2);

#ifdef USE_GTK3_2
	GtkWidget *sep = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
	gtk_grid_attach (GTK_GRID (table), sep, 0,2,2,1);
#else
	GtkWidget *sep = gtk_hseparator_new ();
	gtk_table_attach_defaults (GTK_TABLE (table), sep, 0,2,2,3);
#endif
	e2_widget_add_mid_label_to_table (table, _("User:"), 0.0, 0,1,3,4);
	if ((pw_buf = getpwuid (statbuf.st_uid)) != NULL)
		g_string_printf (label_text, "%s", pw_buf->pw_name);
	else
		g_string_printf (label_text, "%d", (guint) statbuf.st_uid);
	rt.user =
	e2_widget_add_mid_label_to_table (table, label_text->str, 0.0, 1,2,3,4);

	e2_widget_add_mid_label_to_table (table, _("Group:"), 0.0, 0,1,4,5);
	if ((grp_buf = getgrgid (statbuf.st_gid)) != NULL)
		g_string_printf (label_text, "%s", grp_buf->gr_name);
	else
		g_string_printf (label_text, "%d", (guint) statbuf.st_gid);
	rt.group =
	e2_widget_add_mid_label_to_table (table, label_text->str, 0.0, 1,2,4,5);
	gchar *perm_string = e2_fs_get_perm_string (statbuf.st_mode);
	label_text = g_string_assign (label_text, g_utf8_next_char (perm_string));	//skip the "type code" in 1st byte of string
	g_free (perm_string);
	g_string_insert_c (label_text, e2_utils_get_byte_position (label_text->str, 6), ' ');
	g_string_insert_c (label_text, e2_utils_get_byte_position (label_text->str, 3), ' ');
	e2_widget_add_mid_label_to_table (table, _("Permissions:"), 0.0, 0,1,5,6);
	rt.permissions =
	e2_widget_add_mid_label_to_table (table, label_text->str, 0.0, 1,2,5,6);

#ifdef USE_GTK3_2
	sep = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
	gtk_grid_attach (GTK_GRID (table), sep, 0,6,2,1);
#else
	sep = gtk_hseparator_new ();
	gtk_table_attach_defaults (GTK_TABLE (table), sep, 0,2,6,7);
#endif
	s1 = e2_option_str_get ("info-date-format");
	tm_ptr = localtime (&statbuf.st_atime);
	strftime (date_string, sizeof (date_string), s1, tm_ptr);
	s2 = e2_utf8_from_locale (date_string);
	e2_widget_add_mid_label_to_table (table, _("Accessed:"), 0.0, 0,1,7,8);
	rt.accessdate =
	e2_widget_add_mid_label_to_table (table, s2, 0.0, 1,2,7,8);
	e2_widget_set_safetip (rt.accessdate, _("Item was last opened, run, read etc"));
	g_free (s2);

	tm_ptr = localtime (&statbuf.st_mtime);
	strftime (date_string, sizeof (date_string), s1, tm_ptr);
	s2 = e2_utf8_from_locale (date_string);
	e2_widget_add_mid_label_to_table (table, _("Content modified:"), 0.0, 0,1,8,9);
	rt.moddate =
	e2_widget_add_mid_label_to_table (table, s2, 0.0, 1,2,8,9);
	e2_widget_set_safetip (rt.moddate, _("Item content was last changed"));
	g_free (s2);

	tm_ptr = localtime (&statbuf.st_ctime);
	strftime (date_string, sizeof (date_string), s1, tm_ptr);
	s2 = e2_utf8_from_locale (date_string);
	e2_widget_add_mid_label_to_table (table, _("Property changed:"), 0.0, 0,1,9,10);
	rt.changedate =
	e2_widget_add_mid_label_to_table (table, s2, 0.0, 1,2,9,10);
	e2_widget_set_safetip (rt.changedate,
		_("Item name, permission, owner, etc was last changed"));
	g_free (s2);

	g_string_free (label_text,TRUE);
	if (mime != NULL)
		g_free (mime);

//tag DEBUGfreeze2;
//	OPENBGL

	E2_Button local_btn;
	if (multi)
	{
		local_btn = E2_BUTTON_CANCEL;
		local_btn.showflags &= ~E2_BTN_DEFAULT;
		e2_dialog_add_defined_button (rt.dialog, &local_btn);
		e2_dialog_set_negative_response (rt.dialog, E2_RESPONSE_NOTOALL);
	}
	local_btn = E2_BUTTON_CLOSE;
	local_btn.showflags |= E2_BTN_DEFAULT;
	e2_dialog_add_defined_button (rt.dialog, &local_btn);
	//enable menu
	g_signal_connect (G_OBJECT (rt.dialog), "popup-menu",
		G_CALLBACK (_e2_file_info_dialog_popup_menu_cb), &rt);
	g_signal_connect (G_OBJECT (rt.dialog), "button-press-event",
		G_CALLBACK (_e2_file_info_dialog_button_press_cb), &rt);

//tag DEBUGfreeze
	CLOSEBGL
	e2_dialog_setup (rt.dialog, app.main_window);
	OPENBGL

	//block until user responds
	E2_DialogFlags flags = E2_DIALOG_BLOCKED | E2_DIALOG_CLOSELOCK | E2_DIALOG_FREE;
	if (multi)
		flags |= E2_DIALOG_MULTI;

	return (e2_dialog_run (rt.dialog, NULL, flags));
}

/**
@brief register config option related to file-info dialog
@return
*/
void e2_file_info_dialog_options_register (void)
{
	gchar *group_name = g_strconcat(_C(11),":",_("file information"),NULL); //_("dialogs:file information"
	e2_option_str_register ("info-date-format", group_name, _("displayed date/time format"),
		_("Pattern provided to strftime() to format date/time strings in the dialog"),
		NULL, "%a %d %b %Y %I:%M %p",
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
}
