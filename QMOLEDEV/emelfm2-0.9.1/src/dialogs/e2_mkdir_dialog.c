/* $Id: e2_mkdir_dialog.c 2815 2013-10-13 07:00:55Z tpgww $

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
//#include <unistd.h>
#include <string.h>
#include "e2_dialog.h"
#include "e2_mkdir_dialog.h"
#include "e2_task.h"
#include "e2_filelist.h"
#include "e2_icons.h"

//option-pointers shared by all mkdir dialogs in the session
//FIXME make sure these are NULL'd if optionsets are ever renewed
static E2_OptionSet *follow_pane = NULL;
static E2_OptionSet *suggest_dir = NULL;
static E2_OptionSet *show_last = NULL;
static E2_OptionSet *connected = NULL;

static GList *mkdir_history = NULL;

static gboolean _e2_mkdirdlg_change_dir_hook (gchar *path, E2_MkdirDialogRuntime *rt);
static gboolean _e2_mkdirdlg_change_focus_hook (E2_PaneRuntime *pane_rt, E2_MkdirDialogRuntime *rt);
static gboolean _e2_task_mkdirQ (E2_ActionTaskData *qed);

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief determine a suggested directory name, by appending or incrementing a number

@param dir last combo box entry, ie UTF-8, maybe NULL if no entry
@param parent_dir is rt->path, UTF-8
@return newly-allocated UTF-8 string
*/
static gchar *_e2_mkdirdlg_find_dir (const gchar *dir, const gchar *parent_dir)
{
	//we have to suggest something, but if there is nothing
	//in the history yet, we use a default string
	if (dir == NULL)
		dir = _("new directory");

#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#endif
	//the absolute directory name consists of 4 parts:
	//parent directory if the combobox string is not an absolute path
	gchar *part1 = (g_path_is_absolute (dir)) ? "" : (gchar *)parent_dir;
	//part of the combobox string that's a relative path like "bar/" from "bar/foo"
	gchar *part2 = g_path_get_dirname (dir);
	if (!strcmp (part2, "."))
	{	//no path entered
		g_free (part2);
		part2 = g_strdup ("");
	}
//#warning ignore compiler warning about unitialized usage of digit_end
//#warning ignore compiler warning about unitialized usage of part4
	//the base name of the dir
	gchar *part3 = g_path_get_basename (dir);
	//last part to hold the rest of part 3 after we find a number, if any
	//ignore warning about uninitialized usage
	gchar *part4 = NULL;	//assignment for complier-warning prevention only
	//index (chars, not bytes) of the start of a number in part3
	glong digit_start = -1;
	glong digit_end = 0;	//assignment for complier-warning prevention only
	glong digit_len = 1;
	//digit to increment or add to the directory name
	guint i = 0;
	//complete path (all utf8)
	gchar *path = g_build_filename (part1, part2, part3, NULL);

	gchar *local = F_FILENAME_TO_LOCALE (path);
#ifdef E2_VFS
	VPATH ddata = { local, curr_view->spacedata };
	if (!e2_fs_access (&ddata, F_OK E2_ERR_NONE()))	//through links
#else
	if (!e2_fs_access (local, F_OK E2_ERR_NONE()))	//through links
#endif
	{
		//the directory in the combo entry already exists,
		//check (backwards through the name) whether the name includes a number
		glong j = g_utf8_strlen (part3, -1) - 1;
		gboolean last_was_digit = FALSE;
		gchar *p;
		gunichar c;
		while (j >= 0)
		{
			p = g_utf8_offset_to_pointer (part3, j);
			c = g_utf8_get_char (p);
			if (g_unichar_isdigit (c))
			{
				if (!last_was_digit)
				{
					last_was_digit = TRUE;
					digit_end = j;
				}
//				digit_start = j;
				if (j == 0)	//no chance of a preceding non-digit
					digit_start = 0;
			}
			else if (last_was_digit)
			{
				digit_start = j + 1;
				break;
			}
			j--;
		}
		if (digit_start != -1)
		{
			//there is a number, grab it and otherwise carve up the entered name
			digit_len = digit_end - digit_start + 1;
			p = g_utf8_offset_to_pointer (part3, digit_end + 1);
			part4 = g_strdup (p);
			*p ='\0';
			p = g_utf8_offset_to_pointer (part3, digit_start);
			i = (guint) atoi (p);
			*p = '\0';
		}

		gchar numfmt[20];
		numfmt[0] = '%';
		if (digit_len > 1)
			g_snprintf (numfmt+1, sizeof(numfmt)-1, "0%luu", digit_len);
		else
			g_strlcpy (numfmt+1, "u", sizeof(numfmt)-1);
		//if there is no number in the directory string, we simply add it
		//to the end
		gchar *freeme;
		gchar *format = g_strconcat (
				"%s", //(*part1 == '\0') ? "" : G_DIR_SEPARATOR_S,
				"%s", (*part2 == '\0') ? "" : G_DIR_SEPARATOR_S,
				"%s",	//part3 always has something useful
				 numfmt, NULL);
		//otherwise, add a trailing token for part4
		if (digit_start != -1)
		{
			freeme = format;
			format = g_strconcat (freeme, "%s", NULL);
			g_free (freeme);
		}
		//now check for a non-existent item with an incremented number
#ifdef E2_VFS
		while (!e2_fs_access (&ddata, F_OK E2_ERR_NONE()))
#else
		while (!e2_fs_access (local, F_OK E2_ERR_NONE()))
#endif
		{
			g_free (path);
			F_FREE (local, path);
			i++;
			if (digit_start != -1)
				path = g_strdup_printf (format, part1, part2, part3, i, part4);
			else
				path = g_strdup_printf (format, part1, part2, part3, i);
			local = F_FILENAME_TO_LOCALE (path);
		}
		g_free (format);
		g_free (path);
		F_FREE (local, path);

		//now build the string for the entry
		//from the the string the user really entered
		if (digit_start != -1)
		{
			format = g_strconcat ("%s%s", numfmt, "%s", NULL);
			path = g_strdup_printf (format, part2, part3, i, part4);
		}
		else
		{
			format = g_strconcat ("%s%s", numfmt, NULL);
			path = g_strdup_printf (format, part2, part3, i);
		}
		g_free (format);
	}
	else
	{
		g_free (path);
		path = g_strdup (dir);
	}
	//cleanup
	g_free (part2);
	g_free (part3);

	return path;
}
/**
@brief walk up the path of @a parent until an existing directory is found

@param parent path of dir to check, freeable UTF-8 string
@param exists store for flag indicating that directory @a parent was not found
@param i store for no. of path segments scanned
@return UTF-8 string, @a parent or newly-allocated substitute
*/
static gchar *_e2_mkdirdlg_real_parent (gchar *parent, gboolean *exists, gint *i)
{
#ifdef E2_VFSTMP
//FIXME handle vfs parent
#endif
	gchar *local = F_FILENAME_TO_LOCALE (parent);
#ifdef E2_VFS
	VPATH ddata = { local, curr_view->spacedata };
	while (e2_fs_access (&ddata, F_OK E2_ERR_NONE()) || ! e2_fs_is_dir3 (&ddata E2_ERR_NONE()))
#else
	while (e2_fs_access (local, F_OK E2_ERR_NONE()) || ! e2_fs_is_dir3 (local E2_ERR_NONE()))
#endif
	{
		(*i)++;
		*exists = FALSE;
		if ((parent[0] == G_DIR_SEPARATOR) && (parent[1] == '\0'))
			break;
		F_FREE (local, parent);
		gchar *freeme = parent;
		parent = g_path_get_dirname (parent);
		local = F_FILENAME_TO_LOCALE (parent);
#ifdef E2_VFS
		ddata.path = local;
#endif
		g_free (freeme);
	}
	F_FREE (local, parent);
	return parent;
}
/**
@brief change size of a mkdir dialog window
This is called from several places, all with BGL closed
@param rt pointer to dialog data struct
@return
*/
static void _e2_mkdirdlg_update_dialog_size (E2_MkdirDialogRuntime *rt)
{
	WAIT_FOR_EVENTS
	GtkAllocation alloc;
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (rt->dialog, &alloc);
#else
	alloc = rt->dialog->allocation;
#endif
#ifdef USE_GTK2_20
	GtkRequisition req;
# ifdef USE_GTK3_0
	gtk_widget_get_preferred_size (rt->dialog, NULL, &req);
# else
	gtk_widget_get_requisition (rt->dialog, &req);
# endif
	if (alloc.width != req.width || alloc.height != req.height)
#else
	//this way is not so reliable
	if ((alloc.width != rt->dialog->requisition.width) ||
		(alloc.height != rt->dialog->requisition.height))
#endif
	{
		//suspend scrollers so that resizing works as intended
		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (rt->scrolled),
			GTK_POLICY_NEVER, GTK_POLICY_NEVER);
//		WAIT_FOR_EVENTS
#ifdef USE_GTK2_20
# ifdef USE_GTK3_0
		gtk_widget_get_preferred_size (rt->dialog, NULL, &req);
# else
		gtk_widget_size_request (rt->dialog, &req);//CHECKME maybe different now ?
# endif
		//+20 wide seems needed for vertical scroller
		if (req.width > 0 && req.height > 0)
			gtk_window_resize (GTK_WINDOW (rt->dialog), req.width + 20, req.height);
		else
			printd (DEBUG, "Bad dialog size %d X %d", req.width, req.height);
#else
		if (rt->dialog->requisition.width > 0 && rt->dialog->requisition.height > 0)
			gtk_window_resize (GTK_WINDOW (rt->dialog),
				rt->dialog->requisition.width + 20, rt->dialog->requisition.height);
		else
			printd (DEBUG, "Bad dialog size %d X %d",
				rt->dialog->requisition.width, rt->dialog->requisition.height);
#endif
//		WAIT_FOR_EVENTS
		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (rt->scrolled),
			GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
	}
}
/**
@brief update dialog label which describes the parent path
This is called from _e2_mkdirdlg_update_status() (inside BGL)
@param path parent directory path, UTF-8 string
@param rt pointer to dialog data struct
@return
*/
static void _e2_mkdirdlg_update_parent_label (gchar *path, E2_MkdirDialogRuntime *rt)
{
	printd (DEBUG, "mkdir dialog update parent label");
#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#else
	gchar *translated = e2_utils_translate_relative_path (rt->path, path);
#endif
	gchar *public = g_markup_escape_text (translated, -1);
	gchar *label = g_strconcat ("<small>", public, "</small>", NULL);
	gtk_label_set_markup (GTK_LABEL (rt->info_label), label);
	g_free (translated);
	g_free (public);
	g_free (label);
}
/**
@brief update dialog label which describes creation, and also buttons' sensitivity
This is called from _e2_mkdirdlg_update_status() (inside BGL)
@param reason explanation string, or NULL
@param rt pointer to dialog data struct
@return
*/
static void _e2_mkdirdlg_update_creation_widgets (gchar *reason, E2_MkdirDialogRuntime *rt)
{
	gchar *color;
	gchar *constant;
	gchar *label;

	printd (DEBUG, "mkdir dialog update creation widgets");
	if (rt->creation_possible)
	{
		color = e2_option_str_get ("color-positive");
		constant = _("yes");
	}
	else
	{
		color = e2_option_str_get ("color-negative");
		constant = _("no");
	}
	label = g_strconcat ("<span weight=\"bold\" size=\"small\" foreground=\"",
			color, "\">", constant, " </span>", reason == NULL ? NULL : "<small>(",
			reason, ")</small>", NULL);
	gtk_label_set_markup (GTK_LABEL (rt->info_label2), label);
	g_free (label);

	//FIXME gtk stops repeated clicking of create button without leaving it
	//if the E2_RESPONSE_CREATE sensitivity or corresponding button
	//sensitivity is ever changed either way
	gtk_dialog_set_response_sensitive (GTK_DIALOG (rt->dialog), E2_RESPONSE_MORE, rt->creation_possible);
	gtk_dialog_set_response_sensitive (GTK_DIALOG (rt->dialog), GTK_RESPONSE_YES, rt->creation_possible);
}
/**
@brief update the two info labels in the info frame, and buttons' sensitivity
This is done even if the info frame isn't shown, to update the
rt->creation_possible state and buttons
It must be called only from inside BGL as no thread protection is done
@param rt pointer to dialog's data struct
@return
*/
static void _e2_mkdirdlg_update_status (E2_MkdirDialogRuntime *rt)
{
	printd (DEBUG, "mkdir dialog update status");
	gboolean absolute;
	gint len;
	gchar *path, *parent;
	E2_ERR_DECLARE

	rt->creation_possible = FALSE;

	const gchar *dirname = gtk_entry_get_text (
#ifdef USE_GTK2_14
		GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->combo)))
#else
		GTK_ENTRY (GTK_BIN (rt->combo)->child)
#endif
	);
	printd (DEBUG, "dir name entry text is now '%s'", dirname);
	//if the name-entry is empty, we'll only check if the parent directory is
	//writable, so, set path to the currently active directory
	if (*dirname == '\0')
	{
		absolute = TRUE;
		path = g_strdup (rt->path);
		len = strlen (path);
		if (len > 1 && *(path + len - 1) == G_DIR_SEPARATOR)
			*(path + len - 1) = '\0';
		parent = g_strdup (path);	//CHECKME
	}
	else //otherwise we have to create an absolute path from the active
		 //directory and the entry text
	{
		absolute = g_path_is_absolute ((gchar *) dirname);
		path = e2_utils_translate_relative_path (rt->path, (gchar *) dirname);
		len = strlen (path);
		if (len > 1 && *(path + len - 1) == G_DIR_SEPARATOR)
			*(path + len - 1) = '\0';
		parent = g_path_get_dirname (path);
	}

	gboolean parent_exists = TRUE;
	//number of "ancestor" dirs that need to be created
//	gint num = g_str_has_suffix (path, G_DIR_SEPARATOR_S) ? 0 : 1;
	gint num = 1;
	parent = _e2_mkdirdlg_real_parent (parent, &parent_exists, &num);
	_e2_mkdirdlg_update_parent_label (parent, rt);

	gchar *reason = NULL;
	if (*dirname == '\0')
	{
		_e2_mkdirdlg_update_creation_widgets (reason, rt);
		g_free (path);
		g_free (parent);
		return;
	}
	gchar *local = F_FILENAME_TO_LOCALE (parent);
#ifdef E2_VFS
	VPATH ddata = { local, curr_view->spacedata };
	if ((e2_fs_access (&ddata, W_OK | X_OK E2_ERR_PTR())) != 0)
#else
	if ((e2_fs_access (local, W_OK | X_OK E2_ERR_PTR())) != 0)
#endif
	{
		if (absolute)
			reason = g_strdup_printf (_("cannot write to '%s' - %s"), parent,
#ifdef E2_VFS
			E2_ERR_NAME->message
#else
			g_strerror (errno)
#endif
		);
		else if (parent_exists)
			reason = g_strdup_printf (_("cannot write to parent directory - %s"),
#ifdef E2_VFS
				E2_ERR_NAME->message
#else
				g_strerror (errno)
#endif
		);
		else
			reason = g_strdup_printf (_("only '%s' exists - %s"), parent,
#ifdef E2_VFS
				E2_ERR_NAME->message
#else
				g_strerror (errno)
#endif
		);
		_e2_mkdirdlg_update_creation_widgets (reason, rt);

		E2_ERR_CLEAR
		g_free (reason);
		g_free (path);
		g_free (parent);
		F_FREE (local, parent);
		return;
	}
	F_FREE (local, parent);
	local = F_FILENAME_TO_LOCALE (path);
#ifdef E2_VFS
	ddata.path = local;
	if (!e2_fs_access (&ddata, F_OK E2_ERR_NONE()))
#else
	if (!e2_fs_access (local, F_OK E2_ERR_NONE()))
#endif
	{
#ifdef E2_VFS
		if  (e2_fs_is_dir3 (&ddata E2_ERR_NONE()))
#else
		if  (e2_fs_is_dir3 (local E2_ERR_NONE()))
#endif
			reason = _("the directory already exists");
		else
			reason = _("something is in the way");
		_e2_mkdirdlg_update_creation_widgets (reason, rt);
		F_FREE (local, path);
		g_free (path);
		g_free (parent);
		return;
	}
	gboolean warn = (strchr (local, '?') != NULL || strchr (local, '*') != NULL);
	F_FREE (local, path);

	if (warn)
		reason = _("but it's unwise to use wildcard char(s)");
	if (num > 1)
	{
		const gchar *r2 = _("directories will be created");
		reason = (warn) ?
			g_strdup_printf ("%s\n%d %s", reason, num, r2):
			g_strdup_printf ("%d %s", num, r2);
	}
	rt->creation_possible = TRUE;
	_e2_mkdirdlg_update_creation_widgets (reason, rt);
	if (num > 1)
		g_free (reason);
	g_free (parent);
	g_free (path);
}
/**
@brief update suggested name for the next mkdir
This is called from several places, all with BGL closed
@param rt pointer to dialog data struct
@return
*/
static void _e2_mkdirdlg_update_name (E2_MkdirDialogRuntime *rt)
{
	printd (DEBUG, "mkdir dialog update name");
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	if (rt->opt_suggest_dir || rt->opt_show_last)
	{
		gchar *text = e2_combobox_first_text (GTK_COMBO_BOX (rt->combo));
		if (text != NULL)
		{
			if (rt->opt_suggest_dir)
			{
				gchar *new_text = _e2_mkdirdlg_find_dir (text, rt->path);
				gtk_entry_set_text (GTK_ENTRY (entry), new_text);
				g_free (new_text);
			}
			g_free (text);
			gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
		}
		else
			gtk_entry_set_text (GTK_ENTRY (entry), "");
	}
	else
		gtk_entry_set_text (GTK_ENTRY (entry), "");

#ifdef RACE_CHECK
	printd (DEBUG, "mkdir update combo completed");
#endif
}
/**
@brief set or clear mechanism for following changes of active directory
If @a follow is TRUE, the default path is updated and status info etc updated
accordingly.
This is called with BGL closed.
@param rt pointer to dialog data struct
@param follow TRUE to turn on following, FALSE to turn it off
@return
*/
static void _e2_mkdirdlg_update_follow_dir (E2_MkdirDialogRuntime *rt, gboolean follow)
{
	printd (DEBUG, "mkdir dialog update dir-following");
	if (follow)
	{
		g_free (rt->path);
		rt->path = g_strdup (curr_pane->path);
		_e2_mkdirdlg_update_status (rt);
#ifdef E2_VFSTMP
	//CHECKME hooks for non-mounted dirs
#endif
		e2_hook_register (&app.pane1.hook_change_dir, (HookFunc)_e2_mkdirdlg_change_dir_hook, rt);
		e2_hook_register (&app.pane2.hook_change_dir, (HookFunc)_e2_mkdirdlg_change_dir_hook, rt);
		e2_hook_register (&app.hook_pane_focus_changed, (HookFunc)_e2_mkdirdlg_change_focus_hook, rt);
	}
	else
	{
		e2_hook_unregister (&app.pane1.hook_change_dir, (HookFunc)_e2_mkdirdlg_change_dir_hook, rt, TRUE);
		e2_hook_unregister (&app.pane2.hook_change_dir, (HookFunc)_e2_mkdirdlg_change_dir_hook, rt, TRUE);
		e2_hook_unregister (&app.hook_pane_focus_changed, (HookFunc)_e2_mkdirdlg_change_focus_hook, rt, TRUE);
	}
}
/**
@brief hook function for app.paneX.hook_change_dir
This is initiated from cd thread, with BGL off/open
@param path path of an opened directory, UTF-8 string
@param rt pointer to dialog's data struct
@return TRUE
*/
static gboolean _e2_mkdirdlg_change_dir_hook (gchar *path, E2_MkdirDialogRuntime *rt)
{
	g_free (rt->path);
	rt->path = g_strdup (path);
	CLOSEBGL
	_e2_mkdirdlg_update_status (rt);
	_e2_mkdirdlg_update_dialog_size (rt);
	OPENBGL
	return TRUE;
}
/**
@brief hook function for app.hook_pane_focus_changed
This is called with BGL off/open
@param pane_rt data struct for the currenly-focused pane
@param rt pointer to dialog's data struct
@return TRUE
*/
static gboolean _e2_mkdirdlg_change_focus_hook (E2_PaneRuntime *pane_rt, E2_MkdirDialogRuntime *rt)
{
	g_free (rt->path);
#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#else
	LISTS_LOCK
	rt->path = g_strdup (pane_rt->path);
	LISTS_UNLOCK
#endif
	CLOSEBGL
	_e2_mkdirdlg_update_status (rt);
	_e2_mkdirdlg_update_dialog_size (rt);
	OPENBGL
	return TRUE;
}
/**
@brief create a new directory, along with any missing "ancestor(s)", in the active-pane dir
The string provided may be an absolute or relative path, the latter including
just a name. If relative, the last-used CWD (rt->path) will be prepended.
This is called only from _e2_mkdirdlg_response_cb() with BGL closed
@param entry the entry widget for the name-entry combo
@param rt pointer to dialog's data struct
@param close TRUE if the dialog will be closed when finished here

@return
*/
static void _e2_mkdirdlg_create_dir (GtkWidget *entry, E2_MkdirDialogRuntime *rt,
	gboolean close)
{
	printd (DEBUG, "mkdir dialog create dir");
	if (!rt->creation_possible)
		return;
	const gchar *dir = gtk_entry_get_text (GTK_ENTRY (entry));
	//quick exit
	if (*dir == '\0')
		return;

	gchar *path, *parent1, *parent, *local, *freeme;
	path = e2_utils_translate_relative_path (rt->path, (gchar *) dir);

	//find closest existing "ancestor" of the dir to be created, and any
	//intervening ancestor dir(s) that need to be created
	parent1 = g_path_get_dirname (path);
	if (g_str_has_suffix (path, G_DIR_SEPARATOR_S))
	{
		freeme = parent1;
		parent1 = g_path_get_dirname (parent1);
		g_free (freeme);
	}
	freeme = local = F_FILENAME_TO_LOCALE (parent1);
	parent = parent1;

	GList *missing = NULL;
#ifdef E2_VFS
	VPATH ddata = { local, curr_view->spacedata };
	while (e2_fs_access (&ddata, F_OK E2_ERR_NONE()) || !e2_fs_is_dir3 (&ddata E2_ERR_NONE()))
#else
	while (e2_fs_access (local, F_OK E2_ERR_NONE()) || !e2_fs_is_dir3 (local E2_ERR_NONE()))
#endif
	{
		if ((parent[0] == G_DIR_SEPARATOR) && (parent[1] == '\0'))
		{
			if (parent != parent1)
				g_free (parent);
			break;
		}
		missing = g_list_prepend (missing, g_strdup(local));
#ifdef E2_VFSTMP
	//FIXME parent path for virtual dirs
#endif
		local = g_path_get_dirname (parent);
		if (parent != parent1)
			g_free (parent);
		parent = local;
#ifdef E2_VFS
		ddata.path = local;
#endif
	}
	g_free (parent1);
	F_FREE (freeme, parent1);

	E2_ERR_DECLARE
	GList *tmp;
	//create any missing ancestor(s)
	for (tmp = missing; tmp != NULL; tmp = g_list_next (tmp))
	{
		local = (gchar *)tmp->data;
//		E2_ERR_NAME = NULL;
#ifdef E2_VFS
		ddata.path = local;
		if (e2_fs_mkdir (&ddata, 0777 E2_ERR_PTR()))	//FIXME vfs
#else
		if (e2_fs_mkdir (local, 0777 E2_ERR_PTR()))	//FIXME vfs
#endif
		{
			OPENBGL //downstream does local mutex management
			e2_fs_error_local (_("Cannot create directory %s"),
#ifdef E2_VFS
				&ddata E2_ERR_MSGL());
#else
				local E2_ERR_MSGL());
#endif
			CLOSEBGL
			E2_ERR_CLEAR

			if (!close)
				_e2_mkdirdlg_update_status (rt);
#ifdef E2_VFSTMP
			//FIXME path for non-mounted dirs
#else
			e2_filelist_request_refresh (rt->path, TRUE);
#endif
			e2_list_free_with_data (&missing);
			return;
		}
	}
	e2_list_free_with_data (&missing);

	//now create the lowest-level dir
	local = F_FILENAME_TO_LOCALE (path);
#ifdef E2_VFS
	ddata.path = local;
	if (!e2_fs_mkdir (&ddata, 0777 E2_ERR_PTR()))
#else
	if (!e2_fs_mkdir (local, 0777 E2_ERR_PTR()))
#endif
	{	//succeeded
		if (!close)
		{
			_e2_mkdirdlg_update_name (rt);
			_e2_mkdirdlg_update_status (rt);
		}
#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#else
		e2_filelist_request_refresh (rt->path, TRUE);
#endif
		e2_list_update_history (&mkdir_history, dir, NULL, 30, FALSE);
	}
	else	//mkdir failed
	{
		OPENBGL //downstream does local mutex management
		e2_fs_error_local (_("Cannot create directory %s"),
#ifdef E2_VFS
			&ddata E2_ERR_MSGL());
#else
			local E2_ERR_MSGL());
#endif
		CLOSEBGL
		E2_ERR_CLEAR

		if (!close)
			_e2_mkdirdlg_update_status (rt);
	}
	g_free (path);
	F_FREE (local, path);
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief dialog response callback

@param dialog the dialog where the response was initiated
@param response the response assigned to the activated button
@param rt pointer to dialog's data struct
@return
*/
static void _e2_mkdirdlg_response_cb (GtkDialog *dialog, gint response,
	E2_MkdirDialogRuntime *rt)
{
	printd (DEBUG, "mkdir dialog response_cb (dialog:_,response:%d,rt:_)", response);
	GtkWidget *entry;

	switch (response)
	{
		case GTK_RESPONSE_YES:
			if (rt->idle_id > 0)
				g_source_remove (rt->idle_id);	//no more UI-update callbacks when we're finishing
		case E2_RESPONSE_MORE:
			entry =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->combo));
#else
				GTK_BIN (rt->combo)->child;
#endif
			NEEDCLOSEBGL
			const gchar *dir = gtk_entry_get_text (GTK_ENTRY (entry));
			if (dir != NULL && *dir != '\0')
			{
				if (strchr (dir, '%') != NULL)
				{
					gchar *freeme = e2_utils_expand_macros ((gchar *)dir, NULL);
					if (freeme != NULL && freeme != GINT_TO_POINTER (1))
					{
						gtk_entry_set_text (GTK_ENTRY (entry), freeme);
						g_free (freeme);
						dir = gtk_entry_get_text (GTK_ENTRY (entry));
					}
				}
//				*rt->status = E2_TASK_RUNNING;
				if (response == E2_RESPONSE_MORE)
				{
					e2_combobox_prepend_history (rt->combo, dir, 20, FALSE);
					_e2_mkdirdlg_create_dir (entry, rt, FALSE);
//					*rt->status = E2_TASK_PAUSED;
					gtk_widget_grab_focus (entry);
					//prevent the dialog being closed by the other cb
					g_signal_stop_emission_by_name ((gpointer)dialog, "response");
				}
				else
					_e2_mkdirdlg_create_dir (entry, rt, TRUE);
			}
			NEEDOPENBGL
		default:
			break;
	}
}
/**
@brief idle callback to update dialog UI after any change of the name-entry contents
@param rt pointer to dialog's data struct
@return FALSE after update done
*/
static gboolean _e2_mkdirdlg_deferred_change_cb (E2_MkdirDialogRuntime *rt)
{
	rt->idle_id = 0;	//allow sequential changes ASAP
	if (GTK_IS_DIALOG (rt->dialog))
	{
		CLOSEBGL
		_e2_mkdirdlg_update_status (rt);
		_e2_mkdirdlg_update_dialog_size (rt);
		OPENBGL
	}
	return FALSE;
}
/**
@brief directory-name entry "changed" signal callback, initiates a GUI update
This is also called every time a letter is added to or removed from the dialog's
name-entry. So GUI update is deferred to idle time, to help eliminate keypress latency
@param editable UNUSED the editable of the affected combobox-entry
@param rt pointer to dialog's data struct
@return
*/
static void _e2_mkdirdlg_changed_cb (GtkEditable *editable, E2_MkdirDialogRuntime *rt)
{
//	printd (DEBUG, "mkdir dialog changed cb");
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	if (rt->idle_id == 0)	//don't want re-entrance
		rt->idle_id = g_idle_add ((GSourceFunc)_e2_mkdirdlg_deferred_change_cb, rt);
//		rt->idle_id = g_idle_add_full (G_PRIORITY_HIGH,
//			(GSourceFunc)_e2_mkdirdlg_deferred_change_cb, rt, NULL);
//	else
//		printd (DEBUG, " idle callback pending already");
}
/**
@brief dialog "button-press-event" signal callback, to pop up a context menu
@param dialog UNUSED the widget which received the mouse button press
@param event pointer to event data struct
@param menu pointer to context menu for the dialog
@return TRUE if the event was a right-button click
*/
static gboolean _e2_mkdirdlg_button_press_cb (GtkWidget *dialog,
	GdkEventButton *event, GtkWidget *menu)
{
	if (event->button == 3 && event->type == GDK_BUTTON_PRESS
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		NEEDCLOSEBGL
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 3, event->time);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief combobox entry "activate" callback
This simply triggers a 'create' type response
@param entry UNUSED the entry widget for the name-entry combo
@param rt pointer to dialog's data struct
@return
*/
static void _e2_mkdirdlg_activated_cb (GtkWidget *entry, E2_MkdirDialogRuntime *rt)
{
	printd (DEBUG, "mkdir dialog activate cb");
//	NEEDOPENBGL
	g_signal_emit_by_name (G_OBJECT (rt->dialog), "response", GTK_RESPONSE_YES, rt); //trigger all callbacks
//	NEEDCLOSEBGL
}
/**
@brief callback for context menu item for toggling "follow active pane"
@param menu_item the activated widget
@param rt pointer to dialog's data struct
@return
*/
static void _e2_mkdirdlg_toggled1_cb (GtkWidget *menu_item, E2_MkdirDialogRuntime *rt)
{
	printd (DEBUG, "mkdir dialog follow pane toggle");
	gboolean state = gtk_check_menu_item_get_active
		(GTK_CHECK_MENU_ITEM (menu_item));
	NEEDCLOSEBGL
	_e2_mkdirdlg_update_follow_dir (rt, state);
	NEEDOPENBGL
	gpointer p = g_object_get_data (G_OBJECT (rt->dialog), "e2-controller-blocked");
	if (!GPOINTER_TO_INT (p))
		e2_option_bool_set_direct (follow_pane, state);
}
/**
@brief callback for context menu item for toggling "show last entry"
@param menu_item the activated widget
@param rt pointer to dialog's data struct
@return
*/
static void _e2_mkdirdlg_toggled2_cb (GtkWidget *menu_item, E2_MkdirDialogRuntime *rt)
{
	printd (DEBUG, "mkdir dialog show last entry toggle");
	rt->opt_show_last = gtk_check_menu_item_get_active
		(GTK_CHECK_MENU_ITEM (menu_item));
	NEEDCLOSEBGL
	_e2_mkdirdlg_update_name (rt);
	_e2_mkdirdlg_update_status (rt);
	NEEDOPENBGL
	gpointer p = g_object_get_data (G_OBJECT (rt->dialog), "e2-controller-blocked");
	if (!GPOINTER_TO_INT (p))
		e2_option_bool_set_direct (show_last, rt->opt_show_last);
}
/**
@brief callback for context menu item for toggling "suggest dir"
@param menu_item the activated widget
@param rt pointer to dialog's data struct
@return
*/
static void _e2_mkdirdlg_toggled3_cb (GtkWidget *menu_item, E2_MkdirDialogRuntime *rt)
{
	printd (DEBUG, "mkdir dialog suggest dir toggle");
	rt->opt_suggest_dir = gtk_check_menu_item_get_active
		(GTK_CHECK_MENU_ITEM (menu_item));
	NEEDCLOSEBGL
	_e2_mkdirdlg_update_name (rt);
	_e2_mkdirdlg_update_status (rt);
	NEEDOPENBGL
	gpointer p = g_object_get_data (G_OBJECT (rt->dialog), "e2-controller-blocked");
	if (!GPOINTER_TO_INT (p))
		e2_option_bool_set_direct (suggest_dir, rt->opt_suggest_dir);
}
/**
@brief callback for info expander opened or closed
@param expander the activated widget
@param spec
@param rt pointer to dialog's data struct
@return
*/
static void _e2_mkdirdlg_expander_toggled_cb (GtkWidget *expander,
	GParamSpec *spec, E2_MkdirDialogRuntime *rt)
{
//	printd (DEBUG, "mkdir dialog expander toggle");
	NEEDCLOSEBGL
	gboolean value = gtk_expander_get_expanded (GTK_EXPANDER (expander));
	if (value)
	{
#ifdef USE_GTK2_18
		if (!gtk_widget_get_visible (rt->info_box))
#else
		if (!GTK_WIDGET_VISIBLE (rt->info_box))
#endif
			gtk_widget_show (rt->info_box);
	}
	else
#ifdef USE_GTK2_18
		if (gtk_widget_get_visible (rt->info_box))
#else
		if (GTK_WIDGET_VISIBLE (rt->info_box))
#endif
		gtk_widget_hide (rt->info_box);

	_e2_mkdirdlg_update_dialog_size (rt);

	NEEDOPENBGL

	GtkWidget *controller = g_object_get_data (G_OBJECT (expander),
			"e2-controller-widget");
	if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (controller),
			"e2-controller-blocked")))
	{
        E2_OptionSet *showoption = g_object_get_data (G_OBJECT (expander), "e2-show-option");
		e2_option_bool_set_direct (showoption, value);
	}
}
/**
@brief set @a expander state if it's not blocked and not already at the desired state
This is a hook-function callack upon change of set data associated with @a expander
@param state pointerised TRUE/FALSE, the value to apply to @a expander
@param expander the widget to change

@return TRUE always
*/
static gboolean _e2_mkdirdlg_set_expander_hook (gpointer state, GtkWidget *expander)
{
	GtkWidget *controller = g_object_get_data (G_OBJECT (expander),
		"e2-controller-widget");
	if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (controller),
		"e2-controller-blocked")))
	{
		gboolean value = GPOINTER_TO_INT (state);
		gboolean current = gtk_expander_get_expanded (GTK_EXPANDER (expander));
		if (value != current)
			gtk_expander_set_expanded (GTK_EXPANDER (expander), value);
	}
	return TRUE;
}

  /******************/
 /***** action *****/
/******************/
/**
@brief create and show a mkdir dialog

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_mkdir_dialog_create (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_MKDIR, art, from,
		_e2_task_mkdirQ, NULL));
}
static gboolean _e2_task_mkdirQ (E2_ActionTaskData *qed)
{
	E2_MkdirDialogRuntime rt;

	rt.history = e2_list_copy_with_data (mkdir_history);
#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#else
	rt.path = D_FILENAME_FROM_LOCALE (qed->currdir);
#endif
	rt.status = qed->status;	//enable on-the-fly status changes
	rt.creation_possible = TRUE;
	rt.idle_id = 0;	//value allows initial idle to be applied

//tag DEBUGfreeze;

	//create dialog
	CLOSEBGL
	rt.dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION,
		_("What is the new directory's name?"), _("create directory"),
		(ResponseFunc)_e2_mkdirdlg_response_cb, &rt);

	GtkWidget *vbox = e2_dialog_add_sw (rt.dialog);

	//cuz E2_COMBOBOX_MENU_STYLE flag is not set, on gtk2, downstream calls
	//gtk_widget_set_name() which MAY? need BGL closed
	rt.combo = e2_combobox_add (vbox, FALSE, E2_PADDING,
		(ActivateFunc)_e2_mkdirdlg_activated_cb, &rt, &mkdir_history,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_FOCUS_ON_CHANGE);
#ifndef USE_GTK3_0
	OPENBGL
#endif

	rt.scrolled = gtk_widget_get_ancestor (vbox, GTK_TYPE_SCROLLED_WINDOW);
	rt.info_expander = gtk_expander_new (_("info"));
	gtk_box_pack_start (GTK_BOX (vbox), rt.info_expander, FALSE, FALSE, 0);

	E2_OptionSet *infoshow = e2_option_get ("dialog-mkdir-show-info");
	if (e2_option_bool_get_direct (infoshow))
		gtk_expander_set_expanded (GTK_EXPANDER(rt.info_expander), TRUE);

	g_object_set_data (G_OBJECT (rt.info_expander), "e2-controller-widget", rt.dialog);
	g_object_set_data (G_OBJECT (rt.info_expander), "e2-show-option", infoshow);
	e2_option_attach_value_changed_simple (infoshow, rt.info_expander,
		(HookFunc)_e2_mkdirdlg_set_expander_hook, rt.info_expander);

	g_signal_connect (G_OBJECT (rt.info_expander), "notify::expanded",
		G_CALLBACK (_e2_mkdirdlg_expander_toggled_cb), &rt);

	//info box is packed into vbox, not the expander, so that dialog-resizing works
	rt.info_box = e2_widget_get_box (TRUE, FALSE, 1);
	gtk_container_set_border_width (GTK_CONTAINER (rt.info_box), E2_PADDING_SMALL);
	gtk_box_pack_start (GTK_BOX (vbox), rt.info_box, FALSE, FALSE, 0);

	GtkWidget *hbox = e2_widget_add_box (rt.info_box, FALSE, 0, FALSE, FALSE, 0);
	GtkWidget *label = e2_widget_add_label (hbox,
		_("<small>parent directory:</small>"), 0.0, 0.0, FALSE, E2_PADDING_SMALL);
	GtkSizeGroup *group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	gtk_size_group_add_widget (group, label);
	rt.info_label = e2_widget_add_mid_label (hbox, "", 0.0, TRUE, E2_PADDING_SMALL);

	hbox = e2_widget_add_box (rt.info_box, FALSE, 0, FALSE, FALSE, 0);
	label = e2_widget_add_label (hbox,
		_("<small>creation possible:</small>"), 0.0, 0.0, FALSE, E2_PADDING_SMALL);
	gtk_size_group_add_widget (group, label);
	g_object_unref (G_OBJECT (group));
	rt.info_label2 = e2_widget_add_label (hbox, "", 0.0, 0.0, TRUE, E2_PADDING_SMALL);

	if (follow_pane == NULL)
		follow_pane = e2_option_get ("dialog-mkdir-follow-pane");
	if (suggest_dir == NULL)
		suggest_dir = e2_option_get ("dialog-mkdir-suggest-directory");
	if (show_last == NULL)
		show_last = e2_option_get ("dialog-mkdir-show-last");
	if (connected == NULL)
		connected = e2_option_get ("dialog-mkdir-connected");

#ifndef USE_GTK3_0
	CLOSEBGL
#endif
	//CHECKME seems that this needs BGL closed
	rt.menu = e2_menu_create_options_menu (rt.dialog, NULL,
		follow_pane, _e2_mkdirdlg_toggled1_cb, &rt,
		suggest_dir, _e2_mkdirdlg_toggled3_cb, &rt,
		show_last, _e2_mkdirdlg_toggled2_cb, &rt,
		connected, e2_menu_control_cb, rt.dialog,
		NULL);
#ifndef USE_GTK3_0
	OPENBGL
#endif
	//arrange menu cleanup
	g_object_set_data_full (G_OBJECT (rt.dialog), "menu", rt.menu,
		(GDestroyNotify) e2_menu_selection_done_cb);
	//and popup
	g_signal_connect (G_OBJECT (rt.dialog), "button-press-event",
		G_CALLBACK (_e2_mkdirdlg_button_press_cb), rt.menu);

//do this after create button is created ?
//tag DEBUGfreeze
#ifndef USE_GTK3_0
	CLOSEBGL
#endif
	//this updates status but not the suggested dir name
	_e2_mkdirdlg_update_follow_dir (&rt, e2_option_bool_get_direct (follow_pane));
//tag DEBUGfreeze
	OPENBGL
	e2_option_connect (rt.dialog, e2_option_bool_get_direct (connected));
	rt.opt_show_last = e2_option_bool_get_direct (show_last);
	rt.opt_suggest_dir = e2_option_bool_get_direct (suggest_dir);

//tag DEBUGfreeze
	CLOSEBGL
	_e2_mkdirdlg_update_name (&rt);
//tag DEBUGfreeze
	OPENBGL

	E2_Button local_btn;
	local_btn = E2_BUTTON_MORE;
	local_btn.tip = _("Create and continue");
	local_btn.showflags |= E2_BTN_TIPPED;
	e2_dialog_add_defined_button (rt.dialog, &local_btn);

	e2_button_derive (&local_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);
	local_btn.tip = NULL;
	local_btn.showflags |= E2_BTN_DEFAULT;
	e2_dialog_add_defined_button (rt.dialog, &local_btn);

	local_btn.label = _("C_reate");	//this appears with "_Cancel"
	local_btn.name = STOCK_NAME_DIRECTORY;
	local_btn.showflags = 0;
	local_btn.response = GTK_RESPONSE_YES;
	e2_dialog_add_defined_button (rt.dialog, &local_btn);

	vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt.dialog));
#else
		GTK_DIALOG(rt.dialog)->vbox;
#endif
	gtk_widget_show_all (vbox);
	if (!e2_option_bool_get_direct (infoshow))
		gtk_widget_hide (rt.info_box);

//	NEEDOPENBGL
	_e2_mkdirdlg_deferred_change_cb (&rt);	//init dialog widgets
//	NEEDCLOSEBGL
	CLOSEBGL
#ifdef GTK3_COMBO_FIX
	gtk_widget_grab_focus (gtk_bin_get_child (GTK_BIN(rt.combo)));
#endif
	e2_dialog_setup (rt.dialog, app.main_window);
	e2_dialog_run (rt.dialog, NULL, E2_DIALOG_DONT_SHOW_ALL);
	OPENBGL

	//connect signal after showing, to reduce repetition during setup
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt.combo));
#else
		GTK_BIN (rt.combo)->child;
#endif
	g_signal_connect (GTK_EDITABLE(entry), "changed",
		G_CALLBACK (_e2_mkdirdlg_changed_cb), &rt);

	*qed->status = E2_TASK_PAUSED;
	e2_dialog_wait (rt.dialog, FALSE, TRUE, FALSE, FALSE);  //CHECKME TRUE maincontext ?
	*qed->status = E2_TASK_RUNNING;

	if (GTK_IS_DIALOG (rt.dialog))
	{	//not explicitly closed by the user
		CLOSEBGL
		gtk_widget_destroy (rt.dialog);
		OPENBGL
	}

	g_free (rt.path);
	e2_list_free_with_data (&rt.history);
	e2_hook_unregister (&app.pane1.hook_change_dir, (HookFunc)_e2_mkdirdlg_change_dir_hook, &rt, TRUE);
	e2_hook_unregister (&app.pane2.hook_change_dir, (HookFunc)_e2_mkdirdlg_change_dir_hook, &rt, TRUE);
	e2_hook_unregister (&app.hook_pane_focus_changed, (HookFunc)_e2_mkdirdlg_change_focus_hook, &rt, TRUE);

	return TRUE;
}

  /******************/
 /***** public *****/
/******************/

void e2_mkdir_dialog_actions_register ()
{
	E2_Action action =
	{g_strconcat(_A(1),".",_A(63),NULL),_e2_mkdir_dialog_create,FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL};
	e2_action_register (&action);
	//while we're at it, init some session parameters
	e2_cache_list_register ("mkdir-history", &mkdir_history);
}

void e2_mkdir_dialog_options_register ()
{
	gchar* group_name = g_strconcat(_C(11),":",_C(24),NULL);   //_("dialogs:mkdir"
	e2_option_bool_register ("dialog-mkdir-show-info",
		group_name, _("open info frame"),
		_("This causes make-directory dialogs to start with extra information displayed"), NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
	e2_option_bool_register ("dialog-mkdir-follow-pane",
		group_name, _("follow active-pane directory"),
		_("This makes the parent directory for new directories the same as the one in the active pane, even if the latter changes"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("dialog-mkdir-suggest-directory",
		group_name, _("suggest directory name"),
		_("This presents a suggested name for each new directory, based on the last-created directory with an increasing number appended"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("dialog-mkdir-show-last",
		group_name, _("show last directory name"),
		_("This causes the name of the last-created directory to be shown in the entry field, after opening the dialog or when creating another directory"),
			"!dialog-mkdir-suggest-directory", FALSE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("dialog-mkdir-connected", group_name, _("replicate changes"),
		_("This causes option-changes to be replicated in other mkdir dialogs. Otherwise such changes will be confined to the current dialog"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
}
