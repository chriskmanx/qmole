/* $Id: e2_edit_dialog.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2006-2013 tooar <tooar@emelfm2.net>

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

#include "emelfm2.h"
#include <string.h>
#include "e2_dialog.h"
#include "e2_view_dialog.h"
#ifdef USE_GTK2_10
# include "e2_print.h"
#endif
#include "e2_textiter.h"
#include "e2_task.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_icons.h"

//rates around the default blink rate will never actually blink! dunno why ...
//so: MULTIPLIER >= 1.5, MULTIPLIER/DIVIDER <= 0.5
#define OVERWR_CURSOR_MULTIPLIER 2.0
#define INSERT_CURSOR_DIVIDER 4.0
#ifndef CURSOR_ON_MULTIPLIER
#define CURSOR_ON_MULTIPLIER 0.66
#endif
#ifndef CURSOR_OFF_MULTIPLIER
#define CURSOR_OFF_MULTIPLIER 0.34
#endif

//these variables can all be shared among all concurrent and sequential
//view and edit dialogs
extern gboolean case_sensitive;
extern gboolean whole_words;
extern gboolean search_backward;
extern gboolean search_wrap;
extern GList *find_history;
static gboolean replace_all;
static gboolean confirm_before;
static gboolean do_next;
static GList *replace_history = NULL;
#ifdef E2_SPELLCHECK
static GList *lang_history = NULL;
#endif
gint blinkmsec;	//blink-cycle duration (msec)
//keycodes corresponding to mnemonics for translated labels
//these ones are shared with a view dialog
extern guint find_keycode;
extern guint findnext_keycode;
extern guint hide_keycode;
guint replace_keycode;
static guint save_keycode;
static guint undo_keycode;
static guint redo_keycode;

static void _e2_edit_dialog_dirty_change_cb (GtkTextBuffer *textbuffer,
	E2_ViewDialogRuntime *rt);
static gboolean _e2_edit_dialog_reeditQ (E2_ActionTaskData *qed);

  /****************************/
 /**** undo functionality ****/
/****************************/

#define INCLUDED_IN_PARENT
#include "e2_edit_dialog_undo.c"
#undef INCLUDED_IN_PARENT

  /***************************/
 /**** editing functions ****/
/***************************/

#ifdef E2_SPELLCHECK
static void _e2_edit_dialog_cancel_spell (E2_ViewDialogRuntime *rt)
{
	gtkspell_detach (rt->spelldata);	//this destroys the GtkSpell
	rt->spelldata = NULL;
}
#endif

/**
@brief save text selected in @a buffer
This func is also used to save output pane selection
Expects BGL closed
@param buffer text buffer from which the text will be saved
#ifdef E2_VFS
@param spacedata pointer to namspace data
#endif
@param parent parent widget, dialog or main window

@return
*/
void e2_edit_dialog_save_selected (GtkTextBuffer *buffer,
#ifdef E2_VFS
	PlaceInfo *spacedata,
#endif
	GtkWidget *parent)
{
#ifdef E2_VFSTMP
	check assumption that selector only works for current namespace
#endif
	GtkTextIter start, end;
	//get this first, as the dialog unselects the text
	if (gtk_text_buffer_get_selection_bounds (buffer, &start, &end))
	{
		GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
			GTK_WINDOW (parent), GTK_FILE_CHOOSER_ACTION_SAVE, NULL, NULL);
		e2_dialog_setup_chooser (dialog,
			_("file save as"),
			//API doco is silent on whether g_get_home_dir() gets a localised string
			g_get_home_dir (),
			GTK_FILE_CHOOSER_ACTION_SAVE,
			FALSE,	//hide-hidden
			FALSE,	//single-selection
			GTK_RESPONSE_OK,	//default response
			STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
			STOCK_NAME_SAVE, GTK_RESPONSE_OK,
			NULL);

		gint response;
		while ((response = e2_dialog_run_simple (dialog, parent)) == E2_RESPONSE_USER1)
		{}

		if (response == GTK_RESPONSE_OK)
		{
			gchar *local = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
#ifdef E2_VFS
			VPATH ddata = { local, spacedata };
#endif
#ifndef USE_GTK2_8
			//check for o/w
			if (e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
				&& e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
				&& e2_fs_access2 (local E2_ERR_NONE()) == 0)
#endif
			{
				OPENBGL
				DialogButtons choice = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
					&ddata,
#else
					local,
#endif
					NONE);
				CLOSEBGL
				if (choice != OK)
				{
					gtk_widget_destroy (dialog);
					gtk_text_buffer_select_range (buffer, &start, &end);
					g_free (local);
					return;
				}
			}
#endif
			gchar *text = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
			e2_fs_set_file_contents (
#ifdef E2_VFS
			&ddata, text, strlen (text), 0644 E2_ERR_NONE());
#else
			local, text, strlen (text), 0644 E2_ERR_NONE());
#endif
			g_free (text);
			g_free (local);
		}
		gtk_widget_destroy (dialog);
		gtk_text_buffer_select_range (buffer, &start, &end);
	}
}
/**
@brief save currently-selected text

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_savesel_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	e2_edit_dialog_save_selected (rt->textbuffer,
#ifdef E2_VFS
		rt->spacedata,
#endif
		rt->dialog);
	NEEDOPENBGL
}
/**
@brief save file

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_save_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	GtkTextIter start, end;
	gtk_text_buffer_get_bounds (rt->textbuffer, &start, &end);
	gchar *text = gtk_text_buffer_get_text (rt->textbuffer, &start, &end, FALSE);

	//restore line ends
	gint extras;
	if (rt->linebreak == CR+LF)
		extras = gtk_text_buffer_get_line_count (rt->textbuffer) + 1;	//+1 = possible incomplete line
	else
		extras = 0; //irrelevant
	text = e2_utils_revert_line_ends (text, extras, rt->linebreak);

	//after external encoding conversion, or some fallback conversion,
	//we can't know the original encoding, so just save as UTF-8, and warn the user
	if (strcmp (rt->charset, "UNKNOWN")
		&& strstr (rt->charset, "UTF-8") == NULL)	//conversion is actually needed
	{
		gsize bytes_old, bytes_new;
		GError *error = NULL;
		gchar *newtext;
		newtext = g_convert (text, -1, rt->charset, "UTF-8", &bytes_old, &bytes_new, &error);
		if (error == NULL)
		{
			g_free (text);
			text = newtext;
		}
		else
		{
			gchar *msg;
			if (newtext != NULL)
				g_free (newtext);
			NEEDCLOSEBGL
			switch (error->code)
			{
				case G_CONVERT_ERROR_ILLEGAL_SEQUENCE:
					msg = g_strdup_printf (
					_("Cannot save %s in its original encoding %s. Reverting to UTF-8"),
					rt->localpath, rt->charset);
					e2_output_print_error (msg, TRUE);
					g_error_free (error);
					break;
				default:
					msg = g_strdup_printf (
					_("Encoding conversion failed for %s"), rt->localpath);
					e2_output_print_error (msg, TRUE);
					e2_output_print_error (error->message, FALSE);
					g_error_free (error);
					rt->saved_ok = FALSE;
					NEEDOPENBGL
					return;
			}
			NEEDOPENBGL
		}
	}

	const gchar *usepath;
	//rt->localpath may be a replacement name, when "saving as"
	usepath = (rt->saveas) ? rt->newlocalpath : rt->localpath;
#ifdef E2_VFS
	VPATH ddata = { usepath, rt->spacedata };
#endif
	if (e2_option_bool_get ("edit-save-backup")
		&& !rt->saveas
		&& !e2_fs_access (
#ifdef E2_VFS
		&ddata, W_OK E2_ERR_NONE()))
#else
		usepath, W_OK E2_ERR_NONE()))
#endif
	{
		gchar *saved = e2_utils_strcat (usepath, "~");
#ifdef E2_VFS
		VPATH sdata = { saved, rt->spacedata };
#endif
#ifndef LOCAL_BGL
//		NEEDCLOSEBGL
		OPENBGL	//downstream errors invoke local mutex locking
#endif
#ifdef E2_VFS
		e2_task_backend_rename (&ddata, &sdata);	//ignore any warning !!
#else
		e2_task_backend_rename (usepath, saved);	//ignore any warning !!
#endif
#ifndef LOCAL_BGL
		CLOSEBGL
//		NEEDOPENBGL
#endif
		g_free (saved);
	}

	//allow new file permissions to be the same as an old one, if any
	struct stat sb;
	gboolean replaced = !rt->saveas &&
#ifdef E2_VFS
		!e2_fs_stat (&ddata, &sb E2_ERR_NONE());	//through a link
#else
		!e2_fs_stat (usepath, &sb);
#endif

	rt->saved_ok = e2_fs_set_file_contents (
#ifdef E2_VFS
		&ddata, text, strlen (text), 0600 E2_ERR_NONE());
#else
		usepath, text, strlen (text), 0600);
#endif

	g_free (text);
	if (rt->saved_ok)
	{
		mode_t perms = (replaced) ? sb.st_mode & ALLPERMS : 0644;
#ifdef E2_VFS
		e2_fs_chmod (&ddata, perms E2_ERR_NONE());
#else
		e2_fs_chmod (usepath, perms);
#endif
	//	uid_t owner_id = ?;
	//	gid_t group_id = ?;
	//	e2_fs_chown (local, owner_id, group_id);

		if (!strcmp (rt->charset, "UNKNOWN"))
		{
			NEEDCLOSEBGL
			e2_fs_error_simple (
			_("Original encoding of '%s' is unknown, now saved as UTF-8"),
#ifdef E2_VFS
			&ddata);
#else
			usepath);
#endif
			NEEDOPENBGL
		}
		gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (rt->textbuffer), FALSE);
	}
}
/**
@brief save file with new name

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_saveas_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
#ifdef E2_VFSTMP
	check assumption that selector only works for current namespace
#endif
	NEEDCLOSEBGL

	GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
		GTK_WINDOW (rt->dialog), GTK_FILE_CHOOSER_ACTION_SAVE, NULL, NULL);
	gchar *utf = F_FILENAME_FROM_LOCALE (rt->localpath);
	e2_dialog_setup_chooser (dialog,
		_("file save as"),
		utf,
		GTK_FILE_CHOOSER_ACTION_SAVE,
		FALSE, 	//hide-hidden
		FALSE,	//single-selection
		GTK_RESPONSE_OK,	//default response
		STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
		STOCK_NAME_SAVE, GTK_RESPONSE_OK,
		NULL);
	F_FREE (utf, rt->localpath);

	gint response;
	while ((response = e2_dialog_run_simple (dialog, rt->dialog)) == E2_RESPONSE_USER1)
	{}

	if (response == GTK_RESPONSE_OK)
	{
		gchar *local = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
#ifndef USE_GTK2_8
		//check for O/W if not done by the dialog
#ifdef E2_VFS
		VPATH ddata = { local, rt->spacedata };
#endif
		if (e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
			&& e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
			&& e2_fs_access2 (local E2_ERR_NONE()) == 0)
#endif
		{
			OPENBGL
			DialogButtons choice = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
				&ddata,
#else
				local,
#endif
				NONE);
			CLOSEBGL
			if (choice != OK)
			{
				gtk_widget_destroy (dialog);
				g_free (local);
				return;
			}
		}
#endif
		rt->saveas = TRUE;
		rt->newlocalpath = local;
		NEEDOPENBGL
		_e2_edit_dialog_save_cb (NULL, rt);
		NEEDCLOSEBGL
		rt->saveas = FALSE;

		if (rt->saved_ok)
		{
			g_free (rt->localpath);
			rt->localpath = local;
			//update the dialog label
			GtkWidget *label = g_object_get_data (G_OBJECT (rt->dialog), "e2-dialog-label");
			gchar *utf = F_FILENAME_FROM_LOCALE (rt->localpath);
			gtk_label_set_text (GTK_LABEL (label), utf);
			F_FREE (utf, rt->localpath);
		}
	}
	gtk_widget_destroy (dialog);
	NEEDOPENBGL
}
/**
@brief reload edited file

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_refresh_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	if (!gtk_text_buffer_get_modified (GTK_TEXT_BUFFER (rt->textbuffer)))
		return;	//no point in reloading
	NEEDCLOSEBGL
	DialogButtons choice = e2_dialog_warning (
	_("Reverting to saved version cannot be undone"), _("Re_vert"));
	if (choice == OK)
	{
#ifdef E2_VFS
		VPATH ddata = { rt->localpath, rt->spacedata };
		e2_view_dialog_read_text (&ddata, rt);	//no failure test when refreshing
#else
		e2_view_dialog_read_text (rt->localpath, rt);	//no failure test when refreshing
#endif
		gtk_text_view_set_buffer (GTK_TEXT_VIEW (rt->textview), rt->textbuffer);
		g_object_unref (rt->textbuffer); //destroy buffer with view
		gtk_text_buffer_set_modified (rt->textbuffer, FALSE);
	}
	gtk_widget_grab_focus (rt->textview);
	NEEDOPENBGL
}
/**
@brief undo change

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_undo_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2edtdlg_undo_undo_one (rt);
	NEEDOPENBGL
}
#ifdef E2_REDO_ENABLED
/**
@brief redo change

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_redo_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2edtdlg_undo_redo_one (rt);
	NEEDOPENBGL
}
#endif
#ifdef E2_SPELLCHECK
static GList *lang_history;
/**
@brief recheck all buffer spelling

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_spellall_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	gchar *lang = (lang_history == NULL) ? NULL : 	//start speller with default language
					(gchar *) lang_history->data;  //or with last-chosen language
	GError *error = NULL;
	rt->spelldata = gtkspell_new_attach (GTK_TEXT_VIEW (rt->textview), lang, &error);
	if (rt->spelldata == NULL)
	{
		gchar *msg = g_strdup_printf (
			_("Cannot initialize spell checking: %s"), error->message);
		NEEDCLOSEBGL
		e2_output_print_error (msg, TRUE);
		NEEDOPENBGL
		g_error_free (error);
	}
	else
	{
		//disable all gtkspell signal-callbacks
		g_signal_handlers_block_matched (G_OBJECT (rt->textview),
			G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, rt->spelldata);
	}
}
/**
@brief cancel buffer spell checking

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_unspellall_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
//	NEEDOPENBGL
	gtkspell_detach (rt->spelldata);	//this destroys the GtkSpell
	rt->spelldata = NULL;
}
/**
@brief set spell-checker language

@param menuitem UNUSED the activated widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_spell_language_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	gchar *lang;
	if (lang_history == NULL)
	{
		lang = (gchar *)g_getenv ("LANG");	//ok if NULL
		if (lang != NULL && !strcmp (lang, "C"))
			lang = NULL;
		//CHECKME default used by speller backends
	}
	else
		lang = (gchar *)lang_history->data;

	NEEDCLOSEBGL
	DialogButtons choice = e2_dialog_combo_input (_("choose language"),
		_("Enter the spell-checker language (like en_CA):"), lang,
		0, &lang_history, &lang);	//re-use for entered value

	if (choice == OK)
	{
		GError *error = NULL;
		gtkspell_set_language (rt->spelldata, lang, &error);
		if (error != NULL)
		{
			gchar *msg = g_strdup_printf (
				_("Cannot set speller language to %s (%s)"), lang, error->message);
			e2_output_print_error (msg, TRUE);
			g_error_free (error);
		}
		g_free (lang);
	}
	NEEDOPENBGL
}
#endif
/**
@brief toggle text wrapping

@param menuitem UNUSED the selected widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_wrap_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	rt->textwrap = !rt->textwrap;
	NEEDCLOSEBGL
	gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (rt->textview),
		rt->textwrap ? GTK_WRAP_WORD: GTK_WRAP_NONE);
	NEEDOPENBGL
}
/**
@brief hide search/replace bar

@param menuitem UNUSED the selected widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_hide_cb (GtkWidget *menuitem,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	gtk_widget_hide (rt->panel);
	rt->is_hidden = TRUE;
	gtk_widget_hide (rt->info_label);
	gtk_widget_hide (rt->findbtn);
	gtk_widget_hide (rt->replacebtn);
	//in case we last performed an incremental search ...
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	const gchar *choice = gtk_entry_get_text (GTK_ENTRY (entry));
	if (choice != NULL && *choice != '\0')
	{
		e2_combobox_prepend_history (rt->combo, choice, 20, FALSE);
		e2_list_update_history (&find_history, choice, NULL, 20, FALSE);
	}
#ifdef E2_MARK_FINDS
	//turn off any highlihts
	e2_view_dialog_clear_hilites (rt);
#endif
	//hide any selection NAH, might want it for editing
//	GtkTextIter iter;
//	if (gtk_text_buffer_get_selection_bounds (rt->textbuffer, &iter, NULL))
//		gtk_text_buffer_select_range (rt->textbuffer, &iter, &iter);
	//now for the replacements ...
	entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo2));
#else
		GTK_BIN (rt->combo2)->child;
#endif
	choice = gtk_entry_get_text (GTK_ENTRY (entry));
	if (choice != NULL && *choice != '\0')
	{
		e2_combobox_prepend_history (rt->combo2, choice, 20, FALSE);
		e2_list_update_history (&replace_history, choice, NULL, 20, FALSE);
	}
	//focus on the text
	gtk_widget_grab_focus (rt->textview);
	NEEDOPENBGL
}
/**
@brief perform find
This is a menu-selection callback, and can also be called directly
@param menuitem the activated menu widget, or NULL or 0x1 when called directly
@param rt runtime data for the dialog

@return
*/
static void _e2_edit_dialog_find_cb (GtkWidget *menuitem,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	if (!rt->is_hidden) //search panel is visible
	{
		//if a mod key is pressed, treat it specially
		//Ctrl = find first, Shift = temp reverse direction
		GdkModifierType mask =
#ifdef USE_GTK3_0
			e2_utils_get_savedstate (rt->dialog)
#else
			e2_utils_get_modifiers ()
#endif
			 & gtk_accelerator_get_default_mod_mask ();
		if (mask == GDK_SHIFT_MASK)
		{	//temporary direction change
			rt->search_backward = !rt->search_backward;
			e2_view_dialog_search (FALSE, FALSE, rt);
			rt->search_backward = !rt->search_backward;
		}
		else
			//perform non-incremental search, from start|end if <Control> is pressed
			//in menu-initiated search, or direct-call with menuitem = 0x1
			e2_view_dialog_search (
				(menuitem != NULL && mask == GDK_CONTROL_MASK),
				FALSE, rt);
	}
	else
	{	//show the search panel & related things
#ifdef USE_GTK2_18
		if (gtk_widget_get_visible (rt->replacebar))
#else
		if (GTK_WIDGET_VISIBLE (rt->replacebar))
#endif
			gtk_widget_hide (rt->replacebar);
		gtk_widget_show (rt->panel);
		rt->is_hidden = FALSE;
		gtk_widget_show (rt->findbtn);

		GtkWidget *child =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (rt->combo));
#else
			GTK_BIN (rt->combo)->child;
#endif
		GtkTextIter start_iter, end_iter;
		if (gtk_text_buffer_get_selection_bounds (rt->textbuffer,
				&start_iter, &end_iter))
		{
			gchar *find_string = gtk_text_buffer_get_text
				(rt->textbuffer, &start_iter, &end_iter, FALSE);
			gtk_entry_set_text (GTK_ENTRY (child), find_string);
			gtk_editable_select_region (GTK_EDITABLE (child), 0, -1);
			printd (DEBUG, "_e2_edit_dialog_find_cb - non-incremental search");
			e2_view_dialog_search (FALSE, FALSE, rt);
			g_free (find_string);
		}
		else
			e2_view_dialog_update_combo (rt->combo);
		//focus on the text entry
		gtk_widget_grab_focus (child);
	}
	NEEDOPENBGL
}
/**
@brief perform replacement
Assumes BGL closed
@param menuitem UNUSED the selected widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_replace_cb (GtkWidget *menuitem,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
#ifdef USE_GTK2_18
	if (!gtk_widget_get_visible (rt->replacebtn))
		gtk_widget_show (rt->replacebtn);
	if (!gtk_widget_get_visible (rt->findbtn))
		gtk_widget_show (rt->findbtn);
	if (!gtk_widget_get_visible (rt->replacebar))
		gtk_widget_show (rt->replacebar);
#else
	if (!GTK_WIDGET_VISIBLE (rt->replacebtn))
		gtk_widget_show (rt->replacebtn);
	if (!GTK_WIDGET_VISIBLE (rt->findbtn))
		gtk_widget_show (rt->findbtn);
	if (!GTK_WIDGET_VISIBLE (rt->replacebar))
		gtk_widget_show (rt->replacebar);
#endif
	if (!rt->is_hidden) //search panel is visible
	{
		GtkTextIter start, end;
		if (!gtk_text_buffer_get_selection_bounds (rt->textbuffer, &start, &end))
		{	//nothing selected, presumably after a replace that's just happened
			//lazy user, try for another match, like Ctrl F
			e2_view_dialog_search (FALSE, FALSE, rt);
			return;
		}
		GtkWidget *entry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (rt->combo2));
#else
 			GTK_BIN (rt->combo2)->child;
#endif
		const gchar *replace = gtk_entry_get_text (GTK_ENTRY (entry));
		//do replacement(s)
		if (replace_all)
		{
			//ignore compiler warning about unitialized usage
			GtkWidget *dialog = NULL;	//assignment for complier-warning prevention only
			DialogButtons choice;
			if (confirm_before)
			{
				dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION,
					_("Replace this one ?"), _("confirm"), DUMMY_RESPONSE_CB, NULL);
				e2_dialog_add_defined_button (dialog, &E2_BUTTON_CANCEL);

				E2_Button local_btn;
				e2_button_derive (&local_btn, &E2_BUTTON_NO, BTN_NO_KEEP);
				e2_dialog_add_defined_button (dialog, &local_btn);

				e2_button_derive (&local_btn, &E2_BUTTON_YES, BTN_YES_DELETE);
				e2_dialog_add_defined_button (dialog, &local_btn);

				e2_dialog_setup (dialog, rt->dialog);
			}
			choice = NO_TO_ALL;	//default in case of abort
			while (e2_view_dialog_search (FALSE, FALSE, rt))
			{
				if (confirm_before)
				{
					gtk_widget_show (dialog);
					choice = e2_dialog_wait (dialog, TRUE, TRUE, FALSE, TRUE);
					if (GTK_IS_DIALOG (dialog)) //not explicitly closed by the user
						gtk_widget_hide (dialog);
					if (choice == CANCEL)
						continue;
					else if (choice == NO_TO_ALL)
						break;
				}
				gtk_text_buffer_delete_selection (rt->textbuffer, TRUE, TRUE);
				if (*replace != '\0')
					gtk_text_buffer_insert_interactive_at_cursor (rt->textbuffer,
						replace, -1, TRUE);
			}
			if (confirm_before && GTK_IS_DIALOG (dialog)) //not explicitly closed by the user
				gtk_widget_destroy (dialog);
		}
		else if (gtk_text_buffer_get_selection_bounds (rt->textbuffer, &start, &end))
		{
			gtk_text_buffer_delete_selection (rt->textbuffer, TRUE, TRUE);
			if (*replace != '\0')
				gtk_text_buffer_insert_interactive_at_cursor (rt->textbuffer,
					replace, -1, TRUE);
			if (do_next)
				e2_view_dialog_search (FALSE, FALSE, rt);
		}
	}
	else	//first time, make the search panel visible
	{
		gtk_widget_show (rt->panel);
		rt->is_hidden = FALSE;
		e2_view_dialog_update_combo (rt->combo2);	//CHECKME always empty the replace string ?

		GtkWidget *child =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (rt->combo));
#else
			GTK_BIN (rt->combo)->child;
#endif
		GtkTextIter start_iter, end_iter;
		if (gtk_text_buffer_get_selection_bounds (rt->textbuffer,
				&start_iter, &end_iter))
		{
			gchar *find_string = gtk_text_buffer_get_text
				(rt->textbuffer, &start_iter, &end_iter, FALSE);
			gtk_entry_set_text (GTK_ENTRY (child), find_string);
			gtk_editable_select_region (GTK_EDITABLE (child), 0, -1);
			printd (DEBUG, "_e2_edit_dialog_replace_cb - non-incremental search");
			e2_view_dialog_search (FALSE, FALSE, rt);
			g_free (find_string);
		}
		else
			e2_view_dialog_update_combo (rt->combo);
		//focus on the text entry
		gtk_widget_grab_focus (child);
	}
	NEEDOPENBGL
}
/**
@brief construct and pop up destroyable context-menu for the edit-dialog

@param textview the textview widget where the click happened
@param event_button which mouse button was clicked (0 for a menu key)
@param event_time time that the event happened (0 for a menu key)
@param rt runtime struct to work on

@return
*/
static void _e2_edit_dialog_show_context_menu (GtkWidget *textview,
	guint event_button, gint event_time, E2_ViewDialogRuntime *rt)
{
	gchar *item_name;
	GtkWidget *item, *menu = e2_menu_get ();
#ifdef E2_SPELLCHECK
	if (rt->spelldata != NULL)
	{	//decide if we need a spellfix item in the menu
		GtkTextIter iter;
		gboolean atcursor = (event_button == 0); //TRUE if processing a menu-button press
		if (!atcursor)
		{
			GdkEvent *event = gtk_get_current_event ();
			if (event != NULL) //should never happen
			{
				if (event->type == GDK_BUTTON_PRESS)
				{
					gint buf_x, buf_y, x, y;
					x = ((GdkEventButton *) event)->x;
					y = ((GdkEventButton *) event)->y;
					gtk_text_view_window_to_buffer_coords (GTK_TEXT_VIEW (rt->textview),
						GTK_TEXT_WINDOW_TEXT, x, y, &buf_x, &buf_y);
					gtk_text_view_get_iter_at_location (GTK_TEXT_VIEW (rt->textview),
						&iter, buf_x, buf_y);
				}
				else
					atcursor = TRUE;	//revert to using cursor position
				gdk_event_free (event);
			}
			else
				atcursor = TRUE;
		}
		if (atcursor)
		{ //menu button was pressed, or error happened
			GtkTextMark *mark = gtk_text_buffer_get_insert (rt->textbuffer);
			gtk_text_buffer_get_iter_at_mark (rt->textbuffer, &iter, mark);
		}
		//use function from patched gtk-spell to get fixes menu, if any
		GtkWidget *submenu = gtkspell_get_suggestions_menu (rt->spelldata, &iter);
		if (submenu != NULL)
		{	//word is mis-spelt
			item = e2_menu_add (menu, _("Spelling suggestions"), NULL, NULL, NULL, NULL);
			gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
			e2_menu_add_separator (menu);
		}
	}
#endif
	item =
	e2_menu_add (menu, _("_Save"), STOCK_NAME_SAVE,
		_("Save the file"), _e2_edit_dialog_save_cb, rt);
	gtk_widget_set_sensitive (item, rt->is_dirty);
	e2_menu_add (menu, _("Save as.."), STOCK_NAME_SAVE_AS,	//no available valid mnemonic
		_("Save the file with a new name"), _e2_edit_dialog_saveas_cb, rt);
	item =
	e2_menu_add (menu, _("Save se_lection.."), "save_selection"E2ICOND,
		_("Save the selected text"), _e2_edit_dialog_savesel_cb, rt);
	gtk_widget_set_sensitive (item,
		gtk_text_buffer_get_selection_bounds (rt->textbuffer, NULL, NULL));
#ifdef USE_GTK2_10
	gchar *tip = (gtk_text_buffer_get_has_selection (rt->textbuffer)) ?
		_("Print selected text") : _("Print file");
	e2_menu_add (menu, _("_Print.."), STOCK_NAME_PRINT, tip,
		e2_dialog_print_cb, rt);
#endif
	item =
	e2_menu_add (menu, _("Re_vert"), STOCK_NAME_REVERT_TO_SAVED,
		_("Reload the file being edited"), _e2_edit_dialog_refresh_cb, rt);
	gtk_widget_set_sensitive (item, rt->is_dirty);
	e2_menu_add_separator (menu);
	e2_menu_add (menu, _("_Find.."), STOCK_NAME_FIND,
		_("Find matching text"), _e2_edit_dialog_find_cb, rt);
	e2_menu_add (menu, _("_Replace.."), STOCK_NAME_FIND_AND_REPLACE,
		_("Find and replace matching text"), _e2_edit_dialog_replace_cb, rt);
	item =
	e2_menu_add (menu, _("_Hide"), STOCK_NAME_ZOOM_FIT,
		_("Hide the search options bar"), _e2_edit_dialog_hide_cb, rt);
	gtk_widget_set_sensitive (item, !rt->is_hidden);
	item =
	e2_menu_add (menu, _("_Undo"), STOCK_NAME_UNDO,
		_("Undo last change"), _e2_edit_dialog_undo_cb, rt);
	gtk_widget_set_sensitive (item, rt->undo_enabled);
#ifdef E2_REDO_ENABLED
	item =
	e2_menu_add (menu, _("Re_do"), STOCK_NAME_REDO,
		_("Reverse last undo"), _e2_edit_dialog_redo_cb, rt);
	gtk_widget_set_sensitive (item, rt->redo_enabled);
#endif
#ifdef E2_SPELLCHECK
	if (rt->spelldata == NULL)
		e2_menu_add (menu, _("_Check spelling"), STOCK_NAME_SPELL_CHECK,
			_("Flag mis-spelt words"), _e2_edit_dialog_spellall_cb, rt);
	else
		e2_menu_add (menu, _("_Clear spellcheck"), STOCK_NAME_SPELL_CHECK,
			_("Remove all spell-check flags"), _e2_edit_dialog_unspellall_cb, rt);
#endif
	e2_menu_add_separator (menu);
#ifdef E2_SPELLCHECK
	item =
	e2_menu_add (menu, _("_Language.."), STOCK_NAME_INDEX,
		_("Set spell-checker language"), _e2_edit_dialog_spell_language_cb, rt);
	gtk_widget_set_sensitive (item, (rt->spelldata != NULL));
#endif
	item =
	e2_menu_add_check (menu, _("_Wrap"), rt->textwrap,
		 _e2_edit_dialog_wrap_cb, rt);
	e2_widget_set_safetip (item,
		_("If activated, text in the window will be word-wrapped"));

	item_name = g_strconcat (_A(3),".",_A(34),NULL);
	e2_menu_add_action (menu, _("Se_ttings"), STOCK_NAME_PREFERENCES,
		_("Open the configuration dialog at the options page"), item_name,
		_C(11)); //_("dialogs")
	g_free(item_name);

	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	if (event_button == 0)
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_view_dialog_set_menu_position,
			rt, event_button, event_time);
	else
		//this was a button-3 click
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			NULL, NULL, event_button, event_time);
}

  /*****************/
 /***** utils *****/
/*****************/

//Assumes BGL closed
static void _e2_edit_dialog_destroy (E2_ViewDialogRuntime *rt, gboolean forced)
{
	if (rt->is_dirty)
	{
		NEEDCLOSEBGL
		GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION,
			_("Save modified file ?"), _("confirm"), DUMMY_RESPONSE_CB, NULL);

		//arrange for a specific retval if the user presses <Esc>
//default setting is sufficient e2_dialog_set_negative_response (dialog, GTK_RESPONSE_USER1);

		E2_Button resume_btn;
		resume_btn.label = _("_Edit");
		resume_btn.name = STOCK_NAME_EDIT;
		resume_btn.tip = _("Resume editing");
		resume_btn.showflags = E2_BTN_TIPPED;
		resume_btn.response = E2_RESPONSE_NOTOALL;	//must be understood by the response-decoder

		E2_Button discard_btn;
		discard_btn = E2_BUTTON_DISCARD;
		discard_btn.response = E2_RESPONSE_APPLYTOALL;	//must be understood by the response-decoder

		E2_Button yes_btn;
		yes_btn.label = _("_Save");
		yes_btn.name = STOCK_NAME_SAVE;
		yes_btn.showflags = E2_BTN_DEFAULT;
		yes_btn.response = GTK_RESPONSE_YES;

		//block until user selects something
		DialogButtons choice = e2_dialog_show (dialog, app.main_window,
			E2_DIALOG_BLOCKED | E2_DIALOG_FREE,
			&resume_btn, &discard_btn, &yes_btn, NULL);

		NEEDOPENBGL
		switch (choice)
		{
			case OK:	//save
				_e2_edit_dialog_save_cb (NULL, rt);
			case YES_TO_ALL:	//discard
				break;
			default:	//resume, close etc
				if (!forced)
					return;	//close-dialog or Escape-press just cancels the close, in effect
				break;
		}
	}

	if (rt->blink_init_id > 0)
	{
		g_source_remove (rt->blink_init_id);
		rt->blink_init_id = 0;
	}
	if (rt->blink_id > 0)
	{
		g_source_remove (rt->blink_id);
		rt->blink_id = 0;
	}

	NEEDCLOSEBGL
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo2));
#else
		GTK_BIN (rt->combo2)->child;
#endif
	const gchar *repl = gtk_entry_get_text (GTK_ENTRY (entry));
	if (repl != NULL && *repl != '\0')
	{
		e2_combobox_prepend_history (rt->combo2, repl, 20, FALSE);
		e2_list_update_history (&replace_history, repl, NULL, 20, FALSE);
	}
	//if editing an output tab, need some specific cleanup
	GList *tmp;
	for (tmp = app.tabslist; tmp != NULL; tmp = tmp->next)
	{
		if (((E2_OutputTabRuntime*)tmp->data)->buffer == rt->textbuffer)
		{	//we were editing an output pane buffer
			//that stays in existence, so we need to get rid of redundant callbacks
			g_signal_handlers_disconnect_by_func (G_OBJECT (rt->textbuffer),
				_e2edtdlg_insert_text_cb, rt);
			g_signal_handlers_disconnect_by_func (G_OBJECT (rt->textbuffer),
				_e2edtdlg_delete_range_cb, rt);
			g_signal_handlers_disconnect_by_func (G_OBJECT (rt->textbuffer),
				_e2edtdlg_begin_user_action_cb, rt);
			g_signal_handlers_disconnect_by_func (G_OBJECT (rt->textbuffer),
				_e2edtdlg_end_user_action_cb, rt);
			g_signal_handlers_disconnect_by_func (G_OBJECT (rt->textbuffer),
				_e2_edit_dialog_dirty_change_cb, rt);
#ifdef E2_MARK_FINDS
			if (rt->is_lit)
				e2_view_dialog_clear_hilites (rt);	//also clears callback
#endif
			break;
		}
	}
#ifdef E2_SPELLCHECK
	if (rt->spelldata != NULL)
		gtkspell_detach (rt->spelldata);	//this frees GtkSpell data
#endif

	e2_view_dialog_destroy (rt);
	NEEDOPENBGL
}
/**
@brief toggle cursor visibility
This is a timer callback
@param textview the textview for the dialog

@return TRUE (so the timer continues) if @a textview is not focused
*/
// FORCED CURSOR BLINKING STUFFS UP LARGER VERTICAL MOVES
static gboolean _e2_edit_dialog_blink (E2_ViewDialogRuntime *rt)
{
//	printd (DEBUG, "edit dialog cursor visibility toggle cb");
//	CLOSEBGL
//	gboolean blinkme = TRUE;	//GTK_WIDGET_HAS_FOCUS (rt->dialog);
//	if (blinkme)
//	{
//		g_source_remove (blink_id);
	if (GTK_IS_WIDGET (rt->textview))
	{	//the dialog hasn't been closed between callbacks
		gboolean visible = gtk_text_view_get_cursor_visible (GTK_TEXT_VIEW (rt->textview));
		gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (rt->textview), !visible);
		gint period = (visible) ?
			blinkmsec * CURSOR_OFF_MULTIPLIER:
			blinkmsec * CURSOR_ON_MULTIPLIER;
		if (rt->ow_mode)
			period /= INSERT_CURSOR_DIVIDER;
		//FIXME this timer needs to be cancellable at session-end
		rt->blink_id = g_timeout_add_full (G_PRIORITY_LOW, period,
			(GSourceFunc) _e2_edit_dialog_blink, rt, NULL);
	}
//	}
//	OPENBGL
//	return !blinkme; //remove ourself if we're done
//	return TRUE;	//already removed
	return FALSE;	//remove ourself
}
/**
@brief initialize cursor blinking
This is an idle callback
@param rt data struct for the dialog

@return FALSE(so the idle callback is cancelled)
*/
static gboolean _e2_edit_dialog_blink_start (E2_ViewDialogRuntime *rt)
{
//	printd (DEBUG, "Cursor blinking started");
	if (rt->blink_id == 0)
		rt->blink_id = g_timeout_add_full (G_PRIORITY_HIGH_IDLE,
			blinkmsec * CURSOR_OFF_MULTIPLIER,
			(GSourceFunc) _e2_edit_dialog_blink, rt, NULL);
	rt->blink_init_id = 0;
	return FALSE;
}
/**
@brief suspend cursor blinking

@param rt data struct for the dialog

@return
*/
static void _e2_edit_dialog_blink_stop (E2_ViewDialogRuntime *rt)
{
	if (rt->blink_id > 0)
	{
		g_source_remove (rt->blink_id);
		rt->blink_id = 0;
	}
	gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (rt->textview), TRUE);
	//setup to go again when next idle
	rt->blink_init_id = g_idle_add_full (G_PRIORITY_HIGH_IDLE,
		(GSourceFunc)_e2_edit_dialog_blink_start, rt, NULL);
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief text-buffer edited callback

@param textbuffer the textbuffer which is now clean or dirty
@param rt dialog runtime data struct

@return
*/
static void _e2_edit_dialog_dirty_change_cb (GtkTextBuffer *textbuffer,
	E2_ViewDialogRuntime *rt)
{
	rt->is_dirty = !rt->is_dirty;
	NEEDCLOSEBGL
	gtk_widget_set_sensitive (rt->savebtn, rt->is_dirty);
	NEEDOPENBGL
}
/**
@brief menu-button press callback

@param textview the textview widget where the press happened
@param rt dialog runtime data struct

@return TRUE always
*/
static gboolean _e2_edit_dialog_popup_menu_cb (GtkWidget *textview,
	E2_ViewDialogRuntime *rt)
{
	_e2_edit_dialog_blink_stop (rt);
	guint32 event_time = gtk_get_current_event_time ();
	NEEDCLOSEBGL
	_e2_edit_dialog_show_context_menu (textview, 0, event_time, rt);
	NEEDOPENBGL
	return TRUE;
}
/**
@brief general dialog key press callback

@param textview UNUSED the widget where the button was pressed
@param event gdk event data
@param rt rt data for the dialog

@return TRUE (stop other handlers) for recognised keys
*/
gboolean e2_edit_dialog_key_press_cb (GtkWidget *textview,
	GdkEventKey *event, E2_ViewDialogRuntime *rt)
{
	gboolean retval = FALSE;
	NEEDCLOSEBGL
	_e2_edit_dialog_blink_stop (rt);
	guint mask = event->state & gtk_accelerator_get_default_mod_mask (); //SHIFT check used
	if (//g_ascii_isalpha (event->keyval) //&& (event->keyval < 0xF000 || event->keyval > 0xFFFF) //&&
		(mask & GDK_CONTROL_MASK || mask == GDK_MOD1_MASK))
	{	// the key is a letter and the modifier is Alt or a modifier is Ctrl
		guint lower = gdk_keyval_to_lower (event->keyval);
		//hashtable already checked during dialog setup
		guint asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (lower)));
		if (asciicode == 0)
			asciicode = lower;
		if (lower == save_keycode) //|| asciicode == GDK_s)
		{
			NEEDOPENBGL
			if (rt->is_dirty)
				_e2_edit_dialog_save_cb (NULL, rt);
			return TRUE;
		}
		else if (lower == find_keycode || asciicode == findnext_keycode)	//special case again
		{
#ifdef E2_SPELLCHECK
			if (rt->spelldata != NULL)
				_e2_edit_dialog_cancel_spell (rt);
#endif
			rt->release_blocked = TRUE;	//block anomalous key-release-event searches
			//trick cb into doing a from-start|end search for Ctrl f
			NEEDOPENBGL
			_e2_edit_dialog_find_cb (
				(lower == find_keycode) ? NULL : GINT_TO_POINTER (1), rt);
			return TRUE;
		}
		else if (lower == replace_keycode) //|| asciicode == GDK_h)
		{
#ifdef E2_SPELLCHECK
			if (rt->spelldata != NULL)
				_e2_edit_dialog_cancel_spell (rt);
#endif
			NEEDOPENBGL
			_e2_edit_dialog_replace_cb (NULL, rt);
			return TRUE;
		}
		else if (lower == hide_keycode)
		{
			NEEDOPENBGL
			_e2_edit_dialog_hide_cb (NULL, rt);
			return TRUE;
		}
		else if (lower == undo_keycode || (asciicode == GDK_z && mask != GDK_MOD1_MASK)) //no Alt-z support
		{
			if (rt->undo_enabled)
				_e2edtdlg_undo_undo_one (rt);
			retval = TRUE;
		}
#ifdef E2_REDO_ENABLED
		else if (lower == redo_keycode
			 || (asciicode == GDK_z && mask == (GDK_CONTROL_MASK | GDK_SHIFT_MASK)))
		{
			if (rt->redo_enabled)
				_e2edtdlg_undo_redo_one (rt);
			retval = TRUE;
		}
#endif
	}
	else if (event->keyval == GDK_Insert)
	{
		//GTK_TEXT_VIEW (rt->textview)->overwrite_mode can get overwrite_mode state
		rt->ow_mode = !gtk_text_view_get_overwrite (GTK_TEXT_VIEW (rt->textview));
		gtk_text_view_set_overwrite (GTK_TEXT_VIEW (rt->textview), rt->ow_mode);
		retval = TRUE;
	}
/*	else if (lower == GDK_Delete)
	{	//workaround gtk behaviour
		gtk_text_buffer_set_modified (rt->textbuffer, TRUE);
	}
*/
	NEEDOPENBGL
	rt->keyval = event->keyval;	//save, for undo processing
	return retval;
}
/**
@brief replace combo-box key press callback
This ensures that a <Return> keypress behaves like Ctrl-r, and handles some other
special cases too
@param entry UNUSED the widget where the key was pressed
@param event gdk event data
@param rt rt data for the dialog

@return TRUE if recognised key was pressed
*/
static gboolean _e2_edit_dialog_combokey_cb (GtkWidget *entry,
	GdkEventKey *event, E2_ViewDialogRuntime *rt)
{
	guint lower;
	if (event->keyval == GDK_Return
	 || event->keyval == GDK_KP_Enter
	 || event->keyval == GDK_ISO_Enter
	 || event->keyval == GDK_3270_Enter)
		lower = replace_keycode;
	else if (event->keyval < 0xF000 || event->keyval > 0xFFFF)
		lower = gdk_keyval_to_lower (event->keyval);
	else
		lower = 0; //ignore other controls
	//not all key-actions are supported in replace-widget context
	//e.g. no undo or redo
	if (lower == find_keycode
	 || lower == findnext_keycode
	 || lower == replace_keycode
	 || lower == hide_keycode)
	{
//		NEEDOPENBGL
		return (e2_edit_dialog_key_press_cb (rt->textview, event, rt));
	}
	else
		return FALSE;
}
/**
@brief mouse button press callback

@param textview the widget where the button was pressed
@param event gdk event data
@param rt rt data for the dialog

@return TRUE (stop other handlers) for btn 3 press, else FALSE (allow other handlers)
*/
static gboolean _e2_edit_dialog_button_press_cb (GtkWidget *textview,
	GdkEventButton *event, E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2_edit_dialog_blink_stop (rt);
	if (event->button == 3
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		_e2_edit_dialog_show_context_menu (textview, 3, event->time, rt);
		NEEDOPENBGL
		return TRUE;
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief edit dialog response callback

@param dialog the dialog where the response was triggered
@param response the number assigned the activated widget
@param view rt data for the dialog

@return
*/
static void _e2_edit_dialog_response_cb (GtkDialog *dialog, gint response,
	E2_ViewDialogRuntime *rt)
{
	printd (DEBUG, "response_cb (dialog:_,response:%d,rt:_)", response);
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_FIND:
#ifdef E2_SPELLCHECK
			if (rt->spelldata != NULL)
				_e2_edit_dialog_cancel_spell (rt);
#endif
			NEEDOPENBGL
			_e2_edit_dialog_find_cb (NULL, rt);
			return;
		case E2_RESPONSE_USER1:	//replace
#ifdef E2_SPELLCHECK
			if (rt->spelldata != NULL)
				_e2_edit_dialog_cancel_spell (rt);
#endif
			NEEDOPENBGL
			_e2_edit_dialog_replace_cb (NULL, rt);
			return;
		case E2_RESPONSE_USER2:	//save
			NEEDOPENBGL
			_e2_edit_dialog_save_cb (NULL, rt);
			return;
#ifdef E2_SPELLCHECK
		case E2_RESPONSE_USER3:	//cancel spelling
			_e2_edit_dialog_cancel_spell (rt);
			break;
#endif
		default:
//		case GTK_RESPONSE_CLOSE:
			//if close button is clicked, it is not "forced"
			_e2_edit_dialog_destroy (rt, (response != GTK_RESPONSE_CLOSE));
			break;
	}
	NEEDOPENBGL
}

#ifdef E2_TRANSIENTBINDINGS
/**
@brief function to setup default key-bindings for edit dialog
This is just to provide placeholders, the actual bindings are meaningless
@param set pointer to option data struct

@return
*/
static void _e2_edit_dialog_keybindings (E2_OptionSet *set)
{
	//the key name strings are parsed by gtk, and no translation is possible
	e2_option_tree_setup_defaults (set,
	g_strdup("keybindings=<"),  //internal name
	//the column-0 category string(s) here need to match at least the lowest
	//treestore-iter of the full category name
//	g_strconcat(_C(17),"||||",NULL),  //_("general"
//	g_strconcat("\t",_C(11),"||||",NULL),  //_("dialogs"
	g_strconcat("\t\t",_A(46),"||||",NULL),  //_("edit"
	g_strconcat("\t\t\t|<Control>u","||","echo|\"up\"", NULL),//_A(127),".",_A(128),"|<Control>Home",NULL),
	g_strconcat("\t\t\t|<Control>n","||","echo|\"down\"", NULL),//_A(127),".",_A(128),"|<Control>End",NULL),
	g_strdup(">"),
	NULL);
}
# ifdef E2_MOUSECUSTOM
static void _e2_edit_dialog_mousebindings (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("mousebuttons=<"),  //internal name
/*	these lines go into category general.dialogs, so at least 2 leading tabs
	the button name strings are parsed by gtk, and no translation is possible, button codes may be 1 ... whatever
	columns: 0 cat, 1 button, 2 dblclick, 3 trplclick, [4 release,] 5 action, 6 action_data
	if releases handled (#ifdef MCOL_REL), lines must have one extra (=6) separator
*/
	g_strconcat("\t\t",_A(46),"|||||",NULL),  //_("edit"
#ifdef WITH_KEYFAKE
	g_strconcat("\t\t\t|<Control>4","|||",_A(127),".",_A(128),"|<Control>Home",NULL),
	g_strconcat("\t\t\t|<Control>5","|||",_A(127),".",_A(128),"|<Control>End",NULL),
#endif
	//MORE HERE
	g_strdup(">"),
	NULL);
}
#  ifdef E2_PTRGESTURES
static void _e2_edit_dialog_mousegestures (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("mousedrags=<"),  //internal name
	//the column-0 category string(s) here need to match at least the lowest
	//treestore-iter of the full category name
//	g_strconcat(_C(17),"||||",NULL),  //_("general"
//	g_strconcat("\t",_C(11),"||||",NULL),  //_("dialogs"
	g_strconcat("\t\t",_A(46),"||||",NULL),  //_("edit"
#ifdef WITH_KEYFAKE
	g_strconcat("\t\t\t|<Alt>3|^|8,2|",_A(127),".",_A(128),"|Page_Up",NULL), //"key.fake"
	g_strconcat("\t\t\t|<Alt>3||2,8|",_A(127),".",_A(128),"|Page_Down",NULL), //"key.fake"
#endif
	//MORE HERE
	g_strdup(">"),
	NULL);
}
#  endif
# endif
#endif

  /**********************/
 /**** dialog setup ****/
/**********************/
/**
@brief create and show edit dialog
@a localpath may be full path, or at least the name, of item to be edited.
If @a localpath is not an absolute path, curr_view->dir is prepended
If @a srt is non-NULL, and there's a problem during setup, @a srt is set to NULL
Expects BGL closed
@param localpath localised string, or NULL if editing a buffer
@param buf pointer to output pane tab textbuffer, or NULL id editing a file
@param srt pointer to store for returned dialog data, or NULL

@return
*/
static void _e2_edit_dialog_create (VPATH *localpath, GtkTextBuffer *buf, E2_ViewDialogRuntime **srt)
{
	//init view runtime object (0's to ensure view-dialog things are NULL)
	E2_ViewDialogRuntime *rt = ALLOCATE0 (E2_ViewDialogRuntime);
#if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (rt, if (srt != NULL) *srt = NULL; return;)
#else
	if (rt == NULL)
	{
		if (srt != NULL)
			*srt = NULL;
		return;
	}
#endif
	gboolean fileedit = (localpath != NULL);
	if (fileedit)
	{	//edit a file
		if (!e2_view_dialog_read_text (localpath, rt)) //get its content
		{
			DEALLOCATE (E2_ViewDialogRuntime, rt);
			if (srt != NULL)
				*srt = NULL;
			return;
		}
	}
	else
	{	//edit output pane buffer
		const gchar *home = g_get_home_dir ();
		gchar *name = g_strconcat (_C(28), "-", _A(46), NULL);	//_("output-edit")
		rt->localpath = g_build_filename (home, name, NULL);
		rt->charset = "UTF-8";	//the text is from our buffer
		g_free (name);
	}
	rt->case_sensitive = case_sensitive;
	rt->whole_words = whole_words;
	rt->search_backward = search_backward;
	rt->search_wrap = search_wrap;
	//dunno why this gets clobbered when threads used, but ...
	rt->charset = g_strdup (rt->charset);
//	gtk_text_buffer_set_modified (rt->textbuffer, FALSE);
	gtk_text_buffer_set_modified (rt->textbuffer, !fileedit);
//	rt->is_dirty = FALSE; not needed if FALSE set above
	rt->is_dirty = !fileedit;
//	rt->undo_list = NULL;
#ifdef E2_REDO_ENABLED
//	rt->redo_list = NULL;
#endif
//	rt->ui_tmp = NULL;
//	rt->seq_reserve = FALSE;	//default value for ui->seq ??

	//hard-code processing of localised <Ctrl>g for find-next
	findnext_keycode = GDK_g;

	gchar *utf = F_FILENAME_FROM_LOCALE (rt->localpath);
	rt->dialog = e2_dialog_create (NULL, utf, _("editing file"),
		(ResponseFunc)_e2_edit_dialog_response_cb, rt);
	F_FREE (utf, rt->localpath);
	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog));
#else
		GTK_DIALOG (rt->dialog)->vbox;
#endif
//	gtk_window_set_type_hint (GTK_WINDOW (rt->dialog), GDK_WINDOW_TYPE_HINT_NORMAL);
	//override some default label properties
	GtkWidget *label = g_object_get_data (G_OBJECT (rt->dialog),
		"e2-dialog-label");
//	gtk_label_set_line_wrap (GTK_LABEL (label), FALSE);
	gtk_label_set_selectable (GTK_LABEL (label), TRUE);

	e2_widget_add_separator (dialog_vbox, FALSE, 0);
	//create the view
	GtkWidget *sw = e2_widget_add_sw (dialog_vbox,
			GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC, TRUE, E2_PADDING);

	gint char_width, char_height;
	//textview creation needs BGL, on gtk3
	rt->textview = gtk_text_view_new ();
	// set view defaults, defaults to editable and cursor visible
	if (fileedit)
	{
		e2_view_dialog_set_font (&char_width, &char_height, rt);
	}
	else
	{
		rt->textbuffer = buf;
		//preserve appearance of output pane
		const gchar *fontname = e2_utils_get_output_font ();
		e2_widget_set_font (rt->textview, fontname);
		e2_widget_get_font_pixels (rt->textview, &char_width, &char_height);
	}
	rt->textwrap = e2_option_bool_get ("dialog-view-wrap");
	gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (rt->textview),
		rt->textwrap ? GTK_WRAP_WORD : GTK_WRAP_NONE);
#ifdef E2_MARK_FINDS
	e2_view_dialog_init_hilites (rt);
	rt->research = FALSE;
#endif
	rt->window_width = e2_option_int_get ("dialog-view-width");
	rt->window_height = e2_option_int_get ("dialog-view-height");
	if (!fileedit)
	{
		GtkTextMark *mark = gtk_text_buffer_get_insert (buf);
		gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (rt->textview),
			mark, 0.2, TRUE, 0.0, 0.5);
	}
	gtk_container_add (GTK_CONTAINER (sw), rt->textview);

//	rt->idle_id = 0;
	//init search runtime data
	rt->history = e2_list_copy_with_data (find_history);	//CHECKME why copy these ?
	rt->rephistory = e2_list_copy_with_data (replace_history);

	//action_area is a GtkHButtonBox packed at the end of the dialog's vbox
	//ditto for dialog->separator
	//locate find-bar between those 2
	rt->panel = e2_widget_get_box (TRUE, FALSE, 0);
	gtk_box_pack_end (GTK_BOX (dialog_vbox), rt->panel, FALSE, TRUE, E2_PADDING_XSMALL);
	gtk_box_reorder_child (GTK_BOX (dialog_vbox), rt->panel, 1);

	//add handlebox
#ifdef USE_GTK3_4
	GtkWidget *hndlbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
	//if context menu wanted, add event box c.f. toolbar
#else
	GtkWidget *hndlbox = gtk_handle_box_new ();
	gtk_handle_box_set_shadow_type (GTK_HANDLE_BOX (hndlbox), GTK_SHADOW_NONE);
	gtk_handle_box_set_handle_position ((GtkHandleBox*)hndlbox, GTK_POS_LEFT);
#endif
	gtk_container_add (GTK_CONTAINER (rt->panel), hndlbox);

#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
#endif
	gtk_container_add (GTK_CONTAINER (hndlbox), vbox);
	//create search bar
	rt->sgroup = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	GtkWidget *hbox = e2_view_dialog_create_searchbar (rt);
	g_object_unref (G_OBJECT(rt->sgroup));
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

	//create (hidden) replace bar, with buttons-area same size as buttons in search bar
	//(so that the corresponding expandable entries are the same size)
#ifdef USE_GTK3_0
	rt->replacebar = hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	rt->replacebar = hbox = gtk_hbox_new (FALSE, 0);
#endif
	//add replace-string combo
	//cuz E2_COMBOBOX_MENU_STYLE flag is not set, on gtk2, downstream calls
	//gtk_widget_set_name() which MAY? need BGL closed
	rt->combo2 = e2_combobox_add (hbox, TRUE, 0, NULL, NULL, &replace_history,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_FOCUS_ON_CHANGE);
//	gtk_widget_set_size_request (rt->combo2, 230, -1);
//	gtk_size_group_add_widget (group, rt->combo2);
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo2));
#else
		GTK_BIN (rt->combo2)->child;
#endif
	g_signal_connect_after (G_OBJECT (entry), "key-release-event",
		G_CALLBACK (_e2_edit_dialog_combokey_cb), rt);
	e2_widget_set_safetip (entry, _("Replacements"));
	//add replace-option buttons (mnemonics conform to search and action buttons)
	GtkWidget *hbox2 = e2_widget_add_box (hbox, FALSE, 0, FALSE, FALSE, 0);
	e2_button_add_toggle (hbox2, TRUE, do_next,
		_("r_epeat"),_("If activated, the next match will be sought after each replacement"),
		do_next, E2_PADDING_XSMALL, e2_view_dialog_toggled_cb, &do_next);
	e2_button_add_toggle (hbox2, TRUE, replace_all,
		_("_all"), _("If activated, all matches will be replaced at once"),
		replace_all, E2_PADDING_XSMALL, e2_view_dialog_toggled_cb, &replace_all);
	e2_button_add_toggle (hbox2, TRUE, confirm_before,
		_("co_nfirm"),_("If activated, confirmation will be sought when \"replacing all\""),
		confirm_before, E2_PADDING_XSMALL, e2_view_dialog_toggled_cb, &confirm_before);
	gtk_size_group_add_widget (rt->sgroup, hbox2);
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

	//now add things to the action-area
	//produce but don't yet show the "not found" warning
	gchar *labeltext = g_strconcat ("<span weight=\"bold\" foreground=\"",
		e2_option_str_get ("color-negative"), "\">", _("not found"), "</span>", NULL);
	rt->info_label =
	e2_widget_add_mid_label (
#ifdef USE_GTK2_14
		gtk_dialog_get_action_area (GTK_DIALOG (rt->dialog)),
#else
		GTK_DIALOG (rt->dialog)->action_area,
#endif
		labeltext, 0.0, TRUE, 0);
	//left-align the label
	gtk_button_box_set_child_secondary (
		GTK_BUTTON_BOX (
#ifdef USE_GTK2_14
		gtk_dialog_get_action_area (GTK_DIALOG (rt->dialog))
#else
		GTK_DIALOG (rt->dialog)->action_area
#endif
		), rt->info_label, TRUE);
	labeltext = _("_Replace");
	replace_keycode = e2_utils_get_mnemonic_keycode (labeltext);
	guint asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (replace_keycode)));
	if (asciicode != 0)
		replace_keycode = asciicode;
	rt->replacebtn =
	e2_dialog_add_simple_button (rt->dialog, STOCK_NAME_CONVERT, labeltext, E2_RESPONSE_USER1);
	e2_widget_set_safetip (rt->replacebtn, _("Replace this match"));
	labeltext = _("_Find");
	find_keycode = e2_utils_get_mnemonic_keycode (labeltext);
	asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (find_keycode)));
	if (asciicode != 0)
		find_keycode = asciicode;
	rt->findbtn =
	e2_dialog_add_simple_button (rt->dialog, STOCK_NAME_FIND, labeltext, E2_RESPONSE_FIND);
	e2_widget_set_safetip (rt->findbtn, _("Find the next match"));

	e2_dialog_add_defined_button (rt->dialog, &E2_BUTTON_CLOSE);

	labeltext = _("_Save");
	save_keycode = e2_utils_get_mnemonic_keycode (labeltext);
	asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (save_keycode)));
	if (asciicode != 0)
		save_keycode = asciicode;
	rt->savebtn =
	e2_dialog_add_simple_button (rt->dialog, STOCK_NAME_SAVE, labeltext, E2_RESPONSE_USER2);
	//allow saving of a fresh output buffer
	gtk_widget_set_sensitive (rt->savebtn, !fileedit);

//	e2_dialog_set_responses (rt->dialog, E2_RESPONSE_FIND, GTK_RESPONSE_CLOSE);
	//make sure we always exit cleanly
	e2_dialog_set_negative_response (rt->dialog, GTK_RESPONSE_CLOSE);
	//some used keycodes are related to context-menu-item labels which are not created until needed ...
	hide_keycode = e2_utils_get_mnemonic_keycode (_("_Hide"));
	asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (hide_keycode)));
	if (asciicode != 0)
		hide_keycode = asciicode;
	undo_keycode = e2_utils_get_mnemonic_keycode (_("_Undo"));
	asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (undo_keycode)));
	if (asciicode != 0)
		undo_keycode = asciicode;
#ifdef E2_REDO_ENABLED
	redo_keycode = e2_utils_get_mnemonic_keycode (_("Re_do"));
	asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (redo_keycode)));
	if (asciicode != 0)
		redo_keycode = asciicode;
#endif

	//this prevents a check button from being activated by keyboard
//	gtk_dialog_set_default_response (GTK_DIALOG (rt->dialog), E2_RESPONSE_FIND);

	gtk_window_resize (GTK_WINDOW (rt->dialog), char_width * rt->window_width,
				(char_height+3) * rt->window_height);
	//setup for cursor management
	GtkSettings *defs = gtk_widget_get_settings (rt->textview);
	if (defs != NULL)
	{
	  g_object_get (defs, "gtk-cursor-blink-time", &blinkmsec, NULL);
	}
	else
		blinkmsec = 1200;	//gtk default
	blinkmsec *= OVERWR_CURSOR_MULTIPLIER;
	rt->blink_init_id = g_idle_add_full (G_PRIORITY_HIGH_IDLE,
		(GSourceFunc)_e2_edit_dialog_blink_start, rt, NULL);

	//initialize undo arrangements
	_e2edtdlg_undo_initialize (rt->textbuffer, rt);

	//highest priority - arrange for key-translations from locale
#ifdef USE_GTK3_0
	g_signal_connect (G_OBJECT (rt->dialog), "key-press-event",
		G_CALLBACK (e2_window_key_cb), GINT_TO_POINTER(1));	//includes translation, non-NULL data to prevent disconnection by bindings
	g_signal_connect (G_OBJECT (rt->dialog), "key-release-event",
		G_CALLBACK (e2_window_key_cb), NULL);
#else
	g_signal_connect (G_OBJECT (rt->dialog), "key-press-event",
		G_CALLBACK (e2_utils_key_translate_cb), GINT_TO_POINTER(1));
	g_signal_connect (G_OBJECT (rt->dialog), "key-release-event",
		G_CALLBACK (e2_utils_key_translate_cb), NULL);
#endif
#ifdef E2_TRANSIENTBINDINGS
	//add dialog-specific key bindings, before the key-press callback
	//see also dialog-general
	//group name (must be freeable)
	gchar *category = g_strconcat (_C(17),".",_C(11),".",_A(46),NULL);	//_(general.dialogs.edit
	e2_keybinding_enrol (GTK_WIDGET (rt->textview), category, _e2_edit_dialog_keybindings);
# ifdef E2_MOUSECUSTOM
	e2_mousebinding_enrol (rt->dialog, category, _e2_edit_dialog_mousebindings);
#  ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (rt->dialog, category, _e2_edit_dialog_mousegestures);
#  endif
# endif
	g_free (category);
#endif
	//this cb does generic stuff and is needed with or without E2_MOUSECUSTOM
	g_signal_connect (G_OBJECT (rt->textview), "button-press-event",
		G_CALLBACK (_e2_edit_dialog_button_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->textview), "popup-menu",
		G_CALLBACK (_e2_edit_dialog_popup_menu_cb), rt);
	g_signal_connect (G_OBJECT (rt->textview), "key-press-event",
		G_CALLBACK (e2_edit_dialog_key_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->textbuffer), "modified-changed",
		G_CALLBACK (_e2_edit_dialog_dirty_change_cb), rt);

	gtk_widget_show_all (dialog_vbox);
	gtk_widget_hide (rt->panel);
	gtk_widget_hide (rt->info_label);
	gtk_widget_hide (rt->findbtn);
	gtk_widget_hide (rt->replacebtn);

	if (srt != NULL)
		*srt = rt;	//make rt available to re-edit func
}

  /******************/
 /***** action *****/
/******************/

/**
@brief re-open file for editing, at the last-used location in the file
This returns immediately, so if run in a Q-thread, that will end
@param from the widget that was activated to initiate the action
@param art runtime data for the action
@return TRUE if the dialog was opened
*/
static gboolean _e2_edit_dialog_reopen (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_do_task (E2_TASK_EDIT, art, from,
		_e2_edit_dialog_reeditQ, NULL));
}
static gboolean _e2_edit_dialog_reeditQ (E2_ActionTaskData *qed)
{
	gboolean retval;
	//process the 1st selected item in active pane
	//FIXME allow specification of a name
	GPtrArray *names = qed->names;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//".." entries filtered when names compiled
	//for name comparisons, we need the full path
	gchar *localpath = e2_utils_strcat (qed->currdir, (*iterator)->filename);
	if (e2_option_bool_get ("use-external-editor"))
	{
		gchar *editor = e2_option_str_get ("command-editor");
		if (*editor != '\0')
		{
			gchar *utf = F_FILENAME_FROM_LOCALE (localpath);
			gchar *command = e2_utils_replace_name_macros (editor, utf);
			if (command == editor)	//no replacement of macro in editor
			{
//tag E2_BADQUOTES
				//CHECKME may be bad if a file.view action is supplied
				gchar *qp = e2_utils_quote_string (utf);
				command = g_strconcat (editor, " ", qp, NULL);
				g_free (qp);
			}
			CLOSEBGL
			gint res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, curr_view->treeview
#ifdef E2_COMMANDQ
			, TRUE
#endif
			);
			OPENBGL
			F_FREE (utf, localpath);
			g_free (command);
			retval = (res == 0);
		}
		else
			//FIXME warn user
			retval = FALSE;
	}
	else
	{
#ifdef E2_VFS
		VPATH ddata = { localpath, qed->currspace };
#endif
		E2_ViewDialogRuntime *vrt = NULL;
		CLOSEBGL
#ifdef E2_VFS
		_e2_edit_dialog_create (&ddata, NULL, &vrt); //no error check for a reload
#else
		_e2_edit_dialog_create (localpath, NULL, &vrt); //no error check for a reload
#endif
		if (vrt != NULL)
		{
			gtk_text_view_set_buffer (GTK_TEXT_VIEW (vrt->textview), vrt->textbuffer);
			g_object_unref (G_OBJECT (vrt->textbuffer)); //destroy buffer with view
			e2_view_dialog_show_atlast
#ifdef E2_VFS
				(&ddata, vrt);
#else
				(localpath, vrt);
#endif
			retval = TRUE;
		}
		else
			retval = FALSE;
		OPENBGL
	}
	g_free (localpath);
	return retval;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief create and show edit dialog
@a filename may not have any path, or an absolute path.
In those cases, curr_view->dir will be prepended
This returns immediately, so if run in a Q-thread, that will end
Expects BGL closed
@param localpath localised string which has at least the name of item to be processed, or NULL
@param buf pointer to a GtkTextBuffer to edit an output pane tab, or NULL
@return TRUE if the dialog was created
*/
gboolean e2_edit_dialog_create (VPATH *localpath, GtkTextBuffer *buf)
{
	//there is no async thread protection here, as this func can be called from
	//different contexts - any protection is done by the caller
	E2_ViewDialogRuntime *vrt;
	if (localpath != NULL || buf != NULL)
	{
		_e2_edit_dialog_create (localpath, buf, &vrt);
		if (vrt != NULL)
		{
			e2_dialog_setup (vrt->dialog, app.main_window);
			gtk_widget_show (vrt->dialog);
			gtk_text_view_set_buffer (GTK_TEXT_VIEW (vrt->textview), vrt->textbuffer);
			if (buf == NULL) //editing file
				g_object_unref (G_OBJECT (vrt->textbuffer)); //destroy with view when editing a file
			return TRUE;
		}
	}
	return FALSE;
}
/**
@brief register action related to edit dialog
@return
*/
void e2_edit_dialog_actions_register (void)
{
	E2_Action action =
	{g_strconcat(_A(6),".",_A(47),NULL),_e2_edit_dialog_reopen,FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL};
	e2_action_register (&action);
#ifdef E2_SPELLCHECK
	//HACK while we're at it, init some session parameters
	e2_cache_list_register ("language-history", &lang_history);
#endif
}
/**
@brief register config option related to edit dialog
@return
*/
void e2_edit_dialog_options_register (void)
{
	gchar *group_name = g_strconcat(_C(11),":",_A(46),NULL); //_("dialogs:edit"
	//first some options that may, but probably won't, change during the session
	e2_option_bool_register ("edit-save-backup", group_name, _("backup when saving"),
		_("When saving an edited file, an existing file with the same name will be renamed"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREEGROUP);
}
