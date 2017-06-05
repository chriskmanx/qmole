/* $Id: e2_view_dialog.c 2901 2013-10-31 22:31:16Z tpgww $

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

#include "emelfm2.h"
#include <string.h>
#include "e2_dialog.h"
#include "e2_view_dialog.h"
#include "e2_textiter.h"
#include "e2_task.h"
#include "e2_print.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_icons.h"

//enable user to specify file encoding(s) to test before opening
//#define CHOOSE_CODING
//enable encoding check for start of file before using external encoder
//#define SNIFF_CODING //must also manually enable e2_fs_get_file_start()

//stores for session-static and/or cached variables
gboolean case_sensitive;
gboolean search_backward;
gboolean whole_words;
gboolean search_wrap;
GList *find_history = NULL;
GList *view_history = NULL;
//keycodes corresponding to mnemonics for translated labels
guint find_keycode;
guint hide_keycode;
//keycodes corresponding to translated 'hardcode-keys'
guint findnext_keycode;
//we need this one because of widget-sharing with the edit dialog
//value set to 0 for a view dialog
extern guint replace_keycode;

static void _e2_view_dialog_response_cb
	(GtkDialog *dialog, gint response, E2_ViewDialogRuntime *rt);
static void _e2_view_dialog_toggled_cb2 (GtkToggleButton *button, gboolean *store);
static gboolean _e2_view_dialog_key_press_cb (GtkWidget *widget, GdkEventKey *event,
	E2_ViewDialogRuntime *rt);
static gboolean _e2_view_dialog_reviewQ (E2_ActionTaskData *qed);
static gboolean _e2_view_dialog_viewatQ (E2_ActionTaskData *qed);

  /**************************/
 /**** search functions ****/
/**************************/

#ifdef E2_MARK_FINDS
/**
@brief buffer-changed callback
This is to turn off all highlighting of matched strings
@param textbuffer the textbuffer which was changed
@param rt dialog runtime data struct

@return
*/
static void _e2_view_dialog_buffer_changed_cb (GtkTextBuffer *textbuffer,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	e2_view_dialog_clear_hilites (rt);
	NEEDOPENBGL
}
/**
@brief setup to highlight all occurrences of a string in the buffer
@param rt dialog runtime data struct
@return
*/
void e2_view_dialog_init_hilites (E2_ViewDialogRuntime *rt)
{
	gtk_text_buffer_create_tag (rt->textbuffer, "matched", "background",
		e2_option_str_get ("color-highlight"), NULL);
	rt->is_lit = FALSE;
}
/**
@brief by forward scan, highlight all occurrences of a string in the buffer
The current state of the case-insensitive flag is used when searching
@param searchstr the string to find
@param rt dialog runtime data struct
@return
*/
static void _e2_view_dialog_set_hilites (const gchar *searchstr,
	E2_ViewDialogRuntime *rt)
{
	static gchar *prior_search = NULL;
	//when some search-options are changed, need to cleanup first, then re-search
	if (rt->research)
		rt->research = FALSE;
	//avoid repeated scans for the same string
	else if (rt->is_lit && !strcmp (searchstr, prior_search))
		return;
	//clear anything from last scan
	e2_view_dialog_clear_hilites (rt);

	if (prior_search != NULL)
		g_free (prior_search);
	prior_search = g_strdup (searchstr);

	GtkTextIter start, end, iter;
	gtk_text_buffer_get_bounds (rt->textbuffer, &start, &end);

	E2TextSearchFlags flags = E2_SEARCH_TEXT_ONLY;	//superset of GtkTextSearchFlags
	if (!rt->case_sensitive)
		flags |= E2_SEARCH_CASE_INSENSITIVE;
	if (rt->whole_words)
		flags |= E2_SEARCH_WHOLE_WORD;

	gboolean found = FALSE;
	gboolean any = !rt->whole_words;
	iter = start;
	while (e2_iter_forward_search (&iter, searchstr, flags, &start, &end, NULL))
	{
		//as we only check for word-starts in the search func, ensure end is ok if relevant
		if (any || gtk_text_iter_ends_word (&end))
		{
			found = TRUE;
			gtk_text_buffer_apply_tag_by_name (rt->textbuffer, "matched", &start, &end);
		}
		iter = end;
	}
	rt->is_lit = found;
	if (found && rt->replacebar != NULL)	//in edit mode
		g_signal_connect (G_OBJECT (rt->textbuffer), "changed",
			G_CALLBACK(_e2_view_dialog_buffer_changed_cb), rt);
}
/**
@brief extinguish highlighting of matched strings
@param rt dialog runtime data struct

@return
*/
void e2_view_dialog_clear_hilites (E2_ViewDialogRuntime *rt)
{
	if (rt->is_lit)
	{
		GtkTextIter start, end;
		gtk_text_buffer_get_bounds (rt->textbuffer, &start, &end);
		gtk_text_buffer_remove_tag_by_name (rt->textbuffer, "matched", &start, &end);
		rt->is_lit = FALSE;
		if (rt->replacebar != NULL)	//in edit mode
			g_signal_handlers_disconnect_by_func (G_OBJECT (rt->textbuffer),
				_e2_view_dialog_buffer_changed_cb, rt);
	}
}
/**
@brief refresh highlighting of matched strings
@param rt dialog runtime data struct

@return
*/
static void _e2_view_dialog_relite (E2_ViewDialogRuntime *rt)
{
	if (rt->is_lit)
	{
		GtkWidget *entry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (rt->combo));
#else
			GTK_BIN (rt->combo)->child;
#endif
		const gchar *find = gtk_entry_get_text (GTK_ENTRY (entry));
		if (*find == '\0')
			e2_view_dialog_clear_hilites (rt);
		else
		{
			rt->research = TRUE;	//trigger re-highlighting of matches
			_e2_view_dialog_set_hilites (find, rt);
		}
	}
}
#endif	//def E2_MARK_FINDS
/**
@brief update display of entry for @a combo, when it is (re)displayed

@param combo combobox widget to be processed

@return
*/
void e2_view_dialog_update_combo (GtkWidget *combo)
{
	GtkWidget *child =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (combo));
#else
		GTK_BIN (combo)->child;
#endif
	if (e2_option_bool_get ("dialog-search-show-last")
		&& e2_combobox_has_history (GTK_COMBO_BOX (combo)))
			gtk_editable_select_region (GTK_EDITABLE (child), 0, -1);
	else
		gtk_entry_set_text (GTK_ENTRY (child), "");
}
/**
@brief set textbuffer marks related to searching

@param buffer textbuffer to be searched
@param start pointer to textiter at location for start-mark
@param end pointer to textiter at location for end-mark

@return
*/
static void _e2_view_dialog_attach_search_iters (GtkTextBuffer *buffer,
	GtkTextIter *start, GtkTextIter *end)
{
	GtkTextMark *mark = gtk_text_buffer_get_mark (buffer, "e2-search-start");
	if (mark == NULL)
		gtk_text_buffer_create_mark (buffer, "e2-search-start", start, FALSE);
	else
		gtk_text_buffer_move_mark (buffer, mark, start);
	mark = gtk_text_buffer_get_mark (buffer, "e2-search-end");
	if (mark == NULL)
		gtk_text_buffer_create_mark (buffer, "e2-search-end", end, FALSE);
	else
		gtk_text_buffer_move_mark (buffer, mark, end);
}
/**
@brief clear textbuffer marks related to searching, if they exist

@param buffer textbuffer which has been searched

@return
*/
static void _e2_view_dialog_unattach_search_iters (GtkTextBuffer *buffer)
{
	GtkTextMark *mark = gtk_text_buffer_get_mark (buffer, "e2-search-start");
	if (mark != NULL)
		gtk_text_buffer_delete_mark (buffer, mark);
	mark = gtk_text_buffer_get_mark (buffer, "e2-search-end");
	if (mark != NULL)
		gtk_text_buffer_delete_mark (buffer, mark);
}
/**
@brief initiate search process after setting relevant flags

@param search_iter pointer to iter where the search is to start
@param start pointer to iter to store the start of a matched string
@param end pointer to iter to store the end of a matched string
@param find the string to search for, may include 1 or more '\n'
@param incremental TRUE when performing an incremental search after a search-string keypress
@param rt pointer to dialog runtime data struct

@return TRUE if a match was found
*/
static gboolean _e2_view_dialog_dosearch (GtkTextIter *search_iter,
	GtkTextIter *start, GtkTextIter *end, const gchar *find, gboolean incremental,
	E2_ViewDialogRuntime *rt)
{
	E2TextSearchFlags flags = E2_SEARCH_TEXT_ONLY;	//superset of GtkTextSearchFlags
	if (!rt->case_sensitive)
		flags |= E2_SEARCH_CASE_INSENSITIVE;
	if (rt->whole_words)
		flags |= E2_SEARCH_WHOLE_WORD;
	if (rt->search_backward)
	{
		//try for incremental match of a current string in same place
		//FIXME find a more elegant way of doing this
		GtkTextIter next = *search_iter;
		if (incremental && !gtk_text_iter_forward_line (&next))
			next = *search_iter;	//revert after failure
		return (e2_iter_backward_search (&next, find, flags, start, end, NULL));
	}
	else
		return (e2_iter_forward_search (search_iter, find, flags, start, end, NULL));
}
/**
@brief try to find string and update GUI accordingly

@param first TRUE to initiate scanning from start or end of text, FALSE to "find next"
@param incremental TRUE when performing an incremental scan after a match-string keypress
@param rt pointer to dialog runtime data struct

@return TRUE if a match was found
*/
gboolean e2_view_dialog_search (gboolean first, gboolean incremental,
	E2_ViewDialogRuntime *rt)
{
	GtkTextIter iter;
	// get the find string
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	const gchar *find = gtk_entry_get_text (GTK_ENTRY (entry));
	if (*find == '\0')
	{
#ifdef E2_MARK_FINDS
		e2_view_dialog_clear_hilites (rt);
#endif
		//hide any selection
		if (gtk_text_buffer_get_selection_bounds (rt->textbuffer, &iter, NULL))
			gtk_text_buffer_select_range (rt->textbuffer, &iter, &iter);
		return FALSE;
	}
	if (!incremental)
	{
#ifdef E2_MARK_FINDS
		_e2_view_dialog_set_hilites (find, rt);
#endif
		e2_combobox_prepend_history (rt->combo, find, 20, FALSE);
		e2_list_update_history (&find_history, find, NULL, 20, FALSE);
	}
#ifdef E2_MARK_FINDS
	else
		//turn off any highlihts
		e2_view_dialog_clear_hilites (rt);
#endif

	//check if the TextView's buffer has changed and get new TextIters
	GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (rt->textview));
/*	if (buffer != rt->textbuffer)
	{
		rt->textbuffer = buffer;
		unattach_iters (buffer);
		search_iter = NULL;
	}
*/
	GtkTextMark *mark = NULL;
	if (first)
	{	//searching starts from cursor, so move that
		_e2_view_dialog_unattach_search_iters (buffer);	//(re)start searching
		if (rt->search_backward)
			gtk_text_buffer_get_end_iter (buffer, &iter);
		else
			gtk_text_buffer_get_start_iter (buffer, &iter);
		gtk_text_buffer_place_cursor (buffer, &iter);
	}
	else
	{	//searching from cursor, not from start or end of buffer
		gboolean fromstart = (incremental) ? TRUE : rt->search_backward;
		if (fromstart)
			mark = gtk_text_buffer_get_mark (buffer, "e2-search-start");
		else
			mark = gtk_text_buffer_get_mark (buffer, "e2-search-end");
	}

	GtkTextIter *search_iter = NULL;
	if (mark != NULL)
	{
		gtk_text_buffer_get_iter_at_mark (buffer, &iter, mark);
		search_iter = &iter;
	}
	if (search_iter == NULL)
	{
		//search from cursor
		mark = gtk_text_buffer_get_mark (buffer, "insert");
		gtk_text_buffer_get_iter_at_mark (buffer, &iter, mark);
		search_iter = &iter;
/* MAYBE search from top/bottom of window if cursor is off-window ?
		GdkRectangle visible;
		gtk_text_view_get_visible_rect (GTK_TEXT_VIEW (rt->textview),
			&visible);
		if (rt->opt_search_backward)
			//search backward from end of screen
			gtk_text_view_get_iter_at_location (GTK_TEXT_VIEW (rt->textview),
				&iter, visible.width, visible.height);
		else
			//search forward from top of screen
			gtk_text_view_get_iter_at_location (GTK_TEXT_VIEW (rt->textview),
				&iter, visible.x, visible.y); */
	}

	// search for the string
	GtkTextIter start, end;
	gboolean found = _e2_view_dialog_dosearch (search_iter, &start, &end, find,
		incremental, rt);

	if (!found && rt->search_wrap)
	{
		_e2_view_dialog_unattach_search_iters (buffer);
		if (rt->search_backward)
			gtk_text_buffer_get_end_iter (buffer, &iter);
		else
			gtk_text_buffer_get_start_iter (buffer, &iter);
		found = _e2_view_dialog_dosearch (&iter, &start, &end, find, incremental, rt);
	}
	if (found)
	{
		gtk_widget_hide (rt->info_label);
		gtk_text_buffer_select_range (buffer, &start, &end);	//always show where we are now
		_e2_view_dialog_attach_search_iters (buffer, &start, &end);
		//scroll found position into window, if not there already
		GdkRectangle visible_rect;	//, iter_rect;
		gtk_text_view_get_visible_rect (GTK_TEXT_VIEW (rt->textview), &visible_rect);

		gtk_text_view_get_iter_at_location (GTK_TEXT_VIEW (rt->textview), &iter,
			visible_rect.x, visible_rect.y);
		gtk_text_view_get_iter_at_location (GTK_TEXT_VIEW (rt->textview), &end,
			visible_rect.x + visible_rect.width, visible_rect.y + visible_rect.height);
		if (!gtk_text_iter_in_range (&start, &iter, &end))
/* alternative approach, using vertical check only
		gtk_text_view_get_iter_location (GTK_TEXT_VIEW (rt->textview), &start, &iter_rect);
		if ((iter_rect.y + iter_rect.height) > (visible_rect.y + visible_rect.height)
			|| iter_rect.y < visible_rect.y)
*/
			gtk_text_view_scroll_to_iter (GTK_TEXT_VIEW (rt->textview), &start,
				0.0, TRUE, 0.1, (rt->search_backward) ? 0.8 : 0.2);
		return TRUE;
	}
	gtk_widget_show (rt->info_label);
	return FALSE;
}

  /*****************/
 /***** utils *****/
/*****************/

#ifdef CHOOSE_CODING
typedef struct
{
	guint testcount;
	GtkWidget *msg_label;
	GtkWidget *chk_btns [3][8];
	GtkWidget *test_btn;
	GtkWidget *open_btn;
	const gchar *first_coding;
} E2CodeDlgRuntime;

static void _e2_view_dialog_lang_toggle (GtkToggleButton *button,
	E2CodeDlgRuntime *rt)
{
	if (gtk_toggle_button_get_active (button))
	{
		if (++rt->testcount == 1)
			gtk_widget_set_sensitive (rt->test_btn, TRUE);
	}
	else
	{
		if (--rt->testcount == 0)
			gtk_widget_set_sensitive (rt->test_btn, FALSE);
		//user might have de-selected the locale-default
		if (GTK_WIDGET(button) == rt->chk_btns[0][0])
			rt->first_coding = NULL; //FIXME only if this is currently set to default
	}
}
#endif

/**
@brief read text file to be viewed, and put it to created @a rt ->textbuffer
This is used by both view and edit dialogs.
Assumes the item being read is in curr_view i.e. no cd race from queueing
Character encoding is performed if necessary and possible.
@a rt ->filepath has the localised name of the file to be read. It will be
replaced by a newly-allocated string, to be freed elsewhere
Assumes BGL is closed
@param localfile data for item to read, localised path string, not neccessarily absolute
@param rt dialog runtime data struct

@return TRUE if the read and conversion was successful
*/
gboolean e2_view_dialog_read_text (VPATH *localfile, E2_ViewDialogRuntime *rt)
{
	E2_ERR_DECLARE
	gchar *localpath;
	if (!g_path_is_absolute (VPSTR (localfile)))
		localpath = e2_utils_dircat (curr_view, VPCSTR (localfile), TRUE);
	else
		localpath = g_strdup (VPSTR (localfile));	//copy in case we want to keep it
#ifdef E2_VFS
	VPATH ddata = { localpath, localfile->spacedata };
	if (e2_fs_access (&ddata, R_OK E2_ERR_PTR()))	//traverse a link
#else
	if (e2_fs_access (localpath, R_OK E2_ERR_PTR()))	//traverse a link
#endif
	{
		OPENBGL
		e2_fs_error_local (_("Cannot read '%s'"),
#ifdef E2_VFS
		&ddata E2_ERR_MSGL());
#else
		localpath E2_ERR_MSGL());
#endif
		CLOSEBGL
		E2_ERR_CLEAR
		g_free (localpath);
		return FALSE;
	}
#ifdef E2_VFS
	if (e2_fs_is_dir3 (&ddata E2_ERR_NONE()))
#else
	if (e2_fs_is_dir3 (localpath E2_ERR_NONE()))
#endif
	{
		e2_fs_error_simple (_("'%s' is a directory"),
#ifdef E2_VFS
		&ddata);
#else
		localpath);
#endif
		g_free (localpath);
		return FALSE;
	}

	gboolean usable;
	gulong length;
	gpointer contents;
	gchar *utfconverter = NULL;
	if (e2_option_bool_get ("use-external-encoder"))
	{
		utfconverter = e2_option_str_get ("command-encoder");
		if (*utfconverter != '\0')
		{
#ifdef SNIFF_CODING
			//sniff the file to check if conversion is really needed
			//this is bad for files with mixed encoding !
			OPENBGL
			if (!e2_fs_get_file_start (
# ifdef E2_VFS
				&data,
# else
				localpath,
# endif
				&contents, 1024, TRUE E2_ERR_MSGL()))
			{
				e2_fs_error_local (_("Cannot read %s"), localpath E2_ERR_MSGL());
				CLOSEBGL
				E2_ERR_CLEAR
				g_free (localpath);
				return FALSE;
			}
			CLOSEBGL

			if (*(guchar *)contents != '\0')
				rt->charset = e2_utf8_detect_charset ((guchar *)contents, &usable);
			else
				usable = TRUE;
# ifdef USE_GLIB2_10
			g_slice_free1 (1024, contents);
# else
			g_free (contents);
# endif
			if (usable)
			{
				utfconverter = NULL;	//signal no external conversion needed
				if (rt->charset == NULL)
					rt->charset = "UNKNOWN";	//no translation
			}
			else
			{	//we need to do a conversion
#endif //def SNIFF_CODING
				gchar *utfpath = F_FILENAME_FROM_LOCALE (localpath);
				//CHECKME don't quote utfpath, the macro should handle that
				//substitute file for any name/path macro in command string, get language etc
				gchar *utfcmd = e2_utils_expand_macros (utfconverter, utfpath);
				F_FREE (utfpath, localpath);

				if (utfcmd == NULL || utfcmd == GINT_TO_POINTER(1))
				{
					g_free (localpath);
					return FALSE;
				}
				else
				{
					gchar *command;
					if (!strcmp (utfcmd, utfconverter))
					{	//macro expansion didn't do anything
						gchar *tmp2 = F_FILENAME_TO_LOCALE (utfconverter);
						gchar *qp = e2_utils_quote_string (localpath);
						command = g_strconcat (tmp2, " ", qp, NULL);
						g_free (qp);
						F_FREE (tmp2, utfconverter);
					}
					else
						command = D_FILENAME_TO_LOCALE (utfcmd);

					rt->charset = "UNKNOWN";	//no translation
					if (!e2_fs_get_command_output (command, &contents))
					{
						gchar *msg = g_strdup_printf
						(_("Encoding conversion command '%s' failed"), utfcmd);
						e2_output_print_error (msg, TRUE);
						utfconverter = NULL;	//set flag for internal conversion fallback
					}
					g_free (utfcmd);
					g_free (command);
				}
#ifdef SNIFF_CODING
			}
#endif
		}
		else
			utfconverter = NULL;
	}

	if (utfconverter == NULL	//not externally converted
#ifdef E2_VFS
		&& !e2_fs_get_file_contents (&ddata, &contents, &length, TRUE E2_ERR_PTR()))
#else
		&& !e2_fs_get_file_contents (localpath, &contents, &length, TRUE E2_ERR_PTR()))
#endif
	{
		e2_fs_error_local (_("Error reading file %s"),
#ifdef E2_VFS
		 &ddata E2_ERR_MSGL());
#else
		 localpath E2_ERR_MSGL());
#endif
		E2_ERR_CLEAR
		g_free (localpath);
		return FALSE;
	}

	if (length > 0)
	{
		//fix CR's in the text if necessary
		rt->linebreak = e2_utils_LF_line_ends ((gchar *)contents);
		//fix character encoding if necessary and possible
		if (utfconverter == NULL)	//not externally converted
		{
			gchar *format = _("Conversion from %s encoding failed: \"%s\"");
			gchar *msg, *utf;
			GError *utf_error;
			rt->charset = e2_utf8_detect_charset ((guchar *)contents, &usable);
			if (!usable)
			{
				if (rt->charset == NULL)
				{
#ifndef CHOOSE_CODING
					//guess that the file originates in the user's locale
					rt->charset = e2_utf8_guess_charset ((guchar *)contents);
					printd (DEBUG, "Assuming file is from user's locale, encoding %s", rt->charset);
					if (rt->charset == NULL)
						rt->charset = "ISO-8859-15";	//default
#else
					extern const gchar *encoding_table [ENCODE_COUNT][ENCODING_TYPECOUNT];
					const gchar *coding_names [3][8] =
					{
						{"Locale-default",
						 "Arabic", "Cyrilic", "Cyrilic_TJ", "Cyrilic_UA",
						 "Georgian", "Greek-modern", "Hebrew-modern"},
						{"Latin2", "Latin3", "Latin5", "Latin6", "Latin7",
						 "Latin8", "Latin9", "Latin10"},
						{"Chinese_CN", "Chinese_HK", "Chinese_TW",
						 "Japanese", "Korean", "Thai", "Vietnamese", NULL},
					};
					guint coding_vals [3][8] =
					{
						{ENCODE_COUNT,ARABIC,CYRILIC,CYRILIC_TJ,CYRILIC_UA,GEORGIAN,GREEK,HEBREW},
						{LATIN2,LATIN3,LATIN5,LATIN6,LATIN7,LATIN8,LATIN9, LATIN10},
						{CHINESE_CN,CHINESE_HK,CHINESE_TW,JAPANESE,KOREAN,THAI,VIETNAMESE,ENCODE_COUNT},
					};
					gint row, col;
					GtkWidget *button, *table;
					E2CodeDlgRuntime drt;
					memset (&drt, 0, sizeof (E2CodeDlgRuntime));
					//dialog that allows user to choose encoding(s) to test
					DialogButtons choice;
					GtkWidget *dialog = e2_dialog_create (NULL, NULL, _("choose file encoding"),
							DUMMY_RESPONSE_CB, NULL);
					GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
						gtk_dialog_get_content_area (GTK_DIALOG (dialog));
#else
						GTK_DIALOG (dialog)->vbox;
#endif
					gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);
					gchar *utfpath = F_DISPLAYNAME_FROM_LOCALE (localpath);
					e2_widget_add_mid_label (dialog_vbox, utfpath, 0.5, TRUE, 0);
					F_FREE (utfpath, localpath);
					//label which shows prompt, later the result of any test
					drt.msg_label = e2_widget_add_mid_label (dialog_vbox,
						_("Select one or more encodings to test"), 0.5, TRUE, 0);
					//setup table of check-buttons with guessed one of them activated
					table = e2_widget_add_table (dialog_vbox, 8, 3, FALSE, TRUE, 0);
					for (row = 0; row < 3; row++)
					{
						for (col = 0; col < 8; col++)
						{
							if (coding_names [row][col] != NULL)
							{
								button = gtk_check_button_new_with_label (coding_names [row][col]);
								g_signal_connect (G_OBJECT(button), "toggled",
									G_CALLBACK (_e2_view_dialog_lang_toggle), &drt);
#ifdef USE_GTK3_2
								gtk_grid_attach (GTK_GRID (table), button, row, col, 1, 1);
#else
								gtk_table_attach_defaults (GTK_TABLE (table), button,
									row, row+1, col, col+1); //swapped r,c
#endif
								drt.chk_btns[row][col] = button;
							}
						}
					}
					//enable the default locale so that user can just open
					gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(drt.chk_btns[0][0]), TRUE);
					drt.testcount = 1;
					drt.first_coding = e2_utf8_guess_charset ((guchar *)contents);
					if (drt.first_coding == NULL)
						drt.first_coding = "ISO-8859-15";	//default
					//buttons
					drt.test_btn = e2_dialog_add_custom_button_full
					  ( dialog, FALSE, E2_BUTTON_APPLYTOALL.response,
						_("_Test"), STOCK_NAME_EXECUTE, NULL, NULL, NULL);
//					gtk_widget_set_sensitive (drt.test_btn, FALSE);
					e2_dialog_add_defined_button (dialog, &E2_BUTTON_CANCEL);
					drt.open_btn = e2_dialog_add_custom_button_full
					  ( dialog, FALSE, E2_BUTTON_APPLY.response,
						_("_Open"), STOCK_NAME_OK, NULL, NULL, NULL);
//					gtk_widget_set_sensitive (drt.open_btn, FALSE);
					e2_dialog_show (dialog, app.main_window, E2_DIALOG_CLOSELOCK, NULL);
rewait:
					choice = e2_dialog_wait (dialog, FALSE, TRUE, FALSE, TRUE);
					switch (choice)
					{
						const gchar *charset;
						default:
//						case NO_TO_ALL: //cancel
							if (GTK_IS_DIALOG (dialog)) //not explicitly closed by the user
								gtk_widget_destroy (dialog);
							g_free (localpath);
							g_free (contents);
							return FALSE;
						case YES_TO_ALL: //test
							e2_dialog_set_cursor (dialog, GDK_WATCH);
							//iterate through each selection, try to convert
							gboolean match = FALSE;
							//get selected charset(s)' name
							for (row = 0; row < 3; row++)
							{
								for (col = 0; col < 8; col++)
								{
									if (drt.chk_btns[row][col] != NULL &&
										gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(drt.chk_btns[row][col])))
									{
										guint j, type;
										if (row == 0 && col == 0)
										{
											//special-case: default for locale
											j = 0; //FIXME
											charset = e2_utf8_guess_charset ((guchar *)contents);
											utf = g_convert ((gchar *)contents, -1, "UTF-8", charset, NULL, NULL, NULL);	//&utf_error);
											if (utf != NULL)
											{
												g_free (utf);
												drt.first_coding = charset;
												match = TRUE;
												break;
											}
										}
										else
										{
											j = coding_vals[row][col];
										}

										for (type = 0; type < ENCODING_TYPECOUNT; type++)
										{
											charset = encoding_table[j][type];
											if (charset == NULL)
												continue;
											utf = g_convert ((gchar *)contents, -1, "UTF-8", charset, NULL, NULL, NULL);	//&utf_error);
											if (utf != NULL)
											{
												printd (DEBUG, "converted file content from %s to UTF8", charset);
												g_free (utf);
												if (drt.first_coding == NULL)
													drt.first_coding = charset;
												match = TRUE;
												//update lower label in dialog
												gtk_label_set_text (GTK_LABEL(drt.msg_label),
													_("The file encoding may be any of the selected ones"));
												gtk_widget_set_sensitive (drt.open_btn, TRUE);
												break;
											}
											else
												printd (DEBUG, "FAILED file conversion from %s/%s to UTF8", coding_names[row][col],charset);
										}
										if (type == ENCODING_TYPECOUNT)
										{
											//uncheck the corresponding button
											gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(drt.chk_btns[row][col]), FALSE);
										}
									}
								}
								if (col < 8)
									break;
							}
							if (!match) //no match, update label accordingly
							{
								gtk_label_set_text (GTK_LABEL(drt.msg_label),
									_("File encoding cannot be converted from any selected type"));
								gtk_widget_set_sensitive (drt.open_btn, FALSE);
								drt.first_coding = NULL;
							}

							e2_dialog_set_cursor (dialog, GDK_LEFT_PTR);
							goto rewait;
						case OK: //open
							gtk_widget_destroy (dialog);
							//proceed to conversion
							if (drt.first_coding == NULL)
								drt.first_coding = "ISO-8859-15";	//default
							rt->charset = drt.first_coding;
							usable = TRUE;
							break;
					}
#endif
				}
				//convert using the identified encoding if we can
				//but no point in 'non-conversion' ...
				if (strstr (rt->charset, "UTF-8") == NULL)
				{
					utf_error = NULL;
					utf = g_convert ((gchar *)contents, -1, "UTF-8", rt->charset, NULL, NULL, &utf_error);
					if (utf != NULL)
					{
						printd (DEBUG, "converted content from %s to UTF8", rt->charset);
						usable = TRUE;
						g_free (contents);	//need free() if file buffer allocated by malloc()
						contents = utf;
					}
					else
					{
						printd (DEBUG, "FAILED conversion of content from %s to UTF8", rt->charset);
						if (strcmp (rt->charset, "CP1252"))	//that's generally a default when we can't find the real value
						{
							msg = g_strdup_printf (format, rt->charset, utf_error->message);
							e2_output_print_error (msg, TRUE);
						}
						g_error_free (utf_error);
					}
				}
			}
			if (!usable)
			{	//try default coding as a fallback
				const gchar *defset;
				e2_utils_get_charset (&defset);
				//again, no point in 'non-conversion'
				if (strstr (defset, "UTF-8") != NULL)
					defset = e2_cl_options.fallback_encoding;
				if (strstr (defset, "UTF-8") != NULL)
					defset = "ISO-8859-15";
				printd (DEBUG, "Conversion from %s encoding to be attempted", defset);
				utf_error = NULL;
				utf = g_convert ((gchar *)contents, -1, "UTF-8", defset, NULL, NULL, &utf_error);
				if (utf == NULL)
				{
					msg = g_strdup_printf (format, defset, utf_error->message);
					e2_output_print_error (msg, TRUE);
					g_error_free (utf_error);
					g_free (contents);	//need free() if file buffer allocated by malloc()
					return FALSE;
				}
				else if (!g_utf8_validate (utf, -1, NULL))
				{
					g_free (utf);
					g_free (contents);	//need free() if file buffer allocated by malloc()
					return FALSE;
				}
				rt->charset = "UNKNOWN";	//prevent attempts at saving to the unusable charset
				g_free (contents);	//need free() if file buffer allocated by malloc()
				contents = utf;
			}
		}
	}
	else //assume default encoding for empty files
	{
		e2_utils_get_charset (&rt->charset);
//		printd (DEBUG, "empty file, assume default charset %s", rt->charset);
		contents = NULL;
	}

	rt->textbuffer = gtk_text_buffer_new (NULL); //always create a buffer
	if (contents)
	{
		GtkTextIter iter;
		gtk_text_buffer_set_text (rt->textbuffer, contents, -1);
		g_free (contents);
		//cursor normally set to end of buffer
		gtk_text_buffer_get_start_iter (rt->textbuffer, &iter);
        gtk_text_buffer_place_cursor (rt->textbuffer, &iter);
		printd (DEBUG, "Read text file charset is %s", rt->charset);
	}

	if (rt->localpath != NULL)
		g_free (rt->localpath);
	rt->localpath = localpath;	//we're ok, so remember the real path
#ifdef E2_VFS
	rt->spacedata = localfile->spacedata;
#endif

	return TRUE;
}
/**
@brief set dialog font and get size of a typical character in that font

@param char_width store for character width for font
@param char_height store for character height for font
@param rt pointer to dialog data struct

@return
*/
void e2_view_dialog_set_font (gint *char_width, gint *char_height, E2_ViewDialogRuntime *rt)
{
	gchar *fntname;
	if (e2_option_bool_get ("dialog-view-use-font"))
	{
		fntname = e2_option_str_get ("dialog-view-font");
		if (*fntname == '\0')
			fntname = NULL;
	}
	else
		fntname = NULL;
	if (fntname == NULL)
	{
		GtkSettings* defs = gtk_settings_get_default ();
		g_object_get (G_OBJECT (defs), "gtk-font-name", &fntname, NULL);
		if (fntname == NULL)	//CHECKME needed ?
		{
			printd (WARN, "No default font detected");
			fntname = "Sans 10";
		}
	}
	e2_widget_set_font (rt->textview, fntname);
	e2_widget_get_font_pixels (rt->textview, char_width, char_height);
}
/**
@brief set popup menu position

This function is supplied when calling gtk_menu_popup(), to position
the displayed menu.
Set @a push_in to TRUE for menu completely inside the screen,
FALSE for menu clamped to screen size

@param menu the GtkMenu to be positioned
@param x	place to store gint representing the menu left
@param y  place to store gint representing the menu top
@param push_in place to store pushin flag
@param rt data struct for dialog in focus when the menu key was pressed

@return
*/
void e2_view_dialog_set_menu_position (GtkWidget *menu,
	gint *x, gint *y, gboolean *push_in, E2_ViewDialogRuntime *rt)
{
	gint left, top;
	gtk_window_get_position (GTK_WINDOW (rt->dialog), &left, &top);
	GtkAllocation alloc;
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (rt->textview, &alloc);
#else
	alloc = rt->textview->allocation;
#endif
	*x = left + alloc.x + alloc.width/2;
	*y = top + alloc.y + alloc.height/2;
	*push_in = FALSE;
}
/**
@brief perform copy

@param menuitem UNUSED the selected widget, or NULL
@param rt runtime struct to work on

@return
*/
static void _e2_view_dialog_copy_cb (GtkMenuItem *menuitem,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	GtkClipboard *cb = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
	gtk_text_buffer_copy_clipboard (rt->textbuffer, cb);
	NEEDOPENBGL
}
/**
@brief construct and pop up destroyable context-menu for the view-dialog

@param textview the textview widget where the click happened
@param event_button which mouse button was clicked (0 for a menu key)
@param event_time time that the event happened (0 for a menu key)
@param rt runtime struct to work on

@return
*/
static void _e2_view_dialog_show_context_menu (GtkWidget *textview,
	guint event_button, guint32 event_time, E2_ViewDialogRuntime *rt)
{
	gchar *item_name;
	GtkWidget *menu = e2_menu_get ();

/*	GtkWidget *item = e2_menu_add (menu, _("_Find"), STOCK_NAME_FIND,
		NULL,  _e2_view_dialog_menu_find_cb, rt);
#ifdef USE_GTK2_12TIPS
	e2_widget_set_toggletip (
#else
	gtk_tooltips_set_tip (app.tooltips,
#endif
		item,
		_("Show the search options bar"), _("Find the first/next occurrence"));

	GtkWidget *submenu = e2_menu_add_submenu (menu, _("_Settings"), STOCK_NAME_PREFERENCES);
	e2_option_create_menu (GTK_WIDGET (textview), submenu,
		opt_view_wrap, NULL, NULL,
 etc
		app.output.opt_show_on_new, NULL, NULL,
		app.output.opt_show_on_focus_in, NULL, NULL,
		NULL);
*/
	GtkWidget *item = e2_menu_add (menu, _("_Copy"), STOCK_NAME_COPY,
		_("Copy selected text"), _e2_view_dialog_copy_cb, rt);
	gtk_widget_set_sensitive (item,
		gtk_text_buffer_get_selection_bounds (rt->textbuffer, NULL, NULL));
#ifdef USE_GTK2_10
	gchar *tip = (gtk_text_buffer_get_has_selection (rt->textbuffer)) ?
		_("Print selected text") : _("Print file");
	e2_menu_add (menu, _("_Print.."), STOCK_NAME_PRINT, tip,
		e2_dialog_print_cb, rt);
#endif
	e2_menu_add_separator (menu);
	item_name = g_strconcat (_A(3),".",_A(34),NULL);
	e2_menu_add_action (menu, _("_Settings"), STOCK_NAME_PREFERENCES,
		_("Open the configuration dialog at the options page"), item_name,
		_C(11)); //_("dialogs")
	g_free(item_name);

	g_signal_connect (G_OBJECT (menu), "selection-done",
		G_CALLBACK (e2_menu_selection_done_cb), NULL);

	if (event_button == 0)
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			(GtkMenuPositionFunc) e2_view_dialog_set_menu_position,
			rt, 0, event_time);
	else
		//this was a button-3 click
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
			NULL, NULL, event_button, event_time);
}
/**
@brief construct and show hbox with search-bar items
Downstream MAY need BGL closed (on gtk2)
@param rt runtime struct to work on

@return hbox for the searchbar
*/
GtkWidget *e2_view_dialog_create_searchbar (E2_ViewDialogRuntime *rt)
{
	rt->is_hidden = TRUE;	//searchbar is not displayed until a search is started
	rt->release_blocked = FALSE;

#ifdef USE_GTK3_0
	GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
#endif
	//add search-string combo
	//CHECKME close BGL to cut latency ?
	//cuz E2_COMBOBOX_MENU_STYLE flag is not set, on gtk2, downstream calls
	//gtk_widget_set_name() which MAY? need BGL closed
	rt->combo = e2_combobox_add (hbox, TRUE, 0, NULL, NULL, &find_history,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_FOCUS_ON_CHANGE);	//| E2_COMBOBOX_NO_AUTO_HISTORY);
	gtk_widget_set_size_request (rt->combo, 230, -1);

	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (rt->combo));
#else
		GTK_BIN (rt->combo)->child;
#endif
	//any signal applied to this widget will also apply to the dialog window
	//as a whole and to any other element of it!!!
	g_signal_connect_after (G_OBJECT (entry), "key-release-event",
		G_CALLBACK (e2_view_dialog_combokey_cb), rt);
	e2_widget_set_safetip (entry, _("Finds"));
	//add search-option buttons
	//if also replacing, to make expandable entries the same width,
	//the buttons need to be in a specific hbox in a size group
	GtkWidget *hbox2;
	if (rt->sgroup != NULL)	//this is setup in edit dialog code
	{
		hbox2 = e2_widget_add_box (hbox, FALSE, 0, FALSE, FALSE, 0);
		gtk_size_group_add_widget (rt->sgroup, hbox2);
	}
	else
		hbox2 = hbox;

	GtkWidget *btn = e2_button_add_toggle (hbox2, TRUE, rt->case_sensitive,
		_("_match case"), _("If activated, text case does matter when searching"),
		FALSE, E2_PADDING_XSMALL, _e2_view_dialog_toggled_cb2, &rt->case_sensitive);
	//flag that extra processing is needed in the callback
	g_object_set_data (G_OBJECT (btn), "e2_dlg_runtime", rt);
	btn = e2_button_add_toggle (hbox2, TRUE, rt->whole_words,
		_("wh_ole words"),_("If activated, matches must be surrounded by word-separator characters"),
		FALSE, E2_PADDING_XSMALL, _e2_view_dialog_toggled_cb2, &rt->whole_words);
	g_object_set_data (G_OBJECT (btn), "e2_dlg_runtime", rt);
	e2_button_add_toggle (hbox2, TRUE, rt->search_backward,
		_("_backward"),_("If activated, searching proceeds toward document start"),
		FALSE, E2_PADDING_XSMALL, e2_view_dialog_toggled_cb, &rt->search_backward);
	e2_button_add_toggle (hbox2, TRUE, rt->search_wrap,
		_("_loop"), _("If activated, searching cycles from either end to the other"),
		FALSE, E2_PADDING_XSMALL, e2_view_dialog_toggled_cb, &rt->search_wrap);
	//allow actions to be initiated when searchbar is focused
	g_signal_connect (G_OBJECT (hbox), "key-press-event",
		G_CALLBACK (_e2_view_dialog_key_press_cb), rt);

	return hbox;
}
/**
@brief setup initial view position, when re-viewing/editing a file

@param localpath data including full path of item being re-opened
@param rt runtime struct to work on

@return
*/
void e2_view_dialog_show_atlast (VPATH *localpath, E2_ViewDialogRuntime *rt)
{
	GtkTextIter top;
	GList *iterator;
	E2_ViewHistory *viewed;
	for (iterator = view_history; iterator != NULL; iterator = iterator->next)
	{
		viewed = (E2_ViewHistory *) iterator->data;
#ifdef E2_VFSTMP
		compare spacedata ?
#endif
		if (!strcmp (viewed->localpath, VPSTR (localpath)))
			break;
	}
	if (iterator != NULL)
	{
		//cannot use buffer coordinates here (bad for all but smallest files)
		gtk_text_buffer_get_iter_at_line (rt->textbuffer, &top, viewed->topline);
		//need to add a mark to get the scroll to work properly
		GtkTextMark *mark = gtk_text_buffer_create_mark
			(rt->textbuffer, NULL, &top, FALSE);
		gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (rt->textview), mark,
			0.0, TRUE, 0.0, 0.0);
	}
	else
		gtk_text_buffer_get_start_iter (rt->textbuffer, &top);

	gtk_text_buffer_place_cursor (rt->textbuffer, &top);

	e2_dialog_show (rt->dialog, app.main_window, E2_DIALOG_DONT_SHOW_ALL, NULL);
	gtk_window_present (GTK_WINDOW (rt->dialog));
}
/**
@brief cleanup when ending a view dialog

@param rt runtime struct to work on

@return
*/
void e2_view_dialog_destroy (E2_ViewDialogRuntime *rt)
{
	//in case we last performed an incremental search ...
	if (!rt->is_hidden)
	{
		GtkWidget *entry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (rt->combo));
#else
			GTK_BIN (rt->combo)->child;
#endif
		const gchar *find = gtk_entry_get_text (GTK_ENTRY (entry));
		if (find != NULL && *find != '\0')
		{
			e2_combobox_prepend_history (rt->combo, find, 20, FALSE);
			e2_list_update_history (&find_history, find, NULL, 20, FALSE);
		}
	}
	//backup some dialog-specific options
	case_sensitive = rt->case_sensitive;
	whole_words = rt->whole_words;
	search_backward = rt->search_backward;
	search_wrap = rt->search_wrap;
	//in case we want to re-view, remember where we are now
	//(need to use line no. as buffer coords are no use when reloading)
	GList *iterator;
	E2_ViewHistory *viewed;
	for (iterator = view_history; iterator != NULL; iterator = iterator->next)
	{
		viewed = (E2_ViewHistory *) iterator->data;
		if (!strcmp (viewed->localpath, rt->localpath))
			break;
	}
	if (iterator == NULL)
	{
		viewed = MALLOCATE (E2_ViewHistory);	//too small for slice, never deallocated
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (viewed, FIXME;)
#else
		if (viewed != NULL)
		{
#ifdef E2_VFSTMP
			spacedata needed ?
#endif
			viewed->localpath = g_strdup (rt->localpath);
			view_history = g_list_append (view_history, viewed);
		}
#endif
	}

	GdkRectangle visible_rect;
	GtkTextIter start;
	gtk_text_view_get_visible_rect (GTK_TEXT_VIEW (rt->textview), &visible_rect);
	gtk_text_view_get_line_at_y (GTK_TEXT_VIEW (rt->textview), &start,
		visible_rect.y, NULL);
//	if ()
//	{
		//when editing the output buffer, we don't want to change it
		GtkTextIter scan;
		guint i = 0;
		do
		{
			gtk_text_buffer_get_iter_at_line (rt->textbuffer, &scan, i++);
		} while (gtk_text_iter_compare (&scan, &start) < 0);
		viewed->topline = i-1;
/*	}
	else
	{
		//get gtk to count the preceding lines by deleting the rest of the conents
		GtkTextIter end;
		gtk_text_buffer_get_end_iter (rt->textbuffer, &end);
		gtk_text_buffer_delete (rt->textbuffer, &start, &end);
		viewed->topline = gtk_text_buffer_get_line_count (rt->textbuffer);
		if (viewed->topline > 0)
			viewed->topline--;
	} */

	//NOTE all dialog bindings cleared during destruction

	gtk_widget_destroy (rt->dialog);
	gtk_widget_grab_focus (curr_view->treeview);	//CHECKME consistency ok ?
	g_free (rt->localpath);
//	if (rt->idle_id != 0)
//		g_source_remove (rt->idle_id);
	DEALLOCATE (E2_ViewDialogRuntime, rt);
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief menu-button press callback

@param textview the textview widget where the press happened
@param rt dialog runtime data struct

@return TRUE always
*/
static gboolean _e2_view_dialog_popup_menu_cb (GtkWidget *textview,
	E2_ViewDialogRuntime *rt)
{
	guint32 event_time = gtk_get_current_event_time ();
	NEEDCLOSEBGL
	_e2_view_dialog_show_context_menu (textview, 0, event_time, rt);
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
static gboolean _e2_view_dialog_button_press_cb (GtkWidget *textview,
	GdkEventButton *event, E2_ViewDialogRuntime *rt)
{
	if (event->button == 3
#ifdef E2_MOUSECUSTOM
		&& (event->state & E2_MODIFIER_MASK) == 0
#endif
		)
	{
		NEEDCLOSEBGL
		_e2_view_dialog_show_context_menu (textview, 3, event->time, rt);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief check-button state change callback

@param button the button widget that changed
@param store pointer to store for the new state of @a button

@return
*/
void e2_view_dialog_toggled_cb (GtkToggleButton *button, gboolean *store)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	*store =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (button);
#else
		button->active;
#endif
}
/**
@brief alternative check-button state change callback
This applies to whole-word and case-match buttons, which need marking to be
refreshed after change
@param button the button widget that changed
@param store pointer to store for the new state of @a button

@return
*/
static void _e2_view_dialog_toggled_cb2 (GtkToggleButton *button, gboolean *store)
{
	*store =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (button);
#else
		button->active;
#endif
#ifdef E2_MARK_FINDS
	E2_ViewDialogRuntime *rt = g_object_get_data (G_OBJECT (button), "e2_dlg_runtime");
	if (rt != NULL)
		_e2_view_dialog_relite (rt);
#endif
}
/**
@brief key press callback for some elements of the dialog

@param widget UNUSED the widget where the key was pressed
@param event gdk event data
@param rt rt data for the dialog

@return TRUE (stop other handlers) for recognised keys
*/
static gboolean _e2_view_dialog_key_press_cb (GtkWidget *widget, GdkEventKey *event,
	E2_ViewDialogRuntime *rt)
{
	printd (DEBUG, "_e2_view_dialog_key_press_cb widget: %x event: %x data: %x, key: %x",
		widget, event, rt, event->keyval);
	guint mask = event->state; //& gtk_accelerator_get_default_mod_mask ();
	if (//g_ascii_isalpha (event->keyval) //&& (event->keyval < 0xF000 || event->keyval > 0xFFFF) //&&
		(mask & GDK_CONTROL_MASK || mask == GDK_MOD1_MASK))
	{	//the key is a letter and the modifier is Alt or a modifier is Ctrl (CHECKME support Shift+Ctrl to reverse direction?)
//		NEEDCLOSEBGL
		guint lower = (gdk_keyval_is_upper (event->keyval)) ?
			gdk_keyval_to_lower (event->keyval) : event->keyval;
		if (lower == find_keycode || lower == findnext_keycode)
		{
			rt->release_blocked = TRUE;	//block anomalous key-release-event searches
//			NEEDOPENBGL
			_e2_view_dialog_response_cb (GTK_DIALOG (rt->dialog),
				(lower == find_keycode) ? E2_RESPONSE_FIND : E2_RESPONSE_USER2, rt);
			return TRUE;
		}
		else if (lower == hide_keycode)
		{
//			NEEDOPENBGL
			_e2_view_dialog_response_cb (GTK_DIALOG (rt->dialog),
				E2_RESPONSE_USER3, rt);
			return TRUE;
		}
//		NEEDOPENBGL
	}
//	printd (DEBUG, "_e2_view_dialog_key_press_cb returns FALSE");
	return FALSE;
}
/**
@brief key-release callback
This is intended for the search combo entry, but actually applies to any key
release for any widget in the dialog (and we block spurious ones with a flag)
This generally implements non-incremental searches, except when various
special cases are detected.
@param entry UNUSED the entry widget where the key was pressed
@param event pointer to event data struct
@param rt pointer to data struct for the search

@return FALSE always
*/
gboolean e2_view_dialog_combokey_cb (GtkWidget *entry, GdkEventKey *event,
	E2_ViewDialogRuntime *rt)
{
	NEEDCLOSEBGL
	if (event->keyval == GDK_BackSpace || event->keyval == GDK_Delete)
		e2_view_dialog_search (FALSE, TRUE, rt);
	else
	{
		guint mask = event->state & gtk_accelerator_get_default_mod_mask ();
		//except for <Ctrl><Return> release, incremental search is ineffective
		if (event->keyval == GDK_Return
		 || event->keyval == GDK_KP_Enter
		 || event->keyval == GDK_ISO_Enter
		 || event->keyval == GDK_3270_Enter)
		{
			if (mask == GDK_SHIFT_MASK)
			{	//temporary direction change
				rt->search_backward = !rt->search_backward;
				e2_view_dialog_search (FALSE, FALSE, rt);
				rt->search_backward = !rt->search_backward;
			}
			else
				//non-incremental search, from start if <Control> is pressed
				e2_view_dialog_search ((mask == GDK_CONTROL_MASK), FALSE, rt);
		}
		else if (event->keyval < 0xF000 || event->keyval > 0xFFFF)	//only interested in "text" keyreleases
		{	//we recognise the equivalent of button presses here
			if (mask & GDK_CONTROL_MASK || mask == GDK_MOD1_MASK)
			{	//the modifier is Alt or a modifier is Ctrl (Shift+Ctrl reverses direction)
				guint lower = (gdk_keyval_is_upper (event->keyval)) ?
					gdk_keyval_to_lower (event->keyval) : event->keyval;
				if (lower == find_keycode || lower == findnext_keycode)
				{
					if (rt->release_blocked)	//this is an event after a textview keypress
					{
						rt->release_blocked = FALSE;
						NEEDOPENBGL
						return FALSE;
					}
					else
					{
						printd (DEBUG, "e2_view_dialog_combokey_cb - going to response cb");
						NEEDOPENBGL
						_e2_view_dialog_response_cb (GTK_DIALOG (rt->dialog),
							(lower == find_keycode) ? E2_RESPONSE_FIND : E2_RESPONSE_USER2,
							rt);
						return TRUE;
					}
				}
				else if (lower == hide_keycode)
				{
					if (rt->replacebar != NULL)	//actually in edit mode
					{
						NEEDOPENBGL
						return (e2_edit_dialog_key_press_cb (rt->textview, event, rt));
					}
					else
					{
						NEEDOPENBGL
						_e2_view_dialog_response_cb (GTK_DIALOG (rt->dialog),
							E2_RESPONSE_USER3, rt);
						return TRUE;
					}
				}
				else if (lower == replace_keycode)
				{	//this combo is used for the edit dialog too
					if (rt->replacebar != NULL)	//actually in edit mode
					{
						NEEDOPENBGL
						return (e2_edit_dialog_key_press_cb (rt->textview, event, rt));
					}
				}
			}
			// by default, do an incremental search
			e2_view_dialog_search (FALSE, TRUE, rt);
		}
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief view dialog response callback
This can also be called directly, from other mechanisms to initiate a search
The trigger may have been a <Ctrl>f or <Ctrl>g keypress
@param dialog UNUSED the dialog where the response was triggered
@param response the number assigned the activated widget
@param view rt data for the dialog

@return
*/
static void _e2_view_dialog_response_cb (GtkDialog *dialog, gint response,
	E2_ViewDialogRuntime *rt)
{
	printd (DEBUG, "_e2_view_dialog_response_cb (dialog:_,response:%d,rt:_)", response);
	NEEDCLOSEBGL
	switch (response)
	{
	  case E2_RESPONSE_USER1:  //text wrap
		rt->textwrap = !rt->textwrap;
		gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (rt->textview),
			rt->textwrap ? GTK_WRAP_WORD: GTK_WRAP_NONE);
		break;
	  case E2_RESPONSE_FIND:
	  case E2_RESPONSE_USER2:	//find next (after <Ctrl>g press)
		if (!rt->is_hidden) //search panel is visible
		{
			//these tests are irrelevant for an actual response callback,
			//as no callback happens then
			//if a mod key is pressed, treat it specially
			//Ctrl = first, Shift = temp reverse
			GdkModifierType mask =
#ifdef USE_GTK3_0
				e2_utils_get_savedstate (rt->dialog)
#else
				e2_utils_get_modifiers ()
#endif
				& gtk_accelerator_get_default_mod_mask ();
			if (mask & GDK_SHIFT_MASK)
			{	//temporary direction change
				rt->search_backward = !rt->search_backward;
				e2_view_dialog_search (response == E2_RESPONSE_FIND, FALSE, rt);
				rt->search_backward = !rt->search_backward;
			}
			else
				//perform non-incremental search, from start|end if <Control> is pressed
				e2_view_dialog_search (response == E2_RESPONSE_FIND, FALSE, rt);
		}
		else
		{	//show the search panel
			gtk_widget_show (rt->panel);
			rt->is_hidden = FALSE;
			if (rt->hidebtn != NULL)
				gtk_widget_show (rt->hidebtn);

			GtkWidget *entry =
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
				gtk_entry_set_text (GTK_ENTRY (entry), find_string);
				gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
				e2_view_dialog_search (response == E2_RESPONSE_FIND, FALSE, rt); //CHECKME never search for first when first showing the bar
				g_free (find_string);
			}
			else
				e2_view_dialog_update_combo (rt->combo);

			//focus on the text entry
			gtk_widget_grab_focus (entry);
			//swap find btn tip
			if (rt->findbtn != NULL)
				e2_widget_swap_tooltip (rt->findbtn);
		}
		break;
/*	  case E2_RESPONSE_USER2:	//find next
		_e2_view_dialog_search (FALSE, FALSE, rt);
		break; */
	  case E2_RESPONSE_USER3:	//hide
	  {
		//in case we last performed an incremental search ...
		GtkWidget *entry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (rt->combo));
#else
			GTK_BIN (rt->combo)->child;
#endif
		const gchar *find = gtk_entry_get_text (GTK_ENTRY (entry));
		if (find != NULL && *find != '\0')
		{
			e2_combobox_prepend_history (rt->combo, find, 20, FALSE);
			e2_list_update_history (&find_history, find, NULL, 20, FALSE);
		}
		gtk_widget_hide (rt->panel);
		rt->is_hidden = TRUE;
//			gtk_widget_hide (rt->nextbtn);
		if (rt->hidebtn != NULL)	//in view mode
			gtk_widget_hide (rt->hidebtn);
		gtk_widget_hide (rt->info_label);
//			gtk_widget_show (rt->refreshbtn);
#ifdef E2_MARK_FINDS
		//turn off any highlihts
		e2_view_dialog_clear_hilites (rt);
#endif
		//hide any selection
		GtkTextIter iter;
		if (gtk_text_buffer_get_selection_bounds (rt->textbuffer, &iter, NULL))
			gtk_text_buffer_select_range (rt->textbuffer, &iter, &iter);
		//revert find btn tip
		if (rt->findbtn != NULL)
			e2_widget_swap_tooltip (rt->findbtn);
		gtk_widget_grab_focus (rt->textview);
	  }
		break;
	  default:
		e2_view_dialog_destroy (rt);
		break;
	}
	NEEDOPENBGL
}

#ifdef E2_TRANSIENTBINDINGS
/**
@brief function to setup default key-bindings for view dialog
This is just to provide placeholders, the actual bindings are meaningless
@param set pointer to option data struct

@return
*/
static void _e2_view_dialog_keybindings (E2_OptionSet *set)
{
	//the key name strings are parsed by gtk, and no translation is possible
	e2_option_tree_setup_defaults (set,
	g_strdup("keybindings=<"),  //internal name
	//the column-0 category string(s) here need to match at least the lowest
	//treestore-iter of the full category name
//	g_strconcat(_C(17),"||||",NULL),  //_("general"
//	g_strconcat("\t",_C(11),"||||",NULL),  //_("dialogs"
	g_strconcat("\t\t",_A(109),"||||",NULL),  //_("view"
	g_strconcat("\t\t\t|<Control>j","||","echo|\"up\"", NULL),//_A(127),".",_A(128),"|<Control>a",NULL),
	g_strconcat("\t\t\t|<Control>k","||","echo|\"down\"", NULL),//_A(127),".",_A(128),"|<Control>c",NULL),
	g_strdup(">"),
	NULL);
}
# ifdef E2_MOUSECUSTOM
static void _e2_view_dialog_mousebindings (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("mousebuttons=<"),  //internal name
/*	these lines go into category general.dialogs, so at least 2 leading tabs
	the button name strings are parsed by gtk, and no translation is possible, button codes may be 1 ... whatever
	columns: 0 cat, 1 button, 2 dblclick, 3 trplclick, [4 release,] 5 action, 6 action_data
	if releases handled (#ifdef MCOL_REL), lines must have one extra (=6) separator
*/
	g_strconcat("\t\t",_A(109),"|||||",NULL),  //_("view"
#ifdef WITH_KEYFAKE
	g_strconcat("\t\t\t|<Control>6","|||",_A(127),".",_A(128),"|<Control>a",NULL),
	g_strconcat("\t\t\t|<Control>7","|||",_A(127),".",_A(128),"|<Control>c",NULL),
#endif
	//MORE HERE
	g_strdup(">"),
	NULL);
}
#  ifdef E2_PTRGESTURES
static void _e2_view_dialog_mousegestures (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("mousedrags=<"),  //internal name
	//the column-0 category string(s) here need to match at least the lowest
	//treestore-iter of the full category name
//	g_strconcat(_C(17),"||||",NULL),  //_("general"
//	g_strconcat("\t",_C(11),"||||",NULL),  //_("dialogs"
	g_strconcat("\t\t",_A(109),"||||",NULL),  //_("view"
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
@brief create and show view dialog
File content is loaded and converted if possible, but not here added to the
textview, so that upstream can show the dialog before (possibly slowly)
adding/displaying the content, and set appropriate cursor position
Expects BGL closed
@param localpath localised string which has path & name of item to be processed
@param srt pointer to store for returning dialog data, or NULL

@return the dialog widget, or NULL if there's a problem
*/
static GtkWidget *_e2_view_dialog_create (VPATH *localpath,
	E2_ViewDialogRuntime **srt)
{
	//init view runtime object (0's to ensure edit-dialog things are NULL)
	E2_ViewDialogRuntime *rt = ALLOCATE0 (E2_ViewDialogRuntime);
#if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (rt, FIXME; return NULL;)
#else
	if (rt == NULL)
	{
		if (srt != NULL)
			*srt = NULL;
		return NULL;
	}
#endif
	//get the file content into rt->textbuf
	if (!e2_view_dialog_read_text (localpath, rt))
	{
		DEALLOCATE (E2_ViewDialogRuntime, rt);
		if (srt != NULL)
			*srt = NULL;
		return NULL;
	}

	//hard-code processing of localised <Ctrl>g for find-next
	findnext_keycode = GDK_g;
	//for a view dialog, we don't want to respond to replace keypresses
	replace_keycode = 0;

	rt->case_sensitive = case_sensitive;
	rt->whole_words = whole_words;
	rt->search_backward = search_backward;
	rt->search_wrap = search_wrap;
#ifdef E2_MARK_FINDS
	rt->research = FALSE;
	e2_view_dialog_init_hilites (rt);
#endif
	gchar *utfpath = F_FILENAME_FROM_LOCALE (rt->localpath);
	rt->dialog = e2_dialog_create (NULL, utfpath, _("displaying file"),
		(ResponseFunc)_e2_view_dialog_response_cb, rt);
	F_FREE (utfpath, rt->localpath);
//	gtk_window_set_type_hint (GTK_WINDOW (rt->dialog),
//			    GDK_WINDOW_TYPE_HINT_NORMAL);
	//override some default label properties
	GtkWidget *label = g_object_get_data (G_OBJECT (rt->dialog),
		"e2-dialog-label");
	gtk_label_set_selectable (GTK_LABEL (label), TRUE);

	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog));
#else
		GTK_DIALOG (rt->dialog)->vbox;
#endif
	e2_widget_add_separator (dialog_vbox, FALSE, 0);
	GtkWidget *sw = e2_widget_add_sw (dialog_vbox,
			GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC, TRUE, E2_PADDING);
	//create the view
	//BGL must be closed from textview-creation until dialog show, to block
	//idle-callback happening too early (hence on gtk3 at least, no styling, crash)
	rt->textview = gtk_text_view_new ();
	//set view defaults
	gtk_text_view_set_editable (GTK_TEXT_VIEW (rt->textview), FALSE);
	gtk_text_view_set_cursor_visible ((GtkTextView*)rt->textview, FALSE);
	rt->textwrap = e2_option_bool_get ("dialog-view-wrap");
	gtk_text_view_set_wrap_mode ((GtkTextView*)rt->textview,
		rt->textwrap ? GTK_WRAP_WORD : GTK_WRAP_NONE);
	gtk_container_add (GTK_CONTAINER (sw), rt->textview);
	gint char_width, char_height;
	e2_view_dialog_set_font (&char_width, &char_height, rt);
	rt->window_width = e2_option_int_get ("dialog-view-width");
	rt->window_height = e2_option_int_get ("dialog-view-height");;
//	rt->idle_id = 0;
	//init search runtime data
	rt->history = e2_list_copy_with_data (find_history);

	//action area is a GtkHButtonBox packed at the end of the dialog's vbox
	//ditto for dialog->separator
	//locate find-bar between those 2
	rt->panel = e2_widget_get_box (TRUE, FALSE, 0);
	gtk_box_pack_end (GTK_BOX (dialog_vbox), rt->panel, FALSE, TRUE,
		E2_PADDING_XSMALL);
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

	GtkWidget *hbox = e2_view_dialog_create_searchbar (rt);
	gtk_container_add (GTK_CONTAINER (hndlbox), hbox);
	//add things to the action-area
	GtkWidget *hbbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_action_area (GTK_DIALOG (rt->dialog));
#else
		GTK_DIALOG (rt->dialog)->action_area;
#endif
	//the "not found" warning is created but not displayed
	gchar *labeltext = g_strconcat ("<span weight=\"bold\" foreground=\"",
		e2_option_str_get ("color-negative"), "\">", _("not found"), "</span>", NULL);
	rt->info_label = e2_widget_add_mid_label (hbbox, labeltext, 0.0, TRUE, 0);
	//left-align the label
	gtk_button_box_set_child_secondary (GTK_BUTTON_BOX (hbbox), rt->info_label, TRUE);
	//search and/or view buttons
	labeltext = _("_Hide");
	hide_keycode = e2_utils_get_mnemonic_keycode (labeltext);
	guint asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (hide_keycode)));
	if (asciicode != 0)
		hide_keycode = asciicode;

	rt->hidebtn = e2_dialog_add_simple_button
		(rt->dialog, STOCK_NAME_ZOOM_FIT, labeltext, E2_RESPONSE_USER3);
	e2_widget_set_safetip (rt->hidebtn, _("Hide the search options bar"));

	e2_dialog_add_check_button (rt->dialog, rt->textwrap, _("_wrap"),
		_("If activated, text in the window will be word-wrapped"),
		E2_RESPONSE_USER1);

	labeltext = _("_Find");
	find_keycode = e2_utils_get_mnemonic_keycode (labeltext);
	asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
			GUINT_TO_POINTER (find_keycode)));
	if (asciicode != 0)
		find_keycode = asciicode;

	rt->findbtn = e2_dialog_add_simple_button
		(rt->dialog, STOCK_NAME_FIND, labeltext, E2_RESPONSE_FIND);
#ifdef USE_GTK2_12TIPS
	e2_widget_set_toggletip (
#else
	gtk_tooltips_set_tip (app.tooltips,
#endif
		rt->findbtn,
		_("Show the search options bar"), _("Find the next match"));
	e2_dialog_add_defined_button (rt->dialog, &E2_BUTTON_CLOSE);
//	e2_dialog_set_responses (rt->dialog, E2_RESPONSE_FIND, GTK_RESPONSE_CLOSE);
	e2_dialog_set_negative_response (rt->dialog, GTK_RESPONSE_CLOSE);
	//this prevents a check button from being activated by keyboard
//	gtk_dialog_set_default_response (GTK_DIALOG (rt->dialog), E2_RESPONSE_FIND);

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
	gchar *category = g_strconcat (_C(17),".",_C(11),".",_A(109),NULL);	//_(general.dialogs.view
	e2_keybinding_enrol (GTK_WIDGET (rt->textview), category, _e2_view_dialog_keybindings);
# ifdef E2_MOUSECUSTOM
	e2_mousebinding_enrol (rt->dialog, category, _e2_view_dialog_mousebindings);
#  ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (rt->dialog, category, _e2_view_dialog_mousegestures);
#  endif
# endif
	g_free (category);
#endif
	//this cb does generic stuff and is needed with or without E2_MOUSECUSTOM
	g_signal_connect (G_OBJECT (rt->textview), "button-press-event",
		G_CALLBACK (_e2_view_dialog_button_press_cb), rt);
	g_signal_connect (G_OBJECT (rt->textview), "popup-menu",
		G_CALLBACK (_e2_view_dialog_popup_menu_cb), rt);
	g_signal_connect (G_OBJECT (rt->textview), "key-press-event",
		G_CALLBACK (_e2_view_dialog_key_press_cb), rt);

	gtk_widget_show_all (dialog_vbox);
	gtk_widget_hide (rt->panel);
	gtk_widget_hide (rt->info_label);
	gtk_widget_hide (rt->hidebtn);

	gtk_window_resize (GTK_WINDOW (rt->dialog), char_width * rt->window_width,
				(char_height+3) * rt->window_height);

	if (srt != NULL)
		*srt = rt;	//make rt available to caller

	return rt->dialog;
}

  /*****************/
 /**** actions ****/
/*****************/

/**
@brief re-open file for viewing, at the last-used location in the file
This returns immediately, so if run in a Q-thread, that will end
@param from the widget that was activated to initiate the action
@param art runtime data for the action
@return TRUE if the dialog was opened
*/
static gboolean _e2_view_dialog_reopen (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_do_task (E2_TASK_VIEW, art, from,
		_e2_view_dialog_reviewQ, NULL));
}
static gboolean _e2_view_dialog_reviewQ (E2_ActionTaskData *qed)
{
	gboolean retval;
	//process the 1st item that was selected in active pane
	//FIXME also allow specification of a name
	GPtrArray *names = qed->names;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//".." entries filtered when names compiled
	//for name comparisons, we need the full path
	gchar *localpath = e2_utils_strcat (qed->currdir, (*iterator)->filename);
	if (e2_option_bool_get ("use-external-viewer"))
	{
		gchar *viewer = e2_option_str_get ("command-viewer");
		if (*viewer != '\0')
		{
			gchar *utf = F_FILENAME_FROM_LOCALE (localpath);
			gchar *command = e2_utils_replace_name_macros (viewer, utf);
			if (command == viewer)	//no replacement of macro in viewer
			{
//tag E2_BADQUOTES
				//CHECKME may be bad if a file.view action is supplied
				gchar *qp = e2_utils_quote_string (utf);
				command = g_strconcat (viewer, " ", qp, NULL);
				g_free (qp);
			}
			gint res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, curr_view->treeview
#ifdef E2_COMMANDQ
			, TRUE
#endif
			);
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
		E2_ViewDialogRuntime *vrt;

		CLOSEBGL
		GtkWidget *dialog = _e2_view_dialog_create
#ifdef E2_VFS
			(&ddata, &vrt);
#else
			(localpath, &vrt);
#endif
		if (dialog != NULL)
		{
			gtk_text_view_set_buffer (GTK_TEXT_VIEW (vrt->textview), vrt->textbuffer);
			g_object_unref (vrt->textbuffer); //destroy buffer with view
#ifdef E2_VFS
			e2_view_dialog_show_atlast (&ddata, vrt);
#else
			e2_view_dialog_show_atlast (localpath, vrt);
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
/**
@brief open file for viewing, at a location where there is a specified string
@a art includes a string with filepath and an argument that is a text string
to find in the file. Separator between path and find-target is the first unquoted
unescaped ' ' char (not a \t)
This returns immediately, so if run in a Q-thread, that will end
@param from the widget that was activated to initiate the action
@param art runtime data for the action
@return TRUE if dialog was created
*/
static gboolean _e2_view_dialog_openat (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_do_task (E2_TASK_VIEW, art, from,
		_e2_view_dialog_viewatQ, NULL));
}
static gboolean _e2_view_dialog_viewatQ (E2_ActionTaskData *qed)
{
	//for a help doc, has localised doc path, and a "[title]" arg in undefined encoding
	gchar *view_this = (gchar *) qed->rt_data;

	//strip off any arg FIXME doc path may have whitespace
	gchar *target = e2_utils_bare_strchr (view_this, ' ');	//always ascii ' ', don't need g_utf8_strchr()
	if (target != NULL)
		*target = '\0';

#ifdef E2_VFS
	VPATH ddata = { view_this, qed->currspace };
#endif
	E2_ViewDialogRuntime *vrt;
	CLOSEBGL	//dialog setup wants BGL closed
	GtkWidget *dialog = _e2_view_dialog_create
#ifdef E2_VFS
 		(&ddata, &vrt);
#else
		(view_this, &vrt);
#endif
	OPENBGL

	if (target != NULL)
		*target = ' ';	//revert the argument, ready for next time

	if (dialog != NULL)
	{
		gboolean found;
		GtkTextIter match;
		if (target != NULL)
			target = e2_utils_pass_whitespace (target+1);
		if (target != NULL && *(target+1) != '\0')
		{	//try to find the target string and open there
			GtkTextIter start;
			gtk_text_buffer_get_start_iter (vrt->textbuffer, &start);
			found = gtk_text_iter_forward_search (&start, target, 0, &match, NULL, NULL);
//			if (found)
//				gtk_text_buffer_place_cursor (vrt->textbuffer, &match); //probably needs BGL closed
		}
		else
			found = FALSE;
		CLOSEBGL
//		e2_dialog_show (dialog, app.main_window, E2_DIALOG_DONT_SHOW_ALL, NULL);
		e2_dialog_setup (dialog, app.main_window);
//		gtk_window_present (GTK_WINDOW (dialog));
		gtk_widget_show (dialog);
		//insert the text now
		gtk_text_view_set_buffer (GTK_TEXT_VIEW (vrt->textview), vrt->textbuffer);
		g_object_unref (G_OBJECT(vrt->textbuffer)); //destroy buffer with view
		if (found)
		{	//FIXME sometimes it scrolls to wrong spot
			//see API doco for gtk_text_view_scroll_to_iter() re idle-time processing
			gtk_text_buffer_place_cursor (vrt->textbuffer, &match); //probably needs BGL closed
			gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (vrt->textview),
				gtk_text_buffer_get_selection_bound (vrt->textbuffer),
				0.0, TRUE, 0.0, 0.5);
//			WAIT_FOR_EVENTS
		}
		OPENBGL
		return TRUE;
	}
	return FALSE;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief create and show view dialog
@a localpath may not have any path, or an absolute path. In those cases,
curr_view->dir will be prepended
This returns immediately, so if run in a Q-thread, that will end
Assumes BGL closed
@param localpath localised string which has at least the name of item to be processed

@return TRUE if the dialog was created
*/
gboolean e2_view_dialog_create (VPATH *localpath)
{
	E2_ViewDialogRuntime *vrt;
	GtkWidget *dialog = _e2_view_dialog_create (localpath, &vrt);
	if (dialog != NULL)
	{
//		WAIT_FOR_EVENTS_SLOWLY
//		e2_dialog_show (dialog, app.main_window, E2_DIALOG_DONT_SHOW_ALL, NULL);
		e2_dialog_setup (dialog, app.main_window);
//		gtk_window_present (GTK_WINDOW (dialog));
		gtk_widget_show (dialog);
		gtk_text_view_set_buffer (GTK_TEXT_VIEW (vrt->textview), vrt->textbuffer);
		g_object_unref (G_OBJECT (vrt->textbuffer)); //destroy buffer with view
		//put cursor at start of buffer, for searching etc
		GtkTextIter start;
		gtk_text_buffer_get_start_iter (vrt->textbuffer, &start);
		gtk_text_buffer_place_cursor (vrt->textbuffer, &start);
		return TRUE;
	}
	return FALSE;
}
/**
@brief create and show non-queued, non-logged view dialog, for user help
Expects BGL on/closed
@param view_this string with localised path of help doc, AND a heading name
 (in undefined encoding) as its argument
@return TRUE if the dialog was created
*/
gboolean e2_view_dialog_create_immediate (VPATH *view_this)
{
	E2_ActionTaskData qed;
	qed.rt_data = VPSTR (view_this);
#ifdef E2_VFS
	qed.currspace = view_this->spacedata;
#endif
	OPENBGL	//turn off the BGL
	gboolean retval = _e2_view_dialog_viewatQ (&qed);
	CLOSEBGL
	return retval;
}
/**
@brief register actions related to view dialog, and some other initialisation
@return
*/
void e2_view_dialog_actions_register (void)
{
	E2_Action actions[] =
	{
	{g_strconcat(_A(6),".",_A(110),NULL),_e2_view_dialog_reopen, FALSE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	{g_strconcat(_A(6),".",_A(111),NULL),_e2_view_dialog_openat, TRUE, E2_ACTION_TYPE_ITEM, 0, NULL, NULL},
	};
	guint i;
	for (i = 0; i < 2; i++)
		e2_action_register (&actions[i]);
	//HACK while we're at it, init some session parameters
	//these options are static for the session
	case_sensitive = e2_option_bool_get ("dialog-search-case-sensitive");

	if (e2_option_bool_get ("dialog-search-history"))
		e2_cache_list_register ("search-history", &find_history);
}
/**
@brief register config options related to view dialog
@return
*/
void e2_view_dialog_options_register (void)
{
	gchar *group_name = g_strconcat(_C(11),":",_C(42),NULL); //_("dialogs:view"
	//first some options that may, but probably won't, change during the session
	e2_option_bool_register ("dialog-view-wrap",
		group_name, _("wrap text"),
		_("This causes the view window to open with text-wrapping enabled"),
		NULL, TRUE,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_FREEGROUP);
	e2_option_int_register ("dialog-view-width",
		group_name, _("window width"),
		_("The view window will default to showing this many characters per line (but the the displayed buttons may make it wider than this)")
		, NULL, 84, 20, 1000,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_int_register ("dialog-view-height",
		group_name, _("window height"),
		_("The view window will default to showing this many lines of text"), NULL, 30, 10, 1000,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register ("dialog-view-use-font",
		group_name, _("use custom font"),
		_("If activated, the font specified below will be used, instead of the theme default"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC);
	e2_option_font_register ("dialog-view-font", group_name, _("custom font for viewing files"),
		_("This is the font used for text in each view dialog"), "dialog-view-use-font", "Sans 10", 	//_I( font name
		E2_OPTION_FLAG_BASIC);
	e2_option_bool_register
		("dialog-search-case-sensitive", group_name, _("case sensitive searches"),
		_("This causes the view window search-bar to first open with case-sensitive searching enabled"),
		NULL, FALSE,
		E2_OPTION_FLAG_BASIC);
	e2_option_bool_register ("dialog-search-show-last",
		group_name, _("show last search string"),
		_("This shows the last search-string in the entry field, when the view window search-bar is displayed"),
		NULL, TRUE,
		E2_OPTION_FLAG_ADVANCED);
	e2_option_bool_register
		("dialog-search-history", group_name, _("keep search history"),
		_("This causes search strings to be remembered between sessions"),
		NULL, FALSE,
		E2_OPTION_FLAG_ADVANCED);
}
