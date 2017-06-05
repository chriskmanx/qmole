/* $Id: e2_edit_dialog_undo.c 2815 2013-10-13 07:00:55Z tpgww $

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
@file src/dialogs/e2_edit_dialog_undo.c
@brief undo functions for emelfm2 internal text-editor
This file is #included in e2_edit_dialog.c
(i.e. this won't rebuild unless that is rebuilt)
*/

/* This file has code imported from leafpad 0.8.7
Copyright (C) 2004-2006 Tarot Osuji
Plus sprinkling of stuff from GtkSourceview's undo manager
*/

//prevent building of separate object file
#ifdef INCLUDED_IN_PARENT

  /*********************/
 /***** utilities *****/
/*********************/

/**
@brief set flag for menu item (and keypress) sensitivity

@param state TRUE to enable un-doing

@return
*/
static void _e2edtdlg_set_undo_enabled (gboolean state, E2_ViewDialogRuntime *rt)
{
	rt->undo_enabled = state;
}
#ifdef E2_REDO_ENABLED
/**
@brief set flag for menu item (and keypress) sensitivity

@param state TRUE to enable re-doing

@return
*/
static void _e2edtdlg_set_redo_enabled (gboolean state, E2_ViewDialogRuntime *rt)
{
	rt->redo_enabled = state;
}
#endif
/**
@brief cleanup all @a info_list

@param info_list list of UndoInfo's to be cleared (can be NULL)

@return NULL
*/
static GList *_e2edtdlg_undo_clear_info_list (GList *info_list)
{
	GList *iterator;
	for (iterator = info_list; iterator != NULL; iterator = iterator->next)
	{
		g_free (((E2_UndoInfo *)iterator->data)->str);
		DEALLOCATE (E2_UndoInfo, iterator->data);
	}
	g_list_free (info_list);
	return NULL;
}
/**
@brief change the "sequence" flag of the last item in the undo list

@param seq the new value of the flag

@return
*/
static void _e2edtdlg_undo_set_sequency (gboolean seq, E2_ViewDialogRuntime *rt)
{
	printd (DEBUG,"undo_set_sequency: %d", seq);
	if (g_list_length (rt->undo_list))
		((E2_UndoInfo *)g_list_last(rt->undo_list)->data)->seq = seq;
}
/**
@brief change the "next sequence" flag

@return
*/
//CHECKME needed ?
/*static void _e2edtdlg_undo_set_sequency_reserve (void)
{
	seq_reserve = TRUE;
} */
/**
@brief create and fill an undo data item, and add it to the undo list

@param buffer the text buffer being processed
@param command code for INS, DEL, BS
@param start index into @a buffer of the start of the change
@param end index into @a buffer of the end of the change
@param str the text that comprises the change

@return
*/
static void _e2edtdlg_undo_append_undo_info
	(GtkTextBuffer *buffer, gchar command, guint start, guint end, gchar *str,
		E2_ViewDialogRuntime *rt)
{
	printd (DEBUG,"undo_cb: %d %s (%d-%d)", command, str, start, end);
	E2_UndoInfo *ui = ALLOCATE (E2_UndoInfo);
	CHECKALLOCATEDWARN (ui, return;)
	ui->command = command;
	ui->start = start;
	ui->end = end;
//	ui->seq = FALSE;
	ui->seq = rt->seq_reserve;	//what is this ?? STATIC or not ?
	ui->str = str;

	rt->seq_reserve = FALSE;

	rt->undo_list = g_list_append (rt->undo_list, ui);
}
/**
@brief log a change ready for it to be undone
Called only from inside callbacks i.e. BGL closed

@param buffer the text buffer being processed
@param command code for INS, DEL, BS
@param start index into @a buffer of the start of the change
@param end index into @a buffer of the end of the change

@return
*/
static void _e2edtdlg_undo_create_undo_info (GtkTextBuffer *buffer,
	gchar command, guint start, guint end, E2_ViewDialogRuntime *rt)
{
	GtkTextIter start_iter, end_iter;
	gboolean seq_flag = FALSE;
	gchar *str;

	gtk_text_buffer_get_iter_at_offset (buffer, &start_iter, start);
	gtk_text_buffer_get_iter_at_offset (buffer, &end_iter, end);
	str = gtk_text_buffer_get_text (buffer, &start_iter, &end_iter, FALSE);

	if (rt->undo_gstr->len)
	{	//we have something in the sequence-store GString, so
		//check if this change extends the sequence
		if ((end - start == 1) && (command == rt->ui_tmp->command))
		{
			switch (rt->keyval)
			{
				case GDK_BackSpace:
					if (end == rt->ui_tmp->start)
						seq_flag = TRUE;
					break;
				case GDK_Delete:
					if (start == rt->ui_tmp->start)
						seq_flag = TRUE;
					break;
				case GDK_Tab:
				case GDK_space:
					if (start == rt->ui_tmp->end)
						seq_flag = TRUE;
					break;
				default:
					if (start == rt->ui_tmp->end)
						if (rt->keyval && (rt->keyval < 0xF000 || rt->keyval > 0xFFFF))
							switch (rt->prev_keyval)
							{
								case GDK_Return:
								case GDK_KP_Enter:
								case GDK_ISO_Enter:
								case GDK_3270_Enter:
								case GDK_Tab:
								case GDK_space:
									break;
								default:
									seq_flag = TRUE;
							}
			}
		}
		if (seq_flag)
		{	//we're extending the sequence, update the sequence-store
			switch (command)
			{
				case BS:
					rt->undo_gstr = g_string_prepend(rt->undo_gstr, str);
					rt->ui_tmp->start--;
					break;
				default:
					rt->undo_gstr = g_string_append(rt->undo_gstr, str);
					rt->ui_tmp->end++;
			}
			rt->prev_keyval = rt->keyval;
			_e2edtdlg_set_undo_enabled (TRUE, rt);
#ifdef E2_REDO_ENABLED
			//CHECKME why do these when extending a sequence ?
			rt->redo_list = _e2edtdlg_undo_clear_info_list (rt->redo_list);
			_e2edtdlg_set_redo_enabled (FALSE, rt);
#endif
			return;
		}
		//this one doesn't extend the sequence, so that's ended now
		//update the undo list from the sequence-store
		_e2edtdlg_undo_append_undo_info (buffer, rt->ui_tmp->command,
			rt->ui_tmp->start, rt->ui_tmp->end, g_strdup (rt->undo_gstr->str), rt);
		//and clear the sequence-store
		rt->undo_gstr = g_string_erase (rt->undo_gstr, 0, -1);
	}
	//begin a new change-data process
	if (!rt->keyval && rt->prev_keyval)
		_e2edtdlg_undo_set_sequency (TRUE, rt);

	if (end - start == 1 && (rt->keyval &&
		  (rt->keyval < 0xF000 || rt->keyval > 0xFFFF ||
		  rt->keyval == GDK_BackSpace || rt->keyval == GDK_Delete ||
		  rt->keyval == GDK_Tab)))
	{
		rt->ui_tmp->command = command;
		rt->ui_tmp->start = start;
		rt->ui_tmp->end = end;
		rt->undo_gstr = g_string_erase (rt->undo_gstr, 0, -1);
		g_string_append (rt->undo_gstr, str);
	}
	else
		_e2edtdlg_undo_append_undo_info (buffer, command, start, end, g_strdup(str), rt);
	_e2edtdlg_set_undo_enabled (TRUE, rt);
#ifdef E2_REDO_ENABLED
	//after a change is logged, we can undo it, but not yet redo it
	rt->redo_list = _e2edtdlg_undo_clear_info_list (rt->redo_list);
	_e2edtdlg_set_redo_enabled (FALSE, rt);
#endif
	rt->prev_keyval = rt->keyval;
	rt->keyval = 0;
}
/**
@brief move undo information from sequence-buffer to the changes list

@param buffer the text buffer being processed

@return
*/
static void _e2edtdlg_undo_flush_sequence_buffer (GtkTextBuffer *buffer,
	E2_ViewDialogRuntime *rt)
{
	if (rt->undo_gstr->len)
	{
		_e2edtdlg_undo_append_undo_info (buffer, rt->ui_tmp->command,
			rt->ui_tmp->start, rt->ui_tmp->end, g_strdup (rt->undo_gstr->str), rt);
		rt->undo_gstr = g_string_erase (rt->undo_gstr, 0, -1);
	}
}
/**
@brief sync gtk's "buffer-dirty" flag with the cumulative un/re-do's

@param buffer the text buffer being processed

@return
*/
static void _e2edtdlg_undo_sync_modified_status (GtkTextBuffer *buffer,
	E2_ViewDialogRuntime *rt)
{
	gboolean flag = (rt->changes_count == g_list_length (rt->undo_list));
	printd (DEBUG,"sync status flag = %s (%d - %d)", (flag) ? "TRUE" : "FALSE",
		rt->changes_count, g_list_length (rt->undo_list));
	if (gtk_text_buffer_get_modified (buffer) == flag)
	{
		gtk_text_buffer_set_modified (buffer, !flag);
		printd (DEBUG,"gtk buffer flag changed");
	}
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief "insert-text" callback which logs inserted text

@param buffer the text buffer being processed
@param iter iter after the end of the insertion
@param str the inserted string
@param len UNUSED byte-length of @a str
@param rt data struct for the dialog

@return
*/
static void _e2edtdlg_insert_text_cb (GtkTextBuffer *buffer,
	GtkTextIter *iter, gchar *str, gint len, E2_ViewDialogRuntime *rt)
{
	printd (DEBUG,"insert-text cb: %s", str);
	guint start, end;
	end = gtk_text_iter_get_offset (iter);
	start = end - g_utf8_strlen (str, -1);
	NEEDCLOSEBGL
	_e2edtdlg_undo_create_undo_info (buffer, INS, start, end, rt);
	NEEDOPENBGL
}
/**
@brief "delete-range" callback which logs a deleted range of text

@param buffer the text buffer being processed
@param start_iter iter for start of deleted range
@param end_iter iter for end of deleted range
@param rt data struct for the dialog

@return
*/
static void _e2edtdlg_delete_range_cb (GtkTextBuffer *buffer,
	GtkTextIter *start_iter, GtkTextIter *end_iter, E2_ViewDialogRuntime *rt)
{
	printd (DEBUG,"delete-range cb");
	guint start, end;
	gchar command;
	start = gtk_text_iter_get_offset (start_iter);
	end = gtk_text_iter_get_offset (end_iter);

	if (rt->keyval == GDK_BackSpace)
		command = BS;
	else
		command = DEL;
	NEEDCLOSEBGL
	_e2edtdlg_undo_create_undo_info (buffer, command, start, end, rt);
	NEEDOPENBGL
}
/**
@brief "begin-user-action" callback unblocks "insert-text" and "delete-range" callbacks

@param buffer the text buffer being processed

@return
*/
static void _e2edtdlg_begin_user_action_cb (GtkTextBuffer *buffer,
	E2_ViewDialogRuntime *rt)
{
	printd (DEBUG,"begin-user-action(unblock_func): keyval = 0x%X", rt->keyval);
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	g_signal_handlers_unblock_by_func (G_OBJECT (buffer),
		_e2edtdlg_insert_text_cb, rt);
	g_signal_handlers_unblock_by_func (G_OBJECT (buffer),
		_e2edtdlg_delete_range_cb, rt);
}
/**
@brief "end-user-action" callback blocks "insert-text" and "delete-range" callbacks

@param buffer the text buffer being processed

@return
*/
static void _e2edtdlg_end_user_action_cb (GtkTextBuffer *buffer,
	E2_ViewDialogRuntime *rt)
{
	printd (DEBUG,"end-user-action(block_func)");
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	g_signal_handlers_block_by_func (G_OBJECT (buffer),
		_e2edtdlg_insert_text_cb, rt);
	g_signal_handlers_block_by_func (G_OBJECT (buffer),
		_e2edtdlg_delete_range_cb, rt);
}

  /******************/
 /***** public *****/
/******************/

/**
@brief initialize undo arrangements

Called only from _e2_edit_dialog_create() with BGL closed

@param buffer the text buffer being edited

@return
*/
static void _e2edtdlg_undo_initialize (GtkTextBuffer *buffer,
	E2_ViewDialogRuntime *rt)
{
	if (rt->ui_tmp == NULL)
	{
		rt->ui_tmp = ALLOCATE0 (E2_UndoInfo)
		CHECKALLOCATEDWARN (rt->ui_tmp, return);
	}
	else
		memset (rt->ui_tmp, 0, sizeof(E2_UndoInfo));
	rt->ui_tmp->command = INS;

	if (rt->undo_gstr == NULL)
		rt->undo_gstr = g_string_new ("");
	else
		rt->undo_gstr = g_string_erase (rt->undo_gstr, 0, -1);

	rt->undo_list = _e2edtdlg_undo_clear_info_list (rt->undo_list);
	_e2edtdlg_set_undo_enabled (FALSE, rt);
#ifdef E2_REDO_ENABLED
	rt->redo_list = _e2edtdlg_undo_clear_info_list (rt->redo_list);
	_e2edtdlg_set_redo_enabled (FALSE, rt);
#endif
	rt->seq_reserve = FALSE;
	rt->changes_count = 0;
	rt->prev_keyval = 0;

	g_signal_connect_after (G_OBJECT (buffer), "insert-text",
		G_CALLBACK (_e2edtdlg_insert_text_cb), rt);
	g_signal_connect (G_OBJECT (buffer), "delete-range",
		G_CALLBACK (_e2edtdlg_delete_range_cb), rt);
	g_signal_connect_after (G_OBJECT (buffer), "begin-user-action",
		G_CALLBACK (_e2edtdlg_begin_user_action_cb), rt);
	g_signal_connect(G_OBJECT (buffer), "end-user-action",
		G_CALLBACK (_e2edtdlg_end_user_action_cb), rt);
//	NEEDOPENBGL downstream doesn't affect BGL
	_e2edtdlg_end_user_action_cb (buffer, rt);
//	NEEDCLOSEBGL
}
/**
@brief undo a single change

@param buffer the text buffer being processed

@return TRUE if any undo was completed successfully
*/
static gboolean _e2edtdlg_undo_undo_one (E2_ViewDialogRuntime *rt)
{
	GtkTextIter start_iter, end_iter;
	E2_UndoInfo *ui;
	GtkTextBuffer *buffer = rt->textbuffer;

	_e2edtdlg_undo_flush_sequence_buffer (buffer, rt);
	if (g_list_length (rt->undo_list))
	{
//		undo_block_signal(buffer);
		ui = g_list_last (rt->undo_list)->data;
		gtk_text_buffer_get_iter_at_offset(buffer, &start_iter, ui->start);
		switch (ui->command) {
		case INS:
			gtk_text_buffer_get_iter_at_offset (buffer, &end_iter, ui->end);
			gtk_text_buffer_delete (buffer, &start_iter, &end_iter);
			break;
		default:
			gtk_text_buffer_insert(buffer, &start_iter, ui->str, -1);
		}
#ifdef E2_REDO_ENABLED
		rt->redo_list = g_list_append (rt->redo_list, ui);
#endif
		rt->undo_list = g_list_delete_link (rt->undo_list, g_list_last (rt->undo_list));
#ifdef E2_REDO_ENABLED
		printd (DEBUG,"edit_undo_cb: undo left = %d, redo left = %d",
			g_list_length(rt->undo_list), g_list_length (rt->redo_list));
#else
		printd (DEBUG,"edit_undo_cb: undo left = %d", g_list_length (rt->undo_list));
#endif
//		undo_unblock_signal(buffer);
		if (g_list_length (rt->undo_list))
		{
			if (((E2_UndoInfo *)g_list_last (rt->undo_list)->data)->seq)
				return TRUE;
		}
		else
			_e2edtdlg_set_undo_enabled (FALSE, rt);
#ifdef E2_REDO_ENABLED
		_e2edtdlg_set_redo_enabled (TRUE, rt);
#endif
		if (ui->command == DEL)
			gtk_text_buffer_get_iter_at_offset(buffer, &start_iter, ui->start);
		gtk_text_buffer_place_cursor (buffer, &start_iter);
		GtkTextMark *mark = gtk_text_buffer_get_mark (buffer, "insert");
		gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (rt->textview), mark, 0.05, FALSE, 0,0);
	}
	_e2edtdlg_undo_sync_modified_status (buffer, rt);
	return FALSE;
}
/**
@brief undo everything

@param buffer the text buffer being processed

@return
*/
/* UNUSED
static void _e2edtdlg_undo_all (E2_ViewDialogRuntime *rt)
{
	while (_e2edtdlg_undo_undo_one (rt)) {};
} */
#ifdef E2_REDO_ENABLED
/**
@brief redo a single change

@param buffer the text buffer being processed

@return TRUE if a redo happened
*/
static gboolean _e2edtdlg_undo_redo_one (E2_ViewDialogRuntime *rt)
{
	GtkTextIter start_iter, end_iter;
	E2_UndoInfo *ri;
	GtkTextBuffer *buffer = rt->textbuffer;

	if (g_list_length (rt->redo_list))
	{
//		_e2edtdlg_undo_block_signal (buffer);
		ri = g_list_last (rt->redo_list)->data;
		gtk_text_buffer_get_iter_at_offset (buffer, &start_iter, ri->start);
		switch (ri->command)
		{
		case INS:
			gtk_text_buffer_insert (buffer, &start_iter, ri->str, -1);
			break;
		default:
			gtk_text_buffer_get_iter_at_offset (buffer, &end_iter, ri->end);
			gtk_text_buffer_delete(buffer, &start_iter, &end_iter);
		}
		rt->undo_list = g_list_append (rt->undo_list, ri);
		rt->redo_list = g_list_delete_link (rt->redo_list, g_list_last (rt->redo_list));
		printd (DEBUG,"edit_redo_cb: undo left = %d, redo left = %d",
			g_list_length (rt->undo_list), g_list_length (rt->redo_list));
//		_e2edtdlg_undo_unblock_signal (buffer);
		if (ri->seq)
		{
			_e2edtdlg_undo_set_sequency (TRUE, rt);
			return TRUE;
		}
		if (!g_list_length(rt->redo_list))
			_e2edtdlg_set_redo_enabled (FALSE, rt);
		_e2edtdlg_set_undo_enabled (TRUE, rt);
		gtk_text_buffer_place_cursor (buffer, &start_iter);
		GtkTextMark *mark = gtk_text_buffer_get_mark (buffer, "insert");
		gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (rt->textview), mark, 0.05, FALSE, 0,0);
	}
	_e2edtdlg_undo_sync_modified_status (buffer, rt);
	return FALSE;
}
/**
@brief redo everything

@param buffer the text buffer being processed

@return
*/
/* UNUSED
static void _e2edtdlg_undo_redo_all (E2_ViewDialogRuntime *rt)
{
	while (_e2edtdlg_undo_redo_one (rt)) {};
} */
#endif	//def E2_REDO_ENABLED

#endif //def INCLUDED_IN_PARENT
