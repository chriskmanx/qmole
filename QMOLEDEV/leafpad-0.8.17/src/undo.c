/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2005 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "view.h"
#include "undo.h"

#define DV(x)

//"GTK_TEXT_VIEW(view)->overwrite_mode" can get overwrite_mode state

typedef struct {
	gchar command;
	gint start;
	gint end;
	gboolean seq; // sequency flag
	gchar *str;
} UndoInfo;

enum {
	INS = 0,
	BS,
	DEL
};

static GtkWidget *undo_w = NULL;
static GtkWidget *redo_w = NULL;
static GList *undo_list = NULL;
static GList *redo_list = NULL;
static GString *undo_gstr;
static UndoInfo *ui_tmp;
static gint modified_step;
static guint prev_keyval;
static gboolean seq_reserve = FALSE;

static void undo_flush_temporal_buffer(GtkTextBuffer *buffer);

static GList *undo_clear_info_list(GList *info_list)
{
	while (g_list_length(info_list)) {
		g_free(((UndoInfo *)info_list->data)->str);
		g_free(info_list->data);
		info_list = g_list_delete_link(info_list, info_list);
	}
	return info_list;
}

static void undo_append_undo_info(GtkTextBuffer *buffer, gchar command, gint start, gint end, gchar *str)
{
	UndoInfo *ui = g_malloc(sizeof(UndoInfo));
	
	ui->command = command;
	ui->start = start;
	ui->end = end;
//	ui->seq = FALSE;
	ui->seq = seq_reserve;
	ui->str = str;
	
	seq_reserve = FALSE;
	
	undo_list = g_list_append(undo_list, ui);
DV(g_print("undo_cb: %d %s (%d-%d)\n", command, str, start, end));
}

static void undo_create_undo_info(GtkTextBuffer *buffer, gchar command, gint start, gint end)
{
	GtkTextIter start_iter, end_iter;
	gboolean seq_flag = FALSE;
	gchar *str;
	gint keyval = get_current_keyval();
	
	gtk_text_buffer_get_iter_at_offset(buffer, &start_iter, start);
	gtk_text_buffer_get_iter_at_offset(buffer, &end_iter, end);
	str = gtk_text_buffer_get_text(buffer, &start_iter, &end_iter, FALSE);
	
	if (undo_gstr->len) {
		if ((end - start == 1) && (command == ui_tmp->command)) {
			switch (keyval) {
			case GDK_BackSpace:
				if (end == ui_tmp->start)
					seq_flag = TRUE;
				break;
			case GDK_Delete:
				if (start == ui_tmp->start)
					seq_flag = TRUE;
				break;
			case GDK_Tab:
			case GDK_space:
				if (start == ui_tmp->end)
					seq_flag = TRUE;
				break;
			default:
				if (start == ui_tmp->end)
					if (keyval && keyval < 0xF000)
						switch (prev_keyval) {
						case GDK_Return:
						case GDK_Tab:
						case GDK_space:
							break;
						default:
							seq_flag = TRUE;
						}
			}
		}
		if (seq_flag) {
			switch (command) {
			case BS:
				undo_gstr = g_string_prepend(undo_gstr, str);
				ui_tmp->start--;
				break;
			default:
				undo_gstr = g_string_append(undo_gstr, str);
				ui_tmp->end++;
			}
			redo_list = undo_clear_info_list(redo_list);
			prev_keyval = keyval;
			gtk_widget_set_sensitive(undo_w, TRUE);
			gtk_widget_set_sensitive(redo_w, FALSE);
			return;
		}
		undo_append_undo_info(buffer, ui_tmp->command, ui_tmp->start, ui_tmp->end, g_strdup(undo_gstr->str));
		undo_gstr = g_string_erase(undo_gstr, 0, -1);
	}
	
	if (!keyval && prev_keyval)
		undo_set_sequency(TRUE);
	
	if (end - start == 1 &&
		((keyval && keyval < 0xF000) ||
		  keyval == GDK_BackSpace || keyval == GDK_Delete || keyval == GDK_Tab)) {
		ui_tmp->command = command;
		ui_tmp->start = start;
		ui_tmp->end = end;
		undo_gstr = g_string_erase(undo_gstr, 0, -1);
		g_string_append(undo_gstr, str);
	} else 
		undo_append_undo_info(buffer, command, start, end, g_strdup(str));
	
	redo_list = undo_clear_info_list(redo_list);
	prev_keyval = keyval;
	clear_current_keyval();
//	keyevent_setval(0);
	gtk_widget_set_sensitive(undo_w, TRUE);
	gtk_widget_set_sensitive(redo_w, FALSE);
}

static void cb_insert_text(GtkTextBuffer *buffer, GtkTextIter *iter, gchar *str,
gint len)
{
	gint start, end;
	
DV(	g_print("insert-text\n"));
	end = gtk_text_iter_get_offset(iter);
	start = end - g_utf8_strlen(str, -1);
	
	undo_create_undo_info(buffer, INS, start, end);
}

static void cb_delete_range(GtkTextBuffer *buffer, GtkTextIter *start_iter, GtkTextIter *end_iter)
{
	gint start, end;
	gchar command;
	
DV(	g_print("delete-range\n"));
	start = gtk_text_iter_get_offset(start_iter);
	end = gtk_text_iter_get_offset(end_iter);
	
	if (get_current_keyval() == GDK_BackSpace)
		command = BS;
	else
		command = DEL;
	undo_create_undo_info(buffer, command, start, end);
}

void undo_reset_modified_step(GtkTextBuffer *buffer)
{
	undo_flush_temporal_buffer(buffer);
	modified_step = g_list_length(undo_list);
DV(g_print("undo_reset_modified_step: Reseted modified_step by %d\n", modified_step));
}

static void undo_check_modified_step(GtkTextBuffer *buffer)
{
	gboolean flag;
	
	flag = (modified_step == g_list_length(undo_list));
//g_print("%d - %d = %d\n", modified_step, g_list_length(undo_list), flag);
	if (gtk_text_buffer_get_modified(buffer) == flag)
		gtk_text_buffer_set_modified(buffer, !flag);
//g_print("change!\n");}
}
/*
static void undo_check_modified_step(GtkTextBuffer *buffer)
{
	if (modified_step == g_list_length(undo_list))
		gtk_text_buffer_set_modified(buffer, FALSE);
}
*//* // ????? "modified-changed" signal isn't emitted properly...
#include "window.h"
static void undo_check_modified_step(GtkTextBuffer *buffer)
{
	if (modified_step == g_list_length(undo_list)) {
		gtk_text_buffer_set_modified(buffer, FALSE);
		set_main_window_title();
	}
}
*/
static void cb_begin_user_action(GtkTextBuffer *buffer)
{
	g_signal_handlers_unblock_by_func(G_OBJECT(buffer), 
		G_CALLBACK(cb_insert_text), NULL);
	g_signal_handlers_unblock_by_func(G_OBJECT(buffer),
		G_CALLBACK(cb_delete_range), NULL);
DV(g_print("begin-user-action(unblock_func)"));
DV(g_print(": keyval = 0x%X\n", get_current_keyval()));
}

static void cb_end_user_action(GtkTextBuffer *buffer)
{
	g_signal_handlers_block_by_func(G_OBJECT(buffer), 
		G_CALLBACK(cb_insert_text), NULL);
	g_signal_handlers_block_by_func(G_OBJECT(buffer),
		G_CALLBACK(cb_delete_range), NULL);
DV(g_print("end-user-action(block_func)\n"));
}

void undo_clear_all(GtkTextBuffer *buffer)
{
	undo_list = undo_clear_info_list(undo_list);
	redo_list = undo_clear_info_list(redo_list);
	undo_reset_modified_step(buffer);
	gtk_widget_set_sensitive(undo_w, FALSE);
	gtk_widget_set_sensitive(redo_w, FALSE);
	
	ui_tmp->command = INS;
	undo_gstr = g_string_erase(undo_gstr, 0, -1);
	prev_keyval = 0;
}

void undo_init(GtkWidget *view, GtkWidget *undo_button, GtkWidget *redo_button)
{
	GtkTextBuffer *buffer = GTK_TEXT_VIEW(view)->buffer;
	 
	undo_w = undo_button;
	redo_w = redo_button;
	
	g_signal_connect_after(G_OBJECT(buffer), "insert-text",
		G_CALLBACK(cb_insert_text), NULL);
	g_signal_connect(G_OBJECT(buffer), "delete-range",
		G_CALLBACK(cb_delete_range), NULL);
	g_signal_connect_after(G_OBJECT(buffer), "begin-user-action",
		G_CALLBACK(cb_begin_user_action), NULL);
	g_signal_connect(G_OBJECT(buffer), "end-user-action",
		G_CALLBACK(cb_end_user_action), NULL);
	cb_end_user_action(buffer);
	
	ui_tmp = g_malloc(sizeof(UndoInfo));
	undo_gstr = g_string_new("");
	
	undo_clear_all(buffer);
}

void undo_set_sequency(gboolean seq)
{
	if (g_list_length(undo_list))
		((UndoInfo *)g_list_last(undo_list)->data)->seq = seq;
DV(g_print("<undo_set_sequency: %d>\n", seq));	
}

void undo_set_sequency_reserve(void)
{
	seq_reserve = TRUE;
}

static void undo_flush_temporal_buffer(GtkTextBuffer *buffer)
{
	if (undo_gstr->len) {
		undo_append_undo_info(buffer, ui_tmp->command,
			ui_tmp->start, ui_tmp->end, g_strdup(undo_gstr->str));
		undo_gstr = g_string_erase(undo_gstr, 0, -1);
	}
}

gboolean undo_undo_real(GtkTextBuffer *buffer)
{
	GtkTextIter start_iter, end_iter;
	UndoInfo *ui;
	
	undo_flush_temporal_buffer(buffer);
	if (g_list_length(undo_list)) {
//		undo_block_signal(buffer);
		ui = g_list_last(undo_list)->data;
		gtk_text_buffer_get_iter_at_offset(buffer, &start_iter, ui->start);
		switch (ui->command) {
		case INS:
			gtk_text_buffer_get_iter_at_offset(buffer, &end_iter, ui->end);
			gtk_text_buffer_delete(buffer, &start_iter, &end_iter);
			break;
		default:
			gtk_text_buffer_insert(buffer, &start_iter, ui->str, -1);
		}
		redo_list = g_list_append(redo_list, ui);
		undo_list = g_list_delete_link(undo_list, g_list_last(undo_list));
DV(g_print("cb_edit_undo: undo left = %d, redo left = %d\n",
g_list_length(undo_list), g_list_length(redo_list)));
//		undo_unblock_signal(buffer);
		if (g_list_length(undo_list)) {
			if (((UndoInfo *)g_list_last(undo_list)->data)->seq)
				return TRUE;
		} else
			gtk_widget_set_sensitive(undo_w, FALSE);
		gtk_widget_set_sensitive(redo_w, TRUE);
		if (ui->command == DEL)
			gtk_text_buffer_get_iter_at_offset(buffer, &start_iter, ui->start);
		gtk_text_buffer_place_cursor(buffer, &start_iter);
		scroll_to_cursor(buffer, 0.05);
	}
	undo_check_modified_step(buffer);
	return FALSE;
}

gboolean undo_redo_real(GtkTextBuffer *buffer)
{
	GtkTextIter start_iter, end_iter;
	UndoInfo *ri;
	
	if (g_list_length(redo_list)) {
//		undo_block_signal(buffer);
		ri = g_list_last(redo_list)->data;
		gtk_text_buffer_get_iter_at_offset(buffer, &start_iter, ri->start);
		switch (ri->command) {
		case INS:
			gtk_text_buffer_insert(buffer, &start_iter, ri->str, -1);
			break;
		default:
			gtk_text_buffer_get_iter_at_offset(buffer, &end_iter, ri->end);
			gtk_text_buffer_delete(buffer, &start_iter, &end_iter);
		}
		undo_list = g_list_append(undo_list, ri);
		redo_list = g_list_delete_link(redo_list, g_list_last(redo_list));
DV(g_print("cb_edit_redo: undo left = %d, redo left = %d\n",
g_list_length(undo_list), g_list_length(redo_list)));
//		undo_unblock_signal(buffer);
		if (ri->seq) {
			undo_set_sequency(TRUE);
			return TRUE;
		}
		if (!g_list_length(redo_list))
			gtk_widget_set_sensitive(redo_w, FALSE);
		gtk_widget_set_sensitive(undo_w, TRUE);
		gtk_text_buffer_place_cursor(buffer, &start_iter);
		scroll_to_cursor(buffer, 0.05);
	}
	undo_check_modified_step(buffer);
	return FALSE;
}

void undo_undo(GtkTextBuffer *buffer)
{
	while (undo_undo_real(buffer)) {};
}

void undo_redo(GtkTextBuffer *buffer)
{
	while (undo_redo_real(buffer)) {};
}
