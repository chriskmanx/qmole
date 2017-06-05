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

#include "leafpad.h"
#include <gdk/gdkkeysyms.h>
#include <string.h>

static gint keyval;
static gboolean view_scroll_flag = FALSE;

gint get_current_keyval(void)
{
	return keyval;
}

void clear_current_keyval(void)
{
	keyval = 0;
}
/*
gboolean scroll_to_cursor(GtkTextBuffer *buffer, gdouble within_margin)
{
	GtkTextIter iter;
	
//	gtk_text_buffer_get_start_iter(buffer, &iter);
	gtk_text_buffer_get_iter_at_mark(buffer, &iter,
		gtk_text_buffer_get_insert(buffer));
	return gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(pub->mw->view),
		&iter, within_margin, FALSE, 0.5, 0.5);
}
*/
void scroll_to_cursor(GtkTextBuffer *buffer, gdouble within_margin)
{
	gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(pub->mw->view),
		gtk_text_buffer_get_insert(buffer),
		within_margin, FALSE, 0, 0);
}

gint check_text_modification(void)
{
	gchar *basename, *str;
	gint res;
	
	if (gtk_text_buffer_get_modified(pub->mw->buffer)) {
		basename = get_file_basename(pub->fi->filename, FALSE);
		str = g_strdup_printf(_("Save changes to '%s'?"), basename);
		g_free(basename);
		res = run_dialog_message_question(pub->mw->window, str);
		g_free(str);
		switch (res) {
		case GTK_RESPONSE_NO:
			return 0;
		case GTK_RESPONSE_YES:
			if (!on_file_save())
				return 0;
		}
		return -1;
	}
	
	return 0;
}

static gint check_preedit(GtkWidget *view)
{
	gint cursor_pos;
	
	gtk_im_context_get_preedit_string(
		GTK_TEXT_VIEW(view)->im_context, NULL, NULL, &cursor_pos);
	
	return cursor_pos;
}

static gboolean check_selection_bound(GtkTextBuffer *buffer)
{
	GtkTextIter start, end;
	gchar *str, *p;
	
	if (gtk_text_buffer_get_selection_bounds(buffer, &start, &end)) {
		str = gtk_text_iter_get_text(&start, &end);
		p = strchr(str, '\n');
		g_free(str);
		if (p)
			return TRUE;
	}
	
	return FALSE;
}

static gboolean cb_key_press_event(GtkWidget *view, GdkEventKey *event)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter iter;
	GdkRectangle prev_rect;
	
	if (check_preedit(view))
		return FALSE;
	
	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(view));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
	gtk_text_view_get_iter_location(GTK_TEXT_VIEW(view), &iter, &prev_rect);
	
	keyval = 0;
//g_print("key-press-event: 0x%X\n", event->keyval);
	switch (event->keyval) {
	case GDK_Up:		// Try [Shift]+[Down]. it works bad.
	case GDK_Down:
		if (gtk_text_view_move_mark_onscreen(GTK_TEXT_VIEW(view), mark)) {
			GdkRectangle iter_rect;
			gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
			gtk_text_view_get_iter_location(GTK_TEXT_VIEW(view), &iter, &iter_rect);
			if (iter_rect.y < prev_rect.y) {
				gtk_text_view_get_line_at_y(GTK_TEXT_VIEW(view), &iter,				
					iter_rect.y - iter_rect.height, NULL);
				gtk_text_buffer_move_mark(buffer, mark, &iter);
			}
			if (!(event->state & GDK_SHIFT_MASK)) {
				gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
				gtk_text_buffer_place_cursor(buffer, &iter);
			}
			return TRUE;
		}
		break;
	case GDK_Page_Up:
	case GDK_Page_Down:
		if (gtk_text_view_move_mark_onscreen(GTK_TEXT_VIEW(view), mark)) {
			GdkRectangle visible_rect, iter_rect;
			gint pos = 0;
			gtk_text_view_get_visible_rect(GTK_TEXT_VIEW(view), &visible_rect);
			gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
			gtk_text_view_get_iter_location(GTK_TEXT_VIEW(view), &iter, &iter_rect);
			if (iter_rect.y < prev_rect.y)
				pos = 1;
			if (event->keyval == GDK_Page_Up)
				gtk_text_view_get_line_at_y(GTK_TEXT_VIEW(view), &iter,
					iter_rect.y - visible_rect.height + iter_rect.height, NULL);
			else
				gtk_text_view_get_line_at_y(GTK_TEXT_VIEW(view), &iter,
					iter_rect.y + visible_rect.height - iter_rect.height, NULL);
			gtk_text_buffer_move_mark(buffer, mark, &iter);
			gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(view),
				mark, 0, TRUE, 0, pos);
			if (!(event->state & GDK_SHIFT_MASK)) {
				gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
				gtk_text_buffer_place_cursor(GTK_TEXT_VIEW(view)->buffer, &iter);
			}
			return TRUE;
		}
		break;
	case GDK_Return:
		if (indent_get_state()) {
			indent_real(view);
			return TRUE;
		}
		break;
	case GDK_Tab:
		if (event->state & GDK_CONTROL_MASK) {
			indent_toggle_tab_width(view);
			return TRUE;
		}
	case GDK_ISO_Left_Tab:
		if (event->state & GDK_SHIFT_MASK)
			indent_multi_line_unindent(GTK_TEXT_VIEW(view)->buffer);
		else if (!check_selection_bound(GTK_TEXT_VIEW(view)->buffer))
			break;
		else
			indent_multi_line_indent(GTK_TEXT_VIEW(view)->buffer);
		return TRUE;
	}
	keyval = event->keyval;
	if ((event->state & GDK_CONTROL_MASK)
		|| (event->keyval == GDK_Control_L)
		|| (event->keyval == GDK_Control_R)) {
		keyval = keyval + 0x10000;
//g_print("=================================================\n");
	}
	
	return FALSE;
}

static gboolean cb_button_press_event(GtkWidget *view, GdkEventButton *event)
{
	GtkTextIter iter, start, end;
	gint x, y;
	
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(view));
	
	if ((event->button) == 3 && (event->type == GDK_BUTTON_PRESS)) {
		gtk_text_view_window_to_buffer_coords(GTK_TEXT_VIEW(view),
			gtk_text_view_get_window_type(GTK_TEXT_VIEW(view), event->window),
			(gint)event->x, (gint)event->y, &x, &y);
		gtk_text_view_get_iter_at_location(GTK_TEXT_VIEW(view), &iter, x, y);
		gtk_text_buffer_get_selection_bounds(buffer, &start, &end);
		if (!gtk_text_iter_in_range(&iter, &start, &end))
			gtk_text_buffer_place_cursor(buffer, &iter);
	}
	
	return FALSE;
}

static void cb_modified_changed(GtkTextBuffer *buffer, GtkWidget *view)
{
	gboolean modified_flag, exist_flag = FALSE;
	gchar *filename, *title;
	
	modified_flag = gtk_text_buffer_get_modified(buffer);
	filename = get_file_basename(pub->fi->filename, TRUE);
	if (modified_flag)
		title = g_strconcat("*", filename, NULL);
	else {
		title = g_strdup(filename);
		undo_reset_modified_step(buffer);
	}
	g_free(filename);
	gtk_window_set_title(GTK_WINDOW(gtk_widget_get_toplevel(view)), title);
	g_free(title);
	if (pub->fi->filename)
		exist_flag = g_file_test(
			g_filename_to_utf8(pub->fi->filename, -1, NULL, NULL, NULL),
			G_FILE_TEST_EXISTS);
	menu_sensitivity_from_modified_flag(modified_flag || !exist_flag);
}

void force_call_cb_modified_changed(GtkWidget *view)
{
	cb_modified_changed(GTK_TEXT_VIEW(view)->buffer, view);
}

void force_block_cb_modified_changed(GtkWidget *view)
{
	g_signal_handlers_block_by_func(G_OBJECT(GTK_TEXT_VIEW(view)->buffer), 
		G_CALLBACK(cb_modified_changed), view);
}

void force_unblock_cb_modified_changed(GtkWidget *view)
{
	g_signal_handlers_unblock_by_func(G_OBJECT(GTK_TEXT_VIEW(view)->buffer), 
		G_CALLBACK(cb_modified_changed), view);
}
/*
static void cb_mark_set(GtkTextBuffer *buffer, GtkTextIter *iter, GtkTextMark *mark)
{
	if (gtk_text_mark_get_name(mark))
{g_print(gtk_text_mark_get_name(mark));
}else g_print("|");
		menu_sensitivity_from_selection_bound(
			gtk_text_buffer_get_selection_bounds(buffer, NULL, NULL));
}
*/
static void cb_mark_changed(GtkTextBuffer *buffer)
{
	menu_sensitivity_from_selection_bound(
		gtk_text_buffer_get_selection_bounds(buffer, NULL, NULL));
}

static void cb_focus_event(GtkWidget *view, GdkEventFocus *event)
{
	if (!gtk_text_buffer_get_selection_bounds(GTK_TEXT_VIEW(view)->buffer, NULL, NULL))
		gtk_text_mark_set_visible(
			gtk_text_buffer_get_selection_bound(
				GTK_TEXT_VIEW(view)->buffer), !event->in);
	if (event->in)
		menu_sensitivity_from_clipboard();
}
/*
static void cb_begin_user_action(GtkTextBuffer *buffer, GtkWidget *view)
{
	g_signal_handlers_unblock_by_func(G_OBJECT(buffer), 
		G_CALLBACK(cb_modified_changed), view);
//	g_print("begin-user-action\n");
}

static void cb_end_user_action(GtkTextBuffer *buffer, GtkWidget *view)
{
	g_signal_handlers_block_by_func(G_OBJECT(buffer), 
		G_CALLBACK(cb_modified_changed), view);
	gtk_text_view_scroll_mark_onscreen(		// TODO: require?
		GTK_TEXT_VIEW(view),
		gtk_text_buffer_get_insert(buffer));
//	g_print("end-user-action\n");
}
*//*
static void cb_something(GtkTextBuffer *buffer, gchar *data)
{
	g_print("%s\n", data);
}
*/
void set_view_scroll(void)
{
	view_scroll_flag = TRUE;
}

static void cb_end_user_action(GtkTextBuffer *buffer, GtkWidget *view)
{
	if (view_scroll_flag) {
		gtk_text_view_scroll_mark_onscreen(		// TODO: require?
			GTK_TEXT_VIEW(view),
			gtk_text_buffer_get_insert(buffer));
		view_scroll_flag = FALSE;
	}
}

GtkWidget *create_text_view(void)
{
 	GtkWidget *view;
	GtkTextBuffer *buffer;
	
	view = gtk_text_view_new();
	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(view));
	
//	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(view), 1);
//	gtk_text_view_set_right_margin(GTK_TEXT_VIEW(view), 1);
	
	g_signal_connect(G_OBJECT(view), "key-press-event",
		G_CALLBACK(cb_key_press_event), NULL);
	g_signal_connect(G_OBJECT(view), "button-press-event",
		G_CALLBACK(cb_button_press_event), NULL);
	g_signal_connect_after(G_OBJECT(view), "cut-clipboard",
		G_CALLBACK(menu_sensitivity_from_clipboard), NULL);
	g_signal_connect_after(G_OBJECT(view), "copy-clipboard",
		G_CALLBACK(menu_sensitivity_from_clipboard), NULL);
	g_signal_connect_after(G_OBJECT(view), "paste-clipboard",
		G_CALLBACK(set_view_scroll),
		gtk_text_buffer_get_insert(buffer));
/*	g_signal_connect_after(G_OBJECT(view), "paste-clipboard",
		G_CALLBACK(gtk_text_view_scroll_mark_onscreen),
		gtk_text_buffer_get_insert(buffer));*/
	g_signal_connect_after(G_OBJECT(view), "focus-in-event",
		G_CALLBACK(cb_focus_event), NULL);
	g_signal_connect_after(G_OBJECT(view), "focus-out-event",
		G_CALLBACK(cb_focus_event), NULL);
	
	g_signal_connect(G_OBJECT(buffer), "mark-set",
		G_CALLBACK(cb_mark_changed), NULL);
	g_signal_connect(G_OBJECT(buffer), "mark-deleted",
		G_CALLBACK(cb_mark_changed), NULL);
	g_signal_connect(G_OBJECT(buffer), "modified-changed",
		G_CALLBACK(cb_modified_changed), view);
	g_signal_connect_after(G_OBJECT(buffer), "end-user-action",
		G_CALLBACK(cb_end_user_action), view);
/*	g_signal_connect(G_OBJECT(buffer), "begin-user-action",
		G_CALLBACK(cb_begin_user_action), view);
	g_signal_connect_after(G_OBJECT(buffer), "end-user-action",
		G_CALLBACK(cb_end_user_action), view);
	cb_end_user_action(buffer, view);
*/	
	linenum_init(view);
	
	return view;
}
