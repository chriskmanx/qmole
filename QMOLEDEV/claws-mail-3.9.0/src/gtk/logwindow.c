/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "logwindow.h"
#include "utils.h"
#include "gtkutils.h"
#include "log.h"
#include "hooks.h"
#include "prefs_common.h"

static void hide_cb				(GtkWidget	*widget,
						 LogWindow	*logwin);
static gboolean key_pressed			(GtkWidget	*widget,
						 GdkEventKey	*event,
						 LogWindow	*logwin);
static void size_allocate_cb	(GtkWidget *widget,
					 GtkAllocation *allocation,
					 gpointer data);
static gboolean log_window_append		(gpointer 	 source,
						 gpointer   	 data);
static void log_window_clip			(LogWindow	*logwin,
						 guint		 glip_length);
static void log_window_clear			(GtkWidget	*widget,
						 LogWindow	*logwin);
static void log_window_popup_menu_extend	(GtkTextView	*textview,
						 GtkMenu	*menu,
						 LogWindow	*logwin);
					 
/*!
 *\brief	Save Gtk object size to prefs dataset
 */
static void size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation,
					 gpointer data)
{
	gint *prefs_logwin_width = NULL;
	gint *prefs_logwin_height = NULL;
	LogInstance instance = GPOINTER_TO_INT(data);

	cm_return_if_fail(allocation != NULL);

	get_log_prefs(instance, &prefs_logwin_width, &prefs_logwin_height);
	cm_return_if_fail(prefs_logwin_width != NULL);
	cm_return_if_fail(prefs_logwin_height != NULL);

	*prefs_logwin_width = allocation->width;
	*prefs_logwin_height = allocation->height;
}

LogWindow *log_window_create(LogInstance instance)
{
	LogWindow *logwin;
	GtkWidget *window;
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	static GdkGeometry geometry;
	gint *prefs_logwin_width = NULL;
	gint *prefs_logwin_height = NULL;

	debug_print("Creating log window...\n");

	get_log_prefs(instance, &prefs_logwin_width, &prefs_logwin_height);
	cm_return_val_if_fail(prefs_logwin_width != NULL, NULL);
	cm_return_val_if_fail(prefs_logwin_height != NULL, NULL);

	logwin = g_new0(LogWindow, 1);

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "logwindow");
	gtk_window_set_title(GTK_WINDOW(window), get_log_title(instance));
	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(gtk_widget_hide_on_delete), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), logwin);
	g_signal_connect_after(G_OBJECT(window), "hide",
			 G_CALLBACK(hide_cb), logwin);
	gtk_widget_realize(window);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(window), scrolledwin);
	gtk_widget_show(scrolledwin);

	text = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	logwin->buffer = buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));

	g_object_ref(G_OBJECT(logwin->buffer));
	gtk_text_view_set_buffer(GTK_TEXT_VIEW(text), NULL);
	logwin->hidden = TRUE;
	logwin->never_shown = TRUE;

	gtk_text_buffer_get_start_iter(buffer, &iter);
	logwin->end_mark = gtk_text_buffer_create_mark(buffer, "end", &iter, FALSE);

	g_signal_connect(G_OBJECT(text), "populate-popup",
			 G_CALLBACK(log_window_popup_menu_extend), logwin);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);
	gtk_widget_show(text);

	g_signal_connect(G_OBJECT(window), "size_allocate",
			 G_CALLBACK(size_allocate_cb), GINT_TO_POINTER(instance));

	if (!geometry.min_height) {
		geometry.min_width = 520;
		geometry.min_height = 400;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(window, *prefs_logwin_width,
				    *prefs_logwin_height);

	logwin->window = window;
	logwin->scrolledwin = scrolledwin;
	logwin->text = text;
	logwin->hook_id = hooks_register_hook(get_log_hook(instance), log_window_append, logwin);

	return logwin;
}

#define LOG_COLORS 8

void log_window_init(LogWindow *logwin)
{
	GtkTextBuffer *buffer;
#if !GTK_CHECK_VERSION(3, 0, 0)
	GdkColormap *colormap;
	gboolean success[LOG_COLORS];
#endif
	GdkColor color[LOG_COLORS];
	gint i;

	gtkut_convert_int_to_gdk_color(prefs_common.log_msg_color, &color[0]);
	gtkut_convert_int_to_gdk_color(prefs_common.log_warn_color, &color[1]);
	gtkut_convert_int_to_gdk_color(prefs_common.log_error_color, &color[2]);
	gtkut_convert_int_to_gdk_color(prefs_common.log_in_color, &color[3]);
	gtkut_convert_int_to_gdk_color(prefs_common.log_out_color, &color[4]);
	gtkut_convert_int_to_gdk_color(prefs_common.log_status_ok_color, &color[5]);
	gtkut_convert_int_to_gdk_color(prefs_common.log_status_nok_color, &color[6]);
	gtkut_convert_int_to_gdk_color(prefs_common.log_status_skip_color, &color[7]);

	logwin->msg_color = color[0];
	logwin->warn_color = color[1];
	logwin->error_color = color[2];
	logwin->in_color = color[3];
	logwin->out_color = color[4];
	logwin->status_ok_color = color[5];
	logwin->status_nok_color = color[6];
	logwin->status_skip_color = color[7];

#if !GTK_CHECK_VERSION(3, 0, 0)
	colormap = gdk_drawable_get_colormap(gtk_widget_get_window(logwin->window));
	gdk_colormap_alloc_colors(colormap, color, LOG_COLORS, FALSE, TRUE, success);

	for (i = 0; i < LOG_COLORS; i++) {
		if (success[i] == FALSE) {
			GtkStyle *style;

			g_warning("LogWindow: color allocation failed\n");
			style = gtk_widget_get_style(logwin->window);
			logwin->msg_color = logwin->warn_color =
					logwin->error_color = logwin->in_color =
					logwin->out_color = logwin->status_ok_color =
					logwin->status_nok_color = logwin->status_skip_color =
					style->black;
			break;
		}
	}
#endif

	buffer = logwin->buffer;
	gtk_text_buffer_create_tag(buffer, "message",
				   "foreground-gdk", &logwin->msg_color,
				   NULL);
	gtk_text_buffer_create_tag(buffer, "warn",
				   "foreground-gdk", &logwin->warn_color,
				   NULL);
	logwin->error_tag = gtk_text_buffer_create_tag(buffer, "error",
				   "foreground-gdk", &logwin->error_color,
				   NULL);
	gtk_text_buffer_create_tag(buffer, "input",
				   "foreground-gdk", &logwin->in_color,
				   NULL);
	gtk_text_buffer_create_tag(buffer, "output",
				   "foreground-gdk", &logwin->out_color,
				   NULL);
	gtk_text_buffer_create_tag(buffer, "status_ok",
				   "foreground-gdk", &logwin->status_ok_color,
				   NULL);
	gtk_text_buffer_create_tag(buffer, "status_nok",
				   "foreground-gdk", &logwin->status_nok_color,
				   NULL);
	gtk_text_buffer_create_tag(buffer, "status_skip",
				   "foreground-gdk", &logwin->status_skip_color,
				   NULL);
}

#undef LOG_COLORS

void log_window_show(LogWindow *logwin)
{
	GtkTextView *text = GTK_TEXT_VIEW(logwin->text);
	GtkTextBuffer *buffer = logwin->buffer;
	GtkTextMark *mark;

	logwin->hidden = FALSE;

	if (logwin->never_shown)
		gtk_text_view_set_buffer(GTK_TEXT_VIEW(logwin->text), logwin->buffer);

	logwin->never_shown = FALSE;

	mark = gtk_text_buffer_get_mark(buffer, "end");
	gtk_text_view_scroll_mark_onscreen(text, mark);

	gtk_window_deiconify(GTK_WINDOW(logwin->window));
	gtk_widget_show(logwin->window);
	gtk_window_present(GTK_WINDOW(logwin->window));
#ifdef MAEMO
	maemo_window_full_screen_if_needed(GTK_WINDOW(logwin->window));
	maemo_connect_key_press_to_mainwindow(GTK_WINDOW(logwin->window));
#endif
}

static void log_window_jump_to_error(LogWindow *logwin)
{
	GtkTextIter iter;
	gtk_text_buffer_get_end_iter(logwin->buffer, &iter);
	if (!gtk_text_iter_backward_to_tag_toggle(&iter, logwin->error_tag))
		return;

	gtk_text_iter_backward_line(&iter);
	gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(logwin->text), &iter, 0, TRUE, 0, 0);
}

void log_window_show_error(LogWindow *logwin)
{
	log_window_show(logwin);
	log_window_jump_to_error(logwin);
}

void log_window_set_clipping(LogWindow *logwin, gboolean clip, guint clip_length)
{
	cm_return_if_fail(logwin != NULL);

	logwin->clip = clip;
	logwin->clip_length = clip_length;
}

static gboolean log_window_append(gpointer source, gpointer data)
{
	LogText *logtext = (LogText *) source;
	LogWindow *logwindow = (LogWindow *) data;
	GtkTextView *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	gchar *head = NULL;
	const gchar *tag;

	cm_return_val_if_fail(logtext != NULL, TRUE);
	cm_return_val_if_fail(logtext->text != NULL, TRUE);
	cm_return_val_if_fail(logwindow != NULL, FALSE);

	if (logwindow->clip && !logwindow->clip_length)
		return FALSE;

	text = GTK_TEXT_VIEW(logwindow->text);
	buffer = logwindow->buffer;
	gtk_text_buffer_get_iter_at_offset(buffer, &iter, -1);

	switch (logtext->type) {
	case LOG_MSG:
		tag = "message";
		head = "* ";
		break;
	case LOG_WARN:
		tag = "warn";
		head = "** ";
		break;
	case LOG_ERROR:
		tag = "error";
		head = "*** ";
		logwindow->has_error = TRUE;
		break;
	case LOG_STATUS_OK:
		tag = "status_ok";
		head = "> ";
		break;
	case LOG_STATUS_NOK:
		tag = "status_nok";
		head = "> ";
		break;
	case LOG_STATUS_SKIP:
		tag = "status_skip";
		head = "> skipped: ";
		break;
	default:
		tag = NULL;
		break;
	}

	if (logtext->instance == LOG_PROTOCOL) {
		if (tag == NULL) {
			if (strstr(logtext->text, "] POP3>")
			||  strstr(logtext->text, "] IMAP4>")
			||  strstr(logtext->text, "] SMTP>")
			||  strstr(logtext->text, "] ESMTP>")
			||  strstr(logtext->text, "] NNTP>"))
				tag = "output";
			if (strstr(logtext->text, "] POP3<")
			||  strstr(logtext->text, "] IMAP4<")
			||  strstr(logtext->text, "] SMTP<")
			||  strstr(logtext->text, "] ESMTP<")
			||  strstr(logtext->text, "] NNTP<"))
				tag = "input";
		}
	}

	if (head)
		gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, head, -1,
							 tag, NULL);

	if (!g_utf8_validate(logtext->text, -1, NULL)) {
		gchar * mybuf = g_malloc(strlen(logtext->text)*2 +1);
		conv_localetodisp(mybuf, strlen(logtext->text)*2 +1, logtext->text);
		gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, mybuf, -1,
							 tag, NULL);
		g_free(mybuf);
	} else {
		gtk_text_buffer_insert_with_tags_by_name(buffer, &iter, logtext->text, -1,
							 tag, NULL);
	}
	gtk_text_buffer_get_start_iter(buffer, &iter);

	if (logwindow->clip)
	       log_window_clip (logwindow, logwindow->clip_length);

	if (!logwindow->hidden) {
		GtkAdjustment *vadj = gtk_text_view_get_vadjustment(text);
		gfloat upper = gtk_adjustment_get_upper(vadj) -
		    gtk_adjustment_get_page_size(vadj);
		gfloat value = gtk_adjustment_get_value(vadj);
		if (value == upper || 
		    (upper - value < 16 && value < 8))
			gtk_text_view_scroll_mark_onscreen(text, logwindow->end_mark);
	}

	return FALSE;
}

static void hide_cb(GtkWidget *widget, LogWindow *logwin)
{
	logwin->hidden = TRUE;
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event,
			LogWindow *logwin)
{
	if (event && event->keyval == GDK_KEY_Escape)
		gtk_widget_hide(logwin->window);
	else if (event && event->keyval == GDK_KEY_Delete) 
		log_window_clear(NULL, logwin);

	return FALSE;
}

static void log_window_clip(LogWindow *logwin, guint clip_length)
{
        guint length;
	guint point;
	GtkTextBuffer *textbuf = logwin->buffer;
	GtkTextIter start_iter, end_iter;
	
	length = gtk_text_buffer_get_line_count(textbuf);
	/* debug_print("Log window length: %u\n", length); */
	
	if (length > clip_length) {
	        /* find the end of the first line after the cut off
		 * point */
       	        point = length - clip_length;
		gtk_text_buffer_get_iter_at_line(textbuf, &end_iter, point);
		if (!gtk_text_iter_forward_to_line_end(&end_iter))
			return;
		gtk_text_buffer_get_start_iter(textbuf, &start_iter);
		gtk_text_buffer_delete(textbuf, &start_iter, &end_iter);
		if (logwin->has_error) {
			gtk_text_buffer_get_start_iter(textbuf, &start_iter);
			if (mainwindow_get_mainwindow() && !gtk_text_iter_forward_to_tag_toggle(&start_iter, logwin->error_tag)) {
				mainwindow_clear_error(mainwindow_get_mainwindow());
				logwin->has_error = FALSE;
			}
		}
	}
}

static void log_window_clear(GtkWidget *widget, LogWindow *logwin)
{
	GtkTextBuffer *textbuf = logwin->buffer;
	GtkTextIter start_iter, end_iter;
	
	gtk_text_buffer_get_start_iter(textbuf, &start_iter);
	gtk_text_buffer_get_end_iter(textbuf, &end_iter);
	gtk_text_buffer_delete(textbuf, &start_iter, &end_iter);
}

static void log_window_popup_menu_extend(GtkTextView *textview,
   			GtkMenu *menu, LogWindow *logwin)
{
	GtkWidget *menuitem;
	
	cm_return_if_fail(menu != NULL);
	cm_return_if_fail(GTK_IS_MENU_SHELL(menu));

	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), menuitem);
	gtk_widget_show(menuitem);
	
	menuitem = gtk_menu_item_new_with_mnemonic(_("Clear _Log"));
	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(log_window_clear), logwin);
	gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), menuitem);
	gtk_widget_show(menuitem);
}

