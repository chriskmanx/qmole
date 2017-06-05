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
#include <stdio.h>
#include <string.h>
#include "file.h"
#include "view.h"
#include "encoding.h"
#include "dialog.h"
#include "menu.h"
#include "i18n.h"
//#include "undo.h"

gboolean check_file_writable(gchar *filename)
{
	FILE *fp;
	
	if ((fp = fopen(filename, "a")) != NULL) {
		fclose(fp);
		return TRUE;
	}
	return FALSE;
}

gchar *get_file_basename(gchar *filename, gboolean bracket)
{
	gchar *basename = NULL;
	gchar *tmp;
	gboolean exist_flag;
	
	if (filename) {
		tmp = g_path_get_basename(
			g_filename_to_utf8(filename, -1, NULL, NULL, NULL));
		exist_flag = g_file_test(
			g_filename_to_utf8(filename, -1, NULL, NULL, NULL),
			G_FILE_TEST_EXISTS);
	} else {
		tmp = g_strdup(_("Untitled"));
		exist_flag = FALSE;
	}
	
	if (bracket) {
		if (!exist_flag) {
			GString *string = g_string_new(tmp);
			g_string_prepend(string, "(");
			g_string_append(string, ")");
			basename = g_strdup(string->str);
			g_string_free(string, TRUE);
		} else if (!check_file_writable(filename)) {
			GString *string = g_string_new(tmp);
			g_string_prepend(string, "<");
			g_string_append(string, ">");
			basename = g_strdup(string->str);
			g_string_free(string, TRUE);
		}
	}
	
	if (!basename)
		basename = g_strdup(tmp);
	g_free(tmp);
	
	return basename;
}

gchar *parse_file_uri(gchar *uri)
{
	gchar *filename;
//	gchar **strs;
	
	if (strstr(uri, ":")) {
		if (g_strstr_len(uri, 5, "file:"))
			filename = g_filename_from_uri(uri, NULL, NULL);
		else
			return NULL;  // other URI error
	} else
		if (g_path_is_absolute(uri))
			filename = g_strdup(uri);
		else
			filename = g_build_filename(g_get_current_dir(), uri, NULL);
	
/*	if (strstr(filename, " ")) {
		strs = g_strsplit(filename, " ", -1);
		g_free(filename);
		filename = g_strjoinv("\\ ", strs);
		g_strfreev(strs);
	}
*/	
	return filename;
}

gint file_open_real(GtkWidget *view, FileInfo *fi)
{
	gchar *contents;
	gsize length;
	GError *err = NULL;
	const gchar *charset;
	gchar *str = NULL;
	GtkTextIter iter;
	
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(view));
	
	if (!g_file_get_contents(fi->filename, &contents, &length, &err)) {
		if (g_file_test(fi->filename, G_FILE_TEST_EXISTS)) {
			run_dialog_message(gtk_widget_get_toplevel(view),
				GTK_MESSAGE_ERROR, err->message);
			g_error_free(err);
			return -1;
		}
		g_error_free(err);
		err = NULL;
		contents = g_strdup("");
	}
	
	fi->lineend = detect_line_ending(contents);
	if (fi->lineend != LF)
		convert_line_ending_to_lf(contents);
	
	if (fi->charset)
		charset = fi->charset;
	else {
		charset = detect_charset(contents);
		if (charset == NULL)
			charset = get_default_charset();
	}

	if (length)
		do {
			if (err) {
				charset = "ISO-8859-1";
				g_error_free(err);
				err = NULL;
			}
			str = g_convert(contents, -1, "UTF-8", charset, NULL, NULL, &err);
		} while (err);
	else
		str = g_strdup("");
	g_free(contents);
	
	if (charset != fi->charset) {
		g_free(fi->charset);
		fi->charset = g_strdup(charset);
		if (fi->charset_flag)
			fi->charset_flag = FALSE;
	}
	
//	undo_disconnect_signal(textbuffer);
//	undo_block_signal(buffer);
	force_block_cb_modified_changed(view);
	
	gtk_text_buffer_set_text(buffer, "", 0);
	gtk_text_buffer_get_start_iter(buffer, &iter);
	gtk_text_buffer_insert(buffer, &iter, str, strlen(str));
	gtk_text_buffer_get_start_iter(buffer, &iter);
	gtk_text_buffer_place_cursor(buffer, &iter);
	gtk_text_buffer_set_modified(buffer, FALSE);
	gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(view), &iter, 0, FALSE, 0, 0);
	g_free(str);
	
	force_unblock_cb_modified_changed(view);
	menu_sensitivity_from_modified_flag(FALSE);
//	undo_unblock_signal(buffer);
	
	return 0;
}

gint file_save_real(GtkWidget *view, FileInfo *fi)
{
	FILE *fp;
	GtkTextIter start, end;
	gchar *str;
	gsize rbytes, wbytes;
	GError *err = NULL;
	
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(view));
	
	gtk_text_buffer_get_start_iter(buffer, &start);
	gtk_text_buffer_get_end_iter(buffer, &end);	
	str = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);
	
	switch (fi->lineend) {
	case CR:
		convert_line_ending(&str, CR);
		break;
	case CR+LF:
		convert_line_ending(&str, CR+LF);
	}
	
	if (!fi->charset)
		fi->charset = g_strdup(get_default_charset());
	str = g_convert(str, -1, fi->charset, "UTF-8", &rbytes, &wbytes, &err);
	if (err) {
		switch (err->code) {
		case G_CONVERT_ERROR_ILLEGAL_SEQUENCE:
			run_dialog_message(gtk_widget_get_toplevel(view),
				GTK_MESSAGE_ERROR, _("Can't convert codeset to '%s'"), fi->charset);
			break;
		default:
			run_dialog_message(gtk_widget_get_toplevel(view),
				GTK_MESSAGE_ERROR, err->message);
		}
		g_error_free(err);
		return -1;
	}
	
	fp = fopen(fi->filename, "w");
	if (!fp) {
		run_dialog_message(gtk_widget_get_toplevel(view),
			GTK_MESSAGE_ERROR, _("Can't open file to write"));
		return -1;
	}
	if (fputs(str, fp) == EOF) {
		run_dialog_message(gtk_widget_get_toplevel(view),
			GTK_MESSAGE_ERROR, _("Can't write file"));
		return -1;
	}
	
	gtk_text_buffer_set_modified(buffer, FALSE);
	fclose(fp);
	g_free(str);
	
	return 0;
}
