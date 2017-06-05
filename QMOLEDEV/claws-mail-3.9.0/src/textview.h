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

#ifndef __TEXTVIEW_H__
#define __TEXTVIEW_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#include <glib.h>
#include <gtk/gtk.h>

typedef struct _ClickableText	ClickableText;
struct _ClickableText
{
	gchar *uri;

	gchar *filename;

	gpointer data;
	gint data_len;

	guint start;
	guint end;
	
	gboolean is_quote;
	gint quote_level;
	gboolean q_expanded;
	gchar *fg_color;
};


#include "viewtypes.h"
#include "procmime.h"

struct _TextView
{
	GtkWidget *vbox;
	GtkWidget *scrolledwin;
	GtkWidget *text;

	GtkUIManager *ui_manager;
	GtkActionGroup *link_action_group;
	GtkWidget *link_popup_menu;
	GtkActionGroup *mail_action_group;
	GtkWidget *mail_popup_menu;
	GtkActionGroup *file_action_group;
	GtkWidget *file_popup_menu;

	gboolean default_text;
	gboolean is_in_signature;
	gboolean is_diff;
	
	GSList *uri_list;
	gint body_pos;

	MessageView *messageview;
	gint last_buttonpress;

	ClickableText *uri_hover;
	GtkTextIter uri_hover_start_iter;
	GtkTextIter uri_hover_end_iter;
	GtkWidget *image;
	gboolean loading;
	gboolean stop_loading;
	gint prev_quote_level;
};

TextView *textview_create		(void);
void textview_init			(TextView	*textview);
void textview_reflect_prefs		(TextView	*textview);

void textview_show_part		(TextView	*textview,
				 MimeInfo	*mimeinfo,
				 FILE		*fp);
void textview_show_error	(TextView	*textview);
void textview_show_info		(TextView	*textview,
				 const gchar	*info_str);
void textview_show_mime_part	(TextView	*textview,
				 MimeInfo	*partinfo);
void textview_clear		(TextView	*textview);
void textview_destroy		(TextView	*textview);
void textview_set_font		(TextView	*textview,
				 const gchar	*codeset);
void textview_set_text		(TextView	*textview,
				 const gchar	*text);
void textview_set_position	(TextView	*textview,
				 gint		 pos);
void textview_scroll_one_line	(TextView	*textview,
				 gboolean	 up);
gboolean textview_scroll_page	(TextView	*textview,
				 gboolean	 up);
void textview_scroll_max	(TextView 	*textview,
				 gboolean 	 up);

gboolean textview_search_string			(TextView	*textview,
						 const gchar	*str,
						 gboolean	 case_sens);
gboolean textview_search_string_backward	(TextView	*textview,
						 const gchar	*str,
						 gboolean	 case_sens);
void textview_cursor_wait(TextView *textview);
void textview_cursor_normal(TextView *textview);
void textview_show_icon(TextView *textview, const gchar *stock_id);
void textview_get_selection_offsets	(TextView 	*textview, 
					 gint 		*sel_start, 
					 gint		*sel_end);
gboolean textview_uri_security_check	(TextView 	*textview, 
					 ClickableText 	*uri);
gchar *textview_get_visible_uri		(TextView 	*textview, 
					 ClickableText 	*uri);

#define TEXTVIEW_INSERT(str) \
	gtk_text_buffer_insert_with_tags_by_name \
				(buffer, &iter, str, -1,\
				 "header", NULL)

#define TEXTVIEW_INSERT_BOLD(str) \
	gtk_text_buffer_insert_with_tags_by_name \
				(buffer, &iter, str, -1,\
				 "header", "header_title", NULL)

#define TEXTVIEW_INSERT_LINK(str, fname, udata) {				\
	ClickableText *uri;							\
	uri = g_new0(ClickableText, 1);					\
	uri->uri = g_strdup("");					\
	uri->start = gtk_text_iter_get_offset(&iter);			\
	gtk_text_buffer_insert_with_tags_by_name 			\
				(buffer, &iter, str, -1,		\
				 "link", "header_title", "header", 	\
				 NULL); 				\
	uri->end = gtk_text_iter_get_offset(&iter);			\
	uri->filename = fname?g_strdup(fname):NULL;			\
	uri->data = udata;						\
	textview->uri_list =						\
		g_slist_prepend(textview->uri_list, uri);		\
}

#endif /* __TEXTVIEW_H__ */
