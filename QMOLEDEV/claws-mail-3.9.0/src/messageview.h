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

#ifndef __MESSAGEVIEW_H__
#define __MESSAGEVIEW_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#include <glib.h>
#include <gtk/gtk.h>

#include "mainwindow.h"
#include "viewtypes.h"
#include "procmsg.h"
#include "procmime.h"
#include "toolbar.h"

#define MESSAGE_VIEW_SHOW_DONE_HOOKLIST "message_view_show_done_hooklist"


struct _MessageView
{
	GtkWidget *vbox;

	gboolean new_window;
	GtkWidget *window;

	/* Toolbar handlebox */
	GtkWidget *handlebox;
	Toolbar *toolbar;
	GtkWidget *menubar;

	HeaderView *headerview;
	MimeView *mimeview;
	NoticeView *noticeview;
	GtkWidget *statusbar;
	gint statusbar_cid;

	MainWindow *mainwin;

	MsgInfo *msginfo;

	gchar *forced_charset;
	EncodingType forced_encoding;

	gboolean visible;

	/* this message was filtered by an action */
	gboolean filtered;
  
	/* From messageview_show */
	gboolean all_headers;

	gint msginfo_update_callback_id;
	gboolean updating;
	gboolean deferred_destroy;
	
	gboolean show_full_text;
	gboolean partial_display_shown;
	gboolean update_needed;
	GtkUIManager *ui_manager;
	GList *trail;
	gint trail_pos;
};

MessageView *messageview_create			(MainWindow	*mainwin);
MessageView *messageview_create_with_new_window	(MainWindow	*mainwin);

void messageview_init				(MessageView	*messageview);
gint messageview_show				(MessageView	*messageview,
						 MsgInfo	*msginfo,
						 gboolean	 all_headers);
void messageview_clear				(MessageView	*messageview);
void messageview_destroy			(MessageView	*messageview);

TextView *messageview_get_current_textview	(MessageView	*messageview);
MimeInfo *messageview_get_selected_mime_part	(MessageView	*messageview);

void messageview_copy_clipboard			(MessageView	*messageview);
void messageview_select_all			(MessageView	*messageview);
void messageview_set_position			(MessageView	*messageview,
						 gint		 pos);
GList *messageview_get_msgview_list		(void);
void messageview_delete				(MessageView 	*messageview);
gboolean messageview_search_string		(MessageView	*messageview,
						 const gchar	*str,
						 gboolean	 case_sens);
gboolean messageview_search_string_backward	(MessageView	*messageview,
						 const gchar	*str,
						 gboolean	 case_sens);

gboolean messageview_is_visible			(MessageView	*messageview);

void messageview_update_actions_menu		(MessageView	*msgview);
void messageview_reflect_prefs_pixmap_theme	(void);
gchar *messageview_get_selection		(MessageView 	*msgview);

void messageview_set_menu_sensitive		(MessageView	*msgview);
void messageview_learn				(MessageView	*msgview,
						 gboolean is_spam);
void messageview_print				(MsgInfo	*msginfo,
						 gboolean	 all_headers,
						 gint		 sel_start,
						 gint		 sel_end,
						 gint		 partnum);
void messageview_list_urls			(MessageView	*msgview);
void messageview_show_partial_display		(MessageView 	*msgview, 
						 MsgInfo 	*msginfo,
						 size_t 	 length);
gboolean messageview_nav_has_prev(MessageView *messageview);
gboolean messageview_nav_has_next(MessageView *messageview);
MsgInfo *messageview_nav_get_prev(MessageView *messageview);
MsgInfo *messageview_nav_get_next(MessageView *messageview);

#endif /* __MESSAGEVIEW_H__ */
