/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & The Claws Mail Team
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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#ifdef GDK_WINDOWING_X11
#  include <gdk/gdkx.h>
#endif /* GDK_WINDOWING_X11 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#if HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
#include <signal.h>
#include <unistd.h>

#include "utils.h"
#include "gtkutils.h"
#include "manage_window.h"
#include "mainwindow.h"
#include "prefs_common.h"
#include "alertpanel.h"
#include "inputdialog.h"
#include "action.h"
#include "compose.h"
#include "procmsg.h"
#include "msgcache.h"
#include "textview.h"
#include "matcher_parser.h" /* CLAWS */
#include "filtering.h"
#include "procheader.h"

typedef struct _Children		Children;
typedef struct _ChildInfo		ChildInfo;
typedef struct _UserStringDialog	UserStringDialog;

struct _Children
{
	GtkWidget	*dialog;
	GtkWidget	*text;
	GtkWidget	*input_entry;
	GtkWidget	*input_hbox;
	GtkWidget	*progress_bar;
	GtkWidget	*abort_btn;
	GtkWidget	*close_btn;
	GtkWidget	*scrolledwin;

	gchar		*action;
	ActionType	 action_type;
	GSList		*list;
	gint		 nb;
	gint		 initial_nb;
	gint		 open_in;
	gboolean	 output;

	GtkWidget	*msg_text;

 	gboolean	 is_selection;
};

struct _ChildInfo
{
	Children	*children;
	gchar		*cmd;
	pid_t		 pid;
	gint		 next_sig;
	gint		 chld_in;
	gint		 chld_out;
	gint		 chld_err;
	gint		 chld_status;
	gint		 tag_in;
	gint		 tag_out;
	gint		 tag_err;
	gint		 tag_status;
	gint		 new_out;

	GString		*output;
	void (*callback)(void *data);
	void *data;

	GSList		*msginfo_list;
};

static void action_update_menu		(GtkUIManager   *ui_manager,
					 const gchar	*accel_group,
					 gchar		*branch_path,
					 gpointer	 callback,
					 gpointer	 data);
static void compose_actions_execute_cb	(GtkWidget 	*widget, 
					 gpointer 	 data);
static void compose_actions_execute 	(Compose	*compose,
					 guint		 action_nb,
					 GtkWidget 	*widget);

static void mainwin_actions_execute_cb	(GtkWidget 	*widget, 
					 gpointer 	 data);
static void mainwin_actions_execute 	(MainWindow	*mainwin,
					 guint		 action_nb,
					 GtkWidget 	*widget);

static void msgview_actions_execute_cb	(GtkWidget 	*widget, 
					 gpointer 	 data);
static void msgview_actions_execute 	(MessageView	*msgview,
					 guint		 action_nb,
					 GtkWidget 	*widget);

static void message_actions_execute	(MessageView	*msgview,
					 guint		 action_nb,
					 GSList		*msg_list);

static gboolean execute_filtering_actions(gchar		*action, 
					  GSList	*msglist);

static gboolean execute_actions		(gchar		*action, 
					 GSList		*msg_list, 
					 GtkWidget	*text,
					 gint		 body_pos,
					 MimeInfo	*partinfo, 
					 void (*callback)(void *data),
					 void *data);

static gchar *parse_action_cmd		(gchar		*action,
					 MsgInfo	*msginfo,
					 GSList		*msg_list,
					 MimeInfo	*partinfo,
					 const gchar	*user_str,
					 const gchar	*user_hidden_str,
					 const gchar	*sel_str);
static gboolean parse_append_filename	(GString	*cmd,
					 MsgInfo	*msginfo);

static gboolean parse_append_msgpart	(GString	*cmd,
					 MsgInfo	*msginfo,
					 MimeInfo	*partinfo);

static ChildInfo *fork_child		(gchar		*cmd,
					 const gchar	*msg_str,
					 Children	*children);

static gint wait_for_children		(Children	*children);

static void free_children		(Children	*children);

static void childinfo_close_pipes	(ChildInfo	*child_info);

static void create_io_dialog		(Children	*children);
static void update_io_dialog		(Children	*children);

static void hide_io_dialog_cb		(GtkWidget	*widget,
					 gpointer	 data);
static gint io_dialog_key_pressed_cb	(GtkWidget	*widget,
					 GdkEventKey	*event,
					 gpointer	 data);

static void catch_output		(gpointer		 data,
					 gint			 source,
					 GIOCondition		 cond);
static void catch_input			(gpointer		 data, 
					 gint			 source,
					 GIOCondition		 cond);
static void catch_status		(gpointer		 data,
					 gint			 source,
					 GIOCondition		 cond);

static gchar *get_user_string		(const gchar	*action,
					 ActionType	 type);


ActionType action_get_type(const gchar *action_str)
{
	const gchar *p;
	gboolean in_filtering_action = FALSE;
	ActionType action_type = ACTION_NONE;

	cm_return_val_if_fail(action_str,  ACTION_ERROR);
	cm_return_val_if_fail(*action_str, ACTION_ERROR);

	p = action_str;

	if (p[0] == '|') {
		action_type |= ACTION_PIPE_IN;
		p++;
	} else if (p[0] == '>') {
		action_type |= ACTION_USER_IN;
		p++;
	} else if (p[0] == '*') {
		action_type |= ACTION_USER_HIDDEN_IN;
		p++;
	}

	if (p[0] == '\0')
		return ACTION_ERROR;

	while (*p && action_type != ACTION_ERROR) {
		if (!in_filtering_action) {
			if (p[0] == '%' && p[1]) {
				switch (p[1]) {
				case 'a':
					/* CLAWS: filtering action is a mutually exclusive
					* action. we can enable others if needed later. we
					* add ACTION_SINGLE | ACTION_MULTIPLE so it will
					* only be executed from the main window toolbar */
					if (p[2] == 's')  /* source messages */
						action_type = ACTION_FILTERING_ACTION 
								| ACTION_SINGLE 
								| ACTION_MULTIPLE;
					in_filtering_action = TRUE;
					break;
				case 'f':
					action_type |= ACTION_SINGLE;
					break;
				case 'F':
					action_type |= ACTION_MULTIPLE;
					break;
				case 'p':
					action_type |= ACTION_SINGLE;
					break;
				case 's':
					action_type |= ACTION_SELECTION_STR;
					break;
				case 'u':
					action_type |= ACTION_USER_STR;
					break;
				case 'h':
					action_type |= ACTION_USER_HIDDEN_STR;
					break;
				case '%':
					/* literal '%' */
					break;
				default:
					action_type = ACTION_ERROR;
					break;
				}
				p++;
			} else if (p[0] == '|') {
				if (p[1] == '\0')
					action_type |= ACTION_PIPE_OUT;
			} else if (p[0] == '>') {
				if (p[1] == '\0')
					action_type |= ACTION_INSERT;
			} else if (p[0] == '&') {
				if (p[1] == '\0')
					action_type |= ACTION_ASYNC;
			} else if (p[0] == '}') {
				in_filtering_action = FALSE;
			}
		}
			p++;
	}

	return action_type;
}

static gchar *parse_action_cmd(gchar *action, MsgInfo *msginfo,
			       GSList *msg_list, MimeInfo *partinfo,
			       const gchar *user_str,
			       const gchar *user_hidden_str,
			       const gchar *sel_str)
{
	GString *cmd;
	gchar *p;
	GSList *cur;
	
	p = action;
	
	if (p[0] == '|' || p[0] == '>' || p[0] == '*')
		p++;

	cmd = g_string_sized_new(strlen(action));

	while (p[0] &&
	       !((p[0] == '|' || p[0] == '>' || p[0] == '&') && !p[1])) {
		if (p[0] == '%' && p[1]) {
			switch (p[1]) {
			case 'f':
				if (!parse_append_filename(cmd, msginfo)) {
					g_string_free(cmd, TRUE);
					return NULL;
				}
				p++;
				break;
			case 'F':
				for (cur = msg_list; cur != NULL;
				     cur = cur->next) {
					MsgInfo *msg = (MsgInfo *)cur->data;

					if (!parse_append_filename(cmd, msg)) {
						g_string_free(cmd, TRUE);
						return NULL;
					}
					if (cur->next)
						g_string_append_c(cmd, ' ');
				}
				p++;
				break;
			case 'p':
				if (!parse_append_msgpart(cmd, msginfo,
							  partinfo)) {
					g_string_free(cmd, TRUE);
					return NULL;
				}
				p++;
				break;
			case 's':
				if (sel_str)
					g_string_append(cmd, sel_str);
				p++;
				break;
			case 'u':
				if (user_str)
					g_string_append(cmd, user_str);
				p++;
				break;
			case 'h':
				if (user_hidden_str)
					g_string_append(cmd, user_hidden_str);
				p++;
				break;
			case '%':
				g_string_append_c(cmd, p[1]);
				p++;
				break;
			default:
				g_string_append_c(cmd, p[0]);
				g_string_append_c(cmd, p[1]);
				p++;
			}
		} else {
			g_string_append_c(cmd, p[0]);
		}
		p++;
	}
	if (cmd->len == 0) {
		g_string_free(cmd, TRUE);
		return NULL;
	}

	p = cmd->str;
	g_string_free(cmd, FALSE);
	return p;
}

static gboolean parse_append_filename(GString *cmd, MsgInfo *msginfo)
{
	gchar *filename;
	gchar *p, *q;
	gchar escape_ch[] = "\\ ";

	cm_return_val_if_fail(msginfo, FALSE);

	filename = procmsg_get_message_file(msginfo);

	if (!filename) {
		alertpanel_error(_("Could not get message file %d"),
				 msginfo->msgnum);
		return FALSE;
	}

	p = filename;
	while ((q = strpbrk(p, "$\"`'\\ \t*?[]&|;<>()!#~")) != NULL) {
		escape_ch[1] = *q;
		*q = '\0';
		g_string_append(cmd, p);
		g_string_append(cmd, escape_ch);
		p = q + 1;
	}
	g_string_append(cmd, p);

	g_free(filename);

	return TRUE;
}

static gboolean parse_append_msgpart(GString *cmd, MsgInfo *msginfo,
				     MimeInfo *partinfo)
{
	gboolean single_part = FALSE;
	gchar *filename;
	gchar *part_filename;
	gint ret;

	if (!partinfo) {
		partinfo = procmime_scan_message(msginfo);
		if (!partinfo) {
			alertpanel_error(_("Could not get message part."));
			return FALSE;
		}

		single_part = TRUE;
	}

	filename = procmsg_get_message_file_path(msginfo);
	part_filename = procmime_get_tmp_file_name(partinfo);

	ret = procmime_get_part(part_filename, partinfo);

	if (single_part)
		procmime_mimeinfo_free_all(partinfo);
	g_free(filename);

	if (ret < 0) {
		alertpanel_error(_("Can't get part of multipart message: %s"), strerror(-ret));
		g_free(part_filename);
		return FALSE;
	}

	g_string_append(cmd, part_filename);

	g_free(part_filename);

	return TRUE;
}

void actions_execute(gpointer data, 
		     guint action_nb,
		     GtkWidget *widget,
		     gint source)
{
	if (source == TOOLBAR_MAIN) 
		mainwin_actions_execute((MainWindow*)data, action_nb, widget);
	else if (source == TOOLBAR_COMPOSE)
		compose_actions_execute((Compose*)data, action_nb, widget);
	else if (source == TOOLBAR_MSGVIEW)
		msgview_actions_execute((MessageView*)data, action_nb, widget);	
}

void action_update_mainwin_menu(GtkUIManager *ui_manager,
				gchar *branch_path,
				MainWindow *mainwin)
{
	action_update_menu(ui_manager, "<MainwinActions>", branch_path,
			   mainwin_actions_execute_cb, mainwin);
}

void action_update_msgview_menu(GtkUIManager 	*ui_manager,
				gchar *branch_path,
				MessageView *msgview)
{
	action_update_menu(ui_manager, "<MsgviewActions>", branch_path,
			   msgview_actions_execute_cb, msgview);
}

void action_update_compose_menu(GtkUIManager *ui_manager, 
				gchar *branch_path,
				Compose *compose)
{
	action_update_menu(ui_manager, "<ComposeActions>", branch_path,
			   compose_actions_execute_cb, compose);
}

static GtkWidget *find_item_in_menu(GtkWidget *menu, gchar *name)
{
	GList *children = gtk_container_get_children(GTK_CONTAINER(GTK_MENU_SHELL(menu)));
	GList *amenu = children;
	const gchar *existing_name;
	while (amenu) {
		GtkWidget *item = GTK_WIDGET(amenu->data);
		if ((existing_name = g_object_get_data(G_OBJECT(item), "s_name")) != NULL &&
		    !strcmp2(name, existing_name))
		{
			g_list_free(children);
			 return item;
		}
		amenu = amenu->next;
	}

	g_list_free(children);

	return NULL;
}

static GtkWidget *create_submenus(GtkWidget *menu, const gchar *action)
{
	gchar *submenu = g_strdup(action);
	GtkWidget *new_menu = NULL;
	
	if (strchr(submenu, '/')) {
		const gchar *end = (strchr(submenu, '/')+1);
		GtkWidget *menu_item = NULL;
		if (end && *end) {
			*strchr(submenu, '/') = '\0';
			if ((menu_item = find_item_in_menu(menu, submenu)) == NULL) {
				menu_item = gtk_menu_item_new_with_mnemonic(submenu);
				g_object_set_data_full(G_OBJECT(menu_item), "s_name", g_strdup(submenu), g_free);
				gtk_widget_show(menu_item);
				gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_item);		
				new_menu = gtk_menu_new();
				gtk_widget_show(new_menu);
				gtk_menu_item_set_submenu(GTK_MENU_ITEM(menu_item), new_menu);
			} else {
				new_menu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(menu_item));
			}
			new_menu = create_submenus(new_menu, end);
		}
	}
	g_free(submenu);
	return new_menu ? new_menu : menu;
}

static void action_update_menu(GtkUIManager *ui_manager,
			       const gchar *accel_group,
			       gchar *branch_path,
			       gpointer callback, gpointer data)
{
	GSList *cur;
	gchar *action, *action_p;
	int callback_action = 0;
	GtkWidget *menu = gtk_menu_new();
	GtkWidget *item;

	for (cur = prefs_common.actions_list; cur != NULL; cur = cur->next) {
		GtkWidget *cur_menu = menu;
		const gchar *action_name = NULL;
		action   = g_strdup((gchar *)cur->data);
		action_p = strstr(action, ": ");
		if (action_p && action_p[2] &&
		    (action_get_type(&action_p[2]) != ACTION_ERROR) &&
		    (action[0] != '/')) {
			gchar *accel_path = NULL;

			action_p[0] = '\0';
			if (strchr(action, '/')) {
				cur_menu = create_submenus(cur_menu, action);
				action_name = strrchr(action, '/')+1;
			} else {
				action_name = action;
			}
			gtk_menu_set_accel_group (GTK_MENU (cur_menu), 
				gtk_ui_manager_get_accel_group(ui_manager));
			item = gtk_menu_item_new_with_label(action_name);
			gtk_menu_shell_append(GTK_MENU_SHELL(cur_menu), item);
			g_signal_connect(G_OBJECT(item), "activate",
					 G_CALLBACK(callback), data);
			g_object_set_data(G_OBJECT(item), "action_num", GINT_TO_POINTER(callback_action));
			gtk_widget_show(item);
			accel_path = g_strconcat(accel_group,branch_path, "/", action, NULL);
			gtk_menu_item_set_accel_path(GTK_MENU_ITEM(item), accel_path);
			g_free(accel_path);

		}
		g_free(action);
		callback_action++;
	}

	gtk_widget_show(menu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(gtk_ui_manager_get_widget(ui_manager, branch_path)), menu);
}

static void compose_actions_execute_cb(GtkWidget *widget, gpointer data)
{
	Compose *compose = (Compose *)data;
	gint action_nb = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "action_num"));
	compose_actions_execute(compose, action_nb, NULL);
}

static void compose_actions_execute(Compose *compose, guint action_nb, GtkWidget *widget)
{
	gchar *buf, *action;
	ActionType action_type;

	cm_return_if_fail(action_nb < g_slist_length(prefs_common.actions_list));

	buf = (gchar *)g_slist_nth_data(prefs_common.actions_list, action_nb);
	cm_return_if_fail(buf != NULL);
	action = strstr(buf, ": ");
	cm_return_if_fail(action != NULL);

	/* Point to the beginning of the command-line */
	action += 2;

	action_type = action_get_type(action);
	if (action_type & (ACTION_SINGLE | ACTION_MULTIPLE)) {
		alertpanel_warning
			(_("The selected action cannot be used in the compose window\n"
			   "because it contains %%f, %%F, %%as or %%p."));
		return;
	}

	execute_actions(action, NULL, compose->text, 0, NULL, 
		compose_action_cb, compose);
}

static void mainwin_actions_execute_cb(GtkWidget *widget, gpointer data)
{
	MainWindow *mainwin = (MainWindow *)data;
	gint action_nb = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "action_num"));
	mainwin_actions_execute(mainwin, action_nb, NULL);
}

static void mainwin_actions_execute(MainWindow *mainwin, guint action_nb,
				       GtkWidget *widget)
{
	GSList *msg_list;

	msg_list = summary_get_selected_msg_list(mainwin->summaryview);
	message_actions_execute(mainwin->messageview, action_nb, msg_list);
	g_slist_free(msg_list);
}

static void msgview_actions_execute_cb(GtkWidget *widget, gpointer data)
{
	MessageView *msgview = (MessageView *)data;
	gint action_nb = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget), "action_num"));
	msgview_actions_execute(msgview, action_nb, NULL);
}

static void msgview_actions_execute(MessageView *msgview, guint action_nb,
				       GtkWidget *widget)
{
	GSList *msg_list = NULL;

	if (msgview->msginfo)
		msg_list = g_slist_append(msg_list, msgview->msginfo);
	message_actions_execute(msgview, action_nb, msg_list);
	g_slist_free(msg_list);
}

static void message_actions_execute(MessageView *msgview, guint action_nb,
				    GSList *msg_list)
{
	TextView *textview;
	MimeInfo *partinfo;
	gchar *buf;
	gchar *action;
	GtkWidget *text = NULL;
	guint body_pos = 0;
	ActionType action_type;
	
	cm_return_if_fail(action_nb < g_slist_length(prefs_common.actions_list));

	buf = (gchar *)g_slist_nth_data(prefs_common.actions_list, action_nb);

	cm_return_if_fail(buf);
	cm_return_if_fail((action = strstr(buf, ": ")));

	/* Point to the beginning of the command-line */
	action += 2;

	textview = messageview_get_current_textview(msgview);
	if (textview) {
		text     = textview->text;
		body_pos = textview->body_pos;
	}
	partinfo = messageview_get_selected_mime_part(msgview);

	/* this command will alter the message text */
	action_type = action_get_type(action);
	if (action_type & (ACTION_PIPE_OUT | ACTION_INSERT))
		msgview->filtered = TRUE;

	if (action_type & ACTION_FILTERING_ACTION) 
		/* CLAWS: most of the above code is not necessary for applying
		 * filtering */
		execute_filtering_actions(action, msg_list);
	else
		execute_actions(action, msg_list, text, body_pos, partinfo,
			NULL, NULL);
}

static gboolean execute_filtering_actions(gchar *action, GSList *msglist)
{
	GSList *action_list, *p;
	const gchar *sbegin, *send;
	gchar *action_string;
	SummaryView *summaryview = NULL;
	MainWindow *mainwin = NULL;

	if (mainwindow_get_mainwindow()) {
		summaryview = mainwindow_get_mainwindow()->summaryview;
		mainwin = mainwindow_get_mainwindow();
	}

	if (NULL == (sbegin = strstr2(action, "%as{")))
		return FALSE;
	sbegin += sizeof "%as{" - 1;
	if (NULL == (send = strrchr(sbegin, '}')))
		return FALSE;
	action_string = g_strndup(sbegin, send - sbegin);
	
	action_list = matcher_parser_get_action_list(action_string);
	if (action_list == NULL) {
		gchar *tmp = g_strdup(action_string);

		g_strstrip(tmp);
		if (*tmp == '\0')
			alertpanel_error(_("There is no filtering action set"));
		else
			alertpanel_error(_("Invalid filtering action(s):\n%s"), tmp);
		g_free(action_string);
		g_free(tmp);
		return FALSE;
	}
	g_free(action_string);
	
	/* apply actions on each message info */
	for (p = msglist; p && p->data; p = g_slist_next(p)) {
		filteringaction_apply_action_list(action_list, (MsgInfo *) p->data);
	}

	if (summaryview) {
		summary_lock(summaryview);				
		main_window_cursor_wait(mainwin);		
		summary_freeze(summaryview);	
		folder_item_update_freeze();				
	}

	filtering_move_and_copy_msgs(msglist);

	if (summaryview) {
		folder_item_update_thaw();				
		summary_thaw(summaryview);		
		main_window_cursor_normal(mainwin);	
		summary_unlock(summaryview);				
		summary_show(summaryview, summaryview->folder_item);
	}
	for (p = action_list; p; p = g_slist_next(p))
		if (p->data) filteringaction_free(p->data);	
	g_slist_free(action_list);		
	return TRUE;	
}

static gboolean execute_actions(gchar *action, GSList *msg_list,
				GtkWidget *text,
				gint body_pos, MimeInfo *partinfo,
				void (*callback)(void *data), void *data)
{
	GSList *children_list = NULL;
	gint is_ok  = TRUE;
	gint msg_list_len;
	Children *children;
	ChildInfo *child_info;
	ActionType action_type;
	MsgInfo *msginfo;
	gchar *cmd;
	gchar *sel_str = NULL;
	gchar *msg_str = NULL;
	gchar *user_str = NULL;
	gchar *user_hidden_str = NULL;
	GtkTextIter start_iter, end_iter;
	gboolean is_selection = FALSE;

	cm_return_val_if_fail(action && *action, FALSE);

	action_type = action_get_type(action);

	if (action_type == ACTION_ERROR)
		return FALSE;         /* ERR: syntax error */

	if (action_type & (ACTION_SINGLE | ACTION_MULTIPLE) && !msg_list)
		return FALSE;         /* ERR: file command without selection */

	msg_list_len = g_slist_length(msg_list);

	if (action_type & (ACTION_PIPE_OUT | ACTION_PIPE_IN | ACTION_INSERT)) {
		if (msg_list_len > 1)
			return FALSE; /* ERR: pipe + multiple selection */
		if (!text)
			return FALSE; /* ERR: pipe and no displayed text */
	}

	if (action_type & ACTION_SELECTION_STR) {
		if (!text)
			return FALSE; /* ERR: selection string but no text */
	}

	if (text) {
		GtkTextBuffer *textbuf;

		textbuf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
		is_selection = gtk_text_buffer_get_selection_bounds
			(textbuf, &start_iter, &end_iter);
		if (!is_selection) {
			gtk_text_buffer_get_iter_at_offset
				(textbuf, &start_iter, body_pos);
			gtk_text_buffer_get_end_iter(textbuf, &end_iter);
		}
		msg_str = gtk_text_buffer_get_text
			(textbuf, &start_iter, &end_iter, FALSE);
		if (is_selection)
			sel_str = g_strdup(msg_str);
	}

	if (action_type & ACTION_USER_STR) {
		if (!(user_str = get_user_string(action, ACTION_USER_STR))) {
			g_free(msg_str);
			g_free(sel_str);
			return FALSE;
		}
	}

	if (action_type & ACTION_USER_HIDDEN_STR) {
		if (!(user_hidden_str =
			get_user_string(action, ACTION_USER_HIDDEN_STR))) {
			g_free(msg_str);
			g_free(sel_str);
			g_free(user_str);
			return FALSE;
		}
	}

 	if (text && (action_type & ACTION_PIPE_OUT)) {
 		GtkTextBuffer *textbuf;
 		textbuf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
 		gtk_text_buffer_delete(textbuf, &start_iter, &end_iter);
	}

	children = g_new0(Children, 1);

	children->action      = g_strdup(action);
	children->action_type = action_type;
	children->msg_text    = text;
 	children->is_selection = is_selection;

	if ((action_type & (ACTION_USER_IN | ACTION_USER_HIDDEN_IN)) &&
	    ((action_type & ACTION_SINGLE) == 0 || msg_list_len == 1))
		children->open_in = 1;

	if (action_type & ACTION_SINGLE) {
		GSList *cur;

		for (cur = msg_list; cur && is_ok == TRUE; cur = cur->next) {
			msginfo = (MsgInfo *)cur->data;
			if (!msginfo) {
				is_ok  = FALSE; /* ERR: msginfo missing */
				break;
			}
			cmd = parse_action_cmd(action, msginfo, msg_list,
					       partinfo, user_str,
					       user_hidden_str, sel_str);
			if (!cmd) {
				debug_print("Action command error\n");
				is_ok  = FALSE; /* ERR: incorrect command */
				break;
			}
			if ((child_info = fork_child(cmd, msg_str, children))) {
				/* Pass msginfo to catch_status () */
				if (!(action_type & (ACTION_PIPE_OUT | ACTION_INSERT)))
					child_info->msginfo_list = 
						g_slist_append (NULL, msginfo);
				children_list = g_slist_append(children_list,
							       child_info);
			}
			g_free(cmd);
		}
	} else {
		cmd = parse_action_cmd(action, NULL, msg_list, partinfo,
				       user_str, user_hidden_str, sel_str);
		if (cmd) {
			if ((child_info = fork_child(cmd, msg_str, children))) {
				if (!(action_type & (ACTION_PIPE_OUT | ACTION_INSERT)))
					child_info->msginfo_list = 
						g_slist_copy (msg_list);
				children_list = g_slist_append(children_list,
								child_info);
			}
			g_free(cmd);
		} else
			is_ok  = FALSE;         /* ERR: incorrect command */
	}

	g_free(msg_str);
	g_free(sel_str);
	g_free(user_str);
	g_free(user_hidden_str);

	if (!children_list) {
		 /* If not waiting for children, return */
		free_children(children);
	} else {
		GSList *cur;

		children->list	      = children_list;
		children->nb	      = g_slist_length(children_list);
		children->initial_nb  = children->nb;

		for (cur = children_list; cur; cur = cur->next) {
			child_info = (ChildInfo *) cur->data;
			child_info->callback = callback;
			child_info->data = data;
			child_info->tag_status = 
				claws_input_add(child_info->chld_status,
					      G_IO_IN | G_IO_HUP | G_IO_ERR,
					      catch_status, child_info,
					      FALSE);
		}

		create_io_dialog(children);
	}
	return is_ok;
}

static ChildInfo *fork_child(gchar *cmd, const gchar *msg_str,
			     Children *children)
{
#ifdef G_OS_UNIX
	gint chld_in[2], chld_out[2], chld_err[2], chld_status[2];
	gchar *cmdline[4], *ret_str;
	pid_t pid, gch_pid;
	ChildInfo *child_info;
	gint sync;
	gssize by_written = 0, by_read = 0;

	sync = !(children->action_type & ACTION_ASYNC);

	chld_in[0] = chld_in[1] = chld_out[0] = chld_out[1] = chld_err[0]
		= chld_err[1] = chld_status[0] = chld_status[1] = -1;

	if (sync) {
		if (pipe(chld_status) || pipe(chld_in) || pipe(chld_out) ||
		    pipe(chld_err)) {
			alertpanel_error(_("Command could not be started. "
					   "Pipe creation failed.\n%s"),
					g_strerror(errno));
			/* Closing fd = -1 fails silently */
			(void)close(chld_in[0]);
			(void)close(chld_in[1]);
			(void)close(chld_out[0]);
			(void)close(chld_out[1]);
			(void)close(chld_err[0]);
			(void)close(chld_err[1]);
			(void)close(chld_status[0]);
			(void)close(chld_status[1]);
			return NULL; /* Pipe error */
		}
	}

	debug_print("Forking child and grandchild.\n");
	debug_print("Executing: /bin/sh -c %s\n", cmd);

	pid = fork();
	if (pid == 0) { /* Child */
		int r = 0;
		if (setpgid(0, 0))
			perror("setpgid");

		gch_pid = fork();

		if (gch_pid == 0) {
			if (setpgid(0, getppid()))
				perror("setpgid");

			if (sync) {
				if (children->action_type &
				    (ACTION_PIPE_IN |
				     ACTION_USER_IN |
				     ACTION_USER_HIDDEN_IN)) {
					r |= close(fileno(stdin));
					if (dup(chld_in[0]) < 0)
						r = -1;
				}
				r |= close(chld_in[0]);
				r |= close(chld_in[1]);

				r |= close(fileno(stdout));
				if (dup(chld_out[1]) < 0)
					r = -1;

				r |= close(chld_out[0]);
				r |= close(chld_out[1]);

				r |= close(fileno(stderr));
				if (dup(chld_err[1]) < 0)
					r = -1;

				r |= close(chld_err[0]);
				r |= close(chld_err[1]);

				if (r != 0)
					debug_print("%s(%d)", strerror(errno), errno);
			}

			cmdline[0] = "sh";
			cmdline[1] = "-c";
			ret_str = g_locale_from_utf8(cmd, strlen(cmd),
						     &by_read, &by_written,
						     NULL);
			if (ret_str && by_written)
				cmdline[2] = ret_str;
			else
				cmdline[2] = cmd;
			cmdline[3] = NULL;
			execvp("/bin/sh", cmdline);

			perror("execvp");
			g_free(ret_str);
			_exit(1);
		} else if (gch_pid < (pid_t) 0) { /* Fork error */
			if (sync)
				r = write(chld_status[1], "1\n", 2);
			if (r != 0)
				debug_print("%s(%d)", strerror(errno), errno);
			perror("fork");
			_exit(1);
		} else { /* Child */
			if (sync) {
				r |= close(chld_in[0]);
				r |= close(chld_in[1]);
				r |= close(chld_out[0]);
				r |= close(chld_out[1]);
				r |= close(chld_err[0]);
				r |= close(chld_err[1]);
				r |= close(chld_status[0]);

				debug_print("Child: waiting for grandchild\n");
				r |= waitpid(gch_pid, NULL, 0);
				debug_print("Child: grandchild ended\n");
				r |= write(chld_status[1], "0\n", 2);
				r |= close(chld_status[1]);

				if (r != 0)
					debug_print("%s(%d)", strerror(errno), errno);
			}
			_exit(0);
		}
	} else if (pid < 0) { /* Fork error */
		alertpanel_error(_("Could not fork to execute the following "
				   "command:\n%s\n%s"),
				 cmd, g_strerror(errno));
		return NULL; 
	}

	/* Parent */

	if (!sync) {
		waitpid(pid, NULL, 0);
		return NULL;
	}

	(void)close(chld_in[0]);
	if (!(children->action_type &
	      (ACTION_PIPE_IN | ACTION_USER_IN | ACTION_USER_HIDDEN_IN)))
		(void)close(chld_in[1]);
	(void)close(chld_out[1]);
	(void)close(chld_err[1]);
	(void)close(chld_status[1]);

	child_info = g_new0(ChildInfo, 1);

	child_info->children    = children;

	child_info->pid         = pid;
	child_info->next_sig	= SIGTERM;
	child_info->cmd         = g_strdup(cmd);
	child_info->new_out     = FALSE;
	child_info->output      = g_string_new(NULL);
	child_info->chld_in     =
		(children->action_type &
		 (ACTION_PIPE_IN | ACTION_USER_IN | ACTION_USER_HIDDEN_IN))
			? chld_in [1] : -1;
	child_info->chld_out    = chld_out[0];
	child_info->chld_err    = chld_err[0];
	child_info->chld_status = chld_status[0];
	child_info->tag_in      = -1;
	child_info->tag_out     = claws_input_add(chld_out[0], G_IO_IN | G_IO_HUP | G_IO_ERR,
						catch_output, child_info, FALSE);
	child_info->tag_err     = claws_input_add(chld_err[0], G_IO_IN | G_IO_HUP | G_IO_ERR,
						catch_output, child_info, FALSE);

	if (!(children->action_type &
	      (ACTION_PIPE_IN | ACTION_PIPE_OUT | ACTION_INSERT)))
		return child_info;

	if ((children->action_type & ACTION_PIPE_IN) && msg_str) {
		int r;
		ret_str = g_locale_from_utf8(msg_str, strlen(msg_str),
					     &by_read, &by_written, NULL);
		if (ret_str && by_written) {
			r = write(chld_in[1], ret_str, strlen(ret_str));
			g_free(ret_str);
		} else
			r = write(chld_in[1], msg_str, strlen(msg_str));
		if (!(children->action_type &
		      (ACTION_USER_IN | ACTION_USER_HIDDEN_IN)))
			r = close(chld_in[1]);
		child_info->chld_in = -1; /* No more input */
		if (r != 0)
			debug_print("%s(%d)", strerror(errno), errno);
	}

	return child_info;
#else
	return NULL;
#endif /* G_OS_UNIX */
}

static void kill_children_cb(GtkWidget *widget, gpointer data)
{
#ifdef G_OS_UNIX
	GSList *cur;
	Children *children = (Children *) data;
	ChildInfo *child_info;

	for (cur = children->list; cur; cur = cur->next) {
		child_info = (ChildInfo *)(cur->data);
		debug_print("Killing child group id %d\n", child_info->pid);
		if (child_info->pid && kill(-child_info->pid, child_info->next_sig) < 0)
			perror("kill");
		child_info->next_sig = SIGKILL;
	}
#endif /* G_OS_UNIX */
}

static gint wait_for_children(Children *children)
{
	gboolean new_output;
	ChildInfo *child_info;
	GSList *cur;

	cur = children->list;
	new_output = FALSE;
	while (cur) {
		child_info = (ChildInfo *)cur->data;
		new_output |= child_info->new_out;
		cur = cur->next;
	}

	children->output |= new_output;

	if (new_output || (children->dialog && (children->initial_nb != children->nb)))
		update_io_dialog(children);

	if (children->nb)
		return FALSE;

	if (!children->dialog) {
		free_children(children);
	} else if (!children->output) {
		gtk_widget_destroy(children->dialog);
	}

	return FALSE;
}

static void send_input(GtkWidget *w, gpointer data)
{
	Children *children = (Children *) data;
	ChildInfo *child_info = (ChildInfo *) children->list->data;

	child_info->tag_in = claws_input_add(child_info->chld_in,
					   G_IO_OUT | G_IO_ERR,
					   catch_input, children, FALSE);
}

static gint delete_io_dialog_cb(GtkWidget *w, GdkEvent *e, gpointer data)
{
	hide_io_dialog_cb(w, data);
	return TRUE;
}

static void hide_io_dialog_cb(GtkWidget *w, gpointer data)
{

	Children *children = (Children *)data;

	if (!children->nb) {
		g_signal_handlers_disconnect_matched
			(G_OBJECT(children->dialog), G_SIGNAL_MATCH_DATA,
			 0, 0, NULL, NULL, children);
		gtk_widget_destroy(children->dialog);
		free_children(children);
	}
}

static gint io_dialog_key_pressed_cb(GtkWidget *widget, GdkEventKey *event,
				     gpointer data)
{
	if (event && (event->keyval == GDK_KEY_Escape ||
		      event->keyval == GDK_KEY_Return ||
			  event->keyval == GDK_KEY_KP_Enter))
		hide_io_dialog_cb(widget, data);
	return TRUE;
}

static void childinfo_close_pipes(ChildInfo *child_info)
{
	/* stdout and stderr pipes are guaranteed to be removed by
	 * their handler, but in case where we receive child exit notification
	 * before grand-child's pipes closing signals, we check them and close
	 * them if necessary
	 */
	if (child_info->tag_in > 0)
		g_source_remove(child_info->tag_in);
	if (child_info->tag_out > 0)
		g_source_remove(child_info->tag_out);
	if (child_info->tag_err > 0)
		g_source_remove(child_info->tag_err);

	if (child_info->chld_in >= 0)
		(void)close(child_info->chld_in);
	if (child_info->chld_out >= 0)
		(void)close(child_info->chld_out);
	if (child_info->chld_err >= 0)
		(void)close(child_info->chld_err);

	(void)close(child_info->chld_status);
}

static void free_children(Children *children)
{
	ChildInfo *child_info;
	void (*callback)(void *data) = NULL;
	void *data = NULL;

	debug_print("Freeing children data %p\n", children);

	g_free(children->action);
	while (children->list != NULL) {
		child_info = (ChildInfo *)children->list->data;
		g_free(child_info->cmd);
		g_string_free(child_info->output, TRUE);
		children->list = g_slist_remove(children->list, child_info);
		callback = child_info->callback;
		data = child_info->data;
		g_free(child_info);
	}

	if (callback)
		callback(data);
	
	g_free(children);
}

static void update_io_dialog(Children *children)
{
	GSList *cur;

	debug_print("Updating actions input/output dialog.\n");

	if (children->progress_bar) {
		gchar *text;
#ifdef GENERIC_UMPC
		/* use a more compact format */
		const gchar *format = "%s %d/%d";
#else
		const gchar *format = "%s %d / %d";
#endif
		
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(children->progress_bar),
						  (children->initial_nb == 0) ? 0 :
					      (gdouble) (children->initial_nb - children->nb) /
					      (gdouble) children->initial_nb);
		text = g_strdup_printf(format, _("Completed"), 
				       children->initial_nb - children->nb,
				       children->initial_nb);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(children->progress_bar), text);
		g_free(text);
	}

	if (!children->nb) {
		gtk_widget_set_sensitive(children->abort_btn, FALSE);
		gtk_widget_set_sensitive(children->close_btn, TRUE);
		if (children->input_hbox)
			gtk_widget_set_sensitive(children->input_hbox, FALSE);
		gtk_widget_grab_focus(children->close_btn);
		g_signal_connect(G_OBJECT(children->dialog),
				 "key_press_event",
				 G_CALLBACK(io_dialog_key_pressed_cb),
				 children);
	}

	if (children->output) {
		GtkWidget *text = children->text;
		GtkTextBuffer *textbuf;
		GtkTextIter iter, start_iter, end_iter;
		gchar *caption;
		ChildInfo *child_info;

		gtk_widget_show(children->scrolledwin);
		textbuf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
		gtk_text_buffer_get_bounds(textbuf, &start_iter, &end_iter);
		gtk_text_buffer_delete(textbuf, &start_iter, &end_iter);
		gtk_text_buffer_get_start_iter(textbuf, &iter);

		for (cur = children->list; cur; cur = cur->next) {
			child_info = (ChildInfo *)cur->data;
			if (child_info->pid)
				caption = g_strdup_printf
					(_("--- Running: %s\n"),
					 child_info->cmd);
			else
				caption = g_strdup_printf
					(_("--- Ended: %s\n"),
					 child_info->cmd);

			gtk_text_buffer_insert(textbuf, &iter, caption, -1);
			gtk_text_buffer_insert(textbuf, &iter,
					       child_info->output->str, -1);
			g_free(caption);
			child_info->new_out = FALSE;
		}
	}
}

static void create_io_dialog(Children *children)
{
	GtkWidget *dialog;
	GtkWidget *vbox;
	GtkWidget *entry = NULL;
	GtkWidget *input_hbox = NULL;
	GtkWidget *send_button;
	GtkWidget *label;
	GtkWidget *text;
	GtkWidget *scrolledwin;
	GtkWidget *hbox;
	GtkWidget *progress_bar = NULL;
	GtkWidget *abort_button;
	GtkWidget *close_button;

	debug_print("Creating action IO dialog\n");

	dialog = gtk_dialog_new();
	gtk_container_set_border_width
		(GTK_CONTAINER(gtk_dialog_get_action_area(GTK_DIALOG(dialog))), 5);
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	gtk_window_set_title(GTK_WINDOW(dialog), _("Action's input/output"));
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	manage_window_set_transient(GTK_WINDOW(dialog));
	g_signal_connect(G_OBJECT(dialog), "delete_event",
			 G_CALLBACK(delete_io_dialog_cb), children);
	g_signal_connect(G_OBJECT(dialog), "destroy",
			 G_CALLBACK(hide_io_dialog_cb),
			 children);

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add(GTK_CONTAINER(
				gtk_dialog_get_content_area(GTK_DIALOG(dialog))), vbox);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 8);
	gtk_widget_show(vbox);

	label = gtk_label_new(children->action);
	gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
	gtk_widget_show(label);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);
	gtk_box_pack_start(GTK_BOX(vbox), scrolledwin, TRUE, TRUE, 0);
	gtk_widget_set_size_request(scrolledwin, 560, 200);
	gtk_widget_hide(scrolledwin);

	text = gtk_text_view_new();

	if (prefs_common.textfont) {
		PangoFontDescription *font_desc;
		font_desc = pango_font_description_from_string
			(prefs_common.textfont);
		if (font_desc) {
			gtk_widget_modify_font(text, font_desc);
			pango_font_description_free(font_desc);
		}
	}

	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(text), 6);
	gtk_text_view_set_right_margin(GTK_TEXT_VIEW(text), 6);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);
	gtk_widget_show(text);

	if (children->open_in) {
		input_hbox = gtk_hbox_new(FALSE, 8);
		gtk_widget_show(input_hbox);

		entry = gtk_entry_new();
		gtk_widget_set_size_request(entry, 320, -1);
		g_signal_connect(G_OBJECT(entry), "activate",
				 G_CALLBACK(send_input), children);
		gtk_box_pack_start(GTK_BOX(input_hbox), entry, TRUE, TRUE, 0);
		if (children->action_type & ACTION_USER_HIDDEN_IN) {
			gtk_entry_set_visibility(GTK_ENTRY(entry), FALSE);
#ifdef MAEMO
			hildon_gtk_entry_set_input_mode(GTK_ENTRY(entry), 
				HILDON_GTK_INPUT_MODE_FULL | 
				HILDON_GTK_INPUT_MODE_INVISIBLE);
#endif
		}
		gtk_widget_show(entry);

		send_button = gtk_button_new_from_stock(GTK_STOCK_EXECUTE);
		g_signal_connect(G_OBJECT(send_button), "clicked",
				 G_CALLBACK(send_input), children);
		gtk_box_pack_start(GTK_BOX(input_hbox), send_button, FALSE,
				   FALSE, 0);
		gtk_widget_show(send_button);

		gtk_box_pack_start(GTK_BOX(vbox), input_hbox, FALSE, FALSE, 0);
		gtk_widget_grab_focus(entry);
	}

	if (children->initial_nb > 1) {
		gchar *text;
#ifdef GENERIC_UMPC
		/* use a more compact format */
		const gchar *format = "%s 0/%d\n";
#else
		const gchar *format = "%s 0 / %d\n";
#endif
		
		progress_bar = gtk_progress_bar_new();
#if !GTK_CHECK_VERSION(3, 0, 0)
		gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progress_bar),
				GTK_PROGRESS_LEFT_TO_RIGHT);
#else
		gtk_orientable_set_orientation(GTK_PROGRESS_BAR(progress_bar),
				GTK_ORIENTATION_HORIZONTAL);
		gtk_progress_bar_set_inverted(GTK_PROGRESS_BAR(progress_bar),
				FALSE);
#endif
		text = g_strdup_printf(format, _("Completed"), 
		                       children->initial_nb);
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progress_bar),
					  text);
		g_free(text);
		gtk_box_pack_start(GTK_BOX(vbox), progress_bar, FALSE, FALSE, 0);
		gtk_widget_show(progress_bar);
	}

	gtkut_stock_button_set_create(&hbox, &abort_button, GTK_STOCK_STOP,
				      &close_button, GTK_STOCK_CLOSE, NULL, NULL);
	g_signal_connect(G_OBJECT(abort_button), "clicked",
			 G_CALLBACK(kill_children_cb), children);
	g_signal_connect(G_OBJECT(close_button), "clicked",
			 G_CALLBACK(hide_io_dialog_cb), children);
	gtk_widget_show(hbox);

	if (children->nb)
		gtk_widget_set_sensitive(close_button, FALSE);

	gtk_container_add(GTK_CONTAINER(
			gtk_dialog_get_action_area(GTK_DIALOG(dialog))), hbox);

	children->dialog       = dialog;
	children->scrolledwin  = scrolledwin;
	children->text         = text;
	children->input_hbox   = children->open_in ? input_hbox : NULL;
	children->input_entry  = children->open_in ? entry : NULL;
	children->progress_bar = progress_bar;
	children->abort_btn    = abort_button;
	children->close_btn    = close_button;

	gtk_widget_show(dialog);
}

static void catch_status(gpointer data, gint source, GIOCondition cond)
{
	ChildInfo *child_info = (ChildInfo *)data;
	gchar buf;
	gint c;

	g_source_remove(child_info->tag_status);

	c = read(source, &buf, 1);
	if (c != 1) {
		g_message("error reading child return status\n");
	}
	debug_print("Child returned %c\n", buf);

#ifdef G_OS_UNIX
	waitpid(-child_info->pid, NULL, 0);
#endif
	childinfo_close_pipes(child_info);
	child_info->pid = 0;

	if (child_info->children->action_type & (ACTION_SINGLE | ACTION_MULTIPLE)
	    && child_info->msginfo_list) {
		/* Actions on message *files* might change size and
		* time stamp, and thus invalidate the cache */
		SummaryView *summaryview  = NULL;
		GSList      *cur;
		MsgInfo     *msginfo, *nmi;	/* newmsginfo */
		char        *file;
		gboolean     modified_something = FALSE;
		FolderItem  *last_item = NULL;
		if (mainwindow_get_mainwindow ())
			summaryview = mainwindow_get_mainwindow ()->summaryview;
		for (cur = child_info->msginfo_list; cur; cur = cur->next) {
			msginfo = (MsgInfo *)cur->data;
			if (!(msginfo && /* Stuff used valid? */
			    msginfo->folder && msginfo->folder->cache)) 
				continue;
			file = procmsg_get_message_file_path (msginfo);
			if (!file) 
				continue;
			nmi = procheader_parse_file (file, msginfo->flags, TRUE, FALSE);
			if (!nmi) 
				continue; /* Deleted? */
			if (msginfo->mtime != nmi->mtime || msginfo->size != nmi->size) {
				nmi->folder = msginfo->folder;
				nmi->msgnum = msginfo->msgnum;
				msgcache_update_msg (msginfo->folder->cache, nmi);
				modified_something = TRUE;
				last_item = nmi->folder;
			}
			procmsg_msginfo_free (nmi);
			if (summaryview && summaryview->displayed &&
		    	    summaryview->folder_item == msginfo->folder &&
			    summary_get_msgnum(summaryview, summaryview->displayed) == msginfo->msgnum)
				summary_redisplay_msg(summaryview);
					
		}
		if (modified_something && last_item && 
		    summaryview && summaryview->folder_item == last_item) {
			summary_show (summaryview, summaryview->folder_item);
		}
		g_slist_free (child_info->msginfo_list);
		child_info->msginfo_list = NULL;
	}

	if (!child_info->pid)
		child_info->children->nb--;

	wait_for_children(child_info->children);
}
	
static void catch_input(gpointer data, gint source, GIOCondition cond)
{
	Children *children = (Children *)data;
	ChildInfo *child_info = (ChildInfo *)children->list->data;
	gchar *input, *ret_str;
	gint c, count, len, r;
	gssize by_read = 0, by_written = 0;

	debug_print("Sending input to grand child.\n");
	if (!(cond & (G_IO_OUT | G_IO_ERR)))
		return;

	gtk_widget_set_sensitive(children->input_hbox, FALSE);
	gtk_widget_grab_focus(children->abort_btn);

	g_source_remove(child_info->tag_in);
	child_info->tag_in = -1;

	input = gtk_editable_get_chars(GTK_EDITABLE(children->input_entry),
				       0, -1);
	ret_str = g_locale_from_utf8(input, strlen(input), &by_read,
				     &by_written, NULL);
	if (ret_str && by_written) {
		g_free(input);
		input = ret_str;
	}

	len = strlen(input);
	count = 0;

	do {
		c = write(child_info->chld_in, input + count, len - count);
		if (c >= 0)
			count += c;
	} while (c >= 0 && count < len);

	if (c >= 0)
		r = write(child_info->chld_in, "\n", 2);

	g_free(input);

	r = close(child_info->chld_in);
	if (r != 0)
		debug_print("%s(%d)", strerror(errno), errno);
	child_info->chld_in = -1;
	debug_print("Input to grand child sent.\n");
}

static void catch_output(gpointer data, gint source, GIOCondition cond)
{
	ChildInfo *child_info = (ChildInfo *)data;
	gint c;
	gchar buf[BUFFSIZE];

	debug_print("Catching grand child's output.\n");
	if (child_info->children->action_type &
	    (ACTION_PIPE_OUT | ACTION_INSERT)
	    && source == child_info->chld_out) {
		GtkTextView *text =
			GTK_TEXT_VIEW(child_info->children->msg_text);
		GtkTextBuffer *textbuf = gtk_text_view_get_buffer(text);
		GtkTextIter iter;
		GtkTextMark *mark;
		gint ins_pos;

		mark = gtk_text_buffer_get_insert(textbuf);
		gtk_text_buffer_get_iter_at_mark(textbuf, &iter, mark);
		ins_pos = gtk_text_iter_get_offset(&iter);

		while (TRUE) {
			gsize bytes_read = 0, bytes_written = 0;
			gchar *ret_str;

			c = read(source, buf, sizeof(buf) - 1);
			if (c == 0)
				break;

			ret_str = g_locale_to_utf8
				(buf, c, &bytes_read, &bytes_written, NULL);
			if (ret_str && bytes_written > 0) {
				gtk_text_buffer_insert
					(textbuf, &iter, ret_str,
					 -1);
				g_free(ret_str);
			} else
				gtk_text_buffer_insert(textbuf, &iter, buf, c);
		}

		if (child_info->children->is_selection) {
			GtkTextIter ins;

			gtk_text_buffer_get_iter_at_offset
				(textbuf, &ins, ins_pos);
			gtk_text_buffer_select_range(textbuf, &ins, &iter);
		}
	} else {
		c = read(source, buf, sizeof(buf) - 1);
		if (c > 0) {
			gsize bytes_read = 0, bytes_written = 0;
			gchar *ret_str;

			ret_str = g_locale_to_utf8
				(buf, c, &bytes_read, &bytes_written, NULL);
			if (ret_str && bytes_written > 0) {
				g_string_append_len
					(child_info->output, ret_str,
					 bytes_written);
				g_free(ret_str);
			} else
				g_string_append_len(child_info->output, buf, c);

			child_info->new_out = TRUE;
		}
	}
	if (c == 0) {
		if (source == child_info->chld_out) {
			g_source_remove(child_info->tag_out);
			child_info->tag_out = -1;
			(void)close(child_info->chld_out);
			child_info->chld_out = -1;
		} else {
			g_source_remove(child_info->tag_err);
			child_info->tag_err = -1;
			(void)close(child_info->chld_err);
			child_info->chld_err = -1;
		}
	}
	
	wait_for_children(child_info->children);
}

static gchar *get_user_string(const gchar *action, ActionType type)
{
	gchar *message;
	gchar *user_str = NULL;

	switch (type) {
	case ACTION_USER_HIDDEN_STR:
		message = g_strdup_printf
			(_("Enter the argument for the following action:\n"
			   "('%%h' will be replaced with the argument)\n"
			   "  %s"),
			 action);
		user_str = input_dialog_with_invisible
			(_("Action's hidden user argument"), message, NULL);
		break;
	case ACTION_USER_STR:
		message = g_strdup_printf
			(_("Enter the argument for the following action:\n"
			   "('%%u' will be replaced with the argument)\n"
			   "  %s"),
			 action);
		user_str = input_dialog
			(_("Action's user argument"), message, NULL);
		break;
	default:
		g_warning("Unsupported action type %d", type);
	}

	return user_str;
}
