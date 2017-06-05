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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "main.h"
#include "inc.h"
#include "mainwindow.h"
#include "folderview.h"
#include "summaryview.h"
#include "prefs_common.h"
#include "prefs_account.h"
#include "account.h"
#include "procmsg.h"
#include "socket.h"
#include "ssl.h"
#include "pop.h"
#include "recv.h"
#include "mbox.h"
#include "utils.h"
#include "gtkutils.h"
#include "statusbar.h"
#include "msgcache.h"
#include "manage_window.h"
#include "stock_pixmap.h"
#include "progressdialog.h"
#include "inputdialog.h"
#include "alertpanel.h"
#include "folder.h"
#include "filtering.h"
#include "log.h"
#include "hooks.h"
#include "logwindow.h"

#ifdef MAEMO
#ifdef CHINOOK
#include <hildon/hildon-banner.h>
#include <hildon/hildon-sound.h>
#else
#include <hildon-widgets/hildon-banner.h>
#include <hildon-widgets/hildon-system-sound.h>
#endif
#include <libosso.h>

#ifdef CONIC
#include <conicconnection.h>
#include <conicconnectionevent.h>

static ConIcConnection *maemo_connection = NULL;
static gboolean maemo_warned_offline = FALSE;
#endif
#endif

extern SessionStats session_stats;

static GList *inc_dialog_list = NULL;

guint inc_lock_count = 0;

static GdkPixbuf *currentpix;
static GdkPixbuf *errorpix;
static GdkPixbuf *okpix;

#define MESSAGEBUFSIZE	8192

static void inc_update_stats(gint new_msgs);
static void inc_finished		(MainWindow		*mainwin,
					 gboolean		 new_messages,
					 gboolean		 autocheck);
static gint inc_account_mail_real	(MainWindow		*mainwin,
					 PrefsAccount		*account);

static IncProgressDialog *inc_progress_dialog_create
					(gboolean		 autocheck);
static void inc_progress_dialog_set_list(IncProgressDialog	*inc_dialog);
static void inc_progress_dialog_destroy	(IncProgressDialog	*inc_dialog);

static IncSession *inc_session_new	(PrefsAccount		*account);
static void inc_session_destroy		(IncSession		*session);
static gint inc_start			(IncProgressDialog	*inc_dialog);
static IncState inc_pop3_session_do	(IncSession		*session);

static void inc_progress_dialog_update	(IncProgressDialog	*inc_dialog,
					 IncSession		*inc_session);

static void inc_progress_dialog_set_label
					(IncProgressDialog	*inc_dialog,
					 IncSession		*inc_session);
static void inc_progress_dialog_set_progress
					(IncProgressDialog	*inc_dialog,
					 IncSession		*inc_session);

static void inc_progress_dialog_update_periodic
					(IncProgressDialog	*inc_dialog,
					 IncSession		*inc_session);

static gint inc_recv_data_progressive	(Session	*session,
					 guint		 cur_len,
					 guint		 total_len,
					 gpointer	 data);
static gint inc_recv_data_finished	(Session	*session,
					 guint		 len,
					 gpointer	 data);
static gint inc_recv_message		(Session	*session,
					 const gchar	*msg,
					 gpointer	 data);
static gint inc_drop_message		(Pop3Session	*session,
					 const gchar	*file);

static void inc_put_error		(IncState	 istate,
					 Pop3Session 	*session);

static void inc_showlog_cb		(GtkWidget	*widget,
					 gpointer	 data);
static void inc_cancel_cb		(GtkWidget	*widget,
					 gpointer	 data);
static gint inc_dialog_delete_cb	(GtkWidget	*widget,
					 GdkEventAny	*event,
					 gpointer	 data);

static gint get_spool			(FolderItem	*dest,
					 const gchar	*mbox,
					 PrefsAccount	*account);

static gint inc_spool_account(PrefsAccount *account);
static gint inc_all_spool(void);
static void inc_autocheck_timer_set_interval	(guint		 interval);
static gint inc_autocheck_func			(gpointer	 data);

static void inc_notify_cmd		(gint new_msgs, 
 					 gboolean notify);

static void inc_update_stats(gint new_msgs)
{
	/* update session statistics */
	session_stats.received += new_msgs;
}

/**
 * inc_finished:
 * @mainwin: Main window.
 * @new_messages: TRUE if some messages have been received.
 * 
 * Update the folder view and the summary view after receiving
 * messages.  If @new_messages is FALSE, this function avoids unneeded
 * updating.
 **/
static void inc_finished(MainWindow *mainwin, gboolean new_messages, gboolean autocheck)
{
	if (prefs_common.scan_all_after_inc)
		folderview_check_new(NULL);

	if (!autocheck && new_messages && prefs_common.open_inbox_on_inc) {
		FolderItem *item = NULL;

		if (cur_account && cur_account->inbox)
			item = folder_find_item_from_identifier(cur_account->inbox);
		if (item == NULL && cur_account && cur_account->folder)
			item = cur_account->folder->inbox;
		if (item == NULL)
			item = folder_get_default_inbox();

		folderview_unselect(mainwin->folderview);
		folderview_select(mainwin->folderview, item);
	}
}

void inc_mail(MainWindow *mainwin, gboolean notify)
{
	gint new_msgs = 0;
	gint account_new_msgs = 0;

	if (inc_lock_count) return;

	if (prefs_common.work_offline && 
	    !inc_offline_should_override(TRUE,
		_("Claws Mail needs network access in order "
		  "to get mails.")))
		return;

	inc_lock();
	inc_autocheck_timer_remove();
	main_window_lock(mainwin);

	if (prefs_common.use_extinc && prefs_common.extinc_cmd) {
		/* external incorporating program */
		if (execute_command_line(prefs_common.extinc_cmd, FALSE) < 0) {
			main_window_unlock(mainwin);
			inc_autocheck_timer_set();
			inc_unlock();
			return;
		}
	} else {
		account_new_msgs = inc_account_mail_real(mainwin, cur_account);
		if (account_new_msgs > 0)
			new_msgs += account_new_msgs;
	}

	inc_update_stats(new_msgs);
	inc_finished(mainwin, new_msgs > 0, FALSE);
	main_window_unlock(mainwin);
 	inc_notify_cmd(new_msgs, notify);
	inc_autocheck_timer_set();
	inc_unlock();
}

void inc_pop_before_smtp(PrefsAccount *acc)
{
	IncProgressDialog *inc_dialog;
	IncSession *session;
	MainWindow *mainwin;

	mainwin = mainwindow_get_mainwindow();

    	session = inc_session_new(acc);
    	if (!session) return;
	POP3_SESSION(session->session)->pop_before_smtp = TRUE;
		
    	inc_dialog = inc_progress_dialog_create(FALSE);
    	inc_dialog->queue_list = g_list_append(inc_dialog->queue_list,
					       session);
	/* FIXME: assumes to attach to first main window */
	inc_dialog->mainwin = mainwin;
	inc_progress_dialog_set_list(inc_dialog);

	if (mainwin) {
		toolbar_main_set_sensitive(mainwin);
		main_window_set_menu_sensitive(mainwin);
	}
			
	inc_start(inc_dialog);
}

static gint inc_account_mail_real(MainWindow *mainwin, PrefsAccount *account)
{
	IncProgressDialog *inc_dialog;
	IncSession *session;
	
	switch (account->protocol) {
	case A_IMAP4:
	case A_NNTP:
		/* Melvin: bug [14]
		 * FIXME: it should return foldeview_check_new() value.
		 * TODO: do it when bug [19] is fixed (IMAP folder sets 
		 * an incorrect new message count)
		 */
		folderview_check_new(FOLDER(account->folder));
		return 0;
	case A_POP3:
	case A_APOP:
		session = inc_session_new(account);
		if (!session) return 0;
		
		inc_dialog = inc_progress_dialog_create(FALSE);
		inc_dialog->queue_list = g_list_append(inc_dialog->queue_list,
						       session);
		inc_dialog->mainwin = mainwin;
		inc_progress_dialog_set_list(inc_dialog);

		if (mainwin) {
			toolbar_main_set_sensitive(mainwin);
			main_window_set_menu_sensitive(mainwin);
		}
			
		return inc_start(inc_dialog);

	case A_LOCAL:
		return inc_spool_account(account);

	default:
		break;
	}
	return 0;
}

gint inc_account_mail(MainWindow *mainwin, PrefsAccount *account)
{
	gint new_msgs;

	if (inc_lock_count) return 0;

	if (prefs_common.work_offline && 
	    !inc_offline_should_override(TRUE,
		_("Claws Mail needs network access in order "
		  "to get mails.")))
		return 0;

	inc_autocheck_timer_remove();
	main_window_lock(mainwin);

	new_msgs = inc_account_mail_real(mainwin, account);

	inc_update_stats(new_msgs);
	inc_finished(mainwin, new_msgs > 0, FALSE);
	main_window_unlock(mainwin);
	inc_autocheck_timer_set();

	return new_msgs;
}

void inc_all_account_mail(MainWindow *mainwin, gboolean autocheck,
			  gboolean notify)
{
	GList *list, *queue_list = NULL;
	IncProgressDialog *inc_dialog;
	gint new_msgs = 0;
	gint account_new_msgs = 0;
	
	if (prefs_common.work_offline && 
	    !inc_offline_should_override( (autocheck == FALSE),
		_("Claws Mail needs network access in order "
		  "to get mails.")))
		return;

	if (inc_lock_count) return;

	inc_autocheck_timer_remove();
	main_window_lock(mainwin);

	list = account_get_list();
	if (!list) {
		inc_update_stats(new_msgs);
		inc_finished(mainwin, new_msgs > 0, autocheck);
		main_window_unlock(mainwin);
 		inc_notify_cmd(new_msgs, notify);
		inc_autocheck_timer_set();
		return;
	}

	if (prefs_common.use_extinc && prefs_common.extinc_cmd) {
		/* external incorporating program */
		if (execute_command_line(prefs_common.extinc_cmd, FALSE) < 0) {
			log_error(LOG_PROTOCOL, _("%s failed\n"), prefs_common.extinc_cmd);
			
			main_window_unlock(mainwin);
			inc_autocheck_timer_set();
			return;
		}
	}
	
	/* check local folders */
	account_new_msgs = inc_all_spool();
	if (account_new_msgs > 0)
		new_msgs += account_new_msgs;

	/* check IMAP4 / News folders */
	for (list = account_get_list(); list != NULL; list = list->next) {
		PrefsAccount *account = list->data;
		if ((account->protocol == A_IMAP4 ||
		     account->protocol == A_NNTP) && account->recv_at_getall) {
			new_msgs += folderview_check_new(FOLDER(account->folder));
		}
	}

	/* check POP3 accounts */
	for (list = account_get_list(); list != NULL; list = list->next) {
		IncSession *session;
		PrefsAccount *account = list->data;

		if (account->recv_at_getall) {
			session = inc_session_new(account);
			if (session)
				queue_list = g_list_append(queue_list, session);
		}
	}

	if (queue_list) {
		inc_dialog = inc_progress_dialog_create(autocheck);
		inc_dialog->queue_list = queue_list;
		inc_dialog->mainwin = mainwin;
		inc_progress_dialog_set_list(inc_dialog);

		toolbar_main_set_sensitive(mainwin);
		main_window_set_menu_sensitive(mainwin);
		new_msgs += inc_start(inc_dialog);
	}

	inc_update_stats(new_msgs);
	inc_finished(mainwin, new_msgs > 0, autocheck);
	main_window_unlock(mainwin);
 	inc_notify_cmd(new_msgs, notify);
	inc_autocheck_timer_set();
}

static void inc_progress_dialog_size_allocate_cb(GtkWidget *widget,
					 GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.receivewin_width = allocation->width;
	prefs_common.receivewin_height = allocation->height;
}

static IncProgressDialog *inc_progress_dialog_create(gboolean autocheck)
{
	IncProgressDialog *dialog;
	ProgressDialog *progress;
	static GdkGeometry geometry;

	dialog = g_new0(IncProgressDialog, 1);

	progress = progress_dialog_create();
	gtk_window_set_title(GTK_WINDOW(progress->window),
			     _("Retrieving new messages"));
	g_signal_connect(G_OBJECT(progress->showlog_btn), "clicked",
			 G_CALLBACK(inc_showlog_cb), dialog);
	g_signal_connect(G_OBJECT(progress->cancel_btn), "clicked",
			 G_CALLBACK(inc_cancel_cb), dialog);
	g_signal_connect(G_OBJECT(progress->window), "delete_event",
			 G_CALLBACK(inc_dialog_delete_cb), dialog);
	g_signal_connect(G_OBJECT(progress->window), "size_allocate",
			 G_CALLBACK(inc_progress_dialog_size_allocate_cb), NULL);
 	/* manage_window_set_transient(GTK_WINDOW(progress->window)); */

	progress_dialog_get_fraction(progress);

	stock_pixbuf_gdk(progress->treeview, STOCK_PIXMAP_COMPLETE,
			 &okpix);
	stock_pixbuf_gdk(progress->treeview, STOCK_PIXMAP_CONTINUE,
			 &currentpix);
	stock_pixbuf_gdk(progress->treeview, STOCK_PIXMAP_ERROR,
			 &errorpix);

	if (!geometry.min_height) {
		geometry.min_width = 460;
		geometry.min_height = 250;
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(progress->window), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(progress->window, prefs_common.receivewin_width,
				    prefs_common.receivewin_height);

	if (prefs_common.recv_dialog_mode == RECV_DIALOG_ALWAYS ||
	    (prefs_common.recv_dialog_mode == RECV_DIALOG_MANUAL &&
	     !autocheck)) {
		dialog->show_dialog = TRUE;
		gtk_widget_show_now(progress->window);
	}

	dialog->dialog = progress;
	g_get_current_time(&dialog->progress_tv);
	g_get_current_time(&dialog->folder_tv);
	dialog->queue_list = NULL;
	dialog->cur_row = 0;

	inc_dialog_list = g_list_append(inc_dialog_list, dialog);

	return dialog;
}

static void inc_progress_dialog_set_list(IncProgressDialog *inc_dialog)
{
	GList *list;

	for (list = inc_dialog->queue_list; list != NULL; list = list->next) {
		IncSession *session = list->data;
		Pop3Session *pop3_session = POP3_SESSION(session->session);

		session->data = inc_dialog;

		progress_dialog_list_set(inc_dialog->dialog,
					 -1, NULL,
					 pop3_session->ac_prefs->account_name,
					 _("Standby"));
	}
}

static void inc_progress_dialog_clear(IncProgressDialog *inc_dialog)
{
	progress_dialog_get_fraction(inc_dialog->dialog);
	progress_dialog_set_label(inc_dialog->dialog, "");
	if (inc_dialog->mainwin)
		main_window_progress_off(inc_dialog->mainwin);
}

static void inc_progress_dialog_destroy(IncProgressDialog *inc_dialog)
{
	cm_return_if_fail(inc_dialog != NULL);

	inc_dialog_list = g_list_remove(inc_dialog_list, inc_dialog);

	if (inc_dialog->mainwin)
		main_window_progress_off(inc_dialog->mainwin);
	progress_dialog_destroy(inc_dialog->dialog);

	g_free(inc_dialog);
}

static IncSession *inc_session_new(PrefsAccount *account)
{
	IncSession *session;

	cm_return_val_if_fail(account != NULL, NULL);

	if (account->protocol != A_POP3)
		return NULL;
	if (!account->recv_server || !account->userid)
		return NULL;

	session = g_new0(IncSession, 1);

	session->session = pop3_session_new(account);
	session->session->data = session;
	POP3_SESSION(session->session)->drop_message = inc_drop_message;
	session_set_recv_message_notify(session->session,
					inc_recv_message, session);
	session_set_recv_data_progressive_notify(session->session,
						 inc_recv_data_progressive,
						 session);
	session_set_recv_data_notify(session->session,
				     inc_recv_data_finished, session);

	return session;
}

static void inc_session_destroy(IncSession *session)
{
	cm_return_if_fail(session != NULL);

	session_destroy(session->session);
	g_free(session);
}

static gint inc_start(IncProgressDialog *inc_dialog)
{
	IncSession *session;
	GList *qlist;
	Pop3Session *pop3_session;
	IncState inc_state;
	gint error_num = 0;
	gint new_msgs = 0;
	gchar *msg;
	gchar *fin_msg;
	FolderItem *processing, *inbox;
	GSList *msglist, *msglist_element;
	gboolean cancelled = FALSE;

	qlist = inc_dialog->queue_list;
	while (qlist != NULL) {
		GList *next = qlist->next;

		session = qlist->data;
		pop3_session = POP3_SESSION(session->session); 
		pop3_session->user = g_strdup(pop3_session->ac_prefs->userid);
		if (pop3_session->ac_prefs->passwd)
			pop3_session->pass =
				g_strdup(pop3_session->ac_prefs->passwd);
		else {
			gchar *pass;

			if (inc_dialog->show_dialog)
				manage_window_focus_in
					(inc_dialog->dialog->window,
					 NULL, NULL);

			pass = input_dialog_query_password_keep
				(pop3_session->ac_prefs->recv_server,
				 pop3_session->user,
				 &(pop3_session->ac_prefs->session_passwd));

			if (inc_dialog->show_dialog)
				manage_window_focus_out
					(inc_dialog->dialog->window,
					 NULL, NULL);

			if (pass) {
				pop3_session->pass = pass;
			}
		}

		qlist = next;
	}

#define SET_PIXMAP_AND_TEXT(pix, str)					   \
{									   \
	progress_dialog_list_set(inc_dialog->dialog,			   \
				 inc_dialog->cur_row,			   \
				 pix,					   \
				 NULL,					   \
				 str);					   \
}

	for (; inc_dialog->queue_list != NULL && !cancelled; inc_dialog->cur_row++) {
		session = inc_dialog->queue_list->data;
		pop3_session = POP3_SESSION(session->session);
		GSList *filtered, *unfiltered;

		if (pop3_session->pass == NULL) {
			SET_PIXMAP_AND_TEXT(okpix, _("Cancelled"));
			inc_session_destroy(session);
			inc_dialog->queue_list =
				g_list_remove(inc_dialog->queue_list, session);
			continue;
		}

		inc_progress_dialog_clear(inc_dialog);
		progress_dialog_scroll_to_row(inc_dialog->dialog,
					      inc_dialog->cur_row);

		SET_PIXMAP_AND_TEXT(currentpix, _("Retrieving"));

		/* begin POP3 session */
		inc_state = inc_pop3_session_do(session);

		switch (inc_state) {
		case INC_SUCCESS:
			if (pop3_session->cur_total_num > 0)
				msg = g_strdup_printf(
					ngettext("Done (%d message (%s) received)",
						 "Done (%d messages (%s) received)",
					 pop3_session->cur_total_num),
					 pop3_session->cur_total_num,
					 to_human_readable((goffset)pop3_session->cur_total_recv_bytes));
			else
				msg = g_strdup_printf(_("Done (no new messages)"));
			SET_PIXMAP_AND_TEXT(okpix, msg);
			g_free(msg);
			break;
		case INC_CONNECT_ERROR:
			SET_PIXMAP_AND_TEXT(errorpix, _("Connection failed"));
			break;
		case INC_AUTH_FAILED:
			SET_PIXMAP_AND_TEXT(errorpix, _("Auth failed"));
			if (pop3_session->ac_prefs->session_passwd) {
				g_free(pop3_session->ac_prefs->session_passwd);
				pop3_session->ac_prefs->session_passwd = NULL;
			}
			break;
		case INC_LOCKED:
			SET_PIXMAP_AND_TEXT(errorpix, _("Locked"));
			break;
		case INC_ERROR:
		case INC_NO_SPACE:
		case INC_IO_ERROR:
		case INC_SOCKET_ERROR:
		case INC_EOF:
			SET_PIXMAP_AND_TEXT(errorpix, _("Error"));
			break;
		case INC_TIMEOUT:
			SET_PIXMAP_AND_TEXT(errorpix, _("Timeout"));
			break;
		case INC_CANCEL:
			SET_PIXMAP_AND_TEXT(okpix, _("Cancelled"));
			if (!inc_dialog->show_dialog)
				cancelled = TRUE;
			break;
		default:
			break;
		}
		
		if (pop3_session->error_val == PS_AUTHFAIL) {
			if(!prefs_common.no_recv_err_panel) {
				if((prefs_common.recv_dialog_mode == RECV_DIALOG_ALWAYS) ||
				    ((prefs_common.recv_dialog_mode == RECV_DIALOG_MANUAL) && focus_window))
					manage_window_focus_in(inc_dialog->dialog->window, NULL, NULL);
			}
		}

		/* CLAWS: perform filtering actions on dropped message */
		/* CLAWS: get default inbox (perhaps per account) */
		if (pop3_session->ac_prefs->inbox) {
			/* CLAWS: get destination folder / mailbox */
			inbox = folder_find_item_from_identifier(pop3_session->ac_prefs->inbox);
			if (!inbox)
				inbox = folder_get_default_inbox();
		} else
			inbox = folder_get_default_inbox();

		/* get list of messages in processing */
		processing = folder_get_default_processing();
		folder_item_scan(processing);
		msglist = folder_item_get_msg_list(processing);

		/* process messages */
		folder_item_update_freeze();
		
		procmsg_msglist_filter(msglist, pop3_session->ac_prefs, 
				&filtered, &unfiltered, 
				pop3_session->ac_prefs->filter_on_recv);

		filtering_move_and_copy_msgs(msglist);
		if (unfiltered != NULL)		
			folder_item_move_msgs(inbox, unfiltered);

		for(msglist_element = msglist; msglist_element != NULL; 
		    msglist_element = msglist_element->next) {
			MsgInfo *msginfo = (MsgInfo *)msglist_element->data;
			procmsg_msginfo_free(msginfo);
		}
		folder_item_update_thaw();
		
		g_slist_free(msglist);
		g_slist_free(filtered);
		g_slist_free(unfiltered);

		statusbar_pop_all();

		new_msgs += pop3_session->cur_total_num;

		pop3_write_uidl_list(pop3_session);

		if (inc_state != INC_SUCCESS && inc_state != INC_CANCEL) {
			error_num++;
			if (inc_dialog->show_dialog)
				manage_window_focus_in
					(inc_dialog->dialog->window,
					 NULL, NULL);
			inc_put_error(inc_state, pop3_session);
			if (inc_dialog->show_dialog)
				manage_window_focus_out
					(inc_dialog->dialog->window,
					 NULL, NULL);
			if (inc_state == INC_NO_SPACE ||
			    inc_state == INC_IO_ERROR)
				break;
		}
		folder_item_free_cache(processing, TRUE);

		inc_session_destroy(session);
		inc_dialog->queue_list =
			g_list_remove(inc_dialog->queue_list, session);
	}

#undef SET_PIXMAP_AND_TEXT

	if (new_msgs > 0)
		fin_msg = g_strdup_printf(ngettext("Finished (%d new message)",
					  	   "Finished (%d new messages)",
					  	   new_msgs), new_msgs);
	else
		fin_msg = g_strdup_printf(_("Finished (no new messages)"));

	progress_dialog_set_label(inc_dialog->dialog, fin_msg);

	while (inc_dialog->queue_list != NULL) {
		session = inc_dialog->queue_list->data;
		inc_session_destroy(session);
		inc_dialog->queue_list =
			g_list_remove(inc_dialog->queue_list, session);
	}

	if (prefs_common.close_recv_dialog || !inc_dialog->show_dialog)
		inc_progress_dialog_destroy(inc_dialog);
	else {
		gtk_window_set_title(GTK_WINDOW(inc_dialog->dialog->window),
				     fin_msg);
		gtk_button_set_label(GTK_BUTTON(inc_dialog->dialog->cancel_btn),
				     GTK_STOCK_CLOSE);
	}

	g_free(fin_msg);

	return new_msgs;
}

static IncState inc_pop3_session_do(IncSession *session)
{
	Pop3Session *pop3_session = POP3_SESSION(session->session);
	IncProgressDialog *inc_dialog = (IncProgressDialog *)session->data;
	gchar *server;
	gchar *account_name;
	gushort port;
	gchar *buf;

	debug_print("getting new messages of account %s...\n",
		    pop3_session->ac_prefs->account_name);
		    
	pop3_session->ac_prefs->last_pop_login_time = time(NULL);

	buf = g_strdup_printf(_("%s: Retrieving new messages"),
			      pop3_session->ac_prefs->recv_server);
	gtk_window_set_title(GTK_WINDOW(inc_dialog->dialog->window), buf);
	g_free(buf);

	server = pop3_session->ac_prefs->recv_server;
	account_name = pop3_session->ac_prefs->account_name;
#ifdef USE_GNUTLS
	port = pop3_session->ac_prefs->set_popport ?
		pop3_session->ac_prefs->popport :
		pop3_session->ac_prefs->ssl_pop == SSL_TUNNEL ? 995 : 110;
	SESSION(pop3_session)->ssl_type = pop3_session->ac_prefs->ssl_pop;
	if (pop3_session->ac_prefs->ssl_pop != SSL_NONE)
		SESSION(pop3_session)->nonblocking =
			pop3_session->ac_prefs->use_nonblocking_ssl;
#else
	if (pop3_session->ac_prefs->ssl_pop != SSL_NONE) {
		if (alertpanel_full(_("Insecure connection"),
			_("This connection is configured to be secured "
			  "using SSL, but SSL is not available in this "
			  "build of Claws Mail. \n\n"
			  "Do you want to continue connecting to this "
			  "server? The communication would not be "
			  "secure."),
			  GTK_STOCK_CANCEL, _("Con_tinue connecting"), 
			  NULL, FALSE, NULL, ALERT_WARNING,
			  G_ALERTDEFAULT) != G_ALERTALTERNATE)
			return INC_CANCEL;
	}
	port = pop3_session->ac_prefs->set_popport ?
		pop3_session->ac_prefs->popport : 110;
#endif

	buf = g_strdup_printf(_("Account '%s': Connecting to POP3 server: %s:%d..."),
				account_name, server, port);
	statuswindow_print_all("%s", buf);
	log_message(LOG_PROTOCOL, "%s\n", buf);

	progress_dialog_set_label(inc_dialog->dialog, buf);
	GTK_EVENTS_FLUSH();
	g_free(buf);

	session_set_timeout(SESSION(pop3_session),
			    prefs_common.io_timeout_secs * 1000);
	
	if (session_connect(SESSION(pop3_session), server, port) < 0) {
		if(!prefs_common.no_recv_err_panel) {
			if((prefs_common.recv_dialog_mode == RECV_DIALOG_ALWAYS) ||
			    ((prefs_common.recv_dialog_mode == RECV_DIALOG_MANUAL) && focus_window)) {
				manage_window_focus_in(inc_dialog->dialog->window, NULL, NULL);
			}
			alertpanel_error(_("Can't connect to POP3 server: %s:%d"),
					 server, port);
			manage_window_focus_out(inc_dialog->dialog->window, NULL, NULL);
		} else {
			log_error(LOG_PROTOCOL, _("Can't connect to POP3 server: %s:%d\n"),
			    server, port);
		}
		session->inc_state = INC_CONNECT_ERROR;
		statuswindow_pop_all();
		return INC_CONNECT_ERROR;
	}

	while (session_is_running(SESSION(pop3_session)) &&
	       session->inc_state != INC_CANCEL)
		gtk_main_iteration();

	if (session->inc_state == INC_SUCCESS) {
		switch (pop3_session->error_val) {
		case PS_SUCCESS:
			switch (SESSION(pop3_session)->state) {
			case SESSION_ERROR:
				if (pop3_session->state == POP3_READY)
					session->inc_state = INC_CONNECT_ERROR;
				else
					session->inc_state = INC_ERROR;
				break;
			case SESSION_EOF:
				session->inc_state = INC_EOF;
				break;
			case SESSION_TIMEOUT:
				session->inc_state = INC_TIMEOUT;
				break;
			default:
				session->inc_state = INC_SUCCESS;
				break;
			}
			break;
		case PS_AUTHFAIL:
			session->inc_state = INC_AUTH_FAILED;
			break;
		case PS_IOERR:
			session->inc_state = INC_IO_ERROR;
			break;
		case PS_SOCKET:
			session->inc_state = INC_SOCKET_ERROR;
			break;
		case PS_LOCKBUSY:
			session->inc_state = INC_LOCKED;
			break;
		default:
			session->inc_state = INC_ERROR;
			break;
		}
	}

	session_disconnect(SESSION(pop3_session));
	statusbar_pop_all();

	return session->inc_state;
}

static void inc_progress_dialog_update(IncProgressDialog *inc_dialog,
				       IncSession *inc_session)
{
	inc_progress_dialog_set_label(inc_dialog, inc_session);
	inc_progress_dialog_set_progress(inc_dialog, inc_session);
}

static void inc_progress_dialog_set_label(IncProgressDialog *inc_dialog,
					  IncSession *inc_session)
{
	ProgressDialog *dialog = inc_dialog->dialog;
	Pop3Session *session;

	cm_return_if_fail(inc_session != NULL);

	session = POP3_SESSION(inc_session->session);

	switch (session->state) {
	case POP3_GREETING:
		break;
	case POP3_GETAUTH_USER:
	case POP3_GETAUTH_PASS:
	case POP3_GETAUTH_APOP:
		progress_dialog_set_label(dialog, _("Authenticating..."));
		statuswindow_pop_all();
		statusbar_print_all(_("Retrieving messages from %s (%s) ..."),
				    SESSION(session)->server,
				    session->ac_prefs->account_name);
		break;
	case POP3_GETRANGE_STAT:
		progress_dialog_set_label
			(dialog, _("Getting the number of new messages (STAT)..."));
		break;
	case POP3_GETRANGE_LAST:
		progress_dialog_set_label
			(dialog, _("Getting the number of new messages (LAST)..."));
		break;
	case POP3_GETRANGE_UIDL:
		progress_dialog_set_label
			(dialog, _("Getting the number of new messages (UIDL)..."));
		break;
	case POP3_GETSIZE_LIST:
		progress_dialog_set_label
			(dialog, _("Getting the size of messages (LIST)..."));
		break;
	case POP3_RETR:
	case POP3_RETR_RECV:
	case POP3_DELETE:
		break;
	case POP3_LOGOUT:
		progress_dialog_set_label(dialog, _("Quitting"));
		break;
	default:
		break;
	}
}

static void inc_progress_dialog_set_progress(IncProgressDialog *inc_dialog,
					     IncSession *inc_session)
{
	gchar buf[MESSAGEBUFSIZE];
	Pop3Session *pop3_session = POP3_SESSION(inc_session->session);
	gchar *total_size_str;
	gint cur_total;
	gint total;

	if (!pop3_session->new_msg_exist) return;

	cur_total = inc_session->cur_total_bytes;
	total = pop3_session->total_bytes;
	if (pop3_session->state == POP3_RETR ||
	    pop3_session->state == POP3_RETR_RECV ||
	    pop3_session->state == POP3_DELETE) {
		Xstrdup_a(total_size_str, to_human_readable((goffset)total), return);
		g_snprintf(buf, sizeof(buf),
			   _("Retrieving message (%d / %d) (%s / %s)"),
			   pop3_session->cur_msg, pop3_session->count,
			   to_human_readable((goffset)cur_total), total_size_str);
		progress_dialog_set_label(inc_dialog->dialog, buf);
	}

	progress_dialog_set_fraction
		(inc_dialog->dialog, (total == 0) ? 0: (gfloat)cur_total / (gfloat)total);

	statusbar_progress_all(pop3_session->cur_msg, pop3_session->count, 1);

	if (pop3_session->cur_total_num > 0) {
		g_snprintf(buf, sizeof(buf),
			   ngettext("Retrieving (%d message (%s) received)",
			   	    "Retrieving (%d messages (%s) received)",
				    pop3_session->cur_total_num),
			   pop3_session->cur_total_num,
			   to_human_readable
			   ((goffset)pop3_session->cur_total_recv_bytes));
		progress_dialog_list_set_status(inc_dialog->dialog,
						inc_dialog->cur_row,
						buf);
	}
}

static void inc_progress_dialog_update_periodic(IncProgressDialog *inc_dialog,
						IncSession *inc_session)
{
	GTimeVal tv_cur;
	GTimeVal tv_result;
	gint msec;

	g_get_current_time(&tv_cur);

	tv_result.tv_sec = tv_cur.tv_sec - inc_dialog->progress_tv.tv_sec;
	tv_result.tv_usec = tv_cur.tv_usec - inc_dialog->progress_tv.tv_usec;
	if (tv_result.tv_usec < 0) {
		tv_result.tv_sec--;
		tv_result.tv_usec += G_USEC_PER_SEC;
	}

	msec = tv_result.tv_sec * 1000 + tv_result.tv_usec / 1000;
	if (msec > PROGRESS_UPDATE_INTERVAL) {
		inc_progress_dialog_update(inc_dialog, inc_session);
		inc_dialog->progress_tv.tv_sec = tv_cur.tv_sec;
		inc_dialog->progress_tv.tv_usec = tv_cur.tv_usec;
	}
}

static gint inc_recv_data_progressive(Session *session, guint cur_len,
				      guint total_len, gpointer data)
{
	IncSession *inc_session = (IncSession *)data;
	Pop3Session *pop3_session = POP3_SESSION(session);
	IncProgressDialog *inc_dialog;
	gint cur_total;

	cm_return_val_if_fail(inc_session != NULL, -1);

	if (pop3_session->state != POP3_RETR &&
	    pop3_session->state != POP3_RETR_RECV &&
	    pop3_session->state != POP3_DELETE &&
	    pop3_session->state != POP3_LOGOUT) return 0;

	if (!pop3_session->new_msg_exist) return 0;

	cur_total = pop3_session->cur_total_bytes + cur_len;
	if (cur_total > pop3_session->total_bytes)
		cur_total = pop3_session->total_bytes;
	inc_session->cur_total_bytes = cur_total;

	inc_dialog = (IncProgressDialog *)inc_session->data;
	inc_progress_dialog_update_periodic(inc_dialog, inc_session);

	return 0;
}

static gint inc_recv_data_finished(Session *session, guint len, gpointer data)
{
	IncSession *inc_session = (IncSession *)data;
	IncProgressDialog *inc_dialog;

	cm_return_val_if_fail(inc_session != NULL, -1);

	inc_dialog = (IncProgressDialog *)inc_session->data;

	inc_recv_data_progressive(session, 0, 0, inc_session);

	if (POP3_SESSION(session)->state == POP3_LOGOUT) {
		inc_progress_dialog_update(inc_dialog, inc_session);
	}

	return 0;
}

static gint inc_recv_message(Session *session, const gchar *msg, gpointer data)
{
	IncSession *inc_session = (IncSession *)data;
	IncProgressDialog *inc_dialog;

	cm_return_val_if_fail(inc_session != NULL, -1);

	inc_dialog = (IncProgressDialog *)inc_session->data;

	switch (POP3_SESSION(session)->state) {
	case POP3_GETAUTH_USER:
	case POP3_GETAUTH_PASS:
	case POP3_GETAUTH_APOP:
	case POP3_GETRANGE_STAT:
	case POP3_GETRANGE_LAST:
	case POP3_GETRANGE_UIDL:
	case POP3_GETSIZE_LIST:
		inc_progress_dialog_update(inc_dialog, inc_session);
		break;
	case POP3_RETR:
		inc_recv_data_progressive(session, 0, 0, inc_session);
		break;
	case POP3_LOGOUT:
		inc_progress_dialog_update(inc_dialog, inc_session);
		break;
	default:
		break;
	}

	return 0;
}

static gint inc_drop_message(Pop3Session *session, const gchar *file)
{
	FolderItem *inbox;
	FolderItem *dropfolder;
	IncSession *inc_session = (IncSession *)(SESSION(session)->data);
	gint msgnum;

	cm_return_val_if_fail(inc_session != NULL, -1);

	if (session->ac_prefs->inbox) {
		inbox = folder_find_item_from_identifier
			(session->ac_prefs->inbox);
		if (!inbox)
			inbox = folder_get_default_inbox();
	} else
		inbox = folder_get_default_inbox();
	if (!inbox) {
		claws_unlink(file);
		return -1;
	}

	/* CLAWS: claws uses a global .processing folder for the filtering. */
	dropfolder = folder_get_default_processing();

	/* add msg file to drop folder */
	if ((msgnum = folder_item_add_msg(
			dropfolder, file, NULL, TRUE)) < 0) {
		claws_unlink(file);
		return -1;
	}

	return 0;
}

static void inc_put_error(IncState istate, Pop3Session *session)
{
	gchar *log_msg = NULL;
	gchar *err_msg = NULL;
	gboolean fatal_error = FALSE;

	switch (istate) {
	case INC_CONNECT_ERROR:
		fatal_error = TRUE;
		if (prefs_common.no_recv_err_panel)
			break;
		err_msg = g_strdup_printf(_("Connection to %s:%d failed."),
					  SESSION(session)->server, 
					  SESSION(session)->port);
		break;
	case INC_ERROR:
		log_msg = _("Error occurred while processing mail.");
		fatal_error = TRUE;
		if (prefs_common.no_recv_err_panel)
			break;
		if (session->error_msg)
			err_msg = g_strdup_printf
				(_("Error occurred while processing mail:\n%s"),
				 session->error_msg);
		else
			err_msg = g_strdup(log_msg);
		break;
	case INC_NO_SPACE:
		log_msg = _("No disk space left.");
		err_msg = g_strdup(log_msg);
		fatal_error = TRUE;
		break;
	case INC_IO_ERROR:
		log_msg = _("Can't write file.");
		err_msg = g_strdup(log_msg);
		fatal_error = TRUE;
		break;
	case INC_SOCKET_ERROR:
		log_msg = _("Socket error.");
		if (prefs_common.no_recv_err_panel)
			break;
		err_msg = g_strdup_printf(_("Socket error on connection to %s:%d."),
					  SESSION(session)->server, 
					  SESSION(session)->port);
		break;
	case INC_EOF:
		log_msg = _("Connection closed by the remote host.");
		if (prefs_common.no_recv_err_panel)
			break;
		err_msg = g_strdup_printf(_("Connection to %s:%d closed by the remote host."), 
					  SESSION(session)->server, 
					  SESSION(session)->port);
		break;
	case INC_LOCKED:
		log_msg = _("Mailbox is locked.");
		if (prefs_common.no_recv_err_panel)
			break;
		if (session->error_msg)
			err_msg = g_strdup_printf(_("Mailbox is locked:\n%s"),
						  session->error_msg);
		else
			err_msg = g_strdup(log_msg);
		break;
	case INC_AUTH_FAILED:
		log_msg = _("Authentication failed.");
		fatal_error = TRUE;
		if (prefs_common.no_recv_err_panel)
			break;
		if (session->error_msg)
			err_msg = g_strdup_printf
				(_("Authentication failed:\n%s"), session->error_msg);
		else
			err_msg = g_strdup(log_msg);
		break;
	case INC_TIMEOUT:
		log_msg = _("Session timed out. You may be able to "
			    "recover by increasing the timeout value in "
			    "Preferences/Other/Miscellaneous.");
		if (prefs_common.no_recv_err_panel)
			break;
		err_msg = g_strdup_printf(_("Connection to %s:%d timed out."), 
					  SESSION(session)->server, 
					  SESSION(session)->port);
		break;
	default:
		break;
	}

	if (log_msg) {
		if (fatal_error)
			log_error(LOG_PROTOCOL, "%s\n", log_msg);
		else
			log_warning(LOG_PROTOCOL, "%s\n", log_msg);
	}
	if (prefs_common.no_recv_err_panel && fatal_error)
		mainwindow_show_error();

	if (err_msg) {
		alertpanel_error_log("%s", err_msg);
		g_free(err_msg);
	}
}

static void inc_cancel(IncProgressDialog *dialog)
{
	IncSession *session;

	cm_return_if_fail(dialog != NULL);

	if (dialog->queue_list == NULL) {
		inc_progress_dialog_destroy(dialog);
		return;
	}

	session = dialog->queue_list->data;

	session->inc_state = INC_CANCEL;

	log_message(LOG_PROTOCOL, _("Incorporation cancelled\n"));
}

gboolean inc_is_active(void)
{
	return (inc_dialog_list != NULL);
}

void inc_cancel_all(void)
{
	GList *cur;

	for (cur = inc_dialog_list; cur != NULL; cur = cur->next)
		inc_cancel((IncProgressDialog *)cur->data);
}

static void inc_showlog_cb(GtkWidget *widget, gpointer data)
{
	MainWindow *mainwin = mainwindow_get_mainwindow();

	log_window_show(mainwin->logwin);
}

static void inc_cancel_cb(GtkWidget *widget, gpointer data)
{
	inc_cancel((IncProgressDialog *)data);
}

static gint inc_dialog_delete_cb(GtkWidget *widget, GdkEventAny *event,
				 gpointer data)
{
	IncProgressDialog *dialog = (IncProgressDialog *)data;

	if (dialog->queue_list == NULL)
		inc_progress_dialog_destroy(dialog);

	return TRUE;
}

static gint inc_spool_account(PrefsAccount *account)
{
	FolderItem *inbox;
	gchar *mbox;
	gint result;

	if (account->local_inbox) {
		inbox = folder_find_item_from_identifier(account->local_inbox);
		if (!inbox)
			inbox = folder_get_default_inbox();
	} else
		inbox = folder_get_default_inbox();

	if (account->local_mbox) {
		if (is_file_exist(account->local_mbox))
			mbox = g_strdup(account->local_mbox);
		else if (is_dir_exist(account->local_mbox)) 
			mbox = g_strconcat(account->local_mbox, G_DIR_SEPARATOR_S,
					   g_get_user_name(), NULL);
		else {
			debug_print("%s: local mailbox not found.\n", 
				    account->local_mbox);
			return -1;
		}
	} else {
		debug_print("local mailbox not set in account info.\n");
		return -1;
	}	

	result = get_spool(inbox, mbox, account);
	g_free(mbox);
	
	statusbar_pop_all();
	
	return result;
}

static gint inc_all_spool(void)
{
	GList *list = NULL;
	gint new_msgs = 0;
	gint account_new_msgs = 0;

	list = account_get_list();
	if (!list) return 0;

	for (; list != NULL; list = list->next) {
		PrefsAccount *account = list->data;

		if ((account->protocol == A_LOCAL) &&
		    (account->recv_at_getall)) {
			account_new_msgs = inc_spool_account(account);
			if (account_new_msgs > 0)
				new_msgs += account_new_msgs;
		}
	}

	return new_msgs;
}

static gint get_spool(FolderItem *dest, const gchar *mbox, PrefsAccount *account)
{
	gint msgs, size;
	gint lockfd;
	gchar tmp_mbox[MAXPATHLEN + 1];

	cm_return_val_if_fail(dest != NULL, -1);
	cm_return_val_if_fail(mbox != NULL, -1);
	cm_return_val_if_fail(account != NULL, -1);

	if (!is_file_exist(mbox) || (size = get_file_size(mbox)) == 0) {
		debug_print("%s: no messages in local mailbox.\n", mbox);
		return 0;
	} else if (size < 0)
		return -1;

	if ((lockfd = lock_mbox(mbox, LOCK_FLOCK)) < 0)
		return -1;

	g_snprintf(tmp_mbox, sizeof(tmp_mbox), "%s%ctmpmbox.%p",
		   get_tmp_dir(), G_DIR_SEPARATOR, mbox);

	if (copy_mbox(lockfd, tmp_mbox) < 0) {
		unlock_mbox(mbox, lockfd, LOCK_FLOCK);
		return -1;
	}

	debug_print("Getting new messages from %s into %s...\n",
		    mbox, dest->path);

	msgs = proc_mbox(dest, tmp_mbox, account->filter_on_recv, account);

	claws_unlink(tmp_mbox);
	if (msgs >= 0) empty_mbox(mbox);
	unlock_mbox(mbox, lockfd, LOCK_FLOCK);

	return msgs;
}

void inc_lock_real(void)
{
	inc_lock_count++;
}

void inc_unlock_real(void)
{
	if (inc_lock_count > 0)
		inc_lock_count--;
}

static guint autocheck_timer = 0;
static gpointer autocheck_data = NULL;

#ifdef MAEMO
osso_context_t *get_osso_context(void);
#endif

static void inc_notify_cmd(gint new_msgs, gboolean notify)
{
#ifndef MAEMO
	gchar *buf, *numpos, *ret_str;
	gssize by_read = 0, by_written = 0;

	if (!(new_msgs && notify && prefs_common.newmail_notify_cmd &&
	    *prefs_common.newmail_notify_cmd))
		     return;

	buf = g_strdup(prefs_common.newmail_notify_cmd);
	if ((numpos = strstr(buf, "%d")) != NULL) {
		gchar *buf2;

		*numpos = '\0';
		buf2 = g_strdup_printf("%s%d%s", buf, new_msgs, numpos + 2);
		g_free(buf);
		buf = buf2;
	}

	ret_str = g_locale_from_utf8(buf, strlen(buf), &by_read, &by_written,
				     NULL);
	if (ret_str && by_written) {
		g_free(buf);
		buf = ret_str;
	}
	debug_print("executing new mail notification command: %s\n", buf);
	execute_command_line(buf, TRUE);

	g_free(buf);

#else
	if (new_msgs) {
		if (prefs_common.maemo_play_sound)
			hildon_play_system_sound("/usr/share/sounds/ui-new_email.wav");
		if (prefs_common.maemo_show_banner) {
			gchar *info = g_strdup_printf(ngettext("Claws Mail: %d new message",
					  	   "Claws Mail: %d new messages",
					  	   new_msgs), new_msgs);
			osso_system_note_infoprint(get_osso_context(), info, NULL);
			g_free(info);
		}
	}
#endif
}

#if (defined(MAEMO) && defined(CONIC))
static void maemo_connection_event(ConIcConnection *connection, 
                     		   ConIcConnectionEvent *event,
				   gpointer user_data)
{
	ConIcConnectionStatus status =
		con_ic_connection_event_get_status(event);
	MainWindow *mainwin = (MainWindow *)user_data;
	switch(status) {
	case CON_IC_STATUS_CONNECTED:
		debug_print("we're connected\n");
		main_window_toggle_work_offline(mainwin, FALSE, FALSE);
		break;
	default:
		debug_print("we're disconnected\n");
		main_window_toggle_work_offline(mainwin, TRUE, FALSE);
		maemo_warned_offline = FALSE;
		break;
	}
}

#endif

void inc_autocheck_timer_init(MainWindow *mainwin)
{
#if (defined(MAEMO) && defined(CONIC))
	GValue *val = g_new0(GValue, 1);
	maemo_connection = con_ic_connection_new();

	g_value_init(val, G_TYPE_BOOLEAN);
	g_value_set_boolean(val, TRUE);
	g_object_set_property(G_OBJECT(maemo_connection),
			"automatic-connection-events", val);
	g_free(val);	
	g_signal_connect (maemo_connection, "connection-event",
			  G_CALLBACK(maemo_connection_event), mainwin);	
	con_ic_connection_connect (maemo_connection,
		CON_IC_CONNECT_FLAG_AUTOMATICALLY_TRIGGERED);
	autocheck_data = mainwin;
#else
	autocheck_data = mainwin;
	inc_autocheck_timer_set();
#endif
}

static void inc_autocheck_timer_set_interval(guint interval)
{
	inc_autocheck_timer_remove();
	/* last test is to avoid re-enabling auto_check after modifying 
	   the common preferences */
	if (prefs_common.autochk_newmail && autocheck_data
	    && prefs_common.work_offline == FALSE) {
#if GLIB_CHECK_VERSION(2,14,0)
		if (interval % 1000 == 0)
			autocheck_timer =
				g_timeout_add_seconds(interval/1000, inc_autocheck_func, autocheck_data);
		else
#endif
		autocheck_timer = g_timeout_add
			(interval, inc_autocheck_func, autocheck_data);
		debug_print("added timer = %d\n", autocheck_timer);
	}
}

void inc_autocheck_timer_set(void)
{
	inc_autocheck_timer_set_interval(prefs_common.autochk_itv * 60000);
#if (defined(MAEMO) && defined(CONIC))
	con_ic_connection_connect (maemo_connection,
		CON_IC_CONNECT_FLAG_AUTOMATICALLY_TRIGGERED);
#endif
}

void inc_autocheck_timer_remove(void)
{
	if (autocheck_timer) {
		debug_print("removed timer = %d\n", autocheck_timer);
		g_source_remove(autocheck_timer);
		autocheck_timer = 0;
	}
}

static gint inc_autocheck_func(gpointer data)
{
	MainWindow *mainwin = (MainWindow *)data;

	if (inc_lock_count) {
		debug_print("autocheck is locked.\n");
		inc_autocheck_timer_set_interval(1000);
		return FALSE;
	}

 	inc_all_account_mail(mainwin, TRUE, prefs_common.newmail_notify_auto);

	return FALSE;
}

gboolean inc_offline_should_override(gboolean force_ask, const gchar *msg)
{
	static time_t overridden_yes = 0;
	static time_t overridden_no  = 0;
	int length = 10; /* minutes */
	gint answer = G_ALERTDEFAULT;

#ifdef HAVE_NETWORKMANAGER_SUPPORT
	/* If no network connection is available, override is not possible */
	if(!networkmanager_is_online(NULL))
		return FALSE;
#endif

#if (defined(MAEMO) && defined(CONIC))
	if (prefs_common.work_offline) {
		if (force_ask && !maemo_warned_offline) {
			if (mainwindow_get_mainwindow())
				hildon_banner_show_information(
					mainwindow_get_mainwindow()->window, 
					NULL,
					_("Unable to connect: you are offline."));
			maemo_warned_offline = TRUE;
		}
		return FALSE;
	} else {
		return TRUE;
	}
#endif	

	if (prefs_common.autochk_newmail)
		length = prefs_common.autochk_itv; /* minutes */

	if (force_ask) {
		overridden_no = (time_t)0;
	}

	if (prefs_common.work_offline) {
		gchar *tmp = NULL;
		
		if (time(NULL) - overridden_yes < length * 60) /* seconds */
			 return TRUE;
		else if (time(NULL) - overridden_no < length * 60) /* seconds */
			 return FALSE;

		if (!force_ask)
			tmp = g_strdup_printf(
				_("%s%sYou're working offline. Override for %d minutes?"),
				msg?msg:"", 
				msg?"\n\n":"",
				length);
		else
			tmp = g_strdup_printf(
				_("%s%sYou're working offline. Override?"),
				msg?msg:"", 
				msg?"\n\n":"");

		answer = alertpanel(_("Offline warning"), 
			       tmp,
			       GTK_STOCK_NO, "+" GTK_STOCK_YES, 
				!force_ask? _("On_ly once"):NULL);
		g_free(tmp);
		if (answer == G_ALERTALTERNATE) {
			overridden_yes = time(NULL);
			return TRUE;
		} else if (answer == G_ALERTDEFAULT) {
			if (!force_ask)
				overridden_no  = time(NULL);
			return FALSE;
		} else {
			overridden_yes = (time_t)0;
			overridden_no  = (time_t)0;
			return TRUE;
		}
	}
	return TRUE;
}
