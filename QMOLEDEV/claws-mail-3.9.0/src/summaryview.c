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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "main.h"
#include "menu.h"
#include "mbox.h"
#include "mainwindow.h"
#include "folderview.h"
#include "summaryview.h"
#include "messageview.h"
#include "mimeview.h"
#include "foldersel.h"
#include "procmsg.h"
#include "procheader.h"
#include "sourcewindow.h"
#include "prefs_common.h"
#include "prefs_summary_column.h"
#include "prefs_summary_open.h"
#include "prefs_filtering.h"
#include "account.h"
#include "compose.h"
#include "utils.h"
#include "gtkutils.h"
#include "stock_pixmap.h"
#include "filesel.h"
#include "alertpanel.h"
#include "inputdialog.h"
#include "statusbar.h"
#include "folder.h"
#include "colorlabel.h"
#include "inc.h"
#include "imap.h"
#ifndef USE_NEW_ADDRBOOK
	#include "addressbook.h"
#else
	#include "addressbook-dbus.h"
	#include "addressadd.h"
#endif
#include "addr_compl.h"
#include "folder_item_prefs.h"
#include "filtering.h"
#include "string_match.h"
#include "toolbar.h"
#include "news.h"
#include "hooks.h"
#include "description_window.h"
#include "folderutils.h"
#include "quicksearch.h"
#include "partial_download.h"
#include "tags.h"
#include "timing.h"
#include "log.h"
#include "edittags.h"
#include "manual.h"
#include "manage_window.h"

#define SUMMARY_COL_MARK_WIDTH		10
#define SUMMARY_COL_STATUS_WIDTH	13
#define SUMMARY_COL_LOCKED_WIDTH	13
#define SUMMARY_COL_MIME_WIDTH		11

static int normal_row_height = -1;
static GtkStyle *bold_style;
static GtkStyle *bold_marked_style;
static GtkStyle *bold_deleted_style;
static GtkStyle *small_style;
static GtkStyle *small_marked_style;
static GtkStyle *small_deleted_style;

static GdkPixbuf *markxpm;
static GdkPixbuf *deletedxpm;
static GdkPixbuf *movedxpm;
static GdkPixbuf *copiedxpm;

static GdkPixbuf *newxpm;
static GdkPixbuf *unreadxpm;
static GdkPixbuf *repliedxpm;
static GdkPixbuf *forwardedxpm;
static GdkPixbuf *repliedandforwardedxpm;
static GdkPixbuf *ignorethreadxpm;
static GdkPixbuf *watchthreadxpm;
static GdkPixbuf *lockedxpm;
static GdkPixbuf *spamxpm;

static GdkPixbuf *clipxpm;
static GdkPixbuf *keyxpm;
static GdkPixbuf *clipkeyxpm;
static GdkPixbuf *keysignxpm;
static GdkPixbuf *gpgsignedxpm;
static GdkPixbuf *clipgpgsignedxpm;

static void summary_free_msginfo_func	(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*node,
					 gpointer		 data);
static void summary_set_marks_func	(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*node,
					 gpointer		 data);

void  summary_set_menu_sensitive	(SummaryView		*summaryview);
guint summary_get_msgnum		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*node);


static void summary_set_hide_read_msgs_menu (SummaryView *summaryview,
					     guint action);
static void summary_set_hide_del_msgs_menu (SummaryView *summaryview,
					     guint action);
static void summary_set_hide_read_threads_menu (SummaryView *summaryview,
					     guint action);

static GtkCMCTreeNode *summary_find_prev_msg
					(SummaryView		*summaryview,
					 GtkCMCTreeNode		*current_node);
static GtkCMCTreeNode *summary_find_next_msg
					(SummaryView		*summaryview,
					 GtkCMCTreeNode		*current_node);

static GtkCMCTreeNode *summary_find_prev_flagged_msg
					(SummaryView	*summaryview,
					 GtkCMCTreeNode	*current_node,
					 MsgPermFlags	 flags,
					 gboolean	 start_from_prev);
static GtkCMCTreeNode *summary_find_next_flagged_msg
					(SummaryView	*summaryview,
					 GtkCMCTreeNode	*current_node,
					 MsgPermFlags	 flags,
					 gboolean	 start_from_next);

static GtkCMCTreeNode *summary_find_msg_by_msgnum
					(SummaryView		*summaryview,
					 guint			 msgnum);

static void summary_update_status	(SummaryView		*summaryview);

/* display functions */
static void summary_status_show		(SummaryView		*summaryview);
static void summary_set_column_titles	(SummaryView		*summaryview);
static void summary_set_ctree_from_list	(SummaryView		*summaryview,
					 GSList			*mlist);
static inline void summary_set_header	(SummaryView		*summaryview,
					 gchar			*text[],
					 MsgInfo		*msginfo);
static void summary_display_msg		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);
static void summary_display_msg_full	(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row,
					 gboolean		 new_window,
					 gboolean		 all_headers);
static void summary_set_row_marks	(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);

static gboolean summary_set_row_tag	(SummaryView 		*summaryview, 
					 GtkCMCTreeNode 		*row, 
					 gboolean		 refresh,
					 gboolean 		 set, 
					 gint 			 id);
/* message handling */
static void summary_mark_row		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);
static void summary_lock_row		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);
static void summary_unlock_row		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);
static void summary_mark_row_as_read	(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);
static void summary_mark_row_as_unread	(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);
static void summary_delete_row		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);
static void summary_unmark_row		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row);
static void summary_move_row_to		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row,
					 FolderItem		*to_folder);
static void summary_copy_row_to		(SummaryView		*summaryview,
					 GtkCMCTreeNode		*row,
					 FolderItem		*to_folder);

static gint summary_execute_move	(SummaryView		*summaryview);
static void summary_execute_move_func	(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*node,
					 gpointer		 data);
static void summary_execute_copy	(SummaryView		*summaryview);
static void summary_execute_copy_func	(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*node,
					 gpointer		 data);
static void summary_execute_delete	(SummaryView		*summaryview);
static void summary_execute_delete_func	(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*node,
					 gpointer		 data);
static void summary_execute_expunge	(SummaryView		*summaryview);

static void summary_thread_init		(SummaryView		*summaryview);

static void summary_unthread_for_exec		(SummaryView	*summaryview);
static void summary_unthread_for_exec_func	(GtkCMCTree	*ctree,
						 GtkCMCTreeNode	*node,
						 gpointer	 data);

void summary_simplify_subject(SummaryView *summaryview, gchar * rexp,
			      GSList * mlist);

static void summary_filter_func		(MsgInfo *msginfo);

static void summary_colorlabel_menu_item_activate_cb
					  (GtkWidget	*widget,
					   gpointer	 data);
static void summary_colorlabel_menu_item_activate_item_cb
					  (GtkMenuItem	*label_menu_item,
					   gpointer	 data);
static void summary_colorlabel_menu_create(SummaryView	*summaryview,
					   gboolean  refresh);
static void summary_tags_menu_item_activate_cb
					  (GtkWidget	*widget,
					   gpointer	 data);
static void summary_tags_menu_item_activate_item_cb
					  (GtkMenuItem	*label_menu_item,
					   gpointer	 data);
static void summary_tags_menu_create(SummaryView	*summaryview,
					   gboolean  refresh);

static GtkWidget *summary_ctree_create	(SummaryView	*summaryview);

/* callback functions */
static gint summary_toggle_pressed	(GtkWidget		*eventbox,
					 GdkEventButton		*event,
					 SummaryView		*summaryview);
#ifdef GENERIC_UMPC
static void summary_toggle_multiple_pressed
					(GtkWidget		*widget,
					 SummaryView		*summaryview);
#endif
static gint summary_folder_eventbox_pressed	
					(GtkWidget		*eventbox,
					 GdkEventButton		*event,
					 SummaryView		*summaryview);
static gboolean summary_button_pressed	(GtkWidget		*ctree,
					 GdkEventButton		*event,
					 SummaryView		*summaryview);
static gboolean summary_button_released	(GtkWidget		*ctree,
					 GdkEventButton		*event,
					 SummaryView		*summaryview);
static gboolean summary_key_pressed	(GtkWidget		*ctree,
					 GdkEventKey		*event,
					 SummaryView		*summaryview);
static void summary_tree_expanded	(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*node,
					 SummaryView		*summaryview);
static void summary_tree_collapsed	(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*node,
					 SummaryView		*summaryview);
static void summary_selected		(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*row,
					 gint			 column,
					 SummaryView		*summaryview);
static void summary_unselected		(GtkCMCTree		*ctree,
					 GtkCMCTreeNode		*row,
					 gint			 column,
					 SummaryView		*summaryview);
static void summary_col_resized		(GtkCMCList		*clist,
					 gint			 column,
					 gint			 width,
					 SummaryView		*summaryview);
static void summary_mark_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_status_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_mime_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_num_clicked		(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_score_clicked       (GtkWidget *button,
					 SummaryView *summaryview);
static void summary_size_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_date_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_from_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_to_clicked		(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_subject_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_score_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_locked_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);
static void summary_tags_clicked	(GtkWidget		*button,
					 SummaryView		*summaryview);

static void summary_start_drag		(GtkWidget        *widget, 
					 int button,
					 GdkEvent *event,
					 SummaryView      *summaryview);
static void summary_drag_data_get       (GtkWidget        *widget,
					 GdkDragContext   *drag_context,
					 GtkSelectionData *selection_data,
					 guint             info,
					 guint             time,
					 SummaryView      *summaryview);
static void summary_drag_data_received(GtkWidget        *widget,
					GdkDragContext   *drag_context,
					gint              x,
					gint              y,
					GtkSelectionData *data,
					guint             info,
					guint             time,
					SummaryView       *summaryview);
static gboolean summary_drag_motion_cb(GtkWidget      *widget,
					  GdkDragContext *context,
					  gint            x,
					  gint            y,
					  guint           time,
					  SummaryView	 *summaryview);
static void summary_drag_end(GtkWidget *widget,
					  GdkDragContext *drag_context,
					  SummaryView 	 *summaryview);
/* custom compare functions for sorting */

static gint summary_cmp_by_mark		(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_status	(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_mime		(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_num		(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_size		(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_date		(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_thread_date	(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_from		(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_simplified_subject
					(GtkCMCList 		*clist, 
					 gconstpointer 		 ptr1, 
					 gconstpointer 		 ptr2);
static gint summary_cmp_by_score	(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_label	(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_to		(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_subject	(GtkCMCList		*clist,
					 gconstpointer		 ptr1,
					 gconstpointer		 ptr2);
static gint summary_cmp_by_locked	(GtkCMCList 		*clist,
				         gconstpointer 		 ptr1, 
					 gconstpointer 		 ptr2);
static gint summary_cmp_by_tags		(GtkCMCList 		*clist,
				         gconstpointer 		 ptr1, 
					 gconstpointer 		 ptr2);

static void quicksearch_execute_cb	(QuickSearch    *quicksearch,
					 gpointer	 data);

static void tog_searchbar_cb		(GtkWidget	*w,
					 gpointer	 data);

static void summary_find_answers	(SummaryView 	*summaryview, 
					 MsgInfo	*msg);

static gboolean summary_update_msg	(gpointer source, gpointer data);
static gboolean summary_update_folder_item_hook(gpointer source, gpointer data);
static gboolean summary_update_folder_hook(gpointer source, gpointer data);
static void summary_set_colorlabel_color (GtkCMCTree		*ctree,
				   GtkCMCTreeNode		*node,
				   guint		 labelcolor);
static void summary_thread_build(SummaryView *summaryview);

GtkTargetEntry summary_drag_types[3] =
{
	{"text/uri-list", 0, TARGET_MAIL_URI_LIST},
	{"claws-mail/internal", GTK_TARGET_SAME_APP, TARGET_DUMMY},
	{"claws-mail/msg-path-list", 0, TARGET_MAIL_CM_PATH_LIST},
};

#define DO_ACTION(name, act) {						\
	if(!strcmp(name, a_name)) {					\
		act;							\
	}								\
}

static GtkActionEntry summary_popup_entries[] =
{
	{"SummaryViewPopup",				NULL, "SummaryViewPopup" },
	{"SummaryViewPopup/ReplyTo",			NULL, N_("Repl_y to") },
	{"SummaryViewPopup/Mark",			NULL, N_("_Mark") },
	{"SummaryViewPopup/ColorLabel",			NULL, N_("Color la_bel") },
	{"SummaryViewPopup/Tags",			NULL, N_("Ta_gs") },
	{"SummaryViewPopup/CreateFilterRule",		NULL, N_("Create _filter rule") },
#ifndef GENERIC_UMPC
	{"SummaryViewPopup/CreateProcessingRule",	NULL, N_("Create processing rule") },
#endif
	{"SummaryViewPopup/View",			NULL, N_("_View") },
};

static const gchar *const col_label[N_SUMMARY_COLS] = {
	"",		/* S_COL_MARK    */
	N_("S"),	/* S_COL_STATUS  */
	"",		/* S_COL_MIME    */
	N_("Subject"),	/* S_COL_SUBJECT */
	N_("From"),	/* S_COL_FROM    */
	N_("To"),	/* S_COL_TO      */
	N_("Date"),	/* S_COL_DATE    */
	N_("Size"),	/* S_COL_SIZE    */
	N_("#"),	/* S_COL_NUMBER  */
	N_("Score"),	/* S_COL_SCORE   */
	"",		/* S_COL_LOCKED	 */
	N_("Tags"),	/* S_COL_TAGS	 */
};

void summary_freeze(SummaryView *summaryview)
{
	if (summaryview)
		gtk_cmclist_freeze(GTK_CMCLIST(summaryview->ctree));
}

void summary_thaw(SummaryView *summaryview)
{
	if (summaryview)
		gtk_cmclist_thaw(GTK_CMCLIST(summaryview->ctree));
}

void summary_grab_focus(SummaryView *summaryview)
{
	if (summaryview)
		gtk_widget_grab_focus(summaryview->ctree);
}

GtkWidget *summary_get_main_widget(SummaryView *summaryview)
{
	if (summaryview)
		return summaryview->ctree;
	else
		return NULL;
}

#define START_LONG_OPERATION(summaryview,force_freeze) {	\
	summary_lock(summaryview);				\
	main_window_cursor_wait(summaryview->mainwin);		\
	if (force_freeze || sc_g_list_bigger(GTK_CMCLIST(summaryview->ctree)->selection, 1)) {\
		froze = TRUE;						\
		summary_freeze(summaryview);	\
	}							\
	folder_item_update_freeze();				\
	inc_lock();						\
	hooks_unregister_hook(MSGINFO_UPDATE_HOOKLIST,		\
		      summaryview->msginfo_update_callback_id);	\
}
#define END_LONG_OPERATION(summaryview) {			\
	inc_unlock();						\
	folder_item_update_thaw();				\
	if (froze) 						\
		summary_thaw(summaryview);	\
	main_window_cursor_normal(summaryview->mainwin);	\
	summary_unlock(summaryview);				\
	summaryview->msginfo_update_callback_id =		\
		hooks_register_hook(MSGINFO_UPDATE_HOOKLIST, 	\
		summary_update_msg, (gpointer) summaryview);	\
}

SummaryView *summary_create(MainWindow *mainwin)
{
	SummaryView *summaryview;
	GtkWidget *vbox;
	GtkWidget *scrolledwin;
	GtkWidget *ctree;
	GtkWidget *hbox;
	GtkWidget *hbox_l;
	GtkWidget *stat_box;
	GtkWidget *stat_box2;
	GtkWidget *stat_vbox;
	GtkWidget *statlabel_folder;
	GtkWidget *statlabel_select;
	GtkWidget *statlabel_msgs;
	GtkWidget *hbox_spc;
	GtkWidget *toggle_eventbox;
#ifdef GENERIC_UMPC
	GtkWidget *multiple_sel_togbtn;
#endif
	GtkWidget *toggle_arrow;
	GtkWidget *toggle_search;
	QuickSearch *quicksearch;

	debug_print("Creating summary view...\n");
	summaryview = g_new0(SummaryView, 1);

#if !(GTK_CHECK_VERSION(2,12,0))
	summaryview->tooltips = tips;
#endif
#define SUMMARY_VBOX_SPACING 3
	vbox = gtk_vbox_new(FALSE, SUMMARY_VBOX_SPACING);
	
	/* create status label */
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(hbox);

	stat_vbox = gtk_vbox_new(FALSE, 0);
	gtk_widget_show(stat_vbox);

	stat_box = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(stat_box);
	
	stat_box2 = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(stat_box2);
	
	toggle_search = gtk_toggle_button_new();
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(toggle_search),
				     prefs_common.show_searchbar);
	gtkut_widget_set_can_focus(toggle_search, FALSE);
	gtk_widget_show(toggle_search);

	CLAWS_SET_TIP(toggle_search, _("Toggle quick search bar"));
	
	gtk_box_pack_start(GTK_BOX(hbox), toggle_search, FALSE, FALSE, 2);	

	gtk_box_pack_start(GTK_BOX(hbox), stat_vbox, TRUE, TRUE, 0);	
	gtk_box_pack_start(GTK_BOX(stat_vbox), stat_box, TRUE, TRUE, 0);	
	gtk_box_pack_start(GTK_BOX(stat_vbox), stat_box2, TRUE, TRUE, 0);	

	hbox_l = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(hbox_l);
	gtk_box_pack_start(GTK_BOX(stat_box), hbox_l, TRUE, TRUE, 0);
 
	statlabel_folder = gtk_label_new("");
	gtk_widget_show(statlabel_folder);
	gtk_box_pack_start(GTK_BOX(hbox_l), statlabel_folder, FALSE, FALSE, 2);
	statlabel_select = gtk_label_new("");
	gtk_widget_show(statlabel_select);
	gtk_box_pack_start(GTK_BOX(hbox_l), statlabel_select, FALSE, FALSE, 12);
 
	/* toggle view button */
	toggle_eventbox = gtk_event_box_new();
	gtk_widget_show(toggle_eventbox);
	
	gtk_box_pack_end(GTK_BOX(hbox), toggle_eventbox, FALSE, FALSE, 4);

	toggle_arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_OUT);
	gtk_widget_show(toggle_arrow);
	gtk_container_add(GTK_CONTAINER(toggle_eventbox), toggle_arrow);
	g_signal_connect(G_OBJECT(toggle_eventbox), "button_press_event",
			 G_CALLBACK(summary_toggle_pressed),
			 summaryview);

#ifdef GENERIC_UMPC
	multiple_sel_togbtn = gtk_toggle_button_new();
	gtk_widget_show(multiple_sel_togbtn);
	gtk_box_pack_end(GTK_BOX(hbox), multiple_sel_togbtn, FALSE, FALSE, 4);
	CLAWS_SET_TIP(multiple_sel_togbtn,
			     _("Toggle multiple selection"));
	g_signal_connect(G_OBJECT(multiple_sel_togbtn), "toggled",
			 G_CALLBACK(summary_toggle_multiple_pressed),
			 summaryview);
#endif
	
	statlabel_msgs = gtk_label_new("");
	gtk_widget_show(statlabel_msgs);
	gtk_box_pack_end(GTK_BOX(stat_box), statlabel_msgs, FALSE, FALSE, 4);

	hbox_spc = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(hbox_spc);
	gtk_box_pack_end(GTK_BOX(hbox), hbox_spc, FALSE, FALSE, 6);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(scrolledwin);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	summaryview->mainwidget_book = gtk_notebook_new();
        gtk_notebook_set_show_tabs(GTK_NOTEBOOK(summaryview->mainwidget_book), FALSE);
        gtk_notebook_set_show_border(GTK_NOTEBOOK(summaryview->mainwidget_book), FALSE);
#ifndef GENERIC_UMPC
	gtk_container_add(GTK_CONTAINER(summaryview->mainwidget_book),
		scrolledwin);
	gtk_box_pack_start(GTK_BOX(vbox), summaryview->mainwidget_book, TRUE, TRUE, 0);
#endif
	gtk_widget_set_size_request(vbox,
			     prefs_common.summaryview_width,
			     prefs_common.summaryview_height);

	ctree = summary_ctree_create(summaryview);
	gtk_widget_show(ctree);

	gtk_scrolled_window_set_hadjustment(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_CMCLIST(ctree)->hadjustment);
	gtk_scrolled_window_set_vadjustment(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_CMCLIST(ctree)->vadjustment);
	gtk_container_add(GTK_CONTAINER(scrolledwin), ctree);

	/* status label */
	gtk_widget_show_all(stat_vbox);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	/* quick search */
	quicksearch = quicksearch_new();
	gtk_box_pack_start(GTK_BOX(vbox), quicksearch_get_widget(quicksearch), FALSE, FALSE, 0);

#ifdef GENERIC_UMPC
	gtk_container_add(GTK_CONTAINER(summaryview->mainwidget_book),
		scrolledwin);
	gtk_box_pack_start(GTK_BOX(vbox), summaryview->mainwidget_book, TRUE, TRUE, 0);
#endif
	quicksearch_set_execute_callback(quicksearch, quicksearch_execute_cb, summaryview);

  	g_signal_connect (G_OBJECT(toggle_search), "toggled",
			  G_CALLBACK(tog_searchbar_cb), summaryview);

	/* create popup menu */
	
	gtk_action_group_add_actions(mainwin->action_group, summary_popup_entries,
			G_N_ELEMENTS(summary_popup_entries), (gpointer)summaryview);

	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/", "Menus", "Menus", GTK_UI_MANAGER_MENUBAR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus", "SummaryViewPopup", "SummaryViewPopup", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Reply", "Message/Reply", GTK_UI_MANAGER_MENUITEM)
#ifndef GENERIC_UMPC
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "ReplyTo", "SummaryViewPopup/ReplyTo", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Separator1", "Message/---", GTK_UI_MANAGER_SEPARATOR)
#endif
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Forward", "Message/Forward", GTK_UI_MANAGER_MENUITEM)
#ifndef GENERIC_UMPC
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "ForwardAtt", "Message/ForwardAtt", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Redirect", "Message/Redirect", GTK_UI_MANAGER_MENUITEM)
#endif
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Separator2", "Message/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Move", "Message/Move", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Copy", "Message/Copy", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Trash", "Message/Trash", GTK_UI_MANAGER_MENUITEM)
#ifndef GENERIC_UMPC
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Delete", "Message/Delete", GTK_UI_MANAGER_MENUITEM)
#endif
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Separator3", "Message/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Mark", "SummaryViewPopup/Mark", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "ColorLabel", "SummaryViewPopup/ColorLabel", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Tags", "SummaryViewPopup/Tags", GTK_UI_MANAGER_MENU)

	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Separator4", "Message/---", GTK_UI_MANAGER_SEPARATOR)
#ifndef GENERIC_UMPC
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "AddSenderToAB", "Tools/AddSenderToAB", GTK_UI_MANAGER_MENUITEM)
#endif
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "CreateFilterRule", "SummaryViewPopup/CreateFilterRule", GTK_UI_MANAGER_MENU)
#ifndef GENERIC_UMPC
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "CreateProcessingRule", "SummaryViewPopup/CreateProcessingRule", GTK_UI_MANAGER_MENU)
#endif
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Separator5", "Tools/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "View", "SummaryViewPopup/View", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "SaveAs", "File/SaveAs", GTK_UI_MANAGER_MENUITEM)
#ifndef GENERIC_UMPC
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Print", "File/Print", GTK_UI_MANAGER_MENUITEM)
#endif
	/* last separator, for plugins */
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup", "Separator6", "File/---", GTK_UI_MANAGER_SEPARATOR)

	/* submenus - replyto */
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/ReplyTo", "All", "Message/ReplyTo/All", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/ReplyTo", "Sender", "Message/ReplyTo/Sender", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/ReplyTo", "MailingList", "Message/ReplyTo/List", GTK_UI_MANAGER_MENUITEM)

	/* submenus - mark */
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Mark", "Message/Mark/Mark", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Unmark", "Message/Mark/Unmark", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Separator1", "Message/Mark/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "MarkUnread", "Message/Mark/MarkUnread", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "MarkRead", "Message/Mark/MarkRead", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Separator2", "Message/Mark/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "MarkAllRead", "Message/Mark/MarkAllRead", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Separator3", "Message/Mark/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "IgnoreThread", "Message/Mark/IgnoreThread", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "UnignoreThread", "Message/Mark/UnignoreThread", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "WatchThread", "Message/Mark/WatchThread", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "UnwatchThread", "Message/Mark/UnwatchThread", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Separator4", "Message/Mark/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "MarkSpam", "Message/Mark/MarkSpam", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "MarkHam", "Message/Mark/MarkHam", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Separator5", "Message/Mark/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Lock", "Message/Mark/Lock", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/Mark", "Unlock", "Message/Mark/Unlock", GTK_UI_MANAGER_MENUITEM)

	/* submenus - colorlabel and tags are dynamic */
	/* submenus - createfilterrule */
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/CreateFilterRule", "Automatically", "Tools/CreateFilterRule/Automatically", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/CreateFilterRule", "ByFrom", "Tools/CreateFilterRule/ByFrom", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/CreateFilterRule", "ByTo", "Tools/CreateFilterRule/ByTo", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/CreateFilterRule", "BySubject", "Tools/CreateFilterRule/BySubject", GTK_UI_MANAGER_MENUITEM)
		
#ifndef GENERIC_UMPC
	/* submenus - createprocessingrule */
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/CreateProcessingRule", "Automatically", "Tools/CreateProcessingRule/Automatically", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/CreateProcessingRule", "ByFrom", "Tools/CreateProcessingRule/ByFrom", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/CreateProcessingRule", "ByTo", "Tools/CreateProcessingRule/ByTo", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/CreateProcessingRule", "BySubject", "Tools/CreateProcessingRule/BySubject", GTK_UI_MANAGER_MENUITEM)
#endif
		
	/* submenus - view */
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/View", "OpenNewWindow", "View/OpenNewWindow", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/View", "MessageSource", "View/MessageSource", GTK_UI_MANAGER_MENUITEM)
#ifndef GENERIC_UMPC
	MENUITEM_ADDUI_MANAGER(mainwin->ui_manager, "/Menus/SummaryViewPopup/View", "AllHeaders", "View/AllHeaders", GTK_UI_MANAGER_MENUITEM)
#endif		
	summaryview->popupmenu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
				gtk_ui_manager_get_widget(mainwin->ui_manager, "/Menus/SummaryViewPopup")) );


	summaryview->vbox = vbox;
	summaryview->scrolledwin = scrolledwin;
	summaryview->ctree = ctree;
	summaryview->hbox = hbox;
	summaryview->hbox_l = hbox_l;
	summaryview->hbox_spc = hbox_spc;
	summaryview->stat_box = stat_box;
	summaryview->stat_box2 = stat_box2;
	summaryview->statlabel_folder = statlabel_folder;
	summaryview->statlabel_select = statlabel_select;
	summaryview->statlabel_msgs = statlabel_msgs;
	summaryview->toggle_eventbox = toggle_eventbox;
	summaryview->toggle_arrow = toggle_arrow;
#ifdef GENERIC_UMPC
	summaryview->multiple_sel_togbtn = multiple_sel_togbtn;
#endif
	summaryview->toggle_search = toggle_search;
	summaryview->lock_count = 0;
	summaryview->msginfo_update_callback_id =
		hooks_register_hook(MSGINFO_UPDATE_HOOKLIST, summary_update_msg, (gpointer) summaryview);
	summaryview->folder_item_update_callback_id =
		hooks_register_hook(FOLDER_ITEM_UPDATE_HOOKLIST,
				summary_update_folder_item_hook,
				(gpointer) summaryview);
	summaryview->folder_update_callback_id =
		hooks_register_hook(FOLDER_UPDATE_HOOKLIST,
				summary_update_folder_hook,
				(gpointer) summaryview);

	summaryview->target_list = gtk_target_list_new(summary_drag_types, 3);

	summaryview->quicksearch = quicksearch;

	/* CLAWS: need this to get the SummaryView * from
	 * the CList */
	g_object_set_data(G_OBJECT(ctree), "summaryview", (gpointer)summaryview); 

	gtk_widget_show_all(vbox);

	gtk_widget_show(vbox);

	if (prefs_common.show_searchbar)
		quicksearch_show(quicksearch);
	else
		quicksearch_hide(quicksearch);
	
	if (prefs_common.layout_mode == WIDE_MSGLIST_LAYOUT ||
	    prefs_common.layout_mode == SMALL_LAYOUT)
		gtk_widget_hide(summaryview->toggle_eventbox);

	return summaryview;
}

void summary_relayout(SummaryView *summaryview)
{
	gtk_widget_realize(summaryview->stat_box);

	g_object_ref(summaryview->hbox_l);
	g_object_ref(summaryview->statlabel_msgs);
	
	gtkut_container_remove(GTK_CONTAINER(
		gtk_widget_get_parent(summaryview->hbox_l)), summaryview->hbox_l);
	gtkut_container_remove(GTK_CONTAINER(
		gtk_widget_get_parent(summaryview->statlabel_msgs)), summaryview->statlabel_msgs);

	switch (prefs_common.layout_mode) {
	case NORMAL_LAYOUT:
	case WIDE_LAYOUT:
	case WIDE_MSGLIST_LAYOUT:
		gtk_box_pack_start(GTK_BOX(summaryview->stat_box), summaryview->hbox_l, TRUE, TRUE, 0);
		gtk_box_pack_end(GTK_BOX(summaryview->stat_box), summaryview->statlabel_msgs, FALSE, FALSE, 4);
		gtk_widget_show_all(summaryview->stat_box);
		gtk_widget_show_all(summaryview->stat_box2);
		if (prefs_common.layout_mode == WIDE_MSGLIST_LAYOUT ||
		    prefs_common.layout_mode == SMALL_LAYOUT)
			gtk_widget_hide(summaryview->toggle_eventbox);
		else
			gtk_widget_show(summaryview->toggle_eventbox);
		break;
	case VERTICAL_LAYOUT:
	case SMALL_LAYOUT:
		gtk_box_pack_start(GTK_BOX(summaryview->stat_box), summaryview->hbox_l, TRUE, TRUE, 0);
		gtk_box_pack_start(GTK_BOX(summaryview->stat_box2), summaryview->statlabel_msgs, FALSE, FALSE, 4);
		gtk_widget_show_all(summaryview->stat_box);
		gtk_widget_show_all(summaryview->stat_box2);
		if (prefs_common.layout_mode == SMALL_LAYOUT) {
			gtk_widget_hide(summaryview->toggle_eventbox);
			gtk_widget_hide(summaryview->statlabel_msgs);
		} else {
			gtk_widget_show(summaryview->toggle_eventbox);
			gtk_widget_show(summaryview->statlabel_msgs);
		}
			
		break;
	}
	summary_set_column_order(summaryview);

	g_object_unref(summaryview->hbox_l);
	g_object_unref(summaryview->statlabel_msgs);
	quicksearch_relayout(summaryview->quicksearch);
	if (prefs_common.show_searchbar)
		quicksearch_show(summaryview->quicksearch);
	else
		quicksearch_hide(summaryview->quicksearch);
}

static void summary_set_fonts(SummaryView *summaryview)
{
	PangoFontDescription *font_desc;
	gint size;

	font_desc = pango_font_description_from_string(NORMAL_FONT);
	if (font_desc) {
		gtk_widget_modify_font(summaryview->ctree, font_desc);
		pango_font_description_free(font_desc);
	}

	if (!bold_style) {
		bold_style = gtk_style_copy
			(gtk_widget_get_style(summaryview->ctree));

		if (prefs_common.derive_from_normal_font || !BOLD_FONT) {
			font_desc = pango_font_description_from_string(NORMAL_FONT);
			if (font_desc) {
				pango_font_description_free(bold_style->font_desc);
				bold_style->font_desc = font_desc;
			}
			pango_font_description_set_weight
					(bold_style->font_desc, PANGO_WEIGHT_BOLD);
		} else {
			font_desc = pango_font_description_from_string(BOLD_FONT);
			if (font_desc) {
				pango_font_description_free(bold_style->font_desc);
				bold_style->font_desc = font_desc;
			}
		}
		bold_marked_style = gtk_style_copy(bold_style);
		bold_marked_style->fg[GTK_STATE_NORMAL] =
			summaryview->color_marked;
		bold_deleted_style = gtk_style_copy(bold_style);
		bold_deleted_style->fg[GTK_STATE_NORMAL] =
			summaryview->color_dim;
	}

	if (prefs_common.derive_from_normal_font || !SMALL_FONT) {
		font_desc = pango_font_description_new();
		size = pango_font_description_get_size
			(gtk_widget_get_style(summaryview->ctree)->font_desc);
		pango_font_description_set_size(font_desc, size * PANGO_SCALE_SMALL);
	} else {
		font_desc = pango_font_description_from_string(SMALL_FONT);
	}
	if (font_desc) {
		gtk_widget_modify_font(summaryview->statlabel_folder, font_desc);
		gtk_widget_modify_font(summaryview->statlabel_select, font_desc);
		gtk_widget_modify_font(summaryview->statlabel_msgs, font_desc);
		pango_font_description_free(font_desc);
	}

}

static void summary_set_folder_pixmap(SummaryView *summaryview, StockPixmap icon)
{
	GtkWidget *pixmap; 
	if (!summaryview->folder_pixmap_eventbox) {
		summaryview->folder_pixmap_eventbox = gtk_event_box_new();
		gtk_widget_show(summaryview->folder_pixmap_eventbox);
		gtk_box_pack_start(GTK_BOX(summaryview->hbox_l), summaryview->folder_pixmap_eventbox, FALSE, FALSE, 4);
		gtk_box_reorder_child(GTK_BOX(summaryview->hbox_l), summaryview->folder_pixmap_eventbox, 0); /* search_toggle before */
		g_signal_connect(G_OBJECT(summaryview->folder_pixmap_eventbox), "button_press_event",
			 G_CALLBACK(summary_folder_eventbox_pressed),
			 summaryview);
	}
	if (summaryview->folder_pixmap)
		gtk_widget_destroy(summaryview->folder_pixmap);

	pixmap = stock_pixmap_widget(summaryview->hbox_l, icon);
	gtk_container_add(GTK_CONTAINER(summaryview->folder_pixmap_eventbox), pixmap);
	gtk_widget_show(pixmap);
	summaryview->folder_pixmap = pixmap; 
}

void summary_init(SummaryView *summaryview)
{
	GtkWidget *pixmap;

	gtk_widget_realize(summaryview->ctree);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_MARK,
			 &markxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_DELETED,
			 &deletedxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_NEW,
			 &newxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_UNREAD,
			 &unreadxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_REPLIED,
			 &repliedxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_FORWARDED,
			 &forwardedxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_REPLIED_AND_FORWARDED,
			 &repliedandforwardedxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_CLIP,
			 &clipxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_LOCKED,
			 &lockedxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_IGNORETHREAD,
			 &ignorethreadxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_WATCHTHREAD,
			 &watchthreadxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_CLIP_KEY,
			 &clipkeyxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_KEY_SIGN,
			 &keysignxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_KEY,
			 &keyxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_GPG_SIGNED,
			 &gpgsignedxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_CLIP_GPG_SIGNED,
			 &clipgpgsignedxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_SPAM,
			 &spamxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_MOVED,
			 &movedxpm);
	stock_pixbuf_gdk(summaryview->ctree, STOCK_PIXMAP_COPIED,
			 &copiedxpm);

	summary_set_fonts(summaryview);

	summary_set_folder_pixmap(summaryview, STOCK_PIXMAP_DIR_OPEN);

	pixmap = stock_pixmap_widget(summaryview->hbox, STOCK_PIXMAP_QUICKSEARCH);
	gtk_container_add (GTK_CONTAINER(summaryview->toggle_search), pixmap);
	gtk_widget_show(pixmap);
	summaryview->quick_search_pixmap = pixmap;
	
#ifdef GENERIC_UMPC
	pixmap = stock_pixmap_widget(summaryview->hbox, STOCK_PIXMAP_SELECTION);
	gtk_container_add(GTK_CONTAINER(summaryview->multiple_sel_togbtn), pixmap);
	gtk_widget_show(pixmap);
	summaryview->multiple_sel_image = pixmap;
#endif

	/* Init summaryview prefs */
	summaryview->sort_key = SORT_BY_NONE;
	summaryview->sort_type = SORT_ASCENDING;

	/* Init summaryview extra data */
#ifndef G_OS_WIN32
	summaryview->simplify_subject_preg = NULL;
#endif
	summary_clear_list(summaryview);
	summary_set_column_titles(summaryview);
	summary_colorlabel_menu_create(summaryview, FALSE);
	summary_tags_menu_create(summaryview, FALSE);
	main_create_mailing_list_menu (summaryview->mainwin, NULL);	
	summary_set_menu_sensitive(summaryview);

}

#define CURRENTLY_DISPLAYED(m) \
( (m->msgnum == displayed_msgnum) \
  && (!g_ascii_strcasecmp(m->folder->name,item->name)) )

#define FOLDER_SHOWS_TO_HDR(i) \
( i && (folder_has_parent_of_type(i, F_OUTBOX) \
        || folder_has_parent_of_type(i, F_DRAFT) \
        || folder_has_parent_of_type(i, F_QUEUE)) )
  
static void summary_switch_from_to(SummaryView *summaryview, FolderItem *item)
{
	gboolean show_from = FALSE, show_to = FALSE;
	gboolean showing_from = FALSE, showing_to = FALSE;
	gint from_pos = 0, to_pos = 0;
	SummaryColumnState *col_state = summaryview->col_state;
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	
	if (!item || (prefs_common.layout_mode == VERTICAL_LAYOUT && prefs_common.two_line_vert) )
		return;
	if (FOLDER_SHOWS_TO_HDR(item))
		show_to = TRUE;
	else
		show_from = TRUE;
	
	from_pos = summaryview->col_pos[S_COL_FROM];
	to_pos = summaryview->col_pos[S_COL_TO];
	showing_from = col_state[from_pos].visible;
	showing_to = col_state[to_pos].visible;
	
	if (showing_from && showing_to) {
		debug_print("showing both\n");
		return;
	}

	if (!showing_from && !showing_to) {
		debug_print("showing none\n");
		return;
	}

	debug_print("showing %s %s, must show %s %s\n", 
		showing_from?"From":"",
		showing_to?"To":"",
		show_from?"From":"",
		show_to?"To":"");

	if (showing_from == show_from && showing_to == show_to)
		return;
	/* else we'll switch both */

	debug_print("switching columns\n");
	col_state[from_pos].type = S_COL_TO;
	col_state[from_pos].visible = show_to;

	col_state[to_pos].type = S_COL_FROM;
	col_state[to_pos].visible = show_from;

	summaryview->col_pos[S_COL_TO] = from_pos;
	summaryview->col_pos[S_COL_FROM] = to_pos;

	gtk_cmclist_set_column_visibility
		(GTK_CMCLIST(ctree), from_pos, col_state[from_pos].visible);
	gtk_cmclist_set_column_visibility
		(GTK_CMCLIST(ctree), to_pos, col_state[to_pos].visible);

	summary_set_column_titles(summaryview);
}

static void summaryview_reset_recursive_folder_match(SummaryView *summaryview)
{
	GSList *cur;

	for (cur = summaryview->recursive_matched_folders; cur != NULL; cur = cur->next) {
		folderview_update_search_icon(cur->data, FALSE);
	}

	g_slist_free(summaryview->recursive_matched_folders);
	summaryview->recursive_matched_folders = NULL;
	summaryview->search_root_folder = NULL;
}

static gboolean summaryview_quicksearch_recursive_progress(gpointer data, guint at, guint matched, guint total)
{
	QuickSearch *search = (QuickSearch*) data;
	gint interval = quicksearch_is_fast(search) ? 5000 : 100;

	statusbar_progress_all(at, total, interval);
	if (at % interval == 0)
		GTK_EVENTS_FLUSH();

	if (matched > 0)
		return FALSE;

	return TRUE;
}

static void summaryview_quicksearch_recurse_step(SummaryView *summaryview, FolderItem *item)
{
	MsgInfoList *msgs = NULL;
	gboolean result = TRUE;

	statusbar_print_all(_("Searching in %s... \n"),
		item->path ? item->path : "(null)");
	folder_item_update_freeze();

	quicksearch_set_on_progress_cb(summaryview->quicksearch, summaryview_quicksearch_recursive_progress, summaryview->quicksearch);
	if (!quicksearch_run_on_folder(summaryview->quicksearch, item, &msgs))
		result = FALSE;

	result = result && msgs != NULL;

	if (msgs != NULL)
		procmsg_msg_list_free(msgs);

	folder_item_update_thaw();
	statusbar_progress_all(0, 0, 0);
	statusbar_pop_all();

	if (result) {
		summaryview->recursive_matched_folders = g_slist_prepend(
				summaryview->recursive_matched_folders, item);
	
		folderview_update_search_icon(item, TRUE);
	}
}

static void summaryview_quicksearch_search_subfolders(SummaryView *summaryview, FolderItem *folder_item)
{
	FolderItem *cur = NULL;
	GNode *node = folder_item->node->children;

	if (!prefs_common.summary_quicksearch_recurse
			|| !quicksearch_has_sat_predicate(summaryview->quicksearch)
			|| quicksearch_is_in_typing(summaryview->quicksearch))
		return;

	for (; node != NULL; node = node->next) {
		if (!quicksearch_has_sat_predicate(summaryview->quicksearch))
			return;

		cur = FOLDER_ITEM(node->data);
		summaryview_quicksearch_recurse_step(summaryview, cur);
		if (cur->node->children)
			summaryview_quicksearch_search_subfolders(summaryview, cur);
	}
}

static void summaryview_quicksearch_recurse(SummaryView *summaryview)
{
	if (!prefs_common.summary_quicksearch_recurse
		|| !quicksearch_has_sat_predicate(summaryview->quicksearch)
		|| summaryview->folder_item == NULL) {
		return;
	}

	main_window_cursor_wait(summaryview->mainwin);

	summaryview_reset_recursive_folder_match(summaryview);
	summaryview->search_root_folder = summaryview->folder_item;

	summaryview_quicksearch_search_subfolders(summaryview, summaryview->folder_item);
	
	main_window_cursor_normal(summaryview->mainwin);
}

static gboolean summary_check_consistency(FolderItem *item, GSList *mlist)
{
	int u = 0, n = 0, m = 0, t = 0, r = 0, f = 0, l = 0, i = 0, w = 0;
	GSList *cur;
	START_TIMING("");
	for(cur = mlist ; cur != NULL && cur->data != NULL ; cur = g_slist_next(cur)) {
		MsgInfo * msginfo = (MsgInfo *) cur->data;
		t++;
		if (MSG_IS_NEW(msginfo->flags))
			n++;
		if (MSG_IS_UNREAD(msginfo->flags))
			u++;
		if (MSG_IS_MARKED(msginfo->flags))
			m++;		
		if (MSG_IS_REPLIED(msginfo->flags))
			r++;
		if (MSG_IS_FORWARDED(msginfo->flags))
			f++;
		if (MSG_IS_LOCKED(msginfo->flags))
			l++;
		if (MSG_IS_IGNORE_THREAD(msginfo->flags))
			i++;
		if (MSG_IS_WATCH_THREAD(msginfo->flags))
			w++;
	}
	if (t != item->total_msgs
	||  n != item->new_msgs
	||  u != item->unread_msgs
	||  m != item->marked_msgs
	||  r != item->replied_msgs
	||  f != item->forwarded_msgs
	||  l != item->locked_msgs
	||  i != item->ignored_msgs
	||  w != item->watched_msgs
	||  (m == 0 && item->unreadmarked_msgs != 0)
	||  item->unreadmarked_msgs < 0) {
		debug_print("Inconsistency\n");
		folder_item_scan_full(item, FALSE);
		END_TIMING();
		return FALSE;
	} 
	END_TIMING();
	return TRUE;
}

gboolean summaryview_search_root_progress(gpointer data, guint at, guint matched, guint total)
{
	SummaryView *summaryview = (SummaryView*) data;

	gint interval = quicksearch_is_fast(summaryview->quicksearch) ? 5000 : 100;
	
	statusbar_progress_all(at, total, interval);

	if (at % interval == 0)
		GTK_EVENTS_FLUSH();

	return TRUE;
}

gboolean summary_show(SummaryView *summaryview, FolderItem *item)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node = NULL;
	GSList *mlist = NULL;
	gchar *buf;
	gboolean is_refresh;
	guint selected_msgnum = 0;
	guint displayed_msgnum = 0;
	GSList *cur;
        GSList *not_killed;
	gboolean hidden_removed = FALSE;

	if (summary_is_locked(summaryview)) return FALSE;

	if (!summaryview->mainwin)
		return FALSE;
	START_TIMING("");
	summary_switch_from_to(summaryview, item);

	inc_lock();
	summary_lock(summaryview);
	
	menu_set_sensitive_all(GTK_MENU_SHELL(summaryview->popupmenu), TRUE);
	
	utils_free_regex();

	is_refresh = (item == summaryview->folder_item) ? TRUE : FALSE;

	if (item && item->folder->klass->item_opened) {
		item->folder->klass->item_opened(item);
	}

	if (!is_refresh) {
		main_create_mailing_list_menu (summaryview->mainwin, NULL);
		if (prefs_common.layout_mode == SMALL_LAYOUT) {
			if (item) {
				mainwindow_enter_folder(summaryview->mainwin);
				gtk_widget_grab_focus(summaryview->ctree);
			}
		}
	}
	if (!prefs_common.summary_quicksearch_sticky
	 && (!prefs_common.summary_quicksearch_recurse
	  || !quicksearch_has_sat_predicate(summaryview->quicksearch)
	  || (item && !folder_is_child_of(item, summaryview->search_root_folder)))
	 && !quicksearch_is_running(summaryview->quicksearch)
	 && !is_refresh) {
		quicksearch_set(summaryview->quicksearch, prefs_common.summary_quicksearch_type, "");
	}

	/* STATUSBAR_POP(summaryview->mainwin); */

	if (is_refresh) {
		selected_msgnum = summary_get_msgnum(summaryview,
						     summaryview->selected);
		displayed_msgnum = summary_get_msgnum(summaryview,
						      summaryview->displayed);
	}

	/* process the marks if any */
	if (summaryview->mainwin->lock_count == 0 &&
	    (summaryview->moved > 0 || summaryview->copied > 0)) {
		AlertValue val;
		gboolean changed = FALSE;

		val = alertpanel(_("Process mark"),
				 _("Some marks are left. Process them?"),
				 GTK_STOCK_NO, GTK_STOCK_YES, GTK_STOCK_CANCEL);
		if (G_ALERTALTERNATE == val) {
			summary_unlock(summaryview);
			summary_execute(summaryview);
			summary_lock(summaryview);
			changed = TRUE;
		} else if (G_ALERTDEFAULT == val) {
			/* DO NOTHING */
		} else {
			summary_unlock(summaryview);
			inc_unlock();
			END_TIMING();
			return FALSE;
		}
		if (changed || !quicksearch_has_sat_predicate(summaryview->quicksearch))
			folder_update_op_count();
	}
	
	summary_freeze(summaryview);

	summary_clear_list(summaryview);

	buf = NULL;
	if (!item || !item->path || !folder_item_parent(item) || item->no_select) {
		g_free(buf);
		debug_print("empty folder (%p %s %p %d)\n\n",
				item, 
				(item && item->path)?item->path:"(null)",
				item?folder_item_parent(item):0x0,
				item?item->no_select:FALSE);
		summary_set_hide_read_msgs_menu(summaryview, FALSE);
		summary_set_hide_del_msgs_menu(summaryview, FALSE);
		summary_set_hide_read_threads_menu(summaryview, FALSE);
		summary_clear_all(summaryview);
		summaryview->folder_item = item;
		summary_thaw(summaryview);
		summary_unlock(summaryview);
		inc_unlock();
		END_TIMING();
		return TRUE;
	}
	g_free(buf);

	if (!is_refresh)
		messageview_clear(summaryview->messageview);

	summaryview->folder_item = item;
	item->opened = TRUE;

	buf = g_strdup_printf(_("Scanning folder (%s)..."), item->path);
	debug_print("%s\n", buf);
	STATUSBAR_PUSH(summaryview->mainwin, buf);
	g_free(buf);

	main_window_cursor_wait(summaryview->mainwin);

	mlist = folder_item_get_msg_list(item);

	if (!summary_check_consistency(item, mlist)) {
		debug_print("reloading due to inconsistency\n");
		procmsg_msg_list_free(mlist);
		mlist = folder_item_get_msg_list(item);
	}

	if (quicksearch_has_sat_predicate(summaryview->quicksearch)) {
		procmsg_msg_list_free(mlist);
		mlist = NULL;

		START_TIMING("quicksearch");

		statusbar_print_all(_("Searching in %s... \n"), 
			summaryview->folder_item->path ? 
			summaryview->folder_item->path : "(null)");

		folder_item_update_freeze();

		quicksearch_set_on_progress_cb(summaryview->quicksearch, summaryview_search_root_progress, summaryview);
		quicksearch_run_on_folder(summaryview->quicksearch, summaryview->folder_item, &mlist);

		folder_item_update_thaw();
		statusbar_progress_all(0, 0, 0);
		statusbar_pop_all();

		if (!quicksearch_has_sat_predicate(summaryview->quicksearch)) {
			debug_print("search cancelled!\n");
			printf("search cancelled!\n");
			summary_thaw(summaryview);
			STATUSBAR_POP(summaryview->mainwin);
			main_window_cursor_normal(summaryview->mainwin);
			summary_unlock(summaryview);
			inc_unlock();
			summary_show(summaryview, summaryview->folder_item);
			END_TIMING();
			return FALSE;
		}
		END_TIMING();
	}

	if ((summaryview->folder_item->hide_read_msgs
             || summaryview->folder_item->hide_del_msgs
             || summaryview->folder_item->hide_read_threads) &&
	    quicksearch_has_sat_predicate(summaryview->quicksearch) == FALSE) {
		GSList *not_killed;
		
		summary_set_hide_read_msgs_menu(summaryview, summaryview->folder_item->hide_read_msgs);
		summary_set_hide_del_msgs_menu(summaryview, summaryview->folder_item->hide_del_msgs);
		summary_set_hide_read_threads_menu(summaryview, summaryview->folder_item->hide_read_threads);
		not_killed = NULL;
		for(cur = mlist ; cur != NULL && cur->data != NULL ; cur = g_slist_next(cur)) {
			MsgInfo * msginfo = (MsgInfo *) cur->data;
			
			if (!msginfo->hidden) {
				if (MSG_IS_DELETED(msginfo->flags) && summaryview->folder_item->hide_del_msgs) {
					procmsg_msginfo_free(msginfo);
					continue;
				}
				if (summaryview->folder_item->hide_read_msgs) {
					if (MSG_IS_UNREAD(msginfo->flags) &&
					    !MSG_IS_IGNORE_THREAD(msginfo->flags))
						not_killed = g_slist_prepend(not_killed, msginfo);
					else if (MSG_IS_MARKED(msginfo->flags) ||
						 MSG_IS_LOCKED(msginfo->flags))
						not_killed = g_slist_prepend(not_killed, msginfo);
					else if (is_refresh &&
						(msginfo->msgnum == selected_msgnum ||
						 msginfo->msgnum == displayed_msgnum))
						not_killed = g_slist_prepend(not_killed, msginfo);
					else
						procmsg_msginfo_free(msginfo);
				} else {
					not_killed = g_slist_prepend(not_killed, msginfo);
				}
			 } else
			 	procmsg_msginfo_free(msginfo);
		}
		hidden_removed = TRUE;
		g_slist_free(mlist);
		mlist = not_killed;
	} else {
		summary_set_hide_read_msgs_menu(summaryview, FALSE);
		summary_set_hide_del_msgs_menu(summaryview, FALSE);
		summary_set_hide_read_threads_menu(summaryview, FALSE);
	}

	if (!hidden_removed) {
		START_TIMING("removing hidden");
        	not_killed = NULL;
        	for(cur = mlist ; cur != NULL && cur->data != NULL ; cur = g_slist_next(cur)) {
                	MsgInfo * msginfo = (MsgInfo *) cur->data;

                	if (!msginfo->hidden)
                        	not_killed = g_slist_prepend(not_killed, msginfo);
                	else
                        	procmsg_msginfo_free(msginfo);
        	}
		g_slist_free(mlist);
		mlist = not_killed;
		END_TIMING();
	}

	STATUSBAR_POP(summaryview->mainwin);

	/* set ctree and hash table from the msginfo list, and
	   create the thread */
	summary_set_ctree_from_list(summaryview, mlist);

	g_slist_free(mlist);

	if (is_refresh) {
		if (!quicksearch_is_in_typing(summaryview->quicksearch)) {
			summaryview->displayed =
				summary_find_msg_by_msgnum(summaryview,
							   displayed_msgnum);
			if (!summaryview->displayed)
				messageview_clear(summaryview->messageview);
			summary_unlock(summaryview);
			summary_select_by_msgnum(summaryview, selected_msgnum);
			summary_lock(summaryview);
			if (!summaryview->selected) {
				/* no selected message - select first unread
				   message, but do not display it */
				node = summary_find_next_flagged_msg(summaryview, NULL,
								     MSG_UNREAD, FALSE);
				if (node == NULL && GTK_CMCLIST(ctree)->row_list != NULL)
					node = gtk_cmctree_node_nth
						(ctree,
						 item->sort_type == SORT_DESCENDING
						 ? 0 : GTK_CMCLIST(ctree)->rows - 1);
				summary_unlock(summaryview);
				summary_select_node(summaryview, node, FALSE, TRUE);
				summary_lock(summaryview);
			}
		} else {
			/* just select first/last */
			if (GTK_CMCLIST(ctree)->row_list != NULL)
				node = gtk_cmctree_node_nth
					(ctree,
					 item->sort_type == SORT_DESCENDING
					 ? 0 : GTK_CMCLIST(ctree)->rows - 1);
			gtk_sctree_select(GTK_SCTREE(ctree), node);
			summaryview->selected = node;
			gtk_cmctree_node_moveto(ctree, node, 0, 0.5, 0);
		}
	} else {
		/* backward compat */
		int i = 0;
		gboolean set = FALSE, stop = FALSE;
		for (i = 0; i < 6; i++) {
			EntryAction act = prefs_common.summary_select_prio[i];

			if (act != ACTION_UNSET) {
				set = TRUE;
				break;
			}
		}
		if (!set)
			prefs_summary_open_set_defaults();

 		for (i = 0; i < 6 && node == NULL; i++) {
			EntryAction act = prefs_common.summary_select_prio[i];
			
			switch(act) {
			case ACTION_MARKED:
				node = summary_find_next_flagged_msg(summaryview, NULL,
					     MSG_MARKED, FALSE);
				break;
			case ACTION_NEW:
				node = summary_find_next_flagged_msg(summaryview, NULL,
					     MSG_NEW, FALSE);
				break;
			case ACTION_UNREAD:
				node = summary_find_next_flagged_msg(summaryview, NULL,
					     MSG_UNREAD, FALSE);
				break;
			case ACTION_LAST_OPENED:
				if (summaryview->folder_item) {
					node = summary_find_msg_by_msgnum(summaryview, 
							summaryview->folder_item->last_seen);
				}
				break;
			case ACTION_LAST_LIST:
				if (GTK_CMCLIST(ctree)->row_list != NULL) {
					node = gtk_cmctree_node_nth
						(ctree,
						 item->sort_type == SORT_DESCENDING
						 ? 0 : GTK_CMCLIST(ctree)->rows - 1);
				}
				break;
			case ACTION_FIRST_LIST:
				if (GTK_CMCLIST(ctree)->row_list != NULL) {
					node = gtk_cmctree_node_nth
						(ctree,
						 item->sort_type == SORT_ASCENDING
						 ? 0 : GTK_CMCLIST(ctree)->rows - 1);
				}
				break;
			case ACTION_NOTHING:
			case ACTION_UNSET:
				node = NULL;
				stop = TRUE;
				break;
			}
			
			if (stop || node)
				break;
		}

		summary_unlock(summaryview);
		if (node) {
			gboolean show = (prefs_common.always_show_msg == OPENMSG_ALWAYS) ||
				(prefs_common.always_show_msg == OPENMSG_WHEN_VIEW_VISIBLE &&
						messageview_is_visible(summaryview->messageview));
			summary_select_node(summaryview, node, show, TRUE);
		}
		summary_lock(summaryview);
	}

	summary_status_show(summaryview);
	summary_set_menu_sensitive(summaryview);
	toolbar_main_set_sensitive(summaryview->mainwin);
	
	summary_thaw(summaryview);
	debug_print("\n");
	STATUSBAR_PUSH(summaryview->mainwin, _("Done."));
	STATUSBAR_POP(summaryview->mainwin);
	main_window_cursor_normal(summaryview->mainwin);
	summary_unlock(summaryview);
	inc_unlock();
	END_TIMING();
	return TRUE;
}

#undef CURRENTLY_DISPLAYED


void summary_clear_list(SummaryView *summaryview)
{
	GtkCMCList *clist = GTK_CMCLIST(summaryview->ctree);
	gint optimal_width;

	summary_freeze(summaryview);

	gtk_cmctree_pre_recursive(GTK_CMCTREE(summaryview->ctree),
				NULL, summary_free_msginfo_func, NULL);

	if (summaryview->folder_item) {
		summaryview->folder_item->opened = FALSE;
		summaryview->folder_item = NULL;
	}

	summaryview->display_msg = FALSE;

	summaryview->selected = NULL;
	summaryview->displayed = NULL;
	summaryview->total_size = 0;
	summaryview->deleted = summaryview->moved = 0;
	summaryview->copied = 0;
	if (summaryview->msgid_table) {
		g_hash_table_destroy(summaryview->msgid_table);
		summaryview->msgid_table = NULL;
	}
	if (summaryview->subject_table) {
		g_hash_table_destroy(summaryview->subject_table);
		summaryview->subject_table = NULL;
	}
	summaryview->mlist = NULL;

	gtk_cmclist_clear(clist);
	if (summaryview->col_pos[S_COL_SUBJECT] == N_SUMMARY_COLS - 1) {
		optimal_width = gtk_cmclist_optimal_column_width
			(clist, summaryview->col_pos[S_COL_SUBJECT]);
		gtk_cmclist_set_column_width
			(clist, summaryview->col_pos[S_COL_SUBJECT],
			 optimal_width);
	}

	summary_thaw(summaryview);
}

void summary_clear_all(SummaryView *summaryview)
{
	messageview_clear(summaryview->messageview);
	summary_clear_list(summaryview);
	summary_set_menu_sensitive(summaryview);
	toolbar_main_set_sensitive(summaryview->mainwin);
	summary_status_show(summaryview);
}

void summary_lock(SummaryView *summaryview)
{
	summaryview->lock_count++;
}

void summary_unlock(SummaryView *summaryview)
{
	if (summaryview->lock_count)
		summaryview->lock_count--;
}

gboolean summary_is_locked(SummaryView *summaryview)
{
	return summaryview->lock_count > 0;
}

SummarySelection summary_get_selection_type(SummaryView *summaryview)
{
	GtkCMCList *clist = GTK_CMCLIST(summaryview->ctree);
	SummarySelection selection;

	if (!clist->row_list)
		selection = SUMMARY_NONE;
	else if (!clist->selection)
		selection = SUMMARY_SELECTED_NONE;
	else if (!clist->selection->next)
		selection = SUMMARY_SELECTED_SINGLE;
	else
		selection = SUMMARY_SELECTED_MULTIPLE;

	return selection;
}

/*!
 *\return	MsgInfo	* Selected message if there's one selected;
 *		if multiple selected, or none, return NULL.
 */ 
MsgInfo *summary_get_selected_msg(SummaryView *summaryview)
{
	/* summaryview->selected may be valid when multiple 
	 * messages were selected */
	GList *sellist = GTK_CMCLIST(summaryview->ctree)->selection;

	if (sellist == NULL || sellist->next) 
		return NULL;
	
	return GTKUT_CTREE_NODE_GET_ROW_DATA(sellist->data);
}

GSList *summary_get_selected_msg_list(SummaryView *summaryview)
{
	GSList *mlist = NULL;
	GList *cur;
	MsgInfo *msginfo;

	for (cur = GTK_CMCLIST(summaryview->ctree)->selection; cur != NULL && cur->data != NULL;
	     cur = cur->next) {
		msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(cur->data);
		mlist = g_slist_prepend(mlist, msginfo);
	}

	mlist = g_slist_reverse(mlist);

	return mlist;
}

void summary_set_menu_sensitive(SummaryView *summaryview)
{
	SensitiveCond state;
	gboolean sensitive;
	gint i;

	static const struct {
		gchar *const entry;
		SensitiveCond cond;
	} entry[] = {
		{"Menus/SummaryViewPopup/Reply"			, M_HAVE_ACCOUNT|M_TARGET_EXIST},
#ifndef GENERIC_UMPC
		{"Menus/SummaryViewPopup/ReplyTo"			, M_HAVE_ACCOUNT|M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/ReplyTo/All"		, M_HAVE_ACCOUNT|M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/ReplyTo/Sender"             , M_HAVE_ACCOUNT|M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/ReplyTo/MailingList"       , M_HAVE_ACCOUNT|M_TARGET_EXIST},
#endif

		{"Menus/SummaryViewPopup/Forward"			, M_HAVE_ACCOUNT|M_TARGET_EXIST},
#ifndef GENERIC_UMPC
		{"Menus/SummaryViewPopup/ForwardAtt"	, M_HAVE_ACCOUNT|M_TARGET_EXIST},
        	{"Menus/SummaryViewPopup/Redirect"			, M_HAVE_ACCOUNT|M_TARGET_EXIST},
#endif

		{"Menus/SummaryViewPopup/Move"			, M_TARGET_EXIST|M_ALLOW_DELETE|M_NOT_NEWS},
		{"Menus/SummaryViewPopup/Copy"			, M_TARGET_EXIST|M_EXEC},
		{"Menus/SummaryViewPopup/Trash"		, M_TARGET_EXIST|M_ALLOW_DELETE|M_NOT_NEWS|M_NOT_TRASH},
#ifndef GENERIC_UMPC
		{"Menus/SummaryViewPopup/Delete"			, M_TARGET_EXIST|M_ALLOW_DELETE},
#endif

		{"Menus/SummaryViewPopup/Mark"			, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/Mark"   		, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/Unmark"   		, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/MarkUnread"   	, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/MarkRead"   	, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/MarkAllRead"   	, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/IgnoreThread"   	, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/UnignoreThread"   	, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/WatchThread"   		, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/UnwatchThread"   	, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/Unlock"   		, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/Lock"   		, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Mark/MarkSpam"	  	, M_TARGET_EXIST|M_CAN_LEARN_SPAM},
		{"Menus/SummaryViewPopup/Mark/MarkHam" 		, M_TARGET_EXIST|M_CAN_LEARN_SPAM},
		{"Menus/SummaryViewPopup/ColorLabel"			, M_TARGET_EXIST},
		{"Menus/SummaryViewPopup/Tags"			, M_TARGET_EXIST},

#ifndef GENERIC_UMPC
		{"Menus/SummaryViewPopup/AddSenderToAB"	, M_SINGLE_TARGET_EXIST},
#endif
		{"Menus/SummaryViewPopup/CreateFilterRule"		, M_SINGLE_TARGET_EXIST|M_UNLOCKED},
#ifndef GENERIC_UMPC
		{"Menus/SummaryViewPopup/CreateProcessingRule"	, M_SINGLE_TARGET_EXIST|M_UNLOCKED},
#endif

		{"Menus/SummaryViewPopup/View"			, M_SINGLE_TARGET_EXIST},
		{"Menus/SummaryViewPopup/View/OpenNewWindow"     , M_SINGLE_TARGET_EXIST},
		{"Menus/SummaryViewPopup/View/MessageSource"		, M_SINGLE_TARGET_EXIST},
#ifndef GENERIC_UMPC
		{"Menus/SummaryViewPopup/View/AllHeaders"		, M_SINGLE_TARGET_EXIST},
#endif
		{"Menus/SummaryViewPopup/SaveAs"			, M_TARGET_EXIST},
#ifndef GENERIC_UMPC
		{"Menus/SummaryViewPopup/Print"			, M_TARGET_EXIST},
#endif
		{NULL, 0}
	};

	main_window_set_menu_sensitive(summaryview->mainwin);

	state = main_window_get_current_state(summaryview->mainwin);

	for (i = 0; entry[i].entry != NULL; i++) {
		sensitive = ((entry[i].cond & state) == entry[i].cond);
		cm_menu_set_sensitive_full(summaryview->mainwin->ui_manager, entry[i].entry, sensitive);
	}

	summary_lock(summaryview);
#ifndef GENERIC_UMPC
	if (summaryview->messageview 
	&&  summaryview->messageview->mimeview
	&&  summaryview->messageview->mimeview->textview)
		cm_toggle_menu_set_active_full(summaryview->mainwin->ui_manager, "Menus/SummaryViewPopup/View/AllHeaders",
			prefs_common.show_all_headers);
#endif
	summary_unlock(summaryview);
}

void summary_select_prev_unread(SummaryView *summaryview)
{
	GtkCMCTreeNode *node;
	gboolean skip_cur = FALSE;

	if (summaryview->displayed 
	&&  summaryview->selected == summaryview->displayed) {
		debug_print("skipping current\n");
		skip_cur = TRUE;
	}

	node = summary_find_prev_flagged_msg
		(summaryview, summaryview->selected, MSG_UNREAD, skip_cur);

	if (!node || node == summaryview->selected) {
		AlertValue val = 0;

 		switch (prefs_common.next_unread_msg_dialog) {
 			case NEXTUNREADMSGDIALOG_ALWAYS:
				val = alertpanel(_("No more unread messages"),
						 _("No unread message found. "
						   "Search from the end?"),
						 GTK_STOCK_NO, "+"GTK_STOCK_YES, NULL);
 				break;
 			case NEXTUNREADMSGDIALOG_ASSUME_YES:
 				val = G_ALERTALTERNATE;
 				break;
 			case NEXTUNREADMSGDIALOG_ASSUME_NO:
 				val = !G_ALERTALTERNATE;
 				break;
 			default:
 				debug_print(
 					_("Internal error: unexpected value for prefs_common.next_unread_msg_dialog\n"));
 		}
		if (val != G_ALERTALTERNATE) return;
		node = summary_find_prev_flagged_msg(summaryview, NULL,
						     MSG_UNREAD, FALSE);
	}

	if (!node)
		alertpanel_notice(_("No unread messages."));
	else
		summary_select_node(summaryview, node, TRUE, FALSE);
}

void summary_select_next_unread(SummaryView *summaryview)
{
	GtkCMCTreeNode *node = summaryview->selected;
	gboolean skip_cur = FALSE;
	
	if (summaryview->displayed 
	&&  summaryview->selected == summaryview->displayed) {
		debug_print("skipping cur (%p %p)\n",
			summaryview->displayed, summaryview->selected);
		skip_cur = TRUE;
	}


	node = summary_find_next_flagged_msg
		(summaryview, node, MSG_UNREAD, skip_cur);
	
	if (node)
		summary_select_node(summaryview, node, TRUE, FALSE);
	else {
		node = summary_find_next_flagged_msg
			(summaryview, NULL, MSG_UNREAD, FALSE);
		if (node == NULL || node == summaryview->selected) {
			AlertValue val = 0;

 			switch (prefs_common.next_unread_msg_dialog) {
 				case NEXTUNREADMSGDIALOG_ALWAYS:
					val = alertpanel(_("No more unread messages"),
							 _("No unread message found. "
							   "Go to next folder?"),
							 GTK_STOCK_NO, "+"GTK_STOCK_YES, NULL);
 					break;
 				case NEXTUNREADMSGDIALOG_ASSUME_YES:
 					val = G_ALERTALTERNATE;
 					break;
 				case NEXTUNREADMSGDIALOG_ASSUME_NO:
 					val = G_ALERTOTHER;
 					break;
 				default:
 					debug_print(
 						_("Internal error: unexpected value for prefs_common.next_unread_msg_dialog\n"));
 			}

			if (val == G_ALERTALTERNATE) {
				folderview_select_next_unread(summaryview->folderview, TRUE);
				return;
			} 
			else
				return;
		} else
			summary_select_node(summaryview, node, TRUE, FALSE);

	}
}

void summary_select_prev_new(SummaryView *summaryview)
{
	GtkCMCTreeNode *node;
	gboolean skip_cur = FALSE;

	if (summaryview->displayed 
	&&  summaryview->selected == summaryview->displayed) {
		debug_print("skipping current\n");
		skip_cur = TRUE;
	}

	node = summary_find_prev_flagged_msg
		(summaryview, summaryview->selected, MSG_NEW, skip_cur);

	if (!node || node == summaryview->selected) {
		AlertValue val = 0;

 		switch (prefs_common.next_unread_msg_dialog) {
 			case NEXTUNREADMSGDIALOG_ALWAYS:
				val = alertpanel(_("No more new messages"),
						 _("No new message found. "
						   "Search from the end?"),
						 GTK_STOCK_NO, "+"GTK_STOCK_YES, NULL);
 				break;
 			case NEXTUNREADMSGDIALOG_ASSUME_YES:
 				val = G_ALERTALTERNATE;
 				break;
 			case NEXTUNREADMSGDIALOG_ASSUME_NO:
 				val = !G_ALERTALTERNATE;
 				break;
 			default:
 				debug_print(
 					_("Internal error: unexpected value for prefs_common.next_unread_msg_dialog\n"));
 		}
		if (val != G_ALERTALTERNATE) return;
		node = summary_find_prev_flagged_msg(summaryview, NULL,
						     MSG_NEW, FALSE);
	}

	if (!node)
		alertpanel_notice(_("No new messages."));
	else
		summary_select_node(summaryview, node, TRUE, FALSE);
}

void summary_select_next_new(SummaryView *summaryview)
{
	GtkCMCTreeNode *node = summaryview->selected;
	gboolean skip_cur = FALSE;
	
	if (summaryview->displayed 
	&&  summaryview->selected == summaryview->displayed) {
		debug_print("skipping cur (%p %p)\n",
			summaryview->displayed, summaryview->selected);
		skip_cur = TRUE;
	}


	node = summary_find_next_flagged_msg
		(summaryview, node, MSG_NEW, skip_cur);
	
	if (node)
		summary_select_node(summaryview, node, TRUE, FALSE);
	else {
		node = summary_find_next_flagged_msg
			(summaryview, NULL, MSG_NEW, FALSE);
		if (node == NULL || node == summaryview->selected) {
			AlertValue val = 0;

 			switch (prefs_common.next_unread_msg_dialog) {
 				case NEXTUNREADMSGDIALOG_ALWAYS:
					val = alertpanel(_("No more new messages"),
							 _("No new message found. "
							   "Go to next folder?"),
							 GTK_STOCK_NO, "+"GTK_STOCK_YES, NULL);
 					break;
 				case NEXTUNREADMSGDIALOG_ASSUME_YES:
 					val = G_ALERTALTERNATE;
 					break;
 				case NEXTUNREADMSGDIALOG_ASSUME_NO:
 					val = G_ALERTOTHER;
 					break;
 				default:
 					debug_print(
 						_("Internal error: unexpected value for prefs_common.next_unread_msg_dialog\n"));
 			}

			if (val == G_ALERTALTERNATE) {
				folderview_select_next_new(summaryview->folderview);
				return;
			} 
			else
				return;
		} else
			summary_select_node(summaryview, node, TRUE, FALSE);

	}
}

void summary_select_prev_marked(SummaryView *summaryview)
{
	GtkCMCTreeNode *node;

	node = summary_find_prev_flagged_msg
		(summaryview, summaryview->selected, MSG_MARKED, TRUE);

	if (!node) {
		AlertValue val;

		val = alertpanel(_("No more marked messages"),
				 _("No marked message found. "
				   "Search from the end?"),
				 GTK_STOCK_NO, "+"GTK_STOCK_YES, NULL);
		if (val != G_ALERTALTERNATE) return;
		node = summary_find_prev_flagged_msg(summaryview, NULL,
						     MSG_MARKED, TRUE);
	}

	if (!node)
		alertpanel_notice(_("No marked messages."));
	else
		summary_select_node(summaryview, node, TRUE, FALSE);
}

void summary_select_next_marked(SummaryView *summaryview)
{
	GtkCMCTreeNode *node = summaryview->selected;
	gboolean skip_cur = FALSE;
	
	if (summaryview->displayed 
	&&  summaryview->selected == summaryview->displayed) {
		debug_print("skipping cur (%p %p)\n",
			summaryview->displayed, summaryview->selected);
		skip_cur = TRUE;
	}


	node = summary_find_next_flagged_msg
		(summaryview, node, MSG_MARKED, skip_cur);
	
	if (node)
		summary_select_node(summaryview, node, TRUE, FALSE);
	else {
		node = summary_find_next_flagged_msg
			(summaryview, NULL, MSG_MARKED, FALSE);
		if (node == NULL || node == summaryview->selected) {
			AlertValue val = 0;

 			switch (prefs_common.next_unread_msg_dialog) {
 				case NEXTUNREADMSGDIALOG_ALWAYS:
					val = alertpanel(_("No more marked messages"),
							 _("No marked message found. "
							   "Go to next folder?"),
							 GTK_STOCK_NO, "+"GTK_STOCK_YES, NULL);
 					break;
 				case NEXTUNREADMSGDIALOG_ASSUME_YES:
 					val = G_ALERTALTERNATE;
 					break;
 				case NEXTUNREADMSGDIALOG_ASSUME_NO:
 					val = G_ALERTOTHER;
 					break;
 				default:
 					debug_print(
 						_("Internal error: unexpected value for prefs_common.next_unread_msg_dialog\n"));
 			}

			if (val == G_ALERTALTERNATE) {
				folderview_select_next_marked(summaryview->folderview);
				return;
			} 
			else
				return;
		} else
			summary_select_node(summaryview, node, TRUE, FALSE);

	}
}

void summary_select_prev_labeled(SummaryView *summaryview)
{
	GtkCMCTreeNode *node;

	node = summary_find_prev_flagged_msg
		(summaryview, summaryview->selected, MSG_CLABEL_FLAG_MASK, TRUE);

	if (!node) {
		AlertValue val;

		val = alertpanel(_("No more labeled messages"),
				 _("No labeled message found. "
				   "Search from the end?"),
				 GTK_STOCK_NO, "+"GTK_STOCK_YES, NULL);
		if (val != G_ALERTALTERNATE) return;
		node = summary_find_prev_flagged_msg(summaryview, NULL,
						     MSG_CLABEL_FLAG_MASK, TRUE);
	}

	if (!node)
		alertpanel_notice(_("No labeled messages."));
	else
		summary_select_node(summaryview, node, TRUE, FALSE);
}

void summary_select_next_labeled(SummaryView *summaryview)
{
	GtkCMCTreeNode *node;

	node = summary_find_next_flagged_msg
		(summaryview, summaryview->selected, MSG_CLABEL_FLAG_MASK, TRUE);

	if (!node) {
		AlertValue val;

		val = alertpanel(_("No more labeled messages"),
				 _("No labeled message found. "
				   "Search from the beginning?"),
				 GTK_STOCK_NO, "+"GTK_STOCK_YES, NULL);
		if (val != G_ALERTALTERNATE) return;
		node = summary_find_next_flagged_msg(summaryview, NULL,
						     MSG_CLABEL_FLAG_MASK, TRUE);
	}

	if (!node)
		alertpanel_notice(_("No labeled messages."));
	else
		summary_select_node(summaryview, node, TRUE, FALSE);
}

void summary_select_parent(SummaryView *summaryview)
{
	GtkCMCTreeNode *node = NULL;

	if (summaryview->selected)
		node = GTK_CMCTREE_ROW(summaryview->selected)->parent;
	if (node)
		summary_select_node(summaryview, node, TRUE, FALSE);
}

void summary_select_by_msgnum(SummaryView *summaryview, guint msgnum)
{
	GtkCMCTreeNode *node;

	node = summary_find_msg_by_msgnum(summaryview, msgnum);
	summary_select_node(summaryview, node, FALSE, TRUE);
}

void summary_display_by_msgnum(SummaryView *summaryview, guint msgnum)
{
	GtkCMCTreeNode *node;

	node = summary_find_msg_by_msgnum(summaryview, msgnum);
	summary_select_node(summaryview, node, TRUE, FALSE);
}

void summary_select_by_msg_list(SummaryView	*summaryview, GSList *msginfos)
{
	GtkCMCTree *ctree;
	GSList *msgnum_list, *walk;
	gboolean froze = FALSE;

	ctree = GTK_CMCTREE(summaryview->ctree);

	msgnum_list = procmsg_get_number_list_for_msgs(msginfos);

	START_LONG_OPERATION(summaryview, FALSE);
	for(walk = msgnum_list; walk; walk = walk->next) {
		GtkCMCTreeNode *node;
		node = summary_find_msg_by_msgnum(summaryview, GPOINTER_TO_UINT(walk->data));
		gtk_cmctree_select(ctree, node);
	}
	END_LONG_OPERATION(summaryview);
	g_slist_free(msgnum_list);
}

typedef struct _PostponedSelectData
{
	GtkCMCTree *ctree;
	GtkCMCTreeNode *row;
	GtkCMCTreeNode *node;
	GtkScrollType type;
	gint column;
	SummaryView *summaryview;
	gboolean display_msg;
	gboolean do_refresh;
} PostponedSelectData;

static gboolean summary_select_retry(void *data)
{
	PostponedSelectData *psdata = (PostponedSelectData *)data;
	debug_print("trying again\n");
	if (psdata->row)
		summary_selected(psdata->ctree, psdata->row,
			    psdata->column, psdata->summaryview);
	else if (psdata->node)
		summary_select_node(psdata->summaryview, psdata->node,
			    psdata->display_msg, psdata->do_refresh);
	else
		summary_step(psdata->summaryview, psdata->type);
	g_free(psdata);
	return FALSE;
}

/**
 * summary_select_node:
 * @summaryview: Summary view.
 * @node: Summary tree node.
 * @display_msg: TRUE to display the selected message.
 * @do_refresh: TRUE to refresh the widget.
 *
 * Select @node (bringing it into view by scrolling and expanding its
 * thread, if necessary) and unselect all others.  If @display_msg is
 * TRUE, display the corresponding message in the message view.
 * If @do_refresh is TRUE, the widget is refreshed.
 **/
void summary_select_node(SummaryView *summaryview, GtkCMCTreeNode *node,
			 gboolean display_msg, gboolean do_refresh)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	
	if (summary_is_locked(summaryview)
	&& !GTK_SCTREE(ctree)->selecting_range
	&& summaryview->messageview->mimeview
	&& summaryview->messageview->mimeview->type == MIMEVIEW_TEXT
	&& summaryview->messageview->mimeview->textview->loading) {
		PostponedSelectData *data = g_new0(PostponedSelectData, 1);
		summaryview->messageview->mimeview->textview->stop_loading = TRUE;
		
		data->ctree = ctree;
		data->row = NULL;
		data->node = node;
		data->summaryview = summaryview;
		data->display_msg = display_msg;
		data->do_refresh = do_refresh;
		debug_print("postponing open of message till end of load\n");
		g_timeout_add(100, summary_select_retry, data);
		return;
	}
	if (summary_is_locked(summaryview)) {
		return;
	}
	if (!summaryview->folder_item)
		return;
	if (node) {
		gtkut_ctree_expand_parent_all(ctree, node);
		if (do_refresh) {
			summary_lock(summaryview);
			GTK_EVENTS_FLUSH();
			summary_unlock(summaryview);
			gtk_widget_grab_focus(GTK_WIDGET(ctree));
			gtk_cmctree_node_moveto(ctree, node, 0, 0.5, 0);
		}
		if (display_msg && summaryview->displayed == node)
			summaryview->displayed = NULL;
		summaryview->display_msg = display_msg;
		gtk_sctree_select(GTK_SCTREE(ctree), node);
		if (summaryview->selected == NULL)
			summaryview->selected = node;
	}
}

guint summary_get_msgnum(SummaryView *summaryview, GtkCMCTreeNode *node)
{
	GtkCMCTree *ctree =NULL;
	MsgInfo *msginfo;

	if (!summaryview)
		return 0;
	ctree = GTK_CMCTREE(summaryview->ctree);
	if (!node)
		return 0;
	msginfo = gtk_cmctree_node_get_row_data(ctree, node);
	if (msginfo)
		return msginfo->msgnum;
	else 
		return -1;
}

static GtkCMCTreeNode *summary_find_prev_msg(SummaryView *summaryview,
					   GtkCMCTreeNode *current_node)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	MsgInfo *msginfo;

	if (current_node)
		node = current_node;
	else
		node = gtk_cmctree_node_nth(ctree, GTK_CMCLIST(ctree)->rows - 1);

	for (; node != NULL; node = GTK_CMCTREE_NODE_PREV(node)) {
		msginfo = gtk_cmctree_node_get_row_data(ctree, node);
		if (msginfo && !MSG_IS_DELETED(msginfo->flags)) break;
	}

	return node;
}

static GtkCMCTreeNode *summary_find_next_msg(SummaryView *summaryview,
					   GtkCMCTreeNode *current_node)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	MsgInfo *msginfo;

	if (current_node)
		node = current_node;
	else
		node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);

	for (; node != NULL; node = gtkut_ctree_node_next(ctree, node)) {
		msginfo = gtk_cmctree_node_get_row_data(ctree, node);
		if (msginfo && !MSG_IS_DELETED(msginfo->flags) 
		&& !MSG_IS_MOVE(msginfo->flags)) break;
	}

	return node;
}

static GtkCMCTreeNode *summary_find_prev_flagged_msg(SummaryView *summaryview,
						   GtkCMCTreeNode *current_node,
						   MsgPermFlags flags,
						   gboolean start_from_prev)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	MsgInfo *msginfo;

	if (current_node) {
		if (start_from_prev)
			node = GTK_CMCTREE_NODE_PREV(current_node);
		else
			node = current_node;
	} else
		node = gtk_cmctree_node_nth(ctree, GTK_CMCLIST(ctree)->rows - 1);

	for (; node != NULL; node = GTK_CMCTREE_NODE_PREV(node)) {
		msginfo = gtk_cmctree_node_get_row_data(ctree, node);
		if (msginfo && (msginfo->flags.perm_flags & flags) != 0) break;
	}

	return node;
}

static GtkCMCTreeNode *summary_find_next_flagged_msg(SummaryView *summaryview,
						   GtkCMCTreeNode *current_node,
						   MsgPermFlags flags,
						   gboolean start_from_next)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	MsgInfo *msginfo;

	if (current_node) {
		if (start_from_next)
			node = gtkut_ctree_node_next(ctree, current_node);
		else
			node = current_node;
	} else
		node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);

	for (; node != NULL; node = gtkut_ctree_node_next(ctree, node)) {
		msginfo = gtk_cmctree_node_get_row_data(ctree, node);
		/* Find msg with matching flags but ignore messages with
		   ignore flags, if searching for new or unread messages */
		if ((msginfo && (msginfo->flags.perm_flags & flags) != 0) &&
		    !(((flags & (MSG_NEW | MSG_UNREAD)) != 0) && MSG_IS_IGNORE_THREAD(msginfo->flags)) 
			)
			break;
	}

	return node;
}

static GtkCMCTreeNode *summary_find_msg_by_msgnum(SummaryView *summaryview,
						guint msgnum)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	MsgInfo *msginfo;

	node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);

	for (; node != NULL; node = gtkut_ctree_node_next(ctree, node)) {
		msginfo = gtk_cmctree_node_get_row_data(ctree, node);
		if (msginfo && msginfo->msgnum == msgnum) break;
	}

	return node;
}

static guint attract_hash_func(gconstpointer key)
{
	gchar *str;
	gchar *p;
	guint h;

	Xstrdup_a(str, (const gchar *)key, return 0);
	trim_subject(str);

	p = str;
	h = *p;

	if (h) {
		for (p += 1; *p != '\0'; p++)
			h = (h << 5) - h + *p;
	}

	return h;
}

static gint attract_compare_func(gconstpointer a, gconstpointer b)
{
	return subject_compare((const gchar *)a, (const gchar *)b) == 0;
}

void summary_attract_by_subject(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCList *clist = GTK_CMCLIST(ctree);
	GtkCMCTreeNode *src_node;
	GtkCMCTreeNode *dst_node, *sibling;
	GtkCMCTreeNode *tmp;
	MsgInfo *src_msginfo, *dst_msginfo;
	GHashTable *subject_table;

	debug_print("Attracting messages by subject...");
	STATUSBAR_PUSH(summaryview->mainwin,
		       _("Attracting messages by subject..."));

	main_window_cursor_wait(summaryview->mainwin);
	summary_freeze(summaryview);

	subject_table = g_hash_table_new(attract_hash_func,
					 attract_compare_func);

	for (src_node = GTK_CMCTREE_NODE(clist->row_list);
	     src_node != NULL;
	     src_node = tmp) {
		tmp = GTK_CMCTREE_ROW(src_node)->sibling;
		src_msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(src_node);
		if (!src_msginfo) continue;
		if (!src_msginfo->subject) continue;

		/* find attracting node */
		dst_node = g_hash_table_lookup(subject_table,
					       src_msginfo->subject);

		if (dst_node) {
			dst_msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(dst_node);

			/* if the time difference is more than 20 days,
			   don't attract */
			if (ABS(src_msginfo->date_t - dst_msginfo->date_t)
			    > 60 * 60 * 24 * 20)
				continue;

			sibling = GTK_CMCTREE_ROW(dst_node)->sibling;
			if (src_node != sibling)
				gtk_cmctree_move(ctree, src_node, NULL, sibling);
		}

		g_hash_table_insert(subject_table,
				    src_msginfo->subject, src_node);
	}

	g_hash_table_destroy(subject_table);

	gtk_cmctree_node_moveto(ctree, summaryview->selected, 0, 0.5, 0);

	summary_thaw(summaryview);

	debug_print("done.\n");
	STATUSBAR_POP(summaryview->mainwin);

	main_window_cursor_normal(summaryview->mainwin);
}

static void summary_free_msginfo_func(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				      gpointer data)
{
	MsgInfo *msginfo = gtk_cmctree_node_get_row_data(ctree, node);

	if (msginfo)
		procmsg_msginfo_free(msginfo);
}

static void summary_set_marks_func(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				   gpointer data)
{
	SummaryView *summaryview = data;
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, node);

	if (MSG_IS_DELETED(msginfo->flags))
		summaryview->deleted++;

	summaryview->total_size += msginfo->size;

	summary_set_row_marks(summaryview, node);
}

static void summary_update_status(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	MsgInfo *msginfo;

	summaryview->total_size =
	summaryview->deleted = summaryview->moved = summaryview->copied = 0;

	for (node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
	     node != NULL; node = gtkut_ctree_node_next(ctree, node)) {
		msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(node);
		
		if (!msginfo)
			continue;

		if (MSG_IS_DELETED(msginfo->flags))
			summaryview->deleted++;
		if (MSG_IS_MOVE(msginfo->flags))
			summaryview->moved++;
		if (MSG_IS_COPY(msginfo->flags))
			summaryview->copied++;
		summaryview->total_size += msginfo->size;
	}
}

static void summary_status_show(SummaryView *summaryview)
{
	gchar *str;
	gchar *del, *mv, *cp;
	gchar *sel;
	gchar *spc;
	gchar *itstr;
	GList *rowlist, *cur;
	guint n_selected = 0, n_new = 0, n_unread = 0, n_total = 0;
	guint n_marked = 0, n_replied = 0, n_forwarded = 0, n_locked = 0, n_ignored = 0, n_watched = 0;
	goffset sel_size = 0, n_size = 0;
	MsgInfo *msginfo;
	gchar *name;
	gchar *tooltip;
	
	if (!summaryview->folder_item) {
		gtk_label_set_text(GTK_LABEL(summaryview->statlabel_folder), "");
		gtk_label_set_text(GTK_LABEL(summaryview->statlabel_select), "");
		gtk_label_set_text(GTK_LABEL(summaryview->statlabel_msgs),   "");
		toolbar_main_set_sensitive(summaryview->mainwin);
		return;
	}

	rowlist = GTK_CMCLIST(summaryview->ctree)->selection;
	for (cur = rowlist; cur != NULL && cur->data != NULL; cur = cur->next) {
		msginfo = gtk_cmctree_node_get_row_data
			(GTK_CMCTREE(summaryview->ctree),
			 GTK_CMCTREE_NODE(cur->data));
		if (msginfo) {
			sel_size += msginfo->size;
			n_selected++;
		}
	}
	
	if (summaryview->folder_item->hide_read_msgs 
	|| summaryview->folder_item->hide_del_msgs
	|| summaryview->folder_item->hide_read_threads
	|| quicksearch_has_sat_predicate(summaryview->quicksearch)) {
		rowlist = GTK_CMCLIST(summaryview->ctree)->row_list;
		for (cur = rowlist; cur != NULL && cur->data != NULL; cur = cur->next) {
			msginfo = gtk_cmctree_node_get_row_data
				(GTK_CMCTREE(summaryview->ctree),
				 GTK_CMCTREE_NODE(cur));
			if (msginfo) {
				n_size += msginfo->size;
				n_total++;
				if (MSG_IS_NEW(msginfo->flags))
					n_new++;
				if (MSG_IS_UNREAD(msginfo->flags))
					n_unread++;
				if (MSG_IS_MARKED(msginfo->flags))
					n_marked++;
				if (MSG_IS_REPLIED(msginfo->flags))
					n_replied++;
				if (MSG_IS_FORWARDED(msginfo->flags))
					n_forwarded++;
				if (MSG_IS_LOCKED(msginfo->flags))
					n_locked++;
				if (MSG_IS_IGNORE_THREAD(msginfo->flags))
					n_ignored++;
				if (MSG_IS_WATCH_THREAD(msginfo->flags))
					n_watched++;
			}
		}
	} else {
		n_new = summaryview->folder_item->new_msgs;
		n_unread = summaryview->folder_item->unread_msgs;
		n_marked = summaryview->folder_item->marked_msgs;
		n_replied = summaryview->folder_item->replied_msgs;
		n_forwarded = summaryview->folder_item->forwarded_msgs;
		n_locked = summaryview->folder_item->locked_msgs;
		n_ignored = summaryview->folder_item->ignored_msgs;
		n_watched = summaryview->folder_item->watched_msgs;
		n_total = summaryview->folder_item->total_msgs;
		n_size = summaryview->total_size;
	}

	name = folder_item_get_name(summaryview->folder_item);
	gtk_label_set_text(GTK_LABEL(summaryview->statlabel_folder), name);
	g_free(name);

	if (summaryview->deleted)
		del = g_strdup_printf(_("%d deleted"), summaryview->deleted);
	else
		del = g_strdup("");
	if (summaryview->moved)
		mv = g_strdup_printf(_("%s%d moved"),
				     summaryview->deleted ? _(", ") : "",
				     summaryview->moved);
	else
		mv = g_strdup("");
	if (summaryview->copied)
		cp = g_strdup_printf(_("%s%d copied"),
				     summaryview->deleted ||
				     summaryview->moved ? _(", ") : "",
				     summaryview->copied);
	else
		cp = g_strdup("");

	if (summaryview->deleted || summaryview->moved || summaryview->copied)
		spc = "    ";
	else
		spc = "";

	if (n_selected) {
		sel = g_strdup_printf(" (%s)", to_human_readable((goffset)sel_size));
		itstr = g_strdup_printf(ngettext(" item selected"," items selected", n_selected));
	} else {
		sel = g_strdup("");
		itstr = g_strdup("");
	}
		
	if (prefs_common.layout_mode != SMALL_LAYOUT) {
		str = g_strconcat(n_selected ? itos(n_selected) : "",
						itstr, sel, spc, del, mv, cp, NULL);
		g_free(sel);
		g_free(del);
		g_free(mv);
		g_free(cp);
		g_free(itstr);
		
		gtk_label_set_text(GTK_LABEL(summaryview->statlabel_select), str);
		g_free(str);

		str = g_strdup_printf(_("%d new, %d unread, %d total (%s)"),
					      n_new, n_unread, n_total,
					      to_human_readable((goffset)n_size));


		gtk_label_set_text(GTK_LABEL(summaryview->statlabel_msgs), str);
		g_free(str);
		tooltip = g_strdup_printf(_("<b>Message summary</b>\n"
					    "<b>New:</b> %d\n"
					    "<b>Unread:</b> %d\n"
					    "<b>Total:</b> %d\n"
					    "<b>Size:</b> %s\n\n"
					    "<b>Marked:</b> %d\n"
					    "<b>Replied:</b> %d\n"
					    "<b>Forwarded:</b> %d\n"
					    "<b>Locked:</b> %d\n"
					    "<b>Ignored:</b> %d\n"
					    "<b>Watched:</b> %d"),
					      n_new, n_unread, n_total,
					      to_human_readable((goffset)n_size),
					      n_marked,n_replied,n_forwarded,
					      n_locked,n_ignored,n_watched);

		gtk_widget_set_tooltip_markup(GTK_WIDGET(summaryview->statlabel_msgs),
				            tooltip); 
		g_free(tooltip);
	} else {
		gchar *ssize, *tsize;
		if (n_selected) {
			ssize = g_strdup(to_human_readable((goffset)sel_size));
			tsize = g_strdup(to_human_readable((goffset)n_size));
			str = g_strdup_printf(_("%d/%d selected (%s/%s), %d unread"),
				n_selected, n_total, ssize, tsize, n_unread);
			g_free(ssize);
			g_free(tsize);
		} else
			str = g_strdup_printf(_("%d new, %d unread, %d total (%s)"),
				n_new, n_unread, n_total, to_human_readable((goffset)n_size));
		g_free(sel);
		g_free(del);
		g_free(mv);
		g_free(cp);
		g_free(itstr);
		
		gtk_label_set_text(GTK_LABEL(summaryview->statlabel_select), str);
		g_free(str);
	}

	summary_set_menu_sensitive(summaryview);
	toolbar_main_set_sensitive(summaryview->mainwin);
}

static void summary_set_column_titles(SummaryView *summaryview)
{
	GtkCMCList *clist = GTK_CMCLIST(summaryview->ctree);
	FolderItem *item = summaryview->folder_item;
	GtkWidget *hbox;
	GtkWidget *label;
	GtkWidget *arrow;
	gint pos;
	const gchar *title;
	SummaryColumnType type;
	GtkJustification justify;

	static FolderSortKey sort_by[N_SUMMARY_COLS] = {
		SORT_BY_MARK,
		SORT_BY_STATUS,
		SORT_BY_MIME,
		SORT_BY_SUBJECT,
		SORT_BY_FROM,
		SORT_BY_TO,
		SORT_BY_DATE,
		SORT_BY_SIZE,
		SORT_BY_NUMBER,
		SORT_BY_SCORE,
		SORT_BY_LOCKED,
		SORT_BY_TAGS
	};

	for (pos = 0; pos < N_SUMMARY_COLS; pos++) {
		type = summaryview->col_state[pos].type;

		/* CLAWS: mime and unread are single char headers */
		justify = (type == S_COL_NUMBER || type == S_COL_SIZE)
			? GTK_JUSTIFY_RIGHT : GTK_JUSTIFY_LEFT;

		switch (type) {
		case S_COL_SUBJECT:
		case S_COL_FROM:
		case S_COL_TO:
		case S_COL_DATE:
		case S_COL_NUMBER:
			if(type == S_COL_FROM && item != NULL &&
					FOLDER_SHOWS_TO_HDR(item) &&
					!summaryview->col_state[summaryview->col_pos[S_COL_TO]].visible)
				type = S_COL_TO;
			if(type == S_COL_NUMBER)
				title = gettext(col_label[type]);
			else
				title = prefs_common_translated_header_name(col_label[type]);
			break;
		default:
			title = gettext(col_label[type]);
		}

		if (type == S_COL_MIME) {
			label = gtk_image_new_from_pixbuf(clipxpm);
			gtk_widget_show(label);
			gtk_cmclist_set_column_widget(clist, pos, label);
			gtk_sctree_set_column_tooltip(GTK_SCTREE(clist), pos, _("Attachment"));
			continue;
		} else if (type == S_COL_MARK) {
			label = gtk_image_new_from_pixbuf(markxpm);
			gtk_widget_show(label);
			gtk_cmclist_set_column_widget(clist, pos, label);
			gtk_sctree_set_column_tooltip(GTK_SCTREE(clist), pos, _("Mark"));
			continue;
		} else if (type == S_COL_LOCKED) {
			label = gtk_image_new_from_pixbuf(lockedxpm);
			gtk_widget_show(label);
			gtk_cmclist_set_column_widget(clist, pos, label);
			gtk_sctree_set_column_tooltip(GTK_SCTREE(clist), pos, _("Locked"));
			continue;
		} else if (type == S_COL_STATUS) {
			gtk_cmclist_set_column_title(clist, pos, title);
			gtk_sctree_set_column_tooltip(GTK_SCTREE(clist), pos, _("Status"));
			continue;
		}

		hbox  = gtk_hbox_new(FALSE, 4);
		label = gtk_label_new(title);
#ifdef GENERIC_UMPC
	gtk_widget_set_size_request(hbox, -1, 20);
#endif

		if (justify == GTK_JUSTIFY_RIGHT)
			gtk_box_pack_end(GTK_BOX(hbox), label,
					 FALSE, FALSE, 0);
		else
			gtk_box_pack_start(GTK_BOX(hbox), label,
					   FALSE, FALSE, 0);

		if (summaryview->sort_key == sort_by[type]) {
			arrow = gtk_arrow_new
				(summaryview->sort_type == SORT_ASCENDING
				 ? GTK_ARROW_DOWN : GTK_ARROW_UP,
				 GTK_SHADOW_IN);
			gtk_widget_set_size_request(GTK_WIDGET(arrow), 10, 10);
			if (justify == GTK_JUSTIFY_RIGHT)
				gtk_box_pack_start(GTK_BOX(hbox), arrow,
						   FALSE, FALSE, 0);
			else
				gtk_box_pack_end(GTK_BOX(hbox), arrow,
						 FALSE, FALSE, 0);
		}

		gtk_widget_show_all(hbox);
		gtk_cmclist_set_column_widget(clist, pos, hbox);
	}
}

void summary_reflect_tags_changes(SummaryView *summaryview)
{
	GtkMenuShell *menu;
	GList *children, *cur;
	GtkCMCTreeNode *node;
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	gboolean froze = FALSE;
	gboolean redisplay = FALSE;

	/* re-create colorlabel submenu */
	menu = GTK_MENU_SHELL(summaryview->tags_menu);
	cm_return_if_fail(menu != NULL);

	/* clear items. get item pointers. */
	children = gtk_container_get_children(GTK_CONTAINER(menu));
	for (cur = children; cur != NULL && cur->data != NULL; cur = cur->next) {
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(cur->data), NULL);
	}
	g_list_free(children);
	summary_tags_menu_create(summaryview, TRUE);

	START_LONG_OPERATION(summaryview, TRUE);
	for (node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list); node != NULL;
	     node = gtkut_ctree_node_next(ctree, node)) {
		redisplay |= summary_set_row_tag(summaryview,
					   node, TRUE, FALSE, 0);
	}
	END_LONG_OPERATION(summaryview);
	if (redisplay)
		summary_redisplay_msg(summaryview);
}


void summary_reflect_prefs(void)
{
	static gchar *last_smallfont = NULL;
	static gchar *last_normalfont = NULL;
	static gchar *last_boldfont = NULL;
	static gboolean last_derive = 0;
	gboolean update_font = FALSE;
	SummaryView *summaryview = NULL;

	if (!mainwindow_get_mainwindow())
		return;
	summaryview = mainwindow_get_mainwindow()->summaryview;

	if (!last_smallfont || strcmp(last_smallfont, SMALL_FONT) ||
			!last_normalfont || strcmp(last_normalfont, NORMAL_FONT) ||
			!last_boldfont || strcmp(last_boldfont, BOLD_FONT) ||
			last_derive != prefs_common.derive_from_normal_font)
		update_font = TRUE;

	g_free(last_smallfont);
	last_smallfont = g_strdup(SMALL_FONT);
	g_free(last_normalfont);
	last_normalfont = g_strdup(NORMAL_FONT);
	g_free(last_boldfont);
	last_boldfont = g_strdup(BOLD_FONT);
	last_derive = prefs_common.derive_from_normal_font;

	if (update_font) {	
		bold_style = bold_marked_style = bold_deleted_style = 
			small_style = small_marked_style = small_deleted_style = NULL;
		summary_set_fonts(summaryview);
	}

	summary_set_column_titles(summaryview);
	summary_relayout(summaryview);
	
	if (summaryview->folder_item)
		summary_show(summaryview, summaryview->folder_item);
}

void summary_sort(SummaryView *summaryview,
		  FolderSortKey sort_key, FolderSortType sort_type)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCList *clist = GTK_CMCLIST(summaryview->ctree);
	GtkCMCListCompareFunc cmp_func = NULL;
	START_TIMING("");
	g_signal_handlers_block_by_func(G_OBJECT(summaryview->ctree),
				       G_CALLBACK(summary_tree_expanded), summaryview);
	summary_freeze(summaryview);

	switch (sort_key) {
	case SORT_BY_MARK:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_mark;
		break;
	case SORT_BY_STATUS:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_status;
		break;
	case SORT_BY_MIME:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_mime;
		break;
	case SORT_BY_NUMBER:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_num;
		break;
	case SORT_BY_SIZE:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_size;
		break;
	case SORT_BY_DATE:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_date;
		break;
	case SORT_BY_THREAD_DATE:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_thread_date;
		break;
	case SORT_BY_FROM:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_from;
		break;
	case SORT_BY_SUBJECT:
#ifndef G_OS_WIN32
		if (summaryview->simplify_subject_preg)
			cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_simplified_subject;
		else
#endif
			cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_subject;
		break;
	case SORT_BY_SCORE:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_score;
		break;
	case SORT_BY_LABEL:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_label;
		break;
	case SORT_BY_TO:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_to;
		break;
	case SORT_BY_LOCKED:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_locked;
		break;
	case SORT_BY_TAGS:
		cmp_func = (GtkCMCListCompareFunc)summary_cmp_by_tags;
		break;
	case SORT_BY_NONE:
		break;
	default:
		goto unlock;
	}

	summaryview->sort_key = sort_key;
	summaryview->sort_type = sort_type;

	summary_set_column_titles(summaryview);
	summary_set_menu_sensitive(summaryview);

	/* allow fallback to don't sort */
	if (summaryview->sort_key == SORT_BY_NONE)
		goto unlock;

	if (cmp_func != NULL) {
		debug_print("Sorting summary...");
		STATUSBAR_PUSH(summaryview->mainwin, _("Sorting summary..."));

		main_window_cursor_wait(summaryview->mainwin);

		gtk_cmclist_set_compare_func(clist, cmp_func);

		gtk_cmclist_set_sort_type(clist, (GtkSortType)sort_type);
		gtk_sctree_sort_recursive(ctree, NULL);

		gtk_cmctree_node_moveto(ctree, summaryview->selected, 0, 0.5, 0);

		main_window_cursor_normal(summaryview->mainwin);

		debug_print("done.\n");
		STATUSBAR_POP(summaryview->mainwin);
	}
unlock:
	summary_thaw(summaryview);
	g_signal_handlers_unblock_by_func(G_OBJECT(summaryview->ctree),
				       G_CALLBACK(summary_tree_expanded), summaryview);
	END_TIMING();
}

static gboolean summary_update_thread_age(GNode *node, gpointer data)
{
	MsgInfo *msginfo = node->data;
	time_t *most_recent = (time_t *)data;

	if (msginfo->date_t > *most_recent) {
		*most_recent = msginfo->date_t;
	}
	return FALSE;
}	

static void summary_find_thread_age(GNode *gnode)
{
	MsgInfo *msginfo = (MsgInfo *)gnode->data;
	time_t most_recent;

	if (!msginfo)
		return;
	most_recent = msginfo->thread_date = msginfo->date_t;

	g_node_traverse(gnode, G_IN_ORDER, G_TRAVERSE_ALL, -1, summary_update_thread_age, &most_recent);

	msginfo->thread_date = most_recent;
}

static gboolean summary_update_is_read(GNode *node, gpointer data)
{
	MsgInfo *msginfo = node->data;
	gboolean *all_read = (gboolean *)data;

	if (MSG_IS_UNREAD(msginfo->flags)) {
		*all_read = FALSE;
		return TRUE;
	}
	return FALSE;
}	

static gboolean summary_thread_is_read(GNode *gnode)
{
	MsgInfo *msginfo = (MsgInfo *)gnode->data;
	gboolean all_read = TRUE;

	if (!msginfo)
		return all_read;

	g_node_traverse(gnode, G_IN_ORDER, G_TRAVERSE_ALL, -1, summary_update_is_read, &all_read);
    return all_read;
}

static gboolean summary_insert_gnode_func(GtkCMCTree *ctree, guint depth, GNode *gnode,
				   GtkCMCTreeNode *cnode, gpointer data)
{
	SummaryView *summaryview = (SummaryView *)data;
	MsgInfo *msginfo = (MsgInfo *)gnode->data;
	gchar *text[N_SUMMARY_COLS];
	gint *col_pos = summaryview->col_pos;
	const gchar *msgid = msginfo->msgid;
	GHashTable *msgid_table = summaryview->msgid_table;
	gboolean vert = (prefs_common.layout_mode == VERTICAL_LAYOUT);

	summary_set_header(summaryview, text, msginfo);

	gtk_cmctree_set_node_info(ctree, cnode, text[col_pos[S_COL_SUBJECT]], 2,
				NULL, NULL, FALSE, summaryview->threaded && !summaryview->thread_collapsed);
#define SET_TEXT(col) {						\
	gtk_cmctree_node_set_text(ctree, cnode, col_pos[col], 	\
				text[col_pos[col]]);		\
}

	if (summaryview->col_state[summaryview->col_pos[S_COL_NUMBER]].visible)
		SET_TEXT(S_COL_NUMBER);
	if (summaryview->col_state[summaryview->col_pos[S_COL_SCORE]].visible)
		SET_TEXT(S_COL_SCORE);
	if (summaryview->col_state[summaryview->col_pos[S_COL_SIZE]].visible)
		SET_TEXT(S_COL_SIZE);
	if (summaryview->col_state[summaryview->col_pos[S_COL_DATE]].visible)
		SET_TEXT(S_COL_DATE);
	if (summaryview->col_state[summaryview->col_pos[S_COL_FROM]].visible)
		SET_TEXT(S_COL_FROM);
	if (summaryview->col_state[summaryview->col_pos[S_COL_TO]].visible)
		SET_TEXT(S_COL_TO);
	if (summaryview->col_state[summaryview->col_pos[S_COL_TAGS]].visible)
		SET_TEXT(S_COL_TAGS);

	if (vert && prefs_common.two_line_vert)
		g_free(text[summaryview->col_pos[S_COL_SUBJECT]]);

#undef SET_TEXT

	GTKUT_CTREE_NODE_SET_ROW_DATA(cnode, msginfo);
	summary_set_marks_func(ctree, cnode, summaryview);

	if (msgid && msgid[0] != '\0')
		g_hash_table_insert(msgid_table, (gchar *)msgid, cnode);

	return TRUE;
}

static void summary_set_ctree_from_list(SummaryView *summaryview,
					GSList *mlist)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;
	GtkCMCTreeNode *node = NULL;
	GHashTable *msgid_table;
	GHashTable *subject_table = NULL;
	GSList * cur;
	gboolean vert = (prefs_common.layout_mode == VERTICAL_LAYOUT);

	START_TIMING("");
	
	if (!mlist) return;

	debug_print("\tSetting summary from message data...\n");
	STATUSBAR_PUSH(summaryview->mainwin,
		       _("Setting summary from message data..."));
	gdk_flush();

	g_signal_handlers_block_by_func(G_OBJECT(ctree),
				       G_CALLBACK(summary_tree_expanded), summaryview);

	msgid_table = g_hash_table_new(g_str_hash, g_str_equal);
	summaryview->msgid_table = msgid_table;

	if (prefs_common.thread_by_subject) {
		subject_table = g_hash_table_new(g_str_hash, g_str_equal);
		summaryview->subject_table = subject_table;
	} else {
		summaryview->subject_table = NULL;
	}

	if (prefs_common.use_addr_book)
		start_address_completion(NULL);
	
	if (summaryview->threaded) {
		GNode *root, *gnode;
		START_TIMING("threaded");
		root = procmsg_get_thread_tree(mlist);

		
		for (gnode = root->children; gnode != NULL;
		     gnode = gnode->next) {
            if (!summaryview->folder_item->hide_read_threads ||
                    !summary_thread_is_read(gnode))
            {
                summary_find_thread_age(gnode);
                node = gtk_sctree_insert_gnode
                    (ctree, NULL, node, gnode,
                     summary_insert_gnode_func, summaryview);
            }
		}

		g_node_destroy(root);
                
		END_TIMING();
	} else {
		gchar *text[N_SUMMARY_COLS];
		START_TIMING("unthreaded");
		cur = mlist;
		for (; mlist != NULL; mlist = mlist->next) {
			msginfo = (MsgInfo *)mlist->data;

			summary_set_header(summaryview, text, msginfo);

			node = gtk_sctree_insert_node
				(ctree, NULL, node, text, 2,
				 NULL, NULL,
				 FALSE, FALSE);
			if (vert && prefs_common.two_line_vert)
				g_free(text[summaryview->col_pos[S_COL_SUBJECT]]);

			GTKUT_CTREE_NODE_SET_ROW_DATA(node, msginfo);
			summary_set_marks_func(ctree, node, summaryview);

			if (msginfo->msgid && msginfo->msgid[0] != '\0')
				g_hash_table_insert(msgid_table,
						    msginfo->msgid, node);

			if (prefs_common.thread_by_subject)
				subject_table_insert(subject_table,
					     msginfo->subject,
					     node);
		}
		mlist = cur;
		END_TIMING();
	}

	if (prefs_common.enable_hscrollbar &&
	    summaryview->col_pos[S_COL_SUBJECT] == N_SUMMARY_COLS - 1) {
		gint optimal_width;

		optimal_width = gtk_cmclist_optimal_column_width
			(GTK_CMCLIST(ctree), summaryview->col_pos[S_COL_SUBJECT]);
		gtk_cmclist_set_column_width(GTK_CMCLIST(ctree),
					   summaryview->col_pos[S_COL_SUBJECT],
					   optimal_width);
	}

	if (prefs_common.use_addr_book)
		end_address_completion();

	debug_print("done.\n");
	STATUSBAR_POP(summaryview->mainwin);
	if (debug_get_mode()) {
		debug_print("\tmsgid hash table size = %d\n",
			    g_hash_table_size(msgid_table));
		if (prefs_common.thread_by_subject)
			debug_print("\tsubject hash table size = %d\n",
			    g_hash_table_size(subject_table));
	}

	summary_sort(summaryview, summaryview->sort_key, summaryview->sort_type);

	node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);

	if (prefs_common.bold_unread) {
		START_TIMING("bold_unread");
		while (node) {
			GtkCMCTreeNode *next = GTK_CMCTREE_NODE_NEXT(node);
			if (GTK_CMCTREE_ROW(node)->children)
				summary_set_row_marks(summaryview, node);
			node = next;
		}
		END_TIMING();
	}

	g_signal_handlers_unblock_by_func(G_OBJECT(ctree),
				       G_CALLBACK(summary_tree_expanded), summaryview);
	END_TIMING();
}

static gchar *summary_complete_address(const gchar *addr)
{
	gint count;
	gchar *res, *tmp, *email_addr;
	
	if (addr == NULL || !strchr(addr, '@'))
		return NULL;

	Xstrdup_a(email_addr, addr, return NULL);
	extract_address(email_addr);
	if (!*email_addr)
		return NULL;

	/*
	 * completion stuff must be already initialized
	 */
	res = NULL;
	if (1 < (count = complete_address(email_addr))) {
		tmp = get_complete_address(1);
		res = procheader_get_fromname(tmp);
		g_free(tmp);
	}

	return res;
}

static inline void summary_set_header(SummaryView *summaryview, gchar *text[],
			       MsgInfo *msginfo)
{
	static gchar date_modified[80];
	static gchar col_score[11];
	static gchar buf[BUFFSIZE], tmp1[BUFFSIZE], tmp2[BUFFSIZE], tmp3[BUFFSIZE];
	gint *col_pos = summaryview->col_pos;
	gchar *from_text = NULL, *to_text = NULL, *tags_text = NULL;
	gboolean should_swap = FALSE;
	gboolean vert = (prefs_common.layout_mode == VERTICAL_LAYOUT);
	static const gchar *color_dim_rgb = NULL;
	if (!color_dim_rgb)
		color_dim_rgb = gdk_color_to_string(&summaryview->color_dim);
	text[col_pos[S_COL_FROM]]   = "";
	text[col_pos[S_COL_TO]]     = "";
	text[col_pos[S_COL_SUBJECT]]= "";
	text[col_pos[S_COL_MARK]]   = "";
	text[col_pos[S_COL_STATUS]] = "";
	text[col_pos[S_COL_MIME]]   = "";
	text[col_pos[S_COL_LOCKED]] = "";
	text[col_pos[S_COL_DATE]]   = "";
	text[col_pos[S_COL_TAGS]]   = "";
	if (summaryview->col_state[summaryview->col_pos[S_COL_NUMBER]].visible)
		text[col_pos[S_COL_NUMBER]] = itos(msginfo->msgnum);
	else
		text[col_pos[S_COL_NUMBER]] = "";

	/* slow! */
	if (summaryview->col_state[summaryview->col_pos[S_COL_SIZE]].visible)
		text[col_pos[S_COL_SIZE]] = to_human_readable(msginfo->size);
	else
		text[col_pos[S_COL_SIZE]] = "";

	if (summaryview->col_state[summaryview->col_pos[S_COL_SCORE]].visible)
		text[col_pos[S_COL_SCORE]] = itos_buf(col_score, msginfo->score);
	else
		text[col_pos[S_COL_SCORE]] = "";

	if (summaryview->col_state[summaryview->col_pos[S_COL_TAGS]].visible) {
		tags_text = procmsg_msginfo_get_tags_str(msginfo);
		if (!tags_text) {
			text[col_pos[S_COL_TAGS]] = "-";
		} else {
			strncpy2(tmp1, tags_text, sizeof(tmp1));
			tmp1[sizeof(tmp1)-1]='\0';
			g_free(tags_text);
			text[col_pos[S_COL_TAGS]] = tmp1;
		}
	} else
		text[col_pos[S_COL_TAGS]] = "";

	/* slow! */
	if (summaryview->col_state[summaryview->col_pos[S_COL_DATE]].visible || 
	    (vert && prefs_common.two_line_vert)) {
		if (msginfo->date_t && msginfo->date_t > 0) {
			procheader_date_get_localtime(date_modified,
						      sizeof(date_modified),
						      msginfo->date_t);
			text[col_pos[S_COL_DATE]] = date_modified;
		} else if (msginfo->date)
			text[col_pos[S_COL_DATE]] = msginfo->date;
		else
			text[col_pos[S_COL_DATE]] = _("(No Date)");
	}
	
	if (prefs_common.swap_from && msginfo->from && msginfo->to
	&&  !summaryview->col_state[summaryview->col_pos[S_COL_TO]].visible) {
		gchar *addr = NULL;
		
		addr = g_strdup(msginfo->from);

		if (addr) {
			extract_address(addr);
			if (account_find_from_address(addr, FALSE)) {
				should_swap = TRUE;
			}
			g_free(addr);
		}
	}

	if (!prefs_common.use_addr_book) {
		if (prefs_common.summary_from_show == SHOW_NAME)
			from_text = msginfo->fromname;
		else if (prefs_common.summary_from_show == SHOW_BOTH)
			from_text = msginfo->from;
		else {
			from_text = msginfo->from;
			extract_address(from_text);
		}
		if (!from_text)
			_("(No From)");		
	} else {
		gchar *tmp = summary_complete_address(msginfo->from);
		if (tmp) {
			strncpy2(buf, tmp, sizeof(buf));
			g_free(tmp);
			from_text = buf;
		} else {
			if (prefs_common.summary_from_show == SHOW_NAME)
				from_text = msginfo->fromname;
			else if (prefs_common.summary_from_show == SHOW_BOTH)
				from_text = msginfo->from;
			else {
				from_text = msginfo->from;
				extract_address(from_text);
			}
			if (!from_text)
				_("(No From)");		
		}
	}
	
	to_text = msginfo->to ? msginfo->to : 
		   (msginfo->cc ? msginfo->cc :
		     (msginfo->newsgroups ? msginfo->newsgroups : _("(No Recipient)")
		     )
		   );

	text[col_pos[S_COL_TO]] = to_text;
	if (!should_swap) {
		text[col_pos[S_COL_FROM]] = from_text;
	} else {
		if (prefs_common.use_addr_book) {
			gchar *tmp = summary_complete_address(to_text);
			if (tmp) {
				strncpy2(buf, tmp, sizeof(buf));
				g_free(tmp);
				to_text = buf;
			} else {
				to_text = to_text ? to_text : _("(No From)");
			}
		}
		snprintf(tmp2, BUFFSIZE-1, "--> %s", to_text);
		tmp2[BUFFSIZE-1]='\0';
		text[col_pos[S_COL_FROM]] = tmp2;
	}
	
#ifndef G_OS_WIN32
	if (summaryview->simplify_subject_preg != NULL)
		text[col_pos[S_COL_SUBJECT]] = msginfo->subject ? 
			string_remove_match(tmp3, BUFFSIZE, msginfo->subject, 
					summaryview->simplify_subject_preg) : 
			_("(No Subject)");
	else 
#endif
		text[col_pos[S_COL_SUBJECT]] = msginfo->subject ? msginfo->subject :
			_("(No Subject)");
	if (vert && prefs_common.two_line_vert) {
		if (!FOLDER_SHOWS_TO_HDR(summaryview->folder_item)) {
			gchar *tmp = g_markup_printf_escaped(_("%s\n<span color='%s' style='italic'>From: %s, on %s</span>"),
					text[col_pos[S_COL_SUBJECT]],
					color_dim_rgb,
					text[col_pos[S_COL_FROM]],
					text[col_pos[S_COL_DATE]]);
			text[col_pos[S_COL_SUBJECT]] = tmp;
		} else {
			gchar *tmp = g_markup_printf_escaped(_("%s\n<span color='%s' style='italic'>To: %s, on %s</span>"),
					text[col_pos[S_COL_SUBJECT]],
					color_dim_rgb,
					text[col_pos[S_COL_TO]],
					text[col_pos[S_COL_DATE]]);
			text[col_pos[S_COL_SUBJECT]] = tmp;
		}
	}
}

static void summary_display_msg(SummaryView *summaryview, GtkCMCTreeNode *row)
{
	summary_display_msg_full(summaryview, row, FALSE, FALSE);
}

static gboolean defer_change(gpointer data);
typedef struct _ChangeData {
	MsgInfo *info;
	gint op; /* 0, 1, 2 for unset, set, change */
	MsgPermFlags set_flags;
	MsgTmpFlags  set_tmp_flags;
	MsgPermFlags unset_flags;
	MsgTmpFlags  unset_tmp_flags;
} ChangeData;

static void summary_msginfo_unset_flags(MsgInfo *msginfo, MsgPermFlags flags, MsgTmpFlags tmp_flags)
{
	if (!msginfo->folder || !msginfo->folder->processing_pending) {
		debug_print("flags: doing unset now\n");
		procmsg_msginfo_unset_flags(msginfo, flags, tmp_flags);
	} else {
		ChangeData *unset_data = g_new0(ChangeData, 1);
		unset_data->info = msginfo;
		unset_data->op = 0;
		unset_data->unset_flags = flags;
		unset_data->unset_tmp_flags = tmp_flags;
		debug_print("flags: deferring unset\n");
		g_timeout_add(100, defer_change, unset_data);
	}
}

static void summary_msginfo_set_flags(MsgInfo *msginfo, MsgPermFlags flags, MsgTmpFlags tmp_flags)
{
	if (!msginfo->folder || !msginfo->folder->processing_pending) {
		debug_print("flags: doing set now\n");
		procmsg_msginfo_set_flags(msginfo, flags, tmp_flags);
	} else {
		ChangeData *set_data = g_new0(ChangeData, 1);
		set_data->info = msginfo;
		set_data->op = 1;
		set_data->set_flags = flags;
		set_data->set_tmp_flags = tmp_flags;
		debug_print("flags: deferring set\n");
		g_timeout_add(100, defer_change, set_data);
	}
}

static void summary_msginfo_change_flags(MsgInfo *msginfo, 
		MsgPermFlags add_flags, MsgTmpFlags add_tmp_flags,
		MsgPermFlags rem_flags, MsgTmpFlags rem_tmp_flags)
{
	if (!msginfo->folder || !msginfo->folder->processing_pending) {
		debug_print("flags: doing change now\n");
		procmsg_msginfo_change_flags(msginfo, add_flags, add_tmp_flags,
			rem_flags, rem_tmp_flags);
	} else {
		ChangeData *change_data = g_new0(ChangeData, 1);
		change_data->info = msginfo;
		change_data->op = 2;
		change_data->set_flags = add_flags;
		change_data->set_tmp_flags = add_tmp_flags;
		change_data->unset_flags = rem_flags;
		change_data->unset_tmp_flags = rem_tmp_flags;
		debug_print("flags: deferring change\n");
		g_timeout_add(100, defer_change, change_data);
	}
}

gboolean defer_change(gpointer data)
{
	ChangeData *chg = (ChangeData *)data;
	if (chg->info->folder && chg->info->folder->processing_pending) {
		debug_print("flags: trying later\n");
		return TRUE; /* try again */
	} else {
		debug_print("flags: finally doing it\n");
		switch(chg->op) {
		case 0:
			procmsg_msginfo_unset_flags(chg->info, chg->unset_flags, chg->unset_tmp_flags);
			break;
		case 1:
			procmsg_msginfo_set_flags(chg->info, chg->set_flags, chg->set_tmp_flags);
			break;
		case 2:
			procmsg_msginfo_change_flags(chg->info, chg->set_flags, chg->set_tmp_flags,
				chg->unset_flags, chg->unset_tmp_flags);
			break;
		default:
			g_warning("shouldn't happen\n");
		}
		g_free(chg);
	}
	return FALSE;
}

static void msginfo_mark_as_read (SummaryView *summaryview, MsgInfo *msginfo,
				      GtkCMCTreeNode *row)
{
	cm_return_if_fail(summaryview != NULL);
	cm_return_if_fail(msginfo != NULL);
	cm_return_if_fail(row != NULL);

	if (MSG_IS_NEW(msginfo->flags) || MSG_IS_UNREAD(msginfo->flags)) {
		summary_msginfo_unset_flags
			(msginfo, MSG_NEW | MSG_UNREAD, 0);
		summary_set_row_marks(summaryview, row);
		summary_status_show(summaryview);
	}
}

typedef struct  {
	MsgInfo *msginfo;
	SummaryView *summaryview;
} MarkAsReadData;

static int msginfo_mark_as_read_timeout(void *data)
{
	MarkAsReadData *mdata = (MarkAsReadData *)data;
	if (!mdata)
		return FALSE;
	
	if (mdata->msginfo == summary_get_selected_msg(mdata->summaryview))
		msginfo_mark_as_read(mdata->summaryview, mdata->msginfo,
				     mdata->summaryview->selected); 

	g_free(mdata);

	return FALSE;	
}

static void summary_display_msg_full(SummaryView *summaryview,
				     GtkCMCTreeNode *row,
				     gboolean new_window, gboolean all_headers)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;
	gint val;
	START_TIMING("");
	if (!new_window) {
		if (summaryview->displayed == row &&
		    messageview_is_visible(summaryview->messageview))
			return;
		else if (summaryview->messageview)
			summaryview->messageview->filtered = FALSE;
	}			
	cm_return_if_fail(row != NULL);

	if (summary_is_locked(summaryview)) return;
	summary_lock(summaryview);

	STATUSBAR_POP(summaryview->mainwin);
	GTK_EVENTS_FLUSH();

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);

	if (!msginfo) {
		debug_print("NULL msginfo\n");
		summary_unlock(summaryview);
		END_TIMING();
		return;
	}	

	if (new_window && prefs_common.layout_mode != SMALL_LAYOUT) {
		MessageView *msgview;

		msgview = messageview_create_with_new_window(summaryview->mainwin);
		val = messageview_show(msgview, msginfo, all_headers);
	} else {
		MessageView *msgview;

		if (prefs_common.layout_mode == SMALL_LAYOUT) {
			if (summaryview->ext_messageview == NULL)
				summaryview->ext_messageview = messageview_create_with_new_window(summaryview->mainwin);
			else
				gtkut_window_popup(summaryview->ext_messageview->window);
			msgview = summaryview->ext_messageview;
			summaryview->displayed = row;
			val = messageview_show(msgview, msginfo, all_headers);
			if (mimeview_tree_is_empty(msgview->mimeview))
				gtk_widget_grab_focus(summaryview->ctree);
			gtkut_ctree_node_move_if_on_the_edge(ctree, row,
				GTK_CMCLIST(summaryview->ctree)->focus_row);
		} else {
			msgview = summaryview->messageview;
			summaryview->displayed = row;
			if (!messageview_is_visible(msgview) &&
			    gtk_window_is_active(GTK_WINDOW(summaryview->mainwin->window))) {
				main_window_toggle_message_view(summaryview->mainwin);
				GTK_EVENTS_FLUSH();
			}
			val = messageview_show(msgview, msginfo, all_headers);
			if (mimeview_tree_is_empty(msgview->mimeview))
				gtk_widget_grab_focus(summaryview->ctree);
			gtkut_ctree_node_move_if_on_the_edge(ctree, row,
				GTK_CMCLIST(summaryview->ctree)->focus_row);
		}
	}

	if (val == 0 && MSG_IS_UNREAD(msginfo->flags)) {
		if (!prefs_common.mark_as_read_on_new_window &&
		    prefs_common.mark_as_read_delay) {
			MarkAsReadData *data = g_new0(MarkAsReadData, 1);
			data->summaryview = summaryview;
			data->msginfo = msginfo;
#if GLIB_CHECK_VERSION(2,14,0)
			g_timeout_add_seconds(prefs_common.mark_as_read_delay,
				msginfo_mark_as_read_timeout, data);
#else
			g_timeout_add(prefs_common.mark_as_read_delay * 1000,
				msginfo_mark_as_read_timeout, data);
#endif
		} else if (new_window || !prefs_common.mark_as_read_on_new_window) {
			msginfo_mark_as_read(summaryview, msginfo, row);
		}
	}

	summary_set_menu_sensitive(summaryview);
	toolbar_main_set_sensitive(summaryview->mainwin);
	messageview_set_menu_sensitive(summaryview->messageview);

	summary_unlock(summaryview);
	END_TIMING();
}

void summary_display_msg_selected(SummaryView *summaryview,
				  gboolean all_headers)
{
	if (summary_is_locked(summaryview)) return;
	summaryview->displayed = NULL;
	summary_display_msg_full(summaryview, summaryview->selected, FALSE,
				 all_headers);
}

void summary_redisplay_msg(SummaryView *summaryview)
{
	GtkCMCTreeNode *node;

	if (summaryview->displayed) {
		node = summaryview->displayed;
		summaryview->displayed = NULL;
		summary_display_msg(summaryview, node);
	}
}

void summary_open_msg(SummaryView *summaryview)
{
	if (!summaryview->selected) return;
	
	/* CLAWS: if separate message view, don't open a new window
	 * but rather use the current separated message view */
	summary_display_msg_full(summaryview, summaryview->selected, 
				 TRUE, FALSE);
}

void summary_view_source(SummaryView * summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;
	SourceWindow *srcwin;

	if (!summaryview->selected) return;

	srcwin = source_window_create();
	msginfo = gtk_cmctree_node_get_row_data(ctree, summaryview->selected);
	source_window_show_msg(srcwin, msginfo);
	source_window_show(srcwin);
#ifdef MAEMO
	maemo_window_full_screen_if_needed(GTK_WINDOW(srcwin->window));
	maemo_connect_key_press_to_mainwindow(GTK_WINDOW(srcwin->window));
#endif
}

void summary_reedit(SummaryView *summaryview)
{
	MsgInfo *msginfo;

	if (!summaryview->selected) return;
	if (!summaryview->folder_item) return;
	if (!FOLDER_SHOWS_TO_HDR(summaryview->folder_item))
		return;

	msginfo = gtk_cmctree_node_get_row_data(GTK_CMCTREE(summaryview->ctree),
					      summaryview->selected);
	if (!msginfo) return;

	compose_reedit(msginfo, FALSE);
}

gboolean summary_step(SummaryView *summaryview, GtkScrollType type)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;

	if (summary_is_locked(summaryview)
	&& !GTK_SCTREE(ctree)->selecting_range
	&& summaryview->messageview->mimeview
	&& summaryview->messageview->mimeview->type == MIMEVIEW_TEXT
	&& summaryview->messageview->mimeview->textview->loading) {
		PostponedSelectData *data = g_new0(PostponedSelectData, 1);
		summaryview->messageview->mimeview->textview->stop_loading = TRUE;
		
		data->ctree = ctree;
		data->row = NULL;
		data->node = NULL;
		data->type = type;
		data->summaryview = summaryview;
		debug_print("postponing open of message till end of load\n");
		g_timeout_add(100, summary_select_retry, data);
		return FALSE;
	}
	if (summary_is_locked(summaryview))
		return FALSE;
	if (type == GTK_SCROLL_STEP_FORWARD) {
		node = gtkut_ctree_node_next(ctree, summaryview->selected);
		if (node)
			gtkut_ctree_expand_parent_all(ctree, node);
		else
			return FALSE;
	} else {
		if (summaryview->selected) {
			node = GTK_CMCTREE_NODE_PREV(summaryview->selected);
			if (!node) return FALSE;
		}
	}

	if (messageview_is_visible(summaryview->messageview))
		summaryview->display_msg = TRUE;

	g_signal_emit_by_name(G_OBJECT(ctree), "scroll_vertical", type, 0.0);

	if (GTK_CMCLIST(ctree)->selection)
		gtk_sctree_set_anchor_row
			(GTK_SCTREE(ctree),
			 GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->selection->data));

	return TRUE;
}

gboolean summary_is_list(SummaryView *summaryview)
{
	return (gtk_notebook_get_current_page(
		GTK_NOTEBOOK(summaryview->mainwidget_book)) == 0);
}

void summary_toggle_view(SummaryView *summaryview)
{
	if (prefs_common.layout_mode == SMALL_LAYOUT)
		return;
	if (summary_is_locked(summaryview))
		return;
	if (!messageview_is_visible(summaryview->messageview) &&
	    summaryview->selected && summary_is_list(summaryview))
		summary_display_msg(summaryview,
				    summaryview->selected);
	else
		main_window_toggle_message_view(summaryview->mainwin);
}

static gboolean summary_search_unread_recursive(GtkCMCTree *ctree,
						GtkCMCTreeNode *node)
{
	MsgInfo *msginfo;

	if (node) {
		msginfo = gtk_cmctree_node_get_row_data(ctree, node);
		if (msginfo && MSG_IS_UNREAD(msginfo->flags) && !MSG_IS_IGNORE_THREAD(msginfo->flags))
			return TRUE;
		node = GTK_CMCTREE_ROW(node)->children;
	} else
		node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);

	while (node) {
		if (summary_search_unread_recursive(ctree, node) == TRUE)
			return TRUE;
		node = GTK_CMCTREE_ROW(node)->sibling;
	}

	return FALSE;
}

static gboolean summary_have_unread_children(SummaryView *summaryview,
					     GtkCMCTreeNode *node)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);

	if (!node) return FALSE;

	node = GTK_CMCTREE_ROW(node)->children;

	while (node) {
		if (summary_search_unread_recursive(ctree, node) == TRUE)
			return TRUE;
		node = GTK_CMCTREE_ROW(node)->sibling;
	}
	return FALSE;
}

static void summary_set_row_marks(SummaryView *summaryview, GtkCMCTreeNode *row)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkStyle *style = NULL;
	MsgInfo *msginfo;
	MsgFlags flags;
	gint *col_pos = summaryview->col_pos;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	if (!msginfo) return;

	flags = msginfo->flags;

	gtk_cmctree_node_set_foreground(ctree, row, NULL);

	/* set new/unread column */
	if (MSG_IS_IGNORE_THREAD(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_STATUS],
					  ignorethreadxpm);
	} else if (MSG_IS_WATCH_THREAD(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_STATUS],
					  watchthreadxpm);
	} else if (MSG_IS_SPAM(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_STATUS],
					  spamxpm);
	} else if (MSG_IS_NEW(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_STATUS],
					  newxpm);
	} else if (MSG_IS_UNREAD(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_STATUS],
					  unreadxpm);
	} else if (MSG_IS_REPLIED(flags) && MSG_IS_FORWARDED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_STATUS],
					  repliedandforwardedxpm);
	} else if (MSG_IS_REPLIED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_STATUS],
					  repliedxpm);
	} else if (MSG_IS_FORWARDED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_STATUS],
					  forwardedxpm);
	} else {
		gtk_cmctree_node_set_text(ctree, row, col_pos[S_COL_STATUS],
					"");
	}

	if (prefs_common.bold_unread &&
	    ((MSG_IS_UNREAD(flags) && !MSG_IS_IGNORE_THREAD(flags)) ||
	     (!GTK_CMCTREE_ROW(row)->expanded &&
	      GTK_CMCTREE_ROW(row)->children &&
	      summary_have_unread_children(summaryview, row))))
		style = bold_style;

	/* set mark column */
	if (MSG_IS_DELETED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MARK],
					  deletedxpm);
		if (style)
			style = bold_deleted_style;
		else {
			style = small_deleted_style;
		}
			gtk_cmctree_node_set_foreground
				(ctree, row, &summaryview->color_dim);
	} else if (MSG_IS_MARKED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MARK],
					  markxpm);
	} else if (MSG_IS_MOVE(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MARK],
					  movedxpm);
		if (!msginfo->to_folder ||
		    !folder_has_parent_of_type(msginfo->to_folder, F_TRASH)) {
			if (style)
				style = bold_marked_style;
			else {
				style = small_marked_style;
			}
			gtk_cmctree_node_set_foreground
				(ctree, row, &summaryview->color_marked);
		} else {
			if (style)
				style = bold_deleted_style;
			else {
				style = small_deleted_style;
			}
				gtk_cmctree_node_set_foreground
					(ctree, row, &summaryview->color_dim);
		}
	} else if (MSG_IS_COPY(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MARK],
					  copiedxpm);
		if (style)
			style = bold_marked_style;
		else {
			style = small_marked_style;
		}
			gtk_cmctree_node_set_foreground
                        	(ctree, row, &summaryview->color_marked);
	} else {
		gtk_cmctree_node_set_text(ctree, row, col_pos[S_COL_MARK], "");
	}

	if (MSG_IS_LOCKED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_LOCKED],
					  lockedxpm);
	}
	else {
		gtk_cmctree_node_set_text(ctree, row, col_pos[S_COL_LOCKED], "");
	}

	if (MSG_IS_WITH_ATTACHMENT(flags) && MSG_IS_SIGNED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MIME],
					  clipgpgsignedxpm);
	} else if (MSG_IS_SIGNED(flags)) {
		if (MSG_IS_ENCRYPTED(flags)) {
			gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MIME],
					  keysignxpm);
		} else {
			gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MIME],
						  gpgsignedxpm);
		}
	} else if (MSG_IS_WITH_ATTACHMENT(flags) && MSG_IS_ENCRYPTED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MIME],
					  clipkeyxpm);
	} else if (MSG_IS_ENCRYPTED(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MIME],
					  keyxpm);
	} else if (MSG_IS_WITH_ATTACHMENT(flags)) {
		gtk_cmctree_node_set_pixbuf(ctree, row, col_pos[S_COL_MIME],
					  clipxpm);
	} else {
		gtk_cmctree_node_set_text(ctree, row, col_pos[S_COL_MIME], "");
	}
	if (!style)
		style = small_style;

	gtk_cmctree_node_set_row_style(ctree, row, style);

	if (MSG_GET_COLORLABEL(flags))
		summary_set_colorlabel_color(ctree, row, MSG_GET_COLORLABEL_VALUE(flags));
}

static void summary_mark_row(SummaryView *summaryview, GtkCMCTreeNode *row)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);
	if (MSG_IS_DELETED(msginfo->flags))
		summaryview->deleted--;
	if (MSG_IS_MOVE(msginfo->flags))
		summaryview->moved--;
	if (MSG_IS_COPY(msginfo->flags))
		summaryview->copied--;

	procmsg_msginfo_set_to_folder(msginfo, NULL);
	summary_msginfo_change_flags(msginfo, MSG_MARKED, 0, MSG_DELETED, 
		MSG_MOVE | MSG_COPY | MSG_MOVE_DONE);
	summary_set_row_marks(summaryview, row);
	debug_print("Message %s/%d is marked\n", msginfo->folder->path, msginfo->msgnum);
}

static void summary_lock_row(SummaryView *summaryview, GtkCMCTreeNode *row)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);
	if (MSG_IS_DELETED(msginfo->flags))
		summaryview->deleted--;
	if (MSG_IS_MOVE(msginfo->flags)) {
		summaryview->moved--;
	}
	if (MSG_IS_COPY(msginfo->flags)) {
		summaryview->copied--;
	}
	procmsg_msginfo_set_to_folder(msginfo, NULL);
	summary_msginfo_change_flags(msginfo, MSG_LOCKED, 0, MSG_DELETED, 
		MSG_MOVE | MSG_COPY | MSG_MOVE_DONE);
	
	summary_set_row_marks(summaryview, row);
	debug_print("Message %d is locked\n", msginfo->msgnum);
}

static void summary_unlock_row(SummaryView *summaryview, GtkCMCTreeNode *row)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);
	if (!MSG_IS_LOCKED(msginfo->flags))
		return;
	procmsg_msginfo_set_to_folder(msginfo, NULL);
	summary_msginfo_unset_flags(msginfo, MSG_LOCKED, 0);
	summary_set_row_marks(summaryview, row);
	debug_print("Message %d is unlocked\n", msginfo->msgnum);
}

void summary_mark(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	if (summary_is_locked(summaryview))
		return;
	START_LONG_OPERATION(summaryview, FALSE);
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		summary_mark_row(summaryview, GTK_CMCTREE_NODE(cur->data));
	folder_item_set_batch(summaryview->folder_item, FALSE);
	END_LONG_OPERATION(summaryview);

	summary_status_show(summaryview);
}

static void summary_mark_row_as_read(SummaryView *summaryview,
				     GtkCMCTreeNode *row)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);

	if(!(MSG_IS_NEW(msginfo->flags) || MSG_IS_UNREAD(msginfo->flags)))
		return;

	summary_msginfo_unset_flags(msginfo, MSG_NEW | MSG_UNREAD, 0);
	summary_set_row_marks(summaryview, row);
	debug_print("Message %d is marked as read\n",
		msginfo->msgnum);
}

void summary_mark_as_read(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	if (summary_is_locked(summaryview))
		return;
	START_LONG_OPERATION(summaryview, FALSE);
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		summary_mark_row_as_read(summaryview,
					 GTK_CMCTREE_NODE(cur->data));
	folder_item_set_batch(summaryview->folder_item, FALSE);
	END_LONG_OPERATION(summaryview);
	
	summary_status_show(summaryview);
}

void summary_msgs_lock(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	if (summary_is_locked(summaryview))
		return;
	START_LONG_OPERATION(summaryview, FALSE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		summary_lock_row(summaryview,
					 GTK_CMCTREE_NODE(cur->data));
	END_LONG_OPERATION(summaryview);
	
	summary_status_show(summaryview);
}

void summary_msgs_unlock(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	if (summary_is_locked(summaryview))
		return;
	START_LONG_OPERATION(summaryview, FALSE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		summary_unlock_row(summaryview,
				   GTK_CMCTREE_NODE(cur->data));
	END_LONG_OPERATION(summaryview);
	
	summary_status_show(summaryview);
}

void summary_mark_all_read(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	AlertValue val;
	gboolean froze = FALSE;

	if (prefs_common.ask_mark_all_read) {
		val = alertpanel_full(_("Mark all as read"),
			_("Do you really want to mark all mails in this "
			  "folder as read ?"), GTK_STOCK_NO, _("Mark all as read"), NULL,
			  TRUE, NULL, ALERT_QUESTION, G_ALERTDEFAULT);

		if ((val & ~G_ALERTDISABLE) != G_ALERTALTERNATE)
			return;
		else if (val & G_ALERTDISABLE)
			prefs_common.ask_mark_all_read = FALSE;
	}
	
	if (summary_is_locked(summaryview))
		return;
	START_LONG_OPERATION(summaryview, TRUE);
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list); node != NULL;
	     node = gtkut_ctree_node_next(ctree, node))
		summary_mark_row_as_read(summaryview, node);
	folder_item_set_batch(summaryview->folder_item, FALSE);
	for (node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list); node != NULL;
	     node = gtkut_ctree_node_next(ctree, node)) {
		if (!GTK_CMCTREE_ROW(node)->expanded)
			summary_set_row_marks(summaryview, node);
	}
	END_LONG_OPERATION(summaryview);
	
	summary_status_show(summaryview);
}

void summary_mark_as_spam(SummaryView *summaryview, guint action, GtkWidget *widget)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean is_spam = action;
	GSList *msgs = NULL;
	gboolean immediate_exec = prefs_common.immediate_exec;
	gboolean moved = FALSE;
	gboolean froze = FALSE;


	if (summary_is_locked(summaryview))
		return;

	prefs_common.immediate_exec = FALSE;
	START_LONG_OPERATION(summaryview, FALSE);
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next) {
		GtkCMCTreeNode *row = GTK_CMCTREE_NODE(cur->data);
		MsgInfo *msginfo = gtk_cmctree_node_get_row_data(ctree, row);
		if (msginfo)
			msgs = g_slist_prepend(msgs, msginfo);
	}
	
	if (procmsg_spam_learner_learn(NULL, msgs, is_spam) == 0) {
		for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next) {
			GtkCMCTreeNode *row = GTK_CMCTREE_NODE(cur->data);
			MsgInfo *msginfo = gtk_cmctree_node_get_row_data(ctree, row);
			if (!msginfo)
				continue;
			if (is_spam) {
				summary_msginfo_change_flags(msginfo, MSG_SPAM, 0, MSG_NEW|MSG_UNREAD, 0);
				if (procmsg_spam_get_folder(msginfo) != summaryview->folder_item) {
					summary_move_row_to(summaryview, row,
							procmsg_spam_get_folder(msginfo));
					moved = TRUE;
				}
			} else {
				summary_msginfo_unset_flags(msginfo, MSG_SPAM, 0);
			}
			summaryview->display_msg = prefs_common.always_show_msg;
	
			summary_set_row_marks(summaryview, row);
		}
	} else {
		log_error(LOG_PROTOCOL, _("An error happened while learning.\n"));
	}

	prefs_common.immediate_exec = immediate_exec;
	folder_item_set_batch(summaryview->folder_item, FALSE);
	END_LONG_OPERATION(summaryview);

	if (prefs_common.immediate_exec && moved) {
		summary_execute(summaryview);
	}

	if (!moved && msgs) {
		MsgInfo *msginfo = (MsgInfo *)msgs->data;
		toolbar_set_learn_button
			(summaryview->mainwin->toolbar,
			 MSG_IS_SPAM(msginfo->flags)?LEARN_HAM:LEARN_SPAM);
	}
	g_slist_free(msgs);
	
	summary_status_show(summaryview);	
}


static void summary_mark_row_as_unread(SummaryView *summaryview,
				       GtkCMCTreeNode *row)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);
	if (MSG_IS_DELETED(msginfo->flags)) {
		procmsg_msginfo_set_to_folder(msginfo, NULL);
		summary_msginfo_unset_flags(msginfo, MSG_DELETED, 0);
		summaryview->deleted--;
	}

	summary_msginfo_set_flags(msginfo, MSG_UNREAD, 0);
	debug_print("Message %d is marked as unread\n",
		msginfo->msgnum);

	summary_set_row_marks(summaryview, row);
}

void summary_mark_as_unread(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	if (summary_is_locked(summaryview))
		return;
	START_LONG_OPERATION(summaryview, FALSE);
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; 
		cur = cur->next)
		summary_mark_row_as_unread(summaryview,
					   GTK_CMCTREE_NODE(cur->data));
	folder_item_set_batch(summaryview->folder_item, FALSE);
	END_LONG_OPERATION(summaryview);
	
	summary_status_show(summaryview);
}

static gboolean check_permission(SummaryView *summaryview, MsgInfo * msginfo)
{
	GList * cur;
	gboolean found;

	switch (FOLDER_TYPE(summaryview->folder_item->folder)) {

	case F_NEWS:

		/*
		  security : checks if one the accounts correspond to
		  the author of the post
		*/

		found = FALSE;
		for(cur = account_get_list() ; cur != NULL ; cur = cur->next) {
			PrefsAccount * account;
			gchar * from_name;
			
			account = cur->data;
			if (account->name && *account->name)
				from_name =
					g_strdup_printf("%s <%s>",
							account->name,
							account->address);
			else
				from_name =
					g_strdup_printf("%s",
							account->address);
			
			if (g_utf8_collate(from_name, msginfo->from) == 0) {
				g_free(from_name);
				found = TRUE;
				break;
			}
			g_free(from_name);
		}

		if (!found) {
			alertpanel_error(_("You're not the author of the article.\n"));
		}
		
		return found;

	default:
		return TRUE;
	}
}

static void summary_delete_row(SummaryView *summaryview, GtkCMCTreeNode *row)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);

	if (MSG_IS_LOCKED(msginfo->flags)) return;

	if (MSG_IS_DELETED(msginfo->flags)) return;

	if (MSG_IS_MOVE(msginfo->flags))
		summaryview->moved--;
	if (MSG_IS_COPY(msginfo->flags))
		summaryview->copied--;

	procmsg_msginfo_set_to_folder(msginfo, NULL);
	summary_msginfo_change_flags(msginfo, MSG_DELETED, 0, MSG_MARKED, 
		MSG_MOVE | MSG_COPY | MSG_MOVE_DONE);
	summaryview->deleted++;

	if (!prefs_common.immediate_exec && 
	    !folder_has_parent_of_type(summaryview->folder_item, F_TRASH)) {
		summary_set_row_marks(summaryview, row);
	} else if (summaryview->folder_item->folder->account && !summaryview->folder_item->folder->account->imap_use_trash) {
		summary_set_row_marks(summaryview, row);
	}
	debug_print("Message %s/%d is set to delete\n",
		    msginfo->folder->path, msginfo->msgnum);
}

void summary_cancel(SummaryView *summaryview)
{
	MsgInfo * msginfo;

	msginfo = gtk_cmctree_node_get_row_data(GTK_CMCTREE(summaryview->ctree),
					      summaryview->selected);
	if (!msginfo) return;

	if (!check_permission(summaryview, msginfo))
		return;

	news_cancel_article(summaryview->folder_item->folder, msginfo);
	
	if (summary_is_locked(summaryview)) return;

	summary_lock(summaryview);

	summary_freeze(summaryview);

	summary_update_status(summaryview);
	summary_status_show(summaryview);

	summary_thaw(summaryview);

	summary_unlock(summaryview);
}

void summary_delete(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	FolderItem *item = summaryview->folder_item;
	GList *cur;
	GtkCMCTreeNode *sel_last = NULL;
	GtkCMCTreeNode *node;
	AlertValue aval;
	MsgInfo *msginfo;
	gboolean froze = FALSE;

	if (!item) return;

	if (summary_is_locked(summaryview)) return;

	if (!summaryview->folder_item) return;

	if (!summaryview->folder_item->folder->account || summaryview->folder_item->folder->account->imap_use_trash) {
		if (!prefs_common.live_dangerously) {
			gchar *buf = NULL;
			int num = g_list_length(GTK_CMCLIST(summaryview->ctree)->selection);
			buf = g_strdup_printf(ngettext(
				"Do you really want to delete the selected message?",
				"Do you really want to delete the %d selected messages?", num), 
				num);
			aval = alertpanel(_("Delete message(s)"),
					  buf,
					  GTK_STOCK_CANCEL, "+"GTK_STOCK_DELETE, NULL);
			g_free(buf);
			if (aval != G_ALERTALTERNATE) return;
		}
	}

	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; 
	     cur = cur->next) {
		GtkCMCTreeNode *row = GTK_CMCTREE_NODE(cur->data);
		msginfo = gtk_cmctree_node_get_row_data(ctree, row);
		if (msginfo && msginfo->total_size != 0 && 
		    msginfo->size != (goffset)msginfo->total_size)
			partial_mark_for_delete(msginfo);
	}

	main_window_cursor_wait(summaryview->mainwin);

	/* next code sets current row focus right. We need to find a row
	 * that is not deleted. */
	START_LONG_OPERATION(summaryview, FALSE);
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next) {
		sel_last = GTK_CMCTREE_NODE(cur->data);
		summary_delete_row(summaryview, sel_last);
	}
	folder_item_set_batch(summaryview->folder_item, FALSE);
	END_LONG_OPERATION(summaryview);

	node = summary_find_next_msg(summaryview, sel_last);
	if (!node)
		node = summary_find_prev_msg(summaryview, sel_last);

	summary_select_node(summaryview, node, prefs_common.always_show_msg, TRUE);
	
	if (prefs_common.immediate_exec || folder_has_parent_of_type(item, F_TRASH)) {
		summary_execute(summaryview);
		/* after deleting, the anchor may be at an invalid row
		 * so reset it to the node we found earlier */
		gtk_sctree_set_anchor_row(GTK_SCTREE(ctree), node);
	} else
		summary_status_show(summaryview);

		
	main_window_cursor_normal(summaryview->mainwin);
}

void summary_delete_trash(SummaryView *summaryview)
{
	FolderItem *to_folder = NULL;
	PrefsAccount *ac;
	if (!summaryview->folder_item ||
	    FOLDER_TYPE(summaryview->folder_item->folder) == F_NEWS) return;
	
	if (NULL != (ac = account_find_from_item(summaryview->folder_item)))
		to_folder = account_get_special_folder(ac, F_TRASH);

	if (to_folder == NULL)
		to_folder = summaryview->folder_item->folder->trash;
	
	if (to_folder == NULL || to_folder == summaryview->folder_item
	    || folder_has_parent_of_type(summaryview->folder_item, F_TRASH)
	    || (summaryview->folder_item->folder->account && !summaryview->folder_item->folder->account->imap_use_trash))
		summary_delete(summaryview);
	else
		summary_move_selected_to(summaryview, to_folder);
}


static void summary_unmark_row(SummaryView *summaryview, GtkCMCTreeNode *row)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);
	if (MSG_IS_DELETED(msginfo->flags))
		summaryview->deleted--;
	if (MSG_IS_MOVE(msginfo->flags))
		summaryview->moved--;
	if (MSG_IS_COPY(msginfo->flags))
		summaryview->copied--;

	procmsg_msginfo_set_to_folder(msginfo, NULL);
	summary_msginfo_unset_flags(msginfo, MSG_MARKED | MSG_DELETED, 
		MSG_MOVE | MSG_COPY | MSG_MOVE_DONE);
	summary_set_row_marks(summaryview, row);

	debug_print("Message %s/%d is unmarked\n",
		    msginfo->folder->path, msginfo->msgnum);
}

void summary_unmark(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	if (summary_is_locked(summaryview))
		return;
	START_LONG_OPERATION(summaryview, FALSE);
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		summary_unmark_row(summaryview, GTK_CMCTREE_NODE(cur->data));
	folder_item_set_batch(summaryview->folder_item, FALSE);
	END_LONG_OPERATION(summaryview);
	
	summary_status_show(summaryview);
}

static void summary_move_row_to(SummaryView *summaryview, GtkCMCTreeNode *row,
				FolderItem *to_folder)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	cm_return_if_fail(to_folder != NULL);

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);
	if (MSG_IS_LOCKED(msginfo->flags))
		return;

	procmsg_msginfo_set_to_folder(msginfo, to_folder);
	if (MSG_IS_DELETED(msginfo->flags))
		summaryview->deleted--;
	if (MSG_IS_COPY(msginfo->flags)) {
		summaryview->copied--;
	}
	if (!MSG_IS_MOVE(msginfo->flags)) {
		summary_msginfo_change_flags(msginfo, 0, MSG_MOVE, MSG_DELETED, 
			MSG_COPY | MSG_MOVE_DONE);
		summaryview->moved++;
	} else {
		summary_msginfo_unset_flags(msginfo, MSG_DELETED, MSG_COPY);
	}
	
	if (!prefs_common.immediate_exec) {
		summary_set_row_marks(summaryview, row);
	} else if (msginfo->folder->folder->account && !msginfo->folder->folder->account->imap_use_trash) {
		summary_set_row_marks(summaryview, row);
	}

	debug_print("Message %d is set to move to %s\n",
		    msginfo->msgnum, to_folder->path);
}

void summary_move_selected_to(SummaryView *summaryview, FolderItem *to_folder)
{
	GList *cur;
	GtkCMCTreeNode *sel_last = NULL;
	gboolean froze = FALSE;

	if (!to_folder) return;
	if (!summaryview->folder_item ||
	    FOLDER_TYPE(summaryview->folder_item->folder) == F_NEWS) return;

	if (summary_is_locked(summaryview)) return;

	if (summaryview->folder_item == to_folder) {
		alertpanel_error(_("Destination is same as current folder."));
		return;
	}

	if (to_folder->no_select) {
		alertpanel_error(_("The destination folder can only be used to "
				   "store subfolders."));
		return;
	}

	START_LONG_OPERATION(summaryview, FALSE); 

	for (cur = GTK_CMCLIST(summaryview->ctree)->selection;
	     cur != NULL && cur->data != NULL; cur = cur->next) {
		sel_last = GTK_CMCTREE_NODE(cur->data);
		summary_move_row_to
			(summaryview, GTK_CMCTREE_NODE(cur->data), to_folder);
	}
	END_LONG_OPERATION(summaryview);

	summaryview->display_msg = (prefs_common.always_show_msg == OPENMSG_ALWAYS) ||
		((prefs_common.always_show_msg == OPENMSG_WHEN_VIEW_VISIBLE &&
				messageview_is_visible(summaryview->messageview)));
	
	if (prefs_common.immediate_exec) {
		summary_execute(summaryview);
	} else {
		GtkCMCTreeNode *node = summary_find_next_msg(summaryview, sel_last);
		if (!node)
			node = summary_find_prev_msg(summaryview, sel_last);
		summary_select_node(summaryview, node, summaryview->display_msg, TRUE);
		summary_status_show(summaryview);
	}
	
	if (!summaryview->selected) { /* this was the last message */
		GtkCMCTreeNode *node = gtk_cmctree_node_nth (GTK_CMCTREE(summaryview->ctree), 
							 GTK_CMCLIST(summaryview->ctree)->rows - 1);
		if (node)
			summary_select_node(summaryview, node, summaryview->display_msg, TRUE);
	}

}

void summary_move_to(SummaryView *summaryview)
{
	FolderItem *to_folder;

	if (!summaryview->folder_item ||
	    FOLDER_TYPE(summaryview->folder_item->folder) == F_NEWS) return;

	to_folder = foldersel_folder_sel(summaryview->folder_item->folder,
					 FOLDER_SEL_MOVE, NULL, FALSE);
	summary_move_selected_to(summaryview, to_folder);
}

static void summary_copy_row_to(SummaryView *summaryview, GtkCMCTreeNode *row,
				FolderItem *to_folder)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	cm_return_if_fail(to_folder != NULL);

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);
	procmsg_msginfo_set_to_folder(msginfo, to_folder);
	if (MSG_IS_DELETED(msginfo->flags))
		summaryview->deleted--;
	if (MSG_IS_MOVE(msginfo->flags)) {
		summaryview->moved--;
	}
	
	if (!MSG_IS_COPY(msginfo->flags)) {
		summary_msginfo_change_flags(msginfo, 0, MSG_COPY, MSG_DELETED, 
			MSG_MOVE | MSG_MOVE_DONE);
		summaryview->copied++;
	} else {
		summary_msginfo_unset_flags(msginfo, MSG_DELETED, MSG_MOVE);
	}
	if (!prefs_common.immediate_exec) {
		summary_set_row_marks(summaryview, row);
	}

	debug_print("Message %d is set to copy to %s\n",
		    msginfo->msgnum, to_folder->path);
}

void summary_copy_selected_to(SummaryView *summaryview, FolderItem *to_folder)
{
	GList *cur;
	gboolean froze = FALSE;

	if (!to_folder) return;
	if (!summaryview->folder_item) return;

	if (summary_is_locked(summaryview)) return;

	if (summaryview->folder_item == to_folder) {
		alertpanel_error
			(_("Destination to copy is same as current folder."));
		return;
	}

	if (to_folder->no_select) {
		alertpanel_error(_("The destination folder can only be used to "
				   "store subfolders."));
		return;
	}

	START_LONG_OPERATION(summaryview, FALSE);

	for (cur = GTK_CMCLIST(summaryview->ctree)->selection;
	     cur != NULL && cur->data != NULL; cur = cur->next)
		summary_copy_row_to
			(summaryview, GTK_CMCTREE_NODE(cur->data), to_folder);

	END_LONG_OPERATION(summaryview);

	if (prefs_common.immediate_exec)
		summary_execute(summaryview);
	else {
		summary_status_show(summaryview);
	}
}

void summary_copy_to(SummaryView *summaryview)
{
	FolderItem *to_folder;

	if (!summaryview->folder_item) return;

	to_folder = foldersel_folder_sel(summaryview->folder_item->folder,
					 FOLDER_SEL_COPY, NULL, FALSE);
	summary_copy_selected_to(summaryview, to_folder);
}

void summary_add_address(SummaryView *summaryview)
{
	MsgInfo *msginfo, *full_msginfo;
	gchar *from;
	GtkWidget *image = NULL;
	GdkPixbuf *picture = NULL;

	msginfo = gtk_cmctree_node_get_row_data(GTK_CMCTREE(summaryview->ctree),
					      summaryview->selected);
	if (!msginfo || !msginfo->from) 
		return;

	Xstrdup_a(from, msginfo->from, return);
	eliminate_address_comment(from);
	extract_address(from);
	
	full_msginfo = procmsg_msginfo_get_full_info(msginfo);
	if (full_msginfo &&
	    full_msginfo->extradata &&
	    full_msginfo->extradata->face) {
		image = face_get_from_header(full_msginfo->extradata->face);
	} 
#if HAVE_LIBCOMPFACE
	else if (full_msginfo &&
	         full_msginfo->extradata &&
		 full_msginfo->extradata->xface) {
		image = xface_get_from_header(full_msginfo->extradata->xface,
				&summaryview->ctree->style->white,
				summaryview->mainwin->window->window);	
	}
#endif
	procmsg_msginfo_free(full_msginfo);
	if (image)
		picture = gtk_image_get_pixbuf(GTK_IMAGE(image));

#ifndef USE_NEW_ADDRBOOK
	addressbook_add_contact(msginfo->fromname, from, NULL, picture);
#else
	if (addressadd_selection(msginfo->fromname, from, NULL, picture)) {
		debug_print( "addressbook_add_contact - added\n" );
	}
#endif
	if (image)
		gtk_widget_destroy(image);
}

void summary_select_all(SummaryView *summaryview)
{
	if (!summaryview->folder_item) return;

	summary_lock(summaryview);
	gtk_cmclist_select_all(GTK_CMCLIST(summaryview->ctree));
	summary_unlock(summaryview);
	summary_status_show(summaryview);
}

void summary_unselect_all(SummaryView *summaryview)
{
	summary_lock(summaryview);
	gtk_sctree_unselect_all(GTK_SCTREE(summaryview->ctree));
	summary_unlock(summaryview);
	summary_status_show(summaryview);
}

void summary_select_thread(SummaryView *summaryview, gboolean delete_thread)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node = NULL;
	gboolean froze = FALSE;
	GList *cur = NULL;
	GList *copy = NULL;
	if (!GTK_CMCLIST(summaryview->ctree)->selection) 
		return;


	START_LONG_OPERATION(summaryview, FALSE);
	copy = g_list_copy(GTK_CMCLIST(summaryview->ctree)->selection);
	for (cur = copy; cur != NULL && cur->data != NULL;
	     cur = cur->next) {
		node = GTK_CMCTREE_NODE(cur->data);
		if (!node)
			continue;
		while (GTK_CMCTREE_ROW(node)->parent != NULL)
			node = GTK_CMCTREE_ROW(node)->parent;

		gtk_cmctree_select_recursive(ctree, node);
	}
	g_list_free(copy);
	END_LONG_OPERATION(summaryview);

	if (delete_thread) {
		if (FOLDER_TYPE(summaryview->folder_item->folder) == F_NEWS)
			summary_delete(summaryview);
		else
			summary_delete_trash(summaryview);
	}
	summary_status_show(summaryview);
}

void summary_save_as(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;
	gchar *filename = NULL;
	gchar *src, *dest;
	gchar *tmp;

	AlertValue aval = 0;

	if (!summaryview->selected) return;
	msginfo = gtk_cmctree_node_get_row_data(ctree, summaryview->selected);
	if (!msginfo) return;

	if (msginfo->subject) {
		Xstrdup_a(filename, msginfo->subject, return);
		subst_for_filename(filename);
	}

	manage_window_focus_in(summaryview->window, NULL, NULL);

	if (filename && !g_utf8_validate(filename, -1, NULL)) {
		gchar *oldstr = filename;
		filename = conv_codeset_strdup(filename,
					       conv_get_locale_charset_str(),
					       CS_UTF_8);
		if (!filename) {
			g_warning("summary_save_as(): failed to convert character set.");
			filename = g_strdup(oldstr);
		}
		dest = filesel_select_file_save(_("Save as"), filename);
		g_free(filename);
	} else
		dest = filesel_select_file_save(_("Save as"), filename);
	filename = NULL;
	if (!dest) return;
	if (is_file_exist(dest)) {
		aval = alertpanel(_("Append or Overwrite"),
				  _("Append or overwrite existing file?"),
				  _("_Append"), _("_Overwrite"),
				  GTK_STOCK_CANCEL);
		if (aval != 0 && aval != 1)
			return;
	}

	src = procmsg_get_message_file(msginfo);
	tmp = g_path_get_basename(dest);

	if ( aval==0 ) { /* append */
		if (append_file(src, dest, TRUE) < 0) 
			alertpanel_error(_("Couldn't save the file '%s'."), tmp);
	} else { /* overwrite */
		if (copy_file(src, dest, TRUE) < 0)
			alertpanel_error(_("Couldn't save the file '%s'."), tmp);
	}
	g_free(src);
	
	/*
	 * If two or more msgs are selected,
	 * append them to the output file.
	 */
	if (GTK_CMCLIST(ctree)->selection->next) {
		GList *item;
		for (item = GTK_CMCLIST(ctree)->selection->next; item != NULL; item=item->next) {
			msginfo = gtk_cmctree_node_get_row_data(ctree, GTK_CMCTREE_NODE(item->data));
			if (!msginfo) break;
			src = procmsg_get_message_file(msginfo);
			if (append_file(src, dest, TRUE) < 0)
				alertpanel_error(_("Couldn't save the file '%s'."), tmp);
		}
		g_free(src);
	}
	g_free(dest);
	g_free(tmp);
}

void summary_print(SummaryView *summaryview)
{
	GtkCMCList *clist = GTK_CMCLIST(summaryview->ctree);
	GList *cur;
	gchar *msg = g_strdup_printf(_("You are about to print %d "
				       "messages, one by one. Do you "
				       "want to continue?"), 
				       g_list_length(clist->selection));
	if (g_list_length(clist->selection) > 9
	&&  alertpanel(_("Warning"), msg, GTK_STOCK_CANCEL, "+" GTK_STOCK_YES, NULL)
	    != G_ALERTALTERNATE) {
		g_free(msg);
		return;
	}
	g_free(msg);

	if (clist->selection == NULL) return;
	for (cur = clist->selection; 
	     cur != NULL && cur->data != NULL; 
	     cur = cur->next) {
		GtkCMCTreeNode *node = GTK_CMCTREE_NODE(cur->data);
		MsgInfo *msginfo = gtk_cmctree_node_get_row_data(
					GTK_CMCTREE(summaryview->ctree),
					node);
		gint sel_start = -1, sel_end = -1, partnum = 0;
		
		if (node == summaryview->displayed) {
			partnum = mimeview_get_selected_part_num(summaryview->messageview->mimeview);
			textview_get_selection_offsets(summaryview->messageview->mimeview->textview,
				&sel_start, &sel_end);
		} 
		messageview_print(msginfo, summaryview->messageview->all_headers,
			sel_start, sel_end, partnum);
	}
}

gboolean summary_execute(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCList *clist = GTK_CMCLIST(summaryview->ctree);
	GtkCMCTreeNode *node, *next;
	GtkCMCTreeNode *new_selected = NULL;
	gint move_val = -1;

	if (!summaryview->folder_item) return FALSE;

	if (summary_is_locked(summaryview)) return FALSE;
	summary_lock(summaryview);

	summary_freeze(summaryview);

	main_window_cursor_wait(summaryview->mainwin);

	if (summaryview->threaded)
		summary_unthread_for_exec(summaryview);

	folder_item_update_freeze();
	move_val = summary_execute_move(summaryview);
	summary_execute_copy(summaryview);
	summary_execute_delete(summaryview);
	
	node = GTK_CMCTREE_NODE(clist->row_list);
	for (; node != NULL; node = next) {
		next = gtkut_ctree_node_next(ctree, node);
		if (gtk_cmctree_node_get_row_data(ctree, node) != NULL) continue;

		if (node == summaryview->displayed) {
			messageview_clear(summaryview->messageview);
			summaryview->displayed = NULL;
		}
		if (GTK_CMCTREE_ROW(node)->children != NULL) {
			next = NULL;
			if (GTK_CMCTREE_ROW(node)->sibling) {
				next = GTK_CMCTREE_ROW(node)->sibling;
			} else {
				GtkCMCTreeNode *parent = NULL;
				for (parent = GTK_CMCTREE_ROW(node)->parent; parent != NULL;
				     parent = GTK_CMCTREE_ROW(parent)->parent) {
					if (GTK_CMCTREE_ROW(parent)->sibling) {
						next = GTK_CMCTREE_ROW(parent)->sibling;
					}
				}
			}
		}

		if (!new_selected &&
		    gtkut_ctree_node_is_selected(ctree, node)) {
			summary_unselect_all(summaryview);
			new_selected = summary_find_next_msg(summaryview, node);
			if (!new_selected)
				new_selected = summary_find_prev_msg
					(summaryview, node);
		}

		gtk_sctree_remove_node((GtkSCTree *)ctree, node);
	}

	folder_item_update_thaw();

	if (new_selected) {
		summary_unlock(summaryview);
		gtk_sctree_select
			(GTK_SCTREE(ctree), new_selected);
		summary_lock(summaryview);
	}

	if (summaryview->threaded) {
		summary_thread_build(summaryview);
		summary_thread_init(summaryview);
	}

	summary_thaw(summaryview);

	summaryview->selected = clist->selection ?
		GTK_CMCTREE_NODE(clist->selection->data) : NULL;

	if (!GTK_CMCLIST(summaryview->ctree)->row_list) {
		menu_set_insensitive_all
			(GTK_MENU_SHELL(summaryview->popupmenu));
		gtk_widget_grab_focus(summaryview->folderview->ctree);
	} else {
		menu_set_sensitive_all(GTK_MENU_SHELL(summaryview->popupmenu), TRUE);
		gtk_widget_grab_focus(summaryview->ctree);
	}
	summary_update_status(summaryview);
	summary_status_show(summaryview);

	gtk_cmctree_node_moveto(ctree, summaryview->selected, 0, 0.5, 0);

	summary_unlock(summaryview);

	main_window_cursor_normal(summaryview->mainwin);

	if (move_val < 0) 
		summary_show(summaryview, summaryview->folder_item);
	return TRUE;
}

gboolean summary_expunge(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCList *clist = GTK_CMCLIST(summaryview->ctree);
	GtkCMCTreeNode *node, *next;
	GtkCMCTreeNode *new_selected = NULL;

	if (!summaryview->folder_item) return FALSE;

	if (summary_is_locked(summaryview)) return FALSE;

	summary_lock(summaryview);

	summary_freeze(summaryview);

	main_window_cursor_wait(summaryview->mainwin);

	if (summaryview->threaded)
		summary_unthread_for_exec(summaryview);

	folder_item_update_freeze();
	summary_execute_expunge(summaryview);
	
	node = GTK_CMCTREE_NODE(clist->row_list);
	for (; node != NULL; node = next) {
		next = gtkut_ctree_node_next(ctree, node);
		if (gtk_cmctree_node_get_row_data(ctree, node) != NULL) continue;

		if (node == summaryview->displayed) {
			messageview_clear(summaryview->messageview);
			summaryview->displayed = NULL;
		}
		if (GTK_CMCTREE_ROW(node)->children != NULL) {
			next = NULL;
			if (GTK_CMCTREE_ROW(node)->sibling) {
				next = GTK_CMCTREE_ROW(node)->sibling;
			} else {
				GtkCMCTreeNode *parent = NULL;
				for (parent = GTK_CMCTREE_ROW(node)->parent; parent != NULL;
				     parent = GTK_CMCTREE_ROW(parent)->parent) {
					if (GTK_CMCTREE_ROW(parent)->sibling) {
						next = GTK_CMCTREE_ROW(parent)->sibling;
					}
				}
			}
		}

		if (!new_selected &&
		    gtkut_ctree_node_is_selected(ctree, node)) {
			summary_unselect_all(summaryview);
			new_selected = summary_find_next_msg(summaryview, node);
			if (!new_selected)
				new_selected = summary_find_prev_msg
					(summaryview, node);
		}

		gtk_sctree_remove_node((GtkSCTree *)ctree, node);
	}

	folder_item_update_thaw();

	if (new_selected) {
		summary_unlock(summaryview);
		gtk_sctree_select
			(GTK_SCTREE(ctree), new_selected);
		summary_lock(summaryview);
	}

	if (summaryview->threaded) {
		summary_thread_build(summaryview);
		summary_thread_init(summaryview);
	}

	summary_thaw(summaryview);

	summaryview->selected = clist->selection ?
		GTK_CMCTREE_NODE(clist->selection->data) : NULL;

	if (!GTK_CMCLIST(summaryview->ctree)->row_list) {
		menu_set_insensitive_all
			(GTK_MENU_SHELL(summaryview->popupmenu));
		gtk_widget_grab_focus(summaryview->folderview->ctree);
	} else {
		menu_set_sensitive_all(GTK_MENU_SHELL(summaryview->popupmenu), TRUE);
		gtk_widget_grab_focus(summaryview->ctree);
	}

	summary_update_status(summaryview);
	summary_status_show(summaryview);

	gtk_cmctree_node_moveto(ctree, summaryview->selected, 0, 0.5, 0);

	summary_unlock(summaryview);

	main_window_cursor_normal(summaryview->mainwin);

	return TRUE;
}

static void summary_set_deleted_func(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				      gpointer data)
{
	SummaryView *summaryview = data;
	MsgInfo *msginfo;

	msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(node);

	if (msginfo && MSG_IS_MOVE(msginfo->flags)) {
		msginfo->flags.tmp_flags &= ~ MSG_MOVE;
		msginfo->flags.perm_flags |= MSG_DELETED;
		summary_set_row_marks(summaryview, node);
		summaryview->moved--;
		summaryview->deleted++;
	}
}

static gint summary_execute_move(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GSList *cur;
	gint val = -1;
	/* search moving messages and execute */
	
	gtk_cmctree_pre_recursive(ctree, NULL, summary_execute_move_func,
			summaryview);

	if (summaryview->mlist) {
		hooks_unregister_hook(MSGINFO_UPDATE_HOOKLIST,
			summaryview->msginfo_update_callback_id);
		val = procmsg_move_messages(summaryview->mlist);
		summaryview->msginfo_update_callback_id =
		hooks_register_hook(MSGINFO_UPDATE_HOOKLIST, 
			summary_update_msg, (gpointer) summaryview);

		if (!summaryview->folder_item->folder->account || summaryview->folder_item->folder->account->imap_use_trash) {
			for (cur = summaryview->mlist; cur != NULL && cur->data != NULL; cur = cur->next)
				procmsg_msginfo_free((MsgInfo *)cur->data);
		} 
		if (summaryview->folder_item->folder->account && !summaryview->folder_item->folder->account->imap_use_trash) {
			gtk_cmctree_pre_recursive(ctree, NULL, summary_set_deleted_func,
					summaryview);
		}
		g_slist_free(summaryview->mlist);
		summaryview->mlist = NULL;
		return val;
	}
	return 0;
}

static void summary_execute_move_func(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				      gpointer data)
{
	SummaryView *summaryview = data;
	MsgInfo *msginfo;

	msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(node);

	if (msginfo && MSG_IS_MOVE(msginfo->flags) && msginfo->to_folder) {
		summaryview->mlist =
			g_slist_prepend(summaryview->mlist, msginfo);
		if (!summaryview->folder_item->folder->account || 
		     summaryview->folder_item->folder->account->imap_use_trash) {
			gtk_cmctree_node_set_row_data(ctree, node, NULL);

			if (msginfo->msgid && *msginfo->msgid &&
			    node == g_hash_table_lookup(summaryview->msgid_table,
							msginfo->msgid))
				g_hash_table_remove(summaryview->msgid_table,
						    msginfo->msgid);
			if (prefs_common.thread_by_subject &&
			    msginfo->subject && *msginfo->subject && 
			    node == subject_table_lookup(summaryview->subject_table,
							 msginfo->subject)) {
				subject_table_remove(summaryview->subject_table,
						     msginfo->subject);
			}
		}
	}
}

static void summary_execute_copy(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);

	/* search copying messages and execute */
	hooks_unregister_hook(MSGINFO_UPDATE_HOOKLIST,
		summaryview->msginfo_update_callback_id);
	gtk_cmctree_pre_recursive(ctree, NULL, summary_execute_copy_func,
				summaryview);

	if (summaryview->mlist) {
		summaryview->mlist = g_slist_reverse(summaryview->mlist);
		procmsg_copy_messages(summaryview->mlist);

		g_slist_free(summaryview->mlist);
		summaryview->mlist = NULL;
	}
	summaryview->msginfo_update_callback_id =
		hooks_register_hook(MSGINFO_UPDATE_HOOKLIST, 
			summary_update_msg, (gpointer) summaryview);
}

static void summary_execute_copy_func(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				      gpointer data)
{
	SummaryView *summaryview = data;
	MsgInfo *msginfo;

	msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(node);

	if (msginfo && MSG_IS_COPY(msginfo->flags) && msginfo->to_folder) {
		summaryview->mlist =
			g_slist_prepend(summaryview->mlist, msginfo);

		summary_msginfo_unset_flags(msginfo, 0, MSG_COPY);
		summary_set_row_marks(summaryview, node);
	}
}

static void summary_execute_delete(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GSList *cur;

	/* search deleting messages and execute */
	if (!summaryview->folder_item->folder->account || summaryview->folder_item->folder->account->imap_use_trash) {
		gtk_cmctree_pre_recursive
			(ctree, NULL, summary_execute_delete_func, summaryview);
	}
	if (!summaryview->mlist) return;

	hooks_unregister_hook(MSGINFO_UPDATE_HOOKLIST,
		summaryview->msginfo_update_callback_id);

	folder_item_remove_msgs(summaryview->folder_item,
				summaryview->mlist);

	summaryview->msginfo_update_callback_id =
		hooks_register_hook(MSGINFO_UPDATE_HOOKLIST, 
			summary_update_msg, (gpointer) summaryview);
	if (!summaryview->folder_item->folder->account || summaryview->folder_item->folder->account->imap_use_trash) {
		for (cur = summaryview->mlist; cur != NULL && cur->data != NULL; cur = cur->next)
			procmsg_msginfo_free((MsgInfo *)cur->data);
	}
	g_slist_free(summaryview->mlist);
	summaryview->mlist = NULL;
}

static void summary_execute_delete_func(GtkCMCTree *ctree, GtkCMCTreeNode *node,
					gpointer data)
{
	SummaryView *summaryview = data;
	MsgInfo *msginfo;

	msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(node);

	if (msginfo && MSG_IS_DELETED(msginfo->flags)) {
		summaryview->mlist =
			g_slist_prepend(summaryview->mlist, msginfo);
		gtk_cmctree_node_set_row_data(ctree, node, NULL);

		if (msginfo->msgid && *msginfo->msgid &&
		    node == g_hash_table_lookup(summaryview->msgid_table,
						msginfo->msgid)) {
			g_hash_table_remove(summaryview->msgid_table,
					    msginfo->msgid);
		}	
		if (prefs_common.thread_by_subject &&
		    msginfo->subject && *msginfo->subject && 
		    node == subject_table_lookup(summaryview->subject_table,
						 msginfo->subject)) {
			subject_table_remove(summaryview->subject_table,
					     msginfo->subject);
		}					    
	}
}

static void summary_execute_expunge_func(GtkCMCTree *ctree, GtkCMCTreeNode *node,
					gpointer data)
{
	SummaryView *summaryview = data;
	MsgInfo *msginfo;

	msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(node);

	if (msginfo && MSG_IS_DELETED(msginfo->flags)) {
		summaryview->mlist =
			g_slist_prepend(summaryview->mlist, msginfo);
		gtk_cmctree_node_set_row_data(ctree, node, NULL);

		if (msginfo->msgid && *msginfo->msgid &&
		    node == g_hash_table_lookup(summaryview->msgid_table,
						msginfo->msgid)) {
			g_hash_table_remove(summaryview->msgid_table,
					    msginfo->msgid);
		}	
		if (prefs_common.thread_by_subject &&
		    msginfo->subject && *msginfo->subject && 
		    node == subject_table_lookup(summaryview->subject_table,
						 msginfo->subject)) {
			subject_table_remove(summaryview->subject_table,
					     msginfo->subject);
		}					    
	}
}

static void summary_execute_expunge(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GSList *cur;

	gtk_cmctree_pre_recursive
		(ctree, NULL, summary_execute_expunge_func, summaryview);

	hooks_unregister_hook(MSGINFO_UPDATE_HOOKLIST,
		summaryview->msginfo_update_callback_id);

	folder_item_expunge(summaryview->folder_item);

	summaryview->msginfo_update_callback_id =
		hooks_register_hook(MSGINFO_UPDATE_HOOKLIST, 
			summary_update_msg, (gpointer) summaryview);
	for (cur = summaryview->mlist; cur != NULL && cur->data != NULL; cur = cur->next)
		procmsg_msginfo_free((MsgInfo *)cur->data);

	g_slist_free(summaryview->mlist);
	summaryview->mlist = NULL;
}

/* thread functions */

static void summary_thread_build(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node;
	GtkCMCTreeNode *next;
	GtkCMCTreeNode *parent;
	MsgInfo *msginfo;
        GSList *reflist;

	summary_lock(summaryview);

	debug_print("Building threads...");
	STATUSBAR_PUSH(summaryview->mainwin, _("Building threads..."));
	main_window_cursor_wait(summaryview->mainwin);

	g_signal_handlers_block_by_func(G_OBJECT(ctree),
				       G_CALLBACK(summary_tree_expanded), summaryview);
	summary_freeze(summaryview);

	node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
	while (node) {
		next = GTK_CMCTREE_ROW(node)->sibling;

		msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(node);

		parent = NULL;

		if (msginfo && msginfo->inreplyto) {
			parent = g_hash_table_lookup(summaryview->msgid_table,
						     msginfo->inreplyto);
                                                     
			if (!parent && msginfo->references) {
				for (reflist = msginfo->references;
				     reflist != NULL; reflist = reflist->next)
					if ((parent = g_hash_table_lookup
						(summaryview->msgid_table,
						 reflist->data)))
						break;
			}
		}

		if (msginfo && prefs_common.thread_by_subject && parent == NULL) {
			parent = subject_table_lookup
				(summaryview->subject_table,
				 msginfo->subject);
		}

		if (parent && parent != node && parent != GTK_CMCTREE_ROW(node)->parent) {
			gtk_cmctree_move(ctree, node, parent, NULL);
		}

		node = next;
	}

	gtkut_ctree_set_focus_row(ctree, summaryview->selected);

	summary_thaw(summaryview);
	g_signal_handlers_unblock_by_func(G_OBJECT(ctree),
					 G_CALLBACK(summary_tree_expanded), summaryview);

	debug_print("done.\n");
	STATUSBAR_POP(summaryview->mainwin);
	main_window_cursor_normal(summaryview->mainwin);

	summaryview->threaded = TRUE;

	summary_unlock(summaryview);
}

static void summary_thread_init(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
	GtkCMCTreeNode *next;
	START_TIMING("");
	if (!summaryview->thread_collapsed) {
		g_signal_handlers_block_by_func(G_OBJECT(ctree),
				       G_CALLBACK(summary_tree_expanded), summaryview);
		while (node) {
			next = GTK_CMCTREE_ROW(node)->sibling;
			if (GTK_CMCTREE_ROW(node)->children)
				gtk_cmctree_expand_recursive(ctree, node);
			node = next;
		}
		g_signal_handlers_unblock_by_func(G_OBJECT(ctree),
				       G_CALLBACK(summary_tree_expanded), summaryview);
	} 
	END_TIMING();
}

static void summary_unthread_for_exec(SummaryView *summaryview)
{
	GtkCMCTreeNode *node;
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	gboolean froze = FALSE;

	debug_print("Unthreading for execution...");

	START_LONG_OPERATION(summaryview, TRUE);
	for (node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
	     node != NULL; node = GTK_CMCTREE_NODE_NEXT(node)) {
		summary_unthread_for_exec_func(ctree, node, summaryview);
	}

	END_LONG_OPERATION(summaryview);

	debug_print("done.\n");
}

static void summary_unthread_for_exec_func(GtkCMCTree *ctree, GtkCMCTreeNode *node,
					   gpointer data)
{
	MsgInfo *msginfo;
	GtkCMCTreeNode *top_parent;
	GtkCMCTreeNode *child;
	GtkCMCTreeNode *sibling;
	SummaryView * summaryview = (SummaryView *)data;
	msginfo = GTKUT_CTREE_NODE_GET_ROW_DATA(node);

	if (!msginfo ||
	    (!MSG_IS_MOVE(msginfo->flags) &&
	     !MSG_IS_DELETED(msginfo->flags)))
		return;
	child = GTK_CMCTREE_ROW(node)->children;
	if (!child) return;

	if (node == summaryview->selected)
		summaryview->selected = NULL;
	if (node == summaryview->displayed)
		summaryview->displayed = NULL;

	for (top_parent = node;
	     GTK_CMCTREE_ROW(top_parent)->parent != NULL;
	     top_parent = GTK_CMCTREE_ROW(top_parent)->parent)
		;
	sibling = GTK_CMCTREE_ROW(top_parent)->sibling;

	GTK_SCTREE(ctree)->sorting = TRUE;
	while (child != NULL) {
		GtkCMCTreeNode *next_child;
		MsgInfo *cinfo = GTKUT_CTREE_NODE_GET_ROW_DATA(child);
		
		next_child = GTK_CMCTREE_ROW(child)->sibling;
		
		if (!MSG_IS_MOVE(cinfo->flags) && !MSG_IS_DELETED(cinfo->flags)) {
			gtk_cmctree_move(ctree, child, 
				NULL, 
				sibling); 
		} else {
			if (child == summaryview->displayed) {
				messageview_clear(summaryview->messageview);
				summaryview->displayed = NULL;
			}
			if (child == summaryview->selected) {
				messageview_clear(summaryview->messageview);
				summaryview->selected = NULL;
			}
		}
		child = next_child;
	}
	GTK_SCTREE(ctree)->sorting = FALSE;
}

void summary_expand_threads(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
	GtkCMCTreeNode *focus_node = GTK_CMCTREE_NODE (g_list_nth (GTK_CMCLIST(ctree)->row_list, GTK_CMCLIST(ctree)->focus_row));

	g_signal_handlers_block_by_func(G_OBJECT(ctree),
				       G_CALLBACK(summary_tree_expanded), summaryview);
	summary_freeze(summaryview);
	GTK_SCTREE(ctree)->sorting = TRUE;

	while (node) {
		if (GTK_CMCTREE_ROW(node)->children) {
			gtk_cmctree_expand(ctree, node);
			summary_set_row_marks(summaryview, node);
		}
		node = GTK_CMCTREE_NODE_NEXT(node);
	}

	GTK_SCTREE(ctree)->sorting = FALSE;
	if (focus_node) {
		GTK_CMCLIST(ctree)->focus_row = g_list_position (GTK_CMCLIST(ctree)->row_list,(GList *)focus_node);
	}
	summary_thaw(summaryview);

	g_signal_handlers_unblock_by_func(G_OBJECT(ctree),
					 G_CALLBACK(summary_tree_expanded), summaryview);

	summaryview->thread_collapsed = FALSE;

	gtk_cmctree_node_moveto(ctree, summaryview->selected, 0, 0.5, 0);
}

void summary_collapse_threads(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GtkCMCTreeNode *node = NULL;
	GtkCMCTreeNode *focus_node = GTK_CMCTREE_NODE (g_list_nth (GTK_CMCLIST(ctree)->row_list, GTK_CMCLIST(ctree)->focus_row));

	g_signal_handlers_block_by_func(G_OBJECT(ctree),
				       G_CALLBACK(summary_tree_collapsed), summaryview);
	summary_freeze(summaryview);
	GTK_SCTREE(ctree)->sorting = TRUE;

	node = focus_node;
	while (node && GTK_CMCTREE_ROW(node)->parent) {
		focus_node = node = GTK_CMCTREE_ROW(node)->parent;
	}
	gtk_sctree_select(GTK_SCTREE(ctree), focus_node);
	node = GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->row_list);
	while (node) {
		if (GTK_CMCTREE_ROW(node)->children) {
			gtk_cmctree_collapse(ctree, node);
			summary_set_row_marks(summaryview, node);
		}
		node = GTK_CMCTREE_ROW(node)->sibling;
	}

	GTK_SCTREE(ctree)->sorting = FALSE;
	if (focus_node) {
		GTK_CMCLIST(ctree)->focus_row = g_list_position (GTK_CMCLIST(ctree)->row_list,(GList *)focus_node);
	}
	GTK_SCTREE(ctree)->anchor_row =
			gtk_cmctree_node_nth(ctree, GTK_CMCLIST(ctree)->focus_row);
	summary_thaw(summaryview);
	g_signal_handlers_unblock_by_func(G_OBJECT(ctree),
					 G_CALLBACK(summary_tree_collapsed), summaryview);
	
	summaryview->thread_collapsed = TRUE;

	gtk_cmctree_node_moveto(ctree, summaryview->selected, 0, 0.5, 0);
}

static void account_rules_radio_button_toggled_cb(GtkToggleButton *btn, gpointer data)
{
	prefs_common.apply_per_account_filtering_rules = GPOINTER_TO_INT(data);
}

static gboolean summary_filter_get_mode(void)
/* ask what to do w/ them: skip them, apply them regardless to the account,
   use the current account */
{
	/* TODO: eventually also propose to use the current folder's default account,
	   if it is set */
	/* TODO: eventually allow to select the account to use from a optmenu */

	GtkWidget *vbox;
	GtkWidget *account_rules_skip;
	GtkWidget *account_rules_force;
	GtkWidget *account_rules_user_current;
	AlertValue val;

	vbox = gtk_vbox_new (FALSE, 0);

	account_rules_skip = gtk_radio_button_new_with_label
							(NULL, _("Skip these rules"));
	account_rules_force = gtk_radio_button_new_with_label_from_widget
							(GTK_RADIO_BUTTON(account_rules_skip),
							_("Apply these rules regardless of the account they belong to"));
	account_rules_user_current = gtk_radio_button_new_with_label_from_widget
							(GTK_RADIO_BUTTON(account_rules_skip),
							_("Apply these rules if they apply to the current account"));
	gtk_box_pack_start (GTK_BOX (vbox), account_rules_skip, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), account_rules_force, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), account_rules_user_current, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(account_rules_skip), "toggled",
			G_CALLBACK(account_rules_radio_button_toggled_cb),
			GINT_TO_POINTER(FILTERING_ACCOUNT_RULES_SKIP));
	g_signal_connect(G_OBJECT(account_rules_force), "toggled",
			G_CALLBACK(account_rules_radio_button_toggled_cb),
			GINT_TO_POINTER(FILTERING_ACCOUNT_RULES_FORCE));
	g_signal_connect(G_OBJECT(account_rules_user_current), "toggled",
			G_CALLBACK(account_rules_radio_button_toggled_cb),
			GINT_TO_POINTER(FILTERING_ACCOUNT_RULES_USE_CURRENT));
	switch (prefs_common.apply_per_account_filtering_rules) {
	case FILTERING_ACCOUNT_RULES_SKIP:
		gtk_toggle_button_set_active(
				GTK_TOGGLE_BUTTON(account_rules_skip), TRUE);
		break;
	case FILTERING_ACCOUNT_RULES_FORCE:
		gtk_toggle_button_set_active(
				GTK_TOGGLE_BUTTON(account_rules_force), TRUE);
		break;
	case FILTERING_ACCOUNT_RULES_USE_CURRENT:
		gtk_toggle_button_set_active(
				GTK_TOGGLE_BUTTON(account_rules_user_current), TRUE);
		break;
	}

	val = alertpanel_with_widget(
			_("Filtering"),
			_("There are some filtering rules that belong to an account.\n"
			  "Please choose what to do with these rules:"),
			GTK_STOCK_CANCEL, _("_Filter"), NULL, TRUE, G_ALERTALTERNATE, vbox);

	if ((val & ~G_ALERTDISABLE) != G_ALERTALTERNATE) {
		return FALSE;
	} else if (val & G_ALERTDISABLE)
		prefs_common.ask_apply_per_account_filtering_rules = FALSE;

	return TRUE;
}

void summary_filter(SummaryView *summaryview, gboolean selected_only)
{
	GSList *mlist = NULL, *cur_list;
	summary_lock(summaryview);

	/* are there any per-account filtering rules? */
	if (prefs_common.ask_apply_per_account_filtering_rules == TRUE &&
		filtering_peek_per_account_rules(filtering_rules)) {

		if (summary_filter_get_mode() == FALSE) {
			summary_unlock(summaryview);
			return;
		}
	}

	folder_item_update_freeze();
	
	debug_print("filtering...");
	STATUSBAR_PUSH(summaryview->mainwin, _("Filtering..."));
	main_window_cursor_wait(summaryview->mainwin);

	summary_freeze(summaryview);

	if (selected_only) {
		GList *cur;

		for (cur = GTK_CMCLIST(summaryview->ctree)->selection;
	     	     cur != NULL && cur->data != NULL; cur = cur->next) {
			mlist = g_slist_prepend(mlist, 
				 procmsg_msginfo_new_ref(
				  GTKUT_CTREE_NODE_GET_ROW_DATA(cur->data)));
		}
		mlist = g_slist_reverse(mlist);
	} else {
		mlist = folder_item_get_msg_list(summaryview->folder_item);
	}
	
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (cur_list = mlist; cur_list; cur_list = cur_list->next) {
		summary_filter_func((MsgInfo *)cur_list->data);
	}
	folder_item_set_batch(summaryview->folder_item, FALSE);
	
	filtering_move_and_copy_msgs(mlist);
	
	for (cur_list = mlist; cur_list; cur_list = cur_list->next) {
		procmsg_msginfo_free((MsgInfo *)cur_list->data);
	}
	g_slist_free(mlist);

	summary_thaw(summaryview);

	folder_item_update_thaw();
	debug_print("done.\n");
	STATUSBAR_POP(summaryview->mainwin);
	main_window_cursor_normal(summaryview->mainwin);

	summary_unlock(summaryview);

	/* 
	 * CLAWS: summary_show() only valid after having a lock. ideally
	 * we want the lock to be context aware...  
	 */
	summary_show(summaryview, summaryview->folder_item);
}

static void summary_filter_func(MsgInfo *msginfo)
{
	MailFilteringData mail_filtering_data;

	mail_filtering_data.msginfo = msginfo;
	mail_filtering_data.msglist = NULL;			
	mail_filtering_data.filtered = NULL;			
	mail_filtering_data.unfiltered = NULL;			
	if (hooks_invoke(MAIL_MANUAL_FILTERING_HOOKLIST, &mail_filtering_data))
		return;

	filter_message_by_msginfo(filtering_rules, msginfo, NULL,
			FILTERING_MANUALLY, NULL);
}

void summary_msginfo_filter_open(FolderItem * item, MsgInfo *msginfo,
				 PrefsFilterType type, gint processing_rule)
{
	gchar *header = NULL;
	gchar *key = NULL;

	procmsg_get_filter_keyword(msginfo, &header, &key, type);
	
	if (processing_rule) {
		if (item == NULL)
			prefs_filtering_open(&pre_global_processing,
					     _("Processing rules to apply before folder rules"),
					     MANUAL_ANCHOR_PROCESSING,
					     header, key, FALSE);
		else
			prefs_filtering_open(&item->prefs->processing,
					     _("Processing configuration"),
					     MANUAL_ANCHOR_PROCESSING,
					     header, key, FALSE);
	}
	else {
		prefs_filtering_open(&filtering_rules,
				_("Filtering configuration"),
				MANUAL_ANCHOR_FILTERING,
				header, key, TRUE);
	}
	
	g_free(header);
	g_free(key);
}

void summary_filter_open(SummaryView *summaryview, PrefsFilterType type,
			 gint processing_rule)
{
	MsgInfo *msginfo;
	FolderItem * item;
	
	if (!summaryview->selected) return;

	msginfo = gtk_cmctree_node_get_row_data(GTK_CMCTREE(summaryview->ctree),
					      summaryview->selected);
	if (!msginfo) return;
	
	item = summaryview->folder_item;
	summary_msginfo_filter_open(item, msginfo, type, processing_rule);
}

/* color label */

#define N_COLOR_LABELS colorlabel_get_color_count()

static void summary_colorlabel_menu_item_activate_cb(GtkWidget *widget,
						     gpointer data)
{
	guint color = GPOINTER_TO_UINT(data);
	SummaryView *summaryview;

	summaryview = g_object_get_data(G_OBJECT(widget), "summaryview");
	cm_return_if_fail(summaryview != NULL);

	/* "dont_toggle" state set? */
	if (g_object_get_data(G_OBJECT(summaryview->colorlabel_menu),
				"dont_toggle"))
		return;

	summary_set_colorlabel(summaryview, color, NULL);
}

/* summary_set_colorlabel_color() - labelcolor parameter is the color *flag*
 * for the messsage; not the color index */
void summary_set_colorlabel_color(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				  guint labelcolor)
{
	GdkColor color;
	GtkStyle *style, *prev_style, *ctree_style;
	MsgInfo *msginfo;
	gint color_index;

	msginfo = gtk_cmctree_node_get_row_data(ctree, node);
	cm_return_if_fail(msginfo);

	color_index = labelcolor == 0 ? -1 : (gint)labelcolor - 1;
	ctree_style = gtk_widget_get_style(GTK_WIDGET(ctree));
	prev_style = gtk_cmctree_node_get_row_style(ctree, node);

	if (color_index < 0 || color_index >= N_COLOR_LABELS) {
		if (!prev_style) return;
		style = gtk_style_copy(prev_style);
		color = ctree_style->fg[GTK_STATE_NORMAL];
		style->fg[GTK_STATE_NORMAL] = color;
		color = ctree_style->fg[GTK_STATE_SELECTED];
		style->fg[GTK_STATE_SELECTED] = color;
	} else {
		if (prev_style)
			style = gtk_style_copy(prev_style);
		else
			style = gtk_style_copy(ctree_style);
		color = colorlabel_get_color(color_index);
		style->fg[GTK_STATE_NORMAL] = color;
		/* get the average of label color and selected fg color
		   for visibility */
		style->fg[GTK_STATE_SELECTED].red   = (color.red   + 3*ctree_style->fg[GTK_STATE_SELECTED].red  ) / 4;
		style->fg[GTK_STATE_SELECTED].green = (color.green + 3*ctree_style->fg[GTK_STATE_SELECTED].green) / 4;
		style->fg[GTK_STATE_SELECTED].blue  = (color.blue  + 3*ctree_style->fg[GTK_STATE_SELECTED].blue ) / 4;
	}

	gtk_cmctree_node_set_row_style(ctree, node, style);
	g_object_unref(style);
}

static void summary_set_row_colorlabel(SummaryView *summaryview, GtkCMCTreeNode *row, guint labelcolor)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);

	summary_msginfo_change_flags(msginfo, MSG_COLORLABEL_TO_FLAGS(labelcolor), 0, 
					MSG_CLABEL_FLAG_MASK, 0);
	summary_set_row_marks(summaryview, row);
}

void summary_set_colorlabel(SummaryView *summaryview, guint labelcolor,
			    GtkWidget *widget)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	START_LONG_OPERATION(summaryview, FALSE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		summary_set_row_colorlabel(summaryview,
					   GTK_CMCTREE_NODE(cur->data), labelcolor);
	END_LONG_OPERATION(summaryview);
}

static gboolean summary_set_row_tag(SummaryView *summaryview, GtkCMCTreeNode *row, gboolean refresh, gboolean set, gint id)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	MsgInfo *msginfo;
	gchar *tags_str = NULL;
	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_val_if_fail(msginfo, FALSE);

	procmsg_msginfo_update_tags(msginfo, set, id);
	
	if (summaryview->col_state[summaryview->col_pos[S_COL_TAGS]].visible) {
		tags_str = procmsg_msginfo_get_tags_str(msginfo);
		gtk_cmctree_node_set_text(ctree, row, 
				summaryview->col_pos[S_COL_TAGS],
				tags_str?tags_str:"-");
		g_free(tags_str);
	}

	summary_set_row_marks(summaryview, row);
	if (row == summaryview->displayed) {
		return TRUE;
	}
	return FALSE;
}

void summary_set_tag(SummaryView *summaryview, gint tag_id,
			    GtkWidget *widget)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean set = tag_id > 0;
	gint real_id = set? tag_id:-tag_id;
	gboolean froze = FALSE;
	gboolean redisplay = FALSE;

	if (summary_is_locked(summaryview))
		return;
	START_LONG_OPERATION(summaryview, FALSE);
	folder_item_set_batch(summaryview->folder_item, TRUE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next) {
		redisplay |= summary_set_row_tag(summaryview,
					   GTK_CMCTREE_NODE(cur->data), FALSE, set, real_id);
	}
	folder_item_set_batch(summaryview->folder_item, FALSE);
	END_LONG_OPERATION(summaryview);
	if (redisplay)
		summary_redisplay_msg(summaryview);
}

static void summary_tags_menu_item_activate_cb(GtkWidget *widget,
						     gpointer data)
{
	gint id = GPOINTER_TO_INT(data);
	gboolean set = gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget));
	SummaryView *summaryview;

	summaryview = g_object_get_data(G_OBJECT(widget), "summaryview");
	cm_return_if_fail(summaryview != NULL);

	/* "dont_toggle" state set? */
	if (g_object_get_data(G_OBJECT(summaryview->tags_menu),
				"dont_toggle"))
		return;

	if (!set)
		id = -id;
	summary_set_tag(summaryview, id, NULL);
}

static void summary_colorlabel_menu_item_activate_item_cb(GtkMenuItem *menu_item,
							  gpointer data)
{
	SummaryView *summaryview;
	GtkMenuShell *menu;
	GtkCheckMenuItem **items;
	gint n;
	GList *children, *cur, *sel;

	summaryview = (SummaryView *)data;
	cm_return_if_fail(summaryview != NULL);

	sel = GTK_CMCLIST(summaryview->ctree)->selection;
	if (!sel) return;

	menu = GTK_MENU_SHELL(summaryview->colorlabel_menu);
	
	cm_return_if_fail(menu != NULL);

	Xalloca(items, (N_COLOR_LABELS + 1) * sizeof(GtkWidget *), return);

	/* NOTE: don't return prematurely because we set the "dont_toggle"
	 * state for check menu items */
	g_object_set_data(G_OBJECT(menu), "dont_toggle",
			  GINT_TO_POINTER(1));

	/* clear items. get item pointers. */
	children = gtk_container_get_children(GTK_CONTAINER(menu));
	for (n = 0, cur = children; cur != NULL && cur->data != NULL; cur = cur->next) {
		if (GTK_IS_CHECK_MENU_ITEM(cur->data)) {
			gtk_check_menu_item_set_active
				(GTK_CHECK_MENU_ITEM(cur->data), FALSE);
			items[n] = GTK_CHECK_MENU_ITEM(cur->data);
			n++;
		}
	}

	g_list_free(children);

	if (n == (N_COLOR_LABELS + 1)) {
		/* iterate all messages and set the state of the appropriate
		 * items */
		for (; sel != NULL; sel = sel->next) {
			MsgInfo *msginfo;
			gint clabel;

			msginfo = gtk_cmctree_node_get_row_data
				(GTK_CMCTREE(summaryview->ctree),
				 GTK_CMCTREE_NODE(sel->data));
			if (msginfo) {
				clabel = MSG_GET_COLORLABEL_VALUE(msginfo->flags);
				if (!gtk_check_menu_item_get_active(items[clabel]))
					gtk_check_menu_item_set_active
						(items[clabel], TRUE);
			}
		}
	} else
		g_warning("invalid number of color elements (%d)\n", n);

	/* reset "dont_toggle" state */
	g_object_set_data(G_OBJECT(menu), "dont_toggle",
			  GINT_TO_POINTER(0));
}

static void summary_colorlabel_menu_create(SummaryView *summaryview, gboolean refresh)
{
	GtkWidget *label_menuitem;
	GtkWidget *menu;
	GtkWidget *item;
	gint i;
	gchar *accel_path = NULL;

	label_menuitem = gtk_ui_manager_get_widget(summaryview->mainwin->ui_manager, "/Menus/SummaryViewPopup/ColorLabel");
	g_signal_connect(G_OBJECT(label_menuitem), "activate",
			 G_CALLBACK(summary_colorlabel_menu_item_activate_item_cb),
			   summaryview);
	gtk_widget_show(label_menuitem);

	menu = gtk_menu_new();

	gtk_menu_set_accel_group (GTK_MENU (menu), 
		gtk_ui_manager_get_accel_group(mainwindow_get_mainwindow()->ui_manager));

	/* create sub items. for the menu item activation callback we pass the
	 * index of label_colors[] as data parameter. for the None color we
	 * pass an invalid (high) value. also we attach a data pointer so we
	 * can always get back the SummaryView pointer. */

	item = gtk_check_menu_item_new_with_label(_("None"));
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
	g_signal_connect(G_OBJECT(item), "activate",
			 G_CALLBACK(summary_colorlabel_menu_item_activate_cb),
			   GUINT_TO_POINTER(0));
	g_object_set_data(G_OBJECT(item), "summaryview", summaryview);
	gtk_widget_show(item);

	accel_path = g_strdup_printf("<ClawsColorLabels>/None");
	gtk_menu_item_set_accel_path(GTK_MENU_ITEM(item), accel_path);
	g_free(accel_path);
	gtk_accel_map_add_entry("<ClawsColorLabels>/None", GDK_0, GDK_CONTROL_MASK);

	item = gtk_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
	gtk_widget_show(item);

	/* create pixmap/label menu items */
	for (i = 0; i < N_COLOR_LABELS; i++) {
		item = colorlabel_create_check_color_menu_item(
			i, refresh, SUMMARY_COLORMENU);
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		g_signal_connect(G_OBJECT(item), "activate",
				 G_CALLBACK(summary_colorlabel_menu_item_activate_cb),
				 GUINT_TO_POINTER(i + 1));
		g_object_set_data(G_OBJECT(item), "summaryview",
				  summaryview);
		gtk_widget_show(item);
		accel_path = g_strdup_printf("<ClawsColorLabels>/%d", i+1);
		gtk_menu_item_set_accel_path(GTK_MENU_ITEM(item), accel_path);
		if (i < 9)
			gtk_accel_map_add_entry(accel_path, GDK_1+i, GDK_CONTROL_MASK);
		g_free(accel_path);
		g_signal_connect (gtk_ui_manager_get_accel_group(mainwindow_get_mainwindow()->ui_manager), 
			"accel-changed", G_CALLBACK (mainwin_accel_changed_cb), item);
	}

	gtk_widget_show(menu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(label_menuitem), menu);
	summaryview->colorlabel_menu = menu;
}

static void summary_tags_menu_item_activate_item_cb(GtkMenuItem *menu_item,
							  gpointer data)
{
	GtkMenuShell *menu;
	GList *children, *cur;
	GList *sel;
	GHashTable *menu_table = g_hash_table_new_full(
					g_direct_hash,
					g_direct_equal,
					NULL, NULL);
	GHashTable *menu_allsel_table = g_hash_table_new_full(
					g_direct_hash,
					g_direct_equal,
					NULL, NULL);
	gint sel_len;
	SummaryView *summaryview = (SummaryView *)data;
	cm_return_if_fail(summaryview != NULL);

	sel = GTK_CMCLIST(summaryview->ctree)->selection;
	if (!sel) return;

	menu = GTK_MENU_SHELL(summaryview->tags_menu);
	cm_return_if_fail(menu != NULL);

	/* NOTE: don't return prematurely because we set the "dont_toggle"
	 * state for check menu items */
	g_object_set_data(G_OBJECT(menu), "dont_toggle",
			  GINT_TO_POINTER(1));

	/* clear items. get item pointers. */
	children = gtk_container_get_children(GTK_CONTAINER(menu));
	for (cur = children; cur != NULL && cur->data != NULL; cur = cur->next) {
		if (GTK_IS_CHECK_MENU_ITEM(cur->data)) {
			gint id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cur->data),
				"tag_id"));
			gtk_check_menu_item_set_active
				(GTK_CHECK_MENU_ITEM(cur->data), FALSE);
				
			g_hash_table_insert(menu_table, GINT_TO_POINTER(id), GTK_CHECK_MENU_ITEM(cur->data));
			g_hash_table_insert(menu_allsel_table, GINT_TO_POINTER(id), GINT_TO_POINTER(0));
		}
	}

	g_list_free(children);

	/* iterate all messages and set the state of the appropriate
	 * items */
	sel_len = 0;
	for (; sel != NULL; sel = sel->next) {
		MsgInfo *msginfo;
		GSList *tags = NULL;
		GtkCheckMenuItem *item;
		msginfo = gtk_cmctree_node_get_row_data
			(GTK_CMCTREE(summaryview->ctree),
			 GTK_CMCTREE_NODE(sel->data));
		sel_len++;
		if (msginfo) {
			tags =  msginfo->tags;
			if (!tags)
				continue;

			for (; tags; tags = tags->next) {
				gint num_checked = GPOINTER_TO_INT(g_hash_table_lookup(menu_allsel_table, tags->data));
				item = g_hash_table_lookup(menu_table, GINT_TO_POINTER(tags->data));
				if (item && !gtk_check_menu_item_get_active(item)) {
					gtk_check_menu_item_set_active
						(item, TRUE);
				}
				num_checked++;
				g_hash_table_replace(menu_allsel_table, tags->data, GINT_TO_POINTER(num_checked));
			}
		}
	}

	children = gtk_container_get_children(GTK_CONTAINER(menu));
	for (cur = children; cur != NULL && cur->data != NULL; cur = cur->next) {
		if (GTK_IS_CHECK_MENU_ITEM(cur->data)) {
			gint id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cur->data),
				"tag_id"));
			gint num_checked = GPOINTER_TO_INT(g_hash_table_lookup(menu_allsel_table, GINT_TO_POINTER(id)));
			if (num_checked < sel_len && num_checked > 0)
				gtk_check_menu_item_set_inconsistent(GTK_CHECK_MENU_ITEM(cur->data), TRUE);
			else
				gtk_check_menu_item_set_inconsistent(GTK_CHECK_MENU_ITEM(cur->data), FALSE);
		}
	}
	g_list_free(children);
	g_hash_table_destroy(menu_table);
	g_hash_table_destroy(menu_allsel_table);
	/* reset "dont_toggle" state */
	g_object_set_data(G_OBJECT(menu), "dont_toggle",
			  GINT_TO_POINTER(0));
}

void summaryview_destroy(SummaryView *summaryview)
{
#ifndef G_OS_WIN32
	if(summaryview->simplify_subject_preg) {
		regfree(summaryview->simplify_subject_preg);
		g_free(summaryview->simplify_subject_preg);
		summaryview->simplify_subject_preg = NULL;
	}
#endif
}
static void summary_tags_menu_item_apply_tags_activate_cb(GtkWidget *widget,
						     gpointer data)
{
	SummaryView *summaryview;

	summaryview = g_object_get_data(G_OBJECT(widget), "summaryview");
	cm_return_if_fail(summaryview != NULL);

	/* "dont_toggle" state set? */
	if (g_object_get_data(G_OBJECT(summaryview->tags_menu),
				"dont_toggle"))
		return;
	
	tag_apply_open(summary_get_selection(summaryview));	
}

static gint summary_tag_cmp_list(gconstpointer a, gconstpointer b)
{
	gint id_a = GPOINTER_TO_INT(a);
	gint id_b = GPOINTER_TO_INT(b);
	const gchar *tag_a = tags_get_tag(id_a);
	const gchar *tag_b = tags_get_tag(id_b);
	
	if (tag_a == NULL)
		return tag_b == NULL ? 0:1;
	
	if (tag_b == NULL)
		return tag_a == NULL ? 0:1;

	return g_utf8_collate(tag_a, tag_b);
}

static void summary_tags_menu_create(SummaryView *summaryview, gboolean refresh)
{

	GtkWidget *label_menuitem;
	GtkWidget *menu;
	GtkWidget *item;
	GSList *cur = tags_get_list();
	GSList *orig = NULL;
	gboolean existing_tags = FALSE;
	gchar *accel_path = NULL;

	cur = orig = g_slist_sort(cur, summary_tag_cmp_list);
	label_menuitem = gtk_ui_manager_get_widget(summaryview->mainwin->ui_manager, "/Menus/SummaryViewPopup/Tags");
	g_signal_connect(G_OBJECT(label_menuitem), "activate",
			 G_CALLBACK(summary_tags_menu_item_activate_item_cb),
			   summaryview);

	gtk_widget_show(label_menuitem);

	menu = gtk_menu_new();

	gtk_menu_set_accel_group (GTK_MENU (menu), 
		gtk_ui_manager_get_accel_group(summaryview->mainwin->ui_manager));

	/* create tags menu items */
	for (; cur; cur = cur->next) {
		gint id = GPOINTER_TO_INT(cur->data);
		const gchar *tag = tags_get_tag(id);
		item = gtk_check_menu_item_new_with_label(tag);
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		g_signal_connect(G_OBJECT(item), "activate",
				 G_CALLBACK(summary_tags_menu_item_activate_cb),
				 GINT_TO_POINTER(id));
		g_object_set_data(G_OBJECT(item), "summaryview",
				  summaryview);
		g_object_set_data(G_OBJECT(item), "tag_id",
				  GINT_TO_POINTER(id));
		gtk_widget_show(item);
		accel_path = g_strconcat("<ClawsTags>/",tag, NULL);
		gtk_menu_item_set_accel_path(GTK_MENU_ITEM(item), accel_path);
		g_free(accel_path);
		existing_tags = TRUE;
	}
	if (existing_tags) {
		/* separator */
		item = gtk_menu_item_new();
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		gtk_widget_show(item);
	}

	item = gtk_menu_item_new_with_label(_("Apply tags..."));
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
	g_signal_connect(G_OBJECT(item), "activate",
			 G_CALLBACK(summary_tags_menu_item_apply_tags_activate_cb),
			 NULL);
	g_object_set_data(G_OBJECT(item), "summaryview",
			  summaryview);
	gtk_widget_show(item);
	accel_path = g_strdup_printf("<ClawsTags>/ApplyTags");
	gtk_menu_item_set_accel_path(GTK_MENU_ITEM(item), accel_path);
	g_free(accel_path);

	g_slist_free(orig);
	gtk_widget_show(menu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(label_menuitem), menu);
	summaryview->tags_menu = menu;
}

static gboolean summary_popup_menu(GtkWidget *widget, gpointer data)
{
	SummaryView *summaryview = (SummaryView *)data;
	summaryview->display_msg = messageview_is_visible(summaryview->messageview);

	gtk_menu_popup(GTK_MENU(summaryview->popupmenu), 
		       NULL, NULL, NULL, NULL, 
		       3, gtk_get_current_event_time());

	return TRUE;
}

#if !GENERIC_UMPC
static gchar *summaryview_get_tooltip_text(SummaryView *summaryview, MsgInfo *info, gint column)
{
	MsgFlags flags;
	if (!info)
		return NULL;

	flags = info->flags;

	switch(summaryview->col_state[column].type) {
		case S_COL_STATUS:
			if (MSG_IS_IGNORE_THREAD(flags)) {
				return _("Ignored thread");
			} else if (MSG_IS_WATCH_THREAD(flags)) {
				return _("Watched thread");
			} else if (MSG_IS_SPAM(flags)) {
				return _("Spam");
			} else if (MSG_IS_NEW(flags)) {
				return _("New");
			} else if (MSG_IS_UNREAD(flags)) {
				return _("Unread");
			} else if (MSG_IS_REPLIED(flags) && MSG_IS_FORWARDED(flags)) {
				return _("Replied but also forwarded - click to see reply");
			} else if (MSG_IS_REPLIED(flags)) {
				return _("Replied - click to see reply");
			} else if (MSG_IS_FORWARDED(flags)) {
				return _("Forwarded");
			} else {
				return NULL;
			}
		case S_COL_MARK:
			if (MSG_IS_DELETED(flags)) {
				return _("Deleted");
			} else if (MSG_IS_MARKED(flags)) {
				return _("Marked");
			} else if (MSG_IS_MOVE(flags)) {
				return _("To be moved");
			} else if (MSG_IS_COPY(flags)) {
				return _("To be copied");
			} else {
				return NULL;
			}
		case S_COL_LOCKED:
			if (MSG_IS_LOCKED(flags)) {
				return _("Locked");
			} else {
				return NULL;
			}
		case S_COL_MIME:
			if (MSG_IS_WITH_ATTACHMENT(flags) && MSG_IS_SIGNED(flags)) {
				return _("Signed, has attachment(s)");
			} else if (MSG_IS_SIGNED(flags)) {
				return _("Signed");
			} else if (MSG_IS_WITH_ATTACHMENT(flags) && MSG_IS_ENCRYPTED(flags)) {
				return _("Encrypted, has attachment(s)");
			} else if (MSG_IS_ENCRYPTED(flags)) {
				return _("Encrypted");
			} else if (MSG_IS_WITH_ATTACHMENT(flags)) {
				return _("Has attachment(s)");
			} else {
				return NULL;
			}
		default:
			return NULL;
	}
}
static gboolean tooltip_cb (GtkWidget  *widget,
                            gint        x,
                            gint        y,
                            gboolean    keyboard_mode,
                            GtkTooltip *tooltip,
                            gpointer    user_data) 
{
	GtkCMCTree *ctree = GTK_CMCTREE(widget);
	SummaryView *summaryview = (SummaryView *)user_data;
	gint row = -1, column = -1;
	int offset = prefs_common.show_col_headers ? 24:0;
	GtkCMCTreeNode *node = NULL;
	gchar *text = NULL;
	gchar *formatted = NULL;
	MsgInfo *info = NULL;
	GdkRectangle rect;
	gboolean vert = (prefs_common.layout_mode == VERTICAL_LAYOUT);

	if (!prefs_common.show_tooltips)
		return FALSE;

	if (y - offset < 0)
		return FALSE;

	if (!gtk_cmclist_get_selection_info(GTK_CMCLIST(ctree), x, y - offset,
					  &row, &column))
		return FALSE;

	if ((node = gtk_cmctree_node_nth(ctree, row)) == NULL)
		return FALSE;

	if ((info = gtk_cmctree_node_get_row_data(ctree, node)) == NULL)
		return FALSE;

	switch (gtk_cmctree_node_get_cell_type(ctree, node, column)) {
		case GTK_CMCELL_TEXT:
			if (gtk_cmctree_node_get_text(ctree, node, column, &text) != TRUE)
				return FALSE;
			break;
		case GTK_CMCELL_PIXTEXT:
			if (gtk_cmctree_node_get_pixtext(ctree, node, column, &text, 
				NULL, NULL) != TRUE)
				return FALSE;
			break;
		default: 
			if ((text = summaryview_get_tooltip_text(summaryview, info, column)) == NULL)
				return FALSE;
	}
	
	if (!text || !*text)
		return FALSE;

	formatted = g_strdup(text);
	g_strstrip(formatted);

	if (!vert)	
		gtk_tooltip_set_text (tooltip, formatted);
	else if (prefs_common.two_line_vert)
		gtk_tooltip_set_markup (tooltip, formatted);
	g_free(formatted);
	
	rect.x = x - 2;
	rect.y = y - 2;
	rect.width = 12;
	rect.height= 12;	
	gtk_tooltip_set_tip_area(tooltip, &rect);
	
	return TRUE;
}
#endif
static GtkWidget *summary_ctree_create(SummaryView *summaryview)
{
	GtkWidget *ctree;
	gint *col_pos = summaryview->col_pos;
	SummaryColumnState *col_state;
	gchar *titles[N_SUMMARY_COLS];
	SummaryColumnType type;
	gint pos;
	gboolean vert = (prefs_common.layout_mode == VERTICAL_LAYOUT);

	memset(titles, 0, sizeof(titles));

	col_state = prefs_summary_column_get_config();
	for (pos = 0; pos < N_SUMMARY_COLS; pos++) {
		summaryview->col_state[pos] = col_state[pos];
		type = col_state[pos].type;
		col_pos[type] = pos;
		titles[pos] = "dummy";
	}
	col_state = summaryview->col_state;

	ctree = gtk_sctree_new_with_titles
		(N_SUMMARY_COLS, col_pos[S_COL_SUBJECT], titles);

	/* get normal row height */
	gtk_cmclist_set_row_height(GTK_CMCLIST(ctree), 0);
	normal_row_height = GTK_CMCLIST(ctree)->row_height;

	if (prefs_common.show_col_headers == FALSE)
		gtk_cmclist_column_titles_hide(GTK_CMCLIST(ctree));

	gtk_cmclist_set_selection_mode(GTK_CMCLIST(ctree), GTK_SELECTION_EXTENDED);
	gtk_cmclist_set_column_justification(GTK_CMCLIST(ctree), col_pos[S_COL_MARK],
					   GTK_JUSTIFY_CENTER);
	gtk_cmclist_set_column_justification(GTK_CMCLIST(ctree), col_pos[S_COL_STATUS],
					   GTK_JUSTIFY_CENTER);
	gtk_cmclist_set_column_justification(GTK_CMCLIST(ctree), col_pos[S_COL_LOCKED],
					   GTK_JUSTIFY_CENTER);
	gtk_cmclist_set_column_justification(GTK_CMCLIST(ctree), col_pos[S_COL_MIME],
					   GTK_JUSTIFY_CENTER);
	gtk_cmclist_set_column_justification(GTK_CMCLIST(ctree), col_pos[S_COL_SIZE],
					   GTK_JUSTIFY_RIGHT);
	gtk_cmclist_set_column_justification(GTK_CMCLIST(ctree), col_pos[S_COL_NUMBER],
					   GTK_JUSTIFY_RIGHT);
	gtk_cmclist_set_column_justification(GTK_CMCLIST(ctree), col_pos[S_COL_SCORE],
					   GTK_JUSTIFY_RIGHT);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_MARK],
				   prefs_common.summary_col_size[S_COL_MARK]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_STATUS],
				   prefs_common.summary_col_size[S_COL_STATUS]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_LOCKED],
				   prefs_common.summary_col_size[S_COL_LOCKED]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_MIME],
				   prefs_common.summary_col_size[S_COL_MIME]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_SUBJECT],
				   prefs_common.summary_col_size[S_COL_SUBJECT]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_FROM],
				   prefs_common.summary_col_size[S_COL_FROM]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_TO],
				   prefs_common.summary_col_size[S_COL_TO]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_DATE],
				   prefs_common.summary_col_size[S_COL_DATE]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_SIZE],
				   prefs_common.summary_col_size[S_COL_SIZE]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_NUMBER],
				   prefs_common.summary_col_size[S_COL_NUMBER]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_SCORE],
				   prefs_common.summary_col_size[S_COL_SCORE]);
	gtk_cmclist_set_column_width(GTK_CMCLIST(ctree), col_pos[S_COL_TAGS],
				   prefs_common.summary_col_size[S_COL_TAGS]);

	gtk_cmctree_set_line_style(GTK_CMCTREE(ctree), GTK_CMCTREE_LINES_NONE);
	gtk_cmctree_set_expander_style(GTK_CMCTREE(ctree),
			     GTK_CMCTREE_EXPANDER_TRIANGLE);

	gtk_sctree_set_stripes(GTK_SCTREE(ctree), prefs_common.use_stripes_in_summaries);

	gtk_cmctree_set_indent(GTK_CMCTREE(ctree), 12);
	g_object_set_data(G_OBJECT(ctree), "summaryview", (gpointer)summaryview); 

	for (pos = 0; pos < N_SUMMARY_COLS; pos++) {
		gtkut_widget_set_can_focus(GTK_CMCLIST(ctree)->column[pos].button,
				       FALSE);
		if (((pos == summaryview->col_pos[S_COL_FROM] && !FOLDER_SHOWS_TO_HDR(summaryview->folder_item)) ||
		     (pos == summaryview->col_pos[S_COL_TO] && FOLDER_SHOWS_TO_HDR(summaryview->folder_item)) ||
		     pos == summaryview->col_pos[S_COL_DATE]) && vert &&
			    prefs_common.two_line_vert)
			gtk_cmclist_set_column_visibility
				(GTK_CMCLIST(ctree), pos, FALSE);
		else
			gtk_cmclist_set_column_visibility
				(GTK_CMCLIST(ctree), pos, col_state[pos].visible);
	}
	if (prefs_common.two_line_vert)
		gtk_sctree_set_use_markup(GTK_SCTREE(ctree), summaryview->col_pos[S_COL_SUBJECT], vert);

	/* connect signal to the buttons for sorting */
#define CLIST_BUTTON_SIGNAL_CONNECT(col, func) \
	g_signal_connect \
		(G_OBJECT(GTK_CMCLIST(ctree)->column[col_pos[col]].button), \
		 "clicked", \
		 G_CALLBACK(func), \
		 summaryview)

	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_MARK   , summary_mark_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_STATUS , summary_status_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_MIME   , summary_mime_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_NUMBER , summary_num_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_SIZE   , summary_size_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_DATE   , summary_date_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_FROM   , summary_from_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_TO     , summary_to_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_SUBJECT, summary_subject_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_SCORE,   summary_score_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_LOCKED,  summary_locked_clicked);
	CLIST_BUTTON_SIGNAL_CONNECT(S_COL_TAGS,    summary_tags_clicked);
	
#undef CLIST_BUTTON_SIGNAL_CONNECT

	g_signal_connect(G_OBJECT(ctree), "tree_select_row",
			 G_CALLBACK(summary_selected), summaryview);
	g_signal_connect(G_OBJECT(ctree), "tree_unselect_row",
			 G_CALLBACK(summary_unselected), summaryview);
	g_signal_connect(G_OBJECT(ctree), "button_press_event",
			 G_CALLBACK(summary_button_pressed),
			 summaryview);
#ifndef MAEMO
	g_signal_connect(G_OBJECT(ctree), "popup-menu",
			 G_CALLBACK(summary_popup_menu), summaryview);
#else
	gtk_widget_tap_and_hold_setup(GTK_WIDGET(ctree), NULL, NULL,
			GTK_TAP_AND_HOLD_NONE | GTK_TAP_AND_HOLD_NO_INTERNALS);
	g_signal_connect(G_OBJECT(ctree), "tap-and-hold",
			 G_CALLBACK(summary_popup_menu), summaryview);
#endif
	g_signal_connect(G_OBJECT(ctree), "button_release_event",
			 G_CALLBACK(summary_button_released),
			 summaryview);
	g_signal_connect(G_OBJECT(ctree), "key_press_event",
			 G_CALLBACK(summary_key_pressed), summaryview);
	g_signal_connect(G_OBJECT(ctree), "resize_column",
			 G_CALLBACK(summary_col_resized), summaryview);
        g_signal_connect(G_OBJECT(ctree), "open_row",
			 G_CALLBACK(summary_open_row), summaryview);

	g_signal_connect_after(G_OBJECT(ctree), "tree_expand",
			       G_CALLBACK(summary_tree_expanded),
			       summaryview);
	g_signal_connect_after(G_OBJECT(ctree), "tree_collapse",
			       G_CALLBACK(summary_tree_collapsed),
			       summaryview);

	g_signal_connect(G_OBJECT(ctree), "start_drag",
			 G_CALLBACK(summary_start_drag),
			 summaryview);
	g_signal_connect(G_OBJECT(ctree), "drag_data_get",
			 G_CALLBACK(summary_drag_data_get),
			 summaryview);
	g_signal_connect(G_OBJECT(ctree), "drag_end",
			 G_CALLBACK(summary_drag_end),
			 summaryview);

	gtk_drag_dest_set(ctree, GTK_DEST_DEFAULT_ALL & ~GTK_DEST_DEFAULT_HIGHLIGHT,
			  summary_drag_types, 3,
			  GDK_ACTION_MOVE | GDK_ACTION_COPY | GDK_ACTION_DEFAULT);

	g_signal_connect(G_OBJECT(ctree), "drag_data_received",
			 G_CALLBACK(summary_drag_data_received),
			 summaryview);

	g_signal_connect(G_OBJECT(ctree), "drag_motion",
			 G_CALLBACK(summary_drag_motion_cb),
			 summaryview);

#if !GENERIC_UMPC
	g_object_set (G_OBJECT(ctree), "has-tooltip", TRUE, NULL);
	g_signal_connect(G_OBJECT(ctree), "query-tooltip", 
			 G_CALLBACK(tooltip_cb),
			summaryview);
#endif
	return ctree;
}

void summary_set_column_order(SummaryView *summaryview)
{
	GtkWidget *ctree;
	GtkWidget *scrolledwin = summaryview->scrolledwin;
	FolderItem *item;
	guint selected_msgnum = summary_get_msgnum(summaryview, summaryview->selected);
	guint displayed_msgnum = summary_get_msgnum(summaryview, summaryview->displayed);

	item = summaryview->folder_item;

	summary_clear_all(summaryview);
	gtk_widget_destroy(summaryview->ctree);

	summaryview->ctree = ctree = summary_ctree_create(summaryview);
	summary_set_fonts(summaryview);
	summary_set_column_titles(summaryview);
	gtk_scrolled_window_set_hadjustment(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_CMCLIST(ctree)->hadjustment);
	gtk_scrolled_window_set_vadjustment(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_CMCLIST(ctree)->vadjustment);
	gtk_container_add(GTK_CONTAINER(scrolledwin), ctree);
	gtk_widget_show(ctree);

	summary_show(summaryview, item);

	summary_select_by_msgnum(summaryview, selected_msgnum);

	summaryview->selected = summary_find_msg_by_msgnum(summaryview, selected_msgnum);
	summaryview->displayed = summary_find_msg_by_msgnum(summaryview, displayed_msgnum);
	if (!summaryview->displayed)
		messageview_clear(summaryview->messageview);
	else
		summary_redisplay_msg(summaryview);

	if (prefs_common.layout_mode == VERTICAL_LAYOUT &&
	    prefs_common.two_line_vert) {
		gtk_cmclist_set_row_height(GTK_CMCLIST(summaryview->ctree), 2*normal_row_height + 2);		
	} else {
		gtk_cmclist_set_row_height(GTK_CMCLIST(summaryview->ctree), 0);		
	}
}


/* callback functions */

static gint summary_folder_eventbox_pressed(GtkWidget *eventbox, GdkEventButton *event,
					    SummaryView *summaryview)
{
	if (event) {
		gtk_widget_grab_focus(summaryview->folderview->ctree);
		mainwindow_exit_folder(summaryview->mainwin);
	}
	return TRUE;
}

static gint summary_toggle_pressed(GtkWidget *eventbox, GdkEventButton *event,
				   SummaryView *summaryview)
{
	if (event)  
		summary_toggle_view(summaryview);
	return TRUE;
}
#ifdef GENERIC_UMPC
static void summary_toggle_multiple_pressed(GtkWidget *widget,
				   SummaryView *summaryview)
{
	GTK_SCTREE(summaryview->ctree)->force_additive_sel = 
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
}
#endif
static gboolean summary_button_pressed(GtkWidget *ctree, GdkEventButton *event,
				       SummaryView *summaryview)
{
	if (!event) return FALSE;

	if (event->button == 3) {
		/* right clicked */
		summary_set_menu_sensitive(summaryview);
		gtk_menu_popup(GTK_MENU(summaryview->popupmenu), NULL, NULL,
			       NULL, NULL, event->button, event->time);
	} else if (event->button == 2) {
		summaryview->display_msg = messageview_is_visible(summaryview->messageview);
	} else if (event->button == 1) {
		if (!prefs_common.emulate_emacs &&
		    messageview_is_visible(summaryview->messageview))
			summaryview->display_msg = TRUE;
	}

	return FALSE;
}

static gboolean summary_button_released(GtkWidget *ctree, GdkEventButton *event,
					SummaryView *summaryview)
{
	return FALSE;
}

gboolean summary_pass_key_press_event(SummaryView *summaryview, GdkEventKey *event)
{
	if (!summaryview)
		return FALSE;
	if (summary_is_list(summaryview))
		return summary_key_pressed(summaryview->ctree, event, summaryview);
	else
		return FALSE;
}

#define BREAK_ON_MODIFIER_KEY() \
	if ((event->state & (GDK_MOD1_MASK|GDK_CONTROL_MASK)) != 0) break

static gboolean summary_key_pressed(GtkWidget *widget, GdkEventKey *event,
				    SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(widget);
	GtkCMCTreeNode *node;
	MessageView *messageview;
	GtkAdjustment *adj;
	gboolean mod_pressed;

	if (!event) 
		return TRUE;

	if (quicksearch_has_focus(summaryview->quicksearch))
		return FALSE;

	messageview = summaryview->messageview;

	mod_pressed =
		((event->state & (GDK_SHIFT_MASK|GDK_MOD1_MASK)) != 0);

	if (summaryview->selected) {
		gboolean handled = FALSE;
		switch (event->keyval) {
		case GDK_KEY_space:		/* Page down or go to the next */
			handled = TRUE;
			if (event->state & GDK_CONTROL_MASK) {
				handled = FALSE;
				break;
			}
			if (event->state & GDK_SHIFT_MASK) 
				mimeview_scroll_page(messageview->mimeview, TRUE);
			else {
				if (summaryview->displayed != summaryview->selected) {
					summary_display_msg(summaryview,
							    summaryview->selected);
					break;
				}
				if (mod_pressed) {
					if (!mimeview_scroll_page(messageview->mimeview, TRUE))
						summary_select_prev_unread(summaryview);
				} else {
					if (!mimeview_scroll_page(messageview->mimeview, FALSE))
						summary_select_next_unread(summaryview);
				}				
			}
			break;
		case GDK_KEY_BackSpace:	/* Page up */
			handled = TRUE;
			mimeview_scroll_page(messageview->mimeview, TRUE);
			break;
		case GDK_KEY_Return:	/* Scroll up/down one line */
		case GDK_KEY_KP_Enter:
			handled = TRUE;
			if (summaryview->displayed != summaryview->selected) {
#ifndef GENERIC_UMPC
				summary_display_msg(summaryview,
						    summaryview->selected);
#else
				summary_open_row(NULL, summaryview);
#endif
				break;
			}
			mimeview_scroll_one_line(messageview->mimeview, mod_pressed);
			break;
		}
		
		if (handled)
			return FALSE;
	}
	if (summary_is_locked(summaryview)) 
		return TRUE;

	switch (event->keyval) {
	case GDK_KEY_Left:		/* Move focus */
		adj = gtk_scrolled_window_get_hadjustment
			(GTK_SCROLLED_WINDOW(summaryview->scrolledwin));
		if (gtk_adjustment_get_lower(adj) != gtk_adjustment_get_value(adj))
			break;
		/* FALLTHROUGH */	
	case GDK_KEY_Escape:
		gtk_widget_grab_focus(summaryview->folderview->ctree);
		mainwindow_exit_folder(summaryview->mainwin);
		return TRUE;
	case GDK_KEY_Home:
	case GDK_KEY_End:
		if ((node = summaryview->selected) != NULL) {
			GtkCMCTreeNode *next = NULL;
			next = (event->keyval == GDK_KEY_Home)
					? gtk_cmctree_node_nth(ctree, 0)
					: gtk_cmctree_node_nth(ctree, 
						g_list_length(GTK_CMCLIST(ctree)->row_list)-1);
			if (next) {
				gtk_sctree_select_with_state
					(GTK_SCTREE(ctree), next, (event->state & ~GDK_CONTROL_MASK) );

				/* Deprecated - what are the non-deprecated equivalents? */
				if (gtk_cmctree_node_is_visible(GTK_CMCTREE(ctree), next) != GTK_VISIBILITY_FULL)
					gtk_cmctree_node_moveto(GTK_CMCTREE(ctree), next, 0, 0, 0);
				summaryview->selected = next;
			}
		}
		return TRUE;
	default:
		break;
	}

	if (!summaryview->selected) {
		node = gtk_cmctree_node_nth(ctree, 0);
		if (node)
			gtk_sctree_select(GTK_SCTREE(ctree), node);
		else
			return TRUE;
	}

	switch (event->keyval) {
	case GDK_KEY_Delete:
		BREAK_ON_MODIFIER_KEY();
		summary_delete_trash(summaryview);
		break;
	default:
		break;
	}
	return FALSE;
}

static void quicksearch_execute_cb(QuickSearch *quicksearch, gpointer data)
{
	SummaryView *summaryview = data;

	summaryview_reset_recursive_folder_match(summaryview);
	if (summary_show(summaryview, summaryview->folder_item))
		summaryview_quicksearch_recurse(summaryview);
	else
		summaryview_reset_recursive_folder_match(summaryview);
}

static void tog_searchbar_cb(GtkWidget *w, gpointer data)
{
	SummaryView *summaryview = (SummaryView *)data;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w))) {
		prefs_common.show_searchbar = TRUE;
 		quicksearch_show(summaryview->quicksearch);
	} else {
		prefs_common.show_searchbar = FALSE;
 		quicksearch_hide(summaryview->quicksearch);
	}
}

void summaryview_activate_quicksearch(SummaryView *summaryview, gboolean show) 
{
	prefs_common.show_searchbar = show;
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON(summaryview->toggle_search), 
		show);
	if (show) {
		quicksearch_show(summaryview->quicksearch);
	} else {
		quicksearch_hide(summaryview->quicksearch);
		summary_grab_focus(summaryview);
	}
}

void summary_open_row(GtkSCTree *sctree, SummaryView *summaryview)
{
	if (FOLDER_SHOWS_TO_HDR(summaryview->folder_item))
		summary_reedit(summaryview);
	else
		summary_open_msg(summaryview);

	summaryview->display_msg = FALSE;
}

static void summary_tree_expanded(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				  SummaryView *summaryview)
{
	summary_set_row_marks(summaryview, node);
	if (prefs_common.bold_unread) {
		while (node) {
			GtkCMCTreeNode *next = GTK_CMCTREE_NODE_NEXT(node);
			if (GTK_CMCTREE_ROW(node)->children)
				summary_set_row_marks(summaryview, node);
			node = next;
		}
	}
}

static void summary_tree_collapsed(GtkCMCTree *ctree, GtkCMCTreeNode *node,
				   SummaryView *summaryview)
{
	gtk_sctree_select(GTK_SCTREE(ctree), node);
	summary_set_row_marks(summaryview, node);
}

static void summary_unselected(GtkCMCTree *ctree, GtkCMCTreeNode *row,
			       gint column, SummaryView *summaryview)
{
	if (summary_is_locked(summaryview)
	||  GTK_SCTREE(ctree)->selecting_range) {
		return;
	}

	summary_status_show(summaryview);
}

static void summary_selected(GtkCMCTree *ctree, GtkCMCTreeNode *row,
			     gint column, SummaryView *summaryview)
{
	GList *list, *cur;
	MessageView *msgview;
	MsgInfo *msginfo;
	gboolean marked_unread = FALSE;

	if (summary_is_locked(summaryview)
	&& !GTK_SCTREE(ctree)->selecting_range
	&& summaryview->messageview
	&& summaryview->messageview->mimeview
	&& summaryview->messageview->mimeview->type == MIMEVIEW_TEXT
	&& summaryview->messageview->mimeview->textview->loading) {
		PostponedSelectData *data = g_new0(PostponedSelectData, 1);
		summaryview->messageview->mimeview->textview->stop_loading = TRUE;
		
		data->ctree = ctree;
		data->row = row;
		data->node = NULL;
		data->column = column;
		data->summaryview = summaryview;
		debug_print("postponing open of message till end of load\n");
		g_timeout_add(100, summary_select_retry, data);
		return;
	}
	if (summary_is_locked(summaryview)
	||  GTK_SCTREE(ctree)->selecting_range) {
		return;
	}

	summary_status_show(summaryview);

	if (GTK_CMCLIST(ctree)->selection &&
	    GTK_CMCLIST(ctree)->selection->next) {
		summaryview->display_msg = FALSE;
		summary_set_menu_sensitive(summaryview);
		toolbar_main_set_sensitive(summaryview->mainwin);
		return;
	}

	summaryview->selected = row;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo != NULL);

	main_create_mailing_list_menu (summaryview->mainwin, msginfo);
	toolbar_set_learn_button
		(summaryview->mainwin->toolbar,
		 MSG_IS_SPAM(msginfo->flags)?LEARN_HAM:LEARN_SPAM);

	switch (column < 0 ? column : summaryview->col_state[column].type) {
	case S_COL_MARK:
		if (!MSG_IS_DELETED(msginfo->flags) &&
		    !MSG_IS_MOVE(msginfo->flags) &&
		    !MSG_IS_COPY(msginfo->flags)) {
			if (MSG_IS_MARKED(msginfo->flags)) {
				summary_unmark_row(summaryview, row);
				summary_status_show(summaryview);
			} else {
				summary_mark_row(summaryview, row);
				summary_status_show(summaryview);
			}
		}
		break;
	case S_COL_STATUS:
		if (MSG_IS_UNREAD(msginfo->flags)) {
			summary_mark_row_as_read(summaryview, row);
			summary_status_show(summaryview);
		} else if (MSG_IS_SPAM(msginfo->flags)) {
				if (procmsg_spam_learner_learn(msginfo, NULL, FALSE) == 0)
					summary_msginfo_unset_flags(msginfo, MSG_SPAM, 0);
				else
					log_error(LOG_PROTOCOL, _("An error happened while learning.\n"));
		} else if (!MSG_IS_REPLIED(msginfo->flags) &&
			 !MSG_IS_FORWARDED(msginfo->flags)) {
			marked_unread = TRUE;
		} else if (MSG_IS_REPLIED(msginfo->flags)) {
			summary_find_answers(summaryview, msginfo);
			return;
		} 
		break;
	case S_COL_LOCKED:
		if (MSG_IS_LOCKED(msginfo->flags)) {
			summary_unlock_row(summaryview, row);
			summary_status_show(summaryview);
		}
		else {
			summary_lock_row(summaryview, row);
			summary_status_show(summaryview);
		}
		break;
	default:
		break;
	}

	list = messageview_get_msgview_list();
	for (cur = list; cur != NULL; cur = cur->next) {
		msgview = (MessageView *) cur->data;
		
		if (msgview->new_window && msgview->update_needed) {
			MsgInfo *new_msginfo = summary_get_selected_msg(summaryview);
			messageview_show(msgview, new_msginfo, msgview->all_headers);
			msgview->update_needed = FALSE;		
		}
	}

	if (summaryview->display_msg ||
	    (prefs_common.always_show_msg &&
	     messageview_is_visible(summaryview->messageview))) {
		summaryview->display_msg = FALSE;
		if (summaryview->displayed != row) {
			summary_display_msg(summaryview, row);
			if (marked_unread) {
				summary_mark_row_as_unread(summaryview, row);
				summary_status_show(summaryview);
			} 
			return;
		}
	}
	
	if (marked_unread) {
		summary_mark_row_as_unread(summaryview, row);
		summary_status_show(summaryview);
	} 

	summary_set_menu_sensitive(summaryview);
	toolbar_main_set_sensitive(summaryview->mainwin);
}

static void summary_col_resized(GtkCMCList *clist, gint column, gint width,
				SummaryView *summaryview)
{
	SummaryColumnType type = summaryview->col_state[column].type;

	prefs_common.summary_col_size[type] = width;
}


/*
 * \brief get List of msginfo selected in SummaryView
 *
 * \param summaryview
 *
 * \return GSList holding MsgInfo
 */
GSList *summary_get_selection(SummaryView *summaryview)
{
	GList *sel = NULL;
	GSList *msginfo_list = NULL;
	
	cm_return_val_if_fail(summaryview != NULL, NULL);

	sel = GTK_CMCLIST(summaryview->ctree)->selection;

	cm_return_val_if_fail(sel != NULL, NULL);

	for ( ; sel != NULL; sel = sel->next)
		msginfo_list = 
			g_slist_prepend(msginfo_list, 
				       gtk_cmctree_node_get_row_data(GTK_CMCTREE(summaryview->ctree),
								   GTK_CMCTREE_NODE(sel->data)));
	return g_slist_reverse(msginfo_list);
}

static void summary_sort_by_column_click(SummaryView *summaryview,
					 FolderSortKey sort_key)
{
	GtkCMCTreeNode *node = NULL;
	START_TIMING("");
	if (summaryview->sort_key == sort_key)
		summary_sort(summaryview, sort_key,
			     summaryview->sort_type == SORT_ASCENDING
			     ? SORT_DESCENDING : SORT_ASCENDING);
	else
		summary_sort(summaryview, sort_key, SORT_ASCENDING);

	node = GTK_CMCTREE_NODE(GTK_CMCLIST(summaryview->ctree)->row_list);

	summary_freeze(summaryview);
	if (prefs_common.bold_unread) {
		while (node) {
			GtkCMCTreeNode *next = GTK_CMCTREE_NODE_NEXT(node);
			if (GTK_CMCTREE_ROW(node)->children)
				summary_set_row_marks(summaryview, node);
			node = next;
		}
	}
	summary_thaw(summaryview);
	END_TIMING();
}

static void summary_mark_clicked(GtkWidget *button, SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_MARK);
}

static void summary_status_clicked(GtkWidget *button, SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_STATUS);
}

static void summary_mime_clicked(GtkWidget *button, SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_MIME);
}

static void summary_num_clicked(GtkWidget *button, SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_NUMBER);
}

static void summary_size_clicked(GtkWidget *button, SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_SIZE);
}

static void summary_date_clicked(GtkWidget *button, SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_DATE);
}

static void summary_from_clicked(GtkWidget *button, SummaryView *summaryview)
{
	if (summaryview->col_state[summaryview->col_pos[S_COL_FROM]].visible)
		summary_sort_by_column_click(summaryview, SORT_BY_FROM);
	else
		summary_sort_by_column_click(summaryview, SORT_BY_TO);
}

static void summary_to_clicked(GtkWidget *button, SummaryView *summaryview)
{
	if (summaryview->col_state[summaryview->col_pos[S_COL_TO]].visible)
		summary_sort_by_column_click(summaryview, SORT_BY_TO);
	else
		summary_sort_by_column_click(summaryview, SORT_BY_FROM);
}

static void summary_subject_clicked(GtkWidget *button,
				    SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_SUBJECT);
}

static void summary_score_clicked(GtkWidget *button,
				  SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_SCORE);
}

static void summary_locked_clicked(GtkWidget *button,
				   SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_LOCKED);
}

static void summary_tags_clicked(GtkWidget *button,
				   SummaryView *summaryview)
{
	summary_sort_by_column_click(summaryview, SORT_BY_TAGS);
}

static void summary_start_drag(GtkWidget *widget, gint button, GdkEvent *event,
			       SummaryView *summaryview)
{
	GdkDragContext *context;

	cm_return_if_fail(summaryview != NULL);
	cm_return_if_fail(summaryview->folder_item != NULL);
	cm_return_if_fail(summaryview->folder_item->folder != NULL);

	if (summaryview->selected == NULL) return;

	context = gtk_drag_begin(widget, summaryview->target_list,
				 GDK_ACTION_MOVE|GDK_ACTION_COPY|GDK_ACTION_DEFAULT, button, event);
	gtk_drag_set_icon_default(context);
	if (prefs_common.layout_mode == SMALL_LAYOUT) {
		GtkWidget *paned = gtk_widget_get_parent(GTK_WIDGET_PTR(summaryview));
		if (paned && GTK_IS_PANED(paned)) {
	        	mainwindow_reset_paned(GTK_PANED(paned));
		}
	}
}

static gboolean summary_return_to_list(void *data)
{
	SummaryView *summaryview = (SummaryView *)data;
	mainwindow_enter_folder(summaryview->mainwin);
	return FALSE;
}

static void summary_drag_end	   (GtkWidget	    *widget, 
				   GdkDragContext   *drag_context,
                                   SummaryView 	    *summaryview)
{
	if (prefs_common.layout_mode == SMALL_LAYOUT) {
		g_timeout_add(250, summary_return_to_list, summaryview);
	}
}

static void summary_drag_data_get(GtkWidget        *widget,
				  GdkDragContext   *drag_context,
				  GtkSelectionData *selection_data,
				  guint             info,
				  guint             time,
				  SummaryView      *summaryview)
{
	if (info == TARGET_MAIL_URI_LIST) {
		GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
		GList *cur;
		MsgInfo *msginfo;
		gchar *mail_list = NULL, *tmp1, *tmp2;

		for (cur = GTK_CMCLIST(ctree)->selection;
		     cur != NULL && cur->data != NULL; cur = cur->next) {
			msginfo = gtk_cmctree_node_get_row_data
				(ctree, GTK_CMCTREE_NODE(cur->data));
			tmp2 = procmsg_get_message_file(msginfo);
			if (!tmp2) continue;
			if (msginfo->subject) {
				gchar *san_subject = g_strdup(msginfo->subject);
				gchar *dest = NULL;
				subst_for_filename(san_subject);
				dest = g_strdup_printf("%s%s%s.%d.txt",
						get_tmp_dir(),
						G_DIR_SEPARATOR_S,
						san_subject, msginfo->msgnum);
				g_free(san_subject);
				san_subject = g_filename_from_utf8(dest, -1, NULL, NULL, NULL);
				g_free(dest);
				dest = san_subject;
				if (copy_file(tmp2, dest, TRUE) == 0) {
					g_free(tmp2);
					tmp2 = dest;
				}
			} 
			tmp1 = g_filename_to_uri(tmp2, NULL, NULL);
			g_free(tmp2);
			tmp2 = g_strconcat(tmp1, "\r\n", NULL);
			g_free(tmp1);
			tmp1 = tmp2;

			if (!mail_list) {
				mail_list = tmp1;
			} else {
				tmp2 = g_strconcat(mail_list, tmp1, NULL);
				g_free(mail_list);
				g_free(tmp1);
				mail_list = tmp2;
			}
		}

		if (mail_list != NULL) {
			gtk_selection_data_set(selection_data,
					       gtk_selection_data_get_target(selection_data), 8,
					       mail_list, strlen(mail_list));
			g_free(mail_list);
		} 
	} else if (info == TARGET_DUMMY) {
		if (GTK_CMCLIST(summaryview->ctree)->selection)
			gtk_selection_data_set(selection_data,
					       gtk_selection_data_get_target(selection_data), 8,
					       "Dummy-Summaryview", 
					       strlen("Dummy-Summaryview")+1);
	} else if (info == TARGET_MAIL_CM_PATH_LIST) {
		/* content: folder_item_identifier\nmsgid1\nmsgid2\nmsgid3 */

		GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
		GList *cur;
		MsgInfo *msginfo;
		gchar *path_list = NULL;

		/* identifier */
		if(GTK_CMCLIST(ctree)->selection != NULL) {
			msginfo = gtk_cmctree_node_get_row_data(ctree, GTK_CMCTREE_NODE(GTK_CMCLIST(ctree)->selection->data));
            if(msginfo && msginfo->folder)
              path_list = folder_item_get_identifier(msginfo->folder);
		}

		for (cur = GTK_CMCLIST(ctree)->selection;
		     cur != NULL && cur->data != NULL; cur = cur->next) {
			gchar *tmp;

			msginfo = gtk_cmctree_node_get_row_data(ctree, GTK_CMCTREE_NODE(cur->data));
            if(!msginfo)
              continue;
			tmp = path_list;
			path_list = g_strconcat(path_list, "\n", (msginfo->msgid ? msginfo->msgid : "unknown"), NULL);
			g_free(tmp);
		}

		if (path_list != NULL) {
			gtk_selection_data_set(selection_data,
					       gtk_selection_data_get_target(selection_data), 8,
					       path_list, strlen(path_list));
			g_free(path_list);
		}
    }
}

static gboolean summary_drag_motion_cb(GtkWidget      *widget,
					  GdkDragContext *context,
					  gint            x,
					  gint            y,
					  guint           time,
					  SummaryView	 *summaryview)
{
	FolderItem *item = summaryview->folder_item;
	if (!(item && item->folder && folder_item_parent(item) != NULL
		    && FOLDER_CLASS(item->folder)->add_msg != NULL)) {
		gdk_drag_status(context, 0, time);
		return FALSE;
	} else if (gtk_drag_get_source_widget(context) ==
		mainwindow_get_mainwindow()->folderview->ctree) {
		/* no folders */
		gdk_drag_status(context, 0, time);
		return FALSE;
	} else if (gtk_drag_get_source_widget(context) ==
		summaryview->ctree) {
		/* not from same folder */
		gdk_drag_status(context, 0, time);
		return FALSE;
	} else {
		gdk_drag_status(context, GDK_ACTION_COPY, time);
		return TRUE;
	}
}

static void summary_drag_data_received(GtkWidget        *widget,
					GdkDragContext   *drag_context,
					gint              x,
					gint              y,
					GtkSelectionData *data,
					guint             info,
					guint             time,
					SummaryView       *summaryview)
{
	if (info == TARGET_MAIL_URI_LIST) {
		FolderItem *item = summaryview->folder_item;
		if (!item) {
			gtk_drag_finish(drag_context, FALSE, FALSE, time);			
			return;
		} else {
			folderview_finish_dnd(gtk_selection_data_get_data(data),
				drag_context, time, item);
		}
	}
}


/* custom compare functions for sorting */

static gint summary_cmp_by_date(GtkCMCList *clist,
		      gconstpointer ptr1, gconstpointer ptr2)
{
	MsgInfo *msginfo1 = ((GtkCMCListRow *)ptr1)->data;
	MsgInfo *msginfo2 = ((GtkCMCListRow *)ptr2)->data;
	gint res;
	if (!msginfo1 || !msginfo2)
		return -1;

	res = (msginfo1->date_t - msginfo2->date_t);
	if (res == 0)
		res = msginfo1->msgnum - msginfo2->msgnum;
	return res;
}

#define CMP_FUNC_DEF(func_name, val)					 \
static gint func_name(GtkCMCList *clist,				 \
		      gconstpointer ptr1, gconstpointer ptr2)		 \
{									 \
	MsgInfo *msginfo1 = ((GtkCMCListRow *)ptr1)->data;		 \
	MsgInfo *msginfo2 = ((GtkCMCListRow *)ptr2)->data;		 \
	gint res;							 \
	if (!msginfo1 || !msginfo2)					 \
		return -1;						 \
									 \
	res = (val);							 \
	return (res != 0) ? res:summary_cmp_by_date(clist, ptr1, ptr2);	 \
}

CMP_FUNC_DEF(summary_cmp_by_mark,
	     MSG_IS_MARKED(msginfo1->flags) - MSG_IS_MARKED(msginfo2->flags))
CMP_FUNC_DEF(summary_cmp_by_status,
	     (-(MSG_IS_SPAM(msginfo1->flags))+(MSG_IS_UNREAD(msginfo1->flags)<<1)+(MSG_IS_NEW(msginfo1->flags)<<2)) 
	     - (-(MSG_IS_SPAM(msginfo2->flags))+(MSG_IS_UNREAD(msginfo2->flags)<<1)+(MSG_IS_NEW(msginfo2->flags)<<2)) )
CMP_FUNC_DEF(summary_cmp_by_mime,
	     MSG_IS_WITH_ATTACHMENT(msginfo1->flags) - MSG_IS_WITH_ATTACHMENT(msginfo2->flags))
CMP_FUNC_DEF(summary_cmp_by_label,
	     MSG_GET_COLORLABEL(msginfo1->flags) -
	     MSG_GET_COLORLABEL(msginfo2->flags))
CMP_FUNC_DEF(summary_cmp_by_locked,
	     MSG_IS_LOCKED(msginfo1->flags) - MSG_IS_LOCKED(msginfo2->flags))

CMP_FUNC_DEF(summary_cmp_by_num, msginfo1->msgnum - msginfo2->msgnum)
CMP_FUNC_DEF(summary_cmp_by_size, msginfo1->size - msginfo2->size)

#undef CMP_FUNC_DEF

static gint summary_cmp_by_subject(GtkCMCList *clist,
				   gconstpointer ptr1,
				   gconstpointer ptr2)
{
	MsgInfo *msginfo1 = ((GtkCMCListRow *)ptr1)->data;
	MsgInfo *msginfo2 = ((GtkCMCListRow *)ptr2)->data;
	gint res;

	if (!msginfo1->subject)
		return (msginfo2->subject != NULL);
	if (!msginfo2->subject)
		return -1;

	res = subject_compare_for_sort
		(msginfo1->subject, msginfo2->subject);
	return (res != 0)? res: summary_cmp_by_date(clist, ptr1, ptr2);
}

static gint summary_cmp_by_thread_date(GtkCMCList *clist,
				   gconstpointer ptr1,
				   gconstpointer ptr2)
{
	MsgInfo *msginfo1 = ((GtkCMCListRow *)ptr1)->data;
	MsgInfo *msginfo2 = ((GtkCMCListRow *)ptr2)->data;
	gint thread_diff = msginfo1->thread_date - msginfo2->thread_date;
	
	if (msginfo1->thread_date > 0 && msginfo2->thread_date > 0)
		return thread_diff;
	else 
		return msginfo1->date_t - msginfo2->date_t;
}

static gint summary_cmp_by_from(GtkCMCList *clist, gconstpointer ptr1,
				gconstpointer ptr2)
{
	const gchar *str1, *str2;
	const GtkCMCListRow *r1 = (const GtkCMCListRow *) ptr1;
	const GtkCMCListRow *r2 = (const GtkCMCListRow *) ptr2;
	MsgInfo *msginfo1 = ((GtkCMCListRow *)ptr1)->data;
	MsgInfo *msginfo2 = ((GtkCMCListRow *)ptr2)->data;
	const SummaryView *sv = g_object_get_data(G_OBJECT(clist), "summaryview");
	gint res;

	cm_return_val_if_fail(sv, -1);
	if (sv->col_state[sv->col_pos[S_COL_FROM]].visible) {
		str1 = GTK_CMCELL_TEXT(r1->cell[sv->col_pos[S_COL_FROM]])->text;
		str2 = GTK_CMCELL_TEXT(r2->cell[sv->col_pos[S_COL_FROM]])->text;
	} else {
		str1 = msginfo1->from;
		str2 = msginfo2->from;
	}

	if (!str1)
		return str2 != NULL;
 
	if (!str2)
 		return -1;
 
	res = g_utf8_collate(str1, str2);
	return (res != 0)? res: summary_cmp_by_date(clist, ptr1, ptr2);
}
 
static gint summary_cmp_by_to(GtkCMCList *clist, gconstpointer ptr1,
				gconstpointer ptr2)
{
	const gchar *str1, *str2;
	const GtkCMCListRow *r1 = (const GtkCMCListRow *) ptr1;
	const GtkCMCListRow *r2 = (const GtkCMCListRow *) ptr2;
	MsgInfo *msginfo1 = ((GtkCMCListRow *)ptr1)->data;
	MsgInfo *msginfo2 = ((GtkCMCListRow *)ptr2)->data;
	const SummaryView *sv = g_object_get_data(G_OBJECT(clist), "summaryview");
	gint res;
	cm_return_val_if_fail(sv, -1);
	
	if (sv->col_state[sv->col_pos[S_COL_TO]].visible) {
		str1 = GTK_CMCELL_TEXT(r1->cell[sv->col_pos[S_COL_TO]])->text;
		str2 = GTK_CMCELL_TEXT(r2->cell[sv->col_pos[S_COL_TO]])->text;
	} else {
		str1 = msginfo1->to;
		str2 = msginfo2->to;
	}

	if (!str1)
		return str2 != NULL;
 
	if (!str2)
 		return -1;
 
	res = g_utf8_collate(str1, str2);
	return (res != 0)? res: summary_cmp_by_date(clist, ptr1, ptr2);
}
 
static gint summary_cmp_by_tags(GtkCMCList *clist, gconstpointer ptr1,
				gconstpointer ptr2)
{
	gchar *str1, *str2;
	const GtkCMCListRow *r1 = (const GtkCMCListRow *) ptr1;
	const GtkCMCListRow *r2 = (const GtkCMCListRow *) ptr2;
	const SummaryView *sv = g_object_get_data(G_OBJECT(clist), "summaryview");
	MsgInfo *msginfo1 = ((GtkCMCListRow *)ptr1)->data;
	MsgInfo *msginfo2 = ((GtkCMCListRow *)ptr2)->data;
	gint res;
	cm_return_val_if_fail(sv, -1);
	
	if (sv->col_state[sv->col_pos[S_COL_TAGS]].visible) {
		str1 = g_strdup(GTK_CMCELL_TEXT(r1->cell[sv->col_pos[S_COL_TAGS]])->text);
		str2 = g_strdup(GTK_CMCELL_TEXT(r2->cell[sv->col_pos[S_COL_TAGS]])->text);
	} else {
		str1 = procmsg_msginfo_get_tags_str(msginfo1);
		str2 = procmsg_msginfo_get_tags_str(msginfo2);
	}

	if (!str1) {
		res = (str2 != NULL);
		g_free(str2);
		return res;
	}
	if (!str2) {
		g_free(str1);
 		return -1;
	}
 
	res = g_utf8_collate(str1, str2);
	g_free(str1);
	g_free(str2);
	return (res != 0)? res: summary_cmp_by_date(clist, ptr1, ptr2);
}
 
static gint summary_cmp_by_simplified_subject
	(GtkCMCList *clist, gconstpointer ptr1, gconstpointer ptr2)
{
	const FolderItemPrefs *prefs;
	const gchar *str1, *str2;
	const GtkCMCListRow *r1 = (const GtkCMCListRow *) ptr1;
	const GtkCMCListRow *r2 = (const GtkCMCListRow *) ptr2;
	const MsgInfo *msginfo1 = r1->data;
	const MsgInfo *msginfo2 = r2->data;
	const SummaryView *sv = g_object_get_data(G_OBJECT(clist), "summaryview");
	gint res;

	cm_return_val_if_fail(sv, -1);
	cm_return_val_if_fail(msginfo1 != NULL && msginfo2 != NULL, -1);
	
	if (sv->col_state[sv->col_pos[S_COL_SUBJECT]].visible) {
		str1 = GTK_CMCELL_TEXT(r1->cell[sv->col_pos[S_COL_SUBJECT]])->text;
		str2 = GTK_CMCELL_TEXT(r2->cell[sv->col_pos[S_COL_SUBJECT]])->text;
	} else {
		str1 = msginfo1->subject;
		str2 = msginfo2->subject;
	}

	if (!str1)
		return str2 != NULL;

	if (!str2)
		return -1;

	prefs = msginfo1->folder->prefs;
	if (!prefs)
		prefs = msginfo2->folder->prefs;
	if (!prefs)
		return -1;
	
	res = subject_compare_for_sort(str1, str2);
	return (res != 0)? res: summary_cmp_by_date(clist, ptr1, ptr2);
}

static gint summary_cmp_by_score(GtkCMCList *clist,
				 gconstpointer ptr1, gconstpointer ptr2)
{
	MsgInfo *msginfo1 = ((GtkCMCListRow *)ptr1)->data;
	MsgInfo *msginfo2 = ((GtkCMCListRow *)ptr2)->data;
	int diff;

	/* if score are equal, sort by date */

	diff = msginfo1->score - msginfo2->score;
	if (diff != 0)
		return diff;
	else
		return summary_cmp_by_date(clist, ptr1, ptr2);
}

static void summary_ignore_thread_func(GtkCMCTree *ctree, GtkCMCTreeNode *row, gpointer data)
{
	SummaryView *summaryview = (SummaryView *) data;
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);

	summary_msginfo_unset_flags(msginfo, MSG_WATCH_THREAD, 0);
	summary_msginfo_change_flags(msginfo, MSG_IGNORE_THREAD, 0, MSG_NEW | MSG_UNREAD, 0);

	summary_set_row_marks(summaryview, row);
	debug_print("Message %d is marked as ignore thread\n",
	    msginfo->msgnum);
}

void summary_ignore_thread(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	START_LONG_OPERATION(summaryview, FALSE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		gtk_cmctree_pre_recursive(ctree, GTK_CMCTREE_NODE(cur->data), 
					GTK_CMCTREE_FUNC(summary_ignore_thread_func), 
					summaryview);

	END_LONG_OPERATION(summaryview);

	summary_status_show(summaryview);
}

static void summary_unignore_thread_func(GtkCMCTree *ctree, GtkCMCTreeNode *row, gpointer data)
{
	SummaryView *summaryview = (SummaryView *) data;
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);

	summary_msginfo_unset_flags(msginfo, MSG_IGNORE_THREAD, 0);

	summary_set_row_marks(summaryview, row);
	debug_print("Message %d is marked as unignore thread\n",
	    msginfo->msgnum);
}

void summary_unignore_thread(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	START_LONG_OPERATION(summaryview, FALSE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		gtk_cmctree_pre_recursive(ctree, GTK_CMCTREE_NODE(cur->data), 
					GTK_CMCTREE_FUNC(summary_unignore_thread_func), 
					summaryview);

	END_LONG_OPERATION(summaryview);

	summary_status_show(summaryview);
}

static void summary_check_ignore_thread_func
		(GtkCMCTree *ctree, GtkCMCTreeNode *row, gpointer data)
{
	MsgInfo *msginfo;
	gint *found_ignore = (gint *) data;

	if (*found_ignore) return;
	else {
		msginfo = gtk_cmctree_node_get_row_data(ctree, row);
		*found_ignore = msginfo && MSG_IS_IGNORE_THREAD(msginfo->flags);
	}		
}

void summary_toggle_ignore_thread(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gint found_ignore = 0;

	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		gtk_cmctree_pre_recursive(ctree, GTK_CMCTREE_NODE(cur->data),
					GTK_CMCTREE_FUNC(summary_check_ignore_thread_func),
					&found_ignore);

	if (found_ignore) 
		summary_unignore_thread(summaryview);
	else 
		summary_ignore_thread(summaryview);
}

static void summary_watch_thread_func(GtkCMCTree *ctree, GtkCMCTreeNode *row, gpointer data)
{
	SummaryView *summaryview = (SummaryView *) data;
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);

	summary_msginfo_change_flags(msginfo, MSG_WATCH_THREAD, 0, MSG_IGNORE_THREAD, 0);

	summary_set_row_marks(summaryview, row);
	debug_print("Message %d is marked as watch thread\n",
	    msginfo->msgnum);
}

void summary_watch_thread(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	START_LONG_OPERATION(summaryview, FALSE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		gtk_cmctree_pre_recursive(ctree, GTK_CMCTREE_NODE(cur->data), 
					GTK_CMCTREE_FUNC(summary_watch_thread_func), 
					summaryview);

	END_LONG_OPERATION(summaryview);

	summary_status_show(summaryview);
}

static void summary_unwatch_thread_func(GtkCMCTree *ctree, GtkCMCTreeNode *row, gpointer data)
{
	SummaryView *summaryview = (SummaryView *) data;
	MsgInfo *msginfo;

	msginfo = gtk_cmctree_node_get_row_data(ctree, row);
	cm_return_if_fail(msginfo);

	summary_msginfo_unset_flags(msginfo, MSG_WATCH_THREAD, 0);

	summary_set_row_marks(summaryview, row);
	debug_print("Message %d is marked as unwatch thread\n",
	    msginfo->msgnum);
}

void summary_unwatch_thread(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gboolean froze = FALSE;

	START_LONG_OPERATION(summaryview, FALSE);
	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		gtk_cmctree_pre_recursive(ctree, GTK_CMCTREE_NODE(cur->data), 
					GTK_CMCTREE_FUNC(summary_unwatch_thread_func), 
					summaryview);

	END_LONG_OPERATION(summaryview);

	summary_status_show(summaryview);
}

static void summary_check_watch_thread_func
		(GtkCMCTree *ctree, GtkCMCTreeNode *row, gpointer data)
{
	MsgInfo *msginfo;
	gint *found_watch = (gint *) data;

	if (*found_watch) return;
	else {
		msginfo = gtk_cmctree_node_get_row_data(ctree, row);
		*found_watch = msginfo && MSG_IS_WATCH_THREAD(msginfo->flags);
	}		
}

void summary_toggle_watch_thread(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE(summaryview->ctree);
	GList *cur;
	gint found_watch = 0;

	for (cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next)
		gtk_cmctree_pre_recursive(ctree, GTK_CMCTREE_NODE(cur->data),
					GTK_CMCTREE_FUNC(summary_check_watch_thread_func),
					&found_watch);

	if (found_watch) 
		summary_unwatch_thread(summaryview);
	else 
		summary_watch_thread(summaryview);
}

void summary_toggle_show_read_messages(SummaryView *summaryview)
{
	FolderItemUpdateData source;
	if (summaryview->folder_item->hide_read_msgs)
 		summaryview->folder_item->hide_read_msgs = 0;
 	else
 		summaryview->folder_item->hide_read_msgs = 1;

	source.item = summaryview->folder_item;
	source.update_flags = F_ITEM_UPDATE_NAME;
	source.msg = NULL;
	hooks_invoke(FOLDER_ITEM_UPDATE_HOOKLIST, &source);
 	summary_show(summaryview, summaryview->folder_item);
}
 
void summary_toggle_show_del_messages(SummaryView *summaryview)
{
	FolderItemUpdateData source;
	if (summaryview->folder_item->hide_del_msgs)
 		summaryview->folder_item->hide_del_msgs = 0;
 	else
 		summaryview->folder_item->hide_del_msgs = 1;

	source.item = summaryview->folder_item;
	source.update_flags = F_ITEM_UPDATE_NAME;
	source.msg = NULL;
	hooks_invoke(FOLDER_ITEM_UPDATE_HOOKLIST, &source);
 	summary_show(summaryview, summaryview->folder_item);
}
 
void summary_toggle_show_read_threads(SummaryView *summaryview)
{
	FolderItemUpdateData source;
	if (summaryview->folder_item->hide_read_threads)
 		summaryview->folder_item->hide_read_threads = 0;
 	else
 		summaryview->folder_item->hide_read_threads = 1;

	source.item = summaryview->folder_item;
	source.update_flags = F_ITEM_UPDATE_NAME;
	source.msg = NULL;
	hooks_invoke(FOLDER_ITEM_UPDATE_HOOKLIST, &source);
 	summary_show(summaryview, summaryview->folder_item);
}
 
static void summary_set_hide_read_msgs_menu (SummaryView *summaryview,
 					     guint action)
{
 	GtkWidget *widget;

 	widget = gtk_ui_manager_get_widget(summaryview->mainwin->ui_manager, "/Menu/View/HideReadMessages");
 	g_object_set_data(G_OBJECT(widget), "dont_toggle",
 			  GINT_TO_POINTER(1));
 	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(widget), action);
 	g_object_set_data(G_OBJECT(widget), "dont_toggle",
 			  GINT_TO_POINTER(0));
}

static void summary_set_hide_read_threads_menu (SummaryView *summaryview,
 					     guint action)
{
 	GtkWidget *widget;

 	widget = gtk_ui_manager_get_widget(summaryview->mainwin->ui_manager, "/Menu/View/HideReadThreads");
 	g_object_set_data(G_OBJECT(widget), "dont_toggle",
 			  GINT_TO_POINTER(1));
 	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(widget), action);
 	g_object_set_data(G_OBJECT(widget), "dont_toggle",
 			  GINT_TO_POINTER(0));
}

static void summary_set_hide_del_msgs_menu (SummaryView *summaryview,
 					     guint action)
{
 	GtkWidget *widget;

 	widget = gtk_ui_manager_get_widget(summaryview->mainwin->ui_manager, "/Menu/View/HideDelMessages");
 	g_object_set_data(G_OBJECT(widget), "dont_toggle",
 			  GINT_TO_POINTER(1));
 	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(widget), action);
 	g_object_set_data(G_OBJECT(widget), "dont_toggle",
 			  GINT_TO_POINTER(0));
}

void summary_reflect_prefs_pixmap_theme(SummaryView *summaryview)
{
	GtkWidget *ctree = summaryview->ctree;
	GtkWidget *pixmap; 

	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_MARK, &markxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_DELETED, &deletedxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_NEW, &newxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_UNREAD, &unreadxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_REPLIED, &repliedxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_FORWARDED, &forwardedxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_REPLIED_AND_FORWARDED, &repliedandforwardedxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_CLIP, &clipxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_LOCKED, &lockedxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_IGNORETHREAD, &ignorethreadxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_WATCHTHREAD, &watchthreadxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_CLIP_KEY, &clipkeyxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_KEY, &keyxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_KEY_SIGN, &keysignxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_GPG_SIGNED, &gpgsignedxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_CLIP_GPG_SIGNED, &clipgpgsignedxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_SPAM, &spamxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_MOVED, &movedxpm);
	stock_pixbuf_gdk(ctree, STOCK_PIXMAP_COPIED, &copiedxpm);

	summary_set_folder_pixmap(summaryview, STOCK_PIXMAP_DIR_OPEN);

	pixmap = stock_pixmap_widget(summaryview->hbox, STOCK_PIXMAP_QUICKSEARCH);
	gtk_container_remove (GTK_CONTAINER(summaryview->toggle_search), 
			      summaryview->quick_search_pixmap);
	gtk_container_add(GTK_CONTAINER(summaryview->toggle_search), pixmap);
	gtk_widget_show(pixmap);
	summaryview->quick_search_pixmap = pixmap;

#ifdef GENERIC_UMPC
	pixmap = stock_pixmap_widget(summaryview->hbox, STOCK_PIXMAP_SELECTION);
	gtk_container_remove (GTK_CONTAINER(summaryview->multiple_sel_togbtn), 
			      summaryview->multiple_sel_image);
	gtk_container_add(GTK_CONTAINER(summaryview->multiple_sel_togbtn), pixmap);
	gtk_widget_show(pixmap);
	summaryview->multiple_sel_togbtn = pixmap;
#endif

	folderview_unselect(summaryview->folderview);
	folderview_select(summaryview->folderview, summaryview->folder_item);
	summary_set_column_titles(summaryview);
}

void summary_reflect_prefs_custom_colors(SummaryView *summaryview)
{
	GtkMenuShell *menu;
	GList *children, *cur;

	/* re-create colorlabel submenu */
	menu = GTK_MENU_SHELL(summaryview->colorlabel_menu);
	cm_return_if_fail(menu != NULL);

	/* clear items. get item pointers. */
	children = gtk_container_get_children(GTK_CONTAINER(menu));
	for (cur = children; cur != NULL && cur->data != NULL; cur = cur->next) {
		g_signal_handlers_disconnect_matched
			 (gtk_ui_manager_get_accel_group(summaryview->mainwin->ui_manager), 
			 G_SIGNAL_MATCH_DATA|G_SIGNAL_MATCH_FUNC,
			 0, 0, NULL, mainwin_accel_changed_cb, cur->data);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(cur->data), NULL);
	}
	g_list_free(children);
	summary_colorlabel_menu_create(summaryview, TRUE);
}

/*
 * Harvest addresses for selected messages in summary view.
 */
void summary_harvest_address(SummaryView *summaryview)
{
	GtkCMCTree *ctree = GTK_CMCTREE( summaryview->ctree );
	GList *cur;
	GList *msgList;
	MsgInfo *msginfo;

	msgList = NULL;
	for( cur = GTK_CMCLIST(ctree)->selection; cur != NULL && cur->data != NULL; cur = cur->next ) {
		msginfo = gtk_cmctree_node_get_row_data( ctree, GTK_CMCTREE_NODE(cur->data) );
		if (!msginfo)
			continue;
		msgList = g_list_append( msgList, GUINT_TO_POINTER( msginfo->msgnum ) );
	}

	addressbook_harvest( summaryview->folder_item, TRUE, msgList );

	g_list_free( msgList );
}

#ifndef G_OS_WIN32
static regex_t *summary_compile_simplify_regexp(gchar *simplify_subject_regexp)
{
	int err;
	gchar buf[BUFFSIZE];
	regex_t *preg = NULL;
	
	preg = g_new0(regex_t, 1);

	err = string_match_precompile(simplify_subject_regexp, 
				      preg, REG_EXTENDED);
	if (err) {
		regerror(err, preg, buf, BUFFSIZE);
		alertpanel_error(_("Regular expression (regexp) error:\n%s"), buf);
		g_free(preg);
		preg = NULL;
	}
	
	return preg;
}
#endif
void summary_set_prefs_from_folderitem(SummaryView *summaryview, FolderItem *item)
{
	FolderSortKey sort_key;
	FolderSortType sort_type;
	cm_return_if_fail(summaryview != NULL);
	cm_return_if_fail(item != NULL);

	/* Subject simplification */
#ifndef G_OS_WIN32
	if(summaryview->simplify_subject_preg) {
		regfree(summaryview->simplify_subject_preg);
		g_free(summaryview->simplify_subject_preg);
		summaryview->simplify_subject_preg = NULL;
	}
	if(item->prefs && item->prefs->simplify_subject_regexp && 
	   item->prefs->simplify_subject_regexp[0] && item->prefs->enable_simplify_subject)
		summaryview->simplify_subject_preg = summary_compile_simplify_regexp(item->prefs->simplify_subject_regexp);
#endif
	/* Sorting */
	sort_key = item->sort_key;
	sort_type = item->sort_type;

	folder_get_sort_type(item->folder, &sort_key, &sort_type);

	summaryview->sort_key = sort_key;
	summaryview->sort_type = sort_type;

	/* Threading */
	summaryview->threaded = item->threaded;
	summaryview->thread_collapsed = item->thread_collapsed;

	/* Scoring */
}

void summary_save_prefs_to_folderitem(SummaryView *summaryview, FolderItem *item)
{
	/* Sorting */
	item->sort_key = summaryview->sort_key;
	item->sort_type = summaryview->sort_type;

	/* Threading */
	item->threaded = summaryview->threaded;
	item->thread_collapsed = summaryview->thread_collapsed;
}

static gboolean summary_update_msg(gpointer source, gpointer data) 
{
	MsgInfoUpdate *msginfo_update = (MsgInfoUpdate *) source;
	SummaryView *summaryview = (SummaryView *)data;
	GtkCMCTreeNode *node;

	cm_return_val_if_fail(msginfo_update != NULL, TRUE);
	cm_return_val_if_fail(summaryview != NULL, FALSE);

	if (msginfo_update->msginfo->folder != summaryview->folder_item)
		return FALSE;

	if (msginfo_update->flags & MSGINFO_UPDATE_FLAGS) {
		node = gtk_cmctree_find_by_row_data(
				GTK_CMCTREE(summaryview->ctree), NULL, 
				msginfo_update->msginfo);

		if (node) 
			summary_set_row_marks(summaryview, node);
	}

	return FALSE;
}

void summary_update_unread(SummaryView *summaryview, FolderItem *removed_item)
{
	guint new, unread, unreadmarked, marked, total;
	guint replied, forwarded, locked, ignored, watched;
	static gboolean tips_initialized = FALSE;
#if !(GTK_CHECK_VERSION(2,12,0))
	GtkTooltips *tips = summaryview->tooltips;
#endif

	if (prefs_common.layout_mode != SMALL_LAYOUT) {
		if (tips_initialized) {
			summary_set_folder_pixmap(summaryview, STOCK_PIXMAP_DIR_OPEN);
			CLAWS_SET_TIP(summaryview->folder_pixmap_eventbox,
			     NULL);
			tips_initialized = FALSE;
		} 
		return;
	}
	folder_count_total_msgs(&new, &unread, &unreadmarked, &marked, &total,
				&replied, &forwarded, &locked, &ignored,
				&watched);
	if (removed_item) {
		total -= removed_item->total_msgs;
		new -= removed_item->new_msgs;
		unread -= removed_item->unread_msgs;
	}
	
	if (new > 0 || unread > 0) {
		tips_initialized = TRUE;
		summary_set_folder_pixmap(summaryview, STOCK_PIXMAP_DIR_OPEN_HRM);
		CLAWS_SET_TIP(summaryview->folder_pixmap_eventbox,
			     _("Go back to the folder list (You have unread messages)"));
	} else {
		tips_initialized = TRUE;
		summary_set_folder_pixmap(summaryview, STOCK_PIXMAP_DIR_OPEN);
		CLAWS_SET_TIP(summaryview->folder_pixmap_eventbox,
			     _("Go back to the folder list"));
	}
}

static gboolean summary_update_folder_item_hook(gpointer source, gpointer data)
{
	FolderItemUpdateData *hookdata = (FolderItemUpdateData *)source;
	SummaryView *summaryview = (SummaryView *)data;

	cm_return_val_if_fail(hookdata != NULL, FALSE);
	cm_return_val_if_fail(hookdata->item != NULL, FALSE);
	cm_return_val_if_fail(summaryview != NULL, FALSE);

	if (hookdata->update_flags & F_ITEM_UPDATE_NAME) {
		gchar *name = folder_item_get_name(hookdata->item);
		gtk_label_set_text(GTK_LABEL(summaryview->statlabel_folder), name);
		g_free(name);
	}
	summary_update_unread(summaryview, NULL);

	return FALSE;
}

static gboolean summary_update_folder_hook(gpointer source, gpointer data)
{
	FolderUpdateData *hookdata;
	SummaryView *summaryview = (SummaryView *)data;
	hookdata = source;
	if (hookdata->update_flags & FOLDER_REMOVE_FOLDERITEM) {
		summary_update_unread(summaryview, hookdata->item);
	} else
		summary_update_unread(summaryview, NULL);

	return FALSE;
}

/*!
 *\brief	change summaryview to display your answer(s) to a message
 *
 *\param	summaryview The SummaryView ;)
 *\param	msginfo The message for which answers are searched
 *
 */
static void summary_find_answers (SummaryView *summaryview, MsgInfo *msg)
{
	FolderItem *sent_folder = NULL;
	PrefsAccount *account = NULL;
	GtkCMCTreeNode *node = NULL;
	char *buf = NULL;
	if (msg == NULL || msg->msgid == NULL)
		return;
	
	account = account_get_reply_account(msg, prefs_common.reply_account_autosel);
	if (account == NULL) 
		return;
	sent_folder = account_get_special_folder
				(account, F_OUTBOX);
	
	buf = g_strdup_printf("inreplyto matchcase \"%s\"", msg->msgid);

	if (sent_folder != summaryview->folder_item) {
		folderview_select(summaryview->mainwin->folderview, sent_folder);
	}
	
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(summaryview->toggle_search), TRUE);

	quicksearch_set(summaryview->quicksearch, ADVANCED_SEARCH_EXTENDED, buf);
	g_free(buf);

	node = gtk_cmctree_node_nth(GTK_CMCTREE(summaryview->ctree), 0);
	if (node)
		summary_select_node(summaryview, node, TRUE, TRUE);
}

gint summaryview_export_mbox_list(SummaryView *summaryview)
/* return values: -2 skipped, -1 error, 0 OK */
{
	GSList *list = summary_get_selected_msg_list(summaryview);
	gchar *mbox = filesel_select_file_save(_("Export to mbox file"), NULL);
	gint ret;
	
	if (mbox == NULL)
		return -2;
	if (list == NULL)
		return -1;
		
	ret = export_list_to_mbox(list, mbox);
	
	g_slist_free(list);
	g_free(mbox);
	
	return ret;
}

void summaryview_lock(SummaryView *summaryview, FolderItem *item)
{
	if (!summaryview || !summaryview->folder_item || !item) {
		return;
	}

	if (summaryview->folder_item->folder == item->folder) {
		gtk_widget_set_sensitive(summaryview->ctree, FALSE);
	}
}
void summaryview_unlock(SummaryView *summaryview, FolderItem *item)
{
	gtk_widget_set_sensitive(summaryview->ctree, TRUE);
}
