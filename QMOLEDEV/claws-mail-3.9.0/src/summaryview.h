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

#ifndef __SUMMARY_H__
#define __SUMMARY_H__

#include <sys/types.h>
#include <regex.h>

#include <glib.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>

#include "viewtypes.h"
typedef struct _SummaryColumnState	SummaryColumnState;

#define MAIL_MANUAL_FILTERING_HOOKLIST "mail_manual_filtering_hooklist"

typedef enum
{
	S_COL_MARK,
	S_COL_STATUS,
	S_COL_MIME,
	S_COL_SUBJECT,
	S_COL_FROM,
	S_COL_TO,
	S_COL_DATE,
	S_COL_SIZE,
	S_COL_NUMBER,
	S_COL_SCORE,
	S_COL_LOCKED,
	S_COL_TAGS
} SummaryColumnType;

#define N_SUMMARY_COLS	12

typedef enum
{
	SUMMARY_NONE,
	SUMMARY_SELECTED_NONE,
	SUMMARY_SELECTED_SINGLE,
	SUMMARY_SELECTED_MULTIPLE
} SummarySelection;

typedef enum
{
	TARGET_MAIL_URI_LIST,
	TARGET_DUMMY,
	TARGET_MAIL_CM_PATH_LIST,
} TargetInfo;

#include "mainwindow.h"
#include "compose.h"
#include "folder.h"
#include "gtksctree.h"
#include "prefs_filtering.h"
#include "quicksearch.h"

extern GtkTargetEntry summary_drag_types[3];

struct _SummaryColumnState
{
	SummaryColumnType type;
	gboolean visible;
};

struct _SummaryView
{
	GtkWidget *vbox;
	GtkWidget *mainwidget_book;
	GtkWidget *scrolledwin;
	GtkWidget *ctree;
	GtkWidget *hbox;
	GtkWidget *hbox_l;
	GtkWidget *hbox_spc;
	GtkWidget *stat_box;
	GtkWidget *stat_box2;
	GtkWidget *folder_pixmap;
	GtkWidget *folder_pixmap_eventbox;
	GtkWidget *statlabel_folder;
	GtkWidget *statlabel_select;
	GtkWidget *statlabel_msgs;
	GtkWidget *toggle_eventbox;
	GtkWidget *toggle_arrow;
#ifdef GENERIC_UMPC
	GtkWidget *multiple_sel_togbtn;
	GtkWidget *multiple_sel_image;
#endif
	GtkWidget *toggle_search;
	GtkWidget *quick_search_pixmap;
	GtkWidget *popupmenu;
	GtkWidget *colorlabel_menu;
	GtkWidget *tags_menu;

	GtkWidget *window;

	GtkCMCTreeNode *selected;
	GtkCMCTreeNode *displayed;

	gboolean display_msg;

	GdkColor color_important;
	SummaryColumnState col_state[N_SUMMARY_COLS];
	gint col_pos[N_SUMMARY_COLS];

	GdkColor color_marked;
	GdkColor color_dim;

	guint lock_count;

	MainWindow   *mainwin;
	FolderView   *folderview;
	HeaderView   *headerview;
	MessageView  *messageview;
	MessageView  *ext_messageview;
	QuickSearch  *quicksearch;

	FolderItem *folder_item;

	/* summaryview prefs */
	gint important_score;
	FolderSortKey sort_key;
	FolderSortType sort_type;
	guint threaded;
	guint thread_collapsed;

	/* Extra data for summaryview */
	regex_t *simplify_subject_preg;

	/* current message status */
	gint   unreadmarked;
	goffset total_size;
	gint   deleted;
	gint   moved;
	gint   copied;

/*
private:
*/
	/* table for looking up message-id */
	GHashTable *msgid_table;
	GHashTable *subject_table;

	/* list for moving/deleting messages */
	GSList *mlist;
	int msginfo_update_callback_id;

	/* update folder label when renaming */
	gint folder_item_update_callback_id;
	gint folder_update_callback_id;

	GtkTargetList *target_list; /* DnD */

	// folders with matches for recursive quicksearch queries
	GSList *recursive_matched_folders;
	FolderItem *search_root_folder;
	
#if !GTK_CHECK_VERSION(2,12,0)
	GtkTooltips *tooltips;
#endif
};

SummaryView	*summary_create(MainWindow *mainwin);
void summaryview_destroy(SummaryView *summaryview);
void summary_init		  (SummaryView		*summaryview);
gboolean summary_show		  (SummaryView		*summaryview,
				   FolderItem		*fitem);
void summary_clear_list		  (SummaryView		*summaryview);
void summary_clear_all		  (SummaryView		*summaryview);

void summary_lock		  (SummaryView		*summaryview);
void summary_unlock		  (SummaryView		*summaryview);
void summary_freeze		  (SummaryView		*summaryview);
void summary_thaw		  (SummaryView		*summaryview);
void summary_grab_focus		  (SummaryView		*summaryview);
GtkWidget *summary_get_main_widget(SummaryView 		*summaryview);
gboolean summary_is_locked	  (SummaryView		*summaryview);

SummarySelection summary_get_selection_type	(SummaryView	*summaryview);
MsgInfo *summary_get_selected_msg		(SummaryView *summaryview);
GSList *summary_get_selected_msg_list		(SummaryView	*summaryview);

void summary_select_prev_unread	  (SummaryView		*summaryview);
void summary_select_next_unread	  (SummaryView		*summaryview);
void summary_select_prev_new	  (SummaryView		*summaryview);
void summary_select_next_new	  (SummaryView		*summaryview);
void summary_select_prev_marked	  (SummaryView		*summaryview);
void summary_select_next_marked	  (SummaryView		*summaryview);
void summary_select_prev_labeled  (SummaryView		*summaryview);
void summary_select_next_labeled  (SummaryView		*summaryview);
void summary_select_parent        (SummaryView		*summaryview);
void summary_select_by_msgnum	  (SummaryView		*summaryview,
				   guint		 msgnum);
void summary_display_by_msgnum	  (SummaryView		*summaryview,
				   guint		 msgnum);
void summary_select_by_msg_list   (SummaryView		*summaryview, GSList *msginfos);
guint summary_get_current_msgnum  (SummaryView		*summaryview);
void summary_select_node	  (SummaryView		*summaryview,
				   GtkCMCTreeNode		*node,
				   gboolean		 display_msg,
				   gboolean		 do_refresh);

void summary_expand_threads	  (SummaryView		*summaryview);
void summary_collapse_threads	  (SummaryView		*summaryview);
void summary_toggle_ignore_thread (SummaryView		*summaryview);
void summary_toggle_watch_thread  (SummaryView		*summaryview);

void summary_filter		  (SummaryView		*summaryview,
				   gboolean		 selected_only);
void summary_filter_open          (SummaryView *summaryview,
				   PrefsFilterType type,
				   gint processing_rule);
void summary_msginfo_filter_open  (FolderItem * item, MsgInfo *msginfo,
				   PrefsFilterType type, gint processing_rule);

void summary_sort		  (SummaryView		*summaryview,
				   FolderSortKey	 sort_key,
				   FolderSortType	 sort_type);

void summary_delete		  (SummaryView		*summaryview);
void summary_delete_trash	  (SummaryView		*summaryview);

void summary_cancel               (SummaryView          *summaryview);

gboolean summary_execute	  (SummaryView		*summaryview);
gboolean summary_expunge	  (SummaryView		*summaryview);

void summary_attract_by_subject	  (SummaryView		*summaryview);

gint summary_write_cache	  (SummaryView		*summaryview);

gboolean summary_pass_key_press_event (SummaryView		*summaryview,
				   GdkEventKey		*event);

void summary_display_msg_selected (SummaryView		*summaryview,
				   gboolean		 all_headers);
void summary_redisplay_msg	  (SummaryView		*summaryview);
void summary_open_msg		  (SummaryView		*summaryview);
void summary_open_row		  (GtkSCTree *sctree, SummaryView *summaryview);
void summary_view_source	  (SummaryView		*summaryview);
void summary_reedit		  (SummaryView		*summaryview);
gboolean summary_step		  (SummaryView		*summaryview,
				   GtkScrollType	 type);
void summary_toggle_view	  (SummaryView		*summaryview);
void summary_set_marks_selected	  (SummaryView		*summaryview);
guint summary_get_msgnum	  (SummaryView		*summaryview,
				   GtkCMCTreeNode		*node);

void summary_move_selected_to	  (SummaryView		*summaryview,
				   FolderItem		*to_folder);
void summary_move_to		  (SummaryView		*summaryview);
void summary_copy_selected_to	  (SummaryView		*summaryview,
				   FolderItem		*to_folder);
GSList *summary_get_selection	  (SummaryView 		*summaryview);
void summary_copy_to		  (SummaryView		*summaryview);
void summary_save_as		  (SummaryView		*summaryview);
void summary_print		  (SummaryView		*summaryview);
void summary_mark		  (SummaryView		*summaryview);
void summary_unmark		  (SummaryView		*summaryview);
void summary_mark_as_unread	  (SummaryView		*summaryview);
void summary_mark_as_read	  (SummaryView		*summaryview);
void summary_msgs_lock		  (SummaryView		*summaryview);
void summary_msgs_unlock	  (SummaryView		*summaryview);
void summary_mark_all_read	  (SummaryView		*summaryview);
void summary_mark_as_spam	  (SummaryView 		*summaryview, 
				   guint 		 action, 
				   GtkWidget 		*widget);
void summary_ignore_thread	  (SummaryView 		*summaryview);
void summary_unignore_thread	  (SummaryView 		*summaryview);
void summary_watch_thread	  (SummaryView 		*summaryview);
void summary_unwatch_thread	  (SummaryView 		*summaryview);

void summary_add_address	  (SummaryView		*summaryview);
void summary_select_all		  (SummaryView		*summaryview);
void summary_unselect_all	  (SummaryView		*summaryview);
void summary_select_thread	  (SummaryView		*summaryview,
				   gboolean		 delete_thread);

void summary_set_colorlabel	  (SummaryView		*summaryview,
				   guint		 labelcolor,
				   GtkWidget		*widget);
void summary_set_tag		  (SummaryView		*summaryview,
				   gint			 tag_id,
				   GtkWidget		*widget);

void summary_set_column_order	  (SummaryView		*summaryview);

void summary_toggle_show_read_messages
				  (SummaryView *summaryview);
void summary_toggle_show_read_threads
				  (SummaryView *summaryview);
void summary_toggle_show_del_messages
				  (SummaryView *summaryview);

void summary_toggle_view_real	  (SummaryView	*summaryview);

void summary_reflect_prefs_pixmap_theme
                                  (SummaryView *summaryview);
void summary_reflect_prefs_custom_colors(SummaryView *summaryview);
void summary_reflect_tags_changes(SummaryView *summaryview);
void summary_harvest_address      (SummaryView *summaryview);
void summary_set_prefs_from_folderitem
                                  (SummaryView *summaryview, FolderItem *item);
void summary_save_prefs_to_folderitem
                                  (SummaryView *summaryview, FolderItem *item);
gint summaryview_export_mbox_list (SummaryView *summaryview);
void summaryview_lock(SummaryView *summaryview, FolderItem *item);
void summaryview_unlock(SummaryView *summaryview, FolderItem *item);
void summary_reflect_prefs(void);
void summaryview_activate_quicksearch(SummaryView *summaryview, gboolean show);
void summary_set_menu_sensitive	(SummaryView		*summaryview);
void summary_relayout(SummaryView *summaryview);
void summary_update_unread(SummaryView *summaryview, FolderItem *removed_item);
gboolean summary_is_list(SummaryView *summaryview);
gboolean summaryview_search_root_progress(gpointer data, guint at, guint matched, guint total);
#endif /* __SUMMARY_H__ */
