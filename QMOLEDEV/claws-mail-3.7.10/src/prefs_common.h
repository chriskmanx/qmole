/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __PREFS_COMMON_H__
#define __PREFS_COMMON_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <glib.h>

#include "mainwindow.h"
#include "summaryview.h"
#include "folderview.h"
#include "codeconv.h"
#include "textview.h"
#include "procmime.h"
#include "prefs_msg_colors.h"
#include "prefs_summary_open.h"

typedef struct _PrefsCommon	PrefsCommon;

typedef enum {
	RECV_DIALOG_ALWAYS,
	RECV_DIALOG_MANUAL,
	RECV_DIALOG_NEVER
} RecvDialogMode;

typedef enum {
	COMPOSE_DND_ASK,
	COMPOSE_DND_INSERT,
	COMPOSE_DND_ATTACH
} ComposeDndInsertOrAttach;

typedef enum {
	CTE_AUTO,
	CTE_BASE64,
	CTE_QUOTED_PRINTABLE,
	CTE_8BIT
} TransferEncodingMethod;

typedef enum
{
/* U = unread, N = new, M = marked */
	SELECTONENTRY_NOTHING,
	SELECTONENTRY_UNM,
	SELECTONENTRY_UMN,
	SELECTONENTRY_NUM,
	SELECTONENTRY_NMU,
	SELECTONENTRY_MNU,
	SELECTONENTRY_MUN,
	SELECTONENTRY_LAST
} SelectOnEntry;

typedef enum
{
	ACTION_UNSET = 0, /* for backward compatibility */
	ACTION_MARKED,
	ACTION_NEW,
	ACTION_UNREAD,
	ACTION_LAST_OPENED,
	ACTION_LAST_LIST,
	ACTION_NOTHING,
	ACTION_FIRST_LIST
} EntryAction;

typedef enum
{
	NEXTUNREADMSGDIALOG_ALWAYS,
	NEXTUNREADMSGDIALOG_ASSUME_YES,
	NEXTUNREADMSGDIALOG_ASSUME_NO
} NextUnreadMsgDialogShow;

typedef enum
{
	OPENMSG_REQUEST_ONLY = 0,
	OPENMSG_ALWAYS = 1,
	OPENMSG_WHEN_VIEW_VISIBLE
} ShowMsgPolicy;

struct _PrefsCommon
{
#ifdef MAEMO
	gchar *data_root;
#endif
	/* Receive */
	gboolean use_extinc;
	gchar *extinc_cmd;
	gboolean scan_all_after_inc;
	gboolean autochk_newmail;
	gint autochk_itv;
	gboolean chk_on_startup;
	gboolean open_inbox_on_inc;
 	gboolean newmail_notify_auto;
 	gboolean newmail_notify_manu;
 	gchar   *newmail_notify_cmd;
#ifdef MAEMO
	gboolean maemo_show_led;
	gboolean maemo_play_sound;
	gboolean maemo_show_banner;
#endif
	RecvDialogMode recv_dialog_mode;
	gint receivewin_width;
	gint receivewin_height;
	gboolean close_recv_dialog;
	gboolean no_recv_err_panel;

	/* Send */
	gboolean savemsg;
	gboolean confirm_send_queued_messages;
	gboolean send_dialog_invisible;
	gint sendwin_width;
	gint sendwin_height;
	gchar *outgoing_charset;
	TransferEncodingMethod encoding_method;
	gboolean outgoing_fallback_to_ascii;

	gboolean allow_jisx0201_kana;

	/* Compose */
	gint undolevels;
	gint linewrap_len;
	gboolean linewrap_quote;
	gboolean linewrap_pastes;
	gboolean primary_paste_unselects;
	gboolean autowrap;
	gboolean auto_indent;
	gboolean auto_exteditor;
	gboolean reply_account_autosel;
	gboolean default_reply_list;
	gboolean forward_account_autosel;
	gboolean reedit_account_autosel;
	gboolean show_ruler;
	gboolean autosave;
	gint autosave_length;
	gboolean warn_large_insert;
	gint warn_large_insert_size;
	gboolean compose_no_markup;
	ComposeDndInsertOrAttach compose_dnd_mode;
	gboolean compose_with_format;
	gchar *compose_subject_format;
	gchar *compose_body_format;
	gboolean show_compose_margin;

	/* Quote */
	gboolean reply_with_quote;
	gchar *quotemark;
	gchar *quotefmt;
	gchar *fw_quotemark;
	gchar *fw_quotefmt;
	gboolean forward_as_attachment;
	gboolean redirect_keep_from;
	gboolean block_cursor;
	gchar *quote_chars;
	
	gboolean enable_aspell;
	gchar *dictionary;
	gchar *alt_dictionary;
	gulong misspelled_col;
	gboolean check_while_typing;
	gboolean recheck_when_changing_dict;
	gboolean use_alternate;
	gboolean use_both_dicts;
        
	/* Display */
	/* obsolete fonts */
	gchar *widgetfont_gtk1;
	gchar *textfont_gtk1;
	gchar *normalfont_gtk1;
	gchar *boldfont_gtk1;
	gchar *smallfont_gtk1;

	/* new fonts */
	gchar *widgetfont;
	gchar *textfont;
	gchar *printfont;
	gchar *boldfont;
	gchar *normalfont;
	gchar *smallfont;
	gchar *titlefont;
	gboolean use_different_print_font;
	gboolean derive_from_normal_font;

	/* custom colors */
	ColorlabelPrefs custom_colorlabel[COLORLABELS];

	/* image viewer */
	gboolean display_img;
	gboolean resize_img;
	gboolean inline_img;

	gboolean trans_hdr;
	gint display_folder_unread;
	gint ng_abbrev_len;

	gboolean show_searchbar;
	gboolean expand_thread;
	gboolean swap_from;
	gboolean use_addr_book;
	gchar *date_format;
	gboolean *msgview_date_format;

	gboolean use_stripes_everywhere;
	gboolean use_stripes_in_summaries; /* overrides if use_stripes_everywhere is set to TRUE */
	gint stripes_color_offset;
	gboolean enable_dotted_lines;
	gboolean enable_hscrollbar;
	gboolean bold_unread;
	gboolean enable_thread;
	gboolean thread_by_subject;
	gint thread_by_subject_max_age; /*!< Max. age of a thread which was threaded
					 *   by subject (days) */

	gchar *last_opened_folder;
	gboolean goto_last_folder_on_startup;

	ToolbarStyle toolbar_style;
	gboolean toolbar_detachable;
	gboolean show_statusbar;
	gboolean show_col_headers;

	gint folderview_vscrollbar_policy;

	/* Filtering */
	GSList *fltlist;

	gint kill_score;
	gint important_score;

	/* Actions */
	GSList *actions_list;

	/* Summary columns visibility, position and size */
	gboolean summary_col_visible[N_SUMMARY_COLS];
	gint summary_col_pos[N_SUMMARY_COLS];
	gint summary_col_size[N_SUMMARY_COLS];

	gboolean folder_col_visible[N_FOLDER_COLS];
	gint folder_col_pos[N_FOLDER_COLS];
	gint folder_col_size[N_FOLDER_COLS];

	/* Widget visibility, position and size */
	gint folderwin_x;
	gint folderwin_y;
	gint folderview_width;
	gint folderview_height;
	gboolean folderview_visible;

	gint summaryview_width;
	gint summaryview_height;

	gint main_msgwin_x;
	gint main_msgwin_y;
	gint msgview_width;
	gint msgview_height;
	gboolean msgview_visible;

	gint mainview_x;
	gint mainview_y;
	gint mainview_width;
	gint mainview_height;
	gint mainwin_x;
	gint mainwin_y;
	gint mainwin_width;
	gint mainwin_height;
	gint mainwin_maximised;
	gint mainwin_fullscreen;

	gint msgwin_width;
	gint msgwin_height;

	gint sourcewin_width;
	gint sourcewin_height;

	gint compose_width;
	gint compose_height;
	gint compose_x;
	gint compose_y;

	/* Message */
	gboolean enable_color;
	gboolean enable_bgcolor;
	gulong quote_level1_col;
	gulong quote_level2_col;
	gulong quote_level3_col;
	gulong quote_level1_bgcol;
	gulong quote_level2_bgcol;
	gulong quote_level3_bgcol;
	gulong uri_col;
	gulong tgt_folder_col;
	gulong signature_col;
	gulong emphasis_col;
	gboolean recycle_quote_colors;
	gboolean display_header_pane;
	gboolean display_header;
	gboolean display_xface;
	gint line_space;
	gboolean render_html;
	gboolean invoke_plugin_on_html;
	gboolean promote_html_part;
	gboolean textview_cursor_visible;
	gboolean enable_smooth_scroll;
	gint scroll_step;
	gboolean scroll_halfpage;
	gboolean hide_quoted;
	gboolean respect_flowed_format;

	gboolean show_other_header;
	GSList *disphdr_list;

	gboolean attach_desc;

	/* MIME viewer */
	gchar *mime_textviewer;
	gchar *mime_open_cmd;
	gchar *attach_save_dir;
	gchar *attach_load_dir;

	GList *mime_open_cmd_history;
	gboolean show_inline_attachments;

	/* Addressbook */
	gboolean addressbook_use_editaddress_dialog;
	gint addressbook_hpaned_pos;
	gint addressbook_vpaned_pos;
	GList *addressbook_custom_attributes;

	/* Interface */
	gboolean layout_mode;

	gint statusbar_update_step;
	gboolean emulate_emacs;
	ShowMsgPolicy always_show_msg;
	gboolean mark_as_read_on_new_window;
	gboolean mark_as_read_delay;
	gboolean immediate_exec;
	SelectOnEntry select_on_entry;
	gboolean show_tooltips;

	EntryAction summary_select_prio[SUMMARY_OPEN_ACTIONS-1];

	NextUnreadMsgDialogShow next_unread_msg_dialog;
	gboolean add_address_by_click;
	gchar *pixmap_theme_path;
	int hover_timeout; /* msecs mouse hover timeout */
	gboolean ask_mark_all_read;
	gboolean ask_apply_per_account_filtering_rules;
	gint apply_per_account_filtering_rules;

	/* Other */
#ifndef G_OS_WIN32
	gchar *uri_cmd;
#endif
	gchar *ext_editor_cmd;
	gboolean cmds_use_system_default;

    	gboolean cliplog;
    	guint loglength;
	gboolean enable_log_standard;
	gboolean enable_log_warning;
	gboolean enable_log_error;
	gboolean enable_log_status;

	gulong log_msg_color;
	gulong log_warn_color;
	gulong log_error_color;
	gulong log_in_color;
	gulong log_out_color;
	gulong log_status_ok_color;
	gulong log_status_nok_color;
	gulong log_status_skip_color;

	gboolean enable_filtering_debug;
	gint filtering_debug_level;
	gboolean enable_filtering_debug_inc;
	gboolean enable_filtering_debug_manual;
	gboolean enable_filtering_debug_folder_proc;
	gboolean enable_filtering_debug_pre_proc;
	gboolean enable_filtering_debug_post_proc;
	gboolean filtering_debug_cliplog;
	guint filtering_debug_loglength;

	gboolean confirm_on_exit;
	gboolean session_passwords;
	gboolean clean_on_exit;
	gboolean ask_on_clean;
	gboolean warn_queued_on_exit;

	gint io_timeout_secs;

	gboolean gtk_can_change_accels;
	
	/* Memory cache*/
	gint cache_max_mem_usage;
	gint cache_min_keep_time;
	
	/* boolean for work offline 
	   stored here for use in inc.c */
	gboolean work_offline;
	
	gint summary_quicksearch_type;
	gint summary_quicksearch_sticky;
	gint summary_quicksearch_recurse;
	gint summary_quicksearch_dynamic;
	gint summary_quicksearch_autorun;
	gulong color_new;
	
	GList *summary_quicksearch_history;
	GList *summary_search_from_history;
	GList *summary_search_to_history;
	GList *summary_search_subject_history;
	GList *summary_search_body_history;
	GList *summary_search_adv_condition_history;
	GList *message_search_history;
	GList *compose_save_to_history;

	gint filteringwin_width;
	gint filteringwin_height;
	gint filteringactionwin_width;
	gint filteringactionwin_height;
	gint matcherwin_width;
	gint matcherwin_height;
	gint templateswin_width;
	gint templateswin_height;
	gint actionswin_width;
	gint actionswin_height;
	gint tagswin_width;
	gint tagswin_height;
	gint addressbookwin_width;
	gint addressbookwin_height;
	gint addressbookeditpersonwin_width;
	gint addressbookeditpersonwin_height;
	gint addressbookeditgroupwin_width;
	gint addressbookeditgroupwin_height;
	gint pluginswin_width;
	gint pluginswin_height;
	gint prefswin_width;
	gint prefswin_height;
	gint folderitemwin_width;
	gint folderitemwin_height;
	gchar *zero_replacement;
	gint editaccountwin_width;
	gint editaccountwin_height;
	gint accountswin_width;
	gint accountswin_height;
	gint logwin_width;
	gint logwin_height;
	gint filtering_debugwin_width;
	gint filtering_debugwin_height;
	gint folderselwin_width;
	gint folderselwin_height;
	gint addressaddwin_width;
	gint addressaddwin_height;
	gint addressbook_folderselwin_width;
	gint addressbook_folderselwin_height;
	gint aboutwin_width;
	gint aboutwin_height;
	gint addrgather_width;
	gint addrgather_height;
	gint news_subscribe_width;
	gint news_subscribe_height;

	gint warn_dnd;
	gint broken_are_utf8;
	gint skip_ssl_cert_check;
	gint live_dangerously;
	gint save_parts_readwrite;
	gint never_send_retrcpt;
	gint hide_quotes;
	gboolean unsafe_ssl_certs;
	gboolean real_time_sync;
	
	gchar *print_paper_type;
	gint print_paper_orientation;
	gint print_margin_top;
	gint print_margin_bottom;
	gint print_margin_left;
	gint print_margin_right;
  
	gint print_use_color;
	gint print_use_collate;
	gint print_use_reverse;
	gint print_use_duplex;
	gint print_imgs;
	gint print_previewwin_width;
	gint print_previewwin_height;
	
	gboolean use_networkmanager;
	gboolean use_shred;
	gboolean two_line_vert;
	gboolean inherit_folder_props;
	gboolean flush_metadata;

};

extern PrefsCommon prefs_common;

PrefsCommon *prefs_common_get_prefs(void);

GList *prefs_common_read_history_from_dir_with_defaults(const gchar *dirname, const gchar *history,
															  GList *default_list);
void prefs_common_read_config	(void);
void prefs_common_write_config	(void);
void prefs_common_open		(void);
void pref_get_unescaped_pref(gchar *out, const gchar *in);
void pref_get_escaped_pref(gchar *out, const gchar *in);
void pref_set_textview_from_pref(GtkTextView *textview, const gchar *txt);
void pref_set_entry_from_pref(GtkEntry *entry, const gchar *txt);
gchar *pref_get_pref_from_textview(GtkTextView *textview);
gchar *pref_get_pref_from_entry(GtkEntry *entry);
const gchar *prefs_common_translated_header_name(const gchar *header_name);
const gchar *prefs_common_get_uri_cmd(void);
const gchar *prefs_common_get_ext_editor_cmd(void);
#endif /* __PREFS_COMMON_H__ */
