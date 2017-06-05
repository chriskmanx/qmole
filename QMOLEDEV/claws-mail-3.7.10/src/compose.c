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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "defs.h"

#ifndef PANGO_ENABLE_ENGINE
#  define PANGO_ENABLE_ENGINE
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include <pango/pango-break.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#if HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
#include <signal.h>
#include <errno.h>
#ifndef G_OS_WIN32  /* fixme we should have a configure test. */
#include <libgen.h>
#endif

#if (HAVE_WCTYPE_H && HAVE_WCHAR_H)
#  include <wchar.h>
#  include <wctype.h>
#endif

#include "claws.h"
#include "main.h"
#include "mainwindow.h"
#include "compose.h"
#include "addressbook.h"
#include "folderview.h"
#include "procmsg.h"
#include "menu.h"
#include "stock_pixmap.h"
#include "send_message.h"
#include "imap.h"
#include "news.h"
#include "customheader.h"
#include "prefs_common.h"
#include "prefs_account.h"
#include "action.h"
#include "account.h"
#include "filesel.h"
#include "procheader.h"
#include "procmime.h"
#include "statusbar.h"
#include "about.h"
#include "base64.h"
#include "quoted-printable.h"
#include "codeconv.h"
#include "utils.h"
#include "gtkutils.h"
#include "socket.h"
#include "alertpanel.h"
#include "manage_window.h"
#include "gtkshruler.h"
#include "folder.h"
#include "addr_compl.h"
#include "quote_fmt.h"
#include "undo.h"
#include "foldersel.h"
#include "toolbar.h"
#include "inc.h"
#include "message_search.h"
#include "combobox.h"
#include "hooks.h"
#include "privacy.h"
#include "timing.h"
#include "autofaces.h"
#include "spell_entry.h"

enum
{
	COL_MIMETYPE = 0,
	COL_SIZE     = 1,
	COL_NAME     = 2,
	COL_CHARSET  = 3,
	COL_DATA     = 4,
	COL_AUTODATA = 5,
	N_COL_COLUMNS
};

#define N_ATTACH_COLS	(N_COL_COLUMNS)

typedef enum
{
	COMPOSE_CALL_ADVANCED_ACTION_MOVE_BEGINNING_OF_LINE,
	COMPOSE_CALL_ADVANCED_ACTION_MOVE_FORWARD_CHARACTER,
	COMPOSE_CALL_ADVANCED_ACTION_MOVE_BACKWARD_CHARACTER,
	COMPOSE_CALL_ADVANCED_ACTION_MOVE_FORWARD_WORD,
	COMPOSE_CALL_ADVANCED_ACTION_MOVE_BACKWARD_WORD,
	COMPOSE_CALL_ADVANCED_ACTION_MOVE_END_OF_LINE,
	COMPOSE_CALL_ADVANCED_ACTION_MOVE_NEXT_LINE,
	COMPOSE_CALL_ADVANCED_ACTION_MOVE_PREVIOUS_LINE,
	COMPOSE_CALL_ADVANCED_ACTION_DELETE_FORWARD_CHARACTER,
	COMPOSE_CALL_ADVANCED_ACTION_DELETE_BACKWARD_CHARACTER,
	COMPOSE_CALL_ADVANCED_ACTION_DELETE_FORWARD_WORD,
	COMPOSE_CALL_ADVANCED_ACTION_DELETE_BACKWARD_WORD,
	COMPOSE_CALL_ADVANCED_ACTION_DELETE_LINE,
	COMPOSE_CALL_ADVANCED_ACTION_DELETE_TO_LINE_END
} ComposeCallAdvancedAction;

typedef enum
{
	PRIORITY_HIGHEST = 1,
	PRIORITY_HIGH,
	PRIORITY_NORMAL,
	PRIORITY_LOW,
	PRIORITY_LOWEST
} PriorityLevel;

typedef enum
{
	COMPOSE_INSERT_SUCCESS,
	COMPOSE_INSERT_READ_ERROR,
	COMPOSE_INSERT_INVALID_CHARACTER,
	COMPOSE_INSERT_NO_FILE
} ComposeInsertResult;

typedef enum
{
	COMPOSE_WRITE_FOR_SEND,
	COMPOSE_WRITE_FOR_STORE
} ComposeWriteType;

typedef enum
{
	COMPOSE_QUOTE_FORCED,
	COMPOSE_QUOTE_CHECK,
	COMPOSE_QUOTE_SKIP
} ComposeQuoteMode;

typedef enum {
    TO_FIELD_PRESENT,
    SUBJECT_FIELD_PRESENT,
    BODY_FIELD_PRESENT,
    NO_FIELD_PRESENT
} MailField;

#define B64_LINE_SIZE		57
#define B64_BUFFSIZE		77

#define MAX_REFERENCES_LEN	999

static GList *compose_list = NULL;

static Compose *compose_generic_new			(PrefsAccount	*account,
						 const gchar	*to,
						 FolderItem	*item,
						 GPtrArray 	*attach_files,
						 GList          *listAddress );

static Compose *compose_create			(PrefsAccount	*account,
						 FolderItem		 *item,
						 ComposeMode	 mode,
						 gboolean batch);

static void compose_entry_mark_default_to	(Compose	  *compose,
					 const gchar	  *address);
static Compose *compose_followup_and_reply_to	(MsgInfo	*msginfo,
					 ComposeQuoteMode	 quote_mode,
					 gboolean	 to_all,
					 gboolean	 to_sender,
					 const gchar	*body);
static Compose *compose_forward_multiple	(PrefsAccount	*account, 
					 GSList		*msginfo_list);
static Compose *compose_reply			(MsgInfo	*msginfo,
					 ComposeQuoteMode	 quote_mode,
					 gboolean	 to_all,
					 gboolean	 to_ml,
					 gboolean	 to_sender,
					 const gchar	*body);
static Compose *compose_reply_mode		(ComposeMode 	 mode, 
					 GSList 	*msginfo_list, 
					 gchar 		*body);
static void compose_template_apply_fields(Compose *compose, Template *tmpl);
static void compose_update_privacy_systems_menu(Compose	*compose);

static GtkWidget *compose_account_option_menu_create
						(Compose	*compose);
static void compose_set_out_encoding		(Compose	*compose);
static void compose_set_template_menu		(Compose	*compose);
static void compose_destroy			(Compose	*compose);

static MailField compose_entries_set		(Compose	*compose,
						 const gchar	*mailto,
						 ComposeEntryType to_type);
static gint compose_parse_header		(Compose	*compose,
						 MsgInfo	*msginfo);
static gchar *compose_parse_references		(const gchar	*ref,
						 const gchar	*msgid);

static gchar *compose_quote_fmt			(Compose	*compose,
						 MsgInfo	*msginfo,
						 const gchar	*fmt,
						 const gchar	*qmark,
						 const gchar	*body,
						 gboolean	 rewrap,
						 gboolean	 need_unescape,
						 const gchar *err_msg);

static void compose_reply_set_entry		(Compose	*compose,
						 MsgInfo	*msginfo,
						 gboolean	 to_all,
						 gboolean	 to_ml,
						 gboolean	 to_sender,
						 gboolean
						 followup_and_reply_to);
static void compose_reedit_set_entry		(Compose	*compose,
						 MsgInfo	*msginfo);

static void compose_insert_sig			(Compose	*compose,
						 gboolean	 replace);
static ComposeInsertResult compose_insert_file	(Compose	*compose,
						 const gchar	*file);

static gboolean compose_attach_append		(Compose	*compose,
						 const gchar	*file,
						 const gchar	*type,
						 const gchar	*content_type,
						 const gchar	*charset);
static void compose_attach_parts		(Compose	*compose,
						 MsgInfo	*msginfo);

static gboolean compose_beautify_paragraph	(Compose	*compose,
						 GtkTextIter	*par_iter,
						 gboolean	 force);
static void compose_wrap_all			(Compose	*compose);
static void compose_wrap_all_full		(Compose	*compose,
						 gboolean	 autowrap);

static void compose_set_title			(Compose	*compose);
static void compose_select_account		(Compose	*compose,
						 PrefsAccount	*account,
						 gboolean	 init);

static PrefsAccount *compose_current_mail_account(void);
/* static gint compose_send			(Compose	*compose); */
static gboolean compose_check_for_valid_recipient
						(Compose	*compose);
static gboolean compose_check_entries		(Compose	*compose,
						 gboolean 	check_everything);
static gint compose_write_to_file		(Compose	*compose,
						 FILE		*fp,
						 gint 		 action,
						 gboolean	 attach_parts);
static gint compose_write_body_to_file		(Compose	*compose,
						 const gchar	*file);
static gint compose_remove_reedit_target	(Compose	*compose,
						 gboolean	 force);
static void compose_remove_draft			(Compose	*compose);
static gint compose_queue_sub			(Compose	*compose,
						 gint		*msgnum,
						 FolderItem	**item,
						 gchar		**msgpath,
						 gboolean	check_subject,
						 gboolean 	remove_reedit_target);
static int compose_add_attachments		(Compose	*compose,
						 MimeInfo	*parent);
static gchar *compose_get_header		(Compose	*compose);

static void compose_convert_header		(Compose	*compose,
						 gchar		*dest,
						 gint		 len,
						 gchar		*src,
						 gint		 header_len,
						 gboolean	 addr_field);

static void compose_attach_info_free		(AttachInfo	*ainfo);
static void compose_attach_remove_selected	(GtkAction	*action,
						 gpointer	 data);

static void compose_template_apply		(Compose	*compose,
						 Template	*tmpl,
						 gboolean	 replace);
static void compose_attach_property		(GtkAction	*action,
						 gpointer	 data);
static void compose_attach_property_create	(gboolean	*cancelled);
static void attach_property_ok			(GtkWidget	*widget,
						 gboolean	*cancelled);
static void attach_property_cancel		(GtkWidget	*widget,
						 gboolean	*cancelled);
static gint attach_property_delete_event	(GtkWidget	*widget,
						 GdkEventAny	*event,
						 gboolean	*cancelled);
static gboolean attach_property_key_pressed	(GtkWidget	*widget,
						 GdkEventKey	*event,
						 gboolean	*cancelled);

static void compose_exec_ext_editor		(Compose	*compose);
#ifdef G_OS_UNIX
static gint compose_exec_ext_editor_real	(const gchar	*file);
static gboolean compose_ext_editor_kill		(Compose	*compose);
static gboolean compose_input_cb		(GIOChannel	*source,
						 GIOCondition	 condition,
						 gpointer	 data);
static void compose_set_ext_editor_sensitive	(Compose	*compose,
						 gboolean	 sensitive);
#endif /* G_OS_UNIX */

static void compose_undo_state_changed		(UndoMain	*undostruct,
						 gint		 undo_state,
						 gint		 redo_state,
						 gpointer	 data);

static void compose_create_header_entry	(Compose *compose);
static void compose_add_header_entry	(Compose *compose, const gchar *header,
					 gchar *text, ComposePrefType pref_type);
static void compose_remove_header_entries(Compose *compose);

static void compose_update_priority_menu_item(Compose * compose);
#if USE_ENCHANT
static void compose_spell_menu_changed	(void *data);
static void compose_dict_changed	(void *data);
#endif
static void compose_add_field_list	( Compose *compose,
					  GList *listAddress );

/* callback functions */

static gboolean compose_edit_size_alloc (GtkEditable	*widget,
					 GtkAllocation	*allocation,
					 GtkSHRuler	*shruler);
static void account_activated		(GtkComboBox *optmenu,
					 gpointer	 data);
static void attach_selected		(GtkTreeView	*tree_view, 
					 GtkTreePath	*tree_path,
					 GtkTreeViewColumn *column, 
					 Compose *compose);
static gboolean attach_button_pressed	(GtkWidget	*widget,
					 GdkEventButton	*event,
					 gpointer	 data);
static gboolean attach_key_pressed	(GtkWidget	*widget,
					 GdkEventKey	*event,
					 gpointer	 data);
static void compose_send_cb		(GtkAction	*action, gpointer data);
static void compose_send_later_cb	(GtkAction	*action, gpointer data);

static void compose_save_cb		(GtkAction	*action,
					 gpointer	 data);

static void compose_attach_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_insert_file_cb	(GtkAction	*action,
					 gpointer	 data);
static void compose_insert_sig_cb	(GtkAction	*action,
					 gpointer	 data);

static void compose_close_cb		(GtkAction	*action,
					 gpointer	 data);

static void compose_set_encoding_cb	(GtkAction	*action, GtkRadioAction *current, gpointer data);

static void compose_address_cb		(GtkAction	*action,
					 gpointer	 data);
static void about_show_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_template_activate_cb(GtkWidget	*widget,
					 gpointer	 data);

static void compose_ext_editor_cb	(GtkAction	*action,
					 gpointer	 data);

static gint compose_delete_cb		(GtkWidget	*widget,
					 GdkEventAny	*event,
					 gpointer	 data);

static void compose_undo_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_redo_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_cut_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_copy_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_paste_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_paste_as_quote_cb	(GtkAction	*action,
					 gpointer	 data);
static void compose_paste_no_wrap_cb	(GtkAction	*action,
					 gpointer	 data);
static void compose_paste_wrap_cb	(GtkAction	*action,
					 gpointer	 data);
static void compose_allsel_cb		(GtkAction	*action,
					 gpointer	 data);

static void compose_advanced_action_cb	(GtkAction	*action,
					 gpointer	 data);

static void compose_grab_focus_cb	(GtkWidget	*widget,
					 Compose	*compose);

static void compose_changed_cb		(GtkTextBuffer	*textbuf,
					 Compose	*compose);

static void compose_wrap_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_wrap_all_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_find_cb		(GtkAction	*action,
					 gpointer	 data);
static void compose_toggle_autowrap_cb	(GtkToggleAction *action,
					 gpointer	 data);
static void compose_toggle_autoindent_cb(GtkToggleAction *action,
					 gpointer	 data);

static void compose_toggle_ruler_cb	(GtkToggleAction *action,
					 gpointer	 data);
static void compose_toggle_sign_cb	(GtkToggleAction *action,
					 gpointer	 data);
static void compose_toggle_encrypt_cb	(GtkToggleAction *action,
					 gpointer	 data);
static void compose_set_privacy_system_cb(GtkWidget *widget, gpointer data);
static void compose_update_privacy_system_menu_item(Compose * compose, gboolean warn);
static void activate_privacy_system     (Compose *compose, 
                                         PrefsAccount *account,
					 gboolean warn);
static void compose_use_signing(Compose *compose, gboolean use_signing);
static void compose_use_encryption(Compose *compose, gboolean use_encryption);
static void compose_toggle_return_receipt_cb(GtkToggleAction *action,
					 gpointer	 data);
static void compose_toggle_remove_refs_cb(GtkToggleAction *action,
					 gpointer	 data);
static void compose_set_priority_cb	(GtkAction *action, GtkRadioAction *current, gpointer data);
static void compose_reply_change_mode	(Compose *compose, ComposeMode action);
static void compose_reply_change_mode_cb(GtkAction *action, GtkRadioAction *current, gpointer data);

static void compose_attach_drag_received_cb (GtkWidget		*widget,
					     GdkDragContext	*drag_context,
					     gint		 x,
					     gint		 y,
					     GtkSelectionData	*data,
					     guint		 info,
					     guint		 time,
					     gpointer		 user_data);
static void compose_insert_drag_received_cb (GtkWidget		*widget,
					     GdkDragContext	*drag_context,
					     gint		 x,
					     gint		 y,
					     GtkSelectionData	*data,
					     guint		 info,
					     guint		 time,
					     gpointer		 user_data);
static void compose_header_drag_received_cb (GtkWidget		*widget,
					     GdkDragContext	*drag_context,
					     gint		 x,
					     gint		 y,
					     GtkSelectionData	*data,
					     guint		 info,
					     guint		 time,
					     gpointer		 user_data);

static gboolean compose_drag_drop	    (GtkWidget *widget,
					     GdkDragContext *drag_context,
					     gint x, gint y,
					     guint time, gpointer user_data);

static void text_inserted		(GtkTextBuffer	*buffer,
					 GtkTextIter	*iter,
					 const gchar	*text,
					 gint		 len,
					 Compose	*compose);
static Compose *compose_generic_reply(MsgInfo *msginfo,
				  ComposeQuoteMode quote_mode,
				  gboolean to_all,
				  gboolean to_ml,
				  gboolean to_sender,
				  gboolean followup_and_reply_to,
				  const gchar *body);

static void compose_headerentry_changed_cb	   (GtkWidget	       *entry,
					    ComposeHeaderEntry *headerentry);
static gboolean compose_headerentry_key_press_event_cb(GtkWidget	       *entry,
					    GdkEventKey        *event,
					    ComposeHeaderEntry *headerentry);
static gboolean compose_headerentry_button_clicked_cb (GtkWidget *button,
					ComposeHeaderEntry *headerentry);

static void compose_show_first_last_header (Compose *compose, gboolean show_first);

static void compose_allow_user_actions (Compose *compose, gboolean allow);

static void compose_nothing_cb		   (GtkAction *action, gpointer data)
{

}

#if USE_ENCHANT
static void compose_check_all		   (GtkAction *action, gpointer data);
static void compose_highlight_all	   (GtkAction *action, gpointer data);
static void compose_check_backwards	   (GtkAction *action, gpointer data);
static void compose_check_forwards_go	   (GtkAction *action, gpointer data);
#endif

static PrefsAccount *compose_guess_forward_account_from_msginfo	(MsgInfo *msginfo);

static MsgInfo *compose_msginfo_new_from_compose(Compose *compose);

#ifdef USE_ENCHANT
static void compose_set_dictionaries_from_folder_prefs(Compose *compose,
						FolderItem *folder_item);
#endif
static void compose_attach_update_label(Compose *compose);
static void compose_set_folder_prefs(Compose *compose, FolderItem *folder,
				     gboolean respect_default_to);

static GtkActionEntry compose_popup_entries[] =
{
	{"Compose",			NULL, "Compose" },
	{"Compose/Add",			NULL, N_("_Add..."), NULL, NULL, G_CALLBACK(compose_attach_cb) },
	{"Compose/Remove",			NULL, N_("_Remove"), NULL, NULL, G_CALLBACK(compose_attach_remove_selected) },
	{"Compose/---",			NULL, "---", NULL, NULL, NULL },
	{"Compose/Properties",		NULL, N_("_Properties..."), NULL, NULL, G_CALLBACK(compose_attach_property) },
};

static GtkActionEntry compose_entries[] =
{
	{"Menu",				NULL, "Menu" },
/* menus */
	{"Message",			NULL, N_("_Message") },
	{"Edit",			NULL, N_("_Edit") },
#if USE_ENCHANT
	{"Spelling",			NULL, N_("_Spelling") },
#endif
	{"Options",			NULL, N_("_Options") },
	{"Tools",			NULL, N_("_Tools") },
	{"Help",			NULL, N_("_Help") },
/* Message menu */
	{"Message/Send",		NULL, N_("S_end"), "<control>Return", NULL, G_CALLBACK(compose_send_cb) },
	{"Message/SendLater",		NULL, N_("Send _later"), "<shift><control>S", NULL, G_CALLBACK(compose_send_later_cb) },
	{"Message/---",			NULL, "---" },

	{"Message/AttachFile",		NULL, N_("_Attach file"), "<control>M", NULL, G_CALLBACK(compose_attach_cb) },
	{"Message/InsertFile",		NULL, N_("_Insert file"), "<control>I", NULL, G_CALLBACK(compose_insert_file_cb) },
	{"Message/InsertSig",		NULL, N_("Insert si_gnature"), "<control>G", NULL, G_CALLBACK(compose_insert_sig_cb) },
	/* {"Message/---",		NULL, "---" }, */
	{"Message/Save",		NULL, N_("_Save"), "<control>S", NULL, G_CALLBACK(compose_save_cb) }, /*COMPOSE_KEEP_EDITING*/
	/* {"Message/---",		NULL, "---" }, */
	{"Message/Close",		NULL, N_("_Close"), "<control>W", NULL, G_CALLBACK(compose_close_cb) },

/* Edit menu */
	{"Edit/Undo",			NULL, N_("_Undo"), "<control>Z", NULL, G_CALLBACK(compose_undo_cb) },
	{"Edit/Redo",			NULL, N_("_Redo"), "<control>Y", NULL, G_CALLBACK(compose_redo_cb) },
	{"Edit/---",			NULL, "---" },

	{"Edit/Cut",			NULL, N_("Cu_t"), "<control>X", NULL, G_CALLBACK(compose_cut_cb) },
	{"Edit/Copy",			NULL, N_("_Copy"), "<control>C", NULL, G_CALLBACK(compose_copy_cb) },
	{"Edit/Paste",			NULL, N_("_Paste"), "<control>V", NULL, G_CALLBACK(compose_paste_cb) },

	{"Edit/SpecialPaste",		NULL, N_("Special paste") },
	{"Edit/SpecialPaste/AsQuotation",	NULL, N_("as _quotation"), NULL, NULL, G_CALLBACK(compose_paste_as_quote_cb) },
	{"Edit/SpecialPaste/Wrapped",	NULL, N_("_wrapped"), NULL, NULL, G_CALLBACK(compose_paste_wrap_cb) },
	{"Edit/SpecialPaste/Unwrapped",	NULL, N_("_unwrapped"), NULL, NULL, G_CALLBACK(compose_paste_no_wrap_cb) },

	{"Edit/SelectAll",		NULL, N_("Select _all"), "<control>A", NULL, G_CALLBACK(compose_allsel_cb) },

	{"Edit/Advanced",		NULL, N_("A_dvanced") },
	{"Edit/Advanced/BackChar",	NULL, N_("Move a character backward"), "<shift><control>B", NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_MOVE_BACKWARD_CHARACTER*/
	{"Edit/Advanced/ForwChar",	NULL, N_("Move a character forward"), "<shift><control>F", NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_MOVE_FORWARD_CHARACTER*/
	{"Edit/Advanced/BackWord",	NULL, N_("Move a word backward"), NULL, NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_MOVE_BACKWARD_WORD*/
	{"Edit/Advanced/ForwWord",	NULL, N_("Move a word forward"), NULL, NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_MOVE_FORWARD_WORD*/
	{"Edit/Advanced/BegLine",	NULL, N_("Move to beginning of line"), NULL, NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_MOVE_BEGINNING_OF_LINE*/
	{"Edit/Advanced/EndLine",	NULL, N_("Move to end of line"), NULL, NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_MOVE_END_OF_LINE*/
	{"Edit/Advanced/PrevLine",	NULL, N_("Move to previous line"), "<control>P", NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_MOVE_PREVIOUS_LINE*/
	{"Edit/Advanced/NextLine",	NULL, N_("Move to next line"), "<control>N", NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_MOVE_NEXT_LINE*/
	{"Edit/Advanced/DelBackChar",	NULL, N_("Delete a character backward"), "<control>H", NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_DELETE_BACKWARD_CHARACTER*/
	{"Edit/Advanced/DelForwChar",	NULL, N_("Delete a character forward"), "<control>D", NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_DELETE_FORWARD_CHARACTER*/
	{"Edit/Advanced/DelBackWord",	NULL, N_("Delete a word backward"), NULL, NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_DELETE_BACKWARD_WORD*/
	{"Edit/Advanced/DelForwWord",	NULL, N_("Delete a word forward"), NULL, NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_DELETE_FORWARD_WORD*/
	{"Edit/Advanced/DelLine",	NULL, N_("Delete line"), "<control>U", NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_DELETE_LINE*/
	{"Edit/Advanced/DelEndLine",	NULL, N_("Delete to end of line"), "<control>K", NULL, G_CALLBACK(compose_advanced_action_cb) }, /*COMPOSE_CALL_ADVANCED_ACTION_DELETE_TO_LINE_END*/

	/* {"Edit/---",			NULL, "---" }, */
	{"Edit/Find",		NULL, N_("_Find"), "<control>F", NULL, G_CALLBACK(compose_find_cb) },

	/* {"Edit/---",			NULL, "---" }, */
	{"Edit/WrapPara",		NULL, N_("_Wrap current paragraph"), "<control>L", NULL, G_CALLBACK(compose_wrap_cb) }, /* 0 */
	{"Edit/WrapAllLines",		NULL, N_("Wrap all long _lines"), "<control><alt>L", NULL, G_CALLBACK(compose_wrap_all_cb) }, /* 1 */
	/* {"Edit/---",			NULL, "---" }, */
	{"Edit/ExtEditor",		NULL, N_("Edit with e_xternal editor"), "<shift><control>X", NULL, G_CALLBACK(compose_ext_editor_cb) },
#if USE_ENCHANT
/* Spelling menu */
	{"Spelling/CheckAllSel",	NULL, N_("_Check all or check selection"), NULL, NULL, G_CALLBACK(compose_check_all) },
	{"Spelling/HighlightAll",	NULL, N_("_Highlight all misspelled words"), NULL, NULL, G_CALLBACK(compose_highlight_all) },
	{"Spelling/CheckBackwards",	NULL, N_("Check _backwards misspelled word"), NULL, NULL, G_CALLBACK(compose_check_backwards) },
	{"Spelling/ForwardNext",	NULL, N_("_Forward to next misspelled word"), NULL, NULL, G_CALLBACK(compose_check_forwards_go) },

	{"Spelling/---",		NULL, "---" },
	{"Spelling/Options",		NULL, N_("_Options") },
#endif

/* Options menu */

	{"Options/ReplyMode",		NULL, N_("Reply _mode") },
	{"Options/---",			NULL, "---" },
	{"Options/PrivacySystem",	NULL, N_("Privacy _System") },
	{"Options/PrivacySystem/PlaceHolder",	NULL, "Placeholder", NULL, NULL, G_CALLBACK(compose_nothing_cb) },

	/* {"Options/---",		NULL, "---" }, */

	{"Options/Priority",		NULL, N_("_Priority") },

	{"Options/Encoding",		NULL, N_("Character _encoding") },
	{"Options/Encoding/---",	NULL, "---" },
#define ENC_ACTION(cs_char,c_char,string) \
	{ "Options/Encoding/" cs_char, NULL, N_(string), NULL, NULL, c_char }

	{"Options/Encoding/Western",	NULL, N_("Western European") },
	{"Options/Encoding/Baltic",	NULL, N_("Baltic") },
	{"Options/Encoding/Hebrew",	NULL, N_("Hebrew") },
	{"Options/Encoding/Arabic",	NULL, N_("Arabic") },
	{"Options/Encoding/Cyrillic",	NULL, N_("Cyrillic") },
	{"Options/Encoding/Japanese",	NULL, N_("Japanese") },
	{"Options/Encoding/Chinese",	NULL, N_("Chinese") },
	{"Options/Encoding/Korean",	NULL, N_("Korean") },
	{"Options/Encoding/Thai",	NULL, N_("Thai") },

/* Tools menu */
	{"Tools/AddressBook",		NULL, N_("_Address book"), NULL, NULL, G_CALLBACK(compose_address_cb) }, 

	{"Tools/Template",	NULL, N_("_Template") },
	{"Tools/Template/PlaceHolder",	NULL, "Placeholder", NULL, NULL, G_CALLBACK(compose_nothing_cb) },
	{"Tools/Actions",	NULL, N_("Actio_ns") },
	{"Tools/Actions/PlaceHolder",	NULL, "Placeholder", NULL, NULL, G_CALLBACK(compose_nothing_cb) },

/* Help menu */
	{"Help/About",		NULL, N_("_About"), NULL, NULL, G_CALLBACK(about_show_cb) }, 
};

static GtkToggleActionEntry compose_toggle_entries[] =
{
	{"Edit/AutoWrap",		NULL, N_("Aut_o wrapping"), "<shift><control>L", NULL, G_CALLBACK(compose_toggle_autowrap_cb) }, /* TOGGLE */
	{"Edit/AutoIndent",		NULL, N_("Auto _indent"), NULL, NULL, G_CALLBACK(compose_toggle_autoindent_cb) }, /* TOGGLE */
	{"Options/Sign",		NULL, N_("Si_gn"), NULL, NULL, G_CALLBACK(compose_toggle_sign_cb) }, /* Toggle */
	{"Options/Encrypt",		NULL, N_("_Encrypt"), NULL, NULL, G_CALLBACK(compose_toggle_encrypt_cb) }, /* Toggle */
	{"Options/RequestRetRcpt",	NULL, N_("_Request Return Receipt"), NULL, NULL, G_CALLBACK(compose_toggle_return_receipt_cb) }, /* TOGGLE */
	{"Options/RemoveReferences",	NULL, N_("Remo_ve references"), NULL, NULL, G_CALLBACK(compose_toggle_remove_refs_cb) }, /* TOGGLE */
	{"Tools/ShowRuler",		NULL, N_("Show _ruler"), NULL, NULL, G_CALLBACK(compose_toggle_ruler_cb) }, /* Toggle */
};

static GtkRadioActionEntry compose_radio_rm_entries[] =
{
	{"Options/ReplyMode/Normal",	NULL, N_("_Normal"), NULL, NULL, COMPOSE_REPLY }, /* RADIO compose_reply_change_mode_cb */
	{"Options/ReplyMode/All",	NULL, N_("_All"), NULL, NULL, COMPOSE_REPLY_TO_ALL }, /* RADIO compose_reply_change_mode_cb */
	{"Options/ReplyMode/Sender",	NULL, N_("_Sender"), NULL, NULL, COMPOSE_REPLY_TO_SENDER }, /* RADIO compose_reply_change_mode_cb */
	{"Options/ReplyMode/List",	NULL, N_("_Mailing-list"), NULL, NULL, COMPOSE_REPLY_TO_LIST }, /* RADIO compose_reply_change_mode_cb */
};

static GtkRadioActionEntry compose_radio_prio_entries[] =
{
	{"Options/Priority/Highest",	NULL, N_("_Highest"), NULL, NULL, PRIORITY_HIGHEST }, /* RADIO compose_set_priority_cb */
	{"Options/Priority/High",	NULL, N_("Hi_gh"), NULL, NULL, PRIORITY_HIGH }, /* RADIO compose_set_priority_cb */
	{"Options/Priority/Normal",	NULL, N_("_Normal"), NULL, NULL, PRIORITY_NORMAL }, /* RADIO compose_set_priority_cb */
	{"Options/Priority/Low",	NULL, N_("Lo_w"), NULL, NULL, PRIORITY_LOW }, /* RADIO compose_set_priority_cb */
	{"Options/Priority/Lowest",	NULL, N_("_Lowest"), NULL, NULL, PRIORITY_LOWEST }, /* RADIO compose_set_priority_cb */
};

static GtkRadioActionEntry compose_radio_enc_entries[] =
{
	ENC_ACTION(CS_AUTO, C_AUTO, N_("_Automatic")), /* RADIO compose_set_encoding_cb */
	ENC_ACTION(CS_US_ASCII, C_US_ASCII, N_("7bit ASCII (US-ASC_II)")), /* RADIO compose_set_encoding_cb */
	ENC_ACTION(CS_UTF_8, C_UTF_8, N_("Unicode (_UTF-8)")), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Western/"CS_ISO_8859_1, C_ISO_8859_1, "ISO-8859-_1"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Western/"CS_ISO_8859_15, C_ISO_8859_15, "ISO-8859-15"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Western/"CS_WINDOWS_1252, C_WINDOWS_1252, "Windows-1252"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION(CS_ISO_8859_2, C_ISO_8859_2, N_("Central European (ISO-8859-_2)")), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Baltic/"CS_ISO_8859_13, C_ISO_8859_13, "ISO-8859-13"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Baltic/"CS_ISO_8859_4, C_ISO_8859_14, "ISO-8859-_4"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION(CS_ISO_8859_7, C_ISO_8859_7, N_("Greek (ISO-8859-_7)")), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Hebrew/"CS_ISO_8859_8, C_ISO_8859_8, "ISO-8859-_8"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Hebrew/"CS_WINDOWS_1255, C_WINDOWS_1255, "Windows-1255"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Arabic/"CS_ISO_8859_6, C_ISO_8859_6, "ISO-8859-_6"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Arabic/"CS_WINDOWS_1256, C_WINDOWS_1256, "Windows-1256"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION(CS_ISO_8859_9, C_ISO_8859_9, N_("Turkish (ISO-8859-_9)")), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Cyrillic/"CS_ISO_8859_5, C_ISO_8859_5, "ISO-8859-_5"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Cyrillic/"CS_KOI8_R, C_KOI8_R, "KOI8-_R"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Cyrillic/"CS_KOI8_U, C_KOI8_U, "KOI8-_U"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Cyrillic/"CS_WINDOWS_1251, C_WINDOWS_1251, "Windows-1251"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Japanese/"CS_ISO_2022_JP, C_ISO_2022_JP, "ISO-2022-_JP"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Japanese/"CS_ISO_2022_JP_2, C_ISO_2022_JP_2, "ISO-2022-JP-_2"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Japanese/"CS_EUC_JP, C_EUC_JP, "_EUC-JP"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Japanese/"CS_SHIFT_JIS, C_SHIFT_JIS, "_Shift-JIS"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Chinese/"CS_GB18030, C_GB18030, "_GB18030"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Chinese/"CS_GB2312, C_GB2312, "_GB2312"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Chinese/"CS_GBK, C_GBK, "GB_K"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Chinese/"CS_BIG5, C_BIG5, "_Big5-JP"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Chinese/"CS_EUC_TW, C_EUC_TW, "EUC-_TW"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Korean/"CS_EUC_KR, C_EUC_KR, "_EUC-KR"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Korean/"CS_ISO_2022_KR, C_ISO_2022_KR, "_ISO-2022-KR"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Thai/"CS_TIS_620, C_TIS_620, "_TIS-620-KR"), /* RADIO compose_set_encoding_cb */
	ENC_ACTION("Thai/"CS_WINDOWS_874, C_WINDOWS_874, "_Windows-874"), /* RADIO compose_set_encoding_cb */
};

static GtkTargetEntry compose_mime_types[] =
{
	{"text/uri-list", 0, 0},
	{"UTF8_STRING", 0, 0},
	{"text/plain", 0, 0}
};

static gboolean compose_put_existing_to_front(MsgInfo *info)
{
	GList *compose_list = compose_get_compose_list();
	GList *elem = NULL;
	
	if (compose_list) {
		for (elem = compose_list; elem != NULL && elem->data != NULL; 
		     elem = elem->next) {
			Compose *c = (Compose*)elem->data;

			if (!c->targetinfo || !c->targetinfo->msgid ||
			    !info->msgid)
			    	continue;

			if (!strcmp(c->targetinfo->msgid, info->msgid)) {
				gtkut_window_popup(c->window);
				return TRUE;
			}
		}
	}
	return FALSE;
}

static GdkColor quote_color1 = 
	{(gulong)0, (gushort)0, (gushort)0, (gushort)0};
static GdkColor quote_color2 = 
	{(gulong)0, (gushort)0, (gushort)0, (gushort)0};
static GdkColor quote_color3 = 
	{(gulong)0, (gushort)0, (gushort)0, (gushort)0};

static GdkColor quote_bgcolor1 = 
	{(gulong)0, (gushort)0, (gushort)0, (gushort)0};
static GdkColor quote_bgcolor2 = 
	{(gulong)0, (gushort)0, (gushort)0, (gushort)0};
static GdkColor quote_bgcolor3 = 
	{(gulong)0, (gushort)0, (gushort)0, (gushort)0};

static GdkColor signature_color = {
	(gulong)0,
	(gushort)0x7fff,
	(gushort)0x7fff,
	(gushort)0x7fff
};

static GdkColor uri_color = {
	(gulong)0,
	(gushort)0,
	(gushort)0,
	(gushort)0
};

static void compose_create_tags(GtkTextView *text, Compose *compose)
{
	GtkTextBuffer *buffer;
	GdkColor black = {(gulong)0, (gushort)0, (gushort)0, (gushort)0};
	GdkColormap *cmap;
	GdkColor color[8];
	gboolean success[8];
	int i;

	buffer = gtk_text_view_get_buffer(text);

	if (prefs_common.enable_color) {
		/* grab the quote colors, converting from an int to a GdkColor */
		gtkut_convert_int_to_gdk_color(prefs_common.quote_level1_col,
					       &quote_color1);
		gtkut_convert_int_to_gdk_color(prefs_common.quote_level2_col,
					       &quote_color2);
		gtkut_convert_int_to_gdk_color(prefs_common.quote_level3_col,
					       &quote_color3);
		gtkut_convert_int_to_gdk_color(prefs_common.quote_level1_bgcol,
					       &quote_bgcolor1);
		gtkut_convert_int_to_gdk_color(prefs_common.quote_level2_bgcol,
					       &quote_bgcolor2);
		gtkut_convert_int_to_gdk_color(prefs_common.quote_level3_bgcol,
					       &quote_bgcolor3);
		gtkut_convert_int_to_gdk_color(prefs_common.signature_col,
					       &signature_color);
		gtkut_convert_int_to_gdk_color(prefs_common.uri_col,
					       &uri_color);
	} else {
		signature_color = quote_color1 = quote_color2 = quote_color3 = 
			quote_bgcolor1 = quote_bgcolor2 = quote_bgcolor3 = uri_color = black;
	}

	if (prefs_common.enable_color && prefs_common.enable_bgcolor) {
		compose->quote0_tag = gtk_text_buffer_create_tag(buffer, "quote0",
					   "foreground-gdk", &quote_color1,
					   "paragraph-background-gdk", &quote_bgcolor1,
					   NULL);
		compose->quote1_tag = gtk_text_buffer_create_tag(buffer, "quote1",
					   "foreground-gdk", &quote_color2,
					   "paragraph-background-gdk", &quote_bgcolor2,
					   NULL);
		compose->quote2_tag = gtk_text_buffer_create_tag(buffer, "quote2",
					   "foreground-gdk", &quote_color3,
					   "paragraph-background-gdk", &quote_bgcolor3,
					   NULL);
	} else {
		compose->quote0_tag = gtk_text_buffer_create_tag(buffer, "quote0",
					   "foreground-gdk", &quote_color1,
					   NULL);
		compose->quote1_tag = gtk_text_buffer_create_tag(buffer, "quote1",
					   "foreground-gdk", &quote_color2,
					   NULL);
		compose->quote2_tag = gtk_text_buffer_create_tag(buffer, "quote2",
					   "foreground-gdk", &quote_color3,
					   NULL);
	}
	
 	compose->signature_tag = gtk_text_buffer_create_tag(buffer, "signature",
				   "foreground-gdk", &signature_color,
				   NULL);
 	
	compose->uri_tag = gtk_text_buffer_create_tag(buffer, "link",
					"foreground-gdk", &uri_color,
					 NULL);
	compose->no_wrap_tag = gtk_text_buffer_create_tag(buffer, "no_wrap", NULL);
	compose->no_join_tag = gtk_text_buffer_create_tag(buffer, "no_join", NULL);

	color[0] = quote_color1;
	color[1] = quote_color2;
	color[2] = quote_color3;
	color[3] = quote_bgcolor1;
	color[4] = quote_bgcolor2;
	color[5] = quote_bgcolor3;
	color[6] = signature_color;
	color[7] = uri_color;
	cmap = gdk_drawable_get_colormap(compose->window->window);
	gdk_colormap_alloc_colors(cmap, color, 8, FALSE, TRUE, success);

	for (i = 0; i < 8; i++) {
		if (success[i] == FALSE) {
			GtkStyle *style;

			g_warning("Compose: color allocation failed.\n");
			style = gtk_widget_get_style(GTK_WIDGET(text));
			quote_color1 = quote_color2 = quote_color3 = 
				quote_bgcolor1 = quote_bgcolor2 = quote_bgcolor3 = 
				signature_color = uri_color = black;
		}
	}
}

Compose *compose_new(PrefsAccount *account, const gchar *mailto,
		     GPtrArray *attach_files)
{
	return compose_generic_new(account, mailto, NULL, attach_files, NULL);
}

Compose *compose_new_with_folderitem(PrefsAccount *account, FolderItem *item, const gchar *mailto)
{
	return compose_generic_new(account, mailto, item, NULL, NULL);
}

Compose *compose_new_with_list( PrefsAccount *account, GList *listAddress )
{
	return compose_generic_new( account, NULL, NULL, NULL, listAddress );
}

#define SCROLL_TO_CURSOR(compose) {				\
	GtkTextMark *cmark = gtk_text_buffer_get_insert(	\
		gtk_text_view_get_buffer(			\
			GTK_TEXT_VIEW(compose->text)));		\
	gtk_text_view_scroll_mark_onscreen(			\
		GTK_TEXT_VIEW(compose->text),			\
		cmark);						\
}

static void compose_set_save_to(Compose *compose, const gchar *folderidentifier)
{
	GtkEditable *entry;
	if (folderidentifier) {
		combobox_unset_popdown_strings(GTK_COMBO_BOX(compose->savemsg_combo));
		prefs_common.compose_save_to_history = add_history(
				prefs_common.compose_save_to_history, folderidentifier);
		combobox_set_popdown_strings(GTK_COMBO_BOX(compose->savemsg_combo),
				prefs_common.compose_save_to_history);
	}

	entry = GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(compose->savemsg_combo)));
	if (folderidentifier)
		gtk_entry_set_text(GTK_ENTRY(entry), folderidentifier);
	else
		gtk_entry_set_text(GTK_ENTRY(entry), "");
}

static gchar *compose_get_save_to(Compose *compose)
{
	GtkEditable *entry;
	gchar *result = NULL;
	entry = GTK_EDITABLE(gtk_bin_get_child(GTK_BIN(compose->savemsg_combo)));
	result = gtk_editable_get_chars(entry, 0, -1);
	
	if (result) {
		combobox_unset_popdown_strings(GTK_COMBO_BOX(compose->savemsg_combo));
		prefs_common.compose_save_to_history = add_history(
				prefs_common.compose_save_to_history, result);
		combobox_set_popdown_strings(GTK_COMBO_BOX(compose->savemsg_combo),
				prefs_common.compose_save_to_history);
	}
	return result;
}

Compose *compose_generic_new(PrefsAccount *account, const gchar *mailto, FolderItem *item,
			     GPtrArray *attach_files, GList *listAddress )
{
	Compose *compose;
	GtkTextView *textview;
	GtkTextBuffer *textbuf;
	GtkTextIter iter;
	const gchar *subject_format = NULL;
	const gchar *body_format = NULL;
	gchar *mailto_from = NULL;
	PrefsAccount *mailto_account = NULL;
	MsgInfo* dummyinfo = NULL;
	gint cursor_pos = -1;
	MailField mfield = NO_FIELD_PRESENT;
	gchar* buf;
	GtkTextMark *mark;

	/* check if mailto defines a from */
	if (mailto && *mailto != '\0') {
		scan_mailto_url(mailto, &mailto_from, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
		/* mailto defines a from, check if we can get account prefs from it,
		   if not, the account prefs will be guessed using other ways, but we'll keep
		   the from anyway */
		if (mailto_from)
			mailto_account = account_find_from_address(mailto_from, TRUE);
		if (mailto_account)
			account = mailto_account;
	}

	/* if no account prefs set from mailto, set if from folder prefs (if any) */
	if (!mailto_account && item && item->prefs && item->prefs->enable_default_account)
		account = account_find_from_id(item->prefs->default_account);

	/* if no account prefs set, fallback to the current one */
 	if (!account) account = cur_account;
	cm_return_val_if_fail(account != NULL, NULL);

	compose = compose_create(account, item, COMPOSE_NEW, FALSE);

	/* override from name if mailto asked for it */
	if (mailto_from) {
		gtk_entry_set_text(GTK_ENTRY(compose->from_name), mailto_from);
		g_free(mailto_from);
	} else
		/* override from name according to folder properties */
		if (item && item->prefs &&
			item->prefs->compose_with_format &&
			item->prefs->compose_override_from_format &&
			*item->prefs->compose_override_from_format != '\0') {

			gchar *tmp = NULL;
			gchar *buf = NULL;

			dummyinfo = compose_msginfo_new_from_compose(compose);

			/* decode \-escape sequences in the internal representation of the quote format */
			tmp = g_malloc(strlen(item->prefs->compose_override_from_format)+1);
			pref_get_unescaped_pref(tmp, item->prefs->compose_override_from_format);

#ifdef USE_ENCHANT
			quote_fmt_init(dummyinfo, NULL, NULL, FALSE, compose->account, FALSE,
					compose->gtkaspell);
#else
			quote_fmt_init(dummyinfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
			quote_fmt_scan_string(tmp);
			quote_fmt_parse();

			buf = quote_fmt_get_buffer();
			if (buf == NULL)
				alertpanel_error(_("New message From format error."));
			else
				gtk_entry_set_text(GTK_ENTRY(compose->from_name), buf);
			quote_fmt_reset_vartable();

			g_free(tmp);
		}

	compose->replyinfo = NULL;
	compose->fwdinfo   = NULL;

	textview = GTK_TEXT_VIEW(compose->text);
	textbuf = gtk_text_view_get_buffer(textview);
	compose_create_tags(textview, compose);

	undo_block(compose->undostruct);
#ifdef USE_ENCHANT
	compose_set_dictionaries_from_folder_prefs(compose, item);
#endif

	if (account->auto_sig)
		compose_insert_sig(compose, FALSE);
	gtk_text_buffer_get_start_iter(textbuf, &iter);
	gtk_text_buffer_place_cursor(textbuf, &iter);

	if (account->protocol != A_NNTP) {
		if (mailto && *mailto != '\0') {
			mfield = compose_entries_set(compose, mailto, COMPOSE_TO);

		} else {
			compose_set_folder_prefs(compose, item, TRUE);
		}
		if (item && item->ret_rcpt) {
			cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/RequestRetRcpt", TRUE);
		}
	} else {
		if (mailto && *mailto != '\0') {
			if (!strchr(mailto, '@'))
				mfield = compose_entries_set(compose, mailto, COMPOSE_NEWSGROUPS);
			else
				mfield = compose_entries_set(compose, mailto, COMPOSE_TO);
		} else if (item && FOLDER_CLASS(item->folder) == news_get_class()) {
			compose_entry_append(compose, item->path, COMPOSE_NEWSGROUPS, PREF_FOLDER);
			mfield = TO_FIELD_PRESENT;
		}
		/*
		 * CLAWS: just don't allow return receipt request, even if the user
		 * may want to send an email. simple but foolproof.
		 */
		cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/RequestRetRcpt", FALSE); 
	}
	compose_add_field_list( compose, listAddress );

	if (item && item->prefs && item->prefs->compose_with_format) {
		subject_format = item->prefs->compose_subject_format;
		body_format = item->prefs->compose_body_format;
	} else if (account->compose_with_format) {
		subject_format = account->compose_subject_format;
		body_format = account->compose_body_format;
	} else if (prefs_common.compose_with_format) {
		subject_format = prefs_common.compose_subject_format;
		body_format = prefs_common.compose_body_format;
	}

	if (subject_format || body_format) {

		if ( subject_format
			 && *subject_format != '\0' )
		{
			gchar *subject = NULL;
			gchar *tmp = NULL;
			gchar *buf = NULL;

			if (!dummyinfo)
				dummyinfo = compose_msginfo_new_from_compose(compose);

			/* decode \-escape sequences in the internal representation of the quote format */
			tmp = g_malloc(strlen(subject_format)+1);
			pref_get_unescaped_pref(tmp, subject_format);

			subject = gtk_editable_get_chars(GTK_EDITABLE(compose->subject_entry), 0, -1);
#ifdef USE_ENCHANT
			quote_fmt_init(dummyinfo, NULL, subject, FALSE, compose->account, FALSE,
					compose->gtkaspell);
#else
			quote_fmt_init(dummyinfo, NULL, subject, FALSE, compose->account, FALSE);
#endif
			quote_fmt_scan_string(tmp);
			quote_fmt_parse();

			buf = quote_fmt_get_buffer();
			if (buf == NULL)
				alertpanel_error(_("New message subject format error."));
			else
				gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), buf);
			compose_attach_from_list(compose, quote_fmt_get_attachments_list(), FALSE);
			quote_fmt_reset_vartable();

			g_free(subject);
			g_free(tmp);
			mfield = SUBJECT_FIELD_PRESENT;
		}

		if ( body_format
			 && *body_format != '\0' )
		{
			GtkTextView *text;
			GtkTextBuffer *buffer;
			GtkTextIter start, end;
			gchar *tmp = NULL;

			if (!dummyinfo)
				dummyinfo = compose_msginfo_new_from_compose(compose);

			text = GTK_TEXT_VIEW(compose->text);
			buffer = gtk_text_view_get_buffer(text);
			gtk_text_buffer_get_start_iter(buffer, &start);
			gtk_text_buffer_get_iter_at_offset(buffer, &end, -1);
			tmp = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);

			compose_quote_fmt(compose, dummyinfo,
			        	  body_format,
			        	  NULL, tmp, FALSE, TRUE,
						  _("The body of the \"New message\" template has an error at line %d."));
			compose_attach_from_list(compose, quote_fmt_get_attachments_list(), FALSE);
			quote_fmt_reset_vartable();

			g_free(tmp);
#ifdef USE_ENCHANT
			if (compose->gtkaspell && compose->gtkaspell->check_while_typing)
				gtkaspell_highlight_all(compose->gtkaspell);
#endif
			mfield = BODY_FIELD_PRESENT;
		}

	}
	procmsg_msginfo_free( dummyinfo );

	if (attach_files) {
		gint i;
		gchar *file;

		for (i = 0; i < attach_files->len; i++) {
			file = g_ptr_array_index(attach_files, i);
			compose_attach_append(compose, file, file, NULL, NULL);
		}
	}

	compose_show_first_last_header(compose, TRUE);

	/* Set save folder */
	if (item && item->prefs && item->prefs->save_copy_to_folder) {
		gchar *folderidentifier;

    		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn), prefs_common.savemsg);
		folderidentifier = folder_item_get_identifier(item);
		compose_set_save_to(compose, folderidentifier);
		g_free(folderidentifier);
	}

	/* Place cursor according to provided input (mfield) */
	switch (mfield) { 
		case NO_FIELD_PRESENT:
			if (compose->header_last)
				gtk_widget_grab_focus(compose->header_last->entry);
			break;
		case TO_FIELD_PRESENT:
			buf = gtk_editable_get_chars(GTK_EDITABLE(compose->subject_entry), 0, -1);
			if (buf) {
				gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), buf);
				g_free(buf);
			}
			gtk_widget_grab_focus(compose->subject_entry);
			break;
		case SUBJECT_FIELD_PRESENT:
			textview = GTK_TEXT_VIEW(compose->text);
			if (!textview)
				break;
			textbuf = gtk_text_view_get_buffer(textview);
			if (!textbuf)
				break;
			mark = gtk_text_buffer_get_insert(textbuf);
			gtk_text_buffer_get_iter_at_mark(textbuf, &iter, mark);
			gtk_text_buffer_insert(textbuf, &iter, "", -1);
		    /* 
		     * SUBJECT_FIELD_PRESENT and BODY_FIELD_PRESENT
		     * only defers where it comes to the variable body
		     * is not null. If no body is present compose->text
		     * will be null in which case you cannot place the
		     * cursor inside the component so. An empty component
		     * is therefore created before placing the cursor
		     */
		case BODY_FIELD_PRESENT:
			cursor_pos = quote_fmt_get_cursor_pos();
			if (cursor_pos == -1)
				gtk_widget_grab_focus(compose->header_last->entry);
			else
				gtk_widget_grab_focus(compose->text);
			break;
	}

	undo_unblock(compose->undostruct);

	if (prefs_common.auto_exteditor)
		compose_exec_ext_editor(compose);

	compose->draft_timeout_tag = -1;
	SCROLL_TO_CURSOR(compose);

	compose->modified = FALSE;
	compose_set_title(compose);

	hooks_invoke(COMPOSE_CREATED_HOOKLIST, compose);

        return compose;
}

static void compose_force_encryption(Compose *compose, PrefsAccount *account,
		gboolean override_pref, const gchar *system)
{
	const gchar *privacy = NULL;

	cm_return_if_fail(compose != NULL);
	cm_return_if_fail(account != NULL);

	if (override_pref == FALSE && account->default_encrypt_reply == FALSE)
		return;

	if (system)
		privacy = system;
	else if (account->default_privacy_system
	&&  strlen(account->default_privacy_system)) {
		privacy = account->default_privacy_system;
	} else {
		GSList *privacy_avail = privacy_get_system_ids();
		if (privacy_avail && g_slist_length(privacy_avail)) {
			privacy = (gchar *)(privacy_avail->data);
		}
	}
	if (privacy != NULL) {
		if (system) {
			g_free(compose->privacy_system);
			compose->privacy_system = NULL;
		}
		if (compose->privacy_system == NULL)
			compose->privacy_system = g_strdup(privacy);
		else if (*(compose->privacy_system) == '\0') {
			g_free(compose->privacy_system);
			compose->privacy_system = g_strdup(privacy);
		}
		compose_update_privacy_system_menu_item(compose, FALSE);
		compose_use_encryption(compose, TRUE);
	}
}	

static void compose_force_signing(Compose *compose, PrefsAccount *account, const gchar *system)
{
	const gchar *privacy = NULL;

	if (system)
		privacy = system;
	else if (account->default_privacy_system
	&&  strlen(account->default_privacy_system)) {
		privacy = account->default_privacy_system;
	} else {
		GSList *privacy_avail = privacy_get_system_ids();
		if (privacy_avail && g_slist_length(privacy_avail)) {
			privacy = (gchar *)(privacy_avail->data);
		}
	}

	if (privacy != NULL) {
		if (system) {
			g_free(compose->privacy_system);
			compose->privacy_system = NULL;
		}
		if (compose->privacy_system == NULL)
			compose->privacy_system = g_strdup(privacy);
		compose_update_privacy_system_menu_item(compose, FALSE);
		compose_use_signing(compose, TRUE);
	}
}	

static Compose *compose_reply_mode(ComposeMode mode, GSList *msginfo_list, gchar *body)
{
	MsgInfo *msginfo;
	guint list_len;
	Compose *compose = NULL;
	
	cm_return_val_if_fail(msginfo_list != NULL, NULL);

	msginfo = (MsgInfo*)g_slist_nth_data(msginfo_list, 0);
	cm_return_val_if_fail(msginfo != NULL, NULL);

	list_len = g_slist_length(msginfo_list);

	switch (mode) {
	case COMPOSE_REPLY:
	case COMPOSE_REPLY_TO_ADDRESS:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_CHECK,
		    	      FALSE, prefs_common.default_reply_list, FALSE, body);
		break;
	case COMPOSE_REPLY_WITH_QUOTE:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_FORCED, 
			FALSE, prefs_common.default_reply_list, FALSE, body);
		break;
	case COMPOSE_REPLY_WITHOUT_QUOTE:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_SKIP, 
			FALSE, prefs_common.default_reply_list, FALSE, NULL);
		break;
	case COMPOSE_REPLY_TO_SENDER:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_CHECK,
			      FALSE, FALSE, TRUE, body);
		break;
	case COMPOSE_FOLLOWUP_AND_REPLY_TO:
		compose = compose_followup_and_reply_to(msginfo,
					      COMPOSE_QUOTE_CHECK,
					      FALSE, FALSE, body);
		break;
	case COMPOSE_REPLY_TO_SENDER_WITH_QUOTE:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_FORCED, 
			FALSE, FALSE, TRUE, body);
		break;
	case COMPOSE_REPLY_TO_SENDER_WITHOUT_QUOTE:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_SKIP, 
			FALSE, FALSE, TRUE, NULL);
		break;
	case COMPOSE_REPLY_TO_ALL:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_CHECK,
			TRUE, FALSE, FALSE, body);
		break;
	case COMPOSE_REPLY_TO_ALL_WITH_QUOTE:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_FORCED, 
			TRUE, FALSE, FALSE, body);
		break;
	case COMPOSE_REPLY_TO_ALL_WITHOUT_QUOTE:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_SKIP, 
			TRUE, FALSE, FALSE, NULL);
		break;
	case COMPOSE_REPLY_TO_LIST:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_CHECK,
			FALSE, TRUE, FALSE, body);
		break;
	case COMPOSE_REPLY_TO_LIST_WITH_QUOTE:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_FORCED, 
			FALSE, TRUE, FALSE, body);
		break;
	case COMPOSE_REPLY_TO_LIST_WITHOUT_QUOTE:
		compose = compose_reply(msginfo, COMPOSE_QUOTE_SKIP, 
			FALSE, TRUE, FALSE, NULL);
		break;
	case COMPOSE_FORWARD:
		if (prefs_common.forward_as_attachment) {
			compose = compose_reply_mode(COMPOSE_FORWARD_AS_ATTACH, msginfo_list, body);
			return compose;
		} else {
			compose = compose_reply_mode(COMPOSE_FORWARD_INLINE, msginfo_list, body);
			return compose;
		}
		break;
	case COMPOSE_FORWARD_INLINE:
		/* check if we reply to more than one Message */
		if (list_len == 1) {
			compose = compose_forward(NULL, msginfo, FALSE, body, FALSE, FALSE);
			break;
		} 
		/* more messages FALL THROUGH */
	case COMPOSE_FORWARD_AS_ATTACH:
		compose = compose_forward_multiple(NULL, msginfo_list);
		break;
	case COMPOSE_REDIRECT:
		compose = compose_redirect(NULL, msginfo, FALSE);
		break;
	default:
		g_warning("compose_reply_mode(): invalid Compose Mode: %d\n", mode);
	}
	
	if (compose == NULL) {
		alertpanel_error(_("Unable to reply. The original email probably doesn't exist."));
		return NULL;
	}

	compose->rmode = mode;
	switch (compose->rmode) {
	case COMPOSE_REPLY:
	case COMPOSE_REPLY_WITH_QUOTE:
	case COMPOSE_REPLY_WITHOUT_QUOTE:
	case COMPOSE_FOLLOWUP_AND_REPLY_TO:
		debug_print("reply mode Normal\n");
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/ReplyMode/Normal", TRUE);
		compose_reply_change_mode(compose, COMPOSE_REPLY); /* force update */
		break;
	case COMPOSE_REPLY_TO_SENDER:
	case COMPOSE_REPLY_TO_SENDER_WITH_QUOTE:
	case COMPOSE_REPLY_TO_SENDER_WITHOUT_QUOTE:
		debug_print("reply mode Sender\n");
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/ReplyMode/Sender", TRUE);
		break;
	case COMPOSE_REPLY_TO_ALL:
	case COMPOSE_REPLY_TO_ALL_WITH_QUOTE:
	case COMPOSE_REPLY_TO_ALL_WITHOUT_QUOTE:
		debug_print("reply mode All\n");
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/ReplyMode/All", TRUE);
		break;
	case COMPOSE_REPLY_TO_LIST:
	case COMPOSE_REPLY_TO_LIST_WITH_QUOTE:
	case COMPOSE_REPLY_TO_LIST_WITHOUT_QUOTE:
		debug_print("reply mode List\n");
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/ReplyMode/List", TRUE);
		break;
	case COMPOSE_REPLY_TO_ADDRESS:
		cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/ReplyMode", FALSE);
		break;
	default:
		break;
	}
	return compose;
}

static Compose *compose_reply(MsgInfo *msginfo,
				   ComposeQuoteMode quote_mode,
				   gboolean to_all,
				   gboolean to_ml,
				   gboolean to_sender, 
				   const gchar *body)
{
	return compose_generic_reply(msginfo, quote_mode, to_all, to_ml, 
			      to_sender, FALSE, body);
}

static Compose *compose_followup_and_reply_to(MsgInfo *msginfo,
				   ComposeQuoteMode quote_mode,
				   gboolean to_all,
				   gboolean to_sender,
				   const gchar *body)
{
	return compose_generic_reply(msginfo, quote_mode, to_all, FALSE, 
			      to_sender, TRUE, body);
}

static void compose_extract_original_charset(Compose *compose)
{
	MsgInfo *info = NULL;
	if (compose->replyinfo) {
		info = compose->replyinfo;
	} else if (compose->fwdinfo) {
		info = compose->fwdinfo;
	} else if (compose->targetinfo) {
		info = compose->targetinfo;
	}
	if (info) {
		MimeInfo *mimeinfo = procmime_scan_message_short(info);
		MimeInfo *partinfo = mimeinfo;
		while (partinfo && partinfo->type != MIMETYPE_TEXT)
			partinfo = procmime_mimeinfo_next(partinfo);
		if (partinfo) {
			compose->orig_charset = 
				g_strdup(procmime_mimeinfo_get_parameter(
						partinfo, "charset"));
		}
		procmime_mimeinfo_free_all(mimeinfo);
	}
}

#define SIGNAL_BLOCK(buffer) {					\
	g_signal_handlers_block_by_func(G_OBJECT(buffer),	\
				G_CALLBACK(compose_changed_cb),	\
				compose);			\
	g_signal_handlers_block_by_func(G_OBJECT(buffer),	\
				G_CALLBACK(text_inserted),	\
				compose);			\
}

#define SIGNAL_UNBLOCK(buffer) {				\
	g_signal_handlers_unblock_by_func(G_OBJECT(buffer),	\
				G_CALLBACK(compose_changed_cb),	\
				compose);			\
	g_signal_handlers_unblock_by_func(G_OBJECT(buffer),	\
				G_CALLBACK(text_inserted),	\
				compose);			\
}

static Compose *compose_generic_reply(MsgInfo *msginfo,
				  ComposeQuoteMode quote_mode,
				  gboolean to_all, gboolean to_ml,
				  gboolean to_sender,
				  gboolean followup_and_reply_to,
				  const gchar *body)
{
	Compose *compose;
	PrefsAccount *account = NULL;
	GtkTextView *textview;
	GtkTextBuffer *textbuf;
	gboolean quote = FALSE;
	const gchar *qmark = NULL;
	const gchar *body_fmt = NULL;
	gchar *s_system = NULL;
	START_TIMING("");
	cm_return_val_if_fail(msginfo != NULL, NULL);
	cm_return_val_if_fail(msginfo->folder != NULL, NULL);

	account = account_get_reply_account(msginfo, prefs_common.reply_account_autosel);

	cm_return_val_if_fail(account != NULL, NULL);

	compose = compose_create(account, msginfo->folder, COMPOSE_REPLY, FALSE);

	compose->updating = TRUE;

	cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/RemoveReferences", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/RemoveReferences", TRUE);

	compose->replyinfo = procmsg_msginfo_get_full_info(msginfo);
	if (!compose->replyinfo)
		compose->replyinfo = procmsg_msginfo_copy(msginfo);

	compose_extract_original_charset(compose);
	
    	if (msginfo->folder && msginfo->folder->ret_rcpt)
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/RequestRetRcpt", TRUE);

	/* Set save folder */
	if (msginfo->folder && msginfo->folder->prefs && msginfo->folder->prefs->save_copy_to_folder) {
		gchar *folderidentifier;

    		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn), TRUE);
		folderidentifier = folder_item_get_identifier(msginfo->folder);
		compose_set_save_to(compose, folderidentifier);
		g_free(folderidentifier);
	}

	if (compose_parse_header(compose, msginfo) < 0) {
		compose->updating = FALSE;
		compose_destroy(compose);
		return NULL;
	}

	/* override from name according to folder properties */
	if (msginfo->folder && msginfo->folder->prefs &&
		msginfo->folder->prefs->reply_with_format &&
		msginfo->folder->prefs->reply_override_from_format &&
		*msginfo->folder->prefs->reply_override_from_format != '\0') {

		gchar *tmp = NULL;
		gchar *buf = NULL;

		/* decode \-escape sequences in the internal representation of the quote format */
		tmp = g_malloc(strlen(msginfo->folder->prefs->reply_override_from_format)+1);
		pref_get_unescaped_pref(tmp, msginfo->folder->prefs->reply_override_from_format);

#ifdef USE_ENCHANT
		quote_fmt_init(compose->replyinfo, NULL, NULL, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(compose->replyinfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
		quote_fmt_scan_string(tmp);
		quote_fmt_parse();

		buf = quote_fmt_get_buffer();
		if (buf == NULL)
			alertpanel_error(_("The \"From\" field of the \"Reply\" template contains an invalid email address."));
		else
			gtk_entry_set_text(GTK_ENTRY(compose->from_name), buf);
		quote_fmt_reset_vartable();

		g_free(tmp);
	}

	textview = (GTK_TEXT_VIEW(compose->text));
	textbuf = gtk_text_view_get_buffer(textview);
	compose_create_tags(textview, compose);

	undo_block(compose->undostruct);
#ifdef USE_ENCHANT
		compose_set_dictionaries_from_folder_prefs(compose, msginfo->folder);
#endif

	if (quote_mode == COMPOSE_QUOTE_FORCED ||
			(quote_mode == COMPOSE_QUOTE_CHECK && prefs_common.reply_with_quote)) {
		/* use the reply format of folder (if enabled), or the account's one
		   (if enabled) or fallback to the global reply format, which is always
		   enabled (even if empty), and use the relevant quotemark */
		quote = TRUE;
		if (msginfo->folder && msginfo->folder->prefs &&
				msginfo->folder->prefs->reply_with_format) {
			qmark = msginfo->folder->prefs->reply_quotemark;
			body_fmt = msginfo->folder->prefs->reply_body_format;

		} else if (account->reply_with_format) {
			qmark = account->reply_quotemark;
			body_fmt = account->reply_body_format;

		} else {
			qmark = prefs_common.quotemark;
			if (prefs_common.quotefmt && *prefs_common.quotefmt)
				body_fmt = gettext(prefs_common.quotefmt);
			else
				body_fmt = "";
		}
	}

	if (quote) {
		/* empty quotemark is not allowed */
		if (qmark == NULL || *qmark == '\0')
			qmark = "> ";
		compose_quote_fmt(compose, compose->replyinfo,
			          body_fmt, qmark, body, FALSE, TRUE,
					  _("The body of the \"Reply\" template has an error at line %d."));
		compose_attach_from_list(compose, quote_fmt_get_attachments_list(), FALSE);
		quote_fmt_reset_vartable();
#ifdef USE_ENCHANT
		if (compose->gtkaspell && compose->gtkaspell->check_while_typing)
			gtkaspell_highlight_all(compose->gtkaspell);
#endif
	}

	if (MSG_IS_ENCRYPTED(compose->replyinfo->flags)) {
		compose_force_encryption(compose, account, FALSE, s_system);
	}

	privacy_msginfo_get_signed_state(compose->replyinfo, &s_system);
	if (MSG_IS_SIGNED(compose->replyinfo->flags) && account->default_sign_reply) {
		compose_force_signing(compose, account, s_system);
	}
	g_free(s_system);

	SIGNAL_BLOCK(textbuf);
	
	if (account->auto_sig)
		compose_insert_sig(compose, FALSE);

	compose_wrap_all(compose);

	SIGNAL_UNBLOCK(textbuf);
	
	gtk_widget_grab_focus(compose->text);

	undo_unblock(compose->undostruct);

	if (prefs_common.auto_exteditor)
		compose_exec_ext_editor(compose);
		
	compose->modified = FALSE;
	compose_set_title(compose);

	compose->updating = FALSE;
	compose->draft_timeout_tag = -1; /* desinhibit auto-drafting after loading */
	SCROLL_TO_CURSOR(compose);
	
	if (compose->deferred_destroy) {
		compose_destroy(compose);
		return NULL;
	}
	END_TIMING();

	return compose;
}

#define INSERT_FW_HEADER(var, hdr) \
if (msginfo->var && *msginfo->var) { \
	gtk_stext_insert(text, NULL, NULL, NULL, hdr, -1); \
	gtk_stext_insert(text, NULL, NULL, NULL, msginfo->var, -1); \
	gtk_stext_insert(text, NULL, NULL, NULL, "\n", 1); \
}

Compose *compose_forward(PrefsAccount *account, MsgInfo *msginfo,
			 gboolean as_attach, const gchar *body,
			 gboolean no_extedit,
			 gboolean batch)
{
	Compose *compose;
	GtkTextView *textview;
	GtkTextBuffer *textbuf;
	gint cursor_pos = -1;
	ComposeMode mode;

	cm_return_val_if_fail(msginfo != NULL, NULL);
	cm_return_val_if_fail(msginfo->folder != NULL, NULL);

	if (!account && 
	    !(account = compose_guess_forward_account_from_msginfo
				(msginfo)))
		account = cur_account;

	if (!prefs_common.forward_as_attachment)
		mode = COMPOSE_FORWARD_INLINE;
	else
		mode = COMPOSE_FORWARD;
	compose = compose_create(account, msginfo->folder, mode, batch);

	compose->updating = TRUE;
	compose->fwdinfo = procmsg_msginfo_get_full_info(msginfo);
	if (!compose->fwdinfo)
		compose->fwdinfo = procmsg_msginfo_copy(msginfo);

	compose_extract_original_charset(compose);

	if (msginfo->subject && *msginfo->subject) {
		gchar *buf, *buf2, *p;

		buf = p = g_strdup(msginfo->subject);
		p += subject_get_prefix_length(p);
		memmove(buf, p, strlen(p) + 1);

		buf2 = g_strdup_printf("Fw: %s", buf);
		gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), buf2);
		
		g_free(buf);
		g_free(buf2);
	}

	/* override from name according to folder properties */
	if (msginfo->folder && msginfo->folder->prefs &&
		msginfo->folder->prefs->forward_with_format &&
		msginfo->folder->prefs->forward_override_from_format &&
		*msginfo->folder->prefs->forward_override_from_format != '\0') {

		gchar *tmp = NULL;
		gchar *buf = NULL;
		MsgInfo *full_msginfo = NULL;

		if (!as_attach)
			full_msginfo = procmsg_msginfo_get_full_info(msginfo);
		if (!full_msginfo)
			full_msginfo = procmsg_msginfo_copy(msginfo);

		/* decode \-escape sequences in the internal representation of the quote format */
		tmp = g_malloc(strlen(msginfo->folder->prefs->forward_override_from_format)+1);
		pref_get_unescaped_pref(tmp, msginfo->folder->prefs->forward_override_from_format);

#ifdef USE_ENCHANT
		quote_fmt_init(full_msginfo, NULL, NULL, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(full_msginfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
		quote_fmt_scan_string(tmp);
		quote_fmt_parse();

		buf = quote_fmt_get_buffer();
		if (buf == NULL)
			alertpanel_error(_("The \"From\" field of the \"Forward\" template contains an invalid email address."));
		else
			gtk_entry_set_text(GTK_ENTRY(compose->from_name), buf);
		quote_fmt_reset_vartable();

		g_free(tmp);
		procmsg_msginfo_free(full_msginfo);
	}

	textview = GTK_TEXT_VIEW(compose->text);
	textbuf = gtk_text_view_get_buffer(textview);
	compose_create_tags(textview, compose);
	
	undo_block(compose->undostruct);
	if (as_attach) {
		gchar *msgfile;

		msgfile = procmsg_get_message_file(msginfo);
		if (!is_file_exist(msgfile))
			g_warning("%s: file not exist\n", msgfile);
		else
			compose_attach_append(compose, msgfile, msgfile,
					      "message/rfc822", NULL);

		g_free(msgfile);
	} else {
		const gchar *qmark = NULL;
		const gchar *body_fmt = NULL;
		MsgInfo *full_msginfo;

		if (prefs_common.fw_quotefmt && *prefs_common.fw_quotefmt)
			body_fmt = gettext(prefs_common.fw_quotefmt);
		else
			body_fmt = "";
	
		full_msginfo = procmsg_msginfo_get_full_info(msginfo);
		if (!full_msginfo)
			full_msginfo = procmsg_msginfo_copy(msginfo);

		/* use the forward format of folder (if enabled), or the account's one
		   (if enabled) or fallback to the global forward format, which is always
		   enabled (even if empty), and use the relevant quotemark */
		if (msginfo->folder && msginfo->folder->prefs &&
				msginfo->folder->prefs->forward_with_format) {
			qmark = msginfo->folder->prefs->forward_quotemark;
			body_fmt = msginfo->folder->prefs->forward_body_format;

		} else if (account->forward_with_format) {
			qmark = account->forward_quotemark;
			body_fmt = account->forward_body_format;

		} else {
			qmark = prefs_common.fw_quotemark;
			if (prefs_common.fw_quotefmt && *prefs_common.fw_quotefmt)
				body_fmt = gettext(prefs_common.fw_quotefmt);
			else
				body_fmt = "";
		}

		/* empty quotemark is not allowed */
		if (qmark == NULL || *qmark == '\0')
			qmark = "> ";

		compose_quote_fmt(compose, full_msginfo,
			          body_fmt, qmark, body, FALSE, TRUE,
					  _("The body of the \"Forward\" template has an error at line %d."));
		compose_attach_from_list(compose, quote_fmt_get_attachments_list(), FALSE);
		quote_fmt_reset_vartable();
		compose_attach_parts(compose, msginfo);

		procmsg_msginfo_free(full_msginfo);
#ifdef USE_ENCHANT
		if (compose->gtkaspell && compose->gtkaspell->check_while_typing)
			gtkaspell_highlight_all(compose->gtkaspell);
#endif
	}

	SIGNAL_BLOCK(textbuf);

	if (account->auto_sig)
		compose_insert_sig(compose, FALSE);

	compose_wrap_all(compose);

	SIGNAL_UNBLOCK(textbuf);
	
	cursor_pos = quote_fmt_get_cursor_pos();
	if (cursor_pos == -1)
		gtk_widget_grab_focus(compose->header_last->entry);
	else
		gtk_widget_grab_focus(compose->text);

	if (!no_extedit && prefs_common.auto_exteditor)
		compose_exec_ext_editor(compose);
	
	/*save folder*/
	if (msginfo->folder && msginfo->folder->prefs && msginfo->folder->prefs->save_copy_to_folder) {
		gchar *folderidentifier;

    		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn), TRUE);
		folderidentifier = folder_item_get_identifier(msginfo->folder);
		compose_set_save_to(compose, folderidentifier);
		g_free(folderidentifier);
	}

	undo_unblock(compose->undostruct);
	
	compose->modified = FALSE;
	compose_set_title(compose);

	compose->updating = FALSE;
	compose->draft_timeout_tag = -1; /* desinhibit auto-drafting after loading */
	SCROLL_TO_CURSOR(compose);

	if (compose->deferred_destroy) {
		compose_destroy(compose);
		return NULL;
	}

	hooks_invoke(COMPOSE_CREATED_HOOKLIST, compose);

        return compose;
}

#undef INSERT_FW_HEADER

static Compose *compose_forward_multiple(PrefsAccount *account, GSList *msginfo_list)
{
	Compose *compose;
	GtkTextView *textview;
	GtkTextBuffer *textbuf;
	GtkTextIter iter;
	GSList *msginfo;
	gchar *msgfile;
	gboolean single_mail = TRUE;
	
	cm_return_val_if_fail(msginfo_list != NULL, NULL);

	if (g_slist_length(msginfo_list) > 1)
		single_mail = FALSE;

	for (msginfo = msginfo_list; msginfo != NULL; msginfo = msginfo->next)
		if (((MsgInfo *)msginfo->data)->folder == NULL)
			return NULL;

	/* guess account from first selected message */
	if (!account && 
	    !(account = compose_guess_forward_account_from_msginfo
				(msginfo_list->data)))
		account = cur_account;

	cm_return_val_if_fail(account != NULL, NULL);

	for (msginfo = msginfo_list; msginfo != NULL; msginfo = msginfo->next) {
		if (msginfo->data) {
			MSG_UNSET_PERM_FLAGS(((MsgInfo *)msginfo->data)->flags, MSG_REPLIED);
			MSG_SET_PERM_FLAGS(((MsgInfo *)msginfo->data)->flags, MSG_FORWARDED);
		}
	}

	if (msginfo_list == NULL || msginfo_list->data == NULL) {
		g_warning("no msginfo_list");
		return NULL;
	}

	compose = compose_create(account, ((MsgInfo *)msginfo_list->data)->folder, COMPOSE_FORWARD, FALSE);

	compose->updating = TRUE;

	/* override from name according to folder properties */
	if (msginfo_list->data) {
		MsgInfo *msginfo = msginfo_list->data;

		if (msginfo->folder && msginfo->folder->prefs &&
			msginfo->folder->prefs->forward_with_format &&
			msginfo->folder->prefs->forward_override_from_format &&
			*msginfo->folder->prefs->forward_override_from_format != '\0') {

			gchar *tmp = NULL;
			gchar *buf = NULL;

			/* decode \-escape sequences in the internal representation of the quote format */
			tmp = g_malloc(strlen(msginfo->folder->prefs->forward_override_from_format)+1);
			pref_get_unescaped_pref(tmp, msginfo->folder->prefs->forward_override_from_format);

#ifdef USE_ENCHANT
			quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE,
					compose->gtkaspell);
#else
			quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
			quote_fmt_scan_string(tmp);
			quote_fmt_parse();

			buf = quote_fmt_get_buffer();
			if (buf == NULL)
				alertpanel_error(_("The \"From\" field of the \"Forward\" template contains an invalid email address."));
			else
				gtk_entry_set_text(GTK_ENTRY(compose->from_name), buf);
			quote_fmt_reset_vartable();

			g_free(tmp);
		}
	}

	textview = GTK_TEXT_VIEW(compose->text);
	textbuf = gtk_text_view_get_buffer(textview);
	compose_create_tags(textview, compose);
	
	undo_block(compose->undostruct);
	for (msginfo = msginfo_list; msginfo != NULL; msginfo = msginfo->next) {
		msgfile = procmsg_get_message_file((MsgInfo *)msginfo->data);

		if (!is_file_exist(msgfile))
			g_warning("%s: file not exist\n", msgfile);
		else
			compose_attach_append(compose, msgfile, msgfile,
				"message/rfc822", NULL);
		g_free(msgfile);
	}
	
	if (single_mail) {
		MsgInfo *info = (MsgInfo *)msginfo_list->data;
		if (info->subject && *info->subject) {
			gchar *buf, *buf2, *p;

			buf = p = g_strdup(info->subject);
			p += subject_get_prefix_length(p);
			memmove(buf, p, strlen(p) + 1);

			buf2 = g_strdup_printf("Fw: %s", buf);
			gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), buf2);

			g_free(buf);
			g_free(buf2);
		}
	} else {
		gtk_entry_set_text(GTK_ENTRY(compose->subject_entry),
			_("Fw: multiple emails"));
	}

	SIGNAL_BLOCK(textbuf);
	
	if (account->auto_sig)
		compose_insert_sig(compose, FALSE);

	compose_wrap_all(compose);

	SIGNAL_UNBLOCK(textbuf);
	
	gtk_text_buffer_get_start_iter(textbuf, &iter);
	gtk_text_buffer_place_cursor(textbuf, &iter);

	gtk_widget_grab_focus(compose->header_last->entry);
	undo_unblock(compose->undostruct);
	compose->modified = FALSE;
	compose_set_title(compose);

	compose->updating = FALSE;
	compose->draft_timeout_tag = -1; /* desinhibit auto-drafting after loading */
	SCROLL_TO_CURSOR(compose);

	if (compose->deferred_destroy) {
		compose_destroy(compose);
		return NULL;
	}

	hooks_invoke(COMPOSE_CREATED_HOOKLIST, compose);

	return compose;
}

static gboolean compose_is_sig_separator(Compose *compose, GtkTextBuffer *textbuf, GtkTextIter *iter) 
{
	GtkTextIter start = *iter;
	GtkTextIter end_iter;
	int start_pos = gtk_text_iter_get_offset(&start);
	gchar *str = NULL;
	if (!compose->account->sig_sep)
		return FALSE;
	
	gtk_text_buffer_get_iter_at_offset(textbuf, &end_iter,
		start_pos+strlen(compose->account->sig_sep));

	/* check sig separator */
	str = gtk_text_iter_get_text(&start, &end_iter);
	if (!strcmp(str, compose->account->sig_sep)) {
		gchar *tmp = NULL;
		/* check end of line (\n) */
		gtk_text_buffer_get_iter_at_offset(textbuf, &start,
			start_pos+strlen(compose->account->sig_sep));
		gtk_text_buffer_get_iter_at_offset(textbuf, &end_iter,
			start_pos+strlen(compose->account->sig_sep)+1);
		tmp = gtk_text_iter_get_text(&start, &end_iter);
		if (!strcmp(tmp,"\n")) {
			g_free(str);
			g_free(tmp);
			return TRUE;
		}
		g_free(tmp);	
	}
	g_free(str);

	return FALSE;
}

static void compose_colorize_signature(Compose *compose)
{
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(compose->text));
	GtkTextIter iter;
	GtkTextIter end_iter;
	gtk_text_buffer_get_start_iter(buffer, &iter);
	while (gtk_text_iter_forward_line(&iter))
		if (compose_is_sig_separator(compose, buffer, &iter)) {
			gtk_text_buffer_get_end_iter(buffer, &end_iter);
			gtk_text_buffer_apply_tag_by_name(buffer,"signature",&iter, &end_iter);
		}
}

#define BLOCK_WRAP() {							\
	prev_autowrap = compose->autowrap;				\
	buffer = gtk_text_view_get_buffer(				\
					GTK_TEXT_VIEW(compose->text));	\
	compose->autowrap = FALSE;					\
									\
	g_signal_handlers_block_by_func(G_OBJECT(buffer),		\
				G_CALLBACK(compose_changed_cb),		\
				compose);				\
	g_signal_handlers_block_by_func(G_OBJECT(buffer),		\
				G_CALLBACK(text_inserted),		\
				compose);				\
}
#define UNBLOCK_WRAP() {						\
	compose->autowrap = prev_autowrap;				\
	if (compose->autowrap) {					\
		gint old = compose->draft_timeout_tag;			\
		compose->draft_timeout_tag = -2;			\
		compose_wrap_all(compose);				\
		compose->draft_timeout_tag = old;			\
	}								\
									\
	g_signal_handlers_unblock_by_func(G_OBJECT(buffer),		\
				G_CALLBACK(compose_changed_cb),		\
				compose);				\
	g_signal_handlers_unblock_by_func(G_OBJECT(buffer),		\
				G_CALLBACK(text_inserted),		\
				compose);				\
}

Compose *compose_reedit(MsgInfo *msginfo, gboolean batch)
{
	Compose *compose = NULL;
	PrefsAccount *account = NULL;
	GtkTextView *textview;
	GtkTextBuffer *textbuf;
	GtkTextMark *mark;
	GtkTextIter iter;
	FILE *fp;
	gchar buf[BUFFSIZE];
	gboolean use_signing = FALSE;
	gboolean use_encryption = FALSE;
	gchar *privacy_system = NULL;
	int priority = PRIORITY_NORMAL;
	MsgInfo *replyinfo = NULL, *fwdinfo = NULL;
	gboolean autowrap = prefs_common.autowrap;
	gboolean autoindent = prefs_common.auto_indent;

	cm_return_val_if_fail(msginfo != NULL, NULL);
	cm_return_val_if_fail(msginfo->folder != NULL, NULL);

	if (compose_put_existing_to_front(msginfo)) {
		return NULL;
	}

        if (folder_has_parent_of_type(msginfo->folder, F_QUEUE) ||
	    folder_has_parent_of_type(msginfo->folder, F_DRAFT) ||
	    folder_has_parent_of_type(msginfo->folder, F_OUTBOX)) {
		gchar queueheader_buf[BUFFSIZE];
		gint id, param;

		/* Select Account from queue headers */
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Claws-Account-Id:")) {
			id = atoi(&queueheader_buf[strlen("X-Claws-Account-Id:")]);
			account = account_find_from_id(id);
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Sylpheed-Account-Id:")) {
			id = atoi(&queueheader_buf[strlen("X-Sylpheed-Account-Id:")]);
			account = account_find_from_id(id);
		}
		if (!account && !procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "NAID:")) {
			id = atoi(&queueheader_buf[strlen("NAID:")]);
			account = account_find_from_id(id);
		}
		if (!account && !procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
		                                    sizeof(queueheader_buf), "MAID:")) {
			id = atoi(&queueheader_buf[strlen("MAID:")]);
			account = account_find_from_id(id);
		}
		if (!account && !procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
		                                                sizeof(queueheader_buf), "S:")) {
			account = account_find_from_address(queueheader_buf, FALSE);
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Claws-Sign:")) {
			param = atoi(&queueheader_buf[strlen("X-Claws-Sign:")]);
			use_signing = param;
			
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Sylpheed-Sign:")) {
			param = atoi(&queueheader_buf[strlen("X-Sylpheed-Sign:")]);
			use_signing = param;
			
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Claws-Encrypt:")) {
			param = atoi(&queueheader_buf[strlen("X-Claws-Encrypt:")]);
			use_encryption = param;
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Sylpheed-Encrypt:")) {
			param = atoi(&queueheader_buf[strlen("X-Sylpheed-Encrypt:")]);
			use_encryption = param;
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Claws-Auto-Wrapping:")) {
			param = atoi(&queueheader_buf[strlen("X-Claws-Auto-Wrapping:")]);
			autowrap = param;
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Claws-Auto-Indent:")) {
			param = atoi(&queueheader_buf[strlen("X-Claws-Auto-Indent:")]);
			autoindent = param;
		}
                if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
                                            sizeof(queueheader_buf), "X-Claws-Privacy-System:")) {
                        privacy_system = g_strdup(&queueheader_buf[strlen("X-Claws-Privacy-System:")]);
                }
                if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
                                            sizeof(queueheader_buf), "X-Sylpheed-Privacy-System:")) {
                        privacy_system = g_strdup(&queueheader_buf[strlen("X-Sylpheed-Privacy-System:")]);
                }
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "X-Priority: ")) {
			param = atoi(&queueheader_buf[strlen("X-Priority: ")]); /* mind the space */
			priority = param;
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "RMID:")) {
			gchar **tokens = g_strsplit(&queueheader_buf[strlen("RMID:")], "\t", 0);
			if (tokens[0] && tokens[1] && tokens[2]) {
				FolderItem *orig_item = folder_find_item_from_identifier(tokens[0]);
				if (orig_item != NULL) {
					replyinfo = folder_item_get_msginfo_by_msgid(orig_item, tokens[2]);
				}
			}
			g_strfreev(tokens);
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, 
					     sizeof(queueheader_buf), "FMID:")) {
			gchar **tokens = g_strsplit(&queueheader_buf[strlen("FMID:")], "\t", 0);
			if (tokens[0] && tokens[1] && tokens[2]) {
				FolderItem *orig_item = folder_find_item_from_identifier(tokens[0]);
				if (orig_item != NULL) {
					fwdinfo = folder_item_get_msginfo_by_msgid(orig_item, tokens[2]);
				}
			}
			g_strfreev(tokens);
		}
	} else {
		account = msginfo->folder->folder->account;
	}

	if (!account && prefs_common.reedit_account_autosel) {
               	gchar from[BUFFSIZE];
		if (!procheader_get_header_from_msginfo(msginfo, from, sizeof(from), "FROM:")) {
		        extract_address(from);
		        account = account_find_from_address(from, FALSE);
                }
	}
        if (!account) {
        	account = cur_account;
        }
	cm_return_val_if_fail(account != NULL, NULL);

	compose = compose_create(account, msginfo->folder, COMPOSE_REEDIT, batch);

	cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Edit/AutoWrap", autowrap);
	cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Edit/AutoIndent", autoindent);
	compose->autowrap = autowrap;
	compose->replyinfo = replyinfo;
	compose->fwdinfo = fwdinfo;

	compose->updating = TRUE;
	compose->priority = priority;

	if (privacy_system != NULL) {
		compose->privacy_system = privacy_system;
		compose_use_signing(compose, use_signing);
		compose_use_encryption(compose, use_encryption);
		compose_update_privacy_system_menu_item(compose, FALSE);
	} else {
		activate_privacy_system(compose, account, FALSE);
	}

	compose->targetinfo = procmsg_msginfo_copy(msginfo);

	compose_extract_original_charset(compose);

        if (folder_has_parent_of_type(msginfo->folder, F_QUEUE) ||
	    folder_has_parent_of_type(msginfo->folder, F_DRAFT) ||
	    folder_has_parent_of_type(msginfo->folder, F_OUTBOX)) {
		gchar queueheader_buf[BUFFSIZE];

		/* Set message save folder */
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, sizeof(queueheader_buf), "SCF:")) {
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn), TRUE);
			compose_set_save_to(compose, &queueheader_buf[4]);
		}
		if (!procheader_get_header_from_msginfo(msginfo, queueheader_buf, sizeof(queueheader_buf), "RRCPT:")) {
			gint active = atoi(&queueheader_buf[strlen("RRCPT:")]);
			if (active) {
				cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/RequestRetRcpt", TRUE);
			}
		}
	}
	
	if (compose_parse_header(compose, msginfo) < 0) {
		compose->updating = FALSE;
		compose_destroy(compose);
		return NULL;
	}
	compose_reedit_set_entry(compose, msginfo);

	textview = GTK_TEXT_VIEW(compose->text);
	textbuf = gtk_text_view_get_buffer(textview);
	compose_create_tags(textview, compose);

	mark = gtk_text_buffer_get_insert(textbuf);
	gtk_text_buffer_get_iter_at_mark(textbuf, &iter, mark);

	g_signal_handlers_block_by_func(G_OBJECT(textbuf),
					G_CALLBACK(compose_changed_cb),
					compose);
	
	if (MSG_IS_ENCRYPTED(msginfo->flags)) {
		fp = procmime_get_first_encrypted_text_content(msginfo);
		if (fp) {
			compose_force_encryption(compose, account, TRUE, NULL);
		}
	} else {
		fp = procmime_get_first_text_content(msginfo);
	}
	if (fp == NULL) {
		g_warning("Can't get text part\n");
	}

	if (fp != NULL) {
		gboolean prev_autowrap = compose->autowrap;
		GtkTextBuffer *buffer = textbuf;
		BLOCK_WRAP();
		while (fgets(buf, sizeof(buf), fp) != NULL) {
			strcrchomp(buf);
			gtk_text_buffer_insert(textbuf, &iter, buf, -1);
		}
		UNBLOCK_WRAP();
		fclose(fp);
	}
	
	compose_attach_parts(compose, msginfo);

	compose_colorize_signature(compose);

	g_signal_handlers_unblock_by_func(G_OBJECT(textbuf),
					G_CALLBACK(compose_changed_cb),
					compose);

	gtk_widget_grab_focus(compose->text);

        if (prefs_common.auto_exteditor) {
		compose_exec_ext_editor(compose);
	}
	compose->modified = FALSE;
	compose_set_title(compose);

	compose->updating = FALSE;
	compose->draft_timeout_tag = -1; /* desinhibit auto-drafting after loading */
	SCROLL_TO_CURSOR(compose);

	if (compose->deferred_destroy) {
		compose_destroy(compose);
		return NULL;
	}
	
	compose->sig_str = account_get_signature_str(compose->account);
	
	hooks_invoke(COMPOSE_CREATED_HOOKLIST, compose);

	return compose;
}

Compose *compose_redirect(PrefsAccount *account, MsgInfo *msginfo,
						 gboolean batch)
{
	Compose *compose;
	gchar *filename;
	FolderItem *item;

	cm_return_val_if_fail(msginfo != NULL, NULL);

	if (!account)
		account = account_get_reply_account(msginfo,
					prefs_common.reply_account_autosel);
	cm_return_val_if_fail(account != NULL, NULL);

	compose = compose_create(account, msginfo->folder, COMPOSE_REDIRECT, batch);

	compose->updating = TRUE;

	compose_create_tags(GTK_TEXT_VIEW(compose->text), compose);
	compose->replyinfo = NULL;
	compose->fwdinfo = NULL;

	compose_show_first_last_header(compose, TRUE);

	gtk_widget_grab_focus(compose->header_last->entry);

	filename = procmsg_get_message_file(msginfo);

	if (filename == NULL) {
		compose->updating = FALSE;
		compose_destroy(compose);

		return NULL;
	}

	compose->redirect_filename = filename;
	
	/* Set save folder */
	item = msginfo->folder;
	if (item && item->prefs && item->prefs->save_copy_to_folder) {
		gchar *folderidentifier;

    		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn), prefs_common.savemsg);
		folderidentifier = folder_item_get_identifier(item);
		compose_set_save_to(compose, folderidentifier);
		g_free(folderidentifier);
	}

	compose_attach_parts(compose, msginfo);

	if (msginfo->subject)
		gtk_entry_set_text(GTK_ENTRY(compose->subject_entry),
				   msginfo->subject);
	gtk_editable_set_editable(GTK_EDITABLE(compose->subject_entry), FALSE);

	compose_quote_fmt(compose, msginfo, "%M", NULL, NULL, FALSE, FALSE,
					  _("The body of the \"Redirect\" template has an error at line %d."));
	quote_fmt_reset_vartable();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(compose->text), FALSE);

	compose_colorize_signature(compose);

	
	cm_menu_set_sensitive_full(compose->ui_manager, "Popup/Compose/Add", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Popup/Compose/Remove", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Popup/Compose/Properties", FALSE);

	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message/Save", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message/InsertFile", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message/AttachFile", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message/InsertSig", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Tools/ShowRuler", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Tools/Actions", FALSE);
	
	if (compose->toolbar->draft_btn)
		gtk_widget_set_sensitive(compose->toolbar->draft_btn, FALSE);
	if (compose->toolbar->insert_btn)
		gtk_widget_set_sensitive(compose->toolbar->insert_btn, FALSE);
	if (compose->toolbar->attach_btn)
		gtk_widget_set_sensitive(compose->toolbar->attach_btn, FALSE);
	if (compose->toolbar->sig_btn)
		gtk_widget_set_sensitive(compose->toolbar->sig_btn, FALSE);
	if (compose->toolbar->exteditor_btn)
		gtk_widget_set_sensitive(compose->toolbar->exteditor_btn, FALSE);
	if (compose->toolbar->linewrap_current_btn)
		gtk_widget_set_sensitive(compose->toolbar->linewrap_current_btn, FALSE);
	if (compose->toolbar->linewrap_all_btn)
		gtk_widget_set_sensitive(compose->toolbar->linewrap_all_btn, FALSE);

	compose->modified = FALSE;
	compose_set_title(compose);
	compose->updating = FALSE;
	compose->draft_timeout_tag = -1; /* desinhibit auto-drafting after loading */
	SCROLL_TO_CURSOR(compose);

	if (compose->deferred_destroy) {
		compose_destroy(compose);
		return NULL;
	}
	
	hooks_invoke(COMPOSE_CREATED_HOOKLIST, compose);

	return compose;
}

GList *compose_get_compose_list(void)
{
	return compose_list;
}

void compose_entry_append(Compose *compose, const gchar *address,
			  ComposeEntryType type, ComposePrefType pref_type)
{
	const gchar *header;
	gchar *cur, *begin;
	gboolean in_quote = FALSE;
	if (!address || *address == '\0') return;

	switch (type) {
	case COMPOSE_CC:
		header = N_("Cc:");
		break;
	case COMPOSE_BCC:
		header = N_("Bcc:");
		break;
	case COMPOSE_REPLYTO:
		header = N_("Reply-To:");
		break;
	case COMPOSE_NEWSGROUPS:
		header = N_("Newsgroups:");
		break;
	case COMPOSE_FOLLOWUPTO:
		header = N_( "Followup-To:");
		break;
	case COMPOSE_INREPLYTO:
		header = N_( "In-Reply-To:");
		break;
	case COMPOSE_TO:
	default:
		header = N_("To:");
		break;
	}
	header = prefs_common_translated_header_name(header);
	
	cur = begin = (gchar *)address;
	
	/* we separate the line by commas, but not if we're inside a quoted
	 * string */
	while (*cur != '\0') {
		if (*cur == '"') 
			in_quote = !in_quote;
		if (*cur == ',' && !in_quote) {
			gchar *tmp = g_strdup(begin);
			gchar *o_tmp = tmp;
			tmp[cur-begin]='\0';
			cur++;
			begin = cur;
			while (*tmp == ' ' || *tmp == '\t')
				tmp++;
			compose_add_header_entry(compose, header, tmp, pref_type);
			g_free(o_tmp);
			continue;
		}
		cur++;
	}
	if (begin < cur) {
		gchar *tmp = g_strdup(begin);
		gchar *o_tmp = tmp;
		tmp[cur-begin]='\0';
		cur++;
		begin = cur;
		while (*tmp == ' ' || *tmp == '\t')
			tmp++;
		compose_add_header_entry(compose, header, tmp, pref_type);
		g_free(o_tmp);		
	}
}

static void compose_entry_mark_default_to(Compose *compose, const gchar *mailto)
{
	static GdkColor yellow;
	static GdkColor black;
	static gboolean yellow_initialised = FALSE;
	GSList *h_list;
	GtkEntry *entry;
		
	if (!yellow_initialised) {
		gdk_color_parse("#f5f6be", &yellow);
		gdk_color_parse("#000000", &black);
		yellow_initialised = gdk_colormap_alloc_color(
			gdk_colormap_get_system(), &yellow, FALSE, TRUE);
		yellow_initialised &= gdk_colormap_alloc_color(
			gdk_colormap_get_system(), &black, FALSE, TRUE);
	}

	for (h_list = compose->header_list; h_list != NULL; h_list = h_list->next) {
		entry = GTK_ENTRY(((ComposeHeaderEntry *)h_list->data)->entry);
		if (gtk_entry_get_text(entry) && 
		    !g_utf8_collate(gtk_entry_get_text(entry), mailto)) {
			if (yellow_initialised) {
				gtk_widget_modify_base(
					GTK_WIDGET(((ComposeHeaderEntry *)h_list->data)->entry),
					GTK_STATE_NORMAL, &yellow);
				gtk_widget_modify_text(
					GTK_WIDGET(((ComposeHeaderEntry *)h_list->data)->entry),
					GTK_STATE_NORMAL, &black);
			}
		}
	}
}

void compose_toolbar_cb(gint action, gpointer data)
{
	ToolbarItem *toolbar_item = (ToolbarItem*)data;
	Compose *compose = (Compose*)toolbar_item->parent;
	
	cm_return_if_fail(compose != NULL);

	switch(action) {
	case A_SEND:
		compose_send_cb(NULL, compose);
		break;
	case A_SENDL:
		compose_send_later_cb(NULL, compose);
		break;
	case A_DRAFT:
		compose_draft(compose, COMPOSE_QUIT_EDITING);
		break;
	case A_INSERT:
		compose_insert_file_cb(NULL, compose);
		break;
	case A_ATTACH:
		compose_attach_cb(NULL, compose);
		break;
	case A_SIG:
		compose_insert_sig(compose, FALSE);
		break;
	case A_EXTEDITOR:
		compose_ext_editor_cb(NULL, compose);
		break;
	case A_LINEWRAP_CURRENT:
		compose_beautify_paragraph(compose, NULL, TRUE);
		break;
	case A_LINEWRAP_ALL:
		compose_wrap_all_full(compose, TRUE);
		break;
	case A_ADDRBOOK:
		compose_address_cb(NULL, compose);
		break;
#ifdef USE_ENCHANT
	case A_CHECK_SPELLING:
		compose_check_all(NULL, compose);
		break;
#endif
	default:
		break;
	}
}

static MailField compose_entries_set(Compose *compose, const gchar *mailto, ComposeEntryType to_type)
{
	gchar *to = NULL;
	gchar *cc = NULL;
	gchar *bcc = NULL;
	gchar *subject = NULL;
	gchar *body = NULL;
	gchar *temp = NULL;
	gsize  len = 0;
	gchar **attach = NULL;
 	gchar *inreplyto = NULL;
	MailField mfield = NO_FIELD_PRESENT;

	/* get mailto parts but skip from */
	scan_mailto_url(mailto, NULL, &to, &cc, &bcc, &subject, &body, &attach, &inreplyto);

	if (to) {
		compose_entry_append(compose, to, to_type, PREF_MAILTO);
		mfield = TO_FIELD_PRESENT;
	}
	if (cc)
		compose_entry_append(compose, cc, COMPOSE_CC, PREF_MAILTO);
	if (bcc)
		compose_entry_append(compose, bcc, COMPOSE_BCC, PREF_MAILTO);
	if (subject) {
		if (!g_utf8_validate (subject, -1, NULL)) {
			temp = g_locale_to_utf8 (subject, -1, NULL, &len, NULL);
			gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), temp);
			g_free(temp);
		} else {
			gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), subject);
		}
		mfield = SUBJECT_FIELD_PRESENT;
	}
	if (body) {
		GtkTextView *text = GTK_TEXT_VIEW(compose->text);
		GtkTextBuffer *buffer = gtk_text_view_get_buffer(text);
		GtkTextMark *mark;
		GtkTextIter iter;
		gboolean prev_autowrap = compose->autowrap;

		compose->autowrap = FALSE;

		mark = gtk_text_buffer_get_insert(buffer);
		gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);

		if (!g_utf8_validate (body, -1, NULL)) {
			temp = g_locale_to_utf8 (body, -1, NULL, &len, NULL);
			gtk_text_buffer_insert(buffer, &iter, temp, -1);
			g_free(temp);
		} else {
			gtk_text_buffer_insert(buffer, &iter, body, -1);
		}
		gtk_text_buffer_insert(buffer, &iter, "\n", 1);

		compose->autowrap = prev_autowrap;
		if (compose->autowrap)
			compose_wrap_all(compose);
		mfield = BODY_FIELD_PRESENT;
	}

	if (attach) {
		gint i = 0, att = 0;
		gchar *warn_files = NULL;
		while (attach[i] != NULL) {
			gchar *utf8_filename = conv_filename_to_utf8(attach[i]);
			if (utf8_filename) {
				if (compose_attach_append(compose, attach[i], utf8_filename, NULL, NULL)) {
					gchar *tmp = g_strdup_printf("%s%s\n",
							warn_files?warn_files:"",
							utf8_filename);
					g_free(warn_files);
					warn_files = tmp;
					att++;
				}
				g_free(utf8_filename);
			} else {
				alertpanel_error(_("Couldn't attach a file (charset conversion failed)."));
			}
			i++;
		}
		if (warn_files) {
			alertpanel_notice(ngettext(
			"The following file has been attached: \n%s",
			"The following files have been attached: \n%s", att), warn_files);
			g_free(warn_files);
		}
	}
 	if (inreplyto)
 		compose_entry_append(compose, inreplyto, COMPOSE_INREPLYTO, PREF_MAILTO);

	g_free(to);
	g_free(cc);
	g_free(bcc);
	g_free(subject);
	g_free(body);
	g_strfreev(attach);
	g_free(inreplyto);
	
	return mfield;
}

static gint compose_parse_header(Compose *compose, MsgInfo *msginfo)
{
	static HeaderEntry hentry[] = {{"Reply-To:",	NULL, TRUE},
				       {"Cc:",		NULL, TRUE},
				       {"References:",	NULL, FALSE},
				       {"Bcc:",		NULL, TRUE},
				       {"Newsgroups:",  NULL, TRUE},
				       {"Followup-To:", NULL, TRUE},
				       {"List-Post:",	NULL, FALSE},
				       {"X-Priority:",	NULL, FALSE},
				       {NULL,		NULL, FALSE}};

	enum
	{
		H_REPLY_TO	= 0,
		H_CC		= 1,
		H_REFERENCES	= 2,
		H_BCC		= 3,
		H_NEWSGROUPS	= 4,
		H_FOLLOWUP_TO	= 5,
		H_LIST_POST	= 6,
 		H_X_PRIORITY	= 7
	};

	FILE *fp;

	cm_return_val_if_fail(msginfo != NULL, -1);

	if ((fp = procmsg_open_message(msginfo)) == NULL) return -1;
	procheader_get_header_fields(fp, hentry);
	fclose(fp);

	if (hentry[H_REPLY_TO].body != NULL) {
		if (hentry[H_REPLY_TO].body[0] != '\0') {
			compose->replyto =
				conv_unmime_header(hentry[H_REPLY_TO].body,
						   NULL, TRUE);
		}
		g_free(hentry[H_REPLY_TO].body);
		hentry[H_REPLY_TO].body = NULL;
	}
	if (hentry[H_CC].body != NULL) {
		compose->cc = conv_unmime_header(hentry[H_CC].body, NULL, TRUE);
		g_free(hentry[H_CC].body);
		hentry[H_CC].body = NULL;
	}
	if (hentry[H_REFERENCES].body != NULL) {
		if (compose->mode == COMPOSE_REEDIT)
			compose->references = hentry[H_REFERENCES].body;
		else {
			compose->references = compose_parse_references
				(hentry[H_REFERENCES].body, msginfo->msgid);
			g_free(hentry[H_REFERENCES].body);
		}
		hentry[H_REFERENCES].body = NULL;
	}
	if (hentry[H_BCC].body != NULL) {
		if (compose->mode == COMPOSE_REEDIT)
			compose->bcc =
				conv_unmime_header(hentry[H_BCC].body, NULL, TRUE);
		g_free(hentry[H_BCC].body);
		hentry[H_BCC].body = NULL;
	}
	if (hentry[H_NEWSGROUPS].body != NULL) {
		compose->newsgroups = hentry[H_NEWSGROUPS].body;
		hentry[H_NEWSGROUPS].body = NULL;
	}
	if (hentry[H_FOLLOWUP_TO].body != NULL) {
		if (hentry[H_FOLLOWUP_TO].body[0] != '\0') {
			compose->followup_to =
				conv_unmime_header(hentry[H_FOLLOWUP_TO].body,
						   NULL, TRUE);
		}
		g_free(hentry[H_FOLLOWUP_TO].body);
		hentry[H_FOLLOWUP_TO].body = NULL;
	}
	if (hentry[H_LIST_POST].body != NULL) {
		gchar *to = NULL, *start = NULL;

		extract_address(hentry[H_LIST_POST].body);
		if (hentry[H_LIST_POST].body[0] != '\0') {
			start = strstr(hentry[H_LIST_POST].body, "mailto:");
			
			scan_mailto_url(start ? start : hentry[H_LIST_POST].body,
					NULL, &to, NULL, NULL, NULL, NULL, NULL, NULL);

			if (to) {
				g_free(compose->ml_post);
				compose->ml_post = to;
			}
		}
		g_free(hentry[H_LIST_POST].body);
		hentry[H_LIST_POST].body = NULL;
	}

	/* CLAWS - X-Priority */
	if (compose->mode == COMPOSE_REEDIT)
		if (hentry[H_X_PRIORITY].body != NULL) {
			gint priority;
			
			priority = atoi(hentry[H_X_PRIORITY].body);
			g_free(hentry[H_X_PRIORITY].body);
			
			hentry[H_X_PRIORITY].body = NULL;
			
			if (priority < PRIORITY_HIGHEST || 
			    priority > PRIORITY_LOWEST)
				priority = PRIORITY_NORMAL;
			
			compose->priority =  priority;
		}
 
	if (compose->mode == COMPOSE_REEDIT) {
		if (msginfo->inreplyto && *msginfo->inreplyto)
			compose->inreplyto = g_strdup(msginfo->inreplyto);
		return 0;
	}

	if (msginfo->msgid && *msginfo->msgid)
		compose->inreplyto = g_strdup(msginfo->msgid);

	if (!compose->references) {
		if (msginfo->msgid && *msginfo->msgid) {
			if (msginfo->inreplyto && *msginfo->inreplyto)
				compose->references =
					g_strdup_printf("<%s>\n\t<%s>",
							msginfo->inreplyto,
							msginfo->msgid);
			else
				compose->references =
					g_strconcat("<", msginfo->msgid, ">",
						    NULL);
		} else if (msginfo->inreplyto && *msginfo->inreplyto) {
			compose->references =
				g_strconcat("<", msginfo->inreplyto, ">",
					    NULL);
		}
	}

	return 0;
}

static gchar *compose_parse_references(const gchar *ref, const gchar *msgid)
{
	GSList *ref_id_list, *cur;
	GString *new_ref;
	gchar *new_ref_str;

	ref_id_list = references_list_append(NULL, ref);
	if (!ref_id_list) return NULL;
	if (msgid && *msgid)
		ref_id_list = g_slist_append(ref_id_list, g_strdup(msgid));

	for (;;) {
		gint len = 0;

		for (cur = ref_id_list; cur != NULL; cur = cur->next)
			/* "<" + Message-ID + ">" + CR+LF+TAB */
			len += strlen((gchar *)cur->data) + 5;

		if (len > MAX_REFERENCES_LEN) {
			/* remove second message-ID */
			if (ref_id_list && ref_id_list->next &&
			    ref_id_list->next->next) {
				g_free(ref_id_list->next->data);
				ref_id_list = g_slist_remove
					(ref_id_list, ref_id_list->next->data);
			} else {
				slist_free_strings(ref_id_list);
				g_slist_free(ref_id_list);
				return NULL;
			}
		} else
			break;
	}

	new_ref = g_string_new("");
	for (cur = ref_id_list; cur != NULL; cur = cur->next) {
		if (new_ref->len > 0)
			g_string_append(new_ref, "\n\t");
		g_string_append_printf(new_ref, "<%s>", (gchar *)cur->data);
	}

	slist_free_strings(ref_id_list);
	g_slist_free(ref_id_list);

	new_ref_str = new_ref->str;
	g_string_free(new_ref, FALSE);

	return new_ref_str;
}

static gchar *compose_quote_fmt(Compose *compose, MsgInfo *msginfo,
				const gchar *fmt, const gchar *qmark,
				const gchar *body, gboolean rewrap,
				gboolean need_unescape,
				const gchar *err_msg)
{
	MsgInfo* dummyinfo = NULL;
	gchar *quote_str = NULL;
	gchar *buf;
	gboolean prev_autowrap;
	const gchar *trimmed_body = body;
	gint cursor_pos = -1;
	GtkTextView *text = GTK_TEXT_VIEW(compose->text);
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(text);
	GtkTextIter iter;
	GtkTextMark *mark;
	

	SIGNAL_BLOCK(buffer);

	if (!msginfo) {
		dummyinfo = compose_msginfo_new_from_compose(compose);
		msginfo = dummyinfo;
	}

	if (qmark != NULL) {
#ifdef USE_ENCHANT
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
		quote_fmt_scan_string(qmark);
		quote_fmt_parse();

		buf = quote_fmt_get_buffer();
		if (buf == NULL)
			alertpanel_error(_("The \"Quotation mark\" of the template is invalid."));
		else
			Xstrdup_a(quote_str, buf, goto error)
	}

	if (fmt && *fmt != '\0') {

		if (trimmed_body)
			while (*trimmed_body == '\n')
				trimmed_body++;

#ifdef USE_ENCHANT
		quote_fmt_init(msginfo, quote_str, trimmed_body, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(msginfo, quote_str, trimmed_body, FALSE, compose->account, FALSE);
#endif
		if (need_unescape) {
			gchar *tmp = NULL;

			/* decode \-escape sequences in the internal representation of the quote format */
			tmp = g_malloc(strlen(fmt)+1);
			pref_get_unescaped_pref(tmp, fmt);
			quote_fmt_scan_string(tmp);
			quote_fmt_parse();
			g_free(tmp);
		} else {
			quote_fmt_scan_string(fmt);
			quote_fmt_parse();
		}

		buf = quote_fmt_get_buffer();
		if (buf == NULL) {
			gint line = quote_fmt_get_line();
			alertpanel_error(err_msg, line);
			goto error;
		}
	} else
		buf = "";

	prev_autowrap = compose->autowrap;
	compose->autowrap = FALSE;

	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
	if (g_utf8_validate(buf, -1, NULL)) { 
		gtk_text_buffer_insert(buffer, &iter, buf, -1);
	} else {
		gchar *tmpout = NULL;
		tmpout = conv_codeset_strdup
			(buf, conv_get_locale_charset_str_no_utf8(),
			 CS_INTERNAL);
		if (!tmpout || !g_utf8_validate(tmpout, -1, NULL)) {
			g_free(tmpout);
			tmpout = g_malloc(strlen(buf)*2+1);
			conv_localetodisp(tmpout, strlen(buf)*2+1, buf);
		}
		gtk_text_buffer_insert(buffer, &iter, tmpout, -1);
		g_free(tmpout);
	}

	cursor_pos = quote_fmt_get_cursor_pos();
	if (cursor_pos == -1)
		cursor_pos = gtk_text_iter_get_offset(&iter);
	compose->set_cursor_pos = cursor_pos;

	gtk_text_buffer_get_start_iter(buffer, &iter);
	gtk_text_buffer_get_iter_at_offset(buffer, &iter, cursor_pos);
	gtk_text_buffer_place_cursor(buffer, &iter);

	compose->autowrap = prev_autowrap;
	if (compose->autowrap && rewrap)
		compose_wrap_all(compose);

	goto ok;

error:
	buf = NULL;
ok:
	SIGNAL_UNBLOCK(buffer);

	procmsg_msginfo_free( dummyinfo );

	return buf;
}

/* if ml_post is of type addr@host and from is of type
 * addr-anything@host, return TRUE
 */
static gboolean is_subscription(const gchar *ml_post, const gchar *from)
{
	gchar *left_ml = NULL;
	gchar *right_ml = NULL;
	gchar *left_from = NULL;
	gchar *right_from = NULL;
	gboolean result = FALSE;
	
	if (!ml_post || !from)
		return FALSE;
	
	left_ml = g_strdup(ml_post);
	if (strstr(left_ml, "@")) {
		right_ml = strstr(left_ml, "@")+1;
		*(strstr(left_ml, "@")) = '\0';
	}
	
	left_from = g_strdup(from);
	if (strstr(left_from, "@")) {
		right_from = strstr(left_from, "@")+1;
		*(strstr(left_from, "@")) = '\0';
	}
	
	if (left_ml && left_from && right_ml && right_from
	&&  !strncmp(left_from, left_ml, strlen(left_ml))
	&&  !strcmp(right_from, right_ml)) {
		result = TRUE;
	}
	g_free(left_ml);
	g_free(left_from);
	
	return result;
}

static void compose_set_folder_prefs(Compose *compose, FolderItem *folder,
				     gboolean respect_default_to)
{
	if (!compose)
		return;
	if (!folder || !folder->prefs)
		return;

	if (respect_default_to && folder->prefs->enable_default_to) {
		compose_entry_append(compose, folder->prefs->default_to,
					COMPOSE_TO, PREF_FOLDER);
		compose_entry_mark_default_to(compose, folder->prefs->default_to);
	}
	if (folder->prefs->enable_default_cc)
		compose_entry_append(compose, folder->prefs->default_cc,
					COMPOSE_CC, PREF_FOLDER);
	if (folder->prefs->enable_default_bcc)
		compose_entry_append(compose, folder->prefs->default_bcc,
					COMPOSE_BCC, PREF_FOLDER);
	if (folder->prefs->enable_default_replyto)
		compose_entry_append(compose, folder->prefs->default_replyto,
					COMPOSE_REPLYTO, PREF_FOLDER);
}

static void compose_reply_set_subject(Compose *compose, MsgInfo *msginfo)
{
	gchar *buf, *buf2;
	gchar *p;
	
	if (!compose || !msginfo)
		return;

	if (msginfo->subject && *msginfo->subject) {
		buf = p = g_strdup(msginfo->subject);
		p += subject_get_prefix_length(p);
		memmove(buf, p, strlen(p) + 1);

		buf2 = g_strdup_printf("Re: %s", buf);
		gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), buf2);

		g_free(buf2);
		g_free(buf);
	} else
		gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), "Re: ");
}

static void compose_reply_set_entry(Compose *compose, MsgInfo *msginfo,
				    gboolean to_all, gboolean to_ml,
				    gboolean to_sender,
				    gboolean followup_and_reply_to)
{
	GSList *cc_list = NULL;
	GSList *cur;
	gchar *from = NULL;
	gchar *replyto = NULL;
	gchar *ac_email = NULL;

	gboolean reply_to_ml = FALSE;
	gboolean default_reply_to = FALSE;

	cm_return_if_fail(compose->account != NULL);
	cm_return_if_fail(msginfo != NULL);

	reply_to_ml = to_ml && compose->ml_post;

	default_reply_to = msginfo->folder && 
		msginfo->folder->prefs->enable_default_reply_to;

	if (compose->account->protocol != A_NNTP) {
		compose_set_folder_prefs(compose, msginfo->folder, FALSE);

		if (reply_to_ml && !default_reply_to) {
			
			gboolean is_subscr = is_subscription(compose->ml_post,
							     msginfo->from);
			if (!is_subscr) {
				/* normal answer to ml post with a reply-to */
				compose_entry_append(compose,
					   compose->ml_post,
					   COMPOSE_TO, PREF_ML);
				if (compose->replyto)
					compose_entry_append(compose,
						compose->replyto,
						COMPOSE_CC, PREF_ML);
			} else {
				/* answer to subscription confirmation */
				if (compose->replyto)
					compose_entry_append(compose,
						compose->replyto,
						COMPOSE_TO, PREF_ML);
				else if (msginfo->from)
					compose_entry_append(compose,
						msginfo->from,
						COMPOSE_TO, PREF_ML);
			}
		}
		else if (!(to_all || to_sender) && default_reply_to) {
			compose_entry_append(compose,
			    msginfo->folder->prefs->default_reply_to,
			    COMPOSE_TO, PREF_FOLDER);
			compose_entry_mark_default_to(compose,
				msginfo->folder->prefs->default_reply_to);
		} else {
			gchar *tmp1 = NULL;
			if (!msginfo->from)
				return;
			Xstrdup_a(tmp1, msginfo->from, return);
			extract_address(tmp1);
			if (to_all || to_sender ||
			    !account_find_from_address(tmp1, FALSE))
				compose_entry_append(compose,
				 (compose->replyto && !to_sender)
					  ? compose->replyto :
					  msginfo->from ? msginfo->from : "",
					  COMPOSE_TO, PREF_NONE);
			else if (!to_all && !to_sender) {
				if (!folder_has_parent_of_type(msginfo->folder, F_QUEUE) &&
				    !folder_has_parent_of_type(msginfo->folder, F_OUTBOX) &&
				    !folder_has_parent_of_type(msginfo->folder, F_DRAFT)) {
					if (compose->replyto) {
						compose_entry_append(compose,
			    				compose->replyto,
			    				COMPOSE_TO, PREF_NONE);
					} else {
						compose_entry_append(compose,
							  msginfo->from ? msginfo->from : "",
							  COMPOSE_TO, PREF_NONE);
					}
				} else {
					/* replying to own mail, use original recp */
					compose_entry_append(compose,
						  msginfo->to ? msginfo->to : "",
						  COMPOSE_TO, PREF_NONE);
					compose_entry_append(compose,
						  msginfo->cc ? msginfo->cc : "",
						  COMPOSE_CC, PREF_NONE);
				}
			}
		}
	} else {
		if (to_sender || (compose->followup_to && 
			!strncmp(compose->followup_to, "poster", 6)))
			compose_entry_append
				(compose, 
				 (compose->replyto ? compose->replyto :
		    		 	msginfo->from ? msginfo->from : ""),
				 COMPOSE_TO, PREF_NONE);
				 
		else if (followup_and_reply_to || to_all) {
			compose_entry_append
		    		(compose,
		    		 (compose->replyto ? compose->replyto :
		    		 msginfo->from ? msginfo->from : ""),
		    		 COMPOSE_TO, PREF_NONE);				
		
			compose_entry_append
				(compose,
			 	 compose->followup_to ? compose->followup_to :
			 	 compose->newsgroups ? compose->newsgroups : "",
			 	 COMPOSE_NEWSGROUPS, PREF_NONE);
		} 
		else 
			compose_entry_append
				(compose,
			 	 compose->followup_to ? compose->followup_to :
			 	 compose->newsgroups ? compose->newsgroups : "",
			 	 COMPOSE_NEWSGROUPS, PREF_NONE);
	}
	compose_reply_set_subject(compose, msginfo);

	if (to_ml && compose->ml_post) return;
	if (!to_all || compose->account->protocol == A_NNTP) return;

	if (compose->replyto) {
		Xstrdup_a(replyto, compose->replyto, return);
		extract_address(replyto);
	}
	if (msginfo->from) {
		Xstrdup_a(from, msginfo->from, return);
		extract_address(from);
	}

	if (replyto && from)
		cc_list = address_list_append_with_comments(cc_list, from);
	if (to_all && msginfo->folder && 
	    msginfo->folder->prefs->enable_default_reply_to)
	    	cc_list = address_list_append_with_comments(cc_list,
				msginfo->folder->prefs->default_reply_to);
	cc_list = address_list_append_with_comments(cc_list, msginfo->to);
	cc_list = address_list_append_with_comments(cc_list, compose->cc);

	ac_email = g_utf8_strdown(compose->account->address, -1);

	if (cc_list) {
		for (cur = cc_list; cur != NULL; cur = cur->next) {
			gchar *addr = g_utf8_strdown(cur->data, -1);
			extract_address(addr);
		
			if (strcmp(ac_email, addr))
				compose_entry_append(compose, (gchar *)cur->data,
						     COMPOSE_CC, PREF_NONE);
			else
				debug_print("Cc address same as compose account's, ignoring\n");

			g_free(addr);
		}
		
		slist_free_strings(cc_list);
		g_slist_free(cc_list);
	}
	
	g_free(ac_email);
}

#define SET_ENTRY(entry, str) \
{ \
	if (str && *str) \
		gtk_entry_set_text(GTK_ENTRY(compose->entry), str); \
}

#define SET_ADDRESS(type, str) \
{ \
	if (str && *str) \
		compose_entry_append(compose, str, type, PREF_NONE); \
}

static void compose_reedit_set_entry(Compose *compose, MsgInfo *msginfo)
{
	cm_return_if_fail(msginfo != NULL);

	SET_ENTRY(subject_entry, msginfo->subject);
	SET_ENTRY(from_name, msginfo->from);
	SET_ADDRESS(COMPOSE_TO, msginfo->to);
	SET_ADDRESS(COMPOSE_CC, compose->cc);
	SET_ADDRESS(COMPOSE_BCC, compose->bcc);
	SET_ADDRESS(COMPOSE_REPLYTO, compose->replyto);
	SET_ADDRESS(COMPOSE_NEWSGROUPS, compose->newsgroups);
	SET_ADDRESS(COMPOSE_FOLLOWUPTO, compose->followup_to);

	compose_update_priority_menu_item(compose);
	compose_update_privacy_system_menu_item(compose, FALSE);
	compose_show_first_last_header(compose, TRUE);
}

#undef SET_ENTRY
#undef SET_ADDRESS

static void compose_insert_sig(Compose *compose, gboolean replace)
{
	GtkTextView *text = GTK_TEXT_VIEW(compose->text);
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(text);
	GtkTextMark *mark;
	GtkTextIter iter, iter_end;
	gint cur_pos, ins_pos;
	gboolean prev_autowrap;
	gboolean found = FALSE;
	gboolean exists = FALSE;
	
	cm_return_if_fail(compose->account != NULL);

	BLOCK_WRAP();

	g_signal_handlers_block_by_func(G_OBJECT(buffer),
					G_CALLBACK(compose_changed_cb),
					compose);
	
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
	cur_pos = gtk_text_iter_get_offset (&iter);
	ins_pos = cur_pos;

	gtk_text_buffer_get_end_iter(buffer, &iter);

	exists = (compose->sig_str != NULL);

	if (replace) {
		GtkTextIter first_iter, start_iter, end_iter;

		gtk_text_buffer_get_start_iter(buffer, &first_iter);

		if (!exists || compose->sig_str[0] == '\0')
			found = FALSE;
		else
			found = gtk_text_iter_forward_to_tag_toggle(&first_iter,
					compose->signature_tag);

		if (found) {
			/* include previous \n\n */
			gtk_text_iter_backward_chars(&first_iter, 1);
			start_iter = first_iter;
			end_iter = first_iter;
			/* skip re-start */
			found = gtk_text_iter_forward_to_tag_toggle(&end_iter,
					compose->signature_tag);
			found &= gtk_text_iter_forward_to_tag_toggle(&end_iter,
					compose->signature_tag);
			if (found) {
				gtk_text_buffer_delete(buffer, &start_iter, &end_iter);
				iter = start_iter;
			}
		} 
	} 

	g_free(compose->sig_str);
	compose->sig_str = account_get_signature_str(compose->account);

	cur_pos = gtk_text_iter_get_offset(&iter);

	if (!compose->sig_str || (replace && !compose->account->auto_sig)) {
		g_free(compose->sig_str);
		compose->sig_str = NULL;
	} else {
		if (compose->sig_inserted == FALSE)
			gtk_text_buffer_insert(buffer, &iter, "\n", -1);
		compose->sig_inserted = TRUE;

		cur_pos = gtk_text_iter_get_offset(&iter);
		gtk_text_buffer_insert(buffer, &iter, compose->sig_str, -1);
		/* remove \n\n */
		gtk_text_buffer_get_iter_at_offset(buffer, &iter, cur_pos);
		gtk_text_iter_forward_chars(&iter, 1);
		gtk_text_buffer_get_end_iter(buffer, &iter_end);
		gtk_text_buffer_apply_tag_by_name(buffer,"signature",&iter, &iter_end);

		if (cur_pos > gtk_text_buffer_get_char_count (buffer))
			cur_pos = gtk_text_buffer_get_char_count (buffer);
	}

	/* put the cursor where it should be 
	 * either where the quote_fmt says, either where it was */
	if (compose->set_cursor_pos < 0)
		gtk_text_buffer_get_iter_at_offset(buffer, &iter, ins_pos);
	else
		gtk_text_buffer_get_iter_at_offset(buffer, &iter, 
			compose->set_cursor_pos);
	
	compose->set_cursor_pos = -1;
	gtk_text_buffer_place_cursor(buffer, &iter);
	g_signal_handlers_unblock_by_func(G_OBJECT(buffer),
					G_CALLBACK(compose_changed_cb),
					compose);
		
	UNBLOCK_WRAP();
}

static ComposeInsertResult compose_insert_file(Compose *compose, const gchar *file)
{
	GtkTextView *text;
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter iter;
	const gchar *cur_encoding;
	gchar buf[BUFFSIZE];
	gint len;
	FILE *fp;
	gboolean prev_autowrap;
	gboolean badtxt = FALSE;
	struct stat file_stat;
	int ret;

	cm_return_val_if_fail(file != NULL, COMPOSE_INSERT_NO_FILE);

	/* get the size of the file we are about to insert */
	ret = g_stat(file, &file_stat);
	if (ret != 0) {
		gchar *shortfile = g_path_get_basename(file);
		alertpanel_error(_("Could not get size of file '%s'."), shortfile);
		g_free(shortfile);
		return COMPOSE_INSERT_NO_FILE;
	} else if (prefs_common.warn_large_insert == TRUE) {

		/* ask user for confirmation if the file is large */
		if (prefs_common.warn_large_insert_size < 0 ||
		    file_stat.st_size > (prefs_common.warn_large_insert_size * 1024)) {
			AlertValue aval;
			gchar *msg;

			msg = g_strdup_printf(_("You are about to insert a file of %s "
						"in the message body. Are you sure you want to do that?"),
						to_human_readable(file_stat.st_size));
			aval = alertpanel_full(_("Are you sure?"), msg, GTK_STOCK_CANCEL,
					_("+_Insert"), NULL, TRUE, NULL, ALERT_QUESTION, G_ALERTDEFAULT);
			g_free(msg);

			/* do we ask for confirmation next time? */
			if (aval & G_ALERTDISABLE) {
				/* no confirmation next time, disable feature in preferences */
				aval &= ~G_ALERTDISABLE;
				prefs_common.warn_large_insert = FALSE;
			}

			/* abort file insertion if user canceled action */
			if (aval != G_ALERTALTERNATE) {
				return COMPOSE_INSERT_NO_FILE;
			}
		}
	}


	if ((fp = g_fopen(file, "rb")) == NULL) {
		FILE_OP_ERROR(file, "fopen");
		return COMPOSE_INSERT_READ_ERROR;
	}

	prev_autowrap = compose->autowrap;
	compose->autowrap = FALSE;

	text = GTK_TEXT_VIEW(compose->text);
	buffer = gtk_text_view_get_buffer(text);
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);

	g_signal_handlers_block_by_func(G_OBJECT(buffer),
					G_CALLBACK(text_inserted),
					compose);

	cur_encoding = conv_get_locale_charset_str_no_utf8();

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		gchar *str;

		if (g_utf8_validate(buf, -1, NULL) == TRUE)
			str = g_strdup(buf);
		else
			str = conv_codeset_strdup
				(buf, cur_encoding, CS_INTERNAL);
		if (!str) continue;

		/* strip <CR> if DOS/Windows file,
		   replace <CR> with <LF> if Macintosh file. */
		strcrchomp(str);
		len = strlen(str);
		if (len > 0 && str[len - 1] != '\n') {
			while (--len >= 0)
				if (str[len] == '\r') str[len] = '\n';
		}

		gtk_text_buffer_insert(buffer, &iter, str, -1);
		g_free(str);
	}

	g_signal_handlers_unblock_by_func(G_OBJECT(buffer),
					  G_CALLBACK(text_inserted),
					  compose);
	compose->autowrap = prev_autowrap;
	if (compose->autowrap)
		compose_wrap_all(compose);

	fclose(fp);

	if (badtxt)
		return COMPOSE_INSERT_INVALID_CHARACTER;
	else 
		return COMPOSE_INSERT_SUCCESS;
}

static gboolean compose_attach_append(Compose *compose, const gchar *file,
				  const gchar *filename,
				  const gchar *content_type,
				  const gchar *charset)
{
	AttachInfo *ainfo;
	GtkTreeIter iter;
	FILE *fp;
	off_t size;
	GAuto *auto_ainfo;
	gchar *size_text;
	GtkListStore *store;
	gchar *name;
	gboolean has_binary = FALSE;

	if (!is_file_exist(file)) {
		gchar *file_from_uri = g_filename_from_uri(file, NULL, NULL);
		gboolean result = FALSE;
		if (file_from_uri && is_file_exist(file_from_uri)) {
			result = compose_attach_append(
						compose, file_from_uri,
						filename, content_type,
						charset);
		}
		g_free(file_from_uri);
		if (result)
			return TRUE;
		alertpanel_error("File %s doesn't exist\n", filename);
		return FALSE;
	}
	if ((size = get_file_size(file)) < 0) {
		alertpanel_error("Can't get file size of %s\n", filename);
		return FALSE;
	}
	if (size == 0) {
		alertpanel_error(_("File %s is empty."), filename);
		return FALSE;
	}
	if ((fp = g_fopen(file, "rb")) == NULL) {
		alertpanel_error(_("Can't read %s."), filename);
		return FALSE;
	}
	fclose(fp);

	ainfo = g_new0(AttachInfo, 1);
	auto_ainfo = g_auto_pointer_new_with_free
			(ainfo, (GFreeFunc) compose_attach_info_free); 
	ainfo->file = g_strdup(file);

	if (content_type) {
		ainfo->content_type = g_strdup(content_type);
		if (!g_ascii_strcasecmp(content_type, "message/rfc822")) {
			MsgInfo *msginfo;
			MsgFlags flags = {0, 0};

			if (procmime_get_encoding_for_text_file(file, &has_binary) == ENC_7BIT)
				ainfo->encoding = ENC_7BIT;
			else
				ainfo->encoding = ENC_8BIT;

			msginfo = procheader_parse_file(file, flags, FALSE, FALSE);
			if (msginfo && msginfo->subject)
				name = g_strdup(msginfo->subject);
			else
				name = g_path_get_basename(filename ? filename : file);

			ainfo->name = g_strdup_printf(_("Message: %s"), name);

			procmsg_msginfo_free(msginfo);
		} else {
			if (!g_ascii_strncasecmp(content_type, "text/", 5)) {
				ainfo->charset = g_strdup(charset);
				ainfo->encoding = procmime_get_encoding_for_text_file(file, &has_binary);
			} else {
				ainfo->encoding = ENC_BASE64;
			}
			name = g_path_get_basename(filename ? filename : file);
			ainfo->name = g_strdup(name);
		}
		g_free(name);
	} else {
		ainfo->content_type = procmime_get_mime_type(file);
		if (!ainfo->content_type) {
			ainfo->content_type =
				g_strdup("application/octet-stream");
			ainfo->encoding = ENC_BASE64;
		} else if (!g_ascii_strncasecmp(ainfo->content_type, "text/", 5))
			ainfo->encoding =
				procmime_get_encoding_for_text_file(file, &has_binary);
		else
			ainfo->encoding = ENC_BASE64;
		name = g_path_get_basename(filename ? filename : file);
		ainfo->name = g_strdup(name);	
		g_free(name);
	}

	if (ainfo->name != NULL
	&&  !strcmp(ainfo->name, ".")) {
		g_free(ainfo->name);
		ainfo->name = NULL;
	}

	if (!strcmp(ainfo->content_type, "unknown") || has_binary) {
		g_free(ainfo->content_type);
		ainfo->content_type = g_strdup("application/octet-stream");
		g_free(ainfo->charset);
		ainfo->charset = NULL;
	}

	ainfo->size = (goffset)size;
	size_text = to_human_readable((goffset)size);

	store = GTK_LIST_STORE(gtk_tree_view_get_model
			(GTK_TREE_VIEW(compose->attach_clist)));
		
	gtk_list_store_append(store, &iter);
	gtk_list_store_set(store, &iter, 
			   COL_MIMETYPE, ainfo->content_type,
			   COL_SIZE, size_text,
			   COL_NAME, ainfo->name,
			   COL_CHARSET, ainfo->charset,
			   COL_DATA, ainfo,
			   COL_AUTODATA, auto_ainfo,
			   -1);
	
	g_auto_pointer_free(auto_ainfo);
	compose_attach_update_label(compose);
	return TRUE;
}

static void compose_use_signing(Compose *compose, gboolean use_signing)
{
	compose->use_signing = use_signing;
	cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/Sign", use_signing);
}

static void compose_use_encryption(Compose *compose, gboolean use_encryption)
{
	compose->use_encryption = use_encryption;
	cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/Encrypt", use_encryption);
}

#define NEXT_PART_NOT_CHILD(info)  \
{  \
	node = info->node;  \
	while (node->children)  \
		node = g_node_last_child(node);  \
	info = procmime_mimeinfo_next((MimeInfo *)node->data);  \
}

static void compose_attach_parts(Compose *compose, MsgInfo *msginfo)
{
	MimeInfo *mimeinfo;
	MimeInfo *child;
	MimeInfo *firsttext = NULL;
	MimeInfo *encrypted = NULL;
	GNode    *node;
	gchar *outfile;
	const gchar *partname = NULL;

	mimeinfo = procmime_scan_message(msginfo);
	if (!mimeinfo) return;

	if (mimeinfo->node->children == NULL) {
		procmime_mimeinfo_free_all(mimeinfo);
		return;
	}

	/* find first content part */
	child = (MimeInfo *) mimeinfo->node->children->data;
	while (child && child->node->children && (child->type == MIMETYPE_MULTIPART))
		child = (MimeInfo *)child->node->children->data;

	if (child) {
		if (child->type == MIMETYPE_TEXT) {
			firsttext = child;
			debug_print("First text part found\n");
		} else if (compose->mode == COMPOSE_REEDIT &&
			 child->type == MIMETYPE_APPLICATION &&
			 !g_ascii_strcasecmp(child->subtype, "pgp-encrypted")) {
			encrypted = (MimeInfo *)child->node->parent->data;
		}
	}
	child = (MimeInfo *) mimeinfo->node->children->data;
	while (child != NULL) {
		gint err;

		if (child == encrypted) {
			/* skip this part of tree */
			NEXT_PART_NOT_CHILD(child);
			continue;
		}

		if (child->type == MIMETYPE_MULTIPART) {
			/* get the actual content */
			child = procmime_mimeinfo_next(child);
			continue;
		}
		    
		if (child == firsttext) {
			child = procmime_mimeinfo_next(child);
			continue;
		}

		outfile = procmime_get_tmp_file_name(child);
		if ((err = procmime_get_part(outfile, child)) < 0)
			g_warning("Can't get the part of multipart message. (%s)", strerror(-err));
		else {
			gchar *content_type;

			content_type = procmime_get_content_type_str(child->type, child->subtype);

			/* if we meet a pgp signature, we don't attach it, but
			 * we force signing. */
			if ((strcmp(content_type, "application/pgp-signature") &&
			    strcmp(content_type, "application/pkcs7-signature") &&
			    strcmp(content_type, "application/x-pkcs7-signature"))
			    || compose->mode == COMPOSE_REDIRECT) {
				partname = procmime_mimeinfo_get_parameter(child, "filename");
				if (partname == NULL)
					partname = procmime_mimeinfo_get_parameter(child, "name");
				if (partname == NULL)
					partname = "";
				compose_attach_append(compose, outfile, 
						      partname, content_type,
						      procmime_mimeinfo_get_parameter(child, "charset"));
			} else {
				compose_force_signing(compose, compose->account, NULL);
			}
			g_free(content_type);
		}
		g_free(outfile);
		NEXT_PART_NOT_CHILD(child);
	}
	procmime_mimeinfo_free_all(mimeinfo);
}

#undef NEXT_PART_NOT_CHILD



typedef enum {
	WAIT_FOR_INDENT_CHAR,
	WAIT_FOR_INDENT_CHAR_OR_SPACE,
} IndentState;

/* return indent length, we allow:
   indent characters followed by indent characters or spaces/tabs,
   alphabets and numbers immediately followed by indent characters,
   and the repeating sequences of the above
   If quote ends with multiple spaces, only the first one is included. */
static gchar *compose_get_quote_str(GtkTextBuffer *buffer,
				    const GtkTextIter *start, gint *len)
{
	GtkTextIter iter = *start;
	gunichar wc;
	gchar ch[6];
	gint clen;
	IndentState state = WAIT_FOR_INDENT_CHAR;
	gboolean is_space;
	gboolean is_indent;
	gint alnum_count = 0;
	gint space_count = 0;
	gint quote_len = 0;

	if (prefs_common.quote_chars == NULL) {
		return 0 ;
	}

	while (!gtk_text_iter_ends_line(&iter)) {
		wc = gtk_text_iter_get_char(&iter);
		if (g_unichar_iswide(wc))
			break;
		clen = g_unichar_to_utf8(wc, ch);
		if (clen != 1)
			break;

		is_indent = strchr(prefs_common.quote_chars, ch[0]) ? TRUE : FALSE;
		is_space = g_unichar_isspace(wc);

		if (state == WAIT_FOR_INDENT_CHAR) {
			if (!is_indent && !g_unichar_isalnum(wc))
				break;
			if (is_indent) {
				quote_len += alnum_count + space_count + 1;
				alnum_count = space_count = 0;
				state = WAIT_FOR_INDENT_CHAR_OR_SPACE;
			} else
				alnum_count++;
		} else if (state == WAIT_FOR_INDENT_CHAR_OR_SPACE) {
			if (!is_indent && !is_space && !g_unichar_isalnum(wc))
				break;
			if (is_space)
				space_count++;
			else if (is_indent) {
				quote_len += alnum_count + space_count + 1;
				alnum_count = space_count = 0;
			} else {
				alnum_count++;
				state = WAIT_FOR_INDENT_CHAR;
			}
		}

		gtk_text_iter_forward_char(&iter);
	}

	if (quote_len > 0 && space_count > 0)
		quote_len++;

	if (len)
		*len = quote_len;

	if (quote_len > 0) {
		iter = *start;
		gtk_text_iter_forward_chars(&iter, quote_len);
		return gtk_text_buffer_get_text(buffer, start, &iter, FALSE);
	}

	return NULL;
}

/* return >0 if the line is itemized */
static int compose_itemized_length(GtkTextBuffer *buffer,
				    const GtkTextIter *start)
{
	GtkTextIter iter = *start;
	gunichar wc;
	gchar ch[6];
	gint clen;
	gint len = 0;
	if (gtk_text_iter_ends_line(&iter))
		return 0;

	while (1) {
		len++;
		wc = gtk_text_iter_get_char(&iter);
		if (!g_unichar_isspace(wc))
			break;
		gtk_text_iter_forward_char(&iter);
		if (gtk_text_iter_ends_line(&iter))
			return 0;
	}

	clen = g_unichar_to_utf8(wc, ch);
	if (clen != 1)
		return 0;

	if (!strchr("*-+", ch[0]))
		return 0;

	gtk_text_iter_forward_char(&iter);
	if (gtk_text_iter_ends_line(&iter))
		return 0;
	wc = gtk_text_iter_get_char(&iter);
	if (g_unichar_isspace(wc)) {
		return len+1;
	}
	return 0;
}

/* return the string at the start of the itemization */
static gchar * compose_get_itemized_chars(GtkTextBuffer *buffer,
				    const GtkTextIter *start)
{
	GtkTextIter iter = *start;
	gunichar wc;
	gint len = 0;
	GString *item_chars = g_string_new("");
	gchar *str = NULL;

	if (gtk_text_iter_ends_line(&iter))
		return NULL;

	while (1) {
		len++;
		wc = gtk_text_iter_get_char(&iter);
		if (!g_unichar_isspace(wc))
			break;
		gtk_text_iter_forward_char(&iter);
		if (gtk_text_iter_ends_line(&iter))
			break;
		g_string_append_unichar(item_chars, wc);
	}

	str = item_chars->str;
	g_string_free(item_chars, FALSE);
	return str;
}

/* return the number of spaces at a line's start */
static int compose_left_offset_length(GtkTextBuffer *buffer,
				    const GtkTextIter *start)
{
	GtkTextIter iter = *start;
	gunichar wc;
	gint len = 0;
	if (gtk_text_iter_ends_line(&iter))
		return 0;

	while (1) {
		wc = gtk_text_iter_get_char(&iter);
		if (!g_unichar_isspace(wc))
			break;
		len++;
		gtk_text_iter_forward_char(&iter);
		if (gtk_text_iter_ends_line(&iter))
			return 0;
	}

	gtk_text_iter_forward_char(&iter);
	if (gtk_text_iter_ends_line(&iter))
		return 0;
	return len;
}

static gboolean compose_get_line_break_pos(GtkTextBuffer *buffer,
					   const GtkTextIter *start,
					   GtkTextIter *break_pos,
					   gint max_col,
					   gint quote_len)
{
	GtkTextIter iter = *start, line_end = *start;
	PangoLogAttr *attrs;
	gchar *str;
	gchar *p;
	gint len;
	gint i;
	gint col = 0;
	gint pos = 0;
	gboolean can_break = FALSE;
	gboolean do_break = FALSE;
	gboolean was_white = FALSE;
	gboolean prev_dont_break = FALSE;

	gtk_text_iter_forward_to_line_end(&line_end);
	str = gtk_text_buffer_get_text(buffer, &iter, &line_end, FALSE);
	len = g_utf8_strlen(str, -1);
	
	if (len == 0) {
		g_free(str);
		g_warning("compose_get_line_break_pos: len = 0!\n");
		return FALSE;
	}

	/* g_print("breaking line: %d: %s (len = %d)\n",
		gtk_text_iter_get_line(&iter), str, len); */

	attrs = g_new(PangoLogAttr, len + 1);

	pango_default_break(str, -1, NULL, attrs, len + 1);

	p = str;

	/* skip quote and leading spaces */
	for (i = 0; *p != '\0' && i < len; i++) {
		gunichar wc;

		wc = g_utf8_get_char(p);
		if (i >= quote_len && !g_unichar_isspace(wc))
			break;
		if (g_unichar_iswide(wc))
			col += 2;
		else if (*p == '\t')
			col += 8;
		else
			col++;
		p = g_utf8_next_char(p);
	}

	for (; *p != '\0' && i < len; i++) {
		PangoLogAttr *attr = attrs + i;
		gunichar wc;
		gint uri_len;

		if (attr->is_line_break && can_break && was_white && !prev_dont_break)
			pos = i;
		
		was_white = attr->is_white;

		/* don't wrap URI */
		if ((uri_len = get_uri_len(p)) > 0) {
			col += uri_len;
			if (pos > 0 && col > max_col) {
				do_break = TRUE;
				break;
			}
			i += uri_len - 1;
			p += uri_len;
			can_break = TRUE;
			continue;
		}

		wc = g_utf8_get_char(p);
		if (g_unichar_iswide(wc)) {
			col += 2;
			if (prev_dont_break && can_break && attr->is_line_break)
				pos = i;
		} else if (*p == '\t')
			col += 8;
		else
			col++;
		if (pos > 0 && col > max_col) {
			do_break = TRUE;
			break;
		}

		if (*p == '-' || *p == '/')
			prev_dont_break = TRUE;
		else
			prev_dont_break = FALSE;

		p = g_utf8_next_char(p);
		can_break = TRUE;
	}

//	debug_print("compose_get_line_break_pos(): do_break = %d, pos = %d, col = %d\n", do_break, pos, col);

	g_free(attrs);
	g_free(str);

	*break_pos = *start;
	gtk_text_iter_set_line_offset(break_pos, pos);

	return do_break;
}

static gboolean compose_join_next_line(Compose *compose,
				       GtkTextBuffer *buffer,
				       GtkTextIter *iter,
				       const gchar *quote_str)
{
	GtkTextIter iter_ = *iter, cur, prev, next, end;
	PangoLogAttr attrs[3];
	gchar *str;
	gchar *next_quote_str;
	gunichar wc1, wc2;
	gint quote_len;
	gboolean keep_cursor = FALSE;

	if (!gtk_text_iter_forward_line(&iter_) ||
	    gtk_text_iter_ends_line(&iter_)) {
		return FALSE;
	}
	next_quote_str = compose_get_quote_str(buffer, &iter_, &quote_len);

	if ((quote_str || next_quote_str) &&
	    strcmp2(quote_str, next_quote_str) != 0) {
		g_free(next_quote_str);
		return FALSE;
	}
	g_free(next_quote_str);

	end = iter_;
	if (quote_len > 0) {
		gtk_text_iter_forward_chars(&end, quote_len);
		if (gtk_text_iter_ends_line(&end)) {
			return FALSE;
		}
	}

	/* don't join itemized lines */
	if (compose_itemized_length(buffer, &end) > 0) {
		return FALSE;
	}

	/* don't join signature separator */
	if (compose_is_sig_separator(compose, buffer, &iter_)) {
		return FALSE;
	}
	/* delete quote str */
	if (quote_len > 0)
		gtk_text_buffer_delete(buffer, &iter_, &end);

	/* don't join line breaks put by the user */
	prev = cur = iter_;
	gtk_text_iter_backward_char(&cur);
	if (gtk_text_iter_has_tag(&cur, compose->no_join_tag)) {
		gtk_text_iter_forward_char(&cur);
		*iter = cur;
		return FALSE;
	}
	gtk_text_iter_forward_char(&cur);
	/* delete linebreak and extra spaces */
	while (gtk_text_iter_backward_char(&cur)) {
		wc1 = gtk_text_iter_get_char(&cur);
		if (!g_unichar_isspace(wc1))
			break;
		prev = cur;
	}
	next = cur = iter_;
	while (!gtk_text_iter_ends_line(&cur)) {
		wc1 = gtk_text_iter_get_char(&cur);
		if (!g_unichar_isspace(wc1))
			break;
		gtk_text_iter_forward_char(&cur);
		next = cur;
	}
	if (!gtk_text_iter_equal(&prev, &next)) {
		GtkTextMark *mark;

		mark = gtk_text_buffer_get_insert(buffer);
		gtk_text_buffer_get_iter_at_mark(buffer, &cur, mark);
		if (gtk_text_iter_equal(&prev, &cur))
			keep_cursor = TRUE;
		gtk_text_buffer_delete(buffer, &prev, &next);
	}
	iter_ = prev;

	/* insert space if required */
	gtk_text_iter_backward_char(&prev);
	wc1 = gtk_text_iter_get_char(&prev);
	wc2 = gtk_text_iter_get_char(&next);
	gtk_text_iter_forward_char(&next);
	str = gtk_text_buffer_get_text(buffer, &prev, &next, FALSE);
	pango_default_break(str, -1, NULL, attrs, 3);
	if (!attrs[1].is_line_break ||
	    (!g_unichar_iswide(wc1) || !g_unichar_iswide(wc2))) {
		gtk_text_buffer_insert(buffer, &iter_, " ", 1);
		if (keep_cursor) {
			gtk_text_iter_backward_char(&iter_);
			gtk_text_buffer_place_cursor(buffer, &iter_);
		}
	}
	g_free(str);

	*iter = iter_;
	return TRUE;
}

#define ADD_TXT_POS(bp_, ep_, pti_) \
	if ((last->next = alloca(sizeof(struct txtpos))) != NULL) { \
		last = last->next; \
		last->bp = (bp_); last->ep = (ep_); last->pti = (pti_); \
		last->next = NULL; \
	} else { \
		g_warning("alloc error scanning URIs\n"); \
	}

static gboolean compose_beautify_paragraph(Compose *compose, GtkTextIter *par_iter, gboolean force)
{
	GtkTextView *text = GTK_TEXT_VIEW(compose->text);
	GtkTextBuffer *buffer;
	GtkTextIter iter, break_pos, end_of_line;
	gchar *quote_str = NULL;
	gint quote_len;
	gboolean wrap_quote = prefs_common.linewrap_quote;
	gboolean prev_autowrap = compose->autowrap;
	gint startq_offset = -1, noq_offset = -1;
	gint uri_start = -1, uri_stop = -1;
	gint nouri_start = -1, nouri_stop = -1;
	gint num_blocks = 0;
	gint quotelevel = -1;
	gboolean modified = force;
	gboolean removed = FALSE;
	gboolean modified_before_remove = FALSE;
	gint lines = 0;
	gboolean start = TRUE;
	gint itemized_len = 0, rem_item_len = 0;
	gchar *itemized_chars = NULL;
	gboolean item_continuation = FALSE;

	if (force) {
		modified = TRUE;
	}
	if (compose->draft_timeout_tag == -2) {
		modified = TRUE;
	}

	compose->autowrap = FALSE;

	buffer = gtk_text_view_get_buffer(text);
	undo_wrapping(compose->undostruct, TRUE);
	if (par_iter) {
		iter = *par_iter;
	} else {
		GtkTextMark *mark;
		mark = gtk_text_buffer_get_insert(buffer);
		gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
	}


	if (compose->draft_timeout_tag == -2) {
		if (gtk_text_iter_ends_line(&iter)) {
			while (gtk_text_iter_ends_line(&iter) &&
			       gtk_text_iter_forward_line(&iter))
				;
		} else {
			while (gtk_text_iter_backward_line(&iter)) {
				if (gtk_text_iter_ends_line(&iter)) {
					gtk_text_iter_forward_line(&iter);
					break;
				}
			}
		}
	} else {
		/* move to line start */
		gtk_text_iter_set_line_offset(&iter, 0);
	}
	
	itemized_len = compose_itemized_length(buffer, &iter);
	
	if (!itemized_len) {
		itemized_len = compose_left_offset_length(buffer, &iter);
		item_continuation = TRUE;
	}

	if (itemized_len)
		itemized_chars = compose_get_itemized_chars(buffer, &iter);

	/* go until paragraph end (empty line) */
	while (start || !gtk_text_iter_ends_line(&iter)) {
		gchar *scanpos = NULL;
		/* parse table - in order of priority */
		struct table {
			const gchar *needle; /* token */

			/* token search function */
			gchar    *(*search)	(const gchar *haystack,
						 const gchar *needle);
			/* part parsing function */
			gboolean  (*parse)	(const gchar *start,
						 const gchar *scanpos,
						 const gchar **bp_,
						 const gchar **ep_,
						 gboolean hdr);
			/* part to URI function */
			gchar    *(*build_uri)	(const gchar *bp,
						 const gchar *ep);
		};

		static struct table parser[] = {
			{"http://",  strcasestr, get_uri_part,   make_uri_string},
			{"https://", strcasestr, get_uri_part,   make_uri_string},
			{"ftp://",   strcasestr, get_uri_part,   make_uri_string},
			{"sftp://",  strcasestr, get_uri_part,   make_uri_string},
			{"gopher://",strcasestr, get_uri_part,   make_uri_string},
			{"www.",     strcasestr, get_uri_part,   make_http_string},
			{"mailto:",  strcasestr, get_uri_part,   make_uri_string},
			{"@",        strcasestr, get_email_part, make_email_string}
		};
		const gint PARSE_ELEMS = sizeof parser / sizeof parser[0];
		gint last_index = PARSE_ELEMS;
		gint  n;
		gchar *o_walk = NULL, *walk = NULL, *bp = NULL, *ep = NULL;
		gint walk_pos;
		
		start = FALSE;
		if (!prev_autowrap && num_blocks == 0) {
			num_blocks++;
			g_signal_handlers_block_by_func(G_OBJECT(buffer),
					G_CALLBACK(text_inserted),
					compose);
		}
		if (gtk_text_iter_has_tag(&iter, compose->no_wrap_tag) && !force)
			goto colorize;

		uri_start = uri_stop = -1;
		quote_len = 0;
		quote_str = compose_get_quote_str(buffer, &iter, &quote_len);

		if (quote_str) {
//			debug_print("compose_beautify_paragraph(): quote_str = '%s'\n", quote_str);
			if (startq_offset == -1) 
				startq_offset = gtk_text_iter_get_offset(&iter);
			quotelevel = get_quote_level(quote_str, prefs_common.quote_chars);
			if (quotelevel > 2) {
				/* recycle colors */
				if (prefs_common.recycle_quote_colors)
					quotelevel %= 3;
				else
					quotelevel = 2;
			}
			if (!wrap_quote) {
				goto colorize;
			}
		} else {
			if (startq_offset == -1)
				noq_offset = gtk_text_iter_get_offset(&iter);
			quotelevel = -1;
		}

		if (prev_autowrap == FALSE && !force && !wrap_quote) {
			goto colorize;
		}
		if (gtk_text_iter_ends_line(&iter)) {
			goto colorize;
		} else if (compose_get_line_break_pos(buffer, &iter, &break_pos,
					       prefs_common.linewrap_len,
					       quote_len)) {
			GtkTextIter prev, next, cur;
			if (prev_autowrap != FALSE || force) {
				compose->automatic_break = TRUE;
				modified = TRUE;
				gtk_text_buffer_insert(buffer, &break_pos, "\n", 1);
				compose->automatic_break = FALSE;
				if (itemized_len && compose->autoindent) {
					gtk_text_buffer_insert(buffer, &break_pos, itemized_chars, -1);
					if (!item_continuation)
						gtk_text_buffer_insert(buffer, &break_pos, "  ", 2);
				}
			} else if (quote_str && wrap_quote) {
				compose->automatic_break = TRUE;
				modified = TRUE;
				gtk_text_buffer_insert(buffer, &break_pos, "\n", 1);
				compose->automatic_break = FALSE;
				if (itemized_len && compose->autoindent) {
					gtk_text_buffer_insert(buffer, &break_pos, itemized_chars, -1);
					if (!item_continuation)
						gtk_text_buffer_insert(buffer, &break_pos, "  ", 2);
				}
			} else 
				goto colorize;
			/* remove trailing spaces */
			cur = break_pos;
			rem_item_len = itemized_len;
			while (compose->autoindent && rem_item_len-- > 0)
				gtk_text_iter_backward_char(&cur);
			gtk_text_iter_backward_char(&cur);

			prev = next = cur;
			while (!gtk_text_iter_starts_line(&cur)) {
				gunichar wc;

				gtk_text_iter_backward_char(&cur);
				wc = gtk_text_iter_get_char(&cur);
				if (!g_unichar_isspace(wc))
					break;
				prev = cur;
			}
			if (!gtk_text_iter_equal(&prev, &next)) {
				gtk_text_buffer_delete(buffer, &prev, &next);
				break_pos = next;
				gtk_text_iter_forward_char(&break_pos);
			}

			if (quote_str)
				gtk_text_buffer_insert(buffer, &break_pos,
						       quote_str, -1);

			iter = break_pos;
			modified |= compose_join_next_line(compose, buffer, &iter, quote_str);

			/* move iter to current line start */
			gtk_text_iter_set_line_offset(&iter, 0);
			if (quote_str) {
				g_free(quote_str);
				quote_str = NULL;
			}
			continue;	
		} else {
			/* move iter to next line start */
			iter = break_pos;
			lines++;
		}

colorize:
		if (!prev_autowrap && num_blocks > 0) {
			num_blocks--;
			g_signal_handlers_unblock_by_func(G_OBJECT(buffer),
					G_CALLBACK(text_inserted),
					compose);
		}
		end_of_line = iter;
		while (!gtk_text_iter_ends_line(&end_of_line)) {
			gtk_text_iter_forward_char(&end_of_line);
		}
		o_walk = walk = gtk_text_buffer_get_text(buffer, &iter, &end_of_line, FALSE);

		nouri_start = gtk_text_iter_get_offset(&iter);
		nouri_stop = gtk_text_iter_get_offset(&end_of_line);

		walk_pos = gtk_text_iter_get_offset(&iter);
		/* FIXME: this looks phony. scanning for anything in the parse table */
		for (n = 0; n < PARSE_ELEMS; n++) {
			gchar *tmp;

			tmp = parser[n].search(walk, parser[n].needle);
			if (tmp) {
				if (scanpos == NULL || tmp < scanpos) {
					scanpos = tmp;
					last_index = n;
				}
			}					
		}

		bp = ep = 0;
		if (scanpos) {
			/* check if URI can be parsed */
			if (parser[last_index].parse(walk, scanpos, (const gchar **)&bp,
					(const gchar **)&ep, FALSE)
			    && (size_t) (ep - bp - 1) > strlen(parser[last_index].needle)) {
					walk = ep;
			} else
				walk = scanpos +
					strlen(parser[last_index].needle);
		} 
		if (bp && ep) {
			uri_start = walk_pos + (bp - o_walk);
			uri_stop  = walk_pos + (ep - o_walk);
		}
		g_free(o_walk);
		o_walk = NULL;
		gtk_text_iter_forward_line(&iter);
		g_free(quote_str);
		quote_str = NULL;
		if (startq_offset != -1) {
			GtkTextIter startquote, endquote;
			gtk_text_buffer_get_iter_at_offset(
				buffer, &startquote, startq_offset);
			endquote = iter;

			switch (quotelevel) {
			case 0:	
				if (!gtk_text_iter_has_tag(&startquote, compose->quote0_tag) ||
				    !gtk_text_iter_has_tag(&end_of_line, compose->quote0_tag)) {
					gtk_text_buffer_apply_tag_by_name(
						buffer, "quote0", &startquote, &endquote);
					gtk_text_buffer_remove_tag_by_name(
						buffer, "quote1", &startquote, &endquote);
					gtk_text_buffer_remove_tag_by_name(
						buffer, "quote2", &startquote, &endquote);
					modified = TRUE;
				}
				break;
			case 1:	
				if (!gtk_text_iter_has_tag(&startquote, compose->quote1_tag) ||
				    !gtk_text_iter_has_tag(&end_of_line, compose->quote1_tag)) {
					gtk_text_buffer_apply_tag_by_name(
						buffer, "quote1", &startquote, &endquote);
					gtk_text_buffer_remove_tag_by_name(
						buffer, "quote0", &startquote, &endquote);
					gtk_text_buffer_remove_tag_by_name(
						buffer, "quote2", &startquote, &endquote);
					modified = TRUE;
				}
				break;
			case 2:	
				if (!gtk_text_iter_has_tag(&startquote, compose->quote2_tag) ||
				    !gtk_text_iter_has_tag(&end_of_line, compose->quote2_tag)) {
					gtk_text_buffer_apply_tag_by_name(
						buffer, "quote2", &startquote, &endquote);
					gtk_text_buffer_remove_tag_by_name(
						buffer, "quote0", &startquote, &endquote);
					gtk_text_buffer_remove_tag_by_name(
						buffer, "quote1", &startquote, &endquote);
					modified = TRUE;
				}
				break;
			}
			startq_offset = -1;
		} else if (noq_offset != -1) {
			GtkTextIter startnoquote, endnoquote;
			gtk_text_buffer_get_iter_at_offset(
				buffer, &startnoquote, noq_offset);
			endnoquote = iter;

			if ((gtk_text_iter_has_tag(&startnoquote, compose->quote0_tag)
			  && gtk_text_iter_has_tag(&end_of_line, compose->quote0_tag)) ||
			    (gtk_text_iter_has_tag(&startnoquote, compose->quote1_tag)
			  && gtk_text_iter_has_tag(&end_of_line, compose->quote1_tag)) ||
			    (gtk_text_iter_has_tag(&startnoquote, compose->quote2_tag)
			  && gtk_text_iter_has_tag(&end_of_line, compose->quote2_tag))) {
				gtk_text_buffer_remove_tag_by_name(
					buffer, "quote0", &startnoquote, &endnoquote);
				gtk_text_buffer_remove_tag_by_name(
					buffer, "quote1", &startnoquote, &endnoquote);
				gtk_text_buffer_remove_tag_by_name(
					buffer, "quote2", &startnoquote, &endnoquote);
				modified = TRUE;
			}
			noq_offset = -1;
		}
		
		if (uri_start != nouri_start && uri_stop != nouri_stop) {
			GtkTextIter nouri_start_iter, nouri_end_iter;
			gtk_text_buffer_get_iter_at_offset(
				buffer, &nouri_start_iter, nouri_start);
			gtk_text_buffer_get_iter_at_offset(
				buffer, &nouri_end_iter, nouri_stop);
			if (gtk_text_iter_has_tag(&nouri_start_iter, compose->uri_tag) &&
			    gtk_text_iter_has_tag(&nouri_end_iter, compose->uri_tag)) {
				gtk_text_buffer_remove_tag_by_name(
					buffer, "link", &nouri_start_iter, &nouri_end_iter);
				modified_before_remove = modified;
				modified = TRUE;
				removed = TRUE;
			}
		}
		if (uri_start >= 0 && uri_stop > 0) {
			GtkTextIter uri_start_iter, uri_end_iter, back;
			gtk_text_buffer_get_iter_at_offset(
				buffer, &uri_start_iter, uri_start);
			gtk_text_buffer_get_iter_at_offset(
				buffer, &uri_end_iter, uri_stop);
			back = uri_end_iter;
			gtk_text_iter_backward_char(&back);
			if (!gtk_text_iter_has_tag(&uri_start_iter, compose->uri_tag) ||
			    !gtk_text_iter_has_tag(&back, compose->uri_tag)) {
				gtk_text_buffer_apply_tag_by_name(
					buffer, "link", &uri_start_iter, &uri_end_iter);
				modified = TRUE;
				if (removed && !modified_before_remove) {
					modified = FALSE;
				} 
			}
		}
		if (!modified) {
//			debug_print("not modified, out after %d lines\n", lines);
			goto end;
		}
	}
//	debug_print("modified, out after %d lines\n", lines);
end:
	g_free(itemized_chars);
	if (par_iter)
		*par_iter = iter;
	undo_wrapping(compose->undostruct, FALSE);
	compose->autowrap = prev_autowrap;
	
	return modified;
}

void compose_action_cb(void *data)
{
	Compose *compose = (Compose *)data;
	compose_wrap_all(compose);
}

static void compose_wrap_all(Compose *compose)
{
	compose_wrap_all_full(compose, FALSE);
}

static void compose_wrap_all_full(Compose *compose, gboolean force)
{
	GtkTextView *text = GTK_TEXT_VIEW(compose->text);
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	gboolean modified = TRUE;

	buffer = gtk_text_view_get_buffer(text);

	gtk_text_buffer_get_start_iter(buffer, &iter);
	while (!gtk_text_iter_is_end(&iter) && modified)
		modified = compose_beautify_paragraph(compose, &iter, force);

}

static void compose_set_title(Compose *compose)
{
	gchar *str;
	gchar *edited;
	gchar *subject;
	
	edited = compose->modified ? _(" [Edited]") : "";
	
	subject = gtk_editable_get_chars(
			GTK_EDITABLE(compose->subject_entry), 0, -1);

#ifndef GENERIC_UMPC
	if (subject && strlen(subject))
		str = g_strdup_printf(_("%s - Compose message%s"),
				      subject, edited);	
	else
		str = g_strdup_printf(_("[no subject] - Compose message%s"), edited);
#else
	str = g_strdup(_("Compose message"));
#endif

	gtk_window_set_title(GTK_WINDOW(compose->window), str);
	g_free(str);
	g_free(subject);
}

/**
 * compose_current_mail_account:
 * 
 * Find a current mail account (the currently selected account, or the
 * default account, if a news account is currently selected).  If a
 * mail account cannot be found, display an error message.
 * 
 * Return value: Mail account, or NULL if not found.
 **/
static PrefsAccount *
compose_current_mail_account(void)
{
	PrefsAccount *ac;

	if (cur_account && cur_account->protocol != A_NNTP)
		ac = cur_account;
	else {
		ac = account_get_default();
		if (!ac || ac->protocol == A_NNTP) {
			alertpanel_error(_("Account for sending mail is not specified.\n"
					   "Please select a mail account before sending."));
			return NULL;
		}
	}
	return ac;
}

#define QUOTE_IF_REQUIRED(out, str)					\
{									\
	if (*str != '"' && strpbrk(str, ",.[]<>")) {			\
		gchar *__tmp;						\
		gint len;						\
									\
		len = strlen(str) + 3;					\
		if ((__tmp = alloca(len)) == NULL) {			\
			g_warning("can't allocate memory\n");		\
			g_string_free(header, TRUE);			\
			return NULL;					\
		}							\
		g_snprintf(__tmp, len, "\"%s\"", str);			\
		out = __tmp;						\
	} else {							\
		gchar *__tmp;						\
									\
		if ((__tmp = alloca(strlen(str) + 1)) == NULL) {	\
			g_warning("can't allocate memory\n");		\
			g_string_free(header, TRUE);			\
			return NULL;					\
		} else 							\
			strcpy(__tmp, str);				\
									\
		out = __tmp;						\
	}								\
}

#define QUOTE_IF_REQUIRED_NORMAL(out, str, errret)			\
{									\
	if (*str != '"' && strpbrk(str, ",.[]<>")) {			\
		gchar *__tmp;						\
		gint len;						\
									\
		len = strlen(str) + 3;					\
		if ((__tmp = alloca(len)) == NULL) {			\
			g_warning("can't allocate memory\n");		\
			errret;						\
		}							\
		g_snprintf(__tmp, len, "\"%s\"", str);			\
		out = __tmp;						\
	} else {							\
		gchar *__tmp;						\
									\
		if ((__tmp = alloca(strlen(str) + 1)) == NULL) {	\
			g_warning("can't allocate memory\n");		\
			errret;						\
		} else 							\
			strcpy(__tmp, str);				\
									\
		out = __tmp;						\
	}								\
}

static void compose_select_account(Compose *compose, PrefsAccount *account,
				   gboolean init)
{
	gchar *from = NULL, *header;
	ComposeHeaderEntry *header_entry;

	cm_return_if_fail(account != NULL);

	compose->account = account;
	if (account->name && *account->name) {
		gchar *buf;
		QUOTE_IF_REQUIRED_NORMAL(buf, account->name, return);
		from = g_strdup_printf("%s <%s>",
				       buf, account->address);
		gtk_entry_set_text(GTK_ENTRY(compose->from_name), from);
	} else {
		from = g_strdup_printf("<%s>",
				       account->address);
		gtk_entry_set_text(GTK_ENTRY(compose->from_name), from);
	}

	g_free(from);

	compose_set_title(compose);

	if (account->default_sign && compose->mode != COMPOSE_REDIRECT)
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/Sign", TRUE);
	else
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/Sign", FALSE);
	if (account->default_encrypt && compose->mode != COMPOSE_REDIRECT)
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/Encrypt", TRUE);
	else
		cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Options/Encrypt", FALSE);
				       
	activate_privacy_system(compose, account, FALSE);

	if (!init && compose->mode != COMPOSE_REDIRECT) {
		undo_block(compose->undostruct);
		compose_insert_sig(compose, TRUE);
		undo_unblock(compose->undostruct);
	}
	
	header_entry = (ComposeHeaderEntry *) compose->header_list->data;
	header = gtk_combo_box_get_active_text(GTK_COMBO_BOX(header_entry->combo));
	
	if (header && !strlen(gtk_entry_get_text(GTK_ENTRY(header_entry->entry)))) {
		if (account->protocol == A_NNTP) {
			if (!strcmp(header, _("To:")))
				combobox_select_by_text(
					GTK_COMBO_BOX(header_entry->combo),
					_("Newsgroups:"));
		} else {
			if (!strcmp(header, _("Newsgroups:")))
				combobox_select_by_text(
					GTK_COMBO_BOX(header_entry->combo),
					_("To:"));
		}
		
	}
	g_free(header);
	
#ifdef USE_ENCHANT
	/* use account's dict info if set */
	if (compose->gtkaspell) {
		if (account->enable_default_dictionary)
			gtkaspell_change_dict(compose->gtkaspell,
					account->default_dictionary, FALSE);
		if (account->enable_default_alt_dictionary)
			gtkaspell_change_alt_dict(compose->gtkaspell,
					account->default_alt_dictionary);
		if (account->enable_default_dictionary
			|| account->enable_default_alt_dictionary)
			compose_spell_menu_changed(compose);
	}
#endif
}

gboolean compose_check_for_valid_recipient(Compose *compose) {
	gchar *recipient_headers_mail[] = {"To:", "Cc:", "Bcc:", NULL};
	gchar *recipient_headers_news[] = {"Newsgroups:", NULL};
	gboolean recipient_found = FALSE;
	GSList *list;
	gchar **strptr;

	/* free to and newsgroup list */
        slist_free_strings(compose->to_list);
	g_slist_free(compose->to_list);
	compose->to_list = NULL;
			
	slist_free_strings(compose->newsgroup_list);
        g_slist_free(compose->newsgroup_list);
        compose->newsgroup_list = NULL;

	/* search header entries for to and newsgroup entries */
	for (list = compose->header_list; list; list = list->next) {
		gchar *header;
		gchar *entry;
		header = gtk_editable_get_chars(GTK_EDITABLE(gtk_bin_get_child(GTK_BIN((((ComposeHeaderEntry *)list->data)->combo)))), 0, -1);
		entry = gtk_editable_get_chars(GTK_EDITABLE(((ComposeHeaderEntry *)list->data)->entry), 0, -1);
		g_strstrip(entry);
		g_strstrip(header);
		if (entry[0] != '\0') {
			for (strptr = recipient_headers_mail; *strptr != NULL; strptr++) {
				if (!g_ascii_strcasecmp(header, prefs_common_translated_header_name(*strptr))) {
					compose->to_list = address_list_append(compose->to_list, entry);
					recipient_found = TRUE;
				}
			}
			for (strptr = recipient_headers_news; *strptr != NULL; strptr++) {
				if (!g_ascii_strcasecmp(header, prefs_common_translated_header_name(*strptr))) {
					compose->newsgroup_list = newsgroup_list_append(compose->newsgroup_list, entry);
					recipient_found = TRUE;
				}
			}
		}
		g_free(header);
		g_free(entry);
	}
	return recipient_found;
}

static gboolean compose_check_for_set_recipients(Compose *compose)
{
	if (compose->account->set_autocc && compose->account->auto_cc) {
		gboolean found_other = FALSE;
		GSList *list;
		/* search header entries for to and newsgroup entries */
		for (list = compose->header_list; list; list = list->next) {
			gchar *entry;
			gchar *header;
			entry = gtk_editable_get_chars(GTK_EDITABLE(((ComposeHeaderEntry *)list->data)->entry), 0, -1);
			header = gtk_editable_get_chars(GTK_EDITABLE(gtk_bin_get_child(GTK_BIN((((ComposeHeaderEntry *)list->data)->combo)))), 0, -1);
			g_strstrip(entry);
			g_strstrip(header);
			if (strcmp(entry, compose->account->auto_cc)
			||  strcmp(header, prefs_common_translated_header_name("Cc:"))) {
				found_other = TRUE;
				g_free(entry);
				break;
			}
			g_free(entry);
			g_free(header);
		}
		if (!found_other) {
			AlertValue aval;
			if (compose->batch) {
				gtk_widget_show_all(compose->window);
			}
			aval = alertpanel(_("Send"),
					  _("The only recipient is the default CC address. Send anyway?"),
					  GTK_STOCK_CANCEL, _("+_Send"), NULL);
			if (aval != G_ALERTALTERNATE)
				return FALSE;
		}
	}
	if (compose->account->set_autobcc && compose->account->auto_bcc) {
		gboolean found_other = FALSE;
		GSList *list;
		/* search header entries for to and newsgroup entries */
		for (list = compose->header_list; list; list = list->next) {
			gchar *entry;
			gchar *header;
			entry = gtk_editable_get_chars(GTK_EDITABLE(((ComposeHeaderEntry *)list->data)->entry), 0, -1);
			header = gtk_editable_get_chars(GTK_EDITABLE(gtk_bin_get_child(GTK_BIN((((ComposeHeaderEntry *)list->data)->combo)))), 0, -1);
			g_strstrip(entry);
			g_strstrip(header);
			if (strcmp(entry, compose->account->auto_bcc)
			||  strcmp(header, prefs_common_translated_header_name("Bcc:"))) {
				found_other = TRUE;
				g_free(entry);
				break;
			}
			g_free(entry);
			g_free(header);
		}
		if (!found_other) {
			AlertValue aval;
			if (compose->batch) {
				gtk_widget_show_all(compose->window);
			}
			aval = alertpanel(_("Send"),
					  _("The only recipient is the default BCC address. Send anyway?"),
					  GTK_STOCK_CANCEL, _("+_Send"), NULL);
			if (aval != G_ALERTALTERNATE)
				return FALSE;
		}
	}
	return TRUE;
}

static gboolean compose_check_entries(Compose *compose, gboolean check_everything)
{
	const gchar *str;

	if (compose_check_for_valid_recipient(compose) == FALSE) {
		if (compose->batch) {
			gtk_widget_show_all(compose->window);
		}
		alertpanel_error(_("Recipient is not specified."));
		return FALSE;
	}

	if (compose_check_for_set_recipients(compose) == FALSE) {
		return FALSE;
	}

	if (!compose->batch) {
		str = gtk_entry_get_text(GTK_ENTRY(compose->subject_entry));
		if (*str == '\0' && check_everything == TRUE && 
		    compose->mode != COMPOSE_REDIRECT) {
			AlertValue aval;
			gchar *button_label;
			gchar *message;

			if (compose->sending)
				button_label = _("+_Send");
			else
				button_label = _("+_Queue");
			message = g_strdup_printf(_("Subject is empty. %s"),
					compose->sending?_("Send it anyway?"):
					_("Queue it anyway?"));

			aval = alertpanel(compose->sending?_("Send"):_("Send later"), message,
					  GTK_STOCK_CANCEL, button_label, NULL);
			g_free(message);
			if (aval != G_ALERTALTERNATE)
				return FALSE;
		}
	}

	if (check_everything && hooks_invoke(COMPOSE_CHECK_BEFORE_SEND_HOOKLIST, compose))
		return FALSE;

	return TRUE;
}

gint compose_send(Compose *compose)
{
	gint msgnum;
	FolderItem *folder = NULL;
	gint val = -1;
	gchar *msgpath = NULL;
	gboolean discard_window = FALSE;
	gchar *errstr = NULL;
	gchar *tmsgid = NULL;
	MainWindow *mainwin = mainwindow_get_mainwindow();
	gboolean queued_removed = FALSE;

	if (prefs_common.send_dialog_invisible
			|| compose->batch == TRUE)
		discard_window = TRUE;

	compose_allow_user_actions (compose, FALSE);
	compose->sending = TRUE;

	if (compose_check_entries(compose, TRUE) == FALSE) {
		if (compose->batch) {
			gtk_widget_show_all(compose->window);
		}
		goto bail;
	}

	inc_lock();
	val = compose_queue(compose, &msgnum, &folder, &msgpath, TRUE);

	if (val) {
		if (compose->batch) {
			gtk_widget_show_all(compose->window);
		}
		if (val == -4) {
			alertpanel_error(_("Could not queue message for sending:\n\n"
					   "Charset conversion failed."));
		} else if (val == -5) {
			alertpanel_error(_("Could not queue message for sending:\n\n"
					   "Couldn't get recipient encryption key."));
		} else if (val == -6) {
			/* silent error */
		} else if (val == -3) {
			if (privacy_peek_error())
			alertpanel_error(_("Could not queue message for sending:\n\n"
					   "Signature failed: %s"), privacy_get_error());
		} else if (val == -2 && errno != 0) {
			alertpanel_error(_("Could not queue message for sending:\n\n%s."), strerror(errno));
		} else {
			alertpanel_error(_("Could not queue message for sending."));
		}
		goto bail;
	}

	tmsgid = compose->msgid ? g_strdup(compose->msgid) : NULL;
	if (discard_window) {
		compose->sending = FALSE;
		compose_close(compose);
		/* No more compose access in the normal codepath 
		 * after this point! */
		compose = NULL;
	}

	if (msgnum == 0) {
		alertpanel_error(_("The message was queued but could not be "
				   "sent.\nUse \"Send queued messages\" from "
				   "the main window to retry."));
		if (!discard_window) {
			goto bail;
		}
		inc_unlock();
		g_free(tmsgid);
		return -1;
	}
	if (msgpath == NULL) {
		msgpath = folder_item_fetch_msg(folder, msgnum);
		val = procmsg_send_message_queue(msgpath, &errstr, folder, msgnum, &queued_removed);
		g_free(msgpath);
	} else {
		val = procmsg_send_message_queue(msgpath, &errstr, folder, msgnum, &queued_removed);
		claws_unlink(msgpath);
		g_free(msgpath);
	}
	if (!discard_window) {
		if (val != 0) {
			if (!queued_removed)
				folder_item_remove_msg(folder, msgnum);
			folder_item_scan(folder);
			if (tmsgid) {
				/* make sure we delete that */
				MsgInfo *tmp = folder_item_get_msginfo_by_msgid(folder, tmsgid);
				if (tmp) {
					debug_print("removing %d via %s\n", tmp->msgnum, tmsgid);
					folder_item_remove_msg(folder, tmp->msgnum);
					procmsg_msginfo_free(tmp);
				} 
			}
		}
	}

	if (val == 0) {
		if (!queued_removed)
			folder_item_remove_msg(folder, msgnum);
		folder_item_scan(folder);
		if (tmsgid) {
			/* make sure we delete that */
			MsgInfo *tmp = folder_item_get_msginfo_by_msgid(folder, tmsgid);
			if (tmp) {
				debug_print("removing %d via %s\n", tmp->msgnum, tmsgid);
				folder_item_remove_msg(folder, tmp->msgnum);
				procmsg_msginfo_free(tmp);
			}
		}
		if (!discard_window) {
			compose->sending = FALSE;
			compose_allow_user_actions (compose, TRUE);
			compose_close(compose);
		}
	} else {
		if (errstr) {
			alertpanel_error_log(_("%s\nUse \"Send queued messages\" from "
				   "the main window to retry."), errstr);
			g_free(errstr);
		} else {
			alertpanel_error_log(_("The message was queued but could not be "
				   "sent.\nUse \"Send queued messages\" from "
				   "the main window to retry."));
		}
		if (!discard_window) {
			goto bail;		
		}
		inc_unlock();
		g_free(tmsgid);
		return -1;
 	}
	g_free(tmsgid);
	inc_unlock();
	toolbar_main_set_sensitive(mainwin);
	main_window_set_menu_sensitive(mainwin);
	return 0;

bail:
	inc_unlock();
	g_free(tmsgid);
	compose_allow_user_actions (compose, TRUE);
	compose->sending = FALSE;
	compose->modified = TRUE; 
	toolbar_main_set_sensitive(mainwin);
	main_window_set_menu_sensitive(mainwin);

	return -1;
}

static gboolean compose_use_attach(Compose *compose) 
{
	GtkTreeModel *model = gtk_tree_view_get_model
				(GTK_TREE_VIEW(compose->attach_clist));
	return gtk_tree_model_iter_n_children(model, NULL) > 0;
}

static gint compose_redirect_write_headers_from_headerlist(Compose *compose, 
							   FILE *fp)
{
	gchar buf[BUFFSIZE];
	gchar *str;
	gboolean first_to_address;
	gboolean first_cc_address;
	GSList *list;
	ComposeHeaderEntry *headerentry;
	const gchar *headerentryname;
	const gchar *cc_hdr;
	const gchar *to_hdr;
	gboolean err = FALSE;

	debug_print("Writing redirect header\n");

	cc_hdr = prefs_common_translated_header_name("Cc:");
 	to_hdr = prefs_common_translated_header_name("To:");

	first_to_address = TRUE;
	for (list = compose->header_list; list; list = list->next) {
		headerentry = ((ComposeHeaderEntry *)list->data);
		headerentryname = gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((headerentry->combo)))));

		if (g_utf8_collate(headerentryname, to_hdr) == 0) {
			const gchar *entstr = gtk_entry_get_text(GTK_ENTRY(headerentry->entry));
			Xstrdup_a(str, entstr, return -1);
			g_strstrip(str);
			if (str[0] != '\0') {
				compose_convert_header
					(compose, buf, sizeof(buf), str,
					strlen("Resent-To") + 2, TRUE);

				if (first_to_address) {
					err |= (fprintf(fp, "Resent-To: ") < 0);
					first_to_address = FALSE;
				} else {
					err |= (fprintf(fp, ",") < 0);
                                }
				err |= (fprintf(fp, "%s", buf) < 0);
			}
		}
	}
	if (!first_to_address) {
		err |= (fprintf(fp, "\n") < 0);
	}

	first_cc_address = TRUE;
	for (list = compose->header_list; list; list = list->next) {
		headerentry = ((ComposeHeaderEntry *)list->data);
		headerentryname = gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((headerentry->combo)))));

		if (g_utf8_collate(headerentryname, cc_hdr) == 0) {
			const gchar *strg = gtk_entry_get_text(GTK_ENTRY(headerentry->entry));
			Xstrdup_a(str, strg, return -1);
			g_strstrip(str);
			if (str[0] != '\0') {
				compose_convert_header
					(compose, buf, sizeof(buf), str,
					strlen("Resent-Cc") + 2, TRUE);

                                if (first_cc_address) {
                                        err |= (fprintf(fp, "Resent-Cc: ") < 0);
                                        first_cc_address = FALSE;
                                } else {
                                        err |= (fprintf(fp, ",") < 0);
                                }
				err |= (fprintf(fp, "%s", buf) < 0);
			}
		}
	}
	if (!first_cc_address) {
		err |= (fprintf(fp, "\n") < 0);
        }
	
	return (err ? -1:0);
}

static gint compose_redirect_write_headers(Compose *compose, FILE *fp)
{
	gchar buf[BUFFSIZE];
	gchar *str;
	const gchar *entstr;
	/* struct utsname utsbuf; */
	gboolean err = FALSE;

	cm_return_val_if_fail(fp != NULL, -1);
	cm_return_val_if_fail(compose->account != NULL, -1);
	cm_return_val_if_fail(compose->account->address != NULL, -1);

	/* Resent-Date */
	get_rfc822_date(buf, sizeof(buf));
	err |= (fprintf(fp, "Resent-Date: %s\n", buf) < 0);

	/* Resent-From */
	if (compose->account->name && *compose->account->name) {
		compose_convert_header
			(compose, buf, sizeof(buf), compose->account->name,
			 strlen("From: "), TRUE);
		err |= (fprintf(fp, "Resent-From: %s <%s>\n",
			buf, compose->account->address) < 0);
	} else
		err |= (fprintf(fp, "Resent-From: %s\n", compose->account->address) < 0);

	/* Subject */
	entstr = gtk_entry_get_text(GTK_ENTRY(compose->subject_entry));
	if (*entstr != '\0') {
		Xstrdup_a(str, entstr, return -1);
		g_strstrip(str);
		if (*str != '\0') {
			compose_convert_header(compose, buf, sizeof(buf), str,
					       strlen("Subject: "), FALSE);
			err |= (fprintf(fp, "Subject: %s\n", buf) < 0);
		}
	}

	/* Resent-Message-ID */
	if (compose->account->set_domain && compose->account->domain) {
		g_snprintf(buf, sizeof(buf), "%s", compose->account->domain); 
	} else if (!strncmp(get_domain_name(), "localhost", strlen("localhost"))) {
		g_snprintf(buf, sizeof(buf), "%s", 
			strchr(compose->account->address, '@') ?
				strchr(compose->account->address, '@')+1 :
				compose->account->address);
	} else {
		g_snprintf(buf, sizeof(buf), "%s", "");
	}

	if (compose->account->gen_msgid) {
		gchar *addr = NULL;
		if (compose->account->msgid_with_addr) {
			addr = compose->account->address;
		}
		generate_msgid(buf, sizeof(buf), addr);
		err |= (fprintf(fp, "Resent-Message-ID: <%s>\n", buf) < 0);
		compose->msgid = g_strdup(buf);
	} else {
		compose->msgid = NULL;
	}

	if (compose_redirect_write_headers_from_headerlist(compose, fp))
		return -1;

	/* separator between header and body */
	err |= (fputs("\n", fp) == EOF);

	return (err ? -1:0);
}

static gint compose_redirect_write_to_file(Compose *compose, FILE *fdest)
{
	FILE *fp;
	size_t len;
	gchar buf[BUFFSIZE];
	int i = 0;
	gboolean skip = FALSE;
	gboolean err = FALSE;
	gchar *not_included[]={
		"Return-Path:",		"Delivered-To:",	"Received:",
		"Subject:",		"X-UIDL:",		"AF:",
		"NF:",			"PS:",			"SRH:",
		"SFN:",			"DSR:",			"MID:",
		"CFG:",			"PT:",			"S:",
		"RQ:",			"SSV:",			"NSV:",
		"SSH:",			"R:",			"MAID:",
		"NAID:",		"RMID:",		"FMID:",
		"SCF:",			"RRCPT:",		"NG:",
		"X-Claws-Privacy",	"X-Claws-Sign:",	"X-Claws-Encrypt",
		"X-Claws-End-Special-Headers:", 		"X-Claws-Account-Id:",
		"X-Sylpheed-Privacy",	"X-Sylpheed-Sign:",	"X-Sylpheed-Encrypt",
		"X-Sylpheed-End-Special-Headers:", 		"X-Sylpheed-Account-Id:",
		"X-Claws-Auto-Wrapping:", "X-Claws-Auto-Indent:",
		NULL
		};
	if ((fp = g_fopen(compose->redirect_filename, "rb")) == NULL) {
		FILE_OP_ERROR(compose->redirect_filename, "fopen");
		return -1;
	}

	while (procheader_get_one_field_asis(buf, sizeof(buf), fp) != -1) {
		skip = FALSE;
		for (i = 0; not_included[i] != NULL; i++) {
			if (g_ascii_strncasecmp(buf, not_included[i],
						strlen(not_included[i])) == 0) {
				skip = TRUE;
				break;
			}
		}
		if (skip)
			continue;
		if (fputs(buf, fdest) == -1)
			goto error;

		if (!prefs_common.redirect_keep_from) {
			if (g_ascii_strncasecmp(buf, "From:",
					  strlen("From:")) == 0) {
				err |= (fputs(" (by way of ", fdest) == EOF);
				if (compose->account->name
				    && *compose->account->name) {
					compose_convert_header
						(compose, buf, sizeof(buf),
						 compose->account->name,
						 strlen("From: "),
						 FALSE);
					err |= (fprintf(fdest, "%s <%s>",
						buf,
						compose->account->address) < 0);
				} else
					err |= (fprintf(fdest, "%s",
						compose->account->address) < 0);
				err |= (fputs(")", fdest) == EOF);
			}
		}

		if (fputs("\n", fdest) == -1)
			goto error;
	}

	if (err)
		goto error;

	if (compose_redirect_write_headers(compose, fdest))
		goto error;

	while ((len = fread(buf, sizeof(gchar), sizeof(buf), fp)) > 0) {
		if (fwrite(buf, sizeof(gchar), len, fdest) != len)
			goto error;
	}

	fclose(fp);

	return 0;
error:
	fclose(fp);

	return -1;
}

static gint compose_write_to_file(Compose *compose, FILE *fp, gint action, gboolean attach_parts)
{
	GtkTextBuffer *buffer;
	GtkTextIter start, end;
	gchar *chars;
	gchar *buf;
	const gchar *out_codeset;
	EncodingType encoding = ENC_UNKNOWN;
	MimeInfo *mimemsg, *mimetext;
	gint line;
	const gchar *src_codeset = CS_INTERNAL;
	gchar *from_addr = NULL;
	gchar *from_name = NULL;

	if (action == COMPOSE_WRITE_FOR_SEND)
		attach_parts = TRUE;

	/* create message MimeInfo */
	mimemsg = procmime_mimeinfo_new();
        mimemsg->type = MIMETYPE_MESSAGE;
        mimemsg->subtype = g_strdup("rfc822");
	mimemsg->content = MIMECONTENT_MEM;
	mimemsg->tmp = TRUE; /* must free content later */
	mimemsg->data.mem = compose_get_header(compose);

	/* Create text part MimeInfo */
	/* get all composed text */
	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(compose->text));
	gtk_text_buffer_get_start_iter(buffer, &start);
	gtk_text_buffer_get_end_iter(buffer, &end);
	chars = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);

	out_codeset = conv_get_charset_str(compose->out_encoding);

	if (!out_codeset && is_ascii_str(chars)) {
		out_codeset = CS_US_ASCII;
	} else if (prefs_common.outgoing_fallback_to_ascii &&
		   is_ascii_str(chars)) {
		out_codeset = CS_US_ASCII;
		encoding = ENC_7BIT;
	}

	if (!out_codeset) {
		gchar *test_conv_global_out = NULL;
		gchar *test_conv_reply = NULL;

		/* automatic mode. be automatic. */
		codeconv_set_strict(TRUE);

		out_codeset = conv_get_outgoing_charset_str();
		if (out_codeset) {
			debug_print("trying to convert to %s\n", out_codeset);
			test_conv_global_out = conv_codeset_strdup(chars, src_codeset, out_codeset);
		}

		if (!test_conv_global_out && compose->orig_charset
		&&  strcmp(compose->orig_charset, CS_US_ASCII)) {
			out_codeset = compose->orig_charset;
			debug_print("failure; trying to convert to %s\n", out_codeset);
			test_conv_reply = conv_codeset_strdup(chars, src_codeset, out_codeset);
		}

		if (!test_conv_global_out && !test_conv_reply) {
			/* we're lost */
			out_codeset = CS_INTERNAL;
			debug_print("failure; finally using %s\n", out_codeset);
		}
		g_free(test_conv_global_out);
		g_free(test_conv_reply);
		codeconv_set_strict(FALSE);
	}

	if (encoding == ENC_UNKNOWN) {
		if (prefs_common.encoding_method == CTE_BASE64)
			encoding = ENC_BASE64;
		else if (prefs_common.encoding_method == CTE_QUOTED_PRINTABLE)
			encoding = ENC_QUOTED_PRINTABLE;
		else if (prefs_common.encoding_method == CTE_8BIT)
			encoding = ENC_8BIT;
		else
			encoding = procmime_get_encoding_for_charset(out_codeset);
	}

	debug_print("src encoding = %s, out encoding = %s, transfer encoding = %s\n",
		    src_codeset, out_codeset, procmime_get_encoding_str(encoding));

	if (action == COMPOSE_WRITE_FOR_SEND) {
		codeconv_set_strict(TRUE);
		buf = conv_codeset_strdup(chars, src_codeset, out_codeset);
		codeconv_set_strict(FALSE);

		if (!buf) {
			AlertValue aval;
			gchar *msg;

			msg = g_strdup_printf(_("Can't convert the character encoding of the message \n"
						"to the specified %s charset.\n"
						"Send it as %s?"), out_codeset, src_codeset);
			aval = alertpanel_full(_("Error"), msg, GTK_STOCK_CANCEL, _("+_Send"), NULL, FALSE,
					      NULL, ALERT_ERROR, G_ALERTDEFAULT);
			g_free(msg);

			if (aval != G_ALERTALTERNATE) {
				g_free(chars);
				return -3;
			} else {
				buf = chars;
				out_codeset = src_codeset;
				chars = NULL;
			}
		}
	} else {
		buf = chars;
		out_codeset = src_codeset;
		chars = NULL;
	}
	g_free(chars);

	if (encoding == ENC_8BIT || encoding == ENC_7BIT) {
		if (!strncmp(buf, "From ", sizeof("From ")-1) ||
		    strstr(buf, "\nFrom ") != NULL) {
			encoding = ENC_QUOTED_PRINTABLE;
		}
	}

	mimetext = procmime_mimeinfo_new();
	mimetext->content = MIMECONTENT_MEM;
	mimetext->tmp = TRUE; /* must free content later */
	/* dup'ed because procmime_encode_content can turn it into a tmpfile
	 * and free the data, which we need later. */
	mimetext->data.mem = g_strdup(buf); 
	mimetext->type = MIMETYPE_TEXT;
	mimetext->subtype = g_strdup("plain");
	g_hash_table_insert(mimetext->typeparameters, g_strdup("charset"),
			    g_strdup(out_codeset));
			    
	/* protect trailing spaces when signing message */
	if (action == COMPOSE_WRITE_FOR_SEND && compose->use_signing && 
	    privacy_system_can_sign(compose->privacy_system)) {
		encoding = ENC_QUOTED_PRINTABLE;
	}
	
	debug_print("main text: %zd bytes encoded as %s in %d\n",
		strlen(buf), out_codeset, encoding);

	/* check for line length limit */
	if (action == COMPOSE_WRITE_FOR_SEND &&
	    encoding != ENC_QUOTED_PRINTABLE && encoding != ENC_BASE64 &&
	    check_line_length(buf, 1000, &line) < 0) {
		AlertValue aval;
		gchar *msg;

		msg = g_strdup_printf
			(_("Line %d exceeds the line length limit (998 bytes).\n"
			   "The contents of the message might be broken on the way to the delivery.\n"
			   "\n"
			   "Send it anyway?"), line + 1);
		aval = alertpanel(_("Warning"), msg, GTK_STOCK_CANCEL, GTK_STOCK_OK, NULL);
		g_free(msg);
		if (aval != G_ALERTALTERNATE) {
			g_free(buf);
			return -1;
		}
	}
	
	if (encoding != ENC_UNKNOWN)
		procmime_encode_content(mimetext, encoding);

	/* append attachment parts */
	if (compose_use_attach(compose) && attach_parts) {
		MimeInfo *mimempart;
		gchar *boundary = NULL;
		mimempart = procmime_mimeinfo_new();
    		mimempart->content = MIMECONTENT_EMPTY;
    		mimempart->type = MIMETYPE_MULTIPART;
	        mimempart->subtype = g_strdup("mixed");

		do {
			g_free(boundary);
			boundary = generate_mime_boundary(NULL);
		} while (strstr(buf, boundary) != NULL);

    		g_hash_table_insert(mimempart->typeparameters, g_strdup("boundary"),
				    boundary);

		mimetext->disposition = DISPOSITIONTYPE_INLINE;

		g_node_append(mimempart->node, mimetext->node);
		g_node_append(mimemsg->node, mimempart->node);

		if (compose_add_attachments(compose, mimempart) < 0)
			return -1;
	} else
		g_node_append(mimemsg->node, mimetext->node);

	g_free(buf);

	if (strlen(gtk_entry_get_text(GTK_ENTRY(compose->from_name))) != 0) {
		gchar *spec = gtk_editable_get_chars(GTK_EDITABLE(compose->from_name), 0, -1);
		/* extract name and address */
		if (strstr(spec, " <") && strstr(spec, ">")) {
			from_addr = g_strdup(strrchr(spec, '<')+1);
			*(strrchr(from_addr, '>')) = '\0';
			from_name = g_strdup(spec);
			*(strrchr(from_name, '<')) = '\0';
		} else {
			from_name = NULL;
			from_addr = NULL;
		}
		g_free(spec);
	}
	/* sign message if sending */
	if (action == COMPOSE_WRITE_FOR_SEND && compose->use_signing && 
	    privacy_system_can_sign(compose->privacy_system))
		if (!privacy_sign(compose->privacy_system, mimemsg, 
			compose->account, from_addr)) {
			g_free(from_name);
			g_free(from_addr);
			return -2;
	}
	g_free(from_name);
	g_free(from_addr);
	procmime_write_mimeinfo(mimemsg, fp);
	
	procmime_mimeinfo_free_all(mimemsg);

	return 0;
}

static gint compose_write_body_to_file(Compose *compose, const gchar *file)
{
	GtkTextBuffer *buffer;
	GtkTextIter start, end;
	FILE *fp;
	size_t len;
	gchar *chars, *tmp;

	if ((fp = g_fopen(file, "wb")) == NULL) {
		FILE_OP_ERROR(file, "fopen");
		return -1;
	}

	/* chmod for security */
	if (change_file_mode_rw(fp, file) < 0) {
		FILE_OP_ERROR(file, "chmod");
		g_warning("can't change file mode\n");
	}

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(compose->text));
	gtk_text_buffer_get_start_iter(buffer, &start);
	gtk_text_buffer_get_end_iter(buffer, &end);
	tmp = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);

	chars = conv_codeset_strdup
		(tmp, CS_INTERNAL, conv_get_locale_charset_str());

	g_free(tmp);
	if (!chars) return -1;

	/* write body */
	len = strlen(chars);
	if (fwrite(chars, sizeof(gchar), len, fp) != len) {
		FILE_OP_ERROR(file, "fwrite");
		g_free(chars);
		fclose(fp);
		claws_unlink(file);
		return -1;
	}

	g_free(chars);

	if (fclose(fp) == EOF) {
		FILE_OP_ERROR(file, "fclose");
		claws_unlink(file);
		return -1;
	}
	return 0;
}

static gint compose_remove_reedit_target(Compose *compose, gboolean force)
{
	FolderItem *item;
	MsgInfo *msginfo = compose->targetinfo;

	cm_return_val_if_fail(compose->mode == COMPOSE_REEDIT, -1);
	if (!msginfo) return -1;

	if (!force && MSG_IS_LOCKED(msginfo->flags))
		return 0;

	item = msginfo->folder;
	cm_return_val_if_fail(item != NULL, -1);

	if (procmsg_msg_exist(msginfo) &&
	    (folder_has_parent_of_type(item, F_QUEUE) ||
	     folder_has_parent_of_type(item, F_DRAFT) 
	     || msginfo == compose->autosaved_draft)) {
		if (folder_item_remove_msg(item, msginfo->msgnum) < 0) {
			g_warning("can't remove the old message\n");
			return -1;
		} else {
			debug_print("removed reedit target %d\n", msginfo->msgnum);
		}
	}

	return 0;
}

static void compose_remove_draft(Compose *compose)
{
	FolderItem *drafts;
	MsgInfo *msginfo = compose->targetinfo;
	drafts = account_get_special_folder(compose->account, F_DRAFT);

	if (procmsg_msg_exist(msginfo)) {
		folder_item_remove_msg(drafts, msginfo->msgnum);
	}

}

gint compose_queue(Compose *compose, gint *msgnum, FolderItem **item, gchar **msgpath,
		   gboolean remove_reedit_target)
{
	return compose_queue_sub (compose, msgnum, item, msgpath, FALSE, remove_reedit_target);
}

static gboolean compose_warn_encryption(Compose *compose)
{
	const gchar *warning = privacy_get_encrypt_warning(compose->privacy_system);
	AlertValue val = G_ALERTALTERNATE;
	
	if (warning == NULL)
		return TRUE;

	val = alertpanel_full(_("Encryption warning"), warning,
		  GTK_STOCK_CANCEL, _("+C_ontinue"), NULL,
		  TRUE, NULL, ALERT_WARNING, G_ALERTALTERNATE);
	if (val & G_ALERTDISABLE) {
		val &= ~G_ALERTDISABLE;
		if (val == G_ALERTALTERNATE)
			privacy_inhibit_encrypt_warning(compose->privacy_system,
				TRUE);
	}

	if (val == G_ALERTALTERNATE) {
		return TRUE;
	} else {
		return FALSE;
	} 
}

static gint compose_queue_sub(Compose *compose, gint *msgnum, FolderItem **item, 
			      gchar **msgpath, gboolean check_subject,
			      gboolean remove_reedit_target)
{
	FolderItem *queue;
	gchar *tmp;
	FILE *fp;
	GSList *cur;
	gint num;
        static gboolean lock = FALSE;
	PrefsAccount *mailac = NULL, *newsac = NULL;
	gboolean err = FALSE;

	debug_print("queueing message...\n");
	cm_return_val_if_fail(compose->account != NULL, -1);

        lock = TRUE;
	
	if (compose_check_entries(compose, check_subject) == FALSE) {
                lock = FALSE;
		if (compose->batch) {
			gtk_widget_show_all(compose->window);
		}
                return -1;
	}

	if (!compose->to_list && !compose->newsgroup_list) {
	        g_warning("can't get recipient list.");
	        lock = FALSE;
                return -1;
        }

	if (compose->to_list) {
    		if (compose->account->protocol != A_NNTP)
            		mailac = compose->account;
		else if (cur_account && cur_account->protocol != A_NNTP)
	    		mailac = cur_account;
		else if (!(mailac = compose_current_mail_account())) {
			lock = FALSE;
			alertpanel_error(_("No account for sending mails available!"));
			return -1;
		}
	}

	if (compose->newsgroup_list) {
                if (compose->account->protocol == A_NNTP)
                        newsac = compose->account;
                else {
			lock = FALSE;
			alertpanel_error(_("Selected account isn't NNTP: Posting is impossible."));
			return -1;
		}			
	}

	/* write queue header */
	tmp = g_strdup_printf("%s%cqueue.%p%08x", get_tmp_dir(),
			      G_DIR_SEPARATOR, compose, (guint) rand());
	debug_print("queuing to %s\n", tmp);
	if ((fp = g_fopen(tmp, "wb")) == NULL) {
		FILE_OP_ERROR(tmp, "fopen");
		g_free(tmp);
		lock = FALSE;
		return -2;
	}

	if (change_file_mode_rw(fp, tmp) < 0) {
		FILE_OP_ERROR(tmp, "chmod");
		g_warning("can't change file mode\n");
	}

	/* queueing variables */
	err |= (fprintf(fp, "AF:\n") < 0);
	err |= (fprintf(fp, "NF:0\n") < 0);
	err |= (fprintf(fp, "PS:10\n") < 0);
	err |= (fprintf(fp, "SRH:1\n") < 0);
	err |= (fprintf(fp, "SFN:\n") < 0);
	err |= (fprintf(fp, "DSR:\n") < 0);
	if (compose->msgid)
		err |= (fprintf(fp, "MID:<%s>\n", compose->msgid) < 0);
	else
		err |= (fprintf(fp, "MID:\n") < 0);
	err |= (fprintf(fp, "CFG:\n") < 0);
	err |= (fprintf(fp, "PT:0\n") < 0);
	err |= (fprintf(fp, "S:%s\n", compose->account->address) < 0);
	err |= (fprintf(fp, "RQ:\n") < 0);
	if (mailac)
		err |= (fprintf(fp, "SSV:%s\n", mailac->smtp_server) < 0);
	else
		err |= (fprintf(fp, "SSV:\n") < 0);
	if (newsac)
		err |= (fprintf(fp, "NSV:%s\n", newsac->nntp_server) < 0);
	else
		err |= (fprintf(fp, "NSV:\n") < 0);
	err |= (fprintf(fp, "SSH:\n") < 0);
	/* write recepient list */
	if (compose->to_list) {
		err |= (fprintf(fp, "R:<%s>", (gchar *)compose->to_list->data) < 0);
		for (cur = compose->to_list->next; cur != NULL;
		     cur = cur->next)
			err |= (fprintf(fp, ",<%s>", (gchar *)cur->data) < 0);
		err |= (fprintf(fp, "\n") < 0);
	}
	/* write newsgroup list */
	if (compose->newsgroup_list) {
		err |= (fprintf(fp, "NG:") < 0);
		err |= (fprintf(fp, "%s", (gchar *)compose->newsgroup_list->data) < 0);
		for (cur = compose->newsgroup_list->next; cur != NULL; cur = cur->next)
			err |= (fprintf(fp, ",%s", (gchar *)cur->data) < 0);
		err |= (fprintf(fp, "\n") < 0);
	}
	/* Sylpheed account IDs */
	if (mailac)
		err |= (fprintf(fp, "MAID:%d\n", mailac->account_id) < 0);
	if (newsac)
		err |= (fprintf(fp, "NAID:%d\n", newsac->account_id) < 0);

	
	if (compose->privacy_system != NULL) {
		err |= (fprintf(fp, "X-Claws-Privacy-System:%s\n", compose->privacy_system) < 0);
		err |= (fprintf(fp, "X-Claws-Sign:%d\n", compose->use_signing) < 0);
		if (compose->use_encryption) {
			gchar *encdata;
			if (!compose_warn_encryption(compose)) {
				lock = FALSE;
				fclose(fp);
				claws_unlink(tmp);
				g_free(tmp);
				return -6;
			}
			if (mailac && mailac->encrypt_to_self) {
				GSList *tmp_list = g_slist_copy(compose->to_list);
				tmp_list = g_slist_append(tmp_list, compose->account->address);
				encdata = privacy_get_encrypt_data(compose->privacy_system, tmp_list);
				g_slist_free(tmp_list);
			} else {
				encdata = privacy_get_encrypt_data(compose->privacy_system, compose->to_list);
			}
			if (encdata != NULL) {
				if (strcmp(encdata, "_DONT_ENCRYPT_")) {
					err |= (fprintf(fp, "X-Claws-Encrypt:%d\n", compose->use_encryption) < 0);
					err |= (fprintf(fp, "X-Claws-Encrypt-Data:%s\n", 
						encdata) < 0);
				} /* else we finally dont want to encrypt */
			} else {
				err |= (fprintf(fp, "X-Claws-Encrypt:%d\n", compose->use_encryption) < 0);
				/* and if encdata was null, it means there's been a problem in 
				 * key selection */
				lock = FALSE;
				fclose(fp);
				claws_unlink(tmp);
				g_free(tmp);
				return -5;
			}
			g_free(encdata);
		}
	}

	/* Save copy folder */
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn))) {
		gchar *savefolderid;
		
		savefolderid = compose_get_save_to(compose);
		err |= (fprintf(fp, "SCF:%s\n", savefolderid) < 0);
		g_free(savefolderid);
	}
	/* Save copy folder */
	if (compose->return_receipt) {
		err |= (fprintf(fp, "RRCPT:1\n") < 0);
	}
	/* Message-ID of message replying to */
	if ((compose->replyinfo != NULL) && (compose->replyinfo->msgid != NULL)) {
		gchar *folderid;
		
		folderid = folder_item_get_identifier(compose->replyinfo->folder);
		err |= (fprintf(fp, "RMID:%s\t%d\t%s\n", folderid, compose->replyinfo->msgnum, compose->replyinfo->msgid) < 0);
		g_free(folderid);
	}
	/* Message-ID of message forwarding to */
	if ((compose->fwdinfo != NULL) && (compose->fwdinfo->msgid != NULL)) {
		gchar *folderid;
		
		folderid = folder_item_get_identifier(compose->fwdinfo->folder);
		err |= (fprintf(fp, "FMID:%s\t%d\t%s\n", folderid, compose->fwdinfo->msgnum, compose->fwdinfo->msgid) < 0);
		g_free(folderid);
	}

	err |= (fprintf(fp, "X-Claws-Auto-Wrapping:%d\n", compose->autowrap) < 0);
	err |= (fprintf(fp, "X-Claws-Auto-Indent:%d\n", compose->autoindent) < 0);

	/* end of headers */
	err |= (fprintf(fp, "X-Claws-End-Special-Headers: 1\n") < 0);

	if (compose->redirect_filename != NULL) {
		if (compose_redirect_write_to_file(compose, fp) < 0) {
			lock = FALSE;
			fclose(fp);
			claws_unlink(tmp);
			g_free(tmp);
			return -2;
		}
	} else {
		gint result = 0;
		if ((result = compose_write_to_file(compose, fp, COMPOSE_WRITE_FOR_SEND, TRUE)) < 0) {
			lock = FALSE;
			fclose(fp);
			claws_unlink(tmp);
			g_free(tmp);
			return result - 1; /* -2 for a generic error, -3 for signing error, -4 for encoding */
		}
	}
	if (err == TRUE) {
		g_warning("failed to write queue message\n");
		fclose(fp);
		claws_unlink(tmp);
		g_free(tmp);
		lock = FALSE;
		return -2;
	}
	if (fclose(fp) == EOF) {
		FILE_OP_ERROR(tmp, "fclose");
		claws_unlink(tmp);
		g_free(tmp);
		lock = FALSE;
		return -2;
	}

	if (item && *item) {
		queue = *item;
	} else {
		queue = account_get_special_folder(compose->account, F_QUEUE);
	}
	if (!queue) {
		g_warning("can't find queue folder\n");
		claws_unlink(tmp);
		g_free(tmp);
		lock = FALSE;
		return -1;
	}
	folder_item_scan(queue);
	if ((num = folder_item_add_msg(queue, tmp, NULL, FALSE)) < 0) {
		g_warning("can't queue the message\n");
		claws_unlink(tmp);
		g_free(tmp);
		lock = FALSE;
		return -1;
	}
	
	if (msgpath == NULL) {
		claws_unlink(tmp);
		g_free(tmp);
	} else
		*msgpath = tmp;

	if (compose->mode == COMPOSE_REEDIT && remove_reedit_target) {
		compose_remove_reedit_target(compose, FALSE);
	}

	if ((msgnum != NULL) && (item != NULL)) {
		*msgnum = num;
		*item = queue;
	}

	return 0;
}

static int compose_add_attachments(Compose *compose, MimeInfo *parent)
{
	AttachInfo *ainfo;
	GtkTreeView *tree_view = GTK_TREE_VIEW(compose->attach_clist);
	MimeInfo *mimepart;
	struct stat statbuf;
	gchar *type, *subtype;
	GtkTreeModel *model;
	GtkTreeIter iter;

	model = gtk_tree_view_get_model(tree_view);
	
	if (!gtk_tree_model_get_iter_first(model, &iter))
		return 0;
	do {
		gtk_tree_model_get(model, &iter,
				   COL_DATA, &ainfo,
				   -1);
		
		if (!is_file_exist(ainfo->file)) {
			gchar *msg = g_strdup_printf(_("Attachment %s doesn't exist anymore. Ignore?"), ainfo->file);
			AlertValue val = alertpanel_full(_("Warning"), msg, _("Cancel sending"), _("Ignore attachment"),
		      		      NULL, FALSE, NULL, ALERT_WARNING, G_ALERTDEFAULT);
			g_free(msg);
			if (val == G_ALERTDEFAULT) {
				return -1;
			}
			continue;
		}
		mimepart = procmime_mimeinfo_new();
		mimepart->content = MIMECONTENT_FILE;
		mimepart->data.filename = g_strdup(ainfo->file);
		mimepart->tmp = FALSE; /* or we destroy our attachment */
		mimepart->offset = 0;

		g_stat(ainfo->file, &statbuf);
		mimepart->length = statbuf.st_size;

    		type = g_strdup(ainfo->content_type);

		if (!strchr(type, '/')) {
			g_free(type);
			type = g_strdup("application/octet-stream");
		}

    		subtype = strchr(type, '/') + 1;
	        *(subtype - 1) = '\0';
    		mimepart->type = procmime_get_media_type(type);
    		mimepart->subtype = g_strdup(subtype);
    		g_free(type);

		if (mimepart->type == MIMETYPE_MESSAGE && 
		    !g_ascii_strcasecmp(mimepart->subtype, "rfc822")) {
			mimepart->disposition = DISPOSITIONTYPE_INLINE;
		} else if (mimepart->type == MIMETYPE_TEXT) {
			if (!ainfo->name && compose->mode == COMPOSE_FORWARD_INLINE) {
				/* Text parts with no name come from multipart/alternative
				* forwards. Make sure the recipient won't look at the 
				* original HTML part by mistake. */
				mimepart->disposition = DISPOSITIONTYPE_ATTACHMENT;
				ainfo->name = g_strdup_printf(_("Original %s part"),
								mimepart->subtype);
			}
			if (ainfo->charset)
				g_hash_table_insert(mimepart->typeparameters,
						    g_strdup("charset"), g_strdup(ainfo->charset));
		}
		if (ainfo->name && mimepart->type != MIMETYPE_MESSAGE) {
			if (mimepart->type == MIMETYPE_APPLICATION && 
			   !strcmp2(mimepart->subtype, "octet-stream"))
				g_hash_table_insert(mimepart->typeparameters,
						g_strdup("name"), g_strdup(ainfo->name));
			g_hash_table_insert(mimepart->dispositionparameters,
					g_strdup("filename"), g_strdup(ainfo->name));
			mimepart->disposition = DISPOSITIONTYPE_ATTACHMENT;
		}

		if (mimepart->type == MIMETYPE_MESSAGE
		    || mimepart->type == MIMETYPE_MULTIPART)
			ainfo->encoding = ENC_BINARY;
		else if (compose->use_signing) {
			if (ainfo->encoding == ENC_7BIT)
				ainfo->encoding = ENC_QUOTED_PRINTABLE;
			else if (ainfo->encoding == ENC_8BIT)
				ainfo->encoding = ENC_BASE64;
		}

		
		
		procmime_encode_content(mimepart, ainfo->encoding);

		g_node_append(parent->node, mimepart->node);
	} while (gtk_tree_model_iter_next(model, &iter));
	
	return 0;
}

#define IS_IN_CUSTOM_HEADER(header) \
	(compose->account->add_customhdr && \
	 custom_header_find(compose->account->customhdr_list, header) != NULL)

static void compose_add_headerfield_from_headerlist(Compose *compose, 
						    GString *header, 
					            const gchar *fieldname,
					            const gchar *seperator)
{
	gchar *str, *fieldname_w_colon;
	gboolean add_field = FALSE;
	GSList *list;
	ComposeHeaderEntry *headerentry;
	const gchar *headerentryname;
	const gchar *trans_fieldname;
	GString *fieldstr;

	if (IS_IN_CUSTOM_HEADER(fieldname))
		return;

	debug_print("Adding %s-fields\n", fieldname);

	fieldstr = g_string_sized_new(64);

	fieldname_w_colon = g_strconcat(fieldname, ":", NULL);
	trans_fieldname = prefs_common_translated_header_name(fieldname_w_colon);

	for (list = compose->header_list; list; list = list->next) {
    		headerentry = ((ComposeHeaderEntry *)list->data);
		headerentryname = gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((headerentry->combo)))));

		if (!g_utf8_collate(trans_fieldname, headerentryname)) {
			str = gtk_editable_get_chars(GTK_EDITABLE(headerentry->entry), 0, -1);
			g_strstrip(str);
			if (str[0] != '\0') {
				if (add_field)
					g_string_append(fieldstr, seperator);
				g_string_append(fieldstr, str);
				add_field = TRUE;
			}
			g_free(str);
		}
	}
	if (add_field) {
		gchar *buf;

		buf = g_new0(gchar, fieldstr->len * 4 + 256);
		compose_convert_header
			(compose, buf, fieldstr->len * 4  + 256, fieldstr->str,
			strlen(fieldname) + 2, TRUE);
		g_string_append_printf(header, "%s: %s\n", fieldname, buf);
		g_free(buf);
	}

	g_free(fieldname_w_colon);
	g_string_free(fieldstr, TRUE);

	return;
}

static gchar *compose_get_header(Compose *compose)
{
	gchar buf[BUFFSIZE];
	const gchar *entry_str;
	gchar *str;
	gchar *name;
	GSList *list;
	gchar *std_headers[] = {"To:", "Cc:", "Bcc:", "Newsgroups:", "Reply-To:", "Followup-To:", NULL};
	GString *header;
	gchar *from_name = NULL, *from_address = NULL;
	gchar *tmp;

	cm_return_val_if_fail(compose->account != NULL, NULL);
	cm_return_val_if_fail(compose->account->address != NULL, NULL);

	header = g_string_sized_new(64);

	/* Date */
	get_rfc822_date(buf, sizeof(buf));
	g_string_append_printf(header, "Date: %s\n", buf);

	/* From */
	
	if (compose->account->name && *compose->account->name) {
		gchar *buf;
		QUOTE_IF_REQUIRED(buf, compose->account->name);
		tmp = g_strdup_printf("%s <%s>",
			buf, compose->account->address);
	} else {
		tmp = g_strdup_printf("%s",
			compose->account->address);
	}
	if (!strcmp(gtk_entry_get_text(GTK_ENTRY(compose->from_name)), tmp)
	||  strlen(gtk_entry_get_text(GTK_ENTRY(compose->from_name))) == 0) {
		/* use default */
		from_name = compose->account->name ? g_strdup(compose->account->name):NULL;
		from_address = g_strdup(compose->account->address);
	} else {
		gchar *spec = gtk_editable_get_chars(GTK_EDITABLE(compose->from_name), 0, -1);
		/* extract name and address */
		if (strstr(spec, " <") && strstr(spec, ">")) {
			from_address = g_strdup(strrchr(spec, '<')+1);
			*(strrchr(from_address, '>')) = '\0';
			from_name = g_strdup(spec);
			*(strrchr(from_name, '<')) = '\0';
		} else {
			from_name = NULL;
			from_address = g_strdup(spec);
		}
		g_free(spec);
	}
	g_free(tmp);
	
	
	if (from_name && *from_name) {
		compose_convert_header
			(compose, buf, sizeof(buf), from_name,
			 strlen("From: "), TRUE);
		QUOTE_IF_REQUIRED(name, buf);
		
		g_string_append_printf(header, "From: %s <%s>\n",
			name, from_address);
	} else
		g_string_append_printf(header, "From: %s\n", from_address);
	
	g_free(from_name);
	g_free(from_address);

	/* To */
	compose_add_headerfield_from_headerlist(compose, header, "To", ", ");

	/* Newsgroups */
	compose_add_headerfield_from_headerlist(compose, header, "Newsgroups", ",");

	/* Cc */
	compose_add_headerfield_from_headerlist(compose, header, "Cc", ", ");

	/* Bcc */
	/* 
	 * If this account is a NNTP account remove Bcc header from 
	 * message body since it otherwise will be publicly shown
	 */
	if (compose->account->protocol != A_NNTP)
		compose_add_headerfield_from_headerlist(compose, header, "Bcc", ", ");

	/* Subject */
	str = gtk_editable_get_chars(GTK_EDITABLE(compose->subject_entry), 0, -1);

	if (*str != '\0' && !IS_IN_CUSTOM_HEADER("Subject")) {
		g_strstrip(str);
		if (*str != '\0') {
			compose_convert_header(compose, buf, sizeof(buf), str,
					       strlen("Subject: "), FALSE);
			g_string_append_printf(header, "Subject: %s\n", buf);
		}
	}
	g_free(str);

	/* Message-ID */
	if (compose->account->set_domain && compose->account->domain) {
		g_snprintf(buf, sizeof(buf), "%s", compose->account->domain); 
	} else if (!strncmp(get_domain_name(), "localhost", strlen("localhost"))) {
		g_snprintf(buf, sizeof(buf), "%s", 
			strchr(compose->account->address, '@') ?
				strchr(compose->account->address, '@')+1 :
				compose->account->address);
	} else {
		g_snprintf(buf, sizeof(buf), "%s", "");
	}
	
	if (compose->account->gen_msgid) {
		gchar *addr = NULL;
		if (compose->account->msgid_with_addr) {
			addr = compose->account->address;
		}
		generate_msgid(buf, sizeof(buf), addr);
		g_string_append_printf(header, "Message-ID: <%s>\n", buf);
		compose->msgid = g_strdup(buf);
	} else {
		compose->msgid = NULL;
	}

	if (compose->remove_references == FALSE) {
		/* In-Reply-To */
		if (compose->inreplyto && compose->to_list)
			g_string_append_printf(header, "In-Reply-To: <%s>\n", compose->inreplyto);
	
		/* References */
		if (compose->references)
			g_string_append_printf(header, "References: %s\n", compose->references);
	}

	/* Followup-To */
	compose_add_headerfield_from_headerlist(compose, header, "Followup-To", ",");

	/* Reply-To */
	compose_add_headerfield_from_headerlist(compose, header, "Reply-To", ", ");

	/* Organization */
	if (compose->account->organization &&
	    strlen(compose->account->organization) &&
	    !IS_IN_CUSTOM_HEADER("Organization")) {
		compose_convert_header(compose, buf, sizeof(buf),
				       compose->account->organization,
				       strlen("Organization: "), FALSE);
		g_string_append_printf(header, "Organization: %s\n", buf);
	}

	/* Program version and system info */
	if (g_slist_length(compose->to_list) && !IS_IN_CUSTOM_HEADER("X-Mailer") &&
	    !compose->newsgroup_list) {
		g_string_append_printf(header, "X-Mailer: %s (GTK+ %d.%d.%d; %s)\n",
			prog_version,
			gtk_major_version, gtk_minor_version, gtk_micro_version,
			TARGET_ALIAS);
	}
	if (g_slist_length(compose->newsgroup_list) && !IS_IN_CUSTOM_HEADER("X-Newsreader")) {
		g_string_append_printf(header, "X-Newsreader: %s (GTK+ %d.%d.%d; %s)\n",
			prog_version,
			gtk_major_version, gtk_minor_version, gtk_micro_version,
			TARGET_ALIAS);
	}

	/* custom headers */
	if (compose->account->add_customhdr) {
		GSList *cur;

		for (cur = compose->account->customhdr_list; cur != NULL;
		     cur = cur->next) {
			CustomHeader *chdr = (CustomHeader *)cur->data;

			if (custom_header_is_allowed(chdr->name)) {
				compose_convert_header
					(compose, buf, sizeof(buf),
					 chdr->value ? chdr->value : "",
					 strlen(chdr->name) + 2, FALSE);
				g_string_append_printf(header, "%s: %s\n", chdr->name, buf);
			}
		}
	}

	/* Automatic Faces and X-Faces */
	if (get_account_xface (buf, sizeof(buf), compose->account->account_name) == 0) {
		g_string_append_printf(header, "X-Face: %s\n", buf);
	}
	else if (get_default_xface (buf, sizeof(buf)) == 0) {
		g_string_append_printf(header, "X-Face: %s\n", buf);
	}
	if (get_account_face (buf, sizeof(buf), compose->account->account_name) == 0) {
		g_string_append_printf(header, "Face: %s\n", buf);
	}
	else if (get_default_face (buf, sizeof(buf)) == 0) {
		g_string_append_printf(header, "Face: %s\n", buf);
	}

	/* PRIORITY */
	switch (compose->priority) {
		case PRIORITY_HIGHEST: g_string_append_printf(header, "Importance: high\n"
						   "X-Priority: 1 (Highest)\n");
			break;
		case PRIORITY_HIGH: g_string_append_printf(header, "Importance: high\n"
						"X-Priority: 2 (High)\n");
			break;
		case PRIORITY_NORMAL: break;
		case PRIORITY_LOW: g_string_append_printf(header, "Importance: low\n"
					       "X-Priority: 4 (Low)\n");
			break;
		case PRIORITY_LOWEST: g_string_append_printf(header, "Importance: low\n"
						  "X-Priority: 5 (Lowest)\n");
			break;
		default: debug_print("compose: priority unknown : %d\n",
				     compose->priority);
	}

	/* Request Return Receipt */
	if (!IS_IN_CUSTOM_HEADER("Disposition-Notification-To")) {
		if (compose->return_receipt) {
			if (compose->account->name
			    && *compose->account->name) {
				compose_convert_header(compose, buf, sizeof(buf), 
						       compose->account->name, 
						       strlen("Disposition-Notification-To: "),
						       TRUE);
				g_string_append_printf(header, "Disposition-Notification-To: %s <%s>\n", buf, compose->account->address);
			} else
				g_string_append_printf(header, "Disposition-Notification-To: %s\n", compose->account->address);
		}
	}

	/* get special headers */
	for (list = compose->header_list; list; list = list->next) {
    		ComposeHeaderEntry *headerentry;
		gchar *tmp;
		gchar *headername;
		gchar *headername_wcolon;
		const gchar *headername_trans;
		gchar *headervalue;
		gchar **string;
		gboolean standard_header = FALSE;

		headerentry = ((ComposeHeaderEntry *)list->data);

		tmp = g_strdup(gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((headerentry->combo))))));
		g_strstrip(tmp);
		if (*tmp == '\0' || strchr(tmp, ' ') != NULL || strchr(tmp, '\r') != NULL || strchr(tmp, '\n') != NULL) {
			g_free(tmp);
			continue;
		}

		if (!strstr(tmp, ":")) {
			headername_wcolon = g_strconcat(tmp, ":", NULL);
			headername = g_strdup(tmp);
		} else {
			headername_wcolon = g_strdup(tmp);
			headername = g_strdup(strtok(tmp, ":"));
		}
		g_free(tmp);
		
		entry_str = gtk_entry_get_text(GTK_ENTRY(headerentry->entry));
		Xstrdup_a(headervalue, entry_str, return NULL);
		subst_char(headervalue, '\r', ' ');
		subst_char(headervalue, '\n', ' ');
		string = std_headers;
		while (*string != NULL) {
			headername_trans = prefs_common_translated_header_name(*string);
			if (!strcmp(headername_trans, headername_wcolon))
				standard_header = TRUE;
			string++;
		}
		if (!standard_header && !IS_IN_CUSTOM_HEADER(headername))
			g_string_append_printf(header, "%s %s\n", headername_wcolon, headervalue);
				
		g_free(headername);
		g_free(headername_wcolon);		
	}

	str = header->str;
	g_string_free(header, FALSE);

	return str;
}

#undef IS_IN_CUSTOM_HEADER

static void compose_convert_header(Compose *compose, gchar *dest, gint len, gchar *src,
				   gint header_len, gboolean addr_field)
{
	gchar *tmpstr = NULL;
	const gchar *out_codeset = NULL;

	cm_return_if_fail(src != NULL);
	cm_return_if_fail(dest != NULL);

	if (len < 1) return;

	tmpstr = g_strdup(src);

	subst_char(tmpstr, '\n', ' ');
	subst_char(tmpstr, '\r', ' ');
	g_strchomp(tmpstr);

	if (!g_utf8_validate(tmpstr, -1, NULL)) {
		gchar *mybuf = g_malloc(strlen(tmpstr)*2 +1);
		conv_localetodisp(mybuf, strlen(tmpstr)*2 +1, tmpstr);
		g_free(tmpstr);
		tmpstr = mybuf;
	}

	codeconv_set_strict(TRUE);
	conv_encode_header_full(dest, len, tmpstr, header_len, addr_field, 
		conv_get_charset_str(compose->out_encoding));
	codeconv_set_strict(FALSE);
	
	if (!dest || *dest == '\0') {
		gchar *test_conv_global_out = NULL;
		gchar *test_conv_reply = NULL;

		/* automatic mode. be automatic. */
		codeconv_set_strict(TRUE);

		out_codeset = conv_get_outgoing_charset_str();
		if (out_codeset) {
			debug_print("trying to convert to %s\n", out_codeset);
			test_conv_global_out = conv_codeset_strdup(src, CS_INTERNAL, out_codeset);
		}

		if (!test_conv_global_out && compose->orig_charset
		&&  strcmp(compose->orig_charset, CS_US_ASCII)) {
			out_codeset = compose->orig_charset;
			debug_print("failure; trying to convert to %s\n", out_codeset);
			test_conv_reply = conv_codeset_strdup(src, CS_INTERNAL, out_codeset);
		}

		if (!test_conv_global_out && !test_conv_reply) {
			/* we're lost */
			out_codeset = CS_INTERNAL;
			debug_print("finally using %s\n", out_codeset);
		}
		g_free(test_conv_global_out);
		g_free(test_conv_reply);
		conv_encode_header_full(dest, len, tmpstr, header_len, addr_field, 
					out_codeset);
		codeconv_set_strict(FALSE);
	}
	g_free(tmpstr);
}

static void compose_add_to_addressbook_cb(GtkMenuItem *menuitem, gpointer user_data)
{
	gchar *address;

	cm_return_if_fail(user_data != NULL);

	address = g_strdup(gtk_entry_get_text(GTK_ENTRY(user_data)));
	g_strstrip(address);
	if (*address != '\0') {
		gchar *name = procheader_get_fromname(address);
		extract_address(address);
		addressbook_add_contact(name, address, NULL, NULL);
	}
	g_free(address);
}

static void compose_entry_popup_extend(GtkEntry *entry, GtkMenu *menu, gpointer user_data)
{
	GtkWidget *menuitem;
	gchar *address;

	cm_return_if_fail(menu != NULL);
	cm_return_if_fail(GTK_IS_MENU_SHELL(menu));

	menuitem = gtk_separator_menu_item_new();
	gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), menuitem);
	gtk_widget_show(menuitem);

	menuitem = gtk_menu_item_new_with_mnemonic(_("Add to address _book"));
	gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), menuitem);

	address = g_strdup(gtk_entry_get_text(GTK_ENTRY(entry)));
	g_strstrip(address);
	if (*address == '\0') {
		gtk_widget_set_sensitive(GTK_WIDGET(menuitem), FALSE);
	}

	g_signal_connect(G_OBJECT(menuitem), "activate",
			 G_CALLBACK(compose_add_to_addressbook_cb), entry);
	gtk_widget_show(menuitem);
}

static void compose_create_header_entry(Compose *compose) 
{
	gchar *headers[] = {"To:", "Cc:", "Bcc:", "Newsgroups:", "Reply-To:", "Followup-To:", NULL};

	GtkWidget *combo;
	GtkWidget *entry;
	GtkWidget *button;
	GtkWidget *hbox;
	gchar **string;
	const gchar *header = NULL;
	ComposeHeaderEntry *headerentry;
	gboolean standard_header = FALSE;
	GtkListStore *model;
	GtkTreeIter iter;
#if !(GTK_CHECK_VERSION(2,12,0))
	GtkTooltips *tips = compose->tooltips;
#endif
	
	headerentry = g_new0(ComposeHeaderEntry, 1);

	/* Combo box */
	model = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);
	combo = gtk_combo_box_entry_new_with_model(GTK_TREE_MODEL(model), 0);
	COMBOBOX_ADD(model, prefs_common_translated_header_name("To:"),
			COMPOSE_TO);
	COMBOBOX_ADD(model, prefs_common_translated_header_name("Cc:"),
			COMPOSE_CC);
	COMBOBOX_ADD(model, prefs_common_translated_header_name("Bcc:"),
			COMPOSE_BCC);
	COMBOBOX_ADD(model, prefs_common_translated_header_name("Newsgroups:"),
			COMPOSE_NEWSGROUPS);			
	COMBOBOX_ADD(model, prefs_common_translated_header_name("Reply-To:"),
			COMPOSE_REPLYTO);
	COMBOBOX_ADD(model, prefs_common_translated_header_name("Followup-To:"),
			COMPOSE_FOLLOWUPTO);

	gtk_combo_box_set_active(GTK_COMBO_BOX(combo), 0);
	g_signal_connect(G_OBJECT(gtk_bin_get_child(GTK_BIN((combo)))), "grab_focus",
			 G_CALLBACK(compose_grab_focus_cb), compose);
	gtk_widget_show(combo);
	gtk_table_attach(GTK_TABLE(compose->header_table), combo, 0, 1,
			compose->header_nextrow, compose->header_nextrow+1,
			GTK_SHRINK, GTK_FILL, 0, 0);
	if (compose->header_last && (compose->draft_timeout_tag != -2)) {
		const gchar *last_header_entry = gtk_entry_get_text(
				GTK_ENTRY(gtk_bin_get_child(GTK_BIN((compose->header_last->combo)))));
		string = headers;
		while (*string != NULL) {
			if (!strcmp(prefs_common_translated_header_name(*string), last_header_entry))
				standard_header = TRUE;
			string++;
		}
		if (standard_header)
			header = gtk_entry_get_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((compose->header_last->combo)))));
	}
	if (!compose->header_last || !standard_header) {
		switch(compose->account->protocol) {
			case A_NNTP:
				header = prefs_common_translated_header_name("Newsgroups:");
				break;
			default:
				header = prefs_common_translated_header_name("To:");
				break;
		}								    
	}
	if (header)
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((combo)))), header);

	g_signal_connect_after(G_OBJECT(gtk_bin_get_child(GTK_BIN((combo)))), "grab_focus",
			 G_CALLBACK(compose_grab_focus_cb), compose);

	/* Entry field with cleanup button */
	button = gtk_button_new();
	gtk_button_set_image(GTK_BUTTON(button),
                        gtk_image_new_from_stock(GTK_STOCK_CLEAR, GTK_ICON_SIZE_MENU));
	gtk_widget_show(button);
	CLAWS_SET_TIP(button,
		_("Delete entry contents"));
	entry = gtk_entry_new(); 
	gtk_widget_show(entry);
	CLAWS_SET_TIP(entry,
		_("Use <tab> to autocomplete from addressbook"));
	hbox = gtk_hbox_new (FALSE, 0);
	gtk_widget_show(hbox);
	gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
	gtk_table_attach(GTK_TABLE(compose->header_table), hbox, 1, 2,
			compose->header_nextrow, compose->header_nextrow+1,
			GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);

        g_signal_connect(G_OBJECT(entry), "key-press-event", 
			 G_CALLBACK(compose_headerentry_key_press_event_cb), 
			 headerentry);
    	g_signal_connect(G_OBJECT(entry), "changed", 
			 G_CALLBACK(compose_headerentry_changed_cb), 
			 headerentry);
	g_signal_connect_after(G_OBJECT(entry), "grab_focus",
			 G_CALLBACK(compose_grab_focus_cb), compose);

	g_signal_connect(G_OBJECT(button), "clicked",
			 G_CALLBACK(compose_headerentry_button_clicked_cb),
			 headerentry); 
			 
	/* email dnd */
	gtk_drag_dest_set(entry, GTK_DEST_DEFAULT_ALL, compose_mime_types, 
			  sizeof(compose_mime_types)/sizeof(compose_mime_types[0]),
			  GDK_ACTION_COPY | GDK_ACTION_MOVE);
	g_signal_connect(G_OBJECT(entry), "drag_data_received",
			 G_CALLBACK(compose_header_drag_received_cb),
			 entry);
	g_signal_connect(G_OBJECT(entry), "drag-drop",
			 G_CALLBACK(compose_drag_drop),
			 compose);
	g_signal_connect(G_OBJECT(entry), "populate-popup",
			 G_CALLBACK(compose_entry_popup_extend),
			 NULL);
	
	address_completion_register_entry(GTK_ENTRY(entry), TRUE);

        headerentry->compose = compose;
        headerentry->combo = combo;
        headerentry->entry = entry;
        headerentry->button = button;
        headerentry->hbox = hbox;
        headerentry->headernum = compose->header_nextrow;
        headerentry->type = PREF_NONE;

        compose->header_nextrow++;
	compose->header_last = headerentry;		
	compose->header_list =
		g_slist_append(compose->header_list,
			       headerentry);
}

static void compose_add_header_entry(Compose *compose, const gchar *header,
				gchar *text, ComposePrefType pref_type) 
{
	ComposeHeaderEntry *last_header = compose->header_last;
	gchar *tmp = g_strdup(text), *email;
	gboolean replyto_hdr;
	
	replyto_hdr = (!strcasecmp(header,
				prefs_common_translated_header_name("Reply-To:")) ||
			!strcasecmp(header,
				prefs_common_translated_header_name("Followup-To:")) ||
			!strcasecmp(header,
				prefs_common_translated_header_name("In-Reply-To:")));
		
	extract_address(tmp);
	email = g_utf8_strdown(tmp, -1);
	
	if (replyto_hdr == FALSE &&
	    g_hash_table_lookup(compose->email_hashtable, email) != NULL)
	{
		debug_print("Ignoring duplicate address - %s %s, pref_type: %d\n",
				header, text, (gint) pref_type);
		g_free(email);
		g_free(tmp);
		return;
	}
	
	if (!strcasecmp(header, prefs_common_translated_header_name("In-Reply-To:")))
		gtk_entry_set_text(GTK_ENTRY(
			gtk_bin_get_child(GTK_BIN(last_header->combo))), header);
	else
		combobox_select_by_text(GTK_COMBO_BOX(last_header->combo), header);
	gtk_entry_set_text(GTK_ENTRY(last_header->entry), text);
	last_header->type = pref_type;

	if (replyto_hdr == FALSE)
		g_hash_table_insert(compose->email_hashtable, email,
				    GUINT_TO_POINTER(1));
	else
		g_free(email);
	
	g_free(tmp);
}

static void compose_destroy_headerentry(Compose *compose, 
					ComposeHeaderEntry *headerentry)
{
	gchar *text = gtk_editable_get_chars(GTK_EDITABLE(headerentry->entry), 0, -1);
	gchar *email;

	extract_address(text);
	email = g_utf8_strdown(text, -1);
	g_hash_table_remove(compose->email_hashtable, email);
	g_free(text);
	g_free(email);
	
	gtk_widget_destroy(headerentry->combo);
	gtk_widget_destroy(headerentry->entry);
	gtk_widget_destroy(headerentry->button);
	gtk_widget_destroy(headerentry->hbox);
	g_free(headerentry);
}

static void compose_remove_header_entries(Compose *compose) 
{
	GSList *list;
	for (list = compose->header_list; list; list = list->next)
		compose_destroy_headerentry(compose, (ComposeHeaderEntry *)list->data);

	compose->header_last = NULL;
	g_slist_free(compose->header_list);
	compose->header_list = NULL;
	compose->header_nextrow = 1;
	compose_create_header_entry(compose);
}

static GtkWidget *compose_create_header(Compose *compose) 
{
	GtkWidget *from_optmenu_hbox;
	GtkWidget *header_scrolledwin;
	GtkWidget *header_table;

	gint count = 0;

	/* header labels and entries */
	header_scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(header_scrolledwin);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(header_scrolledwin), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

	header_table = gtk_table_new(2, 2, FALSE);
	gtk_widget_show(header_table);
	gtk_container_set_border_width(GTK_CONTAINER(header_table), BORDER_WIDTH);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(header_scrolledwin), header_table);
	gtk_viewport_set_shadow_type(GTK_VIEWPORT(gtk_bin_get_child(GTK_BIN((header_scrolledwin)))), GTK_SHADOW_NONE);
	count = 0;

	/* option menu for selecting accounts */
	from_optmenu_hbox = compose_account_option_menu_create(compose);
	gtk_table_attach(GTK_TABLE(header_table), from_optmenu_hbox,
				  0, 2, count, count + 1, GTK_EXPAND | GTK_FILL, GTK_SHRINK, 0, 0);
	count++;

	compose->header_table = header_table;
	compose->header_list = NULL;
	compose->header_nextrow = count;

	compose_create_header_entry(compose);

	compose->table	          = NULL;

	return header_scrolledwin ;
}

static gboolean popup_attach_button_pressed(GtkWidget *widget, gpointer data)
{
	Compose *compose = (Compose *)data;
	GdkEventButton event;
	
	event.button = 3;
	event.time = gtk_get_current_event_time();

	return attach_button_pressed(compose->attach_clist, &event, compose);
}

static GtkWidget *compose_create_attach(Compose *compose)
{
	GtkWidget *attach_scrwin;
	GtkWidget *attach_clist;

	GtkListStore *store;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;

	/* attachment list */
	attach_scrwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(attach_scrwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(attach_scrwin, -1, 80);

	store = gtk_list_store_new(N_ATTACH_COLS, 
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_STRING,
				   G_TYPE_POINTER,
				   G_TYPE_AUTO_POINTER,
				   -1);
	attach_clist = GTK_WIDGET(gtk_tree_view_new_with_model
					(GTK_TREE_MODEL(store)));
	gtk_container_add(GTK_CONTAINER(attach_scrwin), attach_clist);
	g_object_unref(store);
	
	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
			(_("Mime type"), renderer, "text", 
			 COL_MIMETYPE, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(attach_clist), column);			 
	
	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
			(_("Size"), renderer, "text", 
			 COL_SIZE, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(attach_clist), column);			 
	
	renderer = gtk_cell_renderer_text_new();
	column = gtk_tree_view_column_new_with_attributes
			(_("Name"), renderer, "text", 
			 COL_NAME, NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(attach_clist), column);

	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(attach_clist),
				     prefs_common.use_stripes_everywhere);
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(attach_clist));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

	g_signal_connect(G_OBJECT(attach_clist), "row_activated",
			 G_CALLBACK(attach_selected), compose);
	g_signal_connect(G_OBJECT(attach_clist), "button_press_event",
			 G_CALLBACK(attach_button_pressed), compose);
#ifndef MAEMO
	g_signal_connect(G_OBJECT(attach_clist), "popup-menu",
			 G_CALLBACK(popup_attach_button_pressed), compose);
#else
	gtk_widget_tap_and_hold_setup(GTK_WIDGET(attach_clist), NULL, NULL,
			GTK_TAP_AND_HOLD_NONE | GTK_TAP_AND_HOLD_NO_INTERNALS);
	g_signal_connect(G_OBJECT(attach_clist), "tap-and-hold",
			 G_CALLBACK(popup_attach_button_pressed), compose);
#endif
	g_signal_connect(G_OBJECT(attach_clist), "key_press_event",
			 G_CALLBACK(attach_key_pressed), compose);

	/* drag and drop */
	gtk_drag_dest_set(attach_clist,
			  GTK_DEST_DEFAULT_ALL, compose_mime_types, 
			  sizeof(compose_mime_types)/sizeof(compose_mime_types[0]),
			  GDK_ACTION_COPY | GDK_ACTION_MOVE);
	g_signal_connect(G_OBJECT(attach_clist), "drag_data_received",
			 G_CALLBACK(compose_attach_drag_received_cb),
			 compose);
	g_signal_connect(G_OBJECT(attach_clist), "drag-drop",
			 G_CALLBACK(compose_drag_drop),
			 compose);

	compose->attach_scrwin = attach_scrwin;
	compose->attach_clist  = attach_clist;

	return attach_scrwin;
}

static void compose_savemsg_checkbtn_cb(GtkWidget *widget, Compose *compose);
static void compose_savemsg_select_cb(GtkWidget *widget, Compose *compose);

static GtkWidget *compose_create_others(Compose *compose)
{
	GtkWidget *table;
	GtkWidget *savemsg_checkbtn;
	GtkWidget *savemsg_combo;
	GtkWidget *savemsg_select;
	
	guint rowcount = 0;
	gchar *folderidentifier;

	/* Table for settings */
	table = gtk_table_new(3, 1, FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(table), BORDER_WIDTH);
	gtk_widget_show(table);
	gtk_table_set_row_spacings(GTK_TABLE(table), VSPACING_NARROW);
	rowcount = 0;

	/* Save Message to folder */
	savemsg_checkbtn = gtk_check_button_new_with_label(_("Save Message to "));
	gtk_widget_show(savemsg_checkbtn);
	gtk_table_attach(GTK_TABLE(table), savemsg_checkbtn, 0, 1, rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
	if (account_get_special_folder(compose->account, F_OUTBOX)) {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(savemsg_checkbtn), prefs_common.savemsg);
	}
	g_signal_connect(G_OBJECT(savemsg_checkbtn), "toggled",
			 G_CALLBACK(compose_savemsg_checkbtn_cb), compose);

	savemsg_combo = gtk_combo_box_entry_new_text();
	compose->savemsg_checkbtn = savemsg_checkbtn;
	compose->savemsg_combo = savemsg_combo;
	gtk_widget_show(savemsg_combo);

	if (prefs_common.compose_save_to_history)
		combobox_set_popdown_strings(GTK_COMBO_BOX(savemsg_combo),
				prefs_common.compose_save_to_history);

	gtk_table_attach(GTK_TABLE(table), savemsg_combo, 1, 2, rowcount, rowcount + 1, GTK_FILL|GTK_EXPAND, GTK_SHRINK, 0, 0);
	gtk_widget_set_sensitive(GTK_WIDGET(savemsg_combo), prefs_common.savemsg);
	g_signal_connect_after(G_OBJECT(savemsg_combo), "grab_focus",
			 G_CALLBACK(compose_grab_focus_cb), compose);
	if (account_get_special_folder(compose->account, F_OUTBOX)) {
		folderidentifier = folder_item_get_identifier(account_get_special_folder
				  (compose->account, F_OUTBOX));
		compose_set_save_to(compose, folderidentifier);
		g_free(folderidentifier);
	}

	savemsg_select = gtkut_get_browse_file_btn(_("_Browse"));
	gtk_widget_show(savemsg_select);
	gtk_table_attach(GTK_TABLE(table), savemsg_select, 2, 3, rowcount, rowcount + 1, GTK_SHRINK | GTK_FILL, GTK_SHRINK, 0, 0);
	g_signal_connect(G_OBJECT(savemsg_select), "clicked",
			 G_CALLBACK(compose_savemsg_select_cb),
			 compose);

	rowcount++;

	return table;	
}

static void compose_savemsg_checkbtn_cb(GtkWidget *widget, Compose *compose) 
{
	gtk_widget_set_sensitive(GTK_WIDGET(compose->savemsg_combo),
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn)));
}

static void compose_savemsg_select_cb(GtkWidget *widget, Compose *compose)
{
	FolderItem *dest;
	gchar * path;

	dest = foldersel_folder_sel(NULL, FOLDER_SEL_COPY, NULL, FALSE);
	if (!dest) return;

	path = folder_item_get_identifier(dest);

	compose_set_save_to(compose, path);
	g_free(path);
}

static void entry_paste_clipboard(Compose *compose, GtkWidget *entry, gboolean wrap,
				  GdkAtom clip, GtkTextIter *insert_place);


static gboolean text_clicked(GtkWidget *text, GdkEventButton *event,
                                       Compose *compose)
{
	gint prev_autowrap;
	GtkTextBuffer *buffer = GTK_TEXT_VIEW(text)->buffer;
#if USE_ENCHANT
	if (event->button == 3) {
		GtkTextIter iter;
		GtkTextIter sel_start, sel_end;
		gboolean stuff_selected;
		gint x, y;
		/* move the cursor to allow GtkAspell to check the word
		 * under the mouse */
		if (event->x && event->y) {
			gtk_text_view_window_to_buffer_coords(GTK_TEXT_VIEW(text),
				GTK_TEXT_WINDOW_TEXT, event->x, event->y,
				&x, &y);
			gtk_text_view_get_iter_at_location (GTK_TEXT_VIEW(text),
				&iter, x, y);
		} else {
			GtkTextMark *mark = gtk_text_buffer_get_insert(buffer);
			gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
		}
		/* get selection */
		stuff_selected = gtk_text_buffer_get_selection_bounds(
				buffer,
				&sel_start, &sel_end);

		gtk_text_buffer_place_cursor (buffer, &iter);
		/* reselect stuff */
		if (stuff_selected 
		&& gtk_text_iter_in_range(&iter, &sel_start, &sel_end)) {
			gtk_text_buffer_select_range(buffer,
				&sel_start, &sel_end);
		}
		return FALSE; /* pass the event so that the right-click goes through */
	}
#endif
	if (event->button == 2) {
		GtkTextIter iter;
		gint x, y;
		BLOCK_WRAP();
		
		/* get the middle-click position to paste at the correct place */
		gtk_text_view_window_to_buffer_coords(GTK_TEXT_VIEW(text),
			GTK_TEXT_WINDOW_TEXT, event->x, event->y,
			&x, &y);
		gtk_text_view_get_iter_at_location (GTK_TEXT_VIEW(text),
			&iter, x, y);
		
		entry_paste_clipboard(compose, text, 
				prefs_common.linewrap_pastes,
				GDK_SELECTION_PRIMARY, &iter);
		UNBLOCK_WRAP();
		return TRUE;
	}
	return FALSE;
}

#if USE_ENCHANT
static void compose_spell_menu_changed(void *data)
{
	Compose *compose = (Compose *)data;
	GSList *items;
	GtkWidget *menuitem;
	GtkWidget *parent_item;
	GtkMenu *menu = GTK_MENU(gtk_menu_new());
	GSList *spell_menu;

	if (compose->gtkaspell == NULL)
		return;

	parent_item = gtk_ui_manager_get_widget(compose->ui_manager, 
			"/Menu/Spelling/Options");

	/* setting the submenu removes /Spelling/Options from the factory 
	 * so we need to save it */

	if (parent_item == NULL) {
		parent_item = compose->aspell_options_menu;
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(parent_item), NULL);
	} else
		compose->aspell_options_menu = parent_item;

	spell_menu = gtkaspell_make_config_menu(compose->gtkaspell);

	spell_menu = g_slist_reverse(spell_menu);
	for (items = spell_menu;
	     items; items = items->next) {
		menuitem = GTK_WIDGET(GTK_MENU_ITEM(items->data));
		gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), GTK_WIDGET(menuitem));
		gtk_widget_show(GTK_WIDGET(menuitem));
	}
	g_slist_free(spell_menu);

	gtk_menu_item_set_submenu(GTK_MENU_ITEM(parent_item), GTK_WIDGET(menu));
	gtk_widget_show(parent_item);
}

static void compose_dict_changed(void *data)
{
	Compose *compose = (Compose *) data;

	if(compose->gtkaspell && 
       	   compose->gtkaspell->recheck_when_changing_dict == FALSE)
		return;

	gtkaspell_highlight_all(compose->gtkaspell);
	claws_spell_entry_recheck_all(CLAWS_SPELL_ENTRY(compose->subject_entry));
}
#endif

static gboolean compose_popup_menu(GtkWidget *widget, gpointer data)
{
	Compose *compose = (Compose *)data;
	GdkEventButton event;
	
	event.button = 3;
	event.time = gtk_get_current_event_time();
	event.x = 0;
	event.y = 0;

	return text_clicked(compose->text, &event, compose);
}

static gboolean compose_force_window_origin = TRUE;
static Compose *compose_create(PrefsAccount *account,
						 FolderItem *folder,
						 ComposeMode mode,
						 gboolean batch)
{
	Compose   *compose;
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *menubar;
	GtkWidget *handlebox;

	GtkWidget *notebook;
	
	GtkWidget *attach_hbox;
	GtkWidget *attach_lab1;
	GtkWidget *attach_lab2;

	GtkWidget *vbox2;

	GtkWidget *label;
	GtkWidget *subject_hbox;
	GtkWidget *subject_frame;
	GtkWidget *subject_entry;
	GtkWidget *subject;
	GtkWidget *paned;

	GtkWidget *edit_vbox;
	GtkWidget *ruler_hbox;
	GtkWidget *ruler;
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextBuffer *buffer;
	GtkClipboard *clipboard;
	CLAWS_TIP_DECL();

	UndoMain *undostruct;

	gchar *titles[N_ATTACH_COLS];
	GtkWidget *popupmenu;
	GtkWidget *tmpl_menu;
	GtkActionGroup *action_group = NULL;

#if USE_ENCHANT
        GtkAspell * gtkaspell = NULL;
#endif

	static GdkGeometry geometry;

	cm_return_val_if_fail(account != NULL, NULL);

	debug_print("Creating compose window...\n");
	compose = g_new0(Compose, 1);

	titles[COL_MIMETYPE] = _("MIME type");
	titles[COL_SIZE]     = _("Size");
	titles[COL_NAME]     = _("Name");
	titles[COL_CHARSET]  = _("Charset");

	compose->batch = batch;
	compose->account = account;
	compose->folder = folder;
	
	compose->mutex = g_mutex_new();
	compose->set_cursor_pos = -1;

#if !(GTK_CHECK_VERSION(2,12,0))
	compose->tooltips = tips;
#endif

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "compose");

	gtk_window_set_resizable(GTK_WINDOW(window), TRUE);
	gtk_widget_set_size_request(window, -1, prefs_common.compose_height);

	if (!geometry.max_width) {
		geometry.max_width = gdk_screen_width();
		geometry.max_height = gdk_screen_height();
	}

	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL,
				      &geometry, GDK_HINT_MAX_SIZE);
	if (!geometry.min_width) {
		geometry.min_width = 600;
		geometry.min_height = 440;
	}
	gtk_window_set_geometry_hints(GTK_WINDOW(window), NULL,
				      &geometry, GDK_HINT_MIN_SIZE);

#ifndef GENERIC_UMPC	
	if (compose_force_window_origin)
		gtk_window_move(GTK_WINDOW(window), prefs_common.compose_x, 
				 prefs_common.compose_y);
#endif
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(compose_delete_cb), compose);
	MANAGE_WINDOW_SIGNALS_CONNECT(window);
	gtk_widget_realize(window);

	gtkut_widget_set_composer_icon(window);

	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	compose->ui_manager = gtk_ui_manager_new();
	action_group = cm_menu_create_action_group_full(compose->ui_manager,"Menu", compose_entries,
			G_N_ELEMENTS(compose_entries), (gpointer)compose);
	gtk_action_group_add_toggle_actions(action_group, compose_toggle_entries,
			G_N_ELEMENTS(compose_toggle_entries), (gpointer)compose);
	gtk_action_group_add_radio_actions(action_group, compose_radio_rm_entries,
			G_N_ELEMENTS(compose_radio_rm_entries), COMPOSE_REPLY, G_CALLBACK(compose_reply_change_mode_cb), (gpointer)compose);
	gtk_action_group_add_radio_actions(action_group, compose_radio_prio_entries,
			G_N_ELEMENTS(compose_radio_prio_entries), PRIORITY_NORMAL, G_CALLBACK(compose_set_priority_cb), (gpointer)compose);
	gtk_action_group_add_radio_actions(action_group, compose_radio_enc_entries,
			G_N_ELEMENTS(compose_radio_enc_entries), C_AUTO, G_CALLBACK(compose_set_encoding_cb), (gpointer)compose);

#ifndef MAEMO
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/", "Menu", NULL, GTK_UI_MANAGER_MENUBAR)
#else
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/", "Menu", NULL, GTK_UI_MANAGER_POPUP)
#endif

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu", "Message", "Message", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu", "Edit", "Edit", GTK_UI_MANAGER_MENU)
#ifdef USE_ENCHANT
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu", "Spelling", "Spelling", GTK_UI_MANAGER_MENU)
#endif
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu", "Options", "Options", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu", "Tools", "Tools", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu", "Help", "Help", GTK_UI_MANAGER_MENU)

/* Compose menu */
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "Send", "Message/Send", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "SendLater", "Message/SendLater", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "Separator1", "Message/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "AttachFile", "Message/AttachFile", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "InsertFile", "Message/InsertFile", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "InsertSig", "Message/InsertSig", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "Separator2", "Message/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "Save", "Message/Save", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "Separator3", "Message/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Message", "Close", "Message/Close", GTK_UI_MANAGER_MENUITEM)

/* Edit menu */
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Undo", "Edit/Undo", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Redo", "Edit/Redo", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Separator1", "Edit/---", GTK_UI_MANAGER_SEPARATOR)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Cut", "Edit/Cut", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Copy", "Edit/Copy", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Paste", "Edit/Paste", GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "SpecialPaste", "Edit/SpecialPaste", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/SpecialPaste", "AsQuotation", "Edit/SpecialPaste/AsQuotation", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/SpecialPaste", "Wrapped", "Edit/SpecialPaste/Wrapped", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/SpecialPaste", "Unwrapped", "Edit/SpecialPaste/Unwrapped", GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "SelectAll", "Edit/SelectAll", GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Advanced", "Edit/Advanced", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "BackChar", "Edit/Advanced/BackChar", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "ForwChar", "Edit/Advanced/ForwChar", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "BackWord", "Edit/Advanced/BackWord", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "ForwWord", "Edit/Advanced/ForwWord", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "BegLine", "Edit/Advanced/BegLine", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "EndLine", "Edit/Advanced/EndLine", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "PrevLine", "Edit/Advanced/PrevLine", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "NextLine", "Edit/Advanced/NextLine", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "DelBackChar", "Edit/Advanced/DelBackChar", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "DelForwChar", "Edit/Advanced/DelForwChar", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "DelBackWord", "Edit/Advanced/DelBackWord", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "DelForwWord", "Edit/Advanced/DelForwWord", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "DelLine", "Edit/Advanced/DelLine", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit/Advanced", "DelEndLine", "Edit/Advanced/DelEndLine", GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Separator2", "Edit/---", GTK_UI_MANAGER_SEPARATOR)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Find", "Edit/Find", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "WrapPara", "Edit/WrapPara", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "WrapAllLines", "Edit/WrapAllLines", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "AutoWrap", "Edit/AutoWrap", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "AutoIndent", "Edit/AutoIndent", GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "Separator3", "Edit/---", GTK_UI_MANAGER_SEPARATOR)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Edit", "ExtEditor", "Edit/ExtEditor", GTK_UI_MANAGER_MENUITEM)

#if USE_ENCHANT
/* Spelling menu */
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Spelling", "CheckAllSel", "Spelling/CheckAllSel", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Spelling", "HighlightAll", "Spelling/HighlightAll", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Spelling", "CheckBackwards", "Spelling/CheckBackwards", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Spelling", "ForwardNext", "Spelling/ForwardNext", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Spelling", "Separator1", "Spelling/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Spelling", "Options", "Spelling/Options", GTK_UI_MANAGER_MENU)
#endif

/* Options menu */
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "ReplyMode", "Options/ReplyMode", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/ReplyMode", "Normal", "Options/ReplyMode/Normal", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/ReplyMode", "All", "Options/ReplyMode/All", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/ReplyMode", "Sender", "Options/ReplyMode/Sender", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/ReplyMode", "List", "Options/ReplyMode/List", GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Separator1", "Options/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "PrivacySystem", "Options/PrivacySystem", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/PrivacySystem", "PlaceHolder", "Options/PrivacySystem/PlaceHolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Sign", "Options/Sign", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Encrypt", "Options/Encrypt", GTK_UI_MANAGER_MENUITEM)

	
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Separator2", "Options/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Priority", "Options/Priority", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Priority", "Highest", "Options/Priority/Highest", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Priority", "High", "Options/Priority/High", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Priority", "Normal", "Options/Priority/Normal", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Priority", "Low", "Options/Priority/Low", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Priority", "Lowest", "Options/Priority/Lowest", GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Separator3", "Options/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "RequestRetRcpt", "Options/RequestRetRcpt", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Separator4", "Options/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "RemoveReferences", "Options/RemoveReferences", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Separator5", "Options/---", GTK_UI_MANAGER_SEPARATOR)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options", "Encoding", "Options/Encoding", GTK_UI_MANAGER_MENU)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", CS_AUTO, "Options/Encoding/"CS_AUTO, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Separator1", "Options/Encoding/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", CS_US_ASCII, "Options/Encoding/"CS_US_ASCII, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", CS_UTF_8, "Options/Encoding/"CS_UTF_8, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Separator2", "Options/Encoding/---", GTK_UI_MANAGER_SEPARATOR)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Western", "Options/Encoding/Western", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Western", CS_ISO_8859_1, "Options/Encoding/Western/"CS_ISO_8859_1, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Western", CS_ISO_8859_15, "Options/Encoding/Western/"CS_ISO_8859_15, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Western", CS_WINDOWS_1252, "Options/Encoding/Western/"CS_WINDOWS_1252, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", CS_ISO_8859_2, "Options/Encoding/"CS_ISO_8859_2, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Baltic", "Options/Encoding/Baltic", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Baltic", CS_ISO_8859_13, "Options/Encoding/Baltic/"CS_ISO_8859_13, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Baltic", CS_ISO_8859_4, "Options/Encoding/Baltic/"CS_ISO_8859_4, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", CS_ISO_8859_7, "Options/Encoding/"CS_ISO_8859_7, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Hebrew", "Options/Encoding/Hebrew", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Hebrew", CS_ISO_8859_8, "Options/Encoding/Hebrew/"CS_ISO_8859_8, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Hebrew", CS_WINDOWS_1255, "Options/Encoding/Hebrew/"CS_WINDOWS_1255, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Arabic", "Options/Encoding/Arabic", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Arabic", CS_ISO_8859_6, "Options/Encoding/Arabic/"CS_ISO_8859_6, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Arabic", CS_WINDOWS_1256, "Options/Encoding/Arabic/"CS_WINDOWS_1256, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", CS_ISO_8859_9, "Options/Encoding/"CS_ISO_8859_9, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Cyrillic", "Options/Encoding/Cyrillic", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Cyrillic", CS_ISO_8859_5, "Options/Encoding/Cyrillic/"CS_ISO_8859_5, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Cyrillic", CS_KOI8_R, "Options/Encoding/Cyrillic/"CS_KOI8_R, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Cyrillic", CS_KOI8_U, "Options/Encoding/Cyrillic/"CS_KOI8_U, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Cyrillic", CS_WINDOWS_1251, "Options/Encoding/Cyrillic/"CS_WINDOWS_1251, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Japanese", "Options/Encoding/Japanese", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Japanese", CS_ISO_2022_JP, "Options/Encoding/Japanese/"CS_ISO_2022_JP, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Japanese", CS_ISO_2022_JP_2, "Options/Encoding/Japanese/"CS_ISO_2022_JP_2, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Japanese", CS_EUC_JP, "Options/Encoding/Japanese/"CS_EUC_JP, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Japanese", CS_SHIFT_JIS, "Options/Encoding/Japanese/"CS_SHIFT_JIS, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Chinese", "Options/Encoding/Chinese", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Chinese", CS_GB18030, "Options/Encoding/Chinese/"CS_GB18030, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Chinese", CS_GB2312, "Options/Encoding/Chinese/"CS_GB2312, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Chinese", CS_GBK, "Options/Encoding/Chinese/"CS_GBK, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Chinese", CS_BIG5, "Options/Encoding/Chinese/"CS_BIG5, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Chinese", CS_EUC_TW, "Options/Encoding/Chinese/"CS_EUC_TW, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Korean", "Options/Encoding/Korean", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Korean", CS_EUC_KR, "Options/Encoding/Korean/"CS_EUC_KR, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Korean", CS_ISO_2022_KR, "Options/Encoding/Korean/"CS_ISO_2022_KR, GTK_UI_MANAGER_MENUITEM)

	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding", "Thai", "Options/Encoding/Thai", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Thai", CS_TIS_620, "Options/Encoding/Thai/"CS_TIS_620, GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Options/Encoding/Thai", CS_WINDOWS_874, "Options/Encoding/Thai/"CS_WINDOWS_874, GTK_UI_MANAGER_MENUITEM)
/* phew. */

/* Tools menu */
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Tools", "ShowRuler", "Tools/ShowRuler", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Tools", "AddressBook", "Tools/AddressBook", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Tools", "Template", "Tools/Template", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Tools/Template", "PlaceHolder", "Tools/Template/PlaceHolder", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Tools", "Actions", "Tools/Actions", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Tools/Actions", "PlaceHolder", "Tools/Actions/PlaceHolder", GTK_UI_MANAGER_MENUITEM)

/* Help menu */
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Menu/Help", "About", "Help/About", GTK_UI_MANAGER_MENUITEM)

	menubar = gtk_ui_manager_get_widget(compose->ui_manager, "/Menu");
	gtk_widget_show_all(menubar);

	gtk_window_add_accel_group(GTK_WINDOW(window), gtk_ui_manager_get_accel_group(compose->ui_manager));
#ifndef MAEMO
	gtk_box_pack_start(GTK_BOX(vbox), menubar, FALSE, TRUE, 0);
#else
	hildon_window_set_menu(HILDON_WINDOW(window), GTK_MENU(menubar));
#endif

	if (prefs_common.toolbar_detachable) {
		handlebox = gtk_handle_box_new();
	} else {
		handlebox = gtk_hbox_new(FALSE, 0);
	}
	gtk_box_pack_start(GTK_BOX(vbox), handlebox, FALSE, FALSE, 0);

	gtk_widget_realize(handlebox);
#ifdef MAEMO
	compose->toolbar = toolbar_create(TOOLBAR_COMPOSE, window,
					  (gpointer)compose);
#else
	compose->toolbar = toolbar_create(TOOLBAR_COMPOSE, handlebox,
					  (gpointer)compose);
#endif

	vbox2 = gtk_vbox_new(FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), vbox2, TRUE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(vbox2), 0);
	
	/* Notebook */
	notebook = gtk_notebook_new();
	gtk_widget_set_size_request(notebook, -1, 130);
	gtk_widget_show(notebook);

	/* header labels and entries */
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			compose_create_header(compose),
			gtk_label_new_with_mnemonic(_("Hea_der")));
	/* attachment list */
	attach_hbox = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(attach_hbox);
	
	attach_lab1 = gtk_label_new_with_mnemonic(_("_Attachments"));
	gtk_widget_show(attach_lab1);
	gtk_box_pack_start(GTK_BOX(attach_hbox), attach_lab1, TRUE, TRUE, 0);
	
	attach_lab2 = gtk_label_new("");
	gtk_widget_show(attach_lab2);
	gtk_box_pack_start(GTK_BOX(attach_hbox), attach_lab2, FALSE, FALSE, 0);
	
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			compose_create_attach(compose),
			attach_hbox);
	/* Others Tab */
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook),
			compose_create_others(compose),
			gtk_label_new_with_mnemonic(_("Othe_rs")));

	/* Subject */
	subject_hbox = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(subject_hbox);

	subject_frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(subject_frame), GTK_SHADOW_NONE);
	gtk_box_pack_start(GTK_BOX(subject_hbox), subject_frame, TRUE, TRUE, 0);
	gtk_widget_show(subject_frame);

	subject = gtk_hbox_new(FALSE, HSPACING_NARROW);
	gtk_container_set_border_width(GTK_CONTAINER(subject), 0);
	gtk_widget_show(subject);

	label = gtk_label_new(_("Subject:"));
	gtk_box_pack_start(GTK_BOX(subject), label, FALSE, FALSE, 0);
	gtk_widget_show(label);

#ifdef USE_ENCHANT
	subject_entry = claws_spell_entry_new();
#else
	subject_entry = gtk_entry_new();
#endif
	gtk_box_pack_start(GTK_BOX(subject), subject_entry, TRUE, TRUE, 0);
	g_signal_connect_after(G_OBJECT(subject_entry), "grab_focus",
			 G_CALLBACK(compose_grab_focus_cb), compose);
	gtk_widget_show(subject_entry);
	compose->subject_entry = subject_entry;
	gtk_container_add(GTK_CONTAINER(subject_frame), subject);
	
	edit_vbox = gtk_vbox_new(FALSE, 0);

	gtk_box_pack_start(GTK_BOX(edit_vbox), subject_hbox, FALSE, FALSE, 0);

	/* ruler */
	ruler_hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(edit_vbox), ruler_hbox, FALSE, FALSE, 0);

	ruler = gtk_shruler_new();
	gtk_ruler_set_range(GTK_RULER(ruler), 0.0, 100.0, 1.0, 100.0);
	gtk_box_pack_start(GTK_BOX(ruler_hbox), ruler, TRUE, TRUE,
			   BORDER_WIDTH);

	/* text widget */
	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);
	gtk_box_pack_start(GTK_BOX(edit_vbox), scrolledwin, TRUE, TRUE, 0);
	gtk_widget_set_size_request(scrolledwin, prefs_common.compose_width, -1);

	text = gtk_text_view_new();
	if (prefs_common.show_compose_margin) {
		gtk_text_view_set_left_margin(GTK_TEXT_VIEW(text), 6);
		gtk_text_view_set_right_margin(GTK_TEXT_VIEW(text), 6);
	}
	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD_CHAR);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), TRUE);
	clipboard = gtk_clipboard_get(GDK_SELECTION_PRIMARY);
	gtk_text_buffer_add_selection_clipboard(buffer, clipboard);
	
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);

	g_signal_connect_after(G_OBJECT(text), "size_allocate",
			       G_CALLBACK(compose_edit_size_alloc),
			       ruler);
	g_signal_connect(G_OBJECT(buffer), "changed",
			 G_CALLBACK(compose_changed_cb), compose);
	g_signal_connect(G_OBJECT(text), "grab_focus",
			 G_CALLBACK(compose_grab_focus_cb), compose);
	g_signal_connect(G_OBJECT(buffer), "insert_text",
			 G_CALLBACK(text_inserted), compose);
	g_signal_connect(G_OBJECT(text), "button_press_event",
			 G_CALLBACK(text_clicked), compose);
#ifndef MAEMO
	g_signal_connect(G_OBJECT(text), "popup-menu",
			 G_CALLBACK(compose_popup_menu), compose);
#else
	gtk_widget_tap_and_hold_setup(GTK_WIDGET(text), NULL, NULL,
			GTK_TAP_AND_HOLD_NONE | GTK_TAP_AND_HOLD_NO_INTERNALS);
	g_signal_connect(G_OBJECT(text), "tap-and-hold",
			 G_CALLBACK(compose_popup_menu), compose);
#endif
	g_signal_connect(G_OBJECT(subject_entry), "changed",
			 G_CALLBACK(compose_changed_cb), compose);

	/* drag and drop */
	gtk_drag_dest_set(text, GTK_DEST_DEFAULT_ALL, compose_mime_types, 
			  sizeof(compose_mime_types)/sizeof(compose_mime_types[0]),
			  GDK_ACTION_COPY | GDK_ACTION_MOVE);
	g_signal_connect(G_OBJECT(text), "drag_data_received",
			 G_CALLBACK(compose_insert_drag_received_cb),
			 compose);
	g_signal_connect(G_OBJECT(text), "drag-drop",
			 G_CALLBACK(compose_drag_drop),
			 compose);
	gtk_widget_show_all(vbox);

	/* pane between attach clist and text */
	paned = gtk_vpaned_new();
	gtk_container_add(GTK_CONTAINER(vbox2), paned);
#ifdef MAEMO
	if( maemo_mainwindow_is_fullscreen(mainwindow_get_mainwindow()->window) )
		gtk_widget_set_size_request(edit_vbox, -1, mode == COMPOSE_NEW ? 300 : 280);
	else
		gtk_widget_set_size_request(edit_vbox, -1, mode == COMPOSE_NEW ? 250 : 230);
#endif
	gtk_paned_add1(GTK_PANED(paned), notebook);
	gtk_paned_add2(GTK_PANED(paned), edit_vbox);
	gtk_widget_show_all(paned);


	if (prefs_common.textfont) {
		PangoFontDescription *font_desc;

		font_desc = pango_font_description_from_string
			(prefs_common.textfont);
		if (font_desc) {
			gtk_widget_modify_font(text, font_desc);
			pango_font_description_free(font_desc);
		}
	}

	gtk_action_group_add_actions(action_group, compose_popup_entries,
			G_N_ELEMENTS(compose_popup_entries), (gpointer)compose);
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/", "Popup", NULL, GTK_UI_MANAGER_MENUBAR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Popup", "Compose", "Compose", GTK_UI_MANAGER_MENU)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Popup/Compose", "Add", "Compose/Add", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Popup/Compose", "Remove", "Compose/Remove", GTK_UI_MANAGER_MENUITEM)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Popup/Compose", "Separator1", "Compose/---", GTK_UI_MANAGER_SEPARATOR)
	MENUITEM_ADDUI_MANAGER(compose->ui_manager, "/Popup/Compose", "Properties", "Compose/Properties", GTK_UI_MANAGER_MENUITEM)
	
	popupmenu = gtk_menu_item_get_submenu(GTK_MENU_ITEM(gtk_ui_manager_get_widget(compose->ui_manager, "/Popup/Compose")));

	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/Undo", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/Redo", FALSE);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/RemoveReferences", FALSE);

	tmpl_menu = gtk_ui_manager_get_widget(compose->ui_manager, "/Menu/Tools/Template");

	undostruct = undo_init(text);
	undo_set_change_state_func(undostruct, &compose_undo_state_changed,
				   compose);

	address_completion_start(window);

	compose->window        = window;
	compose->vbox	       = vbox;
	compose->menubar       = menubar;
	compose->handlebox     = handlebox;

	compose->vbox2	       = vbox2;

	compose->paned = paned;

	compose->attach_label  = attach_lab2;

	compose->notebook      = notebook;
	compose->edit_vbox     = edit_vbox;
	compose->ruler_hbox    = ruler_hbox;
	compose->ruler         = ruler;
	compose->scrolledwin   = scrolledwin;
	compose->text	       = text;

	compose->focused_editable = NULL;

	compose->popupmenu    = popupmenu;

	compose->tmpl_menu = tmpl_menu;

	compose->mode = mode;
	compose->rmode = mode;

	compose->targetinfo = NULL;
	compose->replyinfo  = NULL;
	compose->fwdinfo    = NULL;

	compose->email_hashtable = g_hash_table_new_full(g_str_hash,
				g_str_equal, (GDestroyNotify) g_free, NULL);
	
	compose->replyto     = NULL;
	compose->cc	     = NULL;
	compose->bcc	     = NULL;
	compose->followup_to = NULL;

	compose->ml_post     = NULL;

	compose->inreplyto   = NULL;
	compose->references  = NULL;
	compose->msgid       = NULL;
	compose->boundary    = NULL;

	compose->autowrap       = prefs_common.autowrap;
	compose->autoindent	= prefs_common.auto_indent;
	compose->use_signing    = FALSE;
	compose->use_encryption = FALSE;
	compose->privacy_system = NULL;

	compose->modified = FALSE;

	compose->return_receipt = FALSE;

	compose->to_list        = NULL;
	compose->newsgroup_list = NULL;

	compose->undostruct = undostruct;

	compose->sig_str = NULL;

	compose->exteditor_file    = NULL;
	compose->exteditor_pid     = -1;
	compose->exteditor_tag     = -1;
	compose->draft_timeout_tag = -2; /* inhibit auto-drafting while loading */

#if USE_ENCHANT
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Spelling", FALSE);
	if (mode != COMPOSE_REDIRECT) {
        	if (prefs_common.enable_aspell && prefs_common.dictionary &&
	    	    strcmp(prefs_common.dictionary, "")) {
			gtkaspell = gtkaspell_new(prefs_common.dictionary,
						  prefs_common.alt_dictionary,
						  conv_get_locale_charset_str(),
						  prefs_common.misspelled_col,
						  prefs_common.check_while_typing,
						  prefs_common.recheck_when_changing_dict,
						  prefs_common.use_alternate,
						  prefs_common.use_both_dicts,
						  GTK_TEXT_VIEW(text),
						  GTK_WINDOW(compose->window),
						  compose_dict_changed,
						  compose_spell_menu_changed,
						  compose);
			if (!gtkaspell) {
				alertpanel_error(_("Spell checker could not "
						"be started.\n%s"),
						gtkaspell_checkers_strerror());
				gtkaspell_checkers_reset_error();
			} else {
				cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Spelling", TRUE);
			}
        	}
	}
	compose->gtkaspell = gtkaspell;
	compose_spell_menu_changed(compose);
	claws_spell_entry_set_gtkaspell(CLAWS_SPELL_ENTRY(subject_entry), gtkaspell);
#endif

	compose_select_account(compose, account, TRUE);

	cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Edit/AutoWrap", prefs_common.autowrap);
	cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Edit/AutoIndent", prefs_common.auto_indent);

	if (account->set_autocc && account->auto_cc && mode != COMPOSE_REEDIT)
		compose_entry_append(compose, account->auto_cc, COMPOSE_CC, PREF_ACCOUNT);

	if (account->set_autobcc && account->auto_bcc && mode != COMPOSE_REEDIT) 
		compose_entry_append(compose, account->auto_bcc, COMPOSE_BCC, PREF_ACCOUNT);
	
	if (account->set_autoreplyto && account->auto_replyto && mode != COMPOSE_REEDIT)
		compose_entry_append(compose, account->auto_replyto, COMPOSE_REPLYTO, PREF_ACCOUNT);

	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/ReplyMode", compose->mode == COMPOSE_REPLY);
	if (account->protocol != A_NNTP)
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((compose->header_last->combo)))),
				prefs_common_translated_header_name("To:"));
	else
		gtk_entry_set_text(GTK_ENTRY(gtk_bin_get_child(GTK_BIN((compose->header_last->combo)))),
				prefs_common_translated_header_name("Newsgroups:"));

	addressbook_set_target_compose(compose);
	
	if (mode != COMPOSE_REDIRECT)
		compose_set_template_menu(compose);
	else {
		cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Tools/Template", FALSE);
	}

	compose_list = g_list_append(compose_list, compose);

	if (!prefs_common.show_ruler)
		gtk_widget_hide(ruler_hbox);
		
	cm_toggle_menu_set_active_full(compose->ui_manager, "Menu/Tools/ShowRuler", prefs_common.show_ruler);

	/* Priority */
	compose->priority = PRIORITY_NORMAL;
	compose_update_priority_menu_item(compose);

	compose_set_out_encoding(compose);
	
	/* Actions menu */
	compose_update_actions_menu(compose);

	/* Privacy Systems menu */
	compose_update_privacy_systems_menu(compose);

	activate_privacy_system(compose, account, TRUE);
	toolbar_set_style(compose->toolbar->toolbar, compose->handlebox, prefs_common.toolbar_style);
	if (batch) {
		gtk_widget_realize(window);
	} else {
		gtk_widget_show(window);
#ifdef MAEMO
		maemo_window_full_screen_if_needed(GTK_WINDOW(window));
		maemo_connect_key_press_to_mainwindow(GTK_WINDOW(window));
#endif
	}
	
	return compose;
}

static GtkWidget *compose_account_option_menu_create(Compose *compose)
{
	GList *accounts;
	GtkWidget *hbox;
	GtkWidget *optmenu;
	GtkWidget *optmenubox;
	GtkListStore *menu;
	GtkTreeIter iter;
	GtkWidget *from_name = NULL;
#if !(GTK_CHECK_VERSION(2,12,0))
	GtkTooltips *tips = compose->tooltips;
#endif

	gint num = 0, def_menu = 0;
	
	accounts = account_get_list();
	cm_return_val_if_fail(accounts != NULL, NULL);

	optmenubox = gtk_event_box_new();
	optmenu = gtkut_sc_combobox_create(optmenubox, FALSE);
	menu = GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(optmenu)));

	hbox = gtk_hbox_new(FALSE, 6);
	from_name = gtk_entry_new();
	
	g_signal_connect_after(G_OBJECT(from_name), "grab_focus",
			 G_CALLBACK(compose_grab_focus_cb), compose);

	for (; accounts != NULL; accounts = accounts->next, num++) {
		PrefsAccount *ac = (PrefsAccount *)accounts->data;
		gchar *name, *from = NULL;

		if (ac == compose->account) def_menu = num;

		name = g_markup_printf_escaped(_("From: <i>%s</i>"),
				       ac->account_name);
		
		if (ac == compose->account) {
			if (ac->name && *ac->name) {
				gchar *buf;
				QUOTE_IF_REQUIRED_NORMAL(buf, ac->name, return NULL);
				from = g_strdup_printf("%s <%s>",
						       buf, ac->address);
				gtk_entry_set_text(GTK_ENTRY(from_name), from);
			} else {
				from = g_strdup_printf("%s",
						       ac->address);
				gtk_entry_set_text(GTK_ENTRY(from_name), from);
			}
		}
		COMBOBOX_ADD(menu, name, ac->account_id);
		g_free(name);
		g_free(from);
	}

	gtk_combo_box_set_active(GTK_COMBO_BOX(optmenu), def_menu);

	g_signal_connect(G_OBJECT(optmenu), "changed",
			G_CALLBACK(account_activated),
			compose);
	g_signal_connect(G_OBJECT(from_name), "populate-popup",
			 G_CALLBACK(compose_entry_popup_extend),
			 NULL);

	gtk_box_pack_start(GTK_BOX(hbox), optmenubox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), from_name, TRUE, TRUE, 0);
	
	CLAWS_SET_TIP(optmenubox,
		_("Account to use for this email"));
	CLAWS_SET_TIP(from_name,
		_("Sender address to be used"));

	compose->account_combo = optmenu;
	compose->from_name = from_name;
	
	return hbox;
}

static void compose_set_priority_cb(GtkAction *action, GtkRadioAction *current, gpointer data)
{
	gboolean active = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (current));
	gint value = gtk_radio_action_get_current_value (GTK_RADIO_ACTION (current));
	Compose *compose = (Compose *) data;
	if (active) {
		compose->priority = value;
	}
}

static void compose_reply_change_mode(Compose *compose,
				    ComposeMode action)
{
	gboolean was_modified = compose->modified;

	gboolean all = FALSE, ml = FALSE, sender = FALSE, followup = FALSE;
	
	cm_return_if_fail(compose->replyinfo != NULL);
	
	if (action == COMPOSE_REPLY && prefs_common.default_reply_list)
		ml = TRUE;
	if (action == COMPOSE_REPLY && compose->rmode == COMPOSE_FOLLOWUP_AND_REPLY_TO)
		followup = TRUE;
	if (action == COMPOSE_REPLY_TO_ALL)
		all = TRUE;
	if (action == COMPOSE_REPLY_TO_SENDER)
		sender = TRUE;
	if (action == COMPOSE_REPLY_TO_LIST)
		ml = TRUE;

	compose_remove_header_entries(compose);
	compose_reply_set_entry(compose, compose->replyinfo, all, ml, sender, followup);
	if (compose->account->set_autocc && compose->account->auto_cc)
		compose_entry_append(compose, compose->account->auto_cc, COMPOSE_CC, PREF_ACCOUNT);

	if (compose->account->set_autobcc && compose->account->auto_bcc) 
		compose_entry_append(compose, compose->account->auto_bcc, COMPOSE_BCC, PREF_ACCOUNT);
	
	if (compose->account->set_autoreplyto && compose->account->auto_replyto)
		compose_entry_append(compose, compose->account->auto_replyto, COMPOSE_REPLYTO, PREF_ACCOUNT);
	compose_show_first_last_header(compose, TRUE);
	compose->modified = was_modified;
	compose_set_title(compose);
}

static void compose_reply_change_mode_cb(GtkAction *action, GtkRadioAction *current, gpointer data)
{
	gboolean active = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (current));
	gint value = gtk_radio_action_get_current_value (GTK_RADIO_ACTION (current));
	Compose *compose = (Compose *) data;
	
	if (active)
		compose_reply_change_mode(compose, value);
}

static void compose_update_priority_menu_item(Compose * compose)
{
	GtkWidget *menuitem = NULL;
	switch (compose->priority) {
		case PRIORITY_HIGHEST:
			menuitem = gtk_ui_manager_get_widget
				(compose->ui_manager, "/Menu/Options/Priority/Highest");
			break;
		case PRIORITY_HIGH:
			menuitem = gtk_ui_manager_get_widget
				(compose->ui_manager, "/Menu/Options/Priority/High");
			break;
		case PRIORITY_NORMAL:
			menuitem = gtk_ui_manager_get_widget
				(compose->ui_manager, "/Menu/Options/Priority/Normal");
			break;
		case PRIORITY_LOW:
			menuitem = gtk_ui_manager_get_widget
				(compose->ui_manager, "/Menu/Options/Priority/Low");
			break;
		case PRIORITY_LOWEST:
			menuitem = gtk_ui_manager_get_widget
				(compose->ui_manager, "/Menu/Options/Priority/Lowest");
			break;
	}
	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);
}	

static void compose_set_privacy_system_cb(GtkWidget *widget, gpointer data)
{
	Compose *compose = (Compose *) data;
	gchar *systemid;
	gboolean can_sign = FALSE, can_encrypt = FALSE;

	cm_return_if_fail(GTK_IS_CHECK_MENU_ITEM(widget));

	if (!GTK_CHECK_MENU_ITEM(widget)->active)
		return;

	systemid = g_object_get_data(G_OBJECT(widget), "privacy_system");
	g_free(compose->privacy_system);
	compose->privacy_system = NULL;
	if (systemid != NULL) {
		compose->privacy_system = g_strdup(systemid);

		can_sign = privacy_system_can_sign(systemid);
		can_encrypt = privacy_system_can_encrypt(systemid);
	}

	debug_print("activated privacy system: %s\n", systemid != NULL ? systemid : "None");

	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/Sign", can_sign);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/Encrypt", can_encrypt);
}

static void compose_update_privacy_system_menu_item(Compose * compose, gboolean warn)
{
	static gchar *branch_path = "/Menu/Options/PrivacySystem";
	GtkWidget *menuitem = NULL;
	GList *amenu;
	gboolean can_sign = FALSE, can_encrypt = FALSE;
	gboolean found = FALSE;

	if (compose->privacy_system != NULL) {
		gchar *systemid;
		menuitem = gtk_menu_item_get_submenu(GTK_MENU_ITEM(
				gtk_ui_manager_get_widget(compose->ui_manager, branch_path)));
		cm_return_if_fail(menuitem != NULL);

		amenu = GTK_MENU_SHELL(menuitem)->children;
		menuitem = NULL;
		while (amenu != NULL) {
		        GList *alist = amenu->next;

			systemid = g_object_get_data(G_OBJECT(amenu->data), "privacy_system");
			if (systemid != NULL) {
				if (strcmp(systemid, compose->privacy_system) == 0 &&
				    GTK_IS_CHECK_MENU_ITEM(amenu->data)) {
					menuitem = GTK_WIDGET(amenu->data);

					can_sign = privacy_system_can_sign(systemid);
					can_encrypt = privacy_system_can_encrypt(systemid);
					found = TRUE;
					break;
				} 
			} else if (strlen(compose->privacy_system) == 0 && 
				   GTK_IS_CHECK_MENU_ITEM(amenu->data)) {
					menuitem = GTK_WIDGET(amenu->data);

					can_sign = FALSE;
					can_encrypt = FALSE;
					found = TRUE;
					break;
			}

			amenu = alist;
		}
		if (menuitem != NULL)
			gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);
		
		if (warn && !found && strlen(compose->privacy_system)) {
			alertpanel_warning(_("The privacy system '%s' cannot be loaded. You "
				  "will not be able to sign or encrypt this message."),
				  compose->privacy_system);
		}
	} 

	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/Sign", can_sign);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options/Encrypt", can_encrypt);
}	
 
static void compose_set_out_encoding(Compose *compose)
{
	CharSet out_encoding;
	const gchar *branch = NULL;
	out_encoding = conv_get_charset_from_str(prefs_common.outgoing_charset);

	switch(out_encoding) {
		case C_AUTO: branch = "Menu/Options/Encoding/" CS_AUTO; break;
		case C_US_ASCII: branch = "Menu/Options/Encoding/" CS_US_ASCII; break;
		case C_UTF_8: branch = "Menu/Options/Encoding/" CS_UTF_8; break;
		case C_ISO_8859_2: branch = "Menu/Options/Encoding/" CS_ISO_8859_2; break;
		case C_ISO_8859_7: branch = "Menu/Options/Encoding/" CS_ISO_8859_7; break;
		case C_ISO_8859_9: branch = "Menu/Options/Encoding/" CS_ISO_8859_9; break;
		case C_ISO_8859_1: branch = "Menu/Options/Encoding/Western/" CS_ISO_8859_1; break;
		case C_ISO_8859_15: branch = "Menu/Options/Encoding/Western/" CS_ISO_8859_15; break;
		case C_WINDOWS_1252: branch = "Menu/Options/Encoding/Western/" CS_WINDOWS_1252; break;
		case C_ISO_8859_13: branch = "Menu/Options/Encoding/Baltic/" CS_ISO_8859_13; break;
		case C_ISO_8859_4: branch = "Menu/Options/Encoding/Baltic" CS_ISO_8859_4; break;
		case C_ISO_8859_8: branch = "Menu/Options/Encoding/Hebrew/" CS_ISO_8859_8; break;
		case C_WINDOWS_1255: branch = "Menu/Options/Encoding/Hebrew/" CS_WINDOWS_1255; break;
		case C_ISO_8859_6: branch = "Menu/Options/Encoding/Arabic/" CS_ISO_8859_6; break;
		case C_WINDOWS_1256: branch = "Menu/Options/Encoding/Arabic/" CS_WINDOWS_1256; break;
		case C_ISO_8859_5: branch = "Menu/Options/Encoding/Cyrillic/" CS_ISO_8859_5; break;
		case C_KOI8_R: branch = "Menu/Options/Encoding/Cyrillic/" CS_KOI8_R; break;
		case C_KOI8_U: branch = "Menu/Options/Encoding/Cyrillic/" CS_KOI8_U; break;
		case C_WINDOWS_1251: branch = "Menu/Options/Encoding/Cyrillic/" CS_WINDOWS_1251; break;
		case C_ISO_2022_JP: branch = "Menu/Options/Encoding/Japanese/" CS_ISO_2022_JP; break;
		case C_ISO_2022_JP_2: branch = "Menu/Options/Encoding/Japanese/" CS_ISO_2022_JP_2; break;
		case C_EUC_JP: branch = "Menu/Options/Encoding/Japanese/" CS_EUC_JP; break;
		case C_SHIFT_JIS: branch = "Menu/Options/Encoding/Japanese/" CS_SHIFT_JIS; break;
		case C_GB18030: branch = "Menu/Options/Encoding/Chinese/" CS_GB18030; break;
		case C_GB2312: branch = "Menu/Options/Encoding/Chinese/" CS_GB2312; break;
		case C_GBK: branch = "Menu/Options/Encoding/Chinese/" CS_GBK; break;
		case C_BIG5: branch = "Menu/Options/Encoding/Chinese/" CS_BIG5; break;
		case C_EUC_TW: branch = "Menu/Options/Encoding/Chinese/" CS_EUC_TW; break;
		case C_EUC_KR: branch = "Menu/Options/Encoding/Korean/" CS_EUC_KR; break;
		case C_ISO_2022_KR: branch = "Menu/Options/Encoding/Korean/" CS_ISO_2022_KR; break;
		case C_TIS_620: branch = "Menu/Options/Encoding/Thai/" CS_TIS_620; break;
		case C_WINDOWS_874: branch = "Menu/Options/Encoding/Thai/" CS_WINDOWS_874; break;
		default: branch = "Menu/Options/Encoding/" CS_AUTO; break;
	}
	cm_toggle_menu_set_active_full(compose->ui_manager, (gchar *)branch, TRUE);
}

static void compose_set_template_menu(Compose *compose)
{
	GSList *tmpl_list, *cur;
	GtkWidget *menu;
	GtkWidget *item;

	tmpl_list = template_get_config();

	menu = gtk_menu_new();

	gtk_menu_set_accel_group (GTK_MENU (menu), 
		gtk_ui_manager_get_accel_group(compose->ui_manager));
	for (cur = tmpl_list; cur != NULL; cur = cur->next) {
		Template *tmpl = (Template *)cur->data;
		gchar *accel_path = NULL;
		item = gtk_menu_item_new_with_label(tmpl->name);
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		g_signal_connect(G_OBJECT(item), "activate",
				 G_CALLBACK(compose_template_activate_cb),
				 compose);
		g_object_set_data(G_OBJECT(item), "template", tmpl);
		gtk_widget_show(item);
		accel_path = g_strconcat("<ComposeTemplates>" , "/", tmpl->name, NULL);
		gtk_menu_item_set_accel_path(GTK_MENU_ITEM(item), accel_path);
		g_free(accel_path);
	}

	gtk_widget_show(menu);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(compose->tmpl_menu), menu);
}

void compose_update_actions_menu(Compose *compose)
{
	action_update_compose_menu(compose->ui_manager, "/Menu/Tools/Actions", compose);
}

static void compose_update_privacy_systems_menu(Compose *compose)
{
	static gchar *branch_path = "/Menu/Options/PrivacySystem";
	GSList *systems, *cur;
	GtkWidget *widget;
	GtkWidget *system_none;
	GSList *group;
	GtkWidget *privacy_menuitem = gtk_ui_manager_get_widget(compose->ui_manager, branch_path);
	GtkWidget *privacy_menu = gtk_menu_new();

	system_none = gtk_radio_menu_item_new_with_mnemonic(NULL, _("_None"));
	g_object_set_data_full(G_OBJECT(system_none), "privacy_system", NULL, NULL);

	g_signal_connect(G_OBJECT(system_none), "activate",
		G_CALLBACK(compose_set_privacy_system_cb), compose);

	gtk_menu_shell_append(GTK_MENU_SHELL(privacy_menu), system_none);
	gtk_widget_show(system_none);

	systems = privacy_get_system_ids();
	for (cur = systems; cur != NULL; cur = g_slist_next(cur)) {
		gchar *systemid = cur->data;

		group = gtk_radio_menu_item_get_group(GTK_RADIO_MENU_ITEM(system_none));
		widget = gtk_radio_menu_item_new_with_label(group,
			privacy_system_get_name(systemid));
		g_object_set_data_full(G_OBJECT(widget), "privacy_system",
				       g_strdup(systemid), g_free);
		g_signal_connect(G_OBJECT(widget), "activate",
			G_CALLBACK(compose_set_privacy_system_cb), compose);

		gtk_menu_shell_append(GTK_MENU_SHELL(privacy_menu), widget);
		gtk_widget_show(widget);
		g_free(systemid);
	}
	g_slist_free(systems);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(privacy_menuitem), privacy_menu);
	gtk_widget_show_all(privacy_menu);
	gtk_widget_show_all(privacy_menuitem);
}

void compose_reflect_prefs_all(void)
{
	GList *cur;
	Compose *compose;

	for (cur = compose_list; cur != NULL; cur = cur->next) {
		compose = (Compose *)cur->data;
		compose_set_template_menu(compose);
	}
}

void compose_reflect_prefs_pixmap_theme(void)
{
	GList *cur;
	Compose *compose;

	for (cur = compose_list; cur != NULL; cur = cur->next) {
		compose = (Compose *)cur->data;
		toolbar_update(TOOLBAR_COMPOSE, compose);
	}
}

static const gchar *compose_quote_char_from_context(Compose *compose)
{
	const gchar *qmark = NULL;

	cm_return_val_if_fail(compose != NULL, NULL);

	switch (compose->mode) {
		/* use forward-specific quote char */
		case COMPOSE_FORWARD:
		case COMPOSE_FORWARD_AS_ATTACH:
		case COMPOSE_FORWARD_INLINE:
			if (compose->folder && compose->folder->prefs &&
					compose->folder->prefs->forward_with_format)
				qmark = compose->folder->prefs->forward_quotemark;
			else if (compose->account->forward_with_format)
				qmark = compose->account->forward_quotemark;
			else
				qmark = prefs_common.fw_quotemark;
			break;

		/* use reply-specific quote char in all other modes */
		default:
			if (compose->folder && compose->folder->prefs &&
					compose->folder->prefs->reply_with_format)
				qmark = compose->folder->prefs->reply_quotemark;
			else if (compose->account->reply_with_format)
				qmark = compose->account->reply_quotemark;
			else
				qmark = prefs_common.quotemark;
			break;
	}

	if (qmark == NULL || *qmark == '\0')
		qmark = "> ";

	return qmark;
}

static void compose_template_apply(Compose *compose, Template *tmpl,
				   gboolean replace)
{
	GtkTextView *text;
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter iter;
	const gchar *qmark;
	gchar *parsed_str = NULL;
	gint cursor_pos = 0;
	const gchar *err_msg = _("The body of the template has an error at line %d.");
	if (!tmpl) return;

	/* process the body */

	text = GTK_TEXT_VIEW(compose->text);
	buffer = gtk_text_view_get_buffer(text);

	if (tmpl->value) {
		qmark = compose_quote_char_from_context(compose);

		if (compose->replyinfo != NULL) {

			if (replace)
				gtk_text_buffer_set_text(buffer, "", -1);
			mark = gtk_text_buffer_get_insert(buffer);
			gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);

			parsed_str = compose_quote_fmt(compose, compose->replyinfo,
						   tmpl->value, qmark, NULL, FALSE, FALSE, err_msg);

		} else if (compose->fwdinfo != NULL) {

			if (replace)
				gtk_text_buffer_set_text(buffer, "", -1);
			mark = gtk_text_buffer_get_insert(buffer);
			gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);

			parsed_str = compose_quote_fmt(compose, compose->fwdinfo,
						   tmpl->value, qmark, NULL, FALSE, FALSE, err_msg);

		} else {
			MsgInfo* dummyinfo = compose_msginfo_new_from_compose(compose);

			GtkTextIter start, end;
			gchar *tmp = NULL;

			gtk_text_buffer_get_start_iter(buffer, &start);
			gtk_text_buffer_get_iter_at_offset(buffer, &end, -1);
			tmp = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);

			/* clear the buffer now */
			if (replace)
				gtk_text_buffer_set_text(buffer, "", -1);

			parsed_str = compose_quote_fmt(compose, dummyinfo,
							   tmpl->value, qmark, tmp, FALSE, FALSE, err_msg);
			procmsg_msginfo_free( dummyinfo );

			g_free( tmp );
		} 
	} else {
		if (replace)
			gtk_text_buffer_set_text(buffer, "", -1);
		mark = gtk_text_buffer_get_insert(buffer);
		gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
	}	

	if (replace && parsed_str && compose->account->auto_sig)
		compose_insert_sig(compose, FALSE);

	if (replace && parsed_str) {
		gtk_text_buffer_get_start_iter(buffer, &iter);
		gtk_text_buffer_place_cursor(buffer, &iter);
	}
	
	if (parsed_str) {
		cursor_pos = quote_fmt_get_cursor_pos();
		compose->set_cursor_pos = cursor_pos;
		if (cursor_pos == -1)
			cursor_pos = 0;
		gtk_text_buffer_get_start_iter(buffer, &iter);
		gtk_text_buffer_get_iter_at_offset(buffer, &iter, cursor_pos);
		gtk_text_buffer_place_cursor(buffer, &iter);
	}

	/* process the other fields */

	compose_template_apply_fields(compose, tmpl);
	compose_attach_from_list(compose, quote_fmt_get_attachments_list(), FALSE);
	quote_fmt_reset_vartable();
	compose_changed_cb(NULL, compose);

#ifdef USE_ENCHANT
	if (compose->gtkaspell && compose->gtkaspell->check_while_typing)
	    	gtkaspell_highlight_all(compose->gtkaspell);
#endif
}

static void compose_template_apply_fields(Compose *compose, Template *tmpl)
{
	MsgInfo* dummyinfo = NULL;
	MsgInfo *msginfo = NULL;
	gchar *buf = NULL;

	if (compose->replyinfo != NULL)
		msginfo = compose->replyinfo;
	else if (compose->fwdinfo != NULL)
		msginfo = compose->fwdinfo;
	else {
		dummyinfo = compose_msginfo_new_from_compose(compose);
		msginfo = dummyinfo;
	}

	if (tmpl->from && *tmpl->from != '\0') {
#ifdef USE_ENCHANT
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
		quote_fmt_scan_string(tmpl->from);
		quote_fmt_parse();

		buf = quote_fmt_get_buffer();
		if (buf == NULL) {
			alertpanel_error(_("Template From format error."));
		} else {
			gtk_entry_set_text(GTK_ENTRY(compose->from_name), buf);
		}
	}

	if (tmpl->to && *tmpl->to != '\0') {
#ifdef USE_ENCHANT
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
		quote_fmt_scan_string(tmpl->to);
		quote_fmt_parse();

		buf = quote_fmt_get_buffer();
		if (buf == NULL) {
			alertpanel_error(_("Template To format error."));
		} else {
			compose_entry_append(compose, buf, COMPOSE_TO, PREF_TEMPLATE);
		}
	}

	if (tmpl->cc && *tmpl->cc != '\0') {
#ifdef USE_ENCHANT
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
		quote_fmt_scan_string(tmpl->cc);
		quote_fmt_parse();

		buf = quote_fmt_get_buffer();
		if (buf == NULL) {
			alertpanel_error(_("Template Cc format error."));
		} else {
			compose_entry_append(compose, buf, COMPOSE_CC, PREF_TEMPLATE);
		}
	}

	if (tmpl->bcc && *tmpl->bcc != '\0') {
#ifdef USE_ENCHANT
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
		quote_fmt_scan_string(tmpl->bcc);
		quote_fmt_parse();

		buf = quote_fmt_get_buffer();
		if (buf == NULL) {
			alertpanel_error(_("Template Bcc format error."));
		} else {
			compose_entry_append(compose, buf, COMPOSE_BCC, PREF_TEMPLATE);
		}
	}

	/* process the subject */
	if (tmpl->subject && *tmpl->subject != '\0') {
#ifdef USE_ENCHANT
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE,
				compose->gtkaspell);
#else
		quote_fmt_init(msginfo, NULL, NULL, FALSE, compose->account, FALSE);
#endif
		quote_fmt_scan_string(tmpl->subject);
		quote_fmt_parse();

		buf = quote_fmt_get_buffer();
		if (buf == NULL) {
			alertpanel_error(_("Template subject format error."));
		} else {
			gtk_entry_set_text(GTK_ENTRY(compose->subject_entry), buf);
		}
	}

	procmsg_msginfo_free( dummyinfo );
}

static void compose_destroy(Compose *compose)
{
	GtkTextBuffer *buffer;
	GtkClipboard *clipboard;

	compose_list = g_list_remove(compose_list, compose);

	if (compose->updating) {
		debug_print("danger, not destroying anything now\n");
		compose->deferred_destroy = TRUE;
		return;
	}
	/* NOTE: address_completion_end() does nothing with the window
	 * however this may change. */
	address_completion_end(compose->window);

	slist_free_strings(compose->to_list);
	g_slist_free(compose->to_list);
	slist_free_strings(compose->newsgroup_list);
	g_slist_free(compose->newsgroup_list);
	slist_free_strings(compose->header_list);
	g_slist_free(compose->header_list);

	compose->header_list = compose->newsgroup_list = compose->to_list = NULL;

	g_hash_table_destroy(compose->email_hashtable);

	procmsg_msginfo_free(compose->targetinfo);
	procmsg_msginfo_free(compose->replyinfo);
	procmsg_msginfo_free(compose->fwdinfo);

	g_free(compose->replyto);
	g_free(compose->cc);
	g_free(compose->bcc);
	g_free(compose->newsgroups);
	g_free(compose->followup_to);

	g_free(compose->ml_post);

	g_free(compose->inreplyto);
	g_free(compose->references);
	g_free(compose->msgid);
	g_free(compose->boundary);

	g_free(compose->redirect_filename);
	if (compose->undostruct)
		undo_destroy(compose->undostruct);

	g_free(compose->sig_str);

	g_free(compose->exteditor_file);

	g_free(compose->orig_charset);

	g_free(compose->privacy_system);

	if (addressbook_get_target_compose() == compose)
		addressbook_set_target_compose(NULL);

#if USE_ENCHANT
        if (compose->gtkaspell) {
	        gtkaspell_delete(compose->gtkaspell);
		compose->gtkaspell = NULL;
        }
#endif

	if (!compose->batch) {
		prefs_common.compose_width = compose->scrolledwin->allocation.width;
		prefs_common.compose_height = compose->window->allocation.height;
	}

	if (!gtk_widget_get_parent(compose->paned))
		gtk_widget_destroy(compose->paned);
	gtk_widget_destroy(compose->popupmenu);

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(compose->text));
	clipboard = gtk_clipboard_get(GDK_SELECTION_PRIMARY);
	gtk_text_buffer_remove_selection_clipboard(buffer, clipboard);

	gtk_widget_destroy(compose->window);
	toolbar_destroy(compose->toolbar);
	g_free(compose->toolbar);
	g_mutex_free(compose->mutex);
	g_free(compose);
}

static void compose_attach_info_free(AttachInfo *ainfo)
{
	g_free(ainfo->file);
	g_free(ainfo->content_type);
	g_free(ainfo->name);
	g_free(ainfo->charset);
	g_free(ainfo);
}

static void compose_attach_update_label(Compose *compose)
{
	GtkTreeIter iter;
	gint i = 1;
	gchar *text;
	GtkTreeModel *model;
	
	if(compose == NULL)
		return;
		
	model = gtk_tree_view_get_model(GTK_TREE_VIEW(compose->attach_clist));
	if(!gtk_tree_model_get_iter_first(model, &iter)) {
		gtk_label_set_text(GTK_LABEL(compose->attach_label), "");	
		return;
	}
	
	while(gtk_tree_model_iter_next(model, &iter))
		i++;
	
	text = g_strdup_printf("(%d)", i);
	gtk_label_set_text(GTK_LABEL(compose->attach_label), text);
	g_free(text);
}

static void compose_attach_remove_selected(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	GtkTreeView *tree_view = GTK_TREE_VIEW(compose->attach_clist);
	GtkTreeSelection *selection;
	GList *sel, *cur;
	GtkTreeModel *model;

	selection = gtk_tree_view_get_selection(tree_view);
	sel = gtk_tree_selection_get_selected_rows(selection, &model);

	if (!sel) 
		return;

	for (cur = sel; cur != NULL; cur = cur->next) {
		GtkTreePath *path = cur->data;
		GtkTreeRowReference *ref = gtk_tree_row_reference_new
						(model, cur->data);
		cur->data = ref;
		gtk_tree_path_free(path);
	}

	for (cur = sel; cur != NULL; cur = cur->next) {
		GtkTreeRowReference *ref = cur->data;
		GtkTreePath *path = gtk_tree_row_reference_get_path(ref);
		GtkTreeIter iter;

		if (gtk_tree_model_get_iter(model, &iter, path))
			gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
		
		gtk_tree_path_free(path);
		gtk_tree_row_reference_free(ref);
	}

	g_list_free(sel);
	compose_attach_update_label(compose);
}

static struct _AttachProperty
{
	GtkWidget *window;
	GtkWidget *mimetype_entry;
	GtkWidget *encoding_optmenu;
	GtkWidget *path_entry;
	GtkWidget *filename_entry;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
} attach_prop;

static void gtk_tree_path_free_(gpointer ptr, gpointer data)
{	
	gtk_tree_path_free((GtkTreePath *)ptr);
}

static void compose_attach_property(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	GtkTreeView *tree_view = GTK_TREE_VIEW(compose->attach_clist);
	AttachInfo *ainfo;
	GtkComboBox *optmenu;
	GtkTreeSelection *selection;
	GList *sel;
	GtkTreeModel *model;
	GtkTreeIter iter;
	GtkTreePath *path;
	static gboolean cancelled;

	/* only if one selected */
	selection = gtk_tree_view_get_selection(tree_view);
	if (gtk_tree_selection_count_selected_rows(selection) != 1) 
		return;

	sel = gtk_tree_selection_get_selected_rows(selection, &model);
	if (!sel)
		return;

	path = (GtkTreePath *) sel->data;
	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_model_get(model, &iter, COL_DATA, &ainfo, -1); 
	
	if (!ainfo) {
		g_list_foreach(sel, gtk_tree_path_free_, NULL);
		g_list_free(sel);
		return;
	}		
	g_list_free(sel);

	if (!attach_prop.window)
		compose_attach_property_create(&cancelled);
	gtk_window_set_modal(GTK_WINDOW(attach_prop.window), TRUE);
	gtk_widget_grab_focus(attach_prop.ok_btn);
	gtk_widget_show(attach_prop.window);
	manage_window_set_transient(GTK_WINDOW(attach_prop.window));

	optmenu = GTK_COMBO_BOX(attach_prop.encoding_optmenu);
	if (ainfo->encoding == ENC_UNKNOWN)
		combobox_select_by_data(optmenu, ENC_BASE64);
	else
		combobox_select_by_data(optmenu, ainfo->encoding);

	gtk_entry_set_text(GTK_ENTRY(attach_prop.mimetype_entry),
			   ainfo->content_type ? ainfo->content_type : "");
	gtk_entry_set_text(GTK_ENTRY(attach_prop.path_entry),
			   ainfo->file ? ainfo->file : "");
	gtk_entry_set_text(GTK_ENTRY(attach_prop.filename_entry),
			   ainfo->name ? ainfo->name : "");

	for (;;) {
		const gchar *entry_text;
		gchar *text;
		gchar *cnttype = NULL;
		gchar *file = NULL;
		off_t size = 0;

		cancelled = FALSE;
		gtk_main();

		gtk_widget_hide(attach_prop.window);
		gtk_window_set_modal(GTK_WINDOW(attach_prop.window), FALSE);
		
		if (cancelled) 
			break;

		entry_text = gtk_entry_get_text(GTK_ENTRY(attach_prop.mimetype_entry));
		if (*entry_text != '\0') {
			gchar *p;

			text = g_strstrip(g_strdup(entry_text));
			if ((p = strchr(text, '/')) && !strchr(p + 1, '/')) {
				cnttype = g_strdup(text);
				g_free(text);
			} else {
				alertpanel_error(_("Invalid MIME type."));
				g_free(text);
				continue;
			}
		}

		ainfo->encoding = combobox_get_active_data(optmenu);

		entry_text = gtk_entry_get_text(GTK_ENTRY(attach_prop.path_entry));
		if (*entry_text != '\0') {
			if (is_file_exist(entry_text) &&
			    (size = get_file_size(entry_text)) > 0)
				file = g_strdup(entry_text);
			else {
				alertpanel_error
					(_("File doesn't exist or is empty."));
				g_free(cnttype);
				continue;
			}
		}

		entry_text = gtk_entry_get_text(GTK_ENTRY(attach_prop.filename_entry));
		if (*entry_text != '\0') {
			g_free(ainfo->name);
			ainfo->name = g_strdup(entry_text);
		}

		if (cnttype) {
			g_free(ainfo->content_type);
			ainfo->content_type = cnttype;
		}
		if (file) {
			g_free(ainfo->file);
			ainfo->file = file;
		}
		if (size)
			ainfo->size = (goffset)size;

		/* update tree store */
		text = to_human_readable(ainfo->size);
		gtk_tree_model_get_iter(model, &iter, path);
		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
				   COL_MIMETYPE, ainfo->content_type,
				   COL_SIZE, text,
				   COL_NAME, ainfo->name,
				   COL_CHARSET, ainfo->charset,
				   -1);
		
		break;
	}

	gtk_tree_path_free(path);
}

#define SET_LABEL_AND_ENTRY(str, entry, top) \
{ \
	label = gtk_label_new(str); \
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, top, (top + 1), \
			 GTK_FILL, 0, 0, 0); \
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5); \
 \
	entry = gtk_entry_new(); \
	gtk_table_attach(GTK_TABLE(table), entry, 1, 2, top, (top + 1), \
			 GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0); \
}

static void compose_attach_property_create(gboolean *cancelled)
{
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *mimetype_entry;
	GtkWidget *hbox;
	GtkWidget *optmenu;
	GtkListStore *optmenu_menu;
	GtkWidget *path_entry;
	GtkWidget *filename_entry;
	GtkWidget *hbbox;
	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GList     *mime_type_list, *strlist;
	GtkTreeIter iter;

	debug_print("Creating attach_property window...\n");

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "compose_attach_property");
	gtk_widget_set_size_request(window, 480, -1);
	gtk_container_set_border_width(GTK_CONTAINER(window), 8);
	gtk_window_set_title(GTK_WINDOW(window), _("Properties"));
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(attach_property_delete_event),
			 cancelled);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(attach_property_key_pressed),
			 cancelled);

	vbox = gtk_vbox_new(FALSE, 8);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	table = gtk_table_new(4, 2, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);
	gtk_table_set_row_spacings(GTK_TABLE(table), 8);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	label = gtk_label_new(_("MIME type")); 
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 0, (0 + 1), 
			 GTK_FILL, 0, 0, 0); 
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5); 
	mimetype_entry = gtk_combo_box_entry_new_text(); 
	gtk_table_attach(GTK_TABLE(table), mimetype_entry, 1, 2, 0, (0 + 1), 
			 GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);
			 
	/* stuff with list */
	mime_type_list = procmime_get_mime_type_list();
	strlist = NULL;
	for (; mime_type_list != NULL; mime_type_list = mime_type_list->next) {
		MimeType *type = (MimeType *) mime_type_list->data;
		gchar *tmp;

		tmp = g_strdup_printf("%s/%s", type->type, type->sub_type);

		if (g_list_find_custom(strlist, tmp, (GCompareFunc)strcmp2))
			g_free(tmp);
		else
			strlist = g_list_insert_sorted(strlist, (gpointer)tmp,
					(GCompareFunc)strcmp2);
	}

	for (mime_type_list = strlist; mime_type_list != NULL; 
		mime_type_list = mime_type_list->next) {
		gtk_combo_box_append_text(GTK_COMBO_BOX(mimetype_entry), mime_type_list->data);
		g_free(mime_type_list->data);
	}
	g_list_free(strlist);
	gtk_combo_box_set_active(GTK_COMBO_BOX(mimetype_entry), 0);		 
	mimetype_entry = gtk_bin_get_child(GTK_BIN((mimetype_entry)));			 

	label = gtk_label_new(_("Encoding"));
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 1, 2,
			 GTK_FILL, 0, 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_table_attach(GTK_TABLE(table), hbox, 1, 2, 1, 2,
			 GTK_EXPAND|GTK_SHRINK|GTK_FILL, 0, 0, 0);

	optmenu = gtkut_sc_combobox_create(NULL, TRUE);
	optmenu_menu = GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(optmenu)));

	COMBOBOX_ADD(optmenu_menu, "7bit", ENC_7BIT);
	COMBOBOX_ADD(optmenu_menu, "8bit", ENC_8BIT);
	COMBOBOX_ADD(optmenu_menu, "quoted-printable",	ENC_QUOTED_PRINTABLE);
	COMBOBOX_ADD(optmenu_menu, "base64", ENC_BASE64);
	gtk_combo_box_set_active(GTK_COMBO_BOX(optmenu), 0);

	gtk_box_pack_start(GTK_BOX(hbox), optmenu, TRUE, TRUE, 0);

	SET_LABEL_AND_ENTRY(_("Path"),      path_entry,     2);
	SET_LABEL_AND_ENTRY(_("File name"), filename_entry, 3);

	gtkut_stock_button_set_create(&hbbox, &cancel_btn, GTK_STOCK_CANCEL,
				      &ok_btn, GTK_STOCK_OK,
				      NULL, NULL);
	gtk_box_pack_end(GTK_BOX(vbox), hbbox, FALSE, FALSE, 0);
	gtk_widget_grab_default(ok_btn);

	g_signal_connect(G_OBJECT(ok_btn), "clicked",
			 G_CALLBACK(attach_property_ok),
			 cancelled);
	g_signal_connect(G_OBJECT(cancel_btn), "clicked",
			 G_CALLBACK(attach_property_cancel),
			 cancelled);

	gtk_widget_show_all(vbox);

	attach_prop.window           = window;
	attach_prop.mimetype_entry   = mimetype_entry;
	attach_prop.encoding_optmenu = optmenu;
	attach_prop.path_entry       = path_entry;
	attach_prop.filename_entry   = filename_entry;
	attach_prop.ok_btn           = ok_btn;
	attach_prop.cancel_btn       = cancel_btn;
}

#undef SET_LABEL_AND_ENTRY

static void attach_property_ok(GtkWidget *widget, gboolean *cancelled)
{
	*cancelled = FALSE;
	gtk_main_quit();
}

static void attach_property_cancel(GtkWidget *widget, gboolean *cancelled)
{
	*cancelled = TRUE;
	gtk_main_quit();
}

static gint attach_property_delete_event(GtkWidget *widget, GdkEventAny *event,
					 gboolean *cancelled)
{
	*cancelled = TRUE;
	gtk_main_quit();

	return TRUE;
}

static gboolean attach_property_key_pressed(GtkWidget *widget,
					    GdkEventKey *event,
					    gboolean *cancelled)
{
	if (event && event->keyval == GDK_Escape) {
		*cancelled = TRUE;
		gtk_main_quit();
	}
	if (event && event->keyval == GDK_Return) {
		*cancelled = FALSE;
		gtk_main_quit();
		return TRUE;
	}
	return FALSE;
}

static void compose_exec_ext_editor(Compose *compose)
{
#ifdef G_OS_UNIX
	gchar *tmp;
	pid_t pid;
	gint pipe_fds[2];

	tmp = g_strdup_printf("%s%ctmpmsg.%p", get_tmp_dir(),
			      G_DIR_SEPARATOR, compose);

	if (pipe(pipe_fds) < 0) {
		perror("pipe");
		g_free(tmp);
		return;
	}

	if ((pid = fork()) < 0) {
		perror("fork");
		g_free(tmp);
		return;
	}

	if (pid != 0) {
		/* close the write side of the pipe */
		close(pipe_fds[1]);

		compose->exteditor_file    = g_strdup(tmp);
		compose->exteditor_pid     = pid;

		compose_set_ext_editor_sensitive(compose, FALSE);

#ifndef G_OS_WIN32
		compose->exteditor_ch = g_io_channel_unix_new(pipe_fds[0]);
#else
		compose->exteditor_ch = g_io_channel_win32_new_fd(pipe_fds[0]);
#endif
		compose->exteditor_tag = g_io_add_watch(compose->exteditor_ch,
							G_IO_IN,
							compose_input_cb,
							compose);
	} else {	/* process-monitoring process */
		pid_t pid_ed;

		if (setpgid(0, 0))
			perror("setpgid");

		/* close the read side of the pipe */
		close(pipe_fds[0]);

		if (compose_write_body_to_file(compose, tmp) < 0) {
			fd_write_all(pipe_fds[1], "2\n", 2);
			_exit(1);
		}

		pid_ed = compose_exec_ext_editor_real(tmp);
		if (pid_ed < 0) {
			fd_write_all(pipe_fds[1], "1\n", 2);
			_exit(1);
		}

		/* wait until editor is terminated */
		waitpid(pid_ed, NULL, 0);

		fd_write_all(pipe_fds[1], "0\n", 2);

		close(pipe_fds[1]);
		_exit(0);
	}

	g_free(tmp);
#endif /* G_OS_UNIX */
}

#ifdef G_OS_UNIX
static gint compose_exec_ext_editor_real(const gchar *file)
{
	gchar buf[1024];
	gchar *p;
	gchar **cmdline;
	pid_t pid;

	cm_return_val_if_fail(file != NULL, -1);

	if ((pid = fork()) < 0) {
		perror("fork");
		return -1;
	}

	if (pid != 0) return pid;

	/* grandchild process */

	if (setpgid(0, getppid()))
		perror("setpgid");

	if (prefs_common_get_ext_editor_cmd() &&
	    (p = strchr(prefs_common_get_ext_editor_cmd(), '%')) &&
	    *(p + 1) == 's' && !strchr(p + 2, '%')) {
		g_snprintf(buf, sizeof(buf), prefs_common_get_ext_editor_cmd(), file);
	} else {
		if (prefs_common_get_ext_editor_cmd())
			g_warning("External editor command-line is invalid: '%s'\n",
				  prefs_common_get_ext_editor_cmd());
		g_snprintf(buf, sizeof(buf), DEFAULT_EDITOR_CMD, file);
	}

	cmdline = strsplit_with_quote(buf, " ", 1024);
	execvp(cmdline[0], cmdline);

	perror("execvp");
	g_strfreev(cmdline);

	_exit(1);
}

static gboolean compose_ext_editor_kill(Compose *compose)
{
	pid_t pgid = compose->exteditor_pid * -1;
	gint ret;

	ret = kill(pgid, 0);

	if (ret == 0 || (ret == -1 && EPERM == errno)) {
		AlertValue val;
		gchar *msg;

		msg = g_strdup_printf
			(_("The external editor is still working.\n"
			   "Force terminating the process?\n"
			   "process group id: %d"), -pgid);
		val = alertpanel_full(_("Notice"), msg, GTK_STOCK_NO, GTK_STOCK_YES,
		      		      NULL, FALSE, NULL, ALERT_WARNING, G_ALERTDEFAULT);
			
		g_free(msg);

		if (val == G_ALERTALTERNATE) {
			g_source_remove(compose->exteditor_tag);
			g_io_channel_shutdown(compose->exteditor_ch,
					      FALSE, NULL);
			g_io_channel_unref(compose->exteditor_ch);

			if (kill(pgid, SIGTERM) < 0) perror("kill");
			waitpid(compose->exteditor_pid, NULL, 0);

			g_warning("Terminated process group id: %d", -pgid);
			g_warning("Temporary file: %s",
				  compose->exteditor_file);

			compose_set_ext_editor_sensitive(compose, TRUE);

			g_free(compose->exteditor_file);
			compose->exteditor_file    = NULL;
			compose->exteditor_pid     = -1;
			compose->exteditor_ch      = NULL;
			compose->exteditor_tag     = -1;
		} else
			return FALSE;
	}

	return TRUE;
}

static gboolean compose_input_cb(GIOChannel *source, GIOCondition condition,
				 gpointer data)
{
	gchar buf[3] = "3";
	Compose *compose = (Compose *)data;
	gsize bytes_read;

	debug_print("Compose: input from monitoring process\n");

	g_io_channel_read_chars(source, buf, sizeof(buf), &bytes_read, NULL);

	g_io_channel_shutdown(source, FALSE, NULL);
	g_io_channel_unref(source);

	waitpid(compose->exteditor_pid, NULL, 0);

	if (buf[0] == '0') {		/* success */
		GtkTextView *text = GTK_TEXT_VIEW(compose->text);
		GtkTextBuffer *buffer = gtk_text_view_get_buffer(text);

		gtk_text_buffer_set_text(buffer, "", -1);
		compose_insert_file(compose, compose->exteditor_file);
		compose_changed_cb(NULL, compose);
		compose_draft((gpointer)compose, COMPOSE_AUTO_SAVE);

		if (claws_unlink(compose->exteditor_file) < 0)
			FILE_OP_ERROR(compose->exteditor_file, "unlink");
	} else if (buf[0] == '1') {	/* failed */
		g_warning("Couldn't exec external editor\n");
		if (claws_unlink(compose->exteditor_file) < 0)
			FILE_OP_ERROR(compose->exteditor_file, "unlink");
	} else if (buf[0] == '2') {
		g_warning("Couldn't write to file\n");
	} else if (buf[0] == '3') {
		g_warning("Pipe read failed\n");
	}

	compose_set_ext_editor_sensitive(compose, TRUE);

	g_free(compose->exteditor_file);
	compose->exteditor_file    = NULL;
	compose->exteditor_pid     = -1;
	compose->exteditor_ch      = NULL;
	compose->exteditor_tag     = -1;

	return FALSE;
}

static void compose_set_ext_editor_sensitive(Compose *compose,
					     gboolean sensitive)
{
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message/Send", sensitive);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message/SendLater", sensitive);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message/InsertFile", sensitive);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message/InsertSig", sensitive);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/WrapPara", sensitive);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/WrapAllLines", sensitive);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/ExtEditor", sensitive);

	gtk_widget_set_sensitive(compose->text,                       sensitive);
	if (compose->toolbar->send_btn)
		gtk_widget_set_sensitive(compose->toolbar->send_btn,      sensitive);
	if (compose->toolbar->sendl_btn)
		gtk_widget_set_sensitive(compose->toolbar->sendl_btn,     sensitive);
	if (compose->toolbar->draft_btn)
		gtk_widget_set_sensitive(compose->toolbar->draft_btn,     sensitive);
	if (compose->toolbar->insert_btn)
		gtk_widget_set_sensitive(compose->toolbar->insert_btn,    sensitive);
	if (compose->toolbar->sig_btn)
		gtk_widget_set_sensitive(compose->toolbar->sig_btn,       sensitive);
	if (compose->toolbar->exteditor_btn)
		gtk_widget_set_sensitive(compose->toolbar->exteditor_btn, sensitive);
	if (compose->toolbar->linewrap_current_btn)
		gtk_widget_set_sensitive(compose->toolbar->linewrap_current_btn, sensitive);
	if (compose->toolbar->linewrap_all_btn)
		gtk_widget_set_sensitive(compose->toolbar->linewrap_all_btn, sensitive);
}
#endif /* G_OS_UNIX */

/**
 * compose_undo_state_changed:
 *
 * Change the sensivity of the menuentries undo and redo
 **/
static void compose_undo_state_changed(UndoMain *undostruct, gint undo_state,
				       gint redo_state, gpointer data)
{
	Compose *compose = (Compose *)data;

	switch (undo_state) {
	case UNDO_STATE_TRUE:
		if (!undostruct->undo_state) {
			undostruct->undo_state = TRUE;
			cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/Undo", TRUE);
		}
		break;
	case UNDO_STATE_FALSE:
		if (undostruct->undo_state) {
			undostruct->undo_state = FALSE;
			cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/Undo", FALSE);
		}
		break;
	case UNDO_STATE_UNCHANGED:
		break;
	case UNDO_STATE_REFRESH:
		cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/Undo", undostruct->undo_state);
		break;
	default:
		g_warning("Undo state not recognized");
		break;
	}

	switch (redo_state) {
	case UNDO_STATE_TRUE:
		if (!undostruct->redo_state) {
			undostruct->redo_state = TRUE;
			cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/Redo", TRUE);
		}
		break;
	case UNDO_STATE_FALSE:
		if (undostruct->redo_state) {
			undostruct->redo_state = FALSE;
			cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/Redo", FALSE);
		}
		break;
	case UNDO_STATE_UNCHANGED:
		break;
	case UNDO_STATE_REFRESH:
		cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit/Redo", undostruct->redo_state);
		break;
	default:
		g_warning("Redo state not recognized");
		break;
	}
}

/* callback functions */

/* compose_edit_size_alloc() - called when resized. don't know whether Gtk
 * includes "non-client" (windows-izm) in calculation, so this calculation
 * may not be accurate.
 */
static gboolean compose_edit_size_alloc(GtkEditable *widget,
					GtkAllocation *allocation,
					GtkSHRuler *shruler)
{
	if (prefs_common.show_ruler) {
		gint char_width = 0, char_height = 0;
		gint line_width_in_chars;

		gtkut_get_font_size(GTK_WIDGET(widget),
				    &char_width, &char_height);
		line_width_in_chars =
			(allocation->width - allocation->x) / char_width;

		/* got the maximum */
		gtk_ruler_set_range(GTK_RULER(shruler),
				    0.0, line_width_in_chars, 0,
				    /*line_width_in_chars*/ char_width);
	}

	return TRUE;
}

typedef struct {
	gchar 			*header;
	gchar 			*entry;
	ComposePrefType		type;
	gboolean		entry_marked;
} HeaderEntryState;

static void account_activated(GtkComboBox *optmenu, gpointer data)
{
	Compose *compose = (Compose *)data;

	PrefsAccount *ac;
	gchar *folderidentifier;
	gint account_id = 0;
	GtkTreeModel *menu;
	GtkTreeIter iter;
	GSList *list, *saved_list = NULL;
	HeaderEntryState *state;
	GtkRcStyle *style = NULL;
	static GdkColor yellow;
	static gboolean color_set = FALSE;

	/* Get ID of active account in the combo box */
	menu = gtk_combo_box_get_model(optmenu);
	gtk_combo_box_get_active_iter(optmenu, &iter);
	gtk_tree_model_get(menu, &iter, 1, &account_id, -1);

	ac = account_find_from_id(account_id);
	cm_return_if_fail(ac != NULL);

	if (ac != compose->account) {
		compose_select_account(compose, ac, FALSE);

		for (list = compose->header_list; list; list = list->next) {
			ComposeHeaderEntry *hentry=(ComposeHeaderEntry *)list->data;
			
			if (hentry->type == PREF_ACCOUNT || !list->next) {
				compose_destroy_headerentry(compose, hentry);
				continue;
			}
			
			state = g_malloc0(sizeof(HeaderEntryState));
			state->header = gtk_editable_get_chars(GTK_EDITABLE(
					gtk_bin_get_child(GTK_BIN(hentry->combo))), 0, -1);
			state->entry = gtk_editable_get_chars(
					GTK_EDITABLE(hentry->entry), 0, -1);
			state->type = hentry->type;
				
			if (!color_set) {
				gdk_color_parse("#f5f6be", &yellow);
				color_set = gdk_colormap_alloc_color(
							gdk_colormap_get_system(),
							&yellow, FALSE, TRUE);
			}
				
			style = gtk_widget_get_modifier_style(hentry->entry);
			state->entry_marked = gdk_color_equal(&yellow,
						&style->base[GTK_STATE_NORMAL]);

			saved_list = g_slist_append(saved_list, state);
			compose_destroy_headerentry(compose, hentry);
		}

		compose->header_last = NULL;
		g_slist_free(compose->header_list);
		compose->header_list = NULL;
		compose->header_nextrow = 1;
		compose_create_header_entry(compose);
		
		if (ac->set_autocc && ac->auto_cc)
			compose_entry_append(compose, ac->auto_cc,
						COMPOSE_CC, PREF_ACCOUNT);

		if (ac->set_autobcc && ac->auto_bcc) 
			compose_entry_append(compose, ac->auto_bcc,
						COMPOSE_BCC, PREF_ACCOUNT);
	
		if (ac->set_autoreplyto && ac->auto_replyto)
			compose_entry_append(compose, ac->auto_replyto,
						COMPOSE_REPLYTO, PREF_ACCOUNT);
		
		for (list = saved_list; list; list = list->next) {
			state = (HeaderEntryState *) list->data;
			
			compose_add_header_entry(compose, state->header,
						state->entry, state->type);
			if (state->entry_marked)
				compose_entry_mark_default_to(compose, state->entry);
			
			g_free(state->header);	
			g_free(state->entry);
			g_free(state);
		}
		g_slist_free(saved_list);
		
		combobox_select_by_data(GTK_COMBO_BOX(compose->header_last->combo),
					(ac->protocol == A_NNTP) ? 
					COMPOSE_NEWSGROUPS : COMPOSE_TO);
	}

	/* Set message save folder */
	if (account_get_special_folder(compose->account, F_OUTBOX)) {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn), prefs_common.savemsg);
	}
	g_signal_connect(G_OBJECT(compose->savemsg_checkbtn), "toggled",
			 G_CALLBACK(compose_savemsg_checkbtn_cb), compose);
			   
	compose_set_save_to(compose, NULL);
	if (account_get_special_folder(compose->account, F_OUTBOX)) {
		folderidentifier = folder_item_get_identifier(account_get_special_folder
				  (compose->account, F_OUTBOX));
		compose_set_save_to(compose, folderidentifier);
		g_free(folderidentifier);
	}
}

static void attach_selected(GtkTreeView *tree_view, GtkTreePath *tree_path,
			    GtkTreeViewColumn *column, Compose *compose)
{
	compose_attach_property(NULL, compose);
}

static gboolean attach_button_pressed(GtkWidget *widget, GdkEventButton *event,
				      gpointer data)
{
	Compose *compose = (Compose *)data;
	GtkTreeSelection *attach_selection;
	gint attach_nr_selected;
	
	if (!event) return FALSE;

	if (event->button == 3) {
		attach_selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
		attach_nr_selected = gtk_tree_selection_count_selected_rows(attach_selection);
			
		if (attach_nr_selected > 0)
		{
			cm_menu_set_sensitive_full(compose->ui_manager, "Popup/Compose/Remove", TRUE);
			cm_menu_set_sensitive_full(compose->ui_manager, "Popup/Compose/Properties", TRUE);
		} else {
			cm_menu_set_sensitive_full(compose->ui_manager, "Popup/Compose/Remove", FALSE);
			cm_menu_set_sensitive_full(compose->ui_manager, "Popup/Compose/Properties", FALSE);
		}
			
		gtk_menu_popup(GTK_MENU(compose->popupmenu), NULL, NULL,
			       NULL, NULL, event->button, event->time);
		return TRUE;			       
	}

	return FALSE;
}

static gboolean attach_key_pressed(GtkWidget *widget, GdkEventKey *event,
				   gpointer data)
{
	Compose *compose = (Compose *)data;

	if (!event) return FALSE;

	switch (event->keyval) {
	case GDK_Delete:
		compose_attach_remove_selected(NULL, compose);
		break;
	}
	return FALSE;
}

static void compose_allow_user_actions (Compose *compose, gboolean allow)
{
	toolbar_comp_set_sensitive(compose, allow);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Message", allow);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Edit", allow);
#if USE_ENCHANT
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Spelling", allow);
#endif	
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Options", allow);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Tools", allow);
	cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Help", allow);
	
	gtk_text_view_set_editable(GTK_TEXT_VIEW(compose->text), allow);

}

static void compose_send_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	if (prefs_common.work_offline && 
	    !inc_offline_should_override(TRUE,
		_("Claws Mail needs network access in order "
		  "to send this email.")))
		return;
	
	if (compose->draft_timeout_tag >= 0) { /* CLAWS: disable draft timeout */
		g_source_remove(compose->draft_timeout_tag);
		compose->draft_timeout_tag = -1;
	}

	compose_send(compose);
}

static void compose_send_later_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	gint val;

	inc_lock();
	compose_allow_user_actions(compose, FALSE);
	val = compose_queue_sub(compose, NULL, NULL, NULL, TRUE, TRUE);
	compose_allow_user_actions(compose, TRUE);
	inc_unlock();

	if (!val) {
		compose_close(compose);
	} else if (val == -1) {
		alertpanel_error(_("Could not queue message."));
	} else if (val == -2) {
		alertpanel_error(_("Could not queue message:\n\n%s."), strerror(errno));
	} else if (val == -3) {
		if (privacy_peek_error())
		alertpanel_error(_("Could not queue message for sending:\n\n"
				   "Signature failed: %s"), privacy_get_error());
	} else if (val == -4) {
		alertpanel_error(_("Could not queue message for sending:\n\n"
				   "Charset conversion failed."));
	} else if (val == -5) {
		alertpanel_error(_("Could not queue message for sending:\n\n"
				   "Couldn't get recipient encryption key."));
	} else if (val == -6) {
		/* silent error */
	}
	toolbar_main_set_sensitive(mainwindow_get_mainwindow());
}

#define DRAFTED_AT_EXIT "drafted_at_exit"
static void compose_register_draft(MsgInfo *info)
{
	gchar *filepath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
				      DRAFTED_AT_EXIT, NULL);
	FILE *fp = g_fopen(filepath, "ab");
	
	if (fp) {
		fprintf(fp, "%s\t%d\n", folder_item_get_identifier(info->folder), 
				info->msgnum);
		fclose(fp);
	}
		
	g_free(filepath);	
}

gboolean compose_draft (gpointer data, guint action) 
{
	Compose *compose = (Compose *)data;
	FolderItem *draft;
	gchar *tmp;
	gint msgnum;
	MsgFlags flag = {0, 0};
	static gboolean lock = FALSE;
	MsgInfo *newmsginfo;
	FILE *fp;
	gboolean target_locked = FALSE;
	gboolean err = FALSE;

	if (lock) return FALSE;

	if (compose->sending)
		return TRUE;

	draft = account_get_special_folder(compose->account, F_DRAFT);
	cm_return_val_if_fail(draft != NULL, FALSE);
	
	if (!g_mutex_trylock(compose->mutex)) {
		/* we don't want to lock the mutex once it's available,
		 * because as the only other part of compose.c locking
		 * it is compose_close - which means once unlocked,
		 * the compose struct will be freed */
		debug_print("couldn't lock mutex, probably sending\n");
		return FALSE;
	}
	
	lock = TRUE;

	tmp = g_strdup_printf("%s%cdraft.%p", get_tmp_dir(),
			      G_DIR_SEPARATOR, compose);
	if ((fp = g_fopen(tmp, "wb")) == NULL) {
		FILE_OP_ERROR(tmp, "fopen");
		goto warn_err;
	}

	/* chmod for security */
	if (change_file_mode_rw(fp, tmp) < 0) {
		FILE_OP_ERROR(tmp, "chmod");
		g_warning("can't change file mode\n");
	}

	/* Save draft infos */
	err |= (fprintf(fp, "X-Claws-Account-Id:%d\n", compose->account->account_id) < 0);
	err |= (fprintf(fp, "S:%s\n", compose->account->address) < 0);

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(compose->savemsg_checkbtn))) {
		gchar *savefolderid;

		savefolderid = compose_get_save_to(compose);
		err |= (fprintf(fp, "SCF:%s\n", savefolderid) < 0);
		g_free(savefolderid);
	}
	if (compose->return_receipt) {
		err |= (fprintf(fp, "RRCPT:1\n") < 0);
	}
	if (compose->privacy_system) {
		err |= (fprintf(fp, "X-Claws-Sign:%d\n", compose->use_signing) < 0);
		err |= (fprintf(fp, "X-Claws-Encrypt:%d\n", compose->use_encryption) < 0);
		err |= (fprintf(fp, "X-Claws-Privacy-System:%s\n", compose->privacy_system) < 0);
	}

	/* Message-ID of message replying to */
	if ((compose->replyinfo != NULL) && (compose->replyinfo->msgid != NULL)) {
		gchar *folderid;
		
		folderid = folder_item_get_identifier(compose->replyinfo->folder);
		err |= (fprintf(fp, "RMID:%s\t%d\t%s\n", folderid, compose->replyinfo->msgnum, compose->replyinfo->msgid) < 0);
		g_free(folderid);
	}
	/* Message-ID of message forwarding to */
	if ((compose->fwdinfo != NULL) && (compose->fwdinfo->msgid != NULL)) {
		gchar *folderid;
		
		folderid = folder_item_get_identifier(compose->fwdinfo->folder);
		err |= (fprintf(fp, "FMID:%s\t%d\t%s\n", folderid, compose->fwdinfo->msgnum, compose->fwdinfo->msgid) < 0);
		g_free(folderid);
	}

	err |= (fprintf(fp, "X-Claws-Auto-Wrapping:%d\n", compose->autowrap) < 0);
	err |= (fprintf(fp, "X-Claws-Auto-Indent:%d\n", compose->autoindent) < 0);

	/* end of headers */
	err |= (fprintf(fp, "X-Claws-End-Special-Headers: 1\n") < 0);

	if (err) {
		fclose(fp);
		goto warn_err;
	}

	if (compose_write_to_file(compose, fp, COMPOSE_WRITE_FOR_STORE, action != COMPOSE_AUTO_SAVE) < 0) {
		fclose(fp);
		goto warn_err;
	}
	if (fclose(fp) == EOF) {
		goto warn_err;
	}
	
	if (compose->targetinfo) {
		target_locked = MSG_IS_LOCKED(compose->targetinfo->flags);
		flag.perm_flags = target_locked?MSG_LOCKED:0;
	}
	flag.tmp_flags = MSG_DRAFT;

	folder_item_scan(draft);
	if ((msgnum = folder_item_add_msg(draft, tmp, &flag, TRUE)) < 0) {
		MsgInfo *tmpinfo = NULL;
		debug_print("didn't get msgnum after adding draft [%s]\n", compose->msgid?compose->msgid:"no msgid");
		if (compose->msgid) {
			tmpinfo = folder_item_get_msginfo_by_msgid(draft, compose->msgid);
		}
		if (tmpinfo) {
			msgnum = tmpinfo->msgnum;
			procmsg_msginfo_free(tmpinfo);
			debug_print("got draft msgnum %d from scanning\n", msgnum);
		} else {
			debug_print("didn't get draft msgnum after scanning\n");
		}
	} else {
		debug_print("got draft msgnum %d from adding\n", msgnum);
	}
	if (msgnum < 0) {
warn_err:
		claws_unlink(tmp);
		g_free(tmp);
		if (action != COMPOSE_AUTO_SAVE) {
			if (action != COMPOSE_DRAFT_FOR_EXIT)
				alertpanel_error(_("Could not save draft."));
			else {
				AlertValue val;
				gtkut_window_popup(compose->window);
				val = alertpanel_full(_("Could not save draft"),
					_("Could not save draft.\n"
					"Do you want to cancel exit or discard this email?"),
					  _("_Cancel exit"), _("_Discard email"), NULL,
					  FALSE, NULL, ALERT_QUESTION, G_ALERTDEFAULT);
				if (val == G_ALERTALTERNATE) {
					lock = FALSE;
					g_mutex_unlock(compose->mutex); /* must be done before closing */
					compose_close(compose);
					return TRUE;
				} else {
					lock = FALSE;
					g_mutex_unlock(compose->mutex); /* must be done before closing */
					return FALSE;
				}
			}
		}
		goto unlock;
	}
	g_free(tmp);

	if (compose->mode == COMPOSE_REEDIT) {
		compose_remove_reedit_target(compose, TRUE);
	}

	newmsginfo = folder_item_get_msginfo(draft, msgnum);

	if (newmsginfo) {
		procmsg_msginfo_unset_flags(newmsginfo, ~0, ~0);
		if (target_locked)
			procmsg_msginfo_set_flags(newmsginfo, MSG_LOCKED, MSG_DRAFT);
		else
			procmsg_msginfo_set_flags(newmsginfo, 0, MSG_DRAFT);
		if (compose_use_attach(compose) && action != COMPOSE_AUTO_SAVE)
			procmsg_msginfo_set_flags(newmsginfo, 0,
						  MSG_HAS_ATTACHMENT);

		if (action == COMPOSE_DRAFT_FOR_EXIT) {
			compose_register_draft(newmsginfo);
		}
		procmsg_msginfo_free(newmsginfo);
	}
	
	folder_item_scan(draft);
	
	if (action == COMPOSE_QUIT_EDITING || action == COMPOSE_DRAFT_FOR_EXIT) {
		lock = FALSE;
		g_mutex_unlock(compose->mutex); /* must be done before closing */
		compose_close(compose);
		return TRUE;
	} else {
		struct stat s;
		gchar *path;

		path = folder_item_fetch_msg(draft, msgnum);
		if (path == NULL) {
			debug_print("can't fetch %s:%d\n", draft->path, msgnum);
			goto unlock;
		}
		if (g_stat(path, &s) < 0) {
			FILE_OP_ERROR(path, "stat");
			g_free(path);
			goto unlock;
		}
		g_free(path);

		procmsg_msginfo_free(compose->targetinfo);
		compose->targetinfo = procmsg_msginfo_new();
		compose->targetinfo->msgnum = msgnum;
		compose->targetinfo->size = (goffset)s.st_size;
		compose->targetinfo->mtime = s.st_mtime;
		compose->targetinfo->folder = draft;
		if (target_locked)
			procmsg_msginfo_set_flags(compose->targetinfo, MSG_LOCKED, 0);
		compose->mode = COMPOSE_REEDIT;
		
		if (action == COMPOSE_AUTO_SAVE) {
			compose->autosaved_draft = compose->targetinfo;
		}
		compose->modified = FALSE;
		compose_set_title(compose);
	}
unlock:
	lock = FALSE;
	g_mutex_unlock(compose->mutex);
	return TRUE;
}

void compose_clear_exit_drafts(void)
{
	gchar *filepath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
				      DRAFTED_AT_EXIT, NULL);
	if (is_file_exist(filepath))
		claws_unlink(filepath);
	
	g_free(filepath);
}

void compose_reopen_exit_drafts(void)
{
	gchar *filepath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
				      DRAFTED_AT_EXIT, NULL);
	FILE *fp = g_fopen(filepath, "rb");
	gchar buf[1024];
	
	if (fp) {
		while (fgets(buf, sizeof(buf), fp)) {
			gchar **parts = g_strsplit(buf, "\t", 2);
			const gchar *folder = parts[0];
			int msgnum = parts[1] ? atoi(parts[1]):-1;
			
			if (folder && *folder && msgnum > -1) {
				FolderItem *item = folder_find_item_from_identifier(folder);
				MsgInfo *info = folder_item_get_msginfo(item, msgnum);
				if (info)
					compose_reedit(info, FALSE);
			}
			g_strfreev(parts);
		}	
		fclose(fp);
	}	
	g_free(filepath);
	compose_clear_exit_drafts();
}

static void compose_save_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	compose_draft(compose, COMPOSE_KEEP_EDITING);
	compose->rmode = COMPOSE_REEDIT;
}

void compose_attach_from_list(Compose *compose, GList *file_list, gboolean free_data)
{
	if (compose && file_list) {
		GList *tmp;

		for ( tmp = file_list; tmp; tmp = tmp->next) {
			gchar *file = (gchar *) tmp->data;
			gchar *utf8_filename = conv_filename_to_utf8(file);
			compose_attach_append(compose, file, utf8_filename, NULL, NULL);
			compose_changed_cb(NULL, compose);
			if (free_data) {
			g_free(file);
				tmp->data = NULL;
			}
			g_free(utf8_filename);
		}
	}
}

static void compose_attach_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	GList *file_list;

	if (compose->redirect_filename != NULL)
		return;

	file_list = filesel_select_multiple_files_open(_("Select file"));

	if (file_list) {
		compose_attach_from_list(compose, file_list, TRUE);
		g_list_free(file_list);
	}
}

static void compose_insert_file_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	GList *file_list;
	gint files_inserted = 0;

	file_list = filesel_select_multiple_files_open(_("Select file"));

	if (file_list) {
		GList *tmp;

		for ( tmp = file_list; tmp; tmp = tmp->next) {
			gchar *file = (gchar *) tmp->data;
			gchar *filedup = g_strdup(file);
			gchar *shortfile = g_path_get_basename(filedup);
			ComposeInsertResult res;
			/* insert the file if the file is short or if the user confirmed that
			   he/she wants to insert the large file */
			res = compose_insert_file(compose, file);
			if (res == COMPOSE_INSERT_READ_ERROR) {
				alertpanel_error(_("File '%s' could not be read."), shortfile);
			} else if (res == COMPOSE_INSERT_INVALID_CHARACTER) {
				alertpanel_error(_("File '%s' contained invalid characters\n"
							"for the current encoding, insertion may be incorrect."),
							shortfile);
			} else if (res == COMPOSE_INSERT_SUCCESS)
				files_inserted++;

			g_free(shortfile);
			g_free(filedup);
			g_free(file);
		}
		g_list_free(file_list);
	}

#ifdef USE_ENCHANT	
	if (files_inserted > 0 && compose->gtkaspell && 
       	    compose->gtkaspell->check_while_typing)
		gtkaspell_highlight_all(compose->gtkaspell);
#endif
}

static void compose_insert_sig_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	compose_insert_sig(compose, FALSE);
}

static gint compose_delete_cb(GtkWidget *widget, GdkEventAny *event,
			      gpointer data)
{
	gint x, y;
	Compose *compose = (Compose *)data;

	gtkut_widget_get_uposition(widget, &x, &y);
	if (!compose->batch) {
		prefs_common.compose_x = x;
		prefs_common.compose_y = y;
	}
	if (compose->sending || compose->updating)
		return TRUE;
	compose_close_cb(NULL, compose);
	return TRUE;
}

void compose_close_toolbar(Compose *compose)
{
	compose_close_cb(NULL, compose);
}

static void compose_close_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	AlertValue val;

#ifdef G_OS_UNIX
	if (compose->exteditor_tag != -1) {
		if (!compose_ext_editor_kill(compose))
			return;
	}
#endif

	if (compose->modified) {
		gboolean reedit = (compose->rmode == COMPOSE_REEDIT);
		if (!g_mutex_trylock(compose->mutex)) {
			/* we don't want to lock the mutex once it's available,
			 * because as the only other part of compose.c locking
			 * it is compose_close - which means once unlocked,
			 * the compose struct will be freed */
			debug_print("couldn't lock mutex, probably sending\n");
			return;
		}
		if (!reedit) {
			val = alertpanel(_("Discard message"),
				 _("This message has been modified. Discard it?"),
				 _("_Discard"), _("_Save to Drafts"), GTK_STOCK_CANCEL);
		} else {
			val = alertpanel(_("Save changes"),
				 _("This message has been modified. Save the latest changes?"),
				 _("_Don't save"), _("+_Save to Drafts"), GTK_STOCK_CANCEL);
		}
		g_mutex_unlock(compose->mutex);
		switch (val) {
		case G_ALERTDEFAULT:
			if (prefs_common.autosave && !reedit)
				compose_remove_draft(compose);			
			break;
		case G_ALERTALTERNATE:
			compose_draft(data, COMPOSE_QUIT_EDITING);
			return;
		default:
			return;
		}
	}

	compose_close(compose);
}

static void compose_set_encoding_cb(GtkAction *action, GtkRadioAction *current, gpointer data)
{
	gboolean active = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (current));
	gint value = gtk_radio_action_get_current_value (GTK_RADIO_ACTION (current));
	Compose *compose = (Compose *) data;

	if (active)
		compose->out_encoding = (CharSet)value;
}

static void compose_address_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	addressbook_open(compose);
}

static void about_show_cb(GtkAction *action, gpointer data)
{
	about_show();
}

static void compose_template_activate_cb(GtkWidget *widget, gpointer data)
{
	Compose *compose = (Compose *)data;
	Template *tmpl;
	gchar *msg;
	AlertValue val;

	tmpl = g_object_get_data(G_OBJECT(widget), "template");
	cm_return_if_fail(tmpl != NULL);

	msg = g_strdup_printf(_("Do you want to apply the template '%s' ?"),
			      tmpl->name);
	val = alertpanel(_("Apply template"), msg,
			 _("_Replace"), _("_Insert"), GTK_STOCK_CANCEL);
	g_free(msg);

	if (val == G_ALERTDEFAULT)
		compose_template_apply(compose, tmpl, TRUE);
	else if (val == G_ALERTALTERNATE)
		compose_template_apply(compose, tmpl, FALSE);
}

static void compose_ext_editor_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	compose_exec_ext_editor(compose);
}

static void compose_undo_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	gboolean prev_autowrap = compose->autowrap;

	compose->autowrap = FALSE;
	undo_undo(compose->undostruct);
	compose->autowrap = prev_autowrap;
}

static void compose_redo_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	gboolean prev_autowrap = compose->autowrap;
	
	compose->autowrap = FALSE;
	undo_redo(compose->undostruct);
	compose->autowrap = prev_autowrap;
}

static void entry_cut_clipboard(GtkWidget *entry)
{
	if (GTK_IS_EDITABLE(entry))
		gtk_editable_cut_clipboard (GTK_EDITABLE(entry));
	else if (GTK_IS_TEXT_VIEW(entry))
		gtk_text_buffer_cut_clipboard(
			gtk_text_view_get_buffer(GTK_TEXT_VIEW(entry)),
			gtk_clipboard_get(GDK_SELECTION_CLIPBOARD),
			TRUE);
}

static void entry_copy_clipboard(GtkWidget *entry)
{
	if (GTK_IS_EDITABLE(entry))
		gtk_editable_copy_clipboard (GTK_EDITABLE(entry));
	else if (GTK_IS_TEXT_VIEW(entry))
		gtk_text_buffer_copy_clipboard(
			gtk_text_view_get_buffer(GTK_TEXT_VIEW(entry)),
			gtk_clipboard_get(GDK_SELECTION_CLIPBOARD));
}

static void entry_paste_clipboard(Compose *compose, GtkWidget *entry, 
				  gboolean wrap, GdkAtom clip, GtkTextIter *insert_place)
{
	if (GTK_IS_TEXT_VIEW(entry)) {
		GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(entry));
		GtkTextMark *mark_start = gtk_text_buffer_get_insert(buffer);
		GtkTextIter start_iter, end_iter;
		gint start, end;
		gchar *contents = gtk_clipboard_wait_for_text(gtk_clipboard_get(clip));

		if (contents == NULL)
			return;
	
		/* we shouldn't delete the selection when middle-click-pasting, or we
		 * can't mid-click-paste our own selection */
		if (clip != GDK_SELECTION_PRIMARY) {
			undo_paste_clipboard(GTK_TEXT_VIEW(compose->text), compose->undostruct);
			gtk_text_buffer_delete_selection(buffer, FALSE, TRUE);
		}
		
		if (insert_place == NULL) {
			/* if insert_place isn't specified, insert at the cursor.
			 * used for Ctrl-V pasting */
			gtk_text_buffer_get_iter_at_mark(buffer, &start_iter, mark_start);
			start = gtk_text_iter_get_offset(&start_iter);
			gtk_text_buffer_insert(buffer, &start_iter, contents, strlen(contents));
		} else {
			/* if insert_place is specified, paste here.
			 * used for mid-click-pasting */
			start = gtk_text_iter_get_offset(insert_place);
			gtk_text_buffer_insert(buffer, insert_place, contents, strlen(contents));
			if (prefs_common.primary_paste_unselects)
				gtk_text_buffer_select_range(buffer, insert_place, insert_place);
		}
		
		if (!wrap) {
			/* paste unwrapped: mark the paste so it's not wrapped later */
			end = start + strlen(contents);
			gtk_text_buffer_get_iter_at_offset(buffer, &start_iter, start);
			gtk_text_buffer_get_iter_at_offset(buffer, &end_iter, end);
			gtk_text_buffer_apply_tag_by_name(buffer, "no_wrap", &start_iter, &end_iter);
		} else if (wrap && clip == GDK_SELECTION_PRIMARY) {
			/* rewrap paragraph now (after a mid-click-paste) */
			mark_start = gtk_text_buffer_get_insert(buffer);
			gtk_text_buffer_get_iter_at_mark(buffer, &start_iter, mark_start);
			gtk_text_iter_backward_char(&start_iter);
			compose_beautify_paragraph(compose, &start_iter, TRUE);
		}
	} else if (GTK_IS_EDITABLE(entry))
		gtk_editable_paste_clipboard (GTK_EDITABLE(entry));

	compose->modified = TRUE;
}

static void entry_allsel(GtkWidget *entry)
{
	if (GTK_IS_EDITABLE(entry))
		gtk_editable_select_region(GTK_EDITABLE(entry), 0, -1);
	else if (GTK_IS_TEXT_VIEW(entry)) {
		GtkTextIter startiter, enditer;
		GtkTextBuffer *textbuf;

		textbuf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(entry));
		gtk_text_buffer_get_start_iter(textbuf, &startiter);
		gtk_text_buffer_get_end_iter(textbuf, &enditer);

		gtk_text_buffer_move_mark_by_name(textbuf, 
			"selection_bound", &startiter);
		gtk_text_buffer_move_mark_by_name(textbuf, 
			"insert", &enditer);
	}
}

static void compose_cut_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	if (compose->focused_editable 
#ifndef GENERIC_UMPC
	    && gtkut_widget_has_focus(compose->focused_editable)
#endif
	    )
		entry_cut_clipboard(compose->focused_editable);
}

static void compose_copy_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	if (compose->focused_editable 
#ifndef GENERIC_UMPC
	    && gtkut_widget_has_focus(compose->focused_editable)
#endif
	    )
		entry_copy_clipboard(compose->focused_editable);
}

static void compose_paste_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	gint prev_autowrap;
	GtkTextBuffer *buffer;
	BLOCK_WRAP();
	if (compose->focused_editable &&
	    gtkut_widget_has_focus(compose->focused_editable))
		entry_paste_clipboard(compose, compose->focused_editable, 
				prefs_common.linewrap_pastes,
				GDK_SELECTION_CLIPBOARD, NULL);
	UNBLOCK_WRAP();

#ifdef USE_ENCHANT
	if (gtkut_widget_has_focus(compose->text) &&
	    compose->gtkaspell && 
            compose->gtkaspell->check_while_typing)
	    	gtkaspell_highlight_all(compose->gtkaspell);
#endif
}

static void compose_paste_as_quote_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	gint wrap_quote = prefs_common.linewrap_quote;
	if (compose->focused_editable 
#ifndef GENERIC_UMPC
	    && gtkut_widget_has_focus(compose->focused_editable)
#endif
	    ) {
		/* let text_insert() (called directly or at a later time
		 * after the gtk_editable_paste_clipboard) know that 
		 * text is to be inserted as a quotation. implemented
		 * by using a simple refcount... */
		gint paste_as_quotation = GPOINTER_TO_INT(g_object_get_data(
						G_OBJECT(compose->focused_editable),
						"paste_as_quotation"));
		g_object_set_data(G_OBJECT(compose->focused_editable),
				    "paste_as_quotation",
				    GINT_TO_POINTER(paste_as_quotation + 1));
		prefs_common.linewrap_quote = prefs_common.linewrap_pastes;
		entry_paste_clipboard(compose, compose->focused_editable, 
				prefs_common.linewrap_pastes,
				GDK_SELECTION_CLIPBOARD, NULL);
		prefs_common.linewrap_quote = wrap_quote;
	}
}

static void compose_paste_no_wrap_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	gint prev_autowrap;
	GtkTextBuffer *buffer;
	BLOCK_WRAP();
	if (compose->focused_editable 
#ifndef GENERIC_UMPC
	    && gtkut_widget_has_focus(compose->focused_editable)
#endif
	    )
		entry_paste_clipboard(compose, compose->focused_editable, FALSE,
			GDK_SELECTION_CLIPBOARD, NULL);
	UNBLOCK_WRAP();

#ifdef USE_ENCHANT
	if (gtkut_widget_has_focus(compose->text) &&
	    compose->gtkaspell && 
            compose->gtkaspell->check_while_typing)
	    	gtkaspell_highlight_all(compose->gtkaspell);
#endif
}

static void compose_paste_wrap_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	gint prev_autowrap;
	GtkTextBuffer *buffer;
	BLOCK_WRAP();
	if (compose->focused_editable 
#ifndef GENERIC_UMPC
	    && gtkut_widget_has_focus(compose->focused_editable)
#endif
	    )
		entry_paste_clipboard(compose, compose->focused_editable, TRUE,
			GDK_SELECTION_CLIPBOARD, NULL);
	UNBLOCK_WRAP();

#ifdef USE_ENCHANT
	if (gtkut_widget_has_focus(compose->text) &&
	    compose->gtkaspell &&
            compose->gtkaspell->check_while_typing)
	    	gtkaspell_highlight_all(compose->gtkaspell);
#endif
}

static void compose_allsel_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	if (compose->focused_editable 
#ifndef GENERIC_UMPC
	    && gtkut_widget_has_focus(compose->focused_editable)
#endif
	    )
		entry_allsel(compose->focused_editable);
}

static void textview_move_beginning_of_line (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	gtk_text_iter_set_line_offset(&ins, 0);
	gtk_text_buffer_place_cursor(buffer, &ins);
}

static void textview_move_forward_character (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	if (gtk_text_iter_forward_cursor_position(&ins))
		gtk_text_buffer_place_cursor(buffer, &ins);
}

static void textview_move_backward_character (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	if (gtk_text_iter_backward_cursor_position(&ins))
		gtk_text_buffer_place_cursor(buffer, &ins);
}

static void textview_move_forward_word (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins;
	gint count;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	count = gtk_text_iter_inside_word (&ins) ? 2 : 1;
	if (gtk_text_iter_forward_word_ends(&ins, count)) {
		gtk_text_iter_backward_word_start(&ins);
		gtk_text_buffer_place_cursor(buffer, &ins);
	}
}

static void textview_move_backward_word (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins;
	gint count;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	count = gtk_text_iter_inside_word (&ins) ? 2 : 1;
	if (gtk_text_iter_backward_word_starts(&ins, 1))
		gtk_text_buffer_place_cursor(buffer, &ins);
}

static void textview_move_end_of_line (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	if (gtk_text_iter_forward_to_line_end(&ins))
		gtk_text_buffer_place_cursor(buffer, &ins);
}

static void textview_move_next_line (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins;
	gint offset;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	offset = gtk_text_iter_get_line_offset(&ins);
	if (gtk_text_iter_forward_line(&ins)) {
		gtk_text_iter_set_line_offset(&ins, offset);
		gtk_text_buffer_place_cursor(buffer, &ins);
	}
}

static void textview_move_previous_line (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins;
	gint offset;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	offset = gtk_text_iter_get_line_offset(&ins);
	if (gtk_text_iter_backward_line(&ins)) {
		gtk_text_iter_set_line_offset(&ins, offset);
		gtk_text_buffer_place_cursor(buffer, &ins);
	}
}

static void textview_delete_forward_character (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins, end_iter;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	end_iter = ins;
	if (gtk_text_iter_forward_char(&end_iter)) {
		gtk_text_buffer_delete(buffer, &ins, &end_iter);
	}
}

static void textview_delete_backward_character (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins, end_iter;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	end_iter = ins;
	if (gtk_text_iter_backward_char(&end_iter)) {
		gtk_text_buffer_delete(buffer, &end_iter, &ins);
	}
}

static void textview_delete_forward_word (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins, end_iter;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	end_iter = ins;
	if (gtk_text_iter_forward_word_end(&end_iter)) {
		gtk_text_buffer_delete(buffer, &ins, &end_iter);
	}
}

static void textview_delete_backward_word (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins, end_iter;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	end_iter = ins;
	if (gtk_text_iter_backward_word_start(&end_iter)) {
		gtk_text_buffer_delete(buffer, &end_iter, &ins);
	}
}

static void textview_delete_line (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins, start_iter, end_iter;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);

	start_iter = ins;
	gtk_text_iter_set_line_offset(&start_iter, 0);

	end_iter = ins;
	if (gtk_text_iter_ends_line(&end_iter)){
		if (!gtk_text_iter_forward_char(&end_iter))
			gtk_text_iter_backward_char(&start_iter);
	}
	else 
		gtk_text_iter_forward_to_line_end(&end_iter);
	gtk_text_buffer_delete(buffer, &start_iter, &end_iter);
}

static void textview_delete_to_line_end (GtkTextView *text)
{
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	GtkTextIter ins, end_iter;

	cm_return_if_fail(GTK_IS_TEXT_VIEW(text));

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &ins, mark);
	end_iter = ins;
	if (gtk_text_iter_ends_line(&end_iter))
		gtk_text_iter_forward_char(&end_iter);
	else
		gtk_text_iter_forward_to_line_end(&end_iter);
	gtk_text_buffer_delete(buffer, &ins, &end_iter);
}

#define DO_ACTION(name, act) {						\
	if(!strcmp(name, a_name)) {					\
		return act;						\
	}								\
}
static ComposeCallAdvancedAction compose_call_advanced_action_from_path(GtkAction *action)
{
	const gchar *a_name = gtk_action_get_name(action);
	DO_ACTION("Edit/Advanced/BackChar", COMPOSE_CALL_ADVANCED_ACTION_MOVE_BACKWARD_CHARACTER);
	DO_ACTION("Edit/Advanced/ForwChar", COMPOSE_CALL_ADVANCED_ACTION_MOVE_FORWARD_CHARACTER);
	DO_ACTION("Edit/Advanced/BackWord", COMPOSE_CALL_ADVANCED_ACTION_MOVE_BACKWARD_WORD);
	DO_ACTION("Edit/Advanced/ForwWord", COMPOSE_CALL_ADVANCED_ACTION_MOVE_FORWARD_WORD);
	DO_ACTION("Edit/Advanced/BegLine", COMPOSE_CALL_ADVANCED_ACTION_MOVE_BEGINNING_OF_LINE);
	DO_ACTION("Edit/Advanced/EndLine", COMPOSE_CALL_ADVANCED_ACTION_MOVE_END_OF_LINE);
	DO_ACTION("Edit/Advanced/PrevLine", COMPOSE_CALL_ADVANCED_ACTION_MOVE_PREVIOUS_LINE);
	DO_ACTION("Edit/Advanced/NextLine", COMPOSE_CALL_ADVANCED_ACTION_MOVE_NEXT_LINE);
	DO_ACTION("Edit/Advanced/DelBackChar", COMPOSE_CALL_ADVANCED_ACTION_DELETE_BACKWARD_CHARACTER);
	DO_ACTION("Edit/Advanced/DelForwChar", COMPOSE_CALL_ADVANCED_ACTION_DELETE_FORWARD_CHARACTER);
	DO_ACTION("Edit/Advanced/DelBackWord", COMPOSE_CALL_ADVANCED_ACTION_DELETE_BACKWARD_WORD);
	DO_ACTION("Edit/Advanced/DelForwWord", COMPOSE_CALL_ADVANCED_ACTION_DELETE_FORWARD_WORD);
	DO_ACTION("Edit/Advanced/DelLine", COMPOSE_CALL_ADVANCED_ACTION_DELETE_LINE);
	DO_ACTION("Edit/Advanced/DelEndLine", COMPOSE_CALL_ADVANCED_ACTION_DELETE_TO_LINE_END);
	return -1;
}

static void compose_advanced_action_cb(GtkAction *gaction, gpointer data)
{
	Compose *compose = (Compose *)data;
	GtkTextView *text = GTK_TEXT_VIEW(compose->text);
	ComposeCallAdvancedAction action = -1;
	
	action = compose_call_advanced_action_from_path(gaction);

	static struct {
		void (*do_action) (GtkTextView *text);
	} action_table[] = {
		{textview_move_beginning_of_line},
		{textview_move_forward_character},
		{textview_move_backward_character},
		{textview_move_forward_word},
		{textview_move_backward_word},
		{textview_move_end_of_line},
		{textview_move_next_line},
		{textview_move_previous_line},
		{textview_delete_forward_character},
		{textview_delete_backward_character},
		{textview_delete_forward_word},
		{textview_delete_backward_word},
		{textview_delete_line},
		{textview_delete_to_line_end}
	};

	if (!gtkut_widget_has_focus(GTK_WIDGET(text))) return;

	if (action >= COMPOSE_CALL_ADVANCED_ACTION_MOVE_BEGINNING_OF_LINE &&
	    action <= COMPOSE_CALL_ADVANCED_ACTION_DELETE_TO_LINE_END) {
		if (action_table[action].do_action)
			action_table[action].do_action(text);
		else
			g_warning("Not implemented yet.");
	}
}

static void compose_grab_focus_cb(GtkWidget *widget, Compose *compose)
{
	gchar *str = NULL;
	
	if (GTK_IS_EDITABLE(widget)) {
		str = gtk_editable_get_chars(GTK_EDITABLE(widget), 0, -1);
		gtk_editable_set_position(GTK_EDITABLE(widget), 
			strlen(str));
		g_free(str);
		if (widget->parent && widget->parent->parent
		 && widget->parent->parent->parent) {
			if (GTK_IS_SCROLLED_WINDOW(widget->parent->parent->parent)) {
				gint y = widget->allocation.y;
				gint height = widget->allocation.height;
				GtkAdjustment *shown = gtk_scrolled_window_get_vadjustment
					(GTK_SCROLLED_WINDOW(widget->parent->parent->parent));

				if (y < (int)shown->value) {
					gtk_adjustment_set_value(GTK_ADJUSTMENT(shown), y - 1);
				}
				if (y + height > (int)shown->value + (int)shown->page_size) {
					if (y - height - 1 < (int)shown->upper - (int)shown->page_size) {
						gtk_adjustment_set_value(GTK_ADJUSTMENT(shown), 
							y + height - (int)shown->page_size - 1);
					} else {
						gtk_adjustment_set_value(GTK_ADJUSTMENT(shown), 
							(int)shown->upper - (int)shown->page_size - 1);
					}
				}
			}
		}
	}

	if (GTK_IS_EDITABLE(widget) || GTK_IS_TEXT_VIEW(widget))
		compose->focused_editable = widget;
	
#ifdef GENERIC_UMPC
	if (GTK_IS_TEXT_VIEW(widget) 
	    && gtk_paned_get_child1(GTK_PANED(compose->paned)) != compose->edit_vbox) {
		g_object_ref(compose->notebook);
		g_object_ref(compose->edit_vbox);
		gtk_container_remove(GTK_CONTAINER(compose->paned), compose->notebook);
		gtk_container_remove(GTK_CONTAINER(compose->paned), compose->edit_vbox);
		gtk_paned_add1(GTK_PANED(compose->paned), compose->edit_vbox);
		gtk_paned_add2(GTK_PANED(compose->paned), compose->notebook);
		g_object_unref(compose->notebook);
		g_object_unref(compose->edit_vbox);
		g_signal_handlers_block_by_func(G_OBJECT(widget),
					G_CALLBACK(compose_grab_focus_cb),
					compose);
		gtk_widget_grab_focus(widget);
		g_signal_handlers_unblock_by_func(G_OBJECT(widget),
					G_CALLBACK(compose_grab_focus_cb),
					compose);
	} else if (!GTK_IS_TEXT_VIEW(widget) 
		   && gtk_paned_get_child1(GTK_PANED(compose->paned)) != compose->notebook) {
		g_object_ref(compose->notebook);
		g_object_ref(compose->edit_vbox);
		gtk_container_remove(GTK_CONTAINER(compose->paned), compose->notebook);
		gtk_container_remove(GTK_CONTAINER(compose->paned), compose->edit_vbox);
		gtk_paned_add1(GTK_PANED(compose->paned), compose->notebook);
		gtk_paned_add2(GTK_PANED(compose->paned), compose->edit_vbox);
		g_object_unref(compose->notebook);
		g_object_unref(compose->edit_vbox);
		g_signal_handlers_block_by_func(G_OBJECT(widget),
					G_CALLBACK(compose_grab_focus_cb),
					compose);
		gtk_widget_grab_focus(widget);
		g_signal_handlers_unblock_by_func(G_OBJECT(widget),
					G_CALLBACK(compose_grab_focus_cb),
					compose);
	}
#endif
}

static void compose_changed_cb(GtkTextBuffer *textbuf, Compose *compose)
{
	compose->modified = TRUE;
//	compose_beautify_paragraph(compose, NULL, TRUE);
#ifndef GENERIC_UMPC
	compose_set_title(compose);
#endif
}

static void compose_wrap_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	compose_beautify_paragraph(compose, NULL, TRUE);
}

static void compose_wrap_all_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	compose_wrap_all_full(compose, TRUE);
}

static void compose_find_cb(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	message_search_compose(compose);
}

static void compose_toggle_autowrap_cb(GtkToggleAction *action,
					 gpointer	 data)
{
	Compose *compose = (Compose *)data;
	compose->autowrap = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action));
	if (compose->autowrap)
		compose_wrap_all_full(compose, TRUE);
	compose->autowrap = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action));
}

static void compose_toggle_autoindent_cb(GtkToggleAction *action,
					 gpointer	 data)
{
	Compose *compose = (Compose *)data;
	compose->autoindent = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action));
}

static void compose_toggle_sign_cb(GtkToggleAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	compose->use_signing = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action));
}

static void compose_toggle_encrypt_cb(GtkToggleAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	compose->use_encryption = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action));
}

static void activate_privacy_system(Compose *compose, PrefsAccount *account, gboolean warn) 
{
	g_free(compose->privacy_system);

	compose->privacy_system = g_strdup(account->default_privacy_system);
	compose_update_privacy_system_menu_item(compose, warn);
}

static void compose_toggle_ruler_cb(GtkToggleAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	if (gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action))) {
		gtk_widget_show(compose->ruler_hbox);
		prefs_common.show_ruler = TRUE;
	} else {
		gtk_widget_hide(compose->ruler_hbox);
		gtk_widget_queue_resize(compose->edit_vbox);
		prefs_common.show_ruler = FALSE;
	}
}

static void compose_attach_drag_received_cb (GtkWidget		*widget,
					     GdkDragContext	*context,
					     gint		 x,
					     gint		 y,
					     GtkSelectionData	*data,
					     guint		 info,
					     guint		 time,
					     gpointer		 user_data)
{
	Compose *compose = (Compose *)user_data;
	GList *list, *tmp;

	if (((gdk_atom_name(data->type) && !strcmp(gdk_atom_name(data->type), "text/uri-list"))
#ifdef G_OS_WIN32
	 || (gdk_atom_name(data->type) && !strcmp(gdk_atom_name(data->type), "DROPFILES_DND"))
#endif
	   ) && gtk_drag_get_source_widget(context) != 
	        summary_get_main_widget(mainwindow_get_mainwindow()->summaryview)) {
		list = uri_list_extract_filenames((const gchar *)data->data);
		for (tmp = list; tmp != NULL; tmp = tmp->next) {
			gchar *utf8_filename = conv_filename_to_utf8((const gchar *)tmp->data);
			compose_attach_append
				(compose, (const gchar *)tmp->data,
				 utf8_filename, NULL, NULL);
			g_free(utf8_filename);
		}
		if (list) compose_changed_cb(NULL, compose);
		list_free_strings(list);
		g_list_free(list);
	} else if (gtk_drag_get_source_widget(context) 
		   == summary_get_main_widget(mainwindow_get_mainwindow()->summaryview)) {
		/* comes from our summaryview */
		SummaryView * summaryview = NULL;
		GSList * list = NULL, *cur = NULL;
		
		if (mainwindow_get_mainwindow())
			summaryview = mainwindow_get_mainwindow()->summaryview;
		
		if (summaryview)
			list = summary_get_selected_msg_list(summaryview);
		
		for (cur = list; cur; cur = cur->next) {
			MsgInfo *msginfo = (MsgInfo *)cur->data;
			gchar *file = NULL;
			if (msginfo)
				file = procmsg_get_message_file_full(msginfo, 
					TRUE, TRUE);
			if (file) {
				compose_attach_append(compose, (const gchar *)file, 
					(const gchar *)file, "message/rfc822", NULL);
				g_free(file);
			}
		}
		g_slist_free(list);
	}
}

static gboolean compose_drag_drop(GtkWidget *widget,
				  GdkDragContext *drag_context,
				  gint x, gint y,
				  guint time, gpointer user_data)
{
	/* not handling this signal makes compose_insert_drag_received_cb
	 * called twice */
	return TRUE;					 
}

static void compose_insert_drag_received_cb (GtkWidget		*widget,
					     GdkDragContext	*drag_context,
					     gint		 x,
					     gint		 y,
					     GtkSelectionData	*data,
					     guint		 info,
					     guint		 time,
					     gpointer		 user_data)
{
	Compose *compose = (Compose *)user_data;
	GList *list, *tmp;

	/* strangely, testing data->type == gdk_atom_intern("text/uri-list", TRUE)
	 * does not work */
#ifndef G_OS_WIN32
	if (gdk_atom_name(data->type) && !strcmp(gdk_atom_name(data->type), "text/uri-list")) {
#else
	if (gdk_atom_name(data->type) && !strcmp(gdk_atom_name(data->type), "DROPFILES_DND")) {
#endif
		AlertValue val = G_ALERTDEFAULT;

		list = uri_list_extract_filenames((const gchar *)data->data);
		if (list == NULL && strstr((gchar *)(data->data), "://")) {
			/* Assume a list of no files, and data has ://, is a remote link */
			gchar *tmpdata = g_strstrip(g_strdup((const gchar *)data->data));
			gchar *tmpfile = get_tmp_file();
			str_write_to_file(tmpdata, tmpfile);
			g_free(tmpdata);  
			compose_insert_file(compose, tmpfile);
			claws_unlink(tmpfile);
			g_free(tmpfile);
			gtk_drag_finish(drag_context, TRUE, FALSE, time);
			compose_beautify_paragraph(compose, NULL, TRUE);
			return;
		}
		switch (prefs_common.compose_dnd_mode) {
			case COMPOSE_DND_ASK:
				val = alertpanel_full(_("Insert or attach?"),
					 _("Do you want to insert the contents of the file(s) "
					   "into the message body, or attach it to the email?"),
					  GTK_STOCK_CANCEL, _("+_Insert"), _("_Attach"),
					  TRUE, NULL, ALERT_QUESTION, G_ALERTALTERNATE);
				break;
			case COMPOSE_DND_INSERT:
				val = G_ALERTALTERNATE;
				break;
			case COMPOSE_DND_ATTACH:
				val = G_ALERTOTHER;
				break;
			default:
				/* unexpected case */
				g_warning("error: unexpected compose_dnd_mode option value in compose_insert_drag_received_cb()");
		}

		if (val & G_ALERTDISABLE) {
			val &= ~G_ALERTDISABLE;
			/* remember what action to perform by default, only if we don't click Cancel */
			if (val == G_ALERTALTERNATE)
				prefs_common.compose_dnd_mode = COMPOSE_DND_INSERT;
			else if (val == G_ALERTOTHER)
					prefs_common.compose_dnd_mode = COMPOSE_DND_ATTACH;
		}

		if (val == G_ALERTDEFAULT || val == G_ALERTCANCEL) {
			gtk_drag_finish(drag_context, FALSE, FALSE, time);
			list_free_strings(list);
			g_list_free(list);
			return;
		} else if (val == G_ALERTOTHER) {
			compose_attach_drag_received_cb(widget, drag_context, x, y, data, info, time, user_data);
			list_free_strings(list);
			g_list_free(list);
			return;
		} 

		for (tmp = list; tmp != NULL; tmp = tmp->next) {
			compose_insert_file(compose, (const gchar *)tmp->data);
		}
		list_free_strings(list);
		g_list_free(list);
		gtk_drag_finish(drag_context, TRUE, FALSE, time);
		return;
	} else {
		return;
	}
	gtk_drag_finish(drag_context, TRUE, FALSE, time);
}

static void compose_header_drag_received_cb (GtkWidget		*widget,
					     GdkDragContext	*drag_context,
					     gint		 x,
					     gint		 y,
					     GtkSelectionData	*data,
					     guint		 info,
					     guint		 time,
					     gpointer		 user_data)
{
	GtkEditable *entry = (GtkEditable *)user_data;
	gchar *email = (gchar *)data->data;

	/* strangely, testing data->type == gdk_atom_intern("text/plain", TRUE)
	 * does not work */

	if (!strncmp(email, "mailto:", strlen("mailto:"))) {
		gchar *decoded=g_new(gchar, strlen(email));
		int start = 0;

		email += strlen("mailto:");
		decode_uri(decoded, email); /* will fit */
		gtk_editable_delete_text(entry, 0, -1);
		gtk_editable_insert_text(entry, decoded, strlen(decoded), &start);
		gtk_drag_finish(drag_context, TRUE, FALSE, time);
		g_free(decoded);
		return;
	}
	gtk_drag_finish(drag_context, TRUE, FALSE, time);
}

static void compose_toggle_return_receipt_cb(GtkToggleAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	if (gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)))
		compose->return_receipt = TRUE;
	else
		compose->return_receipt = FALSE;
}

static void compose_toggle_remove_refs_cb(GtkToggleAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;

	if (gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)))
		compose->remove_references = TRUE;
	else
		compose->remove_references = FALSE;
}

static gboolean compose_headerentry_button_clicked_cb (GtkWidget *button,
                                        ComposeHeaderEntry *headerentry)
{
	gtk_entry_set_text(GTK_ENTRY(headerentry->entry), "");
	return FALSE;
}

static gboolean compose_headerentry_key_press_event_cb(GtkWidget *entry,
					    GdkEventKey *event,
					    ComposeHeaderEntry *headerentry)
{
	if ((g_slist_length(headerentry->compose->header_list) > 0) &&
	    ((headerentry->headernum + 1) != headerentry->compose->header_nextrow) &&
	    !(event->state & GDK_MODIFIER_MASK) &&
	    (event->keyval == GDK_BackSpace) &&
	    (strlen(gtk_entry_get_text(GTK_ENTRY(entry))) == 0)) {
		gtk_container_remove
			(GTK_CONTAINER(headerentry->compose->header_table),
			 headerentry->combo);
		gtk_container_remove
			(GTK_CONTAINER(headerentry->compose->header_table),
			 headerentry->entry);
		headerentry->compose->header_list =
			g_slist_remove(headerentry->compose->header_list,
				       headerentry);
		g_free(headerentry);
	} else 	if (event->keyval == GDK_Tab) {
		if (headerentry->compose->header_last == headerentry) {
			/* Override default next focus, and give it to subject_entry
			 * instead of notebook tabs
			 */
			g_signal_stop_emission_by_name(G_OBJECT(entry), "key-press-event"); 
			gtk_widget_grab_focus(headerentry->compose->subject_entry);
			return TRUE;
		}
	}
	return FALSE;
}

static gboolean scroll_postpone(gpointer data)
{
	Compose *compose = (Compose *)data;

	cm_return_val_if_fail(!compose->batch, FALSE);

	GTK_EVENTS_FLUSH();
	compose_show_first_last_header(compose, FALSE);
	return FALSE;
}

static void compose_headerentry_changed_cb(GtkWidget *entry,
				    ComposeHeaderEntry *headerentry)
{
	if (strlen(gtk_entry_get_text(GTK_ENTRY(entry))) != 0) {
		compose_create_header_entry(headerentry->compose);
		g_signal_handlers_disconnect_matched
			(G_OBJECT(entry), G_SIGNAL_MATCH_DATA,
			 0, 0, NULL, NULL, headerentry);

		if (!headerentry->compose->batch)
			g_timeout_add(0, scroll_postpone, headerentry->compose);
	}
}

static gboolean compose_defer_auto_save_draft(Compose *compose)
{
	compose->draft_timeout_tag = -1;
	compose_draft((gpointer)compose, COMPOSE_AUTO_SAVE);
	return FALSE;
}

static void compose_show_first_last_header(Compose *compose, gboolean show_first)
{
	GtkAdjustment *vadj;

	cm_return_if_fail(compose);
	cm_return_if_fail(!compose->batch);
	cm_return_if_fail(GTK_IS_WIDGET(compose->header_table));
	cm_return_if_fail(GTK_IS_VIEWPORT(compose->header_table->parent));
	vadj = gtk_viewport_get_vadjustment(GTK_VIEWPORT(compose->header_table->parent));
	gtk_adjustment_set_value(vadj, (show_first ? vadj->lower : (vadj->upper - vadj->page_size)));
	gtk_adjustment_changed(vadj);
}

static void text_inserted(GtkTextBuffer *buffer, GtkTextIter *iter,
			  const gchar *text, gint len, Compose *compose)
{
	gint paste_as_quotation = GPOINTER_TO_INT(g_object_get_data
				(G_OBJECT(compose->text), "paste_as_quotation"));
	GtkTextMark *mark;

	cm_return_if_fail(text != NULL);

	g_signal_handlers_block_by_func(G_OBJECT(buffer),
					G_CALLBACK(text_inserted),
					compose);
	if (paste_as_quotation) {
		gchar *new_text;
		const gchar *qmark;
		guint pos = 0;
		GtkTextIter start_iter;

		if (len < 0)
			len = strlen(text);

		new_text = g_strndup(text, len);

		qmark = compose_quote_char_from_context(compose);

		mark = gtk_text_buffer_create_mark(buffer, NULL, iter, FALSE);
		gtk_text_buffer_place_cursor(buffer, iter);

		pos = gtk_text_iter_get_offset(iter);

		compose_quote_fmt(compose, NULL, "%Q", qmark, new_text, TRUE, FALSE,
						  _("Quote format error at line %d."));
		quote_fmt_reset_vartable();
		g_free(new_text);
		g_object_set_data(G_OBJECT(compose->text), "paste_as_quotation",
				  GINT_TO_POINTER(paste_as_quotation - 1));
				  
		gtk_text_buffer_get_iter_at_mark(buffer, iter, mark);
		gtk_text_buffer_place_cursor(buffer, iter);
		gtk_text_buffer_delete_mark(buffer, mark);

		gtk_text_buffer_get_iter_at_offset(buffer, &start_iter, pos);
		mark = gtk_text_buffer_create_mark(buffer, NULL, &start_iter, FALSE);
		compose_beautify_paragraph(compose, &start_iter, FALSE);
		gtk_text_buffer_get_iter_at_mark(buffer, &start_iter, mark);
		gtk_text_buffer_delete_mark(buffer, mark);
	} else {
		if (strcmp(text, "\n") || compose->automatic_break
		|| gtk_text_iter_starts_line(iter)) {
			GtkTextIter before_ins;
			gtk_text_buffer_insert(buffer, iter, text, len);
			if (!strstr(text, "\n") && gtk_text_iter_has_tag(iter, compose->no_join_tag)) {
				before_ins = *iter; 
				gtk_text_iter_backward_chars(&before_ins, len);
				gtk_text_buffer_remove_tag_by_name(buffer, "no_join", &before_ins, iter);
			}
		} else {
			/* check if the preceding is just whitespace or quote */
			GtkTextIter start_line;
			gchar *tmp = NULL, *quote = NULL;
			gint quote_len = 0, is_normal = 0;
			start_line = *iter;
			gtk_text_iter_set_line_offset(&start_line, 0); 
			tmp = gtk_text_buffer_get_text(buffer, &start_line, iter, FALSE);
			g_strstrip(tmp);

			if (*tmp == '\0') {
				is_normal = 1;
			} else {
				quote = compose_get_quote_str(buffer, &start_line, &quote_len);
				if (quote)
					is_normal = 1;
				g_free(quote);
			}
			g_free(tmp);
			
			if (is_normal) {
				gtk_text_buffer_insert(buffer, iter, text, len);
			} else {
				gtk_text_buffer_insert_with_tags_by_name(buffer, 
					iter, text, len, "no_join", NULL);
			}
		}
	}
	
	if (!paste_as_quotation) {
		mark = gtk_text_buffer_create_mark(buffer, NULL, iter, FALSE);
		compose_beautify_paragraph(compose, iter, FALSE);
		gtk_text_buffer_get_iter_at_mark(buffer, iter, mark);
		gtk_text_buffer_delete_mark(buffer, mark);
	}

	g_signal_handlers_unblock_by_func(G_OBJECT(buffer),
					  G_CALLBACK(text_inserted),
					  compose);
	g_signal_stop_emission_by_name(G_OBJECT(buffer), "insert-text");

	if (prefs_common.autosave && 
	    gtk_text_buffer_get_char_count(buffer) % prefs_common.autosave_length == 0 &&
	    compose->draft_timeout_tag != -2 /* disabled while loading */)
		compose->draft_timeout_tag = g_timeout_add
			(500, (GSourceFunc) compose_defer_auto_save_draft, compose);
}

#if USE_ENCHANT
static void compose_check_all(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	if (!compose->gtkaspell)
		return;
		
	if (gtkut_widget_has_focus(compose->subject_entry))
		claws_spell_entry_check_all(
			CLAWS_SPELL_ENTRY(compose->subject_entry));		
	else
		gtkaspell_check_all(compose->gtkaspell);
}

static void compose_highlight_all(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	if (compose->gtkaspell) {
		claws_spell_entry_recheck_all(
			CLAWS_SPELL_ENTRY(compose->subject_entry));
		gtkaspell_highlight_all(compose->gtkaspell);
	}
}

static void compose_check_backwards(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	if (!compose->gtkaspell) {
		cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Spelling", FALSE);
		return;
	}

	if (gtkut_widget_has_focus(compose->subject_entry))
		claws_spell_entry_check_backwards(
			CLAWS_SPELL_ENTRY(compose->subject_entry));
	else
		gtkaspell_check_backwards(compose->gtkaspell);
}

static void compose_check_forwards_go(GtkAction *action, gpointer data)
{
	Compose *compose = (Compose *)data;
	if (!compose->gtkaspell) {
		cm_menu_set_sensitive_full(compose->ui_manager, "Menu/Spelling", FALSE);
		return;
	}

	if (gtkut_widget_has_focus(compose->subject_entry))
		claws_spell_entry_check_forwards_go(
			CLAWS_SPELL_ENTRY(compose->subject_entry));
	else
		gtkaspell_check_forwards_go(compose->gtkaspell);
}
#endif

/*!
 *\brief	Guess originating forward account from MsgInfo and several 
 *		"common preference" settings. Return NULL if no guess. 
 */
static PrefsAccount *compose_guess_forward_account_from_msginfo(MsgInfo *msginfo)
{
	PrefsAccount *account = NULL;
	
	cm_return_val_if_fail(msginfo, NULL);
	cm_return_val_if_fail(msginfo->folder, NULL);
	cm_return_val_if_fail(msginfo->folder->prefs, NULL);

	if (msginfo->folder->prefs->enable_default_account)
		account = account_find_from_id(msginfo->folder->prefs->default_account);
		
	if (!account) 
		account = msginfo->folder->folder->account;
		
	if (!account && msginfo->to && prefs_common.forward_account_autosel) {
		gchar *to;
		Xstrdup_a(to, msginfo->to, return NULL);
		extract_address(to);
		account = account_find_from_address(to, FALSE);
	}

	if (!account && prefs_common.forward_account_autosel) {
		gchar cc[BUFFSIZE];
		if (!procheader_get_header_from_msginfo
			(msginfo, cc,sizeof cc , "Cc:")) { 
			gchar *buf = cc + strlen("Cc:");
		        extract_address(buf);
		        account = account_find_from_address(buf, FALSE);
                }
	}
	
	if (!account && prefs_common.forward_account_autosel) {
		gchar deliveredto[BUFFSIZE];
		if (!procheader_get_header_from_msginfo
			(msginfo, deliveredto,sizeof deliveredto , "Delivered-To:")) { 
			gchar *buf = deliveredto + strlen("Delivered-To:");
		        extract_address(buf);
		        account = account_find_from_address(buf, FALSE);
                }
	}
	
	return account;
}

gboolean compose_close(Compose *compose)
{
	gint x, y;

	if (!g_mutex_trylock(compose->mutex)) {
		/* we have to wait for the (possibly deferred by auto-save)
		 * drafting to be done, before destroying the compose under
		 * it. */
		debug_print("waiting for drafting to finish...\n");
		compose_allow_user_actions(compose, FALSE);
		g_timeout_add (500, (GSourceFunc) compose_close, compose);
		return FALSE;
	}
	cm_return_val_if_fail(compose, FALSE);
	gtkut_widget_get_uposition(compose->window, &x, &y);
	if (!compose->batch) {
		prefs_common.compose_x = x;
		prefs_common.compose_y = y;
	}
	g_mutex_unlock(compose->mutex);
	compose_destroy(compose);
	return FALSE;
}

/**
 * Add entry field for each address in list.
 * \param compose     E-Mail composition object.
 * \param listAddress List of (formatted) E-Mail addresses.
 */
static void compose_add_field_list( Compose *compose, GList *listAddress ) {
	GList *node;
	gchar *addr;
	node = listAddress;
	while( node ) {
		addr = ( gchar * ) node->data;
		compose_entry_append( compose, addr, COMPOSE_TO, PREF_NONE );
		node = g_list_next( node );
	}
}

static void compose_reply_from_messageview_real(MessageView *msgview, GSList *msginfo_list, 
				    guint action, gboolean opening_multiple)
{
	gchar *body = NULL;
	GSList *new_msglist = NULL;
	MsgInfo *tmp_msginfo = NULL;
	gboolean originally_enc = FALSE;
	gboolean originally_sig = FALSE;
	Compose *compose = NULL;
	gchar *s_system = NULL;

	cm_return_if_fail(msgview != NULL);

	cm_return_if_fail(msginfo_list != NULL);

	if (g_slist_length(msginfo_list) == 1 && !opening_multiple) {
		MimeInfo *mimeinfo = messageview_get_selected_mime_part(msgview);
		MsgInfo *orig_msginfo = (MsgInfo *)msginfo_list->data;

		if (mimeinfo != NULL && mimeinfo->type == MIMETYPE_MESSAGE && 
		    !g_ascii_strcasecmp(mimeinfo->subtype, "rfc822")) {
			tmp_msginfo = procmsg_msginfo_new_from_mimeinfo(
						orig_msginfo, mimeinfo);
			if (tmp_msginfo != NULL) {
				new_msglist = g_slist_append(NULL, tmp_msginfo);

				originally_enc = MSG_IS_ENCRYPTED(orig_msginfo->flags);
				privacy_msginfo_get_signed_state(orig_msginfo, &s_system);
				originally_sig = MSG_IS_SIGNED(orig_msginfo->flags);

				tmp_msginfo->folder = orig_msginfo->folder;
				tmp_msginfo->msgnum = orig_msginfo->msgnum; 
				if (orig_msginfo->tags) {
					tmp_msginfo->tags = g_slist_copy(orig_msginfo->tags);
					tmp_msginfo->folder->tags_dirty = TRUE;
				}
			}
		}
	}

	if (!opening_multiple)
		body = messageview_get_selection(msgview);

	if (new_msglist) {
		compose = compose_reply_mode((ComposeMode)action, new_msglist, body);
		procmsg_msginfo_free(tmp_msginfo);
		g_slist_free(new_msglist);
	} else
		compose = compose_reply_mode((ComposeMode)action, msginfo_list, body);

	if (compose && originally_enc) {
		compose_force_encryption(compose, compose->account, FALSE, s_system);
	}

	if (compose && originally_sig && compose->account->default_sign_reply) {
		compose_force_signing(compose, compose->account, s_system);
	}
	g_free(s_system);
	g_free(body);
	hooks_invoke(COMPOSE_CREATED_HOOKLIST, compose);
}

void compose_reply_from_messageview(MessageView *msgview, GSList *msginfo_list, 
				    guint action)
{
	if ((!prefs_common.forward_as_attachment || action != COMPOSE_FORWARD) 
	&&  action != COMPOSE_FORWARD_AS_ATTACH && g_slist_length(msginfo_list) > 1) {
		GSList *cur = msginfo_list;
		gchar *msg = g_strdup_printf(_("You are about to reply to %d "
					       "messages. Opening the windows "
					       "could take some time. Do you "
					       "want to continue?"), 
					       g_slist_length(msginfo_list));
		if (g_slist_length(msginfo_list) > 9
		&&  alertpanel(_("Warning"), msg, GTK_STOCK_CANCEL, "+" GTK_STOCK_YES, NULL)
		    != G_ALERTALTERNATE) {
		    	g_free(msg);
			return;
		}
		g_free(msg);
		/* We'll open multiple compose windows */
		/* let the WM place the next windows */
		compose_force_window_origin = FALSE;
		for (; cur; cur = cur->next) {
			GSList tmplist;
			tmplist.data = cur->data;
			tmplist.next = NULL;
			compose_reply_from_messageview_real(msgview, &tmplist, action, TRUE);
		}
		compose_force_window_origin = TRUE;
	} else {
		/* forwarding multiple mails as attachments is done via a
		 * single compose window */
		compose_reply_from_messageview_real(msgview, msginfo_list, action, FALSE);
	}
}

void compose_check_for_email_account(Compose *compose)
{
	PrefsAccount *ac = NULL, *curr = NULL;
	GList *list;
	
	if (!compose)
		return;

	if (compose->account && compose->account->protocol == A_NNTP) {
		ac = account_get_cur_account();
		if (ac->protocol == A_NNTP) {
			list = account_get_list();
			
			for( ; list != NULL ; list = g_list_next(list)) {
				curr = (PrefsAccount *) list->data;
				if (curr->protocol != A_NNTP) {
					ac = curr;
					break;
				}
			}
		}
		combobox_select_by_data(GTK_COMBO_BOX(compose->account_combo),
					ac->account_id); 
	}
}

void compose_reply_to_address(MessageView *msgview, MsgInfo *msginfo, 
				const gchar *address)
{
	GSList *msginfo_list = NULL;
	gchar *body =  messageview_get_selection(msgview);
	Compose *compose;
	
	msginfo_list = g_slist_prepend(msginfo_list, msginfo);
	
	compose = compose_reply_mode(COMPOSE_REPLY_TO_ADDRESS, msginfo_list, body);
	compose_check_for_email_account(compose);
	compose_set_folder_prefs(compose, msginfo->folder, FALSE);
	compose_entry_append(compose, address, COMPOSE_TO, PREF_NONE);
	compose_reply_set_subject(compose, msginfo);

	g_free(body);
	hooks_invoke(COMPOSE_CREATED_HOOKLIST, compose);
}

void compose_set_position(Compose *compose, gint pos)
{
	GtkTextView *text = GTK_TEXT_VIEW(compose->text);

	gtkut_text_view_set_position(text, pos);
}

gboolean compose_search_string(Compose *compose,
				const gchar *str, gboolean case_sens)
{
	GtkTextView *text = GTK_TEXT_VIEW(compose->text);

	return gtkut_text_view_search_string(text, str, case_sens);
}

gboolean compose_search_string_backward(Compose *compose,
				const gchar *str, gboolean case_sens)
{
	GtkTextView *text = GTK_TEXT_VIEW(compose->text);

	return gtkut_text_view_search_string_backward(text, str, case_sens);
}

/* allocate a msginfo structure and populate its data from a compose data structure */
static MsgInfo *compose_msginfo_new_from_compose(Compose *compose)
{
	MsgInfo *newmsginfo;
	GSList *list;
	gchar buf[BUFFSIZE];

	cm_return_val_if_fail( compose != NULL, NULL );

	newmsginfo = procmsg_msginfo_new();

	/* date is now */
	get_rfc822_date(buf, sizeof(buf));
	newmsginfo->date = g_strdup(buf);

	/* from */
	if (compose->from_name) {
		newmsginfo->from = gtk_editable_get_chars(GTK_EDITABLE(compose->from_name), 0, -1);
		newmsginfo->fromname = procheader_get_fromname(newmsginfo->from);
	}

	/* subject */
	if (compose->subject_entry)
		newmsginfo->subject = gtk_editable_get_chars(GTK_EDITABLE(compose->subject_entry), 0, -1);

	/* to, cc, reply-to, newsgroups */
	for (list = compose->header_list; list; list = list->next) {
		gchar *header = gtk_editable_get_chars(
								GTK_EDITABLE(
								gtk_bin_get_child(GTK_BIN((((ComposeHeaderEntry *)list->data)->combo)))), 0, -1);
		gchar *entry = gtk_editable_get_chars(
								GTK_EDITABLE(((ComposeHeaderEntry *)list->data)->entry), 0, -1);

		if ( strcasecmp(header, prefs_common_translated_header_name("To:")) == 0 ) {
			if ( newmsginfo->to == NULL ) {
				newmsginfo->to = g_strdup(entry);
			} else if (entry && *entry) {
				gchar *tmp = g_strconcat(newmsginfo->to, ", ", entry, NULL);
				g_free(newmsginfo->to);
				newmsginfo->to = tmp;
			}
		} else
		if ( strcasecmp(header, prefs_common_translated_header_name("Cc:")) == 0 ) {
			if ( newmsginfo->cc == NULL ) {
				newmsginfo->cc = g_strdup(entry);
			} else if (entry && *entry) {
				gchar *tmp = g_strconcat(newmsginfo->cc, ", ", entry, NULL);
				g_free(newmsginfo->cc);
				newmsginfo->cc = tmp;
			}
		} else
		if ( strcasecmp(header,
						prefs_common_translated_header_name("Newsgroups:")) == 0 ) {
			if ( newmsginfo->newsgroups == NULL ) {
				newmsginfo->newsgroups = g_strdup(entry);
			} else if (entry && *entry) {
				gchar *tmp = g_strconcat(newmsginfo->newsgroups, ", ", entry, NULL);
				g_free(newmsginfo->newsgroups);
				newmsginfo->newsgroups = tmp;
			}
		}

		g_free(header);
		g_free(entry);	
	}

	/* other data is unset */

	return newmsginfo;
}

#ifdef USE_ENCHANT
/* update compose's dictionaries from folder dict settings */
static void compose_set_dictionaries_from_folder_prefs(Compose *compose,
						FolderItem *folder_item)
{
	cm_return_if_fail(compose != NULL);

	if (compose->gtkaspell && folder_item && folder_item->prefs) {
		FolderItemPrefs *prefs = folder_item->prefs;

		if (prefs->enable_default_dictionary)
			gtkaspell_change_dict(compose->gtkaspell,
					prefs->default_dictionary, FALSE);
		if (folder_item->prefs->enable_default_alt_dictionary)
			gtkaspell_change_alt_dict(compose->gtkaspell,
					prefs->default_alt_dictionary);
		if (prefs->enable_default_dictionary
			|| prefs->enable_default_alt_dictionary)
			compose_spell_menu_changed(compose);
	}
}
#endif

/*
 * End of Source.
 */
