/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto
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

#ifndef __COMPOSE_H__
#define __COMPOSE_H__

typedef struct _Compose		Compose;
typedef struct _AttachInfo	AttachInfo;

#include <glib.h>
#include <gtk/gtk.h>

#include "procmsg.h"
#include "procmime.h"
#include "prefs_account.h"
#include "undo.h"
#include "toolbar.h"
#include "codeconv.h"
#include "template.h"
#include "viewtypes.h"
#include "folder.h"

#ifdef USE_ENCHANT
#include "gtkaspell.h"
#endif

#define COMPOSE_CHECK_BEFORE_SEND_HOOKLIST "compose_check_before_send"
#define COMPOSE_CREATED_HOOKLIST "compose_created"

typedef enum
{
	COMPOSE_TO,
	COMPOSE_CC,
	COMPOSE_BCC,
	COMPOSE_REPLYTO,
	COMPOSE_NEWSGROUPS,
	COMPOSE_FOLLOWUPTO,
	COMPOSE_INREPLYTO
} ComposeEntryType;

typedef enum
{
	COMPOSE_REPLY,
	COMPOSE_REPLY_WITH_QUOTE,
	COMPOSE_REPLY_WITHOUT_QUOTE,
	COMPOSE_REPLY_TO_SENDER,
	COMPOSE_REPLY_TO_ADDRESS,
	COMPOSE_FOLLOWUP_AND_REPLY_TO,
	COMPOSE_REPLY_TO_SENDER_WITH_QUOTE,
	COMPOSE_REPLY_TO_SENDER_WITHOUT_QUOTE,
	COMPOSE_REPLY_TO_ALL,
	COMPOSE_REPLY_TO_ALL_WITH_QUOTE,
	COMPOSE_REPLY_TO_ALL_WITHOUT_QUOTE,
	COMPOSE_REPLY_TO_LIST,
	COMPOSE_REPLY_TO_LIST_WITH_QUOTE,
	COMPOSE_REPLY_TO_LIST_WITHOUT_QUOTE,
	COMPOSE_FORWARD,
	COMPOSE_FORWARD_AS_ATTACH,
	COMPOSE_FORWARD_INLINE,
	COMPOSE_NEW,
	COMPOSE_REDIRECT,
	COMPOSE_REEDIT
} ComposeMode;

typedef enum {
	PREF_ACCOUNT,
	PREF_FOLDER,
	PREF_TEMPLATE,
	PREF_ML,
	PREF_MAILTO,
	PREF_NONE
} ComposePrefType;

typedef struct {
	guint headernum;
	Compose *compose;
	GtkWidget *combo;
	GtkWidget *entry;
	GtkWidget *button;
	GtkWidget *hbox;
	ComposePrefType type;
} ComposeHeaderEntry;

struct _Compose
{
	/* start with window widget don`t change order */
	GtkWidget *window;
	GtkWidget *vbox;
	GtkWidget *menubar;

	/* Toolbar handlebox */
	GtkWidget *handlebox;
	Toolbar *toolbar;
	
	GtkWidget *vbox2;

	/* Header */
	GtkWidget *table_vbox;
	GtkWidget *table;
	GtkWidget *account_combo;
	GtkWidget *subject_entry;
	GtkWidget *paned;

	/* Attachments */
	GtkWidget *attach_scrwin;
	GtkWidget *attach_clist;
	GtkWidget *attach_label;

	/* Others */
	GtkWidget *savemsg_checkbtn;
	GtkWidget *savemsg_combo;

	/* Headers notebook */
	GtkWidget *notebook;

	/* Textedit */
	GtkWidget *edit_vbox;
	GtkWidget *ruler_hbox;
	GtkWidget *ruler;
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkWidget *from_name;
#if !GTK_CHECK_VERSION(2,12,0)
	GtkTooltips *tooltips;
#endif

	GtkWidget *focused_editable;

	GtkWidget *popupmenu;

	GtkWidget *tmpl_menu;

	ComposeMode mode;

	MsgInfo *targetinfo;
	MsgInfo *replyinfo;
	MsgInfo *autosaved_draft;
	MsgInfo *fwdinfo;

	GtkWidget *header_table;
	GSList    *header_list;
	guint	   header_nextrow;
	ComposeHeaderEntry *header_last;

	GHashTable *email_hashtable;

	gchar	*replyto;
	gchar	*cc;
	gchar	*bcc;
	gchar	*newsgroups;
	gchar	*followup_to;

	gchar	*ml_post;

	gchar	*inreplyto;
	gchar	*references;
	gchar	*msgid;
	gchar	*boundary;

	gboolean autowrap;
	gboolean autoindent;

	gboolean use_to;
	gboolean use_cc;
	gboolean use_bcc;
	gboolean use_replyto;
	gboolean use_newsgroups;
	gboolean use_followupto;
	gboolean use_attach;

	CharSet out_encoding;

	/* privacy settings */
	gboolean use_signing;
	gboolean use_encryption;
	gchar *privacy_system;

	gboolean modified;

	gboolean sending;
	
	gboolean return_receipt;

	gboolean batch;
	
	GSList *to_list;
	GSList *newsgroup_list;

	PrefsAccount *account;
	FolderItem *folder;

	UndoMain *undostruct;

	gchar *sig_str;
	gboolean sig_inserted;

	/* external editor */
	gchar      *exteditor_file;
	pid_t       exteditor_pid;
	GIOChannel *exteditor_ch;
	gint        exteditor_tag;

 	/* Priority */
 	gint priority;

	gchar *redirect_filename;
	
	gboolean remove_references;

	guint draft_timeout_tag;
	
	GtkTextTag *no_wrap_tag;
	GtkTextTag *no_join_tag;
	GtkTextTag *signature_tag;
	GtkTextTag *quote0_tag;
	GtkTextTag *quote1_tag;
	GtkTextTag *quote2_tag;
	GtkTextTag *uri_tag;

	gboolean automatic_break;
	GMutex *mutex;
	gchar *orig_charset;
	gint set_cursor_pos;
	
	gboolean updating;
	gboolean deferred_destroy;
	ComposeMode rmode;
	GtkWidget *first_combo;
	GtkWidget *first_entry;
	
	GtkUIManager *ui_manager;
#if USE_ENCHANT
        /* GNU/aspell spell checker */
        GtkAspell *gtkaspell;
	GtkWidget *aspell_options_menu;
#endif
};

struct _AttachInfo
{
	gchar *file;
	gchar *content_type;
	EncodingType encoding;
	gchar *name;
	goffset size;
	gchar *charset;
};

typedef enum
{
	COMPOSE_QUIT_EDITING,
	COMPOSE_KEEP_EDITING,
	COMPOSE_AUTO_SAVE,
	COMPOSE_DRAFT_FOR_EXIT
} ComposeDraftAction;

/*#warning FIXME_GTK2 */
/* attache_files will be locale encode */
Compose *compose_new			(PrefsAccount	*account,
				 	 const gchar	*mailto,
				 	 GList		*attach_files);

Compose *compose_new_with_folderitem	(PrefsAccount	*account,
					 FolderItem	*item,
					 const gchar	*mailto);

Compose *compose_new_with_list		(PrefsAccount	*account,
					 GList          *listAddress);

Compose *compose_forward		(PrefsAccount *account,
					 MsgInfo	*msginfo,
					 gboolean	 as_attach,
					 const gchar	*body,
					 gboolean	 no_extedit,
					 gboolean	 batch);

Compose *compose_redirect		(PrefsAccount	*account,
					 MsgInfo	*msginfo,
					 gboolean	 batch);
Compose *compose_reedit			(MsgInfo	*msginfo,
					 gboolean	 batch);

GList *compose_get_compose_list		(void);

void compose_entry_append		(Compose	  *compose,
					 const gchar	  *address,
					 ComposeEntryType  type,
					 ComposePrefType   pref_type);


gint compose_send			(Compose	  *compose);

void compose_update_actions_menu	(Compose	*compose);
void compose_reflect_prefs_all			(void);
void compose_reflect_prefs_pixmap_theme	(void);

void compose_destroy_all                (void);
gboolean compose_draft	                (gpointer data, guint action);
void compose_toolbar_cb			(gint 		action, 
					 gpointer 	data);
void compose_reply_to_address		(MessageView	*msgview,
					 MsgInfo	*msginfo,
					 const gchar	*address);
void compose_reply_from_messageview	(MessageView 	*msgview, 
					 GSList 	*msginfo_list, 
					 guint 		 action);
void compose_action_cb			(void 		*data);

void compose_set_position				(Compose	*compose,
						 gint		 pos);
gboolean compose_search_string			(Compose	*compose,
						 const gchar	*str,
						 gboolean	 case_sens);
gboolean compose_search_string_backward	(Compose	*compose,
						 const gchar	*str,
						 gboolean	 case_sens);
gint compose_queue			(Compose *compose, 
					 gint *msgnum, 
					 FolderItem **item, 
					 gchar **msgpath,
					 gboolean remove_reedit_target);
gboolean compose_close			(Compose *compose);
void compose_close_toolbar		(Compose *compose);
void compose_clear_exit_drafts		(void);
void compose_reopen_exit_drafts		(void);
void compose_attach_from_list (Compose *compose, GList *file_list, gboolean free_data);
void compose_check_for_email_account(Compose *compose);
#endif /* __COMPOSE_H__ */
