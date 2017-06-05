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

#ifndef __PROCMSG_H__
#define __PROCMSG_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#include <glib.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <string.h>
#include "utils.h"
#include "proctypes.h"

#define MSG_NEW			(1U << 0)
#define MSG_UNREAD		(1U << 1)
#define MSG_MARKED		(1U << 2)
#define MSG_DELETED		(1U << 3)
#define MSG_REPLIED		(1U << 4)
#define MSG_FORWARDED		(1U << 5)

#define MSG_CLABEL_SBIT	(6)		/* start bit of color label */
/* color labels use 4 bits: 6, 7, 8, 9; bit weight is 7<8<9<6,
  IOW the color label value itself must be computed from MsgPermFlags
  bits like this:
  value =   bit-7-value*2^0
          + bit-8-value*2^1
          + bit-9-value*2^2
          + bit-6-value*2^3 */

/* color label permflags masks */
#define MAKE_MSG_CLABEL(x, h, m, l)	(\
					 ((x) << (MSG_CLABEL_SBIT + 0)) | \
					 ((h) << (MSG_CLABEL_SBIT + 3)) | \
					 ((m) << (MSG_CLABEL_SBIT + 2)) | \
					 ((l) << (MSG_CLABEL_SBIT + 1)))
#define MSG_CLABEL_NONE		MAKE_MSG_CLABEL(0U, 0U, 0U, 0U)
#define MSG_CLABEL_1		MAKE_MSG_CLABEL(0U, 0U, 0U, 1U)
#define MSG_CLABEL_2		MAKE_MSG_CLABEL(0U, 0U, 1U, 0U)
#define MSG_CLABEL_3		MAKE_MSG_CLABEL(0U, 0U, 1U, 1U)
#define MSG_CLABEL_4		MAKE_MSG_CLABEL(0U, 1U, 0U, 0U)
#define MSG_CLABEL_5		MAKE_MSG_CLABEL(0U, 1U, 0U, 1U)
#define MSG_CLABEL_6		MAKE_MSG_CLABEL(0U, 1U, 1U, 0U)
#define MSG_CLABEL_7		MAKE_MSG_CLABEL(0U, 1U, 1U, 1U)
#define MSG_CLABEL_8		MAKE_MSG_CLABEL(1U, 0U, 0U, 0U)
#define MSG_CLABEL_9		MAKE_MSG_CLABEL(1U, 0U, 0U, 1U)
#define MSG_CLABEL_10		MAKE_MSG_CLABEL(1U, 0U, 1U, 0U)
#define MSG_CLABEL_11		MAKE_MSG_CLABEL(1U, 0U, 1U, 1U)
#define MSG_CLABEL_12		MAKE_MSG_CLABEL(1U, 1U, 0U, 0U)
#define MSG_CLABEL_13		MAKE_MSG_CLABEL(1U, 1U, 0U, 1U)
#define MSG_CLABEL_14		MAKE_MSG_CLABEL(1U, 1U, 1U, 0U)
#define MSG_CLABEL_15		MAKE_MSG_CLABEL(1U, 1U, 1U, 1U)
#define MSG_CLABEL_FLAG_MASK	(MSG_CLABEL_15)

#define MSG_IGNORE_THREAD	(1U << 10)   /* ignore threads */
#define MSG_LOCKED		(1U << 11)   /* msg is locked  */
#define MSG_RETRCPT_SENT	(1U << 12)   /* new one */ 
#define MSG_SPAM		(1U << 13)   /* new one */ 
#define MSG_POSTFILTERED	(1U << 14)
#define MSG_WATCH_THREAD	(1U << 15)   /* watch threads */
#define MSG_FULLY_CACHED	(1U << 16)   /* IMAP: fully cached */
#define MSG_RETRCPT_GOT		(1U << 17)   /* got return receipt */
 	
/* RESERVED */
#define	MSG_RESERVED_CLAWS	(1U << 30)   /* for claws-mail */
#define	MSG_RESERVED		(1U << 31)

#define MSG_MOVE		(1U << 0)
#define MSG_COPY		(1U << 1)
#define MSG_MOVE_DONE		(1U << 15)		
#define MSG_QUEUED		(1U << 16)
#define MSG_DRAFT		(1U << 17)
#define MSG_ENCRYPTED		(1U << 18)
#define MSG_IMAP		(1U << 19)
#define MSG_NEWS		(1U << 20)
#define MSG_SIGNED		(1U << 21)
#define MSG_MULTIPART		(1U << 29)
#define MSG_HAS_ATTACHMENT	(1U << 30)
#define MSG_SCANNED		(1U << 31)

#define MSG_CACHED_FLAG_MASK	(MSG_MULTIPART | MSG_ENCRYPTED | MSG_SIGNED | MSG_HAS_ATTACHMENT | MSG_SCANNED)

#define MSG_SET_FLAGS(msg, flags)	{ (msg) |= (flags); }
#define MSG_UNSET_FLAGS(msg, flags)	{ (msg) &= ~(flags); }
#define MSG_SET_PERM_FLAGS(msg, flags) \
	MSG_SET_FLAGS((msg).perm_flags, flags)
#define MSG_SET_TMP_FLAGS(msg, flags) \
	MSG_SET_FLAGS((msg).tmp_flags, flags)
#define MSG_UNSET_PERM_FLAGS(msg, flags) \
	MSG_UNSET_FLAGS((msg).perm_flags, flags)
#define MSG_UNSET_TMP_FLAGS(msg, flags) \
	MSG_UNSET_FLAGS((msg).tmp_flags, flags)

#define MSG_IS_NEW(msg)			(((msg).perm_flags & MSG_NEW) != 0)
#define MSG_IS_UNREAD(msg)		(((msg).perm_flags & MSG_UNREAD) != 0)
#define MSG_IS_MARKED(msg)		(((msg).perm_flags & MSG_MARKED) != 0)
#define MSG_IS_DELETED(msg)		(((msg).perm_flags & MSG_DELETED) != 0)
#define MSG_IS_REPLIED(msg)		(((msg).perm_flags & MSG_REPLIED) != 0)
#define MSG_IS_LOCKED(msg)		(((msg).perm_flags & MSG_LOCKED) != 0)
#define MSG_IS_FORWARDED(msg)		(((msg).perm_flags & MSG_FORWARDED) != 0)
#define MSG_IS_POSTFILTERED(msg)	(((msg).perm_flags & MSG_POSTFILTERED) != 0)

/* color label decoding/encoding (permflag storage bits <-> color list index value)*/
#define MSG_COLORLABEL_TO_FLAGS(val) (((((guint)(val)) & 7) << (MSG_CLABEL_SBIT+1)) \
									  | (((guint)(val) & 8) << (MSG_CLABEL_SBIT-3)))
#define MSG_COLORLABEL_FROM_FLAGS(val) ((((guint)(val) >> (MSG_CLABEL_SBIT+1)) & 7 ) \
										| (((guint)(val) >> (MSG_CLABEL_SBIT-3)) & 8))
#define MSG_GET_COLORLABEL(msg)		(((msg).perm_flags & MSG_CLABEL_FLAG_MASK))
#define MSG_GET_COLORLABEL_VALUE(msg)	(MSG_COLORLABEL_FROM_FLAGS(MSG_GET_COLORLABEL(msg)))
#define MSG_SET_COLORLABEL_VALUE(msg, val)	MSG_SET_PERM_FLAGS(msg, MSG_COLORLABEL_TO_FLAGS(val))

#define MSG_IS_MOVE(msg)		(((msg).tmp_flags & MSG_MOVE) != 0)
#define MSG_IS_COPY(msg)		(((msg).tmp_flags & MSG_COPY) != 0)
#define MSG_IS_MOVE_DONE(msg)		(((msg).tmp_flags & MSG_MOVE_DONE) != 0)

#define MSG_IS_QUEUED(msg)		(((msg).tmp_flags & MSG_QUEUED) != 0)
#define MSG_IS_DRAFT(msg)		(((msg).tmp_flags & MSG_DRAFT) != 0)
#define MSG_IS_ENCRYPTED(msg)		(((msg).tmp_flags & MSG_ENCRYPTED) != 0)
#define MSG_IS_SIGNED(msg)		(((msg).tmp_flags & MSG_SIGNED) != 0)
#define MSG_IS_IMAP(msg)		(((msg).tmp_flags & MSG_IMAP) != 0)
#define MSG_IS_NEWS(msg)		(((msg).tmp_flags & MSG_NEWS) != 0)
#define MSG_IS_MULTIPART(msg)		(((msg).tmp_flags & MSG_MULTIPART) != 0)
#define MSG_IS_WITH_ATTACHMENT(msg)	(((msg).tmp_flags & MSG_HAS_ATTACHMENT) != 0)
#define MSG_IS_SCANNED(msg)		(((msg).tmp_flags & MSG_SCANNED) != 0)

/* Claws related flags */
#define MSG_IS_REALLY_DELETED(msg)	(((msg).perm_flags & MSG_REALLY_DELETED) != 0)
#define MSG_IS_IGNORE_THREAD(msg)	(((msg).perm_flags & MSG_IGNORE_THREAD) != 0)
#define MSG_IS_RETRCPT_PENDING(msg)	(((msg).perm_flags & MSG_RETRCPT_PENDING) != 0)
#define MSG_IS_RETRCPT_SENT(msg)	(((msg).perm_flags & MSG_RETRCPT_SENT) != 0)
#define MSG_IS_RETRCPT_GOT(msg)		(((msg).perm_flags & MSG_RETRCPT_GOT) != 0)
#define MSG_IS_SPAM(msg)		(((msg).perm_flags & MSG_SPAM) != 0)
#define MSG_IS_WATCH_THREAD(msg)	(((msg).perm_flags & MSG_WATCH_THREAD) != 0)
#define MSG_IS_FULLY_CACHED(msg)	(((msg).perm_flags & MSG_FULLY_CACHED) != 0)

#define MSGINFO_UPDATE_HOOKLIST "msginfo_update"
#define MAIL_FILTERING_HOOKLIST "mail_filtering_hooklist"
#define MAIL_LISTFILTERING_HOOKLIST "mail_listfiltering_hooklist"
#define MAIL_POSTFILTERING_HOOKLIST "mail_postfiltering_hooklist"

typedef enum {
	MSGINFO_UPDATE_FLAGS = 1 << 0,
	MSGINFO_UPDATE_DELETED = 1 << 1
} MsgInfoUpdateFlags;

#include "prefs_account.h"
#include "prefs_filtering.h"
#include "folder.h"

struct _MsgFlags
{
	MsgPermFlags perm_flags;
	MsgTmpFlags  tmp_flags;
};

typedef enum {
	IS_NOTHING = 0,
	IS_MOVE,
	IS_COPY,
	IS_DELE
} FiltOp;

/* *********************************************************** *
 * WARNING: When adding or removing members to this structure, *
 * be sure to update procmsg.c::procmsg_msginfo_memusage()  to *
 * avoid underestimating cache memory usage - especially since *
 * this would cause an overflow and metadata loss when writing *
 * the cache to disk.                                          *
 * *********************************************************** */
struct _MsgInfo
{
	guint refcnt;

	guint  msgnum;
	goffset  size;
	time_t mtime;
	time_t date_t;
	time_t thread_date;

	MsgFlags flags;

	gchar *fromname;

	gchar *date;
	gchar *from;
	gchar *to;
	gchar *cc;
	gchar *newsgroups;
	gchar *subject;
	gchar *msgid;
	gchar *inreplyto;
	gchar *xref;

	FolderItem *folder;
	FolderItem *to_folder;

	FolderItem *to_filter_folder;	
	FiltOp filter_op;

	GSList *references;
	gchar *fromspace;

	gint score;

	/* used only for encrypted messages */
	gchar *plaintext_file;
        
        gint hidden;

	/* used only for partially received messages */
	gint total_size;
	gint planned_download;

	/* list of tags ids */
	GSList *tags;

	MsgInfoExtraData *extradata;
};

struct _MsgInfoExtraData
{
	gchar *xface;
	gchar *face;

	gchar *dispositionnotificationto;
	gchar *returnreceiptto;

	/* used only for partially received messages */
	gchar *partial_recv;
	gchar *account_server;
	gchar *account_login;

 	/* Mailing list support */
 	gchar *list_post;
 	gchar *list_subscribe;
 	gchar *list_unsubscribe;
 	gchar *list_help;
 	gchar *list_archive;
 	gchar *list_owner;
};

struct _MsgFileInfo
{
	MsgInfo *msginfo;
        gchar *file;
        MsgFlags *flags;
};

struct _MsgInfoUpdate {
	MsgInfo	*msginfo;
	MsgInfoUpdateFlags flags;
};

struct _MailFilteringData
{
	MsgInfo	*msginfo;
	GSList  *msglist;
	GSList  *filtered;
	GSList  *unfiltered;
	PrefsAccount *account;
};

GSList *procmsg_read_cache		(FolderItem	*item,
					 gboolean	 scan_file);
void	procmsg_msg_list_free		(MsgInfoList	*mlist);
MsgNumberList *procmsg_get_number_list_for_msgs(MsgInfoList *msglist);
void	procmsg_get_mark_sum		(const gchar	*folder,
					 gint		*new_msgs,
					 gint		*unread_msgs,
					 gint		*total_msgs,
					 gint		*min,
					 gint		*max,
					 gint		 first);

GNode  *procmsg_get_thread_tree		(GSList		*mlist);

gint	procmsg_move_messages		(GSList		*mlist);
void	procmsg_copy_messages		(GSList		*mlist);

/* return path is locale charset */
gchar  *procmsg_get_message_file_path	(MsgInfo	*msginfo);
gchar  *procmsg_get_message_file	(MsgInfo	*msginfo);
gchar  *procmsg_get_message_file_full	(MsgInfo	*msginfo, 
					 gboolean 	 get_headers,
					 gboolean	 get_body);
GSList *procmsg_get_message_file_list	(MsgInfoList	*mlist);
void	procmsg_message_file_list_free	(MsgInfoList	*file_list);
FILE   *procmsg_open_message		(MsgInfo	*msginfo);
gboolean procmsg_msg_exist		(MsgInfo	*msginfo);

void	procmsg_get_filter_keyword	(MsgInfo	  *msginfo,
					 gchar	         **header,
					 gchar	         **key,
					 PrefsFilterType   type);

void	procmsg_empty_all_trash		(void);

gint	procmsg_send_queue		(FolderItem	*queue,
					 gboolean	 save_msgs,
					 gchar		**errstr);
gboolean procmsg_queue_lock		(gchar		**errstr);
void     procmsg_queue_unlock		(void);
gboolean procmsg_queue_is_empty	(FolderItem *queue);
void	procmsg_print_message		(MsgInfo	*msginfo,
					 const gchar	*cmdline);

MsgInfo *procmsg_msginfo_new		();
MsgInfo *procmsg_msginfo_new_ref	(MsgInfo 	*msginfo);
MsgInfo *procmsg_msginfo_copy		(MsgInfo	*msginfo);
MsgInfo *procmsg_msginfo_get_full_info	(MsgInfo	*msginfo);
MsgInfo *procmsg_msginfo_get_full_info_from_file
					(MsgInfo *msginfo, 
					const gchar *file);
void	 procmsg_msginfo_free		(MsgInfo	*msginfo);
guint	 procmsg_msginfo_memusage	(MsgInfo	*msginfo);

gint procmsg_send_message_queue_with_lock(const gchar *file,
					  gchar **errstr,
					  FolderItem *queue,
					  gint msgnum,
					  gboolean *queued_removed);

gint procmsg_send_message_queue		(const gchar *file,
					 gchar **errstr,
					 FolderItem *queue, 
					 gint msgnum,
					 gboolean *queued_removed);

void procmsg_msginfo_set_flags		(MsgInfo *msginfo,
					 MsgPermFlags perm_flags,
					 MsgTmpFlags tmp_flags);
void procmsg_msginfo_unset_flags	(MsgInfo *msginfo,
					 MsgPermFlags perm_flags,
					 MsgTmpFlags tmp_flags);
void procmsg_msginfo_change_flags	(MsgInfo *msginfo, 
					 MsgPermFlags add_perm_flags, 
					 MsgTmpFlags add_tmp_flags,
					 MsgPermFlags rem_perm_flags, 
					 MsgTmpFlags rem_tmp_flags);
gint procmsg_remove_special_headers	(const gchar 	*in, 
					 const gchar 	*out);

gboolean procmsg_msg_has_flagged_parent	(MsgInfo 	*info,
					 MsgPermFlags    perm_flags);
gboolean procmsg_msg_has_marked_parent	(MsgInfo	*info);
void procmsg_msginfo_set_to_folder	(MsgInfo 	*msginfo,
					 FolderItem 	*to_folder);
void procmsg_msglist_filter		(GSList 	*list, 
					 PrefsAccount 	*ac, 
					 GSList 	**filtered,
					 GSList 	**unfiltered,
					 gboolean 	 do_filter);

MsgInfo *procmsg_msginfo_new_from_mimeinfo
					(MsgInfo 	*src_msginfo, 
					 MimeInfo	*mimeinfo);

void procmsg_register_spam_learner (int (*learn_func)(MsgInfo *info, GSList *list, gboolean spam));
void procmsg_unregister_spam_learner (int (*learn_func)(MsgInfo *info, GSList *list, gboolean spam));
gboolean procmsg_spam_can_learn		(void);
void procmsg_spam_set_folder		(const char *item_identifier, FolderItem *(*spam_get_folder_func)(MsgInfo *info));
FolderItem *procmsg_spam_get_folder	(MsgInfo *msginfo);
int procmsg_spam_learner_learn 	(MsgInfo *msginfo, GSList *msglist, gboolean spam);
gboolean procmsg_have_queued_mails_fast (void);
gboolean procmsg_have_trashed_mails_fast (void);
gboolean procmsg_is_sending(void);
gchar *procmsg_msginfo_get_tags_str(MsgInfo *msginfo);
void procmsg_msginfo_update_tags(MsgInfo *msginfo, gboolean set, gint id);
void procmsg_msginfo_clear_tags(MsgInfo *msginfo);
void procmsg_msginfo_commit_tags(GSList *msglist);
MsgInfo *procmsg_get_msginfo_from_identifier(const gchar *id);
gchar *procmsg_msginfo_get_identifier(MsgInfo *msginfo);
#endif /* __PROCMSG_H__ */
