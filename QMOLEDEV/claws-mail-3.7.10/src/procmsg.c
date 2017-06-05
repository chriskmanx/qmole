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

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "main.h"
#include "utils.h"
#include "procmsg.h"
#include "procheader.h"
#include "send_message.h"
#include "procmime.h"
#include "statusbar.h"
#include "prefs_filtering.h"
#include "filtering.h"
#include "folder.h"
#include "prefs_common.h"
#include "account.h"
#include "alertpanel.h"
#include "news.h"
#include "hooks.h"
#include "msgcache.h"
#include "partial_download.h"
#include "mainwindow.h"
#include "summaryview.h"
#include "log.h"
#include "tags.h"
#include "timing.h"
#include "inc.h"

static gint procmsg_send_message_queue_full(const gchar *file, gboolean keep_session, gchar **errstr,
					    FolderItem *queue, gint msgnum, gboolean *queued_removed);
static void procmsg_update_unread_children	(MsgInfo 	*info,
					 gboolean 	 newly_marked);
enum
{
	Q_SENDER           = 0,
	Q_SMTPSERVER       = 1,
	Q_RECIPIENTS       = 2,
	Q_NEWSGROUPS       = 3,
	Q_MAIL_ACCOUNT_ID  = 4,
	Q_NEWS_ACCOUNT_ID  = 5,
	Q_SAVE_COPY_FOLDER = 6,
	Q_REPLY_MESSAGE_ID = 7,
	Q_FWD_MESSAGE_ID   = 8,
	Q_PRIVACY_SYSTEM   = 9,
	Q_ENCRYPT 	   = 10,
	Q_ENCRYPT_DATA	   = 11,
	Q_CLAWS_HDRS       = 12,
	Q_PRIVACY_SYSTEM_OLD = 13,
	Q_ENCRYPT_OLD 	     = 14,
	Q_ENCRYPT_DATA_OLD   = 15,
	Q_CLAWS_HDRS_OLD     = 16,
};

void procmsg_msg_list_free(GSList *mlist)
{
	GSList *cur;
	MsgInfo *msginfo;

	for (cur = mlist; cur != NULL; cur = cur->next) {
		msginfo = (MsgInfo *)cur->data;
		procmsg_msginfo_free(msginfo);
	}
	g_slist_free(mlist);
}

struct MarkSum {
	gint *new_msgs;
	gint *unread_msgs;
	gint *total_msgs;
	gint *min;
	gint *max;
	gint first;
};

/* CLAWS subject threading:
  
  in the first round it inserts subject lines in a 
  hashtable (subject <-> node)

  the second round finishes the threads by attaching
  matching subject lines to the one found in the
  hashtable. will use the oldest node with the same
  subject that is not more then thread_by_subject_max_age
  days old (see subject_hashtable_lookup)
*/  

static void subject_hashtable_insert(GHashTable *hashtable, GNode *node)
{
	gchar *subject;
	MsgInfo *msginfo;
	GSList *list = NULL;

	cm_return_if_fail(hashtable != NULL);
	cm_return_if_fail(node != NULL);
	msginfo = (MsgInfo *) node->data;
	cm_return_if_fail(msginfo != NULL);

	subject = msginfo->subject;
	if (subject == NULL)
		return;

	subject += subject_get_prefix_length(subject);

	list = g_hash_table_lookup(hashtable, subject);
	list = g_slist_prepend(list, node);
	g_hash_table_insert(hashtable, subject, list);
}

static GNode *subject_hashtable_lookup(GHashTable *hashtable, MsgInfo *msginfo)
{
	gchar *subject;
	GSList *list, *cur;
	GNode *node = NULL, *hashtable_node = NULL;
	gint prefix_length;
	MsgInfo *hashtable_msginfo = NULL, *best_msginfo = NULL;
	gboolean match;
    
	cm_return_val_if_fail(hashtable != NULL, NULL);

	subject = msginfo->subject;
	if (subject == NULL)
		return NULL;
	prefix_length = subject_get_prefix_length(subject);
	if (prefix_length <= 0)
		return NULL;
	subject += prefix_length;
	
	list = g_hash_table_lookup(hashtable, subject);
	if (list == NULL)
		return NULL;

	/* check all nodes with the same subject to find the best parent */
	for (cur = list; cur; cur = cur->next) {
		hashtable_node = (GNode *)cur->data;
		hashtable_msginfo = (MsgInfo *) hashtable_node->data;
		match = FALSE;

		/* best node should be the oldest in the found nodes */
		/* parent node must not be older then msginfo */
		if ((hashtable_msginfo->date_t < msginfo->date_t) &&
		    ((best_msginfo == NULL) ||
		     (best_msginfo->date_t > hashtable_msginfo->date_t)))
			match = TRUE;

		/* parent node must not be more then thread_by_subject_max_age
		   days older then msginfo */
		if (abs(difftime(msginfo->date_t, hashtable_msginfo->date_t)) >
                    prefs_common.thread_by_subject_max_age * 3600 * 24)
			match = FALSE;

		/* can add new tests for all matching
		   nodes found by subject */

		if (match) {
			node = hashtable_node;
			best_msginfo = hashtable_msginfo;
		}
	}

	return node;
}

static void subject_hashtable_free(gpointer key, gpointer value, gpointer data)
{
	g_slist_free(value);
}

/* return the reversed thread tree */
GNode *procmsg_get_thread_tree(GSList *mlist)
{
	GNode *root, *parent, *node, *next;
	GHashTable *msgid_table;
	GHashTable *subject_hashtable = NULL;
	MsgInfo *msginfo;
	const gchar *msgid;
        GSList *reflist;
	START_TIMING("");
	root = g_node_new(NULL);
	msgid_table = g_hash_table_new(g_str_hash, g_str_equal);
	
	if (prefs_common.thread_by_subject) {
		subject_hashtable = g_hash_table_new(g_str_hash, g_str_equal);
	}

	for (; mlist != NULL; mlist = mlist->next) {
		msginfo = (MsgInfo *)mlist->data;
		parent = root;

		if (msginfo->inreplyto) {
			parent = g_hash_table_lookup(msgid_table, msginfo->inreplyto);
			if (parent == NULL) {
				parent = root;
			}
		}
		node = g_node_insert_data_before
			(parent, parent == root ? parent->children : NULL,
			 msginfo);
		if ((msgid = msginfo->msgid) && g_hash_table_lookup(msgid_table, msgid) == NULL)
			g_hash_table_insert(msgid_table, (gchar *)msgid, node);

		/* CLAWS: add subject to hashtable (without prefix) */
		if (prefs_common.thread_by_subject) {
			subject_hashtable_insert(subject_hashtable, node);
		}
	}

	/* complete the unfinished threads */
	for (node = root->children; node != NULL; ) {
		next = node->next;
		msginfo = (MsgInfo *)node->data;
		parent = NULL;
		
                if (msginfo->inreplyto)
			parent = g_hash_table_lookup(msgid_table, msginfo->inreplyto);

		/* try looking for the indirect parent */
		if (!parent && msginfo->references) {
			for (reflist = msginfo->references;
			     reflist != NULL; reflist = reflist->next)
				if ((parent = g_hash_table_lookup
					(msgid_table, reflist->data)) != NULL)
					break;
                }                                        
              
		/* node should not be the parent, and node should not
		   be an ancestor of parent (circular reference) */
		if (parent && parent != node &&
		    !g_node_is_ancestor(node, parent)) {
			g_node_unlink(node);
			g_node_insert_before
				(parent, parent->children, node);
		}
               
		node = next;
	}

	if (prefs_common.thread_by_subject) {
		START_TIMING("thread by subject");
		for (node = root->children; node && node != NULL;) {
			next = node->next;
			msginfo = (MsgInfo *) node->data;
			
			parent = subject_hashtable_lookup(subject_hashtable, msginfo);
			
			/* the node may already be threaded by IN-REPLY-TO, so go up 
			 * in the tree to 
			   find the parent node */
			if (parent != NULL) {
				if (g_node_is_ancestor(node, parent))
					parent = NULL;
				if (parent == node)
					parent = NULL;
			}
			
			if (parent) {
				g_node_unlink(node);
				g_node_append(parent, node);
			}

			node = next;
		}	
		END_TIMING();
	}
	
	if (prefs_common.thread_by_subject)
	{
		g_hash_table_foreach(subject_hashtable, subject_hashtable_free, NULL);
		g_hash_table_destroy(subject_hashtable);
	}

	g_hash_table_destroy(msgid_table);
	END_TIMING();
	return root;
}

gint procmsg_move_messages(GSList *mlist)
{
	GSList *cur, *movelist = NULL;
	MsgInfo *msginfo;
	FolderItem *dest = NULL;
	gint retval = 0;
	gboolean finished = TRUE;
	if (!mlist) return 0;

	folder_item_update_freeze();

next_folder:
	for (cur = mlist; cur != NULL; cur = cur->next) {
		msginfo = (MsgInfo *)cur->data;
		if (!msginfo->to_folder) {
			continue;
		} else {
			finished = FALSE;
		}
		if (!dest) {
			dest = msginfo->to_folder;
			movelist = g_slist_prepend(movelist, msginfo);
		} else if (dest == msginfo->to_folder) {
			movelist = g_slist_prepend(movelist, msginfo);
		} else {
			continue;
		}
		procmsg_msginfo_set_to_folder(msginfo, NULL);
	}
	if (movelist) {
		movelist = g_slist_reverse(movelist);
		retval |= folder_item_move_msgs(dest, movelist);
		g_slist_free(movelist);
		movelist = NULL;
	}
	if (finished == FALSE) {
		finished = TRUE;
		dest = NULL;
		goto next_folder;
	}

	folder_item_update_thaw();
	return retval;
}

void procmsg_copy_messages(GSList *mlist)
{
	GSList *cur, *copylist = NULL;
	MsgInfo *msginfo;
	FolderItem *dest = NULL;
	gboolean finished = TRUE;
	if (!mlist) return;

	folder_item_update_freeze();

next_folder:
	for (cur = mlist; cur != NULL; cur = cur->next) {
		msginfo = (MsgInfo *)cur->data;
		if (!msginfo->to_folder) {
			continue;
		} else {
			finished = FALSE;
		}
		if (!dest) {
			dest = msginfo->to_folder;
			copylist = g_slist_prepend(copylist, msginfo);
		} else if (dest == msginfo->to_folder) {
			copylist = g_slist_prepend(copylist, msginfo);
		} else {
			continue;
		}
		procmsg_msginfo_set_to_folder(msginfo, NULL);
	}
	if (copylist) {
		copylist = g_slist_reverse(copylist);
		folder_item_copy_msgs(dest, copylist);
		g_slist_free(copylist);
		copylist = NULL;
	}
	if (finished == FALSE) {
		finished = TRUE;
		dest = NULL;
		goto next_folder;
	}

	folder_item_update_thaw();
}

gchar *procmsg_get_message_file_path(MsgInfo *msginfo)
{
	gchar *file;

	cm_return_val_if_fail(msginfo != NULL, NULL);

	if (msginfo->plaintext_file)
		file = g_strdup(msginfo->plaintext_file);
	else {
		file = folder_item_fetch_msg(msginfo->folder, msginfo->msgnum);
	}

	return file;
}

gchar *procmsg_get_message_file(MsgInfo *msginfo)
{
	gchar *filename = NULL;

	cm_return_val_if_fail(msginfo != NULL, NULL);

	filename = folder_item_fetch_msg(msginfo->folder, msginfo->msgnum);
	if (!filename)
		debug_print("can't fetch message %d\n", msginfo->msgnum);

	return filename;
}

gchar *procmsg_get_message_file_full(MsgInfo *msginfo, gboolean headers, gboolean body)
{
	gchar *filename = NULL;

	cm_return_val_if_fail(msginfo != NULL, NULL);

	filename = folder_item_fetch_msg_full(msginfo->folder, msginfo->msgnum,
						headers, body);
	if (!filename)
		debug_print("can't fetch message %d\n", msginfo->msgnum);

	return filename;
}

GSList *procmsg_get_message_file_list(GSList *mlist)
{
        GSList *file_list = NULL;
        MsgInfo *msginfo;
        MsgFileInfo *fileinfo;
        gchar *file;

        while (mlist != NULL) {
                msginfo = (MsgInfo *)mlist->data;
                file = procmsg_get_message_file(msginfo);
                if (!file) {
                        procmsg_message_file_list_free(file_list);
                        return NULL;
                }
                fileinfo = g_new(MsgFileInfo, 1);
		fileinfo->msginfo = procmsg_msginfo_new_ref(msginfo);
                fileinfo->file = file;
                fileinfo->flags = g_new(MsgFlags, 1);
                *fileinfo->flags = msginfo->flags;
                file_list = g_slist_prepend(file_list, fileinfo);
                mlist = mlist->next;
        }

        file_list = g_slist_reverse(file_list);

        return file_list;
}

void procmsg_message_file_list_free(MsgInfoList *file_list)
{
	GSList *cur;
	MsgFileInfo *fileinfo;

	for (cur = file_list; cur != NULL; cur = cur->next) {
		fileinfo = (MsgFileInfo *)cur->data;
		procmsg_msginfo_free(fileinfo->msginfo);
		g_free(fileinfo->file);
		g_free(fileinfo->flags);
		g_free(fileinfo);
	}

	g_slist_free(file_list);
}

FILE *procmsg_open_message(MsgInfo *msginfo)
{
	FILE *fp;
	gchar *file;

	cm_return_val_if_fail(msginfo != NULL, NULL);
	
	file = procmsg_get_message_file_path(msginfo);
	cm_return_val_if_fail(file != NULL, NULL);

	if (!is_file_exist(file)) {
		g_free(file);
		file = procmsg_get_message_file(msginfo);
		if (!file)
			return NULL;
	}

	if ((fp = g_fopen(file, "rb")) == NULL) {
		FILE_OP_ERROR(file, "fopen");
		g_free(file);
		return NULL;
	}

	g_free(file);

	if (MSG_IS_QUEUED(msginfo->flags) || MSG_IS_DRAFT(msginfo->flags)) {
		gchar buf[BUFFSIZE];

		while (fgets(buf, sizeof(buf), fp) != NULL) {
			/* new way */
			if ((!strncmp(buf, "X-Claws-End-Special-Headers: 1",
				strlen("X-Claws-End-Special-Headers:"))) ||
			    (!strncmp(buf, "X-Sylpheed-End-Special-Headers: 1",
				strlen("X-Sylpheed-End-Special-Headers:"))))
				break;
			/* old way */
			if (buf[0] == '\r' || buf[0] == '\n') break;
			/* from other mailers */
			if (!strncmp(buf, "Date: ", 6)
			||  !strncmp(buf, "To: ", 4)
			||  !strncmp(buf, "From: ", 6)
			||  !strncmp(buf, "Subject: ", 9)) {
				rewind(fp);
				break;
			}
		}
	}

	return fp;
}

gboolean procmsg_msg_exist(MsgInfo *msginfo)
{
	gchar *path;
	gboolean ret;

	if (!msginfo) return FALSE;

	path = folder_item_get_path(msginfo->folder);
	change_dir(path);
	ret = !folder_item_is_msg_changed(msginfo->folder, msginfo);
	g_free(path);

	return ret;
}

void procmsg_get_filter_keyword(MsgInfo *msginfo, gchar **header, gchar **key,
				PrefsFilterType type)
{
	static HeaderEntry hentry[] = {{"X-BeenThere:",    NULL, TRUE},
				       {"X-ML-Name:",      NULL, TRUE},
				       {"X-List:",         NULL, TRUE},
				       {"X-Mailing-list:", NULL, TRUE},
				       {"List-Id:",        NULL, TRUE},
				       {"X-Sequence:",	   NULL, TRUE},
				       {"Sender:",	   NULL, TRUE},
				       {"List-Post:",	   NULL, TRUE},
				       {NULL,		   NULL, FALSE}};
	enum
	{
		H_X_BEENTHERE	 = 0,
		H_X_ML_NAME      = 1,
		H_X_LIST         = 2,
		H_X_MAILING_LIST = 3,
		H_LIST_ID	 = 4,
		H_X_SEQUENCE	 = 5,
		H_SENDER	 = 6,
		H_LIST_POST	 = 7
	};

	FILE *fp;

	cm_return_if_fail(msginfo != NULL);
	cm_return_if_fail(header != NULL);
	cm_return_if_fail(key != NULL);

	*header = NULL;
	*key = NULL;

	switch (type) {
	case FILTER_BY_NONE:
		return;
	case FILTER_BY_AUTO:
		if ((fp = procmsg_open_message(msginfo)) == NULL)
			return;
		procheader_get_header_fields(fp, hentry);
		fclose(fp);

#define SET_FILTER_KEY(hstr, idx)	\
{					\
	*header = g_strdup(hstr);	\
	*key = hentry[idx].body;	\
	hentry[idx].body = NULL;	\
}

		if (hentry[H_LIST_ID].body != NULL) {
			SET_FILTER_KEY("header \"List-Id\"", H_LIST_ID);
			extract_list_id_str(*key);
		} else if (hentry[H_X_BEENTHERE].body != NULL) {
			SET_FILTER_KEY("header \"X-BeenThere\"", H_X_BEENTHERE);
		} else if (hentry[H_X_ML_NAME].body != NULL) {
			SET_FILTER_KEY("header \"X-ML-Name\"", H_X_ML_NAME);
		} else if (hentry[H_X_LIST].body != NULL) {
			SET_FILTER_KEY("header \"X-List\"", H_X_LIST);
		} else if (hentry[H_X_MAILING_LIST].body != NULL) {
			SET_FILTER_KEY("header \"X-Mailing-List\"", H_X_MAILING_LIST);
		} else  if (hentry[H_X_SEQUENCE].body != NULL) {
			gchar *p;

			SET_FILTER_KEY("X-Sequence", H_X_SEQUENCE);
			p = *key;
			while (*p != '\0') {
				while (*p != '\0' && !g_ascii_isspace(*p)) p++;
				while (g_ascii_isspace(*p)) p++;
				if (g_ascii_isdigit(*p)) {
					*p = '\0';
					break;
				}
			}
			g_strstrip(*key);
		} else if (hentry[H_SENDER].body != NULL) {
			SET_FILTER_KEY("header \"Sender\"", H_SENDER);
		} else if (hentry[H_LIST_POST].body != NULL) {
			SET_FILTER_KEY("header \"List-Post\"", H_LIST_POST);
		} else if (msginfo->to) {
			*header = g_strdup("to");
			*key = g_strdup(msginfo->to);
		} else if (msginfo->subject) {
			*header = g_strdup("subject");
			*key = g_strdup(msginfo->subject);
		}

#undef SET_FILTER_KEY

		g_free(hentry[H_X_BEENTHERE].body);
		hentry[H_X_BEENTHERE].body = NULL;
		g_free(hentry[H_X_ML_NAME].body);
		hentry[H_X_ML_NAME].body = NULL;
		g_free(hentry[H_X_LIST].body);
		hentry[H_X_LIST].body = NULL;
		g_free(hentry[H_X_MAILING_LIST].body);
		hentry[H_X_MAILING_LIST].body = NULL;
		g_free(hentry[H_LIST_ID].body);
		hentry[H_LIST_ID].body = NULL;
		g_free(hentry[H_SENDER].body);
		hentry[H_SENDER].body = NULL;
		g_free(hentry[H_LIST_POST].body);
		hentry[H_LIST_POST].body = NULL;

		break;
	case FILTER_BY_FROM:
		*header = g_strdup("from");
		*key = g_strdup(msginfo->from);
		break;
	case FILTER_BY_TO:
		*header = g_strdup("to");
		*key = g_strdup(msginfo->to);
		break;
	case FILTER_BY_SUBJECT:
		*header = g_strdup("subject");
		*key = g_strdup(msginfo->subject);
		break;
	default:
		break;
	}
}

static void procmsg_empty_trash(FolderItem *trash)
{
	GNode *node, *next;

	if (!trash || 
	    (trash->stype != F_TRASH && 
	     !folder_has_parent_of_type(trash, F_TRASH)))
		return;

	if (trash && trash->total_msgs > 0) {
		GSList *mlist = folder_item_get_msg_list(trash);
		GSList *cur;
		for (cur = mlist ; cur != NULL ; cur = cur->next) {
			MsgInfo * msginfo = (MsgInfo *) cur->data;
			if (MSG_IS_LOCKED(msginfo->flags)) {
				procmsg_msginfo_free(msginfo);
				continue;
			}
			if (msginfo->total_size != 0 && 
			    msginfo->size != (off_t)msginfo->total_size)
				partial_mark_for_delete(msginfo);

			procmsg_msginfo_free(msginfo);
		}
		g_slist_free(mlist);
		folder_item_remove_all_msg(trash);
	}

	if (!trash->node || !trash->node->children)
		return;

	node = trash->node->children;
	while (node != NULL) {
		next = node->next;
		procmsg_empty_trash(FOLDER_ITEM(node->data));
		node = next;
	}
}

void procmsg_empty_all_trash(void)
{
	FolderItem *trash;
	GList *cur;

	for (cur = folder_get_list(); cur != NULL; cur = cur->next) {
		Folder *folder = FOLDER(cur->data);
		trash = folder->trash;
		procmsg_empty_trash(trash);
		if (folder->account && folder->account->set_trash_folder && 
		    folder_find_item_from_identifier(folder->account->trash_folder))
		    	procmsg_empty_trash(
				folder_find_item_from_identifier(folder->account->trash_folder));
	}
}

static PrefsAccount *procmsg_get_account_from_file(const gchar *file)
{
	PrefsAccount *mailac = NULL;
	FILE *fp;
	int hnum;
	gchar buf[BUFFSIZE];
	static HeaderEntry qentry[] = {{"S:",    NULL, FALSE},
				       {"SSV:",  NULL, FALSE},
				       {"R:",    NULL, FALSE},
				       {"NG:",   NULL, FALSE},
				       {"MAID:", NULL, FALSE},
				       {"NAID:", NULL, FALSE},
				       {"SCF:",  NULL, FALSE},
				       {"RMID:", NULL, FALSE},
				       {"FMID:", NULL, FALSE},
				       {"X-Claws-Privacy-System:", NULL, FALSE},
				       {"X-Claws-Encrypt:", NULL, FALSE},
				       {"X-Claws-Encrypt-Data:", NULL, FALSE},
				       {"X-Claws-End-Special-Headers",    NULL, FALSE},
				       {"X-Sylpheed-Privacy-System:", NULL, FALSE},
				       {"X-Sylpheed-Encrypt:", NULL, FALSE},
				       {"X-Sylpheed-Encrypt-Data:", NULL, FALSE},
				       {NULL,    NULL, FALSE}};
	
	cm_return_val_if_fail(file != NULL, NULL);

	if ((fp = g_fopen(file, "rb")) == NULL) {
		FILE_OP_ERROR(file, "fopen");
		return NULL;
	}

	while ((hnum = procheader_get_one_field(buf, sizeof(buf), fp, qentry))
	       != -1) {
		gchar *p = buf + strlen(qentry[hnum].name);

		if (hnum == Q_MAIL_ACCOUNT_ID) {
			mailac = account_find_from_id(atoi(p));
			break;
		}
	}
	fclose(fp);
	return mailac;
}

static GSList *procmsg_list_sort_by_account(FolderItem *queue, GSList *list)
{
	GSList *result = NULL;
	GSList *orig = NULL;
	PrefsAccount *last_account = NULL;
	MsgInfo *msg = NULL;
	GSList *cur = NULL;
	gboolean nothing_to_sort = TRUE;

	if (!list)
		return NULL;

	orig = g_slist_copy(list);
	
	msg = (MsgInfo *)orig->data;
	
	for (cur = orig; cur; cur = cur->next)
		debug_print("sort before %s\n", ((MsgInfo *)cur->data)->from);
	
	debug_print("\n");

parse_again:	
	nothing_to_sort = TRUE;
	cur = orig;
	while (cur) {
		gchar *file = NULL;
		PrefsAccount *ac = NULL;
		msg = (MsgInfo *)cur->data;
		file = folder_item_fetch_msg(queue, msg->msgnum);
		ac = procmsg_get_account_from_file(file);
		g_free(file);

		if (last_account == NULL || (ac != NULL && ac == last_account)) {
			result = g_slist_append(result, msg);
			orig = g_slist_remove(orig, msg);
			last_account = ac;
			nothing_to_sort = FALSE;
			goto parse_again;
		}
		cur = cur->next;
	}
	
	if (orig || g_slist_length(orig)) {
		if (!last_account && nothing_to_sort) {
			/* can't find an account for the rest of the list */
			cur = orig;
			while (cur) {
				result = g_slist_append(result, cur->data);
				cur = cur->next;
			}
		} else {
			last_account = NULL;
			goto parse_again;
		}
	}
	
	g_slist_free(orig);
	
	for (cur = result; cur; cur = cur->next)
		debug_print("sort after %s\n", ((MsgInfo *)cur->data)->from);

	debug_print("\n");

	return result;
}

static gboolean procmsg_is_last_for_account(FolderItem *queue, MsgInfo *msginfo, GSList *elem)
{
	gchar *file = folder_item_fetch_msg(queue, msginfo->msgnum);
	PrefsAccount *ac = procmsg_get_account_from_file(file);
	GSList *cur = elem;
	g_free(file);
	for (cur = elem; cur; cur = cur->next) {
		MsgInfo *cur_msginfo = (MsgInfo *)cur->data;
		file = folder_item_fetch_msg(queue, cur_msginfo->msgnum);
		
		if (cur_msginfo != msginfo && !MSG_IS_LOCKED(cur_msginfo->flags)) {
			if (procmsg_get_account_from_file(file) == ac) {
				g_free(file);
				return FALSE;
			}
		}
		
		g_free(file);
	}
	return TRUE;
}

static gboolean send_queue_lock = FALSE;
/*!
 *\brief	Send messages in queue
 *
 *\param	queue Queue folder to process
 *\param	save_msgs Unused
 *
 *\return	Number of messages sent, negative if an error occurred
 *		positive if no error occurred
 */
gint procmsg_send_queue(FolderItem *queue, gboolean save_msgs, gchar **errstr)
{
	gint sent = 0, err = 0;
	GSList *list, *elem;
	GSList *sorted_list = NULL;
	GNode *node, *next;
	
	if (send_queue_lock) {
		/* Avoid having to translate two similar strings */
		log_warning(LOG_PROTOCOL, "%s\n", _("Already trying to send."));
		if (errstr) {
			if (*errstr) g_free(*errstr);
			*errstr = g_strdup_printf(_("Already trying to send."));
		}
		toolbar_main_set_sensitive(mainwindow_get_mainwindow());
		return -1;
	}
	send_queue_lock = TRUE;
	inc_lock();
	if (!queue)
		queue = folder_get_default_queue();
	
	if (queue == NULL) {
		send_queue_lock = FALSE;
		inc_unlock();
		return -1;
	}

	toolbar_main_set_sensitive(mainwindow_get_mainwindow());

	folder_item_scan(queue);
	list = folder_item_get_msg_list(queue);

	/* sort the list per sender account; this helps reusing the same SMTP server */
	sorted_list = procmsg_list_sort_by_account(queue, list);
	
	for (elem = sorted_list; elem != NULL; elem = elem->next) {
		gchar *file;
		MsgInfo *msginfo;
			
		msginfo = (MsgInfo *)(elem->data);
		if (!MSG_IS_LOCKED(msginfo->flags) && !MSG_IS_DELETED(msginfo->flags)) {
			file = folder_item_fetch_msg(queue, msginfo->msgnum);
			if (file) {
				gboolean queued_removed = FALSE;
				if (procmsg_send_message_queue_full(file, 
						!procmsg_is_last_for_account(queue, msginfo, elem),
						errstr, queue, msginfo->msgnum, &queued_removed) < 0) {
					g_warning("Sending queued message %d failed.\n", 
						  msginfo->msgnum);
					err++;
				} else {
					sent++; 
					if (!queued_removed)
						folder_item_remove_msg(queue, msginfo->msgnum);
				}
				g_free(file);
			}
		}
		/* FIXME: supposedly if only one message is locked, and queue
		 * is being flushed, the following free says something like 
		 * "freeing msg ## in folder (nil)". */
		procmsg_msginfo_free(msginfo);
	}

	g_slist_free(sorted_list);
	folder_item_scan(queue);

	if (queue->node && queue->node->children) {
		node = queue->node->children;
		while (node != NULL) {
			int res = 0;
			next = node->next;
			send_queue_lock = FALSE;
			res = procmsg_send_queue(FOLDER_ITEM(node->data), save_msgs, errstr);
			send_queue_lock = TRUE;
			if (res < 0) 
				err = -res;
			else
				sent += res;
			node = next;
		}
	}
	send_queue_lock = FALSE;
	inc_unlock();
	toolbar_main_set_sensitive(mainwindow_get_mainwindow());

	return (err != 0 ? -err : sent);
}

gboolean procmsg_is_sending(void)
{
	return send_queue_lock;
}

/*!
 *\brief	Determine if a queue folder is empty
 *
 *\param	queue Queue folder to process
 *
 *\return	TRUE if the queue folder is empty, otherwise return FALSE
 */
gboolean procmsg_queue_is_empty(FolderItem *queue)
{
	GSList *list;
	gboolean res = FALSE;
	if (!queue)
		queue = folder_get_default_queue();
	cm_return_val_if_fail(queue != NULL, TRUE);

	folder_item_scan(queue);
	list = folder_item_get_msg_list(queue);
	res = (list == NULL);
	procmsg_msg_list_free(list);

	if (res == TRUE) {
		GNode *node, *next;
		if (queue->node && queue->node->children) {
			node = queue->node->children;
			while (node != NULL) {
				next = node->next;
				if (!procmsg_queue_is_empty(FOLDER_ITEM(node->data)))
					return FALSE;
				node = next;
			}
		}
	}
	return res;
}

gint procmsg_remove_special_headers(const gchar *in, const gchar *out)
{
	FILE *fp, *outfp;
	gchar buf[BUFFSIZE];
	
	if ((fp = g_fopen(in, "rb")) == NULL) {
		FILE_OP_ERROR(in, "fopen");
		return -1;
	}
	if ((outfp = g_fopen(out, "wb")) == NULL) {
		FILE_OP_ERROR(out, "fopen");
		fclose(fp);
		return -1;
	}
	while (fgets(buf, sizeof(buf), fp) != NULL) {
		/* new way */
		if ((!strncmp(buf, "X-Claws-End-Special-Headers: 1",
			strlen("X-Claws-End-Special-Headers:"))) ||
		    (!strncmp(buf, "X-Sylpheed-End-Special-Headers: 1",
			strlen("X-Sylpheed-End-Special-Headers:"))))
			break;
		/* old way */
		if (buf[0] == '\r' || buf[0] == '\n') break;
		/* from other mailers */
		if (!strncmp(buf, "Date: ", 6)
		||  !strncmp(buf, "To: ", 4)
		||  !strncmp(buf, "From: ", 6)
		||  !strncmp(buf, "Subject: ", 9)) {
			rewind(fp);
			break;
		}
	}
	while (fgets(buf, sizeof(buf), fp) != NULL)
		fputs(buf, outfp);
	fclose(outfp);
	fclose(fp);
	return 0;
}

static gint procmsg_save_to_outbox(FolderItem *outbox, const gchar *file,
			    gboolean is_queued)
{
	gint num;
	MsgInfo *msginfo, *tmp_msginfo;
	MsgFlags flag = {0, 0};

	debug_print("saving sent message...\n");

	if (!outbox)
		outbox = folder_get_default_outbox();
	cm_return_val_if_fail(outbox != NULL, -1);

	/* remove queueing headers */
	if (is_queued) {
		gchar tmp[MAXPATHLEN + 1];

		g_snprintf(tmp, sizeof(tmp), "%s%ctmpmsg.out.%08x",
			   get_rc_dir(), G_DIR_SEPARATOR, (guint) rand());
		
		if (procmsg_remove_special_headers(file, tmp) !=0)
			return -1;

		folder_item_scan(outbox);
		if ((num = folder_item_add_msg(outbox, tmp, &flag, TRUE)) < 0) {
			g_warning("can't save message\n");
			claws_unlink(tmp);
			return -1;
		}
	} else {
		folder_item_scan(outbox);
		if ((num = folder_item_add_msg
			(outbox, file, &flag, FALSE)) < 0) {
			g_warning("can't save message\n");
			return -1;
		}
	}
	msginfo = folder_item_get_msginfo(outbox, num);		/* refcnt++ */
	tmp_msginfo = procmsg_msginfo_get_full_info(msginfo);	/* refcnt++ */ 
	if (msginfo != NULL) {
		procmsg_msginfo_unset_flags(msginfo, ~0, 0);
		procmsg_msginfo_free(msginfo);			/* refcnt-- */
		/* tmp_msginfo == msginfo */
		if (tmp_msginfo && msginfo->extradata && 
		    (msginfo->extradata->dispositionnotificationto || 
		     msginfo->extradata->returnreceiptto)) {
			procmsg_msginfo_set_flags(msginfo, MSG_RETRCPT_SENT, 0); 
		}	
		procmsg_msginfo_free(tmp_msginfo);		/* refcnt-- */
	}

	return 0;
}

void procmsg_print_message(MsgInfo *msginfo, const gchar *cmdline)
{
	static const gchar *def_cmd = "lpr %s";
	static guint id = 0;
	gchar *prtmp;
	FILE *tmpfp, *prfp;
	gchar buf[1024];
	gchar *p;
	int r;
	cm_return_if_fail(msginfo);

	if (procmime_msginfo_is_encrypted(msginfo))
		tmpfp = procmime_get_first_encrypted_text_content(msginfo);
	else
		tmpfp = procmime_get_first_text_content(msginfo);
	if (tmpfp == NULL) {
		g_warning("Can't get text part\n");
		return;
	}

	prtmp = g_strdup_printf("%s%cprinttmp.%08x",
				get_mime_tmp_dir(), G_DIR_SEPARATOR, id++);

	if ((prfp = g_fopen(prtmp, "wb")) == NULL) {
		FILE_OP_ERROR(prtmp, "fopen");
		g_free(prtmp);
		fclose(tmpfp);
		return;
	}

	if (msginfo->date) r = fprintf(prfp, "Date: %s\n", msginfo->date);
	if (msginfo->from) r = fprintf(prfp, "From: %s\n", msginfo->from);
	if (msginfo->to)   r = fprintf(prfp, "To: %s\n", msginfo->to);
	if (msginfo->cc)   r = fprintf(prfp, "Cc: %s\n", msginfo->cc);
	if (msginfo->newsgroups)
		r = fprintf(prfp, "Newsgroups: %s\n", msginfo->newsgroups);
	if (msginfo->subject) r = fprintf(prfp, "Subject: %s\n", msginfo->subject);
	fputc('\n', prfp);

	while (fgets(buf, sizeof(buf), tmpfp) != NULL)
		r = fputs(buf, prfp);

	fclose(prfp);
	fclose(tmpfp);

	if (cmdline && (p = strchr(cmdline, '%')) && *(p + 1) == 's' &&
	    !strchr(p + 2, '%'))
		g_snprintf(buf, sizeof(buf) - 1, cmdline, prtmp);
	else {
		if (cmdline)
			g_warning("Print command-line is invalid: '%s'\n",
				  cmdline);
		g_snprintf(buf, sizeof(buf) - 1, def_cmd, prtmp);
	}

	g_free(prtmp);

	g_strchomp(buf);
	if (buf[strlen(buf) - 1] != '&') strcat(buf, "&");
	if (system(buf) == -1)
		g_warning("system(%s) failed.", buf);
}

MsgInfo *procmsg_msginfo_new_ref(MsgInfo *msginfo)
{
	msginfo->refcnt++;
	
	return msginfo;
}

MsgInfo *procmsg_msginfo_new(void)
{
	MsgInfo *newmsginfo;

	newmsginfo = g_new0(MsgInfo, 1);
	newmsginfo->refcnt = 1;
	
	return newmsginfo;
}

MsgInfo *procmsg_msginfo_copy(MsgInfo *msginfo)
{
	MsgInfo *newmsginfo;
        GSList *refs;

	if (msginfo == NULL) return NULL;

	newmsginfo = g_new0(MsgInfo, 1);

	newmsginfo->refcnt = 1;

#define MEMBCOPY(mmb)	newmsginfo->mmb = msginfo->mmb
#define MEMBDUP(mmb)	newmsginfo->mmb = msginfo->mmb ? \
			g_strdup(msginfo->mmb) : NULL

	MEMBCOPY(msgnum);
	MEMBCOPY(size);
	MEMBCOPY(mtime);
	MEMBCOPY(date_t);

	MEMBCOPY(flags);

	MEMBDUP(fromname);

	MEMBDUP(date);
	MEMBDUP(from);
	MEMBDUP(to);
	MEMBDUP(cc);
	MEMBDUP(newsgroups);
	MEMBDUP(subject);
	MEMBDUP(msgid);
	MEMBDUP(inreplyto);
	MEMBDUP(xref);

	MEMBCOPY(folder);
	MEMBCOPY(to_folder);

	if (msginfo->extradata) {
		newmsginfo->extradata = g_new0(MsgInfoExtraData, 1);
		MEMBDUP(extradata->face);
		MEMBDUP(extradata->xface);
		MEMBDUP(extradata->dispositionnotificationto);
		MEMBDUP(extradata->returnreceiptto);
		MEMBDUP(extradata->partial_recv);
		MEMBDUP(extradata->account_server);
		MEMBDUP(extradata->account_login);
		MEMBDUP(extradata->list_post);
		MEMBDUP(extradata->list_subscribe);
		MEMBDUP(extradata->list_unsubscribe);
		MEMBDUP(extradata->list_help);
		MEMBDUP(extradata->list_archive);
		MEMBDUP(extradata->list_owner);
	}

        refs = msginfo->references;
        for (refs = msginfo->references; refs != NULL; refs = refs->next) {
                newmsginfo->references = g_slist_prepend
                        (newmsginfo->references, g_strdup(refs->data)); 
        }
        newmsginfo->references = g_slist_reverse(newmsginfo->references);

	MEMBCOPY(score);
	MEMBDUP(plaintext_file);

	return newmsginfo;
}

MsgInfo *procmsg_msginfo_get_full_info_from_file(MsgInfo *msginfo, const gchar *file)
{
	MsgInfo *full_msginfo;

	if (msginfo == NULL) return NULL;

	if (!file || !is_file_exist(file)) {
		g_warning("procmsg_msginfo_get_full_info_from_file(): can't get message file.\n");
		return NULL;
	}

	full_msginfo = procheader_parse_file(file, msginfo->flags, TRUE, FALSE);
	if (!full_msginfo) return NULL;

	msginfo->total_size = full_msginfo->total_size;
	msginfo->planned_download = full_msginfo->planned_download;

	if (full_msginfo->extradata) {
		if (!msginfo->extradata)
			msginfo->extradata = g_new0(MsgInfoExtraData, 1);
		if (!msginfo->extradata->list_post)
			msginfo->extradata->list_post = g_strdup(full_msginfo->extradata->list_post);
		if (!msginfo->extradata->list_subscribe)
			msginfo->extradata->list_subscribe = g_strdup(full_msginfo->extradata->list_subscribe);
		if (!msginfo->extradata->list_unsubscribe)
			msginfo->extradata->list_unsubscribe = g_strdup(full_msginfo->extradata->list_unsubscribe);
		if (!msginfo->extradata->list_help)
			msginfo->extradata->list_help = g_strdup(full_msginfo->extradata->list_help);
		if (!msginfo->extradata->list_archive)
			msginfo->extradata->list_archive= g_strdup(full_msginfo->extradata->list_archive);
		if (!msginfo->extradata->list_owner)
			msginfo->extradata->list_owner = g_strdup(full_msginfo->extradata->list_owner);
		if (!msginfo->extradata->xface)
			msginfo->extradata->xface = g_strdup(full_msginfo->extradata->xface);
		if (!msginfo->extradata->face)
			msginfo->extradata->face = g_strdup(full_msginfo->extradata->face);
		if (!msginfo->extradata->dispositionnotificationto)
			msginfo->extradata->dispositionnotificationto = 
				g_strdup(full_msginfo->extradata->dispositionnotificationto);
		if (!msginfo->extradata->returnreceiptto)
			msginfo->extradata->returnreceiptto = g_strdup
				(full_msginfo->extradata->returnreceiptto);
		if (!msginfo->extradata->partial_recv && full_msginfo->extradata->partial_recv)
			msginfo->extradata->partial_recv = g_strdup
				(full_msginfo->extradata->partial_recv);
		if (!msginfo->extradata->account_server && full_msginfo->extradata->account_server)
			msginfo->extradata->account_server = g_strdup
				(full_msginfo->extradata->account_server);
		if (!msginfo->extradata->account_login && full_msginfo->extradata->account_login)
			msginfo->extradata->account_login = g_strdup
				(full_msginfo->extradata->account_login);
	}
	procmsg_msginfo_free(full_msginfo);

	return procmsg_msginfo_new_ref(msginfo);
}

MsgInfo *procmsg_msginfo_get_full_info(MsgInfo *msginfo)
{
	MsgInfo *full_msginfo;
	gchar *file;

	if (msginfo == NULL) return NULL;

	file = procmsg_get_message_file_path(msginfo);
	if (!file || !is_file_exist(file)) {
		g_free(file);
		file = procmsg_get_message_file(msginfo);
	}
	if (!file || !is_file_exist(file)) {
		g_warning("procmsg_msginfo_get_full_info(): can't get message file.\n");
		return NULL;
	}

	full_msginfo = procmsg_msginfo_get_full_info_from_file(msginfo, file);
	g_free(file);
	return full_msginfo;
}

void procmsg_msginfo_free(MsgInfo *msginfo)
{
	if (msginfo == NULL) return;

	msginfo->refcnt--;
	if (msginfo->refcnt > 0)
		return;

	if (msginfo->to_folder) {
		msginfo->to_folder->op_count--;
		folder_item_update(msginfo->to_folder, F_ITEM_UPDATE_MSGCNT);
	}

	g_free(msginfo->fromspace);

	g_free(msginfo->fromname);

	g_free(msginfo->date);
	g_free(msginfo->from);
	g_free(msginfo->to);
	g_free(msginfo->cc);
	g_free(msginfo->newsgroups);
	g_free(msginfo->subject);
	g_free(msginfo->msgid);
	g_free(msginfo->inreplyto);
	g_free(msginfo->xref);

	if (msginfo->extradata) {
		g_free(msginfo->extradata->returnreceiptto);
		g_free(msginfo->extradata->dispositionnotificationto);
		g_free(msginfo->extradata->xface);
		g_free(msginfo->extradata->face);
		g_free(msginfo->extradata->list_post);
		g_free(msginfo->extradata->list_subscribe);
		g_free(msginfo->extradata->list_unsubscribe);
		g_free(msginfo->extradata->list_help);
		g_free(msginfo->extradata->list_archive);
		g_free(msginfo->extradata->list_owner);
		g_free(msginfo->extradata->partial_recv);
		g_free(msginfo->extradata->account_server);
		g_free(msginfo->extradata->account_login);
		g_free(msginfo->extradata);
	}
	slist_free_strings(msginfo->references);
	g_slist_free(msginfo->references);
	g_slist_free(msginfo->tags);

	g_free(msginfo->plaintext_file);

	g_free(msginfo);
}

guint procmsg_msginfo_memusage(MsgInfo *msginfo)
{
	guint memusage = 0;
	GSList *tmp;
	
	memusage += sizeof(MsgInfo);
	if (msginfo->fromname)
		memusage += strlen(msginfo->fromname);
	if (msginfo->date)
		memusage += strlen(msginfo->date);
	if (msginfo->from)
		memusage += strlen(msginfo->from);
	if (msginfo->to)
		memusage += strlen(msginfo->to);
	if (msginfo->cc)
		memusage += strlen(msginfo->cc);
	if (msginfo->newsgroups)
		memusage += strlen(msginfo->newsgroups);
	if (msginfo->subject)
		memusage += strlen(msginfo->subject);
	if (msginfo->msgid)
		memusage += strlen(msginfo->msgid);
	if (msginfo->inreplyto)
		memusage += strlen(msginfo->inreplyto);

	for (tmp = msginfo->references; tmp; tmp=tmp->next) {
		gchar *r = (gchar *)tmp->data;
		memusage += r?strlen(r):0 + sizeof(GSList);
	}
	if (msginfo->fromspace)
		memusage += strlen(msginfo->fromspace);

	for (tmp = msginfo->tags; tmp; tmp=tmp->next) {
		memusage += sizeof(GSList);
	}
	if (msginfo->extradata) {
		memusage += sizeof(MsgInfoExtraData);
		if (msginfo->extradata->xface)
			memusage += strlen(msginfo->extradata->xface);
		if (msginfo->extradata->face)
			memusage += strlen(msginfo->extradata->face);
		if (msginfo->extradata->dispositionnotificationto)
			memusage += strlen(msginfo->extradata->dispositionnotificationto);
		if (msginfo->extradata->returnreceiptto)
			memusage += strlen(msginfo->extradata->returnreceiptto);

		if (msginfo->extradata->partial_recv)
			memusage += strlen(msginfo->extradata->partial_recv);
		if (msginfo->extradata->account_server)
			memusage += strlen(msginfo->extradata->account_server);
		if (msginfo->extradata->account_login)
			memusage += strlen(msginfo->extradata->account_login);

		if (msginfo->extradata->list_post)
			memusage += strlen(msginfo->extradata->list_post);
		if (msginfo->extradata->list_subscribe)
			memusage += strlen(msginfo->extradata->list_subscribe);
		if (msginfo->extradata->list_unsubscribe)
			memusage += strlen(msginfo->extradata->list_unsubscribe);
		if (msginfo->extradata->list_help)
			memusage += strlen(msginfo->extradata->list_help);
		if (msginfo->extradata->list_archive)
			memusage += strlen(msginfo->extradata->list_archive);
		if (msginfo->extradata->list_owner)
			memusage += strlen(msginfo->extradata->list_owner);
	}
	return memusage;
}

static gint procmsg_send_message_queue_full(const gchar *file, gboolean keep_session, gchar **errstr,
					    FolderItem *queue, gint msgnum, gboolean *queued_removed)
{
	static HeaderEntry qentry[] = {{"S:",    NULL, FALSE},
				       {"SSV:",  NULL, FALSE},
				       {"R:",    NULL, FALSE},
				       {"NG:",   NULL, FALSE},
				       {"MAID:", NULL, FALSE},
				       {"NAID:", NULL, FALSE},
				       {"SCF:",  NULL, FALSE},
				       {"RMID:", NULL, FALSE},
				       {"FMID:", NULL, FALSE},
				       {"X-Claws-Privacy-System:", NULL, FALSE},
				       {"X-Claws-Encrypt:", NULL, FALSE},
				       {"X-Claws-Encrypt-Data:", NULL, FALSE},
				       {"X-Claws-End-Special-Headers:", NULL, FALSE},
				       {"X-Sylpheed-Privacy-System:", NULL, FALSE},
				       {"X-Sylpheed-Encrypt:", NULL, FALSE},
				       {"X-Sylpheed-Encrypt-Data:", NULL, FALSE},
				       {"X-Sylpheed-End-Special-Headers:", NULL, FALSE},
				       {NULL,    NULL, FALSE}};
	FILE *fp;
	gint filepos;
	gint mailval = 0, newsval = 0;
	gchar *from = NULL;
	gchar *smtpserver = NULL;
	GSList *to_list = NULL;
	GSList *newsgroup_list = NULL;
	gchar *savecopyfolder = NULL;
	gchar *replymessageid = NULL;
	gchar *fwdmessageid = NULL;
	gchar *privacy_system = NULL;
	gboolean encrypt = FALSE;
	gchar *encrypt_data = NULL;
	gchar buf[BUFFSIZE];
	gint hnum;
	PrefsAccount *mailac = NULL, *newsac = NULL;
	gboolean save_clear_text = TRUE;
	gchar *tmp_enc_file = NULL;

	int local = 0;

	cm_return_val_if_fail(file != NULL, -1);

	if ((fp = g_fopen(file, "rb")) == NULL) {
		FILE_OP_ERROR(file, "fopen");
		if (errstr) {
			if (*errstr) g_free(*errstr);
			*errstr = g_strdup_printf(_("Couldn't open file %s."), file);
		}
		return -1;
	}

	while ((hnum = procheader_get_one_field(buf, sizeof(buf), fp, qentry))
	       != -1) {
		gchar *p = buf + strlen(qentry[hnum].name);

		switch (hnum) {
		case Q_SENDER:
			if (from == NULL) 
				from = g_strdup(p);
			break;
		case Q_SMTPSERVER:
			if (smtpserver == NULL) 
				smtpserver = g_strdup(p);
			break;
		case Q_RECIPIENTS:
			to_list = address_list_append(to_list, p);
			break;
		case Q_NEWSGROUPS:
			newsgroup_list = newsgroup_list_append(newsgroup_list, p);
			break;
		case Q_MAIL_ACCOUNT_ID:
			mailac = account_find_from_id(atoi(p));
			break;
		case Q_NEWS_ACCOUNT_ID:
			newsac = account_find_from_id(atoi(p));
			break;
		case Q_SAVE_COPY_FOLDER:
			if (savecopyfolder == NULL) 
				savecopyfolder = g_strdup(p);
			break;
		case Q_REPLY_MESSAGE_ID:
			if (replymessageid == NULL) 
				replymessageid = g_strdup(p);
			break;
		case Q_FWD_MESSAGE_ID:
			if (fwdmessageid == NULL) 
				fwdmessageid = g_strdup(p);
			break;
		case Q_PRIVACY_SYSTEM:
		case Q_PRIVACY_SYSTEM_OLD:
			if (privacy_system == NULL) 
				privacy_system = g_strdup(p);
			break;
		case Q_ENCRYPT:
		case Q_ENCRYPT_OLD:
			if (p[0] == '1') 
				encrypt = TRUE;
			break;
		case Q_ENCRYPT_DATA:
		case Q_ENCRYPT_DATA_OLD:
			if (encrypt_data == NULL) 
				encrypt_data = g_strdup(p);
			break;
		case Q_CLAWS_HDRS:
		case Q_CLAWS_HDRS_OLD:
			/* end of special headers reached */
			goto send_mail; /* can't "break;break;" */
		}
	}
send_mail:
	filepos = ftell(fp);

	if (encrypt) {
		MimeInfo *mimeinfo;

		if (mailac && mailac->save_encrypted_as_clear_text 
		&&  !mailac->encrypt_to_self)
			save_clear_text = TRUE;
		else
			save_clear_text = FALSE;

		fclose(fp);
		fp = NULL;

		mimeinfo = procmime_scan_queue_file(file);
		if (!privacy_encrypt(privacy_system, mimeinfo, encrypt_data)
		|| (fp = my_tmpfile()) == NULL
		||  procmime_write_mimeinfo(mimeinfo, fp) < 0) {
			if (fp)
				fclose(fp);
			procmime_mimeinfo_free_all(mimeinfo);
			g_free(from);
			g_free(smtpserver);
			slist_free_strings(to_list);
			g_slist_free(to_list);
			slist_free_strings(newsgroup_list);
			g_slist_free(newsgroup_list);
			g_free(savecopyfolder);
			g_free(replymessageid);
			g_free(fwdmessageid);
			g_free(privacy_system);
			g_free(encrypt_data);
			if (errstr) {
				if (*errstr) g_free(*errstr);
				*errstr = g_strdup_printf(_("Couldn't encrypt the email: %s"),
						privacy_get_error());
			}
			return -1;
		}
		
		rewind(fp);
		if (!save_clear_text) {
			gchar *content = NULL;
			FILE *tmpfp = get_tmpfile_in_dir(get_mime_tmp_dir(), &tmp_enc_file);
			if (tmpfp) {
				fclose(tmpfp);

				content = file_read_stream_to_str(fp);
				rewind(fp);

				str_write_to_file(content, tmp_enc_file);
				g_free(content);
			} else {
				g_warning("couldn't get tempfile\n");
			}
		} 
		
		procmime_mimeinfo_free_all(mimeinfo);
		
		filepos = 0;
    	}

	if (to_list) {
		debug_print("Sending message by mail\n");
		if (!from) {
			if (errstr) {
				if (*errstr) g_free(*errstr);
				*errstr = g_strdup_printf(_("Queued message header is broken."));
			}
			mailval = -1;
		} else if (mailac && mailac->use_mail_command &&
			   mailac->mail_command && (* mailac->mail_command)) {
			mailval = send_message_local(mailac->mail_command, fp);
			local = 1;
		} else {
			if (!mailac) {
				mailac = account_find_from_smtp_server(from, smtpserver);
				if (!mailac) {
					g_warning("Account not found. "
						    "Using current account...\n");
					mailac = cur_account;
				}
			}

			if (mailac) {
				mailval = send_message_smtp_full(mailac, to_list, fp, keep_session);
				if (mailval == -1 && errstr) {
					if (*errstr) g_free(*errstr);
					*errstr = g_strdup_printf(_("An error happened during SMTP session."));
				}
			} else {
				PrefsAccount tmp_ac;

				g_warning("Account not found.\n");

				memset(&tmp_ac, 0, sizeof(PrefsAccount));
				tmp_ac.address = from;
				tmp_ac.smtp_server = smtpserver;
				tmp_ac.smtpport = SMTP_PORT;
				mailval = send_message_smtp(&tmp_ac, to_list, fp);
				if (mailval == -1 && errstr) {
					if (*errstr) g_free(*errstr);
					*errstr = g_strdup_printf(_("No specific account has been found to "
							"send, and an error happened during SMTP session."));
				}
			}
		}
	} else if (!to_list && !newsgroup_list) {
		if (errstr) {
			if (*errstr) g_free(*errstr);
			*errstr = g_strdup(_("Couldn't determine sending informations. "
				"Maybe the email hasn't been generated by Claws Mail."));
		}
		mailval = -1;
	}

	fseek(fp, filepos, SEEK_SET);
	if (newsgroup_list && newsac && (mailval == 0)) {
		Folder *folder;
		gchar *tmp = NULL;
		FILE *tmpfp;

    		/* write to temporary file */
    		tmp = g_strdup_printf("%s%cnntp%p", get_tmp_dir(),
                    	    G_DIR_SEPARATOR, file);
    		if ((tmpfp = g_fopen(tmp, "wb")) == NULL) {
            		FILE_OP_ERROR(tmp, "fopen");
            		newsval = -1;
			alertpanel_error(_("Couldn't create temporary file for news sending."));
    		} else {
    			if (change_file_mode_rw(tmpfp, tmp) < 0) {
            			FILE_OP_ERROR(tmp, "chmod");
            			g_warning("can't change file mode\n");
    			}

			while ((newsval == 0) && fgets(buf, sizeof(buf), fp) != NULL) {
				if (fputs(buf, tmpfp) == EOF) {
					FILE_OP_ERROR(tmp, "fputs");
					newsval = -1;
					if (errstr) {
						if (*errstr) g_free(*errstr);
						*errstr = g_strdup_printf(_("Error when writing temporary file for news sending."));
					}
				}
			}
			fclose(tmpfp);

			if (newsval == 0) {
				debug_print("Sending message by news\n");

				folder = FOLDER(newsac->folder);

    				newsval = news_post(folder, tmp);
    				if (newsval < 0 && errstr)  {
					if (*errstr) g_free(*errstr);
					*errstr = g_strdup_printf(_("Error occurred while posting the message to %s."),
                            		 newsac->nntp_server);
				}
			}
			claws_unlink(tmp);
		}
		g_free(tmp);
	}

	fclose(fp);

	/* save message to outbox */
	if (mailval == 0 && newsval == 0 && savecopyfolder) {
		FolderItem *outbox;

		debug_print("saving sent message...\n");

		outbox = folder_find_item_from_identifier(savecopyfolder);
		if (!outbox)
			outbox = folder_get_default_outbox();
			
		if (save_clear_text || tmp_enc_file == NULL) {
			gboolean saved = FALSE;
			*queued_removed = FALSE;
			if (queue && msgnum > 0) {
				MsgInfo *queued_mail = folder_item_get_msginfo(queue, msgnum);
				if (folder_item_move_msg(outbox, queued_mail) >= 0) {
					debug_print("moved queued mail %d to sent folder\n", msgnum);
					saved = TRUE;
					*queued_removed = TRUE;
				} else if (folder_item_copy_msg(outbox, queued_mail) >= 0) {
					debug_print("copied queued mail %d to sent folder\n", msgnum);
					saved = TRUE;
				}
				procmsg_msginfo_free(queued_mail);
			}
			if (!saved) {
				debug_print("resaving clear text queued mail to sent folder\n");
				procmsg_save_to_outbox(outbox, file, TRUE);
			}
		} else {
			debug_print("saving encrpyted queued mail to sent folder\n");
			procmsg_save_to_outbox(outbox, tmp_enc_file, FALSE);
		}
	}

	if (tmp_enc_file != NULL) {
		claws_unlink(tmp_enc_file);
		free(tmp_enc_file);
		tmp_enc_file = NULL;
	}

	if (replymessageid != NULL || fwdmessageid != NULL) {
		gchar **tokens;
		FolderItem *item;
		
		if (replymessageid != NULL)
			tokens = g_strsplit(replymessageid, "\t", 0);
		else
			tokens = g_strsplit(fwdmessageid, "\t", 0);
		item = folder_find_item_from_identifier(tokens[0]);

		/* check if queued message has valid folder and message id */
		if (item != NULL && tokens[2] != NULL) {
			MsgInfo *msginfo;
			
			msginfo = folder_item_get_msginfo(item, atoi(tokens[1]));
		
			/* check if referring message exists and has a message id */
			if ((msginfo != NULL) && 
			    (msginfo->msgid != NULL) &&
			    (strcmp(msginfo->msgid, tokens[2]) != 0)) {
				procmsg_msginfo_free(msginfo);
				msginfo = NULL;
			}
			
			if (msginfo == NULL) {
				msginfo = folder_item_get_msginfo_by_msgid(item, tokens[2]);
			}
			
			if (msginfo != NULL) {
				MsgPermFlags to_unset = 0;

				if (prefs_common.mark_as_read_on_new_window)
					to_unset = (MSG_NEW|MSG_UNREAD);

				if (replymessageid != NULL) {
					procmsg_msginfo_unset_flags(msginfo, to_unset|MSG_FORWARDED, 0);
					procmsg_msginfo_set_flags(msginfo, MSG_REPLIED, 0);
				}  else {
					procmsg_msginfo_unset_flags(msginfo, MSG_REPLIED, 0);
					procmsg_msginfo_set_flags(msginfo, MSG_FORWARDED, 0);
				}
				procmsg_msginfo_free(msginfo);
			}
		}
		g_strfreev(tokens);
	}

	g_free(from);
	g_free(smtpserver);
	slist_free_strings(to_list);
	g_slist_free(to_list);
	slist_free_strings(newsgroup_list);
	g_slist_free(newsgroup_list);
	g_free(savecopyfolder);
	g_free(replymessageid);
	g_free(fwdmessageid);
	g_free(privacy_system);
	g_free(encrypt_data);

	return (newsval != 0 ? newsval : mailval);
}

gint procmsg_send_message_queue(const gchar *file, gchar **errstr, FolderItem *queue, gint msgnum, gboolean *queued_removed)
{
	gint result = procmsg_send_message_queue_full(file, FALSE, errstr, queue, msgnum, queued_removed);
	toolbar_main_set_sensitive(mainwindow_get_mainwindow());
	return result;
}

static void update_folder_msg_counts(FolderItem *item, MsgInfo *msginfo, MsgPermFlags old_flags)
{
	MsgPermFlags new_flags = msginfo->flags.perm_flags;

	/* NEW flag */
	if (!(old_flags & MSG_NEW) && (new_flags & MSG_NEW)) {
		item->new_msgs++;
	}

	if ((old_flags & MSG_NEW) && !(new_flags & MSG_NEW)) {
		item->new_msgs--;
	}

	/* UNREAD flag */
	if (!(old_flags & MSG_UNREAD) && (new_flags & MSG_UNREAD)) {
		item->unread_msgs++;
		if (procmsg_msg_has_marked_parent(msginfo))
			item->unreadmarked_msgs++;
	}

	if ((old_flags & MSG_UNREAD) && !(new_flags & MSG_UNREAD)) {
		item->unread_msgs--;
		if (procmsg_msg_has_marked_parent(msginfo))
			item->unreadmarked_msgs--;
	}
	
	/* MARK flag */
	if (!(old_flags & MSG_MARKED) && (new_flags & MSG_MARKED)) {
		procmsg_update_unread_children(msginfo, TRUE);
		item->marked_msgs++;
	}

	if ((old_flags & MSG_MARKED) && !(new_flags & MSG_MARKED)) {
		procmsg_update_unread_children(msginfo, FALSE);
		item->marked_msgs--;
	}

	if (!(old_flags & MSG_REPLIED) && (new_flags & MSG_REPLIED)) {
		item->replied_msgs++;
	}

	if ((old_flags & MSG_REPLIED) && !(new_flags & MSG_REPLIED)) {
		item->replied_msgs--;
	}

	if (!(old_flags & MSG_FORWARDED) && (new_flags & MSG_FORWARDED)) {
		item->forwarded_msgs++;
	}

	if ((old_flags & MSG_FORWARDED) && !(new_flags & MSG_FORWARDED)) {
		item->forwarded_msgs--;
	}

	if (!(old_flags & MSG_LOCKED) && (new_flags & MSG_LOCKED)) {
		item->locked_msgs++;
	}

	if ((old_flags & MSG_LOCKED) && !(new_flags & MSG_LOCKED)) {
		item->locked_msgs--;
	}

	if ((old_flags & MSG_IGNORE_THREAD) && !(new_flags & MSG_IGNORE_THREAD)) {
		item->ignored_msgs--;
	}

	if (!(old_flags & MSG_IGNORE_THREAD) && (new_flags & MSG_IGNORE_THREAD)) {
		item->ignored_msgs++;
	}

	if ((old_flags & MSG_WATCH_THREAD) && !(new_flags & MSG_WATCH_THREAD)) {
		item->watched_msgs--;
	}

	if (!(old_flags & MSG_WATCH_THREAD) && (new_flags & MSG_WATCH_THREAD)) {
		item->watched_msgs++;
	}
}

void procmsg_msginfo_set_flags(MsgInfo *msginfo, MsgPermFlags perm_flags, MsgTmpFlags tmp_flags)
{
	FolderItem *item;
	MsgInfoUpdate msginfo_update;
	MsgPermFlags perm_flags_new, perm_flags_old;
	MsgTmpFlags tmp_flags_old;

	cm_return_if_fail(msginfo != NULL);
	item = msginfo->folder;
	cm_return_if_fail(item != NULL);
	
	debug_print("Setting flags for message %d in folder %s\n", msginfo->msgnum, item->path);

	/* Perm Flags handling */
	perm_flags_old = msginfo->flags.perm_flags;
	perm_flags_new = msginfo->flags.perm_flags | perm_flags;
	if ((perm_flags & MSG_IGNORE_THREAD) || (perm_flags_old & MSG_IGNORE_THREAD)) {
		perm_flags_new &= ~(MSG_NEW | MSG_UNREAD);
	}
	if ((perm_flags & MSG_WATCH_THREAD) || (perm_flags_old & MSG_WATCH_THREAD)) {
		perm_flags_new &= ~(MSG_IGNORE_THREAD);
	}

	if (perm_flags_old != perm_flags_new) {
		folder_item_change_msg_flags(msginfo->folder, msginfo, perm_flags_new);

		update_folder_msg_counts(item, msginfo, perm_flags_old);
		summary_update_unread(mainwindow_get_mainwindow()->summaryview, NULL);
	}

	/* Tmp flags handling */
	tmp_flags_old = msginfo->flags.tmp_flags;
	msginfo->flags.tmp_flags |= tmp_flags;

	/* update notification */
	if ((perm_flags_old != perm_flags_new) || (tmp_flags_old != msginfo->flags.tmp_flags)) {
		msginfo_update.msginfo = msginfo;
		msginfo_update.flags = MSGINFO_UPDATE_FLAGS;
		hooks_invoke(MSGINFO_UPDATE_HOOKLIST, &msginfo_update);
		folder_item_update(msginfo->folder, F_ITEM_UPDATE_MSGCNT);
	}
}

void procmsg_msginfo_unset_flags(MsgInfo *msginfo, MsgPermFlags perm_flags, MsgTmpFlags tmp_flags)
{
	FolderItem *item;
	MsgInfoUpdate msginfo_update;
	MsgPermFlags perm_flags_new, perm_flags_old;
	MsgTmpFlags tmp_flags_old;

	cm_return_if_fail(msginfo != NULL);
	item = msginfo->folder;
	cm_return_if_fail(item != NULL);
	
	debug_print("Unsetting flags for message %d in folder %s\n", msginfo->msgnum, item->path);

	/* Perm Flags handling */
	perm_flags_old = msginfo->flags.perm_flags;
	perm_flags_new = msginfo->flags.perm_flags & ~perm_flags;
	
	if (perm_flags_old != perm_flags_new) {
		folder_item_change_msg_flags(msginfo->folder, msginfo, perm_flags_new);

		update_folder_msg_counts(item, msginfo, perm_flags_old);
	}

	/* Tmp flags hanlding */
	tmp_flags_old = msginfo->flags.tmp_flags;
	msginfo->flags.tmp_flags &= ~tmp_flags;

	/* update notification */
	if ((perm_flags_old != perm_flags_new) || (tmp_flags_old != msginfo->flags.tmp_flags)) {
		msginfo_update.msginfo = msginfo;
		msginfo_update.flags = MSGINFO_UPDATE_FLAGS;
		hooks_invoke(MSGINFO_UPDATE_HOOKLIST, &msginfo_update);
		folder_item_update(msginfo->folder, F_ITEM_UPDATE_MSGCNT);
	}
}

void procmsg_msginfo_change_flags(MsgInfo *msginfo, 
				MsgPermFlags add_perm_flags, MsgTmpFlags add_tmp_flags,
				MsgPermFlags rem_perm_flags, MsgTmpFlags rem_tmp_flags)
{
	FolderItem *item;
	MsgInfoUpdate msginfo_update;
	MsgPermFlags perm_flags_new, perm_flags_old;
	MsgTmpFlags tmp_flags_old;

	cm_return_if_fail(msginfo != NULL);
	item = msginfo->folder;
	cm_return_if_fail(item != NULL);
	
	debug_print("Changing flags for message %d in folder %s\n", msginfo->msgnum, item->path);

	/* Perm Flags handling */
	perm_flags_old = msginfo->flags.perm_flags;
	perm_flags_new = (msginfo->flags.perm_flags & ~rem_perm_flags) | add_perm_flags;
	if ((add_perm_flags & MSG_IGNORE_THREAD) || (perm_flags_old & MSG_IGNORE_THREAD)) {
		perm_flags_new &= ~(MSG_NEW | MSG_UNREAD);
	}
	if ((add_perm_flags & MSG_WATCH_THREAD) || (perm_flags_old & MSG_WATCH_THREAD)) {
		perm_flags_new &= ~(MSG_IGNORE_THREAD);
	}

	if (perm_flags_old != perm_flags_new) {
		folder_item_change_msg_flags(msginfo->folder, msginfo, perm_flags_new);

		update_folder_msg_counts(item, msginfo, perm_flags_old);

	}

	/* Tmp flags handling */
	tmp_flags_old = msginfo->flags.tmp_flags;
	msginfo->flags.tmp_flags &= ~rem_tmp_flags;
	msginfo->flags.tmp_flags |= add_tmp_flags;

	/* update notification */
	if ((perm_flags_old != perm_flags_new) || (tmp_flags_old != msginfo->flags.tmp_flags)) {
		msginfo_update.msginfo = msginfo;
		msginfo_update.flags = MSGINFO_UPDATE_FLAGS;
		hooks_invoke(MSGINFO_UPDATE_HOOKLIST, &msginfo_update);
		folder_item_update(msginfo->folder, F_ITEM_UPDATE_MSGCNT);
	}
}

/*!
 *\brief	check for flags (e.g. mark) in prior msgs of current thread
 *
 *\param	info Current message
 *\param	perm_flags Flags to be checked
 *\param	parentmsgs Hash of prior msgs to avoid loops
 *
 *\return	gboolean TRUE if perm_flags are found
 */
static gboolean procmsg_msg_has_flagged_parent_real(MsgInfo *info,
		MsgPermFlags perm_flags, GHashTable *parentmsgs)
{
	MsgInfo *tmp;

	cm_return_val_if_fail(info != NULL, FALSE);

	if (info != NULL && info->folder != NULL && info->inreplyto != NULL) {
		tmp = folder_item_get_msginfo_by_msgid(info->folder,
				info->inreplyto);
		if (tmp && (tmp->flags.perm_flags & perm_flags)) {
			procmsg_msginfo_free(tmp);
			return TRUE;
		} else if (tmp != NULL) {
			gboolean result;

			if (g_hash_table_lookup(parentmsgs, info)) {
				debug_print("loop detected: %d\n",
					info->msgnum);
				result = FALSE;
			} else {
				g_hash_table_insert(parentmsgs, info, "1");
				result = procmsg_msg_has_flagged_parent_real(
				    tmp, perm_flags, parentmsgs);
			}
			procmsg_msginfo_free(tmp);
			return result;
		} else {
			return FALSE;
		}
	} else
		return FALSE;
}

/*!
 *\brief	Callback for cleaning up hash of parentmsgs
 */
static gboolean parentmsgs_hash_remove(gpointer key,
                            gpointer value,
                            gpointer user_data)
{
	return TRUE;
}

/*!
 *\brief	Set up list of parentmsgs
 *		See procmsg_msg_has_flagged_parent_real()
 */
gboolean procmsg_msg_has_flagged_parent(MsgInfo *info, MsgPermFlags perm_flags)
{
	gboolean result;
	static GHashTable *parentmsgs = NULL;
	
	if (parentmsgs == NULL)
		parentmsgs = g_hash_table_new(NULL, NULL); 

	result = procmsg_msg_has_flagged_parent_real(info, perm_flags, parentmsgs);
	g_hash_table_foreach_remove(parentmsgs, parentmsgs_hash_remove, NULL);

	return result;
}

/*!
 *\brief	Check if msgs prior in thread are marked
 *		See procmsg_msg_has_flagged_parent_real()
 */
gboolean procmsg_msg_has_marked_parent(MsgInfo *info)
{
	return procmsg_msg_has_flagged_parent(info, MSG_MARKED);
}


static GSList *procmsg_find_children_func(MsgInfo *info, 
				   GSList *children, GSList *all)
{
	GSList *cur;

	cm_return_val_if_fail(info!=NULL, children);
	if (info->msgid == NULL)
		return children;

	for (cur = all; cur != NULL; cur = g_slist_next(cur)) {
		MsgInfo *tmp = (MsgInfo *)cur->data;
		if (tmp->inreplyto && !strcmp(tmp->inreplyto, info->msgid)) {
			/* Check if message is already in the list */
			if ((children == NULL) || 
			    (g_slist_index(children, tmp) == -1)) {
				children = g_slist_prepend(children,
						procmsg_msginfo_new_ref(tmp));
				children = procmsg_find_children_func(tmp, 
							children, 
							all);
			}
		}
	}
	return children;
}

static GSList *procmsg_find_children (MsgInfo *info)
{
	GSList *children;
	GSList *all, *cur;

	cm_return_val_if_fail(info!=NULL, NULL);
	all = folder_item_get_msg_list(info->folder);
	children = procmsg_find_children_func(info, NULL, all);
	if (children != NULL) {
		for (cur = all; cur != NULL; cur = g_slist_next(cur)) {
			/* this will not free the used pointers
			   created with procmsg_msginfo_new_ref */
			procmsg_msginfo_free((MsgInfo *)cur->data);
		}
	}
	g_slist_free(all);

	return children;
}

static void procmsg_update_unread_children(MsgInfo *info, gboolean newly_marked)
{
	GSList *children = procmsg_find_children(info);
	GSList *cur;
	for (cur = children; cur != NULL; cur = g_slist_next(cur)) {
		MsgInfo *tmp = (MsgInfo *)cur->data;
		if(MSG_IS_UNREAD(tmp->flags) && !MSG_IS_IGNORE_THREAD(tmp->flags)) {
			if(newly_marked) 
				info->folder->unreadmarked_msgs++;
			else
				info->folder->unreadmarked_msgs--;
			folder_item_update(info->folder, F_ITEM_UPDATE_MSGCNT);
		}
		procmsg_msginfo_free(tmp);
	}
	g_slist_free(children);
}

/**
 * Set the destination folder for a copy or move operation
 *
 * \param msginfo The message which's destination folder is changed
 * \param to_folder The destination folder for the operation
 */
void procmsg_msginfo_set_to_folder(MsgInfo *msginfo, FolderItem *to_folder)
{
	if(msginfo->to_folder != NULL) {
		msginfo->to_folder->op_count--;
		folder_item_update(msginfo->to_folder, F_ITEM_UPDATE_MSGCNT);
	}
	msginfo->to_folder = to_folder;
	if(to_folder != NULL) {
		to_folder->op_count++;
		folder_item_update(msginfo->to_folder, F_ITEM_UPDATE_MSGCNT);
	}
}

/**
 * Apply filtering actions to the msginfo
 *
 * \param msginfo The MsgInfo describing the message that should be filtered
 * \return TRUE if the message was moved and MsgInfo is now invalid,
 *         FALSE otherwise
 */
static gboolean procmsg_msginfo_filter(MsgInfo *msginfo, PrefsAccount* ac_prefs)
{
	MailFilteringData mail_filtering_data;
			
	mail_filtering_data.msginfo = msginfo;			
	mail_filtering_data.msglist = NULL;			
	mail_filtering_data.filtered = NULL;			
	mail_filtering_data.unfiltered = NULL;
	mail_filtering_data.account = ac_prefs;	

	if (!ac_prefs || ac_prefs->filterhook_on_recv)
		if (hooks_invoke(MAIL_FILTERING_HOOKLIST, &mail_filtering_data))
		return TRUE;

	/* filter if enabled in prefs or move to inbox if not */
	if((filtering_rules != NULL) &&
		filter_message_by_msginfo(filtering_rules, msginfo, ac_prefs,
				FILTERING_INCORPORATION, NULL)) {
		return TRUE;
	}
		
	return FALSE;
}

void procmsg_msglist_filter(GSList *list, PrefsAccount *ac, 
			    GSList **filtered, GSList **unfiltered,
			    gboolean do_filter)
{
	GSList *cur, *to_do = NULL;
	gint total = 0, curnum = 0;
	MailFilteringData mail_filtering_data;
			
	cm_return_if_fail(filtered != NULL);
	cm_return_if_fail(unfiltered != NULL);

	*filtered = NULL;
	*unfiltered = NULL;
	
	if (list == NULL)
		return;

	total = g_slist_length(list);

	if (!do_filter) {
		*filtered = NULL;
		*unfiltered = g_slist_copy(list);
		return;
	}

	statusbar_print_all(_("Filtering messages...\n"));

	mail_filtering_data.msginfo = NULL;			
	mail_filtering_data.msglist = list;			
	mail_filtering_data.filtered = NULL;			
	mail_filtering_data.unfiltered = NULL;	
	mail_filtering_data.account = ac;	
			
	if (!ac || ac->filterhook_on_recv)
	hooks_invoke(MAIL_LISTFILTERING_HOOKLIST, &mail_filtering_data);
	
	if (mail_filtering_data.filtered == NULL &&
	    mail_filtering_data.unfiltered == NULL) {
	    	/* nothing happened */
		debug_print(MAIL_LISTFILTERING_HOOKLIST " did nothing. filtering whole list normally.\n");
		to_do = list;
	} 
	if (mail_filtering_data.filtered != NULL) {
		/* keep track of what's been filtered by the hooks */
		debug_print(MAIL_LISTFILTERING_HOOKLIST " filtered some stuff. total %d filtered %d unfilt %d.\n",
			g_slist_length(list),
			g_slist_length(mail_filtering_data.filtered),
			g_slist_length(mail_filtering_data.unfiltered));

		*filtered = g_slist_copy(mail_filtering_data.filtered);
	}
	if (mail_filtering_data.unfiltered != NULL) {
		/* what the hooks didn't handle will go in filtered or 
		 * unfiltered in the next loop */
		debug_print(MAIL_LISTFILTERING_HOOKLIST " left unfiltered stuff. total %d filtered %d unfilt %d.\n",
			g_slist_length(list),
			g_slist_length(mail_filtering_data.filtered),
			g_slist_length(mail_filtering_data.unfiltered));
		to_do = mail_filtering_data.unfiltered;
	} 

	for (cur = to_do; cur; cur = cur->next) {
		MsgInfo *info = (MsgInfo *)cur->data;
		if (procmsg_msginfo_filter(info, ac))
			*filtered = g_slist_prepend(*filtered, info);
		else
			*unfiltered = g_slist_prepend(*unfiltered, info);
		statusbar_progress_all(curnum++, total, prefs_common.statusbar_update_step);
	}

	g_slist_free(mail_filtering_data.filtered);
	g_slist_free(mail_filtering_data.unfiltered);
	
	*filtered = g_slist_reverse(*filtered);
	*unfiltered = g_slist_reverse(*unfiltered);

	statusbar_progress_all(0,0,0);
	statusbar_pop_all();
}

MsgInfo *procmsg_msginfo_new_from_mimeinfo(MsgInfo *src_msginfo, MimeInfo *mimeinfo)
{
	MsgInfo *tmp_msginfo = NULL;
	MsgFlags flags = {0, 0};
	gchar *tmpfile = get_tmp_file();
	FILE *fp = g_fopen(tmpfile, "wb");
	
	if (!mimeinfo || mimeinfo->type != MIMETYPE_MESSAGE ||
	    g_ascii_strcasecmp(mimeinfo->subtype, "rfc822")) {
		g_warning("procmsg_msginfo_new_from_mimeinfo(): unsuitable mimeinfo");
		if (fp) 
			fclose(fp);
		g_free(tmpfile);
		return NULL;
	}
	
	if (fp && procmime_write_mimeinfo(mimeinfo, fp) >= 0) {
		fclose(fp);
		fp = NULL;
		tmp_msginfo = procheader_parse_file(
			tmpfile, flags, 
			TRUE, FALSE);
	}
	if (fp)
		fclose(fp);

	if (tmp_msginfo != NULL) {
		if (src_msginfo)
			tmp_msginfo->folder = src_msginfo->folder;
		tmp_msginfo->plaintext_file = g_strdup(tmpfile);
	} else {
		g_warning("procmsg_msginfo_new_from_mimeinfo(): Can't generate new msginfo");
	}

	g_free(tmpfile);

	return tmp_msginfo;
}

static GSList *spam_learners = NULL;

void procmsg_register_spam_learner (int (*learn_func)(MsgInfo *info, GSList *list, gboolean spam))
{
	if (!g_slist_find(spam_learners, learn_func))
		spam_learners = g_slist_append(spam_learners, learn_func);
	if (mainwindow_get_mainwindow()) {
		main_window_set_menu_sensitive(mainwindow_get_mainwindow());
		summary_set_menu_sensitive(
			mainwindow_get_mainwindow()->summaryview);
		toolbar_main_set_sensitive(mainwindow_get_mainwindow());
	}
}

void procmsg_unregister_spam_learner (int (*learn_func)(MsgInfo *info, GSList *list, gboolean spam))
{
	spam_learners = g_slist_remove(spam_learners, learn_func);
	if (mainwindow_get_mainwindow()) {
		main_window_set_menu_sensitive(mainwindow_get_mainwindow());
		summary_set_menu_sensitive(
			mainwindow_get_mainwindow()->summaryview);
		toolbar_main_set_sensitive(mainwindow_get_mainwindow());
	}
}

gboolean procmsg_spam_can_learn(void)
{
	return g_slist_length(spam_learners) > 0;
}

int procmsg_spam_learner_learn (MsgInfo *info, GSList *list, gboolean spam)
{
	GSList *cur = spam_learners;
	int ret = 0;
	for (; cur; cur = cur->next) {
		int ((*func)(MsgInfo *info, GSList *list, gboolean spam)) = cur->data;
		ret |= func(info, list, spam);
	}
	return ret;
}

static gchar *spam_folder_item = NULL;
static FolderItem * (*procmsg_spam_get_folder_func)(MsgInfo *msginfo) = NULL;
void procmsg_spam_set_folder (const char *item_identifier, FolderItem *(*spam_get_folder_func)(MsgInfo *info))
{
	g_free(spam_folder_item);
	if (item_identifier)
		spam_folder_item = g_strdup(item_identifier);
	else
		spam_folder_item = NULL;
	if (spam_get_folder_func != NULL)
		procmsg_spam_get_folder_func = spam_get_folder_func;
	else
		procmsg_spam_get_folder_func = NULL;
}

FolderItem *procmsg_spam_get_folder (MsgInfo *msginfo)
{
	FolderItem *item = NULL;
	
	if (procmsg_spam_get_folder_func) 
		item = procmsg_spam_get_folder_func(msginfo);
	if (item == NULL && spam_folder_item)
		item = folder_find_item_from_identifier(spam_folder_item);
	if (item == NULL)
		item = folder_get_default_trash();
	return item;
}

static void item_has_queued_mails(FolderItem *item, gpointer data)
{
	gboolean *result = (gboolean *)data;
	if (*result == TRUE)
		return;
	if (folder_has_parent_of_type(item, F_QUEUE)) {
		if (item->total_msgs == 0)
			return;
		else {
			GSList *msglist = folder_item_get_msg_list(item);
			GSList *cur;
			for (cur = msglist; cur; cur = cur->next) {
				MsgInfo *msginfo = (MsgInfo *)cur->data;
				if (!MSG_IS_DELETED(msginfo->flags) &&
				    !MSG_IS_LOCKED(msginfo->flags)) {
					*result = TRUE;
					break;
				}
			}
			procmsg_msg_list_free(msglist);
		}
	}
}

gboolean procmsg_have_queued_mails_fast (void)
{
	gboolean result = FALSE;
	folder_func_to_all_folders(item_has_queued_mails, &result);
	return result;
}

static void item_has_trashed_mails(FolderItem *item, gpointer data)
{
	gboolean *result = (gboolean *)data;
	if (*result == TRUE)
		return;
	if (folder_has_parent_of_type(item, F_TRASH) && item->total_msgs > 0)
		*result = TRUE;
}

gboolean procmsg_have_trashed_mails_fast (void)
{
	gboolean result = FALSE;
	folder_func_to_all_folders(item_has_trashed_mails, &result);
	return result;
}

gchar *procmsg_msginfo_get_tags_str(MsgInfo *msginfo)
{
	GSList *cur = NULL;
	gchar *tags = NULL;
	
	if (!msginfo)
		return NULL;

	if (msginfo->tags == NULL)
		return NULL;
	for (cur = msginfo->tags; cur; cur = cur->next) {
		const gchar *tag = tags_get_tag(GPOINTER_TO_INT(cur->data));
		if (!tag)
			continue;
		if (!tags)
			tags = g_strdup(tag);
		else {
			int olen = strlen(tags);
			int nlen = olen + strlen(tag) + 2 /* strlen(", ") */;
			tags = g_realloc(tags, nlen+1);
			if (!tags)
				return NULL;
			strcpy(tags+olen, ", ");
			strcpy(tags+olen+2, tag);
			tags[nlen]='\0';
		}
	}
	return tags;
}

void procmsg_msginfo_update_tags(MsgInfo *msginfo, gboolean set, gint id)
{
	GSList changed;

	if (id == 0)
		return;

	if (!set) {
		msginfo->tags = g_slist_remove(
					msginfo->tags,
					GINT_TO_POINTER(id));
		changed.data = GINT_TO_POINTER(id);
		changed.next = NULL;
		folder_item_commit_tags(msginfo->folder, msginfo, NULL, &changed);
	} else {
		if (!g_slist_find(msginfo->tags, GINT_TO_POINTER(id))) {
			msginfo->tags = g_slist_append(
					msginfo->tags,
					GINT_TO_POINTER(id));
		}
		changed.data = GINT_TO_POINTER(id);
		changed.next = NULL;
		folder_item_commit_tags(msginfo->folder, msginfo, &changed, NULL);
	}
	
}

void procmsg_msginfo_clear_tags(MsgInfo *msginfo)
{
	GSList *unset = msginfo->tags;
	msginfo->tags = NULL;
	folder_item_commit_tags(msginfo->folder, msginfo, NULL, unset);
	g_slist_free(unset);
}
