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
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <time.h>

#include "folder.h"
#include "folder_item_prefs.h"
#include "mh.h"
#include "procmsg.h"
#include "procheader.h"
#include "utils.h"
#include "codeconv.h"
#include "statusbar.h"
#include "gtkutils.h"
#include "timing.h"

/* Define possible missing constants for Windows. */
#ifdef G_OS_WIN32
# ifndef S_IRGRP
# define S_IRGRP 0
# define S_IWGRP 0
# endif
# ifndef S_IROTH
# define S_IROTH 0
# define S_IWOTH 0
# endif
#endif


static void	mh_folder_init		(Folder		*folder,
					 const gchar	*name,
					 const gchar	*path);

static Folder	*mh_folder_new		(const gchar	*name,
					 const gchar	*path);
static void     mh_folder_destroy	(Folder		*folder);
static gchar   *mh_fetch_msg		(Folder		*folder,
					 FolderItem	*item,
					 gint		 num);
static MsgInfo *mh_get_msginfo		(Folder		*folder,
					 FolderItem	*item,
					 gint		 num);
static gint     mh_add_msg		(Folder		*folder,
					 FolderItem	*dest,
					 const gchar	*file,
					 MsgFlags	*flags);
static gint     mh_add_msgs		(Folder		*folder,
					 FolderItem	*dest,
					 GSList		*file_list,
					 GHashTable 	*relation);
static gint     mh_copy_msg		(Folder		*folder,
					 FolderItem	*dest,
					 MsgInfo	*msginfo);
static gint	mh_copy_msgs		(Folder 	*folder, 
					 FolderItem 	*dest, 
					 MsgInfoList 	*msglist, 
			 		 GHashTable 	*relation);
static gint     mh_remove_msg		(Folder		*folder,
					 FolderItem	*item,
					 gint 		 num);
static gint 	mh_remove_msgs		(Folder 	*folder, 
					 FolderItem 	*item, 
		    			 MsgInfoList 	*msglist, 
					 GHashTable 	*relation);
static gint     mh_remove_all_msg	(Folder		*folder,
					 FolderItem	*item);
static gboolean mh_is_msg_changed	(Folder		*folder,
					 FolderItem	*item,
					 MsgInfo	*msginfo);

static gint 	mh_get_num_list		(Folder 	*folder,
			    		 FolderItem 	*item, 
					 GSList 	**list, 
					 gboolean 	*old_uids_valid);
static gint 	mh_scan_tree		(Folder 	*folder);

static gint    mh_create_tree		(Folder		*folder);
static FolderItem *mh_create_folder	(Folder		*folder,
					 FolderItem	*parent,
					 const gchar	*name);
static gint    mh_rename_folder		(Folder		*folder,
					 FolderItem	*item,
					 const gchar	*name);
static gint    mh_remove_folder		(Folder		*folder,
					 FolderItem	*item);

static gchar   *mh_get_new_msg_filename		(FolderItem	*dest);

static MsgInfo *mh_parse_msg			(const gchar	*file,
						 FolderItem	*item);
static void	mh_remove_missing_folder_items	(Folder		*folder);
static gchar	*mh_filename_from_utf8		(const gchar	*path);
static gchar	*mh_filename_to_utf8		(const gchar	*path);
static void	mh_scan_tree_recursive		(FolderItem	*item);

static gboolean mh_rename_folder_func		(GNode		*node,
						 gpointer	 data);
static gchar   *mh_item_get_path		(Folder *folder, 
						 FolderItem *item);

static gboolean mh_scan_required	(Folder		*folder,
					 FolderItem	*item);
static void mh_set_mtime		(Folder		*folder,
					 FolderItem *item);
static int mh_item_close		(Folder		*folder,
					 FolderItem	*item);
#if 0
static gint mh_get_flags		(Folder *folder, FolderItem *item,
                           		 MsgInfoList *msginfo_list, GHashTable *msgflags);
#endif
static void mh_write_sequences		(FolderItem 	*item, gboolean remove_unseen);

static FolderClass mh_class;

FolderClass *mh_get_class(void)
{
	if (mh_class.idstr == NULL) {
		mh_class.type = F_MH;
		mh_class.idstr = "mh";
		mh_class.uistr = "MH";
		mh_class.supports_server_search = FALSE;
		
		/* Folder functions */
		mh_class.new_folder = mh_folder_new;
		mh_class.destroy_folder = mh_folder_destroy;
		mh_class.set_xml = folder_local_set_xml;
		mh_class.get_xml = folder_local_get_xml;
		mh_class.scan_tree = mh_scan_tree;
		mh_class.create_tree = mh_create_tree;

		/* FolderItem functions */
		mh_class.item_get_path = mh_item_get_path;
		mh_class.create_folder = mh_create_folder;
		mh_class.rename_folder = mh_rename_folder;
		mh_class.remove_folder = mh_remove_folder;
		mh_class.get_num_list = mh_get_num_list;
		mh_class.scan_required = mh_scan_required;
		mh_class.set_mtime = mh_set_mtime;
		mh_class.close = mh_item_close;
		mh_class.get_flags = NULL; /*mh_get_flags */;

		/* Message functions */
		mh_class.get_msginfo = mh_get_msginfo;
		mh_class.fetch_msg = mh_fetch_msg;
		mh_class.add_msg = mh_add_msg;
		mh_class.add_msgs = mh_add_msgs;
		mh_class.copy_msg = mh_copy_msg;
		mh_class.copy_msgs = mh_copy_msgs;
		mh_class.search_msgs = folder_item_search_msgs_local;
		mh_class.remove_msg = mh_remove_msg;
		mh_class.remove_msgs = mh_remove_msgs;
		mh_class.remove_all_msg = mh_remove_all_msg;
		mh_class.is_msg_changed = mh_is_msg_changed;
	}

	return &mh_class;
}

static Folder *mh_folder_new(const gchar *name, const gchar *path)
{
	Folder *folder;

	folder = (Folder *)g_new0(MHFolder, 1);
	folder->klass = &mh_class;
	mh_folder_init(folder, name, path);

	return folder;
}

static void mh_folder_destroy(Folder *folder)
{
	folder_local_folder_destroy(LOCAL_FOLDER(folder));
}

static void mh_folder_init(Folder *folder, const gchar *name, const gchar *path)
{
	folder_local_folder_init(folder, name, path);

}

gboolean mh_scan_required(Folder *folder, FolderItem *item)
{
	gchar *path;
	struct stat s;

	path = folder_item_get_path(item);
	cm_return_val_if_fail(path != NULL, FALSE);

	if (g_stat(path, &s) < 0) {
		FILE_OP_ERROR(path, "stat");
		g_free(path);
		return FALSE;
	}

	if ((s.st_mtime > item->mtime) &&
		(s.st_mtime - 3600 != item->mtime)) {
		debug_print("MH scan required, folder updated: %s (%ld > %ld)\n",
			    path?path:"(null)",
			    (long int) s.st_mtime,
			    (long int) item->mtime);
		g_free(path);
		return TRUE;
	}

	debug_print("MH scan not required: %s (%ld <= %ld)\n",
		    path?path:"(null)",
		    (long int) s.st_mtime,
		    (long int) item->mtime);
	g_free(path);
	return FALSE;
}

static void mh_get_last_num(Folder *folder, FolderItem *item)
{
	gchar *path;
	DIR *dp;
	struct dirent *d;
	gint max = 0;
	gint num;

	cm_return_if_fail(item != NULL);

	debug_print("mh_get_last_num(): Scanning %s ...\n", item->path?item->path:"(null)");

	path = folder_item_get_path(item);
	cm_return_if_fail(path != NULL);
	if (change_dir(path) < 0) {
		g_free(path);
		return;
	}
	g_free(path);

	if ((dp = opendir(".")) == NULL) {
		FILE_OP_ERROR(item->path, "opendir");
		return;
	}

	while ((d = readdir(dp)) != NULL) {
		if ((num = to_number(d->d_name)) > 0 &&
		    dirent_is_regular_file(d)) {
			if (max < num)
				max = num;
		}
		if (num % 2000 == 0)
			GTK_EVENTS_FLUSH();
	}
	closedir(dp);

	debug_print("Last number in dir %s = %d\n", item->path?item->path:"(null)", max);
	item->last_num = max;
}

gint mh_get_num_list(Folder *folder, FolderItem *item, GSList **list, gboolean *old_uids_valid)
{

	gchar *path;
	DIR *dp;
	struct dirent *d;
	gint num, nummsgs = 0;

	cm_return_val_if_fail(item != NULL, -1);

	debug_print("mh_get_num_list(): Scanning %s ...\n", item->path?item->path:"(null)");

	*old_uids_valid = TRUE;

	path = folder_item_get_path(item);
	cm_return_val_if_fail(path != NULL, -1);
	if (change_dir(path) < 0) {
		g_free(path);
		return -1;
	}
	g_free(path);

	if ((dp = opendir(".")) == NULL) {
		FILE_OP_ERROR(item->path, "opendir");
		return -1;
	}

	while ((d = readdir(dp)) != NULL) {
		if ((num = to_number(d->d_name)) > 0) {
			*list = g_slist_prepend(*list, GINT_TO_POINTER(num));
		   	nummsgs++;
		}
	}
	closedir(dp);

	mh_set_mtime(folder, item);
	return nummsgs;
}

static gchar *mh_fetch_msg(Folder *folder, FolderItem *item, gint num)
{
	gchar *path;
	gchar *file;

	cm_return_val_if_fail(item != NULL, NULL);
	cm_return_val_if_fail(num > 0, NULL);

	path = folder_item_get_path(item);
	file = g_strconcat(path, G_DIR_SEPARATOR_S, itos(num), NULL);

	if (!is_file_exist(file)) {
		g_free(file);
		g_free(path);
		return NULL;
	}
	g_free(path);
	return file;
}

static MsgInfo *mh_get_msginfo(Folder *folder, FolderItem *item, gint num)
{
	MsgInfo *msginfo;
	gchar *file;

	cm_return_val_if_fail(item != NULL, NULL);
	if (num <= 0)
		return NULL;

	file = mh_fetch_msg(folder, item, num);
	if (!file) return NULL;

	msginfo = mh_parse_msg(file, item);
	if (msginfo)
		msginfo->msgnum = num;

	g_free(file);

	return msginfo;
}

static gchar *mh_get_new_msg_filename(FolderItem *dest)
{
	gchar *destfile;
	gchar *destpath;

	destpath = folder_item_get_path(dest);
	cm_return_val_if_fail(destpath != NULL, NULL);

	if (!is_dir_exist(destpath))
		make_dir_hier(destpath);

	for (;;) {
		destfile = g_strdup_printf("%s%c%d", destpath, G_DIR_SEPARATOR,
					   dest->last_num + 1);
		if (is_file_entry_exist(destfile)) {
			dest->last_num++;
			g_free(destfile);
		} else
			break;
	}

	g_free(destpath);

	return destfile;
}

static gint mh_add_msg(Folder *folder, FolderItem *dest, const gchar *file, MsgFlags *flags)
{
	gint ret;
	GSList file_list;
	MsgFileInfo fileinfo;

	cm_return_val_if_fail(file != NULL, -1);

	fileinfo.msginfo = NULL;
	fileinfo.file = (gchar *)file;
	fileinfo.flags = flags;
	file_list.data = &fileinfo;
	file_list.next = NULL;

        ret = mh_add_msgs(folder, dest, &file_list, NULL);
	return ret;
} 
 
static gint mh_add_msgs(Folder *folder, FolderItem *dest, GSList *file_list, 
                 GHashTable *relation)
{ 
	gchar *destfile;
	GSList *cur;
	MsgFileInfo *fileinfo;

	cm_return_val_if_fail(dest != NULL, -1);
	cm_return_val_if_fail(file_list != NULL, -1);

	if (dest->last_num < 0) {
		mh_get_last_num(folder, dest);
		if (dest->last_num < 0) return -1;
	}

	for (cur = file_list; cur != NULL; cur = cur->next) {
		fileinfo = (MsgFileInfo *)cur->data;

		destfile = mh_get_new_msg_filename(dest);
		if (destfile == NULL) return -1;

#ifdef G_OS_UNIX
		if (link(fileinfo->file, destfile) < 0) {
#endif
			if (copy_file(fileinfo->file, destfile, TRUE) < 0) {
				g_warning(_("can't copy message %s to %s\n"),
					  fileinfo->file, destfile);
				g_free(destfile);
				return -1;
			}
#ifdef G_OS_UNIX
		}
#endif

		if (relation != NULL)
			g_hash_table_insert(relation, fileinfo, GINT_TO_POINTER(dest->last_num + 1));
		g_free(destfile);
		dest->last_num++;
	}
	mh_write_sequences(dest, TRUE);
	return dest->last_num;
}

static gint mh_copy_msg(Folder *folder, FolderItem *dest, MsgInfo *msginfo)
{
	GSList msglist;

	cm_return_val_if_fail(msginfo != NULL, -1);

	msglist.data = msginfo;
	msglist.next = NULL;

	return mh_copy_msgs(folder, dest, &msglist, NULL);	
}

static gint mh_copy_msgs(Folder *folder, FolderItem *dest, MsgInfoList *msglist, 
			 GHashTable *relation)
{
	gboolean dest_need_scan = FALSE;
	gboolean src_need_scan = FALSE;
	FolderItem *src = NULL;
	gchar *srcfile;
	gchar *destfile;
	FolderItemPrefs *prefs;
	MsgInfo *msginfo = NULL;
	MsgInfoList *cur = NULL;
	gint curnum = 0, total = 0;
	gchar *srcpath = NULL;
	gboolean full_fetch = FALSE;
	time_t last_dest_mtime = (time_t)0;
	time_t last_src_mtime = (time_t)0;

	cm_return_val_if_fail(dest != NULL, -1);
	cm_return_val_if_fail(msglist != NULL, -1);
	
	msginfo = (MsgInfo *)msglist->data;

	cm_return_val_if_fail(msginfo != NULL, -1);

	if (msginfo->folder == dest) {
		g_warning("the src folder is identical to the dest.\n");
		return -1;
	}

	if (msginfo->folder->folder != dest->folder)
		full_fetch = TRUE;
	
	if (FOLDER_TYPE(msginfo->folder->folder) == F_MH) {
		src = msginfo->folder;
	}

	if (dest->last_num < 0) {
		mh_get_last_num(folder, dest);
		if (dest->last_num < 0) return -1;
	}

	prefs = dest->prefs;

	srcpath = folder_item_get_path(msginfo->folder);

	dest_need_scan = mh_scan_required(dest->folder, dest);
	last_dest_mtime = dest->mtime;

	if (src) {
		src_need_scan = mh_scan_required(src->folder, src);
		last_src_mtime = src->mtime;
	}

	total = g_slist_length(msglist);
	if (total > 100) {
		if (MSG_IS_MOVE(msginfo->flags))
			statusbar_print_all(_("Moving messages..."));
		else
			statusbar_print_all(_("Copying messages..."));
	}
	for (cur = msglist; cur; cur = cur->next) {
		msginfo = (MsgInfo *)cur->data;
		if (!msginfo) {
			goto err_reset_status;
		}
		if (!full_fetch) {
			srcfile = g_strconcat(srcpath, 
				G_DIR_SEPARATOR_S, 
				itos(msginfo->msgnum), NULL);
		} else {
			srcfile = procmsg_get_message_file(msginfo);
		}
		if (!srcfile) {
			goto err_reset_status;
		}
		destfile = mh_get_new_msg_filename(dest);
		if (!destfile) {
			g_free(srcfile);
			goto err_reset_status;
		}

		if (total > 100) {
			statusbar_progress_all(curnum, total, 100);
			if (curnum % 100 == 0)
				GTK_EVENTS_FLUSH();
			curnum++;
		}

		debug_print("Copying message %s%c%d to %s ...\n",
			    msginfo->folder->path, G_DIR_SEPARATOR,
			    msginfo->msgnum, dest->path);


		if (MSG_IS_MOVE(msginfo->flags)) {
			msginfo->flags.tmp_flags &= ~MSG_MOVE_DONE;
			if (move_file(srcfile, destfile, TRUE) < 0) {
				FILE_OP_ERROR(srcfile, "move");
				if (copy_file(srcfile, destfile, TRUE) < 0) {
					FILE_OP_ERROR(srcfile, "copy");
					g_free(srcfile);
					g_free(destfile);
					goto err_reset_status;
				}
			} else {
				/* say unlinking's not necessary */
				msginfo->flags.tmp_flags |= MSG_MOVE_DONE;
			}
		} else if (copy_file(srcfile, destfile, TRUE) < 0) {
			FILE_OP_ERROR(srcfile, "copy");
			g_free(srcfile);
			g_free(destfile);
			goto err_reset_status;
		} 
		if (prefs && prefs->enable_folder_chmod && prefs->folder_chmod) {
			if (chmod(destfile, prefs->folder_chmod) < 0)
				FILE_OP_ERROR(destfile, "chmod");
		}
		if (relation) {
			if (g_hash_table_lookup(relation, msginfo) != NULL)
				g_warning("already in : %p", msginfo);
			
			g_hash_table_insert(relation, msginfo, GINT_TO_POINTER(dest->last_num+1));
		}
		g_free(srcfile);
		g_free(destfile);
		dest->last_num++;
	}

	g_free(srcpath);
	mh_write_sequences(dest, TRUE);

	if (dest->mtime == last_dest_mtime && !dest_need_scan) {
		mh_set_mtime(folder, dest);
	}

	if (src && src->mtime == last_src_mtime && !src_need_scan) {
		mh_set_mtime(folder, src);
	}

	if (total > 100) {
		statusbar_progress_all(0,0,0);
		statusbar_pop_all();
	}
	return dest->last_num;
err_reset_status:
	g_free(srcpath);
	mh_write_sequences(dest, TRUE);
	if (total > 100) {
		statusbar_progress_all(0,0,0);
		statusbar_pop_all();
	}
	return -1;

}

static gint mh_remove_msg(Folder *folder, FolderItem *item, gint num)
{
	gboolean need_scan = FALSE;
	time_t last_mtime = (time_t)0;
	gchar *file;

	cm_return_val_if_fail(item != NULL, -1);

	file = mh_fetch_msg(folder, item, num);
	cm_return_val_if_fail(file != NULL, -1);

	need_scan = mh_scan_required(folder, item);
	last_mtime = item->mtime;

	if (claws_unlink(file) < 0) {
		FILE_OP_ERROR(file, "unlink");
		g_free(file);
		return -1;
	}

	if (item->mtime == last_mtime && !need_scan) {
		mh_set_mtime(folder, item);
	}
	g_free(file);
	return 0;
}

static gint mh_remove_msgs(Folder *folder, FolderItem *item, 
		    MsgInfoList *msglist, GHashTable *relation)
{
	gboolean need_scan = FALSE;
	gchar *path, *file;
	time_t last_mtime = (time_t)0;
	MsgInfoList *cur;
	gint total = 0, curnum = 0;

	cm_return_val_if_fail(item != NULL, -1);

	path = folder_item_get_path(item);
	
	need_scan = mh_scan_required(folder, item);
	last_mtime = item->mtime;

	total = g_slist_length(msglist);
	if (total > 100) {
		statusbar_print_all(_("Deleting messages..."));
	}

	for (cur = msglist; cur; cur = cur->next) {
		MsgInfo *msginfo = (MsgInfo *)cur->data;
		if (msginfo == NULL)
			continue;
		if (MSG_IS_MOVE(msginfo->flags) && MSG_IS_MOVE_DONE(msginfo->flags)) {
			msginfo->flags.tmp_flags &= ~MSG_MOVE_DONE;
			continue;
		}
		if (total > 100) {
			statusbar_progress_all(curnum, total, 100);
			if (curnum % 100 == 0)
				GTK_EVENTS_FLUSH();
			curnum++;
		}

		file = g_strconcat(path, G_DIR_SEPARATOR_S, itos(msginfo->msgnum), NULL);
		if (file == NULL)
			continue;
		
		if (claws_unlink(file) < 0) {
			g_free(file);
			continue;
		}
		
		g_free(file);
	}

	if (total > 100) {
		statusbar_progress_all(0,0,0);
		statusbar_pop_all();
	}
	if (item->mtime == last_mtime && !need_scan) {
		mh_set_mtime(folder, item);
	}

	g_free(path);
	return 0;
}

static gint mh_remove_all_msg(Folder *folder, FolderItem *item)
{
	gchar *path;
	gint val;

	cm_return_val_if_fail(item != NULL, -1);

	path = folder_item_get_path(item);
	cm_return_val_if_fail(path != NULL, -1);
	val = remove_all_numbered_files(path);
	g_free(path);

	mh_write_sequences(item, TRUE);

	return val;
}

static gboolean mh_is_msg_changed(Folder *folder, FolderItem *item,
				  MsgInfo *msginfo)
{
	struct stat s;

	if (g_stat(itos(msginfo->msgnum), &s) < 0 ||
	    msginfo->size  != s.st_size || (
		(msginfo->mtime - s.st_mtime != 0) &&
		(msginfo->mtime - s.st_mtime != 3600) &&
		(msginfo->mtime - s.st_mtime != -3600)))
		return TRUE;

	return FALSE;
}

static gint mh_scan_tree(Folder *folder)
{
	FolderItem *item;
	gchar *rootpath;

	cm_return_val_if_fail(folder != NULL, -1);

	if (!folder->node) {
		item = folder_item_new(folder, folder->name, NULL);
		item->folder = folder;
		folder->node = item->node = g_node_new(item);
	} else
		item = FOLDER_ITEM(folder->node->data);

	rootpath = folder_item_get_path(item);
	if (change_dir(rootpath) < 0) {
		g_free(rootpath);
		return -1;
	}
	g_free(rootpath);

	mh_create_tree(folder);
	mh_remove_missing_folder_items(folder);
	mh_scan_tree_recursive(item);

	return 0;
}

#define MAKE_DIR_IF_NOT_EXIST(dir) \
{ \
	if (!is_dir_exist(dir)) { \
		if (is_file_exist(dir)) { \
			g_warning("File `%s' already exists.\n" \
				    "Can't create folder.", dir); \
			return -1; \
		} \
		if (make_dir_hier(dir) < 0) \
			return -1; \
	} \
}

static gint mh_create_tree(Folder *folder)
{
	gchar *rootpath, *f;

	cm_return_val_if_fail(folder != NULL, -1);

	CHDIR_RETURN_VAL_IF_FAIL(get_mail_base_dir(), -1);
	rootpath = LOCAL_FOLDER(folder)->rootpath;
	MAKE_DIR_IF_NOT_EXIST(rootpath);
	CHDIR_RETURN_VAL_IF_FAIL(rootpath, -1);

	/* Create special directories as needed */
	if (folder->inbox != NULL &&
			folder->inbox->path != NULL)
		f = folder->inbox->path;
	else
		f = INBOX_DIR;
	MAKE_DIR_IF_NOT_EXIST(f);

	if (folder->outbox != NULL &&
			folder->outbox->path != NULL)
		f = folder->outbox->path;
	else
		f = OUTBOX_DIR;
	MAKE_DIR_IF_NOT_EXIST(f);

	if (folder->draft != NULL &&
			folder->draft->path != NULL)
		f = folder->draft->path;
	else
		f = DRAFT_DIR;
	MAKE_DIR_IF_NOT_EXIST(f);

	if (folder->queue != NULL &&
			folder->queue->path != NULL)
		f = folder->queue->path;
	else
		f = QUEUE_DIR;
	MAKE_DIR_IF_NOT_EXIST(f);

	if (folder->trash != NULL &&
			folder->trash->path != NULL)
		f = folder->trash->path;
	else
		f = TRASH_DIR;
	MAKE_DIR_IF_NOT_EXIST(f);

	return 0;
}

#undef MAKE_DIR_IF_NOT_EXIST

static gchar *mh_item_get_path(Folder *folder, FolderItem *item)
{
	gchar *folder_path, *path;
	gchar *real_path;
	cm_return_val_if_fail(folder != NULL, NULL);
	cm_return_val_if_fail(item != NULL, NULL);

	folder_path = g_strdup(LOCAL_FOLDER(folder)->rootpath);
	cm_return_val_if_fail(folder_path != NULL, NULL);

        /* FIXME: [W32] The code below does not correctly merge
           relative filenames; there should be a function to handle
           this.  */
        if ( !is_relative_filename (folder_path) ) {
                if (item->path)
                        path = g_strconcat(folder_path, G_DIR_SEPARATOR_S,
                                           item->path, NULL);
                else
                        path = g_strdup(folder_path);
        } else {
                if (item->path)
                        path = g_strconcat(get_home_dir(), G_DIR_SEPARATOR_S,
                                           folder_path, G_DIR_SEPARATOR_S,
                                           item->path, NULL);
                else
                        path = g_strconcat(get_home_dir(), G_DIR_SEPARATOR_S,
                                           folder_path, NULL);
        }
	g_free(folder_path);
	real_path = mh_filename_from_utf8(path);
	if (!is_dir_exist(real_path) && is_dir_exist(path)) {
		/* mmh, older version did put utf8 filenames instead of
		 * the correct encoding */
		g_rename(path, real_path);
		folder_item_scan(item);
	}

	g_free(path);
	return real_path;
}

static FolderItem *mh_create_folder(Folder *folder, FolderItem *parent,
				    const gchar *name)
{
	gchar *path, *real_name;
	gchar *fullpath;
	FolderItem *new_item;
	gchar *mh_sequences_filename;
	FILE *mh_sequences_file;

	cm_return_val_if_fail(folder != NULL, NULL);
	cm_return_val_if_fail(parent != NULL, NULL);
	cm_return_val_if_fail(name != NULL, NULL);

	path = folder_item_get_path(parent);
	if (!is_dir_exist(path)) 
		if (make_dir_hier(path) != 0)
			return NULL;
		
	real_name = mh_filename_from_utf8(name);
	fullpath = g_strconcat(path, G_DIR_SEPARATOR_S, real_name, NULL);
	g_free(real_name);
	g_free(path);

	if (make_dir(fullpath) < 0) {
		g_free(fullpath);
		return NULL;
	}

	g_free(fullpath);

	if (parent->path)
		path = g_strconcat(parent->path, G_DIR_SEPARATOR_S, name,
				   NULL);
	else
		path = g_strdup(name);
	new_item = folder_item_new(folder, name, path);
	folder_item_append(parent, new_item);

	g_free(path);

	path = folder_item_get_path(new_item);
	mh_sequences_filename = g_strconcat(path, G_DIR_SEPARATOR_S,
					    ".mh_sequences", NULL);
	if ((mh_sequences_file = g_fopen(mh_sequences_filename, "a+b")) != NULL) {
		fclose(mh_sequences_file);
	}
	g_free(mh_sequences_filename);
	g_free(path);

	return new_item;
}

static gint mh_rename_folder(Folder *folder, FolderItem *item,
			     const gchar *name)
{
 	gchar *real_name;
	gchar *oldpath;
	gchar *dirname;
	gchar *newpath, *utf8newpath;
	gchar *paths[2];

	cm_return_val_if_fail(folder != NULL, -1);
	cm_return_val_if_fail(item != NULL, -1);
	cm_return_val_if_fail(item->path != NULL, -1);
	cm_return_val_if_fail(name != NULL, -1);

	oldpath = folder_item_get_path(item);
	if (!is_dir_exist(oldpath))
		make_dir_hier(oldpath);

	dirname = g_path_get_dirname(oldpath);
	real_name = mh_filename_from_utf8(name);
	newpath = g_strconcat(dirname, G_DIR_SEPARATOR_S, real_name, NULL);
	g_free(real_name);

	if (g_rename(oldpath, newpath) < 0) {
		FILE_OP_ERROR(oldpath, "rename");
		g_free(oldpath);
		g_free(newpath);
		return -1;
	}

	g_free(oldpath);
	g_free(newpath);

	if (strchr(item->path, G_DIR_SEPARATOR) != NULL) {
		dirname = g_path_get_dirname(item->path);
		utf8newpath = g_strconcat(dirname, G_DIR_SEPARATOR_S,
					  name, NULL);
		g_free(dirname);
	} else
		utf8newpath = g_strdup(name);

	g_free(item->name);
	item->name = g_strdup(name);

	paths[0] = g_strdup(item->path);
	paths[1] = utf8newpath;
	g_node_traverse(item->node, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			mh_rename_folder_func, paths);

	g_free(paths[0]);
	g_free(paths[1]);
	return 0;
}

static gint mh_remove_folder(Folder *folder, FolderItem *item)
{
	gchar *path;

	cm_return_val_if_fail(folder != NULL, -1);
	cm_return_val_if_fail(item != NULL, -1);
	cm_return_val_if_fail(item->path != NULL, -1);

	path = folder_item_get_path(item);
	if (remove_dir_recursive(path) < 0) {
		g_warning("can't remove directory `%s'\n", path);
		g_free(path);
		return -1;
	}

	g_free(path);
	folder_item_remove(item);
	return 0;
}

static MsgInfo *mh_parse_msg(const gchar *file, FolderItem *item)
{
	MsgInfo *msginfo;
	MsgFlags flags;

	cm_return_val_if_fail(item != NULL, NULL);
	cm_return_val_if_fail(file != NULL, NULL);

	flags.perm_flags = MSG_NEW|MSG_UNREAD;
	flags.tmp_flags = 0;

	if (folder_has_parent_of_type(item, F_QUEUE)) {
		MSG_SET_TMP_FLAGS(flags, MSG_QUEUED);
	} else if (folder_has_parent_of_type(item, F_DRAFT)) {
		MSG_SET_TMP_FLAGS(flags, MSG_DRAFT);
	}

	msginfo = procheader_parse_file(file, flags, FALSE, FALSE);
	if (!msginfo) return NULL;

	msginfo->msgnum = atoi(file);
	msginfo->folder = item;

	return msginfo;
}

static gboolean mh_remove_missing_folder_items_func(GNode *node, gpointer data)
{
	FolderItem *item;
	gchar *path;

	cm_return_val_if_fail(node->data != NULL, FALSE);

	if (G_NODE_IS_ROOT(node))
		return FALSE;

	item = FOLDER_ITEM(node->data);

	path = folder_item_get_path(item);
	if (!is_dir_exist(path)) {
		debug_print("folder '%s' not found. removing...\n", path?path:"(null)");
		folder_item_remove(item);
	}
	g_free(path);

	return FALSE;
}

static void mh_remove_missing_folder_items(Folder *folder)
{
	cm_return_if_fail(folder != NULL);

	debug_print("searching missing folders...\n");

	g_node_traverse(folder->node, G_POST_ORDER, G_TRAVERSE_ALL, -1,
			mh_remove_missing_folder_items_func, folder);
}

static void mh_scan_tree_recursive(FolderItem *item)
{
	Folder *folder;
#ifdef G_OS_WIN32
	GDir *dir;
#else
	DIR *dp;
	struct dirent *d;
#endif
	const gchar *dir_name;
	struct stat s;
 	gchar *real_path, *entry, *utf8entry, *utf8name;
	gint n_msg = 0;

	cm_return_if_fail(item != NULL);
	cm_return_if_fail(item->folder != NULL);

	folder = item->folder;

	real_path = item->path ? mh_filename_from_utf8(item->path) : g_strdup(".");
#ifdef G_OS_WIN32
	dir = g_dir_open(real_path, 0, NULL);
	if (!dir) {
		g_warning("failed to open directory: %s\n", real_path);
		g_free(real_path);
		return;
	}
#else
	dp = opendir(real_path);
	if (!dp) {
		FILE_OP_ERROR(real_path, "opendir");
		return;
	}
#endif
	g_free(real_path);

	debug_print("scanning %s ...\n",
		    item->path ? item->path
		    : LOCAL_FOLDER(item->folder)->rootpath);
	if (folder->ui_func)
		folder->ui_func(folder, item, folder->ui_func_data);

#ifdef G_OS_WIN32
	while ((dir_name = g_dir_read_name(dir)) != NULL) {
#else
	while ((d = readdir(dp)) != NULL) {
		dir_name = d->d_name;
#endif
		if (dir_name[0] == '.') continue;

		utf8name = mh_filename_to_utf8(dir_name);
		if (item->path)
			utf8entry = g_strconcat(item->path, G_DIR_SEPARATOR_S,
						utf8name, NULL);
		else
			utf8entry = g_strdup(utf8name);
		entry = mh_filename_from_utf8(utf8entry);

		if (
#if !defined(G_OS_WIN32) && !defined(MAEMO) && defined(HAVE_DIRENT_D_TYPE)
			d->d_type == DT_DIR ||
			(d->d_type == DT_UNKNOWN &&
#endif
			g_stat(entry, &s) == 0 && S_ISDIR(s.st_mode)
#if !defined(G_OS_WIN32) && !defined(MAEMO) && defined(HAVE_DIRENT_D_TYPE)
			)
#endif
		   ) {
			FolderItem *new_item = NULL;
			GNode *node;

			node = item->node;
			for (node = node->children; node != NULL; node = node->next) {
				FolderItem *cur_item = FOLDER_ITEM(node->data);
				gchar *curpath = mh_filename_from_utf8(cur_item->path);
				if (!strcmp2(curpath, entry)) {
					new_item = cur_item;
					g_free(curpath);
					break;
				}
				g_free(curpath);
			}
			if (!new_item) {
				debug_print("new folder '%s' found.\n", entry);
				new_item = folder_item_new(folder, utf8name, utf8entry);
				folder_item_append(item, new_item);
			}

			if (!item->path) {
				if (!folder->inbox &&
				    !strcmp(dir_name, INBOX_DIR)) {
					new_item->stype = F_INBOX;
					folder->inbox = new_item;
				} else if (!folder->outbox &&
					   !strcmp(dir_name, OUTBOX_DIR)) {
					new_item->stype = F_OUTBOX;
					folder->outbox = new_item;
				} else if (!folder->draft &&
					   !strcmp(dir_name, DRAFT_DIR)) {
					new_item->stype = F_DRAFT;
					folder->draft = new_item;
				} else if (!folder->queue &&
					   !strcmp(dir_name, QUEUE_DIR)) {
					new_item->stype = F_QUEUE;
					folder->queue = new_item;
				} else if (!folder->trash &&
					   !strcmp(dir_name, TRASH_DIR)) {
					new_item->stype = F_TRASH;
					folder->trash = new_item;
				}
			}

			mh_scan_tree_recursive(new_item);
		} else if (to_number(dir_name) > 0) n_msg++;

		g_free(entry);
		g_free(utf8entry);
		g_free(utf8name);
	}

#ifdef G_OS_WIN32
	g_dir_close(dir);
#else
	closedir(dp);
#endif

	mh_set_mtime(folder, item);
}

static gboolean mh_rename_folder_func(GNode *node, gpointer data)
{
	FolderItem *item = node->data;
	gchar **paths = data;
	const gchar *oldpath = paths[0];
	const gchar *newpath = paths[1];
	gchar *base;
	gchar *new_itempath;
	gint oldpathlen;

	oldpathlen = strlen(oldpath);
	if (strncmp(oldpath, item->path, oldpathlen) != 0) {
		g_warning("path doesn't match: %s, %s\n", oldpath, item->path);
		return TRUE;
	}

	base = item->path + oldpathlen;
	while (*base == G_DIR_SEPARATOR) base++;
	if (*base == '\0')
		new_itempath = g_strdup(newpath);
	else
		new_itempath = g_strconcat(newpath, G_DIR_SEPARATOR_S, base,
					   NULL);
	g_free(item->path);
	item->path = new_itempath;

	return FALSE;
}

static gchar *mh_filename_from_utf8(const gchar *path)
{
	gchar *real_path = g_filename_from_utf8(path, -1, NULL, NULL, NULL);

	if (!real_path) {
		g_warning("mh_filename_from_utf8: failed to convert character set\n");
		real_path = g_strdup(path);
	}

	return real_path;
}

static gchar *mh_filename_to_utf8(const gchar *path)
{
	gchar *utf8path = g_filename_to_utf8(path, -1, NULL, NULL, NULL);
	if (!utf8path) {
		g_warning("mh_filename_to_utf8: failed to convert character set\n");
		utf8path = g_strdup(path);
	}

	return utf8path;
}

static gint sort_cache_list_by_msgnum(gconstpointer a, gconstpointer b)
{
	MsgInfo *msginfo_a = (MsgInfo *) a;
	MsgInfo *msginfo_b = (MsgInfo *) b;

	return (msginfo_a->msgnum - msginfo_b->msgnum);
}

static gchar *get_unseen_seq_name(void)
{
	static gchar *seq_name = NULL;
	if (!seq_name) {
		gchar buf[BUFFSIZE];
		gchar *tmp;
		gchar *profile_path = g_strconcat(
			get_home_dir(), G_DIR_SEPARATOR_S,
			".mh_profile", NULL);
		FILE *fp = g_fopen(profile_path, "r");
		if (fp) {
			while (fgets(buf, sizeof(buf), fp) != NULL) {
				if (!strncmp(buf, "Unseen-Sequence:", strlen("Unseen-Sequence:"))) {
					gchar *seq_tmp = buf+strlen("Unseen-Sequence:");
					while (*seq_tmp == ' ')
						seq_tmp++;
					seq_name = g_strdup(seq_tmp);
					seq_name = strretchomp(seq_name);
					break;
				}
			}
			fclose(fp);
		}
		if (!seq_name)
			seq_name = g_strdup("unseen");
		tmp = g_strdup_printf("%s:", seq_name);
		g_free(seq_name);
		seq_name = tmp;
	}
	return seq_name;	
}

static void mh_write_sequences(FolderItem *item, gboolean remove_unseen)
{
	gchar *mh_sequences_old, *mh_sequences_new;
	FILE *mh_sequences_old_fp, *mh_sequences_new_fp;
	gchar buf[BUFFSIZE];
	gchar *path = NULL;
	gboolean err = FALSE;
	START_TIMING("");

	if (!item)
		return;
	
	path = folder_item_get_path(item);

	mh_sequences_old = g_strconcat(path, G_DIR_SEPARATOR_S,
					    ".mh_sequences", NULL);
	mh_sequences_new = g_strconcat(path, G_DIR_SEPARATOR_S,
					    ".mh_sequences.new", NULL);
	if ((mh_sequences_new_fp = g_fopen(mh_sequences_new, "w+b")) != NULL) {
		GSList *msglist = folder_item_get_msg_list(item);
		GSList *cur;
		MsgInfo *info = NULL;
		gint start = -1, end = -1;
		gchar *sequence = g_strdup("");
		gint seq_len = 0;
		msglist = g_slist_sort(msglist, sort_cache_list_by_msgnum);
		cur = msglist;
		
		/* write the unseen sequence if we don't have to scrap it */
		if (!remove_unseen) do {
			info = (MsgInfo *)(cur ? cur->data:NULL);
			if (info && (MSG_IS_UNREAD(info->flags) || MSG_IS_NEW(info->flags))) {
				if (start < 0)
					start = end = info->msgnum;
				else
					end = info->msgnum;
			} else {
				if (start > 0 && end > 0) {
					gchar tmp[32];
					gint tmp_len = 0;
					if (start != end)
						snprintf(tmp, 31, " %d-%d", start, end);
					else
						snprintf(tmp, 31, " %d", start);
					
					tmp_len = strlen(tmp);
					sequence = g_realloc(sequence, seq_len+tmp_len+1);
					strcpy(sequence+seq_len, tmp);
					seq_len += tmp_len;

					start = end = -1;
				}
			}
			cur = cur ? cur->next:NULL;
		} while (cur || (start > 0 && end > 0));
		if (sequence && *sequence) {
			if (fprintf(mh_sequences_new_fp, "%s%s\n", 
					get_unseen_seq_name(), sequence) < 0)
				err = TRUE;
			else
				debug_print("wrote unseen sequence: '%s%s'\n", 
					get_unseen_seq_name(), sequence);
		}
		/* rewrite the rest of the file */
		if ((mh_sequences_old_fp = g_fopen(mh_sequences_old, "r+b")) != NULL) {
			while (fgets(buf, sizeof(buf), mh_sequences_old_fp) != NULL) {
				if (strncmp(buf, get_unseen_seq_name(), strlen(get_unseen_seq_name())))
					if (fprintf(mh_sequences_new_fp, "%s", buf) < 0) {
						err = TRUE;
						break;
					}
			}
			fclose(mh_sequences_old_fp);
		}
		
		fflush(mh_sequences_new_fp);
#if 0
		fsync(fileno(mh_sequences_new_fp));
#endif
		if (fclose(mh_sequences_new_fp) == EOF)
			err = TRUE;

		if (!err)
			g_rename(mh_sequences_new, mh_sequences_old);
		g_free(sequence);
		procmsg_msg_list_free(msglist);
	}
	g_free(mh_sequences_old);
	g_free(mh_sequences_new);
	g_free(path);

	END_TIMING();
}

static int mh_item_close(Folder *folder, FolderItem *item)
{
	time_t last_mtime = (time_t)0;
	gboolean need_scan = mh_scan_required(item->folder, item);
	last_mtime = item->mtime;

	mh_write_sequences(item, FALSE);

	if (item->mtime == last_mtime && !need_scan) {
		mh_set_mtime(folder, item);
	}

	return 0;
}

static void mh_set_mtime(Folder *folder, FolderItem *item)
{
	struct stat s;
	gchar *path = folder_item_get_path(item);

	cm_return_if_fail(path != NULL);

	if (g_stat(path, &s) < 0) {
		FILE_OP_ERROR(path, "stat");
		g_free(path);
		return;
	}

	item->mtime = s.st_mtime;
	debug_print("MH: forced mtime of %s to %ld\n", item->name?item->name:"(null)", item->mtime);
	g_free(path);
}
