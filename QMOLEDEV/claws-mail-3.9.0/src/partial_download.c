/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Colin Leroy <colin@colino.net> 
 * and the Claws Mail team
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

/* Partial download:
 * A mail which has been completely downloaded will have no special headers,
 * and its entry in the uidl file will end by 0 (POP3_TOTALLY_RECEIVED);
 *
 * A mail which has been partially downloaded will have some special headers,
 * and its entry in the uidl file will first be 1 (POP3_PARTIALLY_RECEIVED);
 * the special headers will be including "SC-Marked-For-Download" which can 
 * have three values:
 * 0 (POP3_PARTIAL_DLOAD_UNKN) meaning that the user has not yet chosen to
 *  download the mail or let it be deleted - this header is absent until the
 *  user first chooses an action
 * 1 (POP3_PARTIAL_DLOAD_DLOAD) meaning that the user wants to finish 
 *  downloading the mail
 * 2 (POP3_PARTIAL_DLOAD_DELE) meaning that the user does not want to finish
 *  downloading the mail
 * When updating this header to POP3_PARTIAL_DLOAD_DLOAD, the uidl line of
 * this mail will end with the mail's physical path, which Sylpheed will remove
 * after having downloaded the complete mail. msg->partial_recv will equal
 * 2 (POP3_MUST_COMPLETE_RECV).
 * When updating this header to POP3_PARTIAL_DLOAD_DELE, the uidl line of
 * this mail will be 0 (POP3_TOTALLY_RECEIVED), which will let Sylpheed delete
 * this mail from the server as soon as the leave_time preference specifies.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>

#include "partial_download.h"
#include "utils.h"
#include "pop.h"
#include "folder.h"
#include "procheader.h"
#include "msgcache.h"

int partial_msg_in_uidl_list(MsgInfo *msginfo)
{
	gchar *path;
	FILE *fp;
	gchar buf[POPBUFSIZE];
	gchar uidl[POPBUFSIZE];
	time_t recv_time;
	time_t now;
	gchar *sanitized_uid = NULL;
	
	if (!msginfo->extradata)
		return FALSE;

	sanitized_uid = g_strdup(msginfo->extradata->account_login);
	
	subst_for_filename(sanitized_uid);

	if (!msginfo->extradata->account_server
	||  !msginfo->extradata->account_login
	||  !msginfo->extradata->partial_recv)
		return FALSE;
	
	path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			   "uidl", G_DIR_SEPARATOR_S, msginfo->extradata->account_server,
			   "-", msginfo->extradata->account_login, NULL);
	if ((fp = g_fopen(path, "rb")) == NULL) {
		if (ENOENT != errno) FILE_OP_ERROR(path, "fopen");
		g_free(path);
		path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
				   "uidl-", msginfo->extradata->account_server,
				   "-", sanitized_uid, NULL);
		if ((fp = g_fopen(path, "rb")) == NULL) {
			if (ENOENT != errno) FILE_OP_ERROR(path, "fopen");
			g_free(sanitized_uid);
			g_free(path);
			return FALSE;
		}
	}
	g_free(sanitized_uid);
	g_free(path);

	now = time(NULL);

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		gchar tmp[POPBUFSIZE];
		strretchomp(buf);
		recv_time = RECV_TIME_NONE;
		
		if (sscanf(buf, "%s\t%ld\t%s", uidl, (long int *) &recv_time, 
			   tmp) < 2) {
			if (sscanf(buf, "%s", uidl) != 1)
				continue;
			else {
				recv_time = now;
			}
		}
		if (!strcmp(uidl, msginfo->extradata->partial_recv)) {
			fclose(fp);
			return TRUE;
		}
	}

	fclose(fp);	
	return FALSE;
}

static int partial_uidl_mark_mail(MsgInfo *msginfo, int download)
{
	gchar *path;
	gchar *pathnew;
	FILE *fp;
	FILE *fpnew;
	gchar buf[POPBUFSIZE];
	gchar uidl[POPBUFSIZE];
	time_t recv_time;
	time_t now;
	gchar partial_recv[POPBUFSIZE];
	int err = -1;
	gchar *filename;
	MsgInfo *tinfo;
	gchar *sanitized_uid = NULL;	

	filename = procmsg_get_message_file_path(msginfo);
	if (!filename) {
		g_warning("can't get message file path.\n");
		return err;
	}
	tinfo = procheader_parse_file(filename, msginfo->flags, TRUE, TRUE);
	
	if (!tinfo->extradata) {
		g_free(filename);
		return err;
	}

	sanitized_uid = g_strdup(tinfo->extradata->account_login);
	subst_for_filename(sanitized_uid);

	if (!tinfo->extradata->account_server
	||  !tinfo->extradata->account_login
	||  !tinfo->extradata->partial_recv) {
		goto bail;
	}
	path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			   "uidl", G_DIR_SEPARATOR_S, tinfo->extradata->account_server,
			   "-", sanitized_uid, NULL);

	if ((fp = g_fopen(path, "rb")) == NULL) {
		perror("fopen1");
		if (ENOENT != errno) FILE_OP_ERROR(path, "fopen");
		g_free(path);
		path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
				   "uidl-", tinfo->extradata->account_server,
				   "-", tinfo->extradata->account_login, NULL);
		if ((fp = g_fopen(path, "rb")) == NULL) {
			if (ENOENT != errno) FILE_OP_ERROR(path, "fopen");
			g_free(path);
			goto bail;
		}
	}

	pathnew = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			   "uidl", G_DIR_SEPARATOR_S, tinfo->extradata->account_server,
			   "-", sanitized_uid, ".new", NULL);
	
	g_free(sanitized_uid);

	if ((fpnew = g_fopen(pathnew, "wb")) == NULL) {
		perror("fopen2");
		fclose(fp);
		g_free(pathnew);
		goto bail;
	}
	
	now = time(NULL);

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		strretchomp(buf);
		recv_time = RECV_TIME_NONE;
		sprintf(partial_recv,"0");
		
		if (sscanf(buf, "%s\t%ld\t%s", 
			   uidl, (long int *) &recv_time, partial_recv) < 2) {
			if (sscanf(buf, "%s", uidl) != 1)
				continue;
			else {
				recv_time = now;
			}
		}
		if (strcmp(tinfo->extradata->partial_recv, uidl)) {
			if (fprintf(fpnew, "%s\t%ld\t%s\n", 
				uidl, (long int) recv_time, partial_recv) < 0) {
				FILE_OP_ERROR(pathnew, "fprintf");
				fclose(fpnew);
				fclose(fp);
				g_free(path);
				g_free(pathnew);
				goto bail;
			}
		} else {
			gchar *stat = NULL;
			if (download == POP3_PARTIAL_DLOAD_DLOAD) {
				gchar *folder_id = folder_item_get_identifier(
							msginfo->folder);
				stat = g_strdup_printf("%s:%d",
					folder_id, msginfo->msgnum);
				g_free(folder_id);
			}
			else if (download == POP3_PARTIAL_DLOAD_UNKN)
				stat = g_strdup("1");
			else if (download == POP3_PARTIAL_DLOAD_DELE)
				stat = g_strdup("0");
			
			if (fprintf(fpnew, "%s\t%ld\t%s\n", 
				uidl, (long int) recv_time, stat) < 0) {
				FILE_OP_ERROR(pathnew, "fprintf");
				fclose(fpnew);
				fclose(fp);
				g_free(path);
				g_free(pathnew);
				goto bail;
			}
			g_free(stat);
		}
	}
	if (fclose(fpnew) == EOF) {
		FILE_OP_ERROR(pathnew, "fclose");
		fclose(fp);
		g_free(path);
		g_free(pathnew);
		goto bail;
	}
	fclose(fp);

	move_file(pathnew, path, TRUE);

	g_free(path);
	g_free(pathnew);
	
	if ((fp = g_fopen(filename,"rb")) == NULL) {
		perror("fopen3");
		goto bail;
	}
	pathnew = g_strdup_printf("%s.new", filename);
	if ((fpnew = g_fopen(pathnew, "wb")) == NULL) {
		perror("fopen4");
		fclose(fp);
		g_free(pathnew);
		goto bail;
	}
	
	if (fprintf(fpnew, "SC-Marked-For-Download: %d\n", 
			download) < 0) {
		FILE_OP_ERROR(pathnew, "fprintf");
		fclose(fpnew);
		fclose(fp);
		g_free(pathnew);
		goto bail;
	}
	while (fgets(buf, sizeof(buf)-1, fp) != NULL) {
		if(strlen(buf) > strlen("SC-Marked-For-Download: x\n")
		&& !strncmp(buf, "SC-Marked-For-Download:", 
		            strlen("SC-Marked-For-Download:"))) {
			if (fprintf(fpnew, "%s", 
			 buf+strlen("SC-Marked-For-Download: x\n")) < 0) {
				FILE_OP_ERROR(pathnew, "fprintf");
				fclose(fpnew);
				fclose(fp);
				g_free(pathnew);
				goto bail;
			}
			continue;
		} else if (strlen(buf) == strlen("SC-Marked-For-Download: x\n")
		&& !strncmp(buf, "SC-Marked-For-Download:", 
		            strlen("SC-Marked-For-Download:"))) {
			continue;
		}
		if (fprintf(fpnew, "%s", buf) < 0) {
			FILE_OP_ERROR(pathnew, "fprintf");
			fclose(fpnew);
			fclose(fp);
			g_free(pathnew);
			goto bail;
		}
	}
	if (fclose(fpnew) == EOF) {
		FILE_OP_ERROR(pathnew, "fclose");
		fclose(fp);
		g_free(pathnew);
		goto bail;
	}

	fclose(fp);
	claws_unlink(filename);
	g_rename(pathnew, filename);
	g_free(pathnew);
	msginfo->planned_download = download;
	msgcache_update_msg(msginfo->folder->cache, msginfo);

	err = 0;
bail:
	g_free(filename);
	procmsg_msginfo_free(tinfo);
	
	return err;
}
 
int partial_mark_for_delete(MsgInfo *msginfo)
{
	return partial_uidl_mark_mail(msginfo, POP3_PARTIAL_DLOAD_DELE);
}

int partial_mark_for_download(MsgInfo *msginfo)
{
	return partial_uidl_mark_mail(msginfo, POP3_PARTIAL_DLOAD_DLOAD);
}

int partial_unmark(MsgInfo *msginfo)
{
	return partial_uidl_mark_mail(msginfo, POP3_PARTIAL_DLOAD_UNKN);
}

void partial_delete_old(const gchar *file) 
{
	gchar *id = g_strdup(file);
	gchar *snum = strrchr(file, ':');
	int num = 0;
	FolderItem *item = NULL;

	debug_print("too big message updated, should remove %s\n", file?file:"(null)");

	if (snum) {
		snum++;
	} else {
		g_free(id);
		return; /* not a real problem */
	}

	num = atoi(snum);

	if (strrchr(id, ':'))
		*(strrchr(id, ':'))='\0';

	item = folder_find_item_from_identifier(id);
	if (item) {
		debug_print("removing %d in %s\n", num, id);
		folder_item_remove_msg(item, num);
	} 
	g_free(id);
}

gchar *partial_get_filename(const gchar *server, const gchar *login,
				   const gchar *muidl)
{
	gchar *path;
	gchar *result = NULL;
	FILE *fp;
	gchar buf[POPBUFSIZE];
	gchar uidl[POPBUFSIZE];
	time_t recv_time;
	time_t now;
	gchar *sanitized_uid = g_strdup(login);	

	subst_for_filename(sanitized_uid);

	path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			   "uidl", G_DIR_SEPARATOR_S, 
			   server, "-", sanitized_uid, NULL);
	if ((fp = g_fopen(path, "rb")) == NULL) {
		if (ENOENT != errno) FILE_OP_ERROR(path, "fopen");
		g_free(path);
		path = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
				   "uidl-", server,
				   "-", sanitized_uid, NULL);
		if ((fp = g_fopen(path, "rb")) == NULL) {
			if (ENOENT != errno) FILE_OP_ERROR(path, "fopen");
			g_free(sanitized_uid);
			g_free(path);
			return result;
		}
	}
	g_free(sanitized_uid);
	g_free(path);

	now = time(NULL);

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		gchar tmp[POPBUFSIZE];
		strretchomp(buf);
		recv_time = RECV_TIME_NONE;
		
		if (sscanf(buf, "%s\t%ld\t%s", uidl, (long int *) &recv_time, 
			   tmp) < 2) {
			if (sscanf(buf, "%s", uidl) != 1)
				continue;
			else {
				recv_time = now;
			}
		}
		if (!strcmp(muidl, uidl)) {
			result = g_strdup(tmp);
			break;
		}
	}

	fclose(fp);
	
	return result;
}

