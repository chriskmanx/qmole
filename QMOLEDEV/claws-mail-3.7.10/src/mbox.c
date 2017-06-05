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


#define _GNU_SOURCE
#include <stdio.h>

#include "defs.h"
#include <glib.h>
#include <glib/gi18n.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/file.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>

#ifdef G_OS_WIN32
#  include <w32lib.h>
#endif

#include "mbox.h"
#include "procmsg.h"
#include "folder.h"
#include "prefs_common.h"
#include "prefs_account.h"
#include "account.h"
#include "utils.h"
#include "filtering.h"
#include "alertpanel.h"
#include "statusbar.h"

#define MSGBUFSIZE	8192

#ifdef HAVE_FGETS_UNLOCKED
#define SC_FGETS fgets_unlocked
#define SC_FPUTS fputs_unlocked
#define SC_FPUTC fputc_unlocked
#else
#define SC_FGETS fgets
#define SC_FPUTS fputs
#define SC_FPUTC fputc
#endif

#define FPUTS_TO_TMP_ABORT_IF_FAIL(s) \
{ \
	lines++; \
	if (fputs(s, tmp_fp) == EOF) { \
		g_warning("can't write to temporary file\n"); \
		fclose(tmp_fp); \
		fclose(mbox_fp); \
		claws_unlink(tmp_file); \
		g_free(tmp_file); \
		return -1; \
	} \
}

gint proc_mbox(FolderItem *dest, const gchar *mbox, gboolean apply_filter,
	       PrefsAccount *account)
/* return values: -1 error, >=0 number of msgs added */
{
	FILE *mbox_fp;
	gchar buf[MSGBUFSIZE];
	gchar *tmp_file;
	gint msgs = 0;
	gint lines;
	MsgInfo *msginfo;
	gboolean more;
	GSList *to_filter = NULL, *filtered = NULL, *unfiltered = NULL, *cur, *to_add = NULL;
	gboolean printed = FALSE;
	FolderItem *dropfolder;

	cm_return_val_if_fail(dest != NULL, -1);
	cm_return_val_if_fail(mbox != NULL, -1);

	debug_print("Getting messages from %s into %s...\n", mbox, dest->path);

	if ((mbox_fp = g_fopen(mbox, "rb")) == NULL) {
		FILE_OP_ERROR(mbox, "fopen");
		alertpanel_error(_("Could not open mbox file:\n%s\n"), mbox);
		return -1;
	}

	/* ignore empty lines on the head */
	do {
		if (fgets(buf, sizeof(buf), mbox_fp) == NULL) {
			g_warning("can't read mbox file.\n");
			fclose(mbox_fp);
			return -1;
		}
	} while (buf[0] == '\n' || buf[0] == '\r');

	if (strncmp(buf, "From ", 5) != 0) {
		g_warning("invalid mbox format: %s\n", mbox);
		fclose(mbox_fp);
		return -1;
	}

	tmp_file = get_tmp_file();

	folder_item_update_freeze();

	if (apply_filter)
		dropfolder = folder_get_default_processing();
	else
		dropfolder = dest;
	
	do {
		FILE *tmp_fp;
		gint empty_lines;
		gint msgnum;
		
		if (msgs > 0 && msgs%500 == 0) {
			if (printed)
				statusbar_pop_all();
			statusbar_print_all(_("Importing from mbox... (%d mails imported)"), msgs);
			printed=TRUE;
			GTK_EVENTS_FLUSH();
		}
	
		if ((tmp_fp = g_fopen(tmp_file, "wb")) == NULL) {
			FILE_OP_ERROR(tmp_file, "fopen");
			g_warning("can't open temporary file\n");
			fclose(mbox_fp);
			g_free(tmp_file);
			return -1;
		}
		if (change_file_mode_rw(tmp_fp, tmp_file) < 0) {
			FILE_OP_ERROR(tmp_file, "chmod");
		}

		empty_lines = 0;
		lines = 0;
		more = FALSE;

		/* process all lines from mboxrc file */
		while (fgets(buf, sizeof(buf), mbox_fp) != NULL) {
			int offset;

			/* eof not reached, expect more lines */
			more = TRUE;

			/* eat empty lines */
			if (buf[0] == '\n' || buf[0] == '\r') {
				empty_lines++;
				continue;
			}

			/* From separator or quoted From */
			offset = 0;
			/* detect leading '>' char(s) */
			while ((buf[offset] == '>')) {
				offset++;
			}
			if (!strncmp(buf+offset, "From ", 5)) {
				/* From separator: */
				if (offset == 0) {
					/* expect next mbox item */
					break;
				}

				/* quoted From: */
				/* flush any eaten empty line */
				if (empty_lines > 0) {
					while (empty_lines-- > 0) {
						FPUTS_TO_TMP_ABORT_IF_FAIL("\n");
				}
					empty_lines = 0;
				}
				/* store the unquoted line */
				FPUTS_TO_TMP_ABORT_IF_FAIL(buf + 1);
				continue;
			}

			/* other line */
			/* flush any eaten empty line */
			if (empty_lines > 0) {			
				while (empty_lines-- > 0) {
					FPUTS_TO_TMP_ABORT_IF_FAIL("\n");
			}
				empty_lines = 0;
			}
			/* store the line itself */
					FPUTS_TO_TMP_ABORT_IF_FAIL(buf);
		}
		/* end of mbox item or end of mbox */

		/* flush any eaten empty line (but the last one) */
		if (empty_lines > 0) {
			while (--empty_lines > 0) {
				FPUTS_TO_TMP_ABORT_IF_FAIL("\n");
			}
		}

		/* more emails to expect? */
		more = !feof(mbox_fp);

		/* warn if email part is empty (it's the minimum check 
		   we can do */
		if (lines == 0) {
			g_warning("malformed mbox: %s: message %d is empty\n", mbox, msgs);
			fclose(tmp_fp);
			fclose(mbox_fp);
			claws_unlink(tmp_file);
			return -1;
		}

		if (fclose(tmp_fp) == EOF) {
			FILE_OP_ERROR(tmp_file, "fclose");
			g_warning("can't write to temporary file\n");
			fclose(mbox_fp);
			claws_unlink(tmp_file);
			g_free(tmp_file);
			return -1;
		}

		if (apply_filter) {
			if ((msgnum = folder_item_add_msg(dropfolder, tmp_file, NULL, TRUE)) < 0) {
				fclose(mbox_fp);
				claws_unlink(tmp_file);
				g_free(tmp_file);
				return -1;
			}
			msginfo = folder_item_get_msginfo(dropfolder, msgnum);
			to_filter = g_slist_prepend(to_filter, msginfo);
		} else {
			MsgFileInfo *finfo = g_new0(MsgFileInfo, 1);
			finfo->file = tmp_file;
			
			to_add = g_slist_prepend(to_add, finfo);
			tmp_file = get_tmp_file();
			
			/* flush every 500 */
			if (msgs > 0 && msgs % 500 == 0) {
				folder_item_add_msgs(dropfolder, to_add, TRUE);
				procmsg_message_file_list_free(to_add);
				to_add = NULL;
			}
		}
		msgs++;
	} while (more);

	if (printed)
		statusbar_pop_all();

	if (apply_filter) {

		folder_item_set_batch(dropfolder, FALSE);
		procmsg_msglist_filter(to_filter, account, 
				&filtered, &unfiltered, TRUE);
		folder_item_set_batch(dropfolder, TRUE);

		filtering_move_and_copy_msgs(to_filter);
		for (cur = filtered; cur; cur = g_slist_next(cur)) {
			MsgInfo *info = (MsgInfo *)cur->data;
			procmsg_msginfo_free(info);
		}

		unfiltered = g_slist_reverse(unfiltered);
		if (unfiltered) {
			folder_item_move_msgs(dest, unfiltered);
			for (cur = unfiltered; cur; cur = g_slist_next(cur)) {
				MsgInfo *info = (MsgInfo *)cur->data;
				procmsg_msginfo_free(info);
			}
		}

		g_slist_free(unfiltered);
		g_slist_free(filtered);
		g_slist_free(to_filter);
	} else if (to_add) {
		folder_item_add_msgs(dropfolder, to_add, TRUE);
		procmsg_message_file_list_free(to_add);
		to_add = NULL;
	}

	folder_item_update_thaw();
	
	g_free(tmp_file);
	fclose(mbox_fp);
	debug_print("%d messages found.\n", msgs);

	return msgs;
}

gint lock_mbox(const gchar *base, LockType type)
{
#ifdef G_OS_UNIX
	gint retval = 0;

	if (type == LOCK_FILE) {
		gchar *lockfile, *locklink;
		gint retry = 0;
		FILE *lockfp;

		lockfile = g_strdup_printf("%s.%d", base, getpid());
		if ((lockfp = g_fopen(lockfile, "wb")) == NULL) {
			FILE_OP_ERROR(lockfile, "fopen");
			g_warning("can't create lock file %s\n", lockfile);
			g_warning("use 'flock' instead of 'file' if possible.\n");
			g_free(lockfile);
			return -1;
		}

		if (fprintf(lockfp, "%d\n", getpid()) < 0) {
			FILE_OP_ERROR(lockfile, "fprintf");
			g_free(lockfile);
			fclose(lockfp);
			return -1;
		}

		if (fclose(lockfp) == EOF) {
			FILE_OP_ERROR(lockfile, "fclose");
			g_free(lockfile);
			return -1;
		}

		locklink = g_strconcat(base, ".lock", NULL);
		while (link(lockfile, locklink) < 0) {
			FILE_OP_ERROR(lockfile, "link");
			if (retry >= 5) {
				g_warning("can't create %s\n", lockfile);
				claws_unlink(lockfile);
				g_free(lockfile);
				return -1;
			}
			if (retry == 0)
				g_warning("mailbox is owned by another"
					    " process, waiting...\n");
			retry++;
			sleep(5);
		}
		claws_unlink(lockfile);
		g_free(lockfile);
	} else if (type == LOCK_FLOCK) {
		gint lockfd;
		gboolean fcntled = FALSE;
#if HAVE_FCNTL_H && !defined(G_OS_WIN32)
		struct flock fl;
		fl.l_type = F_WRLCK;
		fl.l_whence = SEEK_SET;
		fl.l_start = 0;
		fl.l_len = 0;
#endif

#if HAVE_FLOCK
		if ((lockfd = g_open(base, O_RDWR, 0)) < 0) {
#else
		if ((lockfd = g_open(base, O_RDWR, 0)) < 0) {
#endif
			FILE_OP_ERROR(base, "open");
			return -1;
		}
		
#if HAVE_FCNTL_H && !defined(G_OS_WIN32)
		if (fcntl(lockfd, F_SETLK, &fl) == -1) {
			g_warning("can't fnctl %s (%s)", base, strerror(errno));
			return -1;
		} else {
			fcntled = TRUE;
		}
#endif

#if HAVE_FLOCK
		if (flock(lockfd, LOCK_EX|LOCK_NB) < 0 && !fcntled) {
			perror("flock");
#else
#if HAVE_LOCKF
		if (lockf(lockfd, F_TLOCK, 0) < 0 && !fcntled) {
			perror("lockf");
#else
		{
#endif
#endif /* HAVE_FLOCK */
			g_warning("can't lock %s\n", base);
			if (close(lockfd) < 0)
				perror("close");
			return -1;
		}
		retval = lockfd;
	} else {
		g_warning("invalid lock type\n");
		return -1;
	}

	return retval;
#else
	return -1;
#endif /* G_OS_UNIX */
}

gint unlock_mbox(const gchar *base, gint fd, LockType type)
{
	if (type == LOCK_FILE) {
		gchar *lockfile;

		lockfile = g_strconcat(base, ".lock", NULL);
		if (claws_unlink(lockfile) < 0) {
			FILE_OP_ERROR(lockfile, "unlink");
			g_free(lockfile);
			return -1;
		}
		g_free(lockfile);

		return 0;
	} else if (type == LOCK_FLOCK) {
		gboolean fcntled = FALSE;
#if HAVE_FCNTL_H && !defined(G_OS_WIN32)
		struct flock fl;
		fl.l_type = F_UNLCK;
		fl.l_whence = SEEK_SET;
		fl.l_start = 0;
		fl.l_len = 0;

		if (fcntl(fd, F_SETLK, &fl) == -1) {
			g_warning("can't fnctl %s", base);
		} else {
			fcntled = TRUE;
		}
#endif
#if HAVE_FLOCK
		if (flock(fd, LOCK_UN) < 0 && !fcntled) {
			perror("flock");
#else
#if HAVE_LOCKF
		if (lockf(fd, F_ULOCK, 0) < 0 && !fcntled) {
			perror("lockf");
#else
		{
#endif
#endif /* HAVE_FLOCK */
			g_warning("can't unlock %s\n", base);
			if (close(fd) < 0)
				perror("close");
			return -1;
		}

		if (close(fd) < 0) {
			perror("close");
			return -1;
		}

		return 0;
	}

	g_warning("invalid lock type\n");
	return -1;
}

gint copy_mbox(gint srcfd, const gchar *dest)
{
	FILE *dest_fp;
	ssize_t n_read;
	gchar buf[BUFSIZ];
	gboolean err = FALSE;
	int save_errno = 0;

	if (srcfd < 0) {
		return -1;
	}

	if ((dest_fp = g_fopen(dest, "wb")) == NULL) {
		FILE_OP_ERROR(dest, "fopen");
		return -1;
	}

	if (change_file_mode_rw(dest_fp, dest) < 0) {
		FILE_OP_ERROR(dest, "chmod");
		g_warning("can't change file mode\n");
	}

	while ((n_read = read(srcfd, buf, sizeof(buf))) > 0) {
		if (n_read == -1 && errno != 0) {
			save_errno = errno;
			break;
		}
		if (fwrite(buf, 1, n_read, dest_fp) < n_read) {
			g_warning("writing to %s failed.\n", dest);
			fclose(dest_fp);
			claws_unlink(dest);
			return -1;
		}
	}

	if (save_errno != 0) {
		g_warning("error %d reading mbox: %s\n", save_errno,
				strerror(save_errno));
		err = TRUE;
	}

	if (fclose(dest_fp) == EOF) {
		FILE_OP_ERROR(dest, "fclose");
		err = TRUE;
	}

	if (err) {
		claws_unlink(dest);
		return -1;
	}

	return 0;
}

void empty_mbox(const gchar *mbox)
{
	FILE *fp;

	if ((fp = g_fopen(mbox, "wb")) == NULL) {
		FILE_OP_ERROR(mbox, "fopen");
		g_warning("can't truncate mailbox to zero.\n");
		return;
	}
	fclose(fp);
}

gint export_list_to_mbox(GSList *mlist, const gchar *mbox)
/* return values: -2 skipped, -1 error, 0 OK */
{
	GSList *cur;
	MsgInfo *msginfo;
	FILE *msg_fp;
	FILE *mbox_fp;
	gchar buf[BUFFSIZE];
	int err = 0;

	gint msgs = 1, total = g_slist_length(mlist);
	if (g_file_test(mbox, G_FILE_TEST_EXISTS) == TRUE) {
		if (alertpanel_full(_("Overwrite mbox file"),
					_("This file already exists. Do you want to overwrite it?"),
					GTK_STOCK_CANCEL, _("Overwrite"), NULL, FALSE,
					NULL, ALERT_WARNING, G_ALERTDEFAULT)
				!= G_ALERTALTERNATE) {
			return -2;
		}
	}

	if ((mbox_fp = g_fopen(mbox, "wb")) == NULL) {
		FILE_OP_ERROR(mbox, "fopen");
		alertpanel_error(_("Could not create mbox file:\n%s\n"), mbox);
		return -1;
	}

#ifdef HAVE_FGETS_UNLOCKED
	flockfile(mbox_fp);
#endif

	statuswindow_print_all(_("Exporting to mbox..."));
	for (cur = mlist; cur != NULL; cur = cur->next) {
		int len;
		gchar buft[BUFFSIZE];
		msginfo = (MsgInfo *)cur->data;

		msg_fp = procmsg_open_message(msginfo);
		if (!msg_fp) {
			continue;
		}

#ifdef HAVE_FGETS_UNLOCKED
		flockfile(msg_fp);
#endif
		strncpy2(buf,
			 msginfo->from ? msginfo->from :
			 cur_account && cur_account->address ?
			 cur_account->address : "unknown",
			 sizeof(buf));
		extract_address(buf);

		if (fprintf(mbox_fp, "From %s %s",
			buf, ctime_r(&msginfo->date_t, buft)) < 0) {
			err = -1;
#ifdef HAVE_FGETS_UNLOCKED
			funlockfile(msg_fp);
#endif
			fclose(msg_fp);
			goto out;
		}

		buf[0] = '\0';
		
		/* write email to mboxrc */
		while (SC_FGETS(buf, sizeof(buf), msg_fp) != NULL) {
			/* quote any From, >From, >>From, etc., according to mbox format specs */
			int offset;

			offset = 0;
			/* detect leading '>' char(s) */
			while ((buf[offset] == '>')) {
				offset++;
			}
			if (!strncmp(buf+offset, "From ", 5)) {
				if (SC_FPUTC('>', mbox_fp) == EOF) {
					err = -1;
#ifdef HAVE_FGETS_UNLOCKED
					funlockfile(msg_fp);
#endif
					fclose(msg_fp);
					goto out;
				}
			}
			if (SC_FPUTS(buf, mbox_fp) == EOF) {
				err = -1;
#ifdef HAVE_FGETS_UNLOCKED
				funlockfile(msg_fp);
#endif
				fclose(msg_fp);
				goto out;
			}
		}

		/* force last line to end w/ a newline */
		len = strlen(buf);
		if (len > 0) {
			len--;
			if ((buf[len] != '\n') && (buf[len] != '\r')) {
				if (SC_FPUTC('\n', mbox_fp) == EOF) {
					err = -1;
#ifdef HAVE_FGETS_UNLOCKED
					funlockfile(msg_fp);
#endif
					fclose(msg_fp);
					goto out;
				}
			}
		}

		/* add a trailing empty line */
		if (SC_FPUTC('\n', mbox_fp) == EOF) {
			err = -1;
#ifdef HAVE_FGETS_UNLOCKED
			funlockfile(msg_fp);
#endif
			fclose(msg_fp);
			goto out;
		}

#ifdef HAVE_FGETS_UNLOCKED
		funlockfile(msg_fp);
#endif
		fclose(msg_fp);
		statusbar_progress_all(msgs++,total, 500);
		if (msgs%500 == 0)
			GTK_EVENTS_FLUSH();
	}

out:
	statusbar_progress_all(0,0,0);
	statuswindow_pop_all();

#ifdef HAVE_FGETS_UNLOCKED
	funlockfile(mbox_fp);
#endif
	fclose(mbox_fp);

	return err;
}

/* read all messages in SRC, and store them into one MBOX file. */
/* return values: -2 skipped, -1 error, 0 OK */
gint export_to_mbox(FolderItem *src, const gchar *mbox)
{
	GSList *mlist;
	gint ret;
	
	cm_return_val_if_fail(src != NULL, -1);
	cm_return_val_if_fail(src->folder != NULL, -1);
	cm_return_val_if_fail(mbox != NULL, -1);

	debug_print("Exporting messages from %s into %s...\n",
		    src->path, mbox);

	mlist = folder_item_get_msg_list(src);

	folder_item_update_freeze();
	ret = export_list_to_mbox(mlist, mbox);
	folder_item_update_thaw();

	procmsg_msg_list_free(mlist);

	return ret;
}
