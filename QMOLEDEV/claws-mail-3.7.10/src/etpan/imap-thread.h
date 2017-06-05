/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2005-2011 DINH Viet Hoa and the Claws Mail team
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

#ifndef IMAP_THREAD_H

#define IMAP_THREAD_H

#include <libetpan/libetpan.h>
#include "folder.h"

#define IMAP_SET_MAX_COUNT 500

typedef enum
{
	IMAP_FLAG_SEEN		= 1 << 0,
	IMAP_FLAG_ANSWERED	= 1 << 1,
	IMAP_FLAG_FLAGGED	= 1 << 2,
	IMAP_FLAG_DELETED	= 1 << 3,
	IMAP_FLAG_DRAFT		= 1 << 4,
	IMAP_FLAG_FORWARDED	= 1 << 5,
	IMAP_FLAG_SPAM		= 1 << 6,
	IMAP_FLAG_HAM		= 1 << 7
} IMAPFlags;

void imap_main_set_timeout(int sec);
void imap_main_init(gboolean skip_ssl_cert_check);
void imap_main_done(gboolean have_connectivity);

void imap_init(Folder * folder);
void imap_done(Folder * folder);

int imap_threaded_connect(Folder * folder, const char * server, int port);
int imap_threaded_connect_ssl(Folder * folder, const char * server, int port);
struct mailimap_capability_data * imap_threaded_capability(Folder *folder, int *ok);

#ifndef G_OS_WIN32
int imap_threaded_connect_cmd(Folder * folder, const char * command,
			      const char * server, int port);
#endif
void imap_threaded_disconnect(Folder * folder);

int imap_threaded_list(Folder * folder, const char * base,
		       const char * wildcard,
		       clist ** p_result);
int imap_threaded_lsub(Folder * folder, const char * base,
		       const char * wildcard,
		       clist ** p_result);
int imap_threaded_login(Folder * folder,
			const char * login, const char * password,
			const char * type);
int imap_threaded_status(Folder * folder, const char * mb,
		struct mailimap_mailbox_data_status ** data_status,
		guint mask);
int imap_threaded_close(Folder * folder);

int imap_threaded_noop(Folder * folder, unsigned int * p_exists, 
		       unsigned int *p_recent, 
		       unsigned int *p_expunge,
		       unsigned int *p_unseen,
		       unsigned int *p_uidnext,
		       unsigned int *p_uidval);
int imap_threaded_starttls(Folder * folder, const gchar *host, int port);
int imap_threaded_create(Folder * folder, const char * mb);
int imap_threaded_rename(Folder * folder,
			 const char * mb, const char * new_name);
int imap_threaded_delete(Folder * folder, const char * mb);
int imap_threaded_select(Folder * folder, const char * mb,
			 gint * exists, gint * recent, gint * unseen,
			 guint32 * uid_validity, gint * can_create_flags,
			 GSList **ok_flags);
int imap_threaded_examine(Folder * folder, const char * mb,
			  gint * exists, gint * recent, gint * unseen,
			  guint32 * uid_validity);
int imap_threaded_subscribe(Folder * folder, const char * mb,
		       gboolean subscribe);

enum {
	IMAP_SEARCH_TYPE_SIMPLE,
	IMAP_SEARCH_TYPE_SEEN,
	IMAP_SEARCH_TYPE_UNSEEN,
	IMAP_SEARCH_TYPE_ANSWERED,
	IMAP_SEARCH_TYPE_FLAGGED,
	IMAP_SEARCH_TYPE_DELETED,
	IMAP_SEARCH_TYPE_FORWARDED,
	IMAP_SEARCH_TYPE_SPAM,
};

int imap_threaded_search(Folder * folder, int search_type,
			 struct mailimap_set * set, clist ** result);

int imap_threaded_fetch_uid(Folder * folder, uint32_t first_index,
			    carray ** result);

void imap_fetch_uid_list_free(carray * uid_list);

int imap_threaded_fetch_uid_flags(Folder * folder, uint32_t first_index,
				  carray ** fetch_result);

void imap_fetch_uid_flags_list_free(carray * uid_flags_list);

int imap_threaded_fetch_content(Folder * folder, uint32_t msg_index,
				int with_body,
				const char * filename);

struct imap_fetch_env_info {
	uint32_t uid;
	char * headers;
	uint32_t size;
	int flags;
};

int imap_threaded_fetch_env(Folder * folder, struct mailimap_set * set,
			    carray ** p_env_list);

void imap_fetch_env_free(carray * env_list);

int imap_threaded_append(Folder * folder, const char * mailbox,
			 const char * filename,
			 struct mailimap_flag_list * flag_list,
			 int *uid);

int imap_threaded_expunge(Folder * folder);

int imap_threaded_copy(Folder * folder, struct mailimap_set * set,
		       const char * mb, struct mailimap_set **source,
		       struct mailimap_set **dest);

int imap_threaded_store(Folder * folder, struct mailimap_set * set,
			struct mailimap_store_att_flags * store_att_flags);

void imap_threaded_cancel(Folder * folder);

#endif
