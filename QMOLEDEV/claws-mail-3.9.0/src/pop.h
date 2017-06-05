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

#ifndef __POP_H__
#define __POP_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#include <glib.h>
#include <time.h>

#include "session.h"
#include "prefs_account.h"

typedef struct _Pop3MsgInfo	Pop3MsgInfo;
typedef struct _Pop3Session	Pop3Session;

#define POP3_SESSION(obj)	((Pop3Session *)obj)

#define MAIL_RECEIVE_HOOKLIST	"mail_receive_hooklist"
struct _MailReceiveData
{
	Pop3Session *session;
	char *data;
	guint data_len;
};
typedef struct _MailReceiveData	MailReceiveData;

typedef enum {
	POP3_READY,
	POP3_GREETING,
#ifdef USE_GNUTLS
	POP3_STLS,
#endif
	POP3_GETAUTH_USER,
	POP3_GETAUTH_PASS,
	POP3_GETAUTH_APOP,
	POP3_GETRANGE_STAT,
	POP3_GETRANGE_LAST,
	POP3_GETRANGE_UIDL,
	POP3_GETRANGE_UIDL_RECV,
	POP3_GETSIZE_LIST,
	POP3_GETSIZE_LIST_RECV,
	POP3_RETR,
	POP3_RETR_RECV,
	POP3_TOP,
	POP3_TOP_RECV,
	POP3_DELETE,
	POP3_LOGOUT,
	POP3_DONE,
	POP3_ERROR,

	N_POP3_STATE
} Pop3State;

typedef enum {
	PS_SUCCESS	= 0,	/* command successful */
	PS_NOMAIL	= 1,	/* no mail available */
	PS_SOCKET	= 2,	/* socket I/O woes */
	PS_AUTHFAIL	= 3,	/* user authorization failed */
	PS_PROTOCOL	= 4,	/* protocol violation */
	PS_SYNTAX	= 5,	/* command-line syntax error */
	PS_IOERR	= 6,	/* file I/O error */
	PS_ERROR	= 7,	/* protocol error */
	PS_EXCLUDE	= 8,	/* client-side exclusion error */
	PS_LOCKBUSY	= 9,	/* server responded lock busy */
	PS_SMTP		= 10,	/* SMTP error */
	PS_DNS		= 11,	/* fatal DNS error */
	PS_BSMTP	= 12,	/* output batch could not be opened */
	PS_MAXFETCH	= 13,	/* poll ended by fetch limit */

	PS_NOTSUPPORTED	= 14,	/* command not supported */

	/* leave space for more codes */

	PS_CONTINUE	= 128	/* more responses may follow */
} Pop3ErrorValue;

typedef enum {
	RECV_TIME_NONE     = 0,
	RECV_TIME_RECEIVED = 1,
	RECV_TIME_KEEP     = 2
} RecvTime;

struct _Pop3MsgInfo
{
	gint size;
	gchar *uidl;
	time_t recv_time;
	guint received     : 1;
	guint deleted      : 1;
	guint partial_recv : 2;
};

struct _Pop3Session
{
	Session session;

	Pop3State state;
	gchar *prev_folder;

	PrefsAccount *ac_prefs;
	gboolean pop_before_smtp;

	gchar *greeting;
	gchar *user;
	gchar *pass;
	gint count;
	gint total_bytes;
	gint cur_msg;
	gint cur_total_num;
	gint cur_total_bytes;
	gint cur_total_recv_bytes;

	Pop3MsgInfo *msg;

	GHashTable *uidl_table;
	GHashTable *partial_recv_table;
	
	gboolean new_msg_exist;
	gboolean uidl_is_valid;

	time_t current_time;

	Pop3ErrorValue error_val;
	gchar *error_msg;

	gpointer data;

	/* virtual method to drop message */
	gint (*drop_message)	(Pop3Session	*session,
				 const gchar	*file);
};

#define POPBUFSIZE	512
/* #define IDLEN	128 */
#define IDLEN		POPBUFSIZE

Session *pop3_session_new	(PrefsAccount	*account);
gint pop3_write_uidl_list	(Pop3Session	*session);

#endif /* __POP_H__ */
