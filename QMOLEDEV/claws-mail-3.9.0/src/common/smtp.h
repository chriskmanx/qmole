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

#ifndef __SMTP_H__
#define __SMTP_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#include <glib.h>

#include "session.h"

typedef struct _SMTPSession	SMTPSession;

#define SMTP_SESSION(obj)	((SMTPSession *)obj)

#define MESSAGEBUFSIZE		8192

typedef enum
{
	SM_OK			= 0,
	SM_ERROR		= 128,
	SM_UNRECOVERABLE	= 129,
	SM_AUTHFAIL		= 130
} SMTPErrorValue;

typedef enum
{
	ESMTP_8BITMIME	= 1 << 0,
	ESMTP_SIZE	= 1 << 1,
	ESMTP_ETRN	= 1 << 2
} ESMTPFlag;

typedef enum
{
	SMTPAUTH_LOGIN      = 1 << 0,
	SMTPAUTH_CRAM_MD5   = 1 << 1,
	SMTPAUTH_DIGEST_MD5 = 1 << 2,
	SMTPAUTH_TLS_AVAILABLE = 1 << 3,
	SMTPAUTH_PLAIN      = 1 << 4
} SMTPAuthType;

typedef enum
{
	SMTP_READY,
	SMTP_CONNECTED,
	SMTP_HELO,
	SMTP_EHLO,
	SMTP_STARTTLS,
	SMTP_FROM,
	SMTP_AUTH,
	SMTP_AUTH_LOGIN_USER,
	SMTP_AUTH_LOGIN_PASS,
	SMTP_AUTH_CRAM_MD5,
	SMTP_AUTH_PLAIN,
	SMTP_RCPT,
	SMTP_DATA,
	SMTP_SEND_DATA,
	SMTP_EOM,
	SMTP_RSET,
	SMTP_QUIT,
	SMTP_ERROR,
	SMTP_DISCONNECTED,
	SMTP_MAIL_SENT_OK,

	N_SMTP_PHASE
} SMTPState;

struct _SMTPSession
{
	Session session;

	SMTPState state;

	gchar *hostname;

	gchar *user;
	gchar *pass;

	gchar *from;
	GSList *to_list;
	GSList *cur_to;

	guchar *send_data;
	guint send_data_len;

	gint max_message_size;

	SMTPAuthType avail_auth_type;
	SMTPAuthType forced_auth_type;
	SMTPAuthType auth_type;

	SMTPErrorValue error_val;
	gchar *error_msg;
	gboolean is_esmtp;
	ESMTPFlag esmtp_flags;
	
	void *dialog;

#ifdef USE_GNUTLS
	gboolean tls_init_done;
#endif
};

Session *smtp_session_new	(void *prefs_account);
gint smtp_from(SMTPSession *session);
gint smtp_quit(SMTPSession *session);

#endif /* __SMTP_H__ */
