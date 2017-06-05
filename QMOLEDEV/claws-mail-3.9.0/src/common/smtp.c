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

#include <glib.h>
#ifdef ENABLE_NLS
#include <glib/gi18n.h>
#else
#define _(a) (a)
#define N_(a) (a)
#endif
#include <stdio.h>
#include <string.h>

#include "smtp.h"
#include "md5.h"
#include "base64.h"
#include "utils.h"
#include "log.h"

static void smtp_session_destroy(Session *session);

static gint smtp_auth(SMTPSession *session);
#ifdef USE_GNUTLS
static gint smtp_starttls(SMTPSession *session);
#endif
static gint smtp_auth_cram_md5(SMTPSession *session);
static gint smtp_auth_login(SMTPSession *session);
static gint smtp_auth_plain(SMTPSession *session);

static gint smtp_ehlo(SMTPSession *session);
static gint smtp_ehlo_recv(SMTPSession *session, const gchar *msg);

static gint smtp_helo(SMTPSession *session);
static gint smtp_rcpt(SMTPSession *session);
static gint smtp_data(SMTPSession *session);
static gint smtp_send_data(SMTPSession *session);
static gint smtp_make_ready(SMTPSession *session);
static gint smtp_eom(SMTPSession *session);

static gint smtp_session_recv_msg(Session *session, const gchar *msg);
static gint smtp_session_send_data_finished(Session *session, guint len);


Session *smtp_session_new(void *prefs_account)
{
	SMTPSession *session;

	session = g_new0(SMTPSession, 1);

	session_init(SESSION(session), prefs_account, TRUE);

	SESSION(session)->type             = SESSION_SMTP;

	SESSION(session)->recv_msg         = smtp_session_recv_msg;

	SESSION(session)->recv_data_finished = NULL;
	SESSION(session)->send_data_finished = smtp_session_send_data_finished;

	SESSION(session)->destroy          = smtp_session_destroy;

	session->state                     = SMTP_READY;

#ifdef USE_GNUTLS
	session->tls_init_done             = FALSE;
#endif

	session->hostname                  = NULL;
	session->user                      = NULL;
	session->pass                      = NULL;

	session->from                      = NULL;
	session->to_list                   = NULL;
	session->cur_to                    = NULL;

	session->send_data                 = NULL;
	session->send_data_len             = 0;

	session->max_message_size          = -1;

	session->avail_auth_type           = 0;
	session->forced_auth_type          = 0;
	session->auth_type                 = 0;
	session->esmtp_flags               = 0;

	session->error_val                 = SM_OK;
	session->error_msg                 = NULL;

	return SESSION(session);
}

static void smtp_session_destroy(Session *session)
{
	SMTPSession *smtp_session = SMTP_SESSION(session);

	g_free(smtp_session->hostname);
	g_free(smtp_session->user);
	g_free(smtp_session->pass);
	g_free(smtp_session->from);

	g_free(smtp_session->send_data);

	g_free(smtp_session->error_msg);
}

gint smtp_from(SMTPSession *session)
{
	gchar buf[MESSAGEBUFSIZE];
	gchar *mail_size = NULL;

	cm_return_val_if_fail(session->from != NULL, SM_ERROR);

	session->state = SMTP_FROM;
	
	if (session->is_esmtp && (session->esmtp_flags & ESMTP_SIZE)!=0)
		mail_size = g_strdup_printf(" SIZE=%d", session->send_data_len);
	else
		mail_size = g_strdup("");
		

	if (strchr(session->from, '<'))
		g_snprintf(buf, sizeof(buf), "MAIL FROM:%s%s", session->from,
			   mail_size);
	else
		g_snprintf(buf, sizeof(buf), "MAIL FROM:<%s>%s", session->from,
			   mail_size);

	g_free(mail_size);

	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, buf) < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "%sSMTP> %s\n", (session->is_esmtp?"E":""), buf);

	return SM_OK;
}

static gint smtp_auth(SMTPSession *session)
{

	cm_return_val_if_fail(session->user != NULL, SM_ERROR);

	session->state = SMTP_AUTH;

	if ((session->forced_auth_type == SMTPAUTH_CRAM_MD5
	     || session->forced_auth_type == 0)
            &&
	     (session->avail_auth_type & SMTPAUTH_CRAM_MD5) != 0)
		smtp_auth_cram_md5(session);
	else if ((session->forced_auth_type == SMTPAUTH_LOGIN
		  || session->forced_auth_type == 0)
                 &&
		  (session->avail_auth_type & SMTPAUTH_LOGIN) != 0)
		smtp_auth_login(session);
	else if ((session->forced_auth_type == SMTPAUTH_PLAIN
		  || session->forced_auth_type == 0)
                 &&
		  (session->avail_auth_type & SMTPAUTH_PLAIN) != 0)
		smtp_auth_plain(session);
	else if (session->forced_auth_type == 0) {
		log_warning(LOG_PROTOCOL, _("No SMTP AUTH method available\n"));
		return SM_AUTHFAIL;
	} else {
		log_warning(LOG_PROTOCOL, _("Selected SMTP AUTH method not available\n"));
		return SM_AUTHFAIL;
	}

	return SM_OK;
}

static gint smtp_auth_recv(SMTPSession *session, const gchar *msg)
{
	gchar buf[MESSAGEBUFSIZE];

	switch (session->auth_type) {
	case SMTPAUTH_LOGIN:
		session->state = SMTP_AUTH_LOGIN_USER;

		if (!strncmp(msg, "334 ", 4)) {
			base64_encode(buf, session->user, strlen(session->user));

			if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL,
					 buf) < 0)
				return SM_ERROR;
			log_print(LOG_PROTOCOL, "ESMTP> [USERID]\n");
		} else {
			/* Server rejects AUTH */
			if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL,
					 "*") < 0)
				return SM_ERROR;
			log_print(LOG_PROTOCOL, "ESMTP> *\n");
		}
		break;
	case SMTPAUTH_CRAM_MD5:
		session->state = SMTP_AUTH_CRAM_MD5;

		if (!strncmp(msg, "334 ", 4)) {
			gchar *response;
			gchar *response64;
			gchar *challenge;
			gint challengelen;
			guchar hexdigest[33];

			challenge = g_malloc(strlen(msg + 4) + 1);
			challengelen = base64_decode(challenge, msg + 4, -1);
			challenge[challengelen] = '\0';
			log_print(LOG_PROTOCOL, "ESMTP< [Decoded: %s]\n", challenge);

			g_snprintf(buf, sizeof(buf), "%s", session->pass);
			md5_hex_hmac(hexdigest, challenge, challengelen,
				     buf, strlen(session->pass));
			g_free(challenge);

			response = g_strdup_printf
				("%s %s", session->user, hexdigest);
			log_print(LOG_PROTOCOL, "ESMTP> [Encoded: %s]\n", response);

			response64 = g_malloc((strlen(response) + 3) * 2 + 1);
			base64_encode(response64, response, strlen(response));
			g_free(response);

			if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL,
					 response64) < 0)
				return SM_ERROR;
			log_print(LOG_PROTOCOL, "ESMTP> %s\n", response64);
			g_free(response64);
		} else {
			/* Server rejects AUTH */
			if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL,
					 "*") < 0)
				return SM_ERROR;
			log_print(LOG_PROTOCOL, "ESMTP> *\n");
		}
		break;
	case SMTPAUTH_DIGEST_MD5:
        default:
        	/* stop smtp_auth when no correct authtype */
		if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, "*") < 0)
			return SM_ERROR;
		log_print(LOG_PROTOCOL, "ESMTP> *\n");
		break;
	}

	return SM_OK;
}

static gint smtp_auth_login_user_recv(SMTPSession *session, const gchar *msg)
{
	gchar buf[MESSAGEBUFSIZE];

	session->state = SMTP_AUTH_LOGIN_PASS;

	if (!strncmp(msg, "334 ", 4))
		base64_encode(buf, session->pass, strlen(session->pass));
	else
		/* Server rejects AUTH */
		g_snprintf(buf, sizeof(buf), "*");

	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, buf) < 0)
		return SM_ERROR;

	log_print(LOG_PROTOCOL, "ESMTP> [PASSWORD]\n");

	return SM_OK;
}

static gint smtp_ehlo(SMTPSession *session)
{
	gchar buf[MESSAGEBUFSIZE];

	session->state = SMTP_EHLO;

	session->avail_auth_type = 0;

	g_snprintf(buf, sizeof(buf), "EHLO %s",
		   session->hostname ? session->hostname : get_domain_name());
	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, buf) < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "ESMTP> %s\n", buf);

	return SM_OK;
}

static gint smtp_ehlo_recv(SMTPSession *session, const gchar *msg)
{
	if (strncmp(msg, "250", 3) == 0) {
		const gchar *p = msg;
		p += 3;
		if (*p == '-' || *p == ' ') p++;
		if (g_ascii_strncasecmp(p, "AUTH", 4) == 0) {
			p += 5;
			if (strcasestr(p, "PLAIN"))
				session->avail_auth_type |= SMTPAUTH_PLAIN;
			if (strcasestr(p, "LOGIN"))
				session->avail_auth_type |= SMTPAUTH_LOGIN;
			if (strcasestr(p, "CRAM-MD5"))
				session->avail_auth_type |= SMTPAUTH_CRAM_MD5;
			if (strcasestr(p, "DIGEST-MD5"))
				session->avail_auth_type |= SMTPAUTH_DIGEST_MD5;
		}
		if (g_ascii_strncasecmp(p, "SIZE", 4) == 0) {
			p += 5;
			session->max_message_size = atoi(p);
			session->esmtp_flags |= ESMTP_SIZE;
		}
		if (g_ascii_strncasecmp(p, "STARTTLS", 8) == 0) {
			p += 9;
			session->avail_auth_type |= SMTPAUTH_TLS_AVAILABLE;
		}
		return SM_OK;
	} else if ((msg[0] == '1' || msg[0] == '2' || msg[0] == '3') &&
	    (msg[3] == ' ' || msg[3] == '\0'))
		return SM_OK;
	else if (msg[0] == '5' && msg[1] == '0' &&
		 (msg[2] == '4' || msg[2] == '3' || msg[2] == '1'))
		return SM_ERROR;

	return SM_ERROR;
}

#ifdef USE_GNUTLS
static gint smtp_starttls(SMTPSession *session)
{
	session->state = SMTP_STARTTLS;

	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, "STARTTLS") < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "ESMTP> STARTTLS\n");

	return SM_OK;
}
#endif

static gint smtp_auth_cram_md5(SMTPSession *session)
{
	session->state = SMTP_AUTH;
	session->auth_type = SMTPAUTH_CRAM_MD5;

	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, "AUTH CRAM-MD5") < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "ESMTP> AUTH CRAM-MD5\n");

	return SM_OK;
}

static gint smtp_auth_plain(SMTPSession *session)
{
	gchar buf[MESSAGEBUFSIZE];

	/* 
 	 * +1      +1      +1
	 * \0<user>\0<pass>\0 
	 */
	int b64len = (1 + strlen(session->user) + 1 + strlen(session->pass) + 1);
	gchar *b64buf = g_malloc(b64len);

	/* use the char *ptr to walk the base64 string with embedded \0 */
	char  *a = b64buf;
	int  b64cnt = 0;

	session->state = SMTP_AUTH_PLAIN;
	session->auth_type = SMTPAUTH_PLAIN;

	memset(buf, 0, sizeof buf);

	/*
	 * have to construct the string bit by bit. sprintf can't do it in one.
	 * first field is null, so string is \0<user>\0<password>
	 */
	*a = 0;
	a++;

	g_snprintf (a, b64len - 1, "%s", session->user);

	b64cnt = strlen(session->user)+1;
	a += b64cnt;

	g_snprintf (a, b64len - b64cnt - 1, "%s", session->pass);
	b64cnt += strlen(session->pass) + 1;	

	/*
	 * reuse the char *ptr to offset into the textbuf to meld
	 * the plaintext ESMTP message and the base64 string value
	 */
	strcpy(buf, "AUTH PLAIN ");
	a = buf + strlen(buf);
	base64_encode(a, b64buf, b64cnt);

	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, buf) < 0)
		return SM_ERROR;

	log_print(LOG_PROTOCOL, "ESMTP> [AUTH PLAIN]\n");

	g_free(b64buf);

	return SM_OK;
}

static gint smtp_auth_login(SMTPSession *session)
{
	session->state = SMTP_AUTH;
	session->auth_type = SMTPAUTH_LOGIN;

	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, "AUTH LOGIN") < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "ESMTP> AUTH LOGIN\n");

	return SM_OK;
}

static gint smtp_helo(SMTPSession *session)
{
	gchar buf[MESSAGEBUFSIZE];

	session->state = SMTP_HELO;

	g_snprintf(buf, sizeof(buf), "HELO %s",
		   session->hostname ? session->hostname : get_domain_name());
	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, buf) < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "SMTP> %s\n", buf);

	return SM_OK;
}

static gint smtp_rcpt(SMTPSession *session)
{
	gchar buf[MESSAGEBUFSIZE];
	gchar *to;

	cm_return_val_if_fail(session->cur_to != NULL, SM_ERROR);

	session->state = SMTP_RCPT;

	to = (gchar *)session->cur_to->data;

	if (strchr(to, '<'))
		g_snprintf(buf, sizeof(buf), "RCPT TO:%s", to);
	else
		g_snprintf(buf, sizeof(buf), "RCPT TO:<%s>", to);
	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, buf) < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "SMTP> %s\n", buf);

	session->cur_to = session->cur_to->next;

	return SM_OK;
}

static gint smtp_data(SMTPSession *session)
{
	session->state = SMTP_DATA;

	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, "DATA") < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "SMTP> DATA\n");

	return SM_OK;
}

static gint smtp_send_data(SMTPSession *session)
{
	session->state = SMTP_SEND_DATA;

	session_send_data(SESSION(session), session->send_data,
			  session->send_data_len);

	return SM_OK;
}

static gint smtp_make_ready(SMTPSession *session)
{
	session->state = SMTP_MAIL_SENT_OK;

	return SM_OK;
}

gint smtp_quit(SMTPSession *session)
{
	session->state = SMTP_QUIT;

	session_send_msg(SESSION(session), SESSION_MSG_NORMAL, "QUIT");
	log_print(LOG_PROTOCOL, "SMTP> QUIT\n");

	return SM_OK;
}

static gint smtp_eom(SMTPSession *session)
{
	session->state = SMTP_EOM;

	if (session_send_msg(SESSION(session), SESSION_MSG_NORMAL, ".") < 0)
		return SM_ERROR;
	log_print(LOG_PROTOCOL, "SMTP> . (EOM)\n");

	return SM_OK;
}

static gint smtp_session_recv_msg(Session *session, const gchar *msg)
{
	SMTPSession *smtp_session = SMTP_SESSION(session);
	gboolean cont = FALSE;
	gint ret = 0;
	
	if (strlen(msg) < 4) {
		log_warning(LOG_PROTOCOL, _("bad SMTP response\n"));
		return -1;
	}

	switch (smtp_session->state) {
	case SMTP_EHLO:
	case SMTP_STARTTLS:
	case SMTP_AUTH:
	case SMTP_AUTH_PLAIN:
	case SMTP_AUTH_LOGIN_USER:
	case SMTP_AUTH_LOGIN_PASS:
	case SMTP_AUTH_CRAM_MD5:
		log_print(LOG_PROTOCOL, "ESMTP< %s\n", msg);
		break;
	default:
		log_print(LOG_PROTOCOL, "SMTP< %s\n", msg);
		break;
	}

	if (msg[0] == '5' && msg[1] == '0' &&
	    (msg[2] == '4' || msg[2] == '3' || msg[2] == '1')) {
		log_warning(LOG_PROTOCOL, _("error occurred on SMTP session\n"));
		smtp_session->state = SMTP_ERROR;
		smtp_session->error_val = SM_ERROR;
		g_free(smtp_session->error_msg);
		smtp_session->error_msg = g_strdup(msg);
		return -1;
	}

	if (!strncmp(msg, "535", 3)) {
		log_warning(LOG_PROTOCOL, _("error occurred on authentication\n"));
		smtp_session->state = SMTP_ERROR;
		smtp_session->error_val = SM_AUTHFAIL;
		g_free(smtp_session->error_msg);
		smtp_session->error_msg = g_strdup(msg);
		return -1;
	}

	if (msg[0] != '1' && msg[0] != '2' && msg[0] != '3') {
		log_warning(LOG_PROTOCOL, _("error occurred on SMTP session\n"));
		smtp_session->state = SMTP_ERROR;
		smtp_session->error_val = SM_ERROR;
		g_free(smtp_session->error_msg);
		smtp_session->error_msg = g_strdup(msg);
		return -1;
	}

	if (msg[3] == '-')
		cont = TRUE;
	else if (msg[3] != ' ' && msg[3] != '\0') {
		log_warning(LOG_PROTOCOL, _("bad SMTP response\n"));
		smtp_session->state = SMTP_ERROR;
		smtp_session->error_val = SM_UNRECOVERABLE;
		return -1;
	}

	/* ignore all multiline responses except for EHLO */
	if (cont && smtp_session->state != SMTP_EHLO)
		return session_recv_msg(session);

	switch (smtp_session->state) {
	case SMTP_READY:
		if (strstr(msg, "ESMTP"))
			smtp_session->is_esmtp = TRUE;
	case SMTP_CONNECTED:
#ifdef USE_GNUTLS
		if (smtp_session->user || session->ssl_type != SSL_NONE ||
		    smtp_session->is_esmtp)
#else
		if (smtp_session->user || smtp_session->is_esmtp)
#endif
			ret = smtp_ehlo(smtp_session);
		else
			ret = smtp_helo(smtp_session);
		break;
	case SMTP_HELO:
		ret = smtp_from(smtp_session);
		break;
	case SMTP_EHLO:
		ret = smtp_ehlo_recv(smtp_session, msg);
		if (cont == TRUE)
			break;
		if (smtp_session->max_message_size > 0
		&& smtp_session->max_message_size < 
		   smtp_session->send_data_len) {
			log_warning(LOG_PROTOCOL, _("Message is too big "
			      "(Maximum size is %s)\n"),
			      to_human_readable(
			       (goffset)(smtp_session->max_message_size)));
			smtp_session->state = SMTP_ERROR;
			smtp_session->error_val = SM_ERROR;
			return -1;
		}
#ifdef USE_GNUTLS
		if (session->ssl_type == SSL_STARTTLS &&
		    smtp_session->tls_init_done == FALSE) {
			ret = smtp_starttls(smtp_session);
			break;
		}
#endif
		if (smtp_session->user) {
			if (smtp_auth(smtp_session) != SM_OK) {
#ifdef USE_GNUTLS
				if (session->ssl_type == SSL_NONE
				&&  smtp_session->tls_init_done == FALSE
				&&  (smtp_session->avail_auth_type & SMTPAUTH_TLS_AVAILABLE))
					ret = smtp_starttls(smtp_session);
				else
#endif
					ret = smtp_from(smtp_session);
			}
		} else
			ret = smtp_from(smtp_session);
		break;
	case SMTP_STARTTLS:
#ifdef USE_GNUTLS
		if (session_start_tls(session) < 0) {
			log_warning(LOG_PROTOCOL, _("couldn't start TLS session\n"));
			smtp_session->state = SMTP_ERROR;
			smtp_session->error_val = SM_ERROR;
			return -1;
		}
		smtp_session->tls_init_done = TRUE;
		ret = smtp_ehlo(smtp_session);
#endif
		break;
	case SMTP_AUTH:
		ret = smtp_auth_recv(smtp_session, msg);
		break;
	case SMTP_AUTH_LOGIN_USER:
		ret = smtp_auth_login_user_recv(smtp_session, msg);
		break;
	case SMTP_AUTH_PLAIN:
	case SMTP_AUTH_LOGIN_PASS:
	case SMTP_AUTH_CRAM_MD5:
		ret = smtp_from(smtp_session);
		break;
	case SMTP_FROM:
		if (smtp_session->cur_to)
			ret = smtp_rcpt(smtp_session);
		break;
	case SMTP_RCPT:
		if (smtp_session->cur_to)
			ret = smtp_rcpt(smtp_session);
		else
			ret = smtp_data(smtp_session);
		break;
	case SMTP_DATA:
		ret = smtp_send_data(smtp_session);
		break;
	case SMTP_EOM:
		smtp_make_ready(smtp_session);
		break;
	case SMTP_QUIT:
		session_disconnect(session);
		break;
	case SMTP_ERROR:
	default:
		log_warning(LOG_PROTOCOL, _("error occurred on SMTP session\n"));
		smtp_session->error_val = SM_ERROR;
		return -1;
	}

	if (cont && ret == SM_OK)
		return session_recv_msg(session);

	if (ret != SM_OK)
		smtp_session->error_val = SM_ERROR;

	return ret == SM_OK ? 0 : -1;
}

static gint smtp_session_send_data_finished(Session *session, guint len)
{
	return smtp_eom(SMTP_SESSION(session));
}
