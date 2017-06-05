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

#ifndef PREFS_ACCOUNT_H
#define PREFS_ACCOUNT_H

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif
#include "ssl.h"
typedef struct _PrefsAccount	PrefsAccount;

typedef enum {
	A_POP3,
	A_APOP,	/* deprecated */
	A_RPOP,	/* deprecated */
	A_IMAP4,
	A_NNTP,
	A_LOCAL,
	A_NONE,	/* SMTP only */
	NUM_RECV_PROTOCOLS
} RecvProtocol;

typedef enum {
	SIG_FILE,
	SIG_COMMAND,
	SIG_DIRECT
} SigType;

#include <glib.h>

#include "smtp.h"
#include "gtk/prefswindow.h"

struct _Folder;

/* Changes to this data structure might need to be reflected
 * in account_clone() */
struct _PrefsAccount
{
	gchar *account_name;

	/* Personal info */
	gchar *name;
	gchar *address;
	gchar *organization;

	/* Server info */
	RecvProtocol protocol;
	gchar *recv_server;
	gchar *smtp_server;
	gchar *nntp_server;
	gboolean use_nntp_auth;
	gboolean use_nntp_auth_onconnect;
	gchar *userid;
	gchar *passwd;
	gchar *session_passwd;

	gchar * local_mbox;
	gboolean use_mail_command;
	gchar * mail_command;

	SSLType ssl_pop;
	SSLType ssl_imap;
	SSLType ssl_nntp;
	SSLType ssl_smtp;
	
	gchar *out_ssl_client_cert_file;
	gchar *out_ssl_client_cert_pass;
	gchar *in_ssl_client_cert_file;
	gchar *in_ssl_client_cert_pass;

	gboolean use_nonblocking_ssl;

	/* Receive */
	gboolean use_apop_auth;
	gboolean rmmail;
	gint msg_leave_time;
	gint msg_leave_hour;
	gboolean recv_at_getall;
	gboolean sd_rmmail_on_download;
	gboolean enable_size_limit;
	gint size_limit;
	gboolean filter_on_recv;
	gboolean filterhook_on_recv;
	gchar *inbox;
	gchar *local_inbox;
	gint max_articles;

	gint imap_auth_type;

	/* Send */
	gboolean gen_msgid;
	gboolean gen_xmailer;
	gboolean add_customhdr;
	gboolean use_smtp_auth;
	SMTPAuthType smtp_auth_type;
	gchar *smtp_userid;
	gchar *smtp_passwd;
	gchar *session_smtp_passwd;

	gboolean pop_before_smtp;
	gint pop_before_smtp_timeout;
	time_t last_pop_login_time;

	GSList *customhdr_list;

	/* Compose */
	SigType sig_type;
	gchar    *sig_path;
	gboolean  auto_sig;
	gchar 	 *sig_sep;
	gboolean  set_autocc;
	gchar    *auto_cc;
	gboolean  set_autobcc;
	gchar    *auto_bcc;
	gboolean  set_autoreplyto;
	gchar    *auto_replyto;
	gboolean  enable_default_dictionary;
	gchar	 *default_dictionary;
	gboolean  enable_default_alt_dictionary;
	gchar	 *default_alt_dictionary;
	gboolean  compose_with_format;
	gchar	 *compose_subject_format;
	gchar	 *compose_body_format;
	gboolean  reply_with_format;
	gchar	 *reply_quotemark;
	gchar	 *reply_body_format;
	gboolean  forward_with_format;
	gchar	 *forward_quotemark;
	gchar	 *forward_body_format;

	/* Privacy */
	gchar	 *default_privacy_system;
	gboolean  default_encrypt;
	gboolean  default_encrypt_reply;
	gboolean  default_sign;
	gboolean  default_sign_reply;
	gboolean  save_encrypted_as_clear_text;
	gboolean  encrypt_to_self;

	/* Advanced */
	gboolean  set_smtpport;
	gushort   smtpport;
	gboolean  set_popport;
	gushort   popport;
	gboolean  set_imapport;
	gushort   imapport;
	gboolean  set_nntpport;
	gushort   nntpport;
	gboolean  set_domain;
	gchar    *domain;
	gboolean  msgid_with_addr;
	gboolean  mark_crosspost_read;
	gint	  crosspost_col;

#ifndef G_OS_WIN32
	/* Use this command to open a socket, rather than doing so
	 * directly.  Good if you want to perhaps use a special socks
	 * tunnel command, or run IMAP-over-SSH.  In this case the
	 * server, port etc are only for the user's own information
	 * and are not used.  username and password are used to
	 * authenticate the account only if necessary, since some
	 * tunnels will implicitly authenticate by running e.g. imapd
	 * as a particular user. */
	gboolean  set_tunnelcmd;
	gchar     *tunnelcmd;
#endif

	gchar *imap_dir;
	gboolean imap_subsonly;
	gboolean low_bandwidth;
	gboolean imap_use_trash;

	gboolean set_sent_folder;
	gchar *sent_folder;
	gboolean set_queue_folder;
	gchar *queue_folder;
	gboolean set_draft_folder;
	gchar *draft_folder;
	gboolean set_trash_folder;
	gchar *trash_folder;

	/* Default or not */
	gboolean is_default;
	/* Unique account ID */
	gint account_id;

	struct _Folder *folder;
	GHashTable *privacy_prefs;
	SMTPSession *session;
};

void prefs_account_init			(void);

PrefsAccount *prefs_account_new		(void);

void prefs_account_read_config		(PrefsAccount	*ac_prefs,
					 const gchar	*label);
void prefs_account_write_config_all	(GList		*account_list);

void prefs_account_free			(PrefsAccount	*ac_prefs);

PrefsAccount *prefs_account_open	(PrefsAccount	*ac_prefs, gboolean *dirty);

const gchar *prefs_account_get_privacy_prefs(PrefsAccount *account, gchar *id);
void prefs_account_set_privacy_prefs(PrefsAccount *account, gchar *id, gchar *new_value);

void prefs_account_register_page	(PrefsPage 	*page);
void prefs_account_unregister_page	(PrefsPage 	*page);

#endif /* PREFS_ACCOUNT_H */
