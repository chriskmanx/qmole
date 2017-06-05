/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2005-2012 DINH Viet Hoa and the Claws Mail team
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

#ifdef HAVE_LIBETPAN

#include <glib.h>
#include <glib/gi18n.h>
#include "nntp-thread.h"
#include "news.h"
#include <sys/types.h>
#include <sys/stat.h>
#if (defined(__DragonFly__) || defined (__NetBSD__) || defined (__FreeBSD__) || defined (__OpenBSD__) || defined (__CYGWIN__))
#include <sys/socket.h>
#endif
#include <fcntl.h>
#ifndef G_OS_WIN32
#include <sys/mman.h>
#include <sys/wait.h>
#endif
#include <gtk/gtk.h>
#include <log.h>
#include "etpan-thread-manager.h"
#include "utils.h"
#include "mainwindow.h"
#include "ssl_certificate.h"
#include "socket.h"
#include "remotefolder.h"

#define DISABLE_LOG_DURING_LOGIN

static struct etpan_thread_manager * thread_manager = NULL;
static chash * nntp_hash = NULL;
static chash * session_hash = NULL;
static guint thread_manager_signal = 0;
static GIOChannel * io_channel = NULL;

static void (*previous_stream_logger)(int direction,
    const char * str, size_t size);

static void nntp_logger(int direction, const char * str, size_t size) 
{
	gchar *buf;
	gchar **lines;
	int i = 0;

	if (size > 256) {
		log_print(LOG_PROTOCOL, "NNTP%c [data - %zd bytes]\n", direction?'>':'<', size);
		return;
	}
	buf = malloc(size+1);
	memset(buf, 0, size+1);
	strncpy(buf, str, size);
	buf[size] = '\0';

	if (!strncmp(buf, "<<<<<<<", 7) 
	||  !strncmp(buf, ">>>>>>>", 7)) {
		free(buf);
		return;
	}
	while (strstr(buf, "\r"))
		*strstr(buf, "\r") = ' ';
	while (strlen(buf) > 0 && buf[strlen(buf)-1] == '\n')
		buf[strlen(buf)-1] = '\0';

	lines = g_strsplit(buf, "\n", -1);

	while (lines[i] && *lines[i]) {
		log_print(LOG_PROTOCOL, "NNTP%c %s\n", direction?'>':'<', lines[i]);
		i++;
	}
	g_strfreev(lines);
	free(buf);
}

static void delete_nntp(Folder *folder, newsnntp *nntp)
{
	chashdatum key;

	key.data = &folder;
	key.len = sizeof(folder);
	chash_delete(session_hash, &key, NULL);
	
	key.data = &nntp;
	key.len = sizeof(nntp);
	if (nntp && nntp->nntp_stream) {
		/* we don't want libetpan to logout */
		mailstream_close(nntp->nntp_stream);
		nntp->nntp_stream = NULL;
	}
	debug_print("removing newsnntp %p\n", nntp);
	newsnntp_free(nntp);	
}

static gboolean thread_manager_event(GIOChannel * source,
    GIOCondition condition,
    gpointer data)
{
#ifdef G_OS_WIN32
	gsize bytes_read;
	gchar ch;
	
	if (condition & G_IO_IN)
		g_io_channel_read_chars(source, &ch, 1, &bytes_read, NULL);
#endif
	etpan_thread_manager_loop(thread_manager);
	
	return TRUE;
}

#define ETPAN_DEFAULT_NETWORK_TIMEOUT 60
extern gboolean etpan_skip_ssl_cert_check;

void nntp_main_init(gboolean skip_ssl_cert_check)
{
	int fd_thread_manager;
	
	etpan_skip_ssl_cert_check = skip_ssl_cert_check;
	
	nntp_hash = chash_new(CHASH_COPYKEY, CHASH_DEFAULTSIZE);
	session_hash = chash_new(CHASH_COPYKEY, CHASH_DEFAULTSIZE);
	
	thread_manager = etpan_thread_manager_new();
	
	fd_thread_manager = etpan_thread_manager_get_fd(thread_manager);
	
#ifndef G_OS_WIN32
	io_channel = g_io_channel_unix_new(fd_thread_manager);
#else
	io_channel = g_io_channel_win32_new_fd(fd_thread_manager);
#endif
	
	thread_manager_signal = g_io_add_watch_full(io_channel, 0, G_IO_IN,
						    thread_manager_event,
						    (gpointer) NULL,
						    NULL);
}

void nntp_main_done(gboolean have_connectivity)
{
	etpan_thread_manager_stop(thread_manager);
#if defined(__NetBSD__) || defined(__OpenBSD__) || defined(__FreeBSD__)
	return;
#endif
	etpan_thread_manager_join(thread_manager);
	
	g_source_remove(thread_manager_signal);
	g_io_channel_unref(io_channel);
	
	etpan_thread_manager_free(thread_manager);
	
	chash_free(session_hash);
	chash_free(nntp_hash);
}

void nntp_init(Folder * folder)
{
	struct etpan_thread * thread;
	chashdatum key;
	chashdatum value;
	
	thread = etpan_thread_manager_get_thread(thread_manager);
	
	key.data = &folder;
	key.len = sizeof(folder);
	value.data = thread;
	value.len = 0;
	
	chash_set(nntp_hash, &key, &value, NULL);
}

void nntp_done(Folder * folder)
{
	struct etpan_thread * thread;
	chashdatum key;
	chashdatum value;
	int r;
	
	key.data = &folder;
	key.len = sizeof(folder);
	
	r = chash_get(nntp_hash, &key, &value);
	if (r < 0)
		return;
	
	thread = value.data;
	
	etpan_thread_unbind(thread);
	
	chash_delete(nntp_hash, &key, NULL);
	
	debug_print("remove thread");
}

static struct etpan_thread * get_thread(Folder * folder)
{
	struct etpan_thread * thread;
	chashdatum key;
	chashdatum value;
	
	key.data = &folder;
	key.len = sizeof(folder);
	
	chash_get(nntp_hash, &key, &value);
	thread = value.data;
	
	return thread;
}

static newsnntp * get_nntp(Folder * folder)
{
	newsnntp * nntp;
	chashdatum key;
	chashdatum value;
	int r;
	
	key.data = &folder;
	key.len = sizeof(folder);
	
	r = chash_get(session_hash, &key, &value);
	if (r < 0)
		return NULL;
	
	nntp = value.data;
	debug_print("found nntp %p\n", nntp);
	return nntp;
}


static void generic_cb(int cancelled, void * result, void * callback_data)
{
	struct etpan_thread_op * op;
	
	op = (struct etpan_thread_op *) callback_data;

	debug_print("generic_cb\n");
	op->finished = 1;
}

static void threaded_run(Folder * folder, void * param, void * result,
			 void (* func)(struct etpan_thread_op * ))
{
	struct etpan_thread_op * op;
	struct etpan_thread * thread;
	
	nntp_folder_ref(folder);

	op = etpan_thread_op_new();
	
	op->nntp = get_nntp(folder);
	op->param = param;
	op->result = result;
	
	op->cancellable = 0;
	op->run = func;
	op->callback = generic_cb;
	op->callback_data = op;
	op->cleanup = NULL;
	
	op->finished = 0;
	
	previous_stream_logger = mailstream_logger;
	mailstream_logger = nntp_logger;

	thread = get_thread(folder);
	etpan_thread_op_schedule(thread, op);
	
	while (!op->finished) {
		gtk_main_iteration();
	}
	
	mailstream_logger = previous_stream_logger;

	etpan_thread_op_free(op);

	nntp_folder_unref(folder);
}


/* connect */

struct connect_param {
	newsnntp * nntp;
	PrefsAccount *account;
	const char * server;
	int port;
};

struct connect_result {
	int error;
};

#define CHECK_NNTP() {						\
	if (!param->nntp) {					\
		result->error = NEWSNNTP_ERROR_BAD_STATE;	\
		return;						\
	}							\
}

static void connect_run(struct etpan_thread_op * op)
{
	int r;
	struct connect_param * param;
	struct connect_result * result;
	
	param = op->param;
	result = op->result;
	
	CHECK_NNTP();

	r = newsnntp_socket_connect(param->nntp,
				    param->server, param->port);
	
	result->error = r;
}


int nntp_threaded_connect(Folder * folder, const char * server, int port)
{
	struct connect_param param;
	struct connect_result result;
	chashdatum key;
	chashdatum value;
	newsnntp * nntp, * oldnntp;
	
	oldnntp = get_nntp(folder);

	nntp = newsnntp_new(0, NULL);
	
	if (oldnntp) {
		debug_print("deleting old nntp %p\n", oldnntp);
		delete_nntp(folder, oldnntp);
	}
	
	key.data = &folder;
	key.len = sizeof(folder);
	value.data = nntp;
	value.len = 0;
	chash_set(session_hash, &key, &value, NULL);
	
	param.nntp = nntp;
	param.server = server;
	param.port = port;
	
	refresh_resolvers();
	threaded_run(folder, &param, &result, connect_run);
	
	debug_print("connect ok %i with nntp %p\n", result.error, nntp);
	
	return result.error;
}

static int etpan_certificate_check(const unsigned char *certificate, int len, void *data)
{
#ifdef USE_GNUTLS
	struct connect_param *param = (struct connect_param *)data;
	gnutls_x509_crt cert = NULL;
	gnutls_datum tmp;
	
	if (certificate == NULL || len < 0) {
		g_warning("no cert presented.\n");
		return 0;
	}
	
	tmp.data = malloc(len);
	memcpy(tmp.data, certificate, len);
	tmp.size = len;
	gnutls_x509_crt_init(&cert);
	if (gnutls_x509_crt_import(cert, &tmp, GNUTLS_X509_FMT_DER) < 0) {
		g_warning("nntp: can't get cert\n");
		return 0;
	} else if (ssl_certificate_check(cert, (guint)-1,
		(gchar *)param->server, (gushort)param->port) == TRUE) {
		gnutls_x509_crt_deinit(cert);
		return 0;
	} else {
		gnutls_x509_crt_deinit(cert);
		return -1;
	}
#endif
	return 0;
}

static void connect_ssl_context_cb(struct mailstream_ssl_context * ssl_context, void * data)
{
#ifdef USE_GNUTLS
	PrefsAccount *account = (PrefsAccount *)data;
	const gchar *cert_path = NULL;
	const gchar *password = NULL;
	gnutls_x509_crt x509 = NULL;
	gnutls_x509_privkey pkey = NULL;

	if (account->in_ssl_client_cert_file && *account->in_ssl_client_cert_file)
		cert_path = account->in_ssl_client_cert_file;
	if (account->in_ssl_client_cert_pass && *account->in_ssl_client_cert_pass)
		password = account->in_ssl_client_cert_pass;
	
	if (mailstream_ssl_set_client_certificate_data(ssl_context, NULL, 0) < 0 ||
	    mailstream_ssl_set_client_private_key_data(ssl_context, NULL, 0) < 0)
		debug_print("Impossible to set the client certificate.\n");
	x509 = ssl_certificate_get_x509_from_pem_file(cert_path);
	pkey = ssl_certificate_get_pkey_from_pem_file(cert_path);
	if (!(x509 && pkey)) {
		/* try pkcs12 format */
		ssl_certificate_get_x509_and_pkey_from_p12_file(cert_path, password, &x509, &pkey);
	}
	if (x509 && pkey) {
		unsigned char *x509_der = NULL, *pkey_der = NULL;
		size_t x509_len, pkey_len;
		
		x509_len = (size_t)gnutls_i2d_X509(x509, &x509_der);
		pkey_len = (size_t)gnutls_i2d_PrivateKey(pkey, &pkey_der);
		if (x509_len > 0 && pkey_len > 0) {
			if (mailstream_ssl_set_client_certificate_data(ssl_context, x509_der, x509_len) < 0 ||
			    mailstream_ssl_set_client_private_key_data(ssl_context, pkey_der, pkey_len) < 0) 
				log_error(LOG_PROTOCOL, _("Impossible to set the client certificate.\n"));
			g_free(x509_der);
			g_free(pkey_der);
		}
		gnutls_x509_crt_deinit(x509);
		gnutls_x509_privkey_deinit(pkey);
	}
#endif
}

static void connect_ssl_run(struct etpan_thread_op * op)
{
	int r;
	struct connect_param * param;
	struct connect_result * result;
	
	param = op->param;
	result = op->result;
	
	CHECK_NNTP();

	r = newsnntp_ssl_connect_with_callback(param->nntp,
				 param->server, param->port,
				 connect_ssl_context_cb, param->account);
	result->error = r;
}

int nntp_threaded_connect_ssl(Folder * folder, const char * server, int port)
{
	struct connect_param param;
	struct connect_result result;
	chashdatum key;
	chashdatum value;
	newsnntp * nntp, * oldnntp;
	unsigned char *certificate = NULL;
	int cert_len;
	
	oldnntp = get_nntp(folder);

	nntp = newsnntp_new(0, NULL);
	
	if (oldnntp) {
		debug_print("deleting old nntp %p\n", oldnntp);
		delete_nntp(folder, oldnntp);
	}

	key.data = &folder;
	key.len = sizeof(folder);
	value.data = nntp;
	value.len = 0;
	chash_set(session_hash, &key, &value, NULL);
	
	param.nntp = nntp;
	param.server = server;
	param.port = port;
	param.account = folder->account;

	refresh_resolvers();
	threaded_run(folder, &param, &result, connect_ssl_run);

	if (result.error == NEWSNNTP_NO_ERROR && !etpan_skip_ssl_cert_check) {
		cert_len = (int)mailstream_ssl_get_certificate(nntp->nntp_stream, &certificate);
		if (etpan_certificate_check(certificate, cert_len, &param) < 0)
			return -1;
		if (certificate) 
			free(certificate); 
	}
	debug_print("connect %d with nntp %p\n", result.error, nntp);
	
	return result.error;
}

void nntp_threaded_disconnect(Folder * folder)
{
	newsnntp * nntp;
	
	nntp = get_nntp(folder);
	if (nntp == NULL) {
		debug_print("was disconnected\n");
		return;
	}
	
	debug_print("deleting old nntp %p\n", nntp);
	delete_nntp(folder, nntp);
	
	debug_print("disconnect ok\n");
}

void nntp_threaded_cancel(Folder * folder)
{
	newsnntp * nntp;
	
	nntp = get_nntp(folder);
	if (nntp->nntp_stream != NULL)
		mailstream_cancel(nntp->nntp_stream);
}


struct login_param {
	newsnntp * nntp;
	const char * login;
	const char * password;
};

struct login_result {
	int error;
};

static void login_run(struct etpan_thread_op * op)
{
	struct login_param * param;
	struct login_result * result;
	int r;
#ifdef DISABLE_LOG_DURING_LOGIN
	int old_debug;
#endif
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();

#ifdef DISABLE_LOG_DURING_LOGIN
	old_debug = mailstream_debug;
	mailstream_debug = 0;
#endif

	r = newsnntp_authinfo_username(param->nntp, param->login);
	/* libetpan returning NO_ERROR means it received resp.code 281:
	   in this case auth. is already successful, no password is needed. */
	if (r == NEWSNNTP_WARNING_REQUEST_AUTHORIZATION_PASSWORD) {
		r = newsnntp_authinfo_password(param->nntp, param->password);
	}
	


#ifdef DISABLE_LOG_DURING_LOGIN
	mailstream_debug = old_debug;
#endif
	
	result->error = r;
	if (param->nntp->nntp_response)
		nntp_logger(0, param->nntp->nntp_response, strlen(param->nntp->nntp_response));

	debug_print("nntp login run - end %i\n", r);
}

int nntp_threaded_login(Folder * folder, const char * login, const char * password)
{
	struct login_param param;
	struct login_result result;
	
	debug_print("nntp login - begin\n");
	
	param.nntp = get_nntp(folder);
	param.login = login;
	param.password = password;

	threaded_run(folder, &param, &result, login_run);
	
	debug_print("nntp login - end\n");
	
	return result.error;
}

struct date_param {
	newsnntp * nntp;
	struct tm * lt;
};

struct date_result {
	int error;
};

static void date_run(struct etpan_thread_op * op)
{
	struct date_param * param;
	struct date_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();

	r = newsnntp_date(param->nntp, param->lt);
	
	result->error = r;
	debug_print("nntp date run - end %i\n", r);
}

int nntp_threaded_date(Folder * folder, struct tm *lt)
{
	struct date_param param;
	struct date_result result;
	
	debug_print("nntp date - begin\n");
	
	param.nntp = get_nntp(folder);
	param.lt = lt;

	threaded_run(folder, &param, &result, date_run);
	
	debug_print("nntp date - end\n");
	
	return result.error;
}

struct list_param {
	newsnntp * nntp;
	clist **grouplist;
};

struct list_result {
	int error;
};

static void list_run(struct etpan_thread_op * op)
{
	struct list_param * param;
	struct list_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();

	r = newsnntp_list(param->nntp, param->grouplist);
	
	result->error = r;
	debug_print("nntp list run - end %i\n", r);
}

int nntp_threaded_list(Folder * folder, clist **grouplist)
{
	struct list_param param;
	struct list_result result;
	
	debug_print("nntp list - begin\n");
	
	param.nntp = get_nntp(folder);
	param.grouplist = grouplist;

	threaded_run(folder, &param, &result, list_run);
	
	debug_print("nntp list - end\n");
	
	return result.error;
}

struct post_param {
	newsnntp * nntp;
	char *contents;
	size_t len;
};

struct post_result {
	int error;
};

static void post_run(struct etpan_thread_op * op)
{
	struct post_param * param;
	struct post_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();

	r = newsnntp_post(param->nntp, param->contents, param->len);
	
	result->error = r;
	debug_print("nntp post run - end %i\n", r);
}

int nntp_threaded_post(Folder * folder, char *contents, size_t len)
{
	struct post_param param;
	struct post_result result;
	
	debug_print("nntp post - begin\n");
	
	param.nntp = get_nntp(folder);
	param.contents = contents;
	param.len = len;

	threaded_run(folder, &param, &result, post_run);
	
	debug_print("nntp post - end\n");
	
	return result.error;
}

struct article_param {
	newsnntp * nntp;
	guint32 num;
	char **contents;
	size_t *len;
};

struct article_result {
	int error;
};

static void article_run(struct etpan_thread_op * op)
{
	struct article_param * param;
	struct article_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();

	r = newsnntp_article(param->nntp, param->num, param->contents, param->len);
	
	result->error = r;
	debug_print("nntp article run - end %i\n", r);
}

int nntp_threaded_article(Folder * folder, guint32 num, char **contents, size_t *len)
{
	struct article_param param;
	struct article_result result;
	
	debug_print("nntp article - begin\n");
	
	param.nntp = get_nntp(folder);
	param.num = num;
	param.contents = contents;
	param.len = len;

	threaded_run(folder, &param, &result, article_run);
	
	debug_print("nntp post - end\n");
	
	return result.error;
}

struct group_param {
	newsnntp * nntp;
	const char *group;
	struct newsnntp_group_info **info;
};

struct group_result {
	int error;
};

static void group_run(struct etpan_thread_op * op)
{
	struct group_param * param;
	struct group_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();

	r = newsnntp_group(param->nntp, param->group, param->info);
	
	result->error = r;
	debug_print("nntp group run - end %i\n", r);
}

int nntp_threaded_group(Folder * folder, const char *group, struct newsnntp_group_info **info)
{
	struct group_param param;
	struct group_result result;
	
	debug_print("nntp group - begin\n");
	
	param.nntp = get_nntp(folder);
	param.group = group;
	param.info = info;

	threaded_run(folder, &param, &result, group_run);
	
	debug_print("nntp group - end\n");
	
	return result.error;
}

struct mode_reader_param {
	newsnntp * nntp;
};

struct mode_reader_result {
	int error;
};

static void mode_reader_run(struct etpan_thread_op * op)
{
	struct mode_reader_param * param;
	struct mode_reader_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();

	r = newsnntp_mode_reader(param->nntp);
	
	result->error = r;
	debug_print("nntp mode_reader run - end %i\n", r);
}

int nntp_threaded_mode_reader(Folder * folder)
{
	struct mode_reader_param param;
	struct mode_reader_result result;
	
	debug_print("nntp mode_reader - begin\n");
	
	param.nntp = get_nntp(folder);

	threaded_run(folder, &param, &result, mode_reader_run);
	
	debug_print("nntp mode_reader - end\n");
	
	return result.error;
}

struct xover_param {
	newsnntp * nntp;
	guint32 beg;
	guint32 end;
	struct newsnntp_xover_resp_item **result;
	clist **msglist;
};

struct xover_result {
	int error;
};

static void xover_run(struct etpan_thread_op * op)
{
	struct xover_param * param;
	struct xover_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();
	
	if (param->result) {
		r = newsnntp_xover_single(param->nntp, param->beg, param->result);
	} else {
		r = newsnntp_xover_range(param->nntp, param->beg, param->end, param->msglist);
	}
	
	result->error = r;
	debug_print("nntp xover run - end %i\n", r);
}

int nntp_threaded_xover(Folder * folder, guint32 beg, guint32 end, struct newsnntp_xover_resp_item **single_result, clist **multiple_result)
{
	struct xover_param param;
	struct xover_result result;
	
	debug_print("nntp xover - begin\n");
	
	param.nntp = get_nntp(folder);
	param.beg = beg;
	param.end = end;
	param.result = single_result;
	param.msglist = multiple_result;

	threaded_run(folder, &param, &result, xover_run);
	
	debug_print("nntp xover - end\n");
	
	return result.error;
}

struct xhdr_param {
	newsnntp * nntp;
	const char *header;
	guint32 beg;
	guint32 end;
	clist **hdrlist;
};

struct xhdr_result {
	int error;
};

static void xhdr_run(struct etpan_thread_op * op)
{
	struct xhdr_param * param;
	struct xhdr_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_NNTP();
	
	if (param->beg == param->end) {
		r = newsnntp_xhdr_single(param->nntp, param->header, param->beg, param->hdrlist);
	} else {
		r = newsnntp_xhdr_range(param->nntp, param->header, param->beg, param->end, param->hdrlist);
	}
	
	result->error = r;
	debug_print("nntp xhdr run - end %i\n", r);
}

int nntp_threaded_xhdr(Folder * folder, const char *header, guint32 beg, guint32 end, clist **hdrlist)
{
	struct xhdr_param param;
	struct xhdr_result result;
	
	debug_print("nntp xhdr - begin\n");
	
	param.nntp = get_nntp(folder);
	param.header = header;
	param.beg = beg;
	param.end = end;
	param.hdrlist = hdrlist;

	threaded_run(folder, &param, &result, xhdr_run);
	
	debug_print("nntp xhdr - end\n");
	
	return result.error;
}


#else

void nntp_main_init(void)
{
}
void nntp_main_done(gboolean have_connectivity)
{
}
void nntp_main_set_timeout(int sec)
{
}

void nntp_threaded_cancel(Folder * folder);
{
}

#endif
