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
#include "imap-thread.h"
#include <imap.h>
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
#include "ssl.h"
#include "ssl_certificate.h"
#include "socket.h"
#include "remotefolder.h"
#include "tags.h"

#define DISABLE_LOG_DURING_LOGIN

static struct etpan_thread_manager * thread_manager = NULL;
static chash * courier_workaround_hash = NULL;
static chash * imap_hash = NULL;
static chash * session_hash = NULL;
static guint thread_manager_signal = 0;
static GIOChannel * io_channel = NULL;

static void delete_imap(Folder *folder, mailimap *imap)
{
	chashdatum key;

	key.data = &folder;
	key.len = sizeof(folder);
	chash_delete(session_hash, &key, NULL);
	
	key.data = &imap;
	key.len = sizeof(imap);
	chash_delete(courier_workaround_hash, &key, NULL);
	if (imap && imap->imap_stream) {
		/* we don't want libetpan to logout */
		mailstream_close(imap->imap_stream);
		imap->imap_stream = NULL;
	}
	debug_print("removing mailimap %p\n", imap);
	mailimap_free(imap);	
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

static void imap_logger_noop(int direction, const char * str, size_t size) 
{
	/* inhibit logging */
}

static void imap_logger_cmd(int direction, const char * str, size_t size) 
{
	gchar *buf;
	gchar **lines;
	int i = 0;

	if (size > 8192) {
		log_print(LOG_PROTOCOL, "IMAP4%c [CMD data - %zd bytes]\n", direction?'>':'<', size);
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
		log_print(LOG_PROTOCOL, "IMAP4%c %s\n", direction?'>':'<', lines[i]);
		i++;
	}
	g_strfreev(lines);
	free(buf);
}

static void imap_logger_fetch(int direction, const char * str, size_t size) 
{
	gchar *buf;
	gchar **lines;
	int i = 0;

	if (size > 128 && !direction) {
		log_print(LOG_PROTOCOL, "IMAP4%c [FETCH data - %zd bytes]\n", direction?'>':'<', size);
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

	if (direction != 0 || (buf[0] == '*' && buf[1] == ' ') || size < 32) {
		while (lines[i] && *lines[i]) {
			log_print(LOG_PROTOCOL, "IMAP4%c %s\n", direction?'>':'<', lines[i]);
			i++;
		}
	} else {
		log_print(LOG_PROTOCOL, "IMAP4%c [data - %zd bytes]\n", direction?'>':'<', size);
	}
	g_strfreev(lines);
	free(buf);
}

static void imap_logger_uid(int direction, const char * str, size_t size) 
{
	gchar *buf;
	gchar **lines;
	int i = 0;

	if (size > 8192) {
		log_print(LOG_PROTOCOL, "IMAP4%c [UID data - %zd bytes]\n", direction?'>':'<', size);
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
		int llen = strlen(lines[i]);
		if (llen < 64)
			log_print(LOG_PROTOCOL, "IMAP4%c %s\n", direction?'>':'<', lines[i]);
		else {
			gchar tmp[64];
			strncpy2(tmp, lines[i], 63);
			log_print(LOG_PROTOCOL, "IMAP4%c %s[... - %d bytes more]\n", direction?'>':'<', tmp,
				  llen-64);
		}
		i++;
	}
	g_strfreev(lines);
	free(buf);
}

static void imap_logger_append(int direction, const char * str, size_t size) 
{
	gchar *buf;
	gchar **lines;
	int i = 0;

	if (size > 8192) {
		log_print(LOG_PROTOCOL, "IMAP4%c [APPEND data - %zd bytes]\n", direction?'>':'<', size);
		return;
	} else if (direction == 0 && size > 64) {
		log_print(LOG_PROTOCOL, "IMAP4%c [APPEND data - %zd bytes]\n", direction?'>':'<', size);
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

	if (direction == 0 || (buf[0] == '*' && buf[1] == ' ') || size < 64) {
		while (lines[i] && *lines[i]) {
			log_print(LOG_PROTOCOL, "IMAP4%c %s\n", direction?'>':'<', lines[i]);
			i++;
		}
	} else {
		log_print(LOG_PROTOCOL, "IMAP4%c [data - %zd bytes]\n", direction?'>':'<', size);
	}
	g_strfreev(lines);
	free(buf);
}

#define ETPAN_DEFAULT_NETWORK_TIMEOUT 60
gboolean etpan_skip_ssl_cert_check = FALSE;
extern void mailsasl_ref(void);

void imap_main_init(gboolean skip_ssl_cert_check)
{
	int fd_thread_manager;
	
	etpan_skip_ssl_cert_check = skip_ssl_cert_check;
	mailstream_network_delay.tv_sec = ETPAN_DEFAULT_NETWORK_TIMEOUT;
	mailstream_network_delay.tv_usec = 0;
	
	mailstream_debug = 1;
	mailstream_logger = imap_logger_cmd;
	mailsasl_ref();
	
	imap_hash = chash_new(CHASH_COPYKEY, CHASH_DEFAULTSIZE);
	session_hash = chash_new(CHASH_COPYKEY, CHASH_DEFAULTSIZE);
	courier_workaround_hash = chash_new(CHASH_COPYKEY, CHASH_DEFAULTSIZE);
	
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

void imap_main_set_timeout(int sec)
{
	mailstream_network_delay.tv_sec = sec;
	mailstream_network_delay.tv_usec = 0;
}

void imap_main_done(gboolean have_connectivity)
{
	imap_disconnect_all(have_connectivity);
	etpan_thread_manager_stop(thread_manager);
#if defined(__NetBSD__) || defined(__OpenBSD__) || defined(__FreeBSD__)
	return;
#endif
	etpan_thread_manager_join(thread_manager);
	
	g_source_remove(thread_manager_signal);
	g_io_channel_unref(io_channel);
	
	etpan_thread_manager_free(thread_manager);
	
	chash_free(courier_workaround_hash);
	chash_free(session_hash);
	chash_free(imap_hash);
}

void imap_init(Folder * folder)
{
	struct etpan_thread * thread;
	chashdatum key;
	chashdatum value;
	
	thread = etpan_thread_manager_get_thread(thread_manager);
	
	key.data = &folder;
	key.len = sizeof(folder);
	value.data = thread;
	value.len = 0;
	
	chash_set(imap_hash, &key, &value, NULL);
}

void imap_done(Folder * folder)
{
	struct etpan_thread * thread;
	chashdatum key;
	chashdatum value;
	int r;
	
	key.data = &folder;
	key.len = sizeof(folder);
	
	r = chash_get(imap_hash, &key, &value);
	if (r < 0)
		return;
	
	thread = value.data;
	
	etpan_thread_unbind(thread);
	
	chash_delete(imap_hash, &key, NULL);
	
	debug_print("remove thread");
}

static struct etpan_thread * get_thread(Folder * folder)
{
	struct etpan_thread * thread;
	chashdatum key;
	chashdatum value;
	
	key.data = &folder;
	key.len = sizeof(folder);
	
	chash_get(imap_hash, &key, &value);
	thread = value.data;
	
	return thread;
}

static mailimap * get_imap(Folder * folder)
{
	mailimap * imap;
	chashdatum key;
	chashdatum value;
	int r;
	
	key.data = &folder;
	key.len = sizeof(folder);
	
	r = chash_get(session_hash, &key, &value);
	if (r < 0)
		return NULL;
	
	imap = value.data;
	debug_print("found imap %p\n", imap);
	return imap;
}

static gboolean cb_show_error(gpointer data)
{
	mainwindow_show_error();
	return FALSE;
}

static void generic_cb(int cancelled, void * result, void * callback_data)
{
	struct etpan_thread_op * op;
	
	op = (struct etpan_thread_op *) callback_data;

	debug_print("generic_cb\n");
	if (op->imap && op->imap->imap_response_info &&
	    op->imap->imap_response_info->rsp_alert) {
		log_error(LOG_PROTOCOL, "IMAP4< Alert: %s\n", 
			op->imap->imap_response_info->rsp_alert);
		g_timeout_add(10, cb_show_error, NULL);
	} 
	op->finished = 1;
}

static void threaded_run(Folder * folder, void * param, void * result,
			 void (* func)(struct etpan_thread_op * ))
{
	struct etpan_thread_op * op;
	struct etpan_thread * thread;
	
	imap_folder_ref(folder);

	op = etpan_thread_op_new();
	
	op->imap = get_imap(folder);
	op->param = param;
	op->result = result;
	
	op->cancellable = 0;
	op->run = func;
	op->callback = generic_cb;
	op->callback_data = op;
	op->cleanup = NULL;
	
	op->finished = 0;
	
	thread = get_thread(folder);
	etpan_thread_op_schedule(thread, op);
	
	while (!op->finished) {
		gtk_main_iteration();
	}
	
	etpan_thread_op_free(op);

	imap_folder_unref(folder);
}


/* connect */

struct connect_param {
	mailimap * imap;
	PrefsAccount *account;
	const char * server;
	int port;
};

struct connect_result {
	int error;
};

#define CHECK_IMAP() {						\
	if (!param->imap) {					\
		result->error = MAILIMAP_ERROR_BAD_STATE;	\
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
	
	CHECK_IMAP();

	r = mailimap_socket_connect(param->imap,
				    param->server, param->port);
	
	result->error = r;
}


int imap_threaded_connect(Folder * folder, const char * server, int port)
{
	struct connect_param param;
	struct connect_result result;
	chashdatum key;
	chashdatum value;
	mailimap * imap, * oldimap;
	
	oldimap = get_imap(folder);

	imap = mailimap_new(0, NULL);
	
	if (oldimap) {
		debug_print("deleting old imap %p\n", oldimap);
		delete_imap(folder, oldimap);
	}
	
	key.data = &folder;
	key.len = sizeof(folder);
	value.data = imap;
	value.len = 0;
	chash_set(session_hash, &key, &value, NULL);
	
	param.imap = imap;
	param.server = server;
	param.port = port;
	
	refresh_resolvers();
	threaded_run(folder, &param, &result, connect_run);
	
	debug_print("connect ok %i with imap %p\n", result.error, imap);
	
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
		g_warning("IMAP: can't get cert\n");
		return 0;
	} else if (ssl_certificate_check(cert, (guint)-1, (gchar *)param->server,
			(gushort)param->port) == TRUE) {
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
	
	CHECK_IMAP();

	r = mailimap_ssl_connect_with_callback(param->imap,
				 		param->server, param->port,
						connect_ssl_context_cb, param->account);
	result->error = r;
}

int imap_threaded_connect_ssl(Folder * folder, const char * server, int port)
{
	struct connect_param param;
	struct connect_result result;
	chashdatum key;
	chashdatum value;
	mailimap * imap, * oldimap;
	unsigned char *certificate = NULL;
	int cert_len;
	
	oldimap = get_imap(folder);

	imap = mailimap_new(0, NULL);
	
	if (oldimap) {
		debug_print("deleting old imap %p\n", oldimap);
		delete_imap(folder, oldimap);
	}

	key.data = &folder;
	key.len = sizeof(folder);
	value.data = imap;
	value.len = 0;
	chash_set(session_hash, &key, &value, NULL);
	
	param.imap = imap;
	param.server = server;
	param.port = port;
	param.account = folder->account;

	refresh_resolvers();
	threaded_run(folder, &param, &result, connect_ssl_run);

	if ((result.error == MAILIMAP_NO_ERROR_AUTHENTICATED ||
	     result.error == MAILIMAP_NO_ERROR_NON_AUTHENTICATED) && !etpan_skip_ssl_cert_check) {
		cert_len = (int)mailstream_ssl_get_certificate(imap->imap_stream, &certificate);
		if (etpan_certificate_check(certificate, cert_len, &param) < 0)
			return -1;
		if (certificate) 
			free(certificate); 
	}
	debug_print("connect %d with imap %p\n", result.error, imap);
	
	return result.error;
}

struct capa_param {
	mailimap * imap;
};

struct capa_result {
	int error;
	struct mailimap_capability_data *caps;
};

static void capability_run(struct etpan_thread_op * op)
{
	int r;
	struct capa_param * param;
	struct capa_result * result;
	struct mailimap_capability_data *caps;

	param = op->param;
	result = op->result;
	
	CHECK_IMAP();

	r = mailimap_capability(param->imap, &caps);
	
	result->error = r;
	result->caps = (r == 0 ? caps : NULL);
}


struct mailimap_capability_data * imap_threaded_capability(Folder *folder, int *ok)
{
	struct capa_param param;
	struct capa_result result;
	mailimap *imap;
	
	imap = get_imap(folder);
	
	param.imap = imap;
	
	threaded_run(folder, &param, &result, capability_run);
	
	debug_print("capa %d\n", result.error);
	
	if (ok)
		*ok = result.error;

	return result.caps;
	
}
	
struct disconnect_param {
	mailimap * imap;
};

struct disconnect_result {
	int error;
};

static void disconnect_run(struct etpan_thread_op * op)
{
	int r;
	struct disconnect_param * param;
	struct disconnect_result * result;
	
	param = op->param;
	result = op->result;
	
	CHECK_IMAP();

	r = mailimap_logout(param->imap);
	
	result->error = r;
}

void imap_threaded_disconnect(Folder * folder)
{
	struct connect_param param;
	struct connect_result result;
	mailimap * imap;
	
	imap = get_imap(folder);
	if (imap == NULL) {
		debug_print("was disconnected\n");
		return;
	}
	
	param.imap = imap;
	
	threaded_run(folder, &param, &result, disconnect_run);
	
	if (imap == get_imap(folder)) {
		debug_print("deleting old imap %p\n", imap);
		delete_imap(folder, imap);
	} else {
		debug_print("imap already deleted %p\n", imap);
	}
	
	debug_print("disconnect ok\n");
}


struct list_param {
	mailimap * imap;
	const char * base;
	const char * wildcard;
	gboolean sub_only;
};

struct list_result {
	int error;
	clist * list;
};

static void list_run(struct etpan_thread_op * op)
{
	struct list_param * param;
	struct list_result * result;
	int r;
	clist * list;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	list = NULL;
	
	if (param->base == NULL || param->wildcard == NULL) {
		result->list = list;
		result->error = -1;
		debug_print("no base or wildcard (%p %p)\n", param->base, param->wildcard);
		return;
	}
	if (param->sub_only)
		r = mailimap_lsub(param->imap, param->base,
			  param->wildcard, &list);
	else
		r = mailimap_list(param->imap, param->base,
			  param->wildcard, &list);
	result->error = r;
	result->list = list;
	debug_print("imap list run - end\n");
}

int imap_threaded_list(Folder * folder, const char * base,
		       const char * wildcard,
		       clist ** p_result)
{
	struct list_param param;
	struct list_result result;
	
	debug_print("imap list - begin\n");
	
	param.imap = get_imap(folder);
	param.base = base;
	param.wildcard = wildcard;
	param.sub_only = FALSE;

	threaded_run(folder, &param, &result, list_run);
	
	* p_result = result.list;
	
	debug_print("imap list - end %p\n", result.list);
	
	return result.error;
}

int imap_threaded_lsub(Folder * folder, const char * base,
		       const char * wildcard,
		       clist ** p_result)
{
	struct list_param param;
	struct list_result result;
	
	debug_print("imap lsub - begin\n");
	
	param.imap = get_imap(folder);
	param.base = base;
	param.wildcard = wildcard;
	param.sub_only = TRUE;
	
	threaded_run(folder, &param, &result, list_run);
	
	* p_result = result.list;
	
	debug_print("imap lsub - end %p\n", result.list);
	
	return result.error;
}

struct subscribe_param {
	mailimap * imap;
	const char * mb;
	gboolean subscribe;
};

struct subscribe_result {
	int error;
};

static void subscribe_run(struct etpan_thread_op * op)
{
	struct subscribe_param * param;
	struct subscribe_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	if (param->mb == NULL) {
		result->error = -1;
		debug_print("no mb\n");
		return;
	}
	if (param->subscribe)
		r = mailimap_subscribe(param->imap, param->mb);
	else
		r = mailimap_unsubscribe(param->imap, param->mb);
	result->error = r;
	debug_print("imap %ssubscribe run - end %d\n", param->subscribe?"":"un", r);
}

int imap_threaded_subscribe(Folder * folder, const char * mb,
		       gboolean subscribe)
{
	struct subscribe_param param;
	struct subscribe_result result;
	
	debug_print("imap list - begin\n");
	
	param.imap = get_imap(folder);
	param.mb = mb;
	param.subscribe = subscribe;

	threaded_run(folder, &param, &result, subscribe_run);
	
	return result.error;
}

struct login_param {
	mailimap * imap;
	const char * login;
	const char * password;
	const char * type;
	const char * server;
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

	CHECK_IMAP();

#ifdef DISABLE_LOG_DURING_LOGIN
	old_debug = mailstream_debug;
	mailstream_debug = 0;
#endif
	if (!strcmp(param->type, "LOGIN"))
		r = mailimap_login(param->imap,
			   param->login, param->password);
	else if (!strcmp(param->type, "GSSAPI"))
		r = mailimap_authenticate(param->imap,
			param->type, param->server, NULL, NULL,
			param->login, param->login,
			param->password, NULL);
	else 
		r = mailimap_authenticate(param->imap,
			param->type, NULL, NULL, NULL,
			param->login, param->login,
			param->password, NULL);
#ifdef DISABLE_LOG_DURING_LOGIN
	mailstream_debug = old_debug;
#endif
	
	result->error = r;
	if (param->imap->imap_response)
		imap_logger_cmd(0, param->imap->imap_response, strlen(param->imap->imap_response));
	debug_print("imap login run - end %i\n", r);
}

int imap_threaded_login(Folder * folder,
			const char * login, const char * password,
			const char * type)
{
	struct login_param param;
	struct login_result result;
	
	debug_print("imap login - begin\n");
	
	param.imap = get_imap(folder);
	param.login = login;
	param.password = password;
	param.type = type;
	if (folder && folder->account)
		param.server = folder->account->recv_server;
	else
		param.server = NULL;

	threaded_run(folder, &param, &result, login_run);
	
	debug_print("imap login - end\n");
	
	return result.error;
}


struct status_param {
	mailimap * imap;
	const char * mb;
	struct mailimap_status_att_list * status_att_list;
};

struct status_result {
	int error;
	struct mailimap_mailbox_data_status * data_status;
};

static void status_run(struct etpan_thread_op * op)
{
	struct status_param * param;
	struct status_result * result;
	int r;
	
	param = op->param;
	result = op->result;
	
	CHECK_IMAP();

	r = mailimap_status(param->imap, param->mb,
			    param->status_att_list,
			    &result->data_status);
	
	result->error = r;
	debug_print("imap status run - end %i\n", r);
}

int imap_threaded_status(Folder * folder, const char * mb,
			 struct mailimap_mailbox_data_status ** data_status,
			 guint mask)
{
	struct status_param param;
	struct status_result result;
	struct mailimap_status_att_list * status_att_list;
	
	debug_print("imap status - begin\n");
	
	status_att_list = mailimap_status_att_list_new_empty();
	if (mask & 1 << 0) {
		mailimap_status_att_list_add(status_att_list,
				     MAILIMAP_STATUS_ATT_MESSAGES);
	}
	if (mask & 1 << 1) {
		mailimap_status_att_list_add(status_att_list,
				     MAILIMAP_STATUS_ATT_RECENT);
	}
	if (mask & 1 << 2) {
		mailimap_status_att_list_add(status_att_list,
				     MAILIMAP_STATUS_ATT_UIDNEXT);
	}
	if (mask & 1 << 3) {
		mailimap_status_att_list_add(status_att_list,
				     MAILIMAP_STATUS_ATT_UIDVALIDITY);
	}
	if (mask & 1 << 4) {
		mailimap_status_att_list_add(status_att_list,
				     MAILIMAP_STATUS_ATT_UNSEEN);
	}
	param.imap = get_imap(folder);
	param.mb = mb;
	param.status_att_list = status_att_list;
	
	threaded_run(folder, &param, &result, status_run);
	
	debug_print("imap status - end\n");
	
	* data_status = result.data_status;
	
	mailimap_status_att_list_free(status_att_list);
	
	return result.error;
}



struct noop_param {
	mailimap * imap;
};

struct noop_result {
	int error;
};

static void noop_run(struct etpan_thread_op * op)
{
	struct noop_param * param;
	struct noop_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_noop(param->imap);
	
	result->error = r;
	debug_print("imap noop run - end %i\n", r);
}

int imap_threaded_noop(Folder * folder, unsigned int * p_exists, 
		       unsigned int *p_recent, 
		       unsigned int *p_expunge,
		       unsigned int *p_unseen,
		       unsigned int *p_uidnext,
		       unsigned int *p_uidval)
{
	struct noop_param param;
	struct noop_result result;
	mailimap * imap;
	
	debug_print("imap noop - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;

	threaded_run(folder, &param, &result, noop_run);
	
	if (result.error == 0 && imap && imap->imap_selection_info != NULL) {
		* p_exists = imap->imap_selection_info->sel_exists;
		* p_recent = imap->imap_selection_info->sel_recent;
		* p_unseen = imap->imap_selection_info->sel_unseen;
		* p_uidnext = imap->imap_selection_info->sel_uidnext;
		* p_uidval = imap->imap_selection_info->sel_uidvalidity;
	} else {
		* p_exists = 0;
		* p_recent = 0;
		* p_unseen = 0;
		* p_uidnext = 0;
		* p_uidval = 0;
	}
	if (result.error == 0 && imap && imap->imap_response_info != NULL &&
	    imap->imap_response_info->rsp_expunged != NULL) {
		* p_expunge = clist_count(imap->imap_response_info->rsp_expunged);
	} else {
		* p_expunge = 0;
	}	
	debug_print("imap noop - end [EXISTS %d RECENT %d EXPUNGE %d UNSEEN %d UIDNEXT %d UIDVAL %d]\n",
		*p_exists, *p_recent, *p_expunge, *p_unseen,
		*p_uidnext, *p_uidval);
	
	return result.error;
}


struct starttls_result {
	int error;
};

static void starttls_run(struct etpan_thread_op * op)
{
	struct connect_param * param;
	struct starttls_result * result;
	int r;

	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_starttls(param->imap);
	
	result->error = r;
	debug_print("imap starttls run - end %i\n", r);
	
	if (r == 0) {
		mailimap *imap = param->imap;
		mailstream_low *plain_low = NULL;
		mailstream_low *tls_low = NULL;
		int fd = -1;
		
		plain_low = mailstream_get_low(imap->imap_stream);
		fd = mailstream_low_get_fd(plain_low);
		if (fd == -1) {
			debug_print("imap starttls run - can't get fd\n");
			result->error = MAILIMAP_ERROR_STREAM;
			return;
		}

		tls_low = mailstream_low_tls_open_with_callback(fd, connect_ssl_context_cb, param->account);
		if (tls_low == NULL) {
			debug_print("imap starttls run - can't tls_open\n");
			result->error = MAILIMAP_ERROR_STREAM;
			return;
		}
		mailstream_low_free(plain_low);
		mailstream_set_low(imap->imap_stream, tls_low);
	}
}

int imap_threaded_starttls(Folder * folder, const gchar *host, int port)
{
	struct connect_param param;
	struct starttls_result result;
	int cert_len;
	unsigned char *certificate = NULL;
	
	debug_print("imap starttls - begin\n");
	
	param.imap = get_imap(folder);
	param.server = host;
	param.port = port;
	param.account = folder->account;

	threaded_run(folder, &param, &result, starttls_run);
	
	debug_print("imap starttls - end\n");

	if (result.error == 0 && param.imap && !etpan_skip_ssl_cert_check) {
		cert_len = (int)mailstream_ssl_get_certificate(param.imap->imap_stream, &certificate);
		if (etpan_certificate_check(certificate, cert_len, &param) < 0)
			result.error = MAILIMAP_ERROR_STREAM;
		if (certificate) 
			free(certificate); 
	}	
	return result.error;
}



struct create_param {
	mailimap * imap;
	const char * mb;
};

struct create_result {
	int error;
};

static void create_run(struct etpan_thread_op * op)
{
	struct create_param * param;
	struct create_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_create(param->imap, param->mb);
	
	result->error = r;
	debug_print("imap create run - end %i\n", r);
}

int imap_threaded_create(Folder * folder, const char * mb)
{
	struct create_param param;
	struct create_result result;
	
	debug_print("imap create - begin\n");
	
	param.imap = get_imap(folder);
	param.mb = mb;
	
	threaded_run(folder, &param, &result, create_run);
	
	debug_print("imap create - end\n");
	
	return result.error;
}




struct rename_param {
	mailimap * imap;
	const char * mb;
	const char * new_name;
};

struct rename_result {
	int error;
};

static void rename_run(struct etpan_thread_op * op)
{
	struct rename_param * param;
	struct rename_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_rename(param->imap, param->mb, param->new_name);
	
	result->error = r;
	debug_print("imap rename run - end %i\n", r);
}

int imap_threaded_rename(Folder * folder,
			 const char * mb, const char * new_name)
{
	struct rename_param param;
	struct rename_result result;
	
	debug_print("imap rename - begin\n");
	
	param.imap = get_imap(folder);
	param.mb = mb;
	param.new_name = new_name;
	
	threaded_run(folder, &param, &result, rename_run);
	
	debug_print("imap rename - end\n");
	
	return result.error;
}




struct delete_param {
	mailimap * imap;
	const char * mb;
};

struct delete_result {
	int error;
};

static void delete_run(struct etpan_thread_op * op)
{
	struct delete_param * param;
	struct delete_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_delete(param->imap, param->mb);
	
	result->error = r;
	debug_print("imap delete run - end %i\n", r);
}

int imap_threaded_delete(Folder * folder, const char * mb)
{
	struct delete_param param;
	struct delete_result result;
	
	debug_print("imap delete - begin\n");
	
	param.imap = get_imap(folder);
	param.mb = mb;
	
	threaded_run(folder, &param, &result, delete_run);
	
	debug_print("imap delete - end\n");
	
	return result.error;
}



struct select_param {
	mailimap * imap;
	const char * mb;
};

struct select_result {
	int error;
};

static void select_run(struct etpan_thread_op * op)
{
	struct select_param * param;
	struct select_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_select(param->imap, param->mb);
	
	result->error = r;
	debug_print("imap select run - end %i\n", r);
}

int imap_threaded_select(Folder * folder, const char * mb,
			 gint * exists, gint * recent, gint * unseen,
			 guint32 * uid_validity,gint *can_create_flags,
			 GSList **ok_flags)
{
	struct select_param param;
	struct select_result result;
	mailimap * imap;

	debug_print("imap select - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.mb = mb;
	
	threaded_run(folder, &param, &result, select_run);
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	if (!imap || imap->imap_selection_info == NULL)
		return MAILIMAP_ERROR_PARSE;
	
	* exists = imap->imap_selection_info->sel_exists;
	* recent = imap->imap_selection_info->sel_recent;
	* unseen = imap->imap_selection_info->sel_unseen;
	* uid_validity = imap->imap_selection_info->sel_uidvalidity;
	* can_create_flags = FALSE;

	if (imap->imap_selection_info->sel_perm_flags) {
		GSList *t_flags = NULL;
		clistiter *cur = NULL;
		if (imap->imap_selection_info->sel_perm_flags)
			cur = clist_begin(imap->imap_selection_info->sel_perm_flags);

		for (; cur; cur = clist_next(cur)) {
			struct mailimap_flag_perm *flag = (struct mailimap_flag_perm *)clist_content(cur);
			if (flag->fl_type == MAILIMAP_FLAG_PERM_ALL)
				*can_create_flags = TRUE;
			else if (flag->fl_flag && 
					flag->fl_flag->fl_type == 6 &&
					!strcmp(flag->fl_flag->fl_data.fl_extension, "*"))
				*can_create_flags = TRUE; 
			if (flag->fl_flag && ok_flags) {
				MsgPermFlags c_flag = 0;
				switch (flag->fl_flag->fl_type) {
				case MAILIMAP_FLAG_ANSWERED:
					c_flag = IMAP_FLAG_ANSWERED;
					break;
				case MAILIMAP_FLAG_FLAGGED:
					c_flag = IMAP_FLAG_FLAGGED;
					break;
				case MAILIMAP_FLAG_DELETED:
					c_flag = IMAP_FLAG_DELETED;
					break;
				case MAILIMAP_FLAG_DRAFT:
					c_flag = IMAP_FLAG_DRAFT;
					break;
				case MAILIMAP_FLAG_SEEN:
					c_flag = IMAP_FLAG_SEEN;
					break;
				case MAILIMAP_FLAG_KEYWORD:
					if (!strcasecmp(flag->fl_flag->fl_data.fl_keyword, RTAG_FORWARDED))
						c_flag = IMAP_FLAG_FORWARDED;
					if (!strcasecmp(flag->fl_flag->fl_data.fl_keyword, RTAG_JUNK))
						c_flag = IMAP_FLAG_SPAM;
					if (!strcasecmp(flag->fl_flag->fl_data.fl_keyword, RTAG_NON_JUNK) ||
					    !strcasecmp(flag->fl_flag->fl_data.fl_keyword, RTAG_NO_JUNK) ||
					    !strcasecmp(flag->fl_flag->fl_data.fl_keyword, RTAG_NOT_JUNK))
						c_flag = IMAP_FLAG_HAM;
					break;
				default:
					break;
				}
				if (c_flag != 0) {
					t_flags = g_slist_prepend(t_flags, 
						GUINT_TO_POINTER(c_flag));
				}
			}
		}
		if (ok_flags)
			*ok_flags = t_flags;
	}
	debug_print("imap select - end\n");
	
	return result.error;
}

static void close_run(struct etpan_thread_op * op)
{
	struct select_param * param;
	struct select_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_close(param->imap);
	
	result->error = r;
	debug_print("imap close run - end %i\n", r);
}

int imap_threaded_close(Folder * folder)
{
	struct select_param param;
	struct select_result result;
	mailimap * imap;
	
	debug_print("imap close - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	
	threaded_run(folder, &param, &result, close_run);
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	debug_print("imap close - end\n");
	
	return result.error;
}

struct examine_param {
	mailimap * imap;
	const char * mb;
};

struct examine_result {
	int error;
};

static void examine_run(struct etpan_thread_op * op)
{
	struct examine_param * param;
	struct examine_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_examine(param->imap, param->mb);
	
	result->error = r;
	debug_print("imap examine run - end %i\n", r);
}

int imap_threaded_examine(Folder * folder, const char * mb,
			  gint * exists, gint * recent, gint * unseen,
			  guint32 * uid_validity)
{
	struct examine_param param;
	struct examine_result result;
	mailimap * imap;
	
	debug_print("imap examine - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.mb = mb;
	
	threaded_run(folder, &param, &result, examine_run);
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	if (!imap || imap->imap_selection_info == NULL)
		return MAILIMAP_ERROR_PARSE;
	
	* exists = imap->imap_selection_info->sel_exists;
	* recent = imap->imap_selection_info->sel_recent;
	* unseen = imap->imap_selection_info->sel_unseen;
	* uid_validity = imap->imap_selection_info->sel_uidvalidity;
	
	debug_print("imap examine - end\n");
	
	return result.error;
}




struct search_param {
	mailimap * imap;
	int type;
	struct mailimap_set * set;
	IMAPSearchKey* key;
};

struct search_result {
	int error;
	clist * search_result;
};

static struct mailimap_set_item *sc_mailimap_set_item_copy(struct mailimap_set_item *orig)
{
	return mailimap_set_item_new(orig->set_first, orig->set_last);
}

static struct mailimap_set *sc_mailimap_set_copy(struct mailimap_set *orig)
{
	clist *list = orig ? orig->set_list : NULL;
	clist *newlist = clist_new();
	clistiter *cur;
	
	if (!orig)
		return NULL;
	for (cur = clist_begin(list); cur; cur = clist_next(cur))
		clist_append(newlist, 
			sc_mailimap_set_item_copy(
			(struct mailimap_set_item *)clist_content(cur)));
	return mailimap_set_new(newlist);
}

static void search_run(struct etpan_thread_op * op)
{
	struct search_param * param;
	struct search_result * result;
	int r;
	struct mailimap_search_key * key = NULL;
	struct mailimap_search_key * uid_key = NULL;
	struct mailimap_search_key * search_type_key = NULL;
	clist * search_result;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	/* we copy the mailimap_set because freeing the key is recursive */
	if (param->set != NULL) {
		uid_key = mailimap_search_key_new_uid(sc_mailimap_set_copy(param->set));
	} else if (param->type == IMAP_SEARCH_TYPE_SIMPLE) {
		uid_key = mailimap_search_key_new_all();
	}
	switch (param->type) {
	case IMAP_SEARCH_TYPE_SIMPLE:
		search_type_key = NULL;
		break;
	case IMAP_SEARCH_TYPE_SEEN:
		search_type_key = imap_search_new(IMAP_SEARCH_CRITERIA_READ, NULL, NULL, 0);
		break;
	case IMAP_SEARCH_TYPE_UNSEEN:
		search_type_key = imap_search_new(IMAP_SEARCH_CRITERIA_UNREAD, NULL, NULL, 0);
		break;
	case IMAP_SEARCH_TYPE_ANSWERED:
		search_type_key = imap_search_new(IMAP_SEARCH_CRITERIA_REPLIED, NULL, NULL, 0);
		break;
	case IMAP_SEARCH_TYPE_FLAGGED:
		search_type_key = imap_search_new(IMAP_SEARCH_CRITERIA_MARKED, NULL, NULL, 0);
		break;
	case IMAP_SEARCH_TYPE_DELETED:
		search_type_key = imap_search_new(IMAP_SEARCH_CRITERIA_DELETED, NULL, NULL, 0);
		break;
	case IMAP_SEARCH_TYPE_FORWARDED:
		search_type_key = imap_search_new(IMAP_SEARCH_CRITERIA_TAG, NULL, RTAG_FORWARDED, 0);
		break;
	case IMAP_SEARCH_TYPE_SPAM:
		search_type_key = imap_search_new(IMAP_SEARCH_CRITERIA_TAG, NULL, RTAG_JUNK, 0);
		break;
	case IMAP_SEARCH_TYPE_KEYED:
		search_type_key = param->key;
		break;
	}
	
	if (search_type_key != NULL) {
		if (param->set != NULL) {
			key = mailimap_search_key_new_multiple_empty();
			mailimap_search_key_multiple_add(key, search_type_key);
			mailimap_search_key_multiple_add(key, uid_key);
		} else {
			key = search_type_key;
		}
	} else if (uid_key != NULL) {
		key = uid_key;
	}
	
	if (key == NULL) {
		g_warning("no key!");
		result = op->result;
		result->error = -1;
		result->search_result = NULL;
	} else {
		mailstream_logger = imap_logger_uid;

		r = mailimap_uid_search(param->imap, "UTF-8", key, &search_result);

		mailstream_logger = imap_logger_cmd;

		/* free the key (with the imapset) */
		mailimap_search_key_free(key);

		result->error = r;
		result->search_result = search_result;
	}
	debug_print("imap search run - end %i\n", result->error);
}

int imap_threaded_search(Folder * folder, int search_type, IMAPSearchKey* key,
			 struct mailimap_set * set, clist ** search_result)
{
	struct search_param param;
	struct search_result result;
	mailimap * imap;
	
	debug_print("imap search - begin\n");

	imap = get_imap(folder);
	param.imap = imap;
	param.set = set;
	param.type = search_type;
	param.key = key;

	threaded_run(folder, &param, &result, search_run);
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	debug_print("imap search - end\n");
	
	* search_result = result.search_result;
	
	return result.error;
}


struct _IMAPSearchKey {
	struct mailimap_search_key* key;
};

IMAPSearchKey*	imap_search_new(gint		 criteria, 
				const gchar	*header,
				const gchar	*expr,
				int		 value)
{
	char* sk_bcc = NULL;
	struct mailimap_date* sk_before = NULL;
	char* sk_body = NULL;
	char* sk_cc = NULL;
	char* sk_from = NULL;
	char* sk_keyword = NULL;
	struct mailimap_date* sk_on = NULL;
	struct mailimap_date* sk_since = NULL;
	char* sk_subject = NULL;
	char* sk_text = NULL;
	char* sk_to = NULL;
	char* sk_unkeyword = NULL;
	char* sk_header_name = NULL;
	char* sk_header_value = NULL;
	uint32_t sk_larger = 0;
	struct mailimap_search_key* sk_not = NULL;
	struct mailimap_search_key* sk_or1 = NULL;
	struct mailimap_search_key* sk_or2 = NULL;
	struct mailimap_date* sk_sentbefore = NULL;
	struct mailimap_date* sk_senton = NULL;
	struct mailimap_date* sk_sentsince = NULL;
	uint32_t sk_smaller = 0;
	struct mailimap_set* sk_uid = NULL;
	struct mailimap_set* sk_set = NULL;
	clist* sk_multiple = NULL;
	int etpan_matcher_type;

	switch (criteria) {
	case IMAP_SEARCH_CRITERIA_ALL: etpan_matcher_type = MAILIMAP_SEARCH_KEY_ALL; break;
	case IMAP_SEARCH_CRITERIA_READ: etpan_matcher_type = MAILIMAP_SEARCH_KEY_SEEN; break;
	case IMAP_SEARCH_CRITERIA_UNREAD: etpan_matcher_type = MAILIMAP_SEARCH_KEY_UNSEEN; break;
	case IMAP_SEARCH_CRITERIA_NEW: etpan_matcher_type = MAILIMAP_SEARCH_KEY_NEW; break;
	case IMAP_SEARCH_CRITERIA_MARKED: etpan_matcher_type = MAILIMAP_SEARCH_KEY_FLAGGED; break;
	case IMAP_SEARCH_CRITERIA_REPLIED: etpan_matcher_type = MAILIMAP_SEARCH_KEY_ANSWERED; break;
	case IMAP_SEARCH_CRITERIA_DELETED: etpan_matcher_type = MAILIMAP_SEARCH_KEY_DELETED; break;

	case IMAP_SEARCH_CRITERIA_TAG:
		sk_keyword = strdup(expr);
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_KEYWORD;
		break;

	case IMAP_SEARCH_CRITERIA_SUBJECT:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_SUBJECT;
		sk_subject = strdup(expr);
		break;

	case IMAP_SEARCH_CRITERIA_TO:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_TO;
		sk_to = strdup(expr);
		break;

	case IMAP_SEARCH_CRITERIA_CC:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_CC;
		sk_cc = strdup(expr);
		break;

	case IMAP_SEARCH_CRITERIA_AGE_GREATER:
	case IMAP_SEARCH_CRITERIA_AGE_LOWER:
		{
			struct tm tm;
			time_t limit = time(NULL) - 60 * 60 * 24 * value;

			tzset();
			localtime_r(&limit, &tm);
			if (criteria == IMAP_SEARCH_CRITERIA_AGE_GREATER) {
				etpan_matcher_type = MAILIMAP_SEARCH_KEY_SENTBEFORE;
				sk_sentbefore = mailimap_date_new(tm.tm_mday, tm.tm_mon, tm.tm_year + 1900);
			} else {
				etpan_matcher_type = MAILIMAP_SEARCH_KEY_SENTSINCE;
				sk_sentsince = mailimap_date_new(tm.tm_mday, tm.tm_mon, tm.tm_year + 1900);
			}
			break;
		}

	case IMAP_SEARCH_CRITERIA_BODY:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_BODY;
		sk_body = strdup(expr);
		break;

	case IMAP_SEARCH_CRITERIA_MESSAGE:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_TEXT;
		sk_text = strdup(expr);
		break;

	case IMAP_SEARCH_CRITERIA_HEADER:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_HEADER;
		sk_header_name = strdup(header);
		sk_header_value = strdup(expr);
		break;

	case IMAP_SEARCH_CRITERIA_FROM:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_FROM;
		sk_from = strdup(expr);
		break;

	case IMAP_SEARCH_CRITERIA_SIZE_GREATER:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_LARGER;
		sk_larger = value;
		break;

	case IMAP_SEARCH_CRITERIA_SIZE_SMALLER:
		etpan_matcher_type = MAILIMAP_SEARCH_KEY_SMALLER;
		sk_smaller = value;
		break;

	default:
		return NULL;
	}

	return mailimap_search_key_new(etpan_matcher_type,
		sk_bcc, sk_before, sk_body, sk_cc, sk_from, sk_keyword,
		sk_on, sk_since, sk_subject, sk_text, sk_to,
		sk_unkeyword, sk_header_name,sk_header_value, sk_larger,
		sk_not, sk_or1, sk_or2, sk_sentbefore, sk_senton,
		sk_sentsince, sk_smaller, sk_uid, sk_set, sk_multiple);
}

IMAPSearchKey* imap_search_not(IMAPSearchKey* key)
{
	return mailimap_search_key_new_not(key);
}

IMAPSearchKey* imap_search_or(IMAPSearchKey* l, IMAPSearchKey* r)
{
	return mailimap_search_key_new_or(l, r);
}

IMAPSearchKey* imap_search_and(IMAPSearchKey* l, IMAPSearchKey* r)
{
	IMAPSearchKey* result = mailimap_search_key_new_multiple_empty();
	mailimap_search_key_multiple_add(result, l);
	mailimap_search_key_multiple_add(result, r);

	return result;
}

void imap_search_free(IMAPSearchKey* key)
{
	if (!key)
	    return;

	mailimap_search_key_free(key);
}



static int imap_get_msg_att_info(struct mailimap_msg_att * msg_att,
				 uint32_t * puid,
				 char ** pheaders,
				 size_t * pref_size,
				 struct mailimap_msg_att_dynamic ** patt_dyn);

static int
result_to_uid_list(clist * fetch_result, carray ** result)
{
	clistiter * cur = NULL;
	int r;
	int res;
	carray * tab;
	
	tab = carray_new(128);
	if (tab == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto err;
	}
	
	if (fetch_result)
		cur = clist_begin(fetch_result);

	for(; cur != NULL ; cur = clist_next(cur)) {
		struct mailimap_msg_att * msg_att;
		uint32_t uid;
		uint32_t * puid;
		
		msg_att = clist_content(cur);
		
		uid = 0;
		imap_get_msg_att_info(msg_att, &uid, NULL, NULL, NULL);
		
		puid = malloc(sizeof(* puid));
		if (puid == NULL) {
			res = MAILIMAP_ERROR_MEMORY;
			goto free_list;
		}
		* puid = uid;
			
		r = carray_add(tab, puid, NULL);
		if (r < 0) {
			free(puid);
			res = MAILIMAP_ERROR_MEMORY;
			goto free_list;
		}
	}
		
	* result = tab;

	return MAILIMAP_NO_ERROR;
  
 free_list:
	imap_fetch_uid_list_free(tab);
 err:
	return res;
}

static int imap_get_messages_list(mailimap * imap,
				  uint32_t first_index,
				  carray ** result)
{
	carray * env_list;
	int r;
	struct mailimap_fetch_att * fetch_att;
	struct mailimap_fetch_type * fetch_type;
	struct mailimap_set * set;
	clist * fetch_result;
	int res;
	
	set = mailimap_set_new_interval(first_index, 0);
	if (set == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto err;
	}

	fetch_type = mailimap_fetch_type_new_fetch_att_list_empty();
	if (fetch_type == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto free_set;
	}

	fetch_att = mailimap_fetch_att_new_uid();
	if (fetch_att == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto free_fetch_type;
	}

	r = mailimap_fetch_type_new_fetch_att_list_add(fetch_type, fetch_att);
	if (r != MAILIMAP_NO_ERROR) {
		mailimap_fetch_att_free(fetch_att);
		res = MAILIMAP_ERROR_MEMORY;
		goto free_fetch_type;
	}

	mailstream_logger = imap_logger_fetch;
	
	r = mailimap_uid_fetch(imap, set,
			       fetch_type, &fetch_result);

	mailstream_logger = imap_logger_cmd;
	mailimap_fetch_type_free(fetch_type);
	mailimap_set_free(set);

	if (r != MAILIMAP_NO_ERROR) {
		res = r;
		goto err;
	}

	env_list = NULL;
	r = result_to_uid_list(fetch_result, &env_list);
	mailimap_fetch_list_free(fetch_result);
	
	* result = env_list;

	return MAILIMAP_NO_ERROR;

 free_fetch_type:
	mailimap_fetch_type_free(fetch_type);
 free_set:
	mailimap_set_free(set);
 err:
	return res;
}




struct fetch_uid_param {
	mailimap * imap;
	uint32_t first_index;
};

struct fetch_uid_result {
	int error;
	carray * fetch_result;
};

static void fetch_uid_run(struct etpan_thread_op * op)
{
	struct fetch_uid_param * param;
	struct fetch_uid_result * result;
	carray * fetch_result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	fetch_result = NULL;
	mailstream_logger = imap_logger_noop;
	log_print(LOG_PROTOCOL, "IMAP4- [fetching UIDs...]\n");

	r = imap_get_messages_list(param->imap, param->first_index,
				   &fetch_result);
	
	mailstream_logger = imap_logger_cmd;

	result->error = r;
	result->fetch_result = fetch_result;
	debug_print("imap fetch_uid run - end %i\n", r);
}

int imap_threaded_fetch_uid(Folder * folder, uint32_t first_index,
			    carray ** fetch_result)
{
	struct fetch_uid_param param;
	struct fetch_uid_result result;
	mailimap * imap;
	
	debug_print("imap fetch_uid - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.first_index = first_index;
	
	threaded_run(folder, &param, &result, fetch_uid_run);
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	debug_print("imap fetch_uid - end\n");
	
	* fetch_result = result.fetch_result;
	
	return result.error;
}


void imap_fetch_uid_list_free(carray * uid_list)
{
	unsigned int i;
	
	for(i = 0 ; i < carray_count(uid_list) ; i ++) {
		uint32_t * puid;
		
		puid = carray_get(uid_list, i);
		free(puid);
	}
	carray_free(uid_list);
}




static int imap_flags_to_flags(struct mailimap_msg_att_dynamic * att_dyn, GSList **tags);

static int
result_to_uid_flags_list(clist * fetch_result, carray ** result)
{
	clistiter * cur = NULL;
	int r;
	int res;
	carray * tab;
	GSList *tags = NULL;

	tab = carray_new(128);
	if (tab == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto err;
	}

	if (fetch_result)
		cur = clist_begin(fetch_result);

	for(; cur != NULL ; cur = clist_next(cur)) {
		struct mailimap_msg_att * msg_att;
		uint32_t uid;
		uint32_t * puid;
		struct mailimap_msg_att_dynamic * att_dyn;
		int flags;
		int * pflags;
		
		tags = NULL;

		msg_att = clist_content(cur);
		
		uid = 0;
		att_dyn = NULL;
		imap_get_msg_att_info(msg_att, &uid, NULL, NULL, &att_dyn);
		if (uid == 0)
			continue;
		if (att_dyn == NULL)
			continue;
		
		flags = imap_flags_to_flags(att_dyn, &tags);
		
		puid = malloc(sizeof(* puid));
		if (puid == NULL) {
			res = MAILIMAP_ERROR_MEMORY;
			goto free_list;
		}
		* puid = uid;
		
		r = carray_add(tab, puid, NULL);
		if (r < 0) {
			free(puid);
			res = MAILIMAP_ERROR_MEMORY;
			goto free_list;
		}
		pflags = malloc(sizeof(* pflags));
		if (pflags == NULL) {
			res = MAILIMAP_ERROR_MEMORY;
			goto free_list;
		}
		* pflags = flags;
		r = carray_add(tab, pflags, NULL);
		if (r < 0) {
			free(pflags);
			res = MAILIMAP_ERROR_MEMORY;
			goto free_list;
		}
		r = carray_add(tab, tags, NULL);
		if (r < 0) {
			free(pflags);
			res = MAILIMAP_ERROR_MEMORY;
			goto free_list;
		}
	}
		
	* result = tab;

	return MAILIMAP_NO_ERROR;
  
 free_list:
	imap_fetch_uid_flags_list_free(tab);
	slist_free_strings_full(tags);
 err:
	return res;
}

static int imap_get_messages_flags_list(mailimap * imap,
					uint32_t first_index,
					carray ** result)
{
	carray * env_list;
	int r;
	struct mailimap_fetch_att * fetch_att;
	struct mailimap_fetch_type * fetch_type;
	struct mailimap_set * set;
	clist * fetch_result;
	int res;
	
	set = mailimap_set_new_interval(first_index, 0);
	if (set == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto err;
	}

	fetch_type = mailimap_fetch_type_new_fetch_att_list_empty();
	if (fetch_type == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto free_set;
	}

	fetch_att = mailimap_fetch_att_new_flags();
	if (fetch_att == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto free_fetch_type;
	}
	
	r = mailimap_fetch_type_new_fetch_att_list_add(fetch_type, fetch_att);
	if (r != MAILIMAP_NO_ERROR) {
		mailimap_fetch_att_free(fetch_att);
		res = MAILIMAP_ERROR_MEMORY;
		goto free_fetch_type;
	}
	
	fetch_att = mailimap_fetch_att_new_uid();
	if (fetch_att == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto free_fetch_type;
	}

	r = mailimap_fetch_type_new_fetch_att_list_add(fetch_type, fetch_att);
	if (r != MAILIMAP_NO_ERROR) {
		mailimap_fetch_att_free(fetch_att);
		res = MAILIMAP_ERROR_MEMORY;
		goto free_fetch_type;
	}

	mailstream_logger = imap_logger_fetch;
	
	r = mailimap_uid_fetch(imap, set,
			       fetch_type, &fetch_result);

	mailstream_logger = imap_logger_cmd;
	mailimap_fetch_type_free(fetch_type);
	mailimap_set_free(set);

	if (r != MAILIMAP_NO_ERROR) {
		res = r;
		goto err;
	}

	env_list = NULL;
	r = result_to_uid_flags_list(fetch_result, &env_list);
	mailimap_fetch_list_free(fetch_result);
	
	* result = env_list;

	return MAILIMAP_NO_ERROR;

 free_fetch_type:
	mailimap_fetch_type_free(fetch_type);
 free_set:
	mailimap_set_free(set);
 err:
	return res;
}



static void fetch_uid_flags_run(struct etpan_thread_op * op)
{
	struct fetch_uid_param * param;
	struct fetch_uid_result * result;
	carray * fetch_result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	fetch_result = NULL;
	r = imap_get_messages_flags_list(param->imap, param->first_index,
					 &fetch_result);
	
	result->error = r;
	result->fetch_result = fetch_result;
	debug_print("imap fetch_uid run - end %i\n", r);
}

int imap_threaded_fetch_uid_flags(Folder * folder, uint32_t first_index,
				  carray ** fetch_result)
{
	struct fetch_uid_param param;
	struct fetch_uid_result result;
	mailimap * imap;
	
	debug_print("imap fetch_uid - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.first_index = first_index;
	
	mailstream_logger = imap_logger_noop;
	log_print(LOG_PROTOCOL, "IMAP4- [fetching flags...]\n");

	threaded_run(folder, &param, &result, fetch_uid_flags_run);

	mailstream_logger = imap_logger_cmd;

	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	debug_print("imap fetch_uid - end\n");
	
	* fetch_result = result.fetch_result;
	
	return result.error;
}


void imap_fetch_uid_flags_list_free(carray * uid_flags_list)
{
	unsigned int i;
	
	for(i = 0 ; i < carray_count(uid_flags_list) ; i += 3) {
		void * data;
		
		data = carray_get(uid_flags_list, i);
		free(data);
		data = carray_get(uid_flags_list, i + 1);
		free(data);
	}
	carray_free(uid_flags_list);
}



static int imap_fetch(mailimap * imap,
		      uint32_t msg_index,
		      char ** result,
		      size_t * result_len)
{
	int r;
	struct mailimap_set * set;
	struct mailimap_fetch_att * fetch_att;
	struct mailimap_fetch_type * fetch_type;
	clist * fetch_result;
	struct mailimap_msg_att * msg_att;
	struct mailimap_msg_att_item * msg_att_item;
	char * text;
	size_t text_length;
	int res;
	clistiter * cur;
	struct mailimap_section * section;

	set = mailimap_set_new_single(msg_index);
	if (set == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto err;
	}

	section = mailimap_section_new(NULL);
	if (section == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto free_set;
	}
  
	fetch_att = mailimap_fetch_att_new_body_peek_section(section);
	if (fetch_att == NULL) {
		mailimap_section_free(section);
		res = MAILIMAP_ERROR_MEMORY;
		goto free_set;
	}
  
	fetch_type = mailimap_fetch_type_new_fetch_att(fetch_att);
	if (fetch_type == NULL) {
		res = MAILIMAP_ERROR_MEMORY;
		goto free_fetch_att;
	}

	mailstream_logger = imap_logger_fetch;
	
	r = mailimap_uid_fetch(imap, set,
			       fetch_type, &fetch_result);
  
	mailstream_logger = imap_logger_cmd;
	
	mailimap_fetch_type_free(fetch_type);
	mailimap_set_free(set);
  
	switch (r) {
	case MAILIMAP_NO_ERROR:
		break;
	default:
		return r;
	}
  
	if (fetch_result == NULL || clist_begin(fetch_result) == NULL) {
		mailimap_fetch_list_free(fetch_result);
		return MAILIMAP_ERROR_FETCH;
	}

	msg_att = clist_begin(fetch_result)->data;

	text = NULL;
	text_length = 0;

	if (msg_att->att_list)
		cur = clist_begin(msg_att->att_list);
	else
		cur = NULL;

	for(; cur != NULL ; cur = clist_next(cur)) {
		msg_att_item = clist_content(cur);

		if (msg_att_item->att_type == MAILIMAP_MSG_ATT_ITEM_STATIC) {
			if (msg_att_item->att_data.att_static->att_type ==
			    MAILIMAP_MSG_ATT_BODY_SECTION) {
				text = msg_att_item->att_data.att_static->att_data.att_body_section->sec_body_part;
				/* detach */
				msg_att_item->att_data.att_static->att_data.att_body_section->sec_body_part = NULL;
				text_length =
					msg_att_item->att_data.att_static->att_data.att_body_section->sec_length;
			}
		}
	}

	mailimap_fetch_list_free(fetch_result);

	if (text == NULL)
		return MAILIMAP_ERROR_FETCH;

	* result = text;
	* result_len = text_length;
  
	return MAILIMAP_NO_ERROR;

 free_fetch_att:
	mailimap_fetch_att_free(fetch_att);
 free_set:
	mailimap_set_free(set);
 err:
	return res;
}

static int imap_fetch_header(mailimap * imap,
			     uint32_t msg_index,
			     char ** result,
			     size_t * result_len)
{
  int r;
  struct mailimap_set * set;
  struct mailimap_fetch_att * fetch_att;
  struct mailimap_fetch_type * fetch_type;
  clist * fetch_result;
  struct mailimap_msg_att * msg_att;
  struct mailimap_msg_att_item * msg_att_item;
  char * text;
  size_t text_length;
  int res;
  clistiter * cur;
  struct mailimap_section * section;
  
  set = mailimap_set_new_single(msg_index);
  if (set == NULL) {
    res = MAILIMAP_ERROR_MEMORY;
    goto err;
  }

  section = mailimap_section_new_header();
  if (section == NULL) {
    res = MAILIMAP_ERROR_MEMORY;
    goto free_set;
  }
  
  fetch_att = mailimap_fetch_att_new_body_peek_section(section);
  if (fetch_att == NULL) {
    mailimap_section_free(section);
    res = MAILIMAP_ERROR_MEMORY;
    goto free_set;
  }
  
  fetch_type = mailimap_fetch_type_new_fetch_att(fetch_att);
  if (fetch_type == NULL) {
    res = MAILIMAP_ERROR_MEMORY;
    goto free_fetch_att;
  }

  mailstream_logger = imap_logger_fetch;
  
  r = mailimap_uid_fetch(imap, set, fetch_type, &fetch_result);
  
  mailstream_logger = imap_logger_cmd;
  mailimap_fetch_type_free(fetch_type);
  mailimap_set_free(set);

  switch (r) {
  case MAILIMAP_NO_ERROR:
    break;
  default:
    return r;
  }

  if (fetch_result == NULL || clist_begin(fetch_result) == NULL) {
    mailimap_fetch_list_free(fetch_result);
    return MAILIMAP_ERROR_FETCH;
  }

  msg_att = clist_begin(fetch_result)->data;

  text = NULL;
  text_length = 0;

  if (msg_att->att_list)
     cur = clist_begin(msg_att->att_list);
  else
     cur = NULL;

  for(; cur != NULL ; cur = clist_next(cur)) {
    msg_att_item = clist_content(cur);

    if (msg_att_item->att_type == MAILIMAP_MSG_ATT_ITEM_STATIC) {
      if (msg_att_item->att_data.att_static->att_type ==
	  MAILIMAP_MSG_ATT_BODY_SECTION) {
	text = msg_att_item->att_data.att_static->att_data.att_body_section->sec_body_part;
	msg_att_item->att_data.att_static->att_data.att_body_section->sec_body_part = NULL;
	text_length =
	  msg_att_item->att_data.att_static->att_data.att_body_section->sec_length;
      }
    }
  }

  mailimap_fetch_list_free(fetch_result);

  if (text == NULL)
    return MAILIMAP_ERROR_FETCH;

  * result = text;
  * result_len = text_length;

  return MAILIMAP_NO_ERROR;

 free_fetch_att:
  mailimap_fetch_att_free(fetch_att);
 free_set:
  mailimap_set_free(set);
 err:
  return res;
}



struct fetch_content_param {
	mailimap * imap;
	uint32_t msg_index;
	const char * filename;
	int with_body;
};

struct fetch_content_result {
	int error;
};

static void fetch_content_run(struct etpan_thread_op * op)
{
	struct fetch_content_param * param;
	struct fetch_content_result * result;
	char * content;
	size_t content_size;
	int r;
	int fd;
	FILE * f;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	content = NULL;
	content_size = 0;
	if (param->with_body)
		r = imap_fetch(param->imap, param->msg_index,
			       &content, &content_size);
	else
		r = imap_fetch_header(param->imap, param->msg_index,
				      &content, &content_size);
	
	result->error = r;
	
	if (r == MAILIMAP_NO_ERROR) {
		fd = g_open(param->filename, O_RDWR | O_CREAT, 0600);
		if (fd < 0) {
			result->error = MAILIMAP_ERROR_FETCH;
			goto free;
		}
		
		f = fdopen(fd, "wb");
		if (f == NULL) {
			result->error = MAILIMAP_ERROR_FETCH;
			goto close;
		}
		
		r = fwrite(content, 1, content_size, f);
		if (r < content_size) {
			result->error = MAILIMAP_ERROR_FETCH;
			goto fclose;
		}
		
		r = fclose(f);
		if (r == EOF) {
			result->error = MAILIMAP_ERROR_FETCH;
			goto unlink;
		}
		goto free;
		
	fclose:
		fclose(f);
		goto unlink;
	close:
		close(fd);
	unlink:
		claws_unlink(param->filename);
	
	free:
		/* mmap_string_unref is a simple free in libetpan
		 * when it has MMAP_UNAVAILABLE defined */
		if (mmap_string_unref(content) != 0)
			free(content);
	}
	
	debug_print("imap fetch_content run - end %i\n", result->error);
}

int imap_threaded_fetch_content(Folder * folder, uint32_t msg_index,
				int with_body,
				const char * filename)
{
	struct fetch_content_param param;
	struct fetch_content_result result;
	mailimap * imap;
	
	debug_print("imap fetch_content - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.msg_index = msg_index;
	param.filename = filename;
	param.with_body = with_body;
	
	threaded_run(folder, &param, &result, fetch_content_run);
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	debug_print("imap fetch_content - end\n");
	
	return result.error;
}



static int imap_flags_to_flags(struct mailimap_msg_att_dynamic * att_dyn, GSList **s_tags)
{
	int flags;
	clist * flag_list;
	clistiter * cur;
	GSList *tags = NULL;

	flags = MSG_UNREAD;
	
	flag_list = att_dyn->att_list;
	if (flag_list == NULL)
		return flags;
	
	for(cur = clist_begin(flag_list) ; cur != NULL ;
	    cur = clist_next(cur)) {
		struct mailimap_flag_fetch * flag_fetch;
			
		flag_fetch = clist_content(cur);
		if (flag_fetch->fl_type == MAILIMAP_FLAG_FETCH_RECENT)
			flags |= MSG_NEW;
		else {
			switch (flag_fetch->fl_flag->fl_type) {
			case MAILIMAP_FLAG_ANSWERED:
				flags |= MSG_REPLIED;
				break;
			case MAILIMAP_FLAG_FLAGGED:
				flags |= MSG_MARKED;
				break;
			case MAILIMAP_FLAG_DELETED:
				flags |= MSG_DELETED;
				break;
			case MAILIMAP_FLAG_SEEN:
				flags &= ~MSG_UNREAD;
				flags &= ~MSG_NEW;
				break;
			case MAILIMAP_FLAG_KEYWORD:
				if (!strcasecmp(flag_fetch->fl_flag->fl_data.fl_keyword, RTAG_FORWARDED))
					flags |= MSG_FORWARDED;
				else if (!strcasecmp(flag_fetch->fl_flag->fl_data.fl_keyword, RTAG_JUNK)) 
					flags |= MSG_SPAM;
				else if (!strcasecmp(flag_fetch->fl_flag->fl_data.fl_keyword, RTAG_NON_JUNK) ||
					 !strcasecmp(flag_fetch->fl_flag->fl_data.fl_keyword, RTAG_NO_JUNK) ||
					 !strcasecmp(flag_fetch->fl_flag->fl_data.fl_keyword, RTAG_NOT_JUNK)) 
					flags &= ~MSG_SPAM;
				else if (s_tags)
					tags = g_slist_prepend(tags, g_strdup(flag_fetch->fl_flag->fl_data.fl_keyword));
				break;
			}
		}
	}
	if (s_tags)
		*s_tags = tags;
	return flags;
}

static int imap_get_msg_att_info(struct mailimap_msg_att * msg_att,
				 uint32_t * puid,
				 char ** pheaders,
				 size_t * pref_size,
				 struct mailimap_msg_att_dynamic ** patt_dyn)
{
  clistiter * item_cur;
  uint32_t uid;
  char * headers;
  size_t ref_size;
  struct mailimap_msg_att_dynamic * att_dyn;

  uid = 0;
  headers = NULL;
  ref_size = 0;
  att_dyn = NULL;

  if (msg_att->att_list)
     item_cur = clist_begin(msg_att->att_list);
  else
     item_cur = NULL;
  for(; item_cur != NULL ; item_cur = clist_next(item_cur)) {
    struct mailimap_msg_att_item * item;

    item = clist_content(item_cur);
      
    switch (item->att_type) {
    case MAILIMAP_MSG_ATT_ITEM_STATIC:
      switch (item->att_data.att_static->att_type) {
      case MAILIMAP_MSG_ATT_UID:
	uid = item->att_data.att_static->att_data.att_uid;
	break;

      case MAILIMAP_MSG_ATT_BODY_SECTION:
	if (headers == NULL) {
	  headers = item->att_data.att_static->att_data.att_body_section->sec_body_part;
	}
	break;
      case MAILIMAP_MSG_ATT_RFC822_SIZE:
	      ref_size = item->att_data.att_static->att_data.att_rfc822_size;
	      break;
      }
      break;
      
    case MAILIMAP_MSG_ATT_ITEM_DYNAMIC:
      if (att_dyn == NULL) {
	att_dyn = item->att_data.att_dyn;
      }
      break;
    }
  }

  if (puid != NULL)
    * puid = uid;
  if (pheaders != NULL)
    * pheaders = headers;
  if (pref_size != NULL)
    * pref_size = ref_size;
  if (patt_dyn != NULL)
    * patt_dyn = att_dyn;

  return MAIL_NO_ERROR;
}

static struct imap_fetch_env_info *
fetch_to_env_info(struct mailimap_msg_att * msg_att, GSList **tags)
{
	struct imap_fetch_env_info * info;
	uint32_t uid;
	char * headers;
	size_t size;
	struct mailimap_msg_att_dynamic * att_dyn;

	imap_get_msg_att_info(msg_att, &uid, &headers, &size,
			      &att_dyn);
	
	if (!headers)
		return NULL;
	info = malloc(sizeof(* info));
	info->uid = uid;
	info->headers = strdup(headers);
	info->size = size;
	info->flags = imap_flags_to_flags(att_dyn, tags);
	
	return info;
}

static int
imap_fetch_result_to_envelop_list(clist * fetch_result,
				  carray ** p_env_list)
{
	clistiter * cur;
	carray * env_list;

	env_list = carray_new(16);
  
  	if (fetch_result) {
		for(cur = clist_begin(fetch_result) ; cur != NULL ;
		    cur = clist_next(cur)) {
			struct mailimap_msg_att * msg_att;
			struct imap_fetch_env_info * env_info;
	    		GSList *tags = NULL;

			msg_att = clist_content(cur);

			env_info = fetch_to_env_info(msg_att, &tags);
			if (!env_info)
				return MAILIMAP_ERROR_MEMORY;
			carray_add(env_list, env_info, NULL);
			carray_add(env_list, tags, NULL);
		}
		* p_env_list = env_list;
  	} else {
		* p_env_list = NULL;
	}

	return MAIL_NO_ERROR;
}

static int imap_add_envelope_fetch_att(struct mailimap_fetch_type * fetch_type)
{
	struct mailimap_fetch_att * fetch_att;
	int i;
	char * header;
	clist * hdrlist;
	struct mailimap_header_list * imap_hdrlist;
	struct mailimap_section * section;
	char *headers[] = {
			"Date", "From", "To", "Cc", "Subject", "Message-ID",
			"References", "In-Reply-To", NULL
		};

	hdrlist = clist_new();
	i = 0;
	while (headers[i] != NULL) {
  		header = strdup(headers[i]);
		if (header == NULL || clist_append(hdrlist, header) != 0)
			return MAIL_ERROR_MEMORY;
		++i;
	}
  
	imap_hdrlist = mailimap_header_list_new(hdrlist);
	section = mailimap_section_new_header_fields(imap_hdrlist);
	fetch_att = mailimap_fetch_att_new_body_peek_section(section);
	mailimap_fetch_type_new_fetch_att_list_add(fetch_type, fetch_att);
  
	return MAIL_NO_ERROR;
}

static int imap_add_header_fetch_att(struct mailimap_fetch_type * fetch_type)
{
	struct mailimap_fetch_att * fetch_att;
	struct mailimap_section * section;
	
	section = mailimap_section_new_header();
	fetch_att = mailimap_fetch_att_new_body_peek_section(section);
	mailimap_fetch_type_new_fetch_att_list_add(fetch_type, fetch_att);
	
	return MAIL_NO_ERROR;
}

static int
imap_get_envelopes_list(mailimap * imap, struct mailimap_set * set,
			carray ** p_env_list)
{
	struct mailimap_fetch_att * fetch_att;
	struct mailimap_fetch_type * fetch_type;
	int res;
	clist * fetch_result;
	int r;
	carray * env_list = NULL;
	chashdatum key;
	chashdatum value;
	
	fetch_type = mailimap_fetch_type_new_fetch_att_list_empty();
  
	/* uid */
	fetch_att = mailimap_fetch_att_new_uid();
	r = mailimap_fetch_type_new_fetch_att_list_add(fetch_type, fetch_att);
  
	/* flags */
	fetch_att = mailimap_fetch_att_new_flags();
	r = mailimap_fetch_type_new_fetch_att_list_add(fetch_type, fetch_att);
  
	/* rfc822 size */
	fetch_att = mailimap_fetch_att_new_rfc822_size();
	r = mailimap_fetch_type_new_fetch_att_list_add(fetch_type, fetch_att);
  
	/* headers */
	key.data = &imap;
	key.len = sizeof(imap);
	r = chash_get(courier_workaround_hash, &key, &value);
	if (r < 0)
		r = imap_add_envelope_fetch_att(fetch_type);
	else
		r = imap_add_header_fetch_att(fetch_type);
	
	mailstream_logger = imap_logger_fetch;
	
	r = mailimap_uid_fetch(imap, set, fetch_type, &fetch_result);
	
	mailstream_logger = imap_logger_cmd;
	switch (r) {
	case MAILIMAP_NO_ERROR:
		break;
	default:
		mailimap_fetch_type_free(fetch_type);
		debug_print("uid_fetch: %d\n", r);
		return r;
	}
	
	if (fetch_result == NULL || clist_begin(fetch_result) == NULL) {
		res = MAILIMAP_ERROR_FETCH;
		debug_print("clist_begin = NULL\n");
		goto err;
	}
	
	r = imap_fetch_result_to_envelop_list(fetch_result, &env_list);
	mailimap_fetch_list_free(fetch_result);
	
	if (r != MAILIMAP_NO_ERROR) {
		mailimap_fetch_type_free(fetch_type);
		res = MAILIMAP_ERROR_MEMORY;
		debug_print("fetch_result_to_envelop_list: %d\n", res);
		goto err;
	}
	
	mailimap_fetch_type_free(fetch_type);
	
	* p_env_list = env_list;
	
	return MAILIMAP_NO_ERROR;
  
 err:
	return res;
}

struct fetch_env_param {
	mailimap * imap;
	struct mailimap_set * set;
};

struct fetch_env_result {
	carray * fetch_env_result;
	int error;
};

static void fetch_env_run(struct etpan_thread_op * op)
{
	struct fetch_env_param * param;
	struct fetch_env_result * result;
	carray * env_list;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	env_list = NULL;
	r = imap_get_envelopes_list(param->imap, param->set,
				    &env_list);
	
	result->error = r;
	result->fetch_env_result = env_list;
	
	debug_print("imap fetch_env run - end %i\n", r);
}

int imap_threaded_fetch_env(Folder * folder, struct mailimap_set * set,
			    carray ** p_env_list)
{
	struct fetch_env_param param;
	struct fetch_env_result result;
	mailimap * imap;
	
	debug_print("imap fetch_env - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.set = set;
	
	threaded_run(folder, &param, &result, fetch_env_run);
	
	if (result.error != MAILIMAP_NO_ERROR) {
		chashdatum key;
		chashdatum value;
		int r;
		
		key.data = &imap;
		key.len = sizeof(imap);
		r = chash_get(courier_workaround_hash, &key, &value);
		if (r < 0) {
			value.data = NULL;
			value.len = 0;
			chash_set(courier_workaround_hash, &key, &value, NULL);
			
			threaded_run(folder, &param, &result, fetch_env_run);
		}
	}
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	debug_print("imap fetch_env - end\n");
	
	* p_env_list = result.fetch_env_result;
	
	return result.error;
}

void imap_fetch_env_free(carray * env_list)
{
	unsigned int i;
	
	for(i = 0 ; i < carray_count(env_list) ; i += 2) {
		struct imap_fetch_env_info * env_info;
		
		env_info = carray_get(env_list, i);
		free(env_info->headers);
		free(env_info);
	}
	carray_free(env_list);
}





struct append_param {
	mailimap * imap;
	const char * mailbox;
	const char * filename;
	struct mailimap_flag_list * flag_list;
};

struct append_result {
	int error;
	int uid;
};

static void append_run(struct etpan_thread_op * op)
{
	struct append_param * param;
	struct append_result * result;
	int r;
	char * data;
	size_t size;
#ifndef G_OS_WIN32
	struct stat stat_buf;
	int fd;
#endif
	guint32 uid = 0, val = 0;
	
	param = op->param;
	result = op->result;
	
	CHECK_IMAP();

#ifndef G_OS_WIN32
	r = stat(param->filename, &stat_buf);
	if (r < 0) {
		result->error = MAILIMAP_ERROR_APPEND;
		return;
	}
	size = stat_buf.st_size;
	
	fd = g_open(param->filename, O_RDONLY, 0);
	if (fd < 0) {
		result->error = MAILIMAP_ERROR_APPEND;
		return;
	}
	
	data = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (data == (void *) MAP_FAILED) {
		close(fd);
		result->error = MAILIMAP_ERROR_APPEND;
		return;
	}
#else
	data = file_read_to_str_no_recode(param->filename);
	if (data == NULL) {
		result->error = MAILIMAP_ERROR_APPEND;
		return;
	}
	size = strlen(data);
#endif
	mailstream_logger = imap_logger_append;
	
	r = mailimap_uidplus_append(param->imap, param->mailbox,
			    param->flag_list, NULL,
			    data, size, &val, &uid);

	mailstream_logger = imap_logger_cmd;
	
#ifndef G_OS_WIN32
	munmap(data, size);
	close(fd);
#else
	g_free(data);
#endif
	
	result->error = r;
	result->uid = uid;
	debug_print("imap append run - end %i uid %d\n", r, uid);
}

int imap_threaded_append(Folder * folder, const char * mailbox,
			 const char * filename,
			 struct mailimap_flag_list * flag_list,
			 int *uid)
{
	struct append_param param;
	struct append_result result;
	mailimap * imap;
	
	debug_print("imap append - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.mailbox = mailbox;
	param.filename = filename;
	param.flag_list = flag_list;
	
	threaded_run(folder, &param, &result, append_run);
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	debug_print("imap append - end\n");
	if (uid != NULL)
		*uid = result.uid;

	return result.error;
}




struct expunge_param {
	mailimap * imap;
};

struct expunge_result {
	int error;
};

static void expunge_run(struct etpan_thread_op * op)
{
	struct expunge_param * param;
	struct expunge_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_expunge(param->imap);
	
	result->error = r;
	debug_print("imap expunge run - end %i\n", r);
}

int imap_threaded_expunge(Folder * folder)
{
	struct expunge_param param;
	struct expunge_result result;
	
	debug_print("imap expunge - begin\n");
	
	param.imap = get_imap(folder);
	
	threaded_run(folder, &param, &result, expunge_run);
	
	debug_print("imap expunge - end\n");
	
	return result.error;
}


struct copy_param {
	mailimap * imap;
	struct mailimap_set * set;
	const char * mb;
};

struct copy_result {
	int error;
	struct mailimap_set *source;
	struct mailimap_set *dest;
};

static void copy_run(struct etpan_thread_op * op)
{
	struct copy_param * param;
	struct copy_result * result;
	int r;
	guint32 val;
	struct mailimap_set *source = NULL, *dest = NULL;

	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_uidplus_uid_copy(param->imap, param->set, param->mb,
		&val, &source, &dest);
	
	result->error = r;
	if (r == 0) {
		result->source = source;
		result->dest = dest;
	} else {
		result->source = NULL;
		result->dest = NULL;
	}
	debug_print("imap copy run - end %i\n", r);
}

int imap_threaded_copy(Folder * folder, struct mailimap_set * set,
		       const char * mb, struct mailimap_set **source,
		       struct mailimap_set **dest)
{
	struct copy_param param;
	struct copy_result result;
	mailimap * imap;
	
	debug_print("imap copy - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.set = set;
	param.mb = mb;
	
	threaded_run(folder, &param, &result, copy_run);
	*source = NULL;
	*dest = NULL;
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	*source = result.source;
	*dest = result.dest;

	debug_print("imap copy - end\n");
	
	return result.error;
}



struct store_param {
	mailimap * imap;
	struct mailimap_set * set;
	struct mailimap_store_att_flags * store_att_flags;
};

struct store_result {
	int error;
};

static void store_run(struct etpan_thread_op * op)
{
	struct store_param * param;
	struct store_result * result;
	int r;
	
	param = op->param;
	result = op->result;

	CHECK_IMAP();

	r = mailimap_uid_store(param->imap, param->set,
			       param->store_att_flags);
	
	result->error = r;
	
	debug_print("imap store run - end %i\n", r);
}

int imap_threaded_store(Folder * folder, struct mailimap_set * set,
			struct mailimap_store_att_flags * store_att_flags)
{
	struct store_param param;
	struct store_result result;
	mailimap * imap;
	
	debug_print("imap store - begin\n");
	
	imap = get_imap(folder);
	param.imap = imap;
	param.set = set;
	param.store_att_flags = store_att_flags;
	
	threaded_run(folder, &param, &result, store_run);
	
	if (result.error != MAILIMAP_NO_ERROR)
		return result.error;
	
	debug_print("imap store - end\n");
	
	return result.error;
}


#define ENV_BUFFER_SIZE 512
#ifndef G_OS_WIN32
static void do_exec_command(int fd, const char * command,
			    const char * servername, uint16_t port)
{
	int i, maxopen;
#ifdef SOLARIS
	char env_buffer[ENV_BUFFER_SIZE];
#endif
	
	if (fork() > 0) {
		/* Fork again to become a child of init rather than
		   the etpan client. */
		exit(0);
	}
  
	if (servername)
		g_setenv("ETPANSERVER", servername, TRUE);
	else
		g_unsetenv("ETPANSERVER");
  
	if (port) {
		char porttext[20];
		
		snprintf(porttext, sizeof(porttext), "%d", port);
		g_setenv("ETPANPORT", porttext, TRUE);
	}
	else {
		g_unsetenv("ETPANPORT");
	}
		
	/* Not a lot we can do if there's an error other than bail. */
	if (dup2(fd, 0) == -1)
		exit(1);
	if (dup2(fd, 1) == -1)
		exit(1);
  
	/* Should we close stderr and reopen /dev/null? */
  
	maxopen = sysconf(_SC_OPEN_MAX);
	for (i=3; i < maxopen; i++)
		close(i);
  
#ifdef TIOCNOTTY
	/* Detach from the controlling tty if we have one. Otherwise,
	   SSH might do something stupid like trying to use it instead
	   of running $SSH_ASKPASS. Doh. */
	fd = g_open("/dev/tty", O_RDONLY, 0);
	if (fd != -1) {
		ioctl(fd, TIOCNOTTY, NULL);
		close(fd);
	}
#endif /* TIOCNOTTY */

	execl("/bin/sh", "/bin/sh", "-c", command, NULL);
  
	/* Eep. Shouldn't reach this */
	exit(1);
}

static int subcommand_connect(const char *command,
			      const char *servername, uint16_t port)
{
	/* SEB unsupported on Windows */
	int sockfds[2];
	pid_t childpid;
  
	if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockfds))
		return -1;
  
	childpid = fork();
	if (!childpid) {
		do_exec_command(sockfds[1], command, servername, port);
	}
	else if (childpid == -1) {
		close(sockfds[0]);
		close(sockfds[1]);
		return -1;
	}
  
	close(sockfds[1]);
  
	/* Reap child, leaving grandchild process to run */
	waitpid(childpid, NULL, 0);
  
	return sockfds[0];
}

static int socket_connect_cmd(mailimap * imap, const char * command,
		       const char * server, int port)
{
	int fd;
	mailstream * s;
	int r;
	
	fd = subcommand_connect(command, server, port);
	if (fd < 0)
		return MAILIMAP_ERROR_STREAM;
	
	s = mailstream_socket_open(fd);
	if (s == NULL) {
		close(fd);
		return MAILIMAP_ERROR_STREAM;
	}
	
	r = mailimap_connect(imap, s);
	if (r != MAILIMAP_NO_ERROR_AUTHENTICATED
	&&  r != MAILIMAP_NO_ERROR_NON_AUTHENTICATED) {
		mailstream_close(s);
		if (imap)
			imap->imap_stream = NULL;
		return r;
	}
	
	return r;
}

/* connect cmd */

struct connect_cmd_param {
	mailimap * imap;
	const char * command;
	const char * server;
	int port;
};

struct connect_cmd_result {
	int error;
};

static void connect_cmd_run(struct etpan_thread_op * op)
{
	int r;
	struct connect_cmd_param * param;
	struct connect_cmd_result * result;
	
	param = op->param;
	result = op->result;
	
	CHECK_IMAP();

	r = socket_connect_cmd(param->imap, param->command,
			       param->server, param->port);
	
	result->error = r;
}


int imap_threaded_connect_cmd(Folder * folder, const char * command,
			      const char * server, int port)
{
	struct connect_cmd_param param;
	struct connect_cmd_result result;
	chashdatum key;
	chashdatum value;
	mailimap * imap, * oldimap;
	
	oldimap = get_imap(folder);

	imap = mailimap_new(0, NULL);
	
	if (oldimap) {
		debug_print("deleting old imap %p\n", oldimap);
		delete_imap(folder, oldimap);
	}

	key.data = &folder;
	key.len = sizeof(folder);
	value.data = imap;
	value.len = 0;
	chash_set(session_hash, &key, &value, NULL);
	
	param.imap = imap;
	param.command = command;
	param.server = server;
	param.port = port;
	
	threaded_run(folder, &param, &result, connect_cmd_run);
	
	debug_print("connect_cmd ok %i with imap %p\n", result.error, imap);
	
	return result.error;
}
#endif /* G_OS_WIN32 */

void imap_threaded_cancel(Folder * folder)
{
	mailimap * imap;
	
	imap = get_imap(folder);
	if (imap->imap_stream != NULL)
		mailstream_cancel(imap->imap_stream);
}

#else

void imap_main_init(void)
{
}
void imap_main_done(gboolean have_connectivity)
{
}
void imap_main_set_timeout(int sec)
{
}

void imap_threaded_cancel(Folder * folder);
{
}

#endif
