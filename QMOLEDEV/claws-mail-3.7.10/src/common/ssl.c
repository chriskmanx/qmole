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

#ifdef USE_GNUTLS
#include "defs.h"

#include <stdlib.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <errno.h>
#include <pthread.h>
#include <gcrypt.h>
GCRY_THREAD_OPTION_PTHREAD_IMPL;

#include "claws.h"
#include "utils.h"
#include "ssl.h"
#include "ssl_certificate.h"
#include "hooks.h"

#ifdef HAVE_LIBETPAN
#include <libetpan/mailstream_ssl.h>
#endif

#ifdef USE_PTHREAD
#include <pthread.h>
#endif

#ifdef USE_PTHREAD
typedef struct _thread_data {
	gnutls_session ssl;
	gboolean done;
} thread_data;
#endif

static int gnutls_client_cert_cb(gnutls_session session,
                               const gnutls_datum *req_ca_rdn, int nreqs,
                               const gnutls_pk_algorithm *sign_algos,
                               int sign_algos_length, gnutls_retr_st *st)
{
	SSLClientCertHookData hookdata;
	SockInfo *sockinfo = (SockInfo *)gnutls_session_get_ptr(session);
	gnutls_certificate_type type = gnutls_certificate_type_get(session);
	gnutls_x509_crt crt;
	gnutls_x509_privkey key;

	st->ncerts = 0;

	hookdata.account = sockinfo->account;
	hookdata.cert_path = NULL;
	hookdata.password = NULL;
	hookdata.is_smtp = sockinfo->is_smtp;
	hooks_invoke(SSLCERT_GET_CLIENT_CERT_HOOKLIST, &hookdata);	

	if (hookdata.cert_path == NULL)
		return 0;

	sockinfo->client_crt = ssl_certificate_get_x509_from_pem_file(hookdata.cert_path);
	sockinfo->client_key = ssl_certificate_get_pkey_from_pem_file(hookdata.cert_path);
	if (!(sockinfo->client_crt && sockinfo->client_key)) {
		/* try pkcs12 format */
		ssl_certificate_get_x509_and_pkey_from_p12_file(hookdata.cert_path, hookdata.password, 
			&crt, &key);
		sockinfo->client_crt = crt;
		sockinfo->client_key = key;
	}

	if (type == GNUTLS_CRT_X509 && sockinfo->client_crt && sockinfo->client_key) {
		st->ncerts = 1;
		st->type = type;
		st->cert.x509 = &(sockinfo->client_crt);
		st->key.x509 = sockinfo->client_key;
		st->deinit_all = 0;
		return 0;
	}
	return 0;
}

const gchar *claws_ssl_get_cert_file(void)
{
	const char *cert_files[]={
		"/etc/pki/tls/certs/ca-bundle.crt",
		"/etc/certs/ca-bundle.crt",
		"/usr/share/ssl/certs/ca-bundle.crt",
		"/etc/ssl/certs/ca-certificates.crt",
		"/usr/local/ssl/certs/ca-bundle.crt",
		"/etc/apache/ssl.crt/ca-bundle.crt",
		"/usr/share/curl/curl-ca-bundle.crt",
		"/usr/share/curl/curl-ca-bundle.crt",
		"/usr/lib/ssl/cert.pem",
		NULL};
	int i;
    	
	if (g_getenv("SSL_CERT_FILE"))
		return g_getenv("SSL_CERT_FILE");
#ifndef G_OS_WIN32
	for (i = 0; cert_files[i]; i++) {
		if (is_file_exist(cert_files[i]))
			return cert_files[i];
	}
	return NULL;
#else
	return get_cert_file();
#endif
}

const gchar *claws_ssl_get_cert_dir(void)
{
	const char *cert_dirs[]={
		"/etc/pki/tls/certs",
		"/etc/certs",
		"/usr/share/ssl/certs",
		"/etc/ssl/certs",
		"/usr/local/ssl/certs",
		"/etc/apache/ssl.crt",
		"/usr/share/curl",
		"/usr/lib/ssl/certs",
		NULL};
	int i;
    	
	if (g_getenv("SSL_CERT_DIR"))
		return g_getenv("SSL_CERT_DIR");
#ifndef G_OS_WIN32
	for (i = 0; cert_dirs[i]; i++) {
		if (is_dir_exist(cert_dirs[i]))
			return cert_dirs[i];
	}
	return NULL;
#else
	return "put_what_s_needed_here";
#endif
}

void ssl_init(void)
{
	gcry_control (GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread);
#ifdef HAVE_LIBETPAN
	mailstream_gnutls_init_not_required();
#endif	
	gnutls_global_init();
}

void ssl_done(void)
{
	gnutls_global_deinit();
}

#ifdef USE_PTHREAD
static void *SSL_connect_thread(void *data)
{
	thread_data *td = (thread_data *)data;
	int result = -1;

	pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
	pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

	do {
		result = gnutls_handshake(td->ssl);
	} while (result == GNUTLS_E_AGAIN || result == GNUTLS_E_INTERRUPTED);

	td->done = TRUE; /* let the caller thread join() */
	return GINT_TO_POINTER(result);
}
#endif

static gint SSL_connect_nb(gnutls_session ssl)
{
	int result;
#ifdef USE_PTHREAD
	thread_data *td = g_new0(thread_data, 1);
	pthread_t pt;
	pthread_attr_t pta;
	void *res = NULL;
	time_t start_time = time(NULL);
	gboolean killed = FALSE;
	
	td->ssl  = ssl;
	td->done = FALSE;
	
	/* try to create a thread to initialize the SSL connection,
	 * fallback to blocking method in case of problem 
	 */
	if (pthread_attr_init(&pta) != 0 ||
	    pthread_attr_setdetachstate(&pta, PTHREAD_CREATE_JOINABLE) != 0 ||
	    pthread_create(&pt, &pta, SSL_connect_thread, td) != 0) {
		do {
			result = gnutls_handshake(td->ssl);
		} while (result == GNUTLS_E_AGAIN || result == GNUTLS_E_INTERRUPTED);
		return result;
	}
	debug_print("waiting for SSL_connect thread...\n");
	while(!td->done) {
		/* don't let the interface freeze while waiting */
		claws_do_idle();
		if (time(NULL) - start_time > 30) {
			pthread_cancel(pt);
			td->done = TRUE;
			killed = TRUE;
		}
	}

	/* get the thread's return value and clean its resources */
	pthread_join(pt, &res);
	g_free(td);
	
	if (killed) {
		res = GINT_TO_POINTER(-1);
	}
	debug_print("SSL_connect thread returned %d\n", 
			GPOINTER_TO_INT(res));
	
	return GPOINTER_TO_INT(res);
#else /* USE_PTHREAD */
	do {
		result = gnutls_handshake(ssl);
	} while (result == GNUTLS_E_AGAIN || result == GNUTLS_E_INTERRUPTED);
#endif
}

gboolean ssl_init_socket(SockInfo *sockinfo)
{
	return ssl_init_socket_with_method(sockinfo, SSL_METHOD_SSLv23);
}

gboolean ssl_init_socket_with_method(SockInfo *sockinfo, SSLMethod method)
{
	gnutls_session session;
	int r;
	const int cipher_prio[] = { GNUTLS_CIPHER_AES_128_CBC,
		  		GNUTLS_CIPHER_3DES_CBC,
		  		GNUTLS_CIPHER_AES_256_CBC,
		  		GNUTLS_CIPHER_ARCFOUR_128, 0 };
	const int kx_prio[] = { GNUTLS_KX_DHE_RSA,
			   GNUTLS_KX_RSA, 
			   GNUTLS_KX_DHE_DSS, 0 };
	const int mac_prio[] = { GNUTLS_MAC_SHA1,
		  		GNUTLS_MAC_MD5, 0 };
	const int proto_prio[] = { GNUTLS_TLS1,
		  		  GNUTLS_SSL3, 0 };
	const gnutls_datum *raw_cert_list;
	unsigned int raw_cert_list_length;
	gnutls_x509_crt cert = NULL;
	guint status;
	gnutls_certificate_credentials_t xcred;

	if (gnutls_certificate_allocate_credentials (&xcred) != 0)
		return FALSE;

	r = gnutls_init(&session, GNUTLS_CLIENT);
	if (session == NULL || r != 0)
		return FALSE;
  
	gnutls_transport_set_lowat (session, 1); 
	gnutls_set_default_priority(session);
	gnutls_protocol_set_priority (session, proto_prio);
	gnutls_cipher_set_priority (session, cipher_prio);
	gnutls_kx_set_priority (session, kx_prio);
	gnutls_mac_set_priority (session, mac_prio);
	gnutls_record_disable_padding(session);

	gnutls_credentials_set(session, GNUTLS_CRD_CERTIFICATE, xcred);

	if (claws_ssl_get_cert_file()) {
		r = gnutls_certificate_set_x509_trust_file(xcred, claws_ssl_get_cert_file(),  GNUTLS_X509_FMT_PEM);
		if (r < 0)
			g_warning("Can't read SSL_CERT_FILE %s: %s\n",
				claws_ssl_get_cert_file(), 
				gnutls_strerror(r));
	} else {
		debug_print("Can't find SSL ca-certificates file\n");
	}
	gnutls_certificate_set_verify_flags (xcred, GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT);

	gnutls_transport_set_ptr(session, (gnutls_transport_ptr) sockinfo->sock);
	gnutls_session_set_ptr(session, sockinfo);
	gnutls_certificate_client_set_retrieve_function(xcred, gnutls_client_cert_cb);

	gnutls_dh_set_prime_bits(session, 512);

	if ((r = SSL_connect_nb(session)) < 0) {
		g_warning("SSL connection failed (%s)", gnutls_strerror(r));
		gnutls_certificate_free_credentials(xcred);
		gnutls_deinit(session);
		return FALSE;
	}

	/* Get server's certificate (note: beware of dynamic allocation) */
	raw_cert_list = gnutls_certificate_get_peers(session, &raw_cert_list_length);

	if (!raw_cert_list 
	||  gnutls_certificate_type_get(session) != GNUTLS_CRT_X509
	||  (r = gnutls_x509_crt_init(&cert)) < 0
	||  (r = gnutls_x509_crt_import(cert, &raw_cert_list[0], GNUTLS_X509_FMT_DER)) < 0) {
		g_warning("cert get failure: %d %s\n", r, gnutls_strerror(r));
		gnutls_certificate_free_credentials(xcred);
		gnutls_deinit(session);
		return FALSE;
	}

	r = gnutls_certificate_verify_peers2(session, &status);

	if (!ssl_certificate_check(cert, status, sockinfo->canonical_name, sockinfo->hostname, sockinfo->port)) {
		gnutls_x509_crt_deinit(cert);
		gnutls_certificate_free_credentials(xcred);
		gnutls_deinit(session);
		return FALSE;
	}

	gnutls_x509_crt_deinit(cert);

	sockinfo->ssl = session;
	sockinfo->xcred = xcred;
	return TRUE;
}

void ssl_done_socket(SockInfo *sockinfo)
{
	if (sockinfo && sockinfo->ssl) {
		gnutls_certificate_free_credentials(sockinfo->xcred);
		gnutls_deinit(sockinfo->ssl);
		if (sockinfo->client_crt)
			gnutls_x509_crt_deinit(sockinfo->client_crt);
		if (sockinfo->client_key)
			gnutls_x509_privkey_deinit(sockinfo->client_key);
		sockinfo->client_key = NULL;
		sockinfo->client_crt = NULL;
		sockinfo->ssl = NULL;
	}
}

#endif /* USE_GNUTLS */
