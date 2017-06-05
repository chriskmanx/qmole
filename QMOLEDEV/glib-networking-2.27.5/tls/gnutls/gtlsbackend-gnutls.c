/* GIO - GLib Input, Output and Streaming Library
 *
 * Copyright © 2010 Red Hat, Inc
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "config.h"
#include "glib.h"

#include <errno.h>

#include <gnutls/gnutls.h>
#include <gcrypt.h>
#ifndef G_OS_WIN32
#include <pthread.h>
#endif

#include "gtlsbackend-gnutls.h"
#include "gtlscertificate-gnutls.h"
#include "gtlsclientconnection-gnutls.h"
#include "gtlsserverconnection-gnutls.h"

static void g_tls_backend_gnutls_interface_init (GTlsBackendInterface *iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (GTlsBackendGnutls, g_tls_backend_gnutls, G_TYPE_OBJECT, 0,
				G_IMPLEMENT_INTERFACE_DYNAMIC (G_TYPE_TLS_BACKEND,
							       g_tls_backend_gnutls_interface_init);)

#if defined(GCRY_THREAD_OPTION_PTHREAD_IMPL) && !defined(G_OS_WIN32)
GCRY_THREAD_OPTION_PTHREAD_IMPL;
#endif

#ifdef G_OS_WIN32

static int
gtls_gcry_win32_mutex_init (void **priv)
{
	int err = 0;
	CRITICAL_SECTION *lock = (CRITICAL_SECTION*)malloc (sizeof (CRITICAL_SECTION));

	if (!lock)
		err = ENOMEM;
	if (!err) {
		InitializeCriticalSection (lock);
		*priv = lock;
	}
	return err;
}

static int
gtls_gcry_win32_mutex_destroy (void **lock)
{
	DeleteCriticalSection ((CRITICAL_SECTION*)*lock);
	free (*lock);
	return 0;
}

static int
gtls_gcry_win32_mutex_lock (void **lock)
{
	EnterCriticalSection ((CRITICAL_SECTION*)*lock);
	return 0;
}

static int
gtls_gcry_win32_mutex_unlock (void **lock)
{
	LeaveCriticalSection ((CRITICAL_SECTION*)*lock);
	return 0;
}


static struct gcry_thread_cbs gtls_gcry_threads_win32 = {		 \
	(GCRY_THREAD_OPTION_USER | (GCRY_THREAD_OPTION_VERSION << 8)),	 \
	NULL, gtls_gcry_win32_mutex_init, gtls_gcry_win32_mutex_destroy, \
	gtls_gcry_win32_mutex_lock, gtls_gcry_win32_mutex_unlock,	 \
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

#endif

static gpointer
gtls_gnutls_init (gpointer data)
{
#if defined(GCRY_THREAD_OPTION_PTHREAD_IMPL) && !defined(G_OS_WIN32)
  gcry_control (GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread);
#elif defined(G_OS_WIN32)
  gcry_control (GCRYCTL_SET_THREAD_CBS, &gtls_gcry_threads_win32);
#endif
  gnutls_global_init ();

  /* Leak the module to keep it from being unloaded. */
  g_type_plugin_use (g_type_get_plugin (G_TYPE_TLS_BACKEND_GNUTLS));
  return NULL;
}

static GOnce gnutls_inited = G_ONCE_INIT;

static void
g_tls_backend_gnutls_init (GTlsBackendGnutls *backend)
{
  /* Once we call gtls_gnutls_init(), we can't allow the module to be
   * unloaded, since that would break the pointers to the mutex
   * functions we set for gcrypt. So we initialize it from here rather
   * than at class init time so that it doesn't happen unless the app
   * is actually using TLS (as opposed to just calling
   * g_io_modules_scan_all_in_directory()).
   */
  g_once (&gnutls_inited, gtls_gnutls_init, NULL);
}

static void
g_tls_backend_gnutls_class_init (GTlsBackendGnutlsClass *backend_class)
{
}

static void
g_tls_backend_gnutls_class_finalize (GTlsBackendGnutlsClass *backend_class)
{
}

static void
g_tls_backend_gnutls_interface_init (GTlsBackendInterface *iface)
{
  iface->get_certificate_type       = g_tls_certificate_gnutls_get_type;
  iface->get_client_connection_type = g_tls_client_connection_gnutls_get_type;
  iface->get_server_connection_type = g_tls_server_connection_gnutls_get_type;
}

#ifdef GTLS_SYSTEM_CA_FILE
/* Parsing the system CA list takes a noticeable amount of time.
 * So we only do it once, and only when we actually need to see it.
 */
static const GList *
get_ca_lists (gnutls_x509_crt_t **cas,
	      int                *num_cas)
{
  static gnutls_x509_crt_t *ca_list_gnutls;
  static int ca_list_length;
  static GList *ca_list;

  if (g_once_init_enter ((volatile gsize *)&ca_list_gnutls))
    {
      GError *error = NULL;
      gnutls_x509_crt_t *x509_crts;
      GList *c;
      int i;

      ca_list = g_tls_certificate_list_new_from_file (GTLS_SYSTEM_CA_FILE, &error);
      if (error)
	{
	  g_warning ("Failed to read system CA file %s: %s.",
		     GTLS_SYSTEM_CA_FILE, error->message);
	  g_error_free (error);
	  /* Note that this is not a security problem, since if
	   * G_TLS_VALIDATE_CA is set, then this just means validation
	   * will always fail, and if it isn't set, then it doesn't
	   * matter that we couldn't read the CAs.
	   */
	}

      ca_list_length = g_list_length (ca_list);
      x509_crts = g_new (gnutls_x509_crt_t, ca_list_length);
      for (c = ca_list, i = 0; c; c = c->next, i++)
	x509_crts[i] = g_tls_certificate_gnutls_get_cert (c->data);

      g_once_init_leave ((volatile gsize *)&ca_list_gnutls, GPOINTER_TO_SIZE (x509_crts));
    }

  if (cas)
    *cas = ca_list_gnutls;
  if (num_cas)
    *num_cas = ca_list_length;
  
  return ca_list;
}
#endif

const GList *
g_tls_backend_gnutls_get_system_ca_list_gtls (void)
{
#ifdef GTLS_SYSTEM_CA_FILE
  return get_ca_lists (NULL, NULL);
#else
  return NULL;
#endif
}

void
g_tls_backend_gnutls_get_system_ca_list_gnutls (gnutls_x509_crt_t **cas,
						int                *num_cas)
{
#ifdef GTLS_SYSTEM_CA_FILE
  get_ca_lists (cas, num_cas);
#else
  *cas = NULL;
  *num_cas = 0;
#endif
}

/* Session cache support; all the details are sort of arbitrary. Note
 * that having session_cache_cleanup() be a little bit slow isn't the
 * end of the world, since it will still be faster than the network
 * is. (NSS uses a linked list for its cache...)
 */

G_LOCK_DEFINE_STATIC (session_cache_lock);
GHashTable *session_cache;

#define SESSION_CACHE_MAX_SIZE 50
#define SESSION_CACHE_MAX_AGE (60 * 60) /* one hour */

typedef struct {
  gchar      *session_id;
  GByteArray *session_data;
  time_t      last_used;
} GTlsBackendGnutlsCacheData;

static void
session_cache_cleanup (void)
{
  GHashTableIter iter;
  gpointer key, value;
  GTlsBackendGnutlsCacheData *cache_data;
  time_t expired = time (NULL) - SESSION_CACHE_MAX_AGE;

  g_hash_table_iter_init (&iter, session_cache);
  while (g_hash_table_iter_next (&iter, &key, &value))
    {
      cache_data = value;
      if (cache_data->last_used < expired)
	g_hash_table_iter_remove (&iter);
    }
}

static void
cache_data_free (gpointer data)
{
  GTlsBackendGnutlsCacheData *cache_data = data;

  g_free (cache_data->session_id);
  g_byte_array_unref (cache_data->session_data);
  g_slice_free (GTlsBackendGnutlsCacheData, cache_data);
}

void
g_tls_backend_gnutls_cache_session_data (const gchar *session_id,
					 guchar      *session_data,
					 gsize        session_data_length)
{
  GTlsBackendGnutlsCacheData *cache_data;

  G_LOCK (session_cache_lock);

  if (!session_cache)
    session_cache = g_hash_table_new_full (g_str_hash, g_str_equal,
					   NULL, cache_data_free);

  cache_data = g_hash_table_lookup (session_cache, session_id);
  if (cache_data)
    {
      if (cache_data->session_data->len == session_data_length &&
	  memcmp (cache_data->session_data->data,
		  session_data, session_data_length) == 0)
	{
	  cache_data->last_used = time (NULL);
	  G_UNLOCK (session_cache_lock);
	  return;
	}

      g_byte_array_set_size (cache_data->session_data, 0);
    }
  else
    {
      if (g_hash_table_size (session_cache) >= SESSION_CACHE_MAX_SIZE)
	session_cache_cleanup ();

      cache_data = g_slice_new (GTlsBackendGnutlsCacheData);
      cache_data->session_id = g_strdup (session_id);
      cache_data->session_data = g_byte_array_sized_new (session_data_length);

      g_hash_table_insert (session_cache, cache_data->session_id, cache_data);
    }

  g_byte_array_append (cache_data->session_data,
		       session_data, session_data_length);
  cache_data->last_used = time (NULL);
  G_UNLOCK (session_cache_lock);
}

void
g_tls_backend_gnutls_uncache_session_data (const gchar *session_id)
{
  G_LOCK (session_cache_lock);
  if (session_cache)
    g_hash_table_remove (session_cache, session_id);
  G_UNLOCK (session_cache_lock);
}

GByteArray *
g_tls_backend_gnutls_lookup_session_data (const gchar *session_id)
{
  GTlsBackendGnutlsCacheData *cache_data;
  GByteArray *session_data = NULL;

  G_LOCK (session_cache_lock);
  if (session_cache)
    {
      cache_data = g_hash_table_lookup (session_cache, session_id);
      if (cache_data)
	{
	  cache_data->last_used = time (NULL);
	  session_data = g_byte_array_ref (cache_data->session_data);
	}
    }
  G_UNLOCK (session_cache_lock);

  return session_data;
}

void
g_tls_backend_gnutls_register (GIOModule *module)
{
  g_tls_backend_gnutls_register_type (G_TYPE_MODULE (module));
  g_io_extension_point_implement (G_TLS_BACKEND_EXTENSION_POINT_NAME,
				  g_tls_backend_gnutls_get_type(),
				  "gnutls",
				  0);
}
