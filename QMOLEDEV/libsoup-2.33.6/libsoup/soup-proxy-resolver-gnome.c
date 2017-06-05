/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-proxy-resolver-gnome.c: GNOME proxy resolution
 *
 * Copyright (C) 2008 Red Hat, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdlib.h>

#include "soup-proxy-resolver-gnome.h"
#include "soup-proxy-uri-resolver.h"
#include "soup-message.h"
#include "soup-misc.h"
#include "soup-session-feature.h"
#include "soup-uri.h"

#include <gconf/gconf-client.h>
#include <proxy.h>

/* libproxy/GConf-based proxy resolution, working around multiple
 * problems:
 *
 *   1. GConf is not thread-safe, but a SoupProxyResolver may be used
 *      from multiple threads. We need to take steps to ensure that if
 *      there is a thread running the default main loop, then GConf
 *      calls are only made from that thread.
 *
 *   2. libproxy 0.2 was unaware of GConf's non-thread-safety, so it
 *      is unsafe to call into libproxy other than from the main
 *      thread as well, unless we can guarantee that it won't use the
 *      "gnome" plugin.
 *
 *   3. libproxy 0.3 fixes the thread-safety issues by just calling
 *      out to gconftool-2 on every call. That fixes the crashes but
 *      makes things a lot slower, so ideally we still don't want to
 *      call it if it's going to use the "gnome" plugin.
 *
 *   4. libproxy is not cancellable, and may block (eg, doing WPAD or
 *      just DNS, or running a PAC script), so if we want to be able
 *      to cancel calls to it, we need to do them in another thread
 *      even if we're making a sync call.
 *
 * The only good way to ensure that libproxy won't use the gnome
 * plugin is to ensure that it will use environment variables instead.
 * But if we just copy the GConf settings to the environment, then
 * we'll leak them to child processes. For libproxy 0.2, we *can't*
 * let it use the gnome plugin, so if we need to use libproxy (to get
 * PAC/WPAD or ignore_host support) then we just suffer through
 * environment leakage. For libproxy 0.3, the gconftool-calling
 * slowness is less bad than environment variable leakage would be, so
 * we let it use the gnome plugin in that case.
 */

/* We make these variables static rather than per-resolver, since
 * they're awkward to deal with because of GConf's non-thread-safety,
 * and because there's only one GConf database regardless of how many
 * SoupProxyResolverGNOMEs there are anyway.
 */
G_LOCK_DEFINE_STATIC (resolver_gnome);
static GConfClient *gconf_client;
static char *proxy_user, *proxy_password;
static char *http_proxy, *https_proxy;

static pxProxyFactory *libproxy_factory;
static GThreadPool *libproxy_threadpool;
#ifdef HAVE_LIBPROXY_WITH_NON_THREAD_SAFE_GNOME_MODULE
static gboolean overrode_environment;
#endif

static void soup_proxy_resolver_gnome_interface_init (SoupProxyURIResolverInterface *proxy_resolver_interface);

G_DEFINE_TYPE_EXTENDED (SoupProxyResolverGNOME, soup_proxy_resolver_gnome, G_TYPE_OBJECT, 0,
			G_IMPLEMENT_INTERFACE (SOUP_TYPE_SESSION_FEATURE, NULL)
			G_IMPLEMENT_INTERFACE (SOUP_TYPE_PROXY_URI_RESOLVER, soup_proxy_resolver_gnome_interface_init))

static void gconf_value_changed (GConfClient *client, const char *key,
				 GConfValue *value, gpointer user_data);
static void update_proxy_settings (void);

static void libproxy_threadpool_func (gpointer thread_data, gpointer user_data);

static void get_proxy_uri_async (SoupProxyURIResolver  *proxy_uri_resolver,
				 SoupURI               *uri,
				 GMainContext          *async_context,
				 GCancellable          *cancellable,
				 SoupProxyURIResolverCallback callback,
				 gpointer               user_data);
static guint get_proxy_uri_sync (SoupProxyURIResolver  *proxy_uri_resolver,
				 SoupURI               *uri,
				 GCancellable          *cancellable,
				 SoupURI              **proxy_uri);

typedef struct {
	GMutex *lock;
	GCond *cond;
} SoupProxyResolverGNOMEInitData;

static gboolean
init_gconf (gpointer user_data)
{
	SoupProxyResolverGNOMEInitData *id = user_data;

	if (id)
		g_mutex_lock (id->lock);

	/* resolver_gnome is locked */

	gconf_client = gconf_client_get_default ();

	gconf_client_add_dir (gconf_client, "/system/proxy",
			      GCONF_CLIENT_PRELOAD_RECURSIVE, NULL);
	gconf_client_add_dir (gconf_client, "/system/http_proxy",
			      GCONF_CLIENT_PRELOAD_RECURSIVE, NULL);
	g_signal_connect (gconf_client, "value_changed",
			  G_CALLBACK (gconf_value_changed),
			  NULL);
	update_proxy_settings ();

	if (id) {
		g_mutex_unlock (id->lock);
		g_cond_signal (id->cond);
	}
	return FALSE;
}

static void
soup_proxy_resolver_gnome_init (SoupProxyResolverGNOME *resolver_gnome)
{
	GMainContext *default_context;

	G_LOCK (resolver_gnome);
	if (!gconf_client) {
		/* GConf is not thread-safe, and we might be running
		 * in some random thread right now while other
		 * GConf-related activity is going on in the main
		 * thread. To prevent badness, we try to claim the
		 * default GMainContext; if we succeed, then either
		 * we're in the thread of the default GMainContext, or
		 * else there isn't currently any thread running the
		 * default GMainContext (meaning either the main loop
		 * hasn't been started yet, or else there is no main
		 * loop). Either way, it's safe to use GConf.
		 *
		 * If we can't manage to acquire the default
		 * GMainContext, then that means another thread
		 * already has it, so we use g_idle_add() to ask that
		 * thread to do the GConf initialization, and wait
		 * for that thread to finish.
		 */
		default_context = g_main_context_default ();
		if (g_main_context_acquire (default_context)) {
			init_gconf (NULL);
			g_main_context_release (default_context);
		} else {
			SoupProxyResolverGNOMEInitData id;

			id.lock = g_mutex_new ();
			id.cond = g_cond_new ();

			g_mutex_lock (id.lock);
			g_idle_add (init_gconf, &id);
			g_cond_wait (id.cond, id.lock);
			g_mutex_unlock (id.lock);

			g_cond_free (id.cond);
			g_mutex_free (id.lock);
		}
	}
	G_UNLOCK(resolver_gnome);
}

static void
soup_proxy_resolver_gnome_class_init (SoupProxyResolverGNOMEClass *gconf_class)
{
}

static void
soup_proxy_resolver_gnome_interface_init (SoupProxyURIResolverInterface *proxy_uri_resolver_interface)
{
	proxy_uri_resolver_interface->get_proxy_uri_async = get_proxy_uri_async;
	proxy_uri_resolver_interface->get_proxy_uri_sync = get_proxy_uri_sync;
}

#define SOUP_GCONF_PROXY_MODE           "/system/proxy/mode"
#define SOUP_GCONF_PROXY_AUTOCONFIG_URL "/system/proxy/autoconfig_url"
#define SOUP_GCONF_HTTP_PROXY_HOST      "/system/http_proxy/host"
#define SOUP_GCONF_HTTP_PROXY_PORT      "/system/http_proxy/port"
#define SOUP_GCONF_HTTP_USE_AUTH        "/system/http_proxy/use_authentication"
#define SOUP_GCONF_HTTP_PROXY_USER      "/system/http_proxy/authentication_user"
#define SOUP_GCONF_HTTP_PROXY_PASSWORD  "/system/http_proxy/authentication_password"
#define SOUP_GCONF_HTTPS_PROXY_HOST     "/system/proxy/secure_host"
#define SOUP_GCONF_HTTPS_PROXY_PORT     "/system/proxy/secure_port"
#define SOUP_GCONF_USE_SAME_PROXY       "/system/http_proxy/use_same_proxy"
#define SOUP_GCONF_PROXY_IGNORE_HOSTS   "/system/http_proxy/ignore_hosts"

typedef enum {
	SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_NONE,
	SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_MANUAL,
	SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_AUTO
} SoupProxyResolverGNOMEGConfMode;

static GConfEnumStringPair proxy_mode_map [] = {
	{ SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_NONE,   "none"   },
	{ SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_MANUAL, "manual" },
	{ SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_AUTO,   "auto"   },
	{ 0, NULL }
};

static void
update_proxy_settings (void)
{
	SoupProxyResolverGNOMEGConfMode proxy_mode;
	char *mode, *no_proxy = NULL;
	char *autoconfig_url, *host;
	guint port;
	GSList *ignore;
	gboolean need_libproxy;

	/* resolver_gnome is locked */

	if (http_proxy) {
		g_free (http_proxy);
		http_proxy = NULL;
	}
	if (https_proxy) {
		g_free (https_proxy);
		https_proxy = NULL;
	}
	if (proxy_user) {
		g_free (proxy_user);
		proxy_user = NULL;
	}
	if (proxy_password) {
		memset (proxy_password, 0, strlen (proxy_password));
		g_free (proxy_password);
		proxy_password = NULL;
	}

#ifdef HAVE_LIBPROXY_WITH_NON_THREAD_SAFE_GNOME_MODULE
	if (overrode_environment) {
		g_unsetenv ("PX_CONFIG_ORDER");
		g_unsetenv ("http_proxy");
		g_unsetenv ("https_proxy");
		g_unsetenv ("no_proxy");
		overrode_environment = FALSE;
	}
#endif

	/* Get new settings */
	mode = gconf_client_get_string (
		gconf_client, SOUP_GCONF_PROXY_MODE, NULL);
	if (!mode || !gconf_string_to_enum (proxy_mode_map, mode,
					    (int *)&proxy_mode))
		proxy_mode = SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_NONE;
	g_free (mode);

	switch (proxy_mode) {
	case SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_NONE:
		/* If GConf says "no", but the environment variables are
		 * set, then use the environment variables
		 */
		http_proxy = g_strdup (g_getenv ("http_proxy"));
		https_proxy = g_strdup (g_getenv ("https_proxy"));
		no_proxy = g_strdup (g_getenv ("no_proxy"));
		break;

	case SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_AUTO:
		autoconfig_url = gconf_client_get_string (
			gconf_client, SOUP_GCONF_PROXY_AUTOCONFIG_URL, NULL);
		if (autoconfig_url && !strncmp (autoconfig_url, "http", 4))
			http_proxy = g_strconcat ("pac+", autoconfig_url, NULL);
		else
			http_proxy = g_strdup ("wpad://");
		https_proxy = g_strdup (http_proxy);
		g_free (autoconfig_url);
		goto do_ignore_list;

	case SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_MANUAL:
		host = gconf_client_get_string (
			gconf_client, SOUP_GCONF_HTTP_PROXY_HOST, NULL);
		if (!host || !*host) {
			g_free (host);
			proxy_mode = SOUP_PROXY_RESOLVER_GNOME_GCONF_MODE_NONE;
			return;
		}
		port = gconf_client_get_int (
			gconf_client, SOUP_GCONF_HTTP_PROXY_PORT, NULL);

		if (port) {
			http_proxy = g_strdup_printf ("http://%s:%u",
						      host, port);
		} else
			http_proxy = g_strdup_printf ("http://%s", host);
		g_free (host);

		if (!gconf_client_get_bool (gconf_client, SOUP_GCONF_USE_SAME_PROXY, NULL)) {
			host = gconf_client_get_string (
				gconf_client, SOUP_GCONF_HTTPS_PROXY_HOST, NULL);
			port = gconf_client_get_int (
				gconf_client, SOUP_GCONF_HTTPS_PROXY_PORT, NULL);

			if (host && *host) {
				if (port) {
					https_proxy = g_strdup_printf (
						"http://%s:%u", host, port);
				} else {
					https_proxy = g_strdup_printf (
						"http://%s", host);
				}
			}
			g_free (host);
		}

		if (gconf_client_get_bool (gconf_client, SOUP_GCONF_HTTP_USE_AUTH, NULL)) {
			proxy_user = gconf_client_get_string (
				gconf_client, SOUP_GCONF_HTTP_PROXY_USER, NULL);
			proxy_password = gconf_client_get_string (
				gconf_client, SOUP_GCONF_HTTP_PROXY_PASSWORD, NULL);
		}

		/* fall through */

	do_ignore_list:
		ignore = gconf_client_get_list (
			gconf_client, SOUP_GCONF_PROXY_IGNORE_HOSTS,
			GCONF_VALUE_STRING, NULL);
		if (ignore) {
			GString *ignore_list;
			GSList *i;

			ignore_list = g_string_new (NULL);
			for (i = ignore; i; i = i->next) {
				if (ignore_list->len)
					g_string_append_c (ignore_list, ',');
				g_string_append (ignore_list, i->data);
				g_free (i->data);
			}
			g_slist_free (ignore);
			no_proxy = g_string_free (ignore_list, FALSE);
		}
		break;
	}

	/* We need to use libproxy if (a) there is an ignore list, or
	 * (b) we're using PAC or WPAD.
	 */
	if (no_proxy)
		need_libproxy = TRUE;
	else if (http_proxy &&
		 (g_str_has_prefix (http_proxy, "pac+") ||
		  g_str_has_prefix (http_proxy, "wpad:")))
		need_libproxy = TRUE;
	else if (https_proxy &&
		 (g_str_has_prefix (https_proxy, "pac+") ||
		  g_str_has_prefix (https_proxy, "wpad:")))
		need_libproxy = TRUE;
	else
		need_libproxy = FALSE;

	if (!need_libproxy) {
		if (libproxy_factory) {
			px_proxy_factory_free (libproxy_factory);
			libproxy_factory = NULL;
		}
		if (libproxy_threadpool) {
			g_thread_pool_free (libproxy_threadpool, FALSE, FALSE);
			libproxy_threadpool = NULL;
		}
		return;
	}

	/* If we have a "bad" libproxy, set environment variables
	 * and force it to use them.
	 */
#ifdef HAVE_LIBPROXY_WITH_NON_THREAD_SAFE_GNOME_MODULE
	g_setenv ("PX_CONFIG_ORDER", "envvar", TRUE);
	if (http_proxy)
		g_setenv ("http_proxy", http_proxy, TRUE);
	else
		g_unsetenv ("http_proxy");
	if (https_proxy)
		g_setenv ("https_proxy", https_proxy, TRUE);
	else
		g_unsetenv ("https_proxy");
	if (no_proxy)
		g_setenv ("no_proxy", no_proxy, TRUE);
	else
		g_unsetenv ("no_proxy");
	overrode_environment = TRUE;
#endif

	/* If we haven't created a proxy factory or thread pool yet,
	 * do so. If we already have one, we don't need to update
	 * anything, because it rechecks the environment variables, etc
	 * every time.
	 */
	if (!libproxy_factory)
		libproxy_factory = px_proxy_factory_new ();

	if (!libproxy_threadpool) {
		libproxy_threadpool =
			g_thread_pool_new (libproxy_threadpool_func,
					   NULL, -1, FALSE, NULL);
	}
}

static void
gconf_value_changed (GConfClient *client, const char *key,
		     GConfValue *value, gpointer user_data)
{
	G_LOCK (resolver_gnome);
	update_proxy_settings ();
	G_UNLOCK (resolver_gnome);
}

static guint
get_proxy_for_uri_direct (SoupURI *uri, SoupURI **proxy_uri)
{
	/* resolver_gnome is locked */

	if (uri->scheme == SOUP_URI_SCHEME_HTTP && http_proxy)
		*proxy_uri = soup_uri_new (http_proxy);
	else if (uri->scheme == SOUP_URI_SCHEME_HTTPS && https_proxy)
		*proxy_uri = soup_uri_new (https_proxy);
	else
		*proxy_uri = NULL;

	if (*proxy_uri && proxy_user) {
		soup_uri_set_user (*proxy_uri, proxy_user);
		soup_uri_set_password (*proxy_uri, proxy_password);
	}

	return SOUP_STATUS_OK;
}

static guint
get_proxy_for_uri_via_libproxy (SoupURI *uri, SoupURI **proxy_uri)
{
	char *uristr, **proxies;
	gboolean got_proxy;
	int i;

	*proxy_uri = NULL;

	/* resolver_gnome is locked */

	uristr = soup_uri_to_string (uri, FALSE);
	proxies = px_proxy_factory_get_proxies (libproxy_factory, uristr);
	g_free (uristr);

	if (!proxies)
		return SOUP_STATUS_OK;

	got_proxy = FALSE;
	for (i = 0; proxies[i]; i++) {
		if (!strcmp (proxies[i], "direct://")) {
			got_proxy = TRUE;
			break;
		}
		if (strncmp (proxies[i], "http://", 7) == 0) {
			*proxy_uri = soup_uri_new (proxies[i]);
			got_proxy = TRUE;
			break;
		}
	}
	for (i = 0; proxies[i]; i++)
		free (proxies[i]);
	free (proxies);

	if (got_proxy) {
		if (*proxy_uri && proxy_user) {
			soup_uri_set_user (*proxy_uri, proxy_user);
			soup_uri_set_password (*proxy_uri, proxy_password);
		}

		return SOUP_STATUS_OK;
	} else
		return SOUP_STATUS_CANT_RESOLVE_PROXY;
}

typedef struct {
	SoupProxyURIResolver *proxy_uri_resolver;
	SoupURI *uri, *proxy_uri;
	GMainContext *async_context;
	GCancellable *cancellable;
	guint status;
	SoupProxyURIResolverCallback callback;
	gpointer user_data;
} SoupGNOMEAsyncData;

static gboolean
resolved_proxy (gpointer data)
{
	SoupGNOMEAsyncData *sgad = data;

	sgad->callback (sgad->proxy_uri_resolver, sgad->status,
			sgad->proxy_uri, sgad->user_data);
	g_object_unref (sgad->proxy_uri_resolver);
	if (sgad->uri)
		soup_uri_free (sgad->uri);
	if (sgad->async_context)
		g_main_context_unref (sgad->async_context);
	if (sgad->cancellable)
		g_object_unref (sgad->cancellable);
	if (sgad->proxy_uri)
		soup_uri_free (sgad->proxy_uri);
	g_slice_free (SoupGNOMEAsyncData, sgad);

	return FALSE;
}

static void
libproxy_threadpool_func (gpointer user_data, gpointer thread_data)
{
	SoupGNOMEAsyncData *sgad = user_data;

	/* We don't just call get_proxy_for_uri here, since it's
	 * possible that the proxy mode has changed...
	 */
	sgad->status = get_proxy_uri_sync (sgad->proxy_uri_resolver,
					   sgad->uri, sgad->cancellable,
					   &sgad->proxy_uri);
	soup_add_completion (sgad->async_context, resolved_proxy, sgad);
}

static void
get_proxy_uri_async (SoupProxyURIResolver  *proxy_uri_resolver,
		     SoupURI               *uri,
		     GMainContext          *async_context,
		     GCancellable          *cancellable,
		     SoupProxyURIResolverCallback callback,
		     gpointer               user_data)
{
	SoupGNOMEAsyncData *sgad;

	sgad = g_slice_new0 (SoupGNOMEAsyncData);
	sgad->proxy_uri_resolver = g_object_ref (proxy_uri_resolver);
	sgad->cancellable = cancellable ? g_object_ref (cancellable) : NULL;
	sgad->callback = callback;
	sgad->user_data = user_data;

	G_LOCK (resolver_gnome);
	if (libproxy_threadpool) {
		/* FIXME: cancellable */
		sgad->uri = soup_uri_copy (uri);
		sgad->async_context = async_context ? g_main_context_ref (async_context) : NULL;
		g_thread_pool_push (libproxy_threadpool, sgad, NULL);
	} else {
		sgad->status = get_proxy_for_uri_direct (uri, &sgad->proxy_uri);
		soup_add_completion (async_context, resolved_proxy, sgad);
	}
	G_UNLOCK (resolver_gnome);
}

static guint
get_proxy_uri_sync (SoupProxyURIResolver  *proxy_uri_resolver,
		    SoupURI               *uri,
		    GCancellable          *cancellable,
		    SoupURI              **proxy_uri)
{
	guint status;

	G_LOCK (resolver_gnome);
	if (libproxy_factory)
		status = get_proxy_for_uri_via_libproxy (uri, proxy_uri);
	else
		status = get_proxy_for_uri_direct (uri, proxy_uri);
	G_UNLOCK (resolver_gnome);

	return status;
}
