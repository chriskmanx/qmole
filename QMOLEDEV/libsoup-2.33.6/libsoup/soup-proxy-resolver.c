/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-proxy-resolver.c: HTTP proxy resolver interface
 *
 * Copyright (C) 2008 Red Hat, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "soup-proxy-resolver.h"
#include "soup-proxy-uri-resolver.h"
#include "soup-address.h"
#include "soup-message.h"
#include "soup-session-feature.h"
#include "soup-uri.h"

static void soup_proxy_resolver_interface_init (GTypeInterface *interface);
static void soup_proxy_resolver_uri_resolver_interface_init (SoupProxyURIResolverInterface *uri_resolver_interface);

GType
soup_proxy_resolver_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
  if (g_once_init_enter (&g_define_type_id__volatile))
    {
      GType g_define_type_id =
        g_type_register_static_simple (G_TYPE_INTERFACE,
                                       g_intern_static_string ("SoupProxyResolver"),
                                       sizeof (SoupProxyResolverInterface),
                                       (GClassInitFunc)soup_proxy_resolver_interface_init,
                                       0,
                                       (GInstanceInitFunc)NULL,
                                       (GTypeFlags) 0);
      g_type_interface_add_prerequisite (g_define_type_id, G_TYPE_OBJECT);
      g_type_interface_add_prerequisite (g_define_type_id, SOUP_TYPE_SESSION_FEATURE);
      g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
    }
  return g_define_type_id__volatile;
}

static void
proxy_resolver_interface_check (gpointer func_data, gpointer g_iface)
{
	GTypeInterface *interface = g_iface;

	if (interface->g_type != SOUP_TYPE_PROXY_RESOLVER)
		return;

	/* If the class hasn't already declared that it implements
	 * SoupProxyURIResolver, add our own compat implementation.
	 */
	if (!g_type_is_a (interface->g_instance_type, SOUP_TYPE_PROXY_URI_RESOLVER)) {
		const GInterfaceInfo uri_resolver_interface_info = {
			(GInterfaceInitFunc) soup_proxy_resolver_uri_resolver_interface_init, NULL, NULL
		};
		g_type_add_interface_static (interface->g_instance_type,
					     SOUP_TYPE_PROXY_URI_RESOLVER,
					     &uri_resolver_interface_info);
	}
}


static void
soup_proxy_resolver_interface_init (GTypeInterface *interface)
{
	/* Add an interface_check where we can kludgily add the
	 * SoupProxyURIResolver interface to all SoupProxyResolvers.
	 * (SoupProxyResolver can't just implement
	 * SoupProxyURIResolver itself because interface types can't
	 * implement other interfaces.) This is an ugly hack, but it
	 * only gets used if someone actually creates a
	 * SoupProxyResolver...
	 */
	g_type_add_interface_check (NULL, proxy_resolver_interface_check);
}

void
soup_proxy_resolver_get_proxy_async (SoupProxyResolver  *proxy_resolver,
				     SoupMessage        *msg,
				     GMainContext       *async_context,
				     GCancellable       *cancellable,
				     SoupProxyResolverCallback callback,
				     gpointer            user_data)
{
	SOUP_PROXY_RESOLVER_GET_CLASS (proxy_resolver)->
		get_proxy_async (proxy_resolver, msg,
				 async_context, cancellable,
				 callback, user_data);
}

guint
soup_proxy_resolver_get_proxy_sync (SoupProxyResolver  *proxy_resolver,
				    SoupMessage        *msg,
				    GCancellable       *cancellable,
				    SoupAddress       **addr)
{
	return SOUP_PROXY_RESOLVER_GET_CLASS (proxy_resolver)->
		get_proxy_sync (proxy_resolver, msg, cancellable, addr);
}

/* SoupProxyURIResolver implementation */

static SoupURI *
uri_from_address (SoupAddress *addr)
{
	SoupURI *proxy_uri;

	proxy_uri = soup_uri_new (NULL);
	soup_uri_set_scheme (proxy_uri, SOUP_URI_SCHEME_HTTP);
	soup_uri_set_host (proxy_uri, soup_address_get_name (addr));
	soup_uri_set_port (proxy_uri, soup_address_get_port (addr));
	return proxy_uri;
}

typedef struct {
	SoupProxyURIResolverCallback callback;
	gpointer user_data;
} ProxyURIResolverAsyncData;

static void
compat_got_proxy (SoupProxyResolver *proxy_resolver,
		  SoupMessage *msg, guint status, SoupAddress *proxy_addr,
		  gpointer user_data)
{
	ProxyURIResolverAsyncData *purad = user_data;
	SoupURI *proxy_uri;

	proxy_uri = proxy_addr ? uri_from_address (proxy_addr) : NULL;
	purad->callback (SOUP_PROXY_URI_RESOLVER (proxy_resolver),
			 status, proxy_uri, purad->user_data);
	g_object_unref (msg);
	if (proxy_uri)
		soup_uri_free (proxy_uri);
	g_slice_free (ProxyURIResolverAsyncData, purad);
}

static void
compat_get_proxy_uri_async (SoupProxyURIResolver *proxy_uri_resolver,
			    SoupURI *uri, GMainContext *async_context,
			    GCancellable *cancellable,
			    SoupProxyURIResolverCallback callback,
			    gpointer user_data)
{
	SoupMessage *dummy_msg;
	ProxyURIResolverAsyncData *purad;

	dummy_msg = soup_message_new_from_uri (SOUP_METHOD_GET, uri);

	purad = g_slice_new (ProxyURIResolverAsyncData);
	purad->callback = callback;
	purad->user_data = user_data;

	soup_proxy_resolver_get_proxy_async (
		SOUP_PROXY_RESOLVER (proxy_uri_resolver), dummy_msg,
		async_context, cancellable,
		compat_got_proxy, purad);
}

static guint
compat_get_proxy_uri_sync (SoupProxyURIResolver *proxy_uri_resolver,
			   SoupURI *uri, GCancellable *cancellable,
			   SoupURI **proxy_uri)
{
	SoupMessage *dummy_msg;
	SoupAddress *proxy_addr = NULL;
	guint status;

	dummy_msg = soup_message_new_from_uri (SOUP_METHOD_GET, uri);
	status = soup_proxy_resolver_get_proxy_sync (
		SOUP_PROXY_RESOLVER (proxy_uri_resolver), dummy_msg,
		cancellable, &proxy_addr);
	g_object_unref (dummy_msg);
	if (!proxy_addr)
		return status;

	*proxy_uri = uri_from_address (proxy_addr);
	g_object_unref (proxy_addr);
	return status;
}

static void
soup_proxy_resolver_uri_resolver_interface_init (SoupProxyURIResolverInterface *uri_resolver_interface)
{
	uri_resolver_interface->get_proxy_uri_async = compat_get_proxy_uri_async;
	uri_resolver_interface->get_proxy_uri_sync = compat_get_proxy_uri_sync;
}
