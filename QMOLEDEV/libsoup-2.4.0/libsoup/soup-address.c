/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-address.c: Internet address handing
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "soup-address.h"
#include "soup-dns.h"
#include "soup-enum-types.h"
#include "soup-marshal.h"
#include "soup-misc.h"

#ifndef INET_ADDRSTRLEN
#  define INET_ADDRSTRLEN 16
#  define INET6_ADDRSTRLEN 46
#endif

#ifndef INADDR_NONE
#define INADDR_NONE -1
#endif

/**
 * SECTION:soup-address
 * @short_description: DNS support
 *
 * #SoupAddress represents the address of a TCP connection endpoint:
 * both the IP address and the port. (It is somewhat like an
 * object-oriented version of struct sockaddr.)
 *
 * If libsoup was built with IPv6 support, #SoupAddress will allow
 * both IPv4 and IPv6 addresses.
 **/

enum {
	PROP_0,

	PROP_NAME,
	PROP_FAMILY,
	PROP_PORT,
	PROP_PHYSICAL,
	PROP_SOCKADDR,

	LAST_PROP
};

typedef struct {
	struct sockaddr *sockaddr;

	char *name, *physical;
	guint port;

	SoupDNSLookup *lookup;
	guint timeout_id;
} SoupAddressPrivate;
#define SOUP_ADDRESS_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), SOUP_TYPE_ADDRESS, SoupAddressPrivate))

/* sockaddr generic macros */
#define SOUP_SIN(priv) ((struct sockaddr_in *)priv->sockaddr)
#ifdef HAVE_IPV6
#define SOUP_SIN6(priv) ((struct sockaddr_in6 *)priv->sockaddr)
#endif

/* sockaddr family macros */
#define SOUP_ADDRESS_GET_FAMILY(priv) (priv->sockaddr->sa_family)
#define SOUP_ADDRESS_SET_FAMILY(priv, family) \
	(priv->sockaddr->sa_family = family)
#ifdef HAVE_IPV6
#define SOUP_ADDRESS_FAMILY_IS_VALID(family) \
	(family == AF_INET || family == AF_INET6)
#define SOUP_ADDRESS_FAMILY_SOCKADDR_SIZE(family) \
	(family == AF_INET ? sizeof (struct sockaddr_in) : \
			     sizeof (struct sockaddr_in6))
#define SOUP_ADDRESS_FAMILY_DATA_SIZE(family) \
	(family == AF_INET ? sizeof (struct in_addr) : \
			     sizeof (struct in6_addr))
#else
#define SOUP_ADDRESS_FAMILY_IS_VALID(family) (family == AF_INET)
#define SOUP_ADDRESS_FAMILY_SOCKADDR_SIZE(family) sizeof (struct sockaddr_in)
#define SOUP_ADDRESS_FAMILY_DATA_SIZE(family) sizeof (struct in_addr)
#endif

/* sockaddr port macros */
#define SOUP_ADDRESS_PORT_IS_VALID(port) (port >= 0 && port <= 65535)
#ifdef HAVE_IPV6
#define SOUP_ADDRESS_GET_PORT(priv) \
	(priv->sockaddr->sa_family == AF_INET ? \
		SOUP_SIN(priv)->sin_port : \
		SOUP_SIN6(priv)->sin6_port)
#define SOUP_ADDRESS_SET_PORT(priv, port) \
	G_STMT_START {					\
	if (priv->sockaddr->sa_family == AF_INET)	\
		SOUP_SIN(priv)->sin_port = port;	\
	else						\
		SOUP_SIN6(priv)->sin6_port = port;	\
	} G_STMT_END
#else
#define SOUP_ADDRESS_GET_PORT(priv) (SOUP_SIN(priv)->sin_port)
#define SOUP_ADDRESS_SET_PORT(priv, port) (SOUP_SIN(priv)->sin_port = port)
#endif

/* sockaddr data macros */
#ifdef HAVE_IPV6
#define SOUP_ADDRESS_GET_DATA(priv) \
	(priv->sockaddr->sa_family == AF_INET ? \
		(gpointer)&SOUP_SIN(priv)->sin_addr : \
		(gpointer)&SOUP_SIN6(priv)->sin6_addr)
#else
#define SOUP_ADDRESS_GET_DATA(priv) ((gpointer)&SOUP_SIN(priv)->sin_addr)
#endif
#define SOUP_ADDRESS_SET_DATA(priv, data, length) \
	memcpy (SOUP_ADDRESS_GET_DATA (priv), data, length)


static GObject *constructor (GType                  type,
			     guint                  n_construct_properties,
			     GObjectConstructParam *construct_properties);
static void set_property (GObject *object, guint prop_id,
			  const GValue *value, GParamSpec *pspec);
static void get_property (GObject *object, guint prop_id,
			  GValue *value, GParamSpec *pspec);

G_DEFINE_TYPE (SoupAddress, soup_address, G_TYPE_OBJECT)

static void
soup_address_init (SoupAddress *addr)
{
}

static void
finalize (GObject *object)
{
	SoupAddress *addr = SOUP_ADDRESS (object);
	SoupAddressPrivate *priv = SOUP_ADDRESS_GET_PRIVATE (addr);

	if (priv->sockaddr)
		g_free (priv->sockaddr);
	if (priv->name)
		g_free (priv->name);
	if (priv->physical)
		g_free (priv->physical);

	if (priv->lookup)
		soup_dns_lookup_free (priv->lookup);
	if (priv->timeout_id)
		g_source_remove (priv->timeout_id);

	G_OBJECT_CLASS (soup_address_parent_class)->finalize (object);
}

static void
soup_address_class_init (SoupAddressClass *address_class)
{
	GObjectClass *object_class = G_OBJECT_CLASS (address_class);

	soup_dns_init ();

	g_type_class_add_private (address_class, sizeof (SoupAddressPrivate));

	/* virtual method override */
	object_class->constructor = constructor;
	object_class->finalize = finalize;
	object_class->set_property = set_property;
	object_class->get_property = get_property;

	/* properties */
	g_object_class_install_property (
		object_class, PROP_NAME,
		g_param_spec_string (SOUP_ADDRESS_NAME,
				     "Name",
				     "Hostname for this address",
				     NULL,
				     G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_FAMILY,
		g_param_spec_enum (SOUP_ADDRESS_FAMILY,
				   "Family",
				   "Address family for this address",
				   SOUP_TYPE_ADDRESS_FAMILY,
				   SOUP_ADDRESS_FAMILY_INVALID,
				   G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_PORT,
		g_param_spec_int (SOUP_ADDRESS_PORT,
				  "Port",
				  "Port for this address",
				  -1, 65535, -1,
				  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
	g_object_class_install_property (
		object_class, PROP_PHYSICAL,
		g_param_spec_string (SOUP_ADDRESS_PHYSICAL,
				     "Physical address",
				     "IP address for this address",
				     NULL,
				     G_PARAM_READABLE));
	g_object_class_install_property (
		object_class, PROP_SOCKADDR,
		g_param_spec_pointer (SOUP_ADDRESS_SOCKADDR,
				      "sockaddr",
				      "struct sockaddr for this address",
				      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

#ifdef G_OS_WIN32
	/* This hopefully is a good place to call WSAStartup */
	{
		WSADATA wsadata;
		if (WSAStartup (MAKEWORD (2, 0), &wsadata) != 0)
			g_error ("Windows Sockets could not be initialized");
	}
#endif
}

static GObject *
constructor (GType                  type,
	     guint                  n_construct_properties,
	     GObjectConstructParam *construct_properties)
{
	GObject *addr;
	SoupAddressPrivate *priv;

	addr = G_OBJECT_CLASS (soup_address_parent_class)->constructor (
		type, n_construct_properties, construct_properties);
	if (!addr)
		return NULL;
	priv = SOUP_ADDRESS_GET_PRIVATE (addr);

	if (priv->name) {
		if (!priv->sockaddr)
			priv->lookup = soup_dns_lookup_name (priv->name);
	} else if (priv->sockaddr)
		priv->lookup = soup_dns_lookup_address (priv->sockaddr);
	else {
		g_object_unref (addr);
		return NULL;
	}

	return addr;
}

static void
set_property (GObject *object, guint prop_id,
	      const GValue *value, GParamSpec *pspec)
{
	SoupAddressPrivate *priv = SOUP_ADDRESS_GET_PRIVATE (object);
	SoupAddressFamily family;
	struct sockaddr *sa;
	int len, port;

	/* This is a mess because the properties are mostly orthogonal,
	 * but g_object_constructor wants to set a default value for each
	 * of them.
	 */

	switch (prop_id) {
	case PROP_NAME:
		priv->name = g_value_dup_string (value);
		break;

	case PROP_FAMILY:
		family = g_value_get_enum (value);
		if (family == SOUP_ADDRESS_FAMILY_INVALID)
			return;
		g_return_if_fail (SOUP_ADDRESS_FAMILY_IS_VALID (family));
		g_return_if_fail (priv->sockaddr == NULL);

		priv->sockaddr = g_malloc0 (SOUP_ADDRESS_FAMILY_SOCKADDR_SIZE (family));
		SOUP_ADDRESS_SET_FAMILY (priv, family);
		SOUP_ADDRESS_SET_PORT (priv, htons (priv->port));
		break;

	case PROP_PORT:
		port = g_value_get_int (value);
		if (port == -1)
			return;
		g_return_if_fail (SOUP_ADDRESS_PORT_IS_VALID (port));

		priv->port = port;
		if (priv->sockaddr)
			SOUP_ADDRESS_SET_PORT (priv, htons (port));
		break;

	case PROP_SOCKADDR:
		sa = g_value_get_pointer (value);
		if (!sa)
			return;
		g_return_if_fail (priv->sockaddr == NULL);

		len = SOUP_ADDRESS_FAMILY_SOCKADDR_SIZE (sa->sa_family);
		priv->sockaddr = g_memdup (sa, len);
		priv->port = ntohs (SOUP_ADDRESS_GET_PORT (priv));
		break;
	default:
		break;
	}
}

static void
get_property (GObject *object, guint prop_id,
	      GValue *value, GParamSpec *pspec)
{
	SoupAddressPrivate *priv = SOUP_ADDRESS_GET_PRIVATE (object);

	switch (prop_id) {
	case PROP_NAME:
		g_value_set_string (value, priv->name);
		break;
	case PROP_FAMILY:
		if (priv->sockaddr)
			g_value_set_enum (value, SOUP_ADDRESS_GET_FAMILY (priv));
		else
			g_value_set_enum (value, 0);
		break;
	case PROP_PORT:
		g_value_set_int (value, priv->port);
		break;
	case PROP_PHYSICAL:
		g_value_set_string (value, soup_address_get_physical (SOUP_ADDRESS (object)));
		break;
	case PROP_SOCKADDR:
		g_value_set_pointer (value, priv->sockaddr);
		break;
	default:
		break;
	}
}

/**
 * soup_address_new:
 * @name: a hostname or physical address
 * @port: a port number
 *
 * Creates a #SoupAddress from @name and @port. The #SoupAddress's IP
 * address may not be available right away; the caller can call
 * soup_address_resolve_async() or soup_address_resolve_sync() to
 * force a DNS resolution.
 *
 * Return value: a #SoupAddress
 **/
SoupAddress *
soup_address_new (const char *name, guint port)
{
	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (SOUP_ADDRESS_PORT_IS_VALID (port), NULL);

	return g_object_new (SOUP_TYPE_ADDRESS,
			     SOUP_ADDRESS_NAME, name,
			     SOUP_ADDRESS_PORT, port,
			     NULL);
}

/**
 * soup_address_new_from_sockaddr:
 * @sa: a pointer to a sockaddr
 * @len: size of @sa
 *
 * Returns a #SoupAddress equivalent to @sa (or %NULL if @sa's
 * address family isn't supported)
 *
 * Return value: the new #SoupAddress
 **/
SoupAddress *
soup_address_new_from_sockaddr (struct sockaddr *sa, int len)
{
	g_return_val_if_fail (sa != NULL, NULL);
	g_return_val_if_fail (SOUP_ADDRESS_FAMILY_IS_VALID (sa->sa_family), NULL);
	g_return_val_if_fail (len == SOUP_ADDRESS_FAMILY_SOCKADDR_SIZE (sa->sa_family), NULL);

	return g_object_new (SOUP_TYPE_ADDRESS,
			     SOUP_ADDRESS_SOCKADDR, sa,
			     NULL);
}

/**
 * SoupAddressFamily:
 * @SOUP_ADDRESS_FAMILY_INVALID: an invalid %SoupAddress
 * @SOUP_ADDRESS_FAMILY_IPV4: an IPv4 address
 * @SOUP_ADDRESS_FAMILY_IPV6: an IPv6 address
 *
 * The supported address families. Note that the
 * %SOUP_ADDRESS_FAMILY_IPV6 constant is available even if libsoup was
 * built without IPv6 support, but attempting to create an IPv6
 * address will fail in that case.
 **/

/**
 * SOUP_ADDRESS_ANY_PORT:
 *
 * This can be passed to any #SoupAddress method that expects a port,
 * to indicate that you don't care what port is used.
 **/

/**
 * soup_address_new_any:
 * @family: the address family
 * @port: the port number (usually %SOUP_ADDRESS_ANY_PORT)
 *
 * Returns a #SoupAddress corresponding to the "any" address
 * for @family (or %NULL if @family isn't supported), suitable for
 * passing to soup_socket_server_new().
 *
 * Return value: the new #SoupAddress
 **/
SoupAddress *
soup_address_new_any (SoupAddressFamily family, guint port)
{
	g_return_val_if_fail (SOUP_ADDRESS_FAMILY_IS_VALID (family), NULL);
	g_return_val_if_fail (SOUP_ADDRESS_PORT_IS_VALID (port), NULL);

	return g_object_new (SOUP_TYPE_ADDRESS,
			     SOUP_ADDRESS_FAMILY, family,
			     SOUP_ADDRESS_PORT, port,
			     NULL);
}

/**
 * soup_address_get_name:
 * @addr: a #SoupAddress
 *
 * Returns the hostname associated with @addr.
 *
 * Return value: the hostname, or %NULL if it is not known.
 **/
const char *
soup_address_get_name (SoupAddress *addr)
{
	g_return_val_if_fail (SOUP_IS_ADDRESS (addr), NULL);

	return SOUP_ADDRESS_GET_PRIVATE (addr)->name;
}

/**
 * soup_address_get_sockaddr:
 * @addr: a #SoupAddress
 * @len: return location for sockaddr length
 *
 * Returns the sockaddr associated with @addr, with its length in
 * *@len. If the sockaddr is not yet known, returns %NULL.
 *
 * Return value: the sockaddr, or %NULL
 **/
struct sockaddr *
soup_address_get_sockaddr (SoupAddress *addr, int *len)
{
	SoupAddressPrivate *priv;

	g_return_val_if_fail (SOUP_IS_ADDRESS (addr), NULL);
	priv = SOUP_ADDRESS_GET_PRIVATE (addr);

	if (priv->sockaddr && len)
		*len = SOUP_ADDRESS_FAMILY_SOCKADDR_SIZE (SOUP_ADDRESS_GET_FAMILY (priv));

	return priv->sockaddr;
}

/**
 * soup_address_get_physical:
 * @addr: a #SoupAddress
 *
 * Returns the physical address associated with @addr as a string.
 * (Eg, "127.0.0.1"). If the address is not yet known, returns %NULL.
 *
 * Return value: the physical address, or %NULL
 **/
const char *
soup_address_get_physical (SoupAddress *addr)
{
	SoupAddressPrivate *priv;

	g_return_val_if_fail (SOUP_IS_ADDRESS (addr), NULL);
	priv = SOUP_ADDRESS_GET_PRIVATE (addr);

	if (!priv->sockaddr)
		return NULL;

	if (!priv->physical)
		priv->physical = soup_dns_ntop (priv->sockaddr);

	return priv->physical;
}

/**
 * soup_address_get_port:
 * @addr: a #SoupAddress
 *
 * Returns the port associated with @addr.
 *
 * Return value: the port
 **/
guint
soup_address_get_port (SoupAddress *addr)
{
	g_return_val_if_fail (SOUP_IS_ADDRESS (addr), 0);

	return SOUP_ADDRESS_GET_PRIVATE (addr)->port;
}


static void
update_address (SoupAddress *addr, SoupDNSLookup *lookup)
{
	SoupAddressPrivate *priv = SOUP_ADDRESS_GET_PRIVATE (addr);

	if (!priv->name)
		priv->name = soup_dns_lookup_get_hostname (lookup);

	if (!priv->sockaddr) {
		priv->sockaddr = soup_dns_lookup_get_address (lookup);
		SOUP_ADDRESS_SET_PORT (priv, htons (priv->port));
	}
}

typedef struct {
	SoupAddress         *addr;
	SoupAddressCallback  callback;
	gpointer             callback_data;
} SoupAddressResolveAsyncData;

static void
lookup_resolved (SoupDNSLookup *lookup, guint status, gpointer user_data)
{
	SoupAddressResolveAsyncData *res_data = user_data;
	SoupAddress *addr;
	SoupAddressCallback callback;
	gpointer callback_data;

	addr = res_data->addr;
	callback = res_data->callback;
	callback_data = res_data->callback_data;
	g_free (res_data);

	if (status == SOUP_STATUS_OK)
		update_address (addr, lookup);

	if (callback)
		callback (addr, status, callback_data);

	g_object_unref (addr);
}

/**
 * SoupAddressCallback:
 * @addr: the #SoupAddress that was resolved
 * @status: %SOUP_STATUS_OK, %SOUP_STATUS_CANT_RESOLVE, or
 * %SOUP_STATUS_CANCELLED
 * @data: the user data that was passed to
 * soup_address_resolve_async()
 *
 * The callback function passed to soup_address_resolve_async().
 **/

/**
 * soup_address_resolve_async:
 * @addr: a #SoupAddress
 * @async_context: the #GMainContext to call @callback from
 * @cancellable: a #GCancellable object, or %NULL
 * @callback: callback to call with the result
 * @user_data: data for @callback
 *
 * Asynchronously resolves the missing half of @addr (its IP address
 * if it was created with soup_address_new(), or its hostname if it
 * was created with soup_address_new_from_sockaddr() or
 * soup_address_new_any().)
 *
 * If @cancellable is non-%NULL, it can be used to cancel the
 * resolution. @callback will still be invoked in this case, with a
 * status of %SOUP_STATUS_CANCELLED.
 **/
void
soup_address_resolve_async (SoupAddress *addr, GMainContext *async_context,
			    GCancellable *cancellable,
			    SoupAddressCallback callback, gpointer user_data)
{
	SoupAddressPrivate *priv;
	SoupAddressResolveAsyncData *res_data;

	g_return_if_fail (SOUP_IS_ADDRESS (addr));
	priv = SOUP_ADDRESS_GET_PRIVATE (addr);

	res_data = g_new (SoupAddressResolveAsyncData, 1);
	res_data->addr          = addr;
	res_data->callback      = callback;
	res_data->callback_data = user_data;

	g_object_ref (addr);
	soup_dns_lookup_resolve_async (priv->lookup, async_context, cancellable,
				       lookup_resolved, res_data);
}

/**
 * soup_address_resolve_sync:
 * @addr: a #SoupAddress
 * @cancellable: a #GCancellable object, or %NULL
 *
 * Synchronously resolves the missing half of @addr, as with
 * soup_address_resolve_async().
 *
 * If @cancellable is non-%NULL, it can be used to cancel the
 * resolution. soup_address_resolve_sync() will then return a status
 * of %SOUP_STATUS_CANCELLED.
 *
 * Return value: %SOUP_STATUS_OK, %SOUP_STATUS_CANT_RESOLVE, or
 * %SOUP_STATUS_CANCELLED.
 **/
guint
soup_address_resolve_sync (SoupAddress *addr, GCancellable *cancellable)
{
	SoupAddressPrivate *priv;
	guint status;

	g_return_val_if_fail (SOUP_IS_ADDRESS (addr), SOUP_STATUS_MALFORMED);
	priv = SOUP_ADDRESS_GET_PRIVATE (addr);

	g_object_ref (addr);
	status = soup_dns_lookup_resolve (priv->lookup, cancellable);
	if (status == SOUP_STATUS_OK)
		update_address (addr, priv->lookup);
	g_object_unref (addr);
	return status;
}
