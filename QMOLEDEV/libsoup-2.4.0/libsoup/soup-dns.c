/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-dns.c: Async DNS code
 *
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <errno.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>

#include "soup-dns.h"
#include "soup-misc.h"
#include "soup-status.h"

#ifndef INET_ADDRSTRLEN
#  define INET_ADDRSTRLEN 16
#  define INET6_ADDRSTRLEN 46
#endif

#ifndef INADDR_NONE
#define INADDR_NONE -1
#endif

#ifdef HAVE_IPV6
#define SOUP_DNS_SOCKADDR_LEN(sa) \
	(sa->sa_family == AF_INET ? sizeof (struct sockaddr_in) : \
				    sizeof (struct sockaddr_in6))
#else
#define SOUP_DNS_SOCKADDR_LEN(sa) sizeof (struct sockaddr_in)
#endif

#ifdef G_OS_WIN32

static int
inet_pton(int af, const char* src, void* dst)
{
	int address_length;
	struct sockaddr_storage sa;
	struct sockaddr_in *sin = (struct sockaddr_in *)&sa;
	struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)&sa;

	switch (af) {
	case AF_INET:
		address_length = sizeof (struct sockaddr_in);
		break;
		
	case AF_INET6:
		address_length = sizeof (struct sockaddr_in6);
		break;
		
	default:
		return -1;
	}
	
	if (WSAStringToAddress ((LPTSTR) src, af, NULL, (LPSOCKADDR) &sa, &address_length) == 0) {
		switch (af) {
		case AF_INET:
			memcpy (dst, &sin->sin_addr, sizeof (struct in_addr));
			break;

		case AF_INET6:
			memcpy (dst, &sin6->sin6_addr, sizeof (struct in6_addr));
			break;
		}
		return 1;
	}
	
	return 0;
}

static const char* 
inet_ntop(int af, const void* src, char* dst, size_t size)
{
	int address_length;
	DWORD string_length = size;
	struct sockaddr_storage sa;
	struct sockaddr_in *sin = (struct sockaddr_in *)&sa;
	struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)&sa;
	
	memset (&sa, 0, sizeof (sa));
	switch (af) {
	case AF_INET:
		address_length = sizeof (struct sockaddr_in);
		sin->sin_family = af;
		memcpy (&sin->sin_addr, src, sizeof (struct in_addr));
		break;
		
	case AF_INET6:
		address_length = sizeof (struct sockaddr_in6);
		sin6->sin6_family = af;
		memcpy (&sin6->sin6_addr, src, sizeof (struct in6_addr));
		break;
		
	default:
		return NULL;
	}
	
	if (WSAAddressToString ((LPSOCKADDR) &sa, address_length, NULL,
				dst, &string_length) == 0)
		return dst;
	
	return NULL;
}

#endif

typedef struct {
	char *entry_name;
	guint ref_count;
	time_t expires;

	char *hostname;
	struct sockaddr *sockaddr;

	gboolean resolved;
	GThread *resolver_thread;
	GSList *async_lookups;
} SoupDNSCacheEntry;

static GHashTable *soup_dns_cache;
#define SOUP_DNS_CACHE_MAX 20

struct SoupDNSLookup {
	SoupDNSCacheEntry *entry;

	GMainContext *async_context;
	GCancellable *cancellable;
	SoupDNSCallback callback;
	gpointer user_data;
};

static GMutex *soup_dns_lock;
static GCond *soup_dns_cond;

#if !defined (HAVE_GETADDRINFO) || !defined (HAVE_GETNAMEINFO)
static GMutex *soup_gethost_lock;
#endif

/**
 * soup_dns_init:
 *
 * Initializes the libsoup DNS system. Must be called before any other
 * soup_dns method is called. Normally this gets called automatically
 * by #SoupAddress's class_init function.
 **/
void
soup_dns_init (void)
{
	static volatile gsize inited_dns = 0;

	if (g_once_init_enter (&inited_dns)) {
		soup_dns_cache = g_hash_table_new (soup_str_case_hash, soup_str_case_equal);
		soup_dns_lock = g_mutex_new ();
		soup_dns_cond = g_cond_new ();
#if !defined (HAVE_GETADDRINFO) || !defined (HAVE_GETNAMEINFO)
		soup_gethost_lock = g_mutex_new ();
#endif

		g_once_init_leave (&inited_dns, TRUE);
	}
}

static void
prune_cache_cb (gpointer key, gpointer value, gpointer data)
{
	SoupDNSCacheEntry *entry = value, **prune_entry = data; 

	if (!*prune_entry || (*prune_entry)->expires > entry->expires)
		*prune_entry = entry;
}

static void
soup_dns_cache_entry_set_from_phys (SoupDNSCacheEntry *entry)
{
	struct sockaddr_in sin;
#ifdef HAVE_IPV6
	struct sockaddr_in6 sin6;
#endif

#ifdef HAVE_IPV6
	memset (&sin6, 0, sizeof (struct sockaddr_in6));
	if (inet_pton (AF_INET6, entry->entry_name, &sin6.sin6_addr) != 0) {
		entry->sockaddr = g_memdup (&sin6, sizeof (struct sockaddr_in6));
		entry->sockaddr->sa_family = AF_INET6;
		return;
	}
#endif /* HAVE_IPV6 */

	memset (&sin, 0, sizeof (struct sockaddr_in));
	if (
#if defined(HAVE_INET_PTON)
		inet_pton (AF_INET, entry->entry_name, &sin.sin_addr) != 0
#elif defined(HAVE_INET_ATON)
		inet_aton (entry->entry_name, &sin.sin_addr) != 0
#else
		(sin.sin_addr.s_addr = inet_addr (entry->entry_name)) &&
		(sin.sin_addr.s_addr != INADDR_NONE)
#endif
		) {
		entry->sockaddr = g_memdup (&sin, sizeof (struct sockaddr_in));
		entry->sockaddr->sa_family = AF_INET;
		return;
	}
}

static void
soup_dns_cache_entry_ref (SoupDNSCacheEntry *entry)
{
	entry->ref_count++;
}

static void
soup_dns_cache_entry_unref (SoupDNSCacheEntry *entry)
{
	if (--entry->ref_count == 0) {
		g_free (entry->entry_name);
		g_free (entry->hostname);
		g_free (entry->sockaddr);

		/* If there were lookups pending, ref_count couldn't
		 * have reached zero. So no cleanup needed there.
		 */

		g_slice_free (SoupDNSCacheEntry, entry);
	}
}

static SoupDNSCacheEntry *
soup_dns_cache_entry_new (const char *name)
{
	SoupDNSCacheEntry *entry;

	entry = g_slice_new0 (SoupDNSCacheEntry);
	entry->entry_name = g_strdup (name);
	entry->ref_count = 2; /* One for the caller, one for the cache */
	soup_dns_cache_entry_set_from_phys (entry);

	if (g_hash_table_size (soup_dns_cache) == SOUP_DNS_CACHE_MAX) {
		SoupDNSCacheEntry *prune_entry = NULL;

		g_hash_table_foreach (soup_dns_cache, prune_cache_cb, &prune_entry);
		if (prune_entry) {
			g_hash_table_remove (soup_dns_cache, prune_entry->entry_name);
			soup_dns_cache_entry_unref (prune_entry);
		}
	}

	entry->expires = time (0) + 60 * 60;
	g_hash_table_insert (soup_dns_cache, entry->entry_name, entry);

	return entry;
}

/**
 * soup_dns_ntop:
 * @sa: pointer to a #sockaddr
 *
 * Converts @sa's address into textual form (eg, "141.213.8.59"), like
 * the standard library function inet_ntop(), except that the returned
 * string must be freed.
 *
 * Return value: the text form or @sa, which must be freed.
 **/
char *
soup_dns_ntop (struct sockaddr *sa)
{
	switch (sa->sa_family) {
	case AF_INET:
	{
		struct sockaddr_in *sin = (struct sockaddr_in *)sa;
#ifdef HAVE_INET_NTOP
		char buffer[INET_ADDRSTRLEN];

		inet_ntop (AF_INET, &sin->sin_addr, buffer, sizeof (buffer));
		return g_strdup (buffer);
#else
		return g_strdup (inet_ntoa (sin->sin_addr));
#endif
	}

#ifdef HAVE_IPV6
	case AF_INET6:
	{
		struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)sa;
		char buffer[INET6_ADDRSTRLEN];

		inet_ntop (AF_INET6, &sin6->sin6_addr, buffer, sizeof (buffer));
		return g_strdup (buffer);
	}
#endif

	default:
		return NULL;
	}
}

static void
resolve_address (SoupDNSCacheEntry *entry)
{
#if defined (HAVE_GETADDRINFO)

	struct addrinfo hints, *res;
	int retval;

	memset (&hints, 0, sizeof (struct addrinfo));
#  ifdef HAVE_AI_ADDRCONFIG
	hints.ai_flags = AI_CANONNAME | AI_ADDRCONFIG;
#  else
	hints.ai_flags = AI_CANONNAME;
#  endif
	hints.ai_family = PF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_protocol = IPPROTO_TCP;

	retval = getaddrinfo (entry->hostname, NULL, &hints, &res);
	if (retval == 0) {
		entry->sockaddr = g_memdup (res->ai_addr, res->ai_addrlen);
		entry->resolved = TRUE;
		freeaddrinfo (res);
	} else
		entry->resolved = (retval != EAI_AGAIN);

#else /* !HAVE_GETADDRINFO */

	struct hostent *h;

	g_mutex_lock (soup_gethost_lock);

	h = gethostbyname (entry->hostname);
	if (h && h->h_addrtype == AF_INET) {
		struct sockaddr_in sin;
		memset (&sin, 0, sizeof (struct sockaddr_in));
		sin.sin_family = AF_INET;
		memcpy (&sin.sin_addr, h->h_addr_list[0], sizeof (struct in_addr));
		entry->sockaddr = g_memdup (&sin, sizeof (struct sockaddr_in));
		entry->resolved = TRUE;
	} else
		entry->resolved = (h || h_errno != TRY_AGAIN);


	g_mutex_unlock (soup_gethost_lock);

#endif
}

static void
resolve_name (SoupDNSCacheEntry *entry)
{
#ifdef HAVE_GETNAMEINFO
	int retval, len = 0;
	char *name = NULL;

	do {
		len += 128;
		name = g_realloc (name, len);
		retval = getnameinfo (entry->sockaddr, SOUP_DNS_SOCKADDR_LEN (entry->sockaddr),
				      name, len, NULL, 0, NI_NAMEREQD);
	} while (
#ifdef EAI_OVERFLOW
		retval == EAI_OVERFLOW
#else
		strlen (name) == len - 1
#endif
		);

	if (retval == 0) {
		entry->hostname = name;
		entry->resolved = TRUE;
	} else {
		g_free (name);
		entry->resolved = (retval != EAI_AGAIN);
	}

#else /* !HAVE_GETNAMEINFO */

	struct sockaddr_in *sin = (struct sockaddr_in *)entry->sockaddr;
	struct hostent *h;

	g_mutex_lock (soup_gethost_lock);

	if (sin->sin_family == AF_INET) {
		h = gethostbyaddr (&sin->sin_addr, sizeof (sin->sin_addr), AF_INET);
		if (h) {
			entry->hostname = g_strdup (h->h_name);
			entry->resolved = TRUE;
		} else
			entry->resolved = (h_errno != TRY_AGAIN);
	}

	g_mutex_unlock (soup_gethost_lock);

#endif /* HAVE_GETNAMEINFO */
}

/* Assumes soup_dns_lock is held */
static SoupDNSCacheEntry *
soup_dns_cache_entry_lookup (const char *name)
{
	SoupDNSCacheEntry *entry;

	entry = g_hash_table_lookup (soup_dns_cache, name);
	if (entry)
		soup_dns_cache_entry_ref (entry);
	return entry;
}

/**
 * soup_dns_lookup_name:
 * @name: a hostname (eg, "www.gnome.org") or physical address
 * (eg, "12.107.209.247").
 *
 * Creates a #SoupDNSLookup for @name. This should be passed to
 * soup_dns_lookup_resolve() or soup_dns_lookup_resolve_async().
 *
 * Returns: a #SoupDNSLookup, which should eventually be freed with
 * soup_dns_lookup_free().
 **/
SoupDNSLookup *
soup_dns_lookup_name (const char *name)
{
	SoupDNSCacheEntry *entry;
	SoupDNSLookup *lookup;

	g_mutex_lock (soup_dns_lock);

	entry = soup_dns_cache_entry_lookup (name);
	if (!entry) {
		entry = soup_dns_cache_entry_new (name);
		entry->hostname = g_strdup (name);
		if (entry->sockaddr)
			entry->resolved = TRUE;
	}

	lookup = g_slice_new0 (SoupDNSLookup);
	lookup->entry = entry;
	g_mutex_unlock (soup_dns_lock);

	return lookup;
}

/**
 * soup_dns_lookup_address:
 * @sockaddr: pointer to a #sockaddr
 *
 * Creates a #SoupDNSLookup for @sockaddr. This should be passed to
 * soup_dns_lookup_resolve() or soup_dns_lookup_resolve_async().
 *
 * Returns: a #SoupDNSLookup, which should eventually be freed with
 * soup_dns_lookup_free()
 **/
SoupDNSLookup *
soup_dns_lookup_address (struct sockaddr *sockaddr)
{
	SoupDNSCacheEntry *entry;
	SoupDNSLookup *lookup;
	char *name;

	name = soup_dns_ntop (sockaddr);
	g_return_val_if_fail (name != NULL, NULL);

	g_mutex_lock (soup_dns_lock);

	entry = soup_dns_cache_entry_lookup (name);
	if (!entry)
		entry = soup_dns_cache_entry_new (name); // FIXME
	g_free (name);

	lookup = g_slice_new0 (SoupDNSLookup);
	lookup->entry = entry;
	g_mutex_unlock (soup_dns_lock);

	return lookup;
}

static inline guint
resolve_status (SoupDNSCacheEntry *entry, GCancellable *cancellable)
{
	if (entry->hostname && entry->sockaddr)
		return SOUP_STATUS_OK;
	else if (g_cancellable_is_cancelled (cancellable))
		return SOUP_STATUS_CANCELLED;
	else
		return SOUP_STATUS_CANT_RESOLVE;
}

static void async_cancel (GCancellable *cancellable, gpointer user_data);

static gboolean
do_async_callback (gpointer user_data)
{
	SoupDNSLookup *lookup = user_data;
	SoupDNSCacheEntry *entry = lookup->entry;
	GCancellable *cancellable = lookup->cancellable;

	lookup->callback (lookup, resolve_status (entry, cancellable),
			  lookup->user_data);
	if (cancellable)
		g_signal_handlers_disconnect_by_func (cancellable, async_cancel, lookup);

	return FALSE;
}

static gpointer
resolver_thread (gpointer user_data)
{
	SoupDNSCacheEntry *entry = user_data;
	GSList *async_lookups;
	SoupDNSLookup *lookup;

	if (entry->hostname == NULL)
		resolve_name (entry);
	else if (entry->sockaddr == NULL)
		resolve_address (entry);

	g_mutex_lock (soup_dns_lock);
	entry->resolver_thread = NULL;

	async_lookups = entry->async_lookups;
	entry->async_lookups = NULL;
	g_mutex_unlock (soup_dns_lock);

	g_cond_broadcast (soup_dns_cond);

	while (async_lookups) {
		lookup = async_lookups->data;
		async_lookups = g_slist_remove (async_lookups, lookup);

		soup_add_idle (lookup->async_context, do_async_callback, lookup);
	}

	soup_dns_cache_entry_unref (entry);
	return NULL;
}

static void
sync_cancel (GCancellable *cancellable, gpointer user_data)
{
	/* We can't actually cancel the resolver thread. So we just
	 * wake up the blocking thread, which will see that
	 * @cancellable has been cancelled and then stop waiting for
	 * the result. If the resolver thread eventually finishes,
	 * its result will make it to the cache.
	 */
	g_cond_broadcast (soup_dns_cond);
}

/**
 * soup_dns_lookup_resolve:
 * @lookup: a #SoupDNSLookup
 * @cancellable: a #GCancellable, or %NULL
 *
 * Synchronously resolves @lookup.
 *
 * Return value: %SOUP_STATUS_OK, %SOUP_STATUS_CANT_RESOLVE, or
 * %SOUP_STATUS_CANCELLED
 **/
guint
soup_dns_lookup_resolve (SoupDNSLookup *lookup, GCancellable *cancellable)
{
	SoupDNSCacheEntry *entry = lookup->entry;
	guint cancel_id = 0;

	g_mutex_lock (soup_dns_lock);

	if (!entry->resolved) {
		if (!entry->resolver_thread) {
			soup_dns_cache_entry_ref (entry);
			entry->resolver_thread =
				g_thread_create (resolver_thread, entry,
						 FALSE, NULL);
		}

		if (cancellable) {
			cancel_id = g_signal_connect (cancellable, "cancelled",
						      G_CALLBACK (sync_cancel),
						      NULL);
		}
	}

	while (entry->resolver_thread &&
	       !g_cancellable_is_cancelled (cancellable))
		g_cond_wait (soup_dns_cond, soup_dns_lock);

	if (cancel_id)
		g_signal_handler_disconnect (cancellable, cancel_id);

	g_mutex_unlock (soup_dns_lock);

	return resolve_status (entry, cancellable);
}

static void
async_cancel (GCancellable *cancellable, gpointer user_data)
{
	SoupDNSLookup *lookup = user_data;
	SoupDNSCacheEntry *entry = lookup->entry;

	/* We can't actually cancel the resolver thread. So we just
	 * remove @lookup from the list of pending async lookups and
	 * invoke its callback now. If the resolver thread eventually
	 * finishes, its result will make it to the cache.
	 */
	g_mutex_lock (soup_dns_lock);

	if (g_slist_find (entry->async_lookups, lookup)) {
		entry->async_lookups = g_slist_remove (entry->async_lookups,
						       lookup);
		soup_add_idle (lookup->async_context, do_async_callback, lookup);
	}

	g_mutex_unlock (soup_dns_lock);
}

/**
 * soup_dns_lookup_resolve_async:
 * @lookup: a #SoupDNSLookup
 * @async_context: #GMainContext to call @callback in
 * @cancellable: a #GCancellable, or %NULL
 * @callback: callback to call when @lookup is resolved
 * @user_data: data to pass to @callback;
 *
 * Tries to asynchronously resolve @lookup. Invokes @callback when it
 * has succeeded or failed.
 **/
void
soup_dns_lookup_resolve_async (SoupDNSLookup *lookup,
			       GMainContext *async_context,
			       GCancellable *cancellable,
			       SoupDNSCallback callback, gpointer user_data)
{
	SoupDNSCacheEntry *entry = lookup->entry;

	g_mutex_lock (soup_dns_lock);

	lookup->async_context = async_context;
	lookup->cancellable = cancellable;
	lookup->callback = callback;
	lookup->user_data = user_data;

	if (!entry->resolved) {
		entry->async_lookups = g_slist_prepend (entry->async_lookups,
							lookup);
		if (cancellable) {
			g_signal_connect (cancellable, "cancelled",
					  G_CALLBACK (async_cancel), lookup);
		}

		if (!entry->resolver_thread) {
			soup_dns_cache_entry_ref (entry);
			entry->resolver_thread =
				g_thread_create (resolver_thread, entry,
						 FALSE, NULL);
		}
	} else
		soup_add_idle (lookup->async_context, do_async_callback, lookup);

	g_mutex_unlock (soup_dns_lock);
}

/**
 * soup_dns_lookup_get_hostname:
 * @lookup: a #SoupDNSLookup
 *
 * Gets the hostname of @lookup.
 *
 * Return value: the hostname, which the caller owns and must free, or
 * %NULL if @lookup has not been completely resolved.
 **/
char *
soup_dns_lookup_get_hostname (SoupDNSLookup *lookup)
{
	return g_strdup (lookup->entry->hostname);
}

/**
 * soup_dns_lookup_get_address:
 * @lookup: a #SoupDNSLookup
 *
 * Gets the address of @lookup.
 *
 * Return value: the address, which the caller owns and must free, or
 * %NULL if @lookup has not been completely resolved.
 **/
struct sockaddr *
soup_dns_lookup_get_address (SoupDNSLookup *lookup)
{
	return g_memdup (lookup->entry->sockaddr,
			 SOUP_DNS_SOCKADDR_LEN (lookup->entry->sockaddr));
}

/**
 * soup_dns_lookup_free:
 * @lookup: a #SoupDNSLookup
 *
 * Frees @lookup. It is an error to cancel a lookup while it is
 * running.
 **/
void
soup_dns_lookup_free (SoupDNSLookup *lookup)
{
	soup_dns_cache_entry_unref (lookup->entry);
	g_slice_free (SoupDNSLookup, lookup);
}
