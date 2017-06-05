/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2000-2003, Ximian, Inc.
 */

#ifndef SOUP_DNS_H
#define SOUP_DNS_H

#include <glib.h>
#include <gio/gio.h>
#include <sys/types.h>

#include <libsoup/soup-portability.h>

void             soup_dns_init                 (void);
char            *soup_dns_ntop                 (struct sockaddr *sa);

typedef struct SoupDNSLookup SoupDNSLookup;

SoupDNSLookup   *soup_dns_lookup_name          (const char  *name);
SoupDNSLookup   *soup_dns_lookup_address       (struct sockaddr *sockaddr);
void             soup_dns_lookup_free          (SoupDNSLookup   *lookup);

typedef void (*SoupDNSCallback) (SoupDNSLookup *lookup, guint status, gpointer user_data);

guint            soup_dns_lookup_resolve       (SoupDNSLookup   *lookup,
						GCancellable    *cancellable);
void             soup_dns_lookup_resolve_async (SoupDNSLookup   *lookup,
						GMainContext    *async_context,
						GCancellable    *cancellable,
						SoupDNSCallback  callback,
						gpointer         user_data);

char            *soup_dns_lookup_get_hostname  (SoupDNSLookup   *lookup);
struct sockaddr *soup_dns_lookup_get_address   (SoupDNSLookup   *lookup);


#endif /* SOUP_DNS_H */
