/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

/* 
 * Copyright 1999-2002 Ximian, Inc.
 */


#ifndef  SOUP_URI_H
#define  SOUP_URI_H 1

#include <libsoup/soup-types.h>

G_BEGIN_DECLS

struct _SoupURI {
	const char *scheme;

	char       *user;
	char       *password;

	char       *host;
	guint       port;

	char       *path;
	char       *query;

	char       *fragment;
};

GType     soup_uri_get_type          (void);
#define SOUP_TYPE_URI (soup_uri_get_type ())

#define SOUP_URI_SCHEME_HTTP  (_SOUP_URI_SCHEME_HTTP ? _SOUP_URI_SCHEME_HTTP : (_SOUP_URI_SCHEME_HTTP = g_intern_static_string ("http")))
#define SOUP_URI_SCHEME_HTTPS (_SOUP_URI_SCHEME_HTTPS ? _SOUP_URI_SCHEME_HTTPS : (_SOUP_URI_SCHEME_HTTPS = g_intern_static_string ("https")))
extern const char *_SOUP_URI_SCHEME_HTTP, *_SOUP_URI_SCHEME_HTTPS;

SoupURI  *soup_uri_new_with_base         (SoupURI    *base,
					  const char *uri_string);
SoupURI  *soup_uri_new                   (const char *uri_string);

char     *soup_uri_to_string             (SoupURI    *uri, 
					  gboolean    just_path_and_query);

SoupURI  *soup_uri_copy                  (SoupURI    *uri);

gboolean  soup_uri_equal                 (SoupURI    *uri1, 
					  SoupURI    *uri2);

void      soup_uri_free                  (SoupURI    *uri);

char     *soup_uri_encode                (const char *part,
					  const char *escape_extra);
char     *soup_uri_decode                (const char *part);
char     *soup_uri_normalize             (const char *part,
					  const char *unescape_extra);

gboolean  soup_uri_uses_default_port     (SoupURI    *uri);

void      soup_uri_set_scheme            (SoupURI    *uri,
					  const char *scheme);
void      soup_uri_set_user              (SoupURI    *uri,
					  const char *user);
void      soup_uri_set_password          (SoupURI    *uri,
					  const char *password);
void      soup_uri_set_host              (SoupURI    *uri,
					  const char *host);
void      soup_uri_set_port              (SoupURI    *uri,
					  guint       port);
void      soup_uri_set_path              (SoupURI    *uri,
					  const char *path);
void      soup_uri_set_query             (SoupURI    *uri,
					  const char *query);
void      soup_uri_set_query_from_form   (SoupURI    *uri,
					  GHashTable *form);
void      soup_uri_set_query_from_fields (SoupURI    *uri,
					  const char *first_field,
					  ...);
void      soup_uri_set_fragment          (SoupURI    *uri,
					  const char *fragment);

G_END_DECLS

#endif /*SOUP_URI_H*/
