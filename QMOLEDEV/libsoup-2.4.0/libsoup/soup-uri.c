/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* soup-uri.c : utility functions to parse URLs */

/*
 * Copyright 1999-2003 Ximian, Inc.
 */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "soup-uri.h"
#include "soup-form.h"
#include "soup-misc.h"

/**
 * SECTION:soup-uri
 * @short_description: URIs
 *
 * A #SoupURI represents a (parsed) URI.
 *
 * Many applications will not need to use #SoupURI directly at all; on
 * the client side, soup_message_new() takes a stringified URI, and on
 * the server side, the path and query components are provided for you
 * in the server callback.
 **/

/**
 * SoupURI:
 * @scheme: the URI scheme (eg, "http")
 * @user: a username, or %NULL
 * @password: a password, or %NULL
 * @host: the hostname or IP address
 * @port: the port number on @host
 * @path: the path on @host
 * @query: a query for @path, or %NULL
 * @fragment: a fragment identifier within @path, or %NULL
 *
 * A #SoupURI represents a (parsed) URI. #SoupURI supports RFC 3986
 * (URI Generic Syntax), and can parse any valid URI. However, libsoup
 * only uses "http" and "https" URIs internally.
 *
 * @scheme will always be set in any URI. It is an interned string and
 * is always all lowercase. (If you parse a URI with a non-lowercase
 * scheme, it will be converted to lowercase.) The macros
 * %SOUP_URI_SCHEME_HTTP and %SOUP_URI_SCHEME_HTTPS provide the
 * interned values for "http" and "https" and can be compared against
 * URI @scheme values.
 *
 * @user and @password are parsed as defined in the older URI specs
 * (ie, separated by a colon; RFC 3986 only talks about a single
 * "userinfo" field). Note that @password is not included in the
 * output of soup_uri_to_string(). libsoup does not normally use these
 * fields; authentication is handled via #SoupSession signals.
 *
 * @host contains the hostname, and @port the port specified in the
 * URI. If the URI doesn't contain a hostname, @host will be %NULL,
 * and if it doesn't specify a port, @port may be 0. However, for
 * "http" and "https" URIs, @host is guaranteed to be non-%NULL
 * (trying to parse an http URI with no @host will return %NULL), and
 * @port will always be non-0 (because libsoup knows the default value
 * to use when it is not specified in the URI).
 *
 * @path is always non-%NULL. For http/https URIs, @path will never be
 * an empty string either; if the input URI has no path, the parsed
 * #SoupURI will have a @path of "/".
 *
 * @query and @fragment are optional for all URI types.
 * soup_form_decode_urlencoded() may be useful for parsing @query.
 *
 * Note that @path, @query, and @fragment may contain
 * %<!-- -->-encoded characters. soup_uri_new() calls
 * soup_uri_normalize() on them, but not soup_uri_decode(). This is
 * necessary to ensure that soup_uri_to_string() will generate a URI
 * that has exactly the same meaning as the original. (In theory,
 * #SoupURI should leave @user, @password, and @host partially-encoded
 * as well, but this would be more annoying than useful.)
 **/

static void append_uri_encoded (GString *str, const char *in, const char *extra_enc_chars);
static char *uri_decoded_copy (const char *str, int length);
static char *uri_normalized_copy (const char *str, int length, const char *unescape_extra);

const char *_SOUP_URI_SCHEME_HTTP, *_SOUP_URI_SCHEME_HTTPS;

static inline const char *
soup_uri_get_scheme (const char *scheme, int len)
{
	if (len == 4 && !strncmp (scheme, "http", 4)) {
		return SOUP_URI_SCHEME_HTTP;
	} else if (len == 5 && !strncmp (scheme, "https", 5)) {
		return SOUP_URI_SCHEME_HTTPS;
	} else {
		char *lower_scheme;

		lower_scheme = g_ascii_strdown (scheme, len);
		scheme = g_intern_string (lower_scheme);
		g_free (lower_scheme);
		return scheme;
	}
}

static inline guint
soup_scheme_default_port (const char *scheme)
{
	if (scheme == SOUP_URI_SCHEME_HTTP)
		return 80;
	else if (scheme == SOUP_URI_SCHEME_HTTPS)
		return 443;
	else
		return 0;
}

/**
 * soup_uri_new_with_base:
 * @base: a base URI
 * @uri_string: the URI
 *
 * Parses @uri_string relative to @base.
 *
 * Return value: a parsed #SoupURI.
 **/
SoupURI *
soup_uri_new_with_base (SoupURI *base, const char *uri_string)
{
	SoupURI *uri;
	const char *end, *hash, *colon, *at, *path, *question;
	const char *p, *hostend;
	gboolean remove_dot_segments = TRUE;

	uri = g_slice_new0 (SoupURI);

	/* See RFC 3986 for details. IF YOU CHANGE ANYTHING IN THIS
	 * FUNCTION, RUN tests/uri-parsing AFTERWARDS.
	 */

	/* Find fragment. */
	end = hash = strchr (uri_string, '#');
	if (hash && hash[1]) {
		uri->fragment = uri_normalized_copy (hash + 1, strlen (hash + 1),
						     NULL);
		if (!uri->fragment) {
			soup_uri_free (uri);
			return NULL;
		}
	} else
		end = uri_string + strlen (uri_string);

	/* Find scheme: initial [a-z+.-]* substring until ":" */
	p = uri_string;
	while (p < end && (g_ascii_isalnum (*p) ||
			   *p == '.' || *p == '+' || *p == '-'))
		p++;

	if (p > uri_string && *p == ':') {
		uri->scheme = soup_uri_get_scheme (uri_string, p - uri_string);
		if (!uri->scheme) {
			soup_uri_free (uri);
			return NULL;
		}
		uri_string = p + 1;
	}

	if (!*uri_string && !base)
		return uri;

	/* Check for authority */
	if (strncmp (uri_string, "//", 2) == 0) {
		uri_string += 2;

		path = uri_string + strcspn (uri_string, "/?#");
		at = strchr (uri_string, '@');
		if (at && at < path) {
			colon = strchr (uri_string, ':');
			if (colon && colon < at) {
				uri->password = uri_decoded_copy (colon + 1,
								  at - colon - 1);
				if (!uri->password) {
					soup_uri_free (uri);
					return NULL;
				}
			} else {
				uri->password = NULL;
				colon = at;
			}

			uri->user = uri_decoded_copy (uri_string,
						      colon - uri_string);
			if (!uri->user) {
				soup_uri_free (uri);
				return NULL;
			}
			uri_string = at + 1;
		} else
			uri->user = uri->password = NULL;

		/* Find host and port. */
		if (*uri_string == '[') {
			uri_string++;
			hostend = strchr (uri_string, ']');
			if (!hostend || hostend > path) {
				soup_uri_free (uri);
				return NULL;
			}
			if (*(hostend + 1) == ':')
				colon = hostend + 1;
			else
				colon = NULL;
		} else {
			colon = memchr (uri_string, ':', path - uri_string);
			hostend = colon ? colon : path;
		}

		uri->host = uri_decoded_copy (uri_string, hostend - uri_string);
		if (!uri->host) {
			soup_uri_free (uri);
			return NULL;
		}

		if (colon && colon != path - 1) {
			char *portend;
			uri->port = strtoul (colon + 1, &portend, 10);
			if (portend != (char *)path) {
				soup_uri_free (uri);
				return NULL;
			}
		}

		uri_string = path;
	}

	/* Find query */
	question = memchr (uri_string, '?', end - uri_string);
	if (question) {
		if (question[1]) {
			uri->query = uri_normalized_copy (question + 1,
							  end - (question + 1),
							  NULL);
			if (!uri->query) {
				soup_uri_free (uri);
				return NULL;
			}
		}
		end = question;
	}

	if (end != uri_string) {
		uri->path = uri_normalized_copy (uri_string, end - uri_string,
						 NULL);
		if (!uri->path) {
			soup_uri_free (uri);
			return NULL;
		}
	}

	/* Apply base URI. Again, this is spelled out in RFC 3986. */
	if (base && !uri->scheme && uri->host)
		uri->scheme = base->scheme;
	else if (base && !uri->scheme) {
		uri->scheme = base->scheme;
		uri->user = g_strdup (base->user);
		uri->password = g_strdup (base->password);
		uri->host = g_strdup (base->host);
		uri->port = base->port;

		if (!uri->path) {
			uri->path = g_strdup (base->path);
			if (!uri->query)
				uri->query = g_strdup (base->query);
			remove_dot_segments = FALSE;
		} else if (*uri->path != '/') {
			char *newpath, *last;

			last = strrchr (base->path, '/');
			if (last) {
				newpath = g_strdup_printf ("%.*s/%s",
							   (int)(last - base->path),
							   base->path,
							   uri->path);
			} else
				newpath = g_strdup_printf ("/%s", uri->path);

			g_free (uri->path);
			uri->path = newpath;
		}
	}

	if (remove_dot_segments && uri->path && *uri->path) {
		char *p = uri->path, *q;

		/* Remove "./" where "." is a complete segment. */
		for (p = uri->path + 1; *p; ) {
			if (*(p - 1) == '/' &&
			    *p == '.' && *(p + 1) == '/')
				memmove (p, p + 2, strlen (p + 2) + 1);
			else
				p++;
		}
		/* Remove "." at end. */
		if (p > uri->path + 2 &&
		    *(p - 1) == '.' && *(p - 2) == '/')
			*(p - 1) = '\0';

		/* Remove "<segment>/../" where <segment> != ".." */
		for (p = uri->path + 1; *p; ) {
			if (!strncmp (p, "../", 3)) {
				p += 3;
				continue;
			}
			q = strchr (p + 1, '/');
			if (!q)
				break;
			if (strncmp (q, "/../", 4) != 0) {
				p = q + 1;
				continue;
			}
			memmove (p, q + 4, strlen (q + 4) + 1);
			p = uri->path + 1;
		}
		/* Remove "<segment>/.." at end where <segment> != ".." */
		q = strrchr (uri->path, '/');
		if (q && !strcmp (q, "/..")) {
			p = q - 1;
			while (p > uri->path && *p != '/')
				p--;
			if (strncmp (p, "/../", 4) != 0)
				*(p + 1) = 0;
		}

		/* Remove extraneous initial "/.."s */
		while (!strncmp (uri->path, "/../", 4))
			memmove (uri->path, uri->path + 3, strlen (uri->path) - 2);
		if (!strcmp (uri->path, "/.."))
			uri->path[1] = '\0';
	}

	/* HTTP-specific stuff */
	if (uri->scheme == SOUP_URI_SCHEME_HTTP ||
	    uri->scheme == SOUP_URI_SCHEME_HTTPS) {
		if (!uri->host) {
			soup_uri_free (uri);
			return NULL;
		}
		if (!uri->path)
			uri->path = g_strdup ("/");
	}

	if (!uri->port)
		uri->port = soup_scheme_default_port (uri->scheme);
	if (!uri->path)
		uri->path = g_strdup ("");

	return uri;
}

/**
 * soup_uri_new:
 * @uri_string: a URI
 *
 * Parses an absolute URI.
 *
 * You can also pass %NULL for @uri_string if you want to get back an
 * "empty" #SoupURI that you can fill in by hand.
 *
 * Return value: a #SoupURI, or %NULL.
 **/
SoupURI *
soup_uri_new (const char *uri_string)
{
	SoupURI *uri;

	if (!uri_string)
		return g_slice_new0 (SoupURI);

	uri = soup_uri_new_with_base (NULL, uri_string);
	if (!uri)
		return NULL;
	if (!uri->scheme) {
		soup_uri_free (uri);
		return NULL;
	}

	return uri;
}


/**
 * soup_uri_to_string:
 * @uri: a #SoupURI
 * @just_path_and_query: if %TRUE, output just the path and query portions
 *
 * Returns a string representing @uri.
 *
 * If @just_path_and_query is %TRUE, this concatenates the path and query
 * together. That is, it constructs the string that would be needed in
 * the Request-Line of an HTTP request for @uri.
 *
 * Return value: a string representing @uri, which the caller must free.
 **/
char *
soup_uri_to_string (SoupURI *uri, gboolean just_path_and_query)
{
	GString *str;
	char *return_result;

	/* IF YOU CHANGE ANYTHING IN THIS FUNCTION, RUN
	 * tests/uri-parsing AFTERWARD.
	 */

	str = g_string_sized_new (20);

	if (uri->scheme && !just_path_and_query)
		g_string_sprintfa (str, "%s:", uri->scheme);
	if (uri->host && !just_path_and_query) {
		g_string_append (str, "//");
		if (uri->user) {
			append_uri_encoded (str, uri->user, ":;@?/");
			g_string_append_c (str, '@');
		}
		if (strchr (uri->host, ':')) {
			g_string_append_c (str, '[');
			g_string_append (str, uri->host);
			g_string_append_c (str, ']');
		} else
			append_uri_encoded (str, uri->host, ":/");
		if (uri->port && uri->port != soup_scheme_default_port (uri->scheme))
			g_string_append_printf (str, ":%d", uri->port);
		if (!uri->path && (uri->query || uri->fragment))
			g_string_append_c (str, '/');
	}

	if (uri->path && *uri->path)
		g_string_append (str, uri->path);

	if (uri->query) {
		g_string_append_c (str, '?');
		g_string_append (str, uri->query);
	}
	if (uri->fragment && !just_path_and_query) {
		g_string_append_c (str, '#');
		g_string_append (str, uri->fragment);
	}

	return_result = str->str;
	g_string_free (str, FALSE);

	return return_result;
}

/**
 * soup_uri_copy:
 * @uri: a #SoupURI
 *
 * Copies @uri
 *
 * Return value: a copy of @uri, which must be freed with soup_uri_free()
 **/
SoupURI *
soup_uri_copy (SoupURI *uri)
{
	SoupURI *dup;

	g_return_val_if_fail (uri != NULL, NULL);

	dup = g_slice_new0 (SoupURI);
	dup->scheme   = uri->scheme;
	dup->user     = g_strdup (uri->user);
	dup->password = g_strdup (uri->password);
	dup->host     = g_strdup (uri->host);
	dup->port     = uri->port;
	dup->path     = g_strdup (uri->path);
	dup->query    = g_strdup (uri->query);
	dup->fragment = g_strdup (uri->fragment);

	return dup;
}

/* Temporarily still used by SoupSession, but no longer public */
SoupURI *soup_uri_copy_root (SoupURI *uri);
gboolean soup_uri_host_equal (gconstpointer v1, gconstpointer v2);
guint    soup_uri_host_hash (gconstpointer key);

SoupURI *
soup_uri_copy_root (SoupURI *uri)
{
	SoupURI *dup;

	g_return_val_if_fail (uri != NULL, NULL);

	dup = g_slice_new0 (SoupURI);
	dup->scheme = uri->scheme;
	dup->host   = g_strdup (uri->host);
	dup->port   = uri->port;

	return dup;
}

guint
soup_uri_host_hash (gconstpointer key)
{
	const SoupURI *uri = key;

	return GPOINTER_TO_UINT (uri->scheme) + uri->port +
		soup_str_case_hash (uri->host);
}

gboolean
soup_uri_host_equal (gconstpointer v1, gconstpointer v2)
{
	const SoupURI *one = v1;
	const SoupURI *two = v2;

	if (one->scheme != two->scheme)
		return FALSE;
	if (one->port != two->port)
		return FALSE;

	return g_ascii_strcasecmp (one->host, two->host) == 0;
}

static inline gboolean
parts_equal (const char *one, const char *two, gboolean insensitive)
{
	if (!one && !two)
		return TRUE;
	if (!one || !two)
		return FALSE;
	return insensitive ? !g_ascii_strcasecmp (one, two) : !strcmp (one, two);
}

/**
 * soup_uri_equal:
 * @uri1: a #SoupURI
 * @uri2: another #SoupURI
 *
 * Tests whether or not @uri1 and @uri2 are equal in all parts
 *
 * Return value: %TRUE or %FALSE
 **/
gboolean 
soup_uri_equal (SoupURI *uri1, SoupURI *uri2)
{
	if (uri1->scheme != uri2->scheme                         ||
	    uri1->port   != uri2->port                           ||
	    !parts_equal (uri1->user, uri2->user, FALSE)         ||
	    !parts_equal (uri1->password, uri2->password, FALSE) ||
	    !parts_equal (uri1->host, uri2->host, TRUE)          ||
	    !parts_equal (uri1->path, uri2->path, FALSE)         ||
	    !parts_equal (uri1->query, uri2->query, FALSE)       ||
	    !parts_equal (uri1->fragment, uri2->fragment, FALSE))
		return FALSE;

	return TRUE;
}

/**
 * soup_uri_free:
 * @uri: a #SoupURI
 *
 * Frees @uri.
 **/
void
soup_uri_free (SoupURI *uri)
{
	g_return_if_fail (uri != NULL);

	g_free (uri->user);
	g_free (uri->password);
	g_free (uri->host);
	g_free (uri->path);
	g_free (uri->query);
	g_free (uri->fragment);

	g_slice_free (SoupURI, uri);
}

/* From RFC 3986 */
#define SOUP_URI_UNRESERVED  0
#define SOUP_URI_PCT_ENCODED 1
#define SOUP_URI_GEN_DELIMS  2
#define SOUP_URI_SUB_DELIMS  4
static const char uri_encoded_char[] = {
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* 0x00 - 0x0f */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* 0x10 - 0x1f */
	1, 4, 1, 2, 4, 1, 4, 4, 4, 4, 4, 4, 4, 0, 0, 2,  /*  !"#$%&'()*+,-./ */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 4, 1, 4, 1, 2,  /* 0123456789:;<=>? */
	2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* @ABCDEFGHIJKLMNO */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2, 1, 0,  /* PQRSTUVWXYZ[\]^_ */
	1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* `abcdefghijklmno */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1,  /* pqrstuvwxyz{|}~  */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
};

static void
append_uri_encoded (GString *str, const char *in, const char *extra_enc_chars)
{
	const unsigned char *s = (const unsigned char *)in;

	while (*s) {
		if ((uri_encoded_char[*s] & (SOUP_URI_PCT_ENCODED | SOUP_URI_GEN_DELIMS)) ||
		    (extra_enc_chars && strchr (extra_enc_chars, *s)))
			g_string_append_printf (str, "%%%02X", (int)*s++);
		else
			g_string_append_c (str, *s++);
	}
}

/**
 * soup_uri_encode:
 * @part: a URI part
 * @escape_extra: additional reserved characters to escape (or %NULL)
 *
 * This %<!-- -->-encodes the given URI part and returns the escaped
 * version in allocated memory, which the caller must free when it is
 * done.
 *
 * Return value: the encoded URI part
 **/
char *
soup_uri_encode (const char *part, const char *escape_extra)
{
	GString *str;
	char *encoded;

	str = g_string_new (NULL);
	append_uri_encoded (str, part, escape_extra);
	encoded = str->str;
	g_string_free (str, FALSE);

	return encoded;
}

#define XDIGIT(c) ((c) <= '9' ? (c) - '0' : ((c) & 0x4F) - 'A' + 10)
#define HEXCHAR(s) ((XDIGIT (s[1]) << 4) + XDIGIT (s[2]))

char *
uri_decoded_copy (const char *part, int length)
{
	unsigned char *s, *d;
	char *decoded = g_strndup (part, length);

	s = d = (unsigned char *)decoded;
	do {
		if (*s == '%') {
			if (!g_ascii_isxdigit (s[1]) ||
			    !g_ascii_isxdigit (s[2])) {
				g_free (decoded);
				return NULL;
			}
			*d++ = HEXCHAR (s);
			s += 2;
		} else
			*d++ = *s;
	} while (*s++);

	return decoded;
}

/**
 * soup_uri_decode:
 * @part: a URI part
 *
 * Fully %<!-- -->-decodes @part.
 *
 * Return value: the decoded URI part, or %NULL if an invalid percent
 * code was encountered.
 */
char *
soup_uri_decode (const char *part)
{
	return uri_decoded_copy (part, strlen (part));
}

char *
uri_normalized_copy (const char *part, int length, const char *unescape_extra)
{
	unsigned char *s, *d, c;
	char *normalized = g_strndup (part, length);

	s = d = (unsigned char *)normalized;
	do {
		if (*s == '%') {
			if (!g_ascii_isxdigit (s[1]) ||
			    !g_ascii_isxdigit (s[2])) {
				g_free (normalized);
				return NULL;
			}

			c = HEXCHAR (s);
			if (uri_encoded_char[c] == SOUP_URI_UNRESERVED ||
			    (unescape_extra && strchr (unescape_extra, c))) {
				*d++ = c;
				s += 2;
			} else {
				*d++ = *s++;
				*d++ = g_ascii_toupper (*s++);
				*d++ = g_ascii_toupper (*s);
			}
		} else
			*d++ = *s;
	} while (*s++);

	return normalized;
}

/**
 * soup_uri_normalize:
 * @part: a URI part
 * @unescape_extra: reserved characters to unescape (or %NULL)
 *
 * %<!-- -->-decodes any "unreserved" characters (or characters in
 * @unescape_extra) in @part.
 *
 * "Unreserved" characters are those that are not allowed to be used
 * for punctuation according to the URI spec. For example, letters are
 * unreserved, so soup_uri_normalize() will turn
 * <literal>http://example.com/foo/b%<!-- -->61r</literal> into
 * <literal>http://example.com/foo/bar</literal>, which is guaranteed
 * to mean the same thing. However, "/" is "reserved", so
 * <literal>http://example.com/foo%<!-- -->2Fbar</literal> would not
 * be changed, because it might mean something different to the
 * server.
 *
 * Return value: the normalized URI part, or %NULL if an invalid percent
 * code was encountered.
 */
char *
soup_uri_normalize (const char *part, const char *unescape_extra)
{
	return uri_normalized_copy (part, strlen (part), unescape_extra);
}


/**
 * soup_uri_uses_default_port:
 * @uri: a #SoupURI
 *
 * Tests if @uri uses the default port for its scheme. (Eg, 80 for
 * http.) (This only works for http and https; libsoup does not know
 * the default ports of other protocols.)
 *
 * Return value: %TRUE or %FALSE
 **/
gboolean
soup_uri_uses_default_port (SoupURI *uri)
{
	g_return_val_if_fail (uri->scheme == SOUP_URI_SCHEME_HTTP ||
			      uri->scheme == SOUP_URI_SCHEME_HTTPS, FALSE);

	return uri->port == soup_scheme_default_port (uri->scheme);
}

/**
 * SOUP_URI_SCHEME_HTTP:
 *
 * "http" as an interned string. This can be compared directly against
 * the value of a #SoupURI's <structfield>scheme</structfield>
 **/

/**
 * SOUP_URI_SCHEME_HTTPS:
 *
 * "https" as an interned string. This can be compared directly
 * against the value of a #SoupURI's <structfield>scheme</structfield>
 **/

/**
 * soup_uri_set_scheme:
 * @uri: a #SoupURI
 * @scheme: the URI scheme
 *
 * Sets @uri's scheme to @scheme. This will also set @uri's port to
 * the default port for @scheme, if known.
 **/
void
soup_uri_set_scheme (SoupURI *uri, const char *scheme)
{
	uri->scheme = soup_uri_get_scheme (scheme, strlen (scheme));
	uri->port = soup_scheme_default_port (uri->scheme);
}

/**
 * soup_uri_set_user:
 * @uri: a #SoupURI
 * @user: the username, or %NULL
 *
 * Sets @uri's user to @user.
 **/
void
soup_uri_set_user (SoupURI *uri, const char *user)
{
	g_free (uri->user);
	uri->user = g_strdup (user);
}

/**
 * soup_uri_set_password:
 * @uri: a #SoupURI
 * @password: the password, or %NULL
 *
 * Sets @uri's password to @password.
 **/
void
soup_uri_set_password (SoupURI *uri, const char *password)
{
	g_free (uri->password);
	uri->password = g_strdup (password);
}

/**
 * soup_uri_set_host:
 * @uri: a #SoupURI
 * @host: the hostname or IP address, or %NULL
 *
 * Sets @uri's host to @host.
 *
 * If @host is an IPv6 IP address, it should not include the brackets
 * required by the URI syntax; they will be added automatically when
 * converting @uri to a string.
 **/
void
soup_uri_set_host (SoupURI *uri, const char *host)
{
	g_free (uri->host);
	uri->host = g_strdup (host);
}

/**
 * soup_uri_set_port:
 * @uri: a #SoupURI
 * @port: the port, or 0
 *
 * Sets @uri's port to @port. If @port is 0, @uri will not have an
 * explicitly-specified port.
 **/
void
soup_uri_set_port (SoupURI *uri, guint port)
{
	uri->port = port;
}

/**
 * soup_uri_set_path:
 * @uri: a #SoupURI
 * @path: the path
 *
 * Sets @uri's path to @path.
 **/
void
soup_uri_set_path (SoupURI *uri, const char *path)
{
	g_free (uri->path);
	uri->path = g_strdup (path);
}

/**
 * soup_uri_set_query:
 * @uri: a #SoupURI
 * @query: the query
 *
 * Sets @uri's query to @query.
 **/
void
soup_uri_set_query (SoupURI *uri, const char *query)
{
	g_free (uri->query);
	uri->query = g_strdup (query);
}

/**
 * soup_uri_set_query_from_form:
 * @uri: a #SoupURI
 * @form: a #GHashTable containing HTML form information
 *
 * Sets @uri's query to the result of encoding @form according to the
 * HTML form rules. See soup_form_encode_hash() for more information.
 **/
void
soup_uri_set_query_from_form (SoupURI *uri, GHashTable *form)
{
	g_free (uri->query);
	uri->query = soup_form_encode_urlencoded (form);
}

/**
 * soup_uri_set_query_from_fields:
 * @uri: a #SoupURI
 * @first_field: name of the first form field to encode into query
 * @...: value of @first_field, followed by additional field names
 * and values, terminated by %NULL.
 *
 * Sets @uri's query to the result of encoding the given form fields
 * and values according to the * HTML form rules. See
 * soup_form_encode() for more information.
 **/
void
soup_uri_set_query_from_fields (SoupURI    *uri,
				const char *first_field,
				...)
{
	va_list args;

	g_free (uri->query);
	va_start (args, first_field);
	uri->query = soup_form_encode_valist (first_field, args);
	va_end (args);
}

/**
 * soup_uri_set_fragment:
 * @uri: a #SoupURI
 * @fragment: the fragment
 *
 * Sets @uri's fragment to @fragment.
 **/
void
soup_uri_set_fragment (SoupURI *uri, const char *fragment)
{
	g_free (uri->fragment);
	uri->fragment = g_strdup (fragment);
}


GType
soup_uri_get_type (void)
{
	static volatile gsize type_volatile = 0;

	if (g_once_init_enter (&type_volatile)) {
		GType type = g_boxed_type_register_static (
			g_intern_static_string ("SoupURI"),
			(GBoxedCopyFunc) soup_uri_copy,
			(GBoxedFreeFunc) soup_uri_free);
		g_once_init_leave (&type_volatile, type);
	}
	return type_volatile;
}
