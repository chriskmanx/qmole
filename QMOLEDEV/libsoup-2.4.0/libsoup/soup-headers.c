/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-headers.c: HTTP message header parsing
 *
 * Copyright (C) 2001-2003, Ximian, Inc.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "soup-headers.h"
#include "soup-misc.h"

static gboolean
soup_headers_parse (const char *str, int len, SoupMessageHeaders *dest)
{
	const char *headers_start;
	char *headers_copy, *name, *name_end, *value, *value_end;
	char *eol, *sol;
	gboolean success = FALSE;

	/* Technically, the grammar does allow NUL bytes in the
	 * headers, but this is probably a bug, and if it's not, we
	 * can't deal with them anyway.
	 */
	if (memchr (str, '\0', len))
		return FALSE;

	/* As per RFC 2616 section 19.3, we treat '\n' as the
	 * line terminator, and '\r', if it appears, merely as
	 * ignorable trailing whitespace.
	 */

	/* Skip over the Request-Line / Status-Line */
	headers_start = memchr (str, '\n', len);
	if (!headers_start)
		return FALSE;

	/* We work on a copy of the headers, which we can write '\0's
	 * into, so that we don't have to individually g_strndup and
	 * then g_free each header name and value.
	 */
	headers_copy = g_strndup (headers_start, len - (headers_start - str));
	value_end = headers_copy;

	while (*(value_end + 1)) {
		name = value_end + 1;
		name_end = strchr (name, ':');
		if (!name_end)
			goto done;

		/* Find the end of the value; ie, an end-of-line that
		 * isn't followed by a continuation line.
		 */
		value = name_end + 1;
		value_end = strchr (name, '\n');
		if (!value_end || value_end < name_end)
			goto done;
		while (*(value_end + 1) == ' ' || *(value_end + 1) == '\t') {
			value_end = strchr (value_end + 1, '\n');
			if (!value_end)
				goto done;
		}

		*name_end = '\0';
		*value_end = '\0';

		/* Skip leading whitespace */
		while (value < value_end &&
		       (*value == ' ' || *value == '\t' ||
			*value == '\r' || *value == '\n'))
			value++;

		/* Collapse continuation lines */
		while ((eol = strchr (value, '\n'))) {
			/* find start of next line */
			sol = eol + 1;
			while (*sol == ' ' || *sol == '\t')
				sol++;

			/* back up over trailing whitespace on current line */
			while (eol[-1] == ' ' || eol[-1] == '\t' || eol[-1] == '\r')
				eol--;

			/* Delete all but one SP */
			*eol = ' ';
			g_memmove (eol + 1, sol, strlen (sol) + 1);
		}

		/* clip trailing whitespace */
		eol = strchr (value, '\0');
		while (eol > value &&
		       (eol[-1] == ' ' || eol[-1] == '\t' || eol[-1] == '\r'))
			eol--;
		*eol = '\0';

		soup_message_headers_append (dest, name, value);
        }
	success = TRUE;

done:
	g_free (headers_copy);
	return success;
}

/* RFC 2616 14.10 */
static void
soup_headers_clean_for_10 (SoupMessageHeaders *hdrs)
{
	const char *connection;
	GSList *tokens, *t;

	connection = soup_message_headers_get (hdrs, "Connection");
	if (!connection)
		return;

	tokens = soup_header_parse_list (connection);
	for (t = tokens; t; t = t->next)
		soup_message_headers_remove (hdrs, t->data);
	soup_header_free_list (tokens);
}

/**
 * soup_headers_parse_request:
 * @str: the header string (including the trailing blank line)
 * @len: length of @str up to (but not including) the terminating blank line.
 * @req_headers: #SoupMessageHeaders to store the header values in
 * @req_method: if non-%NULL, will be filled in with the request method
 * @req_path: if non-%NULL, will be filled in with the request path
 * @ver: if non-%NULL, will be filled in with the HTTP version
 *
 * Parses the headers of an HTTP request in @str and stores the
 * results in @req_method, @req_path, @ver, and @req_headers.
 *
 * Beware that @req_headers may be modified even on failure.
 *
 * Return value: %SOUP_STATUS_OK if the headers could be parsed, or an
 * HTTP error to be returned to the client if they could not be.
 **/
guint
soup_headers_parse_request (const char          *str, 
			    int                  len, 
			    SoupMessageHeaders  *req_headers,
			    char               **req_method,
			    char               **req_path,
			    SoupHTTPVersion     *ver) 
{
	const char *method, *method_end, *path, *path_end;
	const char *version, *version_end, *headers;
	unsigned long major_version, minor_version;
	char *p;

	g_return_val_if_fail (str && *str, SOUP_STATUS_MALFORMED);

	/* RFC 2616 4.1 "servers SHOULD ignore any empty line(s)
	 * received where a Request-Line is expected."
	 */
	while (*str == '\r' || *str == '\n') {
		str++;
		len--;
	}

	/* RFC 2616 19.3 "[servers] SHOULD accept any amount of SP or
	 * HT characters between [Request-Line] fields"
	 */

	method = method_end = str;
	while (method_end < str + len && *method_end != ' ' && *method_end != '\t')
		method_end++;
	if (method_end >= str + len)
		return SOUP_STATUS_BAD_REQUEST;

	path = method_end;
	while (path < str + len && (*path == ' ' || *path == '\t'))
		path++;
	if (path >= str + len)
		return SOUP_STATUS_BAD_REQUEST;

	path_end = path;
	while (path_end < str + len && *path_end != ' ' && *path_end != '\t')
		path_end++;
	if (path_end >= str + len)
		return SOUP_STATUS_BAD_REQUEST;

	version = path_end;
	while (version < str + len && (*version == ' ' || *version == '\t'))
		version++;
	if (version + 8 >= str + len)
		return SOUP_STATUS_BAD_REQUEST;

	if (strncmp (version, "HTTP/", 5) != 0 ||
	    !g_ascii_isdigit (version[5]))
		return SOUP_STATUS_BAD_REQUEST;
	major_version = strtoul (version + 5, &p, 10);
	if (*p != '.' || !g_ascii_isdigit (p[1]))
		return SOUP_STATUS_BAD_REQUEST;
	minor_version = strtoul (p + 1, &p, 10);
	version_end = p;
	if (major_version != 1)
		return SOUP_STATUS_HTTP_VERSION_NOT_SUPPORTED;
	if (minor_version < 0 || minor_version > 1)
		return SOUP_STATUS_HTTP_VERSION_NOT_SUPPORTED;

	headers = version_end;
	while (headers < str + len && (*headers == '\r' || *headers == ' '))
		headers++;
	if (headers >= str + len || *headers != '\n')
		return SOUP_STATUS_BAD_REQUEST;

	if (!soup_headers_parse (str, len, req_headers)) 
		return SOUP_STATUS_BAD_REQUEST;

	if (soup_message_headers_get_expectations (req_headers) &
	    SOUP_EXPECTATION_UNRECOGNIZED)
		return SOUP_STATUS_EXPECTATION_FAILED;
	if (minor_version == 0)
		soup_headers_clean_for_10 (req_headers);

	if (req_method)
		*req_method = g_strndup (method, method_end - method);
	if (req_path)
		*req_path = g_strndup (path, path_end - path);
	if (ver)
		*ver = (minor_version == 0) ? SOUP_HTTP_1_0 : SOUP_HTTP_1_1;

	return SOUP_STATUS_OK;
}

/**
 * soup_headers_parse_status_line:
 * @status_line: an HTTP Status-Line
 * @ver: if non-%NULL, will be filled in with the HTTP version
 * @status_code: if non-%NULL, will be filled in with the status code
 * @reason_phrase: if non-%NULL, will be filled in with the reason
 * phrase
 *
 * Parses the HTTP Status-Line string in @status_line into @ver,
 * @status_code, and @reason_phrase. @status_line must be terminated by
 * either "\0" or "\r\n".
 *
 * Return value: %TRUE if @status_line was parsed successfully.
 **/
gboolean
soup_headers_parse_status_line (const char       *status_line,
				SoupHTTPVersion  *ver,
				guint            *status_code,
				char            **reason_phrase)
{
	unsigned long major_version, minor_version, code;
	const char *code_start, *code_end, *phrase_start, *phrase_end;
	char *p;

	if (strncmp (status_line, "HTTP/", 5) == 0 &&
	    g_ascii_isdigit (status_line[5])) {
		major_version = strtoul (status_line + 5, &p, 10);
		if (*p != '.' || !g_ascii_isdigit (p[1]))
			return FALSE;
		minor_version = strtoul (p + 1, &p, 10);
		if (major_version != 1)
			return FALSE;
		if (minor_version < 0 || minor_version > 1)
			return FALSE;
		if (ver)
			*ver = (minor_version == 0) ? SOUP_HTTP_1_0 : SOUP_HTTP_1_1;
	} else if (!strncmp (status_line, "ICY", 3)) {
		/* Shoutcast not-quite-HTTP format */
		*ver = SOUP_HTTP_1_0;
		p = (char *)status_line + 3;
	} else
		return FALSE;

	code_start = p;
	while (*code_start == ' ' || *code_start == '\t')
		code_start++;
	code_end = code_start;
	while (*code_end >= '0' && *code_end <= '9')
		code_end++;
	if (code_end != code_start + 3)
		return FALSE;
	code = atoi (code_start);
	if (code < 100 || code > 599)
		return FALSE;
	if (status_code)
		*status_code = code;

	phrase_start = code_end;
	while (*phrase_start == ' ' || *phrase_start == '\t')
		phrase_start++;
	phrase_end = phrase_start + strcspn (phrase_start, "\n");
	while (phrase_end > phrase_start &&
	       (phrase_end[-1] == '\r' || phrase_end[-1] == ' ' || phrase_end[-1] == '\t'))
		phrase_end--;
	if (reason_phrase)
		*reason_phrase = g_strndup (phrase_start, phrase_end - phrase_start);

	return TRUE;
}

/**
 * soup_headers_parse_response:
 * @str: the header string (including the trailing blank line)
 * @len: length of @str up to (but not including) the terminating blank line.
 * @headers: #SoupMessageheaders to store the header values in
 * @ver: if non-%NULL, will be filled in with the HTTP version
 * @status_code: if non-%NULL, will be filled in with the status code
 * @reason_phrase: if non-%NULL, will be filled in with the reason
 * phrase
 *
 * Parses the headers of an HTTP response in @str and stores the
 * results in @ver, @status_code, @reason_phrase, and @headers.
 *
 * Beware that @headers may be modified even on failure.
 *
 * Return value: success or failure.
 **/
gboolean
soup_headers_parse_response (const char          *str, 
			     int                  len, 
			     SoupMessageHeaders  *headers,
			     SoupHTTPVersion     *ver,
			     guint               *status_code,
			     char               **reason_phrase)
{
	SoupHTTPVersion version;

	if (!str || !*str)
		return FALSE;

	if (!soup_headers_parse (str, len, headers)) 
		return FALSE;

	if (!soup_headers_parse_status_line (str, 
					     &version, 
					     status_code, 
					     reason_phrase))
		return FALSE;
	if (ver)
		*ver = version;

	if (version == SOUP_HTTP_1_0)
		soup_headers_clean_for_10 (headers);

	return TRUE;
}


/*
 * Parsing of specific HTTP header types
 */

static const char *
skip_lws (const char *s)
{
	while (g_ascii_isspace (*s))
		s++;
	return s;
}

static const char *
unskip_lws (const char *s, const char *start)
{
	while (s > start && g_ascii_isspace (*(s - 1)))
		s--;
	return s;
}

static const char *
skip_commas (const char *s)
{
	/* The grammar allows for multiple commas */
	while (g_ascii_isspace (*s) || *s == ',')
		s++;
	return s;
}

static const char *
skip_item (const char *s)
{
	gboolean quoted = FALSE;
	const char *start = s;

	/* A list item ends at the last non-whitespace character
	 * before a comma which is not inside a quoted-string. Or at
	 * the end of the string.
	 */

	while (*s) {
		if (*s == '"')
			quoted = !quoted;
		else if (quoted) {
			if (*s == '\\' && *(s + 1))
				s++;
		} else {
			if (*s == ',')
				break;
		}
		s++;
	}

	return unskip_lws (s, start);
}

/**
 * soup_header_parse_list:
 * @header: a header value
 *
 * Parses a header whose content is described by RFC2616 as
 * "#something", where "something" does not itself contain commas,
 * except as part of quoted-strings.
 *
 * Return value: a #GSList of list elements, as allocated strings
 **/
GSList *
soup_header_parse_list (const char *header)
{
	GSList *list = NULL;
	const char *end;

	header = skip_commas (header);
	while (*header) {
		end = skip_item (header);
		list = g_slist_prepend (list, g_strndup (header, end - header));
		header = skip_commas (end);
	}

	return g_slist_reverse (list);
}

typedef struct {
	char *item;
	double qval;
} QualityItem;

static int
sort_by_qval (const void *a, const void *b)
{
	QualityItem *qia = (QualityItem *)a;
	QualityItem *qib = (QualityItem *)b;

	if (qia->qval == qib->qval)
		return 0;
	else if (qia->qval < qib->qval)
		return 1;
	else
		return -1;
}

/**
 * soup_header_parse_quality_list:
 * @header: a header value
 * @unacceptable: on return, will contain a list of unacceptable
 * values
 *
 * Parses a header whose content is a list of items with optional
 * "qvalue"s (eg, Accept, Accept-Charset, Accept-Encoding,
 * Accept-Language, TE).
 *
 * If @unacceptable is not %NULL, then on return, it will contain the
 * items with qvalue 0. Either way, those items will be removed from
 * the main list.
 *
 * Return value: a #GSList of acceptable values (as allocated
 * strings), highest-qvalue first.
 **/
GSList *
soup_header_parse_quality_list (const char *header, GSList **unacceptable)
{
	GSList *unsorted;
	QualityItem *array;
	GSList *sorted, *iter;
	char *item, *semi;
	const char *param, *equal, *value;
	double qval;
	int n;

	if (unacceptable)
		*unacceptable = NULL;

	unsorted = soup_header_parse_list (header);
	array = g_new0 (QualityItem, g_slist_length (unsorted));
	for (iter = unsorted, n = 0; iter; iter = iter->next) {
		item = iter->data;
		qval = 1.0;
		for (semi = strchr (item, ';'); semi; semi = strchr (semi + 1, ';')) {
			param = skip_lws (semi + 1);
			if (*param != 'q')
				continue;
			equal = skip_lws (param + 1);
			if (!equal || *equal != '=')
				continue;
			value = skip_lws (equal + 1);
			if (!value)
				continue;

			if (value[0] != '0' && value[0] != '1')
				continue;
			qval = (double)(value[0] - '0');
			if (value[0] == '0' && value[1] == '.') {
				if (g_ascii_isdigit (value[2])) {
					qval += (double)(value[2] - '0') / 10;
					if (g_ascii_isdigit (value[3])) {
						qval += (double)(value[3] - '0') / 100;
						if (g_ascii_isdigit (value[4]))
							qval += (double)(value[4] - '0') / 1000;
					}
				}
			}

			*semi = '\0';
			break;
		}

		if (qval == 0.0) {
			if (unacceptable) {
				*unacceptable = g_slist_prepend (*unacceptable,
								 item);
			}
		} else {
			array[n].item = item;
			array[n].qval = qval;
			n++;
		}
	}
	g_slist_free (unsorted);

	qsort (array, n, sizeof (QualityItem), sort_by_qval);
	sorted = NULL;
	while (n--)
		sorted = g_slist_prepend (sorted, array[n].item);
	g_free (array);

	return sorted;
}

/**
 * soup_header_free_list:
 * @list: a #GSList returned from soup_header_parse_list() or
 * soup_header_parse_quality_list()
 *
 * Frees @list.
 **/
void
soup_header_free_list (GSList *list)
{
	GSList *l;

	for (l = list; l; l = l->next)
		g_free (l->data);
	g_slist_free (list);
}

/**
 * soup_header_contains:
 * @header: An HTTP header suitable for parsing with
 * soup_header_parse_list()
 * @token: a token
 *
 * Parses @header to see if it contains the token @token (matched
 * case-insensitively). Note that this can't be used with lists
 * that have qvalues.
 *
 * Return value: whether or not @header contains @token
 **/
gboolean
soup_header_contains (const char *header, const char *token)
{
	const char *end;
	guint len = strlen (token);

	header = skip_commas (header);
	while (*header) {
		end = skip_item (header);
		if (end - header == len &&
		    !g_ascii_strncasecmp (header, token, len))
			return TRUE;
		header = skip_commas (end);
	}

	return FALSE;
}

static void
decode_quoted_string (char *quoted_string)
{
	char *src, *dst;

	src = quoted_string + 1;
	dst = quoted_string;
	while (*src && *src != '"') {
		if (*src == '\\' && *(src + 1))
			src++;
		*dst++ = *src++;
	}
	*dst = '\0';
}

/**
 * soup_header_parse_param_list:
 * @header: a header value
 *
 * Parses a header which is a list of something like
 *   token [ "=" ( token | quoted-string ) ]
 *
 * Tokens that don't have an associated value will still be added to
 * the resulting hash table, but with a %NULL value.
 * 
 * Return value: a #GHashTable of list elements.
 **/
GHashTable *
soup_header_parse_param_list (const char *header)
{
	GHashTable *params;
	GSList *list, *iter;
	char *item, *eq, *name_end, *value;

	list = soup_header_parse_list (header);
	if (!list)
		return NULL;

	params = g_hash_table_new_full (soup_str_case_hash, 
					soup_str_case_equal,
					g_free, NULL);

	for (iter = list; iter; iter = iter->next) {
		item = iter->data;

		eq = strchr (item, '=');
		if (eq) {
			name_end = (char *)unskip_lws (eq, item);
			if (name_end == item) {
				/* That's no good... */
				g_free (item);
				continue;
			}

			*name_end = '\0';

			value = (char *)skip_lws (eq + 1);
			if (*value == '"')
				decode_quoted_string (value);
		} else
			value = NULL;

		g_hash_table_insert (params, item, value);
	}

	g_slist_free (list);
	return params;
}

/**
 * soup_header_free_param_list:
 * @param_list: a #GHashTable returned from soup_header_parse_param_list()
 *
 * Frees @param_list.
 **/
void
soup_header_free_param_list (GHashTable *param_list)
{
	g_hash_table_destroy (param_list);
}
