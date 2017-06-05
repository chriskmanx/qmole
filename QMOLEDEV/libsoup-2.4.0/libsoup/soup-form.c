/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* soup-form.c : utility functions for HTML forms */

/*
 * Copyright 2008 Red Hat, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>

#include "soup-form.h"
#include "soup-message.h"
#include "soup-uri.h"

#define XDIGIT(c) ((c) <= '9' ? (c) - '0' : ((c) & 0x4F) - 'A' + 10)
#define HEXCHAR(s) ((XDIGIT (s[1]) << 4) + XDIGIT (s[2]))

static gboolean
form_decode (char *part)
{
	unsigned char *s, *d;

	s = d = (unsigned char *)part;
	do {
		if (*s == '%') {
			if (!g_ascii_isxdigit (s[1]) ||
			    !g_ascii_isxdigit (s[2]))
				return FALSE;
			*d++ = HEXCHAR (s);
			s += 2;
		} else if (*s == '+')
			*d++ = ' ';
		else
			*d++ = *s;
	} while (*s++);

	return TRUE;
}

/**
 * soup_form_decode:
 * @encoded_form: data of type "application/x-www-form-urlencoded"
 *
 * Decodes @form, which is an urlencoded dataset as defined in the
 * HTML 4.01 spec.
 *
 * Return value: a hash table containing the name/value pairs from
 * @encoded_form, which you can free with g_hash_table_destroy().
 **/
GHashTable *
soup_form_decode (const char *encoded_form)
{
	GHashTable *form_data_set;
	char **pairs, *eq, *name, *value;
	int i;

	form_data_set = g_hash_table_new_full (g_str_hash, g_str_equal,
					       g_free, NULL);
	pairs = g_strsplit (encoded_form, "&", -1);
	for (i = 0; pairs[i]; i++) {
		name = pairs[i];
		eq = strchr (name, '=');
		if (!form_decode (name)) {
			g_free (name);
			continue;
		}
		if (eq) {
			*eq = '\0';
			value = eq + 1;
		} else
			value = NULL;

		g_hash_table_insert (form_data_set, name, value);
	}
	g_free (pairs);

	return form_data_set;
}

static void
append_form_encoded (GString *str, const char *in)
{
	const unsigned char *s = (const unsigned char *)in;

	while (*s) {
		if (*s == ' ') {
			g_string_append_c (str, '+');
			s++;
		} else if (!g_ascii_isalnum (*s))
			g_string_append_printf (str, "%%%02X", (int)*s++);
		else
			g_string_append_c (str, *s++);
	}
}

static void
encode_pair (GString *str, const char *name, const char *value)
{
	if (str->len)
		g_string_append_c (str, '&');
	append_form_encoded (str, name);
	g_string_append_c (str, '=');
	append_form_encoded (str, value);
}

static void
hash_encode_foreach (gpointer name, gpointer value, gpointer str)
{
	encode_pair (str, name, value);
}

/**
 * soup_form_encode:
 * @first_field: name of the first form field
 * @...: value of @first_field, followed by additional field names
 * and values, terminated by %NULL.
 *
 * Encodes the given field names and values into a value of type
 * "application/x-www-form-urlencoded", as defined in the HTML 4.01
 * spec.
 *
 * This method requires you to know the names of the form fields (or
 * at the very least, the total number of fields) at compile time; for
 * working with dynamic forms, use soup_form_encode_hash() or
 * soup_form_encode_datalist().
 *
 * Return value: the encoded form
 **/
char *
soup_form_encode (const char *first_field, ...)
{
	va_list args;
	char *encoded;

	va_start (args, first_field);
	encoded = soup_form_encode_valist (first_field, args);
	va_end (args);

	return encoded;
}

/**
 * soup_form_encode_hash:
 * @form_data_set: a hash table containing name/value pairs (as strings)
 *
 * Encodes @form_data_set into a value of type
 * "application/x-www-form-urlencoded", as defined in the HTML 4.01
 * spec.
 *
 * Note that the HTML spec states that "The control names/values are
 * listed in the order they appear in the document." Since this method
 * takes a hash table, it cannot enforce that; if you care about the
 * ordering of the form fields, use soup_form_encode_datalist().
 *
 * Return value: the encoded form
 **/
char *
soup_form_encode_hash (GHashTable *form_data_set)
{
	GString *str = g_string_new (NULL);

	g_hash_table_foreach (form_data_set, hash_encode_foreach, str);
	return g_string_free (str, FALSE);
}

static void
datalist_encode_foreach (GQuark key_id, gpointer value, gpointer str)
{
	encode_pair (str, g_quark_to_string (key_id), value);
}

/**
 * soup_form_encode_datalist:
 * @form_data_set: a datalist containing name/value pairs
 *
 * Encodes @form_data_set into a value of type
 * "application/x-www-form-urlencoded", as defined in the HTML 4.01
 * spec. Unlike soup_form_encode_hash(), this preserves the ordering
 * of the form elements, which may be required in some situations.
 *
 * Return value: the encoded form
 **/
char *
soup_form_encode_datalist (GData **form_data_set)
{
	GString *str = g_string_new (NULL);

	g_datalist_foreach (form_data_set, datalist_encode_foreach, str);
	return g_string_free (str, FALSE);
}

/**
 * soup_form_encode_valist:
 * @first_field: name of the first form field
 * @args: pointer to additional values, as in soup_form_encode()
 *
 * See soup_form_encode(). This is mostly an internal method, used by
 * various other methods such as soup_uri_set_query_from_fields() and
 * soup_form_request_new().
 *
 * Return value: the encoded form
 **/
char *
soup_form_encode_valist (const char *first_field, va_list args)
{
	GString *str = g_string_new (NULL);
	const char *name, *value;

	name = first_field;
	value = va_arg (args, const char *);
	while (name && value) {
		encode_pair (str, name, value);

		name = va_arg (args, const char *);
		if (name)
			value = va_arg (args, const char *);
	}

	return g_string_free (str, FALSE);
}

static SoupMessage *
soup_form_request_for_data (const char *method, const char *uri_string,
			    char *form_data)
{
	SoupMessage *msg;
	SoupURI *uri;

	uri = soup_uri_new (uri_string);
	if (!uri)
		return NULL;

	if (!strcmp (method, "GET")) {
		g_free (uri->query);
		uri->query = form_data;
		form_data = NULL;
	}

	msg = soup_message_new_from_uri (method, uri);

	if (!strcmp (method, "POST")) {
		soup_message_set_request (
			msg, "application/x-www-form-urlencoded",
			SOUP_MEMORY_TAKE,
			form_data, strlen (form_data));
		form_data = NULL;
	}

	if (form_data) {
		g_warning ("invalid method passed to soup_form_request_new");
		g_free (form_data);
	}

	return msg;
}

/**
 * soup_form_request_new:
 * @method: the HTTP method, either "GET" or "POST"
 * @uri: the URI to send the form data to
 * @first_field: name of the first form field
 * @...: value of @first_field, followed by additional field names
 * and values, terminated by %NULL.
 *
 * Creates a new %SoupMessage and sets it up to send the given data
 * to @uri via @method. (That is, if @method is "GET", it will encode
 * the form data into @uri's query field, and if @method is "POST", it
 * will encode it into the %SoupMessage's request_body.)
 *
 * Return value: the new %SoupMessage
 **/
SoupMessage *
soup_form_request_new (const char *method, const char *uri,
		       const char  *first_field, ...)
{
	va_list args;
	char *form_data;

	va_start (args, first_field);
	form_data = soup_form_encode_valist (first_field, args);
	va_end (args);

	return soup_form_request_for_data (method, uri, form_data);
}

/**
 * soup_form_request_new_from_hash:
 * @method: the HTTP method, either "GET" or "POST"
 * @uri: the URI to send the form data to
 * @form_data_set: the data to send to @uri
 *
 * Creates a new %SoupMessage and sets it up to send @form_data_set to
 * @uri via @method, as with soup_form_request_new().
 *
 * Return value: the new %SoupMessage
 **/
SoupMessage *
soup_form_request_new_from_hash (const char *method, const char *uri,
				 GHashTable *form_data_set)
{
	return soup_form_request_for_data (
		method, uri, soup_form_encode_hash (form_data_set));
}

/**
 * soup_form_request_new_from_datalist:
 * @method: the HTTP method, either "GET" or "POST"
 * @uri: the URI to send the form data to
 * @form_data_set: the data to send to @uri
 *
 * Creates a new %SoupMessage and sets it up to send @form_data_set to
 * @uri via @method, as with soup_form_request_new().
 *
 * Return value: the new %SoupMessage
 **/
SoupMessage *
soup_form_request_new_from_datalist (const char *method, const char *uri,
				     GData **form_data_set)
{
	return soup_form_request_for_data (
		method, uri, soup_form_encode_datalist (form_data_set));
}
