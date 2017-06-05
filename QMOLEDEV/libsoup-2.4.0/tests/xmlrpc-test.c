/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2001-2003, Ximian, Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <libsoup/soup.h>

#include "test-utils.h"

SoupSession *session;
static const char *default_uri = "http://localhost:47524/xmlrpc-server.php";
const char *uri = NULL;

static const char *const value_type[] = {
	"BAD",
	"int",
	"boolean",
	"string",
	"double",
	"datetime",
	"base64",
	"struct",
	"array"
};

static gboolean
do_xmlrpc (const char *method, GValue *retval, ...)
{
	SoupMessage *msg;
	va_list args;
	GValueArray *params;
	GError *err = NULL;
	char *body;

	va_start (args, retval);
	params = soup_value_array_from_args (args);
	va_end (args);

	body = soup_xmlrpc_build_method_call (method, params->values,
					      params->n_values);
	g_value_array_free (params);
	if (!body)
		return FALSE;

	msg = soup_message_new ("POST", uri);
	soup_message_set_request (msg, "text/xml", SOUP_MEMORY_TAKE,
				  body, strlen (body));
	soup_session_send_message (session, msg);

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "ERROR: %d %s\n", msg->status_code,
			      msg->reason_phrase);
		g_object_unref (msg);
		return FALSE;
	}

	if (!soup_xmlrpc_parse_method_response (msg->response_body->data,
						msg->response_body->length,
						retval, &err)) {
		if (err) {
			debug_printf (1, "FAULT: %d %s\n", err->code, err->message);
			g_error_free (err);
		} else
			debug_printf (1, "ERROR: could not parse response\n");
		g_object_unref (msg);
		return FALSE;
	}
	g_object_unref (msg);

	return TRUE;
}

static gboolean
check_xmlrpc (GValue *value, GType type, ...)
{
	va_list args;

	if (!G_VALUE_HOLDS (value, type)) {
		debug_printf (1, "ERROR: could not parse response\n");
		g_value_unset (value);
		return FALSE;
	}

	va_start (args, type);
	SOUP_VALUE_GETV (value, type, args);
	va_end (args);
	return TRUE;
}

static gboolean
test_sum (void)
{
	GValueArray *ints;
	int i, val, sum, result;
	GValue retval;
	gboolean ok;

	debug_printf (1, "sum (array of int -> int): ");

	ints = g_value_array_new (10);
	for (i = sum = 0; i < 10; i++) {
		val = rand () % 100;
		debug_printf (2, "%s%d", i == 0 ? "[" : ", ", val);
		soup_value_array_append (ints, G_TYPE_INT, val);
		sum += val;
	}
	debug_printf (2, "] -> ");

	ok = (do_xmlrpc ("sum", &retval,
			G_TYPE_VALUE_ARRAY, ints,
			G_TYPE_INVALID) &&
	      check_xmlrpc (&retval, G_TYPE_INT, &result));
	g_value_array_free (ints);

	if (!ok)
		return FALSE;

	debug_printf (2, "%d: ", result);
	debug_printf (1, "%s\n", result == sum ? "OK!" : "WRONG!");
	return result == sum;
}

static gboolean
test_countBools (void)
{
	GValueArray *bools;
	int i, trues, falses;
	GValue retval;
	int ret_trues, ret_falses;
	gboolean val, ok;
	GHashTable *result;

	debug_printf (1, "countBools (array of boolean -> struct of ints): ");

	bools = g_value_array_new (10);
	for (i = trues = falses = 0; i < 10; i++) {
		val = rand () > (RAND_MAX / 2);
		debug_printf (2, "%s%c", i == 0 ? "[" : ", ", val ? 'T' : 'F');
		soup_value_array_append (bools, G_TYPE_BOOLEAN, val);
		if (val)
			trues++;
		else
			falses++;
	}
	debug_printf (2, "] -> ");

	ok = (do_xmlrpc ("countBools", &retval,
			 G_TYPE_VALUE_ARRAY, bools,
			 G_TYPE_INVALID) &&
	      check_xmlrpc (&retval, G_TYPE_HASH_TABLE, &result));
	g_value_array_free (bools);
	if (!ok)
		return FALSE;

	if (!soup_value_hash_lookup (result, "true", G_TYPE_INT, &ret_trues)) {
		debug_printf (1, "NO 'true' value in response\n");
		return FALSE;
	}
	if (!soup_value_hash_lookup (result, "false", G_TYPE_INT, &ret_falses)) {
		debug_printf (1, "NO 'false' value in response\n");
		return FALSE;
	}
	g_hash_table_destroy (result);

	debug_printf (2, "{ true: %d, false: %d } ", ret_trues, ret_falses);
	ok = (trues == ret_trues) && (falses == ret_falses);
	debug_printf (1, "%s\n", ok ? "OK!" : "WRONG!");
	return ok;
}

static gboolean
test_md5sum (void)
{
	GByteArray *data, *result;
	int i;
	GChecksum *checksum;
	guchar digest[16];
	gsize digest_len = sizeof (digest);
	GValue retval;
	gboolean ok;

	debug_printf (1, "md5sum (base64 -> base64): ");

	data = g_byte_array_new ();
	g_byte_array_set_size (data, 256);
	for (i = 0; i < data->len; i++)
		data->data[i] = (char)(rand ());

	checksum = g_checksum_new (G_CHECKSUM_MD5);
	g_checksum_update (checksum, data->data, data->len);
	g_checksum_get_digest (checksum, digest, &digest_len);
	g_checksum_free (checksum);

	ok = (do_xmlrpc ("md5sum", &retval,
			 SOUP_TYPE_BYTE_ARRAY, data,
			 G_TYPE_INVALID) &&
	      check_xmlrpc (&retval, SOUP_TYPE_BYTE_ARRAY, &result));
	g_byte_array_free (data, TRUE);
	if (!ok)
		return FALSE;

	if (result->len != digest_len) {
		debug_printf (1, "result has WRONG length (%d)\n", result->len);
		g_byte_array_free (result, TRUE);
		return FALSE;
	}

	ok = (memcmp (digest, result->data, digest_len) == 0);
	debug_printf (1, "%s\n", ok ? "OK!" : "WRONG!");
	g_byte_array_free (result, TRUE);
	return ok;
}

static gboolean
test_dateChange (void)
{
	GHashTable *structval;
	SoupDate *date, *result;
	char *timestamp;
	GValue retval;
	gboolean ok;

	debug_printf (1, "dateChange (date, struct of ints -> time): ");

	date = soup_date_new (1970 + (rand () % 50),
			      1 + rand () % 12,
			      1 + rand () % 28,
			      rand () % 24,
			      rand () % 60,
			      rand () % 60);
	if (debug_level >= 2) {
		timestamp = soup_date_to_string (date, SOUP_DATE_ISO8601_XMLRPC);
		debug_printf (2, "date: %s, {", timestamp);
		g_free (timestamp);
	}

	structval = soup_value_hash_new ();

	if (rand () % 3) {
		date->year = 1970 + (rand () % 50);
		debug_printf (2, "tm_year: %d, ", date->year - 1900);
		soup_value_hash_insert (structval, "tm_year",
					G_TYPE_INT, date->year - 1900);
	}
	if (rand () % 3) {
		date->month = 1 + rand () % 12;
		debug_printf (2, "tm_mon: %d, ", date->month - 1);
		soup_value_hash_insert (structval, "tm_mon",
					G_TYPE_INT, date->month - 1);
	}
	if (rand () % 3) {
		date->day = 1 + rand () % 28;
		debug_printf (2, "tm_mday: %d, ", date->day);
		soup_value_hash_insert (structval, "tm_mday",
					G_TYPE_INT, date->day);
	}
	if (rand () % 3) {
		date->hour = rand () % 24;
		debug_printf (2, "tm_hour: %d, ", date->hour);
		soup_value_hash_insert (structval, "tm_hour",
					G_TYPE_INT, date->hour);
	}
	if (rand () % 3) {
		date->minute = rand () % 60;
		debug_printf (2, "tm_min: %d, ", date->minute);
		soup_value_hash_insert (structval, "tm_min",
					G_TYPE_INT, date->minute);
	}
	if (rand () % 3) {
		date->second = rand () % 60;
		debug_printf (2, "tm_sec: %d, ", date->second);
		soup_value_hash_insert (structval, "tm_sec",
					G_TYPE_INT, date->second);
	}

	debug_printf (2, "} -> ");

	ok = (do_xmlrpc ("dateChange", &retval,
			 SOUP_TYPE_DATE, date,
			 G_TYPE_HASH_TABLE, structval,
			 G_TYPE_INVALID) &&
	      check_xmlrpc (&retval, SOUP_TYPE_DATE, &result));
	g_hash_table_destroy (structval);
	if (!ok) {
		soup_date_free (date);
		return FALSE;
	}

	if (debug_level >= 2) {
		timestamp = soup_date_to_string (result, SOUP_DATE_ISO8601_XMLRPC);
		debug_printf (2, "%s: ", timestamp);
		g_free (timestamp);
	}

	ok = ((date->year   == result->year) &&
	      (date->month  == result->month) &&
	      (date->day    == result->day) &&
	      (date->hour   == result->hour) &&
	      (date->minute == result->minute) &&
	      (date->second == result->second));
	soup_date_free (date);
	soup_date_free (result);

	debug_printf (1, "%s\n", ok ? "OK!" : "WRONG!");
	return ok;
}

static const char *const echo_strings[] = {
	"This is a test",
	"& so is this",
	"and so is <this>",
	"&amp; so is &lt;this&gt;"
};
#define N_ECHO_STRINGS G_N_ELEMENTS (echo_strings)

static gboolean
test_echo (void)
{
	GValueArray *originals, *echoes;
	GValue retval;
	int i;

	debug_printf (1, "echo (array of string -> array of string): ");

	originals = g_value_array_new (N_ECHO_STRINGS);
	for (i = 0; i < N_ECHO_STRINGS; i++) {
		soup_value_array_append (originals, G_TYPE_STRING, echo_strings[i]);
		debug_printf (2, "%s\"%s\"", i == 0 ? "[" : ", ", echo_strings[i]);
	}
	debug_printf (2, "] -> ");

	if (!(do_xmlrpc ("echo", &retval,
			 G_TYPE_VALUE_ARRAY, originals,
			 G_TYPE_INVALID) &&
	      check_xmlrpc (&retval, G_TYPE_VALUE_ARRAY, &echoes))) {
		g_value_array_free (originals);
		return FALSE;
	}
	g_value_array_free (originals);

	if (debug_level >= 2) {
		for (i = 0; i < echoes->n_values; i++) {
			debug_printf (2, "%s\"%s\"", i == 0 ? "[" : ", ",
				      g_value_get_string (&echoes->values[i]));
		}
		debug_printf (2, "] -> ");
	}

	if (echoes->n_values != N_ECHO_STRINGS) {
		debug_printf (1, " WRONG! Wrong number of return strings");
		g_value_array_free (echoes);
		return FALSE;
	}

	for (i = 0; i < echoes->n_values; i++) {
		if (strcmp (echo_strings[i], g_value_get_string (&echoes->values[i])) != 0) {
			debug_printf (1, " WRONG! Mismatch at %d\n", i + 1);
			g_value_array_free (echoes);
			return FALSE;
		}
	}

	debug_printf (1, "OK!\n");
	g_value_array_free (echoes);
	return TRUE;
}

static gboolean
do_bad_xmlrpc (const char *body)
{
	SoupMessage *msg;
	GError *err = NULL;
	GValue retval;

	msg = soup_message_new ("POST", uri);
	soup_message_set_request (msg, "text/xml", SOUP_MEMORY_COPY,
				  body, strlen (body));
	soup_session_send_message (session, msg);

	if (!SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) {
		debug_printf (1, "ERROR: %d %s\n", msg->status_code,
			      msg->reason_phrase);
		g_object_unref (msg);
		return FALSE;
	}

	if (!soup_xmlrpc_parse_method_response (msg->response_body->data,
						msg->response_body->length,
						&retval, &err)) {
		if (err) {
			debug_printf (1, "FAULT: %d %s (OK!)\n",
				      err->code, err->message);
			g_error_free (err);
			g_object_unref (msg);
			return TRUE;
		} else
			debug_printf (1, "ERROR: could not parse response\n");
	} else
		debug_printf (1, "Unexpectedly got successful response!\n");

	g_object_unref (msg);
	return FALSE;
}

static gboolean
test_fault_malformed (void)
{
	debug_printf (1, "malformed request: ");

	return do_bad_xmlrpc ("<methodCall/>");
}

static gboolean
test_fault_method (void)
{
	debug_printf (1, "request to non-existent method: ");

	return do_bad_xmlrpc ("<methodCall><methodName>no_such_method</methodName><params><param><value><int>1</int></value></param></params></methodCall>");
}

static gboolean
test_fault_args (void)
{
	debug_printf (1, "request with invalid args: ");

	return do_bad_xmlrpc ("<methodCall><methodName>sum</methodName><params><param><value><int>1</int></value></param></params></methodCall>");
}

static GOptionEntry uri_entry[] = {
        { "uri", 'u', 0, G_OPTION_ARG_STRING, &uri,
          "Alternate URI for server", NULL },
        { NULL }
};

int
main (int argc, char **argv)
{
	test_init (argc, argv, uri_entry);

	if (!uri) {
		apache_init ();
		uri = default_uri;
	}

	srand (time (NULL));

	session = soup_test_session_new (SOUP_TYPE_SESSION_SYNC, NULL);

	if (!test_sum ())
		errors++;
	if (!test_countBools ())
		errors++;
	if (!test_md5sum ())
		errors++;
	if (!test_dateChange ())
		errors++;
	if (!test_echo ())
		errors++;
	if (!test_fault_malformed ())
		errors++;
	if (!test_fault_method ())
		errors++;
	if (!test_fault_args ())
		errors++;

	soup_session_abort (session);
	g_object_unref (session);

	test_cleanup ();
	return errors != 0;
}
