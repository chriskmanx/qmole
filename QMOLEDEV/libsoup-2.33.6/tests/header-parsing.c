#include <config.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "libsoup/soup-message.h"
#include "libsoup/soup-headers.h"

#include "test-utils.h"

typedef struct {
	const char *name, *value;
} Header;

static struct RequestTest {
	const char *description;
	const char *request;
	int length;
	guint status;
	const char *method, *path;
	SoupHTTPVersion version;
	Header headers[4];
} reqtests[] = {
	/**********************/
	/*** VALID REQUESTS ***/
	/**********************/

	{ "HTTP 1.0 request with no headers",
	  "GET / HTTP/1.0\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_0,
	  { { NULL } }
	},

	{ "Req w/ 1 header",
	  "GET / HTTP/1.1\r\nHost: example.com\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { NULL }
	  }
	},

	{ "Req w/ 1 header, no leading whitespace",
	  "GET / HTTP/1.1\r\nHost:example.com\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { NULL }
	  }
	},

	{ "Req w/ 1 header including trailing whitespace",
	  "GET / HTTP/1.1\r\nHost: example.com \r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { NULL }
	  }
	},

	{ "Req w/ 1 header, wrapped",
	  "GET / HTTP/1.1\r\nFoo: bar\r\n baz\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Foo", "bar baz" },
	    { NULL }
	  }
	},

	{ "Req w/ 1 header, wrapped with additional whitespace",
	  "GET / HTTP/1.1\r\nFoo: bar \r\n  baz\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Foo", "bar baz" },
	    { NULL }
	  }
	},

	{ "Req w/ 1 header, wrapped with tab",
	  "GET / HTTP/1.1\r\nFoo: bar\r\n\tbaz\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Foo", "bar baz" },
	    { NULL }
	  }
	},

	{ "Req w/ 1 header, wrapped before value",
	  "GET / HTTP/1.1\r\nFoo:\r\n bar baz\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Foo", "bar baz" },
	    { NULL }
	  }
	},

	{ "Req w/ 1 header with empty value",
	  "GET / HTTP/1.1\r\nHost:\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "" },
	    { NULL }
	  }
	},

	{ "Req w/ 2 headers",
	  "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { "Connection", "close" },
	    { NULL }
	  }
	},

	{ "Req w/ 3 headers",
	  "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\nBlah: blah\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { "Connection", "close" },
	    { "Blah", "blah" },
	    { NULL }
	  }
	},

	{ "Req w/ 3 headers, 1st wrapped",
	  "GET / HTTP/1.1\r\nFoo: bar\r\n baz\r\nConnection: close\r\nBlah: blah\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Foo", "bar baz" },
	    { "Connection", "close" },
	    { "Blah", "blah" },
	    { NULL }
	  }
	},

	{ "Req w/ 3 headers, 2nd wrapped",
	  "GET / HTTP/1.1\r\nConnection: close\r\nBlah: blah\r\nFoo: bar\r\n baz\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Connection", "close" },
	    { "Blah", "blah" },
	    { "Foo", "bar baz" },
	    { NULL }
	  }
	},

	{ "Req w/ 3 headers, 3rd wrapped",
	  "GET / HTTP/1.1\r\nConnection: close\r\nBlah: blah\r\nFoo: bar\r\n baz\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Connection", "close" },
	    { "Blah", "blah" },
	    { "Foo", "bar baz" },
	    { NULL }
	  }
	},

	{ "Req w/ same header multiple times",
	  "GET / HTTP/1.1\r\nFoo: bar\r\nFoo: baz\r\nFoo: quux\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Foo", "bar, baz, quux" },
	    { NULL }
	  }
	},

	{ "Connection header on HTTP/1.0 message",
	  "GET / HTTP/1.0\r\nFoo: bar\r\nConnection: Bar, Quux\r\nBar: baz\r\nQuux: foo\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_0,
	  { { "Foo", "bar" },
	    { "Connection", "Bar, Quux" },
	    { NULL }
	  }
	},

	/****************************/
	/*** RECOVERABLE REQUESTS ***/
	/****************************/

	/* RFC 2616 section 4.1 says we SHOULD accept this */

	{ "Spurious leading CRLF",
	  "\r\nGET / HTTP/1.1\r\nHost: example.com\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { NULL }
	  }
	},

	/* RFC 2616 section 3.1 says we MUST accept this */

	{ "HTTP/01.01 request",
	  "GET / HTTP/01.01\r\nHost: example.com\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { NULL }
	  }
	},

	/* RFC 2616 section 19.3 says we SHOULD accept these */

	{ "LF instead of CRLF after header",
	  "GET / HTTP/1.1\nHost: example.com\nConnection: close\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { "Connection", "close" },
	    { NULL }
	  }
	},

	{ "LF instead of CRLF after Request-Line",
	  "GET / HTTP/1.1\nHost: example.com\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { NULL }
	  }
	},

	{ "Req w/ incorrect whitespace in Request-Line",
	  "GET  /\tHTTP/1.1\r\nHost: example.com\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { NULL }
	  }
	},

	{ "Req w/ incorrect whitespace after Request-Line",
	  "GET / HTTP/1.1 \r\nHost: example.com\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { NULL }
	  }
	},

	/* qv bug 579318, do_bad_header_tests() below */
	{ "Req w/ mangled header",
	  "GET / HTTP/1.1\r\nHost: example.com\r\nFoo one\r\nBar: two\r\n", -1,
	  SOUP_STATUS_OK,
	  "GET", "/", SOUP_HTTP_1_1,
	  { { "Host", "example.com" },
	    { "Bar", "two" },
	    { NULL }
	  }
	},

	/************************/
	/*** INVALID REQUESTS ***/
	/************************/

	{ "HTTP 0.9 request; not supported",
	  "GET /\r\n", -1,
	  SOUP_STATUS_BAD_REQUEST,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "HTTP 1.2 request (no such thing)",
	  "GET / HTTP/1.2\r\n", -1,
	  SOUP_STATUS_HTTP_VERSION_NOT_SUPPORTED,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "HTTP 2000 request (no such thing)",
	  "GET / HTTP/2000.0\r\n", -1,
	  SOUP_STATUS_HTTP_VERSION_NOT_SUPPORTED,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "Non-HTTP request",
	  "GET / SOUP/1.1\r\nHost: example.com\r\n", -1,
	  SOUP_STATUS_BAD_REQUEST,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "Junk after Request-Line",
	  "GET / HTTP/1.1 blah\r\nHost: example.com\r\n", -1,
	  SOUP_STATUS_BAD_REQUEST,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "NUL in Method",
	  "G\x00T / HTTP/1.1\r\nHost: example.com\r\n", 37,
	  SOUP_STATUS_BAD_REQUEST,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "NUL in Path",
	  "GET /\x00 HTTP/1.1\r\nHost: example.com\r\n", 38,
	  SOUP_STATUS_BAD_REQUEST,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "NUL in Header",
	  "GET / HTTP/1.1\r\nHost: example\x00com\r\n", 37,
	  SOUP_STATUS_BAD_REQUEST,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "No terminating CRLF",
	  "GET / HTTP/1.1\r\nHost: example.com", -1,
	  SOUP_STATUS_BAD_REQUEST,
	  NULL, NULL, -1,
	  { { NULL } }
	},

	{ "Unrecognized expectation",
	  "GET / HTTP/1.1\r\nHost: example.com\r\nExpect: the-impossible\r\n", -1,
	  SOUP_STATUS_EXPECTATION_FAILED,
	  NULL, NULL, -1,
	  { { NULL } }
	}
};
static const int num_reqtests = G_N_ELEMENTS (reqtests);

static struct ResponseTest {
	const char *description;
	const char *response;
	int length;
	SoupHTTPVersion version;
	guint status_code;
	const char *reason_phrase;
	Header headers[4];
} resptests[] = {
	/***********************/
	/*** VALID RESPONSES ***/
	/***********************/

	{ "HTTP 1.0 response w/ no headers",
	  "HTTP/1.0 200 ok\r\n", -1,
	  SOUP_HTTP_1_0, SOUP_STATUS_OK, "ok",
	  { { NULL } }
	},

	{ "HTTP 1.1 response w/ no headers",
	  "HTTP/1.1 200 ok\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { NULL } }
	},

	{ "Response w/ multi-word Reason-Phrase",
	  "HTTP/1.1 400 bad request\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_BAD_REQUEST, "bad request",
	  { { NULL } }
	},

	{ "Response w/ 1 header",
	  "HTTP/1.1 200 ok\r\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar" },
	    { NULL }
	  }
	},

	{ "Response w/ 2 headers",
	  "HTTP/1.1 200 ok\r\nFoo: bar\r\nBaz: quux\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar" },
	    { "Baz", "quux" },
	    { NULL }
	  }
	},

	{ "Response w/ same header multiple times",
	  "HTTP/1.1 200 ok\r\nFoo: bar\r\nFoo: baz\r\nFoo: quux\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar, baz, quux" },
	    { NULL }
	  }
	},

	{ "Response w/ no reason phrase",
	  "HTTP/1.1 200 \r\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "",
	  { { "Foo", "bar" },
	    { NULL }
	  }
	},

	{ "Connection header on HTTP/1.0 message",
	  "HTTP/1.0 200 ok\r\nFoo: bar\r\nConnection: Bar\r\nBar: quux\r\n", -1,
	  SOUP_HTTP_1_0, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar" },
	    { "Connection", "Bar" },
	    { NULL }
	  }
	},

	/*****************************/
	/*** RECOVERABLE RESPONSES ***/
	/*****************************/

	/* RFC 2616 section 3.1 says we MUST accept this */

	{ "HTTP/01.01 response",
	  "HTTP/01.01 200 ok\r\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar" },
	    { NULL }
	  }
	},

	/* RFC 2616 section 19.3 says we SHOULD accept these */

	{ "Response w/ LF instead of CRLF after Status-Line",
	  "HTTP/1.1 200 ok\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar" },
	    { NULL }
	  }
	},

	{ "Response w/ incorrect spacing in Status-Line",
	  "HTTP/1.1  200\tok\r\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar" },
	    { NULL }
	  }
	},

	{ "Response w/ no reason phrase or preceding SP",
	  "HTTP/1.1 200\r\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "",
	  { { "Foo", "bar" },
	    { NULL }
	  }
	},

	{ "Response w/ no whitespace after status code",
	  "HTTP/1.1 200ok\r\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar" },
	    { NULL }
	  }
	},

	/* Shoutcast support */
	{ "Shoutcast server not-quite-HTTP",
	  "ICY 200 OK\r\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_0, SOUP_STATUS_OK, "OK",
	  { { "Foo", "bar" },
	    { NULL }
	  }
	},

	/* qv bug 579318, do_bad_header_tests() below */
	{ "Response w/ mangled header",
	  "HTTP/1.1 200 ok\r\nFoo: one\r\nBar two:2\r\nBaz: three\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "one" },
	    { "Baz", "three" },
	    { NULL }
	  }
	},

	/* qv bug 602863 */
	{ "HTTP 1.1 response with leading line break",
	  "\nHTTP/1.1 200 ok\r\nFoo: bar\r\n", -1,
	  SOUP_HTTP_1_1, SOUP_STATUS_OK, "ok",
	  { { "Foo", "bar" },
	    { NULL } }
	},

	/*************************/
	/*** INVALID RESPONSES ***/
	/*************************/

	{ "Invalid HTTP version",
	  "HTTP/1.2 200 OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "Non-HTTP response",
	  "SOUP/1.1 200 OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "Non-numeric status code",
	  "HTTP/1.1 XXX OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "No status code",
	  "HTTP/1.1 OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "One-digit status code",
	  "HTTP/1.1 2 OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "Two-digit status code",
	  "HTTP/1.1 20 OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "Four-digit status code",
	  "HTTP/1.1 2000 OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "Status code < 100",
	  "HTTP/1.1 001 OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "Status code > 599",
	  "HTTP/1.1 600 OK\r\nFoo: bar\r\n", -1,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "NUL in Reason Phrase",
	  "HTTP/1.1 200 O\x00K\r\nFoo: bar\r\n", 28,
	  -1, 0, NULL,
	  { { NULL } }
	},

	{ "NUL in Header",
	  "HTTP/1.1 200 OK\r\nFoo: b\x00ar\r\n", 28,
	  -1, 0, NULL,
	  { { NULL } }
	},
};
static const int num_resptests = G_N_ELEMENTS (resptests);

static struct QValueTest {
	const char *header_value;
	const char *acceptable[7];
	const char *unacceptable[2];
} qvaluetests[] = {
	{ "text/plain; q=0.5, text/html,\t  text/x-dvi; q=0.8, text/x-c",
	  { "text/html", "text/x-c", "text/x-dvi", "text/plain", NULL },
	  { NULL },
	},

	{ "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5",
	  { "text/html;level=1", "text/html", "*/*", "text/html;level=2",
	    "text/*", NULL },
	  { NULL }
	},

	{ "gzip;q=1.0, identity; q=0.5, *;q=0",
	  { "gzip", "identity", NULL },
	  { "*", NULL },
	}
};
static const int num_qvaluetests = G_N_ELEMENTS (qvaluetests);

static void
print_header (const char *name, const char *value, gpointer data)
{
	debug_printf (1, "              '%s': '%s'\n", name, value);
}

static gboolean
check_headers (Header *headers, SoupMessageHeaders *hdrs)
{
	GSList *header_names, *h;
	SoupMessageHeadersIter iter;
	const char *name, *value;
	gboolean ok = TRUE;
	int i;

	header_names = NULL;
	soup_message_headers_iter_init (&iter, hdrs);
	while (soup_message_headers_iter_next (&iter, &name, &value)) {
		if (!g_slist_find_custom (header_names, name,
					  (GCompareFunc)strcmp))
			header_names = g_slist_append (header_names, (char *)name);
	}

	for (i = 0, h = header_names; headers[i].name && h; i++, h = h->next) {
		if (strcmp (h->data, headers[i].name) != 0) {
			ok = FALSE;
			break;
		}
		value = soup_message_headers_get_list (hdrs, headers[i].name);
		if (strcmp (value, headers[i].value) != 0) {
			ok = FALSE;
			break;
		}
	}
	if (headers[i].name || h)
		ok = FALSE;
	g_slist_free (header_names);
	return ok;
}

static void
do_request_tests (void)
{
	int i, len, h;
	char *method, *path;
	SoupHTTPVersion version;
	SoupMessageHeaders *headers;
	guint status;

	debug_printf (1, "Request tests\n");
	for (i = 0; i < num_reqtests; i++) {
		gboolean ok = TRUE;

		debug_printf (1, "%2d. %s (%s): ", i + 1, reqtests[i].description,
			      soup_status_get_phrase (reqtests[i].status));

		headers = soup_message_headers_new (SOUP_MESSAGE_HEADERS_REQUEST);
		method = path = NULL;

		if (reqtests[i].length == -1)
			len = strlen (reqtests[i].request);
		else
			len = reqtests[i].length;
		status = soup_headers_parse_request (reqtests[i].request, len,
						     headers, &method, &path,
						     &version);
		if (SOUP_STATUS_IS_SUCCESSFUL (status)) {
			if ((reqtests[i].method && strcmp (reqtests[i].method, method) != 0) || !reqtests[i].method)
				ok = FALSE;
			if ((reqtests[i].path && strcmp (reqtests[i].path, path) != 0) || !reqtests[i].path)
				ok = FALSE;
			if (reqtests[i].version != version)
				ok = FALSE;

			if (!check_headers (reqtests[i].headers, headers))
				ok = FALSE;
		} else {
			if (status != reqtests[i].status)
				ok = FALSE;
		}

		if (ok)
			debug_printf (1, "OK!\n");
		else {
			debug_printf (1, "BAD!\n");
			errors++;
			if (reqtests[i].method) {
				debug_printf (1, "    expected: '%s' '%s' 'HTTP/1.%d'\n",
					      reqtests[i].method,
					      reqtests[i].path,
					      reqtests[i].version);
				for (h = 0; reqtests[i].headers[h].name; h++) {
					debug_printf (1, "              '%s': '%s'\n",
						      reqtests[i].headers[h].name,
						      reqtests[i].headers[h].value);
				}
			} else {
				debug_printf (1, "    expected: %s\n",
					      soup_status_get_phrase (reqtests[i].status));
			}
			if (method) {
				debug_printf (1, "         got: '%s' '%s' 'HTTP/1.%d'\n",
					      method, path, version);
				soup_message_headers_foreach (headers, print_header, NULL);
			} else {
				debug_printf (1, "         got: %s\n",
					      soup_status_get_phrase (status));
			}
		}

		g_free (method);
		g_free (path);
		soup_message_headers_free (headers);
	}
	debug_printf (1, "\n");
}

static void
do_response_tests (void)
{
	int i, len, h;
	guint status_code;
	char *reason_phrase;
	SoupHTTPVersion version;
	SoupMessageHeaders *headers;

	debug_printf (1, "Response tests\n");
	for (i = 0; i < num_resptests; i++) {
		gboolean ok = TRUE;

		debug_printf (1, "%2d. %s (%s): ", i + 1, resptests[i].description,
			      resptests[i].reason_phrase ? "should parse" : "should NOT parse");

		headers = soup_message_headers_new (SOUP_MESSAGE_HEADERS_RESPONSE);
		reason_phrase = NULL;

		if (resptests[i].length == -1)
			len = strlen (resptests[i].response);
		else
			len = resptests[i].length;
		if (soup_headers_parse_response (resptests[i].response, len,
						 headers, &version,
						 &status_code, &reason_phrase)) {
			if (resptests[i].version != version)
				ok = FALSE;
			if (resptests[i].status_code != status_code)
				ok = FALSE;
			if ((resptests[i].reason_phrase && strcmp (resptests[i].reason_phrase, reason_phrase) != 0) || !resptests[i].reason_phrase)
				ok = FALSE;

			if (!check_headers (resptests[i].headers, headers))
				ok = FALSE;
		} else {
			if (resptests[i].reason_phrase)
				ok = FALSE;
		}

		if (ok)
			debug_printf (1, "OK!\n");
		else {
			debug_printf (1, "BAD!\n");
			errors++;
			if (resptests[i].reason_phrase) {
				debug_printf (1, "    expected: 'HTTP/1.%d' '%03d' '%s'\n",
					      resptests[i].version,
					      resptests[i].status_code,
					      resptests[i].reason_phrase);
				for (h = 0; resptests[i].headers[h].name; h++) {
					debug_printf (1, "              '%s': '%s'\n",
						      resptests[i].headers[h].name,
						      resptests[i].headers[h].value);
				}
			} else
				debug_printf (1, "    expected: parse error\n");
			if (reason_phrase) {
				debug_printf (1, "         got: 'HTTP/1.%d' '%03d' '%s'\n",
					      version, status_code, reason_phrase);
				soup_message_headers_foreach (headers, print_header, NULL);
			} else
				debug_printf (1, "         got: parse error\n");
		}

		g_free (reason_phrase);
		soup_message_headers_free (headers);
	}
	debug_printf (1, "\n");
}

static void
do_qvalue_tests (void)
{
	int i, j;
	GSList *acceptable, *unacceptable, *iter;
	gboolean wrong;

	debug_printf (1, "qvalue tests\n");
	for (i = 0; i < num_qvaluetests; i++) {
		debug_printf (1, "%2d. %s:\n", i + 1, qvaluetests[i].header_value);

		unacceptable = NULL;
		acceptable = soup_header_parse_quality_list (qvaluetests[i].header_value,
							     &unacceptable);

		debug_printf (1, "    acceptable: ");
		wrong = FALSE;
		if (acceptable) {
			for (iter = acceptable, j = 0; iter; iter = iter->next, j++) {
				debug_printf (1, "%s ", (char *)iter->data);
				if (!qvaluetests[i].acceptable[j] ||
				    strcmp (iter->data, qvaluetests[i].acceptable[j]) != 0)
					wrong = TRUE;
			}
			debug_printf (1, "\n");
			soup_header_free_list (acceptable);
		} else
			debug_printf (1, "(none)\n");
		if (wrong) {
			debug_printf (1, "    WRONG! expected: ");
			for (j = 0; qvaluetests[i].acceptable[j]; j++)
				debug_printf (1, "%s ", qvaluetests[i].acceptable[j]);
			debug_printf (1, "\n");
			errors++;
		}

		debug_printf (1, "  unacceptable: ");
		wrong = FALSE;
		if (unacceptable) {
			for (iter = unacceptable, j = 0; iter; iter = iter->next, j++) {
				debug_printf (1, "%s ", (char *)iter->data);
				if (!qvaluetests[i].unacceptable[j] ||
				    strcmp (iter->data, qvaluetests[i].unacceptable[j]) != 0)
					wrong = TRUE;
			}
			debug_printf (1, "\n");
			soup_header_free_list (unacceptable);
		} else
			debug_printf (1, "(none)\n");
		if (wrong) {
			debug_printf (1, "    WRONG! expected: ");
			for (j = 0; qvaluetests[i].unacceptable[j]; j++)
				debug_printf (1, "%s ", qvaluetests[i].unacceptable[j]);
			debug_printf (1, "\n");
			errors++;
		}

		debug_printf (1, "\n");
	}
}

#define RFC2231_TEST_FILENAME "t\xC3\xA9st.txt"
#define RFC2231_TEST_HEADER "attachment; filename*=UTF-8''t%C3%A9st.txt"

static void
do_rfc2231_tests (void)
{
	SoupMessageHeaders *hdrs;
	GHashTable *params;
	const char *header, *filename;
	char *disposition;

	debug_printf (1, "rfc2231 tests\n");

	hdrs = soup_message_headers_new (SOUP_MESSAGE_HEADERS_MULTIPART);
	params = g_hash_table_new (g_str_hash, g_str_equal);
	g_hash_table_insert (params, "filename", RFC2231_TEST_FILENAME);
	soup_message_headers_set_content_disposition (hdrs, "attachment", params);
	g_hash_table_destroy (params);

	header = soup_message_headers_get_one (hdrs, "Content-Disposition");
	if (!strcmp (header, RFC2231_TEST_HEADER))
		debug_printf (1, "  encoded OK\n");
	else {
		debug_printf (1, "  encoding FAILED!\n    expected: %s\n    got:      %s\n",
			      RFC2231_TEST_HEADER, header);
		errors++;
	}

	soup_message_headers_clear (hdrs);
	soup_message_headers_append (hdrs, "Content-Disposition",
				     RFC2231_TEST_HEADER);
	if (!soup_message_headers_get_content_disposition (hdrs,
							   &disposition,
							   &params)) {
		debug_printf (1, "  decoding FAILED!\n    could not parse\n");
		errors++;
		return;
	}
	g_free (disposition);

	filename = g_hash_table_lookup (params, "filename");
	if (!filename) {
		debug_printf (1, "  decoding FAILED!\n    could not file filename\n");
		errors++;
	} else if (strcmp (filename, RFC2231_TEST_FILENAME) != 0) {
		debug_printf (1, "  decoding FAILED!\n    expected: %s\n    got:      %s\n",
			      RFC2231_TEST_FILENAME, filename);
		errors++;
	} else
		debug_printf (1, "  decoded OK\n");

	g_hash_table_destroy (params);
	soup_message_headers_free (hdrs);

	debug_printf (1, "\n");
}

#define CONTENT_TYPE_TEST_MIME_TYPE "text/plain"
#define CONTENT_TYPE_TEST_ATTRIBUTE "charset"
#define CONTENT_TYPE_TEST_VALUE     "US-ASCII"
#define CONTENT_TYPE_TEST_HEADER    "text/plain; charset=US-ASCII"

#define CONTENT_TYPE_BAD_HEADER     "plain text, not text/html"

static void
do_content_type_tests (void)
{
	SoupMessageHeaders *hdrs;
	GHashTable *params;
	const char *header, *mime_type;

	debug_printf (1, "Content-Type tests\n");

	hdrs = soup_message_headers_new (SOUP_MESSAGE_HEADERS_MULTIPART);
	params = g_hash_table_new (g_str_hash, g_str_equal);
	g_hash_table_insert (params, CONTENT_TYPE_TEST_ATTRIBUTE,
			     CONTENT_TYPE_TEST_VALUE);
	soup_message_headers_set_content_type (hdrs, CONTENT_TYPE_TEST_MIME_TYPE, params);
	g_hash_table_destroy (params);

	header = soup_message_headers_get_one (hdrs, "Content-Type");
	if (!strcmp (header, CONTENT_TYPE_TEST_HEADER))
		debug_printf (1, "  encoded OK\n");
	else {
		debug_printf (1, "  encoding FAILED!\n    expected: %s\n    got:      %s\n",
			      CONTENT_TYPE_TEST_HEADER, header);
		errors++;
	}

	soup_message_headers_clear (hdrs);
	soup_message_headers_append (hdrs, "Content-Type",
				     CONTENT_TYPE_TEST_MIME_TYPE);
	/* Add a second Content-Type header: should be ignored */
	soup_message_headers_append (hdrs, "Content-Type",
				     CONTENT_TYPE_TEST_MIME_TYPE);

	mime_type = soup_message_headers_get_content_type (hdrs, &params);
	if (!mime_type) {
		debug_printf (1, "  decoding FAILED!\n    could not parse\n");
		errors++;
	}

	if (mime_type && strcmp (mime_type, CONTENT_TYPE_TEST_MIME_TYPE) != 0) {
		debug_printf (1, "  decoding FAILED!\n    bad returned MIME type: %s\n",
			      mime_type);
		errors++;
	} else if (params && g_hash_table_size (params) != 0) {
		debug_printf (1, "  decoding FAILED!\n    params contained %d params (should be 0)\n",
			      g_hash_table_size (params));
		errors++;
	} else
		debug_printf (1, "  decoded OK\n");

	if (params)
		g_hash_table_destroy (params);

	soup_message_headers_clear (hdrs);
	soup_message_headers_append (hdrs, "Content-Type",
				     CONTENT_TYPE_BAD_HEADER);
	mime_type = soup_message_headers_get_content_type (hdrs, &params);
	if (mime_type) {
		debug_printf (1, "  Bad content rejection FAILED!\n");
		errors++;
	} else
		debug_printf (1, "  Bad content rejection OK\n");

	soup_message_headers_free (hdrs);

	debug_printf (1, "\n");
}

struct {
	const char *name, *value;
} test_params[] = {
	{ "one", "foo" },
	{ "two", "test with spaces" },
	{ "three", "test with \"quotes\" and \\s" },
	{ "four", NULL },
	{ "five", "test with \xC3\xA1\xC3\xA7\xC4\x89\xC3\xA8\xC3\xB1\xC5\xA3\xC5\xA1" }
};

#define TEST_PARAMS_RESULT "one=foo, two=\"test with spaces\", three=\"test with \\\"quotes\\\" and \\\\s\", four, five*=UTF-8''test%20with%20%C3%A1%C3%A7%C4%89%C3%A8%C3%B1%C5%A3%C5%A1"

static void
do_append_param_tests (void)
{
	GString *params;
	int i;

	debug_printf (1, "soup_header_g_string_append_param() tests\n");

	params = g_string_new (NULL);
	for (i = 0; i < G_N_ELEMENTS (test_params); i++) {
		if (i > 0)
			g_string_append (params, ", ");
		soup_header_g_string_append_param (params,
						   test_params[i].name,
						   test_params[i].value);
	}
	if (strcmp (params->str, TEST_PARAMS_RESULT) != 0) {
		debug_printf (1, "  FAILED!\n    expected: %s\n    got: %s\n",
			      TEST_PARAMS_RESULT, params->str);
		errors++;
	} else
		debug_printf (1, "  OK\n");
	g_string_free (params, TRUE);

	debug_printf (1, "\n");
}

static const struct {
	const char *description, *name, *value;
} bad_headers[] = {
	{ "Empty name", "", "value" },
	{ "Name with spaces", "na me", "value" },
	{ "Name with colon", "na:me", "value" },
	{ "Name with CR", "na\rme", "value" },
	{ "Name with LF", "na\nme", "value" },
	{ "Name with tab", "na\tme", "value" },
	{ "Value with CR", "name", "val\rue" },
	{ "Value with LF", "name", "val\nue" },
	{ "Value with LWS", "name", "val\r\n ue" }
};

static void
do_bad_header_tests (void)
{
	SoupMessageHeaders *hdrs;
	int i;

	debug_printf (1, "bad header rejection tests\n");

	hdrs = soup_message_headers_new (SOUP_MESSAGE_HEADERS_MULTIPART);
	for (i = 0; i < G_N_ELEMENTS (bad_headers); i++) {
		debug_printf (1, "  %s\n", bad_headers[i].description);
		expect_warning = TRUE;
		soup_message_headers_append (hdrs, bad_headers[i].name,
					     bad_headers[i].value);
		if (expect_warning) {
			expect_warning = FALSE;
			debug_printf (1, "    FAILED: soup_message_headers_append() did not reject it\n");
			errors++;
		}
	}
	soup_message_headers_free (hdrs);
}

int
main (int argc, char **argv)
{
	test_init (argc, argv, NULL);

	do_request_tests ();
	do_response_tests ();
	do_qvalue_tests ();
	do_rfc2231_tests ();
	do_content_type_tests ();
	do_append_param_tests ();
	do_bad_header_tests ();

	test_cleanup ();
	return errors != 0;
}
