#include <config.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "libsoup/soup-uri.h"

#include "test-utils.h"

struct {
	const char *uri_string, *result;
} abs_tests[] = {
	{ "foo:", "foo:" },
	{ "file:/dev/null", "file:/dev/null" },
	{ "file:///dev/null", "file:///dev/null" },
	{ "ftp://user@host/path", "ftp://user@host/path" },
	{ "ftp://user@host:9999/path", "ftp://user@host:9999/path" },
	{ "ftp://user:password@host/path", "ftp://user@host/path" },
	{ "ftp://user:password@host:9999/path", "ftp://user@host:9999/path" },
	{ "ftp://user:password@host", "ftp://user@host" },
	{ "http://us%65r@host", "http://user@host/" },
	{ "http://us%40r@host", "http://us%40r@host/" },
	{ "http://us%3ar@host", "http://us%3Ar@host/" },
	{ "http://us%2fr@host", "http://us%2Fr@host/" },
	{ "http://us%3fr@host", "http://us%3Fr@host/" },
	{ "http://host?query", "http://host/?query" },
	{ "http://host/path?query=http%3A%2F%2Fhost%2Fpath%3Fchildparam%3Dchildvalue&param=value",
	  "http://host/path?query=http%3A%2F%2Fhost%2Fpath%3Fchildparam%3Dchildvalue&param=value" },
	{ "http://control-chars/%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F%7F",
	  "http://control-chars/%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F%7F"},
	{ "http://space/%20",
	  "http://space/%20" },
	{ "http://delims/%3C%3E%23%25%22",
	  "http://delims/%3C%3E%23%25%22" },
	{ "http://unwise-chars/%7B%7D%7C%5C%5E%5B%5D%60",
	  "http://unwise-chars/%7B%7D%7C%5C%5E%5B%5D%60" },
	{ "http://host/path%", NULL },
	{ "http://host/path%%", NULL },
	{ "http://host/path%%%", NULL },
	{ "http://host/path%/x/", NULL },
	{ "http://host/path%0x/", NULL },

	/* From RFC 2732 */
	{ "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html",
	  "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]/index.html" },
	{ "http://[1080:0:0:0:8:800:200C:417A]/index.html",
	  "http://[1080:0:0:0:8:800:200C:417A]/index.html" },
	{ "http://[3ffe:2a00:100:7031::1]",
	  "http://[3ffe:2a00:100:7031::1]/" },
	{ "http://[1080::8:800:200C:417A]/foo",
	  "http://[1080::8:800:200C:417A]/foo" },
	{ "http://[::192.9.5.5]/ipng",
	  "http://[::192.9.5.5]/ipng" },
	{ "http://[::FFFF:129.144.52.38]:80/index.html",
	  "http://[::FFFF:129.144.52.38]/index.html" },
	{ "http://[2010:836B:4179::836B:4179]",
	  "http://[2010:836B:4179::836B:4179]/" }
};
int num_abs_tests = G_N_ELEMENTS(abs_tests);

/* From RFC 3986. */
const char *base = "http://a/b/c/d;p?q";
struct {
	const char *uri_string, *result;
} rel_tests[] = {
	{ "g:h", "g:h" },
	{ "g", "http://a/b/c/g" },
	{ "./g", "http://a/b/c/g" },
	{ "g/", "http://a/b/c/g/" },
	{ "/g", "http://a/g" },
	{ "//g", "http://g/" },
	{ "?y", "http://a/b/c/d;p?y" },
	{ "g?y", "http://a/b/c/g?y" },
	{ "#s", "http://a/b/c/d;p?q#s" },
	{ "g#s", "http://a/b/c/g#s" },
	{ "g?y#s", "http://a/b/c/g?y#s" },
	{ ";x", "http://a/b/c/;x" },
	{ "g;x", "http://a/b/c/g;x" },
	{ "g;x?y#s", "http://a/b/c/g;x?y#s" },
	{ ".", "http://a/b/c/" },
	{ "./", "http://a/b/c/" },
	{ "..", "http://a/b/" },
	{ "../", "http://a/b/" },
	{ "../g", "http://a/b/g" },
	{ "../..", "http://a/" },
	{ "../../", "http://a/" },
	{ "../../g", "http://a/g" },
	{ "", "http://a/b/c/d;p?q" },
	{ "../../../g", "http://a/g" },
	{ "../../../../g", "http://a/g" },
	{ "/./g", "http://a/g" },
	{ "/../g", "http://a/g" },
	{ "g.", "http://a/b/c/g." },
	{ ".g", "http://a/b/c/.g" },
	{ "g..", "http://a/b/c/g.." },
	{ "..g", "http://a/b/c/..g" },
	{ "./../g", "http://a/b/g" },
	{ "./g/.", "http://a/b/c/g/" },
	{ "g/./h", "http://a/b/c/g/h" },
	{ "g/../h", "http://a/b/c/h" },
	{ "g;x=1/./y", "http://a/b/c/g;x=1/y" },
	{ "g;x=1/../y", "http://a/b/c/y" },
	{ "g?y/./x", "http://a/b/c/g?y/./x" },
	{ "g?y/../x", "http://a/b/c/g?y/../x" },
	{ "g#s/./x", "http://a/b/c/g#s/./x" },
	{ "g#s/../x", "http://a/b/c/g#s/../x" },

	/* RFC 3986 notes that some old parsers will parse this as
	 * a relative URL ("http://a/b/c/g"), but it should be
	 * interpreted as absolute. libsoup should parse it
	 * correctly as being absolute, but then reject it since it's
	 * an http URL with no host.
	 */
	{ "http:g", NULL }
};
int num_rel_tests = G_N_ELEMENTS(rel_tests);

struct {
	char *one, *two;
} eq_tests[] = {
	{ "example://a/b/c/%7Bfoo%7D", "eXAMPLE://a/./b/../b/%63/%7bfoo%7d" },
	{ "http://example.com", "http://example.com/" },
	/* From RFC 2616 */
	{ "http://abc.com:80/~smith/home.html", "http://abc.com:80/~smith/home.html" },
	{ "http://abc.com:80/~smith/home.html", "http://ABC.com/%7Esmith/home.html" },
	{ "http://abc.com:80/~smith/home.html", "http://ABC.com:/%7esmith/home.html" },
};
int num_eq_tests = G_N_ELEMENTS(eq_tests);

static gboolean
do_uri (SoupURI *base_uri, const char *base_str,
	const char *in_uri, const char *out_uri)
{
	SoupURI *uri;
	char *uri_string;

	if (base_uri) {
		debug_printf (1, "<%s> + <%s> = <%s>? ", base_str, in_uri,
			      out_uri ? out_uri : "ERR");
		uri = soup_uri_new_with_base (base_uri, in_uri);
	} else {
		debug_printf (1, "<%s> => <%s>? ", in_uri,
			      out_uri ? out_uri : "ERR");
		uri = soup_uri_new (in_uri);
	}

	if (!uri) {
		if (out_uri) {
			debug_printf (1, "ERR\n  Could not parse %s\n", in_uri);
			return FALSE;
		} else {
			debug_printf (1, "OK\n");
			return TRUE;
		}
	}

	uri_string = soup_uri_to_string (uri, FALSE);
	soup_uri_free (uri);

	if (!out_uri) {
		debug_printf (1, "ERR\n  Got %s\n", uri_string);
		return FALSE;
	}

	if (strcmp (uri_string, out_uri) != 0) {
		debug_printf (1, "NO\n  Unparses to <%s>\n", uri_string);
		g_free (uri_string);
		return FALSE;
	}
	g_free (uri_string);

	debug_printf (1, "OK\n");
	return TRUE;
}

int
main (int argc, char **argv)
{
	SoupURI *base_uri, *uri1, *uri2;
	char *uri_string;
	int i;

	test_init (argc, argv, NULL);

	debug_printf (1, "Absolute URI parsing\n");
	for (i = 0; i < num_abs_tests; i++) {
		if (!do_uri (NULL, NULL, abs_tests[i].uri_string,
			     abs_tests[i].result))
			errors++;
	}

	debug_printf (1, "\nRelative URI parsing\n");
	base_uri = soup_uri_new (base);
	if (!base_uri) {
		fprintf (stderr, "Could not parse %s!\n", base);
		exit (1);
	}

	uri_string = soup_uri_to_string (base_uri, FALSE);
	if (strcmp (uri_string, base) != 0) {
		fprintf (stderr, "URI <%s> unparses to <%s>\n",
			 base, uri_string);
		errors++;
	}
	g_free (uri_string);

	for (i = 0; i < num_rel_tests; i++) {
		if (!do_uri (base_uri, base, rel_tests[i].uri_string,
			     rel_tests[i].result))
			errors++;
	}
	soup_uri_free (base_uri);

	debug_printf (1, "\nURI equality testing\n");
	for (i = 0; i < num_eq_tests; i++) {
		uri1 = soup_uri_new (eq_tests[i].one);
		uri2 = soup_uri_new (eq_tests[i].two);
		debug_printf (1, "<%s> == <%s>? ", eq_tests[i].one, eq_tests[i].two);
		if (soup_uri_equal (uri1, uri2))
			debug_printf (1, "OK\n");
		else {
			debug_printf (1, "NO\n");
			debug_printf (1, "%s : %s : %s\n%s : %s : %s\n",
				      uri1->scheme, uri1->host, uri1->path,
				      uri2->scheme, uri2->host, uri2->path);
			errors++;
		}
		soup_uri_free (uri1);
		soup_uri_free (uri2);
	}

	test_cleanup ();
	return errors != 0;
}
