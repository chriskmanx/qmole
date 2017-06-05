/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2005 Novell, Inc.
 */

#include <stdio.h>
#include <string.h>

#include <libsoup/soup-date.h>
#include <glib.h>

#include "test-utils.h"

static gboolean check_ok (const char *strdate, SoupDate *date);

static SoupDate *
make_date (const char *strdate)
{
	char *dup;
	SoupDate *date;

	/* We do it this way so that if soup_date_new_from_string()
	 * reads off the end of the string, it will trigger an error
	 * when valgrinding, rather than just reading the start of the
	 * next const string.
	 */
	dup = g_strdup (strdate);
	date = soup_date_new_from_string (dup);
	g_free (dup);
	return date;
}

static const struct {
	SoupDateFormat format;
	const char *date;
} good_dates[] = {
	{ SOUP_DATE_HTTP,            "Sat, 06 Nov 2004 08:09:07 GMT" },
	{ SOUP_DATE_COOKIE,          "Sat, 06-Nov-2004 08:09:07 GMT" },
	{ SOUP_DATE_RFC2822,         "Sat, 6 Nov 2004 08:09:07 -0430" },
	{ SOUP_DATE_ISO8601_COMPACT, "20041106T080907" },
	{ SOUP_DATE_ISO8601_FULL,    "2004-11-06T08:09:07" },
	{ SOUP_DATE_ISO8601_XMLRPC,  "20041106T08:09:07" }
};

static void
check_good (SoupDateFormat format, const char *strdate)
{
	SoupDate *date;
	char *strdate2;

	date = make_date (strdate);
	if (date)
		strdate2 = soup_date_to_string (date, format);
	if (!check_ok (strdate, date))
		return;

	if (strcmp (strdate, strdate2) != 0) {
		debug_printf (1, "  restringification failed: '%s' -> '%s'\n",
			      strdate, strdate2);
		errors++;
	}
	g_free (strdate2);
}

static const char *ok_dates[] = {
	/* rfc1123-date, and broken variants */
	"Sat, 06 Nov 2004 08:09:07 GMT",
	"Sat, 6 Nov 2004 08:09:07 GMT",
	"Sat,  6 Nov 2004 08:09:07 GMT",
	"Sat, 06 Nov 2004 08:09:07",
	"06 Nov 2004 08:09:07 GMT",

	/* rfc850-date, and broken variants */
	"Saturday, 06-Nov-04 08:09:07 GMT",
	"Saturday, 6-Nov-04 08:09:07 GMT",
	"Saturday,  6-Nov-04 08:09:07 GMT",
	"Saturday, 06-Nov-104 08:09:07 GMT",
	"Saturday, 06-Nov-04 08:09:07",
	"06-Nov-04 08:09:07 GMT",

	/* asctime-date, and broken variants */
	"Sat Nov  6 08:09:07 2004",
	"Sat Nov 06 08:09:07 2004",
	"Sat Nov 6 08:09:07 2004",
	"Sat Nov  6 08:09:07 2004 GMT",

	/* ISO 8601 */
	"2004-11-06T08:09:07Z",
	"20041106T08:09:07Z",
	"20041106T08:09:07+00:00",
	"20041106T080907+00:00",

	/* Netscape cookie spec date, and broken variants */
	"Sat, 06-Nov-2004 08:09:07 GMT",
	"Sat, 6-Nov-2004 08:09:07 GMT",
	"Sat,  6-Nov-2004 08:09:07 GMT",
	"Sat, 06-Nov-2004 08:09:07",

	/* Original version of Netscape cookie spec, and broken variants */
	"Sat, 06-Nov-04 08:09:07 GMT",
	"Sat, 6-Nov-04 08:09:07 GMT",
	"Sat,  6-Nov-04 08:09:07 GMT",
	"Sat, 06-Nov-104 08:09:07 GMT",
	"Sat, 06-Nov-04 08:09:07",

	/* Netscape cookie spec example syntax, and broken variants */
	"Saturday, 06-Nov-04 08:09:07 GMT",
	"Saturday, 6-Nov-04 08:09:07 GMT",
	"Saturday,  6-Nov-04 08:09:07 GMT",
	"Saturday, 06-Nov-104 08:09:07 GMT",
	"Saturday, 06-Nov-2004 08:09:07 GMT",
	"Saturday, 6-Nov-2004 08:09:07 GMT",
	"Saturday,  6-Nov-2004 08:09:07 GMT",
	"Saturday, 06-Nov-04 08:09:07",

	/* Miscellaneous broken formats seen on the web */
	"Sat 06-Nov-2004  08:9:07",
	"Saturday, 06-Nov-04 8:9:07 GMT",
	"Sat, 06 Nov 2004 08:09:7 GMT"
};

#define TIME_T 1099728547L
#define TIME_T_STRING "1099728547"

static gboolean
check_ok (const char *strdate, SoupDate *date)
{
	debug_printf (2, "%s\n", strdate);

	if (date &&
	    date->year == 2004 && date->month == 11 && date->day == 6 &&
	    date->hour == 8 && date->minute == 9 && date->second == 7) {
		soup_date_free (date);
		return TRUE;
	}

	debug_printf (1, "  date parsing failed for '%s'.\n", strdate);
	if (date) {
		debug_printf (1, "    got: %d %d %d - %d %d %d\n\n",
			      date->year, date->month, date->day,
			      date->hour, date->minute, date->second);
		soup_date_free (date);
	}
	errors++;
	return FALSE;
}

static const char *bad_dates[] = {
	/* broken rfc1123-date */
	", 06 Nov 2004 08:09:07 GMT",
	"Sat, Nov 2004 08:09:07 GMT",
	"Sat, 06 2004 08:09:07 GMT",
	"Sat, 06 Nov 08:09:07 GMT",
	"Sat, 06 Nov 2004 :09:07 GMT",
	"Sat, 06 Nov 2004 09:07 GMT",
	"Sat, 06 Nov 2004 08::07 GMT",
	"Sat, 06 Nov 2004 08:09: GMT",

	/* broken rfc850-date */
	", 06-Nov-04 08:09:07 GMT",
	"Saturday, -Nov-04 08:09:07 GMT",
	"Saturday, Nov-04 08:09:07 GMT",
	"Saturday, 06-04 08:09:07 GMT",
	"Saturday, 06--04 08:09:07 GMT",
	"Saturday, 06-Nov- 08:09:07 GMT",
	"Saturday, 06-Nov 08:09:07 GMT",
	"Saturday, 06-Nov-04 :09:07 GMT",
	"Saturday, 06-Nov-04 09:07 GMT",
	"Saturday, 06-Nov-04 08::07 GMT",
	"Saturday, 06-Nov-04 08:09: GMT",

	/* broken asctime-date */
	"Nov  6 08:09:07 2004",
	"Sat  6 08:09:07 2004",
	"Sat Nov 08:09:07 2004",
	"Sat Nov  6 :09:07 2004",
	"Sat Nov  6 09:07 2004",
	"Sat Nov  6 08::07 2004",
	"Sat Nov  6 08:09: 2004",
	"Sat Nov  6 08:09:07",
	"Sat Nov  6 08:09:07 GMT 2004"
};

static void
check_bad (const char *strdate, SoupDate *date)
{
	debug_printf (2, "%s\n", strdate);

	if (!date)
		return;
	errors++;

	debug_printf (1, "  date parsing succeeded for '%s'!\n", strdate);
	debug_printf (1, "    got: %d %d %d - %d %d %d\n\n",
		      date->year, date->month, date->day,
		      date->hour, date->minute, date->second);
	soup_date_free (date);
}

static const struct conversion {
	const char *source;
	const char *http, *cookie, *rfc2822, *compact, *full, *xmlrpc;
} conversions[] = {
	/* SOUP_DATE_HTTP */
	{ "Sat, 06 Nov 2004 08:09:07 GMT",

	  "Sat, 06 Nov 2004 08:09:07 GMT",
	  "Sat, 06-Nov-2004 08:09:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 +0000",
	  "20041106T080907Z",
	  "2004-11-06T08:09:07Z",
	  "20041106T08:09:07" },

	/* RFC2822 GMT */
	{ "Sat, 6 Nov 2004 08:09:07 +0000",

	  "Sat, 06 Nov 2004 08:09:07 GMT",
	  "Sat, 06-Nov-2004 08:09:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 +0000",
	  "20041106T080907Z",
	  "2004-11-06T08:09:07Z",
	  "20041106T08:09:07" },

	/* RFC2822 with positive offset */
	{ "Sat, 6 Nov 2004 08:09:07 +0430",

	  "Sat, 06 Nov 2004 04:39:07 GMT",
	  "Sat, 06-Nov-2004 04:39:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 +0430",
	  "20041106T080907+0430",
	  "2004-11-06T08:09:07+04:30",
	  "20041106T08:09:07" },

	/* RFC2822 with negative offset */
	{ "Sat, 6 Nov 2004 08:09:07 -0430",

	  "Sat, 06 Nov 2004 12:39:07 GMT",
	  "Sat, 06-Nov-2004 12:39:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 -0430",
	  "20041106T080907-0430",
	  "2004-11-06T08:09:07-04:30",
	  "20041106T08:09:07" },

	/* RFC2822 floating */
	{ "Sat, 6 Nov 2004 08:09:07 -0000",

	  "Sat, 06 Nov 2004 08:09:07 GMT",
	  "Sat, 06-Nov-2004 08:09:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 -0000",
	  "20041106T080907",
	  "2004-11-06T08:09:07",
	  "20041106T08:09:07" },

	/* ISO GMT */
	{ "2004-11-06T08:09:07Z",

	  "Sat, 06 Nov 2004 08:09:07 GMT",
	  "Sat, 06-Nov-2004 08:09:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 +0000",
	  "20041106T080907Z",
	  "2004-11-06T08:09:07Z",
	  "20041106T08:09:07" },

	/* ISO with positive offset */
	{ "2004-11-06T08:09:07+04:30",

	  "Sat, 06 Nov 2004 04:39:07 GMT",
	  "Sat, 06-Nov-2004 04:39:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 +0430",
	  "20041106T080907+0430",
	  "2004-11-06T08:09:07+04:30",
	  "20041106T08:09:07" },

	/* ISO with negative offset */
	{ "2004-11-06T08:09:07-04:30",

	  "Sat, 06 Nov 2004 12:39:07 GMT",
	  "Sat, 06-Nov-2004 12:39:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 -0430",
	  "20041106T080907-0430",
	  "2004-11-06T08:09:07-04:30",
	  "20041106T08:09:07" },

	/* ISO floating */
	{ "2004-11-06T08:09:07",

	  "Sat, 06 Nov 2004 08:09:07 GMT",
	  "Sat, 06-Nov-2004 08:09:07 GMT",
	  "Sat, 6 Nov 2004 08:09:07 -0000",
	  "20041106T080907",
	  "2004-11-06T08:09:07",
	  "20041106T08:09:07" }
};

static void
check_conversion (const struct conversion *conv)
{
	SoupDate *date;
	char *str;

	debug_printf (2, "%s\n", conv->source);
	date = make_date (conv->source);
	if (!date) {
		debug_printf (1, "  date parsing failed for '%s'.\n", conv->source);
		errors++;
		return;
	}

	str = soup_date_to_string (date, SOUP_DATE_HTTP);
	if (!str || strcmp (str, conv->http) != 0) {
		debug_printf (1, "  conversion of '%s' to HTTP failed:\n"
			      "    wanted: %s\n    got:    %s\n",
			      conv->source, conv->http, str ? str : "(null)");
		errors++;
	}
	g_free (str);

	str = soup_date_to_string (date, SOUP_DATE_COOKIE);
	if (!str || strcmp (str, conv->cookie) != 0) {
		debug_printf (1, "  conversion of '%s' to COOKIE failed:\n"
			      "    wanted: %s\n    got:    %s\n",
			      conv->source, conv->cookie, str ? str : "(null)");
		errors++;
	}
	g_free (str);

	str = soup_date_to_string (date, SOUP_DATE_RFC2822);
	if (!str || strcmp (str, conv->rfc2822) != 0) {
		debug_printf (1, "  conversion of '%s' to RFC2822 failed:\n"
			      "    wanted: %s\n    got:    %s\n",
			      conv->source, conv->rfc2822, str ? str : "(null)");
		errors++;
	}
	g_free (str);

	str = soup_date_to_string (date, SOUP_DATE_ISO8601_COMPACT);
	if (!str || strcmp (str, conv->compact) != 0) {
		debug_printf (1, "  conversion of '%s' to COMPACT failed:\n"
			      "    wanted: %s\n    got:    %s\n",
			      conv->source, conv->compact, str ? str : "(null)");
		errors++;
	}
	g_free (str);

	str = soup_date_to_string (date, SOUP_DATE_ISO8601_FULL);
	if (!str || strcmp (str, conv->full) != 0) {
		debug_printf (1, "  conversion of '%s' to FULL failed:\n"
			      "    wanted: %s\n    got:    %s\n",
			      conv->source, conv->full, str ? str : "(null)");
		errors++;
	}
	g_free (str);

	str = soup_date_to_string (date, SOUP_DATE_ISO8601_XMLRPC);
	if (!str || strcmp (str, conv->xmlrpc) != 0) {
		debug_printf (1, "  conversion of '%s' to XMLRPC failed:\n"
			      "    wanted: %s\n    got:    %s\n",
			      conv->source, conv->xmlrpc, str ? str : "(null)");
		errors++;
	}
	g_free (str);

	soup_date_free (date);
}

int
main (int argc, char **argv)
{
	int i;

	test_init (argc, argv, NULL);

	debug_printf (1, "Good dates:\n");
	for (i = 0; i < G_N_ELEMENTS (good_dates); i++)
		check_good (good_dates[i].format, good_dates[i].date);

	debug_printf (1, "\nOK dates:\n");
	for (i = 0; i < G_N_ELEMENTS (ok_dates); i++)
		check_ok (ok_dates[i], make_date (ok_dates[i]));
	check_ok (TIME_T_STRING, soup_date_new_from_time_t (TIME_T));

	debug_printf (1, "\nBad dates:\n");
	for (i = 0; i < G_N_ELEMENTS (bad_dates); i++)
		check_bad (bad_dates[i], make_date (bad_dates[i]));

	debug_printf (1, "\nConversions:\n");
	for (i = 0; i < G_N_ELEMENTS (conversions); i++)
		check_conversion (&conversions[i] );

	test_cleanup ();
	return errors != 0;
}
