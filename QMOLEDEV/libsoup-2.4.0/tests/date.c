/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2005 Novell, Inc.
 */

#include <stdio.h>
#include <string.h>

#include <libsoup/soup-date.h>
#include <glib.h>

#include "test-utils.h"

const char *date_tests[] = {
	/* rfc1123-date, and broken variants */
	"Sun, 06 Nov 2004 08:09:07 GMT",
	"Sun, 6 Nov 2004 08:09:07 GMT",
	"Sun,  6 Nov 2004 08:09:07 GMT",

	/* rfc850-date, and broken variants */
	"Sunday, 06-Nov-04 08:09:07 GMT",
	"Sunday, 6-Nov-04 08:09:07 GMT",
	"Sunday,  6-Nov-04 08:09:07 GMT",
	"Sunday, 06-Nov-104 08:09:07 GMT",

	/* asctime-date, and broken variants */
	"Sun Nov  6 08:09:07 2004",
	"Sun Nov 06 08:09:07 2004",
	"Sun Nov 6 08:09:07 2004",
	"Sun Nov  6 08:09:07 2004 GMT",

	/* ISO 8601 */
	"2004-11-06T08:09:07Z",
	"20041106T08:09:07Z",
	"20041106T08:09:07+00:00",
	"20041106T080907+00:00",

	/* Netscape cookie spec date, and broken variants */
	"Sun, 06-Nov-2004 08:09:07 GMT",
	"Sun, 6-Nov-2004 08:09:07 GMT",
	"Sun,  6-Nov-2004 08:09:07 GMT",

	/* Original version of Netscape cookie spec, and broken variants */
	"Sun, 06-Nov-04 08:09:07 GMT",
	"Sun, 6-Nov-04 08:09:07 GMT",
	"Sun,  6-Nov-04 08:09:07 GMT",
	"Sun, 06-Nov-104 08:09:07 GMT",

	/* Netscape cookie spec example syntax, and broken variants */
	"Sunday, 06-Nov-04 08:09:07 GMT",
	"Sunday, 6-Nov-04 08:09:07 GMT",
	"Sunday,  6-Nov-04 08:09:07 GMT",
	"Sunday, 06-Nov-104 08:09:07 GMT",
	"Sunday, 06-Nov-2004 08:09:07 GMT",
	"Sunday, 6-Nov-2004 08:09:07 GMT",
	"Sunday,  6-Nov-2004 08:09:07 GMT",

	/* Miscellaneous broken formats seen on the web */
	"Sun 06-Nov-2004  08:9:07",
	"Sunday, 06-Nov-04 8:9:07 GMT",
	"Sun, 06 Nov 2004 08:09:7 GMT",
	"Sun, 06-Nov-2004 08:09:07"
};

#define TIME_T 1099728547L
#define TIME_T_STRING "1099728547"

static void
check (const char *strdate, SoupDate *date)
{
	if (date &&
	    date->year == 2004 && date->month == 11 && date->day == 6 &&
	    date->hour == 8 && date->minute == 9 && date->second == 7) {
		soup_date_free (date);
		return;
	}

	fprintf (stderr, "date parsing failed for '%s'.\n", strdate);
	if (date) {
		fprintf (stderr, "  got: %d %d %d - %d %d %d\n\n",
			 date->year, date->month, date->day,
			 date->hour, date->minute, date->second);
		soup_date_free (date);
	}
	errors++;
}

int
main (int argc, char **argv)
{
	int i;

	test_init (argc, argv, NULL);

	for (i = 0; i < G_N_ELEMENTS (date_tests); i++) {
		check (date_tests[i], soup_date_new_from_string (date_tests[i]));
	}
	check (TIME_T_STRING, soup_date_new_from_time_t (TIME_T));

	test_cleanup ();
	return errors != 0;
}
