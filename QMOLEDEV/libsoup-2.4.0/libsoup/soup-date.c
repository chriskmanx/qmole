/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-date.c: Date/time handling
 *
 * Copyright (C) 2005, Novell, Inc.
 * Copyright (C) 2007, Red Hat, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <glib.h>

#include "soup-date.h"

/**
 * SoupDate:
 * @year: the year, 1 to 9999
 * @month: the month, 1 to 12
 * @day: day of the month, 1 to 31
 * @hour: hour of the day, 0 to 23
 * @minute: minute, 0 to 59
 * @second: second, 0 to 59 (or up to 61 in the case of leap seconds)
 * @utc: %TRUE if the date is in UTC
 * @offset: offset from UTC

 * A date and time. The date is assumed to be in the (proleptic)
 * Gregorian calendar. The time is in UTC if @utc is %TRUE. Otherwise,
 * the time is a local time, and @offset gives the offset from UTC in
 * minutes (such that adding @offset to the time would give the
 * correct UTC time). If @utc is %FALSE and @offset is 0, then the
 * %SoupDate represents a "floating" time with no associated timezone
 * information.
 **/

/* Do not internationalize */
static const char *const months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

/* Do not internationalize */
static const char *const days[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

static const int days_before[] = {
	0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
};

GType
soup_date_get_type (void)
{
	static volatile gsize type_volatile = 0;

	if (g_once_init_enter (&type_volatile)) {
		GType type = g_boxed_type_register_static (
			g_intern_static_string ("SoupDate"),
			(GBoxedCopyFunc) soup_date_copy,
			(GBoxedFreeFunc) soup_date_free);
		g_once_init_leave (&type_volatile, type);
	}
	return type_volatile;
}

/**
 * soup_date_new:
 * @year: the year (1-9999)
 * @month: the month (1-12)
 * @day: the day of the month (1-31, as appropriate for @month)
 * @hour: the hour (0-23)
 * @minute: the minute (0-59)
 * @second: the second (0-59)
 *
 * Creates a #SoupDate representing the indicated time, UTC.
 *
 * Return value: a new #SoupDate
 **/
SoupDate *
soup_date_new (int year, int month, int day, 
	       int hour, int minute, int second)
{
	SoupDate *date = g_slice_new (SoupDate);

	date->year   = year;
	date->month  = month;
	date->day    = day;
	date->hour   = hour;
	date->minute = minute;
	date->second = second;
	date->utc    = TRUE;
	date->offset = 0;

	return date;
}

/**
 * soup_date_new_from_now:
 * @offset_seconds: offset from current time
 *
 * Creates a #SoupDate representing a time @offset_seconds after the
 * current time (or before it, if @offset_seconds is negative). If
 * offset_seconds is 0, returns the current time.
 *
 * Return value: a new #SoupDate
 **/
SoupDate *
soup_date_new_from_now (int offset_seconds)
{
	return soup_date_new_from_time_t (time (NULL) + offset_seconds);
}

static gboolean
parse_iso8601_date (SoupDate *date, const char *date_string)
{
	gulong val;

	if (strlen (date_string) < 15)
		return FALSE;
	if (date_string[4] == '-' &&
	    date_string[7] == '-' &&
	    date_string[10] == 'T') {
		/* YYYY-MM-DD */
		date->year  = atoi (date_string);
		date->month = atoi (date_string + 5);
		date->day   = atoi (date_string + 8);
		date_string += 11;
	} else if (date_string[8] == 'T') {
		/* YYYYMMDD */
		val = atoi (date_string);
		date->year = val / 10000;
		date->month = (val % 10000) / 100;
		date->day = val % 100;
		date_string += 9;
	} else
		return FALSE;

	if (strlen (date_string) >= 8 &&
	    date_string[2] == ':' && date_string[5] == ':') {
		/* HH:MM:SS */
		date->hour   = atoi (date_string);
		date->minute = atoi (date_string + 3);
		date->second = atoi (date_string + 6);
		date_string += 8;
	} else if (strlen (date_string) >= 6) {
		/* HHMMSS */
		val = strtoul (date_string, (char **)&date_string, 10);
		date->hour   = val / 10000;
		date->minute = (val % 10000) / 100;
		date->second = val % 100;
	} else
		return FALSE;

	if (*date_string == '.')
		strtoul (date_string + 1, (char **)&date_string, 10);

	if (*date_string == 'Z') {
		date_string++;
		date->utc = TRUE;
		date->offset = 0;
	} else if (*date_string == '+' || *date_string == '-') {
		int sign = (*date_string == '+') ? -1 : 1;
		val = strtoul (date_string + 1, (char **)&date_string, 10);
		if (*date_string == ':')
			val = 60 * val + strtoul (date_string + 1, (char **)&date_string, 10);
		else
			val = 60 * (val / 100) + (val % 100);
		date->offset = sign * val;
		date->utc = sign && !val;
	}

	return !*date_string;
}

static inline gboolean
parse_day (SoupDate *date, const char **date_string)
{
	char *end;

	date->day = strtoul (*date_string, &end, 10);
	if (end == (char *)date_string)
		return FALSE;

	while (*end == ' ' || *end == '-')
		end++;
	*date_string = end;
	return TRUE;
}

static inline gboolean
parse_month (SoupDate *date, const char **date_string)
{
	int i;

	for (i = 0; i < G_N_ELEMENTS (months); i++) {
		if (!strncmp (*date_string, months[i], 3)) {
			date->month = i + 1;
			*date_string += 3;
			while (**date_string == ' ' || **date_string == '-')
				(*date_string)++;
			return TRUE;
		}
	}
	return FALSE;
}

static inline gboolean
parse_year (SoupDate *date, const char **date_string)
{
	char *end;

	date->year = strtoul (*date_string, &end, 10);
	if (end == (char *)date_string)
		return FALSE;

	if (end == (char *)*date_string + 2) {
		if (date->year < 70)
			date->year += 2000;
		else
			date->year += 1900;
	} else if (end == (char *)*date_string + 3)
		date->year += 1900;

	while (*end == ' ' || *end == '-')
		end++;
	*date_string = end;
	return TRUE;
}

static inline gboolean
parse_time (SoupDate *date, const char **date_string)
{
	char *p;

	date->hour = strtoul (*date_string, &p, 10);
	if (*p++ != ':')
		return FALSE;
	date->minute = strtoul (p, &p, 10);
	if (*p++ != ':')
		return FALSE;
	date->second = strtoul (p, &p, 10);

	while (*p == ' ')
		p++;
	*date_string = p;
	return TRUE;
}

static inline gboolean
parse_timezone (SoupDate *date, const char **date_string)
{
	if (**date_string == '+' || **date_string == '-') {
		gulong val;
		int sign = (**date_string == '+') ? -1 : 1;
		val = strtoul (*date_string + 1, (char **)date_string, 10);
		if (**date_string != ':')
			return FALSE;
		val = 60 * val + strtoul (*date_string + 1, (char **)date_string, 10);
		date->offset = sign * val;
		date->utc = sign && !val;
	} else if (**date_string == 'Z') {
		date->offset = 0;
		date->utc = TRUE;
		(*date_string)++;
	} else if (!strcmp (*date_string, "GMT") ||
		   !strcmp (*date_string, "UTC")) {
		date->offset = 0;
		date->utc = TRUE;
		(*date_string) += 3;
	} else if (strchr ("ECMP", **date_string) &&
		   ((*date_string)[1] == 'D' || (*date_string)[1] == 'S') &&
		   (*date_string)[2] == 'T') {
		date->offset = -60 * (5 * strcspn ("ECMP", *date_string));
		if ((*date_string)[1] == 'D')
			date->offset += 60;
		date->utc = FALSE;
	} else if (!**date_string) {
		date->utc = FALSE;
		date->offset = 0;
	} else
		return FALSE;
	return TRUE;
}

static gboolean
parse_textual_date (SoupDate *date, const char *date_string)
{
	/* If it starts with a word, it must be a weekday, which we skip */
	while (g_ascii_isalpha (*date_string))
		date_string++;
	if (*date_string == ',')
		date_string++;
	while (g_ascii_isspace (*date_string))
		date_string++;

	/* If there's now another word, this must be an asctime-date */
	if (g_ascii_isalpha (*date_string)) {
		/* (Sun) Nov  6 08:49:37 1994 */
		if (!parse_month (date, &date_string) ||
		    !parse_day (date, &date_string) ||
		    !parse_time (date, &date_string) ||
		    !parse_year (date, &date_string))
			return FALSE;

		/* There shouldn't be a timezone, but check anyway */
		parse_timezone (date, &date_string);
	} else {
		/* Non-asctime date, so some variation of
		 * (Sun,) 06 Nov 1994 08:49:37 GMT
		 */
		if (!parse_day (date, &date_string) ||
		    !parse_month (date, &date_string) ||
		    !parse_year (date, &date_string) ||
		    !parse_time (date, &date_string))
			return FALSE;

		/* This time there *should* be a timezone, but we
		 * survive if there isn't.
		 */
		parse_timezone (date, &date_string);
	}
	return TRUE;
}

static int
days_in_month (int month, int year)
{
	return days_before[month + 1] - days_before[month] +
		(((year % 4 == 0) && month == 2) ? 1 : 0);
}

/**
 * SoupDateFormat:
 * @SOUP_DATE_HTTP: RFC 1123 format, used by the HTTP "Date" header. Eg
 * "Sun, 06 Nov 1994 08:49:37 GMT"
 * @SOUP_DATE_COOKIE: The format for the "Expires" timestamp in the
 * Netscape cookie specification. Eg, "Sun, 06-Nov-1994 08:49:37 GMT".
 * @SOUP_DATE_RFC2822: RFC 2822 format, eg "Sun, 6 Nov 1994 09:49:37 -0100"
 * @SOUP_DATE_ISO8601_COMPACT: ISO 8601 date/time with no optional
 * punctuation. Eg, "19941106T094937-0100".
 * @SOUP_DATE_ISO8601_FULL: ISO 8601 date/time with all optional
 * punctuation. Eg, "1994-11-06T09:49:37-01:00".
 * @SOUP_DATE_ISO8601_XMLRPC: ISO 8601 date/time as used by XML-RPC.
 * Eg, "19941106T09:49:37".
 * @SOUP_DATE_ISO8601: An alias for @SOUP_DATE_ISO8601_FULL.
 *
 * Date formats that soup_date_to_string() can use.
 *
 * @SOUP_DATE_HTTP and @SOUP_DATE_COOKIE always coerce the time to
 * UTC. @SOUP_DATE_ISO8601_XMLRPC uses the time as given, ignoring the
 * offset completely. @SOUP_DATE_RFC2822 and the other ISO 8601
 * variants use the local time, appending the offset information if
 * available.
 *
 * This enum may be extended with more values in future releases.
 **/

/**
 * soup_date_new_from_string:
 * @date_string: the date in some plausible format
 *
 * Parses @date_string and tries to extract a date from it. This
 * recognizes all of the "HTTP-date" formats from RFC 2616, all ISO
 * 8601 formats containing both a time and a date, RFC 2822 dates,
 * and reasonable approximations thereof. (Eg, it is lenient about
 * whitespace, leading "0"s, etc.)
 *
 * Return value: a new #SoupDate
 **/
SoupDate *
soup_date_new_from_string (const char *date_string)
{
	SoupDate *date = g_slice_new (SoupDate);
	gboolean success;

	while (g_ascii_isspace (*date_string))
		date_string++;

	/* If it starts with a digit, it's either an ISO 8601 date, or
	 * an RFC2822 date without the optional weekday; in the later
	 * case, there will be a month name later on, so look for one
	 * of the month-start letters.
	 */
	if (g_ascii_isdigit (*date_string) &&
	    !strpbrk (date_string, "JFMASOND"))
		success = parse_iso8601_date (date, date_string);
	else
		success = parse_textual_date (date, date_string);

	if (!success) {
		g_slice_free (SoupDate, date);
		return NULL;
	}

	if (date->year < 1 || date->year > 9999 ||
	    date->month < 1 || date->month > 12 ||
	    date->day < 1 ||
	    date->day > days_in_month (date->month, date->year) ||
	    date->hour < 0 || date->hour > 23 ||
	    date->minute < 0 || date->minute > 59 ||
	    date->second < 0 || date->second > 59) {
		g_slice_free (SoupDate, date);
		return NULL;
	} else
		return date;
}

/**
 * soup_date_new_from_time_t:
 * @when: a #time_t
 *
 * Creates a #SoupDate corresponding to @when
 *
 * Return value: a new #SoupDate
 **/
SoupDate *
soup_date_new_from_time_t (time_t when)
{
	struct tm tm;

#ifdef HAVE_GMTIME_R
	gmtime_r (&when, &tm);
#else
	tm = *gmtime (&when);
#endif

	return soup_date_new (tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
			      tm.tm_hour, tm.tm_min, tm.tm_sec);
}

static const char *
soup_date_weekday (SoupDate *date)
{
	int day;

	/* Proleptic Gregorian 0001-01-01 was a Monday, which
	 * corresponds to 1 in the days[] array. So we take the
	 * number of days since 0000-12-31, modulo 7.
	 */
	day = (date->year - 1) * 365 + ((date->year - 1) / 4);
	day += days_before[date->month] + date->day;
	if (date->year % 4 == 0 && date->month > 2)
		day++;

	return days[day % 7];
}

/**
 * soup_date_to_string:
 * @date: a #SoupDate
 * @format: the format to generate the date in
 *
 * Converts @date to a string in the format described by @format.
 *
 * Return value: @date as a string
 **/
char *
soup_date_to_string (SoupDate *date, SoupDateFormat format)
{
	/* FIXME: offset, 8601 zones, etc */

	switch (format) {
	case SOUP_DATE_HTTP:
		/* "Sun, 06 Nov 1994 08:49:37 GMT" */
		return g_strdup_printf ("%s, %02d %s %04d %02d:%02d:%02d GMT",
					soup_date_weekday (date), date->day,
					months[date->month - 1],
					date->year, date->hour, date->minute,
					date->second);

	case SOUP_DATE_COOKIE:
		/* "Sun, 06-Nov-1994 08:49:37 GMT" */
		return g_strdup_printf ("%s, %02d-%s-%04d %02d:%02d:%02d GMT",
					soup_date_weekday (date), date->day,
					months[date->month - 1],
					date->year, date->hour, date->minute,
					date->second);

	case SOUP_DATE_ISO8601_COMPACT:
		return g_strdup_printf ("%04d%02d%02dT%02d%02d%02d",
					date->year, date->month, date->day,
					date->hour, date->minute, date->second);
	case SOUP_DATE_ISO8601_FULL:
		return g_strdup_printf ("%04d-%02d-%02dT%02d:%02d:%02d",
					date->year, date->month, date->day,
					date->hour, date->minute, date->second);
	case SOUP_DATE_ISO8601_XMLRPC:
		return g_strdup_printf ("%04d%02d%02dT%02d:%02d:%02d",
					date->year, date->month, date->day,
					date->hour, date->minute, date->second);

	default:
		return NULL;
	}
}

/**
 * soup_date_to_time_t:
 * @date: a #SoupDate
 *
 * Converts @date to a %time_t.
 *
 * If @date is not representable as a %time_t, it will be clamped into
 * range. (In particular, some HTTP cookies have expiration dates
 * after "Y2.038k" (2038-01-19T03:14:07Z).)
 *
 * Return value: @date as a %time_t
 **/
time_t
soup_date_to_time_t (SoupDate *date)
{
	time_t tt;

	/* FIXME: offset, etc */

	if (date->year < 1970)
		return 0;

	/* If the year is later than 2038, we're guaranteed to
	 * overflow a 32-bit time_t. (If it's exactly 2038, we'll
	 * *probably* overflow, but only by a little, and it's easiest
	 * to test that at the end by seeing if the result has turned
	 * negative.)
	 */
	if (sizeof (time_t) == 4 && date->year > 2038)
		return (time_t)0x7fffffff;

	tt = (date->year - 1970) * 365;
	tt += (date->year - 1968) / 4;
	tt += days_before[date->month] + date->day - 1;
	if (date->year % 4 == 0 && date->month <= 2)
		tt--;
	tt = ((((tt * 24) + date->hour) * 60) + date->minute) * 60 + date->second;

	if (sizeof (time_t) == 4 && tt < 0)
		return (time_t)0x7fffffff;
	return tt;
}

/**
 * soup_date_copy:
 * @date: a #SoupDate
 *
 * Copies @date.
 **/
SoupDate *
soup_date_copy (SoupDate *date)
{
	SoupDate *copy = g_slice_new (SoupDate);

	memcpy (copy, date, sizeof (SoupDate));
	return copy;
}

/**
 * soup_date_free:
 * @date: a #SoupDate
 *
 * Frees @date.
 **/
void
soup_date_free (SoupDate *date)
{
	g_slice_free (SoupDate, date);
}
