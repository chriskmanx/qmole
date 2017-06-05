/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-timestamp.c:
 *
 * Copyright (C) 2002-2006 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-timestamp.h>
#include <string.h>
#include <time.h>
#include <stdio.h>
#ifdef G_OS_WIN32
#include <windows.h>
#endif

static void
timestamp_to_string (GValue const *src_value, GValue *dest_value)
{
	char *str = gsf_timestamp_as_string (g_value_get_boxed (src_value));
	g_value_set_string (dest_value, str);
	g_free (str);
}

GType
gsf_timestamp_get_type (void)
{
	static GType our_type = 0;

	if (our_type == 0) {
		our_type = g_boxed_type_register_static ("GsfTimestamp",
					(GBoxedCopyFunc)gsf_timestamp_copy,
					(GBoxedFreeFunc)gsf_timestamp_free);
		g_value_register_transform_func	(our_type, G_TYPE_STRING,
			&timestamp_to_string);
	}
	return our_type;
}

GsfTimestamp *
gsf_timestamp_new (void)
{
	GsfTimestamp *res = g_new0 (GsfTimestamp, 1);
	res->timet = -1;
	return res;
}

/**
 * gsf_timestamp_copy:
 * @stamp: timestamp to be copied
 *
 * Copies a timestamp.
 *
 * Returns: a separate copy of @stamp.
 */
GsfTimestamp *
gsf_timestamp_copy (GsfTimestamp const *stamp)
{
	GsfTimestamp *res = gsf_timestamp_new ();
	res->timet = stamp->timet;
	return res;
}

/**
 * gsf_timestamp_free :
 * @stamp : timestamp to be freed
 *
 * Releases the memory in use for @stamp (if any).
 **/
void
gsf_timestamp_free (GsfTimestamp *stamp)
{
	g_free (stamp);
}

#if defined(HAVE_STRUCT_TM_TM_GMTOFF)
#define GMTOFF(t) ((t).tm_gmtoff)
#elif defined(HAVE_STRUCT_TM___TM_GMTOFF)
#define GMTOFF(t) ((t).__tm_gmtoff)
#elif defined(G_OS_WIN32)
#define GMTOFF(t) (gmt_to_local_win32())
#else
/* FIXME: work out the offset anyway. */
#define GMTOFF(t) (0)
#endif

#ifdef G_OS_WIN32
static time_t gmt_to_local_win32(void)
{
    TIME_ZONE_INFORMATION tzinfo;
    DWORD dwStandardDaylight;
    long bias;

    dwStandardDaylight = GetTimeZoneInformation(&tzinfo);
    bias = tzinfo.Bias;

    if (dwStandardDaylight == TIME_ZONE_ID_STANDARD)
        bias += tzinfo.StandardBias;

    if (dwStandardDaylight == TIME_ZONE_ID_DAYLIGHT)
        bias += tzinfo.DaylightBias;

    return (- bias * 60);
}
#endif

/**
 * gsf_timestamp_from_string :
 * @spec : The string to parse
 * @stamp : #GsfTimestamp
 *
 * Very simple parser for time stamps.  Currently requires a format of
 * 	'YYYY-MM-DDThh:mm:ss'
 * and does no bounds checking.
 *
 * Since: 1.14.12
 *
 * Returns: %TRUE on success
 **/
int
gsf_timestamp_from_string (char const *spec, GsfTimestamp *stamp)
{
	struct tm	tm;

	memset (&tm, 0, sizeof (struct tm));

	/* 'YYYY-MM-DDThh:mm:ss' */
	if (6 == sscanf (spec, "%d-%d-%dT%d:%d:%d",
			 &tm.tm_year, &tm.tm_mon, &tm.tm_mday,
			 &tm.tm_hour, &tm.tm_min, &tm.tm_sec)) {
		time_t t;

		tm.tm_mon--; /* 0..11 */

		/* err on the side of avoiding negatives */
		if (tm.tm_year >= 1900)
			tm.tm_year -= 1900;

		t = mktime (&tm);
		if (t == -1)
			return FALSE;

		stamp->timet = t + GMTOFF(tm);
		return TRUE;
	}
	return FALSE;
}

/**
 * gsf_timestamp_parse :
 * @spec : The string to parse
 * @stamp : #GsfTimestamp
 *
 * Very simple parser for time stamps.  Currently requires a format of
 * 	'YYYY-MM-DDThh:mm:ss'
 * and does no bounds checking.
 *
 * Deprecated : Use gsf_timestamp_from_string
 *
 * Returns: %TRUE on success
 **/
int
gsf_timestamp_parse (char const *spec, GsfTimestamp *stamp)
{
	return gsf_timestamp_from_string (spec, stamp);
}

/**
 * gsf_timestamp_as_string :
 * @stamp: timestamp to be converted.
 *
 * Produce a string representation (ISO 8601 format) of @stamp.
 *
 * Returns: a string representation of @stamp. When @stamp is %NULL, the
 * representation is "&lt;invalid&gt;".
 */
char *
gsf_timestamp_as_string	(GsfTimestamp const *stamp)
{
	time_t    t;
	struct tm tm;

	g_return_val_if_fail (stamp != NULL, g_strdup ("<invalid>"));

	t = stamp->timet;	/* Use an honest time_t for gmtime_r.  */
#ifdef HAVE_GMTIME_R
	gmtime_r (&t, &tm);
#else
	/* -NOT- thread-safe */
	tm = *gmtime (&t);
#endif


	/* using 'YYYY-MM-DDThh:mm:ss' */
	return g_strdup_printf ("%4d-%02d-%02dT%02d:%02d:%02dZ",
		tm.tm_year+1900, tm.tm_mon+1, tm.tm_mday,
		tm.tm_hour, tm.tm_min, tm.tm_sec);
}

guint
gsf_timestamp_hash (GsfTimestamp const *stamp)
{
	return stamp->timet;
}

/**
 * gsf_timestamp_equal :
 * @a: a timestamp
 * @b: another timestamp
 *
 * Compare timestamps @a and @b.
 *
 * Returns: true if @a and @b represent the same point in time; false otherwise.
 *
 **/
gboolean
gsf_timestamp_equal (GsfTimestamp const *a, GsfTimestamp const *b)
{
	return a->timet == b->timet;
}

void
gsf_value_set_timestamp (GValue *value, GsfTimestamp const *stamp)
{
	g_value_set_boxed (value, stamp);
}

void
gsf_timestamp_set_time (GsfTimestamp *stamp, guint64 t)
{
	stamp->timet = t;
}
