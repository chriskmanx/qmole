/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
   Copyright (C) 2011 Collabora Ltd.

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stefw@collabora.co.uk>
*/

#include "config.h"

#include "gcr/gcr.h"
#include "gcr/gcr-util.h"

#include <errno.h>

static void
on_line_parsed_match_template (const gchar *line, gpointer user_data)
{
	const gchar ***matching = user_data;

	g_assert (matching);
	g_assert (*matching);

	/* Must be another line to match */
	g_assert ((*matching)[0]);

	/* Match this line against expected, and increment to next */
	g_assert_cmpstr ((*matching)[0], ==, line);
	(*matching)++;
}

static void
test_parse_lines (void)
{
	GString *string = g_string_new ("first line\nsecond line\n\nlast line");
	const gchar *matches[] = { "first line", "second line", "", NULL };
	const gchar **matching = matches;

	_gcr_util_parse_lines (string, FALSE, on_line_parsed_match_template, &matching);

	/* All lines should have matched */
	g_assert (*matching == NULL);

	/* The last line should still be here */
	g_assert_cmpstr (string->str, ==, "last line");
	g_string_free (string, TRUE);
}

static void
test_parse_lines_and_last (void)
{
	GString *string = g_string_new ("first line\nsecond line\n\nlast line");
	const gchar *matches[] = { "first line", "second line", "", "last line", NULL };
	const gchar **matching = matches;

	_gcr_util_parse_lines (string, FALSE, on_line_parsed_match_template, &matching);
	_gcr_util_parse_lines (string, TRUE, on_line_parsed_match_template, &matching);

	/* All lines should have matched */
	g_assert (*matching == NULL);

	/* No more data */
	g_assert_cmpstr (string->str, ==, "");
	g_assert_cmpuint (string->len, ==, 0);
	g_string_free (string, TRUE);
}

static void
test_parse_lines_dos (void)
{
	GString *string = g_string_new ("first line\r\nsecond line\r\n\r\nlast line");
	const gchar *matches[] = { "first line", "second line", "", "last line", NULL };
	const gchar **matching = matches;

	_gcr_util_parse_lines (string, FALSE, on_line_parsed_match_template, &matching);
	_gcr_util_parse_lines (string, TRUE, on_line_parsed_match_template, &matching);

	/* All lines should have matched */
	g_assert (*matching == NULL);

	/* No more data */
	g_assert_cmpstr (string->str, ==, "");
	g_assert_cmpuint (string->len, ==, 0);
	g_string_free (string, TRUE);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/gcr/util/test_parse_lines", test_parse_lines);
	g_test_add_func ("/gcr/util/test_parse_lines_and_last", test_parse_lines_and_last);
	g_test_add_func ("/gcr/util/test_parse_lines_dos", test_parse_lines_dos);

	return g_test_run ();
}
