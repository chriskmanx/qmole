/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/*
   Copyright (C) 2011 Collabora Ltd

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

#include "gcr/gcr-memory-icon.h"

#include "egg/egg-testing.h"

#include <glib.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
	GIcon *icon;
	GAsyncResult *result;
} Test;

static const guint8 test_data[256] = {
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
	16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
	32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
	48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
	64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
	80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
	96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
	112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
	128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
	144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
	160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
	176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
	192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
	208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
	224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
	240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255,
};

static void
setup (Test *test, gconstpointer unused)
{
	test->icon = _gcr_memory_icon_new ("application/octet-stream",
	                                   test_data, sizeof (test_data));
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->icon);
	if (test->result)
		g_object_unref (test->result);
}

static void
test_equal_same (Test *test, gconstpointer unused)
{
	g_assert (g_icon_equal (test->icon, test->icon) == TRUE);
}

static void
test_equal_not (Test *test, gconstpointer unused)
{
	GIcon *icon;

	icon = g_themed_icon_new ("folder");
	g_assert (g_icon_equal (test->icon, icon) == FALSE);
	g_object_unref (icon);
}

static void
test_equal_data (Test *test, gconstpointer unused)
{
	GIcon *icon;

	icon = _gcr_memory_icon_new ("application/octet-stream",
	                             test_data, sizeof (test_data));
	g_assert (g_icon_equal (test->icon, icon) == TRUE);
	g_assert_cmpuint (g_icon_hash (test->icon), ==, g_icon_hash (icon));
	g_object_unref (icon);
}

static void
test_different_type (Test *test, gconstpointer unused)
{
	GIcon *icon;

	icon = _gcr_memory_icon_new ("application/x-other",
	                             test_data, sizeof (test_data));
	g_assert (g_icon_equal (test->icon, icon) == FALSE);
	g_assert_cmpuint (g_icon_hash (test->icon), !=, g_icon_hash (icon));
	g_object_unref (icon);
}

static void
test_different_offset (Test *test, gconstpointer unused)
{
	GIcon *icon;

	icon = _gcr_memory_icon_new_full ("application/octet-stream", (gpointer)test_data,
	                                  sizeof (test_data) - 16, 16, NULL);
	g_assert (g_icon_equal (test->icon, icon) == FALSE);
	g_assert_cmpuint (g_icon_hash (test->icon), !=, g_icon_hash (icon));
	g_object_unref (icon);
}

static void
test_load_sync (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	GInputStream *is;
	gchar buf[1024];
	gsize length;
	gchar *type;

	is = g_loadable_icon_load (G_LOADABLE_ICON (test->icon), 0, &type, NULL, &error);
	g_assert (is != NULL);
	g_assert_no_error (error);
	g_assert_cmpstr (type, ==, "application/octet-stream");

	if (!g_input_stream_read_all (is, buf, sizeof (buf), &length, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);
	egg_assert_cmpsize (length, ==, sizeof (test_data));
	egg_assert_cmpmem (buf, length, ==, test_data, sizeof (test_data));

	g_free (type);
	g_object_unref (is);
}

static void
on_async_ready (GObject *source, GAsyncResult *result, gpointer user_data)
{
	Test *test = user_data;

	g_assert (G_OBJECT (test->icon) == source);
	g_assert (test->result == NULL);
	g_assert (g_async_result_get_source_object (result) == source);

	test->result = g_object_ref (result);
	egg_test_wait_stop ();
}

static void
test_load_async (Test *test, gconstpointer unused)
{
	GError *error = NULL;
	GInputStream *is;
	gchar buf[1024];
	gsize length;
	gchar *type;

	g_loadable_icon_load_async (G_LOADABLE_ICON (test->icon), 0, NULL, on_async_ready, test);
	egg_test_wait_until (500);

	g_assert (test->result);
	is = g_loadable_icon_load_finish (G_LOADABLE_ICON (test->icon), test->result, &type, &error);
	g_assert (is != NULL);
	g_assert_no_error (error);
	g_assert_cmpstr (type, ==, "application/octet-stream");

	if (!g_input_stream_read_all (is, buf, sizeof (buf), &length, NULL, &error))
		g_assert_not_reached ();
	g_assert_no_error (error);
	egg_assert_cmpsize (length, ==, sizeof (test_data));
	egg_assert_cmpmem (buf, length, ==, test_data, sizeof (test_data));

	g_free (type);
	g_object_unref (is);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gcr/memory-icon/equal_same", Test, NULL, setup, test_equal_same, teardown);
	g_test_add ("/gcr/memory-icon/equal_not", Test, NULL, setup, test_equal_not, teardown);
	g_test_add ("/gcr/memory-icon/equal_data", Test, NULL, setup, test_equal_data, teardown);
	g_test_add ("/gcr/memory-icon/different_type", Test, NULL, setup, test_different_type, teardown);
	g_test_add ("/gcr/memory-icon/different_offset", Test, NULL, setup, test_different_offset, teardown);
	g_test_add ("/gcr/memory-icon/load_sync", Test, NULL, setup, test_load_sync, teardown);
	g_test_add ("/gcr/memory-icon/load_async", Test, NULL, setup, test_load_async, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
