/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-secmem.c: Test low level secure memory allocation functionality

   Copyright (C) 2007 Stefan Walter

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

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "egg/egg-secure-memory.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


EGG_SECURE_GLIB_DEFINITIONS ();

/* Declared in egg-secure-memory.c */
extern int egg_secure_warnings;

/*
 * Each test looks like (on one line):
 *     void unit_test_xxxxx (CuTest* cu)
 *
 * Each setup looks like (on one line):
 *     void unit_setup_xxxxx (void);
 *
 * Each teardown looks like (on one line):
 *     void unit_teardown_xxxxx (void);
 *
 * Tests be run in the order specified here.
 */

static gsize
find_non_zero (gpointer mem, gsize len)
{
	guchar *b, *e;
	gsize sz = 0;
	for (b = (guchar*)mem, e = ((guchar*)mem) + len; b != e; ++b, ++sz) {
		if (*b != 0x00)
			return sz;
	}

	return G_MAXSIZE;
}

static void
test_alloc_free (void)
{
	gpointer p;
	gboolean ret;

	p = egg_secure_alloc_full (512, 0);
	g_assert (p != NULL);
	g_assert_cmpint (G_MAXSIZE, ==, find_non_zero (p, 512));

	memset (p, 0x67, 512);

	ret = egg_secure_check (p);
	g_assert (ret == TRUE);

	egg_secure_free_full (p, 0);
}

static void
test_realloc_across (void)
{
	gpointer p, p2;

	/* Tiny allocation */
	p = egg_secure_realloc_full (NULL, 1088, 0);
	g_assert (p != NULL);
	g_assert_cmpint (G_MAXSIZE, ==, find_non_zero (p, 1088));

	/* Reallocate to a large one, will have to have changed blocks */
	p2 = egg_secure_realloc_full (p, 16200, 0);
	g_assert (p2 != NULL);
	g_assert_cmpint (G_MAXSIZE, ==, find_non_zero (p2, 16200));
}

static void
test_alloc_two (void)
{
	gpointer p, p2;
	gboolean ret;

	p2 = egg_secure_alloc_full (4, 0);
	g_assert (p2 != NULL);
	g_assert_cmpint (G_MAXSIZE, ==, find_non_zero (p2, 4));

	memset (p2, 0x67, 4);

	p = egg_secure_alloc_full (16200, 0);
	g_assert (p != NULL);
	g_assert_cmpint (G_MAXSIZE, ==, find_non_zero (p, 16200));

	memset (p, 0x67, 16200);

	ret = egg_secure_check (p);
	g_assert (ret == TRUE);

	egg_secure_free_full (p2, 0);
	egg_secure_free_full (p, 0);
}

static void
test_realloc (void)
{
	gchar *str = "a test string to see if realloc works properly";
	gpointer p, p2;
	gsize len;

	len = strlen (str) + 1;

	p = egg_secure_realloc_full (NULL, len, 0);
	g_assert (p != NULL);
	g_assert_cmpint (G_MAXSIZE, ==, find_non_zero (p, len));

	strcpy ((gchar*)p, str);

	p2 = egg_secure_realloc_full (p, 512, 0);
	g_assert (p2 != NULL);
	g_assert_cmpint (G_MAXSIZE, ==, find_non_zero (((gchar*)p2) + len, 512 - len));

	g_assert (strcmp (p2, str) == 0);

	p = egg_secure_realloc_full (p2, 0, 0);
	g_assert (p == NULL);
}

static void
test_multialloc (void)
{
	GPtrArray *memory;
	gpointer data;
	gsize size;
	int i, action, index;

	/* A predetermined seed to get a predetermined pattern */
	g_random_set_seed (15);
	memory = g_ptr_array_new ();

	/* Don't print "can't allocate" warnings */
	egg_secure_warnings = 0;

	for (i = 0; TRUE; ++i) {

		/* Determine what we want to do */
		if (memory->len > 0) {
			if (i > 100000) /* Once we've done 100000 alocations start freeing */
				action = 2;
			else
				action = g_random_int_range (0, 3);
		} else {
			action = 0; /* No allocations, so allocate */
		}

		switch (action) {
		case 0: /* Allocate some memory */
			size = g_random_int_range (1, 16384);
			data = egg_secure_alloc (size);
			g_assert (data);
			memset (data, 0xCAFEBABE, size);
			g_ptr_array_add (memory, data);
			break;
		case 1: /* Reallocate some memory */
			index = g_random_int_range (0, memory->len);
			data = g_ptr_array_index (memory, index);
			g_assert (data);
			size = g_random_int_range (1, 16384);
			data = egg_secure_realloc (data, size);
			g_assert (data);
			memset (data, 0xCAFEBABE, size);
			g_ptr_array_index (memory, index) = data;
			break;
		case 2: /* Free some memory */
			index = g_random_int_range (0, memory->len);
			data = g_ptr_array_index (memory, index);
			g_assert (data);
			egg_secure_free (data);
			g_ptr_array_remove_index_fast (memory, index);
			break;
		default:
			g_assert_not_reached ();
		}

		egg_secure_validate ();

		if (i > 100000 && !memory->len)
			break;
	}

	g_assert (memory->len == 0);
	g_ptr_array_free (memory, TRUE);

	egg_secure_warnings = 1;
}

static void
test_clear (void)
{
	gpointer p;

	p = egg_secure_alloc_full (188, 0);
	g_assert (p != NULL);
	memset (p, 0x89, 188);
	g_assert (memchr (p, 0x89, 188) == p);

	egg_secure_clear (p, 188);
	g_assert (memchr (p, 0x89, 188) == NULL);

	egg_secure_free_full (p, 0);
}

static void
test_strclear (void)
{
	gchar *str;

	str = egg_secure_strdup ("secret");
	g_assert (str != NULL);
	g_assert_cmpuint (strlen (str), ==, 6);
	g_assert (strchr (str, 't') == str + 5);

	egg_secure_strclear (str);
	g_assert_cmpuint (strlen (str), ==, 6);
	g_assert (strchr (str, 't') == NULL);

	egg_secure_free_full (str, 0);
}

int
main (int argc, char **argv)
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/secmem/alloc_free", test_alloc_free);
	g_test_add_func ("/secmem/realloc_across", test_realloc_across);
	g_test_add_func ("/secmem/alloc_two", test_alloc_two);
	g_test_add_func ("/secmem/realloc", test_realloc);
	g_test_add_func ("/secmem/multialloc", test_multialloc);
	g_test_add_func ("/secmem/clear", test_clear);
	g_test_add_func ("/secmem/strclear", test_strclear);

	return g_test_run ();
}
