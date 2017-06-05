/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-cleanup.c: Test low level cleanup functionality

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "egg/egg-cleanup.h"

#define DATA "some string"

typedef struct _CleanupParam {
	gpointer value;
} CleanupParam;

static void
cleanup_callback (gpointer user_data)
{
	CleanupParam *param = (CleanupParam*)user_data;
	g_assert (param->value && strcmp(param->value, DATA) == 0);
	param->value = NULL;
}

static void
test_cleanup (void)
{
	CleanupParam param;

	param.value = DATA;

	egg_cleanup_register (cleanup_callback, &param);

	egg_cleanup_perform ();

	g_assert (param.value == NULL);
}

/* -----------------------------------------------------------------------------
 * Cleanup handlers are called in the opposite order as installed
 */

static gint order_value = 0;

typedef struct _OrderParam {
	gint reference;
} OrderParam;

static void
order_callback (gpointer user_data)
{
	OrderParam *param = (OrderParam*)user_data;
	/* cleanup handler called out of order */
	g_assert_cmpint (order_value, ==, param->reference);
	param->reference = -1;
	--order_value;
}

static void
test_order (void)
{
	OrderParam param[8];
	int i;

	for (i = 0; i < 8; ++i) {
		param[i].reference = i;
		egg_cleanup_register (order_callback, &param[i]);
	}

	order_value = i - 1;

	egg_cleanup_perform ();

	for (i = 0; i < 8; ++i)
		/* "cleanup handler not called" */
		g_assert (param[i].reference == -1);

	/* "not all cleanup handlers called" */
	g_assert_cmpint (order_value, ==, -1);
}

/* -----------------------------------------------------------------------------
 * A cleanup handler might cause another to be registered.
 */

static gboolean cleaned_up = FALSE;

static void
second_callback (gpointer user_data)
{
	cleaned_up = TRUE;
}

static void
reregister_callback (gpointer user_data)
{
	egg_cleanup_register (second_callback, NULL);
}

static void
test_reregister (void)
{
	cleaned_up = FALSE;

	egg_cleanup_register (reregister_callback, NULL);

	egg_cleanup_perform ();

	/* "second cleanup handler not called" */
	g_assert (cleaned_up == TRUE);
}

/* -----------------------------------------------------------------------------
 * Cleanup handlers can be removed
 */

static gboolean test_cleaned_up = FALSE;

static void
remove_callback (gpointer user_data)
{
	test_cleaned_up = TRUE;
}

static void
test_remove (void)
{
	egg_cleanup_register (remove_callback, NULL);
	egg_cleanup_register (remove_callback, DATA);
	egg_cleanup_unregister (remove_callback, DATA);
	egg_cleanup_unregister (remove_callback, NULL);
	egg_cleanup_perform ();

	/* "removed callback was called" */
	g_assert (test_cleaned_up == FALSE);
}

int
main (int argc, char **argv)
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/cleanup/cleanup", test_cleanup);
	g_test_add_func ("/cleanup/order", test_order);
	g_test_add_func ("/cleanup/reregister", test_reregister);
	g_test_add_func ("/cleanup/remove", test_remove);

	return g_test_run ();
}
