/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-timer.c: Test thread timer functionality

   Copyright (C) 2009 Stefan Walter

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

#include "test-suite.h"
#include "test-module.h"

#include "gkm/gkm-timer.h"

static GkmModule *module = NULL;

DEFINE_SETUP(timer_setup)
{
	module = test_module_initialize_and_enter ();
}

DEFINE_TEARDOWN(timer_teardown)
{
	test_module_leave_and_finalize ();
}

DEFINE_TEST(timer_extra_initialize)
{
	gkm_timer_initialize ();
	gkm_timer_shutdown ();
}

static void
timer_callback (GkmTimer *timer, gpointer user_data)
{
	GkmTimer **value = user_data;
	g_assert (timer);
	g_assert (timer == *value);
	*value = NULL;
}

DEFINE_TEST(timer_simple)
{
	GkmTimer *timer;

	timer = gkm_timer_start (module, 2, timer_callback, &timer);

	test_module_leave ();
	testing_wait_until (2200);
	test_module_enter ();

	g_assert (timer == NULL);
}

DEFINE_TEST(timer_cancel)
{
	GkmTimer *timer;

	timer = gkm_timer_start (module, 2, timer_callback, &timer);

	test_module_leave ();
	testing_wait_until (500);
	test_module_enter ();

	gkm_timer_cancel (timer);

	test_module_leave ();
	testing_wait_until (2000);
	test_module_enter ();

	/* The callback should not have been called */
	g_assert (timer != NULL);
}

DEFINE_TEST(timer_immediate)
{
	GkmTimer *timer;

	/* Setup timer in the past, should execute as soon as possible */
	timer = gkm_timer_start (module, -5, timer_callback, &timer);

	/* Should not be called immediately */
	g_assert (timer != NULL);

	test_module_leave ();
	testing_wait_until (50);
	test_module_enter ();

	/* Should have been called now */
	g_assert (timer == NULL);
}

static GkmTimer *timer_last = NULL;
static gint timer_check = 0;

static void
multiple_callback (GkmTimer *timer, gpointer user_data)
{
	gint value = GPOINTER_TO_INT (user_data);
	g_assert (timer);
	g_assert (timer != timer_last);
	g_assert (value == timer_check);
	timer_last = timer;
	timer_check += 1;
}

DEFINE_TEST(timer_multiple)
{
	timer_check = 0;

	/* Multiple timers, add out of order, should be called in order */
	gkm_timer_start (module, 1, multiple_callback, GINT_TO_POINTER (1));
	gkm_timer_start (module, 3, multiple_callback, GINT_TO_POINTER (3));
	gkm_timer_start (module, 2, multiple_callback, GINT_TO_POINTER (2));
	gkm_timer_start (module, 0, multiple_callback, GINT_TO_POINTER (0));

	test_module_leave ();
	testing_wait_until (3500);
	test_module_enter ();

	g_assert (timer_check == 4);
}

DEFINE_TEST(timer_outstanding)
{
	/* A timer that can't be called */
	gkm_timer_start (module, 5, timer_callback, NULL);
	gkm_timer_start (module, 10, timer_callback, NULL);
	gkm_timer_start (module, 1, timer_callback, NULL);
}
