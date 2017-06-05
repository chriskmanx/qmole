/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-other.c: Test miscellaneous functionality

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

#include "gnome-keyring.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void
test_result_string (void)
{
	const gchar *msg;

	msg = gnome_keyring_result_to_message (GNOME_KEYRING_RESULT_OK);
	/* "should return an empty string" */
	g_assert (msg && !msg[0]);

	msg = gnome_keyring_result_to_message (GNOME_KEYRING_RESULT_CANCELLED);
	/* "should return an empty string" */
	g_assert (msg && !msg[0]);

	msg = gnome_keyring_result_to_message (GNOME_KEYRING_RESULT_DENIED);
	/* "should return an valid message" */
	g_assert (msg && msg[0]);

	msg = gnome_keyring_result_to_message (GNOME_KEYRING_RESULT_NO_KEYRING_DAEMON);
	/* "should return an valid message" */
	g_assert (msg && msg[0]);

	msg = gnome_keyring_result_to_message (GNOME_KEYRING_RESULT_NO_SUCH_KEYRING);
	/* "should return an valid message" */
	g_assert (msg && msg[0]);

	msg = gnome_keyring_result_to_message (GNOME_KEYRING_RESULT_BAD_ARGUMENTS);
	/* "should return an valid message" */
	g_assert (msg && msg[0]);

	msg = gnome_keyring_result_to_message (GNOME_KEYRING_RESULT_IO_ERROR);
	/* "should return an valid message" */
	g_assert (msg && msg[0]);

	msg = gnome_keyring_result_to_message (GNOME_KEYRING_RESULT_KEYRING_ALREADY_EXISTS);
	/* "should return an valid message" */
	g_assert (msg && msg[0]);
}

int
main (int argc, char **argv)
{
	g_test_init (&argc, &argv, NULL);
	g_set_prgname ("test-other");

	g_test_add_func ("/other/result-string", test_result_string);

	return g_test_run ();
}
