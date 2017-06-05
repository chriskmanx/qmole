/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-secret-util.c: Test secret utils

   Copyright (C) 2010 Stefan Walter

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

#include "test-suite.h"

#include "dbus/gkd-secret-util.h"

#include <glib.h>

DEFINE_TEST(secret_util_build_path)
{
	const gchar *identifier = "par_d\xc3\xa9""faut";
	gchar *result;

	result = gkd_secret_util_build_path ("/path/", identifier, strlen (identifier));
	g_assert (result);

	g_assert_cmpstr (result, ==, "/path/par_5fd_c3_a9faut");
	g_free (result);
}
