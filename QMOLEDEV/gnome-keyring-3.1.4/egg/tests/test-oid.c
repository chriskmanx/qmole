/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-oid.c: Test OID routines
   Copyright (C) 2008 Stefan Walter

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

#include "egg/egg-oid.h"

#include <glib.h>

static void
test_tests (void)
{
	GQuark oid;

	oid = g_quark_from_static_string ("0.9.2342.19200300.100.1.25");
	g_assert_cmpstr (egg_oid_get_name (oid), ==, "DC");
	g_assert_cmpstr (egg_oid_get_description (oid), ==, "Domain Component");
	g_assert_cmpuint (egg_oid_get_flags (oid), ==, EGG_OID_PRINTABLE);

	/* Should return OID for invalid oids */
	oid = g_quark_from_static_string ("1.1.1.1.1");
	g_assert_cmpstr (egg_oid_get_name (oid), ==, "1.1.1.1.1");
	g_assert_cmpstr (egg_oid_get_description (oid), ==, "1.1.1.1.1");
	g_assert_cmpuint (egg_oid_get_flags (oid), ==, 0);
}

int
main (int argc, char **argv)
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/oid/tests", test_tests);

	return g_test_run ();
}
