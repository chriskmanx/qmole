/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* check-module.c: Check PKCS#11 implementation

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

#include "config.h"

#include "gnome2-store/gkm-gnome2-store.h"

#include "egg/egg-secure-memory.h"

#include <glib.h>
#include <glib-object.h>

#include <p11-tests.h>

static int failures = 0;

EGG_SECURE_GLIB_DEFINITIONS ();

static void
on_p11_tests_log (int level, const char *section, const char *message)
{
	if (level == P11_TESTS_NONE) {
		g_message ("%s", message);
	} else if (level != P11_TESTS_FAIL) {
		g_message ("%s: %s", section, message);
	} else {
		g_print ("  /gnome2-store/%s: FAIL: %s\n", section, message);
		++failures;
	}
}

int
main (int argc, const char *argv[])
{
	g_type_init ();

	p11_tests_set_log_func (on_p11_tests_log);
	p11_tests_set_unexpected (1);
	p11_tests_set_verbose (0);
	p11_tests_set_write_session (1);
	p11_tests_load_config (SRCDIR "/p11-tests.conf");

	g_print ("CHECK: check-gnome2-module...\n");
	p11_tests_perform (gkm_gnome2_store_get_functions ());

	g_print ("%s: check-gnome2-module\n", failures ? "FAIL" : "PASS");
	return failures;
}
