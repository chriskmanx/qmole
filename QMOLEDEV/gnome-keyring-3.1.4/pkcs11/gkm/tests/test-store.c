/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-store.c: Test general store functionality

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "mock-module.h"

#include "gkm/gkm-store.h"

typedef struct {
	GkmModule *module;
	GkmStore *store;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	test->module = mock_module_initialize_and_enter ();
	test->store = g_object_new (GKM_TYPE_STORE, NULL);
}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->store);
	mock_module_leave_and_finalize ();
}

static void
test_schema (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;

	attr.type = CKA_LABEL;
	attr.pValue = "Label";
	attr.ulValueLen = 5;

	gkm_store_register_schema (test->store, &attr, NULL, 0);
	g_assert (gkm_store_lookup_schema (test->store, CKA_LABEL, NULL));

	/* Not in the schema */
	g_assert (!gkm_store_lookup_schema (test->store, CKA_VALUE, NULL));
}

static void
test_schema_flags (Test *test, gconstpointer unused)
{
	CK_ATTRIBUTE attr;
	guint flags;

	attr.type = CKA_VALUE;
	attr.pValue = NULL;
	attr.ulValueLen = 0;

	gkm_store_register_schema (test->store, &attr, NULL, GKM_STORE_IS_SENSITIVE);
	g_assert (gkm_store_lookup_schema (test->store, CKA_VALUE, &flags));
	g_assert (flags == GKM_STORE_IS_SENSITIVE);
}

/*
 * That's all we can test in the base class of GkmStore without a proper
 * derived class. For more tests see unit-test-memory-store.c and
 * unit-test-file-store.c
 */


int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gkm/store/schema", Test, NULL, setup, test_schema, teardown);
	g_test_add ("/gkm/store/schema_flags", Test, NULL, setup, test_schema_flags, teardown);

	return g_test_run ();
}
