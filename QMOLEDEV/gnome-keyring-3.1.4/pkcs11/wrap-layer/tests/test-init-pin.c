/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "egg/egg-testing.h"

#include "gkm/gkm-mock.h"
#include "gkm/gkm-test.h"

#include "wrap-layer/gkm-wrap-layer.h"

#include "ui/gku-prompt.h"

#include <string.h>

typedef struct {
	CK_FUNCTION_LIST functions;
	CK_FUNCTION_LIST_PTR module;
	CK_SESSION_HANDLE session;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_SLOT_ID slot_id;
	CK_ULONG n_slots = 1;
	CK_RV rv;

	/* Always start off with test test->functions */
	rv = gkm_mock_C_GetFunctionList (&funcs);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	memcpy (&test->functions, funcs, sizeof (test->functions));

	gkm_wrap_layer_reset_modules ();
	gkm_wrap_layer_add_module (&test->functions);
	test->module = gkm_wrap_layer_get_functions ();

	gku_prompt_dummy_prepare_response ();

	/* Open a test->session */
	rv = (test->module->C_Initialize) (NULL);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	rv = (test->module->C_GetSlotList) (CK_TRUE, &slot_id, &n_slots);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	rv = (test->module->C_OpenSession) (slot_id, CKF_SERIAL_SESSION, NULL, NULL, &test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);
}

static void
teardown (Test *test, gconstpointer unused)
{
	CK_RV rv;

	g_assert (!gku_prompt_dummy_have_response ());

	rv = (test->module->C_CloseSession) (test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	test->session = 0;

	rv = (test->module->C_Finalize) (NULL);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	test->module = NULL;
}

static void
test_ok_password (Test *test, gconstpointer unused)
{
	CK_RV rv;

	gku_prompt_dummy_queue_ok_password ("new");

	rv = (test->module->C_InitPIN) (test->session, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	rv = (test->module->C_Login) (test->session, CKU_USER, (guchar*)"new", 3);
	gkm_assert_cmprv (rv, ==, CKR_OK);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/wrap-layer/init-pin/ok_password", Test, NULL, setup, test_ok_password, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
