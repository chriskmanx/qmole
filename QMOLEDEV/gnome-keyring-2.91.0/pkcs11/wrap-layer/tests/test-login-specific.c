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

#include "test-suite.h"

#include "gkm/gkm-mock.h"
#include "gkm/gkm-test.h"

#include "wrap-layer/gkm-wrap-layer.h"

#include "ui/gku-prompt.h"

static CK_FUNCTION_LIST prompt_login_functions;
static CK_FUNCTION_LIST_PTR module = NULL;
static CK_SESSION_HANDLE session = 0;

DEFINE_SETUP (login_specific)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_OBJECT_HANDLE key;
	CK_SLOT_ID slot_id;
	CK_ULONG n_slots = 1;
	CK_ULONG count;
	CK_RV rv;

	CK_BBOOL always = TRUE;
	CK_ATTRIBUTE attrs[] = {
		{ CKA_ALWAYS_AUTHENTICATE, &always, sizeof (always) }
	};

	CK_MECHANISM mech = { CKM_MOCK_PREFIX, NULL, 0 };

	/* Always start off with test functions */
	rv = gkm_mock_C_GetFunctionList (&funcs);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	memcpy (&prompt_login_functions, funcs, sizeof (prompt_login_functions));

	gkm_wrap_layer_reset_modules ();
	gkm_wrap_layer_add_module (&prompt_login_functions);
	module = gkm_wrap_layer_get_functions ();

	gku_prompt_dummy_prepare_response ();

	/* Open a session */
	rv = (module->C_Initialize) (NULL);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	rv = (module->C_GetSlotList) (CK_TRUE, &slot_id, &n_slots);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	rv = (module->C_OpenSession) (slot_id, CKF_SERIAL_SESSION, NULL, NULL, &session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	/* Find the always authenticate object */
	rv = (module->C_FindObjectsInit) (session, attrs, 1);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	rv = (module->C_FindObjects) (session, &key, 1, &count);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	gkm_assert_cmpulong (count, ==, 1);
	gkm_assert_cmpulong (key, !=, 0);

	rv = (module->C_FindObjectsFinal) (session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	/* Start a signing operation, that needs to be authenticated */
	rv = (module->C_SignInit) (session, &mech, key);
	gkm_assert_cmprv (rv, ==, CKR_OK);
}

DEFINE_TEARDOWN (login_specific)
{
	CK_RV rv;

	g_assert (!gku_prompt_dummy_have_response ());

	rv = (module->C_CloseSession) (session);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	session = 0;

	rv = (module->C_Finalize) (NULL);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	module = NULL;
}

DEFINE_TEST (login_specific_ok_password)
{
	CK_RV rv;

	gku_prompt_dummy_queue_ok_password ("booo");

	rv = (module->C_Login) (session, CKU_CONTEXT_SPECIFIC, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);
}

DEFINE_TEST (login_specific_bad_password_then_cancel)
{
	CK_RV rv;

	gku_prompt_dummy_queue_ok_password ("bad password");
	gku_prompt_dummy_queue_no ();

	rv = (module->C_Login) (session, CKU_CONTEXT_SPECIFIC, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_PIN_INCORRECT);
}

DEFINE_TEST (login_specific_cancel_immediately)
{
	CK_RV rv;

	gku_prompt_dummy_queue_no ();

	rv = (module->C_Login) (session, CKU_CONTEXT_SPECIFIC, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_PIN_INCORRECT);
}
