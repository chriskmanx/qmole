/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-xdg-test->module.c: A test PKCS#11 test->module implementation

   Copyright (C) 2010 Stefan Walter
   Copyright (C) 2010 Collabora Ltd

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

#include "mock-xdg-module.h"

#include "xdg-store/gkm-xdg-store.h"

#include "gkm/gkm-session.h"
#include "gkm/gkm-module.h"

#include "egg/egg-testing.h"

#include <errno.h>
#include <sys/times.h>

#include <string.h>

typedef struct {
	GkmModule *module;
	GkmSession *session;
	CK_SLOT_ID slot_id;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	CK_SESSION_INFO info;
	CK_RV rv;

	test->module = mock_xdg_module_initialize_and_enter ();
	test->session = mock_xdg_module_open_session (TRUE);

	rv = gkm_module_C_Login (test->module, gkm_session_get_handle (test->session), CKU_USER, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	rv = gkm_session_C_GetSessionInfo (test->session, &info);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	test->slot_id = info.slotID;
}

static void
teardown (Test *test, gconstpointer unused)
{
	mock_xdg_module_leave_and_finalize ();
	test->module = NULL;
	test->session = NULL;
}

static void
test_module_find_twice_is_same (Test *test, gconstpointer unused)
{
	CK_OBJECT_HANDLE objects[256];
	CK_ULONG n_objects;
	CK_ULONG n_check;
	CK_RV rv;

	rv = gkm_session_C_FindObjectsInit (test->session, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjects (test->session, objects, G_N_ELEMENTS (objects), &n_objects);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjectsFinal (test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	gkm_assert_cmpulong (n_objects, >, 0);

	/* Update the time on the file */
	mock_xdg_module_touch_file ("test-refer-1.trust", 1);

	rv = gkm_session_C_FindObjectsInit (test->session, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjects (test->session, objects, G_N_ELEMENTS (objects), &n_check);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjectsFinal (test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	/* Should have same objects after reload */
	gkm_assert_cmpulong (n_check, ==, n_objects);
}

static void
test_module_file_becomes_invalid (Test *test, gconstpointer unused)
{
	CK_OBJECT_HANDLE objects[256];
	CK_ULONG n_objects;
	CK_ULONG n_check;
	CK_RV rv;

	rv = gkm_session_C_FindObjectsInit (test->session, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjects (test->session, objects, G_N_ELEMENTS (objects), &n_objects);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjectsFinal (test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	gkm_assert_cmpulong (n_objects, >, 0);

	/* Overwrite the file with empty */
	mock_xdg_module_empty_file ("test-refer-1.trust");
	mock_xdg_module_touch_file ("test-refer-1.trust", 2);

	rv = gkm_session_C_FindObjectsInit (test->session, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjects (test->session, objects, G_N_ELEMENTS (objects), &n_check);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjectsFinal (test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	/* Should have less objects */
	gkm_assert_cmpulong (n_check, <, n_objects);
}

static void
test_module_file_remove (Test *test, gconstpointer unused)
{
	CK_OBJECT_HANDLE objects[256];
	CK_ULONG n_objects;
	CK_ULONG n_check;
	CK_RV rv;

	rv = gkm_session_C_FindObjectsInit (test->session, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjects (test->session, objects, G_N_ELEMENTS (objects), &n_objects);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjectsFinal (test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	gkm_assert_cmpulong (n_objects, >, 0);

	/* This file goes away */
	mock_xdg_module_remove_file ("test-refer-1.trust");

	rv = gkm_session_C_FindObjectsInit (test->session, NULL, 0);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjects (test->session, objects, G_N_ELEMENTS (objects), &n_check);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjectsFinal (test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	/* Should have less objects */
	gkm_assert_cmpulong (n_check, <, n_objects);
}

static void
test_create_and_add_object (Test *test, gconstpointer unused)
{
	CK_OBJECT_HANDLE object = 0;
	CK_OBJECT_CLASS klass = CKO_CERTIFICATE;
	CK_CERTIFICATE_TYPE ctype = CKC_X_509;
	CK_BBOOL tval = CK_TRUE;
	gchar *data;
	gsize n_data;
	CK_RV rv;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_VALUE, NULL, 0 },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_TOKEN, &tval, sizeof (tval) },
		{ CKA_CERTIFICATE_TYPE, &ctype, sizeof (ctype) }
	};

	if (!g_file_get_contents (SRCDIR "/files/test-certificate-2.cer", &data, &n_data, NULL))
		g_assert_not_reached ();

	attrs[0].pValue = data;
	attrs[0].ulValueLen = n_data;

	rv = gkm_session_C_CreateObject (test->session, attrs, G_N_ELEMENTS (attrs), &object);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	gkm_assert_cmpulong (object, !=, 0);
}

static void
test_destroy_object (Test *test, gconstpointer unused)
{
	CK_OBJECT_HANDLE object = 0;
	CK_CERTIFICATE_TYPE ctype = CKC_X_509;
	CK_ULONG n_objects = 0;
	CK_BBOOL tval = CK_TRUE;
	CK_RV rv;

	CK_ATTRIBUTE attrs[] = {
		{ CKA_CERTIFICATE_TYPE, &ctype, sizeof (ctype) },
		{ CKA_TOKEN, &tval, sizeof (tval) }
	};

	rv = gkm_session_C_FindObjectsInit (test->session, attrs, G_N_ELEMENTS (attrs));
	gkm_assert_cmprv (rv, ==, CKR_OK);
	rv = gkm_session_C_FindObjects (test->session, &object, 1, &n_objects);
	gkm_assert_cmprv (rv, ==, CKR_OK);
	gkm_assert_cmpulong (n_objects, ==, 1);
	rv = gkm_session_C_FindObjectsFinal (test->session);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	/* Destroy this object, which should be stored on the disk */
	rv = gkm_session_C_DestroyObject (test->session, object);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	/* Make sure it's really gone */
	rv = gkm_session_C_DestroyObject (test->session, object);
	gkm_assert_cmprv (rv, ==, CKR_OBJECT_HANDLE_INVALID);
}

static void
test_get_slot_info (Test *test, gconstpointer unused)
{
	CK_SLOT_INFO info;
	const gchar *str;
	CK_RV rv;

	rv = gkm_module_C_GetSlotInfo (test->module, test->slot_id, &info);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	str = g_strstr_len ((gchar*)info.slotDescription, sizeof (info.slotDescription),
	                    "User Key Storage");
	g_assert (str != NULL);
}

static void
test_get_token_info (Test *test, gconstpointer unused)
{
	CK_TOKEN_INFO info;
	const gchar *str;
	CK_RV rv;

	rv = gkm_module_C_GetTokenInfo (test->module, test->slot_id, &info);
	gkm_assert_cmprv (rv, ==, CKR_OK);

	str = g_strstr_len ((gchar*)info.label, sizeof (info.label),
	                    "User Key Storage");
	g_assert (str != NULL);
}


static void
null_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
                  const gchar *message, gpointer user_data)
{

}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	/* Suppress these messages in tests */
	g_log_set_handler (G_LOG_DOMAIN, G_LOG_LEVEL_MESSAGE | G_LOG_LEVEL_INFO | G_LOG_LEVEL_DEBUG,
	                   null_log_handler, NULL);

	g_test_add ("/xdg-store/module/module_find_twice_is_same", Test, NULL, setup, test_module_find_twice_is_same, teardown);
	g_test_add ("/xdg-store/module/module_file_becomes_invalid", Test, NULL, setup, test_module_file_becomes_invalid, teardown);
	g_test_add ("/xdg-store/module/module_file_remove", Test, NULL, setup, test_module_file_remove, teardown);
	g_test_add ("/xdg-store/module/create_and_add_object", Test, NULL, setup, test_create_and_add_object, teardown);
	g_test_add ("/xdg-store/module/destroy_object", Test, NULL, setup, test_destroy_object, teardown);
	g_test_add ("/xdg-store/module/get_slot_info", Test, NULL, setup, test_get_slot_info, teardown);
	g_test_add ("/xdg-store/module/get_token_info", Test, NULL, setup, test_get_token_info, teardown);

	return egg_tests_run_in_thread_with_loop ();
}
