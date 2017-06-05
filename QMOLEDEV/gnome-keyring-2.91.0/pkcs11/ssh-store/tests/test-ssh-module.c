/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-module.c: A test PKCS#11 module implementation

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
#include "test-ssh-module.h"

#include "gkm/gkm-module.h"

#include "ssh-store/gkm-ssh-store.h"

#include "test-suite.h"

static GMutex *mutex = NULL;

GkmModule*  _gkm_ssh_store_get_module_for_testing (void);
GMutex* _gkm_module_get_scary_mutex_that_you_should_not_touch (GkmModule *module);

GkmModule*
test_ssh_module_initialize_and_enter (void)
{
	CK_FUNCTION_LIST_PTR funcs;
	GkmModule *module;
	CK_RV rv;

	funcs = gkm_ssh_store_get_functions ();
	rv = (funcs->C_Initialize) (NULL);
	g_return_val_if_fail (rv == CKR_OK, NULL);

	module = _gkm_ssh_store_get_module_for_testing ();
	g_return_val_if_fail (module, NULL);

	mutex = _gkm_module_get_scary_mutex_that_you_should_not_touch (module);
	test_ssh_module_enter ();

	return module;
}

void
test_ssh_module_leave_and_finalize (void)
{
	CK_FUNCTION_LIST_PTR funcs;
	CK_RV rv;

	test_ssh_module_leave ();

	funcs = gkm_ssh_store_get_functions ();
	rv = (funcs->C_Finalize) (NULL);
	g_return_if_fail (rv == CKR_OK);
}

void
test_ssh_module_leave (void)
{
	g_assert (mutex);
	g_mutex_unlock (mutex);
}

void
test_ssh_module_enter (void)
{
	g_assert (mutex);
	g_mutex_lock (mutex);
}

GkmSession*
test_ssh_module_open_session (gboolean writable)
{
	CK_ULONG flags = CKF_SERIAL_SESSION;
	CK_SESSION_HANDLE handle;
	GkmModule *module;
	GkmSession *session;
	CK_RV rv;

	module = _gkm_ssh_store_get_module_for_testing ();
	g_return_val_if_fail (module, NULL);

	if (writable)
		flags |= CKF_RW_SESSION;

	rv = gkm_module_C_OpenSession (module, 1, flags, NULL, NULL, &handle);
	g_assert (rv == CKR_OK);

	session = gkm_module_lookup_session (module, handle);
	g_assert (session);

	return session;
}

DEFINE_EXTERNAL(ssh_module)
{
	CK_FUNCTION_LIST_PTR funcs = gkm_ssh_store_get_functions ();
	testing_test_p11_module (funcs, "p11-tests.conf");
}
