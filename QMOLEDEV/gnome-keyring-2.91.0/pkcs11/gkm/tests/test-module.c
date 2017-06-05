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
#include "test-module.h"
#include "test-suite.h"

/* Include all the module entry points */
#include "gkm/gkm-module-ep.h"
GKM_DEFINE_MODULE (test_module, GKM_TYPE_MODULE);

#include "gkm/gkm-certificate.h"

GkmModule*
test_module_initialize_and_enter (void)
{
	CK_RV rv;

	gkm_crypto_initialize ();
	rv = test_module_function_list->C_Initialize (NULL);
	g_return_val_if_fail (rv == CKR_OK, NULL);

	g_return_val_if_fail (pkcs11_module, NULL);

	test_module_enter ();
	return pkcs11_module;
}

void
test_module_leave_and_finalize (void)
{
	CK_RV rv;

	test_module_leave ();
	rv = test_module_function_list->C_Finalize (NULL);
	g_return_if_fail (rv == CKR_OK);
}

void
test_module_leave (void)
{
	g_static_mutex_unlock (&pkcs11_module_mutex);
}

void
test_module_enter (void)
{
	g_static_mutex_lock (&pkcs11_module_mutex);
}

GkmSession*
test_module_open_session (gboolean writable)
{
	CK_ULONG flags = CKF_SERIAL_SESSION;
	CK_SESSION_HANDLE handle;
	GkmSession *session;
	CK_RV rv;

	if (writable)
		flags |= CKF_RW_SESSION;

	rv = gkm_module_C_OpenSession (pkcs11_module, 1, flags, NULL, NULL, &handle);
	g_assert (rv == CKR_OK);

	session = gkm_module_lookup_session (pkcs11_module, handle);
	g_assert (session);

	return session;
}

GkmObject*
test_module_object_new (GkmSession *session)
{
	CK_BBOOL token = CK_FALSE;
	CK_OBJECT_CLASS klass = CKO_CERTIFICATE;
	CK_CERTIFICATE_TYPE type = CKC_X_509;
	GkmObject *object;

	gsize n_data;
	guchar *data = testing_data_read ("test-certificate-1.der", &n_data);

	CK_ATTRIBUTE attrs[] = {
		{ CKA_TOKEN, &token, sizeof (token) },
		{ CKA_CLASS, &klass, sizeof (klass) },
		{ CKA_CERTIFICATE_TYPE, &type, sizeof (type) },
		{ CKA_VALUE, data, n_data },
	};

	object = gkm_session_create_object_for_factory (session, GKM_FACTORY_CERTIFICATE, NULL,
	                                              attrs, G_N_ELEMENTS (attrs));
	if (object) /* Owned by storage */
		g_object_unref (object);
	return object;
}
