/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-private-key.c: Test SSH Key Private key functionality

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

#include "mock-ssh-module.h"

#include "gkm/gkm-credential.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-module.h"

#include "ssh-store/gkm-ssh-private-key.h"

#include "pkcs11i.h"

typedef struct {
	GkmModule *module;
	GkmSession *session;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	test->module = test_ssh_module_initialize_and_enter ();
	test->session = test_ssh_module_open_session (TRUE);
}

static void
teardown (Test *test, gconstpointer unused)
{
	test_ssh_module_leave_and_finalize ();
}

static void
test_parse_plain (Test *test, gconstpointer unused)
{
	GkmSshPrivateKey *key;
	gboolean ret;

	key = gkm_ssh_private_key_new (test->module, "my-unique");
	g_assert (GKM_IS_SSH_PRIVATE_KEY (key));

	ret = gkm_ssh_private_key_parse (key, SRCDIR "/files/id_dsa_plain.pub",
	                                 SRCDIR "/files/id_dsa_plain", NULL);
	g_assert (ret == TRUE);

	g_object_unref (key);
}

static void
test_parse_and_unlock (Test *test, gconstpointer unused)
{
	GkmSshPrivateKey *key;
	GkmCredential *cred;
	gboolean ret;
	CK_RV rv;

	key = gkm_ssh_private_key_new (test->module, "my-unique");
	g_assert (GKM_IS_SSH_PRIVATE_KEY (key));

	ret = gkm_ssh_private_key_parse (key, SRCDIR "/files/id_dsa_encrypted.pub",
	                                 SRCDIR "/files/id_dsa_encrypted", NULL);
	g_assert (ret == TRUE);

	rv = gkm_credential_create (test->module, NULL, GKM_OBJECT (key), (guchar*)"password", 8, &cred);
	g_assert (rv == CKR_OK);

	g_object_unref (cred);
	g_object_unref (key);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/ssh-store/private-key/parse_plain", Test, NULL, setup, test_parse_plain, teardown);
	g_test_add ("/ssh-store/private-key/parse_and_unlock", Test, NULL, setup, test_parse_and_unlock, teardown);

	return g_test_run ();
}
