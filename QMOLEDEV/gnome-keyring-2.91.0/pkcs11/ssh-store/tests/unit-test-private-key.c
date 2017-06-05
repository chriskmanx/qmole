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

#include "test-suite.h"
#include "test-ssh-module.h"

#include "gkm/gkm-credential.h"
#include "gkm/gkm-session.h"
#include "gkm/gkm-module.h"

#include "ssh-store/gkm-ssh-private-key.h"

#include "pkcs11g.h"

static GkmModule *module = NULL;
static GkmSession *session = NULL;

DEFINE_SETUP(private_key_setup)
{
	module = test_ssh_module_initialize_and_enter ();
	session = test_ssh_module_open_session (TRUE);
}

DEFINE_TEARDOWN(private_key_teardown)
{
	test_ssh_module_leave_and_finalize ();
	module = NULL;
	session = NULL;
}

DEFINE_TEST(private_key_parse_plain)
{
	GkmSshPrivateKey *key;
	gchar *pub_path, *priv_path;
	gboolean ret;

	key = gkm_ssh_private_key_new (module, "my-unique");
	g_assert (GKM_IS_SSH_PRIVATE_KEY (key));

	pub_path = testing_data_filename ("id_dsa_plain.pub");
	priv_path = testing_data_filename ("id_dsa_plain");

	ret = gkm_ssh_private_key_parse (key, pub_path, priv_path, NULL);
	g_assert (ret == TRUE);

	g_object_unref (key);
	g_free (pub_path);
	g_free (priv_path);
}


DEFINE_TEST(private_key_parse_and_unlock)
{
	GkmSshPrivateKey *key;
	GkmCredential *cred;
	gchar *pub_path, *priv_path;
	gboolean ret;
	CK_RV rv;

	key = gkm_ssh_private_key_new (module, "my-unique");
	g_assert (GKM_IS_SSH_PRIVATE_KEY (key));

	pub_path = testing_data_filename ("id_dsa_encrypted.pub");
	priv_path = testing_data_filename ("id_dsa_encrypted");

	ret = gkm_ssh_private_key_parse (key, pub_path, priv_path, NULL);
	g_assert (ret == TRUE);

	g_free (pub_path);
	g_free (priv_path);

	rv = gkm_credential_create (module, NULL, GKM_OBJECT (key), (guchar*)"password", 8, &cred);
	g_assert (rv == CKR_OK);

	g_object_unref (cred);
	g_object_unref (key);
}
