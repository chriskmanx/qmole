/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-secret-compat.c: Test secret compat files

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

#include "test-suite.h"

#include "gkm-secret-compat.h"

#include <glib.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

DEFINE_TEST(access_free)
{
	GkmSecretAccess *ac;

	ac = g_new0 (GkmSecretAccess, 1);
	ac->pathname = g_strdup ("/path");
	ac->display_name = g_strdup ("Display");
	ac->types_allowed = GKM_SECRET_ACCESS_READ;

	gkm_secret_compat_access_free (ac);
}

DEFINE_TEST(acl_free)
{
	GkmSecretAccess *ac;
	GList *acl = NULL;
	int i;

	for (i = 0; i < 10; ++i) {
		ac = g_new0 (GkmSecretAccess, 1);
		ac->pathname = g_strdup ("/path");
		ac->display_name = g_strdup ("Display");
		ac->types_allowed = GKM_SECRET_ACCESS_READ;
		acl = g_list_prepend (acl, ac);
	}

	gkm_secret_compat_acl_free (acl);
}

DEFINE_TEST(parse_item_type)
{
	guint type;

	type = gkm_secret_compat_parse_item_type ("org.freedesktop.Secret.Generic");
	g_assert_cmpuint (type, ==, 0);
	type = gkm_secret_compat_parse_item_type ("org.gnome.keyring.NetworkPassword");
	g_assert_cmpuint (type, ==, 1);
	type = gkm_secret_compat_parse_item_type ("org.gnome.keyring.Note");
	g_assert_cmpuint (type, ==, 2);
	type = gkm_secret_compat_parse_item_type ("org.gnome.keyring.ChainedKeyring");
	g_assert_cmpuint (type, ==, 3);
	type = gkm_secret_compat_parse_item_type ("org.gnome.keyring.EncryptionKey");
	g_assert_cmpuint (type, ==, 4);
	type = gkm_secret_compat_parse_item_type ("org.gnome.keyring.PkStorage");
	g_assert_cmpuint (type, ==, 0x100);

	/* Invalid returns generic secret */
	type = gkm_secret_compat_parse_item_type ("invalid");
	g_assert_cmpuint (type, ==, 0);

	/* Null returns generic secret */
	type = gkm_secret_compat_parse_item_type (NULL);
	g_assert_cmpuint (type, ==, 0);
}

DEFINE_TEST(format_item_type)
{
	const gchar *type;

	type = gkm_secret_compat_format_item_type (0);
	g_assert_cmpstr (type, ==, "org.freedesktop.Secret.Generic");
	type = gkm_secret_compat_format_item_type (1);
	g_assert_cmpstr (type, ==, "org.gnome.keyring.NetworkPassword");
	type = gkm_secret_compat_format_item_type (2);
	g_assert_cmpstr (type, ==, "org.gnome.keyring.Note");
	type = gkm_secret_compat_format_item_type (3);
	g_assert_cmpstr (type, ==, "org.gnome.keyring.ChainedKeyring");
	type = gkm_secret_compat_format_item_type (4);
	g_assert_cmpstr (type, ==, "org.gnome.keyring.EncryptionKey");
	type = gkm_secret_compat_format_item_type (0x100);
	g_assert_cmpstr (type, ==, "org.gnome.keyring.PkStorage");

	/* Higher bits shouldn't make a difference */
	type = gkm_secret_compat_format_item_type (0xF0000001);
	g_assert_cmpstr (type, ==, "org.gnome.keyring.NetworkPassword");

	/* Unrecognized should be null */
	type = gkm_secret_compat_format_item_type (32);
	g_assert (type == NULL);
}
