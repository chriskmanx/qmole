/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* test-gck-slot.c - the GObject PKCS#11 wrapper library

   Copyright (C) 2011 Collabora Ltd.

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

   Author: Stef Walter <stefw@collabora.co.uk>
*/

#include "config.h"

#include <errno.h>
#include <glib.h>
#include <string.h>

#include "gck/gck.h"
#include "gck/gck-private.h"
#include "gck/gck-test.h"

typedef struct {
	GckModule *module;
	GckSlot *slot;
} Test;

static void
setup (Test *test, gconstpointer unused)
{
	GError *err = NULL;
	GList *slots;

	/* Successful load */
	test->module = gck_module_initialize (BUILDDIR "/.libs/libmock-test-module.so", &err);
	g_assert_no_error (err);
	g_assert (GCK_IS_MODULE (test->module));

	slots = gck_module_get_slots (test->module, TRUE);
	g_assert (slots != NULL);

	test->slot = GCK_SLOT (slots->data);
	g_object_ref (test->slot);
	gck_list_unref_free (slots);

}

static void
teardown (Test *test, gconstpointer unused)
{
	g_object_unref (test->slot);
	g_object_unref (test->module);
}

static void
test_slot_info (Test *test, gconstpointer unused)
{
	GckSlotInfo *info;
	GckTokenInfo *token;
	GList *slots, *l;

	slots = gck_module_get_slots (test->module, FALSE);
	g_assert (2 == g_list_length (slots) && "wrong number of slots returned");
	g_assert (GCK_IS_SLOT (slots->data) && "missing slot one");
	g_assert (GCK_IS_SLOT (slots->next->data) && "missing slot two");

	for (l = slots; l; l = g_list_next (l)) {
		info = gck_slot_get_info (GCK_SLOT (l->data));
		g_assert (info != NULL && "no slot info");

		g_assert (strcmp("TEST MANUFACTURER", info->manufacturer_id) == 0);
		g_assert (strcmp("TEST SLOT", info->slot_description) == 0);
		g_assert (55 == info->hardware_version_major);
		g_assert (155 == info->hardware_version_minor);
		g_assert (65 == info->firmware_version_major);
		g_assert (165 == info->firmware_version_minor);

		if (info->flags & CKF_TOKEN_PRESENT) {
			token = gck_slot_get_token_info (test->slot);
			g_assert (token != NULL && "no token info");

			g_assert (strcmp ("TEST MANUFACTURER", token->manufacturer_id) == 0);
			g_assert (strcmp ("TEST LABEL", token->label) == 0);
			g_assert (strcmp ("TEST MODEL", token->model) == 0);
			g_assert (strcmp ("TEST SERIAL", token->serial_number) == 0);
			g_assert (1 == token->max_session_count);
			g_assert (2 == token->session_count);
			g_assert (3 == token->max_rw_session_count);
			g_assert (4 == token->rw_session_count);
			g_assert (5 == token->max_pin_len);
			g_assert (6 == token->min_pin_len);
			g_assert (7 == token->total_public_memory);
			g_assert (8 == token->free_public_memory);
			g_assert (9 == token->total_private_memory);
			g_assert (10 == token->free_private_memory);
			g_assert (75 == token->hardware_version_major);
			g_assert (175 == token->hardware_version_minor);
			g_assert (85 == token->firmware_version_major);
			g_assert (185 == token->firmware_version_minor);
			g_assert (927623999 == token->utc_time);

			gck_token_info_free (token);
		}

		gck_slot_info_free (info);
	}

	gck_list_unref_free (slots);
}

static void
test_slot_props (Test *test, gconstpointer unused)
{
	GckModule *mod;
	CK_SLOT_ID slot_id;

	g_object_get (test->slot, "module", &mod, "handle", &slot_id, NULL);
	g_assert (mod == test->module);
	g_assert (slot_id == 52);

	g_object_unref (mod);
}

static void
test_slot_equals_hash (Test *test, gconstpointer unused)
{
	GckModule *other_mod;
	GckSlot *other_slot;
	GObject *obj;
	guint hash;

	hash = gck_slot_hash (test->slot);
	g_assert (hash != 0);

	g_assert (gck_slot_equal (test->slot, test->slot));

	other_mod = gck_module_new (gck_module_get_functions (test->module));
	other_slot = g_object_new (GCK_TYPE_SLOT, "module", other_mod, "handle", gck_slot_get_handle (test->slot), NULL);
	g_assert (gck_slot_equal (test->slot, other_slot));
	g_object_unref (other_mod);
	g_object_unref (other_slot);

	obj = g_object_new (G_TYPE_OBJECT, NULL);
	g_assert (!gck_slot_equal (test->slot, obj));
	g_object_unref (obj);

	other_slot = g_object_new (GCK_TYPE_SLOT, "module", test->module, "handle", 8909, NULL);
	g_assert (!gck_slot_equal (test->slot, obj));
	g_object_unref (other_slot);
}

static void
test_slot_mechanisms (Test *test, gconstpointer unused)
{
	GckMechanisms *mechs;
	GckMechanismInfo *info;
	guint i;

	mechs = gck_slot_get_mechanisms (test->slot);
	g_assert (2 == gck_mechanisms_length (mechs) && "wrong number of mech types returned");

	for (i = 0; i < gck_mechanisms_length (mechs); ++i) {

		info = gck_slot_get_mechanism_info (test->slot, gck_mechanisms_at (mechs, i));
		g_assert (info != NULL && "no mech info returned");

		gck_mechanism_info_free (info);
	}

	gck_mechanisms_free (mechs);
}

static void
test_token_info_match_null (Test *test, gconstpointer unused)
{
	GckTokenInfo *match;
	GckTokenInfo *token;
	gboolean ret;

	token = gck_slot_get_token_info (test->slot);
	match = g_new0 (GckTokenInfo, 1);

	/* Should match, since no fields are set */
	ret = _gck_token_info_match (match, token);
	g_assert (ret);

	gck_token_info_free (match);
	gck_token_info_free (token);
}

static void
test_token_info_match_label (Test *test, gconstpointer unused)
{
	GckTokenInfo *match;
	GckTokenInfo *token;
	gboolean ret;

	token = gck_slot_get_token_info (test->slot);
	match = g_new0 (GckTokenInfo, 1);

	/* Should match since the label and serial are matching */
	match->label = g_strdup (token->label);
	match->serial_number = g_strdup (token->serial_number);
	ret = _gck_token_info_match (match, token);
	g_assert (ret);

	gck_token_info_free (match);
	gck_token_info_free (token);
}

static void
test_token_info_match_different (Test *test, gconstpointer unused)
{
	GckTokenInfo *match;
	GckTokenInfo *token;
	gboolean ret;

	token = gck_slot_get_token_info (test->slot);
	match = g_new0 (GckTokenInfo, 1);

	/* Should not match since serial is different */
	match->label = g_strdup (token->label);
	match->serial_number = g_strdup ("393939393939393");
	ret = _gck_token_info_match (match, token);
	g_assert (!ret);

	gck_token_info_free (match);
	gck_token_info_free (token);
}

int
main (int argc, char **argv)
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_test_add ("/gck/slot/slot_info", Test, NULL, setup, test_slot_info, teardown);
	g_test_add ("/gck/slot/slot_props", Test, NULL, setup, test_slot_props, teardown);
	g_test_add ("/gck/slot/slot_equals_hash", Test, NULL, setup, test_slot_equals_hash, teardown);
	g_test_add ("/gck/slot/slot_mechanisms", Test, NULL, setup, test_slot_mechanisms, teardown);
	g_test_add ("/gck/slot/token_info_match_null", Test, NULL, setup, test_token_info_match_null, teardown);
	g_test_add ("/gck/slot/token_info_match_label", Test, NULL, setup, test_token_info_match_label, teardown);
	g_test_add ("/gck/slot/token_info_match_different", Test, NULL, setup, test_token_info_match_different, teardown);

	return g_test_run ();
}
