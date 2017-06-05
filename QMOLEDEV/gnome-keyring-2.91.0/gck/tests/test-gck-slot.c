
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gck-test.h"
#include "gck-private.h"

static GckModule *module = NULL;
static GckSlot *slot = NULL;

DEFINE_SETUP(load_slots)
{
	GError *err = NULL;
	GList *slots;

	/* Successful load */
	module = gck_module_initialize (".libs/libmock-test-module.so", NULL, 0, &err);
	SUCCESS_RES (module, err);

	slots = gck_module_get_slots (module, TRUE);
	g_assert (slots != NULL);

	slot = GCK_SLOT (slots->data);
	g_object_ref (slot);
	gck_list_unref_free (slots);

}

DEFINE_TEARDOWN(load_slots)
{
	g_object_unref (slot);
	g_object_unref (module);
}

DEFINE_TEST(slot_info)
{
	GckSlotInfo *info;
	GckTokenInfo *token;
	GList *slots, *l;

	slots = gck_module_get_slots (module, FALSE);
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
			token = gck_slot_get_token_info (slot);
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

DEFINE_TEST(slot_props)
{
	GckModule *mod;
	CK_SLOT_ID slot_id;

	g_object_get (slot, "module", &mod, "handle", &slot_id, NULL);
	g_assert (mod == module);
	g_assert (slot_id == 52);

	g_object_unref (mod);
}

DEFINE_TEST(slot_equals_hash)
{
	GckModule *other_mod;
	GckSlot *other_slot;
	GObject *obj;
	guint hash;

	hash = gck_slot_hash (slot);
	g_assert (hash != 0);

	g_assert (gck_slot_equal (slot, slot));

	other_mod = gck_module_new (gck_module_get_functions (module), 0);
	other_slot = g_object_new (GCK_TYPE_SLOT, "module", other_mod, "handle", gck_slot_get_handle (slot), NULL);
	g_assert (gck_slot_equal (slot, other_slot));
	g_object_unref (other_mod);
	g_object_unref (other_slot);

	obj = g_object_new (G_TYPE_OBJECT, NULL);
	g_assert (!gck_slot_equal (slot, obj));
	g_object_unref (obj);

	other_slot = g_object_new (GCK_TYPE_SLOT, "module", module, "handle", 8909, NULL);
	g_assert (!gck_slot_equal (slot, obj));
	g_object_unref (other_slot);
}

DEFINE_TEST(slot_mechanisms)
{
	GckMechanisms *mechs;
	GckMechanismInfo *info;
	guint i;

	mechs = gck_slot_get_mechanisms (slot);
	g_assert (2 == gck_mechanisms_length (mechs) && "wrong number of mech types returned");

	for (i = 0; i < gck_mechanisms_length (mechs); ++i) {

		info = gck_slot_get_mechanism_info (slot, gck_mechanisms_at (mechs, i));
		g_assert (info != NULL && "no mech info returned");

		gck_mechanism_info_free (info);
	}

	gck_mechanisms_free (mechs);
}

DEFINE_TEST(token_info_match_null)
{
	GckTokenInfo *match;
	GckTokenInfo *token;
	gboolean ret;

	token = gck_slot_get_token_info (slot);
	match = g_new0 (GckTokenInfo, 1);

	/* Should match, since no fields are set */
	ret = _gck_token_info_match (match, token);
	g_assert (ret);

	gck_token_info_free (match);
	gck_token_info_free (token);
}

DEFINE_TEST(token_info_match_label)
{
	GckTokenInfo *match;
	GckTokenInfo *token;
	gboolean ret;

	token = gck_slot_get_token_info (slot);
	match = g_new0 (GckTokenInfo, 1);

	/* Should match since the label and serial are matching */
	match->label = g_strdup (token->label);
	match->serial_number = g_strdup (token->serial_number);
	ret = _gck_token_info_match (match, token);
	g_assert (ret);

	gck_token_info_free (match);
	gck_token_info_free (token);
}

DEFINE_TEST(token_info_match_different)
{
	GckTokenInfo *match;
	GckTokenInfo *token;
	gboolean ret;

	token = gck_slot_get_token_info (slot);
	match = g_new0 (GckTokenInfo, 1);

	/* Should not match since serial is different */
	match->label = g_strdup (token->label);
	match->serial_number = g_strdup ("393939393939393");
	ret = _gck_token_info_match (match, token);
	g_assert (!ret);

	gck_token_info_free (match);
	gck_token_info_free (token);
}
