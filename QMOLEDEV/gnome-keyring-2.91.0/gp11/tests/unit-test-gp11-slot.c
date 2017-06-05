
#include <glib.h>
#include <string.h>

#include "test-suite.h"
#include "gp11-test.h"

static GP11Module *module = NULL;
static GP11Slot *slot = NULL;

DEFINE_SETUP(load_slots)
{
	GError *err = NULL;
	GList *slots;
	
	/* Successful load */
	module = gp11_module_initialize (".libs/libgp11-test-module.so", NULL, &err);
	SUCCESS_RES (module, err);
	
	slots = gp11_module_get_slots (module, TRUE);
	g_assert (slots != NULL);
	
	slot = GP11_SLOT (slots->data);
	g_object_ref (slot);
	gp11_list_unref_free (slots);
	
}

DEFINE_TEARDOWN(load_slots)
{
	g_object_unref (slot);
	g_object_unref (module);
}

DEFINE_TEST(slot_info)
{
	GP11SlotInfo *info;
	GP11TokenInfo *token;
	GList *slots, *l;

	slots = gp11_module_get_slots (module, FALSE);
	g_assert (2 == g_list_length (slots) && "wrong number of slots returned");
	g_assert (GP11_IS_SLOT (slots->data) && "missing slot one");
	g_assert (GP11_IS_SLOT (slots->next->data) && "missing slot two");
	
	for (l = slots; l; l = g_list_next (l)) {
		info = gp11_slot_get_info (GP11_SLOT (l->data));
		g_assert (info != NULL && "no slot info");

		g_assert (strcmp("TEST MANUFACTURER", info->manufacturer_id) == 0);
		g_assert (strcmp("TEST SLOT", info->slot_description) == 0);
		g_assert (55 == info->hardware_version_major);
		g_assert (155 == info->hardware_version_minor);
		g_assert (65 == info->firmware_version_major);
		g_assert (165 == info->firmware_version_minor);
	
		if (info->flags & CKF_TOKEN_PRESENT) {		
			token = gp11_slot_get_token_info (slot);
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
			
			gp11_token_info_free (token);
		}
		
		gp11_slot_info_free (info);
	}
	
	gp11_list_unref_free (slots);
}

DEFINE_TEST(slot_props)
{
	GP11Module *mod;
	CK_SLOT_ID slot_id;
	
	g_object_get (slot, "module", &mod, "handle", &slot_id, NULL);
	g_assert (mod == module);
	g_assert (slot_id == 52);

	g_object_unref (mod);
}

DEFINE_TEST(slot_equals_hash)
{
	GP11Module *other_mod;
	GP11Slot *other_slot;
	GObject *obj;
	guint hash;
	
	hash = gp11_slot_hash (slot);
	g_assert (hash != 0);
	
	g_assert (gp11_slot_equal (slot, slot));
	
	other_mod = gp11_module_new (gp11_module_get_functions (module));
	other_slot = g_object_new (GP11_TYPE_SLOT, "module", other_mod, "handle", gp11_slot_get_handle (slot), NULL);
	g_assert (gp11_slot_equal (slot, other_slot));
	g_object_unref (other_mod);
	g_object_unref (other_slot);
	
	obj = g_object_new (G_TYPE_OBJECT, NULL);
	g_assert (!gp11_slot_equal (slot, obj));
	g_object_unref (obj);

	other_slot = g_object_new (GP11_TYPE_SLOT, "module", module, "handle", 8909, NULL);
	g_assert (!gp11_slot_equal (slot, obj));
	g_object_unref (other_slot);
}

DEFINE_TEST(slot_mechanisms)
{
	GP11Mechanisms *mechs;
	GP11MechanismInfo *info;
	guint i;
	
	mechs = gp11_slot_get_mechanisms (slot);
	g_assert (2 == gp11_mechanisms_length (mechs) && "wrong number of mech types returned");

	for (i = 0; i < gp11_mechanisms_length (mechs); ++i) {
		
		info = gp11_slot_get_mechanism_info (slot, gp11_mechanisms_at (mechs, i));
		g_assert (info != NULL && "no mech info returned");
		
		gp11_mechanism_info_free (info);
	}
	
	gp11_mechanisms_free (mechs);
}

