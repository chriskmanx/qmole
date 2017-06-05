/* This is auto-generated code. Edit at your own peril. */
#include "testing/testing.h"
#include "test-suite.h"

static void start_tests (void) {
}

static void stop_tests (void) {
}

static void initialize_tests (void) {
	g_test_add("/gp11_attributes/init_memory", int, NULL, NULL, test_init_memory, NULL);
	g_test_add("/gp11_attributes/init_boolean", int, NULL, NULL, test_init_boolean, NULL);
	g_test_add("/gp11_attributes/init_date", int, NULL, NULL, test_init_date, NULL);
	g_test_add("/gp11_attributes/init_ulong", int, NULL, NULL, test_init_ulong, NULL);
	g_test_add("/gp11_attributes/init_string", int, NULL, NULL, test_init_string, NULL);
	g_test_add("/gp11_attributes/init_invalid", int, NULL, NULL, test_init_invalid, NULL);
	g_test_add("/gp11_attributes/init_empty", int, NULL, NULL, test_init_empty, NULL);
	g_test_add("/gp11_attributes/new_memory", int, NULL, NULL, test_new_memory, NULL);
	g_test_add("/gp11_attributes/new_boolean", int, NULL, NULL, test_new_boolean, NULL);
	g_test_add("/gp11_attributes/new_date", int, NULL, NULL, test_new_date, NULL);
	g_test_add("/gp11_attributes/new_ulong", int, NULL, NULL, test_new_ulong, NULL);
	g_test_add("/gp11_attributes/new_string", int, NULL, NULL, test_new_string, NULL);
	g_test_add("/gp11_attributes/new_invalid", int, NULL, NULL, test_new_invalid, NULL);
	g_test_add("/gp11_attributes/new_empty", int, NULL, NULL, test_new_empty, NULL);
	g_test_add("/gp11_attributes/get_boolean", int, NULL, NULL, test_get_boolean, NULL);
	g_test_add("/gp11_attributes/get_date", int, NULL, NULL, test_get_date, NULL);
	g_test_add("/gp11_attributes/get_ulong", int, NULL, NULL, test_get_ulong, NULL);
	g_test_add("/gp11_attributes/get_string", int, NULL, NULL, test_get_string, NULL);
	g_test_add("/gp11_attributes/dup_attribute", int, NULL, NULL, test_dup_attribute, NULL);
	g_test_add("/gp11_attributes/copy_attribute", int, NULL, NULL, test_copy_attribute, NULL);
	g_test_add("/gp11_attributes/new_attributes", int, NULL, NULL, test_new_attributes, NULL);
	g_test_add("/gp11_attributes/newv_attributes", int, NULL, NULL, test_newv_attributes, NULL);
	g_test_add("/gp11_attributes/new_empty_attributes", int, NULL, NULL, test_new_empty_attributes, NULL);
	g_test_add("/gp11_attributes/new_valist_attributes", int, NULL, NULL, test_new_valist_attributes, NULL);
	g_test_add("/gp11_attributes/add_data_attributes", int, NULL, NULL, test_add_data_attributes, NULL);
	g_test_add("/gp11_attributes/add_attributes", int, NULL, NULL, test_add_attributes, NULL);
	g_test_add("/gp11_attributes/find_attributes", int, NULL, NULL, test_find_attributes, NULL);
	g_test_add("/gp11_mechanism/mech_new", int, NULL, NULL, test_mech_new, NULL);
	g_test_add("/gp11_mechanism/mech_new_with_param", int, NULL, NULL, test_mech_new_with_param, NULL);
	g_test_add("/gp11_mechanism/mech_ref_unref", int, NULL, NULL, test_mech_ref_unref, NULL);
	g_test_add("/gp11_mechanism/mech_unref_null", int, NULL, NULL, test_mech_unref_null, NULL);
	g_test_add("/gp11_module/invalid_modules", int, NULL, setup_load_module, test_invalid_modules, teardown_load_module);
	g_test_add("/gp11_module/module_equals_hash", int, NULL, setup_load_module, test_module_equals_hash, teardown_load_module);
	g_test_add("/gp11_module/module_props", int, NULL, setup_load_module, test_module_props, teardown_load_module);
	g_test_add("/gp11_module/module_info", int, NULL, setup_load_module, test_module_info, teardown_load_module);
	g_test_add("/gp11_module/module_enumerate", int, NULL, setup_load_module, test_module_enumerate, teardown_load_module);
	g_test_add("/gp11_slot/slot_info", int, NULL, setup_load_slots, test_slot_info, teardown_load_slots);
	g_test_add("/gp11_slot/slot_props", int, NULL, setup_load_slots, test_slot_props, teardown_load_slots);
	g_test_add("/gp11_slot/slot_equals_hash", int, NULL, setup_load_slots, test_slot_equals_hash, teardown_load_slots);
	g_test_add("/gp11_slot/slot_mechanisms", int, NULL, setup_load_slots, test_slot_mechanisms, teardown_load_slots);
	g_test_add("/gp11_session/session_props", int, NULL, setup_load_session, test_session_props, teardown_load_session);
	g_test_add("/gp11_session/session_info", int, NULL, setup_load_session, test_session_info, teardown_load_session);
	g_test_add("/gp11_session/open_close_session", int, NULL, setup_load_session, test_open_close_session, teardown_load_session);
	g_test_add("/gp11_session/open_reused", int, NULL, setup_load_session, test_open_reused, teardown_load_session);
	g_test_add("/gp11_session/init_set_pin", int, NULL, setup_load_session, test_init_set_pin, teardown_load_session);
	g_test_add("/gp11_session/login_logout", int, NULL, setup_load_session, test_login_logout, teardown_load_session);
	g_test_add("/gp11_session/auto_login", int, NULL, setup_load_session, test_auto_login, teardown_load_session);
	g_test_add("/gp11_object/object_props", int, NULL, setup_prep_object, test_object_props, teardown_prep_object);
	g_test_add("/gp11_object/object_equals_hash", int, NULL, setup_prep_object, test_object_equals_hash, teardown_prep_object);
	g_test_add("/gp11_object/create_object", int, NULL, setup_prep_object, test_create_object, teardown_prep_object);
	g_test_add("/gp11_object/destroy_object", int, NULL, setup_prep_object, test_destroy_object, teardown_prep_object);
	g_test_add("/gp11_object/get_attributes", int, NULL, setup_prep_object, test_get_attributes, teardown_prep_object);
	g_test_add("/gp11_object/get_data_attribute", int, NULL, setup_prep_object, test_get_data_attribute, teardown_prep_object);
	g_test_add("/gp11_object/set_attributes", int, NULL, setup_prep_object, test_set_attributes, teardown_prep_object);
	g_test_add("/gp11_object/find_objects", int, NULL, setup_prep_object, test_find_objects, teardown_prep_object);
	g_test_add("/gp11_object/explicit_sessions", int, NULL, setup_prep_object, test_explicit_sessions, teardown_prep_object);
	g_test_add("/gp11_crypto/encrypt", int, NULL, setup_crypto_session, test_encrypt, teardown_crypto_session);
	g_test_add("/gp11_crypto/decrypt", int, NULL, setup_crypto_session, test_decrypt, teardown_crypto_session);
	g_test_add("/gp11_crypto/login_context_specific", int, NULL, setup_crypto_session, test_login_context_specific, teardown_crypto_session);
	g_test_add("/gp11_crypto/sign", int, NULL, setup_crypto_session, test_sign, teardown_crypto_session);
	g_test_add("/gp11_crypto/verify", int, NULL, setup_crypto_session, test_verify, teardown_crypto_session);
	g_test_add("/gp11_crypto/generate_key_pair", int, NULL, setup_crypto_session, test_generate_key_pair, teardown_crypto_session);
	g_test_add("/gp11_crypto/wrap_key", int, NULL, setup_crypto_session, test_wrap_key, teardown_crypto_session);
	g_test_add("/gp11_crypto/unwrap_key", int, NULL, setup_crypto_session, test_unwrap_key, teardown_crypto_session);
	g_test_add("/gp11_crypto/derive_key", int, NULL, setup_crypto_session, test_derive_key, teardown_crypto_session);
}

static void run_externals (int *ret) {
}

static int run(void) {
	int ret;
	initialize_tests ();
	start_tests ();
	ret = g_test_run ();
	if (ret == 0)
		run_externals (&ret);
	stop_tests();
	return ret;
}
#include "testing/testing.c"
