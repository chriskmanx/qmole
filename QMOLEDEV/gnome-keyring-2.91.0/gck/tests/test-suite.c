/* This is auto-generated code. Edit at your own peril. */
#include "testing/testing.h"
#include "test-suite.h"

static void start_tests (void) {
}

static void stop_tests (void) {
}

static void initialize_tests (void) {
	g_test_add("/gck_attributes/init_memory", int, NULL, NULL, test_init_memory, NULL);
	g_test_add("/gck_attributes/value_to_boolean", int, NULL, NULL, test_value_to_boolean, NULL);
	g_test_add("/gck_attributes/value_to_ulong", int, NULL, NULL, test_value_to_ulong, NULL);
	g_test_add("/gck_attributes/init_boolean", int, NULL, NULL, test_init_boolean, NULL);
	g_test_add("/gck_attributes/init_date", int, NULL, NULL, test_init_date, NULL);
	g_test_add("/gck_attributes/init_ulong", int, NULL, NULL, test_init_ulong, NULL);
	g_test_add("/gck_attributes/init_string", int, NULL, NULL, test_init_string, NULL);
	g_test_add("/gck_attributes/init_invalid", int, NULL, NULL, test_init_invalid, NULL);
	g_test_add("/gck_attributes/init_empty", int, NULL, NULL, test_init_empty, NULL);
	g_test_add("/gck_attributes/new_memory", int, NULL, NULL, test_new_memory, NULL);
	g_test_add("/gck_attributes/new_boolean", int, NULL, NULL, test_new_boolean, NULL);
	g_test_add("/gck_attributes/new_date", int, NULL, NULL, test_new_date, NULL);
	g_test_add("/gck_attributes/new_ulong", int, NULL, NULL, test_new_ulong, NULL);
	g_test_add("/gck_attributes/new_string", int, NULL, NULL, test_new_string, NULL);
	g_test_add("/gck_attributes/new_invalid", int, NULL, NULL, test_new_invalid, NULL);
	g_test_add("/gck_attributes/new_empty", int, NULL, NULL, test_new_empty, NULL);
	g_test_add("/gck_attributes/get_boolean", int, NULL, NULL, test_get_boolean, NULL);
	g_test_add("/gck_attributes/get_date", int, NULL, NULL, test_get_date, NULL);
	g_test_add("/gck_attributes/get_ulong", int, NULL, NULL, test_get_ulong, NULL);
	g_test_add("/gck_attributes/get_string", int, NULL, NULL, test_get_string, NULL);
	g_test_add("/gck_attributes/dup_attribute", int, NULL, NULL, test_dup_attribute, NULL);
	g_test_add("/gck_attributes/copy_attribute", int, NULL, NULL, test_copy_attribute, NULL);
	g_test_add("/gck_attributes/new_attributes", int, NULL, NULL, test_new_attributes, NULL);
	g_test_add("/gck_attributes/new_empty_attributes", int, NULL, NULL, test_new_empty_attributes, NULL);
	g_test_add("/gck_attributes/add_data_attributes", int, NULL, NULL, test_add_data_attributes, NULL);
	g_test_add("/gck_attributes/add_attributes", int, NULL, NULL, test_add_attributes, NULL);
	g_test_add("/gck_attributes/add_all_attributes", int, NULL, NULL, test_add_all_attributes, NULL);
	g_test_add("/gck_attributes/find_attributes", int, NULL, NULL, test_find_attributes, NULL);
	g_test_add("/gck_module/invalid_modules", int, NULL, setup_load_module, test_invalid_modules, teardown_load_module);
	g_test_add("/gck_module/module_equals_hash", int, NULL, setup_load_module, test_module_equals_hash, teardown_load_module);
	g_test_add("/gck_module/module_props", int, NULL, setup_load_module, test_module_props, teardown_load_module);
	g_test_add("/gck_module/module_info", int, NULL, setup_load_module, test_module_info, teardown_load_module);
	g_test_add("/gck_module/module_enumerate", int, NULL, setup_load_module, test_module_enumerate, teardown_load_module);
	g_test_add("/gck_slot/slot_info", int, NULL, setup_load_slots, test_slot_info, teardown_load_slots);
	g_test_add("/gck_slot/slot_props", int, NULL, setup_load_slots, test_slot_props, teardown_load_slots);
	g_test_add("/gck_slot/slot_equals_hash", int, NULL, setup_load_slots, test_slot_equals_hash, teardown_load_slots);
	g_test_add("/gck_slot/slot_mechanisms", int, NULL, setup_load_slots, test_slot_mechanisms, teardown_load_slots);
	g_test_add("/gck_slot/token_info_match_null", int, NULL, setup_load_slots, test_token_info_match_null, teardown_load_slots);
	g_test_add("/gck_slot/token_info_match_label", int, NULL, setup_load_slots, test_token_info_match_label, teardown_load_slots);
	g_test_add("/gck_slot/token_info_match_different", int, NULL, setup_load_slots, test_token_info_match_different, teardown_load_slots);
	g_test_add("/gck_session/session_props", int, NULL, setup_load_session, test_session_props, teardown_load_session);
	g_test_add("/gck_session/session_info", int, NULL, setup_load_session, test_session_info, teardown_load_session);
	g_test_add("/gck_session/open_close_session", int, NULL, setup_load_session, test_open_close_session, teardown_load_session);
	g_test_add("/gck_session/init_set_pin", int, NULL, setup_load_session, test_init_set_pin, teardown_load_session);
	g_test_add("/gck_session/login_logout", int, NULL, setup_load_session, test_login_logout, teardown_load_session);
	g_test_add("/gck_session/auto_login", int, NULL, setup_load_session, test_auto_login, teardown_load_session);
	g_test_add("/gck_object/object_props", int, NULL, setup_prep_object, test_object_props, teardown_prep_object);
	g_test_add("/gck_object/object_equals_hash", int, NULL, setup_prep_object, test_object_equals_hash, teardown_prep_object);
	g_test_add("/gck_object/create_object", int, NULL, setup_prep_object, test_create_object, teardown_prep_object);
	g_test_add("/gck_object/destroy_object", int, NULL, setup_prep_object, test_destroy_object, teardown_prep_object);
	g_test_add("/gck_object/get_attributes", int, NULL, setup_prep_object, test_get_attributes, teardown_prep_object);
	g_test_add("/gck_object/get_data_attribute", int, NULL, setup_prep_object, test_get_data_attribute, teardown_prep_object);
	g_test_add("/gck_object/set_attributes", int, NULL, setup_prep_object, test_set_attributes, teardown_prep_object);
	g_test_add("/gck_object/find_objects", int, NULL, setup_prep_object, test_find_objects, teardown_prep_object);
	g_test_add("/gck_crypto/encrypt", int, NULL, setup_crypto_session, test_encrypt, teardown_crypto_session);
	g_test_add("/gck_crypto/decrypt", int, NULL, setup_crypto_session, test_decrypt, teardown_crypto_session);
	g_test_add("/gck_crypto/login_context_specific", int, NULL, setup_crypto_session, test_login_context_specific, teardown_crypto_session);
	g_test_add("/gck_crypto/sign", int, NULL, setup_crypto_session, test_sign, teardown_crypto_session);
	g_test_add("/gck_crypto/verify", int, NULL, setup_crypto_session, test_verify, teardown_crypto_session);
	g_test_add("/gck_crypto/generate_key_pair", int, NULL, setup_crypto_session, test_generate_key_pair, teardown_crypto_session);
	g_test_add("/gck_crypto/wrap_key", int, NULL, setup_crypto_session, test_wrap_key, teardown_crypto_session);
	g_test_add("/gck_crypto/unwrap_key", int, NULL, setup_crypto_session, test_unwrap_key, teardown_crypto_session);
	g_test_add("/gck_crypto/derive_key", int, NULL, setup_crypto_session, test_derive_key, teardown_crypto_session);
	g_test_add("/gck_uri/uri_parse", int, NULL, setup_uri, test_uri_parse, teardown_uri);
	g_test_add("/gck_uri/uri_parse_bad_scheme", int, NULL, setup_uri, test_uri_parse_bad_scheme, teardown_uri);
	g_test_add("/gck_uri/uri_parse_with_label", int, NULL, setup_uri, test_uri_parse_with_label, teardown_uri);
	g_test_add("/gck_uri/uri_parse_with_label_and_klass", int, NULL, setup_uri, test_uri_parse_with_label_and_klass, teardown_uri);
	g_test_add("/gck_uri/uri_parse_with_id", int, NULL, setup_uri, test_uri_parse_with_id, teardown_uri);
	g_test_add("/gck_uri/uri_parse_with_bad_string_encoding", int, NULL, setup_uri, test_uri_parse_with_bad_string_encoding, teardown_uri);
	g_test_add("/gck_uri/uri_parse_with_bad_binary_encoding", int, NULL, setup_uri, test_uri_parse_with_bad_binary_encoding, teardown_uri);
	g_test_add("/gck_uri/uri_parse_with_token", int, NULL, setup_uri, test_uri_parse_with_token, teardown_uri);
	g_test_add("/gck_uri/uri_parse_with_token_bad_encoding", int, NULL, setup_uri, test_uri_parse_with_token_bad_encoding, teardown_uri);
	g_test_add("/gck_uri/uri_parse_with_bad_syntax", int, NULL, setup_uri, test_uri_parse_with_bad_syntax, teardown_uri);
	g_test_add("/gck_uri/uri_build_empty", int, NULL, setup_uri, test_uri_build_empty, teardown_uri);
	g_test_add("/gck_uri/uri_build_with_token_info", int, NULL, setup_uri, test_uri_build_with_token_info, teardown_uri);
	g_test_add("/gck_uri/uri_build_with_attributes", int, NULL, setup_uri, test_uri_build_with_attributes, teardown_uri);
	g_test_add("/gck_enumerator/enumerator_create", int, NULL, setup_enumerator, test_enumerator_create, teardown_enumerator);
	g_test_add("/gck_enumerator/enumerator_next", int, NULL, setup_enumerator, test_enumerator_next, teardown_enumerator);
	g_test_add("/gck_enumerator/enumerator_next_and_resume", int, NULL, setup_enumerator, test_enumerator_next_and_resume, teardown_enumerator);
	g_test_add("/gck_enumerator/enumerator_next_n", int, NULL, setup_enumerator, test_enumerator_next_n, teardown_enumerator);
	g_test_add("/gck_enumerator/enumerator_next_async", int, NULL, setup_enumerator, test_enumerator_next_async, teardown_enumerator);
	g_test_add("/gck_enumerator/enumerator_attributes", int, NULL, setup_enumerator, test_enumerator_attributes, teardown_enumerator);
	g_test_add("/gck_enumerator/enumerator_token_match", int, NULL, setup_enumerator, test_enumerator_token_match, teardown_enumerator);
	g_test_add("/gck_modules/modules_enumerate_objects", int, NULL, setup_modules, test_modules_enumerate_objects, teardown_modules);
	g_test_add("/gck_modules/modules_token_for_uri", int, NULL, setup_modules, test_modules_token_for_uri, teardown_modules);
	g_test_add("/gck_modules/modules_token_for_uri_not_found", int, NULL, setup_modules, test_modules_token_for_uri_not_found, teardown_modules);
	g_test_add("/gck_modules/modules_token_for_uri_error", int, NULL, setup_modules, test_modules_token_for_uri_error, teardown_modules);
	g_test_add("/gck_modules/modules_object_for_uri", int, NULL, setup_modules, test_modules_object_for_uri, teardown_modules);
	g_test_add("/gck_modules/modules_object_for_uri_not_found", int, NULL, setup_modules, test_modules_object_for_uri_not_found, teardown_modules);
	g_test_add("/gck_modules/modules_object_for_uri_error", int, NULL, setup_modules, test_modules_object_for_uri_error, teardown_modules);
	g_test_add("/gck_modules/modules_objects_for_uri", int, NULL, setup_modules, test_modules_objects_for_uri, teardown_modules);
	g_test_add("/gck_modules/modules_enumerate_uri", int, NULL, setup_modules, test_modules_enumerate_uri, teardown_modules);
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
