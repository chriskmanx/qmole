/* This is auto-generated code. Edit at your own peril. */
#include "testing/testing.h"
#include "test-suite.h"

static void start_tests (void) {
}

static void stop_tests (void) {
}

static void initialize_tests (void) {
	g_test_add("/secret_compat/access_free", int, NULL, NULL, test_access_free, NULL);
	g_test_add("/secret_compat/acl_free", int, NULL, NULL, test_acl_free, NULL);
	g_test_add("/secret_compat/parse_item_type", int, NULL, NULL, test_parse_item_type, NULL);
	g_test_add("/secret_compat/format_item_type", int, NULL, NULL, test_format_item_type, NULL);
	g_test_add("/secret_fields/fields_new", int, NULL, NULL, test_fields_new, NULL);
	g_test_add("/secret_fields/fields_boxed", int, NULL, NULL, test_fields_boxed, NULL);
	g_test_add("/secret_fields/fields_add_get_values", int, NULL, NULL, test_fields_add_get_values, NULL);
	g_test_add("/secret_fields/fields_parse", int, NULL, NULL, test_fields_parse, NULL);
	g_test_add("/secret_fields/fields_parse_empty", int, NULL, NULL, test_fields_parse_empty, NULL);
	g_test_add("/secret_fields/fields_parse_null_invalid", int, NULL, NULL, test_fields_parse_null_invalid, NULL);
	g_test_add("/secret_fields/fields_parse_missing_value", int, NULL, NULL, test_fields_parse_missing_value, NULL);
	g_test_add("/secret_fields/fields_parse_missing_terminator", int, NULL, NULL, test_fields_parse_missing_terminator, NULL);
	g_test_add("/secret_fields/fields_parse_not_utf8", int, NULL, NULL, test_fields_parse_not_utf8, NULL);
	g_test_add("/secret_fields/fields_serialize", int, NULL, NULL, test_fields_serialize, NULL);
	g_test_add("/secret_fields/fields_serialize_length", int, NULL, NULL, test_fields_serialize_length, NULL);
	g_test_add("/secret_fields/fields_add_get_compat_uint32", int, NULL, NULL, test_fields_add_get_compat_uint32, NULL);
	g_test_add("/secret_fields/fields_get_compat_uint32_fail", int, NULL, NULL, test_fields_get_compat_uint32_fail, NULL);
	g_test_add("/secret_fields/fields_get_compat_hashed_string", int, NULL, NULL, test_fields_get_compat_hashed_string, NULL);
	g_test_add("/secret_fields/fields_get_compat_hashed_already", int, NULL, NULL, test_fields_get_compat_hashed_already, NULL);
	g_test_add("/secret_fields/fields_get_compat_hashed_uint32", int, NULL, NULL, test_fields_get_compat_hashed_uint32, NULL);
	g_test_add("/secret_fields/fields_get_compat_hashed_uint32_already", int, NULL, NULL, test_fields_get_compat_hashed_uint32_already, NULL);
	g_test_add("/secret_fields/fields_get_names", int, NULL, NULL, test_fields_get_names, NULL);
	g_test_add("/secret_fields/fields_match", int, NULL, NULL, test_fields_match, NULL);
	g_test_add("/secret_fields/fields_match_mismatch_value", int, NULL, NULL, test_fields_match_mismatch_value, NULL);
	g_test_add("/secret_fields/fields_match_mismatch_field", int, NULL, NULL, test_fields_match_mismatch_field, NULL);
	g_test_add("/secret_fields/fields_match_wrong_hashed", int, NULL, NULL, test_fields_match_wrong_hashed, NULL);
	g_test_add("/secret_data/secret_data_new", int, NULL, NULL, test_secret_data_new, NULL);
	g_test_add("/secret_data/secret_data_get_set", int, NULL, NULL, test_secret_data_get_set, NULL);
	g_test_add("/secret_data/secret_data_get_raw", int, NULL, NULL, test_secret_data_get_raw, NULL);
	g_test_add("/secret_data/secret_data_remove", int, NULL, NULL, test_secret_data_remove, NULL);
	g_test_add("/secret_data/secret_data_set_transacted", int, NULL, NULL, test_secret_data_set_transacted, NULL);
	g_test_add("/secret_data/secret_data_set_transacted_replace", int, NULL, NULL, test_secret_data_set_transacted_replace, NULL);
	g_test_add("/secret_data/secret_data_set_transacted_fail", int, NULL, NULL, test_secret_data_set_transacted_fail, NULL);
	g_test_add("/secret_data/secret_data_set_transacted_fail_revert", int, NULL, NULL, test_secret_data_set_transacted_fail_revert, NULL);
	g_test_add("/secret_data/secret_data_get_set_master", int, NULL, NULL, test_secret_data_get_set_master, NULL);
	g_test_add("/secret_object/secret_object_is_locked", int, NULL, setup_secret_object, test_secret_object_is_locked, teardown_secret_object);
	g_test_add("/secret_object/secret_object_identifier_prop", int, NULL, setup_secret_object, test_secret_object_identifier_prop, teardown_secret_object);
	g_test_add("/secret_object/secret_object_created_prop", int, NULL, setup_secret_object, test_secret_object_created_prop, teardown_secret_object);
	g_test_add("/secret_object/secret_object_modified_prop", int, NULL, setup_secret_object, test_secret_object_modified_prop, teardown_secret_object);
	g_test_add("/secret_object/secret_object_was_modified", int, NULL, setup_secret_object, test_secret_object_was_modified, teardown_secret_object);
	g_test_add("/secret_object/secret_object_label_prop", int, NULL, setup_secret_object, test_secret_object_label_prop, teardown_secret_object);
	g_test_add("/secret_object/secret_object_identifier_get_attr", int, NULL, setup_secret_object, test_secret_object_identifier_get_attr, teardown_secret_object);
	g_test_add("/secret_object/secret_object_label_get_attr", int, NULL, setup_secret_object, test_secret_object_label_get_attr, teardown_secret_object);
	g_test_add("/secret_object/secret_object_label_set_attr", int, NULL, setup_secret_object, test_secret_object_label_set_attr, teardown_secret_object);
	g_test_add("/secret_object/secret_object_label_set_attr_fail", int, NULL, setup_secret_object, test_secret_object_label_set_attr_fail, teardown_secret_object);
	g_test_add("/secret_object/secret_object_modified_get_attr", int, NULL, setup_secret_object, test_secret_object_modified_get_attr, teardown_secret_object);
	g_test_add("/secret_object/secret_object_created_get_attr", int, NULL, setup_secret_object, test_secret_object_created_get_attr, teardown_secret_object);
	g_test_add("/secret_object/secret_object_locked_get_attr", int, NULL, setup_secret_object, test_secret_object_locked_get_attr, teardown_secret_object);
	g_test_add("/secret_collection/secret_collection_is_locked", int, NULL, setup_secret_collection, test_secret_collection_is_locked, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_unlocked_data", int, NULL, setup_secret_collection, test_secret_collection_unlocked_data, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_get_filename", int, NULL, setup_secret_collection, test_secret_collection_get_filename, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_set_filename", int, NULL, setup_secret_collection, test_secret_collection_set_filename, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_has_item", int, NULL, setup_secret_collection, test_secret_collection_has_item, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_load_unlock_plain", int, NULL, setup_secret_collection, test_secret_collection_load_unlock_plain, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_load_unlock_encrypted", int, NULL, setup_secret_collection, test_secret_collection_load_unlock_encrypted, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_load_unlock_bad_password", int, NULL, setup_secret_collection, test_secret_collection_load_unlock_bad_password, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_unlock_without_load", int, NULL, setup_secret_collection, test_secret_collection_unlock_without_load, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_twice_unlock", int, NULL, setup_secret_collection, test_secret_collection_twice_unlock, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_twice_unlock_bad_password", int, NULL, setup_secret_collection, test_secret_collection_twice_unlock_bad_password, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_memory_unlock", int, NULL, setup_secret_collection, test_secret_collection_memory_unlock, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_memory_unlock_bad_password", int, NULL, setup_secret_collection, test_secret_collection_memory_unlock_bad_password, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_factory", int, NULL, setup_secret_collection, test_secret_collection_factory, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_factory_unnamed", int, NULL, setup_secret_collection, test_secret_collection_factory_unnamed, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_factory_token", int, NULL, setup_secret_collection, test_secret_collection_factory_token, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_factory_duplicate", int, NULL, setup_secret_collection, test_secret_collection_factory_duplicate, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_factory_item", int, NULL, setup_secret_collection, test_secret_collection_factory_item, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_token_remove", int, NULL, setup_secret_collection, test_secret_collection_token_remove, teardown_secret_collection);
	g_test_add("/secret_collection/secret_collection_token_item_remove", int, NULL, setup_secret_collection, test_secret_collection_token_item_remove, teardown_secret_collection);
	g_test_add("/secret_item/secret_item_new", int, NULL, setup_secret_item, test_secret_item_new, teardown_secret_item);
	g_test_add("/secret_item/secret_item_create", int, NULL, setup_secret_item, test_secret_item_create, teardown_secret_item);
	g_test_add("/secret_item/secret_item_create_failed", int, NULL, setup_secret_item, test_secret_item_create_failed, teardown_secret_item);
	g_test_add("/secret_item/secret_item_destroy", int, NULL, setup_secret_item, test_secret_item_destroy, teardown_secret_item);
	g_test_add("/secret_item/secret_item_destroy_failed", int, NULL, setup_secret_item, test_secret_item_destroy_failed, teardown_secret_item);
	g_test_add("/secret_item/secret_item_collection_get", int, NULL, setup_secret_item, test_secret_item_collection_get, teardown_secret_item);
	g_test_add("/secret_item/secret_item_collection_items", int, NULL, setup_secret_item, test_secret_item_collection_items, teardown_secret_item);
	g_test_add("/secret_item/secret_item_collection_remove", int, NULL, setup_secret_item, test_secret_item_collection_remove, teardown_secret_item);
	g_test_add("/secret_item/secret_item_is_locked", int, NULL, setup_secret_item, test_secret_item_is_locked, teardown_secret_item);
	g_test_add("/secret_item/secret_item_get_collection", int, NULL, setup_secret_item, test_secret_item_get_collection, teardown_secret_item);
	g_test_add("/secret_item/secret_item_tracks_collection", int, NULL, setup_secret_item, test_secret_item_tracks_collection, teardown_secret_item);
	g_test_add("/secret_item/secret_item_get_set_fields", int, NULL, setup_secret_item, test_secret_item_get_set_fields, teardown_secret_item);
	g_test_add("/secret_item/secret_item_collection_attr", int, NULL, setup_secret_item, test_secret_item_collection_attr, teardown_secret_item);
	g_test_add("/secret_item/secret_item_secret_attr", int, NULL, setup_secret_item, test_secret_item_secret_attr, teardown_secret_item);
	g_test_add("/secret_item/secret_item_secret_attr_locked", int, NULL, setup_secret_item, test_secret_item_secret_attr_locked, teardown_secret_item);
	g_test_add("/secret_item/secret_item_fields_attr", int, NULL, setup_secret_item, test_secret_item_fields_attr, teardown_secret_item);
	g_test_add("/secret_item/secret_item_fields_attr_locked", int, NULL, setup_secret_item, test_secret_item_fields_attr_locked, teardown_secret_item);
	g_test_add("/secret_item/secret_item_fields_attr_reverts", int, NULL, setup_secret_item, test_secret_item_fields_attr_reverts, teardown_secret_item);
	g_test_add("/secret_search/create_search_incomplete", int, NULL, setup_secret_search, test_create_search_incomplete, teardown_secret_search);
	g_test_add("/secret_search/create_search_bad_fields", int, NULL, setup_secret_search, test_create_search_bad_fields, teardown_secret_search);
	g_test_add("/secret_search/create_search", int, NULL, setup_secret_search, test_create_search, teardown_secret_search);
	g_test_add("/secret_search/create_search_and_match", int, NULL, setup_secret_search, test_create_search_and_match, teardown_secret_search);
	g_test_add("/secret_search/create_search_and_change_to_match", int, NULL, setup_secret_search, test_create_search_and_change_to_match, teardown_secret_search);
	g_test_add("/secret_search/create_search_and_change_to_not_match", int, NULL, setup_secret_search, test_create_search_and_change_to_not_match, teardown_secret_search);
	g_test_add("/secret_search/create_search_for_bad_collection", int, NULL, setup_secret_search, test_create_search_for_bad_collection, teardown_secret_search);
	g_test_add("/secret_search/create_search_for_collection", int, NULL, setup_secret_search, test_create_search_for_collection, teardown_secret_search);
	g_test_add("/secret_search/create_search_for_collection_no_match", int, NULL, setup_secret_search, test_create_search_for_collection_no_match, teardown_secret_search);
	g_test_add("/secret_textual/textual_read", int, NULL, setup_textual, test_textual_read, teardown_textual);
	g_test_add("/secret_textual/textual_read_wrong_format", int, NULL, setup_textual, test_textual_read_wrong_format, teardown_textual);
	g_test_add("/secret_textual/textual_read_bad_number", int, NULL, setup_textual, test_textual_read_bad_number, teardown_textual);
	g_test_add("/secret_textual/textual_write", int, NULL, setup_textual, test_textual_write, teardown_textual);
	g_test_add("/secret_textual/textual_remove_unavailable", int, NULL, setup_textual, test_textual_remove_unavailable, teardown_textual);
	g_test_add("/secret_binary/binary_read", int, NULL, setup_binary, test_binary_read, teardown_binary);
	g_test_add("/secret_binary/binary_read_wrong_format", int, NULL, setup_binary, test_binary_read_wrong_format, teardown_binary);
	g_test_add("/secret_binary/binary_read_wrong_master", int, NULL, setup_binary, test_binary_read_wrong_master, teardown_binary);
	g_test_add("/secret_binary/binary_read_sdata_but_no_master", int, NULL, setup_binary, test_binary_read_sdata_but_no_master, teardown_binary);
	g_test_add("/secret_binary/binary_write", int, NULL, setup_binary, test_binary_write, teardown_binary);
	g_test_add("/secret_binary/binary_remove_unavailable", int, NULL, setup_binary, test_binary_remove_unavailable, teardown_binary);
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
