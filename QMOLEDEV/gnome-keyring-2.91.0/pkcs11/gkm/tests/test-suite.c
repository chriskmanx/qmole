/* This is auto-generated code. Edit at your own peril. */
#include "testing/testing.h"
#include "test-suite.h"

static void start_tests (void) {
}

static void stop_tests (void) {
}

static void initialize_tests (void) {
	g_test_add("/attributes/attribute_equal_zero_len_null_ptr", int, NULL, NULL, test_attribute_equal_zero_len_null_ptr, NULL);
	g_test_add("/attributes/attribute_consume", int, NULL, NULL, test_attribute_consume, NULL);
	g_test_add("/attributes/attribute_consumed", int, NULL, NULL, test_attribute_consumed, NULL);
	g_test_add("/attributes/attribute_set_data", int, NULL, NULL, test_attribute_set_data, NULL);
	g_test_add("/attributes/attribute_set_data_short", int, NULL, NULL, test_attribute_set_data_short, NULL);
	g_test_add("/attributes/attribute_set_data_length", int, NULL, NULL, test_attribute_set_data_length, NULL);
	g_test_add("/attributes/attribute_set_empty", int, NULL, NULL, test_attribute_set_empty, NULL);
	g_test_add("/attributes/attribute_get_bool", int, NULL, NULL, test_attribute_get_bool, NULL);
	g_test_add("/attributes/attribute_get_bool_invalid", int, NULL, NULL, test_attribute_get_bool_invalid, NULL);
	g_test_add("/attributes/attribute_set_time", int, NULL, NULL, test_attribute_set_time, NULL);
	g_test_add("/attributes/attribute_set_time_empty", int, NULL, NULL, test_attribute_set_time_empty, NULL);
	g_test_add("/attributes/attribute_set_time_length", int, NULL, NULL, test_attribute_set_time_length, NULL);
	g_test_add("/attributes/attribute_get_time", int, NULL, NULL, test_attribute_get_time, NULL);
	g_test_add("/attributes/attribute_get_time_empty", int, NULL, NULL, test_attribute_get_time_empty, NULL);
	g_test_add("/attributes/attribute_get_time_invalid", int, NULL, NULL, test_attribute_get_time_invalid, NULL);
	g_test_add("/attributes/attribute_get_time_invalid_length", int, NULL, NULL, test_attribute_get_time_invalid_length, NULL);
	g_test_add("/attributes/attribute_get_string", int, NULL, NULL, test_attribute_get_string, NULL);
	g_test_add("/attributes/attribute_get_string_null", int, NULL, NULL, test_attribute_get_string_null, NULL);
	g_test_add("/attributes/attribute_get_string_not_utf8", int, NULL, NULL, test_attribute_get_string_not_utf8, NULL);
	g_test_add("/attributes/attribute_get_string_bad_pointer", int, NULL, NULL, test_attribute_get_string_bad_pointer, NULL);
	g_test_add("/attributes/attribute_set_bool", int, NULL, NULL, test_attribute_set_bool, NULL);
	g_test_add("/attributes/attribute_set_bool_short", int, NULL, NULL, test_attribute_set_bool_short, NULL);
	g_test_add("/attributes/attribute_set_bool_length", int, NULL, NULL, test_attribute_set_bool_length, NULL);
	g_test_add("/attributes/attribute_set_ulong", int, NULL, NULL, test_attribute_set_ulong, NULL);
	g_test_add("/attributes/attribute_set_ulong_short", int, NULL, NULL, test_attribute_set_ulong_short, NULL);
	g_test_add("/attributes/attribute_set_ulong_length", int, NULL, NULL, test_attribute_set_ulong_length, NULL);
	g_test_add("/attributes/attribute_set_string", int, NULL, NULL, test_attribute_set_string, NULL);
	g_test_add("/attributes/attribute_set_string_null", int, NULL, NULL, test_attribute_set_string_null, NULL);
	g_test_add("/attributes/attribute_set_string_short", int, NULL, NULL, test_attribute_set_string_short, NULL);
	g_test_add("/attributes/attribute_set_string_length", int, NULL, NULL, test_attribute_set_string_length, NULL);
	g_test_add("/attributes/attribute_set_date", int, NULL, NULL, test_attribute_set_date, NULL);
	g_test_add("/attributes/attribute_set_date_none", int, NULL, NULL, test_attribute_set_date_none, NULL);
	g_test_add("/attributes/attribute_set_date_short", int, NULL, NULL, test_attribute_set_date_short, NULL);
	g_test_add("/attributes/attribute_set_date_length", int, NULL, NULL, test_attribute_set_date_length, NULL);
	g_test_add("/attributes/attribute_set_mpi", int, NULL, NULL, test_attribute_set_mpi, NULL);
	g_test_add("/attributes/attribute_set_mpi_short", int, NULL, NULL, test_attribute_set_mpi_short, NULL);
	g_test_add("/attributes/attribute_set_mpi_length", int, NULL, NULL, test_attribute_set_mpi_length, NULL);
	g_test_add("/attributes/attribute_equal", int, NULL, NULL, test_attribute_equal, NULL);
	g_test_add("/attributes/attribute_equal_same", int, NULL, NULL, test_attribute_equal_same, NULL);
	g_test_add("/attributes/attribute_equal_same_pointer", int, NULL, NULL, test_attribute_equal_same_pointer, NULL);
	g_test_add("/attributes/attribute_equal_diff_types", int, NULL, NULL, test_attribute_equal_diff_types, NULL);
	g_test_add("/attributes/attribute_equal_diff_length", int, NULL, NULL, test_attribute_equal_diff_length, NULL);
	g_test_add("/attributes/attribute_equal_diff_value", int, NULL, NULL, test_attribute_equal_diff_value, NULL);
	g_test_add("/attributes/attribute_hash", int, NULL, NULL, test_attribute_hash, NULL);
	g_test_add("/attributes/attribute_contains", int, NULL, NULL, test_attribute_contains, NULL);
	g_test_add("/attributes/attribute_contains_no_value", int, NULL, NULL, test_attribute_contains_no_value, NULL);
	g_test_add("/attributes/attribute_contains_no_type", int, NULL, NULL, test_attribute_contains_no_type, NULL);
	g_test_add("/attributes/attributes_find", int, NULL, NULL, test_attributes_find, NULL);
	g_test_add("/attributes/attributes_find_not_found", int, NULL, NULL, test_attributes_find_not_found, NULL);
	g_test_add("/attributes/attribute_find_boolean", int, NULL, NULL, test_attribute_find_boolean, NULL);
	g_test_add("/attributes/attribute_find_boolean_no_type", int, NULL, NULL, test_attribute_find_boolean_no_type, NULL);
	g_test_add("/attributes/attribute_find_boolean_not_bbool", int, NULL, NULL, test_attribute_find_boolean_not_bbool, NULL);
	g_test_add("/attributes/attribute_find_ulong", int, NULL, NULL, test_attribute_find_ulong, NULL);
	g_test_add("/attributes/attribute_find_ulong_no_type", int, NULL, NULL, test_attribute_find_ulong_no_type, NULL);
	g_test_add("/attributes/attribute_find_ulong_not_ulong", int, NULL, NULL, test_attribute_find_ulong_not_ulong, NULL);
	g_test_add("/attributes/attribute_find_mpi", int, NULL, NULL, test_attribute_find_mpi, NULL);
	g_test_add("/attributes/attribute_find_mpi_no_type", int, NULL, NULL, test_attribute_find_mpi_no_type, NULL);
	g_test_add("/attributes/attributes_consume", int, NULL, NULL, test_attributes_consume, NULL);
	g_test_add("/attributes/template_new_free", int, NULL, NULL, test_template_new_free, NULL);
	g_test_add("/attributes/template_find", int, NULL, NULL, test_template_find, NULL);
	g_test_add("/attributes/template_set_replace", int, NULL, NULL, test_template_set_replace, NULL);
	g_test_add("/sexp/parse_key", int, NULL, setup_crypto_setup, test_parse_key, teardown_crypto_setup);
	g_test_add("/sexp/sexp_key_to_public", int, NULL, setup_crypto_setup, test_sexp_key_to_public, teardown_crypto_setup);
	g_test_add("/data_asn1/asn1_integers", int, NULL, setup_asn1_tree, test_asn1_integers, teardown_asn1_tree);
	g_test_add("/data_der/der_rsa_public", int, NULL, setup_preload, test_der_rsa_public, teardown_preload);
	g_test_add("/data_der/der_dsa_public", int, NULL, setup_preload, test_der_dsa_public, teardown_preload);
	g_test_add("/data_der/der_rsa_private", int, NULL, setup_preload, test_der_rsa_private, teardown_preload);
	g_test_add("/data_der/der_dsa_private", int, NULL, setup_preload, test_der_dsa_private, teardown_preload);
	g_test_add("/data_der/der_dsa_private_parts", int, NULL, setup_preload, test_der_dsa_private_parts, teardown_preload);
	g_test_add("/data_der/read_public_key_info", int, NULL, setup_preload, test_read_public_key_info, teardown_preload);
	g_test_add("/data_der/read_certificate", int, NULL, setup_preload, test_read_certificate, teardown_preload);
	g_test_add("/data_der/write_certificate", int, NULL, setup_preload, test_write_certificate, teardown_preload);
	g_test_add("/data_der/read_ca_certificates_public_key_info", int, NULL, setup_preload, test_read_ca_certificates_public_key_info, teardown_preload);
	g_test_add("/data_der/read_basic_constraints", int, NULL, setup_preload, test_read_basic_constraints, teardown_preload);
	g_test_add("/data_der/read_key_usage", int, NULL, setup_preload, test_read_key_usage, teardown_preload);
	g_test_add("/data_der/read_enhanced_usage", int, NULL, setup_preload, test_read_enhanced_usage, teardown_preload);
	g_test_add("/data_der/read_all_pkcs8", int, NULL, setup_preload, test_read_all_pkcs8, teardown_preload);
	g_test_add("/data_der/read_pkcs8_bad_password", int, NULL, setup_preload, test_read_pkcs8_bad_password, teardown_preload);
	g_test_add("/data_der/write_pkcs8_plain", int, NULL, setup_preload, test_write_pkcs8_plain, teardown_preload);
	g_test_add("/data_der/write_pkcs8_encrypted", int, NULL, setup_preload, test_write_pkcs8_encrypted, teardown_preload);
	g_test_add("/object/object_create_destroy_transient", int, NULL, setup_object_setup, test_object_create_destroy_transient, teardown_object_teardown);
	g_test_add("/object/object_transient_transacted_fail", int, NULL, setup_object_setup, test_object_transient_transacted_fail, teardown_object_teardown);
	g_test_add("/object/object_create_transient_bad_value", int, NULL, setup_object_setup, test_object_create_transient_bad_value, teardown_object_teardown);
	g_test_add("/object/object_create_auto_destruct", int, NULL, setup_object_setup, test_object_create_auto_destruct, teardown_object_teardown);
	g_test_add("/object/object_create_auto_destruct_not_transient", int, NULL, setup_object_setup, test_object_create_auto_destruct_not_transient, teardown_object_teardown);
	g_test_add("/object/object_expose", int, NULL, setup_object_setup, test_object_expose, teardown_object_teardown);
	g_test_add("/object/object_expose_transaction", int, NULL, setup_object_setup, test_object_expose_transaction, teardown_object_teardown);
	g_test_add("/credential/credential_create", int, NULL, setup_credential_setup, test_credential_create, teardown_credential_teardown);
	g_test_add("/credential/credential_create_missing_pin", int, NULL, setup_credential_setup, test_credential_create_missing_pin, teardown_credential_teardown);
	g_test_add("/credential/credential_create_no_object", int, NULL, setup_credential_setup, test_credential_create_no_object, teardown_credential_teardown);
	g_test_add("/credential/credential_create_invalid_object", int, NULL, setup_credential_setup, test_credential_create_invalid_object, teardown_credential_teardown);
	g_test_add("/credential/credential_get_attributes", int, NULL, setup_credential_setup, test_credential_get_attributes, teardown_credential_teardown);
	g_test_add("/credential/credential_object_property", int, NULL, setup_credential_setup, test_credential_object_property, teardown_credential_teardown);
	g_test_add("/credential/credential_login_property", int, NULL, setup_credential_setup, test_credential_login_property, teardown_credential_teardown);
	g_test_add("/credential/credential_data", int, NULL, setup_credential_setup, test_credential_data, teardown_credential_teardown);
	g_test_add("/credential/credential_connect_object", int, NULL, setup_credential_setup, test_credential_connect_object, teardown_credential_teardown);
	g_test_add("/timer/timer_extra_initialize", int, NULL, setup_timer_setup, test_timer_extra_initialize, teardown_timer_teardown);
	g_test_add("/timer/timer_simple", int, NULL, setup_timer_setup, test_timer_simple, teardown_timer_teardown);
	g_test_add("/timer/timer_cancel", int, NULL, setup_timer_setup, test_timer_cancel, teardown_timer_teardown);
	g_test_add("/timer/timer_immediate", int, NULL, setup_timer_setup, test_timer_immediate, teardown_timer_teardown);
	g_test_add("/timer/timer_multiple", int, NULL, setup_timer_setup, test_timer_multiple, teardown_timer_teardown);
	g_test_add("/timer/timer_outstanding", int, NULL, setup_timer_setup, test_timer_outstanding, teardown_timer_teardown);
	g_test_add("/transaction/transaction_empty", int, NULL, NULL, test_transaction_empty, NULL);
	g_test_add("/transaction/transaction_fail", int, NULL, NULL, test_transaction_fail, NULL);
	g_test_add("/transaction/transaction_signals_success", int, NULL, NULL, test_transaction_signals_success, NULL);
	g_test_add("/transaction/transaction_signals_failure", int, NULL, NULL, test_transaction_signals_failure, NULL);
	g_test_add("/transaction/transaction_order_is_reverse", int, NULL, NULL, test_transaction_order_is_reverse, NULL);
	g_test_add("/transaction/transaction_dispose_completes", int, NULL, NULL, test_transaction_dispose_completes, NULL);
	g_test_add("/transaction/remove_file_success", int, NULL, NULL, test_remove_file_success, NULL);
	g_test_add("/transaction/remove_file_abort", int, NULL, NULL, test_remove_file_abort, NULL);
	g_test_add("/transaction/remove_file_non_exist", int, NULL, NULL, test_remove_file_non_exist, NULL);
	g_test_add("/transaction/write_file", int, NULL, NULL, test_write_file, NULL);
	g_test_add("/transaction/write_file_abort_gone", int, NULL, NULL, test_write_file_abort_gone, NULL);
	g_test_add("/transaction/write_file_abort_revert", int, NULL, NULL, test_write_file_abort_revert, NULL);
	g_test_add("/store/store_schema", int, NULL, setup_store, test_store_schema, teardown_store);
	g_test_add("/store/store_schema_flags", int, NULL, setup_store, test_store_schema_flags, teardown_store);
	g_test_add("/memory_store/get_attribute_default", int, NULL, setup_memory_store, test_get_attribute_default, teardown_memory_store);
	g_test_add("/memory_store/read_value_default", int, NULL, setup_memory_store, test_read_value_default, teardown_memory_store);
	g_test_add("/memory_store/read_string", int, NULL, setup_memory_store, test_read_string, teardown_memory_store);
	g_test_add("/memory_store/get_invalid", int, NULL, setup_memory_store, test_get_invalid, teardown_memory_store);
	g_test_add("/memory_store/get_sensitive", int, NULL, setup_memory_store, test_get_sensitive, teardown_memory_store);
	g_test_add("/memory_store/get_internal", int, NULL, setup_memory_store, test_get_internal, teardown_memory_store);
	g_test_add("/memory_store/set_invalid", int, NULL, setup_memory_store, test_set_invalid, teardown_memory_store);
	g_test_add("/memory_store/set_internal", int, NULL, setup_memory_store, test_set_internal, teardown_memory_store);
	g_test_add("/memory_store/set_get_attribute", int, NULL, setup_memory_store, test_set_get_attribute, teardown_memory_store);
	g_test_add("/memory_store/write_read_value", int, NULL, setup_memory_store, test_write_read_value, teardown_memory_store);
	g_test_add("/memory_store/set_no_validate", int, NULL, setup_memory_store, test_set_no_validate, teardown_memory_store);
	g_test_add("/memory_store/set_transaction_default", int, NULL, setup_memory_store, test_set_transaction_default, teardown_memory_store);
	g_test_add("/memory_store/set_transaction_revert_first", int, NULL, setup_memory_store, test_set_transaction_revert_first, teardown_memory_store);
	g_test_add("/memory_store/set_notifies", int, NULL, setup_memory_store, test_set_notifies, teardown_memory_store);
	g_test_add("/memory_store/set_object_gone_first", int, NULL, setup_memory_store, test_set_object_gone_first, teardown_memory_store);
	g_test_add("/secret/test_secret", int, NULL, NULL, test_test_secret, NULL);
	g_test_add("/secret/test_secret_from_login", int, NULL, NULL, test_test_secret_from_login, NULL);
	g_test_add("/secret/test_null_terminated", int, NULL, NULL, test_test_null_terminated, NULL);
	g_test_add("/secret/test_always_has_null", int, NULL, NULL, test_test_always_has_null, NULL);
	g_test_add("/secret/test_null", int, NULL, NULL, test_test_null, NULL);
	g_test_add("/secret/test_empty", int, NULL, NULL, test_test_empty, NULL);
	g_test_add("/secret/test_equal", int, NULL, NULL, test_test_equal, NULL);
	g_test_add("/data_file/test_file_create", int, NULL, setup_file_store, test_test_file_create, teardown_file_store);
	g_test_add("/data_file/test_file_write_value", int, NULL, setup_file_store, test_test_file_write_value, teardown_file_store);
	g_test_add("/data_file/test_file_read_value", int, NULL, setup_file_store, test_test_file_read_value, teardown_file_store);
	g_test_add("/data_file/test_file_read", int, NULL, setup_file_store, test_test_file_read, teardown_file_store);
	g_test_add("/data_file/test_file_lookup", int, NULL, setup_file_store, test_test_file_lookup, teardown_file_store);
	g_test_add("/data_file/file_read_private_without_login", int, NULL, setup_file_store, test_file_read_private_without_login, teardown_file_store);
	g_test_add("/data_file/test_file_write", int, NULL, setup_file_store, test_test_file_write, teardown_file_store);
	g_test_add("/data_file/cant_write_private_without_login", int, NULL, setup_file_store, test_cant_write_private_without_login, teardown_file_store);
	g_test_add("/data_file/write_private_with_login", int, NULL, setup_file_store, test_write_private_with_login, teardown_file_store);
	g_test_add("/data_file/read_private_with_login", int, NULL, setup_file_store, test_read_private_with_login, teardown_file_store);
	g_test_add("/data_file/destroy_entry", int, NULL, setup_file_store, test_destroy_entry, teardown_file_store);
	g_test_add("/data_file/destroy_entry_by_loading", int, NULL, setup_file_store, test_destroy_entry_by_loading, teardown_file_store);
	g_test_add("/data_file/destroy_private_without_login", int, NULL, setup_file_store, test_destroy_private_without_login, teardown_file_store);
	g_test_add("/data_file/entry_added_signal", int, NULL, setup_file_store, test_entry_added_signal, teardown_file_store);
	g_test_add("/data_file/entry_changed_signal", int, NULL, setup_file_store, test_entry_changed_signal, teardown_file_store);
	g_test_add("/data_file/entry_removed_signal", int, NULL, setup_file_store, test_entry_removed_signal, teardown_file_store);
	g_test_add("/data_file/data_file_foreach", int, NULL, setup_file_store, test_data_file_foreach, teardown_file_store);
	g_test_add("/data_file/unique_entry", int, NULL, setup_file_store, test_unique_entry, teardown_file_store);
	g_test_add("/data_file/have_sections", int, NULL, setup_file_store, test_have_sections, teardown_file_store);
	g_test_add("/file_tracker/file_watch", int, NULL, setup_tracker, test_file_watch, teardown_tracker);
	g_test_add("/file_tracker/watch_file", int, NULL, setup_tracker, test_watch_file, teardown_tracker);
	g_test_add("/file_tracker/nomatch", int, NULL, setup_tracker, test_nomatch, teardown_tracker);
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
