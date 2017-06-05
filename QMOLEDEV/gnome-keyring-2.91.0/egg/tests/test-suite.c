/* This is auto-generated code. Edit at your own peril. */
#include "testing/testing.h"
#include "test-suite.h"

static void start_tests (void) {
}

static void stop_tests (void) {
}

static void initialize_tests (void) {
	g_test_add("/asn1/asn1_boolean", int, NULL, NULL, test_asn1_boolean, NULL);
	g_test_add("/asn1/asn1_integer", int, NULL, NULL, test_asn1_integer, NULL);
	g_test_add("/asn1/asn1_octet_string", int, NULL, NULL, test_asn1_octet_string, NULL);
	g_test_add("/asn1/asn1_generalized_time", int, NULL, NULL, test_asn1_generalized_time, NULL);
	g_test_add("/asn1/asn1_implicit", int, NULL, NULL, test_asn1_implicit, NULL);
	g_test_add("/asn1/asn1_explicit", int, NULL, NULL, test_asn1_explicit, NULL);
	g_test_add("/asn1/asn1_bit_string_decode", int, NULL, NULL, test_asn1_bit_string_decode, NULL);
	g_test_add("/asn1/asn1_bit_string_decode_bad", int, NULL, NULL, test_asn1_bit_string_decode_bad, NULL);
	g_test_add("/asn1/asn1_bit_string_decode_ulong", int, NULL, NULL, test_asn1_bit_string_decode_ulong, NULL);
	g_test_add("/asn1/asn1_bit_string_encode_decode", int, NULL, NULL, test_asn1_bit_string_encode_decode, NULL);
	g_test_add("/asn1/asn1_bit_string_encode_decode_ulong", int, NULL, NULL, test_asn1_bit_string_encode_decode_ulong, NULL);
	g_test_add("/asn1/asn1_bit_string_encode_decode_zero", int, NULL, NULL, test_asn1_bit_string_encode_decode_zero, NULL);
	g_test_add("/asn1/asn1_have", int, NULL, NULL, test_asn1_have, NULL);
	g_test_add("/asn1/asn1_any_set_raw", int, NULL, NULL, test_asn1_any_set_raw, NULL);
	g_test_add("/asn1/node_name", int, NULL, setup_asn1_tree, test_node_name, teardown_asn1_tree);
	g_test_add("/asn1/asn1_integers", int, NULL, setup_asn1_tree, test_asn1_integers, teardown_asn1_tree);
	g_test_add("/asn1/boolean", int, NULL, setup_asn1_tree, test_boolean, teardown_asn1_tree);
	g_test_add("/asn1/write_value", int, NULL, setup_asn1_tree, test_write_value, teardown_asn1_tree);
	g_test_add("/asn1/element_length_content", int, NULL, setup_asn1_tree, test_element_length_content, teardown_asn1_tree);
	g_test_add("/asn1/read_element", int, NULL, setup_asn1_tree, test_read_element, teardown_asn1_tree);
	g_test_add("/asn1/oid", int, NULL, setup_asn1_tree, test_oid, teardown_asn1_tree);
	g_test_add("/asn1/general_time", int, NULL, setup_asn1_tree, test_general_time, teardown_asn1_tree);
	g_test_add("/asn1/utc_time", int, NULL, setup_asn1_tree, test_utc_time, teardown_asn1_tree);
	g_test_add("/asn1/read_time", int, NULL, setup_asn1_tree, test_read_time, teardown_asn1_tree);
	g_test_add("/asn1/read_date", int, NULL, setup_asn1_tree, test_read_date, teardown_asn1_tree);
	g_test_add("/asn1/create_by_oid", int, NULL, setup_asn1_tree, test_create_by_oid, teardown_asn1_tree);
	g_test_add("/asn1/create_by_oid_invalid", int, NULL, setup_asn1_tree, test_create_by_oid_invalid, teardown_asn1_tree);
	g_test_add("/asn1/create_by_bad_order", int, NULL, setup_asn1_tree, test_create_by_bad_order, teardown_asn1_tree);
	g_test_add("/asn1/count", int, NULL, setup_asn1_tree, test_count, teardown_asn1_tree);
	g_test_add("/dn/read_dn", int, NULL, setup_dn_cert, test_read_dn, teardown_dn_cert);
	g_test_add("/dn/dn_value", int, NULL, setup_dn_cert, test_dn_value, teardown_dn_cert);
	g_test_add("/dn/parse_dn", int, NULL, setup_dn_cert, test_parse_dn, teardown_dn_cert);
	g_test_add("/dn/read_dn_part", int, NULL, setup_dn_cert, test_read_dn_part, teardown_dn_cert);
	g_test_add("/cleanup/cleanup", int, NULL, NULL, test_cleanup, NULL);
	g_test_add("/cleanup/order", int, NULL, NULL, test_order, NULL);
	g_test_add("/cleanup/reregister", int, NULL, NULL, test_reregister, NULL);
	g_test_add("/cleanup/remove", int, NULL, NULL, test_remove, NULL);
	g_test_add("/hex/hex_encode", int, NULL, NULL, test_hex_encode, NULL);
	g_test_add("/hex/hex_encode_spaces", int, NULL, NULL, test_hex_encode_spaces, NULL);
	g_test_add("/hex/hex_decode", int, NULL, NULL, test_hex_decode, NULL);
	g_test_add("/hex/hex_decode_fail", int, NULL, NULL, test_hex_decode_fail, NULL);
	g_test_add("/oid/oid_tests", int, NULL, NULL, test_oid_tests, NULL);
	g_test_add("/padding/zero_padding", int, NULL, NULL, test_zero_padding, NULL);
	g_test_add("/padding/zero_padding_no_data", int, NULL, NULL, test_zero_padding_no_data, NULL);
	g_test_add("/padding/pkcs1_one_padding", int, NULL, NULL, test_pkcs1_one_padding, NULL);
	g_test_add("/padding/pkcs1_one_padding_no_data", int, NULL, NULL, test_pkcs1_one_padding_no_data, NULL);
	g_test_add("/padding/pkcs1_two_padding", int, NULL, NULL, test_pkcs1_two_padding, NULL);
	g_test_add("/padding/pkcs1_padding_invalid_prefix", int, NULL, NULL, test_pkcs1_padding_invalid_prefix, NULL);
	g_test_add("/padding/pkcs1_padding_invalid_type", int, NULL, NULL, test_pkcs1_padding_invalid_type, NULL);
	g_test_add("/padding/pkcs1_padding_invalid_no_zero", int, NULL, NULL, test_pkcs1_padding_invalid_no_zero, NULL);
	g_test_add("/padding/pkcs1_padding_invalid_length", int, NULL, NULL, test_pkcs1_padding_invalid_length, NULL);
	g_test_add("/padding/pkcs7_padding", int, NULL, NULL, test_pkcs7_padding, NULL);
	g_test_add("/padding/pkcs7_padding_equal_block", int, NULL, NULL, test_pkcs7_padding_equal_block, NULL);
	g_test_add("/padding/pkcs7_padding_zero", int, NULL, NULL, test_pkcs7_padding_zero, NULL);
	g_test_add("/padding/pkcs7_padding_invalid_zero", int, NULL, NULL, test_pkcs7_padding_invalid_zero, NULL);
	g_test_add("/padding/pkcs7_padding_invalid_too_long", int, NULL, NULL, test_pkcs7_padding_invalid_too_long, NULL);
	g_test_add("/padding/pkcs7_padding_invalid_different", int, NULL, NULL, test_pkcs7_padding_invalid_different, NULL);
	g_test_add("/secmem/secmem_alloc_free", int, NULL, NULL, test_secmem_alloc_free, NULL);
	g_test_add("/secmem/secmem_realloc_across", int, NULL, NULL, test_secmem_realloc_across, NULL);
	g_test_add("/secmem/secmem_alloc_two", int, NULL, NULL, test_secmem_alloc_two, NULL);
	g_test_add("/secmem/secmem_realloc", int, NULL, NULL, test_secmem_realloc, NULL);
	g_test_add("/secmem/secmem_multialloc", int, NULL, NULL, test_secmem_multialloc, NULL);
	g_test_add("/secmem/secmem_clear", int, NULL, NULL, test_secmem_clear, NULL);
	g_test_add("/secmem/secmem_strclear", int, NULL, NULL, test_secmem_strclear, NULL);
	g_test_add("/symkey/generate_key_simple", int, NULL, setup_crypto_setup, test_generate_key_simple, teardown_crypto_setup);
	g_test_add("/symkey/generate_key_pkcs12", int, NULL, setup_crypto_setup, test_generate_key_pkcs12, teardown_crypto_setup);
	g_test_add("/symkey/generate_key_pbkdf2", int, NULL, setup_crypto_setup, test_generate_key_pbkdf2, teardown_crypto_setup);
	g_test_add("/symkey/generate_key_pbe", int, NULL, setup_crypto_setup, test_generate_key_pbe, teardown_crypto_setup);
	g_test_add("/openssl/parse_reference", int, NULL, NULL, test_parse_reference, NULL);
	g_test_add("/openssl/write_reference", int, NULL, NULL, test_write_reference, NULL);
	g_test_add("/openssl/openssl_roundtrip", int, NULL, NULL, test_openssl_roundtrip, NULL);
	g_test_add("/dh/dh_perform", int, NULL, NULL, test_dh_perform, NULL);
	g_test_add("/dh/dh_short_pair", int, NULL, NULL, test_dh_short_pair, NULL);
	g_test_add("/dh/dh_default_768", int, NULL, NULL, test_dh_default_768, NULL);
	g_test_add("/dh/dh_default_1024", int, NULL, NULL, test_dh_default_1024, NULL);
	g_test_add("/dh/dh_default_1536", int, NULL, NULL, test_dh_default_1536, NULL);
	g_test_add("/dh/dh_default_2048", int, NULL, NULL, test_dh_default_2048, NULL);
	g_test_add("/dh/dh_default_3072", int, NULL, NULL, test_dh_default_3072, NULL);
	g_test_add("/dh/dh_default_4096", int, NULL, NULL, test_dh_default_4096, NULL);
	g_test_add("/dh/dh_default_8192", int, NULL, NULL, test_dh_default_8192, NULL);
	g_test_add("/dh/dh_default_bad", int, NULL, NULL, test_dh_default_bad, NULL);
	g_test_add("/spawn/test_spawn_sync", int, NULL, NULL, test_test_spawn_sync, NULL);
	g_test_add("/spawn/test_spawn_sync_error", int, NULL, NULL, test_test_spawn_sync_error, NULL);
	g_test_add("/spawn/test_spawn_async", int, NULL, NULL, test_test_spawn_async, NULL);
	g_test_add("/spawn/test_spawn_async_none", int, NULL, NULL, test_test_spawn_async_none, NULL);
	g_test_add("/spawn/test_spawn_async_error", int, NULL, NULL, test_test_spawn_async_error, NULL);
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
