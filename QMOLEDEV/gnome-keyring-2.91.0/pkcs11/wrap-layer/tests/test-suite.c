/* This is auto-generated code. Edit at your own peril. */
#include "testing/testing.h"
#include "test-suite.h"

static void start_tests (void) {
}

static void stop_tests (void) {
}

static void initialize_tests (void) {
	g_test_add("/create_credential/create_credential_ok_password", int, NULL, setup_create_credential, test_create_credential_ok_password, teardown_create_credential);
	g_test_add("/create_credential/create_credential_bad_password_then_cancel", int, NULL, setup_create_credential, test_create_credential_bad_password_then_cancel, teardown_create_credential);
	g_test_add("/create_credential/create_credentiaol_cancel_immediately", int, NULL, setup_create_credential, test_create_credentiaol_cancel_immediately, teardown_create_credential);
	g_test_add("/init_pin/init_pin_ok_password", int, NULL, setup_init_pin, test_init_pin_ok_password, teardown_init_pin);
	g_test_add("/login_auto/login_auto_specific", int, NULL, setup_login_auto, test_login_auto_specific, teardown_login_auto);
	g_test_add("/login_auto/login_auto_user_token", int, NULL, setup_login_auto, test_login_auto_user_token, teardown_login_auto);
	g_test_add("/login_auto/login_auto_unlock_keyring", int, NULL, setup_login_auto, test_login_auto_unlock_keyring, teardown_login_auto);
	g_test_add("/login_hints/login_did_unlock_fail", int, NULL, NULL, test_login_did_unlock_fail, NULL);
	g_test_add("/login_keyring/login_is_usable", int, NULL, setup_login_keyring, test_login_is_usable, teardown_login_keyring);
	g_test_add("/login_keyring/login_usable_fail_open_session", int, NULL, setup_login_keyring, test_login_usable_fail_open_session, teardown_login_keyring);
	g_test_add("/login_keyring/login_usable_fail_not_trusted", int, NULL, setup_login_keyring, test_login_usable_fail_not_trusted, teardown_login_keyring);
	g_test_add("/login_keyring/login_usable_fail_locked", int, NULL, setup_login_keyring, test_login_usable_fail_locked, teardown_login_keyring);
	g_test_add("/login_keyring/login_lookup_secret_no_match", int, NULL, setup_login_keyring, test_login_lookup_secret_no_match, teardown_login_keyring);
	g_test_add("/login_keyring/login_lookup_secret_and_match", int, NULL, setup_login_keyring, test_login_lookup_secret_and_match, teardown_login_keyring);
	g_test_add("/login_keyring/login_lookup_store_secret", int, NULL, setup_login_keyring, test_login_lookup_store_secret, teardown_login_keyring);
	g_test_add("/login_keyring/login_lookup_store_secret_overwrite", int, NULL, setup_login_keyring, test_login_lookup_store_secret_overwrite, teardown_login_keyring);
	g_test_add("/login_keyring/login_lookup_store_null_secret", int, NULL, setup_login_keyring, test_login_lookup_store_null_secret, teardown_login_keyring);
	g_test_add("/login_keyring/login_lookup_store_no_attributes_not_stored", int, NULL, setup_login_keyring, test_login_lookup_store_no_attributes_not_stored, teardown_login_keyring);
	g_test_add("/login_keyring/login_lookup_remove_present", int, NULL, setup_login_keyring, test_login_lookup_remove_present, teardown_login_keyring);
	g_test_add("/login_keyring/login_lookup_remove_no_attributes", int, NULL, setup_login_keyring, test_login_lookup_remove_no_attributes, teardown_login_keyring);
	g_test_add("/login_specific/login_specific_ok_password", int, NULL, setup_login_specific, test_login_specific_ok_password, teardown_login_specific);
	g_test_add("/login_specific/login_specific_bad_password_then_cancel", int, NULL, setup_login_specific, test_login_specific_bad_password_then_cancel, teardown_login_specific);
	g_test_add("/login_specific/login_specific_cancel_immediately", int, NULL, setup_login_specific, test_login_specific_cancel_immediately, teardown_login_specific);
	g_test_add("/login_user/login_fail_unsupported_so", int, NULL, setup_login_user, test_login_fail_unsupported_so, teardown_login_user);
	g_test_add("/login_user/login_skip_prompt_because_pin", int, NULL, setup_login_user, test_login_skip_prompt_because_pin, teardown_login_user);
	g_test_add("/login_user/login_user_ok_password", int, NULL, setup_login_user, test_login_user_ok_password, teardown_login_user);
	g_test_add("/login_user/login_user_bad_password_then_cancel", int, NULL, setup_login_user, test_login_user_bad_password_then_cancel, teardown_login_user);
	g_test_add("/login_user/login_user_cancel_immediately", int, NULL, setup_login_user, test_login_user_cancel_immediately, teardown_login_user);
	g_test_add("/login_user/login_user_fail_get_session_info", int, NULL, setup_login_user, test_login_user_fail_get_session_info, teardown_login_user);
	g_test_add("/login_user/login_user_fail_get_token_info", int, NULL, setup_login_user, test_login_user_fail_get_token_info, teardown_login_user);
	g_test_add("/set_pin/set_pin_ok_passwords", int, NULL, setup_set_pin, test_set_pin_ok_passwords, teardown_set_pin);
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
