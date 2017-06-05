/* This is auto-generated code. Edit at your own peril. */
#include "testing/testing.h"
#include "test-suite.h"

static void start_tests (void) {
}

static void stop_tests (void) {
}

static void initialize_tests (void) {
	g_test_add("/ssh_openssh/parse_public", int, NULL, NULL, test_parse_public, NULL);
	g_test_add("/ssh_openssh/parse_private", int, NULL, NULL, test_parse_private, NULL);
	g_test_add("/private_key/private_key_parse_plain", int, NULL, setup_private_key_setup, test_private_key_parse_plain, teardown_private_key_teardown);
	g_test_add("/private_key/private_key_parse_and_unlock", int, NULL, setup_private_key_setup, test_private_key_parse_and_unlock, teardown_private_key_teardown);
}

static void run_externals (int *ret) {
	testing_external_run ("ssh_module", external_ssh_module, ret);
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
