/* This is auto-generated code. Edit at your own peril. */
#include "testing/testing.h"
#include "test-suite.h"

static void start_tests (void) {
}

static void stop_tests (void) {
}

static void initialize_tests (void) {
	g_test_add("/util/encode_decode_mpi", int, NULL, setup_prompt_util, test_encode_decode_mpi, teardown_prompt_util);
	g_test_add("/util/decode_nonexistant_mpi", int, NULL, setup_prompt_util, test_decode_nonexistant_mpi, teardown_prompt_util);
	g_test_add("/util/encode_decode_hex", int, NULL, setup_prompt_util, test_encode_decode_hex, teardown_prompt_util);
	g_test_add("/util/decode_nonexistant_hex", int, NULL, setup_prompt_util, test_decode_nonexistant_hex, teardown_prompt_util);
	g_test_add("/util/encrypt_decrypt_text", int, NULL, setup_prompt_util, test_encrypt_decrypt_text, teardown_prompt_util);
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
