
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-I$(top_srcdir)/pkcs11 \
	-DSRCDIR="\"$(srcdir)\"" \
	$(GLIB_CFLAGS) \
	-DWITH_TESTABLE

LDADD = \
	$(top_builddir)/pkcs11/wrap-layer/libgkm-wrap-layer.la \
	libgkm-mock-secret-store.a \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(top_builddir)/ui/libgku-prompt-testable.la \
	$(top_builddir)/egg/libegg.la \
	$(GLIB_LIBS)

TEST_PROGS = \
	test-create-credential \
	test-init-pin \
	test-login-auto \
	test-login-hints \
	test-login-keyring \
	test-login-specific \
	test-login-user \
	test-set-pin

check_PROGRAMS = $(TEST_PROGS)

test: $(TEST_PROGS) $(CHECK_PROGS)
	gtester -k --verbose -m $(TEST_MODE) --g-fatal-warnings $(TEST_PROGS)

check-local: test

all-local: $(check_PROGRAMS)

noinst_LIBRARIES = libgkm-mock-secret-store.a

libgkm_mock_secret_store_a_SOURCES = \
	mock-secret-store.c