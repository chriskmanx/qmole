
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-I$(top_srcdir)/pkcs11 \
	-DSRCDIR="\"$(srcdir)\"" \
	$(GLIB_CFLAGS) \
	$(LIBGCRYPT_CFLAGS)

LDADD = \
	libgkm-mock-secret-module.a \
	$(top_builddir)/pkcs11/secret-store/libgkm-secret-store.la \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(top_builddir)/egg/libegg.la \
	$(GLIB_LIBS) \
	$(LIBGCRYPT_LIBS)

noinst_LIBRARIES = libgkm-mock-secret-module.a

libgkm_mock_secret_module_a_SOURCES = \
	mock-secret-module.c mock-secret-module.h

TEST_PROGS = \
	test-secret-compat \
	test-secret-fields \
	test-secret-data \
	test-secret-object \
	test-secret-collection \
	test-secret-item \
	test-secret-search \
	test-secret-textual \
	test-secret-binary

check_PROGRAMS = $(TEST_PROGS)

test: $(TEST_PROGS)
	gtester -k --verbose -m $(TEST_MODE) --g-fatal-warnings $(TEST_PROGS)

check-local: test

all-local: $(check_PROGRAMS)

EXTRA_DIST = \
	files
