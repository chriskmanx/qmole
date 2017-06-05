
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-I$(top_srcdir)/pkcs11 \
	-DSRCDIR="\"$(srcdir)\"" \
	$(GLIB_CFLAGS) \
	$(LIBGCRYPT_CFLAGS)

LDADD = \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(top_builddir)/egg/libegg.la \
	$(GLIB_LIBS) \
	$(LIBGCRYPT_LIBS)

noinst_LIBRARIES = libgkm-mock.a

BUILT_SOURCES = \
	asn1-def-test.h

libgkm_mock_a_SOURCES = \
	mock-module.c mock-module.h \
	mock-locked-object.c mock-locked-object.h
	$(BUILT_SOURCES)

TEST_PROGS = \
	test-attributes \
	test-credential \
	test-data-asn1 \
	test-data-der \
	test-file-tracker \
	test-memory-store \
	test-object \
	test-secret \
	test-sexp \
	test-store \
	test-timer \
	test-transaction

test_credential_LDADD = \
	libgkm-mock.a \
	$(LDADD)

test_memory_store_LDADD = \
	libgkm-mock.a \
	$(LDADD)

test_object_LDADD = \
	libgkm-mock.a \
	$(LDADD)

test_store_LDADD = \
	libgkm-mock.a \
	$(LDADD)

test_timer_LDADD = \
	libgkm-mock.a \
	$(LDADD)

check_PROGRAMS = $(TEST_PROGS)

test: $(TEST_PROGS)
	gtester --verbose -m $(TEST_MODE) --g-fatal-warnings $(TEST_PROGS)

check-local: test

all-local: $(check_PROGRAMS)

asn1-def-test.h: test.asn
	$(ASN1PARSER) -o asn1-def-test.h $(srcdir)/test.asn

EXTRA_DIST = \
	test.asn \
	files

DISTCLEANFILES = \
	$(BUILT_SOURCES)
