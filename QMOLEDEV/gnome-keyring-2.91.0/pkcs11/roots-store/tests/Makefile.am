
TESTING_FILES = \
	test-module.c

UNIT_PROMPT =

TESTING_LIBS = \
	$(top_builddir)/pkcs11/roots-store/libgkm-roots-store.la \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(top_builddir)/egg/libegg.la

include $(top_srcdir)/testing/testing.make

EXTRA_DIST = \
	test-data \
	p11-tests.conf
