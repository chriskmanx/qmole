TESTING_FILES = \
	unit-test-secret-compat.c \
	unit-test-secret-fields.c \
	unit-test-secret-data.c \
	unit-test-secret-object.c \
	unit-test-secret-collection.c \
	unit-test-secret-item.c \
	unit-test-secret-search.c \
	unit-test-secret-textual.c \
	unit-test-secret-binary.c \
	test-secret-module.c test-secret-module.h

UNIT_PROMPT =

TESTING_LIBS =  \
	$(top_builddir)/pkcs11/secret-store/libgkm-secret-store.la \
	$(top_builddir)/pkcs11/gkm/libgkm.la \
	$(top_builddir)/egg/libegg.la

EXTRA_DIST = \
	test-data

include $(top_srcdir)/testing/testing.make
