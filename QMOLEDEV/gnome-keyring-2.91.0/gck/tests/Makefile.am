
# Keep these in the order they should be tested
TESTING_FILES = \
	test-gck-attributes.c \
	test-gck-module.c \
	test-gck-slot.c \
	test-gck-session.c \
	test-gck-object.c \
	test-gck-crypto.c \
	test-gck-uri.c \
	test-gck-enumerator.c \
	test-gck-modules.c

TESTING_FLAGS = \
	-I$(top_srcdir)/gck/ \
	-DEXTERNAL_TEST \
	-DGCK_API_SUBJECT_TO_CHANGE

TESTING_LIBS = \
	$(GIO_LIBS) \
	$(top_builddir)/gck/libgck.la \
	libmock-test-module.la

EXTRA_DIST = \
	gck-test.h

include $(top_srcdir)/testing/testing.make

# ------------------------------------------------------------------------

lib_LTLIBRARIES = libmock-test-module.la

libmock_test_module_la_LDFLAGS = \
	-avoid-version

libmock_test_module_la_CFLAGS = \
	-I$(top_srcdir)/gck \
	-I$(top_srcdir) \
	$(GLIB_CFLAGS) \
	-DGCK_API_SUBJECT_TO_CHANGE

libmock_test_module_la_SOURCES = \
	mock-test-module.c

libmock_test_module_la_LIBADD = \
	$(top_builddir)/gck/libgck.la
