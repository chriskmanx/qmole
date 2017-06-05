
# Keep these in the order they should be tested
TESTING_FILES = \
	unit-test-gp11-attributes.c \
	test-gp11-mechanism.c \
	unit-test-gp11-module.c \
	unit-test-gp11-slot.c \
	unit-test-gp11-session.c \
	unit-test-gp11-object.c \
	unit-test-gp11-crypto.c

TESTING_FLAGS = \
	-I$(top_srcdir)/gp11/ \
	-DEXTERNAL_TEST

TESTING_LIBS = \
	$(GIO_LIBS) \
	$(top_builddir)/gp11/libgp11.la \
	libgp11-test-module.la 
	
EXTRA_DIST = \
	gp11-test.h
	
include $(top_srcdir)/testing/testing.make

# ------------------------------------------------------------------------

lib_LTLIBRARIES = libgp11-test-module.la

libgp11_test_module_la_LDFLAGS = \
	-avoid-version

libgp11_test_module_la_CFLAGS = \
	-I$(top_srcdir)/gp11 \
	-I$(top_srcdir) \
	$(GLIB_CFLAGS)

libgp11_test_module_la_SOURCES = \
	gp11-test-module.c

libgp11_test_module_la_LIBADD = \
	$(top_builddir)/gp11/libgp11.la
	
