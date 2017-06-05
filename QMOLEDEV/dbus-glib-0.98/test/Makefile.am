SUBDIRS = . core interfaces
DIST_SUBDIRS = core interfaces

INCLUDES = \
	-I$(top_srcdir)				\
	-I$(top_builddir)			\
	-I$(top_builddir)/dbus			\
	$(DBUS_CFLAGS)

if DBUS_BUILD_TESTS
TEST_BINARIES=test-service
else
TEST_BINARIES=
endif

noinst_PROGRAMS= $(TEST_BINARIES)

test_service_SOURCES=				\
	test-service.c

test_service_LDADD=$(DBUS_LIBS)

EXTRA_DIST = data/nested-introspect.xml test-compile-nested.sh

TESTS_ENVIRONMENT=top_builddir=$(top_builddir) srcdir=$(srcdir)
TESTS = test-compile-nested.sh
