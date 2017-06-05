
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-DSRCDIR="\"$(srcdir)\"" \
	-DBUILDDIR="\"$(builddir)\"" \
	-DGCK_API_SUBJECT_TO_CHANGE \
	$(GLIB_CFLAGS)

LDADD =  \
	libmock-test-module.la \
	$(top_builddir)/gck/libgck-testable.la \
	$(top_builddir)/egg/libegg-test.la \
	$(top_builddir)/egg/libegg-hex.la \
	$(GTHREAD_LIBS) \
	$(GLIB_LIBS) \
	$(GIO_LIBS)

TEST_PROGS = \
	test-gck-attributes \
	test-gck-module \
	test-gck-slot \
	test-gck-session \
	test-gck-object \
	test-gck-crypto \
	test-gck-uri \
	test-gck-enumerator \
	test-gck-modules

check_PROGRAMS = $(TEST_PROGS)

test: $(TEST_PROGS)
	gtester -k --verbose -m $(TEST_MODE) --g-fatal-warnings $(TEST_PROGS)

check-local: test

all-local: $(check_PROGRAMS)

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
	$(top_builddir)/gck/libgck-testable.la
