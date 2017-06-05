
INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	-DSRCDIR="\"$(srcdir)\"" \
	$(GLIB_CFLAGS) \
	$(LIBGCRYPT_CFLAGS)

LDADD = \
	$(top_builddir)/ui/libgku-prompt-testable.la \
	$(top_builddir)/egg/libegg.la \
	$(GLIB_LIBS) \
	$(GOBJECT_LIBS) \
	$(LIBGCRYPT_LIBS)

TEST_PROGS = \
	test-util

check_PROGRAMS = $(TEST_PROGS)

test: $(TEST_PROGS) $(CHECK_PROGS)
	gtester -k --verbose -m $(TEST_MODE) --g-fatal-warnings $(TEST_PROGS)

check-local: test

all-local: $(check_PROGRAMS)

EXTRA_DIST = \
	files
