
INCLUDES = \
	-I$(top_srcdir)/daemon/dbus \
	-DSRCDIR="\"$(srcdir)\"" \
	$(DAEMON_CFLAGS) \
	$(GLIB_CFLAGS)

LDADD =  \
	$(top_builddir)/daemon/dbus/libgkd-dbus.la

TEST_PROGS = \
	test-secret-util

check_PROGRAMS = $(TEST_PROGS)

test: $(TEST_PROGS)
	gtester -k --verbose -m $(TEST_MODE) --g-fatal-warnings $(TEST_PROGS)

check-local: test

all-local: $(check_PROGRAMS)
