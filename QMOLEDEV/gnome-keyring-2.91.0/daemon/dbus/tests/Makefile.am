
# Test files should be listed in order they need to run
TESTING_FILES = \
	test-secret-util.c

TESTING_LIBS =  \
	$(top_builddir)/daemon/dbus/libgkd-dbus.la

TESTING_FLAGS = \
	$(DAEMON_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GLIB_CFLAGS)

include $(top_srcdir)/testing/testing.make
