INCLUDES= \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	-I$(top_srcdir)/daemon \
	$(GTK_CFLAGS) \
	$(GLIB_CFLAGS)

LIBS = \
	$(GLIB_LIBS) \
	$(GTK_LIBS) \
	$(GTHREAD_LIBS) \
	$(P11_TESTS_LIBS)

noinst_PROGRAMS= \
	test-control-change \
	test-control-init \
	test-control-unlock \
	test-control-quit

# ------------------------------------------------------------------------------
# Test unlocking the login keyring

test_control_change_SOURCES = \
	test-control-change.c

test_control_change_LDADD = \
	$(top_builddir)/daemon/control/libgkd-control-client.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la

test_control_init_SOURCES = \
	test-control-init.c

test_control_init_LDADD = \
	$(top_builddir)/daemon/control/libgkd-control-client.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la

test_control_quit_SOURCES = \
	test-control-quit.c

test_control_quit_LDADD = \
	$(top_builddir)/daemon/control/libgkd-control-client.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la

test_control_unlock_SOURCES = \
	test-control-unlock.c

test_control_unlock_LDADD = \
	$(top_builddir)/daemon/control/libgkd-control-client.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la