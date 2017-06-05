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
	frob-control-change \
	frob-control-init \
	frob-control-unlock \
	frob-control-quit

# ------------------------------------------------------------------------------
# Test unlocking the login keyring

frob_control_change_SOURCES = \
	frob-control-change.c

frob_control_change_LDADD = \
	$(top_builddir)/daemon/control/libgkd-control-client.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la

frob_control_init_SOURCES = \
	frob-control-init.c

frob_control_init_LDADD = \
	$(top_builddir)/daemon/control/libgkd-control-client.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la

frob_control_quit_SOURCES = \
	frob-control-quit.c

frob_control_quit_LDADD = \
	$(top_builddir)/daemon/control/libgkd-control-client.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la

frob_control_unlock_SOURCES = \
	frob-control-unlock.c

frob_control_unlock_LDADD = \
	$(top_builddir)/daemon/control/libgkd-control-client.la \
	$(top_builddir)/egg/libegg-buffer.la \
	$(top_builddir)/egg/libegg-creds.la \
	$(top_builddir)/egg/libegg-secure.la