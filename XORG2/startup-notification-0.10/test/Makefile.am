
INCLUDES=$(LIBSN_CFLAGS) -I$(top_srcdir) -DSN_API_NOT_YET_FROZEN=1


XLIB_TEST=					\
	test-launcher				\
	test-launchee				\
	test-monitor				\
	test-send-xmessage			\
	test-watch-xmessages

XCB_TEST=					\
	test-send-xmessage-xcb			\
	test-monitor-xcb			\
	test-launchee-xcb			\
	test-launcher-xcb			\
	test-watch-xmessages-xcb

noinst_PROGRAMS=$(XLIB_TEST) $(XCB_TEST)

test_launcher_SOURCES= test-launcher.c

test_launcher_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_launchee_SOURCES= test-launchee.c

test_launchee_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_monitor_SOURCES= test-monitor.c

test_monitor_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_send_xmessage_SOURCES= test-send-xmessage.c

test_send_xmessage_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_watch_xmessages_SOURCES= test-watch-xmessages.c

test_watch_xmessages_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_watch_xmessages_xcb_SOURCES= test-watch-xmessages-xcb.c

test_watch_xmessages_xcb_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_send_xmessage_xcb_SOURCES= test-send-xmessage-xcb.c

test_send_xmessage_xcb_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_monitor_xcb_SOURCES= test-monitor-xcb.c

test_monitor_xcb_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_launchee_xcb_SOURCES= test-launchee-xcb.c

test_launchee_xcb_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

test_launcher_xcb_SOURCES= test-launcher-xcb.c

test_launcher_xcb_LDADD= $(LIBSN_LIBS) $(top_builddir)/libsn/libstartup-notification-1.la

EXTRA_DIST=test-boilerplate.h
