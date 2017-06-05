PLUGINDIR = $(pkglibdir)/plugins/

noinst_LTLIBRARIES = libclawsetpan.la

libclawsetpan_la_SOURCES = \
	etpan-thread-manager.c \
	imap-thread.c \
	nntp-thread.c

clawsetpanincludedir = $(pkgincludedir)/etpan
clawsetpaninclude_HEADERS = \
	etpan-thread-manager-types.h \
	etpan-thread-manager.h \
	etpan-errors.h \
	imap-thread.h \
	nntp-thread.h

INCLUDES = \
	-I$(top_srcdir)/src \
	-I$(top_srcdir)/src/common \
	-I$(top_srcdir)/src/gtk

AM_CPPFLAGS = \
	$(GTK_CFLAGS) \
	$(LIBETPAN_CFLAGS) \
	$(MAEMO_CFLAGS) \
	$(ENCHANT_CFLAGS)

libclawsetpan_la_LIBADD = \
	../common/libclawscommon.la \
	$(GTK_LIBS) \
	$(LIBETPAN_LIBS) \
	$(MAEMO_LIBS) \
	$(ENCHANT_LIBS)
