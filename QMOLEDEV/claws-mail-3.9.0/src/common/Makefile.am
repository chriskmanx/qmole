PLUGINDIR = $(pkglibdir)/plugins/
DESKTOPFILEPATH=$(datadir)/applications/claws-mail.desktop

noinst_LTLIBRARIES = libclawscommon.la

if OS_WIN32
arch_sources = fnmatch.c \
               w32_dirent.c w32_reg.c w32_signal.c w32_stat.c \
               w32_stdio.c w32_stdlib.c w32_string.c w32_time.c \
               w32_unistd.c w32_wait.c w32_account.c
arch_headers = fnmatch.h w32lib.h
else
arch_files = 
arch_header = 
endif

libclawscommon_la_SOURCES = $(arch_sources) \
	base64.c \
	hooks.c \
	log.c \
	md5.c \
	mgutils.c \
	passcrypt.c \
	plugin.c \
	prefs.c \
	progressindicator.c \
	quoted-printable.c \
	session.c \
	smtp.c \
	socket.c \
	ssl.c \
	ssl_certificate.c \
	string_match.c \
	stringtable.c \
	claws.c \
	tags.c \
	template.c \
	utils.c \
	uuencode.c \
	xml.c \
	xmlprops.c

clawscommonincludedir = $(pkgincludedir)/common
clawscommoninclude_HEADERS = $(arch_headers) \
	base64.h \
	defs.h \
	hooks.h \
	log.h \
	md5.h \
	mgutils.h \
	passcrypt.h \
	plugin.h \
	prefs.h \
	progressindicator.h \
	quoted-printable.h \
	session.h \
	smtp.h \
	socket.h \
	ssl_certificate.h \
	ssl.h \
	string_match.h \
	stringtable.h \
	claws.h \
	tags.h \
	template.h \
	timing.h \
	utils.h \
	uuencode.h \
	version.h \
	xml.h \
	xmlprops.h

INCLUDES = \
	-I$(top_srcdir)/intl

AM_CPPFLAGS = \
	$(GLIB_CFLAGS) \
	$(MAEMO_CFLAGS) \
	$(VALGRIND_CFLAGS) \
	-DLOCALEDIR=\""$(localedir)"\" \
	-DPLUGINDIR=\"$(PLUGINDIR)\" \
	-DDATAROOTDIR=\""$(datarootdir)"\" \
	-DDESKTOPFILEPATH=\"$(DESKTOPFILEPATH)\" \
	-DGTK_DISABLE_DEPRECATED

libclawscommon_la_LIBADD = \
	$(GLIB_LIBS) \
	$(CRYPT_LIBS) \
	$(PTHREAD_LIBS) \
	$(MAEMO_LIBS)

EXTRA_DIST = \
	version.h.in fnmatch_loop.c

