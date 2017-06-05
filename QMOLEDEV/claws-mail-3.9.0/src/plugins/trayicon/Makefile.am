plugindir = $(pkglibdir)/plugins

plugin_LTLIBRARIES = trayicon.la

if CYGWIN
cygwin_export_lib = -L$(top_builddir)/src -lclaws-mail
else
cygwin_export_lib = 
endif 

trayicon_la_SOURCES = \
	trayicon.c \
	trayicon_prefs.c \
	trayicon_prefs.h

trayicon_la_LDFLAGS = \
	-avoid-version -module -no-undefined

trayicon_la_LIBADD = \
	$(cygwin_export_lib) \
	$(GTK_LIBS) \
	-L/usr/X11R6/lib -lX11 \
	$(MAEMO_LIBS)

INCLUDES = \
	-I$(top_srcdir)/src \
	-I$(top_srcdir)/src/common \
	-I$(top_srcdir)/src/gtk

AM_CPPFLAGS = \
	$(GLIB_CFLAGS) \
	$(GTK_CFLAGS) \
	$(ENCHANT_CFLAGS) \
	$(MAEMO_CFLAGS)

EXTRA_DIST = \
	README 
