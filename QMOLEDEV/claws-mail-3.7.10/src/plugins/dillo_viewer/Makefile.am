plugindir = $(pkglibdir)/plugins

plugin_LTLIBRARIES = dillo_viewer.la

dillo_viewer_la_SOURCES = \
         dillo_viewer.c \
         dillo_prefs.c dillo_prefs.h

dillo_viewer_la_LDFLAGS = \
	-avoid-version -module

if CYGWIN
cygwin_export_lib = -L$(top_builddir)/src -lclaws-mail
else
cygwin_export_lib =
endif
dillo_viewer_la_LIBADD = $(cygwin_export_lib) \
	$(GTK_LIBS) \
	$(MAEMO_LIBS)

INCLUDES = \
	-I$(top_srcdir)/src \
	-I$(top_srcdir)/src/common \
	-I$(top_srcdir)/src/gtk

AM_CPPFLAGS = \
	$(ENCHANT_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GTK_CFLAGS) \
	$(MAEMO_CFLAGS)
