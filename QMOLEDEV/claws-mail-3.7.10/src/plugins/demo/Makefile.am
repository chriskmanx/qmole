plugindir = $(pkglibdir)/plugins

plugin_LTLIBRARIES = demo.la

demo_la_SOURCES = \
	demo.c

demo_la_LDFLAGS = \
	-avoid-version -module

if CYGWIN
cygwin_export_lib = -L$(top_builddir)/src -lclaws-mail
else
cygwin_export_lib = 
endif

demo_la_LIBADD = $(cygwin_export_lib) \
	$(GTK_LIBS) 
INCLUDES = \
	-I$(top_srcdir)/src \
	-I$(top_srcdir)/src/common 

AM_CPPFLAGS = \
	$(GLIB_CFLAGS)
