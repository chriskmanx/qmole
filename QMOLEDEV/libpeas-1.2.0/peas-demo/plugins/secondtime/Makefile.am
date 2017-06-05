plugindir = $(libdir)/peas-demo/plugins/secondtime

INCLUDES = \
	-I$(top_srcdir) 	\
	$(PEAS_CFLAGS)		\
	$(PEAS_GTK_CFLAGS)

plugin_LTLIBRARIES = libsecondtime.la

libsecondtime_la_SOURCES = \
	second-time.h		\
	second-time.c

libsecondtime_la_LDFLAGS = $(PLUGIN_LIBTOOL_FLAGS)
libsecondtime_la_LIBADD  = $(PEAS_LIBS) $(PEAS_GTK_LIBS)

plugin_DATA = secondtime.plugin

EXTRA_DIST = $(plugin_DATA)
