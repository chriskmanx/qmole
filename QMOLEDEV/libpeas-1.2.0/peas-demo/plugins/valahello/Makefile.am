plugindir = $(libdir)/peas-demo/plugins/valahello

INCLUDES = \
	-I$(top_srcdir) 	\
	$(PEAS_CFLAGS)		\
	$(PEAS_GTK_CFLAGS)

VALAFLAGS = \
	--girdir $(top_builddir)/libpeas \
	--girdir $(top_builddir)/libpeas-gtk \
	--pkg PeasGtk-1.0 --pkg gtk+-3.0

plugin_LTLIBRARIES = libvalahello.la

libvalahello_la_SOURCES = peasdemo-vala-hello-plugin.vala

libvalahello_la_LDFLAGS = $(PLUGIN_LIBTOOL_FLAGS)
libvalahello_la_LIBADD  = $(PEAS_LIBS) $(PEAS_GTK_LIBS)

plugin_DATA = valahello.plugin

EXTRA_DIST = $(plugin_DATA)
