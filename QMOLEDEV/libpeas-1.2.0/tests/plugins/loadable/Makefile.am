include $(top_srcdir)/tests/Makefile.plugin

INCLUDES = \
	-I$(top_srcdir)		\
	$(PEAS_CFLAGS)		\
	$(WARN_CFLAGS)		\
	$(DISABLE_DEPRECATED)

noinst_LTLIBRARIES = libloadable.la

libloadable_la_SOURCES = \
	loadable-plugin.c	\
	loadable-plugin.h

libloadable_la_LDFLAGS = $(TEST_PLUGIN_LIBTOOL_FLAGS)
libloadable_la_LIBADD  = $(PEAS_LIBS)

noinst_PLUGIN = loadable.plugin

EXTRA_DIST = $(noinst_PLUGIN)
