include $(top_srcdir)/tests/Makefile.plugin

INCLUDES = \
	-I$(top_srcdir)		\
	$(PEAS_CFLAGS)		\
	$(WARN_CFLAGS)		\
	$(DISABLE_DEPRECATED)

noinst_LTLIBRARIES = libself-dep.la

libself_dep_la_SOURCES = \
	self-dep-plugin.c	\
	self-dep-plugin.h

libself_dep_la_LDFLAGS = $(TEST_PLUGIN_LIBTOOL_FLAGS)
libself_dep_la_LIBADD  = $(PEAS_LIBS)

noinst_PLUGIN = self-dep.plugin

EXTRA_DIST = $(noinst_PLUGIN)
