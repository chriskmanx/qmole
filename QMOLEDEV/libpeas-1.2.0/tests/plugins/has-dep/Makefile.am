include $(top_srcdir)/tests/Makefile.plugin

INCLUDES = \
	-I$(top_srcdir)		\
	$(PEAS_CFLAGS)		\
	$(WARN_CFLAGS)		\
	$(DISABLE_DEPRECATED)

noinst_LTLIBRARIES = libhas-dep.la

libhas_dep_la_SOURCES = \
	has-dep-plugin.c	\
	has-dep-plugin.h

libhas_dep_la_LDFLAGS = $(TEST_PLUGIN_LIBTOOL_FLAGS)
libhas_dep_la_LIBADD  = $(PEAS_LIBS)

noinst_PLUGIN = has-dep.plugin

EXTRA_DIST = $(noinst_PLUGIN)
