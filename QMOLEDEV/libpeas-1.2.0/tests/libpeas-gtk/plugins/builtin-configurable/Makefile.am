include $(top_srcdir)/tests/Makefile.plugin

INCLUDES = \
	-I$(top_srcdir)		\
	$(PEAS_CFLAGS)		\
	$(PEAS_GTK_CFLAGS)	\
	$(WARN_CFLAGS)		\
	$(DISABLE_DEPRECATED)

noinst_LTLIBRARIES = libbuiltin-configurable.la

libbuiltin_configurable_la_SOURCES = \
	builtin-plugin.c	\
	builtin-plugin.h	\
	builtin-configurable.c	\
	builtin-configurable.h

libbuiltin_configurable_la_LDFLAGS = $(TEST_PLUGIN_LIBTOOL_FLAGS)
libbuiltin_configurable_la_LIBADD  = $(PEAS_LIBS) $(PEAS_GTK_LIBS)

noinst_PLUGIN = builtin-configurable.plugin

EXTRA_DIST = $(noinst_PLUGIN)
