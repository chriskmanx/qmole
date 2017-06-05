include $(top_srcdir)/tests/Makefile.plugin

INCLUDES = \
	-I$(top_srcdir)		\
	-I$(srcdir)/../../introspection	\
	$(PEAS_CFLAGS)		\
	$(WARN_CFLAGS)		\
	$(DISABLE_DEPRECATED)

noinst_LTLIBRARIES = libextension-c.la

libextension_c_la_SOURCES = \
	callable-plugin.c	\
	callable-plugin.h	\
	extension-c-plugin.c	\
	extension-c-plugin.h

libextension_c_la_LDFLAGS = $(TEST_PLUGIN_LIBTOOL_FLAGS)
libextension_c_la_LIBADD = \
	$(PEAS_LIBS)						\
	$(builddir)/../../introspection/libintrospection-1.0.la

noinst_PLUGIN = extension-c.plugin

EXTRA_DIST = $(noinst_PLUGIN)
