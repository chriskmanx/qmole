# C plugin loader

loaderdir = $(libdir)/libpeas-1.0/loaders

INCLUDES = \
	-I$(top_srcdir)			\
	$(PEAS_CFLAGS)			\
	$(GCOV_CFLAGS)			\
	$(WARN_CFLAGS)			\
	$(DISABLE_DEPRECATED)

loader_LTLIBRARIES = libcloader.la

libcloader_la_SOURCES = \
	peas-plugin-loader-c.c 	\
	peas-plugin-loader-c.h

libcloader_la_LDFLAGS = $(LOADER_LIBTOOL_FLAGS) $(GCOV_LDFLAGS)
libcloader_la_LIBADD = $(PEAS_LIBS)

gcov_sources = $(libcloader_la_SOURCES)
include $(top_srcdir)/Makefile.gcov
