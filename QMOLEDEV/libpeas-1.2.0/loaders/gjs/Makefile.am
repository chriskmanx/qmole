# GJS plugin loader

loaderdir = $(libdir)/libpeas-1.0/loaders

INCLUDES = \
	-I$(top_srcdir)			\
	$(PEAS_CFLAGS)			\
	$(GCOV_CFLAGS)			\
	$(WARN_CFLAGS)			\
	$(DISABLE_DEPRECATED)		\
	$(GJS_CFLAGS)

loader_LTLIBRARIES = libgjsloader.la

libgjsloader_la_SOURCES = \
	peas-extension-gjs.c		\
	peas-extension-gjs.h		\
	peas-plugin-loader-gjs.c 	\
	peas-plugin-loader-gjs.h

libgjsloader_la_LDFLAGS = $(LOADER_LIBTOOL_FLAGS) $(GCOV_LDFLAGS)
libgjsloader_la_LIBADD = $(PEAS_LIBS) $(GJS_LIBS)

gcov_sources = $(libgjsloader_la_SOURCES)
include $(top_srcdir)/Makefile.gcov
