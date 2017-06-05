# Seed plugin loader

loaderdir = $(libdir)/libpeas-1.0/loaders

INCLUDES = \
	-I$(top_srcdir)			\
	$(PEAS_CFLAGS)			\
	$(GCOV_CFLAGS)			\
	$(WARN_CFLAGS)			\
	$(DISABLE_DEPRECATED)		\
	$(SEED_CFLAGS)

loader_LTLIBRARIES = libseedloader.la

libseedloader_la_SOURCES = \
	peas-extension-seed.c		\
	peas-extension-seed.h		\
	peas-plugin-loader-seed.c 	\
	peas-plugin-loader-seed.h

libseedloader_la_LDFLAGS = $(LOADER_LIBTOOL_FLAGS) $(GCOV_LDFLAGS)
libseedloader_la_LIBADD = $(PEAS_LIBS) $(SEED_LIBS)

gcov_sources = $(libseedloader_la_SOURCES)
include $(top_srcdir)/Makefile.gcov
