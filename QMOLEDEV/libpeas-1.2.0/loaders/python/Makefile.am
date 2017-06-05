# Python plugin loader

loaderdir = $(libdir)/libpeas-1.0/loaders

INCLUDES = \
	-I$(top_srcdir)			\
	$(PEAS_CFLAGS)			\
	$(GCOV_CFLAGS)			\
	$(WARN_CFLAGS)			\
	$(DISABLE_DEPRECATED)		\
	$(PYTHON_CFLAGS)		\
	-DPEAS_PYEXECDIR=\""$(pyexecdir)"\" \
	-DPEAS_LOCALEDIR=\""$(prefix)/$(DATADIRNAME)/locale"\"

loader_LTLIBRARIES = libpythonloader.la

libpythonloader_la_SOURCES = \
	peas-extension-python.c		\
	peas-extension-python.h		\
	peas-plugin-loader-python.c 	\
	peas-plugin-loader-python.h

libpythonloader_la_LDFLAGS = \
	$(LOADER_LIBTOOL_FLAGS)	\
	$(GCOV_LDFLAGS)

libpythonloader_la_LIBADD = \
	$(PEAS_LIBS)			\
	$(PYTHON_LIBS)

gcov_sources = $(libpythonloader_la_SOURCES)
include $(top_srcdir)/Makefile.gcov
