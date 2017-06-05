if PLATFORM_WIN32
no_undefined = -no-undefined
else
no_undefined =
endif

noinst_LTLIBRARIES =  \
	libteststateset.la	\
	libtestrelation.la

libatk = $(top_builddir)/atk/libatk-$(ATK_API_VERSION).la

INCLUDES = -I$(top_srcdir)	\
	-DG_DISABLE_DEPRECATED	\
	$(DEP_CFLAGS)
DEPS = \
	$(libatk)

moduledir = $(libdir)/gtk-2.0/modules

libteststateset_la_SOURCES = teststateset.c
libteststateset_la_LDFLAGS = \
	-rpath $(moduledir) \
	$(no_undefined)	\
	-avoid-version	\
	-module
libteststateset_la_LIBADD = $(libatk) $(DEP_LIBS)

libtestrelation_la_SOURCES = testrelation.c
libtestrelation_la_LDFLAGS = \
	-rpath $(moduledir) \
	$(no_undefined)	\
	-avoid-version	\
	-module
libtestrelation_la_LIBADD = $(libatk) $(DEP_LIBS)
