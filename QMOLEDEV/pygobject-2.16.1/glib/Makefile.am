AUTOMAKE_OPTIONS = 1.7
INCLUDES = $(PYTHON_INCLUDES) $(GLIB_CFLAGS) -DPY_SSIZE_T_CLEAN

pkgincludedir = $(includedir)/pygtk-2.0
pkginclude_HEADERS = pyglib.h

lib_LTLIBRARIES = libpyglib-2.0.la

pkgpyexecdir = $(pyexecdir)/gtk-2.0

pyglibdir = $(pkgpyexecdir)/glib
pyglib_PYTHON = 	\
	__init__.py	\
	option.py
pyglib_LTLIBRARIES = _glib.la

common_ldflags = -module -avoid-version
if PLATFORM_WIN32
common_ldflags += -no-undefined
endif

libpyglib_2_0_la_CFLAGS = $(GLIB_CFLAGS)
libpyglib_2_0_la_LIBADD = $(GLIB_LIBS) $(FFI_LIBS)
libpyglib_2_0_la_SOURCES = 	\
	pyglib.c		\
	pyglib.h		\
	pyglib-private.h 	\
	pyglib-python-compat.h

_glib_la_CFLAGS = $(GLIB_CFLAGS)
_glib_la_LDFLAGS = $(common_ldflags) -export-symbols-regex "_glib|PyInit__glib"
_glib_la_LIBADD = $(GLIB_LIBS) libpyglib-2.0.la
_glib_la_SOURCES = 	 	\
	glibmodule.c	 	\
	pygiochannel.c 	 	\
	pygiochannel.h 	 	\
	pygoptioncontext.c 	\
	pygoptioncontext.h 	\
	pygoptiongroup.c 	\
	pygoptiongroup.h 	\
	pygmaincontext.c 	\
	pygmaincontext.h 	\
	pygmainloop.c	 	\
	pygmainloop.h	 	\
	pygsource.c 	 	\
	pygsource.h 	 	\
	pygspawn.c	 	\
	pygspawn.h

if PLATFORM_WIN32
_glib_la_CFLAGS += -DPLATFORM_WIN32
endif
