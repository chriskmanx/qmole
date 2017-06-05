AUTOMAKE_OPTIONS = 1.7
PLATFORM_VERSION = 2.0

pkgincludedir = $(includedir)/pygtk-$(PLATFORM_VERSION)
pkginclude_HEADERS = pygobject.h

pkgpyexecdir = $(pyexecdir)/gtk-2.0

# gobject python scripts
pygobjectdir = $(pkgpyexecdir)/gobject
pygobject_PYTHON = 	\
	__init__.py 	\
	propertyhelper.py
pygobject_LTLIBRARIES = _gobject.la 
nodist_pygobject_PYTHON = constants.py

common_ldflags = -module -avoid-version
if PLATFORM_WIN32
common_ldflags += -no-undefined
endif

constants.py: generate-constants$(EXEEXT) constants.py.in
	rm -f constants.py
	cp $(srcdir)/constants.py.in constants.py
	chmod 644 constants.py
	$(top_builddir)/gobject/generate-constants$(EXEEXT) >> constants.py
	chmod 444 constants.py

generate_constants_CFLAGS = $(GLIB_CFLAGS) $(PYTHON_INCLUDES)

noinst_PROGRAMS = generate-constants
CLEANFILES = constants.py
EXTRA_DIST = constants.py.in

_gobject_la_CFLAGS = \
	-I$(top_srcdir)/glib \
	$(PYTHON_INCLUDES) \
	$(FFI_CFLAGS) \
	$(GLIB_CFLAGS) \
	 -DPY_SSIZE_T_CLEAN
_gobject_la_LDFLAGS = $(common_ldflags) -export-symbols-regex "_gobject|PyInit__gobject"
_gobject_la_LIBADD = \
	$(GLIB_LIBS) \
	$(FFI_LIBS) \
	$(top_builddir)/glib/libpyglib-2.0.la
_gobject_la_SOURCES =           \
        gobjectmodule.c         \
        pygboxed.c              \
        pygboxed.h              \
        pygenum.c               \
        pygenum.h               \
        pygflags.c              \
        pygflags.h              \
        pyginterface.c          \
        pyginterface.h          \
        pygobject.c             \
        pygobject.h             \
        pygobject-private.h     \
        pygparamspec.c          \
        pygparamspec.h          \
        pygpointer.c            \
        pygpointer.h            \
        pygtype.c               \
        pygtype.h
_gobject_la_DEPENDENCIES = constants.py

if HAVE_LIBFFI
_gobject_la_SOURCES += ffi-marshaller.c ffi-marshaller.h
endif

if PLATFORM_WIN32
_gobject_la_CFLAGS += -DPLATFORM_WIN32
endif
