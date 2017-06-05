incdir = $(includedir)/gp11

inc_HEADERS = \
	gp11.h

INCLUDES = \
	-I$(top_builddir) \
    	-I$(top_srcdir) \
    	$(GOBJECT_CFLAGS) \
	$(GTHREAD_CFLAGS) \
	$(GLIB_CFLAGS)

BUILT_SOURCES = \
	gp11-marshal.c gp11-marshal.h

lib_LTLIBRARIES = libgp11.la

libgp11_la_SOURCES = \
	gp11.h gp11-private.h pkcs11.h \
	gp11-attributes.c \
	gp11-call.c \
	gp11-misc.c \
	gp11-module.c \
	gp11-object.c \
	gp11-session.c \
	gp11-slot.c \
	$(BUILT_SOURCES)
    
libgp11_la_LDFLAGS = \
    	-version-info $(GP11_LT_RELEASE) \
	-no-undefined -export-symbols-regex 'gp11_*'
    
libgp11_la_LIBADD = \
	$(GOBJECT_LIBS) \
	$(GTHREAD_LIBS) \
	$(GIO_LIBS) \
	$(GLIB_LIBS)

gp11-marshal.h: gp11-marshal.list $(GLIB_GENMARSHAL)
	$(GLIB_GENMARSHAL) $< --header --prefix=_gp11_marshal > $@

gp11-marshal.c: gp11-marshal.list $(GLIB_GENMARSHAL)
	echo "#include \"gp11-marshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=_gp11_marshal >> $@

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = gp11-$(GP11_MAJOR).pc

EXTRA_DIST = \
	gp11.pc.in \
	gp11-marshal.list
    
DISTCLEANFILES = \
	gp11-$(GP11_MAJOR).pc
    
gp11-$(GP11_MAJOR).pc: gp11.pc
	cp gp11.pc gp11-$(GP11_MAJOR).pc

if WITH_TESTS
TESTS_DIR = tests
else
TESTS_DIR = 
endif

SUBDIRS = . \
	$(TESTS_DIR)
	