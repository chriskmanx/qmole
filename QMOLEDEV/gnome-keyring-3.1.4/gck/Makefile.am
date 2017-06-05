
if WITH_TESTS
TESTS_DIR = tests
TESTABLE_LIB = libgck-testable.la
else
TESTS_DIR =
TESTABLE_LIB =
endif

SUBDIRS = . $(TESTS_DIR)

incdir = $(includedir)/gck-@GCK_MAJOR@/gck

inc_HEADERS = \
	gck.h

INCLUDES = \
	-I$(top_builddir) \
	-I$(top_srcdir) \
	$(P11_KIT_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GTHREAD_CFLAGS) \
	$(GLIB_CFLAGS) \
	-DGCK_API_SUBJECT_TO_CHANGE \
	-DP11_KIT_API_SUBJECT_TO_CHANGE \
	-DPKCS11_REGISTRY_DIR=\"$(libdir)/pkcs11\"

BUILT_SOURCES = \
	gck-marshal.c gck-marshal.h

lib_LTLIBRARIES = libgck-@GCK_MAJOR@.la

libgck_@GCK_MAJOR@_la_SOURCES = \
	gck.h gck-private.h pkcs11.h \
	gck-attributes.c \
	gck-call.c \
	gck-dump.c \
	gck-enumerator.c \
	gck-misc.c \
	gck-module.c \
	gck-modules.c \
	gck-object.c \
	gck-session.c \
	gck-slot.c \
	gck-uri.c \
	$(BUILT_SOURCES)

libgck_@GCK_MAJOR@_la_LDFLAGS = \
	-version-info $(GCK_LT_RELEASE) \
	-no-undefined \
	-export-symbols-regex '^gck_*'

libgck_@GCK_MAJOR@_la_LIBADD = \
	$(top_builddir)/egg/libegg-hex.la \
	$(P11_KIT_LIBS) \
	$(GOBJECT_LIBS) \
	$(GTHREAD_LIBS) \
	$(GIO_LIBS) \
	$(GLIB_LIBS)

noinst_LTLIBRARIES = $(TESTABLE_LIB)
libgck_testable_la_SOURCES = \
	gck-mock.c \
	gck-mock.h \
	gck-test.c \
	gck-test.h
libgck_testable_la_LIBADD = $(libgck_@GCK_MAJOR@_la_OBJECTS) \
	$(libgck_@GCK_MAJOR@_la_LIBADD)
libgck_testable_la_DEPENDENCIES = $(libgck_@GCK_MAJOR@_la_OBJECTS)

gck-marshal.h: gck-marshal.list $(GLIB_GENMARSHAL)
	$(GLIB_GENMARSHAL) $< --header --prefix=_gck_marshal > $@

gck-marshal.c: gck-marshal.list $(GLIB_GENMARSHAL)
	echo "#include \"gck-marshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=_gck_marshal >> $@

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = gck-$(GCK_MAJOR).pc

EXTRA_DIST = \
	gck.pc.in \
	gck-marshal.list

DISTCLEANFILES = \
	gck-$(GCK_MAJOR).pc

gck-$(GCK_MAJOR).pc: gck.pc
	cp gck.pc gck-$(GCK_MAJOR).pc

symbols: libgck-$(VERSION).symbols

libgck-$(VERSION).symbols: .libs/libgck-@GCK_MAJOR@.so
	nm -D .libs/libgck-@GCK_MAJOR@.so | grep -F ' T ' | \
		cut -d ' ' -f 3 | sort > $@
