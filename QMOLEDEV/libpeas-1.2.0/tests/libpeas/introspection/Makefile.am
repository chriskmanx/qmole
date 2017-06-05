noinst_LTLIBRARIES = libintrospection-1.0.la

INTROSPECTION_INCLUDES = \
	-I$(top_srcdir)	\
	-I$(srcdir)	\
	$(PEAS_CFLAGS)

INCLUDES = \
	$(WARN_CFLAGS)		\
	$(DISABLE_DEPRECATED)

libintrospection_1_0_la_LDFLAGS = \
	-export-dynamic -no-undefined -export-symbols-regex "^[^_].*"

libintrospection_1_0_la_LIBADD = $(PEAS_LIBS)

libintrospection_1_0_la_SOURCES = \
	introspection-callable.c	\
	introspection-callable.h	\
	introspection-properties.c	\
	introspection-properties.h	\
	introspection-unimplemented.c	\
	introspection-unimplemented.h

INCLUDES += $(INTROSPECTION_INCLUDES)


-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS = Introspection-1.0.gir
INTROSPECTION_TYPELIBS = $(INTROSPECTION_GIRS:.gir=.typelib)
INTROSPECTION_SCANNER_ARGS = --warn-all

introspection_sources = $(libintrospection_1_0_la_SOURCES)

Introspection-1.0.gir: libintrospection-1.0.la
Introspection_1_0_gir_INCLUDES = GObject-2.0
Introspection_1_0_gir_CFLAGS = $(INTROSPECTION_INCLUDES)
Introspection_1_0_gir_LIBS = libintrospection-1.0.la
Introspection_1_0_gir_FILES = $(addprefix $(srcdir)/,$(introspection_sources))

CLEANFILES = $(INTROSPECTION_GIRS) $(INTROSPECTION_TYPELIBS)

all: Introspection-1.0.typelib

