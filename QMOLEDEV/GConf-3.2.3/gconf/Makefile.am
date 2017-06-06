NULL=

GCONFD_BINARY_NAME=gconfd-2

INCLUDES=								\
	-I$(top_srcdir)							\
	-I$(top_builddir)						\
	$(DEPENDENT_WITH_XML_AND_GTK_CFLAGS) 				\
	$(DEPENDENT_DBUS_CFLAGS)				\
	$(DEPENDENT_ORBIT_CFLAGS)				\
	-DG_LOG_DOMAIN=\"GConf\"					\
	-DPREFIX=\""$(prefix)"\"					\
	-DGCONF_LOCALE_DIR=\""$(datadir)/locale"\"			\
        -DGCONF_SRCDIR=\""$(absolute_top_srcdir)"\"			\
	-DGCONF_CONFDIR=\""$(sysgconfdir)/$(MAJOR_VERSION)"\"		\
	-DGCONF_ETCDIR=\""$(sysgconfdir)"\"				\
	-DGCONF_BINDIR=\""$(bindir)"\"					\
	-DGCONF_SERVERDIR=\""$(libexecdir)"\"           \
	-DGCONF_BUILDDIR=\""$(top_builddir)"\"				\
	-DGCONF_BACKEND_DIR=\""$(pkglibdir)/$(MAJOR_VERSION)"\"		\
	-DVERSION=\""$(VERSION)"\"					\
	-DGCONF_ENABLE_INTERNALS=1					\
	-DGCONFD=\""$(GCONFD_BINARY_NAME)$(EXEEXT)"\"			\
	$(NULL)

EFENCE=

if GTK
SANITY_CHECK=gconf-sanity-check-2
else
SANITY_CHECK=
endif

lib_LTLIBRARIES = libgconf-2.la

bin_PROGRAMS = gconftool-2 
libexec_PROGRAMS = gconfd-2 $(SANITY_CHECK)

CORBA_SOURCECODE = GConfX-common.c GConfX-skels.c GConfX-stubs.c GConfX.h

BUILT_SOURCES = 			\
	gconfmarshal.h			\
	gconfmarshal.c
if HAVE_CORBA
BUILT_SOURCES += $(CORBA_SOURCECODE)
endif


$(libgconf_2_la_OBJECTS) $(gconftool_2_OBJECTS) $(gconfd_2_OBJECTS): $(built_sourcecode)

CLEANFILES = $(BUILT_SOURCES) $(service_DATA) $(CORBA_SOURCECODE)

DISTCLEANFILES = default.path

dist-hook:
	cd $(distdir) ; rm -f $(CLEANFILES)

gconf_headers = 		\
	gconf.h			\
	gconf-changeset.h	\
	gconf-listeners.h	\
	gconf-schema.h		\
	gconf-value.h		\
	gconf-error.h		\
	gconf-engine.h		\
	gconf-client.h

gconfincludedir = $(includedir)/gconf/$(MAJOR_VERSION)/gconf
gconfinclude_HEADERS = 		\
	$(gconf_headers) 	\
	gconf-enum-types.h

## This is broken I know, I'm going to break the server/client up and have a library later

gconfd_2_SOURCES = \
	gconf-database.h	\
	gconf-database.c	\
	gconf-sources.h		\
	gconfd.h		\
	gconfd.c

if HAVE_DBUS
gconfd_2_SOURCES += \
	gconfd-dbus.c \
	gconfd-dbus.h \
	gconf-database-dbus.h \
	gconf-database-dbus.c
endif
if OS_WIN32
gconfd_2_LDFLAGS = -mwindows
endif

gconfd_2_CFLAGS = $(INCLUDES) $(DEPENDENT_DBUS_CFLAGS)

gconfd_2_LDADD = libgconf-$(MAJOR_VERSION).la $(EFENCE) $(INTLLIBS) \
		 $(DEPENDENT_LIBS) $(DEPENDENT_DBUS_LIBS) $(DEPENDENT_ORBIT_LIBS)

gconftool_2_SOURCES = \
	gconftool.c

gconftool_2_LDADD = libgconf-$(MAJOR_VERSION).la $(EFENCE) $(INTLLIBS) $(DEPENDENT_WITH_XML_LIBS)

gconf_sanity_check_2_SOURCES = \
	gconf-sanity-check.c

gconf_sanity_check_2_LDADD = libgconf-$(MAJOR_VERSION).la $(EFENCE) $(INTLLIBS) $(DEPENDENT_WITH_XML_AND_GTK_LIBS)

$(CORBA_SOURCECODE): $(srcdir)/GConfX.idl $(ORBIT_IDL)
	$(ORBIT_IDL) $(srcdir)/GConfX.idl

if OS_WIN32
WIN32_SOURCECODE = dllmain.c
endif

libgconf_2_la_SOURCES = \
	gconf-internals.c	\
	gconf-internals.h	\
	gconf-backend.h		\
	gconf-backend.c		\
	gconf-changeset.c	\
	gconf-error.c		\
	gconf-listeners.c	\
	gconf-locale.h  	\
	gconf-locale.c  	\
	gconf-schema.c		\
	gconf-sources.c		\
	gconf-value.c		\
	gconf.c			\
	gconf-client.c		\
	gconf-enum-types.c	\
	$(WIN32_SOURCECODE)

if HAVE_CORBA
libgconf_2_la_SOURCES += $(CORBA_SOURCECODE)
endif

if HAVE_DBUS
libgconf_2_la_SOURCES += \
	gconf-dbus.c \
	gconf-dbus-utils.c \
	gconf-dbus-utils.h
endif

libgconf_2_la_LDFLAGS = -version-info $(GCONF_CURRENT):$(GCONF_REVISION):$(GCONF_AGE) -no-undefined

libgconf_2_la_LIBADD = $(INTLLIBS) $(DEPENDENT_LIBS) $(DEPENDENT_DBUS_LIBS) $(DEPENDENT_ORBIT_LIBS)

-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = --warn-all --add-include-path=$(srcdir)
INTROSPECTION_COMPILER_ARGS= --includedir=$(srcdir)

if HAVE_INTROSPECTION
introspection_files =				\
	$(gconfinclude_HEADERS)			\
	$(filter-out $(CORBA_SOURCECODE), $(filter %.c,$(libgconf_2_la_SOURCES)))
GConf-2.0.gir: libgconf-2.la Makefile
GConf_2_0_gir_INCLUDES = GObject-2.0
GConf_2_0_gir_CFLAGS = \
	$(INCLUDES)
GConf_2_0_gir_LIBS = libgconf-2.la
GConf_2_0_gir_FILES = $(addprefix $(srcdir)/, $(introspection_files))
GConf_2_0_gir_SCANNERFLAGS = --identifier-prefix=GConf --symbol-prefix=gconf --c-include "gconf/gconf.h"
GConf_2_0_gir_EXPORT_PACKAGES = gconf-2.0
INTROSPECTION_GIRS += GConf-2.0.gir

girdir = $(datadir)/gir-1.0/
gir_DATA = $(INTROSPECTION_GIRS)

typelibsdir = $(libdir)/girepository-1.0/
typelibs_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES += $(gir_DATA) $(typelibs_DATA)
endif

EXTRA_DIST=GConfX.idl default.path.in org.gnome.GConf.service.in gconfmarshal.list regenerate-enum-header.sh regenerate-enum-footer.sh

default.path: $(srcdir)/default.path.in
	sed -e 's,[@]sysgconfdir[@],$(sysgconfdir),g' \
		<$(srcdir)/default.path.in >default.path
org.gnome.GConf.service:  $(srcdir)/org.gnome.GConf.service.in
	sed -e 's,[@]libexecdir[@],$(libexecdir),g' \
		<$(srcdir)/org.gnome.GConf.service.in >org.gnome.GConf.service

servicedir = $(datadir)/dbus-1/services
service_DATA = org.gnome.GConf.service

install-data-local: default.path
	$(mkinstalldirs) $(DESTDIR)$(sysgconfdir)/$(MAJOR_VERSION)
	if test -f $(DESTDIR)$(sysgconfdir)/$(MAJOR_VERSION)/path; then				\
		echo "Existing path configuration file not changed";				\
	else 											\
		$(INSTALL_DATA) default.path $(DESTDIR)$(sysgconfdir)/$(MAJOR_VERSION)/path;	\
	fi

uninstall-local:
	rm -f $(DESTDIR)$(sysgconfdir)/$(MAJOR_VERSION)/path

gconfmarshal.h: gconfmarshal.list $(GLIB_GENMARSHAL)
	$(GLIB_GENMARSHAL) $< --header --prefix=gconf_marshal > $@

gconfmarshal.c: gconfmarshal.list $(GLIB_GENMARSHAL)
	echo "#include \"gconfmarshal.h\"" > $@ && \
	$(GLIB_GENMARSHAL) $< --body --prefix=gconf_marshal >> $@

regenerate-built-sources:
	GCONF_SRCDIR=$(srcdir) $(srcdir)/regenerate-enum-header.sh $(gconf_headers)
	GCONF_SRCDIR=$(srcdir) $(srcdir)/regenerate-enum-footer.sh $(gconf_headers)
