# Makefile.in generated automatically by automake 1.4-p6 from Makefile.am

# Copyright (C) 1994, 1995-8, 1999, 2001 Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.


SHELL = /bin/sh

srcdir = .
top_srcdir = ..

prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${prefix}/info
mandir = ${prefix}/man
includedir = ${prefix}/include
oldincludedir = /usr/include

DESTDIR =

pkgdatadir = $(datadir)/linc
pkglibdir = $(libdir)/linc
pkgincludedir = $(includedir)/linc

top_builddir = ..

ACLOCAL = /home/chris/linc-0.7.1/missing aclocal-1.4 
AUTOCONF = autoconf
AUTOMAKE = /home/chris/linc-0.7.1/missing automake-1.4
AUTOHEADER = autoheader

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL} $(AM_INSTALL_PROGRAM_FLAGS)
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL}
transform = s,x,x,

NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
host_alias = 
host_triplet = arm-apple-darwin11.0.0
AS = @AS@
CC = /usr/bin/gcc
DLLTOOL = @DLLTOOL@
ECHO = echo
EXEEXT = 
GLIB_REQUIRED = 1.3.11
GTKDOC = false
HTML_DIR = ${datadir}/gtk-doc/html
LIBTOOL = $(SHELL) $(top_builddir)/libtool
LINC_CFLAGS = -D_REENTRANT -I/usr/local/include/glib-2.0 -I/usr/local/lib/glib-2.0/include  
LINC_LIBS = -L/usr/local/lib -lgobject-2.0 -lgthread-2.0 -lglib-2.0 -lintl  
LINC_MAJOR_VERSION = 0
LINC_MICRO_VERSION = 1
LINC_MINOR_VERSION = 7
LINC_SSL_SUPPORT = 0
LINC_VERSION = 0.7.1
LN_S = ln -s
LT_VERSION = 1:0:0
MAINT = #
MAKEINFO = makeinfo
OBJDUMP = @OBJDUMP@
OBJEXT = o
PACKAGE = linc
PKG_CONFIG = /usr/local/bin/pkg-config
RANLIB = ranlib
SSL_CFLAGS = 
SSL_LIBS = 
STRIP = strip
VERSION = 0.7.1
WARN_CFLAGS = -Wall -Wunused -Wmissing-prototypes -Wmissing-declarations

lib_LTLIBRARIES = liblinc.la

bin_PROGRAMS = linc-cleanup-sockets

INCLUDES = \
	-I$(top_builddir)/include  \
	-I$(top_srcdir)/include    \
	$(LINC_CFLAGS)             \
	$(SSL_CFLAGS)              \
	$(WARN_CFLAGS)             \
	-DG_DISABLE_DEPRECATED	   \
	-D_GNU_SOURCE


liblinc_la_SOURCES = \
	linc.c			\
	linc-connection.c	\
	linc-debug.h		\
	linc-compat.h		\
	linc-private.h		\
	linc-protocols.c	\
	linc-server.c		\
	linc-source.c


liblinc_la_LDFLAGS = \
	$(LINC_LIBS)                 \
	$(SSL_LIBS)                  \
	-version-info $(LT_VERSION)


linc_cleanup_sockets_SOURCES = \
	cleanup.c


linc_cleanup_sockets_LDFLAGS = \
	$(LINC_LIBS)

mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_HEADER = ../config.h
CONFIG_CLEAN_FILES = 
LTLIBRARIES =  $(lib_LTLIBRARIES)


DEFS = -DHAVE_CONFIG_H -I. -I$(srcdir) -I..
CPPFLAGS = 
LDFLAGS = 
LIBS = 
liblinc_la_LIBADD = 
liblinc_la_OBJECTS =  linc.lo linc-connection.lo linc-protocols.lo \
linc-server.lo linc-source.lo
bin_PROGRAMS =  linc-cleanup-sockets$(EXEEXT)
PROGRAMS =  $(bin_PROGRAMS)

linc_cleanup_sockets_OBJECTS =  cleanup.$(OBJEXT)
linc_cleanup_sockets_LDADD = $(LDADD)
linc_cleanup_sockets_DEPENDENCIES = 
CFLAGS = -g -O2
COMPILE = $(CC) $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)
LTCOMPILE = $(LIBTOOL) --mode=compile $(CC) $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)
CCLD = $(CC)
LINK = $(LIBTOOL) --mode=link $(CCLD) $(AM_CFLAGS) $(CFLAGS) $(LDFLAGS) -o $@
DIST_COMMON =  Makefile.am Makefile.in


DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) $(TEXINFOS) $(EXTRA_DIST)

TAR = gtar
GZIP_ENV = --best
SOURCES = $(liblinc_la_SOURCES) $(linc_cleanup_sockets_SOURCES)
OBJECTS = $(liblinc_la_OBJECTS) $(linc_cleanup_sockets_OBJECTS)

all: all-redirect
.SUFFIXES:
.SUFFIXES: .S .c .lo .o .obj .s
$(srcdir)/Makefile.in: # Makefile.am $(top_srcdir)/configure.in $(ACLOCAL_M4) 
	cd $(top_srcdir) && $(AUTOMAKE) --gnu --include-deps src/Makefile

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status


mostlyclean-libLTLIBRARIES:

clean-libLTLIBRARIES:
	-test -z "$(lib_LTLIBRARIES)" || rm -f $(lib_LTLIBRARIES)

distclean-libLTLIBRARIES:

maintainer-clean-libLTLIBRARIES:

install-libLTLIBRARIES: $(lib_LTLIBRARIES)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(libdir)
	@list='$(lib_LTLIBRARIES)'; for p in $$list; do \
	  if test -f $$p; then \
	    echo "$(LIBTOOL)  --mode=install $(INSTALL) $$p $(DESTDIR)$(libdir)/$$p"; \
	    $(LIBTOOL)  --mode=install $(INSTALL) $$p $(DESTDIR)$(libdir)/$$p; \
	  else :; fi; \
	done

uninstall-libLTLIBRARIES:
	@$(NORMAL_UNINSTALL)
	list='$(lib_LTLIBRARIES)'; for p in $$list; do \
	  $(LIBTOOL)  --mode=uninstall rm -f $(DESTDIR)$(libdir)/$$p; \
	done

.c.o:
	$(COMPILE) -c $<

# FIXME: We should only use cygpath when building on Windows,
# and only if it is available.
.c.obj:
	$(COMPILE) -c `cygpath -w $<`

.s.o:
	$(COMPILE) -c $<

.S.o:
	$(COMPILE) -c $<

mostlyclean-compile:
	-rm -f *.o core *.core
	-rm -f *.$(OBJEXT)

clean-compile:

distclean-compile:
	-rm -f *.tab.c

maintainer-clean-compile:

.c.lo:
	$(LIBTOOL) --mode=compile $(COMPILE) -c $<

.s.lo:
	$(LIBTOOL) --mode=compile $(COMPILE) -c $<

.S.lo:
	$(LIBTOOL) --mode=compile $(COMPILE) -c $<

mostlyclean-libtool:
	-rm -f *.lo

clean-libtool:
	-rm -rf .libs _libs

distclean-libtool:

maintainer-clean-libtool:

liblinc.la: $(liblinc_la_OBJECTS) $(liblinc_la_DEPENDENCIES)
	$(LINK) -rpath $(libdir) $(liblinc_la_LDFLAGS) $(liblinc_la_OBJECTS) $(liblinc_la_LIBADD) $(LIBS)

mostlyclean-binPROGRAMS:

clean-binPROGRAMS:
	-test -z "$(bin_PROGRAMS)" || rm -f $(bin_PROGRAMS)

distclean-binPROGRAMS:

maintainer-clean-binPROGRAMS:

install-binPROGRAMS: $(bin_PROGRAMS)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(bindir)
	@list='$(bin_PROGRAMS)'; for p in $$list; do \
	  if test -f $$p; then \
	    echo " $(LIBTOOL)  --mode=install $(INSTALL_PROGRAM) $$p $(DESTDIR)$(bindir)/`echo $$p|sed 's/$(EXEEXT)$$//'|sed '$(transform)'|sed 's/$$/$(EXEEXT)/'`"; \
	    $(LIBTOOL)  --mode=install $(INSTALL_PROGRAM) $$p $(DESTDIR)$(bindir)/`echo $$p|sed 's/$(EXEEXT)$$//'|sed '$(transform)'|sed 's/$$/$(EXEEXT)/'`; \
	  else :; fi; \
	done

uninstall-binPROGRAMS:
	@$(NORMAL_UNINSTALL)
	list='$(bin_PROGRAMS)'; for p in $$list; do \
	  rm -f $(DESTDIR)$(bindir)/`echo $$p|sed 's/$(EXEEXT)$$//'|sed '$(transform)'|sed 's/$$/$(EXEEXT)/'`; \
	done

linc-cleanup-sockets$(EXEEXT): $(linc_cleanup_sockets_OBJECTS) $(linc_cleanup_sockets_DEPENDENCIES)
	@rm -f linc-cleanup-sockets$(EXEEXT)
	$(LINK) $(linc_cleanup_sockets_LDFLAGS) $(linc_cleanup_sockets_OBJECTS) $(linc_cleanup_sockets_LDADD) $(LIBS)

tags: TAGS

ID: $(HEADERS) $(SOURCES) $(LISP)
	list='$(SOURCES) $(HEADERS)'; \
	unique=`for i in $$list; do echo $$i; done | \
	  awk '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	here=`pwd` && cd $(srcdir) \
	  && mkid -f$$here/ID $$unique $(LISP)

TAGS:  $(HEADERS) $(SOURCES)  $(TAGS_DEPENDENCIES) $(LISP)
	tags=; \
	here=`pwd`; \
	list='$(SOURCES) $(HEADERS)'; \
	unique=`for i in $$list; do echo $$i; done | \
	  awk '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	test -z "$(ETAGS_ARGS)$$unique$(LISP)$$tags" \
	  || (cd $(srcdir) && etags $(ETAGS_ARGS) $$tags  $$unique $(LISP) -o $$here/TAGS)

mostlyclean-tags:

clean-tags:

distclean-tags:
	-rm -f TAGS ID

maintainer-clean-tags:

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = src

distdir: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(distdir)/$$file; \
	  else \
	    test -f $(distdir)/$$file \
	    || ln $$d/$$file $(distdir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(distdir)/$$file || :; \
	  fi; \
	done
cleanup.o: cleanup.c ../config.h linc-compat.h
linc-connection.lo linc-connection.o : linc-connection.c ../config.h \
	linc-debug.h linc-private.h ../include/linc/linc.h \
	../include/linc/linc-config.h ../include/linc/linc-types.h \
	../include/linc/linc-protocol.h \
	../include/linc/linc-connection.h ../include/linc/linc-server.h
linc-protocols.lo linc-protocols.o : linc-protocols.c ../config.h \
	linc-compat.h ../include/linc/linc-protocol.h \
	../include/linc/linc-types.h ../include/linc/linc-connection.h \
	linc-private.h ../include/linc/linc.h \
	../include/linc/linc-config.h ../include/linc/linc-server.h \
	linc-debug.h
linc-server.lo linc-server.o : linc-server.c ../config.h \
	../include/linc/linc.h ../include/linc/linc-config.h \
	../include/linc/linc-types.h ../include/linc/linc-protocol.h \
	../include/linc/linc-connection.h ../include/linc/linc-server.h \
	linc-debug.h linc-private.h linc-compat.h
linc-source.lo linc-source.o : linc-source.c linc-debug.h linc-private.h \
	../config.h ../include/linc/linc.h \
	../include/linc/linc-config.h ../include/linc/linc-types.h \
	../include/linc/linc-protocol.h \
	../include/linc/linc-connection.h ../include/linc/linc-server.h
linc.lo linc.o : linc.c linc-debug.h linc-private.h ../config.h \
	../include/linc/linc.h ../include/linc/linc-config.h \
	../include/linc/linc-types.h ../include/linc/linc-protocol.h \
	../include/linc/linc-connection.h ../include/linc/linc-server.h

info-am:
info: info-am
dvi-am:
dvi: dvi-am
check-am: all-am
check: check-am
installcheck-am:
installcheck: installcheck-am
install-exec-am: install-libLTLIBRARIES install-binPROGRAMS
install-exec: install-exec-am

install-data-am:
install-data: install-data-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
install: install-am
uninstall-am: uninstall-libLTLIBRARIES uninstall-binPROGRAMS
uninstall: uninstall-am
all-am: Makefile $(LTLIBRARIES) $(PROGRAMS)
all-redirect: all-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) AM_INSTALL_PROGRAM_FLAGS=-s install
installdirs:
	$(mkinstalldirs)  $(DESTDIR)$(libdir) $(DESTDIR)$(bindir)


mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*

maintainer-clean-generic:
mostlyclean-am:  mostlyclean-libLTLIBRARIES mostlyclean-compile \
		mostlyclean-libtool mostlyclean-binPROGRAMS \
		mostlyclean-tags mostlyclean-generic

mostlyclean: mostlyclean-am

clean-am:  clean-libLTLIBRARIES clean-compile clean-libtool \
		clean-binPROGRAMS clean-tags clean-generic \
		mostlyclean-am

clean: clean-am

distclean-am:  distclean-libLTLIBRARIES distclean-compile \
		distclean-libtool distclean-binPROGRAMS distclean-tags \
		distclean-generic clean-am
	-rm -f libtool

distclean: distclean-am

maintainer-clean-am:  maintainer-clean-libLTLIBRARIES \
		maintainer-clean-compile maintainer-clean-libtool \
		maintainer-clean-binPROGRAMS maintainer-clean-tags \
		maintainer-clean-generic distclean-am
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

maintainer-clean: maintainer-clean-am

.PHONY: mostlyclean-libLTLIBRARIES distclean-libLTLIBRARIES \
clean-libLTLIBRARIES maintainer-clean-libLTLIBRARIES \
uninstall-libLTLIBRARIES install-libLTLIBRARIES mostlyclean-compile \
distclean-compile clean-compile maintainer-clean-compile \
mostlyclean-libtool distclean-libtool clean-libtool \
maintainer-clean-libtool mostlyclean-binPROGRAMS distclean-binPROGRAMS \
clean-binPROGRAMS maintainer-clean-binPROGRAMS uninstall-binPROGRAMS \
install-binPROGRAMS tags mostlyclean-tags distclean-tags clean-tags \
maintainer-clean-tags distdir info-am info dvi-am dvi check check-am \
installcheck-am installcheck install-exec-am install-exec \
install-data-am install-data install-am install uninstall-am uninstall \
all-redirect all-am all installdirs mostlyclean-generic \
distclean-generic clean-generic maintainer-clean-generic clean \
mostlyclean distclean maintainer-clean


# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
