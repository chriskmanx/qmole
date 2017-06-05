# Makefile.in generated automatically by automake 1.4-p6 from Makefile.am

# Copyright (C) 1994, 1995-8, 1999, 2001 Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

# The name of the module, e.g. 'glib'.


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

DOC_MODULE = linc

# The top-level SGML file. Change it if you want.
DOC_MAIN_SGML_FILE = $(DOC_MODULE)-docs.sgml

# The directory containing the source code. Relative to $(srcdir).
# gtk-doc will search all .c & .h files beneath here for inline comments
# documenting functions and macros.
DOC_SOURCE_DIR = ..

# Extra options to supply to gtkdoc-scan.
SCAN_OPTIONS = 

# Extra options to supply to gtkdoc-mkdb.
MKDB_OPTIONS = 

# Extra options to supply to gtkdoc-fixref.
FIXXREF_OPTIONS = 

# Used for dependencies.
HFILE_GLOB = 
CFILE_GLOB = 

# Header files to ignore when scanning.
IGNORE_HFILES = config.h acconfig.h linc-private.h

# Images to copy into HTML directory.
HTML_IMAGES = 

# Extra SGML files that are included by $(DOC_MAIN_SGML_FILE).
content_files = 

# Other files to distribute.
extra_files = 

# CFLAGS and LDFLAGS for compiling scan program. Only needed if your app/lib
# contains GtkObjects/GObjects and you want to document signals and properties.
GTKDOC_CFLAGS = 
GTKDOC_LIBS = 

GTKDOC_CC = $(LIBTOOL) --mode=compile $(CC)
GTKDOC_LD = $(LIBTOOL) --mode=link $(CC)

# If you need to override some of the declarations, place them in the
# $(DOC_MODULE)-overrides.txt file and uncomment the second line here.
DOC_OVERRIDES = 
#DOC_OVERRIDES = $(DOC_MODULE)-overrides.txt

###########################################################################
# Everything below here is generic and you shouldn't need to change it.
###########################################################################

TARGET_DIR = $(HTML_DIR)/$(DOC_MODULE)

EXTRA_DIST = \
	$(content_files)		\
	$(extra_files)			\
	$(HTML_IMAGES)			\
	$(DOC_MAIN_SGML_FILE)		\
	$(DOC_MODULE).types		\
	$(DOC_MODULE)-sections.txt	\
	$(DOC_OVERRIDES)


DOC_STAMPS = scan-build.stamp tmpl-build.stamp sgml-build.stamp html-build.stamp \
	   $(srcdir)/tmpl.stamp $(srcdir)/sgml.stamp $(srcdir)/html.stamp


SCANOBJ_FILES = \
	$(DOC_MODULE).args 	\
	$(DOC_MODULE).hierarchy \
	$(DOC_MODULE).signals

mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_HEADER = ../config.h
CONFIG_CLEAN_FILES = 
DIST_COMMON =  Makefile.am Makefile.in


DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) $(TEXINFOS) $(EXTRA_DIST)

TAR = gtar
GZIP_ENV = --best
all: all-redirect
.SUFFIXES:
$(srcdir)/Makefile.in: # Makefile.am $(top_srcdir)/configure.in $(ACLOCAL_M4) 
	cd $(top_srcdir) && $(AUTOMAKE) --gnu --include-deps docs/Makefile

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

tags: TAGS
TAGS:


distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = docs

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
	$(MAKE) $(AM_MAKEFLAGS) top_distdir="$(top_distdir)" distdir="$(distdir)" dist-hook
info-am:
info: info-am
dvi-am:
dvi: dvi-am
check-am: all-am
check: check-am
installcheck-am:
installcheck: installcheck-am
install-exec-am:
install-exec: install-exec-am

install-data-am: install-data-local
install-data: install-data-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
install: install-am
uninstall-am:
uninstall: uninstall-am
all-am: Makefile all-local
all-redirect: all-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) AM_INSTALL_PROGRAM_FLAGS=-s install
installdirs:


mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*

maintainer-clean-generic:
mostlyclean-am:  mostlyclean-generic

mostlyclean: mostlyclean-am

clean-am:  clean-generic mostlyclean-am clean-local

clean: clean-am

distclean-am:  distclean-generic clean-am
	-rm -f libtool

distclean: distclean-am

maintainer-clean-am:  maintainer-clean-generic distclean-am \
		maintainer-clean-local
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

maintainer-clean: maintainer-clean-am

.PHONY: tags distdir info-am info dvi-am dvi check check-am \
installcheck-am installcheck install-exec-am install-exec \
install-data-local install-data-am install-data install-am install \
uninstall-am uninstall all-local all-redirect all-am all installdirs \
mostlyclean-generic distclean-generic clean-generic \
maintainer-clean-generic clean mostlyclean distclean maintainer-clean


#all-local: html-build.stamp

#### scan ####

#scan-build.stamp: $(HFILE_GLOB)
#	@echo '*** Scanning header files ***'
#	if grep -l '^..*$$' $(srcdir)/$(DOC_MODULE).types > /dev/null ; then \
#	    CC="$(GTKDOC_CC)" LD="$(GTKDOC_LD)" CFLAGS="$(GTKDOC_CFLAGS)" LDFLAGS="$(GTKDOC_LIBS)" gtkdoc-scanobj --module=$(DOC_MODULE) --output-dir=$(srcdir) ; \
#	else \
#	    cd $(srcdir) ; \
#	    for i in $(SCANOBJ_FILES) ; do \
#               test -f $$i || touch $$i ; \
#	    done \
#	fi
#	cd $(srcdir) && \
#	  gtkdoc-scan --module=$(DOC_MODULE) --source-dir=$(DOC_SOURCE_DIR) --ignore-headers="$(IGNORE_HFILES)" $(SCAN_OPTIONS) $(EXTRA_HFILES)
#	touch scan-build.stamp

#$(DOC_MODULE)-decl.txt $(SCANOBJ_FILES): scan-build.stamp
#	@true

#### templates ####

#tmpl-build.stamp: $(DOC_MODULE)-decl.txt $(SCANOBJ_FILES) $(DOC_MODULE)-sections.txt $(DOC_OVERRIDES)
#	@echo '*** Rebuilding template files ***'
#	cd $(srcdir) && gtkdoc-mktmpl --module=$(DOC_MODULE)
#	touch tmpl-build.stamp

#tmpl.stamp: tmpl-build.stamp
#	@true

#### sgml ####

#sgml-build.stamp: tmpl.stamp $(CFILE_GLOB) $(srcdir)/tmpl/*.sgml
#	@echo '*** Building SGML ***'
#	cd $(srcdir) && \
#	gtkdoc-mkdb --module=$(DOC_MODULE) --source-dir=$(DOC_SOURCE_DIR) --main-sgml-file=$(DOC_MAIN_SGML_FILE) $(MKDB_OPTIONS)
#	touch sgml-build.stamp

#sgml.stamp: sgml-build.stamp
#	@true

#### html ####

#html-build.stamp: sgml.stamp $(DOC_MAIN_SGML_FILE) $(content_files)
#	@echo '*** Building HTML ***'
#	test -d $(srcdir)/html || mkdir $(srcdir)/html
#	cd $(srcdir)/html && gtkdoc-mkhtml $(DOC_MODULE) ../$(DOC_MAIN_SGML_FILE)
#	test "x$(HTML_IMAGES)" = "x" || ( cd $(srcdir) && cp $(HTML_IMAGES) html )
#	@echo '-- Fixing Crossreferences' 
#	cd $(srcdir) && gtkdoc-fixxref --module-dir=html --html-dir=$(HTML_DIR) $(FIXXREF_OPTIONS)
#	touch html-build.stamp

##############

clean-local:
	rm -f *~ *.bak $(SCANOBJ_FILES) *-unused.txt $(DOC_STAMPS)

maintainer-clean-local: clean
	cd $(srcdir) && rm -rf sgml html $(DOC_MODULE)-decl-list.txt $(DOC_MODULE)-decl.txt

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(TARGET_DIR)
	(installfiles=`echo $(srcdir)/html/*.html`; \
	if test "$$installfiles" = '$(srcdir)/html/*.html'; \
	then echo '-- Nothing to install' ; \
	else \
	  for i in $$installfiles; do \
	    echo '-- Installing '$$i ; \
	    $(INSTALL_DATA) $$i $(DESTDIR)$(TARGET_DIR); \
	  done; \
	  echo '-- Installing $(srcdir)/html/index.sgml' ; \
	  $(INSTALL_DATA) $(srcdir)/html/index.sgml $(DESTDIR)$(TARGET_DIR); \
	fi)

#
# Require gtk-doc when making dist
#
#dist-check-gtkdoc:
dist-check-gtkdoc:
	@echo "*** gtk-doc must be installed and enabled in order to make dist"
	@false

dist-hook: dist-check-gtkdoc html-build.stamp dist-hook-local
	mkdir $(distdir)/tmpl
	mkdir $(distdir)/sgml
	mkdir $(distdir)/html
	-cp $(srcdir)/tmpl/*.sgml $(distdir)/tmpl
	-cp $(srcdir)/sgml/*.sgml $(distdir)/sgml
	-cp $(srcdir)/html/index.sgml $(distdir)/html
	-cp $(srcdir)/html/*.html $(srcdir)/html/*.css $(distdir)/html

	images=$(HTML_IMAGES) ;    	      \
	for i in $$images ; do		      \
	  cp $(srcdir)/$$i $(distdir)/html ;  \
	done

.PHONY : dist-hook-local

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
