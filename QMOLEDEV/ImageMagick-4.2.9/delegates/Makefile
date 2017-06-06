# Generated automatically from Makefile.in by configure.
# Makefile.in generated automatically by automake 1.4 from Makefile.am

# Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

#  Makefile for installing delegate support files
#
#  Copyright 1998 E. I. du Pont de Nemours and Company
#
#  Permission is hereby granted, free of charge, to any person obtaining a
#  copy of this software and associated documentation files ("ImageMagick"),
#  to deal in ImageMagick without restriction, including without limitation 
#  the rights to use, copy, modify, merge, publish, distribute, sublicense, 
#  and/or sell copies of ImageMagick, and to permit persons to whom the 
#  ImageMagick is furnished to do so, subject to the following conditions:
#   
#  The above copyright notice and this permission notice shall be included in 
#  all copies or substantial portions of ImageMagick. 
#   
#  The software is provided "as is", without warranty of any kind, express or 
#  implied, including but not limited to the warranties of merchantability, 
#  fitness for a particular purpose and noninfringement.In no event shall 
#  E. I. du Pont de Nemours and Company be liable for any claim, damages or 
#  other liability, whether in an action of contract, tort or otherwise,
#  arising from, out of or in connection with ImageMagick or the use or other 
#  dealings in ImageMagick. 
#   
#  Except as contained in this notice, the name of the E. I. du Pont de 
#  Nemours and Company shall not be used in advertising or otherwise to 
#  promote the sale, use or other dealings in ImageMagick without prior 
#  written authorization from the E. I. du Pont de Nemours and Company. 
#
#

# Don't require all the GNU mandated files


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
includedir = ${prefix}/include/magick
oldincludedir = /usr/include

DESTDIR =

pkgdatadir = $(datadir)/ImageMagick
pkglibdir = $(libdir)/ImageMagick
pkgincludedir = $(includedir)/ImageMagick

top_builddir = ..

ACLOCAL = aclocal
AUTOCONF = autoconf
AUTOMAKE = automake
AUTOHEADER = autoheader

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL} $(AM_INSTALL_PROGRAM_FLAGS)
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL_PROGRAM}
transform = s,x,x,

NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
build_alias = arm-apple-darwin11.0.0
build_triplet = arm-apple-darwin11.0.0
host_alias = arm-apple-darwin11.0.0
host_triplet = arm-apple-darwin11.0.0
target_alias = arm-apple-darwin11.0.0
target_triplet = arm-apple-darwin11.0.0
AS = 
BZIPDelegate = /bin/bzip2
BrowseDelegate = netscape
CC = /usr/bin/gcc
CGMDecodeDelegate = ralcgm
CPP = /usr/bin/gcc -E
CPPFLAGS = -I/usr/local/include
CatDelegate = /bin/cat
ConvertDelegate = /usr/local/bin/convert
DELEGATES =  jpeg png tiff x zlib
DLLTOOL = 
DOTDecodeDelegate = dot
DVIDecodeDelegate = dvips
EXEEXT = 
EchoDelegate = /bin/echo
EditorDelegate = /usr/local/bin/xterm
FIGDecodeDelegate = fig2dev
GSColorDevice = 
GSEPSDevice = 
GSMonoDevice = 
GSPDFDevice = 
GSPSDevice = 
GnuplotDecodeDelegate = gnuplot
HPGLDecodeDelegate = hp2xx
HTMLDecodeDelegate = html2ps
ILBMDecodeDelegate = ilbmtoppm
ILBMEncodeDelegate = ppmtoilbm
LD = /usr/bin/ld
LDFLAGS = -L/usr/local/lib
LIBTOOL = $(SHELL) $(top_builddir)/libtool
LN_S = ln -s
LPDelegate = lp
LZWDecodeDelegate = /bin/uncompress
LZWEncodeDelegate = /usr/bin/compress
LaunchDelegate = gimp
MAGICKLIB = -L/usr/local/lib -lMagick
MAINT = #
MAJOR_VERSION = 4
MAKEINFO = makeinfo
MANDelegate = /usr/local/bin/groff
MICRO_VERSION = 9
MINOR_VERSION = 2
MPEGDecodeDelegate = mpeg2decode
MPEGEncodeDelegate = mpeg2encode
MVDelegate = /bin/mv
MogrifyDelegate = /usr/local/bin/mogrify
NM = /usr/bin/nm -p
OBJDUMP = 
OBJEXT = o
PACKAGE = ImageMagick
PERL = 
PGPDecodeDelegate = pgpv
POVDelegate = povray
PSDelegate = gs
RADDecodeDelegate = ra_ppm
RANLIB = ranlib
RLEEncodeDelegate = rawtorle
RMDelegate = /bin/rm
SCANDecodeDelegate = scanimage
ShowImageDelegate = /usr/local/bin/display
TXTDelegate = enscript
VERSION = 4.2.9
WMFDecodeDelegate = wmftogif
WWWDecodeDelegate = GET
XCFDecodeDelegate = xcftopnm
XTP_DEFS =  -DXTP_FTP="\"ftp\""
XTP_FTP = ftp
XTP_LIBS =   
X_CFLAGS = 
X_EXTRA_LIBS = 
X_LIBS = 
X_PRE_LIBS = -lSM -lICE
ZipDelegate = /bin/gzip

AUTOMAKE_OPTIONS = 1.2 foreign

# Architecture-independent data files
pkgdata_DATA = delegates.mgk

# Additional files to distribute
EXTRA_DIST = delegates.mgk.in delegates.mgk mac.mgk nt.mgk unix.mgk vms.mgk win95.mgk
mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_HEADER = ../magick/config.h
CONFIG_CLEAN_FILES =  delegates.mgk
DATA =  $(pkgdata_DATA)

DIST_COMMON =  Makefile.am Makefile.in delegates.mgk.in


DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) $(TEXINFOS) $(EXTRA_DIST)

TAR = gtar
GZIP_ENV = --best
all: all-redirect
.SUFFIXES:
$(srcdir)/Makefile.in: # Makefile.am $(top_srcdir)/configure.in $(ACLOCAL_M4) 
	cd $(top_srcdir) && $(AUTOMAKE) --foreign --include-deps delegates/Makefile

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

delegates.mgk: $(top_builddir)/config.status delegates.mgk.in
	cd $(top_builddir) && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

install-pkgdataDATA: $(pkgdata_DATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)
	@list='$(pkgdata_DATA)'; for p in $$list; do \
	  if test -f $(srcdir)/$$p; then \
	    echo " $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(pkgdatadir)/$$p"; \
	    $(INSTALL_DATA) $(srcdir)/$$p $(DESTDIR)$(pkgdatadir)/$$p; \
	  else if test -f $$p; then \
	    echo " $(INSTALL_DATA) $$p $(DESTDIR)$(pkgdatadir)/$$p"; \
	    $(INSTALL_DATA) $$p $(DESTDIR)$(pkgdatadir)/$$p; \
	  fi; fi; \
	done

uninstall-pkgdataDATA:
	@$(NORMAL_UNINSTALL)
	list='$(pkgdata_DATA)'; for p in $$list; do \
	  rm -f $(DESTDIR)$(pkgdatadir)/$$p; \
	done
tags: TAGS
TAGS:


distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = delegates

distdir: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$/$$file $(distdir)/$$file; \
	  else \
	    test -f $(distdir)/$$file \
	    || ln $$d/$$file $(distdir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(distdir)/$$file || :; \
	  fi; \
	done
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

install-data-am: install-pkgdataDATA
install-data: install-data-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
install: install-am
uninstall-am: uninstall-pkgdataDATA
uninstall: uninstall-am
all-am: Makefile $(DATA)
all-redirect: all-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) AM_INSTALL_PROGRAM_FLAGS=-s install
installdirs:
	$(mkinstalldirs)  $(DESTDIR)$(pkgdatadir)


mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*

maintainer-clean-generic:
mostlyclean-am:  mostlyclean-generic

mostlyclean: mostlyclean-am

clean-am:  clean-generic mostlyclean-am

clean: clean-am

distclean-am:  distclean-generic clean-am
	-rm -f libtool

distclean: distclean-am

maintainer-clean-am:  maintainer-clean-generic distclean-am
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

maintainer-clean: maintainer-clean-am

.PHONY: uninstall-pkgdataDATA install-pkgdataDATA tags distdir info-am \
info dvi-am dvi check check-am installcheck-am installcheck \
install-exec-am install-exec install-data-am install-data install-am \
install uninstall-am uninstall all-redirect all-am all installdirs \
mostlyclean-generic distclean-generic clean-generic \
maintainer-clean-generic clean mostlyclean distclean maintainer-clean


# Ensure that processed delegates file is not distributed
#dist-hook:
#	$(RM) $(distdir)/delegates.mgk

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
