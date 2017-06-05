#
# $Header: /cvsroot/lesstif/lesstif/Makefile.am,v 1.61 2005/03/31 19:06:34 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in configure libtool config.guess config.sub \
                     ltconfig ltmain.sh aclocal.m4 config.h.in install-sh \
		     missing mkinstalldirs \
		     BUG-REPORTING INSTALL.txt ReleaseNotes.txt

SUBDIRS	     = . scripts include lib clients doc @BuildTestDir@ autopackage

DIST_SUBDIRS = scripts include lib clients doc test autopackage

#
# Install some information files
#
# Lets put the informational stuff in a sub directory to avoid
# polluting /usr/local for default installs.
#
rootdir=	$(prefix)/LessTif
dist_root_DATA=	AUTHORS  BUG-REPORTING  COPYING  COPYING.LIB  CREDITS \
                FAQ  INSTALL.txt  README  ReleaseNotes.html ReleaseNotes.txt


@MAINT@changelog:
@MAINT@	@ (cd $(srcdir); cvs -Q -z9 log -rrelease_@LAST_MAJOR_VERSION@_@LAST_MINOR_VERSION@_@LAST_PICO_VERSION@: \
@MAINT@                  acconfig.h acinclude.m4 configure.in Makefile.am \
@MAINT@                  include \
@MAINT@                  lib \
@MAINT@                  clients \
@MAINT@                  doc/lessdox ) \
@MAINT@	    | $(srcdir)/scripts/parselog release_@LAST_MAJOR_VERSION@_@LAST_MINOR_VERSION@_@LAST_PICO_VERSION@ >$(srcdir)/ChangeLog

docdir=	$(rootdir)/doc
doc_DATA=	ReleaseNotes.html

all-local: BUG-REPORTING  INSTALL.txt

BUG-REPORTING: $(top_srcdir)/doc/www.lesstif.org/bugs.html
	$(HTML2TEXT) $(srcdir)/doc/www.lesstif.org/bugs.html >$@

ReleaseNotes.txt: $(srcdir)/ReleaseNotes.html
	$(HTML2TEXT) $(srcdir)/ReleaseNotes.html > $@

# auto* can't handle a target named "INSTALL" properly ...
INSTALL.txt: $(srcdir)/doc/www.lesstif.org/INSTALL.html
	$(HTML2TEXT) $(srcdir)/doc/www.lesstif.org/INSTALL.html >$@

#
# Daily is a target to create a normal distribution but then rename it.
#
daily:	dist
#	mv lesstif-@MAJOR_VERSION@.@MINOR_VERSION@.@PICO_VERSION@.tar.gz lesstif-daily.tar.gz
	tar xfz lesstif-@MAJOR_VERSION@.@MINOR_VERSION@.@PICO_VERSION@.tar.gz
	mv lesstif-@MAJOR_VERSION@.@MINOR_VERSION@.@PICO_VERSION@ lesstif-daily
	tar cfz lesstif-daily.tar.gz lesstif-daily
	-rm -rf lesstif-daily

#
# Some convenient targets for users of Exuberant ctags
#

if Have_Exctags

# Specify options
#  - for recursive search
#  - to prevent to follow our symbolic links across Motif/library versions
#  - to avoid picking up Makefiles, etc.
#    (selecting C++ support picks up the .h files. We dont use C++.)
CTAGS_CMD=ctags -R --links=no --languages=c,c++,yacc

# One target for each version and an unified one
ctags-1.2:
	$(CTAGS_CMD) \
        include/*.h \
        include/Motif-1.2 \
        lib/Xm \
        lib/Mrm \
        lib/Uil \
        clients/Motif-1.2

ctags-2.0:
	$(CTAGS_CMD) \
        include/*.h \
        include/Motif-2.0 \
        lib/Xm-2.0 \
        lib/Mrm-2.0 \
        lib/Uil-2.0 \
        clients/Motif-2.0

ctags-2.1:
	$(CTAGS_CMD) \
        include/*.h \
        include/Motif-2.1 \
        lib/Xm-2.1 \
        lib/Mrm-2.1 \
        lib/Uil-2.1 \
        lib/Dt \
        clients/Motif-2.1

ctags-all:
	$(CTAGS_CMD) \
        include/*.h \
        include/Motif-2.1  include/Motif-2.0  include/Motif-1.2 \
        lib/Xm-2.1         lib/Xm-2.0         lib/Xm \
        lib/Mrm-2.1        lib/Mrm-2.0        lib/Mrm \
        lib/Uil-2.1        lib/Uil-2.0        lib/Uil \
        lib/Dt \
        clients/Motif-2.1  clients/Motif-2.0  clients/Motif-1.2 \
        test/common
endif
