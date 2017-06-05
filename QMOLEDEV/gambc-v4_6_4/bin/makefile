# makefile for Gambit-C bin directory.

# Copyright (c) 1994-2009 by Marc Feeley, All Rights Reserved.

herefromroot = bin
rootfromhere = ..
SUBDIRS =

PACKAGE_SHORTNAME = gambc
PACKAGE_NAME = Gambit-C
PACKAGE_VERSION = v4.6.4
PACKAGE_STRING = Gambit-C v4.6.4
PACKAGE_BUGREPORT = gambit@iro.umontreal.ca
PACKAGE_TARNAME = gambc-v4_6_4



srcdir = .

srcdirpfx = 

C_COMPILER = /usr/bin/gcc
C_PREPROC = /usr/bin/gcc -E
FLAGS_OBJ =    -no-cpp-precomp -Wno-unused -Wno-write-strings -O2 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common 
FLAGS_DYN =    -bundle  -no-cpp-precomp -Wno-unused -Wno-write-strings -O2 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common   -flat_namespace -undefined suppress
FLAGS_LIB =    -dynamiclib -install_name $(libdir)/$(LIBRARY)   -flat_namespace -undefined suppress
FLAGS_EXE =     -no-cpp-precomp -Wno-unused -Wno-write-strings -O2 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common  
DEFS = -DHAVE_CONFIG_H
LIBS = 

GAMBCLIB_DEFS =  -D___GAMBCDIR="\"/usr/local/Gambit-C\"" -D___SYS_TYPE_CPU="\"arm\"" -D___SYS_TYPE_VENDOR="\"apple\"" -D___SYS_TYPE_OS="\"darwin10.4.0\"" -D___CONFIGURE_COMMAND="\"./configure 'CC=/usr/bin/gcc'"\" -D___OBJ_EXTENSION="\".o\"" -D___EXE_EXTENSION="\"\"" -D___BAT_EXTENSION="\"\""
LIB_PREFIX = lib
LIB_EXTENSION = .a
GAMBCLIB = gambc
GAMBCGSCLIB = gambcgsc
GAMBCGSILIB = gambcgsi
INSTALL = $(rootfromhere)/install-sh -c
INSTALL_DATA = $(rootfromhere)/install-sh -c -m 644
INSTALL_LIB = $(rootfromhere)/install-sh -c -m 644
INSTALL_PROGRAM = $(rootfromhere)/install-sh -c -m 755
LN_S = ln -s
RANLIB = ranlib
RC = git
GIT = git
HG = hg

prefix = /usr/local/Gambit-C
exec_prefix = ${prefix}
includedir = ${prefix}/include
libdir = ${prefix}/lib
bindir = ${prefix}/bin
docdir = ${prefix}/doc
infodir = ${prefix}/info
emacsdir = ${datadir}/emacs/site-lisp
libexecdir = ${exec_prefix}/libexec
datarootdir = ${prefix}/share
datadir = ${prefix}/share
htmldir = ${docdir}
dvidir = ${docdir}
pdfdir = ${docdir}
psdir = ${docdir}
localedir = ${datarootdir}/locale
mandir = ${datarootdir}/man

.SUFFIXES:

GSI_EXECUTABLE = gsi
GSC_EXECUTABLE = gsc
GSI_EXECUTABLE_INSTALLED = gsi
GSC_EXECUTABLE_INSTALLED = gsc

RCFILES = makefile.in \
gambc-cc.unix.in gambc-cc.bat.windows.in \
gambc-doc.unix.in gambc-doc.bat.windows.in

GENDISTFILES =

DISTFILES = $(RCFILES) $(GENDISTFILES)

INSTFILES_BIN_PROG = gambc-cc gambc-doc
INSTFILES_BIN_LINK_GSI = six gsi-script six-script \
scheme-srfi-0 scheme-r5rs scheme-r4rs scheme-ieee-1178-1990
INSTFILES_BIN_LINK_GSC = gsc-script
INSTFILES_BIN_LINK = $(INSTFILES_BIN_LINK_GSI) $(INSTFILES_BIN_LINK_GSC)
INSTFILES_BIN = $(INSTFILES_BIN_PROG) $(INSTFILES_BIN_LINK)

all:

all-pre:

all-post: gambc-cc gambc-doc six gsi-script gsc-script six-script scheme-srfi-0 scheme-r5rs scheme-r4rs scheme-ieee-1178-1990

gambc-cc: makefile gambc-cc.unix gambc-cc.bat.windows
	rm -f gambc-cc
	if test "" = ""; then \
	  cp gambc-cc.unix gambc-cc; \
	  chmod +x gambc-cc; \
	else \
	  cp gambc-cc.bat.windows gambc-cc; \
	fi

gambc-doc: makefile gambc-doc.unix gambc-doc.bat.windows
	rm -f gambc-doc
	if test "" = ""; then \
	  cp gambc-doc.unix gambc-doc; \
	  chmod +x gambc-doc; \
	else \
	  cp gambc-doc.bat.windows gambc-doc; \
	fi

six: makefile
	rm -f six six.lnk
	if test "" = ""; then \
	  $(LN_S) $(rootfromhere)/gsi/$(GSI_EXECUTABLE) six; \
	else \
	  cp $(rootfromhere)/gsi/$(GSI_EXECUTABLE) six; \
	fi

gsi-script: makefile
	rm -f gsi-script gsi-script.lnk
	if test "" = ""; then \
	  $(LN_S) $(rootfromhere)/gsi/$(GSI_EXECUTABLE) gsi-script; \
	else \
	  echo "@gsi %*" > gsi-script; \
	fi

gsc-script: makefile
	rm -f gsc-script gsc-script.lnk
	if test "" = ""; then \
	  $(LN_S) $(rootfromhere)/gsc/$(GSC_EXECUTABLE) gsc-script; \
	else \
	  echo "@gsc %*" > gsc-script; \
	fi

six-script: makefile
	rm -f six-script six-script.lnk
	if test "" = ""; then \
	  $(LN_S) $(rootfromhere)/gsi/$(GSI_EXECUTABLE) six-script; \
	else \
	  echo "@gsi %*" > six-script; \
	fi

scheme-srfi-0: makefile
	rm -f scheme-srfi-0 scheme-srfi-0.lnk
	if test "" = ""; then \
	  $(LN_S) $(rootfromhere)/gsi/$(GSI_EXECUTABLE) scheme-srfi-0; \
	else \
	  echo "@gsi %*" > scheme-srfi-0; \
	fi

scheme-r5rs: makefile
	rm -f scheme-r5rs scheme-r5rs.lnk
	if test "" = ""; then \
	  $(LN_S) $(rootfromhere)/gsi/$(GSI_EXECUTABLE) scheme-r5rs; \
	else \
	  echo "@gsi %*" > scheme-r5rs; \
	fi

scheme-r4rs: makefile
	rm -f scheme-r4rs scheme-r4rs.lnk
	if test "" = ""; then \
	  $(LN_S) $(rootfromhere)/gsi/$(GSI_EXECUTABLE) scheme-r4rs; \
	else \
	  echo "@gsi %*" > scheme-r4rs; \
	fi

scheme-ieee-1178-1990: makefile
	rm -f scheme-ieee-1178-1990 scheme-ieee-1178-1990.lnk
	if test "" = ""; then \
	  $(LN_S) $(rootfromhere)/gsi/$(GSI_EXECUTABLE) scheme-ieee-1178-1990; \
	else \
	  echo "@gsi %*" > scheme-ieee-1178-1990; \
	fi

install-pre:

install-post: all
	c_bindir=$(DESTDIR)$(prefix)/bin; \
	i_bindir=$(DESTDIR)$(bindir); \
	t_bindir=$$i_bindir; \
	$(srcdirpfx)$(rootfromhere)/mkidirs $$i_bindir; \
	if test "no" = "yes"; then \
	  $(srcdirpfx)$(rootfromhere)/mkidirs $$c_bindir; \
	  t_bindir=$$c_bindir; \
	fi; \
	for file in $(INSTFILES_BIN_LINK); do \
	  rm -f $$t_bindir/$$file $$t_bindir/$$file.lnk; \
	done; \
	for file in $(INSTFILES_BIN_PROG); do \
	  $(INSTALL_PROGRAM) $$file $$t_bindir/$$file; \
	done; \
	if test "" = ""; then \
	  for file in $(INSTFILES_BIN_LINK_GSI); do \
	    (cd $$t_bindir && $(LN_S) $(GSI_EXECUTABLE_INSTALLED) $$file); \
	  done; \
	  for file in $(INSTFILES_BIN_LINK_GSC); do \
	    (cd $$t_bindir && $(LN_S) $(GSC_EXECUTABLE_INSTALLED) $$file); \
	  done; \
	else \
	  for file in $(INSTFILES_BIN_LINK); do \
	    $(INSTALL_PROGRAM) $$file $$t_bindir/$$file; \
	  done; \
	fi; \
	if test "$$t_bindir" != "$$i_bindir"; then \
	  r_bindir=`$(rootfromhere)/relpath "$$t_bindir" "$$i_bindir" no`; \
	  for file in $(INSTFILES_BIN_LINK); do \
	    rm -f $$i_bindir/$$file $$i_bindir/$$file.lnk; \
	  done; \
	  for file in $(INSTFILES_BIN); do \
	    (cd $$i_bindir && $(LN_S) $$r_bindir$$file $$file); \
	  done; \
	fi

uninstall-pre:

uninstall-post:
	c_bindir=$(DESTDIR)$(prefix)/bin; \
	i_bindir=$(DESTDIR)$(bindir); \
	t_bindir=$$i_bindir; \
	$(srcdirpfx)$(rootfromhere)/mkidirs $$i_bindir; \
	if test "no" = "yes"; then \
	  $(srcdirpfx)$(rootfromhere)/mkidirs $$c_bindir; \
	  t_bindir=$$c_bindir; \
	fi; \
	for file in $(INSTFILES_BIN_LINK); do \
	  rm -f $$t_bindir/$$file $$t_bindir/$$file.lnk; \
	done; \
	for file in $(INSTFILES_BIN_PROG); do \
	  rm -f $$t_bindir/$$file; \
	done; \
	if test "$$t_bindir" != "$$i_bindir"; then \
	  for file in $(INSTFILES_BIN_LINK); do \
	    rm -f $$i_bindir/$$file $$i_bindir/$$file.lnk; \
	  done; \
	  for file in $(INSTFILES_BIN_PROG); do \
	    rm -f $$i_bindir/$$file; \
	  done; \
	fi; \
	rmdir $$t_bindir 2> /dev/null; \
	if test "$$t_bindir" != "$$i_bindir"; then \
	  rmdir $$i_bindir 2> /dev/null; \
	fi

mostlyclean-pre:

mostlyclean-post:

clean-pre: mostlyclean-pre

clean-post: mostlyclean-post
	rm -f gambc-cc \
	  gambc-doc \
	  six six.lnk \
	  gsi-script \
	  gsi-script.lnk \
	  gsc-script \
	  gsc-script.lnk \
	  six-script \
	  six-script.lnk \
	  scheme-srfi-0 \
	  scheme-srfi-0.lnk \
	  scheme-r5rs \
	  scheme-r5rs.lnk \
	  scheme-r4rs \
	  scheme-r4rs.lnk \
	  scheme-ieee-1178-1990 \
	  scheme-ieee-1178-1990.lnk

distclean-pre: clean-pre

distclean-post: clean-post

bootclean-pre: distclean-pre

bootclean-post: distclean-post

realclean-pre: bootclean-pre

realclean-post: bootclean-post
	rm -f gambc-cc.unix gambc-cc.bat.windows \
	  gambc-doc.unix gambc-doc.bat.windows makefile

rc-setup-pre:
	$(RC) add $(RCFILES)

rc-setup-post:

dist-pre dist-devel-pre:
	mkdir $(rootfromhere)/$(PACKAGE_TARNAME)/$(herefromroot)
	chmod 777 $(rootfromhere)/$(PACKAGE_TARNAME)/$(herefromroot)
	@echo "  Copying distribution files:"
	@for file in $(DISTFILES); do \
	  echo "    $(herefromroot)/$$file"; \
	  ln $(srcdirpfx)$$file $(rootfromhere)/$(PACKAGE_TARNAME)/$(herefromroot) 2> /dev/null \
	    || cp -p $(srcdirpfx)$$file $(rootfromhere)/$(PACKAGE_TARNAME)/$(herefromroot); \
	done

dist-post dist-devel-post:

all-recursive install-recursive uninstall-recursive mostlyclean-recursive clean-recursive distclean-recursive bootclean-recursive realclean-recursive rc-setup-recursive dist-recursive dist-devel-recursive:
	@if test -n "$(SUBDIRS)"; then \
	  for subdir in ""$(SUBDIRS); do \
	    target=`echo $@ | sed 's/-recursive//'`; \
	    echo making $$target in $$subdir; \
	    (cd $$subdir && $(MAKE) $$target) || exit 1; \
	  done \
	fi

all: all-post

all-post: all-recursive

all-recursive: all-pre

install: install-post

install-post: install-recursive

install-recursive: install-pre

uninstall: uninstall-post

uninstall-post: uninstall-recursive

uninstall-recursive: uninstall-pre

mostlyclean: mostlyclean-post

mostlyclean-post: mostlyclean-recursive

mostlyclean-recursive: mostlyclean-pre

clean: clean-post

clean-post: clean-recursive

clean-recursive: clean-pre

distclean: distclean-post

distclean-post: distclean-recursive

distclean-recursive: distclean-pre

bootclean: bootclean-post

bootclean-post: bootclean-recursive

bootclean-recursive: bootclean-pre

realclean: realclean-post

realclean-post: realclean-recursive

realclean-recursive: realclean-pre

rc-setup: rc-setup-post

rc-setup-post: rc-setup-recursive

rc-setup-recursive: rc-setup-pre

dist: dist-post

dist-post: dist-recursive

dist-recursive: dist-pre

dist-devel: dist-devel-post

dist-devel-post: dist-devel-recursive

dist-devel-recursive: dist-devel-pre

makefile: makefile.in $(rootfromhere)/config.status
	cd $(rootfromhere) && CONFIG_FILES=$(herefromroot)/$@ CONFIG_HEADERS= ./config.status

gambc-cc.unix: gambc-cc.unix.in $(rootfromhere)/config.status
	cd $(rootfromhere) && CONFIG_FILES=$(herefromroot)/$@ CONFIG_HEADERS= ./config.status

gambc-cc.bat.windows: gambc-cc.bat.windows.in $(rootfromhere)/config.status
	cd $(rootfromhere) && CONFIG_FILES=$(herefromroot)/$@ CONFIG_HEADERS= ./config.status

gambc-doc.unix: gambc-doc.unix.in $(rootfromhere)/config.status
	cd $(rootfromhere) && CONFIG_FILES=$(herefromroot)/$@ CONFIG_HEADERS= ./config.status

gambc-doc.bat.windows: gambc-doc.bat.windows.in $(rootfromhere)/config.status
	cd $(rootfromhere) && CONFIG_FILES=$(herefromroot)/$@ CONFIG_HEADERS= ./config.status

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
