# makefile for Gambit-C miscellanea.

# Copyright (c) 1994-2010 by Marc Feeley, All Rights Reserved.

herefromroot = misc
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

RCFILES = makefile.in README bench.tgz simple-make-project.tgz \
openwatcom.bat vcexpress.bat vctoolkit.bat vc2008.bat vstudio.zip \
proj_msc.zip conf_msc conf_msc.sed conf_mpp.sed \
proj_wat.zip conf_wat conf_wat.sed conf_wpp.sed conf_win.sed \
proj_bor.zip conf_bor conf_bor.sed conf_bpp.sed \
conf_dj conf_dj.sed conf_dpp.sed \
conf_lcc conf_lcc.sed \
proj_cw.hqx \
gambit.el \
changev changev1 \
set-regs \
publish-release

GENDISTFILES =

DISTFILES = $(RCFILES) $(GENDISTFILES)

INSTFILES_EMACS = gambit.el

all:

all-pre:

all-post:

install-pre:

install-post: all
	c_emacsdir=$(DESTDIR)$(prefix)/share/emacs/site-lisp; \
	i_emacsdir=$(DESTDIR)$(emacsdir); \
	t_emacsdir=$$i_emacsdir; \
	$(srcdirpfx)$(rootfromhere)/mkidirs $$i_emacsdir; \
	if test "no" = "yes"; then \
	  $(srcdirpfx)$(rootfromhere)/mkidirs $$c_emacsdir; \
	  t_emacsdir=$$c_emacsdir; \
	fi; \
	for file in $(INSTFILES_EMACS); do \
	  $(INSTALL_DATA) $(srcdirpfx)$$file $$t_emacsdir/$$file; \
	done; \
	if test "$$t_emacsdir" != "$$i_emacsdir"; then \
	  r_emacsdir=`$(rootfromhere)/relpath "$$t_emacsdir" "$$i_emacsdir" no`; \
	  for file in $(INSTFILES_EMACS); do \
	    (cd $$i_emacsdir && $(LN_S) $$r_emacsdir$$file $$file); \
	  done; \
	fi

uninstall-pre:

uninstall-post:
	c_emacsdir=$(DESTDIR)$(prefix)/share/emacs/site-lisp; \
	i_emacsdir=$(DESTDIR)$(emacsdir); \
	t_emacsdir=$$i_emacsdir; \
	if test "no" = "yes"; then \
	  t_emacsdir=$$c_emacsdir; \
	fi; \
	for file in $(INSTFILES_EMACS); do \
	  rm -f $$t_emacsdir/$$file; \
	done; \
	if test "$$t_emacsdir" != "$$i_emacsdir"; then \
	  for file in $(INSTFILES_EMACS); do \
	    rm -f $$i_emacsdir/$$file; \
	  done; \
	fi; \
	rmdir $$t_emacsdir 2> /dev/null; \
	if test "$$t_emacsdir" != "$$i_emacsdir"; then \
	  rmdir $$i_emacsdir 2> /dev/null; \
	fi

mostlyclean-pre:

mostlyclean-post:

clean-pre: mostlyclean-pre

clean-post: mostlyclean-post

distclean-pre: clean-pre

distclean-post: clean-post

bootclean-pre: distclean-pre

bootclean-post: distclean-post

realclean-pre: bootclean-pre

realclean-post: bootclean-post
	rm -f makefile

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

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
