# makefile for iOS example

# Copyright (c) 1994-2012 by Marc Feeley, All Rights Reserved.

herefromroot = examples/iOS
rootfromhere = ../..
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

.SUFFIXES: .scm .m .o

RCFILES = makefile.in README build-gambit-iOS GambitREPL.xcodeproj.tgz \
digest.scm "digest\#.scm" genport.scm "genport\#.scm" \
zlib.scm "zlib\#.scm" tar.scm "tar\#.scm" \
json.scm "json\#.scm" url.scm "url\#.scm" html.scm "html\#.scm" \
wiki.scm "wiki\#.scm" repl-server.scm "repl-server\#.scm" \
intf.scm "intf\#.scm" intf.h script.scm "script\#.scm" \
repo.scm "repo\#.scm" help.scm "help\#.scm" program.scm \
Prefix.pch main.m \
AppDelegate.m AppDelegate.h ViewController.m ViewController.h \
AccessoryView.xib MainWindow.xib ViewController.xib \
Icon-72.png Icon-Small-50.png Icon-Small.png Icon-Small@2x.png Icon-512.png \
Icon.png Icon@2x.png Icon-unscaled.key Icon-unscaled.tiff \
Icon-unscaled-alpha.tiff \
8ball.png edit.png f1.png f10.png f11.png f12.png f2.png f3.png \
f4.png f5.png f6.png f7.png f8.png f9.png heart.png note.png rocket.png \
stop.png user.png \
button-abc.png button-cancel.png button-compass.png button-empty.png button-left-left.png button-left.png button-question.png button-right-right.png button-right.png button-up.png button-up-arrow.png \
key-8ball-46x42.png key-comma-46x42.png key-doublequote-46x42.png \
key-edit-46x42.png key-empty-46x42.png key-f1-46x42.png \
key-f10-46x42.png key-f11-46x42.png key-f12-46x42.png key-f2-46x42.png \
key-f3-46x42.png key-f4-46x42.png key-f5-46x42.png key-f6-46x42.png \
key-f7-46x42.png key-f8-46x42.png key-f9-46x42.png key-heart-46x42.png \
key-lambda-46x42.png key-lparen-46x42.png key-minus-46x42.png \
key-note-46x42.png key-plus-46x42.png key-quote-46x42.png \
key-rocket-46x42.png key-rparen-46x42.png key-sharp-46x42.png \
key-star-46x42.png key-stop-46x42.png key-user-46x42.png \
make-appstore-screenshots make-keys make-scaled-icons \
Info.plist \
help.html r5rs.html r5rs.pdf gambit-c.html

GENDISTFILES =

DISTFILES = $(RCFILES) $(GENDISTFILES)

all:

all-pre:

all-post:

examples: prepare-for-xcode

gambit-iOS:
	@echo "*************************************************************"
	@echo "*                                                           *"
	@echo "* Building Gambit for iOS                                   *"
	@echo "*                                                           *"
	@echo "*   This builds the Gambit runtime library for              *"
	@echo "*   the iPhone/iPad/iPod Touch devices and simulators       *"
	@echo "*                                                           *"
	@echo "*************************************************************"
	chmod +x build-gambit-iOS
	./build-gambit-iOS
	rm -rf lib
	mkdir lib
	cp gambit-iOS/current/lib/*.scm lib
	cp gambit-iOS/current/doc/gambit-c.pdf .

prepare-for-xcode: digest.m genport.m zlib.m tar.m json.m url.m html.m wiki.m repl-server.m intf.m script.m repo.m help.m program.m program_.m GambitREPL.xcodeproj gambit-iOS
	@echo "*************************************************************"
	@echo "*                                                           *"
	@echo "* To complete the iOS example the following steps must be   *"
	@echo "* completed manually:                                       *"
	@echo "*                                                           *"
	@echo "*  1) open GambitREPL.xcodeproj in XCode 4.0 (or later)     *"
	@echo "*                                                           *"
	@echo "*  2) select the appropriate target (simulator or device)   *"
	@echo "*                                                           *"
	@echo "*  3) select menu Product -> Run                            *"
	@echo "*                                                           *"
	@echo "*************************************************************"

.scm.m:
	 $(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -c -check -o $*.m $(srcdirpfx)$*.scm

program_.m: digest.m genport.m zlib.m tar.m json.m url.m html.m wiki.m repl-server.m intf.m script.m repo.m help.m program.m
	 $(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -link -o $@ $^

GambitREPL.xcodeproj: GambitREPL.xcodeproj.tgz
	rm -rf GambitREPL.xcodeproj
	tar zxf GambitREPL.xcodeproj.tgz

xcodeproj:
	tar cf GambitREPL.xcodeproj.tar GambitREPL.xcodeproj/project.pbxproj
	gzip -9 GambitREPL.xcodeproj.tar
	mv GambitREPL.xcodeproj.tar.gz GambitREPL.xcodeproj.tgz

install-pre:

install-post: all

uninstall-pre:

uninstall-post:

mostlyclean-pre:

mostlyclean-post:
	rm -f digest.m genport.m zlib.m tar.m json.m url.m html.m wiki.m repl-server.m intf.m script.m repo.m help.m program.m program_.m

clean-pre: mostlyclean-pre

clean-post: mostlyclean-post
	rm -f *.gcov *.gcno *.gcda

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
