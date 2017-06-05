# makefile for GUIDE library, Time-stamp: <2011-03-25 08:45:02 feeley>

# Copyright (c) 1994-2009 by Marc Feeley, All Rights Reserved.

herefromroot = lib/guide
rootfromhere = ../..
SUBDIRS = images

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

GAMBCDIR_INCLUDE = $(rootfromhere)/include
GAMBCDIR_LIB = $(rootfromhere)/lib
GAMBCDIR_LIB_GSI = $(rootfromhere)/gsi
GAMBCDIR_LIB_GSC = $(rootfromhere)/gsc

BUILD_OBJ_OUTPUT_FILENAME = $*.o
BUILD_OBJ_CC_OPTIONS =  -I$(srcdir) $(DEFS) $(GAMBCLIB_DEFS) -D___PRIMAL
BUILD_OBJ_INPUT_FILENAMES = $(srcdirpfx)$*.c

.SUFFIXES:
.SUFFIXES: .scm .c .cpp .in .o

.c.o:
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES} -D___LIBRARY

.scm.c:
	 $(rootfromhere)/gsc-comp -:=$(srcdirpfx)$(rootfromhere) -f -c -check $(srcdirpfx)$*.scm

NORMAL_C =
NORMAL_CPP = guideuicodeformat.cpp guideuirepl.cpp guideuihighlighter.cpp guideuihighlighterscheme.cpp guideuischeme.cpp guideuiconsoleinfo.cpp guideuitextedit.cpp guideuicont.cpp guideuienv.cpp guideuiinspector.cpp guide.cpp
NORMAL_H = guideuicodeformat.h guideuirepl.h guideuitableitem.h guideuihighlighter.h guideuihighlighterscheme.h guideuischeme.h guideuiconsoleinfo.h guideuitextedit.h guideuicont.h guideuienv.h guideuiinspector.h _guide.h guide.h
NORMAL_UI = guideuimainwindow.ui guideuifileeditor.ui guideuiformatchooser.ui guideuisearchdialog.ui guideuiabout.ui
NORMAL_O = guideuicodeformat.o guideuirepl.o guideuihighlighter.o guideuihighlighterscheme.o guideuischeme.o guideuiconsoleinfo.o guideuitextedit.o guideuicont.o guideuienv.o guideuiinspector.o guide.o
NORMAL_O_PLUS = +guideuicodeformat.o +guideuirepl.o +guideuihighlighter.o +guideuihighlighterscheme.o +guideuischeme.o +guideuiconsoleinfo.o +guideuitextedit.o +guideuicont.o +guideuienv.o +guideuiinspector.o +guide.o
NORMAL_O_COMMA = guideuicodeformat.o,guideuirepl.o,guideuihighlighter.o,guideuihighlighterscheme.o,guideuischeme.o,guideuiconsoleinfo.o,guideuitextedit.o,guideuicont.o,guideuienv.o,guideuiinspector.o,guide.o
NORMAL_SOURCES = $(NORMAL_C) $(NORMAL_CPP) $(NORMAL_H) $(NORMAL_UI)

HEADERS_SCM = "_guide\#.scm"
MODULES = _guide
MODULES_SCM = _guide.scm
MODULES_C = _guide.c
MODULES_O = _guide.o
MODULES_O_PLUS = +_guide.o
MODULES_O_COMMA = _guide.o

OBJECTS = $(NORMAL_O) $(MODULES_O)
OBJECTS_PLUS = $(NORMAL_O_PLUS) $(MODULES_O_PLUS)
OBJECTS_COMMA = $(NORMAL_O_COMMA),$(MODULES_O_COMMA)

RCFILES = makefile.in guidepro.in $(MODULES_SCM) $(HEADERS_SCM) \
$(NORMAL_SOURCES)

GENDISTFILES = $(MODULES_C)

DISTFILES = $(RCFILES) $(GENDISTFILES)

all:

all-pre:

all-post: guidemak $(MODULES_O)
	$(MAKE) -f guidemak $(MDEFINES)

$(MODULES_C): $(srcdirpfx)$(rootfromhere)/lib/header.scm

$(MODULES_O): $(srcdirpfx)$(rootfromhere)/include/gambit.h

install-pre:

install-post: all

uninstall-pre:

uninstall-post:

mostlyclean-pre:

mostlyclean-post:
	rm -f $(OBJECTS)

clean-pre: mostlyclean-pre

clean-post: mostlyclean-post
	rm -f *.da *.bb *.bbg *.gcov

distclean-pre: clean-pre

distclean-post: clean-post

bootclean-pre: distclean-pre

bootclean-post: distclean-post
	# rm -f $(MODULES_C)

realclean-pre: bootclean-pre

realclean-post: bootclean-post
	rm -f guidemak guidepro makefile

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

#all: all-post

#all-post: all-recursive

#all-recursive: all-pre

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

guidepro: guidepro.in $(rootfromhere)/config.status
	cd $(rootfromhere) && CONFIG_FILES=$(herefromroot)/$@ CONFIG_HEADERS= ./config.status

guidemak: guidepro
	qmake -o guidemak guidepro

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
