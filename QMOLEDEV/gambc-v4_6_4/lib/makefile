# makefile for Gambit-C library, Time-stamp: <2009-07-30 11:52:23 feeley>

# Copyright (c) 1994-2009 by Marc Feeley, All Rights Reserved.

herefromroot = lib
rootfromhere = ..
SUBDIRS = guide

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
FLAGS_OBJ =    -no-cpp-precomp -Wno-unused -Wno-write-strings  -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common 
FLAGS_DYN =    -bundle  -no-cpp-precomp -Wno-unused -Wno-write-strings -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common   -flat_namespace -undefined suppress
FLAGS_LIB =    -dynamiclib -install_name $(libdir)/$(LIBRARY)   -flat_namespace -undefined suppress
FLAGS_EXE =     -no-cpp-precomp -Wno-unused -Wno-write-strings  -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common  
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
.SUFFIXES: .scm .c .o

NORMAL_C = main.c setup.c mem.c os.c os_base.c os_time.c os_shell.c os_files.c os_dyn.c os_tty.c os_io.c c_intf.c
NORMAL_CPP =
NORMAL_H = setup.h mem.h os.h os_base.h os_time.h os_shell.h os_files.h os_dyn.h os_tty.h os_io.h c_intf.h dead.h
NORMAL_O = main.o setup.o mem.o c_intf.o os.o os_base.o os_time.o os_shell.o os_files.o os_dyn.o os_tty.o os_io.o
NORMAL_O_PLUS = +main.o +setup.o +mem.o +c_intf.o +os.o +os_base.o +os_time.o +os_shell.o +os_files.o +os_dyn.o +os_tty.o +os_io.o
NORMAL_O_COMMA = main.o,setup.o,mem.o,c_intf.o,os.o,os_base.o,os_time.o,os_shell.o,os_files.o,os_dyn.o,os_tty.o,os_io.o
NORMAL_SOURCES = $(NORMAL_C) $(NORMAL_CPP) $(NORMAL_H)

HEADERS_SCM = "_kernel\#.scm" "_system\#.scm" "_num\#.scm" "_std\#.scm" \
"_eval\#.scm" "_io\#.scm" "_nonstd\#.scm" "_thread\#.scm" "_repl\#.scm"
MODULES = _kernel _system _num _std \
_eval _io _nonstd _thread _repl
MODULES_SCM = _kernel.scm _system.scm _num.scm _std.scm \
_eval.scm _io.scm _nonstd.scm _thread.scm _repl.scm
MODULES_C = _kernel.c _system.c _num.c _std.c \
_eval.c _io.c _nonstd.c _thread.c _repl.c
MODULES_O = _kernel.o _system.o _num.o _std.o \
_eval.o _io.o _nonstd.o _thread.o _repl.o
MODULES_O_PLUS = +_kernel.o +_system.o +_num.o +_std.o \
+_eval.o +_io.o +_nonstd.o +_thread.o +_repl.o
MODULES_O_COMMA = _kernel.o,_system.o,_num.o,_std.o,\
+_eval.o,_io.o,_nonstd.o,_thread.o,_repl.o
NORMAL_O_IN_COMPILE_ORDER = main.o os_tty.o c_intf.o \
os_io.o setup.o mem.o os_files.o os.o os_base.o \
os_time.o os_shell.o os_dyn.o 
MODULES_O_IN_COMPILE_ORDER = _io.o _num.o _std.o \
_kernel.o _nonstd.o _repl.o _eval.o _thread.o _system.o 

LIBRARIES_SCM = $(HEADERS_SCM) "_gambit\#.scm" "gambit\#.scm" \
"r5rs\#.scm" "r4rs\#.scm" "digest\#.scm" digest.scm syntax-case.scm

LIBRARY = $(LIB_PREFIX)$(GAMBCLIB)$(LIB_EXTENSION)
LIBRARY_OBJECTS = $(NORMAL_O) $(MODULES_O) _$(GAMBCLIB).o
LIBRARY_OBJECTS_PLUS = $(NORMAL_O_PLUS) $(MODULES_O_PLUS) +_$(GAMBCLIB).o
LIBRARY_OBJECTS_COMMA = $(NORMAL_O_COMMA),$(MODULES_O_COMMA),_$(GAMBCLIB).o
LIBRARY_OBJECTS_IN_COMPILE_ORDER = $(MODULES_O_IN_COMPILE_ORDER) \
$(NORMAL_O_IN_COMPILE_ORDER) _$(GAMBCLIB).o

LIBRARIES = $(LIBRARY)

LIBRARY_LIBS =

OBJECTS = $(LIBRARY_OBJECTS)

RCFILES = makefile.in header.scm $(MODULES_SCM) sys.scm \
$(LIBRARIES_SCM) $(NORMAL_SOURCES) check___P check___P.st \
psyntax69c.pp psyntax69c.ss psyntax73.pp psyntax73.ss \
syntax-case-build syntax-case-expand-file \
syntax-case-postlude.scm syntax-case-prelude.scm

GENDISTFILES = $(MODULES_C) _$(GAMBCLIB).c

DISTFILES = $(RCFILES) $(GENDISTFILES)

INSTFILES_LIB_DATA = _$(GAMBCLIB).c $(LIBRARIES_SCM)

GAMBCDIR_INCLUDE = $(rootfromhere)/include
GAMBCDIR_LIB = $(rootfromhere)/lib
GAMBCDIR_LIB_GSI = $(rootfromhere)/gsi
GAMBCDIR_LIB_GSC = $(rootfromhere)/gsc

BUILD_OBJ_OUTPUT_FILENAME = $*.o
BUILD_OBJ_CC_OPTIONS =  -I$(srcdir) $(DEFS) $(GAMBCLIB_DEFS) -D___PRIMAL
BUILD_OBJ_INPUT_FILENAMES = $(srcdirpfx)$*.c

all:

all-pre:

all-post: $(LIBRARY) _$(GAMBCLIB).c

.c.o:
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES} -D___LIBRARY

.scm.c:
	 $(rootfromhere)/gsc-comp -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -c -check $(srcdirpfx)$*.scm

$(LIBRARY): $(LIBRARY_OBJECTS_IN_COMPILE_ORDER)
	rm -f $(LIBRARY)
	ar rc $(LIBRARY) $(LIBRARY_OBJECTS) && $(RANLIB) $(LIBRARY)

_$(GAMBCLIB).c: $(MODULES_C)
	 $(rootfromhere)/gsc-comp -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -link -flat -o _$(GAMBCLIB).c $(MODULES_C)

main.o: main.c $(srcdirpfx)$(rootfromhere)/include/gambit.h

setup.o: setup.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h setup.h mem.h c_intf.h

mem.o: mem.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h setup.h mem.h c_intf.h

c_intf.o: c_intf.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h setup.h mem.h c_intf.h

os.o: os.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h os_base.h os_time.h os_shell.h os_files.h os_dyn.h os_tty.h os_io.h setup.h mem.h c_intf.h

os_base.o: os_base.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h os_base.h

os_time.o: os_time.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h os_time.h

os_shell.o: os_shell.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h os_shell.h os_files.h

os_files.o: os_files.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h os_files.h

os_dyn.o: os_dyn.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h os_dyn.h

os_tty.o: os_tty.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h os_tty.h

os_io.o: os_io.c $(srcdirpfx)$(rootfromhere)/include/gambit.h os.h os_io.h

$(MODULES_C): $(srcdirpfx)$(rootfromhere)/lib/header.scm \
$(srcdirpfx)$(rootfromhere)/lib/gambit\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_gambit\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/gambit\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_gambit\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_kernel\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_system\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_num\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_std\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_eval\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_io\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_nonstd\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_thread\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_repl\#.scm

$(MODULES_O): $(srcdirpfx)$(rootfromhere)/include/gambit.h

_kernel.o: $(srcdirpfx)$(rootfromhere)/include/stamp.h

install-pre:

install-post-comp-sc: all
	@echo "*** Compiling ~~lib/syntax-case.scm (this may take a while)"
	rm -f syntax-case.o*
	 $(srcdirpfx)$(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include syntax-case

install-post: all
	c_libdir=$(DESTDIR)$(prefix)/lib; \
	i_libdir=$(DESTDIR)$(libdir); \
	t_libdir=$$i_libdir; \
	$(srcdirpfx)$(rootfromhere)/mkidirs $$i_libdir; \
	if test "no" = "yes"; then \
	  $(srcdirpfx)$(rootfromhere)/mkidirs $$c_libdir; \
	  t_libdir=$$c_libdir; \
	fi; \
	for file in $(INSTFILES_LIB_DATA); do \
	  $(INSTALL_DATA) $$file $$t_libdir/$$file; \
	done; \
	for file in $(LIBRARIES); do \
	  $(INSTALL_LIB) $$file $$t_libdir/$$file; \
	done; \
	if test "$$t_libdir" != "$$i_libdir"; then \
	  r_libdir=`$(rootfromhere)/relpath "$$t_libdir" "$$i_libdir" no`; \
	  for file in $(INSTFILES_LIB_DATA) $(LIBRARIES); do \
	    (cd $$i_libdir && $(LN_S) $$r_libdir$$file $$file); \
	  done; \
	fi

uninstall-pre:

uninstall-post:
	c_libdir=$(DESTDIR)$(prefix)/lib; \
	i_libdir=$(DESTDIR)$(libdir); \
	t_libdir=$$i_libdir; \
	if test "no" = "yes"; then \
	  t_libdir=$$c_libdir; \
	fi; \
	for file in $(INSTFILES_LIB_DATA) $(LIBRARIES); do \
	  rm -f $$t_libdir/$$file; \
	done; \
	if test "$$t_libdir" != "$$i_libdir"; then \
	  for file in $(INSTFILES_LIB_DATA) $(LIBRARIES); do \
	    rm -f $$i_libdir/$$file; \
	  done; \
	fi; \
	rmdir $$t_libdir 2> /dev/null; \
	if test "$$t_libdir" != "$$i_libdir"; then \
	  rmdir $$i_libdir 2> /dev/null; \
	fi

mostlyclean-pre:

mostlyclean-post:
	rm -f $(OBJECTS) $(LIBRARY)

clean-pre: mostlyclean-pre

clean-post: mostlyclean-post
	rm -f *.da *.bb *.bbg *.gcov *.gcno *.gcda

distclean-pre: clean-pre

distclean-post: clean-post

bootclean-pre: distclean-pre

bootclean-post: distclean-post
	rm -f $(MODULES_C) _$(GAMBCLIB).c

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

all-recursive:
	@if test -n ""; then \
	  for subdir in ""; do \
	    (cd $$subdir && $(MAKE) all) || exit 1; \
	  done \
	fi

install-recursive uninstall-recursive mostlyclean-recursive clean-recursive distclean-recursive bootclean-recursive realclean-recursive rc-setup-recursive dist-recursive dist-devel-recursive:
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
