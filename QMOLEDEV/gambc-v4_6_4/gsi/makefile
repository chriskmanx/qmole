# makefile for Gambit-C interpreter.

# Copyright (c) 1994-2009 by Marc Feeley, All Rights Reserved.

herefromroot = gsi
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
.SUFFIXES: .scm .c .o

LIBRARIES_SCM =

LIBRARY = $(LIB_PREFIX)$(GAMBCGSILIB)$(LIB_EXTENSION)
LIBRARY_NORMAL_C =
LIBRARY_NORMAL_CPP =
LIBRARY_NORMAL_H =
LIBRARY_NORMAL_O =
LIBRARY_NORMAL_O_PLUS =
LIBRARY_NORMAL_O_COMMA =
LIBRARY_NORMAL_SOURCES = $(LIBRARY_NORMAL_C) $(LIBRARY_NORMAL_CPP) \
$(LIBRARY_NORMAL_H)
LIBRARY_MODULES = _gsilib
LIBRARY_MODULES_SCM = _gsilib.scm
LIBRARY_MODULES_C = _gsilib.c
LIBRARY_MODULES_O = _gsilib.o
LIBRARY_MODULES_O_PLUS = +_gsilib.o
LIBRARY_MODULES_O_COMMA = _gsilib.o
LIBRARY_MODULES_O_IN_COMPILE_ORDER = _gsilib.o
LIBRARY_OBJECTS = $(LIBRARY_NORMAL_O) \
$(LIBRARY_MODULES_O) _$(GAMBCGSILIB).o
LIBRARY_OBJECTS_PLUS = $(LIBRARY_NORMAL_O_PLUS) \
$(LIBRARY_MODULES_O_PLUS) +_$(GAMBCGSILIB).o
LIBRARY_OBJECTS_COMMA = $(LIBRARY_NORMAL_O_COMMA),\
$(LIBRARY_MODULES_O_COMMA),_$(GAMBCGSILIB).o
LIBRARY_OBJECTS_IN_COMPILE_ORDER = $(LIBRARY_MODULES_O_IN_COMPILE_ORDER) \
$(LIBRARY_NORMAL_O_IN_COMPILE_ORDER) _$(GAMBCGSILIB).o

MAKE_LIBRARY_LIBS = $(rootfromhere)/lib/$(LIB_PREFIX)$(GAMBCLIB)$(LIB_EXTENSION)
MAKE_LIBRARY_FOR_INSTALL_LIBS = -L$(rootfromhere)/lib -l$(GAMBCLIB)

EXECUTABLE = gsi
EXECUTABLE_INSTALLED = gsi
EXECUTABLE_NORMAL_C =
EXECUTABLE_NORMAL_CPP =
EXECUTABLE_NORMAL_H =
EXECUTABLE_NORMAL_O =
EXECUTABLE_NORMAL_O_PLUS =
EXECUTABLE_NORMAL_O_COMMA =
EXECUTABLE_NORMAL_SOURCES = $(EXECUTABLE_NORMAL_C) $(EXECUTABLE_NORMAL_CPP) \
$(EXECUTABLE_NORMAL_H)
EXECUTABLE_MODULES = _gsi
EXECUTABLE_MODULES_SCM = _gsi.scm
EXECUTABLE_MODULES_C = _gsi.c
EXECUTABLE_MODULES_O = _gsi.o
EXECUTABLE_MODULES_O_PLUS = +_gsi.o
EXECUTABLE_MODULES_O_COMMA = _gsi.o
EXECUTABLE_MODULES_O_IN_COMPILE_ORDER = _gsi.o
EXECUTABLE_OBJECTS = $(EXECUTABLE_NORMAL_O) \
$(EXECUTABLE_MODULES_O) _gsi_.o
EXECUTABLE_OBJECTS_PLUS = $(EXECUTABLE_NORMAL_O_PLUS) \
$(EXECUTABLE_MODULES_O_PLUS) +_gsi_.o
EXECUTABLE_OBJECTS_COMMA = $(EXECUTABLE_NORMAL_O_COMMA),\
$(EXECUTABLE_MODULES_O_COMMA),_gsi_.o
EXECUTABLE_OBJECTS_IN_COMPILE_ORDER = $(EXECUTABLE_MODULES_O_IN_COMPILE_ORDER) \
$(EXECUTABLE_NORMAL_O_IN_COMPILE_ORDER) _gsi_.o

LINK_LIBS = $(LIBRARY) $(rootfromhere)/lib/$(LIB_PREFIX)$(GAMBCLIB)$(LIB_EXTENSION)
LINK_FOR_INSTALL_LIBS = -L. -L$(rootfromhere)/lib -l$(GAMBCGSILIB) -l$(GAMBCLIB)

MODULES_SCM = $(LIBRARY_MODULES_SCM) $(EXECUTABLE_MODULES_SCM)
MODULES_C = $(LIBRARY_MODULES_C) $(EXECUTABLE_MODULES_C)
OBJECTS = $(LIBRARY_OBJECTS) $(EXECUTABLE_OBJECTS)

RCFILES = makefile.in main.scm $(MODULES_SCM)

GENDISTFILES = $(MODULES_C) _$(GAMBCGSILIB).c _gsi_.c

DISTFILES = $(RCFILES) $(GENDISTFILES)

GAMBCDIR_INCLUDE = $(srcdirpfx)$(rootfromhere)/include
GAMBCDIR_LIB = $(srcdirpfx)$(rootfromhere)/lib
GAMBCDIR_LIB_GSI = $(srcdirpfx)$(rootfromhere)/gsi
GAMBCDIR_LIB_GSC = $(srcdirpfx)$(rootfromhere)/gsc

BUILD_OBJ_OUTPUT_FILENAME = $*.o
BUILD_OBJ_CC_OPTIONS =  -I$(srcdir) $(DEFS)
BUILD_OBJ_INPUT_FILENAMES = $(srcdirpfx)$*.c

BUILD_EXE_OUTPUT_FILENAME = $(EXECUTABLE)
BUILD_EXE_CC_OPTIONS =
BUILD_EXE_LD_OPTIONS_PRELUDE =
BUILD_EXE_INPUT_FILENAMES = $(EXECUTABLE_OBJECTS)
BUILD_EXE_LD_OPTIONS = $(LIBRARY)

all:

all-pre:

all-post: $(LIBRARY) $(EXECUTABLE)

$(LIBRARY): $(LIBRARY_OBJECTS_IN_COMPILE_ORDER)
	rm -f $(LIBRARY)
	ar rc $(LIBRARY) $(LIBRARY_OBJECTS) && $(RANLIB) $(LIBRARY)

_$(GAMBCGSILIB).c: $(LIBRARY_MODULES_C) $(rootfromhere)/lib/_gambc.c
	 $(rootfromhere)/gsc-comp -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -link -l $(srcdirpfx)$(rootfromhere)/lib/_gambc -o $@ $(LIBRARY_MODULES_C)

$(EXECUTABLE): $(EXECUTABLE_OBJECTS_IN_COMPILE_ORDER) $(LINK_LIBS)
	${C_COMPILER} ${FLAGS_EXE} ${DEFS_EXE} -I"${GAMBCDIR_INCLUDE}"  -o "${BUILD_EXE_OUTPUT_FILENAME}" ${BUILD_EXE_CC_OPTIONS} ${BUILD_EXE_LD_OPTIONS_PRELUDE} ${BUILD_EXE_INPUT_FILENAMES} "${GAMBCDIR_LIB}/libgambc.a" ${LIBS} ${BUILD_EXE_LD_OPTIONS}

_gsi_.c: $(EXECUTABLE_MODULES_C) _$(GAMBCGSILIB).c
	 $(rootfromhere)/gsc-comp -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -link -l _$(GAMBCGSILIB) -o $@ $(EXECUTABLE_MODULES_C)

_gsi.o: _gsi.c
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES}

_gsi_.o: _gsi_.c
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES}

.c.o:
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES} -D___LIBRARY

.scm.c:
	 $(rootfromhere)/gsc-comp -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -c -check $(srcdirpfx)$*.scm

_gsi.c: $(srcdirpfx)$(rootfromhere)/gsi/main.scm

$(MODULES_C): $(srcdirpfx)$(rootfromhere)/lib/header.scm \
$(srcdirpfx)$(rootfromhere)/lib/gambit\#.scm \
$(srcdirpfx)$(rootfromhere)/lib/_gambit\#.scm

$(MODULES_O): $(srcdirpfx)$(rootfromhere)/include/gambit.h

install-pre:

install-post: all
	c_bindir=$(DESTDIR)$(prefix)/bin; \
	i_bindir=$(DESTDIR)$(bindir); \
	t_bindir=$$i_bindir; \
	c_libdir=$(DESTDIR)$(prefix)/lib; \
	i_libdir=$(DESTDIR)$(libdir); \
	t_libdir=$$i_libdir; \
	$(srcdirpfx)$(rootfromhere)/mkidirs $$i_bindir $$i_libdir; \
	if test "no" = "yes"; then \
	  $(srcdirpfx)$(rootfromhere)/mkidirs $$c_bindir $$c_libdir; \
	  t_bindir=$$c_bindir; \
	  t_libdir=$$c_libdir; \
	fi; \
	$(INSTALL_DATA) $(srcdirpfx)_$(GAMBCGSILIB).c $$t_libdir/_$(GAMBCGSILIB).c; \
	:; \
	$(INSTALL_LIB) $(LIBRARY) $$t_libdir/$(LIBRARY); \
	$(RANLIB) $$t_libdir/$(LIBRARY); \
	:; \
	$(INSTALL_PROGRAM) $(EXECUTABLE) $$t_bindir/$(EXECUTABLE_INSTALLED); \
	if test "$$t_libdir" != "$$i_libdir"; then \
	  r_libdir=`$(rootfromhere)/relpath "$$t_libdir" "$$i_libdir" no`; \
	  for file in _$(GAMBCGSILIB).c $(LIBRARY); do \
	    (cd $$i_libdir && $(LN_S) $$r_libdir$$file $$file); \
	  done; \
	fi; \
	if test "$$t_bindir" != "$$i_bindir"; then \
	  r_bindir=`$(rootfromhere)/relpath "$$t_bindir" "$$i_bindir" no`; \
	  for file in $(EXECUTABLE_INSTALLED); do \
	    (cd $$i_bindir && $(LN_S) $$r_bindir$$file $$file); \
	  done; \
	fi

uninstall-pre:

uninstall-post:
	c_bindir=$(DESTDIR)$(prefix)/bin; \
	i_bindir=$(DESTDIR)$(bindir); \
	t_bindir=$$i_bindir; \
	c_libdir=$(DESTDIR)$(prefix)/lib; \
	i_libdir=$(DESTDIR)$(libdir); \
	t_libdir=$$i_libdir; \
	if test "no" = "yes"; then \
	  t_bindir=$$c_bindir; \
	  t_libdir=$$c_libdir; \
	fi; \
	rm -f $$t_libdir/_$(GAMBCGSILIB).c \
	  $$t_libdir/$(LIBRARY) \
	  $$t_bindir/$(EXECUTABLE_INSTALLED); \
	if test "$$t_libdir" != "$$i_libdir"; then \
	  for file in _$(GAMBCGSILIB).c $(LIBRARY); do \
	    rm -f $$i_libdir/$$file; \
	  done; \
	fi; \
	if test "$$t_bindir" != "$$i_bindir"; then \
	  for file in $(EXECUTABLE_INSTALLED); do \
	    rm -f $$i_bindir/$$file; \
	  done; \
	fi; \
	rmdir $$t_libdir 2> /dev/null; \
	if test "$$t_libdir" != "$$i_libdir"; then \
	  rmdir $$i_libdir 2> /dev/null; \
	fi; \
	rmdir $$t_bindir 2> /dev/null; \
	if test "$$t_bindir" != "$$i_bindir"; then \
	  rmdir $$i_bindir 2> /dev/null; \
	fi

mostlyclean-pre:

mostlyclean-post:
	rm -f $(OBJECTS) $(LIBRARY) $(EXECUTABLE)

clean-pre: mostlyclean-pre

clean-post: mostlyclean-post
	rm -f *.da *.bb *.bbg *.gcov *.gcno *.gcda

distclean-pre: clean-pre

distclean-post: clean-post

bootclean-pre: distclean-pre

bootclean-post: distclean-post
	rm -f $(MODULES_C) _$(GAMBCGSILIB).c _gsi_.c

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
