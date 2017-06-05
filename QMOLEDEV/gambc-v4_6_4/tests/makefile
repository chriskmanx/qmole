# makefile for regression tests, Time-stamp: <2011-07-29 09:46:00 feeley>

# Copyright (c) 1994-2009 by Marc Feeley, All Rights Reserved.

herefromroot = tests
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

RCFILES = makefile.in \
debug.scm error.scm r4rstest.scm mix.scm mem.scm client.c server.scm server.h \
test1.ok test2.ok test3.ok test4.ok test5.ok test6.ok test7.ok \
test8.ok test9.ok test10.scm input

GENDISTFILES =

DISTFILES = $(RCFILES) $(GENDISTFILES)

GAMBCDIR_INCLUDE = $(srcdirpfx)$(rootfromhere)/include
GAMBCDIR_LIB = $(srcdirpfx)$(rootfromhere)/lib
GAMBCDIR_LIB_GSI = $(srcdirpfx)$(rootfromhere)/gsi
GAMBCDIR_LIB_GSC = $(srcdirpfx)$(rootfromhere)/gsc

all:

all-pre:

all-post:
	@if [ ! -f debug.scm ]; then \
	  cp $(srcdirpfx)debug.scm .; \
	  touch debug.rm; \
	  cp $(srcdirpfx)error.scm .; \
	  touch error.rm; \
	  cp $(srcdirpfx)r4rstest.scm .; \
	  touch r4rstest.rm; \
	  cp $(srcdirpfx)mix.scm .; \
	  touch mix.rm; \
	  cp $(srcdirpfx)mem.scm .; \
	  touch mem.rm; \
	  cp $(srcdirpfx)server.scm .; \
	  touch server.rm; \
	fi

check: all test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
	@echo "============ ALL TESTS SUCCESSFUL"
	rm -f mem.c mem_.c mem.o mem.o1 mem_.o mem \
	  mix.c mix_.c mix.o mix_.o mix \
	  server.c server_.c server.o server_.o client.o \
	  client so_locations *.out tmp*

test1:
	@echo "------------ TEST 1 (debugging support)"
	 $(rootfromhere)/gsi/gsi -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f debug.scm > test1.out
	diff $(srcdirpfx)test1.ok test1.out && rm -f test1.out

test2:
	@echo "------------ TEST 2 (error handling)"
	 $(rootfromhere)/gsi/gsi -:h4000,~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f error.scm < error.scm > test2.out
	diff $(srcdirpfx)test2.ok test2.out && rm -f test2.out

test3:
	@echo "------------ TEST 3 (interpreter and library functions)"
	 $(rootfromhere)/gsi/gsi -:s,~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -e '(begin (load "r4rstest.scm") (test-cont) (test-sc4) (test-delay) (exit))' > test3.out
	diff $(srcdirpfx)test3.ok test3.out && rm -f test3.out tmp*

test4:
	@echo "------------ TEST 4 (interpreter running an application)"
	rm -f mix.o
	 $(rootfromhere)/gsi/gsi -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f mix.scm > test4.out
	diff $(srcdirpfx)test4.ok test4.out && rm -f test4.out

test5:
	@echo "------------ TEST 5 (compiler generating C code)"
	rm -f mix.c
	 $(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -c mix.scm
	diff $(srcdirpfx)test5.ok mix.c

test6: test5
	@echo "------------ TEST 6 (link and execute the code generated)"
	rm -f mix_.c mix.o mix_.o mix
	 $(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -o mix -exe mix.c
	 ./mix -:m4000 > test6.out
	diff $(srcdirpfx)test6.ok test6.out && \
	  rm -f test6.out mix.c mix_.c mix.o mix_.o mix

test7:
	@echo "------------ TEST 7 (memory management and C-interface)"
	rm -f mem.c mem_.c mem.o mem_.o mem
	 $(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -o mem -exe mem.scm
	 ./mem -:h4000 > test7.out
	diff $(srcdirpfx)test7.ok test7.out && \
	  rm -f test7.out mem.c mem_.c mem.o mem_.o mem

test8:
	@echo "------------ TEST 8 (use C-interface to implement a Scheme server)"
	rm -f server.c server_.c server.o server_.o \
	  client.o client
	 $(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -link server.scm
	$(MAKE) build_obj \
	  BUILD_OBJ_OUTPUT_FILENAME="client.o" \
	  BUILD_OBJ_CC_OPTIONS="" \
	  BUILD_OBJ_INPUT_FILENAMES="client.c"
	$(MAKE) build_obj_library \
	  BUILD_OBJ_OUTPUT_FILENAME="server.o" \
	  BUILD_OBJ_CC_OPTIONS="" \
	  BUILD_OBJ_INPUT_FILENAMES="server.c"
	$(MAKE) build_obj_library \
	  BUILD_OBJ_OUTPUT_FILENAME="server_.o" \
	  BUILD_OBJ_CC_OPTIONS="" \
	  BUILD_OBJ_INPUT_FILENAMES="server_.c"
	$(MAKE) build_exe \
	  BUILD_EXE_OUTPUT_FILENAME="client" \
	  BUILD_EXE_CC_OPTIONS="" \
	  BUILD_EXE_LD_OPTIONS_PRELUDE="" \
	  BUILD_EXE_INPUT_FILENAMES="client.o server.o server_.o" \
	  BUILD_EXE_LD_OPTIONS=""
	 ./client > test8.out
	diff -w $(srcdirpfx)test8.ok test8.out && \
	  rm -f test8.out server.c server_.c server.o server_.o \
	    client.o client

test9: test7
	@echo "------------ TEST 9 (dynamic compilation and loading of a module)"
	rm -rf mem.o mem.o*
	 $(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f mem.scm
	 $(rootfromhere)/gsi/gsi -:h4000 -f mem.o1 > test9.out
	diff $(srcdirpfx)test9.ok test9.out && rm -f test9.out mem.o mem.o1

test10:
	@echo "------------ TEST 10 (consistency of lib/gambit#.scm)"
	rm -rf mem.o mem.o*
	 $(rootfromhere)/gsc/gsc -:~~bin=$(srcdirpfx)$(rootfromhere)/bin,~~lib=$(srcdirpfx)$(rootfromhere)/lib,~~include=$(srcdirpfx)$(rootfromhere)/include -f -i test10.scm

build_obj:
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES}

build_obj_library:
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES} -D___LIBRARY

build_exe:
	${C_COMPILER} ${FLAGS_EXE} ${DEFS_EXE} -I"${GAMBCDIR_INCLUDE}"  -o "${BUILD_EXE_OUTPUT_FILENAME}" ${BUILD_EXE_CC_OPTIONS} ${BUILD_EXE_LD_OPTIONS_PRELUDE} ${BUILD_EXE_INPUT_FILENAMES} "${GAMBCDIR_LIB}/libgambc.a" ${LIBS} ${BUILD_EXE_LD_OPTIONS}

install-pre:

install-post: all

uninstall-pre:

uninstall-post:

mostlyclean-pre:

mostlyclean-post:
	rm -f mem.c mem_.c mem.o mem.o1 mem_.o mem mem.exe \
	  mix.c mix_.c mix.o mix_.o mix mix.exe \
	  server.c server_.c server.o server_.o client.o client \
	  client.exe gambit.h so_locations *.out tmp*

clean-pre: mostlyclean-pre

clean-post: mostlyclean-post
	test ! -f debug.rm || rm -f debug.rm debug.scm
	test ! -f error.rm || rm -f error.rm error.scm
	test ! -f r4rstest.rm || rm -f r4rstest.rm r4rstest.scm
	test ! -f mix.rm || rm -f mix.rm mix.scm
	test ! -f mem.rm || rm -f mem.rm mem.scm
	test ! -f server.rm || rm -f server.rm server.scm
	rm -f *.da *.bb *.bbg *.gcov *.gcno *.gcda

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
