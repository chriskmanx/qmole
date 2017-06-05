# makefile for Gambit-C system.

# Copyright (c) 1994-2012 by Marc Feeley, All Rights Reserved.

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

# This directory's subdirectories are mostly independent; you can cd
# into them and run `make' without going through this makefile.
# To change the values of `make' variables: instead of editing makefiles,
# (1) if the variable is set in `config.status', edit `config.status'
#     (which will cause the makefiles to be regenerated when you run `make');
# (2) otherwise, pass the desired values on the `make' command line.

MDEFINES = prefix=$(prefix) exec_prefix=$(exec_prefix) \
includedir=$(includedir) libdir=$(libdir) \
bindir=$(bindir) docdir=$(docdir) \
infodir=$(infodir) emacsdir=$(emacsdir)

SUBDIRS = include lib gsi gsc bin misc doc tests examples prebuilt

RCFILES = README INSTALL.txt LICENSE-2.0.txt LGPL.txt \
makefile.in configure configure.ac config.guess config.sub install-sh mkidirs \
relpath

GITRCFILES = .gitignore .gitattributes
HGRCFILES = .hgignore .hgtags

GENDISTFILES =

DISTFILES = $(RCFILES) $(GENDISTFILES)

GITDISTFILES = $(GITRCFILES)
HGDISTFILES = $(HGRCFILES)

#BUILD_FARM = farm
BUILD_FARM = localhost
#BUILD_HOST = baro.iro.umontreal.ca
BUILD_HOST = localhost

.SUFFIXES:

all:

all-pre:

all-post:
	test -f "gsc-comp" || cp gsc/gsc gsc-comp

fake_target:

doc info pdf html txt: fake_target
	cd doc && $(MAKE) $(MDEFINES) $@

bootstrap: fake_target all
	cp gsc/gsc gsc-comp

check: fake_target all
	cd tests && $(MAKE) $(MDEFINES) $@

examples: fake_target all
	cd examples && $(MAKE) $(MDEFINES) $@

prebuilt:  dist dist-devel
	PATH="gsi:$$PATH" prebuilt/remote-connect $(BUILD_FARM) "" "(define tmpdir \"gambtmp0\")(define gambc-dist \"$(PACKAGE_TARNAME)\")(define timeout 600)(ssh timeout (string-append \"rm -rf \" tmpdir \";mkdir \" tmpdir \";echo 'logged in on:';hostname\"))(scp-to timeout (string-append gambc-dist \".tgz\") (string-append tmpdir \"/.\"))(ssh timeout (string-append \"ssh $(BUILD_HOST) 'cd \" tmpdir \";tar zxf \" gambc-dist \".tgz;cd \" gambc-dist \";./configure;make -j 4;mv ../\" gambc-dist \".tgz .;cd prebuilt;make prebuilt'\"))(scp-from timeout (string-append tmpdir \"/\" gambc-dist \"/$(PACKAGE_TARNAME)-*\") \".\")"
	@# cd prebuilt && $(MAKE) $(MDEFINES) $@

stamp: fake_target
	cd include && $(MAKE) $(MDEFINES) $@

commit: fake_target stamp
	@if test "$(RC)" = "$(GIT)"; then \
	  $(GIT) commit -a; \
	else \
	  $(HG) commit; \
	fi

diff: fake_target
	@$(RC) diff | cat

log: fake_target
	@$(RC) log | cat

status: fake_target
	@$(RC) status || :

push: fake_target
	@if test "$(RC)" = "$(GIT)"; then \
	  $(GIT) push ssh://gambit@frontal07.iro.umontreal.ca/~/HTML/repo/gambit.git/; \
	else \
	  $(HG) push; \
	fi

pull: fake_target
	@if test "$(RC)" = "$(GIT)"; then \
	  $(GIT) pull; \
	  $(GIT) fetch --tags; \
	else \
	  $(HG) pull; \
	fi

update-nopull: fake_target
	@if test "$(RC)" = "$(GIT)"; then \
	  next_version=`$(GIT) tag | fgrep -e '-bootstrap' | sed -e 's/v//g' -e 's/-bootstrap.*//g' | sort -r -s -t. -k 1,1n -k 2,2n -k 3,3n -k 4,4n -k 5,5n | sed 's/^/v/g' | sed -n '/^$(PACKAGE_VERSION)/{n;p;}'`; \
	else \
	  next_version=`$(HG) tags | fgrep -e '-bootstrap' | sed -e 's/v//g' -e 's/-bootstrap.*//g' | sort -r -s -t. -k 1,1n -k 2,2n -k 3,3n -k 4,4n -k 5,5n | sed 's/^/v/g' | sed -n '/^$(PACKAGE_VERSION)/{n;p;}'`; \
	fi; \
	if test "$$next_version" = ""; then \
	  if test "$(RC)" = "$(GIT)"; then \
	    $(GIT) checkout master; \
	  else \
	    $(HG) checkout; \
	  fi; \
	else \
	  $(RC) checkout $$next_version-bootstrap; \
	  $(MAKE) bootstrap; \
	  $(RC) checkout $$next_version; \
	  $(MAKE) bootclean bootstrap update; \
	fi

update: pull update-nopull
	$(MAKE)

new-major: fake_target
	major=`echo $(PACKAGE_VERSION) | sed -e "s/v//g" -e "s/\.[^.]*\.[^.]*//g"`; \
	next_major=`eval expr 1 + $$major`; \
	next_version=v$$next_major.0.0; \
	$(MAKE) NEW_VERSION=$$next_version change-version

new-minor: fake_target
	major=`echo $(PACKAGE_VERSION) | sed -e "s/v//g" -e "s/\.[^.]*\.[^.]*//g"`; \
	minor=`echo $(PACKAGE_VERSION) | sed -e "s/v[^.]*\.//g" -e "s/\.[^.]*//g"`; \
	next_minor=`eval expr 1 + $$minor`; \
	next_version=v$$major.$$next_minor.0; \
	$(MAKE) NEW_VERSION=$$next_version change-version

new-revision: fake_target
	major=`echo $(PACKAGE_VERSION) | sed -e "s/v//g" -e "s/\.[^.]*\.[^.]*//g"`; \
	minor=`echo $(PACKAGE_VERSION) | sed -e "s/v[^.]*\.//g" -e "s/\.[^.]*//g"`; \
	revision=`echo $(PACKAGE_VERSION) | sed -e "s/v[^.]*\.[^.]*\.//g" -e "s///g"`; \
	next_revision=`eval expr 1 + $$revision`; \
	next_version=v$$major.$$minor.$$next_revision; \
	$(MAKE) NEW_VERSION=$$next_version change-version

change-version: fake_target
	major=`echo $(PACKAGE_VERSION) | sed -e "s/v//g" -e "s/\.[^.]*\.[^.]*//g"`; \
	minor=`echo $(PACKAGE_VERSION) | sed -e "s/v[^.]*\.//g" -e "s/\.[^.]*//g"`; \
	revision=`echo $(PACKAGE_VERSION) | sed -e "s/v[^.]*\.[^.]*\.//g" -e "s///g"`; \
	next_major=`echo $(NEW_VERSION) | sed -e "s/v//g" -e "s/\.[^.]*\.[^.]*//g"`; \
	next_minor=`echo $(NEW_VERSION) | sed -e "s/v[^.]*\.//g" -e "s/\.[^.]*//g"`; \
	next_revision=`echo $(NEW_VERSION) | sed -e "s/v[^.]*\.[^.]*\.//g" -e "s///g"`; \
	version_num=`eval expr "\( 100000 \\* $$major \) + \( 1000 \\* $$minor \) + $$revision"`; \
	new_version_num=`eval expr "\( 100000 \\* $$next_major \) + \( 1000 \\* $$next_minor \) + $$next_revision"`; \
	sed -e "/(define (compiler-version) $$version_num)/s/$$version_num/$$new_version_num/" gsc/_parms.scm > gsc/_parms.scm-new; \
	if ! diff gsc/_parms.scm gsc/_parms.scm-new > /dev/null; then \
	  mv gsc/_parms.scm-new gsc/_parms.scm; \
	  if test "$(RC)" = "$(GIT)"; then \
	    $(GIT) commit -a -m "[COMPILER CHANGES NEEDED FOR $(NEW_VERSION)] Changed version in compiler"; \
	  else \
	    $(HG) commit -m "[COMPILER CHANGES NEEDED FOR $(NEW_VERSION)] Changed version in compiler"; \
	  fi; \
	  $(RC) tag $(NEW_VERSION)-bootstrap; \
	  if $(MAKE) bootstrap; then \
	    misc/changev $$version_num $$new_version_num; \
	    touch doc/gambit-c.txi; \
	    cd tests;  ../gsc/gsc -:~~bin=../bin,~~lib=../lib,~~include=../include -f -c mix.scm; mv mix.c test5.ok; cd ..; \
	    if $(MAKE) bootclean && $(MAKE) -j 2 && $(MAKE) check && $(MAKE) doc; then \
	      if test "$(RC)" = "$(GIT)"; then \
	        $(GIT) commit -a -m "[RUNTIME CHANGES NEEDED FOR $(NEW_VERSION)] Changed version of runtime using misc/changev"; \
	      else \
	        $(HG) commit -m "[RUNTIME CHANGES NEEDED FOR $(NEW_VERSION)] Changed version of runtime using misc/changev"; \
	      fi; \
	      $(RC) tag $(NEW_VERSION); \
	    fi; \
	  fi; \
	else \
	  echo Compiler version in gsc/_parms.scm is inconsistent with PACKAGE_VERSION; \
	fi

release: fake_target
	$(MAKE) bootstrap
	$(MAKE) clean
	$(MAKE) all
	$(MAKE) check
	$(MAKE) doc
	rm -f $(PACKAGE_TARNAME).tgz $(PACKAGE_TARNAME)-*.*
	$(MAKE) prebuilt

publish-release: fake_target
	misc/publish-release $(PACKAGE_VERSION) $(PACKAGE_TARNAME).tgz $(PACKAGE_TARNAME)-*.*

install-pre:
	@if test "no" = "yes" -a "no" = "yes" -a "$(DESTDIR)" != ""; then \
	  echo "*** The use of --enable-shared and --enable-absolute-shared-libs is"; \
	  echo "*** incompatible with the use of DESTDIR.  You can either specify an"; \
	  echo "*** empty DESTDIR or reconfigure with --disable-absolute-shared-libs."; \
	  exit 1; \
	fi

install-post: all
	if test "no" = "yes"; then \
	  if test "" = ""; then \
	    $(srcdirpfx)./mkidirs $(DESTDIR)$(prefix); \
	    rm -f $(DESTDIR)$(prefix)/../current \
	      $(DESTDIR)$(prefix)/../current.lnk; \
	    (cd $(DESTDIR)$(prefix)/.. && $(LN_S) $(PACKAGE_VERSION) current); \
	  fi; \
	fi

uninstall-pre:

uninstall-post:
	if test "no" = "yes"; then \
	  if test "" = ""; then \
	    (cd $(DESTDIR)$(prefix)/.. && \
	     rm -f current && \
	     rmdir $(PACKAGE_VERSION)); \
	  fi; \
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
	rm -f config.log config.status makefile

rc-setup-pre:
	$(RC) init
	@if test "$(RC)" = "$(GIT)"; then \
	  $(GIT) add $(RCFILES) $(GITRCFILES); \
	  rm -f .git/config; \
	  echo '[core]' >> .git/config; \
	  echo '	repositoryformatversion = 0' >> .git/config; \
	  echo '	fileMode = false' >> .git/config; \
	  echo '	bare = false' >> .git/config; \
	  echo '	logallrefupdates = true' >> .git/config; \
	  echo '	ignorecase = true' >> .git/config; \
	  echo '	autocrlf = false' >> .git/config; \
	  echo '[remote "origin"]' >> .git/config; \
	  echo '	url = ssh://gambit@frontal07.iro.umontreal.ca/~/HTML/repo/gambit.git' >> .git/config; \
	  echo '	fetch = +refs/heads/*:refs/remotes/origin/*' >> .git/config; \
	  echo '[branch "master"]' >> .git/config; \
	  echo '	remote = origin' >> .git/config; \
	  echo '	merge = refs/heads/master' >> .git/config; \
	else \
	  $(HG) add $(RCFILES) $(HGRCFILES); \
	  rm -f .hg/hgrc; \
	  echo "[paths]" > .hg/hgrc; \
	  echo "default-push = ssh://gambit@frontal07.iro.umontreal.ca/HTML/repo/gambit" >> .hg/hgrc; \
	  echo "default = http://www.iro.umontreal.ca/~gambit/repo/gambit/" >> .hg/hgrc; \
	fi

rc-setup-post:
	@if test "$(RC)" = "$(GIT)"; then \
	  $(GIT) commit -a -m "Initial commit of $(PACKAGE_STRING)"; \
	else \
	  $(HG) commit -m "Initial commit of $(PACKAGE_STRING)"; \
	fi

dist-pre:
	rm -rf $(PACKAGE_TARNAME)
	mkdir $(PACKAGE_TARNAME)
	chmod 777 $(PACKAGE_TARNAME)
	@echo "  Copying distribution files:"
	@for file in $(DISTFILES); do \
	  echo "    $$file"; \
	  ln $(srcdirpfx)$$file $(PACKAGE_TARNAME) 2> /dev/null \
	    || cp -p $(srcdirpfx)$$file $(PACKAGE_TARNAME); \
	done

dist-post:
	tar chof $(PACKAGE_TARNAME).tar $(PACKAGE_TARNAME)
	gzip -9 $(PACKAGE_TARNAME).tar
	mv $(PACKAGE_TARNAME).tar.gz $(PACKAGE_TARNAME).tgz
	rm -rf $(PACKAGE_TARNAME)

dist-devel-pre:
	rm -rf $(PACKAGE_TARNAME)
	mkdir $(PACKAGE_TARNAME)
	chmod 777 $(PACKAGE_TARNAME)
	@echo "  Copying distribution files:"
	@for file in $(DISTFILES); do \
	  echo "    $$file"; \
	  ln $(srcdirpfx)$$file $(PACKAGE_TARNAME) 2> /dev/null \
	    || cp -p $(srcdirpfx)$$file $(PACKAGE_TARNAME); \
	done
	@if test "$(RC)" = "$(GIT)"; then \
	  for file in $(GITDISTFILES); do \
	    echo "    $$file"; \
	    ln $(srcdirpfx)$$file $(PACKAGE_TARNAME) 2> /dev/null \
	      || cp -p $(srcdirpfx)$$file $(PACKAGE_TARNAME); \
	  done; \
	  echo "    .git"; \
	  ln $(srcdirpfx).git $(PACKAGE_TARNAME) 2> /dev/null \
	    || cp -p -R $(srcdirpfx).git $(PACKAGE_TARNAME); \
	else \
	  for file in $(HGDISTFILES); do \
	    echo "    $$file"; \
	    ln $(srcdirpfx)$$file $(PACKAGE_TARNAME) 2> /dev/null \
	      || cp -p $(srcdirpfx)$$file $(PACKAGE_TARNAME); \
	  done; \
	  echo "    .hg"; \
	  ln $(srcdirpfx).hg $(PACKAGE_TARNAME) 2> /dev/null \
	    || cp -p -R $(srcdirpfx).hg $(PACKAGE_TARNAME); \
	fi

dist-devel-post:
	rm -rf $(PACKAGE_TARNAME)-devel
	mv $(PACKAGE_TARNAME) $(PACKAGE_TARNAME)-devel
	tar chof $(PACKAGE_TARNAME)-devel.tar $(PACKAGE_TARNAME)-devel
	gzip -9 $(PACKAGE_TARNAME)-devel.tar
	mv $(PACKAGE_TARNAME)-devel.tar.gz $(PACKAGE_TARNAME)-devel.tgz
	rm -rf $(PACKAGE_TARNAME)-devel

all-recursive install-recursive uninstall-recursive mostlyclean-recursive clean-recursive distclean-recursive bootclean-recursive realclean-recursive rc-setup-recursive dist-recursive dist-devel-recursive:
	@for subdir in $(SUBDIRS); do \
	  target=`echo $@ | sed 's/-recursive//'`; \
	  echo making $$target in $$subdir; \
	  (cd $$subdir && $(MAKE) $$target) || exit 1; \
	done

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

# For an explanation of the following makefile rules, see node
# `Automatic Remaking' in GNU Autoconf documentation.

makefile: makefile.in config.status
	CONFIG_FILES=$@ CONFIG_HEADERS= ./config.status

config.status: configure
	./config.status --recheck

configure: configure.ac
	cd $(srcdir) && autoconf configure.ac > configure && chmod 755 configure

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
