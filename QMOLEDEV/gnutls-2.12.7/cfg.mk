# Copyright (C) 2006, 2007, 2008, 2009, 2010 Free Software Foundation,
# Inc.
#
# Author: Simon Josefsson
#
# This file is part of GnuTLS.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

WFLAGS ?= --enable-gcc-warnings
ADDFLAGS ?=
CFGFLAGS ?= --enable-gtk-doc --enable-gtk-doc-pdf $(ADDFLAGS) $(WFLAGS)
PACKAGE ?= gnutls

INDENT_SOURCES = `find . -name \*.[ch] -o -name gnutls.h.in | grep -v -e ^./build-aux/ -e ^./lib/minitasn1/ -e ^./lib/build-aux/ -e ^./gl/ -e ^./src/cfg/ -e -gaa.[ch] -e asn1_tab.c -e ^./tests/suite/`

ifeq ($(.DEFAULT_GOAL),abort-due-to-no-makefile)
.DEFAULT_GOAL := bootstrap
endif

PODIR := lib/po
PO_DOMAIN := libgnutls

local-checks-to-skip = sc_prohibit_strcmp sc_prohibit_atoi_atof		\
	sc_error_message_uppercase sc_prohibit_have_config_h		\
	sc_require_config_h sc_require_config_h_first			\
	sc_trailing_blank sc_unmarked_diagnostics sc_immutable_NEWS \
	sc_prohibit_magic_number_exit sc_texinfo_acronym
VC_LIST_ALWAYS_EXCLUDE_REGEX = \
	^(((lib/|libextra/)?(gl|build-aux))|tests/suite)/.*

autoreconf:
	for f in $(PODIR)/*.po.in; do \
		cp $$f `echo $$f | sed 's/.in//'`; \
	done
	mv lib/build-aux/config.rpath lib/build-aux/config.rpath-
	test -f ./configure || autoreconf --install
	test `hostname` = "gaggia" && cp gl/m4/size_max.m4 m4/ || true
	mv lib/build-aux/config.rpath- lib/build-aux/config.rpath

update-po: refresh-po
	for f in `ls $(PODIR)/*.po | grep -v quot.po`; do \
		cp $$f $$f.in; \
	done
	git add $(PODIR)/*.po.in
	git commit -m "Sync with TP." $(PODIR)/LINGUAS $(PODIR)/*.po.in

bootstrap: autoreconf
	./configure $(CFGFLAGS)

#Two runs the first should add the LGPL components and the
#second the components used by src/ files.
glimport:
	gnulib-tool --m4-base gl/m4 --tests-base=gl/tests --libtool \
	--dir=. --local-dir=gl/override --lib=libgnu --source-base=gl \
	--aux-dir=build-aux --with-tests --avoid=alignof-tests --avoid=lseek-tests \
	--import crypto/hmac-md5 crypto/md5 extensions havelib lib-msvc-compat lib-symbol-versions \
	byteswap c-ctype func gettext lib-msvc-compat lib-symbol-versions memmem-simple minmax \
	netdb read-file snprintf sockets socklen stdint strcase strverscmp sys_socket sys_stat time_r unistd \
	vasprintf vsnprintf manywarnings warnings netinet_in alloca getpass u64
	gnulib-tool --m4-base gl/m4 --tests-base=gl/tests --libtool \
	--dir=. --local-dir=gl/override --lib=libgnu --source-base=gl \
	--aux-dir=build-aux --with-tests --avoid=alignof-tests --avoid=lseek-tests \
	--add-import progname version-etc timespec version-etc-fsf gettime valgrind-tests

# Code Coverage

pre-coverage:
	ln -sf /usr/local/share/gaa/gaa.skel src/gaa.skel

web-coverage:
	rm -fv `find $(htmldir)/coverage -type f | grep -v CVS`
	cp -rv doc/coverage/* $(htmldir)/coverage/

upload-web-coverage:
	cd $(htmldir) && \
		cvs commit -m "Update." coverage

# Mingw32

W32ROOT ?= $(HOME)/gnutls4win/inst

mingw32: autoreconf 
	./configure $(CFGFLAGS) --host=i586-mingw32msvc --build=`build-aux/config.guess` --with-libtasn1-prefix=$(W32ROOT) --with-libgcrypt-prefix=$(W32ROOT) --prefix $(W32ROOT)

.PHONY: bootstrap autoreconf mingw32

# Release

ChangeLog:
	git log --pretty --numstat --summary --since="2008 November 07" -- | git2cl > ChangeLog
	cat .clcopying >> ChangeLog

tag = $(PACKAGE)_`echo $(VERSION) | sed 's/\./_/g'`
htmldir = ../www-$(PACKAGE)

release: prepare upload web upload-web

prepare:
	! git tag -l $(tag) | grep $(PACKAGE) > /dev/null
	rm -f ChangeLog
	$(MAKE) ChangeLog distcheck
	git commit -m Generated. ChangeLog
	git tag -u b565716f! -m $(VERSION) $(tag)

upload:
	git push
	git push --tags
	build-aux/gnupload --to ftp.gnu.org:$(PACKAGE) $(distdir).tar.bz2
	scp $(distdir).tar.bz2 $(distdir).tar.bz2.sig igloo.linux.gr:~ftp/pub/gnutls/
	ssh igloo.linux.gr 'cd ~ftp/pub/gnutls/ && sha1sum *.tar.bz2 > CHECKSUMS'
	cp $(distdir).tar.bz2 $(distdir).tar.bz2.sig ../releases/$(PACKAGE)/

web:
	echo generating documentation for $(PACKAGE)
	cd doc && $(SHELL) ../build-aux/gendocs.sh \
		--html "--css-include=texinfo.css" \
		-o ../$(htmldir)/manual/ $(PACKAGE) "$(PACKAGE_NAME)"
	#cd doc/doxygen && doxygen && cd ../.. && cp -v doc/doxygen/html/* $(htmldir)/devel/doxygen/ && cd doc/doxygen/latex && make refman.pdf && cd ../../../ && cp doc/doxygen/latex/refman.pdf $(htmldir)/devel/doxygen/$(PACKAGE).pdf
	cp -v doc/reference/html/*.html doc/reference/html/*.png doc/reference/html/*.devhelp doc/reference/html/*.css $(htmldir)/reference/
	#cp -v doc/cyclo/cyclo-$(PACKAGE).html $(htmldir)/cyclo/

upload-web:
	cd $(htmldir) && \
		cvs commit -m "Update." manual/ reference/ \
			doxygen/ devel/ cyclo/
