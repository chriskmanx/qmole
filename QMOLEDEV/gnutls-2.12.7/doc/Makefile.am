## Process this file with automake to produce Makefile.in
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
# 2009, 2010 Free Software Foundation, Inc.
#
# Author: Nikos Mavrogiannopoulos
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

EXTRA_DIST = TODO certtool.cfg gnutls.ps gnutls.pdf gnutls.html	\
	doxygen/Doxyfile.in doxygen/Doxyfile.orig texinfo.css

SUBDIRS = examples cyclo scripts manpages credentials
if ENABLE_GTK_DOC
SUBDIRS += reference
endif

info_TEXINFOS = gnutls.texi
gnutls_TEXINFOS = gnutls.texi fdl-1.3.texi lgpl-2.1.texi gpl-3.0.texi	\
	cha-auth.texi cha-bib.texi cha-cert-auth.texi			\
	cha-ciphersuites.texi cha-copying.texi cha-functions.texi	\
	cha-gtls-app.texi cha-internals.texi cha-intro-tls.texi		\
	cha-library.texi cha-preface.texi cha-programs.texi		\
	cha-tls-app.texi

# Examples.
gnutls_TEXINFOS += examples/ex-client1.c examples/ex-client2.c		\
	examples/ex-session-info.c examples/ex-verify.c			\
	examples/ex-cert-select.c examples/ex-client-resume.c		\
	examples/ex-client-srp.c examples/ex-rfc2818.c			\
	examples/ex-serv1.c examples/ex-serv-anon.c			\
	examples/ex-serv-pgp.c examples/ex-serv-srp.c			\
	examples/ex-alert.c examples/ex-x509-info.c examples/ex-crq.c	\
	examples/ex-pkcs12.c

# Images.  Make sure there are eps + png + pdf of each, plus the source dia.
gnutls_TEXINFOS += gnutls-internals.dia gnutls-internals.eps		\
                   gnutls-internals.png gnutls-internals.pdf
gnutls_TEXINFOS += gnutls-layers.dia gnutls-layers.eps			\
                   gnutls-layers.png gnutls-layers.pdf
gnutls_TEXINFOS += gnutls-crypto-layers.dia gnutls-crypto-layers.eps	\
                   gnutls-crypto-layers.png gnutls-crypto-layers.pdf
gnutls_TEXINFOS += gnutls-pgp.dia gnutls-pgp.eps gnutls-pgp.png		\
                   gnutls-pgp.pdf
gnutls_TEXINFOS += gnutls-x509.dia gnutls-x509.eps gnutls-x509.png	\
                   gnutls-x509.pdf
gnutls_TEXINFOS += gnutls-logo.eps gnutls-logo.png gnutls-logo.pdf
gnutls_TEXINFOS += pkcs11-vision.dia pkcs11-vision.eps	\
                   pkcs11-vision.png pkcs11-vision.pdf

# Images.  Make sure there are eps + png + pdf of each, plus the source dia.
gnutls_TEXINFOS += gnutls-certificate-user-use-case.dia	\
	gnutls-certificate-user-use-case.eps		\
	gnutls-certificate-user-use-case.pdf		\
	gnutls-certificate-user-use-case.png
gnutls_TEXINFOS += gnutls-client-server-use-case.dia	\
	gnutls-client-server-use-case.eps		\
	gnutls-client-server-use-case.pdf		\
	gnutls-client-server-use-case.png
gnutls_TEXINFOS += gnutls-extensions_st.dia gnutls-extensions_st.eps	\
	gnutls-extensions_st.pdf gnutls-extensions_st.png
gnutls_TEXINFOS += gnutls-handshake-sequence.dia			\
	gnutls-handshake-sequence.eps gnutls-handshake-sequence.pdf	\
	gnutls-handshake-sequence.png
gnutls_TEXINFOS += gnutls-handshake-state.dia			\
	gnutls-handshake-state.eps gnutls-handshake-state.pdf	\
	gnutls-handshake-state.png
gnutls_TEXINFOS += gnutls-mod_auth_st.dia gnutls-mod_auth_st.eps	\
	gnutls-mod_auth_st.pdf gnutls-mod_auth_st.png
gnutls_TEXINFOS += gnutls-objects.dia gnutls-objects.eps	\
	gnutls-objects.pdf gnutls-objects.png

imagesdir = $(infodir)
images_DATA = gnutls-certificate-user-use-case.png		\
	gnutls-extensions_st.png gnutls-handshake-state.png	\
	gnutls-layers.png gnutls-mod_auth_st.png gnutls-pgp.png	\
	gnutls-client-server-use-case.png			\
	gnutls-handshake-sequence.png gnutls-internals.png	\
	gnutls-logo.png gnutls-objects.png gnutls-x509.png	\
	pkcs11-vision.png

AM_MAKEINFOFLAGS = -I $(top_srcdir)/doc
TEXI2DVI = texi2dvi $(AM_MAKEINFOFLAGS)
AM_MAKEINFOHTMLFLAGS = $(AM_MAKEINFOFLAGS) \
	--no-split --css-include=$(srcdir)/texinfo.css

MAINTAINERCLEANFILES =

# Generated texinfos.

gnutls_TEXINFOS += gnutls-api.texi extra-api.texi ia-api.texi	\
	x509-api.texi pgp-api.texi
MAINTAINERCLEANFILES += gnutls-api.texi extra-api.texi ia-api.texi	\
	x509-api.texi pgp-api.texi

gnutls-api.texi: $(srcdir)/../lib/*.c
	echo "" > $@-tmp
	for i in $^; do \
		echo -n "Creating documentation for file $$i... " && \
		$(srcdir)/scripts/gdoc -texinfo $$i >> $@-tmp && \
		echo "ok"; \
	done
	$(srcdir)/scripts/sort2.pl < $@-tmp > $@-tmp2
	rm -f $@-tmp
	mv -f $@-tmp2 $@

x509-api.texi: $(srcdir)/../lib/x509/*.c
	echo "" > $@-tmp
	for i in $^; do \
		echo -n "Creating documentation for file $$i... " && \
		$(srcdir)/scripts/gdoc -texinfo $$i >> $@-tmp && \
		echo "ok"; \
	done
	$(srcdir)/scripts/sort2.pl < $@-tmp > $@-tmp2
	rm -f $@-tmp
	mv -f $@-tmp2 $@

pgp-api.texi: $(srcdir)/../lib/openpgp/*.c
	echo "" > $@-tmp
	for i in $^; do \
		echo -n "Creating documentation for file $$i... " && \
		$(srcdir)/scripts/gdoc -texinfo $$i >> $@-tmp && \
		echo "ok"; \
	done
	$(srcdir)/scripts/sort2.pl < $@-tmp > $@-tmp2
	rm -f $@-tmp
	mv -f $@-tmp2 $@

extra-api.texi: $(srcdir)/../libextra/gnutls_extra.c
	echo "" > $@-tmp
	for i in $^; do \
		echo -n "Creating documentation for file $$i... " && \
		$(srcdir)/scripts/gdoc -texinfo $$i >> $@-tmp && \
		echo "ok"; \
	done
	$(srcdir)/scripts/sort2.pl < $@-tmp > $@-tmp2
	rm -f $@-tmp
	mv -f $@-tmp2 $@

ia-api.texi: $(srcdir)/../libextra/gnutls_ia.c
	echo "" > $@-tmp
	for i in $^; do \
		echo -n "Creating documentation for file $$i... " && \
		$(srcdir)/scripts/gdoc -texinfo $$i >> $@-tmp && \
		echo "ok"; \
	done
	$(srcdir)/scripts/sort2.pl < $@-tmp > $@-tmp2
	rm -f $@-tmp
	mv -f $@-tmp2 $@

# Generated texinfos.

gnutls_TEXINFOS += error_codes.texi algorithms.texi
MAINTAINERCLEANFILES += error_codes.texi algorithms.texi

AM_CPPFLAGS = -I$(top_srcdir)/lib/includes -I$(top_builddir)/lib/includes

noinst_PROGRAMS = errcodes printlist

errcodes_SOURCES = errcodes.c
errcodes_LDADD = ../lib/libgnutls.la ../gl/libgnu.la

printlist_SOURCES = printlist.c
printlist_LDADD = ../lib/libgnutls.la ../gl/libgnu.la

error_codes.texi: $(top_srcdir)/lib/gnutls_errors.c $(srcdir)/errcodes.c
	make $(builddir)/errcodes
	$(builddir)/errcodes > $@-tmp
	mv -f $@-tmp $@

algorithms.texi: $(srcdir)/printlist.c
	make $(builddir)/printlist
	$(builddir)/printlist > $@-tmp
	mv -f $@-tmp $@

# Guile texinfos.

guile_texi           = core.c.texi extra.c.texi
gnutls_TEXINFOS      += guile.texi $(guile_texi)
BUILT_SOURCES        = $(guile_texi)
MAINTAINERCLEANFILES += $(guile_texi)
EXTRA_DIST           += $(guile_texi) extract-guile-c-doc.scm

if HAVE_GUILE

GUILE_FOR_BUILD =				\
  GUILE_AUTO_COMPILE=0				\
  $(GUILE) -L $(top_srcdir)/guile/modules

SNARF_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)			\
	 -I$(top_srcdir)/lib/includes -I$(top_builddir)/lib/includes	\
	 -I$(top_srcdir)/libextra/includes				\
         -I$(top_srcdir)/guile/src -I$(top_builddir)/guile/src

core.c.texi: $(top_srcdir)/guile/src/core.c
	$(MAKE) -C ../guile/src built-sources &&			\
	$(GUILE_FOR_BUILD) -l "$(srcdir)/extract-guile-c-doc.scm"	\
	   -e '(apply main (cdr (command-line)))'			\
	   -- "$^" "$(CPP)" "$(SNARF_CPPFLAGS) $(CPPFLAGS)"		\
	   > "$@"

extra.c.texi: $(top_srcdir)/guile/src/extra.c
	$(MAKE) -C ../guile/src built-sources &&			\
	$(GUILE_FOR_BUILD) -l "$(srcdir)/extract-guile-c-doc.scm"	\
	   -e '(apply main (cdr (command-line)))'			\
	   -- "$^" "$(CPP)" "$(SNARF_CPPFLAGS) $(CPPFLAGS)"		\
	   > "$@"


else !HAVE_GUILE

core.c.texi:
	echo "(Guile not available, documentation not generated.)" > $@

extra.c.texi:
	echo "(Guile not available, documentation not generated.)" > $@


endif !HAVE_GUILE
