## Process this file with automake to produce Makefile.in
# Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software
# Foundation, Inc.
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

SUBDIRS = . rsa-md5-collision pkcs1-padding pkcs8-decode pkcs12-decode	\
	userid pathlen key-id sha2 safe-renegotiation dsa

if ENABLE_OPENPGP
SUBDIRS += openpgp-certs
endif

if WANT_TEST_SUITE
SUBDIRS += suite
endif

EXTRA_DIST = libgcrypt.supp eagain-common.h

AM_CFLAGS = $(WARN_CFLAGS) $(WERROR_CFLAGS)
AM_CPPFLAGS = \
	-I$(top_srcdir)/gl			\
	-I$(top_builddir)/gl			\
	-I$(top_srcdir)/lib/includes		\
	-I$(top_builddir)/lib/includes		\
	-I$(top_srcdir)/libextra/includes	\
	-I$(top_builddir)/libextra/includes	\
	-I$(top_srcdir)/lib			\
	-I$(top_srcdir)/doc/examples

AM_LDFLAGS = -no-install
LDADD = ../lib/libgnutls.la \
	../gl/libgnu.la \
	libutils.la \
	$(LTLIBGCRYPT)	\
	$(LIBSOCKET) $(INET_NTOP_LIB) $(INET_PTON_LIB)

if ENABLE_MINITASN1
AM_CPPFLAGS += -I$(srcdir)/../lib/minitasn1
endif

noinst_LTLIBRARIES = libutils.la
libutils_la_SOURCES = utils.h utils.c

ctests = simple gc set_pkcs12_cred certder certuniqueid mpi			\
	certificate_set_x509_crl dn parse_ca moredn crypto_rng mini	\
	hostname-check cve-2008-4989 pkcs12_s2k chainverify	\
	crq_key_id x509sign-verify cve-2009-1415 cve-2009-1416		\
	crq_apis init_roundtrip pkcs12_s2k_pem dn2 mini-eagain		\
	nul-in-x509-names x509_altname pkcs12_encode mini-x509		\
	mini-x509-rehandshake rng-fork x509cert #gendh

if ENABLE_OPENSSL
ctests +=  openssl
openssl_LDADD = $(LDADD) ../libextra/libgnutls-openssl.la
endif

if ENABLE_OPENPGP
ctests += openpgp-auth openpgp-keyring pgps2kgnu
openpgp_keyring_LDADD = $(LDADD) ../libextra/libgnutls-extra.la
endif

if HAVE_FORK
ctests += x509self x509dn anonself pskself dhepskself	\
	resume setcredcrash

if ENABLE_OPENPGP
ctests += openpgpself 
endif

endif

check_PROGRAMS = $(ctests)
dist_check_SCRIPTS = rfc2253-escape-test

TESTS = $(ctests) $(dist_check_SCRIPTS)

TESTS_ENVIRONMENT =						\
	PKCS12FILE=$(srcdir)/pkcs12-decode/client.p12		\
	PKCS12PASSWORD=foobar					\
	PKCS12FILE_2=$(srcdir)/pkcs12-decode/pkcs12_2certs.p12	\
	PKCS12PASSWORD_2=""					\
	EXEEXT=$(EXEEXT)					\
	srcdir="$(srcdir)"					\
	$(VALGRIND)
