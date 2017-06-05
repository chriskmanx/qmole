## Process this file with automake to produce Makefile.in
# Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 Free Software
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

AM_CFLAGS = $(WARN_CFLAGS) $(WERROR_CFLAGS)
AM_CPPFLAGS = \
	-I$(top_srcdir)/lib/includes -I$(top_builddir)/lib/includes	\
	-I$(top_srcdir)/libextra/includes				\
	-I$(top_srcdir)/gl -I$(top_builddir)/gl

# Gnulib warns and suggests use of fseeko instead of fseek, which is
# used in ex-cert-select.c, but certificate files will not be > 4 GB,
# so we just silence the warning instead of fixing the code here.
AM_CPPFLAGS += -D_GL_NO_LARGE_FILES

AM_LDFLAGS = -no-install
LDADD = libexamples.la				\
	../../lib/libgnutls.la			\
	../../libextra/libgnutls-extra.la	\
	../../gl/libgnu.la			\
	$(LIBSOCKET) $(INET_NTOP_LIB) $(INET_PTON_LIB)

CXX_LDADD = $(LDADD) \
	../../lib/libgnutlsxx.la

noinst_PROGRAMS = ex-client2 ex-client-resume
noinst_PROGRAMS += ex-cert-select ex-rfc2818
noinst_PROGRAMS += ex-cert-select-pkcs11

if ENABLE_PKI
noinst_PROGRAMS += ex-crq ex-serv1
endif

if ENABLE_CXX
ex_cxx_SOURCES = ex-cxx.cpp
ex_cxx_LDADD = $(CXX_LDADD)
noinst_PROGRAMS += ex-cxx
endif

if ENABLE_ANON
noinst_PROGRAMS += ex-client1 ex-serv-anon
endif

if ENABLE_OPENPGP
noinst_PROGRAMS += ex-serv-pgp
endif

if ENABLE_PSK
noinst_PROGRAMS += ex-client-psk
if ENABLE_PKI
noinst_PROGRAMS += ex-serv-psk
endif
endif

if ENABLE_SRP
noinst_PROGRAMS += ex-client-srp ex-serv-srp
endif

noinst_LTLIBRARIES = libexamples.la

libexamples_la_SOURCES = examples.h ex-alert.c ex-pkcs12.c		\
	ex-session-info.c ex-x509-info.c ex-verify.c	\
	tcp.c 
