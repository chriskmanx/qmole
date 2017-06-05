## Process this file with automake to produce Makefile.in
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
# Free Software Foundation, Inc.
#
# Author: Nikos Mavrogiannopoulos
#
# This file is part of GnuTLS-EXTRA.
#
# GnuTLS-extra is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# GnuTLS-extra is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GnuTLS-EXTRA; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

ACLOCAL_AMFLAGS = -I m4 -I gl/m4 -I ../lib/m4 -I ../lib/gl/m4

AM_CFLAGS = $(WERROR_CFLAGS) $(WSTACK_CFLAGS) $(WARN_CFLAGS)
AM_CPPFLAGS = \
	-I$(srcdir)/../lib/gl		\
	-I$(builddir)/../lib/gl		\
	-I$(srcdir)/gl			\
	-I$(builddir)/../lib/includes	\
	-I$(srcdir)/../lib/includes	\
	-I$(srcdir)/includes		\
	-I$(srcdir)/../lib

if ENABLE_MINITASN1
AM_CPPFLAGS += -I$(srcdir)/../lib/minitasn1
endif

SUBDIRS = gl includes

# Pkg-config script.
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = gnutls-extra.pc
DISTCLEANFILES = $(pkgconfig_DATA)

defexecdir = $(bindir)
defexec_DATA = 

lib_LTLIBRARIES = libgnutls-extra.la

libgnutls_extra_la_SOURCES = libgnutls-extra.map gnutls_extra.c fipsmd5.c

libgnutls_openssl_la_LDFLAGS = -no-undefined

# OpenSSL

if ENABLE_OPENSSL
lib_LTLIBRARIES += libgnutls-openssl.la

libgnutls_openssl_la_SOURCES = gnutls_openssl.c openssl_compat.h	\
	openssl_compat.c

libgnutls_openssl_la_LIBADD = ../lib/gl/liblgnu.la $(LIBSOCKET) \
	../lib/libgnutls.la

libgnutls_openssl_la_LDFLAGS += -version-info $(LT_SSL_CURRENT):$(LT_SSL_REVISION):$(LT_SSL_AGE)

if ENABLE_MINITASN1
libgnutls_openssl_la_LIBADD += ../lib/minitasn1/libminitasn1.la
else
libgnutls_openssl_la_LDFLAGS += $(LTLIBTASN1)
endif

if HAVE_LD_OUTPUT_DEF
libgnutls_openssl_la_LDFLAGS += \
	-Wl,--output-def,libgnutls-openssl-$(DLL_VERSION).def
defexec_DATA += libgnutls-openssl-$(DLL_VERSION).def
endif
endif

# OpenPGP

libgnutls_extra_la_LIBADD = $(LIBSOCKET)
libgnutls_extra_la_LDFLAGS = -no-undefined

# TLS/IA

libgnutls_extra_la_SOURCES += \
	ext_inner_application.h ext_inner_application.c gnutls_ia.c

if !ENABLE_NETTLE
libgnutls_extra_la_LIBADD += $(LTLIBGCRYPT)
endif

# Rest

if HAVE_LD_OUTPUT_DEF
libgnutls_extra_la_LDFLAGS += \
	-Wl,--output-def,libgnutls-extra-$(DLL_VERSION).def
defexec_DATA += libgnutls-extra-$(DLL_VERSION).def
endif

DISTCLEANFILES += $(defexec_DATA)

libgnutls_extra_la_LDFLAGS += \
	-version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) \
	$(LZO_LIBS)

libgnutls_extra_la_LIBADD += ../lib/gl/liblgnu.la gl/libxgnu.la \
	../lib/libgnutls.la

if HAVE_LD_VERSION_SCRIPT
libgnutls_extra_la_LDFLAGS += \
	-Wl,--version-script=$(srcdir)/libgnutls-extra.map
else
libgnutls_extra_la_LDFLAGS += -export-symbols-regex '^(gnutls_).*'
endif
