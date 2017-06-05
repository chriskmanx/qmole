## Process this file with automake to produce Makefile.in
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
# 2009, 2010 Free Software Foundation, Inc.
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

SUBDIRS = cfg

AM_CFLAGS = $(WARN_CFLAGS) $(WERROR_CFLAGS)
AM_CPPFLAGS = \
	-I$(srcdir)/../gl			\
	-I$(builddir)/../gl			\
	-I$(builddir)/../lib/includes		\
	-I$(srcdir)/../lib/includes		\
	-I$(srcdir)/../libextra/includes	\
	-I$(srcdir)/cfg

noinst_PROGRAMS = benchmark
bin_PROGRAMS = gnutls-serv gnutls-cli psktool gnutls-cli-debug
if ENABLE_PKI
bin_PROGRAMS += certtool p11tool
endif
if ENABLE_SRP
bin_PROGRAMS += srptool
endif

noinst_LTLIBRARIES =

gnutls_serv_SOURCES = list.h serv.c common.h common.c certtool-common.h p11common.c p11common.h

gnutls_serv_LDADD = ../lib/libgnutls.la ../libextra/libgnutls-extra.la
gnutls_serv_LDADD += libcmd-serv.la ../gl/libgnu.la
gnutls_serv_LDADD += $(LTLIBGCRYPT) $(LIBSOCKET) $(GETADDRINFO_LIB)
noinst_LTLIBRARIES += libcmd-serv.la
libcmd_serv_la_CFLAGS =
libcmd_serv_la_SOURCES = serv.gaa serv-gaa.h serv-gaa.c

if ENABLE_SRP
srptool_SOURCES = crypt.c
srptool_LDADD = ../lib/libgnutls.la libcmd-srp.la ../gl/libgnu.la
noinst_LTLIBRARIES += libcmd-srp.la
libcmd_srp_la_CFLAGS =
libcmd_srp_la_SOURCES = crypt.gaa crypt-gaa.h crypt-gaa.c
endif

psktool_SOURCES = psk.c
psktool_LDADD = ../lib/libgnutls.la libcmd-psk.la ../gl/libgnu.la
noinst_LTLIBRARIES += libcmd-psk.la
libcmd_psk_la_CFLAGS =
libcmd_psk_la_SOURCES = psk.gaa psk-gaa.h psk-gaa.c

benchmark_SOURCES = benchmark.c
benchmark_LDADD = ../lib/libgnutls.la ../gl/libgnu.la $(LIB_CLOCK_GETTIME)

gnutls_cli_SOURCES = cli.c common.h common.c p11common.c p11common.h
gnutls_cli_LDADD = ../lib/libgnutls.la ../libextra/libgnutls-extra.la
gnutls_cli_LDADD += libcmd-cli.la ../gl/libgnu.la
gnutls_cli_LDADD += $(LTLIBGCRYPT) $(LIBSOCKET) $(GETADDRINFO_LIB)
noinst_LTLIBRARIES += libcmd-cli.la
libcmd_cli_la_CFLAGS =
libcmd_cli_la_SOURCES = cli.gaa cli-gaa.h cli-gaa.c

gnutls_cli_debug_SOURCES = tls_test.c tests.h tests.c common.h common.c p11common.c p11common.h
gnutls_cli_debug_LDADD = ../lib/libgnutls.la libcmd-cli-debug.la
gnutls_cli_debug_LDADD += ../gl/libgnu.la $(LIBSOCKET) $(GETADDRINFO_LIB)
noinst_LTLIBRARIES += libcmd-cli-debug.la
libcmd_cli_debug_la_CFLAGS =
libcmd_cli_debug_la_SOURCES = tls_test.gaa tls_test-gaa.h tls_test-gaa.c

#certtool

certtool_SOURCES = certtool.c prime.c certtool-common.c p11common.c p11common.h
certtool_LDADD = ../lib/libgnutls.la 
certtool_LDADD += libcmd-certtool.la ../gl/libgnu.la
certtool_LDADD += $(LTLIBGCRYPT)
if HAVE_LIBCFG
certtool_LDADD += -lcfg+
else
noinst_LTLIBRARIES += libcfg.la
libcfg_la_CFLAGS =
libcfg_la_SOURCES = cfg/cfg+.c cfg/cfgfile.c cfg/cmdline.c cfg/parse.c	\
	cfg/props.c cfg/shared.c cfg/platon/str/dynfgets.c		\
	cfg/platon/str/strctype.c cfg/platon/str/strdyn.c		\
	cfg/platon/str/strplus.c
certtool_LDADD += libcfg.la
endif
noinst_LTLIBRARIES += libcmd-certtool.la
libcmd_certtool_la_CFLAGS =
libcmd_certtool_la_SOURCES = certtool-gaa.c certtool.gaa certtool-gaa.h	\
	certtool-cfg.h certtool-cfg.c
libcmd_certtool_la_LIBADD = ../gl/libgnu.la $(LTLIBREADLINE)
libcmd_certtool_la_LIBADD += ../lib/libgnutls.la 
libcmd_certtool_la_LIBADD += ../gl/libgnu.la $(INET_PTON_LIB)

# p11 tool
p11tool_gaa_CFLAGS =
p11tool_SOURCES = p11tool.gaa p11tool.c pkcs11.c certtool-common.c p11tool.h p11common.c
p11tool_LDADD = ../lib/libgnutls.la 
p11tool_LDADD += libcmd-p11tool.la ../gl/libgnu.la
p11tool_LDADD += $(LTLIBGCRYPT)

if HAVE_LIBCFG
p11tool_LDADD += -lcfg+
else
p11tool_LDADD += libcfg.la
endif
noinst_LTLIBRARIES += libcmd-p11tool.la
libcmd_p11tool_la_CFLAGS =
libcmd_p11tool_la_SOURCES = p11tool-gaa.c p11tool.gaa p11tool-gaa.h	\
	certtool-cfg.h certtool-cfg.c
libcmd_p11tool_la_LIBADD = ../gl/libgnu.la $(LTLIBREADLINE)
libcmd_p11tool_la_LIBADD += ../lib/libgnutls.la
libcmd_p11tool_la_LIBADD += ../gl/libgnu.la $(INET_PTON_LIB)



psk-gaa.c: $(srcdir)/psk.gaa
	-$(GAA) $< -o psk-gaa.c -i psk-gaa.h
crypt-gaa.c: $(srcdir)/crypt.gaa
	-$(GAA) $< -o crypt-gaa.c -i crypt-gaa.h
p11tool-gaa.c: $(srcdir)/p11tool.gaa
	-$(GAA) $< -o p11tool-gaa.c -i p11tool-gaa.h
certtool-gaa.c: $(srcdir)/certtool.gaa
	-$(GAA) $< -o certtool-gaa.c -i certtool-gaa.h
cli-gaa.c: $(srcdir)/cli.gaa
	-$(GAA) $< -o cli-gaa.c -i cli-gaa.h
tls_test-gaa.c: $(srcdir)/tls_test.gaa
	-$(GAA) $< -o tls_test-gaa.c -i tls_test-gaa.h
serv-gaa.c: $(srcdir)/serv.gaa
	-$(GAA) $< -o serv-gaa.c -i serv-gaa.h
