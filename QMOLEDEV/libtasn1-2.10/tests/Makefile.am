## Process this file with automake to produce Makefile.in
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
# 2011 Free Software Foundation, Inc.
#
# This file is part of LIBTASN1.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AM_CPPFLAGS = -I$(top_srcdir)/lib -I$(top_srcdir)/gl -I../gl

AM_LDFLAGS = -no-install
LDADD = ../lib/libtasn1.la ../gl/libgnu.la

EXTRA_DIST = Test_parser.asn Test_tree.asn Test_tree_asn1_tab.c	\
	Test_encoding.asn pkix.asn TestIndef.p12

# For crlf.
EXTRA_DIST += crlf.cer
dist_check_SCRIPTS = crlf

dist_check_SCRIPTS += threadsafety

MOSTLYCLEANFILES = Test_parser_ERROR.asn

check_PROGRAMS = Test_parser Test_tree Test_encoding Test_indefinite	\
	Test_errors

TESTS = Test_parser Test_tree Test_encoding Test_indefinite	\
	Test_errors crlf threadsafety

TESTS_ENVIRONMENT = \
	ASN1PARSER=$(srcdir)/Test_parser.asn \
	ASN1TREE=$(srcdir)/Test_tree.asn \
	ASN1PKIX=$(srcdir)/pkix.asn \
	ASN1PKIX=$(srcdir)/pkix.asn \
	ASN1INDEF=$(srcdir)/TestIndef.p12 \
	ASN1ENCODING=$(srcdir)/Test_encoding.asn \
	THREADSAFETY_FILES=`find $(top_srcdir)/lib -name \*.c` \
	EXEEXT=$(EXEEXT) \
	$(VALGRIND)
