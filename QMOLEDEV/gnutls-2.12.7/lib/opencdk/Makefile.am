## Process this file with automake to produce Makefile.in
# Copyright (C) 2004, 2005, 2006, 2007, 2008, 2010 Free Software
# Foundation, Inc.
#
# Author: Nikos Mavroyanopoulos
#
# This file is part of GnuTLS.
#
# The GnuTLS is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1 of
# the License, or (at your option) any later version.
#
# The GnuTLS is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with GnuTLS; if not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA

AM_CPPFLAGS = \
	-I$(srcdir)/../gl		\
	-I$(builddir)/../gl		\
	-I$(srcdir)/../includes		\
	-I$(builddir)/../includes	\
	-I$(srcdir)/..

if ENABLE_MINITASN1
AM_CPPFLAGS += -I$(srcdir)/../minitasn1
endif

noinst_LTLIBRARIES = libminiopencdk.la

libminiopencdk_la_SOURCES = armor.c filters.h keydb.h main.c types.h	\
	kbnode.c main.h packet.h dummy.c sig-check.c hash.c \
	keydb.c pubkey.c stream.c write-packet.c misc.c	seskey.c \
	context.h literal.c new-packet.c read-packet.c stream.h opencdk.h

EXTRA_DIST = README
