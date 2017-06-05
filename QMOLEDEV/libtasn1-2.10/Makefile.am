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

DISTCHECK_CONFIGURE_FLAGS = --enable-gtk-doc

EXTRA_DIST = windows/asn1-parser/asn1-parser.vcproj		\
	windows/asn1-parser/config.h windows/libtasn1.ncb	\
	windows/libtasn1.sln windows/libtasn1.suo		\
	windows/gnulib/getopt.h windows/gnulib/gnulib.vcproj	\
	windows/libtasn1/libtasn1.vcproj			\
	windows/test-parser/test-parser.vcproj			\
	windows/test-tree/test-tree.vcproj

SUBDIRS = gl lib src examples tests doc

ACLOCAL_AMFLAGS = -I m4 -I gl/m4 -I lib/glm4

EXTRA_DIST += cfg.mk maint.mk .clcopying
