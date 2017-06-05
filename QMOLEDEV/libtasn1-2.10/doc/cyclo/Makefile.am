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

EXTRA_DIST = cyclo-libtasn1.html

cyclo-libtasn1.html:
	$(PMCCABE) ${top_srcdir}/lib/*.[ch] \
		| sort -nr \
		| $(AWK) -f ${top_srcdir}/build-aux/pmccabe2html \
			-v lang=html -v name="$(PACKAGE_NAME)" \
			-v vcurl="http://git.savannah.gnu.org/gitweb/?p=libtasn1.git;a=blob;f=%FILENAME%;hb=HEAD" \
			-v url="http://www.gnu.org/software/libtasn1/" \
			-v css=../../build-aux/pmccabe.css \
			> tmp
	mv tmp $@
