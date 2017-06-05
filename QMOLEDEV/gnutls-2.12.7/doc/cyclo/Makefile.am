## Process this file with automake to produce Makefile.in
# Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2010 Free Software
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

EXTRA_DIST = cyclo-gnutls.html

cyclo-gnutls.html:
	$(PMCCABE) ${top_srcdir}/lib/*.[ch] ${top_srcdir}/lib/x509/*.[ch] ${top_srcdir}/lib/openpgp/*.[ch] ${top_srcdir}/lib/opencdk/*.[ch] \
		| sort -nr \
		| $(AWK) -f ${top_srcdir}/build-aux/pmccabe2html \
			-v lang=html -v name="$(PACKAGE_NAME)" \
			-v vcurl="http://git.savannah.gnu.org/gitweb/?p=gnutls.git;a=blob;f=%FILENAME%;hb=HEAD" \
			-v url="http://www.gnu.org/software/gnutls/" \
			-v css=../../build-aux/pmccabe.css \
			> tmp
	mv tmp $@
