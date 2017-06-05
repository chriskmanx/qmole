# This file is part of GNU cpio
# Copyright (C) 2003, 2004, 2007, 2009, 2010 Free Software Foundation,
# Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA.

INCLUDES=-I. -I.. -I$(top_srcdir)/gnu -I$(top_builddir)/gnu  -I$(top_srcdir)/lib -I$(top_builddir)/lib

bin_PROGRAMS=cpio @CPIO_MT_PROG@
EXTRA_PROGRAMS=mt

cpio_SOURCES = \
 copyin.c\
 copyout.c\
 copypass.c\
 defer.c\
 dstring.c\
 global.c\
 fatal.c\
 main.c\
 tar.c\
 util.c\
 filemode.c\
 idcache.c\
 makepath.c\
 userspec.c

noinst_HEADERS =\
 cpio.h\
 cpiohdr.h\
 tar.h\
 tarhdr.h\
 defer.h\
 dstring.h\
 extern.h\
 filetypes.h\
 safe-stat.h

LDADD=../lib/libpax.a ../gnu/libgnu.a @INTLLIBS@

