# This file is part of GNU cpio
# Copyright (C) 2009 Free Software Foundation
#
# GNU cpio is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU cpio is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU cpio.  If not, see <http://www.gnu.org/licenses/>.

dnl CPIO_FLUSHLEFT -- remove all whitespace at the beginning of lines
dnl This is useful for c-code which may include cpp statements
dnl
dnl WARNING: When editing this macro, make sure your editor does not
dnl clobber the regexp argument to m4_bpatsubst: it must contain one
dnl space and one tab (ASCII 9) character.
dnl
AC_DEFUN([CPIO_FLUSHLEFT],
 [m4_changequote(`,')dnl
 m4_bpatsubst(`$1', `^[ 	]+')
 m4_changequote([,])])dnl

